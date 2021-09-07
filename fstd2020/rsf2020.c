/*
 * Copyright (C) 2021  Environnement et Changement climatique Canada
 *
 * This is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * Author:
 *     M. Valin,   Recherche en Prevision Numerique, 2021
 */

// Random Segmented Files functions

#include <ransegfile_internals.h>

#if ! defined(DEBUG)
#define DEBUG 0
#endif

static struct{             // empty file template
  start_of_segment sos ;   // start of segment
  start_of_record sor ;    // empty directory record
  dir_body dir ;
  end_of_record eor ;
  end_of_segment eos ;     // end of segment
} zero_file = 
             { {{RT_SOS ,RL_SOS, 0}, {'R','S','F','0','<','-','-','>'}, 0xDEADBEEFFEEBDAED,                        {RT_SOS ,RL_SOS, 0}} ,
                {RT_DIR ,RL_EMPTY_DIR, 0}, {0 , 0}, {RT_DIR, RL_EMPTY_DIR, 0} ,
                {{RT_EOS, RL_EOS, 0}, 0xBEBEFADAADAFEBEB, (sizeof(zero_file)/sizeof(uint64_t)), 0xCAFEFADEEDAFEFAC, {RT_EOS, RL_EOS, 0}} };

static struct RSF_file rss_null = NULL_RSF_file ;

static int max_rsf_files_open=1024 ;

typedef void * pointer ;

static pointer *rsf_files = NULL ;
static int rsf_files_open = 0 ;

// struct RSF_file{                  // PRIVATE structure
//   uint32_t version ;
//   int32_t fd ;
//   off_t file_pos ;               // current file position
//   off_t file_siz ;               // file size (should NEVER SHRINKS)
//   dir_body *dir ;                    // pointer to directory file
//   RSF_file *next ;               // pointer to next file if "linked"
//   RSF_match match ;
//   uint32_t head_type ;           // 0 : invalid, 1: read, 2 : written
//   uint32_t tail_type ;           // 0 : invalid, 1: read, 2 : written
//   start_of_record  last_head ;   // last record header written/read
//   end_of_record    last_tail ;   // last record trailer written/read
//   uint32_t mode ;                // file mode (RO/RW/AP)
//   uint32_t direntry_size ;       // size of directory entry
//   uint32_t nwrites ;
//   uint32_t buf_used ;            // number of used words in buffer
//   uint32_t buf_size ;            // size of dynamic buffer
// } ;
void RSF_filedump(RSF_handle h)
{
  RSF_file *fp = h.p ;
  fprintf(stderr,"==================================================================\nversion = %d",fp->version) ;
  fprintf(stderr,", fd = %d, pos = %ld, size = %ld, mode = %d\n", fp->fd, fp->file_pos, fp->file_siz, fp->mode) ;
  fprintf(stderr,"%d/%d ", fp->direntry_used, fp->direntry_slots) ;
  fprintf(stderr,"directory entries of size %d used", fp->direntry_size) ;
  fprintf(stderr,", %d records written", fp->nwritten) ;
  fprintf(stderr,"\n==================================================================\n");
}

static void *allocate_rsf_files_open()
{
  struct rlimit rlim ;
  getrlimit(RLIMIT_NOFILE, &rlim) ;                           // get file limit for process
  max_rsf_files_open = rlim.rlim_cur ;
  return  malloc( sizeof(pointer) * max_rsf_files_open) ;     // allocate table for max number of allowed files
}

static int32_t get_rsf_file_slot(void *p)
{
  int i ;

  if(rsf_files == NULL) rsf_files = allocate_rsf_files_open() ;
  if(rsf_files == NULL) return -1 ;

  for(i = 0 ; i < max_rsf_files_open ; i++) {
    if(rsf_files[i] == NULL) {
      rsf_files[i] = p ;
      rsf_files_open ++ ;
      return i ;
    }
  }
  return -1 ;     // eventually add code to allocate a larger table
}

static int32_t purge_rsf_file_slot(void *p)
{
  int i ;

  if(rsf_files == NULL) rsf_files = allocate_rsf_files_open() ;
  if(rsf_files == NULL) return -1 ;

  for(i = 0 ; i < max_rsf_files_open ; i++) {
    if(rsf_files[i] == p) {
      rsf_files[i] = (void *) NULL ;
      rsf_files_open-- ;
      return i ;
    }
  }
  return -1 ;
}

RSF_handle rsf_open(        // function returns a ahndle to the RSF file (null if error)
  char *name,               // file name
  int32_t options,          // open options (RO/RW/AP)
  char *app,                // application signature (4 characters)
  int32_t dirsize,          // max number of entries in directory (may be 0 if open for read)
  int32_t entrysize,        // size of a directory entry (may be 0 if open for read)
  int32_t bufsz)            // buffer size for the file (may be 0 if auto allocation desired)
{
  RSF_file *fp ;
  RSF_handle h ;
  h.p = NULL ;
  int flags, slot ;
  ssize_t nbytes ;
  char *mode ;
  struct{
    end_of_record eor ;        // directory record tail
    end_of_segment eos ;     // EOS record
  }eor_eos ;

  fp = malloc(sizeof(RSF_file) + bufsz * sizeof(uint32_t));
  *fp = rss_null ;                  // initialize struct
  h.p = fp ;                        // pointer to allocated RSF_file struct
  if(fp == NULL) return h ;         // allocation failure

  if(options & RSF_AP) options |= RSF_RW;
  mode = "NULL" ;

  if(options & RSF_RO) {                             // open in read-only mode
    fp->fd = open(name, O_RDONLY) ;
    mode = "read" ;

  }else if(options & RSF_RW) {                       // open for read and write
    mode = "write" ;
    if( options & RSF_AP ) mode = "append" ;
    flags = O_RDWR ;
    if( (options & RSF_AP) == 0) flags |= O_CREAT ;  // create unless AP (append) mode is selected
    fp->fd = open(name, flags, 0600) ;
    
    if(fp->fd >= 0){
      fp->file_pos = lseek(fp->fd, 0, SEEK_END) ;    // get file size
      if(DEBUG) printf("DEBUG: file position at end = %ld\n", fp->file_pos) ;
      if(fp->file_pos == 0){                           // new file, create empty file with empty directory
        if(DEBUG) printf("DEBUG: creating file = '%s', fd = %d\n", name, fp->fd) ;
        write(fp->fd, &zero_file, sizeof(zero_file)) ;
        mode = "create" ;
      }
    }

  }else{                                             // invalid mode
    printf("ERROR: invalid mode = %d for file '%s'\n", options, name) ;
    free(fp) ;                                      // free previously allocated space
    h.p = NULL ;
    return h ;
  }

  if(fp->fd < 0){                                    // invalid mode or open error
    free(h.p) ;                                      // free previously allocated space
    h.p = NULL ;
    if(DEBUG) printf("DEBUG: open file '%s' in %s mode failed,\n", name, mode) ;
    return h ;
  }else{
    if(DEBUG) printf("DEBUG: file '%s' open in %s mode\n", name, mode) ;
  }

  fp->mode = options ;
  fp->match = rsf_default_match ;
  fp->buf_size = bufsz ;
  fp->file_pos = lseek(fp->fd, 0, SEEK_END) ;     // position at end
  fp->file_siz = fp->file_pos ;                              // get file size
  fp->file_pos = lseek(fp->fd, -sizeof(eor_eos), SEEK_END) ; // position before directory record tail
  bzero(&eor_eos, sizeof(eor_eos)) ;
  nbytes = read(fp->fd, &eor_eos, sizeof(eor_eos) ) ; // get directory record tail + EOS record
  if( nbytes != sizeof(eor_eos) ) {
    fprintf(stderr, "ERROR: file '%s' unexpectedly truncated\n", name);
    free(h.p) ;
    h.p = NULL ;
    return h ;
  }
  h.p = fp ;

  slot = get_rsf_file_slot(fp) ;
  if(DEBUG) {
    int64_t rl = eor_eos.eor.rl ;
    int64_t rlt = eor_eos.eos.tail.rl ;
    printf("DEBUG: file = '%s', fd = %d, slot = %d, size = %ld\n", name, fp->fd, slot, fp->file_siz) ;
    printf("DEBUG: End Of Segment, rt = %d, rl = %ld ", eor_eos.eos.head.rt, rlt) ;
    printf("s1 =%9.8lx, s2 =%9.8lx, dir type(length) = %d(%ld) \n", eor_eos.eos.sig1>>32, eor_eos.eos.sig2>>32, eor_eos.eor.rt, rl) ;
  }

  // now we can read the directory record(s) and consolidate them if more than 1 (starting from the end)

  return h ;    // return handle
}

int32_t rsf_close (RSF_handle rsf)
{
  RSF_file *fp = (RSF_file *) rsf.p ;

  if(fp == NULL) return -1 ;   // not open

  if(fp->nwritten > 0){
    if(DEBUG) printf("DEBUG: %d records written\n", fp->nwritten) ;
  }
  if(fp->fd >= 0) close (fp->fd) ;
  if(DEBUG) printf("DEBUG: fd = %d closed\n", fp->fd);
  purge_rsf_file_slot(rsf.p) ;
  fp->fd = -1 ;
  free(rsf.p) ;
  return 0 ;
}

int64_t rsf_seek(RSF_handle rsf, void *metadata, void *mask)
{
  return -1 ;
}

// set metadata match function to user defined function
RSF_match_fn *rsf_set_match(RSF_handle rsf, RSF_match_fn *fn)
{
  RSF_file *fp = (RSF_file *) rsf.p ;
  RSF_match_fn *old_fn ;

  old_fn = fp->match ;
  fp->match = fn ;
  return old_fn ;
}

// match criteria and meta where mask has bits set to 1
// if mask == NULL, it is not used
int32_t rsf_default_match(RSF_handle rsf, uint32_t *criteria, uint32_t *meta, uint32_t *mask, int nitems)
{
  int i ;
  printf("DEBUG: calling rsf_default_match\n");
  if(mask != NULL) {
    for(i = 0 ; i < nitems ; i++){
      if( (criteria[i] & mask[i]) != (meta[i] & mask[i]) ) return 0 ;  // mismatch, no need to go any further
    }
  }else{
    for(i = 0 ; i < nitems ; i++){
      if( criteria[i] != meta[i] ) return 0 ;  // mismatch, no need to go any further
    }
  }
  return 1 ;
}

// same as rsf_default_match but ignores mask
int32_t rsf_base_match(RSF_handle rsf, uint32_t *criteria, uint32_t *meta, uint32_t *mask, int nitems)
{
  int i ;
  printf("DEBUG: calling rsf_base_match\n");
  for(i = 0 ; i < nitems ; i++){
    if( criteria[i] != meta[i] ) return 0 ;  // mismatch, no need to go any further
  }
  return 1 ;
}

// function called to check if a directory entry matches some criteria
int32_t rsf_match(RSF_handle rsf, uint32_t *criteria, uint32_t *meta, uint32_t *mask, int nitems)
{
  RSF_file *fp = (RSF_file *) rsf.p ;

  return (fp->match != NULL) ? (fp->match)(rsf, criteria, meta, mask, nitems) : rsf_base_match(rsf, criteria, meta, mask, nitems) ;
}
