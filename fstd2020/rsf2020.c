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

// eos with zero segment length
static end_of_segment zero_eos = {{RT_EOS, RL_EOS, 0}, 0xBEBEFADAADAFEBEB, (sizeof(zero_file)/sizeof(uint64_t)), 0xCAFEFADEEDAFEFAC, {RT_EOS, RL_EOS, 0}} ;

// empty directory
static struct{
  start_of_record sor ;    // empty directory record
  dir_body dir ;
  end_of_record eor ;
} empty_dir = { {RT_DIR ,RL_EMPTY_DIR, 0}, {0 , 0}, {RT_DIR, RL_EMPTY_DIR, 0} } ;

static struct RSF_file rss_null = NULL_RSF_file ;

static int max_rsf_files_open=1024 ;

typedef void * pointer ;

// struct RSF_file{                 // internal structure for access to Random Segmented Files
//   uint32_t version ;
//   int32_t fd ;
//   DIR_PAGE *dirpages ;           // pointer to directory pages chain
//   DIR_PAGE *lastpage ;           // pointer to last directory page
//   RSF_file *next ;               // pointer to next file if "linked"
//   uint32_t *mask ;               // mask used for metadata matches
//   RSF_match_fn *match ;          // pointer to metadata matching function
//   off_t file_pos ;               // current file position
//   off_t file_siz ;               // file size (should NEVER SHRINK)
//   uint32_t head_type ;           // 0 : invalid, 1: read, 2 : written
//   uint32_t tail_type ;           // 0 : invalid, 1: read, 2 : written
//   start_of_record  last_head ;   // last record header written/read (validated by head_type)
//   end_of_record    last_tail ;   // last record trailer written/read (validated by tail_type)
//   uint32_t mode ;                // file mode (RO/RW/AP)
//   uint32_t direntry_size ;       // size of a directory entry (in 32 bit units)
//   uint32_t direntry_used ;       // number of directory entries in use (all pages)
//   uint32_t direntry_slots ;      // max number of entries in directory (nb of directory pages * DIR_PAGE_SIZE)
//   uint32_t nwritten ;            // number of records written
//   uint32_t nread ;               // number of records read
//   uint32_t buf_used ;            // number of used words in buffer (see RSF_file_flex)
//   uint32_t buf_size ;            // size of dynamic buffer (see RSF_file_flex)
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

// =================================  table of pointers to rsf files =================================
static pointer *rsf_files = NULL ;                            // global table of pointers to rsf files
static int rsf_files_open = 0 ;

static void *allocate_rsf_files_open()                        // allocate global table of pointers to rsf files
{
  struct rlimit rlim ;
  getrlimit(RLIMIT_NOFILE, &rlim) ;                           // get file limit for process
  max_rsf_files_open = rlim.rlim_cur ;
  return  malloc( sizeof(pointer) * max_rsf_files_open) ;     // allocate table for max number of allowed files
}

static int32_t find_rsf_file_slot(void *p)                    // find slot in global table
{
  int i ;
  if(rsf_files == NULL) rsf_files = allocate_rsf_files_open() ;
  if(rsf_files == NULL) return -1 ;

  if(p == NULL) return -1 ;
  for(i = 0 ; i < max_rsf_files_open ; i++) {
    if(rsf_files[i] == p) return i ;  // slot number
  }
  return -1  ; // not found
}

static int32_t get_rsf_file_slot(void *p)                     // find and fill a free slot in global table
{
  int i ;

  if(rsf_files == NULL) rsf_files = allocate_rsf_files_open() ;
  if(rsf_files == NULL) return -1 ;

  for(i = 0 ; i < max_rsf_files_open ; i++) {
    if(rsf_files[i] == NULL) {
      rsf_files[i] = p ;
      rsf_files_open ++ ;
      return i ;  // slot number
    }
  }
  return -1 ;     // eventually add code to allocate a larger table
}

static int32_t purge_rsf_file_slot(void *p)                   // remove file pointer from global table
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

// =================================  directory management =================================
// add a blank directory page to the list of pages for file pinted to by fp
// return pointer to new page if successful, NULL if unsuccessful
static DIR_PAGE *add_directory_page(RSF_file *fp)
{
  int i ;
  DIR_PAGE *newpage = malloc(sizeof(DIR_PAGE) + fp->direntry_size * sizeof(uint32_t) * DIR_PAGE_SIZE) ;

  if(newpage == NULL) return NULL ;      // allocation failure

  newpage->next = NULL ;                 // initialize new directory page
  newpage->nused = 0 ;                   // page is empty
  newpage->nslots = DIR_PAGE_SIZE ;      // page capacity
  fp->direntry_slots += DIR_PAGE_SIZE ;  // update total directory size

  for(i = 0 ; i < DIR_PAGE_SIZE ; i++){  // technically not necessary, but cleaner
    newpage->warl[i].wa = 0 ;
    newpage->warl[i].rl = 0 ;
  }
  bzero(newpage->meta, fp->direntry_size * sizeof(uint32_t) * DIR_PAGE_SIZE) ;

  if(fp->lastpage == NULL){          // first page allocated to this file
    fp->lastpage = newpage ;         // last page
    fp->dirpages = fp->lastpage ;    // first page
  }else{
    fp->lastpage->next = newpage ;   // link new page at end
  }
  return newpage ;                   // address of new page
}

// add a new record entry into the file directory
// returns file index in upper 32 bits, record index in lower 32 bits
// meta   : record primary metadata (used for searches) (direntry_size 32 bit elements)
// wa     : address in file (in 64 bit units)
// rl     : record length (metadata + data + head + tail) (in 64 bit units)
static int64_t add_directory_entry(RSF_file *fp, uint32_t *meta, uint64_t wa, uint64_t rl)
{
  DIR_PAGE *cur_page ;
  int index, i ;
  int64_t slot ;

  slot = find_rsf_file_slot(fp) ;    // slot number
  if(slot == -1) return -1 ;         // file not found in master table
  slot++ ;                           // origin 1
  slot <<= 32 ;                      // in upper 32 bits

  if(fp->direntry_slots <= fp->direntry_used) cur_page = add_directory_page(fp) ;  // directory is full (or non existent)
  if(cur_page == NULL) return -1 ;   // failed to allocate new directory page

  slot |= fp->direntry_used ;                 // add index into directory
  slot++ ;                                    // origin 1
  fp->direntry_used++ ;                       // bump directory used slots
  index = cur_page->nused ;
  cur_page->nused ++ ;                        // bump directory page used slots

  cur_page->warl[index].wa = wa ;             // insert file address
  cur_page->warl[index].rl = rl ;             // insert record length
  index = index * fp->direntry_size ;
  for(i = 0 ; i < fp->direntry_size ; i++ ){  // copy metadata
    cur_page->meta[index+i] = meta[i] ;
  }
  
  return slot ;
}

// scan directory of file fp to find a record whose metadata matched 
static int64_t scan_directory(RSF_file *fp, uint32_t *criteria, uint32_t *mask, uint64_t *wa, uint64_t *rl)
{
  int64_t slot = -1 ;
  DIR_PAGE *cur_page ;
  RSF_match_fn *scan_match = NULL ;
  int i, index ;
  int nitems ;
  uint32_t *meta ;

  *wa = 0 ;  // precondition for failure
  *rl = 0 ;
  if(fp == NULL) return slot ;
  slot = find_rsf_file_slot(fp) ; 
  if(slot == -1) return slot ;         // file not found in master table
  slot <<= 32 ;                      // in upper 32 bits

  scan_match = fp->match ;           // get metadata match function associated to this file
  if(scan_match == NULL) scan_match = &rsf_default_match ;         // no function associated
  nitems = fp->direntry_size ;

  cur_page = fp->dirpages ;
  index = 0 ;
  while(cur_page != NULL) {
    meta = cur_page->meta ;             // bottom of metadata for this page
    for(i = 0 ; i < cur_page->nused ; i++){
      if((*scan_match)(criteria, meta, mask, nitems) == 1 ){   // do we have a match at position i ?
        slot = slot + index + i ;       // add record number to slot
        *wa = cur_page->warl[i].wa ;    // position of record in file
        *rl = cur_page->warl[i].rl ;    // record length
        return slot ;
      }
      meta += nitems ;                  // metadata for next record
    }
    cur_page = cur_page->next ;
    index = index + DIR_PAGE_SIZE ;   // bump index by directory page size
  }
  return slot ;
}

// =================================  internal rsf file functions =================================
// set file position just before performing the first write (truncate file before last segment directory)
static int32_t truncate_last_segment(RSF_file *fp){
  off_t offset = (fp->directory_size + RL_EOS) * sizeof(uint64_t) ;
  off_t offset2 ;

  offset2 = lseek(fp->fd, -offset, SEEK_END) ;     // position just before directory and End Of Segment
printf("DEBUG: truncate at %ld (by %ld), fp->directory_size = %d\n",offset2, offset,fp->directory_size );  
  return ftruncate(fp->fd, offset2) ;
}

// =================================  user callable rsf file functions =================================
// check if fd is an open rsf file
int32_t is_valid_rsf_file(int fd)  // 1 if fd points to a valid rsf file, 0 otherwise
{
  start_of_segment sos ;
  end_of_segment eos ;
  off_t offset = 0 ;
  ssize_t nbytes ;

  if(fd < 0) return 0 ;                    // invalid fd
  offset = lseek(fd, offset, SEEK_SET) ;   // beginning of file
  nbytes = read(fd, &sos, sizeof(sos)) ;   // read start_of_segment
  if(nbytes != sizeof(sos)) return 0 ;     // file is too short
  if((sos.head.zr != 0) || (sos.head.rt != RT_SOS) || (sos.head.rl != RL_SOS) ||
     (sos.tail.zr != 0) || (sos.tail.rt != RT_SOS) || (sos.tail.rl != RL_SOS) ) return 0 ;     // bad markers
  if(strncmp((const char *)sos.sig1,"RSF0",4) != 0) return 0 ;                                 // did not find 'RSF0'
  if(sos.sig2 != 0xDEADBEEFFEEBDAED) return 0 ;                                                // bad signature

  offset = -sizeof(eos) ;
  offset = lseek(fd, offset, SEEK_END) ;   // end of file - length of end of segment
  nbytes = read(fd, &eos, sizeof(eos)) ;   // read end_of_segment
  if(nbytes != sizeof(eos)) return 0 ;     // file is too short
  if((eos.head.zr != 0) || (eos.head.rt != RT_EOS) || (eos.head.rl != RL_EOS) ||
     (eos.tail.zr != 0) || (eos.tail.rt != RT_EOS) || (eos.tail.rl != RL_EOS) ) return 0 ;     // bad markers
  if(eos.sig1 != 0xBEBEFADAADAFEBEB) return 0 ;     // bad signature
  if(eos.sig2 != 0xCAFEFADEEDAFEFAC) return 0 ;     // bad signature
  return 1 ;
}

// open a Random Segmented File for reading/writing/appending
// returns a handle to the RSF file (null if error)
RSF_handle rsf_open(
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
  int32_t flags, slot, valid ;
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
      if(fp->file_pos == 0){                           // new file, create minimal empty file with empty directory
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
    valid = is_valid_rsf_file(fp->fd) ;
    if(DEBUG) printf("DEBUG: file '%s' open in %s mode, valid = %d\n", name, mode, valid) ;
  }

  fp->mode     = options ;
  fp->match    = rsf_default_match ;
  fp->buf_size = bufsz ;
  fp->file_pos = lseek(fp->fd, 0, SEEK_END) ;                // position at end
  fp->file_siz = fp->file_pos ;                              // get file size
  fp->file_pos = lseek(fp->fd, -sizeof(eor_eos), SEEK_END) ; // position before directory record tail
  fp->next_write = fp->file_siz - sizeof(end_of_segment) ;   // will write over end of segment
  bzero(&eor_eos, sizeof(eor_eos)) ;                         // fill with zero before reading
  nbytes = read(fp->fd, &eor_eos, sizeof(eor_eos) ) ;        // get directory record tail + EOS record
  slot = get_rsf_file_slot(fp) ;

  if( (nbytes != sizeof(eor_eos)) | (slot < 0) ) {
    fprintf(stderr, "ERROR: file '%s' unexpectedly truncated or global file table full\n", name);
    free(h.p) ;
    h.p = NULL ;
    return h ;
  }
  h.p = fp ;
  fp->directory_size = eor_eos.eor.rl ;
  if(DEBUG) {
    int64_t rl = eor_eos.eor.rl ;
    int64_t rlt = eor_eos.eos.tail.rl ;
    printf("DEBUG: file = '%s', fd = %d, slot = %d, size = %ld, directory size = %d\n", name, fp->fd, slot, fp->file_siz, fp->directory_size) ;
    printf("DEBUG: End Of Segment, rt = %d, rl = %ld ", eor_eos.eos.head.rt, rlt) ;
    printf("s1 =%9.8lx, s2 =%9.8lx, dir type(length) = %d(%ld) \n", eor_eos.eos.sig1>>32, eor_eos.eos.sig2>>32, eor_eos.eor.rt, rl) ;
  }
  // now we can read the directory record(s) and consolidate them if more than 1 (starting from the end)

  return h ;    // return handle
}

// close Random Segmented File
// return 0 upon success, -1 in case of error
int32_t rsf_close (RSF_handle rsf)
{
  RSF_file *fp = (RSF_file *) rsf.p ;
  off_t offset ;

  if(fp == NULL) return -1 ;   // not open

  if(fp->nwritten > 0){
    if(DEBUG) printf("DEBUG: %d records written\n", fp->nwritten) ;
    // ADD CODE TO PROPERLY WRAP UP THE FILE
    // WRITE directory
    offset = lseek(fp->fd, 0, SEEK_END) ;  // position at end
write(fp->fd, &empty_dir, sizeof(empty_dir)) ;
    // WRITE End Of Segment marker
    offset = lseek(fp->fd, 0, SEEK_END) ;  // position at end
    zero_eos.segl = (offset / sizeof(uint64_t)) + RL_EOS ;
    write(fp->fd, &zero_eos, sizeof(zero_eos)) ;
  }
  if(fp->fd >= 0) close (fp->fd) ;
  if(DEBUG) printf("DEBUG: fd = %d closed\n", fp->fd);
  purge_rsf_file_slot(rsf.p) ;
  fp->fd = -1 ;
  free(rsf.p) ;
  return 0 ;
}

int64_t rsf_write(RSF_handle rsf, void *data, uint64_t data_size , void *meta, void *aux, uint32_t aux_size)
{
  RSF_file *fp = (RSF_file *) rsf.p ;
  uint64_t wa, rl ;
  int64_t slot ;

  if(fp == NULL) return -1 ;   // not open


  wa = 0 ;
  rl = fp->direntry_size + data_size + aux_size ;
  rl = (rl + 1) / 2 ; // 64 bit units
  rl = RL_SOR + rl + RL_EOR ;
  slot = add_directory_entry(fp, (uint32_t *)meta, wa, rl) ;
  if(slot < 0) return slot ;
printf("DEBUG: writing data size = %ld, meta size = %d, aux size = %d\n", data_size, fp->direntry_size, aux_size);
  if(fp->nwritten == 0) truncate_last_segment(fp) ;  // first write
  fp->nwritten ++ ;

  return slot ;
}

// look for record with metadata matching criteria (where mask bits are 1)
int64_t rsf_seek(RSF_handle rsf, void *criteria, void *mask)
{
  RSF_file *fp ;
  uint64_t wa, rl ;

  if(rsf.p == NULL) return -1 ;  // invalid file pointer
  fp = (RSF_file *) rsf.p ;
  return scan_directory(fp, criteria, mask, &wa, &rl) ;

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
int32_t rsf_default_match(uint32_t *criteria, uint32_t *meta, uint32_t *mask, int nitems)
{
  int i ;
  printf("DEBUG: calling rsf_default_match, nitems = %d\n", nitems);
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
int32_t rsf_base_match(uint32_t *criteria, uint32_t *meta, uint32_t *mask, int nitems)
{
  int i ;
  printf("DEBUG: calling rsf_base_match, nitems = %d\n", nitems);
  for(i = 0 ; i < nitems ; i++){
    if( criteria[i] != meta[i] ) return 0 ;  // mismatch, no need to go any further
  }
  return 1 ;
}

// function called to check if a directory entry matches some criteria
int32_t rsf_match(RSF_handle rsf, uint32_t *criteria, uint32_t *meta, uint32_t *mask, int nitems)
{
  RSF_file *fp = (RSF_file *) rsf.p ;
  if(nitems <=0)  nitems = fp->direntry_size ;

  return (fp->match != NULL) ? (fp->match)(criteria, meta, mask, nitems) : rsf_base_match(criteria, meta, mask, nitems) ;
}
