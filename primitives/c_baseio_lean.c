/* RMNLIB - Library of useful routines for C and Fortran programming
 * Copyright (C) 1975-2020  Division de Recherche en Prevision Numerique
 *                          Environnement Canada
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 */

/*CoMpIlAtIoN_OpTiOnS ::SX4=-Onooverlap::SX5=-Onooverlap::*/
#define _LARGEFILE64_SOURCE
#define _FILE_OFFSET_BITS 64


// NOTE : D77MULT is deprecated (determined by the Fortran routine that does the open)
#if ! defined(NO_RPN_MACROS)
#include <rpnmacros.h>
#else
#define D77MULT 4
#endif

#define swap_word_endianness(mot) { register uint32_t tmp =(uint32_t)mot; \
   mot = (tmp>>24) | (tmp<<24) | ((tmp>>8)&0xFF00) | ((tmp&0xFF00)<<8); }
   
#define swap_buffer_endianness(buff,nwds) {\
    uint32_t *buf=(uint32_t *)buff ;\
   register int32_t nwords=nwds ;\
   while(nwords--) { swap_word_endianness(*buf);buf++; };\
   }

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#define FNOM_OWNER
#include "../INTRALIB_INCLUDES/fnom.h"
#include "./wafile64.h"

#if defined(__linux__) || defined(__AIX__)
#define tell64(fd) lseek64(fd,0,1)
#endif

void static dump_file_entry(int i);
static void reset_file_entry(int i);
static int find_file_entry(char *caller, int iun);
static int c_qqqfscr(char *type);
static int qqcclos(int indf);
static long long filepos(int indf);
static int qqcopen(int indf);
static void qqcwawr64(int32_t *buf,uint64_t ladr, int lnmots, int indf);
static void qqcward64(int32_t *buf,uint64_t ladr, int lnmots, int indf);
int C_existe(char *filename);
// void f_tracebck();

static ENTETE_CMCARC cmcarc;
// static ENTETE_CMCARC_V5 cmcarc64;

static FILEINFO wafile[MAXWAFILES];

static int init = 0;
static int debug_mode = 0;
static int subfile_length=0;
static int fnom_initialized=0;
static int endian_int=1;
static int stdoutflag=0;
static int stdinflag=0;
static char *little_endian=(char *)&endian_int;

static char *AFSISIO=NULL;
static char *ARMNLIB=NULL;
static char *LOCALDIR="./";
//static char *usrlocalenv="/usr/local/env/armnlib";
static char *armnlibpath=NULL;

static int (*f90_open)() = NULL;
static int (*f90_clos)() = NULL;

/****************************************************************************
*            C _ S T D I N, C _ S T D O U T, C _ S T D E R R                *
*****************************************************************************
*/
FILE *C_STDIN()  { return stdin  ; }  // for eventuel use with freopen
FILE *C_STDOUT() { return stdout ; }
FILE *C_STDERR() { return stderr ; }
/****************************************************************************
*                   C _ F R E T O U R ,   F R E T O U R                     *
*****************************************************************************
*
***function c_fretour, fretour
*
*OBJECT: Kept only for backward compatibility.
*
*ARGUMENTS: in iun   unit number, ignored
*
*RETURNS: zero.
*
*/
int c_fretour(int iun){ return(0) ; }
// ftnword f77name(fretour)(ftnword *fiun){ return(0) ; }  transferred to f_baseio.F90


/****************************************************************************
*                         D U M P _ F I L E _ E N T R Y                     *
*****************************************************************************
*
***function dump_file_entry
*
*OBJECT: Prints the characteristics and attributes of one file 
*        of the master file table(for debugging use).
*
*ARGUMENTS: in  i   index of the entry to dump
*
*/
void static dump_file_entry(int i)
{
      fprintf(stderr,"FGFDT[%d] ",i);
      fprintf(stderr,"file_name=%s subname=%s file_type=%s\n",
              FGFDT[i].file_name,FGFDT[i].subname,FGFDT[i].file_type);
      fprintf(stderr,"iun=%d,fd=%d,size=%ld,esize=%ld,lrec=%d,flags=%s%s%s%s%s%s%s%s%s%s%s%s%s%s\n",
              FGFDT[i].iun,
              FGFDT[i].fd,
              FGFDT[i].file_size,
              FGFDT[i].eff_file_size,
              FGFDT[i].lrec,
              FGFDT[i].open_flag?"OPEN":"",
              FGFDT[i].attr.stream?"+STREAM":"",
              FGFDT[i].attr.std?"+STD":"",
              FGFDT[i].attr.burp?"+BURP":"",
              FGFDT[i].attr.rnd?"+RND":"+SEQ",
              FGFDT[i].attr.wa?"+WA":"",
              FGFDT[i].attr.wap?"+WAP":"",
              FGFDT[i].attr.sparse?"+SPARSE":"",
              FGFDT[i].attr.ftn?"+FTN":"",
              FGFDT[i].attr.unf?"+UNF":"+FMT",
              FGFDT[i].attr.read_only?"+R/O":"+R/W",
              FGFDT[i].attr.old?"+OLD":"",
              FGFDT[i].attr.notpaged?"+NOT PAGED":"",
              FGFDT[i].attr.scratch?"+SCRATCH":"");
      fprintf(stderr,"\n");
}

/****************************************************************************
*                                D _ F G F D T                              *
*****************************************************************************
*
***function d_fgfdt
*
*OBJECT: Prints file characteristics and attributes of in use files in 
*        the master file table(for debugging use).
*
*ARGUMENTS: none
*
*/
void d_fgfdt()
{
   int i=0;
   fprintf(stderr,"********** DUMP of MASTER FILE TABLE **********\n");
   for (i=0 ; i<MAXFILES ; i++) {
      if(FGFDT[i].iun != 0) dump_file_entry(i);
   }
}
void d_fgfdt__(){ d_fgfdt() ;}
void d_fgfdt_(){ d_fgfdt() ;}

/****************************************************************************
*                     R E S E T _ F I L E _ E N T R Y                       *
*****************************************************************************
*
***function reset_file_entry
*
*OBJECT: Resets a file entry in the master file table to "not in use" values.
*
*ARGUMENTS: in  i  index of the file to reset
*
*/
static void reset_file_entry(int i){
   if (FGFDT[i].file_name) free(FGFDT[i].file_name);
   if (FGFDT[i].subname)   free(FGFDT[i].subname);
   if (FGFDT[i].file_type) free(FGFDT[i].file_type);

   FGFDT[i].file_name      = (char *) NULL;
   FGFDT[i].subname        = (char *) NULL;
   FGFDT[i].file_type      = (char *) NULL;
   FGFDT[i].iun            = 0;
   FGFDT[i].fd             = -1;
   FGFDT[i].file_size      = 0;
   FGFDT[i].eff_file_size  = 0;
   FGFDT[i].lrec           = 0;
   FGFDT[i].open_flag      = 0;
   FGFDT[i].attr.stream    = 0;
   FGFDT[i].attr.std       = 0;
   FGFDT[i].attr.burp      = 0;
   FGFDT[i].attr.rnd       = 0;
   FGFDT[i].attr.wa        = 0;
   FGFDT[i].attr.sparse    = 0;
   FGFDT[i].attr.wap       = 0;
   FGFDT[i].attr.ftn       = 0;
   FGFDT[i].attr.unf       = 0;
   FGFDT[i].attr.read_only = 0;
   FGFDT[i].attr.old       = 0;
   FGFDT[i].attr.scratch   = 0;
   FGFDT[i].attr.notpaged  = 0;
   FGFDT[i].attr.write_mode= 0;
   FGFDT[i].attr.remote    = 0;    /* remote file, socket wa file */
}
/****************************************************************************
*                      F I N D _ F I L E _ E N T R Y                        *
*****************************************************************************
*
***function find_file_entry
*
*OBJECT: Returns the index in the master file table of the 
*        file having iun as unit number.
*
*ARGUMENTS: in  caller   name of the function calling find_file_entry
*           in  iun      unit number
*
*RETURNS: the index in the master file table of the 
*         file having iun as unit number.
*/
static int find_file_entry(char *caller, int iun)
{
   int i;
   for (i=0; i<MAXFILES; i++)
      if (FGFDT[i].iun == iun)
         return(i);
/*   if (i == MAXFILES) { */
   fprintf(stderr,"%s error: unit %d is not associated with any file\n",caller,iun);
   return(-1);
/*   } */
}

/****************************************************************************
*                            C _ F N O M , F N O M                          *
*****************************************************************************
*
***function c_fnom, fnom
*
*OBJECT: Open a file and make the connection with a unit number.
*        Process record file attributes.
*
*ARGUMENTS:in  iun    unit number
*          in  nom    string containing the name of the file
*          in  type   string that contains the desired file attributes
*          in  lrec   length of record(must be 0 except if type contains D77)
*
*RETURNS: zero if connection is successful, non-zero otherwise.
*
*NOTES: If name is all in upper case it will be converted to lower case.
*       c_fnom is intended to be called from C.
*       fnom is intended to be called from Fortran.
*
*AUTHOR: Mario Lepine - RPN - nov 1995
* 
*Revision - mars 1999 - Bug fix allocation pour filename
*           avril 2008 - Correction pour reconnaissance de iun=6,output iun=5,input
*           sept 2008 - Correction du nom de fichier passe pour fichier cmcarc remote 
*
*/
// the original fnom entry point has been moved to f_baseio.F90 because of needed fortran callbacks
void c_fnom_externals(int (*f90open)(), int (*f90clos)()){
  f90_open = f90open;
  f90_clos = f90clos;
}

#if ! defined (FNOM_FILE_SIZE_64_)
  file size in FGFDT has the wrong type (too small)
#endif
int c_fnom(int *iun,char *nom,char *type,int lrec)
{
  int liun,ier = 0, minus = 0, majus = 0, lng, i, j, pid, lngt, junk, mode;
  char *c, *c2, *tmpdir, *cmcarc;
  char nom2[1024];
  intptr_t intptrt;

  if( sizeof(uint64_t) != sizeof(FGFDT[0].file_size) ){
    fprintf(stderr,"ERROR: file_size from <fnom.h> too small (uint64_t expected), PLS reconfigure library\n");
    exit(1);
  }

  if(fnom_initialized == 0) {
    /* Make sure that file descriptor 0 (stdin) will not be returned by open for use with a regular file */
    /* This is a workaround for a particular case on Linux in batch mode with PBS */
    mode = O_RDONLY;
    junk = open("/dev/null", mode);
    if (junk != 0)
      close(junk);
    /*    else
          printf("Debug junk associe a /dev/null\n"); */
     ARMNLIB=getenv("ARMNLIB");
     armnlibpath=ARMNLIB;
     if (armnlibpath == NULL) armnlibpath = LOCALDIR;
     if( ARMNLIB == NULL ) ARMNLIB = LOCALDIR;
     AFSISIO=getenv("AFSISIO");
     if( AFSISIO == NULL ) AFSISIO = LOCALDIR;
     for (i=0; i<MAXFILES; i++) reset_file_entry(i);
     fnom_initialized=1;
     }

  if ( ( (intptr_t) iun > 0) && ( (intptr_t) iun < 1000) ) {     /* an integer value has been passed to c_fnom as iun */
     intptrt = (intptr_t) iun ;
     liun = intptrt;
     }
  else {     /* a pointer has been passed to c_fnom as iun */
     if (*iun == 0)
        *iun = c_qqqfscr(type);
     if (*iun == -1) {
      fprintf(stderr,"C_FNOM ERROR: no more units available\n");
      return(-1);
     }
     liun = *iun;
     }

  if ((liun ==6) && ((strcmp(nom,"$OUT") == 0) || 
                     (strcmp(nom,"$OUTPUT") == 0) || 
                     (strcmp(nom,"OUTPUT") == 0) || 
                     (strcmp(nom,"output") == 0))) {
    stdoutflag=1;
/*    fprintf(stderr,"C_FNOM DEBUG already connected: iun=%d filename=%s\n",liun,nom);   */
    return(0);
  }
  if ((liun ==5) && ((strcmp(nom,"$IN") == 0) || 
                     (strcmp(nom,"$INPUT") == 0) || 
                     (strcmp(nom,"INPUT") == 0) || 
                     (strcmp(nom,"input") == 0))) {
    stdinflag=1;
/*    fprintf(stderr,"C_FNOM DEBUG already connected: iun=%d filename=%s\n",liun,nom);   */
    return(0);
  }

  if ((liun == 6) || (liun == -2)) {
    c = nom;
    c2 = nom2;
    nom2[strlen(nom)] = '\0';
    for (j = 0; (j < strlen(nom) && j < 1024); j++, c++, c2++) {
       if (islower(*c)) {
         minus=1;
         *c2 = *c;
       }
       else if (isupper(*c)) {
         majus=1;
         *c2 = tolower(*c);
       }
       else
         *c2 = *c;
    }
  }
  if (liun == 6) {
    fclose(stdout);
    if (minus && majus)
      stdout = freopen(nom,"a",stdout);
    else
      stdout = freopen(nom2,"a",stdout);
/*    fprintf(stderr,"C_FNOM DEBUG: freopen %s pour stdout\n",nom2) ; */
    stdoutflag=1;
    return(0);
  }
  else if (liun == -2) {
    fclose(stderr);
    if (minus && majus)
      stderr = freopen(nom,"a",stderr);
    else
      stderr = freopen(nom2,"a",stderr);
    return(0);
  }
  for (i=0; i<MAXFILES; i++)
     if (FGFDT[i].iun == liun) {
        fprintf(stderr,"c_fnom error: unit %d is already in use\n", liun);
        return(-1);
        }
  for (i=0; i<MAXFILES; i++)
     if (FGFDT[i].iun == 0) {
        FGFDT[i].iun = liun;
        break;
        }
  if (i == MAXFILES) {
     fprintf(stderr,"c_fnom error: too many files, file table is full\n");
     return(-1);
     }
/*
 *   record file attributes
 */ 
  lngt = strlen(type) + 1;
  FGFDT[i].file_type = malloc(lngt+1);
  strncpy(FGFDT[i].file_type,type,lngt);
  FGFDT[i].attr.stream=0;
  FGFDT[i].attr.std=0;
  FGFDT[i].attr.burp=0;
  FGFDT[i].attr.rnd=0;
  FGFDT[i].attr.wa=0;
  FGFDT[i].attr.wap=0;
  FGFDT[i].attr.sparse=0;
  FGFDT[i].attr.ftn=0;
  FGFDT[i].attr.unf=0;
  FGFDT[i].attr.read_only=0;
  FGFDT[i].attr.old=0;
  FGFDT[i].attr.notpaged=0;
  FGFDT[i].attr.scratch = 0;
  FGFDT[i].attr.pipe = 0;
  FGFDT[i].attr.remote=0;

// TO DO : process attribute to indicate that a WA file may be sparse

  if (strstr(type,"STREAM") || strstr(type,"stream")){ FGFDT[i].attr.stream=1;
                                                       FGFDT[i].attr.rnd=1; }
  if (strstr(type,"STD")    || strstr(type,"std"))   { FGFDT[i].attr.std=1;
                                                       FGFDT[i].attr.rnd=1; }
  if (strstr(type,"STDP")   || strstr(type,"stdp"))  { FGFDT[i].attr.std=1;
                                                       FGFDT[i].attr.wap=1; /* STDP = STD + WAP  */
                                                       FGFDT[i].attr.rnd=1; }
  if (strstr(type,"BURP")   || strstr(type,"burp"))  { FGFDT[i].attr.burp=1;
                                                       FGFDT[i].attr.rnd=1; }
  if (strstr(type,"RND")    || strstr(type,"rnd"))     FGFDT[i].attr.rnd=1;
  if (strstr(type,"WA")     || strstr(type,"wa"))    { FGFDT[i].attr.rnd=1;
                                                       FGFDT[i].attr.wap=0; }  // wa attribute will be set by waopen
  if (strstr(type,"WAP")    || strstr(type,"wap"))   { FGFDT[i].attr.wap=1;
                                                       FGFDT[i].attr.rnd=1; }
  if (strstr(type,"SPARSE") || strstr(type,"sparse")){ FGFDT[i].attr.sparse=1;
                                                       FGFDT[i].attr.rnd=1; } // sparse implies rnd
  if (strstr(type,"FTN")    || strstr(type,"ftn"))   { FGFDT[i].attr.ftn=1;
                                                       FGFDT[i].attr.rnd=0; }
  if (strstr(type,"UNF")    || strstr(type,"unf"))   { FGFDT[i].attr.unf=1;
                                                       FGFDT[i].attr.ftn=1;
                                                       FGFDT[i].attr.rnd=0; }
  if (strstr(type,"OLD")    || strstr(type,"old"))     FGFDT[i].attr.old=1;
  if (strstr(type,"R/O")    || strstr(type,"r/o"))   { FGFDT[i].attr.read_only=1;
                                                       FGFDT[i].attr.old=1; }
  if (strstr(type,"R/W")    || strstr(type,"r/w"))   { FGFDT[i].attr.read_only=0;
                                                       FGFDT[i].attr.write_mode=1; }
  if (strstr(type,"D77")    || strstr(type,"d77"))   { FGFDT[i].attr.ftn=1;
                                                       FGFDT[i].attr.rnd=1; }
  if (strstr(type,"SCRATCH") || strstr(type,"scratch")) FGFDT[i].attr.scratch=1;
//   if (strstr(type,"REMOTE") || strstr(type,"remote")) { FGFDT[i].attr.remote=1; }  // no longer recognized
    
  if (!FGFDT[i].attr.std && !FGFDT[i].attr.burp && !FGFDT[i].attr.wap && 
      !FGFDT[i].attr.wa && !FGFDT[i].attr.rnd  && !FGFDT[i].attr.stream)
     FGFDT[i].attr.ftn = 1;
  FGFDT[i].lrec = lrec;
  FGFDT[i].open_flag = 0;

/*
 *   if scratch file, add tmpdir directory and pid to the file name 
 */
  if (FGFDT[i].attr.scratch) {
     if(strstr(nom,"/")){
        fprintf(stderr,"c_fnom error: / is illegal in scratch file name\n");
        fprintf(stderr,"              specified name was %s\n",nom);
        return(-1);
     }
     pid = getpid();
     tmpdir = getenv("TMPDIR");
     if (tmpdir == NULL) {
       fprintf(stderr,"c_fnom warning: TMPDIR environment variable is not defined, /tmp is used\n");
       tmpdir = "/tmp";
     }
     lng = strlen(nom) + strlen(tmpdir) + 10 + 3 + 128;    /* espace tampon supplementaire */
     if ((FGFDT[i].file_name = malloc(lng)) == NULL) {
       fprintf(stderr,"c_fnom error: can't allocate memory for file name\n");
       perror("c_fnom");
       exit(1);
     }
     sprintf(FGFDT[i].file_name, "%s/%d_%s", tmpdir, pid, nom);
  }
  else {
/*
 *   convert file name to lower case unless it contains a mix of 
 *   upper case / lower case
 */
     lng = strlen(nom);
     FGFDT[i].file_name = malloc(lng+1);
     FGFDT[i].attr.remote=0;
     c = nom;
     if (nom[0] == '@') {  /* name is of the form @some_file_name */
       c++;              /* skip the @, scan later under        */
       lng--;            /* AFSISIO & ARMNLIB if not local file */
       }
     if (nom[0] == '%') {  /* name is of the form %[%@]some_pipe_ file */
       c++;              /* skip the %  */
       lng--;
       FGFDT[i].attr.pipe = 1;
       }
     if (nom[0] == '+') {  /* name is of the form +some_file_name */
       c++;              /* skip the +, do not convert to lowercase */
       lng--;            
       strncpy(FGFDT[i].file_name, nom+1, lng);
       c2 = FGFDT[i].file_name ;
       *(c2+lng)  = '\0';
       }
     else {
       c2 = FGFDT[i].file_name ;
       *(c2 + lng) = '\0';
       for (j = 0; j < lng; j++, c++, c2++) {
         if (islower(*c)) {
           minus = 1;
           *c2 = *c;
         }
         else if (isupper(*c)) {
           majus=1;
           *c2 = tolower(*c);
         }
         else
           *c2 = *c;
       }
       if (majus && minus)
         strncpy(FGFDT[i].file_name,nom,lng);
     }
  }
/*
 *   check for a file within a cmcarc file (filename@subfilename)
 */
  if ( (cmcarc = strchr(FGFDT[i].file_name,'@')) ) {
     FGFDT[i].subname = malloc(lng+1);
     strcpy(FGFDT[i].subname,cmcarc+1);
     *cmcarc = '\0';
     FGFDT[i].attr.old = 1;  /* file must exist and be read/only */
     FGFDT[i].attr.read_only = 1;
     }
  else
     FGFDT[i].subname = NULL;
/*
 *   check for @file (@filename)
 */
  if (nom[0]=='@') {                                      // implied lookup cascade
     char filename[1024];
     strcpy(filename,FGFDT[i].file_name);

     if (access(filename,F_OK) == -1) {                   // 1 - not a local file
       sprintf(filename,"%s/datafiles/constants/%s",AFSISIO,FGFDT[i].file_name);

       if (access(filename,F_OK)  == -1) {                // 2 - not under $AFSISIO/datafiles/constants/
          sprintf(filename,"%s/data/%s",ARMNLIB,FGFDT[i].file_name);

          if (access(filename,F_OK)  == -1) {             // 3 - nor under $ARMNLIB/data/
             return(-1);
             }
          }
       }
     free(FGFDT[i].file_name);
     FGFDT[i].file_name = malloc(strlen(filename)+10);
     strcpy(FGFDT[i].file_name,filename);
     lng=strlen(filename);
     }

  if ( (FGFDT[i].attr.old || FGFDT[i].attr.read_only) )
     if (! C_existe(FGFDT[i].file_name) ) {
        fprintf(stderr,"c_fnom error: file %s should exist and does not\n",FGFDT[i].file_name);
        junk=c_fclos(liun);
        return(-1);
        }
/*
 *   Fortran files must be opened by a Fortran routine calling the Fortran runtime
 */
  ier = 0;
  if (FGFDT[i].attr.ftn) {
     int32_t iun77=liun;
     int32_t lrec77=lrec;
     int32_t rndflag77 = FGFDT[i].attr.rnd;
     int32_t unfflag77 = FGFDT[i].attr.unf;
//      int32_t lmult = D77MULT;       // no longer necessary, *f90_open can figure it out
     ier = open64(FGFDT[i].file_name,O_RDONLY);    // if file exists, get its size
     if (ier <=0) {                                // file does not exist
        FGFDT[i].file_size = -1;
        FGFDT[i].eff_file_size = -1;
        }
     else {                                        // file exists, get size
        LLSK dimm=0;
        dimm = LSEEK(ier,dimm,SEEK_END);
        FGFDT[i].file_size = dimm / sizeof(int32_t);
        FGFDT[i].eff_file_size = dimm / sizeof(int32_t);
        close(ier);
        }
     ier = (*f90_open)(iun77, FGFDT[i].file_name, type, lrec77, rndflag77, unfflag77);   // callback function
  }
  else if (FGFDT[i].attr.stream || FGFDT[i].attr.std || FGFDT[i].attr.burp || FGFDT[i].attr.wa ||
          (FGFDT[i].attr.rnd && !FGFDT[i].attr.ftn) ) {
     ier = c_waopen2(liun);
     FGFDT[i].attr.wa = 0;   // reset flag, it will be set by later call to waopen (flag set means file already opened by waopen)
  }
    
  if (ier == 0) FGFDT[i].open_flag = 1;
  if (ier < 0) junk=c_fclos(liun);
  return(ier<0?-1:0);
}  

/****************************************************************************
*                            C _ F C L O S,   F C L O S                     *
*****************************************************************************
*
***functions c_fclos and fclos
*
*OBJECT: Close file associated with unit iun.
*        Returns zero 
*        and non-zero otherwise.
*
*ARGUMENTS: in iun   unit number
*
*RETURNS: zero if the connection is successful, non-zero 
*         otherwise
*
*/
int c_fclos(int iun)
{
   int i,ier;
   int32_t iun77;

   if ((iun == 6) && (stdoutflag)) return(0);
   if ((iun == 5) && (stdinflag)) return(0);

   if ((i=find_file_entry("c_fclos",iun)) < 0) return(i);
   iun77 = iun;
   ier=0;
   if (FGFDT[i].open_flag){
      if (FGFDT[i].attr.ftn)
         ier = (*f90_clos)(&iun77);   // need to get this fortran runtime library done via a callback
      else
         ier = close(FGFDT[i].fd);
      }

   reset_file_entry(i);
   return(ier);
}

/****************************************************************************
*                          C _ Q Q Q F S C R                                *
*****************************************************************************
*
***function c_qqqfscr
*
*OBJECT: Generates a scratch unit number. 
*
*ARGUMENTS: in type  string that contains the file attributes (see FNOM)
*
*RETURNS: the scratch unit number
*
*/
static int c_qqqfscr(char *type)          
{
   int iun,i,j,inused,start;

   iun = -1;
   if (strstr(type,"FTN") || strstr(type,"ftn") || strstr(type,"D77") || strstr(type,"d77")) 
      start = 99;
   else
      start = 999;
   for (j=start; j>10; j--) {
      inused = 0;
      for (i=0; i<MAXFILES; i++)
         if (FGFDT[i].iun == j) {
            inused = 1;
            break;
            }
      if (! inused) {
         iun = j;
         break;
         }
      }
   return(iun);
}

/****************************************************************************
*                             Q Q Q F N O M                                 *
*****************************************************************************
*
***function qqqfnom
*
*OBJECT: Obtain some information about a unit: file name, file type,
*        lenght of record.
*
*ARGUMENTS:in  iun     unit number
*          out nom     file name(blank padded to have a total length of l1)
*          out type    type of file(blank padded to have a total length of l2)
*          out flrec   length of record
*          in  l1      length of filename
*          in  l2      length of type 
*
*/
int F_qqqfnom(int iun,char *nom,char *type,int *flrec,int l1,int l2)
{
   int i,j;

   j=iun;
   if ((i=find_file_entry("qqqfnom",j)) < 0) return(i);

   strncpy(nom,FGFDT[i].file_name,l1);
   for(j=strlen(FGFDT[i].file_name);j<l1;j++) nom[j]=' ';
   strncpy(type,FGFDT[i].file_type,l2);
   for(j=strlen(FGFDT[i].file_type);j<l2;j++) type[j]=' ';
   *flrec=FGFDT[i].lrec;
   return(0);
}
/****************************************************************************
*                              Q Q C C L O S                                *
*****************************************************************************
*
***function qqcclos
*
*OBJECT: Closes a file, given its index. 
*
*ARGUMENTS: in indf   index of the file in the master file table
*
*RETURNS: zero if file correctly closed, non-zero otherwise
*
*/
static int qqcclos(int indf)
{
  int lfd=FGFDT[indf].fd;
  int ind;

  ind = 0;
  while ((wafile[ind].file_desc != lfd) && (ind < MAXWAFILES))
     ind++;
  if (ind == MAXWAFILES) {
     fprintf(stderr,"qqcclos error: file is not open, fd=%d, name=%s\n",
                    lfd,FGFDT[indf].file_name);
     return(1);
     }
     
  wafile[ind].file_desc = -1;
  FGFDT[indf].fd = -1;
  FGFDT[indf].open_flag = 0;
  close(lfd);
  return(0);
}

/****************************************************************************
*  C _ W A O P E N ,   C _ W A O P E N 2 ,   W A O P E N 2 ,   W A O P E N  *
*****************************************************************************
*
***function c_waopen
*
*OBJECT: Opens a word addressable file.
*
*ARGUMENTS: in iun   unit number
*
*RETURNS: (only for c_waopen2 and waopen2) zero if file correctly opened, non-zero otherwise
*
*/
// static int create_wap_file(fd){  /* create a WAP file using parameters from fnom */
//   return(0);
// }

void c_waopen(int iun) { int scrap=c_waopen2(iun); if(scrap<=0) exit(1); }
int c_waopen2(int iun)   /* open unit iun for WORD ADDRESSABLE access */
{
   int i,ier;

   for (i=0; i < MAXFILES; i++)
      if (FGFDT[i].iun == iun)
         break;
   if (i == MAXFILES) {
      for (i=0; i < MAXFILES; i++)
         if (FGFDT[i].iun == 0) {
            FGFDT[i].iun = iun;
            break;
            }
      if (i == MAXFILES) {
        fprintf(stderr,"c_waopen error: file table is full\n");
        return(-1);
        }
/*
 *  file is not associated with fnom, file name is set to Wafileiun
 */
      FGFDT[i].file_name = malloc(10);
      sprintf(FGFDT[i].file_name,"%s%d","Wafile",iun);
      FGFDT[i].attr.wa = 1;
      FGFDT[i].attr.rnd = 1;
      }
   else {
      if (FGFDT[i].attr.rnd == 0) {
         fprintf(stderr,"c_waopen error: waopen needs a file with the RND or WA type\n");
         return(-1);
      }
      if (FGFDT[i].open_flag) {
         if (FGFDT[i].attr.wa == 1){ /* fnom opens the file but does not set wa flag */
            fprintf(stderr,"c_waopen warning: unit %d already open as %s\n",iun,FGFDT[i].file_name); 
            }
         FGFDT[i].attr.wa = 1;
         return(FGFDT[i].fd);
         }
      }
   ier = qqcopen(i);
   if (ier >=0) { FGFDT[i].open_flag = 1; FGFDT[i].attr.wa = 1; FGFDT[i].attr.rnd = 1; }
// recognize existing WAP file here by its signature
   if(FGFDT[i].attr.wap) {
// do what needs to be done when opening WAP file, including creation if new file
   }
   return ier;
}
/****************************************************************************
* C _ W A C L O S ,   C _ W A C L O S 2 ,   W A C L O S ,   W A C L O S 2   *
*****************************************************************************
*
***function c_waclos
*
*OBJECT: Closes a word addressable file.
*
*ARGUMENTS: in iun   unit number
*
*RETURNS: (only for c_waclos2 and waclos2) zero if closure is successful,
*         non-zero otherwise.
*
*/
int c_waclos2(int iun)    // c_waclos with a status return
{
   int i,ier;

   if ((i=find_file_entry("c_waclos",iun)) < 0) return(i);

   if (! FGFDT[i].open_flag) {
      fprintf(stderr,"c_waclos error: unit %d is not open\n",iun);
      return(-1);
      }
   if(FGFDT[i].attr.wap) {    // deferred implementation
// do what needs to be done when closing WAP file
// write back WAP tables if they have been overwritten
   }
   ier = qqcclos(i);
   FGFDT[i].open_flag = 0;
   FGFDT[i].attr.wa = 0;
   FGFDT[i].attr.wap = 0;
   return(ier);
}
void c_waclos(int iun) { c_waclos2(iun) ; }
/****************************************************************************
*   C _ W A W R I T ,   C _ W A W R I T 2 ,   W A W R I T ,   W A W R I T 2 *
*****************************************************************************
*
***function c_wawrit, c_wawrit2, wawrit, wawrit2
*
*OBJECT: Writes in a word addressable. c_wawrit2 and wawrit2 are functions
*        returning an error code.
*
*ARGUMENTS: in iun    unit number
*           in buf    will contain data
*           in adr    file address
*           in nmots  number of words
*
*RETURNS: (only for c-wawrit2 and wawrit2) the number of words written.
*
*/

#define WA_HOLE 2048
int c_wawrit64(int iun,void *buf, uint64_t adr,unsigned int nmots, unsigned int part)
{
   int i ;
   int32_t scrap[WA_HOLE];
   int32_t *bufswap = (int32_t *) buf;
   uint64_t ladr;
   unsigned int count;

   if(part != 0) {     // only partition 0 supported for now
     fprintf(stderr,"c_wawrit error: invalid partition %d for write\n",part);
     return(-1);
   }
   if ((i=find_file_entry("c_wawrit",iun)) < 0) return(i);

   if (! FGFDT[i].open_flag) {
      fprintf(stderr,"c_wawrit error: unit %d is not open\n",iun);
      return(-1);
      }
   if ( FGFDT[i].attr.read_only != 0 ) {
      fprintf(stderr,"c_wawrit error: unit %d ,file= %s is READ ONLY\n",
                     iun,FGFDT[i].file_name);
      return(-1);
      }
   // cannot write with start beyond file size + WA_HOLE, unless file is "sparse"
   if ( (adr > FGFDT[i].file_size+WA_HOLE) && (FGFDT[i].attr.sparse == 0) ) {
      fprintf(stderr,"c_wawrit error: attempt to write beyond EOF+%d\n",WA_HOLE);
      fprintf(stderr,"                unit = %d, adr=%lu > file_size=%ld\n",
                     iun,adr,FGFDT[i].file_size);
      fprintf(stderr,"                filename=%s\n",FGFDT[i].file_name);
      return(-1);
      }
   if ( (adr > FGFDT[i].file_size+1) && (FGFDT[i].attr.sparse == 0) ){   // fill hole with 0 bytes if not sparse
      count = adr-FGFDT[i].file_size;
      ladr = FGFDT[i].file_size+1;
      qqcwawr64(scrap,ladr,count,i);
      }
   if (*little_endian) swap_buffer_endianness(bufswap,nmots);  // destructive in-place byte swap
   qqcwawr64((int32_t *)buf,adr,nmots,i);                      // write data into file
   if (*little_endian) swap_buffer_endianness(bufswap,nmots);  // undo destructive in-place byte swap
   return( nmots > 0 ? nmots : 0);
}
void c_wawrit(int iun,void *buf,unsigned int adr,int nmots)
{
  uint64_t ladr = adr;
  c_wawrit64(iun,buf,ladr,nmots,0);
}
int c_wawrit2(int iun,void *buf,unsigned int adr,int nmots)
{
  uint64_t ladr = adr;
  return c_wawrit64(iun,buf,ladr,nmots,0);
}
/****************************************************************************
* C _ W A R E A D ,   C _ W A R E A D 2 ,   W A R E A D ,   W A R E A D 2   *
*****************************************************************************
*
***function c_waread, c_waread2, waread, waread2
*
*OBJECT: Read from a word addressable file.
*
*ARGUMENTS: in  iun     unit number
*           out buf     will contain the data read
*           in  adr     adress to start from
*           in  nmots   number of words to read
*
*RETURNS: (only for c-waread2 and waread2) the number of words read.
*
*/
void c_waread(int iun,void *buf,unsigned int adr,int nmots)
{
  int ier, i;
  uint64_t ladr = adr;

  ier = c_waread64(iun,buf,ladr,nmots,0);
  if (ier == -2) {
    i = find_file_entry("c_waread",iun);
    fprintf(stderr,
            "c_waread error: attempt to read beyond EOF, of file %s\n",
            FGFDT[i].file_name);
    fprintf(stderr,"                addr = %u, EOF = %ld\n",
            adr,FGFDT[i].eff_file_size);
  }

}
int c_waread64(int iun,void *buf,uint64_t adr,unsigned int nmots,unsigned int part)
{
   int i ;
   int32_t *bufswap = (int32_t *) buf;
   unsigned int count = nmots;

   if(part != 0) {     // only partition 0 supported for now
     fprintf(stderr,"c_waread error: invalid partition %d for read\n",part);
     return(-1);
   }
   if ((i=find_file_entry("c_waread",iun)) < 0) return(i);
   
   if (! FGFDT[i].open_flag) {
      fprintf(stderr,"c_waread error: unit %d is not open\n",iun);
      return(-1);
      }

   if ( adr > FGFDT[i].eff_file_size+2 ) {    // attempt to read with start beyond EOF
     return(-2);
   }

   if ( FGFDT[i].eff_file_size == 0 ) return(0);

   if ( adr+nmots-1 > FGFDT[i].eff_file_size ) {       // truncate read if end is beyond EOF
      nmots -= (adr+nmots-1-FGFDT[i].eff_file_size);
      }
   if ( nmots == 0 ) return(0);
   qqcward64((int32_t *)buf, adr, count, i);
   if (*little_endian) swap_buffer_endianness(bufswap,nmots);
   return(nmots);
}
int c_waread2(int iun,void *buf,unsigned int adr,int nmots)
{
  uint64_t ladr = adr;
  return c_waread64(iun,buf,ladr,nmots,0);
}
/****************************************************************************
*                      C _ W A S I Z E ,   W A S I Z E                      *
*****************************************************************************
*
***function c_wasize, wasize
*
*OBJECT: Returns the size (in words) of a file, given its unit number.
*
*ARGUMENTS: in iun   unit number
*
*RETURNS: the size of a file in words.
*
*NOTE: Gives the size of any file that can be passed to fnom.
*
*/
int32_t c_wasize(int iun)
{
   int i ;
   int64_t n;

   if ((i=find_file_entry("c_wasize",iun)) < 0) return(i);

   if (! FGFDT[i].open_flag) {
      qqcopen(i);
      n = FGFDT[i].eff_file_size;
      qqcclos(i);
   }else{
      n = FGFDT[i].eff_file_size;
   }

   return(n);
}
int64_t c_wasize64(int iun)  // same as c_wasize but result is 64 bit
{
   int i ;
   int64_t n;

   if ((i=find_file_entry("c_wasize",iun)) < 0) return(i);

   if (! FGFDT[i].open_flag) {
      qqcopen(i);
      n = FGFDT[i].eff_file_size;
      qqcclos(i);
   }else{
      n = FGFDT[i].eff_file_size;
   }

   return(n);
}
/****************************************************************************
*                    C _ N U M B L K S ,   N U M B L K S                    *
*****************************************************************************
*
***function c_numblks, numblks
*
*OBJECT: Returns the size of a file in blocks of 512 32 bit elements
*
*ARGUMENTS: in iun   unit number
*
*RETURNS: the size of a file in kilobytes.
*
*NOTE: It uses c_wasize.
*
*/
int32_t c_numblks(int iun)
{
   int64_t n = 512;
   return ( (c_wasize64(iun) +n -1) / n );
}
/****************************************************************************
*                             E X I S T E                                   *
*****************************************************************************
*
***function C_existe
*
*OBJECT: Checks if file exists.
*
*ARGUMENTS: in  nom  name of the file
*
*RETURNS: one if the file exists,
*         zero if it doesn't.
*         
*
*/
int C_existe(char *filename) 
{   
   if (access(filename, F_OK) == -1)
      return(0);                     /* file does not exist */
   else
      return(1);                     /* file exists */
}
/***************************************************************************
*                     C _ G E T F D S C ,   G E T F D S C                  *
****************************************************************************
*
***function c_getfdsc,getfdsc
*
*OBJECT: Get file descriptor associated to unit iun. 
*
*ARGUMENTS: in iun   unit number
*
*RETURNS: a file descriptor, if there is no error,
*         or a negative number if there is an error.
*
*/
int c_getfdsc(int iun) { 
   int i;

   if ((i=find_file_entry("c_getfdsc",iun)) < 0) return(i);

   if (! FGFDT[i].attr.stream) {
      fprintf(stderr,"c_getfdsc error: unit %d does not have the STREAM attribute\n",iun);
      return(-1);
      }
   if (! FGFDT[i].open_flag) {
      fprintf(stderr,"c_getfdsc error: unit %d is not open\n",iun);
      return(-1);
      }

   return(FGFDT[i].fd) ;
   }
/****************************************************************************
*                              F I L E P O S                                *
*****************************************************************************
*
***function filepos
*
*OBJECT: Returns the position of the start of the data of a subfile 
*        inside a CMCARC archive file (v4 as well as v5).
*
*ARGUMENTS: in indf  index of the subfile in the master file table
*
*RETURNS: the position of the start of the data of a subfile 
*         in a CMCARC file.
*
*/
static long long filepos(int indf)
{
  char sign[25];

  typedef struct {
    unsigned char ntotal[4];
    unsigned char ndata[4];
    char code;
    char header[MAX_NAME];
  } HEADER_CMCARC;
  
//   typedef struct {
//     unsigned char ntotal[8];
//     unsigned char ndata[8];
//     char code;
//     char header[MAX_NAME];
//   } HEADER_CMCARC_V5;

  HEADER_CMCARC *cmcarc_file;
  int nblu,lng,found=0,version=0,tail_offset;
  unsigned int nt,nd;
  int64_t nt64, nd64, lng64, nblu64, pos64, retour;
  
  
  lseek(FGFDT[indf].fd,(off_t) 0,SEEK_SET);
  nblu = read(FGFDT[indf].fd,sign,8);
  if (strncmp(sign,CMCARC_SIGN,8) != 0) {
    nblu = read(FGFDT[indf].fd,&sign[8],17);
    if (strncmp(&sign[9],CMCARC_SIGN,8) == 0) {                   /* skip to beginning of next file */
      version=4;
      }
    else {
      if (strncmp(&sign[17],CMCARC_SIGN_V5,8) == 0) {
        version=5;
        }
      else {
        fprintf(stderr,"%s is not a CMCARC type file\n",FGFDT[indf].file_name);
        return(-1);
        }
    }
    cmcarc_file = (HEADER_CMCARC *) &sign[0];
    nt = (cmcarc_file->ntotal[0] << 24) |
    (cmcarc_file->ntotal[1] << 16) |
    (cmcarc_file->ntotal[2] <<  8) |
    (cmcarc_file->ntotal[3]);
    
    nd = (cmcarc_file->ndata[0] << 24) |
    (cmcarc_file->ndata[1] << 16) |
    (cmcarc_file->ndata[2] <<  8) |
    (cmcarc_file->ndata[3]);
    
    if (version == 5)
      nt = nd;
    else
      if (nd != 0) {
        fprintf(stderr, "%s is a CMCARC file but nd=%d\n",FGFDT[indf].file_name,nd);
        return(-1);
      }
    lng = (nt *8) - 25;
    if (lseek(FGFDT[indf].fd,(off_t)lng,SEEK_CUR) == (off_t)(-1)) {
      return (-1);
    }
  }
  subfile_length = 0;
  do {
    
    nblu = read(FGFDT[indf].fd,&cmcarc,8); /* lire nt et nd */
    if (nblu != 8) return -2;
    
    nt = (cmcarc.ntc[0] << 24) |
      (cmcarc.ntc[1] << 16) |
      (cmcarc.ntc[2] <<  8) |
      (cmcarc.ntc[3]);
    
    nd = (cmcarc.ndc[0] << 24) |
      (cmcarc.ndc[1] << 16) |
      (cmcarc.ndc[2] <<  8) |
      (cmcarc.ndc[3]);
    
    if (nt >= nd+4) {
      nt64 = nt;
      nd64 = nd;
      lng64 = (nt64 - nd64 - 2) * 8;
      tail_offset = 1;
      }
    else {
      tail_offset = 2;
      nt64 = nt;
      nt64 = (nt64 << 32) | nd;
      nblu = read(FGFDT[indf].fd,&cmcarc,8);
      nd64 = cmcarc.ntc[0];
      nd64 = (nd64 << 8) | cmcarc.ntc[1];
      nd64 = (nd64 << 8) | cmcarc.ntc[2];
      nd64 = (nd64 << 8) | cmcarc.ntc[3];
      nd64 = (nd64 << 8) | cmcarc.ndc[0];
      nd64 = (nd64 << 8) | cmcarc.ndc[1];
      nd64 = (nd64 << 8) | cmcarc.ndc[2];
      nd64 = (nd64 << 8) | cmcarc.ndc[3];
      lng64 = (nt64 - nd64 - 4) * 8;
      if (nt64 < nd64+6) {
        fprintf(stderr, "%s is a CMCARC file but nt=%ld nd=%ld\n",FGFDT[indf].file_name,nt64,nd64);
        return(-1);
        }
      }
/*    printf("Debug+ nt64=%Ld nd64=%Ld lng64=%Ld\n",nt64,nd64,lng64); */
    nblu64 = read(FGFDT[indf].fd,cmcarc.cmcarc_name,lng64);
/*    printf("Debug cmcarc.cmcarc_name=%s\n",&cmcarc.cmcarc_name[1]); */
    if (nblu64 != lng64) return -3;
    if (strcmp(FGFDT[indf].subname,&cmcarc.cmcarc_name[1]) == 0) {
      found = 1;
    }
    else {              /* sauter les donnees */
      lng64 = (nd64+tail_offset) * 8;
      if (lseek64(FGFDT[indf].fd,(off64_t)lng64,SEEK_CUR) == (off_t)(-1)) {
        return (-1);
      }
    }
  } while(!found);
  subfile_length = (nd*8)/sizeof(int32_t);
  pos64=tell64(FGFDT[indf].fd);
  retour = pos64/sizeof(int32_t);
  return(retour);
}

/****************************************************************************
*                              Q Q C O P E N                                *
*****************************************************************************
*
***function qqcopen
*
*OBJECT: Opens a non-fortran file.
*        Active part of c_waopen2 & al.
*
*ARGUMENTS: in indf  index of the file in the master file table 
*
*NOTE: Processes files contained in a CMCARC file.
*
*RETURNS: the file descriptor, if the open is successful,
*         or a negative number otherwise.
*
*/
static int qqcopen(int indf)
{
  int fd, mode;
  int ind ;
  int atoi();
  LLSK dim;
  char *errmsg="";
  
  /*    beginning of INITIALIZATION section    */
if (! init) {  
  for (ind = 0; ind < MAXWAFILES; ind++) {
    wafile[ind].file_desc = -1;
    wafile[ind].offset = 0;
  }
  init = 1;
}
/*    end of INITIALIZATION section    */

FGFDT[indf].fd = -1;
ind = 0;
while ((wafile[ind].file_desc != -1) && (ind < MAXWAFILES))
ind++;
if (ind == MAXWAFILES) {
  fprintf(stderr,"qqcopen error: too many open files\n");
  return(-1);
}

fd = -1 ;
if (FGFDT[indf].subname) {    // CMCARC type file
  if (debug_mode > 4) {
    fprintf(stderr,"Debug opening subfile %s from file %s\n",
            FGFDT[indf].subname,FGFDT[indf].file_name);
  }
  FGFDT[indf].attr.read_only = 1;
  mode = O_RDONLY;
  if ((fd = open64(FGFDT[indf].file_name,mode)) == -1) {
    fprintf(stderr,"qqcopen error: cannot open file %s\n",FGFDT[indf].file_name);
    return(-1);
  }
  wafile[ind].file_desc = fd;
  FGFDT[indf].fd = fd;
  if ((wafile[ind].offset = filepos(indf)) <= 0) {
    fprintf(stderr,"qqcopen error: subfile %s not found in %s\n",
            FGFDT[indf].subname,FGFDT[indf].file_name);
    return(-1);
  }
  FGFDT[indf].open_flag = 1;
  if (debug_mode > 4) {
    fprintf(stderr,"Debug subfile found at position %ld\n",wafile[ind].offset);
  }
}

else {                        // not a CMCARC type file
  if (access(FGFDT[indf].file_name, F_OK) == -1)
    {
      if (errno == ENOENT)     /* nouveau fichier, creation */
        {
          fd = open64(FGFDT[indf].file_name, O_RDWR | O_CREAT,
                    S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
          FGFDT[indf].attr.read_only = 0;
          errmsg="cannot create file";
        }
    }
  else
    if (! FGFDT[indf].attr.read_only)      /* tentative d'ouverture en mode R/W */
      {
        mode = O_RDWR;
        fd = open64(FGFDT[indf].file_name, mode);
        if (fd == -1) {
          if (!FGFDT[indf].attr.write_mode)
            {
              mode = O_RDONLY;
              FGFDT[indf].attr.read_only = 1;
              fd = open64(FGFDT[indf].file_name, mode);
              errmsg="cannot open file";
            }
          else                  /* ouverture demande en mode R/W */
            errmsg="cannot open in write mode";
        }
      }
    else if (FGFDT[indf].attr.read_only)  /* ouverture en mode R/O */
      {
        mode = O_RDONLY;
        fd = open64(FGFDT[indf].file_name, mode);
        errmsg="cannot open file";
      }
  if (fd == -1)
    {
      fprintf(stderr, "ERROR (qqcopen) : %s filename=(%s) !\n",errmsg,FGFDT[indf].file_name);
      return(-1);
    }
  wafile[ind].file_desc = fd;
  FGFDT[indf].fd = fd;
  FGFDT[indf].open_flag = 1;
}
dim = 0;
dim = LSEEK(fd, dim, SEEK_END);
FGFDT[indf].file_size = dim / sizeof(int32_t);
FGFDT[indf].eff_file_size = dim / sizeof(int32_t);
dim = 0;
dim = LSEEK(fd, dim, SEEK_SET);
if (subfile_length > 0) 
FGFDT[indf].eff_file_size = subfile_length;
subfile_length = 0;

return(fd);
}


/****************************************************************************
*                               D _ W A F D T                               *
*****************************************************************************
*
***function d_wafdt
*
*OBJECT: Prints the wa control table.
*
*ARGUMENTS: none.
*
*/

void d_wafdt()
{
int i;
fprintf(stderr,"\n   DUMP OF WA CONTROL TABLE \n");
for (i=0;i<MAXWAFILES;i++){
   if(wafile[i].file_desc != -1)
   fprintf(stderr,"waindex=%d, fd=%d, offset=%ld\n",
    i,wafile[i].file_desc,wafile[i].offset);
   }
}
void d_wafdt__(){ d_wafdt() ; }
void d_wafdt_(){ d_wafdt() ; }

/*****************************************************************************
*                      H R J U S T                                           *
******************************************************************************
*
***function hrjust
*
*OBJECT: Right justifies a group of ncar characters(8 bits each).
*
*ARGUMENTS: in moth   word to justify
*           in ncar number of characters (max 4)
*
*RETURNS: the group of characters right justified.
*
*/
uint32_t hrjust (uint32_t *moth, int32_t *ncar)
{
   int sc;
   sc = 8 * ( sizeof(int32_t) - *ncar );
   return (sc<=0 ? *moth : (*moth) >> sc);
}
uint32_t hrjust__ (uint32_t *moth, int32_t *ncar){ return hrjust(moth, ncar) ;}
uint32_t hrjust_ (uint32_t *moth, int32_t *ncar){ return hrjust(moth, ncar) ;}

/*****************************************************************************
*                      H L J U S T                                           *
******************************************************************************
*
***function hljust
*
*OBJECT: Left justifies a group of ncar characters(8 bits each).
*
*ARGUMENTS: in moth   word to justify 
*           in ncar   number of characters (max 4)
*
*RETURNS: the group of characters left justified.
*
*/
uint32_t hljust (uint32_t *moth, int32_t *ncar)
{
   int sc;
   sc = 8 * ( sizeof(int32_t) - *ncar );
   return (sc<=0 ? *moth : (*moth) << sc);
}
uint32_t hljust__ (uint32_t *moth, int32_t *ncar){ return hljust(moth, ncar) ;}
uint32_t hljust_ (uint32_t *moth, int32_t *ncar){ return hljust(moth, ncar) ;}
/****************************************************************************
*                              Q Q C W A W R                                *
*****************************************************************************
*
***function qqcwawr64
*
*OBJECT: Writes in a word adressable file. 
*        Active part of c_wawrit2 / c_wawrit64.
*
*ARGUMENTS: in indf   file index in file table
*           in buf    contains data to write
*           in ladr   file address in words  (origin 1)
*           in lnmots number of 32 bit words to write
*           in indf   index in the master file table
*
*/

static void qqcwawr64(int32_t *buf,uint64_t ladr, int lnmots,int indf)
{

  int nwritten;
  int ind ;
  int lfd=FGFDT[indf].fd;
  char *cbuf;
  size_t count,togo;

  ind = 0;
  while ((wafile[ind].file_desc != lfd) && (ind < MAXWAFILES))
    ind++;
  if (ind == MAXWAFILES) {
    fprintf(stderr,"qqcwawr error: filename=%s , fd=%d not found in table\n",
                  FGFDT[indf].file_name,lfd);
    exit(1);
  }

  if (ladr != 0) 
    ladr += wafile[ind].offset;

  if (FGFDT[indf].attr.read_only) {
    fprintf(stderr,"qqcwawr error: no write permission for file %s\n",FGFDT[indf].file_name);
    exit(1);
  }
  
  if(ladr!=0) WSEEK(lfd,ladr - 1, SEEK_SET);
  count = sizeof(int32_t) * lnmots;
  if ((nwritten=write(lfd, buf, count )) != count) {
    if (errno == 14) {                                              // invalid descriptor
	fprintf(stderr, "qqcwawr error: write error for file %s\n",FGFDT[indf].file_name);
	fprintf(stderr,"qqcwawr: filename=%s, buf=%p adr=%lu, nmots=%d, nwritten=%d, errno=%d\n",
		FGFDT[indf].file_name,buf,ladr,lnmots,nwritten,errno);
	fprintf(stderr, "*** Contactez un membre de la section informatique de RPN ***\n");
	fprintf(stderr, "*** Seek support from RPN informatics section ***\n");
	perror("qqcwawr");
	exit(1);
    }
    if (nwritten >= 0) {                                // not everything was written
      cbuf = (char *) buf;
      cbuf += nwritten;
      togo = (lnmots * sizeof(int32_t)) - nwritten;
      nwritten = write(lfd,buf,togo);                   // try to write missing tail part of previous write
      fprintf(stderr,"qqcwawr WARNING: multiple write attempt of file %s last write=%ld bytes, total needed=%ld bytes\n",
	      FGFDT[indf].file_name,togo,lnmots*sizeof(int32_t));
      if (nwritten != togo) {                           // second attempt failed
	fprintf(stderr, "qqcwawr error: write error for file %s\n",FGFDT[indf].file_name);
	fprintf(stderr,"qqcwawr: filename=%s, buf=%p adr=%lu, nmots=%d, nwritten=%d, errno=%d\n",
		FGFDT[indf].file_name,buf,ladr,lnmots,nwritten,errno);
	perror("qqcwawr");
	exit(1);
      }
    }else{
      fprintf(stderr, "qqcwawr error: write error or file not open for write!\n");
      fprintf(stderr,"qqcwawr: filename=%s, buf=%p adr=%lu, nmots=%d, nwritten=%d, errno=%d\n",
	      FGFDT[indf].file_name,buf,ladr,lnmots,nwritten,errno);
      perror("qqcwawr");
      exit(1);
    }
  }
  if (ladr+lnmots-1 > FGFDT[indf].file_size) {
    FGFDT[indf].file_size = ladr+lnmots-1;
    FGFDT[indf].eff_file_size = ladr+lnmots-1;
  }
}

/****************************************************************************
*                              Q Q C W A R D                                *
*****************************************************************************
*
***function qqcward64
*
*OBJECT: Reads from a word addressable file.
*        Active part of c_waread2 / c_waread64.
*
*ARGUMENTS: in  indf    index of file in the master file table
*           out buf     will contain data read
*           in  ladr    file address in words (origin 1)
*           in  lnmots  number of 32 bit words to read
*
*/
static void qqcward64(int32_t *buf,uint64_t ladr,int lnmots,int indf)
{
  int was_read, ind;
  int lfd=FGFDT[indf].fd;
  size_t count;

  ind = 0;
  while ((wafile[ind].file_desc != lfd) && (ind < MAXWAFILES))
    ind++;
  if (ind == MAXWAFILES) {
    fprintf(stderr,"qqcward error: fd=%d not found in table\n",lfd);
    exit(1);
  }
  if (ladr != 0) 
    ladr += wafile[ind].offset;

  if(ladr!=0) WSEEK(lfd, ladr - 1, SEEK_SET);
  count = sizeof(int32_t) * lnmots;
  was_read=read(lfd, buf, count);
  if(was_read != sizeof(int32_t)*lnmots) {
      fprintf(stderr,"qqcward error: tried to read %ld bytes, only read %d\n",
		    sizeof(int32_t)*lnmots,was_read);
      fprintf(stderr,"qqcward: wafile[ind].offset=%ld ladr=%ld\n",wafile[ind].offset,ladr);
      exit(1);
  }
}

// small embedded test routine
void TEST_c_fnom()
{
  intptr_t iun = 600;
  int iun2 = 0;
  int lrec = 0;
  int status;
  fprintf(stderr," ========== test_c_fnom ==========\n");
  status = c_fnom((int *)iun,"C_file","RND+STD",lrec);
  if(status != 0) { fprintf(stderr," ERROR\n"); exit(1) ; }
  status = c_fnom(&iun2,"C_file2","RND+STD",lrec);
  if(status != 0) { fprintf(stderr," ERROR\n"); exit(1) ; }
  fprintf(stderr," PASSED\n");
}

#if defined(SELF_TEST)
int main(int argc, char **argv)
{
  TEST_c_fnom();
}
// void f_tracebck(){}
#endif
