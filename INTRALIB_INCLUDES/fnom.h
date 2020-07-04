#define MAXFILES 1024
// TO DO : add attribute to indicate that a file may be sparse
// typedef struct {
//   uint32_t 
//      stream:1,  std:1,     burp:1,  rnd:1,  wa:1,         ftn:1,    unf:1,    read_only:1, 
//      old:1,     scratch:1, paged:1, pipe:1, write_mode:1, remote:1, sparse:1, atomic:1,
//      nopages:1, dopages:1, mapped:1,
//      padding:13;
// } attributs_plus;
typedef struct {
  unsigned int 
     stream:1, std:1, burp:1, rnd:1, wa:1, ftn:1,
     unf:1, read_only:1, old:1, scratch:1, notpaged:1,
     pipe:1, write_mode:1, remote:1, wap:1, stdp:1,  padding:16;
} attributs;
typedef struct {
  char * file_name;               // complete file name
  char * subname;                 // sub file name for cmcarc files
  char * file_type;               // file type and options
  int64_t file_size;              // file size in words
  int64_t eff_file_size;          // effective file size in words
  int32_t iun;                    // fnom unit number
  int32_t fd;                     // c file descriptor
  int32_t lrec;                   // record length when appliable
  int32_t open_flag;              // open/close flag
  attributs attr;                 // file attributes
} general_file_info ;

#if defined(FNOM_OWNER)
general_file_info Fnom_General_File_Desc_Table[MAXFILES];
#else
extern general_file_info Fnom_General_File_Desc_Table[MAXFILES];
#endif
#define FGFDT Fnom_General_File_Desc_Table

int c_fretour(int iun);
void (d_fgfdt)();
int c_fnom(int *iun,char *nom,char *type,int lrec);
int c_fnom_callback(int *iun,char *nom,char *type,int lrec,int (*f90open)(), int (*f90clos)());
int c_fclos(int iun);

void c_waopen(int iun);
int c_waopen2(int iun);

void c_waclos(int iun);
int c_waclos2(int iun);

void c_wawrit(int iun,void *buf,unsigned int adr,int nmots);
int c_wawrit2(int iun,void *buf,unsigned int adr,int nmots);
int c_wawrit64(int iun,void *buf,uint64_t adr,unsigned int nmots,unsigned int part);

void c_waread(int iun,void *buf,unsigned int adr,int nmots);
int c_waread2(int iun,void *buf,unsigned int adr,int nmots);
int c_waread64(int iun,void *buf,uint64_t adr,unsigned int nmots,unsigned int part);

int32_t c_wasize(int iun);
int32_t c_numblks(int iun);

void c_openda(int iun);
void c_closda(int iun);
void c_checda(int iun);
void c_readda(int iun,int *bufptr,int ns,int is);
void c_writda(int iun,int *bufptr,int ns,int is);

int c_getfdsc(int iun);
void c_sqopen(int iun);
void c_sqrew(int iun);
void c_sqeoi(int iun);
int c_sqgetw(int iun, int32_t *bufptr, int nmots);
int c_sqputw(int iun, int32_t *bufptr, int nmots);
int c_sqgets(int iun, char *bufptr, int nchar);
int c_sqputs(int iun, char *bufptr, int nchar);
void d_wafdt();  
