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

// Random Segmented Files, version 1.0.0, INTERNAL PRIVATE definitions

#include <sys/types.h>
#include <unistd.h>
#include <sys/resource.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <ransegfile.h>

// record type rt
// 0   : illegal
// 1   : data record
// 2   : segment directory record
// 3   : start of segment record
// 4   : end of segment record
// 255 : deleted record
//
// rt:rl:zr is also used for endianness detection (most significant byte always zero, least significant byte 1/2/3/4/255) 
// the zr field is ALWAYS 0
// max record length is 2**48 * 8 - 1 bytes (rl -s in 64 its units)
#define RT_DATA 1
#define RT_DIR  2
#define RT_SOS  3
#define RT_EOS  4
#define RT_DEL  0xFF
#define RL_SOR ( sizeof(start_of_record)/sizeof(uint64_t) )
#define RL_EOR ( sizeof(end_of_record)  /sizeof(uint64_t) )
#define RL_SOS ( sizeof(start_of_segment)/sizeof(uint64_t) )
#define RL_EOS ( sizeof(end_of_segment)  /sizeof(uint64_t) )
#define RL_EMPTY_DIR ( (sizeof(dir_body)/sizeof(uint64_t)) + RL_SOR + RL_EOR )

// the size of the following struct MUST be a MULTIPLE of 64 bits
typedef struct{                     // header for all records (allows little/big endian detection)
  uint64_t rt:8, rl:48, zr:8    ;   // zero (8 bits) | record length (48 bits) | record type (8bits)
} start_of_record ;   // SOR

#define NULL_SOR {0, 0, 0}

// the size of the following struct MUST be a MULTIPLE of 64 bits
typedef struct{                     // trailer for all records (allows little/big endian detection)
  uint64_t rt:8, rl:48, zr:8    ;   // zero (8 bits) | record length (48 bits) | record type (8bits)
} end_of_record ;     // EOR

#define NULL_EOR {0, 0, 0}

//      ZR        LNG         RT                                              RT        LNG         ZR
//    +----+----------------+----+------------+---------+---------+---------+----+----------------+----+
//    |0x00| 0x000000000006 |0x03| 'APPLSRF0' |   DESZ  |  SEGL   |   SIG2  |0x03| 0x000000000006 |0x00|
//    +----+----------------+----+------------+---------+---------+---------+----+----------------+----+
//    8 bits     48 bits    8 bits  8x8 bits    64 bits  64 bits    64 bits 8 bits     48 bits    8 bits
// 
//    LNG   length      : 6 64 bit elements
//    ZR    zero byte   : used as endianness indicator
//    RT    record type : 3 (Start Of Segment)
//    SIGN  file type signature (APPL would be FsT2 for "RPN standard" files version 2)
//    DESZ  directory entry size ( + size of address block if applicable ?)
//    SIG2  0xDEADBEEFFEEBDAED (bi-endian signature)
//    SEGL  length of segment in 64 bit units, including S O S and E O S 

// the size of the following struct MUST be a MULTIPLE of 64 bits
typedef struct{           // SOS (Start Of Segment) record
  start_of_record head ;  // rt=3, zr=0, rl=6
  unsigned char sig1[8] ; // RSF marker + application marker ('RSF0FsT2' for standard files 2020)
  uint64_t direntry_size ;  // size of directory entry
  uint64_t segl ;         // segment length (in 64 bit units)
  uint64_t sig2 ;         // 0xDEADBEEFFEEBDAED (bi-endian signature)
  end_of_record tail ;    // rt=3, zr=0, rl=6
} start_of_segment ;

//                        SOR                      sig1               direntry_size   sig2              EOR
#define EMPTY_SOS {{RT_SOS ,RL_SOS, 0}, {'R','S','F','0','<','-','-','>'}, 0, 0xDEADBEEFFEEBDAED, {RT_SOS ,RL_SOS, 0}}

//      ZR        LNG         RT                                                                          RT        LNG         ZR
//    +----+----------------+----+----------+---------+---------+         +---------+---------+---------+----+----------------+----+
//    |0x00| 0x000000000008 |0x03|   SIG1   |   DESZ  |   SEGL  |...GAP...|   DESZ  |  SEGL   |   SIG2  |0x03| 0x000000000008 |0x00|
//    +----+----------------+----+----------+---------+---------+         +---------+---------+---------+----+----------------+----+
//    8 bits     48 bits    8 bits 64 bits    64 bits   64 bits             64 bits  64 bits    64 bits 8 bits     48 bits    8 bits
// 
//    LNG   length      : 6 64 bit elements
//    ZR    zero byte   : used as endianness indicator
//    RT    record type : 3 (Start Of Segment)
//    SIG1  0xBEBEFADAADAFEBEB (bi-endian signature)
//    DESZ  directory entry size ( + size of address block if applicable ?)
//    GAP   if file is sparse, unoccupied space so that the EOS end resides on an address block
//          the segment size will then be a multiple of the address block
//          in this case, 2 copies of the EOS will be found, overlapping GAP at both ends
//    SIG2  0xDEADBEEFFEEBDAED (bi-endian signature)
//    SEGL  length of segment in 64 bit units, including S O S and E O S 

// the size of the following struct MUST be a MULTIPLE of 64 bits
typedef struct{           // EOS (End Of Segment) record
  start_of_record head ;  // rt=4, zr=0, rl=8 + size of gap
  uint64_t sig1 ;         // 0xBEBEFADAADAFEBEB (bi-endian signature)
  uint64_t direntry_size ;  //  size of directory entry,  ( + size of address block if applicable ?)
  uint64_t Segl ;         // segment length (in 64 bit units)
  uint64_t desz ;         // size of directory entry, ( + size of address block if applicable ?)
  uint64_t segl ;         // segment length (in 64 bit units)
  uint64_t sig2 ;         // 0xCAFEFADEEDAFEFAC (bi-endian signature)
  end_of_record tail ;    // rt=4, zr=0, rl=8 + size of gap
} end_of_segment ;

//                         SOR                 sig1    direntry_size         segl                          sig2                EOR
#define EMPTY_EOS {{RT_EOS, RL_EOS, 0}, 0xBEBEFADAADAFEBEB, 0, (sizeof(zero_file)/sizeof(uint64_t)), 0xCAFEFADEEDAFEFAC, {RT_EOS, RL_EOS, 0}}

// this reflects the format on storage media (disk)
// start_of_record | dir_body | end_of_record
// the size of the following struct MUST be a MULTIPLE of 64 bits (after allocation)
typedef struct{                  // allocated size should be 1 + nslots * direntry_size
//   uint64_t segment_base ;        // meaningless when on disk, gets adjusted in memory when segment directory is read
  uint32_t disk_entry_size ;     // size of a directory entry on disk (metadata + WA + RL)
  uint32_t nused ;               // number of directory entries in use
//   uint32_t nslots ;              // max number of entries in directory
  uint32_t data[] ;              // directory entries, [nused, entry_size] entry_size = direntry_size + 4 (WARL size)
} dir_body ;
#define EMPTY_DIR_BODY   {0, 0}
#define EMPTY_DIR_RECORD {RT_DIR ,RL_EMPTY_DIR, 0}, EMPTY_DIR_BODY , {RT_DIR, RL_EMPTY_DIR, 0}

typedef struct WARL WARL ;
struct WARL{
  uint64_t wa ;    // word address in file
  uint64_t rl ;    // record length
} ;

// directory record on storage device templates
//
// disk entry in the directory on storage device
// used by disk_directory
//
typedef struct{
  uint64_t wa ;       // word address relative to start of segment
  uint64_t rl ;       // record length
  uint32_t meta[] ;   // normally [direntry_size]
} disk_dir_entry ;
//
// the end of record address has to be fudged as the address of directory.entry[direntry_size * entries_nused]
//
// the disk_dir_entry address has to be fudged as the address of directory.entry[direntry_size * entry_index]
//
// disk_directory allocated size should be 
//    sizeof(start_of_record) + 
//    sizeof(disk_directory)  + 
//    direntry_size * entries_nused * sizeof(unit32_t) + 
//    sizeof(end_of_record)
// the directory will normally be followed on the storage device by an End Of Segment record
//
typedef struct{
  start_of_record sor ;
  uint32_t entries_nused ;          // number of directory entries used
  uint32_t direntry_size ;          // size of a directory entry (in 32 bit units)
  uint32_t entry[] ;                // entry[] cannot be used directly
} disk_directory ;                  // doubly 

typedef struct DIR_PAGE DIR_PAGE ;

// the following 3 defines MUST BE KEPT CONSISTENT, DIR_PAGE_SIZE is a POWER OF 2, DIR_PAGE_SHFT is log2(DIR_PAGE_SIZE)
#define DIR_PAGE_SIZE 512
#define DIR_PAGE_MASK (DIR_PAGE_SIZE-1)
#define DIR_PAGE_SHFT 9

struct DIR_PAGE{    // directory page
  uint32_t nused ;               // number of entries in use
  uint32_t nslots ;              // max number of entries in page
  WARL warl[DIR_PAGE_SIZE] ;     // file address + length for record
  uint32_t meta[] ;              // metadata [nslots, direntry_size] (allocated as [DIR_PAGE_SIZE, direntry_size] )
} ;

typedef struct RSF_file RSF_file;

#define DEFAULT_PAGE_TABLE 128
struct RSF_file{                 // internal (in memory) structure for access to Random Segmented Files
  uint32_t version ;
  int32_t  fd ;                  // OS file descriptor (-1 if invalid)
  int32_t  file_slot ;           // slot number of file (-1 if invalid)
  int32_t  cur_rec ;             // current record index in curpage (-1 if invalid)
  uint32_t *mask ;               // mask used for metadata matches (NULL if not used)
  RSF_match_fn *match ;          // pointer to metadata matching function
  DIR_PAGE **pagetable ;         // directory page table (pointers to directory pages for this file)
  RSF_file *next ;               // pointer to next file if "linked" (NULL if not linked)
  int32_t  dirpages ;            // number of available directory pages
  int32_t  curpage ;             // current page in use (may be -1)
  int32_t  lastpage ;            // last directory page in use (-1 
  off_t file_pos ;               // current file position
  off_t file_siz ;               // file size (should NEVER SHRINK)
  off_t next_write ;             // position for next write
  uint32_t head_type ;           // 0 : invalid, 1: read, 2 : written
  uint32_t tail_type ;           // 0 : invalid, 1: read, 2 : written
  start_of_record  last_head ;   // last record header written/read (validated by head_type)
  end_of_record    last_tail ;   // last record trailer written/read (validated by tail_type)
  uint32_t mode ;                // file mode (RO/RW/AP)
  uint32_t direntry_size ;       // size of a directory entry (in 32 bit units)
  uint32_t direntry_used ;       // number of directory entries in use (all pages belonging to this file)
  uint32_t direntry_slots ;      // max number of entries in directory (nb of directory pages * DIR_PAGE_SIZE)
  uint32_t directory_size ;      // size of directory record (in 64 bit units)
  uint32_t nwritten ;            // number of records written
  uint32_t nread ;               // number of records read
  uint32_t buf_used ;            // number of used words in buffer (see RSF_file_flex)
  uint32_t buf_size ;            // size of dynamic buffer (see RSF_file_flex)
} ;

#define NULL_RSF_file {RSF_VERSION, -1, -1, -1, NULL, &rsf_default_match, NULL, NULL, -1, -1, -1, 0, 0, 0, 0, 0, NULL_SOR, NULL_EOR, 0, 0, 0, 0, 0, 0, 0, 0, 0 }

struct RSF_file_flex{            // RSF_file with flexible array at end
  RSF_file f ;
  uint32_t buf[] ;               // buffer used to build records
} ;
