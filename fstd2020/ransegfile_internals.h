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
} start_of_record ;

// the size of the following struct MUST be a MULTIPLE of 64 bits
typedef struct{                     // trailer for all records (allows little/big endian detection)
  uint64_t rt:8, rl:48, zr:8    ;   // zero (8 bits) | record length (48 bits) | record type (8bits)
} end_of_record ;

// the size of the following struct MUST be a MULTIPLE of 64 bits
typedef struct{           // SOS (Start Of Segment) record
  start_of_record head ;  // rt=3, zr=0, rl=4
  unsigned char sig1[8] ; // RSF marker + application marker ('RSF0FsT2' for standard files 2020)
  uint64_t sig2 ;         // 0xDEADBEEFFEEBDAED (bi-endian signature)
  end_of_record tail ;    // rt=3, zr=0, rl=4
} start_of_segment ;

// the size of the following struct MUST be a MULTIPLE of 64 bits
typedef struct{           // EOS (End Of Segment) record
  start_of_record head ;  // rt=4, zr=0, rl=5
  uint64_t sig1 ;         // 0xBEBEFADAADAFEBEB (bi-endian signature)
  uint64_t segl ;         // segment length (64 bit units)
  uint64_t sig2 ;         // 0xCAFEFADEEDAFEFAC (bi-endian signature)
  end_of_record tail ;    // rt=4, zr=0, rl=5
} end_of_segment ;

// the size of the following struct MUST be a MULTIPLE of 64 bits (after allocation)
typedef struct{                  // allocated size should be 1 + nslots * direntry_size
  uint32_t nused ;               // number of directory entries in use
  uint32_t nslots ;              // max number of entries in directory
  uint64_t data[] ;
} dir_body ;

typedef struct WARL WARL ;
struct WARL{
  uint64_t wa ;    // word address in file
  uint64_t rl ;    // record length
} ;

typedef struct DIR_PAGE DIR_PAGE ;

#define DIR_PAGE_SIZE 512
struct DIR_PAGE{    // directory page
  DIR_PAGE *next ;
  uint32_t nused ;               // number of entries in use
  uint32_t nslots ;              // max number of entries in page
  WARL warl[DIR_PAGE_SIZE] ;     // file address + length for record
  uint32_t meta[] ;              // metadata [nslots, direntry_size]
} ;

#define NULL_RSF_file {RSF_VERSION, -1, NULL, NULL, NULL, NULL, &rsf_default_match, 0, 0, 0, 0, 0, {0, 0, 0}, {0, 0, 0}, 0, 0, 0, 0, 0, 0, 0, 0 }

typedef struct RSF_file RSF_file;

struct RSF_file{                 // internal structure for access to Random Segmented Files
  uint32_t version ;
  int32_t fd ;
  DIR_PAGE *dirpages ;           // pointer to directory pages chain
  DIR_PAGE *lastpage ;           // pointer to last directory page
  RSF_file *next ;               // pointer to next file if "linked"
  uint32_t *mask ;               // mask used for metadata matches
  RSF_match_fn *match ;          // pointer to metadata matching function
  off_t file_pos ;               // current file position
  off_t file_siz ;               // file size (should NEVER SHRINK)
  off_t next_write ;             // position for next write
  uint32_t head_type ;           // 0 : invalid, 1: read, 2 : written
  uint32_t tail_type ;           // 0 : invalid, 1: read, 2 : written
  start_of_record  last_head ;   // last record header written/read (validated by head_type)
  end_of_record    last_tail ;   // last record trailer written/read (validated by tail_type)
  uint32_t mode ;                // file mode (RO/RW/AP)
  uint32_t direntry_size ;       // size of a directory entry (in 32 bit units)
  uint32_t direntry_used ;       // number of directory entries in use (all pages)
  uint32_t direntry_slots ;      // max number of entries in directory (nb of directory pages * DIR_PAGE_SIZE)
  uint32_t nwritten ;            // number of records written
  uint32_t nread ;               // number of records read
  uint32_t buf_used ;            // number of used words in buffer (see RSF_file_flex)
  uint32_t buf_size ;            // size of dynamic buffer (see RSF_file_flex)
} ;

struct RSF_file_flex{            // RSF_file with flexible array at end
  RSF_file f ;
  uint32_t buf[] ;               // buffer used to build records
} ;