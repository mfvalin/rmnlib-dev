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

#include <stdint.h>

// record type
// 0   : illegal
// 1   : data record
// 2   : segment directory record
// 3   : start of segment record
// 4   : end of segment record
// 255 : deleted record
//
// lng_hi_rt is also used for endianness detection (most significant byte always zero, least significant byte 1/2/3/4/255) 
#define RT_DATA 1
#define RT_DIR  2
#define RT_SOS  3
#define RT_EOS  4
#define RT_DEL  0xFF

typedef struct{
  uint32_t lng_lo ;      // record length (lower 32 bits) (64 bit units)
  uint32_t lng_hi_rt ;   // zero (8 bits) | record length (upper 16 bits) | record type (8bits)
} start_of_record ;

typedef struct{
  uint32_t lng_hi_rt ;   // zero (8 bits) | record length (upper 16 bits) | record type (8bits)
  uint32_t lng_lo ;      // record length (lower 32 bits) (64 bit units)
} end_of_record ;

typedef struct{
  start_of_record head ;
  uint32_t appl ;         // application marker ('FsT2' for standard files 2020)
  uint32_t srf0 ;         // 'SRF0'
  end_of_record tail ;
} start_of_segment ;

typedef struct{
  start_of_record head ;
  uint32_t cafe ;         // 0xCAFE
  uint32_t fade ;         // 0xFADE
  uint32_t segl_lo ;      // segment length (lower 32 bits) (64 bit units)
  uint32_t segl_hi ;      // segment length (upper 32 bits) (64 bit units)
  uint32_t bebe ;         // 0xBEBE
  uint32_t fada ;         // 0xFADA
  end_of_record tail ;
} end_of_segment ;
