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

// Random Segmented Files PUBLIC interface
#define RSF_VERSION 10000

// RSF_RO implies that the file MUST exist
#define RSF_RO    2
// RSF_RW implies create file if it does not exist
#define RSF_RW    4
// RSF_AP implies that the file MUST exist and will be written into (implies RSF_RW)
#define RSF_AP    8
// RSF_SEG1 means consolidate segments into ONE (invalid if RSF_RW not set)
#define RSF_SEG1  1024

#define _LARGEFILE64_SOURCE

#include <stdint.h>

typedef struct{   // this struct only contains a pointer to the actual full control structure
  void *p ;
} RSF_handle ;

// typedef int32_t (*int_function)() ;
typedef int32_t RSF_match_fn(uint32_t *criteria, uint32_t *meta, uint32_t *mask, int nitems) ;

RSF_handle rsf_open(char *name, int32_t options, char *app, int32_t dirsize, int32_t entrysize, int32_t bufsz) ;

int32_t rsf_close (RSF_handle rsf) ;

int64_t rsf_seek(RSF_handle rsf, void *metadata, void *mask) ;

RSF_match_fn *rsf_set_match(RSF_handle rsf, RSF_match_fn *fn) ;

int32_t rsf_default_match(uint32_t *criteria, uint32_t *meta, uint32_t *mask, int nitems) ;
int32_t rsf_base_match(uint32_t *criteria, uint32_t *meta, uint32_t *mask, int nitems) ;

int32_t rsf_match(RSF_handle rsf, uint32_t *criteria, uint32_t *meta, uint32_t *mask, int nitems) ;

void RSF_filedump(RSF_handle h) ;
