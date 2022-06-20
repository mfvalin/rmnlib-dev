// compensated sums for FORTRAN and C
// Copyright (C) 2022  Recherche en Prevision Numerique
// 
// This software is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation,
// version 2.1 of the License.
// 
// This software is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
// 
// \author M. Valin,   Recherche en Prevision Numerique
// \date 2022

// the main procedure body is the same for all and comes from compensated_sums_c.h

// -DVOLATILE may be needed to defeat some aggressive optimizations (as in -Ofast)
// -DVOLATILE is highly detrimental to performance

#if ! defined(VOLATILE)
#define VOLATILE
#else
#undef VOLATILE
#define VOLATILE volatile
#endif

double CoNsTaNt1[4] = {1.0, 1.0, 1.0, 1.0 } ;

// structs for accumulators, function protptypes, macros
#include <compensated_sums.h>

// input data : 4 byte floats
// partial sum and error accumulators : 4 byte floats
void AddToDot4(float *sum, float *err, float *a, float *b, int n, int fold){
  float VOLATILE  S[4], E[4] ; // volatile attribute needed to prevent unwanted optimization
  float VOLATILE  Z[4], Y[4] ; // volatile attribute needed to prevent unwanted optimization
  int  i, j, i0 ;
#define input(x) (a[x] * b[x])
#include "compensated_sums_c.h"
#undef input
}

// input data : 4 byte floats
// partial sum and error accumulators : 8 byte doubles
void AddToDot48(double *sum, double *err, float *a, float *b, int n, int fold){
  double VOLATILE  S[4], E[4] ; // volatile attribute needed to prevent unwanted optimization
  double VOLATILE  Z[4], Y[4] ; // volatile attribute needed to prevent unwanted optimization
  int  i, j, i0 ;
#define input(x) ((double)a[x] * (double)b[x])
#include "compensated_sums_c.h"
#undef input
}

// input data : 8 byte doubles
// partial sum and error accumulators : 8 byte doubles
void AddToDot8(double *sum, double *err, double *a, double *b, int n, int fold){
  double VOLATILE  S[4], E[4] ; // volatile attribute needed to prevent unwanted optimization
  double VOLATILE  Z[4], Y[4] ; // volatile attribute needed to prevent unwanted optimization
  int  i, j, i0 ;
#define input(x) (a[x] * b[x] )
#include "compensated_sums_c.h"
#undef input
}

// input data : 4 byte floats
// partial sum and error accumulators : 4 byte floats
void AddToSum4(float *sum, float *err, float *a, int n, int fold){
  float VOLATILE  S[4], E[4] ; // volatile attribute needed to prevent unwanted optimization
  float VOLATILE  Z[4], Y[4] ; // volatile attribute needed to prevent unwanted optimization
  int  i, j, i0 ;
#define input(x) a[x]
#include "compensated_sums_c.h"
#undef input
}

// input data : 4 byte floats
// partial sum and error accumulators : 8 byte doubles
void AddToSum48(double *sum, double *err, float *a, int n, int fold){
  double VOLATILE  S[4], E[4] ; // volatile attribute needed to prevent unwanted optimization
  double VOLATILE  Z[4], Y[4] ; // volatile attribute needed to prevent unwanted optimization
  int  i, j, i0 ;
#define input(x) ((double)a[x])
#include "compensated_sums_c.h"
#undef input
}

// input data : 8 byte doubles
// partial sum and error accumulators : 8 byte doubles
void AddToSum8(double *sum, double *err, double *a, int n, int fold){
  double VOLATILE  S[4], E[4] ; // volatile attribute needed to prevent unwanted optimization
  double VOLATILE  Z[4], Y[4] ; // volatile attribute needed to prevent unwanted optimization
  int  i, j, i0 ;
#define input(x) a[x]
#include "compensated_sums_c.h"
#undef input
}

#if defined(CHECK_SYNTAX)
// syntax check for macros from compensated_sums.h, no code generation expected
static void test_syntax(){
  csum8 c8 ;
  csum4 c4 ;
  double a8[1], b8[1] ;
  float  a4[1], b4[1] ;
   ADDTODOT4(c4, a4, b4, 1, 1) ;
   ADDTOSUM4(c4, a4,     1, 1) ;
  ADDTODOT48(c8, a4, b4, 1, 1) ;
  ADDTOSUM48(c8, a4,     1, 1) ;
   ADDTODOT8(c8, a8, b8, 1, 1) ;
   ADDTOSUM8(c8, a8,     1, 1) ;
}
#endif
