// compensated sums for FORTRAN 
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

#if ! defined(VOLATILE)
#define VOLATILE
#else
#undef VOLATILE
#define VOLATILE volatile
#endif

void AddToDot48_c(double *sum, double *err, float *a, float *b, int n, int fold){
  double VOLATILE  S[4], E[4] ; // volatile attribute needed to prevent unwanted optimization
  double VOLATILE  Z[4], Y[4] ; // volatile attribute needed to prevent unwanted optimization
  int  i, j, i0 ;
  double  input ;
#define input(x) ((double)a[x] * (double)b[x] )
#include "compensated_sums_c.h"
#undef input
}

void AddToDot8_c(double *sum, double *err, double *a, double *b, int n, int fold){
  double VOLATILE  S[4], E[4] ; // volatile attribute needed to prevent unwanted optimization
  double VOLATILE  Z[4], Y[4] ; // volatile attribute needed to prevent unwanted optimization
  int  i, j, i0 ;
#define input(x) (a[x] * b[x] )
#include "compensated_sums_c.h"
#undef input
}

void AddToSum48_c(double *sum, double *err, float *a, int n, int fold){
  double VOLATILE  S[4], E[4] ; // volatile attribute needed to prevent unwanted optimization
  double VOLATILE  Z[4], Y[4] ; // volatile attribute needed to prevent unwanted optimization
  int  i, j, i0 ;
  double  input ;
#define input(x) ((double)a[x])
#include "compensated_sums_c.h"
#undef input
}

void AddToSum8_c(double *sum, double *err, double *a, int n, int fold){
  double VOLATILE  S[4], E[4] ; // volatile attribute needed to prevent unwanted optimization
  double VOLATILE  Z[4], Y[4] ; // volatile attribute needed to prevent unwanted optimization
  int  i, j, i0 ;
  double  input ;
#define input(x) a[x]
#include "compensated_sums_c.h"
#undef input
}
