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

  for(i=0 ; i<4 ; i++) S[i] = sum[i] ;
  for(i=0 ; i<4 ; i++) E[i] = err[i] ;
  i0 = n & 0x3 ;

  for(j = 0 ; j < i0 ; j++) {        // the first (1/2/3) sums (if applicable)
    Z[j] = S[j] ;
    Y[j] = input(j) + E[j] ;
    S[j] = Z[j] + Y[j] ;
    E[j] = (Z[j] - S[j]) + Y[j] ;
  }
  for(i = i0 ; i < n-3 ; i += 4 ) {  // 4 parallel sums
    for(j=0 ; j<4 ; j++) {
      Z[j] = S[j] ;                  // save current value of sum
      Y[j] = input(i+j) + E[j] ;     // add accumulated error and input
      S[j] = Z[j] + Y[j] ;           // add new term to sum
      E[j] = (Z[j] - S[j]) + Y[j] ;  // new error = (old sum - new sum) + (new term + error)
    }
  }

  if(fold) {
    for(j=1 ; j<4 ; j++) {         // fold everything into S[0], E[0]
      E[0] = E[0] + E[j] ;         // fold error terms
      E[j] = 0.0 ;
      Z[0] = S[0] ;                // save current value of sum
      Y[0] = S[j] + E[0] ;         // add accumulated error and input
      S[j] = 0.0 ;
      S[0] = Z[0] + Y[0] ;         // add new term to sum
      E[0] = (Z[0] - S[0]) + Y[0] ;// new error = (old sum - new sum) + (new term + error)
    }
    sum[0] = S[0] ;
    err[0] = E[0] ;
  }else{
    for(i=0 ; i<4 ; i++) sum[i] = S[i] ;
    for(i=0 ; i<4 ; i++) err[i] = E[i] ;
  }
#undef input
}

void AddToDot8_c(double *sum, double *err, double *a, double *b, int n, int fold){

  double VOLATILE  S[4], E[4] ; // volatile attribute needed to prevent unwanted optimization
  double VOLATILE  Z[4], Y[4] ; // volatile attribute needed to prevent unwanted optimization
  int  i, j, i0 ;
#define input(x) (a[x] * b[x] )

  for(i=0 ; i<4 ; i++) S[i] = sum[i] ;
  for(i=0 ; i<4 ; i++) E[i] = err[i] ;
  i0 = n & 0x3 ;

  for(j = 0 ; j < i0 ; j++) {        // the first (1/2/3) sums (if applicable)
    Z[j] = S[j] ;
    Y[j] = input(j) + E[j] ;
    S[j] = Z[j] + Y[j] ;
    E[j] = (Z[j] - S[j]) + Y[j] ;
  }
  for(i = i0 ; i < n-3 ; i += 4 ) {  // 4 parallel sums
    for(j=0 ; j<4 ; j++) {
      Z[j] = S[j] ;                  // save current value of sum
      Y[j] = input(i+j) + E[j] ;     // add accumulated error and input
      S[j] = Z[j] + Y[j] ;           // add new term to sum
      E[j] = (Z[j] - S[j]) + Y[j] ;  // new error = (old sum - new sum) + (new term + error)
    }
  }

  if(fold) {
    for(j=1 ; j<4 ; j++) {         // fold everything into S[0], E[0]
      E[0] = E[0] + E[j] ;         // fold error terms
      E[j] = 0.0 ;
      Z[0] = S[0] ;                // save current value of sum
      Y[0] = S[j] + E[0] ;         // add accumulated error and input
      S[j] = 0.0 ;
      S[0] = Z[0] + Y[0] ;         // add new term to sum
      E[0] = (Z[0] - S[0]) + Y[0] ;// new error = (old sum - new sum) + (new term + error)
    }
    sum[0] = S[0] ;
    err[0] = E[0] ;
  }else{
    for(i=0 ; i<4 ; i++) sum[i] = S[i] ;
    for(i=0 ; i<4 ; i++) err[i] = E[i] ;
  }
#undef input
}

void AddToSum48_c(double *sum, double *err, float *a, int n, int fold){

  double VOLATILE  S[4], E[4] ; // volatile attribute needed to prevent unwanted optimization
  double VOLATILE  Z[4], Y[4] ; // volatile attribute needed to prevent unwanted optimization
  int  i, j, i0 ;
  double  input ;
#define input(x) ((double)a[x])

  for(i=0 ; i<4 ; i++) S[i] = sum[i] ;
  for(i=0 ; i<4 ; i++) E[i] = err[i] ;
  i0 = n & 0x3 ;

  for(j = 0 ; j < i0 ; j++) {                      // the first (1/2/3) sums (if applicable)
    Z[j] = S[j] ;
    Y[j] = input(j) + E[j] ;
    S[j] = Z[j] + Y[j] ;
    E[j] = (Z[j] - S[j]) + Y[j] ;
  }
  for(i = i0 ; i < n-3 ; i += 4 ) {                // 4 parallel sums
    for(j=0 ; j<4 ; j++) {
      Z[j] = S[j] ;                  // save current value of sum
      Y[j] = input(i+j) + E[j] ;     // add accumulated error and input
      S[j] = Z[j] + Y[j] ;           // add new term to sum
      E[j] = (Z[j] - S[j]) + Y[j] ;  // new error = (old sum - new sum) + (new term + error)
    }
  }

  if(fold) {
    for(j=1 ; j<4 ; j++) {                   // fold everything into S[0], E[0]
      E[0] = E[0] + E[j] ;         // fold error terms
      E[j] = 0.0 ;
      Z[0] = S[0] ;                // save current value of sum
      Y[0] = S[j] + E[0] ;         // add accumulated error and input
      S[j] = 0.0 ;
      S[0] = Z[0] + Y[0] ;         // add new term to sum
      E[0] = (Z[0] - S[0]) + Y[0] ;// new error = (old sum - new sum) + (new term + error)
    }
    sum[0] = S[0] ;
    err[0] = E[0] ;
  }else{
    for(i=0 ; i<4 ; i++) sum[i] = S[i] ;
    for(i=0 ; i<4 ; i++) err[i] = E[i] ;
  }
#undef input
}

void AddToSum8_c(double *sum, double *err, double *a, int n, int fold){

  double VOLATILE  S[4], E[4] ; // volatile attribute needed to prevent unwanted optimization
  double VOLATILE  Z[4], Y[4] ; // volatile attribute needed to prevent unwanted optimization
  int  i, j, i0 ;
  double  input ;
#define input(x) a[x]

  for(i=0 ; i<4 ; i++) S[i] = sum[i] ;
  for(i=0 ; i<4 ; i++) E[i] = err[i] ;
  i0 = n & 0x3 ;

  for(j = 0 ; j < i0 ; j++) {                      // the first (1/2/3) sums (if applicable)
    Z[j] = S[j] ;
    Y[j] = input(j) + E[j] ;
    S[j] = Z[j] + Y[j] ;
    E[j] = (Z[j] - S[j]) + Y[j] ;
  }
  for(i = i0 ; i < n-3 ; i += 4 ) {                // 4 parallel sums
    for(j=0 ; j<4 ; j++) {
      Z[j] = S[j] ;                  // save current value of sum
      Y[j] = input(i+j) + E[j] ;     // add accumulated error and input
      S[j] = Z[j] + Y[j] ;           // add new term to sum
      E[j] = (Z[j] - S[j]) + Y[j] ;  // new error = (old sum - new sum) + (new term + error)
    }
  }

  if(fold) {
    for(j=1 ; j<4 ; j++) {                   // fold everything into S[0], E[0]
      E[0] = E[0] + E[j] ;         // fold error terms
      E[j] = 0.0 ;
      Z[0] = S[0] ;                // save current value of sum
      Y[0] = S[j] + E[0] ;         // add accumulated error and input
      S[j] = 0.0 ;
      S[0] = Z[0] + Y[0] ;         // add new term to sum
      E[0] = (Z[0] - S[0]) + Y[0] ;// new error = (old sum - new sum) + (new term + error)
    }
    sum[0] = S[0] ;
    err[0] = E[0] ;
  }else{
    for(i=0 ; i<4 ; i++) sum[i] = S[i] ;
    for(i=0 ; i<4 ; i++) err[i] = E[i] ;
  }
#undef input
}
