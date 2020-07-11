!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2020  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! */
function gregorian_to_jd(Y, M, D) result(JDN)
  implicit none
  integer, intent(IN) :: Y, M, D
  integer :: JDN
  JDN = (1461 * (Y + 4800 + (M - 14)/12))/4 + &
        (367 * (M - 2 - 12 * ((M - 14)/12)))/12 - &
        (3 * ((Y + 4900 + (M - 14)/12)/100))/4 + D - 32075
  JDN = 367 * Y - 7 * (Y + (M + 9)/12)/4 - &
        3 * ((Y + (M - 9)/7)/100 + 1)/4  +275 * M/9 + D + 1721029
  JDN = 367 * Y - 7 * (Y + (M + 9)/12)/4  + 275 * M/9 + D + 1721014
end function gregorian_to_jd
