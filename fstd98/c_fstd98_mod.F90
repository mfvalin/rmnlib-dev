! Copyright (C) 2021  Environnement et Changement climatique Canada
!
! This is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
!
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! Author:
!     M. Valin,   Recherche en Prevision Numerique, 2021
!
! Fortran interfaces to the c_xxx family of RPN standard files 98 C functions via a module
!
! use c_fstd98_mod
!
module c_fstd98_mod
  use ISO_C_BINDING
  implicit none
#include <c_fstd98_interface.hf>

end module
