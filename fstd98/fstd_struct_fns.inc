! * RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 2019  Division de Recherche en Prevision Numerique
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
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! *
! from original work by : Andre Plante, CMC, 2010
! adaptation to tuples  : M. Valin, RPN, 2019
include 'fstd_struct.inc'
interface
  function fstprm_rec(fstrec) result(status)   ! get record metadata
    import :: fstd_tuple
    type(fstd_tuple), intent(INOUT) :: fstrec
    integer :: status
  end function fstprm_rec
  function fstinf_rec(iun, ntup, rec, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(status)
    import :: fstd_tuple
    integer, intent(in) :: iun, ntup
    integer, optional, intent(in) ::  datev, ip1, ip2, ip3
    character(len=*), optional, intent(in) ::  etiket
    character(len=*), optional, intent(in) ::  typvar
    character(len=*), optional, dimension(ntup), intent(in) ::  nomvar      
    type(fstd_tuple), intent(inout)  ::  rec
    integer :: status
  end function fstinf_rec
  function fstluk_rec(rec) result(status)
    import :: fstd_tuple
    type(fstd_tuple), intent(inout)  ::  rec
    integer :: status
  end function fstluk_rec
end interface
