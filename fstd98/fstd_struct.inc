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
  integer, parameter :: MAX_TUPLE=2  ! vector pairs for the time being
  integer, parameter :: VALID_ALLOC    = 00001      ! data pointer(s), allocsize(s)
  integer, parameter :: VALID_HANDLE   = 00002      ! handle(s)
  integer, parameter :: VALID_DIMS     = 00004      ! ni,  nj, nk
  integer, parameter :: VALID_DATES    = 00008      ! dateo, datev, deet, npas
  integer, parameter :: VALID_IP       = 00016      ! ip1, ip2, ip3
  integer, parameter :: VALID_GRID     = 00032      ! grtyp, ig1, ig2, ig3, ig4
  integer, parameter :: VALID_TAGS     = 00064      ! etiket, nomvar, typvar, 
  integer, parameter :: VALID_DTYP     = 00064      ! datyp, nbits
  integer, parameter :: VALID_DATA     = VALID_DTYP+VALID_DIMS+VALID_ALLOC
  integer, parameter :: VALID_META     = VALID_IP+VALID_GRID+VALID_TAGS+VALID_DATES
  integer, parameter :: VALID_WRITE    = VALID_DATA+VALID_META       ! everything necessary to write is there
  integer, parameter :: VALID_READ     = VALID_WRITE+VALID_HANDLE    ! after a read
  integer, parameter :: VALID_XTRA     = 00128      ! dltf, swa, lng, ubc, extra1, extra2, extra3

  type fstd_tuple     ! flags always start at 0 everything is flagged as invalid
    type(C_PTR), dimension(MAX_TUPLE) :: data
    integer, dimension(MAX_TUPLE) :: allocsize, handle
    integer :: flags = 0
    integer :: ni,  nj, nk, nbits, datyp, dateo, datev, deet, npas
    integer :: dltf, swa, lng, ubc, extra1, extra2, extra3
    integer :: ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=12) :: etiket
    character(len=4), dimension(MAX_TUPLE) :: nomvar
    character(len=2)  :: typvar
    character(len=1)  :: grtyp
  end type fstd_tuple
  type(fstd_tuple), parameter :: FSTD_NULL_TUPLE = fstd_tuple (  &
                    [C_NULL_PTR, C_NULL_PTR], [0,0], [-1,-1], &
                    0, &
                    0,0,0,0,0,0,0,0,0, &
                    0,0,0,0,0,0,0,&
                    0,0,0,0,0,0,0, &
                    " " ,[" "," "]," "," "                     )
  interface
    function c_malloc(nbytes) BIND(C, NAME='malloc')
      use, intrinsic :: ISO_C_BINDING, only: C_PTR, C_SIZE_T
      integer(C_SIZE_T), intent(IN), value :: nbytes
      type(C_PTR) :: c_malloc
    end function c_malloc
    subroutine c_free(p) BIND(C, NAME='free')
      use, intrinsic :: ISO_C_BINDING, only: C_PTR
      type(C_PTR), intent(IN), VALUE :: p
    end subroutine c_free
    function c_fstluk(field, handle, ni, nj, nk) result(status) bind(C,name='c_fstluk')
      import :: C_PTR, C_INT
      type(C_PTR), intent(IN), value :: field
      integer(C_INT), intent(IN) :: handle
      integer(C_INT), intent(OUT) :: ni, nj, nk
      integer(C_INT) :: status
    end function c_fstluk
    function c_fstecr(field, work, npak, iun, date, deet, npas, ni, nj, nk, ip1, ip2, ip3, &
                      typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit) &
                      result(status) bind(C,name='c_fstecr')
      import :: C_PTR, C_INT, C_CHAR
      type(C_PTR), intent(IN), value :: field, work
      integer(C_INT), intent(IN), value :: npak, iun, date, deet, npas, ni, nj, nk, ip1, ip2, ip3
      character(C_CHAR), dimension(*), intent(IN) :: typvar, nomvar, etiket, grtyp
      integer(C_INT), intent(IN), value :: ig1, ig2, ig3, ig4, datyp, rewrit
      integer :: status
    end function c_fstecr
  end interface
  integer, external :: fstinf
