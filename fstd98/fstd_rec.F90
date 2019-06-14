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

function fstprm_rec(fstrec) result(status)   ! get record metadata
  use ISO_C_BINDING
  implicit none
  include 'fstd_struct.inc'
  type(fstd_tuple), intent(INOUT) :: fstrec
  integer :: status
  !
  integer, external :: fstprm
  real*8 :: nhours
  !
  status = fstprm(fstrec%handle(1), &
	fstrec%dateo,fstrec%deet,fstrec%npas, &
	fstrec%ni,fstrec%nj,fstrec%nk,fstrec%nbits,fstrec%datyp, &
	fstrec%ip1,fstrec%ip2,fstrec%ip3, &
	fstrec%typvar,fstrec%nomvar,fstrec%etiket, &
	fstrec%ig1,fstrec%ig2,fstrec%ig3,fstrec%ig4, &
	fstrec%grtyp,fstrec%swa,fstrec%lng,fstrec%dltf,fstrec%ubc, &
	fstrec%extra1,fstrec%extra2,fstrec%extra3)
  nhours=fstrec%deet
  nhours = nhours * fstrec%npas / 3600.0_8
  call incdatr(fstrec%datev, fstrec%dateo, nhours)    ! set datev using dateo, deet, npas
end function fstprm_rec

! get handles for all members of a tuple
function fstinf_rec(iun, ntup, rec, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(status)
  use ISO_C_BINDING
  implicit none
  include 'fstd_struct.inc'
  integer, intent(in) :: iun, ntup
  integer, optional, intent(in) ::  datev, ip1, ip2, ip3
  character(len=*), optional, intent(in) ::  etiket
  character(len=*), optional, intent(in) ::  typvar
  character(len=*), optional, dimension(ntup), intent(in) ::  nomvar      
  type(fstd_tuple), intent(inout)  ::  rec
  integer :: status
  !
  ! Local variabes
  !
  type(fstd_tuple) :: ff
  integer :: i, ni, nj, nk
  !
  status = -1
  ff%datev = -1
  if( present(datev))ff%datev = datev
  ff%ip1 = -1
  if( present(ip1))ff%ip1 = ip1
  ff%ip2 = -1
  if( present(ip2))ff%ip2 = ip2
  ff%ip3 = -1
  if( present(ip3))ff%ip3 = ip3
  ff%etiket=' '
  if( present(etiket))ff%etiket = etiket
  ff%typvar = ' '
  if( present(typvar))ff%typvar = typvar
  ff%nomvar = ' '
  if( present(nomvar))ff%nomvar(1) = nomvar(1)
  !
  rec%handle(1) = fstinf(iun,rec%ni,rec%nj,rec%nk,ff%datev,ff%etiket,ff%ip1,ff%ip2,ff%ip3,ff%typvar,ff%nomvar(1))
  do i = 2, ntup
    rec%handle(i) = fstinf(iun,ni,nj,nk,ff%datev,ff%etiket,ff%ip1,ff%ip2,ff%ip3,ff%typvar,ff%nomvar(i))
    if(rec%ni .ne. ni .or. rec%nj .ne. nj .or. rec%nk .ne. nk .or. rec%handle(i) < 0) then
      status = -1
      return
    endif
  enddo
  status = rec%handle(1)
end function fstinf_rec

! write all members of a tuple
function fstecr_rec(iun,rec,rewrite) result(status)
  use ISO_C_BINDING
  implicit none
  include 'fstd_struct.inc'
  type(fstd_tuple), intent(inout)  ::  rec
  integer(C_INT), intent(IN) :: iun, rewrite
  integer :: status

  integer :: i

  status = -1
  do i = 1, MAX_TUPLE
    if(C_ASSOCIATED(rec%data(i)) .and. (iand(VALID_WRITE,rec%flags) == VALID_WRITE) ) then
      status=c_fstecr(rec%data(i), C_NULL_PTR, -rec%nbits, iun, &
		      rec%dateo, rec%deet, rec%npas, &
		      rec%ni, rec%nj, rec%nk, &
		      rec%ip1, rec%ip2, rec%ip3, &
		      rec%typvar//achar(0), rec%nomvar(i)//achar(0), rec%etiket//achar(0), &
		      rec%grtyp, rec%ig1, rec%ig2, rec%ig3, rec%ig4, rec%datyp, rewrite)
      if(status < 0) return
    endif
  enddo
end function fstecr_rec

! read all members of a tuple
function fstluk_rec(rec) result(status)
  use ISO_C_BINDING
  implicit none
  include 'fstd_struct.inc'
  type(fstd_tuple), intent(inout)  ::  rec
  integer :: status

  integer(C_SIZE_T) :: nbytes
  integer :: i, ni, nj, nk

  status = -1
  do i = 1, MAX_TUPLE
    if(rec%handle(i) >= 0) then                          ! valid handle
      if(C_ASSOCIATED(rec%data(i))) then                 ! data container already allocated
	if(rec%allocsize(i) < rec%ni*rec%nj*rec%nk)then  ! too small, reallocate
	  call c_free(rec%data(i))
	  nbytes = rec%ni*rec%nj*rec%nk*4
	  rec%allocsize(i) = rec%ni*rec%nj*rec%nk
	  rec%data(i) = c_malloc(nbytes)
	endif
      else                                               ! allocate
	nbytes = rec%ni*rec%nj*rec%nk*4
	rec%data(i) = c_malloc(nbytes)
      endif
      status = c_fstluk(rec%data(1), rec%handle(1), ni, nj, nk)
      if(rec%ni .ne. ni .or. rec%nj .ne. nj .or. rec%nk .ne. nk .or. status < 0) then
	status = -1
	return
      endif
    endif
  enddo
end function fstluk_rec

program test
  use ISO_C_BINDING
  implicit none
  include 'fstd_struct_fns.inc'

  type(fstd_tuple) :: my_rec = FSTD_NULL_TUPLE
  integer(C_SIZE_T) :: nbytes

  nbytes = 1024
  my_rec%data(1) = c_malloc(nbytes)
  if(C_ASSOCIATED(my_rec%data(1))) then
    print *,'my_rec%data(1) is associated'
  else
    print *,'my_rec%data(1) is not associated'
  endif
  call c_free(my_rec%data(1))
  my_rec%data(1) = C_NULL_PTR
  call c_free(my_rec%data(1))
  stop
end
