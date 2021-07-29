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
module copy_f2c_mod
  use ISO_C_BINDING

  private :: c_strnlen
  interface
    function c_strnlen(str, maxlen) result(strlen) bind(C, name='strnlen')
      import :: C_CHAR, C_SIZE_T
      implicit none
      character(C_CHAR), dimension(*), intent(IN) :: str
      integer(C_SIZE_T), intent(IN) :: maxlen
      integer(C_SIZE_T) :: strlen
    end function c_strnlen
    function memset(s, byte, n) result(p) bind(C, name='memset')
      import :: C_PTR, C_SIZE_T, C_INT
      implicit none
      type(C_PTR), intent(IN), value :: s
      integer(C_INT), intent(IN), value :: byte
      integer(C_SIZE_T), intent(IN), value :: n
      type(C_PTR) :: p
    end function memset
  end interface
contains
  subroutine strncpy_f2c(f_str, c_str, n)    ! copy a Fortran string into a C string of at most n characters
    implicit none
    character(len=*), intent(IN) :: f_str
    integer, intent(IN), value :: n
    character(len=n), intent(OUT) :: c_str
    integer :: clen
    clen = min(len(f_str), n-1)              ! C string can accept at most n-1 bytes
    c_str(1:clen) = f_str(1:clen)            ! copy string
    c_str(clen:clen) = achar(0)              ! add terminating null
  end subroutine strncpy_f2c

  subroutine strncpy_c2f(f_str, c_str, n)    ! copy a C string of up to n characters into a Fortran string 
    implicit none
    character(len=*), intent(OUT) :: f_str
    integer, intent(IN), value :: n
    character(len=n), intent(IN) :: c_str
    integer(C_SIZE_T) :: flen, clen
    flen = len(f_str)
    clen = n
    clen = c_strnlen(c_str, clen)            ! C string cannot be longer thatn n bytes
    clen = min(flen,clen)                    ! at most flen characters can fir in Fortran string
    f_str(1:clen) = c_str(1:clen)            ! copy string
    if(flen > clen) f_str(clen+1:flen) = ' ' ! pad with blanks
  end subroutine strncpy_c2f

end module

module fstd98_mod
  use ISO_C_BINDING
  use copy_f2c_mod
  use c_fstd98_mod
  implicit none

  type :: fstd98
    integer :: iun
  contains
    procedure, PASS(this) :: ckp
    procedure, PASS(this) :: ouv
    procedure, PASS(this) :: nbr
    procedure, PASS(this) :: nbrv
    procedure, PASS(this) :: inf
    procedure, PASS(this) :: sui
    procedure, PASS(this) :: inl
    procedure, PASS(this) :: infx
    procedure, PASS(this) :: lir
    procedure, PASS(this) :: lir_d
    procedure, PASS(this) :: lir_h
    procedure, PASS(this) :: lir_b
    procedure, PASS(this) :: lir_s
    procedure, PASS(this) :: lirx
    procedure, PASS(this) :: lis
    procedure, PASS(this) :: ecr
    procedure, PASS(this) :: ecr_d
    procedure, PASS(this) :: ecr_h
    procedure, PASS(this) :: ecr_b
    procedure, PASS(this) :: ecr_s
    procedure, PASS(this) :: frm
    procedure, PASS(this) :: skp
    procedure, PASS(this) :: rwd
    procedure, PASS(this) :: voi
    procedure, PASS(this) :: weo
    procedure, NOPASS     :: version => fst_version
    procedure, NOPASS     :: opi => fstopi
    procedure, NOPASS     :: opl => fstopl
    procedure, NOPASS     :: opr => fstopr
    procedure, NOPASS     :: opc => fstopc
    procedure, NOPASS     :: eff => fsteff
    procedure, NOPASS     :: prm => fstprm
    procedure, NOPASS     :: luk => fstluk
    procedure, NOPASS     :: lnk => fstlnk
    procedure, NOPASS     :: unl => fstunl
    procedure, NOPASS     :: data_length => fst_data_length
    procedure, NOPASS     :: check => fstcheck
    procedure, NOPASS     :: reset_ip_flags => fstreset_ip_flags
    procedure, NOPASS     :: ip1_all
    procedure, NOPASS     :: ip2_all
    procedure, NOPASS     :: ip3_all
    procedure, NOPASS     :: ip1_val
    procedure, NOPASS     :: ip2_val
    procedure, NOPASS     :: ip3_val
  end type

  integer :: link_n = 0
  integer, dimension(:), pointer :: links_list => NULL()


contains

! /*****************************************************************************
!  *                              F S T O U V                                  *
!  *                                                                           *
!  *Object                                                                     *
!  *   Opens a RPN standard file.                                              *
!  *                                                                           *
!  *Arguments                                                                  *
!  *                                                                           *
!  *  IN  iun     unit number associated to the file                           *
!  *  IN  options random or sequential access                                  *
!  *                                                                           *
!  *****************************************************************************/
  function fstouv(iun, options) result (status)
    implicit none
    integer(C_INT), intent(IN), value :: iun
    character(len=*), intent(IN) :: options
    integer(C_INT) :: status
    status = c_fstouv(iun, trim(options)//achar(0))
  end function fstouv

  function ouv(this, iun, options) result (status) ! calls fstouv
    implicit none
    class(fstd98), intent(INOUT) :: this
    integer(C_INT), intent(IN), value :: iun
    character(len=*), intent(IN) :: options
    integer(C_INT) :: status
    this%iun = iun
    status = fstouv(this%iun, options)
  end function ouv

! /***************************************************************************** 
!  *                              F S T F R M                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Closes a RPN standard file.                                             *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *                                                                           * 
!  *****************************************************************************/
  function fstfrm(iun) result (status)
    implicit none
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT) :: status
    status = c_fstfrm(iun)
  end function fstfrm

  function frm(this) result (status)
    implicit none
    class(fstd98), intent(INOUT) :: this
    integer(C_INT) :: status
    status = fstfrm(this%iun)
    this%iun = -1
  end function frm

! /***************************************************************************** 
!  *                              F S T N B R                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Returns the number of records of the file associated with unit number.  *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *                                                                           * 
!  *****************************************************************************/
  function fstnbr(iun) result (nrec)
    implicit none
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT) :: nrec
    nrec = c_fstnbr(iun)
  end function fstnbr

  function nbr(this) result (nrec)
    implicit none
    class(fstd98), intent(IN) :: this
    integer(C_INT) :: nrec
    nrec = c_fstnbr(this%iun)
  end function nbr

! /***************************************************************************** 
!  *                              F S T N B R V                                *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Returns the number of valid records (excluding deleted records) of the  *
!  *   file associated with unit number.                                       *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *                                                                           * 
!  *****************************************************************************/
  function fstnbrv(iun) result (status)
    implicit none
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT) :: status
    status = c_fstnbrv(iun)
  end function fstnbrv

  function nbrv(this) result (nrecv)
    implicit none
    class(fstd98), intent(IN) :: this
    integer(C_INT) :: nrecv
    nrecv = c_fstnbrv(this%iun)
  end function nbrv

! /***************************************************************************** 
!  *                                F S T C K P                                *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Checkpoint. Clear buffers, rewrite headers.                             *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *                                                                           * 
!  *****************************************************************************/
  function fstckp(iun) result (status)
    implicit none
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT) :: status
    status = c_fstckp(iun)
  end function fstckp
  function ckp(this) result (status)
    implicit none
    class(fstd98), intent(IN) :: this
    integer(C_INT) :: status
    status = fstckp(this%iun)
  end function ckp

! /***************************************************************************** 
!  *                              F S T I N F                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Locate the next record that matches the research keys.                  *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  OUT ni      dimension 1 of the data field                                * 
!  *  OUT nj      dimension 2 of the data field                                * 
!  *  OUT nk      dimension 3 of the data field                                * 
!  *  IN  datev   valid date                                                   * 
!  *  IN  etiket  label                                                        * 
!  *  IN  ip1     vertical level                                               * 
!  *  IN  ip2     forecast hour                                                * 
!  *  IN  ip3     user defined identifier                                      * 
!  *  IN  typvar  type of field                                                * 
!  *  IN  nomvar  variable name                                                * 
!  *                                                                           * 
!  *****************************************************************************/
  function fstinf(iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
    character(len=5)  :: nom
    character(len=3)  :: typ
    character(len=13) :: eti
    call strncpy_f2c(typvar, typ, 3)
    call strncpy_f2c(nomvar, nom, 5)
    call strncpy_f2c(etiket, eti, 13)
    handle = c_fstinf(iun, ni, nj, nk, datev, eti, ip1, ip2, ip3, typ, nom)
  end function fstinf

  function inf(this, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
    class(fstd98), intent(IN) :: this
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
    handle = fstinf(this%iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
  end function inf

!  /***************************************************************************** 
!  *                            F S T S U I                                    *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Finds the next record that matches the last search criterias            *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  OUT ni      dimension 1 of the data field                                * 
!  *  OUT nj      dimension 2 of the data field                                * 
!  *  OUT nk      dimension 3 of the data field                                * 
!  *                                                                           * 
!  *****************************************************************************/
  function fstsui(iun, ni, nj, nk) result(handle)
    implicit none
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT) :: handle
    handle = c_fstsui(iun, ni, nj, nk)
  end function fstsui

  function sui(this, ni, nj, nk) result(handle)
    implicit none
    class(fstd98), intent(IN) :: this
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT) :: handle
    handle = c_fstsui(this%iun, ni, nj, nk)
  end function sui

! /***************************************************************************** 
!  *                              F S T I N F X                                *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Locate the next record that matches the research keys.                  *
!  *   The search begins at the position given by the start handle.            * 
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  start   handle from which the search begins                          *
!  *  IN  iun     unit number associated to the file                           * 
!  *  OUT ni      dimension 1 of the data field                                * 
!  *  OUT nj      dimension 2 of the data field                                * 
!  *  OUT nk      dimension 3 of the data field                                * 
!  *  IN  datev   valid date                                                   * 
!  *  IN  etiket  label                                                        * 
!  *  IN  ip1     vertical level                                               * 
!  *  IN  ip2     forecast hour                                                * 
!  *  IN  ip3     user defined identifier                                      * 
!  *  IN  typvar  type of field                                                * 
!  *  IN  nomvar  variable name                                                * 
!  *                                                                           * 
!  *****************************************************************************/
  function fstinfx(start, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
    integer(C_INT), intent(IN), value :: iun, start
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
    character(len=5)  :: nom
    character(len=3)  :: typ
    character(len=13) :: eti
    call strncpy_f2c(typvar, typ, 3)
    call strncpy_f2c(nomvar, nom, 5)
    call strncpy_f2c(etiket, eti, 13)
    handle = c_fstinfx(start, iun, ni, nj, nk, datev, eti, ip1, ip2, ip3, typ, nom)
  end function fstinfx

  function infx(this, start, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
    class(fstd98), intent(IN) :: this
    integer(C_INT), intent(IN), value :: start
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
    handle = fstinfx(start, this%iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
  end function infx

! /***************************************************************************** 
!  *                              F S T I N L                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Locates all the records that match the research keys.                   *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  OUT ni      dimension 1 of the data field                                * 
!  *  OUT nj      dimension 2 of the data field                                * 
!  *  OUT nk      dimension 3 of the data field                                * 
!  *  IN  datev   valid date                                                   * 
!  *  IN  etiket  label                                                        * 
!  *  IN  ip1     vertical level                                               * 
!  *  IN  ip2     forecast hour                                                * 
!  *  IN  ip3     user defined identifier                                      * 
!  *  IN  typvar  type of field                                                * 
!  *  IN  nomvar  variable name                                                *
!  *  OUT liste   list of handles to the records                               *
!  *  OUT infon   number of elements for the list (number of records found)    *
!  *  OUT nmax    dimension of list as given by caller                         *
!  *                                                                           * 
!  *****************************************************************************/
  function fstinl(iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar, liste, infon, nmax) result(status)
    implicit none
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3, nmax
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT), intent(OUT) :: infon
    integer(C_INT), dimension(nmax), intent(OUT) :: liste
    integer(C_INT) :: status
    character(len=5)  :: nom
    character(len=3)  :: typ
    character(len=13) :: eti
    call strncpy_f2c(typvar, typ, 3)
    call strncpy_f2c(nomvar, nom, 5)
    call strncpy_f2c(etiket, eti, 13)
    status = c_fstinl(iun, ni, nj, nk, datev, eti, ip1, ip2, ip3, typ, nom, liste, infon, nmax)
  end function fstinl

  function inl(this, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar, liste, infon, nmax) result(status)
    implicit none
    class(fstd98), intent(IN) :: this
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3, nmax
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: status
    integer(C_INT), intent(OUT) :: infon
    integer(C_INT), dimension(nmax), intent(OUT) :: liste
    status = fstinl(this%iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar, liste, infon, nmax)
  end function inl

! /*****************************************************************************
!  *                              F S T P R M                                  *
!  *                                                                           *
!  *Object                                                                     *
!  *   Get all the description informations of the record.                     *
!  *                                                                           *
!  *Arguments                                                                  *
!  *                                                                           *
!  *  IN  handle  positioning information to the record                        *
!  *  OUT date    date time stamp                                              *
!  *  OUT deet    length of a time step in seconds                             *
!  *  OUT npas    time step number                                             *
!  *  OUT ni      first dimension of the data field                            *
!  *  OUT nj      second dimension of the data field                           *
!  *  OUT nk      third dimension of the data field                            * 
!  *  OUT nbits   number of bits kept for the elements of the field            * 
!  *  OUT datyp   data type of the elements                                    * 
!  *  OUT ip1     vertical level                                               * 
!  *  OUT ip2     forecast hour                                                * 
!  *  OUT ip3     user defined identifier                                      * 
!  *  OUT typvar  type of field (forecast, analysis, climatology)              * 
!  *  OUT nomvar  variable name                                                * 
!  *  OUT etiket  label                                                        * 
!  *  OUT grtyp   type of geographical projection                              * 
!  *  OUT ig1     first grid descriptor                                        * 
!  *  OUT ig2     second grid descriptor                                       * 
!  *  OUT ig3     third grid descriptor                                        * 
!  *  OUT ig4     fourth grid descriptor                                       * 
!  *  OUT swa     starting word address                                        * 
!  *  OUT lng     record length                                                * 
!  *  OUT dltf    delete flag                                                  * 
!  *  OUT ubc     unused bit count                                             * 
!  *  OUT extra1  extra parameter                                              * 
!  *  OUT extra2  extra parameter                                              * 
!  *  OUT extra3  extra parameter                                              * 
!  *                                                                           * 
!  *****************************************************************************/
  subroutine fstprm(handle, date, deet, npas, ni, nj, nk, nbits, datyp, ip1, ip2, ip3, &
                    typvar, nomvar, etiket, grtyp, &
                    ig1, ig2, ig3, ig4, swa, lng, dlft, ubc, extra1, extra2, extra3)
    implicit none
    integer, intent(IN), value :: handle
    integer, intent(OUT) :: date, deet, npas, ni, nj, nk, nbits, datyp, ip1, ip2, ip3
    integer, intent(OUT) :: ig1, ig2, ig3, ig4, swa, lng, dlft, ubc, extra1, extra2, extra3
    character(len=*), intent(OUT) :: typvar, nomvar, etiket, grtyp
    character(len=5)  :: nom
    character(len=3)  :: typ
    character(len=2)  :: gty
    character(len=13) :: eti
    nom = ' ' ; nom(5:5) = achar(0)
    typ = ' ' ; typ(3:3) = achar(0)
    gty = ' ' ; gty(2:2) = achar(0)
    eti = ' ' ; eti(13:13) = achar(0)
    call c_fstprm(handle, date, deet, npas, ni, nj, nk, nbits, datyp, ip1, ip2, ip3, &
                  typ, nom, eti, gty, &
                  ig1, ig2, ig3, ig4, swa, lng, dlft, ubc, extra1, extra2, extra3)
    typvar = typ(1:2)
    nomvar = nom(1:4)
    grtyp = gty(1:1)
    etiket = eti(1:12)
  end subroutine fstprm

! /***************************************************************************** 
!  *                              F S T L I R X                                *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Reads the next record that matches the research keys.                   *
!  *   The search begins at the position given by the start handle.            * 
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  OUT field   data field to be read                                        * 
!  *  IN  start   handle from which the search begins                          *
!  *  IN  iun     unit number associated to the file                           * 
!  *  OUT ni      dimension 1 of the data field                                * 
!  *  OUT nj      dimension 2 of the data field                                * 
!  *  OUT nk      dimension 3 of the data field                                * 
!  *  IN  datev   valid date                                                   * 
!  *  IN  etiket  label                                                        * 
!  *  IN  ip1     vertical level                                               * 
!  *  IN  ip2     forecast hour                                                * 
!  *  IN  ip3     user defined identifier                                      * 
!  *  IN  typvar  type of field                                                * 
!  *  IN  nomvar  variable name                                                * 
!  *                                                                           * 
!  *****************************************************************************/
  function fstlirx(field, start, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN), value :: iun, start
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
    character(len=5)  :: nom
    character(len=3)  :: typ
    character(len=13) :: eti
    call strncpy_f2c(typvar, typ, 3)
    call strncpy_f2c(nomvar, nom, 5)
    call strncpy_f2c(etiket, eti, 13)
    handle = c_fstlirx(field, start, iun, ni, nj, nk, datev, eti, ip1, ip2, ip3, typ, nom)
  end function fstlirx

  function lirx(field, start, this, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    class(fstd98), intent(IN) :: this
    integer(C_INT), intent(IN), value :: start
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
    character(len=5)  :: nom
    character(len=3)  :: typ
    character(len=13) :: eti
    call strncpy_f2c(typvar, typ, 3)
    call strncpy_f2c(nomvar, nom, 5)
    call strncpy_f2c(etiket, eti, 13)
    handle = c_fstlirx(field, start, this%iun, ni, nj, nk, datev, eti, ip1, ip2, ip3, typ, nom)
  end function lirx

! /***************************************************************************** 
!  *                              F S T L I R                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Reads the next record that matches the research keys.                   *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  OUT field   data field to be read                                        * 
!  *  OUT ni      dimension 1 of the data field                                * 
!  *  OUT nj      dimension 2 of the data field                                * 
!  *  OUT nk      dimension 3 of the data field                                * 
!  *  IN  datev   valid date                                                   * 
!  *  IN  etiket  label                                                        * 
!  *  IN  ip1     vertical level                                               * 
!  *  IN  ip2     forecast hour                                                * 
!  *  IN  ip3     user defined identifier                                      * 
!  *  IN  typvar  type of field                                                * 
!  *  IN  nomvar  variable name                                                * 
!  *                                                                           * 
!  *****************************************************************************/
  function fstlir(field, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
    character(len=5)  :: nom
    character(len=3)  :: typ
    character(len=13) :: eti
    call strncpy_f2c(typvar, typ, 3)
    call strncpy_f2c(nomvar, nom, 5)
    call strncpy_f2c(etiket, eti, 13)
    handle = c_fstlir(field, iun, ni, nj, nk, datev, eti, ip1, ip2, ip3, typ, nom)
  end function fstlir

  function lir(field, this, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    class(fstd98), intent(IN) :: this
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
    handle = fstlir(field, this%iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
  end function lir

  function fstlir_d(dblewords, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
#define IgnoreTypeKindRank dblewords
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
    integer :: status
    status = fst_data_length(8)
    handle = fstlir(dblewords, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
    status = fst_data_length(4)
  end function fstlir_d

  function lir_d(dblewords, this, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
#define IgnoreTypeKindRank dblewords
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    class(fstd98), intent(IN) :: this
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
    handle = fstlir_d(dblewords, this%iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
  end function lir_d

  function fstlir_h(halfwords, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
#define IgnoreTypeKindRank halfwords
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
    integer :: status
    status = fst_data_length(2)
    handle = fstlir(halfwords, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
    status = fst_data_length(4)
  end function fstlir_h

  function lir_h(halfwords, this, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
#define IgnoreTypeKindRank halfwords
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    class(fstd98), intent(IN) :: this
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
    handle = fstlir_h(halfwords, this%iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
  end function lir_h

  function fstlir_b(bytes, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
#define IgnoreTypeKindRank bytes
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
    integer :: status
    status = fst_data_length(1)
    handle = fstlir(bytes, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
    status = fst_data_length(4)
  end function fstlir_b

  function lir_b(bytes, this, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
    implicit none
#define IgnoreTypeKindRank bytes
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    class(fstd98), intent(IN) :: this
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
    handle = fstlir_b(bytes, this%iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
  end function lir_b

! /***************************************************************************** 
!  *                              F S T L I R _ S                              *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Reads the next record that matches the research keys.                   *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  OUT string  character string to be read                                  * 
!  *  OUT ni      dimension 1 of the data field                                * 
!  *  OUT nj      dimension 2 of the data field                                * 
!  *  OUT nk      dimension 3 of the data field                                * 
!  *  IN  datev   valid date                                                   * 
!  *  IN  etiket  label                                                        * 
!  *  IN  ip1     vertical level                                               * 
!  *  IN  ip2     forecast hour                                                * 
!  *  IN  ip3     user defined identifier                                      * 
!  *  IN  typvar  type of field                                                * 
!  *  IN  nomvar  variable name                                                * 
!  *                                                                           * 
!  *****************************************************************************/
! ftnword f77name(fstlir_s)(void *string, ftnword *f_iun,
!                         ftnword *f_ni, ftnword *f_nj,
!                         ftnword *f_nk, ftnword *f_datev, char *f_etiket,
!                         ftnword *f_ip1, ftnword *f_ip2, ftnword *f_ip3,
!                         char *f_typvar, char *f_nomvar,
!                         int lng_string, F2Cl ll1, F2Cl ll2, F2Cl ll3)
  function fstlir_s(field, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar, lngstr) result(handle)
    implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes ,target
#include <IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3, lngstr
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
    integer(C_SIZE_T) :: nset
    type(C_PTR) :: p
    nset = lngstr
    p = memset(C_LOC(field), ichar(' '), nset)
    handle = fstlir(field, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
  end function fstlir_s

  function lir_s(field, this, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar, lngstr) result(handle)
    implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes ,target
#include <IgnoreTypeKindRank.hf>
    class(fstd98), intent(IN) :: this
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3, lngstr
    character(len=*), intent(IN) :: typvar, nomvar, etiket
    integer(C_INT) :: handle
    handle = fstlir_s(field, this%iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar, lngstr)
  end function lir_s

! /***************************************************************************** 
!  *                            F S T L I S                                    *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Reads the next record that matches the last search criterias            *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  OUT field   data field to be read                                        * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  OUT ni      dimension 1 of the data field                                * 
!  *  OUT nj      dimension 2 of the data field                                * 
!  *  OUT nk      dimension 3 of the data field                                * 
!  *                                                                           * 
!  *****************************************************************************/
! int c_fstlis(word *field, int iun, int *ni, int *nj, int *nk)
! ftnword f77name(fstlis)(word *field, ftnword *f_iun,
!                         ftnword *f_ni, ftnword *f_nj, ftnword *f_nk)
  function fstlis(field, iun, ni, nj, nk) result(handle)
    implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT) :: handle
    handle = c_fstlis(field, iun, ni, nj, nk)
  end function fstlis

  function lis(field, this, ni, nj, nk) result(handle)
    implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    class(fstd98), intent(IN) :: this
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT) :: handle
    handle = c_fstlis(field, this%iun, ni, nj, nk)
  end function lis

! /***************************************************************************** 
!  *                             F S T L I C                                   *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Search for a record that matches the research keys and check that the   *
!  *   remaining parmeters match the record descriptors                        *
!  *                                                                           *
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  OUT field    data field to be read                                       * 
!  *  IN  iun      unit number associated to the file                          * 
!  *  IN  niin     dimension 1 of the data field                               * 
!  *  IN  njin     dimension 2 of the data field                               * 
!  *  IN  nkin     dimension 3 of the data field                               * 
!  *  IN  datev    valid date                                                  * 
!  *  IN  etiket   label                                                       * 
!  *  IN  ip1      vertical level                                              * 
!  *  IN  ip2      forecast hour                                               * 
!  *  IN  ip3      user defined identifier                                     * 
!  *  IN  typvar   type of field                                               * 
!  *  IN  nomvar   variable name                                               * 
!  *  IN  ig1      first grid descriptor                                       * 
!  *  IN  ig2      second grid descriptor                                      * 
!  *  IN  ig3      third grid descriptor                                       * 
!  *  IN  ig4      fourth grid descriptor                                      * 
!  *  IN  grtyp    type of geographical projection                             * 
!  *                                                                           * 
!  *****************************************************************************/
  function fstlic(field, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar, &
                  ig1, ig2, ig3, ig4, grtyp) result(handle)
    implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
    integer(C_INT), intent(IN)  :: ig1, ig2, ig3, ig4
    integer(C_INT) :: handle
    character(len=5)  :: nom
    character(len=3)  :: typ
    character(len=13) :: eti
    character(len=2)  :: gty
    call strncpy_f2c(typvar, typ, 3)
    call strncpy_f2c(nomvar, nom, 5)
    call strncpy_f2c(etiket, eti, 13)
    call strncpy_f2c(grtyp,  gty, 2)
    handle = c_fstlic(field, iun, ni, nj, nk, datev, eti, ip1, ip2, ip3, typ, nom, &
                      ig1, ig2, ig3, ig4, gty)
  end function fstlic

! /***************************************************************************** 
!  *                              F S T E C R                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Writes a record into a file.                                                  *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  field   field to write to the file                                   * 
!  *  IN  work    work field (kept for backward compatibility)                 * 
!  *  IN  npak    number of bits kept for the elements of the field (-npak)    * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  IN  date    date time stamp                                              * 
!  *  IN  deet    length of a time step in seconds                             * 
!  *  IN  npas    time step number                                             * 
!  *  IN  ni      first dimension of the data field                            * 
!  *  IN  nj      second dimension of the data field                           * 
!  *  IN  nk      third dimension of the data field                            * 
!  *  IN  ip1     vertical level                                               * 
!  *  IN  ip2     forecast hour                                                * 
!  *  IN  ip3     user defined identifier                                      * 
!  *  IN  typvar  type of field (forecast, analysis, climatology)              * 
!  *  IN  nomvar  variable name                                                * 
!  *  IN  etiket  label                                                        * 
!  *  IN  grtyp   type of geographical projection                              * 
!  *  IN  ig1     first grid descriptor                                        * 
!  *  IN  ig2     second grid descriptor                                       * 
!  *  IN  ig3     third grid descriptor                                        * 
!  *  IN  ig4     fourth grid descriptor                                       * 
!  *  IN  datyp   data type of the elements                                    * 
!  *  IN  rewrit  rewrite flag (true=rewrite existing record, false=append)    *
!  *                                                                           * 
!  *****************************************************************************/
  subroutine fstecr(field, work, npak, iun, date, deet, npas, ni, nj, nk, &
                    ip1, ip2, ip3, typvar, nomvar, etiket, &
                    grtyp, ig1, ig2, ig3, ig4, datyp, rewrite)
    implicit none
#define IgnoreTypeKindRank field, work
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp, rewrite
    integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
    integer(C_INT) :: status
    character(len=4)  :: nom
    character(len=2)  :: typ
    character(len=1)  :: gty
    character(len=12) :: eti
    nom = nomvar
    typ = typvar
    gty = grtyp
    eti = etiket
    status = c_fstecr(field, work, npak, iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typ//achar(0), nom//achar(0), eti//achar(0), gty//achar(0),  &
                      ig1, ig2, ig3, ig4, datyp, rewrite)
  end subroutine fstecr

  subroutine ecr(field, work, npak, this, date, deet, npas, ni, nj, nk, &
                  ip1, ip2, ip3, typvar, nomvar, etiket, &
                  grtyp, ig1, ig2, ig3, ig4, datyp, rewrite)
    implicit none
#define IgnoreTypeKindRank field, work
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    class(fstd98), intent(IN) :: this
    integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp, rewrite
    integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
    call fstecr(field, work, npak, this%iun, date, deet, npas, ni, nj, nk, &
                ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                ig1, ig2, ig3, ig4, datyp, rewrite)
  end subroutine ecr

  subroutine fstecr_d(dblewords, work, npak, iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, &
                      grtyp, ig1, ig2, ig3, ig4, datyp, rewrite)
    implicit none
#define IgnoreTypeKindRank dblewords, work
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp, rewrite
    integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
    integer :: status
    status = fst_data_length(8)
    call fstecr(dblewords, work, npak, iun, date, deet, npas, ni, nj, nk, &
                ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                ig1, ig2, ig3, ig4, datyp, rewrite)
    status = fst_data_length(4)
  end subroutine fstecr_d

  subroutine ecr_d(dblewords, work, npak, this, date, deet, npas, ni, nj, nk, &
                  ip1, ip2, ip3, typvar, nomvar, etiket, &
                  grtyp, ig1, ig2, ig3, ig4, datyp, rewrite)
    implicit none
#define IgnoreTypeKindRank dblewords, work
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    class(fstd98), intent(IN) :: this
    integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp, rewrite
    integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
    call fstecr_d(dblewords, work, npak, this%iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                      ig1, ig2, ig3, ig4, datyp, rewrite)
  end subroutine ecr_d

  subroutine fstecr_h(halfwords, work, npak, iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, &
                      grtyp, ig1, ig2, ig3, ig4, datyp, rewrite)
    implicit none
#define IgnoreTypeKindRank halfwords, work
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp, rewrite
    integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
    integer :: status
    status = fst_data_length(2)
    call fstecr(halfwords, work, npak, iun, date, deet, npas, ni, nj, nk, &
                ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                ig1, ig2, ig3, ig4, datyp, rewrite)
    status = fst_data_length(4)
  end subroutine fstecr_h

  subroutine ecr_h(halfwords, work, npak, this, date, deet, npas, ni, nj, nk, &
                  ip1, ip2, ip3, typvar, nomvar, etiket, &
                  grtyp, ig1, ig2, ig3, ig4, datyp, rewrite)
    implicit none
#define IgnoreTypeKindRank halfwords, work
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    class(fstd98), intent(IN) :: this
    integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp, rewrite
    integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
    call fstecr_h(halfwords, work, npak, this%iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                      ig1, ig2, ig3, ig4, datyp, rewrite)
  end subroutine ecr_h

  subroutine fstecr_b(bytes, work, npak, iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, &
                      grtyp, ig1, ig2, ig3, ig4, datyp, rewrite)
    implicit none
#define IgnoreTypeKindRank bytes, work
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp, rewrite
    integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
    integer :: status
    status = fst_data_length(1)
    call fstecr(bytes, work, npak, iun, date, deet, npas, ni, nj, nk, &
                ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                ig1, ig2, ig3, ig4, datyp, rewrite)
    status = fst_data_length(4)
  end subroutine fstecr_b

  subroutine ecr_b(bytes, work, npak, this, date, deet, npas, ni, nj, nk, &
                  ip1, ip2, ip3, typvar, nomvar, etiket, &
                  grtyp, ig1, ig2, ig3, ig4, datyp, rewrite)
    implicit none
#define IgnoreTypeKindRank bytes, work
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    class(fstd98), intent(IN) :: this
    integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp, rewrite
    integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
    call fstecr_b(bytes, work, npak, this%iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                      ig1, ig2, ig3, ig4, datyp, rewrite)
  end subroutine ecr_b

! /***************************************************************************** 
!  *                              F S T E C R _ S                              *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Writes record to file.                                                  *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  string  character string to write to the file                        * 
!  *  IN  work    work field (kept for backward compatibility)                 * 
!  *  IN  npak    number of bits kept for the elements of the field (-npak)    * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  IN  date    date time stamp                                              * 
!  *  IN  deet    length of a time step in seconds                             * 
!  *  IN  npas    time step number                                             * 
!  *  IN  ni      first dimension of the data field                            * 
!  *  IN  nj      second dimension of the data field                           * 
!  *  IN  nk      third dimension of the data field                            * 
!  *  IN  ip1     vertical level                                               * 
!  *  IN  ip2     forecast hour                                                * 
!  *  IN  ip3     user defined identifier                                      * 
!  *  IN  typvar  type of field (forecast, analysis, climatology)              * 
!  *  IN  nomvar  variable name                                                * 
!  *  IN  etiket  label                                                        * 
!  *  IN  grtyp   type of geographical projection                              * 
!  *  IN  ig1     first grid descriptor                                        * 
!  *  IN  ig2     second grid descriptor                                       * 
!  *  IN  ig3     third grid descriptor                                        * 
!  *  IN  ig4     fourth grid descriptor                                       * 
!  *  IN  datyp   data type of the elements                                    * 
!  *  IN  rewrit  rewrite flag (true=rewrite existing record, false=append)    *
!  *                                                                           * 
!  *****************************************************************************/
  function fstecr_s(field, work, npak, iun, date, deet, npas, ni, nj, nk, &
                    ip1, ip2, ip3, typvar, nomvar, etiket, &
                    grtyp, ig1, ig2, ig3, ig4, datyp, rewrite, lngstr) result(status)
    implicit none
#define IgnoreTypeKindRank field, work
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp, rewrite
    integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4, lngstr
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
    integer(C_INT) :: status
    integer ninjnk
    character(len=4)  :: nom
    character(len=2)  :: typ
    character(len=1)  :: gty
    character(len=12) :: eti
    status = 1
    ninjnk = max(1,ni) * max(1,nj) * max(1,nk)
!     if (ninjnk > lngstr * nj) return
    if (ninjnk > lngstr) return
    nom = nomvar
    typ = typvar
    gty = grtyp
    eti = etiket
    call fstecr(field, work, npak, iun, date, deet, npas, ni, nj, nk, &
                ip1, ip2, ip3, typ//achar(0), nom//achar(0), eti//achar(0), gty//achar(0),  &
                ig1, ig2, ig3, ig4, datyp, rewrite)
    status = 0
  end function fstecr_s

  function ecr_s(field, work, npak, this, date, deet, npas, ni, nj, nk, &
                 ip1, ip2, ip3, typvar, nomvar, etiket, &
                 grtyp, ig1, ig2, ig3, ig4, datyp, rewrite, lngstr) result(status)
    implicit none
#define IgnoreTypeKindRank field, work
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    class(fstd98), intent(IN) :: this
    integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp, rewrite
    integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4, lngstr
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
    integer(C_INT) :: status
    status = fstecr_s(field, work, npak, this%iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                      ig1, ig2, ig3, ig4, datyp, rewrite, lngstr)
    status = 0
  end function ecr_s

  function fstecr_str(string, work, npak, iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, &
                      grtyp, ig1, ig2, ig3, ig4, datyp, rewrite) result(status)
    implicit none
    character(len=*), intent(IN) :: string
#define IgnoreTypeKindRank work
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN), value :: iun
    integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp, rewrite
    integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
    integer(C_INT) :: status
    integer ninjnk
    ninjnk = max(1,ni) * max(1,nj) * max(1,nk)
    status = 1
    if (ninjnk > len(string)) return
    status = fstecr_s(string, work, npak, iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                      ig1, ig2, ig3, ig4, datyp, rewrite, len(string))
  end function fstecr_str

  function ecr_str(string, work, npak, this, date, deet, npas, ni, nj, nk, &
                   ip1, ip2, ip3, typvar, nomvar, etiket, &
                   grtyp, ig1, ig2, ig3, ig4, datyp, rewrite) result(status)
    implicit none
    character(len=*), intent(IN) :: string
#define IgnoreTypeKindRank work
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    class(fstd98), intent(IN) :: this
    integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp, rewrite
    integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
    integer(C_INT) :: status
    status = fstecr_str(string, work, npak, this%iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, grtyp,  &
                      ig1, ig2, ig3, ig4, datyp, rewrite)
  end function ecr_str

! /***************************************************************************** 
!  *                             F S T E F F                                   *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Deletes the record associated to handle.                                *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  handle  handle to the record to delete                               * 
!  *                                                                           * 
!  *****************************************************************************/
  function fsteff(handle) result (status)
    implicit none
    integer(C_INT), intent(IN), value :: handle
    integer(C_INT) :: status
    status = c_fsteff(handle)
  end function fsteff

! /***************************************************************************** 
!  *                              F S T L U K                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Read the record at position given by handle.                            *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  OUT field   data field to be read                                        * 
!  *  IN  handle  positioning information to the record                        * 
!  *  OUT ni      dimension 1 of the data field                                * 
!  *  OUT nj      dimension 2 of the data field                                * 
!  *  OUT nk      dimension 3 of the data field                                * 
!  *                                                                           * 
!  *****************************************************************************/
  function fstluk(field, handle, ni, nj, nk) result(handle_out)
    implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    integer(C_INT), intent(IN), value :: handle
    integer(C_INT), intent(OUT) :: ni, nj, nk
    integer(C_INT) :: handle_out
    handle_out = c_fstluk(field, handle, ni, nj, nk)
  end function fstluk

! /***************************************************************************** 
!  *                              F S T L N K                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Links a list of files together for search purpose.                      *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  liste   list of unit numbers associated to the files                 * 
!  *  IN  n       number of files to link                                      * 
!  *                                                                           * 
!  *****************************************************************************/
  function fstlnk(link_list,n) result(status) bind(C,name='c_fstlnk')
    implicit none
    integer(C_INT), intent(IN), value :: n
    integer(C_INT), dimension(n) :: link_list
    integer(C_INT) :: status
    if(associated(links_list)) then  ! ERROR
      status = 1
    else
      allocate(links_list(n))
      link_n = n
      links_list(1:n) = link_list(1:n)
      status = c_xdflnk(links_list, link_n)
    endif
  end function fstlnk

!  /***************************************************************************** 
!  *                              F S T U N L                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Unlinks a list of files previously linked by fstlnk.                    *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  liste   list of unit numbers associated to the files                 * 
!  *  IN  n       number of files to link                                      * 
!  *                                                                           * 
!  *****************************************************************************/
  function fstunl() result(status) bind(C,name='c_fstunl')
    implicit none
    integer(C_INT) :: status
    if(associated(links_list)) then
      status = c_xdfunl(links_list,link_n)
      deallocate(links_list)
      link_n = 0
    else
      status = 1
    endif
  end function fstunl

! /***************************************************************************** 
!  *                      F S T _ D A T A _ L E N G T H                        *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Gives information on data lenght of the elements passed to fstecr       *
!  *   and fstlir (double, short integer, byte ...)                            *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  length_type     data length kind                                     * 
!  *                      1: byte                                              *
!  *                      2: short (16 bits)                                   *
!  *                      4: regular 32 bits                                   *
!  *                      8: double (64 bits)                                  *
!  *                                                                           * 
!  *****************************************************************************/
  function fst_data_length(l) result(status)
    implicit none
    integer(C_INT), intent(IN), value :: l
    integer(C_INT) :: status
    status = c_fst_data_length(l)
  end function fst_data_length

! /***************************************************************************** 
!  *                            F S T M S Q                                    *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Mask a portion of the research keys.                                    *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *   IN    iun     unit number associated to the file                        * 
!  * IN/OUT  mip1    mask for vertical level                                   * 
!  * IN/OUT  mip2    mask for forecast hour                                    * 
!  * IN/OUT  mip3    mask for ip3 (user defined identifier)                    * 
!  * IN/OUT  metiket mask for label                                            * 
!  *   IN    getmode logical (1: getmode 0:set mode)                           * 
!  *                                                                           * 
!  *****************************************************************************/
! ftnword f77name(fstmsq)(ftnword *f_iun, ftnword *f_mip1, ftnword *f_mip2,
!                         ftnword *f_mip3, char *f_metiket, ftnword *f_getmode,
! int c_fstmsq(int iun, int *mip1, int *mip2, int *mip3, char *metiket,
!                      int getmode)
  function fstmsq(iun, mip1, mpi2, mpi3, metiket, getmode) result(status)
      implicit none
      integer(C_INT), intent(IN), value :: iun, getmode
      integer(C_INT), intent(INOUT) :: mip1, mpi2, mpi3
      character(len=*), intent(INOUT) :: metiket
      integer(C_INT) :: status
      character(len=13) :: eti
      eti = metiket
      eti(13:13) = achar(0)
      status = c_fstmsq(iun, mip1, mpi2, mpi3, eti, getmode)
      metiket = eti(1:min(12,len(metiket)))
  end function fstmsq

  function msq(this, mip1, mpi2, mpi3, metiket, getmode) result(status)
      implicit none
      class(fstd98), intent(IN) :: this
      integer(C_INT), intent(IN), value :: getmode
      integer(C_INT), intent(INOUT) :: mip1, mpi2, mpi3
      character(len=*), intent(INOUT) :: metiket
      integer(C_INT) :: status
      status = fstmsq(this%iun, mip1, mpi2, mpi3, metiket, getmode)
  end function msq

! /***************************************************************************** 
!  *                             I P n _ A L L                                 *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Generates all possible coded ip1 values for a given level               *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  level          ip1 level (float value)                               * 
!  *  IN  kind           level kind as defined in convip_plus                  * 
!  *                                                                           * 
!  *****************************************************************************/

    function ip1_all(level, vkind) result(ip_new)
      implicit none
      real(C_FLOAT), intent(IN), value :: level
      integer(C_INT), intent(IN), value :: vkind
      integer(C_INT) :: ip_new
      ip_new = c_ip1_all(level, vkind)
    end function ip1_all
    function ip2_all(level, vkind) result(ip_new)
      implicit none
      real(C_FLOAT), intent(IN), value :: level
      integer(C_INT), intent(IN), value :: vkind
      integer(C_INT) :: ip_new
      ip_new = c_ip2_all(level, vkind)
    end function ip2_all
    function ip3_all(level, vkind) result(ip_new)
      implicit none
      real(C_FLOAT), intent(IN), value :: level
      integer(C_INT), intent(IN), value :: vkind
      integer(C_INT) :: ip_new
      ip_new = c_ip3_all(level, vkind)
    end function ip3_all

! /***************************************************************************** 
!  *                             I P n _ V A L                                 *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Generates all possible coded ip1 values for a given level               *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  level          ip1 level (float value)                               * 
!  *  IN  kind           level kind as defined in convip_plus                  * 
!  *                                                                           * 
!  *****************************************************************************/

    function ip1_val(level, vkind) result(ip_new)
      implicit none
      real(C_FLOAT), intent(IN), value :: level
      integer(C_INT), intent(IN), value :: vkind
      integer(C_INT) :: ip_new
      ip_new = c_ip1_val(level, vkind)
    end function ip1_val
    function ip2_val(level, vkind) result(ip_new)
      implicit none
      real(C_FLOAT), intent(IN), value :: level
      integer(C_INT), intent(IN), value :: vkind
      integer(C_INT) :: ip_new
      ip_new = c_ip2_val(level, vkind)
    end function ip2_val
    function ip3_val(level, vkind) result(ip_new)
      implicit none
      real(C_FLOAT), intent(IN), value :: level
      integer(C_INT), intent(IN), value :: vkind
      integer(C_INT) :: ip_new
      ip_new = c_ip3_val(level, vkind)
    end function ip3_val

!  /*****************************************************************************
!  *                              F S T O P I                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Print out or set a fstd or xdf global variable option.                  *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *   IN     option   option name to be set/printed                           * 
!  *   IN     value    option value                                            * 
!  *   IN     getmode  logical (1: get option, 0: set option)                  * 
!  *                                                                           * 
!  *****************************************************************************/

    function fstopi(option, val, getmode) result(status)
      implicit none
      character(len=*), intent(IN) :: option
      integer(C_INT), intent(IN), value :: val, getmode
      integer(C_INT) :: status
      status = c_fstopi(trim(option)//achar(0), val, getmode)
    end function fstopi

!  /***************************************************************************** 
!  *                              F S T O P L                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Print out or set a fstd or xdf global variable option.                  *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *   IN     option   option name to be set/printed                           * 
!  *   IN     value    option value                                            * 
!  *   IN     getmode  logical (1: get option, 0: set option)                  * 
!  *                                                                           * 
!  *****************************************************************************/

    function fstopl(option, val, getmode) result(status)
      implicit none
      character(len=*), intent(IN) :: option
      integer(C_INT), intent(IN), value :: val, getmode
      integer(C_INT) :: status
      status = c_fstopl(trim(option)//achar(0), val, getmode)
    end function fstopl

!  /***************************************************************************** 
!  *                              F S T O P R                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Print out or set a fstd or xdf global variable option.                  *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *   IN     option   option name to be set/printed                           * 
!  *   IN     value    option value                                            * 
!  *   IN     getmode  logical (1: get option, 0: set option)                  * 
!  *                                                                           * 
!  *****************************************************************************/

    function fstopr(option, val, getmode) result(status)
      implicit none
      character(len=*), intent(IN) :: option
      real(C_FLOAT), intent(IN), value :: val
      integer(C_INT), intent(IN), value :: getmode
      integer(C_INT) :: status
      status = c_fstopr(trim(option)//achar(0), val, getmode)
    end function fstopr

! /*****************************************************************************
!  *                              F S T O P C                                  *
!  *                                                                           *
!  *Object                                                                     *
!  *   Print out or set a fstd or xdf global variable option.                  *
!  *                                                                           *
!  *Arguments                                                                  *
!  *                                                                           *
!  *   IN     option   option name to be set/printed                           *
!  *   IN     value    option value                                            *
!  *   IN     getmode  logical (1: get option, 0: set option)                  *
!  *                                                                           *
!  *****************************************************************************/

    function fstopc(option, val, getmode) result(status)
      implicit none
      character(len=*), intent(IN) :: option
      character(len=*), intent(IN) :: val
      integer(C_INT), intent(IN), value :: getmode
      integer(C_INT) :: status
      status = c_fstopc(trim(option)//achar(0), trim(val)//achar(0), getmode)
    end function fstopc

!  /*****************************************************************************
!  *                              F S T C H E C K                              *
!  *                                                                           *
!  *Object                                                                     *
!  *   Checks if an RPN standard file is valid.                                *
!  *                                                                           *
!  *Arguments                                                                  *
!  *                                                                           *
!  *  IN  filename Path of the file to be checked                              *
!  *                                                                           *
!  *****************************************************************************/
    function fstcheck(path) result(status)
      implicit none
      character(len=*), intent(IN) :: path
      integer(C_INT) :: status
      status = c_fstcheck(trim(path)//achar(0))
    end function fstcheck

!  /***************************************************************************** 
!  *                   F S T R E S E T _ I P _ F L A G S                       *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Reset all the flags previously set by ip(1-3)_val                       *
!  *                                                                           * 
!  *****************************************************************************/
    subroutine fstreset_ip_flags()
      call c_fstreset_ip_flags()
    end subroutine fstreset_ip_flags

!  /***************************************************************************** 
!  *                               F S T R W D                                 *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Rewinds a RPN standard sequential file.                                 *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *                                                                           * 
!  *****************************************************************************/
    function fstrwd(iun) result(status)
      implicit none
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT) :: status
      status = c_fstrwd(iun)
    end function fstrwd

    function rwd(this) result(status)
      implicit none
      class(fstd98), intent(IN) :: this
      integer(C_INT) :: status
      status = fstrwd(this%iun)
    end function rwd

!  /***************************************************************************** 
!  *                                F S T S K P                                *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Skip nrec records forward or backward in the sequential file.           *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  IN  nrec    number of records to skip (negative nrec means backward)     * 
!  *                                                                           * 
!  *****************************************************************************/

    function fstskp(iun, nrec) result(status)
      implicit none
      integer(C_INT), intent(IN), value :: iun, nrec
      integer(C_INT) :: status
      status = c_fstskp(iun, nrec)
    end function fstskp

    function skp(this, nrec) result(status)
      implicit none
      class(fstd98), intent(IN) :: this
      integer(C_INT), intent(IN), value :: nrec
      integer(C_INT) :: status
      status = c_fstskp(this%iun, nrec)
    end function skp


! /***************************************************************************** 
!  *                           F S T  _ V E R S I O N                          *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Returns package version number.                                          *
!  *                                                                           * 
!  *****************************************************************************/

    function fst_version() result(version)
      implicit none
      integer(C_INT) :: version
      version = c_fst_version()
    end function fst_version

! /***************************************************************************** 
!  *                              F S T V O I                                  *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Opens a RPN standard file.                                              *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  IN  options random or sequential access                                  * 
!  *                                                                           * 
!  *****************************************************************************/

    function fstvoi(iun, options) result(status)
      implicit none
      integer(C_INT), intent(IN), value :: iun
      character(len=*), intent(IN) :: options
      integer(C_INT) :: status
      status = c_fstvoi(iun, trim(options)//achar(0))
    end function fstvoi

    function voi(this, options) result(status)
      implicit none
      class(fstd98), intent(IN) :: this
      character(len=*), intent(IN) :: options
      integer(C_INT) :: status
      status = fstvoi(this%iun, trim(options)//achar(0))
    end function voi

!  /***************************************************************************** 
!  *                             F S T W E O                                   *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Writes a logical end of file on a sequential file.                      *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  IN  level   level of logical end of file                                 * 
!  *                                                                           * 
!  *****************************************************************************/

    function fstweo(iun, level) result(status)
      implicit none
      integer(C_INT), intent(IN), value :: iun, level
      integer(C_INT) :: status
      status = c_fstweo(iun, level)
    end function fstweo

    function weo(this, level) result(status)
      implicit none
      class(fstd98), intent(IN) :: this
      integer(C_INT), intent(IN), value :: level
      integer(C_INT) :: status
      status = fstweo(this%iun, level)
    end function weo

! /***************************************************************************** 
!  *                             F S T A P P                                   *
!  *                                                                           * 
!  *Object                                                                     * 
!  *   Position at the end of a sequential file for an append.                 *
!  *                                                                           * 
!  *Arguments                                                                  * 
!  *                                                                           * 
!  *  IN  iun     unit number associated to the file                           * 
!  *  IN  option  kept for backward compatibility (not used)                   * 
!  *                                                                           * 
!  *****************************************************************************/
    function fstapp(iun, option) result(status)
      implicit none
      integer(C_INT), intent(IN), value :: iun
      character(len=*), intent(IN) :: option
      integer(C_INT) :: status
      status = c_fstapp(iun, trim(option)//achar(0))
    end function fstapp

    function app(this, option) result(status)
      implicit none
      class(fstd98), intent(IN) :: this
      character(len=*), intent(IN) :: option
      integer(C_INT) :: status
      status = fstapp(this%iun, option)
    end function app
! 
end module
