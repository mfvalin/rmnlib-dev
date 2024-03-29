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
module c_fstd98_mod   ! Fortran interfaces the the c_xxx family of RPN standard files 98 C functions
  use ISO_C_BINDING
  implicit none

  interface

    ! int c_fst_data_length(int length_type);
    function c_fst_data_length(length_type) result(status) bind(C, name='c_fst_data_length')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: length_type
      integer(C_INT) :: status
    end function c_fst_data_length

    ! int c_ip1_all(float level, int kind);
    function c_ip1_all(level, vkind) result(ip_new) bind(C, name='c_ip1_all')
      import :: C_INT, C_FLOAT
      implicit none
      real(C_FLOAT), intent(IN), value :: level
      integer(C_INT), intent(IN), value :: vkind
      integer(C_INT) :: ip_new
    end function c_ip1_all

    ! int c_ip1_val(float level, int kind);
    function c_ip1_val(level, vkind) result(ip_new) bind(C, name='c_ip1_val')
      import :: C_INT, C_FLOAT
      implicit none
      real(C_FLOAT), intent(IN), value :: level
      integer(C_INT), intent(IN), value :: vkind
      integer(C_INT) :: ip_new
    end function c_ip1_val

    ! int c_ip2_all(float level, int kind);
    function c_ip2_all(level, vkind) result(ip_new) bind(C, name='c_ip2_all')
      import :: C_INT, C_FLOAT
      implicit none
      real(C_FLOAT), intent(IN), value :: level
      integer(C_INT), intent(IN), value :: vkind
      integer(C_INT) :: ip_new
    end function c_ip2_all

    ! int c_ip2_val(float level, int kind);
    function c_ip2_val(level, vkind) result(ip_new) bind(C, name='c_ip2_val')
      import :: C_INT, C_FLOAT
      implicit none
      real(C_FLOAT), intent(IN), value :: level
      integer(C_INT), intent(IN), value :: vkind
      integer(C_INT) :: ip_new
    end function c_ip2_val

    ! int c_ip3_all(float level, int kind);
    function c_ip3_all(level, vkind) result(ip_new) bind(C, name='c_ip3_all')
      import :: C_INT, C_FLOAT
      implicit none
      real(C_FLOAT), intent(IN), value :: level
      integer(C_INT), intent(IN), value :: vkind
      integer(C_INT) :: ip_new
    end function c_ip3_all

    ! int c_ip3_val(float level, int kind);
    function c_ip3_val(level, vkind) result(ip_new) bind(C, name='c_ip3_val')
      import :: C_INT, C_FLOAT
      implicit none
      real(C_FLOAT), intent(IN), value :: level
      integer(C_INT), intent(IN), value :: vkind
      integer(C_INT) :: ip_new
    end function c_ip3_val

    ! int c_xdflnk(int *liste, int n);
    function c_xdflnk(list,n) result(status) bind(C, name='c_xdflnk')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: n
      integer(C_INT), dimension(n) :: list
      integer(C_INT) :: status
    end function c_xdflnk

    ! int c_xdfunl(int *liste, int n);
    function c_xdfunl(list,n) result(status) bind(C, name='c_xdfunl')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: n
      integer(C_INT), dimension(n) :: list
      integer(C_INT) :: status
    end function c_xdfunl

    ! int c_fstckp(int iun);
    function c_fstckp(iun) result (status) bind(C,name='c_fstckp')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT) :: status
    end function c_fstckp

    ! int c_fstmsq(int iun, int *mip1, int *mip2, int *mip3, char *metiket, int getmode);
    function c_fstmsq(iun, mip1, mpi2, mpi3, metiket, getmode) result(status) bind(C,name='c_fstmsq')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: iun, getmode
      integer(C_INT), intent(INOUT) :: mip1, mpi2, mpi3
      character(C_CHAR), dimension(*), intent(INOUT) :: metiket
      integer(C_INT) :: status
    end function c_fstmsq

    ! int c_fstopi(char *option, int value, int getmode);
    function c_fstopi(option, val, getmode) result(status) bind(C,name='c_fstopi')
      import :: C_INT, C_CHAR
      implicit none
      character(C_CHAR), dimension(*), intent(IN) :: option
      integer(C_INT), intent(IN), value :: val, getmode
      integer(C_INT) :: status
    end function c_fstopi

    ! int c_fstopl(char *option, int value, int getmode);
    function c_fstopl(option, val, getmode) result(status) bind(C,name='c_fstopl')
      import :: C_INT, C_CHAR
      implicit none
      character(C_CHAR), dimension(*), intent(IN) :: option
      integer(C_INT), intent(IN), value :: val, getmode
      integer(C_INT) :: status
    end function c_fstopl

    ! int c_fstopr(char *option, float value, int getmode);
    function c_fstopr(option, val, getmode) result(status) bind(C,name='c_fstopr')
      import :: C_INT, C_CHAR, C_FLOAT
      implicit none
      character(C_CHAR), dimension(*), intent(IN) :: option
      integer(C_INT), intent(IN), value :: getmode
      real(C_FLOAT), intent(IN), value :: val
      integer(C_INT) :: status
    end function c_fstopr

    ! int c_fstopc(char *option, char *value, int getmode);
    function c_fstopc(option, val, getmode) result(status) bind(C,name='c_fstopc')
      import :: C_INT, C_CHAR
      implicit none
      character(C_CHAR), dimension(*), intent(IN) :: option
      integer(C_INT), intent(IN), value :: getmode
      character(C_CHAR), dimension(*), intent(IN) :: val
      integer(C_INT) :: status
    end function c_fstopc

    ! int c_fstcheck(const char *filePath);
    function c_fstcheck(path) result(status) bind(C,name='c_fstcheck')
      import :: C_INT, C_CHAR
      implicit none
      character(C_CHAR), dimension(*), intent(IN) :: path
      integer(C_INT) :: status
    end function c_fstcheck

    ! int c_fstinf(int iun, int *ni, int *nj, int *nk, int datev,char *in_etiket,
    !              int ip1, int ip2, int ip3, char *in_typvar, char *in_nomvar);
    function c_fstinf(iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) &
                      result(handle) bind(C,name='c_fstinf')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: iun, datev, ip1, ip2, ip3
      integer(C_INT), intent(OUT)       :: ni, nj, nk
      character(C_CHAR), dimension(*), intent(IN) :: typvar, nomvar, etiket
      integer(C_INT) :: handle
    end function c_fstinf

    ! int c_fstsui(int iun, int *ni, int *nj, int *nk);
    function c_fstsui(iun, ni, nj, nk) result(handle) bind(C,name='c_fstsui')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT), intent(OUT)       :: ni, nj, nk
      integer(C_INT) :: handle
    end function c_fstsui

    ! int c_fstinl(int iun, int *ni, int *nj, int *nk, int datev, char *etiket,
    !              int ip1, int ip2, int ip3, char *typvar, char *nomvar,
    !              int *liste, int *infon, int nmax);
    function c_fstinl(iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar, liste, infon, nmax) &
                      result(status) bind(C,name='c_fstinl')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: iun, datev, ip1, ip2, ip3, nmax
      integer(C_INT), intent(OUT)       :: ni, nj, nk
      character(C_CHAR), dimension(*), intent(IN) :: typvar, nomvar, etiket
      integer(C_INT), intent(OUT) :: infon
      integer(C_INT), dimension(nmax), intent(OUT) :: liste
      integer(C_INT) :: status
    end function c_fstinl

    ! int c_fstinfx(int handle, int iun, int *ni, int *nj, int *nk,
    !               int datev,char *in_etiket,
    !               int ip1, int ip2, int ip3, char *in_typvar, char *in_nomvar);
    function c_fstinfx(handle, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) &
                      result(handle_new) bind(C,name='c_fstinfx')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: handle, iun, datev, ip1, ip2, ip3
      integer(C_INT), intent(OUT)       :: ni, nj, nk
      character(C_CHAR), dimension(*), intent(IN) :: typvar, nomvar, etiket
      integer(C_INT) :: handle_new
    end function c_fstinfx

    ! int c_fstlir(int *field, int iun, int *ni, int *nj, int *nk,
    !              int datev, char *etiket,
    !              int ip1, int ip2, int ip3, char *typvar, char *nomvar);
    function c_fstlir(field, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) &
                      result(handle) bind(C,name='c_fstlir')
      import :: C_INT, C_CHAR
      implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
      integer(C_INT), intent(IN), value :: iun, datev, ip1, ip2, ip3
      integer(C_INT), intent(OUT)       :: ni, nj, nk
      character(C_CHAR), dimension(*), intent(IN) :: typvar, nomvar, etiket
      integer(C_INT) :: handle
    end function c_fstlir

    ! int c_fstlirx(int *field, int handle, int iun,
    !               int *ni, int *nj, int *nk, int datev, char *etiket,
    !               int ip1, int ip2, int ip3, char *typvar, char *nomvar);
    function c_fstlirx(field, handle, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) &
                      result(handle_new) bind(C,name='c_fstlirx')
      import :: C_INT, C_CHAR
      implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
      integer(C_INT), intent(IN), value :: handle, iun, datev, ip1, ip2, ip3
      integer(C_INT), intent(OUT)       :: ni, nj, nk
      character(C_CHAR), dimension(*), intent(IN) :: typvar, nomvar, etiket
      integer(C_INT) :: handle_new
    end function c_fstlirx

    ! int c_fstlis(int *field, int iun, int *ni, int *nj, int *nk);
    function c_fstlis(field, iun, ni, nj, nk) &
                      result(handle) bind(C,name='c_fstlis')
      import :: C_INT
      implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT), intent(OUT)       :: ni, nj, nk
      integer(C_INT) :: handle
    end function c_fstlis

    ! int c_fstlic(int *field, int iun, int niin, int njin, int nkin,
    !              int datein, char *etiketin, int ip1in, int ip2in, int ip3in,
    !              char *typvarin, char *nomvarin,
    !              int ig1in, int ig2in, int ig3in, int ig4in, char *grtypin);
    function c_fstlic(field, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar, &
                                            ig1, ig2, ig3, ig4, grtyp) &
                      result(handle) bind(C,name='c_fstlic')
      import :: C_INT, C_CHAR
      implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
      integer(C_INT), intent(IN), value :: iun, datev, ip1, ip2, ip3, ig1, ig2, ig3, ig4
      integer(C_INT), intent(OUT)       :: ni, nj, nk
      character(C_CHAR), dimension(*), intent(IN) :: typvar, nomvar, etiket, grtyp
      integer(C_INT) :: handle
    end function c_fstlic

    ! int c_fstluk(int *field, int handle, int *ni, int *nj, int *nk);
    function c_fstluk(field, handle, ni, nj, nk) result(handle_out) BIND(C,name='c_fstluk')
      import :: C_INT
      implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
      integer(C_INT), intent(IN), value :: handle
      integer(C_INT), intent(OUT) :: ni, nj, nk
      integer(C_INT) :: handle_out
    end function c_fstluk

    ! int c_fstprm(int handle, int *dateo, int *deet, int *npas, int *ni, int *nj, int *nk,
    !              int *nbits, int *datyp, int *ip1, int *ip2, int *ip3, char *typvar,
    !              char *nomvar, char *etiket, char *grtyp,
    !              int *ig1, int *ig2, int *ig3, int *ig4, int *swa, int *lng,
    !              int *dltf, int *ubc, int *extra1, int *extra2, int *extra3);
    subroutine c_fstprm(handle, date, deet, npas, ni, nj, nk, nbits, datyp, ip1, ip2, ip3, &
                        typvar, nomvar, etiket, grtyp, &
                        ig1, ig2, ig3, ig4, swa, lng, dlft, ubc, extra1, extra2, extra3) &
                        bind(C,name='c_fstprm')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: handle
      integer(C_INT), intent(OUT) :: date, deet, npas, ni, nj, nk, nbits, datyp, ip1, ip2, ip3
      integer(C_INT), intent(OUT) :: ig1, ig2, ig3, ig4, swa, lng, dlft, ubc, extra1, extra2, extra3
      character(C_CHAR), dimension(*), intent(OUT) :: typvar, nomvar, etiket, grtyp
    end subroutine c_fstprm

    ! int c_fstecr(int *field_in, void * work, int npak, int iun, int date,
    !              int deet, int npas, int ni, int nj, int nk, int ip1, int ip2, int ip3,
    !              char *in_typvar, char *in_nomvar, char *in_etiket, char *in_grtyp, 
    !              int ig1, int ig2, int ig3, int ig4, int in_datyp_ori, int rewrit);
    function c_fstecr(field, work, npak, iun, date, deet, npas, ni, nj, nk, &
                      ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, &
                      ig1, ig2, ig3, ig4, datyp, rewrite) &
                      result(status) bind(C,name='c_fstecr')
      import :: C_INT, C_CHAR
      implicit none
#define IgnoreTypeKindRank field, work
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
      integer(C_INT), intent(IN), value :: iun, npak, date, deet, npas, ni, nj, nk, datyp, rewrite
      integer(C_INT), intent(IN), value :: ip1, ip2, ip3, ig1, ig2, ig3, ig4
      character(C_CHAR), dimension(*), intent(IN) :: typvar, nomvar, etiket, grtyp
      integer(C_INT) :: status
    end function c_fstecr

    ! int c_fstouv(int iun, char *options);
    function c_fstouv(iun, options) result(status) bind(C, name='c_fstouv')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: iun
      character(C_CHAR), dimension(*), intent(IN) :: options
      integer(C_INT) :: status
    end function c_fstouv

    ! int c_fstvoi(int iun,char *options);
    function c_fstvoi(iun, options) result(status) bind(C, name='c_fstvoi')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: iun
      character(C_CHAR), dimension(*), intent(IN) :: options
      integer(C_INT) :: status
    end function c_fstvoi

    ! int c_fstapp(int iun, char *option);
    function c_fstapp(iun, option) result(status) bind(C, name='c_fstapp')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: iun
      character(C_CHAR), dimension(*), intent(IN) :: option
      integer(C_INT) :: status
    end function c_fstapp

    ! int c_fsteff(int handle);
    function c_fsteff(iun) result(nrec) bind(C, name='c_fsteff')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT) :: nrec
    end function c_fsteff

    ! int c_fsteof(int iun);
    function c_fsteof(iun) result(status) bind(C, name='c_fsteof')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT) :: status
    end function c_fsteof

    ! int c_fstrwd(int iun);
    function c_fstrwd(iun) result(status) bind(C, name='c_fstrwd')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT) :: status
    end function c_fstrwd

    ! int c_fstskp(int iun, int nrec);
    function c_fstskp(iun, nrec) result(status) bind(C, name='c_fstskp')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: iun, nrec
      integer(C_INT) :: status
    end function c_fstskp

    ! int c_fstweo(int iun, int level);
    function c_fstweo(iun, level) result(status) bind(C, name='c_fstweo')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: iun, level
      integer(C_INT) :: status
    end function c_fstweo

    ! int c_fstnbr(int iun);
    function c_fstnbr(iun) result(nrec) bind(C, name='c_fstnbr')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT) :: nrec
    end function c_fstnbr

    ! int c_fstnbrv(int iun);
    function c_fstnbrv(iun) result(nrecv) bind(C, name='c_fstnbrv')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT) :: nrecv
    end function c_fstnbrv

    ! int c_fstfrm(int iun);
    function c_fstfrm(iun) result(status) bind(C, name='c_fstfrm')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT) :: status
    end function c_fstfrm

    ! int c_fst_version();
    function c_fst_version() result(version) bind(C, name='c_fst_version')
      import :: C_INT
      implicit none
      integer(C_INT) :: version
    end function c_fst_version

    ! void c_fstreset_ip_flags();
    subroutine c_fstreset_ip_flags() bind(C, name='c_fstreset_ip_flags')
    end subroutine c_fstreset_ip_flags

    ! void c_fst_env_var(char *cle, int index, char *content);
    subroutine c_fst_env_var(cle, indx, content) bind(C, name='c_fst_env_var')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: indx
      character(C_CHAR), dimension(*), intent(IN) :: cle, content
    end subroutine c_fst_env_var

    ! int c_fst_edit_dir_plus(int handle, unsigned int date, int deet, int npas,
    !                         int ni, int nj, int nk, int ip1, int ip2, int ip3,
    !                         char *in_typvar, char *in_nomvar, char *in_etiket, char *in_grtyp, 
    !                         int ig1, int ig2, int ig3, int ig4, int datyp);
    function c_fst_edit_dir_plus(handle, date, deet, npas, ni, nj, nk, ip1, ip2, ip3, &
                                 typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp) &
                                 result(status) bind(C,name='c_fst_edit_dir_plus')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: handle, date, deet, npas, ni, nj, nk, ip1, ip2, ip3
      integer(C_INT), intent(IN), value :: ig1, ig2, ig3, ig4, datyp
      character(C_CHAR), dimension(*), intent(IN) :: typvar, nomvar, etiket, grtyp
      integer(C_INT) :: status
    end function c_fst_edit_dir_plus

    ! int c_fst_edit_dir(int handle, unsigned int date, int deet, int npas,
    !                    int ni, int nj, int nk, int ip1, int ip2, int ip3,
    !                    char *in_typvar, char *in_nomvar, char *in_etiket, char *in_grtyp, 
    !                    int ig1, int ig2, int ig3, int ig4, int datyp);
    function c_fst_edit_dir(handle, date, deet, npas, ni, nj, nk, ip1, ip2, ip3, &
                            typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp) &
                            result(status) bind(C,name='c_fst_edit_dir')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: handle, date, deet, npas, ni, nj, nk, ip1, ip2, ip3
      integer(C_INT), intent(IN), value :: ig1, ig2, ig3, ig4, datyp
      character(C_CHAR), dimension(*), intent(IN) :: typvar, nomvar, etiket, grtyp
      integer(C_INT) :: status
    end function c_fst_edit_dir

  end interface
end module
