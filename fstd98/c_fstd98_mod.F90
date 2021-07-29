module c_fstd98_mod
  use ISO_C_BINDING
  implicit none

  interface

    function c_fst_data_length(length_type) result(status) bind(C, name='c_fst_data_length')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: length_type
      integer(C_INT) :: status
    end function c_fst_data_length

    function c_ip1_all(level, vkind) result(ip_new) bind(C, name='c_ip1_all')
      import :: C_INT, C_FLOAT
      implicit none
      real(C_FLOAT), intent(IN), value :: level
      integer(C_INT), intent(IN), value :: vkind
      integer(C_INT) :: ip_new
    end function c_ip1_all

    function c_ip1_val(level, vkind) result(ip_new) bind(C, name='c_ip1_val')
      import :: C_INT, C_FLOAT
      implicit none
      real(C_FLOAT), intent(IN), value :: level
      integer(C_INT), intent(IN), value :: vkind
      integer(C_INT) :: ip_new
    end function c_ip1_val

    function c_ip2_all(level, vkind) result(ip_new) bind(C, name='c_ip2_all')
      import :: C_INT, C_FLOAT
      implicit none
      real(C_FLOAT), intent(IN), value :: level
      integer(C_INT), intent(IN), value :: vkind
      integer(C_INT) :: ip_new
    end function c_ip2_all

    function c_ip2_val(level, vkind) result(ip_new) bind(C, name='c_ip2_val')
      import :: C_INT, C_FLOAT
      implicit none
      real(C_FLOAT), intent(IN), value :: level
      integer(C_INT), intent(IN), value :: vkind
      integer(C_INT) :: ip_new
    end function c_ip2_val

    function c_ip3_all(level, vkind) result(ip_new) bind(C, name='c_ip3_all')
      import :: C_INT, C_FLOAT
      implicit none
      real(C_FLOAT), intent(IN), value :: level
      integer(C_INT), intent(IN), value :: vkind
      integer(C_INT) :: ip_new
    end function c_ip3_all

    function c_ip3_val(level, vkind) result(ip_new) bind(C, name='c_ip3_val')
      import :: C_INT, C_FLOAT
      implicit none
      real(C_FLOAT), intent(IN), value :: level
      integer(C_INT), intent(IN), value :: vkind
      integer(C_INT) :: ip_new
    end function c_ip3_val

    function c_xdflnk(list,n) result(status) bind(C, name='c_xdflnk')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: n
      integer(C_INT), dimension(n) :: list
      integer(C_INT) :: status
    end function c_xdflnk

    function c_xdfunl(list,n) result(status) bind(C, name='c_xdfunl')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: n
      integer(C_INT), dimension(n) :: list
      integer(C_INT) :: status
    end function c_xdfunl

    function c_fstckp(iun) result (status) bind(C,name='c_fstckp')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT) :: status
    end function c_fstckp

    function c_fstmsq(iun, mip1, mpi2, mpi3, metiket, getmode) result(status) bind(C,name='c_fstmsq')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: iun, getmode
      integer(C_INT), intent(INOUT) :: mip1, mpi2, mpi3
      character(C_CHAR), dimension(*), intent(INOUT) :: metiket
      integer(C_INT) :: status
    end function c_fstmsq

    function c_fstopi(option, val, getmode) result(status) bind(C,name='c_fstopi')
      import :: C_INT, C_CHAR
      implicit none
      character(C_CHAR), dimension(*), intent(IN) :: option
      integer(C_INT), intent(IN), value :: val, getmode
      integer(C_INT) :: status
    end function c_fstopi

    function c_fstopl(option, val, getmode) result(status) bind(C,name='c_fstopl')
      import :: C_INT, C_CHAR
      implicit none
      character(C_CHAR), dimension(*), intent(IN) :: option
      integer(C_INT), intent(IN), value :: val, getmode
      integer(C_INT) :: status
    end function c_fstopl

    function c_fstopr(option, val, getmode) result(status) bind(C,name='c_fstopr')
      import :: C_INT, C_CHAR, C_FLOAT
      implicit none
      character(C_CHAR), dimension(*), intent(IN) :: option
      integer(C_INT), intent(IN), value :: getmode
      real(C_FLOAT), intent(IN), value :: val
      integer(C_INT) :: status
    end function c_fstopr

    function c_fstopc(option, val, getmode) result(status) bind(C,name='c_fstopc')
      import :: C_INT, C_CHAR
      implicit none
      character(C_CHAR), dimension(*), intent(IN) :: option
      integer(C_INT), intent(IN), value :: getmode
      character(C_CHAR), dimension(*), intent(IN) :: val
      integer(C_INT) :: status
    end function c_fstopc

    function c_fstcheck(path) result(status) bind(C,name='c_fstcheck')
      import :: C_INT, C_CHAR
      implicit none
      character(C_CHAR), dimension(*), intent(IN) :: path
      integer(C_INT) :: status
    end function c_fstcheck

    function c_fstinf(iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) &
                      result(handle) bind(C,name='c_fstinf')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: iun, datev, ip1, ip2, ip3
      integer(C_INT), intent(OUT)       :: ni, nj, nk
      character(C_CHAR), dimension(*), intent(IN) :: typvar, nomvar, etiket
      integer(C_INT) :: handle
    end function c_fstinf

    function c_fstsui(iun, ni, nj, nk) result(handle) bind(C,name='c_fstsui')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT), intent(OUT)       :: ni, nj, nk
      integer(C_INT) :: handle
    end function c_fstsui

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

    function c_fstinfx(handle, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) &
                      result(handle_new) bind(C,name='c_fstinfx')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: handle, iun, datev, ip1, ip2, ip3
      integer(C_INT), intent(OUT)       :: ni, nj, nk
      character(C_CHAR), dimension(*), intent(IN) :: typvar, nomvar, etiket
      integer(C_INT) :: handle_new
    end function c_fstinfx

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

    function c_fstouv(iun, options) result(status) bind(C, name='c_fstouv')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: iun
      character(C_CHAR), dimension(*), intent(IN) :: options
      integer(C_INT) :: status
    end function c_fstouv

    function c_fstvoi(iun, options) result(status) bind(C, name='c_fstvoi')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: iun
      character(C_CHAR), dimension(*), intent(IN) :: options
      integer(C_INT) :: status
    end function c_fstvoi

    function c_fstapp(iun, option) result(status) bind(C, name='c_fstapp')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: iun
      character(C_CHAR), dimension(*), intent(IN) :: option
      integer(C_INT) :: status
    end function c_fstapp

    function c_fsteff(iun) result(nrec) bind(C, name='c_fsteff')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT) :: nrec
    end function c_fsteff

    function c_fsteof(iun) result(status) bind(C, name='c_fsteof')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT) :: status
    end function c_fsteof

    function c_fstrwd(iun) result(status) bind(C, name='c_fstrwd')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT) :: status
    end function c_fstrwd

    function c_fstskp(iun, nrec) result(status) bind(C, name='c_fstskp')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: iun, nrec
      integer(C_INT) :: status
    end function c_fstskp

    function c_fstweo(iun, level) result(status) bind(C, name='c_fstweo')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: iun, level
      integer(C_INT) :: status
    end function c_fstweo

    function c_fstnbr(iun) result(nrec) bind(C, name='c_fstnbr')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT) :: nrec
    end function c_fstnbr

    function c_fstnbrv(iun) result(nrecv) bind(C, name='c_fstnbrv')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT) :: nrecv
    end function c_fstnbrv

    function c_fstfrm(iun) result(status) bind(C, name='c_fstfrm')
      import :: C_INT
      implicit none
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT) :: status
    end function c_fstfrm

    function c_fst_version() result(version) bind(C, name='c_fst_version')
      import :: C_INT
      implicit none
      integer(C_INT) :: version
    end function c_fst_version

    subroutine c_fstreset_ip_flags() bind(C, name='c_fstreset_ip_flags')
    end subroutine c_fstreset_ip_flags

    subroutine c_fst_env_var(cle, indx, content) bind(C, name='c_fst_env_var')
      import :: C_INT, C_CHAR
      implicit none
      integer(C_INT), intent(IN), value :: indx
      character(C_CHAR), dimension(*), intent(IN) :: cle, content
    end subroutine c_fst_env_var

  end interface
end module
