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
! ====================================================
#define FNOM_OWNER
module fnom_helpers         ! routines for internal use only
  use ISO_C_BINDING
  implicit none
#include <librmn_interface.hf>
! ====================================================
! this is a Fortran interface to the active routine written in C.
! the fortran functions (qqqfopen,qqqfclos) that c_fnom may have to call
! are passed as callbacks (address of function to call)
! this helps isolate c_baseio from any need to know fortran names and types
! ====================================================
  interface
    subroutine c_fnom_ext(qqqfopen,qqqfclos) bind(C,name='c_fnom_externals')
      import
      type(C_FUNPTR), intent(IN), value :: qqqfopen
      type(C_FUNPTR), intent(IN), value :: qqqfclos
    end subroutine c_fnom_ext
    function cqqqfnom(iun,name,ftyp,flrec,lname,lftyp) result(status) bind(C,name='F_qqqfnom')
      import
      integer(C_INT), intent(IN), value :: iun, lname, lftyp
      integer(C_INT), intent(OUT) :: flrec
      character(C_CHAR), dimension(lname), intent(OUT) :: name
      character(C_CHAR), dimension(lftyp), intent(OUT) :: ftyp
      integer(C_INT) :: status
    end function cqqqfnom
  end interface
  contains
  ! close a Fortran file (opened with Fortran open) (normally used as a callback by c_fnom)
  function ftnclos_c(iun) result(status) bind(C,name='F90clos_for_c') ! for C callback
    use ISO_C_BINDING
    implicit none
    integer(C_INT), intent(IN) :: iun
    integer(C_INT) :: status

    close(unit = iun)
    status = 0
    return
  end function ftnclos_c

  ! C callable function (called by c_fnom) to address file open operations
  ! that must be performed by the Fortran library
  ! passed as a "callback"
  ! lrec : coming directly from fnom c_fnom call, needs to be multiplied by d77mult
  function qqqf7op_c(iun,c_name, c_options, lrec, rndflag, unfflag) result(status) bind(C,name='QQQf7op_for_c') ! for C callback
    use ISO_C_BINDING
    implicit none
    integer(C_INT), intent(IN), value :: iun, lrec, rndflag, unfflag
    character(C_CHAR), dimension(*), intent(IN) :: c_name, c_options  ! C NULL terminated strings
    integer(C_INT) :: status

    integer i, stat
    integer, save :: d77mult = 0
    logical :: opened
    integer, dimension(5) :: scrap

    scrap = 0  ! get rid of compiler warning
    inquire (unit=iun, opened=opened, iostat=stat)
    if(opened) then
      status = -1
      print *,'ERROR: Fortran unit',iun,' is already open'
      return
    endif
    if(d77mult == 0) then   ! find value of dmult (1 or 4) (compiler dependent)
      i = 100
      opened = .true.
      do while(opened .and. iun > 1)
        i = i - 1
        inquire (unit=i, opened=opened, iostat=stat)
      enddo
      open(unit=i,ACCESS='DIRECT',FORM='UNFORMATTED',STATUS='SCRATCH',RECL=5)
      d77mult = 4                     ! this value if write generates an error
      write(iun,rec=1,err=1) scrap    ! there will be an error if d77mult needs to be 4 (recl in bytes)
      d77mult = 1                     ! no error, recl had room for 16 bytes, recl is in words
  1   close(unit=i)
  !     print *,'DEBUG: d77mult =',d77mult
    endif

    status = qqqf7op_plus(iun, c_name, c_options, lrec*d77mult, rndflag ,unfflag)
    return
  end function qqqf7op_c

  ! this is called by the C code to set some Fortran I/O setup options (NOT USER CALLABLE)
  ! iun      : Fortran unit number
  ! path     : file name
  ! options  : SCRATCH APPEND OLD R/O (other options are ignored)
  ! lrec     : record length for direct access open (includes d77mult factor)
  ! rndflag  : 0 sequential file, 1 random file
  ! unfflag  : 0 formatted file, 1 unformattted file
  ! lng_in   : length of path
  FUNCTION qqqf7op_plus(iun,path,options,lrec,rndflag,unfflag) result(status)
    use ISO_C_BINDING
    implicit none
    integer(C_INT), intent(IN) :: iun, lrec, rndflag ,unfflag
    character(C_CHAR), dimension(*), intent(IN) :: path, options   ! C NULL terminated strings
    integer(C_INT) :: status
  
    character(len=4096) :: name
    character(len=16) :: acc, form, action, position, fstat
    character(len=128) :: optns
    integer :: i, lng
!
    status=0
!   copy null terminated C strings to Fortran strings
    i = 1
    lng = 0
    name = ' '
    do while(path(i) .ne. achar(0) .and. i < 4096)   ! copy path to Fortran string
      lng = lng + 1
      name(lng:lng) = path(i)
      i = i + 1
    enddo
    i = 1
    optns = ' '
    do while(options(i) .ne. achar(0) .and. i < 128) ! copy options to Fortran string
      optns(i:i) = options(i)
      i = i + 1
    enddo
!
    if ((name(1:lng).EQ.'input') .OR. (name(1:lng).EQ.'$input')            &
    &  .OR. (name(1:lng).EQ.'output') .OR. (name(1:lng).EQ.'$output')     &
    &  .OR. (name(1:lng).EQ.'$in') .OR. (name(1:lng).EQ.'$out'))          &
    &  then
      return        ! stdin/stdout, nothing to do
    endif
!   set default values for options
    form     = 'FORMATTED'     ! default values
    acc      = 'SEQUENTIAL'
    position = 'REWIND'
    action   = 'READWRITE'
    fstat    = 'UNKNOWN'
!   process options SCRATCH, APPEND, OLD, R/O  (might add others if need be in the future)
    if (rndflag == 1) acc  = 'DIRECT'          ! already processed by c_fnom
    if (unfflag == 1) form = 'UNFORMATTED'     ! already processed by c_fnom
    if( index(optns,'SCRATCH',.false.) > 0 .or. index(optns,'scratch',.false.) > 0 ) then
      fstat    = 'SCRATCH'
      name     = 'NoNe'      ! name is irrelevant, make sure to give an acceptable one
      lng = 4
    endif
    if( index(optns,'APPEND',.false.) > 0 .or. index(optns,'append',.false.) > 0 ) then    ! open in "append" mode
      position = 'APPEND'
    endif
    if( index(optns,'OLD',.false.) > 0 .or. index(optns,'old',.false.) > 0 ) then          ! file MUST EXIST
      fstat    = 'OLD'
    endif
    if( index(optns,'R/O',.false.) > 0 .or. index(optns,'r/o',.false.) > 0 ) then          ! file is "read only"
      fstat    = 'OLD'       ! better exist if file is to be r/o
      action   = 'READ'
    endif
!
    if(rndflag == 1) then  ! no position nor formatted if random 77
      if(trim(fstat) == 'SCRATCH') then
        OPEN(iun                 ,ACCESS='DIRECT',FORM='UNFORMATTED', &
          STATUS='SCRATCH',ACTION=action,RECL=lrec,ERR=77)
      else
        OPEN(iun,FILE=name(1:lng),ACCESS='DIRECT',FORM='UNFORMATTED', &
          STATUS=fstat    ,ACTION=action,RECL=lrec,ERR=77)
      endif
    else                   ! no recl if not random 77, positioning is allowed as well as formatted (if not scratch)
      if(trim(fstat) == 'SCRATCH') then
        if(unfflag == 1) &
          OPEN(iun,ACCESS='SEQUENTIAL',FORM=form,STATUS='SCRATCH', &
              POSITION=position,ACTION=action              ,ERR=77)
        if(unfflag == 0) &
          OPEN(iun,ACCESS='SEQUENTIAL',FORM=form,STATUS='SCRATCH', &
              POSITION=position,ACTION=action,DELIM='QUOTE',ERR=77)
      else   ! use quote delimiter if formatted file
        if(unfflag == 1) &
          OPEN(iun,FILE=name(1:lng),ACCESS='SEQUENTIAL',FORM=form,STATUS=fstat    , &
              POSITION=position,ACTION=action              ,ERR=77)
        if(unfflag == 0) &
          OPEN(iun,FILE=name(1:lng),ACCESS='SEQUENTIAL',FORM=form,STATUS=fstat    , &
              POSITION=position,ACTION=action,DELIM='QUOTE',ERR=77)
      endif
    endif
!
    return
77  continue
    print *,'error in qqqf7op_from_c'
    status = -1
    return
  end
end module fnom_helpers
!
! open a file with some options (files known or not to Fortran)
!
! iun        : if zero upon entry, fnom will find an appropriate unit number
! name       : file name (character string)
!              some_name@file_path refers to file some_name inside CMCARC archive file_path
! opti       : list of + separated options (upper case or lower case)
!              STD         RPN "standard" file (implies WA+RND) (non Fortran)
!              FTN         Fortran file (UNF, FMT, D77, APPEND may be used as other attributes)
!              D77         Fortran direct access file (reclen must be non zero)
!              UNF         Fortran sequential unformatted file (default is formatted)
!              RND         random access file (normally used with STD)  (non Fortran)
!              WA          Word Addressable file (Big Endian) (implies RND)  (non Fortran)
!              STREAM      stream file (non Fortran, no record markers, Big Endian)
!              BURP        Meteorological reports file  (non Fortran)
!              APPEND      file is opened in "append" mode (write at end)
!              OLD         file must exist (applies to all files)
!              R/O         file is Read Only (default is Read/Write) (applies to all files) (implies OLD)
!              R/W         file is Read Write (default) (applies to all files)
!              SCRATCH     File will be removed when closed (applies to all files)
!              SPARSE      Unix WA sparse file (may be written into far beyond end of file)  (non Fortran)
!              ex.   STD+RND+OLD+R/W open existing random standard file for reading and writing
!                    FTN+UNF         open Fortran sequential file for reading and writing , create it if it does not exist
! reclen     : record length in 4 byte units for Fortran D77 file records (should be zero otherwise, mostly ignored)
!
function fnom(iun,name,opti,reclen) result (status)
  use ISO_C_BINDING
  use fnom_helpers
  implicit none
  integer(C_INT), intent(INOUT) :: iun
  integer(C_INT), intent(IN)    :: reclen
  character(len=*), intent(IN)  :: name,opti
  integer(C_INT)                :: status

!   character(C_CHAR), dimension(len(trim(name))+1), target :: name1
!   character(C_CHAR), dimension(len(trim(opti))+1), target :: opti1
  logical :: opened
  integer :: stat, last_unit

  if(iun == 0) then                            ! automatic assign for Fortran files
    if( index(opti,'FTN',.false.) > 0 .or. &
        index(opti,'ftn',.false.) > 0 .or. &
        index(opti,'D77',.false.) > 0 .or. &
        index(opti,'d77',.false.) > 0) then
      opened = .true.
      last_unit = 100
      do while(opened .and. last_unit > 1)
        last_unit = last_unit - 1
        inquire (unit=last_unit, opened=opened, iostat=stat)
      enddo
      if( .not. opened) then
        iun = last_unit
!         print *,'DEBUG: assigning unit =',iun
      endif
    endif
  endif

!   name1 = transfer(trim(name)//achar(0),name1)
!   opti1 = transfer(trim(opti)//achar(0),opti1)

  call c_fnom_ext(C_FUNLOC(qqqf7op_c), C_FUNLOC(ftnclos_c))   ! setup callbacks from C

!   status = c_fnom(iun, name1, opti1, reclen)                ! get the job done
  status = c_fnom(iun, trim(name)//achar(0), trim(opti)//achar(0), reclen)                ! get the job done
end function fnom

! iun   : file unit
! name  : file name
! ftyp  : file options string
! flrec : record length, as specified in call to fnom/c_fnom
function qqqfnom(iun,name,ftyp,flrec) result(status)  ! get filename, properties, record length  info from unit number
  use ISO_C_BINDING
  use fnom_helpers
  implicit none
  integer(C_INT), intent(IN)    :: iun
  integer(C_INT), intent(OUT)   :: flrec
  character(len=*), intent(OUT) :: name,ftyp
  integer(C_INT)                :: status

  character(len=1), dimension(len(name)) :: name1
  character(len=1), dimension(len(ftyp)) :: ftyp1
  integer :: lname, lftyp, i

  lname = len(name)
  lftyp = len(ftyp)
  status = cqqqfnom(iun,name1,ftyp1,flrec,lname,lftyp)  ! get job done by C function, blank padded
  do i = 1 , lftyp
    ftyp(i:i) = ftyp1(i)
  enddo
  do i = 1 , lname
    name(i:i) = name1(i)
  enddo

end function qqqfnom

! close a file opened by fnom
function fclos(iun) result(status)
  use fnom_helpers
  implicit none
  integer, intent(IN) :: iun
  integer :: status

  status = c_fclos(iun)
  return
end function fclos

! no longer useful, NO-OP, legacy
! always returns 0
! iun : Fortran unit number
function fretour(iun) result(status)
  integer :: status
  if(iun .ne. -99999999) status = 0  ! idiot test to prevent compiler warning about unused argument
  return
end function fretour

integer function longueur(nom)    ! legacy, length of a Fortran character string
  implicit none
  character (len=*) nom

  longueur = len(trim(nom))
  RETURN
END
! ====================================================
!  waopen/waclos waread/waread64/wawrit/wawrit64
!   random access by word (32 bit) routines
!   these routines take care of endian conversion
!   the file contents are always BIG-ENDIAN (32 bit units)
! IUN(IN)       : fortran unit number
! BUF(IN/OUT)   : array to write from or read into
! NMOTS(IN)     : number of "words" to read (word = 4 bytes)
! ADR(IN)       : address of first word for transfer
!                 32 bit integer (waread, wawrit)
!                 64 bit integer (waread64, wawrit64)
! PARTITION(IN) : deferred implementation (partition 0 only for now)
!                 (waread64, wawrit64)
!               file starts at word #1
! ====================================================
subroutine waopen(iun)    ! open a Word Addressable (WA) file
  use fnom_helpers
  implicit none
  integer(C_INT), intent(IN) :: iun

  call c_waopen(iun)
end subroutine waopen

subroutine waclos(iun)    ! close a Word Addressable (WA) file
  use fnom_helpers
  implicit none
  integer(C_INT), intent(IN) :: iun

  call c_waclos(iun)
end subroutine waclos

subroutine waread(iun,buf,adr,nmots)                         ! read from a Word Addressable (WA) file
  use fnom_helpers
  implicit none
  integer(C_INT), intent(IN) :: iun, nmots
  integer(C_INT), intent(IN) :: adr                          ! max = 2GWords, 8GBytes
  integer(C_INT), intent(OUT), dimension(nmots), target :: buf

  call c_waread(iun,C_LOC(buf),adr,nmots)
end subroutine waread

function waread64(iun,buf,adr,nmots,partition) result(nw32)  ! same as waread, but supports large files
  use fnom_helpers
  implicit none
  integer(C_INT), intent(IN) :: iun, nmots
  integer(C_INT), intent(IN) :: partition                    ! partitioned file (deferred implementation)
  integer(C_LONG_LONG), intent(IN) :: adr                    ! max = 2**63 Words, 2**65 Bytes
  integer(C_INT), intent(OUT), dimension(nmots), target :: buf
  integer(C_INT) :: nw32

  nw32 = c_waread64(iun,C_LOC(buf),adr,nmots,partition)
end function waread64

subroutine wawrit(iun,buf,adr,nmots)                         ! write into a Word Addressable (WA) file
  use fnom_helpers
  implicit none
  integer(C_INT), intent(IN) :: iun, nmots
  integer(C_INT), intent(IN) :: adr                          ! max = 2GWords, 8GBytes
  integer(C_INT), intent(OUT), dimension(nmots), target :: buf

  call c_wawrit(iun,C_LOC(buf),adr,nmots)
end subroutine wawrit

function wawrit64(iun,buf,adr,nmots,partition) result(nw32)  ! same as wawrit, but supports large files
  use fnom_helpers
  implicit none
  integer(C_INT), intent(IN) :: iun, nmots
  integer(C_INT), intent(IN) :: partition                    ! partitioned file (deferred implementation)
  integer(C_LONG_LONG), intent(IN) :: adr                    ! max = 2**63 Words, 2**65 Bytes
  integer(C_INT), intent(OUT), dimension(nmots), target :: buf
  integer(C_INT) :: nw32

  nw32 = c_wawrit64(iun,C_LOC(buf),adr,nmots,partition)
end function wawrit64
! ====================================================
!  LEGACY routines, probably not used any more
!     openda/closda readda/writda/checda
!     "asynchronous" random access by block routines
!     (the current implementation is SYNCHRONOUS)
! IUN(IN)     : fortran unit number
! BUF(IN/OUT) : array to write from or read into
! NS          : number of "sectors" (512 x 32 bit words)
! IS          : address of first "sector" for transfer
!               file starts at sector #1
!   these routines take care of endian conversion
!   the file contents are always BIG-ENDIAN (32 bit units)
! ====================================================
subroutine openda(iun)
  use ISO_C_BINDING
  implicit none
  integer(C_INT), intent(IN) :: iun
  call waopen(iun)
end subroutine openda
subroutine readda(iun,buf,ns,is)
  use fnom_helpers
  implicit none
  integer(C_INT), intent(IN) :: iun, ns, is
  integer(C_INT), intent(OUT), dimension(*), target :: buf
  call c_waread(iun,C_LOC(buf),(is-1)*512+1,ns*512)
end subroutine readda
subroutine writda(iun,buf,ns,is)
  use fnom_helpers
  implicit none
  integer(C_INT), intent(IN) :: iun, ns, is
  integer(C_INT), intent(IN), dimension(*), target :: buf
  call c_wawrit(iun,C_LOC(buf),(is-1)*512+1,ns*512)
end subroutine writda
subroutine checda(iun)
  use ISO_C_BINDING
  implicit none
  integer(C_INT), intent(IN) :: iun
  if(iun < 0) print *,'ERROR: (checda) invalid Fortran unit',iun
end subroutine checda
subroutine closda(iun)
  use ISO_C_BINDING
  implicit none
  integer(C_INT), intent(IN) :: iun
  call waclos(iun)
end subroutine closda
!
! ====================================================
! return 1 if file exists, 0 otherwise
function existe(name) result(status)
  use fnom_helpers
  implicit none
  character(len=*), intent(IN) :: name
  integer(C_INT) :: status
!   character(C_CHAR), dimension(len(trim(name))+1), target :: name1

!   name1 = transfer(trim(name)//achar(0),name1)
!   status = c_existe(C_LOC(name1(1)))
  status = c_existe(trim(name)//achar(0))
  return
end function existe

! get C file descriptor associated to unit iun (-1 if there is none)
function getfdsc(iun) result(fd)
  use fnom_helpers
  implicit none
  integer(C_INT), intent(IN) :: iun
  integer(C_INT) :: fd

  fd = c_getfdsc(iun)
end function getfdsc

! get size of file in 512 word (32 bit) blocks
function numblks(iun) result(nblks)
  use fnom_helpers
  implicit none
  integer(C_INT), intent(IN) :: iun
  integer(C_INT) :: nblks

  nblks = c_numblks(iun)
end function numblks

! get size of file in word (32 bit) units
function wasize(iun) result(nwds32)
  use fnom_helpers
  implicit none
  integer(C_INT), intent(IN) :: iun
  integer(C_INT) :: nwds32

  nwds32 = c_wasize(iun)
end function wasize
