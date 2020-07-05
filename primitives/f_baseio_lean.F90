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
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!int c_fnom(int *iun,char *nom,char *type,int lrec)
!ftnword f77name(fnom)(ftnword *iun,char *nom,char *type,ftnword *flrec,F2Cl l1,F2Cl l2)
! ====================================================
! fnom (open a file with attributes), see c_fnom for
! argument description
! this is a Fortran interface to the active routine written in C.
! the fortran functions (qqqfopen,qqqfclos) that c_fnom may have to call
! are passed as callbacks (address of function to call)
! this helps isolate c_baseio from any need to know fortran names and types
! ====================================================
#define FNOM_OWNER
module fnom_helpers         ! routines for internal use only
  use ISO_C_BINDING
  implicit none
#include <librmn_interface.hf>
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
  ! options  : SCRATCH APPEND OLD R/O
  ! lrec     : record length for direct access open
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

function fnom(iun,name,opti,reclen) result (status)
  use ISO_C_BINDING
  use fnom_helpers
  implicit none
  integer(C_INT), intent(INOUT) :: iun
  integer(C_INT), intent(IN)    :: reclen
  character(len=*), intent(IN)  :: name,opti
  integer(C_INT)                :: status

  character(C_CHAR), dimension(len(trim(name))+1), target :: name1
  character(C_CHAR), dimension(len(trim(opti))+1), target :: opti1
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
        print *,'DEBUG: assigning unit =',iun
      endif
    endif
  endif

  name1 = transfer(trim(name)//achar(0),name1)
  opti1 = transfer(trim(opti)//achar(0),opti1)

  call c_fnom_ext(C_FUNLOC(qqqf7op_c), C_FUNLOC(ftnclos_c))   ! setup callbacks from C

  status = c_fnom(iun, name1, opti1, reclen)                ! get the job done
end function fnom

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
  status = cqqqfnom(iun,name1,ftyp1,flrec,lname,lftyp)
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

function fretour(iun) result(status)
  integer :: status
! ARGUMENTS: in iun   unit number, ignored
! RETURNS: zero.
! Kept only for backward compatibility. NO-OP
  if(iun .ne. -99999999) status = 0
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
!   random access by word (4 bytes) routines
!   these routines take care of endian conversion
!   the file contents are always BIG-ENDIAN (4 bytes)
! IUN(IN)     : fortran unit number
! BUF(IN/OUT) : array to write from or read into
! NMOTS(IN)   : number of "words" to read (word = 4 bytes)
! ADR(IN)     : address of first word for transfer
! PARTITION(IN) : deferred implementation (partition 0 only for now)
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
print *,'waread, adr, nmots',adr,nmots
  call c_waread(iun,C_LOC(buf),adr,nmots)
end subroutine waread

function waread64(iun,buf,adr,nmots,partition) result(nw32)  ! same as waread, but supports larger files
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

function wawrit64(iun,buf,adr,nmots,partition) result(nw32)  ! same as wawrit, but supports larger files
  use fnom_helpers
  implicit none
  integer(C_INT), intent(IN) :: iun, nmots
  integer(C_INT), intent(IN) :: partition                    ! partitioned file (deferred implementation)
  integer(C_LONG_LONG), intent(IN) :: adr                    ! max = 2**63 Words, 2**65 Bytes
  integer(C_INT), intent(OUT), dimension(nmots), target :: buf
  integer(C_INT) :: nw32

  nw32 = c_wawrit64(iun,C_LOC(buf),adr,nmots,partition)
end function wawrit64

function existe(name) result(status)
  use fnom_helpers
  implicit none
  character(len=*), intent(IN) :: name
  integer(C_INT) :: status
  character(C_CHAR), dimension(len(trim(name))+1), target :: name1

  name1 = transfer(trim(name)//achar(0),name1)
  status = c_existe(C_LOC(name1(1)))
  return
end function existe

function getfdsc(iun) result(fd)
  use fnom_helpers
  implicit none
  integer(C_INT), intent(IN) :: iun
  integer(C_INT) :: fd

  fd = c_getfdsc(iun)
end function getfdsc

function numblks(iun) result(nblks)
  use fnom_helpers
  implicit none
  integer(C_INT), intent(IN) :: iun
  integer(C_INT) :: nblks

  nblks = c_numblks(iun)
end function numblks

function wasize(iun) result(nwds32)
  use fnom_helpers
  implicit none
  integer(C_INT), intent(IN) :: iun
  integer(C_INT) :: nwds32

  nwds32 = c_wasize(iun)
end function wasize
