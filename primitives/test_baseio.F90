program test
  use ISO_C_BINDING
  implicit none
#include <librmn_interface.hf>
  integer, external :: wawrit64, waread64
  integer :: iun, status, i, r64, w64, iund77, iun1, iun2
  integer*8 :: ladr
  integer, dimension(1024) :: array0, array1, array2
  integer, dimension(100) :: darray
  character(len=128) :: str1, str2
  character(len=16) :: s1, s2, s3
  namelist /namelist_s/ s1, s2, s3
  interface
    subroutine test_c_fnom() bind(C,name='TEST_c_fnom')
    end subroutine test_c_fnom
  end interface

  s1 = 'string no 1'
  s2 = 'string no 2'
  s3 = 'string no 3'
  do i=1,size(array0)
    array0(i) = i
  enddo
  print *,'========== base IO test, c_baseio + f_baseio =========='
  call test_c_fnom()

  print *,'==========testing with Fortran file 99 already open =========='
  iun1 = 99
  open(unit=99,STATUS='SCRATCH',err=777)
  status = fnom(iun1,'already_open','FTN+FMT',0)
  print *,'an error message is expected'
  print *,'(fnom) iun,status =',iun1,status
  print *,'========== testing formatted I/O + namelist  =========='
  iun1 = 0
  status = fnom(iun1,'test_namelist','FTN+FMT',0)
  print *,'(fnom) iun,status =',iun1,status
  write(iun1,*) 'Line no 1'
  write(iun1,nml=namelist_s) 
  write(iun1,*) 'Line no 2'
  status = fclos(iun1)
  iun2 = 0
  status = fnom(iun2,'test_namelist','FTN+FMT+OLD',0)
  rewind(iun2)
  print *,'(fnom) iun,status =',iun2,status
  read(iun2,*) str1
  print *,'"'//trim(str1)//'"'
  s1 = ""
  s2 = ""
  s3 = ""
  read(iun2,nml=namelist_s)
  print *,'"'//trim(s1)//'" "'//trim(s2)//'" "'//trim(s3)//'"'
  read(iun2,*) str2
  print *,'"'//trim(str2)//'"'
  status = fclos(iun2)

  print *,'========== testing WA functions/subroutines  =========='
  iun = 0
  status = fnom(iun,'/tmp/Scrap','RND+WA',0)
  print *,'(fnom) iun,status =',iun,status
  if(status .ne. 0) goto 777                     ! error
  call waopen(iun)
  array1 = array0
  call wawrit(iun,array1,1,size(array0))
  call waread(iun,array2,1,size(array0))
  if(any(array1 .ne. array2)) then
    print *,'did not read back what was written'
    goto 777                                     ! error
  else
    print *,'read back what was written',array2(1),array2(size(array0))
  endif

  array1 = array0 + 512
  ladr = 513
  w64 = wawrit64(iun,array1,ladr,size(array0),1)
  print *,'error expected, w64 =',w64
  if(w64 .ne. -1) goto 777

  w64 = wawrit64(iun,array1,ladr,size(array0),0)
  print *,'OK expected, w64 =',w64
  if(w64 .ne. size(array0)) goto 777

  ladr = 129
  r64 = waread64(iun,array2,ladr,size(array0),1)
  print *,'error expected, r64 =',r64
  if(r64 .ne. -1) goto 777

  r64 = waread64(iun,array2,ladr,size(array0),0)
  print *,'OK expected, r64 =',r64
  if(any(array0+128 .ne. array2)) then
    print *,'did not read back what was written'
  else
    print *,'read back what was written',array2(1),array2(size(array0))
  endif
  if(r64 .ne. size(array0)) goto 777
  call waclos(iun)
#if defined(NOT_LEAN)
  print *,'========== testing DA functions/subroutines  =========='
  call openda(iun)
  array2 = -1
  array1 = array0 + 1024 + 512
  call writda(iun,array1,2,4)
  call checda(iun)
  call readda(iun,array2,2,3)
  call checda(iun)
  if(any(array0+1024 .ne. array2)) then
    print *,'did not read back what was written'
    goto 777
  else
    print *,'read back what was written',array2(1),array2(size(array0))
  endif
  call closda(iun)

  call waopen(iun)
  ladr = 1385
  r64 = waread64(iun,array2,ladr,size(array0),0)
  print *,'r64 =',r64
  if(any(array0+1384 .ne. array2)) then
    print *,'did not read back what was written'
    goto 777
  else
    print *,'read back what was written',array2(1),array2(size(array0))
  endif
  call waclos(iun)
  status = fclos(iun)
  print *,'(waclos+fclos) iun,status =',iun,status
  if(status .ne. 0) goto 777                     ! error
#endif
  print *,'========== testing D77 functionality  =========='
  iund77 = 0
  status = fnom(iund77,'/tmp/Scrap','D77+UNF',10)
  print *,'(fnom) iun,D77 status =',iund77,status
  darray = -1
  read(iund77,rec=2)darray(2:11)
  print *,'expecting -1, 11 to 20, -1, BIG ENDIAN order'
  print 11,darray(1:12)
11 format(12I10)
  status = fclos(iund77)
  print *,'(fclos) iun,D77 status =',iund77,status
  if(darray(2) == ishft(11,24)) then
    print 12,'ENDIAN ORDER ERROR reading D77, expecting 11, got',darray(2)
12 format(A,Z10.8)
    goto 777
  endif
  if(darray(2) .ne. 11) then
    print 12,'ERROR reading D77, expecting 11, got',darray(2)
    goto 777
  endif
  print *,'========== testing Fortran formatted IO  =========='
  print *,'========== writing =========='
  iun1 = 0
  status = fnom(iun1,'/tmp/Scrap1','FTN+FMT',0)
  print *,'(fnom) iun SEQ,status =',iun1,status
  write(iun1,100)"0123456789"
  write(iun1,100)"abcdefghij"
100 format(A10)
  status = fclos(iun1)
  print *,'(fclos) iun,SEQ status =',iun,status
  print *,'========== reading back =========='
  iun1 = 0
  status = fnom(iun1,'/tmp/Scrap1','FTN+FMT+OLD',0)
  print *,'(fnom) iun SEQ,status =',iun1,status
  str1 = ""
  str2 = ""
  read(iun1,100) str1(1:10)
  read(iun1,100) str2(1:10)
  print *,' read back '//trim(str1)//" "//trim(str2)
  if(trim(str1) .ne. "0123456789" .or. trim(str2) .ne. "abcdefghij") goto 777
  status = fclos(iun1)
  print *,'(fclos) iun,SEQ status =',iun,status
  print *,'========== testing Fortran unformatted IO  =========='
  print *,'========== writing =========='
  iun2 = 0
  status = fnom(iun2,'/tmp/Scrap2','FTN+UNF',0)
  print *,'(fnom) iun SEQ UNF,status =',iun2,status
  write(iun2)"01234567"
  write(iun2)"abcdefgh"
  status = fclos(iun2)
  print *,'(fclos) iun,SEQ UNF status =',iun2,status
  print *,'========== reading back =========='
  iun2 = 0
  status = fnom(iun2,'/tmp/Scrap2','FTN+UNF+OLD',0)
  print *,'(fnom) iun SEQ UNF,status =',iun2,status
  str1 = ""
  str2 = ""
  read(iun2)str1(1:8)
  read(iun2)str2(1:8)
  print *,' read back '//trim(str1)//" "//trim(str2)
  if(trim(str1) .ne. "01234567" .or. trim(str2) .ne. "abcdefgh") goto 777
  status = fclos(iun2)
  print *,'(fclos) iun,SEQ UNF status =',iun2,status
  goto 888
777 continue
  print *,'ERROR IN TEST'
888 continue
end program
