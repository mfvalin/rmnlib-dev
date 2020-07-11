program test
  use ISO_C_BINDING
  implicit none
#include <iso_c_binding_extras.hf>
#include <librmn_interface.hf>
  integer, external :: wawrit64, waread64
  integer :: iun, status, i, r64, w64, iund77, iun1, iun2
  integer*8 :: ladr, lsize
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
  print *,'========== basic IO test, c_baseio + f_baseio =========='
  call test_c_fnom()

  print *,'==========testing with Fortran file 99 already open =========='
  iun1 = 99
  open(unit=99,STATUS='SCRATCH',err=777)
  print *,'       an error is expected'
  status = fnom(iun1,'already_open','FTN+FMT',0)
  if(status .ne. -1) goto 777
  print *,'PASSED'

  print *,'========== case conversion if no lower case character =========='
  iun1 = 21
  status = fnom(iun1,'ABCDEF','FTN+FMT',0)
  if(status .ne. 0) goto 777                     ! error
  status = fclos(iun1)
  if(status .ne. 0) goto 777                     ! error
  if(c_existe('abcdef'//achar(0)) == 0) goto 777

  status = fnom(iun1,'./ABCDEF.01+','FTN+FMT',0)
  if(status .ne. 0) goto 777                     ! error
  status = fclos(iun1)
  if(status .ne. 0) goto 777                     ! error
  if(c_existe('abcdef.01+'//achar(0)) == 0) goto 777

  status = fnom(iun1,'./ABCDEF_01','FTN+FMT',0)
  if(status .ne. 0) goto 777                     ! error
  status = fclos(iun1)
  if(status .ne. 0) goto 777                     ! error
  if(c_existe('abcdef_01'//achar(0)) == 0) goto 777

  status = fnom(iun1,'ABCDEF_02','FTN+FMT',0)
  if(status .ne. 0) goto 777                     ! error
  status = fclos(iun1)
  if(status .ne. 0) goto 777                     ! error
  if(c_existe('abcdef_02'//achar(0)) == 0) goto 777

  status = fnom(iun1,'aBCDEF','FTN+FMT',0)
  if(status .ne. 0) goto 777                     ! error
  status = fclos(iun1)
  if(status .ne. 0) goto 777                     ! error
  if(c_existe('aBCDEF'//achar(0)) == 0) goto 777

  status = fnom(iun1,'ABCDEf','FTN+FMT',0)
  if(status .ne. 0) goto 777                     ! error
  status = fclos(iun1)
  if(status .ne. 0) goto 777                     ! error
  if(c_existe('ABCDEf'//achar(0)) == 0) goto 777

  print *,'PASSED'

  print *,'========== testing file removal  =========='

  if(c_unlink('abcdef'//achar(0)) /= 0) goto 777
  if(c_existe('abcdef'//achar(0)) == 1) goto 777
  if(c_unlink('abcdef.01+'//achar(0)) /= 0) goto 777
  if(c_existe('abcdef.01+'//achar(0)) == 1) goto 777
  if(c_unlink('abcdef_01'//achar(0)) /= 0) goto 777
  if(c_existe('abcdef_01'//achar(0)) == 1) goto 777
  if(c_unlink('abcdef_02'//achar(0)) /= 0) goto 777
  if(c_existe('abcdef_02'//achar(0)) == 1) goto 777
  if(c_unlink('aBCDEF'//achar(0)) /= 0) goto 777
  if(c_existe('aBCDEF'//achar(0)) == 1) goto 777
  if(c_unlink('ABCDEf'//achar(0)) /= 0) goto 777
  if(c_existe('ABCDEf'//achar(0)) == 1) goto 777

  print *,'PASSED'

  print *,'========== testing formatted I/O + namelist  =========='
  iun1 = 0
  status = fnom(iun1,'test_namelist','FTN+FMT',0)
  if(iun1 .ne. 98 .or. status .ne. 0) goto 777
  write(iun1,*) 'Line no 1'
  write(iun1,nml=namelist_s) 
  write(iun1,*) 'Line no 2'
  status = fclos(iun1)
  if(status .ne. 0) goto 777
  iun2 = 0
  status = fnom(iun2,'test_namelist','FTN+FMT+OLD',0)
  if(status .ne. 0) goto 777
  rewind(iun2)
  read(iun2,*) str1
  if( trim(str1) .ne. 'Line no 1' ) goto 777
  s1 = ""
  s2 = ""
  s3 = ""
  read(iun2,nml=namelist_s)
  if( trim(s1) .ne. 'string no 1' ) goto 777
  if( trim(s2) .ne. 'string no 2' ) goto 777
  if( trim(s3) .ne. 'string no 3' ) goto 777
  read(iun2,*) str2
  if( trim(str2) .ne. 'Line no 2' ) goto 777
  status = fclos(iun2)
  if(c_unlink('test_namelist'//achar(0)) /= 0) goto 777
  if(c_existe('test_namelist'//achar(0)) == 1) goto 777
  print *,'PASSED'

  print *,'========== testing WA functions/subroutines  =========='
  iun = 0
  status = fnom(iun,'/tmp/Scrap','RND+WA',0)
  if(status .ne. 0) goto 777                     ! error
  call waopen(iun)
  array1 = array0
  call wawrit(iun,array1,1,size(array0))
  call waread(iun,array2,1,size(array0))
  if(any(array1 .ne. array2)) then
    print *,'failed to read back what was written (1)'
    goto 777                                     ! error
  endif

  array1 = array0 + 512
  ladr = 513
  print *,'       an error is expected'
  w64 = wawrit64(iun,array1,ladr,size(array0),1)
  if(w64 .ne. -1) goto 777

  w64 = wawrit64(iun,array1,ladr,size(array0),0)
  if(w64 .ne. size(array0)) goto 777

  ladr = 129
  print *,'       an error is expected'
  r64 = waread64(iun,array2,ladr,size(array0),1)
  if(r64 .ne. -1) goto 777

  r64 = waread64(iun,array2,ladr,size(array0),0)
  if(r64 .ne. size(array0)) goto 777
  if(any(array0+128 .ne. array2)) then
    print *,'failed to read back what was written (2)'
    goto 777
  endif
  if(r64 .ne. size(array0)) goto 777
  call waclos(iun)
! TODO: add error test writing too far beyond file end, when not in sparse mode
  print *,'PASSED'

  print *,'========== testing WA64 in sparse mode  =========='
  iun = 0
  status = fnom(iun,'/tmp/Sparse','RND+WA+SPARSE',0)
  if(status .ne. 0) goto 777                     ! error
  call waopen(iun)
  array1 = array0
  array2 = 0
  ladr   = 16000000000_8
  w64 = wawrit64(iun,array1,ladr,size(array0),0)
  r64 = waread64(iun,array2,ladr,size(array0),0)
  if(any(array1 .ne. array2)) then
    print *,'failed to read back what was written (1)'
    goto 777                                     ! error
  endif
  call waclos(iun)
  call waopen(iun)
  lsize = numblks(iun)
  ladr = (ladr + size(array0) -1)
  ladr = (ladr + 511)/512
  if( lsize .ne. ladr ) goto 777
  call waclos(iun)
  if(c_unlink('/tmp/Sparse'//achar(0)) /= 0) goto 777
  if(c_existe('/tmp/Sparse'//achar(0)) == 1) goto 777
  print *,'PASSED'

  print *,'========== testing DA functions/subroutines  =========='
  iun = 0
  status = fnom(iun,'/tmp/Scrap','RND+WA',0)     ! use file written for the WA test
  if(status .ne. 0) goto 777                     ! error
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
  endif
  call closda(iun)

  call waopen(iun)
  ladr = 1385
  r64 = waread64(iun,array2,ladr,size(array0),0)
  if(r64 .ne. size(array0)) goto 777
  if(any(array0+1384 .ne. array2)) then
    print *,'did not read back what was written'
    goto 777
  endif
  call waclos(iun)
  status = fclos(iun)
  if(status .ne. 0) goto 777                     ! error
  print *,'PASSED'

  print *,'========== testing D77 functionality  =========='
  iund77 = 0
  status = fnom(iund77,'/tmp/Scrap','D77+UNF',10)
  if(status .ne. 0) goto 777                     ! error
  darray = -1
  read(iund77,rec=2)darray(2:11)
  if(darray(2) == ishft(11,24)) then
    print 12,'ENDIAN ORDER ERROR reading D77, expecting 11, got',darray(2)
12 format(A,Z10.8)
    goto 777
  endif
  if( any(darray(1:12) .ne. [-1,11,12,13,14,15,16,17,18,19,20,-1]) ) then
    print *,'expecting -1, 11 to 20, -1, BIG ENDIAN order'
    print 11,'got: ',darray(1:12)
    goto 777
  endif
11 format(A,12I10)
  status = fclos(iund77)
  if(status .ne. 0) goto 777                     ! error
  if(c_unlink('/tmp/Scrap'//achar(0)) /= 0) goto 777
  if(c_existe('/tmp/Scrap'//achar(0)) == 1) goto 777
  print *,'PASSED'

  print *,'========== testing Fortran formatted IO  =========='
!   print *,'==========   writing    =========='
  iun1 = 0
  status = fnom(iun1,'/tmp/Scrap1','FTN+FMT',0)
  if(status .ne. 0) goto 777                     ! error
  write(iun1,100)"0123456789"
  write(iun1,100)"abcdefghij"
100 format(A10)
  status = fclos(iun1)
  if(status .ne. 0) goto 777                     ! error
!   print *,'========== reading back =========='
  iun1 = 0
  status = fnom(iun1,'/tmp/Scrap1','FTN+FMT+OLD',0)
  if(status .ne. 0) goto 777                     ! error
  str1 = ""
  str2 = ""
  read(iun1,100) str1(1:10)
  read(iun1,100) str2(1:10)
  if(trim(str1) .ne. "0123456789" .or. trim(str2) .ne. "abcdefghij") goto 777
  status = fclos(iun1)
  if(status .ne. 0) goto 777                     ! error
  if(c_unlink('/tmp/Scrap1'//achar(0)) /= 0) goto 777
  if(c_existe('/tmp/Scrap1'//achar(0)) == 1) goto 777
  print *,'PASSED'

  print *,'========== testing Fortran unformatted IO  =========='
!   print *,'==========   writing    =========='
  iun2 = 0
  status = fnom(iun2,'/tmp/Scrap2','FTN+UNF',0)
  if(status .ne. 0) goto 777                     ! error
  write(iun2)"01234567"
  write(iun2)"abcdefgh"
  status = fclos(iun2)
  if(status .ne. 0) goto 777                     ! error
!   print *,'========== reading back =========='
  iun2 = 0
  status = fnom(iun2,'/tmp/Scrap2','FTN+UNF+OLD',0)
  if(status .ne. 0) goto 777                     ! error
  str1 = ""
  str2 = ""
  read(iun2)str1(1:8)
  read(iun2)str2(1:8)
  if(trim(str1) .ne. "01234567" .or. trim(str2) .ne. "abcdefgh") goto 777
  status = fclos(iun2)
  if(status .ne. 0) goto 777                     ! error
  if(c_unlink('/tmp/Scrap2'//achar(0)) /= 0) goto 777
  if(c_existe('/tmp/Scrap2'//achar(0)) == 1) goto 777
  print *,'PASSED'

  goto 888
777 continue
  print *,'ERROR IN TEST'
888 continue
end program
