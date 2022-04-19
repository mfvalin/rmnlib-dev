program test
  use ISO_C_BINDING
  implicit none
#include <iso_c_binding_extras.hf>
#include <librmn_interface.hf>
  type(C_PTR)     :: stdout
  type(C_FILEPTR) :: stdoutf
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
  external :: message_at_exit
  type(C_PTR) :: dlhandle
  type(C_FUNPTR)       :: demo1,  demo1b,  demo2,  demo2b

! NOTE: BIND(C) necessity discovered with Intel oneapi compiler for functions with value arguments
! test from shared library failed if BIND(C) not present in proc1 and proc2
! no problem noticed with gfortran / pgfortran / flang(pgi derived variety)
  interface
    function proc1b(a) result(r) BIND(C)
      integer, intent(IN) :: a
      integer :: r
    end function proc1b
    function proc1(a) result(r) BIND(C)
      integer, intent(IN), value :: a
      integer :: r
    end function proc1
    function proc2b(a, b) result(r) BIND(C)
      integer, intent(IN) :: a
      real, intent(IN) :: b
      real :: r
    end function proc2b
    function proc2(a, b) result(r) BIND(C)
      integer, intent(IN), value :: a
      real, intent(IN), value :: b
      real :: r
    end function proc2
  end interface
  procedure(proc1b), pointer :: fdemo1b
  procedure(proc1) :: fproc1
  procedure(proc1), pointer :: fdemo1, pproc1
  procedure(proc2b), pointer :: fdemo2b
  procedure(proc2), pointer :: fdemo2

  integer :: f1
  real :: f2
  integer, external :: test_shm
!
! using macro Cstr() to transform a Fortran string into a C null terminated string
!
  s1 = 'string no 1'
  s2 = 'string no 2'
  s3 = 'string no 3'
  do i=1,size(array0)
    array0(i) = i
  enddo
  write(6,*)'========== basic IO test, c_baseio + f_baseio =========='
  call test_c_fnom()
  if(c_existe(Cstr('C_file'))  == 0) goto 777
  if(c_existe(Cstr('C_file2')) == 0) goto 777
  if(c_unlink(Cstr('C_file'))  /= 0) goto 777
  if(c_unlink(Cstr('C_file2')) /= 0) goto 777
  if(c_existe(Cstr('C_file'))  == 1) goto 777
  if(c_existe(Cstr('C_file2')) == 1) goto 777

  write(6,*)'==========testing with Fortran file 99 already open =========='
  iun1 = 99
  open(unit=99,STATUS='SCRATCH',err=777)
  write(6,*)'       an error is expected'
  status = fnom(iun1,'already_open','FTN+FMT',0)
  if(status .ne. -1) goto 777
  write(6,*)'PASSED'

  write(6,*)'========== case conversion if no lower case character =========='
  iun1 = 21
  status = fnom(iun1,'ABCDEF','FTN+FMT',0)
  if(status .ne. 0) goto 777                     ! error
  status = fclos(iun1)
  if(status .ne. 0) goto 777                     ! error
  if(c_existe(Cstr('abcdef')) == 0) goto 777

  status = fnom(iun1,'./ABCDEF.01+','FTN+FMT',0)
  if(status .ne. 0) goto 777                     ! error
  status = fclos(iun1)
  if(status .ne. 0) goto 777                     ! error
  if(c_existe(Cstr('abcdef.01+')) == 0) goto 777

  status = fnom(iun1,'./ABCDEF_01','FTN+FMT',0)
  if(status .ne. 0) goto 777                     ! error
  status = fclos(iun1)
  if(status .ne. 0) goto 777                     ! error
  if(c_existe(Cstr('abcdef_01')) == 0) goto 777

  status = fnom(iun1,'ABCDEF_02','FTN+FMT',0)
  if(status .ne. 0) goto 777                     ! error
  status = fclos(iun1)
  if(status .ne. 0) goto 777                     ! error
  if(c_existe(Cstr('abcdef_02')) == 0) goto 777

  status = fnom(iun1,'aBCDEF','FTN+FMT',0)
  if(status .ne. 0) goto 777                     ! error
  status = fclos(iun1)
  if(status .ne. 0) goto 777                     ! error
  if(c_existe(Cstr('aBCDEF')) == 0) goto 777

  status = fnom(iun1,'ABCDEf','FTN+FMT',0)
  if(status .ne. 0) goto 777                     ! error
  status = fclos(iun1)
  if(status .ne. 0) goto 777                     ! error
  if(c_existe(Cstr('ABCDEf')) == 0) goto 777

  write(6,*)'PASSED'

  write(6,*)'========== testing file removal  =========='

  if(c_unlink(Cstr('abcdef'))     /= 0) goto 777
  if(c_existe(Cstr('abcdef'))     == 1) goto 777
  if(c_unlink(Cstr('abcdef.01+')) /= 0) goto 777
  if(c_existe(Cstr('abcdef.01+')) == 1) goto 777
  if(c_unlink(Cstr('abcdef_01'))  /= 0) goto 777
  if(c_existe(Cstr('abcdef_01'))  == 1) goto 777
  if(c_unlink(Cstr('abcdef_02'))  /= 0) goto 777
  if(c_existe(Cstr('abcdef_02'))  == 1) goto 777
  if(c_unlink(Cstr('aBCDEF'))     /= 0) goto 777
  if(c_existe(Cstr('aBCDEF'))     == 1) goto 777
  if(c_unlink(Cstr('ABCDEf'))     /= 0) goto 777
  if(c_existe(Cstr('ABCDEf'))     == 1) goto 777

  write(6,*)'PASSED'

  write(6,*)'========== testing formatted I/O + namelist  =========='
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
  if(c_unlink(Cstr('test_namelist')) /= 0) goto 777
  if(c_existe(Cstr('test_namelist')) == 1) goto 777
  write(6,*)'PASSED'

  write(6,*)'========== testing WA functions/subroutines  =========='
  iun = 0
  status = fnom(iun,'/tmp/Scrap','RND+WA',0)
  if(status .ne. 0) goto 777                     ! error
  call waopen(iun)
  array1 = array0
  call wawrit(iun,array1,1,size(array0))
  call waread(iun,array2,1,size(array0))
  if(any(array1 .ne. array2)) then
    write(6,*)'failed to read back what was written (1)'
    goto 777                                     ! error
  endif

  array1 = array0 + 512
  ladr = 513
  write(6,*)'       an error is expected'
  w64 = wawrit64(iun,array1,ladr,size(array0),1)
  if(w64 .ne. -1) goto 777

  w64 = wawrit64(iun,array1,ladr,size(array0),0)
  if(w64 .ne. size(array0)) goto 777

  ladr = 129
  write(6,*)'       an error is expected'
  r64 = waread64(iun,array2,ladr,size(array0),1)
  if(r64 .ne. -1) goto 777

  r64 = waread64(iun,array2,ladr,size(array0),0)
  if(r64 .ne. size(array0)) goto 777
  if(any(array0+128 .ne. array2)) then
    write(6,*)'failed to read back what was written (2)'
    goto 777
  endif
  if(r64 .ne. size(array0)) goto 777
  call waclos(iun)
! TODO: add error test writing too far beyond file end, when not in sparse mode
  write(6,*)'PASSED'

  write(6,*)'========== testing WA64 in sparse mode  =========='
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
    write(6,*)'failed to read back what was written (1)'
    goto 777                                     ! error
  endif
  call waclos(iun)
  call waopen(iun)
  lsize = numblks(iun)
  ladr = (ladr + size(array0) -1)
  ladr = (ladr + 511)/512
  if( lsize .ne. ladr ) goto 777
  call waclos(iun)
  if(c_unlink(Cstr('/tmp/Sparse')) /= 0) goto 777
  if(c_existe(Cstr('/tmp/Sparse')) == 1) goto 777
  write(6,*)'PASSED'

  write(6,*)'========== testing DA functions/subroutines  =========='
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
    write(6,*)'did not read back what was written'
    goto 777
  endif
  call closda(iun)

  call waopen(iun)
  ladr = 1385
  r64 = waread64(iun,array2,ladr,size(array0),0)
  if(r64 .ne. size(array0)) goto 777
  if(any(array0+1384 .ne. array2)) then
    write(6,*)'did not read back what was written'
    goto 777
  endif
  call waclos(iun)
  status = fclos(iun)
  if(status .ne. 0) goto 777                     ! error
  write(6,*)'PASSED'

  write(6,*)'========== testing D77 functionality  =========='
  iund77 = 0
  status = fnom(iund77,'/tmp/Scrap','D77+UNF',10)
  if(status .ne. 0) goto 777                     ! error
  darray = -1
  read(iund77,rec=2)darray(2:11)
  if(darray(2) == ishft(11,24)) then
    write(6, 12)'ENDIAN ORDER ERROR reading D77, expecting 11, got',darray(2)
12 format(A,Z10.8)
    goto 777
  endif
  if( any(darray(1:12) .ne. [-1,11,12,13,14,15,16,17,18,19,20,-1]) ) then
    write(6,*)'expecting -1, 11 to 20, -1, BIG ENDIAN order'
    write(6, 11)'got: ',darray(1:12)
    goto 777
  endif
11 format(A,12I10)
  status = fclos(iund77)
  if(status .ne. 0) goto 777                     ! error
  if(c_unlink(Cstr('/tmp/Scrap')) /= 0) goto 777
  if(c_existe(Cstr('/tmp/Scrap')) == 1) goto 777
  write(6,*)'PASSED'

  write(6,*)'========== testing Fortran formatted IO  =========='
!   write(6,*)'==========   writing    =========='
  iun1 = 0
  status = fnom(iun1,'/tmp/Scrap1','FTN+FMT',0)
  if(status .ne. 0) goto 777                     ! error
  write(iun1,100)"0123456789"
  write(iun1,100)"abcdefghij"
100 format(A10)
  status = fclos(iun1)
  if(status .ne. 0) goto 777                     ! error
!   write(6,*)'========== reading back =========='
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
  if(c_unlink(Cstr('/tmp/Scrap1')) /= 0) goto 777
  if(c_existe(Cstr('/tmp/Scrap1')) == 1) goto 777
  write(6,*)'PASSED'

  write(6,*)'========== testing Fortran unformatted IO  =========='
!   write(6,*)'==========   writing    =========='
  iun2 = 0
  status = fnom(iun2,'/tmp/Scrap2','FTN+UNF',0)
  if(status .ne. 0) goto 777                     ! error
  write(iun2)"01234567"
  write(iun2)"abcdefgh"
  status = fclos(iun2)
  if(status .ne. 0) goto 777                     ! error
!   write(6,*)'========== reading back =========='
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
  if(c_unlink(Cstr('/tmp/Scrap2')) /= 0) goto 777
  if(c_existe(Cstr('/tmp/Scrap2')) == 1) goto 777
  write(6,*)'PASSED'
  call flush(6)

  write(6,*)'========== testing function pointers  =========='
  f1 = fproc1(1234)
  write(6,*)'fproc1(1234) = ',f1
  fdemo1 => fproc1
  f1 = fdemo1(1234)
  write(6,*)'fdemo1(1234) = ',f1
  demo1 = C_FUNLOC(fproc1)
  call C_F_PROCPOINTER(demo1,fdemo1)
  f1 = fdemo1(1234)
  if(f1 == 1234) then
    write(6,*)'fdemo1(1234) == ',f1
  else
    write(6,*)'fdemo1(1234) <> ',f1
  endif

  write(6,*)'========== testing interface to shared library  =========='
  dlhandle = c_dlopen(Cstr('./libdemo.so'), RTLD_LAZY)
  if(C_ASSOCIATED(dlhandle)) then
    demo1b = c_dlsym(dlhandle, Cstr('Demo1b'))
    if(C_ASSOCIATED(demo1b)) then
      call C_F_PROCPOINTER(demo1b,fdemo1b)
      f1 = fdemo1b(1234)
      if(f1 .ne. 1234) goto 777
    else
      write(6,*)'entry Demo1b not found'
      goto 777
    endif
    demo1 = c_dlsym(dlhandle, Cstr('Demo1'))
    if(C_ASSOCIATED(demo1)) then
      call C_F_PROCPOINTER(demo1,fdemo1)
      f1 = fdemo1(1234)
      if(f1 .ne. 1234) then
        write(6,*)'entry Demo1 execution error, f1 .ne. 1234'
!         goto 777
      endif
    else
      write(6,*)'entry Demo1 not found'
      goto 777
    endif
    demo2b = c_dlsym(dlhandle, Cstr('Demo2b'))
    if(C_ASSOCIATED(demo2b)) then
      call C_F_PROCPOINTER(demo2b,fdemo2b)
      f2 = fdemo2b(5678, 12.23)
      if(f2 .ne. 12.23+5678) then
        write(6,*)'entry Demo2b execution error'
!         goto 777
      endif
    else
      write(6,*)'entry Demo2b not found'
      goto 777
    endif
    demo2 = c_dlsym(dlhandle, Cstr('Demo2'))
    if(C_ASSOCIATED(demo2)) then
      call C_F_PROCPOINTER(demo2,fdemo2)
      f2 = fdemo2(5678, 12.23)
      if(f2 .ne. 12.23+5678) then
        write(6,*)'entry Demo2 execution error, f2 .ne. 12.23+5678'
!         goto 777
      endif
    else
      write(6,*)'entry Demo2 not found'
      goto 777
    endif
    status = c_dlclose(dlhandle)
    if(status .ne. 0) goto 777
    write(6,*)'PASSED'
  else
    write(6,*)'libdemo.so not found'
    write(6,*)'SKIPPED'
  endif

  write(6,*)'========== testing stdout redirection  =========='
  stdoutf = C_FILEPTR( C_STDOUT() )
  stdout  = c_freopen(Cstr('./my_stdout'), Cstr('w+'), stdoutf)
  write(6,*)'this should not appear on screen but in file my_stdout'
  call flush(6)
  call system(" echo 'TEST: listing contents of ./my_stdout' 1>&2")
  call system("cat ./my_stdout 1>&2")
  if(c_existe(Cstr('./my_stdout')) == 0) goto 777
  if(c_unlink(Cstr('./my_stdout')) /= 0) goto 777
  goto 888
777 continue
  write(0,*)'ERROR(S) IN TEST'
  call c_exit(13)
888 continue
  if( test_shm() .ne. 0) goto 777
  write(0,*)'PASSED'

  write(0,*)'========== testing atexit functionality  =========='
  status = c_atexit(C_FUNLOC(message_at_exit))
  call c_exit(0)
end program

subroutine message_at_exit()
  write(0,*)'SUCCESSFUL END OF TEST (atexit)'
end subroutine message_at_exit

integer function test_shm()
  use ISO_C_BINDING
  implicit none
#include <iso_c_binding_extras.hf>
#include <librmn_interface.hf>

  type(shmid_ds) :: shmidds
  integer(C_INT) :: shmemid, dummy
  integer(C_SIZE_T) :: shmemsiz
  type(C_PTR)    :: memadr
  integer(C_INTPTR_T) :: memint
  integer(C_INT8_T), dimension(:), pointer :: array

  write(0,*)'========== testing shared memory interface  =========='
  shmemsiz= 1024*1024*1024   ! 1GBytes
  shmemid = c_shmget(IPC_PRIVATE, shmemsiz, IPC_CREAT+S_IRUSR+S_IWUSR)
  write(0,*) 'shared memory segment id =',shmemid
  memadr  = c_shmat(shmemid, C_NULL_PTR, 0)
  call C_F_POINTER(memadr, array, [shmemsiz])
  memint  = transfer(memadr, memint)
  write(0,'(A,Z16.16)') ' attached at ',memint
  test_shm = c_shmctl(shmemid, IPC_RMID, shmidds)
  if(test_shm .ne. 0) return
  write(0,*) 'before initializing array, PLS input an integer value'
  read(5,*) dummy
  array = 1
  write(0,*) 'after initializing array, PLS input an integer value'
  read(5,*) dummy
  test_shm = c_shmdt(memadr)
  return
end function test_shm

function fproc1(a) result(r) BIND(C)
  integer, intent(IN), value :: a
  integer :: r
  print *,'fproc1 : a =',a
  r = a
end function fproc1
