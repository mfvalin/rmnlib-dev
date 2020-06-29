program test_hf
  use ISO_C_BINDING
  implicit none
#include <iso_c_binding_extras.hf>
  type(timeval)          :: tv1
  type(timeval)          :: tv2
  type(timezone)         :: tz1
  type(timespec)         :: ts1, ts2
  integer(C_INT) :: stamp
  integer, dimension(100), target :: iarr, ibrr
  integer :: int1, fd, ok, i, microsecs
  integer(C_SSIZE_T) :: nbo
  integer(C_SIZE_T)  :: nbi, nelem, selem
  type(C_FILEPTR) :: file
  integer(C_CLOCK_T) :: c1, c2
  integer(C_OFF_T) :: off0, off1
  integer(C_LONG) :: offl0, offl1
  type(C_PTR) :: cptr
  character(C_CHAR), dimension(1024), target :: cstr
  character(len=128) :: s1, s2, s3

  ok    = c_gettimeofday(tv1, C_NULL_PTR)
  c1    = c_clock()
  c2    = c_sleep(1)
  int1  = c_gethostid()
  int1  = c_sched_getcpu()
  print*,'cpu =',int1
  int1  = c_numa_node_of_cpu(int1)
  print*,'numa node =',int1
  int1  = c_alarm(123)
  int1  = c_mkdir(Cstr('tagada'), S_IRWXU+S_IRGRP+S_IXGRP+S_IROTH+S_IXOTH)
  int1  = c_chdir(Cstr('tagada'))
  ok    = c_system(Cstr('pwd'))

  do i = 1, 100
    iarr = i
  enddo
  nbi = 200
  fd    = c_open(Cstr('DataFile_001'), O_RDWR+O_CREAT, S_IRWXU+S_IRGRP+S_IXGRP+S_IROTH+S_IXOTH)
  nbo   = c_write(fd, C_LoC(iarr) , nbi)
  ok    = c_close(fd)

  ibrr = 0
  fd    = c_open(Cstr('DataFile_001'), O_RDWR+O_CREAT, S_IRWXU+S_IRGRP+S_IXGRP+S_IROTH+S_IXOTH)
  nbo   = c_read(fd, C_LoC(ibrr) , nbi)
  if( all( iarr(1:nbo/4) == ibrr(1:nbo/4) ) ) print *,nbo,' bytes written and read back'
  off0  = nbi/2
  off1  = c_lseek(fd, off0, SEEK_SET)
  ibrr = 0
  nbo   = c_read(fd, C_LoC(ibrr) , nbi/2)
  if( all( iarr(1+nbi/8:nbi/4) == ibrr(1:nbo/4) ) ) print *,nbo,' bytes read at tail of file'
  ok    = c_close(fd)

  ok    = c_system(Cstr('ls -al'))
  ok    = c_unlink(Cstr('DataFile_001'))
  print *,'file "DataFile_001" closed and deleted'
  ok    = c_system(Cstr('ls -al'))

  selem = 4  ! writing 32 bit words
  nelem = 10
  ibrr = 0
  ! some compilers have problems returning type(C_FILEPTR)
  file  = C_FILEPTR( c_fopen(Cstr('DataFile_002'), Cstr('w+')) ) 
  if(C_ASSOCIATED(file%p)) print *,'fopen OK, fd =',c_fileno(file)
  nbi   = c_fwrite(C_LoC(iarr), selem, nelem, file)
  ok    = c_fclose(file)
  ! some compilers have problems returning type(C_FILEPTR)
  file  = C_FILEPTR( c_fopen(Cstr('DataFile_002'), Cstr('r+')) ) 
  nbi   = c_fread(C_LoC(ibrr), selem, nelem, file)
  if( all( iarr(1:nbi) == ibrr(1:nbi) ) ) print *,nbi,'elements written and read back'
  offl0 = nelem*selem / 2
  ok    = c_fseek(file, offl0, SEEK_SET)
  offl1 = c_ftell(file)
  if(offl0 .ne. offl1) print *,'ERROR: wrong position'
  ibrr  = 0
  nbi   = c_fread(C_LoC(ibrr), selem, nelem/2, file)
  if( all( iarr(nbi/2:nbi) == ibrr(1:nbi) ) ) print *,nbi,' elements read at tail of file'
  ok    = c_fclose(file)

  ok    = c_system(Cstr('ls -al'))
  ok    = c_remove(Cstr('DataFile_002'))
  print *,'file "DataFile_002" closed and deleted'
  ok    = c_system(Cstr('ls -al'))

  ok    = c_chdir(Cstr('..'))
  ok    = c_rmdir(Cstr('tagada'))
  print *,'directory tagada deleted'
  ok    = c_system(Cstr('ls -al tagada'))
  print *,'command status =',ok

  ok    = c_gettimeofday(tv2, C_LoC(tz1))
  c2    = c_clock()
  microsecs = (tv2%tv_usec - tv1%tv_usec) + 1000000*(tv2%tv_sec - tv1%tv_sec)
  print *,'c_gettimeofday: wall time =',microsecs,' microseconds'
  print *,'c_clock:        CPU time  =',nint((c2-c1)/C_CLOCKS_PER_SEC*1000000),' microseconds'
  print *,'tz_minuteswest =',tz1%tz_minuteswest,', tz_dsttime =',tz1%tz_dsttime

  ok    = c_gettimeofday(tv1, C_NULL_PTR)
  ok    = c_usleep(100000)
  ok    = c_gettimeofday(tv2, C_NULL_PTR)
  microsecs = (tv2%tv_usec - tv1%tv_usec) + 1000000*(tv2%tv_sec - tv1%tv_sec)
  print *,'c_gettimeofday: wall time     =',microsecs,' microseconds'
  print *,'c_gettimeofday: usleep time   =',100000

  ok    = c_gettimeofday(tv1, C_NULL_PTR)
  ts1%tv_sec = 0
  ts1%tv_nsec = 50000000
  ts2%tv_sec = 0
  ts2%tv_nsec = 0
  ok    = c_nanosleep(ts1, C_NULL_PTR)
#if defined(WITH_POLYMORPHIC)
  ok    = c_nanosleep(ts1, ts2)
#else
  ok    = c_nanosleep(ts1, C_LoC(ts2))
#endif
  ok    = c_gettimeofday(tv2, C_NULL_PTR)
  microsecs = (tv2%tv_usec - tv1%tv_usec) + 1000000*(tv2%tv_sec - tv1%tv_sec)
  print *,'c_gettimeofday: wall time       =',microsecs,' microseconds'
  print *,'c_gettimeofday: nanosleep time  =',2*50000
  print *,'nanosleep remainder = ',ts2%tv_sec,' seconds,',ts2%tv_nsec,' nanoseconds'

  s1 = 'blah blah blah'
  cptr = C_LOC(cstr)
  call c_f_string(cptr, s1, 1024)
  call c_f_string(cptr, s2, 0)
  print *,'s2 = "'//trim(s2)//'"'

  call c_f_string(cstr, s2, 1024)
  call c_f_string(cstr, s3, 0)
  print *,'s3 = "'//trim(s3)//'"'

end program
