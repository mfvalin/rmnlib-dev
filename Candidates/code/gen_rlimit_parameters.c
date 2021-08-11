#include <stdio.h>
#include <sys/time.h>
#include <sys/resource.h>

int main(int argc, char **argv){
  printf("module rlimits_mod\n  use ISO_C_BINDING\n  implicit none\n");
  printf("  type, bind(C) :: rlimit\n") ;
  if( sizeof(rlim_t) == 4) {
    printf("    integer(C_INT32_T) :: rlim_cur\n    integer(C_INT32_T) :: rlim_max\n") ;
  }else{
    printf("    integer(C_INT64_T) :: rlim_cur\n    integer(C_INT64_T) :: rlim_max\n") ;
  }
  printf("  end type\n");
  printf("  integer, parameter :: RLIM_INFINITY = %ld\n",RLIM_INFINITY);
  printf("  integer, parameter :: RLIMIT_AS = %d\n",RLIMIT_AS);
  printf("  integer, parameter :: RLIMIT_CORE = %d\n",RLIMIT_CORE);
  printf("  integer, parameter :: RLIMIT_CPU = %d\n",RLIMIT_CPU);
  printf("  integer, parameter :: RLIMIT_DATA = %d\n",RLIMIT_DATA);
  printf("  integer, parameter :: RLIMIT_FSIZE = %d\n",RLIMIT_FSIZE);
  printf("  integer, parameter :: RLIMIT_LOCKS = %d\n",RLIMIT_LOCKS);
  printf("  integer, parameter :: RLIMIT_MEMLOCK = %d\n",RLIMIT_MEMLOCK);
  printf("  integer, parameter :: RLIMIT_MSGQUEUE = %d\n",RLIMIT_MSGQUEUE);
  printf("  integer, parameter :: RLIMIT_NICE = %d\n",RLIMIT_NICE);
  printf("  integer, parameter :: RLIMIT_NOFILE = %d\n",RLIMIT_NOFILE);
  printf("  integer, parameter :: RLIMIT_NPROC = %d\n",RLIMIT_NPROC);
  printf("  integer, parameter :: RLIMIT_RSS = %d\n",RLIMIT_RSS);
  printf("  integer, parameter :: RLIMIT_RTPRIO = %d\n",RLIMIT_RTPRIO);
  printf("  integer, parameter :: RLIMIT_RTTIME = %d\n",RLIMIT_RTTIME);
  printf("  integer, parameter :: RLIMIT_SIGPENDING = %d\n",RLIMIT_SIGPENDING);
  printf("  integer, parameter :: RLIMIT_STACK = %d\n",RLIMIT_STACK);
  printf("  interface\n") ;
  printf("    function getrlimit(resource, limit) result(status) bind(C,name='getrlimit')\n") ;
  printf("      import :: C_INT, rlimit\n") ;
  printf("      implicit none\n") ;
  printf("      integer(C_INT), intent(IN), value :: resource\n") ;
  printf("      type(rlimit), intent(OUT) :: limit\n") ;
  printf("      integer(C_INT) :: status\n") ;
  printf("    end function getrlimit\n") ;
  printf("    function setrlimit(resource, limit) result(status) bind(C,name='setrlimit')\n") ;
  printf("      import :: C_INT, rlimit\n") ;
  printf("      implicit none\n") ;
  printf("      integer(C_INT), intent(IN), value :: resource\n") ;
  printf("      type(rlimit), intent(IN) :: limit\n") ;
  printf("      integer(C_INT) :: status\n") ;
  printf("    end function setrlimit\n") ;
  printf("  end interface\n") ;
  printf("  end module \n");
}
