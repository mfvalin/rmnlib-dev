module rlimits_mod
  use ISO_C_BINDING
  implicit none
  type, bind(C) :: rlimit
    integer(C_INT64_T) :: rlim_cur
    integer(C_INT64_T) :: rlim_max
  end type
  integer, parameter :: RLIM_INFINITY = -1
  integer, parameter :: RLIMIT_AS = 9
  integer, parameter :: RLIMIT_CORE = 4
  integer, parameter :: RLIMIT_CPU = 0
  integer, parameter :: RLIMIT_DATA = 2
  integer, parameter :: RLIMIT_FSIZE = 1
  integer, parameter :: RLIMIT_LOCKS = 10
  integer, parameter :: RLIMIT_MEMLOCK = 8
  integer, parameter :: RLIMIT_MSGQUEUE = 12
  integer, parameter :: RLIMIT_NICE = 13
  integer, parameter :: RLIMIT_NOFILE = 7
  integer, parameter :: RLIMIT_NPROC = 6
  integer, parameter :: RLIMIT_RSS = 5
  integer, parameter :: RLIMIT_RTPRIO = 14
  integer, parameter :: RLIMIT_RTTIME = 15
  integer, parameter :: RLIMIT_SIGPENDING = 11
  integer, parameter :: RLIMIT_STACK = 3
  interface
    function getrlimit(resource, limit) result(status) bind(C,name='getrlimit')
      import :: C_INT, rlimit
      implicit none
      integer(C_INT), intent(IN), value :: resource
      type(rlimit), intent(OUT) :: limit
      integer(C_INT) :: status
    end function getrlimit
    function setrlimit(resource, limit) result(status) bind(C,name='setrlimit')
      import :: C_INT, rlimit
      implicit none
      integer(C_INT), intent(IN), value :: resource
      type(rlimit), intent(IN) :: limit
      integer(C_INT) :: status
    end function setrlimit
  end interface
  end module 
