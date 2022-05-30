
module iso_c_binding_extras
  use ISO_C_BINDING
  implicit none
#define MODULE_FUNCTION module function
#include <iso_c_binding_extras.hf>

end module

submodule (iso_c_binding_extras) iso_c_binding_extras_1
contains
  module procedure f_c_strlen2
    implicit none
    strlen = f_c_strlen(str//achar(0))
  end procedure f_c_strlen2

  module procedure f_c_strnlen2
    implicit none
    strlen = f_c_strnlen(str//achar(0), maxlen)
  end procedure f_c_strnlen2
end submodule iso_c_binding_extras_1

! make this procedure available even outside of the module
function f_c_strlen2(str) result(strlen)
  use ISO_C_BINDING
  implicit none
  character(len=*), intent(IN) :: str
  integer(C_SIZE_T) :: strlen
  interface
  function f_c_strlen(str) result(length) bind(C,name='strlen')
    import C_CHAR, C_SIZE_T
    character(C_CHAR), dimension(*), intent(IN) :: str
    integer(C_SIZE_T) :: length
  end function f_c_strlen
  end interface
  strlen = f_c_strlen(str//achar(0))
end function f_c_strlen2

! make this procedure available even outside of the module
function f_c_strnlen2(str, maxlen) result(strlen)
  use ISO_C_BINDING
  implicit none
  character(len=*), intent(IN) :: str
  integer(C_SIZE_T), intent(IN) :: maxlen
  integer(C_SIZE_T) :: strlen
  interface
  function f_c_strnlen(str, maxlen) result(strlen) bind(C, name='strnlen')
    import :: C_CHAR, C_SIZE_T
    implicit none
    character(C_CHAR), dimension(*), intent(IN) :: str
    integer(C_SIZE_T), intent(IN) :: maxlen
    integer(C_SIZE_T) :: strlen
  end function f_c_strnlen
  end interface
  strlen = f_c_strnlen(str//achar(0), maxlen)
end function f_c_strnlen2

#if defined(SELF_TEST)
program self_test
  call test_001
  call test_002
end program

subroutine test_002
  use ISO_C_BINDING
  implicit NONE
! test straight include
#undef MODULE_FUNCTION
#include <iso_c_binding_extras.hf>
  character(len=10) :: c10
  integer(C_SIZE_T) :: one = 1

  c10 = ' ' 
  if(c_strlen(c10) == 10) then
    print *,'test_002, C10 SUCCESS (strlen)'
  else
    print *,'test_002, c_strlen(c10) =', c_strlen(c10),' should be 10'
  endif
  print *,'test_002, c10(5:5) = C_NULL_CHAR'
  c10(5:5) = C_NULL_CHAR  ! C length is now 4
  if(c_strlen(c10) == 4) then
    print *,'test_002, C10 SUCCESS (strlen)'
  else
    print *,'test_002, c_strlen(c10) =', c_strlen(c10),' should be 4'
  endif
end subroutine test_002

subroutine test_001
! test using module
  use iso_c_binding_extras
  implicit NONE
  character(len=10) :: c10
  character(C_CHAR), dimension(12), target :: c12  ! target attribute needed for gfortran
  type(shmid_ds) :: shmid
  type(C_PTR) :: pc10, pc12
  integer(C_SIZE_T) :: one = 1

  print *,'shmid =', shmid%x
  pc10 = TRANSFER(LOC(C10), C_NULL_PTR)
  c10 = ' ' 
  if(c_strlen(c10) == 10) then
    print *,'C10 SUCCESS (strlen)'
  else
    print *,'c_strlen(c10) =', c_strlen(c10),' should be 10'
  endif
  if(c_strnlen(c10, len(c10)*ONE) == 10) then
    print *,'C10 SUCCESS (strnlen)'
  else
    print *,'c_strnlen(c10, len(c10)*ONE) =', c_strnlen(c10, len(c10)*ONE),' should be 10'
  endif
  if(c_strlen(pc10) == 10) then
    print *,'PC10 SUCCESS (strlen)'
  else
    print *,'c_strlen(c10) =', c_strlen(pc10),' should be 10'
  endif

  print *,'c10(5:5) = C_NULL_CHAR'
  c10(5:5) = C_NULL_CHAR  ! C length is now 4
  if(c_strlen(c10) == 4) then
    print *,'C10 SUCCESS (strlen)'
  else
    print *,'c_strlen(c10) =', c_strlen(c10),' should be 4'
  endif
  if(c_strlen(pc10) == 4) then
    print *,'PC10 SUCCESS (strlen)'
  else
    print *,'c_strlen(pc10) =', c_strlen(pc10),' should be 4'
  endif
  if(c_strnlen(pc10, len(c10)*ONE) == 4) then
    print *,'PC10 SUCCESS (strnlen)'
  else
    print *,'c_strnlen(pc10, len(c10)*ONE) =', c_strnlen(pc10, len(c10)*ONE),' should be 4'
  endif

  pc12 = C_LOC(C12(1))
  C12 = spread(' ', 1, size(C12))
  C12(12) = C_NULL_CHAR
  if(c_strlen(c12) == 11) then
    print *,'C12 SUCCESS (strlen)'
  else
    print *,'c_strlen(c12) =', c_strlen(c12),' should be 11'
  endif
  if(c_strlen(pc12) == 11) then
    print *,'PC12 SUCCESS (strlen)'
  else
    print *,'c_strlen(pc12) =', c_strlen(pc12),' should be 11'
  endif
  ! multiply by ONE to force correct integer size
  if(c_strnlen(pc12, size(c12)*ONE) == 11) then
    print *,'PC12 SUCCESS (strnlen)'
  else
    print *,'c_strnlen(pc12, size(c12)*ONE) =', c_strnlen(pc12, size(c12)*ONE),' should be 11'
  endif
end subroutine
#endif

