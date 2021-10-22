! Copyright (C) 2021  Environnement et Changement climatique Canada
!
! This is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
!
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! Author:
!     M. Valin,   Recherche en Prevision Numerique, 2021
!
! some basic Fortran functions for string manipulation
! string format #1 (fstring) : character(len=*) blank padded character variables
! string format #2 (cstring) : character(C_CHAR), dimension(*) null termainated C compatible array
!
! 3 generic functions are defined
! FC_switch   : switch between the rtwo formats
! FC_strlen   : effective length of string (without trailing blanks or up to first NULL character)
! FC_strsize  : full size of string
!
module F_C_base_string_mod   
  use ISO_C_BINDING
  use iso_c_binding_extras, ONLY : c_strlen  ! length of a C compatible null terminated array of characters
  implicit none

  private :: c_strlen   ! do not export beyond this module

  interface FC_switch                     !  switch between the 2 formats
    module procedure Fortran_TO_C_string  ! (fstring -> cstring)
    module procedure C_TO_Fortran_string  ! (cstring -> fstring)
    module procedure C_PTR_TO_Fortran_string  ! (C_PTR -> fstring)
  end interface

  interface FC_strlen   ! number of "useful" characters in string ( <= size of string)
    module procedure string_length_null_terminated    ! uses C library strlen()
    module procedure string_length_fortran_variable   ! uses Fortran len(trim())
  end interface

  interface FC_strsize  ! size of string ( >= length of string)
    module procedure string_size_null_terminated
    module procedure string_size_fortran_variable
  end interface

  interface
    function c_strnlen(str) result(length, maxlen) bind(C,name='strnlen')
      import C_PTR, C_SIZE_T
      type(C_PTR), intent(IN), value :: str
      integer(C_SIZE_T), intent(IN), value :: maxlen
      integer(C_SIZE_T) :: length
    end function c_strlen
  end interface

  contains

  ! this functions exists soi that an argument can be called "trim"
  function len_trim(fstr) result(length)    ! length of (len=*) variable ignoring trailing spaces
    implicit none
    character(len=*), intent(IN)                :: fstr   ! Fortran character string (len=*)
    integer :: length
    length = len(trim(fstr))
  end function len_trim

  ! convert  C compatible null terminated cstring into a character(len=*) fstring
  ! optional argument lng specifies the minimum capacity of the result fstring
  ! if optional argument trim is .false., lng is ignored and the fstring will only contain characters up to the first NULL in cstring
  function C_TO_Fortran_string(c_str, lng, trim) result(f_str)
    implicit none
    character(C_CHAR), dimension(:), intent(IN) :: c_str   ! null terminated array of characters
    integer, intent(IN), optional               :: lng     ! minimum length of result fstring (ignore if trim is .true.
    logical, intent(IN), optional               :: trim    ! if true, trim result
    character(len=:), allocatable               :: f_str   ! Fortran character variable (len=*) (fstring)
    integer(C_SIZE_T) :: length, length0
    integer :: i

    length0 = c_strlen(c_str)                  ! effective cstring length (stops at first NULL character)
    length  = size(c_str) - 1                  ! size of c_str array
    if(present(lng)) length = max(length, lng)
    if(present(trim)) then
      if(trim) length = length0                ! shorten length0 to effective length of cstring (and override lng if present)
    endif

    allocate(character(len=length) :: f_str)   ! allocate result fstring

    do i = 1, length0                          ! copy from input up to effective string length
      f_str(i:i) = c_str(i)
    enddo
    if(length > length0) f_str(length0+1:length) = ' '   ! pad with blanks if necessary
  end function C_TO_Fortran_string

  function C_PTR_TO_Fortran_string(c_strp, lng, trim) result(f_str)
    implicit none
    type(C_PTR), intent(IN)                     :: c_strp  ! pointer to null terminated array of characters
    integer, intent(IN), optional               :: lng     ! minimum length of result fstring (ignore if trim is .true.
    logical, intent(IN), optional               :: trim    ! if true, trim result
    character(len=:), allocatable               :: f_str   ! Fortran character variable (len=*) (fstring)
    integer(C_SIZE_T) :: length, length0, maxlen
    integer :: i
    character(C_CHAR), dimension(:), pointer :: c_str

    maxlen = 1024*1024*1024
    length  = c_strnlen(c_strp, maxlen)                ! effective cstring length (stops at first NULL character)
    call C_F_POINTER(c_strp, c_str, [length])
    if(present(lng)) length = max(length, lng)
    if(present(trim)) then
      if(trim) length = length0                ! shorten length0 to effective length of cstring (and override lng if present)
    endif

    allocate(character(len=length) :: f_str)   ! allocate result fstring

    do i = 1, length0                          ! copy from input up to effective string length
      f_str(i:i) = c_str(i)
    enddo
    if(length > length0) f_str(length0+1:length) = ' '   ! pad with blanks if necessary

  end function C_PTR_TO_Fortran_string

  ! convert Fortran character(len=*) fstring into a  C compatible null terminated character array (cstring)
  ! optional argument lng specifies the minimum capacity of the result cstring
  ! if optional argument trim is .false., lng is ignored and the cstring will not contain the trailing blanks in the fstring
  function Fortran_TO_C_string(f_str, lng, trim) result(c_str)
    implicit none
    character(len=*), intent(IN)                :: f_str       ! Fortran character string (len=*)
    integer, intent(IN), optional               :: lng         ! minimum length of output cstring
    logical, intent(IN), optional               :: trim        ! if true, trim result
    character(C_CHAR), dimension(:), allocatable    :: c_str   ! null terminated array of characters
    integer :: length, length0, i

    length0 = len(f_str)                     ! size of fortran fstring including trailing blanks
    length  = length0
    if(present(lng)) length = max(length, lng )
    if(present(trim)) then
      if(trim) length0 = len_trim(f_str)     ! shorten length0 to trimmed length of input fstring
    endif

    allocate(c_str(length + 1))              ! allow space for null terminator

    do i = 1, length0                        ! copy from trimmed input
      c_str(i) = f_str(i:i)
    enddo
    c_str(length0+1 : length+1) = achar(0)   ! null terminator
  end function Fortran_TO_C_string

  function string_length_null_terminated(cstring) result(length) ! effective length of cstring (up to NULL)
    implicit none
    character(C_CHAR), dimension(:), intent(IN) :: cstring
    integer :: length
    length = c_strlen(cstring)
  end function string_length_null_terminated

  function string_length_fortran_variable(fstring) result(length) ! effective length of cstring (without trailing blanks)
    implicit none
    character(len=*), intent(IN) :: fstring
    integer :: length
    length = len_trim(fstring)
  end function string_length_fortran_variable

  function string_size_null_terminated(cstring) result(length)  ! full size of character array
    implicit none
    character(C_CHAR), dimension(:), intent(IN) :: cstring
    integer :: length
    length = size(cstring)
  end function string_size_null_terminated

  function string_size_fortran_variable(fstring) result(length)  ! full size of Fortran fstring
    implicit none
    character(len=*), intent(IN) :: fstring
    integer :: length
    length = len(fstring)
  end function string_size_fortran_variable

  subroutine FC_str_test
    implicit none
    character(C_CHAR), dimension(:), allocatable :: alloc_str1, alloc_str2
    character(len=:), allocatable, target :: alloc_name1, alloc_name0, alloc_name2

    alloc_name0 = "tagada    "
    print *,'alloc_name0 : size, len =',FC_strsize(alloc_name0), FC_strlen(alloc_name0)
    alloc_str1 = FC_switch(alloc_name0)
    print *,'alloc_str1 = FC_switch(alloc_name0) =',"'",alloc_str1,"'"
    print *,'alloc_str1 : size, len =',FC_strsize(alloc_str1), FC_strlen(alloc_str1)
    alloc_str2 = FC_switch(alloc_name0, trim=.true.)
    print *,'alloc_str2 = FC_switch(alloc_name0, 0, .t.) =',"'",alloc_str2,"'"
    print *,'alloc_str2 : size, len =',FC_strsize(alloc_str2), FC_strlen(alloc_str2)
    alloc_name1 = FC_switch(alloc_str1)
    print *,"alloc_name1 = FC_switch(alloc_str1) = '",alloc_name1,"'"
    print *,'alloc_name1 : size, len =',FC_strsize(alloc_name1), FC_strlen(alloc_name1)
    alloc_name2 = FC_switch(alloc_str2)
    print *,"alloc_name2 = FC_switch(alloc_str2) = '",alloc_name2,"'"
    print *,'alloc_name2 : size, len =',FC_strsize(alloc_name2), FC_strlen(alloc_name2)
    alloc_name2 = FC_switch(alloc_str2, trim=.true.)
    print *,"alloc_name2 = FC_switch(alloc_str2, 0, .t.) = '",alloc_name2,"'"
    print *,'alloc_name2 : size, len =',FC_strsize(alloc_name2), FC_strlen(alloc_name2)

  end subroutine FC_str_test

end module
