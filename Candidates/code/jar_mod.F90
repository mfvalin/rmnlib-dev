!  serializer for FORTRAN programming
!  Copyright (C) 2021  Recherche en Prevision Numerique
! 
!  This software is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation,
!  version 2.1 of the License.
! 
!  This software is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
! Authors:   M.Valin   , Recherche en Prevision Numerique, 2021
!            V.Magnoux , Recherche en Prevision Numerique, 2021
!
! _the Cray Fortran compiler treats loc() as a type(C_PTR), other compilers as integer(C_INTPTR_T)
#if defined(_CRAYFTN)
#define WHAT_TYPE type(C_PTR)
#else
#define WHAT_TYPE integer(C_INTPTR_T)
#endif

module jar_module
  use ISO_C_BINDING
  implicit none

  private

  interface
    function libc_malloc(sz) result(p) BIND(C,name='malloc')
      import :: C_SIZE_T, C_PTR
      implicit none
      integer(C_SIZE_T), intent(IN), value :: sz
      type(C_PTR) :: p
    end function libc_malloc
    subroutine libc_free(p) BIND(C, name='free')
      import :: C_PTR
      implicit none
      type(C_PTR), intent(IN), value :: p
    end subroutine libc_free
  end interface

  logical, save, private :: debug_mode = .false.

  integer, parameter, public :: JAR_ELEMENT = C_INT64_T !< We want 64-bit elements in the jars

  type, public, BIND(C) :: c_jar                         ! C interoperable version of jar
    private
    integer(JAR_ELEMENT) :: size = 0             ! capacity of jar
    integer(JAR_ELEMENT) :: top  = 0             ! last posision "written" (cannot write beyond size)
    integer(JAR_ELEMENT) :: bot  = 0             ! last position "read" (cannot read beyond top)
    integer(JAR_ELEMENT) :: opt  = 0             ! option flags (0 owner of data memory, 1 not owner)
    type(C_PTR)          :: p = C_NULL_PTR       ! address of actual data
  end type

  type, public :: jar                                    ! same as c_jar, but with type bound procedures
    private
    integer(JAR_ELEMENT) :: size = 0             ! capacity of jar
    integer(JAR_ELEMENT) :: top  = 0             ! last posision "written" (cannot write beyond size)
    integer(JAR_ELEMENT) :: bot  = 0             ! last position "read" (cannot read beyond top)
    integer(JAR_ELEMENT) :: opt  = 0             ! option flags (0 owner of data memory, 1 not owner)
    type(C_PTR)          :: p = C_NULL_PTR       ! address of actual data
  contains
    procedure, PASS :: new     => new_jar        ! create a new data jar, allocate data storage
    procedure, PASS :: shape   => shape_as_jar   ! transform an integer array into a jar
    procedure, PASS :: valid   => valid_jar      ! is jar valid (is there is a valid data pointer ?)
    procedure, PASS :: free    => free_jar       ! free jar data
    procedure, PASS :: rewind  => rewind_jar     ! rewind jar for extraction
    procedure, PASS :: reset   => reset_jar      ! make jar empty, keep allocated space
    procedure, PASS :: data    => jar_pointer    ! get C pointer to actual jar data
    procedure, PASS :: array   => jar_contents   ! get Fortran pointer to actual jar data
    procedure, PASS :: raw_array => jar_contents_full ! get Fortran pointer to entire jar data array
    procedure, PASS :: usable  => jar_size       ! maximum capacity of data jar
    procedure, PASS :: high    => jar_top        ! current number of elements inserted (written)
    procedure, PASS :: low     => jar_bot        ! current number of elements extracted (read)
    procedure, PASS :: avail   => jar_avail      ! current number of elements available for extraction (high - low)
    procedure, PASS :: put     => put_into_jar   ! add data to jar at top of jar (or at a specific position)
    procedure, PASS :: get     => get_outof_jar  ! get data from jar at current (or specific position)
    procedure, PASS :: print   => print_jar
    procedure, NOPASS :: debug => debug_jars
    final :: final_jar
  end type

  contains

  function debug_jars(mode) result(old)      ! set debug mode, get previous setting
    implicit none
    logical, intent(IN), value :: mode       ! .true. , set debug mode, .false. cancel debug mode
    logical :: old                           ! return previous setting

    if(debug_mode .or. mode) print *,'DEBUG: setting debug mode to',mode,' was',debug_mode
    old        = debug_mode
    debug_mode = mode
  end function debug_jars

  function new_jar(j, data_size) result(ok)  ! allocate a jar of arbitrary size
    implicit none
    class(jar), intent(INOUT) :: j           ! the data jar
    integer, intent(IN), value :: data_size  ! number of elements in jar
    integer :: ok                            ! 0 if O.K., -1 if error

    integer(C_SIZE_T)    :: dsz
    integer(JAR_ELEMENT) :: dummy_jar_element

    ok = -1
    if(C_ASSOCIATED(j % p)) return          ! error, there is already an allocated data container

    dsz = data_size
    j % p = libc_malloc( dsz * storage_size(dummy_jar_element) / 8) ! size in bytes
    if(.not. C_ASSOCIATED(j % p)) return    ! malloc failed
    ok = 0

    j % top  = 0                            ! data jar is empty (no data written)
    j % bot  = 0                            ! data jar is empty (no data to read)
    j % opt  = 0                            ! options = 0, jar owns the memory storage
    j % size = data_size                    ! data jar capacity

  end function new_jar

  function shape_as_jar(j, array, arraysize) result(ok)  ! transform an integer array into a jar
    implicit none
    class(jar), intent(INOUT) :: j          ! the data jar
    integer(C_INT), intent(IN), value :: arraysize  ! number of elements in arrray
    integer(JAR_ELEMENT), dimension(arraysize), intent(IN) :: array    ! DO NOT LIE
    integer :: ok                            ! 0 if O.K., -1 if error

    integer(C_INTPTR_T) :: temp

    ok = -1
    if(C_ASSOCIATED(j % p)) return          ! error, there is already an allocated data container

    temp = LOC(array)
    j % p = transfer(temp, j % p)
    ok = 0
!   arraysize * storage_size(jar_element)/storage_size(integer)
    j % top  = arraysize                    ! data jar is full
    j % bot  = 0                            ! no data has been read yet
    j % opt  = 1                            ! options = 1, jar is not the owner of the storage
    j % size = arraysize                    ! data jar capacity

  end function shape_as_jar

  function valid_jar(j) result(ok)          ! do we have a valid data pointer
    implicit none
    class(jar), intent(INOUT) :: j          ! the data jar
    logical :: ok                           ! .true. if valid, .false. if not

    ok = C_ASSOCIATED(j%p)
  end function valid_jar

  subroutine reset_jar(j)                   ! re initialize a jar (leave data container alone)
    implicit none
    class(jar), intent(INOUT) :: j          ! the data jar

    j % top  = 0                            ! data jar is empty (no data written)
    j % bot  = 0                            ! data jar is empty (no data read)
  end subroutine reset_jar

  subroutine rewind_jar(j)                  ! reset extraction pointer of a jar to the beginning
    implicit none
    class(jar), intent(INOUT) :: j          ! the data jar

    j % bot  = 0                            ! no data read yet
  end subroutine rewind_jar

  subroutine print_jar(j, max_elem)         ! print jar info
    implicit none
    class(jar), intent(IN) :: j             ! the data jar
    integer(JAR_ELEMENT), intent(IN), value :: max_elem  ! max data elements to print

    integer, dimension(:), pointer :: data

    call C_F_POINTER(j%p, data, [j%size])
    print 1, j%size, j%bot, j%top, data(1:min(max_elem,j%top))
1   format(3I6,(20Z9.8))
  end subroutine print_jar

  function jar_avail(j) result(sz)          ! get amount of available data in jar (JAR_ELEMENT units)
    implicit none
    class(jar), intent(IN) :: j             ! the data jar
    integer(JAR_ELEMENT) :: sz              ! available data in jar (JAR_ELEMENT units)

    sz = -1
    if(.not. C_ASSOCIATED(j % p)) return
    sz = j % top - j % bot

  end function jar_avail

  function jar_top(j) result(sz)            ! get amount of data in jar (JAR_ELEMENT units)
    implicit none
    class(jar), intent(IN) :: j             ! the data jar
    integer(JAR_ELEMENT) :: sz              ! amount of data inserted in jar (JAR_ELEMENT units)

    sz = -1
    if(.not. C_ASSOCIATED(j % p)) return
    sz = j % top

  end function jar_top

  function jar_bot(j) result(sz)            ! get amount of data in jar (JAR_ELEMENT units)
    implicit none
    class(jar), intent(IN) :: j             ! the data jar
    integer(JAR_ELEMENT) :: sz              ! amount of data inserted in jar (JAR_ELEMENT units)

    sz = -1
    if(.not. C_ASSOCIATED(j % p)) return
    sz = j % bot

  end function jar_bot

  function jar_size(j) result(sz)           ! get data jar capacity (JAR_ELEMENT units)
    implicit none
    class(jar), intent(IN) :: j             ! the data jar
    integer(JAR_ELEMENT) :: sz              ! data jar capacity (JAR_ELEMENT units)

    sz = -1
    if(.not. C_ASSOCIATED(j % p)) return
    sz = j % size

  end function jar_size

  function jar_pointer(j) result(p)         ! get C pointer to jar data (1 D array of JAR_ELEMENTs)
    implicit none
    class(jar), intent(IN) :: j             ! the data jar
    type(C_PTR) :: p                        ! C pointer to jar data 

    p = C_NULL_PTR
    if(.not. C_ASSOCIATED(j % p)) return
    p = j % p

  end function jar_pointer

  function jar_contents(j) result(fp)       ! get Fortran pointer to jar data (1 D array of JAR_ELEMENTs)
    implicit none
    class(jar), intent(IN) :: j             ! the data jar
    integer(JAR_ELEMENT), dimension(:),  pointer :: fp  ! Fortran pointer to jar data

    nullify(fp)
    if(.not. C_ASSOCIATED(j % p)) return    ! no data pointer, return NULL pointer
    if(j % top == 0) return                 ! empty jar, return NULL pointer

    call C_F_POINTER(j % p, fp, [j % top])  ! Fortran pointer to array of j%size JAR_ELEMENTs

  end function jar_contents

  function jar_contents_full(j) result(fp)
    implicit none
    class(jar), intent(IN) :: j
    integer(JAR_ELEMENT), dimension(:), pointer :: fp

    nullify(fp)
    if (.not. C_ASSOCIATED(j % p)) return

    call C_F_POINTER(j % p, fp, [j % size])
  end function jar_contents_full

  subroutine final_jar(j)                    ! deallocate a jar's data if not already done at finalize (if jar owns it)
    implicit none
    type(jar), intent(INOUT) :: j            ! the data jar

    if(C_ASSOCIATED(j % p)) then
      if(debug_mode .and. j%opt == 0) print *,'DEBUG(jar finalize): freing jar memory, size =', j%size
      if(debug_mode .and. j%opt == 1) print *,'DEBUG(jar finalize): not owner, not freing jar memory, size =', j%size
      if(j%opt == 0) call libc_free(j % p)      ! release storage associated with jar if jar owns it
      j % p = C_NULL_PTR
    else
      if(debug_mode) print *,'DEBUG(jar finalize): nothing to free in jar'
    endif
    j % top  = 0                             ! data jar is now empty (no data written)
    j % bot  = 0                             ! data jar is now empty (no data to read)
    j % opt  = 0                             ! reset ownership flag
    j % size = 0                             ! data jar cannot store data
  end subroutine final_jar

  function free_jar(j) result(status)        ! deallocate a jar's data space (if jar owns it)
    implicit none
    class(jar), intent(INOUT) :: j           ! the data jar
    integer :: status                        ! 0 if O.K., -1 if nothing to free

    if(C_ASSOCIATED(j % p)) then
      if(debug_mode .and. j%opt == 0) print *,'DEBUG(jar free): freing jar memory'
      if(debug_mode .and. j%opt == 1) print *,'DEBUG(jar free): not owner, not freing jar memory'
      if(j % opt == 0) call libc_free(j % p)    ! release storage associated with jar if jar owns it
      j % p = C_NULL_PTR                     ! nullify data pointer to avoid accidents
      status = 0
    else
      if(debug_mode) print *,'DEBUG(jar free): nothing to free in jar'
      status = -1                            ! already freed
    endif
    j % top  = 0                             ! data jar is now empty (no data written)
    j % bot  = 0                             ! data jar is now empty (no data to read)
    j % opt  = 0                             ! reset ownership flag
    j % size = 0                             ! data jar cannot store data
  end function free_jar

#define IgnoreTypeKindRank object
#define ExtraAttributes , target
  function put_into_jar(j, object, size, where) result(sz)  ! insert data into data jar (IgnoreTypeKindRank version)
    implicit none
    class(jar), intent(INOUT) :: j                          ! the data jar
#include <IgnoreTypeKindRankPlus.hf>
    integer, intent(IN), value :: size                      ! size to insert = storage_size(item) * nb_of_items
    integer(JAR_ELEMENT), intent(IN), optional, value :: where           ! optional argument to force insertion point (1 = start of jar)
    integer(JAR_ELEMENT) :: sz                                           ! position of last inserted element (-1 if error)

    integer(JAR_ELEMENT) :: intsize, pos
    type(C_PTR) :: temp
    integer(JAR_ELEMENT), dimension(:), pointer :: je, content

    sz = -1
    if(.not. C_ASSOCIATED(j % p)) return

    call C_F_POINTER(j % p, content, [j % size])            ! pointer to jar data
    if( present(where) ) then
      pos = where - 1                                       ! insert starting at position "where"
    else
      pos = j%top                                           ! insert after data currently in jar
    endif
    if(pos < 0) return                                      ! invalid insertion position
    if(pos > j%top) content(j%top+1:pos) = 0                ! zero fill skipped portion

    intsize = (size + storage_size(content(1)) - 1) / storage_size(content(1))
    if(pos + intsize > j%size) return                       ! jar would overflow

!     temp    = transfer(what,temp)                           ! address of data to insert
    temp = C_LOC(object)
    call C_F_POINTER(temp, je, [intsize])                   ! make what into an integer array
    content(pos + 1 : pos + intsize) = je(1 : intsize)      ! insert into data portion of jar
    j % top = pos + intsize                                 ! update top of jar position
    sz = j % top                                            ! number of elements in jar

  end function put_into_jar

!   function add_to_jar(j, what, size, where) result(sz)      ! insert data into data jar
!     implicit none
!     class(jar), intent(INOUT) :: j                          ! the data jar
!     WHAT_TYPE, intent(IN), value :: what                    ! must match type of loc(xxx)
!     integer, intent(IN), value :: size                      ! size to insert = storage_size(item) * nb_of_items
!     integer, intent(IN), optional, value :: where           ! optional argument to force insertion point (1 = start of jar)
!     integer :: sz                                           ! position of last inserted element (-1 if error)
! 
!     integer :: intsize, pos
!     type(C_PTR) :: temp
!     integer(JAR_ELEMENT), dimension(:), pointer :: je, content
! 
!     sz = -1
!     if(.not. C_ASSOCIATED(j % p)) return
! 
!     call C_F_POINTER(j % p, content, [j % size])            ! pointer to jar data
!     if( present(where) ) then
!       pos = where - 1                                       ! insert starting at position "where"
!     else
!       pos = j%top                                           ! insert after data currently in jar
!     endif
!     if(pos < 0) return                                      ! invalid insertion position
!     if(pos > j%top) content(j%top+1:pos) = 0                ! zero fill skipped portion
! 
!     intsize = size / storage_size(content(1))
!     if(pos + intsize > j%size) return                       ! jar would overflow
! 
!     temp    = transfer(what,temp)                           ! address of data to insert
!     call C_F_POINTER(temp, je, [intsize])                   ! make what into an integer array
!     content(pos + 1 : pos + intsize) = je(1 : intsize)      ! insert into data portion of jar
!     j % top = pos + intsize                                 ! update top of jar position
!     sz = j % top                                            ! number of elements in jar
! 
!   end function add_to_jar

#define IgnoreTypeKindRank object
#define ExtraAttributes , target
  function get_outof_jar(j, object, size, where) result(sz) ! get data from data jar (IgnoreTypeKindRank version)
    implicit none
    class(jar), intent(INOUT) :: j                          ! the data jar
#include <IgnoreTypeKindRankPlus.hf>
    integer, intent(IN), value :: size                      ! size to insert = storage_size(item) * nb_of_items
    integer(JAR_ELEMENT), intent(IN), optional, value :: where           ! optional argument to force insertion point (1 = start of jar)
    integer(JAR_ELEMENT) :: sz                                           ! position of last extracted element (-1 if error)

    integer(JAR_ELEMENT) :: intsize, pos
    type(C_PTR) :: temp
    integer(JAR_ELEMENT), dimension(:), pointer :: je, content

    sz = -1
    if(.not. C_ASSOCIATED(j % p)) return

    call C_F_POINTER(j % p, content, [j % size])            ! pointer to jar data
    if( present(where) ) then
      pos = where - 1                                       ! insert at position "where"
    else
      pos = j%bot                                           ! insert after data currently in jar
    endif
    if(pos < 0) return                                      ! invalid insertion position

    intsize = size / storage_size(content(1))
    if(pos + intsize > j%top) return                        ! insufficient data in jar

!     temp    = transfer(what,temp)                           ! address of data to insert
    temp = C_LOC(object)
    call C_F_POINTER(temp, je, [intsize])                   ! make what into an integer array
    je(1 : intsize) = content(pos + 1 : pos + intsize)      ! insert into data portion of jar
    j % bot = pos + intsize                                 ! update top of jar position
    sz = j % bot                                            ! position of last extracted element

  end function get_outof_jar

!   function get_from_jar(j, what, size, where) result(sz)    ! get data from data jar
!     implicit none
!     class(jar), intent(INOUT) :: j                          ! the data jar
!     WHAT_TYPE, intent(IN), value :: what                    ! must match type of loc(xxx)
!     integer, intent(IN), value :: size                      ! size to insert = storage_size(item) * nb_of_items
!     integer, intent(IN), optional, value :: where           ! optional argument to force insertion point (1 = start of jar)
!     integer :: sz                                           ! position of last extracted element (-1 if error)
! 
!     integer :: intsize, pos
!     type(C_PTR) :: temp
!     integer(JAR_ELEMENT), dimension(:), pointer :: je, content
! 
!     sz = -1
!     if(.not. C_ASSOCIATED(j % p)) return
! 
!     call C_F_POINTER(j % p, content, [j % size])            ! pointer to jar data
!     if( present(where) ) then
!       pos = where - 1                                       ! insert at position "where"
!     else
!       pos = j%bot                                           ! insert after data currently in jar
!     endif
!     if(pos < 0) return                                      ! invalid insertion position
! 
!     intsize = size / storage_size(content(1))
!     if(pos + intsize > j%top) return                        ! insufficient data in jar
! 
!     temp    = transfer(what,temp)                           ! address of data to insert
!     call C_F_POINTER(temp, je, [intsize])                   ! make what into an integer array
!     je(1 : intsize) = content(pos + 1 : pos + intsize)      ! insert into data portion of jar
!     j % bot = pos + intsize                                 ! update top of jar position
!     sz = j % bot                                            ! position of last extracted element
! 
!   end function get_from_jar

end module jar_module
