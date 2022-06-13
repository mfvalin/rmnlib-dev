!  compensated sums for FORTRAN 
!  Copyright (C) 2022  Recherche en Prevision Numerique
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
!> \author M. Valin,   Recherche en Prevision Numerique
!> \date 2022
!
! -DVOLATILE may be needed to defeat some aggressive optimizations
! Intel ifx seems to need it if -fp-model=precise/strict is not used
! -DVOLATILE is highly detrimental to performance

module compensated_sums
  implicit none
  private :: init_csum
  private :: add_to_csum4, add_to_csum41, add_to_csum42
  private :: add_to_csum8, add_to_cdot4, add_to_cdot8
  public :: csum
#include <compensated_sum.inc>
  type :: csum
    private
    real(kind=8), dimension(4) :: sum
    real(kind=8), dimension(4) :: err
    contains
    procedure, PASS :: init  => init_csum
    procedure, PASS :: csum  => get_csum
    procedure, PASS :: csums => get_csums

    procedure, PASS :: add4  => add_to_csum4
    procedure, PASS :: add41 => add_to_csum41
    procedure, PASS :: add42 => add_to_csum42
    procedure, PASS :: add43 => add_to_csum43
    procedure, PASS :: add8  => add_to_csum8
    procedure, PASS :: add81 => add_to_csum81
    procedure, PASS :: add82 => add_to_csum82
    procedure, PASS :: add83 => add_to_csum83
    GENERIC :: add => add81, add82, add83, add41, add42, add43

    procedure, PASS :: dot4  => add_to_cdot4
    procedure, PASS :: dot41 => add_to_cdot41
    procedure, PASS :: dot42 => add_to_cdot42
    procedure, PASS :: dot43 => add_to_cdot43
    procedure, PASS :: dot8  => add_to_cdot8
    procedure, PASS :: dot81 => add_to_cdot81
    procedure, PASS :: dot82 => add_to_cdot82
    procedure, PASS :: dot83 => add_to_cdot83
    GENERIC :: dot => dot81, dot82, dot83, dot41, dot42, dot43
  end type
contains
  function get_csum(this) result(sum)
    implicit none
    class(csum), intent(IN) :: this
    real(kind=8) :: sum
    sum = this%sum(1)
  end function
  function get_csums(this) result(sums)
    implicit none
    class(csum), intent(IN) :: this
    real(kind=8), dimension(4) :: sums
    sums = this%sum
  end function
  subroutine init_csum(this)
    implicit none
    class(csum), intent(OUT) :: this
    this%sum = 0.0
    this%err = 0.0
  end subroutine
  subroutine add_to_csum4(this, a, n, fold, init)
    implicit none
    class(csum), intent(INOUT) :: this
    integer, intent(IN) :: n
#define IgnoreTypeKindRank a
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    integer, intent(IN), OPTIONAL :: fold, init
    integer :: ifold
    ifold = 0
    if(present(fold)) ifold = fold
    if(present(init)) call init_csum(this)
    call add_to_sum48(this%sum, this%err, a, n, ifold)
  end subroutine
  subroutine add_to_csum41(this, a, n, fold, init)
    implicit none
    class(csum), intent(INOUT) :: this
    integer, intent(IN) :: n
    real(kind=4), dimension(*), intent(IN)    :: a
    integer, intent(IN), OPTIONAL :: fold, init
    integer :: ifold
    ifold = 0
    if(present(fold)) ifold = fold
    if(present(init)) call init_csum(this)
    call add_to_sum48(this%sum, this%err, a, n, ifold)
  end subroutine
  subroutine add_to_csum42(this, a, n, fold, init)
    implicit none
    class(csum), intent(INOUT) :: this
    integer, intent(IN) :: n
    real(kind=4), dimension(1,*), intent(IN)    :: a
    integer, intent(IN), OPTIONAL :: fold, init
    integer :: ifold
    ifold = 0
    if(present(fold)) ifold = fold
    if(present(init)) call init_csum(this)
    call add_to_sum48(this%sum, this%err, a, n, ifold)
  end subroutine
  subroutine add_to_csum43(this, a, n, fold, init)
    implicit none
    class(csum), intent(INOUT) :: this
    integer, intent(IN) :: n
    real(kind=4), dimension(1,1,*), intent(IN)    :: a
    integer, intent(IN), OPTIONAL :: fold, init
    integer :: ifold
    ifold = 0
    if(present(fold)) ifold = fold
    if(present(init)) call init_csum(this)
    call add_to_sum48(this%sum, this%err, a, n, ifold)
  end subroutine
  subroutine add_to_csum8(this, a, n, fold, init)
    implicit none
    class(csum), intent(INOUT) :: this
    integer, intent(IN) :: n
#define IgnoreTypeKindRank a
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    integer, intent(IN), OPTIONAL :: fold, init
    integer :: ifold
    ifold = 0
    if(present(fold)) ifold = fold
    if(present(init)) call init_csum(this)
    call add_to_sum8(this%sum, this%err, a, n, ifold)
  end subroutine
  subroutine add_to_csum81(this, a, n, fold, init)
    implicit none
    class(csum), intent(INOUT) :: this
    integer, intent(IN) :: n
    real(kind=8), dimension(*), intent(IN)    :: a
    integer, intent(IN), OPTIONAL :: fold, init
    integer :: ifold
    ifold = 0
    if(present(fold)) ifold = fold
    if(present(init)) call init_csum(this)
    call add_to_sum8(this%sum, this%err, a, n, ifold)
  end subroutine
  subroutine add_to_csum82(this, a, n, fold, init)
    implicit none
    class(csum), intent(INOUT) :: this
    integer, intent(IN) :: n
    real(kind=8), dimension(1,*), intent(IN)    :: a
    integer, intent(IN), OPTIONAL :: fold, init
    integer :: ifold
    ifold = 0
    if(present(fold)) ifold = fold
    if(present(init)) call init_csum(this)
    call add_to_sum8(this%sum, this%err, a, n, ifold)
  end subroutine
  subroutine add_to_csum83(this, a, n, fold, init)
    implicit none
    class(csum), intent(INOUT) :: this
    integer, intent(IN) :: n
    real(kind=8), dimension(1,1,*), intent(IN)    :: a
    integer, intent(IN), OPTIONAL :: fold, init
    integer :: ifold
    ifold = 0
    if(present(fold)) ifold = fold
    if(present(init)) call init_csum(this)
    call add_to_sum8(this%sum, this%err, a, n, ifold)
  end subroutine
  subroutine add_to_cdot4(this, a, b, n, fold, init)
    implicit none
    class(csum), intent(INOUT) :: this
    integer, intent(IN) :: n
#define IgnoreTypeKindRank a, b
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    integer, intent(IN), OPTIONAL :: fold, init
    integer :: ifold
    ifold = 0
    if(present(fold)) ifold = fold
    if(present(init)) call init_csum(this)
    call add_to_dot48(this%sum, this%err, a, b, n, ifold)
  end subroutine
  subroutine add_to_cdot41(this, a, b, n, fold, init)
    implicit none
    class(csum), intent(INOUT) :: this
    integer, intent(IN) :: n
    real(kind=4), dimension(*), intent(IN)    :: a, b
    integer, intent(IN), OPTIONAL :: fold, init
    integer :: ifold
    ifold = 0
    if(present(fold)) ifold = fold
    if(present(init)) call init_csum(this)
    call add_to_dot48(this%sum, this%err, a, b, n, ifold)
  end subroutine
  subroutine add_to_cdot42(this, a, b, n, fold, init)
    implicit none
    class(csum), intent(INOUT) :: this
    integer, intent(IN) :: n
    real(kind=4), dimension(1,*), intent(IN)    :: a, b
    integer, intent(IN), OPTIONAL :: fold, init
    integer :: ifold
    ifold = 0
    if(present(fold)) ifold = fold
    if(present(init)) call init_csum(this)
    call add_to_dot48(this%sum, this%err, a, b, n, ifold)
  end subroutine
  subroutine add_to_cdot43(this, a, b, n, fold, init)
    implicit none
    class(csum), intent(INOUT) :: this
    integer, intent(IN) :: n
    real(kind=4), dimension(1,1,*), intent(IN)    :: a, b
    integer, intent(IN), OPTIONAL :: fold, init
    integer :: ifold
    ifold = 0
    if(present(fold)) ifold = fold
    if(present(init)) call init_csum(this)
    call add_to_dot48(this%sum, this%err, a, b, n, ifold)
  end subroutine
  subroutine add_to_cdot8(this, a, b, n, fold, init)
    implicit none
    class(csum), intent(INOUT) :: this
    integer, intent(IN) :: n
#define IgnoreTypeKindRank a, b
#define ExtraAttributes 
#include <IgnoreTypeKindRank.hf>
    integer, intent(IN), OPTIONAL :: fold, init
    integer :: ifold
    ifold = 0
    if(present(fold)) ifold = fold
    if(present(init)) call init_csum(this)
    call add_to_dot8(this%sum, this%err, a, b, n, ifold)
  end subroutine
  subroutine add_to_cdot81(this, a, b, n, fold, init)
    implicit none
    class(csum), intent(INOUT) :: this
    integer, intent(IN) :: n
    real(kind=8), dimension(*), intent(IN)    :: a, b
    integer, intent(IN), OPTIONAL :: fold, init
    integer :: ifold
    ifold = 0
    if(present(fold)) ifold = fold
    if(present(init)) call init_csum(this)
    call add_to_dot8(this%sum, this%err, a, b, n, ifold)
  end subroutine
  subroutine add_to_cdot82(this, a, b, n, fold, init)
    implicit none
    class(csum), intent(INOUT) :: this
    integer, intent(IN) :: n
    real(kind=8), dimension(1,*), intent(IN)    :: a, b
    integer, intent(IN), OPTIONAL :: fold, init
    integer :: ifold
    ifold = 0
    if(present(fold)) ifold = fold
    if(present(init)) call init_csum(this)
    call add_to_dot8(this%sum, this%err, a, b, n, ifold)
  end subroutine
  subroutine add_to_cdot83(this, a, b, n, fold, init)
    implicit none
    class(csum), intent(INOUT) :: this
    integer, intent(IN) :: n
    real(kind=8), dimension(1,1,*), intent(IN)    :: a, b
    integer, intent(IN), OPTIONAL :: fold, init
    integer :: ifold
    ifold = 0
    if(present(fold)) ifold = fold
    if(present(init)) call init_csum(this)
    call add_to_dot8(this%sum, this%err, a, b, n, ifold)
  end subroutine
end module

#if ! defined(VOLATILE)
#define VOLATILE
#else
#undef VOLATILE
#define VOLATILE , volatile
#endif

#if defined(OLD_FORTRAN_CODE)
subroutine add_to_dot48_4(sum, err, a, b, N, fold) BIND(C,name='AddToDot48')
  implicit none
  integer, intent(IN) :: N
  real(kind=4), dimension(N), intent(IN)    :: a, b    ! input data to dot product
  real(kind=8), dimension(4), intent(INOUT) :: sum     ! accumulated partial sums
  real(kind=8), dimension(4), intent(INOUT) :: err     ! acccumulated error terms
  integer, intent(IN) :: fold

  real(kind=8) VOLATILE :: S(4), E(4)  ! volatile attribute needed to prevent unwanted optimization
  real(kind=8) VOLATILE :: Z(4), Y(4)  ! volatile attribute needed to prevent unwanted optimization
  integer :: i, j, i0, k
  real(kind=8) :: input
  input(k) = dble(a(k)) * dble(b(k))

  S = sum
  E = err
  i0 = mod(n,4)
  do j = 1, i0                      ! the first (1/2/3) sums (if applicable)
    Z(j) = S(j)
    Y(j) = input(j) + E(j)
    S(j) = Z(j) + Y(j)
    E(j) = (Z(j) - S(j)) + Y(j)
  enddo
  do i = i0, n-4, 4                 ! 4 parallel sums
    do j = 1, 4
      Z(j) = S(j)                   ! save current value of sum
      Y(j) = input(i+j) + E(j)      ! add accumulated error and input
      S(j) = Z(j) + Y(j)            ! add new term to sum
      E(j) = (Z(j) - S(j)) + Y(j)   ! new error = (old sum - new sum) + (new term + error)
    enddo
  enddo

  if(fold .ne. 0) then
    do j = 2, 4                   ! fold everything into S(1), E(1)
      E(1) = E(1) + E(j)          ! fold error terms
      E(j) = 0.0                  ! cancel after use
      Z(1) = S(1)                 ! save current value of sum
      Y(1) = S(j) + E(1)          ! add accumulated error and input
      S(j) = 0.0                  ! cancel after use
      S(1) = Z(1) + Y(1)          ! add new term to sum
      E(1) = (Z(1) - S(1)) + Y(1) ! new error = (old sum - new sum) + (new term + error)
    enddo
    sum(1) = S(1)
    err(1) = E(1)
  else
    sum = S
    err = E
  endif

end

subroutine add_to_sum48_4(sum, err, input, N, fold) BIND(C,name='AddToSum48')
  implicit none
  integer, intent(IN) :: N
  real(kind=4), dimension(N), intent(IN)    :: input   ! input data to sum
  real(kind=8), dimension(4), intent(INOUT) :: sum     ! accumulated partial sums
  real(kind=8), dimension(4), intent(INOUT) :: err     ! acccumulated error terms
  integer, intent(IN) :: fold

  real(kind=8) VOLATILE :: S(4), E(4)  ! volatile attribute needed to prevent unwanted optimization
  real(kind=8) VOLATILE :: Z(4), Y(4)  ! volatile attribute needed to prevent unwanted optimization
  integer :: i, j, i0

  S = sum
  E = err
  i0 = mod(n,4)
  do j = 1, i0                      ! the first (1/2/3) sums (if applicable)
    Z(j) = S(j)
    Y(j) = input(j) + E(j)
    S(j) = Z(j) + Y(j)
    E(j) = (Z(j) - S(j)) + Y(j)
  enddo
  do i = i0, n-4, 4                 ! 4 parallel sums
    do j = 1, 4
      Z(j) = S(j)                   ! save current value of sum
      Y(j) = input(i+j) + E(j)      ! add accumulated error and input
      S(j) = Z(j) + Y(j)            ! add new term to sum
      E(j) = (Z(j) - S(j)) + Y(j)   ! new error = (old sum - new sum) + (new term + error)
    enddo
  enddo

  if(fold .ne. 0) then
    do j = 2, 4                   ! fold everything into S(1), E(1)
      E(1) = E(1) + E(j)          ! fold error terms
      E(j) = 0.0                  ! cancel after use
      Z(1) = S(1)                 ! save current value of sum
      Y(1) = S(j) + E(1)          ! add accumulated error and input
      S(j) = 0.0                  ! cancel after use
      S(1) = Z(1) + Y(1)          ! add new term to sum
      E(1) = (Z(1) - S(1)) + Y(1) ! new error = (old sum - new sum) + (new term + error)
    enddo
    sum(1) = S(1)
    err(1) = E(1)
  else
    sum = S
    err = E
  endif

end

subroutine add_to_dot8_4(sum, err, a, b, N, fold) BIND(C,name='AddToDot8')
  implicit none
  integer, intent(IN) :: N
  real(kind=8), dimension(N), intent(IN)    :: a, b    ! input data to sum
  real(kind=8), dimension(4), intent(INOUT) :: sum     ! accumulated partial sums
  real(kind=8), dimension(4), intent(INOUT) :: err     ! acccumulated error terms
  integer, intent(IN) :: fold

  real(kind=8) VOLATILE :: S(4), E(4)  ! volatile attribute needed to prevent unwanted optimization
  real(kind=8) VOLATILE :: Z(4), Y(4)  ! volatile attribute needed to prevent unwanted optimization
  integer :: i, j, i0, k
  real(kind=8) :: input
  input(k) = a(k) * b(k)

  S = sum
  E = err
  i0 = mod(n,4)
  do j = 1, i0                      ! the first (1/2/3) sums (if applicable)
    Z(j) = S(j)
    Y(j) = input(j) + E(j)
    S(j) = Z(j) + Y(j)
    E(j) = (Z(j) - S(j)) + Y(j)
  enddo
  do i = i0, n-4, 4                 ! 4 parallel sums
    do j = 1, 4
      Z(j) = S(j)                   ! save current value of sum
      Y(j) = input(i+j) + E(j)      ! add accumulated error and input
      S(j) = Z(j) + Y(j)            ! add new term to sum
      E(j) = (Z(j) - S(j)) + Y(j)   ! new error = (old sum - new sum) + (new term + error)
    enddo
  enddo

  if(fold .ne. 0) then
    do j = 2, 4                   ! fold everything into S(1), E(1)
      E(1) = E(1) + E(j)          ! fold error terms
      E(j) = 0.0                  ! cancel after use
      Z(1) = S(1)                 ! save current value of sum
      Y(1) = S(j) + E(1)          ! add accumulated error and input
      S(j) = 0.0                  ! cancel after use
      S(1) = Z(1) + Y(1)          ! add new term to sum
      E(1) = (Z(1) - S(1)) + Y(1) ! new error = (old sum - new sum) + (new term + error)
    enddo
    sum(1) = S(1)
    err(1) = E(1)
  else
    sum = S
    err = E
  endif

end

subroutine add_to_sum8_4(sum, err, input, N, fold) BIND(C,name='AddToSum8')
  implicit none
  integer, intent(IN) :: N
  real(kind=8), dimension(N), intent(IN)    :: input   ! input data to sum
  real(kind=8), dimension(4), intent(INOUT) :: sum     ! accumulated partial sums
  real(kind=8), dimension(4), intent(INOUT) :: err     ! acccumulated error terms
  integer, intent(IN) :: fold

  real(kind=8) VOLATILE :: S(4), E(4)  ! volatile attribute needed to prevent unwanted optimization
  real(kind=8) VOLATILE :: Z(4), Y(4)  ! volatile attribute needed to prevent unwanted optimization
  integer :: i, j, i0

  S = sum
  E = err
  i0 = mod(n,4)
  do j = 1, i0                      ! the first (1/2/3) sums (if applicable)
    Z(j) = S(j)
    Y(j) = input(j) + E(j)
    S(j) = Z(j) + Y(j)
    E(j) = (Z(j) - S(j)) + Y(j)
  enddo
  do i = i0, n-4, 4                 ! 4 parallel sums
    do j = 1, 4
      Z(j) = S(j)                   ! save current value of sum
      Y(j) = input(i+j) + E(j)      ! add accumulated error and input
      S(j) = Z(j) + Y(j)            ! add new term to sum
      E(j) = (Z(j) - S(j)) + Y(j)   ! new error = (old sum - new sum) + (new term + error)
    enddo
  enddo

  if(fold .ne. 0) then
    do j = 2, 4                   ! fold everything into S(1), E(1)
      E(1) = E(1) + E(j)          ! fold error terms
      E(j) = 0.0                  ! cancel after use
      Z(1) = S(1)                 ! save current value of sum
      Y(1) = S(j) + E(1)          ! add accumulated error and input
      S(j) = 0.0                  ! cancel after use
      S(1) = Z(1) + Y(1)          ! add new term to sum
      E(1) = (Z(1) - S(1)) + Y(1) ! new error = (old sum - new sum) + (new term + error)
    enddo
    sum(1) = S(1)
    err(1) = E(1)
  else
    sum = S
    err = E
  endif

end

subroutine add_to_sum4_8(sum, err, input, N, fold) BIND(C,name='AddToSum4')
  implicit none
  integer, intent(IN) :: N
  real(kind=4), dimension(N), intent(IN)    :: input   ! input data to sum
  real(kind=4), dimension(8), intent(INOUT) :: sum     ! accumulated partial sums
  real(kind=4), dimension(8), intent(INOUT) :: err     ! acccumulated error terms
  integer, intent(IN) :: fold

  real(kind=4) VOLATILE :: S(8), E(8)  ! volatile attribute needed to prevent unwanted optimization
  real(kind=4) VOLATILE :: Z(8), Y(8)  ! volatile attribute needed to prevent unwanted optimization
  integer :: i, j, i0, i1

  S = sum
  E = err
  do i = 1, n-7, 8                  ! 8 parallel sums
    do j = 1, 8
      Z(j) = S(j)                   ! save current value of sum
      Y(j) = input(i+j-1) + E(j)    ! add accumulated error and input
      S(j) = Z(j) + Y(j)            ! add new term to sum
      E(j) = (Z(j) - S(j)) + Y(j)   ! new error = (old sum - new sum) + (new term + error)
    enddo
    i0 = i + 8
  enddo

  do i = i0, n-3, 4                 ! 4 parallel sums
    do j = 1, 4
      Z(j) = S(j)                   ! save current value of sum
      Y(j) = input(i+j-1) + E(j)    ! add accumulated error and input
      S(j) = Z(j) + Y(j)            ! add new term to sum
      E(j) = (Z(j) - S(j)) + Y(j)   ! new error = (old sum - new sum) + (new term + error)
    enddo
    i1 = i + 4
  enddo

  do j = 1, mod(n,4)                ! the last (1/2/3) sums
    Z(j) = S(j)
    Y(j) = input(i1+j-1) + E(j)
    S(j) = Z(j) + Y(j)
    E(j) = (Z(j) - S(j)) + Y(j)
  enddo

  if(fold .ne. 0) then
    do j = 2, 8                   ! fold everything into S(1), E(1)
      E(1) = E(1) + E(j)          ! fold error terms
      E(j) = 0.0                  ! cancel after use
      Z(1) = S(1)                 ! save current value of sum
      Y(1) = S(j) + E(1)          ! add accumulated error and input
      S(j) = 0.0                  ! cancel after use
      S(1) = Z(1) + Y(1)          ! add new term to sum
      E(1) = (Z(1) - S(1)) + Y(1) ! new error = (old sum - new sum) + (new term + error)
    enddo
    sum(1) = S(1)
    err(1) = E(1)
  else
    sum = S
    err = E
  endif

end
#endif
