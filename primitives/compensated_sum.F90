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
#if ! defined(VOLATILE)
#define VOLATILE
#else
#undef VOLATILE
#define VOLATILE , volatile
#endif

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

#if defined(SELF_TEST)
#if 0
function SUM4_1(input, n) result(sum)
  implicit none
  integer, intent(IN) :: N
  real(kind=4), dimension(N), intent(IN)    :: input
  real(kind=4) :: sum

  integer :: i
  real(kind=4), volatile :: Y, S, E , Z

  S = 0.0
  E = 0.0
  do i = 1, n
    Y = input(i) + E      ! add accumulated error
    Z = S                ! save current value of sum
    S = Z + Y            ! add new term to sum
    E = (Z - S) + Y      ! new error = (old sum - new sum) + (new term + error)
  enddo
  sum = S
end
#endif

#if defined LARGE
#define NPTS 64001031
#define EXP4 4
#define EXP8 32
#else
#define NPTS 1031
#endif

function dumbsum(a,n) result(s)
  integer, intent(IN) :: n
  real(kind=4), dimension(n), intent(IN) :: a
  real(kind=4) :: s
  real(kind=8) :: s8
  integer :: i
  s8 = 0.0
  do i=1,n
    s8 = s8 + a(i)
  enddo
  s = s8
end function

function WALL_Time() result(S)
  real(kind=8) :: s
  interface
  function gettimeofday(t1, t2) result(status) bind(C,name='gettimeofday')
    integer(kind=8), dimension(*), intent(OUT) :: t1, t2
    integer :: status
  end
  end interface
  integer(kind=8), dimension(2) :: t1, t2
  integer :: status
  status = gettimeofday(t1, t2)
  s = t1(1) + 1E-6_8 * t1(2)
  return
end

program test_sum
  use mpi
  implicit none
  include 'compensated_sum.inc'
  real(kind=4), dimension(NPTS) :: A4, B4
  real(kind=8), dimension(NPTS) :: A8, B8
  real(kind=4), dimension(8) :: S4, E4
  real(kind=8), dimension(4) :: S8, E8, S48, E48
  real(kind=4) :: r4, r4b
  real(kind=8) :: r8, r8b
  real(kind=4), external :: dumbsum
  real(kind=8), external :: WALL_Time
  real(kind=8) :: t4, t8, t48
  integer :: ierr, my_rank

  call mpi_init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
  A4       = 1.0
  B4       = 1.0
  A8       = 1.0
  B8       = 1.0
  A4(NPTS-999)   = 0.5
  A8(NPTS-999)   = 0.5
#if defined LARGE
  A4(NPTS-999:NPTS) = A4(NPTS-999) ** 4
  A8(NPTS-999:NPTS) = A8(NPTS-999) ** 32
!   A8(NPTS-999:NPTS) = A8(NPTS-999) ** 4
#else
  A4(NPTS-999:NPTS) = A4(NPTS-999) ** 24
  A8(NPTS-999:NPTS) = A8(NPTS-999) ** 53
!   A8(NPTS-999:NPTS) = A8(NPTS-999) ** 24
#endif
  S4 = 0.0
  E4 = 0.0
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  t4 = WALL_Time()
  call add_to_sum4_8(S4, E4, A4, NPTS, 1)
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  t4 = WALL_Time() - t4
  S8 = 0.0
  E8 = 0.0
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  t8 = WALL_Time()
  call add_to_sum8_4(S8, E8, A8, NPTS, 1)
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  t8 = WALL_Time() - t8
  r4 = (size(a4) - 1000) + (1000.0_8*A4(NPTS-999))
  r4b = S4(1)+(1.0_8*E4(1))
  r8b = S4(1)+(1.0_8*E4(1))
  r8 = (size(a8) - 1000) + (1000.0*A8(NPTS-999))
  if(my_rank ==0) then
  print 3,'                 ANSWER                   SUM                 COMPENSATED SUM                ERROR'
  print 1,'SUM4  =', sum(1.0_8*A4), sum(A4), S4(1), E4(1), dumbsum(A4, NPTS)!, r8b
  print 2,'SUM8  =', r8, sum(A8), S8(1), E8(1)
  endif
  S48 = 0.0
  E48 = 0.0
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  t48 = WALL_Time()
  call add_to_sum48_4(S48, E48, A4, NPTS, 1)
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  t48 = WALL_Time() - t48
  if(my_rank ==0) then
  print 1,'SUM48 =', sum(1.0_8*A4), sum(1.0*A4), S48(1), E48(1)
  print 1, 'T     =',t4,t8,t48
  print 1, 'PTS/s =',NPTS/t4,NPTS/t8,NPTS/t48
!-------------------------------------------------------------------------------------
  print *,''
  print *,'dot product with a vector of 1.0'
  endif
  S8 = 0.0
  E8 = 0.0
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  t8 = WALL_Time()
  call add_to_dot8_4(S8, E8, A8, B8, NPTS, 1)
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  t8 = WALL_Time() - t8
  if(my_rank ==0) then
  print 2,'DOT8  =', r8, sum(A8*B8), S8(1), E8(1)
  endif
  S8 = 0.0
  E8 = 0.0
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  t48 = WALL_Time()
  call add_to_dot48_4(S8, E8, A4, B4, NPTS, 1)
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  t48 = WALL_Time() - t48
  if(my_rank ==0) then
  print 1,'DOT48 =', sum(1.0_8*A4*B4), sum(A4*B4), S8(1), E8(1)
  print 1, 'T     =',t8,t48
  print 1, 'PTS/s =',NPTS/t8,NPTS/t48
!-------------------------------------------------------------------------------------
  print *,''
  print *,'dot product with self'
  endif
  A4(NPTS-999)   = 0.5
  A8(NPTS-999)   = 0.5
#if defined LARGE
  A4(NPTS-999:NPTS) = A4(NPTS-999) ** 2
  A8(NPTS-999:NPTS) = A8(NPTS-999) ** 16
!   A8(NPTS-999:NPTS) = A8(NPTS-999) ** 2
#else
  A4(NPTS-999:NPTS) = A4(NPTS-999) ** 12
  A8(NPTS-999:NPTS) = A8(NPTS-999) ** 26
!   A8(NPTS-999:NPTS) = A8(NPTS-999) ** 12
#endif
  S8 = 0.0
  E8 = 0.0
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  t8 = WALL_Time()
  call add_to_dot8_4(S8, E8, A8, A8, NPTS, 1)
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  t8 = WALL_Time() - t8
  r8 = (size(a8) - 1000) + (1000.0 * A8(NPTS-999)**2) 
  if(my_rank ==0) then
  print 2,'DOT8  =', r8, sum(A8**2), S8(1), E8(1)
  endif
  S8 = 0.0
  E8 = 0.0
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  t48 = WALL_Time()
  call add_to_dot48_4(S8, E8, A4, A4, NPTS, 1)
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  t48 = WALL_Time() - t48
  r4 = (size(a4) - 1000) + (1000.0_8 * A4(NPTS-999)**2)
  if(my_rank ==0) then
  print 1,'DOT48 =', sum(1.0_8*A4*A4), sum(A4*A4), S8(1), E8(1)
  print 1, 'T     =',t8,t48
  print 1, 'PTS/s =',NPTS/t8,NPTS/t48
  endif

  call mpi_finalize(ierr)
1 format (A8,8G25.9)
2 format (A8,8G25.17)
3 format (A)
end
#endif
