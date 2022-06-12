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
interface
subroutine add_to_dot48_4(sum, err, a, b, N, fold) BIND(C,name='AddToDot48')
  implicit none
  integer, intent(IN) :: N
  real(kind=4), dimension(N), intent(IN)    :: a, b    ! input data to dot product
  real(kind=8), dimension(4), intent(INOUT) :: sum     ! accumulated partial sums
  real(kind=8), dimension(4), intent(INOUT) :: err     ! acccumulated error terms
  integer, intent(IN) :: fold
end
subroutine add_to_dot48_c(sum, err, a, b, N, fold) BIND(C,name='AddToDot48_c')
  implicit none
  integer, intent(IN), value :: N
  real(kind=4), dimension(N), intent(IN)    :: a, b    ! input data to dot product
  real(kind=8), dimension(4), intent(INOUT) :: sum     ! accumulated partial sums
  real(kind=8), dimension(4), intent(INOUT) :: err     ! acccumulated error terms
  integer, intent(IN), value :: fold
end
subroutine add_to_sum48_4(sum, err, input, N, fold) BIND(C,name='AddToSum48')
  implicit none
  integer, intent(IN) :: N
  real(kind=4), dimension(N), intent(IN)    :: input   ! input data to sum
  real(kind=8), dimension(4), intent(INOUT) :: sum     ! accumulated partial sums
  real(kind=8), dimension(4), intent(INOUT) :: err     ! acccumulated error terms
  integer, intent(IN) :: fold
end
subroutine add_to_sum48_c(sum, err, input, N, fold) BIND(C,name='AddToSum48_c')
  implicit none
  integer, intent(IN), value :: N
  real(kind=4), dimension(N), intent(IN)    :: input   ! input data to sum
  real(kind=8), dimension(4), intent(INOUT) :: sum     ! accumulated partial sums
  real(kind=8), dimension(4), intent(INOUT) :: err     ! acccumulated error terms
  integer, intent(IN), value :: fold
end
subroutine add_to_dot8_4(sum, err, a, b, N, fold) BIND(C,name='AddToDot8')
  implicit none
  integer, intent(IN) :: N
  real(kind=8), dimension(N), intent(IN)    :: a, b    ! input data to sum
  real(kind=8), dimension(4), intent(INOUT) :: sum     ! accumulated partial sums
  real(kind=8), dimension(4), intent(INOUT) :: err     ! acccumulated error terms
  integer, intent(IN) :: fold
end
subroutine add_to_dot8_c(sum, err, a, b, N, fold) BIND(C,name='AddToDot8_c')
  implicit none
  integer, intent(IN), value :: N
  real(kind=8), dimension(N), intent(IN)    :: a, b    ! input data to sum
  real(kind=8), dimension(4), intent(INOUT) :: sum     ! accumulated partial sums
  real(kind=8), dimension(4), intent(INOUT) :: err     ! acccumulated error terms
  integer, intent(IN), value :: fold
end
subroutine add_to_sum8_4(sum, err, input, N, fold) BIND(C,name='AddToSum8')
  implicit none
  integer, intent(IN) :: N
  real(kind=8), dimension(N), intent(IN)    :: input   ! input data to sum
  real(kind=8), dimension(4), intent(INOUT) :: sum     ! accumulated partial sums
  real(kind=8), dimension(4), intent(INOUT) :: err     ! acccumulated error terms
  integer, intent(IN) :: fold
end
subroutine add_to_sum8_c(sum, err, input, N, fold) BIND(C,name='AddToSum8_c')
  implicit none
  integer, intent(IN), value :: N
  real(kind=8), dimension(N), intent(IN)    :: input   ! input data to sum
  real(kind=8), dimension(4), intent(INOUT) :: sum     ! accumulated partial sums
  real(kind=8), dimension(4), intent(INOUT) :: err     ! acccumulated error terms
  integer, intent(IN), value :: fold
end
subroutine add_to_sum4_8(sum, err, input, N, fold) BIND(C,name='AddToSum4')
  implicit none
  integer, intent(IN) :: N
  real(kind=4), dimension(N), intent(IN)    :: input   ! input data to sum
  real(kind=4), dimension(8), intent(INOUT) :: sum     ! accumulated partial sums
  real(kind=4), dimension(8), intent(INOUT) :: err     ! acccumulated error terms
  integer, intent(IN) :: fold
end
end interface
