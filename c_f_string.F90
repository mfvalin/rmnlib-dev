!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2020  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
! convert between C null terminated string and Fortran character string
! the C string is pointed to by a type(C_PTR) pointer
! direction == 0 C to Fortran conversion
! direction  > 0 Fortran to C conversion, direction = size of reception array 
! C_PTR <--> character(len=*)
subroutine c_f_string1(c_string, f_string, direction)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN), value :: c_string
  character(len=*), intent(INOUT) :: f_string
  integer, intent(IN) :: direction    ! 0 if C to Fortran, size of c_string if Fortran to C

  character(C_CHAR), dimension(:), pointer :: temp
  integer :: i

  call C_F_POINTER(c_string, temp, [len(trim(f_string))])
  if(direction == 0) then   ! C to Fortran
    i = 1
    do while(i <= len(trim(f_string)) .and. temp(i) .ne. ACHAR(0))
      f_string(i:i) = temp(i)
      i = i + 1
    enddo
    do while(i <= len(trim(f_string)))
      f_string(i:i) = ' '
      i = i + 1
    enddo
  else                      ! Fortran to C
    i = 1
    do while(i <= min(len(trim(f_string)), direction-1) )
      temp(i) = f_string(i:i)
      i = i + 1
    enddo
    temp(i) = ACHAR(0)
  endif
end subroutine c_f_string1

! convert between C null terminated string and Fortran character string
! the C string is contained in an array of character(len=1)
! direction == 0 C to Fortran conversion
! direction  > 0 Fortran to C conversion, direction = size of reception array 
! dimension(*) <--> character(len=*)
subroutine c_f_string2(c_string, f_string, direction)
  use ISO_C_BINDING
  implicit none
  character(C_CHAR), dimension(*), intent(INOUT), target :: c_string
  character(len=*), intent(INOUT)  :: f_string
  integer, intent(IN) :: direction    ! 0 if C to Fortran, size of c_string if Fortran to C
  integer :: i

  if(direction == 0) then   ! C to Fortran
    i = 1
    do while(i <= len(trim(f_string)) .and. c_string(i) .ne. ACHAR(0))
      f_string(i:i) = c_string(i)
      i = i + 1
    enddo
    do while(i <= len(trim(f_string)))
      f_string(i:i) = ' '
      i = i + 1
    enddo
  else                      ! Fortran to C
    i = 1
    do while(i <= min(len(trim(f_string)), direction-1) )
      c_string(i) = f_string(i:i)
      i = i + 1
    enddo
    c_string(i) = ACHAR(0)
  endif
end subroutine c_f_string2
