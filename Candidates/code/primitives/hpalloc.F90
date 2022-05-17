subroutine hpalloc(addr, nitems, ierr, abort)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(OUT) :: addr         ! memory address of allocated item
  integer, intent(IN) :: nitems            ! number of " words" toallocate
  integer, intent(IN) :: abort             ! if abort == 8, allocate double words
  integer, intent(OUT) :: ierr             ! error code
  interface
    function malloc(size) result(addr) BIND(C,name='malloc')
      import :: C_PTR, C_SIZE_T
      implicit none
      integer(C_SIZE_T), intent(IN), value :: size
      type(C_PTR) :: addr
    end function
  end interface
  integer(C_SIZE_T) :: size
  ierr = 1
  addr = C_NULL_PTR
  size = nitems * 4
  if(abort == 8) size = size * 2
  addr = malloc(size)
  if(C_ASSOCIATED(addr)) ierr = 0
end subroutine

subroutine hpdeallc(addr, ierr, abort)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: addr         ! memory address of allocated item
  integer, intent(IN) :: abort            ! ignored, compatibility argument
  integer, intent(OUT) :: ierr            ! error code
  interface
  subroutine free(addr) BIND(C,name='free')
    import :: C_PTR
    implicit none
    type(C_PTR), intent(IN), value :: addr
  end subroutine
  end interface
  call free(addr)
  ierr = 0
end subroutine

subroutine hpcheck(ierr)                  ! NO-OP now, always successful
  integer, intent(OUT) :: ierr            ! error code
  ierr = 0
end subroutine
