module c_sched_affinity
  use ISO_C_BINDING
  implicit none
  private
  public :: CPU_SET_T, c_sched_getaffinity, c_sched_setaffinity
  public :: affinitymask_to_cpu, cpu_to_affinitymask, GetCpuAffinity, GetActiveCores
#include <iso_c_binding_extras.hf>
  interface
    function GetCpuAffinity(cpumask, string, stringsize) result(nc) bind(C,name='GetCpuAffinity')
      import :: C_INT, CPU_SET_T, C_CHAR
      implicit none
      type(CPU_SET_T), intent(OUT) :: cpumask
      character(C_CHAR), dimension(*), intent(OUT) :: string
      integer(C_INT), intent(IN), value :: stringsize
      integer(C_INT) :: nc
    end function GetCpuAffinity
    function GetActiveCores() result(ncores) bind(C,name='GetActiveCores')
      import :: C_INT
      implicit none
      integer(C_INT) :: ncores
    end function GetActiveCores
  end interface

 contains
  subroutine affinitymask_to_cpu(cpumask, cpus, ncpu)     ! translate affinity mask into cpu array
    implicit none
    type(CPU_SET_T), intent(IN), target       :: cpumask  ! affinity mask
    integer, intent(IN), value                :: ncpu     ! size of cpus table
    integer, dimension(0:ncpu-1), intent(OUT) :: cpus     ! table to receive affinity bit for cpus 0 -> ncpu-1

    integer(C_INT) :: c
    integer :: i, j

    do i = 0, min( size(cpumask%set) , ncpu/32 ) - 1      ! do not exceed cpus size nor cpu set size
      c = cpumask%set(i)
      do j = 0, 31
        cpus(i*32 + j) = and(c,1)                         ! copy affinity bit (LSB) into cpus table 
        c = ishft(c, -1)                                  ! next bit into LSB 
      enddo
    enddo
  end subroutine affinitymask_to_cpu

  subroutine cpu_to_affinitymask(cpumask, cpus, ncpu)  ! add to affinity mask from cpu list
    implicit none
    type(CPU_SET_T), intent(INOUT), target :: cpumask  ! affinity mask
    integer, intent(IN), value             :: ncpu     ! size of cpus array
    integer, dimension(ncpu), intent(IN)   :: cpus     ! list of cpus to add to affinity mask

    integer(C_INT32_T) :: c
    integer :: i, j, k

    do k = 1, ncpu
      if(cpus(k) < 0) cycle                    ! invalid CPU number, ignore
      i = cpus(k) / 32                         ! index into integer array
      if(i >= size(cpumask%set)) cycle         ! would overflow cpu set, ignore
      j = mod(cpus(k),32)                      ! bit position into integer
      c = ishft(1, j)                          ! get 1 at right position
      cpumask%set(i) = ior(cpumask%set(i), c)  ! set proper bit at proper position
    enddo
  end subroutine cpu_to_affinitymask
end module

#if defined(SELF_TEST)
program cpu_set
  use ISO_C_BINDING
  use c_sched_affinity
  implicit none
  type(CPU_SET_T), target :: cpumask
  integer, dimension(0:255) :: cpu
  integer(C_SIZE_T) :: sz
  integer :: i, j, status
  character(len=1024) :: string

  print *,'ncores =',GetActiveCores()

  cpumask%set = 0
  cpu = 0
  call affinitymask_to_cpu(cpumask, cpu, size(cpu))
  print 2,cpu(0:127)

  sz = size(cpumask%set)
  status = c_sched_getaffinity(0, sz, cpumask)
  cpu = 0
  call affinitymask_to_cpu(cpumask, cpu, size(cpu))
  print 2,cpu(0:127)

  cpumask%set = 0  ! zero out mask
  call cpu_to_affinitymask(cpumask, [0, 3], 2 )  ! add cpu 0 + cpu 3 to  affinity mask
  status = c_sched_setaffinity(0, sz, cpumask)
  print *,'status set =',status

  status = c_sched_getaffinity(0, sz, cpumask)
  print *,'status get =',status
  cpu = 0
  call affinitymask_to_cpu(cpumask, cpu, size(cpu))
  print 2,cpu(0:127)
2 format(128b1.1)

  status = GetCpuAffinity(cpumask, string, len(string))
  print *,'nc =',status,", '"//string(1:status)//"'"
end program
#endif

