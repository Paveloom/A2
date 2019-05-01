module IntegrateModule
use :: omp_lib
implicit none

contains
    real(8) function ParallelIntegrate(f, a, b, eps)
        interface
            real(8) function f(x)
                real(8) :: x
            end function
        end interface
        real(8) :: a, b, eps
        !переменные подзадач
        real(8) :: task_a, task_b, task_result, task_step

!$omp parallel private(task_a, task_b, task_result, task_step) firstprivate(a, b, eps) reduction(+: ParallelIntegrate)
        !пределы интегрирования подзадачи
        task_step = (b - a) / omp_get_num_threads()
        task_a = a + omp_get_thread_num() * task_step
        task_b = task_a + task_step
        !интегрирование подзадачи
        ParallelIntegrate = Integrate(f, task_a, task_b, eps)
!$omp end parallel
    end function
    
    real(8) function ParallelDoIntegrate(f, a, b, eps)
        interface
            real(8) function f(x)
                real(8) :: x
            end function
        end interface
        real(8) :: a, b, eps
        integer(4) :: i, steps_count

        steps_count = (b - a) / eps

        !$omp parallel firstprivate(a, b, eps, steps_count) private(i) reduction(+: ParallelDoIntegrate)
        ParallelDoIntegrate = 0d0
        !$omp do
        do i=1, steps_count
            ParallelDoIntegrate = ParallelDoIntegrate + f(eps * i)
        enddo
        !$omp end do
        !$omp end parallel
        ParallelDoIntegrate = ParallelDoIntegrate * eps
    end function

    real(8) function ParallelDoIntegrateWithoutParallel(f, a, b, eps)
        interface
            real(8) function f(x)
                real(8) :: x
            end function
        end interface
        real(8) :: a, b, eps
        integer(4) :: i, steps_count

        steps_count = (b - a) / eps

        ParallelDoIntegrateWithoutParallel = 0d0

        do i=1, steps_count
            ParallelDoIntegrateWithoutParallel = ParallelDoIntegrateWithoutParallel + f(eps * i)
        enddo

        ParallelDoIntegrateWithoutParallel = ParallelDoIntegrateWithoutParallel * eps
    end function

    real(8) function Integrate(f, a, b, eps)
        interface
            real(8) function f(x)
                real(8) :: x
            end function
        end interface
        real(8) :: a, b, eps, current

        current = a
        Integrate = 0d0
        do while (current < b)
            Integrate = Integrate + f(current)
            current = current + eps
        enddo
        Integrate = Integrate * eps
    end function
    
    real(8) function SomeFunction(x)
        real(8) :: x
        SomeFunction = dsin(x)
    end function
end module

program OpenMP
  use :: omp_lib
  use :: IntegrateModule
  implicit none
  call omp_set_num_threads(2)
  !write(*,*) Integrate(SomeFunction, 0d0, 3.1415926545d0*0.5, 1d-7)
  !write(*,*) ParallelIntegrate(SomeFunction, 0d0, 3.1415926545d0*0.5, 1d-7)
  write(*,*) ParallelDoIntegrate(SomeFunction, 0d0, 3.1415926545d0*0.5, 1d-7)
  !write(*,*) ParallelDoIntegrateWithoutParallel(SomeFunction, 0d0, 3.1415926545d0*0.5, 1d-7)
end program

