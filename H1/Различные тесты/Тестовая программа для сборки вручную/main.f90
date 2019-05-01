program example1b
use omp_lib
implicit

!$omp parallel
    write(*,*) "Hello Parallel", omp_get_thread_num()
!$omp end parallel
end
