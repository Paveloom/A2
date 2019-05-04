program main
use Task
implicit none

     real(8), allocatable, dimension(:,:) :: A
     integer x1, x2, y1, y2, i, j, A_size
     
     integer ier

     A_size = 1000
     
     allocate(A(A_size,A_size), stat = ier)
     if (ier .ne. 0) stop 'Не могу выделить память для массива A'

     call omp_set_num_threads(4)

     !$omp parallel do private(i,j) shared(A,A_size) default(none) schedule(dynamic)
     
     do i = 1, A_size; do j = 1, A_size
          
          A(i,j) = i + j
     
     enddo; enddo
     
     !$omp end parallel do

     call GetMaxCoordinates(A, x1, y1, x2, y2)
     
     write(*,'(/,4x,a,i6,/,4x,a,i6,/,4x,a,i6,/,4x,a,i6,/)') 'x1   = ', x1, 'y1   = ', y1, 'x2   = ', x2, 'y2   = ', y2 
     
     deallocate(A)

end
