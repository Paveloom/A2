program main
use Task
implicit none

real(8), allocatable, dimension(:,:) :: A
integer x1, x2, y1, y2, i, j, thr_num, A_size

     A_size = 100
     allocate(A(A_size,A_size))

     do i = 1, A_size; do j = 1, A_size
          
          A(i,j) = i + j
     
     enddo; enddo

     call GetMaxCoordinates(A, x1, y1, x2, y2)

     deallocate(A)

end
