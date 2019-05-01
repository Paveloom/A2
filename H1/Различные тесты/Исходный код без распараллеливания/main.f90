program main
use Homework
implicit none

real(8) A(1000,1000)
real(8) sum
integer x1, x2, y1, y2, i, j

do i=1,1000; do j=1,1000
A(i,j)=i+j
enddo; enddo

call FindMaxCoordinates(A, x1, y1, x2, y2)

write(*,*) ' '
write(*,*) "x1 = ", x1
write(*,*) "y1 = ", y1
write(*,*) "x2 = ", x2
write(*,*) "y2 = ", y2
write(*,*) ' '

end
