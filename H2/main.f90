program main
use Task ! Модуль с процедурами
implicit none

real(fgsl_double), allocatable, dimension(:) :: x_array, y_array
real(fgsl_double) x
integer n

read(*,*) n
read(*,*) x

allocate(x_array(n), y_array(n))

read(*,*) x_array
read(*,*) y_array

write(*,*) GetSplineValue(x,x_array,y_array)

deallocate(x_array,y_array)

end
