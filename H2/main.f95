program main
use fgsl
use Task ! Модуль с процедурами
implicit none

real(fgsl_double), allocatable, dimension(:) :: x_array, y_array
real(fgsl_double) x

allocate(x_array(4), y_array(4))

x = 0
x_array = 1
y_array = 2

write(*,*) GetSplineValue(x,x_array,y_array)

deallocate(x_array,y_array)

end
