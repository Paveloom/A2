module Task ! Модуль с процедурами
use fgsl ! Подключение библиотеки FGSL
implicit none

contains

function GetSplineValue(x, x_array, y_array) ! Интерполяция сплайнами

real(fgsl_double), dimension(:), intent(in) :: x_array, y_array
real(fgsl_double), intent(in) :: x
real(fgsl_double) :: GetSplineValue
    
integer(fgsl_size_t) n
integer(fgsl_int) status

     type(fgsl_interp_accel) :: acc
     type(fgsl_spline) :: spline
  
     n = size(x_array)

     spline =  fgsl_spline_alloc(fgsl_interp_cspline_periodic, n)
    
     status = fgsl_spline_init(spline, x_array, y_array)
  
          GetSplineValue = fgsl_spline_eval(spline, x, acc)
     
     call fgsl_spline_free (spline)
     call fgsl_interp_accel_free (acc)
    
end function  

end module
