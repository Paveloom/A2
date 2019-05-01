module Task
use fgsl
implicit none
contains

  function GetSplineValue(x, x_array, y_array)
    real(fgsl_double), dimension(:), intent(in) :: x_array, y_array
    real(fgsl_double), intent(in) :: x
    real(fgsl_double) :: GetSplineValue
    
    GetSplineValue = 10
    
  end function  
  
end module
