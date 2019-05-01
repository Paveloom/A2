module Task
use fgsl
implicit none

contains

function GetSplineValue(x, x_array, y_array)

real(fgsl_double), dimension(:), intent(in) :: x_array, y_array
real(fgsl_double), intent(in) :: x
real(fgsl_double) :: GetSplineValue
    
integer(fgsl_size_t) n
integer(fgsl_int) :: i, status

! Note: first = last for periodic data

    type(fgsl_interp_accel) :: acc
    type(fgsl_spline) :: spline
  
    n = size(x_array)

    spline =  fgsl_spline_alloc(fgsl_interp_cspline_periodic, n)
  
    write(6, '(''#m=0,S=5'')')
    
    do i = 1, n
     write(6, '(2(F10.5,1X))') x_array(i), y_array(i)
    end do
    
  write(6, '(''#m=1,S=0'')')
  status = fgsl_spline_init(spline, x_array, y_array)
  
     GetSplineValue = fgsl_spline_eval(spline, x, acc)
     write(6, '(2(F10.5,1X))') x, GetSplineValue
     
  call fgsl_spline_free (spline)
  call fgsl_interp_accel_free (acc)
    
  end function  

end module
