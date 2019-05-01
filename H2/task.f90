module Task
use fgsl
implicit none
contains

function GetSplineValue(x, x_array, y_array)
    real(fgsl_double), dimension(:), intent(in) :: x_array, y_array
    real(fgsl_double), intent(in) :: x
    real(fgsl_double) :: GetSplineValue
    
  integer(fgsl_size_t), parameter :: n = 4
  integer(fgsl_int) :: i, status
  real(fgsl_double) :: xi, yi
!      Note: first = last for periodic data
  type(fgsl_interp_accel) :: acc
  type(fgsl_spline) :: spline

  spline =  fgsl_spline_alloc(fgsl_interp_cspline_periodic, n)
  write(6, '(''#m=0,S=5'')')
  do i=1,n
     write(6, '(2(F10.5,1X))') x_array(i), y_array(i)
  end do
  write(6, '(''#m=1,S=0'')')
  status = fgsl_spline_init(spline, x_array, y_array)
  do i=1, 100
     xi = (1.D0 - dble(i-1)/100.D0) * x_array(1) + dble(i-1)/100.D0 * x_array(n)
     yi = fgsl_spline_eval(spline, xi, acc)
     write(6, '(2(F10.5,1X))') xi, yi
  end do
  call fgsl_spline_free (spline)
  call fgsl_interp_accel_free (acc)
    
  end function  

end module
