module Task
use omp_lib
implicit none

contains

     subroutine GetMaxCoordinates(A, x1, y1, x2, y2)
     implicit none
        
     ! Входные и выходные данные
     real(8), dimension(:,:), intent(in) :: A
     integer(4), intent(out) :: x1, y1, x2, y2
     
     ! Вспомогательные переменные
     integer ier
        
     ! Переменные для работы алгоритма Кадане
     integer(4) :: n, L, R, Up, Down, m, tmp, thr_id, thr_num, thr_max
     real(8), allocatable :: current_column(:,:), B(:,:)
     real(8) :: current_sum
     logical :: transpos
        
     ! Переменные для определения результата
     real(8), allocatable, dimension(:) :: max_sum   
     integer(4), allocatable, dimension(:):: X_1, X_2, Y_1, Y_2

     m = size(A, dim=1) 
     n = size(A, dim=2) 
     transpos = .FALSE.

     if (m .lt. n) then 
          
          transpos = .TRUE.   
          B = transpose(A)
          m = size(B, dim=1) 
          n = size(B, dim=2) 
               
     else
          
          B = A     
          
     endif

     thr_num = omp_get_max_threads()
     
     ! Выделение памяти под массивы

     allocate(current_column(m,0:thr_num-1), stat = ier)
     if (ier .ne. 0) stop 'Не могу выделить память для массива current_column'
     
     allocate(X_1(0:thr_num-1), stat = ier)
     if (ier .ne. 0) stop 'Не могу выделить память для массива X_1'
     allocate(X_2(0:thr_num-1), stat = ier)
     if (ier .ne. 0) stop 'Не могу выделить память для массива X_2'
     allocate(Y_1(0:thr_num-1), stat = ier)
     if (ier .ne. 0) stop 'Не могу выделить память для массива Y_1'
     allocate(Y_2(0:thr_num-1), stat = ier)
     if (ier .ne. 0) stop 'Не могу выделить память для массива Y_2'
     
     allocate(max_sum(0:thr_num-1), stat = ier)
     if (ier .ne. 0) stop 'Не могу выделить память для массива max_sum'

     max_sum = B(1,1)
     X_1 = 1
     Y_1 = 1
     X_2 = 1
     Y_2 = 1

     !$omp parallel do private(R,L,current_sum,thr_id,Up,Down) shared(B,n,m,current_column,max_sum,X_1,Y_1,X_2,Y_2) default(none) schedule(dynamic)
          
     do L = 1, n        

          thr_id = omp_get_thread_num()
          current_column(:,thr_id) = B(:, L)
          
          do R = L, n
 
               if (R .gt. L) then 
               
                    current_column(:,thr_id) = current_column(:,thr_id) + B(:, R)
                    
               endif
           
               call FindMaxInArray(current_column(:,thr_id), current_sum, Up, Down) 
                 
               if (current_sum > max_sum(thr_id)) then
               
                    max_sum(thr_id) = current_sum
                    X_1(thr_id) = Up
                    X_2(thr_id) = Down
                    Y_1(thr_id) = L
                    Y_2(thr_id) = R
               
               endif
          
          end do
          
     end do
          
     !$omp end parallel do

     thr_max = maxloc(max_sum, dim=1)
          
     x1 = X_1(thr_max-1)
     x2 = X_2(thr_max-1)
     y1 = Y_1(thr_max-1)
     y2 = Y_2(thr_max-1)

     if (transpos) then  
               
          tmp = x1
          x1 = y1
          y1 = tmp
    
          tmp = y2
          y2 = x2
          x2 = tmp
          
     endif

     deallocate(current_column)
     deallocate(max_sum, X_1, X_2, Y_1, Y_2)

     end subroutine


     subroutine FindMaxInArray(a, ans, Up, Down)
     implicit none
     
     real(8), intent(in), dimension(:) :: a
     integer(4), intent(out) :: Up, Down
     real(8), intent(out) :: ans
     real(8) :: cur, sum, min_sum
     integer(4) :: min_pos, i

     ans = a(1)
     Up = 1
     Down = 1
     sum = 0d0
     min_sum = 0d0
     min_pos = 0

     do i = 1, size(a)
               
          sum = sum + a(i)
          cur = sum - min_sum
                    
          if (cur .gt. ans) then
               
               ans = cur
               Up = min_pos + 1
               Down = i
                
          endif
         
          if (sum .lt. min_sum) then
                    
               min_sum = sum
               min_pos = i
                    
          endif

     enddo

     end subroutine FindMaxInArray

end module Task
