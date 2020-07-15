module Task
use mpi
implicit none

contains

     subroutine GetMaxCoordinates(A, x1, y1, x2, y2) ! Процедура нахождения координат максимальной подматрицы
     implicit none
        
     ! Входные и выходные данные
     real(8), dimension(:,:), intent(in) :: A
     integer(4), intent(out) :: x1, y1, x2, y2
     
     ! Переменные для работы алгоритма Кадане
     integer(4) L, R, Up, Down
     integer(4) n, m, tmp
     real(8), allocatable :: current_column(:), B(:,:)
     real(8) current_sum
     real(8) max_sum
     logical transpos
     
     ! Вспомогательные переменные
     integer ier
        
     ! Вспомогательные переменные MPI
     integer(4) mpiErr, mpiSize, mpiRank
     integer(4) ierr
     
     ! Переменные для определения результата
     real(8), allocatable, dimension(:) :: max_sum_array ! Максимальное значение max_sum и ранг процесса, который его нашёл
     integer(4) max_rank ! Ранг процесса, который нашел максимальное значение max_sum
     
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

     call mpi_comm_size(MPI_COMM_WORLD, mpiSize, mpiErr)
     call mpi_comm_rank(MPI_COMM_WORLD, mpiRank, mpiErr)

     max_sum = B(1,1)
     x1 = 1
     y1 = 1
     x2 = 1
     y2 = 1

     ! Работа процесса над своей порцией 
     
     allocate(current_column(m), stat = ier)
     if (ier .ne. 0) stop 'Не могу выделить память для массива current_column'  
               
     do L = mpiRank + 1, n, mpiSize
          current_column = B(:, L)
          do R = L, n
               if (R .gt. L) then 
                    current_column = current_column + B(:, R)
               endif
                
               call FindMaxInArray(current_column, current_sum, Up, Down) 
                      
               if (current_sum .gt. max_sum) then
                    max_sum = current_sum
                    x1 = Up
                    x2 = Down
                    y1 = L
                    y2 = R
               endif
          end do
     end do
        
     deallocate(current_column)
       
     ! Определение ранга процесса с максимальным значением max_sum
     
     allocate(max_sum_array(mpiSize), stat = ier)
     if (ier .ne. 0) stop 'Не могу выделить память для массива max_sum_array'
     
     call mpi_gather(max_sum, 1, MPI_REAL8, max_sum_array, 1, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
     
     if (mpiRank .eq. 0) then
          max_rank = maxloc(max_sum_array, dim = 1)
          max_rank = max_rank - 1
     endif
     
     call mpi_bcast(max_rank, 1, MPI_INTEGER4, 0, MPI_COMM_WORLD, ierr)
     
     deallocate(max_sum_array)
     
     ! Сообщение другим процессам результатов
     
     call mpi_bcast(x1, 1, MPI_REAL8, max_rank, MPI_COMM_WORLD, ierr)
     call mpi_bcast(y1, 1, MPI_REAL8, max_rank, MPI_COMM_WORLD, ierr)
     call mpi_bcast(x2, 1, MPI_REAL8, max_rank, MPI_COMM_WORLD, ierr)
     call mpi_bcast(y2, 1, MPI_REAL8, max_rank, MPI_COMM_WORLD, ierr)
                                                                       
                                                                       
     if (transpos) then  
          tmp = x1
          x1 = y1
          y1 = tmp
    
          tmp = y2
          y2 = x2
          x2 = tmp
     endif

     end subroutine


     subroutine FindMaxInArray(a, ans, Up, Down) ! Процедура поиска максимального подотрезка в массиве
     implicit none
        
     real(8), intent(in), dimension(:) :: a
     integer(4), intent(out) :: Up, Down
     real(8), intent(out) :: ans
     real(8) cur, sum, min_sum
     integer(4) min_pos, i

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

end module
