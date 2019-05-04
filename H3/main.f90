program main
use Task
implicit none

     ! Входные и выходные данные
     real(8), allocatable, dimension(:,:) :: A
     integer(4) x1, x2, y1, y2
     
     ! Вспомогательные переменные
     integer(4) i, j
     
     ! Переменные для деления первого подпространства итераций на порции
     integer(4) A_size
     integer(4) A_partion_size
     integer(4) A_partion_size_mod
     integer(4) A_leftbound, A_rightbound ! Границы индексов для данного ранга
     
     ! Вспомогательные переменные MPI
     integer(4) mpiErr, mpiSize, mpiRank
     integer(4) ierr, status

     ! Указание на размер квадратной матрицы
     A_size = 1000
          
     allocate(A(A_size,A_size))
     
     call mpi_init(mpiErr)
        
     call mpi_comm_size(MPI_COMM_WORLD, mpiSize, mpiErr)
     call mpi_comm_rank(MPI_COMM_WORLD, mpiRank, mpiErr)
          
     ! Вычисление размера порции
     A_partion_size_mod = mod(A_size,mpiSize)
               
     if (A_partion_size_mod .eq. 0) then
               
          A_partion_size = A_size / mpiSize
               
     else
               
          A_partion_size = (A_size + (mpiSize - A_partion_size_mod)) / mpiSize
          
     endif
 
     ! Заполнение процесcом своей порции
     A_leftbound = 1 + mpiRank * A_partion_size
     A_rightbound = A_partion_size + mpiRank * A_partion_size
     
     do i = A_leftbound, A_rightbound; 
     
          if (i .gt. A_size) cycle
     
     do j = 1, A_size
        
          A(i,j) = i + j
     
     enddo; enddo
     
     ! Передача всех порций процессу 0
           
     if (mpiRank .gt. 0) then
     
          call mpi_send(A(A_leftbound:A_rightbound,:), A_partion_size*A_size, MPI_DOUBLE_PRECISION, 0, mpiRank, MPI_COMM_WORLD, ierr)
     
     else
     
          do i = 1, mpiSize - 1
               
               call mpi_recv(A(1 + i * A_partion_size:A_partion_size + i * A_partion_size,:), A_partion_size*A_size, MPI_DOUBLE_PRECISION, i, i, MPI_COMM_WORLD, status, ierr)
     
          enddo
          
     endif
     
     ! Передача процессом 0 готовой матрицы A остальным процессам
     call mpi_bcast(A, A_size*A_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)   
     
     ! Вызов процедуры нахождения координат максимальной подматрицы
     call GetMaxCoordinates(A, x1, y1, x2, y2)
     
     call mpi_finalize(mpiErr)
     
     ! Вывод результата
     
     if (mpiRank .eq. mpiSize - 1) then
     
          write(*,'(/,4x,a,i6,/,4x,a,i6,/,4x,a,i6,/,4x,a,i6,/)') 'x1   = ', x1, 'y1   = ', y1, 'x2   = ', x2, 'y2   = ', y2 
     
     endif

     deallocate(A)

end
