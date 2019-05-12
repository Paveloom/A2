program main
use Task
implicit none

     ! Входные и выходные данные
     real(8), allocatable, dimension(:,:) :: A
     integer(4) x1, x2, y1, y2
     
     ! Вспомогательные переменные
     integer(4) i, j, ier
     
     ! Переменные для деления первого подпространства итераций на порции
     integer(4) A_size                    ! Длина стороны квадратной матрицы
     integer(4) A_partion_size            ! Размер порции
     integer(4) A_partion_size_mod        ! Остаток от деления A_size на mpiSize
     integer(4) A_partion_shift           ! Сдвиг порции от начала в зависимости от ранга
     integer(4) A_leftbound, A_rightbound ! Границы индексов для данного ранга
     
     ! Вспомогательные переменные при обмене сообщениями
     integer(4) send_leftbound, send_rightbound ! Границы индексов для данного ранга при ранге i
     
     ! Вспомогательные переменные MPI
     integer(4) mpiErr, mpiSize, mpiRank
     integer(4) ierr, status

     ! Указание на размер квадратной матрицы
     A_size = 1001
          
     allocate(A(A_size,A_size), stat = ier); if (ier .ne. 0) stop 'Не могу выделить память для массива A'
     
     call mpi_init(mpiErr)
        
     call mpi_comm_size(MPI_COMM_WORLD, mpiSize, mpiErr)
     call mpi_comm_rank(MPI_COMM_WORLD, mpiRank, mpiErr)
          
     ! Вычисление размера порции
     A_partion_size_mod = mod(A_size,mpiSize)
               
     if (A_partion_size_mod .eq. 0) then
               
          A_partion_size = A_size / mpiSize
          A_partion_shift = mpiRank * A_partion_size
               
     elseif (mpiRank .eq. mpiSize - 1) then
     
          A_partion_size = (A_size + (mpiSize - A_partion_size_mod)) / mpiSize - (mpiSize - A_partion_size_mod)
          A_partion_shift = mpiRank * (A_partion_size + (mpiSize - A_partion_size_mod))
     
     else
               
          A_partion_size = (A_size + (mpiSize - A_partion_size_mod)) / mpiSize
          A_partion_shift = mpiRank * A_partion_size
          
     endif
     
     ! Заполнение процесcом своей порции
     A_leftbound = 1 + A_partion_shift
     A_rightbound = A_partion_size + A_partion_shift
     
     do i = A_leftbound, A_rightbound, 1
     do j = 1, A_size, 1
        
          A(i,j) = i + j
     
     enddo;enddo
     
     ! Передача всех порций процессу 0
           
     if (mpiRank .gt. 0) then
     
          call mpi_send(A(A_leftbound:A_rightbound,:), A_partion_size*A_size, MPI_DOUBLE_PRECISION, 0, mpiRank, MPI_COMM_WORLD, ierr)
     
     else
     
          do i = 1, mpiSize - 1
          
               send_leftbound = 1 + i * A_partion_size
               
               if (i .eq. mpiSize - 1 .and. A_partion_size_mod .ne. 0) then
               
                    send_rightbound = A_partion_size + i * A_partion_size - (mpiSize - A_partion_size_mod)
               
               else
               
                    send_rightbound = A_partion_size + i * A_partion_size
               
               endif
               
               call mpi_recv(A(send_leftbound:send_rightbound,:), A_partion_size*A_size, MPI_DOUBLE_PRECISION, i, i, MPI_COMM_WORLD, status, ierr)
     
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
