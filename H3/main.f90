program main
use Task
implicit none

     real(8), allocatable, dimension(:,:) :: A
     integer x1, x2, y1, y2
         
     integer i, j
     
     ! Переменные для деления первого подпространства итераций на порции
     integer A_size
     integer A_partion_size
     integer A_partion_size_mod
     integer, allocatable, dimension(:) :: A_size_array
     integer A_LB, A_RB ! Границы индексов для данного ранга
     
     ! Вспомогательные переменные MPI
     integer(4) mpiErr, mpiSize, mpiRank
     integer ierr, status

     A_size = 10000
     allocate(A_size_array(A_size))
          
          ! Заполнение индексами первого подпространства итераций 
          do i = 1, A_size
               A_size_array(i) = i
          enddo
          
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
     A_LB = 1 + mpiRank * A_partion_size
     A_RB = A_partion_size + mpiRank * A_partion_size
     
     do i = A_LB, A_RB; do j = 1, A_size
     
          if (i .gt. A_size) cycle
          A(i,j) = i + j
     
     enddo; enddo
     
     ! Передача всех порций процессу 0
           
     if (mpiRank .gt. 0) then
     
          call mpi_send(A(A_LB:A_RB,:), A_partion_size*A_size, MPI_DOUBLE_PRECISION, 0, mpiRank, MPI_COMM_WORLD, ierr)
     
     else
     
          do i = 1, mpiSize - 1
               
               call mpi_recv(A(1 + i * A_partion_size:A_partion_size + i * A_partion_size,:), A_partion_size*A_size, MPI_DOUBLE_PRECISION, i, i, MPI_COMM_WORLD, status, ierr)
     
          enddo
          
     endif
     
     ! Передача процессом 0 готовой матрицы A остальным процессам
     call mpi_bcast(A, A_size*A_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)   
     
     call mpi_finalize(mpiErr)
     
!     call GetMaxCoordinates(A, x1, y1, x2, y2)
     
!     write(*,*) x1
!     write(*,*) y1
!     write(*,*) x2
!     write(*,*) y2

     deallocate(A, A_size_array)

end
