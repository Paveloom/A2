program main
use Task
implicit none

     real(8), allocatable, dimension(:,:) :: A
     integer x1, x2, y1, y2
         
     integer i, j
     
     ! Переменные для деления первого подпространства итераций на порции
     integer A_size
     integer A_partion_size
     integer, allocatable, dimension(:) :: A_partion
     integer, allocatable, dimension(:) :: A_size_array
     
     ! Вспомогательные переменные MPI
     integer(4) mpiErr, mpiSize, mpiRank
     integer ierr

     A_size = 1000
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
               A_partion_size = (A_size + (mpiSize - mod(A_size,mpiSize))) / mpiSize
          
          allocate(A_partion(A_partion_size))
          
          ! Распределение порций
          call mpi_scatter(A_size_array, A_partion_size, MPI_INT, A_partion, A_partion_size, MPI_INT, 0, MPI_COMM_WORLD, ierr)

     ! Заполнение процесcом своей порции
     do i = 1, A_partion_size; do j = 1, A_size
     
          if (A_partion(i) .eq. 0) cycle 
          A(A_partion(i),j) = A_partion(i) + j
     
     enddo; enddo
     
     call mpi_bcast(A, A_size*A_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)

     call mpi_finalize(mpiErr)
     
!     call GetMaxCoordinates(A, x1, y1, x2, y2)
     
!     write(*,*) x1
!     write(*,*) y1
!     write(*,*) x2
!     write(*,*) y2

     deallocate(A, A_size_array, A_partion)

end
