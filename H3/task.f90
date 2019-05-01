module Task
use mpi
implicit none

contains

subroutine MpiCommSizeAndRank()

    integer(4) :: mpiErr, mpiSize, mpiRank

    call mpi_init(mpiErr)

    call mpi_comm_size(MPI_COMM_WORLD, mpiSize, mpiErr)
    call mpi_comm_rank(MPI_COMM_WORLD, mpiRank, mpiErr)
    write(*,*) "MPI_COMM_WORLD Size and Rank:", mpiSize, mpiRank

    call mpi_finalize(mpiErr)
    
end subroutine


end module
