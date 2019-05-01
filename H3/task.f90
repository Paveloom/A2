module Task
use mpi
implicit none

contains

        subroutine GetMaxCoordinates(A, x1, y1, x2, y2)
        implicit none
        
        real(8), dimension(:,:), intent(in) :: A
        integer(4), intent(out) :: x1, y1, x2, y2
        integer(4) :: n, L, R, Up, Down, m, tmp
        real(8), allocatable :: current_column(:), B(:,:)
        real(8) :: current_sum
        logical :: transpos
        real(8) :: max_sum   
        
!        integer(4) mpiErr, mpiSize
!        
!        call mpi_init(mpiErr)
!        
!        call mpi_comm_size(MPI_COMM_WORLD, mpiSize, mpiErr)
!        
!        call mpi_finalize(mpiErr)
!        
!        write(*,*) mpiSize
        
        m = size(A, dim=1) 
        n = size(A, dim=2) 
        transpos = .FALSE.

        if (m < n) then 
            transpos = .TRUE.   
            B = transpose(A)
            m = size(B, dim=1) 
            n = size(B, dim=2) 
        else
            B = A     
        endif

        allocate(current_column(m))

        max_sum=B(1,1)
        x1=1
        y1=1
        x2=1
        y2=1

        do L=1, n        

            current_column = B(:, L)
            do R = L, n
 
                if (R > L) then 
                    current_column = current_column + B(:, R)
                endif
                
                call FindMaxInArray(current_column, current_sum, Up, Down) 
                      
                if (current_sum > max_sum) then
                    max_sum = current_sum
                    x1 = Up
                    x2 = Down
                    y1 = L
                    y2 = R
                    
                endif
            end do
        end do
        
       deallocate(current_column)

        if (transpos) then  
            tmp = x1
            x1 = y1
            y1 = tmp
    
            tmp = y2
            y2 = x2
            x2 = tmp
            endif

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

            do i=1, size(a)
                sum = sum + a(i)
                cur = sum - min_sum
            if (cur > ans) then
                ans = cur
                Up = min_pos + 1
                Down = i
                endif
         
            if (sum < min_sum) then
                min_sum = sum
                min_pos = i
                endif

            enddo

        end subroutine FindMaxInArray


end module
