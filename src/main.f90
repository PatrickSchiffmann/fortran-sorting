module modsort
    implicit none
contains
    subroutine sort(N, X)
        implicit none
        integer :: N
        integer, dimension(N) :: X

    end subroutine

    logical function is_sorted(N, X)
        implicit none
        integer :: N
        integer, dimension(N) :: X
        integer :: i
        
        is_sorted = .false.
        do i = 1, N-1
            if (X(i) > X(i+1)) RETURN
        end do

        is_sorted = .true.

    end function
end module

program main
    use modsort, only: sort, is_sorted
    call test1
contains
    subroutine test1
        integer, dimension(5) :: d1 = (/1, 3, 5, 2, 4/)
        call sort(5, d1)
        WRITE(*,*) is_sorted(5, d1), d1
    end subroutine

end program

