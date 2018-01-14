program main
    use ModuleSort, only: sort, is_sorted
    call test_sort_0length
    call test_sort_1length
    call test_sort_2length
    call test_sort_5length 
contains
    subroutine test_sort_0length
        implicit none
        integer, dimension(0) :: d0 
        call sort(0, d0)
        WRITE(*,*) is_sorted(0, d0), d0
    end subroutine

    subroutine test_sort_1length
        implicit none
        integer, dimension(1) :: d0 = (/1/)
        call sort(1, d0)
        WRITE(*,*) is_sorted(1, d0), d0
    end subroutine

    subroutine test_sort_2length
        implicit none
        integer, dimension(2) :: d0 = (/1, 2/)
        integer, dimension(2) :: d1 = (/2, 1/)
        integer, dimension(2) :: d2 = (/-1, -2/)
        integer, dimension(2) :: d3 = (/-2, -1/)
        integer, dimension(2) :: d4 = (/-1, 1/)
        
        call sort(2, d0)
        WRITE(*,*) is_sorted(2, d0), d0
        call sort(2, d1)
        WRITE(*,*) is_sorted(2, d1), d1
        call sort(2, d2)
        WRITE(*,*) is_sorted(2, d2), d2
        call sort(2, d3)
        WRITE(*,*) is_sorted(2, d3), d3
        call sort(2, d4)
        WRITE(*,*) is_sorted(2, d4), d4
    end subroutine

    subroutine test_sort_5length
        implicit none
        integer, dimension(5) :: d0 = (/2, 1, 0, -1, -2/)
        integer, dimension(5) :: d1 = (/0, 0, -42, -42, -42/)
        integer, dimension(5) :: d2 = (/1, 10, 100, 1000, 10000/)
        integer, dimension(5) :: d3 = (/1, 3, 5, 4, 2/)
        integer, dimension(5) :: d4 = (/-1, -3, -5, -4, -2/)
        
        call sort(5, d0)
        WRITE(*,*) is_sorted(5, d0), d0
        call sort(5, d0, .false.)
        WRITE(*,*) is_sorted(5, d0, .false.), d0
        call sort(5, d1)
        WRITE(*,*) is_sorted(5, d1), d1
        call sort(5, d2)
        WRITE(*,*) is_sorted(5, d2), d2
        call sort(5, d3)
        WRITE(*,*) is_sorted(5, d3), d3
        call sort(5, d4)
        WRITE(*,*) is_sorted(5, d4), d4
    end subroutine

end program

