module modsort
    implicit none
    private
    public sort, is_sorted, sort_create_permutation, sort_apply_permutation

    interface sort
        module procedure sort_int
    end interface

    interface is_sorted
        module procedure is_sorted_int
    end interface

    interface sort_create_permutation
        module procedure sort_create_permutation_int
    end interface

    interface sort_apply_permutation
        module procedure sort_apply_permutation_int
    end interface

contains
    subroutine sort_int(N, X)
        implicit none
        integer :: N
        integer, dimension(N) :: X
        integer, allocatable, dimension(:) :: permutation, temp
        integer :: i
        allocate(permutation(N))
        
        !call mergesort(N, X)
        !call insertionsort(N, X)
        
        call sort_create_permutation(N, X, permutation)
        call sort_apply_permutation(N, X, permutation)
    end subroutine


    subroutine sort_create_permutation_int(N, X, permutation)
        implicit none
        integer, intent(in) :: N
        integer, dimension(N), intent(in) :: X
        integer, dimension(N), intent(out) :: permutation
        integer, dimension(:), allocatable :: copy
        integer :: i
        
        allocate(copy(N))
        do i = 1, N
            permutation(i) = i
        end do
        
        copy = X
        call insertionsort_kv(N, copy, permutation)
    end subroutine

    subroutine sort_apply_permutation_int(N, values, permutation)
        implicit none
        
        integer, intent(in) :: N
        integer, dimension(N), intent(inout) :: values
        integer, dimension(N), intent(in) :: permutation

        integer, dimension(:), allocatable :: reordered
        integer :: i
        
        allocate(reordered(N))
        do i = 1, N
            reordered(i) = values(permutation(i))
        end do
        values = reordered
    end subroutine
    
    logical function is_sorted_int(N, X)
        implicit none
        integer :: N
        integer, dimension(N) :: X
        integer :: i
        
        is_sorted_int = .false.
        do i = 1, N-1
            if (X(i) > X(i+1)) RETURN
        end do

        is_sorted_int = .true.

    end function

    subroutine insertionsort(N, X)
        implicit none
        integer :: N
        integer, dimension(N) :: X
        integer :: i, j, val
        
        do i = 2, N
            val = X(i)
            j = i-1
            do while(j >= 1 .and. X(j) > val)
                X(j+1) = X(j)
                j = j - 1
            end do
              
            X(j+1) = val
        end do
    end subroutine

    subroutine insertionsort_kv(N, key, val)
        implicit none
        integer :: N
        integer, dimension(N) :: key, val
        integer :: i, j, curkey, curval

        do i = 2, N
            curkey = key(i)
            curval = val(i)
            j = i - 1
            do while(j >= 1 .and. key(j) > curkey)
                val(j+1) = val(j)
                key(j+1) = key(j)
                j = j - 1
            end do
            val(j + 1) = curval
            key(j + 1) = curkey
        end do
    end subroutine

    recursive subroutine mergesort(N, X)
        implicit none
        integer :: N
        integer, dimension(N) :: X
        integer :: mid, tmp
        if (N <= 1) then
            return
        elseif (N == 2) then
            if (X(2) < X(1)) call swap(X(1), X(2))
            return
        endif

        mid = N/2
        call mergesort(mid, X)
        call mergesort(N-mid, X(mid+1))
        call mergesort_merge(N, mid, X)
    end subroutine
    
    recursive subroutine mergesort_kv(N, key, val)
        implicit none
        integer :: N
        integer, dimension(N) :: key, val
        integer :: mid, tmp
        if (N <= 1) then
            return
        elseif (N == 2) then
            if (key(2) < key(1)) then
                call swap(key(1), key(2))
                call swap(val(1), val(2))
            end if
            return
        endif

        mid = N/2
        call mergesort_kv(mid, key, val)
        call mergesort_kv(N-mid, key(mid+1), val(mid+1))
        call mergesort_kv_merge(N, mid, key, val)
    end subroutine
    
    subroutine mergesort_merge(N, mid, X)
        implicit none
        integer :: N, mid
        integer, dimension(N) :: X

        integer, dimension(:), allocatable :: sorted
        integer :: i, j, k, cnt

        allocate(sorted(N))
        i = 1; j = mid+1; k = 1
        
        do while(i <= mid .and. j <= N)
            if(X(i) <= X(j)) then
                sorted(k) = X(i)
                i = i + 1
            else
                sorted(k) = X(j)
                j = j + 1
            end if
            k = k + 1
        end do

        do cnt = i, mid
            sorted(k) = X(i)
            k = k + 1
        end do

        do cnt = j, N
            sorted(k) = X(j)
            k = k + 1
        end do

        X = sorted
    end subroutine

    subroutine mergesort_kv_merge(N, mid, key, val)
        implicit none
        integer :: N, mid
        integer, dimension(N) :: key, val

        integer, dimension(:), allocatable :: sorted_key, sorted_val
        integer :: i, j, k, cnt

        allocate(sorted_key(N), sorted_val(N))
        i = 1; j = mid+1; k = 1

        do while(i <= mid .and. j <= N)
            if(key(i) <= key(j)) then
                sorted_key(k) = key(i)
                sorted_val(k) = val(i)
                i = i + 1
            else
                sorted_key(k) = key(j)
                sorted_val(k) = val(j)
                j = j + 1
            end if
            k = k + 1
        end do

        do cnt = i, mid
            sorted_key(k) = key(i)
            sorted_val(k) = val(i)
            k = k + 1
        end do

        do cnt = j, N
            sorted_key(k) = key(j)
            sorted_val(k) = val(j)
            k = k + 1
        end do

        key = sorted_key
        val = sorted_val
    end subroutine

    subroutine swap(x, y)
        integer, intent(inout) :: x, y
        integer :: tmp
        tmp = x
        x = y
        y =  tmp
    end subroutine
end module

program main
    use modsort, only: sort, is_sorted
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

