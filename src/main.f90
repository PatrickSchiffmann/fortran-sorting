module ModuleSort
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

    integer, parameter :: INSERTION_THRESHOLD = 16

contains
    subroutine sort_int(N, X)
        implicit none
        
        integer, intent(in) :: N
        integer, dimension(N), intent(inout) :: X
        
        integer, allocatable, dimension(:) :: permutation
        
        !call insertionsort_int(N, X)
        !call mergesort_int(N, X)
        !call quicksort_int(N, X)

        allocate(permutation(N))
        call sort_create_permutation(N, X, permutation)
        call sort_apply_permutation(N, X, permutation, .true.)
    end subroutine


    subroutine sort_create_permutation_int(N, X, permutation)
        implicit none
        
        integer, intent(in) :: N
        integer, dimension(N), intent(in) :: X
        integer, dimension(N), intent(out) :: permutation
        
        integer, dimension(:), allocatable :: copy
        integer :: i

        allocate(copy(N))
        copy = X

        do i = 1, N
            permutation(i) = i
        end do
        
        !call insertionsort_kv_int(N, copy, permutation)
        !call mergesort_kv_int(N, copy, permutation)
        call quicksort_kv_int(N, copy, permutation)
    end subroutine

    subroutine sort_apply_permutation_int(N, values, permutation, asc_opt)
        implicit none
        
        integer, intent(in) :: N
        integer, dimension(N), intent(inout) :: values
        integer, dimension(N), intent(in) :: permutation
        logical, intent(in), optional :: asc_opt
        
        integer, dimension(:), allocatable :: reordered
        integer :: i
        logical :: asc
        
        if (present(asc_opt)) then
            asc = asc_opt
        else
            asc = .true.
        end if
        
        allocate(reordered(N))
        do i = 1, N
            if (asc) then
                reordered(i) = values(permutation(i))
            else
                reordered(N + 1 - i) = values(permutation(i))
            end if
        end do
        values = reordered
    end subroutine
    
    pure logical function is_sorted_int(N, X)
        implicit none

        integer, intent(in) :: N
        integer, dimension(N), intent(in) :: X
        
        integer :: i
        
        is_sorted_int = .false.
        do i = 1, N-1
            if (X(i) > X(i+1)) return
        end do

        is_sorted_int = .true.
    end function

    subroutine insertionsort_int(N, X)
        implicit none
        
        integer, intent(in) :: N
        integer, dimension(N), intent(inout) :: X
        
        integer :: i, j, cur
        
        do i = 2, N
            cur = X(i)
            j = i-1
            do while(j >= 1 .and. X(j) > cur)
                X(j+1) = X(j)
                j = j - 1
            end do
            X(j+1) = cur
        end do
    end subroutine

    subroutine insertionsort_kv_int(N, key, val)
        implicit none
        
        integer, intent(in) :: N
        integer, dimension(N), intent(inout) :: key, val
        
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

    recursive subroutine mergesort_int(N, X)
        implicit none
        
        integer, intent(in) :: N
        integer, dimension(N), intent(inout) :: X
        
        integer :: mid

        if (N <= INSERTION_THRESHOLD) then
            call insertionsort_int(N, X)
            return
        endif

        mid = N/2
        call mergesort_int(mid, X)
        call mergesort_int(N-mid, X(mid+1))
        call mergesort_merge_int(N, mid, X)
    end subroutine
    
    recursive subroutine mergesort_kv_int(N, key, val)
        implicit none
        
        integer, intent(in) :: N
        integer, dimension(N), intent(inout) :: key, val
        
        integer :: mid
        
        if (N <= INSERTION_THRESHOLD) then
            call insertionsort_kv_int(N, key, val)
            return
        endif

        mid = N/2
        call mergesort_kv_int(mid, key, val)
        call mergesort_kv_int(N-mid, key(mid+1), val(mid+1))
        call mergesort_kv_merge_int(N, mid, key, val)
    end subroutine
    
    subroutine mergesort_merge_int(N, mid, X)
        implicit none
        
        integer, intent(in) :: N, mid
        integer, dimension(N), intent(inout) :: X

        integer, dimension(:), allocatable :: sorted
        integer :: i, j, k, cnt

        allocate(sorted(N))
        i = 1
        j = mid + 1
        k = 1
        
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

    subroutine mergesort_kv_merge_int(N, mid, key, val)
        implicit none
        
        integer, intent(in) :: N, mid
        integer, dimension(N), intent(inout) :: key, val

        integer, dimension(:), allocatable :: sorted_key, sorted_val
        integer :: i, j, k, cnt

        allocate(sorted_key(N), sorted_val(N))
        i = 1
        j = mid + 1
        k = 1

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
    
    recursive subroutine quicksort_int(N, X)
        implicit none
        
        integer, intent(in) :: N
        integer, dimension(N), intent(inout) :: X
        
        integer :: mid

        if (N <= INSERTION_THRESHOLD) then
            call insertionsort_int(N, X)    
            return
        endif
        
        call quicksort_partition_int(N, X, mid)
        call quicksort_int(mid, X)
        call quicksort_int(N-mid, X(mid+1))
    end subroutine

    recursive subroutine quicksort_kv_int(N, key, val)
        implicit none
        
        integer, intent(in) :: N
        integer, dimension(N), intent(inout) :: key, val
        
        integer :: mid
        
        if (N <= INSERTION_THRESHOLD) then
            call insertionsort_kv_int(N, key, val)    
            return
        endif
        
        call quicksort_partition_kv_int(N, key,val, mid)
        call quicksort_kv_int(mid, key, val)
        call quicksort_kv_int(N-mid, key(mid+1), val(mid+1))
    end subroutine

    subroutine quicksort_partition_int(N, X, mid)
        implicit none
        
        integer, intent(in) :: N
        integer, intent(out) :: mid
        integer, dimension(N), intent(inout) :: X

        integer :: pivot, i, j

        pivot = quicksort_pivot_int(N, X)
        i = 1
        j = N

        do
            do while (X(i) < pivot)
                i = i + 1
            end do

            do while (X(j) > pivot)
                j = j - 1
            end do
            
            if ( i >= j ) then
                mid = j
                return
            end if

            call swap(X(i), X(j))
            i = i + 1
            j = j - 1
        end do
    end subroutine

    subroutine quicksort_partition_kv_int(N, key, val, mid)
        implicit none
        
        integer, intent(in) :: N
        integer, intent(out) :: mid
        integer, dimension(N), intent(inout) :: key, val

        integer :: pivot, i, j

        pivot = quicksort_pivot_int(N, key)
        i = 1
        j = N

        do
            do while (key(i) < pivot)
                i = i + 1
            end do

            do while (key(j) > pivot)
                j = j - 1
            end do
            
            if ( i >= j ) then
                mid = j
                return
            end if

            call swap(key(i), key(j))
            call swap(val(i), val(j))
            i = i + 1
            j = j - 1
        end do
    end subroutine

    pure integer function quicksort_pivot_int(N, X)
        implicit none
        
        integer, intent(in) :: N
        integer, dimension(N), intent(in) :: X
        
        integer :: a, b, c

        if (N >= 3 .or. INSERTION_THRESHOLD >= 3) then
            a = X(1)
            b = X(N/2)
            c = X(N)
            
            if (a <= b .and. b <= c) then
                quicksort_pivot_int = b
            elseif (b <= a .and. a <= c) then
                quicksort_pivot_int = a
            else
                quicksort_pivot_int = c
            end if
        else
           quicksort_pivot_int = X(1)
        end if
    end function
            

    subroutine swap(x, y)
        integer, intent(inout) :: x, y
        integer :: tmp
        tmp = x
        x = y
        y =  tmp
    end subroutine
end module

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

