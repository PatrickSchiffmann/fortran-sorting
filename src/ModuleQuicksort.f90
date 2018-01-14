module ModuleQuicksort
    use ModuleInsertionsort

    implicit none
    private
    public quicksort_int, quicksort_kv_int

    integer, parameter :: INSERTIONSORT_THRESHOLD = 4

contains
    
    recursive subroutine quicksort_int(N, X)
        implicit none
        
        integer, intent(in) :: N
        integer, dimension(N), intent(inout) :: X
        
        integer :: mid

        if (N <= INSERTIONSORT_THRESHOLD) then
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
        
        if (N <= INSERTIONSORT_THRESHOLD) then
            call insertionsort_kv_int(N, key, val)    
            return
        endif
        
        call quicksort_partition_kv_int(N, key,val, mid)
        call quicksort_kv_int(mid, key, val)
        call quicksort_kv_int(N-mid, key(mid+1), val(mid+1))
    end subroutine

    subroutine quicksort_partition_int(N, X, mid)
        ! C. A. R. Hoare partition scheme
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

            call swap_int(X(i), X(j))
            i = i + 1
            j = j - 1
        end do
    end subroutine

    subroutine quicksort_partition_kv_int(N, key, val, mid)
        ! C. A. R. Hoare partition scheme
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

            call swap_int(key(i), key(j))
            call swap_int(val(i), val(j))
            i = i + 1
            j = j - 1
        end do
    end subroutine

    pure integer function quicksort_pivot_int(N, X)
        ! Median of first, mid and last element is implemented
        implicit none
        
        integer, intent(in) :: N
        integer, dimension(N), intent(in) :: X
        
        integer :: a, b, c

        ! Comparison against threshold optimizes away the branch
        ! while preserving correctness for lower thresholds
        if (N >= 3 .or. INSERTIONSORT_THRESHOLD >= 3) then
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
            

    pure subroutine swap_int(x, y)
        integer, intent(inout) :: x, y
        integer :: tmp
        tmp = x
        x = y
        y =  tmp
    end subroutine

end module

