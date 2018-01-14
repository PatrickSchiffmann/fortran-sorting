module ModuleMergesort
    use ModuleInsertionsort

    implicit none
    private
    public mergesort_int, mergesort_kv_int

    integer, parameter :: INSERTIONSORT_THRESHOLD = 16

contains
    
    recursive subroutine mergesort_int(N, X)
        implicit none
        
        integer, intent(in) :: N
        integer, dimension(N), intent(inout) :: X
        
        integer :: mid

        if (N <= INSERTIONSORT_THRESHOLD) then
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
        
        if (N <= INSERTIONSORT_THRESHOLD) then
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

end module

