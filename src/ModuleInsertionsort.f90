module ModuleInsertionsort
    implicit none

    private
    public insertionsort_int, insertionsort_kv_int

contains
    
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

end module

