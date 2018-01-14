!> ModuleSort
!! Typesafe sorting routines for integers and reals
!! Can sort 1d arrays by value, and create permutation array
!! from one array, and apply to N other arrays.
!!
!! Implemented algorithms
!! Insertionsort O(N^2) time with O(1) space
!! Mergesort O(N log N) time with O(N) space and O(lg N) allocations
!! Quicksort O(N log N) time with O(1) space and no allocations
!! The key value variants _kv add O(N) space and O(1) allocations
!! Merge- and Quicksort fall back to Insertionsort for small inputs
!! Remember the worst case time complexity for quicksort is O(N^2)
!!
!! Sample use
!! use ModuleSort, only: sort, sort_create_permutation, sort_apply_permutation
!! 1. Sorting an array
!! integer, dimension(N) :: data = ...
!! call sort(N, data)
!! 
!! 2. Sorting one array, by another
!! integer, dimension(N) :: keys, values, permutation
!! call sort_create_permutation(N, keys, values, permutation)
!! call sort_apply_permutation(N, key, values, permutation)
!!
!! 3. Reverse sorting
!! sort and sort_apply_permutation have an addition optional logical
!! argument at the end, which reverses the sorting direction. It is
!! possible to create a permutation and apply it both for ascneding
!! and descending sorts.

module ModuleSort
    use ModuleInsertionsort
    use ModuleQuicksort
    use ModuleMergesort

    implicit none
    private
    public sort, sort_create_permutation, sort_apply_permutation
    public is_sorted ! for testing

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

    integer, parameter :: INSERTIONSORT_THRESHOLD = 2


contains
    
    subroutine sort_int(N, X, asc_opt)
        implicit none
        
        integer, intent(in) :: N
        integer, dimension(N), intent(inout) :: X
        logical, intent(in), optional :: asc_opt

        logical :: asc
        integer, allocatable, dimension(:) :: permutation
        
        if (present(asc_opt)) then
            asc = asc_opt
        else
            asc = .true.
        end if

        if (asc) then
            !call insertionsort_int(N, X)
            !call mergesort_int(N, X)
            call quicksort_int(N, X)
        else
            allocate(permutation(N))
            call sort_create_permutation(N, X, permutation)
            call sort_apply_permutation(N, X, permutation, asc)
        end if
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
    
    pure logical function is_sorted_int(N, X, asc_opt)
        implicit none

        integer, intent(in) :: N
        integer, dimension(N), intent(in) :: X
        logical, intent(in), optional :: asc_opt

        logical :: asc
        integer :: i
        
        if (present(asc_opt)) then
            asc = asc_opt
        else
            asc = .true.
        end if
       
        is_sorted_int = .false.
        if (asc) then
            do i = 1, N-1
                if (X(i) > X(i+1)) return
            end do
        else
            do i = 1, N-1
                if (X(i) < X(i+1)) return
            end do
        end if
        is_sorted_int = .true.
    end function

end module

