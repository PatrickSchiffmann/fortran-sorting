INTEGER (C_INT) FUNCTION insertionsort(N, X) BIND(C)
    USE ISO_C_BINDING
    USE ModuleInsertionsort

    INTEGER (C_INT), VALUE :: N
    INTEGER (C_INT), DIMENSION(N) :: X
    
    CALL insertionsort_int(N, X)
    insertionsort = N
END FUNCTION

INTEGER (C_INT) FUNCTION mergesort(N, X) BIND(C)
    USE ISO_C_BINDING
    USE ModuleMergesort

    INTEGER (C_INT), VALUE :: N
    INTEGER (C_INT), DIMENSION(N) :: X
    
    CALL mergesort_int(N, X)
    mergesort = N
END FUNCTION

INTEGER (C_INT) FUNCTION quicksort(N, X) BIND(C)
    USE ISO_C_BINDING
    USE ModuleQuicksort

    INTEGER (C_INT), VALUE :: N
    INTEGER (C_INT), DIMENSION(N) :: X
    
    CALL quicksort_int(N, X)
    quicksort = N
END FUNCTION

