program main
    USE QuickAlg !Tells compiler to use this module. Kind of like a .h file
    IMPLICIT NONE !Overrides implicit datatypes from the olden days
    INTEGER, DIMENSION(10) :: A = (/ 10, 12, 1, 5, 8, 7, 13, 15, 3, 2 /) !initializes an array of size 10 - Arrays start at 1.
    INTEGER :: pivot = 1, array_end = 10, i = 0 !declare and initialize vars
    INTEGER k

    CALL quickSort(A, pivot, array_end, i) !Calls subroutine from the QuickAlg module

    DO k = 1, 10 !Loop - Notice how arrays start at 1. What monsters....
        PRINT*, A(k) !Print contents of array after sort
    END DO

end program
