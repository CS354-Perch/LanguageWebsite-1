!Matt Mecham CS354
!Test main for rando_module
PROGRAM randomNumGen
    USE rand_module
    IMPLICIT NONE

    INTEGER, DIMENSION(20) :: A !Change the size of the array to test the module
    INTEGER i

    CALL randomize_indices(A)
    
    DO i = 1, SIZE(A) 
        PRINT*, A(i) !Print out the contents to ensure random with no dupes
    END DO
END PROGRAM
