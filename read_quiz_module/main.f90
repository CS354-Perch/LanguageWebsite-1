Matt Mecham CS354
!Test main for random_module and read_quiz
PROGRAM randomNumGen
    USE rand_module
    USE read_quiz

    IMPLICIT NONE

    INTEGER, DIMENSION(15) :: A3 !Change the size of the array to test the module
    CHARACTER*128, DIMENSION(:), ALLOCATABLE :: A1 !This is how you declare an array that can be dynamically allocated
    CHARACTER*128, DIMENSION(:), ALLOCATABLE :: A2 !Same as above
    INTEGER i

    CALL randomize_indices(A3) !Call to param
    CALL read_questions(A1, A2)

    DO i = 1,size(A1,1)
        WRITE(*,*) trim(A1(i)) !Prints the array - trim is an intrinsic function that removes trailing empty chars
    END DO

    DO i = 1,size(A2,1)
        WRITE(*,*) trim(A2(i)) !Prints the array - trim is an intrinsic function that removes trailing empty chars
    END DO

    DO i = 1, SIZE(A3)
        WRITE(*,*) A3(i) !Print out the contents to ensure random with no dupes
    END DO
END PROGRAM
