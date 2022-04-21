!Matt Mecham - CS354
!Random Number Generator
!Takes in an array, and fills the indices
!with a random, non-duplicate integers in the
!range of array length
MODULE rand_module

IMPLICIT NONE

CONTAINS

    !Routine to populate an array with random ints ranging from 1 to array size
    SUBROUTINE randomize_indices(array)
        INTEGER, DIMENSION(:), INTENT(INOUT) :: array !Declare the parameter to be used and modified within the subroutine
        INTEGER array_size, num, flag, i, j
        REAL r

        array_size = SIZE(array) !Store the size of the array

        DO i = 1, array_size !Iterate over the array
            flag = 1 !Flag to mark the existence of a duplicate in the array - default "true"

            DO WHILE (flag == 1) !Assume there is a duplicate to start the check
                CALL random_number(r) !make a random number in the interval [0,1}
                num = (r * array_size) + 1 !Times it by our size + 1 giving us range [1, size]
                flag = -1 !switch the flag

                DO j = 1, i !Iterate over the array from j to i
                    IF (array(j) == num) THEN !if there is a match
                        flag = 1 !Set the flag again to initiate the while loop and generate a new random value
                        EXIT
                    END IF
                END DO
            END DO

            IF (flag == -1) THEN !Then we have successfully found a non-dupe value
                array(i) = num !Store the number in the array index
            END IF
        END DO
    END SUBROUTINE
END MODULE
