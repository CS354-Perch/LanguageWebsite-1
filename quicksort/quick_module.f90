!Matt Mecham -CS 354
!Quicksort algorithm
!Turn to page 170 of your CS321 book for the finer details
!Interesting note: a function has parameters and returns one thing
!However, subroutines parameters can be specified with intent. Thought C++ was bad? Sheesh
MODULE QuickAlg

contains
    RECURSIVE SUBROUTINE quickSort(array, pivot, array_end, i) !Have to declare subroutine to be recursive
        INTEGER, DIMENSION(:), INTENT(INOUT) :: array !These intents specify what is intended with the parameters. Essentially, read/write/both
        INTEGER, INTENT(IN) :: pivot
        INTEGER, INTENT(IN) :: array_end
        INTEGER, INTENT(INOUT) :: i

        IF (pivot < array_end) THEN !IF then block. Very familiar to our grammar for the interpreter assignment
            CALL partition(array, pivot, array_end, i) !Subroutine calls. Finds the "middle"
            CALL quickSort(array, pivot, i - 1, i) !one half of the array
            CALL quickSort(array, i + 1, array_end, i) !the other half
        END IF

    END SUBROUTINE

    SUBROUTINE partition(array, pivot, array_end, i) !Subroutine to start partitioning a little sorted subarray
        INTEGER, DIMENSION(:), INTENT(INOUT) :: array
        INTEGER, INTENT(IN) :: pivot
        INTEGER, INTENT(IN) :: array_end
        INTEGER, INTENT(INOUT) :: i
        INTEGER x, j

        x = array(array_end)
        i = pivot - 1
        DO j = pivot, (array_end - 1)
            IF (array(j) <= x) THEN
                i = i + 1
                CALL exchange(array(i), array(j)) !Where the magic happens. First unsorted item gets swapped, thus sorting
            END IF
        END DO
        CALL exchange(array(i + 1), array(array_end)) !
        i = i + 1 !Have to return
    END SUBROUTINE

    SUBROUTINE exchange (x , y) !pretty basic subroutine to the values of two values in an array
        INTEGER, INTENT(INOUT) :: x, y !Notice that I just pass the integer in the index, not the whole array. This is accomplished with the inout.
        INTEGER :: temp

        temp = y
        y = x
        x = temp

    END SUBROUTINE
END MODULE
