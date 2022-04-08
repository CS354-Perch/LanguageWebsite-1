PROGRAM IntFactorial
! Program computes factorial of integer gathered from user input
! @author Samuel Wasko

    IMPLICIT NONE
    INTEGER :: m = 0
    WRITE(*,*)  'Enter integer to compute factorial : '
    READ(*,*)   m
    WRITE(*,*)
    PRINT *, 'Factorial Computed --->',m,'! = ', factorial(m)

CONTAINS

    ! factorial function recursively calls itself decrementing down one integer at a time
    ! to computer an integer factorial
    RECURSIVE FUNCTION factorial(m) result (val)

        IMPLICIT NONE

        INTEGER, INTENT(in) :: m    ! intent(in) tells compiler variable comes from system in
        INTEGER :: val

        IF( m == 0 ) THEN
            val = 1
        ELSE
            val = m * factorial(m - 1)
        END IF

    END FUNCTION

END PROGRAM IntFactorial