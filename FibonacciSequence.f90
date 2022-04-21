! Jake Beslanowitch
! Calcuates the Fibonacci Sequence up until the 45th number
! 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, and so on
PROGRAM FibonacciSequence
    IMPLICIT NONE
    INTEGER :: X, Y, TEMP, ITR
    X = 0 ! start with base case 
    Y = 1 ! start with base case 
    WRITE (*,*) X ! write the value of x
    WRITE (*,*) Y ! write the value of y
    ! do: initial value, final value, increment value
    ! if no increment is set then default to 1
    DO ITR = 1, 45, 1       ! iterate until 45
          TEMP = X + Y      ! create temp variable with base cases 
          X = Y             ! assign right val with left val
          Y = TEMP          ! right val is now the value of tmp
          WRITE (*,*) TEMP
       END DO
END PROGRAM FibonacciSequence