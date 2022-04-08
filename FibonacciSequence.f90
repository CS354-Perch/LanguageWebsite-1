! Jake Beslanowitch
! Calcuates the Fibonacci Sequence up until the 45th number
PROGRAM FibonacciSequence
    IMPLICIT NONE
    INTEGER :: X, Y, TEMP, ITR
    X = 0 ! start with base case 
    Y = 1 ! start with base case 
    WRITE (*,*) X ! write the value of x
    WRITE (*,*) Y ! write the value of y
    ! iterate until 45
    DO ITR = 1, 45, 1
          TEMP = X + Y
          X = Y
          Y = TEMP
          WRITE (*,*) TEMP
       END DO
END PROGRAM FibonacciSequence