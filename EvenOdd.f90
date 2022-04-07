PROGRAM EvenOdd
    IMPLICIT NONE

    INTEGER :: userNum
    INTEGER :: n

    WRITE(*,*) 'Enter your number: '
    READ(*,*)  userNum

    n = MOD(userNum,2)

    IF (n==1) THEN
        WRITE(*,*) 'Your number is odd!'
    ELSE
        WRITE(*,*) 'Your number is even!'
    END IF

END PROGRAM EvenOdd
