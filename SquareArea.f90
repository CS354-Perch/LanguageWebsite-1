PROGRAM SquareArea
    !Calculates the area of a square after
    !getting the size of each side of the
    !square from user input
    IMPLICIT NONE

    REAL :: Side
    REAL :: Area

    !Gather square sides from user input
    WRITE(*,*) 'Enter the length of a side of the square: '
    READ(*,*) Side

    !Calculate the area of the square
    Area = Side*Side

    !Print results to the console
    WRITE(*,*) 'Square side length =', Side
    WRITE(*,*) 'Area of this square =', Area

END PROGRAM SquareArea
