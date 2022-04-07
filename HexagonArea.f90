! Jake Beslanowitch
! HexagonArea Calculator
PROGRAM  HexagonArea
    IMPLICIT  NONE
    
    REAL  :: sideLength, area, input
    DOUBLE PRECISION:: n

    WRITE(*,*)  'Enter one side of the trapezoid : '
    READ(*,*)    sideLength
    input = 3
    n = SQRT(input)
    n = (n * 3)/2
    area = (sideLength**2) * n

    WRITE(*,*)  'Entered values:'
    WRITE(*,*)  'One side  = ', sideLength
    WRITE(*,*)
    WRITE(*,*)  'Area Calculated = ', area

END PROGRAM  HexagonArea