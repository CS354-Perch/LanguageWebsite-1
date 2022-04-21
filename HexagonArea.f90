! Jake Beslanowitch
! HexagonArea Calculator
PROGRAM  HexagonArea
    ! all variables starting with i, j, k, l, m and n, if
    ! not declared, are of the INTEGER type by default.
    IMPLICIT  NONE
    REAL  :: sideLength, area, input
    ! works the same as real
    DOUBLE PRECISION:: n
    ! enter the size of one side
    WRITE(*,*)  'Enter one side of the trapezoid : '
    ! second asterisk makes the read statement list-directed.
    READ(*,*)    sideLength
    ! ((3 * SQRT(3)) / 2) * a^2
    input = 3
    n = SQRT(input)
    n = (n * 3)/2
    area = (sideLength**2) * n

    ! Write will send items to a numbered unit, which can be a
    screen
    WRITE(*,*)  'Entered values:'
    WRITE(*,*)  'One side  = ', sideLength
    WRITE(*,*)
    WRITE(*,*)  'Area Calculated = ', area

END PROGRAM  HexagonArea