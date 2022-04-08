PROGRAM  TrapezoidArea
!   Program computes the area of a trapezoid
!   after gathering three real dimensions from user input
!   @author Samuel Wasko
    IMPLICIT  NONE

    REAL  :: Base1, Base2, Height
    REAL  :: Area, t

    ! gather dimensions from user input
    WRITE(*,*)  'Enter height of trapezoid : '
    READ(*,*)   Height
    WRITE(*,*)  'Enter 1st base length of trapezoid : '
    READ(*,*)   Base1
    WRITE(*,*)  'Enter 2nd base length of trapezoid : '
    READ(*,*)   Base2

    ! use temp variables to compute area
    t      = (Base1+Base2)/2
    Area   = Height * t * 1

    ! write statements for computation output
    WRITE(*,*)  'Entered values:'
    WRITE(*,*)  'Height = ', Height
    WRITE(*,*)  'Base1 = ', Base1
    WRITE(*,*)  'Base2 = ', Base2
    WRITE(*,*)
    WRITE(*,*)  'Area Calculated = ', Area

END PROGRAM  TrapezoidArea
