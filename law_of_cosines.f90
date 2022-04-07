!Matt Mecham - CS354
program law_of_cosine
    implicit none !Required to override implicit typing from the olden days

    REAL, PARAMETER :: PI = 3.141592 !Declaration of a Constant i.e Static variable
    REAL s1, s2, radian, degree, s3 !Variables used to calculate length of third side of a triangle

    PRINT *, "This program uses the law of cosine to find the length of a side of a triangle."
    PRINT*, "Please enter the lengths of two sides: "
    READ*, s1, s2
    PRINT*, "Now the angle in between in decimal radians: "
    READ*, radian
    degree = radian * (180/PI)
    s3 = sqrt(s1**2 + s2**2 - 2*s1*s2*cos(radian)) !Notice that sqrt, power, and trig are all implicit functions of Fortran
    PRINT*, "The angle between in degrees is: ", degree, "and the length of the third side is", s3, "units long"
end program
