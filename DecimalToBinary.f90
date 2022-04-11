PROGRAM DecimalToBinary
    !Program that converts a decimal number to a binary number
    !@author Sawyer Ball

    IMPLICIT NONE

    INTEGER :: number
    Character(len = 20) :: string, answer
    
    !Get binary number from user input
    WRITE(*,*) 'Enter a decimal number: '
    READ(*,*) number
    WRITE(*,*)
    WRITE(*,*) 'Entered decimal number: ', number
    answer = convert(number, string)
    PRINT *, 'Converted to binary: ', answer

CONTAINS

    !Function to convert a decimal number into a binary number
    RECURSIVE FUNCTION convert(number, string) result (converted)

        IMPLICIT NONE

        INTEGER, INTENT(in) :: number
        Character(len = 20), INTENT(in) :: string
        Character(len = 20) :: converted

        if (number == 0) then
            converted = string
        else 
            if (mod(number, 2) == 1) then
                converted = '1'//trim(string)
                converted = convert(number/2, converted)
            else 
                converted = '0'//trim(string)
                converted = convert(number/2, converted)
            end if
        end if

    END FUNCTION

END PROGRAM DecimalToBinary
