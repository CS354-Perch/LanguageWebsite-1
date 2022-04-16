!Matt Mecham - CS354
!Module that takes two allocatable arrays,
!Reads from a file that has alternating lines of questions and answers
!Stores the questions in A1 and the answer in A2
MODULE read_quiz

CONTAINS
    SUBROUTINE read_questions(A1, A2)
        IMPLICIT NONE
        CHARACTER*128, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: A1 !Declares that A1 param is an array, is going to be dynamic, and has read/write permissions
        CHARACTER*128, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: A2 !Same as A1
        character*128 :: string !Assumes no line in file is longer than 128 chars. Fortran strings are as bad as C
        INTEGER :: file_id = 1 !Fortran uses integer values to refer to files. Calling it file_id and assigning it 1 makes it more readable
        INTEGER :: read_error = 0 !Fortran uses a flag when reading files for errors. If it encounters an error when reading, it will assign
        INTEGER :: i = 0, num_lines = 0 !Loop variables

        OPEN(file_id,file='dates.txt') !opens the file

        !Get number of lines
        DO WHILE (read_error == 0)
            num_lines = num_lines + 1
            READ(file_id,*,IOSTAT=read_error) string !IOSTAT is the error handling for file reading, * formats to read to the first white space - enough to count.
        END DO
        num_lines = num_lines - 1 !Loops one too many times, apparently.

        ALLOCATE(A1(num_lines/2)) !Will store the questions (half of the size of the file)
        ALLOCATE(A2(num_lines/2)) !Will store the answer (half of the size of the file)
        REWIND(file_id) !rewinds the "scanner", if you will, to the top of the file.

        !This loop reads through our file line by line. If the modulo of i is odd, then it is a question and is stored in A1
        !If modulo of i is even, then it is an answer and stored in A2
        !Since the file is getting split into two arrays, the indices of each are operated on accordingly to make sure that the questoin/answer
        !pairs have the same indices between the two arrays
        DO i = 1, num_lines
            IF (MODULO(i, 2) == 1) THEN
                READ(file_id,'(A)') A1(i/2 + 1) !read(<file id #>, <format>) variable - In this case, '(A)' tells it to read character*n - basically, the end of the line
            ELSE
                READ(file_id,'(A)') A2(i/2)
            END IF
        END DO

        CLOSE(file_id) !close the file

    END SUBROUTINE
END MODULE
