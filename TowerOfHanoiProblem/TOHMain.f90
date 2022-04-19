PROGRAM TOHMain
      USE TowerOfHanoi
      IMPLICIT NONE

      INTEGER :: n

      WRITE(*,*) 'Enter number of disks: '
      READ(*,*) n

      CALL toh(n,'A','C','B')
END PROGRAM TOHMain
