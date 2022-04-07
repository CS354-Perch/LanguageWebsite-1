MODULE TowerOfHanoi
      IMPLICIT NONE
CONTAINS

RECURSIVE SUBROUTINE toh( n, ft, et, at )
      INTEGER, INTENT(IN) :: n
      CHARACTER, INTENT(IN) :: ft
      CHARACTER, INTENT(IN) :: et
      CHARACTER, INTENT(IN) :: at
      INTEGER :: m

      m=n-1

      IF (n<=0) THEN
              return
      END  IF

      CALL toh(m,ft,at,et)
      WRITE(*,*) 'Move disk ',n,' from tower ',ft,' to tower ',et
      CALL toh(m,at,et,ft)

END SUBROUTINE toh

END MODULE TowerOfHanoi
