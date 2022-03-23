      MODULE s_LAGINT
      CONTAINS

C*
      SUBROUTINE LAGINT (X,Y,xint,yout)
CC
CC NAME       :  LAGINT
CC
CC PURPOSE    :  ADAPTED FROM IERS ROUTINE IN FILE INTERP.f
CC
CC               THIS SUBROUTINE PERFORMS LAGRANGIAN INTERPOLATION
CC               WITHIN A SET OF (X,Y) PAIRS TO GIVE THE Y
CC               VALUE CORRESPONDING TO XINT. THIS PROGRAM USES A
CC               WINDOW OF 4 DATA POINTS TO PERFORM THE INTERPOLATION.
CC               IF THE WINDOW SIZE NEEDS TO BE CHANGED, THIS CAN BE
CC               DONE BY CHANGING THE INDICES IN THE DO LOOPS FOR
CC               VARIABLES M AND J.
CC
CC PARAMETER  :
CC        IN  :  X      : ARRAY OF VALUES OF THE              R*8(:)
CC                        INDEPENDENT VARIABLE
CC               Y      : ARRAY OF FUNCTION VALUES            R*8(:)
CC                        CORRESPONDING TO X
CC               XINT   : THE X-VALUE FOR WHICH THE           R*8
CC                        ESTIMATE OF Y IS DESIRED
CC       OUT  :  YOUT   : THE Y VALUE RETURNED TO CALLER      R*8
CC
CC REMARKS    :
CC
CC AUTHOR     :  CODED BY CH. BIZOUARD (OBSERVATOIRE DE PARIS) - NOVEMBER 2002
CC
CC CREATED    :  21-JAN-06
CC
CC CHANGES    :
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      2006     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE M_BERN
      IMPLICIT NONE
      REAL(r8b),DIMENSION(:) :: X,Y
      REAL(r8b)              :: xint,yout,term
      INTEGER(i4b)           :: N,I,J,K,M
C
      N    = SIZE (X)
      YOUT = 0.0D0
      DO I = 1,N-1
        IF ( xint .GE. X(I) .AND. xint .LT. X(I+1) ) K = I
      ENDDO
      IF ( K .LT. 2 ) K = 2
      IF ( K .GT. N-2 ) K = N-2
      DO M = K-1,K+2
        TERM = Y(M)
        DO J = K-1,K+2
          IF ( M .NE. J ) THEN
            TERM = TERM * (XINT - X(J))/(X(M) - X(J))
          END IF
        ENDDO
        YOUT = YOUT + TERM
      ENDDO
      RETURN
      END SUBROUTINE

      END MODULE
