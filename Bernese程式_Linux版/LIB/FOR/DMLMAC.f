      MODULE s_DMLMAC
      CONTAINS

C*
      SUBROUTINE DMLMAC(X,C,Y)
CC
CC NAME       :  DMLMAC
CC
CC PURPOSE    :  MULTIPLIES A 3*3 - MATRIX X WITH A CONSTANT C
CC               (USUALLY USED FOR ROTATIONS IN 3-D SPACE)
CC               Y = (C) * X
CC               X AND Y MAY BE IDENTICAL IN THE CALLING PROGRAM
CC
CC PARAMETERS :
CC         IN :  X      : 3*3 - MATRIX                        R*8(3,3)
CC               C      : CONSTANT                            R*8
CC        OUT :  Y      : 3*3 - MATRIX: Y = C * X             R*8(3,3)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/30 17:45
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I, K
C
      REAL*8 X(3,3),Y(3,3),C
      DO 2 I=1,3
        DO 1 K=1,3
          Y(I,K)=C*X(I,K)
1       CONTINUE
2     CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
