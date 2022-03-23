      MODULE s_DMLMAM
      CONTAINS

C*
      SUBROUTINE DMLMAM(A,B,C)
CC
CC NAME       :  DMLMAM
CC
CC PURPOSE    :  MULTIPLIES TWO 3*3 - MATRICES
CC               (USUALLY USED FOR ROTATIONS IN 3-D SPACE)
CC               C = A * B
CC               A AND C MAY BE IDENTICAL IN THE CALLING PROGRAM
CC
CC PARAMETERS :
CC         IN :  A      : 3*3 - MATRIX                        R*8(3,3)
CC               B      : 3*3 - MATRIX                        R*8(3,3)
CC        OUT :  C      : 3*3 - MATRIX : PRODUCT A*B          R*8(3,3)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/30 17:49
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
      INTEGER*4 I, J, K
C
      REAL*8 A(3,3),B(3,3),C(3,3),D(3,3)
      DO 3 I=1,3
        DO 2 J=1,3
          D(I,J)=0.D0
          DO 1 K=1,3
            D(I,J)=D(I,J)+A(I,K)*B(K,J)
1         CONTINUE
2       CONTINUE
3     CONTINUE
      DO 5 I=1,3
        DO 4 J=1,3
          C(I,J)=D(I,J)
4       CONTINUE
5     CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
