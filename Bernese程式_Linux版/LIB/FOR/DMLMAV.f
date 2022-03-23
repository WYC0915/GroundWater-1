      MODULE s_DMLMAV
      CONTAINS

C*
      SUBROUTINE DMLMAV(X,T,Y)
CC
CC NAME       :  DMLMAV
CC
CC PURPOSE    :  MULTIPLIES A 3*3 - MATRIX T WITH A VECTOR X :
CC               (USUALLY USED FOR ROTATIONS IN 3-D SPACE)
CC               Y = T * X
CC               X AND Y MAY BE IDENTICAL IN THE CALLING PROGRAM
CC
CC PARAMETERS :
CC         IN :  X      : VECTOR (3 DIM.)                     R*8(3)
CC               T      : MATRIX (3*3)                        R*8(3,3)
CC        OUT :  Y      : VECTOR (3 DIM.) : PRODUCT T*X       R*8(3)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/29 09:22
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
      REAL*8 T(3,*),X(*),Y(*),Z(3)
      DO 1 I=1,3
        Z(I)=0
        DO 1 K=1,3
          Z(I)=Z(I)+T(I,K)*X(K)
1     CONTINUE
      DO 2 I=1,3
        Y(I)=Z(I)
2     CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
