      MODULE s_SOLVE
      CONTAINS

C*
      SUBROUTINE SOLVE(NP,A,B,X)
CC
CC NAME       :  SOLVE
CC
CC PURPOSE    :  COMPUTE X=A*B
CC               WHERE   A IS A (COLUMN-WISE) LINEARIZED NP*NP - MATRIX
CC                         (UPPER TRIANGLE ONLY)
CC                       B IS A VECTOR WITH NP ELEMENTS
CC
CC PARAMETERS :
CC         IN :  NP     : DIMENSION OF MATRICES A,B,X         I*4
CC               A(I),I=1,2,...,NP*(NP+1)/2 COLUMNS-WISE      R*8
CC                        LINEARIZED MATRIX A
CC                        (UPPER TRIANGLE ONLY)
CC               B(I),I=1,2,...,NP: VECTOR B                  R*8
CC        OUT :  X(I),I=1,2,...,NP: RESULT OF A*B             R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/03 11:30
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE f_ikf
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I  , K  , NP
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 A(*),B(*),X(*)
      DO 10 I=1,NP
      X(I)=0
      DO 10 K=1,NP
      X(I)=X(I)+A(IKF(I,K))*B(K)
10    CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
