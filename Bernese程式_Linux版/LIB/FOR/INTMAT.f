      MODULE s_INTMAT
      CONTAINS

C*
      SUBROUTINE INTMAT(M1,T,MAT)
CC
CC NAME       :  INTMAT
CC
CC PURPOSE    :  THE CONDITION EQUATIONS FOR THE INTERPOLATION
CC               POLYNOMIAL OF DEGREE M OF FUNCTION F(T) MAY
CC               BE WRITTEN AS FOLLOWS:
CC
CC               MAT * P = F
CC                       T
CC               WHERE: P = (P ,P ,...,P )
CC                            0  1      M
CC                       T   ARE THE POLYNOMIAL COEFFICIENTS
CC                      F = (F(T ),....,F(T   ))
CC                           ARE THE FUNCTION VALUES AT THE
CC                           INTERPOLATION POINTS
CC                      MAT IS THE CORRESPONDING COEFFICIENT MATRIX
CC                                           -1
CC               THIS SUBROUTINE COMPUTES MAT
CC               M1 = M + 1
CC
CC PARAMETERS :
CC         IN :  M1     : POLYNOMIAL DEGREE + 1               I*4
CC               T(I),I=1,2,..,M+1: INTERPOLATION POINTS      R*8
CC        OUT :  MAT    : RESULTING MATRIX                    R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.1
CC
CC CREATED    :  89/12/05 12:35
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_dminv
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I   , K   , M1  , MAXM
C
      REAL*8    DET , TI  , TIK
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
      PARAMETER (MAXM=20)
      REAL*8 MAT(M1,*),T(*)
      INTEGER*4 L1(MAXM),L2(MAXM)
C
C CHECK MAXIMUM DIMENSIONS
C ------------------------
      IF(M1.GT.MAXM)THEN
        WRITE(LFNERR,1)M1,MAXM
1       FORMAT(/,' *** SR INTMAT: ACTUAL SIZE OF MATRIX',I5,/,
     1            '               GT MAX DIMENSION     ',I5,/)
        CALL EXITRC(2)
      END IF
C
C DEFINE COEFFICIENT MATRIX
C -------------------------
      DO 100 I=1,M1
        MAT(I,1)=1.D0
        IF(DABS(T(I)).LT.1.D-5)THEN
          DO 10 K=2,M1
            MAT(I,K)=0.D0
10        CONTINUE
        ELSE
          TIK=1.D0
          TI=T(I)
          DO 20 K=2,M1
            TIK=TIK*TI
            MAT(I,K)=TIK
20        CONTINUE
        END IF
100   CONTINUE
C
C MATRIX INVERSION
C ----------------
      CALL DMINV(MAT,M1,DET,L1,L2)
      RETURN
      END SUBROUTINE

      END MODULE
