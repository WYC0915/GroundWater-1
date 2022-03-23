      MODULE s_UPDCOE
      CONTAINS

C*
      SUBROUTINE UPDCOE(N,Q1N,D,H,FACT,MAT,FT0K,YSTART,YCOE)
CC
CC NAME        : UPDCOE
CC
CC PURPOSE     : COMPUTE NEW COEFFICIENTS YCOE ACCORDING TO
CC                  YCOE = MAT*FT0K
CC                  PLUS INITIAL CONDITIONS YSTART
CC               UPDCOE IS AN AUXILIARY SUBROUTINE FOR THE
CC               SR PACKAGE FOR NUMERICAL INTEGRATION DEVELOPED
CC               WINTER 89/90
CC
CC
CC PARAMETERS  :
CC          IN : N      : ORDER OF DEQ SYSTEM                   I*4
CC               Q1N    : Q1N = Q + 1 - N                       I*4
CC                        Q: ORDER OF APPROXIMATION
CC               D      : DIMENSION OF SYSTEM                   I*4
CC               H      : NORMALIZATION CONSTANT                R*8
CC               FACT(K),K=1,2,..,Q+1: FACTORIALS               R*8
CC               MAT(I,K),I=1,..Q+1-N,K=1,..,Q+1-N              R*8
CC                        COEFFICIENT MATRIX
CC               FT0K(K,I),K=1,..,D,I=1,2,..,Q+1-N: RIGHT HAND  R*8
CC                        SIDES OF DEQ SYSTEM
CC               YSTART(K,I),K=1,..,D,I=1,..,N: INITIAL COND'S  R*8
CC         OUT : YCOE(K,I),K=1,..,D, I=1,..,Q+1: COEFF. MATRIX  R*8
CC
CC RESTRICTIONS: NONE
CC
CC AUTHOR      : G. BEUTLER
CC               ASTRONOMICAL INSTITUTE, UNIVERSITY OF BERN
CC               SWITZERLAND
CC
CC CREATED     : 1990 JAN 24
CC
CC CHANGES     : 23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , K     , L     , N     , NPOINT
C
      REAL*8    FACI  , H     , HN
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
      INTEGER*4 Q1N,D
      REAL*8    FACT(*),MAT(Q1N,*),FT0K(D,*),YSTART(D,*),YCOE(D,*)
C
C COMPUTE SOLUTION VECTORS
      NPOINT=Q1N
      HN=H**N
      DO 250 I=1,NPOINT
        DO 240 K=1,D
          YCOE(K,N+I)=0.D0
          DO 240 L=1,NPOINT
            YCOE(K,N+I)=YCOE(K,N+I)+MAT(I,L)*FT0K(K,L)
240      CONTINUE
          FACI=HN*FACT(I)/FACT(I+N)
        DO 245 K=1,D
          YCOE(K,N+I)=YCOE(K,N+I)*FACI
245     CONTINUE
250   CONTINUE
C
C TERMS 1-N :
      DO 260 I=1,N
      DO 260 K=1,D
        YCOE(K,I)=YSTART(K,I)/FACT(I)*H**(I-1)
260   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
