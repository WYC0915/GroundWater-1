      MODULE s_SETMAT
      CONTAINS

C*
      SUBROUTINE SETMAT(ITYP,Q,TL,TR,TREL,MAT,TIMMAT)
CC
CC NAME       :  SETMAT
CC
CC PURPOSE    :  DEFINE MATRIX FOR NUMERICAL INTEGRATION
CC
CC PARAMETERS :
CC         IN :  ITYP   : PROBLEM TYPE                        I*4
CC                    =0: INITIAL VALUE PROBLEM
CC                    =1: BOUNDARY VALUE PROBLEM
CC               TL     : INITIAL EPOCH OR FIRST BOUNDARY     R*8
CC               TR     : SECOND BOUNDARY (NOT USED           R*8
CC                        FOR ITYP=1)
CC               TREL(K),K=1,2,...,Q-1: EPOCHS FOR WHICH      R*8
CC                        DEQ SYSTEM IS SATISFIED
CC        OUT :  MAT(I),I=1,2,....,(Q+1)**2: RESULT MATRIX    R*8
CC               TIMMAT(I),=1,2,..,(Q+1)*Q-1) : COLUMN NR I   R*8
CC                        CONTAINS POWERS OR TREL(1)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/12/28 16:05
CC
CC CHANGES    :  08-MAR-95 : BASE MATRIX MAT ON DIFFERENCES OF
CC                           RHS OF DEQ TO AVOID NUMERICAL PROBLEMS
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE s_dminv
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I    , I0   , IC   , IDIFF, IK   , IK1  , IL   , IND1 ,
     1          IND2 , ITYP , K    , L
C
      REAL*8    DET  , TL   , TR
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
      INTEGER*4 Q,L1(50),L2(50)
      REAL*8 TREL(*),MAT(*),TIMMAT(*)
C
C A. INITIAL VALUE PROBLEM
C    ---------------------
      IF(ITYP.EQ.0)THEN
        MAT(1)=1.D0
        MAT(2)=0.D0
        MAT(2+Q+1)=1.D0
        DO 10 K=2,Q+1
          IK1=1+(K-1)*(Q+1)
          MAT(IK1)=TL**(K-1)
          IF(K.GT.2)MAT(IK1+1)=(K-1)*TL**(K-2)
10      CONTINUE
      ELSE
        MAT(1)=1.D0
        MAT(2)=1.D0
        DO 20 K=2,Q+1
          IK1=1+(K-1)*(Q+1)
          MAT(IK1)  =TL**(K-1)
          MAT(IK1+1)=TR**(K-1)
20      CONTINUE
      END IF
C
C DEFINE REMAINING Q-1 LINES
C --------------------------
      DO 40 I=1,Q-1
        IL=I+2
        MAT(IL)=0.D0
        MAT(IL+Q+1)=0.D0
        MAT(IL+2*(Q+1))=2.D0
        DO 30 K=4,Q+1
          IK=IL+(K-1)*(Q+1)
          MAT(IK)=(K-1)*(K-2)*TREL(I)**(K-3)
30      CONTINUE
40    CONTINUE
C
C FORM DIFFERENCES
C ----------------
      DO 90 IC=1,Q+1
        DO 80 IDIFF=1,Q-2
          DO 70 L=Q+1,3+IDIFF,-1
            IND1=L+(IC-1)*(Q+1)
            IND2=L-1+(IC-1)*(Q+1)
            MAT(IND1)=MAT(IND1)-MAT(IND2)
            IF(DABS(MAT(IND1)).LT.1.D-11)THEN
              MAT(IND1)=0.D0
            END IF
70        CONTINUE
80      CONTINUE
90    CONTINUE
C
C INVERT MATRIX
C -------------
      CALL DMINV(MAT,Q+1,DET,L1,L2)
C
C DEFINE MATRIX TIMMAT
C --------------------
      DO 100 I=1,Q-1
        I0=(I-1)*(Q+1)
        TIMMAT(I0+1)=1.D0
        DO 100 K=2,Q+1
          TIMMAT(I0+K)=TREL(I)**(K-1)
100   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
