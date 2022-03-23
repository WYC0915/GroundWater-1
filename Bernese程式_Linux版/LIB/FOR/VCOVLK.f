      MODULE s_VCOVLK
      CONTAINS

C*
      SUBROUTINE VCOVLK(NOLD,NNEW,INDEX,QNOR,COE,COV)
CC
CC NAME       :  VCOVLK
CC
CC PURPOSE    : LET:
CC                    QNOR : VARIANCE-COVARIANCE MATRIX OF ADJUSTMENT,
CC                    INDEX(I),I=1,2,..,NOLD CHARACTERIZES THE POSITIONS
CC                          OF NOLD ELEMENTS OF THE ADJUSTMENT
CC                          (KPOS=INDEX(I)=ELEMENT NUMBER IN THE ARRAY
CC                           OF PARAMETERS),
CC                    P THE ARRAY DEFINED BY INDEX,
CC                    PNEW = COE * P : LINEAR COMBINATION OF PARAMETER
CC                          SET P.
CC
CC              THE SUBROUTINE COMPUTES THE VARIANCE COVARIANCE MATRIX
CC              OF THE ARRAY PNEW AS
CC
CC                                  T
CC              COV = C * COV(P) * C
CC              =====================
CC
CC PARAMETERS :
CC        IN  : NOLD    : SIZE OF ARRAY P                           I*4
CC              NNEW    : SIZE OF ARRAY PNEW                        I*4
CC              INDEX   : POSITIONS OF ELEMENTS OF P IN             I*4
CC                        ORIGINAL ARRAY
CC              QNOR    : CO-FACTOR MATRIX OF ADJUSTMENT            R*8(*)
CC              COE     : TRANSITION MATRIX P --> PNEW              R*8(*,*)
CC        OUT : COV     : VARIANCE-COVARIANCE MATRIX OF PNEW        I*4
CC
CC REMARKS    :
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.5  (MAY 94)
CC
CC CREATED    :  94/05/11
CC
CC CHANGES    :  21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               25-SEP-08 : DT: INCREASE MAXELE 1000->2000
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE f_ikf
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IK    , IL    , K     , K1    , L     ,
     1          L1    , LK    , MAXELE, MXCLCQ, NNEW  , NOLD
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
      PARAMETER  (MAXELE=2000)
      CHARACTER*6 MXNLCQ
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      INTEGER*4  INDEX(*)
      REAL*8     QNOR(*),COE(NNEW,NOLD),COV(*)
      REAL*8     HELP(MAXELE)
C
C CHECK MAXIMUM DIMENSION FOR ARRAY HELP
      IF(NNEW*NOLD.GT.MAXELE)THEN
        WRITE(LFNERR,10)MAXELE
10      FORMAT(//,' ** SR VCOVLK : LOCAL DIM MAXELE(',I5,
     1            ') TOO SMALL',//)
        CALL EXITRC(2)
      END IF
C
C HELP = C * COV(P)
C -----------------
      DO 30 I=1,NNEW
        DO 30 K=1,NOLD
          IK=I+(K-1)*NNEW
          HELP(IK)=0.D0
          DO 20 L=1,NOLD
            L1=INDEX(L)
            K1=INDEX(K)
            LK=IKF(L1,K1)
            HELP(IK)=HELP(IK)+COE(I,L)*QNOR(LK)
20        CONTINUE
30    CONTINUE
C
C COV PNEW =  C * COV(P) * TRN(C)
C ------------------------------------
      DO 50 I=1,NNEW
        DO 50 K=1,I
          IK=IKF(I,K)
          COV(IK)=0.D0
          DO 40 L=1,NOLD
            IL=I+(L-1)*NNEW
            COV(IK)=COV(IK)+HELP(IL)*COE(K,L)
40        CONTINUE
50    CONTINUE
999   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
