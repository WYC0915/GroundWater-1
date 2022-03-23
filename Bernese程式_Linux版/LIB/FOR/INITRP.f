      MODULE s_INITRP
      CONTAINS

C*
      SUBROUTINE INITRP(NPAR,ICARR,NDIFF,RMSL12,SIGWGS,
     1                  NTRP,RMSTRP,ANOR,BNOR)
CC
CC NAME       :  INITRP
CC
CC PURPOSE    :  INITIALIZE TRIPLE DIFFERENCE SOLUTION USING CARRIER
CC               ICARR.
CC
CC PARAMETERS :
CC         IN :  NPAR   : NUMBER OF PARAMETERS                I*4
CC               ICARR  : CARRIER USED                        I*4
CC               NDIFF  : NUMBER OF DIFFERENCES (ZD=0, SD=1)  I*4
CC               RMSL12 : RMS OF L1 (OR L2) OBSERVATION       R*8(2)
CC               SIGWGS(K),K=1,2,3: A PRIORI SIGMAS OF        R*8
CC                        COORDINATES (IN M)
CC        OUT :  NTRP   : NUMBER OF TRIPLE DIFFERENCES        I*4
CC               RMSTRP : RMS ERROR OF TRIPLE DIFFERENCE      R*8
CC               ANOR(IK),IK=1,2,..,6: UPPER TRIANGULAR PART  R*8
CC                        OF NORMAL EQUATION SYSTEM (COLUMNWISE
CC                        LINEARIZED
CC               BNOR(K),K=1,2,3: RIGHT HAND SIDE OF NEQ-SYST.R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/05/04 08:12
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               19-JUN-02 : RD: OPEN SIZE FOR ANOR/BNOR
CC                               STATION WEIGHTS ONLY FOR COORD-EST
CC               21-JUN-02 : RD/MR: SCALE RMS FOR DIFF. LEVEL
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
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
      INTEGER*4 I     , ICARR , IK    , NDIFF , NPAR  , NTRP
C
      REAL*8    RMS   , RMSTRP
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C     IMPLICIT INTEGER*4 (I-N)
      REAL*8 SIGWGS(3),ANOR(*),BNOR(*),RMSL12(2)
C
C
C NUMBER OF TRIPLE DIFFERENCES, RMS ERROR
C ---------------------------------------
      NTRP=0
      RMSTRP=0.D0
C
C DEFINE MEAN ERROR OF UNIT WEIGHT (A PRIORI)
C -------------------------------------------
      IF(ICARR.LT.3)THEN
        RMS=RMSL12(ICARR)
      ELSE IF(ICARR.EQ.3)THEN
        RMS=3*RMSL12(1)
      ELSE IF(ICARR.EQ.4)THEN
        WRITE(LFNERR,901)
901     FORMAT(/,' *** SR INITRP: ATTEMPT TO MAKE L4 ',
     1                           'TRIPLE DIFF. SOLUTION',/)
        CALL EXITRC(2)
      ELSE
        RMS=5*RMSL12(1)
      END IF
C
C SCALE RMS FOR DIFFERENCE LEVEL
C ------------------------------
      RMS=RMS*DSQRT(2D0)**(NDIFF+2)
C
C INITIALIZE TRIPLE DIFFERENCE SOLUTION
C -------------------------------------
      BNOR(1:NPAR) = 0D0
C
      IK=IKF(NPAR,NPAR)
      ANOR(1:IK) = 0D0
C
      IF (NPAR.GE.3) THEN
        DO I=1,3
          IF (SIGWGS(I).EQ.0D0) CYCLE
          IK=IKF(i,i)
          ANOR(IK)=(RMS/SIGWGS(I))**2
        ENDDO
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
