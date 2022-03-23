      MODULE s_TRPSOL
      CONTAINS

C*
      SUBROUTINE TRPSOL(LTRIP,NTRP,ANOR,BNOR,RMS,TRPFLG,PAR,SIGPAR)
CC
CC NAME       :  TRPSOL
CC
CC PURPOSE    :  TRIPLE DIFFERENCE SOLUTION
CC
CC PARAMETERS :
CC         IN :  LTRIP  : FREQUENCY FOR TRIPLE DIFF. SOLUTION I*4
CC                        (L1=1,L2=2,L3=3,L5=5)
CC               NTRP   : NUMBER OF TRIPLE DIFFERENCES        I*4
CC               ANOR(I),I=1,2,..,6: UPPER TRIANGULAR PART    R*8
CC                        OF NORMAL EQUATION MATRIX
CC               BNOR(I),I=1,2,3: RIGHT HAND SIDE OF          R*8
CC                        SYSTEM
CC        OUT :  RMS    : MEAN ERROR OF TRIPLE DIFFERENCE     R*8
CC               TRPFLG : TRIPLE DIFFERENCE SOLUTION FLAG    CH*1
CC                        ='B': BAD SOLUTION
CC                        ='G': GOOD SOLUTION
CC               PAR(I),I=1,2,3: SOLUTION VECTOR              R*8
CC               SIGPAR(I),I=1,2,3: RMS OF SOLUTION VECTOR    R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/05/16 08:19
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               04-FEB-97 : JJ: SET FLAG IF TRP RMS IS TOO HIGH
CC                               HARDWIRED TO 1M!!!! NEED TO MAKE IT A
CC                               PARAMETER IN FUTURE
CC               26-JUN-02 : RD: ADAPT PROGRAM OUTPUT TO ZD CASE
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               10-JUL-12 : RD: USE SYMINVG INSTEAD OF SYMIN8
CC               10-JUL-12 : RD: USE M_BERN WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,  ONLY: LFNPRT, LFNERR
      USE s_syminvg
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I    , IK   , ISING, K    , KK   , LTRIP, N3   , NTRP
C
      REAL*8    RMS
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      REAL*8      ANOR(6),BNOR(3),PAR(3),SIGPAR(3)
      CHARACTER*1 TRPFLG
C
C
C INVERT ANOR
C -----------
      N3=3
      CALL SYMINVG(N3,ANOR,0,ISING)
C
C CHECK WHETHER MATRIX IS SINGULAR
C --------------------------------
      IF(ISING.NE.0)THEN
        WRITE(LFNERR,10)
10      FORMAT(/,' *** SR TRPSOL: NEQ-MATRIX SINGULAR',/)
        CALL EXITRC(2)
      END IF
C
C COMPUTE SOLUTION VECTOR AND UPDATE RMS
C --------------------------------------
      DO 30 I=1,3
        PAR(I)=0.D0
        DO 20 K=1,3
          IF(I.LE.K)THEN
            IK=I+K*(K-1)/2
          ELSE
            IK=K+I*(I-1)/2
          END IF
          PAR(I)=PAR(I)+ANOR(IK)*BNOR(K)
20      CONTINUE
        RMS=RMS-PAR(I)*BNOR(I)
30    CONTINUE
C
C COMPUTE RMS OF SOLUTION, PRINT SOLUTION
C ---------------------------------------
      IF(RMS.GT.0.D0.AND.NTRP.GT.3)THEN
        TRPFLG='G'
        RMS=DSQRT(RMS/(NTRP-3))
        DO 40 K=1,3
          KK=K*(K+1)/2
          SIGPAR(K)=RMS*DSQRT(ANOR(KK))
40      CONTINUE
        WRITE(LFNPRT,50) LTRIP,NTRP,RMS
50      FORMAT(//,1X,72('-'),/,' EPOCH DIFFERENCE SOLUTION',
     1          /,1X,72('-'),/,
     2          /,' FREQUENCY OF EPOCH DIFF. SOLU.:',I12,
     3          /,' #OBS. USED FOR EPOCH DIFF. SOLU:',I12,
     4          /,' RMS OF EPOCH DIFF. SOLUTION (M):',F12.3)
        WRITE(LFNPRT,60) (PAR(K),SIGPAR(K),K=1,3)
60      FORMAT(/,' COORDINATES  NEW-A PRIORI X (M):',F12.3,' +-',F12.3,
     1         /,'                           Y (M):',F12.3,' +-',F12.3,
     2         /,'                           Z (M):',F12.3,' +-',F12.3,
     3        //,1X,72('-'))
C
C SPECIAL CHECK OF RMS (HARDWIRED!!!!) SHOULD CHANGE IN FUTURE
C ------------------------------------------------------------
        IF (RMS.GE.1.D0) THEN
          TRPFLG='B'
          WRITE(LFNERR,65) RMS,1.D0
65        FORMAT(' RMS OF ',F12.3,'TOO HIGH: CUT OFF = ',F12.3)
        ENDIF
C
      ELSE
        TRPFLG='B'
        IF(NTRP.LE.3)THEN
          WRITE(LFNERR,70) NTRP
70        FORMAT(' NOT ENOUGH OBS. (',I1,') FOR COMPUTATION OF RMS',/,
     1           ' RMS := 1 CM',/)
        ELSE IF(RMS.LT.0.D0)THEN
          WRITE(LFNERR,80) RMS
80        FORMAT(' NEGATIVE SUM OF RES. SQUARES=',D20.6,/,
     1           ' RMS := 1 CM',/)
          RMS=.01D0
          DO 41 K=1,3
            KK=K*(K+1)/2
            SIGPAR(K)=RMS*DSQRT(ANOR(KK))
41        CONTINUE
          WRITE(LFNPRT,50) LTRIP,NTRP,RMS
          WRITE(LFNPRT,60) (PAR(K),SIGPAR(K),K=1,3)
        END IF
        RMS=.01D0
      END IF
C
      RETURN
      END SUBROUTINE

      END MODULE
