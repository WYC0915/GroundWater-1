      MODULE s_SNGSOL
      CONTAINS

C*
      SUBROUTINE SNGSOL(STANAM,RMSMAX,LSNG  ,NSNG  ,NPAR  ,
     1                  ANOR  ,BNOR  ,IEPOCH,RMS   ,
     2                  SNGFLG,PAR   ,SIGPAR,LINE)
CC
CC NAME       :  SNGSOL
CC
CC PURPOSE    :  SINGLE DIFFERENCE SOLUTION
CC
CC PARAMETERS :
CC         IN :  STANAM(I),I=1,2: STATION NAMES               CH*16
CC               RMSMAX : MAX RMS ALLOWED, OTHERWISE SOL.FLAG R*8
CC               LSNG   : FREQUENCY FOR SINGLE DIFF. SOLUTION I*4
CC                        (L1=1,L2=2,L3=3,L5=5)
CC               NSNG   : NUMBER OF SINGLE DIFFERENCES        I*4
CC               NPAR   : NUMBER OF PARAMETERS                I*4
CC               ANOR(I),I=1,2,..,10: UPPER TRIANGULAR PART   R*8
CC                        OF NORMAL EQUATION MATRIX
CC               BNOR(I),I=1,2,3,4: RIGHT HAND SIDE OF        R*8
CC                        SYSTEM
CC               IEPOCH : EPOCH NUMBER                        I*4
CC        OUT :  RMS    : MEAN ERROR OF SINGLE DIFFERENCE     R*8
CC               SNGFLG : SINGLE DIFFERENCE SOLUTION FLAG    CH*1
CC                        ='G': GOOD SOLUTION
CC                        ='B': BAD SOLUTION
CC                        ='R': TOO HIGH RMS
CC               PAR(I),I=1,2,3,4: SOLUTION VECTOR            R*8
CC               SIGPAR(I),I=1,2,3,4: RMS OF SOLUTION VECTOR  R*8
CC               LINE   : MESSAGES FROM PROCESSING           CH*80
CC
CC REMARKS    :  CHECK RMS HARDWIRED TO 1M!!!!
CC               UP TO 4 PARAMETERS
CC
CC AUTHOR     :  D. SVEHLA, M. ROTHACHER
CC
CC VERSION    :  5.0
CC
CC CREATED    :  03-JAN-01
CC
CC CHANGES    :  09-AUG-02 : RD: ADD BASELINE NAME TO THE WARNING MSG.
CC               14-AUG-02 : RD: "BIG RMS VALUE" IS A PARAMETER
CC               19-AUG-02 : RD: RMS MAY BECOME ZERO
CC               04-NOV-02 : RD: WRITE MESSAGE IN DETSLP NOW
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               10-JUL-12 : RD: USE SYMINVG INSTEAD OF SYMIN8
CC               10-JUL-12 : RD: USE M_BERN WITH ONLY; REMOVE UNUSED PARAMETERS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC               UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,  ONLY: LFNERR
      USE s_syminvg
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IEPOCH, IK    , ISING , K     , KK    , LSNG  ,
     1          NPAR  , NSNG
C
      REAL*8    RMS   , RMSMAX
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      REAL*8       ANOR(10),BNOR(4),PAR(4),SIGPAR(4)
      CHARACTER*1  SNGFLG
      CHARACTER*16 STANAM(2)
      CHARACTER*80 LINE
C
C
C INIT ERROR MESSAGE LINE
C -----------------------
      LINE=' '
C
C INVERT ANOR
C -----------
      CALL SYMINVG(NPAR,ANOR,0,ISING)
C
C CHECK WHETHER MATRIX IS SINGULAR
C --------------------------------
      IF(ISING.NE.0)THEN
        WRITE(LFNERR,10) STANAM(1),TRIM(STANAM(2)),IEPOCH
10      FORMAT(/,' *** SR SNGSOL: NEQ-MATRIX SINGULAR',/,
     1                       16X,'BASELINE:  ',A,1X,A,/,
     2                       16X,'EPOCH:    ',I6/)
        CALL EXITRC(2)
      END IF
C
C COMPUTE SOLUTION VECTOR AND UPDATE RMS
C --------------------------------------
      DO 30 I=1,NPAR
        PAR(I)=0.D0
        DO 20 K=1,NPAR
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
      IF(RMS.GE.0.D0.AND.NSNG.GT.NPAR)THEN
        SNGFLG='G'
        RMS=DSQRT(RMS/(NSNG-NPAR))
        DO 40 K=1,NPAR
          KK=K*(K+1)/2
          SIGPAR(K)=RMS*DSQRT(ANOR(KK))
40      CONTINUE
C
C SPECIAL CHECK OF RMS
C --------------------
        IF (RMS.GE.RMSMAX) THEN
          SNGFLG='R'
C
C GENERATE A MESSAGE LINE
          LINE=' ### SR SNGSOL: EPOCH,BIG RMS OF EPOCH SOL.: '
          WRITE(LINE(46:80),'(I6,F16.3,1X,A)')
     1          IEPOCH,RMS,'(' // STANAM(1)(1:4) // ')'
          IF (DABS(RMS).GE.1D9) WRITE(LINE(52:67),'(E16.6)') RMS
          IF (LEN_TRIM(STANAM(2)).GT.0) THEN
            WRITE(LINE(74:80),'(A)')  '-' // STANAM(2)(1:4) // ')'
          ENDIF
C          WRITE(LFNERR,'(A)') LINE
        ENDIF
C
      ELSE
        SNGFLG='B'
        IF(NSNG.LE.NPAR)THEN
          RMS=.01D0
          SIGPAR(:)=0.D0
C
C GENERATE A MESSAGE LINE: TOO FEW OBS.
          LINE=' ### SR SNGSOL: RMS:=1CM, EPOCH, TOO FEW OBS:'
          WRITE(LINE(46:80),'(I6,I12,5X,A)')
     1          IEPOCH,NSNG,'(' // STANAM(1)(1:4) // ')'
          IF (LEN_TRIM(STANAM(2)).GT.0) THEN
            WRITE(LINE(74:80),'(A)')  '-' // STANAM(2)(1:4) // ')'
          ENDIF
C          WRITE(LFNERR,'(A)') LINE
C
        ELSE IF(RMS.LT.0.D0)THEN
          RMS=.01D0
C
C GENERATE A MESSAGE LINE: NEGATIVE SUM OF SQUARES
          LINE=' ### SR SNGSOL: RMS:=1CM, ' //
     1         'EPOCH, NEG. SUM OF RES. SQUARES:'
          WRITE(LINE(62:80),'(I6,1X,A)')
     1          IEPOCH,'(' // STANAM(1)(1:4) // ')'
          IF (LEN_TRIM(STANAM(2)).GT.0) THEN
            WRITE(LINE(74:80),'(A)')  '-' // STANAM(2)(1:4) // ')'
          ENDIF
C          WRITE(LFNERR,'(A)') LINE
C
          DO 41 K=1,NPAR
            KK=K*(K+1)/2
            SIGPAR(K)=RMS*DSQRT(ANOR(KK))
41        CONTINUE
        ELSE
          RMS=.01D0
C
C GENERATE A MESSAGE LINE: NEGATIVE SUM OF SQUARES
          LINE=' ### SR SNGSOL: RMS:=1CM, ' //
     1         'EPOCH, THE ELSE CASE...:'
          WRITE(LINE(62:80),'(I6,1X,A)')
     1          IEPOCH,'(' // STANAM(1)(1:4) // ')'
          IF (LEN_TRIM(STANAM(2)).GT.0) THEN
            WRITE(LINE(74:80),'(A)')  '-' // STANAM(2)(1:4) // ')'
          ENDIF
C          WRITE(LFNERR,'(A)') LINE
C
        END IF
      END IF
C
      RETURN
      END SUBROUTINE

      END MODULE
