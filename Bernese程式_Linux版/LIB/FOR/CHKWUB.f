      MODULE s_CHKWUB
      CONTAINS

C*
      SUBROUTINE CHKWUB(FILRNX,OBSTIM,MAXSLP,RMSMAX,MINOBS,SLPMIN,
     1                  SLPOUT,IPOLY ,NOBS  ,SVN   ,LINTIM,LINOBS,
     2                  LINFLG,NBAD  ,ARCOFF,ARCRMS,NSLIP ,SLPTIM,
     3                  SLPCYC)
CC
CC NAME       :  CHKWUB
CC
CC PURPOSE    :  CHECKS AN OBSERVATION ARC FOR JUMPS AND OUTLIERS
CC
CC PARAMETERS :
CC        IN  :  FILRNX : NAME OF THE RINEX FILE FOR MESSAGE  CH*32
CC               OBSTIM : BEGIN OF THE ARC                    R*8
CC               RMSMAX : THRESHOLD RMS VALUE (L5 CYCLES)     R*8
CC               MINOBS : MINIMAL NUMBER OF OBSERVATIONS      I*4
CC               SLPMIN : MINIMAL SIZE OF DETECTABLE CYCLE    R*8
CC                        SLIPS  (IN L5 CYCLES)
CC               SLPOUT : MINIMAL SIZE OF DETECTABLE OUTLIERS R*8
CC                        (IN L5 CYCLES)
CC               IPOLY  : POLYNOMIAL DEGREE FOR FIT           I*4
CC               NOBS   : NUMBER OF OBSERVATIONS IN ARC       I*4
CC               SVN    : NUMBER OF SATELLITE                 I*4
CC               LINTIM : ARRAY WITH TIME VALUES (HOURS)      R*8(*)
CC               LINOBS : ARRAY WITH OBSERVATIONS             R*8(*)
CC     IN/OUT :  LINFLG : ARRAY WITH OBSERVATION FLAGS       CH*1(*)
CC                        BIT=0: OUTLIER
CC                        BIT=1: NEW AMBIGUITY
CC        OUT :  NBAD   : NUMBER FLAGGED/BAD POINTS           I*4
CC               ARCOFF : PHASE-CODE DIFFERENCE (METERS)      R*8
CC               ARCRMS : PHASE-CODE RMS        (METERS)      R*8
CC               NSLIP  : NUMBER OF REPAIRED CYCLE SLIPS      I*4
CC               SLPTIM : INDEX TIME IN ARC OF THE CYCLE SLIP I*4(*)
CC               SLPCYC : SIZE OF THE CYCLE SLIPS             R*8(*)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  T.A. SPRINGER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  22-JUL-96
CC
CC CHANGES    :  30-MAR-98 : TS: INITIALIZATION OF SLPOLD
CC               31-AUG-98 : DI: USE 'COMFREQ' (FOR GLONASS SATELLITES)
CC               17-MAR-03 : RD: TOLERANCE FOR TEST FOR IFC-COMPILER
CC               30-JUN-03 : RD: "JUMP" HAS NEVER TO BE ZERO!
CC               30-AUG-03 : HU: SHARED DO LABELS REMOVED
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               13-JUL-06 : RD: TOLERANCE WHEN FINDING "JUMP"
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern
      USE s_setflg
      USE s_exitrc
      USE s_clrflg
      USE s_offset
      USE s_timst2
      USE f_tstflg
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IDBL  , IEND  , IFLG  , INDEX , IPOLY , ISLIP ,
     1          ISTART, ISTEP , J     , JUMP  , K     , L     , MAXSLP,
     2          MINOBS, NBAD  , NDIF  , NOBS  , NSLIP , NSLOLD
C
      REAL*8    ARCOFF, ARCRMS, CYCLES, DIFF  , DIFMAX, RMSMAX, SLPLVL,
     1          SLPMIN, SLPOLD, SLPOUT, OBSTIM, CYCMAX
C
CCC       IMPLICIT  REAL*8(A-H,O-Z)
C
      REAL*8      LINTIM(*),LINOBS(*)
      REAL*8      OFF(2),RMS(2),ARCTIM(2)
      REAL*8      SLPCYC(*)
C
      INTEGER*4   SLPTIM(*),ISLDBL(300),SVN
      INTEGER*4   JP(4),IOBS(2),IFLAG(2)
C
      CHARACTER*40 TIMSTR
      CHARACTER*32 FILRNX
      CHARACTER*1  LINFLG(*)
C
      INCLUDE     'COMFREQ.inc'
C
C ESTIMATE L5-P5 OFFSET
C ---------------------
      NSLOLD=NSLIP
1     CALL OFFSET(LINTIM,LINOBS,LINFLG,NOBS,ARCOFF,ARCRMS)
C
C CHECK RMSMAX
C ------------
      IF (ARCRMS.GT.RMSMAX) THEN
C
C CUT THE ARC INTO TWO PARTS ONE SHOULD CONTAIN THE (LARGEST) CYCLE SLIP
C ----------------------------------------------------------------------
        IFLAG(1)=0
        IFLAG(2)=0
10      IOBS(1)=NOBS/2
        IOBS(2)=NOBS-IOBS(1)
        JP(1)=1
        JP(2)=1+IOBS(1)
        IF (IOBS(1).LT.MINOBS .OR. IOBS(2).LT.MINOBS) THEN
          NBAD=NOBS
          GOTO 1000
        ENDIF
C
        CALL OFFSET(LINTIM(JP(1)),LINOBS(JP(1)),LINFLG(JP(1)),IOBS(1),
     1              OFF(1),RMS(1))
        CALL OFFSET(LINTIM(JP(2)),LINOBS(JP(2)),LINFLG(JP(2)),IOBS(2),
     1              OFF(2),RMS(2))
C
C DETERMINE WHICH PART HAS THE (LARGEST) CYCLE SLIP
C INDEX WILL POINT TO THE PART WITH THE SMALLEST RMS
C ISLIP WILL POINT TO THE PART WITH THE LARGEST RMS
C --------------------------------------------------
        IF (RMS(1).LT.RMS(2)) THEN
          INDEX=1
          ISLIP=2
        ELSE
          INDEX=2
          ISLIP=1
        ENDIF
C
C APPROXIMATE THE SIZE OF THE CYCLE SLIP
C --------------------------------------
        NDIF=0
        CYCLES=0D0
        CYCMAX=0D0
        ISTART=JP(ISLIP)
        IEND  =ISTART+IOBS(ISLIP)-1
        DO 110 I=ISTART,IEND
          DIFF=DABS(LINOBS(I)-OFF(INDEX))
          IF (CYCMAX.LT.DIFF) CYCMAX=DIFF
          IF (DIFF.GT.SLPMIN) THEN
            NDIF=NDIF+1
            CYCLES=CYCLES+DIFF
          ENDIF
110     CONTINUE
        IF (NDIF.GT.1) CYCLES=CYCLES/(NDIF*1D0)
C
C ONLY TOO SMALL JUMPS: NO CYCLE SLIP ONLY "HIGHER NOISE"
C -------------------------------------------------------
        IF (NDIF.EQ.0) THEN
          ARCTIM(1) = LINTIM(1)/24d0+DNINT(OBSTIM)
          ARCTIM(2) = LINTIM(NOBS)/24d0+DNINT(OBSTIM)
          CALL TIMST2(1,2,ARCTIM,TIMSTR)
          WRITE(lfnerr,'(/,A,/,16X,A,A,/,16X,A,I4,A,A,/,' //
     1                  '2(16X,A,F7.2,7X,A,F7.2,1X,A,/))')
     2    ' ### SR CHKWUB: An arc with high RMS but no big ' //
     3                    'cycle slip detected',
     4                    'RINEX file:         ',TRIM(FILRNX),
     5                    'Arc (SVN',svn,'):      ',timstr,
     6                    'RMS for the arc:    ',ARCRMS,
     7                    'limit:',RMSMAX,'L5-cycles',
     8                    'Biggest cycle slip: ',CYCMAX,
     9                    'limit:',SLPMIN,'L5-cycles'
          GOTO 200
        ENDIF
C
C SET THE SEARCH SENSITIVITY
C --------------------------
        IF (CYCLES.LT.SLPMIN) THEN
          DIFMAX=SLPMIN
        ELSE
          DIFMAX=DMIN1(10D0,CYCLES)
        ENDIF
C
C SET THE SCREENING ORDER
C -----------------------
        IF (ISLIP.EQ.1) THEN
          ISTART=JP(ISLIP)
          IEND  =ISTART+IOBS(ISLIP)-1
          ISTEP =1
        ELSE
          IEND  =JP(ISLIP)
          ISTART=ISTART+IOBS(ISLIP)-1
          ISTEP =-1
        ENDIF
C
C FIND THE EPOCH OF THE CYCLE SLIP
C --------------------------------
        JUMP=0
        DO 120 I=ISTART,IEND,ISTEP
          DIFF=DABS(LINOBS(I)-OFF(INDEX))
!
! RD: DIFF MAY BE EQUAL TO DIFMAX IF NDIF.EQ.1 AND CYCLES.LT.10D0
!     (SEE LOOP 110, COMPUTATION OF CYCLES)
!          IF (DIFF.GT.DIFMAX) THEN
          IF (DIFF+1d-6.GE.DIFMAX) THEN
            JUMP=I
          ENDIF
120     CONTINUE
C
C CHECK IF A CYCLE SLIP WAS FOUND
C -------------------------------
        IF (JUMP.EQ.0) THEN
          IFLAG(ISLIP)=IOBS(ISLIP)
C
C GET THE RIGHT CYCLE SLIP INDEX
C ------------------------------
        ELSE
          IF (ISLIP.EQ.1) THEN
            JUMP=JUMP+1
          ELSE
            JUMP=JUMP
          ENDIF
        ENDIF
C
C CHECK THE TWO PARTS
C -------------------
        JP(1)=1
        JP(2)=JUMP
        IOBS(1)=JUMP-1
        IOBS(2)=NOBS-JUMP+1
        DO 130 I=1,2
          SLPOLD=9D20
C
C ENOUGHT OBSERVATIONS ?
C ----------------------
135       IF (IOBS(I)-IFLAG(I).GE.MINOBS) THEN
            CALL OFFSET(LINTIM(JP(I)),LINOBS(JP(I)),LINFLG(JP(I)),
     1                  IOBS(I),OFF(I),RMS(I))
C
C IF RMS TO LARGE THEN POSSIBLY MORE CYCLE SLIPS OR OUTLIERS
C USE 3*RMS TO SCREEN DATA OF THIS PART
C ----------------------------------------------------------
            IF (RMS(I).GT.RMSMAX) THEN
C
C SET THE OUTLIER DETECTION LEVEL (AVOID ENDLESS LOOP)
C RD: TOLERANCE IS NECESSARY FOR A (STUPID) COMPILER
C ----------------------------------------------------
              SLPLVL=RMS(I)*3D0
              IF (SLPLVL.GE.SLPOLD-1D-4) SLPLVL=SLPOLD/2D0
              SLPOLD=SLPLVL
              IFLAG(I)=0
              DO 140 IFLG=JP(I),JP(I)+IOBS(I)-1
                DIFF=DABS(LINOBS(IFLG)-OFF(I))
C
                IF (DIFF.GT.SLPLVL) THEN
                  CALL SETFLG(LINFLG(IFLG),0)
                  IFLAG(I)=IFLAG(I)+1
                ELSE
                  CALL CLRFLG(LINFLG(IFLG),0)
                ENDIF
140           CONTINUE
              GOTO 135
            ENDIF
          ELSE
C
C NOT ENOUGHT OBSERVATIONS IN THIS PART
C -------------------------------------
            OFF(I)=0D0
            IFLAG(I)=IOBS(I)
            DO 150 IFLG=JP(I),JP(I)+IOBS(I)-1
              CALL SETFLG(LINFLG(IFLG),0)
150         CONTINUE
          ENDIF
130     CONTINUE
C
C BOTH PARTS BAD
C --------------
        IF (OFF(1).EQ.0D0 .AND. OFF(2).EQ.0D0) THEN
          NBAD=NOBS
          GOTO 1000
C
C ONE PART BAD --> NO JUMP (BUT THERE MIGHT BE A JUMP IN THE GOOD PART?!)
C ------------------------
        ELSE IF (OFF(1).EQ.0D0) THEN
          ARCOFF=OFF(2)
          GOTO 200
        ELSE IF (OFF(2).EQ.0D0) THEN
          ARCOFF=OFF(1)
          GOTO 200
C
C BOTH PARTS GOOD: CONNECT THE TWO PARTS, RESET FLAGS, RETURN TO START
C --------------------------------------------------------------------
        ELSE
          ARCOFF=OFF(1)
          CYCLES=DNINT(OFF(2)-OFF(1))
          IF (DABS(CYCLES).GE.SLPMIN) THEN
            NSLIP=NSLIP+1
C
C CHECK MAXSLP
C ------------
            IF (NSLIP.GT.MAXSLP) THEN
              WRITE(LFNERR,901)NSLIP,MAXSLP
901           FORMAT(/,' *** SR CHKWUB: MAXSLP EXCEEDED ',/,
     1                            16X,' NUMBER OF SLIPS : ',I4,/,
     2                            16X,' MAXIMUM NUMBER  : ',I4,/)
              CALL EXITRC(2)
            ENDIF
            SLPTIM(NSLIP)=JUMP
            SLPCYC(NSLIP)=CYCLES
            CALL SETFLG(LINFLG(JUMP),1)
            DO 160 I=JP(2),JP(2)+IOBS(2)-1
              LINOBS(I)=LINOBS(I)-CYCLES
160         CONTINUE
          ENDIF
C
          NBAD=0
          DO 170 I=1,NOBS
            CALL CLRFLG(LINFLG(I),0)
170       CONTINUE
          IF (NSLOLD.LT.NSLIP) THEN
            NSLOLD=NSLIP
            GOTO 1
          ENDIF
        ENDIF
C
C NO JUMP DETECTED OR TO FEW POINTS: ONLY OUTLIERS IN ONE PART !!
C ---------------------------------------------------------------
200     CONTINUE
C
      ENDIF
C
C NOW ARC RMS IS SMALLER THAN RMSMAX OR NO MORE SLIPS DETECTED
C ============================================================
C
C SCREEN FOR OUTLIERS UNTIL "ARCRMS" IS LOWER THEN MAXRMS
C -------------------------------------------------------
      SLPOLD=9D20
      SLPLVL=SLPOUT
410   NBAD=0
      DO 400 I=1,NOBS
        DIFF=DABS(LINOBS(I)-ARCOFF)
        IF (DIFF.GT.SLPLVL) THEN
          NBAD=NBAD+1
          CALL SETFLG(LINFLG(I),0)
        ELSE
          CALL CLRFLG(LINFLG(I),0)
        ENDIF
400   CONTINUE
      IF (NOBS-NBAD.LT.MINOBS) THEN
        NBAD=NOBS
        GOTO 1000
      ENDIF
      CALL OFFSET(LINTIM,LINOBS,LINFLG,NOBS,ARCOFF,ARCRMS)
C
C CHECK RMS (PREVENT ENDLESS LOOP WITH SLPOLD)
C --------------------------------------------
      IF (ARCRMS.GE.RMSMAX) THEN
        SLPLVL=DMIN1(SLPOUT,ARCRMS*4D0)
        IF (SLPLVL.GE.SLPOLD) SLPLVL=SLPOLD/2D0
        SLPOLD=SLPLVL
cc        IF (SLPLVL.LT.SLPOUT) GOTO 410
        GOTO 410
      ELSE
        SLPOLD=SLPLVL
        SLPLVL=DMIN1(SLPOLD,ARCRMS*4D0)
      ENDIF
C
C FINAL CHECK
C -----------
      NBAD=0
      DO 420 I=1,NOBS
        DIFF=DABS(LINOBS(I)-ARCOFF)
        IF (DIFF.GT.SLPLVL) THEN
          CALL SETFLG(LINFLG(I),0)
          NBAD=NBAD+1
          LINOBS(I)=0D0
        ELSE
          CALL CLRFLG(LINFLG(I),0)
        ENDIF
420   CONTINUE
      IF (NOBS-NBAD.LT.MINOBS) THEN
        NBAD=NOBS
        GOTO 1000
      ENDIF
      CALL OFFSET(LINTIM,LINOBS,LINFLG,NOBS,ARCOFF,ARCRMS)
C
C REMOVE INTEGER NUMBER OF L5 CYCLES TO SET L5-P5 TO ZERO
C SET BAD OBSERVATIONS TO ZERO
C -------------------------------------------------------
      DO 430 I=1,NOBS
        IF (.NOT.TSTFLG(LINFLG(I),0)) THEN
          LINOBS(I)=LINOBS(I)-ARCOFF
        ENDIF
430   CONTINUE
C
C CHECK PERCENTAGE OF BAD POINTS
C ------------------------------
1000  CONTINUE
      IF (NOBS-NBAD.LT.MINOBS) THEN
        NBAD=NOBS
        ARCOFF=0D0
        ARCRMS=0D0
        DO 1010 I=1,NOBS
          CALL SETFLG(LINFLG(I),0)
          LINOBS(I)=0D0
1010    CONTINUE
      ENDIF
C
C CONVERT ARCOFF TO METERS
C ------------------------
      ARCOFF=ARCOFF*WLGT(5,SVN)
      ARCRMS=ARCRMS
C
C CHECK FOR SLIPS AT IDENTICAL EPOCHS
C -----------------------------------
      DO 1110 I=1,NSLIP
        IDBL=0
        DO 1120 J=I+1,NSLIP
          IF (SLPTIM(I).EQ.SLPTIM(J)) THEN
            SLPCYC(I)=SLPCYC(I)+SLPCYC(J)
            IDBL=IDBL+1
            ISLDBL(IDBL)=J
          ENDIF
1120    CONTINUE
C
C REMOVE THE IDENTICAL SLIPS
C --------------------------
        DO 1130 K=IDBL,1,-1
          DO L=ISLDBL(K),NSLIP-1
            SLPTIM(L)=SLPTIM(L+1)
          ENDDO
1130    CONTINUE
        NSLIP=NSLIP-IDBL
1110  CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
