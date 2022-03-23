      MODULE s_CHKL4P
      CONTAINS

C*
      SUBROUTINE CHKL4P(RMSL4 ,GAPL4 ,MINL4 ,IFXSLP,IPOLY ,NOBS  ,
     1                  SVN   ,LINTIM,LINOBS,LINFLG,NBAD  ,NSLIP ,
     2                  SLPTIM,CYCL1 ,CYCL2 ,CYCL3 ,CYCL4 ,CYCL5 )
CC
CC NAME       :  CHKL4P
CC
CC PURPOSE    :  CHECKS AN L4 PHASE OBSERVATION ARC FOR JUMPS AND OUTLIERS
CC
CC PARAMETERS :
CC        IN  :  RMSL4  : THRESHOLD RMS VALUE (m)             R*8
CC               GAPL4  : MAXIMAL GAP FOR CONNECTION (sec)    I*4
CC               MINL4  : NUMBER OF OBSERVATIONS FOR FIT      I*4
CC               IFXSLP : FIX CYCLE SLIPS OR NOT (0/1)        I*4
CC               IPOLY  : POLYNOMIAL DEGREE FOR FIT           I*4
CC               NOBS   : NUMBER OF OBSERVATIONS IN ARC       I*4
CC               SVN    : NUMBER OF SATELLITE                 I*4
CC               LINTIM : ARRAY WITH TIME VALUES (HOURS)      R*8(*)
CC               LINOBS : ARRAY WITH OBSERVATIONS             R*8(*)
CC     IN/OUT :  LINFLG : ARRAY WITH OBSERVATION FLAGS       CH*1(*)
CC                        BIT=0: OUTLIER
CC                        BIT=1: NEW AMBIGUITY
CC        OUT :  NBAD   : NUMBER FLAGGED/BAD POINTS           I*4
CC               NSLIP  : NUMBER OF REPAIRED CYCLE SLIPS      I*4
CC               SLPTIM : INDEX TIME IN ARC OF THE CYCLE SLIP I*4(*)
CC               CYCL1  : SIZE OF THE CYCLE SLIPS IN L1       R*8(*)
CC               CYCL2  : SIZE OF THE CYCLE SLIPS IN L2       R*8(*)
CC               CYCL3  : SIZE OF THE CYCLE SLIPS IN L3       R*8(*)
CC               CYCL4  : SIZE OF THE CYCLE SLIPS IN L4       R*8(*)
CC         IN :  CYCL5  : SIZE OF THE CYCLE SLIPS IN L5       R*8(*)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  T.A. SPRINGER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  22-JUL-96
CC
CC CHANGES    :  13-MAY-97 : MR: REMOVE UNUSED VARAIBLES "J","JOBS"
CC               31-AUG-98 : DI: USE 'COMFREQ' (FOR GLONASS SATELLITES)
CC               25-JUN-99 : TS: TEST GAP DURING CYCLE SLIP REPAIR
CC                3-JUL-00 : TS: TEST MAXRES
CC               03-SEP-04 : PS: REMOVE DEBUGGING STUFF
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern
      USE f_tstflg
      USE s_clrflg
      USE s_setflg
      USE s_polyap
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IEND  , IFIRST, IFXSLP, ILAST , IOBS  , IPOLY ,
     1          ISLP  , ISTART, MAXPOL, MAXRES, MINL4 , NBAD  , NOBS  ,
     2          NSLIP
C
      REAL*8    DIFF  , DIFFL4, DIFFL5, DL1   , DL2   , DL3   , DL4   ,
     1          DL5   , DT    , GAPL4 , OFF1  , OFF2  , RMSL4
C
CCC       IMPLICIT  REAL*8(A-H,O-Z)
C
      PARAMETER(MAXPOL=10,MAXRES=1000)
C
      REAL*8      LINTIM(*),LINOBS(*)
      REAL*8      COEF1(MAXPOL),COEF2(MAXPOL),CRMS(MAXPOL),RMS(2)
      REAL*8      CYCL1(*),CYCL2(*),CYCL3(*),CYCL4(*),CYCL5(*)
      REAL*8      RES(MAXRES)
C
      INTEGER*4   SLPTIM(*)
      INTEGER*4   IFOK(2),SVN
C
      CHARACTER*1 LINFLG(*)
C
      INCLUDE     'COMFREQ.inc'
C
C LOOP OVER ALL JUMPS IN THIS ARC
C -------------------------------
      DO 10 ISLP=1,NSLIP
        CYCL1(ISLP)=0.D0
        CYCL2(ISLP)=0.D0
        CYCL3(ISLP)=0.D0
        CYCL4(ISLP)=0.D0
        RMS(1)=0.D0
        RMS(2)=0.D0
        ISTART=SLPTIM(ISLP)-1
        IEND=SLPTIM(ISLP)
C
C FIND "MINL4" OBSERVATIONS BEFORE CYCLE SLIP
C -------------------------------------------
        IOBS=0
20      IF (RMS(1).EQ.0.D0) THEN
          IF (ISTART.LT.1) THEN
            CALL CLRFLG(LINFLG(SLPTIM(ISLP)),1)
            DO 25 I=1,SLPTIM(ISLP)-1
              CALL SETFLG(LINFLG(I),0)
              LINOBS(I)=0.D0
              NBAD=NBAD+1
25          CONTINUE
            GOTO 10
          ENDIF
          IF (TSTFLG(LINFLG(ISTART),0)) THEN
            ISTART=ISTART-1
            GOTO 20
          ELSE
            IOBS=IOBS+1
            IF (IOBS.EQ.1) ILAST=ISTART
            IF (IOBS.LT.MINL4) THEN
              ISTART=ISTART-1
              GOTO 20
            ENDIF
          ENDIF
        ENDIF
C
C FIND "MINL4" OBSERVATIONS AFTER CYCLE SLIP
C ------------------------------------------
        IOBS=0
30      IF (RMS(2).EQ.0.D0) THEN
          IF (IEND.GT.NOBS) THEN
            CALL CLRFLG(LINFLG(SLPTIM(ISLP)),1)
            DO 35 I=SLPTIM(ISLP),NOBS
              CALL SETFLG(LINFLG(I),0)
              LINOBS(I)=0.D0
              NBAD=NBAD+1
35          CONTINUE
            GOTO 10
          ENDIF
          IF (TSTFLG(LINFLG(IEND),0)) THEN
            IEND=IEND+1
            GOTO 30
          ELSE
            IOBS=IOBS+1
            IF (IOBS.EQ.1) IFIRST=IEND
            IF (IOBS.LT.MINL4) THEN
              IEND=IEND+1
              GOTO 30
            ENDIF
          ENDIF
        ENDIF
C
C CHECK IF GAP IS STILL SMALL ENOUGH
C ----------------------------------
        DT=LINTIM(IFIRST)-LINTIM(ILAST)
        IF (DT.GE.GAPL4) THEN
          DO I=ILAST+1,IFIRST-1
            CALL SETFLG(LINFLG(I),0)
            CALL CLRFLG(LINFLG(I),1)
            LINOBS(I)=0.D0
            NBAD=NBAD+1
          ENDDO
          GOTO 300
        ENDIF
C
C FIT POLYNOMIAL THROUGH BOTH PARTS
C ---------------------------------
        IOBS=ILAST-ISTART+1
        IF (IOBS.GT.MAXRES) THEN
          WRITE(LFNERR,999)IOBS,MAXRES
999       FORMAT(/,' *** SR CHKL4P: TOO MANY DATA POINTS',
     1                       /,16X,'NUMBER OF OBSERVATIONS:',I5,
     2                       /,16X,'MAXIMUM NUMBER        :',I5,/)
          CALL EXITRC(2)
        ENDIF
        CALL POLYAP(LINTIM(ISTART),LINOBS(ISTART),LINFLG(ISTART),
     1              IOBS,IPOLY,COEF1,CRMS,RES,RMS(1),IFOK(1))
C
        IOBS=IEND-IFIRST+1
        IF (IOBS.GT.MAXRES) THEN
          WRITE(LFNERR,999)IOBS,MAXRES
          CALL EXITRC(2)
        ENDIF
        CALL POLYAP(LINTIM(IFIRST),LINOBS(IFIRST),LINFLG(IFIRST),
     1              IOBS,IPOLY,COEF2,CRMS,RES,RMS(2),IFOK(2))
C
C IF RMS TO LARGE SHIFT THE DATA PIECE ONE OBSERVATION
C ----------------------------------------------------
        IF (RMS(1).GT.RMSL4 .AND. RMS(2).GT.RMSL4) THEN
          IOBS=0
          ISTART=ILAST-1
          IEND=IFIRST+1
          RMS(1)=0.D0
          RMS(2)=0.D0
          GOTO 20
        ELSE IF (RMS(1).GT.RMSL4) THEN
          IOBS=0
          ISTART=ILAST-1
          RMS(1)=0.D0
          GOTO 20
        ELSE IF (RMS(2).GT.RMSL4) THEN
          IOBS=0
          IEND=IFIRST+1
          RMS(2)=0.D0
          GOTO 30
        ENDIF
C
C MARK ALL BAD DATA POINTS BETWEEN ILAST+1 AND IFIRST-1
C -----------------------------------------------------
        DO 40 I=ILAST+1,IFIRST-1
          CALL SETFLG(LINFLG(I),0)
          CALL CLRFLG(LINFLG(I),1)
          LINOBS(I)=0.D0
          NBAD=NBAD+1
40      CONTINUE
C
C IF ALL IS WELL DETERMINE THE SIZE OF THE CYCLE SLIP
C ---------------------------------------------------
        OFF1=0.D0
        DT=LINTIM(SLPTIM(ISLP))-LINTIM(ISTART+IFOK(1))
        DO 50 I=1,IPOLY
          OFF1=OFF1+COEF1(I)*DT**(I-1)
50      CONTINUE
C
        OFF2=0.D0
        DT=LINTIM(SLPTIM(ISLP))-LINTIM(IFIRST+IFOK(2))
        DO 110 I=1,IPOLY
          OFF2=OFF2+COEF2(I)*DT**(I-1)
110     CONTINUE
C
        DIFF=(OFF2-OFF1)/WLGT(4,SVN)
        DL4=DNINT(DIFF)
C
        DL2=(WLGT(1,SVN)*CYCL5(ISLP)-WLGT(4,SVN)*DL4)/
     1                                      (WLGT(2,SVN)-WLGT(1,SVN))
        DL1=DL2+CYCL5(ISLP)
        CYCL1(ISLP)=DNINT(DL1)
        CYCL2(ISLP)=DNINT(DL2)
        CYCL4(ISLP)=DL4
        DL4=WLGT(1,SVN)*CYCL1(ISLP)-WLGT(2,SVN)*CYCL2(ISLP)
        DL4=DNINT(DL4/WLGT(4,SVN))
        DL5=CYCL1(ISLP)-CYCL2(ISLP)
        DIFFL4=DABS(DL4-CYCL4(ISLP))
        DIFFL5=DABS(DL5-CYCL5(ISLP))
C
C L3 CYCLES (IN METERS)
C
        DL3= FACLIN(3,1,SVN)*WLGT(1,SVN)*CYCL1(ISLP)
     1              +FACLIN(3,2,SVN)*WLGT(2,SVN)*CYCL2(ISLP)
        CYCL3(ISLP)=DNINT(DL3/WLGT(3,SVN))
C
C CORRECT THE L4 CYCLES (IN METERS) TO GET CONTINEOUS L4
C SET BAD OBSERVATIONS TO ZERO
C -------------------------------------------------------
        DO 200 I=IFIRST,NOBS
          IF (TSTFLG(LINFLG(I),0)) THEN
            LINOBS(I)=0D0
          ELSE
            LINOBS(I)=LINOBS(I)-CYCL4(ISLP)*WLGT(4,SVN)
          ENDIF
200     CONTINUE
C
C FLAG THE RIGHT EPOCH WITH THE APPROPRIATE FLAG
C ----------------------------------------------
300     CONTINUE
        DT=LINTIM(IFIRST)-LINTIM(ILAST)
        CALL CLRFLG(LINFLG(SLPTIM(ISLP)),1)
        CALL SETFLG(LINFLG(IFIRST),2)
        SLPTIM(ISLP)=IFIRST
        IF ((DIFFL4.GE.RMSL4).OR.(DT.GE.GAPL4).OR.IFXSLP.EQ.0) THEN
          CALL SETFLG(LINFLG(IFIRST),1)
        ENDIF
C
C NEXT CYCLE SLIP
C ---------------
10    CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
