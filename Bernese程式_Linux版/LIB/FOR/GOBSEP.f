      MODULE s_gobsep
      CONTAINS
C*
      SUBROUTINE GOBSEP(ILFN  ,NFLSES,FILNUM,TLAST ,WINDOW,DTSIM ,
     1                  TIMREF,IDELTT,IFRMAT,MEATYP,NSATEL,NUMSAT,
     2                  NFRFIL,ICARR ,NSHD  ,SATSHD,TIMSHD,FLGSHD,
     3                  TOBS  ,OBSNUM,NSATFL,SVNFIL,OBSFLG,OBSERV,
     4                  SVNFI1,OBSFL1,OBSER1,DTFIL ,FILACT,NDIFF ,
     5                  NORXCLK,SECIPL,STNAME,STFIL,IRETC)
CC
CC NAME       :  GOBSEP
CC
CC PURPOSE    :  GIVEN NFLSES OBSERVATION FILES, RETURN OBSERVATIONS
CC               AND CORRESPONDING FLAGS OF NEXT OBSERVATION EPOCH.
CC
CC PARAMETERS :
CC         IN :  ILFN   : 1: USE LFN001, 2: USE LFN002        I*4
CC               NFLSES : NUMBER OF FILES FOR SESSION         I*4
CC               FILNUM(I),I=1,..,NFLSES: OBS. FILE NUMBERS   I*4
CC               TLAST  : LAST OBSERVATION TIME               R*8
CC               WINDOW(K,I),K=1,2, I=1,2,..,NFTOT: TIME WINDOW
CC               DTSIM  : MAXIMUM INTERVAL TO IDENTIFY TWO    R*8
CC                        TIMES WITH THE SAME EPOCH
CC               TIMREF(I),I=1,..,NFTOT: REFERENCE EPOCHS OF  R*8
CC                        ALL FILES
CC               IDELTT(I),I=1,..,NFTOT: OBSERV. INTERVAL OF  I*4
CC                        ALL FILES
CC               IFRMAT(I),I=1,..,NFTOT: FORMAT NUMBERS FOR   I*4
CC                        ALL FILES
CC               MEATYP(I),I=1,..,NFTOT: MEASUREMENT TYPE     I*4
CC                        =1: PHASE OBSERVATIONS
CC                        =2: CODE OBSERVATIONS
CC                        =3: RANGE OBSERVATIONS
CC               NSATEL(I),I=1,..,NFTOT: NUMBER OF SATELLITES I*4
CC               NUMSAT(K,I),K=1,..,NSATEL(I),I=1,..,NFTOT:   I*4
CC                        SATELLITES
CC               NFRFIL(I),I=1,..,NFTOT : NUMBER OF FRE-      I*4
CC                        QUENCIES PER FILE
CC               ICARR(K,I),K=1,..,NFRFIL(I),I=1,..,NFTOT     I*4
CC                        REQUESTED CARRIERS
CC               NDIFF(I):I=1,..,NFTOT: DIFFERENCE TYPE       I*4
CC                        NDIFF=0: ZERO DIFFERENCE
CC                        NDIFF=1: SINGLE DIFFERENCE
CC               NSHD   : NUMBER OF SATELLITES IN SHADOW      I*4
CC               SATSHD : PRN NUMBERS IN SHADOW               I*4(*)
CC               TIMSHD : EXCLUDED TIME INTERVAL              R*8(2,*)
CC               FLGSHD : FLAG OBSERV. (=2) OR NOT (=0)       I*4(*)
CC               SECIPL : MAX INTERVAL FOR CLK INTERPOLATION  R*8
CC               STNAME(I),I=1,..,NSTAT: STATION NAMES        CH*16
CC               STFIL(K,I),K=1,2, I=1,2,..,NFTOT: STATIONS   I*4
CC                        IN FILES
CC               NORXCLK  WHAT TO DO IF NO INPUT CLOCK RINEX  I*4
CC                        FOR RECEIVER CLOCKS:
CC                         -1: IGNORE CLOCK RINEX FILE
CC                          0: TRY ALSO SAT CLK FILE
CC                          1: USE OBS. (INTERPOL. CLK RNX)
CC                          2: SKIP OBS.
CC                          3: USE OBS. (SAT CLK = ZERO)
CC        OUT :  TOBS   : ACTUAL OBSERVATION EPOCH            R*8
CC               OBSNUM(I),I=1,..,NFLSES: OBSERV. NUMBERS FOR I*4
CC                        DIFFERENT FILES IN SESSION
CC               NSATFL(I),I=1,..,NFLSES: NUMBER OF SATEL-    I*4
CC                        LITES PER FILE
CC               SVNFIL(K,I),K=1,..,NSATFL(I),I=1,..,NFLSES   I*4
CC                        SATELLITE NUMBERS FOR EACH FILE
CC               OBSFLG : OBSERVATION FLAGS                   CH*1
CC                      (I,J,IF): SATELLITE I
CC                                FREQUENCY J
CC                                FILE IF
CC               OBSERV : OBSERVATIONS                        R*8
CC                      (I,J,IF): SATELLITE I
CC                                FREQUENCY J
CC                                FILE IF
CC               SVNFI1(I),I=1,..,NSATFL: SATELLITES (AUX.    I*4
CC                        ARRAY)
CC               OBSFL1(I,K),I=1,..,NSATFL,K=1,..,NFRFIL:     CH*1
CC                        OBSERVATION FLAGS (AUX. ARRAY)
CC               OBSER1(I,K),I=1,..,NSATFL,K=1,..,NFRFIL:     R*8
CC                        OBSERVATIONS (AUX. ARRAY)
CC               DTFIL(2,I),I=1,..,NFLSES: DIFFERENCES TO       R*8
CC                        NOMINAL EPOCH IN SECONDS FOR BOTH
CC                        RECEIVERS
CC               FILACT(I),I=1,NFLSES: FILES INVOLVED IN      CH*1
CC                        CURRENT EPOCH
CC               IRETC  : RETURN CODE.
CC                        =0: AT LEAST ONE FILE ACTIVE
CC                        =1: ALL FILES CLOSED
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/16 14:32
CC
CC CHANGES    :  25-JUL-92 : ??: MARK SATELLITES ACCORDING TO SATELLITE
CC                               PROBLEM FILE. ADD PARAMETER "MEATYP"
CC               10-AUG-94 : MR: CALL EXITRC
CC               17-AUG-94 : MR: MAXBAD=200 (OLD: MAXBAD=20)
CC               21-FEB-96 : TS: CHANGED MAXFLS FROM 30 TO 35
CC               26-MAR-96 : TS: SEVERAL CHANGES FOR CLOCKS AND SLR PROCESSING
CC               21-JUL-96 : TS: INCREASED MAXFLS FROM 55 --> 60
CC               05-AUG-96 : TS: INCREASED MAXFLS FROM 60 --> 65
CC               23-AUG-96 : TS: MAXBAD IN INCLUDE FILE
CC               27-SEP-96 : TS: MAXFLS IN INCLUDE FILE
CC               14-AUG-97 : SS: GEOMETRY-FREE LC OF ZERO DIFFERENCES
CC               02-FEB-00 : RD: APRIORI VALUES FOR REC-CLOCK ESTIMATION
CC                               IN DELTAT(2,*) ARE HANDELD CORRECT
CC               28-APR-00 : RD: RCLKMX HAS TO BE 1D-3 !!
CC               01-MAI-00 : TS: SMALL CORRECTION FOR DTFIL IN ZD CASE
CC               13-JUN-00 : HB: ASSUME MS-JUMP ONLY IF DELTAT(2,*).EQ.0.D0
CC               27-NOV-00 : TS: SMALL CORRECTION FOR DELTAT(2,*)
CC               07-OCT-01 : HU: MAXFLS DYNAMIC, ILFN ADDED
CC               15-DEC-01 : HU: INCLUDE D_CONST
CC               20-NOV-02 : RD: CHECK WINDOW ON 1-SEC-LEVEL ONLY (NOT AS REAL)
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               27-JAN-04 : HU: EXCLUSION OF ECLIPSING SATELLITE OBSERVATIONS
CC               29-MAR-04 : CU: CORRECT EPOCH BY FRACTION OF SECOND FOR RANGE
CC                               MEASUREMENTS
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               17-AUG-06 : HU: CONVERTED TO MODULE
CC               26-FEB-08 : RD: USE GTSATB FROM D_SATCRX
CC               29-MAY-09 : RD: RECEIVER CLOCK FROM INPUT CLK RNX FILE
CC               21-SEP-09 : RD: ECLIPSING FLAG FOR CLOCK RESULTS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const,  ONLY: c
      USE d_clkrnx, ONLY: undef
      USE m_maxdim, ONLY: maxbad
      USE d_satcrx, ONLY: gtsatb
      USE s_rdobsi
      USE s_alcerr
      USE s_setflg
      USE s_exitrc
      USE s_gtflna
      USE s_gtrxck
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IBAD  , IF    , IFOPEN, IFRQ  , IFTOBS, IG    , ILFN  ,
     1          IRC   , IRETC , ISAT  , ISATEL, ISHD  , ISTA  , KSAT  ,
     2          LFNTOP, LFNUSE, MXCFRQ, MXCSAT, NBAD  , NFLSES, NSHD  ,
     3          NORXCLK
C
      REAL*8    DTSIM , RCLKMX, TLAST , TOBS  , TOBSEC, CLOCK , SIGMA ,
     1          SECIPL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C
      REAL*8      WINDOW(2,*),OBSERV(MXCSAT,MXCFRQ,*),DTFIL(2,*)
      REAL*8      OBSER1(MXCSAT,*),TIMREF(*)
      REAL*8      TIMBAD(2,MAXBAD),TIMSHD(2,*)
C
      REAL(r8b),DIMENSION(:,:),ALLOCATABLE,SAVE     :: OBSTIM
      REAL(r8b),DIMENSION(:,:,:),ALLOCATABLE,SAVE   :: DELTAT
C
      INTEGER*4   NFRFIL(*),NSATFL(*),SVNFIL(MXCSAT,*),OBSNUM(*)
      INTEGER*4   ICARR(MXCFRQ,*),FILNUM(*),IDELTT(*)
      INTEGER*4   IFRMAT(*),NSATEL(*),NUMSAT(MXCSAT,*),SVNFI1(*)
      INTEGER*4   MEATYP(*),NDIFF(*)
      INTEGER*4   IOBBAD(MAXBAD),IACBAD(MAXBAD),SATBAD(MAXBAD)
      INTEGER*4   SATSHD(*),STFIL(2,*),FLGSHD(*)
C
      INTEGER(i4b),SAVE                             :: IFIRST=0
      INTEGER(i4b),DIMENSION(:,:),ALLOCATABLE,SAVE  :: IRETRN
C
      CHARACTER*1  OBSFLG(MXCSAT,MXCFRQ,*),FILACT(*)
      CHARACTER*1  OBSFL1(MXCSAT,*),EPOFLG
      CHARACTER*6  MXNSAT,MXNFRQ
      CHARACTER*16 STNAME(*)
      CHARACTER*32 FILNAM
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
C
C MAXIMUM RECEIVER CLOCK ERROR
C (TO DISTINGUISH BETWEEN CLOCK ERROR AND MS-JUMPS)
C -------------------------------------------------
      RCLKMX=1D-3
C
C SELECT LFNUM
C ------------
      IF (ILFN.EQ.1) THEN
        LFNUSE=LFN001
      ELSEIF (ILFN.EQ.2) THEN
        LFNUSE=LFN002
      ELSE
        WRITE(LFNERR,910)ILFN
910     FORMAT(/,' *** SR GOBSEP: VALUE FOR LFN NOT ALLOWED:',I6,/)
        CALL EXITRC(2)
      ENDIF
C
C MAXIMUM POSSIBLE NUMBER OF FILES
C --------------------------------
      LFNTOP=LFNEPH
      IF (LFNUSE+NFLSES .GT. LFNTOP) THEN
        WRITE(LFNERR,920) NFLSES,LFNTOP-LFNUSE-1
920     FORMAT(/,' *** SR GOBSEP: TOO MANY OPEN FILES',
     1         /,'                NUMBER OF OPEN FILES  :',I6,
     2         /,'                MAXIMUM ALLOWED NUMBER:',I6,/)
        CALL EXITRC(2)
      ENDIF
C
C CHECK LOCAL MAXIMUM DIMENSIONS
C ------------------------------
      IF(IFIRST.EQ.0) THEN
        ALLOCATE (OBSTIM(NFLSES,2),STAT=IRC)
        CALL ALCERR(IRC,'OBSTIM',(/NFLSES,2/),'GOBSEP')
        ALLOCATE (DELTAT(2,NFLSES,2),STAT=IRC)
        CALL ALCERR(IRC,'DELTAT',(/2,NFLSES,2/),'GOBSEP')
        ALLOCATE (IRETRN(NFLSES,2),STAT=IRC)
        CALL ALCERR(IRC,'IRETRN',(/NFLSES,2/),'GOBSEP')
C
        IFIRST=1
C
C GET BAD SATELLITE INTERVALS FROM SATELLITE PROBLEM FILE
C -------------------------------------------------------
        CALL GTFLNA(0,'SATCRUX',FILNAM,IRC)
        CALL GTSATB(MAXBAD,FILNAM,NBAD,SATBAD,IOBBAD,IACBAD,TIMBAD)
      ENDIF
C
C INITIALIZE NEW SESSION
C ----------------------
      IF(TLAST.EQ.0.D0) THEN
        DO 10 IF=1,NFLSES
          FILACT(IF)='R'
          IRETRN(IF,ILFN)=0
10      CONTINUE
      ENDIF
C
C READ NEW EPOCHS FOR THOSE FILES USED IN PREVIOUS EPOCH
C ------------------------------------------------------
      DO 30 IF=1,NFLSES
        IF(IRETRN(IF,ILFN).EQ.0.AND.FILACT(IF).EQ.'R') THEN
          IG=FILNUM(IF)
20        CALL RDOBSI(LFNUSE+IF-1,IFRMAT(IG),NFRFIL(IG),ICARR(1,IG),
     1                OBSTIM(IF,ILFN),DELTAT(1,IF,ILFN),EPOFLG,
     2                NSATFL(IF),SVNFI1,OBSFL1,OBSER1,IRETRN(IF,ILFN))
C
C CHECK RETURN CODE AND WINDOW
C ----------------------------
          IF(IRETRN(IF,ILFN).EQ.1.OR.
     1       DNINT(OBSTIM(IF,ILFN)*86400D0).GT.
     2                                DNINT(WINDOW(2,IG)*86400D0))THEN
            IRETRN(IF,ILFN)=1
          ELSE
            IF(DNINT(OBSTIM(IF,ILFN)*86400D0).LT.
     1                            DNINT(WINDOW(1,IG)*86400D0)) GOTO 20
C
C MARK OBSERVATION WITHIN BAD TIME INTERVALS
C ------------------------------------------
            DO ISAT=1,NSATFL(IF)
              DO IBAD=1,NBAD
                IF (SATBAD(IBAD).EQ.SVNFI1(ISAT).AND.
     1              TIMBAD(1,IBAD).LE.OBSTIM(IF,ILFN).AND.
     2              TIMBAD(2,IBAD).GT.OBSTIM(IF,ILFN).AND.
     3              (IOBBAD(IBAD).EQ.MEATYP(IG).OR.
     4               IOBBAD(IBAD).EQ.3)) THEN
                  DO IFRQ=1,NFRFIL(IG)
                    CALL SETFLG(OBSFL1(ISAT,IFRQ),0)
                  ENDDO
                ENDIF
              ENDDO
C
C EXCLUDE SATELLITES IN SHADOW
              DO ISHD=1,NSHD
                IF (FLGSHD(ISHD).EQ.2.AND.
     1              SATSHD(ISHD).EQ.SVNFI1(ISAT).AND.
     2              TIMSHD(1,ISHD).LE.OBSTIM(IF,ILFN).AND.
     3              TIMSHD(2,ISHD).GT.OBSTIM(IF,ILFN))THEN
                  DO IFRQ=1,NFRFIL(IG)
                    CALL SETFLG(OBSFL1(ISAT,IFRQ),0)
                  ENDDO
                ENDIF
              ENDDO
            ENDDO
C
C READ RECEIVER CLOCK CORRECTIONS FORM CLOCK RINEX FILE
C -----------------------------------------------------
            IF (NORXCLK.NE.-1 .AND. NDIFF(IG).EQ.0) THEN
              CALL GTRXCK(OBSTIM(IF,ILFN),DTSIM,STNAME(STFIL(1,IF)),
     1                    0,SECIPL,CLOCK,SIGMA,IRC)
              IF (IRC == 0 .AND. CLOCK /= undef) THEN
                DELTAT(2,IF,ILFN)=-CLOCK/1D6-DELTAT(1,IF,ILFN)
              ELSE IF (NORXCLK.NE.0) THEN
                DO ISAT=1,NSATFL(IF)
                  DO IFRQ=1,NFRFIL(IG)
                    CALL SETFLG(OBSFL1(ISAT,IFRQ),0)
                  ENDDO
                ENDDO
              ELSE
              ENDIF
            ENDIF
C
C ORDER SATELLITES ACCORDING TO HEADER SATELLITE LIST
C ---------------------------------------------------
            KSAT=0
            DO 28 ISATEL=1,NSATEL(IG)
              DO 26 ISAT=1,NSATFL(IF)
                IF(SVNFI1(ISAT).EQ.NUMSAT(ISATEL,IG)) THEN
                  KSAT=KSAT+1
                  SVNFIL(KSAT,IF)=SVNFI1(ISAT)
                  DO 24 IFRQ=1,NFRFIL(IG)
                    OBSFLG(KSAT,IFRQ,IF)=OBSFL1(ISAT,IFRQ)
                    OBSERV(KSAT,IFRQ,IF)=OBSER1(ISAT,IFRQ)
                    IF (NDIFF(IG).EQ.0 .AND.
     1                  OBSER1(ISAT,IFRQ).NE.0.D0) THEN
C
C IF A RECEIVER CLOCK BECOMES LARGER THAN RCLKMX -> ASSUME A MS-JUMP
C (NO FOR SLR-OBSERVATIONS)
                      IF (MEATYP(IG).EQ.1 .OR. MEATYP(IG).EQ.2) THEN
                        IF ((DELTAT(2,IF,ILFN).EQ.0D0) .AND.
     1                      (DABS(DELTAT(1,IF,ILFN)) .GE. RCLKMX))
     2                    DELTAT(2,IF,ILFN)=-DELTAT(1,IF,ILFN)
                      ENDIF
C
C CORRECT OBSERVATION ALSO FOR THE 2nd PART OF CLOCK CORRECTION
C (THAN THIS TOPIC IS COMPLETE, NOT FURTHER HANDLING IN PRANGE IS NECESSARY)
C
                      IF (MEATYP(IG).EQ.1) THEN
                        IF (ICARR(IFRQ,IG).NE.4) THEN
                          OBSERV(KSAT,IFRQ,IF)=-OBSERV(KSAT,IFRQ,IF)+
     1                                         DELTAT(2,IF,ILFN)*C
                        ELSE
                          OBSERV(KSAT,IFRQ,IF)=-OBSERV(KSAT,IFRQ,IF)
                        ENDIF
                      ELSE IF (MEATYP(IG).EQ.2) THEN
                        IF (ICARR(IFRQ,IG).NE.4) THEN
                          OBSERV(KSAT,IFRQ,IF)= OBSERV(KSAT,IFRQ,IF)+
     1                                          DELTAT(2,IF,ILFN)*C
                        ELSE
                          OBSERV(KSAT,IFRQ,IF)= OBSERV(KSAT,IFRQ,IF)
                        ENDIF
                      ELSE IF (MEATYP(IG).EQ.3) THEN
                        OBSERV(KSAT,IFRQ,IF)= OBSERV(KSAT,IFRQ,IF)
                      ELSE
                        WRITE(LFNERR,901) MEATYP(IG)
901                     FORMAT(/,' *** SR GOBSEP: ',
     1                           'UNKNOWN MEASUREMENT TYPE'
     2                         /,16X,'MEASUREMENT TYPE NUMBER :',I5,/)
                        CALL EXITRC(2)
                      ENDIF
                   ENDIF
24                CONTINUE
                  GOTO 28
                ENDIF
26            CONTINUE
28          CONTINUE
          ENDIF
        ENDIF
30    CONTINUE
C
C FIND NEXT OBSERVATION EPOCH
C ---------------------------
      TOBSEC=1.D05
      IFOPEN=0
      DO 40 IF=1,NFLSES
        IF(IRETRN(IF,ILFN).EQ.0) THEN
          IFOPEN=IFOPEN+1
          IF(OBSTIM(IF,ILFN).LE.TOBSEC) THEN
            TOBSEC=OBSTIM(IF,ILFN)
            IFTOBS=IF
          ENDIF
        ENDIF
40    CONTINUE
      TLAST=TOBSEC
C
C CORRECT EPOCH TO BY FRACTION OF SECOND OF FIRST RECEIVER
C --------------------------------------------------------
      TOBS=OBSTIM(IFTOBS,ILFN)+DELTAT(1,IFTOBS,ILFN)/86400.D0
C
C IN CASE OF ZERO DIFFERENCE DO NOT CORRECT EPOCH TOBS
C FOR GPS DELTAT(1:2,*) BOTH CONTAIN THE MILLISECOND JUMPS (BE CAREFULL)!!
C FOR SLR DELTAT(1,*) CONTAINS THE REAL FRACTIONAL SECONDS,
c for slr: tobs  = obstim(sec) + deltat(1)(millisec) in units of mjd
c          dtfil = deltat(2)(microsec)               in units of sec
C
      IF (NDIFF(FILNUM(IFTOBS)).EQ.0 .AND. MEATYP(FILNUM(IFTOBS)).NE.3)
     1   TOBS=OBSTIM(IFTOBS,ILFN)
C
C DEFINE RETURN CODE
C ------------------
      IF(IFOPEN.EQ.0) THEN
        IRETC=1
        GOTO 999
      ELSE
        IRETC=0
      ENDIF
C
C FIND ALL QUASI SIMULTANEOUS EPOCHS
C ----------------------------------
      DO 50 IF=1,NFLSES
        IF(IRETRN(IF,ILFN).EQ.0.AND.OBSTIM(IF,ILFN)-TOBSEC.LE.DTSIM)THEN
          FILACT(IF)='R'
          OBSNUM(IF)=IDNINT((OBSTIM(IFTOBS,ILFN)-TIMREF(FILNUM(IF)))/
     1                      IDELTT(FILNUM(IF))*86400.D0)+1
          DO 45 ISTA=1,NDIFF(FILNUM(IF))+1
            IF (NDIFF(FILNUM(IF)).EQ.1) THEN
              DTFIL(ISTA,IF)=DELTAT(ISTA,IF,ILFN)-DELTAT(1,IFTOBS,ILFN)
     1                      +DNINT((OBSTIM(IF,ILFN)-OBSTIM(IFTOBS,ILFN))
     2                                                        *86400.D0)
            ELSE
              IF (MEATYP(FILNUM(IF)).EQ.3) THEN
                DTFIL(ISTA,IF)=DELTAT(1,IF,ILFN)+DELTAT(2,IF,ILFN)+
     1                      DNINT((OBSTIM(IF,ILFN)-OBSTIM(IFTOBS,ILFN))
     2                      *86400.D0)-DELTAT(1,IFTOBS,ILFN)
              ELSE
                DTFIL(ISTA,IF)=DELTAT(1,IF,ILFN)+DELTAT(2,IF,ILFN)+
     1                      DNINT((OBSTIM(IF,ILFN)-OBSTIM(IFTOBS,ILFN))
     2                      *86400.D0)
              ENDIF
            ENDIF
45        CONTINUE
        ELSE
          FILACT(IF)='U'
        ENDIF
50    CONTINUE
C
999   RETURN
      END SUBROUTINE
      END MODULE
