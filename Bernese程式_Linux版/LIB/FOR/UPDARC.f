      MODULE s_UPDARC
      CONTAINS

C*
      SUBROUTINE UPDARC(MAXSLP,MAXARC,DTSIM ,GAPARC,MINOBS,NOBS  ,
     1                  LINTIM,LINOBS,LINFLG,NEVT  ,TIMEVT,SIZEVT,
     2                  SYSEVT,SVN   ,FILNAM,NARC  ,NOBA  ,NBAD  ,
     3                  ARCTIM,NSLIP ,SLPTIM,SLPCYC,IRC)
CC
CC NAME       :  UPDARC
CC
CC PURPOSE    :  UPDATES THE ARC INFORMATION.
CC               CYCLE SLIP FLAGS ARE SET ALSO AT "BAD" EPOCHS
CC               THE SR UPDOBS WILL TAKE CARE OF THAT
CC
CC PARAMETERS :
CC        IN  :  MAXSLP : MAXIMUM NUMBER OF CYCLE SLIPS PER   I*4
CC                        ARC
CC            :  MAXARC : MAXIMUM NUMBER OF ARCS              I*4
CC            :  DTSIM  : TOLERANCE FOR ONE EPOCH (IN DAYS)   R*8
CC            :  GAPARC : MAXIMUM TIME GAP BETWEEN PIECES     R*8
CC                        (HOURS)
CC            :  MINOBS : MINIMAL NUMBER OF OBSERVATIONS      I*4
CC            :  NOBS   : NUMBER OF OBSERVATIONS              I*4(*)
CC            :  LINTIM : ARRAY WITH TIME VALUES (HOURS)      R*8(*)
CC            :  LINOBS : ARRAY WITH OBSERVATIONS             R*8(*)
CC            :  LINFLG : ARRAY WITH OBSERVATION FLAGS       CH*1(*)
CC                        BIT=0: OUTLIER
CC                        BIT=1: NEW AMBIGUITY
CC                        BIT=2: CORRECTED CYCLE SLIP
CC               NEVT   : NUMBER OF CLOCK EVENTS              I*4
CC               TIMEVT : EPOCH OF CLOCK EVENT                R*8(*)
CC                        (TIME IN HOURS SINCE DNINT(OBSTIM(1))
CC               SIZEVT : SIZE OF THE EVENT IN METERS         R*8(*)
CC                        (1D20 IF UNKNOWN SIZE)
CC               SYSEVT : SATELLITE SYSTEM CHARACTER FROM     CH*1(*)
CC                        G_SVNSYS
CC               SVN    : SATELLITE NUMBER                    I*4
CC               FILNAM : RINEX FILENAME                      CH*
CC     IN/OUT :  NARC   : NUMBER OF ARCS                      I*4(*)
CC            :  NOBA   : NUMBER OF OBSERVATIONS IN ARC       I*4(*)
CC            :  NBAD   : NUMBER OF BAD OBSERVATIONS IN ARC   I*4(*)
CC               ARCTIM : INDEX TIME OF THE ARC START AND END I*4(MAXARC,2)
CC               NSLIP  : NUMBER OF REPAIRED CYCLE SLIPS      I*4(*)
CC               SLPTIM : INDEX TIME OF THE CYCLE SLIP        I*4(*)
CC               SLPCYC : SIZE OF THE CYCLE SLIPS             R*8(*)
CC               IRC    : RETURN CODE                         I*4
CC                        0: OK; 1: DIMENSION EXCEEDED
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  T.A. SPRINGER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  22-JUL-1996
CC
CC CHANGES    :  06-FEB-97 : ??: NOK=NOK+1 IN CHK IF OBS IS O.K.
CC               19-JAN-04 : RD: HANDLE CLOCK EVENTS (SPLIT ARCS)
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               02-DEC-08 : RD: CONTINUE WITH NEXT FILE IN CASE OF ERROR
CC               28-MAR-12 : RD: USE SVN2CHR AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern,  ONLY: LFNERR
      USE f_tstflg
      USE s_svn2chr
      USE s_setflg
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IARC  , IEVT  , IOBS  , IS    , ISLIP , ISNEW ,
     1          ISOLD , MAXARC, MAXSLL, MAXSLP, MINOBS, NARC  , NEVT  ,
     2          NOBS  , NOK   , NSLP  , IRC
C
      REAL*8    DTSIM , ENDTIM, GAPARC, TIMDIF
C
CCC       IMPLICIT  REAL*8(A-H,O-Z)
      PARAMETER(MAXSLL=2000)
C
      REAL*8      LINTIM(*),LINOBS(*)
      REAL*8      SLPCYC(MAXSLP,MAXARC,5)
      REAL*8      SLSIZE(MAXSLL,5)
      REAL*8      TIMEVT(*),SIZEVT(*)
C
      INTEGER*4   NOBA(*),NBAD(*),NSLIP(*)
      INTEGER*4   SLPTIM(MAXSLP,MAXARC),ARCTIM(MAXARC,2)
      INTEGER*4   SLPIND(MAXSLL)
      INTEGER*4   SVN,SVNNUM
C
      CHARACTER*1 LINFLG(*),SYSEVT(*),SVNCHR
      CHARACTER(LEN=*)  FILNAM
C
      LOGICAL*4   ISEVT
C
C
C SAVE ORIGINAL CYCLE SLIP INDICES
C --------------------------------
      IRC =0
      NSLP=0
      DO 10 IARC=1,NARC
        DO 20 ISLIP=1,NSLIP(IARC)
          NSLP=NSLP+1
          SLPIND(NSLP)=ARCTIM(IARC,1)+SLPTIM(ISLIP,IARC)-1
          SLSIZE(NSLP,1)=SLPCYC(ISLIP,IARC,1)
          SLSIZE(NSLP,2)=SLPCYC(ISLIP,IARC,2)
          SLSIZE(NSLP,3)=SLPCYC(ISLIP,IARC,3)
          SLSIZE(NSLP,4)=SLPCYC(ISLIP,IARC,4)
          SLSIZE(NSLP,5)=SLPCYC(ISLIP,IARC,5)
20      CONTINUE
10    CONTINUE
C
C CHECK MAXSLL
C ------------
      IF (NSLP.GT.MAXSLL) THEN
        WRITE(LFNERR,901)NSLP,MAXSLL
901     FORMAT(/,' *** SR UPDARC: MAXSLL EXCEEDED ',/,
     1                      16X,' NUMBER OF SLIPS : ',I4,/,
     2                      16X,' MAXIMUM NUMBER  : ',I4,/)
        CALL EXITRC(2)
      ENDIF
C
C INITIALIZE FIRST ARC
C --------------------
      NARC=1
      NOK=0
      NOBA(NARC)=0
      NBAD(NARC)=0
      NSLIP(NARC)=0
      ARCTIM(NARC,1)=1
      ARCTIM(NARC,2)=1
      CALL SETFLG(LINFLG(1),1)
      ENDTIM=LINTIM(1)
      CALL SVN2CHR(SVN,SVNNUM,SVNCHR)
C
C LOOP OVER ALL OBSERVATIONS OF THIS SATELLITE
C --------------------------------------------
      DO 110 IOBS=1,NOBS
C
C CHECK FOR NEW ARC DUE TO GAP OR CLOCK EVENT OR UNCORRECTED CYCLE SLIP
C ---------------------------------------------------------------------
        TIMDIF=LINTIM(IOBS)-ENDTIM
C
        ISEVT=.FALSE.
        DO IEVT=1,NEVT
          IF (SIZEVT(IEVT).NE.1D20) CYCLE
          IF ((SYSEVT(IEVT).EQ.'*'.OR.SYSEVT(IEVT).EQ.SVNCHR).AND.
     1         DABS(TIMEVT(IEVT)-LINTIM(IOBS)).LE.DTSIM) ISEVT=.TRUE.
          IF (TIMEVT(IEVT)-LINTIM(IOBS).GT.DTSIM) EXIT
        ENDDO
C
        IF ((TIMDIF.GT.GAPARC) .OR. ISEVT .OR.
     1       TSTFLG(LINFLG(IOBS),1).AND.TSTFLG(LINFLG(IOBS),2)) THEN
C
C CHECK IF OLD ARC HAD ENOUGH OBSERVATIONS
C ----------------------------------------
          IF (NOK.LT.MINOBS) THEN
            NBAD(NARC)=0
            DO 120 I=ARCTIM(NARC,1),ARCTIM(NARC,2)
              NBAD(NARC)=NBAD(NARC)+1
              CALL SETFLG(LINFLG(I),0)
              LINOBS(I)=0D0
120         CONTINUE
          ENDIF
C
C INITIALIZE NEW ARC
C ------------------
          NARC=NARC+1
          NOK=0
          NOBA(NARC)=0
          NBAD(NARC)=0
          NSLIP(NARC)=0
          ARCTIM(NARC,1)=IOBS
          CALL SETFLG(LINFLG(IOBS),1)
        ENDIF
C
C UPDATE ARC PARAMETERS: OBSERVATIONS IN ARC, END TIME OF ARC, NUMBER OF SLIPS
C ----------------------------------------------------------------------------
        NOBA(NARC)=NOBA(NARC)+1
        ARCTIM(NARC,2)=IOBS
        IF (TSTFLG(LINFLG(IOBS),2)) THEN
          NSLIP(NARC)=NSLIP(NARC)+1
          SLPTIM(NSLIP(NARC),NARC)=NOBA(NARC)
        ENDIF
C
C CHECK IF OBSERVATION IS O.K.
C ----------------------------
        IF (TSTFLG(LINFLG(IOBS),0)) THEN
          NBAD(NARC)=NBAD(NARC)+1
        ELSE
          NOK=NOK+1
        ENDIF
        ENDTIM=LINTIM(IOBS)
110   CONTINUE
C
C CHECK IF LAST ARC HAD ENOUGH OBSERVATIONS
C -----------------------------------------
      IF (NOK.LT.MINOBS) THEN
        NBAD(NARC)=0
        DO 130 I=ARCTIM(NARC,1),ARCTIM(NARC,2)
          NBAD(NARC)=NBAD(NARC)+1
          CALL SETFLG(LINFLG(I),0)
          LINOBS(I)=0D0
130     CONTINUE
      ENDIF
C
C CHECK MAXARC
C ------------
      IF (NARC.GT.MAXARC) THEN
        WRITE(LFNERR,902)TRIM(FILNAM),NARC,MAXARC
902     FORMAT(/,' ### SR UPDARC: ' //
     1           'MAXARC EXCEEDED, NO FURTHER PROCESSING ',/,
     2           16X,' FILENAME       : ',A,/,
     3           16X,' NUMBER OF ARC  : ',I4,/,
     4           16X,' MAXIMUM NUMBER : ',I4,/)
        IRC=1
        RETURN
      ENDIF
C
C RESTORE ORIGINAL CYCLE SLIP SIZES
C ---------------------------------
      DO 1010 IARC=1,NARC
        DO 1020 ISNEW=1,NSLIP(IARC)
C
C CHECK MAXSLP
C ------------
          IF (NSLIP(IARC).GT.MAXSLP) THEN
            WRITE(LFNERR,903)TRIM(FILNAM),IARC,NSLIP(IARC),MAXSLP
903         FORMAT(/,' ### SR UPDARC: ' //
     1               'MAXSLP EXCEEDED, NO FURTHER PROCESSING ',/,
     2          16X,' FILENAME       : ',A,/,
     3          16X,' ARC INDEX       : ',I4,/,
     4          16X,' NUMBER OF SLIPS : ',I4,/,
     5          16X,' MAXIMUM NUMBER  : ',I4,/)
            IRC=1
            RETURN
          ENDIF
C
C FIND CORRESPONDING CYCLE SLIP INDICES
C -------------------------------------
          IS=ARCTIM(IARC,1)+SLPTIM(ISNEW,IARC)-1
          DO 1030 ISOLD=1,NSLP
            IF (IS.EQ.SLPIND(ISOLD)) THEN
              SLPCYC(ISNEW,IARC,1)=SLSIZE(ISOLD,1)
              SLPCYC(ISNEW,IARC,2)=SLSIZE(ISOLD,2)
              SLPCYC(ISNEW,IARC,3)=SLSIZE(ISOLD,3)
              SLPCYC(ISNEW,IARC,4)=SLSIZE(ISOLD,4)
              SLPCYC(ISNEW,IARC,5)=SLSIZE(ISOLD,5)
              GOTO 1020
            ENDIF
1030    CONTINUE
1020    CONTINUE
1010  CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
