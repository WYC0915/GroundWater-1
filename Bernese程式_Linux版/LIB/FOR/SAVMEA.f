      MODULE s_SAVMEA
      CONTAINS

C*
      SUBROUTINE SAVMEA(FILRNX,FILCOD,FILPHA,IFRMAT,NFREQ ,OBSMJD,
     1                  DELTT2,EPOFLG,IOFSET,NSATEP,SATEP ,TFIRST,
     2                  TLAST ,IOK1  ,NEPFLG,NSMEA ,SVNMEA,ANZCOD,
     3                  ANZPHA,CODL12,PHAL12,CODFLG,PHAFLG,PHA001,
     4                  RNGOBS)
CC
CC NAME       :  SAVMEA
CC
CC PURPOSE    :  WRITE MEASUREMENTS OF ONE EPOCH INTO CODE AND PHASE
CC               FILE.
CC               UPDATE: NUMBER OF SATELLITES
CC                       NUMBER OF OBSERVATIONS PER SATELLITE
CC                       FIRST AND LAST OBSERVATION TIMES
CC                       PHASE JUMPS
CC
CC PARAMETERS :
CC         IN :  FILRNX : NAME OF INPUT RINEX FILE            CH*32
CC               FILCOD : NAME OF CODE HEADER FILE            CH*32
CC                          =BLANK: NO SAVE OF CODE OBSERV.
CC               FILPHA : NAME OF PHASE HEADER FILE           CH*32
CC                          =BLANK: NO SAVE OF PHASE OBSERV.
CC               IFRMAT : OBSERVATION FILE FORMAT NUMBER      I*4
CC               NFREQ(K),K=1,2: NUMBER OF FREQUENCIES FOR    I*4
CC                        CODE (K=1) AND PHASE (K=2)
CC               OBSMJD : OBSERVATION TIME (WITH FRACTION OF  R*8
CC                        SECOND) IN MJD
CC               DELTT2 : RECEIVER CLOCK CORRECTION (SEC)     R*8
CC               EPOFLG(K),K=1,2: EPOCH FLAG FOR CODE(K=1)    CH*1
CC                        AND PHASE(K=2)
CC               IOFSET : CORRECT PHASE OFFSET                I*4
CC                        =0: NO, =1: YES
CC               NSATEP : NUMBER OF SATELLITES IN EPOCH       I*4
CC               SATEP(I),I=1,NSATEP: SATELLITE NUMBERS IN    I*4
CC                        EPOCH
CC               RNGOBS : 'Y'=Range observations, 'N'=else    CH*1
CC     IN/OUT :  TFIRST(K),K=1,2: FIRST OBS. TIME(MJD)        R*8
CC                        FOR CODE(K=1), AND PHASE(K=2)
CC               TLAST(K), K=1,2: LAST  OBS. TIME(MJD)        R*8
CC                        FOR CODE(K=1), AND PHASE(K=2)
CC               IOK1(K),K=1,2: NUMBER OF CODE, PHASE RECORDS I*4
CC                        STORED IN OBSERVATION FILES
CC               NEPFLG(K),K=1,2: NUMBER OF EPOCH FLAGS       I*4
CC                        ENCOUTERED FOR CODE(K=1),PHASE(K=2)
CC               NSMEA(I),I=1,2: TOTAL NUMBER OF SATELLITES   I*4
CC                        FOR CODE(I=1) AND PHASE(I=2)
CC               SVNMEA(K,I),K=1,2,..,NSAT, I=1,2: SVN        I*4
CC                        NUMBERS FOR ALL SATELLITES IN
CC                        CURRENT FILE (CODE: I=1, PHASE: I=2)
CC               ANZCOD(K,I),K=1,..,MXCSAT, I=1,2: NUMBER OF  I*4
CC                        CODE  OBSERVATIONS FOR CARRIER I AND
CC                        SATELLITE K
CC               ANZPHA(K,I),K=1,..,MXCSAT, I=1,2: NUMBER OF  I*4
CC                        PHASE OBSERVATIONS FOR CARRIER I AND
CC                        SATELLITE K
CC               CODL12(K,I),K=1,..,MXCSAT, I=1,2: CODE OBS.  R*8
CC                        FOR CARRIER I AND SAT. K
CC               PHAL12(K,I),K=1,..,MXCSAT, I=1,2: PHASE OBS. R*8
CC                        FOR CARRIER I AND SAT. K
CC               CODFLG(K,I),K=1,..,MXCSAT, I=1,2: CODE FLAG  CH*1
CC                        FOR CARRIER I AND SAT. K
CC               PHAFLG(K,I),K=1,..,MXCSAT, I=1,2: PHASE FLAG CH*1
CC                        FOR CARRIER I AND SAT. K
CC               PHA001(K,I),K=1,..,MXCSAT, I=1,2: PHASE      R*8
CC                        INITIAL VALUE
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/01/15 16:01
CC
CC CHANGES    :   5-JUN-93 : ??: ROUND EPOCH TO NEAREST MICROSEC
CC                               (BEFORE IT WAS TO NEAREST 0.1 MILLISEC)
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               18-AUG-95 : MR: ROUND "FRACTI" TO 10 MICROSEC
CC               30-OCT-95 : MR: ROUND "FRACTI" TO 10 MICROSEC, BUT
CC                               ONLY FOR INTEGER MILLISEC OBS.TIMES
CC               16-APR-96 : TS: USE SATCRUX TO MARK OR ELIMINATE OBSERVATIONS
CC               23-AUG-96 : TS: MAXBAD IN INCLUDE FILE
CC               22-OCT-97 : HH: INCLUDE "MAXSAT" AND "COMFREQ" FOR GLONASS
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               29-MAR-04 : CU: FOR RANGE MEASUREMENTS: SAVE MICROSEC
CC                               SEPARATELY (deltat(2)) IN OBS FILE
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               26-FEB-08 : RD: USE GTSATB FROM D_SATCRX
CC               07-OCT-10 : SL: USE M_BERN WITH ONLY, SAT PROBLEM TO LFNERR
CC               23-NOV-10 : RD: 4 DIGIT YEARS IN WARNING MESSAGE
CC               05-JAN-11 : RD: ADD RINEX NAME TO THE SATCRUX-MESSAGE
CC               09-FEB-11 : RD: MESSAGE ON CLOCK OFFSET ONLY ONCE PER FILE
CC               25-JAN-12 : RD: REVISE FORMAT FOR SATCRUX MESSAGE
CC               16-FEB-12 : RD: SKIP UNKNOWN RECORDS FROM GTSATB
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfn001, lfn002, lfnErr
      USE m_maxdim, ONLY: MAXSAT,MAXBAD
      USE d_satcrx, ONLY: gtsatb
      USE f_tstflg
      USE f_djul
      USE s_wtobsi
      USE s_maxtst
      USE s_setflg
      USE s_timst2
      USE s_exitrc
      USE s_jmt
      USE s_radgms
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IBAD  , IDAY  , IFIRST, IFRMAT, IFRQ  , IHOUR , IMIN  ,
     1          IMONTH, IOFSET, IRC   , ISACOD, ISAPHA, ISATEP, ISEC  ,
     2          IYEAR , MXCSAT, NBAD  , NSATEP, NSCOD , NSPHA
C
      REAL*8    DAY   , DELTT2, FRACTF, FRACTI, FRACTM, OBSMJD,
     1          OBSTIM, PHAJMP, SEC
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C PARAMETERS
C
C
C MAXSAT: MAXIMUM NUMBER OF BAD SATELLITES IN SATCRUX
C MAXBAD: MAXIMUM NUMBER OF BAD SATELLITES IN SATCRUX
C
C DECLARATIONS
      CHARACTER*40 TSTRNG
      CHARACTER*32 FILCOD,FILPHA,FILCRX,FILRNX,RNXSAV
      CHARACTER*14 TYPTXT(3)
      CHARACTER*12 ACTTXT(2)
      CHARACTER*6  MXNSAT
      CHARACTER*1  CODFLG(MXCSAT,*),PHAFLG(MXCSAT,*),EPOFLG(2),VORZ
      CHARACTER*1  RNGOBS
C
      REAL*8       PHAL12(MXCSAT,*),CODL12(MXCSAT,*),PHA001(MXCSAT,*)
      REAL*8       TFIRST(2),TLAST(2),DELTAT(2),OFFSET(2)
      REAL*8       TIMBAD(2,MAXBAD)
C
      INTEGER*4    ANZCOD(MXCSAT,*),ANZPHA(MXCSAT,*),IOK1(2),NEPFLG(2)
      INTEGER*4    SATEP(*),NFREQ(2),NSMEA(2),SVNMEA(MXCSAT,2)
      INTEGER*4    SVNCOD(MAXSAT),SVNPHA(MAXSAT),IPRINT(MAXBAD)
      INTEGER*4    SATBAD(MAXBAD),IOBBAD(MAXBAD),IACBAD(MAXBAD)
C
C COMMON BLOCKS
C -------------
      COMMON/MCMSAT/MXCSAT,MXNSAT
      INCLUDE 'COMFREQ.inc'
C
      DATA IFIRST/1/
      DATA OFFSET/6.D3,-7.6D3/
      DATA TYPTXT/'BAD PHASE     ','BAD CODE      ','BAD PHASE+CODE'/
      DATA ACTTXT/'OBS. MARKED ','OBS. REMOVED'/
C
C CHECK MAXIMAL LOCAL DIMENSIONS
C ------------------------------
      IF(IFIRST.EQ.1) THEN
        IFIRST=0
        CALL MAXTST(1,'SAVMEA',MXNSAT,MAXSAT,MXCSAT,IRC)
        IF(IRC.NE.0) CALL EXITRC(2)
C
C GET SATCRUX INFORMATION
C -----------------------
        CALL GTFLNA(0,'SATCRUX',FILCRX,IRC)
        CALL GTSATB(MAXBAD,FILCRX,NBAD,SATBAD,IOBBAD,IACBAD,
     1              TIMBAD)
        DO 90 IBAD=1,NBAD
          IF (IOBBAD(IBAD).GT.0.AND.IOBBAD(IBAD).LE.SIZE(TYPTXT).AND.
     1        IACBAD(IBAD).GT.0.AND.IACBAD(IBAD).LE.SIZE(ACTTXT))THEN
            IPRINT(IBAD)=0
          ELSE
            IPRINT(IBAD)=-1
          ENDIF
90      CONTINUE
C
        RNXSAV=''
      ENDIF
C
C OBSERVATION TIME (INTEGER SECONDS) AND FRACTION OF SECOND
C ---------------------------------------------------------
C
C WEEKEND PROBLEM
      IF(TFIRST(1).NE.1.D20.AND.OBSMJD-TLAST(1).LT.-6.D0) THEN
        OBSMJD=OBSMJD+7.D0
      ENDIF
C
C CONVERSION
      CALL JMT(OBSMJD,IYEAR,IMONTH,DAY)
      IDAY=IDINT(DAY)
      CALL RADGMS(3,DAY,VORZ,IHOUR,IMIN,SEC)
      ISEC=IDNINT(SEC)
      DAY=IDAY+IHOUR/24.D0+IMIN/1440.D0+ISEC/86400.D0
      OBSTIM=DJUL(IYEAR,IMONTH,DAY)
      FRACTI=(OBSMJD-OBSTIM)*86400.D0
C
C ROUND FRACTION OF SECOND IN OBSERVATION TIME, IF YOU CAN ASSUME THAT
C THE OBSERVATION TIME SHOULD BE IN FULL MILLISECOND
C
      FRACTM=DNINT(FRACTI*1.D3)/1.D3
      IF (DABS(FRACTM-FRACTI).LT.2.D-6) THEN
        FRACTI=DNINT(FRACTI*1.D5)/1.D5
      ENDIF
C
C For RANGE measurements:
C -----------------------
C   RINEX files created from QLRINEXO contain (INPUT):
C     obstim ... epoch including full seconds (in mjd)
C     deltt2 ... fraction of second (in sec)
C   BERNESE files will contain (OUTPUT):
C     obstim    = yy mm dd hh mm ss - from obstim
C     deltat(1) = millisec(sec)         - from deltt2
C     deltat(2) = microsec(sec)         - from deltt2
C   The separation of millisec and microsec is necessary for further processing
C   not to loose the microsec by computing tobs in units of mjd due to R*8:
C     The nominal epoch computed in GOBSEP (called by PRCEPO) is:
C       tobs  = obstim + deltat(1) in mjd
C     The correction from nominal epoch to epoch computed in GOBSEP is:
C       dtfil = deltat(2) in sec(microsec from RINEX file are still available!)
C
C For CODE and PHASE measurements:
C --------------------------------
C   RINEX files contain (INPUT):
C     obstim ... epoch including fraction of second (in mjd)
C     deltt2 ... receiver clock offset (=0)
C   BERNESE files will contain (OUTPUT):
C     obstim    = yy mm dd hh mm ss         - from obstim
C     deltat(1) = fraction of seconds (sec) - from obstim
C     deltat(2) = receiver clock offset (=0)- from deltt2
C   deltt2 is the receiver clock offset, which is assumed to be 0, otherwise:
C   The RINEX header keyword "RCV CLOCK OFFS APPL" should indicate, if
C   code and phase are corrected by applying the receiver clock offset.
C   Until now the software is not able to read this keyword. Therefore it is
C   assumed that the observations in the RINEX file are not corrected
C   by applying this clock offset - a warning message appears.
C   Splitting up of deltt2 is not allowed.
C
      IF (RNGOBS.EQ.'Y') THEN  ! RANGE
        FRACTM    = DNINT(DELTT2*1.D3)/1.D3
        FRACTF    = DELTT2-FRACTM
        DELTAT(1) = FRACTI + FRACTM
        DELTAT(2) = FRACTF
C
      ELSE                     ! CODE or PHASE
        DELTAT(1) = FRACTI
        DELTAT(2) = DELTT2
        IF (DELTT2.NE.0d0 .AND.RNXSAV.NE.FILRNX) THEN
          WRITE(lfnerr,'(/,A,/,6(16X,A,/))')
     1    ' ### SR SAVMEA: Receiver clock offset in RINEX file found.',
     2    'It is assumed, that the observations in the ',
     3    'RINEX file are not corrected by applying the',
     4    'receiver clock offset. Please check the ',
     5    'keyword "RCV CLOCK OFFS APPL" in the RINEX file',
     6    'header!',
     7    'RINEX FILENAME: ' // TRIM(FILRNX)
          RNXSAV = FILRNX
        ENDIF
      ENDIF
C
C LOOP OVER ALL SATELLITES OF EPOCH
C ---------------------------------
      NSCOD=0
      NSPHA=0
      DO 100 ISATEP=1,NSATEP
C
C MARK OUTLIERS
C -------------
CC      IF(CODL12(ISATEP,       1).GT.1.D9.OR.
CC   1     CODL12(ISATEP,       1).LT.0.D0.OR.
CC   2     CODL12(ISATEP,NFREQ(1)).GT.1.D9.OR.
CC   3     CODL12(ISATEP,NFREQ(1)).LT.0.D0)      THEN
        IF(CODL12(ISATEP,       1).GT.1.D9.OR.
     2     CODL12(ISATEP,NFREQ(1)).GT.1.D9)      THEN
C
          CODL12(ISATEP,       1)=0.D0
          CODL12(ISATEP,NFREQ(1))=0.D0
          PHAL12(ISATEP,       1)=0.D0
          PHAL12(ISATEP,NFREQ(2))=0.D0
        ENDIF
C
C HANDLE "SATCRUX" REQUESTS
C -------------------------
        DO 110 IBAD=1,NBAD
C
C         UNKNOWN PROBLEM/ACTION CODE
          IF (IPRINT(IBAD).LT.0) GOTO 110
C
          IF ((SATEP(ISATEP).EQ.SATBAD(IBAD)) .AND.
     1         (OBSMJD.GE.TIMBAD(1,IBAD))     .AND.
     2         (OBSMJD.LE.TIMBAD(2,IBAD)))     THEN
C
C PRINT MESSAGE ON FIRST MARKING
C ------------------------------
            IF (IPRINT(IBAD).EQ.0) THEN
              IPRINT(IBAD)=1
              CALL TIMST2(1,2,TIMBAD(1:2,IBAD),TSTRNG)
              WRITE(LFNERR,104)SATBAD(IBAD),TRIM(FILCRX),
     1              TYPTXT(IOBBAD(IBAD)),ACTTXT(IACBAD(IBAD)),
     2              TSTRNG,TRIM(FILRNX)
104           FORMAT(/,' ### SR SAVMEA: PROBLEM FOR SATELLITE: ',I4
     1               /,16X,'INDICATED IN SATCRUX : ',A,
     2               /,16X,'PROBLEM              : ',A14,
     3               /,16X,'REQUESTED ACTION     : ',A12,
     4               /,16X,'TIME WINDOW          : ',A40,
     5               /,16X,'IN RINEX FILE        : ',A,/)
            ENDIF
C
C MARK OBSERVATIONS
C -----------------
            IF (IACBAD(IBAD).EQ.1) THEN
              IF (IOBBAD(IBAD).EQ.1) THEN
                CALL SETFLG(PHAFLG(ISATEP,1),0)
                CALL SETFLG(PHAFLG(ISATEP,NFREQ(2)),0)
              ELSE IF (IOBBAD(IBAD).EQ.2) THEN
                CALL SETFLG(CODFLG(ISATEP,1),0)
                CALL SETFLG(CODFLG(ISATEP,NFREQ(2)),0)
              ELSE IF (IOBBAD(IBAD).EQ.3) THEN
                CALL SETFLG(PHAFLG(ISATEP,1),0)
                CALL SETFLG(PHAFLG(ISATEP,NFREQ(2)),0)
                CALL SETFLG(CODFLG(ISATEP,1),0)
                CALL SETFLG(CODFLG(ISATEP,NFREQ(2)),0)
              ENDIF
C
C REMOVE OBSERVATIONS
C -------------------
            ELSE IF (IACBAD(IBAD).EQ.2) THEN
              IF (IOBBAD(IBAD).EQ.1) THEN
                PHAL12(ISATEP,       1)=0.D0
                PHAL12(ISATEP,NFREQ(2))=0.D0
              ELSE IF (IOBBAD(IBAD).EQ.2) THEN
                CODL12(ISATEP,       1)=0.D0
                CODL12(ISATEP,NFREQ(1))=0.D0
              ELSE IF (IOBBAD(IBAD).EQ.3) THEN
                PHAL12(ISATEP,       1)=0.D0
                PHAL12(ISATEP,NFREQ(2))=0.D0
                CODL12(ISATEP,       1)=0.D0
                CODL12(ISATEP,NFREQ(1))=0.D0
              ENDIF
            ENDIF
          ENDIF
110     ENDDO
C
C NEW SATELLITE IN CODE
C ---------------------
        DO 10 ISACOD=1,NSMEA(1)
          IF(SVNMEA(ISACOD,1).EQ.SATEP(ISATEP)) GOTO 13
10      CONTINUE
        IF(CODL12(ISATEP,    1).NE.0.D0.OR.
     1     CODL12(ISATEP,NFREQ(1)).NE.0.D0) THEN
          NSMEA(1)=NSMEA(1)+1
          SVNMEA(NSMEA(1),1)=SATEP(ISATEP)
          ISACOD=NSMEA(1)
C
C CHECK MAXIMUM NUMBER OF SATELLITES ALLOWED
          IF(NSMEA(1).GT.MXCSAT) THEN
            WRITE(LFNERR,901) FILRNX,NSMEA(1),MXCSAT
901         FORMAT(/,' *** SR SAVMEA: TOO MANY SATELLITES',/,
     1                           16X,'IN RINEX FILE         :',A32,/,
     1                           16X,'NUMBER OF SATELLITES  :',I3,/,
     2                           16X,'MAXIMUM NUMBER OF SAT.:',I3,/)
            CALL EXITRC(2)
          ENDIF
        ENDIF
C
C NEW SATELLITE IN PHASE
C ----------------------
13      DO 15 ISAPHA=1,NSMEA(2)
          IF(SVNMEA(ISAPHA,2).EQ.SATEP(ISATEP)) GOTO 20
15      CONTINUE
        IF(PHAL12(ISATEP,    1).NE.0.D0.OR.
     1     PHAL12(ISATEP,NFREQ(2)).NE.0.D0) THEN
          NSMEA(2)=NSMEA(2)+1
          SVNMEA(NSMEA(2),2)=SATEP(ISATEP)
          ISAPHA=NSMEA(2)
C
C CHECK MAXIMUM NUMBER OF SATELLITES ALLOWED
          IF(NSMEA(2).GT.MXCSAT) THEN
            WRITE(LFNERR,901) NSMEA(2),MXCSAT
            CALL EXITRC(2)
          ENDIF
        ENDIF
C
C CODE OBSERVATIONS AVAILABLE ?
C -----------------------------
20      IF(CODL12(ISATEP,1).NE.0.D0.OR.
     1     CODL12(ISATEP,NFREQ(1)).NE.0.D0) THEN
          NSCOD=NSCOD+1
          SVNCOD(NSCOD)=SVNMEA(ISACOD,1)
C
C FIRST AND LAST OBSERVATION TIME
          IF(NSCOD.EQ.1)THEN
            TLAST(1)=OBSTIM
            IF(TFIRST(1).EQ.1.D20) TFIRST(1)=OBSTIM
          END IF
          DO 30 IFRQ=1,NFREQ(1)
            CODL12(NSCOD,IFRQ)=CODL12(ISATEP,IFRQ)
            CODFLG(NSCOD,IFRQ)=CODFLG(ISATEP,IFRQ)
            IF(CODL12(NSCOD,IFRQ).NE.0.D0)
     1        ANZCOD(ISACOD,IFRQ)=ANZCOD(ISACOD,IFRQ)+1
30        CONTINUE
        ENDIF
C
C PHASE OBSERVATIONS AVAILABLE ?
C ------------------------------
        IF(PHAL12(ISATEP,1).NE.0.D0.OR.
     1     PHAL12(ISATEP,NFREQ(2)).NE.0.D0) THEN
          NSPHA=NSPHA+1
          SVNPHA(NSPHA)=SVNMEA(ISAPHA,2)
C
C FIRST AND LAST OBSERVATION TIME
          IF(NSPHA.EQ.1)THEN
            TLAST(2)=OBSTIM
            IF(TFIRST(2).EQ.1.D20) TFIRST(2)=OBSTIM
          END IF
          DO 40  IFRQ=1,NFREQ(2)
            PHAL12(NSPHA,IFRQ)=PHAL12(ISATEP,IFRQ)
            PHAFLG(NSPHA,IFRQ)=PHAFLG(ISATEP,IFRQ)
            IF(PHAL12(NSPHA,IFRQ).NE.0.D0) THEN
              ANZPHA(ISAPHA,IFRQ)=ANZPHA(ISAPHA,IFRQ)+1
C
C REMOVE NOMINAL FREQUENCY OFFSETS (6000 HZ FOR L1, -7600 HZ FOR L2)
              IF(IOFSET.EQ.1) THEN
                PHAL12(NSPHA,IFRQ)=PHAL12(NSPHA,IFRQ)+OFFSET(IFRQ)*
     1             86400.D0*(OBSTIM-TFIRST(2))*WLGT(IFRQ,SATEP(ISATEP))
              ENDIF
C
C INITIALIZE THE CONTINUOUS PHASE SUBTRACTING A INTEGER NUMBER OF CYCLES
              IF(PHA001(ISAPHA,IFRQ).EQ.0.01D0) THEN
                PHA001(ISAPHA,IFRQ)=
     1           -DNINT(PHAL12(NSPHA,IFRQ)/WLGT(IFRQ,SATEP(ISATEP)))
     2                 *WLGT(IFRQ,SATEP(ISATEP))
              ENDIF
              PHAL12(NSPHA,IFRQ)=PHAL12(NSPHA,IFRQ)+PHA001(ISAPHA,IFRQ)
C
C AVOID FORMAT OVERFLOW (INTRODUCTION OF A CYCLE SLIP)
              IF(PHAL12(NSPHA,IFRQ).GE.1.D9.OR.
     1           PHAL12(NSPHA,IFRQ).LE.-1.D8) THEN
                PHAJMP=-DNINT(PHAL12(NSPHA,IFRQ)
     1                 /WLGT(IFRQ,SATEP(ISATEP)))
     2                 *WLGT(IFRQ,SATEP(ISATEP))
                PHA001(ISAPHA,IFRQ)=PHA001(ISAPHA,IFRQ)+PHAJMP
                PHAL12(NSPHA,IFRQ)=PHAL12(NSPHA,IFRQ)+PHAJMP
                CALL SETFLG(PHAFLG(NSPHA,IFRQ),1)
                WRITE(LFNERR,902) FILRNX,IFRQ,SVNMEA(ISAPHA,2),
     1                            PHAJMP/WLGT(IFRQ,SATEP(ISATEP))
902             FORMAT(/,' ### SR SAVMEA: JUMP INTRODUCED INTO PHASES '
     1                            ,/,16X,'DUE TO FORMAT OVERFLOW',/,
     2                               16X,'RINEX FILE   : ',A32,/,
     2                               16X,'FREQUENCY    : L',I1,/,
     3                               16X,'SATELLITE    :',I3,/,
     4                               16X,'JUMP (CYCLES):',F15.1,/)
              ENDIF
            ENDIF
40        CONTINUE
        END IF
100   CONTINUE
C
C WRITE CODE RECORD
C -----------------
      IF(FILCOD.NE.' ') THEN
        CALL WTOBSI(LFN001,IFRMAT,NFREQ(1),OBSTIM,DELTAT,EPOFLG(1),
     1              NSCOD,SVNCOD,CODFLG,CODL12)
        IF(NSCOD.NE.0) THEN
          IOK1(1)=IOK1(1)+1
          IF(TSTFLG(EPOFLG(1),0)) NEPFLG(1)=NEPFLG(1)+1
        ENDIF
      ENDIF
C
C WRITE PHASE RECORD
C ------------------
      IF(FILPHA.NE.' ') THEN
        CALL WTOBSI(LFN002,IFRMAT,NFREQ(2),OBSTIM,DELTAT,EPOFLG(2),
     1              NSPHA,SVNPHA,PHAFLG,PHAL12)
        IF(NSPHA.NE.0) THEN
          IOK1(2)=IOK1(2)+1
          IF(TSTFLG(EPOFLG(2),0)) NEPFLG(2)=NEPFLG(2)+1
        ENDIF
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
