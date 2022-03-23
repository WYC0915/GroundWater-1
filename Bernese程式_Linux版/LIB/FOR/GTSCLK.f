      MODULE s_GTSCLK
      CONTAINS

C*
      SUBROUTINE GTSCLK(RNXCLK,EPOCH,SATNUM,SECIPL,ISTOP,DTSATC,IRCODE)
CC
CC NAME       :  GTSCLK
CC
CC PURPOSE    :  GET SATELLITE CLOCK CORRECTION FOR ONE EPOCH AND ONE
CC               SATELLITE FROM THE SATELLITE CLOCK FILE
CC               THE SATELLITE CLOCK FILE IS READ ON THE FIRST CALL
CC
CC PARAMETERS :
CC         IN :  RNXCLK:  WHAT TO DO IF NO INPUT CLOCK RINEX       I*4
CC                        FOR SATELLITE CLOCKS:
CC                         -1: IGNORE CLOCK RINEX FILE
CC                          0: TRY ALSO SAT CLK FILE
CC                          1: USE OBS. (INTERPOL. CLK RNX)
CC                          2: SKIP OBS.
CC                          3: USE OBS. (SAT CLK = ZERO)
CC               EPOCH  : EPOCH (MJD)                              R*8
CC               SATNUM : SPACE VEHICLE NUMBER  OF SATELLITE       I*4
CC               SECIPL : LENGTH OF ALLOWED INTERPOLATION INTERVAL R*8
CC                        =0.D0 : NO INTERPOLATION ALLOWED
CC                        /=0.D0: NUMBER OF SECONDS
CC               ISTOP  : FLAG FOR STOP ON ERROR              I*4
CC                        =0 : NO STOP, RETURN CODE SET
CC                        =1 : STOP ON ERROR
CC                        =2 : NO STOP, IF SATELLITE MISSING.
CC                             PRINT WARNING ONCE PER SATEL.
CC        OUT :  DTSATC : SATELLITE CLOCK CORRECTION IN SEC        R*8
CC               IRCODE : RETURN CODE                              I*4
CC                        =0: OK
CC                        =1: NEXT SAT. CLOCK SET NOT NEAR ENOUGH
CC                        =2: NO SATELLITE CLOCK VALUES
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  90/07/05 09:11
CC
CC CHANGES    :  12-MAR-92 : ??: OPEN SAT.CLOCK FILE WITH "OPNFIL"
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               17-AUG-94 : MR: REMOVE SUBTRACTION OF 1800 SEC FROM
CC                               "EPOCH" WHEN SEARCHING FOR NEAREST SET
CC               06-SEP-94 : MR: OPTION "ISTOP" AND RETURN CODE
CC               30-AUG-95 : TS: SMALLER EPOCH DISTANCE WHEN NCLK=1
CC               26-APR-96 : TS: LARGER EPOCH DISTANCE (3 DAYS) NCLK > 1
CC                               USEFULL FOR CLOCK "PREDICTIONS"
CC               20-MAY-96 : TS: POLYNOMIAL EPOCH INTERPRETED AS START EPOCH
CC               12-JUL-96 : TS: CORRECTED SMALL INTERPRETATION BUG
CC               06-FEB-97 : ??: PARAMETERS CHANGED
CC               23-SEP-97 : DI: USE MAXSAT.inc
CC               30-DEC-97 : TS: MAKE SURE DTSATC=0.0D0 IF NO CLOCK FOUND
CC               31-AUG-98 : TS: CORRECT USE OF "NCLK" IN 150 LOOP
CC               16-AUG-99 : RD: USE MAXSAC.inc
CC               16-AUG-99 : RD: DIMENSIONS (SMALL/MEDIUM/LARGE)
CC               22-NOV-00 : TS: INCREASED MAXEPH FROM 1100 TO 3000
CC               02-JUL-02 : HU: ERROR MESSAGE IF ERROR READING FILE
CC               12-DEC-02 : RD: WRITE EPOCH STRINGS FOR ERROR MESSAGES
CC               17-FEB-03 : LM: USE M_MAXDIM, PREPROCESSOR COMMANDS
CC               07-FEB-04 : HU: TIME RANGE INCREASED 0.004/24 -> 0.0043/24
CC               16-MAR-04 : SS: DISMAX FROM 3 TO 6 DAYS
CC               19-APR-05 : MP/RD: BUFFER IS ALLOCATABLE
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               08-Aug-05 : HB: Use new SR TIMST2 (module)
CC               07-SEP-06 : RD: WARNING FOR EXTRAPOLATION
CC               11-Dec-07 : ===== MERGE: USE VERSION FROM HPF-VERSION
CC               MODIFICATIONS IN HPF-VERSION:
CC               17-FEB-04 : DS: CLOCK INTERPOLATION IMPLEMENTED
CC               17-FEB-04 : DS: CLOCK EXTRAPOLATION IMPLEMENTED
CC               17-FEB-04 : DS: CHECK FOR NCLK=0
CC               17-FEB-04 : DS: SAMPLING OF THE CLOCK FILE USED TO TEST
CC                               THE DISTANCE TO THE NEAREST EPOCH (NCLK=1)
CC               29-JUL-05 : HU: NO INTERPOLATION IF EPOCHS MISSING
CC               28-FEB-06 : HB: ALLOW FOR TWO EPOCHS WITH SAME EPOCH
CC                               (ORBIT OVERLAP PROCESSING)
CC               15-NOV-06 : HB: ENLARGE DISMAX FOR NCLK>1
CC               01-NOV-07 : HB: ADD PARAMETER SECIPL, OTHER MODIFICATIONS
CC                               TO ENSURE SAME BEHAVIOUR AS OFFICIAL VERSION
CC               14-MAR-08 : RD: SAVE FILENAME
CC               24-APR-08 : MM: ERROR MESSAGE CORRECTED
CC               29-MAY-09 : RD: INPUT CLOCKS ALSO FROM INPUT CLK RNX FILE
CC               04-MAR-10 : HB: DO NOT EXTRAPOLATE CLOCKS IF TOO FAR AWAY
CC                               (BUGFIX)
CC               24-AUG-11 : HB: GET CORRECT SAMPLING OF CLOCKS FOR CHECK TO
CC                               ALLOW INTERPOLATION
CC               05-MAR-12 : RD: USE LISTI4 AS MODULE NOW
CC               28-MAR-12 : RD: USE SVN2CHR AS MODULE NOW
CC               12-JUL-12 : RD: INTERPOLATION INTERVAL CHECK OMPROVED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1990     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, r8b, lfnerr, lfnloc, fileNameLength
      USE m_maxdim, ONLY: MAXSAC, MAXSAT
      USE s_gtrxck
      USE s_opnfil
      USE s_alcerr
      USE s_opnerr
      USE s_timst2
      USE s_exitrc
      USE s_gtflna
      USE f_gpsmjd
      USE f_listi4
      USE s_svn2chr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ICLK  , IEPH  , IEPMIN, INIT  , IOSTAT, IRCODE,
     1          IRCSAT, ISACLK, ISAT  , ISAT1 , ISTOP , IWEEK , IRC   ,
     2          MAXEPH, NCLK  , NPRSAT, NRSAT , NSAT  , IOS   , SVNMOD,
     3          RNXCLK
C
      REAL*8    DISMAX, DT00  , DT01  , DTSATC, DTTEST, EPOCH , T00   ,
     1          TMIN  , TOC   , SAMDIF, SECIPL, OLDTOC, DTTT, dttest2 ,
     2          DTSIM , CLOCK , SIGMA
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER(LEN=6),              PARAMETER    :: SRNAME = 'GTSCLK'
      CHARACTER(LEN=fileNameLength), SAVE         :: FILSAC
      CHARACTER*40 EPOSTR
      CHARACTER*3  SVNSTR
      CHARACTER*1  SVNCHR
      REAL(r8b),   DIMENSION(:,:)  , ALLOCATABLE, SAVE :: TSACLK        (MAXEPH,MAXSAT)
      REAL(r8b),   DIMENSION(:,:,:), ALLOCATABLE, SAVE :: SATCLK        (MAXSAC,MAXEPH,MAXSAT)
      REAL*8       SCLOCK(10),SAMPL
      INTEGER*4    SATNUM,NUMSAT(MAXSAT),NEPH(MAXSAT)
      INTEGER*4    PRTSAT(MAXSAT)
      INTEGER(i4b),DIMENSION(:,:),   ALLOCATABLE, SAVE :: NSACLK        (MAXEPH,MAXSAT)
C
C     COMMON /CGSCLK/TSACLK,SATCLK,NSACLK
C
      DATA INIT/1/
C
C USE CLOCK RINEX INPUT FILE, IF REQUESTED
C ----------------------------------------
      IF (RNXCLK.NE.-1) THEN
        DTSIM=0.1D0/86400D0
C
        CALL SVN2CHR(SATNUM,SVNMOD,SVNCHR)
        WRITE(SVNSTR,'(A1,I2.2)') SVNCHR,SVNMOD
C
        CALL GTRXCK(EPOCH,DTSIM,SVNSTR,ISTOP,SECIPL,CLOCK,SIGMA,IRCODE)
C
        IF (IRCODE == 0) THEN
          DTSATC=CLOCK/1D6
          RETURN
        ELSE IF (RNXCLK .GE. 2) THEN
          DTSATC=0D0
          IF (IRCODE>=2) IRCODE = 2
          RETURN
        ENDIF
      ENDIF
C
C IF "INIT=1", READ ALL SATELLITE CLOCK COEFFICIENTS
C  -------------------------------------------------
      IF(INIT.EQ.1) THEN
        CALL GTFLNA(0,'SATCLK ',FILSAC,IRCSAT)
        IF (IRCSAT.EQ.0) THEN
          ISACLK=1
        ELSE
          ISACLK=0
        ENDIF
C
        NPRSAT=0
C
C SATELLITE CLOCK FILE AVAILABLE
        IF (ISACLK.EQ.1) THEN
C
C COMPUTE MAXEPH
C --------------
C OPEN FILE
          CALL OPNFIL(LFNLOC,FILSAC,'OLD',' ', 'READONLY',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILSAC,'GTSCLK')
C
C READ TITLE LINES
          READ(LFNLOC,1)
1         FORMAT(////)
C
C INITIALIZE NUMBER OF SATELLITE CLOCK SETS PER SATELLITE AND NUMBER
C OF SATELLITES
          NSAT = 0
          NEPH = 0
C
C LOOP OVER ALL CLOCK SETS
          DO
            READ(LFNLOC,'(I3)',iostat=ios) NRSAT
C
C END OF FILE
            IF(IOS.NE.0.OR.NRSAT.EQ.0) EXIT
C
C FIND THE SATELLITE IN THE LIST
            ISAT = LISTI4(1,MAXSAT,NUMSAT,NRSAT,NSAT)
C
C TOO MANY SATELLITES
            IF (ISAT.EQ.0) THEN
              WRITE(LFNERR,902) NSAT,MAXSAT
902           FORMAT(/,' *** SR GTSCLK: TOO MANY SATELLITES IN ',/,
     1                             16X,'SATELLITE CLOCK FILE',/,
     2                             16X,'NUMBER OF SATELLITES >=',I4,/,
     3                             16X,'MAX. NUMBER OF SAT.   :',I4,/)
              CALL EXITRC(2)
            ENDIF
C
            NEPH(ISAT)=NEPH(ISAT)+1
          ENDDO
C
C COMPUTE MAXEPH
          MAXEPH = 1
          DO ISAT =1,NSAT
            IF (MAXEPH.LT.NEPH(ISAT)) MAXEPH=NEPH(ISAT)
          ENDDO
C
C ALLOCATE BUFFERS
          ALLOCATE(TSACLK(MAXEPH,MAXSAT), STAT=IRC)
          CALL ALCERR(irc,'TSACLK',(/MAXEPH,MAXSAT/),SRNAME)

          ALLOCATE(SATCLK(MAXSAC,MAXEPH,MAXSAT), STAT=IRC)
          CALL ALCERR(irc,'SATCLK',(/MAXSAC,MAXEPH,MAXSAT/),SRNAME)

          ALLOCATE(NSACLK(MAXEPH,MAXSAT), STAT=IRC)
          CALL ALCERR(IRC,'NSACLK',(/MAXEPH,MAXSAT/),SRNAME)
C
C READ TITLE LINES
          REWIND(LFNLOC)
          READ(LFNLOC,1)
C
C INITIALIZE NUMBER OF SATELLITE CLOCK SETS PER SATELLITE AND NUMBER
C OF SATELLITES
          DO 10 ISAT=1,MAXSAT
            NEPH(ISAT)=0
10        CONTINUE
C
C LOOP OVER ALL CLOCK SETS
          SAMPL=1.D20
          DO
            READ(LFNLOC,2,END=110,ERR=910) NRSAT,IWEEK,TOC,NCLK,
     1                                    (SCLOCK(I),I=1,NCLK)
2           FORMAT(I3,I5,F9.0,I3,1X,10D17.9)
C
C END OF FILE
            IF(NRSAT.EQ.0) GOTO 110
C
C CHECK NUMBER OF CLOCK COEFF. ALLOWED
            IF(NCLK.GT.MAXSAC) THEN
              WRITE(LFNERR,901) NRSAT,NCLK,MAXSAC
901           FORMAT(/,' *** SR GTSCLK: TOO MANY SAT. CLOCK COEFF.',/,
     1                             16X,'SATELLITE            :',I3,/,
     2                             16X,'NUMBER OF COEFF.     :',I3,/,
     3                             16X,'MAX. NUMBER OF COEFF.:',I3,/)
              CALL EXITRC(2)
            ENDIF
            IF(NCLK.LT.1) THEN
              WRITE(LFNERR,920) NRSAT,NCLK,1
920           FORMAT(/,' *** SR GTSCLK: NOT ENOUGH SAT. CLOCK COEFF.',/,
     1                             16X,'SATELLITE            :',I3,/,
     2                             16X,'NUMBER OF COEFF.     :',I3,/,
     3                             16X,'MIN. NUMBER OF COEFF.:',I3,/)
              CALL EXITRC(2)
            ENDIF
C
C SATELLITE INDEX
            ISAT = LISTI4(0,MAXSAT,NUMSAT,NRSAT,NSAT)
C
C NEW CLOCK SET FOR SATELLITE "ISAT"
            NEPH(ISAT)=NEPH(ISAT)+1
            IEPH=NEPH(ISAT)
C
            TSACLK(IEPH,ISAT)=GPSMJD(TOC,IWEEK)
            NSACLK(IEPH,ISAT)=NCLK
            DO 40 ICLK=1,NCLK
              SATCLK(ICLK,IEPH,ISAT)=SCLOCK(ICLK)
40          CONTINUE
C
C SAMPLING
            IF (IEPH .GT. 1) THEN
              SAMDIF=DABS(TOC-OLDTOC)
              IF (SAMDIF.LT.SAMPL .AND. SAMDIF.NE.0.D0) SAMPL=SAMDIF
            END IF
            OLDTOC=TOC
C
          ENDDO
C
C CLOSE CLOCK FILE
110       CLOSE(UNIT=LFNLOC)
        ENDIF
        INIT=0
      ENDIF
C
C INITIALIZE
C ----------
      IRCODE=0
      DTSATC=0.D0
C
C COMPUTE SATELLITE CLOCK CORRECTION
C ----------------------------------
      IF(ISACLK.EQ.1) THEN
C
C FIND SATELLITE INDEX
        ISAT = LISTI4(0,MAXSAT,NUMSAT,SATNUM,NSAT)
C
C SATELLITE NOT FOUND
C -------------------
        IF (ISAT.EQ.0) THEN
          IRCODE=2
          IF (ISTOP.EQ.1) THEN
            WRITE(LFNERR,904) SATNUM,FILSAC
904         FORMAT(/,' *** SR GTSCLK: NO SATELLITE CLOCK SET FOUND',/,
     1                           16X,'SATELLITE:',I4,/,
     2                           16X,'FILE     :',A32,/)
            CALL EXITRC(2)
          ELSE IF (ISTOP.EQ.2) THEN
            ISAT1 = LISTI4(0,MAXSAT,PRTSAT,SATNUM,NPRSAT)
            IF (ISAT1.EQ.0) THEN
              WRITE(LFNERR,905) SATNUM,FILSAC
905           FORMAT(/,' ### SR GTSCLK: NO SATELLITE CLOCK SET FOUND',/,
     1                             16X,'SATELLITE:',I4,/,
     2                             16X,'FILE     :',A32,/)
              NPRSAT=NPRSAT+1
              PRTSAT(NPRSAT)=SATNUM
            ENDIF
          ENDIF
          GOTO 999
        ENDIF
C
C SATELLITE FOUND: FIND NEAREST CLOCK SET
C ---------------------------------------
        T00=EPOCH
        TMIN=1.D20
        DO 150 IEPH=1,NEPH(ISAT)
          IF ((NSACLK(IEPH,ISAT).EQ.1.AND.SECIPL==0.D0)
     1                .OR.IEPH.EQ.1) THEN
            DTTEST=DABS(T00-TSACLK(IEPH,ISAT))
          ELSE
            DTTEST=T00-TSACLK(IEPH,ISAT)
          ENDIF
          IF(DTTEST.GT.TMIN .OR. DTTEST.LT.0.0D0) GOTO 150
          IEPMIN=IEPH
          TMIN=DTTEST
          IF (TMIN==0.D0)EXIT
150     CONTINUE
C
C SATELLITE CLOCK CORRECTION FOR EPOCH (NCLK=1)
C ---------------------------------------------
        NCLK=NSACLK(IEPMIN,ISAT)
C
        IF (NCLK.EQ.1) THEN
          IF (NEPH(ISAT).EQ.1) THEN
            WRITE(LFNERR,157) SATNUM,EPOCH
157            FORMAT(/,' ### SR GTSCLK: ONLY ONE EPOCH AVAILABLE FOR ',
     1                  'CLOCK INTERPOLATION/EXTRAPOLATION',/,
     2                            16X,'SATELLITE       :',I4,/,
     3                            16X,'EPOCH           :',F15.7,/)
            DTSATC=SATCLK(1,IEPMIN,ISAT)
            IRCODE=2
            IF (ISTOP.EQ.1) CALL EXITRC(2)
            GOTO 999
          END IF
C
C INTERPOLATION/EXTRAPOLATION
C ---------------------------
          DTSATC=SATCLK(1,IEPMIN,ISAT)
          IF (IEPMIN.EQ.NEPH(ISAT)) IEPMIN = IEPMIN-1
          DT00=(EPOCH-TSACLK(IEPMIN,ISAT))*86400.D0
          DTTT=DNINT((TSACLK(IEPMIN+1,ISAT)-TSACLK(IEPMIN,ISAT))
     1                *86400.D0)*1.D0
          IF (dTTT==0.D0)dttt=1.D0
          IF (IDNINT(dttt*1d3)<=IDNINT(SECIPL*1d3).AND.
     1        IDNINT(DABS(dt00)*1D3)<= IDNINT(SECIPL*1D3)) THEN
            DTSATC=(SATCLK(1,IEPMIN+1,ISAT)-SATCLK(1,IEPMIN,ISAT))*
     1                  DT00/DTTT+SATCLK(1,IEPMIN,ISAT)
          ELSEIF (SECIPL/=0.D0) THEN
            IRCODE=1
            DTSATC=0.D0
          ENDIF
C
C SATELLITE CLOCK CORRECTION FOR EPOCH (NCLK.GT.1)
C ------------------------------------------------
        ELSE
          DT00=(EPOCH-TSACLK(IEPMIN,ISAT))
          DTSATC=SATCLK(NCLK,IEPMIN,ISAT)
          DO ICLK=NCLK-1,1,-1
            DTSATC=DTSATC*DT00*86400.D0+SATCLK(ICLK,IEPMIN,ISAT)
          END DO
        END IF
C
C CHECK DISTANCE FROM NEXT SET
C ----------------------------
        IF (NCLK.EQ.1) THEN
          DISMAX= 1.01D0*SAMPL/86400D0
          DT00  = TSACLK(IEPMIN+1,ISAT)-TSACLK(IEPMIN,ISAT)
          DTTEST= EPOCH-TSACLK(IEPMIN,ISAT)
          dttest2=tsaclk(iepmin+1,isat)-epoch
          IF (IEPMIN.EQ.1) THEN
            DT01 = DABS(EPOCH - TSACLK(IEPMIN,ISAT))
            DT00 = DMAX1(DT00,DT01)
          ELSEIF (IEPMIN+1.EQ.NEPH(ISAT)) THEN
            DT01 = DABS(EPOCH - TSACLK(IEPMIN+1,ISAT))
            DT00 = DMAX1(DT00,DT01)
          ENDIF
        ELSE
!!          DISMAX= 1.01D0*SAMPL/86400D0
          DISMAX= 6.D0
        ENDIF
C
        IF (SECIPL==0.D0) THEN
          IF(DABS(DTTEST).GT.6.D-6.AND.DABS(DTTEST2).GT.6.D-6) THEN
            IF (DABS(DT00).GT.DISMAX.OR.
     1         (DABS(DTTEST).GT.DISMAX/2.D0.AND.
     2            DABS(DTTEST2).GT.DISMAX/2.D0) ) THEN
              IRCODE=1
              DTSATC=0.D0
            ENDIF
          ENDIF
        ENDIF
        IF (IRCODE==1) THEN
          IF (ISTOP.EQ.0) THEN
            GOTO 999
          ELSEIF (ISTOP.EQ.1) THEN
            IF (SECIPL/=0.D0) THEN
              WRITE(LFNERR,930) ' ***',SATNUM,EPOCH,DT00-SAMPL/2,SECIPL
930           FORMAT(/,A4,' SR GTSCLK: NEAREST SATELLITE CLOCK SET',
     1                 ' TOO FAR AWAY FOR INTERPOLATION',/,
     2                            16X,'SATELLITE        :',I4,/,
     3                            16X,'EPOCH REQUEST    :',F15.7,/,
     4                            16X,'DIFFERENCE IN SEC:',F15.7,/,
     5                            16X,'MAX. DIFFERENCE  :',F15.7,/)
            ELSE
              CALL TIMST2(1,2,(/TMIN,EPOCH/),EPOSTR)
              WRITE(EPOSTR(1:13),'(I13)') INT((TMIN+0.5/86400d0)*24d0)
              WRITE(LFNERR,940) ' ***',EPOSTR(8:19),EPOSTR(22:40),
     1                                 SATNUM,FILSAC
940           FORMAT(/,A4,' SR GTSCLK: NEAREST SATELLITE CLOCK SET TOO',
     1                 ' FAR AWAY',/,
     2                           16X,'NEAREST SET (HOURS):',A,/,
     3                           16X,'EPOCH REQUEST      :',2X,A,/,
     4                           16X,'SATELLITE          :',I6,/,
     5                           16X,'FILE               :',A32,/)
            END IF
            CALL EXITRC(2)
          ELSE
            ISAT1 = LISTI4(0,MAXSAT,PRTSAT,SATNUM,NPRSAT)
            IF (ISAT1.EQ.0) THEN
              IF (NCLK.EQ.1) THEN
                WRITE(LFNERR,930)' ###',SATNUM,EPOCH,DT00-SAMPL/2,SECIPL
              ELSE
                CALL TIMST2(1,2,(/TMIN,EPOCH/),EPOSTR)
                WRITE(EPOSTR(1:13),'(I13)') INT((TMIN+0.5/86400d0)*24d0)
                WRITE(LFNERR,950) EPOSTR(8:19),EPOSTR(22:40),SATNUM,
     1                            FILSAC
950         FORMAT(/,' ### SR GTSCLK: SATELLITE CLOCK EXTRAPOLATED',/,
     1                           16X,'NEAREST SET (HOURS):',A,/,
     2                           16X,'EPOCH REQUEST      :',2X,A,/,
     3                           16X,'SATELLITE          :',I6,/,
     4                           16X,'FILE               :',A32,/)
              ENDIF
              NPRSAT=NPRSAT+1
              PRTSAT(NPRSAT)=SATNUM
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      GOTO 999
C
910   WRITE(LFNERR,911) TRIM(FILSAC)
911   FORMAT(/,' *** SR GTSCLK: READING ERROR IN CLOCK FILE',
     1       /,'                FILE: ',A,/)
      CALL EXITRC(2)
C
999   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
