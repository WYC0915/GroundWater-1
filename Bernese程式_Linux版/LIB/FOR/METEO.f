      MODULE s_METEO
      CONTAINS

C*
      SUBROUTINE METEO(INIT  ,ITROPO,IEXTRA,NSTAT ,STAT  ,SVN   ,
     1                 TMET  ,XSTELL,ZEN   ,AZI   ,WL    ,DR    ,
     2                 PSAV  ,TSAV  ,HSAV )
CC
CC NAME       :  METEO
CC
CC PURPOSE    :  COMPUTE TROPOSPHERIC REFRACTION CORRECTIONS FOR
CC               NSTAT STATIONS.
CC
CC PARAMETERS :
CC         IN :  INIT    : SESSION INITIALIZATION INDICATOR   I*4
CC                         =0: NORMAL REQUEST FOR TROPOSPHERIC
CC                             DELAY
CC                         =1: INITIALIZATION REQUEST (OPEN
CC                             OF METEO FILES OF SESSION)
CC                         =2: CLOSE REQUEST (CLOSE METEO
CC                             FILES OF SESSION)
CC               ITROPO  : MODEL INDEX                        I*4
CC                     = 0: NO CORRECTION
CC                     = 1: SAASTAMOINEN
CC                     = 2: HOPFIELD
CC                     = 3: ESSEN AND FROOME
CC                     = 4: MARINI MURRAY (SLR)
CC                     = 5: SAASTAMOINEN WITH NIELL DRY MAPPING
CC                     =11: SAASTAMOINEN DRY DELAY ONLY
CC                     =12: HOPFIELD DRY DELAY ONLY
CC                     =15: SAASTAMOINEN DRY WITH NIELL DRY MAPPING
CC               IEXTRA  : METEO DATA SOURCE INDICATOR        I*4
CC                     =0: USE REAL DATA FROM EXTERNAL FILES
CC                     =1: USE VALUES FROM ATMOSPHERIC MODEL
CC                     =2: USE ESTIMATED VALUES IN BERNESE
CC                         TRP-FORMAT (FROM GPSEST OR ADDNEQ)
CC                     =3: Same as 2, but calling GETTRP2
CC                         (special version for piecewise linear
CC                          interpolation of troposphere, for CLKEST)
CC               NSTAT   : LIST OF STATIONS TO BE PROCESSED
CC               STAT(I),I=1,2,..,NSTAT: STATION NAMES        CH*16
CC               SVN     : SATELLITE NUMBER                   I*4
CC               TMET    : TIME OF REQUEST IN MJD             R*8
CC               XSTELL(J,I),J=1..3,I=1,..,NSTAT: ELLIPSOIDAL R*8
CC                         STATION COORDINATES (LAT/LON IN
CC                         RAD, HEIGHT IN M)
CC               ZEN(I),I=1,2,..,NSTAT: ZENITH DISTANCES      R*8
CC               AZI(I),I=1,2,..,NSTAT: AZIMUTHS              R*8
CC               WL      : WAVELENGHT FOR MARINI-MURRAY(RANGE)R*8
CC        OUT :  DR(I),I=1,2,..,NSTAT: TROPOSPHERIC ZENITH    R*8
CC                         CORRECTION
CC               PSAV(I),I=1,2,..,NSTAT: PRESSURE VALUES      R*8
CC               TSAV(I),I=1,2,..,NSTAT: TEMPERATURE VALUES   R*8
CC               HSAV(I),I=1,2,..,NSTAT: HUMIDITY VALUES      R*8
CC
CC REMARKS    :  TOTAL NUMBER OF METEO FILES < 100
CC               TOTAL NUMBER OF FILES PER SESSION < 30
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 94)
CC
CC CREATED    :  87/09/28 09:54
CC
CC CHANGES    :  28-DEC-92 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC                  SEP-93 : ??: ADD APRIORI MODEL TO DR INTROD. FROM FILE
CC               21-APR-94 : ??: ERROR FOR ITYP=1 (WAS CAUSED BY CHANGE OF
CC                               SEP-93)
CC               29-JUL-94 : MR: ABRITRARY NUMBER OF TROPOSPHERE VALUES
CC                               IN MET.FILES, ADD SVN AND AZI TO PARA-
CC                               METER LIST
CC               10-AUG-94 : MR: CALL EXITRC
CC               31-AUG-94 : MR: READ METEO FILES WITH CORRECT NAMES
CC               20-SEP-94 : MR: ALLOW STATIONS WITHOUT METEO FILE
CC               23-SEP-94 : MR: OPEN LFN'S IN SEQUENCE, NOT BY METEO
CC                               FILE INDEX. INIT=2: CLOSE FILES
CC               05-JAN-95 : MR: TEST HERRING MAPPING (1ST TERM ONLY)
CC               31-AUG-95 : SS/MR: CHECK COLLISION OF LF-NUMBERS
CC               27-SEP-95 : TVH:OPTION TO MODEL DRY DELAY ONLY (PWV EST.)
CC               27-SEP-95 : TVH:CHANGE OPTION TO MODEL DRY DELAY
CC               27-SEP-95 : TVH:ALLOW FOR ITYP 3 OR 4 IN MET FILE
CC               29-SEP-95 : MR: REDEFINE METEO FILE TYPES; MOVE ITYP=3
CC                               AND ITYP=4 OF TVH TO ITYP=5 AND ITYP=6;
CC                               READ MODEL NUMBER FROM METEO FILE;
CC                               ITYP=5 REALIZED NOW WITH ITYP=3 AND
CC                               IMODEL=1.
CC               21-NOV-95 : TS: CORRECT READING OF METEO FILE HEADER
CC               26-MAR-96 : TS: SOME COSMETIC CHANGES
CC               21-JUN-96 : TS: ALLOW INPUT OF ESTIMATED VALUES
CC               29-JUL-96 : MR: CORRECT ERROR MESSAGE 903
CC               14-AUG-96 : EB: NEW CALL GETTRP
CC               31-DEC-96 : WG: TEST FOR 2000 WAS MISSING
CC               08-APR-97 : SS: NIELL MAPPING, TROPOSPHERE GRADIENTS
CC               14-APR-97 : SS: ERROR MESSAGES CORRECTED
CC               01-JUL-99 : PF: CALL IYEAR4 FOR CONVERSION YY->YYYY
CC               28-AUG-00 : MR: SAASTAMOINEN WITH NIELL MAPPING
CC               31-AUG-00 : MR: USE "ITROPO" AS A PRIORI MODEL IF NO
CC                               ESTIMATES AVAILABLE IN TROP.EST.FILE
CC               10-SEP-01 : HU: IEXTRA=3: CALL GETTRP WITH ITRPMD=99
CC               07-JUL-03 : MM: NEW GETTRP CALL
CC               08-JUL-03 : MM: CHKTRP MECHANISM INCLUDED
CC               23-DEC-03 : HU: DRY_NIELL ALSO FOR MET FILES
CC               29-MAR-04 : CU: NEW INPUT PARAMETER: WL (WAVELENGTH FOR RANGE),
CC                               ADD WL TO CALL OF SR TROPOS
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               13-DEC-05 : CU: ADAPT CALL OF SR TROPOS
CC               24-AUG-06 : AG: TDELAY USED, GMF IMPLEMENTED
CC               13-MAR-07 : AG: INITIALIZATION OF METEX (=0)
CC               01-SEP-08 : DT: MAXMET INCREASED 100->250
CC               05-FEB-09 : DT: CREATE FICTITIOUS SECOND RECORD IF ONLY ONE
CC                               AVAILABLE
CC               18-JUL-11 : PS: APPLY GRADIENTS WITH CHEN-HERRING MAPPING,
CC                               ERROR FOR UNKNOWN GRADIENT MODEL
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const,  ONLY: PI
      USE s_tdelay
      USE s_opnfil
      USE s_gtfile
      USE f_djul
      USE s_gettrp
      USE s_opnerr
      USE s_chktrp
      USE s_posmet
      USE s_exitrc
      USE f_iyear4
      USE s_trpmap
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IDD   , IEXTRA, IFIRST, IHH   , IMET  , INIT  , IOSTAT,
     1          IRCODE, ISES  , ISS   , ISTAT , ITRGRD, ITRMAP,
     2          ITROPM, ITROPO, ITRPMD, IVAL  , JJ    ,
     3          K     , L     , LFN   , LFNTOP, LFNTST, MAXMET, MAXMVA,
     4          METSES, MM    , MO    , NFLCOL, NMET  , NSTAT , METEX
C
      REAL*8    CZ    , DD    , DRHELP, DRWET , FAK   , FAK1  ,
     1          FRAC  , HUM   , PRESS , PWV   , TEMP  , TEMPW , TMET
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXMET=250,MAXMVA=30)
C
C MAXMET: MAXIMUM NUMBER OF METEO FILES
C MAXMVA: MAXIMUM FILES PER SESSION
C
      CHARACTER*132 LINE
      CHARACTER*32  FILMET(MAXMET),FILTRP
      CHARACTER*16  STAT(*),STAMET(MAXMET)
C
      REAL*8       XSTELL(3,*),ZEN(*),AZI(*),DR(*)
      REAL*8       METVAL(2,MAXMVA,MAXMET),METINT(MAXMVA)
      REAL*8       PSAV(*),TSAV(*),HSAV(*)
      REAL*8       DRTROP(3),MAPFUN(2)
      REAL*8       WL,MAZ
C
      INTEGER*4    UTLOC(MAXMET),ITYP(MAXMET),NMETEO(MAXMET)
      INTEGER*4    IMODEL(MAXMET),INDMET(MAXMET),SVN
      INTEGER*4    ItrpOk
C
C
      COMMON/CMETEO/STAMET,FILMET,METVAL,UTLOC,ITYP,INDMET,NMETEO
C
      DATA IFIRST/1/
C
C INITIALIZATION PHASE
C **************************************************************
      IF (INIT.EQ.1 .AND. IEXTRA.EQ.0) THEN
        IF (IFIRST.EQ.1) THEN
C
C UPON FIRST CALL, GET TABLE WITH METEO FILE NAMES
          IFIRST=0
          NFLCOL=1
          CALL GTFILE('METFIL ',NFLCOL,MAXMET,NMET,FILMET)
C
C READ HEADER OF METEO FILES TO GET STATION NAMES ASSOCIATED WITH THEM
          DO 110 IMET=1,NMET
C
            CALL OPNFIL(LFN002,FILMET(IMET),'OLD','FORMATTED',
     1                  'READONLY',' ',IOSTAT)
            CALL OPNERR(LFNERR,LFN002,IOSTAT,FILMET(IMET),'METEO ')
            READ(LFN002,'(/,A)',END=234) LINE
            READ(LINE,103) STAMET(IMET),UTLOC(IMET),
     1                     ITYP(IMET),NMETEO(IMET),
     2                     IMODEL(IMET)
103         FORMAT(10X,A16,26X,I3,5X,I2,10X,I3,6X,I3)
C
C INCLUDE TIME IN "NMETEO"
            NMETEO(IMET)=NMETEO(IMET)+1
C
C INSURE CONSISTENCY OF EARLIER FORMAT WITHOUT "NMETEO"
            IF (NMETEO(IMET).EQ.1) THEN
              IF (ITYP(IMET).EQ.1 .OR. ITYP(IMET).EQ.2) THEN
                NMETEO(IMET)=4
                IMODEL(IMET)=0
              ELSEIF (ITYP(IMET).EQ.3) THEN
                NMETEO(IMET)=2
                IMODEL(IMET)=0
              ELSEIF (ITYP(IMET).EQ.4) THEN
                NMETEO(IMET)=2
                IMODEL(IMET)=-1
              ENDIF
            ENDIF
C
C CHECK MAXIMUM NUMBER OF TROP. VALUES ALLOWED (MAXMVA)
            IF (NMETEO(IMET).GT.MAXMVA) THEN
              WRITE(LFNERR,122) NMETEO(IMET),MAXMVA,FILMET(IMET)
122           FORMAT(/,' *** SR METEO : TOO MANY VALUES PER RECORD',
     1                 ' IN METEO FILE',/,
     2                             15X,'NUMBER OF VALUES      : ',I2,/,
     3                             15X,'MAXIMUM NUMBER ALLOWED: ',I2,/,
     4                             15X,'METEO FILE NAME       : ',A,/)
              CALL EXITRC(2)
            END IF
C
C CHECK MET FILE FOR THE SAME STATION IS ALREADY AVAILABLE
CCC            DO 130 K=1, STAMET(IMET-1)
CCC               write(*,*) 'only first met-file per station is used!'
CCC130         CONTINUE
C
            CLOSE(UNIT=LFN002)
110       CONTINUE
        ENDIF
C
C OPEN NEW FILES
        METSES=0
        DO 240 ISTAT=1,NSTAT
          DO 220 IMET=1,NMET
            IF (STAT(ISTAT).EQ.STAMET(IMET)) GOTO 230
220       CONTINUE
C
C NO METEO FILE FOUND: USE A PRIORI MODEL: ITYP(IMET)=0
          WRITE(LFNERR,221) STAT(ISTAT)
221       FORMAT(/,' ### SR METEO: NO METEO FILE FOUND',/,
     1                        15X,'STATION: ',A16,/)
          NMET=NMET+1
          ITYP(NMET)=0
          STAMET(NMET)=STAT(ISTAT)
          IMET=NMET
C
230       CONTINUE
C
          METSES=METSES+1
          INDMET(METSES)=IMET
C
C OPEN ONLY, IF METEO FILE AVAILABLE
          IF (ITYP(IMET).GT.0) THEN
            LFN=LFN002-1+METSES
C
C CHECK COLLISION OF LOGICAL FILE NUMBERS
C ---------------------------------------
            LFNTST=LFN
            LFNTOP=LFNEPH
            IF (LFNTST.GE.LFNTOP) THEN
              WRITE(LFNERR,902) METSES,LFN002,LFNTOP-1
902           FORMAT(/,' *** SR METEO:  TOO MANY METEO FILES ',
     1                 'IN SESSION TO BE OPEN SIMULTANEOUSLY',/,
     2             16X,'COLLISION OF LOGICAL FILE NUMBERS ',
     3                 '(SEE INCLUDE FILE "LFNUM")',/,
     4             16X,'# METEO FILES IN SESSION >= ',I4,/,
     5             16X,'STARTING LF-NUMBER        : ',I4,/,
     6             16X,'MAXIMUM LF-NUMBER ALLOWED : ',I4,/)
              CALL EXITRC(2)
            END IF
            CALL OPNFIL(LFN,FILMET(IMET),'OLD','FORMATTED',
     1                  'READONLY',' ',IOSTAT)
            CALL OPNERR(LFNERR,LFN,IOSTAT,FILMET(IMET),'METEO ')
C
C SKIP 3 TITLE LINES
            READ (LFN,'(//)',END=234)
C
C READ FIRST TWO METEO RECORDS
C ----------------------------
            DO 239 L=1,2

CCC              READ(LFN,*,END=234) JJ,MO,IDD,IHH,MM,ISS,
              READ(LFN,*,END=2340) JJ,MO,IDD,IHH,MM,ISS,
     1                   (METVAL(L,K,METSES),K=2,NMETEO(IMET))
              IF (JJ.GT.0) GO TO 235
C
235           JJ = IYEAR4(JJ)
              DD=IDD+IHH/24.D0+MM/1440.D0+ISS/86400.D0
              METVAL(L,1,METSES)=DJUL(JJ,MO,DD)+UTLOC(IMET)/24.D0
CCCC
CCCC Added 05-Feb-2009 DT: if only 1 epoch
2340          IF (L.EQ.2) THEN
                DO K=1,NMETEO(IMET)
                  METVAL(L,K,METSES)=METVAL(1,K,METSES)
                ENDDO

              ELSE
                DO K=1,NMETEO(IMET)
                  METVAL(2,K,METSES)=METVAL(1,K,METSES)
                ENDDO
CCC  GO TO 234
              ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
239         CONTINUE
          ENDIF
240     CONTINUE
C
C CLOSE ALL METEO FILES OPEN
C --------------------------
      ELSEIF (INIT.EQ.2 .AND. IEXTRA.EQ.0) THEN
        DO 210 ISES=1,METSES
          IF (ITYP(INDMET(ISES)).GT.0) THEN
            LFN=LFN002-1+ISES
            CLOSE(UNIT=LFN)
          ENDIF
210     CONTINUE
      ELSEIF (INIT.EQ.0) THEN
C **************************************************************
C
C HANDLING OF A NORMAL REQUEST
C ----------------------------
C
C IEXTRA=0: USE REAL METEO DATA FROM FILES
C ----------------------------------------
        IF (IEXTRA.EQ.0) THEN
          DO 350 ISTAT=1,NSTAT
            DO 310 ISES=1,METSES
              IMET=INDMET(ISES)
              IF(STAT(ISTAT).EQ.STAMET(IMET))GO TO 320
310         CONTINUE
C
C POSITION METEO FILE
320         IF (ITYP(IMET).GT.0) THEN
              LFN=LFN002-1+ISES
              CALL POSMET(LFN,TMET,UTLOC(IMET),NMETEO(IMET),
     1                    METVAL(1,1,ISES))
C
              IF (TMET.LE.METVAL(1,1,ISES)) THEN
                FRAC = 0.D0
              ELSE IF (TMET.GE.METVAL(2,1,ISES)) THEN
                FRAC = 1.D0
              ELSE IF (METVAL(2,1,ISES).EQ.METVAL(1,1,ISES)) THEN
                FRAC = 1.D0
              ELSE
                FRAC = (TMET-METVAL(1,1,ISES)) /
     1                 (METVAL(2,1,ISES)-METVAL(1,1,ISES))
              END IF
C
C INTERPOLATE ALL TROPOSPHERE VALUES
              METINT(1)=TMET
              DO 330 IVAL=2,NMETEO(IMET)
                METINT(IVAL)=METVAL(1,IVAL,ISES) +
     1               (METVAL(2,IVAL,ISES)-METVAL(1,IVAL,ISES)) * FRAC
330           CONTINUE
            ENDIF
            metex = 0
C
C EXTRAPOLATED METEO FOR STATIONS WITHOUT METEO FILE
            IF (ITYP(IMET).EQ.0) THEN
              metex=1
              CALL TDELAY(TMET,ZEN(ISTAT),XSTELL(:,ISTAT),ITROPO,metex,
     1                    WL,TEMP,PRESS,HUM,DR(ISTAT))
              PSAV(ISTAT)=PRESS
              TSAV(ISTAT)=TEMP
              HSAV(ISTAT)=HUM
C
C PRESSURE, TEMP, AND HUMIDITY IN METEO FILE
            ELSEIF (ITYP(IMET).EQ.1) THEN
              PRESS=METINT(2)
              TEMP =METINT(3)
              HUM  =METINT(4)
              CALL TDELAY(TMET,ZEN(ISTAT),XSTELL(:,ISTAT),ITROPO,metex,
     1                    WL,TEMP,PRESS,HUM,DR(ISTAT))
              PSAV(ISTAT)=PRESS
              TSAV(ISTAT)=TEMP
              HSAV(ISTAT)=HUM
C
C PRESSURE, DRY AND WET TEMPERATURE IN METEO FILE
            ELSEIF(ITYP(IMET).EQ.2) THEN
              PRESS=METINT(2)
              TEMP =METINT(3)
              TEMPW=METINT(4)
              FAK =10.D0**(7.5D0*TEMP/(TEMP+237.3D0)+.7858D0)
              FAK1=10.D0**(7.5D0*TEMPW/(TEMPW+237.3D0)+.7858D0)
              HUM=-100.D0/FAK*((TEMP-TEMPW)*.662D-3*PRESS-FAK1)
              CALL TDELAY(TMET,ZEN(ISTAT),XSTELL(:,ISTAT),ITROPO,metex,
     1                    WL,TEMP,PRESS,HUM,DR(ISTAT))
              PSAV(ISTAT)=PRESS
              TSAV(ISTAT)=TEMP
              HSAV(ISTAT)=HUM
C
C TOTAL ZENITH DELAY IN METEO FILE
            ELSEIF (ITYP(IMET).EQ.3) THEN
              CZ=DCOS(ZEN(ISTAT))
C IMODEL=0: USE 1/COS MAPPING (DEFAULT)
              IF (IMODEL(IMET).EQ.0) THEN
                DR(ISTAT) = METINT(2)/CZ
C IMODEL>0: USE MAPPING FUNCTION ACCORDING TO IMODEL
C (SEE SR TRPMAP FOR THE DEFINITON OF THE MAPPING FUNCTION CODES)
              ELSEIF (IMODEL(IMET).GT.0) THEN
                CALL TRPMAP(IMODEL(IMET),TMET,XSTELL(1,ISTAT),
     1                      ZEN(ISTAT),MAPFUN)
                DR(ISTAT) = METINT(2)*MAPFUN(1)
              ELSE
                GOTO 990
              ENDIF
              PSAV(ISTAT)=0
C
C TROPOSPHERIC ZENITH DELAYS FROM GPSEST ESTIMATES IN METEO FILE
            ELSEIF (ITYP(IMET).EQ.4) THEN
              IF (IMODEL(IMET).LT.0) THEN
                ITROPM=-IMODEL(IMET)
                metex=1
                CALL TDELAY(TMET,ZEN(ISTAT),XSTELL(:,ISTAT),ITROPM,
     1                      metex,WL,TEMP,PRESS,HUM,DRHELP)
              ELSEIF (IMODEL(IMET).EQ.0) THEN
                DRHELP=0.D0
              ELSEIF (IMODEL(IMET).GT.0) THEN
                WRITE(LFNERR,903) IMODEL(IMET),ITYP(IMET),FILMET(IMET)
903             FORMAT(/,' *** SR METEO: THE METEO FILE YOU USE CONT',
     1              'AINS TROPOSPHERE',
     2            /,15X,'ESTIMATES FROM GPSEST/ADDNEQ.',
     3            /,15X,'WHEN ESTIMATING THOSE TROPOSPHERE PARA',
     4              'METERS,'
     5            /,15X,'METEO FILES WERE INTRODUCED ALREADY.',
     6            /,15X,'SUCH AN ITERATION PROCEDURE IS NOT ALLOWED.',
     7            /,15X,'TROPOSPHERE ESTIMATES MAY ONLY BE INTRO',
     8              'DUCED',
     9            /,15X,'IF THE OPTION "EXTRAPOLATED" WAS USED',
     1              ' FOR',
     2            /,15X,'THEIR GENERATION',
     7            /,15X,'METEO MODEL NUMBER:',I4,
     8            /,15X,'METEO FILE TYPE   :',I4,
     9            /,15X,'METEO FILE        : ',A32,/)
                CALL EXITRC(2)
              ENDIF
              DR(ISTAT) = DRHELP + METINT(2)/DCOS(ZEN(ISTAT))
              PSAV(ISTAT)=0.D0
C
C PRESSURE, TEMP, HUMIDITY AND ZENITH WET DELAY IN METEO FILE
            ELSEIF (ITYP(IMET).EQ.5) THEN
              PRESS=METINT(2)
              TEMP =METINT(3)
              HUM  =METINT(4)
              DRWET=METINT(5)
              ITROPM=ITROPO
C SWITCH TO DRY PART ONLY FOR A PRIORI MODEL IF WET DELAY WILL BE ADDED
              IF (IMODEL(IMET).EQ.1.AND.ITROPO.LE.10) ITROPM=ITROPO+10
              CALL TDELAY(TMET,ZEN(ISTAT),XSTELL(:,ISTAT),ITROPM,metex,
     1                    WL,TEMP,PRESS,HUM,DRHELP)
              PSAV(ISTAT)=PRESS
              TSAV(ISTAT)=TEMP
              HSAV(ISTAT)=HUM
C MODEL=0: USE PRESS, TEMP, AND HUMIDITY ONLY
              IF (IMODEL(IMET).EQ.0) THEN
                DR(ISTAT)=DRHELP
C MODEL=1: COMPUTE DRY DELAY FROM PRESS, TEMP, AND TAKE WET FROM FILE
              ELSEIF (IMODEL(IMET).EQ.1) THEN
                DR(ISTAT)=DRHELP+DRWET/DCOS(ZEN(ISTAT))
              ELSE
                GOTO 990
              ENDIF
C
C PRESSURE, TEMP, HUMIDITY AND PRECIPITABLE WATER VAPOUR IN METEO FILE
            ELSEIF (ITYP(IMET).EQ.6) THEN
              PRESS=METINT(2)
              TEMP =METINT(3)
              HUM  =METINT(4)
              PWV  =METINT(5)
              ITROPM=ITROPO
              CALL TDELAY(TMET,ZEN(ISTAT),XSTELL(:,ISTAT),ITROPM,metex,
     1                    WL,TEMP,PRESS,HUM,DR(ISTAT))
              PSAV(ISTAT)=PRESS
              TSAV(ISTAT)=TEMP
              HSAV(ISTAT)=HUM
C IMODEL=0: USE PRESS, TEMP, AND HUMIDITY ONLY
              IF (IMODEL(IMET).NE.0) GOTO 990
C
C INVALID METEO FILE TYPE
            ELSE
              WRITE(LFNERR,901) ITYP(IMET),FILMET(IMET)
901           FORMAT(/,' *** SR METEO: ILLEGAL METEO FILE TYPE',/,
     1                             15X,'METEO FILE TYPE: ',I3,/,
     2                             15X,'METEO FILE NAME: ',A,/)
              CALL EXITRC(2)
            ENDIF
350       CONTINUE
C
C IEXTRA=1: USE EXTRAPOLATED METEO
C --------------------------------
        ELSEIF (IEXTRA.EQ.1) THEN
C
C ITROPO=0: NO TROPOSPHERE CORRECTION
          IF (ITROPO.EQ.0) THEN
            DO 15 ISTAT=1,NSTAT
              DR(ISTAT)=0.D0
15          CONTINUE
C
C ITROPO>0: METEO MODEL ACCORDING TO SR TROPOS
          ELSE
            DO 20 ISTAT=1,NSTAT
              metex=1
              CALL TDELAY(TMET,ZEN(ISTAT),XSTELL(:,ISTAT),ITROPO,metex,
     1                    WL,TEMP,PRESS,HUM,DR(ISTAT))
              PSAV(ISTAT)=PRESS
              TSAV(ISTAT)=TEMP
              HSAV(ISTAT)=HUM
20          CONTINUE
          ENDIF
C
C IEXTRA=2: USE ESTIMATES FROM BERNESE TRP-FILE
C ---------------------------------------------
        ELSEIF (IEXTRA.EQ.2.OR.IEXTRA.EQ.3) THEN
          DO 400 ISTAT=1,NSTAT
C
C TROPOSPHEREIC ZENITH DELAYS FROM GPSEST ESTIMATES IN BERNESE FILE
C -----------------------------------------------------------------
            FILTRP=' '
            ITRPMD=0
            IF (IEXTRA.EQ.3) ITRPMD=99
C
            CALL CHKTRP(1,iTrpOk,staNam=STAT(ISTAT))
            CALL GETTRP(FILTRP,TMET,STAT(ISTAT),2,0,ITRPMD,ITRMAP,
     1                  ITRGRD,DRTROP,IRCODE)
            IF (iTrpOk==1) THEN
              DRTROP=0.d0
              IRCODE=1
            END IF
C
            IF (IRCODE.EQ.0) THEN
              ITROPM=ITRPMD
            ELSE
              ITROPM=-ITROPO
            ENDIF
            IF (ITROPM.LE.0) THEN
              ITROPM=-ITROPM
C
C ITROPM=0: NO A PRIORI METEO CORRECTION
              IF (ITROPM.EQ.0) THEN
                DRHELP=0.D0
C
C ITROPM>0: A PRIORI MODEL ACCORDING TO SR TROPOS
              ELSE
                metex=1
                CALL TDELAY(TMET,ZEN(ISTAT),XSTELL(:,ISTAT),ITROPM,
     1                      metex,WL,TEMP,PRESS,HUM,DRHELP)
                PSAV(ISTAT)=0.D0
                TSAV(ISTAT)=TEMP
                HSAV(ISTAT)=HUM
              ENDIF
              CALL TRPMAP(ITRMAP,TMET,XSTELL(1,ISTAT),ZEN(ISTAT),
     1                    MAPFUN)
              DR(ISTAT) = DRHELP + DRTROP(3)*MAPFUN(1)
C
C APPLY GRADIENTS
              IF (ITRGRD.EQ.1) THEN
                DR(ISTAT)=DR(ISTAT)
     1                    +DRTROP(1)*MAPFUN(2)*DCOS(AZI(ISTAT))
     2                    +DRTROP(2)*MAPFUN(2)*DSIN(AZI(ISTAT))
              ELSEIF (ITRGRD.EQ.2) THEN
                DR(ISTAT)=DR(ISTAT)
     1                    +DRTROP(1)*DTAN(ZEN(ISTAT))*DCOS(AZI(ISTAT))
     2                    +DRTROP(2)*DTAN(ZEN(ISTAT))*DSIN(AZI(ISTAT))
              ELSEIF (ITRGRD.EQ.3) THEN
                DR(ISTAT)=DR(ISTAT)
     1                    +DRTROP(1)*MAPFUN(1)*DTAN(ZEN(ISTAT))*
     2                     DCOS(AZI(ISTAT))
     3                    +DRTROP(2)*MAPFUN(1)*DTAN(ZEN(ISTAT))*
     4                     DSIN(AZI(ISTAT))
              ELSEIF (ITRGRD.EQ.4) THEN
                MAZ=1.D0/(DCOS(ZEN(ISTAT))*DTAN(PI/2.D0-ZEN(ISTAT))
     1              +0.0032D0)
                DR(ISTAT)=DR(ISTAT)
     1                    +DRTROP(1)*MAZ*DCOS(AZI(ISTAT))
     2                    +DRTROP(2)*MAZ*DSIN(AZI(ISTAT))
              ELSEIF (ITRGRD.NE.0) THEN
                WRITE(LFNERR,906) ITRGRD
906             FORMAT(/,' *** SR METEO: UNKNOWN GRADIENT MODEL',/,
     1                               15X,'ITRGRD: ',I3,/)
                CALL EXITRC(2)

              ENDIF
C
C CHECK THAT NO REAL METEO VALUES WERE USED WHEN CREATING THE TRP-FILE
            ELSE
              WRITE(LFNERR,905)ITRPMD
905           FORMAT(/,' *** SR METEO: REAL METEO FILES WERE USED',
     1                 ' WHEN ESTIMATING ',
     2               /,15X,'THE TROPOSPHERE PARAMETERS THAT ARE ',
     3                 'NOW INTRODUCED AS METEO FILE VALUES',
     4               /,15X,'NOW EXTRAPOLATED WOULD BE USED FOR ',
     5                'THE A PRIORI MODEL PART',
     6               /,15X,'LEADING TO INCONSISTENT RESULTS',
     7               /,15X,'METEO MODEL NUMBER:',I4,/)
              CALL EXITRC(2)
            ENDIF
400       CONTINUE
        ENDIF
      ENDIF
C
999   RETURN
C
C ERROR MESSAGE
234   WRITE(LFNERR,233) FILMET(IMET)
233   FORMAT(/,' *** SR METEO: NOT ENOUGH RECORDS IN METEO FILE',
     1                  /,15X,'FILE: ',A32,/)
      CALL EXITRC(2)
C
C INVALID MODEL NUMBER IN METEO FILE FOR SPECIFIC FILE TYPE
990   WRITE(LFNERR,904) IMODEL(IMET),ITYP(IMET),FILMET(IMET)
904   FORMAT(/,' *** SR METEO: INVALID METEO MODEL NUMBER FOR THIS ',
     1         'METEO FILE TYPE',
     2       /,15X,'METEO MODEL NUMBER:',I4,
     3       /,15X,'METEO FILE TYPE   :',I4,
     4       /,15X,'METEO FILE        : ',A32,/)
      CALL EXITRC(2)
C
      END SUBROUTINE

      END MODULE
