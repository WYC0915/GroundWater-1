      MODULE s_CDRESU
      CONTAINS

C*
      SUBROUTINE CDRESU(PGMNAM,CAMPGN,STITLE,NSAMPL,ZENMAX,OBSWIN,
     1                  FILWIN,ITROPO,IONO,ICLPOL,ICLKSV,IOUTSV,
     2                  NFTOT,OBSFIL,FILFRQ,ICAMPN,STNAME,STFIL,
     4                  XSTAT,XSTELL,DATUM,
     5                  IRUNIT,NEPOCH,IDELTT,TIMREF,NSATEL,
     6                  SATNUM,OBSSAT,XSTANW,MCOR,XSTNEL,MELL,CLKMOD,
     7                  MCLOCK,SIGMA0,OFFS0,IMFIL,CSESS,RMSSAT,
     8                  IOUTLR,DIFMAX,CONFID,MINDOF,USRSIG,NDEL,LSTDEL,
     9                  NITERF,TIMOFF,SIGOFF,INDEX,KINEST)
CC
CC NAME       :  CDRESU
CC
CC PURPOSE    :  PRINT RESULTS OF ALL FILES (PGM CODSPP)
CC               (INCL. A PRIORI INFO)
CC
CC PARAMETERS :
CC         IN :  PGMNAM : PROGRAM NAME                        CHR*6
CC               CAMPGN(I): CAMPAIGNS                          CH*16
CC                        I=1,..,NCAMP
CC               STITLE : SHORT TITLE                         CHR*64
CC               NSAMPL : SAMPLING RATE (ONLY EVERY N-TH OBS.)  I*4
CC               ZENMAX : MAXIMUM ZENITH DISTANCE  (RAD)        R*8
CC               OBSWIN(K)  : OBSERVATION WINDOW (MJD)-INPUT    R*8(2)
CC               FILWIN(K,I): OBSERVATION WINDOW (MJD)-FOR FILE R*8(2,IFIL)
CC                        K=1,2  I=1,NFTOT
CC               ITROPO : TROPOSPHERIC MODEL                    I*4
CC               IONO   : IONOSPHERIC MODEL                     I*4
CC               ICLPOL : TYPE OF CLOCK MODELLING               I*4
CC                        ICLPOL>0: POL. OF DEGREE ICLPOL
CC                        ICLPOL=-1: ONE OFFSET PER EPOCH
CC               ICLKSV : SAVE CLOCK ESTIMATES INTO OBS FILE    I*4
CC                         0: DO NOT SAVE AT ALL
CC                         1: CODE OBS FILES ONLY
CC                         2: PHASE OBS FILES ONLY
CC                         3: CODE+PHASE OBS FILES
CC                        ICLKSV(I),I=1,NFTOT
CC               IOUTSV : MARK OUTLIERS IN THE OBS. FILES       I*4
CC                         0: DO NOT SAVE AT ALL
CC                         1: CODE OBS FILES ONLY
CC                         2: PHASE OBS FILES ONLY
CC                         3: CODE+PHASE OBS FILES
CC                        IOUTSV(I),I=1,NFTOT
CC               NFTOT  : TOTAL NUMBER OF FILES                 I*4
CC               OBSFIL(I): OBSERVATION FILE NAMES             CH*32
CC                        I=1,2,..,NFTOT
CC               FILFRQ(I): FREQUENCY   TO BE PROCESSED         I*4
CC                        I=1,2,..,MAXFIL
CC               ICAMPN : CAMPAIGN NUMBER OF FILE I             I*4
CC                        I=1,2,..,MAXFIL
CC               STNAME(I): STATION NAMES                      CH*16
CC                        I=1,2,..,NSTAT
CC               STFIL(K,I),K=1,2, I=...: INTERNAL NUMBERS      I*4
CC                        OF STATIONS OBSERVING IN FILE I
CC               XSTAT(K,I),K=1,2,3;I=1,..,NSTAT: RECTANGULAR   R*8
CC                        STATION COORDINATES (WGS-84)
CC               XSTELL(K,I),K=1,2,3;I=1,..,NSTAT: ELLIPSO-     R*8
CC                        IDAL STATION COORDINATES IN SPECI-
CC                        FIED DATUM
CC               DATUM   : LOCAL GEODETIC DATUM                CH*16
CC               IRUNIT(L,I): RECEIVER UNIT NUMBER              I*4
CC                        K=1,2
CC                        I=1,2,...,NFTOT
CC               NEPOCH(I): NUMBER OF EPOCHS IN FILE            I*4
CC                        I=1,2,...,NFTOT
CC               IDELTT(I): OBSERVATION INTERVAL (SEC)          I*4
CC                        I=1,2,...,NFTOT
CC               TIMREF(I): REFERENCE TIME OF FILE (MJD)        R*8
CC                        I=1,2,...,NFTOT
CC               NSATEL(I): NUMBER OF SATELLITES FOR FILE I     I*4
CC                        I=1,2,...,NFTOT
CC               SATNUM(K,I): SVN NUMBERS                       I*4
CC                        K=1,2,...,NSATEL
CC                        I=1,2,...,NFTOT
CC               OBSSAT(L,K,I): NUMBER OF OBSERVATION PER SAT.  I*4
CC                        I=1: TOTAL NUMBER
CC                        I=2: UNUSED
CC                        K=1,2,...,MAXSAT
CC                        I=1,2,...,NFTOT
CC               XSTANW(K,I): NEW RECTANGULAR STATION COORD.    R*8
CC                        K=1,2,3
CC                        I=1,2,...,NFTOT
CC               MCOR(K,I): RMS ERRORS OF COORDINATES           R*8
CC                        K=1,2,3
CC                        I=1,2,...,NFTOT
CC               XSTNEL(K,I): NEW ELLIPSOIDAL STATION COORD.    R*8
CC                        IN SPECIFIED DATUM
CC                        K=1,2,3
CC                        I=1,2,...,NFTOT
CC               MELL(K,I): RMS ERRORS OF COORDINATES (ELL.)    R*8
CC                        K=1,2,3
CC                        I=1,2,...,NFTOT
CC               CLKMOD(K,I): NEW RECEIVER CLOCK PARAMETERS     R*8
CC                        K=1,2,...,ICLPOL
CC                        I=1,2,...,NFTOT
CC               MCLOCK(K,I): RMS ERRORS OF CLOCK PARAMETERS    R*8
CC                        K=1,2,...,ICLPOL
CC                        I=1,2,...,NFTOT
CC               SIGMA0(I): SIGMA0                              R*8
CC                        I=1,2,...,NFTOT
CC               OFFS0(I): CLOCK OFFSET OF REFERENCE EPOCH      R*8
CC                         I=1,MAXFIL
CC               IMFIL(I): FILE FLAG FOR FILES WITH MISMATCHING
CC                         PROCESSING OPTIONS                   I*4
CC                         I=1,MAXFIL
CC               CSESS(J,I): SESSION IDENT. (J=1) AND SESSION  CH*4
CC                        FILE IDENT. (J=2),I=1,2,...,NFTOT
CC               RMSSAT(ISAT,IFIL) : SATELLITE DEPENDED        R*8(*)
CC                              ACCURACY STATISTICS
CC               IOUTLR: OUTLIER DETECTION (YES=1, NO=0)        I*4
CC               DIFMAX: MAXIMUM RESIDUAL DIFFERENCE TO "BEST   R*8
CC                       SATELLITE
CC               CONFID: CONFIDENCE INTERVAL FOR OUTLIERS       R*8
CC                       (E.G. 3.D0 FOR 3-SIGMA INTERVAL)
CC               MINDOF: MINIMAL ALLOWED DEGREE OF FREEDOM      I*4
CC               USRSIG: MAXIMAL RMS OF EPOCH SOLUTIONS         R*8
CC               NDEL   : NUMBER OF DELETIONS                   I*4
CC               LSTDEL(K,I),K=1,..,5, I=1,2,..,NDEL            I*4
CC                        DEFINITION OF MARK REQUEST NUMBER I
CC                        (1,I): SV-NUMBER
CC                        (2,I): FIRST EPOCH OF MARKED AREA I
CC                        (3,I): LAST  EPOCH OF MARKED AREA I
CC                        (4,I): FILE NUMBER
CC                        (5,I): MARKED BY
CC                               =1: OUTLIER DETECTION
CC                               =2: MISSING ORBITS
CC                               =3: SATCRUX
CC                               =4: CLOCK
CC                               =5: PROBLEM WITH KIN.
CC                               ALL AREAS MARKED OR CHANGED IN THE
CC                               LATEST RUN HAVE A NEGATIVE SIGN
CC              NITERF(I),I=1,..,NFTOT: NUMBER OF ITERATIONS    I*4
CC              KINEST : ESTIMATION OF KINEMATIC COORDINATES    I*4
CC                         =0 NO KINEMATIC ESTIMATION
CC                         =1 ESTIMATE KINEMATIC COORDINATES
CC      OUT :   TIMOFF : TIME OFFSET BETWEEN GLONASS AND GPS    R*8
CC                       TIME SYSTEM (S)
CC              SIGOFF : RMS ERROR OF TIME OFFSET
CC      LOCAL : INDEX(I),I=1,MAXSAT: INDEX TO ORDER SATELLITES  I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  T. SCHILDKNECHT
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/12/11 12:02
CC
CC CHANGES    :  17-JUL-91 : ??: WRITE RESULT SUMMARY FILES (GEOCENTRIC AND
CC                               ELLIPSOIDAL COORDINATES)
CC               23-APR-92 : ??: NEW INTERNAL FILE NAMES "SMCINO" AND
CC                               "SMEINO"
CC               26-MAY-92 : LM: SATELLITE DEPENDENT RMS
CC               29-APR-93 : EB: TEST OF MAX. LINES IN FILE FILSMC
CC               23-NOV-93 : MR: PRINT DELETED AREAS FOR ALL FILES
CC               08-MAR-94 : MR: ORDER SATELLITES FOR PRINTING
CC               31-MAR-94 : MR: INCREASE #SAT FROM 26 TO 30 IN FORMATS
CC               14-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               06-SEP-94 : MR: MARKING TYPE 4 "NO SAT. CLOCKS"
CC               09-FEB-95 : SF: MAXLOOP CHANGED TO MXLOOP
CC               29-SEP-95 : TS: ALLOW TROPOSPHERE TYPE 4: MARINI
CC               30-OCT-95 : MR: ADD WARNING MESSAGE IF CLOCKS NOT
CC                               SAVED IN PHASE FILES
CC               13-DEC-95 : MR: PRINT NUMBER OF ITERATIONS
CC               24-JUN-96 : TS: ALLOW ESTIMATED TROPOSPHERE INPUT
CC               06-OCT-97 : HH: PRINT GPS/GLONASS TIME DIFFERENCE
CC               30-OCT-97 : DI: USE NEW FUNCTION "MIXSVN"
CC                               CHANGE FORMAT FOR CLOCK PARAMETERS, GPS
CC                               AND GLONASS SATELLITE STATISTICS
CC               31-OCT-97 : DI: HANDLE CASE "GOOD OBS = 0"
CC               05-NOV-97 : DI: WRITE CORRECT RESULT SUMMARIES FOR GLONASS
CC               19-NOV-97 : SS: INCLUDE "PGMVER"
CC               07-MAY-98 : MR: ADD I:MAXSAT FOR DECL. OF BDPERR,BDPERG
CC               25-AUG-98 : MR: COMPUTE COMBINED SYSTEM OFFSET
CC                               (GLONASS/GPS)
CC               24-SEP-98 : MR: CHECK TIME OFFSET SINGULARITIES
CC               14-JAN-00 : RD: NEW OPTION: SAVE CLK ESTIM. INTO OBS FILES
CC               08-MAR-00 : HH: COMPUTE AVERAGE OF GLONASS SYSTEMTIME OFFSET
CC               22-AUG-00 : RD: SAVE CLK ESTIM.: ANOTHER REALIZATION
CC               30-OCT-00 : HU: INQUIRE REPLACED BY CALL INQUIRE, HOLLERIT
CC                               REPLACED BY CHARACTER
CC               09-NOV-00 : CU: SWITCH TO NEW MENU SYSTEM
CC               08-JAN-01 : RD: MARK OUTLIERS IN OBS FILES
CC               10-JAN-01 : RD: OBSWIN FROM NEW MENU (MAY BE FILE SPECIFIC)
CC               06-JUN-01 : MR: USE "SVNSYS" INSTEAD OF "MIXSVN", REMOVE
CC                               SUPERFLUOUS PARAMETERS
CC               29-AUG-02 : DS: ESTIMATION OF KIN COORDINATES - RESULTS
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               18-FEB-03 : HU: USE PGMVER FROM M_BERN
CC               19-MAY-03 : RD: INIT TIME WINDOW TO (/0d0,1d20/)
CC               31-JUL-03 : RD: MARK BECAUSE OF KINEMATICS
CC               22-AUG-03 : AJ: PRINT KINEMATIC SETINGS
CC               10-MAR-04 : HB: CHANGE ORDER OF MODULES
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               16-NOV-06 : RD: INCREASE #SAT FROM 30 TO 32 IN FORMATS
CC               16-MAY-07 : AG: GALILEO OUTPUT IMPLEMENTED
CC               30-JUN-08 : RD: VMF ADDED
CC               01-OCT-09 : RD/SL: WRITE FORMAT STATEMENTS CORRECTED
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC               14-FEB-11 : RD: REMOVE MAXSTA-COMMON (NOT NEEDED)
CC               26-MAR-12 : RD: USE TIMSTR AS MODULE NOW
CC               28-MAR-12 : RD: USE SVNSYS AS MODULE NOW
CC               15-OCT-12 : RD/SL: BERNESE GPS->GNSS SOFTWARE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: pgmver,lfnloc,lfnprt,lfnerr,lfnplt,lfnres
      USE m_maxdim, ONLY: MAXSAT
      USE d_const,  ONLY: AE, DATE, PI, TIME
      USE s_iordup
      USE s_opnfil
      USE f_djul
      USE s_inquire
      USE s_opnerr
      USE s_timstr
      USE f_svnsys
      USE s_jmt
      USE s_radgms
      USE f_lengt1
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ICMP  , IDAY  , IDEL  , IELMAX, IEP1  , IEP2  ,
     1          IFIL  , IFLOLD, II    , IND   , IONO  , IONOP , IOS   ,
     2          IOSTAT, IOUTLR, IRCAX1, IRCSMC, IRCSME, IREC  , ISTA  ,
     3          ISVN  , ITROPO, ITYP  , J0    , J1    , J2    , JAN   ,
     4          KINEST, M0    , M1    , M2    , MIN0  , MIN1  ,
     5          MIN2  , MINDOF, MXCCLK, MXCCMP, MXCCOR, MXCFIL, MXCSAT,
     6          MXCSVN, MXLOOP, NDEL  , NFIL  , NFTOT , NGACT ,
     7          NOBSG , NOBSR , NOBSE , NRACT , NSACT , NSAMPL, NEACT
C
      REAL*8    CONFID, D0    , D1    , D2    , DIFMAX, OBSOK , RMSGAL,
     1          OFFSUM, RMSGLO, RMSGPS, SEC0  , SEC1  , SEC2  , SIGOFF,
     2          TIMOFF, TIMRE1, USRSIG, WGT   , WGTSUM, ZENMAX
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C  DECLARATIONS
C  ------------
      REAL*8 OBSWIN(2),FILWIN(2,MXCFIL),TIMREF(MXCFIL)
      REAL*8 XSTAT(:,:),XSTELL(:,:)
      REAL*8 MCLOCK(MXCCLK,MXCFIL),SIGMA0(MXCFIL)
      REAL*8 MCOR(MXCCOR,MXCFIL),MELL(MXCCOR,MXCFIL),MELLM(3)
      REAL*8 XSTANW(3,MXCFIL),XSTNEL(3,MXCFIL),CLKMOD(MXCCLK,MXCFIL)
      REAL*8 S(3),OFFS0(MXCFIL),RMSSAT(MXCSAT,*)
      REAL*8 TMJD(2),BDPERG(MAXSAT),BDPERR(MAXSAT),BDPERE(MAXSAT)
C
      INTEGER*4 FILFRQ(MXCFIL)
      INTEGER*4 STFIL(2,MXCFIL)
      INTEGER*4 NSATEL(MXCFIL),IMFIL(MXCFIL)
      INTEGER*4 IDELTT(MXCFIL),ICAMPN(MXCFIL)
      INTEGER*4 NEPOCH(MXCFIL),ICLPOL(MXCFIL),ICLKSV(MXCFIL)
      INTEGER*4 IRUNIT(2,MXCFIL)
      INTEGER*4 SATNUM(MXCSAT,MXCFIL)
      INTEGER*4 OBSSAT(2,MXCSAT,MXCFIL)
      INTEGER*4 IG(3),M(3),OBSTOT(6),H0,H1,H2
      INTEGER*4 LSTDEL(5,*),INDEX(*),NITERF(*),IOUTSV(*)
C
      CHARACTER*512 STRING
      CHARACTER*132 STRCPY
      CHARACTER*64  STITLE
      CHARACTER*36  TSTRNG
      CHARACTER*32  OBSFIL(MXCFIL),FILSMC,FILSME
      CHARACTER*32  FILAX1,FILAX2
      CHARACTER*17  LONTXT
      CHARACTER*16  LATTXT
      CHARACTER*16  CAMPGN(MXCCMP),STNAME(:),DATUM
      CHARACTER*12  TROPT(21),IONTXT(2)
      CHARACTER*6   MXNSAT,MXNSVN,MXNCLK,MXNFIL
      CHARACTER*6   MXNCMP,MXNCOR,PGMNAM
      CHARACTER*4   CSESS(2,MXCFIL)
      CHARACTER*3   TYPE(5)
      CHARACTER*2   TRPMOD(21)
      CHARACTER*1   VORZ(3),IONMOD(2),CLKTXT
C
      LOGICAL*4     YES,FLGMIX,FLGMX1
C
C  COMMON FOR CONSTANTS AND MAXIMAL DIMENSIONS
C  -------------------------------------------
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMSVN/MXCSVN,MXNSVN
      COMMON/MCMCOR/MXCCOR,MXNCOR
      COMMON/MCMCLK/MXCCLK,MXNCLK
      COMMON/MCMFIL/MXCFIL,MXNFIL
      COMMON/MCMCMP/MXCCMP,MXNCMP
C
      DATA  TROPT/'   NONE     ','SAASTAMOINEN',' HOPFIELD   ',
     1            '            ','   MARINI   ','   NIELL    ',
     2            '   GMF      ','  VMF       ','            ',
     3            '            ','            ',' DRY_SAAST  ',
     4            'DRY_HOPFIELD','            ','            ',
     5            ' DRY_NIELL  ','  DRY_GMF   ',' DRY_VMF    ',
     6            '            ','            ',' ESTIMATED  '/
      DATA IONTXT/'    NONE    ','SINGLE LAYER'/
      DATA TRPMOD/' N',' S',' H','  ',' M',' N',' G',' V','  ','  ',
     1            '  ','DS','DH','  ','  ','DN','DG','DF','  ','  ',
     2            ' E'/
      DATA IONMOD/'N','Y'/
      DATA TYPE/'OUT','ORB','CRX','CLK','KIN'/
      DATA JAN/1/
C
      MXLOOP=50000
C
C  FILE FOR SUMMARY OF XYZ-COORDINATES
C  -----------------------------------
      CALL GTFLNA(0,'SMCINO ',FILSMC,IRCSMC)
      IF (IRCSMC.EQ.0) THEN
C
C  OPEN SCRATCH FILE FOR COPY
        CALL GTFLNA(1,'AUXFIL1',FILAX1,IRCAX1)
        CALL OPNFIL(LFNPLT,FILAX1,' ',' ',' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNPLT,IOSTAT,FILAX1,'CDRESU')
C
C  SUMMARY FILE EXISTS ALREADY ?
        CALL INQUIRE(FILE=FILSMC,EXIST=YES)
        IF (YES) THEN
          CALL OPNFIL(LFNLOC,FILSMC,' ',' ',' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILSMC,'CDRESU')
C
C  COPY SUMMARY FILE TO SCRATCH FILE
          DO 10 IREC=1,6
            READ(LFNLOC,11,END=25) STRCPY
            WRITE(LFNPLT,11) STRCPY(1:LENGT1(STRCPY))
11          FORMAT(A)
10        CONTINUE
C
          DO 20 IREC=1,MXLOOP
            READ(LFNLOC,11,END=25) STRCPY
            IF (STRCPY(1:16).EQ.' ') GOTO 25
            WRITE(LFNPLT,11) STRCPY(1:LENGT1(STRCPY))
20        CONTINUE
          IF (IREC.EQ.MXLOOP)THEN
            WRITE(LFNERR,16) FILSMC,MXLOOP
16          FORMAT(/,' *** SR CDRESU: FILE ',A32,' LONGER THAN ',I4,/,
     1           16X,'INCREASE MXLOOP IN SR CDRESU',I4,/)
          ENDIF
C
25        CLOSE(UNIT=LFNLOC)
C
C WRITE TITLE LINES (XYZ-COORDINATES)
        ELSE
          WRITE(LFNPLT,26)
26        FORMAT('SINGLE POINT POSITIONING SOLUTIONS: GEOCENTRIC ',
     1           'COORDINATES IN WGS-84',/,68('-'),//,
     2           'STATION NAME     SESS F FR C TR I EL NS  DT   ',
     3           '#OBS   RMS(M)        X (M)      SX(M)        Y ',
     4           '(M)      SY(M)        Z (M)      SZ(M)',/,132('-'),/)
        ENDIF
      ENDIF
C
C  FILE FOR SUMMARY OF ELLIPSOIDAL COORDINATES
C  -------------------------------------------
      CALL GTFLNA(0,'SMEINO ',FILSME,IRCSME)
      IF (IRCSME.EQ.0) THEN
C
C  OPEN SCRATCH FILE FOR COPY
        CALL GTFLNA(1,'AUXFIL2',FILAX2,IRCAX1)
        CALL OPNFIL(LFNRES,FILAX2,' ',' ',' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRES,IOSTAT,FILAX2,'CDRESU')
C
C  SUMMARY ERROR FILE EXISTS ALREADY ?
        CALL INQUIRE(FILE=FILSME,EXIST=YES)
        IF (YES) THEN
          CALL OPNFIL(LFNLOC,FILSME,' ',' ',' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILSME,'CDRESU')
C
C  COPY SUMMARY FILE TO SCRATCH FILE
          DO 30 IREC=1,6
            READ(LFNLOC,11,END=45) STRCPY
            WRITE(LFNRES,11) STRCPY(1:LENGT1(STRCPY))
30        CONTINUE
C
          DO 40 IREC=1,30000
            READ(LFNLOC,11,END=45) STRCPY
            IF (STRCPY(1:16).EQ.' ') GOTO 45
            WRITE(LFNRES,11) STRCPY(1:LENGT1(STRCPY))
40        CONTINUE
C
45        CLOSE(UNIT=LFNLOC)
C
C WRITE TITLE LINES (ELLIPSOIDAL COORDINATES)
        ELSE
          WRITE(LFNRES,46)
46        FORMAT('SINGLE POINT POSITIONING SOLUTIONS: ELLIPSOIDAL ',
     1           'COORDINATES IN WGS-84',/,69('-'),//,
     2           'STATION NAME     SESS F FR C TR I EL NS  DT   #',
     3           'OBS   RMS(M)      LATITUDE       SN(M)       LON',
     4           'GITUDE      SE(M)    HEIGHT    SH(M)',/,132('-'),/)
        ENDIF
      ENDIF
C
C  CUT-OFF ANGLE
      IELMAX=IDNINT(90.D0-ZENMAX*180.D0/PI)
C
C  LOOP OVER FILES
C  ---------------
      FLGMX1=.FALSE.
      DO 500 IFIL=1,NFTOT
C
C  INTERNAL STATION NUMBER, INTERNAL CAMPAIGN NUMBER
        ISTA=STFIL(1,IFIL)
        ICMP=ICAMPN(IFIL)
C
C  CHECK FOR MARKED FILES (INCOMPATIBLE OPTIONS)
        IF (IMFIL(IFIL) .EQ. 1) THEN
          IF (IRCSMC.EQ.0) THEN
            WRITE(LFNPLT,51) STNAME(ISTA),CSESS(1,IFIL),
     1                       CSESS(2,IFIL)(1:1)
51          FORMAT(A16,1X,A4,1X,A1,1X,'ERROR: NO RESULTS AVAILABLE')
          ENDIF
          IF (IRCSME.EQ.0) THEN
            WRITE(LFNRES,51) STNAME(ISTA),CSESS(1,IFIL),
     1                       CSESS(2,IFIL)(1:1)
          ENDIF
          GOTO 500
        ENDIF
C
C CHECK OBSERVATION TYPE
C ----------------------
        FLGMIX=SVNSYS(-1,NSATEL(IFIL),SATNUM(1,IFIL)).AND.(KINEST.EQ.0)
        FLGMX1=FLGMX1.OR.FLGMIX
C
C  HEADER
        WRITE(LFNPRT,101)CAMPGN(ICMP),PGMNAM,DATE,TIME,STITLE,PGMVER
101     FORMAT('1'/1X,1A16,82X,'PROGRAM ',1A6,4X,1A9,1X,1A5/
     1         ' ',1A64,34X,
     2             'BERNESE GNSS SOFTWARE VERSION ',1A3/' ',131('-'))
C
C  STATION, FILE, RECEIVER UNIT
        WRITE(LFNPRT,103)STNAME(ISTA),OBSFIL(IFIL),IRUNIT(1,IFIL)
103     FORMAT(//' STATION:  ',1A16,' FILE:  ',1A32,
     1           ' RECEIVER UNIT:  ',I8/
     2           1X,91('-'))
C
C  GPSDAY
        CALL JMT(TIMREF(IFIL),J0,M0,D0)
        TIMRE1=DJUL(J0,JAN,1.D0)
        IDAY=(TIMREF(IFIL)-TIMRE1)+1
        WRITE(LFNPRT,107)IDAY
107     FORMAT(//' DAY OF YEAR         :',I5)
C
C  OBSERVATION WINDOW, REQUESTED WINDOW
        TIMRE1=TIMREF(IFIL)+(NEPOCH(IFIL)-1)*IDELTT(IFIL)/86400.D0
        CALL JMT(TIMRE1,J2,M2,D2)
        CALL RADGMS(3,D0,VORZ(1),H0,MIN0,SEC0)
        CALL RADGMS(3,D2,VORZ(1),H2,MIN2,SEC2)
        WRITE(LFNPRT,111)
111     FORMAT(/38X,'FROM',T70,'TO')
        WRITE(LFNPRT,113)J0,M0,IDINT(D0),H0,MIN0,SEC0,
     1                    J2,M2,IDINT(D2),H2,MIN2,SEC2
113     FORMAT(  ' OBSERVATIONS        :',
     1            2(7X,I4,2('-',I2.2),2X,2(I2.2,':'),F5.2))
C
        WRITE(STRING,119)
119     FORMAT(  ' REQUESTED WINDOW    :',T40,'--')
        IF(OBSWIN(1).NE.0D00) THEN
          CALL JMT(FILWIN(1,IFIL),J1,M1,D1)
          CALL RADGMS(3,D1,VORZ(1),H1,MIN1,SEC1)
          WRITE(STRING,121)J1,M1,IDINT(D1),H1,MIN1,SEC1
121       FORMAT(  ' REQUESTED WINDOW    :',
     1            7X,I4,2('-',I2.2),2X,2(I2.2,':'),F5.2)
        ENDIF
        STRING(70:71)='--'
        IF(OBSWIN(2).NE.1D20) THEN
          CALL JMT(FILWIN(2,IFIL),J2,M2,D2)
          CALL RADGMS(3,D2,VORZ(1),H2,MIN2,SEC2)
          WRITE(STRING(60:82),123)J2,M2,IDINT(D2),H2,MIN2,SEC2
123       FORMAT(I4,2('-',I2.2),2X,2(I2.2,':'),F5.2)
        ENDIF
        WRITE(LFNPRT,129)STRING(1:LENGT1(STRING))
129     FORMAT(A)
C
C  MEASUREMENT INTERVAL, SAMPLING RATE
        WRITE(LFNPRT,133) IDELTT(IFIL),NSAMPL
133     FORMAT(/' MEASUREMENT INTERVAL:',I5,' SEC'/
     1          ' SAMPLING RATE       :',I5)
C
C  FREQUENCY, ELEVATION LIMIT
        WRITE(LFNPRT,137)FILFRQ(IFIL),IELMAX
137     FORMAT( ' PROCESSED FREQUENCY :   L',I1/
     1          ' ELEVATION LIMIT     :',I5,' DEG')
C
C  ATMOSPHERE MODELS
        WRITE(LFNPRT,141)
141     FORMAT(/T32,'TROPOSPHERE',T58,'IONOSPHERE')
        IF(FILFRQ(IFIL).EQ.3) THEN
          IONOP=0
        ELSE
          IONOP=IONO
        ENDIF
        WRITE(LFNPRT,143)TROPT(ITROPO+1),IONTXT(IONOP+1)
143     FORMAT(' ATMOSPHERE MODELS   :',T32,1A12,T57,1A12)
C
C ORDER SATELLITES
        NSACT=NSATEL(IFIL)
        CALL IORDUP(SATNUM(1,IFIL),NSACT,INDEX)
C FIND NUMBER OF GPS, GLONASS AND GALILEO SATELLITES
        NGACT=0
        NRACT=0
        NEACT=0
        DO 148 I=1,NSACT
          IF (SATNUM(INDEX(I),IFIL).LT.100) THEN
            NGACT=NGACT+1
          ELSEIF (SATNUM(INDEX(I),IFIL).LT.200) THEN
            NRACT=NRACT+1
          ELSEIF (SATNUM(INDEX(I),IFIL).LT.300) THEN
            NEACT=NEACT+1
          ENDIF
148     CONTINUE
C
C  STATISTICS
        DO I=1,6
          OBSTOT(I)=0
        ENDDO
        RMSGPS=0.D0
        RMSGLO=0.D0
        RMSGAL=0.D0
        DO I=1,NSACT
          OBSOK = OBSSAT(1,I,IFIL)-OBSSAT(2,I,IFIL)
          IF(SATNUM(I,IFIL).LT.100)THEN
            OBSTOT(1)=OBSTOT(1)+OBSSAT(1,I,IFIL)
            OBSTOT(2)=OBSTOT(2)+OBSSAT(2,I,IFIL)
            RMSGPS=RMSGPS+RMSSAT(I,IFIL)
          ELSEIF(SATNUM(I,IFIL).LT.200) THEN
            OBSTOT(3)=OBSTOT(3)+OBSSAT(1,I,IFIL)
            OBSTOT(4)=OBSTOT(4)+OBSSAT(2,I,IFIL)
            RMSGLO=RMSGLO+RMSSAT(I,IFIL)
          ELSEIF(SATNUM(I,IFIL).LT.300) THEN
            OBSTOT(5)=OBSTOT(5)+OBSSAT(1,I,IFIL)
            OBSTOT(6)=OBSTOT(6)+OBSSAT(2,I,IFIL)
            RMSGAL=RMSGAL+RMSSAT(I,IFIL)
          ENDIF
          IF (OBSOK .NE. 0) THEN
            RMSSAT(I,IFIL) = SQRT(RMSSAT(I,IFIL)/OBSOK)
          ELSE
            RMSSAT(I,IFIL) = 0.D0
          END IF
        ENDDO
        NOBSG=OBSTOT(1)-OBSTOT(2)
        IF (NOBSG.NE.0) RMSGPS=SQRT(RMSGPS/NOBSG)
        NOBSR=OBSTOT(3)-OBSTOT(4)
        IF (NOBSR.NE.0) RMSGLO=SQRT(RMSGLO/NOBSR)
        NOBSE=OBSTOT(5)-OBSTOT(6)
        IF (NOBSE.NE.0) RMSGAL=SQRT(RMSGAL/NOBSE)
C
C GPS SATELLITES
C --------------
        IF (OBSTOT(1).GT.0) THEN
          WRITE(LFNPRT,151)
151       FORMAT(//,' STATISTICS FOR GPS SATELLITES:',/,
     1              ' ------------------------------',/)
          WRITE(STRING,152)(SATNUM(INDEX(II),IFIL),II=1,NGACT)
152       FORMAT(' SATELLITE NUMBER    :',40I6)
          STRING(23+NGACT*6:23+NGACT*6+8)='    TOTAL'
          WRITE(LFNPRT,129)STRING(1:LENGT1(STRING))
          WRITE(STRING,153)(OBSSAT(1,INDEX(II),IFIL),II=1,NGACT)
153       FORMAT(' OBSERVATIONS IN FILE:',40I6)
          WRITE(STRING(23+NGACT*6:23+NGACT*6+7),157)OBSTOT(1)
157       FORMAT(I8)
          WRITE(LFNPRT,129)STRING(1:LENGT1(STRING))
          DO I=1,NGACT
            IF (OBSSAT(1,INDEX(I),IFIL).EQ.0) THEN
              BDPERG(I)=0d0
            ELSE
              BDPERG(I)=1.D2*OBSSAT(2,INDEX(I),IFIL)/
     1          OBSSAT(1,INDEX(I),IFIL)
            ENDIF
          ENDDO
          WRITE(STRING,163)(BDPERG(II),II=1,NGACT)
163       FORMAT(' BAD OBSERVATIONS (%):',40F6.1)
          WRITE(STRING(23+NGACT*6:23+NGACT*6+7),158) 1.D2*OBSTOT(2)/
     1      OBSTOT(1)
158       FORMAT(F8.1)
          WRITE(LFNPRT,129)STRING(1:LENGT1(STRING))
          WRITE(STRING,164,IOSTAT=IOS)
     1      (RMSSAT(INDEX(II),IFIL),II=1,NGACT)
164       FORMAT(' RMS ERROR (M)       :',40F6.1)
          WRITE(STRING(23+NGACT*6:23+NGACT*6+7),167) RMSGPS
167       FORMAT(F8.1)
          WRITE(LFNPRT,129)STRING(1:LENGT1(STRING))
        END IF
C
C GLONASS SATELITTES
C ------------------
        IF (OBSTOT(3).GT.0) THEN
          WRITE(LFNPRT,1511)
1511      FORMAT(//,' STATISTICS FOR GLONASS SATELLITES:',/,
     1              ' ----------------------------------',/)
          WRITE(STRING,152)(SATNUM(INDEX(II),IFIL),
     1                                    II=NGACT+1,NGACT+NRACT)
          STRING(23+NRACT*6:23+NRACT*6+8)='    TOTAL'
          WRITE(LFNPRT,129)STRING(1:LENGT1(STRING))
          WRITE(STRING,153)
     1      (OBSSAT(1,INDEX(II),IFIL),II=NGACT+1,NGACT+NRACT)
          WRITE(STRING(23+NRACT*6:23+NRACT*6+7),157)OBSTOT(3)
          WRITE(LFNPRT,129)STRING(1:LENGT1(STRING))
          DO I=NGACT+1,NGACT+NRACT
            IF (OBSSAT(1,INDEX(I),IFIL).EQ.0) THEN
              BDPERR(I)=0d0
            ELSE
              BDPERR(I)=1.D2*OBSSAT(2,INDEX(I),IFIL)/
     1          OBSSAT(1,INDEX(I),IFIL)
            ENDIF
          ENDDO
          WRITE(STRING,163)(BDPERR(II),II=NGACT+1,NGACT+NRACT)
          WRITE(STRING(23+NRACT*6:23+NRACT*6+7),158) 1.D2*OBSTOT(4)/
     1      OBSTOT(3)
          WRITE(LFNPRT,129)STRING(1:LENGT1(STRING))
          WRITE(STRING,164,IOSTAT=IOS)(RMSSAT(INDEX(II),IFIL)
     1                                          ,II=NGACT+1,NGACT+NRACT)
          WRITE(STRING(23+NRACT*6:23+NRACT*6+7),167) RMSGLO
          WRITE(LFNPRT,129)STRING(1:LENGT1(STRING))
        ENDIF
C
C GALILEO SATELLITES
C ------------------
        IF (OBSTOT(5).GT.0) THEN
          WRITE(LFNPRT,1512)
1512      FORMAT(//,' STATISTICS FOR GALILEO SATELLITES:',/,
     1              ' ----------------------------------',/)
          WRITE(STRING,152)(SATNUM(INDEX(II),IFIL),
     1                          II=NGACT+NRACT+1,NGACT+NRACT+NEACT)
          STRING(23+NEACT*6:23+NEACT*6+8)='    TOTAL'
          WRITE(LFNPRT,129)STRING(1:LENGT1(STRING))
          WRITE(STRING,153)(OBSSAT(1,INDEX(II),IFIL),
     1                          II=NGACT+NRACT+1,NGACT+NRACT+NEACT)
          WRITE(STRING(23+NEACT*6:23+NEACT*6+7),157)OBSTOT(5)
          WRITE(LFNPRT,129)STRING(1:LENGT1(STRING))
          DO I=NGACT+NRACT+1,NGACT+NRACT+NEACT
            IF (OBSSAT(1,INDEX(I),IFIL).EQ.0) THEN
              BDPERE(I)=0d0
            ELSE
              BDPERE(I)=1.D2*OBSSAT(2,INDEX(I),IFIL)/
     1          OBSSAT(1,INDEX(I),IFIL)
            ENDIF
          ENDDO
          WRITE(STRING,163)(BDPERE(II),
     1                         II=NGACT+NRACT+1,NGACT+NRACT+NEACT)
          WRITE(STRING(23+NEACT*6:23+NEACT*6+7),158) 1.D2*OBSTOT(6)/
     1      OBSTOT(5)
          WRITE(LFNPRT,129)STRING(1:LENGT1(STRING))
          WRITE(STRING,164,IOSTAT=IOS)
     1      (RMSSAT(INDEX(II),IFIL),II=NGACT+NRACT+1,NGACT+NRACT+NEACT)
          WRITE(STRING(23+NEACT*6:23+NEACT*6+7),167) RMSGAL
          WRITE(LFNPRT,129)STRING(1:LENGT1(STRING))
        END IF
C
C  RESULTS (SIGMA0)
C  ----------------
        IF (KINEST.EQ.0) THEN
          WRITE(LFNPRT,171)OBSTOT(1)+OBSTOT(3)+OBSTOT(5),
     1      1.D2*(OBSTOT(2)+OBSTOT(4)+OBSTOT(6))/
     2           (OBSTOT(1)+OBSTOT(3)+OBSTOT(5)),
     3      SIGMA0(IFIL),NITERF(IFIL)
171       FORMAT(//,' RESULTS:'/
     1              ' --------'//
     2              ' OBSERVATIONS IN FILE:',I10,
     3            /,' BAD OBSERVATIONS    :',F13.2,' %',
     4            /,' RMS OF UNIT WEIGHT  :',F13.2,' M',
     5            /,' NUMBER OF ITERATIONS:',I10)
C
C  STATION COORDINATES WGS
          WRITE(LFNPRT,173)DATUM
173       FORMAT(//' STATION COORDINATES:'/
     1             ' --------------------'//
     2             ' LOCAL GEODETIC DATUM:  ',1A16//
     3            T37,'A PRIORI',T54,'NEW',T69,'NEW- A PRIORI',T85,
     4            'RMS ERROR')
          WRITE(LFNPRT,179)STNAME(ISTA),
     1      XSTAT(1,ISTA),XSTANW(1,IFIL),XSTANW(1,IFIL)-XSTAT(1,ISTA),
     2      MCOR(1,IFIL),
     3      '(MARKER)        ',
     4      XSTAT(2,ISTA),XSTANW(2,IFIL),XSTANW(2,IFIL)-XSTAT(2,ISTA),
     5      MCOR(2,IFIL),
     6      XSTAT(3,ISTA),XSTANW(3,IFIL),XSTANW(3,IFIL)-XSTAT(3,ISTA),
     7      MCOR(3,IFIL)
179       FORMAT(2X,1A16,2X,'X',T30,2(F16.2),F16.2,F13.2/
     2           2X,1A16,2X,'Y',T30,2(F16.2),F16.2,F13.2/
     3                  20X,'Z',T30,2(F16.2),F16.2,F13.2/)
C
C  STATION COORDINATES ELLIPSOIDAL
C  -------------------------------
          WRITE(LFNPRT,191)
     1    XSTELL(3,ISTA),XSTNEL(3,IFIL),XSTNEL(3,IFIL)-XSTELL(3,ISTA),
     2    MELL(3,IFIL)
191       FORMAT(20X,'HEIGHT',T30,2(F16.2),F16.2,F13.2)
          CALL RADGMS(1,XSTELL(1,ISTA),VORZ(1),IG(1),M(1),S(1))
          CALL RADGMS(1,XSTNEL(1,IFIL),VORZ(2),IG(2),M(2),S(2))
          CALL RADGMS(1,XSTNEL(1,IFIL)-XSTELL(1,ISTA),VORZ(3),
     1                IG(3),M(3),S(3))
          WRITE(LFNPRT,193)(VORZ(I),IG(I),M(I),S(I),I=1,3),
     1                      MELL(1,IFIL)/PI*180.D0*3600.D0
193       FORMAT(20X,'LATITUDE',T33,1A1,I3,I3,F7.3,
     1                          T49,1A1,I3,I3,F7.3,
     2                          T65,1A1,I3,I3,F7.3,
     3                          T80,F13.4)
          WRITE(LATTXT,194) VORZ(2),IG(2),M(2),S(2)
194       FORMAT(A1,I2,I3,F10.6)
          CALL RADGMS(1,XSTELL(2,ISTA),VORZ(1),IG(1),M(1),S(1))
          CALL RADGMS(1,XSTNEL(2,IFIL),VORZ(2),IG(2),M(2),S(2))
          CALL RADGMS(1,XSTNEL(2,IFIL)-XSTELL(2,ISTA),VORZ(3),
     1                IG(3),M(3),S(3))
          WRITE(LFNPRT,197)(VORZ(I),IG(I),M(I),S(I),I=1,3),
     1                      MELL(2,IFIL)/PI*180.D0*3600.D0
197       FORMAT(20X,'LONGITUDE',T33,1A1,I3,I3,F7.3,
     1                          T49,1A1,I3,I3,F7.3,
     2                          T65,1A1,I3,I3,F7.3,
     3                          T80,F13.4)
          WRITE(LONTXT,198) VORZ(2),IG(2),M(2),S(2)
198       FORMAT(A1,I3,I3,F10.6)
        END IF
C
C  CENTER COORDINATES
CDD     IF(ICENTR(ISTA).NE.ISTA) THEN
C   NOT  YET  IMPLEMENTED
C
C  RECEIVER CLOCK PARAMETERS
C  -------------------------
        IF (ICLPOL(IFIL) .GT. 0) THEN
          WRITE(LFNPRT,201)
201       FORMAT(///' CLOCK PARAMETERS:'/
     1              ' -----------------'//
     2            T24,'OFFSET (S)',T38,'DRIFT(S/S)',T52,'P2 (S/S**2)',
     3           T66,'P3 (S/S**3)',T80,'P4 (S/S**4)',T94,'P5 (S/S**5)',
     4            T108,'P6 (S/S**6)',T122,'P7 (S/S**7)')
          WRITE(LFNPRT,202)(CLKMOD(I,IFIL),I=1,ICLPOL(IFIL))
202       FORMAT(' PARAMETER',10X,F14.9,7D14.5)
          WRITE(LFNPRT,203)(MCLOCK(I,IFIL),I=1,ICLPOL(IFIL))
203       FORMAT(' RMS ERROR',10X,8D14.5)
C
          WRITE(CLKTXT,233) ICLPOL(IFIL)
233       FORMAT(I1)
        ELSE IF (ICLPOL(IFIL) .EQ. -1) THEN
          WRITE(LFNPRT,204) OFFS0(IFIL)
204       FORMAT(///' CLOCK PARAMETERS:'/
     1              ' -----------------'//
     2    ' OFFSET FOR REFERENCE EPOCH:',13X,F14.9,'  SEC',/)
C
          CLKTXT='E'
        ENDIF
        IF (FLGMIX) THEN
          IF (ICLPOL(IFIL).GT.0) THEN
            WRITE(LFNPRT,215) CLKMOD(ICLPOL(IFIL)+1,IFIL)*1.D9,
     1                        MCLOCK(ICLPOL(IFIL)+1,IFIL)*1.D9
          ELSE IF (ICLPOL(IFIL).EQ.-1) THEN
            WRITE(LFNPRT,215) CLKMOD(1,IFIL)*1.D9,
     1                        MCLOCK(1,IFIL)*1.D9
          ENDIF
215       FORMAT(/' GPS/GLONASS SYSTEM DIFFERENCE: '
     1             ,' OFFSET    : ',F10.2,' NSEC',/,
     2          32X,' RMS ERROR : ',F10.2,' NSEC',/)
        ENDIF
C
C MESSAGE WHETHER CLOCKS SAVED IN PHASE FILES
C -------------------------------------------
        IF (ICLKSV(IFIL) .EQ. 0 .OR. ICLKSV(IFIL) .EQ. 1) THEN
          WRITE(LFNERR,207) OBSFIL(IFIL)
207       FORMAT(/,' ### SR CDRESU: CLOCK OFFSETS WERE NOT STORED ',
     1      'IN PHASE OBSERVATION FILE!',
     2      /,16X,'OBSERVATION FILE NAME: ',A,/)
        ENDIF
C
C MESSAGE WHETHER CLOCKS SAVED IN CODE AND PHASE FILES
C ----------------------------------------------------
        IF (ICLKSV(IFIL) .EQ. 0 .OR. ICLKSV(IFIL) .EQ. 2) THEN
          WRITE(LFNERR,209) OBSFIL(IFIL)
209       FORMAT(/,' ### SR CDRESU: CLOCK OFFSETS WERE NOT STORED ',
     1      'IN CODE OBSERVATION FILE!',
     2      /,16X,'OBSERVATION FILE NAME: ',A,/)
        ENDIF
        IF (ICLKSV(IFIL) .EQ. 1) THEN
          WRITE(LFNPRT,205) 'CODE',' '
        ELSE IF (ICLKSV(IFIL) .EQ. 2) THEN
          WRITE(LFNPRT,205) 'PHASE',' '
        ELSE IF (ICLKSV(IFIL) .EQ. 3) THEN
          WRITE(LFNPRT,205) 'CODE+PHASE','S'
        ENDIF
205     FORMAT(/,' CLOCK OFFSETS STORED IN ',A,' OBSERVATION FILE',A)
C
C RECEIVER UNIT AND REFERENCE EPOCH
C ---------------------------------
        WRITE(LFNPRT,212)IRUNIT(1,IFIL)
212     FORMAT(/' RECEIVER UNIT       :',I8)
        WRITE(LFNPRT,213)J0,M0,IDINT(D0),H0,MIN0,SEC0
213     FORMAT( ' REFERENCE EPOCH     :',
     1            7X,I4,2('-',I2.2),2X,2(I2.2,':'),F5.2)
C
C WRITE SUMMARY LINE TO COORDINATE RESULT FILE (X,Y,Z)
C ----------------------------------------------------
        IF (IRCSMC.EQ.0) THEN
          WRITE(LFNPLT,301) STNAME(ISTA),CSESS(1,IFIL),
     1      CSESS(2,IFIL)(1:1),FILFRQ(IFIL),CLKTXT,TRPMOD(ITROPO+1),
     2      IONMOD(IONOP+1),IELMAX,NSAMPL,IDELTT(IFIL),
     3      OBSTOT(1)-OBSTOT(2)+OBSTOT(3)-OBSTOT(4),
     4      SIGMA0(IFIL),(XSTANW(I,IFIL),MCOR(I,IFIL),I=1,3)
301       FORMAT(A16,1X,A4,1X,A1,1X,'L',I1,1X,A1,1X,A2,1X,A1,2I3,I4,I8,
     1           F8.2,1X,3(F16.4,F8.4))
        ENDIF
C
C WRITE SUMMARY LINE TO CORDINATE RESULT FILE (ELLIPSOIDAL COORDINATES)
C ---------------------------------------------------------------------
        IF (IRCSME.EQ.0) THEN
          MELLM(1)=MELL(1,IFIL)*AE
          MELLM(2)=MELL(2,IFIL)*AE*DCOS(XSTNEL(1,IFIL))
          MELLM(3)=MELL(3,IFIL)
          WRITE(LFNRES,302) STNAME(ISTA),CSESS(1,IFIL),
     1      CSESS(2,IFIL)(1:1),FILFRQ(IFIL),CLKTXT,TRPMOD(ITROPO+1),
     2      IONMOD(IONOP+1),IELMAX,NSAMPL,IDELTT(IFIL),
     3      OBSTOT(1)-OBSTOT(2)+OBSTOT(3)-OBSTOT(4),
     4      SIGMA0(IFIL),LATTXT,MELLM(1),LONTXT,MELLM(2),
     5      XSTNEL(3,IFIL),MELLM(3)
302       FORMAT(A16,1X,A4,1X,A1,1X,'L',I1,1X,A1,1X,A2,1X,A1,2I3,I4,I8,
     1           F8.2,3X,A16,F8.4,2X,A17,F8.4,F11.4,F8.4)
        ENDIF
C
C NEXT FILE
C ---------
500   CONTINUE
C
C  COPY RESULT SUMMARY FILES BACK TO ORGINIAL FILES
C  ------------------------------------------------
      IF (IRCSMC.EQ.0) THEN
        WRITE(LFNPLT,501)
501     FORMAT(/,132('-'))
C
        REWIND (UNIT=LFNPLT)
        CALL OPNFIL(LFNLOC,FILSMC,' ',' ',' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILSMC,'CDRESU')
C
        DO 510 IREC=1,MXLOOP
          READ(LFNPLT,11,END=515) STRCPY
          WRITE(LFNLOC,11) STRCPY(1:LENGT1(STRCPY))
510     CONTINUE
C
515     CLOSE(UNIT=LFNPLT,STATUS='DELETE')
        CLOSE(UNIT=LFNLOC)
      ENDIF
C
C  COPY FORMAL ERROR SUMMARY FILES BACK TO ORGINIAL FILE
C  -----------------------------------------------------
      IF (IRCSME.EQ.0) THEN
        WRITE(LFNRES,502)
502     FORMAT(/,132('-'))
C
        REWIND (UNIT=LFNRES)
        CALL OPNFIL(LFNLOC,FILSME,' ',' ',' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILSME,'CDRESU')
C
        DO 520 IREC=1,MXLOOP
          READ(LFNRES,11,END=525) STRCPY
          WRITE(LFNLOC,11) STRCPY(1:LENGT1(STRCPY))
520     CONTINUE
C
525     CLOSE(UNIT=LFNRES,STATUS='DELETE')
        CLOSE(UNIT=LFNLOC)
      ENDIF
C
C WRITE DELETED AREAS FOR ALL FILES
C ---------------------------------
      WRITE(LFNPRT,901)
901   FORMAT(///,' ',79('*'),
     1         /,' SUMMARY OF BAD OBSERVATIONS',/,' ',79('*'),/)
      IF (IOUTLR.EQ.0) THEN
        WRITE(LFNPRT,902)
902     FORMAT(' NO OUTLIER DETECTION DONE')
      ELSE
        IF(KINEST.NE.1) THEN
          WRITE(LFNPRT,903) DIFMAX,CONFID
903       FORMAT(' MAXIMUM RESIDUAL DIFFERENCE ALLOWED  :',F10.2,' M',/,
     1           ' CONFIDENCE INTERVAL OF F*SIGMA WITH F:',F10.2)
        ELSE
          WRITE(LFNPRT,9031) MINDOF,USRSIG
9031      FORMAT(' MINIMAL ALLOWED DEGREE OF FREEDOM     :',I9,/,
     1           ' MAXIMAL ALLOWED RMS OF EPOCH SOLUTION :',F10.2,' M')
        END IF
        IF (IOUTSV(1) .EQ. 1) THEN
          WRITE(LFNPRT,'(A)')
     1    ' OUTLIERS MARKED IN THE CODE OBSERVATION FILES'
        ELSE IF (IOUTSV(1) .EQ. 2) THEN
          WRITE(LFNPRT,'(A)')
     1    ' OUTLIERS MARKED IN THE PHASE OBSERVATION FILES'
        ELSE IF (IOUTSV(1) .EQ. 3) THEN
          WRITE(LFNPRT,'(A)')
     1    ' OUTLIERS MARKED IN THE CODE+PHASE OBSERVATION FILES'
        ENDIF
      ENDIF
      WRITE(LFNPRT,904) NDEL
904   FORMAT(/,' NUMBER OF BAD OBSERVATION PIECES     :',I10)
C
      IF (NDEL.NE.0) THEN
        WRITE(LFNPRT,905)
905     FORMAT(/,' NUMB FIL  STATION',11X,'TYP SAT',8X,'FROM',16X,'TO',
     1         10X,'#EPO'/,1X,79('-'))
        IFLOLD=0
        DO 900 IDEL=1,NDEL
          ISVN=LSTDEL(1,IDEL)
          IEP1=LSTDEL(2,IDEL)
          IEP2=LSTDEL(3,IDEL)
          IFIL=LSTDEL(4,IDEL)
          ITYP=IABS(LSTDEL(5,IDEL))
          IF (IFIL.NE.IFLOLD) WRITE(LFNPRT,'( )')
          IFLOLD=IFIL
          TMJD(1)=TIMREF(IFIL)+(IEP1-1)*IDELTT(IFIL)/86400.D0
          TMJD(2)=TIMREF(IFIL)+(IEP2-1)*IDELTT(IFIL)/86400.D0
          CALL TIMSTR(2,TMJD,TSTRNG)
          WRITE(LFNPRT,906) IDEL,IFIL,STNAME(STFIL(1,IFIL)),
     1                      TYPE(ITYP),ISVN,TSTRNG,IEP2-IEP1+1
906       FORMAT(I5,I4,2X,A16,2X,A3,I4,2X,A36,I6)
900     CONTINUE
        WRITE(LFNPRT,907)
907     FORMAT(/,' ',79('-'),/)
      ENDIF
C
C COMPUTE WEIGHTED MEAN SYSTEM TIME OFFSET (BETWEEN GLONASS AND GPS)
C ------------------------------------------------------------------
      IF (FLGMX1) THEN
        NFIL=0
        OFFSUM=0.D0
        WGTSUM=0.D0
        DO IFIL=1,NFTOT
          IF (IMFIL(IFIL).EQ.0) THEN
            IF (ICLPOL(IFIL).GT.0) THEN
              IND=ICLPOL(IFIL)+1
            ELSE IF (ICLPOL(IFIL).EQ.-1) THEN
              IND=1
            ENDIF
            IF (MCLOCK(IND,IFIL).NE.0.D0) THEN
              NFIL=NFIL+1
              WGT=1.D0/MCLOCK(IND,IFIL)**2
              OFFSUM=OFFSUM+WGT*CLKMOD(IND,IFIL)
              WGTSUM=WGTSUM+WGT
            ENDIF
          ENDIF
        ENDDO
C
        IF (NFIL.NE.0) THEN
          TIMOFF=OFFSUM/WGTSUM
          SIGOFF=1.D0/DSQRT(WGTSUM)
        ELSE
          TIMOFF=0.D0
          SIGOFF=0.D0
        ENDIF
C
C WRITE COMBINED TIME OFFSET AND RMS ERROR
C ----------------------------------------
        WRITE(LFNPRT,908)
908     FORMAT(///,' ',79('*'),
     1           /,' GLONASS/GPS TIME OFFSETS',/,' ',79('*'),//,
     2             ' FILE  STATION NAME       TIME OFFSET (NS)    ',
     3             'RMS ERROR (NS)',/,
     4             1X,59('-'),/)
        DO IFIL=1,NFTOT
          ISTA=STFIL(1,IFIL)
          IF (IMFIL(IFIL).EQ.0) THEN
            IF (ICLPOL(IFIL).GT.0) THEN
              IND=ICLPOL(IFIL)+1
            ELSE IF (ICLPOL(IFIL).EQ.-1) THEN
              IND=1
            ENDIF
            IF (MCLOCK(IND,IFIL).NE.0.D0) THEN
              WRITE(LFNPRT,909) IFIL,STNAME(ISTA),CLKMOD(IND,IFIL)*1.D9,
     1                          MCLOCK(IND,IFIL)*1.D9
909           FORMAT(I4,3X,A16,1X,2F18.3)
            ELSE
              WRITE(LFNPRT,910) IFIL,STNAME(ISTA)
            ENDIF
          ELSE
            WRITE(LFNPRT,910) IFIL,STNAME(ISTA)
910           FORMAT(I4,3X,A16,3X,'NO RESULTS AVAILABLE')
          ENDIF
        ENDDO
        WRITE(LFNPRT,911) NFIL,TIMOFF*1.D9,SIGOFF*1.D9
911     FORMAT(/,1X,59('-'),
     1         //,I4,'   TOTAL',12X,2F18.3,/,
     2           1X,59('-'),/)
      ENDIF
C
999   RETURN
C
      END SUBROUTINE

      END MODULE
