      MODULE s_DSRDBL
      CONTAINS

C*
      SUBROUTINE DSRDBL(NFRCHK,NFRFIL,LTRIP ,IBLOCK,NEPO  ,IUSFLG,
     1                  MRK1O2,IPRNT1,MAXZEN,SECIPL,TIMREF,IDELTT,
     2                  STNAME,XWGS  ,XSTELL,RECTYP,ANTTYP,IANTEN,
     3                  CSESS ,IFRMAT,NSLIP ,LSTSLP,SLPLST,NDEL  ,
     4                  LSTDEL,NSATEL,NUMSAT,IIONO ,ISATCO,KINSTA,
     5                  NFRAUX,FRQAUX,NEPEFF,NSATBL,SVNBL ,
     6                  NOBSAT,OBTIME,AOBS  ,ABSVAL,OBSFLG,IRETRN,
     7                  MXHOLE,MNCONT,IAMNEW,NNEWAM,LSTAMB,LASTCS,
     8                  NTONLY,L5CLEA,IPRNT2,AMSFLG,NDIFF ,MXZLEO,
     9                  ITROPO,IEXTRA,MEATYP,POSECC,AELL  ,BELL  ,
     .                  DXELL,DRELL,SCELL)
CC
CC NAME       :  DSRDBL
CC
CC PURPOSE    :  - READ ALL OBSERVATIONS OF A BLOCK OF NEPO EPOCHS
CC               - APPLY A PRIORI KNOWN CYCLE SLIPS/ DELETE SOME OBS.
CC               - COMPUTE TERMS "O-C"
CC               - COMPUTE FIRST DESIGN MATRIX
CC               - COPY NECESSARY INFO OF OBS. EQNS. ON FILE
CC
CC PARAMETERS :
CC         IN :  NFRCHK : FREQUENCIES TO BE CHECKED           I*4
CC               NFRFIL : NUMBER OF FREQUENCIES PER FILE      I*4
CC               LTRIP  : FREQUENCY TO BE USED FOR            I*4
CC                        TRIPLE DIFFERENCE SOLUTION
CC               IBLOCK : BLOCK NUMBER                        I*4
CC               NEPO   : NOMINAL NUMBER OF EPOCHS IN BLOCK   I*4
CC               IUSFLG : =1: USE FLAGS IN OBS-FILE           I*4
CC                        =0: IGNORE THEM
CC               MRK1O2 : FLAG TO MARK UNPAIRED L1/L2 OBSERVA- I*4
CC                        TIONS
CC                         =0 : NO MARKING DONE
CC                         =1 : L1 WITHOUT L2, OR L2 WITHOUT L1
CC                              OBSERVATIONS ARE MARKED
CC               IPRNT1 : PRINT LEVEL FOR OBS. SCREENING       I*4
CC                        =0: NO MESSAGES PRINTED
CC                        =1: SUMMARY MESSAGES PRINTED
CC                        =2: ALL MESSAGES PRINTED
CC               MAXZEN : MAXIMUM SATELLITE ZENITH DISTANCE   I*4
CC                        (DEGREE)
CC               SECIPL : MAX INTERVAL FOR CLK INTERPOLATION  R*8
CC               TIMREF : FILE REFERENCE TIME                 R*8
CC               IDELTT : TABULAR INTERVAL IN SECONDS         I*4
CC               STNAME(I),I=1,2: STATION NAMES               CH*16
CC               XWGS(K,I),K=1,3, I=1,2: WGS COORDINATES      R*8
CC               XSTELL(K,I),K=1,2,3,I=1,2: ELLIPSOIDAL COORD.R*8
CC               RECTYP : RECEIVER TYPES                       CH*20(2)
CC               ANTTYP : ANTENNA  TYPES                       CH*20(2)
CC               IANTEN : RECEIVER ANTENNA NUMBERS             I*4(2)
CC               CSESS(I),I=1,2: SESSION IDENTIFICATIONS      CH*4
CC               IFRMAT : FORMAT NUMBER OF OBSERVATION FILE   I*4
CC               NSLIP  : NUMBER OF SLIPS                     I*4
CC               LSTSLP(K,I),K=1,..,6, I=1,2,..,NSLIP:        I*4
CC                        SLIP DESCRIPTION
CC               SLPLST(I),I=1,2,..,NSLIP: SLIPS              R*8
CC               NDEL   : NUMBER OF DELETIONS                 I*4
CC               LSTDEL(K,I),K=1,..,5, I=1,2,..,NDEL          I*4
CC                        DEFINITION OF MARK REQUEST NUMBER I
CC                        (1,I): SV-NUMBER
CC                        (2,I): FIRST EPOCH OF MARKED AREA I
CC                        (3,I): LAST  EPOCH OF MARKED AREA I
CC                        (4,I): FREQUENCY (1=L1, 2=L2)
CC                        (5,I): MARKED BY
CC                               =1: SINGLE FREQ. REJECTION
CC                               =2: DUAL   FREQ. REJECTION
CC                               =3: UNPAIRED L1/L2 OBSERVATIONS
CC                               =4: USER
CC                               =5: SMALL ELEVATION
CC                               =6: SMALL PIECES
CC                               =7: BAD OBSERVED-COMPUTED
CC                               ALL AREAS MARKED OR CHANGED IN THE
CC                               LATEST RUN HAVE A NEGATIVE SIGN
CC               NSATEL : NUMBER OF SATELLITES (IN HEADER)    I*4
CC               NUMSAT(I),I=1,..,NSATEL: SATELLITE NUMBERS   I*4
CC               IIONO  : IONOSPHERE INDEX (1=APPLY IONOSP.)  I*4
CC               ISATCO : ISATCO=0: NO SATELLITE CLOCK CORR.  I*4
CC                        ISATCO=1: SAT. CLOCK CORRECTIONS
CC                                  TO BE APPLIED
CC               KINSTA : KINEMATIC COORDINATES ESTIMATION    I*4
CC               IPRNT2 : PRINT LEVEL FOR CYCLE SLIP DETECT.  I*4
CC               NDIFF  : NUMBER OF DIFFERENCES (ZD=0, SD=1)  I*4
CC               MXZLEO : LEO MAX. ZENITH ANGLE (DEGREE)      I*4
CC               ITROPO  : MODEL INDEX                        I*4
CC                     = 0: NO CORRECTION
CC                     = 1: SAASTAMOINEN
CC                     = 2: HOPFIELD
CC                     = 3: ESSEN AND FROOME
CC                     = 4: MARINI MURRAY (SLR)
CC                     = 5: SAASTAMOINEN WITH NIELL DRY MAPPING
CC                     = 6: GMF
CC                     = 7: VMF
CC                     =11: SAASTAMOINEN DRY DELAY ONLY
CC                     =12: HOPFIELD DRY DELAY ONLY
CC                     =15: SAASTAMOINEN DRY WITH NIELL DRY MAPPING
CC                     =16: GMF DRY DELAY ONLY
CC                     =17: VMF DRY DELAY ONLY
CC               IEXTRA  : METEO DATA SOURCE INDICATOR        I*4
CC                     =0: USE REAL DATA FROM EXTERNAL FILES
CC                     =1: USE VALUES FROM ATMOSPHERIC MODEL
CC                     =2: USE ESTIMATED VALUES IN BERNESE
CC                         TRP-FORMAT (FROM GPSEST OR ADDNEQ)
CC                         IN PRINPT IEXTRA SET TO 3 IN THIS CASE
CC                         IF (IEXTRA==2) IEXTRA=3
CC                     =3: Same as 2, but calling GETTRP2
CC                         (special version for piecewise linear
CC                          interpolation of troposphere, for CLKEST)
CC                         IN SR PRINPT IEXTRA SET TO 3 IN THIS CASE
CC                         IF (IEXTRA==2) IEXTRA=3
CC               MEATYP : MEASUREMENT TYPE                     I*4
CC                        =1: PHASE OBSERVATIONS
CC                        =2: CODE OBSERVATIONS
CC                        =3: RANGE OBSERVATIONS
CC               POSECC(I,K),I=1,2,3,K=1,2                    R*8
CC                        POSITIONING ECCENTRICITIES
CC                        I: COORDINATE
CC                        K: STATION
CC               AELL,BELL: SEMI-MAJOR AND -MINOR AXIS OF     R*8
CC                        ELIPSOID
CC               DXELL(I),I=1,2,3: SHIFT TO WGS-84            R*8
CC               DRELL(I),I=1,2,3: ROTATIONS TO WGS-84        R*8
CC               SCELL  : SCALE TO WGS-84                     R*8
CC        OUT :  NFRAUX : NUMBER OF FREQUENCIES ON RESULTING  I*4
CC                        AUXILIARY FILE
CC               FRQAUX(I),I=1,2,..,NFRAUX: FREQUENCIES ON    I*4
CC                        AUX. FILE
CC               NEPEFF : ACTUAL NUMBER OF EPOCHS IN BLOCK    I*4
CC               NSATBL : NUMBER OF SATELLITES IN BLOCK       I*4
CC               SVNBL(I),I=1,2,..,NSATBL: CORRESP. SVNS      I*4
CC               NOBSAT(ISAT),ISAT=1,..,NSATBL                I*4
CC               OBTIME(IEPO): OBSERVATION TIME               R*8
CC               AOBS(K,IEPO,ISAT): FIRST DESIGN MATRIX       R*8
CC               ABSVAL(IEPO,ISAT): TERMS "OBS-COMP" FOR      R*8
CC                        CARRIER LTRIP
CC               OBSFLG(IEPO,ISAT): OBSERVATION FLAGS         CH*1
CC               IRETRN : RETURN CODE                         I*4
CC                      =0: OK
CC                      =1: LAST RECORD ON INPUT FILE ENCOUNTERED,
CC                          NEPEFF<NEPO
CC               MXHOLE : MAXIMAL GAP IN OBSERVATIONS ALLOWED  I*4
CC                        TO BE CONSIDERED AS CONTINUOUS (SEC)
CC               MNCONT : MINIMAL TIME INTERVAL OF CONTINUOUS  I*4
CC                        OBSERVATIONS (SEC)
CC               IAMNEW(I),I=1,2,3: SETTING OF NEW AMBIGUITIES I*4
CC                        I=1 : USE CYCLE SLIP FLAG (0/1)
CC                        I=2 : IF PROBLEM IN SLIP-FIXING (0/1)
CC                        I=3 : AFTER GAP LARGER THAN (SEC)
CC                        I=4 : USE AMBIGUITIES IN FILE (0/1)
CC                        I=5 : MIN. TIME OF OBS. PER AMB. (SEC)
CC               NTONLY : TEST OBS. WITH CYCLE SLIP FLAG       I*4
CC                          ONLY (0/1)
CC               L5CLEA : L5 IS CLEAN (EXCEPT FLAGGED          I*4
CC                          EPOCHS)  (0/1)
CC               AMSFLG : INDICATER OF MAXAMS EXCEEDINGS      CH*1
CC   IN/OUT :    NNEWAM(ISATEL) : NUMBER OF AMBIGUITIES        I*4
CC               LSTAMB(I,ISATEL,IAMB)  LIST OF AMBIGUITIES    I*4
CC                        I=1 : THE FIRST EPOCH
CC                        I=2 : TYPE: 1 ... FILE
CC                                    2 ... CYCLE SLIP
CC                                    3 ... USER
CC                                    4 ... GAP
CC                                    5 ... PREPROCESSING PROBLEM
CC                                    6 ... CLOCK EVENT
CC                        I=3 : THE LAST EPOCH WITH OBSERVATIONS
CC               LASTCS(ISATEL) : LAST EPOCH WITH CYCLE        I*4
CC                                SLIP FLAG
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER, L.MERVART
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/05/16 09:16
CC
CC CHANGES    :  05-JUN-92 : ??: CHANGES FOR THE NEW MAUPRP VERSION
CC                               OPNFIL IMPLEMENTED; NEW CALLS: "GETORB",
CC                               "COOTRA", "XYZTIM" FOR J2000.0
CC               03-MAR-93 : ??: ADD ELEVATION DEPENDENT PHASE CENTER
CC                               CORRECTIONS. NEW PARAMETERS "RECTYP",
CC                               "ANTTYP","IANTEN"
CC               28-OCT-93 : ??: HANDLE SATELLITES NOT AVAILABLE IN
CC                               STANDARD-ORBIT FILE
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               08-APR-94 : MR: ADD AZIMUTH DEPENDENT CORRECTIONS
CC               29-JUL-94 : MR: ADD SATELLITE NUMBER TO CALL PRANGE
CC               10-AUG-94 : MR: CALL EXITRC
CC               24-APR-95 : MR: UPDATE DESCRIPTION OF LSTDEL
CC               21-AUG-95 : MR: ALWAYS MARK BOTH FREQ. FOR MRK1O2=1;
CC                               CHANGE "IEPO" TO "IEPOCH" AT LOOP 103
CC               06-DEC-95 : SS: CALL OF SR PRANGE MODIFIED
CC               06-DEC-95 : SS: CALL OF SR IONOSP MODIFIED (IORSYS)
CC               05-MAR-96 : TS: HANDLING OF "MAXAMS" EXCEEDINGS
CC               26-MAR-96 : MR: ADD "CSESS" AS PARAMETER AND TO CALL
CC                               GPHECC
CC               06-MAY-96 : TS: CORRECTED PRANGE CALL WITH "NDIFF"
CC                               AND NO MORE SAT. CLOCK STUFF
CC               08-APR-97 : SS: NIELL MAPPING, TROPOSPHERE GRADIENTS
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               09-OCT-97 : TS: NEW SOLID TIDES WITH XYZTIM
CC               25-NOV-97 : SS: "IRCCLK" IN CALL OF SR PRANGE
CC               26-JAN-98 : SS: STATION-SPECIFIC GIMS
CC               28-JUL-98 : HH: MODIFICATIONS FOR GLONASS
CC               05-AUG-99 : SS: PASS "ZENMAX" TO SR PRANGE
CC               23-MAR-00 : SS: PASS "SVN" AND "ZENMAX" TO SR IONOSP
CC               05-JUL-00 : TS: CORRECT PROBLEM WITH MISSING ORBITS
CC               01-MAY-01 : DS: SR:STAFLG - HANDLE STATION TYPES
CC               01-MAY-01 : DS: LEO KIN. COORDINATES AND ORBIT
CC               23-DEC-01 : DS: USE READKIN AND READVEL INSTEAD OF GETKIN
CC               19-JUN-02 : RD: CHANGE SIGN OF OBS. ONLY FOR ZD-OBS.
CC               05-JUL-02 : RD: KINEMATIC STATIONS ARE POSSIBLE
CC               13-SEP-02 : DS: KEEP LEO VELOCITY IN TRUE SYSTEM
CC               18-SEP-02 : DS: KINEMATICS FOR:LEO,AIRPLANE,
CC                               SHIP,GROUND,STATIC GROUND
CC               13-NOV-02 : RD: ADD "SAVE" FOR FILE NAMES
CC               14-NOV-02 : RD: USE ALL FLAGS IN .KIN FILES FOR STATIONS
CC               04-DEC-02 : RS: CALL OF SR PRANGE MODIFIED
CC               06-DEC-02 : RD: ADD THIRD PARAMETER FOR LSTAMB
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               23-APR-03 : RD: CLOSE ONLY OPENED FILES
CC               28-MAY-03 : RD: NEW CALL OF SR GPHECC
CC               20-JUN-03 : RD: MSG ABOUT COORD FILE ONLY FOR KIN. STATIONS
CC               23-JUN-03 : HB: INTERFACE FR SR STAFLG
CC               11-AUG-03 : RS: ADD CALL OF GTSATA, APPLY SATELLITE
CC                               ANTENNA PHASE CENTER VARIATIONS, CHANGE
CC                               CALL OF LEOSKY, RECTYP=' ' IN 4 CALLS OF
CC                               GPHECC
CC               08-SEP-03 : HU: ANTNAM, RECNAM CHR16 -> CHR20
CC               19-DEC-03 : PS: MARK SMALL ELEVATIONS BEFORE GPHECC CALL
CC               29-DEC-03 : PS: AVOID GPHECC CALL FOR MARKED OBSERVATIONS
CC               08-APR-04 : RS: ADD AZISOK*
CC               10-MAY-04 : DS: PASS SVN NUMBER AND IORSYS TO LEOSKY
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-AUG-05 : HB: USE NEW SR TIMST2 (MODULE)
CC               09-NOV-05 : AG: SENNUM FOR GTSATA & GPHECC CALLS ADDED
CC               29-JUN-06 : HB: ADD PARAMETER SATCLK TO SR PRANGE
CC               17-AUG-06 : HU: USE FOR S_LEOSKY
CC               07-DEC-06 : RD: ENABLE MRK1O2 FOR ALL DUAL-FREQ. FILES
CC               12-JUN-07 : AG: USE D_PHAECC INSTEAD OF GPHECC AND GTSATA
CC               01-NOV-07 : HB: ADD PARAMETER SECIPL
CC               05-DEC-07 : HB: ADD PARAMETER NADMAX, IRCPCV
CC               17-JAN-08 : AJ: VEL/ACC TO XSTSAT ADDED
CC               30-JUN-08 : RD: VMF ADDED
CC               01-OCT-08 : HB: ADD PARAMETER ZENION TO SR PRANGE
CC               29-MAY-09 : RD: NEW CALL OF PRANGE
CC               04-JAN-10 : SL: HOI ADDED TO IONOSP CALLS
CC               25-OCT-10 : CR: NEW CALL OF PRANGE
CC               19-NOV-10 : PS  MISSING XSTELL ADDED TO IONOSP CALLS
CC               19-NOV-10 : SL: USE M_BERN WITH ONLY
CC               11-NOV-11 : MM: TAKE SATELLITE PCOS FROM PCV FILE
CC               31-JAN-12 : RD: CORRECT MESSAGE ABOUT MISSING CLOCKS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnErr, lfnPrt, lfnRp1, lfn002, lfn001,
     1                    fileNameLength
      USE d_stacrx, ONLY: MTypeSPACE
      USE d_satfil, ONLY: typeMWTR
      USE m_maxdim, ONLY: MAXSAT
      USE d_const,  ONLY: C, PI
      USE d_phaecc, ONLY: sta_off,sat_off

      USE s_rdobsi
      USE s_opnfil
      USE s_pdcor
      USE f_tstflg
      USE s_mrkobs
      USE s_cootra
      USE s_prange
      USE s_leoante
      USE s_updamb
      USE s_wtobsi
      USE s_lincom
      USE s_inquire
      USE s_clrflg
      USE s_getorb
      USE s_truearth
      USE s_opnerr
      USE s_maxtst
      USE s_staflg
      USE s_timst2
      USE s_setflg
      USE s_readvel
      USE s_cycmrk
      USE s_xyztim
      USE s_readkin
      USE s_ionosp
      USE s_exitrc
      USE s_gtleoco
      USE s_ellecc
      USE s_leosky
      USE s_gtflna
      USE s_xyzell
      USE s_gnssatti
C
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I      , I1SAT  , IAMB   , IBLOCK , ICARR  , ICRARC ,
     1          IDELTT , IEPO   , IEPOCH , IEXTRA , IFLAG  , IFRMAT ,
     2          IFRQ   , IFRQ1  , IFRQ2  , IGAR   , II     , IIONO  ,
     3          IORSYS , IOSTAT , IPRNT1 , IPRNT2 , IRC    , IRCPCV ,
     4          IRCCLK , IRCKIN , IRCKIN2, IRCODE , IRCSTD , IRCVEL ,
     5          IRCVEL2, IRET   , IRETRN , ISATCO , ISAT   ,
     6          ISATEL , ISATEP , ISATR  , ISTA   , ISVN   , ITITLE ,
     7          ITROPO , IUSFLG , JUMP   , K      , KCARR  , KINSTA ,
     8          L      , L5CLEA , LFNOBS , LL     , LTRIP  , MAXEPO ,
     9          MAXZEN , MEATYP , MNCONT , MRK1O2 , MXCDEL , MXCEPO ,
     1          MXCSAT , MXHOLE , MXZLEO , NDEL   , NDIFF  , NEPEFF ,
     2          NEPO   , NFRAUX , NFRCHK , NFRFIL , NSATBL , NSATEL ,
     3          NSATEP , NSLIP  , NTONLY
C
      REAL*8    AELL   , BELL   , DDD    , OBSCOM , OBSTIM , SCELL  ,
     1          SECIPL , SZ     , TIMREF , TOSC   , UT1GPS , XPOL   ,
     2          YPOL
C
CCC       IMPLICIT     REAL*8 (A-H,O-Z)
CCC       IMPLICIT     INTEGER*4 (I-N)
C
      PARAMETER    (MAXEPO=6000)
C
      INTEGER*4    SVNBL(*),L12(2),FRQAUX(*),IANTEN(2)
      INTEGER*4    INDSAT(MAXSAT),SVNEP(MAXSAT),NOBSAT(*)
      INTEGER*4    LSTSLP(6,*),LSTDEL(5,*),IAMNEW(*)
      INTEGER*4    NUMSAT(*)
      INTEGER*4    GARBAG(MAXSAT,2,3),LSTAMB(3,MXCSAT,*)
      INTEGER*4    NNEWAM(MXCSAT),LASTCS(MXCSAT)
C
      REAL*8       XWGS(3,2),XSTELL(3,2),OBTIME(*)
      REAL*8       XSTEPO(9,2),XSTSAT2(9),XSTSAT(9,2)
      REAL*8       AOBS(3,MXCEPO,*),ABSVAL(MXCEPO,*)
      REAL*8       OBSEPO(MAXSAT,2),XSAT(9),ELE(7),TOPPOS(3,2)
      REAL*8       DIST(2),DION(2),ZEN(2),ZENRAD(2),AZIRAD(2),ZENION(2)
      REAL*8       ABSEPO(3),SLPLST(*)
      REAL*8       DELTAT(2),DT12(2)
      REAL*8       ZENRAD2(2),AZIRAD2(2),TOPPOS2(3,2),ZEN2(2)
      REAL*8       ANTECC(3),TRUOFF(3),XKIN(3)
      REAL*8       XVEL(3),XKV(6),XTMP(9)
      REAL*8       MAXZ(2),ZENMAX(2),NADMAX
      REAL*8       POSECC(3,*),DXELL(*),DRELL(*)
      REAL*8       XELEPO(3),EXC(3)
      REAL*8       NADRAD(2),AZISATRAD(2),NADRAD2(2),AZISATRAD2(2)
      REAL*8       AZISOKRAD(2),AZISOKRAD2(2)
      REAL*8       SATCLK
      REAL*8       DUMMY
      REAL*8       HOI(3)
      REAL*8       OFFSET(3),EX(3),EY(3),EZ(3)
C
      CHARACTER(LEN=fileNameLength),SAVE   :: FILORB,FILKIN,FILVEL
      CHARACTER*1  OBSFLG(MXCEPO,*),OBSFLE(MAXSAT,2),FLGCOM,FLGAUX(3)
      CHARACTER*1  EPOFLG,AMSFLG
      CHARACTER*4  CSESS(2)
      CHARACTER*6  MXNEPO,MXNSAT,MXNDEL
      CHARACTER*16 STNAME(2)
      CHARACTER*20 RECTYP(2),ANTTYP(2)
      CHARACTER*20 MARTYP
      CHARACTER*32 FILNAM,FILCOP
      CHARACTER*80 LINE
C
      LOGICAL      OPENED,PRTKINMSG
C
C COMMON BLOCKS
C -------------
      COMMON/MCMEPO/MXCEPO,MXNEPO
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMDEL/MXCDEL,MXNDEL
C
C INITIALIZATION FOR FIRST BLOCK
C ------------------------------
      IF(IBLOCK.EQ.1) THEN
C
C CHECK MAXIMUM NUMBER OF SATELLITES
C ----------------------------------
        CALL MAXTST(0,'DSRDBL',MXNSAT,MAXSAT,MXCSAT,IRC)
        IF(IRC.NE.0) CALL EXITRC(2)
C
C GET LEO ORBIT INFORMATION
C -------------------------
        CALL GTFLNA(0,'LEOSTD ',FILORB,IRCSTD)
        CALL GTFLNA(0,'KININP ',FILKIN,IRCKIN)
        CALL GTFLNA(0,'KINVEL ',FILVEL,IRCVEL)
C
C DEFINE FREQUENCIES FOR SR RDOBSI
C --------------------------------
        L12(1)=1
        L12(2)=2
C
C DEFINE FREQUENCIES FOR AUX-FILE
C -------------------------------
        MRK1O2=1
        IF(NFRCHK.LT.3) THEN
          NFRAUX=1
          FRQAUX(1)=NFRCHK
          MRK1O2=0
        ELSE IF(NFRCHK.EQ.3) THEN
          NFRAUX=3
          FRQAUX(1)=1
          FRQAUX(2)=2
          FRQAUX(3)=5
        ELSE
          NFRAUX=2
          FRQAUX(1)=1
          FRQAUX(2)=2
        ENDIF
C
C OPEN AUXILIARY FILE FOR STORAGE OF OBS EQNS
C -------------------------------------------
        CALL inquire(UNIT=LFN002,OPENED=OPENED)
        IF(OPENED) CLOSE(UNIT=LFN002)
        CALL GTFLNA(1,'AUXFIL ',FILNAM,IRC)
        CALL OPNFIL(LFN002,FILNAM,'UNKNOWN','UNFORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN002,IOSTAT,FILNAM,'DSRDBL')
C
C OPEN FILE FOR COPY OF OBSERVATION FILE
C --------------------------------------
        CALL inquire(UNIT=LFNRP1,OPENED=OPENED)
        IF(OPENED) CLOSE(UNIT=LFNRP1)
        CALL GTFLNA(1,'OBCOPY ',FILCOP,IRC)
        CALL OPNFIL(LFNRP1,FILCOP,'UNKNOWN','UNFORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRP1,IOSTAT,FILCOP,'DSRDBL')
C
C DEFINE CHANNEL NUMBER FOR INPUT FILE
C -------------------------------------
        LFNOBS=LFN001+2
C
C INITIALIZE SAT.CLOCK ARRAY INDEX
C --------------------------------
        ISATEL=1
C
C INITIALIZE ARRAY FOR MARKING OF SMALL PIECES
C AND THE VARIABLE "LASTCS"
C --------------------------------------------
        DO 15 ISVN=1,MAXSAT
          LASTCS(ISVN) = 1
          DO 10 IFRQ=1,NFRFIL
            GARBAG(ISVN,IFRQ,1) = 0
            GARBAG(ISVN,IFRQ,2) = 0
            GARBAG(ISVN,IFRQ,3) = 0
10        CONTINUE
15      CONTINUE
C
      ENDIF
C
C INITIALIZATION FOR IBLOCK
C -------------------------
      NSATBL=0
      ITITLE=1
C
C PROCESS NEPO EPOCHS
C -------------------
      IRETRN=0
      DO 200 IEPO=1,NEPO
C
C READ OBSERVATIONS FOR ONE EPOCH
C -------------------------------
        CALL RDOBSI(LFNOBS,IFRMAT,NFRFIL,L12,OBSTIM,DELTAT,EPOFLG,
     1              NSATEP,SVNEP,OBSFLE,OBSEPO,IRET)
        IF(IRET.NE.0) THEN
          IRETRN=1
          REWIND LFNRP1
          REWIND LFN002
          GOTO 201
        ENDIF
C
C CLEAR OBS-FILE FLAGS IF NECESSARY
C AND CHANGE SIGN OF PHASE ZERO DIFFERENCES
C (SEE MAUPRP)
C ------------------------------------------
        DO ISATEP=1,NSATEP
          DO L=1,NFRFIL
            IF(IUSFLG.EQ.0) CALL CLRFLG(OBSFLE(ISATEP,L),0)
            IF (NDIFF.EQ.0 .AND. MEATYP.EQ.1) THEN
              OBSEPO(ISATEP,L)=-OBSEPO(ISATEP,L)
            END IF
          END DO
        END DO
C
C COPY OBS-FILE
C -------------
        CALL WTOBSI(LFNRP1,IFRMAT,NFRFIL,OBSTIM,DELTAT,EPOFLG,
     1              NSATEP,SVNEP,OBSFLE,OBSEPO)
C
C OBSERVATION EPOCH, OBSERV.TIME OF FIRST RECEIVER, TIME CORRECTIONS
C ------------------------------------------------------------------
        IEPOCH=IDNINT((OBSTIM-TIMREF)*86400.D0/IDELTT)+1
        IF (NDIFF.EQ.1) THEN
          OBTIME(IEPO)=OBSTIM+DELTAT(1)/86400.D0
          DT12(1)=0.D0
          DT12(2)=DELTAT(2)-DELTAT(1)
        ELSE
C
C CHECK FOR MILLI SEC. JUMPS
C --------------------------
          IF (DELTAT(2).EQ.0D0.AND.DABS(DELTAT(1)).GE.1D-3)
     1      DELTAT(2)=-DELTAT(1)
C
C CORRECT OBSERVATIONS FOR DELTAT(2)
C ----------------------------------
          DO ISAT=1,NSATEP
            DO IFRQ=1,NFRFIL
              IF (OBSEPO(ISAT,IFRQ).EQ.0D0) CYCLE
              OBSEPO(ISAT,IFRQ)=OBSEPO(ISAT,IFRQ)+DELTAT(2)*C
            ENDDO
          ENDDO
C
          OBTIME(IEPO)=OBSTIM+(DELTAT(1)+DELTAT(2))/86400d0
          DT12(1:2)=0D0
        END IF
C
C APPLY CYCLE SLIPS, DELETE SOME OBSERVATIONS
C -------------------------------------------
        CALL CYCMRK(NSATEP,NFRFIL,SVNEP,IEPOCH,NSLIP,LSTSLP,SLPLST,
     1              NDEL,LSTDEL,OBSEPO,OBSFLE)
C
C MARK UNPAIRED DUAL FREQUENCY OBSERVATIONS
C -----------------------------------------
        IF(MRK1O2.EQ.1.AND.NFRFIL.EQ.2) THEN
          DO 35 ISATEP=1,NSATEP
            DO 30 L=1,NFRFIL
              IF(L.EQ.1) THEN
                LL=2
              ELSE
                LL=1
              ENDIF
              IF((.NOT.TSTFLG(OBSFLE(ISATEP,L),0).AND.
     1            OBSEPO(ISATEP,L).NE.0.D0).AND.
     2           (TSTFLG(OBSFLE(ISATEP,LL),0).OR.
     3            OBSEPO(ISATEP,LL).EQ.0.D0)) THEN
C
C MARK UNPAIRED OBSERVATION
                CALL SETFLG(OBSFLE(ISATEP,L),0)
C
C UPDATE MARKED AREAS WITH MARKED UNPAIRED OBSERVATIONS
C
                CALL MRKOBS(3,SVNEP(ISATEP),IEPOCH,L,NSATEL,NUMSAT,
     1                      NDEL,LSTDEL)
C
              ENDIF
30          CONTINUE
35        CONTINUE
        ENDIF
C
C ONLY ONE MESSAGE PER EPOCH FOR MISSING KINEMATIC STATIONS
C ---------------------------------------------------------
       PRTKINMSG=.TRUE.
C
C ARRANGE SATELLITES IN BLOCK SPECIFIC ARRAY
C ------------------------------------------
        DO 50 ISATEP=1,NSATEP
          DO 40 ISAT=1,NSATBL
            IF(SVNBL(ISAT).EQ.SVNEP(ISATEP)) THEN
              INDSAT(ISATEP)=ISAT
              GOTO 50
            ENDIF
40        CONTINUE
          NSATBL=NSATBL+1
          SVNBL(NSATBL)=SVNEP(ISATEP)
          NOBSAT(NSATBL)=0
          INDSAT(ISATEP)=NSATBL
50      CONTINUE
C
C INITIALIZE OBS-COMP AND FLAG FOR CURRENT EPOCH
C ----------------------------------------------
        DO 60 ISAT=1,MAXSAT
          ABSVAL(IEPO,ISAT)=0.D0
          CALL CLRFLG(OBSFLG(IEPO,ISAT),0)
          CALL CLRFLG(OBSFLG(IEPO,ISAT),1)
60      CONTINUE
C
C WRITE EXTERNAL EPOCH NUMBER, NUMBER OF SATELLITES/EPOCH,
C SATELLITE NUMBERS
C --------------------------------------------------------
        WRITE(LFN002) IEPOCH,NSATEP,(SVNEP(I),I=1,NSATEP),
     1                (DELTAT(I),I=1,2)
C
C PROCESS ALL SATELLITES OF CURRENT EPOCH
C ---------------------------------------
        DO 100 ISATEP=1,NSATEP
          ISAT=INDSAT(ISATEP)
C
C FIND SATELLITE INDEX FOR SAT.CLOCK ARRAYS (IF SAT.CLOCK TO BE APPLIED)
C ----------------------------------------------------------------------
          DO 65 ISATEL=1,NSATEL
            IF(NUMSAT(ISATEL).EQ.SVNEP(ISATEP)) GOTO 68
65        CONTINUE
68        CONTINUE
C
C TEST CYCLE SLIP FLAGS
C ---------------------
          IF (IAMNEW(1) .EQ. 1) THEN
            DO 69 L=1,NFRFIL
              IF (TSTFLG(OBSFLE(ISATEP,L),1))
     1          LASTCS(ISATEL) = IEPOCH
69          CONTINUE
          END IF
C
C "OBSERVED-COMPUTED"
C -------------------
          CALL GETORB(SVNEP(ISATEP),1,2,2,OBTIME(IEPO),ICRARC,
     1                IORSYS,XSAT,TOSC,ELE,IRC)
          IF (IRC.NE.0) THEN
            DO 72 L=1,NFRFIL
              IF ((.NOT.TSTFLG(OBSFLE(ISATEP,L),0)).AND.
     1            (OBSEPO(ISATEP,L) .NE. 0.D0)) THEN
                CALL SETFLG(OBSFLE(ISATEP,L),0)
                CALL MRKOBS(4,SVNEP(ISATEP),IEPOCH,L,NSATEL,NUMSAT,
     1                      NDEL,LSTDEL)
              ENDIF
72          CONTINUE
            DO K=1,3
              AOBS(K,IEPO,ISAT)=0.0D0
            ENDDO
            DO ICARR=1,NFRAUX
              ABSEPO(ICARR)=0.0D0
              CALL SETFLG(FLGAUX(ICARR),0)
            ENDDO
            GOTO 99
          ENDIF
C
C APPLY SATELLITE ANTENNA PHASE CENTER OFFSET
C -------------------------------------------
          CALL SAT_OFF(SVNEP(ISATEP),OBTIME(IEPO),
     1                 typeMWTR,LTRIP,OFFSET)
          CALL GNSSATTI(IORSYS,SVNEP(ISATEP),OBTIME(IEPO),0,XSAT,
     1                  EX_SAT=EX,EY_SAT=EY,EZ_SAT=EZ)
          DO K = 1,3
            XSAT(K)=XSAT(K)+EX(K)*OFFSET(1)
     1                     +EY(K)*OFFSET(2)
     2                     +EZ(K)*OFFSET(3)
          ENDDO
C
C TRANSFORM SATELLITE POSITION TO EARTH-FIXED SYSTEM
          CALL COOTRA(IORSYS,2,OBTIME(IEPO),XSAT,SZ,XPOL,YPOL,UT1GPS)
C
          DO 70 ISTA=1,NDIFF+1
C
C HANDLE STATION TYPES
C --------------------
            CALL STAFLG(STNAME(ISTA),OBTIME(IEPO),IFLAG,MARTYP)
C
C HANDLE KINEMATIC COORDINATES FOR:
C =================================
C
C 1) ALL EXCEPT SPACEBORNE
C -------------------------
            IF (.NOT. (MARTYP.EQ.MTypeSPACE)) THEN
              XSTEPO(1:3,ISTA)=XWGS(1:3,ISTA)
C
C SOLID EARTH TIDE CORRECTION
              IF (MARTYP.EQ.' ') THEN
                CALL XYZTIM(IORSYS,OBTIME(IEPO),XSTEPO(1,ISTA),
     1                      STNAME(ISTA),XSTEPO(1,ISTA))
              END IF
C
C APPLY SENSOR OFFSET AND PHASE CENTER OFFSET
              CALL STA_OFF(ANTTYP(ISTA),IANTEN(ISTA),STNAME(ISTA),
     1                    SVNEP(ISATEP),LTRIP,CSESS(1),ANTECC)
              CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,
     1                    XSTEPO(1:3,ISTA),XELEPO)
              CALL ELLECC(XELEPO,ANTECC,EXC)
              XSTSAT(1:3,ISTA)=XSTEPO(1:3,ISTA)+EXC(1:3)
C
C KINEMATIC COORDINATES
C ---------------------
              IF (IRCKIN.EQ.0) THEN
                CALL  READKIN(FILKIN,STNAME(ISTA),OBTIME(IEPO),
     1                        1-KINSTA,0,XKIN,IRCKIN2)
C
C APPLY SENSOR OFFSET AND PHASE CENTER OFFSET
C -------------------------------------------
                IF (IRCKIN2.EQ.0) THEN
                  CALL STA_OFF(ANTTYP(ISTA),IANTEN(ISTA),STNAME(ISTA),
     1                         SVNEP(ISATEP),LTRIP,CSESS(1),ANTECC)
                  ANTECC(1:3)=POSECC(1:3,ISTA)+ANTECC(1:3)
                  CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,
     1                        XSTEPO(1:3,ISTA),XELEPO)
                  CALL ELLECC(XELEPO,ANTECC,EXC)
                  XSTSAT(1:3,ISTA)=XKIN(1:3)+EXC(1:3)
C
C STATION NOT IN KININP, BUT NOT STATIC ASSUMED
C ---------------------------------------------
                ELSE IF (IRCKIN2.EQ.2.AND.MARTYP.NE.' ') THEN
                  IF (PRTKINMSG) THEN
                    WRITE(LFNERR,1021)
     1              STNAME(ISTA)(1:5),TRIM(MARTYP)
1021                FORMAT(' ### SR DSRDBL: ',
     1              'A PRIORI POSITIONS TAKEN FROM COORD. FILE, ',
     2              'STATION/MARTYP: ',A6,A20)
                    PRTKINMSG=.FALSE.
                  ENDIF
C
C NO POSITION "K" FOUND, BUT COORDINATE WILL NOT BE IMPROVED
C SKIP ALL OBSERVATIONS FOR THIS EPOCH FROM THIS STATION
C ----------------------------------------------------------
                ELSE IF (IRCKIN2.EQ.1.AND.KINSTA.EQ.0) THEN
                  IF (PRTKINMSG) THEN
                    WRITE(LFNERR,
     .              '(/,A,/,16X,A,/,16X,2A,/,16X,A,F15.7,/)')
     1              ' ### SR DSRDBL: ' //
     2              'No kinematic apriori coordinates found.',
     3              'The observations from file have to be skipped' //
     4              ' for this epoch.',
     5              'Station name: ',TRIM(STNAME(ISTA)),
     6              'Epoch:        ',OBTIME(IEPO)
                    PRTKINMSG=.FALSE.
                  ENDIF
C
C                 Flag corresponding observation as "O-C"
                  DO L=1,NFRFIL
                    IF ((.NOT.TSTFLG(OBSFLE(ISATEP,L),0)).AND.
     1                  (OBSEPO(ISATEP,L) .NE. 0.D0)) THEN
                      CALL SETFLG(OBSFLE(ISATEP,L),0)
                      CALL MRKOBS(4,SVNEP(ISATEP),IEPOCH,L,NSATEL,
     1                    NUMSAT,NDEL,LSTDEL)
                    END IF
                  END DO
                  AOBS(1:3,IEPO,ISAT)=0D0
                  ABSEPO(1:NFRAUX)   =0D0
                  DO ICARR=1,NFRAUX
                    CALL SETFLG(FLGAUX(ICARR),0)
                  ENDDO
                  GOTO 99
C
C USE EXISTING STATION COORDINATES FOR AN EPOCH
C ---------------------------------------------
                ELSE IF (IRCKIN2.EQ.1) THEN
                  IF (PRTKINMSG) THEN
                    WRITE(LFNERR,1022)
     1              OBTIME(IEPO),STNAME(ISTA)(1:5),TRIM(MARTYP)
1022                FORMAT(' ### SR DSRDBL: ',
     1              'A PRIORI POSITION TAKEN FROM COORD. FILE, ',
     2              'EPOCH/STATION/MARTYP: ',F15.7,A6,A20)
                    PRTKINMSG=.FALSE.
                  ENDIF
                END IF
              END IF
C
C 3) LEO
C -------
            ELSE
C
C 3.1)  FROM THE FILE "KININP"
C ----------------------------
              IRCKIN2=1
              IRCVEL2=1
              IF (IRCKIN==0) THEN
                CALL  READKIN(FILKIN,STNAME(ISTA),
     1                        OBTIME(IEPO),1,0,XKIN,IRCKIN2)
              END IF
              IF (IRCVEL==0) THEN
                CALL READVEL(FILVEL,STNAME(ISTA),
     1                       OBTIME(IEPO),1,0,XVEL,IRCVEL2)
              END IF
C
C TRANSFORMATION INTO TRUE SYSTEM OF EPOCH
C ----------------------------------------
              IF (IRCKIN2==0) THEN
                IF (IRCVEL2.NE.0) THEN
                  CALL TRUEARTH(XKIN,XSTEPO(1:3,ISTA),1,0,
     1                          SZ,XPOL,YPOL)
                  XSTEPO(4:9,ISTA)=0.D0
                ELSE
                  XKV(1:3)=XKIN(1:3)
                  XKV(4:6)=XVEL(1:3)
                  CALL TRUEARTH(XKV,XSTEPO(1:6,ISTA),1,1,
     1                          SZ,XPOL,YPOL)
                  XSTEPO(7:9,ISTA)=0.D0
                  GOTO 75
                END IF
              ELSE
                IF (IRCVEL2.EQ.0 .AND. IRCSTD.EQ.0 ) THEN
                    CALL GTLEOCO(STNAME(ISTA),OBTIME(IEPO),2,0,
     1                           XKV,SZ,XPOL,YPOL,IRCODE)
                  IF (IRCODE.EQ.0) THEN
                    XKV(4:6)=XVEL(1:3)
                    CALL TRUEARTH(XKV,XSTEPO(1:6,ISTA),1,1,
     1                            SZ,XPOL,YPOL)
                    XSTEPO(7:9,ISTA)=0.D0
                  END IF
                END IF
              END IF
C
C 3.2) GET LEO COORDINATES FROM THE STANDARD ORBIT
C ------------------------------------------------
              IF (IRCSTD==0) THEN
                CALL GTLEOCO(STNAME(ISTA),OBTIME(IEPO),1,2,
     1                       XTMP(1:9),SZ,XPOL,YPOL,IRCODE)
                IF (IRCODE==0) THEN
                  IF (IRCKIN2.NE.0) XSTEPO(1:3,ISTA)=XTMP(1:3)
                  IF (IRCVEL2.NE.0) THEN
                    XSTEPO(4:6,ISTA)=XTMP(4:6)
                    XSTSAT(4:6,ISTA)=XTMP(4:6)
                  END IF
                  XSTEPO(7:9,ISTA)=XTMP(7:9)
                  XSTSAT(7:9,ISTA)=XTMP(7:9)
                  GOTO 75
                END IF
              END IF
C
C IF NO LEO COORDINATES AVAILABLE ISSUE AN ERROR
C ----------------------------------------------
              WRITE(LFNERR,74) STNAME(ISTA),OBTIME(IEPO)
74            FORMAT(/,' ### SR DSRDBL: LEO POS./VEL. DATA NOT FOUND',
     1                  /,16X,'SENSOR      : ',A16,
     2                  /,16X,'EPOCH (MJD) : ',F16.6,/)
              CALL EXITRC(2)
C
C APPLY SENSOR OFFSET AND PHASE CENTER OFFSET
C --------------------------------------------
75            CALL STA_OFF(ANTTYP(ISTA),IANTEN(ISTA),STNAME(ISTA),
     1                        SVNEP(ISATEP),LTRIP,CSESS(1),ANTECC)
              XSTSAT2(1:9)=XSTEPO(1:9,ISTA)
              CALL LEOANTE(STNAME(ISTA),OBTIME(IEPO),ANTECC,
     1                     0,XSTEPO(:,ISTA),TRUOFF)
              XSTSAT2(1:3)=XSTEPO(1:3,ISTA)+TRUOFF(:)
C
C TRANSFORMATION INTO EARTH-FIXED SYSTEM
C --------------------------------------
              CALL TRUEARTH(XSTSAT2,XSTSAT(:,ISTA),0,0,
     1                      SZ,XPOL,YPOL)
            END IF
C
C COMPUTE ZENMAX IN RADIANS AND SET MAXZ FOR LEO
C ----------------------------------------------
            NADMAX =PI/180.D0*17.D0
            IF (MARTYP.EQ.MTypeSPACE) THEN
              ZENMAX(ISTA)=PI/180.D0*MXZLEO
              MAXZ(ISTA)=MXZLEO
            ELSE
              ZENMAX(ISTA)=PI/180.D0*MAXZEN
              MAXZ(ISTA)=MAXZEN
            END IF
C
C SR PRANGE CALLED WITH MEATYP=1, ICARR=3 (NO IONOSPHERE!), NDIFF=1
            CALL PRANGE(ISTA,OBTIME(IEPO),DT12(ISTA),SECIPL,
     1                  STNAME(ISTA),XSTSAT(1,ISTA),SVNEP(ISATEP),XSAT,
     2                  SZ,XPOL,YPOL,ISATCO,ITROPO,IEXTRA,
     3                  XSTELL(1,ISTA),1,3,IORSYS,TOPPOS(1,ISTA),
     4                  ZENMAX(ISTA),NADMAX,DIST(ISTA),SATCLK,
     5                  ZENRAD(ISTA),AZIRAD(ISTA),ZENION(ISTA),NDIFF,
     6                  -1,IRCCLK,IRCPCV,NADRAD(ISTA),AZISATRAD(ISTA),
     7                  AZISOKRAD(ISTA),ANTTYP(ISTA),IANTEN(ISTA),
     8                  CSESS(1),0)
            ZEN(ISTA)=ZENRAD(ISTA)*180.D0/PI
C
C COMPUTE ZENITH DISTANCE AND AZIMUTH OF A SATELLITE AS SEEN FROM SENSOR SYSTEM
C -----------------------------------------------------------------------------
            IF (MARTYP.EQ.MTypeSPACE) THEN
              CALL LEOSKY(STNAME(ISTA),OBTIME(IEPO),0,0,
     1                    XSTSAT(:,ISTA),XSAT,SZ,
     2                    DT12(ISTA),XPOL,YPOL,1,TOPPOS2(1,ISTA),
     3                    DUMMY,ZENRAD2(ISTA),AZIRAD2(ISTA),
     4                    NADRAD2(ISTA),AZISATRAD2(ISTA),
     5                    SVNEP(ISATEP),IORSYS,AZISOKRAD2(ISTA))
              ZEN2(ISTA)=ZENRAD2(ISTA)*180.D0/PI
            ELSE
              TOPPOS2(:,ISTA)=TOPPOS(:,ISTA)
              ZENRAD2(ISTA)=ZENRAD(ISTA)
              AZIRAD2(ISTA)=AZIRAD(ISTA)
              NADRAD2(ISTA)=NADRAD(ISTA)
              AZISATRAD2(ISTA)=AZISATRAD(ISTA)
              AZISOKRAD2(ISTA)=AZISOKRAD(ISTA)
              ZEN2(ISTA)=ZEN(ISTA)
            END IF


C
C MARK OBSERVATIONS WITH A SMALL ELEVATION
C ----------------------------------------
          JUMP=0
          IF (IRCPCV.NE.0) THEN
C     1        (NDIFF.EQ.1 .AND. (ZEN2(2) .GT. MAXZ(2)))) THEN
            JUMP=1
            DO 80 L=1,NFRFIL
              IF ((.NOT.TSTFLG(OBSFLE(ISATEP,L),0)).AND.
     1            (OBSEPO(ISATEP,L) .NE. 0.D0)) THEN
                CALL SETFLG(OBSFLE(ISATEP,L),0)
C
C OBSERVATION TO BE MARKED NEXT TO MARKED AREA ?
                CALL MRKOBS(5,SVNEP(ISATEP),IEPOCH,L,NSATEL,NUMSAT,
     1                    NDEL,LSTDEL)
              END IF
80          CONTINUE
          ENDIF

C SKIP GPHECC FOR SMALL ELEVATIONS
          IF(JUMP .EQ. 1) GOTO 70
C
C IONOSPHERIC CORRECTION FOR TRIPLE DIFF. FREQUENCY
          IF(IIONO.EQ.1) THEN
            CALL IONOSP(XSTSAT(1,ISTA),XSAT,OBTIME(IEPO),SZ,
     1                  ZENION(ISTA),LTRIP,IORSYS,STNAME(ISTA),
     2                  SVNEP(ISATEP),ZENMAX(ISTA),DION(ISTA),
     3                  MEATYP,AZIRAD(ISTA),HOI,XSTELL(:,ISTA))
          ELSE
            DION(ISTA)=0.D0
          ENDIF
70      CONTINUE

C
C MARK OBSERVATIONS WITHOUT SATELLITE CLOCK
C -----------------------------------------
        IF (NDIFF.EQ.0 .AND. IRCCLK.NE.0) THEN

C
C GENERATE A MESSAGE LINE:
          LINE=' ### SR DSRDBL: GNSS CLOCK IS  MISSING: EPOCH, SAT: '
          WRITE(LINE(59:80),'(I6,I4,1X,A)')
     1            IEPOCH,SVNEP(ISATEP),'(' // STNAME(1)(1:4) // ')'
          IF (LEN_TRIM(STNAME(2)).GT.0) THEN
            WRITE(LINE(74:80),'(A)')  '-' // STNAME(2)(1:4) // ')'
          ENDIF
          WRITE(LFNERR,'(A)') LINE

          DO L=1,NFRFIL
            IF ((.NOT.TSTFLG(OBSFLE(ISATEP,L),0)).AND.
     1              (OBSEPO(ISATEP,L) .NE. 0.D0)) THEN
              CALL SETFLG(OBSFLE(ISATEP,L),0)
C
C OBSERVATION TO BE MARKED NEXT TO MARKED AREA ?
              CALL MRKOBS(8,SVNEP(ISATEP),IEPOCH,L,NSATEL,NUMSAT,
     1                    NDEL,LSTDEL)
            END IF
          END DO
        END IF
C
C MARK PIECES WITH SMALL NUMBER OF OBSERVATIONS,
C UPDATE LIST OF AMBIGUITIES ACCORDING TO
C GAPS AND CYCLE SLIP FLAGS
C ---------------------------------------------
        DO 85 L=1,NFRFIL
          IF ((OBSEPO(ISATEP,L) .NE. 0.D0) .AND.
     1          (.NOT. TSTFLG(OBSFLE(ISATEP,L),0))) THEN
            IF (GARBAG(ISATEL,L,1) .EQ. 0) THEN
              GARBAG(ISATEL,L,1) = IEPOCH
              GARBAG(ISATEL,L,2) = IEPOCH
            ELSE
C
              IF (((IEPOCH-GARBAG(ISATEL,L,2)) .GT. 1) .AND.
     1            (LASTCS(ISATEL) .GT. GARBAG(ISATEL,L,2))) THEN
                LASTCS(ISATEL) = IEPOCH
                CALL UPDAMB(ISATEL,GARBAG(ISATEL,L,2)+1,IEPOCH,2,
     1                        NNEWAM,LSTAMB,NSATEL,NUMSAT,IPRNT2,
     2                        AMSFLG,IRC)
              END IF
C
              IF ((IEPOCH-GARBAG(ISATEL,L,2))*IDELTT .GT.
     1                MAX0(MXHOLE,IDELTT)) THEN
                IF (((GARBAG(ISATEL,L,2)-GARBAG(ISATEL,L,1)+1)*IDELTT)
     1                 .LT. MNCONT) THEN
                  IF(MRK1O2.EQ.1.AND.NFRFIL.EQ.2) THEN
                    IFRQ1=1
                    IFRQ2=2
                  ELSE
                    IFRQ1=L
                    IFRQ2=L
                  ENDIF
                  DO 87 IGAR = GARBAG(ISATEL,L,1),GARBAG(ISATEL,L,2)
                    DO 88 IFRQ=IFRQ1,IFRQ2
                      CALL MRKOBS(6,SVNEP(ISATEP),IGAR,IFRQ,NSATEL,
     1                              NUMSAT,NDEL,LSTDEL)
88                  CONTINUE
87                CONTINUE
C
                  IF ((LASTCS(ISATEL) .GE. GARBAG(ISATEL,L,1)) .AND.
     1               (LASTCS(ISATEL) .LE. GARBAG(ISATEL,L,2))) THEN
                    LASTCS(ISATEL) = IEPOCH
                    CALL UPDAMB(ISATEL,GARBAG(ISATEL,L,1),IEPOCH,2,
     1                          NNEWAM,LSTAMB,NSATEL,NUMSAT,IPRNT2,
     2                          AMSFLG,IRC)
                  END IF
C
                ELSE
                  GARBAG(ISATEL,L,3) = GARBAG(ISATEL,L,2)
                END IF
                GARBAG(ISATEL,L,1) = IEPOCH
                IF (IEPOCH-GARBAG(ISATEL,L,3) .GT. IAMNEW(3)) THEN
                  CALL UPDAMB(ISATEL,GARBAG(ISATEL,L,3),IEPOCH,4,
     1                        NNEWAM,LSTAMB,NSATEL,NUMSAT,IPRNT2,
     2                        AMSFLG,IRC)
                ELSE
                  CALL UPDAMB(ISATEL,GARBAG(ISATEL,L,3),IEPOCH,-4,
     1                        NNEWAM,LSTAMB,NSATEL,NUMSAT,IPRNT2,
     2                        AMSFLG,IRC)
                END IF
              END IF
              GARBAG(ISATEL,L,2) = IEPOCH
            END IF
            DO IAMB=NNEWAM(ISATEL),1,-1
              IF(IEPOCH.GE.LSTAMB(1,ISATEL,IAMB)) THEN
                LSTAMB(3,ISATEL,IAMB)=IEPOCH
                EXIT
              ENDIF
            ENDDO
          END IF
85      CONTINUE
C
C CALL LINEAR COMBINATION FOR FREQUENCY REQUESTED FOR
C TRIPLE DIFFERENCE SOLUTION
C ---------------------------------------------------
        CALL LINCOM(LTRIP,SVNEP(ISATEP),
     1              OBSEPO(ISATEP,1),OBSEPO(ISATEP,2),
     2              OBSFLE(ISATEP,1),OBSFLE(ISATEP,2),
     3              OBSCOM,FLGCOM)
        II=1+NDIFF
        DDD=DSQRT(TOPPOS(1,II)**2+TOPPOS(2,II)**2+TOPPOS(3,II)**2)
        CALL PDCOR(TOPPOS(1,II),DDD,SZ,XPOL,YPOL,
     1              AOBS(1,IEPO,ISAT))
        OBSFLG(IEPO,ISAT)=FLGCOM
        IF (NDIFF.EQ.0) THEN
          IF (MEATYP.EQ.1) THEN
            AOBS(1:3,IEPO,ISAT)=-AOBS(1:3,IEPO,ISAT)
          ENDIF
          IF(ZEN2(1).LE.MAXZ(1).AND.
     1                .NOT.TSTFLG(FLGCOM,0)) THEN
            ABSVAL(IEPO,ISAT)=OBSCOM-(DIST(1)+DION(1))
            NOBSAT(ISAT)=NOBSAT(ISAT)+1
          ELSE
            ABSVAL(IEPO,ISAT)=0.D0
          ENDIF
        ELSE
          IF(ZEN2(1).LE.MAXZ(1).AND.ZEN2(2).LE.MAXZ(2).AND.
     1                .NOT.TSTFLG(FLGCOM,0)) THEN
            ABSVAL(IEPO,ISAT)=OBSCOM-(DIST(1)-DIST(2)+DION(1)-DION(2))
            NOBSAT(ISAT)=NOBSAT(ISAT)+1
          ELSE
            ABSVAL(IEPO,ISAT)=0.D0
          ENDIF
        END IF
C
C LOOP OVER ALL CARRIERS OF AUX. FILE
C -----------------------------------
        DO 95 ICARR=1,NFRAUX
          CALL LINCOM(FRQAUX(ICARR),SVNEP(ISATEP),
     1                OBSEPO(ISATEP,1),OBSEPO(ISATEP,2),
     2                OBSFLE(ISATEP,1),OBSFLE(ISATEP,2),
     3                OBSCOM,FLGAUX(ICARR))
          IF (NDIFF.EQ.0) THEN
            IF(ZEN2(1).LE.MAXZ(1).AND.
     1                  .NOT.TSTFLG(FLGAUX(ICARR),0).AND.
     2                  OBSCOM.NE.0.D0) THEN
              IF(IIONO.EQ.1) THEN
                CALL IONOSP(XSTSAT(1,1),XSAT,
     1                      OBTIME(IEPO),SZ,ZENION(1),
     2                      FRQAUX(ICARR),IORSYS,STNAME(1),
     3                      SVNEP(ISATEP),ZENMAX(ISTA),DION(1),
     4                      MEATYP,AZIRAD(1),HOI,XSTELL(:,1))
                ABSEPO(ICARR)=OBSCOM-(DIST(1)+DION(1))
              ELSE
                ABSEPO(ICARR)=OBSCOM-DIST(1)
              ENDIF
            ELSE
              ABSEPO(ICARR)=0.D0
            ENDIF
          ELSE
            IF(ZEN2(1).LE.MAXZ(1).AND.ZEN2(2).LE.MAXZ(2).AND.
     1                  .NOT.TSTFLG(FLGAUX(ICARR),0).AND.
     2                  OBSCOM.NE.0.D0) THEN
              IF(IIONO.EQ.1) THEN
                CALL IONOSP(XSTSAT(1,1),XSAT,
     1                      OBTIME(IEPO),SZ,ZENION(1),
     2                      FRQAUX(ICARR),IORSYS,STNAME(1),
     3                      SVNEP(ISATEP),ZENMAX(1),DION(1),
     4                      MEATYP,AZIRAD(1),HOI,XSTELL(:,1))
                CALL IONOSP(XSTSAT(1,2),XSAT,
     1                      OBTIME(IEPO),SZ,ZENION(2),
     2                      FRQAUX(ICARR),IORSYS,STNAME(2),
     3                      SVNEP(ISATEP),ZENMAX(2),DION(2),
     4                      MEATYP,AZIRAD(2),HOI,XSTELL(:,2))
                ABSEPO(ICARR)=OBSCOM-
     1                      (DIST(1)-DIST(2)+DION(1)-DION(2))
              ELSE
                ABSEPO(ICARR)=OBSCOM-(DIST(1)-DIST(2))
              ENDIF
            ELSE
              ABSEPO(ICARR)=0.D0
            ENDIF
          END IF
95      CONTINUE
C
C CHECK IF ALL LINEAR COMBINATIONS ARE VALID
C ------------------------------------------
        IF (NFRAUX.EQ.3) THEN
          DO 96 ICARR=1,NFRAUX
            IF(ABSEPO(ICARR).EQ.0.D0.OR.
     1                  TSTFLG(FLGAUX(ICARR),0)) THEN
              DO 97 KCARR=1,NFRAUX
                ABSEPO(KCARR)=0.D0
97            CONTINUE
              GOTO 98
            ENDIF
96        CONTINUE
        ENDIF
98      CONTINUE
C
C WRITE OBSERVATION EQN(S) ON DISK FILE
C -------------------------------------
99      CONTINUE
        WRITE(LFN002) (AOBS(K,IEPO,ISAT),K=1,3),
     1              (ABSEPO(ICARR),FLGAUX(ICARR),ICARR=1,NFRAUX)
100   CONTINUE
C
C MARK ONE REMAINING SATELLITE
C ----------------------------
      DO 101 L=1,NFRFIL
        ISATR = 0
        DO 102 ISATEP=1,NSATEP
          IF ((OBSEPO(ISATEP,L) .NE. 0.D0) .AND.
     1                (.NOT. TSTFLG(OBSFLE(ISATEP,L),0))) THEN
            ISATR=ISATR+1
            I1SAT=ISATEP
          END IF
102     CONTINUE
        IF (ISATR.EQ.1) THEN
          IF(MRK1O2.EQ.1.AND.NFRFIL.EQ.2) THEN
            IFRQ1=1
            IFRQ2=2
          ELSE
            IFRQ1=L
            IFRQ2=L
          ENDIF
          DO 103 IFRQ=IFRQ1,IFRQ2
            CALL MRKOBS(6,SVNEP(I1SAT),IEPOCH,IFRQ,NSATEL,
     1                  NUMSAT,NDEL,LSTDEL)
103       CONTINUE
        ENDIF
101   CONTINUE
C
200   CONTINUE
C
C DEALLOCATE KINEMATIC ARRAYS
C ---------------------------
      IF (IRCKIN.EQ.0) THEN
        CALL  READKIN(FILKIN,STNAME(1),
     1              OBTIME(IEPO),1,1,XKIN,IRCKIN2)
      END IF
      IF (IRCVEL.EQ.0) THEN
        CALL  READVEL(FILVEL,STNAME(1),
     1                OBTIME(IEPO),1,1,XKIN,IRCVEL2)
      END IF
C
      NEPEFF=NEPO
      GOTO 999
C
201   NEPEFF=IEPO-1
C
C MARK PIECES WITH SMALL NUMBER OF OBSERVATIONS (END OF FILE)
C -----------------------------------------------------------
      DO 288 ISATEL=1,NSATEL
        DO 285 L=1,NFRFIL
          IF (GARBAG(ISATEL,L,1) .NE. 0) THEN
            IF (((GARBAG(ISATEL,L,2)-GARBAG(ISATEL,L,1)+1)*IDELTT)
     1          .LT. MNCONT) THEN
              IF(MRK1O2.EQ.1.AND.NFRFIL.EQ.2) THEN
                IFRQ1=1
                IFRQ2=2
              ELSE
                IFRQ1=L
                IFRQ2=L
              ENDIF
              DO 289 IFRQ=IFRQ1,IFRQ2
                DO 287 IGAR = GARBAG(ISATEL,L,1),GARBAG(ISATEL,L,2)
                  CALL MRKOBS(6,NUMSAT(ISATEL),IGAR,IFRQ,NSATEL,
     1                        NUMSAT,NDEL,LSTDEL)
287             CONTINUE
289           CONTINUE
              CALL UPDAMB(ISATEL,GARBAG(ISATEL,L,1),GARBAG(ISATEL,L,2),
     1                    0,NNEWAM,LSTAMB,NSATEL,NUMSAT,IPRNT2,
     2                    AMSFLG,IRC)
            ELSE
              GARBAG(ISATEL,L,3) = GARBAG(ISATEL,L,2)
            END IF
          END IF
285     CONTINUE
288   CONTINUE
C
C WRITE FINAL OUTPUT LINE OF MARKING MESSAGES
C -------------------------------------------
999   IF(ITITLE.EQ.0) WRITE(LFNPRT,998)
998     FORMAT(/,1X,72('-'))
C
      RETURN
      END SUBROUTINE

      END MODULE

