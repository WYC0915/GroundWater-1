      MODULE s_PREPHA
      CONTAINS

C*
      SUBROUTINE PREPHA(IUSFLG,DISCLV,Q     ,MXINTR,IPPROC,LTRIP ,
     1                  RMSMAX,JMPOPT,TOLJMP,STANAM,XSTAT ,XSTELL,
     2                  SIGWGS,NSATEL,NUMSAT,NEPOCH,NFRCHK,NFREQ ,
     3                  IFRMAT,TIMREF,IDELTT,RECTYP,ANTTYP,IANTEN,
     4                  CSESS ,SWIDTH,MRK1O2,IRJECT,MXOGAP,MXIOND,
     5                  DELAMB,IWLSCR,SIGL12,IPRNT1,IPRNT2,MINCYC,
     6                  IIONO ,IREFIL,NDEL  ,LSTDEL,NMAXI ,LSTMXI,
     7                  MAXZEN,SECIPL,IRESID,NSLIP ,LSTSLP,SLPLST,
     8                  SLPXXX,IONO  ,TRPFLG,PAR   ,MXHOLE,MNCONT,
     9                  IAMNEW,NNEWAM,LSTAMB,LASTCS,NTONLY,L5CLEA,
     .                  OMCMAX,AMSFLG,NDIFF ,KINSTA,MXZLEO,ITROPO,
     1                  IEXTRA,MEATYP,POSECC,AELL  ,BELL  ,DXELL ,
     2                  DRELL ,SCELL ,CLKFLG,NCLKEV,LSTCLK)
CC
CC NAME       :  PREPHA
CC
CC PURPOSE    :  - NON-PARAMETRIC SCREENING OF SINGLE DIFFERENCES
CC               - NON-PARAMETRIC SCREENING OF DOUBLE DIFFERENCES
CC               - TRIPLE DIFFERENCE SOLUTION USING CARRIER LTRIP
CC               - DETECT ALL CYCLE SLIPS (TOTAL NUMBER NSLIP)
CC                 THIS SUBROUTINE MAY BE USED TO PROCESS ONE
CC                 FREQUENCY AT THE TIME OR BOTH FREQUENCIES.
CC
CC PARAMETERS :
CC         IN :  IUSFLG : USE FLAGS IN OBS FILES               I*4
CC                         =0 : IGNORE FLAGS
CC                         =1 : USE FLAGS
CC               DISCLV(I),I=1,2: MAX. ALLOWED DISCONTINUITY   R*8
CC                        IN L1 AND L2
CC               Q      : POLYNOMIAL DEGREE FOR PARAMETRIC     R*8
CC                        SCREENING
CC               MXINTR : MAX. INTERVAL LENGTH FOR POLYNOMIAL  I*4
CC                        SCREENING
CC               IPPROC : SCREENING FLAG                       I*4
CC                        IPPROC(1): SCREENING SINGLE DIFF.
CC                        IPPROC(2): SCREENING SINGLE DIFF.
CC                         =0 : SKIP SCREENING
CC                         =1 : SCREEN OBSERVATIONS
CC               LTRIP  : CARRIER TO BE USED FOR TRIPLE        I*4
CC                        DIFFERENCE SOLUTION
CC               RMSMAX : MAX RMS ALLOWED, OTHERWISE SOL.FLAG  R*8
CC               JMPOPT : CLOCK EVENT OPTIONS                  I*4(6)
CC                         (1): 0/1 ALLOW MS-JUMP CYCLE SLIPS
CC                         (2): MIN. SIZE OF A CLOCK EVENT (NS)
CC                         (3): MARK EPOCHS WITH CLOCK EVENTS
CC                              UP TO (IN S)
CC                         (4): 0/1 AMBIGUITIES FOR ALL SATELLITES
CC                         (5): 0/1 FLAG IF MS-JUMP IN FILE
CC                         (6): 0/1 FLAG IF A CLOCK EVENT IN FILE
CC               TOLJMP : TOLERANCE TO DETECT A MS-JUMP        R*8
CC               STANAM(I),I=1,2: STATION NAMES                CH*16
CC               XSTAT(K,I),K=1,3,I=1,2: STATION COORDINATES   R*8
CC               XSTELL(K,I),K=1,3,I=1,2: ELLIPSOIDAL STAT. COORD
CC               SIGWGS(I),I=1,2,3: A PRIORI WEIGHTS FOR       R*8
CC                        COORDINATES
CC               NSATEL : TOTAL NUMBER OF SATELLITES           I*4
CC               NUMSAT(I),I=1,..,NSATEL: SATELLITE NUMBERS    I*4
CC               NEPOCH : NUMBER OF EPOCHS IN  FILE            I*4
CC               NFRCHK : FREQUENCY TO BE CHECKED              I*4
CC                        =1: CHECK L1
CC                        =2: CHECK L2
CC                        =3 : L1 AND L2 TOGETHER (VIA L5 AND L3)
CC                        =4 : L1 AND L2 SEPARATELY
CC               NFREQ  : NUMBER OF FREQUENCIES IN FILE        I*4
CC               IFRMAT : FORMAT NUMBER OF OBSERVATION FILE    I*4
CC               TIMREF : REFERENCE TIME                       R*8
CC               IDELTT : TAB. INTERVAL IN SECONDS             I*4
CC               RECTYP : RECEIVER TYPES                       CH*20(2)
CC               ANTTYP : ANTENNA  TYPES                       CH*20(2)
CC               IANTEN : RECEIVER ANTENNA NUMBERS             I*4(2)
CC               CSESS(I),I=1,2: SESSION IDENTIFICATIONS       CH*4
CC               SWIDTH(K),K=1,2: NUMBER OF NEAREST INTEGERS   I*4
CC                        TO BE TESTED
CC                         K=1 : IN L1/L2
CC                         K=2 : IN L5
CC               MRK1O2 : FLAG TO MARK UNPAIRED L1/L2 OBSERVA- I*4
CC                        TIONS
CC                         =0 : NO MARKING DONE
CC                         =1 : L1 WITHOUT L2, OR L2 WITHOUT L1
CC                              OBSERVATIONS ARE MARKED
CC               IRJECT : OUTLIER REJECTION (YES=1,NO=0)       I*4
CC               MXOGAP : MAXIMUM OBSERVATION GAP ALLOWED IN   I*4
CC                        OUTLIER REJECTION (SEC)
CC               MXIOND : MAXIMUM IONOSPHERE CHANGE BETWEEN    I*4
CC                        EPOCHS (IN % OF L1 CYCLES) FOR OUT-
CC                        LIER REJECTION
CC               IWLSCR(L),L=1,2: WAVELENGTH FACTOR FOR        I*4
CC                        L1, L2 SCREENING
CC               SIGL12(L),L=1,2: RMS FOR CARRIER L            R*8
CC               IPRNT1 : PRINT LEVEL FOR OBS. SCREENING       I*4
CC                        =0: NO MESSAGES PRINTED
CC                        =1: SUMMARY MESSAGES PRINTED
CC                        =2: ALL MESSAGES PRINTED
CC               IPRNT2 : PRINT LEVEL FOR CYCLE SLIP DETECTION I*4
CC                        =0: NO MESSAGES PRINTED
CC                        =1: SUMMARY MESSAGES PRINTED
CC                        =2: ALL MESSAGES PRINTED
CC               MINCYC : ACCEPT CYCLE SLIPS > "MINCYC" CYCLES I*4
CC               IIONO  : IONOSPHERE INDEX (1=APPLY IONOSP.)   I*4
CC               IREFIL : FILE NUMBER IN RESIDUAL FILE         I*4
CC               NDEL   : NUMBER OF DELETIONS                  I*4
CC               LSTDEL(K,I),K=1,..,5, I=1,2,..,NDEL           I*4
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
CC                               =6: SMALL PIECE
CC                               =7: BAD OBSERVED-COMPUTED
CC                               ALL AREAS MARKED OR CHANGED IN THE
CC                               LATEST RUN HAVE A NEGATIVE SIGN
CC               NMAXI  : NUMBER OF "MAX. INTERVAL EXCEEDED"   I*4
CC               LSTMXI(K,I),K=1,..,5, I=1,2,..,NMAXI          I*4
CC                        DEFINITION OF "MAX.INT.EXC" NUMBER I
CC                        (1,I): EPOCH
CC                        (2,I): SATELLITE
CC                        (3,I): FREQUENCY (1=L1, 2=L2, 3=BOTH)
CC                        (4,I): BREAK IN NUMBER OF EPOCHS
CC                        (5,I): DETECTED BY
CC                               =1: SINGLE FREQ. REJECTION
CC                               =2: DUAL   FREQ. REJECTION
CC               SECIPL : MAX INTERVAL FOR CLK INTERPOLATION   R*8
CC                        (SECONDS)
CC               MAXZEN : MAXIMUM SATELLITE ZENITH DISTANCE    I*4
CC                        (DEGREE)
CC               IRESID : SAVE RESIDUAL FLAG                   I*4
CC                        =0 : DO NOT SAVE RESIDUALS
CC                        =1 : SAVE RESIDUALS
CC               MXHOLE : MAXIMAL GAP IN OBSERVATIONS ALLOWED  I*4
CC                        TO BE CONSIDERED AS CONTINUOUS (SEC)
CC               MNCONT : MINIMAL TIME INTERVAL OF CONTINUOUS  I*4
CC                        OBSERVATIONS (SEC)
CC               IAMNEW(I),I=1,2,3: SETTING OF NEW AMBIGUITIES I*4
CC                        I=1 : USE CYCLE SLIP FLAG (0/1)
CC                        I=2 : IF PROBLEM IN SLIP-FIXING (0/1)
CC                        I=3 : AFTER GAP LARGER THAN (SEC)
CC                        I=4 : USE AMBIGUITIES FROM FILE (0/1)
CC                        I=5 : MIN. TIME OF OBS. PER AMB. (SEC)
CC               NTONLY : TEST OBS. WITH CYCLE SLIP FLAG       I*4
CC                          ONLY (0/1)
CC               L5CLEA : L5 IS CLEAN (EXCEPT FLAGGED          I*4
CC                          EPOCHS)  (0/1)
CC               OMCMAX : MAXIMUM OBSERV-COMPUTED VALUE (M)    R*8
CC               DELAMB(IAMB) : AMBIGUITIES TO BE DELETED      I*4
CC               NDIFF  : NUMBER OF DIFFERENCES (ZD=0, SD=1)   I*4
CC               KINSTA : KINEMATIC COORDINATES ESTIMATION     I*4
CC               MXZLEO : LEO MAX. ZENITH ANGLE (DEGREE)       I*4
CC               ITROPO  : MODEL INDEX                         I*4
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
CC                         IN PRINPT IEXTRA SET TO 3 IN THIS CASE
CC                         IF (IEXTRA==2) IEXTRA=3
CC               MEATYP : MEASUREMENT TYPE                     I*4
CC                        =1: PHASE OBSERVATIONS
CC                        =2: CODE OBSERVATIONS
CC                        =3: RANGE OBSERVATIONS
CC               POSECC(I,K),I=1,2,3,K=1,2                     R*8
CC                        POSITIONING ECCENTRICITIES
CC                        I: COORDINATE
CC                        K: STATION
CC               AELL,BELL: SEMI-MAJOR AND -MINOR AXIS OF      R*8
CC                        ELIPSOID
CC               DXELL(I),I=1,2,3: SHIFT TO WGS-84             R*8
CC               DRELL(I),I=1,2,3: ROTATIONS TO WGS-84         R*8
CC               SCELL  : SCALE TO WGS-84                      R*8
CC        OUT :  NSLIP  : NUMBER OF CYCLE SLIPS FOUND          I*4
CC               LSTSLP(K,I),K=1,..,6,I=1,2,..,NSLIP:          R*8
CC                        SLIP DEFINITION
CC               SLPLST(I),I=1,2,..,NSLIP: SIZE OF SLIPS       R*8
CC               SLPXXX(2,I),I=1,..,NSLIP: REAL VALUED         R*8
CC                        ESTIMATES FOR SLIPS - INTEGER SLIP
CC                        SINGLE FREQ.: L1/L2 ESTIMATES
CC                        DUAL   FREQ.: L1/L2 AND L5 ESTIMATES
CC               IONO(K,I),K=1,2,3, I=1,2,..,NSLIP             R*8
CC                        K=1: MEAN VALUE FOR IONOSPHERE FROM
CC                             L1 AND L2 CARRIER
CC                        K=2: DIFFERENCE OF ESTIMATES FROM L1
CC                             AND L2
CC                        K=3: RESIDUAL IN L3
CC               TRPFLG : TRIPLE DIFFERENCE SOLUTION FLAG     CH*1
CC                        ='B': BAD OR NO SOLUTION
CC                        ='G': GOOD SOLUTION
CC               PAR(I),I=1,2,3: TRIPLE DIFFERENCE SOLUTION    R*8
CC                        VECTOR
CC    IN/OUT :   NNEWAM(ISATEL) : NUMBER OF AMBIGUITIES        I*4
CC               LSTAMB(I,ISATEL,IAMB)  LIST OF AMBIGUITIES    I*4
CC                        I=1 : THE FIRST EPOCH
CC                        I=2 : TYPE: 1 ... FILE
CC                                    2 ... CYCLE SLIP
CC                                    3 ... USER
CC                                    4 ... GAP
CC                                    5 ... PREPOCESSING PROBLEM
CC                                    6 ... CLOCK EVENT
CC                        I=3 : THE LAST EPOCH WITH OBS.
CC               LASTCS(ISATEL) : LAST EPOCH WITH CYCLE        I*4
CC                                SLIP FLAG
CC               AMSFLG : INDICATER OF MAXAMS EXCEEDINGS      CH*1
CC               CLKFLG : CLOCK FLAG                          CH*1
CC                        'G': OK, NO EVENT
CC                        'J': MS-JUMP, REPAIRED AS CYCLE SLIP
CC                        'B': OTHER BIG CLOCK VALUE
CC               NCLKEV : NUMBER OF CLOCK EVENTS IN LIST      I*4
CC               LSTCLK : (1,I): EPOCH OF THE CLOCK EVENT
CC                        (2,I): MAGNITUDE OF THE CLOCK EVENT
CC                               IN NANOSECONDS
CC                        (3,I): CLOCK ESTIMATE FROM CODSPP
CC                               IN NANOSECONDS
CC                        (4,I): ACTION (0:NONE,1:MARK,2:AMB)
CC
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER, L.MERVART
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/04/19 14:19
CC
CC CHANGES    :  05-JUN-92 : ??: CHANGES FOR THE NEW MAUPRP VERSION
CC               23-DEC-92 : ??: DECLARATION OF "MXNAMB"
CC               03-MAR-93 : ??: NEW PARAMETERS "RECTYP","ANTTYP","IANTEN"
CC                               FOR CALL DSRDBL
CC               19-MAR-93 : ??: MAXEPO REDUCED TO 300, ELSE MANY OBSERV.
CC                               LOST DUE TO MISSING REFERENCE SATELLITE
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               24-APR-95 : MR: MARKING OF BAD TRIPLE DIFF. OBS.
CC               21-AUG-95 : MR: AUXILIARY VARIABLES "LSTDL2","LSTDL3"
CC               05-OCT-95 : MR: ADD PROBLEM STRING IN CALL (REMOVED)
CC               05-MAR-96 : TS: HANDLING OF "MAXAMS" EXCEEDINGS
CC               26-MAR-96 : MR: ADD "CSESS" AS PARAMETER AND TO CALL
CC                               DSRDBL
CC               06-MAY-96 : TS: REMOVED OLD SAT.CLOCK STUFF
CC               23-SEP-97 : DI: USE MAXSAT.inc
CC               16-JAN-02 : DS: ZERO DIFFERENCE PROCESSING
CC               19-JUN-02 : RD: NEW CALL FOR SR INITRP
CC               26-JUN-02 : RD: ADAPT PROGRAM OUTPUT TO ZD CASE
CC               09-AUG-02 : RD: ADD STANAM TO UPDTRP FOR OUTPUT
CC               18-SEP-02 : DS: KINEMATICS FOR:LEO,AIRPLANE,
CC                               SHIP,GROUND,STATIC GROUND
CC               19-SEP-02 : RD: IMPROVED OMCMAX-HANDLING FOR BAD REF.SAT
CC               07-OCT-02 : RD: PREVENT INDEX==0 FOR MRK1O2
CC               04-DEC-02 : RD: ADD CLOCK EVENT FLAG AND LIST
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               27-JUN-03 : RD: ADD KINSTA TO PARAMETER LIST OF DSRDBL
CC               08-SEP-03 : HU: ANTNAM, RECNAM CHR16 -> CHR20
CC               10-MAR-04 : HB: CHANGE ORDER OF MODULES
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               01-NOV-07 : HB: ADD PARAMETER SECIPL
CC               30-JUN-08 : RD: VMF ADDED
CC               08-SEP-10 : RD: REQUEST SAT-CLK-CORRECTIONS BY NDIFF
CC                               (OLD BY THE AVAILABILITY OF SATCLK FILE)
CC               31-JAN-12 : RD: CORRECT FORMAT FOR DELCHR = "deleted"
CC               29-FEB-12 : RD: REMOVE UNUSED VARIABLES
CC               30-JUL-12 : RD: SR STATIS WITH OPTIONAL ARGUMENTS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE M_BERN,   ONLY: r8b, lfnprt, lfnerr, lfn002
      USE m_maxdim, ONLY: MAXSAT
      USE s_alcerr
      USE f_tstflg
      USE s_mrkobs
      USE s_dsrdbl
      USE s_updamb
      USE s_chkobs
      USE s_maxtst
      USE s_updtrp
      USE s_initrp
      USE s_trpsol
      USE s_statis
      USE s_exitrc
      USE s_detslp
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IAMB  , IBLOCK, IDEL  , IDELAM, IDELST, IDELTT,
     1          IEPO  , IEPOCH, IEXTRA, IFRMAT, IFRQ  , IFRQ1 , IFRQ2 ,
     2          IFRQM , IGAR  , IIONO , INDDEL, IPRCHK, IPRNT1, IPRNT2,
     3          IRC   , IRC1  , IRC2  , IREF  , IREFIL, IRESID, IRETRN,
     4          IRJECT, ISATCO, ISAT  , ISATEL, ISATI , ITEST , ITIM12,
     5          ITROPO, IUSFLG, JEPO  , JSAT  , K     , KINSTA,
     6          L5CLEA, LSTDL2, LSTDL3, LSTEPO, LTRIP , MAXEPO, MAXOBS,
     7          MAXZEN, MEATYP, MINCYC, MNCONT, MRK1O2, MRKTYP, MXCAMB,
     8          MXCEPO, MXCSAT, MXHOLE, MXINTR, MXIOND, MXOGAP, MXZLEO,
     9          NBLOCK, NCLKEV, NCLKSA, NDEL  , NDELAM, NDELO , NDIFF ,
     1          NEPEFF, NEPLST, NEPO  , NEPOCH, NEPTOT, NFRAUX, NFRCHK,
     2          NFREQ , NGOOD , NMAXI , NOBS  , NPAR  , NSATBL, NSATEL,
     3          NSLIP , NTONLY, NTRP
C
      REAL*8    AELL  , BELL  , CLKSUM, CLOCK , CLOCK0, OMCMAX, RMSMAX,
     1          RMSTRP, SCELL , SECIPL, TIMREF, TOLJMP
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      PARAMETER (MAXEPO=300)
C
      REAL(r8b),ALLOCATABLE,DIMENSION(:)::EPOOMC
      REAL*8 DISCLV(2),SIGWGS(3),SIGPAR(3),SIGL12(2)
      REAL*8 XSTAT(3,2),XSTELL(3,2)
      REAL*8 ANOR(6),BNOR(3),PAR(3)
      REAL*8 OBTIME(MAXEPO),ABSVAL(MAXEPO,MAXSAT)
      REAL*8 AOBS(3,MAXEPO,MAXSAT)
      REAL*8 SLPLST(*),SLPXXX(2,*),IONO(3,*)
      REAL*8 POSECC(3,*),DXELL(*),DRELL(*)
C
      INTEGER*4 Q(2),SVNBL(MAXSAT),IANTEN(2)
      INTEGER*4 NUMSAT(*)
      INTEGER*4 MARKED(MAXSAT),NSLIPS(MAXSAT),IWLSCR(2)
      INTEGER*4 NOBSAT(MAXSAT),FRQAUX(3)
      INTEGER*4 LSTSLP(6,*),LSTDEL(5,*),LSTMXI(5,*),LSTCLK(4,*)
      INTEGER*4 SWIDTH(2),IAMNEW(*),IPPROC(2),JMPOPT(6)
      INTEGER*4 NOBSTT(MAXSAT,2),NMRKTT(MAXSAT,2),NSLPTT(MAXSAT,2)
      INTEGER*4 NSLITT(MAXSAT,2),N1O2TT(2,MAXSAT),NSELTT(2,MAXSAT)
      INTEGER*4 NGAPTT(2,MAXSAT),NGATOT(2),LASTFL(2,MAXSAT)
      INTEGER*4 N12TOT(2),NOBTOT(2),NMRTOT(2),NSLTOT(2),NSITOT(2)
      INTEGER*4 NSETOT(2),LSTAMB(3,MXCSAT,*),NNEWAM(MXCSAT)
      INTEGER*4 LASTCS(MXCSAT),DELAMB(MXCAMB)
      INTEGER*4 TIMLST(3,MAXSAT)
C
      CHARACTER*1  OBSFLG(MAXEPO,MAXSAT),TRPFLG,AMSFLG,CLKFLG
      CHARACTER*4  CSESS(2)
      CHARACTER*6  MXNAMB,MXNEPO,MXNSAT
      CHARACTER*12 delchr
      CHARACTER*16 STANAM(2)
      CHARACTER*20 RECTYP(2),ANTTYP(2)
      CHARACTER*80 LINE
C
      COMMON/CPRPHA/OBTIME,ABSVAL,AOBS,OBSFLG
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMEPO/MXCEPO,MXNEPO
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
C DEFINE PRINT LEVEL FOR "CHKOBS"
      IF(IPRNT1.EQ.2) THEN
        IPRCHK=1
      ELSE
        IPRCHK=0
      ENDIF
C
C CHECK MAXIMUM DIMENSIONS
C ------------------------
      CALL MAXTST(1,'PREPHA',MXNSAT,MAXSAT,MXCSAT,IRC1)
      CALL MAXTST(0,'PREPHA',MXNEPO,MAXEPO,MXCEPO,IRC2)
      IF(IRC1.NE.0.OR.IRC2.NE.0) CALL EXITRC(2)
C
C INITIALIZE ARRAY LASTFL FOR THE SUBROUTINE CYCEPO (THIS ARRAY
C IS TO BE INITIALIZED FOR EACH FILE)
C -------------------------------------------------------------
      DO 70 ISATI=1,NSATEL
        LASTFL(1,ISATI) = 0
        LASTFL(2,ISATI) = 0
70    CONTINUE
C
C CHECK IF SATELLITE CLOCKS ARE REQUESTED
C ---------------------------------------
      IF(NDIFF.EQ.0) THEN
        ISATCO=1
      ELSE
        ISATCO=0
      ENDIF
C
C INITIALIZE TRIPLE DIFFERENCE SOLUTION
C -------------------------------------
      NPAR=3
      CALL INITRP(NPAR,LTRIP,NDIFF,SIGL12,SIGWGS,NTRP,RMSTRP,ANOR,BNOR)
C
C COMPUTE NUMBER OF BLOCKS, NUMBER OF EPOCHS IN LAST BLOCK
C --------------------------------------------------------
      NBLOCK=NEPOCH/MXCEPO
      IF(NBLOCK*MXCEPO.NE.NEPOCH) THEN
        NEPLST=NEPOCH-NBLOCK*MXCEPO
        NBLOCK=NBLOCK+1
      ELSE
        NEPLST=MXCEPO
      ENDIF
C
C INITIALIZE COUNTERS FOR NUMBER OF OBSERV., MARKED OBS., AND SLIPS
C -----------------------------------------------------------------
      DO 20 I=1,2
        N12TOT(I)=0
        NSETOT(I)=0
        NOBTOT(I)=0
        NMRTOT(I)=0
        NSLTOT(I)=0
        NSITOT(I)=0
        NGATOT(I)=0
        DO 10 ISATEL=1,NSATEL
          N1O2TT(I,ISATEL)=0
          NSELTT(I,ISATEL)=0
          NGAPTT(I,ISATEL)=0
          NOBSTT(ISATEL,I)=0
          NMRKTT(ISATEL,I)=0
          NSLPTT(ISATEL,I)=0
          NSLITT(ISATEL,I)=0
10      CONTINUE
20    CONTINUE
C
C PROCESS BASELINE IN NBLOCK BLOCKS
C ---------------------------------
      NEPTOT=0
      DO 500 IBLOCK=1,NBLOCK
C
C NUMBER OF EPOCHS IN CURRENT BLOCK
        IF(IBLOCK.LT.NBLOCK) THEN
          NEPO=MXCEPO
        ELSE
          NEPO=NEPLST
        ENDIF
C
C GET NECESSARY INFORMATION FOR BLOCK IBLOCK
C ------------------------------------------
        CALL DSRDBL(NFRCHK,NFREQ ,LTRIP ,IBLOCK,NEPO  ,IUSFLG,MRK1O2,
     1              IPRNT1,MAXZEN,SECIPL,TIMREF,IDELTT,STANAM,XSTAT ,
     2              XSTELL,RECTYP,ANTTYP,IANTEN,CSESS ,IFRMAT,NSLIP ,
     3              LSTSLP,SLPLST,NDEL  ,LSTDEL,NSATEL,NUMSAT,IIONO ,
     4              ISATCO,KINSTA,NFRAUX,FRQAUX,NEPEFF,NSATBL,
     5              SVNBL ,NOBSAT,OBTIME,AOBS  ,ABSVAL,OBSFLG,IRETRN,
     6              MXHOLE,MNCONT,IAMNEW,NNEWAM,LSTAMB,LASTCS,NTONLY,
     7              L5CLEA,IPRNT2,AMSFLG,NDIFF ,MXZLEO,ITROPO,IEXTRA,
     8              MEATYP,POSECC,AELL  ,BELL  ,DXELL ,DRELL ,SCELL)
        NEPTOT=NEPTOT+NEPEFF
C
C CHECK SINGLE DIFFERENCES FOR DISCONTINUITIES
C --------------------------------------------
        IF(IPPROC(1).EQ.1) THEN
          IF(IPRNT1.EQ.2) WRITE(LFNPRT,11) IBLOCK
11          FORMAT(//,1X,72('-'),
     1              /' CHECK SINGLE DIFFERENCES: BLOCK',I3,
     2              /,1X,72('-'))
          DO 120 ISAT=1,NSATBL
            IF(IPRNT1.EQ.2) WRITE(LFNPRT,12) SVNBL(ISAT)
12            FORMAT(//,' SATELLITE',I3,':',/,1X,12('-'))
            CALL CHKOBS(MEATYP,IPRCHK,Q(1),MXINTR,NEPEFF,TIMREF,
     1                  IDELTT,DISCLV(1),OBTIME,ABSVAL(1,ISAT),
     2                  OBSFLG(1,ISAT),MARKED(ISAT),NSLIPS(ISAT))
            DO 30 ISATEL=1,NSATEL
              IF(NUMSAT(ISATEL).EQ.SVNBL(ISAT)) GOTO 40
30          CONTINUE
40          NOBTOT(1)=NOBTOT(1)+NOBSAT(ISAT)
            NMRTOT(1)=NMRTOT(1)+MARKED(ISAT)
            NOBSTT(ISATEL,1)=NOBSTT(ISATEL,1)+NOBSAT(ISAT)
            NMRKTT(ISATEL,1)=NMRKTT(ISATEL,1)+MARKED(ISAT)
            IF(NSLIPS(ISAT).NE.0) THEN
              NSLTOT(1)=NSLTOT(1)+NSLIPS(ISAT)-1
              NSITOT(1)=NSITOT(1)+1
              NSLPTT(ISATEL,1)=NSLPTT(ISATEL,1)+NSLIPS(ISAT)-1
              NSLITT(ISATEL,1)=NSLITT(ISATEL,1)+1
            ENDIF
120       CONTINUE
        ENDIF
C
C SELECT REFERENCE SATELLITE FOR DOUBLE DIFFERENCES
C -------------------------------------------------
        MAXOBS=0
        DO 130 ISAT=1,NSATBL
          IF(IPPROC(1).EQ.0) THEN
            NGOOD=NOBSAT(ISAT)
          ELSE
            NGOOD=NOBSAT(ISAT)-MARKED(ISAT)-NSLIPS(ISAT)
          ENDIF
          IF(NGOOD.GT.MAXOBS) THEN
            IREF=ISAT
            MAXOBS=NGOOD
          ENDIF
130     CONTINUE
C
C FORM DOUBLE DIFFERENCES WITH RESPECT TO
C THE REFERENCE SATELLITE CHOSEN
C ---------------------------------------
        DO 170 ISAT=1,NSATBL
          IF(ISAT.EQ.IREF) GOTO 170
C
C INDEX IN HEADER SATELLITE ARRAY
          DO 133 ISATEL=1,NSATEL
            IF(NUMSAT(ISATEL).EQ.SVNBL(ISAT)) GOTO 135
133       CONTINUE
C
C FORM DOUBLE DIFFERENCES
135       DO 150 IEPO=1,NEPEFF
            IF(ABSVAL(IEPO,ISAT).NE.0.D0.AND.
     1         ABSVAL(IEPO,IREF).NE.0.D0) THEN
              ABSVAL(IEPO,ISAT)=ABSVAL(IEPO,ISAT)-ABSVAL(IEPO,IREF)
              NOBTOT(2)=NOBTOT(2)+1
              NOBSTT(ISATEL,2)=NOBSTT(ISATEL,2)+1
              DO 140 K=1,3
                AOBS(K,IEPO,ISAT)=AOBS(K,IEPO,ISAT)-
     1                            AOBS(K,IEPO,IREF)
140           CONTINUE
            ELSE
              ABSVAL(IEPO,ISAT)=0.D0
            ENDIF
150       CONTINUE
170     CONTINUE
C
C CHECK DOUBLE DIFFERENCES FOR DISCONTINUITIES
C --------------------------------------------
        IF(IPPROC(2).EQ.1) THEN
          IF(IPRNT1.EQ.2) WRITE(LFNPRT,13) IBLOCK
13          FORMAT(//,1X,72('-'),
     1              /' CHECK SATELLITE DIFFERENCES: BLOCK',I3,
     2              /,1X,72('-'))
C
          DO 200 ISAT=1,NSATBL
            IF(ISAT.EQ.IREF) GOTO 200
            IF(IPRNT1.EQ.2) WRITE(LFNPRT,14) SVNBL(ISAT),SVNBL(IREF)
14            FORMAT(//,' SATELLITE',I3,' (REF. SATELLITE',I3,'):',
     1                /,1X,32('-'))
C
C CHECK DOUBLE DIFFERENCES
            CALL CHKOBS(MEATYP,IPRCHK,Q(2),MXINTR,NEPEFF,TIMREF,
     1                  IDELTT,DISCLV(2),OBTIME,ABSVAL(1,ISAT),
     2                  OBSFLG(1,ISAT),MARKED(ISAT),NSLIPS(ISAT))
            DO 183 ISATEL=1,NSATEL
              IF(NUMSAT(ISATEL).EQ.SVNBL(ISAT)) GOTO 185
183         CONTINUE
185         NMRTOT(2)=NMRTOT(2)+MARKED(ISAT)
            NMRKTT(ISATEL,2)=NMRKTT(ISATEL,2)+MARKED(ISAT)
            IF(NSLIPS(ISAT).NE.0) THEN
              NSLTOT(2)=NSLTOT(2)+NSLIPS(ISAT)-1
              NSITOT(2)=NSITOT(2)+1
              NSLPTT(ISATEL,2)=NSLPTT(ISATEL,2)+NSLIPS(ISAT)-1
              NSLITT(ISATEL,2)=NSLITT(ISATEL,2)+1
            ENDIF
200       CONTINUE
        ENDIF
C
C CHECK FOR THE CLOCK (ZD ONLY)
C (Wrong sat. clock for ref. sat should not delete all observations.)
C -------------------------------------------------------------------
        CLKSUM=0D0
        jEpo  =0
        DO IEPO=1,NEPEFF

          IF (NDIFF.NE.0) CYCLE

          IF (TSTFLG(OBSFLG(IEPO,IREF),0)) CYCLE

          IF (JEPO.NE.0) THEN
            IF (TSTFLG(OBSFLG(JEPO,IREF),0)) CYCLE
          ENDIF

          CLOCK=0D0
          NCLKSA=0
          ALLOCATE(EPOOMC(NSATEL),STAT=IRC)
          CALL ALCERR(IRC,'EPOOMC',(/NSATEL/),'PREPHA')
          DO ISAT=1,NSATBL
            IF (ISAT.EQ.IREF) CYCLE
            IF (ABSVAL(IEPO,ISAT).EQ.0D0) CYCLE
            IF (TSTFLG(OBSFLG(IEPO,ISAT),0)) CYCLE
            IF (JEPO.NE.0) THEN
              IF (ABSVAL(JEPO,ISAT).EQ.0D0) CYCLE
              IF (TSTFLG(OBSFLG(JEPO,ISAT),0)) CYCLE
              EPOOMC(NCLKSA+1)=ABSVAL(IEPO,ISAT)-ABSVAL(JEPO,ISAT)
            ENDIF
            NCLKSA=NCLKSA+1
          ENDDO

          IF (JEPO.NE.0.AND.NCLKSA.GT.1) THEN
            CALL STATIS(NCLKSA,EPOOMC,xMed=CLOCK0)
            JSAT=0
            DO ISAT=1,NCLKSA
              IF (OMCMAX.EQ.0D0) CYCLE
              IF (DABS(CLOCK0-EPOOMC(ISAT)).LE.OMCMAX) THEN
                JSAT=JSAT+1
                CLOCK=CLOCK+EPOOMC(ISAT)
              ENDIF
            ENDDO
            IF (JSAT.GT.1) THEN
              CLOCK=CLOCK/JSAT
            ELSE
              CLOCK=0D0
            ENDIF
          ENDIF
          DEALLOCATE(EPOOMC,STAT=IRC)

          IF (OMCMAX.NE.0D0.AND.DABS(CLOCK).GT.OMCMAX) THEN
C
C GENERATE A MESSAGE LINE
            IEPOCH=IDNINT((OBTIME(IEPO)-TIMREF)*86400.D0/IDELTT)+1
            LINE=' ### SR PREPHA: EPOCH,#SA,SVNREF,O-C TOO BIG:'
            WRITE(LINE(46:80),'(I6,2I3,F10.3,1X,A)') IEPOCH,NCLKSA,
     1            SVNBL(IREF),CLOCK,'(' // STANAM(1)(1:4) // ')'
            IF (DABS(CLOCK).GE.1D3) WRITE(LINE(58:67),'(E10.3)') CLOCK
            IF (LEN_TRIM(STANAM(2)).GT.0) THEN
              WRITE(LINE(74:80),'(A)')  '-' // STANAM(2)(1:4) // ')'
            ENDIF
            WRITE(LFNERR,'(A)') LINE
C
            CLKSUM=CLKSUM+CLOCK
          ENDIF
C
          DO ISAT=1,NSATBL
            IF (ISAT.EQ.IREF.OR.1==1) CYCLE
            IF (TSTFLG(OBSFLG(JEPO,ISAT),0)) CYCLE
            IF (TSTFLG(OBSFLG(IEPO,ISAT),0)) CYCLE
            IF (ABSVAL(JEPO,ISAT).EQ.0D0) CYCLE
            IF (ABSVAL(IEPO,ISAT).EQ.0D0) CYCLE
            ABSVAL(IEPO,ISAT)=ABSVAL(IEPO,ISAT)+CLKSUM
          ENDDO
        ENDDO
C
C UPDATE NORMAL EQUATION SYSTEM
C -----------------------------
        DO 250 ISAT=1,NSATBL
          IF(ISAT.NE.IREF) THEN
            CALL UPDTRP(NEPEFF,OBTIME,IDELTT,TIMREF,SVNBL(ISAT),
     1                  SVNBL(IREF),STANAM,ABSVAL(1,ISAT),
     2                  OBSFLG(1,ISAT),OBSFLG(1,IREF),
     3                  AOBS(1,1,ISAT),OMCMAX,NFREQ,NSATEL,NUMSAT,
     4                  NDEL,LSTDEL,NTRP,ANOR,BNOR,RMSTRP)
          ENDIF
250     CONTINUE
C
C CHECK FOR END OF FILE
        IF(IRETRN.NE.0) GOTO 501
500   CONTINUE
501   CONTINUE
C
C CHECK INTERVAL BETWEEN TWO AMBIGUITIES, DELETE OBSERVATIONS
C AND UPDATE AMBIGUITIES ACCORDING IAMNEW(5)
C -----------------------------------------------------------
      DO 700 ISATEL=1,NSATEL
        NDELAM=0
        DO 710 IAMB=1,NNEWAM(ISATEL)-1
          IF (((LSTAMB(3,ISATEL,IAMB)-LSTAMB(1,ISATEL,IAMB))*IDELTT)
     1        .LT. IAMNEW(5)) THEN
            NDELAM=NDELAM+1
            DELAMB(NDELAM)=LSTAMB(1,ISATEL,IAMB)
            DO 720 IEPOCH=LSTAMB(1,ISATEL,IAMB),
     1                    LSTAMB(1,ISATEL,IAMB+1)-1
              DO 730 IFRQ=1,NFREQ
                CALL MRKOBS(6,NUMSAT(ISATEL),IEPOCH,IFRQ,NSATEL,NUMSAT,
     1                      NDEL,LSTDEL)
730           CONTINUE
720         CONTINUE
          END IF
710     CONTINUE
        DO 740 IDELAM=1,NDELAM
          CALL UPDAMB(ISATEL,DELAMB(IDELAM),DELAMB(IDELAM),0,
     1                NNEWAM,LSTAMB,NSATEL,NUMSAT,IPRNT2,AMSFLG,IRC)
740     CONTINUE
700   CONTINUE
C
C CHECK INTERVAL BETWEEN SUBSEQUENT MARKED AREAS, DELETE OBSERVATIONS
C IF INTERVAL SMALLER THAN "SMALL PIECES"
C -------------------------------------------------------------------
      DO 900 ISATEL=1,NSATEL
        DO 910 IFRQ=1,NFREQ
          IDELST=0
          NDELO=NDEL
          IF(MRK1O2.EQ.1.AND.NFREQ.EQ.2) THEN
            IFRQ1=1
            IFRQ2=2
          ELSE
            IFRQ1=IFRQ
            IFRQ2=IFRQ
          ENDIF
          DO 920 IDEL=1,NDELO
            IF (LSTDEL(1,IDEL).EQ.NUMSAT(ISATEL) .AND.
     1          LSTDEL(4,IDEL).EQ.IFRQ) THEN
              IF (IDELST.NE.0) THEN
                ITEST=(LSTDEL(2,IDEL)-LSTDEL(3,IDELST))*IDELTT
                IF (ITEST.GT.0 .AND.ITEST.LT.MNCONT) THEN
                  LSTDL2=LSTDEL(2,IDEL)
                  LSTDL3=LSTDEL(3,IDELST)
                  DO 930 IFRQM=IFRQ1,IFRQ2
                    DO 940 IGAR = LSTDL3+1,LSTDL2-1
                      CALL MRKOBS(6,NUMSAT(ISATEL),IGAR,IFRQM,NSATEL,
     1                              NUMSAT,NDEL,LSTDEL)
940                 CONTINUE
930               CONTINUE
                ENDIF
              ENDIF
              IDELST=IDEL
            ENDIF
920       CONTINUE
910     CONTINUE
900   CONTINUE
C
C REWIND FILE WITH OBS. EQNS
C --------------------------
      REWIND(UNIT=LFN002)
C
C WRITE SUMMARY OF MARKED OBS. AND CYCLE SLIPS
C --------------------------------------------
      IF(IPRNT1.GT.0) THEN
        DO 820 ISATEL=1,NSATEL
          DO 810 IFRQ=1,NFREQ
            DO 800 IDEL=1,NDEL
              IF(LSTDEL(1,IDEL).EQ.NUMSAT(ISATEL).AND.
     1           LSTDEL(4,IDEL).EQ.IFRQ) THEN
                MRKTYP=IABS(LSTDEL(5,IDEL))
                NOBS=LSTDEL(3,IDEL)-LSTDEL(2,IDEL)+1
                IF(MRKTYP.EQ.3) THEN
                  N1O2TT(IFRQ,ISATEL)=N1O2TT(IFRQ,ISATEL)+NOBS
                  N12TOT(IFRQ)=N12TOT(IFRQ)+NOBS
                ELSE IF(MRKTYP.EQ.5) THEN
                  NSELTT(IFRQ,ISATEL)=NSELTT(IFRQ,ISATEL)+NOBS
                  NSETOT(IFRQ)=NSETOT(IFRQ)+NOBS
                ELSE IF(MRKTYP.EQ.6) THEN
                  NGAPTT(IFRQ,ISATEL)=NGAPTT(IFRQ,ISATEL)+NOBS
                  NGATOT(IFRQ)=NGATOT(IFRQ)+NOBS
                ENDIF
              ENDIF
800         CONTINUE
810       CONTINUE
820     CONTINUE
C
C UNPAIRED L1/L2 OBSERVATIONS
        IF(MRK1O2.EQ.1.AND.NFREQ.EQ.2) THEN
          WRITE(LFNPRT,506)
506       FORMAT(//,1X,72('-'),
     1            /' MARK UNPAIRED L1/L2 OBSERVATIONS: SUMMARY',
     2            /,1X,72('-'),/,
     3            /,' SATELLITE    #L1 MARKED   #L2 MARKED',
     4            /,1X,72('-'),/)
          DO 510 ISATEL=1,NSATEL
            WRITE(LFNPRT,507) NUMSAT(ISATEL),
     1                        (N1O2TT(IFRQ,ISATEL),IFRQ=1,NFREQ)
507         FORMAT(I6,I15,I13)
510       CONTINUE
          WRITE(LFNPRT,508) (N12TOT(IFRQ),IFRQ=1,NFREQ)
508       FORMAT(/,1X,72('-'),/,' TOTAL',I15,I13,/,1X,72('-'))
        ENDIF
C
C OBSERVATIONS WITH SMALL ELEVATION
        WRITE(LFNPRT,556)
556     FORMAT(//,1X,72('-'),
     1        /' MARK OBSERVATIONS WITH SMALL ELEVATION: SUMMARY',
     2        /,1X,72('-'),/,
     3        /,' SATELLITE    #L1 MARKED   #L2 MARKED',
     4        /,1X,72('-'),/)
        DO 551 ISATEL=1,NSATEL
          WRITE(LFNPRT,507) NUMSAT(ISATEL),
     1                      (NSELTT(IFRQ,ISATEL),IFRQ=1,NFREQ)
551     CONTINUE
        WRITE(LFNPRT,508) (NSETOT(IFRQ),IFRQ=1,NFREQ)
C
C OBSERVATIONS FROM SMALL PIECES
        WRITE(LFNPRT,560)
560     FORMAT(//,1X,72('-'),
     1        /' MARK OBSERVATIONS WITHIN SMALL PIECES: SUMMARY',
     2        /,1X,72('-'),/,
     3        /,' SATELLITE    #L1 MARKED   #L2 MARKED',
     4        /,1X,72('-'),/)
        DO 561 ISATEL=1,NSATEL
          WRITE(LFNPRT,507) NUMSAT(ISATEL),
     1                      (NGAPTT(IFRQ,ISATEL),IFRQ=1,NFREQ)
561     CONTINUE
        WRITE(LFNPRT,508) (NGATOT(IFRQ),IFRQ=1,NFREQ)
C
C SINGLE DIFF. SCREENING
        IF(IPPROC(1).EQ.1) THEN
          WRITE(LFNPRT,511)
511       FORMAT(//,1X,72('-'),
     1            /' CHECK OBSERVATIONS FROM FILE: SUMMARY',
     2            /,1X,72('-'),/,
     3            /,' SATELLITE    #OBS.   MARKED   SLIPS  INIT.SLIPS',
     4            /,1X,72('-'),/)
          DO 520 ISATEL=1,NSATEL
            WRITE(LFNPRT,512) NUMSAT(ISATEL),NOBSTT(ISATEL,1),
     1                        NMRKTT(ISATEL,1),NSLPTT(ISATEL,1),
     2                        NSLITT(ISATEL,1)
512         FORMAT(I6,I11,I9,I8,I10)
520       CONTINUE
          WRITE(LFNPRT,513) NOBTOT(1),NMRTOT(1),NSLTOT(1),NSITOT(1)
513       FORMAT(/,1X,72('-'),/,' TOTAL',I11,I9,I8,I10,/,1X,72('-'))
        ENDIF
C
C DOUBLE DIFF. SCREENING
        IF(IPPROC(2).EQ.1) THEN
          WRITE(LFNPRT,516)
516       FORMAT(//,1X,72('-'),
     1            /,' CHECK SATELLITE DIFFERENCES: SUMMARY (WITH ',
     2              'RESPECT TO REF.SATELLITE)',
     3            /,1X,72('-'),/,
     4            /,' SATELLITE    #OBS.   MARKED   SLIPS  INIT.SLIPS',
     5            /,1X,72('-'),/)
          DO 530 ISATEL=1,NSATEL
      delchr=''
      if (nobstt(isatel,2) > 0) then
        if (DBLE(nmrktt(isatel,2))/DBLE(nobstt(isatel,2)) > 0.25d0 .OR.
     1      DBLE(nslptt(isatel,2))/DBLE(nobstt(isatel,2)) > 0.25d0) then
          delchr='  << deleted'
          do iepo=1,nepoch
            do ifrq=1,nfreq
              call mrkobs(7,numsat(isatel),iepo,ifrq,nsatel,numsat,
     1                    ndel,lstdel)
            enddo
          enddo
        endif
      endif
            WRITE(LFNPRT,5120) NUMSAT(ISATEL),NOBSTT(ISATEL,2),
     1                         NMRKTT(ISATEL,2),NSLPTT(ISATEL,2),
     2                         NSLITT(ISATEL,2),delchr

530       CONTINUE
          WRITE(LFNPRT,513) NOBTOT(2),NMRTOT(2),NSLTOT(2),NSITOT(2)
5120      FORMAT(I6,I11,I9,I8,I10,A)
        ENDIF
      ENDIF
C
C TRIPLE DIFFERENCE SOLUTION
C --------------------------
      CALL TRPSOL(LTRIP,NTRP,ANOR,BNOR,RMSTRP,TRPFLG,PAR,SIGPAR)
      IF (KINSTA.EQ.1) PAR(1:3)=0.D0
C
C FIND CYCLE SLIPS
C ----------------
      CALL DETSLP(LFN002,IPRNT2,RMSMAX,JMPOPT,TOLJMP,IRESID,IREFIL,
     1            NFREQ ,NFRAUX,FRQAUX,NSATEL,NUMSAT,NEPTOT,LASTFL,
     2            PAR   ,SIGL12,IWLSCR,TIMREF,IDELTT,NEPOCH,SWIDTH,
     3            MRK1O2,IRJECT,MXOGAP,MXIOND,MINCYC,STANAM,NSLIP ,
     4            LSTSLP,SLPLST,SLPXXX,IONO  ,NDEL  ,LSTDEL,NMAXI ,
     5            LSTMXI,IAMNEW,NNEWAM,LSTAMB,NTONLY,L5CLEA,AMSFLG,
     6            LTRIP ,SIGWGS,KINSTA,NDIFF ,MEATYP,TIMLST,CLKFLG,
     7            NCLKEV,LSTCLK)
C
C CHECK INTERVAL BETWEEN TWO AMBIGUITIES, DELETE OBSERVATIONS
C AND UPDATE AMBIGUITIES ACCORDING IAMNEW(5)
C -----------------------------------------------------------
      DO 600 ISATEL=1,NSATEL
        NDELAM=0
        DO 610 IAMB=1,NNEWAM(ISATEL)
          INDDEL=0
          IF (IAMB.EQ.NNEWAM(ISATEL)) THEN
            ITIM12=MAX0(TIMLST(1,ISATEL),TIMLST(2,ISATEL))
C
            IF (((ITIM12-LSTAMB(1,ISATEL,IAMB))*IDELTT)
     1        .LT. IAMNEW(5)) THEN
              INDDEL=1
              LSTEPO=ITIM12
            END IF
          ELSE
            IF (((LSTAMB(1,ISATEL,IAMB+1)-LSTAMB(1,ISATEL,IAMB))*IDELTT)
     1        .LT. IAMNEW(5)) THEN
              INDDEL=1
              LSTEPO=LSTAMB(1,ISATEL,IAMB+1)-1
            END IF
          END IF
          IF (INDDEL.EQ.1) THEN
            NDELAM=NDELAM+1
            DELAMB(NDELAM)=LSTAMB(1,ISATEL,IAMB)
            DO 620 IEPOCH=LSTAMB(1,ISATEL,IAMB),LSTEPO
              DO 630 IFRQ=1,NFREQ
                CALL MRKOBS(6,NUMSAT(ISATEL),IEPOCH,IFRQ,NSATEL,NUMSAT,
     1                      NDEL,LSTDEL)
630           CONTINUE
620         CONTINUE
          END IF
610     CONTINUE
        DO 640 IDELAM=1,NDELAM
          CALL UPDAMB(ISATEL,DELAMB(IDELAM),DELAMB(IDELAM),0,
     1                NNEWAM,LSTAMB,NSATEL,NUMSAT,IPRNT2,AMSFLG,IRC)
640     CONTINUE
600   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
