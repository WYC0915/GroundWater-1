      MODULE s_CDCOMP
      CONTAINS

C*
      SUBROUTINE CDCOMP(PRIOPT,NSAMPL,ZENMAX,SECIPL,OBSWIN,ITROPO,
     1                  IONO  ,ICORFL,ICLPOL,USEMRK,OBSFIL,FILFRQ,
     2                  NSATEL,IDELTT,TIMREF,STANAM,POSECC,RECTYP,
     3                  ANTTYP,IANTEN,CSESS ,SATNUM,XSTAT ,XSTELL,
     4                  AELL  ,BELL  ,DXELL ,DRELL ,SCELL ,EPH   ,
     5                  CLOCK ,IFRMAT,IORBFL,IOUTLR,DIFMAX,CONFID,
     6                  MINDOF,USRSIG,IFILE ,MEATYP,NITER ,INDSVN,
     7                  OBSSAT,XSTANW,XSTNEL,CLKMOD,OFFS  ,DOFFS ,
     8                  SIGMA0,NORDIM,DPAR  ,MCOR  ,MELL  ,MCLOCK,
     9                  NDEL  ,LSTDEL,NITERF,KINEST,
     1                  LFNKOU,IRCODE)
CC
CC NAME       :  CDCOMP
CC
CC PURPOSE    :  PARAMETER ESTIMATION PART OF PGM CODSPP
CC               (ONE FILE PER CALL)
CC
CC PARAMETERS :
CC         IN :  PRIOPT : PRINT OPTIONS (YES=1,NO=0)            I*4
CC                        PRIOPT(1): SATELLITE ELEVATIONS
CC                        PRIOPT(2): RESIDUALS
CC               NSAMPL : SAMPLING RATE (ONLY EVERY N-TH OBS.)  I*4
CC               ZENMAX : MAXIMUM ZENITH DISTANCE  (RAD)        R*8
CC               SECIPL : MAX INTERVAL FOR CLOCK INTERPOLATION  R*8
CC               OBSWIN(I): OBSERVATION WINDOW (MJD)            R*8(2)
CC                        I=1,2
CC               ITROPO : TROPOSPHERIC MODEL                    I*4
CC               IONO   : IONOSPHERIC MODEL                     I*4
CC               ICORFL : COORD. ESTIMATION (YES=1, NO=0)       I*4
CC               ICLPOL : TYPE OF CLOCK MODELLING               I*4
CC                        ICLPOL>0 : POLYNOMIAL OF DEGREE ICLPOL
CC                        ICLPOL=-1: ONE OFFSET PER EPOCH
CC               USEMRK : USE MARK FLAGS FROM OBSERVATION FILES I*4
CC               OBSFIL : OBSERVATION FILE NAME                CH*32
CC               FILFRQ : FREQUENCY   TO BE PROCESSED           I*4
CC               NSATEL : NUMBER OF SATELLITES                  I*4
CC               IDELTT : OBSERVATION INTERVAL (SEC)            I*4
CC               TIMREF : REFERENCE TIME OF FILE (MJD)          R*8
CC               STANAM : STATION NAME                         CH*16
CC               POSECC(K,J): POSITIONING ECCENTRICITIES        R*8
CC                        K=1,2,3: COORDINATE
CC                        J=1,2  : STATION
CC               RECTYP(ISTA),ISTA=1,2: RECEIVER TYPE          CH*20
CC               ANTTYP(ISTA),ISAT=1,2: ANTENNA TYPE           CH*20
CC               IANTEN(ISTA),ISTA=1,2: ANTENNA NUMBER          I*4
CC               CSESS(I),I=1,2: SESSION IDENTIFICATIONS       CH*4
CC               SATNUM (K): SVN NUMBERS                        I*4
CC                        K=1,2,...,NSATEL
CC               XSTAT(K): RECTANGULAR STATION COORDINATES      R*8
CC                        K=1,2,3
CC               XSTELL(K): ELLIPSOIDAL STATION COORDINATES     R*8
CC                        IN SPECIFIED DATUM
CC                        K=1,2,3
CC               AELL   : SEMI-MAJOR AXIS OF ELLIPSOID          R*8
CC               BELL   : SEMI-MINOR AXIS OF ELLIPSOID          R*8
CC               DXELL(I),I=1,2,3: SHIFTS TO WGS-84 (M)         R*8
CC               DRELL(I),I=1,2,3: ROTATIONAL TO WGS-84         R*8
CC               SCELL  : SCALE FACTOR BETWEEN LOCAL GEODETIC   R*8
CC                        DATUM AND WGS-84
CC               EPH(I,K): BROADCAST EPHEMERIS DATA SETS        R*8
CC                        I=1,2,...,20*MAXCLK; K=1,2,...,MAXSAT
CC               CLOCK(I,K): BROADCAST CLOCK DATA SETS          R*8
CC                        I=1,2,...,20*MAXCLK; K=1,2,...,MAXSAT
CC               IFRMAT: FILE FORMAT NUMBER                     I*4
CC               IORBFL: ORBIT FLAG (1:BROADCAST,2:STD. ORBIT)  I*4
CC               IOUTLR: OUTLIER DETECTION (YES=1, NO=0)        I*4
CC               DIFMAX: MAXIMUM RESIDUAL DIFFERENCE TO "BEST   R*8
CC                       SATELLITE
CC               CONFID: CONFIDENCE INTERVAL FOR OUTLIERS       R*8
CC                       (E.G. 3.D0 FOR 3-SIGMA INTERVAL)
CC               MINDOF: MINIMAL ALLOWED DEGREE OF FREEDOM      I*4
CC               USRSIG: MAXIMAL RMS OF EPOCH SOLUTIONS         R*8
CC               IFILE : FILE NUMBER                            I*4
CC               MEATYP: OBS-TYPE (1=PHASE, 2=CODE, 3=range)    I*4
CC               NITER : MAXIMUM NUMBER OF ITERATIONS           I*4
CC               KINEST: ESTIMATION OF KINEMATIC COORDINATES    I*4
CC                         =0 NO KINEMATIC ESTIMATION
CC                         =1 ESTIMATE KINEMATIC COORDINATES
CC               LFNKOU: LFN FOR KINEMATIC OUTPUT FILE          I*4
CC                        (=0 NO FILE REQUEST, ELSE LFN002+3)
CC        OUT :  INDSVN(K): POINTER SVN --> INTERNAL NUMBER     I*4
CC                        K=1,2,...,MAXSVN
CC               OBSSAT(I,K): NUMBER OF OBSERVATION PER SAT.    I*4
CC                        I=1: TOTAL NUMBER
CC                        I=2: UNUSED
CC                        K=1,2,...,MAXSAT
CC               XSTANW(K): NEW RECTANGULAR STATION COORDINATES R*8
CC                        K=1,2,3
CC               XSTNEL(K): NEW ELLIPSOIDAL STATION COORDINATES R*8
CC                        IN SPECIFIED DATUM
CC                        K=1,2,3
CC               CLKMOD(K): NEW RECEIVER CLOCK PARAMETERS       R*8
CC                        K=1,2,...,ICLPOL
CC                        FOR MIXED GPS/GLONASS:
CC                        ICLPOL > 0:
CC                          CLKMOD(ICLPOL+1)=UTC-GLONASS
CC                        ICLPOL=-1:
CC                          CLKMOD(1)=UTC-GLONASS
CC               OFFS(IEPO),IEPO=1,MXCEPO: CLOCK OFFSET         R*8
CC               DOFFS(IEPO),IEPO=1,MXCEPO: CLOCK OFFSET
CC                   (PARAMETER CORRECTION OF LAST ITERATION
CC                    STEP --> USED FOR RESIDUAL COMPUTATION)   R*8
CC               SIGMA0 : SIGMA0                                R*8
CC               NORDIM : DIMENSION OF NORMAL EQU. SYSTEM       I*4
CC               DPAR(K): PARAMETER VECTOR                      R*8
CC                        K=1,2,...,MAXCOR+ICLPOL
CC               MCOR(K): RMS ERRORS OF COORDINATES             R*8
CC                        K=1,2,...,MAXCOR
CC               MELL(K): RMS ERRORS OF COORDINATES (ELL.)      R*8
CC                        K=1,2,...,MAXCOR
CC               MCLOCK(K): RMS ERRORS OF CLOCK PARAMETERS      R*8
CC                        K=1,2,...,ICLPOL
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
CC                               =4: MISSING SATELLITE CLOCKS
CC                               ALL AREAS MARKED OR CHANGED IN THE
CC                               LATEST RUN HAVE A NEGATIVE SIGN
CC               NITERF : NUMBER OF ITERATIONS PERFORMED        I*4
CC               IRCODE : RETURN CODE                           I*4
CC                        =0 : PROCESSING OK
CC                        =1 : NOT ENOUGH OBSERVATION
CC                        =2 : SATELLITE NUMBER OUT OF RANGE
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  T. SCHILDKNECHT, M. ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/30 08:11
CC
CC CHANGES    :  17-JUL-91 : ??: ALLOW STANDARD ORBITS AND SAT. CLOCK FILE
CC               23-MAY-92 : LM : WRITE A SCRATCH FILE FOR A-MATRIX
CC                                IN ANY CASE
CC               04-JUN-92 : ??: OPNFIL IMPLEMENTED; NEW CALL GETORB;
CC                               COOTRA WITH IORSYS FOR J2000.0
CC               07-AUG-92 : ??: SET RETURN CODE, IF PROBLEM
CC               03-MAR-93 : ??: NEW PARAMETERS "ICORR","ZEN" IN GPHECC
CC               28-OCT-93 : ??: MARKING OF SATELLITES WITH SATCRUX-FILE
CC                               ALLOW SATELLITES MISSING IN ORBIT-FILE
CC               21-NOV-93 : ??: MARK BAD OBSERVATIONS AND SAVE IN "LSTDEL"
CC               08-APR-94 : MR: ADD PARAM. "AZI" TO CALL OF GPHECC
CC               21-APR-94 : MR: NEW PARAMETER "SIGAPR" FOR SR SIGMA1
CC               10-AUG-94 : MR: CALL EXITRC
CC               17-AUG-94 : MR: MAXBAD=200 (OLD: MAXBAD=20)
CC               06-SEP-94 : MR: GTSCLK WITH RETURN CODE
CC               10-JUL-95 : TS: ADDED PERIODIC RELATIVITY
CC               28-AUG-95 : TS: CORRECT SAMPLING FOR ALL FILES
CC                6-SEP-95 : TS: CORRECT OBSERVATION EPOCH
CC                8-SEP-95 : TS: CORRECT HANDLING MILLISECOND JUMPS
CC               18-SEP-95 : TS: MARKING EPOCHS WITHOUT CLOCKS AFTER ZENMAX
CC               18-SEP-95 : TS: CORRECT ZENITH ANGLE
CC               28-SEP-95 : TS: HANDLE MEATYP=3: RANGES
CC                4-OCT-95 : TS: NO MRKOBS FOR FLGACT='C'
CC               30-OCT-95 : MR: SUBTRACT 3 FROM NPARMS, IF ICORFL=0
CC               13-DEC-95 : MR: NEW OPTION "MAX. NUMBER OF ITER."
CC               26-MAR-96 : MR: ADD "CSESS" AS PARAMETER
CC               24-JUN-96 : TS: ALLOW TROPOSPHERE ESTIMATES INPUT
CC               23-AUG-96 : TS: MAXBAD IN INCLUDE FILE
CC               08-APR-97 : SS: NIELL MAPPING, TROPOSPHERE GRADIENTS
CC               06-OCT-97 : HH: ADD GLONASS
CC               30-OCT-97 : DI: USE NEW FUNCTION "MIXSVN"
CC               04-OCT-00 : SS: CONSIDER NIELL A PRIORI TROPOSPHERE
CC                               MODEL
CC               06-JUN-01 : MR: USE FUNCTION "SVNSYS" INSTEAD
CC                               OF "MIXSVN"
CC               27-JUL-01 : DS: HANDLE KINEMATIC COORDINATES
CC               27-JUL-01 : DS: LEO ORBIT
CC               10-SEP-01 : HU: INITIALIZE ITRPMD FOR SR GETTRP
CC               09-FEB-02 : RD: P: REAL*4->REAL*8 (BECAUSE ADDNOR)
CC               05-MAR-02 : DS: LEO KINEMATIC POSITIONS: SR READKIN
CC               27-AUG-02 : DS: USE XSTANW FOR LEO
CC               28-AUG-02 : DS: AIRPLANE INTRODUCED
CC               28-AUG-02 : DS: ESTIMATION OF KINEMATIC COORDINATES
CC               28-AUG-02 : DS: USE MXCEPO TO LOOP OVER ALL EPOCHS
CC               29-AUG-02 : SS: CONSIDER DIFFERENTAIL CODE BIASES
CC               06-SEP-02 : MR: GOTO 500 CHANGED FOR SPACEBORNE
CC               18-SEP-02 : DS: KINEMATIC ESTIMATION:LEO,AIRPLANE,
CC                               SHIP,GROUND,STATIC GROUND
CC               16-DEC-02 : RD: USE MARK FLAG FROM OBSERV. FILE IS AN OPTION
CC                               RMSDIF IS AT LEAST DIFMAX IN 1ST ITERATION
CC               21-Jan-03 : RD: USE STA-SIGMA FOR SCALING OF OUTLIER LIMIT
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               23-APR-03 : RD: CLOSE KINOUT ONLY IF IT WAS OPENED
CC               06-MAY-03 : HU: OBSWGT ACTIVATED
CC               28-MAY-03 : RD: NEW CALL OF SR GPHECC
CC               23-JUN-03 : HB: INTERFACE FOR SR STAFLG
CC               07-Jul-03 : MM: NEW GETTRP CALL
CC               11-AUG-03 : RS: RECTYP=' ' IN 2 CALLS OF GPHECC
CC               31-JUL-03 : RD: OPEN/CLOSE KINOUT IN CODSPP (>1 STATION)
CC                               INTERPOLATE MISSING EPOCH IN KINOUT
CC               25-AUG-03 : AJ: KINEMATIC CASE COMPLETELY REVISED
CC               03-SEP-03 : RD: SEPERATE SCREENING PER SAT-SYSTEM IN THE FIRST
CC                               ITERATION OF THE STATIC MODE
CC               08-SEP-03 : HU: ANTNAM, RECNAM CHR16 -> CHR20
CC               29-MAR-04 : CU: GET WAVELENGTH FOR MARINI MURRAY FROM
CC                               FREQUENCY INFO FILE, CALL GETWAV
CC                               ADD WL TO CALL OF SR TROPOS
CC               28-DEC-04 : HU: USE HREF NOT HUMREF AS STATION REF HEIGHT
CC                               ANALYTICAL TRANSF. OF SATELLITE VELOCITY
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               13-DEC-05 : CU: ADAPT CALL OF SR TROPOS
CC               03-FEB-06 : RD: ENABLE MODULE FOR SIGMA1
CC               12-JUN-06 : HB: USE NEW SR OSAMPL FOR FINDING CORRECT
CC                               SAMPLING EPOCH
CC               05-JUL-06 : RD: >=2 OBSERV. HAVE TO BE EQUAL TO COMPUTE OFFS
CC               24-AUG-06 : AG: GMF IMPLEMENTED, TDELAY instead of TROPOS used
CC               02-OCT-06 : AG: ITROPO = 99 FOR "ESTIMATAED"
CC               15-NOV-06 : HB: INITIALIZE TESTSIGMA0
CC               12-JUN-07 : AG: USE STA_OFF INSTEAD OF GPHECC
CC               11-SEP-07 : HB: XSTANWS_SAV INTRODUCED TO AVOID ADD UP OF
CC                               PHASE CENTER OFFSETS
CC               01-NOV-07 : HB: SECIPL ADDED (MAX INTERVAL FOR CLOCK
CC                               INTERPOLATION)
CC               05-DEC-07 : HB: SMALL CORRECTION FOR LEO PROCESSING
CC                               (NEW PCV-FILE)
CC               26-FEB-08 : RD: USE GTSATB FROM D_SATCRX
CC               09-MAY-09 : RD: NEW CALL OF DCBCOR
CC               29-MAY-09 : RD: INPUT CLOCKS ALSO FROM INPUT CLK RNX FILE
CC               01-APR-10 : HB/AJ: CORRECT TIME ARGUMENT FOR SR GTLEOCO
CC               20-JUL-11 : PS/RD: APPLY GRADIENTS WITH CHEN-HERRING MAPPING,
CC                               ERROR FOR UNKNOWN GRADIENT MODEL
CC               09-SEP-11 : RD: CORRECT THE USAGE IN INPUT TRP-FILES
CC               11-NOV-11 : MM: TAKE SATELLITE PCOs FROM PCV FILE
CC               05-MAR-12 : RD: REMOVE UNUSED VARIBALES
CC               28-MAR-12 : RD: USE SVN2CHR AS MODULE NOW
CC               28-MAR-12 : RD: USE SVNSYS AS MODULE NOW
CC               10-JUL-12 : RD: USE SYMINVG INSTEAD OF SYMIN8
CC               10-JUL-12 : RD: REMOVE UNUSED VARIABLES FROM REDNAV
CC               10-JUL_12 : RD: REMOVE UNUSED LABELS/VARIABLES
CC               13-SEP-12 : RD: EMPTY MCLOCK FOR GPS-ONLY STATIONS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
C
C  DECLARATIONS
C  ------------
      USE m_bern,   ONLY: i4b, fileNameLength, lfnprt, lfnerr, lfn001
      USE m_maxdim, ONLY: MAXBAD, MAXSAT
      USE m_global, ONLY: G_SVNSYS
      USE d_stacrx, ONLY: MTypeSPACE
      USE d_const,  ONLY: C, OMEGA, PI
      USE d_phaecc, ONLY: sta_off,sat_off
      USE d_satfil, ONLY: typeMWTR
      USE d_satcrx, ONLY: gtsatb
C
      USE s_getwav
      USE s_rdobsi
      USE s_opnfil
      USE f_sigma1
      USE f_tstflg
      USE s_mrkobs
      USE s_mjdgps
      USE s_cootra
      USE s_leoante
      USE s_major1
      USE s_err3d
      USE s_wgtsta
      USE s_gettrp
      USE s_svn2chr
      USE s_clrflg
      USE s_truearth
      USE s_getorb
      USE s_opnerr
      USE s_dcbcor
      USE s_maxtst
      USE s_staflg
      USE s_eccell
      USE s_setflg
      USE s_solve
      USE s_xyztim
      USE s_syminvg
      USE s_readkin
      USE f_svnsys
      USE s_rednav
      USE s_rfiapl
      USE s_exitrc
      USE s_gtleoco
      USE s_ellecc
      USE s_addnor
      USE s_gtflna
      USE s_gtsclk
      USE s_brceph
      USE s_xyzell
      USE s_trpmap
      USE s_tdelay
      USE s_osampl
      USE s_gnssatti
C
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I       , IBAD    , IBDSAT  , ICLPO1  , ICLPOL  ,
     1          ICORFL  , ICRARC  , IDELTT  , IDER    ,
     2          IDUMMY  , IEPO    , IEPOCH  , IFILE   , IFIRST  ,
     3          IFLAG   , IFRMAT  , II      , IK      , INDACT  ,
     4          IONO    , IORBFL  , IORSYS  , IORSYSL ,
     5          IOSTAT  , IOUTLR  , IP      , IRC     , IRC1    ,
     6          IRC2    , IRC3    , IRCEPO2 , IRCEPOS ,
     7          IRCLK   , IRCOD   , IRCODE  , IRCTRP  , IRETRN  ,
     8          IRFSAT  , ISAMAX  , ISAT    , ISING   , ISYS    ,
     9          ITER    , ITRGRD  , ITRMAP  , ITROPO  , ITRPMD  ,
     1          JPAR    , JSCR    , JTER    , K       , KINEST  ,
     2          KK      , LFNKOU  , LFNSCR  , MAXEQN  , MINDOF  ,
     3          MPAR    , MXCCLK  , MXCCOR  , MXCDEL  , MXCEPH  ,
     4          MXCEPO  , MXCFIL  , MXCSAT  , MXCSVN  , MXLCLK  ,
     5          MXLCOR  , N1PAR   , NBAD    , NCOPAR  , NDEL    ,
     6          NDEL0   , NDOF0   , NITACT  , NITER   , NITERF  ,
     7          NJTACT  , NOBSEQ  , NORDIM  , NPARMS  , NPARN   ,
     8          NREC    , NREC0   , NREPO   , NSACT   , NSAMPL  ,
     9          NSARMS  , NSATEL  , NSATOK  , NSCR    , NSINGE  ,
     1          NTSAT   , NWEEK   , NWEEK1  , metex
C
      REAL*8    AELL    , AZIMUT  , BELL    , BESTRMS ,
     1          CLOCKC  , CONFID  , CSZ     , DERIV   , DIFMAX  ,
     2          DMAX    , DMEAN1  , DPTEST  , DRIONO  , DRTROP  ,
     3          DSQRT2  , DTREC   , DTREC1  , DTSIM   , DUMMY   ,
     4          EPS     , H       , OBSEPO  , OBSTIM  , OBSWGT  ,
     5          P       , RELATIVE, RHO     , RHOACT  , RMSDIF  ,
     6          RMSSUM  , SCAL    , SCELL   , SECOND  , SECOND1 ,
     7          SECSAV  , SIGAPR  , SIGMA0  , SMPINT  ,
     8          SSZ     , SZ      , SZ1     , T       , TEST    ,
     9          TIMREF  , TLAST   , TOBS    , TOBS0   ,
     1          TOBS1   , TOLCRD  , TOSC    , TREL    , USRSIG  ,
     2          UT1GPS  , UTCDER  , UTCGL   , WGTGEN  , XL12    ,
     3          XP1     , XPOL    , YP1     , YPOL    , MAZ     ,
     4          ZENITH  , ZENMAX  , TSAMPL  , TPREV   , TNEXT   ,
     5          SECIPL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MXLCOR=3,MXLCLK=10)
C
C
      REAL*8 OBSWIN(2),PHAECC(3),OFFS(MXCEPO),DOFFS(MXCEPO)
      REAL*8 XSTAT(3),XSTELL(3),POSECC(3,2),EXC(3),DXELL(3),DRELL(3)
      REAL*8 XSTANW(9),XSTNEL(3),xloc(3)
      REAL*8 CLKMOD(MXCCLK)
      REAL*8 AMAT(MXLCOR+MXLCLK,MAXSAT),DPHI(MAXSAT),DPHIMJ(MAXSAT)
      REAL*8 AZEILE(MXLCOR+MXLCLK)
      REAL*8 MCLOCK(MXCCLK),DPAR(MXCCOR+MXCCLK)
      REAL*8 MCOR(MXCCOR),MELL(MXCCOR)
      REAL*8 B(MXLCOR+MXLCLK),OBSERV(MAXSAT)
      REAL*8 XSAT(3),XSAT0(3),VSAT(3),VREL(3),TOPDER(3),CLKDER(MXLCLK)
      REAL*8 EPH(20*MXCEPH,MXCSAT),CLOCK(20*MXCEPH,MXCSAT)
      REAL*8 XSTOPO(3),ELEACT(MAXSAT)
      REAL*8 CM(3,3),DM(3,3),DELTAT(2)
      REAL*8 BNOR(MXLCOR+MXLCLK)
      REAL*8 ANOR((MXLCOR+MXLCLK)*((MXLCOR+MXLCLK)+1)/2)
      REAL*8 A0I(4),AII(1),HELP(1),VSIG(MXLCOR+MXLCLK)
      REAL*8 ELE(7),XXSAT(6),X1SSAT(3),V1SSAT(3)
      REAL*8 X1PSAT(3),V1PSAT(3),TIMBAD(2,MAXBAD),DRTEST(3),MAPFUN(2)
      REAL*8 XKIN(9),XKIN2(9),TRUOFF(3)
      REAL*8 XDUMMY(7)
      REAL*8 RMSMIN(5),RMSMAX(5)
      REAL*8 RMSMEAN(5)
      REAL*8 XELEPO(3)
      REAL*8 XKIN0(3),XKIN1(3)
      REAL*8 OFFSET(3),EX(3),EY(3),EZ(3)
C
      REAL*8 TESTSIGMA0(MXCSAT),TESTXSTANW(MXCSAT,3)
      REAL*8 TESTOFFS(MXCSAT,MXCEPO),TESTMCOR(MXCSAT,MXCCLK)
      REAL*8 TESTAMAT(MXCSAT,MXLCOR+MXLCLK,MAXSAT)
      REAL*8 TESTMCLOCK(MXCSAT,MXCCLK)
      REAL*8 TESTDPHI(MXCSAT,MAXSAT)
      REAL*8 TESTANOR(MXCSAT,((MXLCOR+MXLCLK)*(MXLCOR+MXLCLK+1))/2)
      REAL*8 TESTDPAR(MXCSAT,MXCCOR+MXCCLK)
      REAL*8 XSTANWHELP(3)
      REAL*8 XSTATA(3),XLAST(3),XSTANWT(9)
      REAL*8 XSTANWS(9),XSTANWS_SAV(3)

      REAL*8 PMAT(1)
C
      REAL*8 WL
C
      INTEGER(i4b),SAVE  :: IRCSTD,IRCKIN
C
      INTEGER*4 PRIOPT(*),FILFRQ,SATNUM(MXCSAT),SATNRA(MAXSAT)
      INTEGER*4 INDSVN(MXCSVN),OBSSAT(2,MXCSAT)
      INTEGER*4 INDP(MXLCOR+MXLCLK)
      INTEGER*4 IANTEN(2)
      INTEGER*4 IOBBAD(MAXBAD),IACBAD(MAXBAD),SATBAD(MAXBAD)
      INTEGER*4 LSTDEL(5,*),MEATYP,USEMRK
C
      INTEGER*4 INDA(MXLCOR+MXLCLK),INDI(1),INDK(1)
C
      CHARACTER(LEN=fileNameLength),SAVE :: FILKIN
      CHARACTER(LEN=fileNameLength)      :: OBSFIL,SCRFIL,FILCRX,FILTRP
      CHARACTER(LEN=fileNameLength)      :: FILSTD
      CHARACTER*20 MARTYP
      CHARACTER*20 RECTYP(2),ANTTYP(2)
      CHARACTER*16 STANAM
      CHARACTER*6  MXNSAT,MXNSVN,MXNEPH,MXNCLK,MXNCOR,MXNFIL
      CHARACTER*6  MXNEPO,MXNDEL
      CHARACTER*4  CSESS(2)
      CHARACTER*1  OBSFLG(MAXSAT),FLGACT(MAXSAT),EPOFLG,SVNCHR
C
      CHARACTER*2  FRQID
C
      LOGICAL*4 FLGMIX,MIXMRK
C
C  COMMON FOR CONSTANTS AND MAXIMAL DIMENSIONS
C  -------------------------------------------
      INCLUDE 'COMFREQ.inc'
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMSVN/MXCSVN,MXNSVN
      COMMON/MCMEPH/MXCEPH,MXNEPH
      COMMON/MCMCLK/MXCCLK,MXNCLK
      COMMON/MCMCOR/MXCCOR,MXNCOR
      COMMON/MCMFIL/MXCFIL,MXNFIL
      COMMON/MCMEPO/MXCEPO,MXNEPO
      COMMON/MCMDEL/MXCDEL,MXNDEL
C
      DATA IFIRST/1/
      DATA TOLCRD/0.5D0/
      DATA EPS/0.000001D0/
C
C INITIALIZATION
C --------------
      IF (IFIRST.EQ.1) THEN
        DSQRT2=DSQRT(2.D0)
C
C CHECK DIMENSIONS
C ----------------
        CALL MAXTST(1,'CDCOMP',MXNSAT,MAXSAT,MXCSAT,IRC1)
        CALL MAXTST(0,'CDCOMP',MXNCOR,MXLCOR,MXCCOR,IRC2)
        CALL MAXTST(1,'CDCOMP',MXNCLK,MXLCLK,MXCCLK,IRC3)
        IF(IRC1+IRC2+IRC3.NE.0) CALL EXITRC(2)
C
C GET BAD SATELLITE INTERVALS FROM SATELLITE PROBLEM FILE
C -------------------------------------------------------
        CALL GTFLNA(0,'SATCRUX',FILCRX,IRC)
        CALL GTSATB(MAXBAD,FILCRX,NBAD,SATBAD,IOBBAD,IACBAD,TIMBAD)
C
C GET LEO ORBIT INFORMATION
C -------------------------
        CALL GTFLNA(0,'LEOSTD ',FILSTD,IRCSTD)
        CALL GTFLNA(0,'KININP ',FILKIN,IRCKIN)
        TESTSIGMA0=0.D0
      ENDIF
C
C LOGICAL FILE NUMBERS FOR SCRATCH FILES
C --------------------------------------
      LFNSCR = LFN001 + 10
C
      WGTGEN  = 1.D0
      PMAT(1) = 1.D0
      MAXEQN  = 1
      IRCLK   = 0
C
      MIXMRK=.FALSE.
      FLGMIX=SVNSYS(-1,NSATEL,SATNUM)
C
C CHECK POLYNOMIAL DEGREE FOR MIXED OBSERVATION
C ---------------------------------------------
      IF (FLGMIX.AND.ICLPOL.GT.0) THEN
        IF (ICLPOL+1.GT.MXLCLK) THEN
          WRITE(LFNERR,17)ICLPOL,MXLCLK-1
17        FORMAT(/' *** SR CDCOMP: POLYNOMIAL DEGREE NOT ALLOWED'/
     1                      16X,'FOR MIXED OBSERVATION'/
     2                      16X,'POLYNOMIAL:',I3,/
     3                      16X,'MAX. ALLOWED:',I3,/)
          CALL EXITRC(2)
        ENDIF
      ENDIF
C
C ONLY GPS SATELLITES IN KINEMATIC CASE
C -------------------------------------
      IF(FLGMIX.AND.KINEST.EQ.1) THEN
        WRITE(LFNERR,19)
19      FORMAT(/' ### SR CDCOMP: MIXED OBSERVATIONS NOT ALLOWED'/
     1                      16X,'IN KINEMATIC CASE:'/
     2                      16X,'ONLY GPS OBSERVATIONS USED'/)
        FLGMIX=.FALSE.
        MIXMRK=.TRUE.
      END IF
C
C DEFINE CONSTANTS
C ----------------
      NCOPAR=MXLCOR
      IF (ICLPOL .GT. 0) THEN
        NORDIM = ICLPOL + NCOPAR
        ICLPO1 = ICLPOL
      ELSE
        NORDIM = NCOPAR + 1
        ICLPO1 = 1
      ENDIF
      IF (FLGMIX) THEN
        NORDIM=NORDIM+1
      ENDIF
      IFIRST = 1
C
C INIT ARRAYS (OUTSIDE ITERATION LOOP)
C ------------------------------------
C
C INIT INDEX ARRAY SVN --> SATNUM(I)
      DO 20 I=1,MXCSVN
        INDSVN(I)=0
20    CONTINUE
      DO 30 I=1,NSATEL
        IF(SATNUM(I).GT.MXCSVN) THEN
          WRITE(LFNERR,21)SATNUM(I),MXCSVN
21        FORMAT(/' *** SR CDCOMP: SATELLITE NUMBER OUT OF RANGE',/,
     1                        16X,'SATELLITES NUMBER:',I3,/,
     2                        16X,'MAX. SAT. NUMBER :',I3,/)
          IRCODE=2
          GOTO 999
        ENDIF
        INDSVN(SATNUM(I))=I
30    CONTINUE
C
C APPLY POSECC TO STATION COORDINATES
      CALL ELLECC(XSTELL,POSECC(1,1),EXC)
      DO 40 K=1,3
        XSTATA(K)=XSTAT(K)+EXC(K)
40    CONTINUE
C
C INIT RECEIVER CLOCK MODEL PARAMETERS
      DO 50 K=1,ICLPO1
        CLKMOD(K)=0.D0
50    CONTINUE
      CLKMOD(ICLPO1+1)=0.D0
      UTCGL=0.D0
C
C INITIALIZE CLOCK OFFSETS
      DO 55 IEPO = 1,MXCEPO
        OFFS(IEPO)  = 0.D0
        DOFFS(IEPO) = 0.D0
55    CONTINUE
C
C SAMPLING INTERVAL (SEC)
      SMPINT=IDELTT*NSAMPL
C
C SAVE LAST MARKED AREA OF LAST FILE PROCESSED (FOR RE-INITIALIZATION)
      NDEL0=NDEL
C
C OPEN FILES
C ----------
C
C OBS FILE
      CALL OPNFIL(LFN001,OBSFIL,'OLD','UNFORMATTED',
     1            'READONLY',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFN001,IOSTAT,OBSFIL,'CDCOMP')
C
C SCRATCH FILE FOR ELEVATIONS AND A-MATRIX
      CALL GTFLNA(1,'AUXFIL ',SCRFIL,IRC)
      CALL OPNFIL(LFNSCR,SCRFIL,'UNKNOWN','UNFORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNSCR,IOSTAT,SCRFIL,'CDCOMP')

C
C KINEMATIC STATISTICS
C --------------------
      IF (KINEST.EQ.1) THEN
        RMSMIN(1:5)  = 1.D20
        RMSMAX(1:5)  =-1.D20
        RMSMEAN(1:5) = 0.D0
        NREPO  =0
        NSINGE =0
        NDOF0  =0
      END IF
C
C DIFFERENT ITERATIONS FOR STATIC/KINEMATIC CASE
C ----------------------------------------------
      NSCR = 1
      IF(KINEST.EQ.0) THEN
        NITACT = NITER
        NJTACT = 1
      ELSEIF(KINEST.EQ.1) THEN
        NITACT = 1
        NJTACT = NITER
        IF(IOUTLR.EQ.1) NSCR   = 10
      END IF
C
C ITERATION LOOP FOR STATIC CASE
C ------------------------------
      DO 400 ITER=1,NITACT
        JPAR   = 0
        NOBSEQ = 0
        RMSSUM = 0.D0
        NDEL   = NDEL0
C
C INIT. ARRAYS FOR OBS.- AND NORMAL EQUATION SYSTEMS
C --------------------------------------------------
        IF(KINEST.EQ.0) THEN
          MPAR = NORDIM*(NORDIM+1)/2
          DO 80 I=1,NORDIM
            DPAR(I) = 0.D0
            BNOR(I) = 0.D0
80        CONTINUE
          DO 85 K=1,MPAR
            ANOR(K) = 0.D0
85        CONTINUE
        END IF
C
C IF NO COORD. ESTIMATION REQUESTED ANOR(II)=1.D20, I=1,2,3
        IF(ICORFL.EQ.0.AND.KINEST.EQ.0) THEN
          IK = 0
          DO 90 I=1,3
            IK=IK+I
            ANOR(IK)=1.D20
90        CONTINUE
        ENDIF
C
C INIT STATISTICS
        DO 95 I=1,NSATEL
          OBSSAT(1,I) = 0
          OBSSAT(2,I) = 0
95      CONTINUE
C
C LOOP OVER EPOCHS
C ----------------
        DO 300 NREC=1,MXCEPO
C
C OFFSET PER EPOCH  --> INITIALIZE REDUCED PART OF NEQ-SYSTEM
          IF (KINEST.EQ.0.AND.ICLPOL .EQ. -1) THEN
            IF (FLGMIX) THEN
              DO 98 IP = 11,15
                ANOR(IP) = 0.D0
98            CONTINUE
              BNOR(5) = 0.D0
            ELSE
              DO 99 IP = 7,10
                ANOR(IP) = 0.D0
99            CONTINUE
              BNOR(4) = 0.D0
            ENDIF
          ENDIF
C
C READ ALL OBSERVATIONS OF ONE EPOCH
C ----------------------------------
100       CONTINUE
          CALL RDOBSI(LFN001,IFRMAT,1,(/FILFRQ/),OBSTIM,DELTAT,EPOFLG,
     1                NSACT,SATNRA,OBSFLG,OBSERV,IRETRN)
C
C END OF FILE
          IF (KINEST.EQ.1 .AND. IRETRN.NE.0) GOTO 390
          IF(IRETRN.NE.0) GOTO 310
C
C RESET MARKING IN THE OBSERVATION FILE
C -------------------------------------
          IF (USEMRK.NE.1) THEN
            DO ISAT=1,NSACT
              CALL CLRFLG(OBSFLG(ISAT),0)
            ENDDO
          ENDIF
C
C REFERENCE EPOCH
          IF (IFIRST .EQ. 1) THEN
            IFIRST = 0
          ENDIF
C
C OBSERVATION EPOCH
          OBSEPO = OBSTIM + DELTAT(1)/86400.d0
C
C OBSERVATION WINDOW
          IF(OBSTIM.LT.OBSWIN(1).OR.OBSTIM.GT.OBSWIN(2)) GOTO 100
C
C SAMPLING
          IF (NSAMPL.NE.1) THEN
            CALL OSAMPL(OBSTIM,SMPINT,1D0,TSAMPL,TPREV,TNEXT)
            IF (TSAMPL.EQ.0D0) THEN
              GOTO 100
            ENDIF
          ENDIF
C
C COMPUTE EPOCH NUMBER
          IEPOCH = IDNINT(((OBSTIM-TIMREF)*86400.D0)/IDELTT) + 1
C
C REMOVE 0.D0 OBSERVATIONS
          II=1
          DO 105 I=1,NSACT
            IF(OBSERV(I).NE.0.D0) THEN
              OBSERV(II)=OBSERV(I)
              SATNRA(II)=SATNRA(I)
              OBSFLG(II)=OBSFLG(I)
              II=II+1
            ENDIF
105       CONTINUE
          NSACT=II-1
C
C REC. CLOCK ERROR
C ----------------
          TREL=(OBSEPO-TIMREF)*86400.D0
          DTREC=0.D0
          IF (ICLPOL .GT. 0) THEN
            IF(TREL.EQ.0.D0) THEN
              DTREC=CLKMOD(1)
            ELSE
              DO 110 I=1,ICLPOL
                DTREC=DTREC+CLKMOD(I)*TREL**(I-1)
110           CONTINUE
            ENDIF
          ELSE
            DTREC=OFFS(IEPOCH)
          ENDIF
          DTREC1=DTREC
C
C OBSERVATION TIME (A PRIORI)
          TOBS=OBSEPO-(DTREC1)/86400.D0
C
C HANDLE DIFFERENT STATION TYPES
C ------------------------------
          CALL STAFLG(STANAM,OBSEPO,IFLAG,MARTYP)
C
          IF (MARTYP.NE.MTypeSPACE) THEN
C
C USE EXISTING STATION COORDINATES
            IF(ITER.EQ.1) XSTANW(1:3) = XSTATA(1:3)
C
C GET A PRIORI COORDINATES FROM KIN. INPUT FILE
            IF (IRCKIN==0) THEN
              CALL  READKIN(FILKIN,STANAM,TOBS,
     1                      1-KINEST,0,XSTANW(1:3),IRCEPOS)
C
              IF (IRCEPOS.EQ.0) THEN
                CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,XSTANW,XELEPO)
                CALL ELLECC(XELEPO,POSECC(1,1),EXC)
                XSTANW(1:3)=XSTANW(1:3)+EXC(1:3)
C
C KINEMATIC STATION; BUT NO VALUE FOR THIS EPOCH
              ELSE IF (IRCEPOS.EQ.1.AND.KINEST.EQ.0) THEN
                DO ISAT=1,NSACT
                  CALL SETFLG(OBSFLG(ISAT),0)
                END DO
              END IF
            END IF
          END IF
C
C LEO
C ---
          IF (MARTYP.EQ.MTypeSPACE) THEN
             XSTAT(1:3)=0.D0

C
C GET LEO A PRIORI COORDINATES FROM KIN. INPUT FILE
C -------------------------------------------------
            XKIN(1:9)=0.D0
            XKIN2(1:9)=0.D0
C
            XKIN(4:6)=1.D20
            TRUOFF(1:3)=0.D0
C
            IRCEPO2=1
            IF (IRCKIN==0) THEN
              CALL  READKIN(FILKIN,STANAM,TOBS,
     1                      0,0,XKIN2(1:3),IRCEPO2)
C
C TRANSFORMATION IN TRUE SYSTEM OF EPOCH
              IF (IRCEPO2==0) THEN
                XDUMMY(1:3)=0.0D0
                IORSYSL=2
                CALL COOTRA(IORSYSL,0,TOBS,XDUMMY,
     1                      SZ,XPOL,YPOL,UT1GPS)
                CALL TRUEARTH(XKIN2(1:3),XKIN(1:3),1,0,SZ,XPOL,YPOL)
              END IF
            END IF
C
C GET LEO A PRIORI COORDINATES FROM THE ORBIT
C -------------------------------------------
            IF (IRCSTD==0) THEN
              CALL GTLEOCO(STANAM,OBSEPO-(DTREC)/86400.D0,1,2,
     1                     XKIN2,SZ,XPOL,YPOL,IRCOD)
              IF (IRCOD==0) THEN
                IF (IRCEPO2.NE.0) XKIN(1:3)=XKIN2(1:3)
                XKIN(4:6)=XKIN2(4:6)
              END IF
            END IF
C
            IF((IRCKIN.EQ.0.AND.IRCEPO2.EQ.0).OR.
     1        (IRCSTD.EQ.0.AND.IRCOD.EQ.0)) THEN
C
C TRANSFORMATION INTO EARTH FIXED SYSTEM
              CALL TRUEARTH(XKIN2,XSTANW,0,0,SZ,XPOL,YPOL)
            END IF
          END IF
C
C SCREENING LOOP (KINEMATIC CASE)
C  -------------------------------
          DO 3100 JSCR=1,NSCR
C
C SAVE LATEST VALUES FOR STATION COORDINATES
            XSTANWHELP(1:3) = XSTANW(1:3)
C
C START NEW SCREENING PROCEDURE
C -----------------------------
            IF(JSCR.NE.1.AND.KINEST.EQ.1) THEN
              IF((OBSWGT*USRSIG).GT.SIGMA0.OR.ISING.NE.0) GOTO 3105
              NTSAT=NSACT
            ELSE
              NTSAT=1
            END IF
C
C TEST ALL NSACT SATELLITES
C -------------------------
            DO 2100 IBDSAT=1,NTSAT
              IF(JSCR.NE.1.AND.KINEST.EQ.1) THEN
                IF(FLGACT(IBDSAT).EQ.'G') THEN
                  FLGACT(IBDSAT) = 'B'
                ELSE
                  CYCLE
                END IF
              END IF
C
C GET LATEST A PRIORI STATION COORDINATES
              XSTANW(1:3) = XSTANWHELP(1:3)
              IF(KINEST.EQ.1) OFFS(IEPOCH) = 0.D0
C
C ITERATION LOOP FOR KINEMATIC COORDINATE ESTIMATION
C --------------------------------------------------
              DO 1100 JTER=1,NJTACT
                ISING  = 0
C
C INITIALIZE VARIABLES FOR EACH ITERATION (KINEMATIC CASE)
                IF(KINEST.EQ.1) THEN
                  DTREC=OFFS(IEPOCH)
                  DTREC1=DTREC
C
                  NOBSEQ=0
                  RMSSUM=0.D0
                  MPAR = NORDIM*(NORDIM+1)/2
                  DO 1180 I=1,NORDIM
                    DPAR(I) = 0.D0
                    BNOR(I) = 0.D0
1180              CONTINUE
                  DO 1185 I=1,MPAR
                    ANOR(I) = 0.D0
1185              CONTINUE
                END IF
C
C APPLY CORRECTION FOR SOLID EARTH TIDES
                IF (MARTYP.NE.MTypeSPACE) THEN
                  IORSYS=2
                  CALL xyztim(IORSYS,OBSEPO-DTREC/86400.D0,XSTANW,
     1                        STANAM,XSTANWS(1:3))
                ELSE
                  XSTANWS = XSTANW
                END IF
                XSTANWS(4:9)=0.D0
C
C NEW ELLIPSOIDAL A PRIORI STATION COORDINATES
                CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,
     1                      XSTANWS,XSTNEL)
                xstanws_sav(1:3)=XSTANWS(1:3)
C
C LOOP OVER SATELLITES
C --------------------
                DO 200 ISAT=1,NSACT
C
C APPLY ANTENNA OFFSETS
                  CALL sta_off(ANTTYP(1),IANTEN(1),STANAM,SATNRA(ISAT),
     1                         FILFRQ,CSESS(1),PHAECC)
                  IF (MARTYP.EQ.MTypeSPACE) THEN
                    CALL TRUEARTH(XSTANW(1:3),XKIN(1:3),1,0,SZ,XPOL,
     1                                                             YPOL)
                    CALL LEOANTE(STANAM,OBSEPO,PHAECC,0,XKIN,TRUOFF)
                    XKIN2(1:3)=XKIN(1:3)+TRUOFF(1:3)
                    CALL TRUEARTH(XKIN2,XSTANWS,0,0,SZ,XPOL,YPOL)
                  ELSE
                    CALL ELLECC(XSTNEL,PHAECC,EXC)
                    DO I=1,3
                      XSTANWS(1:3)=XSTANWS_SAV(1:3)+EXC(1:3)
                    END DO
                  ENDIF
C
C FLGMIX=.TRUE.: ADD UTCGL TO DTREC FOR GLONASS SAT. (ONLY IN STATIC CASE)
C ------------------------------------------------------------------------
                  IF (FLGMIX) THEN
                    IF (IDINT(SATNRA(ISAT)/100.D0).EQ.1) THEN
                      DTREC=DTREC1+UTCGL
                    ELSE
                      DTREC=DTREC1
                    ENDIF
                  ENDIF
C
C MARKED OBSERVATION (ONLY FOR FIRST ITERATION JSCR = 1)
C ------------------------------------------------------
                  IF(JSCR.EQ.1) THEN
                    IF(OBSERV(ISAT).EQ.0.D0) THEN
                      FLGACT(ISAT)=' '
                      GOTO 200
                    ENDIF
C
                    IF(TSTFLG(OBSFLG(ISAT),0)) THEN
                      FLGACT(ISAT)='M'
                    ELSE
                      FLGACT(ISAT)='G'
                    ENDIF
C
C CHECK SATELLITE PROBLEM FILE FOR BAD SATELLITE
                    DO 115 IBAD=1,NBAD
                      IF (SATBAD(IBAD).EQ.SATNRA(ISAT).AND.
     1                    TIMBAD(1,IBAD).LE.OBSEPO    .AND.
     2                    TIMBAD(2,IBAD).GT.OBSEPO    .AND.
     3                    (IOBBAD(IBAD).EQ.2          .OR.
     4                     IOBBAD(IBAD).EQ.3)) THEN
                        FLGACT(ISAT)='B'
                        CALL MRKOBS(3,SATNRA(ISAT),IEPOCH,IFILE,
     1                              NSATEL,SATNUM,NDEL,LSTDEL)
                      ENDIF
115                 CONTINUE
C
C MARK GLONASS SATELLITES IN KINEMATIC CASE
                    IF (MIXMRK) THEN
                      IF (IDINT(SATNRA(ISAT)/100.D0).EQ.1) THEN
                        FLGACT(ISAT)='X'
                        GOTO 200
                      END IF
                    ENDIF
C
                  END IF
C
                  IF (IORBFL .EQ. 1) THEN
C
C ACTUAL SATELLITE COORDINATES (USING BROADCAST MESSAGES)
C (ACTUAL EPOCH = (OBS. EPOCH - REC. CLK. ERR.))
C ----------------------------------------------
                    CALL BRCEPH(OBSEPO-(DTREC)/86400.D0,
     1                          SATNRA(ISAT),XSAT,CLOCKC,EPH,CLOCK,IRC)
                    IF (IRC.NE.0) THEN
                      FLGACT(ISAT)='X'
                      CALL MRKOBS(2,SATNRA(ISAT),IEPOCH,IFILE,
     1                            NSATEL,SATNUM,NDEL,LSTDEL)
                      GOTO 200
                    ENDIF
C
C ACTUAL TOPOCENTRIC SATELLITE COORDINATES
C SATELLITE VELOCITY
C ----------------------------------------
                    CALL BRCEPH(OBSEPO-(DTREC+1.D-1)/86400.D0,
     1                          SATNRA(ISAT),XSAT0,DUMMY,EPH,CLOCK,IRC)
                    IF (IRC.NE.0) THEN
                      FLGACT(ISAT)='X'
                      CALL MRKOBS(2,SATNRA(ISAT),IEPOCH,IFILE,
     1                            NSATEL,SATNUM,NDEL,LSTDEL)
                      GOTO 200
                    ENDIF
                    DO 120 I=1,3
                      VSAT(I)=(XSAT(I)-XSAT0(I))/1.D-1
                      XSTOPO(I)=XSAT(I)-XSTANWS(I)
120                 CONTINUE
C
                  ELSE IF (IORBFL .EQ. 2) THEN
C
C ACTUAL SATELLITE COORDINATES (USING STANDARD ORBITS)
C ----------------------------------------------------
                    IDER = 1
                    CALL GETORB(SATNRA(ISAT),1,IDER,2,
     1                          OBSEPO-(DTREC)/86400.D0,ICRARC,
     2                          IORSYS,XXSAT,TOSC,ELE,IRC)
                    IF (IRC.NE.0) THEN
                      FLGACT(ISAT)='X'
                      CALL MRKOBS(2,SATNRA(ISAT),IEPOCH,IFILE,
     1                            NSATEL,SATNUM,NDEL,LSTDEL)
                      GOTO 200
                    ENDIF
C
C APPLY SATELLITE ANTENNA PHASE CENTER OFFSET
C -------------------------------------------
                    CALL SAT_OFF(SATNRA(ISAT),OBSEPO-(DTREC)/86400.D0,
     1                           typeMWTR,FILFRQ,OFFSET)
                    CALL GNSSATTI(IORSYS,SATNRA(ISAT),
     1                            OBSEPO-(DTREC)/86400.D0,0,XXSAT,
     2                            EX_SAT=EX,EY_SAT=EY,EZ_SAT=EZ)
                    DO K = 1,3
                      XXSAT(K)=XXSAT(K)+EX(K)*OFFSET(1)
     1                                 +EY(K)*OFFSET(2)
     2                                 +EZ(K)*OFFSET(3)
                    ENDDO
C
C TRANSFORM SATELLITE INTO EARTH-FIXED SYSTEM
C SATELLITE POSITION
                    CALL COOTRA(IORSYS,IDER,OBSEPO-(DTREC)/86400.D0,
     1                          XXSAT,SZ1,XP1,YP1,UT1GPS)
C
                    SSZ=DSIN(SZ1)
                    CSZ=DCOS(SZ1)
                    X1SSAT(1)= CSZ*XXSAT(1)+SSZ*XXSAT(2)
                    X1SSAT(2)=-SSZ*XXSAT(1)+CSZ*XXSAT(2)
                    X1SSAT(3)=                         XXSAT(3)
C
                    X1PSAT(1)=     X1SSAT(1)              +XP1*X1SSAT(3)
                    X1PSAT(2)=                   X1SSAT(2)-YP1*X1SSAT(3)
                    X1PSAT(3)=-XP1*X1SSAT(1)+YP1*X1SSAT(2)    +X1SSAT(3)
C
C SATELLITE VELOCITY
                    V1SSAT(1)= CSZ*XXSAT(4)+SSZ*XXSAT(5)
                    V1SSAT(2)=-SSZ*XXSAT(4)+CSZ*XXSAT(5)
                    V1SSAT(3)=                         XXSAT(6)
C
                    V1PSAT(1)=     V1SSAT(1)              +XP1*V1SSAT(3)
                    V1PSAT(2)=                   V1SSAT(2)-YP1*V1SSAT(3)
                    V1PSAT(3)=-XP1*V1SSAT(1)+YP1*V1SSAT(2)    +V1SSAT(3)
C
C TOPOCENTRIC SATELLITE VECTOR / SATELLITE VELOCITY
                    DO 125 I=1,3
                      XSTOPO(I) = X1PSAT(I) - XSTANWS(I)
                      VSAT(I)=V1PSAT(I)
                      IF(I.EQ.1) VSAT(I)=VSAT(I)+OMEGA*X1PSAT(2)
                      IF(I.EQ.2) VSAT(I)=VSAT(I)-OMEGA*X1PSAT(1)
125                 CONTINUE
C
C GET SATELLITE CLOCK CORRECTION
C ------------------------------
                    CALL GTSCLK(-1,OBSEPO-(DTREC)/86400.D0,
     1                          SATNRA(ISAT),SECIPL,2,CLOCKC,IRClk)
C
                  ENDIF
C
C APPROXIMATE ZENITH DISTANCE AND AZIMUTH ANGLE
C ---------------------------------------------
                  CALL ECCELL(XSTNEL,XSTOPO,XLOC)
                  XL12=DSQRT(XLOC(1)**2+XLOC(2)**2)
                  IF (XL12.NE.0.D0 .OR. XLOC(3).NE.0.D0) THEN
                    ZENITH=DATAN2(XL12,XLOC(3))
                  ELSE
                    ZENITH=0.D0
                  ENDIF
C
                  IF (XLOC(1).NE.0.D0 .OR. XLOC(2).NE.0.D0) THEN
                    AZIMUT=DATAN2(XLOC(2),XLOC(1))
                  ELSE
                    AZIMUT=0.D0
                  ENDIF
                  IF (AZIMUT.LT.0.D0) AZIMUT=AZIMUT+2*PI
C
                  IF(ZENITH.LT.ZENMAX .AND. IRCLK.NE.0) THEN
                    FLGACT(ISAT)='C'
                    GOTO 200
                  ENDIF
C
C TOPOCENTRIC SATELLITE DISTANCE
C ------------------------------
                  SCAL=0.
                  RHOACT=0.
                  RELATIVE=0.D0
                  DO 130 I=1,3
                    RHOACT=RHOACT+XSTOPO(I)*XSTOPO(I)
                    VREL(I)=VSAT(I)/C
                    IF(I.EQ.1) VREL(I)=VREL(I)-OMEGA*XSTANWS(2)/C
                    IF(I.EQ.2) VREL(I)=VREL(I)+OMEGA*XSTANWS(1)/C
                    SCAL=SCAL+VREL(I)*XSTOPO(I)
                    IF (IORBFL .EQ. 1) THEN
                      RELATIVE=RELATIVE+2*VSAT(I)*XSAT(I)/C
                    ELSE IF (IORBFL .EQ. 2) THEN
                      RELATIVE=RELATIVE+2*XXSAT(3+I)*XXSAT(I)/C
                    ENDIF
130               CONTINUE
                  RHOACT=DSQRT(RHOACT)
                  IF (MEATYP.EQ.3) THEN
                    RHO=(RHOACT-SCAL*(1.D0+SCAL/2.D0/RHOACT))
                  ELSE
                    RHO=(RHOACT-SCAL*(1.D0+SCAL/2.D0/RHOACT))+RELATIVE
                  ENDIF
C
C FIRST DERIVATIVE OF TOPOC. SAT. VECTOR
C --------------------------------------
                  DO 135 I=1,3
                    TOPDER(I)=-XSTOPO(I)/RHOACT+VREL(I)
135               CONTINUE
                  DERIV=0.D0
                  DO 140 I=1,3
                    DERIV=DERIV+VSAT(I)*TOPDER(I)
140               CONTINUE
                  DO 148 I=1,ICLPO1
                    CLKDER(I)=DERIV/C
                    DO 145 K=2,I
                      CLKDER(I)=CLKDER(I)*TREL
145                 CONTINUE
148               CONTINUE
                  IF (FLGMIX.AND.IDINT(SATNRA(ISAT)/100.D0).EQ.1) THEN
                    UTCDER=DERIV/C+1
                  ELSE
                    UTCDER=0.D0
                  ENDIF
C
C CORRECT COMPUTED RANGE DUE TO IONOSPHERIC REFRACTION
C ----------------------------------------------------
                  IF(IONO.EQ.1.AND.FILFRQ.NE.3) THEN
                    CALL RFIAPL(FRQ(FILFRQ,SATNRA(ISAT)),150.D0,ZENITH,
     1                          XSTELL(2),OBSEPO,DRIONO)
                    RHO=RHO+DRIONO
                  ENDIF
C
C CORRECT COMPUTED RANGE DUE TO TROPOSPHERIC REFRACTION
C -----------------------------------------------------
                  IF (MARTYP.NE.MTypeSPACE) THEN
                    IF(ITROPO.NE.0) THEN
                      IF (ITROPO.EQ.20) THEN
                        FILTRP=' '
                        ITRPMD=0
                        CALL GETTRP(FILTRP,OBSEPO,STANAM,2,0,ITRPMD,
     1                              ITRMAP,ITRGRD,DRTEST,IRCTRP)
                        ITRPMD=ABS(ITRPMD)
                        IF (IRCTRP.NE.0.AND.ITRPMD>10)ITRPMD=ITRPMD-10
                        metex=1
                        CALL TDELAY(OBSEPO,ZENITH,XSTNEL,ITRPMD,metex,
     1                              0D0,T,P,H,DRTROP)
C
                        CALL TRPMAP(ITRMAP,OBSEPO,XSTNEL,ZENITH,MAPFUN)
                        DRTROP=DRTROP+DRTEST(3)*MAPFUN(1)
C
C APPLY GRADIENTS
                        IF (ITRGRD.EQ.1) THEN
                          DRTROP=DRTROP
     1                              +DRTEST(1)*MAPFUN(2)*DCOS(AZIMUT)
     2                              +DRTEST(2)*MAPFUN(2)*DSIN(AZIMUT)
                        ELSEIF (ITRGRD.EQ.2) THEN
                          DRTROP=DRTROP
     1                              +DRTEST(1)*DTAN(ZENITH)*DCOS(AZIMUT)
     2                              +DRTEST(2)*DTAN(ZENITH)*DSIN(AZIMUT)
                        ELSEIF (ITRGRD.EQ.3) THEN
                          DRTROP=DRTROP
     1                              +DRTEST(1)*MAPFUN(1)*DTAN(ZENITH)*
     2                               DCOS(AZIMUT)
     3                              +DRTEST(2)*MAPFUN(1)*DTAN(ZENITH)*
     4                               DSIN(AZIMUT)
                        ELSEIF (ITRGRD.EQ.4) THEN
                          MAZ=1.D0/(DCOS(ZENITH)*DTAN(PI/2.D0-
     1                        ZENITH)+0.0032D0)
                          DRTROP=DRTROP
     1                              +DRTEST(1)*MAZ*DCOS(AZIMUT)
     2                              +DRTEST(2)*MAZ*DSIN(AZIMUT)
                        ELSEIF (ITRGRD.NE.0) THEN
                          WRITE(LFNERR,906) ITRGRD
906                       FORMAT(/,' *** SR CDCOMP: ',
     1                             'UNKNOWN GRADIENT MODEL',/,
     2                              16X,'ITRGRD: ',I3,/)
                          CALL EXITRC(2)

                        ENDIF
                      ELSE
                        WL = 0.D0
C MARINI-MURRAY
                        IF (ITROPO.EQ.4) THEN
                          IF (MEATYP.EQ.1) FRQID(1:1) = 'L'
                          IF (MEATYP.EQ.2) FRQID(1:1) = 'P'
                          IF (MEATYP.EQ.3) FRQID(1:1) = 'R'
                          WRITE(FRQID(2:2),'(I1)') FILFRQ
                          CALL GETWAV(MEATYP,STANAM,FRQID,TOBS,WL)
                        ENDIF
                        metex=1
                        CALL TDELAY(OBSEPO,ZENITH,XSTNEL,ITROPO,METEX,
     1                              WL,T,P,H,DRTROP)
                      ENDIF
                      RHO=RHO+DRTROP
                    ENDIF
                  END IF
C
C CORRECT COMPUTED RANGE DUE TO DIFFERENTIAL CODE BIASES
C ------------------------------------------------------
                  CALL DCBCOR(MEATYP,FILFRQ,SATNRA(ISAT),
     1                        STANAM,RECTYP(1),0,0,OBSEPO,RHO)
C
C CORRECT COMPUTED RANGE FOR SATELLITE CLOCK:
C (RHO IN METERS, CLOCKC IN SEC)
C -------------------------------------------
                  RHO=RHO-CLOCKC*C
C
C CORRECT COMPUTED RANGE FOR RECEIVER CLOCK ERROR
C -----------------------------------------------
                  IF (MEATYP.EQ.2) RHO=RHO+DTREC*C
C
C MARK OBSERVATION, IF ZENITH-DISTANCE > "ZENMAX" (RAD)
C -----------------------------------------------------
                  IF(ZENITH.GT.ZENMAX.AND.FLGACT(ISAT).EQ.'G') THEN
                    IF(KINEST.EQ.0) THEN
                      FLGACT(ISAT)='E'
                    ELSE
                      IF(JTER.GT.1) FLGACT(ISAT)='E'
                    END IF
                  END IF
C ELEVATION
                  ELEACT(ISAT)=(PI/2-ZENITH)*180/PI
C
C OBS. EQU. (DIMENSIONS: METERS)
C ---------
C ROW OF A-MATRIX
C ---------------
                  DO 150 KK=1,NCOPAR
                    AZEILE(KK)=TOPDER(KK)
                    INDA(KK) = KK
150               CONTINUE
                  IF (FLGMIX) THEN
                    KK=NCOPAR+1
                    AZEILE(KK)=UTCDER
                    INDA(KK) = KK
                  ENDIF
                  TREL=(OBSEPO-TIMREF)*86400.D0
                  DO 170 K=1,ICLPO1
                    KK=K+NCOPAR
                    IF (FLGMIX) KK=KK+1
                    AZEILE(KK)=1.D0
                    DO 160 I=2,K
                      AZEILE(KK)=AZEILE(KK)*TREL
160                 CONTINUE
                    AZEILE(KK)=AZEILE(KK)+CLKDER(K)
                    INDA(KK) = KK
170               CONTINUE
C
C COPY TO MATRIX "AMAT"
C ---------------------
                  DO 180 I=1,NORDIM
                    AMAT(I,ISAT)=AZEILE(I)
180               CONTINUE
C
C DPHI
C ----
                  DPHI(ISAT)=(OBSERV(ISAT)-RHO)
C
C NEXT SATELLITE
C --------------
200             CONTINUE
C
C GET THE WEIGHT FOR A STATION
C ----------------------------
                DTSIM=DBLE(IDELTT/2)/86400D0
                CALL WGTSTA(OBSEPO,DTSIM,2,STANAM,OBSWGT)
                IF (OBSWGT.LT.1D0) OBSWGT=1D0
C
C CHECK QUALITY OF OBSERVATIONS (STATIC CASE)
C -------------------------------------------
                NSATOK=NSACT
                IF (IOUTLR.EQ.1.AND.KINEST.EQ.0) THEN
C
C DO SCREENING FOR EACH SATELLITE SYSTEM SEPARATE FOR THE FIRST ITERATION
C (BECAUSE THE SYSTEM CLOCK OFFSETES ARE STILL NOT INCLUDED)
                  DO ISYS=0,SIZE(G_SVNSYS)-1
                    IF (ITER.GT.1.AND.ISYS.GT.0) EXIT
                    IF (G_SVNSYS(ISYS).EQ.' ') CYCLE
C
                    IF (ITER.EQ.1) THEN
                      DO ISAT=1,NSACT
                        IF (FLGACT(ISAT).NE.'G') CYCLE
                        CALL SVN2CHR(SATNRA(ISAT),IDUMMY,SVNCHR)
                        IF (SVNCHR.NE.G_SVNSYS(ISYS)) THEN
                          FLGACT(ISAT) = 'S'
                        ENDIF
                      ENDDO
                    ENDIF

C
260                 NSATOK=0
                    DO 210 ISAT=1,NSACT
                      IF (FLGACT(ISAT).EQ.'G') THEN
                        DPHIMJ(ISAT)=DPHI(ISAT)
                        NSATOK=NSATOK+1
                      ELSE
                        DPHIMJ(ISAT)=1.D20
                      ENDIF
210                 CONTINUE
C
C ONLY ONE GOOD SATELLITE
                    IF (NSATOK.EQ.1) THEN
                      DO ISAT=1,NSACT
                        IF (FLGACT(ISAT).EQ.'G') THEN
                          FLGACT(ISAT)='B'
                          CALL MRKOBS(1,SATNRA(ISAT),IEPOCH,IFILE,
     1                                NSATEL,SATNUM,NDEL,LSTDEL)
                          DPHIMJ(ISAT)=1.D20
                        ENDIF
                      ENDDO
                    ENDIF
C
C MAJORITY VOTING
                    CALL MAJOR1(NSACT,DPHIMJ,IRFSAT,DMEAN1)
C
C WORST SATELLITE (COMPARTED TO REFERENCE)
                    DMAX=0.D0
                    DO 220 ISAT=1,NSACT
                      IF (FLGACT(ISAT).EQ.'G' .AND. ISAT.NE.IRFSAT) THEN
                        DPTEST=DABS(DPHI(ISAT)-DPHI(IRFSAT))
                        IF (DPTEST.GT.DMAX) THEN
                          DMAX=DPTEST
                          ISAMAX=ISAT
                        ENDIF
                      ENDIF
220                 CONTINUE
C
C RMS SUM WITHOUT WORST SATELLITE
                    IF (DMAX.GT.DIFMAX*OBSWGT) THEN
                      RMSDIF=0.D0
                      DO 230 ISAT=1,NSACT
                        IF (FLGACT(ISAT).EQ.'G' .AND.
     1                      ISAT.NE.IRFSAT .AND. ISAT.NE.ISAMAX) THEN
                          RMSDIF=RMSDIF+(DPHI(ISAT)-DPHI(IRFSAT))**2
                        ENDIF
230                   CONTINUE
                      NSARMS=NSATOK-2
                      IF (NSARMS.GT.1) THEN
                        RMSDIF=DSQRT(RMSDIF/(NSATOK-1))
                      ELSE
                        RMSDIF=0.D0
                      ENDIF
                      IF (ITER.GT.1) THEN
                        IF (RMSDIF.EQ.0.D0) THEN
                          RMSDIF=SIGMA0*DSQRT2
                        ELSE
                          RMSDIF=DMIN1(RMSDIF,SIGMA0*DSQRT2)
                        ENDIF
C
C USE AT LEAST DIFMAX FOR THE FIRST ITERATION
                      ELSE IF (RMSDIF.NE.0D0) THEN
                        RMSDIF=DMIN1(RMSDIF,DIFMAX*OBSWGT)
                      ELSE
                        RMSDIF=DIFMAX*OBSWGT
                      ENDIF
C
                      IF (RMSDIF.NE.0.D0 .AND.
     1                  DMAX.GT.CONFID*RMSDIF) THEN
                        FLGACT(ISAMAX)='B'
                        CALL MRKOBS(1,SATNRA(ISAMAX),IEPOCH,IFILE,
     1                              NSATEL,SATNUM,NDEL,LSTDEL)
                        GOTO 260
                      ENDIF
                    ENDIF
C
C SET SATELLITE SYSTEM FLAGS BACK
                    DO ISAT=1,NSACT
                      IF (FLGACT(ISAT).EQ.'S') THEN
                        FLGACT(ISAT) = 'G'
                      ENDIF
                    ENDDO
C
C NEXT SATELIITE SYSTEM FOR FIRST ITERATION
                  ENDDO
                ENDIF
C
C UPDATE NORMAL EQUATION SYSTEM (NOT FOR MARKED OBSERVATIONS)
C -----------------------------
                DO 290 ISAT=1,NSACT
C
                  IF(FLGACT(ISAT).EQ.'G') THEN

                    CALL ADDNOR(1,NORDIM,WGTGEN,PMAT,AMAT(1,ISAT),
     1                          INDA,INDI,INDK,HELP,MAXEQN,DPHI(ISAT),
     2                          ANOR,BNOR,RMSSUM,NOBSEQ)
                  ENDIF
C
C STATISTICS
C ----------
                  IF(KINEST.EQ.0) THEN
                    INDACT=INDSVN(SATNRA(ISAT))
                    OBSSAT(1,INDACT)=OBSSAT(1,INDACT)+1
                    IF(FLGACT(ISAT).NE.'G')
     1                OBSSAT(2,INDACT)=OBSSAT(2,INDACT)+1
                  END IF
290             CONTINUE
C
C COMPUTE RECEIVER OFFSET FOR CURRENT EPOCH (STATIC CASE)
C -------------------------------------------------------
                IF (ICLPOL.EQ.-1 .AND. KINEST.EQ.0) THEN
C
C COMPUTE CLOCK OFFSET
                  KK=(NORDIM*(NORDIM+1))/2
                  IF (ANOR(KK) .NE. 0.D0) THEN
                    DOFFS(IEPOCH) = ((1.D0/ANOR(KK))*BNOR(NORDIM))/C
                    OFFS(IEPOCH) = OFFS(IEPOCH) + DOFFS(IEPOCH)
C
                    JPAR = JPAR + 1
C
C REDUCE NORMAL EQUATION SYSTEM
                    NPARN = NORDIM - 1
                    CALL REDNAV(NORDIM,NPARN,ANOR,BNOR,RMSSUM,AII,A0I)
C
                  ELSE
                    OFFS(IEPOCH) = 0.D0
                  ENDIF
C
                ENDIF
C
C SOLVE NORM. EQU. SYSTEM FOR KINEMATIC COORDINATES
C -------------------------------------------------
                IF (KINEST.EQ.1) THEN
C
C INVERSION OF NEQ-MATRIX
C -----------------------
                  IF((NOBSEQ-NORDIM).GE.MINDOF) THEN
C
                    CALL SYMINVG(NORDIM,ANOR,0,ISING)
                    CALL SOLVE(NORDIM,ANOR,BNOR,DPAR)
C
C SIGMA0
C ------
                    SIGAPR=1.D0
                    SIGMA0=SIGMA1(0,NORDIM,NORDIM,NOBSEQ,DPAR,INDP,
     1                            AZEILE,INDA,B,PMAT(1),BNOR,
     2                            RMSSUM,VSIG,SIGAPR)
                  ELSE
                    ISING=99999
                  END IF
C
C REMOVE TOO FEW OBSERVATIONS
                  IF (ISING.NE.0) THEN
                    DO ISAT=1,NSACT
                      IF (FLGACT(ISAT).NE.'G') CYCLE
                      FLGACT(ISAT)='B'
                      CALL MRKOBS(5,SATNRA(ISAT),IEPOCH,IFILE,
     1                            NSATEL,SATNUM,NDEL,LSTDEL)
                    END DO
                  END IF
C
C RESULTS
C -------
C
C MCOR,MCLOCK
C -----------
                  IF (ISING.EQ.0) THEN
                    KK = 0
                    DO K=1,NCOPAR
                      KK = KK + K
                      MCOR(K)=SIGMA0*DSQRT(ANOR(KK))
                    END DO
                    K=NCOPAR+1
                    KK = KK + K
C
                    MCLOCK(1)=SIGMA0*DSQRT(ANOR(KK))/C
                  END IF
C
C NEW STATION COORDINATES
C -----------------------
                  IF (ISING.EQ.0) THEN
                    DO K=1,NCOPAR
                      XSTANW(K)=XSTANW(K)+DPAR(K)
                    END DO
                  ELSE
                    XSTANW(1:3)=0.D0
                  END IF
C
C COMPUTE CLOCK OFFSET
C --------------------
                  IF (ISING.EQ.0) THEN
                    OFFS(IEPOCH) = OFFS(IEPOCH) + DPAR(NORDIM)/C
                  ELSE
                    OFFS(IEPOCH) = 0.D0
                  END IF
C
C MORE ITERATIONS ?
C -----------------
                  IF(ISING.NE.0) GOTO 3105
                  TEST=DSQRT(DPAR(1)**2+DPAR(2)**2+DPAR(3)**2)
                  IF(TEST.LT.EPS) GOTO 1105
                  IF(JTER.GE.NJTACT.AND.TEST.GT.EPS) THEN
                    ISING = 88888
                    GOTO 1105
                  END IF
C
                END IF
C
C END OF KINEMATIC ITERATION LOOP
C -------------------------------
1100          CONTINUE
C
1105          CONTINUE
C
C SAVE RESULTS FROM SOLUTION WITHOUT SVN SATNRA(IBDSAT)
C -----------------------------------------------------
              IF(JSCR.NE.1.AND.KINEST.EQ.1) THEN
                TESTSIGMA0(IBDSAT) = SIGMA0
                TESTXSTANW(IBDSAT,1:3) = XSTANW(1:3)
                TESTOFFS(IBDSAT,IEPOCH) = OFFS(IEPOCH)
                TESTMCOR(IBDSAT,1:NCOPAR) = MCOR(1:NCOPAR)
                TESTMCLOCK(IBDSAT,1) = MCLOCK(1)
                TESTAMAT(IBDSAT,:,:) = AMAT
                TESTDPHI(IBDSAT,:) = DPHI
                TESTANOR(IBDSAT,:) = ANOR
                TESTDPAR(IBDSAT,:) = DPAR
C
C UNMARK TEMPORARILY MARKED SVN SATNRA(BADSAT)
C --------------------------------------------
                FLGACT(IBDSAT) = 'G'
              END IF
C
2100        CONTINUE
C
C SELECT SOLUTION WITH SMALLEST RMS
C ---------------------------------
            IF(JSCR.NE.1.AND.KINEST.EQ.1) THEN
              BESTRMS = 100000.D0
              DO I=1,NSACT
                IF(TESTSIGMA0(I).LT.BESTRMS.AND.FLGACT(I).EQ.'G') THEN
                  BESTRMS=TESTSIGMA0(I)
                  IBDSAT=I
                END IF
              END DO
C
C MARK WORST SATELLITE
C --------------------
              FLGACT(IBDSAT) = 'B'
              CALL MRKOBS(1,SATNRA(IBDSAT),IEPOCH,IFILE,NSATEL,
     1                    SATNUM,NDEL,LSTDEL)
C
C SAVE BEST SOLUTION
              SIGMA0=TESTSIGMA0(IBDSAT)
              XSTANW(1:3)=TESTXSTANW(IBDSAT,1:3)
              OFFS(IEPOCH)=TESTOFFS(IBDSAT,IEPOCH)
              DTREC=OFFS(IEPOCH)
C
              MCOR(1:NCOPAR) = TESTMCOR(IBDSAT,1:NCOPAR)
              MCLOCK(1) = TESTMCLOCK(IBDSAT,1)
C
              AMAT = TESTAMAT(IBDSAT,:,:)
              DPHI = TESTDPHI(IBDSAT,:)
              ANOR = TESTANOR(IBDSAT,:)
              DPAR = TESTDPAR(IBDSAT,:)
            ELSEIF(JSCR.EQ.1.AND.KINEST.EQ.1) THEN
              DTREC=OFFS(IEPOCH)
            END IF
C
C END OF KINEMATIC SCREENING LOOP
3100      CONTINUE
C
3105      CONTINUE
C
C WRITE KINEMATIC COORDINATES TO THE KIN OUTPUT FILE
C --------------------------------------------------
          IF(KINEST.EQ.1) THEN
            TOBS=OBSEPO-(DTREC1)/86400.D0
C
C PREPARE DIFFERENT STATION TYPES
C -------------------------------
            IF(ISING.EQ.0) THEN
              IF (MARTYP.NE.MTypeSPACE) THEN
C
C NEW ELLIPSOIDAL STATION COORDINATES (ANTENNA)
                CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,XSTANW,XELEPO)
C
C NEW STATION COORDINATES (MARKER) (REMOVE ANTENNA ECCENTRICITY (ANTECC))
                CALL ELLECC(XELEPO,POSECC(1,1),EXC)
                DO I=1,3
                  XSTANW(I)=XSTANW(I)-EXC(I)
                ENDDO
C
C NEW ELLIPSOIDAL STATION COORDINATES (MARKER)
                CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,XSTANW,XSTNEL)
              END IF
C
C LEO
C ---
              IF (MARTYP.EQ.MTYPESPACE) THEN
                IORSYSL=2
                CALL COOTRA(IORSYSL,0,TOBS,XDUMMY,SZ,XPOL,YPOL,UT1GPS)
                CALL TRUEARTH(XSTANW(1:3),XSTANWT(1:3),1,0,SZ,XPOL,YPOL)
C
C APPLY SENSOR OFFSET AND PHASE CENTER OFFSET
C --------------------------------------------
C
C GET VELOCITY FROM STANDARD ORBIT (IF AVAILABLE)
                IF (IRCSTD==0.AND.IRCOD==0) THEN
                  XSTANWT(4:6)=XKIN(4:6)
C
C COMPUTE CRUDE APPROXIMATION FOR VELOCITY
                ELSE
                  IF (DABS((TLAST-TOBS)*86400.D0).LE.120.D0) THEN
                    XSTANWT(4:6)= (XSTANWT(1:3)-XLAST(1:3))/
     1                            (86400.D0*(TOBS-TLAST))
                    XLAST(1:3)=XSTANWT(1:3)
                    TLAST=TOBS
                  ELSE
C
C NO ATTITUDE INFORMATION (SO FAR)
                    XSTANWT(4:6)=1.D20
                    XLAST(1:3)=XSTANWT(1:3)
                    TLAST=TOBS
                  END IF
                END IF
C
C COMPUTE OFFSET
                CALL LEOANTE(STANAM,TOBS,PHAECC,0,XSTANWT,TRUOFF)
                XSTANWT(1:3)=XSTANWT(1:3)-TRUOFF(1:3)
C
C TRANSFORMATION INTO EARTH FIXED SYSTEM
C --------------------------------------
                CALL TRUEARTH(XSTANWT(1:3),XSTANW(1:3),0,0,SZ,XPOL,YPOL)
              END IF
            END IF


            IF(LFNKOU.NE.0) THEN
              CALL MJDGPS(OBSTIM,SECOND,NWEEK)
              SECSAV=DNINT(SECOND)
              IF (ISING.EQ.0) THEN
C
C INTERPOLATE: MISSING EPOCHS
C ---------------------------
                IF (NREC.GT.1.AND.NREC0.GE.1.AND.
     1              NREC.GT.NREC0+1) THEN
                  TOBS1=TOBS0+IDELTT/86400D0
                  DO WHILE (TOBS1+0.1d0*IDELTT/86400D0.LT.TOBS)
                    DO I=1,3
                      XKIN1(I)=XKIN0(I)+(XSTANW(I)-XKIN0(I))/
     1                         (TOBS-TOBS0)*(TOBS1-TOBS0)
                    ENDDO
                    CALL MJDGPS(TOBS1,SECOND1,NWEEK1)
                    WRITE(LFNKOU,711) STANAM,NWEEK1,DNINT(SECOND1),
     1                   (XKIN1(I),I=1,3),'X'
                    TOBS1=TOBS1+IDELTT/86400D0
                  ENDDO
                ENDIF
                WRITE(LFNKOU,711) STANAM,NWEEK,SECSAV,
     1                           (XSTANW(K),K=1,3),'K'
711             FORMAT(1X,A16,1X,I4,1X,F8.0,1X,3F15.4,1X,A1)
                NREC0 = NREC
                TOBS0 = TOBS
                XKIN0 = XSTANW(1:3)
              ENDIF
            ENDIF
C
C TITLE FOR PROGRAM OUTPUT FILE
C -----------------------------
            IF(NREC.EQ.1) THEN
              WRITE(lfnprt,'(3(A,/),A)')
     1        ' KINEMATIC RESULTS',
     2        ' -----------------------------------------------------'//
     3        '------------------------------------------------------'//
     4        '-------------------------------------------------',
     5        ' EPOCH     MJD      STATION          SAT     RMS    '//
     6        ' X-COORDINATE       RMS     Y-COORDINATE       RMS    '//
     7        ' Z-COORDINATE       RMS    CLOCK           RMS',
     8        ' -----------------------------------------------------'//
     9        '------------------------------------------------------'//
     1        '-------------------------------------------------'

            END IF
C
C WRITE KINEMATIC COORDINATES TO THE PROGRAM OUTPUT FILE
C ------------------------------------------------------
            IF (ISING.EQ.0) THEN
              WRITE (lfnprt,'(I6,F13.6,1X,A16,I2,"/",I2,1X,
     1            F8.4,3(F15.4," +- ",F8.4),F8.0," us"," +- ",F8.4,
     2            " ns")')
     3            iEpoch,tObs,staNam,nsAct-nObsEq,nsAct,
     4            sigma0,(xStaNw(ii)-xStat(ii),mCor(ii),ii=1,3),
     5            offs(iEpoch)*1.d6,mClock(1)*1.d9
            ELSE IF (ISING.EQ.99999) THEN
              WRITE (LFNPRT,'(I6,F13.6,1X,A16,I2,"/",I2,3X,A)')
     1        IEPOCH,TOBS,STANAM,NSACT-NOBSEQ,NSACT,
     2        'TOO FEW OBSERVATIONS'
            ELSE IF (ISING.EQ.88888) THEN
              WRITE (LFNPRT,'(I6,F13.6,1X,A16,I2,"/",I2,3X,A)')
     1        IEPOCH,TOBS,STANAM,NSACT-NOBSEQ,NSACT,
     2        'SOLUTION DOES NOT CONVERGE'
            ELSE
              WRITE (LFNPRT,'(I6,F13.6,1X,A16,I2,"/",I2,3X,A)')
     1        IEPOCH,TOBS,STANAM,NSACT-NOBSEQ,NSACT,
     2        'SINGULAR EPOCH SOLUTION'
            END IF
C
C OBSERVATION STATISTICS
C ----------------------
            DO ISAT=1,NSACT
              INDACT=INDSVN(SATNRA(ISAT))
              OBSSAT(1,INDACT)=OBSSAT(1,INDACT)+1
              IF(FLGACT(ISAT) /= 'G')OBSSAT(2,INDACT)=OBSSAT(2,INDACT)+1
            ENDDO
C
C KINEMATIC STATISTICS
C -------------------
            IF (ISING.EQ.0) THEN
              DO K=1,3
                IF (MCOR(K).LT.RMSMIN(K+1)) RMSMIN(K+1)=MCOR(K)
                IF (MCOR(K).GT.RMSMAX(K+1)) RMSMAX(K+1)=MCOR(K)
                RMSMEAN(K+1)=RMSMEAN(K+1)+MCOR(K)
              END DO
              IF (MCLOCK(1).LT.RMSMIN(5)) RMSMIN(5)=MCLOCK(1)
              IF (MCLOCK(1).GT.RMSMAX(5)) RMSMAX(5)=MCLOCK(1)
              RMSMEAN(5)=RMSMEAN(5)+MCLOCK(1)
              IF (SIGMA0.LT.RMSMIN(1)) RMSMIN(1)=SIGMA0
              IF (SIGMA0.GT.RMSMAX(1)) RMSMAX(1)=SIGMA0
              RMSMEAN(1)=RMSMEAN(1)+SIGMA0
              NREPO=NREPO+1
            ELSEIF (ISING.EQ.99999) THEN
              NDOF0=NDOF0+1
            ELSE
              NSINGE=NSINGE+1
            END IF
          END IF
C
C WRITE ELEVATIONS AND/OR DATA FOR RESIDUAL COMP. ON SCRATCH
C FILE (IF REQUESTED)
C ----------------------------------------------------------
          WRITE(LFNSCR)OBSEPO,NSACT,
     1                (SATNRA(II),FLGACT(II),II=1,NSACT)
          IF(PRIOPT(1).EQ.1) THEN
            WRITE(LFNSCR)(ELEACT(II),II=1,NSACT)
          ENDIF
          WRITE(LFNSCR)((AMAT(II,KK),II=1,3),DPHI(KK),KK=1,NSACT)
          IF (KINEST.EQ.1) WRITE(LFNSCR) DPAR(1:3)
C
C INIT. ARRAYS FOR OBS.- AND NORMAL EQUATION SYSTEMS
C --------------------------------------------------
          IF (KINEST.EQ.1) THEN
            DOFFS(IEPOCH)=DPAR(4)/C
          END IF
C
C NEXT EPOCH
C ----------
C
300     CONTINUE
C
C SOLVE NORM. EQU. SYSTEM
C -----------------------
310     CONTINUE
C
C ENOUGH OBSERVATIONS?
C --------------------
        IF(NOBSEQ.LT.NORDIM) THEN
          WRITE(LFNERR,301) NOBSEQ,OBSFIL
301       FORMAT(/' *** SR CDCOMP: NOT ENOUGH OBSERVATIONS',/,
     1                        16X,'NUMBER OF OBSERVATIONS:',I4,/,
     2                        16X,'OBSERVATION FILE      : ',A,/)
          IRCODE=1
          GOTO 999
        ENDIF
C
C INVERSION OF NEQ-MATRIX
C -----------------------
        IF (ICLPOL .GT. 0) THEN
          N1PAR = NORDIM
          NPARMS = NORDIM
        ELSE IF (ICLPOL .EQ. -1) THEN
          N1PAR = NCOPAR
          NPARMS = NCOPAR + JPAR
          IF (FLGMIX) THEN
            N1PAR=N1PAR+1
            NPARMS=NPARMS+1
          ENDIF
        ENDIF
        IF (ICORFL.EQ.0) NPARMS=NPARMS-3
C
        CALL SYMINVG(N1PAR,ANOR,0,ISING)
        CALL SOLVE(N1PAR,ANOR,BNOR,DPAR)
C
C SIGMA0
C ------
        SIGAPR=1.D0
        SIGMA0=SIGMA1(0,NORDIM,NPARMS,NOBSEQ,DPAR,INDP,AZEILE,INDA,B,
     1                PMAT(1),BNOR,RMSSUM,VSIG,SIGAPR)
C
C NEW STATION COORDINATES
C -----------------------
        DO 360 K=1,NCOPAR
          XSTANW(K)=XSTANW(K)+DPAR(K)
360     CONTINUE
C
C NEW RECEIVER CLOCK MODEL PARAMETERS
C -----------------------------------
        IF (ICLPOL .GT. 0) THEN
          IF (FLGMIX) THEN
            DO 369 K=1,ICLPOL
              CLKMOD(K)=CLKMOD(K)+DPAR(K+NCOPAR)/C
369         CONTINUE
            UTCGL=UTCGL+DPAR(NCOPAR+ICLPOL+1)/C
            CLKMOD(ICLPOL+1)=UTCGL
          ELSE
            DO 370 K=1,ICLPOL
              CLKMOD(K)=CLKMOD(K)+DPAR(K+NCOPAR)/C
370         CONTINUE
          ENDIF
        ELSE
          UTCGL=UTCGL+DPAR(NCOPAR+1)/C
          CLKMOD(1)=UTCGL
        ENDIF
C
C WAS THE LINEARIZATION ALLOWED?
C ------------------------------
        TEST=DSQRT(DPAR(1)**2+DPAR(2)**2+DPAR(3)**2)
        IF (ITER.GE.NITER) THEN
          IF (TEST.GT.TOLCRD) THEN
            WRITE(LFNERR,901) TEST,TOLCRD,ITER,OBSFIL
901         FORMAT(/,' ### SR CDCOMP: MAXIMUM NUMBER OF ITERATIONS ',
     1               'REACHED,'
     2             /,16X,'BUT COORDINATE IMPROVEMENT EXCEEDS TEST ',
     3               'CRITERIUM',
     4             /,16X,'COORDINATE IMPROVEMENT:',F7.1
     5             /,16X,'TEST CRITERIUM (METER):',F7.1
     6             /,16X,'ITERATION STEP        :',I5,
     7             /,16X,'OBSERVATION FILENAME  : ',A,/)
          ENDIF
          GOTO 410
        ENDIF
        IF (TEST.LT.TOLCRD .AND. ITER.GT.1) GOTO 410
C
C PREPARE NEXT ITERATION
C ----------------------
390     REWIND(UNIT=LFN001)
        REWIND(UNIT=LFNSCR)
C
C NEXT ITERATION STEP
C -------------------
400   CONTINUE
410   CONTINUE
      NITERF=ITER
C
C WRITE STATISTICS FOR KINEMATIC ESTIMATION
C -----------------------------------------
      IF (KINEST.EQ.1 .AND. NREPO.GT.0) THEN
        DO K=1,5
          RMSMEAN(K)=RMSMEAN(K)/NREPO
        END DO
        RMSMIN(5)=RMSMIN(5)*1.D9
        RMSMAX(5)=RMSMAX(5)*1.D9
        RMSMEAN(5)=RMSMEAN(5)*1.D9
C
        WRITE (LFNPRT,'(" ",156("-"),/,
     1         35X,"  MIN: ",3(F8.4,19X),F8.4,15X,F8.4,/,
     2         35X,"  MAX: ",3(F8.4,19X),F8.4,15X,F8.4,/,
     3         35X," MEAN: ",3(F8.4,19X),F8.4,15X,F8.4//,
     4         " NUMBER OF EPOCHS WITH TOO LESS REDUNDANCY: ",I6/,
     5         " NUMBER OF SINGULAR KINEMATIC EPOCHS:       ",I6,/)')
     6        (RMSMIN(K),K=1,5),
     7        (RMSMAX(K),K=1,5),
     8        (RMSMEAN(K),K=1,5),
     9         NDOF0,
     1         NSINGE

      END IF
      IF (KINEST.EQ.1 .AND. NREPO.EQ.0) THEN
        WRITE (LFNPRT,'(132("-"),/," NO REGULAR KINEMATIC EPOCHS",/,
     1       " NUMBER OF SINGULAR KINEMATIC EPOCHS: ",I6,/)') NSINGE
      END IF
C
C CLOSE/REWIND FILES
C ------------------
      CLOSE(UNIT=LFN001)
      REWIND(UNIT=LFNSCR)
C
C RETURN IN KINEMATIC CASE
C ------------------------
      IF (KINEST.EQ.1) GOTO 999
C
C RESULTS
C -------
C
C MCOR,MCLOCK
C -----------
      KK = 0
      DO 480 K=1,NCOPAR
        KK = KK + K
        MCOR(K)=SIGMA0*DSQRT(ANOR(KK))
480   CONTINUE
      IF (ICLPOL .GT. 0) THEN
        IF (FLGMIX) THEN
          K=NCOPAR+1
          KK = KK + K
          MCLOCK(ICLPOL+1)=SIGMA0*DSQRT(ANOR(KK))/C
          DO 483 K=NCOPAR+2,NORDIM
            KK = KK + K
            MCLOCK(K-NCOPAR-1)=SIGMA0*DSQRT(ANOR(KK))/C
483       CONTINUE
        ELSE
          DO 485 K=NCOPAR+1,NORDIM
            KK = KK + K
            MCLOCK(K-NCOPAR)=SIGMA0*DSQRT(ANOR(KK))/C
485       CONTINUE
        ENDIF
      ELSEIF (FLGMIX) THEN
        K=NCOPAR+1
        KK = KK + K
        MCLOCK(1)=SIGMA0*DSQRT(ANOR(KK))/C
      ENDIF
C
C NEW ELLIPSOIDAL STATION COORDINATES (ANTENNA)
C -----------------------------------
      CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,XSTANW,XSTNEL)
C
C NEW WGS STATION COORDINATES (MARKER)
C (REMOVE ANTENNA ECCENRICITY (ANTECC))
C -------------------------------------
      CALL ELLECC(XSTNEL,POSECC(1,1),EXC)
      DO 490 I=1,3
        XSTANW(I)=XSTANW(I)-EXC(I)
490   CONTINUE
C
C NEW ELLIPSOIDAL STATION COORDINATES (MARKER)
C -----------------------------------
      CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,XSTANW,XSTNEL)
C
C NEW ELLIPSOIDAL COORD ERRORS
      KK = 0
      DO 510 K=1,NCOPAR
        KK = KK + (K-1)
        DO 505 I=1,K
          IK = KK + I
          CM(I,K)=ANOR(IK)
          IF (I .NE. K) CM(K,I) = CM(I,K)
505     CONTINUE
510   CONTINUE
      IF (MARTYP.NE.MTypeSPACE) THEN
        CALL ERR3D(XSTNEL(1),XSTNEL(2),XSTNEL(3),AELL,BELL,-1,CM,DM)
      ELSE
        DM(1:NCOPAR,1:NCOPAR)=0.D0
      END IF
      DO 520 K=1,NCOPAR
        MELL(K)=SIGMA0*DSQRT(DM(K,K))
520   CONTINUE
C
      IRCODE=0
C
999   RETURN
C
      END SUBROUTINE

      END MODULE

