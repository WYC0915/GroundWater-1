      MODULE s_SNDIFF
      CONTAINS

C*
      SUBROUTINE SNDIFF(NPAR  ,NPARN ,MAXSNG,MAXEQN,ISNG  ,IS12  ,
     1                  ICENTR,SVN   ,XVSAT ,XSTAT ,WGSEPO,XTOPO ,
     2                  SZ    ,XPOL  ,YPOL  ,TPOL  ,CLFT  ,
     3                  TOBS  ,IEPO  ,IARC  ,TBOUND,TSEC  ,ELE   ,
     4                  IFILE ,LOCQ  ,PARTYP,A     ,INDA  ,AHELP ,
     5                  NOBSPA,ZENITH,AZIMUT,AMBWLF,NFRFIL,
     6                  ICARR ,NFREQ ,NUMAMB,AMBIEP,AMBCLS,AMBSAT,
     7                  MEATYP,MELWUB,TRPLIM,TRPLMS,ITRMAP,ITRGRD,
     8                  ELLS12,IORSYS,TOSC  ,TIMSTC,SCASTC,SCAHIL,
     9                  SCAPOT,NSAOFF,SATOFF,TIMOFF,SCAALB,SCACEN,
     .                  NSAALB,SATALB,RECTYP,ANTTYP,IANTEN,CSESS ,
     1                  ANTCAL,NUMCAL,PRNCAL,OPTDIP,OPTGIM,POLGIM,
     2                  EPOGIM,SCAGIM,ANTRAO,NUMRAO,PRNRAO,IPAMB ,
     3                  CLKHED,CLKREC,NDIFF ,IARC2 ,TBOUND2,STNAME,
     4                  TIMSTC2,NSASPV,SATSPV,NADIR,NADMAX,
     5                  RAPZENMAX,IPHSEP,ZENIT2,AZIMU2,AZISOK,NRGB,
     6                  OPLOAD,TIMISB,DTSIM,ZENMAX)
CC
CC NAME       :  SNDIFF
CC
CC PURPOSE    :  DEFINE FIRST DESIGN MATRIX A FOR SINGLE DIFFERENCE
CC               OBSERVATIONS.
CC
CC PARAMETERS :
CC         IN :  NPAR   : NUMBER OF PARAMETERS TO BE ESTIM.   I*4
CC               NPARN  : NUMBER OF NON AMBIGUITY PARAMETERS  I*4
CC               MAXSNG : MAXIMUM NUMBER OF NON-ZERO PARAM.   I*4
CC                        IN A-MATRIX
CC               MAXEQN : MAXIMUM NUMBER OF SIMULTANEOUS      I*4
CC                        SINGLE DIFFERENCES
CC               ISNG   : OBSERVATION NUMBER                  I*4
CC               IS12   : STATION NUMBERS INVOLVED            I*4(2)
CC               ICENTR(I),I=1,..,NSTAT: INDEX OF CENTER STAT.I*4
CC                        FOR EACH STATION
CC               SVN    : SVN-NUMBER OF SATELLITE OBSERVED    I*4
CC               XVSAT  : POSITION AND VELOCITY OF SATELLITE  R*8
CC                        IN SYSTEM OF EPOCH
CC               XSTAT(K,I),K=1,2,3 , I=1,2,..: TABLE WITH    R*8
CC                        STATION COORDINATES
CC               WGSEPO(K,I),K=1,2,3,I=1,2: GEOCENTRIC        R*8(3,*)
CC                        STATION COORDINATES CORRECTED FOR
CC                        EARTH TIDES (CURRENT STATIONS ONLY)
CC               XTOPO(K,I),K=1,2,3 , I=1,2 : TOPOCENTRIC     R*8
CC                        SATELLITE POSITIONS
CC               SZ     : SIDEREAL TIME (GREENWICH)           R*8
CC               XPOL   : X-COORDINATE OF POLE POSITIONS      R*8
CC               YPOL   : Y-COORDINATE OF POLE POSITIONS      R*8
CC               TPOL   : START AND END TIME OF INTERVAL FOR  R*8(2,*)
CC                        ONE SET OF PARAMETERS
CC                        1,2 :=BEGIN,END TIME, *:= 1..MAXPOL
CC               CLFT(K,I),K=1,2,I=1,2,... : BOUNDARIES FOR   R*8
CC                        CLOCK-ERROR NR. I
CC               TOBS   : OBSERVATION TIME (MJD)              R*8
CC               IEPO   : EPOCH NUMBER                        I*4
CC               IARC   : ARC NUMBER                          I*4
CC               TBOUND(K),K=1,2: START AND END OF ARC "IARC" R*8
CC                        IN MJD
CC               TSEC   : TIME ARGUMENT FOR PARTIAL DERIVA-   R*8
CC                        TIVES
CC               ELE    : ARRAY WITH OSCULATING ELEMENTS      R*8
CC               IFILE  : FILE NUMBER                         I*4
CC               LOCQ   : TABLE WITH PARAMETER DEFINITION     I*4
CC               PARTYP : PARAMETER DESCRIPTION               t_partyp(*)
CC               A(I),I=1,2,... : FIRST DESIGN MATRIX FOR     R*8
CC                        SINGLE DIFFERENCES
CC               INDA(I),I=1,2,..: INDEX FOR A MATRIX         I*2
CC               AHELP(I),I=1,..,NPAR: ONE LINE OF A-MATRIX   R*8
CC               NOBSPA : NUM.OF OBSERV PER PARAMETER         I*4(*,*)
CC                        NOBSPA(MAXMEA*ISYS+IMEA,IPAR)
CC               ZENITH(I),I=1,2: ZENITH DISTANCES FOR STAT.  R*8
CC               AZIMUT(I),I=1,2: AZIMUTHS FOR STAT.          R*8
CC               AMBWLF(I,J),I=1,..,NUMAMB,J=1,2: WAVELENGTH  I*4
CC                        FACTORS FOR BOTH FREQUENCIES
CC               NFRFIL : NUMBER OF FREQUENCIES FOR CURRENT   I*4
CC                        FILE
CC               ICARR  : FREQUENCY                           I*4
CC               NFREQ  : NUMBER OF FREQUENCIES IN THE        I*4
CC                        CURRENT FILE
CC               NUMAMB : NUMBER OF AMBIGUITIES               I*4
CC               AMBIEP(J),J=1,..NUMAMB : AMBIGUITY EPOCH     I*4
CC               AMBCLS(L,K),L=1,..,NUMAMB, K=1,2,3 :         I*4
CC                        AMBIGUITY CLUSTERS
CC               AMBSAT(J),J=1,..,NUMAMB :                    I*4
CC                        AMBIGUITY SATELLITE NUMBERS
CC               MEATYP : MEASUREMENT TYPE                    I*4
CC               MELWUB : MELBOURNE-WUEBBENA LC               I*4
CC                        =0: NO
CC                        =1: YES
CC                        =2: DTEC LC
CC               TRPLIM(K,I),K=1,2, I=1,2,...: LIMITS OF      R*8
CC                        APPLICABILITY FOR MODEL I (MJD)
CC               TRPLMS(K,I),K=1,2, I=1,2,...: LIMITS OF      R*8
CC                        APPLICABILITY FOR STATION SPECIFIC
CC                        TROPOSPHERE PARAMETERS
CC               ITRMAP : MAPPING FUNCTION FOR TROPOSP.EST.   I*4
CC                        =1: 1/COS(Z)
CC                        =2: HOPFIELD
CC                        =3: DRY NIELL
CC                        =4: WET NIELL
CC                        =5: DRY GMF
CC                        =6: WET GMF
CC                        =7: DRY VMF
CC                        =8: WET VMF
CC               ITRGRD : (1): EST. OF TROPOSPHERIC GRADIENTS I*4(*)
CC                             =0: NO ESTIMATION
CC                             =1: TILTING
CC                             =2: LINEAR
CC                             =3: TAN(Z)
CC                             =4: CHEN & HERRING
CC                        (2): RATIO OF NUMBER OF ZENITH TO
CC                             GRADIENT PARAMETERS
CC               ELLS12(I,K), I=1,2,3,K=1,2: ELLIPSOIDAL      R*8(3,2)
CC                        COORDINATES OF BOTH STATIONS:
CC                        LAT (RAD), LON (RAD), HEIGHT (M)
CC               IORSYS : ORBIT SYSTEM                        I*4
CC                        =1: B1950.0
CC                        =2: J2000.0
CC               TOSC   : OSCULATION EPOCH                    R*8
CC               TIMSTC : EPOCHS WITH STOCHASTIC IMPULSES     R*8(*,*,*,*)
CC               SCASTC : SCALING FACTOR FOR STOCH DERIVATIVES R*8
CC               SCAHIL : SCALE FACTOR FOR HILL PARAMETERS    R*8
CC               SCAPOT : SCALE FACTOR FOR EARTH POT. PAR.    R*8
CC               NSAOFF(I),I=1,..,NANOFF: NUMBER OF           I*4
CC                        SATELLITES BELONGING TO GROUP I
CC               SATOFF(J,I),J=1,..,NSAOFF(I),I=1,..,NANOFF:  I*4
CC                        SATELLITE NUMBERS OF EACH ANTENNA
CC                        GROUP
CC               TIMOFF(J,I),J=1,2,I=1,..,NRQOFF: TIME INTER- R*8
CC                        VAL FOR ANTENNA REQUEST I
CC               SCAALB : SCALE FACTOR FOR ALBEDO PARMS       R*8
CC               SCACEN : SCALE FACTOR FOR CENTER OF MASS     R*8
CC               NSAALB : NUMBER OF SATELLITES PER GROUP      I*4(*)
CC               SATALB(J,I),J=1,..,NSAALB(I),I=1,..,NALBGR:  I*4(*,*)
CC                        SATELLITE NUMBERS OF EACH ALBEDO
CC                        GROUP
CC               RECTYP : RECEIVER TYPES                      CH*20(2)
CC               ANTTYP : ANTENNA  TYPES                      CH*20(2)
CC               IANTEN : RECEIVER ANTENNA NUMBERS            I*4(2)
CC               CSESS(I),I=1,2: SESSION IDENTIFICATIONS      CH*4
CC               ANTCAL(J,I),J=1,2, I=1,..,NANCAL:            CH*20
CC                        RECEIVER (J=1) AND ANTENNA (J=2)
CC                        NAME FOR REQUEST I
CC               NUMCAL(2,I),I=1,..,NANCAL: ANTENNA NUMBERS   I*4
CC                        "FROM - TO" FOR REQUEST I
CC               PRNCAL(I),I=1,..,NANRAO: SAT.SYSTEM FOR      I*4
CC                        RECEIVER ANT. OFFSET REQUEST I
CC               OPTDIP : OPTIONS FOR DIFF. ION. PARAMETERS   I*4(3)
CC                        (1): =0: NO DIFF. ION. PARAMETERS
CC                             =1: ONE PAR. PER EPOCH AND SAT.
CC                             =2: PARAMETERS EPOCH-WISE PRE-
CC                                 ELIMINATED
CC                        (2): ELIMINATION OF REF. ION. PAR.
CC                        (3): ELEV.-DEP. PAR. CONSTRAINING
CC               OPTGIM : OPTIONS FOR GLOBAL IONOSPHERE MODEL I*4(*)
CC                        (1): MAXIMUM DEGREE
CC                        (2): MAXIMUM ORDER
CC                        (3): FLAG FOR REFERENCE FRAME
CC                             =1: GEOGRAPHICAL
CC                             =2: GEOMAGNETIC
CC                        (4): FLAG FOR POSITION OF THE SUN
CC                             =1: MEAN
CC                             =2: TRUE
CC                        (5): ESTIMATION OF LAYER HEIGHT
CC                             =0: NO
CC                             =1: ONE PARAMETER IN ALL
CC                             =2: ONE PARAMETER PER MODEL
CC                        (6): MODE OF TEMPORAL MODELING
CC                             =1: STATIC MODEL
CC                             =2: DYNAMIC MODEL
CC                        (7): TOTAL NUMBER OF MODELS
CC                        (8): MAPPING FUNCTION
CC                             =0: NONE
CC                             =1: 1/COS
CC                        (9): STATION-SPECIFIC MODELS
CC                        (10): COMPONENT TO BE ESTIMATED
CC                              =1: DETERMINISTIC
CC                              =2: STOCHASTIC
CC               POLGIM(I,J),I=1,2,3,J=1,..,OPTGIM(7):        R*8(3,*)
CC                        I=1: HEIGHT OF SINGLE LAYER (M)
CC                        I=2: LAT. OF NORTH GEOMAGNETIC POLE
CC                        I=3: EAST LONGITUDE
CC               EPOGIM(I,J),I=1,2,J=1,..,OPTGIM(7): PERIODS  R*8(2,*)
CC                        OF VALIDITY / REF EPOCHS (MJD)
CC               SCAGIM : SCALING FACTOR FOR                  R*8(*)
CC                        (1): ION. COEFFICIENTS
CC                        (2): SINGLE-LAYER HEIGHT
CC                        (3): DTEC PARAMETERS
CC               ANTRAO(J,I),J=1,2, I=1,..,NANRAO:            CH*20
CC                        RECEIVER (J=1) AND ANTENNA (J=2)
CC                        NAME FOR REQUEST I
CC               NUMRAO(2,I),I=1,..,NANRAO: ANTENNA NUMBERS   I*4
CC                        "FROM - TO" FOR REQUEST I
CC               PRNRAO(I),I=1,..,NANRAO: SAT.SYSTEM FOR      I*4
CC                        RECEIVER ANT. OFFSET REQUEST I
CC               CLKHED : CLOCK HEADER INFORMATION            T_CLKHEAD
CC                          %NUMREF=0: FIX REF-CLOCKS
CC                          %NUMREF=2: SUM FOR REF-CLOCKS
CC               CLKREC : %NEPO: EPOCHS WITH HIGHEST SAMPL.   T_CLKREC
CC                        %EPOCH(1): LAST EPOCH
CC                                (SEC. SINCE CLKHED%TFIRST)
CC               NDIFF  : DIFFERENCE TYPE                     I*4
CC                        NDIFF=0: ZERO DIFFERENCE
CC                        NDIFF=1: SINGLE DIFFERENCE
CC               IARC2  : ARC NUMBER FOR LEO                  I*4
CC               TBOUND2(K),K=1,2: START AND END OF           R*8
CC                        ARC "IARC2" IN MJD
CC               STNAME(I),I=1,..,NSTAT: STATION NAMES        CH*16(*)
CC               TIMSTC2: EPOCHS WITH STOCHASTIC IMPULSES     R*8(*,*,*,*)
CC                        LEO ONLY
CC               NSASPV(I),I=1,..,NANSPV: NUMBER OF           I*4
CC                        SATELLITES BELONGING TO ANTENNA
CC                        PHASE CENTER GROUP I
CC               SATSPV(J,I),J=1,..,NSASPV(I),I=1,..,NANSPV:  I*4
CC                        SATELLITE NUMBERS OF EACH ANTENNA
CC                        PHASE CENTER GROUP
CC               NADIR  : NADIR ANGLE (RECEIVER SEEN FROM     R*8(2)
CC                        THE SATELLITE; RAD)
CC               NADMAX : MAXIMUM NADIR ANGLE ALLOWED FOR     R*8
CC                        SAT. ANT. PATTERN ESTIMATION
CC               RAPZENMAX : MAXIMUM ZENITH ANGLE ALLOWED FOR R*8
CC                        REC. ANT. PATTERN ESTIMATION
CC               IPHSEP : ONLY PHASE FOR RESUBST OF EPO-PARAM. I*4
CC               ZENIT2(I),I=1,2: LEO ZENITH DISTANCES        R*8
CC               AZIMU2(I),I=1,2: LEO AZIMUTHS                R*8
CC               AZISOK : CORRECTED AZIMUTH ANGLE (RECEIVER   R*8(2)
CC                        SEEN FROM THE SATELLITE; ANGLE
CC                        COUNTS CLOCKWISE FROM THE Y-AXIS
CC                        WHEN LOOKING TOWARDS THE Z-AXIS;
CC                        RAD)
CC               NRGB   : RANGE BIAS REQUESTS                 I*4
CC               OPLOAD : SCALING FACTORS FOR VIENNA GRID FILES   T_OPTLOAD(3)
CC                        1: ATMOSPHERIC NON-TIDAL LOADING
CC                        2: OCEAN NON-TIDAL LOADING
CC                        3: HYDROSTATIC PRESSURE LOADING
CC               ZENMAX : MAXIMUM ZENITH DISTANCE (RAD)       R*8
CC         OUT : IPAMB  : PARAMETER INDEX FOR AMBIGUITY       I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/28 18:41
CC
CC CHANGES    :  13-FEB-92 : ??: CHANGES DUE TO ERP-ESTIMATION
CC               04-JUN-92 : ??: NEW PARAMETER "IORSYS" FOR J2000.0
CC                               NEW CALLS "COOTRA","GETRAD"
CC               23-JUL-92 : ??: GET MANOEUVRE TIMES AND NEW PARAMETER
CC                               "TBOUND"
CC               02-AUG-92 : ??: SR CALL OF PDPOL (DUE TO POLYNOME MODEL)
CC               06-AUG-92 : ??: INDEX FOR AMBIGUITY PARAMETER "IPAMB"
CC               14-AUG-92 : ??: NEW PARAMETER "NPARN"
CC               10-NOV-92 : ??: SETTING OF IPAMB WAS NOT CORRECTLY PLACED
CC               20-MAR-93 : ??: ESTIMATION OF STOCHASTIC ORBIT PARAMETERS
CC               03-APR-93 : ??: SATELLITE ANTENNA OFFSET PARAMETERS
CC               14-MAY-93 : ??: NEW PARAMETER TYPES (POTENTIAL, HILL,
CC                               ALBEDO, CENTER OF MASS)
CC               15-JUL-93 : ??: CORRECT TEST OF TROPOSPHERE BOUNDARIES
CC               02-AUG-93 : ??: SCALING OF P0/P2 FROM 1.D5 TO 1.D9
CC               05-NOV-93 : ??: ADD 1 SEC (DTBND1) TO TBOUND FOR MANOEUVRE
CC               27-DEC-93 : MR: TIME WINDOWS FOR SAT. ANTENNA OFFSETS
CC               28-NOV-93 : MR: HANDLE STOCHASTIC PARAMETERS FOR
CC                               MANOEUVRE SATELLITES. "SVN" REMOVED
CC                               FROM CALL TO "PDSTC"
CC               13-APR-94 : SS: DIFFERENTIAL IONOSPHERE PARAMETERS
CC               19-APR-94 : RW: CPO-MODEL PARAMETERS
CC                               SR PDPOL REPLACED BY SR PDTRNS
CC                               (ALLOWS TO ESTIMATE DEPS, DPSI)
CC               29-JUL-94 : MR: MAPPING FUNCTIONS
CC               10-AUG-94 : MR: CALL EXITRC
CC               17-AUG-94 : MR: MAXMAN=100 (OLD: MAXMAN=20)
CC               06-NOV-94 : MR: ANTENNA PHASE CENTER PARAMETERS
CC               24-NOV-94 : MR: REMOVE TEST OUTPUT FOR ANT.PHA.CEN.
CC               21-APR-95 : SS: NEW FUNCTION "ASLEFU" FOR APC-
CC                               PARAMETERS
CC               01-MAY-95 : SS: NEW SR "GIMARG" FOR GIM-PARAMETERS
CC               05-MAY-95 : MR: ALLOW BLANK RECEIVER NAME WHEN
CC                               ESTIMATING PHASE CENTER VARIATIONS
CC               06-JUN-95 : SS: GLOBAL IONOSPHERE MODEL PARAMETERS
CC               08-JUN-95 : LM: KINEMATIC COORDINATES
CC               25-AUG-95 : SS: EPOCH NUMBER ALWAYS KNOWN FOR SIPS
CC               28-SEP-95 : JJ: DECLARE MXNAMB AS C*6 INSTEAD I*4
CC               05-DEC-95 : SS: NEW IONOSPHERE MODEL (TYPE 2)
CC               05-DEC-95 : SS: USE "FACSLM(1)" (AND "FACSLM(2)")
CC               05-DEC-95 : SS: APPLY "SCAGIM"
CC               05-DEC-95 : SS: NEW SR PDGIM
CC               05-DEC-95 : SS: CALL WITH "WGSEPO"
CC               09-JAN-96 : GB: ACCOMODATE NEW ORBIT MODEL
CC                               SR PRTDER REPLACING RPARTN, GETRAD
CC                               FOR NEW ORBIT FORMAT, NEW SR PDSTC2.
CC                               LOCAL PARAMETER MAXVAR
CC               26-MAR-96 : MR: RECEIVER ANTENNA OFFSETS, "CSESS"
CC               06-JUN-96 : TS: REMOVED UNUSED VARIABLES
CC               29-AUG-96 : TS: EPOCHS OF STATION CLOCKS CHANGED
CC               08-APR-97 : SS: NIELL MAPPING, TROPOSPHERE GRADIENTS
CC               20-MAY-97 : MR: HELP VARIABLE "DERCLK" IN CALL PDCLK
CC               25-JUL-97 : LM: CORRECT CALL PRTDER (UNIX PROBLEM)
CC               14-AUG-97 : SS: DIFFERENTIAL CODE BIASES
CC               08-SEP-97 : SS: ESTIMATE ZENITH SIP PARAMETERS
CC               08-OCT-97 : MR: RATIO ZENITH/GRADIENT PARAMETERS
CC               22-OCT-97 : SS: CODE BIAS I/O FILES
CC               26-JAN-98 : SS: STATION-SPECIFIC GIMS
CC               19-MAR-98 : MR: USE FUNCTION MODSVN
CC               06-APR-98 : SS: SR PDGIM OPTIMIZED
CC               29-APR-98 : SS: DTEC LC
CC               04-MAY-98 : SS: INCLUDE 'COMFREQ.inc' FOR GLONASS
CC               18-MAY-98 : SS: CHECK SATELLITE NUMBER
CC               26-MAY-98 : SS: "IONDEV" REDIMENSIONED
CC               24-JUN-98 : HH: "WLGT" STATT "WLGTH"
CC               26-NOV-98 : SS: SR PDANT WITH "NDIFF"
CC               21-DEC-98 : SS: GPS/GLONASS DCBS FOR RECEIVERS
CC               25-AUG-99 : JJ: CHANGED CHDUMM FROM '' TO ' '
CC               27-JAN-00 : TS: CHANGES FOR CLOCK OUTPUT
CC               13-APR-00 : SS: ESTIMATE (P1-C1) CODE BIASES
CC               02-MAY-00 : SS: CHECK WHETHER RECEIVER MANAGEABLE
CC               10-MAY-00 : SS: CHECK STATEMENT MOVED
CC               30-JUN-00 : TS: SMALL CHANGE FOR KIN. STATIONS
CC               28-AUG-00 : MR: REMOVE ROUNDING OF OBS.TIME FOR TEST
CC                               OF TROPOSPHERE PARAMETER BOUNDARIES
CC               25-OCT-00 : RD: COUNT OBS. FOR CLOCKS
CC               22-NOV-00 : TS: USE WGSEPO FOR COORDINATE PART. DERIV.
CC               14-DEC-00 : SS: DO NOT STOP IN CASE OF INCOMPATIBLE
CC                               RECEIVERS
CC               16-MAY-01 : DS: ORBIT AND STOCHASTIC PARAMETERS FOR LEO
CC               16-MAY-01 : DS: NEW SR PRTDE2, STAFLG, LEOPRN
CC               16-MAY-01 : DS: NEW PARAMETERS: IARC2,TBOUND2,STNAME
CC               26-MAY-01 : DS: USE m_bern AND d_stacrux
CC               30-JUL-01 : DS: CALL PDGIM WITH NAMSTA
CC               20-MAR-01 : RD: DCB TRIMBLE 4700 (SEE IGS-MAIL 3236)
CC               26-JUN-01 : RD: PREVENT 0D0**0 FOR LOCAL TROPO.
CC               14-AUG-01 : MR: ADD PARAM. STNAME
CC               04-SEP-01 : SS: SR ASLEFU REPLACED BY SR ASLEF2;
CC                               APC-PARAMETER REPRESENTATION TYP 4
CC               19-SEP-01 : RD: IS12(2).EQ.0 FOR ZERO-DIFF CASE
CC                               THEREFORE (ICENTR(IS12(2))) IS UNDEF.
CC               27-NOV-01 : SS: ADD TRIMBLE 5700
CC               18-DEC-01 : MM: USE RECEIVER FILE TO IDENTIFY "SPECIAL"
CC                               RECEIVERS (C1-P2, C1-C2)
CC               29-JAN-02 : RD: A GENERIC EPOCH COMPUTATION FOR EPOCH PARAM.
CC               07-MAY-02 : SS: DCB UPDATE
CC               25-JUN-02 : RD/DS: MERGE VERSION BE AND MUNICH
CC               26-JUN-02 : RD: USE D_STACRX INSTEAD OF D_STACRUX
CC               30-JUN-02 : DS/HU: SAVE f90 FILENAMES
CC               12-OCT-02 : MR: USE "NEWORL" INSTEAD OF "NEWORB" FOR LEOS
CC               12-OCT-02 : DS: ALLOW PARTIALS FOR SEVERAL LEOs
CC               25-OCT-02 : DS: ESTIMATION OF THE LEO ANTENNA OFFSET
CC               30-OCT-02 : RD: ZERO-DIFF CASE FOR: REC.ANT.OFF (TYPE=5),
CC                               LOCAL IONO (TYPE=7),LOCAL TROPO (TYPE=9),
CC                               DIFF.IONO (TYPE=17),REC.ANT.PCV (TYPE=18)
CC               14-NOV-02 : RS: SATELLITE ANTENNA PHASE CENTER VARIATIONS
CC               15-NOV-02 : DS: PARTIALS FOR ERP AND GEOCENTER WITH LEO
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               23-FEB-03 : MR: CORRECT ORBIT PARTIALS FOR LEO SLR DATA
CC                               (DYNAMIC AND STOCHASTIC PARAMETERS)
CC               24-FEB-03 : RD: MORE IS12(2).EQ.0 FOR ZERO-DIFF CASE
CC               27-MAR-03 : RD: CORRECT TIME WINDOW CHECK FOR RCO
CC               11-MAY-03 : MM: PIECEWISE LINEAR TROPOSPHERE
CC               26-MAY-03 : RD: CORRECT DIMENSION OF WGSEPO (SEE PRCEPO)
CC               24-JUN-03 : HU: INTERFACE TO STAFLG
CC               11-AUG-03 : RS: CHANGE CALLS OF GETAZI
CC               08-SEP-03 : HU: ANTNAM, RECNAM CHR16 -> CHR20
CC               22-OCT-03 : RS: ADD RAPZENMAX
CC               10-DEC-03 : AJ: PIECEWISE CONSTANT ACCELERATIONS
CC               02-FEB-04 : MM: DABS INSTEAD OF ABS
CC               24-MAR-04 : DS: ESTIMATION OF THE LEO ANTENNA PATTERN
CC               08-APR-04 : RS: ADD AZISOK
CC               21-APR-04 : HU: ANTAZI -> -ANTAZI
CC               05-MAY-04 : HB: CORRECT SYNTAX ERROR (EPOCH-WISE AMBIGUITIES)
CC               06-APR-05 : AJ: INTERFACE TO PRTDER, PRTDE2
CC               26-MAY-05 : RD: USE DIMENSION MAXSGR FOR SATOFF AND SATSPV
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               24-AUG-06 : AG: TAN(Z) FOR GRADIENTS IMPLEMENTED
CC               04-OCT-06 : AG: SETUP OF SATELLITE SPECIFIC PCO/PCV
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC               17-Jun-07 : RD: ONLY PHASE FOR RESUBSTITUTION OF EPO-PARAM.
CC               26-FEB-08 : RD: USE GTSATM FROM D_SATCRX
CC               19-May-08 : DT: Lageos treated as GNSS orbit instead of LEO
CC                               (assuming SVN>=951)
CC               17-JUN-08 : RD: COUNTER FOR OBSERV. PER PARAMETER ADDED
CC               30-JUN-08 : RD: VMF ADDED
CC               28-OCT-08 : DT: Use maxVar from M_MAXDIM
CC               02-Apr-09 : DT: Add NRGB for Range Biases
CC               04-MAY-09 : RD: SCALING OF LOADING MODELS ADDED
CC               09-MAY-09 : RD: SAT/FRQ-SPECIFIC RECEIVER CLOCK BIASES
CC               09-MAY-09 : RD: SEPERATE RECEIVER CLOCKS FOR GPS/GLONASS
CC               04-JAN-10 : SL: HOI SCALING PARAMETERS ADDED
CC               03-MAR-10 : SL/RD: CORRECTION W.R.T. HOI FOR ZERO-DIFF
CC               08-SEP-10 : RD: MERGE SLR-TIME BIAS OPTION
CC               16-NOV-10 : RD: UPDATE INTERVAL FOR PIECE-WISE LINEAR PARAM.
CC               25-NOV-10 : MM: GNSS-SPECIFIC PARAMETERS
CC               03-DEC-10 : HB: ADD PARAMETER FOR SR PRTDER
CC               04-JAN-11 : PS: CHEN/HERRING GRADIENT MAPPING ADDED
CC               24-MAY-11 : PS: BUGFIX FOR CHEN/HERRING GRADIENT MAPPING
CC               24-JAN-12 : RD: SPEED UP THE STATION-WISE HOIS SCALING FACTORS
CC               04-MAY-12 : RD: USE DMOD FROM MODULE, REMOVE UNUSED VARIABLES
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, r8b, lfnerr, fileNameLength
      USE m_global, ONLY: g_svnsys
      USE m_maxdim, ONLY: maxgim, maxgit, maxsat, maxvar
      USE d_stacrx, ONLY: MTypeSPACE
      USE d_clkrnx, ONLY: t_clkhead,t_clkrec
      USE d_trpest, ONLY: DTMAX
      USE d_const,  ONLY: C, GM, HREF, PI, ARS
      USE d_satcrx, ONLY: gtsatm
      USE d_grid,   ONLY: grdNeq, getGrid, getGridKeyw
      USE p_gpsest, ONLY: maxmea, t_optLoad, t_parTyp
      USE l_basfun, ONLY: dmod
      USE s_pddcb
      USE s_dimtst
      USE s_prtde2
      USE s_pdgim
      USE s_prtder
      USE s_getazi
      USE s_hilder
      USE s_kepder
      USE s_pdcor
      USE s_cootra
      USE f_modsvn
      USE f_poldif
      USE s_pdstc
      USE f_parfac
      USE s_pdclk
      USE s_pdtrns
      USE s_staflg
      USE s_ionbsf
      USE s_rpartn
      USE s_pdant
      USE s_pdstc2
      USE s_ionosi
      USE s_exitrc
      USE s_leoprn
      USE f_aslef2
      USE s_gtflna
      USE s_trpmap
      USE s_stdorbit
      USE s_ionosp
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IA    , IARC  , IARC2 , IARCRP, IAZI  , ICAL  ,
     1          ICARR , ICLS  , ICOM  , ICOR  , ICRARC, IDEG  ,
     2          IEPO  , IFILE , IFIRST, IFLG  , IFORB , IFORB2, IFRQ  ,
     3          IGRP  , II    , IMAN  , IMO2  , IMOD  , IMOD1 , IND   ,
     4          INDSTA, INDX  , INTEPO, IO    , IOFR  , ION   , ION001,
     5          IORB  , IORB2 , IORS  , IORSYS, IP    , IPAMB , IPAR  ,
     6          IRAO  , IRC   , IRCRAD, IRCRPR, IREQ  , IS    , ISAALB,
     7          ISAOFF, ISASPV, ISAT  , ISEQ  , ISNG  , ISPV  , ISTA  ,
     8          ISTART, ITRMAP, ITYP  , ITYP1 , IZEN  , K     ,
     9          KDEG  , LEOIP , LEOMAN, LEONUM, LEORAD, MAXEQN, MAXMAN,
     1          MAXSNG, MEATYP, MELWUB, MODEL , MXCAMB,
     2          MXCLCQ, MXCSAT, MXCSGR, MXCSTC, MXCVAR, NAZI  ,
     3          NDIFF , NEWORB, NEWORL, NFREQ , NFRFIL, NMAN  , NORB  ,
     4          NPAR  , NPARN , NRAD  , NSNG  , NUMAMB, NVAR  , NZEN  ,
     5          IPHSEP, ISYS  , NRGB
      INTEGER(i4b) :: HILKEP=3
C
      REAL*8    AHELP2, AHLP  , AMBHLP, ANTAZI, AZIOK , B0    ,
     1          CZP1  , CZP2  , DH1   , DH2   , DIFFA , DIFFZ , DTBND1,
     2          DTIME , FACT  , HELP  , S0    , SCAALB,
     3          SCACEN, SCAHIL, SCAPOT, SZ    , TOBS  ,
     4          TOSC  , TSEC  , XNORM , XPOL  , YPOL  , ZDM   ,
     5          ZENOK , TISB  , DTSIM , ZENMAX
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      PARAMETER (MAXMAN=100)
C
      TYPE(t_clkhead) CLKHED
      TYPE(t_clkrec)  CLKREC
      TYPE(t_optLoad), DIMENSION(:) :: opLoad
      TYPE(t_parTyp),  DIMENSION(:) :: parTyp
C
      CHARACTER(LEN=fileNameLength), SAVE :: FILRPR,CHDUMM, LEORPR
      CHARACTER*20 MARTYP
      CHARACTER*20 RECTYP(2),ANTTYP(2),ANTCAL(2,*),ANTRAO(2,*)
      CHARACTER*16 STNAME(*)
      CHARACTER*8  ANLTYP
      CHARACTER*6  MXNLCQ,MXNSAT,MXNSTC,MXNAMB,MXNVAR,MXNSGR
      CHARACTER*4  CSESS(2)
C
      REAL*8       XVSAT(*),XSTAT(3,*),XTOPO(3,2),D(2),TUV(3,2)
      REAL*8       ELE(7),FACSIP(5),TBOUND(2),TBOUND2(2)
      REAL*8       DRDELE(3,MAXVAR),A(*),CLFT(2,*), DUM
      REAL*8       AHELP(*),AUX(3),RPRPAR(MAXVAR)
      REAL*8       DB(2),DS(2),ZP(2),TRPLMS(2,*),TRPLIM(2,*),ZENITH(2)
      REAL*8       IONDEV(10,MAXGIM),AZIMUT(2)
      REAL*8       TPOL(2,*),POLDER(2),TIMMAN(MAXMAN)
      REAL*8       TIMSTC(3,MXCSTC,MXCSAT,*),TIMOFF(2,*)
      REAL*8       TIMSTC2(3,MXCSTC,MXCSAT,*),SCASTC(*)
      REAL*8       DRDMOD(3,6),DRDACC(3,9),DRDPAR(3),DERCLK(2)
      REAL*8       POLGIM(3,*),EPOGIM(2,*),SCAGIM(*)
      REAL*8       XFACT,TTRP,DT12,TIMISB(3,*)
      REAL*8       WGSEPO(9,2),ELLS12(3,2),MAPFUN(2)
      REAL*8       NADIR(2),NADMAX,RAPZENMAX,AZISOK(2)
      REAL*8       ZENIT2(2),AZIMU2(2),HLPGRD(3)
      REAL*8       DUM_IONCOE(MAXGIT,1),DUM_IONSIG(MAXGIT,1)
      REAL*8       DR(2),HOI(3,2)
C
      INTEGER*4    LOCQ(MXCLCQ,*),IS12(2),ICENTR(*)
      INTEGER*4    SVN,SVNIP,SVNMAN,SATMAN(MAXMAN)
      INTEGER*4    NSAOFF(*),SATOFF(MXCSGR,*)
      INTEGER*4    NSAALB(*),SATALB(MXCSAT,*)
      INTEGER*4    AMBCLS(MXCAMB,3),AMBSAT(MXCAMB),AMBIEP(MXCAMB)
      INTEGER*4    AMBWLF(MXCAMB,2),IANTEN(2),NUMCAL(2,*),PRNCAL(*)
      INTEGER*4    IONREQ(6,MAXGIM),OPTGIM(*),NUMRAO(2,*),PRNRAO(*)
      INTEGER*4    OPTDIP(*),NOBSPA(:,:)
      INTEGER*4    ITRGRD(*),ONEOR2,NSASPV(*),SATSPV(MXCSGR,*)
      INTEGER*4    STCDEF(MAXSAT),STCDEF2(MAXSAT)
      INTEGER*4    DUM_NM(MAXGIT,2,1), DUM_NTERM(1)
C
      INTEGER*4    INDA(*)
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMSTC/MXCSTC,MXNSTC
      COMMON/MCMVAR/MXCVAR,MXNVAR
      COMMON/MCMSGR/MXCSGR,MXNSGR
C
      INCLUDE 'COMFREQ.inc'
C
      DATA ION001/0/
      DATA IFIRST/1/,IFORB/1/,IFORB2/1/
C
C INITIALIZATION
C --------------
      IF (IFIRST.EQ.1) THEN
        MXCVAR=MAXVAR
        MXNVAR='MAXVAR'
C
        CHDUMM = ' '
        CALL GTFLNA(0,'RPRCOE ',FILRPR,IRCRAD)
        CALL GTFLNA(0,'LEORPR ',LEORPR,LEORAD)
C
C GET SATELLITE MANOEUVRES
        CALL GTSATM(MAXMAN,NMAN,SATMAN,TIMMAN)
C
C EXTRAPOLATION INTERVAL AT ARC BOUNDARIES (1 SEC)
        DTBND1=1.D0/86400.D0
C
        IFIRST=0
      END IF
C
C CHECK SATELLITE NUMBER
C ----------------------
      IF (SVN.LE.0) THEN
        WRITE(LFNERR,901)
901     FORMAT(/,' *** SR SNDIFF: INVALID SATELLITE NUMBER',
     1    /,16X,'SATELLITE NUMBER: ',I4,/)
        CALL EXITRC(2)
      ENDIF
C
C DEFINE FACTORS CONCERNING STOCHASTIC IONOSPHERE PARAMETERS AND
C DIFFERENTIAL CODE BIASES
C --------------------------------------------------------------
      FACSIP(1)= 1.D0
      FACSIP(2)= FRQ(1,SVN)**2/FRQ(2,SVN)**2
      FACSIP(3)= 0.D0
      FACSIP(4)= FACSIP(1)-FACSIP(2)
      FACSIP(5)=-FRQ(1,SVN)/FRQ(2,SVN)
C
C COMPUTE TOPOCENTRIC DISTANCES AND TOPOCENTRIC UNIT VECTOR
C ---------------------------------------------------------
      DO 1 I=1,NDIFF+1
        D(I)=0.D0
        DO 2 K=1,3
          D(I)=D(I)+XTOPO(K,I)**2
2       CONTINUE
        D(I)=DSQRT(D(I))
        DO 3 K=1,3
          TUV(K,I)=XTOPO(K,I)/D(I)
3       CONTINUE
1     CONTINUE
C
C INITIALIZE PARAMETER NUMBER
C ---------------------------
      IORB=0
      IORB2=0
      IP=0
      IPAMB=0
      DO 4 ISAT=1,MAXSAT
        STCDEF(ISAT)=0
        STCDEF2(ISAT)=0
4     CONTINUE

      DO 5000 II=1,NPAR
C
C INITIALIZE ELEMENT NR IP
      IP=IP+1
      AHELP(IP)=0.D0
C
C DETECT TYPE OF PARAMETER
      ITYP=LOCQ(1,IP)
      GOTO ( 100, 200, 300, 400, 500, 600, 700, 800, 900,1000,
     1      1100,1200,1300,1300,1300,1400,1700,1800,1900,2000,
     2      2100,2200,2300,2400,2500,2600,2700,5000,5000,3000) ITYP
      CALL EXITRC(2)
C
C STATION COORDINATES
C -------------------
100   IS=LOCQ(2,IP)
      IF(IS.NE.ICENTR(IS12(1)).AND.IS12(2).EQ.0) GOTO 1500
      IF (IS12(2).NE.0) THEN
        IF(IS.NE.ICENTR(IS12(1)).AND.IS.NE.ICENTR(IS12(2))) GOTO 1500
      ENDIF
      IF(ICARR.EQ.4 .OR. MELWUB.GT.0) GOTO 1500
      IF(IS.NE.ICENTR(IS12(1))) GOTO 50
C
      CALL PDCOR(XTOPO(1,1),D(1),SZ,XPOL,YPOL,AUX)
      DO 10 I=1,3
        AHELP(IP)=-AUX(I)
        IP=IP+1
10    CONTINUE
      IP=IP-1
      GOTO 1500
C
50    CONTINUE
      CALL PDCOR(XTOPO(1,2),D(2),SZ,XPOL,YPOL,AUX)
      DO 11 I=1,3
        AHELP(IP)=AUX(I)
        IP=IP+1
11    CONTINUE
      IP=IP-1
      GOTO 1500
C
C CLOCK PARAMETERS
C ----------------
200   IS=LOCQ(2,IP)
      IREQ=LOCQ(3,IP)
C
      IF(IS.NE.IS12(1).AND.IS12(2).EQ.0)  GOTO 1500
      IF(IS.NE.IS12(1).AND.IS.NE.IS12(2)) GOTO 1500
C
      IF(LOCQ(6,IP).NE.5) THEN
        IF(TOBS.LT.CLFT(1,IREQ)) GOTO 1500
        IF(TOBS.GT.CLFT(2,IREQ)) GOTO 1500
        IF(LOCQ(6,IP).EQ.0.AND.MEATYP.EQ.2) GOTO 1500
        IF(LOCQ(6,IP).NE.0.AND.MEATYP.NE.2) GOTO 1500
      ENDIF
      IF(LOCQ(6,IP).EQ.0.AND.MEATYP.EQ.3.AND.
     1   LOCQ(7,IP).NE.0.AND.LOCQ(7,IP).NE.SVN)             GOTO 1500
      IF(LOCQ(6,IP).EQ.1.AND.LOCQ(4,IP).NE.FREQNM(SVN))     GOTO 1500
      IF(LOCQ(6,IP).EQ.1.AND.G_SVNSYS(INT(SVN/100)).NE.'R') GOTO 1500
      IF(LOCQ(6,IP).EQ.2.AND.LOCQ(4,IP).NE.SVN)             GOTO 1500
      IF(LOCQ(6,IP).EQ.4.AND.G_SVNSYS(INT(SVN/100)).NE.'R') GOTO 1500
      IF(LOCQ(6,IP).EQ.5.AND.G_SVNSYS(INT(SVN/100)).NE.'R') GOTO 1500
C
C     FIRST STATION
      IF(IS.EQ.IS12(1)) THEN
C COMPUTE INTERPOLATION FACTOR
        XFACT=1D0
        IF (LOCQ(6,IP).EQ.5) THEN
          XFACT=0d0
          TISB=TIMISB(1,LOCQ(4,IP))
          IF (TIMISB(2,LOCQ(4,IP)).LE.TOBS.AND.
     1        TIMISB(1,LOCQ(4,IP)).GE.TOBS) THEN
            DT12=DABS(TIMISB(2,LOCQ(4,IP))-TIMISB(1,LOCQ(4,IP)))
            XFACT=PARFAC(TOBS,TISB,DT12,DTSIM,PARTYP(IP))
          ENDIF
          IF (TIMISB(1,LOCQ(4,IP)).LE.TOBS.AND.
     1        TIMISB(3,LOCQ(4,IP)).GE.TOBS) THEN
            DT12=DABS(TIMISB(1,LOCQ(4,IP))-TIMISB(3,LOCQ(4,IP)))
            XFACT=PARFAC(TOBS,TISB,DT12,DTSIM,PARTYP(IP))
          ENDIF
          IF (XFACT.EQ.0.D0) GOTO 1500
        ENDIF
C       CALL PDCLK(1,XTOPO(1,1),D(1),XVSAT,XSTAT(1,IS),0d0,SZ,DERCLK)
        CALL PDCLK(1,XTOPO(1,1),D(1),XVSAT,WGSEPO(1,1),0d0,SZ,DERCLK)
        AHELP(IP)=AHELP(IP)-(DERCLK(1)+DERCLK(2)/C)/C
        IF (LOCQ(6,IP).NE.0) AHELP(IP)=XFACT*(AHELP(IP)-1d0)
        IF (LOCQ(6,IP).EQ.4) THEN
          AHELP(IP)=AHELP(IP)*(FREQNM(SVN)-6)**(LOCQ(4,IP)-1)
        ENDIF
C ?? TIME BIAS, NOT SURE THAT THIS IS CORRECT ??
C        IF (MEATYP.EQ.3) AHELP(IP) = 1D0
      ENDIF
C
C     SECOND STATION
      IF(IS.EQ.IS12(2)) THEN
C COMPUTE INTERPOLATION FACTOR
        XFACT=1D0
        IF (LOCQ(6,IP).EQ.5) THEN
          XFACT=0d0
          TISB=TIMISB(1,LOCQ(4,IP))
          IF (TIMISB(2,LOCQ(4,IP)).LE.TOBS.AND.
     1        TIMISB(1,LOCQ(4,IP)).GE.TOBS) THEN
            DT12=DABS(TIMISB(2,LOCQ(4,IP))-TIMISB(1,LOCQ(4,IP)))
            XFACT=PARFAC(TOBS,TISB,DT12,DTSIM,PARTYP(IP))
          ENDIF
          IF (TIMISB(1,LOCQ(4,IP)).LE.TOBS.AND.
     1        TIMISB(3,LOCQ(4,IP)).GE.TOBS) THEN
            DT12=DABS(TIMISB(1,LOCQ(4,IP))-TIMISB(3,LOCQ(4,IP)))
            XFACT=PARFAC(TOBS,TISB,DT12,DTSIM,PARTYP(IP))
          ENDIF
          IF (XFACT.EQ.0.D0) GOTO 1500
        ENDIF
C        CALL PDCLK(1,XTOPO(1,2),D(2),XVSAT,XSTAT(1,IS),0d0,SZ,DERCLK)
        CALL PDCLK(1,XTOPO(1,2),D(2),XVSAT,WGSEPO(1,2),0d0,SZ,DERCLK)
        AHELP(IP)=AHELP(IP)+(DERCLK(1)+DERCLK(2)/C)/C
        IF (LOCQ(6,IP).NE.0) AHELP(IP)=XFACT*(AHELP(IP)+1d0)
        IF (LOCQ(6,IP).EQ.4) THEN
          AHELP(IP)=AHELP(IP)*(FREQNM(SVN)-6)**(LOCQ(4,IP)-1)
        ENDIF
C ?? TIME BIAS, NOT SURE THAT THIS IS CORRECT ??
C        IF (MEATYP.EQ.3) AHELP(IP) = -1D0
      ENDIF
      GOTO 1500
C
C ORBIT PARAMETERS
C ----------------
300   SVNIP=LOCQ(3,IP)
C Lageos treated as GNSS orbit (assuming SVN>=951 for geodetic SLR satellites)
      IF(SVNIP.GE.900 .AND. SVNIP.LT.951) GOTO 360
      IF(IARC.NE.LOCQ(2,IP)) GOTO 1500
      IF(MODSVN(SVNIP).NE.SVN) GOTO 1500
C
C FIND OUT ABOUT ORBIT FORMAT
C ---------------------------
      IF(IFORB.EQ.1)THEN
        IFORB=0
        CALL PRTDER(CHDUMM,SVNIP,1,0,1,TOBS,1,ICRARC,IORS,
     1              NVAR,NRAD,DRDELE,ELE,RPRPAR,ANLTYP,IRC)
        IF(NVAR.EQ.15.OR.NVAR.EQ.18.OR.NVAR.EQ.21)THEN
          NEWORB=1
        ELSE
          NEWORB=0
        END IF
      END IF
C
C MANOEUVRE SATELLITE ?
      SVNMAN=SVN
      DO 305 IMAN=1,NMAN
        IF (SATMAN(IMAN).EQ.SVN.AND.
     1      TIMMAN(IMAN).GT.TBOUND(1)-DTBND1.AND.
     2      TIMMAN(IMAN).LE.TOBS) THEN
          SVNMAN=SVNMAN+50
          GOTO 307
        ENDIF
305   CONTINUE
307   CONTINUE
      IF (SVNMAN.NE.SVNIP) GOTO 1500
C
      NORB=LOCQ(5,IP)
C
      IF(NEWORB.EQ.0)THEN
        CALL RPARTN(IARC,SVNIP,GM,TSEC,0.D0,ELE(1),ELE(2),ELE(3),
     1              ELE(4),ELE(5),ELE(7),DRDELE)
      ELSE
        DO 306 IORB=1,6
          CALL PRTDER(CHDUMM,SVNIP,IORB,0,1,TOBS,1,ICRARC,IORS,NVAR,
     1                NRAD,DRDELE(1,IORB),ELE,RPRPAR,ANLTYP,IRC)
306     CONTINUE
      END IF
      IORB=1
      DO IO=1,6
        DO K=1,3
          DRDMOD(K,IO)=DRDELE(K,IO)
        ENDDO
      ENDDO
      IF(IRCRAD.EQ.0) THEN
        DO 320 IPAR=1,NRAD
          IF(NEWORB.EQ.0)THEN
            ISTART=0
          ELSE
            ISTART=6
          END IF
          CALL PRTDER(CHDUMM,SVN,ISTART+IPAR,0,1,TOBS,1,IARCRP,
     1                IORSYS,NVAR,NRAD,DRDELE(1,6+IPAR),ELE,RPRPAR,
     2                ANLTYP,IRCRPR)
          DO 310 K=1,3
            IF(IPAR.LE.3) DRDACC(K,IPAR+3)=DRDELE(K,6+IPAR)/SCASTC(2)
            IF(IPAR.GT.9.AND.IPAR.LE.12)
     1        DRDACC(K,IPAR-9)=DRDELE(K,6+IPAR)/SCASTC(2)
            IF(IPAR.GT.12) DRDACC(K,IPAR-6)=DRDELE(K,6+IPAR)/SCASTC(2)
            DRDELE(K,6+IPAR)=DRDELE(K,6+IPAR)/1.D9
310       CONTINUE
320     CONTINUE
      END IF
      IP=IP-1
      DO 350 I=1,NORB
        IP=IP+1
        ISEQ=LOCQ(4,IP)
        HELP=0.D0
        CALL COOTRA(IORSYS,0,TOBS,DRDELE(1,ISEQ),
     1              DUM,DUM,DUM,DUM)
        DO 340 K=1,3
          IF (NDIFF.EQ.1) THEN
            HELP=HELP+(XTOPO(K,1)/D(1)-XTOPO(K,2)/D(2))*DRDELE(K,ISEQ)
          ELSE
            HELP=HELP+(XTOPO(K,1)/D(1))*DRDELE(K,ISEQ)
          ENDIF
340     CONTINUE
        IF(ISEQ.GT.1.AND.ISEQ.LE.6) THEN
          AHELP(IP)=HELP/ARS
        ELSE
          AHELP(IP)=HELP
        END IF
350   CONTINUE
      GOTO 1500
C
C ORBIT PARAMETERS FOR LEO
C ------------------------
360   LEOIP=LOCQ(3,IP)
      IF(IARC2.NE.LOCQ(2,IP)) GOTO 1500
      DO I=1,NDIFF+1
        CALL STAFLG(STNAME(IS12(I)),TOBS,IFLG,MARTYP)
        IF (MARTYP.EQ.MTypeSPACE) THEN
          CALL LEOPRN(STNAME(IS12(I)),TOBS,LEONUM)
          ONEOR2=I
          IF (MODSVN(LEOIP).EQ.LEONUM) GOTO 365
        END IF
      END DO
C
C LEO AS "SATELLITE" AND NOT AS "STATION" (E.G. FOR SLR)
      LEONUM=SVN
      IF (MODSVN(LEOIP).NE.LEONUM) GOTO 1500
      ONEOR2=0
365   CONTINUE
C
C FIND OUT ABOUT ORBIT FORMAT FOR LEO
C -----------------------------------
      IF(IFORB2.EQ.1)THEN
        IFORB2=0
        CALL PRTDE2(LEORPR,LEOIP,1,0,1,TOBS,1,ICRARC,IORS,
     1              NVAR,NRAD,DRDELE,ELE,RPRPAR,ANLTYP,IRC)
        IF(NVAR.EQ.15.OR.NVAR.EQ.18.OR.NVAR.EQ.21)THEN
          NEWORL=1
        ELSE
          NEWORL=0
        END IF
      END IF
C
C MANOEUVRE LEO ?
      LEOMAN=LEONUM
      DO IMAN=1,NMAN
        IF (SATMAN(IMAN).EQ.LEONUM.AND.
     1      TIMMAN(IMAN).GT.TBOUND2(1)-DTBND1.AND.
     2      TIMMAN(IMAN).LE.TOBS) THEN
          LEOMAN=LEOMAN+50
          GOTO 395
        ENDIF
      END DO
395   CONTINUE
      IF (LEOMAN.NE.LEOIP) GOTO 1500
C
      NORB=LOCQ(5,IP)
C
      IF(NEWORL.EQ.0)THEN
        CALL RPARTN(IARC2,LEOIP,GM,TSEC,0.D0,ELE(1),ELE(2),ELE(3),
     1              ELE(4),ELE(5),ELE(7),DRDELE)
      ELSE
        DO IORB2=1,6
          CALL PRTDE2(LEORPR,LEOIP,IORB2,0,1,TOBS,1,ICRARC,IORS,NVAR,
     1                NRAD,DRDELE(1,IORB2),ELE,RPRPAR,ANLTYP,IRC)
        END DO
      END IF
      IORB2=1
      DO IO=1,6
        DO K=1,3
          DRDMOD(K,IO)=DRDELE(K,IO)
        END DO
      END DO
      IF(LEORAD.EQ.0) THEN
        DO IPAR=1,NRAD
          IF(NEWORL.EQ.0)THEN
            ISTART=0
          ELSE
            ISTART=6
          END IF
          CALL PRTDE2(LEORPR,LEOIP,ISTART+IPAR,0,1,TOBS,1,IARCRP,
     1                IORSYS,NVAR,NRAD,DRDELE(1,6+IPAR),ELE,RPRPAR,
     2                ANLTYP,IRCRPR)
          DO K=1,3
            IF(IPAR.LE.3) DRDACC(K,IPAR+3)=DRDELE(K,6+IPAR)/SCASTC(2)
            IF(IPAR.GT.9.AND.IPAR.LE.12)
     1        DRDACC(K,IPAR-9)=DRDELE(K,6+IPAR)/SCASTC(2)
            IF(IPAR.GT.12) DRDACC(K,IPAR-6)=DRDELE(K,6+IPAR)/SCASTC(2)
            DRDELE(K,6+IPAR)=DRDELE(K,6+IPAR)/1.D9
          END DO
        END DO
      END IF
      IP=IP-1
      DO I=1,NORB
        IP=IP+1
        ISEQ=LOCQ(4,IP)
        HELP=0.D0
        CALL COOTRA(IORSYS,0,TOBS,DRDELE(1,ISEQ),
     1              DUM,DUM,DUM,DUM)
        DO K=1,3
          IF (NDIFF.EQ.1) THEN
            IF (ONEOR2.EQ.0) THEN
              HELP=HELP+(XTOPO(K,1)/D(1))*DRDELE(K,ISEQ)
            ELSE IF (ONEOR2.EQ.1) THEN
              HELP=HELP-(XTOPO(K,1)/D(1))*DRDELE(K,ISEQ)
            ELSE
              HELP=HELP+(XTOPO(K,2)/D(2))*DRDELE(K,ISEQ)
            END IF
          ELSE
            IF (ONEOR2.EQ.0) THEN
              HELP=HELP+(XTOPO(K,1)/D(1))*DRDELE(K,ISEQ)
            ELSE
              HELP=HELP-(XTOPO(K,1)/D(1))*DRDELE(K,ISEQ)
            ENDIF
          ENDIF
        END DO
        IF(ISEQ.GT.1.AND.ISEQ.LE.6) THEN
          AHELP(IP)=HELP/ARS
        ELSE
          AHELP(IP)=HELP
        END IF
      END DO
      GOTO 1500
C
C AMBIGUITY PARAMETERS
C --------------------
400   CONTINUE
      IF (MELWUB.EQ.2) GOTO 1500
      IF (IFILE.NE.LOCQ(2,IP)) GOTO 1500
      IF (ICARR.EQ.2) THEN
        IFRQ=2
      ELSE IF (ICARR.EQ.5) THEN
        IFRQ=3
      ELSE
        IFRQ=1
      END IF
C FIND AMBIGUITY INDEX
      DO 410 IA=NUMAMB,1,-1
        IF(AMBSAT(IA).EQ.SVN .AND.
     1     AMBIEP(IA).LE.IEPO) THEN
          GO TO 411
        END IF
410   CONTINUE
      IA=1
411   CONTINUE
      ICLS=AMBCLS(IA,IFRQ)
      IF (LOCQ(3,IP) .NE. ICLS) GO TO 1500
      IF(ICARR.NE.5) THEN
        IF(NFREQ.NE.1.AND.LOCQ(6,IP).GT.1) THEN
          AMBHLP=FACLIN(ICARR,1,SVN)*WLGT(1,SVN)/AMBWLF(IA,1) +
     1           FACLIN(ICARR,2,SVN)*WLGT(2,SVN)/AMBWLF(IA,1)
        ELSE
          IF(NFREQ.NE.1) THEN
            AMBHLP=FACLIN(ICARR,1,SVN)*WLGT(1,SVN)/AMBWLF(IA,1) +
     1             FACLIN(ICARR,2,SVN)*WLGT(2,SVN)/AMBWLF(IA,2)
          ELSE
            AMBHLP=FACLIN(ICARR,1,SVN)*WLGT(1,SVN)/AMBWLF(IA,1)
          ENDIF
        ENDIF
      ELSE
        AMBHLP=WLGT(ICARR,SVN)/AMBWLF(IA,1)/AMBWLF(IA,2)
      ENDIF
      IF(ICARR.EQ.LOCQ(5,IP)) AHELP(IP)=AMBHLP
      IF(NFRFIL.EQ.2.AND.ICARR.EQ.2.AND.LOCQ(5,IP).EQ.1.AND.
     1   LOCQ(6,IP).GT.1)       AHELP(IP)=AMBHLP
      IF(NFRFIL.EQ.2.AND.ICARR.EQ.4.AND.LOCQ(5,IP).EQ.3.AND.
     1   LOCQ(6,IP).GT.1)       AHELP(IP)=AMBHLP
      IF (AHELP(IP).NE.0.D0) IPAMB=IP-NPARN
      GOTO 1500
C
C RECEIVER ANTENNA OFFSETS
C ------------------------
500   CONTINUE
      IRAO =LOCQ(2,IP)
      IFRQ =LOCQ(3,IP)
      ICOR =LOCQ(4,IP)
C
C CHECK FREQUENCY
      IF (IFRQ.EQ.1.AND.ICARR.NE.1) GOTO 1500
      IF (IFRQ.EQ.2.AND.ICARR.NE.2) GOTO 1500
      IF (IFRQ.EQ.3.AND.ICARR.NE.1.AND.ICARR.NE.2.AND.
     1                  ICARR.NE.3.AND.ICARR.NE.5) GOTO 1500
      IF (IFRQ.EQ.4.AND.ICARR.NE.4) GOTO 1500
C
C CHECK SATELLITE SYSTEM
      IF (INT(SVN/100).NE.PRNRAO(IRAO) .AND.
     1    PRNRAO(IRAO).NE.10) GOTO 1500
C
      IF (IFRQ.LT.3) THEN
        FACT=FACLIN(ICARR,IFRQ,SVN)
      ELSE
        FACT=1D0
      ENDIF
C
      DO 510 I=1,NDIFF+1
        IF ((RECTYP(I).EQ.ANTRAO(1,IRAO) .OR. ANTRAO(1,IRAO).EQ.' ')
     1                                  .AND.
     2      ANTTYP(I).EQ.ANTRAO(2,IRAO) .AND.
     3      IANTEN(I).GE.NUMRAO(1,IRAO) .AND.
     4      IANTEN(I).LE.NUMRAO(2,IRAO)) THEN
C
C GET ANTENNA ORIENTATION TO CORRECT THE AZIMUTH
          CALL GETAZI(' ',ANTTYP(I),IANTEN(I),CSESS(1),ANTAZI)

          CALL STAFLG(STNAME(IS12(I)),TOBS,IFLG,MARTYP)
          IF (MARTYP.EQ.MTypeSPACE) THEN
            AZIOK=AZIMU2(I)-ANTAZI
            ZENOK=ZENIT2(I)
          ELSE
            AZIOK=AZIMUT(I)-ANTAZI
            ZENOK=ZENITH(I)
          END IF
C
          IF (ICOR.EQ.1) THEN
            AHLP=-DCOS(AZIOK)*DSIN(ZENOK)
          ELSEIF (ICOR.EQ.2) THEN
            AHLP=-DSIN(AZIOK)*DSIN(ZENOK)
          ELSE
            AHLP=-DCOS(ZENOK)
          ENDIF
          AHLP=AHLP*(-1.D0)**(I-1)*FACT
          AHELP(IP)=AHELP(IP)+AHLP
        END IF
510   CONTINUE
      GO TO 1500
C
C STATION SPECIFIC TROPOSPHERE PARAMETERS:
C ---------------------------------------
600   CONTINUE
      IMOD=LOCQ(2,IP)
      ISTA=LOCQ(3,IP)
      ICOM=LOCQ(4,IP)
      IMO2=LOCQ(7,IP)
C
C STRANGE ROUNDING OF TOBS REMOVED (28-AUG-00) BECAUSE OF PROBLEM
C WITH INTRODUCTION OF TROP. ESTIMATES INTO GPSEST
CCCCC
CCCCC ROUNDING OF TOBS (PROBLEMS WITH PARAMETER BOUNDARIES)
CCCC      TROUND=1.D+8
CCCC      TNEW=(DNINT((TOBS-DINT(TOBS))*TROUND))/TROUND+DINT(TOBS)
CCCC      IF(TNEW.GE.TRPLMS(1,IMOD).AND.TNEW.LT.TRPLMS(2,IMO2)) THEN
C

C
C PIECEWISE LINEAR TROPOSPHERE
C ----------------------------
      IF (ICOM.EQ.3) THEN
        TTRP=TRPLMS(1,IMOD)
        DT12=DABS(TRPLMS(2,IMOD)-TRPLMS(1,IMOD))
      ELSE
        TTRP=TRPLMS(1,IMOD)
        DT12=DABS(ITRGRD(2)*(TRPLMS(2,IMOD)-TRPLMS(1,IMOD)))
      END IF
C
C COMPUTE INTERPOLATION FACTOR
      XFACT=PARFAC(TOBS,TTRP,DT12,DTMAX,PARTYP(IP))
      IF (XFACT.EQ.0.D0) GOTO 1500
C
C DEAL WITH "BOUNDARY PROBLEM"
C      DTTROP=DTMAX/86400.D0*DT12
C      XFACT=(TOBS-TTRP)/DT12
C      IF (DABS(XFACT+1).LE.DTTROP) XFACT=-1
C      IF (DABS(XFACT).LE.DTTROP)   XFACT= 0
C      IF (DABS(XFACT-1).LE.DTTROP) XFACT= 1
C
C COMPUTE INTERPOLATION FACTOR
C      IF (XFACT.GT.-1.D0.AND.XFACT.LE.0.D0) THEN
C        XFACT=1+XFACT
C      ELSE IF (XFACT.GT.0.D0.AND.XFACT.LT.1.D0) THEN
C        XFACT=1-XFACT
C      ELSE
C        GOTO 1500
C      END IF
C
C FIRST OR SECOND STATION?
      INDSTA=0
      IF(ISTA.EQ.IS12(1)) THEN
        INDSTA=1
      ELSEIF(ISTA.EQ.IS12(2)) THEN
        INDSTA=2
        XFACT=-XFACT
      ENDIF
C
C COMPUTE PARTIAL DERIVATIVE
      IF (INDSTA.NE.0) THEN
        IF (ICOM.EQ.1) THEN
C NORTH/SOUTH GRADIENTS
          IF (ITRGRD(1).EQ.1) THEN
            CALL TRPMAP(ITRMAP,TOBS,ELLS12(1,INDSTA),ZENITH(INDSTA),
     1                  MAPFUN)
            AHELP(IP)=XFACT*MAPFUN(2)*DCOS(AZIMUT(INDSTA))
          ELSEIF (ITRGRD(1).EQ.2) THEN
            AHELP(IP)=XFACT*DTAN(ZENITH(INDSTA))*DCOS(AZIMUT(INDSTA))
          ELSEIF (ITRGRD(1).EQ.3) THEN
            CALL TRPMAP(ITRMAP,TOBS,ELLS12(1,INDSTA),ZENITH(INDSTA),
     1                  MAPFUN)
            AHELP(IP)=XFACT*MAPFUN(1)*DTAN(ZENITH(INDSTA))*
     1                                          DCOS(AZIMUT(INDSTA))
C CHEN and HERRING
          ELSEIF (ITRGRD(1).EQ.4) THEN
            AHELP(IP)=XFACT/(DTAN(pi/2.D0-ZENITH(INDSTA))*
     1       DCOS(ZENITH(INDSTA))+0.0032D0)*DCOS(AZIMUT(INDSTA))
          ENDIF
C EAST/WEST GRADIENTS
        ELSEIF (ICOM.EQ.2) THEN
          IF (ITRGRD(1).EQ.1) THEN
            CALL TRPMAP(ITRMAP,TOBS,ELLS12(1,INDSTA),ZENITH(INDSTA),
     1                  MAPFUN)
            AHELP(IP)=XFACT*MAPFUN(2)*DSIN(AZIMUT(INDSTA))
          ELSEIF (ITRGRD(1).EQ.2) THEN
            AHELP(IP)=XFACT*DTAN(ZENITH(INDSTA))*DSIN(AZIMUT(INDSTA))
          ELSEIF (ITRGRD(1).EQ.3) THEN
            CALL TRPMAP(ITRMAP,TOBS,ELLS12(1,INDSTA),ZENITH(INDSTA),
     1                  MAPFUN)
            AHELP(IP)=XFACT*MAPFUN(1)*DTAN(ZENITH(INDSTA))*
     1                                          DSIN(AZIMUT(INDSTA))
C CHEN and HERRING
          ELSEIF (ITRGRD(1).EQ.4) THEN
            AHELP(IP)=XFACT/(DTAN(pi/2.D0-ZENITH(INDSTA))*
     1       DCOS(ZENITH(INDSTA))+0.0032D0)*DSIN(AZIMUT(INDSTA))
          ENDIF
        ELSE
          CALL TRPMAP(ITRMAP,TOBS,ELLS12(1,INDSTA),ZENITH(INDSTA),
     1                MAPFUN)
          AHELP(IP)=XFACT*MAPFUN(1)
        ENDIF
      ENDIF
      GOTO 1500
C
C IONOSPHERE REQUEST
C ------------------
700   IMOD=LOCQ(2,IP)
      IF(ICARR.EQ.3) GOTO 1500
      IF(ION001.EQ.0) THEN
        ION001=1
        CALL IONOSI(0,MODEL,IONREQ,IONDEV,
     1              DUM_NTERM,DUM_NM, DUM_IONCOE,DUM_IONSIG)

      END IF
      IF(TOBS.LT.IONDEV(4,IMOD).OR.TOBS.GT.IONDEV(5,IMOD)) GOTO 1500
      B0=IONDEV(1,IMOD)
      S0=DMOD(IONDEV(2,IMOD),1.D0)*2*PI+IONDEV(3,IMOD)-PI
      S0=DMOD(S0+10*PI,2*PI)
      DO 710 I=1,NDIFF+1
        CALL IONBSF(XSTAT(1,IS12(I)),XVSAT,TOBS,SZ,ZENITH(I),
     1             IONREQ(4,IMOD)*1000.D0,ICARR,B0,S0,
     2             IONDEV(6,IMOD),IONDEV(7,IMOD),IONDEV(8,IMOD),
     3             FACT,DB(I),DS(I),ZP(I))
710   CONTINUE
      CZP1=DCOS(ZP(1))
      IF (NDIFF.EQ.1) CZP2=DCOS(ZP(2))
C
C SATISFY ENTIRE IONOSPHERE-REQUEST
      IP=IP-1
      DO 790 ION=1,1000
        IP=IP+1
        ITYP1=LOCQ(1,IP)
        IMOD1=LOCQ(2,IP)
        IF(ITYP1.EQ.ITYP.AND.IMOD1.EQ.IMOD) GOTO 720
        IP=IP-1
        GOTO 1500
720     KDEG=LOCQ(3,IP)
        IDEG=LOCQ(4,IP)
        IF (NDIFF.EQ.0) THEN
          AHELP(IP)=FACT*(DS(1)**IDEG*DB(1)**KDEG/CZP1)
        ELSE
          AHELP(IP)=FACT*(DS(1)**IDEG*DB(1)**KDEG/CZP1
     1                   -DS(2)**IDEG*DB(2)**KDEG/CZP2)
        ENDIF
        IF(MEATYP.EQ.2)AHELP(IP)=-AHELP(IP)
790   CONTINUE
      GOTO 1500
C
C DIFFERENTIAL CODE BIASES
C ------------------------
800   CONTINUE
      IF (MELWUB.EQ.1) THEN
        CALL PDDCB (LOCQ(:,IP),2,6,SVN,IS12,RECTYP,
     1              NDIFF,TOBS,AHELP(IP))
      ELSE
        CALL PDDCB (LOCQ(:,IP),MEATYP,ICARR,SVN,IS12,RECTYP,
     1              NDIFF,TOBS,AHELP(IP))
      ENDIF
      GOTO 1500
C
C LOCAL TROPOSPHERE PARAMETERS
C ----------------------------
900   IF(ICARR.EQ.4) GOTO 1500
      IMOD=LOCQ(2,IP)
      IDEG=LOCQ(3,IP)-1
      IF(TOBS.GE.TRPLIM(1,IMOD).AND.TOBS.LT.TRPLIM(2,IMOD)) THEN
C
        AHELP2=0D0
        IF (NDIFF.EQ.1) THEN
          DH2=(ELLS12(3,2)-HREF)/100.D0
          IF (IDEG .EQ. 0) THEN
            AHELP2=DH2/DCOS(ZENITH(2))
          ELSE
            AHELP2=DH2**IDEG/DCOS(ZENITH(2))
          ENDIF
        ENDIF
C
        DH1=(ELLS12(3,1)-HREF)/100.D0
        IF (IDEG .EQ. 0) THEN
          AHELP(IP)=DH1/DCOS(ZENITH(1))-AHELP2
        ELSE
          AHELP(IP)=DH1**IDEG/DCOS(ZENITH(1))-AHELP2
        ENDIF
      ENDIF
      GOTO 1500
C
C EARTH ROTATION PARAMETER
C ------------------------
1000  IF(LOCQ(2,IP).NE.1) GOTO 1500
      IF(TOBS.GE.TPOL(1,LOCQ(3,IP)).AND.TOBS.LT.TPOL(2,LOCQ(3,IP)))THEN
        INDX=LOCQ(3,IP)
        DO 1005 I=1,NDIFF+1
          CALL STAFLG(STNAME(IS12(I)),TOBS,IFLG,MARTYP)
          IF ( .NOT. (MARTYP.EQ.' ')) GOTO 1005
          CALL PDTRNS(LOCQ(4,IP),LOCQ(5,IP),TUV(1,I),XVSAT,TPOL(1,INDX),
     1                TOBS,SZ,POLDER(I))
          IF (I.EQ.1) THEN
            AHELP(IP)=AHELP(IP)+POLDER(I)
          ELSE
            AHELP(IP)=AHELP(IP)-POLDER(I)
          END IF
1005    CONTINUE
      END IF
      GOTO 1500
C
C STOCHASTIC ORBIT PARAMETERS
C ---------------------------
1100  CONTINUE
      SVNIP=LOCQ(3,IP)
      ISAT=LOCQ(7,IP)
      IF(SVNIP.GE.900 .AND. SVNIP.LT.951) GOTO 1150
      IF (IARC.NE.LOCQ(2,IP)) GOTO 1500
      IF (MODSVN(SVNIP).NE.SVN) GOTO 1500
C
C MANOEUVRE SATELLITE ?
      IF (IORB.EQ.0) THEN
        SVNMAN=SVN
        DO 1110 IMAN=1,NMAN
          IF (SATMAN(IMAN).EQ.SVN.AND.
     1        TIMMAN(IMAN).GT.TBOUND(1)-DTBND1.AND.
     2        TIMMAN(IMAN).LE.TOBS) THEN
            SVNMAN=SVNMAN+50
            GOTO 1120
          ENDIF
1110    CONTINUE
1120    CONTINUE
      ENDIF
C
      IF (SVNMAN.NE.SVNIP) GOTO 1500
C
      IF(IORB.EQ.0)THEN
        IF(IFORB.EQ.1)THEN
          IFORB=0
          CALL PRTDER(CHDUMM,SVNIP,1,0,1,TOBS,1,ICRARC,IORS,
     1                NVAR,NRAD,DRDELE,ELE,RPRPAR,ANLTYP,IRC)
          IF(NVAR.EQ.15.OR.NVAR.EQ.18.OR.NVAR.EQ.21)THEN
            NEWORB=1
          ELSE
            NEWORB=0
          END IF
        END IF
        IF(STCDEF(ISAT).EQ.0) THEN
          STCDEF(ISAT)=1
          IF(NEWORB.EQ.0)THEN
            CALL RPARTN(IARC,SVNIP,GM,TSEC,0.D0,ELE(1),ELE(2),ELE(3),
     1                  ELE(4),ELE(5),ELE(7),DRDMOD)
          ELSE
            DO 1121 IPAR=1,6
              CALL PRTDER(CHDUMM,SVNIP,IPAR,0,1,TOBS,1,ICRARC,IORS,
     1                    NVAR,NRAD,DRDMOD(1,IPAR),ELE,RPRPAR,ANLTYP,
     2                    IRC)
1121        CONTINUE
            IF(IRCRAD.EQ.0) THEN
              ISTART=6
              DO 1131 IPAR=1,NRAD
                CALL PRTDER(CHDUMM,SVN,ISTART+IPAR,0,1,TOBS,1,IARCRP,
     1                      IORSYS,NVAR,NRAD,DRDELE(1,6+IPAR),ELE,
     2                      RPRPAR,ANLTYP,IRCRPR)
                DO 1130 K=1,3
                  IF(IPAR.LE.3) DRDACC(K,IPAR+3)=DRDELE(K,6+IPAR)
     1                                           /SCASTC(2)
                  IF(IPAR.GT.9.AND.IPAR.LE.12)
     1              DRDACC(K,IPAR-9)=DRDELE(K,6+IPAR)
     2                                           /SCASTC(2)
                  IF(IPAR.GT.12) DRDACC(K,IPAR-6)=DRDELE(K,6+IPAR)
     1                                            /SCASTC(2)
                  DRDELE(K,6+IPAR)=DRDELE(K,6+IPAR)/1.D9
1130            CONTINUE
1131          CONTINUE
            END IF
          END IF
        END IF
      ENDIF
C
C PARTIALS W.R.T. TO STOCHASTIC PARAMETERS (DIFFERENT COMPUTATION
C FOR OLD/NEW ORBIT MODEL)
C ---------------------------------------------------------------
      IF(NEWORB.EQ.0)THEN
        CALL PDSTC(LOCQ(1,IP),IORSYS,TOBS,TSEC,ELE,TOSC,TIMSTC,
     1             TUV,SCASTC,DRDMOD,NDIFF,ONEOR2,AHELP(IP))
      ELSE
        CALL PDSTC2(LOCQ(1,IP),TOBS,TIMSTC,
     1              TUV,SCASTC,DRDMOD,NDIFF,ONEOR2,DRDACC,AHELP(IP))
      END IF
      GO TO 1500
C
C STOCHASTIC ORBIT PARAMETERS FOR LEO
C -----------------------------------
1150  LEOIP=LOCQ(3,IP)
      IF(IARC2.NE.LOCQ(2,IP)) GOTO 1500
      DO I=1,NDIFF+1
        CALL STAFLG(STNAME(IS12(I)),TOBS,IFLG,MARTYP)
        IF (MARTYP.EQ.MTypeSPACE) THEN
          CALL LEOPRN(STNAME(IS12(I)),TOBS,LEONUM)
          ONEOR2=I
          IF(MODSVN(LEOIP).EQ.LEONUM) GOTO 1160
        END IF
      END DO
C
C LEO AS "SATELLITE" AND NOT AS "STATION" (E.G. FOR SLR)
      LEONUM=SVN
      IF (MODSVN(LEOIP).NE.LEONUM) GOTO 1500
      ONEOR2=0
1160  CONTINUE
C
C MANOEUVRE LEO ???
      IF (IORB2.EQ.0) THEN
        LEOMAN=LEONUM
        DO IMAN=1,NMAN
          IF (SATMAN(IMAN).EQ.LEONUM.AND.
     1        TIMMAN(IMAN).GT.TBOUND(1)-DTBND1.AND.
     2        TIMMAN(IMAN).LE.TOBS) THEN
            LEOMAN=LEOMAN+50
            GOTO 1165
          ENDIF
        END DO
1165    CONTINUE
      ENDIF
C
      IF (LEOMAN.NE.LEOIP) GOTO 1500
C
C ??????????????????????????????????
C     IORB2=0
      IF(IORB2.EQ.0)THEN
C
        IF(IFORB2.EQ.1)THEN
          IFORB2=0
          CALL PRTDE2(LEORPR,LEOIP,1,0,1,TOBS,1,ICRARC,IORS,
     1                NVAR,NRAD,DRDELE,ELE,RPRPAR,ANLTYP,IRC)
          IF(NVAR.EQ.15.OR.NVAR.EQ.18.OR.NVAR.EQ.21)THEN
            NEWORL=1
          ELSE
            NEWORL=0
          END IF
        END IF
C
        IF(STCDEF2(ISAT).EQ.0) THEN
          STCDEF2(ISAT)=1
          IF(NEWORL.EQ.0)THEN
            CALL RPARTN(IARC2,LEOIP,GM,TSEC,0.D0,ELE(1),ELE(2),ELE(3),
     1                  ELE(4),ELE(5),ELE(7),DRDMOD)
          ELSE
            DO IPAR=1,6
              CALL PRTDE2(LEORPR,LEOIP,IPAR,0,1,TOBS,1,ICRARC,IORS,
     1                    NVAR,NRAD,DRDMOD(1,IPAR),ELE,RPRPAR,ANLTYP,
     2                    IRC)
            END DO
            IF(LEORAD.EQ.0) THEN
              ISTART=6
              DO IPAR=1,NRAD
                CALL PRTDE2(LEORPR,LEOIP,ISTART+IPAR,0,1,TOBS,1,
     1                      IARCRP,IORSYS,NVAR,NRAD,DRDELE(1,6+IPAR),
     2                      ELE,RPRPAR,ANLTYP,IRCRPR)
                DO K=1,3
                  IF(IPAR.LE.3) DRDACC(K,IPAR+3)=DRDELE(K,6+IPAR)
     1                                           /SCASTC(2)
                  IF(IPAR.GT.9.AND.IPAR.LE.12)
     1              DRDACC(K,IPAR-9)=DRDELE(K,6+IPAR)
     2                                           /SCASTC(2)
                  IF(IPAR.GT.12) DRDACC(K,IPAR-6)=DRDELE(K,6+IPAR)
     1                                            /SCASTC(2)
                  DRDELE(K,6+IPAR)=DRDELE(K,6+IPAR)/1.D9
                END DO
              END DO
            END IF
          END IF
        END IF
      ENDIF
C
C PARTIALS W.R.T. TO STOCHASTIC PARAMETERS (DIFFERENT COMPUTATION
C FOR OLD/NEW ORBIT MODEL)   ******** -  LEO ONLY - ********
C ---------------------------------------------------------------
      IF(NEWORL.EQ.0)THEN
        CALL PDSTC(LOCQ(1,IP),IORSYS,TOBS,TSEC,ELE,TOSC,TIMSTC2,
     1             TUV,SCASTC,DRDMOD,NDIFF,ONEOR2,AHELP(IP))
      ELSE
        CALL PDSTC2(LOCQ(1,IP),TOBS,TIMSTC2,
     1              TUV,SCASTC,DRDMOD,NDIFF,ONEOR2,DRDACC,AHELP(IP))
      END IF
      GO TO 1500
C
C SATELLITE ANTENNA OFFSET PARAMETERS
C -----------------------------------
1200  CONTINUE
      IOFR=LOCQ(2,IP)
      ICOR=LOCQ(3,IP)
      IGRP=LOCQ(4,IP)
      IF (TOBS.LT.TIMOFF(1,IOFR) .OR. TOBS.GE.TIMOFF(2,IOFR)) GOTO 1500
      IF (IGRP.EQ.0) THEN
        IF (SVN.EQ.LOCQ(5,IP)) GOTO 1220
      ELSE
        DO 1210 ISAOFF=1,NSAOFF(IGRP)
          IF (SVN.EQ.SATOFF(ISAOFF,IGRP)) GOTO 1220
1210    CONTINUE
      ENDIF
      GOTO 1500
1220  CALL PDANT(IORSYS,ICOR,TOBS,TUV,XVSAT,NDIFF,AHELP(IP))
      GOTO 1500
C
C EARTH POTENTIAL(TYP=13) / HILL PARAMETERS (TYP=14) / ALBEDO (TYP=15)
1300  CONTINUE
      IF(LOCQ(1,IP).EQ.13)THEN
        XNORM=SCAPOT
      ELSE IF(LOCQ(1,IP).EQ.14)THEN
        XNORM=SCAHIL
      ELSE IF(LOCQ(1,IP).EQ.15)THEN
        XNORM=SCAALB
      END IF
      IF(LOCQ(1,IP).NE.14.OR.(SVN.EQ.LOCQ(3,IP)))THEN
C
C FOR ALBEDO PARAMETERS CHECK WHETHER THE SATELLITE GROUP IS OK
        IF(LOCQ(1,IP).EQ.15)THEN
          IGRP=LOCQ(5,IP)
          DO 1310 ISAALB=1,NSAALB(IGRP)
            IF (SVN.EQ.SATALB(ISAALB,IGRP)) GOTO 1320
1310      CONTINUE
          GO TO 1500
1320      CONTINUE
        END IF
C
C various options to calculate partials
        IF     (hilkep == 1) THEN
          CALL HILDER(LOCQ(1,IP),SVN,IARC,TOSC,ELE,TOBS,XNORM,
     &                IORSYS,DRDPAR)
        ELSEIF (hilkep == 2) THEN
          CALL KEPDER(LOCQ(1,IP),SVN,IARC,TOSC,ELE,TOBS,XNORM,
     &                IORSYS,DRDPAR)
        ELSE
          CALL getDxvDp2(ip,LOCQ(:,1:nPar),SVN,TBOUND,TOBS,XNORM,DRDPAR)
        ENDIF
        CALL COOTRA(IORSYS,0,TOBS,DRDPAR,DUM,DUM,DUM,DUM)
C
        AHELP(IP)=0.D0
        DO 341 K=1,3
          IF (NDIFF.EQ.1) THEN
            AHELP(IP)=AHELP(IP)+(XTOPO(K,1)/D(1)-XTOPO(K,2)/D(2))
     1                          *DRDPAR(K)
          ELSE
            AHELP(IP)=AHELP(IP)+(XTOPO(K,1)/D(1))*DRDPAR(K)
          ENDIF
341     CONTINUE
      END IF
      GO TO 1500
C
C CENTER OF MASS PARAMETERS :
C -------------------------
1400  CONTINUE
      ICOR=LOCQ(2,IP)
      DO 1410 K=1,3
        DRDPAR(K)=0.D0
1410  CONTINUE
      IF(ICOR.EQ.1)THEN
        DRDPAR(1)=DRDPAR(1)+DCOS(SZ)/SCACEN
        DRDPAR(2)=DRDPAR(2)+DSIN(SZ)/SCACEN
      ELSE IF(ICOR.EQ.2)THEN
        DRDPAR(1)=DRDPAR(1)-DSIN(SZ)/SCACEN
        DRDPAR(2)=DRDPAR(2)+DCOS(SZ)/SCACEN
      ELSE IF(ICOR.EQ.3)THEN
        DRDPAR(3)=DRDPAR(3)+1.D0/SCACEN
      END IF
      AHELP(IP)=0.D0
C
      DO 1420 I=1,NDIFF+1
        CALL STAFLG(STNAME(IS12(I)),TOBS,IFLG,MARTYP)
        IF ( .NOT. (MARTYP.EQ.' ')) GOTO 1420
        DO K=1,3
          IF (I.EQ.1) THEN
            AHELP(IP)=AHELP(IP)+(XTOPO(K,I)/D(I))*DRDPAR(K)
          ELSE
            AHELP(IP)=AHELP(IP)-(XTOPO(K,I)/D(I))*DRDPAR(K)
          END IF
        END DO
1420  CONTINUE
      GO TO 1500
C
C DIFFERENTIAL IONOSPHERE PARAMETERS
C ----------------------------------
1700  CONTINUE
      IF(IFILE.NE.LOCQ(2,IP).OR.
     1   SVN.NE.LOCQ(3,IP).OR.
     2   IEPO.NE.LOCQ(4,IP).OR.
     3   ICARR.EQ.3) GOTO 1500
C
      ZDM=(ZENITH(1)+ZENITH(NDIFF+1))/2.D0
      LOCQ(6,IP)=IDNINT(ARS*ZDM)
      IF (OPTDIP(3).EQ.1) THEN
C
C ZENITH PARAMETERS
        AHELP(IP)=FACSIP(ICARR)/DCOS(ZDM)
      ELSE
C
C LINE-OF-SIGHT PARAMETERS
        AHELP(IP)=FACSIP(ICARR)
      ENDIF
      IF(MEATYP.EQ.2)AHELP(IP)=-AHELP(IP)
      GOTO 1500
C
C RECEIVER ANTENNA PHASE CENTER VARIATIONS
C ----------------------------------------
1800  CONTINUE
      ICAL =LOCQ(2,IP)
      IFRQ =LOCQ(3,IP)
      IZEN =LOCQ(4,IP)
      IAZI =LOCQ(5,IP)
      NZEN =LOCQ(6,IP)
      NAZI =LOCQ(7,IP)
C
C CHECK FREQUENCY
      IF (IFRQ.EQ.1.AND.ICARR.NE.1) GOTO 1500
      IF (IFRQ.EQ.2.AND.ICARR.NE.2) GOTO 1500
      IF (IFRQ.EQ.3.AND.ICARR.NE.1.AND.ICARR.NE.2.AND.
     1                  ICARR.NE.3.AND.ICARR.NE.5) GOTO 1500
      IF (IFRQ.EQ.4.AND.ICARR.NE.4) GOTO 1500
C
C CHECK SATELLITE SYSTEM
      IF (INT(SVN/100).NE.PRNCAL(ICAL) .AND.
     1    PRNCAL(ICAL).NE.10) GOTO 1500
C
      IF (IFRQ.LT.3) THEN
        FACT=FACLIN(ICARR,IFRQ,SVN)
      ELSE
        FACT=1D0
      ENDIF
C
      DO 1810 I=1,NDIFF+1
        IF ((RECTYP(I).EQ.ANTCAL(1,ICAL) .OR. ANTCAL(1,ICAL).EQ.' ')
     1                                  .AND.
     2      ANTTYP(I).EQ.ANTCAL(2,ICAL) .AND.
     3      IANTEN(I).GE.NUMCAL(1,ICAL) .AND.
     4      IANTEN(I).LE.NUMCAL(2,ICAL)) THEN
C
C GET ANTENNA ORIENTATION TO CORRECT THE AZIMUTH
          CALL GETAZI(' ',ANTTYP(I),IANTEN(I),CSESS(1),ANTAZI)

          CALL STAFLG(STNAME(IS12(I)),TOBS,IFLG,MARTYP)
          IF (MARTYP.EQ.MTypeSPACE) THEN
            AZIOK=AZIMU2(I)-ANTAZI
            ZENOK=ZENIT2(I)
          ELSE
            AZIOK=AZIMUT(I)-ANTAZI
            ZENOK=ZENITH(I)
          END IF
C
C POLYGON MODEL
          IF (NZEN.GT.0) THEN
            DIFFZ = POLDIF(0,RAPZENMAX,NZEN,IZEN,ZENOK)
            DIFFA = POLDIF(1,2.D0*PI,NAZI,IAZI,AZIOK)
C
            IF (DIFFZ.LT.1.D0 .AND. DIFFA.LT.1.D0) THEN
              AHELP(IP)= AHELP(IP) + (1.D0-DIFFZ) * (1.D0-DIFFA) *
     1                   FACT * (-1.D0)**(I+1)
            ENDIF
C
C SPHERICAL HARMONICS
          ELSE
            AHELP(IP)=AHELP(IP)+(-1.D0)**(I+1)*FACT*
     1        ASLEF2(ZENOK,AZIOK,-IZEN,IAZI,IABS(NZEN),NAZI)
          END IF
        END IF
1810  CONTINUE
      GO TO 1500
C
C GLOBAL IONOSPHERE MODEL PARAMETERS
C ----------------------------------
1900  CONTINUE
      CALL PDGIM (NPARN ,LOCQ  ,PARTYP,OPTGIM,POLGIM,EPOGIM,
     1            SCAGIM,TOBS  ,DTSIM ,WGSEPO,XVSAT ,SZ    ,
     2            ZENITH,ICARR ,IORSYS,MEATYP,NDIFF ,IS12  ,
     3            SVN   ,STNAME,IP    ,AHELP )
      GO TO 1500
C
C STATION VELOCITIES (ESTIMATED IN ADDNEQ ONLY)
C ---------------------------------------------
2000  CONTINUE
C
C KINEMATIC COORDINATES
C ---------------------
2100  CONTINUE
      IS=LOCQ(2,IP)
      IF(IS.NE.ICENTR(IS12(1)).AND.IS12(2).EQ.0)          GOTO 1500
      IF (IS12(2).NE.0) THEN
        IF(IS.NE.ICENTR(IS12(1)).AND.IS.NE.ICENTR(IS12(2))) GOTO 1500
      ENDIF
      IF(ICARR.EQ.4) GOTO 1500
      DTIME=TOBS-CLKHED%TFIRST
      INTEPO=IDNINT((DTIME*864D2)/CLKREC%EPOCH(1)*CLKREC%NEPO)+1
      IF (LOCQ(4,IP).EQ.-1) LOCQ(4,IP)=INTEPO
      IF (LOCQ(4,IP).NE.INTEPO) GOTO 1500
C
      IF(IS.NE.ICENTR(IS12(1))) GOTO 2120
C
      CALL PDCOR(XTOPO(1,1),D(1),SZ,XPOL,YPOL,AUX)
      DO 2110 I=1,3
        AHELP(IP)=-AUX(I)
        IF (LOCQ(4,IP).EQ.-1) LOCQ(4,IP)=INTEPO
        LOCQ(6,IP)=LOCQ(6,IP)+1
        IP=IP+1
2110  CONTINUE
      IP=IP-1
      GOTO 1500
C
2120  CONTINUE
      CALL PDCOR(XTOPO(1,2),D(2),SZ,XPOL,YPOL,AUX)
      DO 2130 I=1,3
        AHELP(IP)=AUX(I)
        IF (LOCQ(4,IP).EQ.-1) LOCQ(4,IP)=INTEPO
        IF (IPHSEP.EQ.0.OR.MEATYP.EQ.1) LOCQ(6,IP)=LOCQ(6,IP)+1
        IP=IP+1
2130  CONTINUE
      IP=IP-1
      GOTO 1500
C
C SCALE FACTORS FOR VIENNA GRID FILES
C -----------------------------------
2200  CONTINUE
      IF(ICARR.EQ.4) GOTO 1500
C
      CALL STAFLG(STNAME(IS12(1)),TOBS,IFLG,MARTYP)
C
C     THE PARAMETER FOR THE FIRST STATION
      IF (MARTYP.NE.MTypeSPACE .AND.
     1   (OPLOAD(LOCQ(2,IP))%STACLU(1).EQ.-1 .OR.
     2    OPLOAD(LOCQ(2,IP))%STALST(LOCQ(3,IP)).EQ.STNAME(IS12(1))))THEN
C
        HLPGRD(1) = -DCOS(ZENITH(1))
        HLPGRD(2) = -DCOS(AZIMUT(1))*DSIN(ZENITH(1))
        HLPGRD(3) = -DSIN(AZIMUT(1))*DSIN(ZENITH(1))
C
C       ONE PARAMETER PER COMPONENT
        IF (LOCQ(5,IP).EQ.3) THEN
          AHELP(IP)=AHELP(IP)
     1             +getGrid(getGridKeyw(grdNeq(LOCQ(2,IP)),LOCQ(4,IP)),
     2                      TOBS, ELLS12(1:3,1))*HLPGRD(LOCQ(4,IP))
C       ONE PARAMETER PER FOR THE HORIZONTAL COMPONENTS
        ELSE IF (LOCQ(5,IP).EQ.2.AND.LOCQ(4,IP).EQ.2) THEN
          AHELP(IP)=AHELP(IP)
     1             +getGrid(getGridKeyw(grdNeq(LOCQ(2,IP)),2),
     2                      TOBS, ELLS12(1:3,1))*HLPGRD(2)
     3             +getGrid(getGridKeyw(grdNeq(LOCQ(2,IP)),3),
     4                      TOBS, ELLS12(1:3,1))*HLPGRD(3)
C       ONE PARAMETER PER FOR THE VERTICAL COMPONENT
        ELSE IF (LOCQ(5,IP).EQ.2.AND.LOCQ(4,IP).EQ.1) THEN
          AHELP(IP)=AHELP(IP)
     1             +getGrid(getGridKeyw(grdNeq(LOCQ(2,IP)),1),
     2                      TOBS, ELLS12(1:3,1))*HLPGRD(1)
C       ONE PARAMETER PER FOR ALL COMPONENTS
        ELSE IF (LOCQ(5,IP).EQ.1) THEN
          AHELP(IP)=AHELP(IP)
     1             +getGrid(getGridKeyw(grdNeq(LOCQ(2,IP)),1),
     2                      TOBS, ELLS12(1:3,1))*HLPGRD(1)
     3             +getGrid(getGridKeyw(grdNeq(LOCQ(2,IP)),2),
     4                      TOBS, ELLS12(1:3,1))*HLPGRD(2)
     5             +getGrid(getGridKeyw(grdNeq(LOCQ(2,IP)),3),
     6                      TOBS, ELLS12(1:3,1))*HLPGRD(3)
        ENDIF
      ENDIF
C
C     NO SECOND STATION AVAILABLE
      IF (IS12(2).EQ.0) GOTO 1500
C
      CALL STAFLG(STNAME(IS12(2)),TOBS,IFLG,MARTYP)
      IF (MARTYP.EQ.MTypeSPACE) GOTO 1500
C
C     ONE PARAMETER FOR ALL STATIONS  OR  THE PARAMETER FOR THE STATION
      IF (OPLOAD(LOCQ(2,IP))%STACLU(1).EQ.-1 .OR.
     1    OPLOAD(LOCQ(2,IP))%STALST(LOCQ(3,IP)).EQ.STNAME(IS12(2))) THEN
C
        HLPGRD(1) = -DCOS(ZENITH(2))
        HLPGRD(2) = -DCOS(AZIMUT(2))*DSIN(ZENITH(2))
        HLPGRD(3) = -DSIN(AZIMUT(2))*DSIN(ZENITH(2))

C       ONE PARAMETER PER COMPONENT
        IF (LOCQ(5,IP).EQ.3) THEN
          AHELP(IP)=AHELP(IP)
     1             -getGrid(getGridKeyw(grdNeq(LOCQ(2,IP)),LOCQ(4,IP)),
     2                      TOBS, ELLS12(1:3,2))*HLPGRD(LOCQ(4,IP))
C       ONE PARAMETER PER FOR THE HORIZONTAL COMPONENTS
        ELSE IF (LOCQ(5,IP).EQ.2.AND.LOCQ(4,IP).EQ.2) THEN
          AHELP(IP)=AHELP(IP)
     1             -getGrid(getGridKeyw(grdNeq(LOCQ(2,IP)),2),
     2                      TOBS, ELLS12(1:3,2))*HLPGRD(2)
     3             -getGrid(getGridKeyw(grdNeq(LOCQ(2,IP)),3),
     4                      TOBS, ELLS12(1:3,2))*HLPGRD(3)
C       ONE PARAMETER PER FOR THE VERTICAL COMPONENT
        ELSE IF (LOCQ(5,IP).EQ.2.AND.LOCQ(4,IP).EQ.1) THEN
          AHELP(IP)=AHELP(IP)
     1             -getGrid(getGridKeyw(grdNeq(LOCQ(2,IP)),1),
     2                      TOBS, ELLS12(1:3,2))*HLPGRD(1)
C       ONE PARAMETER PER FOR ALL COMPONENTS
        ELSE IF (LOCQ(5,IP).EQ.1) THEN
          AHELP(IP)=AHELP(IP)
     1             -getGrid(getGridKeyw(grdNeq(LOCQ(2,IP)),1),
     2                      TOBS, ELLS12(1:3,2))*HLPGRD(1)
     3             -getGrid(getGridKeyw(grdNeq(LOCQ(2,IP)),2),
     4                      TOBS, ELLS12(1:3,2))*HLPGRD(2)
     5             -getGrid(getGridKeyw(grdNeq(LOCQ(2,IP)),3),
     6                      TOBS, ELLS12(1:3,2))*HLPGRD(3)
        ENDIF
      ENDIF
C
      GOTO 1500
C
C EPOCH WISE CLOCKS: STATIONS
C ---------------------------
2300  CONTINUE
      IS=LOCQ(2,IP)
      IF(IS.EQ.0) GOTO 1500
      IF(IS.NE.ICENTR(IS12(1)).AND.IS12(2).EQ.0)            GOTO 1500
      IF (IS12(2).NE.0) THEN
        IF(IS.NE.ICENTR(IS12(1)).AND.IS.NE.ICENTR(IS12(2))) GOTO 1500
      ENDIF
      IF(ICARR.EQ.4) GOTO 1500
      IF(LOCQ(3,IP).NE.0.AND.LOCQ(3,IP)-1.NE.INT(SVN/100))  GOTO 1500
      DTIME=TOBS-CLKHED%TFIRST
      INTEPO=IDNINT((DTIME*864D2)/CLKREC%EPOCH(1)*CLKREC%NEPO)+1
      IF (LOCQ(4,IP).EQ.-1) LOCQ(4,IP)=INTEPO
      IF (LOCQ(4,IP).NE.INTEPO) GOTO 1500
C
CCCC  CALL PDCLK(1,XTOPO(1,1),D(1),XVSAT,XSTAT(1,IS),0D0,SZ,DERCLK)
      CALL PDCLK(1,XTOPO(1,1),D(1),XVSAT,WGSEPO(1,1),0D0,SZ,DERCLK)
      AHELP(IP)=(-DERCLK(1)-DERCLK(2)/C-C)/C
      IF (IPHSEP.EQ.0.OR.MEATYP.EQ.1) LOCQ(6,IP)=LOCQ(6,IP)+1
      GOTO 1500
C
C EPOCH WISE CLOCKS: SATELLITES
C -----------------------------
2400  CONTINUE
      IS=LOCQ(3,IP)
      IF(IS.EQ.0) GOTO 1500
      IF(MODSVN(IS).NE.SVN) GOTO 1500
      IF(ICARR.EQ.4) GOTO 1500
      DTIME=TOBS-CLKHED%TFIRST
      INTEPO=IDNINT((DTIME*864D2)/CLKREC%EPOCH(1)*CLKREC%NEPO)+1
      IF (LOCQ(4,IP).EQ.-1) LOCQ(4,IP)=INTEPO
      IF (LOCQ(4,IP).NE.INTEPO) GOTO 1500
C
C AHELP(IP)=C/C
C
      AHELP(IP)=1.D0
      IF (IPHSEP.EQ.0.OR.MEATYP.EQ.1) LOCQ(6,IP)=LOCQ(6,IP)+1
      GOTO 1500
C
C SATELLITE ANTENNA PHASE CENTER VARIATIONS
C -----------------------------------------
2500  CONTINUE
      ISPV =LOCQ(2,IP)
      IGRP =LOCQ(3,IP)
      IZEN =LOCQ(4,IP)
      IAZI =LOCQ(5,IP)
      NZEN =LOCQ(6,IP)
      NAZI =LOCQ(7,IP)
C
      DO ISASPV=1,NSASPV(ISPV)
        IF (SVN.EQ.SATSPV(ISASPV,ISPV)) GOTO 2505
      ENDDO
      GOTO 1500
C
2505  CONTINUE
      DO 2510 I=1,2
C
C POLYGON MODEL
        IF (NZEN.GT.0) THEN
          DIFFZ = POLDIF(0,NADMAX,NZEN,IZEN,NADIR(I))
          DIFFA = POLDIF(1,2.D0*PI,NAZI,IAZI,AZISOK(I))
C
          IF (DIFFZ.LT.1.D0 .AND. DIFFA.LT.1.D0) THEN
            AHELP(IP)= AHELP(IP) + (1.D0-DIFFZ) *
     1                             (1.D0-DIFFA) * (-1.D0)**(I+1)
          ENDIF
        ENDIF
2510  CONTINUE
      GO TO 1500
C
C SLR RANGE BIASES
C ----------------
2600  CONTINUE
      ISTA = LOCQ(2,IP)
      IS = LOCQ(5,IP)

      IF ( NRGB.GT.0 .AND. ISTA == ICENTR(IS12(1)) ) THEN

C Satellite-specific
        IF ( IS<1000 .AND. IS>0 .AND. IS == SVN ) GOTO 2601
C System-specific
        IF ( IS<0 .AND. (ABS(IS)-1) == INT(SVN/100) )
     1    GOTO 2601
C All satellites together
        IF ( IS==1000 ) GOTO 2601

        GOTO 1500

2601    AHELP(IP) = AHELP(IP) + 1.D0

      END IF
      GOTO 1500
C
C
C HIGHER-ORDER IONOSPHERE SCALING FACTORS
C ---------------------------------------
2700  CONTINUE
      DR(:)=0D0
      DO I=1,NDIFF+1
        IF( LOCQ(3,IP).EQ.1 .OR.
     1     (LOCQ(3,IP).EQ.2.AND.IS12(I).EQ.LOCQ(4,IP)))THEN
          CALL IONOSP(WGSEPO(:,I),XVSAT,TOBS,SZ,ZENITH(I),ICARR,
     1                IORSYS,STNAME(IS12(I)),SVN,ZENMAX,DR(I),
     1                MEATYP,AZIMUT(I),HOI(:,I),ELLS12(:,I))
        ENDIF
      ENDDO
C     ONE_FOR_ALL
      IF(LOCQ(3,IP).EQ.1)THEN
        AHELP(IP)=HOI(LOCQ(2,IP),1)-HOI(LOCQ(2,IP),2)*DBLE(NDIFF)
C     ONE_PER_STATION
      ELSEIF(LOCQ(3,IP).EQ.2)THEN
        DO I=1,NDIFF+1
          IF(IS12(I).EQ.LOCQ(4,IP))THEN
            AHELP(IP)=HOI(LOCQ(2,IP),I)*(-1D0)**(I+1)
          ENDIF
        ENDDO
      ENDIF
      IF(MEATYP.EQ.2) AHELP(IP)=-AHELP(IP)
      GO TO 1500
C
C GNSS-SPECIFIC PARAMETERS
C ------------------------
3000  ISTA=LOCQ(2,IP)
      ICOR=LOCQ(3,IP)
      ISYS=LOCQ(4,IP)
C
C WHICH STATION
      INDSTA=0
      IF(ISTA.EQ.IS12(1)) THEN
        INDSTA=1
      ELSEIF(ISTA.EQ.IS12(2)) THEN
        INDSTA=2
      ENDIF
      IF (INDSTA.EQ.0 .OR. INT(SVN/100).NE.ISYS) GOTO 5000
C
C STATION TRANSLATIONS
      IF (ICOR.EQ.1) THEN
        CALL PDCOR(XTOPO(1,INDSTA),D(INDSTA),SZ,XPOL,YPOL,AUX)
        DO I=1,3
          AHELP(IP)=AUX(I)*(-1.D0)**(INDSTA+0)
          IP=IP+1
        ENDDO
        IP=IP-1
        GOTO 1500
C
C TROPOSPHERE BIAS
      ELSEIF (ICOR.EQ.4) THEN
        CALL TRPMAP(ITRMAP,TOBS,ELLS12(1,INDSTA),ZENITH(INDSTA),MAPFUN)
        AHELP(IP)=MAPFUN(1)*(-1.D0)**(INDSTA-1)
        GOTO 1500
      ELSE
        GOTO 5000
      ENDIF
C
C END OF PARAMETER TYPES
C ----------------------
1500  IF(IP.EQ.NPAR) GOTO 5200
5000  CONTINUE
5200  CONTINUE
C
C REMOVE ZEROES FROM OBSERVATION EQUATION
C ---------------------------------------
      IA=0
      DO 2020 IP=1,NPAR
        IF(AHELP(IP).NE.0.D0) THEN
          IA=IA+1
C
          NSNG=IA+1
          CALL DIMTST(1,2,2,'SNDIFF','MAXSNG',
     1      'NON-ZERO ELEMENTS IN A-MATRIX',' ',NSNG,MAXSNG,IRC)
C
          IND=ISNG+(IA-1)*MAXEQN
          INDA(IND)=IP
          A(IND)=AHELP(IP)
C
C COUNT THE OBSERVATION
          ISYS=SVN/100
          II=MAXMEA*ISYS+MEATYP
          IF (MEATYP.EQ.3) II = 3
          NOBSPA(II,IP)=NOBSPA(II,IP)+1
        END IF
2020  CONTINUE
      IND=ISNG+IA*MAXEQN
      INDA(IND)=0
      A(IND)=0.D0
C
      RETURN
      END SUBROUTINE

      END MODULE
