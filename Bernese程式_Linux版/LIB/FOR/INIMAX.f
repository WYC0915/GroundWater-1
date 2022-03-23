      MODULE s_INIMAX
      CONTAINS

C*
      SUBROUTINE INIMAX(NFTOT  ,STFIL  ,NSTAT  ,ICENTR ,NFIX   ,STFIX  ,
     1                  NKIN   ,STKIN  ,NCLREQ ,ISTCLK ,NCLK   ,IBIAS  ,
     2                  CLFRTO ,NREC   ,ISBTIM ,TIMREF ,NARC   ,NORB   ,
     3                  NSATEL ,SATNUM ,NUMSAT ,NAVNUM ,ARCINT ,NFRFIL ,
     4                  ICARR  ,MEATYP ,MELWUB ,NIOREQ ,IONREQ ,OPTDCB ,
     5                  POLMOD ,POLPAR ,NPOL   ,NTRREQ ,NPARTR ,NTRSTA ,
     6                  STATRP ,ITRGRD ,NUMAMB ,AMBCLS ,AMBSAT ,AMBIEP ,
     7                  AMBDEF ,NSASTC ,NUMSTC ,NSTCEF ,NSTCEP ,NRQOFF ,
     8                  PAROFF ,NHILL  ,NPOT   ,NALBGR ,NALB   ,NCENM  ,
     9                  NEPOCH ,WINDOW ,NSAMPL ,IDELTT ,DTSIM  ,NANCAL ,
     1                  NFRCAL ,NPTCAL ,STRAMB ,OPTDIP ,OPTHOI ,OPTELI ,
     1                  OPTGIM ,POLGIM ,NAMGIM ,EPOGIM ,NANRAO ,NFRRAO ,
     2                  NEURAO ,NCLKST ,NCLKSA ,CLKSYS ,NRGB   ,CLKHED ,
     3                  CLKREC ,NDIFF  ,STNAME ,IOREST ,NESTSAT,ESTSAT ,
     4                  NARC2  ,ARCINT2,NUMSAT2,NAVNUM2,NSASTC2,NSTCEF2,
     5                  NUMSTC2,NSTCEP2,NORB2  ,NANSPV ,NPTSPV ,NSASPV ,
     6                  CORSTR ,MAXTYP ,MIXED  ,OPLOAD ,MAXSAT ,MAXAMB ,
     7                  MAXFLS ,MAXSAS ,MAXFRS ,NMXSNG ,NEPSNG ,NMXLOC ,
     8                  NMXPAR ,NMXAMP ,RECTYP ,IRUNIT ,globalWindow   ,
     9                  numIsb ,OPTGSP)
CC
CC NAME       :  INIMAX
CC
CC PURPOSE    :  DEFINE THE SEQUENCE OF PARAMETERS TO BE ESTIMATED
CC
CC PARAMETERS :
CC         IN :  NFTOT  : TOTAL NUMBER OF FILES               I*4
CC               STFIL(L,I),L=1,2 , I=1,2,...,NFTOT: STATION  I*4
CC                        NUMBERS INVOLVED IN OBSFILE I
CC               NSTAT  : NUMBER OF STATIONS INVOLVED         I*4
CC               ICENTR(I),I=1,..,NSTAT: CENTER STATION NUMB. I*4
CC               NFIX   : NUMBER OF FIXED STATIONS            I*4
CC               STFIX(I),I=1,2,... STATION NUMBERS TO BE     I*4
CC                        KEPT FIXED
CC               NKIN   : # STATIONS ESTIMATED IN KIN. MODUS  I*4
CC               STKIN  : NUMBERS OF THE KIN. STATIONS        I*4(*)
CC               NCLREQ : NUMBER OF REQUESTS FOR ESTIMATING   I*4
CC                        CLOCK PARAMETERS
CC               ISTCLK(I),I=1,2,..,NCLREQ: STATION NUMBER    I*4
CC                        FOR REQUEST I
CC               NCLK(I),I=1,2,..,NCLREQ: NUMBER OF CLOCK     I*4
CC                        PARAMETER FOR REQUEST I
CC               IBIAS(I),I=1,2,..,NCLREQ: TYPE OF CLOCK BIAS I*4
CC                        0: STATION SPECIFIC
CC                        1: FREQUENCY SPECIFIC
CC                        2: SATELLITE SPECIFIC
CC                        3: SAT. SPECIFIC FOR NON-GPS
CC                        4: FREQUENCY SPECIFIC WITH POLYNOM
CC               CLFRTO(K,I),K=1,2: TIME LIMITS FOR REQUEST I R*8
CC               nRec   : Number of isb-requests              I*4
CC               isbTim(:): Time-dep. inter-syst. biases      t_isbTime(:)
CC               TIMREF(I),I=1,2,..,NFTOT: FIRST OBSERVATION  R*8
CC                        EPOCH OF OBS.FILE NUMBER I
CC               NARC   : TOTAL NUMBER OF SATELLITE ARCS      I*4
CC               NORB   : NUMBER OF ORBIT PARAMETERS PER      I*4
CC                        SATELLITE ARC
CC               NSATEL(I),I=1,2,...,NFTOT : NUMBER OF SATEL- I*4
CC                        LITES OBSERVED IN FILE I
CC               SATNUM(K,I),K=1,2,..,NSATEL(I),I=1,..,NFTOT: I*4
CC                        SVN OBSERVED IN FILE I
CC               NUMSAT(I),I=1,2,...,NARC: NUMBER OF SATELLI- I*4
CC                        TES IN ARC I
CC               NAVNUM(L),L=1,2,... : NAV-NUMBERS FOR ALL    I*4
CC                        ARCS
CC               ARCINT(I),I=1,2,...,NFTOT : ARC NUMBER       I*4
CC                        BELONGING TO FILE I
CC               NFRFIL(I),I=1,2,..,NFTOT: NUMBER OF FREQ.    I*4
CC                        PER FILE
CC               ICARR(K,I),K=1,..,NFRFIL(I),I=1,2,..,NFTOT:  I*4
CC                        CARRIER (L1=1,L2=2,...) FOR EACH FILE
CC               MEATYP(I),I=1,2,..,NFTOT: MEASUREMENT TYPE   I*4
CC                        =1 : PHASE
CC                        =2 : CODE
CC               MELWUB : MELBOURNE-WUEBBENA LC               I*4
CC                        =0: NO
CC                        =1: YES
CC                        =2: DTEC LC
CC               NIOREQ : NUMBER OF IONOSPHERE REQUESTS       I*4
CC               IONREQ(K,I),K=1,3: INDICATOR FOR IONOSPHERIC I*4
CC                        PARAMETERS:
CC                          I  : REQUEST NUMBER
CC                          K=1: DEGREE OF DEVELOPMENT IN LATITUDE
CC                          K=2: DEGREE OF DEVELOPMENT IN HOUR ANGLE
CC                          K=3: MAX. DEGREE OF MIXED COEFFICIENTS
CC                          IONREQ(1,I)<=0: END
CC               OPTDCB : OPTIONS FOR ESTIMATION OF           I*4(*)
CC                        DIFFERENTIAL CODE BIASES
CC                        (1): ESTIMATE DCBS FOR SATELLITES
CC                             = 0: NO
CC                             = 1: P1-P2
CC                             = 2: P1-C1
CC                             = 3: LC
CC                        (2): ESTIMATE DCBS FOR RECEIVERS
CC                             = 0: NO
CC                             = 1: P1-P2
CC                             = 2: P1-C1
CC                             = 3: LC
CC                             =-2: P1-C1 MULTIPLIER
CC                        (3): REFERENCE SATELLITE NUMBER
CC                             = 0: CONSTRAIN ALL SAT
CC                             =-1: CONSTRAIN SUM OF ALL SAT
CC                        (4): PROCESS NIGHT-TIME DATA ONLY
CC               POLMOD : MODEL OF POLAR WOBBLE               I*4
CC               POLPAR : NUMBER OF PARAMS. TO BE ESTIMATED   I*4(5)
CC                        (= POLYNOM DEGREE +1)
CC                        (1): XP, (2): YP, (3): DT
CC                        (4):EPS, (5):PSI
CC               NPOL   : NUMBER OF POLE PARAMETER SETS       I*4
CC               NTRREQ : NUMBER OF LOCAL TROPOSPHERE         I*4
CC                        REQUESTS
CC               NPARTR(I),I=1,2,..,NTRREQ : NUMBER OF PARAM. I*4
CC                        FOR MODEL I
CC               NTRSTA : NUMBER OF STATION SPECIFIC TROPOS-  I*4
CC                        PHERE REQUESTS
CC               STATRP(I),I=1,..,NTRSTA: ASSOC. STAT.NRS.    I*4
CC               ITRGRD : (1): EST. OF TROPOSPHERIC GRADIENTS I*4(*)
CC                             =0: NO ESTIMATION
CC                             =1: TILTING
CC                             =2: LINEAR
CC                        (2): RATIO OF NUMBER OF ZENITH TO
CC                             GRADIENT PARAMETERS
CC               NUMAMB(M),M=1,.,NFTOT: NUMBER OF AMBIGUITIES I*4
CC                        IN FILE M
CC               AMBCLS : AMBIGUITY CLUSTERS                I*4(*,*,3)
CC                          AMBIGU(I,K,M): AMBIGUITY NUMBER I
CC                                         FREQUENCY NUMBER K
CC                                         FILE NUMBER M
CC                            K=1: L1 AMBIGUITIES
CC                            K=2: L2 AMBIGUITIES
CC                            K=3: L5 AMBIGUITIES (WIDELANE)
CC               AMBSAT(IAMB,IFIL): SATELLITE FOR AMBIGUITY   I*4(*,*)
CC               AMBIEP(IAMB,IFIL): EPOCH FOR AMBIGUITY       I*4(*,*)
CC               AMBDEF(I),I=1,..,NFTOT: DEFINITION OF AMBI-  I*4
CC                        GUITY HANDLING FOR EACH FILE
CC                          1: IGNORE INFORMATION IN FILE,
CC                             ESTIMATE NEW AMBIGUITIES
CC                          2: TAKE ONLY WIDE-LANE AMBIGUITIES FROM
CC                             FILE
CC                          3: TAKE ALL INFORMATION AVAILABLE
CC                             FROM FILE (L1, L2, AND L5)
CC                          4: TAKE ALL INFORMATION AVAILABLE
CC                             FROM FILE (L1 AND L2, NO L5)
CC               NSASTC : NUMBER OF SATELLITES WITH STOCH.    I*4
CC                        ORBIT MODELLING
CC               NUMSTC : CORRESPONDING SAT. NUMBERS          I*4(*)
CC               NSTCEF : NUMBER OF STOCH. EPOCHS PER SAT.    I*4(*,*)
CC                        AND ARC
CC               NSTCEP : NUMBER OF STOCH. FORCES PER EPOCH   I*4
CC               NRQOFF : NUMBER OF SATELLITE ANTENNA OFFSET  I*4
CC                        REQUESTS
CC               PAROFF(I),I=1,2,3: SATELLITE ANTENNA OFFSET  I*4
CC                        COMPONENTS TO BE ESTIMATED (X,Y,Z
CC                        IN SATELLITE REFERENCE FRAME
CC                        =1: TO BE ESTIMATED
CC                        =0: NOT TO BE ESTIMATED
CC               NHILL  : NUMBER OF HILL PARAMETERS           I*4
CC               NPOT   : NO OF PARMS OF EARTH'S POT          I*4
CC               NALBGR : NUMBER OF ALBEDO GROUPS             I*4
CC               NALB   : NUMBER OF ALBEDO PARAMETERS/GROUP   I*4
CC               NCENM  : NUMBER OF CENTER OF MASS PARAMETER  I*4
CC                        (1, 2, OR 3)
CC               NEPOCH : NUMBER OF EPOCHS IN FILE            I*4(*)
CC               WINDOW : TIME WINDOW                         R*8(2,*)
CC               NSAMPL : SAMPLING RATE (SEC)                 I*4(3)
CC                        1: OBSERVATIONS
CC                        2: RESUBSTITUTION OF EPOCH PARAMETERS
CC                        3: PREELIMINATE OF EPOCH PARAMETERS
CC               IDELTT : OBSERVATION INTERVAL OF ALL FILES   I*4(*)
CC               DTSIM  : MAXIMAL INTERVAL TO IDENTIFY EPOCH  R*8
CC                        (DAY FRACTION)
CC               NANCAL : NUMBER OF RECEIVER ANTENNA PHASE    I*4
CC                        CENTER REQUESTS
CC               NFRCAL(I),I=1,..,NANCAL: FREQUENCY FOR REC.  I*4
CC                        ANTENNA PHASE CENTER REQUEST I
CC               NPTCAL(J,I),J=1,2, I=1,..,NANCAL: NUMBER OF  I*4
CC                        POINTS TO BE ESTIMATED IN ELEVATION
CC                        (J=1) AND AZIMUTH (J=2) DIRECTION
CC               STRAMB : AMBIGUITY RESOLUTION STRATEGY       I*4(*)
CC               OPTDIP : OPTIONS FOR DIFF. ION. PARAMETERS   I*4(3)
CC                        (1): =0: NO DIFF. ION. PARAMETERS
CC                             =1: ONE PAR. PER EPOCH AND SAT.
CC                             =2: PARAMETERS EPOCH-WISE PRE-
CC                                 ELIMINATED
CC                        (2): ELIMINATION OF REF. ION. PAR.
CC                        (3): ELEV.-DEP. PAR. CONSTRAINING
CC               OPTHOI : OPTIONS FOR HOI SCALING FACTORS     I*4(3)
CC               OPTELI(I),I=1,..,MAXTYP: OPTION FOR PRE-     I*4
CC                        ELIMINATION OF PARAMETER TYPES:
CC                        =0 : NOT PRE-ELIMINATED
CC                        =1 : PRE-ELIMINATED BEFORE INVERSION
CC                        =2 : PRE-ELIMINATED AFTER  INVERSION
CC                        =3 : PRE-ELIMINATED EPOCH-WISE
CC               NCLKST : NUMBER OF EPOCH WISE STATION CLOCKS I*4
CC               NCLKSA : NUMBER OF EPOCH WISE SAT. CLOCKS    I*4
CC               NRGB   : NUMBER OF RANGE BIASES              I*4
CC               CLKSTA(I),I=1,..,MAXSTA: STATION NUMBERS FOR I*4(*)
CC                        CLOCK ESTIMATION
CC               CLKSAT(I),I=1,..,MAXSAT: SAT. NUMBERS FOR    I*4(*)
CC                        CLOCK ESTIMATION
CC               CLKSYS : =1: ONE REC.CLK FOR EACH SAT.SYS    I*4
CC               CLKHED : CLOCK HEADER INFORMATION            T_CLKHEAD
CC                          %NUMREF=0: FIX REF-CLOCKS
CC                          %NUMREF=2: SUM FOR REF-CLOCKS
CC               CLKREC : %NEPO: EPOCHS WITH HIGHEST SAMPL.   T_CLKREC
CC                        %EPOCH(1): LAST EPOCH
CC                                (SEC. SINCE CLKHED%TFIRST)
CC               NDIFF(I),I=1,..,MAXFIL DIFFERENCE TYPE       I*4(*)
CC               STNAME(I),I=1,..,NSTAT: STATION NAMES        CH*16
CC               IOREST : ORBIT ESTIMATION FOR                I*4
CC                        =0: ALL SATELLITES
CC                        =1: GPS SATELLITES ONLY
CC                        =2: GLONASS SATELLITES ONLY
CC                        =3: LEOs ONLY
CC               NESTSAT: #SAT FOR ORBIT DETERMINATION        I*4
CC               ESTSAT : PRN FOR THAT ORBITS ARE REQU.       I*4(*)
CC               NARC2  : TOTAL NUMBER OF LEO ARCS            I*4
CC               ARCINT2(I),I=1,2,...,NFTOT : ARC NUMBER      I*4
CC                        BELONGING TO FILE I (LEO ONLY)
CC               NUMSAT2(I),I=1,2,...,NARC2: NUMBER OF        I*4
CC                        LEOs IN ARC I (LEO ONLY)
CC               NAVNUM2(L),L=1,2,... : NAV-NUMBERS FOR ALL   I*4
CC                        LEO ARCS
CC               NSASTC2: NUMBER OF LEOs WITH STOCH ORBITS    I*4
CC               NSTCEF2: NUMBER OF STOCH. EPOCHS PER LEO     I*4(*,*)
CC                        AND ARC (LEO ONLY)
CC               NUMSTC2: CORRESPONDING LEO NUMBERS           I*4(*)
CC               NSTCEP2: NUMBER OF STOCH. FORCES PER EPOCH   I*4
CC               NORB2  : NUMBER OF ORBIT PARAMETERS PER      I*4
CC                        LEO SATELLITE ARC
CC               NANSPV : NUMBER OF SATELL. ANTENNA PHASE     I*4
CC                        CENTER GROUPS
CC               NPTSPV(J,I),J=1,2, I=1,..,NANSPV: NUMBER OF  I*4
CC                        POINTS TO BE ESTIMATED IN ELEVATION
CC                        (J=1) AND AZIMUTH (J=2) DIRECTION
CC               NSASPV(I),I=1,..,NANSPV: NUMBER OF           I*4
CC                        SATELLITES BELONGING TO SAT. ANT.
CC                        PHASE CENTER GROUP I
CC               CORSTR : CORRELATION STRATEGY:               I*4
CC                         =1: CORRELATIONS WITHIN BASELINE
CC                         =2: CORRELATIONS WITHIN FREQUENCY
CC                               PROCESSED
CC                         =3: CORRELATIONS CORRECTLY MODELLED
CC               MIXED  : SATELLITE SYSTEM                    I*4
CC                        =0: GPS ONLY
CC                        =1: MIXED (GPS AND/OR GLONASS AND/OR GALILEO)
CC                        =2: GLONASS ONLY
CC                        =3: GALILEO ONLY
CC               OPLOAD : SCALING FACTORS FOR VIENNA GRID FILES   T_OPTLOAD(3)
CC                        1: ATMOSPHERIC NON-TIDAL LOADING
CC                        2: OCEAN NON-TIDAL LOADING
CC                        3: HYDROSTATIC PRESSURE LOADING
CC               OPTGSP: GNSS-SPECIFIC PARAMETER OPTIONS          T_OPTGSP
CC     IN/OUT :  OPTGIM : OPTIONS FOR GLOBAL IONOSPHERE MODEL I*4(*)
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
CC                             =1: 1/COS
CC                        (9): STATION-SPECIFIC MODELS
CC                        (10): COMPONENT TO BE ESTIMATED
CC                              =1: DETERMINISTIC
CC                              =2: STOCHASTIC
CC               POLGIM(I,J),I=1,2,3,J=1,..,OPTGIM(7):        R*8(3,*)
CC                        I=1: HEIGHT OF SINGLE LAYER (M)
CC                        I=2: LAT. OF NORTH GEOMAGNETIC POLE
CC                        I=3: EAST LONGITUDE
CC               NAMGIM(I),I=1,..,OPTGIM(7): MODEL NUMBERS    CH*16(*)
CC               EPOGIM(I,J),I=1,2,J=1,..,OPTGIM(7): PERIODS  R*8(2,*)
CC                        OF VALIDITY / REF EPOCHS (MJD)
CC               NANRAO : NUMBER OF RECEIVER ANTENNA OFFSETS  I*4
CC               NFRRAO(I),I=1,..,NANRAO: FREQUENCY FOR       I*4
CC                        RECEIVER ANT. OFFSET REQUEST I
CC               NEURAO(I),I=1,3: COMPONENTS TO BE ESTIMATED  I*4
CC                        (I=1: NORTH, I=2: EAST, I=3: UP)
CC                        =1: ESTIMATION
CC                        =0: NO ESTIMATION
CC        OUT :  NMXSNG : ACTUAL VALUE FOR MAXSNG             I*4
CC               NEPSNG : LOCAL  VALUE FOR MAXSNG in RESEPO   I*4
CC               NMXLOC : ACTUAL VALUE FOR MAXLOC             I*4
CC               NMXPAR : ACTUAL VALUE FOR MAXPAR             I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W.GURTNER, G.BEUTLER, M.ROTHACHER, S.FANKHAUSER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/03 11:00
CC
CC CHANGES    :  13-FEB-92 : ??: CHANGES DUE TO ERP-ESTIMATION
CC               23-JUL-92 : ??: ADD "NUMSAT" AND "NAVNUM" TO CALL TO SET
CC                               UP ORBITAL ELEMENTS FOR MANOEUVRE SAT.
CC               02-AUG-92 : ??: POLYNOM MODEL FOR ERP-ESTIMATION
CC               20-MAR-93 : ??: STOCHASTIC FORCE MODELLING
CC               20-MAR-93 : ??: ADD MORE INFO IN LOCQ FOR ERPS
CC               03-APR-93 : ??: ESTIMATION OF SATELLITE ANTENNA OFFSETS
CC               14-MAY-93 : ??: NEW PARAMETER TYPES (POTENTIAL, HILL,
CC                               ALBEDO, CENTER OF MASS)
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               27-DEC-93 : MR: SATELLITE OFFSETS WITH TIME WINDOWS
CC               13-APR-94 : SS: DIFFERENTIAL IONOSPHERE PARAMETERS
CC               18-APR-94 : SS: USE "WINDOW" AND "NSAMPL" TO DEFINE
CC                               ONLY NECESSARY DIFF. ION. PARAMETERS
CC               19-APR-94 : RW: CPO-MODEL PARAMETERS
CC               08-JUL-94 : SS: "DTSIM" INTRODUCED
CC               25-JUL-94 : MR: PRE-ELIMINATION STATISTICS SIMPLIFIED
CC               10-AUG-94 : MR: CALL EXITRC
CC               06-NOV-94 : MR: ANTENNA PHASE CENTER PARAMETERS
CC               01-JUN-95 : SS: "MAXLOC" REMOVED FROM CALL
CC               06-JUN-95 : SS: GLOBAL IONOSPHERE MODEL PARAMETERS
CC               08-JUN-95 : LM: KINEMATIC COORDINATES
CC               14-AUG-95 : LM: MEMORY COMPACT VERSION
CC               09-NOV-95 : SS: REMOVE "PARLST" BEFORE INITIALIZATION
CC               05-DEC-95 : SS: NEW IONOSPHERE MODEL (TYPE 2)
CC               20-DEC-95 : SS: DECLARE "NUMGIM" AS CH*7
CC               13-JAN-96 : GB: ADAPTIONS FOR NEW ORBIT MODEL.
CC                               IN PARTICULAR LOCQ(6,IPAR) DEFINED
CC                               FOR ORBIT PARAMETERS (LOCQ(1,IPAR)=3).
CC               26-MAR-96 : MR: RECEIVER ANTENNA OFFSETS
CC               29-AUG-96 : TS: EPOCHS OF STATION CLOCKS CHANGED
CC               08-APR-97 : SS: NIELL MAPPING, TROPOSPHERE GRADIENTS
CC               17-APR-97 : MR: ADD REQUEST NUMBER PER STATION
CC               05-AUG-97 : SS: ELEV.-DEP. SIP CONSTRAINING
CC               07-AUG-97 : MR: SET LOCQ(7,...) TO THE LAST EPOCH
CC                               NUMBER OF THE AMBIGUITY CLUSTER
CC               14-AUG-97 : SS: DIFFERENTIAL CODE BIASES
CC               26-SEP-97 : DI: USE MAXSAT.inc
CC               08-OCT-97 : MR: RATIO ZENITH/GRADIENT PARAMETERS
CC               22-OCT-97 : SS: CREATE SATELLITE LIST FOR DCBS
CC               08-JAN-98 : SS: SR DIMTST USED
CC               26-JAN-98 : SS: STATION-SPECIFIC GIMS
CC               02-APR-98 : SS: COPY COMPLETE "POLGIM"
CC               29-APR-98 : SS: DTEC LC
CC               27-AUG-98 : MR: USE FUNCTION "MODSVN"
CC               14-OCT-98 : MR: ORBIT ESTIMATION GPS/GLONASS ONLY
CC               21-OCT-98 : TS: ALLOW TWO FILES FOR KINEMATIC (ZERO DIFF)
CC               21-DEC-98 : SS: GPS/GLONASS DCBS FOR RECEIVERS
CC               27-JAN-00 : TS: CHANGES FOR CLOCK OUTPUT
CC               13-APR-00 : SS: ESTIMATE (P1-C1) CODE BIASES
CC               30-JUN-00 : TS: USE INTEPO FOR KIN. STATIONS
CC               25-OCT-00 : RD: COUNT OBS. FOR CLOCKS
CC               15-MAY-01 : DS: NEW PARAMETERS, INFO FOR LEO ORBIT:
CC                               IOREST, NARC2,ARCINT2,NUMSAT2,
CC                               NAVNUM2,NSASTC2,NSTCEF2,NUMSTC2
CC               23-JAN-02 : RD: CONDITION OF SUM FOR REFERENCE CLOCKS
CC               29-JAN-02 : RD: A GENERIC EPOCH COMPUTATION FOR EPOCH PARAM.
CC               07-MAY-02 : SS: DCB UPDATE
CC               23-JUN-02 : DS: LEO+GPS ORBITS: NSTCEP2,FRCTYP2,
CC                               NORB2,SEQORB2
CC               25-JUN-02 : RD/DS: MERGE VERSION BE AND MUNICH
CC               13-NOV-02 : RS: SATELLITE ANTENNA PHASE CENTER VARIATIONS
CC               22-NOV-02 : RD: PREVENT INDEX 0 FOR SATEPO
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               27-MAR-03 : RD: CORRECT TIME WINDOW CHECK FOR RCO
CC               19-MAY-03 : RD: DO NOT SETUP AMBIGUITIES OUTSIDE THE WINDOW
CC               10-JUN-03 : MM: TROPOSPHERE NOW PIECEWISE LINEAR
CC               24-JUN-03 : RD: CORRECT SETUP OF KIN COORD (TYPE 21)
CC               13-AUG-03 : RS: DO NOT SETUP SATELLITE ANTENNA PATTERNS,
CC                               IF SATELLITE GROUP IS EMPTY
CC               10-NOV-03 : RS: ADD GNROFF, GRPSPV -> GNRSPV
CC               24-NOV-03 : HU: DO NOT SET UP STATIC LEO COORDINATES
CC               05-DEC-03 : RS: ESTIMATE HALF SET OF SPHERICAL HARMONICS
CC                               WHEN USING UPPER HEMISPHERE ONLY
CC               19-JAN-03 : SS/MM: REVISION OF GPSEST INPUT PANELS
CC               03-MAR-04 : RD: SET NCLS TO INIT IR5CLS IN SR AMBINF
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               05-OCT-05 : RD: ADJUST "MAXSNG" FOR RESEPO
CC               16-Oct-06 : RD: MANUAL SELECTION OF SAT. FOR ORBIT DETERM.
CC               03-APR-07 : AG: USE SVNSYS INSTEAD OF MIXSVN
CC               15-MAR-08 : RD: COMPUTE NUM.OF PARAM. FOR PCV-SH CORRECT
CC               02-Apr-09 : DT: ADD RANGE BIASES (NRGB IN CALL)
CC               05-MAY-09 : RD: SCALING OF LOADING MODELS ADDED
CC               09-MAY-09 : RD: SAT/FRQ-SPECIFIC RECEIVER CLOCK BIASES
CC               09-MAY-09 : RD: SEPERATE RECEIVER CLOCKS FOR GPS/GLONASS
CC               27-MAY-09 : RD: SPECIAL SAMPLING FOR RESUBST. OF EPOCH PARAM.
CC               04-JAN-10 : SL: OPTHOI for HOI SCALING FACTORS added
CC               25-NOV-10 : MM: GNSS-SPECIFIC PARAMETERS
CC               17-FEB-11 : SS: STRAMB(3) FOR SELECTION OF GNSS
CC               06-JUN-12 : LP: USE F_SVNSYS AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_time,   ONLY: t_timint
      USE d_clkrnx, ONLY: t_clkhead,t_clkrec
      USE m_maxdim, ONLY: MAXGIM
      USE d_stacrx, ONLY: MTypeSPACE
      USE p_gpsest, ONLY: t_optLoad,t_isbTime,t_optGsp
      USE f_mixsvn
      USE s_dimtst
      USE s_mjdgps
      USE f_modsvn
      USE s_seqgim
      USE s_maxtst
      USE s_staflg
      USE s_exitrc
      USE s_ambinf
      USE s_inlist
      USE s_rdpwin
      USE s_parint
      USE s_wildcd
      USE s_getRcv
      USE f_svnsys
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I      , IA     , IAMB   , IAZI   , ICAL   , ICAR   ,
     1          ICLS   , ICOR   , ICR    , IDEG   , IEPO   , IEPPAR ,
     2          IF     , IFIL   , IFIX   , IFLAG  , IFR    , IGIM   ,
     3          IHOUR  , IKIN   , ILAT   , ILIM   , IMIX   , IMOD   ,
     4          IORD   , IOREST , IPRE   , IRAO   , IRC    , IRC1   ,
     5          IRC2   , IREC   , IREQ   , IS     , ISAT   , ISAT0  ,
     6          ISATEL , ISPV   , ISTAT  , ISTC   , ISTREQ , ISTTRP ,
     7          ITYP   , IZEN   , JAMB   , K      , KPAR   , MAXAMB ,
     8          MAXFLS , MAXFRS , MAXLOC , MAXPRE , MAXSAS , MAXSAT ,
     9          MAXTYP , MELWUB , MIXED  , MXCAMB , MXNCLK , NEPSNG ,
     1          MXCFRQ , MXCLCQ , MXCLOC , MXCSAT , MXS    , MXX1   ,
     2          NALB   , NALBGR , NANCAL , NANRAO , NARC   , NARC2  ,
     3          NCENM  , NCLKSA , NCLKST , NCLREQ , NCLS   , NCOR   ,
     4          NDEG   , NDTOT  , NERPAR , NFIX   , NFRQ   , NFTOT  ,
     5          NHILL  , NIOREQ , NKIN   , NLEORB , NLESTC , NMOD   ,
     6          NMXAMP , NMXLOC , NMXPAR , NMXSNG , NORB   , NORB2  ,
     7          NORBT  , NORD   , NPOL   , NPOT   , NREC   , NRQOFF ,
     8          NSARC  , NSASTC , NSASTC2, NSAT   , NSAT0  , NEPO   ,
     9          NSTAT  , NSTCEP , NSTCEP2, NTRREQ , NTRSTA , NWEEK  ,
     1          CLKSYS , IEST   , NESTSAT, NSTA   , II     , NISB   ,
     2          NUMISB , IHOI   , NRDCB  , NRGB   , NFREQ  , isys   ,
     3          ICLASS , ISYS1  , ISYS2  , NTRA   , NTRP

C
      REAL*8    AEPOCH , DTSIM  , GPSSEC , TFRAC  , TOBS   , T_0    ,
     1          DT_0
C
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL LOCAL DIMENSIONS
C ------------------------
c      PARAMETER (MAXAMB=1000)
C
C
      TYPE(t_clkhead) CLKHED
      TYPE(t_clkrec)  CLKREC
      TYPE(t_optLoad), DIMENSION(:):: opLoad
      TYPE(t_isbTime), DIMENSION(:), POINTER:: isbTim
      TYPE(t_timint)   :: globalWindow
      TYPE(t_timint),  DIMENSION(:), POINTER:: iSbWin
      TYPE(t_optGsp) OPTGSP
C
      REAL*8       CLFRTO(2,*),TIMREF(*),WINDOW(2,*)
      REAL*8       POLGIM(3,*),EPOGIM(2,*),AMBWIN(2)
C
      INTEGER*4    STFIL(*),STFIX(*),STKIN(*),ICENTR(*)
      INTEGER*4    ARCINT(*)
      INTEGER*4    ARCINT2(*),IBIAS(*)
      INTEGER*4    ISTCLK(*),NCLK(*),NFRFIL(*),ICARR(MXCFRQ,*)
      INTEGER*4    STATRP(*),CORSTR
      INTEGER*4    MEATYP(*),IONREQ(3,*),NPARTR(*)
      INTEGER*4    NSATEL(*),SATNUM(MXCSAT,*),NSHLP(MAXSAT)
      INTEGER*4    NUMAMB(*),AMBCLS(MXCAMB,3,*),AMBSAT(MXCAMB,*)
      INTEGER*4    AMBIEP(MXCAMB,*),AMBDEF(*)
      INTEGER*4    POLMOD,POLPAR(*),NUMSAT(*),NAVNUM(*)
      INTEGER*4    NUMSAT2(*),NAVNUM2(*)
      INTEGER*4    NUMSTC(*),NSTCEF(MXCSAT,*),PAROFF(*)
      INTEGER*4    NUMSTC2(*),NSTCEF2(MXCSAT,*)
      INTEGER*4    NUMCLS(MAXAMB),IR5CLS(MAXAMB)
      INTEGER*4    NEPOCH(*),IDELTT(*)
      INTEGER*4    NFRCAL(*),NPTCAL(2,*),OPTGIM(*)
      INTEGER*4    STRAMB(*),OPTDIP(*),OPTHOI(*),OPTELI(*)
      INTEGER*4    NEURAO(3),NFRRAO(*),OPTDCB(*)
      INTEGER*4    NDIFF(*)
      INTEGER*4    SVNORB,SVNOBS,ITRGRD(*),NSAMPL(3)
      INTEGER*4    SATLIS(MAXSAT)
      INTEGER*4    NANSPV,NPTSPV(2,*),NSASPV(*)
      INTEGER*4    NUMPAR(MAXTYP)
      INTEGER*4    ESTSAT(*),IRUNIT(2,*)
      INTEGER*4    ICODE(2),IWLFAC(2)
      INTEGER*4    SYSPAR(2)
C
      CHARACTER*16 NAMGIM(*),STNAME(*)
      CHARACTER*6  MXNSAT,MXNFRQ,MXNAMB,MXNLCQ,MXNLOC
      CHARACTER*20 MARTYP,RECTYP(2,*)
      CHARACTER*20 rcvTyp
C
      LOGICAL      IsSYS(10)
C
C COMMON FOR MAXIMAL DIMENSIONS
C -----------------------------
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMLOC/MXCLOC,MXNLOC
C
C
      MAXLOC=MXCLOC
C
C CHECK MAXIMUM LOCAL DIMENSIONS
C ------------------------------
      CALL MAXTST(1,'INIMAX',MXNSAT,MAXSAT,MXCSAT,IRC1)
      CALL MAXTST(1,'INIMAX',MXNAMB,MAXAMB,MXCAMB,IRC2)
      IF(IRC1+IRC2.NE.0) CALL EXITRC(2)
C
C INITIALIZE MAXSNG
C -----------------
      NMXSNG=0
      NEPSNG=0
      NUMPAR=0
      IEPPAR=0
      NULLIFY(iSbWin)
C
C ZERO OR SINGLE?
C ---------------
      NDTOT=0
      DO IFIL=1,NFTOT
        IF (NDIFF(IFIL).GT.NDTOT) NDTOT=NDIFF(IFIL)
      ENDDO
C
C DEFINITION OF STATION COORDINATES
C ---------------------------------
      IF (ICARR(1,1).EQ.4 .OR. MELWUB.GT.0) GOTO 70
C
      ITYP=1
      DO 60 ISTAT=1,NSTAT
        CALL staflg(stname(istat),timref(1),iflag,martyp)
        IF (martyp.EQ.MTypeSPACE)  GOTO 60
        IF(ICENTR(ISTAT).NE.ISTAT) GOTO 60
        IF(NFIX.EQ.0 .AND. NKIN.EQ.0) GOTO 40
        DO 30 IFIX=1,NFIX
          IF(STFIX(IFIX).EQ.ISTAT) GOTO 60
30      CONTINUE
        DO 31 IKIN=1,NKIN
          IF(STKIN(IKIN).EQ.ISTAT) GOTO 60
31      CONTINUE
40      NUMPAR(ITYP)=NUMPAR(ITYP)+3
60    CONTINUE
      IF (NUMPAR(ITYP).GT.0) NMXSNG=NMXSNG+(NDTOT+1)*3
C
70    CONTINUE
C
C STATION CLOCK PARAMETERS
C ------------------------
      ITYP=2
C
      MXNCLK=0
      DO 140 ICR=1,NCLREQ
C
C CHECK VALIDITY OF REQUEST
        DO 100 IF=1,NFTOT
          IF(TIMREF(IF)+DBLE(NEPOCH(IF)*IDELTT(IF))/86400D0.LT.
     1       CLFRTO(1,ICR))GO TO 100
          IF(TIMREF(IF).GT.CLFRTO(2,ICR))GO TO 100
          IF(ISTCLK(ICR).NE.STFIL(2*IF-1).AND.
     1       ISTCLK(ICR).NE.STFIL(2*IF))
     2      GO TO 100
C
C REQUEST IS O.K.
          GO TO 120
100     CONTINUE
C
C INVALID REQUEST
        WRITE(LFNERR,110) ICR
110     FORMAT(/,' *** SR INIMAX: INVALID CLOCK REQUEST',/,
     1                       16X,'REQUEST NUMBER:',I3,/)
        CALL EXITRC(2)
120     CONTINUE
        IF (IBIAS(ICR).EQ.0) THEN
          NUMPAR(ITYP)=NUMPAR(ITYP)+NCLK(ICR)
          IF (MXNCLK<NCLK(ICR)) MXNCLK=NCLK(ICR)
        ELSE IF (IBIAS(ICR).EQ.4) THEN
          DO IFIL=1,NFTOT
            IF (MEATYP(IFIL).EQ.2) THEN
              NUMPAR(ITYP)=NUMPAR(ITYP)+NCLK(ICR)
              IF (MXNCLK<NCLK(ICR)) MXNCLK=NCLK(ICR)
              EXIT
            ENDIF
          ENDDO
        ELSE
          DO IFIL=1,NFTOT
            IF (MEATYP(IFIL).EQ.2) THEN
              NUMPAR(ITYP)=NUMPAR(ITYP)+NCLK(ICR)*MAXSAS
              IF (MXNCLK<NCLK(ICR)) MXNCLK=NCLK(ICR)
              EXIT
            ENDIF
          ENDDO
        ENDIF
140   CONTINUE
C
      IF (NUMPAR(ITYP).GT.0) NMXSNG=NMXSNG+MXNCLK*(NDTOT+1)
C
C Time-dependent ISBs (not yet GALILEO-ready due to MIXSVN)
C ---------------------------------------------------------
C     GET THE PARAMETER TIME WINDOW DEFINITION
      CALL rdpwin('PAR_OFF',(/' ',' '/),t_0,dt_0)

      numIsb = 0
      DO IFIL=1,NFTOT
        IF (MIXSVN(NSATEL(IFIL),SATNUM(1,IFIL)) .NE. 1) CYCLE
        IF (NREC == 0) EXIT
        DO IREC=1,NREC
          IF (LEN_TRIM(isbTim(IREC)%recnam) == 0) CYCLE
          DO II = 1,NDIFF(IFIL)+1
            CALL WILDCD(isbTim(IREC)%recnam,RECTYP(II,IFIL),IRC)
            IF (IRC.EQ.1.AND.
     1          IRUNIT(II,IFIL).GE.isbTim(iRec)%recnum(1) .AND.
     2          IRUNIT(II,IFIL).LE.isbTim(iRec)%recnum(2)) THEN
                CALL parint(globalWindow,dtSim,t_0,dt_0,
     1               isbTim(IREC)%isbint,
     2               'Time-dependent interfrequency biases',
     3               nISb,iSbWin)
                DEALLOCATE(isbWin,stat=irc)
              NUMPAR(ITYP)=NUMPAR(ITYP)+nIsb+100*0
              numIsb = numIsb + nIsb + 100*0
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
      IF (NUMISB.GT.0) NMXSNG=NMXSNG+2*(NDTOT+1)*4
C
C ORBIT PARAMETERS
C ----------------
      IF (IOREST.EQ.3) GOTO 200
      ITYP=3
C
      NORBT=0
      NSAT0=0
      DO 190 IA=1,NARC
        NSARC=0
        DO 165 ISAT0=1,NUMSAT(IA)
C
          SVNORB=NAVNUM(NSAT0+ISAT0)
          SVNOBS=MODSVN(NAVNUM(NSAT0+ISAT0))
C
C SELECT SATELLITE FOR ORBIT DETERMINATION
          CALL INLIST(SVNOBS,NESTSAT,ESTSAT,IEST)
          IF (IEST.EQ.0) GOTO 165
C
C CHECK WHETHER A SATELLITE IN THE STD.ORBIT FILE IS OBSERVED
          DO 160 IF=1,NFTOT
            IF (ARCINT(IF).NE.IA) GOTO 160
            DO 150 K=1,NSATEL(IF)
              IF(SATNUM(K,IF).EQ.SVNOBS) GOTO 155
150         CONTINUE
160       CONTINUE
C
C SATELLITE NOT OBSERVED
          GOTO 165
C
C SATELLITE IS OBSERVED, ADD IT TO LIST OF SATELLITES TO BE ESTIMATED
155       NSARC=NSARC+1
          NSHLP(NSARC)=SVNORB
165     CONTINUE
        NSAT0=NSAT0+NUMSAT(IA)
        NUMPAR(ITYP)=NUMPAR(ITYP)+NSARC*NORB
C
C ADD STOCHASTIC ORBIT PARAMETERS
        DO 180 IS=1,NSARC
          DO 171 ISTC=1,NSASTC
            IF(NSHLP(IS).EQ.NUMSTC(ISTC))GO TO 172
171       CONTINUE
          GO TO 180
172       CONTINUE
C
C SATELLITE FOUND IN LIST NUMSTC
          NUMPAR(11)=NUMPAR(11)+NSTCEP*NSTCEF(ISTC,IA)
180     CONTINUE
190   CONTINUE
      IF (NUMPAR(ITYP).GT.0) NMXSNG=NMXSNG+(NDTOT+1)*NORB
      IF (NUMPAR(11).GT.0)
     1  NMXSNG=NMXSNG+(NDTOT+1)*NSTCEP*MAXVAL(NSTCEF(1:NSASTC,1))
C
C ORBIT PARAMETERS FOR LEO
C ------------------------
200   IF (IOREST.EQ.3.OR.IOREST.EQ.0) THEN
        ITYP=3
        NORBT=0
        NSAT0=0
        NLEORB=0
        NLESTC=0
        DO IA=1,NARC2
          NSARC=0
          DO 265 ISAT0=1,NUMSAT2(IA)
C
            SVNORB=NAVNUM2(NSAT0+ISAT0)
            SVNOBS=MODSVN(NAVNUM2(NSAT0+ISAT0))
C
C ORBIT ESTIMATION FOR LEOs
            IF (SVNOBS.LT.900) GOTO 265
C
C CHECK WHETHER A LEO IN THE STD.ORBIT FILE IS OBSERVED
            DO 260 IF=1,NFTOT
              IF (ARCINT2(IF).NE.IA) GOTO 260
CCC              DO K=1,NLEO(IF)
CCC                IF(LEONUM(K,IF).EQ.SVNOBS) GOTO 255
CCC              END DO
260         CONTINUE
C
C LEO NOT OBSERVED
CCC            GOTO 265
C
C LEO IS OBSERVED, ADD IT TO LIST OF SATELLITES TO BE ESTIMATED
255         NSARC=NSARC+1
            NSHLP(NSARC)=SVNORB
265       CONTINUE
          NSAT0=NSAT0+NUMSAT2(IA)
C
          NLEORB=NLEORB+NSARC*NORB2
C
C ADD STOCHASTIC ORBIT PARAMETERS FOR LEO
          DO 280 IS=1,NSARC
            DO ISTC=1,NSASTC2
              IF(NSHLP(IS).EQ.NUMSTC2(ISTC))GO TO 272
            END DO
            GO TO 280
272         CONTINUE
C
C SATELLITE FOUND IN LIST NUMSTC2
            NLESTC=NLESTC+NSTCEP2*NSTCEF2(ISTC,IA)
280       CONTINUE
        END DO
        NUMPAR(ITYP)=NUMPAR(ITYP)+NLEORB
        NUMPAR(11)=NUMPAR(11)+NLESTC
        IF (NLEORB.GT.0) NMXSNG=NMXSNG+(NDTOT+1)*NORB2
        IF (NLESTC.GT.0) NMXSNG=NMXSNG+
     1          (NDTOT+1)*NSTCEP2*MAXVAL(NSTCEF2(1:NSASTC2,1))
      END IF
C
C RECEIVER ANTENNA OFFSETS
C ------------------------
      ITYP=5
C
      NCOR=0
      DO ICOR=1,3
        IF (NEURAO(ICOR).EQ.1) NCOR=NCOR+1
      ENDDO
C
      DO 950 IRAO=1,NANRAO
        IF (NFRRAO(IRAO).EQ.3) THEN
          NFRQ=2
        ELSE
          NFRQ=1
        ENDIF
        NUMPAR(ITYP)=NUMPAR(ITYP)+NFRQ*NCOR
950   CONTINUE
      IF (NUMPAR(ITYP).GT.0) NMXSNG=NMXSNG+(NDTOT+1)*NFRQ*NCOR
C
C STATION SPECIFIC TROPOSPHERE PARAMETERS
C ---------------------------------------
      ITYP=6
C
      ISTTRP=0
      DO 210 I=1,NTRSTA
C
C COUNT REQUESTS OF EACH STATION
        IF (ISTTRP.NE.STATRP(I)) ISTREQ=0
        ISTREQ=ISTREQ+1
        ISTTRP=STATRP(I)
C
        IF (ITRGRD(1).EQ.0) THEN
          NUMPAR(ITYP)=NUMPAR(ITYP)+1
C
C ZENITH/GRADIENT RATIO
        ELSEIF (MOD(ISTREQ-1,ITRGRD(2)).NE.0) THEN
          NUMPAR(ITYP)=NUMPAR(ITYP)+1
        ELSE
          NUMPAR(ITYP)=NUMPAR(ITYP)+3
        ENDIF
210   CONTINUE
C
      IF (NUMPAR(ITYP).GT.0 .AND. ITRGRD(1).EQ.0) THEN
        NMXSNG=NMXSNG+(NDTOT+1)*2
      ELSE IF (NUMPAR(ITYP).GT.0) THEN
        NMXSNG=NMXSNG+(NDTOT+1)*2*3
      ENDIF
C
C  IONOSPHERIC DEVELOPMENT
C  -----------------------
      ITYP=7
C
      DO 410 IREQ=1,NIOREQ
        DO 420 ILAT=0,IONREQ(1,IREQ)
          DO 430 IHOUR=0,IONREQ(2,IREQ)
            IF(ILAT+IHOUR.GT.IONREQ(3,IREQ)) GOTO 420
            NUMPAR(ITYP)=NUMPAR(ITYP)+1
430       CONTINUE
420     CONTINUE
410   CONTINUE
      IF (NUMPAR(ITYP).GT.0)
     1   NMXSNG=NMXSNG+(MAXVAL(IONREQ(1,1:NIOREQ))+1)*
     2          (MAXVAL(IONREQ(2,1:NIOREQ))+1)
C
C DIFFERENTIAL CODE BIASES FOR SATELLITES AND RECEIVERS
C -----------------------------------------------------
      ITYP=8
C
C SET UP RECEIVER-SPECIFIC DCBS
      NSAT=0
      IF (OPTDCB(1).GT.0) THEN
C
C CREATE LIST OF OBSERVED SATELLITES
        DO IFIL=1,NFTOT
          DO 850 ISATEL=1,NSATEL(IFIL)
            DO ISAT=1,NSAT
              IF (SATNUM(ISATEL,IFIL).EQ.SATLIS(ISAT)) GOTO 850
            ENDDO
C
            NSAT=NSAT+1
            SATLIS(NSAT)=SATNUM(ISATEL,IFIL)
850       CONTINUE
        ENDDO
      ENDIF
C
C CHECK WHETHER REFERENCE SATELLITE IS OBSERVED
      IF (OPTDCB(3).GT.0) THEN
        DO ISAT=1,NSAT
          IF (OPTDCB(3).EQ.SATLIS(ISAT)) GOTO 860
        ENDDO
C
        WRITE(LFNERR,870) OPTDCB(3)
870     FORMAT(/,' *** SR INIMAX: ESTIMATION OF DIFFERENTIAL CODE ',
     1    'BIASES',/,
     2    16X,'REFERENCE SATELLITE ',I3.2,' NOT OBSERVED',/)
        CALL EXITRC(2)
C
860     CONTINUE
      ENDIF
C
C SET UP RECEIVER-SPECIFIC DCBS
      NRDCB=0
      IF (OPTDCB(2).NE.0) THEN
        DO 845 IREC=1,NSTAT
          IsSYS=.false.
          DO IFIL=1,NFTOT
           IF (STFIL(2*IFIL-1).EQ.IREC) THEN
              DO isys=0,2
                IF(SVNSYS(isys,NSATEL(IFIL),SATNUM(:,IFIL)))
     1                                         IsSYS(isys+1)=.true.
              ENDDO
           ENDIF
          ENDDO
          DO isys=1,3
            IF (IsSYS(isys)) NRDCB=NRDCB+1
          ENDDO
845     CONTINUE
      ENDIF
C
      NUMPAR(ITYP)=NUMPAR(ITYP)+NSAT+NRDCB
      IF (NSAT.GT.0) NMXSNG=NMXSNG+(NDTOT+1)
      IF (NRDCB.GT.0) NMXSNG=NMXSNG+(NDTOT+1)*2
C
C LOCAL TROPOSPHERE MODEL PARAMETERS
C ----------------------------------
      ITYP=9
C
      DO 550 IMOD=1,NTRREQ
        NUMPAR(ITYP)=NUMPAR(ITYP)+NPARTR(IMOD)
550   CONTINUE
C
      IF (NUMPAR(ITYP).GT.0)
     1  NMXSNG=NMXSNG+(NDTOT+1)*MAXVAL(NPARTR(1:NTRREQ))
C
C EARTH ROTATION PARAMETERS
C -------------------------
      ITYP=10
C
      IF (POLMOD.EQ.1) THEN
        NERPAR=0
        DO 610 KPAR=1,5
          NERPAR=NERPAR+POLPAR(KPAR)
610     CONTINUE
        NUMPAR(ITYP)=NUMPAR(ITYP)+NERPAR*NPOL
        IF (NUMPAR(ITYP).GT.0) NMXSNG=NMXSNG+5*MAXVAL(POLPAR(1:5))
      ENDIF
C
C SATELLITE ANTENNA OFFSETS
C -------------------------
      ITYP=12
C
      IF (NRQOFF.NE.0) THEN
        DO 700 ICOR=1,3
          IF (PAROFF(ICOR).EQ.0) GOTO 700
          NUMPAR(ITYP)=NUMPAR(ITYP)+NRQOFF
700     CONTINUE
        IF (NUMPAR(ITYP).GT.0) NMXSNG=NMXSNG+(NDTOT+1)*3
      ENDIF
C
C EARTH' POTENTIAL
C ----------------
      ITYP=13
C
      IF(NPOT.NE.0)THEN
        NUMPAR(ITYP)=NUMPAR(ITYP)+NPOT
        NMXSNG=NMXSNG+NPOT
      END IF
C
C HILL'S RESONANCE TERMS
C ----------------------
      ITYP=14
C
      IF(NHILL.NE.0)THEN
        NUMPAR(ITYP)=NUMPAR(ITYP)+NARC*NHILL
        NMXSNG=NMXSNG+NHILL
      END IF
C
C ALBEDO PARAMETERS
C -----------------
      ITYP=15
C
      IF(NALB.NE.0.AND.NALBGR.NE.0)THEN
        NUMPAR(ITYP)=NUMPAR(ITYP)+NALBGR*NALB
        NMXSNG=NMXSNG+NALBGR*NALB
      END IF
C
C CENTER OF MASS PARAMETERS
C -------------------------
      ITYP=16
C
      IF(NCENM.NE.0)THEN
        NUMPAR(ITYP)=NUMPAR(ITYP)+NCENM
        NMXSNG=NMXSNG+NCENM
      END IF
C
C DIFFERENTIAL IONOSPHERE PARAMETERS
C ----------------------------------
      ITYP=17
C
      IF(OPTDIP(1).EQ.1)THEN
        DO 805 IFIL=1,NFTOT
          DO 810 ISAT=1,NSATEL(IFIL)
            DO 815 IEPO=1,NEPOCH(IFIL)
              TOBS=TIMREF(IFIL)+(IEPO-1)*IDELTT(IFIL)/86400.D0
              IF(TOBS.LT.(WINDOW(1,IFIL)-DTSIM).OR.
     1           TOBS.GT.(WINDOW(2,IFIL)+DTSIM)) GOTO 815
              IF(NSAMPL(1).NE.0.AND.NSAMPL(3).EQ.0)THEN
                CALL MJDGPS(TOBS,GPSSEC,NWEEK)
                TFRAC=GPSSEC-DNINT(GPSSEC/NSAMPL(1))*NSAMPL(1)
                IF(DABS(TFRAC).GT.DTSIM*86400.D0) GOTO 815
              ELSE IF(NSAMPL(3).NE.0)THEN
                CALL MJDGPS(TOBS,GPSSEC,NWEEK)
                TFRAC=GPSSEC-DNINT(GPSSEC/NSAMPL(3))*NSAMPL(3)
                IF(DABS(TFRAC).GT.DTSIM*86400.D0) GOTO 815
              ENDIF
              NUMPAR(ITYP)=NUMPAR(ITYP)+1
815         CONTINUE
810       CONTINUE
805     CONTINUE
      ELSEIF(OPTELI(ITYP).EQ.3) THEN
        IEPPAR=1
        NUMPAR(ITYP)=NUMPAR(ITYP)+MAXFLS*MAXSAS
      ENDIF
      IF (NUMPAR(ITYP).GT.0) NMXSNG=NMXSNG+(NDTOT+1)
C
C RECEIVER ANTENNA PHASE CENTER VARIATIONS
C ----------------------------------------
      ITYP=18
C
      MXS=0
      DO 900 ICAL=1,NANCAL
        IF (NFRCAL(ICAL).EQ.3) THEN
          NFRQ=2
        ELSE
          NFRQ=1
        ENDIF
        IF (NPTCAL(1,ICAL).GT.0) THEN
          MXX1=NPTCAL(1,ICAL)*(NPTCAL(2,ICAL)-1)
          IF (MXX1.GT.MXS) MXS=MXX1
          NUMPAR(ITYP)=NUMPAR(ITYP)+
     1         IABS(NPTCAL(1,ICAL))*NFRQ*(NPTCAL(2,ICAL)-1)
        ELSE
          ILIM = MIN0(-NPTCAL(1,ICAL),NPTCAL(2,ICAL))
          MXX1=(2*ILIM+1)**2
          IF (MXX1.GT.MXS) MXS=MXX1
          DO 915 IAZI=-ILIM,ILIM
            DO 935 IZEN=1,-NPTCAL(1,ICAL)
              IF (IZEN.LT.IABS(IAZI)) GO TO 935
              IF (MOD(IZEN-IABS(IAZI),2).NE.0) GOTO 935
              NUMPAR(ITYP)=NUMPAR(ITYP)+NFRQ
935         CONTINUE
915       CONTINUE
        END IF
900   CONTINUE
      IF (NUMPAR(ITYP).GT.0) NMXSNG=NMXSNG+(NDTOT+1)*NFRQ*MXS
C
C GLOBAL IONOSPHERE MODEL PARAMETERS
C ----------------------------------
      ITYP=19
C
      CALL SEQGIM(OPTGIM,POLGIM,NAMGIM,EPOGIM)
C
      IF (OPTGIM(9).EQ.1) THEN
        NMOD=NSTAT
C
        CALL DIMTST(1,1,2,'INIMAX','MAXGIM',
     1    'GLOBAL IONOSPHERE MODELS','INCLUDE FILE USED',NMOD,MAXGIM,
     2    IRC)
C
      ELSE
        NMOD=OPTGIM(7)
      ENDIF
C
      IF (NMOD.EQ.1.AND.OPTGIM(5).EQ.1) OPTGIM(5)=2
C
C SINGLE-LAYER HEIGHT
      IF (OPTGIM(5).EQ.1) THEN
        NUMPAR(ITYP)=NUMPAR(ITYP)+1
      ELSE IF (OPTGIM(5).EQ.2) THEN
        NUMPAR(ITYP)=NUMPAR(ITYP)+NMOD
      END IF
      NMXSNG=NMXSNG+OPTGIM(5)
C
C IONOSPHERIC COEFFICIENTS
      NDEG=OPTGIM(1)
      NORD=OPTGIM(2)
C
      IGIM=0
      IF (NMOD.GT.0) THEN
        DO 1930 IDEG=0,NDEG
          DO 1940 IORD=0,NORD
            IF (IORD.GT.IDEG) GO TO 1930
            NUMPAR(ITYP)=NUMPAR(ITYP)+NMOD
            IGIM=IGIM+1
            IF (IORD.GT.0) THEN
              NUMPAR(ITYP)=NUMPAR(ITYP)+NMOD
              IGIM=IGIM+1
            ENDIF
1940      CONTINUE
1930    CONTINUE
      ENDIF
      IF (IGIM.GT.0) NMXSNG=NMXSNG+2*IGIM
C
C KINEMATIC COORDINATES
C ---------------------
      ITYP=21
C
      IF (OPTELI(ITYP).EQ.3) THEN
        NUMPAR(ITYP)=NUMPAR(ITYP)+3*NKIN
        IEPPAR=1
        IF (NSAMPL(3).NE.0) THEN
          NEPO=DNINT(CLKREC%EPOCH(1)/NSAMPL(3))+1
          NUMPAR(ITYP)=NUMPAR(ITYP)+NEPO*3*NKIN
        ENDIF
      ELSE
        NUMPAR(ITYP)=NUMPAR(ITYP)+CLKREC%NEPO*3*NKIN
      ENDIF
      IF (NKIN.GT.0) THEN
        NMXSNG=NMXSNG+(NDTOT+1)*3
        IF (OPTELI(ITYP).EQ.3) NEPSNG=NEPSNG+(NDTOT+1)*3
      ENDIF
C
C SCALING FACTORS FOR VIENNA GRID FILES
C -------------------------------------
      ITYP=22
C
      DO II = 1,SIZE(OPLOAD)
        NSTA = OPLOAD(II)%NSTA
        IF (NSTA > 0) THEN
          IF (OPLOAD(II)%STACLU(1).EQ.-1) NSTA = 1
        ENDIF
        NUMPAR(ITYP)=NUMPAR(ITYP)+NSTA*OPLOAD(II)%NPAR
        IF (OPLOAD(II)%NSTA.GT.0) THEN
          NMXSNG=NMXSNG+(NDTOT+1)*OPLOAD(II)%NPAR
        ENDIF
      ENDDO
C
C EPOCH CLOCKS STATIONS
C ---------------------
      ITYP=23
C
      IF (OPTELI(ITYP).EQ.3) THEN
        NUMPAR(ITYP)=NUMPAR(ITYP)+NCLKST*(CLKSYS+1)
        IEPPAR=1
        IF (NSAMPL(3).NE.0) THEN
          NEPO=DNINT(CLKREC%EPOCH(1)/NSAMPL(3))+1
          NUMPAR(ITYP)=NUMPAR(ITYP)+NEPO*NCLKST
        ENDIF
      ELSE
        NUMPAR(ITYP)=NUMPAR(ITYP)+CLKREC%NEPO*NCLKST*(CLKSYS+1)
      ENDIF
      IF (NCLKST.GT.0) THEN
        NMXSNG=NMXSNG+(NDTOT+1)
        IF (OPTELI(ITYP).EQ.3) NEPSNG=NEPSNG+(NDTOT+1)
      ENDIF
C
C EPOCH CLOCKS SATELLITES
C -----------------------
      ITYP=24
C
      IF (OPTELI(ITYP).EQ.3) THEN
        NUMPAR(ITYP)=NUMPAR(ITYP)+NCLKSA
        IEPPAR=1
        IF (NSAMPL(3).NE.0) THEN
          NEPO=DNINT(CLKREC%EPOCH(1)/NSAMPL(3))+1
          NUMPAR(ITYP)=NUMPAR(ITYP)+NEPO*NCLKSA
        ENDIF
      ELSE
        NUMPAR(ITYP)=NUMPAR(ITYP)+CLKREC%NEPO*NCLKSA
      ENDIF
      IF (NCLKSA.GT.0) THEN
        NMXSNG=NMXSNG+(NDTOT+1)
        IF (OPTELI(ITYP).EQ.3) NEPSNG=NEPSNG+(NDTOT+1)
      ENDIF
C
C SATELLITE ANTENNA PHASE CENTER VARIATIONS
C -----------------------------------------
      ITYP=25
C
      IF (NANSPV.NE.0) THEN
        DO 1200 ISPV=1,NANSPV
          IF (NSASPV(ISPV).EQ.0) CYCLE
          NUMPAR(ITYP)=NUMPAR(ITYP)+NPTSPV(1,ISPV)*(NPTSPV(2,ISPV)-1)
1200    CONTINUE
        NMXSNG=NMXSNG+(NDTOT+1)*MAXVAL(NPTSPV(1,1:NANSPV))*
     1                         (MAXVAL(NPTSPV(2,1:NANSPV))-1)
      ENDIF
C
C SLR RANGE BIASES
C -----------------
      ITYP=26
C
      IF(NRGB.NE.0)THEN
        NUMPAR(ITYP)=NUMPAR(ITYP)+NRGB*MAXSAT
        NMXSNG=NMXSNG+(NDTOT+1)*NRGB*MAXSAT
      END IF
C
C HOI SCALING FACTORS
C -------------------
      ITYP=27
C
      DO IHOI=1,3
        IF(OPTHOI(IHOI).EQ.0) THEN
          CYCLE
        ELSEIF(OPTHOI(IHOI).EQ.1) THEN
          NUMPAR(ITYP)=NUMPAR(ITYP)+1
          NMXSNG=NMXSNG+1
        ELSEIF(OPTHOI(IHOI).EQ.2) THEN
          NUMPAR(ITYP)=NUMPAR(ITYP)+NSTAT
          NMXSNG=NMXSNG+(NDTOT+1)
        ENDIF
      ENDDO
C
C GNSS-SPECIFIC PARAMETER SETUP
C -----------------------------
      ITYP=30
      NTRA=0
      NTRP=0
C
      IF (OPTGSP%TRASYS.EQ.0 .AND. OPTGSP%TRPSYS.EQ.0) GOTO 3020
      DO 3010 ISTAT=1,NSTAT
        SYSPAR = 0
C
C FIND RECEIVER
        DO IFIL=1,NFTOT
          ISYS2=-1
          IF ((STFIL(2*(IFIL-1)+1).EQ.ISTAT               ) .OR.
     1        (STFIL(2*(IFIL-1)+2).EQ.ISTAT.AND.NDTOT.EQ.1)     ) THEN
            RCVTYP = RECTYP(1,IFIL)
            CALL GETRCV(RCVTYP,NFREQ,ICODE,IWLFAC,ICLASS,ISYS1)
            IF (NDTOT.EQ.1) THEN
              RCVTYP = RECTYP(2,IFIL)
              CALL GETRCV(RCVTYP,NFREQ,ICODE,IWLFAC,ICLASS,ISYS2)
            ENDIF
C
C COMBINED RECEIVER(S): GLONASS
            IF ( (ISYS1.EQ.-1.OR.ISYS1.EQ.1).AND.
     1           (ISYS2.EQ.-1.OR.ISYS2.EQ.1).AND. SYSPAR(1).EQ.0) THEN
              SYSPAR(1) = 1
              IF (OPTGSP%TRASYS.NE.2) THEN
                NUMPAR(ITYP) = NUMPAR(ITYP)+3
                NTRA         = NTRA+3
              ENDIF
              IF (OPTGSP%TRPSYS.NE.2) THEN
                NUMPAR(ITYP) = NUMPAR(ITYP)+1
                NTRP         = NTRP+1
              ENDIF
            ENDIF
C
C COMBINED RECEIVER(S): Galileo
            IF ( (ISYS1.EQ.-1.OR.ISYS1.EQ.2).AND.
     1           (ISYS2.EQ.-1.OR.ISYS2.EQ.2).AND. SYSPAR(2).EQ.0) THEN
              SYSPAR(2) = 1
              IF (OPTGSP%TRASYS.NE.1) THEN
                NUMPAR(ITYP) = NUMPAR(ITYP)+3
                NTRA         = NTRA+3
              ENDIF
              IF (OPTGSP%TRPSYS.NE.1) THEN
                NUMPAR(ITYP) = NUMPAR(ITYP)+1
                NTRP         = NTRP+1
              ENDIF
            ENDIF
C
            IF (SYSPAR(1).EQ.1.AND.SYSPAR(2).EQ.1) GOTO 3010
          ENDIF
        ENDDO
3010  CONTINUE
C
      IF (NTRA.GT.0) NMXSNG = NMXSNG+(NDTOT+1)*3
      IF (NTRP.GT.0) NMXSNG = NMXSNG+(NDTOT+1)
C
3020  CONTINUE

C
C AMBIGUITIES
C -----------
      ITYP=4
C
      NCLS=MAXAMB
      DO 250 IF=1,NFTOT
        IF (MEATYP(IF).NE.1) GOTO 250
        DO 240 IFR=1,NFRFIL(IF)
          ICAR=ICARR(IFR,IF)
          CALL AMBINF(ICAR,NUMAMB(IF),AMBDEF(IF),AMBCLS(1,1,IF),
     1                NCLS,NUMCLS,IR5CLS)
          DO 220 ICLS=1,NCLS
C
            IF((ICAR.EQ.2       .OR. ICAR.EQ.4      ) .AND.
     1         (AMBDEF(IF).EQ.2 .OR. AMBDEF(IF).EQ.3) .AND.
     2         IR5CLS(ICLS).NE.1                      .AND.
     3         NFRFIL(IF).EQ.2)                   GOTO 220
C
C IS THE AMBIGUITY IN THE FILE WINDOW?
            AMBWIN(1)=1D20
            AMBWIN(2)=0D0
            DO IAMB=1,NUMAMB(IF)
              IF((ICAR.NE.2.AND.ICAR.NE.5.AND.
     1            NUMCLS(ICLS).EQ.AMBCLS(IAMB,1,IF))              .OR.
     2          (ICAR.EQ.2.AND.NUMCLS(ICLS).EQ.AMBCLS(IAMB,2,IF)) .OR.
     3          (ICAR.EQ.5.AND.NUMCLS(ICLS).EQ.AMBCLS(IAMB,3,IF))) THEN
C
                AEPOCH=TIMREF(IF)+
     1                 DBLE(IDELTT(IF)*(AMBIEP(IAMB,IF)-1))/86400d0
                IF (AEPOCH.LT.AMBWIN(1)) AMBWIN(1)=AEPOCH
C
                AEPOCH=TIMREF(IF)+
     1                 DBLE(IDELTT(IF)*(NEPOCH(IF)-1))/86400d0
                DO JAMB=IAMB+1,NUMAMB(IF)
                  IF(AMBSAT(IAMB,IF).EQ.AMBSAT(JAMB,IF))THEN
                    AEPOCH=TIMREF(IF)+
     1                     DBLE(IDELTT(IF)*(AMBIEP(JAMB,IF)-2))/86400d0
                    EXIT
                  ENDIF
                ENDDO
                IF (AEPOCH.GT.AMBWIN(2)) AMBWIN(2)=AEPOCH
              ENDIF
            ENDDO
C
            IF((AMBWIN(1).NE.1D20.AND.AMBWIN(1).GT.WINDOW(2,IF)) .OR.
     1         (AMBWIN(2).NE.0D0 .AND.AMBWIN(2).LT.WINDOW(1,IF)))
     2        GOTO 220
C
C SET NEW AMBIGUITY PARAMETER
            NUMPAR(ITYP)=NUMPAR(ITYP)+1
220       CONTINUE
240     CONTINUE
250   CONTINUE
      IF(NUMPAR(ITYP).GT.0) NMXSNG=NMXSNG+(NDTOT+1)
C
C HOW MANY AMBIGUITIES ARE PROCESSED TOGETHER:
C --------------------------------------------
C EPOCH PARAMETERS IN SEQEPO
      IF (IEPPAR.GT.0) THEN
        NMXAMP=NUMPAR(4)
C NO CORRECT CORRELATION, PREELIMINATION EVERY SESSION
      ELSE IF (CORSTR.NE.3.AND.OPTELI(4).EQ.1) THEN
        NMXAMP=MAXAMB
C AS SOON AS POSSIBLE (EVERY EPOCH)
      ELSE IF (STRAMB(1).EQ.-1.AND.STRAMB(2).EQ.NSAMPL(1)) THEN
        NMXAMP=2*MAXFLS*MAXSAS*MAXFRS
C THE GENERAL CASE:
      ELSE
        NMXAMP=NUMPAR(4)
      ENDIF
C
C COMPUTE THE NUMBER OF PARAMETERS
C --------------------------------
      NMXPAR=0
      NMXLOC=0
      DO ITYP=1,MAXTYP
        IF (ITYP.EQ.4) THEN
          NMXPAR=NMXPAR+NMXAMP
        ELSE
          NMXPAR=NMXPAR+NUMPAR(ITYP)
        ENDIF
        NMXLOC=NMXLOC+NUMPAR(ITYP)
      ENDDO
C
C ACOUNT ADDITIONAL MEMORY FOR PREELIMINATION
C -------------------------------------------
      MAXPRE=0
C
C EPOCH-WISE PRE-ELIM FOR EPOCH-PARAMTERS
      IPRE=0
      DO ITYP=17,24
        IF (ITYP.GT.17.AND.ITYP.LT.21) CYCLE
        IF (ITYP.EQ.22) CYCLE
        IF (OPTELI(ITYP).NE.3) CYCLE
        IPRE=IPRE+NUMPAR(ITYP)
      ENDDO
      IF (IPRE.GT.MAXPRE) MAXPRE=IPRE
C
C AMBIGUITY PRE-ELIMINATION EVERY SESSION
      IPRE=0
      IF (OPTELI(4).EQ.1) IPRE=NUMPAR(4)
      IF (IPRE.GT.MAXPRE) MAXPRE=IPRE
C
C PARAMETER PRE-ELIMINATION WITHIN THE SESSION LOOP
      IPRE=0
      DO ITYP=2,6
        IF (ITYP.EQ.4.OR.ITYP.EQ.5) CYCLE
        IF (OPTELI(ITYP).NE.1) CYCLE
        IPRE=IPRE+NUMPAR(ITYP)
      ENDDO
      IF (IPRE.GT.MAXPRE) MAXPRE=IPRE
C
C PARAMETER PRE-ELIMINATION OUTSIDE THE SESSION LOOP
C (PARAM. W/O OBSERVATIONS ARE ELIMINIATED TOO)
      IPRE=0
      DO ITYP=1,MAXTYP
        IF (OPTELI(ITYP).EQ.0.AND. (ITYP.EQ. 4 .OR. ITYP.EQ.21 .OR.
     1      ITYP.EQ.23 .OR. ITYP.EQ.24)) IPRE=IPRE+NUMPAR(ITYP)
        IF (ITYP.GE.2.AND.ITYP.LE.4) CYCLE
        IF (ITYP.EQ.6) CYCLE
        IF (OPTELI(ITYP).EQ.1) CYCLE
        IPRE=IPRE+NUMPAR(ITYP)
      ENDDO
      IF (IPRE.GT.MAXPRE) MAXPRE=IPRE
C
C REFERENCE AMBIGUITIES
      IPRE=0
      IF (OPTELI(4).NE.1.AND.NDTOT.EQ.1) IPRE=2*NUMPAR(4)
      IF (IPRE.GT.MAXPRE) MAXPRE=IPRE
C
C PARAMETER PRE-ELIMINATION BEFORE NEQ SAVING
      IPRE=0
      DO ITYP=1,MAXTYP
        IF (OPTELI(ITYP).NE.2) CYCLE
        IPRE=IPRE+NUMPAR(ITYP)
      ENDDO
      IF (IPRE.GT.MAXPRE) MAXPRE=IPRE
C
C UPDATE THE ARRAY SIZES:
      NMXLOC=NMXLOC+MAXPRE
C
C CONSIDER REFERENCE AMBIGUITIES FOR DD AND GPS ONLY
      IF (NDTOT.EQ.1.AND.MIXED.EQ.0.AND.STRAMB(1).NE.-1)
     1  NMXAMP=NMXAMP+INT(DBLE(NUMPAR(4))/2d0)+1
C
C ADD TERMINATING (ZERO-)ELEMENT
      NMXSNG=NMXSNG+1
      NEPSNG=NEPSNG+1
C
      RETURN
      END SUBROUTINE

      END MODULE
