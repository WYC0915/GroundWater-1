      MODULE s_SEQPAR
      CONTAINS

C*
      SUBROUTINE SEQPAR(MAXTYP ,NFTOT  ,STFIL  ,NSTAT  ,ICENTR ,NFIX   ,
     1                  STFIX  ,NKIN   ,STKIN  ,NCLREQ ,ISTCLK ,ISACLK ,
     2                  NCLK   ,IBIAS  ,CLFRTO ,TIMREF ,NARC   ,NORB   ,
     3                  NSATEL ,SATNUM ,NUMSAT ,NAVNUM ,SEQORB ,ARCINT ,
     4                  NFRFIL ,ICARR  ,MEATYP ,MELWUB ,NIOREQ ,IONREQ ,
     5                  IONMOD ,OPTDCB ,POLMOD ,POLPAR ,NPOL   ,NTRREQ ,
     6                  NPARTR ,NTRSTA ,STATRP ,ITRGRD ,NUMAMB ,AMBCLS ,
     6                  AMBSAT ,AMBIEP ,AMBDEF ,NSASTC ,NUMSTC ,NSTCEF ,
     7                  NSTCEP ,FRCTYP ,NRQOFF ,PAROFF ,GRPOFF ,NHILL  ,
     8                  HILTYP ,NPOT   ,POTTYP ,NALBGR ,NALB   ,ALBTYP ,
     9                  NCENM  ,CENMAS ,NEPOCH ,WINDOW ,NSAMPL ,IDELTT ,
     1                  DTSIM  ,NANCAL ,NFRCAL ,NPTCAL ,STRAMB ,OPTDIP ,
     2                  OPTHOI ,OPTELI ,OPTGIM ,POLGIM ,NAMGIM ,EPOGIM ,
     3                  NANRAO ,NFRRAO ,NEURAO ,NPAR   ,NAMB   ,NPARN  ,
     4                  NPARMS ,LOCQ   ,PARLST ,PARTYP ,NCLKST ,CLKSTA ,
     5                  NCLKSA ,CLKSAT ,CLKSYS ,CLKHED ,CLKREC ,NDIFF  ,
     6                  STNAME ,IOREST ,ISASYS ,NESTSAT,ESTSAT ,NARC2  ,
     7                  ARCINT2,NUMSAT2,NAVNUM2,NSASTC2,NSTCEF2,NUMSTC2,
     8                  NSTCEP2,FRCTYP2,NORB2  ,SEQORB2,NANSPV ,GNRSPV ,
     9                  NPTSPV ,NSASPV ,GNROFF ,OPLOAD ,MAXSAT ,MAXAMB ,
     .                  NRGB   ,STARGB ,SATSPEC,NREC   ,ISBTIM ,RECTYP ,
     2                  IRUNIT ,globalWindow   ,timisb ,OPTGSP ,USEGEOS,
     3                  GOBSDEF)


CC
CC NAME       :  SEQPAR
CC
CC PURPOSE    :  DEFINE THE SEQUENCE OF PARAMETERS TO BE ESTIMATED
CC
CC PARAMETERS :
CC         IN :  MAXTYP : MAX. NUMBER OF PARAMETER TYPES      I*4
CC               NFTOT  : TOTAL NUMBER OF FILES               I*4
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
CC               ISACLK(I),I=1,2,..,NCLREQ: SATELLITE NUMBER  I*4
CC                        FOR REQUEST I (ONLY SLR TIME BIASES)
CC               NCLK(I),I=1,2,..,NCLREQ: NUMBER OF CLOCK     I*4
CC                        PARAMETER FOR REQUEST I
CC               IBIAS(I),I=1,2,..,NCLREQ: TYPE OF CLOCK BIAS I*4
CC                        0: STATION SPECIFIC
CC                        1: FREQUENCY SPECIFIC
CC                        2: SATELLITE SPECIFIC
CC                        3: SAT. SPECIFIC FOR NON-GPS
CC                        4: FREQUENCY SPECIFIC WITH POLYNOM
CC               CLFRTO(K,I),K=1,2: TIME LIMITS FOR REQUEST I R*8
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
CC               SEQORB(I),I=1,2,..,NORB : SEQUENCE OF ORBIT  I*4
CC                        PARAMETERS
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
CC                        (2): ESTIMATE RECEIVER BIASES
CC                             = 0: NO
CC                             = 1: P1-P2
CC                             = 2: P1-C1
CC                             = 3: LC (ISBs)
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
CC               FRCTYP : CORRESPONDING FORCE TYPES           I*4(*)
CC               NRQOFF : NUMBER OF SATELLITE ANTENNA OFFSET  I*4
CC                        REQUESTS
CC               PAROFF(I),I=1,2,3: SATELLITE ANTENNA OFFSET  I*4
CC                        COMPONENTS TO BE ESTIMATED (X,Y,Z
CC                        IN SATELLITE REFERENCE FRAME
CC                        =1: TO BE ESTIMATED
CC                        =0: NOT TO BE ESTIMATED
CC               GRPOFF(I),I=1,..,NRQOFF : SAT. ANT. OFFSET   I*4
CC                        GROUP FOR REQUEST I
CC               NHILL  : NUMBER OF HILL PARAMETERS           I*4
CC               HILTYP : CHARACT. OF HILL PARMS              I*4(3,*)
CC               NPOT   : NO OF PARMS OF EARTH'S POT          I*4
CC               POTTYP : PARM CHARACTERIZATION (EARTH POT)   I*4(3,*)
CC               NALBGR : NUMBER OF ALBEDO GROUPS             I*4
CC               NALB   : NUMBER OF ALBEDO PARAMETERS/GROUP   I*4
CC               ALBTYP : PARAMETER TYPE (1, 2, OR 3)         I*4(*)
CC               NCENM  : NUMBER OF CENTER OF MASS PARAMETER  I*4
CC                        (1, 2, OR 3)
CC               CENMAS : CORRESP. COORDINATE NUMBERS         I*4(*)
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
CC               ISASYS : SATELLITE SYSTEM TO BE CONSIDERED   I*4
CC                        =0: ALL
CC                        =1: GPS
CC                        =2: GLONASS
CC                        =3: GALILEO
CC                        =4: GPS+GLONASS
CC                        =5: GPS+Galileo
CC                        =6: GLONASS+Galileo
CC                        ADD_GNSS_HERE  conflict for SBAS,COMPASS,QZSS
CC                        (to be solved together with IRXVRS issue)
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
CC               FRCTYP2: CORRESPONDING FORCE TYPES (LEO)     I*4(*)
CC               NORB2  : NUMBER OF ORBIT PARAMETERS PER      I*4
CC                        LEO SATELLITE ARC
CC               SEQORB2(I),I=1,2,..,NORB2 : SEQUENCE OF      I*4
CC                        LEO ORBIT PARAMETERS
CC               NANSPV : NUMBER OF SATELL. ANTENNA PHASE     I*4
CC                        CENTER GROUPS
CC               GNRSPV(I),I=1,..,NANSPV: USER-DEFINED NUMBER I*4
CC                        FOR SATELLITE ANTENNA PHASE CENTER
CC                        GROUP I
CC               NPTSPV(J,I),J=1,2, I=1,..,NANSPV: NUMBER OF  I*4
CC                        POINTS TO BE ESTIMATED IN ELEVATION
CC                        (J=1) AND AZIMUTH (J=2) DIRECTION
CC               NSASPV(I),I=1,..,NANSPV: NUMBER OF           I*4
CC                        SATELLITES BELONGING TO SAT. ANT.
CC                        PHASE CENTER GROUP I
CC               GNROFF(I),I=1,..,NANOFF: USER-DEFINED NUMBER I*4
CC                        FOR SATELLITE ANTENNA OFFSET GROUP I
CC               OPLOAD : SCALING FACTORS FOR VIENNA GRID FILES   T_OPTLOAD(3)
CC                        1: ATMOSPHERIC NON-TIDAL LOADING
CC                        2: OCEAN NON-TIDAL LOADING
CC                        3: HYDROSTATIC PRESSURE LOADING
CC               NRGB   : NUMBER OF SLR RANGE BIAS REQUESTS   I*4
CC               STARGB(I), I=1,...NRGB: Station numbers for  I*4
CC                        Range Bias requests
CC               SATSPEC: Type of RGB set-up wrt. satellites  I*4
CC               OPTGSP : GNSS-SPECIFIC PARAMETER OPTIONS     T_OPTGSP
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
CC        OUT :  NPAR   : TOTAL NUMBER OF PARAMETERS          I*4
CC               NAMB   : TOTAL NUMBER OF AMBIGUITY PARAM.    I*4
CC               NPARN  : NPARN=NPAR-NAMB                     I*4
CC               NPARMS : NUMBER OF PARAMETERS TO COMPUTE RMS I*4
CC               LOCQ(I,K),K=1,...,NPAR,I=1,..,MAXLCQ:        I*4
CC                        DEFINITION OF EACH PARAMETER
CC               PARTYP: PARAMETER DESCRIPTION                t_partyp(*)
CC               PARLST(I,K), I=1,..,5,K=1,..,MAXTYP: NUMBER  I*4
CC                        OF PARAMETERS:
CC                        I=1: #PARAMETERS OF TYPE I (NPARMS)
CC                        I=2: #SET-UP
CC                        I=3: #NO-OBS
CC                        I=4: #REF. PARAMETERS
CC                        I=5: #SINGULAR
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W.GURTNER, G.BEUTLER, M.ROTHACHER, S.FANKHAUSER
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
CC               19-NOV-04 : RD: ALLOCATE SOME ARRAYS ON START-UP
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               04-OCT-06 : AG: SAT. SPECIFIC SETUP FOR SATELLITE ANTENNA
CC                               PCO/PCV
CC               16-OCT-06 : RD: MANUAL SELECTION OF SAT. FOR ORBIT DETERM.
CC               03-APR-07 : AG: USE SVNSYS INSTEAD OF MIXSVN
CC               22-APR-08 : AS: CONSIDER ISASYS FOR DCB-PAR. SETUP
CC               24-JUN-08 : DT: ASSUMING SVN>=951 FOR SLR
CC               02-APR-09 : DT: ADD RANGE BIASES
CC               05-MAY-09 : RD: SCALING OF LOADING MODELS ADDED
CC               09-MAY-09 : RD: SAT/FRQ-SPECIFIC RECEIVER CLOCK BIASES
CC               09-MAY-09 : RD: SEPERATE RECEIVER CLOCKS FOR GPS/GLONASS
CC               31-JAN-03 : RD: SPECIAL SAMPLING FOR EPOCH PARAM.
CC               04-JAN-10 : SL: HOI SCALING PARAMETERS (ITYP 27) ADDED
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC               16-Nov-10 : RD: DISTINGUISH BETWEEN PIECE-WISE LINEAR PARAM.
CC               25-NOV-10 : MM: GNSS-SPECIFIC PARAMETERS
CC               16-JAN-11 : RD: REMOVE STANUM (STARGB GOES THROUGH STEXIN)
CC               26-JAN-11 : LP: CHANGED DEFREQ CALL
CC               17-FEB-11 : SS: STRAMB(3) FOR SELECTION OF GNSS
CC               07-JUN-11 : LP: ADD GALILEO CHAR 'E' AT SOME POINTS
CC               15-SEP-11 : LP: BUGFIX FOR SETTING UP REC BIASES
CC               28-MAR-12 : RD: USE SVN2CHR AS MODULE NOW
CC               27-APR-12 : RD: NULLIFY ALL POINTERS
CC               02-MAY-12 : LP: Sat.-specific obstypes in DEFREQ call
CC               06-JUN-12 : LP: USE F_SVNSYS AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, lfnerr
      USE d_clkrnx, ONLY: t_clkhead,t_clkrec
      USE m_maxdim, ONLY: MAXGIM
      USE m_time,   ONLY: t_timint
      USE d_stacrx, ONLY: MTypeSPACE
      USE d_par,    ONLY: parType_constant, parType_linear,
     1                    parType_epochSpecific
      USE d_rinex3, ONLY: t_gobsdef
      USE p_gpsest, ONLY: t_optLoad,t_isbTime,t_partyp,t_optGsp
      USE s_iordup
      USE s_defreq
      USE f_mixsvn
      USE s_dimtst
      USE s_mjdgps
      USE f_modsvn
      USE s_seqgim
      USE s_staflg
      USE s_exitrc
      USE s_ambinf
      USE s_inlist
      USE s_wildcd
      USE s_rdpwin
      USE s_parint
      USE s_getRcv
      USE s_svn2chr
      USE f_svnsys
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I      , IA     , IAMB   , IARC   , IAZI   , ICAL   ,
     1          ICAR   , ICLK   , ICLS   , ICOM   , ICOR   , ICR    ,
     2          IDEG   , IEPO   , IF     , IFIL   , IFIX   , IFLAG  ,
     3          IFR    , IFRQ   , IGR    , IHOUR  , IKIN   , ILAT   ,
     4          ILIM   , IMIX   , IMOD   , INTEPO , IOFR   , IORD   ,
     5          IOREST , IPA    , IPAR   , IPE    , IPOL   , IPOS   ,
     6          IRAO   , IRC    , IREC   , IREF   , IREQ   , IS     ,
     7          ISAT   , ISAT0  , ISATEL , ISPV   , ISTAT  , ISTC   ,
     8          ISTREQ , ISTTRP , ITYP   , ITYPE  , IZEN   , J      ,
     9          JAMB   , K      , KPAR   , MAXAMB , MAXEPO , MAXLOC ,
     1          MAXSAT , MAXTYP , MELWUB , MXCAMB , ISTA   , JSTA   ,
     2          MXCFRQ , MXCLCQ , MXCLOC , MXCSAT , NALB   , NALBGR ,
     3          NAMB   , NANCAL , NANRAO , NARC   , NARC2  , NCENM  ,
     4          NCLKSA , NCLKST , INUM   , NCLREQ , NCLS   , NCOM   ,
     5          NDEG   , NFIX   , NFR1   , NFR2   , NFTOT  , NHILL  ,
     6          NIOREQ , NKIN   , NMOD   , NORB   , NORB2  , NORBT  ,
     7          NORD   , NPAR   , NPARMS , NPARN  , NPOL   , NPOT   ,
     8          NRQOFF , NSAEPO , NSARC  , NSASTC , NSASTC2, JSAT   ,
     9          NSAT   , NSAT0  , NSTAT  , NSTCEP , NSTCEP2, NTRREQ ,
     1          NTRSTA , NWEEK  , IP     , JP     , IPAR0  , ISASYS ,
     2          MIXRCO , CLKSYS , ICLOCK , II     , NESTSAT, IEST   ,
     3          ISVN   , NISB   , NUMISB , IHOI   , NREC   , NRGB   ,
     4          SATSPEC, ISYS   , IRGB   , IHLP   , NFREQ  ,
     5          ICLASS , ISYS1  , ISYS2  , IE
C
      REAL*8    AEPOCH , DTIME  , DTSIM  , GPSSEC , TFRAC  , TOBS   ,
     1          DSEC   , TCLK   , T_0    , DT_0
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL LOCAL DIMENSIONS
C ------------------------
      PARAMETER (MAXEPO=3000)
C
C
      TYPE(t_clkhead) CLKHED
      TYPE(t_clkrec)  CLKREC
      TYPE(t_optLoad), DIMENSION(:):: opLoad
      TYPE(t_isbTime), DIMENSION(:), POINTER:: isbTim
      TYPE(t_timint)   :: globalWindow
      TYPE(t_timint),  DIMENSION(:), POINTER:: iSbWin
      TYPE(t_parTyp),  DIMENSION(:) :: parTyp
      TYPE(t_optGsp)   OPTGSP
C
      REAL*8       CLFRTO(2,*),TIMREF(*),WINDOW(2,*),SATTIM(MAXEPO)
      REAL*8       POLGIM(3,*),EPOGIM(2,*),AMBWIN(2),TIMISB(3,*)
C
      INTEGER*4    LOCQ(MXCLCQ,*)
      INTEGER*4    STFIL(*),STFIX(*),STKIN(*),ICENTR(*),SEQORB(*)
      INTEGER*4    ARCINT(*),SEQORB2(*)
      INTEGER*4    ARCINT2(*)
      INTEGER*4    ISTCLK(*),ISACLK(*),NCLK(*),IBIAS(*),NFRFIL(*)
      INTEGER*4    ICARR(MXCFRQ,*)
      INTEGER*4    STATRP(*)
      INTEGER*4    MEATYP(*),IONREQ(3,*),IONMOD(*),NPARTR(*)
      INTEGER*4    NSATEL(*),SATNUM(MXCSAT,*),NSHLP(MAXSAT)
      INTEGER*4    NUMAMB(*),AMBCLS(MXCAMB,3,*),AMBSAT(MXCAMB,*)
      INTEGER*4    AMBIEP(MXCAMB,*),AMBDEF(*)
      INTEGER*4    POLMOD,POLPAR(*),NUMSAT(*),NAVNUM(*)
      INTEGER*4    NUMSAT2(*),NAVNUM2(*)
      INTEGER*4    NUMSTC(*),NSTCEF(MXCSAT,*),FRCTYP(*),PAROFF(*)
      INTEGER*4    NUMSTC2(*),NSTCEF2(MXCSAT,*),FRCTYP2(*)
      INTEGER*4    NUMCLS(MAXAMB),IR5CLS(MAXAMB),GRPOFF(*)
      INTEGER*4    GNROFF(*)
      INTEGER*4    HILTYP(3,*),POTTYP(3,*),ALBTYP(*),CENMAS(*)
      INTEGER*4    NEPOCH(*),IDELTT(*),PARLST(5,*)
      INTEGER*4    NFRCAL(*),NPTCAL(2,*),OPTGIM(*)
      INTEGER*4    STRAMB(*),OPTDIP(*),OPTELI(*),OPTHOI(*)
      INTEGER*4    NEURAO(3),NFRRAO(*),OPTDCB(*)
      INTEGER*4    CLKSTA(*),CLKSAT(*),NDIFF(*),SATEPO(MAXEPO)
      INTEGER*4    SVNORB,SVNOBS,ITRGRD(*),NSAMPL(3)
      INTEGER*4    SATLIS(MAXSAT),SATIND(MAXSAT)
      INTEGER*4    NANSPV,GNRSPV(*),NPTSPV(2,*),NSASPV(*)
      INTEGER*4    ESTSAT(*),IRUNIT(2,*),STAISB(NSTAT)
      INTEGER*4    STARGB(*)
      INTEGER*4    ICODE(2),IWLFAC(2)
      INTEGER*4    SYSPAR(2),USEGEOS
C
      CHARACTER*16 NAMGIM(*),CLKNAM,STNAME(*)
      CHARACTER*6  MXNSAT,MXNFRQ,MXNAMB,MXNLCQ,MXNLOC
      CHARACTER*20 MARTYP,RCVTYP,RECTYP(2,*)
      CHARACTER*1  SVNCHR
C
      LOGICAL      SORTED
      LOGICAL      IsSYS(10)
C
      TYPE(t_gobsdef) :: GOBSDEF ! Giove External Obs. Selection info
C
C
      INCLUDE 'COMFREQ.inc'
C
      LOGICAL      RGBLST(NRGB,MAXSAT),ISRANG
C
C COMMON FOR MAXIMAL DIMENSIONS
C -----------------------------
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMLOC/MXCLOC,MXNLOC
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(ISBWIN)
C
      MAXLOC=MXCLOC
C
C CHECK MAXIMUM LOCAL DIMENSIONS
C ------------------------------
c      CALL MAXTST(1,'SEQPAR',MXNSAT,MAXSAT,MXCSAT,IRC1)
c      CALL MAXTST(1,'SEQPAR',MXNAMB,MAXAMB,MXCAMB,IRC2)
c      IF(IRC1+IRC2.NE.0) CALL EXITRC(2)
C
      IPAR=0
C
C DEFINITION OF STATION COORDINATES
C ---------------------------------
      ITYP=1
C
      IF (ICARR(1,1).EQ.4 .OR. MELWUB.GT.0) GOTO 70
C
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
40      DO 50 K=1,3
          IPAR=IPAR+1
          IF (IPAR.GT.MAXLOC) GOTO 990
          LOCQ(1,IPAR)=ITYP
          LOCQ(2,IPAR)=ISTAT
          LOCQ(3,IPAR)=K
          LOCQ(4,IPAR)=0
          LOCQ(5,IPAR)=0
          PARTYP(IPAR)%TYPE =parType_constant
50      CONTINUE
60    CONTINUE
C
70    CONTINUE
C
C STATION CLOCK PARAMETERS
C ------------------------
      ITYP=2
      DO 140 ICR=1,NCLREQ
C
C CHECK VALIDITY OF REQUEST
        MIXRCO=-1
        ISRANG=.FALSE.
        DO 100 IF=1,NFTOT
          IF(TIMREF(IF)+DBLE(NEPOCH(IF)*IDELTT(IF))/86400D0.LT.
     1       CLFRTO(1,ICR))GO TO 100
          IF(TIMREF(IF).GT.CLFRTO(2,ICR))GO TO 100
          IF(ISTCLK(ICR).NE.STFIL(2*IF-1).AND.
     1       ISTCLK(ICR).NE.STFIL(2*IF-1+NDIFF(IF)))
     2      GO TO 100
          ISRANG=ISRANG.OR.(MEATYP(IF).EQ.3)
          IF (.NOT. ISRANG) THEN
C
C IS IT A REAL GNSS REQUEST
            IF (MIXRCO.EQ.-1) THEN
              IF (ISASYS.EQ.0.OR.ISASYS.GT.3) THEN
                MIXRCO=MIXSVN(NSATEL(IF),SATNUM(1,IF))
              ELSE IF (ISASYS.EQ.1) THEN
                MIXRCO=0
              ELSE IF (ISASYS.EQ.2) THEN
                MIXRCO=2
              ENDIF
            ELSE IF (MIXRCO.NE.1.AND.(ISASYS.EQ.0.OR.ISASYS.GT.3)) THEN
              IF (MIXRCO.NE.MIXSVN(NSATEL(IF),SATNUM(1,IF))) MIXRCO=1
            ENDIF
          ENDIF
100     CONTINUE
C
C INVALID REQUEST
        IF (MIXRCO.EQ.-1) THEN
          WRITE(LFNERR,110) ICR
110       FORMAT(/,' *** SR SEQPAR: INVALID CLOCK REQUEST',/,
     1                         16X,'REQUEST NUMBER:',I3,/)
          CALL EXITRC(2)
        ENDIF
C
C RECEIVER CLOCK ESTIMATION PER SATELLITE SYSTEM
        IF(CLKSYS.EQ.1.AND.MIXRCO.NE.0) MIXRCO=2
C
C STATION SPECIFIC RECEIVER CLOCK OFFSET
C --------------------------------------
        IF (IBIAS(ICR).EQ.0) THEN
          DO I=1,NCLK(ICR)
            IPAR=IPAR+1
            IF (IPAR.GT.MAXLOC) GOTO 990
            LOCQ(1,IPAR)=ITYP
            LOCQ(2,IPAR)=ISTCLK(ICR)
            LOCQ(3,IPAR)=ICR
            LOCQ(4,IPAR)=I
            LOCQ(5,IPAR)=NCLK(ICR)
            LOCQ(6,IPAR)=0
            IF (ISRANG) LOCQ(7,IPAR)=ISACLK(ICR)
            PARTYP(IPAR)%TYPE =parType_constant
          ENDDO
C
C SATELLITE OR FREQUENCY SPECIFIC (not yet GALILEO-ready due to MIXSVN)
C -------------------------------
C         ADD_GNSS_HERE
c
        ELSE
C
C DOES THE FILE CONTRIBUTE?
          IPAR0 = IPAR+1
          DO IFIL=1,NFTOT
            IF(MEATYP(IFIL).NE.2) CYCLE
            IF(TIMREF(IFIL)+DBLE(NEPOCH(IFIL)*IDELTT(IFIL))/86400D0.LT.
     1         CLFRTO(1,ICR)) CYCLE
            IF(TIMREF(IFIL).GT.CLFRTO(2,ICR)) CYCLE
            IF(ISTCLK(ICR).NE.STFIL(2*IFIL-1).AND.
     1         ISTCLK(ICR).NE.STFIL(2*IFIL-1+NDIFF(IFIL))) CYCLE
C
C IS THE STATION ALREADY IN THE LIST
            IP=0
            DO JP=IPAR0,IPAR
              IF (LOCQ(2,JP).EQ.ISTCLK(ICR)) THEN
                IP=JP
                EXIT
              ENDIF
            ENDDO
            IF (IP.NE.0) CYCLE
C
C GET THE GLONASS FREQUENCY NUMBERS
            IF (IBIAS(ICR).EQ.1.OR.IBIAS(ICR).EQ.4) THEN
c              CALL DEFREQ(CLFRTO(1:2,ICR),NSATEL(IFIL),SATNUM(:,IFIL))
              CALL DEFREQ(CLFRTO(1:2,ICR),NSATEL(IFIL),SATNUM(:,IFIL),
     1                    USEGEOS=USEGEOS,GOBSDEF=GOBSDEF,MEATYPC='U')
            ENDIF
C
            DO ISAT=1,NSATEL(IFIL)
C
C CONSIDER THE SELECTED SATELLITE SYSTEM
              CALL SVN2CHR(SATNUM(ISAT,IFIL),INUM,SVNCHR)
              IF (ISASYS.EQ.1 .AND. SVNCHR.NE.'G') CYCLE
              IF (ISASYS.EQ.2 .AND. SVNCHR.NE.'R') CYCLE
              IF (ISASYS.EQ.3 .AND. SVNCHR.NE.'E') CYCLE
c              IF (ISASYS.EQ.4 .AND. SVNCHR.NE.'S') CYCLE
c              IF (ISASYS.EQ.5 .AND. SVNCHR.NE.'C') CYCLE
c              IF (ISASYS.EQ.6 .AND. SVNCHR.NE.'J') CYCLE
C
C             ADD_GNSS_HERE conflict for SBAS,COMPASS,QZSS
C
C GPS ONLY FOR "ALL SATELLITES"
              IF (IBIAS(ICR).NE.2.AND.SVNCHR.NE.'R') CYCLE
C
C IS THE FREQUENCY ALREADY IN THE LIST
              IP=0
              IF (IBIAS(ICR).EQ.1) THEN
                DO JP=IPAR0,IPAR
                  IF (LOCQ(4,JP).EQ.FREQNM(SATNUM(ISAT,IFIL))) THEN
                    IP=JP
                    EXIT
                  ENDIF
                ENDDO
                IF (IP.NE.0) CYCLE
              ENDIF
C
C IS THE SATELLITE ALREADY IN THE LIST
              IF (IBIAS(ICR).EQ.2.OR.IBIAS(ICR).EQ.3) THEN
                DO JP=IPAR0,IPAR
                  IF (LOCQ(4,JP).EQ.SATNUM(ISAT,IFIL)) THEN
                    IP=JP
                    EXIT
                  ENDIF
                ENDDO
                IF (IP.NE.0) CYCLE
              ENDIF
C
C ADD A NEW SATELLITE/FREQUENCY
              DO I=1,NCLK(ICR)
                IPAR=IPAR+1
                IF (IPAR.GT.MAXLOC) GOTO 990
                LOCQ(1,IPAR)=ITYP
                LOCQ(2,IPAR)=ISTCLK(ICR)
                LOCQ(3,IPAR)=ICR
                IF (IBIAS(ICR).EQ.1) THEN
                  LOCQ(4,IPAR)=FREQNM(SATNUM(ISAT,IFIL))
                ELSE IF (IBIAS(ICR).LE.3) THEN
                  LOCQ(4,IPAR)=SATNUM(ISAT,IFIL)
                ELSE IF (IBIAS(ICR).EQ.4) THEN
                  LOCQ(4,IPAR)=I
                ENDIF
                LOCQ(5,IPAR)=NCLK(ICR)
                LOCQ(6,IPAR)=IBIAS(ICR)
                IF (LOCQ(6,IPAR).EQ.3) LOCQ(6,IPAR)=2
                LOCQ(7,IPAR)=MIXRCO
                PARTYP(IPAR)%TYPE =parType_constant
              ENDDO
              IF (IBIAS(ICR).EQ.4) EXIT
            ENDDO
C
C SORT THE REQUESTS
            SORTED = .FALSE.
            DO WHILE(.NOT. SORTED)
              SORTED = .TRUE.
              DO IP=IPAR0,IPAR-1
                IF (LOCQ(4,IP).GT.LOCQ(4,IP+1)) THEN
                  SORTED=.FALSE.
                  ISAT=LOCQ(4,IP)
                  LOCQ(4,IP)=LOCQ(4,IP+1)
                  LOCQ(4,IP+1)=ISAT
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDIF
140   CONTINUE
C
C     GET THE PARAMETER TIME WINDOW DEFINITION
      CALL rdpwin('PAR_OFF',(/' ',' '/),t_0,dt_0)
      STAISB(:)=0

      NUMISB=0
      DO IFIL=1,NFTOT
        IF (ISASYS.EQ.1.OR.ISASYS.EQ.2.OR.ISASYS.EQ.3) EXIT
C
C             ADD_GNSS_HERE  conflict for SBAS,COMPASS,QZSS
C
        IF (MIXSVN(NSATEL(IFIL),SATNUM(1,IFIL)) .NE. 1) CYCLE
        DO II = 1,NDIFF(IFIL)+1
          IF (STAISB(STFIL(2*(IFIL-1)+II)).EQ.0) THEN
            STAISB(STFIL(2*(IFIL-1)+II)) = HUGE(i4b)
          ENDIF
        ENDDO
        DO IREC=1,NREC
          IF (LEN_TRIM(isbTim(IREC)%recnam) == 0) CYCLE
          DO II = 1,NDIFF(IFIL)+1
            JP = 0
            DO IP=IPAR,1,-1
              IF (LOCQ(1,IP)==ITYP .AND. LOCQ(6,IP)==5 .AND.
     1            LOCQ(2,IP)==STFIL(2*(IFIL-1)+II)) THEN
                JP = IP
                EXIT
              ENDIF
              IF (LOCQ(1,IP)/=ITYP .AND. LOCQ(6,IP)/=5) EXIT
            ENDDO
            IF (JP /= 0) CYCLE
C
            CALL WILDCD(isbTim(IREC)%recnam,RECTYP(II,IFIL),IRC)
            IF (IRC.EQ.1.AND.
     1          IRUNIT(II,IFIL).GE.isbTim(iRec)%recnum(1) .AND.
     2          IRUNIT(II,IFIL).LE.isbTim(iRec)%recnum(2)) THEN
              CALL parint(globalWindow,dtSim,t_0,dt_0,
     1             isbTim(IREC)%isbint,
     2             'Time-dependent interfrequency biases',
     3             nISb,iSbWin)
              DO I=1,NISB+1
                IPAR=IPAR+1
                IF (IPAR.GT.MAXLOC) GOTO 990
                NUMISB = NUMISB+1
                IF (I.GT.NISB) THEN
                  TIMISB(1,NUMISB) = isbWin(NISB)%t(2)
                  TIMISB(2,NUMISB) = isbWin(NISB)%t(1)
                  TIMISB(3,NUMISB) = isbWin(NISB)%t(2)
                ELSEIF (I==1) THEN
                  TIMISB(1,NUMISB) = isbWin(I)%t(1)
                  TIMISB(2,NUMISB) = isbWin(I)%t(1)
                  TIMISB(3,NUMISB) = isbWin(I)%t(2)
                ELSE
                  TIMISB(1,NUMISB) = isbWin(I)%t(1)
                  TIMISB(2,NUMISB) = isbWin(I-1)%t(1)
                  TIMISB(3,NUMISB) = isbWin(I)%t(2)
                ENDIF
                LOCQ(1,IPAR)=ITYP
                LOCQ(2,IPAR)=STFIL(2*(IFIL-1)+II)
                LOCQ(3,IPAR)=ICARR(1,IFIL)
                LOCQ(4,IPAR)=NUMISB
                IF (I == 1) THEN
                  LOCQ(5,IPAR)=NINT(DABS(TIMISB(3,NUMISB)-
     1                         TIMISB(1,NUMISB))*86400d0)
                ELSEIF (I > NISB) THEN
                  LOCQ(5,IPAR)=NINT(DABS(TIMISB(2,NUMISB)-
     1                         TIMISB(1,NUMISB))*86400d0)
                ELSE
                  LOCQ(5,IPAR)=MIN0(NINT(DABS(TIMISB(3,NUMISB)-
     1                         TIMISB(1,NUMISB))*86400d0),
     2                         NINT(DABS(TIMISB(2,NUMISB)-
     3                         TIMISB(1,NUMISB))*86400d0))
                ENDIF
                LOCQ(6,IPAR)=5
                LOCQ(7,IPAR)=1
                PARTYP(IPAR)%TYPE =parType_linear
              ENDDO
              DEALLOCATE(isbWin,stat=irc)
              STAISB(STFIL(2*(IFIL-1)+II))=
     1        MAX0(NINT(DABS(TIMISB(2,NUMISB)-
     2             TIMISB(1,NUMISB))*86400d0),
     1             NINT(DABS(TIMISB(3,NUMISB)-
     2             TIMISB(1,NUMISB))*86400d0))
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      DO IP=IPAR,1,-1
        IF (LOCQ(1,IP).NE.ITYP.OR.LOCQ(6,IP).NE.5) EXIT
        LOCQ(7,IP) = MAXVAL(STAISB(1:NSTAT))
      ENDDO
C
C ORBIT PARAMETERS
C ----------------
      IF (IOREST.EQ.3) GOTO 200
      ITYP=3
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
C
C DEFINE LOCQ
        DO 180 IS=1,NSARC
          DO 170 K=1,NORB
            IPAR=IPAR+1
            IF (IPAR.GT.MAXLOC) GOTO 990
            NORBT=NORBT+1
            LOCQ(1,IPAR)=ITYP
            LOCQ(2,IPAR)=IA
            LOCQ(3,IPAR)=NSHLP(IS)
            LOCQ(4,IPAR)=SEQORB(K)
            LOCQ(5,IPAR)=NORB
            LOCQ(6,IPAR)=K
            PARTYP(IPAR)%TYPE=parType_constant
170       CONTINUE
C
C ADD STOCHASTIC ORBIT PARAMETERS
          DO 171 ISTC=1,NSASTC
            IF(NSHLP(IS).EQ.NUMSTC(ISTC))GO TO 172
171       CONTINUE
          GO TO 180
172       CONTINUE
C
C SATELLITE FOUND IN LIST NUMSTC
          DO 179 IPA=1,NSTCEF(ISTC,IA)
            DO 177 IPE=1,NSTCEP
              IF (IPAR.GT.MAXLOC) GOTO 990
              IPAR=IPAR+1
              LOCQ(1,IPAR)=11
              LOCQ(2,IPAR)=IA
              LOCQ(3,IPAR)=NSHLP(IS)
              LOCQ(4,IPAR)=IPA
              LOCQ(5,IPAR)=FRCTYP(IPE)
              LOCQ(6,IPAR)=IPE
              LOCQ(7,IPAR)=ISTC
              PARTYP(IPAR)%TYPE=parType_epochSpecific
177         CONTINUE
179       CONTINUE
180     CONTINUE
190   CONTINUE
C
C ORBIT PARAMETERS FOR LEO
C ------------------------
200   IF (IOREST.EQ.3.OR.IOREST.EQ.0) THEN
        ITYP=3
        NORBT=0
        NSAT0=0
        DO IA=1,NARC2
          NSARC=0
          DO 265 ISAT0=1,NUMSAT2(IA)
C
            SVNORB=NAVNUM2(NSAT0+ISAT0)
            SVNOBS=MODSVN(NAVNUM2(NSAT0+ISAT0))
C
C ORBIT ESTIMATION FOR LEOs
            IF (SVNOBS.LT.900 .OR. SVNOBS.GE.951) GOTO 265
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
            NSARC=NSARC+1
            NSHLP(NSARC)=SVNORB
265       CONTINUE
          NSAT0=NSAT0+NUMSAT2(IA)
C
C DEFINE LOCQ
          DO 280 IS=1,NSARC
            DO K=1,NORB2
              IPAR=IPAR+1
              IF (IPAR.GT.MAXLOC) GOTO 990
              NORBT=NORBT+1
              LOCQ(1,IPAR)=ITYP
              LOCQ(2,IPAR)=IA
              LOCQ(3,IPAR)=NSHLP(IS)
              LOCQ(4,IPAR)=SEQORB2(K)
              LOCQ(5,IPAR)=NORB2
              LOCQ(6,IPAR)=K
              PARTYP(IPAR)%TYPE=parType_constant
            END DO
C
C ADD STOCHASTIC ORBIT PARAMETERS FOR LEO
            DO ISTC=1,NSASTC2
              IF(NSHLP(IS).EQ.NUMSTC2(ISTC))GO TO 272
            END DO
            GO TO 280
272         CONTINUE
C
C SATELLITE FOUND IN LIST NUMSTC2
            DO IPA=1,NSTCEF2(ISTC,IA)
              DO IPE=1,NSTCEP2
                IF (IPAR.GT.MAXLOC) GOTO 990
                IPAR=IPAR+1
                LOCQ(1,IPAR)=11
                LOCQ(2,IPAR)=IA
                LOCQ(3,IPAR)=NSHLP(IS)
                LOCQ(4,IPAR)=IPA
                LOCQ(5,IPAR)=FRCTYP2(IPE)
                LOCQ(6,IPAR)=IPE
                LOCQ(7,IPAR)=ISTC
                PARTYP(IPAR)%TYPE=parType_epochSpecific
              END DO
            END DO
280       CONTINUE
        END DO
      END IF
C
C RECEIVER ANTENNA OFFSETS
C ------------------------
      ITYP=5
      DO 950 IRAO=1,NANRAO
        IF (NFRRAO(IRAO).EQ.1) THEN
          NFR1=1
          NFR2=1
        ELSEIF (NFRRAO(IRAO).EQ.2) THEN
          NFR1=2
          NFR2=2
        ELSEIF (NFRRAO(IRAO).EQ.3) THEN
          NFR1=1
          NFR2=2
        ELSEIF (NFRRAO(IRAO).EQ.4) THEN
          NFR1=3
          NFR2=3
        ELSEIF (NFRRAO(IRAO).EQ.5) THEN
          NFR1=4
          NFR2=4
        ELSE
          GOTO 950
        ENDIF
        DO 970 IFRQ=NFR1,NFR2
          DO 960 ICOR=1,3
            IF (NEURAO(ICOR).EQ.1) THEN
              IPAR=IPAR+1
              IF (IPAR.GT.MAXLOC) GOTO 990
              LOCQ(1,IPAR)=ITYP
              LOCQ(2,IPAR)=IRAO
              LOCQ(3,IPAR)=IFRQ
              LOCQ(4,IPAR)=ICOR
              PARTYP(IPAR)%TYPE=parType_constant
            ENDIF
960       CONTINUE
970     CONTINUE
950   CONTINUE
C
C STATION SPECIFIC TROPOSPHERE PARAMETERS
C ---------------------------------------
      IF (ITRGRD(1).EQ.0) THEN
        NCOM=1
      ELSE
        NCOM=3
      ENDIF
C
      ISTTRP=0
C
      DO 210 I=1,NTRSTA
C
C COUNT REQUESTS OF EACH STATION
        IF (ISTTRP.NE.STATRP(I)) ISTREQ=0
        ISTREQ=ISTREQ+1
        ISTTRP=STATRP(I)
C
        DO 208 ICOM=1,3
          IF (ITRGRD(1).EQ.0 .AND. ICOM.NE.3) GOTO 208
C
C ZENITH/GRADIENT RATIO
          IF (ICOM.NE.3 .AND. MOD(ISTREQ-1,ITRGRD(2)).NE.0) GOTO 208
          IPAR=IPAR+1
          IF (IPAR.GT.MAXLOC) GOTO 990
          LOCQ(1,IPAR)=6
          LOCQ(2,IPAR)=I
          LOCQ(3,IPAR)=STATRP(I)
          LOCQ(4,IPAR)=ICOM
          LOCQ(5,IPAR)=NCOM
          LOCQ(6,IPAR)=ISTREQ
          LOCQ(7,IPAR)=I
          PARTYP(IPAR)%TYPE=parType_linear
C
C UPDATE LOCQ(7,...) TO GET THE NUMBER OF THE LAST REQUEST BELONGING TO
C THE CURRENT GRADIENT PARAMETER
C          IF (ITRGRD(1).NE.0) THEN
C            IF (ICOM.EQ.1) THEN
C              IPGRD1=IPAR
C            ELSEIF (ICOM.EQ.2) THEN
C              IPGRD2=IPAR
C            ELSEIF (ICOM.EQ.3) THEN
C              LOCQ(7,IPGRD1)=I
C              LOCQ(7,IPGRD2)=I
C            ENDIF
C          ENDIF
208     CONTINUE
210   CONTINUE
C
C  IONOSPHERIC DEVELOPMENT
C  -----------------------
      DO 410 IREQ=1,NIOREQ
        DO 420 ILAT=0,IONREQ(1,IREQ)
          DO 430 IHOUR=0,IONREQ(2,IREQ)
            IF(ILAT+IHOUR.GT.IONREQ(3,IREQ)) GOTO 420
            IPAR=IPAR+1
            IF (IPAR.GT.MAXLOC) GOTO 990
            LOCQ(1,IPAR)=7
            LOCQ(2,IPAR)=IONMOD(IREQ)
            LOCQ(3,IPAR)=ILAT
            LOCQ(4,IPAR)=IHOUR
            PARTYP(IPAR)%TYPE=parType_constant
430       CONTINUE
420     CONTINUE
410   CONTINUE
C
C DIFFERENTIAL CODE BIASES FOR SATELLITES AND RECEIVERS
C -----------------------------------------------------
      IF (OPTDCB(1).GT.0) THEN
C
C CREATE LIST OF OBSERVED SATELLITES
        NSAT=0
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
C
C ORDER SATELLITE LIST
        CALL IORDUP(SATLIS,NSAT,SATIND)
C
C SET UP SATELLITE-SPECIFIC DCBS
        DO ISAT=1,NSAT
C
C         NO P1C1 BIAS FOR GLONASS (GOES INTO INTER-SYSTEM/FREQUENCY BIASES)
          IF (OPTDCB(1).EQ.2.AND.SATLIS(SATIND(ISAT))/100.EQ.1) CYCLE
          IPAR=IPAR+1
          IF (IPAR.GT.MAXLOC) GOTO 990
          LOCQ(1,IPAR)=8
          LOCQ(2,IPAR)=1
          LOCQ(3,IPAR)=SATLIS(SATIND(ISAT))
          LOCQ(4,IPAR)=NSAT
          LOCQ(5,IPAR)=OPTDCB(1)
          PARTYP(IPAR)%TYPE=parType_constant
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
870     FORMAT(/,' *** SR SEQPAR: ESTIMATION OF DIFFERENTIAL CODE ',
     1    'BIASES',/,
     2    16X,'REFERENCE SATELLITE ',I3.2,' NOT OBSERVED',/)
        CALL EXITRC(2)
C
860     CONTINUE
      ENDIF
C
C SET UP RECEIVER-SPECIFIC DCBS
C
C             ADD_GNSS_HERE  conflict for SBAS,COMPASS,QZSS in ISASYS
C
      IF (OPTDCB(2).NE.0) THEN
        DO 845 IREC=1,NSTAT
          IsSYS=.false.
          iNum = 0
          DO IFIL=1,NFTOT
            IF (STFIL(2*IFIL-1).EQ.IREC) THEN
              DO isys=0,2
                IF (ISASYS.EQ.1 .AND. ISYS.NE.0) CYCLE
                IF (ISASYS.EQ.2 .AND. ISYS.NE.1) CYCLE
                IF (ISASYS.EQ.3 .AND. ISYS.NE.2) CYCLE
                IF(SVNSYS(isys,NSATEL(IFIL),SATNUM(:,IFIL)))
     1                                         IsSYS(isys+1)=.true.
C               ADD_GNSS_HERE
              ENDDO
            ENDIF
            IF (NDIFF(IFIL).EQ.1) THEN
              IF (STFIL(2*IFIL).EQ.IREC) THEN
                DO isys=0,2
                  IF (ISASYS.EQ.1 .AND. ISYS.NE.0) CYCLE
                  IF (ISASYS.EQ.2 .AND. ISYS.NE.1) CYCLE
                  IF (ISASYS.EQ.3 .AND. ISYS.NE.2) CYCLE
                  IF(SVNSYS(isys,NSATEL(IFIL),SATNUM(:,IFIL)))
     1                                         IsSYS(isys+1)=.true.
C               ADD_GNSS_HERE
                ENDDO
              ENDIF
            ENDIF
          ENDDO
          DO isys=1,3
C           ADD_GNSS_HERE
            IF (.NOT. IsSYS(isys)) CYCLE
            iNum = iNum + 1
            IF ((iNum <= 1).AND.(OPTDCB(2).EQ.3)) CYCLE
            IPAR=IPAR+1
            IF (IPAR.GT.MAXLOC) GOTO 990
            LOCQ(1,IPAR)=8
            LOCQ(2,IPAR)=2
            LOCQ(3,IPAR)=IREC
            LOCQ(4,IPAR)=NSTAT
            LOCQ(5,IPAR)=isys
            LOCQ(6,IPAR)=OPTDCB(2)
          ENDDO
845     CONTINUE
      ENDIF
C
C LOCAL TROPOSPHERE MODEL PARAMETERS
C ----------------------------------
      DO 550 IMOD=1,NTRREQ
        DO 540 KPAR=1,NPARTR(IMOD)
          IPAR=IPAR+1
          IF (IPAR.GT.MAXLOC) GOTO 990
          LOCQ(1,IPAR)=9
          LOCQ(2,IPAR)=IMOD
          LOCQ(3,IPAR)=KPAR
          LOCQ(4,IPAR)=NPARTR(IMOD)
          PARTYP(IPAR)%TYPE=parType_constant
540     CONTINUE
550   CONTINUE
C
C EARTH ROTATION PARAMETERS
C -------------------------
      IF (POLMOD.EQ.1) THEN
        DO 600 IPOL=1,NPOL
          DO 610 KPAR=1,5
            DO 620 IDEG=1,POLPAR(KPAR)
              IPAR=IPAR+1
              IF (IPAR.GT.MAXLOC) GOTO 990
              LOCQ(1,IPAR)=10
              LOCQ(2,IPAR)=POLMOD
              LOCQ(3,IPAR)=IPOL
              LOCQ(4,IPAR)=KPAR
              LOCQ(5,IPAR)=IDEG
              LOCQ(6,IPAR)=POLPAR(KPAR)
              LOCQ(7,IPAR)=NPOL
              PARTYP(IPAR)%TYPE=parType_constant
620         CONTINUE
610       CONTINUE
600     CONTINUE
      ENDIF
C
C SATELLITE ANTENNA OFFSETS
C -------------------------
      IF (NRQOFF.NE.0) THEN
        DO 710 IOFR=1,NRQOFF
          DO 700 ICOR=1,3
            IF (PAROFF(ICOR).EQ.0) GOTO 700
            IPAR=IPAR+1
            IF (IPAR.GT.MAXLOC) GOTO 990
            LOCQ(1,IPAR)=12
            LOCQ(2,IPAR)=IOFR
            LOCQ(3,IPAR)=ICOR
            LOCQ(4,IPAR)=GRPOFF(IOFR)
            IF (GRPOFF(IOFR) /= 0) THEN
              LOCQ(5,IPAR)=GNROFF(GRPOFF(IOFR))
            ELSE
              LOCQ(5,IPAR)=GNROFF(IOFR)
            ENDIF
            LOCQ(6,IPAR)=ICARR(1,1)
            PARTYP(IPAR)%TYPE=parType_constant
700       CONTINUE
710     CONTINUE
      ENDIF
C
C EARTH' POTENTIAL
C ----------------
      IF(NPOT.NE.0)THEN
        DO 720 I=1,NPOT
          IPAR=IPAR+1
          LOCQ(1,IPAR)=13
          LOCQ(2,IPAR)=0
          LOCQ(3,IPAR)=0
          LOCQ(4,IPAR)=POTTYP(1,I)
          LOCQ(5,IPAR)=POTTYP(2,I)
          LOCQ(6,IPAR)=POTTYP(3,I)
          LOCQ(7,IPAR)=I
          PARTYP(IPAR)%TYPE=parType_constant
720     CONTINUE
      END IF
C
C HILL'S RESONANCE TERMS
C ----------------------
      IF(NHILL.NE.0)THEN
        DO 770 IARC=1,NARC
          DO 760 I=1,NHILL
            IPAR=IPAR+1
            LOCQ(1,IPAR)=14
            LOCQ(2,IPAR)=IARC
            LOCQ(3,IPAR)=HILTYP(1,I)
            LOCQ(4,IPAR)=HILTYP(2,I)
            LOCQ(5,IPAR)=HILTYP(3,I)
            LOCQ(7,IPAR)=I
            PARTYP(IPAR)%TYPE=parType_constant
760       CONTINUE
770     CONTINUE
      END IF
C
C ALBEDO PARAMETERS
C -----------------
      IF(NALB.NE.0.AND.NALBGR.NE.0)THEN
        DO 790 IGR=1,NALBGR
          DO 780 ITYP=1,NALB
            IPAR=IPAR+1
            LOCQ(1,IPAR)=15
            LOCQ(2,IPAR)=0
            LOCQ(3,IPAR)=0
            LOCQ(4,IPAR)=ALBTYP(ITYP)
            LOCQ(5,IPAR)=IGR
            LOCQ(6,IPAR)=0
            LOCQ(7,IPAR)=ITYP
            PARTYP(IPAR)%TYPE=parType_constant
780       CONTINUE
790     CONTINUE
      END IF
C
C CENTER OF MASS PARAMETERS
C -------------------------
      IF(NCENM.NE.0)THEN
        DO 800 I=1,NCENM
          IPAR=IPAR+1
          LOCQ(1,IPAR)=16
          LOCQ(2,IPAR)=CENMAS(I)
          DO 795 K=3,MXCLCQ
            LOCQ(K,IPAR)=0
795       CONTINUE
          LOCQ(7,IPAR)=I
          PARTYP(IPAR)%TYPE=parType_constant
800     CONTINUE
      END IF
C
C DIFFERENTIAL IONOSPHERE PARAMETERS
C ----------------------------------
      IF(OPTDIP(1).EQ.1)THEN
        DO 805 IFIL=1,NFTOT
          DO 810 ISAT=1,NSATEL(IFIL)
            DO 815 IEPO=1,NEPOCH(IFIL)
              TOBS=TIMREF(IFIL)+(IEPO-1)*IDELTT(IFIL)/86400.D0
              IF(TOBS.LT.(WINDOW(1,IFIL)-DTSIM).OR.
     1           TOBS.GT.(WINDOW(2,IFIL)+DTSIM)) GOTO 815
              IF(NSAMPL(1).NE.0)THEN
                CALL MJDGPS(TOBS,GPSSEC,NWEEK)
                TFRAC=GPSSEC-DNINT(GPSSEC/NSAMPL(1))*NSAMPL(1)
                IF(DABS(TFRAC).GT.DTSIM*86400.D0) GOTO 815
              ELSE IF(NSAMPL(3).NE.0)THEN
                CALL MJDGPS(TOBS,GPSSEC,NWEEK)
                TFRAC=GPSSEC-DNINT(GPSSEC/NSAMPL(3))*NSAMPL(3)
                IF(DABS(TFRAC).GT.DTSIM*86400.D0) GOTO 815
              ENDIF
              IPAR=IPAR+1
              IF(IPAR.GT.MAXLOC) GOTO 990
              LOCQ(1,IPAR)=17
              LOCQ(2,IPAR)=IFIL
              LOCQ(3,IPAR)=SATNUM(ISAT,IFIL)
              LOCQ(4,IPAR)=IEPO
              LOCQ(5,IPAR)=0
              PARTYP(IPAR)%TYPE=parType_epochSpecific
815         CONTINUE
810       CONTINUE
805     CONTINUE
      ENDIF
C
C RECEIVER ANTENNA PHASE CENTER VARIATIONS
C ----------------------------------------
      ITYP=18
      DO 900 ICAL=1,NANCAL
        IF (NFRCAL(ICAL).EQ.1) THEN
          NFR1=1
          NFR2=1
        ELSEIF (NFRCAL(ICAL).EQ.2) THEN
          NFR1=2
          NFR2=2
        ELSEIF (NFRCAL(ICAL).EQ.3) THEN
          NFR1=1
          NFR2=2
        ELSEIF (NFRCAL(ICAL).EQ.4) THEN
          NFR1=3
          NFR2=3
        ELSEIF (NFRCAL(ICAL).EQ.5) THEN
          NFR1=4
          NFR2=4
        ELSE
          GOTO 900
        ENDIF
        IF (NPTCAL(1,ICAL).GT.0) THEN
          DO 910 IAZI=1,NPTCAL(2,ICAL)-1
            DO 920 IFRQ=NFR1,NFR2
              DO 930 IZEN=1,IABS(NPTCAL(1,ICAL))
                IPAR=IPAR+1
                IF (IPAR.GT.MAXLOC) GOTO 990
                LOCQ(1,IPAR)=ITYP
                LOCQ(2,IPAR)=ICAL
                LOCQ(3,IPAR)=IFRQ
                LOCQ(4,IPAR)=IZEN
                LOCQ(5,IPAR)=IAZI
                LOCQ(6,IPAR)=NPTCAL(1,ICAL)
                LOCQ(7,IPAR)=NPTCAL(2,ICAL)
                PARTYP(IPAR)%TYPE=parType_constant
930           CONTINUE
920         CONTINUE
910       CONTINUE
        ELSE
          ILIM = MIN0(-NPTCAL(1,ICAL),NPTCAL(2,ICAL))
          DO 915 IAZI=-ILIM,ILIM
            DO 925 IFRQ=NFR1,NFR2
              DO 935 IZEN=1,-NPTCAL(1,ICAL)
                IF (IZEN.LT.IABS(IAZI)) GO TO 935
                IF (MOD(IZEN-IABS(IAZI),2).NE.0) GOTO 935
                IPAR=IPAR+1
                IF (IPAR.GT.MAXLOC) GOTO 990
                LOCQ(1,IPAR)=ITYP
                LOCQ(2,IPAR)=ICAL
                LOCQ(3,IPAR)=IFRQ
                LOCQ(4,IPAR)=IZEN
                LOCQ(5,IPAR)=IAZI
                LOCQ(6,IPAR)=NPTCAL(1,ICAL)
                LOCQ(7,IPAR)=NPTCAL(2,ICAL)
                PARTYP(IPAR)%TYPE=parType_constant
935           CONTINUE
925         CONTINUE
915       CONTINUE
        END IF
900   CONTINUE
C
C GLOBAL IONOSPHERE MODEL PARAMETERS
C ----------------------------------
      CALL SEQGIM(OPTGIM,POLGIM,NAMGIM,EPOGIM)
C
      IF (OPTGIM(9).EQ.1) THEN
        NMOD=NSTAT
        OPTGIM(7)=NMOD
C
        CALL DIMTST(1,1,2,'SEQPAR','MAXGIM',
     1    'GLOBAL IONOSPHERE MODELS','INCLUDE FILE USED',NMOD,MAXGIM,
     2    IRC)
C
        DO IMOD=2,NMOD
          EPOGIM(1,IMOD)=EPOGIM(1,1)
          EPOGIM(2,IMOD)=EPOGIM(2,1)
          POLGIM(1,IMOD)=POLGIM(1,1)
          POLGIM(2,IMOD)=POLGIM(2,1)
          POLGIM(3,IMOD)=POLGIM(3,1)
        ENDDO
      ELSE
        NMOD=OPTGIM(7)
      ENDIF
C
      IF (NMOD.EQ.1.AND.OPTGIM(5).EQ.1) OPTGIM(5)=2
C
C SINGLE-LAYER HEIGHT
      IF (OPTGIM(5).EQ.1) THEN
        IPAR=IPAR+1
        IF (IPAR.GT.MAXLOC) GO TO 990
        LOCQ(1,IPAR)=19
        LOCQ(2,IPAR)=2
        LOCQ(3,IPAR)=0
        LOCQ(4,IPAR)=0
        LOCQ(5,IPAR)=0
        PARTYP(IPAR)%TYPE=parType_constant
      ELSE IF (OPTGIM(5).EQ.2) THEN
        DO 1910 IMOD=1,NMOD
          IPAR=IPAR+1
          IF (IPAR.GT.MAXLOC) GO TO 990
          LOCQ(1,IPAR)=19
          LOCQ(2,IPAR)=2
          LOCQ(3,IPAR)=IMOD
          LOCQ(4,IPAR)=0
          LOCQ(5,IPAR)=0
          PARTYP(IPAR)%TYPE=parType_constant
          IF (EPOGIM(2,IMOD)==0d0) PARTYP(IPAR)%TYPE=parType_linear
1910    CONTINUE
      END IF
C
C IONOSPHERIC COEFFICIENTS
      NDEG=OPTGIM(1)
      NORD=OPTGIM(2)
C
      IF (OPTGIM(10).EQ.1) THEN
        IREQ=1
      ELSE
        IREQ=3
      ENDIF
C
      DO 1920 IMOD=1,NMOD
        DO 1930 IDEG=0,NDEG
          DO 1940 IORD=0,NORD
            IF (IORD.GT.IDEG) GO TO 1930
            IPAR=IPAR+1
            IF (IPAR.GT.MAXLOC) GO TO 990
            LOCQ(1,IPAR)=19
            LOCQ(2,IPAR)=IREQ
            LOCQ(3,IPAR)=IMOD
            LOCQ(4,IPAR)=IDEG
            LOCQ(5,IPAR)=IORD
            IF (IDEG.EQ.0) THEN
              LOCQ(6,IPAR)= 324000
              LOCQ(7,IPAR)=-324000
            END IF
            PARTYP(IPAR)%TYPE=parType_constant
            IF (EPOGIM(2,IMOD)==0d0) PARTYP(IPAR)%TYPE=parType_linear
            IF (IORD.GT.0) THEN
              IPAR=IPAR+1
              IF (IPAR.GT.MAXLOC) GO TO 990
              LOCQ(1,IPAR)=19
              LOCQ(2,IPAR)=IREQ
              LOCQ(3,IPAR)=IMOD
              LOCQ(4,IPAR)=IDEG
              LOCQ(5,IPAR)=-IORD
              PARTYP(IPAR)%TYPE=parType_constant
              IF (EPOGIM(2,IMOD)==0d0) PARTYP(IPAR)%TYPE=parType_linear
            END IF
1940      CONTINUE
1930    CONTINUE
1920  CONTINUE
C
C KINEMATIC COORDINATES
C ---------------------
      ITYP=21
C
      IF (NKIN.GT.0 .AND. (OPTELI(ITYP).LE.2.OR.NSAMPL(3).NE.0)) THEN
        IFIL=NFTOT
CC        IF (IFIL.GT.2) THEN
CC          WRITE(LFNERR,*)'SEQPAR: NFTOT .GT. 2'
CC          CALL EXITRC(2)
CC        END IF
C
        DO 500 ISTAT=1,NSTAT
          IF(ICENTR(ISTAT).EQ.ISTAT) THEN
C
            DO 510 IKIN=1,NKIN
              IF(STKIN(IKIN).EQ.ISTAT) THEN
C                DO 520 IEPO=1,NEPOCH(IFIL)
C                  TOBS=TIMREF(IFIL)+(IEPO-1)*IDELTT(IFIL)/86400.D0
C                  IF (TOBS.LT.(WINDOW(1,IFIL)-DTSIM) .OR.
C     1                TOBS.GT.(WINDOW(2,IFIL)+DTSIM)) GOTO 520
C                  IF (NSAMPL(1).NE.0.AND.
C     1                (OPTELI(21).NE.3.OR.NSAMPL(3).EQ.0)) THEN
C                    CALL MJDGPS(TOBS,GPSSEC,NWEEK)
C                    TFRAC=GPSSEC-DNINT(GPSSEC/NSAMPL(1))*NSAMPL(1)
C                    IF (DABS(TFRAC).GT.DTSIM*86400.D0) GOTO 520
C                  ELSE IF (OPTELI(21).EQ.3.AND.NSAMPL(3).NE.0) THEN
C                    CALL MJDGPS(TOBS,GPSSEC,NWEEK)
C                    TFRAC=GPSSEC-DNINT(GPSSEC/NSAMPL(3))*NSAMPL(3)
C                    IF (DABS(TFRAC).GT.DTSIM*86400.D0) GOTO 520
C                  ENDIF
C
C                  DTIME=TOBS-CLKHED%TFIRST
C                  INTEPO=IDNINT((DTIME*86400D0)/
C     1                           CLKREC%EPOCH(1)*CLKREC%NEPO)+1
C
                DO 520 INTEPO=1,CLKREC%NEPO
                  IF (NSAMPL(3).NE.0) THEN
                    DSEC=DBLE(INTEPO-1)*CLKREC%EPOCH(1)/CLKREC%NEPO
                    TCLK=CLKHED%TFIRST+DSEC/86400D0
C
                    CALL MJDGPS(TCLK,GPSSEC,NWEEK)
                    TFRAC=GPSSEC-DNINT(GPSSEC/NSAMPL(3))*NSAMPL(3)
                    IF (DABS(TFRAC).GT.DTSIM*86400.D0) GOTO 520
                  ENDIF
                  DO 530 K=1,3
                    IPAR=IPAR+1
                    IF (IPAR.GT.MAXLOC) GO TO 990
                    LOCQ(1,IPAR)=ITYP
                    LOCQ(2,IPAR)=ISTAT
                    LOCQ(3,IPAR)=K
                    LOCQ(4,IPAR)=INTEPO
                    LOCQ(5,IPAR)=0
                    LOCQ(6,IPAR)=0
                    PARTYP(IPAR)%TYPE=parType_epochSpecific
530               CONTINUE
520             CONTINUE
              END IF
510         CONTINUE
C
          END IF
500     CONTINUE
      END IF
C
C SCALING FACTORS FOR VIENNA GRID FILES
C -------------------------------------
      ITYP=22
C
      DO II = 1,SIZE(OPLOAD)
        DO ISTAT = 1,OPLOAD(II)%NSTA
          JSTA = 0
          DO ISTA = 1,ISTAT-1
            IF (OPLOAD(II)%STACLU(ISTAT).EQ.0) EXIT
            IF (OPLOAD(II)%STACLU(ISTAT).EQ.OPLOAD(II)%STACLU(ISTA))THEN
              JSTA=ISTA
              EXIT
            ENDIF
          ENDDO
          IF (JSTA.NE.0) CYCLE
          DO K = 1,OPLOAD(II)%NPAR
            IPAR=IPAR+1
            IF (IPAR.GT.MAXLOC) GO TO 990
            LOCQ(1,IPAR)=ITYP
            LOCQ(2,IPAR)=II
            LOCQ(3,IPAR)=ISTAT
            LOCQ(4,IPAR)=K
            LOCQ(5,IPAR)=OPLOAD(II)%NPAR
            LOCQ(6,IPAR)=0
            LOCQ(7,IPAR)=0
            PARTYP(IPAR)%TYPE=parType_constant
          ENDDO
C ONE PARAMETER FOR ALL STATIONS
          IF (OPLOAD(II)%STACLU(ISTAT).EQ.-1) EXIT
        ENDDO
      ENDDO
C
C EPOCH CLOCKS STATIONS
C ---------------------
      ITYP=23
      IF (OPTELI(ITYP).LE.2.OR.NSAMPL(3).NE.0) THEN
C
C STATION LOOP (ASSUMPTION ONLY ONE ZERO DIFFERENCE OBSERVATION FILE PER STAT.)
C -----------------------------------------------------------------------------
        DO 1000 ICLK=1,NCLKST
          DO 1010 IFIL=1,NFTOT
            IF (NDIFF(IFIL).EQ.0) THEN
              IF (CLKSTA(ICLK).EQ.STFIL((IFIL*2)-1)) THEN
C
                ICLOCK=-1
                IF (CLKSYS.EQ.1) THEN
                  ICLOCK=MIXSVN(NSATEL(IFIL),SATNUM(1,IFIL))
                ENDIF
C
                DO 1020 IEPO=1,NEPOCH(IFIL)
                  TOBS=TIMREF(IFIL)+(IEPO-1)*IDELTT(IFIL)/86400.D0
                  IF (TOBS.LT.(WINDOW(1,IFIL)-DTSIM) .OR.
     1                TOBS.GT.(WINDOW(2,IFIL)+DTSIM)) GOTO 1020
                  IF (NSAMPL(1).NE.0.AND.
     1                (OPTELI(23).NE.3.OR.NSAMPL(3).EQ.0)) THEN
                    CALL MJDGPS(TOBS,GPSSEC,NWEEK)
                    TFRAC=GPSSEC-DNINT(GPSSEC/NSAMPL(1))*NSAMPL(1)
                    IF (DABS(TFRAC).GT.DTSIM*86400.D0) GOTO 1020
                  ELSE IF (OPTELI(23).EQ.3.AND.NSAMPL(3).NE.0) THEN
                    CALL MJDGPS(TOBS,GPSSEC,NWEEK)
                    TFRAC=GPSSEC-DNINT(GPSSEC/NSAMPL(3))*NSAMPL(3)
                    IF (DABS(TFRAC).GT.DTSIM*86400.D0) GOTO 1020
                  ENDIF
C
C DEFINE EPOCH NUMBER WITH RESPECT TO FIRST OBSERVATION OF THE FIRST FILE
C ------------------------------------------------------------------------
                  DTIME=TOBS-CLKHED%TFIRST
                  INTEPO=IDNINT((DTIME*86400D0)/
     1                          CLKREC%EPOCH(1)*CLKREC%NEPO)+1
C
                  DO II = 1,2
                    IF (II.EQ.1.AND.ICLOCK.EQ.2) CYCLE
C
                    IPAR=IPAR+1
                    IF (IPAR.GT.MAXLOC) GO TO 990
                    LOCQ(1,IPAR)=ITYP
                    LOCQ(2,IPAR)=CLKSTA(ICLK)
                    IF (CLKSYS.EQ.0) THEN
                      LOCQ(3,IPAR)=0
                    ELSE
                      LOCQ(3,IPAR)=II
                    ENDIF
                    LOCQ(4,IPAR)=INTEPO
                    LOCQ(5,IPAR)=IFIL
                    LOCQ(6,IPAR)=0
                    LOCQ(7,IPAR)=0
                    IF (CLKHED%NUMREF.EQ.2) THEN
                      CLKNAM=STNAME(CLKSTA(iCLK))
                      DO 1030 IREF=1,CLKHED%REF(1)%NREF
                        IF (CLKHED%REF(1)%CLK(IREF)%NAME.EQ.CLKNAM) THEN
                          LOCQ(7,IPAR)=IREF
                        ENDIF
1030                  CONTINUE
                    ENDIF
                    PARTYP(IPAR)%TYPE=parType_epochSpecific
C
                    IF (ICLOCK.NE.1) EXIT
                  ENDDO
C
1020            CONTINUE
                GOTO 1000
              END IF
            END IF
1010      CONTINUE
1000    CONTINUE
      END IF
C
C EPOCH CLOCKS SATELLITES
C -----------------------
      ITYP=24
      IF (OPTELI(ITYP).LE.2.OR.NSAMPL(3).NE.0) THEN
        DO 1100 IARC=1,NARC
          DO 1110 ISAT=1,NUMSAT(IARC)
C
C           SELECT SATELLITE SYSTEM
            CALL SVN2CHR(NAVNUM(ISAT),ISVN,SVNCHR)
            IF (ISASYS.EQ.1 .AND. SVNCHR.NE.'G') GOTO 1110
            IF (ISASYS.EQ.2 .AND. SVNCHR.NE.'R') GOTO 1110
            IF (ISASYS.EQ.3 .AND. SVNCHR.NE.'E') GOTO 1110
c            IF (ISASYS.EQ.4 .AND. SVNCHR.NE.'S') GOTO 1110
c            IF (ISASYS.EQ.5 .AND. SVNCHR.NE.'C') GOTO 1110
c            IF (ISASYS.EQ.6 .AND. SVNCHR.NE.'J') GOTO 1110
C
C             ADD_GNSS_HERE  conflict for SBAS,COMPASS,QZSS
C
            DO JSAT=1,ISAT-1
              IF (NAVNUM(JSAT).EQ.MODSVN(NAVNUM(ISAT))) GOTO 1110
            ENDDO
C
            DO 1120 ICLK=1,NCLKSA
              IF(CLKSAT(ICLK).EQ.MODSVN(NAVNUM(ISAT))) THEN
C
C DETERMINE ALL EPOCHS THE SATELLITE WAS OBSERVED
C -----------------------------------------------
                NSAEPO=0
                DO 1130 IFIL=1,NFTOT
                  DO 1140 IEPO=1,NEPOCH(IFIL)
                    TOBS=TIMREF(IFIL)+(IEPO-1)*IDELTT(IFIL)/86400.D0
                    IF (TOBS.LT.(WINDOW(1,IFIL)-DTSIM) .OR.
     1                  TOBS.GT.(WINDOW(2,IFIL)+DTSIM)) GOTO 1140
                    IF (NSAMPL(1).NE.0.AND.
     2                  (OPTELI(24).NE.3.OR.NSAMPL(1).EQ.0)) THEN
                      CALL MJDGPS(TOBS,GPSSEC,NWEEK)
                      TFRAC=GPSSEC-DNINT(GPSSEC/NSAMPL(1))*NSAMPL(1)
                      IF (DABS(TFRAC).GT.DTSIM*86400.D0) GOTO 1140
                    ELSE IF (OPTELI(24).EQ.3.AND.NSAMPL(3).NE.0) THEN
                      CALL MJDGPS(TOBS,GPSSEC,NWEEK)
                      TFRAC=GPSSEC-DNINT(GPSSEC/NSAMPL(3))*NSAMPL(3)
                      IF (DABS(TFRAC).GT.DTSIM*86400.D0) GOTO 1140
                    ENDIF
                    DO 1150 I=1,NSAEPO
                      IF (DNINT(SATTIM(I)*864D2).EQ.DNINT(TOBS*864D2))
     1                  GOTO 1140
1150                CONTINUE
C
C DEFINE EPOCH NUMBER WITH RESPECT TO FIRST OBSERVATION OF THE FIRST FILE
C ------------------------------------------------------------------------
                    DTIME=TOBS-CLKHED%TFIRST
                    INTEPO=IDNINT((DTIME*864D2)/
     1                             CLKREC%EPOCH(1)*CLKREC%NEPO)+1
C
C SORT SATELLITE CLOCKS
C ---------------------
                    IF (NSAEPO.EQ.0) THEN
                      NSAEPO=NSAEPO+1
                      SATEPO(NSAEPO)=INTEPO
                    ELSE IF (INTEPO.GT.SATEPO(NSAEPO)) THEN
                      NSAEPO=NSAEPO+1
                      SATEPO(NSAEPO)=INTEPO
                    ELSE
                      DO 1170 I=1,NSAEPO
                        IF (INTEPO.LT.SATEPO(I)) THEN
                          DO 1180 J=NSAEPO,I,-1
                            SATEPO(J+1)=SATEPO(J)
1180                      CONTINUE
                          GOTO 1190
                        ENDIF
1170                  CONTINUE
1190                  NSAEPO=NSAEPO+1
                      SATEPO(I)=INTEPO
                    ENDIF
                    SATTIM(NSAEPO)=TOBS
                    IF (NSAEPO.GT.MAXEPO) THEN
                      WRITE(LFNERR,*)'SEQPAR: NSAEPO .GT. MAXEPO'
                      CALL EXITRC(2)
                    END IF
1140              CONTINUE
1130            CONTINUE
                DO 1160 I=1,NSAEPO
                  IPAR=IPAR+1
CC                  PARLST(1,ITYP)=PARLST(1,ITYP)+1
CC                  PARLST(2,ITYP)=PARLST(2,ITYP)+1
                  IF (IPAR.GT.MAXLOC) GO TO 990
                  LOCQ(1,IPAR)=ITYP
                  LOCQ(2,IPAR)=0
                  LOCQ(3,IPAR)=CLKSAT(ICLK)
                  LOCQ(4,IPAR)=SATEPO(I)
                  LOCQ(5,IPAR)=1
                  LOCQ(6,IPAR)=0
                  LOCQ(7,IPAR)=0
                  IF (CLKHED%NUMREF.EQ.2) THEN
                    CLKNAM=' '
                    CALL SVN2CHR(CLKSAT(ICLK),IHLP,CLKNAM(1:1))
                    WRITE(CLKNAM(2:3),'(I2.2)') IHLP
                    DO 1195 IREF=1,CLKHED%REF(1)%NREF
                      IF (CLKHED%REF(1)%CLK(IREF)%NAME.EQ.CLKNAM) THEN
                        LOCQ(7,IPAR)=IREF
                      ENDIF
1195                CONTINUE
                  ENDIF
                  PARTYP(IPAR)%TYPE=parType_epochSpecific
C
1160            CONTINUE
              END IF
1120        CONTINUE
1110      CONTINUE
1100    CONTINUE
      END IF
C
C SATELLITE ANTENNA PHASE CENTER VARIATIONS
C -----------------------------------------
      IF (NANSPV.NE.0) THEN
        ITYP=25
        DO 1200 ISPV=1,NANSPV
          IF (NSASPV(ISPV).NE.0) THEN
            DO 1210 IAZI=1,NPTSPV(2,ISPV)-1
              DO 1220 IZEN=1,NPTSPV(1,ISPV)
                IPAR=IPAR+1
                IF (IPAR.GT.MAXLOC) GOTO 990
                LOCQ(1,IPAR)=ITYP
                LOCQ(2,IPAR)=ISPV
                LOCQ(3,IPAR)=GNRSPV(ISPV)
                LOCQ(4,IPAR)=IZEN
                LOCQ(5,IPAR)=IAZI
                LOCQ(6,IPAR)=NPTSPV(1,ISPV)
                LOCQ(7,IPAR)=NPTSPV(2,ISPV)
                PARTYP(IPAR)%TYPE=parType_constant
1220          CONTINUE
1210        CONTINUE
          ENDIF
1200    CONTINUE
      ENDIF
C
C SLR RANGE BIASES (SATELLITE- AND STATION-SPECIFIC)
C ---------------------------------------------------
      IF (NRGB.NE.0) THEN
        ITYP=26

        NSAT = 0
C
C Prepare parameter set up
        DO IFIL = 1, NFTOT

          DO IRGB=1, NRGB
            IF ( STFIL(2*IFIL-1).EQ.STARGB(IRGB) ) THEN
C
C Initialize list for Range biases
              DO ISAT = 1, MAXSAT
                RGBLST(IRGB,ISAT) = .FALSE.
              END DO
C
C Range bias common to all satellites
              IF ( satSpec == 3 ) THEN
                NSAT = 1
                ISAT = 1
                SATLIS(NSAT) = 1000
                GOTO 851
              ENDIF
C
C CREATE LIST OF ALL RELEVANT SATELLITES
              DO ISATEL=1, NSATEL(IFIL)
                ISYS = INT(SATNUM(ISATEL,IFIL)/100)

                DO ISAT=1, NSAT
                  IF ( satSpec == 1 .AND.
     1                 SATNUM(ISATEL,IFIL) .EQ. SATLIS(ISAT) ) GOTO 852
C
                  IF ( satSpec == 2 .AND.
     1                 (-1)*(ISYS+1) .EQ. SATLIS(ISAT) ) GOTO 852
                ENDDO
                NSAT = NSAT + 1
                IF ( satSpec == 1 ) THEN
                  SATLIS(NSAT) = SATNUM(ISATEL,IFIL)
                ELSE
                  SATLIS(NSAT) = (-1)*(ISYS+1)
                ENDIF

852             CONTINUE
              ENDDO

851           CONTINUE

            ENDIF
          ENDDO
        ENDDO
C
        CALL IORDUP(SATLIS,NSAT,SATIND)
C
C Look for Station/Satellite Range biases that are observed
        Loop_RGB: DO IRGB = 1, NRGB

          Loop_Fil: DO IFIL = 1, NFTOT
            IF ( STFIL(2*IFIL-1).EQ.STARGB(IRGB) ) THEN

C Range bias common to all satellites
              IF ( satSpec == 3 .AND. RGBLST(IRGB,1) ) CYCLE Loop_Fil
C
C Look for index of satellite
              Loop_Sat: DO ISATEL = 1, NSATEL(IFIL)
                ISYS = INT(SATNUM(ISATEL,IFIL)/100)

                DO ISAT = 1, NSAT
                  IF ( satSpec == 1 .AND.
     1                 SATNUM(ISATEL,IFIL).EQ.SATLIS(SATIND(ISAT))) EXIT
C
                  IF ( satSpec == 2 .AND.
     1                 (-1)*(ISYS+1) .EQ. SATLIS(SATIND(ISAT)) ) EXIT
                END DO
C
C Station- and satellite-specific RGB
                IF ( satSpec <= 2 .AND. RGBLST(IRGB,SATIND(ISAT)) )
     1            CYCLE Loop_Sat
C
                CONTINUE
                IPAR = IPAR + 1
                IREC = IREC + 1
                IF (IPAR.GT.MAXLOC ) GOTO 990

                LOCQ(1,IPAR) = ITYP
                LOCQ(2,IPAR) = STFIL(2*IFIL-1)
                LOCQ(3,IPAR) = ISAT
                LOCQ(4,IPAR) = 1
CCC              LOCQ(4,IPAR) = IWL
                LOCQ(5,IPAR) = SATLIS(SATIND(ISAT))
                LOCQ(6,IPAR) = IREC
                RGBLST(IRGB,SATIND(ISAT)) = .TRUE.

                IF ( satSpec == 3 ) THEN
                  LOCQ(3,IPAR) = 1
                  LOCQ(5,IPAR) = 1000
                  EXIT
                ENDIF
                PARTYP(IPAR)%TYPE=parType_constant

              ENDDO Loop_Sat

            ENDIF
          ENDDO Loop_Fil
        ENDDO Loop_RGB

      ENDIF
C
C HOI SCALING PARAMETERS
C ----------------------
      ITYP = 27
      DO IHOI = 1,3
        IF (OPTHOI(IHOI).EQ.0) THEN
          CYCLE
C
C       ONE_FOR_ALL
        ELSEIF (OPTHOI(IHOI).EQ.1) THEN
          IPAR = IPAR+1
          IF(IPAR.GT.MAXLOC) GOTO 990
          LOCQ(1,IPAR) = ITYP
          LOCQ(2,IPAR) = IHOI
          LOCQ(3,IPAR) = OPTHOI(IHOI)
          PARTYP(IPAR)%TYPE=parType_constant
C
C       ONE_PER_STATION
        ELSEIF (OPTHOI(IHOI).EQ.2) THEN
          DO ISTAT = 1,NSTAT
            IPAR = IPAR+1
            IF (IPAR.GT.MAXLOC) GOTO 990
            LOCQ(1,IPAR) = ITYP
            LOCQ(2,IPAR) = IHOI
            LOCQ(3,IPAR) = OPTHOI(IHOI)
            LOCQ(4,IPAR) = ISTAT
            PARTYP(IPAR)%TYPE=parType_constant
          ENDDO
        ENDIF
      ENDDO
C
C GNSS-SPECIFIC PARAMETERS
C ------------------------
      ITYP=30
C
      IF (OPTGSP%TRASYS.EQ.0.AND.OPTGSP%TRPSYS.EQ.0) GOTO 3020
      DO 3010 ISTAT=1,NSTAT
        SYSPAR = 0
C
C FIND RECEIVER
        DO IFIL=1,NFTOT
          ISYS = 0
          ISYS1= 0
          ISYS2=-1
          IF ((STFIL(2*(IFIL-1)+1).EQ.ISTAT               ) .OR.
     1        (STFIL(2*(IFIL-1)+2).EQ.ISTAT.AND.NDIFF(IFIL).EQ.1)) THEN
            RCVTYP = RECTYP(1,IFIL)
            CALL GETRCV(RCVTYP,NFREQ,ICODE,IWLFAC,ICLASS,ISYS1)
            IF (NDIFF(IFIL).EQ.1) THEN
              RCVTYP = RECTYP(2,IFIL)
              CALL GETRCV(RCVTYP,NFREQ,ICODE,IWLFAC,ICLASS,ISYS2)
            ENDIF
C
C COMBINED RECEIVER(S): GLONASS
            IF ( (ISYS1.EQ.-1.OR.ISYS1.EQ.1).AND.
     1           (ISYS2.EQ.-1.OR.ISYS2.EQ.1).AND.SYSPAR(1).EQ.0) THEN
              SYSPAR(1) = 1
              ISYS = 1
              IS   = 4
              IE   = 3
              IF (OPTGSP%TRASYS.NE.2.AND.OPTGSP%TRASYS.NE.0) IS = 1
              IF (OPTGSP%TRPSYS.NE.2.AND.OPTGSP%TRPSYS.NE.0) IE = 4
              DO II=IS,IE
                IPAR = IPAR+1
                IF (IPAR.GT.MAXLOC) GOTO 990
                LOCQ(1,IPAR) = ITYP
                LOCQ(2,IPAR) = ISTAT
                LOCQ(3,IPAR) = II
                LOCQ(4,IPAR) = ISYS
                PARTYP(IPAR)%TYPE=parType_constant
              ENDDO
            ENDIF
C
C COMBINED RECEIVER(S): Galileo
            IF ( (ISYS1.EQ.-1.OR.ISYS1.EQ.2).AND.
     1           (ISYS2.EQ.-1.OR.ISYS2.EQ.2).AND.SYSPAR(2).EQ.0) THEN
              SYSPAR(2) = 1
              ISYS = 2
              IS   = 4
              IE   = 3
              IF (OPTGSP%TRASYS.NE.1.AND.OPTGSP%TRASYS.NE.0) IS = 1
              IF (OPTGSP%TRPSYS.NE.1.AND.OPTGSP%TRPSYS.NE.0) IE = 4
              DO II=IS,IE
                IPAR = IPAR+1
                IF (IPAR.GT.MAXLOC) GOTO 990
                LOCQ(1,IPAR) = ITYP
                LOCQ(2,IPAR) = ISTAT
                LOCQ(3,IPAR) = II
                LOCQ(4,IPAR) = ISYS
                PARTYP(IPAR)%TYPE=parType_constant
              ENDDO
            ENDIF
C
            IF (SYSPAR(1).EQ.1.AND.SYSPAR(2).EQ.1) GOTO 3010
          ENDIF
        ENDDO
3010  CONTINUE
3020  CONTINUE
C
C AMBIGUITIES
C -----------
      NAMB=0
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
            IPAR=IPAR+1
            IF (IPAR.GT.MAXLOC) GOTO 990
            NAMB=NAMB+1
            LOCQ(1,IPAR)=4
            LOCQ(2,IPAR)=IF
            LOCQ(3,IPAR)=NUMCLS(ICLS)
            LOCQ(4,IPAR)=ICAR
            IF((ICAR.EQ.2.OR.ICAR.EQ.3.OR.ICAR.EQ.4) .AND.
     1         (AMBDEF(IF).EQ.2.OR.AMBDEF(IF).EQ.3)  .AND.
     2         IR5CLS(ICLS).NE.1)           LOCQ(4,IPAR)=1
            LOCQ(5,IPAR)=ICAR
            LOCQ(6,IPAR)=IR5CLS(ICLS)
C
C IF AMBIGUITIES ARE PRE-ELIMINATED, SET LOCQ(7,...) TO LAST EPOCH
C OF FILE; OTHERWISE LOCQ(7,...) IS USED TO SAVE REFERENCE AMBIGUITY
C AND IS INITIALIZED TO ZERO HERE
            IF (STRAMB(1).EQ.-1) THEN
              LOCQ(7,IPAR)=NEPOCH(IF)
            ELSE
              LOCQ(7,IPAR)=0
            ENDIF
            PARTYP(IPAR)%TYPE=parType_constant
220       CONTINUE
240     CONTINUE
250   CONTINUE
C
C STORE NUMBER OF PARAMETERS
C --------------------------
      NPAR=IPAR
      NPARN=NPAR-NAMB
      NPARMS=NPAR
C
C COUNT NUMBER OF PARAMETERS OF EACH TYPE
C ---------------------------------------
      DO 300 ITYP=1,MAXTYP
        DO 290 IPOS=1,5
          PARLST(IPOS,ITYP)=0
290     CONTINUE
300   CONTINUE
C
      DO 310 IPAR=1,NPAR
        ITYPE=LOCQ(1,IPAR)
        PARLST(1,ITYPE)=PARLST(1,ITYPE)+1
        PARLST(2,ITYPE)=PARLST(2,ITYPE)+1
310   CONTINUE
C
      GOTO 999
C
C TOO MANY PARAMETERS FOR LOCQ
C ----------------------------
990   CONTINUE
      CALL DIMTST(1,2,2,'SEQPAR','MAXLOC',
     1  'PARAMETERS TO BE CHARACTERIZED',' ',IPAR,MAXLOC,IRC)
C
999   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
