      MODULE s_PRIAPR
      CONTAINS

C*
      SUBROUTINE PRIAPR(MAXTYP ,NFTOT  ,HEADER ,OBSFIL ,NFRFIL ,ICARR  ,
     1                  WINDOW ,NCAMP  ,CAMPGN ,NSTAT  ,STNAME ,STFIL  ,
     2                  NSCAMP ,STCAMP ,MEATYP ,NSATEL ,CSESS  ,IDELTT ,
     3                  TIMREF ,ICAMPN ,NFREQ  ,NEPOCH ,IRMARK ,NEPFLG ,
     4                  ICLOCK ,SATNUM ,NUMAMB ,AMBSAT ,AMBIEP ,AMBWLF ,
     5                  AMBIGU ,AMBCLS ,STANUM ,NCENTR ,ICENTR ,XSTAT  ,
     6                  XSTELL ,XSTECC ,DATUM  ,AELL   ,BELL   ,DXELL  ,
     7                  DRELL  ,SCELL  ,NARC   ,ARCINT ,NUMSAT ,SOURCE ,
     8                  TBOUND ,NAVNUM ,NPAR   ,LOCQ   ,NSESS  ,SESSID ,
     9                  STITLE ,PRIOPT ,ISYNCR ,ITROPO ,IEXTRA ,AMBDEF ,
     1                  STRAMB ,AMBSAV ,SIGAMB ,ZENMAX ,ZMXLEO ,NSAMPL ,
     2                  ISASYS ,SIGAPR ,ICOELV ,NORRES ,NESTSAT,ESTSAT ,
     3                  NORB   ,SEQORB ,PREC   ,NFIX   ,STFIX  ,NCLREQ ,
     4                  ISTCLK ,ISACLK ,NCLK   ,IBIAS  ,CLKWGT ,CLFRTO ,
     5                  NSTWGT ,
     5                  ISTWGT ,STWGT  ,WGTFILE,NIOREQ ,IONMOD ,IONREQ ,
     6                  OPTDCB ,SIGDCB ,NTRREQ ,NPARTR ,TRPLIM ,SIGTRP ,
     7                  NTRSTA ,STATRP ,TRPLMS ,SIGTRS ,ISGTRS ,
     8                  POLPAR ,NPOL   ,TPOL   ,SIGPOL ,ISGPOL ,CORSTR ,
     9                  DTSIM  ,NWGT   ,SATWGT ,TIMWGT ,WGTWGT ,AR2MOD ,
     1                  AR2INF ,NSTCEP ,FRCTYP ,NSASTC ,NUMSTC1,NSTCEF ,
     2                  TIMSTC ,SIGSTC ,NANOFF ,NSAOFF ,SATOFF ,PAROFF ,
     3                  NRQOFF ,GRPOFF ,SIGOFF ,TIMOFF ,ISGOFF ,ITRMAP ,
     4                  ITRGRD ,OPTDIP ,SIGDIP ,ISGNUT ,NANCAL ,ANTCAL ,
     5                  NUMCAL ,PRNCAL ,NFRCAL ,NPTCAL ,SIGCAL ,OPTGIM ,
     6                  POLGIM ,SIGGIM ,NAMGIM ,EPOGIM ,NANRAO ,ANTRAO ,
     7                  NUMRAO ,PRNRAO ,NFRRAO ,SIGRAO ,NEURAO ,NDIFF  ,
     8                  NUMSA1 ,NUMOB1 ,NUMMR1 ,NUMAM1 ,AMBSA1 ,AMBIE1 ,
     9                  AMBWL1 ,AMBIG1 ,AMBCL1 ,FILNUM ,TAECMP ,CLKHED ,
     1                  NCLKST ,NCLKSA ,NOINCLK,SECIPL ,CLKSYS ,TITLE  ,
     2                  TITLES ,NSASTC2,NUMSTC2,SIGSTC2,TIMSTC2,NARC2  ,
     3                  NUMSAT2,SOURCE2,TBOUND2,NAVNUM2,NSTCEF2,
     4                  RECTYP ,IRUNIT ,NORB2  ,NSTCEP2,SEQORB2,PREC2  ,
     5                  FRCTYP2,NANSPV ,NSASPV ,SATSPV ,GNRSPV ,NPTSPV ,
     6                  SIGSPV ,ANTTYP ,GNROFF ,NADMAX ,RAPZENMAX      ,
     7                  EDTLVL ,IPOLAR ,NSHD   ,SATSHD ,TIMSHD ,FLGSHD ,
     8                  IZEROD ,IQXX,   IPHSEP ,OPLOAD ,TIMISB ,OPTGSP ,
     9                  IREL2  ,nAllSat,allSatNum)
CC
CC NAME       :  PRIAPR
CC
CC PURPOSE    :  PRINT ALL RELEVANT OR REQUESTED A PRIORI INFORMATION
CC
CC PARAMETERS :
CC         IN :  MAXTYP: MAX. NUMBER OF PARAMETER TYPES       I*4
CC               NFTOT  : NUMBER OF FILES                     I*4
CC               HEADER(I),I=1,..,NFTOT: HEADER FILE NAMES    CH*(*)
CC               OBSFIL(I),I=1,..,NFTOT: OBSERV. FILE NAMES   CH*(*)
CC               NFRFIL(I),I=1,..,NFTOT: NUMBER OF FREQ.      I*4
CC                        TO BE PROCESSED
CC               ICARR(K,I),K=1,..,NFRFIL(I),I=1,..,NFTOT:    I*4
CC                        CARRIERS
CC               WINDOW(2,I),I=1,..,NFTOT: WINDOW FOR FILE    R*8
CC               NCAMP  : NUMBER OF CAMPAIGNS                 I*4
CC               CAMPGN(I),I=1,..,NCAMP: CAMPAIGNS            CH*16
CC               NSTAT  : NUMBER OF STATIONS                  I*4
CC               STNAME(I),I=1,..,NSTAT: STATION NAMES        CH*16
CC               STFIL(2,IF): STATION NUMBERS OF FILE I       I*4
CC               NSCAMP(I),I=1,..,NCAMP: NUMBER OF STATIONS   I*4
CC                        OF CAMPAIGN I
CC               STCAMP(K,I),K=1,..,NSCAMP(I),I=1,..,NCAMP:   I*4
CC                        STATION NUMBERS OF THE CAMPAIGN I   I*4
CC               MEATYP(I),I=1,..,NFTOT: MEASUREMENT TYPE     I*4
CC               NSATEL(I),I=1,..,NFTOT: NUMBER OF SATELLITES I*4
CC               CSESS(2,I),I=1,..,NFTOT: SESSION IDENTIFIERS CH*4
CC                        AND FILE IDENTIFIER
CC               IDELTT(I),I=1,..,NFTOT: OBSERVATION INTERVAL I*4
CC               TIMREF(I),I=1,..,NFTOT: REFERENCE EPOCH (MJD)R*8
CC               ICAMPN(I),I=1,..,NFTOT: CAMPAIGN NUMBER      I*4
CC               NFREQ(I),I=1,...,NFTOT: NUMBER OF FREQ.      I*4
CC                        AVAILABLE
CC               NEPOCH(I),I=1,..,NFTOT: NUMBER OF EPOCHS     I*4
CC               IRMARK(I),I=1,..,NFTOT: REMARK NUMBER        I*4
CC               ICLOCK(2,I),I=1,..,NFTOT: TYPE OF CLOCK      I*4
CC                        CORRECTION
CC               SATNUM(K,I),K=1,..,NSATEL(I),I=1,..,NFTOT:   I*4
CC                        SATELLITE NUMBERS
CC               NUMAMB(I),I=1,..,NFTOT: NUMBER OF AMBIGU.    I*4
CC               AMBSAT(J,I),J=1,..,NUMAMB(I),I=1,..,NFTOT:   I*4
CC                        AMBIGUITY SATELLITE NUMBERS
CC               AMBIEP(J,I),J=1,..,NUMAMB(I),I=1,..,NFTOT:   I*4
CC                        STARTING EPOCH NRS FOR AMBIGUITIES
CC               AMBIGU(L,K,I),L=1,..,NUMAMB(I), K=1,2,3,     R*8
CC                        I=1,..,NFTOT: AMBIGUITIES
CC               AMBWLF(K,J,I),K=1,..,J=1,2,I=1,..,NFTOT:     I*4
CC                        WAVELENGTH FACTORS
CC                        K: AMBIGUITY
CC                        J: FREQUENCY
CC                        I: FILE
CC               AMBCLS(L,K,I),L=1,..,NUMAMB(I), K=1,2,3,     I*4
CC                        I=1,..,NFTOT: AMBIGUITY CLUSTERS
CC               STANUM(I),I=1,..,NSTAT: EXTERNAL STATION NR. I*4
CC               NCENTR : NUMBER OF NOT-DIRECTLY OBS. CENTER  I*4
CC                        STATIONS
CC               ICENTR(I),I=1,..,NSTAT: INDEX OF CENTER STAT.I*4
CC                        FOR STATION I
CC               XSTAT(3,I),I=1,..,NSTAT: X,Y,Z-STATION COORD.R*8
CC               XSTELL(3,I),I=1,..,NSTAT: ELLIP. STAT. COORD.R*8
CC               XSTECC(3,I),I=1,..,NSTAT: STATION ECCENTR.   R*8
CC                        IN WGS - 84 SYSTEM (METERS)
CC               DATUM  : LOCAL GEODETIC DATUM                CH*16
CC               AELL   : SEMI-MAJOR AXIS OF ELLIPSOID        R*8
CC               BELL   : SEMI-MINOR AXIS OF ELLIPSOID        R*8
CC               DXELL(3): SHIFTS TO WGS-84                   R*8
CC               DRELL(3): ROTATIONS TO WGS-84                R*8
CC               SCELL  : SCALE FACTOR TO WGS-84              R*8
CC               NARC   : NUMBER OF SATELLITE ARCS            I*4
CC               ARCINT(I),I=1,..,NFTOT: ARC NUMBER ASSOCIAT. I*4
CC                        WITH FILE I
CC               NUMSAT(I),I=1,..,NARC: NUMBER OF SATELLITES  I*4
CC                        IN ARC I
CC               SOURCE(K,I),K=1,..,10,I=1,..,NARC: ORBIT     CH*1
CC                        SOURCE
CC               TBOUND(2,I),I=1,..,NARC: INTERVAL BOUNDARIES R*8
CC                        FOR ARC I (MJD)
CC               NAVNUM(I),I=1,..: SATELLITE NUMBERS FOR ALL  I*4
CC                        ARCS
CC               NPAR   : NUMBER OF PARAMETERS                I*4
CC               LOCQ(K,I),K=1,..,MAXLCQ, I=1,..,NPAR:        I*4
CC                        PARAMETER CHARACTERIZATION
CC               NSESS  : NUMBER OF SESSIONS                  I*4
CC               SESSID(I),I=1,..,NSESS: SESSION IDENTIFIERS  CH*4
CC               STITLE : SHORT INPUT TITLE                   CH*64
CC               PRIOPT(I),I=1,..: PRINT OPTIONS              I*4
CC               ISYNCR : =1 NO SYNCHRONIZATION ERR. APPLIED  I*4
CC                        =0 SYNCHRONIZATION ERRORS APPLIED
CC               ITROPO : TROPOSPHERE MODEL                   I*4
CC               IEXTRA : =0: USE MEASURED METEO VALUES (FILE)I*4
CC                        =1: USE EXTRAPOLATED VALUES
CC                        =2: USE ESTIMATED (GPSEST) VALUES
CC               AMBDEF(I),I=1,..,NFTOT: TYPE OF AMBIGUITY    I*4
CC                        INITIALIZATION
CC               STRAMB : (1): AMBIGUITY RESOLUTION STRATEGY  I*4(*)
CC                             =-1: AMBIGUITY PRE-ELIMINATION
CC                             = 0: NO AMBIGUITY RESOLUTION
CC                             = 1: ROUND-TO-NEAREST-INTEGER
CC                             = 2: GENERAL SEARCH
CC                             = 3: SIGMA-DEPENDENT
CC                             = 4: QUASI-IONOSPHERE-FREE
CC                             = 5: LAMBDA
CC                        (2): AMBIGUITY PRE-ELIMINATION
CC                             =-1: ONCE PER SESSION
CC                             = 0: EVERY EPOCH
CC                             = N: EVERY N SECONDS
CC                        (3): SELECTION OF GNSS
CC                             = 0: ALL
CC                             = 1: GPS
CC                             = 2: GLONASS
CC                             = 3: GALILEO
CC               AMBSAV(I),I=1,..,NFTOT: SAVE AMBIGUITIES     I*4
CC                         (SAVE=1, NO SAVE=0)
CC               SIGAMB : OPTIONS FOR SIGMA-DEPENDENT OR      R*8(4)
CC                        QIF AMBIGUITY RESOLUTION STRATEGY
CC                   IF STRAMB(1)=3 (SIGMA)
CC                          SIGAMB(1): AT LEAST 1 INTEGER WITHIN
CC                                     SIGAMB(1)*SIGMA ALLOWED
CC                          SIGAMB(2): MAXIMUM  SIGMA ALLOWED FOR
CC                                     THE AMBIGUITY WHICH SHOULD
CC                                     BE RESOLVED
CC                          SIGAMB(3): MINIMAL SIGMA USED IN TEST
CC                          SIGAMB(4): MAX. NUMBER OF AMBIGUITIES
CC                             TO BE SOLVED IN ONE ITERATION STEP
CC                          SIGAMB(5): GLONASS AMBIGUITY RESOLUTION
CC                                     BETWEEN DIFFERENT FREQUENCY
CC                                     CHANNELS:
CC                                     = 0: NEVER
CC                                     = 1: SAME REVEIVER TYPE
CC                                     = 2: SAME REVEIVER MODEL
CC                                     = 3: SAME REVEIVER GROUP
CC                                     =-1: ALWAYS
CC                    IF STRAMB(1)=4 (QIF)
CC                          SIGAMB(1): SEARCH WIDTH IN WIDE-LANE
CC                                     CYCLES
CC                          SIGAMB(2): MAXIMAL ALLOWED RMS ERROR
CC                                     OF NARROW-LANE AMBIGUITY
CC                                     (BET13*X1+BET23*X2) IN
CC                                     NARROW-LANE CYCLES
CC                          SIGAMB(3): MAXIMAL ALLOWED DISTANCE IN
CC                                 L1&L2 SPACE FROM GRID POINT WHICH IS
CC                                 SUPPOSED TO BE THE CORRECT SOLUTION
CC                                 (IN NARROW-LANE CYCLES)
CC                          SIGAMB(4): MAX. NUMBER OF AMBIGUITIY PAIRS
CC                             TO BE SOLVED IN ONE ITERATION STEP
CC                    IF STRAMB(1)=5 (LAMBDA)
CC                          SIGAMB(1): MAXIMUM ALLOWED RMS RATIO
CC                                     CONCERNING FIXED TO FLOAT
CC                          SIGAMB(2): MAXIMUM ALLOWED RMS OF UNIT
CC                                     WEIGHT FOR FIXED SOLUTION
CC                          SIGAMB(3): RESOLUTION MODE CONCERNING
CC                                     INVOLVED GNSS
CC                                     =1: GNSS BY GNSS
CC                                     =2: MIXED-GNSS
CC                                     =3: SEPARATE-GNSS
CC               ZENMAX : MAXIMUM ZENITH DISTANCE             R*8
CC               ZMXLEO : MAXIMUM ZENITH DISTANCE FOR LEO     R*8
CC               NSAMPL : SAMPLING RATE (SEC)                 I*4
CC                        1: OBSERVATIONS
CC                        2: RESUBSTITUTION OF EPOCH PARAMETERS
CC                        3: PREELIMINATE OF EPOCH PARAMETERS
CC               ISASYS : SATELLITE SYSTEM TO BE CONSIDERED   I*4
CC                        =0: ALL
CC                        =1: GPS
CC                        =2: GLONASS
CC               SIGAPR : A PRIORI SIGMA                      R*8
CC               ICOELV : MODEL FOR ELEV.-DEP. OBS. WEIGHTING I*4
CC                        =0: EQUAL WEIGHTING FOR ALL OBS.
CC                        >0: MODEL NUMBER (SEE SR WGTELV)
CC                        i=1: station i=2: LEOs; j=meatyp
CC               NORRES : RESIDUAL COMPUTATION                I*4
CC                        =1: REAL RESIDUALS SAVED
CC                        =2: L1-NORMALIZED RESIDUALS SAVED
CC                        =3: NORMALIZED WITH APRIORI WGT ONLY
CC               NESTSAT: #SAT FOR ORBIT DETERMINATION        I*4
CC               ESTSAT : PRN FOR THAT ORBITS ARE REQU.       I*4(*)
CC               NORB   : # ORBITAL PARAMETERS TO BE ESTIM.   I*4
CC               SEQORB : SEQUENCE FOR ORBITAL ELEMENTS       I*4(1)
CC               PREC   : A PRIORI ORBITAL PRECISIONS         R*8(1)
CC               NFIX   : # STATIONS HELD FIXED               I*4
CC               STFIX  : NUMBERS OF THE STATIONS HELD FIXED  I*4(1)
CC               NCLREQ : # CLOCK ERROR REQUESTS              I*4
CC               ISTCLK : STATION NUMBERS FOR CLOCK REQUESTS  I*4(1)
CC               ISACLK : SATELLITE NUMB. FOR SLR-TIME BIAS   I*4(1)
CC               NCLK   : # CLOCK PARAMETERS TO BE ESTIMATED  I*4(1)
CC                        NCLK(I)=1 : OFFSET ESTIMATED
CC                                    FOR STATION I
CC               IBIAS(I),I=1,2,..,NCLREQ: TYPE OF CLOCK BIAS I*4
CC                        0: STATION SPECIFIC
CC                        1: FREQUENCY SPECIFIC
CC                        2: SATELLITE SPECIFIC
CC                        3: SAT. SPECIFIC FOR NON-GPS
CC                        4: FREQUENCY SPECIFIC WITH POLYNOM
CC               CLKWGT : CLKWGT(J,I): A PRIORI WEIGHT FOR    R*8(2,1)
CC                        CLOCK ERROR J, STATION I (J=1,2)
CC                        IN SEC
CC               CLFRTO : TIME INTERVAL FOR CLOCK ERRORS      R*8(2,1)
CC                        CLFRTO(1,K): START OF THE INTERVAL
CC                        CLFRTO(2,K): END OF THE INTERVAL
CC                        IN JULIAN DATE FOR CLOCK REQUEST K
CC               NSTWGT : # STATIONS WITH A PRIORI WEIGHTS    I*4
CC                        FOR COORDINATES
CC               ISTWGT : NUMBERS OF THE STATION WITH A       I*4(1)
CC                        PRIORI WEIGHTS
CC               STWGT  : STWGT(K,I) A PRIORI WEIGHT FOR      R*8(3,1)
CC                        STATION I AND COORDINATE K
CC               NIOREQ : # IONOSPHERE REQUESTS               I*4
CC               IONMOD : NUMBER  OF IONOSPHERE MODEL TO BE   I*4(1)
CC                        IMPROVED FOR REQUEST I
CC               IONREQ : IONREQ(1,I): DEGREE OF DEVELOPMENT  I*4(3,1)
CC                                     IN LATITUDE
CC                        IONREQ(2,I): DEGREE OF DEVELOPMENT
CC                                     IN HOUR ANGLE
CC                        IONREQ(3,I): MAX. DEGREE OF MIXED
CC                                     COEFFICIENTS
CC               OPTDCB : OPTIONS FOR ESTIMATION OF           I*4(*)
CC                        DIFFERENTIAL CODE BIASES
CC                        (1): ESTIMATE DCBS FOR SATELLITES
CC                        (2): ESTIMATE DSBS FOR RECEIVERS
CC                        (3): REFERENCE SATELLITE NUMBER
CC                             = 0: CONSTRAIN ALL SAT
CC                             =-1: CONSTRAIN SUM OF ALL SAT
CC                        (4): SPECIAL OBSERVATION DATA SELECTION
CC                             = 0: NO
CC                             = 1: NIGHT-TIME
CC                             = 2: NON-ECLIPSING
CC                             = 3: NON-ECLIPSING ONLY FOR
CC                                  GPS BLOCK I, II, IIA
CC               SIGDCB : A PRIORI SIGMA FOR DCBS (IN NS)     R*8(*)
CC                        (1): REFERENCE SATELLITE BIASES
CC                        (2): RECEIVER BIASES
CC               NTRREQ : NUMBER OF LOCAL TROPOS. MODEL       I*4
CC                        REQUESTS
CC               NPARTR(I),I=1,2,..,NTRREQ: NUMBER OF PARA-   I*4
CC                        METERS IN REQUEST I
CC               TRPLIM(K,I),K=1,2, I=1,2,..,NTRREQ: TIME     R*8
CC                        INTERVAL OF VALIDITY (MJD) FOR
CC                        REQUEST I
CC               SIGTRP(I),I=1,2,..,NTRREQ: A PRIORI SIGMAS   R*8
CC                        FOR TROPOSPHERE PARAMETERS IN
CC                        M, M/(100M), M/(100M)**2, ...
CC               NTRSTA : NUMBER OF TROPOSPHERE REQUESTS FOR  I*4
CC                        INDIVIDUAL STATIONS
CC               STATRP(I),I=1,..,NTRSTA: STATION NUMBERS OF  I*4
CC                        THE TROPOSPHERE REQUESTS
CC               TRPLMS(2,I),I=1,..,NTRSTA: TIME INTERVAL OF  R*8
CC                        TROPOSPHERE REQUEST I
CC               SIGTRS(J,I),I=1,2,..,NTRSTA: A PRIORI SIGMA  R*8
CC                        IN M FOR INDIV. TROPOSPHERE PARAM.
CC                        J=1: NORTH (GRADIENT)
CC                        J=2: EAST (GRADIENT)
CC                        J=3: UP (ZENITH DELAY)
CC               ISGTRS(I),I=1,..,NTRSTA: TYPE OF A PRIORI    I*4
CC                        SIGMA (ABS=0, REL=1)
CC               POLPAR : NUMBER OF PARAMS. TO BE ESTIMATED   I*4(5)
CC                        (= POLYNOM DEGREE +1)
CC                        (1): XP, (2): YP, (3): DT
CC                        (4):EPS, (5):PSI
CC               NPOL   : NUMBER OF POLE PARAMETER SETS       I*4
CC               TPOL   : START AND END TIME OF INTERVAL FOR  R*8(2,*)
CC                        ONE SET OF PARAMETERS
CC                        1,2 :=BEGIN,END TIME, *:= 1..MAXPOL
CC               SIGPOL : A PRIORI SIGMA OF POLE PARAMETERS   R*8(5,*)
CC                        1-5 := XP,YP,DT,DE,DP *:= 1..MAXPOL
CC                        1,2,4,5 GIVEN IN MAS, 3 IN MSEC
CC               ISGPOL : EARTH ROTATION PARAMETER SIGMAS :   I*4(*)
CC                        =0 : APPLY FOR RELEVANT PARAMETER
CC                             ONLY THE ABSOLUTE CONSTRAINTS
CC                             GIVEN IN INPUT OPTION FILE
CC                        =1 : ENSURE CONTINUITY WITH RESPECT
CC                             TO PREVIOUS POLYNOMIAL (IN ADD.
CC                             TO ABSOLUTE CONSTRAINTS)
CC                        =4 : CONSTRAIN DRIFTS TO ZERO
CC                        =5 : ENSURE CONTINUITY WITH RESPECT
CC                             TO PREVIOUS POLYNOMIAL AND
CC                             CONSTRAIN DRIFTS TO ZERO
CC               CORSTR : CORRELATION STRATEGY                I*4
CC               DTSIM  : MAX. INTERVAL TO IDENTIFY EPOCH     R*8
CC                        (DAY FRACTION)
CC               NWGT   : NUMBER OF INTERVALS WITH WEIGHTED   I*4
CC                        SATELLITES
CC               SATWGT(I),I=1,..,NWGT: NUMBERS OF WEIGHTED   I*4
CC                        SATELLITES
CC               TIMWGT(K,I),K=1,2,I=1,..,NWGT: START AND END R*8
CC                        OF TIME INTERVAL WITH WEIGHTED
CC                        SATELLITES IN MODIFIED JULIAN DATE
CC               WGTWGT(I),I=1,..,NWGT: SATELLITE SPECIFIC    R*8
CC                        SIGMA
CC               AR2MOD : MODE OF PROCESSING FOR AMBIGUITY    I*4
CC                        RESOLUTION STRATEGY 2
CC                        =0 : RESOLVE ALL PRESENT AMBIGUITIES
CC                        =1 : BASELINE-WISE AMBIGUITY RESOLUTION
CC               AR2INF : INFORMATION FOR A.R., STRATEGY 2    R*8
CC                        (1) : SEARCH WIDTH IN UNITS OF STD DEV
CC                        (2) : MAX ALLOWED RMS(FIXED)/RMS(FLOAT)
CC                        (3) : MIN ALLOWED
CC                              RMS(2-ND AMB)/RMS(1-ST AMB)
CC                        (4) : MINIMUM SEARCH WIDTH FOR GEO-
CC                              METRY-FREE LC (IN L1 CYCLES)
CC               NSTCEP : NUMBER OF STOCHASTIC EPOCHS         I*4
CC               FRCTYP : STOCHASTIC FORCE TYPES FOR MAX      I*4(*)
CC                        3 FORCES PER EPOCH
CC                     (L)=1 FORCE = R : RADIAL
CC                     (L)=2 FORCE = S : NORMAL TO R IN ORB. PLANE
CC                     (L)=3 FORCE = W : NORMAL TO ORB PLANE
CC                     (L)=4 FORCE = DIRECTION SUN --> SATELLITE
CC                     (L)=5 FORCE = Y DIRECTION OF SPACE CRAFT
CC                     (L)=6 FORCE = X DIRECTION OF SPACE CRAFT
CC               NSASTC : NUMBER OF SATS WITH STOCH ORBITS    I*4
CC               NUMSTC1: CORRESPONDING SAT NUMBERS           I*4(*)
CC               NSTCEF : NUMBER OF STOCHASTIC EPOCHS/ARC     I*4(*)
CC               TIMSTC : STOCHASTIC EPOCHS (=INT BOUNDARIES) R*8(*,*,*,*)
CC               SIGSTC : A PRIORI SIGMAS FOR STOCH REQUESTS  R*8(*,*)
CC               NANOFF : NUMBER OF SATELLITE ANTENNA OFFSET  I*4
CC                        GROUPS TO BE ESTIMATED
CC               NSAOFF(I),I=1,..,NANOFF: NUMBER OF           I*4
CC                        SATELLITES BELONGING TO GROUP I
CC               SATOFF(J,I),J=1,..,NSAOFF(I),I=1,..,NANOFF:  I*4
CC                        SATELLITE NUMBERS OF EACH SAT. ANT.
CC                        OFFSET GROUP
CC               PAROFF(K),K=1,2,3: ANTENNA OFFSET COMPONENTS I*4
CC                        TO BE ESTIMATED (X,Y,Z IN SATELLITE
CC                        REFERENCE FRAME
CC                        =1: ESTIMATED
CC                        =0: NOT ESTIMATED
CC               NRQOFF : NUMBER OF SAT. ANT. OFFSET REQUESTS I*4
CC               GRPOFF(I),I=1,..,NRQOFF: ANTENNA GROUP FOR   I*4
CC                        REQUEST NUMBER I
CC               SIGOFF(J,I),J=1,2,3,I=1,..,NRQOFF: A PRIORI  R*8
CC                        SIGMAS FOR COMP. J AND ANT. REQ. I
CC               TIMOFF(J,I),J=1,2,I=1,..,NRQOFF: TIME INTER- R*8
CC                        VAL FOR ANTENNA REQUEST I
CC               ISGOFF(I),I=1,..,NRQOFF: TYPE OF SIGMA       I*4
CC                        =0: ABSOLUTE SIGMA
CC                        =1: SIGMA RELATIVE TO THE PREVIOUS
CC                            PARAMETER OF THE SAME GROUP
CC               ITRMAP : MAPPING FUNCTION FOR TROPOSP.EST.   I*4
CC                        =1: 1/COS(Z)
CC                        =2: HOPFIELD
CC                        =3,4: DRY/WET NIELL
CC                        =5,6: DRY/WET GMF
CC                        =7,8: DRY/WET VMF
CC               ITRGRD : (1): EST. OF TROPOSPHERIC GRADIENTS I*4(*)
CC                             =0: NO ESTIMATION
CC                             =1: TILTING
CC                             =2: LINEAR
CC                        (2): RATIO OF NUMBER OF ZENITH TO
CC                             GRADIENT PARAMETERS
CC               OPTDIP : OPTIONS FOR DIFF. ION. PARAMETERS   I*4(3)
CC                        (1): =0: NO DIFF. ION. PARAMETERS
CC                             =1: ONE PAR. PER EPOCH AND SAT.
CC                             =2: PARAMETERS EPOCH-WISE PRE-
CC                                 ELIMINATED
CC                        (2): ELIMINATION OF REF. ION. PAR.
CC                        (3): ELEV.-DEP. PAR. CONSTRAINING
CC               SIGDIP : A PRIORI SIGMAS FOR DIFF. ION. PAR. R*8(2)
CC                        (1): ABSOLUTE SIGMA
CC                        (2): RELATIVE SIGMA IN M/MIN**1/2
CC               ISGNUT : EARTH ORIENTATION PARAMETER SIGMAS :   I*4(*)
CC                        =0 : APPLY FOR RELEVANT PARAMETER
CC                             ONLY THE ABSOLUTE CONSTRAINTS
CC                             GIVEN IN INPUT OPTION FILE
CC                        =1 : ENSURE CONTINUITY WITH RESPECT
CC                             TO PREVIOUS POLYNOMIAL (IN ADD.
CC                             TO ABSOLUTE CONSTRAINTS)
CC                        =4 : CONSTRAIN DRIFTS TO ZERO
CC                        =5 : ENSURE CONTINUITY WITH RESPECT
CC                             TO PREVIOUS POLYNOMIAL AND
CC                             CONSTRAIN DRIFTS TO ZERO
CC               NANCAL : NUMBER OF RECEIVER ANTENNA PHASE    I*4
CC                        CENTER REQUESTS
CC               ANTCAL(J,I),J=1,2, I=1,..,NANCAL:            CH*20
CC                        RECEIVER (J=1) AND ANTENNA (J=2)
CC                        NAME FOR REQUEST I
CC               NUMCAL(2,I),I=1,..,NANCAL: ANTENNA NUMBERS   I*4
CC                        "FROM - TO" FOR REQUEST I
CC               PRNCAL(I),I=1,..,NANCAL: SAT.SYSTEM FOR REC. I*4
CC                        ANTENNA PHASE CENTER REQUEST I
CC               NFRCAL(I),I=1,..,NANCAL: FREQUENCY FOR REC.  I*4
CC                        ANTENNA PHASE CENTER REQUEST I
CC               NPTCAL(J,I),J=1,2, I=1,..,NANCAL: NUMBER OF  I*4
CC                        POINTS TO BE ESTIMATED IN ELEVATION
CC                        (J=1) AND AZIMUTH (J=2) DIRECTION
CC               SIGCAL(I),I=1,..,NANCAL: A PRIORI SIGMAS IN  R*8
CC                        METERS
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
CC               SIGGIM : ABSOLUTE SIGMA FOR                  R*8(*)
CC                        (1): ION. COEFFICIENTS (TECU)
CC                        (2): SINGLE-LAYER HEIGHT (M)
CC                        RELATIVE SIGMA FOR
CC                        (3): ION. COEFFICIENTS (TECU)
CC                        (4): SINGLE-LAYER HEIGHT (M)
CC               NAMGIM(I),I=1,..,OPTGIM(7): MODEL NUMBERS    CH*16(*)
CC               EPOGIM(I,J),I=1,2,J=1,..,OPTGIM(7): PERIODS  R*8(2,*)
CC                        OF VALIDITY / REF EPOCHS (MJD)
CC               NANRAO : NUMBER OF RECEIVER ANTENNA OFFSETS  I*4
CC               ANTRAO(J,I),J=1,2, I=1,..,NANRAO:            CH*20
CC                        RECEIVER (J=1) AND ANTENNA (J=2)
CC                        NAME FOR REQUEST I
CC               NUMRAO(2,I),I=1,..,NANRAO: ANTENNA NUMBERS   I*4
CC                        "FROM - TO" FOR REQUEST I
CC               PRNRAO(I),I=1,..,NANRAO: SAT.SYSTEM FOR      I*4
CC                        RECEIVER ANT. OFFSET REQUEST I
CC               NFRRAO(I),I=1,..,NANRAO: FREQUENCY FOR       I*4
CC                        RECEIVER ANT. OFFSET REQUEST I
CC               SIGRAO(J,I),J=1,2,I=1,..,NANRAO: A PRIORI    R*8
CC                        SIGMAS IN METERS
CC                        J=1: HORIZONTAL COMPONENTS
CC                        J=2: VERTICAL COMPONENT
CC               NEURAO(I),I=1,3: COMPONENTS TO BE ESTIMATED  I*4
CC                        (I=1: NORTH, I=2: EAST, I=3: UP)
CC                        =1: ESTIMATION
CC                        =0: NO ESTIMATION
CC               NDIFF(I):I=1,..,NFTOT: DIFFERENCE TYPE       I*4
CC                        NDIFF=0: ZERO DIFFERENCE
CC                        NDIFF=1: SINGLE DIFFERENCE
CC               NSASTC2: NUMBER OF LEOs WITH STOCH ORBITS    I*4
CC               NUMSTC2: CORRESPONDING LEO NUMBERS           I*4(*)
CC               SIGSTC2: A PRIORI SIGMAS FOR STOCH REQUESTS  R*8(*,*)
CC                        (LEO ONLY)
CC               TIMSTC2: STOCHASTIC EPOCHS (=INT BOUNDARIES) R*8(*,*,*,*)
CC                        (LEO ONLY)
CC               NARC2  : TOTAL NUMBER OF LEO ARCS            I*4
CC               NUMSAT2(I),I=1,2,...,NARC2: NUMBER OF        I*4
CC                        LEOs IN ARC I
CC               SOURCE2(K,I),K=1,..,10,I=1,2,..,NARC2        CH*1
CC               TBOUN2(K),K=1,2: START AND END OF            R*8
CC                        ARC "LEOARC" IN MJD
CC               NAVNUM2(L),L=1,2,... : NAV-NUMBERS FOR ALL   I*4
CC                        ARCS  (LEO ONLY)
CC               NSTCEF2: NUMBER OF STOCH. EPOCHS PER SAT.    I*4(*,*)
CC                        AND ARC (LEO ONLY)
CC               RECTYP(K,I),K=1,2,I=1,..,NFTOT: RECEIVER     CH*20
CC                        TYPES
CC               NORB2  : # LEO ORBITAL PARAM. TO BE ESTIM.   I*4
CC               NSTCEP2: NUMBER OF STOCH. FORCES PER EPOCH   I*4
CC               SEQORB2: SEQUENCE FOR LEO ORBITAL ELEMENTS   I*4(1)
CC               PREC2  : A PRIORI LEO ORBITAL PRECISIONS     R*8(1)
CC               FRCTYP2: LEO STOCHASTIC FORCE TYPES FOR MAX  I*4(*)
CC                        3 FORCES PER EPOCH
CC                     (L)=1 FORCE = R : RADIAL
CC                     (L)=2 FORCE = S : NORMAL TO R IN ORB. PLANE
CC                     (L)=3 FORCE = W : NORMAL TO ORB PLANE
CC                     (L)=4 FORCE = DIRECTION SUN --> SATELLITE
CC                     (L)=5 FORCE = Y DIRECTION OF SPACE CRAFT
CC                     (L)=6 FORCE = X DIRECTION OF SPACE CRAFT
CC               NANSPV : NUMBER OF SATELLITE ANTENNA PHASE   I*4
CC                        CENTER GROUPS TO BE ESTIMATED
CC               NSASPV(I),I=1,..,NANSPV: NUMBER OF           I*4
CC                        SATELLITES BELONGING TO SAT. ANT.
CC                        PHASE CENTER GROUP I
CC               SATSPV(J,I),J=1,..,NSASPV(I),I=1,..,NANSPV:  I*4
CC                        SATELLITE NUMBERS OF EACH SAT.
CC                        ANTENNA PHASE CENTER GROUP
CC               GNRSPV(I),I=1,..,NANSPV: USER-DEFINED NUMBER I*4
CC                        FOR SATELLITE ANTENNA PHASE CENTER
CC                        GROUP I
CC               NPTSPV(J,I),J=1,2, I=1,..,NANSPV: NUMBER OF  I*4
CC                        POINTS TO BE ESTIMATED IN ELEVATION
CC                        (J=1) AND AZIMUTH (J=2) DIRECTION
CC               SIGSPV(I),I=1,..,NANSPV: A PRIORI SIGMAS IN  R*8
CC                        METERS
CC               ANTTYP(K,I),K=1,2,I=1,..,NFTOT: ANTENNA      CH*20
CC                        TYPES
CC               GNROFF(I),I=1,..,NANOFF: USER-DEFINED NUMBER I*4
CC                        FOR SATELLITE ANTENNA OFFSET GROUP I
CC               NADMAX:  MAXIMUM NADIR ANGLE ALLOWED FOR     R*8
CC                        SAT. ANT. PATTERN ESTIMATION
CC               RAPZENMAX: MAXIMUM ZENITH ANGLE ALLOWED FOR  R*8
CC                        REC. ANT. PATTERN ESTIMATION
CC               EDTLVL:  O-C EDITING LEVEL                   R*8
CC               CLKHED : CLOCK HEADER INFORMATION            T_CLKHEAD
CC                          %NUMREF=0: FIX REF-CLOCKS
CC                          %NUMREF=2: SUM FOR REF-CLOCKS
CC               NCLKST : NUMBER OF EPOCH WISE STATION CLOCKS I*4
CC               NCLKSA : NUMBER OF EPOCH WISE SAT. CLOCKS    I*4
CC               CLKSYS : =1: ONE REC.CLK FOR EACH SAT.SYS    I*4
CC               NOINCLK  WHAT TO DO IF NO INPUT CLOCK:       I*4(3)
CC                        NOINCLK(1): REC FROM CLK RNX
CC                         -1: IGNORE CLOCK RINEX FILE
CC                          0: USE OBS. (REC FROM OBS-FILE)
CC                          1: USE OBS. (INTERPOL. CLK RNX)
CC                          2: SKIP OBS.
CC                        NOINCLK(2): SAT FROM CLK RNX
CC                         -1: IGNORE CLOCK RINEX FILE
CC                          0: TRY ALSO SAT CLK FILE
CC                          1: USE OBS. (INTERPOL. CLK RNX)
CC                          2: SKIP OBS.
CC                          3: USE OBS. (SAT CLK = ZERO)
CC                        NOINCLK(3): SAT FROM SAT CLK FILE
CC                          2: SKIP OBS.
CC                          3: USE OBS. (SAT CLK = ZERO)
CC               SECIPL : MAX INTERVAL FOR CLK INTERPOLATION  R*8
CC               IPOLAR : POLARIZATION EFFECT                 I*4
CC                        =0: NONE, =1: GEOM., =2: FULL
CC               OPLOAD : SCALING FACTORS FOR VIENNA GRID FILES   T_OPTLOAD(3)
CC                        1: ATMOSPHERIC NON-TIDAL LOADING
CC                        2: OCEAN NON-TIDAL LOADING
CC                        3: HYDROSTATIC PRESSURE LOADING
CC               OPTGSP : GNSS-SPECIFIC PARAMETER OPTIONS     T_OPTGSP
CC               IREL2  : FLAG FOR PERIODIC RELATIVISTIC J2   I*4
CC                        CORRECTION
CC               NALLSAT:     ! NUMBER OF ALL SATELLITES      I*4
CC               ALLSATNUM:   ! SATELLITE NUMBERS             I*4(*)
CC      LOCAL :  NUMSA1 : LOCAL SATELLITE NUMBERS             I*4(*)
CC               NUMOB1 : LOCAL NUMBER OF OBSERVATIONS        I*4(*,2)
CC               NUMMR1 : LOCAL NUMBER OF MARKED OBSERV.      I*4(*,2)
CC               NUMAM1 : NUMBER OF AMBIGU.    I*4
CC               AMBSA1(J),J=1,..,NUMAMB : LOCAL AMBIGUITY    I*4
CC                        SATELLITE NUMBERS
CC               AMBIE1(J),J=1,..,NUMAMB : LOCAL STARTING     I*4
CC                        EPOCH NRS FOR AMBIGUITIES
CC               AMBIG1(L,K),L=1,..,NUMAMB(I), K=1,2,3 :      R*8
CC                        LOCAL AMBIGUITIES
CC               AMBWL1(K,J),K=1,..,J=1,2 : LOCAL WAVELENGTH  I*4
CC                        FACTORS :  K ... AMBIGUITY
CC                                   J ... FREQUENCY
CC                                   I ... FILE
CC               AMBCL1(L,K),L=1,..,NUMAMB, K=1,2,3 : LOCAL   I*4
CC                         AMBIGUITY CLUSTERS
CC               FILNUM(I),I=1,.. : FILE NUMBERS OF ONE SESS. I*4
CC               TAECMP(2,I),I=1,..,NCAMP: OBSERVATION INTER- R*8
CC                        VAL FOR CAMPAIGN I
CC               IZEROD : FLAG TO IDENTIFY CODE OR PHASE      I*4
CC                        ONLY (=2) AND CODE+PHASE (=1) SOLUTION
CC               IQXX   : FLAG FOR VAR/COV COMP.(YES=1,NO=0)  I*4
CC               IPHSEP : ONLY PHASE FOR RESUBST OF EPO-PARAM. I*4
CC        OUT :  TITLE  : TITLE FOR OUTPUT FILES              CH*80
CC               TITLES(I),I=1,2: TITLE LINES                 CH*132
CC               NSHD   : NUMBER OF SATELLITES IN SHADOW      I*4
CC               SATSHD : PRN NUMBERS IN SHADOW               I*4(*)
CC               TIMSHD : EXCLUDED TIME INTERVAL              R*8(2,*)
CC               FLGSHD : FLAG OBSERV. (=2) OR NOT (=0)       I*4(*)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, S. FANKHAUSER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/07 16:51
CC
CC CHANGES    :  27-MAY-91 : ??: DON'T PRINT TRAILING BLANKS
CC               27-SEP-91 : ??: DESCRIPTION OF PARAMETER "NSAMPL" CHANGED
CC               10-DEC-91 : ??: WRONG POSITION IN TEXT FILE IF MORE THAN
CC                               ONE CAMPAIGN
CC               23-DEC-91 : ??: VERSION NUMBER CHANGED TO 3.3
CC               13-FEB-92 : ??: CHANGES DUE TO ERP-ESTIMATION
CC               30-MAY-92 : ??: ABSOLUTE AND RELATIVE TROPOS. SIGMAS
CC               27-JUL-92 : ??: PRINT GENERAL FILE NAMES (SR PRFLNA)
CC               28-JUL-92 : ??: PRINT SAT. SPEC. WEIGHTS AND SAT.PROBLEMS
CC               31-JUL-92 : ??: OPTION FOR "FARA" (AR2MOD,AR2INF)
CC               02-AUG-92 : ??: CHANGE DESCRIPTION OF "POLPAR"
CC               04-AUG-92 : LM: NEW OPTION SIGAMB(4)
CC               10-SEP-92 : ??: NEW CALL TO "PRFLNA"
CC               11-JAN-93 : ??: CHANGE VERSION NUMBER TO 3.4
CC               22-MAR-93 : ??: CONTINUITY IN POLE POLYNOMIALS (PARAMETER
CC                               "ISGPOL") AND STOCHASTIC PARAMETERS
CC               03-APR-93 : ??: SATELLITE ANTENNA OFFSET PARAMETERS
CC               28-APR-93 : ??: REDUCE NUMBER OF CONI. LINES IN CALL
CC               26-APR-93 : ??: PRE-ELIMINATION OF ANY PARAMETER TYPE
CC               09-NOV-93 : MR: CHANGE CALL TO "PRIOBS"
CC               21-DEC-93 : MR: CHANGE CALL TO "PRIFIL","PRISAT",
CC                               "PRIARC","PRIOSC"
CC               28-DEC-93 : MR: TIME WINDOW FOR SAT.ANT. OFFSETS
CC               13-APR-94 : LM: QIF STRATEGY
CC               14-APR-94 : SS: DIFFERENTIAL IONOSPHERE PARAMETERS
CC               19-APR-94 : RW: CPO-MODEL INCLUDED
CC               25-JUL-94 : MR: CHANGE VERSION 3.4 TO 3.5
CC               25-JUL-94 : MR: PRE-ELIMINATION STATISTICS SIMPLIFIED
CC               27-JUL-94 : MR: NEW TABLE OF CONTENTS
CC               29-JUL-94 : MR: TROPOSPHERE MAPPING FUNCTION
CC               12-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               06-NOV-94 : MR: ANTENNA PHASE CENTER PARAMETERS
CC               06-JUN-95 : SS: GLOBAL IONOSPHERE MODEL PARAMETERS
CC               14-AUG-95 : LM: STRAMB=ARRAY
CC               07-OCT-95 : MR: CHANGE VERSION TO 4.0
CC               05-DEC-95 : SS: NEW IONOSPHERE MODEL (TYPE 2)
CC               05-DEC-95 : SS: "IONO" REMOVED (SEE SR CHKION)
CC               20-DEC-95 : SS: DECLARE "NUMGIM" AS CH*7
CC               26-MAR-96 : MR: REMOVE RECEIVER HEIGHT PARAMETERS,
CC                               REMOVE "RECHGT" FROM PARAMETER LIST
CC                               AND FROM CALL PRIREC; ADD SR PRIPHC;
CC                               CORRECT CALL PRICON (ONLY CONSTANTS)
CC               23-APR-96 : MR: NEW VERSION 4.1
CC               06-MAY-96 : TS: ADDED NDIFF FOR ZERO-DIFFERENCES
CC               14-JAN-97 : MR: "OBSFIL" ADDED TO PARAMETER LIST
CC               30-JAN-97 : MR: ELEV.-DEP. OBS. WEIGHTING
CC               08-APR-97 : SS: NIELL MAPPING, TROPOSPHERE GRADIENTS
CC               05-AUG-97 : SS: ELEV.-DEP. SIP CONSTRAINING
CC               11-AUG-97 : SS: NEW OPTION "STRAMB(2)"
CC               14-AUG-97 : SS: DIFFERENTIAL CODE BIASES
CC               22-SEP-97 : SS: 5-DEG BIN OBSERVATION STATISTICS
CC               08-OCT-97 : MR: RATIO ZENITH/GRADIENT PARAMETERS
CC               08-OCT-97 : SS: RESIDUAL NORMALIZATION
CC               18-NOV-97 : SS: INCLUDE "PGMVER"
CC               26-JAN-98 : SS: STATION-SPECIFIC GIMS
CC               26-JAN-98 : SS: RELATIVE SIGMA FOR GIMS
CC               29-APR-98 : SS: DTEC LC
CC               18-MAY-01 : DS: NEW PARAMETERS FOR LEO ORBIT:
CC                               NSASTC2,SIGSTC2,TIMSTC2,
CC                               NARC2,ARCINT2,NUMSAT2,SOURCE2,
CC                               TBOUND2,NAVNUM2,NSTCEF2
CC               06-AUG-01 : DS: NEW PARAMETER: RECTYP
CC               05-SEP-01 : HU: Interface for prflna added
CC               07-SEP-01 : RD: New call for SR prista, prists
CC               14-FEB-02 : SS: ADJUST "TAECMP" WRT "WINDOW"
CC               07-MAY-02 : SS: DCB UPDATE
CC               23-JUN-02 : DS: SEPARATION OF LEO AND GPS ORBIT
CC                               PARAMETERISATION
CC               24-JUN-02 : DS: LEO ELEV.DEP.WEIGHT. FLAG IN ICOELV(2)
CC               30-JUL-02 : HU: USE INTERFACE FOR PRIORP
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               13-NOV-02 : RS: SATELLITE ANTENNA PHASE CENTER VARIATIONS
CC               13-NOV-02 : HB: INTERFACE TO PRITRO ADDED
CC               10-DEC-02 : CU: PRINT TITLE "4. STATIONS", NEW PARAMETER
CC                               WGTFILE (A PRIORI CRD SIGMA FILE)
CC               18-FEB-03 : HU: USE PGMVER FROM M_BERN
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE
CC               28-MAR-03 : RD: REPORT CLOCK INPUT OPTIONS IN SR PRIREC
CC               24-APR-03 : RS: CORRECT SKELETON FILE SUBSTITUTES
CC               30-APR-03 : SS: SATELLITE SYSTEM SELECTION
CC               19-MAY-03 : RD: INIT WINDOW (/0D0,1D20/), INDEPENDENTLY
CC               22-JUL-03 : RD: NORMALIZED ON APRIORI WEIGHTS ONLY
CC               11-AUG-03 : RS: NEW PARAMETER ANTTYP, ADD ANTTYP TO CALL
CC                               OF SR PRIFIL
CC               08-SEP-03 : HU: ANTNAM, RECNAM CHR16 -> CHR20,
CC                               FILENAMES CHR(*)
CC               10-SEP-03 : HU: MERGED
CC               13-NOV-03 : RS: ADD GNROFF, NADMAX AND RAPZENMAX, CHANGE
CC                               CALL OF PRIORP, GRPSPV -> GNRSPV
CC               24-NOV-03 : HU: ADDITIONAL ARGUMENT FOR PRISTA
CC               10-DEC-03 : AJ: ADDITIONAL COMPONENT FOR TIMSTC,TIMSTC2
CC               19-JAN-04 : SS/MM: REVISION OF GPSEST INPUT PANELS
CC               26-JAN-04 : HU: TFRLST INSTEAD OF TFIRST IN PRIOSC/PRIOSF
CC               27-JAN-04 : HU: EXCLUSION OF ECLIPSING SATELLITE OBSERVATIONS
CC               26-MAY-05 : RD: USE DIMENSION MAXSGR FOR SATOFF AND SATSPV
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               18-SEP-06 : HU: POLARIZATION EFFECT OPTION
CC               16-Oct-06 : RD: MANUAL SELECTION OF SAT. FOR ORBIT DETERM.
CC               03-NOV-06 : RD: NON-ECLIPSING ONLY FOR OLD GPS SAT.
CC               17-JUN-07 : RD: ONLY PHASE FOR RESUBSTITUTION OF EPO-PARAM.
CC               18-JUN-07 : RD: ICOELV INDIV. FOR EACH MEATYP
CC               30-JUN-08 : RD: VMF ADDED
CC               05-NOV-08 : AJ: PASS ZMXLEO TO SR PRIWDW
CC               04-MAY-09 : RD: SCALING OF LOADING MODELS ADDED
CC               09-MAY-09 : RD: SAT/FRQ-SPECIFIC RECEIVER CLOCK BIASES
CC               09-MAY-09 : RD: SEPERATE RECEIVER CLOCKS FOR GPS/GLONASS
CC               27-MAY-09 : RD: SPECIAL SAMPLING FOR RESUBST. OF EPOCH PARAM.
CC               29-MAY-09 : RD: INPUT CLOCKS ALSO FROM INPUT CLK RNX FILE
CC               21-SEP-09 : RD: ECLIPSING FLAG FOR CLOCK RESULTS ADDED
CC               08-SEP-10 : RD: MERGE SLR-TIME BIAS OPTION
CC               26-OCT-10 : CR: ADD SWITCH (IREL2) FOR PERIODIC RELATIVISTIC
CC                               J2-CORRECTION
CC               06-DEC-10 : MM: GNSS-SPECIFIC PARAMETERS
CC               04-Feb-11 : SL: call PRISTA with DATUM
CC               14-FEB-11 : RD: REMOVE MAXSTA-COMMON (UNUSED)
CC               16-FEB-11 : SS: SIGAMB(5) FOR GLONASS AMBIGUITY RESOLUTION
CC               17-FEB-11 : SS: STRAMB(3) FOR SELECTION OF GNSS
CC               17-FEB-11 : SS: LAMBDA AMBIGUITY RESOLUTION STRATEGY
CC               17-NOV-11 : HB: SHADOW TRANSITS ONLY FOR GNSS SATELLITES
CC               26-MAR-12 : RD: REMOVE UNUSED VARIABLES FROM PRIPOL
CC               26-MAR-12 : RD: REMOVE UNUSED VARIABLES
CC               15-OCT-12 : RD/SL: BERNESE GPS->GNSS SOFTWARE
CC               16-JUL-13 : RD: NEW CALL OF PRISHD
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: LFNPRT, PGMVER, FILENAMELENGTH
      USE D_CLKRNX, ONLY: T_CLKHEAD
      USE d_const,  ONLY: DATE, TIME
      USE p_gpsest, ONLY: t_optLoad,t_optGsp
      USE s_pritro
      USE s_pricmp
      USE s_prflna
      USE s_pricon
      USE s_pripol
      USE s_priorp
      USE s_pridat
      USE s_prista
      USE s_priphc
      USE s_priwdw
      USE s_priion
      USE s_priarc
      USE s_prisat
      USE s_prifil
      USE s_prispv
      USE s_prists
      USE s_priosc
      USE s_priecc
      USE s_priosf
      USE s_priamb
      USE s_prirec
      USE s_priobs
      USE s_pripar
      USE s_pricrx
      USE f_lengt1
      USE s_gtflna
      USE s_prigen
      USE s_prishd
      USE s_prigrd
      USE s_prigsp
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IC     , ICAMP  , ICH1   , ICH2   , ICMP   , IEXTRA ,
     1          IF     , IRCLEO , ISASYS , ISTA   , ISYNCR , ITRMAP ,
     2          ITROPO , MAXCMP , MAXTYP , MXCAMB , MXCFRQ , NALLSAT,
     3          MXCLCQ , MXCSAT , MXCSGR , MXCSHD , MXCSTC ,
     4          NANCAL , NANOFF , NANRAO , NARC   , NARC2  , NCAMP  ,
     5          NCENTR , NCLKSA , NCLKST , NCLREQ , NCOL   , NFIX   ,
     6          NFTOT  , NIOREQ , NORB   , NORB2  , NORRES ,
     7          NPAR   , NPOL   , NRQOFF , NSASTC ,
     8          NSASTC2, NSESS  , NSHD   , NSTAT  , NSTCEP , NSTCEP2,
     9          NSTWGT , NTRREQ , NTRSTA , NUMAM1 , NWGT   , NESTSAT,
     1          IPOLAR , IZEROD , IQXX   , IPHSEP , CLKSYS , IREL2
C
      REAL*8    AELL   , BELL   , DTSIM  , EDTLVL , SCELL  , SIGAPR ,
     1          TLAST  , ZENMAX , ZMXLEO , SECIPL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXCMP=10)
C
      TYPE(T_CLKHEAD) CLKHED
      TYPE(t_optLoad), DIMENSION(:):: opLoad
      TYPE(t_optGsp) optGsp
C
      CHARACTER(LEN=fileNameLength)   :: FILORB
      CHARACTER*132 TITLES(2)
      CHARACTER*80  TITLE
      CHARACTER*64  STITLE
      CHARACTER*(*) HEADER(*),OBSFIL(*)
      CHARACTER*16  CAMPGN(*),STNAME(*),DATUM,NAMGIM(*)
      CHARACTER*20  ANTCAL(2,*),ANTRAO(2,*),RECTYP(2,*),ANTTYP(2,*)
      CHARACTER*6   MXNSAT,MXNFRQ,MXNAMB,MXNLCQ,MXNSTC,MXNSHD
      CHARACTER*6   MXNSGR
      CHARACTER*4   SESSID(*),CSESS(2,*)
      CHARACTER*1   SOURCE(10,*),SOURCE2(10,*)
      CHARACTER*80  WGTFILE
C
      REAL*8    WINDOW(2,*),TIMREF(*),TIMISB(3,*)
      REAL*8    AMBIGU(MXCAMB,3,*),XSTAT(3,*),XSTELL(3,*)
      REAL*8    XSTECC(3,*),DXELL(*),DRELL(*),TBOUND(2,*)
      REAL*8    TBOUND2(2,*)
      REAL*8    AMBIG1(MXCAMB,3)
      REAL*8    PREC(*),CLKWGT(2,*),CLFRTO(2,*),STWGT(3,*),TRPLIM(2,*)
      REAL*8    SIGTRP(*),TRPLMS(2,*),SIGTRS(3,*),TFRLST(2),TAECMP(2,*)
      REAL*8    SIGAMB(*),AR2INF(*),PREC2(*)
      REAL*8    TPOL(2,*),SIGPOL(5,*),TIMWGT(2,*),WGTWGT(*)
      REAL*8    TIMSTC(3,MXCSTC,MXCSAT,*),SIGSTC(3,*)
      REAL*8    TIMOFF(2,*),SIGOFF(3,*),SIGDIP(*),SIGCAL(*)
      REAL*8    POLGIM(3,*),SIGGIM(*),EPOGIM(2,*)
      REAL*8    SIGRAO(2,*),SIGDCB(*),SIGSPV(*)
      REAL*8    TIMSTC2(3,MXCSTC,MXCSAT,*),SIGSTC2(3,*)
      REAL*8    TIMSHD(2,MXCSHD)
      REAL*8    NADMAX,RAPZENMAX
C
      INTEGER*4 PRIOPT(*),FILNUM(*),NEPFLG(*),IRUNIT(2,*)
      INTEGER*4 NFRFIL(*),ICARR(MXCFRQ,*),STFIL(2,*),NSCAMP(*)
      INTEGER*4 STCAMP(:,:),MEATYP(*),NSATEL(*)
      INTEGER*4 IDELTT(*),ICAMPN(*),NEPOCH(*)
      INTEGER*4 IRMARK(*),ICLOCK(2,*),SATNUM(MXCSAT,*)
      INTEGER*4 NUMAMB(*),AMBSAT(MXCAMB,*),AMBIEP(MXCAMB,*)
      INTEGER*4 AMBWLF(MXCAMB,2,*),AMBCLS(MXCAMB,3,*)
      INTEGER*4 AMBSA1(MXCAMB),AMBIE1(MXCAMB)
      INTEGER*4 AMBWL1(MXCAMB,2),AMBCL1(MXCAMB,3)
      INTEGER*4 ICENTR(*),ARCINT(*),NUMSAT(*),NAVNUM(*),LOCQ(MXCLCQ,*)
      INTEGER*4 NUMSAT2(*),NAVNUM2(*),ALLSATNUM(*)
      INTEGER*4 NFREQ(*)
      INTEGER*4 NUMSA1(*),NUMOB1(MXCSAT,2),NUMMR1(MXCSAT,2)
      INTEGER*4 SEQORB(*),STFIX(*),ISTCLK(*),ISACLK(*),NCLK(*),IBIAS(*)
      INTEGER*4 SEQORB2(*),ICOELV(2,*),STANUM(*)
      INTEGER*4 ISTWGT(*),IONMOD(*),IONREQ(3,*)
      INTEGER*4 NPARTR(*),STATRP(*),AMBDEF(*),STRAMB(*),AMBSAV(*),CORSTR
      INTEGER*4 POLPAR(*),ISGPOL(*),ISGNUT(*)
      INTEGER*4 ISGTRS(*),SATWGT(*),AR2MOD
      INTEGER*4 FRCTYP(*),NUMSTC1(*),NSTCEF(MXCSAT,*),FRCTYP2(*)
      INTEGER*4 NUMSTC2(*),NSTCEF2(MXCSAT,*)
      INTEGER*4 NSAOFF(*),SATOFF(MXCSGR,*),PAROFF(3)
      INTEGER*4 GRPOFF(*),GNROFF(*),ISGOFF(*),OPTDIP(*)
      INTEGER*4 NUMCAL(2,*),PRNCAL(*),NFRCAL(*),NPTCAL(2,*)
      INTEGER*4 NUMRAO(2,*),PRNRAO(*),NFRRAO(*),NEURAO(3)
      INTEGER*4 OPTGIM(*),NDIFF(*),OPTDCB(*),ITRGRD(*),NSAMPL(3)
      INTEGER*4 NANSPV,NSASPV(*),SATSPV(MXCSGR,*),GNRSPV(*),NPTSPV(2,*)
      INTEGER*4 SATSHD(*),ESTSAT(*),NOINCLK(3),FLGSHD(*)
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMSTC/MXCSTC,MXNSTC
      COMMON/MCMSHD/MXCSHD,MXNSHD
      COMMON/MCMSGR/MXCSGR,MXNSGR
C
C FIRST AND LAST OBSERVATION EPOCH OF ALL FILES AND OF EACH CAMPAIGN
C ------------------------------------------------------------------
      TFRLST(1)=1.D20
      TFRLST(2)=0.D0
      DO ICMP=1,NCAMP
        TAECMP(1,ICMP)=1.D20
        TAECMP(2,ICMP)=0.D0
      ENDDO
C
      DO IF=1,NFTOT
        ICMP=ICAMPN(IF)
        IF (WINDOW(1,IF).EQ.0.D0 .OR. WINDOW(1,IF).LT.TIMREF(IF)) THEN
          IF(TIMREF(IF).LT.TFRLST(1))      TFRLST(1)=TIMREF(IF)
          IF(TIMREF(IF).LT.TAECMP(1,ICMP)) TAECMP(1,ICMP)=TIMREF(IF)
        ELSE
          IF(TIMREF(IF).LT.TFRLST(1))      TFRLST(1)=WINDOW(1,IF)
          IF(TIMREF(IF).LT.TAECMP(1,ICMP)) TAECMP(1,ICMP)=WINDOW(1,IF)
        ENDIF
C
        TLAST=TIMREF(IF)+(NEPOCH(IF)-1)*IDELTT(IF)/86400.D0
        IF (WINDOW(2,IF).EQ.1.D20 .OR. WINDOW(2,IF).GT.TLAST) THEN
          IF(TLAST.GT.TFRLST(2))      TFRLST(2)=TLAST
          IF(TLAST.GT.TAECMP(2,ICMP)) TAECMP(2,ICMP)=TLAST
        ELSE
          IF(TLAST.GT.TFRLST(2))      TFRLST(2)=WINDOW(2,IF)
          IF(TLAST.GT.TAECMP(2,ICMP)) TAECMP(2,ICMP)=WINDOW(2,IF)
        ENDIF
      ENDDO
C
C TITLE FOR OUTPUT FILES
C ----------------------
      TITLE=' '
      TITLE(1:64)=STITLE
      TITLE(66:74)=DATE
      TITLE(76:80)=TIME
C
C OUTPUT TITLE LINES
C ------------------
      TITLES(1)='1'
      TITLES(2)=' '
C
C CAMPAIGN NAMES
      ICH1=2
      DO 30 ICAMP=1,NCAMP
        DO 10 IC=16,1,-1
          IF(CAMPGN(ICAMP)(IC:IC).NE.' ') GOTO 20
10      CONTINUE
20      ICH2=ICH1+IC-1
        TITLES(1)(ICH1:ICH2)=CAMPGN(ICAMP)(1:IC)
        IF(ICAMP.NE.NCAMP) THEN
          TITLES(1)(ICH2+1:ICH2+2)=', '
          ICH1=ICH2+3
        ENDIF
30    CONTINUE
      TITLES(1)(100:117)='PROGRAM GPSEST    '
      TITLES(1)(118:126)=DATE
      TITLES(1)(128:132)=TIME
      TITLES(2)(2:65)=STITLE
      TITLES(2)(100:132)='BERNESE GNSS SOFTWARE VERSION '//PGMVER
C
C WRITE TABLE OF CONTENTS
C -----------------------
      WRITE(LFNPRT,1001) TITLES(1)(1:LENGT1(TITLES(1))),
     1                   TITLES(2)(1:LENGT1(TITLES(2)))
1001  FORMAT(//,A,/,A,/,' ',131('-'),//)
C
      WRITE(LFNPRT,"(
     1     ' TABLE OF CONTENTS'
     2  ,/,' -----------------'
     3  ,/,' '
     4  ,/,'  1. CAMPAIGNS'
     5  ,/,'  2. OBSERVATION FILES'
     6  ,/,'  3. GENERAL OPTIONS'
     7  ,/,'  4. STATIONS'
     8  ,/,'  5. SATELLITE ORBITS'
     9  ,/,'  6. ATMOSPHERE'
     .  ,/,'  7. CLOCK PARAMETERS'
     1  ,/,'  8. POLE COORDINATES AND TIME INFORMATION'
     2  ,/,'  9. ANTENNA PHASE CENTERS'
     3  ,/,' 10. CONSTANTS'
     4  ,/,' 11. PARAMETER CHARACTERIZATION LIST'
     5  ,/,' 12. TEST OUTPUT'
     6  ,/,' 13. RESULTS (PART 1)'
     7  ,/,' 14. RESULTS (PART 2)'
     8  ,/,1X)")
C
C WRITE LIST OF GENERAL FILES
C ---------------------------
      WRITE(LFNPRT,1001) TITLES(1)(1:LENGT1(TITLES(1))),
     1                   TITLES(2)(1:LENGT1(TITLES(2)))
C
      NCOL=131
C
      CALL PRFLNA(NCOL)
C
C WRITE CAMPAIGN LIST
C -------------------
      CALL PRICMP(TITLES,NCAMP,CAMPGN,STANUM,STNAME,NSCAMP,STCAMP)
C
C WRITE FILE INFORMATION
C ----------------------
      WRITE(LFNPRT,"(
     1     ' '
     2  ,/,' 2. OBSERVATION FILES'
     3  ,/,' --------------------'
     4  ,/,1X)")
C
C LOOP OVER ALL CAMPAIGNS
C -----------------------
      DO 300 ICAMP=1,NCAMP
C
C TITLE LINES
        IF(ICAMP.NE.1) THEN
          WRITE(LFNPRT,1001) TITLES(1)(1:LENGT1(TITLES(1))),
     1                       TITLES(2)(1:LENGT1(TITLES(2)))
        ENDIF
        WRITE(LFNPRT,1004) CAMPGN(ICAMP)
1004    FORMAT(' ',131('-'),/,' ',A16,/,' ',131('-'),/)
C
C MAIN CHARACTERISTICS
C --------------------
        CALL PRIFIL(NFTOT ,ICAMPN,ICAMP ,MEATYP,NFREQ ,NEPOCH,NSATEL,
     1              CSESS ,ICLOCK,IDELTT,TIMREF,IRMARK,NEPFLG,AMBDEF,
     2              AMBSAV,NUMAMB,AMBCLS,NFRFIL,ICARR ,STNAME,STFIL ,
     3              ARCINT,NDIFF ,HEADER,OBSFIL,AMBSA1,RECTYP,ANTTYP)
C
C SATELLITE INFORMATION
C ---------------------
        CALL PRISAT(NFTOT,ICAMPN,ICAMP,NSATEL,SATNUM,NUMSA1)
C
C OBSERVATION SELECTION
C ---------------------
        CALL PRIWDW(NFTOT,ICAMPN,ICAMP,NSAMPL,ZENMAX,ZMXLEO,ISASYS,
     1              OPTDCB(4),EDTLVL,WINDOW)
C
C OBSERVATION INFORMATION
C -----------------------
        CALL PRIOBS(PRIOPT(1),TITLES,NFTOT,HEADER,CAMPGN(ICAMP),NUMSA1,
     1              NUMOB1,NUMMR1,NUMAM1,AMBSA1,AMBIE1,AMBWL1,AMBIG1,
     2              AMBCL1)
C
C POSITIONING ECCENTRICITIES AND RECEIVER INFORMATION
C ---------------------------------------------------
        CALL PRIECC(PRIOPT(2),TITLES,NFTOT,HEADER,CAMPGN(ICAMP),NUMSA1,
     1              NUMOB1,NUMMR1,NUMAM1,AMBSA1,AMBIE1,AMBWL1,AMBIG1,
     2              AMBCL1)
C
C AMBIGUITIES
C -----------
        CALL PRIAMB(PRIOPT(4),TITLES,NFTOT,ICAMP,ICAMPN,NFREQ,
     1              NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS)
300   CONTINUE
C
C GENERAL OPTIONS
C ---------------
      CALL PRIGEN(TITLES,SIGAPR,ICOELV,NORRES,CORSTR,DTSIM ,
     1            NFTOT ,CSESS ,NSESS ,SESSID,FILNUM,STRAMB,
     2            SIGAMB,AR2MOD,AR2INF,ISYNCR,IPOLAR,IZEROD,
     3            IQXX  ,IPHSEP,ISASYS,IREL2)
C
C STATION INFORMATION:
C -------------------
C LOCAL GEODETIC DATUM
C --------------------
      WRITE(LFNPRT,'(//,2(A,/))')
     1 ' 4. STATIONS',
     2 ' -----------'
      CALL PRIDAT(DATUM,AELL,BELL,DXELL,DRELL,SCELL)
C
C STATION COORDINATES AND ECCENTRICITIES
C --------------------------------------
      CALL PRISTA(NSTAT,STNAME,STANUM,XSTAT,XSTELL,XSTECC,
     1            NCENTR,ICENTR,NFIX,STFIX,0,STFIX,TIMREF(1),DATUM)
C
C A PRIORI SIGMAS FOR STATION COORDINATES
C ---------------------------------------
      CALL PRISTS(NSTWGT,ISTWGT,STWGT,0,ISTWGT,STWGT,STNAME,STANUM,
     1            WGTFILE,1)
C
C GNSS-SPECIFIC PARAMETERS
C ------------------------
      CALL PRIGSP(NSTAT,STNAME,NPAR,LOCQ,OPTGSP)
C
C SATELLITE ORBIT INFORMATION:
C ---------------------------
C ARC CHARACTERISTICS
C -------------------
      CALL PRIARC(TITLES,NARC,NUMSAT,NAVNUM,TBOUND,SOURCE,NUMSA1)
      IF (NARC2.GT.0) THEN
        CALL PRIARC(TITLES,NARC2,NUMSAT2,NAVNUM2,TBOUND2,
     1                SOURCE2,NUMSA1)
      END IF
C
C OSCULATING ELEMENTS
C -------------------
      CALL PRIOSC(NARC,NUMSAT,NAVNUM,TBOUND,TFRLST(1),NUMSA1)
      CALL GTFLNA(0,'LEOSTD ',FILORB,IRCLEO)
      IF (IRCLEO.EQ.0) THEN
        CALL PRIOSF(FILORB,NARC2,NUMSAT2,NAVNUM2,TBOUND2,
     1              TFRLST(1),NUMSA1)
      END IF
C
C ORBIT PARAMETERS AND A PRIORI SIGMAS (INCL. SAT. ANT. OFFSETS)
C --------------------------------------------------------------
      CALL PRIORP(1,NESTSAT,ESTSAT,NORB  ,SEQORB,PREC  ,NSTCEP,
     1            FRCTYP,NSASTC,NUMSTC1,NARC  ,NSTCEF,TIMSTC,SIGSTC,
     2            NANOFF,NSAOFF,SATOFF,PAROFF,NRQOFF,GRPOFF,SIGOFF,
     3            TIMOFF,ISGOFF,GNROFF)
      CALL PRIORP(2,0,ESTSAT,NORB2 ,SEQORB2,PREC2 ,NSTCEP2,
     1            FRCTYP2,NSASTC2,NUMSTC2,NARC2,NSTCEF2,TIMSTC2,
     2            SIGSTC2,0,NSAOFF,SATOFF,(/0,0,0/),0,GRPOFF,SIGOFF,
     3            TIMOFF,ISGOFF,GNROFF)
C
C SATELLITE SPECIFIC WEIGHTS AND SATELLITE PROBLEMS
C -------------------------------------------------
      CALL PRICRX(NWGT,SATWGT,TIMWGT,WGTWGT,SIGAPR,NCAMP,TAECMP)
C
C SHADOW TRANSITS
C ---------------
      NSHD=0
      IF (SATNUM(1,1) < 900) THEN
        IF (OPTDCB(4).EQ.2) THEN
          CALL PRISHD(2,0.5D0,NALLSAT,ALLSATNUM,
     1              0,(/0/),TFRLST,MXCSHD,NSHD,SATSHD,TIMSHD,FLGSHD)
        ELSEIF (OPTDCB(4).EQ.3) THEN
          CALL PRISHD(2,0.5D0,NALLSAT,ALLSATNUM,
     1              5,(/1,2,3,8,102/),
     1              TFRLST,MXCSHD,NSHD,SATSHD,TIMSHD,FLGSHD)
        ELSE
          CALL PRISHD(0,0.5D0,NALLSAT,ALLSATNUM,
     1              0,(/0/),TFRLST,MXCSHD,NSHD,SATSHD,TIMSHD,FLGSHD)
        ENDIF
      ENDIF
C
C ATMOSPHERE:
C ----------
C TROPOSPHERE MODEL AND MODEL PARAMETERS
C --------------------------------------
      CALL PRITRO(TITLES,ITROPO,IEXTRA,NTRREQ,NPARTR,TRPLIM,SIGTRP,
     1            NTRSTA,STATRP,TRPLMS,SIGTRS,ISGTRS,ITRMAP,ITRGRD,
     2            STNAME)
C
C IONOSPHERE MODELS AND MODEL PARAMETERS
C --------------------------------------
      IF (OPTGIM(5).EQ.0 .AND.
     1    OPTGIM(9).EQ.1) THEN
        DO ISTA=1,OPTGIM(7)
          NAMGIM(ISTA)=STNAME(ISTA)
        ENDDO
      ENDIF
C
      CALL PRIION(NIOREQ,IONMOD,IONREQ,TFRLST,OPTDIP,SIGDIP,
     1            OPTGIM,POLGIM,SIGGIM,NAMGIM,EPOGIM,PRIOPT(6),
     2            OPTDCB,SIGDCB)
C
C CLOCK PARAMETERS AND RECEIVER ANTENNA HEIGHT PARAMETERS
C -------------------------------------------------------
      CALL PRIREC(TITLES,NCLREQ,ISTCLK,ISACLK,STNAME,NCLK  ,IBIAS  ,
     1            CLKWGT,CLFRTO,CLKHED,NCLKST,NCLKSA,CLKSYS,NOINCLK,
     2            SECIPL,NPAR  ,LOCQ  ,TIMISB,NFTOT ,NDIFF ,STFIL  ,
     3            RECTYP,IRUNIT)
C
C POLE INFORMATION AND MODEL PARAMETERS
C -------------------------------------
      CALL PRIPOL(TITLES,NCAMP,TAECMP,POLPAR,
     1            NPOL,TPOL,SIGPOL,ISGPOL,ISGNUT)
C
C RECEIVER ANTENNA PHASE CENTER CORRECTIONS AND PARAMETERS
C --------------------------------------------------------
      CALL PRIPHC(PRIOPT(6),TITLES,NANRAO,ANTRAO,NUMRAO,PRNRAO,NFRRAO,
     1            SIGRAO,NEURAO,NANCAL,ANTCAL,NUMCAL,PRNCAL,NFRCAL,
     2            NPTCAL,SIGCAL,NANSPV,RAPZENMAX)
C
C SATELLITE ANTENNA PHASE CENTER CORRECTIONS AND PARAMETERS
C ---------------------------------------------------------
      CALL PRISPV(NANSPV,NSASPV,SATSPV,GNRSPV,NPTSPV,SIGSPV,NADMAX)
C
C CONSTANTS
C ---------
      CALL PRICON(PRIOPT(6),TITLES)
C
C Information on Vienna grid files
C --------------------------------
      CALL PRIGRD(opLoad,titles)
C
C PARAMETER CHARACTERIZATION LIST
C -------------------------------
      CALL PRIPAR(MAXTYP,PRIOPT(5),TITLES,NPAR,LOCQ,STNAME,opLoad,
     1            TIMISB)
C
      RETURN
      END SUBROUTINE

      END MODULE
