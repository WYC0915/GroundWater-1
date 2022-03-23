      MODULE s_PRCEPO
      CONTAINS

C*
      SUBROUTINE PRCEPO(SESSID ,STRAMB ,IRESID ,PRIOPT ,TPRINT ,NPASES ,
     1                  NSASES ,NFLSES ,NFRSES ,FRQSES ,FILNUM ,TLAST  ,
     2                  NSTAT  ,ICENTR ,NSAMPL ,WINDOW ,DTSIM  ,TIMREF ,
     3                  IDELTT ,SECIPL ,IFRMAT ,ICARR  ,MEATYP ,SATSES ,
     4                  NUMOBS ,STNAME ,WGSSES ,ELLSES ,ISATCO ,ZENMAX ,
     5                  ITROPO ,IEXTRA ,CORSTR ,ICOELV ,IAMB1  ,AMBDEF ,
     6                  NSATEL ,NUMSAT ,NUMAMB ,AMBSAT ,AMBIEP ,AMBCLS ,
     7                  AMBWLF ,NFRFIL ,NFREQ  ,AMBIGU ,AMB0   ,SYNCM  ,
     8                  ISYNC  ,SYNC   ,ISYNCR ,XMAXCL ,NPAR   ,NPARN  ,
     9                  NPAEPO ,IPHSEP ,STFIL  ,XSTAT  ,CLFRTO ,MELWUB ,
     1                  IZEROD ,INDARC ,TBOUND ,LOCQ   ,RECTYP ,ANTTYP ,
     2                  IANTEN ,CSESS  ,NWGT   ,SATWGT ,TIMWGT ,WGTWGT ,
     3                  OBSFLG ,OBSERV ,SVNFIL ,OBSFL1 ,OBSER1 ,SVNFI1 ,
     4                  TRPLIM ,TRPLMS ,TPOL   ,IPOLAR ,AOBS   ,AAUX   ,
     5                  AHELP  ,INDA   ,INDAUX ,BOBS   ,WEIGHT ,TIMSTC ,
     6                  SCASTC ,SCAHIL ,SCAPOT ,NSAOFF ,SATOFF ,TIMOFF ,
     7                  SCAALB ,SCACEN ,NSAALB ,SATALB ,ITRMAP ,ITRGRD ,
     8                  POLARS ,ANTCAL ,NUMCAL ,PRNCAL ,OPTDIP ,OPTGIM ,
     9                  POLGIM ,EPOGIM ,SCAGIM ,ANTRAO ,NUMRAO ,PRNRAO ,
     1                  OPTELI ,NKIN   ,STKIN  ,NPARMS ,PARLST ,ANOR   ,
     2                  BNOR   ,XXX0   ,XXREF  ,PARTYP ,RMSSUM ,RMSSES ,
     3                  NOBS   ,NOBSPA ,ELEVMM ,NOBELV ,OBSCLS ,ICLUST ,
     4                  NDIFF  ,NCLKST ,CLKSTA ,NCLKSA ,CLKSAT ,NOINCLK,
     5                  CLKSYS ,CLKHED ,CLKREC ,CLKPAR ,NDBLE  ,EDTLVL ,
     6                  OBSNUM ,OPTDCB ,IEPPAR ,ISASYS ,TOBS   ,IRETC  ,
     7                  LEOARC ,TBOUND2,TIMSTC2,ZMXLEO ,POSECC ,AELL   ,
     8                  BELL   ,DXELL  ,DRELL  ,SCELL  ,NSASPV ,SATSPV ,
     9                  NADMAX ,NAMAX  ,NOBNAD ,NADIMM ,RAPZENMAX,NSHD ,
     1                  SATSHD ,TIMSHD ,FLGSHD ,OBSEPO ,TIMFIL ,ICLU   ,
     2                  MAXSNG ,MAXFLS ,MAXSAS ,MAXFRS ,MAXSAT ,LSTSES ,
     3                  NOBAZIS,XSTELL ,NRGB   ,OPLOAD ,TIMISB ,IREL2)
CC
CC
CC
CC NAME       :  PRCEPO
CC
CC PURPOSE    :  PROCESSING OF ONE EPOCH IN BERNESE GPS SOFTWARE
CC
CC PARAMETERS :
CC         IN :  SESSID : SESSION NUMBER                      CH*4
CC               STRAMB : (1): AMBIGUITY RESOLUTION STRATEGY   I*4(*)
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
CC               IRESID : RESIDUAL PRINT INDEX                I*4
CC               PRIOPT : PRINTING OPTIONS                    I*4
CC                          PRIOPT(7): SATELLITE ELEVATIONS
CC                          PRIOPT(8): SYNCHRONIZATION ERRORS
CC               TPRINT : LAST EPOCH PRINTED                  R*8
CC               NPASES : NUMBER OF PARAMETERS FOR THIS       I*4
CC                        SESSION: NUMBER OF PARAMETERS WITH-
CC                        OUT AMBIGUITIES + AMBIGUITIES OF THE
CC                        SESSION
CC               NSASES : NUMBER OF SATELLITES                I*4
CC               NFLSES : NUMBER OF FILES                     I*4
CC               NFRSES : NUMBER OF FREQUENCIES               I*4
CC               FRQSES(I),I=1,..,NFRSES: FREQUENCIES         I*4
CC               FILNUM(I),I=1,..,NFLSES: OBS. FILE NUMBERS   I*4
CC               TLAST  : LAST OBSERVATION TIME               R*8
CC               NSTAT  : TOTAL NUMBER OF STATIONS            I*4
CC               ICENTR(I),I=1,..,NSTAT: INDEX OF CENTER      I*4
CC                        STATION FOR EACH STATION
CC               NSAMPL : SAMPLING RATE (SEC)                 I*4(3)
CC                        1: OBSERVATIONS
CC                        2: RESUBSTITUTION OF EPOCH PARAMETERS
CC                        3: PREELIMINATE OF EPOCH PARAMETERS
CC               WINDOW(K,I),K=1,2,I=1,..,NFTOT: OBSERV.      R*8
CC                        WINDOW
CC               DTSIM  : TIME INTERVAL TO IDENTIFY OBSERV.   R*8
CC                        OF THE SAME EPOCH
CC               TIMREF(I),I=1,..,NFTOT: REFERENCE EPOCHS     R*8
CC                        OF FILES
CC               IDELTT(I),I=1,..,NFTOT: TABULAR INTERVAL     I*4
CC                        FOR FILES
CC               SECIPL : MAX INTERVAL FOR CLK INTERPOLATION  R*8
CC               IFRMAT(I),I=1,..,NFTOT: FORMAT NUMBER FOR    I*4
CC                        OBSERVATION FILES
CC               ICARR(K,I),K=1,..,NFRFIL(I):  FREQUENCIES    I*4
CC               MEATYP(I),I=1,..,NFTOT: MEASUREMENT TYPE     I*4
CC               SATSES(I),I=1,..,NSASES: SVNS IN SESSION     I*4
CC               NUMOBS(J,I),J=1,..,NFREQ,I=1,..,NFTOT:       I*4
CC                        NUMBER OF DOUBLE DIFF. OBSERVATIONS
CC               STNAME(I),I=1,..,NSTAT: STATION NAMES        CH*16
CC               WGSSES(K,J,L,I),K=1,2,3,J=1,2,L=1,..,MAXFRQ, R*8
CC                        I=1,..,NFLSES:
CC                        WGS COORDINATES (INCL. ECCENTR.)
CC               ELLSES(K,J,L,I): SAME BUT ELLIPSOIDAL COORD. R*8
CC               ISATCO : ISATCO=0: NO SATELLITE CLOCK CORR.  I*4
CC                        ISATCO=1: APPLY SATELLITE CLOCK COR.
CC               ZENMAX : MAXIMUM ZENITH DISTANCE             R*8
CC               ITROPO : TROPOSPHERE INDEX                   I*4
CC               IEXTRA : INDEX FOR TROPOS. MODEL             I*4
CC               CORSTR : CORRELATION STRATEGY:               I*4
CC                         =1: CORRELATIONS WITHIN BASELINE
CC                         =2: CORRELATIONS WITHIN FREQUENCY
CC                               PROCESSED
CC                         =3: CORRELATIONS CORRECTLY MODELLED
CC               ICOELV(1:2,J) : MODEL FOR ELEV.-DEP.         I*4(2,*)
CC                          OBS. WEIGHTING
CC                         I=1: STATION, AIRSPACE
CC                         I=2: LEO
CC                         J=MEATYP
CC                        =0: EQUAL WEIGHTING FOR ALL OBS.
CC                        >0: MODEL NUMBER (SEE SR WGTELV)
CC               IAMB1(L,K,I),L=1,..,NUMAMB,                  CH*1
CC                        K=1,..,NFRFIL,I=1,..,NFLSES:
CC                        AMB. INITIALIZATION FLAG
CC               AMBDEF(I),I=1,..,NFTOT: AMBIGUITY INITIALI-  I*4
CC                        ZATION TYPE
CC               NSATEL(I),I=1,..,NFTOT: NUMBER OF SAT. IN    I*4
CC                        FILE I
CC               NUMSAT(K,I),K=1,..,NSATEL(I),I=1,..,NFTOT    I*4
CC                        SATELLITES
CC               NUMAMB(I),I=1,..,NFTOT: NUMBER OF AMBIGU.    I*4
CC               AMBSAT(J,I),J=1,..,NUMAMB(I),I=1,..,NFTOT:   I*4
CC                        AMBIGUITY SATELLITE NUMBERS
CC               AMBIEP(J,I),J=1,..,NUMAMB(I),I=1,..,NFTOT:   I*4
CC                        STARTING EPOCH NRS FOR AMBIGUITIES
CC               AMBCLS(L,K,I),L=1,..,NUMAMB(I), K=1,2,3,     I*4
CC                        I=1,..,NFTOT: AMBIGUITY CLUSTERS
CC               AMBWLF(K,J,I),K=1,..,J=1,2,I=1,..,NFTOT:     I*4
CC                        WAVELENGTH FACTORS
CC                        K: AMBIGUITY
CC                        J: FREQUENCY
CC                        I: FILE
CC               NFRFIL(I),I=1,..,NFTOT: NUMBER OF FREQ.      I*4
CC                        TO BE PROCESSED
CC               NFREQ(I),I=1,..,NFTOT: NUMBER OF FREQ.       I*4
CC                        IN FILE
CC               AMBIGU(L,K,I),L=1,..,NUMAMB, K=1,2,3,        R*8
CC                        I=1,..,NFTOT: AMBIGUITIES
CC               AMB0(L,K,I),L=1,..,NUMAMB,                   R*8
CC                        K=1,..,NFRFIL,I=1,..,NFLSES:
CC                        COMPUTED A PRIORI AMBIGUITY
CC               SYNCM(I),I=1,..,NFLSES: SYNCH. ERRORS        R*8
CC               ISYNC(K,I),K=1,..,NSASES,I=1,..,NFLSES:      I*4
CC                        SYNCHR. FLAG
CC               SYNC(K,I),K=1,..,NSASES,I=1,..,NFLSES:       R*8
CC                        SYNCHR. VALUES FOR EACH SATELLITE
CC               ISYNCR : SYNCHRONIZATION ERRORS ARE APPLIED  I*4
CC                          =1 ELSE =0
CC               XMAXCL(I),I=1,..,NFTOT: MAX. SYNCH. ERRORS   R*8
CC                        FOR ALL FILES
CC               NPAR   : NUMBER OF PARAMETERS IN ADJUSTMENT  I*4
CC               NPARN  : NUMBER OF NON AMBIGUITY PARAMETERS  I*4
CC               NPAEPO : NUMBER OF EPOCH-SPECIFIC PARAMETERS I*4
CC               IPHSEP : ONLY PHASE FOR RESUBST OF EPO-PARAM. I*4
CC               STFIL(K,I),K=1,2, I=1,2,..,NFTOT: STATIONS   I*4
CC                        IN FILES
CC               XSTAT(K,I),K=1,2,3, I=1,..,NSTAT: WGS COORD. R*8
CC               CLFRTO(K,I),K=1,2, I=1,..,NCLREQ:            R*8
CC               MELWUB : MELBOURNE-WUEBBENA LC               I*4
CC                        =0: NO
CC                        =1: YES
CC                        =2: DTEC LC
CC               INDARC : ARC NUMBER FOR SESSION              I*4
CC               TBOUND(K),K=1,2: START AND END OF ARC        R*8
CC                        NUMBER "INDARC"
CC               LOCQ(K,I),K=1,..,MAXLCQ, I=1,..,NPAR:        I*4
CC                        PARAMETER DESCRIPTOR
CC               RECTYP(K,I),K=1,2,I=...: RECEIVER TYPES      CH*20
CC               ANTTYP(K,I),K=1,2,I=...: ANTENNA  TYPES      CH*20
CC               IANTEN(K,I),K=1,2,I=...: ANTENNA NUMBERS     I*4
CC               CSESS(K,I) ,K=1,2,I=...: SESSION IDENTIF.    CH*4
CC               NWGT   : NUMBER OF INTERVALS WITH WEIGHTED   I*4
CC                        SATELLITES
CC               SATWGT(I),I=1,..,NWGT: NUMBERS OF WEIGHTED   I*4
CC                        SATELLITES
CC               TIMWGT(K,I),K=1,2,I=1,..,NWGT: START AND END R*8
CC                        OF TIME INTERVAL WITH WEIGHTED
CC                        SATELLITES IN MODIFIED JULIAN DATE
CC               WGTWGT(I),I=1,..,NWGT: SATELLITE SPECIFIC    R*8
CC                        SIGMA
CC               OBSFLG(K,J,I),K=1,..,NSAT,J=1,..,NFREQ,I=1,. CH*1
CC                        NFLSES: OBSERVATION FLAGS
CC               OBSERV(K,J,I),K=1,..,NSAT,J=1,..,NFREQ,I=1,. R*8
CC                        NFLSES: OBSERVATIONS
CC               SVNFIL(K,I),K=1,..,NSAT,I=1,..,NFLSES:       I*4
CC                        SATELLITE NUMBERS
CC               OBSFL1(K,J),K=1,..,NSAT,J=1,..,NFREQ:        CH*1
CC                        OBSERVATION FLAGS (AUX. ARRAY)
CC               OBSER1(K,J),K=1,..,NSAT,J=1,..,NFREQ:        R*8
CC                        OBSERVATIONS (AUX. ARRAY)
CC               SVNFI1(K),K=1,..,NSAT: SATELLITES (AUX.      I*4
CC                        ARRAY)
CC               TRPLIM(K,I),K=1,2, I=1,..,NTRPMOD: LIMITS    R*8
CC                        OF APPLICABILITY FOR TROP. MODEL I
CC               TPOL   : START AND END TIME OF INTERVAL FOR  R*8(2,*)
CC                        ONE SET OF PARAMETERS
CC                        1,2 :=BEGIN,END TIME, *:= 1..MAXPOL
CC               TRPLMS(K,I),K=1,2, I=1,..,NTRPMOD: LIMITS    R*8
CC                        OF APPLICABILITY FOR STATION SPECIFIC
CC                        TROPOSPHERE REQUESTS
CC               IPOLAR : POLARIZATION EFFECT                 I*4
CC                        =0: NONE, =1: GEOM., =2: FULL
CC               AOBS(I),I=1,..,: ARRAY CONTAINING ALL OBS-   R*8
CC                        EQNS FOR ONE EPOCH (NON-ZERO ELEMENTS)
CC               AAUX(I),I=1,.,MAXSNG: ONE LINE OF AOBS       R*8
CC               AHELP(I),I=1,..: ONE LINE OF FIRST DESIGN    R*8
CC                        MATRIX WITH ALL ZEROES
CC               INDA(I),I=1,..,: INDEX FOR AOBS              I*4
CC               INDAUX(I),I=1,..,MAXSNG: ONE LINE OF INDA    I*4
CC               BOBS(I),I=1,2,.: OBS-COMP FOR ONE EPOCH      R*8
CC               WEIGHT(I),I=1,..: WEIGHT MATRIX FOR EPOCH    R*8
CC               TIMSTC : EPOCHS WITH STOCH. ORBIT VAR.       R*8(*,*,*)
CC               SCASTC : SCALING FACTOR FOR                  R*8(*)
CC                        (1): STOCHASTIC PULSES
CC                        (2): STOCHASTIC ACCELERATIONS
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
CC               POLARS(L,K,I),L=1,..,NUMAMB,K=1,..,NFRFIL,   R*8
CC                        I=1,..,NFLSES: POLARIZATION SAVE ARRAY
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
CC               OPTELI(I),I=1,..,MAXTYP: OPTION FOR PRE-     I*4
CC                        ELIMINATION OF PARAMETER TYPES:
CC                        =0 : NOT PRE-ELIMINATED
CC                        =1 : PRE-ELIMINATED BEFORE INVERSION
CC                        =2 : PRE-ELIMINATED AFTER  INVERSION
CC                        =3 : PRE-ELIMINATED EPOCH-WISE
CC               NKIN   : # STATIONS ESTIMATED IN KIN. MODUS  I*4
CC               STKIN  : NUMBERS OF THE KIN. STATIONS        I*4(*)
CC               NPARMS : NUMBER OF PARAMETERS TO COMPUTE RMS I*4
CC               PARLST(I,K), I=1,..,5,K=1,..,MAXTYP: NUMBER  I*4
CC                        OF PARAMETERS:
CC                        I=1: #PARAMETERS OF TYPE I (NPARMS)
CC                        I=2: #SET-UP
CC                        I=3: #NO-OBS
CC                        I=4: #REF. PARAMETERS
CC                        I=5: #SINGULAR
CC               NDIFF(I):I=1,..,NFTOT: DIFFERENCE TYPE       I*4
CC                        NDIFF=0: ZERO DIFFERENCE
CC                        NDIFF=1: SINGLE DIFFERENCE
CC               NCLKST : NUMBER OF EPOCH WISE STATION CLOCKS I*4
CC               CLKSTA(I),I=1,..,MAXSTA: STATION NUMBERS FOR I*4
CC                        CLOCK ESTIMATION
CC               NCLKSA : NUMBER OF EPOCH WISE SAT. CLOCKS    I*4
CC               CLKSAT(I),I=1,..,MAXSAT: SAT. NUMBERS FOR    I*4
CC                        CLOCK ESTIMATION
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
CC               CLKSYS : =1: ONE REC.CLK FOR EACH SAT.SYS    I*4
CC               CLKHED : CLOCK HEADER INFORMATION            T_CLKHEAD
CC                          %NUMREF=0: FIX REF-CLOCKS
CC                          %NUMREF=2: SUM FOR REF-CLOCKS
CC               CLKREC : APRIORI STATION CLOCKS              T_CLKREC
CC                          %NEPO=0: NOTHING STORED (NO REQUESTS,
CC                                   PREELIM. "BI" OR "EP")
CC                          %CLOCK(ISAT,IEPO)
CC               CLKPAR(I,1..CLKHED%NCLK) : CHECKS WETHER  I*4(2,*)
CC                        A CLOCK IS OBSERVED OR NOT
CC                        I=1: GPS IF CLKSYS==1
CC                        I=2: GLONASS  IF CLKSYS==1
CC               EDTLVL : O-C EDIT LEVEL FOR ZERO DIFFERNECES R*8
CC               OBSNUM(I),I=1,..,NFLSES: OBSERV. NUMBERS FOR I*4
CC                        DIFFERENT FILES IN SESSION
CC               OPTDCB : OPTIONS FOR ESTIMATION OF           I*4(*)
CC                        DIFFERENTIAL CODE BIASES
CC                        (1): ESTIMATE DCBS FOR SATELLITES
CC                             =0: NO
CC                             = 1: P1-P2
CC                             = 2: P1-C1
CC                             = 3: LC
CC                        (2): ESTIMATE DSBS FOR RECEIVERS
CC                             = 0: NO
CC                             = 1: P1-P2
CC                             = 2: P1-C1
CC                             = 3: LC
CC                             =-2: P1-C1 MULTIPLIER
CC                        (3): REFERENCE SATELLITE NUMBER
CC                             = 0: CONSTRAIN ALL SAT
CC                             =-1: CONSTRAIN SUM OF ALL SAT
CC                        (4): SPECIAL OBSERVATION DATA SELECTION
CC                             = 0: NO
CC                             = 1: NIGHT-TIME
CC                             = 2: NON-ECLIPSING
CC                             = 3: NON-ECLIPSING ONLY FOR
CC                                  GPS BLOCK I, II, IIA
CC               IEPPAR : AT LEAST ONE PARAMETER TYPE PRE-    I*4
CC                        ELIMINATED EPOCH BY EPOCH (1=YES)
CC               ISASYS : SATELLITE SYSTEM TO BE CONSIDERED   I*4
CC                        =0: ALL
CC                        =1: GPS
CC                        =2: GLONASS
CC               LEOARC:  ARC NUMBER FOR LEO                  I*4
CC               TBOUND2(K),K=1,2: START AND END OF           R*8
CC                        ARC "LEOARC" IN MJD
CC               TIMSTC2: EPOCHS WITH STOCH. IMPULSES     R*8(*,*,*,*)
CC                        LEO ONLY
CC               ZMXLEO : MAX ZENITH ANGLE FOR LEO            R*8
CC               POSECC(I,K,L),I=1,2,3,K=1,2,L=1,..,NFTOT:    R*8
CC                        POSITIONING ECCENTRICITIES
CC                        I: COORDINATE
CC                        K: STATION
CC                        L: FILE
CC               AELL,BELL: SEMI-MAJOR AND -MINOR AXIS OF     R*8
CC                        ELIPSOID
CC               DXELL(I),I=1,2,3: SHIFT TO WGS-84            R*8
CC               DRELL(I),I=1,2,3: ROTATIONS TO WGS-84        R*8
CC               SCELL  : SCALE TO WGS-84                     R*8
CC               NSASPV(I),I=1,..,NANSPV: NUMBER OF           I*4
CC                        SATELLITES BELONGING TO ANTENNA
CC                        PHASE CENTER GROUP I
CC               SATSPV(J,I),J=1,..,NSASPV(I),I=1,..,NANSPV:  I*4
CC                        SATELLITE NUMBERS OF EACH ANTENNA
CC                        PHASE CENTER GROUP
CC               NADMAX:  MAXIMUM NADIR ANGLE ALLOWED FOR     R*8
CC                        SAT. ANT. PATTERN ESTIMATION
CC               RAPZENMAX: MAXIMUM ZENITH ANGLE ALLOWED FOR  R*8
CC                        REC. ANT. PATTERN ESTIMATION
CC               NSHD   : NUMBER OF SATELLITES IN SHADOW      I*4
CC               SATSHD : PRN NUMBERS IN SHADOW               I*4(*)
CC               TIMSHD : EXCLUDED TIME INTERVAL              R*8(2,*)
CC               FLGSHD : FLAG OBSERV. (=2) OR NOT (=0)       I*4(*)
CC               OBSEPO(I,J),I=1..MAXSAT,J=1,NFREQ,K=1,NFLSES I*4(*,*,*)
CC                        SAT OBSEVRED IN THE PREV. EPOCH
CC               TIMFIL(1..NFTOT) TIME INT. CONNECTED BY AMB. t_ambTime(:,:)
CC               ICLU(0..MAXSYS,1..MAXAMB,1..MAXFIL) PARAMETER FOR
CC                        CLUSTER NUM.                        I*4
CC               NRGB   : NUMBER OF RANGE BIAS REQUESTS       I*4
CC               OPLOAD : SCALING FACTORS FOR VIENNA GRID FILES   T_OPTLOAD(3)
CC                        1: ATMOSPHERIC NON-TIDAL LOADING
CC                        2: OCEAN NON-TIDAL LOADING
CC                        3: HYDROSTATIC PRESSURE LOADING
CC               IREL2  : FLAG FOR PERIODIC RELATIVISTIC J2   I*4
CC                        CORRECTION
CC    IN/OUT  :  LSMAT(I),I=1,..,NPAR: AUX. ARRAY             CH*1
CC               ANOR(I),I=1,.. : NORMAL EQN MATRIX           R*8
CC               BNOR(I),I=1,..,NPAR: RIGHT HAND SIDE OF      R*8
CC                        NEQ-SYSTEM
CC               XXX0(I),I=1,..,NPAR: A PRIORI VALUES OF PAR. R*8
CC               XXREF(I),I=1,clock%nref: REFERENCE CLOCK PARAMETER t_par
CC               PARTYP : PARAMETER DESCRIPTION               t_partyp(*)
CC               RMSSUM : SUM OF RES. SQUARE                  R*8
CC               RMSSES : SESION SPECIFIC RMS ERROR           R*8
CC               NOBS   : NUMBER OF OBSERVATIONS              I*4
CC               NOBSPA : NUM.OF OBSERV PER PARAMETER         I*4(*,*)
CC                        NOBSPA(MAXMEA*ISYS+IMEA,IPAR)
CC               ELEVMM(2,I),I=1,..,NFTOT: MINIMAL (1) AND    R*8
CC                        MAXIMUM (2) ELEVATION ANGLE
CC               NOBELV(I,J,K),I=1,2,J=1,..,18,K=1,..,NFTOT   I*4
CC                        5-DEG BIN OBSERVATION STATISTICS
CC               NAMAX  : ACTUAL MAXIMUM NADIR ANGLE          R*8
CC               NOBNAD(I,J,K),I=1,2,J=1,..,30,K=1,..,NFTOT   I*4
CC                        0.5-DEG BIN OBSERVATION STATISTICS
CC               NADIMM(2,I),I=1,..,NFTOT: MINIMAL (1) AND    R*8
CC                        MAXIMUM (2) NADIR ANGLE
CC               NOBAZIS(L,K,J,I),L=1,2,K=1,..,36,            I*4
CC                        J=1,..,NSATEL(I),I=1,..,NFTOT
CC                        OBSERVATION STATISTICS FOR THE
CC                        AZIMUTH ANGLE AT THE SATELLITE
CC        OUT :  OBSCLS(I),I=1,..MAXAMP CLUSTER FOR AMB.PAR.  I*4
CC               ICLUST(I),I=1,..MAXFIL NUMBER OF CLUSTERS    I*4
CC               TOBS   : ACTUAL OBSERVATION EPOCH            R*8
CC               IRETC  : RETURN CODE                         I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC CREATED    :  87/10/20 17:07
CC
CC CHANGES    :  27-SEP-91 : ??: SAMPLING RATE IN SECONDS
CC               13-FEB-92 : ??: CHANGES DUE TO ERP- ESTIMATION
CC               04-JUN-92 : ??: NEW PARAMETERS IN "GETORB","COOTRA",
CC                               "XYZTIM" AND "SNDIFF" FOR OPTION
CC                               SYSTEM J2000.0
CC               23-JUL-92 : ??: ADD "TBOUND" AS PARAMETER AND IN CALL
CC                               SNDIFF (MANOEUVRE)
CC               25-JUL-92 : ??: ADD "MEATYP" IN CALL GOBSEP (MARK SAT.)
CC               27-JUL-92 : ??: ADD "NWGT,SATWGT,TIMWGT,WGTWGT" AS
CC                               PARAMETERS. CALL WGTSAT. ADD "COVSAT"
CC                               TO CALL CORREL.
CC               31-JUL-92 : ??: ADD PARAMETER "RMSSES" FOR "FARA"
CC               14-AUG-92 : ??: SUBROUTINE "CLUSTA"
CC               25-FEB-93 : ??: NEW PARAMETERS "RECTYP","ANTTYP","IANTEN"
CC                               FOR CALL GPHECC TO APPLY ELEVATION
CC                               DEPENDENT PHASE CENTER CORRECTIONS
CC               20-MAR-93 : ??: INTRODUCTION OF STOCHASTIC ORBIT PARAMETERS
CC               03-APR-93 : ??: SATELLITE ANTENNA OFFSET PARAMETERS
CC               28-APR-93 : ??: REDUCE NUMBER OF CONTINUATION LINES
CC               14-MAY-93 : ??: NEW PARAMETER TYPES (POTENTIAL, HILL,
CC                               ALBEDO, CENTER OF MASS)
CC               29-OCT-93 : ??: HANDLE SATELLITES MISSING IN STD.ORBIT FILE
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               27-DEC-93 : MR: TIME WINDOWS FOR SAT.ANT.OFFSETS
CC               06-APR-94 : MR: PRINT ELEVATIONS FOR 31 SATELLITES
CC               08-APR-94 : MR: ADD AZIMUTH DEP. PHASE CENTER CORR.
CC               13-APR-94 : SS: NPAEPO ADDED TO PARAMETER LIST AND
CC                               TO CALL OF CLUSTA
CC               29-JUL-94 : MR: MAPPING FUNCTIONS
CC               12-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               19-AUG-94 : MR: ADD "LOCQ" TO CALL OF "CLUSTA"
CC               27-OCT-94 : MR: NO CALL CLUSTA FOR CODE FILES,
CC                               INCREASE TEXT STRING AND FORMAT FOR
CC                               SYNCHRON. ERRORS
CC               06-NOV-94 : MR: ANTENNA PHASE CENTER PARAMETERS
CC               10-APR-95 : MR: REMOVE EQUIVALENCE FOR WGTGEN
CC               06-JUN-95 : SS: GLOBAL IONOSPHERE MODEL PARAMETERS
CC               14-AUG-95 : LM: MEMORY COMPACT VERSION
CC               17-AUG-95 : LM: MEMORY COMPACT VERSION 2
CC               22-AUG-95 : SS: CONVERT RMS TO L1 PHASE FOR WUEBB LC
CC               25-AUG-95 : SS: MODIFIED CALL OF SR SEQEPO
CC               05-DEC-95 : SS: NEW IONOSPHERE MODEL (TYPE 2)
CC               05-DEC-95 : SS: "IONO" REMOVED (SEE SR CHKION)
CC               05-DEC-95 : SS: CALL OF SR PRANGE WITH "IORSYS"
CC               05-DEC-95 : SS: APPLY "SCAGIM"
CC               05-DEC-95 : SS: CALL OF SR SNDIFF WITH "WGSEPO"
CC               25-JAN-96 : SS: INCREASE "MAXFLS" FROM 28 TO 30
CC               21-FEB-96 : TS: INCREASE "MAXFLS" FROM 30 TO 35
CC               26-MAR-96 : MR: ADD "CSESS", REMOVE "ISES" FROM
CC                               PARAMETER LIST. CALL GPHECC WITH
CC                               "CSESS"
CC               26-MAR-96 : MR: RECEIVER ANTENNA OFFSETS
CC               06-MAY-96 : TS: REMOVED OLD SATELLITE CLOCK STUFF
CC               19-JUL-96 : TS: CORRECT USE OF "CORREL" FOR ZERO DIFF. CASE
CC               21-JUL-96 : TS: INCREASED MAXFLS FROM 55 --> 60
CC                5-AUG-96 : TS: INCREASED MAXFLS FROM 60 --> 65
CC               27-SEP-96 : TS: MAXFLS IN INCLUDE FILE
CC               23-OCT-96 : MR: WTRESI ONLY IF RESIDUALS ARE REQUESTED
CC               13-NOV-96 : HM: PASS "IFRSES" (NOW "IFRSE2") TO "WTRESI"
CC                               AS ARRAY TO HANDLE PROPERLY CORRELATION
CC                               STARTEGY 3
CC               27-JAN-97 : MR: MAX. AND. MIN ELEVATION ANGLE PER FILE
CC               29-JAN-97 : SS: ELEVATION-DEPENDENT OBS. WEIGHTING
CC               27-JAN-97 : JJ: TAKE MAXFLS OUT FOR ALL PARAM STATEMENTS
CC                               SINCE I:MAXFLS IS BEING USED.
CC               25-MAR-97 : MR: CORRECT WRONG DIMENSION OF "ELEVA" FROM
CC                               "MAXSAS" TO "MAXSAT" (ERROR INTRODUCED
CC                               WITH CHANGE OF 27-JAN-97).
CC               08-APR-97 : SS: NIELL MAPPING, TROPOSPHERE GRADIENTS
CC               20-MAY-97 : MR: HELP VARIABLE "DERHLP" IN CALL PDCLK
CC               05-AUG-97 : SS: ELEV.-DEP. SIP CONSTRAINING
CC               07-AUG-97 : MR: ADD "OBSNUM" TO PARAMETER LIST
CC               11-AUG-97 : SS: NEW OPTION "STRAMB(2)"
CC               14-AUG-97 : SS: DIFFERENTIAL CODE BIASES
CC               14-AUG-97 : SS: USE "IEPPAR"
CC               14-AUG-97 : SS: "OPTDIP" REMOVED
CC               08-SEP-97 : SS: ESTIMATE ZENITH SIP PARAMETERS
CC               22-SEP-97 : SS: 5-DEG BIN OBSERVATION STATISTICS
CC               26-SEP-97 : DI: USE MAXSAT.inc
CC               02-OCT-97 : TS: CORRECTED O-C "GOTO 300" JUMP
CC               07-OCT-97 : SS: REPLACE "CODSIG" BY "EDTLVL"
CC               07-OCT-97 : SS: HANDLE ELEVATION STATISTICS FOR WUEBB
CC               08-OCT-97 : MR: RATIO ZENITH/GRADIENT PARAMETERS
CC               09-OCT-97 : TS: NEW CALL TO XYZTIM FOR NEW SOLID-TIDES MODEL
CC               22-OCT-97 : SS: CODE BIAS I/O FILES
CC               10-NOV-97 : SS: ELEV.-DEP. "EDTLVL" IF "ICOELV>0"
CC               25-NOV-97 : TS: MAXSAS IN INCLUDED FILE
CC               25-NOV-97 : SS: CHECK "IRCCLK"
CC               21-JAN-98 : SS: CALL OF SR INIEPO MODIFIED
CC               26-JAN-98 : SS: STATION-SPECIFIC GIMS
CC               30-MAR-98 : TS: SIMULTANEOUS CODE AND PHASE ZD PROCESSING
CC               29-APR-98 : SS: DTEC LC
CC               18-MAY-98 : SS: "GOTO 300" ADDED
CC               24-JUN-98 : HH: MODIFICATIONS FOR GLONASS
CC               25-AUG-98 : MR: NO AMBIGUITY INITIALIZATION IF 1 OBS.
CC               04-AUG-99 : MR: ADD "AMBSAT" TO CALL OF SR AMBSET
CC               05-AUG-99 : SS: PASS "ZENMAX" TO SR PRANGE
CC               16-AUG-99 : RD: DIMENSIONS (SMALL/MEDIUM/LARGE) INTO I:GPSEST
CC               27-JAN-00 : TS: ABUSE SVNOBS FOR ELE AND AZI IF NDIFF=1
CC               27-JAN-00 : TS: CHANGES FOR CLOCK RINEX OUTPUT
CC               27-MAR-00 : RD: HANDLE AMBIGUITIES FOR SATELLITES WITH NO OBS.
CC               13-APR-00 : SS: ESTIMATE (P1-C1) CODE BIASES
CC               17-APR-00 : RD: APRIORI AMBIG FOR ZERO DIFF SOLUTIONS
CC               02-OCT-00 : SS: CONSIDER P1-C1 DCBS IN CASE OF MELWUB=1
CC               25-OCT-00 : RD: COUNT OBS. FOR CLOCKS
CC               22-NOV-00 : TS: HANDLE KINEMATIC COORDINATES
CC               17-FEB-01 : MR: SR:STAFLG - HANDLE STATION TYPES
CC               28-FEB-01 : DS: SR:GTLEOCO - GET LEO COORDINATES
CC               28-FEB-01 : DS: XKIN,WGSEPO INCREASED DIMENS. FROM 3 TO 9
CC               16-MAY-01 : DS: NEW PARAMETERS, FOR LEO ARC: LEOARC,TBOUND2
CC               27-AUG-01 : MR: CORRECT ZEN2 FOR LEO
CC               28-AUG-01 : MR: REMOVE STRANGE CODE/PHASE WEIGHTING "WGTDEF"
CC               30-AUG-01 : DS: CUT-OFF FOR LEO (0 AT THE MOMENT)
CC               20-FEB-02 : DS: HANDLE SPACEBORNE, AIRBORNE AND KINEMATIC
CC               22-FEB-02 : DS: SAVE IRCVEL
CC               14-AUG-01 : MR: ADD PARAM STNAME TO SR SNDIFF
CC               15-OCT-01 : HU: NEW CALL OF GOBSEP
CC               22-JAN-02 : RD: CONDITION OF SUM FOR REFERENCE CLOCKS
CC               23-JAN-02 : RD: REMOVE ZAMBIG
CC               25-JAN-02 : RD: STORE APRIORI STATION CLOCKS IF THEY ARE
CC                               NOT PREELIMINATED WITH "BEFORE" OR "EPOCH"
CC               09-FEB-02 : RD: WEIGHT REAL*4->REAL*8
CC               07-MAY-02 : SS: DCB UPDATE
CC               24-JUN-02 : DS: LEO ELEV.DEP.WEIGHT. FLAG IN ICOELV(2)
CC               25-JUN-02 : DS: NEW PARAMETER: MAX ZENITH ANGLE FOR LEO
CC               25-JUN-02 : RD/DS: MERGE VERSION BE AND MUNICH
CC               19-JUL-02 : DS: LEO SLR ORBIT VALIDATION
CC               04-SEP-02 : RD: SKIP OBS. IF NO SAT-CLK AVAILABLE
CC               18-SEP-02 : DS: KINEMATIC ESTIMATION:LEO,AIRPLANE,
CC                               SHIP,GROUND,STATIC GROUND
CC               07-OCT-02 : DS: IN LEO CASE WITHOUT KIN FILE DO NOT WRITE
CC                               A PRIORI LEO COORDINATES
CC               10-OCT-02 : DS: COMPUTE TRUOFF ONLY ONCE
CC               12-OCT-02 : DS: PASS FULL "STNAME" TO SNDIFF
CC               14-NOV-02 : RS: SATELLITE ANTENNA PHASE CENTER VARIATIONS
CC               22-JAN-03 : CU: ARGUMENT STNAME FOR SR SNDIFF CORRECTED
CC               23-JAN-03 : RD: SCALE EDTLVL W.R.T. STATION WEIGHT
CC               28-JAN-03 : RS: COVSAT,COVELV REAL*4 -> REAL*8
CC               28-JAN-03 : RD: USE ISYNCR FOR COMP. APRIORI AMBIGUITIES
CC                               TEXT IN TXTSYN MAY BE LONGER THAN "(A132)"
CC               29-JAN-03 : RD: NO POLARIZATION FOR CODE IN ALL CASES
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               08-MAR-03 : MR: MAXAMB MOVED TO M_MAXDIM
CC               08-MAR-03 : HU: INTERFACE FOR WGTSTA ADDED
CC               15-MAR-03 : HU: REMOVE UNUSED MAXxxx
CC               30-Apr-03 : SS: SATELLITE SYSTEM SELECTION
CC               28-MAY-03 : RD: NEW CALL OF SR GPHECC
CC               08-JUN-03 : HU: SAVE FILNAM AND FILSCR
CC               23-JUN-03 : HB: INTERFACE FOR SR STAFLG
CC               24-JUN-03 : RD: MORE THAN ONE STATION IN KIN. SCRATCH FILE
CC               25-JUN-03 : RD: DIFFERENT CALL OF READKIN FOR IMPROVE OR
CC                               INTRODUCE VALUES
CC               26-JUN-03 : RD: SIMPLIFY THE USE OF SHIP AND AIR
CC               11-AUG-03 : RS: ADD CALL OF GTSATA, APPLY SATELLITE
CC                               ANTENNA PHASE CENTER VARIATIONS, CHANGE
CC                               CALL OF LEOSKY, RECTYP=' ' IN 4 CALLS OF
CC                               GPHECC, CHANGE SENSOR NAME IN 3 CALLS OF
CC                               LEOANTE2
CC               08-SEP-03 : HU: ANTNAM, RECNAM CHR16 -> CHR20
CC               16-SEP-03 : HU: POLARIZATION ACTIVATED FOR ZERO-DIFF
CC               22-OCT-03 : RS: ADD RAPZENMAX
CC               12-DEC-03 : AJ: ADDITIONAL COMPONENTS FOR TIMSTC,SCASTC
CC               23-JAN-04 : AJ: LEO CUT-OFF REFERRING TO ZEN
CC               27-JAN-04 : HU: EXCLUSION OF ECLIPSING SATELLITE OBSERVATIONS
CC               26-FEB-04 : DS: POLARIZATION EFFECT FOR ZERO-DIFFERENES
CC               24-MAR-04 : DS: ADD ZEN2, AZI2 IN CALL OF SNDIFF
CC               13-APR-04 : RS: ADD AZISOK TO CALLS OF PRANGE AND SNDIFF,
CC                               ADD NOBAZIS
CC               10-MAY-04 : DS: PASS SVN NUMBER AND IORSYS TO LEOSKY
CC               05-JUL-04 : HU: WIDER TABLES FOR SAT.ELEV. AND REC.SYNCHR
CC               08-JUL-04 : RD: USE ADDNOR/SYMIN8-HELP ARRAYS FROM GPSEST
CC               20-OCT-04 : RD: HANDLE ISASYS IN GFRQEP
CC                               ALLOCATE AND SAVE LOCAL ARRAYS
CC               21-OCT-04 : RD: REMOVE UNUSED ARGUMENTS: IDEL, A0I, AII
CC               07-FEB-05 : HU: DECL. OF FREQUENCY ARRAYS 124 -> 999
CC               20-MAY-05 : RD: MAXEQN+1, FOR VERY SMALL OBS. FILES
CC               26-MAY-05 : RD: USE DIMENSION MAXSGR FOR SATOFF AND SATSPV
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               07-AUG-05 : CU: CHECK IF RAPZENMAX > 0
CC               08-Aug-05 : HB: Use new SR TIMST2 (module)
CC               12-AUG-05 : RD: NEW PARAMETER FOR SR SEQEPO
CC               09-NOV-05 : AG: SENNUM for GTSATA CALL ADDED
CC               10-JAN-06 : RD: STATISTICS ON PHASE-CONNECTED EPOCHS
CC               13-FEB-06 : HB: ADAPT CALL OF SR SIZE_AMBTIME
CC               15-FEB-06 : HB: CORRECT IF-CONDITION FOR
CC                               "SHOULD NEVER HAPPEN"
CC               23-FEB-06 : AG: SENNUM for GPHECC CALL ADDED
CC               23-FEB-06 : RD: GIVE SIZE OF DTFIL TO WTRESI
CC               19-APR-06 : HB: CORRECT IMPLEMENTATION FOR "PHASE-CONNECTED"
CC                               EPOCHS WHEN PROCESSING CODE OBSERVATIONS
CC               29-JUN-06 : HB: A PRIORI VALUES (XXX0) AS PARAMETER,
CC                               SR PRANGE: SATCLK AS PARAMETER
CC                               NEW SR APRVAL TO SAVE A PRIORI VALUES
CC                               SR WTRESI: XXX0 AS PARAMETER
CC                               GET LEO POSITION WITH CORRECTED EPOCH
CC               18-JUL-06 : AG: CMC ADDED
CC               02-AUG-06 : HB: PUT MXCLCQ INTO PARAMETER LIST FROM APRVAL
CC               17-Aug-06 : HU: USE FOR S_GOBSEP, S_GFRQEP, S_LEOSKY
CC               24-Aug-06 : HB: SAVE A PRIORI COORDINATES IN CASE OF
CC                               KINEMATIC STATION AND NO KININP FILE
CC               18-SEP-06 : HU: POLARIZATION EFFECT OPTION
CC               21-SEP-06 : HU: ERROR MESSAGE IN CASE OF MISSING SAT ANTENNA
CC               05-Dec-06 : HB: ICLU AS PARAMETER
CC               11-JAN-07 : HB: OBSEPO(MAXSAT,...) INSTEAD OF MAXSAS
CC               22-JAN-07 : HB: ICLU AS 2-DIM ARRAY
CC               12-JUN-07 : AG: USE STA_OFF instead of GPHECC
CC               17-JUN-07 : RD: ONLY PHASE FOR RESUBSTITUTION OF EPO-PARAM.
CC               18-JUN-07 : RD: ICOELV INDIV. FOR EACH MEATYP
CC               01-NOV-07 : HB: ADD PARAMETER SECIPL
CC               05-Dec-07 : HB: MODIFICATIONS FOR LEO PROCESSING
CC               11-DEC-07 : AJ: KINEMATIC LEO SLR VALIDATION AGAIN WORKING
CC               17-JAN-08 : AJ: VEL TO WGSSAT ADDED
CC                           HB: XXREF ADDED (REFERENCE CLOCK PARAMETER)
CC               27-MAR-08 : HB: CORRECTION FOR KINEMATIC MODE FOR SECOND
CC                               STATION IN BASELINE
CC               29-Apr-08 : LP: CMC correction for LEOs introduced (SR cmc_leo)
CC               16-MAY-08 : DT: SLR SATELLITES TREATED AS GNSS NOT AS LEO
CC               17-JUN-08 : RD: COUNTER FOR OBSERV. PER PARAMETER ADDED
CC               30-JUN-08 : RD: VMF ADDED
CC               01-OCT-08 : HB: ADD ZENION AS PARAMETER TO SR PRANGE
CC               16-OCT-08 : HB: KIN-FILE AS INPUT FOR LEO PROCESSING IS
CC                               WORKING AGAIN
CC               26-FEB-09 : HB: STANAM(2) ONLY FILLED IN SINGLE-DIFFERENCE CASE
CC               02-APR-09 : DT: ADD NRGB FOR RANGE BIASES
CC               04-MAY-09 : RD: SCALING OF LOADING MODELS ADDED
CC               09-MAY-09 : RD: SEPERATE RECEIVER CLOCKS FOR GPS/GLONASS
CC               27-MAY-09 : RD: SPECIAL SAMPLING FOR RESUBST. OF EPOCH PARAM.
CC               29-MAY-09 : RD: INPUT CLOCKS ALSO FROM INPUT CLK RNX FILE
CC               21-SEP-09 : RD: ECLIPSING FLAG FOR CLOCK RESULTS ADDED
CC               04-JAN-10 : SL: ZMAX ADDED TO SNDIFF CALL
CC               04-MAR-10 : SL: TAB REPLACED BY BLANKS
CC               16-NOV-10 : RD: UPDATE INTERVAL FOR PIECE-WISE LINEAR PARAM.
CC               02-DEC-10 : RD: CMC FOR ATL ADDED
CC               03-FEB-11 : CR: ADD SWITCH (IREL2) FOR PERIODIC RELATIVISTIC
CC                               J2-CORRECTION
CC               17-FEB-11 : SS: STRAMB(3) FOR SELECTION OF GNSS
CC               17-AUG-11 : HB: GET ELLIPSOIDAL COORDINATES FOR LEO, SMALL
CC                               MODIFICATIONS FOR TIMFIL
CC               29-AUG-11 : HB: CORRECT CHECK FOR AVAILABLE SATELLITE CLOCK
CC               04-OCT-11 : SL: USE M_BERN WITH ONLY, PRINT 80 SYNC ERRORS
CC               11-NOV-11 : RD/MM: TAKE SATELLITE PCOS FROM PCV FILE
CC               26-MAR-12 : RD: SWITCH FROM TIMSTR TO TIMST2
CC               28-MAR-12 : RD: USE SVN2CHR AS MODULE NOW
CC               28-MAR-12 : RD: REMOVE UNUSED VARIABLES FROM APRVAL
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC               04-MAY-12 : RD: REMOVE UNUSED VARIABLES AND MODULES
CC               10-JUL-12 : RD: REMOVE UNUSED VARIABLES
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, r8b, lfn002, lfnPrt, lfnErr,
     1                    longLineLength, fileNameLength, staNam2Length
      USE m_global, ONLY: maxSys
      USE d_isbfil, ONLY: getisb
      USE d_stacrx, ONLY: MTypeSPACE,MTypeAIR,MTypeSHIP
      USE d_phaecc, ONLY: sta_off, sat_off
      USE d_satfil, ONLY: typeSLR, typeMWTR
      USE d_clkrnx, ONLY: t_clkhead,t_clkrec, clkIdx
      USE d_const,  ONLY: C, DATE, PI, TIME, WGTCOD, WGTPHA
      USE d_par,    ONLY: t_par
      USE d_grid,   ONLY: getGrid,getGridKeyw,grdNeq
C
      USE p_gpsest, ONLY: MAXMEA,maxPar,maxLoc,maxLcq,
     1                    maxStc,maxArc,t_ambTime,size_ambTime,
     2                    t_parTyp,t_optLoad,t_ambTimeRec
      USE l_basfun, ONLY: dmod
      USE s_seqepo
      USE s_readpre
      USE s_seqamb
      USE s_alcerr
      USE s_opnfil
      USE s_wtresi
      USE s_mjdgps
      USE s_bldzro
      USE s_cootra
      USE s_prange
      USE s_leoante
      USE s_chkntd
      USE s_iniepo
      USE s_wgtsta
      USE s_svn2chr
      USE s_getorb
      USE s_truearth
      USE s_opnerr
      USE s_dcbcor
      USE s_pdclk
      USE s_maxtst
      USE s_getorf
      USE s_timst2
      USE s_staflg
      USE s_polari
      USE s_wgtsat
      USE s_readvel
      USE s_timst2
      USE s_major
      USE s_gobsep
      USE s_readdt
      USE s_correl
      USE s_xyztim
      USE s_blddbl
      USE s_readkin
      USE s_aprval
      USE s_sndiff
      USE s_setsyn
      USE s_leoante2
      USE s_gfrqep
      USE s_exitrc
      USE s_gtleoco
      USE s_gtsensor
      USE s_ellecc
      USE s_jmt
      USE s_radgms
      USE s_addcor
      USE s_addnor
      USE s_clusta
      USE s_leosky
      USE s_ambset
      USE s_gtflna
      USE s_xyzell
      USE s_polarleo
      USE s_wgtelv
      USE s_cmc_leo
      USE s_gnssatti
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I      , IAC    , IAE    , IAMB   , IAMSAV , IANT   ,
     1          IANTL  , IAZI   , IBIN   , IBINNAD, ICHR1  , ICLS   ,
     2          ICRARC , IDAY   , IDBLE  , IDTEC  , IDUMMY , IELE   ,
     3          IEPPAR , IEXTRA , IF     , IF2    , IFIL   , IFIRST ,
     4          IFLAG  , IFLSES , IFREQ  , IFRPRI , IFRQ   , IFRSES ,
     5          IHOUR  , II     , IKIN   , IKINSC , IMIN   , IMONTH ,
     6          IMW    , INDARC , IORSYS , IOSTAT , IP     , IPREPO ,
     7          IRC    , IRC1   , IRC2   , IRC3   , IRCCLK , IRCPCV ,
     8          IRCEPO , IRCEPO2, IRCEPO3, IRCNTD , IRCODE , IRCPRE ,
     9          IRCSCR , IRCVAL1, IRCVAL2, IRCVEL2, IRESID , IRETC  ,
     1          IRFIND , IS     , ISATCO , ISAEPO , ISAFIL , JJ     ,
     2          ISAMPL , ISASES , ISASYS , ISAT   , ISATEP , ISATMW ,
     3          ISEC   , ISFL   , ISFLEP , ISING  , ISING2 , IST    ,
     4          ISTKIN , ISVMOD , ISYNCR , ITEC   , ITEC0  , ITRMAP ,
     5          ITROPO , IVAL   , IYEAR  , IZEROD , JEPO   , K      ,
     6          KSAT   , LEOARC , LFNKSC , MAXDER , MAXEQN , MAXFLS ,
     7          MAXFRS , MAXSAS , MAXSAT , MAXSNG , MEA    , MEAMW  ,
     8          MELWUB , MXCAMB , MXCFLS , MXCFRQ , MXCLCQ , MXCSAS ,
     9          MXCSAT , MXCSGR , MXCSHD , MXCSTC , MXESNG , NCLKSA ,
     1          NCLKST , NDBIDB , NDBLE  , NDBTOT , NFILEP , NFILMW ,
     2          NFLSES , NFRSES , NKIN   , NMAX   , NOBS   , NOK    ,
     3          NPAEPO , NPAR   , NPARMS , NPARN  , NPASES ,
     4          NSAEPO , NSASES , NSATEP , NSATMW , NUMEPO ,
     5          NSATNW , NSHD   , NSING  , NSNEW  , NSTAT  , NWEEK  ,
     6          NWGT   , IPOLAR , IPHSEP,  IP1    , IP2    , IBINAZIS,
     7          JF     , clOk   , clsNum , iCl    , CLKSYS , ICLK   ,
     8          NRGB
C
      REAL*8    AELL   , BELL   , CDMEAN , DAY    , DD     , DTSEC  ,
     1          DTSIM  , DTVAL  , DTVALS , DUMMY  , DXVAL1 , DXVAL2 ,
     2          EDTLV0 , EDTLVL , ELEV   , GPSSEC , OBSWGT , PLREFF ,
     3          PRDIF  , RMSOLD , RMSSES , RMSSUM , SCAALB , SCACEN ,
     4          SCAHIL , SCAPOT , SCELL  , SEC    , SECIPL , SECOND ,
     5          SZ     , SZ1    , SZ2    , SZL    , TEST   , TFRAC  ,
     6          TLAST  , TOBS   , TOSC   , TPRINT , TSEC   , TVAL1  ,
     7          TVAL2  , UT1GPS , UT1GPS1, UT1GPS2, WGTDEF , XDTEC  ,
     8          XPOL   , XPOL1  , XPOL2  , XPOLL  , YPOL   , YPOL1  ,
     9          YPOL2  , YPOLL  , ZENMAX , zMax   , ZMXLEO , TOB0   ,
     .          EDTNOR , RHLP   , NPOLAR
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      CHARACTER*6 MXNSAT,MXNFRQ,MXNAMB,MXNFLS,MXNSAS,MXNLCQ
      CHARACTER*6 MXNSTC,MXNSHD,MXNSGR
C
C COMMON BLOCKS
C -------------
c      COMMON/CPRCEP/HADD,OBFLEP,BOLD,FIND,FINDO,CIND,CINDO,
c     1              IFLOBS,IFRSE2,SVNOBS,SAFLEP,COVELV,INDI,INDK
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMFLS/MXCFLS,MXNFLS
      COMMON/MCMSAS/MXCSAS,MXNSAS
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMSTC/MXCSTC,MXNSTC
      COMMON/MCMSHD/MXCSHD,MXNSHD
      COMMON/MCMSGR/MXCSGR,MXNSGR
C
C GLOBAL DIMENSIONS
C -----------------
      TYPE(t_clkhead) CLKHED
      TYPE(t_clkrec)  CLKREC
      TYPE(t_ambTime), DIMENSION(0:MAXSYS,*):: TIMFIL
      TYPE(t_ambTimeRec)                    :: hlpInt
      TYPE(t_par),DIMENSION(:)              :: XXREF
      TYPE(t_optLoad), DIMENSION(:)         :: opLoad
      TYPE(t_parTyp),  DIMENSION(:)         :: parTyp
C
      INTEGER*4 INDA(*),INDAUX(*)
C
      INTEGER*4 FILNUM(*),IDELTT(*),ICARR(MXCFRQ,*),NUMOBS(MXCFRQ,*)
      INTEGER*4 ICENTR(*),NFREQ(*),CORSTR
      INTEGER*4 STRAMB(*),FRQSES(*),MEATYP(*),IFRMAT(*),SATSES(*)
      INTEGER*4 AMBDEF(*),NSATEL(*),NUMSAT(MXCSAT,*)
      INTEGER*4 NUMAMB(*),AMBIEP(MXCAMB,*),AMBSAT(MXCAMB,*)
      INTEGER*4 AMBCLS(MXCAMB,3,*)
      INTEGER*4 AMBWLF(MXCAMB,2,*),NFRFIL(*),ISYNC(MXCSAT,*),STFIL(2,*)
      INTEGER*4 LOCQ(MXCLCQ,*),SVNFIL(MXCSAT,*),PRIOPT(*)
      INTEGER*4 SVNFI1(*),SATWGT(*)
      INTEGER*4 OBSCLS(*),ICLUST(*),IANTEN(2,*),NUMCAL(2,*),PRNCAL(*)
      INTEGER*4 NSAOFF(*),SATOFF(MXCSGR,*),NSAALB(*),SATALB(MXCSAT,*)
      INTEGER*4 OPTGIM(*),NUMRAO(2,*),PRNRAO(*),OPTDIP(*)
      INTEGER*4 OPTELI(*),STKIN(*),PARLST(5,*)
      INTEGER*4 CLKSTA(*),CLKSAT(*),NDIFF(*),OPTDCB(*)
      INTEGER*4 NOBELV(2,18,*),ITRGRD(*),NSASPV(*),SATSPV(MXCSGR,*)
      INTEGER*4 NOBNAD(2,30,*),NOBAZIS(2,36,MXCSAT,*)
      INTEGER*4 ICOELV(2,*)
      INTEGER*4 SATSHD(*),FLGSHD(*)
      INTEGER*4 CLKPAR(2,*)
      INTEGER*4 OBSEPO(MXCSAT,MXCFRQ,*)
      INTEGER*4 OBSNUM(*),NOBSPA(:,:)
      INTEGER*4 ICLU(0:MAXSYS,MXCAMB,*),NSAMPL(3),NOINCLK(3)
      INTEGER*4 IREL2
C
      REAL*8    WEIGHT(*)
C
      REAL*8    WINDOW(2,*),TIMREF(*),TIMISB(3,*)
      REAL*8    WGSSES(3,2,MXCFRQ,*),ELLSES(3,2,MXCFRQ,*)
      REAL*8    AMBIGU(MXCAMB,3,*)
      REAL*8    AMB0(MXCAMB,MXCFRQ,*),SYNCM(*),AHELP(*)
      REAL*8    SYNC(MXCSAT,*),XSTAT(3,*),AOBS(*),AAUX(*),BOBS(*)
      REAL*8    CLFRTO(2,*),TRPLIM(2,*),TRPLMS(2,*)
      REAL*8    XMAXCL(*),ANOR(*),BNOR(*),OBSERV(MXCSAT,MXCFRQ,*)
      REAL*8    OBSER1(MXCSAT,*)
      REAL*8    TPOL(2,*),TBOUND(2),TBOUND2(2),TIMWGT(2,*),WGTWGT(*)
      REAL*8    TIMSTC(3,MXCSTC,MXCSAT,*),TIMOFF(2,*)
      REAL*8    TIMSTC2(3,MXCSTC,MXCSAT,*),SCASTC(*)
      REAL*8    POLARS(MXCAMB,MXCFRQ,*)
      REAL*8    POLGIM(3,*),EPOGIM(2,*)
      REAL*8    SCAGIM(*),ELEVMM(2,*),NADIMM(2,*)
      REAL*8    POSECC(3,2,*),DXELL(3),DRELL(3)
      REAL*8    TIMSHD(2,*),XSTELL(3,*)
      REAL*8    NADMAX,NAMAX,RAPZENMAX
      REAL*8    XXX0(*),gridld(3),grdxyz(3)
      REAL*8    OFFSET(3),EX(3),EY(3),EZ(3)
C
      CHARACTER*1   IAMB1(MXCAMB,MXCFRQ,*),SVNCHR
      CHARACTER*1   OBSFLG(MXCSAT,MXCFRQ,*),OBSFL1(MXCSAT,*)
      CHARACTER*3   SVNPRI(MAXSAS)
      CHARACTER*4   SESSID,SESOLD(2),CSESS(2,*),OFFTYP
      CHARACTER*9   TMPSTR
      CHARACTER*16  STNAME(*),staNam(2)
      CHARACTER*20  RECTYP(2,*),ANTTYP(2,*),ANTCAL(2,*),ANTRAO(2,*)
      CHARACTER*19  TSTRNG2
      CHARACTER*20  MARTYP
      CHARACTER*255 TEXT
      CHARACTER(LEN=longLineLength)  :: TXTSYN
C
      LOGICAL LSTSES,cmcyn(2),cmcyn_leo(2)
C
C LOCAL DIMENSIONS
C ----------------
C
C
C
      PARAMETER (MAXDER=2)
C
      CHARACTER(LEN=6), PARAMETER :: SRNAME= 'PRCEPO'
C
      CHARACTER(LEN=fileNameLength),SAVE :: FILKIN,FILVEL,FILPRE
      CHARACTER(LEN=fileNameLength),SAVE :: FILNAM,FILSCR
      CHARACTER(LEN=staNam2Length)                      :: SATNAM
      CHARACTER*80 TITLE
      CHARACTER*16 DATUM
      CHARACTER(LEN=1), DIMENSION(:), ALLOCATABLE, SAVE :: SYNFLG        ! (MAXFLS)
      CHARACTER(LEN=1), DIMENSION(:), ALLOCATABLE, SAVE :: FILACT        ! (MAXFLS)
      CHARACTER(LEN=1), DIMENSION(:), ALLOCATABLE, SAVE :: FGFLEP        ! (MAXSAS)
      CHARACTER*1 VORZ
C
      INTEGER(i4b),SAVE  :: IRCSTD,IRCKIN,IRCVEL
C
      INTEGER(i4b), DIMENSION(:),     ALLOCATABLE, SAVE :: INDI          ! (MAXSAS*MAXFLS)
      INTEGER(i4b), DIMENSION(:),     ALLOCATABLE, SAVE :: INDK          ! (MAXSAS*MAXFLS)
C
      INTEGER(i4b), DIMENSION(:),     ALLOCATABLE, SAVE :: NSATFL        ! (MAXFLS)
      INTEGER(i4b), DIMENSION(:),     ALLOCATABLE, SAVE :: SVNEP         ! (MAXSAS)
      INTEGER(i4b), DIMENSION(:),     ALLOCATABLE, SAVE :: FILEP         ! (MAXFLS)
      INTEGER(i4b), DIMENSION(:),     ALLOCATABLE, SAVE :: SATEPO        ! (MAXSAS)
      INTEGER(i4b), DIMENSION(:),     ALLOCATABLE, SAVE :: NSFLEP        ! (MAXFLS)
      INTEGER(i4b), DIMENSION(:,:),   ALLOCATABLE, SAVE :: SAFLEP        ! (MAXSAS,MAXFLS)
      INTEGER(i4b), DIMENSION(:),     ALLOCATABLE, SAVE :: SVNMW         ! (MAXSAS)
      INTEGER(i4b),                                SAVE :: FILMW
      INTEGER(i4b),                                SAVE :: NSFLMW
      INTEGER(i4b), DIMENSION(:),     ALLOCATABLE, SAVE :: SAFLMW        ! (MAXSAS)
      INTEGER(i4b), DIMENSION(:),     ALLOCATABLE, SAVE :: SATSAV        ! (MAXSAS)
      INTEGER(i4b), DIMENSION(:,:),   ALLOCATABLE, SAVE :: SVNOBS        ! (2,MAXSAS*MAXFLS)
      INTEGER(i4b), DIMENSION(:),     ALLOCATABLE, SAVE :: IFLOBS        ! (MAXSAS*MAXFLS)
      INTEGER(i4b), DIMENSION(:),     ALLOCATABLE, SAVE :: IFRSE2        ! (MAXSAS*MAXFLS)
      INTEGER(i4b), DIMENSION(:,:),   ALLOCATABLE, SAVE :: CIND          ! (4,MAXSAS*MAXFLS)
      INTEGER(i4b), DIMENSION(:,:),   ALLOCATABLE, SAVE :: CINDO         ! (4,MAXSAS*MAXFLS)
      INTEGER(i4b), DIMENSION(:),     ALLOCATABLE, SAVE :: FIND          ! (MAXSAS*MAXFLS)
      INTEGER(i4b), DIMENSION(:),     ALLOCATABLE, SAVE :: FINDO         ! (MAXSAS*MAXFLS)
      INTEGER(i4b), DIMENSION(:),     ALLOCATABLE, SAVE :: DIFF          ! (MAXSAS*MAXFLS)
      INTEGER(i4b), DIMENSION(:),     ALLOCATABLE, SAVE :: IPAMB         ! (MAXSAS)
      INTEGER(i4b), DIMENSION(:,:,:), ALLOCATABLE, SAVE :: TECINF        ! (2,2,MAXSAS)
      INTEGER(i4b), DIMENSION(2),                  SAVE :: NTEC
      INTEGER(i4b), DIMENSION(NSTAT)                    :: KINWRI
C
      REAL(r8b),    DIMENSION(:),     ALLOCATABLE, SAVE :: COVSAT        ! (MAXSAT)
      REAL(r8b),    DIMENSION(:,:),   ALLOCATABLE, SAVE :: COVELV        ! (4,MAXSAS*MAXFLS)
C
      REAL(r8b),    DIMENSION(3),                  SAVE :: ANTECC
      REAL(r8b),    DIMENSION(3),                  SAVE :: TRUOFF
      REAL(r8b),    DIMENSION(6),                  SAVE :: WGSSAT2
      REAL(r8b),    DIMENSION(3),                  SAVE :: WGSSATHLP
      REAL(r8b),    DIMENSION(3,2),                SAVE :: WGSAPR
      REAL(r8b),    DIMENSION(:,:),   ALLOCATABLE, SAVE :: DTFIL         ! (2,MAXFLS)
      REAL(r8b),    DIMENSION(:,:),   ALLOCATABLE, SAVE :: BOLD          ! (MAXSAT,MAXFLS)
      REAL(r8b),    DIMENSION(:),     ALLOCATABLE, SAVE :: DERCLK        ! (MAXSAS)
      REAL(r8b),    DIMENSION(2),                  SAVE :: DERHLP
      REAL(r8b),    DIMENSION(:,:),   ALLOCATABLE, SAVE :: OBFLEP        ! (MAXSAS,MAXFLS)
      REAL(r8b),    DIMENSION(MAXMEA),             SAVE :: WGTGEN
      REAL(r8b),    DIMENSION(9,2),                SAVE :: WGSEPO
      REAL(r8b),    DIMENSION(9,2),                SAVE :: WGSSAT
      REAL(r8b),    DIMENSION(:),     ALLOCATABLE, SAVE :: OBFLMW        ! (MAXSAS)
      REAL(r8b),    DIMENSION(:),     ALLOCATABLE, SAVE :: CODDIF        ! (MAXSAS)
      REAL(r8b),    DIMENSION(:,:),   ALLOCATABLE, SAVE :: XVA           ! ((MAXDER+1)*3,MAXSAS)
      REAL(r8b),    DIMENSION(:,:),   ALLOCATABLE, SAVE :: ELE           ! (7,MAXSAS)
      REAL(r8b),    DIMENSION((MAXDER+1)*3)             :: XVAEP
      REAL(r8b),    DIMENSION(3,2),                SAVE :: TOPPOS
      REAL(r8b),    DIMENSION(2),                  SAVE :: DIST
      REAL(r8b),    DIMENSION(2),                  SAVE :: ZEN
      REAL(r8b),    DIMENSION(2),                  SAVE :: AZI
      REAL(r8b),    DIMENSION(2),                  SAVE :: ZENION
      REAL(r8b),    DIMENSION(2),                  SAVE :: DPOLAR
      REAL(r8b),    DIMENSION(3,2),                SAVE :: TOPPOS2
      REAL(r8b),    DIMENSION(2),                  SAVE :: ZEN2
      REAL(r8b),    DIMENSION(2),                  SAVE :: AZI2
      REAL(r8b),    DIMENSION(:),     ALLOCATABLE, SAVE :: HADD          ! (MAXSAS*MAXFLS)
      REAL(r8b),    DIMENSION(:,:),   ALLOCATABLE, SAVE :: ELEVA         ! (2,MAXSAT)
      REAL(r8b),    DIMENSION(:,:),   ALLOCATABLE, SAVE :: AZIMU         ! (2,MAXSAT)
      REAL(r8b),    DIMENSION(:,:),   ALLOCATABLE, SAVE :: TECOBS        ! (2,MAXSAS)
      REAL(r8b),    DIMENSION(2),                  SAVE :: TECEPO
      REAL(r8b),    DIMENSION(:),     ALLOCATABLE, SAVE :: SYNCMR        ! (MAXFLS)
      REAL(r8b),    DIMENSION(2,999),              SAVE :: FACTMW
      REAL(r8b),    DIMENSION(999),                SAVE :: FACTL5
      REAL(r8b),    DIMENSION(999),                SAVE :: FACTP6
      REAL(r8b),    DIMENSION(999),                SAVE :: FRMSMW
      REAL(r8b),    DIMENSION(3),                  SAVE :: XKIN
      REAL(r8b),    DIMENSION(3),                  SAVE :: XKIN2
      REAL(r8b),    DIMENSION(3),                  SAVE :: XKINHLP
      REAL(r8b),    DIMENSION(3),                  SAVE :: XVEL
      REAL(r8b),    DIMENSION(6),                  SAVE :: XKV
      REAL(r8b),    DIMENSION(9),                  SAVE :: XTMP
      REAL(r8b),    DIMENSION(9),                  SAVE :: XSLRD1
      REAL(r8b),    DIMENSION(9),                  SAVE :: XSLRD2
      REAL(r8b),    DIMENSION(6),                  SAVE :: XSLRK1
      REAL(r8b),    DIMENSION(6),                  SAVE :: XSLRK2
      REAL(r8b),    DIMENSION(7),                  SAVE :: DUMMY7
      REAL(r8b),    DIMENSION(3),                  SAVE :: SLROFF
      REAL(r8b),    DIMENSION(3),                  SAVE :: XELEPO
      REAL(r8b),    DIMENSION(3),                  SAVE :: EXC
      REAL(r8b),    DIMENSION(2),                  SAVE :: NAD
      REAL(r8b),    DIMENSION(2),                  SAVE :: AZISAT
      REAL(r8b),                                   SAVE :: NADI
      REAL(r8b),    DIMENSION(2),                  SAVE :: NAD2
      REAL(r8b),    DIMENSION(2),                  SAVE :: AZISA2
      REAL(r8b),    DIMENSION(2),                  SAVE :: AZISOK
      REAL(r8b),    DIMENSION(2),                  SAVE :: AZISO2
      REAL(r8b),                                   SAVE :: SATCLK
      REAL(r8b),    DIMENSION(2),                  SAVE :: recClk
C
      INCLUDE 'COMFREQ.inc'
C
      DATA IFIRST/0/,IKINSC/0/,IFRPRI/1/,SESOLD/'****','****'/
C
C CHECK MAXIMUM LOCAL DIMENSIONS
C ------------------------------
      IF(IFIRST.EQ.0) THEN
        CALL MAXTST(0,'PRCEPO',MXNSAS,MAXSAS,MXCSAS,IRC1)
        CALL MAXTST(0,'PRCEPO',MXNFLS,MAXFLS,MXCFLS,IRC2)
        CALL MAXTST(1,'PRCEPO',MXNSAT,MAXSAT,MXCSAT,IRC3)
        IF(IRC1.NE.0.OR.IRC2.NE.0.OR.IRC3.NE.0) CALL EXITRC(2)
C
        WGTGEN(1)=1.D0
        WGTGEN(2)=WGTCOD/WGTPHA
        WGTGEN(3)=1.D0
C
        EDTLV0=EDTLVL
C
C ALLOCATE LOCAL ARRAYS
C ---------------------
        ALLOCATE(SYNFLG(MAXFLS),STAT=IAC)
        CALL ALCERR(IAC,'SYNFLG',(/MAXFLS/),SRNAME)
        SYNFLG=''
C
        ALLOCATE(FILACT(MAXFLS),STAT=IAC)
        CALL ALCERR(IAC,'FILACT',(/MAXFLS/),SRNAME)
        FILACT=''
C
        ALLOCATE(FGFLEP(MAXSAS),STAT=IAC)
        CALL ALCERR(IAC,'FGFLEP',(/MAXSAS/),SRNAME)
        FGFLEP=''
C
        ALLOCATE(INDI(MAXSAS*MAXFLS*MAXFRS),STAT=IAC)
        CALL ALCERR(IAC,'INDI',(/MAXSAS*MAXFLS*MAXFRS/),SRNAME)
        INDI=0
C
        ALLOCATE(INDK(MAXSAS*MAXFLS*MAXFRS),STAT=IAC)
        CALL ALCERR(IAC,'INDK',(/MAXSAS*MAXFLS*MAXFRS/),SRNAME)
        INDK=0
C
        ALLOCATE(NSATFL(MAXFLS),STAT=IAC)
        CALL ALCERR(IAC,'NSATFL',(/MAXFLS/),SRNAME)
        NSATFL=0
C
        ALLOCATE(SVNEP(MAXSAS),STAT=IAC)
        CALL ALCERR(IAC,'SVNEP',(/MAXSAS/),SRNAME)
        SVNEP=0
C
        ALLOCATE(FILEP(MAXFLS),STAT=IAC)
        CALL ALCERR(IAC,'FILEP',(/MAXFLS/),SRNAME)
        FILEP=0
C
        ALLOCATE(SATEPO(MAXSAS),STAT=IAC)
        CALL ALCERR(IAC,'SATEPO',(/MAXSAS/),SRNAME)
        SATEPO=0
C
        ALLOCATE(NSFLEP(MAXFLS),STAT=IAC)
        CALL ALCERR(IAC,'NSFLEP',(/MAXFLS/),SRNAME)
        NSFLEP=0
C
        ALLOCATE(SAFLEP(MAXSAS,MAXFLS),STAT=IAC)
        CALL ALCERR(IAC,'SAFLEP',(/MAXSAS,MAXFLS/),SRNAME)
        SAFLEP=0
C
        ALLOCATE(SVNMW(MAXSAS),STAT=IAC)
        CALL ALCERR(IAC,'SVNMW',(/MAXSAS/),SRNAME)
        SVNMW=0
C
        ALLOCATE(SAFLMW(MAXSAS),STAT=IAC)
        CALL ALCERR(IAC,'SAFLMW',(/MAXSAS/),SRNAME)
        SAFLMW=0
C
        ALLOCATE(SATSAV(MAXSAS),STAT=IAC)
        CALL ALCERR(IAC,'SATSAV',(/MAXSAS/),SRNAME)
        SATSAV=0
C
        ALLOCATE(SVNOBS(2,MAXSAS*MAXFLS*MAXFRS),STAT=IAC)
        CALL ALCERR(IAC,'SVNOBS',(/2,MAXSAS*MAXFLS*MAXFRS/),SRNAME)
        SVNOBS=0
C
        ALLOCATE(IFLOBS(MAXSAS*MAXFLS*MAXFRS),STAT=IAC)
        CALL ALCERR(IAC,'IFLOBS',(/MAXSAS*MAXFLS*MAXFRS/),SRNAME)
        IFLOBS=0
C
        ALLOCATE(IFRSE2(MAXSAS*MAXFLS*MAXFRS),STAT=IAC)
        CALL ALCERR(IAC,'IFRSE2',(/MAXSAS*MAXFLS*MAXFRS/),SRNAME)
        IFRSE2=0
C
        ALLOCATE(CIND(4,MAXSAS*MAXFLS*MAXFRS),STAT=IAC)
        CALL ALCERR(IAC,'CIND',(/4,MAXSAS*MAXFLS*MAXFRS/),SRNAME)
        CIND=0
C
        ALLOCATE(CINDO(4,MAXSAS*MAXFLS*MAXFRS),STAT=IAC)
        CALL ALCERR(IAC,'CINDO',(/4,MAXSAS*MAXFLS*MAXFRS/),SRNAME)
        CINDO=0
C
        ALLOCATE(FIND(MAXSAS*MAXFLS*MAXFRS),STAT=IAC)
        CALL ALCERR(IAC,'FIND',(/MAXSAS*MAXFLS*MAXFRS/),SRNAME)
        FIND=0
C
        ALLOCATE(FINDO(MAXSAS*MAXFLS*MAXFRS),STAT=IAC)
        CALL ALCERR(IAC,'FINDO',(/MAXSAS*MAXFLS*MAXFRS/),SRNAME)
        FINDO=0
C
        ALLOCATE(DIFF(MAXSAS*MAXFLS*MAXFRS),STAT=IAC)
        CALL ALCERR(IAC,'DIFF',(/MAXSAS*MAXFLS*MAXFRS/),SRNAME)
        DIFF=0
C
        ALLOCATE(IPAMB(MAXSAS),STAT=IAC)
        CALL ALCERR(IAC,'IPAMB',(/MAXSAS/),SRNAME)
        IPAMB=0
C
        ALLOCATE(TECINF(2,2,MAXSAS),STAT=IAC)
        CALL ALCERR(IAC,'TECINF',(/2,2,MAXSAS/),SRNAME)
        TECINF=0
C
        ALLOCATE(COVSAT(MAXSAT),STAT=IAC)
        CALL ALCERR(IAC,'COVSAT',(/MAXSAT/),SRNAME)
        COVSAT=0D0
C
        ALLOCATE(COVELV(4,MAXSAS*MAXFLS*MAXFRS),STAT=IAC)
        CALL ALCERR(IAC,'COVELV',(/4,MAXSAS*MAXFLS*MAXFRS/),SRNAME)
        COVELV=0D0
C
        ALLOCATE(DTFIL(2,MAXFLS),STAT=IAC)
        CALL ALCERR(IAC,'DTFIL',(/2,MAXFLS/),SRNAME)
        DTFIL=0D0
C
        ALLOCATE(BOLD(MAXSAT,MAXFLS),STAT=IAC)
        CALL ALCERR(IAC,'BOLD',(/MAXSAT,MAXFLS/),SRNAME)
        BOLD=0D0
C
        ALLOCATE(DERCLK(MAXSAS),STAT=IAC)
        CALL ALCERR(IAC,'DERCLK',(/MAXSAS/),SRNAME)
        DERCLK=0D0
C
        ALLOCATE(OBFLEP(MAXSAS,MAXFLS),STAT=IAC)
        CALL ALCERR(IAC,'OBFLEP',(/MAXSAS,MAXFLS/),SRNAME)
        OBFLEP=0D0
C
        ALLOCATE(OBFLMW(MAXSAS),STAT=IAC)
        CALL ALCERR(IAC,'OBFLMW',(/MAXSAS/),SRNAME)
        OBFLMW=0D0
C
        ALLOCATE(CODDIF(MAXSAS),STAT=IAC)
        CALL ALCERR(IAC,'CODDIF',(/MAXSAS/),SRNAME)
        CODDIF=0D0
C
        ALLOCATE(XVA((MAXDER+1)*3,MAXSAS),STAT=IAC)
        CALL ALCERR(IAC,'XVA',(/(MAXDER+1)*3,MAXSAS/),SRNAME)
        XVA=0D0
C
        ALLOCATE(ELE(7,MAXSAS),STAT=IAC)
        CALL ALCERR(IAC,'ELE',(/7,MAXSAS/),SRNAME)
        ELE=0D0
C
        ALLOCATE(HADD(MAXSAS*MAXFLS*MAXFRS),STAT=IAC)
        CALL ALCERR(IAC,'HADD',(/MAXSAS*MAXFLS*MAXFRS/),SRNAME)
        HADD=0D0
C
        ALLOCATE(ELEVA(2,MAXSAT),STAT=IAC)
        CALL ALCERR(IAC,'ELEVA',(/2,MAXSAT/),SRNAME)
        ELEVA=0D0
C
        ALLOCATE(AZIMU(2,MAXSAT),STAT=IAC)
        CALL ALCERR(IAC,'AZIMU',(/2,MAXSAT/),SRNAME)
        AZIMU=0D0
C
        ALLOCATE(TECOBS(2,MAXSAS),STAT=IAC)
        CALL ALCERR(IAC,'TECOBS',(/2,MAXSAS/),SRNAME)
        TECOBS=0D0
C
        ALLOCATE(SYNCMR(MAXFLS),STAT=IAC)
        CALL ALCERR(IAC,'SYNCMR',(/MAXFLS/),SRNAME)
        SYNCMR=0D0
C
C GET LEO ORBIT INFORMATION
C -------------------------
        CALL GTFLNA(0,'LEOSTD ',FILNAM,IRCSTD)
        CALL GTFLNA(0,'KININP ',FILKIN,IRCKIN)
        CALL GTFLNA(0,'KINVEL ',FILVEL,IRCVEL)
        CALL GTFLNA(0,'LEOPRE ',FILPRE,IRCPRE)
C        CALL GTFLNA(0,'SLROCEF',KINRES,IWSLROCEF)
        IFIRST=1
      ENDIF

C
C SET LOGICAL I/O NUMBERS
C -----------------------
      LFNKSC=LFN002+3
C
      IF(SESSID.NE.SESOLD(1)) THEN
        DO 5 ISAT=1,NSASES
          FACTMW(1,SATSES(ISAT))=FRQ(1,SATSES(ISAT))/
     1               (FRQ(1,SATSES(ISAT))+FRQ(2,SATSES(ISAT)))
          FACTMW(2,SATSES(ISAT))=FRQ(2,SATSES(ISAT))/
     1               (FRQ(1,SATSES(ISAT))+FRQ(2,SATSES(ISAT)))
          FACTL5(SATSES(ISAT))=FACLIN(5,1,SATSES(ISAT))**2+
     1                         FACLIN(5,2,SATSES(ISAT))**2
          FACTP6(SATSES(ISAT))=FACTMW(1,SATSES(ISAT))**2+
     1                           FACTMW(2,SATSES(ISAT))**2
          FRMSMW(SATSES(ISAT))=FACTL5(SATSES(ISAT))/
     1                         (FACTL5(SATSES(ISAT))+
     1                         WGTPHA/WGTCOD*FACTP6(SATSES(ISAT)))
5       CONTINUE
      ENDIF
C
C INITIALIZE ELEVATIONS TO BE PRINTED FOR FIRST STATION OF SESSION
      IPREPO=1
C
C INITIALIZE WRITING OF AUX. FILE KINSCR
      KINWRI(1:nstat)=1
C
C MAXIMUM NUMBER OF SIMULTANEOUS SINGLE DIFFERENCE EQUATIONS
      MAXEQN=MAXSAS*MAXFLS*MAXFRS+1
C
C INITIALIZE EPOCH PROCESSING
C ---------------------------
      CALL INIEPO(MAXSAS,MAXFLS,SYNFLG,ELEVA,LOCQ)
C
C GET ALL SIMULTANEOUS OBSERVATIONS OF NEXT EPOCH (WITH SAMPLING)
C ---------------------------------------------------------------
40    CONTINUE
        CALL GOBSEP(1     ,NFLSES,FILNUM,TLAST ,WINDOW,DTSIM ,
     1              TIMREF,IDELTT,IFRMAT,MEATYP,NSATEL,NUMSAT,
     2              NFRFIL,ICARR ,NSHD  ,SATSHD,TIMSHD,FLGSHD,
     3              TOBS  ,OBSNUM,NSATFL,SVNFIL,OBSFLG,OBSERV,
     4              SVNFI1,OBSFL1,OBSER1,DTFIL ,FILACT,NDIFF ,
     5              NOINCLK(1),SECIPL,STNAME,STFIL ,IRETC)
        IF(IRETC.EQ.1) GOTO 999
        IF(NSAMPL(1).NE.0) THEN
          CALL MJDGPS(TOBS,GPSSEC,NWEEK)
          TFRAC=GPSSEC-DNINT(GPSSEC/NSAMPL(1))*NSAMPL(1)
          IF(DABS(TFRAC).GT.DTSIM*86400.D0) GOTO 40
        ENDIF
C
C SET UP EPOCH-SPECIFIC PARAMETERS
C --------------------------------
      IF (IEPPAR.EQ.1) THEN
        NPAEPO=0
        IF(NSAMPL(3).NE.0) THEN
          CALL MJDGPS(TOBS,GPSSEC,NWEEK)
          TFRAC=GPSSEC-DNINT(GPSSEC/NSAMPL(3))*NSAMPL(3)
        ENDIF
        IF(DABS(TFRAC).GT.DTSIM*86400.D0 .OR. NSAMPL(3).EQ.0) THEN
          CALL SEQEPO(OPTELI,NFLSES,FILNUM,FILACT,NSATFL,SVNFIL,
     1                NFRFIL,ICARR ,OBSFLG,OBSERV,OBSNUM,NSTAT ,
     2                ICENTR,NKIN  ,STKIN ,NPAR  ,NPARMS,PARLST,
     3                LOCQ  ,ANOR  ,BNOR  ,PARTYP,NOBSPA,NPAEPO,
     4                STFIL ,NCLKST,CLKSTA,NCLKSA,CLKSAT,CLKHED,
     5                STNAME,NDIFF ,MEATYP,ISASYS,CLKSYS)
        ENDIF
      ENDIF
C
C SET SATELLITE SPECIFIC WEIGHTS
C ------------------------------
      CALL WGTSAT(NWGT,SATWGT,TIMWGT,WGTWGT,TOBS,NSASES,SATSES,
     1            COVSAT)
C
C LOOP OVER MEASUREMENT TYPES
C ---------------------------
      NSAEPO=0
      DO 500 MEA=1,3
C
C NO MEA=2 FOR MELBOURNE-WUEBBENA
        IF (MELWUB.EQ.1.AND.MEA.EQ.2) GOTO 500
        IF (IZEROD.EQ.1.AND.MEA.NE.1) GOTO 500
C
C LOOP OVER FREQUENCIES
C ---------------------
        NDBLE=0
        MXESNG=0
        DO 449 I=1,MAXSAS
          CODDIF(I)=0.D0
449     CONTINUE
        DO 450 IFREQ=1,NFRSES
C
C INITIALIZATION FOR CORRELATIONS WITHIN FREQUENCY
C ------------------------------------------------
          IF(CORSTR.LE.2) THEN
            NDBLE=0
            MXESNG=0
          ENDIF
C
C INITIALIZE ANTENNA OFFSET REQUEST FOR GETORB (MEA=3: SLR --> IANT=2)
C (ALSO USED FOR NADIR-DEPENDENT SATELLITE ANTENNA PHASE CENTER CORR.)
C --------------------------------------------------------------------
          IANT=1
          IF (MEA.EQ.3) IANT=2
C
C GET ALL OBSERVATIONS OF THE FREQUENCY TO BE PROCESSED
C -----------------------------------------------------
          IFRSES=FRQSES(IFREQ)
          CALL GFRQEP(IFRSES,NFLSES,FILACT,NSATFL,SVNFIL,OBSFLG,OBSERV,
     1                NFRFIL,ICARR ,FILNUM,MEATYP,MEA   ,IZEROD,MAXSAS,
     2                ISASYS,NSATEP,NFILEP,SVNEP ,FILEP ,NSFLEP,SAFLEP,
     3                OBFLEP)
          IF(NFILEP.EQ.0) GOTO 410
C
C GET CODE OBSERVATIONS TO FORM MELBOURNE-WUEBBENA
C ------------------------------------------------
          IF (MELWUB.EQ.1) THEN
            MEAMW=2
            DO 830 IMW=1,2
              CALL GFRQEP(IMW   ,NFLSES,FILACT,NSATFL,SVNFIL,OBSFLG,
     1                    OBSERV,NFRFIL,ICARR ,FILNUM,MEATYP,MEAMW ,
     2                    IZEROD,MAXSAS,ISASYS,NSATMW,NFILMW,SVNMW ,
     3                    (/FILMW/),(/NSFLMW/),SAFLMW,OBFLMW)
              IF(NFILMW.EQ.0.OR.NSATMW.LT.2) GOTO 410
C
              DO 820 ISATEP=1,NSATEP
                IF (IMW.EQ.2 .AND. FGFLEP(ISATEP).EQ.'B') GO TO 820
                FGFLEP(ISATEP)='B'
                DO 810 ISATMW=1,NSATMW
                  IF (SVNMW(ISATMW).EQ.SVNEP(ISATEP)) THEN
                    OBFLEP(ISATEP,1)=OBFLEP(ISATEP,1)-
     1                      FACTMW(IMW,SVNEP(ISATEP))*OBFLMW(ISATMW)
                    FGFLEP(ISATEP)='G'
                    CODDIF(ISATEP)=CODDIF(ISATEP)+(-1.D0)**IMW*
     1                             OBFLMW(ISATMW)
                    IF (IMW.EQ.2 .AND. EDTLVL.GT.0.D0) THEN
                      IF (DABS(CODDIF(ISATEP)).GT.EDTLVL) THEN
                        FGFLEP(ISATEP)='B'
                        WRITE(LFNPRT,104) SVNEP(ISATEP),OBSNUM(2),
     1                                    CODDIF(ISATEP)
104                     FORMAT(' ### SR PRCEPO: CODE MEASUREMENT ',
     1                    'REJECTED',/,
     2                    16X,'SATELLITE:',I6,/,
     3                    16X,'EPOCH    :',I6,/,
     4                    16X,'L1-L2    :',F12.1)
                      ENDIF
                    ENDIF
                    GOTO 820
                  ENDIF
810             CONTINUE
820           CONTINUE
830         CONTINUE
C
C REMOVE SATELLITE WITHOUT DUAL CODE OBSERVATIONS (FGFLEP='B')
            NSATNW=0
            DO 840 ISATEP=1,NSATEP
              IF (FGFLEP(ISATEP).EQ.'B') GOTO 840
              NSATNW=NSATNW+1
              SVNEP(NSATNW)=SVNEP(ISATEP)
              SAFLEP(NSATNW,1)=SAFLEP(ISATEP,1)
              OBFLEP(NSATNW,1)=OBFLEP(ISATEP,1)
840         CONTINUE
            NSATEP=NSATNW
            NSFLEP(1)=NSATNW
          ENDIF
C
C SATELLITE POSITIONS
C -------------------
          DO 10 ISAT=1,NSATEP
C
C FIND SATELLITE IN EPOCH SATELLITE LIST
            DO 15 KSAT=1,NSAEPO
              IF(SATEPO(KSAT).EQ.SVNEP(ISAT)) GOTO 10
15          CONTINUE
C
C NEW SATELLITE FOR EPOCH SATELLITE LIST
            NSAEPO=NSAEPO+1
            IF(NSAEPO.GT.MAXSAS) THEN
              CALL TIMST2(1,1,TOBS,TSTRNG2)
              WRITE(LFNERR,901) NSAEPO,MAXSAS,SESSID,TSTRNG2
901           FORMAT(/,' *** SR PRCEPO: TOO MANY SATELLITES PER EPOCH',
     1                         /,16X,'# SATELLITES PER EPOCH >=',I5,
     2                         /,16X,'MAX. # SATELLITES       :',I5,
     3                         /,16X,'SESSION IDENTFIER       : ',A4,
     4                         /,16X,'EPOCH                   : ',A19,/)
              CALL EXITRC(2)
            ENDIF
            SATEPO(NSAEPO)=SVNEP(ISAT)
C
C CHECK FOR SATELLITE SYSTEM
C --------------------------
            CALL SVN2CHR(SVNEP(ISAT),ISVMOD,SVNCHR)
C
C GET ORBIT FOR SATELLITE SYSTEM
C ------------------------------
            IF (SVNCHR.NE.'L' .OR.
     1          (SVNCHR.EQ.'L' .AND. SVNEP(ISAT).GE.951) ) THEN
              CALL GETORB(SVNEP(ISAT),IANT,MAXDER,2,TOBS,ICRARC,
     1                    IORSYS,XVA(1,NSAEPO),TOSC,ELE(1,NSAEPO),IRC,
     2                    cmcyn=cmcyn)
              IF (IRC.NE.0) THEN
                NSAEPO=NSAEPO-1
                GOTO 10
              ENDIF
              CALL COOTRA(IORSYS,MAXDER,TOBS,XVA(1,NSAEPO),
     1                    SZ,XPOL,YPOL,UT1GPS)
            ELSE IF (SVNCHR.EQ.'L' .AND. SVNEP(ISAT).LT.951) THEN
              CALL GTSENSOR(SVNEP(ISAT),TOBS,typeSLR,SATNAM)
C
              IF (MEA.EQ.3) IANTL=0
              CALL GETORF(FILNAM,SVNEP(ISAT),IANTL,MAXDER,2,TOBS,
     1                    ICRARC,IORSYS,XVA(1,NSAEPO),TOSC,
     2                    ELE(1,NSAEPO),IRC)
              IF (IRC.NE.0) THEN
                NSAEPO=NSAEPO-1
                GOTO 10
              ENDIF
              CALL COOTRA(IORSYS,MAXDER,TOBS,XVA(1,NSAEPO),
     1                    SZ,XPOL,YPOL,UT1GPS)
              CALL LEOANTE2(SVNEP(ISAT),SATNAM,TOBS,
     1                      ANTECC,1,XVA(1,NSAEPO),SLROFF)
              IF (IRCPRE.NE.0) THEN
                DO II=1,3
                  XVA(II,NSAEPO)=XVA(II,NSAEPO)+SLROFF(II)
                END DO
              END IF
C
C USE PRECISE ORBIT FOR LEO
C -------------------------
              IF (IRCPRE.EQ.0) THEN
                CALL READDT(FILPRE,DTVALS)
                DTVAL=DTVALS/86400.D0
                TVAL1=TOBS-DMOD(TOBS,DTVAL)
                TVAL2=TVAL1+DTVAL
C
                CALL GETORF(FILNAM,SVNEP(ISAT),IANTL,1,2,TVAL1,IDUMMY,
     1                      IDUMMY,XSLRD1,DUMMY,DUMMY7,IRCVAL1)
                CALL GETORF(FILNAM,SVNEP(ISAT),IANTL,1,2,TVAL2,IDUMMY,
     1                      IDUMMY,XSLRD2,DUMMY,DUMMY7,IRCVAL2)
                CALL COOTRA(IORSYS,0,TVAL1,XSLRD1,
     1                      SZ1,XPOL1,YPOL1,UT1GPS1)
                CALL COOTRA(IORSYS,0,TVAL2,XSLRD2,
     1                      SZ2,XPOL2,YPOL2,UT1GPS2)
                IF (IRCVAL1.NE.0 .OR. IRCVAL2.NE.0) THEN
                  NSAEPO=NSAEPO-1
                  GOTO 10
                END IF
C
                CALL READPRE(FILPRE,SVNEP(ISAT),TVAL1,0,
     1                       XSLRK1,IRCVAL1)
                CALL READPRE(FILPRE,SVNEP(ISAT),TVAL2,0,
     1                       XSLRK2,IRCVAL2)

                IF (IRCVAL1.NE.0 .OR. IRCVAL2.NE.0) THEN
                  NSAEPO=NSAEPO-1
                  GOTO 10
                END IF
C SLR CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                ELSE IF (ISLRVAL.EQ.3 .AND. IRCKIN.EQ.0) THEN
C                CALL LEOSTA(SVNEP(ISAT),TVAL1,LEONAME)
C                CALL READKIN(FILKIN,LEONAME,
C     1                       TVAL1,0,0,XKIN,IRCVAL1)
C                CALL LEOSTA(SVNEP(ISAT),TVAL1,LEONAME)
C                CALL READKIN(FILKIN,LEONAME,
C     1                       TVAL2,0,0,XKIN2,IRCVAL2)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
                CALL TRUEARTH(XSLRK1,XSLRK1,1,0,SZ1,XPOL1,YPOL1)
                CALL LEOANTE2(SVNEP(ISAT),SATNAM,TVAL1,
     1                        ANTECC,1,XSLRD1,SLROFF)
                XSLRK1(1:3)=XSLRK1(1:3)+SLROFF(1:3)
C
                CALL TRUEARTH(XSLRK2,XSLRK2,1,0,SZ2,XPOL2,YPOL2)
                CALL LEOANTE2(SVNEP(ISAT),SATNAM,TVAL2,
     1                        ANTECC,1,XSLRD2,SLROFF)
                XSLRK2(1:3)=XSLRK2(1:3)+SLROFF(1:3)
C
                DO IVAL=1,3
                  DXVAL1=XSLRK1(IVAL)-XSLRD1(IVAL)
                  DXVAL2=XSLRK2(IVAL)-XSLRD2(IVAL)
                  XVA(IVAL,ISAT)=XVA(IVAL,ISAT)+DXVAL1+
     1                           (TOBS-TVAL1)*(DXVAL2-DXVAL1)/DTVAL
                END DO
              END IF
            END IF
10        CONTINUE
C
C SET UP ALL DOUBLE DIFFERENCE EQUATIONS OF THE EPOCH
C (FOR ONE FREQUENCY AND ONE MEASUREMENT TYPE)
C ---------------------------------------------------
          DO 400 IFIL=1,NFILEP
            IFLSES=FILEP(IFIL)
            IF=FILNUM(IFLSES)
C
C FREQUENCY INDEX
            DO 21 IFRQ=1,NFRFIL(IF)
              IF(ICARR(IFRQ,IF).EQ.IFRSES) GOTO 22
21          CONTINUE
22          ISING=0
C
C CORRECT STATION COORDINATES FOR TIME-DEPENDENT EFFECTS (E.G. TIDES)
C -------------------------------------------------------------------
            DO 25 IST=1,NDIFF(IF)+1
C
C HANDLE STATION TYPES
C --------------------
              CALL STAFLG(STNAME(STFIL(IST,IF)),TOBS,IFLAG,MARTYP)
C
C CORRECTION FOR EARTH TIDES
C --------------------------
              IF (MARTYP.EQ.' ') THEN
                CALL XYZTIM(IORSYS,TOBS,WGSSES(1,IST,IFRQ,IFLSES),
     1                      STNAME(STFIL(IST,IF)),WGSEPO(:,IST),
     2                      cmcyn)
C
C CONSIDER CORRECTIONS FROM VIENNA GRID FILES
C -------------------------------------------
                GRIDLD(:) = 0D0
                DO II = 1, SIZE(GRDNEQ)-1
                  DO JJ = 1,3
                    GRIDLD(JJ) = GRIDLD(JJ) +
     1                GETGRID(GETGRIDKEYW(GRDNEQ(II),JJ),TOBS,
     2                        XSTELL(1:3,STFIL(IST,IF)))
                  ENDDO
                ENDDO
C REORDER FROM "UNE" TO "NEU"
                RHLP = GRIDLD(1)
                GRIDLD(1) = GRIDLD(2)
                GRIDLD(2) = GRIDLD(3)
                GRIDLD(3) = RHLP
C
                CALL ELLECC(XSTELL(1:3,STFIL(IST,IF)),GRIDLD,GRDXYZ)
                WGSEPO(1:3,IST)=WGSEPO(1:3,IST)+GRDXYZ(1:3)
              ELSE
                WGSEPO(1:3,IST)=WGSSES(1:3,IST,IFRQ,IFLSES)
              END IF
C
C HANDLE KINEMATIC COORDINATES FOR:
C ---------------------------------
              ISTKIN = 0
              DO IKIN = 1,NKIN
                IF (STFIL(IST,IF) == STKIN(IKIN)) ISTKIN = 1
              ENDDO
C
C 1) KINEMATICS OF "STATIC" STATION
C ---------------------------------
              IF (.NOT. (MARTYP.EQ.MTypeSPACE)) THEN
                zMax=zenMax
c              IF (MARTYP.EQ.' '.OR.
c     1            MARTYP.EQ.MTypeAIR .OR. MARTYP.EQ.MTypeSHIP) THEN
C
C               TRY TO FIND COORDINATES FROM KININP
                IF (IRCKIN==0) THEN
                  CALL READKIN(FILKIN,STNAME(STFIL(IST,IF)),
     1                          TOBS,1-ISTKIN,0,XKIN,IRCEPO)

C
C                 POSITION FOUND: USE IT INSTEAD OF XSTAT
                  IF (IRCEPO.EQ.0) THEN
                    DO K=1,3
                      WGSEPO(K,IST)=WGSEPO(K,IST)+
     1                  (XKIN(K)-XSTAT(K,STFIL(IST,IF)))
                    END DO
                    WGSAPR(1:3,IST)=XKIN(1:3)
C
C                 STATION NOT IN KININP (STATIC STATION ASSUMED)
                  ELSEIF (IRCEPO.EQ.2 .AND. MARTYP.EQ.' ') THEN
                    KINWRI(STFIL(IST,IF))=2
C
C                 STATION NOT IN KININP (SHIP OR AIR)
                  ELSEIF (IRCEPO.EQ.2) THEN
                    KINWRI(STFIL(IST,IF))=2
                    WRITE(LFNPRT,1022)TOBS,MARTYP
1022                  FORMAT(/,' ### SR PRCEPO: ',
     1                'A PRIORI POSITION TAKEN FROM COORD. FILE, ',
     2                'EPOCH/MARTYP: ',F15.6,A20/)
C
C                 NO POSITION "K" FOUND, BUT COORDINATE WILL NOT BE IMPROVED
C                 SKIP ALL OBSERVATIONS FOR THIS EPOCH FROM THIS STATION
                  ELSE IF (ISTKIN == 0) THEN
                    WRITE(LFNERR,
     .              '(/,A,/,16X,A,/,16X,2A,/,16X,A,F15.7,/,16X,A,I7,/)')
     1              ' ### SR PRCEPO: ' //
     2              'No kinematic apriori coordinates found.',
     3              'The observations from file have to be skipped' //
     4              ' for this epoch.',
     5              'Station name: ',TRIM(STNAME(STFIL(IST,IF))),
     6              'Epoch:        ',TOBS,
     7              'File number:  ',IF
                    GOTO 400
                  ELSE
                    WGSAPR(1:3,IST)=XSTAT(1:3,STFIL(IST,IF))
                  END IF
                ELSE
                  WGSAPR(1:3,IST)=XSTAT(1:3,STFIL(IST,IF))
                END IF
                GOTO 34
              END IF
C
C 2) AIRBORNE, SHIP, GROUND
C -------------------------
              IF (MARTYP.EQ.MTypeAIR .OR. MARTYP.EQ.MTypeSHIP) THEN
                IRCEPO3 = 1
                IF (IRCKIN.EQ.0) THEN
                  CALL  READKIN(FILKIN,STNAME(STFIL(IST,IF)),
     1                          TOBS,1,0,WGSEPO(1:3,IST),IRCEPO3)
                  IF (IRCEPO3.EQ.0) THEN
                    WGSAPR(1:3,IST)=WGSEPO(1:3,IST)
                    GOTO 34
                  END IF
                END IF
C USE EXISTING STATION COORDINATES
C --------------------------------
                WGSEPO(1:3,IST)=WGSSES(1:3,IST,IFRQ,IFLSES)
                WGSAPR(1:3,IST)=XSTAT(1:3,STFIL(IST,IF))
                WRITE(LFNPRT,1023)TOBS,MARTYP
1023              FORMAT(/,' *** SR PRCEPO: ',
     1            'A PRIORI POSITION TAKEN FROM COORD. FILE, ',
     2            'EPOCH/MARTYP: ',F15.6,A20/)
                GOTO 34
              END IF
C
C 3) LEO
C ------
              IF (MARTYP.EQ.MTypeSPACE) THEN
                zMax=zmxLeo
C
C 3.1) GET LEO A PRIORI COORDINATES FROM THE FILE "KININP"
C --------------------------------------------------------
                IRCEPO2=1
                IRCVEL2=1
                IF (IRCKIN==0) THEN
                  CALL READKIN(FILKIN,STNAME(STFIL(IST,IF)),
     1                          TOBS,1,0,XKIN,IRCEPO2)
                END IF
                IF (IRCVEL==0) THEN
                  DATUM(1:16)=' '
                  CALL READVEL(FILVEL,STNAME(STFIL(IST,IF)),
     1                         TOBS,1,0,XVEL,IRCVEL2)
                END IF
C
C TRANSFORMATION INTO TRUE SYSTEM OF EPOCH
C ----------------------------------------
                IF (IRCEPO2==0) THEN
                  IF (IRCVEL2.NE.0) THEN
                    WGSAPR(1:3,IST)=xkin(1:3)
                    CALL TRUEARTH(XKIN,WGSEPO(1:3,IST),1,0,
     1                            SZ,XPOL,YPOL)
                    WGSEPO(4:9,IST)=0.D0
                  ELSE
                    XKV(1:3)=XKIN(1:3)
                    XKV(4:6)=XVEL(1:3)
                    CALL TRUEARTH(XKV,WGSEPO(1:6,IST),1,1,
     1                            SZ,XPOL,YPOL)
                    WGSEPO(7:9,IST)=0.D0
                    GOTO 24
                  END IF
                ELSE
                  IF (IRCVEL2.EQ.0 .AND. IRCSTD.EQ.0 ) THEN
                    CALL GTLEOCO(STNAME(STFIL(IST,IF)),
     1                          TOBS+DTFIL(IST,IF)/86400.D0,2,0,
     1                          XKV,SZL,XPOLL,YPOLL,IRCODE,
     1                          cmcyn_leo)
                    IF (IRCODE.EQ.0) THEN
                      XKV(4:6)=XVEL(1:3)
                      CALL TRUEARTH(XKV,WGSEPO(1:6,IST),1,1,
     1                              SZL,XPOLL,YPOLL)
                      WGSEPO(7:9,IST)=0.D0
                    ELSE
                    END IF
                  END IF
                END IF
C
C 3.2) GET LEO A PRIORI COORDINATES FROM THE ORBIT
C ------------------------------------------------
                IF (IRCSTD==0) THEN
                  CALL GTLEOCO(STNAME(STFIL(IST,IF)),
     1                         TOBS+DTFIL(IST,IF)/86400.D0,1,2,
     1                         XTMP(1:9),SZL,XPOLL,YPOLL,IRCODE,
     1                         cmcyn_leo)
                  IF (IRCODE==0) THEN
                    IF (IRCEPO2.NE.0) WGSEPO(1:3,IST)=XTMP(1:3)
                    IF (IRCVEL2.NE.0) THEN
                      WGSEPO(4:6,IST)=XTMP(4:6)
                      WGSSAT(4:6,IST)=XTMP(4:6)
                    END IF
                    GOTO 24
                  END IF
                END IF
C
C 3.3) USE EXISTING STATION COORDINATES
C --------------------------------------
                WGSEPO(1:3,IST)=WGSSES(1:3,IST,IFRQ,IFLSES)
                IF ((IRCEPO2.NE.0 .AND. IRCODE.NE.0) .OR.
     1            (IRCVEL2.NE.0 .AND. IRCODE.NE.0)) THEN
                  WRITE(LFNERR,23)TOBS
23                FORMAT(/,' *** SR PRCEPO: ',
     1            'NO LEO A PRIORI POSITION/VELOCITY FOR THE EPOCH: ',
     2            F15.6/)
                END IF
24            END IF
C
C WRITE A PRIORI COORDINATES TO THE FILE "KINSCR"
C -----------------------------------------------
34            IF (IRCKIN==0) THEN
                IF (IKINSC==0) THEN
                  IKINSC=1
                  CALL GTFLNA(1,'KINSCR ',FILSCR,IRCSCR)
                  CALL OPNFIL(LFNKSC,FILSCR,'UNKNOWN','FORMATTED',
     1                        ' ',' ',IOSTAT)
                  CALL OPNERR(LFNERR,LFNKSC,IOSTAT,FILSCR,'PRCEPO')
                  CALL JMT(TOBS,IYEAR,IMONTH,DAY)
                  IDAY=IDINT(DAY)
                  CALL RADGMS(3,DAY,VORZ,IHOUR,IMIN,SEC)
                  ISEC=IDNINT(SEC)
                  WRITE(TSTRNG2(1:19),12) IYEAR,IMONTH,IDAY,
     1                  IHOUR,IMIN,ISEC
12                  FORMAT(I4,2('-',I2.2),I3,2(':',I2.2))
                  TSTRNG2(1:19)=' '
                  TITLE(1:80)=' '
                  DATUM(1:16)=' '
                  WRITE(LFNKSC,13) TITLE,DATE,TIME,DATUM,TSTRNG2
13                  FORMAT(A64,1X,A9,1X,A5,/,80('-'),/,
     1              'LOCAL GEODETIC DATUM: ',A16,2X,'EPOCH: ',A19,//,
     2              ' STATION NAME     WEEK  SECONDS',8X,
     3              'X (M)',10X,'Y (M)',10X,'Z (M)',4X,'F',/)
                END IF
                IF (KINWRI(STFIL(IST,IF)).EQ.1.AND.ISTKIN.EQ.1) THEN
                  KINWRI(STFIL(IST,IF))=0
                  CALL MJDGPS(TOBS,SECOND,NWEEK)
                  SECOND=DNINT(SECOND)
                  WRITE(LFNKSC,365) STNAME(STFIL(IST,IF)),NWEEK,
     1                  SECOND,WGSAPR(1:3,IST),'K'
365               FORMAT(1X,A16,1X,I4,1X,F8.0,1X,3F15.4,1X,A1)
                END IF
              END IF
25          CONTINUE
C
C SET FLAG FOR INITIALIZATION OF TEC OBSERVATIONS
            IF (MELWUB.EQ.2) THEN
              IDTEC=0
              IF (SESSID.NE.SESOLD(2)) THEN
                IDTEC=1
                SESOLD(2)=SESSID
                NTEC(1)=0
                NTEC(2)=0
              ELSEIF (NTEC(2).NE.0) THEN
                TECEPO(1)=TECEPO(2)
                DO ITEC=1,NTEC(2)
                  TECOBS(1,ITEC)=TECOBS(2,ITEC)
                  TECINF(1,1,ITEC)=TECINF(2,1,ITEC)
                  TECINF(1,2,ITEC)=TECINF(2,2,ITEC)
                ENDDO
                NTEC(1)=NTEC(2)
                NTEC(2)=0
              ENDIF
            ENDIF
C
C LOOP OVER ALL SATELLITES OF EPOCH
C ---------------------------------
            IAMSAV=0
            DO 300 ISAT=1,NSFLEP(IFIL)
C
C SATELLITE INDEX IN SESSION ARRAY
              DO 20 KSAT=1,NSASES
                IF(SATSES(KSAT).EQ.SAFLEP(ISAT,IFIL)) GOTO 30
20            CONTINUE
30            ISASES=KSAT
C
C SATELLITE INDEX IN EPOCH ARRAY
              DO 41 KSAT=1,NSAEPO
                IF(SATEPO(KSAT).EQ.SAFLEP(ISAT,IFIL)) GOTO 42
41            CONTINUE
C
C SATELLITE NOT IN STANDARD ORBIT
              SAFLEP(ISAT,IFIL)=0
              GOTO 300
C
42            ISAEPO=KSAT
C
C SATELLITE INDEX IN FILE ARRAY
              DO 35 KSAT=1,NSATEL(IF)
                IF(NUMSAT(KSAT,IF).EQ.SAFLEP(ISAT,IFIL)) GOTO 45
35            CONTINUE
45            ISAFIL=KSAT
C
C APPLY SATELLITE ANTENNA OFFSET
              OFFTYP=typeMWTR
              IF (MEATYP(IF).EQ.3) OFFTYP=typeSLR
              CALL SAT_OFF(SAFLEP(ISAT,IFIL),TOBS,OFFTYP,IFRSES,OFFSET)
              CALL GNSSATTI(IORSYS,SAFLEP(ISAT,IFIL),TOBS,1,
     1                      XVA(:,ISAEPO),EX_SAT=EX,EY_SAT=EY,EZ_SAT=EZ)
              XVAEP = XVA(:,ISAEPO)
              DO K = 1,3
                XVAEP(K)=XVAEP(K)+EX(K)*OFFSET(1)
     1                           +EY(K)*OFFSET(2)
     2                           +EZ(K)*OFFSET(3)
              ENDDO
C
C APPLY ANTENNA OFFSETS AND COMPUTE PSEUDORANGES FOR BOTH STATIONS OF THE FILE
C ----------------------------------------------------------------------------
              DO 60 IST=1,NDIFF(IF)+1
C FOR AIR AND SHIP
                IF (MARTYP.EQ.MTypeAIR .OR. MARTYP.EQ.MTypeSHIP .AND.
     1                      IRCEPO3 .EQ. 0) THEN
                  CALL STA_OFF(ANTTYP(IST,IF),IANTEN(IST,IF),
     1                        STNAME(STFIL(IST,IF)),SAFLEP(ISAT,IFIL),
     2                        ICARR(IFRQ,IF),CSESS(IST,IF),ANTECC)
                  ANTECC(1:3)=POSECC(1:3,IST,IF)+ANTECC(1:3)
                  CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,
     1                        WGSEPO(1:3,IST),XELEPO)
                  CALL ELLECC(XELEPO,ANTECC,EXC)
                  WGSSAT(1:3,IST)=WGSEPO(1:3,IST)+EXC(1:3)
C FOR LEO
                ELSEIF (MARTYP.EQ.MTypeSPACE) THEN
                  CALL STA_OFF(ANTTYP(IST,IF),IANTEN(IST,IF),
     1                        STNAME(STFIL(IST,IF)),SAFLEP(ISAT,IFIL),
     2                        ICARR(IFRQ,IF),CSESS(IST,IF),ANTECC)
                  CALL LEOANTE(STNAME(STFIL(IST,IF)),
     1                         TOBS+DTFIL(IST,IF)/86400.D0,ANTECC,
     1                         0,WGSEPO(:,IST),TRUOFF)
                  WGSSAT2(1:3)=WGSEPO(1:3,IST)+TRUOFF(1:3)
                  WGSSAT2(4:6)=WGSEPO(4:6,IST)
c                  CALL LEOANTE(STNAME(STFIL(IST,IF)),
c     1                        TOBS+DTFIL(IST,IF)/86400.D0,ANTECC,
c     1                        0,WGSEPO(:,IST),TRUOFF)
                  XKIN2(1:3)=WGSEPO(1:3,IST)
C               TRANSFORMATION INTO EARTH FIXED SYSTEM
                  CALL TRUEARTH(WGSSAT2(1:3),WGSSATHLP(1:3),0,0,
     1                        SZL,XPOLL,YPOLL)
                  CALL TRUEARTH(XKIN2(1:3),XKINHLP(1:3),0,0,
     1                        SZL,XPOLL,YPOLL)
                  CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,
     1                        WGSSATHLP(1:3),XELEPO)
                  ELLSES(1:3,IST,IFRQ,IF)=XELEPO(1:3)
C               APPLY CMC CORRECTION
                  IF (IRCEPO2==0)cmcyn_leo=.FALSE.
                  CALL CMC_LEO(XKINHLP(1:3),WGSAPR(1:3,IST),
     1                         WGSSATHLP(1:3),WGSSAT(1:3,IST),
     1                         TOBS+DTFIL(IST,IF)/86400.D0,
     1                         cmcyn,cmcyn_leo)

C FOR ALL OTHERS
                ELSE
                  CALL STA_OFF(ANTTYP(IST,IF),IANTEN(IST,IF),
     1                        STNAME(STFIL(IST,IF)),SAFLEP(ISAT,IFIL),
     2                        ICARR(IFRQ,IF),CSESS(1,IF),ANTECC)
                  CALL ELLECC(XSTELL(1,STFIL(IST,IF)),ANTECC,EXC)
                  DO K=1,3
                    WGSSAT(K,IST) = WGSEPO(K,IST)+EXC(K)
                  ENDDO
                ENDIF
C
                CALL STAFLG(STNAME(STFIL(IST,IF)),TOBS,IFLAG,MARTYP)
                CALL PRANGE(IST,TOBS,DTFIL(IST,IFLSES),SECIPL,
     1                      STNAME(STFIL(IST,IF)),WGSSAT(1,IST),
     2                      SAFLEP(ISAT,IFIL),XVAEP,SZ,
     3                      XPOL,YPOL,ISATCO,ITROPO,
     4                      IEXTRA,ELLSES(1,IST,IFRQ,IFLSES),
     5                      MEATYP(IF),IFRSES,IORSYS,TOPPOS(1,IST),
     6                      ZMAX,NADMAX,DIST(IST),SATCLK ,ZEN(IST),
     7                      AZI(IST),ZENION(IST),NDIFF(IF),NOINCLK(2),
     8                      IRCCLK,IRCPCV,NAD(IST),AZISAT(IST),
     9                      AZISOK(IST),ANTTYP(IST,IF),IANTEN(IST,IF),
     1                      CSESS(IST,IF),IREL2)
C
C STORE STATION CLOCK CORRECTION AS A PRIORI (IF NECESSARY)
C ---------------------------------------------------------
                DTSEC=(TOBS-CLKHED%TFIRST)*86400d0
                JEPO=NINT(DTSEC/CLKREC%EPOCH(1)*CLKREC%NEPO)+1
C
                IF (opteli(23) == 0 .OR. opteli(23) == 2) THEN
                  CLKREC%CLOCK(STFIL(IST,IF),JEPO)=-DTFIL(IST,IFLSES)
                ELSE IF (OPTELI(23).EQ.3.AND.NSAMPL(3).NE.0) THEN
                  CALL MJDGPS(TOBS,GPSSEC,NWEEK)
                  TFRAC=DTSEC-DNINT(DTSEC/NSAMPL(3))*NSAMPL(3)
                  NUMEPO=NINT(clkrec%epoch(1)/DBLE(nSampl(3)))+1
                  JEPO=NINT(DTSEC/CLKREC%EPOCH(1)*(NUMEPO-1))+1
                  IF(DABS(TFRAC).LE.DTSIM*86400.D0) THEN
                    CLKREC%CLOCK(STFIL(IST,IF),JEPO)=-DTFIL(IST,IFLSES)
                  ENDIF
                ENDIF
                recClk(ist)=dtFil(IST,IFLSES)
C
C COMPUTE ZENITH DISTANCE AND AZIMUTH OF A SATELLITE AS SEEN FROM SENSOR SYSTEM
C -----------------------------------------------------------------------------
                IF (MARTYP.EQ.MTypeSPACE) THEN
                  CALL LEOSKY(STNAME(STFIL(IST,IF)),TOBS,0,0,
     1                        WGSSAT(:,IST),XVAEP,SZ,
     2                        DTFIL(IST,IFLSES),XPOL,YPOL,MEATYP(IF),
     3                        TOPPOS2(1,IST),DUMMY,ZEN2(IST),AZI2(IST),
     4                        NAD2(IST),AZISA2(IST),SAFLEP(ISAT,IFIL),
     5                        IORSYS,AZISO2(IST))
                ELSE
                  TOPPOS2(:,IST)=TOPPOS(:,IST)
                  ZEN2(IST)=ZEN(IST)
                  AZI2(IST)=AZI(IST)
                  NAD2(IST)=NAD(IST)
                  AZISA2(IST)=AZISAT(IST)
                  AZISO2(IST)=AZISOK(IST)
                END IF
C
C CHECK WHETHER SATELLITE CLOCK AVAILABLE
                IF(IRCCLK.NE.0 .AND.
     1             ( NOINCLK(2).EQ.1 .OR.
     2               NOINCLK(2).EQ.2 .OR.
     3              (NOINCLK(2).LE.0 .AND. NOINCLK(3) .GE. 1 .AND.
     4                                     NOINCLK(3) .LE. 2 ))) THEN
                  SAFLEP(ISAT,IFIL)=0
                  GOTO 300
                ENDIF
C CHECK IF ZENITH DISTANCE OKAY (FROM IRCPCV, SEE SR PRANGE)
                IF(IRCPCV.NE.0) THEN
                  SAFLEP(ISAT,IFIL)=0
                  GOTO 300
                ENDIF
C
C CHECK SATELLITE ZENITH DISTANCE AND NADIR ANGLE
                IF (MARTYP.EQ.' ') THEN
                  IF (NAD2(IST).GT.NAMAX) THEN
                    NAMAX=(NAD2(IST))
                  ENDIF
                  IF (ZEN2(IST).GT.RAPZENMAX .AND.
     1                RAPZENMAX.GT.0D0) THEN
                    SAFLEP(ISAT,IFIL)=0
                    GOTO 300
                  ENDIF
                END IF
C
C PROCESS NIGHT-TIME DATA ONLY, IF REQUESTED
                IF (OPTDCB(4).EQ.1) THEN
                  CALL CHKNTD(TOBS,WGSSAT(1,IST),IORSYS,IRCNTD)
C
                  IF (IRCNTD.EQ.0) THEN
                    SAFLEP(ISAT,IFIL)=0
                    GOTO 300
                  ENDIF
                ENDIF
C
C POLARIZATION EFFECT OF DIPOLE ANTENNAS (RECEIVER AND SATELLITE)
C ---------------------------------------------------------------
                IF (MARTYP.NE.MTypeSPACE) THEN
                  CALL POLARI(TOBS,IPOLAR,TOPPOS2(1,IST),
     1                        XVAEP,DPOLAR(IST))
                ELSE
                  CALL POLARLEO(TOBS,IPOLAR,STNAME(STFIL(IST,IF)),
     1                          WGSSAT2,XVAEP,DPOLAR(IST))
                END IF
C
C DIFFERENTIAL CODE BIAS CORRECTION
                IF (.NOT.OPTDCB(2).EQ.-2) THEN
                  CALL DCBCOR(MEATYP(IF),IFRSES,SAFLEP(ISAT,IFIL),
     1              STNAME(STFIL(IST,IF)),RECTYP(IST,IF),0,0,TOBS,
     2              DIST(IST))
                ENDIF
C
                CALL SVN2CHR(SAFLEP(ISAT,IFIL),ISVMOD,SVNCHR)
                IF (SVNCHR.EQ.'R') THEN
                  DIST(IST) = DIST(IST) - C/1D9 *
     1            getIsb(0,STNAME(STFIL(IST,IF)),TOBS,ICARR(1,IF),IRC)
                ENDIF
C
60            CONTINUE
C
C SAVE SATELLITE ELEVATIONS FOR ELEVATION-DEPENDENT WEIGHTING
C AND MINIMAL AND MAXIMUM ELEVATION AND NADIR ANGLE
              DO 65 IST=1,NDIFF(IF)+1
                ELEV=(PI/2.D0-ZEN2(IST))*180.0D0/PI
                ELEVA(IST,ISASES)=ELEV
                AZIMU(IST,ISASES)=AZI2(IST)*180.0D0/PI
                IF (ELEV.LT.ELEVMM(1,IF)) ELEVMM(1,IF)=ELEV
                IF (ELEV.GT.ELEVMM(2,IF)) ELEVMM(2,IF)=ELEV
                NADI=NAD2(IST)*180.0D0/PI
                IF (NADI.LT.NADIMM(1,IF)) NADIMM(1,IF)=NADI
                IF (NADI.GT.NADIMM(2,IF)) NADIMM(2,IF)=NADI
C
C HISTOGRAM OF OBSERVATIONS (ELEVATION)
                IBIN=IDINT(ELEV/5.D0)+1
                IF (IBIN.LT. 1) IBIN= 1
                IF (IBIN.GT.18) IBIN=18
                NOBELV(IST,IBIN,IF)=NOBELV(IST,IBIN,IF)+1
C
C HISTOGRAM OF OBSERVATIONS (NADIR ANGLE)
                IBINNAD=IDINT(NAD2(IST)/PI*180.D0/0.5D0)+1
                IF (IBINNAD.LT. 1) IBINNAD= 1
                IF (IBINNAD.GT.30) IBINNAD=30
                NOBNAD(IST,IBINNAD,IF)=NOBNAD(IST,IBINNAD,IF)+1
C
C HISTOGRAM OF OBSERVATIONS (AZIMUTH ANGLE AT THE SATELLITE)
                IBINAZIS=IDINT(AZISO2(IST)/PI*180.D0/10.D0)+1
                IF (IBINAZIS.LT.1)  IBINAZIS= 1
                IF (IBINAZIS.GT.36) IBINAZIS=36
                NOBAZIS(IST,IBINAZIS,ISAFIL,IF)=
     1                        NOBAZIS(IST,IBINAZIS,ISAFIL,IF)+1
C
C HANDLE ELEVATION AND NADIR ANGLE STATISTICS FOR MELBOURNE-WUEBBENA CASE
                IF (MELWUB.EQ.1) THEN
                  IF2=FILNUM(2)
                  ELEVMM(1,IF2)=ELEVMM(1,IF)
                  ELEVMM(2,IF2)=ELEVMM(2,IF)
                  NOBELV(IST,IBIN,IF2)=NOBELV(IST,IBIN,IF)
                  NADIMM(1,IF2)=NADIMM(1,IF)
                  NADIMM(2,IF2)=NADIMM(2,IF)
                  NOBNAD(IST,IBINNAD,IF2)=NOBNAD(IST,IBINNAD,IF)
                  NOBAZIS(IST,IBINAZIS,ISAFIL,IF2)=
     1                           NOBAZIS(IST,IBINAZIS,ISAFIL,IF)
                ENDIF
65            CONTINUE
C
C SET UP THE AMBIGUITY PARAMETERS IF THEY SHOULD BE PRE-ELIMINATED
C ----------------------------------------------------------------
              IF (MEATYP(IF).EQ. 1 .AND.
     1            STRAMB(1) .EQ.-1 .AND.
     2            IEPPAR    .EQ. 0 .AND.
     3            MELWUB    .NE. 2) THEN
                CALL SEQAMB(IF    ,IFRSES,OBSNUM(IFLSES)      ,
     1                      SATSES(ISASES)      ,NUMAMB,AMBSAT,AMBIEP,
     2                      AMBCLS,NPARN ,NPAR  ,NPASES,LOCQ  ,ANOR  ,
     3                      BNOR  )
              ENDIF
C
C FIND AMBIGUITY INDEX, FOR PHASE OBSERVATIONS ONLY
              IF (MEATYP(IF).EQ. 1) THEN
                DO 80 IAMB=NUMAMB(IF),1,-1
                  IF(AMBSAT(IAMB,IF).EQ.SATSES(ISASES) .AND.
     1               AMBIEP(IAMB,IF).LE.OBSNUM(IFLSES)) GOTO 81
80              CONTINUE
                IAMB=-1
              ELSE
                IAMB=-1
              ENDIF
81            CONTINUE
C
C INITIALIZE AMBIGUITY IF PHASE DATA PROCESSED
C --------------------------------------------
              IF ((MEATYP(IF).EQ.1).AND.(IAMB.NE.-1)) THEN
                IF(IAMB1(IAMB,IFRQ,IFLSES).EQ.'U')THEN
                  IF(IFRSES.NE.4 .AND. MELWUB.EQ.0) THEN
C                    PRDIF=DIST(1)-DIST(2)+C*SYNCM(IFLSES)
                    PRDIF=DIST(1)-DIST(2)+DBLE(ISYNCR)*C*SYNCM(IFLSES)
                  ELSE IF (IFRSES.EQ.4) THEN
C                    PRDIF=C*SYNCM(IFLSES)
                    PRDIF=DBLE(ISYNCR)*C*SYNCM(IFLSES)
                  ELSE
                    PRDIF=0.D0
                  ENDIF
                  CALL AMBSET(AMBDEF(IF),IFRSES,
     1                     IAMB,NUMAMB(IF),AMBCLS(1,1,IF),
     2                     OBFLEP(ISAT,IFIL),PRDIF,AMBSAT(1,IF),
     3                     AMBWLF(1,1,IF),NFRFIL(IF),NFREQ(IF),IFRQ,
     4                     IAMB1(1,1,IFLSES),AMB0(1,1,IFLSES),
     5                     AMBIGU(1,1,IF))
C
C SAVE INDEX OF AMBIGUITY FLAG "IAMB1" (IF ONLY ONE OBSERVATION)
                  IAMSAV=IAMB
                ENDIF
              ENDIF
C
C APPLY POLARIZATION EFFECT AND SAVE POLARIZATION CORRECTION
C ----------------------------------------------------------
              IF (NDIFF(IF).EQ.1 .AND. MEATYP(IF).EQ.1) THEN
                NPOLAR=DNINT(POLARS(IAMB,IFRQ,IFLSES)-
     1                      (DPOLAR(1)-DPOLAR(2)))
                POLARS(IAMB,IFRQ,IFLSES)=NPOLAR+
     1                                   DPOLAR(1)-DPOLAR(2)
                PLREFF=FACLIN(IFRSES,1,SAFLEP(ISAT,IFIL))*
     1             POLARS(IAMB,IFRQ,IFLSES)*WLGT(1,SAFLEP(ISAT,IFIL))+
     2             FACLIN(IFRSES,2,SAFLEP(ISAT,IFIL))*
     3             POLARS(IAMB,IFRQ,IFLSES)*WLGT(2,SAFLEP(ISAT,IFIL))
C
C POLARIZATION NOT APPLIED FOR ZERO-DIFFERENCES
              ELSE IF (NDIFF(IF).EQ.0 .AND. MEATYP(IF).EQ.1) THEN
                NPOLAR=DNINT(POLARS(IAMB,IFRQ,IFLSES)-DPOLAR(1))
                POLARS(IAMB,IFRQ,IFLSES)=NPOLAR+DPOLAR(1)
C
                PLREFF=FACLIN(IFRSES,1,SAFLEP(ISAT,IFIL))*
     1               POLARS(IAMB,IFRQ,IFLSES)*WLGT(1,SAFLEP(ISAT,IFIL))+
     2               FACLIN(IFRSES,2,SAFLEP(ISAT,IFIL))*
     3               POLARS(IAMB,IFRQ,IFLSES)*WLGT(2,SAFLEP(ISAT,IFIL))
              ELSE
                PLREFF=0.D0
              ENDIF
C
C CHECK O-C FOR ZERO DIFFERENCE CODE OBSERVATIONS
C -----------------------------------------------
              IF (NDIFF(IF).EQ.0 .AND.
     1            MEATYP(IF).NE.1 .AND.
     2            EDTLVL.GT.0.D0) THEN
                TEST=OBFLEP(ISAT,IFIL)-DIST(1)
                IF (MARTYP.EQ.MTypeSPACE) THEN
                  CALL WGTELV(ICOELV(2,MEATYP(IF)),ZEN2(1),EDTNOR)
                  EDTLV0=EDTLVL*DSQRT(EDTNOR)
                ELSE
                  CALL WGTELV(ICOELV(1,MEATYP(IF)),ZEN2(1),EDTNOR)
                  EDTLV0=EDTLVL*DSQRT(EDTNOR)
                END IF
C STATION WEIGHTING
                CALL WGTSTA(TOBS,DTSIM,MEATYP(IF),STNAME(STFIL(1,IF)),
     1                      OBSWGT)
                IF (OBSWGT.GT.1D0) EDTLV0=EDTLV0*OBSWGT
                IF (DABS(TEST).GT.EDTLV0) THEN
                  WRITE(LFNPRT,9999)TEST,EDTLV0,
     1                              STFIL(1,IF),STNAME(STFIL(1,IF)),
     2                              ISASES,SATSES(ISASES),ELEV,
     3                              OBSNUM(IFLSES)
9999              FORMAT(/,' ### SR PRCEPO: EDTLVL EXCEEDED',/
     1                             16X,'DIFFERENCE     :',F15.2,/,
     1                             16X,'O-C EDIT LEVEL :',F15.2,/,
     2                             16X,'STATION        :',I5,3X,A16,/,
     3                             16X,'SATELLITE (ELE):',I5,I5,F5.0,/,
     4                             16X,'EPOCH NUMBER   :',I5,/)
                  SAFLEP(ISAT,IFIL)=0
                  GOTO 300
                ENDIF
              ENDIF
C
C INCREASE NUMBER OF EQUATIONS
C ----------------------------
              ISING=ISING+1
C
C HANDLE DTEC OBSERVATIONS
C ------------------------
              IF (MELWUB.EQ.2) THEN
C
C SAVE AMBIGUITY CLUSTER ASSOCIATED TO CURRENT TEC OBSERVATION
                ICLS=AMBCLS(IAMB,I,IF)
C
                IF (IDTEC.EQ.1) THEN
C
C INITIALIZE MEMORY CONCERNING TEC OBSERVATIONS
                  NTEC(1)=NTEC(1)+1
                  TECEPO(1)=TOBS
                  TECOBS(1,NTEC(1))=OBFLEP(ISAT,IFIL)-DIST(1)-
     1              AMB0(IAMB,IFRQ,IFLSES)-PLREFF
                  IF (NDIFF(IF).EQ.1) THEN
                    TECOBS(1,NTEC(1))=TECOBS(1,NTEC(1))+DIST(2)
                  ENDIF
                  TECINF(1,1,NTEC(1))=SATSES(ISASES)
                  TECINF(1,2,NTEC(1))=ICLS
C
                  SAFLEP(ISAT,IFIL)=0
                  ISING=ISING-1
                  GOTO 300
                ELSE
C
C FORM DTEC OBSERVATIONS
                  NTEC(2)=NTEC(2)+1
                  TECEPO(2)=TOBS
                  TECOBS(2,NTEC(2))=OBFLEP(ISAT,IFIL)-DIST(1)-
     1              AMB0(IAMB,IFRQ,IFLSES)-PLREFF
                  IF (NDIFF(IF).EQ.1) THEN
                    TECOBS(2,NTEC(2))=TECOBS(2,NTEC(2))+DIST(2)
                  ENDIF
                  TECINF(2,1,NTEC(2))=SATSES(ISASES)
                  TECINF(2,2,NTEC(2))=ICLS
C
                  ITEC0=0
                  DO ITEC=1,NTEC(1)
                    IF (TECINF(1,1,ITEC).EQ.TECINF(2,1,NTEC(2)) .AND.
     1                  TECINF(1,2,ITEC).EQ.TECINF(2,2,NTEC(2)))
     2                ITEC0=ITEC
                  ENDDO
                  IF (ITEC0.EQ.0) THEN
                    SAFLEP(ISAT,IFIL)=0
                    ISING=ISING-1
                    GOTO 300
                  ELSE
                    XDTEC=TECOBS(2,NTEC(2))-TECOBS(1,ITEC0)
C
C CHECK TIME INTERVAL
                    ISAMPL=IDNINT(86400.D0*(TECEPO(2)-TECEPO(1)))
                    IF (ISAMPL.GT.NSAMPL(1) .AND. NTEC(2).EQ.1) THEN
                      WRITE(LFNERR,88) ISAMPL,IF,OBSNUM(IFLSES)
88                    FORMAT(/,' ### SR PRCEPO: DATA GAP DETECTED',
     1                  /,16X,'TIME INTERVAL (SEC) : ',I5,
     2                  /,16X,'FILE NUMBER         : ',I5,
     3                  /,16X,'EPOCH NUMBER        : ',I5,/)
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C CHECK MAXIMUM NUMBER OF EQUATIONS
              NDBTOT=NDBLE+ISING
C
              IF(NDBTOT.GT.MAXEQN) THEN
                WRITE(LFNERR,91) NDBTOT,MAXEQN,IF,OBSNUM(IFLSES)
91              FORMAT(/,' *** SR PRCEPO: TOO MANY EQUATIONS/EPOCH',/,
     1                             16X,'NUMBER OF EQUATIONS     >',I5,/,
     2                             16X,'MAX. NUMBER OF EQUATIONS:',I5,/,
     3                             16X,'FILE NUMBER             :',I5,/,
     4                             16X,'EPOCH NUMBER            :',I5,/,
     5                             16X,'SET MAXFLS TO #FILE/SESSION * ',
     6                                 '#FREQUENCIES PROCESSED/FILE',/)
                CALL EXITRC(2)
              ENDIF
C
              SATSAV(ISING)=ISASES
C
C FORM SINGLE DIFFERENCE OBSERVATION EQUATION
C -------------------------------------------
              TSEC=(TOBS-TOSC)*86400.D0
              CALL SNDIFF(NPAR,NPARN,MAXSNG,MAXEQN,NDBTOT,
     1                    STFIL(1,IF),ICENTR,SAFLEP(ISAT,IFIL),
     2                    XVAEP,XSTAT,WGSSAT,TOPPOS,SZ,
     3                    XPOL,YPOL,TPOL,CLFRTO,TOBS,
     4                    OBSNUM(IFLSES),INDARC,TBOUND,TSEC,
     5                    ELE(1,ISAEPO),IF,LOCQ,PARTYP,AOBS,INDA,
     6                    AHELP,NOBSPA,ZEN,AZI,
     7                    AMBWLF(1,1,IF),NFRFIL(IF),IFRSES,NFREQ(IF),
     8                    NUMAMB(IF),AMBIEP(1,IF),AMBCLS(1,1,IF),
     9                    AMBSAT(1,IF),MEATYP(IF),MELWUB,TRPLIM,TRPLMS,
     1                    ITRMAP,ITRGRD,ELLSES(1,1,IFRQ,IFLSES),IORSYS,
     2                    TOSC,TIMSTC,SCASTC,SCAHIL,SCAPOT,NSAOFF,
     3                    SATOFF,TIMOFF,SCAALB,SCACEN,NSAALB,SATALB,
     4                    RECTYP(1,IF),ANTTYP(1,IF),IANTEN(1,IF),
     5                    CSESS(1,IF),ANTCAL,NUMCAL,PRNCAL,OPTDIP,
     6                    OPTGIM,POLGIM,EPOGIM,SCAGIM,ANTRAO,NUMRAO,
     7                    PRNRAO,IP,CLKHED,CLKREC,NDIFF(IF),LEOARC,
     8                    TBOUND2,STNAME,TIMSTC2,NSASPV,SATSPV,NAD,
     9                    NADMAX,RAPZENMAX,IPHSEP,ZEN2,AZI2,
     1                    AZISOK,NRGB,OPLOAD,TIMISB,DTSIM,ZMAX)
              IPAMB(ISING)=IP

! SAVE A PRIORI VALUES FOR EPOCH PARAMETERS
! -----------------------------------------
              staNam(1)=stname(stfil(1,if))
              staNam(2)=stname(stfil(nDiff(if)+1,if))
              CALL aprval(TOBS,MXCLCQ,NPAR,LOCQ,NDBTOT,INDA,
     1                    MAXEQN,NDIFF(IF),IFRSES,NKIN,STKIN,NSTAT,
     2                    STNAME,STANAM,SAFLEP(ISAT,IFIL),WGSAPR,
     3                    recClk,SATCLK,CLKHED,XXX0,XXREF)

C
C OBSERVED MINUS COMPUTED
C -----------------------
C
C SET "DIST" TO ZERO FOR MELBOURNE-WUEBBENA LC
              IF (MELWUB.EQ.1) THEN
                DO IST=1,NDIFF(IF)+1
                  DIST(IST)=0.D0
                  CALL DCBCOR(2,6,SAFLEP(ISAT,IFIL),
     1              STNAME(STFIL(IST,IF)),RECTYP(IST,IF),0,2,TOBS,
     2              DIST(IST))
                ENDDO
              ENDIF
C
C SET O-C TERM FOR DTEC OBSERVATION
              IF (MELWUB.EQ.2) THEN
                BOBS(NDBTOT)=XDTEC
                GOTO 300
              ENDIF
C
C COMPUTE O-C TERM FOR ZERO-DIFFERENCE CODE OBSERVATION
              BOBS(NDBTOT)=OBFLEP(ISAT,IFIL)-DIST(1)-PLREFF
              DERCLK(ISING)=0.D0
C
C SUBTRACT A PRIORI AMBIGUITY VALUE FOR PHASE OBSERVATION
              IF (MEATYP(IF).EQ.1) THEN
                BOBS(NDBTOT)=BOBS(NDBTOT)-AMB0(IAMB,IFRQ,IFLSES)
              ENDIF
C
C ADD RANGE OF SECOND STATION FOR SINGLE-DIFFERENCE OBSERVATION
              IF (NDIFF(IF).EQ.1) THEN
                BOBS(NDBTOT)=BOBS(NDBTOT)+DIST(2)
              ENDIF
C
C COMPUTE PARTIALS W.R.T. RECEIVER CLOCK FOR SYNCHONIZATION ERRORS
              IF (IFRSES.NE.4.AND.MELWUB.EQ.0.AND.NDIFF(IF).EQ.1) THEN
                DD=DSQRT(TOPPOS(1,2)**2+TOPPOS(2,2)**2+
     1                   TOPPOS(3,2)**2)
                CALL PDCLK(1,TOPPOS(1,2),DD,XVAEP,
     1                     WGSSAT(1,2),DUMMY,SZ,DERHLP)
                DERCLK(ISING)=DERHLP(1)
              ENDIF
C
C CHECK WHICH CLOCKS ARE REALLY OBSERVED
              IF (NDIFF(IF).EQ.0.AND.CLKSYS.EQ.1) THEN
                IF (SAFLEP(ISAT,IFIL).LT.100) THEN
                  II = 1
                ELSE
                  II = 2
                ENDIF
C
                ICLK = CLKIDX(CLKHED,CLKNAM=STNAME(STFIL(1,IF)))
                CLKPAR(II,ICLK)=CLKPAR(II,ICLK)+1
C
                ICLK = CLKIDX(CLKHED,SATNUM=SAFLEP(ISAT,IFIL))
                CLKPAR(II,ICLK)=CLKPAR(II,ICLK)+1
              ENDIF
C
C UPDATE STATISTICS ON EPOCHS CONNECTED BY CONT. PHASE OBSERVATIONS
C -----------------------------------------------------------------
              IF (MEATYP(IF).EQ.1.AND.IAMB.NE.-1) THEN
C
C START OF THE AMBIGUITY BEFORE THE PREV. OBSERVATION
                IF (AMBIEP(IAMB,IF).LE.OBSEPO(ISASES,IFREQ,IFLSES)) THEN
                  II = 0
                  DO WHILE (II <= MAXSYS)
C
C PHASE OBSERV. ARE CONNECTED, UPDATE TIME WINDOW LIST
                    TOB0=TIMREF(IF)+(OBSEPO(ISASES,IFREQ,IFLSES)-1)
     1                     *IDELTT(IF)/86400D0
                    IP1=0
                    IP2=0
                    DO IP=1,TIMFIL(ii,IF)%NINTER
                      IF(TIMFIL(ii,IF)%AMBINT(IP)%INT%T(1)-DTSIM
     1                                                    .LE.TOB0 .AND.
     2                   TIMFIL(ii,IF)%AMBINT(IP)%INT%T(2)+DTSIM
     3                                                    .GE.TOB0) THEN
                        IP1=IP
                      ENDIF
                      IF(TIMFIL(ii,IF)%AMBINT(IP)%INT%T(1)-DTSIM
     1                                                    .LE.TOBS) THEN
                        IP2=IP
                      ENDIF
                    ENDDO
C
C CONNECT TWO INTERVALS:
                    IF (IP1.NE.IP2.AND.IP1.NE.0.AND.IP2.NE.0) THEN
                      TIMFIL(ii,IF)%AMBINT(IP1)%INT%T(2)=
     1                                TIMFIL(ii,IF)%AMBINT(IP2)%INT%T(2)
C
                      IF (TIMFIL(ii,IF)%AMBINT(IP2)%REF.NE.0) THEN
                        IF (TIMFIL(ii,IF)%AMBINT(IP1)%REF.EQ.0) THEN
                          TIMFIL(ii,IF)%AMBINT(IP1)%REF=
     1                           TIMFIL(ii,IF)%AMBINT(IP2)%REF
                        ELSEIF (TIMFIL(ii,IF)%AMBINT(IP1)%REF.EQ.2 .OR.
     1                          TIMFIL(ii,IF)%AMBINT(IP2)%REF.EQ.2) THEN
                          WRITE(LFNERR,'(/,A,3(/,16X,A,I6),/,A,/)')
     1                    ' ### SR PRCEPO: REFERENCE AMBIGUITY PROBLEM',
     2                    'FILE        : ',IF,
     3                    'SATELLITE   : ',AMBSAT(IAMB,IF),
     4                    'EPOCH NUMBER: ',OBSEPO(ISASES,IFREQ,IFLSES),
     5                    'SHOULD NEVER HAPPEN!!!!'
                        ENDIF
                      ENDIF
C
                      IF (IP2.LT.TIMFIL(ii,IF)%NINTER) THEN
                        DO IP=IP2,TIMFIL(ii,IF)%NINTER-1
                          TIMFIL(ii,IF)%AMBINT(IP)%INT%T(1)=
     1                               TIMFIL(ii,IF)%AMBINT(IP+1)%INT%T(1)
                          TIMFIL(ii,IF)%AMBINT(IP)%INT%T(2)=
     1                               TIMFIL(ii,IF)%AMBINT(IP+1)%INT%T(2)
                          TIMFIL(ii,IF)%AMBINT(IP)%REF=
     1                               TIMFIL(ii,IF)%AMBINT(IP+1)%REF
                        ENDDO
                      ENDIF
                      TIMFIL(ii,IF)%NINTER=TIMFIL(ii,IF)%NINTER-1
C
C ADD THE ACTUAL EPOCH
                    ELSE IF (IP1.NE.0) THEN
                      TIMFIL(ii,IF)%AMBINT(IP1)%INT%T(2)=TOBS
C
C ADD A NEW INTERVAL
                    ELSE
                      IP1=TIMFIL(ii,IF)%NINTER+1
                      CALL SIZE_AMBTIME(TIMFIL(ii,IF),IP1,5,NUMAMB(IF))
                      TIMFIL(ii,IF)%NINTER=IP1
                      TIMFIL(ii,IF)%AMBINT(IP1)%INT%T(1)=TOB0
                      TIMFIL(ii,IF)%AMBINT(IP1)%INT%T(2)=TOBS
                      TIMFIL(ii,IF)%AMBINT(IP1)%REF=0
                    ENDIF
C
                    IF(ICARR(IFRQ,IF).EQ.2) THEN
                      CLSNUM= AMBCLS(IAMB,2,IF)
                    ELSE IF(ICARR(IFRQ,IF).EQ.5) THEN
                      CLSNUM= AMBCLS(IAMB,3,IF)
                    ELSE
                      CLSNUM= AMBCLS(IAMB,1,IF)
                    ENDIF
                    clOk = 0
                    DO iCl = 1,iClu(ii,ip1,IF)
                      IF (CLSNUM ==
     1                          TIMFIL(ii,IF)%AMBINT(IP1)%CLU(ICL)) THEN
                        clOk = 1
                        EXIT
                      ENDIF
                    ENDDO
                    IF (clOk == 0) THEN
                      iClu(ii,ip1,IF)=iClu(ii,ip1,IF)+1
                      TIMFIL(ii,IF)%AMBINT(IP1)%CLU(ICLU(ii,ip1,IF))=
     1                                                            clsNum
                    ENDIF
C
C NEXT ROUND: system specific
                    IF (II == 0) THEN
                      II = INT(SAFLEP(ISAT,IFIL)/100)+1
                    ELSE
                      EXIT
                    ENDIF
                  ENDDO
C
                ENDIF
                OBSEPO(ISASES,IFREQ,IFLSES)=OBSNUM(IFLSES)
              ENDIF
C
C UPDATE STATISTICS ON EPOCHS CONNECTED BY CONT. PHASE OBSERVATIONS
C -----------------------------------------------------------------
              IF (MEATYP(IF).EQ.2) THEN
                II = 0
                DO WHILE (II <= MAXSYS)
                  IF (TIMFIL(II,IF)%NINTER==0) THEN
                    DO JF=1,IF-1
                      IF (STNAME(STFIL(1,IF))==STNAME(STFIL(1,JF))) THEN
                        IF (NDIFF(IF).EQ.0) THEN
                          TIMFIL(II,IF)%NINTER=JF
                          EXIT
                        ENDIF
                        IF (NDIFF(IF).GT.0 .AND. NDIFF(JF).GT.0)THEN
                          IF(STNAME(STFIL(2,IF))==STNAME(STFIL(2,JF)))
     1                                                              THEN
                            TIMFIL(II,IF)%NINTER=JF
                            EXIT
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDDO
                  ENDIF
                  IF (TIMFIL(II,IF)%NINTER/=0) THEN
                    DO JJ = 1,TIMFIL(II,TIMFIL(II,IF)%NINTER)%NINTER
                      hlpInt=TIMFIL(II,TIMFIL(II,IF)%NINTER)%AMBINT(JJ)
                      IF (TOBS >= hlpInt%INT%T(1).AND.
     1                    TOBS <= hlpInt%INT%T(2))THEN
                        IF (hlpInt%REF==2)THEN
                          WRITE(*,*)'ALARM'
                        ELSE
                        TIMFIL(II,TIMFIL(II,IF)%NINTER)%AMBINT(JJ)%REF=1
                          EXIT
                        ENDIF
                      ENDIF
                    ENDDO
                  ENDIF
C
C NEXT ROUND: system specific
                  IF (II == 0) THEN
                    II = INT(SAFLEP(ISAT,IFIL)/100)+1
                  ELSE
                    EXIT
                  ENDIF
                ENDDO
              ENDIF
C
300         CONTINUE
C
C PRINT SATELLITE ELEVATIONS FOR FIRST STATION AND FIRST FILE
C -----------------------------------------------------------
            IF(PRIOPT(7).EQ.1.AND.IF.EQ.FILNUM(1).AND.IPREPO.EQ.1) THEN
              IPREPO=0
              TEST=(TOBS-TPRINT)*1440.D0
              IF(TEST.GT.5.99D0) THEN
                WRITE(TEXT,311) OBSNUM(IFLSES),
     1                          (IDNINT(ELEVA(1,I)),I=1,NSASES)
311             FORMAT(I5,84I4)
                DO 312 I=1,NSASES
                  ICHR1=6+(I-1)*4
                  IF(ELEVA(1,I).EQ.0.D0) TEXT(ICHR1:ICHR1+3)='    '
312             CONTINUE
                WRITE(LFNPRT,313) TRIM(TEXT)
313             FORMAT(A)
                TPRINT=TOBS
              END IF
            END IF
            NSING=ISING
C
C CHECK O-C FOR DOUBLE DIFFERENCE CODE OBSERVATIONS
C -------------------------------------------------
              IF (NDIFF(IF).EQ.1 .AND.
     1            MEATYP(IF).NE.1 .AND.
     2            EDTLVL.GT.0.D0) THEN
                CALL MAJOR(NSING,BOBS(NDBTOT-NSING+1),
     1                     EDTLVL,NOK,NMAX,CDMEAN,IRFIND)
                ISING=0
                ISING2=0
                DO 303 ISFLEP=1,NSFLEP(IFIL)
                IF (SAFLEP(ISFLEP,IFIL).EQ.0) GOTO 303
                ISING=ISING+1
                TEST=DABS(BOBS(NDBTOT-NSING+ISING)-CDMEAN)
                IF (MARTYP.EQ.MTypeSPACE) THEN
                  CALL WGTELV(ICOELV(2,MEATYP(IF)),ZEN2(1),EDTNOR)
                  EDTLV0=EDTLVL*DSQRT(EDTNOR)
                ELSE
                  CALL WGTELV(ICOELV(1,MEATYP(IF)),ZEN2(1),EDTNOR)
                  EDTLV0=EDTLVL*DSQRT(EDTNOR)
                END IF
                IF (DABS(TEST).GT.EDTLV0) THEN
                  WRITE(LFNPRT,9999)TEST,EDTLV0,
     1                              STFIL(1,IF),STNAME(STFIL(1,IF)),
     2                              ISFLEP,SAFLEP(ISFLEP,IFIL),ELEV,
     3                              OBSNUM(IFLSES)
                  SAFLEP(ISFLEP,IFIL)=0
                ELSE
                  ISING2=ISING2+1
                  BOBS(NDBTOT-NSING+ISING2)=BOBS(NDBTOT-NSING+ISING)
                ENDIF
303             CONTINUE
                NSING=ISING2
              ENDIF

C
C UPDATE ACTUALLY OBSERVED SATELLITES IN FILE IFIL
C ------------------------------------------------
            NSNEW=0
            DO 310 ISAT=1,NSFLEP(IFIL)
              IF(SAFLEP(ISAT,IFIL).NE.0) THEN
                NSNEW=NSNEW+1
                SAFLEP(NSNEW,IFIL)=SAFLEP(ISAT,IFIL)
              ENDIF
310         CONTINUE
            NSFLEP(IFIL)=NSNEW
C
C NO SINGLE DIFFERENCE OBSERVATIONS FOR THIS FILE
            IF(NSING.LE.NDIFF(IF)) THEN
              IF (IAMSAV.NE.0) THEN
                IAMB1(IAMSAV,IFRQ,IFLSES)='U'
              ENDIF
              GOTO 400
            ENDIF
C
C SET CLUSTER NUMBERS FOR AMBIGUITY PARAMETERS
C --------------------------------------------
            IF (STRAMB(1).NE.-1 .AND. ICARR(1,IF).EQ.IFRSES .AND.
     1          NDIFF(IF).NE.0  .AND. MEA.EQ.1 .AND.
     2          MEATYP(IF).EQ.1) THEN
              CALL CLUSTA(NPAR-NPAEPO,NPARN,LOCQ,NSING,IPAMB,
     1                    ICLUST(IF),OBSCLS)
            ENDIF
C
C HANDLE CLOCK SYNCHRONIZATION FOR FILE IFIL
C ------------------------------------------
C           IF(IFRSES.NE.4 .AND. MELWUB.EQ.0) THEN
           IF(MEATYP(IF).NE.3) THEN
              IF(SYNFLG(IFLSES).EQ.'U') THEN
                SYNFLG(IFLSES)='R'
                CALL SETSYN(NSING,SATSAV,NSASES,
     1                      BOBS(NDBLE+1),ISYNC(1,IFLSES),
     2                      SYNC(1,IFLSES),SYNCM(IFLSES),BOLD(1,IFLSES))
                IF(ISYNCR.EQ.0.OR.IFRSES.EQ.4)THEN
                  SYNCMR(IFLSES)=0.D0
                ELSE
                  SYNCMR(IFLSES)=SYNCM(IFLSES)
                END IF
                IF(PRIOPT(8).EQ.1)THEN
                  IF(IFRPRI.EQ.1) THEN
                    IFRPRI=0
                    WRITE(LFNPRT,315)
315                 FORMAT(/,' SYNCHRONIZATION ERRORS (NS):',
     1                     /,' ',27('-'))
                  ENDIF
                  IF(SESSID.NE.SESOLD(1)) THEN
                    IF(SESOLD(1).NE.'****') WRITE(LFNPRT,316)
316                   FORMAT(' ')
                    SESOLD(1)=SESSID
                    WRITE(LFNPRT,317) SESSID,(FILNUM(I),I=1,NFLSES)
317                 FORMAT(/,' SESSION: ',A4,/,' FILES  : ',30I4,/,
     1                     10(10X,50I4,/))
                    DO K=1,NSASES
                      CALL SVN2CHR(SATSES(K),ISVMOD,SVNCHR)
                      WRITE(SVNPRI(K),"(A1,I2.2)") SVNCHR,ISVMOD
                    ENDDO
                    WRITE(LFNPRT,318) (SVNPRI(K),K=1,NSASES)
318                 FORMAT(/,' FILE  IOBS ',80(2X,A3,1X))
                    WRITE(LFNPRT,319)
319                 FORMAT(' ',131('-'),/)
                  ENDIF
                  WRITE(TXTSYN,321) FILNUM(IFLSES),OBSNUM(IFLSES),
     1                            (SYNC(K,IFLSES)*1.D9,K=1,NSASES)
321               FORMAT(I5,I6,1X,80F6.0)
                  DO 322 IS=1,NSASES
                    DO 325 ISFL=1,NSFLEP(IFIL)
                      IF(SAFLEP(ISFL,IFIL).EQ.SATSES(IS)) GOTO 322
325                 CONTINUE
                    ICHR1=13+(IS-1)*6
                    TXTSYN(ICHR1:ICHR1+5)='      '
322               CONTINUE
                  WRITE(LFNPRT,'(A)') TRIM(TXTSYN)
                END IF
                IF(DABS(SYNCMR(IFLSES))*1.D9.GT.XMAXCL(FILNUM(IFLSES)))
     1            XMAXCL(FILNUM(IFLSES))=DABS(SYNCMR(IFLSES))*1.D9
              ENDIF
              DO 320 ISING=1,NSING
                BOBS(NDBTOT)=BOBS(NDBTOT)-SYNCMR(IFLSES)*DERCLK(ISING)
320           CONTINUE
C           ENDIF
           ENDIF
C
C FORM DOUBLE DIFFERENCES IN CASE OF SINGLE DIFFERENCE FILES
C ----------------------------------------------------------
            IF (NDIFF(IF).EQ.1) THEN
              CALL BLDDBL(NDBLE ,NSING ,MAXEQN,MAXSNG,AAUX  ,INDAUX,
     1                    MELWUB,AOBS  ,INDA  ,BOBS  ,MXESNG)
            ELSE
              CALL BLDZRO(NDBLE ,NSING ,MAXEQN,MAXSNG,AAUX  ,INDAUX,
     1                    MELWUB,AOBS  ,INDA  ,BOBS  ,MXESNG)
            ENDIF
C
C UPDATE CORRELATION MATRIX INFORMATION
C -------------------------------------
            CALL ADDCOR(NDBLE,IFRSES,NSASES,NSFLEP(IFIL),SATSAV,
     1                  STFIL(1,IF),MELWUB,MEATYP(IF),NDIFF(IF),
     2                  TOBS,DTSIM,STNAME,IZEROD,ICOELV,ELEVA,
     3                  COVELV,CIND,FIND,DIFF)
C
C SAVE ESSENTIAL INFORMATION FOR SUBSEQUENT WRITING OF OBS-EQNS.
C --------------------------------------------------------------
            IF(STRAMB(1).NE.-1) THEN
              IF(STRAMB(1).NE.0.OR.IRESID.NE.0) THEN
                DO 330 IDBLE=1,NSING-NDIFF(IF)
                    NDBIDB=NDBLE+IDBLE
                    SVNOBS(1,NDBIDB)=SATSES(SATSAV(IDBLE))
                    IF (NDIFF(IF).EQ.1) THEN
                      SVNOBS(2,NDBIDB)=SATSES(SATSAV(IDBLE+1))
                    ELSE
                      IELE=IDNINT(ELEVA(1,SATSAV(IDBLE))*1.0D2)
                      IAZI=IDNINT(AZIMU(1,SATSAV(IDBLE))*1.0D2)
                      IF (IELE.GT.9999) IELE=9999
                      IF (IELE.LT.0) IELE=0
                      WRITE(TMPSTR,335)IAZI,IELE
                      READ(TMPSTR,336)IAE
335                   FORMAT(I5.5,I4.4)
336                   FORMAT(I9)
                      SVNOBS(2,NDBIDB)=IAE
                    ENDIF
                    IFLOBS(NDBIDB)=IFLSES
                    IFRSE2(NDBIDB)=IFRSES
330             CONTINUE
              ENDIF
            ENDIF
C
C END PROCESSING OF FILE IFIL
            NDBLE=NDBLE+NSING-NDIFF(IF)
            NUMOBS(IFRQ,IF)=NUMOBS(IFRQ,IF)+NSING-NDIFF(IF)
400       CONTINUE
C
C CORRELATIONS, UPDATE NORMAL EQUATIONS, ...
C CORSTR=1,2: CORRELATIONS ETC. FOR ALL OBS. OF THE SAME FREQUENCY
C CORSTR=3  : CORRELATIONS ETC. AFTER ALL OBS. OF EPOCH
C ----------------------------------------------------------------
410       IF(CORSTR.LE.2.OR.IFREQ.EQ.NFRSES) THEN
C
C NO DOUBLE DIFFERENCES FOR THIS FREQUENCY OR EPOCH ?
            IF(NDBLE.EQ.0) GOTO 450
C
C CORRELATIONS
C ------------
            CALL CORREL(NDBLE ,NSASES,NSTAT ,COVSAT,COVELV,CIND  ,
     1                  FIND  ,DIFF  ,WGTGEN,CINDO ,FINDO ,WEIGHT)
C
C UPDATE NORMAL EQUATION SYSTEM
C -----------------------------
            RMSOLD=RMSSUM
            IF (IZEROD.EQ.1) THEN
              WGTDEF=1D0
            ELSE
              WGTDEF=WGTGEN(MEA)
            ENDIF
C
C CONVERT RMS TO L1 PHASE FOR WUEBB LC
            IF (MELWUB.EQ.1) WGTDEF=FRMSMW(SATSES(1))
            IF(STRAMB(1).EQ.-1) THEN
              CALL ADDNOR(NDBLE ,NPASES,WGTDEF,WEIGHT,AOBS  ,INDA  ,
     1                    INDI  ,INDK  ,HADD  ,MAXEQN,BOBS  ,
     2                    ANOR  ,BNOR  ,RMSSUM,NOBS  )
            ELSE
              CALL ADDNOR(NDBLE ,NPAR  ,WGTDEF,WEIGHT,AOBS  ,INDA  ,
     1                    INDI  ,INDK  ,HADD  ,MAXEQN,BOBS  ,
     2                    ANOR  ,BNOR  ,RMSSUM,NOBS  )
            ENDIF
            RMSSES=RMSSES+(RMSSUM-RMSOLD)
C
C WRITE DOUBLE DIFFERENCES ON FILE
C --------------------------------
            IF(STRAMB(1).NE.-1) THEN
              IF(IRESID.NE.0) THEN
                IF(NSAMPL(2).NE.0.AND.IZEROD.NE.0) THEN
                  CALL MJDGPS(TOBS,GPSSEC,NWEEK)
                  TFRAC=GPSSEC-DNINT(GPSSEC/NSAMPL(2))*NSAMPL(2)
                ELSE
                  TFRAC=0d0
                ENDIF
                IF(DABS(TFRAC).LE.DTSIM*86400.D0) THEN
                  CALL WTRESI(NDBLE ,MAXEQN,MXESNG,NPAR  ,WEIGHT,SVNOBS,
     1                        IFLOBS,OBSNUM,IFRSE2,FILNUM,DTFIL ,XXX0  ,
     2                        AOBS  ,INDA  ,BOBS  ,LOCQ  ,IZEROD,NPAEPO,
     3                        MAXFLS,IPHSEP,MEATYP)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
C
C END PROCESSING OF FREQUENCY "IFRSES"
450     CONTINUE
C
C END PROCESSING OF MEASUREMENT TYPE "MEA"
500   CONTINUE
C
C SET RETURN CODE
C ---------------
      IRETC=0
999   CONTINUE
C
C DEALLOCATE LOCAL ARRAYS IN THE LAST CALL
      IF (LSTSES.AND.IRETC.NE.0) THEN
        DEALLOCATE(SYNFLG,STAT=IAC)
        DEALLOCATE(FILACT,STAT=IAC)
        DEALLOCATE(FGFLEP,STAT=IAC)
        DEALLOCATE(INDI,STAT=IAC)
        DEALLOCATE(INDK,STAT=IAC)
        DEALLOCATE(NSATFL,STAT=IAC)
        DEALLOCATE(SVNEP,STAT=IAC)
        DEALLOCATE(FILEP,STAT=IAC)
        DEALLOCATE(SATEPO,STAT=IAC)
        DEALLOCATE(NSFLEP,STAT=IAC)
        DEALLOCATE(SAFLEP,STAT=IAC)
        DEALLOCATE(SVNMW,STAT=IAC)
        DEALLOCATE(SAFLMW,STAT=IAC)
        DEALLOCATE(SATSAV,STAT=IAC)
        DEALLOCATE(SVNOBS,STAT=IAC)
        DEALLOCATE(IFLOBS,STAT=IAC)
        DEALLOCATE(IFRSE2,STAT=IAC)
        DEALLOCATE(CIND,STAT=IAC)
        DEALLOCATE(CINDO,STAT=IAC)
        DEALLOCATE(FIND,STAT=IAC)
        DEALLOCATE(FINDO,STAT=IAC)
        DEALLOCATE(DIFF,STAT=IAC)
        DEALLOCATE(IPAMB,STAT=IAC)
        DEALLOCATE(TECINF,STAT=IAC)
        DEALLOCATE(COVSAT,STAT=IAC)
        DEALLOCATE(COVELV,STAT=IAC)
        DEALLOCATE(DTFIL,STAT=IAC)
        DEALLOCATE(BOLD,STAT=IAC)
        DEALLOCATE(DERCLK,STAT=IAC)
        DEALLOCATE(OBFLEP,STAT=IAC)
        DEALLOCATE(OBFLMW,STAT=IAC)
        DEALLOCATE(CODDIF,STAT=IAC)
        DEALLOCATE(XVA,STAT=IAC)
        DEALLOCATE(ELE,STAT=IAC)
        DEALLOCATE(HADD,STAT=IAC)
        DEALLOCATE(ELEVA,STAT=IAC)
        DEALLOCATE(AZIMU,STAT=IAC)
        DEALLOCATE(TECOBS,STAT=IAC)
        DEALLOCATE(SYNCMR,STAT=IAC)
      ENDIF
      RETURN
      END SUBROUTINE


      END MODULE

