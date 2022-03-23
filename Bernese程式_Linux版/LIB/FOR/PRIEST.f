      MODULE s_PRIEST
      CONTAINS

C*
      SUBROUTINE PRIEST(TITLES,IPART ,NPARMS,NPAR  ,NPARN ,NOBS  ,
     1                  NFTOT ,MEATYP,NUMOBS,PRIOPT,RMS   ,TRPLMS,
     2                  SIGAPR,SECIPL,ICOELV,XXX   ,NSTAT ,STNAME,
     3                  STANUM,
     3                  ICENTR,XSTAT ,XSTELL,XSTECC,LOCQ  ,ANOR  ,
     4                  XXX0  ,AELL  ,BELL  ,DXELL ,DRELL ,SCELL ,
     5                  STRAMB,NFRFIL,ICARR ,MAXFRS,NOSOL ,NUMAMB,
     6                  AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,NREF  ,
     7                  SCASTC,SCAPOT,SCAHIL,SCAALB,SCACEN,TIMSTC1,
     8                  TIMOFF,ANTCAL,NUMCAL,PRNCAL,OPTGIM,POLGIM,
     9                  NAMGIM,EPOGIM,SCAGIM,ANTRAO,NUMRAO,PRNRAO,
     .                  MAXTYP,OPTELI,PARLST,MELWUB,IEXTRA,ITROPO,
     1                  ITRGRD,ZENMAX,RECTYP,STFIL ,NDIFF ,INFGIM,
     2                  NEPOBS,NCLKST,NCLKSA,RNXCLK,CLKHED,CLKREC,
     3                  NSAMPL,IRAUX2,TIMSTC2,PARFLG,NADMAX,NAMAX ,
     4                  NADIGN,RAPZENMAX,IACST,SATSPEC,OPLOAD,
     5                  NALLSAT,ALLSATNUM,CLFRTO,TIMISB,
     6                  MXCPAR,USEGEOS,GOBSDEF,TIMREF,IDELTT)
CC
CC NAME       :  PRIEST
CC
CC PURPOSE    :  PRINT RESULTS ESTIMATED BY PROGRAM GPSEST
CC
CC PARAMETERS :
CC         IN :  TITLES(I),I=1,2: TITLE LINES                 CH*132
CC               IPART  : RESULTS PART 1(=1) OR 2 (=2)        I*4
CC               NPARMS : NUMBER OF PARAMETERS TO COMPUTE RMS I*4
CC               NPAR   : NUMBER OF PARAMETERS (WITHOUT PRE-  I*4
CC                        ELIMINATED PARAMETERS)
CC               NPARN  : NUMBER OF PARAMETERS WITHOUT AMBIG. I*4
CC               NOBS   : NUMBER OF DOUBLE DIFFERENCE OBSERV. I*4
CC               NFTOT  : TOTAL NUMBER OF FILES               I*4
CC               MEATYP(I),I=1,..,NFTOT: MEASUREMENT TYPES    I*4
CC               NUMOBS(J,I),I=1,..,MAXFRQ,J=1,..,NFTOT:      I*4
CC                        NUMBER OF OBSERVATIONS PER FILE
CC               PRIOPT(I): I=9 PRINT NUMBER OF OBSERVATIONS  I*4(*)
CC                              PER FILE
CC                          I=12 DO NOT PRINT EPOCH PARAMETERS
CC                          I=14 DO NOT PRINT TROPO PARAMETERS
CC                          I=15 DO NOT PRINT COORD PARAMETERS
CC                          I=16 DO NOT PRINT AMBIG PARAMETERS
CC               RMS    : RMS ERROR OF UNIT WEIGHT (M)        R*8
CC               SIGAPR : A PRIORI SIGMA OF UNIT WEIGHT (M)   R*8
CC               SECIPL : INTERVAL FOR CLOCK INTERPOL. (SEC)  R*8
CC               ICOELV : MODEL FOR ELEV.-DEP. OBS. WEIGHTING I*4
CC                        =0: EQUAL WEIGHTING FOR ALL OBS.
CC                        >0: MODEL NUMBER (SEE SR WGTELV)
CC               XXX(I) : SOLUTION VECTOR                     R*8
CC               NSTAT  : NUMBER OF STATIONS                  I*4
CC               STNAME(I),I=1,2,..,NSTAT : STATION NAMES     CH*16
CC               STANUM(I),I=1,2,..,NSTAT : STATION NUMBERS   I*4
CC               ICENTR(I),I=1,2,..,NSTAT : INDEX OF CENTER   I*4
CC                        STATION FOR STATION I
CC               XSTAT(K,I),K=1,2,3 , I=1,2,..,NSTAT: RECT-   R*8
CC                        ANGULAR GEOCENTRIC STATION COORD.
CC               XSTELL(K,I),K=1,2,3 ,I=1,2,..,NSTAT: ELLIP-  R*8
CC                        SOIDAL COORDINATES
CC               XSTECC(K,I),K=1,2,3 ,I=1,2,..,NSTAT: ECCEN-  R*8
CC                        TRICITIES FOR STATION I
CC               LOCQ(K,I),K=1,..,MAXLCQ, I=1,2,...,NPAR:     I*4
CC                        CHARACTERISTICS FOR EACH PARAMETER
CC               ANOR(I),I=1,2,..,NPAR*(NPAR+1)/2: INVERSE OF R*8
CC                        NORMAL EQUATION MATRIX (UPPER
CC                        TRIANGLE ONLY, COLUMNWISE LINEAR.)
CC               XXX0(I),I=1,2,..,NPAR A PRIORI VALUES        R*8
CC               AELL,BELL: SEMIMAJOR/MINOR AXES OF ELLIPSOID R*8
CC               DXELL(I),I=1,2,3: SHIFT OF DATUM TO WGS      R*8
CC               DRELL(I),I=1,2,3: ROTATIONS TO WGS-84        R*8
CC               SCELL  : SCALE FACTOR TO WGS-84              R*8
CC               STRAMB : STRATEGY OF AMBIGUITY RESOLUTION    I*4(*)
CC               NFRFIL(I),I=1,..,NFTOT: NUMBER OF FREQUEN-   I*4
CC                        CIES TO BE PROCESSED FI OR FILE I
CC               ICARR(K,I),K=1,..,NFRFIL(I),I=1,..,NFTOT:    I*4
CC                        FREQUENCIES TO BE PROCESSED FOR
CC                        FILE I
CC               MAXFRS : MAXIMUM NUMBER OF DIFFERENT FREQ.   I*4
CC               NFROBS(K,I),K=1,MAXMEA,                      I*4
CC                        I=1,..,MAXFRS: NUMBER OF OBSERV.
CC                        WITH MEASUREMENT TYPE
CC                        K AND FREQUENCY I
CC               NOSOL  : =1: COMPUTE NO SOLUTION             I*4
CC                        =0: COMPUTE SOLUTION
CC               NUMAMB(I),I=1,..,NFTOT: NUMBER OF AMBIGU.    I*4
CC               AMBSAT(J,I),J=1,..,NUMAMB(I),I=1,..,NFTOT:   I*4
CC                        AMBIGUITY SATELLITE NUMBERS
CC               AMBIEP(J,I),J=1,..,NUMAMB(I),I=1,..,NFTOT:   I*4
CC                        STARTING EPOCH NRS FOR AMBIGUITIES
CC               AMBWLF(L,K,I),L=1,..,NUMAMB(I), K=1,2 ,      R*8
CC                        I=1,..,NFTOT: WAVELENGTH FACTORS
CC               AMBIGU(L,K,I),L=1,..,NUMAMB(I), K=1,2,3,     R*8
CC                        I=1,..,NFTOT: AMBIGUITIES
CC               AMBCLS(L,K,I),L=1,..,NUMAMB(I), K=1,2,3,     I*4
CC                        I=1,..,NFTOT: AMBIGUITY CLUSTERS
CC               NREF   : NUMBER OF REFERENCE AMBIGUITIES     I*4
CC               SCASTC : SCALING FACTOR FOR                  R*8(*)
CC                        (1): STOCHASTIC PULSES
CC                        (2): STOCHASTIC ACCELERATIONS
CC               SCAPOT : SCALE FOR EARTH POT PARMS           R*8
CC               SCAHIL : SCALE FOR HILL PARMS                R*8
CC               SCAALB : SCALE FOR ALB. PARMS                R*8
CC               SCACEN : SCALE FOR CENTER OF MASS COORD.     R*8
CC               TIMSTC1: EPOCHS WITH STOCH. PERTURBATIONS  R*8(*,*,*,*)
CC               TIMSTC2: EPOCHS WITH STOCH. PERTURBATIONS  R*8(*,*,*,*)
CC                        LEO ONLY
CC               TIMOFF(J,I),J=1,2,I=1,..,NRQOFF: TIME INTER- R*8
CC                        VAL FOR SATELLITE ANTENNA OFFSET
CC                        REQUEST I
CC               ANTCAL(J,I),J=1,2, I=1,..,NANCAL:            CH*20
CC                        RECEIVER (J=1) AND ANTENNA (J=2)
CC                        NAME FOR RECEIVER ANTENNA PHASE
CC                        CENTER REQUEST I
CC               NUMCAL(2,I),I=1,..,NANCAL: ANTENNA NUMBERS   I*4
CC                        "FROM - TO" FOR REQUEST I
CC               PRNCAL(I),I=1,..,NANRAO: SAT.SYSTEM FOR      I*4
CC                        RECEIVER ANT. OFFSET REQUEST I
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
CC               NAMGIM(I),I=1,..,OPTGIM(7): MODEL NUMBERS    CH*16(*)
CC               EPOGIM(I,J),I=1,2,J=1,..,OPTGIM(7): PERIODS  R*8(2,*)
CC                        OF VALIDITY / REF EPOCHS (MJD)
CC               SCAGIM : SCALING FACTOR FOR                  R*8(*)
CC                        (1): ION. COEFFICIENTS
CC                        (2): SINGLE-LAYER HEIGHT
CC                        (3): DTEC PARAMETERS
CC               ANTRAO(J,I),J=1,2, I=1,..,NANRAO:            CH*20
CC                        RECEIVER (J=1) AND ANTENNA (J=2)
CC                        NAME FOR RECEIVER ANTENNA OFFSET
CC                        REQUEST I
CC               NUMRAO(2,I),I=1,..,NANRAO: ANTENNA NUMBERS   I*4
CC                        "FROM - TO" FOR REQUEST I
CC               PRNRAO(I),I=1,..,NANRAO: SAT.SYSTEM FOR      I*4
CC                        RECEIVER ANT. OFFSET REQUEST I
CC               MAXTYP : MAX. NUMBER OF PARAMETER TYPES      I*4
CC               OPTELI(I),I=1,..,MAXTYP: OPTION FOR PRE-     I*4
CC                        ELIMINATION OF PARAMETER TYPES:
CC                        =0 : NOT PRE-ELIMINATED
CC                        =1 : PRE-ELIMINATED BEFORE INVERSION
CC                        =2 : PRE-ELIMINATED AFTER  INVERSION
CC                        =3 : PRE-ELIMINATED EPOCH-WISE
CC               PARLST(I,K), I=1,..,5,K=1,..,MAXTYP: NUMBER  I*4
CC                        OF PARAMETERS:
CC                        I=1: #PARAMETERS OF TYPE I (NPARMS)
CC                        I=2: #SET-UP
CC                        I=3: #NO-OBS
CC                        I=4: #REF. PARAMETERS
CC                        I=5: #SINGULAR
CC               MELWUB : MELBOURNE-WUEBBENA LC               I*4
CC                        =0: NO
CC                        =1: YES
CC                        =2: DTEC LC
CC               IEXTRA : EXTRAPOLATED METEO USED             I*4
CC                        =0: NO
CC                        =1: YES
CC                        =2: ESTIMATED VALUES USED
CC               ITROPO : TROPOSPHERIC MODEL                  I*4
CC               ITRGRD : (1): EST. OF TROPOSPHERIC GRADIENTS I*4(*)
CC                             =0: NO ESTIMATION
CC                             =1: TILTING
CC                             =2: LINEAR
CC                        (2): RATIO OF NUMBER OF ZENITH TO
CC                             GRADIENT PARAMETERS
CC               ZENMAX : MAXIMUM ZENITH DISTANCE IN RAD      R*8
CC               RECTYP(K,I),K=1,2,I=...: RECEIVER TYPES      CH*20
CC               STFIL(K,I),K=1,2, I=1,2,..,NFTOT: STATIONS   I*4
CC                        IN FILES
CC               NDIFF(I):I=1,..,NFTOT: DIFFERENCE TYPE       I*4
CC                        NDIFF=0: ZERO DIFFERENCE
CC                        NDIFF=1: SINGLE DIFFERENCE
CC               PARFLG(K),K=1,..NPAR: FLAG FOR SINGULAR PAR. I*4
CC                        =0 : PARAMETER NOT SINGULAR
CC                        =1 : PARAMETER SINGULAR
CC               NADMAX : MAXIMUM NADIR ANGLE ALLOWED FOR     R*8
CC                        SAT. ANT. PATTERN ESTIMATION
CC               NAMAX  : ACTUAL MAXIMUM NADIR ANGLE          R*8
CC               NADIGN : NUMBER OF OBSERVATIONS IGNORED DUE  I*4
CC                        TO A TOO HIGH NADIR ANGLE
CC               RAPZENMAX : MAXIMUM ZENITH ANGLE ALLOWED FOR R*8
CC                        REC. ANT. PATTERN ESTIMATION
CC               IACST  : NUMBER OF CONSTRAINED AMBIGUITIES   I*4
CC               SATSPEC: Sat. SELECTION FOR RANGE BIASES     I*4
CC               NALLSAT:     ! NUMBER OF ALL SATELLITES      I*4
CC               ALLSATNUM:   ! SATELLITE NUMBERS             I*4(*)
CC               CLFRTO : TIME INTERVAL FOR CLOCK ERRORS      R*8(2,*)
CC                        CLFRTO(1,K): START OF THE INTERVAL
CC                        CLFRTO(2,K): END OF THE INTERVAL
CC                        IN JULIAN DATE FOR CLOCK REQUEST K
CC               MXCPAR : MAXIMUM # OF PARAMETERS ACTUALLY USED
CC               IDELTT : OBSERVATION INTERVAL OF ALL FILES   I*4(*)
CC               TIMREF(I),I=1,2,..,NFTOT: FIRST OBSERVATION  R*8
CC                        EPOCH OF OBS.FILE NUMBER I
CC               GOBSDEF  Giove External Obs. Selection info  t_gobsdef
CC               USEGEOS  =0/1: GOBSDEF data set not/available I*4
CC        OUT :  INFGIM(I,J),I=1,2,J=1,..,OPTGIM(7):          R*8(2,*)
CC                        I=1: MAXIMUM TEC (TECU)
CC                        I=2: RMS ERROR (TECU)
CC               NEPOBS(I): MIN: # OF OBS. FOR EPOCH PARAM.S  I*4(3)
CC                        (I=1 STA-CLK / I=2 SAT-CLK / I=3 KIN.)
CC               NCLKST : NUMBER OF ESTIMATED STATION CLOCKS  I*4
CC               NCLKSA : NUMBER OF EST. SATELLITE CLOCKS     I*4
CC               RNXCLK:  WHAT TO DO IF NO INPUT CLOCK RINEX  I*4
CC                        FOR SATELLITE CLOCKS:
CC                         -1: IGNORE CLOCK RINEX FILE
CC                          0: TRY ALSO SAT CLK FILE
CC                          1: USE OBS. (INTERPOL. CLK RNX)
CC                          2: SKIP OBS.
CC                          3: USE OBS. (SAT CLK = ZERO)
CC               CLKHED : CLOCK HEADER INFORMATION            T_CLKHEAD
CC                          %NUMREF=0: FIX REF-CLOCKS
CC                          %NUMREF=2: SUM FOR REF-CLOCKS
CC               CLKREC : %NEPO: EPOCHS WITH HIGHEST SAMPL.   T_CLKREC
CC                        %EPOCH(1): LAST EPOCH
CC                                (SEC. SINCE CLKHED%TFIRST)
CC                        %CLOCK(ISTA,IEPO): APRIORI STATION
CC                                CLOCK (IF NOT PREELIM BI or EP)
CC               NSAMPL : SAMPLING RATE (SEC)                 I*4(3)
CC                        1: OBSERVATIONS
CC                        2: RESUBSTITUTION OF EPOCH PARAMETERS
CC                        3: PREELIMINATE OF EPOCH PARAMETERS
CC               IRAUX2 : EPOCH RESULT SCRATCH FILE IS        I*4
CC                        0: AVAILABLE / 1: NOT AVAIL.
CC               OPLOAD : SCALING FACTORS FOR VIENNA GRID FILES   T_OPTLOAD(3)
CC                        1: ATMOSPHERIC NON-TIDAL LOADING
CC                        2: OCEAN NON-TIDAL LOADING
CC                        3: HYDROSTATIC PRESSURE LOADING
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, W.GURTNER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/02 09:00
CC
CC CHANGES    :  27-MAY-91 : ??: DON'T PRINT TRAILING BLANKS
CC               10-SEP-91 : ??: PRINT DIFFERENCES AND RMS FOR LATITUTDE
CC                               AND LONGITUDE IN METERS INSTEAD OF ARCSEC
CC               23-SEP-91 : ??: INCLUDE COORD.OUTPUT FILENAME INTO OUTPUT
CC               09-DEC-91 : ??: ADD "NPARMS" AS PARAMETER
CC               13-FEB-92 : ??: CHANGES DUE TO ERP-ESTIMATION
CC               02-AUG-92 : ??: CHANGES DUE TO POLYNOM ESTIMATION OF ERPS
CC               18-JAN-93 : ??: USE SR "PRIAMI" TO PRINT AMBIGUITIES
CC               21-JAN-93 : ??: DECLARATION OF HLP1,HLP2
CC               20-MAR-93 : ??: STOCHASTIC ORBIT PARAMETERS
CC               03-APR-93 : ??: ESTIMATION OF SATELLITE ANTENNA OFFSETS
CC               26-APR-93 : ??: PARAMETER SUMMARY CHANGED IF PARAMETERS
CC                               ARE PREELIMINATED
CC               30-SEP-93 : ??: 3-D AND 2-D ERROR ELLIPSOIDS
CC               28-DEC-93 : MR: TIME WINDOWS FOR SAT.ANT. OFFSETS
CC               23-FEB-94 : MR: MORE DIGITS FOR RECEIVER CLOCK PARAM.
CC               12-APR-84 : MR: ADD "AMBWLF", REMOVE "IAMB1" FROM
CC                               PARAMETER LIST
CC               13-APR-94 : SS: DIFFERENTIAL IONOSPHERE PARAMETERS
CC               19-APR-94 : RW: CPO-MODEL PARAMETERS
CC               25-APR-94 : MR: INTRODUCE STRING "ERPTXT"
CC               25-JUL-94 : MR: PRE-ELIMINATION STATISTICS SIMPLIFIED
CC               28-JUL-94 : MR: 4 DIGITS FOR TROPOSPHERE PARAMETERS
CC               10-AUG-94 : MR: CALL EXITRC
CC               12-AUG-94 : LM: MORE DIGITS FOR RMS OF ORB. ELEMENTS
CC               29-AUG-94 : LM: ANOTHER FORMAT FOR RMS OF ORB. ELEMENTS
CC               13-OCT-94 : MR: REMOVE COMMA IN "WRITE"
CC               06-NOV-94 : MR: ANTENNA PHASE CENTER PARAMETERS
CC               06-JUN-95 : SS: GLOBAL IONOSPHERE MODEL PARAMETERS
CC               08-JUN-95 : LM: KINEMATIC COORDINATES
CC               12-JUN-95 : LM: RMS RELATIVE TO 1ST EPOCH
CC               14-AUG-95 : LM: STRAMB=ARRAY
CC               30-OCT-95 : MR: ADD PARAMETER "MELWUB" TO CALL
CC               05-DEC-95 : SS: NEW IONOSPHERE MODEL (TYPE 2)
CC               05-DEC-95 : SS: APPLY "SCAGIM"
CC               05-DEC-95 : SS: NEW SR PRIGIM
CC               20-DEC-95 : SS: DECLARE "NUMGIM" AS CH*7
CC               25-JAN-96 : GB: NEW ORBIT MODEL IMPLEMENTED
CC               26-MAR-96 : MR: RECEIVER ANTENNA OFFSETS
CC               27-MAR-96 : TS: CLOCK/SLR CHANGES
CC               04-JUN-96 : MR: REMOVE UNUSED VARIABLE
CC               08-APR-97 : SS: NIELL MAPPING, TROPOSPHERE GRADIENTS
CC               14-APR-97 : SS: PRINTING OF TROPOSPHERE PARAMETERS
CC               15-APR-97 : SS: WRITING OF SIGMA OF OBSERVATION
CC               23-MAY-97 : TS: CORRECTED EPOCH CLOCK UNITS: ADDED "COMCONST"
CC               14-AUG-97 : SS: DIFFERENTIAL CODE BIASES
CC               08-SEP-97 : SS: WRITING OF SIGMA OF OBSERVATION
CC               08-OCT-97 : MR: RATIO ZENITH/GRADIENT PARAMETERS
CC               09-OCT-97 : SS: PRINT POST-FIT SIGMA FOR ONE-WAY OBSERVABLE
CC               30-OCT-97 : SS: SIGMA OF ZERO-DIFFERENCE OBS
CC               26-JAN-98 : SS: STATION-SPECIFIC GIMS
CC               29-APR-98 : SS: DTEC LC
CC               12-MAY-98 : MR: ADD COMMA IN FORMAT
CC               26-MAY-98 : SS: "INFGIM" ADDED
CC               04-AUG-99 : RD: CORRECT 360 DEG PROBLEM IN "DLON" (2x)
CC               27-JAN-00 : TS: CHANGES FOR CLOCK OUTPUT
CC               17-APR-00 : RD: APRIORI AMBIG. FOR ZERO DIFF SOLUTIONS
CC               28-AUG-00 : MR: ADD PARAMETERS IEXTRA,ITROPO
CC               22-JAN-02 : RD: PRINT NUMBER OF OBS FOR CLOCKS
CC               23-JAN-02 : RD: REMOVE ZAMBIG
CC               24-JAN-02 : RD: NEW SR PRIKIN+PRICLK
CC                               NO MORE PROGRAM OUTPUT IN SR RESEPO
CC               25-JAN-02 : RD: USE APRIORI CLOCKS FOR STA+SAT
CC               29-JAN-02 : RD: OPTIONAL PRINT OF EPOCH PARAM.
CC               07-MAY-02 : SS: DCB UPDATE
CC               04-JUN-02 : RD: CORRECT CHECK FOR EPOCH PARAMETERS
CC               20-FEB-02 : DS: HANDLE SPACEBORNE, AIRBORNE AND KINEMATIC
CC               25-JUN-02 : RD/DS: MERGING VERSION BE AND MUNICH
CC               14-NOV-02 : RS: SATELLITE ANTENNA PHASE CENTER VARIATIONS
CC               28-JAN-03 : RD: NUMBER OF OBS. FOR KIN. POS.(CLKOBS->NEPOBS)
CC                               USE IRAUX2 FOR EPOCH RESULT SCRATCH FILE
CC               19-FEB-03 : RS: INITIALISATION OF ANTNAD
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE
CC               07-MAR-03 : MR: PARAMETER DESCRIPTION UPDATED
CC               08-MAR-03 : HU: ADD TOBS FOR KINEMATIC SCRATCH FILE
CC               10-MAR-03 : CU: BUG FIXED, USE NEW SR RDSCRA
CC               13-MAR-03 : CU: BUG FIXED (FORMAT STATEMENTS)
CC               19-MAR-03 : CU: FORMAT STATEMENTS CORRECTED, TITLE FOR DCB
CC               24-APR-03 : RS: CORRECT SKELETON FILE SUBSTITUTES
CC               26-MAY-03 : RD: NEW CALL OF SR PRIAMI
CC               19-JUN-03 : RD: ADD NEPOBS TO THE CALL OF PRIKIN
CC               19-JUN-03 : RD: ADD PARFLG TO PARAMETER LIST OF PRIKIN
CC               26-JUN-03 : RD: NEW FORMATTED OUTPUT FOR KINEMATIC COORD.
CC               08-SEP-03 : HU: ANTNAM, RECNAM CHR16 -> CHR20
CC               11-NOV-03 : RS: ADD RAPZENMAX, CHANGE RECNAM AND ANTNAM IN
CC                               OUTPUT, SATELLITE ANTENNA OFFSETS: PRINT
CC                               USER-DEFINED GROUP NUMBERS
CC               05-DEC-03 : RS: WRITE CORRECT MODTYP FOR ESTIMATION OF
CC                               RECEIVER ANTENNA PHASE CENTER VARIATIONS
CC               10-DEC-03 : AJ: DIFF. TYPES OF STOCH. PARAMETERS
CC               16-FEB-04 : RD: REPORT SINGULAR COORDINATES
CC               04-JAN-05 : RD: REMOVE NFROBS FROM PARAMETER LIST
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-Aug-05 : HB: Use new SR TIMST2 (module)
CC               07-JUN-06 : RD: COMPUTE NO SOLUTION
CC               11-JUN-06 : HB: Get a priori values from scratch file
CC               29-JUN-06 : HB: ... or from XXX0-array
CC               24-AUG-06 : AG: TRPLMS FOR PRITRP ADDED
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC               01-NOV-07 : HB: ADD PARAMETER SECIPL
CC               29-JAN-08 : SL: MORE PRINTING OPTIONS
CC               30-JAN-08 : HB: COUNT FOR CONSTRAINED AMBIGUITIES
CC               24-JUN-08 : DT: ASSUMING SVN FOR SLR >=951
CC               04-AUG-08 : DT: SYSTEM OF DYNAMIC ORBIT PARAMETERS
CC                               (DYX, RSW, DRSW)
CC               28-OCT-08 : DT: USE MAXVAR FROM M_MAXDIM
CC               10-NOV-08 : AJ: CORRECT CAPTION FOR STOCH. PARAMETERS
CC               12-NOV-08 : DT: TAKE ORBIT DESCRIPTION FROM ORBDSC%ORBMOD
CC               02-APR-09 : DT: ADD RANGE BIASES (TYP 26)
CC               04-MAY-09 : RD: SCALING OF LOADING MODELS ADDED
CC               09-MAY-09 : RD: SAT/FRQ-SPECIFIC RECEIVER CLOCK BIASES
CC               09-MAY-09 : RD: SEPERATE RECEIVER CLOCKS FOR GPS/GLONASS
CC               27-MAY-09 : HB: PARFLG-ARRAY FOR PRIKIN EXTENDED
CC               29-MAY-09 : RD: INPUT CLOCKS ALSO FROM INPUT CLK RNX FILE
CC               04-JAN-10 : SL: HOI SCALING PARAMETERS ADDED
CC               27-AUG-10 : HB: MAXVAR REPLACED BY MAXELE
CC               08-SEP-10 : RD: MERGE SLR-TIME BIAS OPTION
CC               25-NOV-10 : MM: GNSS-SPECIFIC PARAMETERS
CC               20-DEC-10 : DT: ADD LOCQ(1)=28
CC               17-FEB-11 : SS: STRAMB(3) FOR SELECTION OF GNSS
CC               14-SEP-11 : LP: GIVE MXCPAR TO SR PRIGIM
CC               30-SEP-11 : LP: OUTPUT OF TECU FOR STOCH. IONO. PAR.
CC               03-NOV-11 : RD: TIMSTR->TIMST2 FOR STC/SAO (TYPE=11/12)
CC               17-NOV-11 : HB: Small correction in TIMST2 usage
CC               12-MAR-12 : HB: LAYOUT UNIFICATION FOR STOCHASTIC PARAMETERS
CC               14-MAR-12 : HB: TYPO CORRECTED SIGNULAR=>SINGULAR
CC               26-MAR-12 : RD: REMOVE UNSUSED VARIABLES
CC               28-MAR-12 : RD: USE SVNSYS AS MODULE NOW
CC               25-SEP-12 : SS: COSMETIC CHANGE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: LFN002, LFNPRT, LFNERR
      USE m_global, ONLY: g_svnsys,g_strsys3
      USE d_isbFil, ONLY: getIsb
      USE d_clkrnx, ONLY: t_clkhead,t_clkrec
      USE d_const,  ONLY: PI, WGTPHA, C, ARS, FACTEC
      USE d_satfil, ONLY: typemwtr
      USE d_rinex3, ONLY: t_gobsdef
      USE p_orbgen, ONLY: orbdsc
      USE p_gpsest, ONLY: t_optLoad
      USE f_ikf
      USE s_pritrp
      USE s_pridcb
      USE s_opnfil
      USE s_prigim
      USE s_covdia
      USE s_rdscra
      USE s_eccell
      USE s_err3d
      USE s_prikin
      USE s_opnerr
      USE s_maxtst
      USE s_priclk
      USE s_timst2
      USE s_exitrc
      USE s_radgms
      USE s_priami
      USE s_priref
      USE f_lengt1
      USE s_gtflna
      USE s_xyzell
      USE s_dcbcor
      USE s_defreq
      USE f_svnsys
      USE s_gtsensor
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IALBT , IAMB1(1),IAMB2(1),IARC, IAROLD, IAZI  ,
     1          ICAL  , ICAOLD, ICAR  , ICH1  , ICH2  , ICH3  , ICH4  ,
     2          ICH   , ICOELV, ICOR  , ICOR1 , IDAZI , IDEG  , IDOF  ,
     3          IDZEN , IELE  , IEPO  , IEXTRA, IF    , IFIL  , IFOLD ,
     4          IFRC  , IFROLD, IFRQ  , IGROLD, IGRP  , IGRP1 , II    ,
     5          IK    , ILAT  , IMIOLD, IMOD  , IMTOLD, IMTYP , ISYS  ,
     6          IMTYPA, INUL  , INUM  , IOFOLD, IOFR  , IOFR1 , IOS   ,
     7          IOSTAT, IP    , IP1   , IP1O  , IP2   , IP2O  , IPAMB ,
     8          IPAR1 , IPART , IPCOE , IPCOR , IPOFF , IPOS  , IPPRT ,
     9          IPSET , IRAO  , IRAOLD, IRAUX2, IRC   , IRCMAX,
     1          IREF  , IREQ  , IRSW  , ISAOLD, ISAT  , ISC   , ISPOLD,
     2          ISPV  , ISTA  , ISTAT , ISTC  , ITIM  , ITROPO, ITYP  ,
     3          IZEN  , IZEOLD, JJ    , JNUM  , JTYP  , K     , L1    ,
     4          L1O   , L2    , L2O   , LFNAUX, M     , MAXFRS, MAXELE,
     5          MAXMEA, MAXTXT, MAXTYP, MELWUB, MNUM  , MODSTC, MODTYP,
     6          MXCAMB, MXCFRQ, MXCLCQ, MXCSAT, MXCSTC, MXCVAR, MXLTYP,
     7          N     , NAMB  , NAZI  , NCLKSA, NCLKST, NFTOT , NNUM  ,
     8          NOBS  , NPAELI, NPAR  , NPARMS, NPARN , NPELI , NREF  ,
     9          NSTAT , NUMSAT, NUSA  , NZEN  , NOSOL , IELIM , IACST ,
     1          IFRCHP, ISTA0 , ITYPHP, NALLSAT,FRQNUM, RNXCLK,
     2          IWL   , SATSPEC, OLDSTA
C
      REAL*8    AELL  , ALPHA , BELL  , COSPHI, DH    , DLON  ,
     1          DLONR , DPHI  , DSEC  , HH    , HHO   , P3    , P3O   ,
     2          PHI   , RADIUS, RMS   , RMS1  , RMS2  , SCAALB, SCACEN,
     3          SCAHIL, SCAPOT, SCELL , SECIPL, SIGAPR, SIGHGT, SIGLON,
     4          SIGPHI, TOBS  , XCHI  , XL3   , XL3O  , XLONG , XXX1  ,
     5          XXX2  , ZDM1  , ZDM2  , ZENMAX, TIMPAR, TECU  , EPOTIME
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MXLTYP=30,MAXMEA=3,MAXELE=15)
      PARAMETER (MAXTXT=30)
C
      TYPE(t_clkhead) CLKHED
      TYPE(t_clkrec)  CLKREC
      TYPE(t_optLoad), DIMENSION(:):: opLoad
      TYPE(t_gobsdef) GOBSDEF
C
      CHARACTER*512 ANTTXT,ANTRMS,ANTNAD
      CHARACTER*132 TITLES(2),POLTXT,OFFTXT
      CHARACTER*48  STRSIG
      CHARACTER*40  TIMSTR2
      CHARACTER*32  FILAUX
      CHARACTER*132 TXTSTRG
      CHARACTER*32  FILSTRG
      CHARACTER*19  TIMSTRG
      CHARACTER*51  TXTTYP(MAXTXT)
      CHARACTER*16  STNAME(*),NAMGIM(*),STHELP
      CHARACTER*20  RECTYP(2,*),ANTCAL(2,*),ANTRAO(2,*)
      CHARACTER*13  UNITTXT(3)
      CHARACTER*12  ELITXT(4)
      CHARACTER*10  PAR(6),CTEXT
      CHARACTER*9   NULTXT(2)
      CHARACTER*8   MTYP(9),ELEUNI(MAXELE)
      CHARACTER*8   PSOBS
      CHARACTER*6   MXNFRQ,MXNLCQ,MXNSAT,MXNAMB,MXNSTC,MXNTYP,MXNVAR
      CHARACTER*6   TYPTXT(3)
      CHARACTER*5   ELETYP(MAXELE), RSWSYS(MAXELE-6)
      CHARACTER*3   FRCTXT(6),GRDTYP(3,3)
      CHARACTER*3   HILCHR(3,2)
      CHARACTER*2   ERPTXT(5),FRQTXT(4)
      CHARACTER*1   VL,VP,VLO,VPO
      CHARACTER*1   CORTXT(3),CO2TXT(3)
      CHARACTER*4   empiri
C
      REAL*8 XSTAT(3,*),XSTELL(3,*),XXX(*),ANOR(*),DXELL(3),DRELL(3)
      REAL*8 XXX0(*),XAPR0(3),TIMISB(3,*)
      REAL*8 XPART(3),XPELL(3),COVM1(3,3),COVM2(3,3),XSTECC(3,*)
      REAL*8 AMBIGU(MXCAMB,3,*)
      REAL*8 TIMSTC1(3,MXCSTC,MXCSAT,*),TIMOFF(2,*)
      REAL*8 TIMSTC2(3,MXCSTC,MXCSAT,*),SCASTC(*)
      REAL*8 DIAG(3),DIAG2(2)
      REAL*8 COVM3(3,3),REULER(3),EPOGIM(2,*),POLGIM(3,*)
      REAL*8 SCAGIM(*),INFGIM(2,*)
      REAL*8 ATMP(6),XTMP(3),NADMAX,NAMAX,RIDZEN,ZENJJ
      REAL*8 RAPZENMAX,FREQ(2,20)
      REAL*8 TRPLMS(2,*),CLFRTO(2,*),TIMREF(*)
C
      INTEGER*4 LOCQ(MXCLCQ,*),LCQ2(MXCLCQ,3)
      INTEGER*4 STRAMB(*),NFRFIL(*),ICARR(MXCFRQ,*)
      INTEGER*4 STANUM(*),ICENTR(*),NUMOBS(MXCFRQ,*)
      INTEGER*4 NFROBS(MAXMEA,MAXFRS)
      INTEGER*4 MEATYP(*),PRIOPT(*),NMTOBS(MAXMEA)
      INTEGER*4 HLP1,HLP2,NUMCAL(2,*),PRNCAL(*),OPTGIM(*)
      INTEGER*4 NUMAMB(*),AMBIEP(MXCAMB,*),AMBWLF(MXCAMB,2,*)
      INTEGER*4 AMBSAT(MXCAMB,*),AMBCLS(MXCAMB,3,*)
      INTEGER*4 OPTELI(*),PARLST(5,*),NPATOT(5)
      INTEGER*4 NUMRAO(2,*),PRNRAO(*),ITRGRD(*),NEPOBS(3)
      INTEGER*4 STFIL(2,*),NDIFF(*),NSAMPL(3),IDELTT(*)
      INTEGER*4 PARFLG(*),NADIGN,ALLSATNUM(*),MXCPAR
      INTEGER*4 USEGEOS
C
      LOGICAL SCRATCH
C
      INCLUDE 'COMFREQ.inc'
C
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMSTC/MXCSTC,MXNSTC
      COMMON/MCMVAR/MXCVAR,MXNVAR
C
      DATA MTYP/'PHASE','CODE','RANGE',' ',' ',' ',' ','DTEC','WUEBB'/
      DATA PSOBS/'PSEUDO'/
      DATA ELITXT/' ','(BEFORE INV)','(AFTER INV)',
     1            '(EPOCH-WISE)'/
      DATA ELETYP/'A','E','I','NODE','PER','U0','D0','Y0','X0',
     1                                          'DC','YC','XC',
     2                                          'DS','YS','XS'/

      DATA RSWSYS/'R0','S0','W0', 'RC','SC','WC', 'RS','SS','WS'/

      DATA ELEUNI/'M',' ','"','"','"','"',9*'M/S**2'/
      DATA FRCTXT/'rad','alo','out',
     1            'Sun','y  ','x  '/
      DATA TYPTXT/'epoch ','const ','linear'/
      DATA UNITTXT/'*1.E-5 M/S**2','*1.E-8 M/S**2','*1.E-8 M/S**2'/
      DATA CO2TXT/'N','E','U'/
      DATA CORTXT/'X','Y','Z'/
      DATA ERPTXT/'X ','Y ','DT','DE','DP'/
      DATA NULTXT/'        ','SINGULAR'/
      DATA FRQTXT/'L1','L2','LC','L4'/
      DATA (GRDTYP(1,II),II=1,3)/'ALL','   ','   '/
      DATA (GRDTYP(2,II),II=1,3)/'UP ','N/E','   '/
      DATA (GRDTYP(3,II),II=1,3)/'U  ','N  ','E  '/
C
      DATA TXTTYP/
     1    ' STATION COORDINATES                               ',
     2    ' RECEIVER CLOCK BIASES / TIME BIASES               ',
     3    ' ORBITAL ELEMENTS                                  ',
     4    ' AMBIGUITIES                                       ',
     5    ' RECEIVER ANTENNA OFFSETS                          ',
     6    ' SITE-SPECIFIC TROPOSPHERE PARAMETERS              ',
     7    ' LOCAL IONOSPHERE MODEL PARAMETERS                 ',
     8    ' DIFFERENTIAL CODE BIASES                          ',
     9    ' LOCAL TROPOSPHERE MODEL PARAMETERS                ',
     .    ' EARTH ROTATION PARAMETERS                         ',
     1    ' STOCHASTIC ORBIT PARAMETERS                       ',
     2    ' SATELLITE ANTENNA OFFSETS                         ',
     3    ' EARTH POTENTIAL PARAMETERS                        ',
     4    ' RESONANCE TERMS                                   ',
     5    ' ALBEDO PARAMETERS                                 ',
     6    ' CENTER OF MASS                                    ',
     7    ' STOCHASTIC IONOSPHERE PARAMETERS                  ',
     8    ' RECEIVER ANTENNA PHASE CENTER VARIATIONS          ',
     9    ' GLOBAL IONOSPHERE MODEL PARAMETERS                ',
     .    ' STATION VELOCITIES                                ',
     1    ' KINEMATIC COORDINATES                             ',
     2    ' SCALING FACTORS FOR VIENNA GRID FILES             ',
     3    ' EPOCH WISE STATION CLOCKS                         ',
     4    ' EPOCH WISE SATELLITE CLOCKS                       ',
     5    ' SATELLITE ANTENNA PHASE CENTER VARIATIONS         ',
     6    ' RANGE BIASES                                      ',
     7    ' HOI SCALING PARAMETERS                            ',
     8    ' HELMERT PARAMETERS                                ',
     9    ' NOT USED                                          ',
     .    ' GNSS-SPECIFIC PARAMETERS                          '/
C
      MXNTYP='MAXTYP'
      CALL MAXTST(1,'PRIEST',MXNTYP,MXLTYP,MAXTYP,IRCMAX)
      IF (IRCMAX.NE.0) CALL EXITRC(2)
      CALL MAXTST(1,'PRIEST','MAXTXT',MAXTXT,MAXTYP,IRCMAX)
      IF (IRCMAX.NE.0) CALL EXITRC(2)
C
      IFOLD=0
      IMIOLD=0
      IMTOLD=0
      IAROLD=0
      IGROLD=0
      IOFOLD=0
      ICAOLD=0
      IZEOLD=0
      IRAOLD=0
      IFROLD=0
      ISPOLD=0
      IPSET=0
      IPCOR=0
      IPAMB=0
      POLTXT=' '
      ANTTXT=' '
      ANTRMS=' '
      ANTNAD=' '
      PAR(1)='X         '
      PAR(2)='Y         '
      PAR(3)='Z         '
      PAR(4)='LATITUDE  '
      PAR(5)='LONGITUDE '
      PAR(6)='HEIGHT    '
      HILCHR(1,1)=' R '
      HILCHR(2,1)=' S '
      HILCHR(3,1)=' W '
      HILCHR(1,2)='CON'
      HILCHR(2,2)='COS'
      HILCHR(3,2)='SIN'
C
C GENERAL FORMATS
C ---------------
1     FORMAT(//,A,/,A,/,' ',131('-'),//)
2     FORMAT(A)
3     FORMAT(' ')
C
C CHECK FOR A SCRATCH FILE WITH RESULTS FROM SR RESEPO
C ----------------------------------------------------
      LFNAUX=LFN002
      IF (IRAUX2.EQ.0) THEN
        CALL GTFLNA(1,'AUXFIL1',FILAUX,IRC)
        CALL OPNFIL(LFNAUX,FILAUX,'UNKNOWN','UNFORMATTED',
     1                  ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNAUX,IOSTAT,FILAUX,'PRIEST')
      ENDIF
C
C Get system of dynamic orbit parameters
C --------------------------------------
      DO 10 I=1,orbdsc%nlin
        IF (orbdsc%orbmod(i)(1:7)=='EMPIRI:') THEN
           empiri = orbdsc%orbmod(i)(9:12)
        ENDIF
10    CONTINUE
      IF (empiri=='RSW') ELETYP(7)=RSWSYS(1)
      IF (empiri=='RSW ' .OR. empiri=='DRSW') THEN
         ELETYP(8:15)=RSWSYS(2:9)
      ENDIF
C
C PRINT TITLE LINES
C -----------------
      WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                TITLES(2)(1:LENGT1(TITLES(2)))
C
      IF (IPART.EQ.1) WRITE(LFNPRT,"(' 13. RESULTS (PART 1)')")
      IF (IPART.EQ.2) WRITE(LFNPRT,"(' 14. RESULTS (PART 2)')")
C
      WRITE(LFNPRT,"(
     1     ' --------------------'
     2  ,/,' '
     3  ,/,' NUMBER OF PARAMETERS (PART ',I1,'):'
     4  ,/,' -----------------------------'
     5  ,/,' '
     6  ,/,' PARAMETER TYPE                                     '
     6    ,'#PARAMETERS   #PRE-ELIMINATED        #SET-UP  '
     6    ,'#NO-OBS  #REF     #SINGULAR'
     7  ,/,1X,131('-'),/,1X)") IPART
C
C NUMBER OF PARAMETERS FOR DIFFERENT TYPES
C ----------------------------------------
      NPELI=0
      DO 20 IPOS=1,5
        NPATOT(IPOS)=0
20    CONTINUE
C
      DO 25  ITYP=1,MAXTYP
        DO 27 IPOS=1,5
          NPATOT(IPOS)=NPATOT(IPOS)+PARLST(IPOS,ITYP)
27      CONTINUE
        IELIM=OPTELI(ITYP)
        IF (IELIM.EQ.2.AND.NOSOL.EQ.1) IELIM=0
        IF (IELIM.GT.0) THEN
          NPELI=NPELI+PARLST(1,ITYP)
          NPAELI=PARLST(1,ITYP)
        ELSE
          NPAELI=0
        ENDIF
C
        IF (PARLST(2,ITYP).GT.0) THEN
          WRITE(TXTSTRG(1:51),'(A51)') TXTTYP(ITYP)
          WRITE(TXTSTRG(52:132),21) PARLST(1,ITYP),NPAELI,
     1                              ELITXT(IELIM+1),
     2                             (PARLST(IPOS,ITYP),IPOS=2,5)
21        FORMAT(I8,I12,2X,A12,4I9)
          WRITE(LFNPRT,2) TXTSTRG(1:LENGT1(TXTSTRG))
        ENDIF
C
25    CONTINUE
C
      WRITE(LFNPRT,"(
     1     ' '
     2  ,/,1X,131('-')
     3  ,/,' TOTAL NUMBER OF PARAMETERS',24X,I8,I12,14X,4I9
     4  ,/,1X,131('-')
     5  ,/,' '
     6  ,/,' '
     7  ,/,' NUMBER OF OBSERVATIONS (PART ',I1,'):'
     8  ,/,' -------------------------------'
     9  ,/,' '
     .  ,/,' TYPE         FREQUENCY         FILE/PAR          '
     .    ,'#OBSERVATIONS'
     1  ,/,1X,131('-')
     2  ,/,1X)") NPATOT(1),NPELI,(NPATOT(IPOS),IPOS=2,5),IPART
C
C NUMBER OF OBSERVATIONS
C ----------------------
      DO 40 IMTYP=1,MAXMEA
        NMTOBS(IMTYP)=0
        DO 35 IFRQ=1,MAXFRS
          NFROBS(IMTYP,IFRQ)=0
35      CONTINUE
40    CONTINUE
C
      DO 50 IF=1,NFTOT
        IMTYP=MEATYP(IF)
        DO 45 IFRQ=1,NFRFIL(IF)
          ICAR=ICARR(IFRQ,IF)
          NMTOBS(IMTYP)=NMTOBS(IMTYP)+NUMOBS(IFRQ,IF)
          NFROBS(IMTYP,ICAR)=NFROBS(IMTYP,ICAR)+NUMOBS(IFRQ,IF)
          IF(PRIOPT(9).EQ.1.AND.NUMOBS(IFRQ,IF).NE.0) THEN
            IF (MELWUB.EQ.0) THEN
              IMTYPA=IMTYP
            ELSEIF (MELWUB.EQ.1) THEN
              IMTYPA=9
            ELSE
              IMTYPA=8
            ENDIF
            WRITE(LFNPRT,41) MTYP(IMTYPA),ICAR,IF,NUMOBS(IFRQ,IF)
41          FORMAT(1X,A8,8X,'L',I1,11X,I6,15X,I8)
          ENDIF
45      CONTINUE
50    CONTINUE
      IF(PRIOPT(9).EQ.1) WRITE(LFNPRT,51)
51      FORMAT(/,' ',131('-'),/)
C
      DO 60 IMTYP=1,MAXMEA
        DO 55 IFRQ=1,MAXFRS
          IF(NFROBS(IMTYP,IFRQ).NE.0) THEN
            IF (MELWUB.EQ.0) THEN
              IMTYPA=IMTYP
            ELSEIF (MELWUB.EQ.1) THEN
              IMTYPA=9
            ELSE
              IMTYPA=8
            ENDIF
            WRITE(LFNPRT,52) MTYP(IMTYPA),IFRQ,NFROBS(IMTYP,IFRQ)
52          FORMAT(1X,A8,8X,'L',I1,14X,'ALL',15X,I8)
          ENDIF
55      CONTINUE
60    CONTINUE
C
      IF(NMTOBS(1).NE.0 .AND. NMTOBS(2).NE.0 .AND. NMTOBS(3).NE.0) THEN
        WRITE(LFNPRT,51)
        DO 62 IMTYP=1,MAXMEA
          IF (MELWUB.EQ.0) THEN
            IMTYPA=IMTYP
          ELSEIF (MELWUB.EQ.1) THEN
            IMTYPA=9
          ELSE
            IMTYPA=8
          ENDIF
          WRITE(LFNPRT,61) MTYP(IMTYPA),NMTOBS(IMTYP)
61        FORMAT(1X,A8,7X,'ALL',14X,'ALL',15X,I8)
62      CONTINUE
      ENDIF
C
C COUNT FOR PSEUDO-OBSERVATIONS (CURRENTLY ONLY REFERENCE AMBIGUITIES)
C --------------------------------------------------------------------
      IF (IACST >0) THEN
        WRITE(LFNPRT,"(
     1    1X,A8,24X,'ALL',15X,I8,10X
     2    ,'(CURRENTLY ONLY REFERENCE AMBIGUITIES)')")PSOBS,IACST
      ENDIF
C
      WRITE(LFNPRT,"(
     1     ' '
     2  ,/,1X,131('-')
     3  ,/,' TOTAL NUMBER OF OBSERVATIONS',22X,I8
     4  ,/,1X,131('-'))") NMTOBS(1)+NMTOBS(2)+NMTOBS(3)+IACST
C
C STOP HERE IF NO SOLUTION IS COMPUTED
C ------------------------------------
      IF (NOSOL.EQ.1) THEN
        WRITE(LFNPRT,'(/,1X,A,/)') 'SOLUTION SKIPPED ...'
        RETURN
      ENDIF
C
C ADJUSTMENT-RELATED INFORMATION
C ------------------------------
      WRITE(LFNPRT,"(
     1     ' '
     2  ,/,' '
     3  ,/,' A POSTERIORI SIGMA OF UNIT WEIGHT (PART ',I1,'):'
     4  ,/,' ------------------------------------------')") IPART
C
C POST-FIT SIGMA OF UNIT WEIGHT
      IF (ICOELV.EQ.0) THEN
        STRSIG='(SIGMA OF ONE-WAY L1 PHASE OBSERVABLE)'
      ELSE
        STRSIG='(SIGMA OF ONE-WAY L1 PHASE OBSERVABLE AT ZENITH)'
      ENDIF
      IF (WGTPHA.NE.1.D0) STRSIG=' '
      WRITE(LFNPRT,66) RMS,STRSIG(1:LENGT1(STRSIG))
66    FORMAT(/,' A POSTERIORI SIGMA OF UNIT WEIGHT  : ',F9.4,' M',
     1  2X,A,/)
C
C DEGREE OF FREEDOM (DOF)
      IDOF=NOBS-NPARMS
      WRITE(LFNPRT,67) IDOF
67    FORMAT(' DEGREE OF FREEDOM (DOF)            : ',I9)
C
C CHI**2/DOF
      XCHI=(RMS/SIGAPR)**2
      WRITE(LFNPRT,68) XCHI
68    FORMAT(' CHI**2/DOF                         : ',F9.2)
C
C STATION COORDINATES
C -------------------
      ITYP = 1
C
C PRINT TITLE
      IF (PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1.AND.
     1    PRIOPT(15).EQ.0) THEN
        CALL GTFLNA(0,'COORDRS',FILSTRG,IRC)
        IF(TRIM(FILSTRG).EQ.'') FILSTRG = '(NOT SAVED)'
C
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' STATION COORDINATES:',23X,A
     2    ,/,' -------------------'
     3    ,/,' '
     4    ,/,' NUM  STATION NAME     PARAMETER    A PRIORI VALUE       '
     4      ,'NEW VALUE     NEW- A PRIORI  RMS ERROR   '
     4      ,'3-D ELLIPSOID       2-D ELLIPSE'
     5    ,/,1X,131('-'))") TRIM(FILSTRG)
C
C LOOP OVER ALL PARAMETERS
        DO IP = 1, NPAR
          IF(LOCQ(1,IP).EQ.1) THEN
            ISTA=LOCQ(2,IP)
            ICOR=LOCQ(3,IP)
            IF(ICOR.NE.1) CYCLE
C
C FIND ALL ECCENTERS BELONGING TO STATIONS "ISTA", TOO
            DO 150 ISTAT=1,NSTAT
              IF(ICENTR(ISTAT).NE.ISTA) GOTO 150
              IF(ICENTR(ISTAT).EQ.ISTAT) THEN
                CTEXT='          '
              ELSE
                CTEXT='(ECCENTER)'
              ENDIF
C
C X,Y,Z
              DO 120 I=1,3
                II=IKF(I+IP-1,I+IP-1)
                RMS1=RMS*DSQRT(ANOR(II))
                INUL=1
                IF (ANOR(II).EQ.0D0) INUL=2
                XPART(I)=XSTAT(I,ISTA)+XXX(IP-1+I)+XSTECC(I,ISTAT)
                IF(I.EQ.1) THEN
                  WRITE(LFNPRT,101) STANUM(ISTAT),STNAME(ISTAT),PAR(I),
     1                        XSTAT(I,ISTAT),XPART(I),
     2                        XXX(IP-1+I),RMS1,NULTXT(INUL)
101               FORMAT(/,I4,2X,A16,1X,A10,F16.4,F19.4,2F12.4,6X,A)
                ELSE IF(I.EQ.2) THEN
                  WRITE(LFNPRT,102)CTEXT,PAR(I),XSTAT(I,ISTAT),XPART(I),
     1                        XXX(IP-1+I),RMS1,NULTXT(INUL)
102               FORMAT(6X,A10,7X,A10,F16.4,F19.4,2F12.4,6X,A)
                ELSE
                  WRITE(LFNPRT,103) PAR(I),XSTAT(I,ISTAT),XPART(I),
     1                        XXX(IP-1+I),RMS1,NULTXT(INUL)
103               FORMAT(23X,A10,F16.4,F19.4,2F12.4,6X,A)
                ENDIF
                DO 110 K=1,3
                  IK=IKF(I+IP-1,K+IP-1)
                  COVM1(I,K)=RMS**2*ANOR(IK)
                  COVM3(I,K)=ANOR(IK)
110             CONTINUE
120           CONTINUE
C
C ELLIPSOIDAL COORDINATES OF RECEIVER (NEW)
              CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,XPART,XPELL)
              PHI  =XPELL(1)
              XLONG=XPELL(2)
              HH   =XPELL(3)
              CALL ERR3D(PHI,XLONG,HH,AELL,BELL,-1,COVM1,COVM2)
C
C DIAGONAL TRANSFORMATION OF COVARIANCE MATRIX
              CALL COVDIA(AELL,BELL,DXELL,DRELL,SCELL,XPART,COVM3,
     1                    RMS,DIAG,REULER,DIAG2,ALPHA)
              CALL RADGMS(1,XSTELL(1,ISTAT),VPO,IP1O,IP2O,P3O)
              CALL RADGMS(1,XSTELL(2,ISTAT),VLO,L1O,L2O,XL3O)
              CALL RADGMS(1,PHI,VP,IP1,IP2,P3)
              CALL RADGMS(1,XLONG,VL,L1,L2,XL3)
              HHO=XSTELL(3,ISTAT)
              RADIUS=DSQRT(XPART(1)**2+XPART(2)**2+XPART(3)**2)
              COSPHI=DCOS(PHI)
              SIGPHI=RADIUS*DSQRT(COVM2(1,1))
              SIGLON=RADIUS*COSPHI*DSQRT(COVM2(2,2))
              SIGHGT=DSQRT(COVM2(3,3))
              DPHI=RADIUS*(PHI-XSTELL(1,ISTAT))
C CHECK MODULO 2*PI
              DLONR=XLONG-XSTELL(2,ISTAT)
              IF (DLONR.GT. PI) DLONR=DLONR-2*PI
              IF (DLONR.LT.-PI) DLONR=DLONR+2*PI
              DLON=RADIUS*COSPHI*DLONR
C
              DH=HH-HHO
              WRITE(LFNPRT,121)PAR(6),HHO,HH,DH,SIGHGT,DIAG(3),REULER(2)
121           FORMAT(/,23X,A10,F16.4,F19.4,3F12.4,F7.1)
              WRITE(LFNPRT,122) PAR(4),VPO,IP1O,IP2O,P3O ,
     1                    VP ,IP1 ,IP2 ,P3  ,DPHI,SIGPHI,
     2                    DIAG(1),REULER(1),DIAG2(1),ALPHA
              WRITE(LFNPRT,124) PAR(5),VLO,L1O ,L2O ,XL3O,
     1                    VL ,L1  ,L2  ,XL3 ,DLON,SIGLON,
     2                    DIAG(2),REULER(3),DIAG2(2)
122           FORMAT(23X,A10,1X,A1,2I3,F10.6,2X,A1,2I3,F10.6,F10.4,
     1                    2F12.4,F7.1,F12.4,F7.1)
124           FORMAT(23X,A10,1X,A1,2I3,F10.6,2X,A1,2I3,F10.6,F10.4,
     1                    2F12.4,F7.1,F12.4)
150         CONTINUE
          ENDIF
        ENDDO
C
      ENDIF
C
C GNSS-SPECIFIC PARAMETERS
C ------------------------
      ITYP = 30
C
C PRINT TITLE
      IF (PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1.AND.
     1    PRIOPT(15).EQ.0) THEN
C
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' GNSS-SPECIFIC PARAMETERS:'
     2    ,/,' ------------------------'
     3    ,/,' '
     4    ,/,' NUM  STATION NAME     SYS CMP    A PRIORI VALUE       '
     4      ,'NEW VALUE     NEW- A PRIORI  RMS ERROR   '
     5    ,/,1X,131('-'))")
C
C LOOP OVER ALL PARAMETERS
        OLDSTA = 0
        DO IP = 1, NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            ISTA=LOCQ(2,IP)
            ICOR=LOCQ(3,IP)
            ISYS=LOCQ(4,IP)
C
C STATION TRANSLATIONS
            IF(ICOR.EQ.1) THEN
              OLDSTA = ISTA
C
C X,Y,Z
              DO I=1,3
                II=IKF(I+IP-1,I+IP-1)
                RMS1=RMS*DSQRT(ANOR(II))
                INUL=1
                IF (ANOR(II).EQ.0D0) INUL=2
                XPART(I)=XXX(IP-1+I)
                IF(I.EQ.1) THEN
                  WRITE(LFNPRT,3001) ISTA,STNAME(ISTA),g_strSys3(ISYS),
     1                               CORTXT(I),0.d0,XPART(I),
     2                               XXX(IP-1+I),RMS1
3001              FORMAT(/,I4,2X,A16,1X,A3,2X,A1,1X,F16.4,F19.4,2F12.4)
                ELSE
                  WRITE(LFNPRT,3003) CORTXT(I),0.d0,
     1                               XPART(I),XXX(IP-1+I),RMS1
3003              FORMAT(28X,A1,1X,F16.4,F19.4,2F12.4)
                ENDIF
                DO K=1,3
                  IK=IKF(I+IP-1,K+IP-1)
                  COVM1(I,K)=RMS**2*ANOR(IK)
                  COVM3(I,K)=ANOR(IK)
                ENDDO
              ENDDO
C
C N, E, U
              CALL ECCELL(XSTELL(1,ISTA),XPART,XPELL)
              PHI  =XSTELL(1,ISTA)
              XLONG=XSTELL(2,ISTA)
              HH   =XSTELL(3,ISTA)
              CALL ERR3D(PHI,XLONG,HH,AELL,BELL,-1,COVM1,COVM2)
              RADIUS=DSQRT(XSTAT(1,ISTA)**2+XSTAT(2,ISTA)**2+
     1                     XSTAT(3,ISTA)**2)
              COSPHI=DCOS(PHI)
              SIGPHI=RADIUS*DSQRT(COVM2(1,1))
              SIGLON=RADIUS*COSPHI*DSQRT(COVM2(2,2))
              SIGHGT=DSQRT(COVM2(3,3))

              WRITE(LFNPRT,'()')
              WRITE(LFNPRT,3003) CO2TXT(3),0.d0,
     1                           XPELL(3),XPELL(3),SIGHGT
              WRITE(LFNPRT,3003) CO2TXT(1),0.d0,
     1                           XPELL(1),XPELL(1),SIGPHI
              WRITE(LFNPRT,3003) CO2TXT(2),0.d0,
     1                           XPELL(2),XPELL(2),SIGLON
C
C TROPOSPHERE BIASES
            ELSEIF(ICOR.EQ.4) THEN
              RMS1=RMS*DSQRT(ANOR(IKF(IP,IP)))
              IF (OLDSTA.EQ.ISTA) THEN
                WRITE(LFNPRT,'()')
                WRITE(LFNPRT,3003) "T",0.d0,XXX(IP),XXX(IP),RMS1

              ELSE
                WRITE(LFNPRT,3001) ISTA,STNAME(ISTA),g_strSys3(ISYS),
     1                             "TRP",0.d0,XXX(IP),XXX(IP),RMS1
              ENDIF
            ENDIF
          ENDIF
        ENDDO
C
      ENDIF
C
C CLOCK-PARAMETERS
C ----------------
      ITYP = 2
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' RECEIVER CLOCKS / TIME BIASES:'
     2    ,/,' -----------------------------'
     3    ,/,' '
     4    ,/,' REQUEST  STATION NAME        OFFSET (USEC)  RMS (NSEC)'
     5    ,/,1X,131('-')
     6    ,/,1X)")
C
C LOOP OVER ALL PARAMETERS
        ISTA0=0
        DO IP = 1, NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            ISTA=LOCQ(2,IP)
            IREQ=LOCQ(3,IP)
C
C ADD THE FIXED GPS SATELLITES/FREQUENCIES
            IF (ISTA.NE.ISTA0) THEN
              IF (LOCQ(6,IP).EQ.1) THEN
                WRITE(LFNPRT,201) IREQ,STNAME(ISTA),0d0,0D0,'FRQ'
201             FORMAT(I6,4X,A16,F14.6,8X,'---',3X,F14.6,8X,'---',
     1                 10X,A,' GPS')
              ELSE IF (LOCQ(7,IP).EQ.1.AND.
     1                 LOCQ(6,IP).EQ.2.AND.LOCQ(4,IP).GE.100) THEN
                WRITE(LFNPRT,201) IREQ,STNAME(ISTA),0d0,0d0,'SAT'
              ELSE IF (LOCQ(6,IP).EQ.4) THEN
                IF (ISTA0.NE.0) WRITE(LFNPRT,'(1X)')
                FREQ=0D0
              ENDIF
            ENDIF
C
C PRINT THE ESTIMATED RESULTS
            RMS1=1.0D9*RMS*DSQRT(ANOR(IP*(IP-1)/2+IP))/C
            XXX1=1.0D6*XXX(IP)/C
            TIMPAR=(CLFRTO(1,LOCQ(3,IP))+CLFRTO(2,LOCQ(3,IP)))/2d0
C STATION-SPECIFIC
            IF (LOCQ(6,IP).EQ.0.AND.LOCQ(7,IP).EQ.0) THEN
              WRITE(LFNPRT,202) IREQ,STNAME(ISTA),XXX1,RMS1
202           FORMAT(I6,4X,A16,F14.6,F11.3)
C SLR TIME_BIAS
            ELSE IF (LOCQ(6,IP).EQ.0) THEN
              WRITE(LFNPRT,207) IREQ,STNAME(ISTA),XXX1,RMS1,LOCQ(7,IP)
207           FORMAT(I6,4X,A16,F14.6,F11.3,'SAT ',I3)
C FREQUENCY-SPECIFIC
            ELSE IF(LOCQ(6,IP).EQ.1) THEN
              DO ISAT = 1,NALLSAT
                IF (.NOT. SVNSYS(1,1,(/ ALLSATNUM(ISAT) /)) ) CYCLE
                CALL GTSENSOR(ALLSATNUM(ISAT),TIMPAR,
     1                        type1=typeMWTR,IFRQ=FRQNUM)
                IF (LOCQ(4,IP).EQ.FRQNUM) THEN
                  CALL DCBCOR(2,0,ALLSATNUM(ISAT),STNAME(LOCQ(2,IP)),'',
     1                        0,5,TIMPAR,XXX2)
                  XXX2=XXX2/1000d0
                  WRITE(LFNPRT,203) IREQ,STNAME(ISTA),XXX2,XXX1,
     1                              XXX1+XXX2,RMS1,LOCQ(4,IP)
203               FORMAT(I6,4X,A16,3F14.6,F11.3,10X,'FRQ ',I3)
                  EXIT
                ENDIF
              ENDDO
C SATELLITE-SPECIFIC
            ELSE IF(LOCQ(6,IP).EQ.2) THEN
              CALL DCBCOR(2,0,LOCQ(4,IP),STNAME(LOCQ(2,IP)),'',0,
     1                    5,TIMPAR,XXX2)
              XXX2=XXX2/1000
              WRITE(LFNPRT,204) IREQ,STNAME(ISTA),XXX2,XXX1,
     1                          XXX1+XXX2,RMS1,LOCQ(4,IP)
204           FORMAT(I6,4X,A16,3F14.6,F11.3,10X,'SAT ',I3)
C FREQUENCY-SPECIFIC, POLYNOMIAL
            ELSE IF(LOCQ(6,IP).EQ.4) THEN
              WRITE(LFNPRT,205) IREQ,STNAME(ISTA),XXX1,RMS1,LOCQ(4,IP)-1
205           FORMAT(I6,4X,A16,14X,F14.6,14X,F11.3,10X,
     1               'FRQ polynomial n = ',I3)
              IF (LOCQ(4,IP).EQ.LOCQ(5,IP)) THEN
                WRITE(LFNPRT,201) IREQ,STNAME(ISTA),0d0,0d0,'FRQ'
              ENDIF
              DO II=-7,12
                FREQ(1,II+8)=FREQ(1,II+8)+
     1                        XXX1*(II-6)**(LOCQ(4,IP)-1)
                FREQ(2,II+8)=FREQ(2,II+8)+
     1                       (RMS1*(II-6)**(LOCQ(4,IP)-1))**2
                IF (LOCQ(4,IP).NE.LOCQ(5,IP)) CYCLE
C
                CALL DCBCOR(2,0,0,STNAME(LOCQ(2,IP)),'',II,
     1                      5,TIMPAR,XXX2)
                XXX2=XXX2/1000d0
                WRITE(LFNPRT,203) IREQ,STNAME(ISTA),XXX2,FREQ(1,II+8),
     1                       XXX2+FREQ(1,II+8),DSQRT(FREQ(2,II+8)),II
              ENDDO
C TIME-DPENDENT INTER-SYSTEM BIAS
            ELSE IF(LOCQ(6,IP).EQ.5) THEN
              CALL TIMST2(1,1,TIMISB(1,LOCQ(4,IP)),TIMSTRG)
              XXX2=getIsb(0,STNAME(ISTA),TIMISB(1,LOCQ(4,IP)),
     1                    locq(3,ip),irc)/1d3
              WRITE(LFNPRT,206) LOCQ(4,IP),STNAME(ISTA),XXX2,XXX1,
     1                    XXX2+XXX1,RMS1,TIMSTRG
206           FORMAT(I6,4X,A16,3F14.6,F11.3,10X,A)
            ENDIF
            ISTA0=ISTA
          ENDIF
        ENDDO
C
      ENDIF
C
C ORBIT PARAMETERS
C ----------------
      ITYP = 3
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        CALL GTFLNA(0,'ORBITRS',FILSTRG,IRC)
        IF(TRIM(FILSTRG).EQ.'') THEN
          CALL GTFLNA(0,'LEORBRS',FILSTRG,IRC)
          IF(TRIM(FILSTRG).EQ.'') FILSTRG = '(NOT SAVED)'
        ENDIF
C
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' ORBITAL ELEMENTS:',26X,A
     2    ,/,' ----------------'
     3    ,/,' '
     4    ,/,' ARC  SAT.  ELEMENT   IMPROVEMENT      RMS'
     5    ,/,1X,131('-'))") TRIM(FILSTRG)
C
C LOOP OVER ALL PARAMETERS
        DO IP = 1, NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            IARC=LOCQ(2,IP)
            ISAT=LOCQ(3,IP)
            IELE=LOCQ(4,IP)
            IF(IARC.NE.IAROLD) THEN
              IAROLD=IARC
              ISAOLD=0
            ENDIF
            IF(ISAT.NE.ISAOLD) WRITE(LFNPRT,3)
            ISAOLD=ISAT
            XXX1=XXX(IP)
            RMS1=RMS*DSQRT(ANOR(IP*(IP+1)/2))
            XXX2=XXX1/ARS
            RMS2=RMS1/ARS
C
C A
            IF(IELE.EQ.1)
     1        WRITE(LFNPRT,311) IARC,ISAT,ELETYP(IELE),XXX1,RMS1,
     2                          ELEUNI(IELE)
311           FORMAT(I3,I6,4X,A5,F14.2,F15.4,2X,A8)
C
C E
            IF(IELE.EQ.2)
     1        WRITE(LFNPRT,321) IARC,ISAT,ELETYP(IELE),XXX2,RMS2,
     2                          ELEUNI(IELE)
321           FORMAT(I3,I6,4X,A5,F14.7,D15.5,2X,A8)
C
C I,NODE,PER,U0
            IF(IELE.GE.3.AND.IELE.LE.6)
     1        WRITE(LFNPRT,331) IARC,ISAT,ELETYP(IELE),XXX1,RMS1,
     2                          ELEUNI(IELE)
331           FORMAT(I3,I6,4X,A5,F14.3,F15.5,2X,A8)
C
C DYNAMICAL ORBIT PARAMETERS
            IF(IELE.GE.7.AND.IELE.LE.15)
     1        WRITE(LFNPRT,341)IARC,ISAT,ELETYP(IELE),XXX1/1.D9,
     2                         RMS1/1.D9,ELEUNI(IELE)
341           FORMAT(I3,I6,4X,A5,D14.3,D15.5,2X,A8)
          ENDIF
        ENDDO
C
      ENDIF
C
C AMBIGUITY PARAMETERS (SR PRIAMI: ALL AMBIGUITIES IN ONE CALL)
C--------------------------------------------------------------
      ITYP = 4
C
      IF(PARLST(1,ITYP) .GT. 0  .AND.
     1            IPART .EQ. 1  .AND.
     2        STRAMB(1) .NE.-1  .AND.
     3        PRIOPT(16).EQ. 0) THEN
C
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1      ' AMBIGUITIES:'
     2   ,/,' -----------')")
C
        NAMB=NPAR-NPARN
        CALL PRIAMI(0,NAMB,NPARN,NREF,LOCQ,XXX,RMS,ANOR,IAMB1,
     1              IAMB2,NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,
     2              AMBCLS,ICARR,NDIFF)
C
      ENDIF
C
C RECEIVER ANTENNA OFFSETS
C ------------------------
      ITYP = 5
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        CALL GTFLNA(0,'PHASRSG',FILSTRG,IRC)
        IF(TRIM(FILSTRG).EQ.'')
     1    CALL GTFLNA(0,'PHASRSH',FILSTRG,IRC)
        IF(TRIM(FILSTRG).EQ.'') FILSTRG = '(NOT SAVED)'
C
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' RECEIVER ANTENNA OFFSETS:',18X,A
     2    ,/,' ------------------------'
     3    ,/,'                                             '
     3      ,' ANTENNA S/N                        OFFSET   RMS'
     4    ,/,' ANTENNA NAME           RECEIVER NAME        ',
     4       '  FROM    TO      FRQ.  SYS.  CRD.    (M)    (M)'
     5    ,/,1X,131('-'))") TRIM(FILSTRG)
C
C LOOP OVER ALL PARAMETERS
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            IRAO=LOCQ(2,IP)
            IFRQ=LOCQ(3,IP)
            ICOR=LOCQ(4,IP)
            RMS1=RMS*DSQRT(ANOR(IKF(IP,IP)))
            IF (IRAOLD.NE.IRAO .OR. IFROLD.NE.IFRQ) THEN
              WRITE(LFNPRT,501) (ANTRAO(JJ,IRAO),JJ=2,1,-1),
     1                    (NUMRAO(JJ,IRAO),JJ=1,2),FRQTXT(IFRQ),
     2                    G_STRSYS3(PRNRAO(IRAO)),
     3                    CO2TXT(ICOR),XXX(IP),RMS1
501           FORMAT(/,1X,A20,3X,A20,1X,2I7,4X,A2,4X,A3,4X,A1,F10.4,
     1               F8.4)
            ELSE
              WRITE(LFNPRT,502) CO2TXT(ICOR),XXX(IP),RMS1
502           FORMAT( 76X,                        A1,F10.4,F8.4)
            ENDIF
            IRAOLD=IRAO
            IFROLD=IFRQ
          ENDIF
        ENDDO
C
      ENDIF
C
C TROPOSPHERE PARAMETERS FOR INDIVIDUAL STATIONS
C ----------------------------------------------
      ITYP = 6
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1.AND.
     1   PRIOPT(14).EQ.0) THEN
C
C PRINT TITLE
        CALL GTFLNA(0,'TROPSAV',FILSTRG,IRC)
        IF(TRIM(FILSTRG).EQ.'') FILSTRG = '(NOT SAVED)'
C
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' SITE-SPECIFIC TROPOSPHERE PARAMETERS:',6X,A
     2    ,/,' ------------------------------------'
     3    ,/,1X)") TRIM(FILSTRG)
C
C LOOP OVER ALL PARAMETERS
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            CALL PRITRP(IPART ,NPAR  ,LOCQ  ,XXX   ,ANOR  ,RMS   ,
     1                  STNAME,XSTELL,IEXTRA,ITROPO,ITRGRD,ZENMAX,
     2                  IP    ,TRPLMS)
          ENDIF
        ENDDO
C
      ENDIF
C
C PRINT IONOSPHERIC POLYNOMIAL COEFFICIENTS
C -----------------------------------------
      ITYP = 7
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' LOCAL IONOSPHERE MODELS:'
     2    ,/,' -----------------------'
     3    ,/,'        POL.DEG. IN'
     4    ,/,' MODEL  TIME  LATIT.   COEFFICIENT       RMS'
     5    ,/,1X,131('-'))")
C
C LOOP OVER ALL PARAMETERS
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            IMOD=LOCQ(2,IP)
            ILAT=LOCQ(3,IP)
            ITIM=LOCQ(4,IP)
            IF(IMOD.NE.IMIOLD) WRITE(LFNPRT,3)
            IMIOLD=IMOD
            RMS1=RMS*DSQRT(DABS(ANOR(IP*(IP-1)/2+IP)))
            WRITE(LFNPRT,701) IMOD,ITIM,ILAT,XXX(IP),RMS1
701         FORMAT(I5,2I6,D16.3,D14.3)
          ENDIF
        ENDDO
C
      ENDIF
C
C DIFFERENTIAL CODE BIASES
C ------------------------
      ITYP = 8
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
C LOOP OVER ALL PARAMETERS
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            CALL PRIDCB(IPART ,LOCQ  ,XXX   ,ANOR  ,RMS   ,STNAME,
     1                  RECTYP,STFIL ,NDIFF ,NFTOT ,IP    )
          ENDIF
        ENDDO
C
      ENDIF
C
C LOCAL TROPOSPHERE MODELS
C ------------------------
      ITYP = 9
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' LOCAL TROPOSPHERE MODELS:'
     2    ,/,' ------------------------'
     3    ,/,'       POL.DEG.'
     4    ,/,' MODEL  HEIGHT   COEFFICIENT (M)  RMS (M)'
     5    ,/,1X,131('-'))")
C
C LOOP OVER ALL PARAMETERS
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            IMOD=LOCQ(2,IP)
            IDEG=LOCQ(3,IP)
            IF(IMTOLD.NE.IMOD) WRITE(LFNPRT,3)
            IMTOLD=IMOD
            RMS1=RMS*DSQRT(ANOR(IP*(IP+1)/2))
            WRITE(LFNPRT,901) IMOD,IDEG,XXX(IP),RMS1
901         FORMAT(I5,I7,F15.4,F13.4)
          ENDIF
        ENDDO
C
      ENDIF
C
C EARTH ROTATION PARAMETER
C ------------------------
      ITYP = 10
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' EARTH ROTATION PARAMETERS:'
     2    ,/,' -------------------------'
     3    ,/,' '
     4    ,/,' CRD. REQ.     ("")        RMS        ("")/DAY    '
     4      ,'  RMS      ("")/DAY**2     RMS     ("")/DAY**3   '
     4      ,'  RMS      ("")/DAY**4     RMS'
     5    ,/,'               (S) (DT)   RMS        (S)/DAY    '
     5      ,'  RMS      (S)/DAY**2     RMS     (S)/DAY**3   '
     5      ,'  RMS      (S)/DAY**4     RMS'
     6    ,/,1X,131('-')
     7    ,/,1X)")
C
C LOOP OVER ALL PARAMETERS
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            IPPRT=0
            IPSET=LOCQ(3,IP)
            IPCOR=LOCQ(4,IP)
            IPCOE=LOCQ(5,IP)
            IF (IP.EQ.NPAR) THEN
              IPPRT=1
            ELSE IF (LOCQ(1,IP+1).NE.10) THEN
              IPPRT=1
            ELSE IF (LOCQ(3,IP+1).NE.IPSET) THEN
              IPPRT=2
            ELSE IF (LOCQ(4,IP+1).NE.IPCOR) THEN
              IPPRT=1
            END IF
            HLP1=10+25*(IPCOE-1)
            HLP2=HLP1+24
            RMS1=RMS*DSQRT(ANOR(IP*(IP+1)/2))/1000
            WRITE(POLTXT(HLP1:HLP2),1002) XXX(IP)/1000,RMS1
1002        FORMAT(F12.7,F11.7,1X)
            IF (IPPRT.GT.0) THEN
              WRITE(POLTXT(3:4),'(A)') ERPTXT(IPCOR)
              WRITE(POLTXT(6:8),1001) IPSET
1001          FORMAT(I3)
              WRITE(LFNPRT,2) POLTXT(1:LENGT1(POLTXT))
              IF (IPPRT.EQ.2) WRITE(LFNPRT,*)
              POLTXT=' '
              IPPRT=0
            END IF
          ENDIF
        ENDDO
C
      ENDIF
C
C STOCHASTIC ORBIT PARAMETERS
C ---------------------------
      ITYP = 11
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        CALL GTFLNA(0,'ORBITRS',FILSTRG,IRC)
        IF(TRIM(FILSTRG).EQ.'') THEN
          CALL GTFLNA(0,'LEORBRS',FILSTRG,IRC)
          IF(TRIM(FILSTRG).EQ.'') FILSTRG = '(NOT SAVED)'
        ENDIF
C
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' STOCHASTIC ORBIT PARAMETERS:',15X,A
     2    ,/,' ---------------------------'
     3    ,/,'                               '
     3      ,' perturbation epoch'
     4    ,/,' arc  sat.  set    type   dir  '
     4      ,' or  interval start    improvement '
     5      ,'    rms    unit'
     5    ,/,1X,131('-')
     6    ,/,1X)") TRIM(FILSTRG)
C
C LOOP OVER ALL PARAMETERS
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            IARC  =LOCQ(2,IP)
            NUMSAT=LOCQ(3,IP)
            IFRC  =LOCQ(5,IP)
            ISTC  =LOCQ(4,IP)
            ISAT  =LOCQ(7,IP)
            IFRCHP= mod(IFRC,10)
            ITYPHP= 1+(IFRC-mod(IFRC,10))/10
            IF (NUMSAT.GE.900 .AND. NUMSAT.LT.951) THEN
              IF(IFRC.LT.20) THEN
                CALL TIMST2(1,1,TIMSTC2(1,ISTC,ISAT,IARC),TIMSTRG)
              ELSE
                CALL TIMST2(1,1,TIMSTC2(2,ISTC,ISAT,IARC),TIMSTRG)
              END IF
              IF(IFRC.LT.10) THEN
                RMS1=RMS*DSQRT(ANOR(IP*(IP+1)/2))/SCASTC(1)
                WRITE(LFNPRT,1110) IARC,NUMSAT,ISTC,TYPTXT(ITYPHP),
     1                             FRCTXT(IFRCHP),
     1                             TIMSTRG,XXX(IP)/SCASTC(1)*1.D5,
     2                             RMS1*1.D5,UNITTXT(ITYPHP)
              ELSE
                RMS1=RMS*DSQRT(ANOR(IP*(IP+1)/2))/SCASTC(2)
                WRITE(LFNPRT,1110) IARC,NUMSAT,ISTC,TYPTXT(ITYPHP),
     1                             FRCTXT(IFRCHP),
     1                             TIMSTRG,XXX(IP)/SCASTC(2)*1.D8,
     2                             RMS1*1.D8,UNITTXT(ITYPHP)
              END IF
            ELSE
              IF(IFRC.LT.20) THEN
                CALL TIMST2(1,1,TIMSTC1(1,ISTC,ISAT,IARC),TIMSTRG)
              ELSE
                CALL TIMST2(1,1,TIMSTC1(2,ISTC,ISAT,IARC),TIMSTRG)
              END IF
              IF(IFRC.LT.10) THEN
                RMS1=RMS*DSQRT(ANOR(IP*(IP+1)/2))/SCASTC(1)
                WRITE(LFNPRT,1110) IARC,NUMSAT,ISTC,TYPTXT(ITYPHP),
     1                             FRCTXT(IFRCHP),
     1                             TIMSTRG,XXX(IP)/SCASTC(1)*1.D5,
     2                             RMS1*1.D5,UNITTXT(ITYPHP)
              ELSE
                RMS1=RMS*DSQRT(ANOR(IP*(IP+1)/2))/SCASTC(2)
                WRITE(LFNPRT,1110) IARC,NUMSAT,ISTC,TYPTXT(ITYPHP),
     1                             FRCTXT(IFRCHP),
     1                             TIMSTRG,XXX(IP)/SCASTC(2)*1.D8,
     2                             RMS1*1.D8,UNITTXT(ITYPHP)
              END IF
            END IF
1110        FORMAT(I3,I6,I6,4X,A6,1X,A3,3X,A19,F14.5,F10.5,2X,A13)
          ENDIF
        ENDDO
C
      ENDIF
C
C SATELLITE ANTENNA OFFSETS
C -------------------------
      ITYP = 12
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' SATELLITE ANTENNA OFFSETS:'
     2    ,/,' -------------------------'
     3    ,/,' '
     4    ,/,' GROUP  REQ  FROM                 TO                    '
     4      ,' X-OFF(M)  RMS(M)    Y-OFF(M)  RMS(M)    Z-OFF(M)  RMS(M)'
     5    ,/,1X,131('-'))")
C
C LOOP OVER ALL PARAMETERS
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            IOFR = LOCQ(2,IP)
            IGRP = LOCQ(5,IP)
            IF (IGRP.NE.IGROLD) WRITE(LFNPRT,3)
            IF (IOFR.NE.IOFOLD) THEN
              CALL TIMST2(1,2,TIMOFF(1:2,IOFR),TIMSTR2)
              WRITE(OFFTXT,1201) IGRP,IOFR,TIMSTR2
1201          FORMAT(2I5,3X,A40)
C
              DO 1210 ICOR=1,3
                IPOFF=IP-1+ICOR
                IPAR1 = LOCQ(1,IPOFF)
                IOFR1 = LOCQ(2,IPOFF)
                IGRP1 = LOCQ(5,IPOFF)
                ICOR1 = LOCQ(3,IPOFF)
                IF (IOFR1.NE.IOFR .OR.
     1                      IGRP1.NE.IGRP .OR.
     2                      IPAR1.NE.  12)       GOTO 1220
                RMS1=RMS*DSQRT(ANOR(IPOFF*(IPOFF+1)/2))
                ICH=54+(ICOR1-1)*20
                WRITE(OFFTXT(ICH:ICH+19),1202) XXX(IPOFF),RMS1
1202            FORMAT(F11.4,F9.4)
1210          CONTINUE
C
1220          WRITE(LFNPRT,2) OFFTXT(1:LENGT1(OFFTXT))
            ENDIF
            IGROLD=IGRP
            IOFOLD=IOFR
          ENDIF
        ENDDO
C
      ENDIF
C
C EARTH POTENTIAL PARAMETERS
C --------------------------
      ITYP = 13
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' EARTH POTENTIAL PARAMETERS:'
     2    ,/,' --------------------------'
     3    ,/,' '
     4    ,/,' DEGREE N ORDER M  SIN COS     VALUE        RMS'
     5    ,/,1X,131('-')
     6    ,/,1X)")
C
C LOOP OVER ALL PARAMETERS
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            ISC=LOCQ(4,IP)
            N=LOCQ(5,IP)
            M=LOCQ(6,IP)
            RMS1=RMS*DSQRT(ANOR(IP*(IP+1)/2))/SCAPOT
            IF(ISC.EQ.1)THEN
              WRITE(LFNPRT,1310)N,M,XXX(IP)/SCAPOT,RMS1
1310          FORMAT(I5,2X,I6,8X,'COS',4X,2D12.4)
            ELSE IF(ISC.EQ.2)THEN
              WRITE(LFNPRT,1311)N,M,XXX(IP)/SCAPOT,RMS1
1311          FORMAT(I5,2X,I6,8X,'SIN',4X,2D12.4)
            END IF
          ENDIF
        ENDDO
C
      ENDIF
C
C HILL PARAMETERS
C ---------------
      ITYP = 14
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' RESONANCE TERMS (HILL THEORY):'
     2    ,/,' -----------------------------'
     3    ,/,' '
     4    ,/,' SAT  ARC R/S/W  CONST/SIN/COS       VALUE        RMS'
     5    ,/,1X,131('-')
     6    ,/,1X)")
C
C LOOP OVER ALL PARAMETERS
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            IARC=LOCQ(2,IP)
            NUSA=LOCQ(3,IP)
            IRSW=LOCQ(4,IP)
            ISC =LOCQ(5,IP)
            RMS1=RMS*DSQRT(ANOR(IP*(IP+1)/2))/SCAHIL
            WRITE(LFNPRT,1410)NUSA,IARC,HILCHR(IRSW,1),HILCHR(ISC,2),
     1                  XXX(IP)/SCAHIL,RMS1
1410        FORMAT(I3,I5,3X,A3,9X,A3,7X,2D12.4)
          ENDIF
        ENDDO
C
      ENDIF
C
C ALBEDO PARAMETERS
C -----------------
      ITYP = 15
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' ALBEDO PARAMETERS:'
     2    ,/,' -----------------'
     3    ,/,' '
     4    ,/,' GROUP  TYPE     VALUE        RMS'
     5    ,/,1X,131('-')
     6    ,/,1X)")
C
C LOOP OVER ALL PARAMETERS
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            IGRP=LOCQ(5,IP)
            IALBT=LOCQ(4,IP)
            RMS1=RMS*DSQRT(ANOR(IP*(IP+1)/2))/SCAALB
            WRITE(LFNPRT,1510) IGRP,IALBT,XXX(IP)/SCAALB,RMS1
1510        FORMAT(I4,I6,2D14.4)
          ENDIF
        ENDDO
C
      ENDIF
C
C CENTER OF MASS
C --------------
      ITYP = 16
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' CENTER OF MASS:'
     2    ,/,' --------------'
     3    ,/,' '
     4    ,/,' COOR.       VALUE        RMS'
     5    ,/,1X,131('-')
     6    ,/,1X)")
C
C LOOP OVER ALL PARAMETERS
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            RMS1=RMS*DSQRT(ANOR(IP*(IP+1)/2))/SCACEN
            WRITE(LFNPRT,1610) CORTXT(LOCQ(2,IP)),XXX(IP)/SCACEN,RMS1
1610        FORMAT(3X,A1,2F15.4)
          ENDIF
        ENDDO
C
      ENDIF
C
C DIFFERENTIAL IONOSPHERE PARAMETERS
C ----------------------------------
      ITYP = 17
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' STOCHASTIC IONOSPHERE PARAMETERS:'
     2    ,/,' --------------------------------'
     3    ,/,' '
     4    ,/,' FILE  SAT.  MZD (DEG)  REF.  MZD (DEG)  EPOCH   VALUE '
     5      ,'(M)  RMS (M)   EPOCH (MJD)    TECU '
     6    ,/,1X,131('-')
     7    ,/,1X)")
C
C LOOP OVER ALL PARAMETERS
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            IFIL=LOCQ(2,IP)
            ISAT=LOCQ(3,IP)
            IEPO=LOCQ(4,IP)
            IREF=LOCQ(5,IP)
            ZDM1=LOCQ(6,IP)/3600.D0
            RMS1=RMS*DSQRT(ANOR(IP*(IP+1)/2))
            EPOTIME = TIMREF(IFIL)+(IEPO-1)*IDELTT(IFIL)/86400.D0
            CALL DEFREQ((/EPOTIME,EPOTIME/),1,(/ISAT/),
     1           USEGEOS=USEGEOS,GOBSDEF=GOBSDEF,MEATYPC='U')
            TECU = -XXX(IP)*FRQ(1,ISAT)**2/FACTEC

            IF (IREF.NE.0) THEN
              ZDM2=LOCQ(7,IP)/3600.D0
              WRITE(LFNPRT,1710) IFIL,ISAT,ZDM1,IREF,ZDM2,IEPO,
     1                    XXX(IP),RMS1,EPOTIME,TECU
1710          FORMAT(I4,I6,F9.2,I8,F9.2,I9,2F11.4,F14.7,F11.4)
            ELSE
              WRITE(LFNPRT,1720) IFIL,ISAT,ZDM1,IEPO,XXX(IP),RMS1,
     1              EPOTIME,TECU
1720          FORMAT(I4,I6,F9.2,I26,2F11.4,F14.7,F11.4)
            ENDIF
          ENDIF
        ENDDO
C
      ENDIF
C
C RECEIVER ANTENNA PHASE CENTER VARIATIONS
C ----------------------------------------
      ITYP = 18
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' RECEIVER ANTENNA PHASE CENTER VARIATIONS:'
     2    ,/,' ----------------------------------------')")
C
C LOOP OVER ALL PARAMETERS
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            ICAL =LOCQ(2,IP)
            IFRQ =LOCQ(3,IP)
            IZEN =LOCQ(4,IP)
            IAZI =LOCQ(5,IP)
            NZEN =LOCQ(6,IP)
            NAZI =LOCQ(7,IP)
C
C WRITE TITLE
            IF (ICAL.NE.ICAOLD) THEN
              IF (NZEN.GT.1) THEN
                MODTYP=1
                IDZEN = IDNINT(RAPZENMAX/PI*180.D0/DBLE(NZEN-1))
                IDAZI = IDNINT(360.D0/DBLE(NAZI-1))
                WRITE(LFNPRT,1801) (ANTCAL(JJ,ICAL),JJ=2,1,-1),
     1                      (NUMCAL(JJ,ICAL),JJ=1,2),
     2                      MODTYP,IDZEN,IDAZI
                DO JJ=0,NZEN-1
                  ICH1=JJ*7+1
                  ICH2=ICH1+6
                  IF (ICH2.LE.LEN(ANTTXT)) THEN
                    WRITE(ANTTXT(ICH1:ICH2),'(I7)') IDZEN*JJ
                  ELSE
                    ICH2 = LEN(ANTTXT)
                    ICH1 = ICH2 - 2
                    WRITE(ANTTXT(ICH1:ICH2),'(A)') '...'
                  ENDIF
                ENDDO
                WRITE(LFNPRT,1803) TRIM(ANTTXT)
              ELSE IF (NZEN.LT.0) THEN
                MODTYP=4
                WRITE(LFNPRT,1802) (ANTCAL(JJ,ICAL),JJ=2,1,-1),
     1                      (NUMCAL(JJ,ICAL),JJ=1,2),
     2                      MODTYP,-NZEN,NAZI
                DO JJ=1,-NZEN
                  ICH1=(JJ-1)*7+1
                  ICH2=ICH1+6
                  IF (ICH2.LE.LEN(ANTTXT)) THEN
                    WRITE(ANTTXT(ICH1:ICH2),'(I7)') JJ
                  ELSE
                    ICH2 = LEN(ANTTXT)
                    ICH1 = ICH2 - 2
                    WRITE(ANTTXT(ICH1:ICH2),'(A)') '...'
                  ENDIF
                ENDDO
                WRITE(LFNPRT,1803) TRIM(ANTTXT)
              END IF
1801         FORMAT(/,' ANTENNA TYPE             RECEIVER TYPE       ',
     1                    '     FROM   TO      TYP   D(Z) D(A)',
     2                    /,' ',131('-'),
     3                   /,2(1X,A20,4X),1X,I6,1X,I6,2X,I3,2X,2(2X,I3),/)
1803          FORMAT(1X,'S FQ A/Z',A)
1802          FORMAT(/,' ANTENNA TYPE             RECEIVER TYPE       ',
     1                    '     FROM   TO      TYP   N(Z) M(A)',
     2                   /,' ',131('-'),
     3                   /,2(1X,A20,4X),1X,I6,1X,I6,2X,I3,2X,2(2X,I3),/)
              ANTTXT=' '
            ENDIF
C
C WRITE VALUES INTO STRING
            ICH1=(IZEN-1)*7+1
            ICH2=ICH1+6
            RMS1=RMS*DSQRT(ANOR(IKF(IP,IP)))
            IF (ICH2.LE.LEN(ANTTXT)) THEN
              WRITE(ANTTXT(ICH1:ICH2),'(F7.2)') XXX(IP)*1000.D0
              WRITE(ANTRMS(ICH1:ICH2),'(F7.2)') RMS1*1000.D0
            ELSE
              ICH2 = LEN(ANTTXT)
              ICH1 = ICH2 - 2
              WRITE(ANTTXT(ICH1:ICH2),'(A)') '...'
              WRITE(ANTRMS(ICH1:ICH2),'(A)') '...'
            ENDIF
C
C WRITE COMPLETED LINE
            IF (IP.EQ.NPAR .OR. (LOCQ(1,IP+1).NE.18    .OR.
     1                  LOCQ(2,IP+1).NE.ICAL  .OR.
     2                  LOCQ(3,IP+1).NE.IFRQ  .OR.
     3                  LOCQ(5,IP+1).NE.IAZI)) THEN
              IF (NZEN.GT.1) THEN
                IDAZI = IDNINT(360.D0/DBLE(NAZI-1))
                WRITE(LFNPRT,1804) G_SVNSYS(PRNCAL(ICAL)),FRQTXT(IFRQ),
     1                      IDAZI*(IAZI-1),TRIM(ANTTXT),TRIM(ANTRMS)
              ELSE IF (NZEN.LT.0) THEN
                WRITE(LFNPRT,1804) G_SVNSYS(PRNCAL(ICAL)),FRQTXT(IFRQ),
     1                      IAZI,TRIM(ANTTXT),TRIM(ANTRMS)
              END IF
1804          FORMAT(1X,A1,1X,A2,I4,A,/,9X,A)
              ANTRMS=' '
              ANTTXT=' '
            END IF
C
            ICAOLD=ICAL
          ENDIF
        ENDDO
C
      ENDIF
C
C GLOBAL IONOSPHERE MODEL PARAMETERS
C ----------------------------------
      ITYP = 19
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        CALL GTFLNA(0,'IONOSRS',FILSTRG,IRC)
        IF(TRIM(FILSTRG).EQ.'') FILSTRG = '(NOT SAVED)'
C
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' GLOBAL IONOSPHERE MODEL PARAMETERS:',8X,A
     2    ,/,' ----------------------------------'
     3    ,/,1X)") TRIM(FILSTRG)
C
C LOOP OVER ALL PARAMETERS
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            CALL PRIGIM(IPART ,NPAR  ,LOCQ  ,XXX   ,ANOR  ,RMS   ,
     1                  OPTGIM,POLGIM,NAMGIM,EPOGIM,SCAGIM,IP    ,
     2                  INFGIM,MXCPAR)
          ENDIF
        ENDDO
C
      ENDIF
C
C STATION VELOCITIES (ESTIMATED IN ADDNEQ ONLY)
C ---------------------------------------------
      ITYP = 20
C
C KINEMATIC COORDINATES
C ---------------------
      ITYP = 21
      IF(PARLST(1,ITYP).GT.0.AND.PRIOPT(12).EQ.0.AND.
     1   OPTELI(ITYP).NE.1) THEN
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        CALL TIMST2(1,1,CLKHED%TFIRST,TIMSTRG)

        CALL gtflna(0,'KINOUT',filstrg,irc)
        IF (irc /= 0 .OR. LEN_TRIM(filstrg) == 0) THEN
          FILSTRG = '(NOT SAVED)'
        ENDIF

C
C       New non-LEO output
        IF (1== 1) THEN
        WRITE(LFNPRT,"(
     1       ' KINEMATIC COORDINATES:',21X,A
     2    ,/,' ---------------------'
     3    ,/,' '
     4    ,/,'  EPO: EPOCHS SINCE  ',A19,'  (SAMPLING ',I5,' SEC)'
     5    ,/,' '
     6    ,/,'                                               CORRECTIO',
     6       'N AND RMS IN METER                      ESTIMATED POSITI',
     6       'ON WRT. FIXED COORD.'
     7    ,/,'  EPO   EPOCH(MJD)     #OBS STA         LATITUDE        ',
     7       '  LONGITUDE             HEIGHT          NORTH(M)       E',
     7       'AST(M)         UP(M)'
     8    ,/,1X,131('-'),/,1X)")
     9    TRIM(FILSTRG),TIMSTRG,IDNINT(CLKREC%EPOCH(1)/CLKREC%NEPO)
C
C       Old non-LEO output
        ELSE
        WRITE(LFNPRT,"(
     1       ' KINEMATIC COORDINATES:'
     2    ,/,' ---------------------'
     3    ,/,' '
     4    ,/,'  EPO: EPOCHS SINCE  ',A19,'  (SAMPLING ',I5,' SEC)'
     5    ,/,' '
     6    ,/,'  EPO  STATION/#OBS          LATITUDE       '
     6      ,'   LONGITUDE            HEIGHT'
     7    ,/,1X,131('-')
     8    ,/,1X)") TIMSTRG,IDNINT(CLKREC%EPOCH(1)/CLKREC%NEPO)
        ENDIF
C
C LOOP OVER ALL PARAMETERS (READ FROM ANOR)
        SCRATCH = .TRUE.
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            CALL PRIKIN(MXCLCQ,IPART, NSTAT, ICENTR,STNAME,XSTAT ,
     1                  XSTELL,XSTECC,AELL,  BELL,  DXELL, DRELL ,
     2                  SCELL, CLKHED,CLKREC,NEPOBS(3)    ,parflg,
     3                  IP    ,LOCQ  ,RMS   ,ANOR,XXX0(IP),XXX   )
            SCRATCH = .FALSE.
          ENDIF
        ENDDO
C
C LOOP OVER ALL PARAMETERS (READ FROM SCRATCH FOR PREELIMINATED EPOCH PARAMETERS)
        IF (SCRATCH.AND.IRAUX2.EQ.0) THEN
C
C KIN. COORDINATES HAVE TO BE SORTED BY STATIONS
C   JNUM: STATION NUMBER FOUND IN THE SCRATCH FILE
C   INUM: ACTUAL STATION NUMBER TO PRINT - PRINT IF INUM.EQ.JNUM
C   NNUM: NEXT STATION NUMBER FOR INUM
C   MNUM: BIGGEST STATION NUMBER FOUND IN THE SCRATCH FILE - MAX(JNUM)
          INUM=0
          MNUM=0
          NNUM=1
          DO 9005
            IF (MNUM.GT.0.AND.INUM.EQ.MNUM) GOTO 9006
            IF (MNUM.EQ.0.AND.NNUM.EQ.INUM) GOTO 9006
C WHAT IS THE NEXT STATION
            REWIND(LFNAUX)
            INUM=NNUM
C GET THE TYPE OF PARAMETER
            DO 9004
C
              CALL RDSCRA(MXCLCQ,LFNAUX,JTYP,JNUM,TOBS,LCQ2,XAPR0,
     1                    XTMP,ATMP,IOS)
              IF (IOS.NE.0) GOTO 9005
              IF (JTYP.NE.ITYP) GOTO 9004
C
              IF (MNUM.LT.JNUM) MNUM=JNUM
              IF (JNUM.GT.INUM.AND.
     1          (JNUM.LT.NNUM.OR.INUM.EQ.NNUM)) NNUM=JNUM
C
              IF (JNUM.NE.INUM) GOTO 9004
C
              CALL PRIKIN(MXCLCQ,IPART, NSTAT, ICENTR,STNAME,XSTAT,
     1                    XSTELL,XSTECC,AELL,  BELL,  DXELL, DRELL,
     2                    SCELL, CLKHED,CLKREC,NEPOBS(3),    (/0,0,0/),
     3                    0,     LCQ2,  RMS,ATMP,  XAPR0 ,XTMP  )
9004        CONTINUE
9005      CONTINUE
9006      CONTINUE
        ENDIF
C
      ENDIF
C
C SCALING FACTORS FOR VIENNA GRID FILES
C -------------------------------------
      ITYP = 22
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
        WRITE(LFNPRT,'(2(1X,A,/),1X,/,1X,A,/,1X,131("-"),/,1X)')
     1   'SCALING FACTORS FOR VIENNA GRID FILES:',
     2   '-------------------------------------',
     3   'GRID TYPE   STATION NAME       COMPONENT    FACTOR        RMS'
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            STHELP = OPLOAD(LOCQ(2,IP))%staLst(LOCQ(3,IP))
            IF (OPLOAD(LOCQ(2,IP))%staClu(LOCQ(3,IP)) == -1) THEN
              STHELP = 'ALL STATIONS'
            ELSE IF (OPLOAD(LOCQ(2,IP))%staClu(LOCQ(3,IP)) > 0) THEN
              STHELP = ''
              WRITE(STHELP,'(A,1X,I3.3)')
     1              'GROUP',OPLOAD(LOCQ(2,IP))%staClu(LOCQ(3,IP))
            ENDIF
            WRITE(LFNPRT,'(1X,A9,3X,A16,6X,A3,3X,2F12.6)')
     1            OPLOAD(LOCQ(2,IP))%KEYW,STHELP,
     2            GRDTYP(LOCQ(5,IP),LOCQ(4,IP)),
     3            1D0+XXX(IP),RMS*DSQRT(ANOR(IKF(IP,IP)))
          ENDIF
        ENDDO
      ENDIF
C
C EPOCH WISE STATION CLOCKS
C -------------------------
      ITYP = 23
C
      IF(PARLST(1,ITYP).GT.0.AND.PRIOPT(12).EQ.0.AND.
     1   OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        CALL GTFLNA(0,'CLKRNX ',FILSTRG,IRC)
        IF(TRIM(FILSTRG).EQ.'')FILSTRG = '(NOT SAVED)'
C
        WRITE(LFNPRT,"(
     1       ' EPOCH WISE STATION CLOCKS:',17X,A
     2    ,/,' --------------------------'
     3    ,/,50X,' STATION CLOCK VALUES (USEC)'
     4    ,/,' TYPE  STAT       EPOCH(MJD)             A PRIORI        '
     4      ,' CORRECTION              TOTAL       RMS(NSEC)    #OBS  '
     4      ,' STATION'
     5    ,/,1X,131('-')
     6    ,/,1X)") TRIM(FILSTRG)
C
        CALL PRIREF(MXCLCQ,ITYP,NCLKST,NCLKSA,SECIPL,CLKHED,CLKREC,
     1              STNAME,NSAMPL(3),NEPOBS,RNXCLK,NPARN,LOCQ,ANOR)
C
C LOOP OVER ALL PARAMETERS (READ FROM ANOR)
        SCRATCH = .TRUE.
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            ISTA=LOCQ(2,IP)
            IEPO=LOCQ(4,IP)
C
            DSEC=DBLE(IEPO-1)*CLKREC%EPOCH(1)/CLKREC%NEPO
            TOBS=CLKHED%TFIRST+DSEC/86400.D0
C
            CALL PRICLK(MXCLCQ,STNAME,NEPOBS,NCLKST,NCLKSA,TOBS,
     1                  RMS,LOCQ(1,IP),-XXX0(IP),XXX(IP),
     2                  ANOR(IKF(IP,IP)))
C
            IF (NSAMPL(3).EQ.0) SCRATCH = .FALSE.
          ENDIF
        ENDDO
C
C LOOP OVER ALL PARAMETERS (READ FROM SCRATCH FOR PREELIMINATED EPOCH PARAMETERS)
        IF (SCRATCH.AND.IRAUX2.EQ.0) THEN
          REWIND(LFNAUX)
C GET THE TYPE OF PARAMETER
          DO 9014
            CALL RDSCRA(MXCLCQ,LFNAUX,JTYP,JNUM,TOBS,LCQ2,XAPR0,
     1                  XTMP,ATMP,IOS)
            IF (IOS.NE.0) GOTO 9016
            IF (JTYP.NE.ITYP) GOTO 9014
C
            CALL PRICLK(MXCLCQ,STNAME,NEPOBS,NCLKST,NCLKSA,
     1                  TOBS,  RMS,   LCQ2,  XAPR0(1),XTMP(1),ATMP(1))
9014      CONTINUE
9016      CONTINUE
        ENDIF
C
      ENDIF
C
C EPOCH WISE SATELLITE CLOCKS
C ---------------------------
      ITYP = 24
C
      IF(PARLST(1,ITYP).GT.0.AND.PRIOPT(12).EQ.0.AND.
     1   OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        CALL GTFLNA(0,'CLKSAV ',FILSTRG,IRC)
        IF(TRIM(FILSTRG).EQ.'') THEN
          CALL GTFLNA(0,'CLKRNX ',FILSTRG,IRC)
          IF(TRIM(FILSTRG).EQ.'')FILSTRG = '(NOT SAVED)'
        ENDIF
C
        WRITE(LFNPRT,"(
     1       ' EPOCH WISE SATELLITE CLOCKS:',15X,A
     2    ,/,' ----------------------------'
     3    ,/,50X,'SATELLITE CLOCK VALUES (USEC)'
     4    ,/,' TYPE   SAT       EPOCH(MJD)             A PRIORI        '
     4      ,' CORRECTION              TOTAL       RMS(NSEC)    #OBS'
     5    ,/,1X,131('-')
     6    ,/,1X)") TRIM(FILSTRG)
C
        CALL PRIREF(MXCLCQ,ITYP,NCLKST,NCLKSA,SECIPL,CLKHED,CLKREC,
     1              STNAME,NSAMPL(3),NEPOBS,RNXCLK,NPARN,LOCQ,ANOR)
C
C LOOP OVER ALL PARAMETERS (READ FROM ANOR)
        SCRATCH = .TRUE.
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            ISAT=LOCQ(3,IP)
            IEPO=LOCQ(4,IP)
C
            DSEC=DBLE(IEPO-1)*CLKREC%EPOCH(1)/CLKREC%NEPO
            TOBS=CLKHED%TFIRST+DSEC/86400.D0
C
            CALL PRICLK(MXCLCQ,STNAME,NEPOBS,NCLKST,NCLKSA,TOBS,
     1                  RMS,LOCQ(1,IP),XXX0(IP),XXX(IP),
     2                  ANOR(IKF(IP,IP)))
            IF (NSAMPL(3).EQ.0) SCRATCH = .FALSE.
C
          ENDIF
        ENDDO
C
C LOOP OVER ALL PARAMETERS (READ FROM SCRATCH FOR PREELIMINATED EPOCH PARAMETERS)
        IF (SCRATCH.AND.IRAUX2.EQ.0) THEN
          REWIND(LFNAUX)
C GET THE TYPE OF PARAMETER
          DO 9024
            CALL RDSCRA(MXCLCQ,LFNAUX,JTYP,JNUM,TOBS,LCQ2,XAPR0,
     1                  XTMP,ATMP,IOS)
            IF (IOS.NE.0) GOTO 9026
            IF (JTYP.NE.ITYP) GOTO 9024
C
            CALL PRICLK(MXCLCQ,STNAME,NEPOBS,NCLKST,NCLKSA,
     1                  TOBS,  RMS,   LCQ2,  XAPR0(1),XTMP(1),ATMP(1))
9024      CONTINUE
9026      CONTINUE
        ENDIF
C
      ENDIF
C
C SATELLITE ANTENNA PHASE CENTER VARIATIONS
C -----------------------------------------
      ITYP = 25
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' SATELLITE ANTENNA PHASE CENTER VARIATIONS IN MM:'
     2    ,/,' -----------------------------------------------')")
C
C LOOP OVER ALL PARAMETERS
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            ISPV =LOCQ(2,IP)
            IGRP =LOCQ(3,IP)
            IZEN =LOCQ(4,IP)
            IAZI =LOCQ(5,IP)
            NZEN =LOCQ(6,IP)
            NAZI =LOCQ(7,IP)
C
C WRITE TITLE
            IF (ISPV.EQ.1 .AND. ISPV.NE.ISPOLD) THEN
C
C RD: "NADIGN" IS CURRENTLY NOT COUNTED IN PRCEPO!!!
              WRITE(LFNPRT,2510) (NAMAX/PI*180.D0),NADIGN
2510          FORMAT(/,' ACTUAL MAXIMUM NADIR ANGLE:     ',F6.3,' DEG',
     1               /,' # OBS IGNORED (NADIR > NADMAX): ',I6,/)
            ENDIF
            IF (ISPV.NE.ISPOLD) THEN
              RIDZEN =
     1         DBLE(IDNINT((NADMAX/PI*180.D0)/DBLE(NZEN-1)*10.D0))/10.D0
              IDAZI = IDNINT(360.D0/DBLE(NAZI-1))
              WRITE(LFNPRT,2501) IGRP,RIDZEN,IDAZI
              WRITE(ANTNAD(1:14),2503)
2501          FORMAT(/,' SATELLITE GROUP   D(N) D(A)',
     1               /,' ',131('-'),
     2               /,13X,I3,2X,F5.1,2X,I3,/)
2503          FORMAT(5X,'A/N   0.0')
              DO 2502 JJ=1,NZEN-1
                ICH3=(JJ-1)*7+15
                ICH4=ICH3+6
                ZENJJ = DBLE(IDNINT((NADMAX/PI*180.D0)/DBLE(NZEN-1)
     1                                                 *JJ*10.D0))/10.D0
                WRITE(ANTNAD(ICH3:ICH4),2505) ZENJJ
2505            FORMAT(F7.1)
2502          CONTINUE
              WRITE(LFNPRT,2506) TRIM(ANTNAD)
2506          FORMAT(A)
            ENDIF
C
C WRITE VALUES INTO STRING
            ICH1=(IZEN-1)*7+1
            ICH2=ICH1+6
            IF (ANOR(IKF(IP,IP)).GT.0.0) THEN
              RMS1=RMS*DSQRT(ANOR(IKF(IP,IP)))
            ELSE
              RMS1=0.09999D0
            ENDIF
            WRITE(ANTTXT(ICH1:ICH2),'(F7.2)') (XXX(IP)*1000.D0)
            WRITE(ANTRMS(ICH1:ICH2),'(F7.2)') (RMS1*1000.D0)
C WRITE COMPLETED LINE
            IF (IP.EQ.NPAR .OR. (LOCQ(1,IP+1).NE.25    .OR.
     1                           LOCQ(2,IP+1).NE.ISPV  .OR.
     2                           LOCQ(3,IP+1).NE.IGRP  .OR.
     3                           LOCQ(5,IP+1).NE.IAZI)) THEN
              IDAZI = IDNINT(360.D0/(NAZI-1))
              WRITE(LFNPRT,2504) IDAZI*(IAZI-1),
     1                           ANTTXT(1:LENGT1(ANTTXT)),
     2                           ANTRMS(1:LENGT1(ANTRMS))
2504          FORMAT(3X,I4,A,/,7X,A)
              ANTRMS=' '
              ANTTXT=' '
              ANTNAD=' '
            END IF
C
            ISPOLD=ISPV
          ENDIF
        ENDDO
      ENDIF
C
C SLR RANGE BIASES
C ----------------
      ITYP = 26
C
      IF(PARLST(1,ITYP).GT.0.AND.OPTELI(ITYP).NE.1) THEN
C
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
C
        WRITE(LFNPRT,"(
     1       ' SLR RANGE BIASES IN MM (IMPROVEMENTS):'
     2    ,/,' -------------------------------------'
     3    ,/,' '
     4    ,/,' REQUEST  STATION NAME    SATELLITE   WL     ',
     5       'BIAS (MM)    RMS (MM)'
     6    ,/,1X,131('-')
     7    ,/,1X)")
C
C LOOP OVER ALL PARAMETERS
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            II=IKF(IP,IP)

            ISTA = LOCQ(2,IP)
            IWL  = LOCQ(4,IP)
            ISAT = LOCQ(5,IP)
            IREQ = LOCQ(6,IP)
            RMS1 = 1d3 * RMS * DSQRT(ANOR(II))
            XXX1 = 1d3 * XXX(IP)

            INUL=1
            IF (ANOR(II).EQ.0D0) INUL=2

            IF ( satSpec == 1 ) THEN
              WRITE(LFNPRT,2601) IREQ,STNAME(ISTA),ISAT,IWL, XXX1,RMS1,
     1                           NULTXT(INUL)
2601          FORMAT(I6,4X,A16,3X,I3,6X,I2,2(2X,F10.2),6X,A)

            ELSEIF ( satSpec == 2 ) THEN
              WRITE(LFNPRT,2602) IREQ,STNAME(ISTA),
     1                           g_strsys3(ABS(ISAT)-1),IWL,
     2                           XXX1,RMS1,NULTXT(INUL)
2602          FORMAT(I6,4X,A16,3X,A3,6X,I2,2(2X,F10.2),6X,A)

            ELSEIF ( satSpec == 3 ) THEN
              WRITE(LFNPRT,2603) IREQ,STNAME(ISTA),g_strsys3(ISAT/100),
     1                           IWL, XXX1,RMS1,NULTXT(INUL)
2603          FORMAT(I6,4X,A16,3X,A3,6X,I2,2(2X,F10.2),6X,A)
            ENDIF

          ENDIF
        ENDDO
      ENDIF
C
C HIGHER-ORDER IONOSPHERE (HOI) SCALING FACTORS
C ---------------------------------------------
      ITYP = 27
      IF(PARLST(1,ITYP).GT.0) THEN
C
C PRINT TITLE
        WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
        WRITE(LFNPRT,"(
     1       ' HIGHER-ORDER IONOSPHERE SCALING FACTORS:'
     2    ,/,' ---------------------------------------'
     3    ,/,' '
     4    ,/,' STATION NAME    HOI       VALUE          RMS'
     5    ,/,1X,131('-')
     6    ,/,1X)")
C
C LOOP OVER ALL PARAMETERS
        DO IP=1,NPAR
          IF(LOCQ(1,IP).EQ.ITYP) THEN
            RMS1=RMS*DSQRT(ANOR(IKF(IP,IP)))
            IF(LOCQ(3,IP).EQ.1) THEN
              WRITE(LFNPRT,2710) ' ',LOCQ(2,IP),1D0+XXX(IP),RMS1
            ELSEIF(LOCQ(3,IP).EQ.2) THEN
              WRITE(LFNPRT,2710) STNAME(LOCQ(4,IP)),
     1                               LOCQ(2,IP),1D0+XXX(IP),RMS1
            ENDIF
2710        FORMAT(1X,A16,I2,2F15.5)
          ENDIF
        ENDDO
      ENDIF
C
C CLOSE SCRATCH FILE WITH RESULTS FROM RESEPO
C -------------------------------------------
      IF (IRAUX2.EQ.0) CLOSE(LFNAUX)
C
      RETURN
      END SUBROUTINE

      END MODULE
