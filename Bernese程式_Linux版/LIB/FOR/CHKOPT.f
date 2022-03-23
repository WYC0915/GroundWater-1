      MODULE s_CHKOPT
      CONTAINS

C*
      SUBROUTINE CHKOPT(MAXTYP, STRAMB,SIGAMB,NSAMPL,OPTELI,OPTPAR,
     1                  OPTDIP, SIGDIP,OPTGIM,POLGIM,AR2MOD,NIOREQ,
     2                  MELWUB, NFTOT ,NFRFIL,ICARR ,NANOFF,NRQOFF,
     3                  GRPOFF, NFIX  ,NKIN,  NORB  ,NORB2 ,SEQORB,
     4                  SEQORB2,ITRGRD,ZENMAX,NANSPV,CORSTR,EPOGIM,
     5                  CLKHED, PRIOPT,IEPPAR,IRESID,IFREQ ,NANRAO,
     6                  NANCAL, NOSOL ,NCLREQ,ISTCLK,IBIAS ,CLFRTO,
     7                  NALLSAT,ALLSATNUM,NALLSTA,ALLSTANUM,
     8                  ALLSTANAME,IDIFF)
CC
CC NAME       :  CHKOPT
CC
CC PURPOSE    :  CHECK CONSISTENCY OF OPTIONS FOR PROGRAM "GPSEST"
CC               SET RESIDUAL SAVE FLAG
CC
CC PARAMETERS :
CC         IN :  MAXTYP : MAXIMUM NUMBER OF PARAMETER TYPES   I*4
CC               STRAMB : (1): AMBIGUITY RESOLUTION STRATEGY  I*4(2)
CC                             =-1: AMBIGUITY PRE-ELIMINATION
CC                             = 0: NO AMBIGUITY RESOLUTION
CC                             = 1: ROUND-TO-NEAREST-INTEGER
CC                             = 2: GENERAL SEARCH
CC                             = 3: SIGMA-DEPENDENT
CC                             = 4: QUASI-IONOSPHERE-FREE
CC                        (2): AMBIGUITY PRE-ELIMINATION
CC                             =-1: ONCE PER SESSION
CC                             = 0: EVERY EPOCH
CC                             = N: EVERY N SECONDS
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
CC               NSAMPL : SAMPLING RATE IN SEC                I*4
CC               OPTELI(I),I=1,..,MAXTYP: OPTION FOR PRE-     I*4
CC                        ELIMINATION OF PARAMETER TYPES:
CC                        =0 : NOT PRE-ELIMINATED
CC                        =1 : PRE-ELIMINATED BEFORE INVERSION
CC                        =2 : PRE-ELIMINATED AFTER  INVERSION
CC                        =3 : PRE-ELIMINATED EPOCH-WISE
CC               OPTPAR(I),I=1,..,MAXTYP: FLAG WHETHER PARA-  I*4
CC                        METER TYPE I IS ESTIMATED
CC                        =0 : NOT ESTIMATED
CC                        =1 : ESTIMATED
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
CC               POLGIM(I,J),I=1,2,3,J=1,..,OPTGIM(7):        R*8(3,*)
CC                        I=1: HEIGHT OF SINGLE LAYER (M)
CC                        I=2: LAT. OF NORTH GEOMAGNETIC POLE
CC                        I=3: EAST LONGITUDE
CC               AR2MOD : MODE OF PROCESSING FOR AMBIGUITY    I*4
CC                        RESOLUTION STRATEGY 2
CC                        =0 : RESOLVE ALL PRESENT AMBIGUITIES
CC                        =1 : BASELINE-WISE AMBIGUITY RESOLUTION
CC               NIOREQ : NUMBER OF IONOSPHERE MODELS TO BE   I*4
CC                          TO BE ESTIMATED
CC               MELWUB : MELBOURNE-WUEBBENA COMBINATION      I*4
CC                          =0 : NO
CC                          =1 : YES
CC                          =2 : DTEC OBSERVATIONS
CC               NFTOT  : TOTAL NUMBER OF FILES               I*4
CC               NFRFIL : NUMBER OF FREQUENCIES TO BE PROC.   I*4
CC                          NFRFIL(I):
CC                            I: FILE
CC               ICARR  : FREQUENCIES TO BE PROCESSED         I*4
CC                          ICARR(K,I):
CC                            K: FREQUENCY
CC                            I: FILE
CC               NANOFF : NUMBER OF SATELLITE ANTENNA OFFSET  I*4
CC                        GROUPS TO BE ESTIMATED
CC               NRQOFF : NUMBER OF SATELL. ANTENNA REQUESTS  I*4
CC               GRPOFF(I),I=1,..,NRQOFF: ANTENNA GROUP FOR   I*4
CC                        REQUEST NUMBER I
CC               NFIX   : NUMBER OF FIX STATIONS              I*4
CC               NKIN   : NUMBER OF STATION WITH KIN COORD    I*4
CC               NORB   : # OF GPS ORBITAL PARAM. TO BE ESTIM.I*4
CC               NORB2  : # OF LEO ORBITAL PARAM. TO BE ESTIM.I*4
CC               SEQORB : SEQUENCE FOR GPS ORBITAL ELEMENTS   I*4(1)
CC               SEQORB2: SEQUENCE FOR LEO ORBITAL ELEMENTS   I*4(1)
CC               ITRGRD : (1): EST. OF TROPOSPHERIC GRADIENTS I*4(*)
CC                             =0: NO ESTIMATION
CC                             =1: TILTING
CC                             =2: LINEAR
CC                        (2): RATIO OF NUMBER OF ZENITH TO
CC                             GRADIENT PARAMETERS
CC               ZENMAX : MAXIMUM ZENITH DISTANCE IN RAD      R*8
CC               NANSPV : NUMBER OF SATELLITE ANTENNA PHASE   I*4
CC                        CENTER GROUPS TO BE ESTIMATED
CC               NANRAO : NUMBER OF RECEIVER ANTENNA OFFSETS  I*4
CC               NANCAL : NUMBER OF RECEIVER ANTENNA PHASE    I*4
CC                        CENTER REQUESTS
CC               NOSOL  : =1: COMPUTE NO SOLUTION             I*4
CC                        =0: COMPUTE SOLUTION
CC               IDIFF  : =1: DOUBLE DIFF. OBSERVATIONS       I*4
CC                        =2: ZERO DIFF. OBSERVATIONS
CC                        =3: RANGE OBSERVATIONS
CC     IN/OUT :  CORSTR : CORRELATION STRATEGY                I*4
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
CC                             =1: COSZ
CC                             =2: MSLM
CC                             =3: ESM
CC                        (9): STATION-SPECIFIC MODELS
CC                        (10): COMPONENT TO BE ESTIMATED
CC                              =1: DETERMINISTIC
CC                              =2: STOCHASTIC
CC               EPOGIM(I,J),I=1,2,J=1,..,OPTGIM(7): PERIODS  R*8(2,*)
CC                        OF VALIDITY / REF EPOCHS (MJD)
CC               CLKHED : CLOCK HEADER INFORMATION            T_CLKHEAD
CC                          %NUMREF=0: FIX REF-CLOCKS
CC                          %NUMREF=2: SUM FOR REF-CLOCKS
CC               PRIOPT(I),I=1,..: PRINT OPTIONS              I*4
CC               IEPPAR : EPOCH PARAMETERS REQUESTED          I*4
CC        OUT :  IRESID : SAVE RESIDUALS IN FILE              I*4
CC                          =0: NO SAVE
CC                          =1: SAVE RESIDUALS
CC               IFREQ  : FREQUENCY FOR SATELLITE ANTENNA     I*4
CC                        PATTERN EST. (USED IN NEQWRITE)
CC               NCLREQ : # CLOCK ERROR REQUESTS              I*4
CC               ISTCLK : STATION NUMBERS FOR CLOCK REQUESTS  I*4(:)
CC               NCLK   : # CLOCK PARAMETERS TO BE ESTIMATED  I*4(*)
CC                        NCLK(I)=1 : OFFSET ESTIMATED FOR STATION I
CC               IBIAS  : 0: STATION SPECIFIC                 I*4(*)
CC                        1: FREQUENCY SPECIFIC
CC                        2: SATELLITE SPECIFIC
CC                        3: SAT. SPECIFIC FOR NON-GPS
CC                        4: FREQUENCY SPECIFIC WITH POLYNOM
CC               CLFRTO : TIME INTERVAL FOR CLOCK ERRORS      R*8(2,*)
CC                        CLFRTO(1,K): START OF THE INTERVAL
CC                        CLFRTO(2,K): END OF THE INTERVAL
CC                        IN JULIAN DATE FOR CLOCK REQUEST K
CC               NALLSAT:     ! NUMBER OF ALL SATELLITES      I*4
CC               ALLSATNUM:   ! SATELLITE NUMBERS             I*4(*)
CC               NALLSTA:     ! NUMBER OF ALL STATIONS        I*4
CC               ALLSTANUM:   ! STATION NUMBERS               I*4(*)
CC               ALLSTANAME:  ! STATION NAMES                 CH*(*)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, S.SCHAER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  90/04/07 13:47
CC
CC CHANGES    :  23-APR-92 : ??: NEW INTERNAL FILE NAME "RESIDRS"
CC               18-JAN-93 : ??: CHECK AMBIGUITY SEARCH OPTIONS
CC               26-APR-93 : ??: CHECK OPTIONS IF PREELIMINATING ORBITS
CC               11-APR-94 : ??: CHECK OPTIONS CONCERNING DIF.ION.PAR.
CC               19-APR-94 : RW: CPO-MODEL INCLUDED
CC               25-JUL-94 : MR: PRE-ELIMINATION STATISTICS SIMPLIFIED
CC               27-JUL-94 : MR: FLAGS FOR ESTIMATED PARAMETER TYPES
CC               10-AUG-94 : MR: CALL EXITRC
CC               16-AUG-94 : SS: CHECK "SIGDIP(1)"
CC               22-AUG-94 : SS: DIFF. IONOS. PARAMETER RENAMED
CC               20-SEP-94 : EB: NO RPR IN NEQ SAVING CASE
CC               06-JUN-95 : SS: GLOBAL IONOSPHERE MODEL PARAMETERS
CC               08-JUN-95 : LM: KINEMATIC COORDINATES
CC               14-AUG-95 : LM: STRAMB = ARRAY
CC               30-AUG-95 : MR: REMOVE SLASH FROM FORMAT
CC               05-DEC-95 : SS: NEW IONOSPHERE MODEL (TYPE 2)
CC               05-DEC-95 : SS: "IONO" REMOVED (SEE SR CHKION)
CC               26-MAR-96 : TS: CHECK CLOCK PRE-ELIMINATION OPTIONS
CC               19-JUL-96 : MR: CHECK AMBIG. PRE-ELIMI. IF NEQS SAVED
CC               15-SEP-96 : EB: CHECK AMBIG. PRE-ELIMI. IF NEQS SAVED (MODIF.)
CC               08-APR-97 : SS: NIELL MAPPING, TROPOSPHERE GRADIENTS
CC               14-APR-97 : SS: ALLOW NEQ SAVING WITH TROPOSPHERE GRADIENTS
CC               15-APR-97 : SS: CHECK ELEVATION CUT-OFF ANGLE
CC               05-AUG-97 : SS: ELEV.-DEP. SIP CONSTRAINING
CC               11-AUG-97 : SS: NEW OPTION "STRAMB(2)"
CC               14-AUG-97 : SS: "OPTELI(17)" ALREADY SET IN SR RDINPT
CC               08-OCT-97 : MR: RATIO ZENITH/GRADIENT PARAMETERS
CC               30-OCT-97 : SS: SIGMA OF ZERO-DIFFERENCE OBS
CC               26-JAN-98 : SS: STATION-SPECIFIC GIMS
CC               29-APR-98 : SS: DTEC LC
CC               18-SEP-01 : SS: NEW TEC MAPPING FUNCTIONS
CC               23-JAN-02 : RD: CONDITION OF SUM FOR REFERENCE CLOCKS
CC               29-JAN-02 : RD: SET "IRESID" IF EPOCH PARAMETERS BUT
CC                               NOT A RESI FILE SPECIFIED
CC               31-JAN-02 : RD: KININP AND KINOUT WORK ONLY WITH EPOCH-PREELIM
CC               26-JUN-02 : RD/DS: MERGIN VERSIONS BE AND MUNICH
CC               19-JAN-03 : SS/MM: REVISION OF GPSEST INPUT PANELS
CC               28-JAN-03 : RD: NO EPOCH SOLUTION IF RESULTS NOT REQUESTED
CC               19-JUN-03 : RD: KININP AND KINOUT WORKS IN ANY VERSION NOW
CC               13-AUG-03 : RS: SATELLITE ANTENNA PATTERN EST. OPTIONS
CC               10-NOV-03 : RS: ALLOW SIMULTANEOUS ESTIMATION OF SATELLITE
CC                               ANTENNA OFFSETS AND PATTERNS, ADD WARNINGS
CC                               FOR SIMULTANEOUS EST. OF ANTENNA PARAMETERS
CC               03-JAN-05 : RD: SET CORSTR TO CORRECT FOR EPOCH PARAMETERS
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               07-JUN-06 : RD: COMPUTE NO SOLUTION
CC               04-OCT-06 : AG: SATELLITE SPECIFIC ANTENNA PCO/PCV IMPLEMENTED
CC               13-NOV-06 : AG: INITIALISATION FOR IFREQ MOVED
CC               05-MAY-08 : DT: IDIFF AS INPUT PARAMETER;
CC                               NO CHECK FOR AMBIGUITIES IF SLR OBSERVATIONS
CC               04-MAY-09 : RD: SCALING OF LOADING MODELS ADDED TO NEQ%MISC
CC               09-MAY-09 : RD: CHECK APRIORI VALUES FOR FREQ-DEP. CODE BIAS
CC               11-OCT-12 : RD: ONLY ONE AMB. PER ITERATION FOR GLONASS AMBRES
CC               11-OCT-12 : RD: USE M_BERM WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1990     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, lfnerr
      USE m_global, ONLY: g_svnsys
      USE d_clkrnx, ONLY: t_clkhead
      USE d_const,  ONLY: PI, WGTCOD, WGTPHA
      USE d_satfil, ONLY: typeMWTR
      USE d_grid,   ONLY: grdNeq
      USE d_neq,    ONLY: t_misc
C
      USE s_chkion
      USE s_exitrc
      USE s_gtflna
      USE s_dcbcor
      USE s_gtsensor
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IELE  , IELETL, IELPAR, IEPPAR, IFIL  , IMOD1 ,
     1          IMOD2 , IOFR  , IOK   , IOK2  , IONTYP, IQIFOK, IRCIRS,
     2          IRCKIN, IRCLK1, IRCLK2, IRCNEQ, IRCORB, IRCRES, IRCRPR,
     3          IRESID, ITYP  , MAXTYP, MELWUB, MXCFRQ, NFIX  , NFTOT ,
     4          NIOREQ, NKIN  , NORB  , NORB2 , NRQOFF, NSAMPL, NOSOL ,
     5          IRCDCB, NCLREQ, NALLSAT,NALLSTA,ISAT  , JSAT  , ICLK  ,
     6          ISTA  , IFRQ  , JFRQ  , IDIFF
C
      REAL*8    ZENMAX, XXX0  , XXX2   , TIMPAR
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      TYPE(t_clkhead) CLKHED
      TYPE(t_misc)    MISC
C
      CHARACTER*32 FILNAM
      CHARACTER*16 ALLSTANAME(*)
      CHARACTER*6  MXNFRQ
C
      REAL*8       SIGDIP(*),POLGIM(3,*),EPOGIM(2,*),CLFRTO(2,*)
      REAL*8       SIGAMB(*)
C
      INTEGER*4    STRAMB(*),AR2MOD,OPTELI(*),OPTPAR(*),OPTGIM(*)
      INTEGER*4    CORSTR,OPTDIP(*),NFRFIL(*),ICARR(MXCFRQ,*)
      INTEGER*4    GRPOFF(*),SEQORB(*),SEQORB2(*),ITRGRD(*),PRIOPT(*)
      INTEGER*4    NANSPV,NANOFF,IFREQ,NANRAO,NANCAL
      INTEGER*4    ISTCLK(*),IBIAS(*),ALLSATNUM(*),ALLSTANUM(*)
C
      LOGICAL      YESNO
C
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
C
C
C CHECK WHETHER WEIGHT OF PHASE OBSERVABLE IS 1
C ---------------------------------------------
      IF (WGTPHA.NE.1.D0) THEN
        WRITE(LFNERR,900) WGTPHA,WGTCOD
900     FORMAT(/,' ### SR CHKOPT: WEIGHT OF PHASE OBSERVABLE IS NOT 1.'
     1    /,16X,'SIGMA OF UNIT WEIGHT HAS TO BE INTERPRETED ',
     2    'ACCORDINGLY.',
     3    /,16X,'WEIGHT OF PHASE OBSERVABLE: ',D9.2,
     4    /,16X,'WEIGHT OF CODE  OBSERVABLE: ',D9.2,/)
      ENDIF
C
C ONLY ONE AMBIGUITY PER ITERATION IS ALLOWED AS SOON AS GLONASS IS INVOLVED
C --------------------------------------------------------------------------
      IF (stramb(3).EQ.0.OR.stramb(3).EQ.2) THEN
        IF ((stramb(1).EQ.3.AND.IDNINT(sigamb(4)).NE.1) .OR.
     1      (stramb(1).EQ.4.AND.IDNINT(sigamb(4)).NE.1)) THEN
          DO ISAT=1,NALLSAT
            IF (ALLSATNUM(ISAT).GT.100.AND.ALLSATNUM(ISAT).LT.200) THEN
              WRITE(LFNERR,'(/,A,A,/,16X,A,/)')
     1        ' ### SR CHKOPT: ONLY ONE AMBIGUITY CAN BE RESOLVED PER ',
     2        'ITERATION','IF GLONASS AMBIGZUITIES ARE RESOLVED'
              IF (stramb(1).EQ.3) sigamb(4)=1D0
              IF (stramb(1).EQ.4) sigamb(4)=1D0
              EXIT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
C
C
C AMBIGUITY PRE-ELIMINATION
C -------------------------
      IF (STRAMB(1).EQ.-1) THEN
        OPTELI(4)=1
C
        IF (STRAMB(2).GT.0.AND.NSAMPL.GT.0) THEN
          IF (MOD(STRAMB(2),NSAMPL).NE.0) THEN
            WRITE(LFNERR,110) NSAMPL
110         FORMAT(/,' ### SR CHKOPT: AMBIGUITY PRE-ELIMINATION IS ',
     1        'PERFORMED EVERY ',I3,' SEC'
     2        /,16X,'TO BE COMPATIBLE WITH THE SAMPLING RATE',/)
C
            STRAMB(2)=NSAMPL
          ENDIF
        ENDIF
C
        IF (STRAMB(2).EQ.0) STRAMB(2)=NSAMPL
      ENDIF
C
C PRE-ELIMINATION OF DIFFERENTIAL IONOSPHERE PARAMETER
C ----------------------------------------------------
      IF (OPTELI(17).EQ.3) OPTDIP(1)=2
C
      IF (SIGDIP(1).EQ.0.D0 .AND. OPTDIP(1).GT.0) THEN
        WRITE(LFNERR,971)
971     FORMAT(/,' ### SR CHKOPT: STOCH. IONOSPHERE PARAMETERS ARE ',
     1           'NOT CONSTRAINED.',
     2         /,16X,'YOU RUN THE RISK OF GETTING A SINGULAR NORMAL',
     3         /,16X,'EQUATION MATRIX. SEE PARAMETER STATISTICS.',/)
      ENDIF
C
      IF (SIGDIP(2).NE.0.D0 .AND. (OPTDIP(1).EQ.2 .OR.
     1                (OPTDIP(1).EQ.1 .AND. OPTDIP(2).EQ.1))) THEN
        WRITE(LFNERR,941)
941     FORMAT(/,' ### SR CHKOPT: RELATIVE SIGMAS FOR STOCH. ',
     1           'IONOSPHERE PARAMETERS',
     2         /,16X,'MAY ONLY BE APPLIED IF THESE PARAMETERS ARE ',
     3         /,16X,'NOT PRE-ELIMINATED AND REFERENCE PARAMETERS ',
     4         /,16X,'ARE NOT ELIMINATED:',
     5         /,16X,'RELATIVE SIGMAS WERE NOT APPLIED',/)
        SIGDIP(2)=0.D0
      ENDIF
C
C CKECK SAMPLING RATE WHEN ANALYZING DTEC LC
C ------------------------------------------
      IF (MELWUB.EQ.2 .AND. NSAMPL.EQ.0) THEN
        WRITE(LFNERR,410)
410     FORMAT(/,' *** SR CHKOPT: SAMPLING RATE MUST BE SPECIFIED ',
     1    'WHEN ANALYZING DTEC LC',/)
        CALL EXITRC(2)
      ENDIF
C
C GET TYPE CODE OF IONOSPHERE MODELS
C ----------------------------------
      CALL CHKION(IONTYP)
C
C CHECK OPTIONS FOR GLOBAL IONOSPHERE MODEL PARAMETERS
C ----------------------------------------------------
      IF (MELWUB.EQ.2) OPTGIM(10)=2
C
C MAXIMUM ORDER OF SPHERICAL HARMONICS
      IF (OPTGIM(7).GT.0.AND.
     1    OPTGIM(2).GT.OPTGIM(1)) THEN
        WRITE(LFNERR,310) OPTGIM(1)
310     FORMAT(/,' ### SR CHKOPT: GLOBAL IONOSPHERE MODEL ',
     1         'PARAMETERS.',
     2         /,16X,'MAXIMUM ORDER OF SPHERICAL HARMONICS MUST BE',
     3         /,16X,'LESS OR EQUAL THAN MAXIMUM DEGREE.',
     4         /,16X,'MAXIMUM ORDER RESET TO ',I2,'.',/)
C
        OPTGIM(2)=OPTGIM(1)
      END IF
C
C MAPPING FUNCTION
      IF (OPTGIM(7).GT.0 .AND.
     1   (OPTGIM(8).LT.0 .OR. OPTGIM(8).GT.3)) THEN
        WRITE(LFNERR,315) OPTGIM(8)
315     FORMAT(/,' ### SR CHKOPT: GLOBAL IONOSPHERE MODEL ',
     1         'PARAMETERS.',
     2         /,16X,'MAPPING FUNCTION (',I1,') NOT IMPLEMENTED.',
     3         /,16X,'MAPPING FUNCTION (1) SELECTED.',/)
C
        OPTGIM(8)=1
      ENDIF
C
C ESTIMATION OF SINGLE-LAYER HEIGHT
      IF (OPTGIM(5).GT.0 .AND. OPTGIM(10).EQ.1) THEN
        IF (IONTYP.GE.2) THEN
          OPTPAR(19)=1
        ELSE
          OPTGIM(5)=0
C
          WRITE(LFNERR,330)
330       FORMAT(/,' ### SR CHKOPT: GLOBAL IONOSPHERE MODEL ',
     1           'PARAMETERS.',
     2           /,16X,'ESTIMATION OF SINGLE-LAYER HEIGHT ',
     3           'IS POSSIBLE ONLY',
     4           /,16X,'IF A PRIORI IONOSPHERE MODELS ',
     5           'ARE APPLIED.',/)
        END IF
      ELSE
        OPTGIM(5)=0
      END IF
C
C STATION-SPECIFIC IONOSPHERE MODELS
      IF (OPTGIM(9).EQ.1) THEN
        IF (OPTGIM(7).GT.1 .OR.
     1      OPTGIM(6).NE.1) THEN
          WRITE(LFNERR,335)
335       FORMAT(/,' ### SR CHKOPT: GLOBAL IONOSPHERE MODEL ',
     1      'PARAMETERS.',
     2      /,16X,'ONLY ONE STATIC MODEL PER STATION ALLOWED.',/)
          CALL EXITRC(2)
        ENDIF
      ENDIF
C
C COORDINATES OF GEOMAGNETIC POLE
      IF (OPTGIM(7).GT.0.AND.
     1    OPTGIM(3).EQ.2.AND.
     2    POLGIM(2,1).EQ.0.D0.AND.
     3    POLGIM(3,1).EQ.0.D0) THEN
        WRITE(LFNERR,320)
320     FORMAT(/,' *** SR CHKOPT: GLOBAL IONOSPHERE MODEL ',
     1         'PARAMETERS.',
     2         /,16X,'COORDINATES OF GEOMAGNETIC POLE NOT FOUND.',/)
        CALL EXITRC(2)
      END IF
C
C MINIMUM NUMBER OF EPOCH-SPECIFIC MODELS
      IF (OPTGIM(7).EQ.1.AND.
     1    OPTGIM(6).EQ.2) THEN
        WRITE(LFNERR,360)
360     FORMAT(/,' *** SR CHKOPT: GLOBAL IONOSPHERE MODEL ',
     1         'PARAMETERS.',
     2         /,16X,'AT LEAST TWO REFERENCE EPOCHS ',
     3         'MUST BE SPECIFIED.',/)
        CALL EXITRC(2)
      END IF
C
C CHRONOLOGICAL ORDER OF EPOCH-SPECIFIC MODELS
      IF (OPTGIM(7).GT.0.AND.
     1    OPTGIM(6).EQ.2) THEN
        DO 370 IMOD2=2,OPTGIM(7)
          IMOD1=IMOD2-1
          IF (EPOGIM(1,IMOD2).LE.EPOGIM(1,IMOD1)) THEN
            WRITE(LFNERR,380)
380         FORMAT(/,' *** SR CHKOPT: GLOBAL IONOSPHERE MODEL ',
     1             'PARAMETERS.',
     2             /,16X,'REFERENCE EPOCHS MUST BE GIVEN ',
     3             'IN CHRONOLOGICAL ORDER.',/)
            CALL EXITRC(2)
          END IF
C
          EPOGIM(2,IMOD1)=0.D0
          EPOGIM(2,IMOD2)=0.D0
370     CONTINUE
      END IF
C
C IONOSPHERE MODEL PARAMETERS
C ---------------------------
      IF (OPTPAR( 7).GT.0.AND.
     1    OPTPAR(19).GT.0) THEN
        WRITE(LFNERR,340)
340     FORMAT(/,' *** SR CHKOPT: SIMULTANEOUS ESTIMATION OF LOCAL ',
     1         'AND GLOBAL',
     2         /,16X,'IONOSPHERE MODEL PARAMETERS NOT ALLOWED.',/)
        CALL EXITRC(2)
      END IF
C
C IONOSPHERE MODEL PARAMETER PRE-ELIMINATION
C ------------------------------------------
      CALL GTFLNA(0,'IONOSRS',FILNAM,IRCIRS)
      IF (IRCIRS.EQ.0) THEN
        IF (OPTELI( 7).EQ.1.OR.
     1      OPTELI(19).EQ.1) THEN
          WRITE(LFNERR,350)
350       FORMAT(/,' ### SR CHKOPT: IONOSPHERE MODEL PARAMETERS ',
     1           'CANNOT BE SAVED',/,
     2           16X,'IF THEY ARE PRE-ELIMINATED BEFORE INVERSION.',/,
     3           16X,'PARAMETER PRE-ELIMINATION SUPPRESSED.',/)
C
          OPTELI( 7)=0
          OPTELI(19)=0
        END IF
      END IF
C
C CHECK EPOCH-WISE PRE-ELIMINATION
C --------------------------------
      DO 220 ITYP=1,MAXTYP
        IF (OPTELI(ITYP).EQ.3 .AND.
     1      ITYP.NE.17        .AND.
     2      ITYP.NE.21        .AND.
     3      ITYP.NE.22        .AND.
     3      ITYP.NE.23        .AND.
     4      ITYP.NE.24             )  THEN
          WRITE(LFNERR,921) ITYP
921       FORMAT(/,' ### SR CHKOPT: ONLY IONOSPHERE, KINEMATIC AND ',
     1             'STATION AND SATELLITE CLOCK PARAMETERS',
     2           /,16X,'MAY BE PRE-ELIMINATED EPOCH-WISE:',
     3           /,16X,'PARAMETER TYPE',I3,' IS PRE-ELIMINATED ',
     4             'BEFORE INVERSION',/)
          OPTELI(ITYP)=1
        ENDIF
220   CONTINUE
C
C CHECK IF OTHER PARAMETERS THAN COORDINATES AND AMBIGUITIES
C ARE ESTIMATED WITH AMBIG. RESOLUTION STRATEGY 2, AR2MOD=1
C AND NOT PRE-ELIMINATED BEFORE INVERSION OR EPOCH-WISE
C ----------------------------------------------------------
      IELPAR=0
      DO 210 ITYP=1,MAXTYP
        IF (ITYP.EQ.1 .OR. ITYP.EQ.4) GOTO 210
        IF ((OPTELI(ITYP).EQ.0 .OR. OPTELI(ITYP).EQ.2) .AND.
     1       OPTPAR(ITYP).NE.0) IELPAR=1
210   CONTINUE
      IF (STRAMB(1).EQ.2 .AND. AR2MOD.EQ.1 .AND. IELPAR.EQ.1) THEN
        WRITE(LFNERR,901)
901     FORMAT(/,' *** SR CHKOPT: ONLY COORDINATES AND AMBIGUITY ',
     1           'PARAMETERS',
     2         /,16X,'MAY BE ESTIMATED WHEN USING THE SEARCH OPTION',
     3         /,16X,'FOR AMBIGUITY RESOLUTION IN THE BASELINE-WISE',
     4         /,16X,'MODE. USE NETWORK-WISE MODE OR PRE-ELIMINATE ',
     5         /,16X,'ADDITIONAL PARAMETER TYPES BEFORE INVERSION',/)
        CALL EXITRC(2)
      ENDIF
C
C BASELINE-WISE AMBIGUITY RESOLUTION WITH STRATEGY 2
C --------------------------------------------------
      IF (STRAMB(1).EQ.2 .AND. AR2MOD.EQ.1 .AND. CORSTR.NE.1) THEN
        WRITE(LFNERR,922)
922     FORMAT(/,' ### SR CHKOPT: ONLY CORRELATIONS WITHIN BASELINE ',
     1           'ALLOWED',
     2         /,16X,'FOR AMBIGUITY RESOLUTION WITH GENERAL SEARCH ',
     3         /,16X,'STRATEGY USING THE BASELINE-WISE MODE:',
     4         /,16X,'CORRELATION MODE SET TO "BASELINE"',/)
        CORSTR=1
      ENDIF
C
C CHECK OPTIONS CONCERNING QIF AMBIGUITY RESOLUTION STRATEGY
C ----------------------------------------------------------
      IF (STRAMB(1).EQ.4 .AND. OPTDIP(1).EQ.0) THEN
        WRITE(LFNERR,951)
951     FORMAT(/,' *** SR CHKOPT: STOCHASTIC IONOSPHERE PARAMETERS ',
     1           'HAVE TO BE',
     2         /,16X,'ESTIMATED WHEN USING THE QIF STRATEGY',/)
        CALL EXITRC(2)
      ENDIF
C
      IF (STRAMB(1).EQ.4) THEN
        IQIFOK=1
        DO 260 IFIL=1,NFTOT
          IF (NFRFIL(IFIL).NE.2) THEN
            IQIFOK=0
          ELSE IF(ICARR(1,IFIL).NE.1 .OR. ICARR(2,IFIL).NE.2) THEN
            IQIFOK=0
          ENDIF
260     CONTINUE
C
        IF (IQIFOK.EQ.0) THEN
          WRITE(LFNERR,961)
961       FORMAT(/,' *** SR CHKOPT: PHASE L1 AND L2 OBSERVATIONS ',
     1             'HAVE TO BE',
     2           /,16X,'PROCESSED SIMULTANEOUSLY WHEN USING THE QIF ',
     3             'STRATEGY',/)
          CALL EXITRC(2)
        ENDIF
      ENDIF
C
C GET RESIDUAL FILENAME (FILNAME = BLANK: NO SAVE)
C ------------------------------------------------
      CALL GTFLNA(0,'RESIDRS',FILNAM,IRCRES)
      CALL GTFLNA(0,'KINOUT',FILNAM,IRCKIN)
      CALL GTFLNA(0,'CLKSAV',FILNAM,IRCLK1)
      CALL GTFLNA(0,'CLKRNX',FILNAM,IRCLK2)
      IF(IRCRES.NE.0.AND.IEPPAR.NE.1) THEN
        IRESID=0
C SOLUTION OF EPOCH PARAMETERS IS OUT OF INTEREST
      ELSE IF (IRCKIN.NE.0.AND.IRCLK1.NE.0.AND.IRCLK2.NE.0.AND.
     1         PRIOPT(12).EQ.1.AND.IRCRES.NE.0) THEN
        IRESID=0
      ELSE
        IRESID=1
      ENDIF
C
      DO 10 ITYP=1,MAXTYP
        IF (IRCRES.EQ.0 .AND. OPTELI(ITYP).EQ.1) THEN
          IRESID=0
          WRITE(LFNERR,902)
902       FORMAT(/,' ### SR CHKOPT: NO RESIDUALS AVAILABLE FOR ',
     1             'OUTPUT',/,
     2             16X,'SINCE AMBIGUITIES OR OTHER PARAMETERS',/,
     3             16X,'ARE PRE-ELIMINATED BEFORE INVERSION',/)
          GOTO 20
        ELSE IF (IRESID.EQ.1 .AND. OPTELI(ITYP).EQ.1) THEN
          IRESID=0
          WRITE(LFNERR,'(/,A,2(/,16X,A),/)')
     1         ' ### SR CHKOPT: NO SOLUTION FOR EPOCH PARAMETERS IS ' //
     2                         'AVAILABLE',
     3                         'SINCE AMBIGUITIES OR OTHER PARAMETERS',
     4                         'ARE PRE-ELIMINATED BEFORE INVERSION'
          GOTO 20
        ENDIF
10    CONTINUE
20    CONTINUE
C
C EPOCH-WISE PREELIMINATION AND RESUBSTITUTION IS ONLY PERMITTED WITH
C CORRECT CORRELATIONS:
C -------------------------------------------------------------------
      IF ((OPTELI(21).EQ.3.OR.OPTELI(23).EQ.3.OR.OPTELI(24).EQ.3).AND.
     1    IRESID.EQ.1.AND.CORSTR.NE.3) THEN
        WRITE(LFNERR,'(/,A,2(/,16X,A),/)')
     1        ' ### SR CHKOPT: THE SOLUTION FOR EPOCH PARAMETERS IS ' //
     2                         'ONLY AVAILABLE',
     3                        'FOR THE CORRECT CORRELATION MODE.',
     4                        'CORRELATION MODE SET TO "CORRECT"'
        CORSTR=3
      ENDIF
C
C LOCAL IONOSPHERE MODEL PARAMETERS
C ---------------------------------
      IF(IONTYP.NE.1.AND.
     1   NIOREQ.GT.0) THEN
        WRITE(LFNERR,903)
903     FORMAT(/,' *** SR CHKOPT: NO IONOSPHERE MODEL FILE ',
     1         'ENTERED.',/,
     2         16X,'LOCAL IONOSPHERE MODEL PARAMETERS ',
     3         'CANNOT BE ESTIMATED.',/)
        CALL EXITRC(2)
      ENDIF
C
C SAVE OF ORBITAL ELEMENTS: CHECK IF RAD.PRESS.COEFF. FILENAME AVAILABLE
C ----------------------------------------------------------------------
      CALL GTFLNA(0,'ORBITRS',FILNAM,IRCORB)
      IF(IRCORB.EQ.0) THEN
        CALL GTFLNA(0,'RPRCOE ',FILNAM,IRCRPR)
        IF(IRCRPR.NE.0) THEN
          WRITE(LFNERR,904)
904       FORMAT(/,' *** SR CHKOPT: NO RAD.PRESSURE COEFF. FILENAME',
     1                             ' ENTERED',/,
     2                   16X,'--> ORBITAL ELEMENTS CAN NOT BE SAVED',/)
          CALL EXITRC(2)
        ENDIF
      ENDIF
C
C SOLVE FOR DYNAMICAL PARAMETERS AND STORE NEQ
C --------------------------------------------
      CALL GTFLNA(0,'NEQUARS',FILNAM,IRCNEQ)
      IF(IRCNEQ.EQ.0) THEN
        IOK=0
        DO I=1,NORB
          IF (SEQORB(I).GE.7.AND.IOK.EQ.0) IOK=1
        ENDDO
        IF (IOK.EQ.1) THEN
          CALL GTFLNA(0,'RPRCOE ',FILNAM,IRCRPR)
          IF(IRCRPR.NE.0) THEN
            WRITE(LFNERR,910)
910         FORMAT(/,' *** SR CHKOPT: WARNING: NO GPS RAD.PRESSURE ',
     1                     'COEFF. FILENAME ENTERED',/,
     2                     16X,'--> NEQ STACKING MAY NEED A RAD. ',
     3                     'PRESSURE FILE FOR GPS ORBIT COMBINATION',/)
          ENDIF
        ENDIF
C
C CHECK FOR LEO RAD. PRESSURE FILE
C --------------------------------
        IOK2=0
        DO I=1,NORB2
          IF (SEQORB2(I).GE.7.AND.IOK2.EQ.0) IOK2=1
        ENDDO
        IF (IOK2.EQ.1) THEN
          CALL GTFLNA(0,'LEORPR ',FILNAM,IRCRPR)
          IF (IRCRPR.NE.0) THEN
            WRITE(LFNERR,915)
915         FORMAT(/,' *** SR CHKOPT: WARNING: NO LEO RAD.PRESSURE ',
     1                     'COEFF. FILENAME ENTERED',/,
     2                     16X,'--> NEQ STACKING MAY NEED A RAD. ',
     3                     'PRESSURE FILE FOR LEO ORBIT COMBINATION',/)
          END IF
        ENDIF
      ENDIF
C
C WARNING: IF ORBITS SHOULD BE PREELIMINATED,
C BUT AN ELE-FILE IS SPECIFIED
C -------------------------------------------
      IF(IRCORB.EQ.0) THEN
        IF (OPTELI(3).EQ.1 .OR. OPTELI(11).EQ.1) THEN
            WRITE(LFNERR,905)
905         FORMAT(/,' ### SR CHKOPT: NO ELE-FILE WILL BE SAVED',/,
     1                       16X,'SINCE ORBIT PARAMETERS ARE PRE-',
     2                           'ELIMINATED BEFORE INVERSION',/)
          ENDIF
100     CONTINUE
      ENDIF
C
C IF MELBOURNE-WUEBBENA COMBINATION USED CORRELATION WITHIN BASELINE ONLY
C -----------------------------------------------------------------------
      IF (CORSTR.NE.1 .AND. MELWUB.GE.1) THEN
        CORSTR=1
        WRITE(LFNERR,907)
907     FORMAT(/,' ### SR CHKOPT: FOR MELBOURNE-WUEBBENA AND DTEC ',
     1    'LINEAR COMBINATION',
     2    /,16X,'ONLY CORSTR=1 ALLOWED',/)
      ENDIF
C
C ERROR: IF SATELLITE ANTENNA OFFSETS REQUESTS ARE LARGER THAN GROUPS:
C        COULD NOT BE SAVED IN NORMAL EQUATIONS
C --------------------------------------------------------------------
      CALL GTFLNA(0,'NEQUARS',FILNAM,IRCNEQ)
      IF(IRCNEQ.EQ.0) THEN
C
C WHEN SAVING NORMAL EQUATIONS AMBIGUITIES HAVE TO BE PRE-ELIMINATED
C
        IF (OPTELI(4).EQ.0 .AND. IDIFF.LE.2) THEN
          WRITE(LFNERR,911)
911       FORMAT(/,' ### SR CHKOPT: AMBIGUITIES HAVE TO BE PRE-ELIMI',
     1            'NATED (BEFORE OR AFTER INVERSION) IF YOU WANT',
     2     /,16X,'TO SAVE NORMAL EQUATIONS !',
     3     /,16X,'RE-RUN THIS JOB WITH THE OPTION "PRE-ELIMIN',
     4                        'ATE" FOR',
     5     /,16X,'AMBIGUITY PARAMETERS (BI OR AI) OR DO NOT ',
     6           'USE THE NORMAL EQUATION',
     7     /,16X,'FILE PRODUCED IN THIS RUN!',/)
        ENDIF
C
        IF (OPTELI(12).EQ.2) GOTO 150
C
        IF (NRQOFF.EQ.0) GOTO 150
C
        IF (NRQOFF.LE.NANOFF) THEN
          YESNO = .FALSE.
          DO 120 IOFR=1,NRQOFF
            IF (GRPOFF(IOFR).NE.IOFR .AND. GRPOFF(IOFR) /= 0)
     1                                                    YESNO = .TRUE.
120       CONTINUE
          IF (.NOT.YESNO) GOTO 150
        ENDIF
C
        WRITE(LFNERR,908)NRQOFF,NANOFF
908     FORMAT(/,' *** SR CHKOPT: NORMAL EQUATIONS CANNOT ',
     1  'BE SAVED!',/,
     2  16X,'REQUESTS NOT CONSISTENT WITH GROUPS.',/,
     3  16X,'USE PREELIMINATION OPTION FOR NORMAL EQUATIONS!',/,
     4  16X,' NRQOFF: ',I4,/,
     5  16X,' NANOFF: ',I4,/)
        CALL EXITRC(2)
C
      ENDIF
      IF(IRCNEQ.EQ.0 .AND. SIZE(grdNeq) /= SIZE(misc%grdNeq)-1)THEN
        WRITE(LFNERR,'(/,A,2(/,16X,A),/)')
     1  ' *** SR CHKOPT: SCALING FACTORS OF THE VIENNA GRID FILES.',
     2  'THE SIZE OF THE KEYWORD RECORDS DIFFERS BETWEEN THE MODULES',
     3  '"${I}/D_NEQ.f90" AND "${I}/D_GRID.f90".'
        CALL EXITRC(2)
      ENDIF
150   CONTINUE
C
C WARNING IF NORMAL EQUATION ARE STORED AND COORDINATES ARE FIXED
C ---------------------------------------------------------------
      IF(IRCNEQ.EQ.0.AND.NFIX.GT.0) THEN
        WRITE(LFNERR,909)
909     FORMAT(/,' ### SR CHKOPT: NORMAL EQUATIONS ARE ',
     1  'SAVED WITHOUT FIXED COORDINATES!',/)
      ENDIF
C
C ERROR IF NORMAL EQUATION ARE STORED AND TROP. GRADIENTS ESTIMATED
C -----------------------------------------------------------------
ccc      IF (IRCNEQ.EQ.0 .AND. ITRGRD(1).NE.0 .AND. OPTELI(6).EQ.0) THEN
ccc        WRITE(LFNERR,912)
ccc912     FORMAT(/,' *** SR CHKOPT: NORMAL EQUATIONS CANNOT BE SAVED',
ccc     1    ' IF TROPOSPHERIC',
ccc     2    /,16X,'GRADIENT PARAMETERS ARE ESTIMATED,',
ccc     3    /,16X,'BUT NOT PRE-ELIMINATED (AFTER INVERSION).',/)
ccc        CALL EXITRC(2)
ccc      ENDIF
C
C WARNING IF ELEVATION CUT-OFF ANGLE TOO SMALL
C ------------------------------------------
      IELE=IDNINT(90.D0-180.D0/PI*ZENMAX)
      IELETL=3
C
      IF (IELE.LT.IELETL) THEN
        WRITE(LFNERR,913) IELETL,IELE
913     FORMAT(/,' ### SR CHKOPT: ELEVATION CUT-OFF ANGLE ',
     1    'DANGEROUSLY LOW.',
     2    /,16X,'SOME TROPOSPHERIC MAPPING FUNCTIONS MAY GIVE ',
     3    'UNRELIABLE',
     4    /,16X,'RESULTS BELOW ',I3,' DEGREES.',
     5    /,16X,'ELEVATION CUT-OFF ANGLE: ',I3,' DEGREES',/)
      ENDIF
C
C CONDITION OF SUM FOR REFERENCE CLOCKS
C -------------------------------------
      IF (CLKHED%NUMREF.EQ.2) THEN
        IF (OPTELI(23).NE.OPTELI(24)) THEN
          WRITE(LFNERR,914)
914       FORMAT(/,' ### SR CHKOPT: IF YOU WANT TO USE CONDITION OF',
     1                 ' SUM FOR REFERENCE CLOCKS',
     2           /,16X,'YOU SHOULD SELECT THE SAME PREELIMINATION ',
     3                 'STRATEGY FOR',
     4           /,16X,'(EPOCHWISE) SATELLITE AS WELL AS FOR STATION',
     5                 ' CLOCKS.',/)
        ENDIF
      ENDIF
C
C SATELLITE ANTENNA PHASE PATTERNS AND FREQUENCY
C ----------------------------------------------
      IFREQ=ICARR(1,1)
      IF (NANSPV.NE.0) THEN
C        IFREQ=ICARR(1,1)
        DO IFIL=1,NFTOT
          IF (NFRFIL(IFIL).GT.1) THEN
            WRITE(LFNERR,916) IFIL,NFRFIL(IFIL)
916         FORMAT(/,' *** SR CHKOPT: IT IS NOT POSSIBLE TO',
     1               ' ESTIMATE SATELLITE ANTENNA PATTERNS',
     2         /,16X,'FOR MORE THAN ONE FREQUENCY/LINEAR',
     3               ' COMBINATION!',
     4         /,16X,'FILE       :',I4,
     5         /,16X,'# OF FREQU.:',I4,/)
            CALL EXITRC(2)
          ENDIF
          IF (ICARR(1,IFIL) .NE. IFREQ) THEN
            WRITE(LFNERR,917) IFIL,ICARR(1,IFIL)
917         FORMAT(/,' *** SR CHKOPT: IT IS NOT POSSIBLE TO',
     1               ' ESTIMATE SATELLITE ANTENNA PATTERNS',
     2         /,16X,'FOR MORE THAN ONE FREQUENCY/LINEAR',
     3               ' COMBINATION!'
     4         /,16X,'FREQUENCY OF THE FOLLOWING FILE DOES NOT',
     5               ' AGREE WITH THE FREQUENCY',
     6         /,16X,'OF THE FIRST FILE.',
     7         /,16X,'FILE     :',I4,
     8         /,16X,'FREQUENCY:',I4,/)
            CALL EXITRC(2)
          ENDIF
        ENDDO
      ENDIF
C
C WARNING IF SIMULTANEOUS ESTIMATION OF DIFFERENT ANTENNA PARAMETERS
C ------------------------------------------------------------------
      IF ((NANRAO.NE.0 .OR. NANCAL.NE.0) .AND.
     1    (NANOFF.NE.0 .OR. NANSPV.NE.0)) THEN
        WRITE(LFNERR,918)
918     FORMAT(/,' ### SR CHKOPT: SIMULTANEOUS ESTIMATION OF ',
     1             'RECEIVER AND SATELLITE ANTENNA PARAMETERS',
     2       /,16X,'DANGEROUS DUE TO A HIGH CORRELATION!',/)
      ENDIF
C
      IF ((NANRAO.NE.0 .OR. NANOFF.NE.0) .AND.
     1    (NANCAL.NE.0 .OR. NANSPV.NE.0)) THEN
        WRITE(LFNERR,919)
919     FORMAT(/,' ### SR CHKOPT: SIMULTANEOUS ESTIMATION OF ',
     1             'ANTENNA OFFSET AND PATTERN PARAMETERS',
     2       /,16X,'DANGEROUS DUE TO A HIGH CORRELATION!',/)
      ENDIF
C
C KIN COORD WITH INPUT/OUTPUT FILE
C --------------------------------
c      IF (NKIN.GT.0.AND.OPTELI(21).NE.3) THEN
c        CALL GTFLNA(0,'KININP ',FILNAM,IRCKIN)
c        CALL GTFLNA(0,'KINOUT ',FILNAM,IRCKOU)
c        IF (IRCKIN.EQ.0.OR.IRCKOU.EQ.0) THEN
c          WRITE(LFNERR,'(/,A,2(/,16X,A),/)')
c     1    ' ### SR CHKOPT: THE USE OF INPUT AND OUTPUT FILES FOR ',
c     2    'KINEMATIC COORDINATES IS ONLY POSSIBLE USING',
c     3    'THE EPOCH WISE PREELIMINATION FOR THIS PARAMETER TYPE.'
c        ENDIF
c      ENDIF
C
C
C NO SOLUTION OPTION: NO PREELIMINATION "PRIOR TO NEQ SAVING"
C -----------------------------------------------------------
      IF (NOSOL.eq.1) THEN
        DO ITYP=1,MAXTYP
          IF (OPTELI(ITYP).EQ.2) THEN
            WRITE(LFNERR,'(/,1X,A,/,2(16X,A,/))') '### SR CHKOPT: '//
     1           'THE PREELIMINATION "PRIOR_TO_NEQ_SAVING" IS',
     2           'IGNORED IF THE NEQ FILE IS DIRECTLY STORED',
     3           'AND NO SOLUTION IS COMPUTED.'
            EXIT
          ENDIF
        ENDDO
      ENDIF
C
C CHECK APRIORI VALUES FOR INTER-FREQUENCY CODE BIASES
C ----------------------------------------------------
      CALL GTFLNA(0,'DCBINP',FILNAM,IRCDCB)
      IF (IRCDCB.EQ.0) THEN
        CLKLOOP: DO ICLK=1,NCLREQ
          IF (IBIAS(ICLK).NE.1.AND.IBIAS(ICLK).NE.4) CYCLE
          TIMPAR=(CLFRTO(1,ICLK)+CLFRTO(2,ICLK))/2d0
C
          DO ISTA=1,NALLSTA
            IF(ALLSTANUM(ISTA).NE.ISTCLK(ICLK)) CYCLE
C
            DO ISAT=1,NALLSAT-1
              IF(G_SVNSYS(INT(ALLSATNUM(ISAT)/100)).NE.'R') CYCLE
              CALL gtsensor(ALLSATNUM(ISAT),TIMPAR,
     1                      type1=typeMWTR,IFRQ=IFRQ)
C
              CALL DCBCOR(2,0,ALLSATNUM(ISAT),ALLSTANAME(ISTA),'',0,
     1                    5,TIMPAR,XXX0)
              DO JSAT=ISAT+1,NALLSAT
                IF(G_SVNSYS(INT(ALLSATNUM(JSAT)/100)).NE.'R') CYCLE
                CALL gtsensor(ALLSATNUM(JSAT),TIMPAR,
     1                        type1=typeMWTR,IFRQ=JFRQ)
                IF (IFRQ.NE.JFRQ) CYCLE
C
                CALL DCBCOR(2,0,ALLSATNUM(JSAT),ALLSTANAME(ISTA),'',0,
     1                      5,TIMPAR,XXX2)
                IF (XXX0.NE.XXX2) THEN
                  WRITE(LFNERR,'(/,A,5(/,16X,A),/)')
     1            ' ### SR CHKOPT: YOU ARE GOING TO ESTIMATE ' //
     2                           ',FREQUENCY-SPECIFIC ',
     3            'INTER-FREQUENCY CODE BIASES WHEREAS YOU INTRODUCE ',
     4            'DIFFERENT A PRIORI VALUES FOR SATELLITES WITH ',
     5            'IDENTICAL FREQUENCIES. THIS INCONSISTENCY WILL ',
     6            'RESULT IN AN INCOMPLETE PROGRAM OUTPUT AND ',
     7            'UNUSEABLE NORMAL EQUATION FILES.'
                  EXIT CLKLOOP
                ENDIF
              ENDDO
            ENDDO
            EXIT
          ENDDO
        ENDDO CLKLOOP
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
