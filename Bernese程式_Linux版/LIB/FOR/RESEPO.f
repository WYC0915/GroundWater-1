      MODULE s_RESEPO
      CONTAINS

C*
      SUBROUTINE RESEPO(TITLE ,NFTOT ,STFIL ,STNAME,TIMREF,CSESS ,
     1                  NFRFIL,ICARR ,MEATYP,NEPOBS,NPAR  ,XXX   ,
     2                  INDP  ,INDA  ,ASING ,BSING ,WEIGHT,ISV121,
     3                  IFIL1 ,IOBNU1,IFRSE1,IDELTT,NSATEL,SATNUM,
     4                  NDIFF ,NORRES,SECIPL,CLKHED,CLKREC,NSTAT ,
     5                  XSTAT ,IRAUX2,AELL  ,BELL  ,DXELL ,DRELL ,
     6                  SCELL ,NSTWGT,ISTWGT,STWGT ,SIGAPR,IQXX  ,
     7                  IPHSEP,ANOR  ,IZEROD,RNXCLK,MAXFLS,MAXSAS,
     8                  MAXFRS,MAXPAR,MAXLOC,MAXSNG,MAXPAE,NEPSNG)
CC
CC NAME       :  RESEPO
CC
CC PURPOSE    :  1) COMPUTE EPOCH PARAMETERS (ZERO DIFF CLOCK ONLY)
CC               2) COMPUTE EPOCH RESIDUALS
CC               3) WRITE RESIDUALS TO OUTPUT FILE
CC
CC PARAMETERS :
CC         IN :  TITLE  : TITLE LINE                          CH*80
CC               NFTOT  : TOTAL NUMBER OF FILES               I*4
CC               STFIL(K,I),K=1,2, I=1,..,NFTOT: STATION NRS  I*4
CC                        INVOLVED IN FILE I
CC               STNAME(I),I=1,2,..: STATION NAMES            CH*16
CC               TIMREF(I),I=1,2,..,NFTOT: REFERENCE TIME     R*8
CC                        FOR FILE I
CC               CSESS(K,I),K=1,2, I=1,..,NFTOT:              CH*4
CC                        K=1: SESSION IDENTIFIER,
CC                        K=2: FILE IDENTIFIER IN SESSION
CC               NFRFIL(I),I=1,2,...,NFTOT: NUMBER OF REQ.    I*4
CC                        FREQUENCIES PER FILE
CC               ICARR(K,I),K=1,..,NFRFIL(I),I=1,..,NFTOT     I*4
CC                        K=1:FIRST REQ. FREQ, K=2: SECOND ...
CC               MEATYP(I),I=1,2,..,NFTOT: MEASUREMENT TYPE   I*4
CC                        IN FILE I
CC               NEPOBS(I): MIN: # OF OBS. FOR EPOCH PARAM.S  I*4(3)
CC                        (I=1 STA-CLK / I=2 SAT-CLK / I=3 KIN.)
CC               NPAR   : NUMBER OF PARAMETERS                I*4
CC               XXX(I),I=1,2,..,NPAR: SOLUTION VECTOR        R*8
CC               INDP(I),I=1,2,.. :AUX. ARRAY                 I*4
CC               INDA(I),I=1,2,.. :AUX. ARRAY                 I*2
CC               ASING(I),I=1,2,.. : NON ZERO ELEMENTS OF ALL R*8
CC                        DOUBLE DIFF OBS EQNS OF ONE EPOCH
CC               BSING(I),I=1,2,..: OBS-COMP FOR ALL OBSER-   R*8
CC                        VATIONS OF ONE EPOCH
CC               WEIGHT(I),I=1,2,..: WEIGHT MATRIX OF ALL SIM. R*8
CC                        OBS. EQNS
CC               ISV12(2,I),I=1,2,..: AUX. ARRAY              I*4
CC               IFIL(I),I=1,2,..: AUX. ARRAY                 I*4
CC               IOBNUM(I),I=1,2,..: AUX. ARRAY               I*4
CC               IFRSES(I),I=1,2,..: AUX. ARRAY               I*4
CC               IDELTT(I),I=1,2,..,NFTOT: SPACING BETWEEN    I*4
CC                        OBSERVATIONS (SEC)
CC               NSATEL(I),I=1,..,NFTOT: NUMBER OF SATELLITES I*4(*)
CC               SATNUM(K,I),K=1,..,NSATEL(I),I=1,..,NFTOT:   I*4
CC                        SATELLITE NUMBERS
CC               NDIFF(I):I=1,..,NFTOT: DIFFERENCE TYPE       I*4
CC                        NDIFF=0: ZERO DIFFERENCE
CC                        NDIFF=1: SINGLE DIFFERENCE
CC               NORRES:  FLAG FOR RESIDUAL NORMALIZATION     I*4
CC                        =1 REAL RESIDUALS
CC                        =2 NORALIZATION WEIGHT+APOST. COV
CC                        =3 NORALIZATION ON APRIORI WEIGHTS
CC               SECIPL : INTERVAL FOR CLOCK INTERPOLATION    R*8
CC               CLKHED : CLOCK HEADER INFORMATION            T_CLKHEAD
CC                          %NUMREF=0: FIX REF-CLOCKS
CC                          %NUMREF=2: SUM FOR REF-CLOCKS
CC               CLKREC : %NEPO: EPOCHS WITH HIGHEST SAMPL.   T_CLKREC
CC                        %EPOCH(1): LAST EPOCH
CC                                (SEC. SINCE CLKHED%TFIRST)
CC               NSTAT  : NUMBER OF STATIONS WITH FLAG IN     I*4
CC                        "FLAGS"
CC               XSTAT(3,I),I=1,..,NSTAT: RECTANGULAR STATION R*8
CC                        COORDINATES IN WGS-84 (METERS)
CC               IRAUX2 : EPOCH RESULT SCRATCH FILE IS        I*4
CC                        0: AVAILABLE / 1: NOT AVAIL.
CC               AELL,BELL: SEMI-MAJOR AND -MINOR AXIS OF     R*8
CC                        ELIPSOID
CC               DXELL(I),I=1,2,3: SHIFT TO WGS-84            R*8
CC               DRELL(I),I=1,2,3: ROTATIONS TO WGS-84        R*8
CC               SCELL  : SCALE TO WGS-84                     R*8
CC               NSTWGT : # STATIONS WITH A PRIORI WEIGHTS    I*4
CC                        FOR COORDINATES
CC               ISTWGT : NUMBERS OF THE STATION WITH A       I*4(1)
CC                        PRIORI WEIGHTS
CC               STWGT  : STWGT(K,I) A PRIORI WEIGHT FOR      R*8(3,1)
CC                        STATION I AND COORDINATE K
CC               SIGAPR : A PRIORI SIGMA                      R*8
CC               IQXX   : FLAG FOR VAR/COV COMP.(YES=1,NO=0)  I*4
CC               IPHSEP : ONLY PHASE FOR RESUBST OF EPO-PARAM. I*4
CC               ANOR(I),I=1,..,NPAR*(NPAR+1)/2 (N11)         R*8
CC               IZEROD : FLAG TO IDENTIFY CODE OR PHASE      I*4
CC                        ONLY (=2) AND CODE+PHASE (=1) SOLUTION
CC               RNXCLK:  WHAT TO DO IF NO INPUT CLOCK RINEX  I*4
CC                        FOR SATELLITE CLOCKS:
CC                         -1: IGNORE CLOCK RINEX FILE
CC                          0: TRY ALSO SAT CLK FILE
CC                          1: USE OBS. (INTERPOL. CLK RNX)
CC                          2: SKIP OBS.
CC                          3: USE OBS. (SAT CLK = ZERO)
CC
CC REMARKS    :  BASED ON SR RESOUT. DEVELOPED FOR ZERO DIFFERENCE
CC               ESTIMATION ONLY! MIGHT NOT WORK IN OTHER CASES.
CC               ACTIVE FILES: LFNERR, LFNPRT: NORMAL OUTPUT FILES
CC                             LFNRES: RESIDUAL INPUT FILE
CC                             LFNLOC: RESIDUAL OUTPUT
CC                    LFNAUX = LFN002: RESULT SCRATCH FILE FOR OUTPUT
CC                                       IN OTHER SRs
CC
CC
CC AUTHOR     :  T.A. SPRINGER
CC
CC VERSION    :  4.1  (MAY 97)
CC
CC CREATED    :  21-MAY-97
CC
CC CHANGES    :  21-MAY-97 : TS: CREATION
CC               07-MAY-98 : MR: CHANGE LOCQ TEST
CC               08-JUN-98 : TS: ADD NORMALIZED RESIDUALS OPTION
CC               27-JAN-00 : TS: ABUSE ISV12 FOR ELE AND AZI IF NDIFF=1
CC               27-JAN-00 : TS: CHANGES FOR CLOCK RINEX OUTPUT
CC               28-FEB-00 : TS: CHANGE OF SIGN OF XXX1 FOR STATIONS!
CC                               WE SHOULD CHECK THE STATION CLOCK SIGNS!
CC               30-JUN-00 : TS: TEST FOR KIN. STATIONS
CC               25-OCT-00 : RD: COUNT OBS. FOR CLOCKS
CC               22-NOV-00 : TS: HANDLE KIN. STATIONS
CC               27-NOV-00 : TS: SOME COSMETICS
CC               23-APR-01 : DS: HANDLE STATION TYPES
CC               23-APR-01 : DS: HANDLE LEO FILE "KINSCR"
CC               23-AUG-01 : DS: CONSTRAINTS FOR KIN. COORDINATES
CC               24-AUG-01 : MR: ADD A POSTERIORI RMS
CC               27-AUG-01 : MR: DETECTION OF EPOCH PARAMETERS CHANGED
CC               24-NOV-01 : DS: VAR/COV COMPUTATION
CC               20-FEB-02 : DS: HANDLE SPACEBORNE, AIRBORNE AND KINEMATIC
CC               15-MAY-01 : RD: USE F90 SR FOR WRITING THE CLK RINEX FILE
CC               27-AUG-01 : RD: INIT EPOCH FOR CLOCK RINEX RECORD
CC               22-JAN-02 : RD: CONDITION OF SUM FOR REFERENCE CLOCKS
CC               23-JAN-02 : RD: DO OUTPUT OF EPOCH PARAMETERS IN PRIEST
CC               25-JAN-02 : RD: WRITE ALL CLOCK RESULT FILES IN CLKSAV
CC               29-JAN-02 : RD: A GENERIC EPOCH COMPUTATION FOR EPOCH PARAM.
CC               09-FEB-02 : RD: WEIGHT REAL*4->REAL*8
CC               18-FEB-02 : RD: ADD CLKOBS TO THE CALL OF CLKSUM
CC               25-JUN-02 : RD/DS: MERGE VERSION BE AND MUNICH
CC               21-AUG-02 : RD: DETECTION OF PROBLEMS IN RESIDUAL NORMALIZATION
CC               27-AUG-02 : RD: HANDLE NEW FORMATTED RESIDUAL FILES
CC               26-SEP-02 : DS: CORRECT INITIALIZATION FOR WITING EPOCH-WISE
CC                               LOCQ, USE LCQ3 INSTEAD OF LCQ2
CC               26-SEP-02 : DS: IN ORDER TO SPEED UP RESEPO COMMENTED
CC                               WRITING OF REF. CLOCKS
CC               07-OCT-02 : DS: WRITE 1D20 WHEN SINGULAR COORD.
CC               13-NOV-02 : RD: DO NOT ALLOW SAME NAMES FOR KININP AND KINOUT
CC               29-Nov-02 : RD: NEW CALL OF SR CLKSUM
CC               28-Jan-03 : RD: NUMBER OF OBS. FOR KIN. POS.(CLKOBS->NEPOBS)
CC                               ADD IRAUX2
CC               30-JAN-03 : RD: HANDLE REF-AMB FROM PHASE BASELINE FILES
CC               17-FEB-03 : LM: USE M_MAXDIM, P_GPSEST
CC               08-MAR-03 : MR: MAXAMB MOVED TO M_MAXDIM
CC                           HU: UNUSED MAXxxx REMOVED
CC               19-MAR-03 : HU: REFERENCE CLOCK SECTION ACTIVATED AGAIN
CC               26-MAR-03 : RD: DO NOT READ SCRATCH FILE IF IT IS NOT OPEN
CC               11-APR-03 : RD: BETTER INIT OF LCQ2 FOR REFERNECE CLOCKS
CC               15-May-03 : HB: INITIALIZE STRUCTURE
CC               23-May-03 : RD: ALLOCATE SOME OF THE BIG LOCAL ARRAYS
CC               28-MAY-03 : RD: CORRECT SETTING OF RESHED - ARRAY
CC               05-JUN-03 : RD: USE THE CORRECT NUMBER OF EPOCH PARAMETERS
CC               10-JUN-03 : RD: CORRECT SIZE FOR INDE12 (MAXPAE->MAXPAR)
CC               19-JUN-03 : RD: CORRECT MERGE BUG FOR KINEMATIC COORDINATES
CC               24-JUN-03 : RD: CORRECT NORMILIZE OF RESI FOR CODE ONLY
CC               26-JUN-03 : RD: INDIV. SINGULARITY HANDLING FOR KIN. AND CLK
CC               07-JUL-03 : RD: "ADVANCED SINGULARITY LOGIC" ALSO FOR IQXX
CC               22-JUL-03 : RD: MOVE CONSTRAINING OF KIN.POS. TO SR WGTKIN
CC               22-JUL-03 : RD: NORMALIZED ON APRIORI WEIGHTS ONLY
CC               21-Jan-04 : RD: PROCESS ONLY A RANGE OF PARAMETERS IN CLKSUM
CC               11-OCT-04 : CU: CLOSE LFNRES
CC               16-NOV-04 : RD: GET SOME DIMENSIONS AS PARAMETERS FROM GPSEST
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               23-JUL-05 : HU: MISSING DECLARATIONS ADDED
CC               02-SEP-05 : HB: SPLIT SCRATCH FILE INTO FOUR FILES
CC                               (ONLY QUICK SOLUTION, NOT GENERALLY SOLVED)
CC               14-SEP-05 : HB: SCRATCH FILE SPLITTING GENERALLY SOLVED
CC               05-OCT-05 : RD: ADJUST "MAXSNG" FOR RESEPO
CC               23-FEB-06 : RD: READ DTFIL FROM A SEPARATE RECORD
CC               11-JUN-06 : HB: READ A PRIORI VALUES FROM LFNRES AND WRITE
CC                               THEM TO LFNAUX
CC               15-NOV-06 : HB: CORRECT ORDER OF A PRIORI VALUES
CC               17-JUN-07 : RD: ONLY PHASE FOR RESUBSTITUTION OF EPO-PARAM.
CC               01-NOV-07 : HB: ADD PARAMETER SECIPL
CC               29-MAY-09 : RD: INPUT CLOCKS ALSO FROM INPUT CLK RNX FILE
CC               21-MAY-10 : MF: CALL SR INIT_FILHEAD
CC               10-JUL-12 : RD: USE SYMINVG INSTEAD OF SYMIN8/SYMING
CC               10-JUL-12 : RD: USE M_BERN WITH ONLY
CC               10-JUL-12 : RD: REMOVE UNUSED PARAMETERS AND VARIABLES
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, r8b,
     1                    lfnprt, lfnerr, lfnloc, lfnres, lfn002
      USE d_clkrnx, ONLY: t_clkhead, t_clkrec, undef
      USE d_resFil, ONLY: t_resHead, resHed_GPSEST0, resHed_GPSEST1,
     1                    init_reshead, init_filhead
      USE p_gpsest, ONLY: MAXLCQ
      USE d_const,  ONLY: WGTCOD, WGTPHA
      USE f_ikf
      USE s_alcerr
      USE s_opnfil
      USE s_wgtkin
      USE s_inquire
      USE s_opnerr
      USE s_solve
      USE s_syminvg
      USE s_qxxleo
      USE s_addnor
      USE s_addn12
      USE s_wtresh2
      USE s_gtflna
      USE s_gtsclk
      USE s_clksum
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I       , IAC     , IDIR    , IE      ,
     1          IEPO    , IFIL    , II      , IK      , IPHSEP  ,
     2          ILCQ    , IND     , INDL    , INDX    , INDX12  ,
     3          IO      , IOSTAT  , IQXX    , IRAUX2  ,
     4          IRC     , IRCRES  , IREC    , IREF    , ISING   ,
     5          ISINGUL , ISNG    , ISNG0   , ISNGTOT , ISNGWG  ,
     6          ISTA    , ISVN    , ITYP    , IZEROD  , J       ,
     7          JJ      , K       , KII     , KK      , KK12    ,
     8          KPAR    , L       , LFNAUX  , LSAV    , MAXDIM  ,
     9          MAXEQN  , MAXFLS  , MAXFRS  , MAXLOC  , MAXOBS  ,
     1          MAXPAE  , MAXPAR  , MAXSAS  , MAXSNG  , MXCEQN  ,
     2          MXCFRQ  , MXCLCQ  , MXCSAT  , MXESNG  , NCLK    ,
     3          NDIM    , NEPO    , NFTOT   , NITEM   , NOBS    ,
     4          NORRES  , NPAEPO  , NPAEPO_0, NPAR    , NPAR12  ,
     5          NPAREP  , NREC    , NREF    , NSTAT   , NSTWGT  ,
     6          NEPSNG  , JFIL    , RNXCLK
C
      REAL*8    AELL    , BELL    , CLOCKC  , DSEC    ,
     1          RMSSUM  , SCELL   , SECIPL  ,
     2          SIGAPR  , SIGV    , TEST    , TOBS    ,
     3          WGTDEF  , WGTT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
c      PARAMETER (MAXOBS=MAXSAS*MAXFLS,
c     1           MAXPAE=MAXSAS+MAXFLS,MAXDIM=MAXOBS*6)
C
C MAXFLS: MAXIMUM NUMBER OF FILES IN A SESSION
C MAXSAS: MAXIMUM NUMBER OF SATELLITES AT ONE EPOCH
C MAXMEA: MAXIMUM NUMBER OF MEASUREMENT TYPES
C MAXOBS: MAXOBS=MAXFLS*MAXSAS*2: MAXIMUM NUMBER OF OBSERVATIONS PER EPOCH
C                             *2: BECAUSE OF CODE + PHASE
C MAXPAE: MAXIMUM NUMBER OF EPOCH PARAMETERS (ONLY CLOCKS: MAXSAS+MAXFLS)
C MAXDIM: MAXIMUM SIZE OF "ASNGx" ARRAY (MAXOBS*2+3 should be enough)
C         BASED ON NUMBER OF OBSERVATIONS TIMES MAXIMUM NON-ZERO ELEMENTS
C         IN FIRST DESIGN MATRIX. THIS SHOULD BE 2+3: ONE STATION AND ONE
C         SATELLITE CLOCK AND THE KINEMATIC POSITIONS PER OBSERVATION.
C         HOWEVER, BECAUSE OF ADNHLP WE HAVE TO MAKE THIS 6
C    -->> now computed in INIMAX
C
      TYPE(t_clkhead) clkhed
      TYPE(t_clkrec)  clkrec
      TYPE(t_resHead) reshed
C
      CHARACTER(LEN=6), PARAMETER :: srName = 'resepo'
C
      CHARACTER*255 NEWFIL
      CHARACTER*80 TITLE
      CHARACTER*32 FILRES,FILAUX,FILNAM
      CHARACTER*16 STNAME(*)
      CHARACTER*6  MXNFRQ,MXNEQN,MXNSAT
      CHARACTER*4  CSESS(2,*)
      CHARACTER*1  FLGAMB
C
      REAL*8       TIMREF(*),XXX(*),ASING(*),BSING(*)
      REAL*8       VSIG(1000)
      REAL*8       HELP1(MAXPAR),XSTAT(3,*)
      REAL(r8b), DIMENSION(:),   ALLOCATABLE :: HADD
      REAL(r8b), DIMENSION(:),   ALLOCATABLE :: ASNG1
      REAL(r8b), DIMENSION(:),   ALLOCATABLE :: BSNG1
      REAL(r8b), DIMENSION(:),   ALLOCATABLE :: ASAV
      REAL(r8b), DIMENSION(:),   ALLOCATABLE :: ATMP
      REAL(r8b), DIMENSION(:),   ALLOCATABLE :: BTMP
      REAL(r8b), DIMENSION(:),   ALLOCATABLE :: XTMP
      REAL(r8b), DIMENSION(:),   ALLOCATABLE :: XXX0
c
      REAL(r8b), DIMENSION(:),   ALLOCATABLE :: ASNG12
      REAL(r8b), DIMENSION(:),   ALLOCATABLE :: ATMP12
      REAL(r8b), DIMENSION(:,:), ALLOCATABLE :: QXX
      REAL*8       ANOR(*),WGTGEN(3)
      REAL*8       STWGT(3,*),DXELL(3),DRELL(3)
C
      REAL*8       WEIGHT(*)
C
      INTEGER*4    NFRFIL(*),ICARR(MXCFRQ,*),MEATYP(*),NSATEL(*)
      INTEGER*4    INDP(*),STFIL(2,*),IDELTT(*)
      INTEGER*4    IFIL1(MXCEQN),IOBNU1(MXCEQN),ISV121(2,MXCEQN)
      INTEGER*4    IFRSE1(MXCEQN)
      INTEGER*4    SATNUM(MXCSAT,*),NDIFF(*),ISTWGT(*)
C      INTEGER*4    PARHLP(MAXPAE),SNGLST(MAXPAE),ISTWGT(*)
C
      INTEGER*4    LOCQ(MAXLCQ,MAXLOC),LCQ2(MAXLCQ,MAXLOC)
C
      INTEGER(i4b), DIMENSION(:),ALLOCATABLE :: PARHLP
      INTEGER(i4b), DIMENSION(:),ALLOCATABLE :: SNGLST
      INTEGER(i4b), DIMENSION(:),ALLOCATABLE :: INDE
      INTEGER(i4b), DIMENSION(:),ALLOCATABLE :: INDI
      INTEGER(i4b), DIMENSION(:),ALLOCATABLE :: INDK
      INTEGER(i4b), DIMENSION(:),ALLOCATABLE :: IND1
      INTEGER(i4b), DIMENSION(:),ALLOCATABLE :: IND0
C
      INTEGER(i4b), DIMENSION(:),ALLOCATABLE :: INDE12
      INTEGER(i4b), DIMENSION(:),ALLOCATABLE :: IND12
c      INTEGER*4    INDA(*),INDE(MAXPAE),INDE12(MAXPAE)
c      INTEGER*4    INDI(MAXOBS),INDK(MAXOBS)
c      INTEGER*4    IND1(MAXDIM),IND12(MAXOBS*MAXSNG)
      INTEGER*4    INDA(*),NEPOBS(3)
      INTEGER*4    IFIRST
C
      LOGICAL*4    LNEW,YES
C
      COMMON/CRSEPO/VSIG
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
      COMMON/MCMEQN/MXCEQN,MXNEQN
C
      DATA FLGAMB/' '/
      DATA IFIRST/1/
C
C NULLIFY POINTERS
C ----------------
      CALL init_reshead(reshed)

C INITIALIZE SOME VARIABLES
C -------------------------
      ITYP=1
C
      WGTGEN(1)=1.D0
      WGTGEN(2)=WGTCOD/WGTPHA
      WGTGEN(3)=1.D0
C
      DO 10 I=1,NFTOT
        IF (NFRFIL(I) .EQ.2 .AND.
     1      ICARR(1,I).EQ.1 .AND. ICARR(2,I).EQ.2) THEN
          ITYP=2
        ENDIF
10    CONTINUE
C
C WRITE RESIDUAL FILE HEADER
C --------------------------
      CALL GTFLNA(0,'RESIDRS',FILRES,IRCRES)
      IF (IRCRES.EQ.0) THEN
C
C INIT RESIDUAL HEADER RECORD
C ---------------------------
        RESHED%TITLE = TITLE
        IF (NDIFF(1).EQ.0) THEN
          RESHED%DSC  = resHed_GPSEST0
        ELSE
          RESHED%DSC  = resHed_GPSEST1
        ENDIF
        RESHED%DSC%NPAR = NPAR
        JFIL = 0
        ALLOCATE(RESHED%FILHEAD(NFTOT),STAT=irc)
        CALL ALCERR(IRC,'RESHED%FILHEAD',(/NFTOT/),'RESEPO')
        DO IFIL=1,NFTOT
          CALL init_filhead(RESHED%FILHEAD(IFIL))
          IF (IPHSEP.EQ.1.AND.MEATYP(IFIL).NE.1) CYCLE
          JFIL=JFIL+1
          IF (NFRFIL(IFIL) .NE.2 .OR.
     1        ICARR(1,IFIL).NE.1 .OR. ICARR(2,IFIL).NE.2)
     2      RESHED%DSC%ITYP=1
          RESHED%FILHEAD(JFIL)%MEATYP = MEATYP(IFIL)
          RESHED%FILHEAD(JFIL)%NFRFIL = NFRFIL(IFIL)
          RESHED%FILHEAD(JFIL)%ICARR(1:NFRFIL(IFIL)) =
     1                                     ICARR(1:NFRFIL(IFIL),IFIL)
          RESHED%FILHEAD(JFIL)%STANAM(1) = STNAME(STFIL(1,IFIL))
          IF (NDIFF(1).GT.0)
     1      RESHED%FILHEAD(JFIL)%STANAM(2) = STNAME(STFIL(2,IFIL))
          RESHED%FILHEAD(JFIL)%CSESS(:) = CSESS(:,IFIL)
          RESHED%FILHEAD(JFIL)%IDELTT = IDELTT(IFIL)
          RESHED%FILHEAD(JFIL)%TIMREF = TIMREF(IFIL)
          RESHED%FILHEAD(JFIL)%NSATEL = NSATEL(IFIL)
          ALLOCATE(RESHED%FILHEAD(JFIL)%NUMSAT(NSATEL(IFIL)),STAT=IRC)
          CALL ALCERR(IRC,'RESHED%FILHEAD(JFIL)%NUMSAT',
     1               (/NSATEL(IFIL)/),'RESEPO')
          RESHED%FILHEAD(JFIL)%NUMSAT(:) = SATNUM(1:NSATEL(IFIL),IFIL)
        ENDDO
        RESHED%NFIL = JFIL
C
C WRITE THE RESIDUAL FILE HEADER
C ------------------------------
        CALL WTRESH2(LFNLOC,RESHED)
C
        DO IFIL=1,RESHED%NFIL
          DEALLOCATE(RESHED%FILHEAD(IFIL)%NUMSAT,STAT=IRC)
        ENDDO
        DEALLOCATE(RESHED%FILHEAD,STAT=irc)
      ENDIF
C
C
C RETURN IF SCRATCH FILE IS NOT AVAILABLE
C (E.G., NO OBSERVATIONS FOUND IN PRCEPO)
C -------------------------------------
      CALL INQUIRE(UNIT=LFNRES,OPENED=YES)
      IF (.NOT. YES) RETURN
C
C ALLOCATE SOME OF THE BIG LOCAL ARRAYS
C -------------------------------------
      MAXOBS=MAXSAS*MAXFLS*MAXFRS
CC      MAXDIM=MAXOBS*6
      MAXDIM=MAXOBS*NEPSNG
C
      ALLOCATE(HADD(MAXOBS),STAT=IAC)
      CALL ALCERR(IAC,'HADD',(/MAXOBS/),SRNAME)
      ALLOCATE(ASNG1(MAXDIM),STAT=IAC)
      CALL ALCERR(IAC,'ASNG1',(/MAXDIM/),SRNAME)
      ALLOCATE(BSNG1(MAXOBS),STAT=IAC)
      CALL ALCERR(IAC,'BSNG1',(/MAXOBS/),SRNAME)
      ALLOCATE(ASAV((MAXPAE*(MAXPAE+1))/2),STAT=IAC)
      CALL ALCERR(IAC,'ASAV',(/(MAXPAE*(MAXPAE+1))/2/),SRNAME)
      ALLOCATE(ATMP((MAXPAE*(MAXPAE+1))/2),STAT=IAC)
      CALL ALCERR(IAC,'ATMP',(/(MAXPAE*(MAXPAE+1))/2/),SRNAME)
      ALLOCATE(BTMP(MAXPAE),STAT=IAC)
      CALL ALCERR(IAC,'BTMP',(/MAXPAE/),SRNAME)
      ALLOCATE(XTMP(MAXPAE),STAT=IAC)
      CALL ALCERR(IAC,'XTMP',(/MAXPAE/),SRNAME)
C
      ALLOCATE(PARHLP(MAXPAE),STAT=IAC)
      CALL ALCERR(IAC,'PARHLP',(/MAXPAE/),SRNAME)
      ALLOCATE(SNGLST(MAXPAE),STAT=IAC)
      CALL ALCERR(IAC,'SNGLST',(/MAXPAE/),SRNAME)
      ALLOCATE(INDE(MAXPAE),STAT=IAC)
      CALL ALCERR(IAC,'INDE',(/MAXPAE/),SRNAME)
      ALLOCATE(INDI(MAXOBS),STAT=IAC)
      CALL ALCERR(IAC,'INDI',(/MAXOBS/),SRNAME)
      ALLOCATE(INDK(MAXOBS),STAT=IAC)
      CALL ALCERR(IAC,'INDK',(/MAXOBS/),SRNAME)
      ALLOCATE(IND1(MAXDIM),STAT=IAC)
      CALL ALCERR(IAC,'IND1',(/MAXDIM/),SRNAME)
      ALLOCATE(IND0(MAXPAE),STAT=IAC)
      CALL ALCERR(IAC,'IND0',(/MAXPAE/),SRNAME)
C
c      ALLOCATE(INDE12(MAXPAE),STAT=IAC)
c      CALL ALCERR(IAC,'INDE12',(/MAXPAE/),SRNAME)
      ALLOCATE(INDE12(MAXPAR),STAT=IAC)
      CALL ALCERR(IAC,'INDE12',(/MAXPAR/),SRNAME)
      ALLOCATE(IND12(MAXOBS*MAXSNG),STAT=IAC)
      CALL ALCERR(IAC,'IND12',(/MAXOBS*MAXSNG/),SRNAME)
      ALLOCATE(XXX0(MAXPAE),STAT=IAC)
      CALL ALCERR(IAC,'XXX0',(/MAXPAE/),SRNAME)
C
      IF (IQXX.EQ.1) THEN
        ALLOCATE(ASNG12(MAXOBS*MAXSNG),STAT=IAC)
        CALL ALCERR(IAC,'ASNG12',(/MAXOBS*MAXSNG/),SRNAME)
        ALLOCATE(ATMP12(MAXPAE*MAXPAR),STAT=IAC)
        CALL ALCERR(IAC,'ATMP12',(/MAXPAE*MAXPAR/),SRNAME)
        ALLOCATE(QXX(MAXPAE,MAXPAE),STAT=IAC)
        CALL ALCERR(IAC,'QXX',(/MAXPAE,MAXPAE/),SRNAME)
      ENDIF
C
C REWIND FILE WITH OBSERVATION EQUATIONS
C --------------------------------------
      REWIND LFNRES
C
C OPEN A SCRATCH FILE TO STORE VALUES FOR PROGRAM OUTPUT
C ------------------------------------------------------
      LFNAUX=LFN002
      CALL GTFLNA(1,'AUXFIL1',FILAUX,IRC)
      CALL OPNFIL(LFNAUX,FILAUX,'UNKNOWN','UNFORMATTED',
     1              ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNAUX,IOSTAT,FILAUX,'RESEPO')
      IRAUX2=0
C
C COMPUTE EPOCH SOLUTIONS AND WRITE RESIDUAL FILE
C -----------------------------------------------
      NEPO=0
      DO 100
        NEPO=NEPO+1
C
C INITIALIZE "INDx" AND "ASNGx"
C -----------------------------
        NPAEPO=0
        DO I=1,MAXDIM
          IND1(I)=0
          ASNG1(I)=0.D0
        ENDDO
C
        DO I=1,MAXOBS*MAXSNG
          IND12(I)=0
          IF (IQXX.EQ.1) ASNG12(I)=0.D0
        ENDDO
C
C INITIALIZE "BSNGx"
C ------------------
        DO I=1,MAXOBS
          BSNG1(I)=0.D0
        ENDDO
C
C INITIALIZE "ATMP" "BTMP" AND "XTMP"
C -----------------------------------
        DO I=1,MAXPAR
          INDE12(I)=0
        ENDDO
        DO I=1,MAXPAE
          IND0(I)=0
          INDE(I)=0
          BTMP(I)=0.D0
          XTMP(I)=0.D0
          DO K=1,I
            IK=I*(I-1)/2+K
            ATMP(IK)=0.D0
          ENDDO
        ENDDO
C
C INITIALIZE "QXX"
C -----------------
        IF (IQXX.EQ.1) THEN
          QXX(1:MAXPAE,1:MAXPAE)=0.D0
          ATMP12(1:MAXPAE*MAXPAR)=0.D0
        END IF
C
C READ FIRST RESIDUAL RECORD
C --------------------------
        IF (IFIRST==1) THEN
          IFIRST=0
          CLOSE(unit=lfnres)
          CALL GTFLNA(1,'AUXFIL ',FILNAM,IRC)
          CALL OPNFIL(LFNRES,FILNAM,'UNKNOWN','UNFORMATTED',
     1              ' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNRES,IOSTAT,FILNAM,'RESEPO')
        ENDIF

        READ(LFNRES,END=1000)NDIM,MAXEQN,MXESNG,NPAREP,MXCLCQ

C END OF SCRATCH FILE => NEW FILE HAS TO BE OPENED
        IF (NDIM == -1) THEN
          READ(LFNRES)newFil
          CLOSE(unit=LFNRES)
          CALL OPNFIL(LFNRES,NEWFIL,'UNKNOWN','UNFORMATTED',
     1                ' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNRES,IOSTAT,NEWFIL,'RESEPO')
          READ(LFNRES,END=1000)NDIM,MAXEQN,MXESNG,NPAREP,MXCLCQ
        ENDIF

        NREC=(NDIM+1)/2
        NITEM=NDIM*(NDIM+1)/2/NREC
C
C READ WEIGHT MATRIX
C ------------------
        DO 50 IREC=1,NREC
          READ(LFNRES)(WEIGHT((IREC-1)*NITEM+I),I=1,NITEM)
50      CONTINUE
C
C READ MATRIX ASING, INDA AND BSING
C ---------------------------------
        DO 60 IO=1,NDIM
          READ(LFNRES)IFIL1(IO),IFRSE1(IO),IOBNU1(IO),
     1                (ISV121(K,IO),K=1,2),
     2                (ASING(IO+(K-1)*MAXEQN),INDA(IO+(K-1)*MAXEQN),
     3                K=1,MXESNG),BSING(IO)
60      CONTINUE
C
C READ LOCQ MATRIX
C ----------------
        DO 70 ILCQ=1,MXCLCQ
          READ(LFNRES)(LOCQ(ILCQ,I),I=1,NPAREP)
70      CONTINUE
C
C READ NUMBER OF EPOCH PARAMETERS
C -------------------------------
        READ(LFNRES) NPAEPO_0
C
C READ APRIORI VALUES FOR EPOCH PARAMETERS
C ----------------------------------------
        READ(LFNRES) XXX0(1:NPAEPO_0)
C
C FIND ALL EPOCH PARAMETERS
C -------------------------
        DO 135 I=1,NDIM
          DO 130 K=1,MXESNG
            IND=I+(K-1)*MAXEQN
            KPAR=INDA(IND)
            IF (KPAR.EQ.0) GOTO 135
            IF (KPAR.GT.NPAREP-NPAEPO_0) THEN
C
C CHECK IF NEW PARAMETER (KPAR)
C -----------------------------
              LNEW=.TRUE.
              DO 110 J=1,NPAEPO
                IF (KPAR.EQ.INDE(J)) THEN
                  LNEW=.FALSE.
                ENDIF
110           CONTINUE
C
C SORT PARAMETERS IN ASCENDING ORDER
C ----------------------------------
              IF (LNEW) THEN
                LSAV=1
                DO 120 L=1,NPAEPO
                  IF (KPAR.GT.INDE(L)) LSAV=L+1
120             CONTINUE
                NPAEPO=NPAEPO+1
                DO 125 L=NPAEPO,LSAV+1,-1
                  INDE(L)=INDE(L-1)
                  IND0(L)=IND0(L-1)
125             CONTINUE
                INDE(LSAV)=KPAR
                IND0(LSAV)=KPAR-(NPAREP-NPAEPO_0)
              ENDIF
            ENDIF
130       CONTINUE
135     CONTINUE
C
C NO EPOCH PARAMETERS, NEXT EPOCH
C -------------------------------
        IF (NPAEPO.EQ.0) GOTO 100
C
C FIND ALL NON-EPOCH PARAMETERS
C -------------------------------
        IF (IQXX.EQ.1) THEN
          NPAR12=0
          DO 138 I=1,NDIM
            DO 137 K=1,MXESNG
              IND=I+(K-1)*MAXEQN
              KPAR=INDA(IND)
              IF (KPAR.EQ.0) GOTO 138
              IF (KPAR.LE.(NPAREP-NPAEPO_0).AND.INDP(KPAR).NE.0) THEN
                LNEW=.TRUE.
                DO IE=1,NPAR12
                  IF (INDE12(IE).EQ.INDP(KPAR)) LNEW=.FALSE.
                END DO
C
C SORT NON-EPOCH PARAMETERS IN ASCENDING ORDER
C --------------------------------------------
                IF (LNEW) THEN
                  LSAV=1
                  DO L=1,NPAR12
                    IF (INDP(KPAR).GT.INDE12(L)) LSAV=L+1
                  END DO
                  NPAR12=NPAR12+1
                  DO L=NPAR12,LSAV+1,-1
                    INDE12(L)=INDE12(L-1)
                  END DO
                  INDE12(LSAV)=INDP(KPAR)
                END IF
              END IF
137         CONTINUE
138       CONTINUE
        END IF
C
C CORRECT "BSING" FOR ESTIMATED PARAMETERS (THE NON EPOCH PARAMETERS)
C AND CREATE NEW "ASING" CONTAINING ONLY THE EPOCH PARAMETERS
C -------------------------------------------------------------------
        DO 150 I=1,NDIM
          BSNG1(I)=BSING(I)
          KK=0
          KK12=0
          DO 140 K=1,MXESNG
            IND=I+(K-1)*MAXEQN
            KPAR=INDA(IND)
            IF (KPAR.EQ.0) GOTO 150
C
C CREATE NEW INDA AND ASING ARRAYS FOR EPOCH PARAMETERS ONLY
C ----------------------------------------------------------
            IF (KPAR.GT.NPAREP-NPAEPO_0) THEN
              LNEW=.TRUE.
              DO 145 J=1,NPAEPO
                IF (KPAR.EQ.INDE(J)) THEN
                  LNEW=.FALSE.
                  INDL=J
                ENDIF
145           CONTINUE
              IF (LNEW) THEN
                WRITE(LFNERR,*)' *** SR RESEPO: EPOCH PARAMETER!!'
              ENDIF
              KK=KK+1
              INDX=I+(KK-1)*MAXOBS
              IND1(INDX)=INDL
              ASNG1(INDX)=ASING(IND)
            ELSE
              IF (INDP(KPAR).NE.0) THEN
                BSNG1(I)=BSNG1(I)-ASING(IND)*XXX(INDP(KPAR))
                IF (IQXX.EQ.1) THEN
                  KK12=KK12+1
                  INDX12=I+(KK12-1)*MAXOBS
                  ASNG12(INDX12)=ASING(IND)
                  IND12(INDX12)=INDP(KPAR)
                END IF
              END IF
            ENDIF
140       CONTINUE
150     CONTINUE
C
C CREATE EPOCH NORMAL EQUATION "ATMP", "BTMP" AND ATMP12
C ------------------------------------------------------
        WGTDEF=WGTGEN(MEATYP(IFIL1(1)))
        CALL ADDNOR(NDIM  ,NPAEPO,WGTDEF,WEIGHT,ASNG1 ,IND1  ,
     1              INDI  ,INDK  ,HADD  ,MAXOBS,BSNG1 ,
     2              ATMP  ,BTMP  ,RMSSUM,NOBS  )

        IF (IQXX.EQ.1) THEN
          CALL ADDN12(NDIM  ,NPAEPO,NPAR12,WGTDEF,WEIGHT,
     1                ASNG1 ,IND1  ,ASNG12,IND12 ,INDI  ,
     2                HADD  ,MAXOBS,ATMP12)
        END IF
C
C COPY THE LOCQ INFORMATION
C -------------------------
        DO KII=1,NPAEPO
          ILCQ=INDE(KII)
          LCQ2(:,KII)=LOCQ(:,ILCQ)
          XXX0(KII)=XXX0(IND0(KII))
        ENDDO
C
C ABSOLUTE CONSTRAINTS FOR KINEMATIC COORDINATES
C ----------------------------------------------
        CALL WGTKIN(1     ,NPAEPO,ATMP  ,LCQ2  ,STNAME,
     1              CLKHED,CLKREC,AELL  ,BELL  ,DXELL ,DRELL ,
     2              SCELL ,XSTAT ,SIGAPR,NSTWGT,ISTWGT,STWGT )
C
C CONDITION OF SUM FOR EPOCH CLOCKS
C ---------------------------------
        IF (clkhed%numref.EQ.2)THEN
          CALL CLKSUM(MAXLCQ,CLKHED,STNAME,STFIL,NEPOBS,
     1                1     ,NPAEPO,LCQ2,1D0,ATMP,(/.TRUE.,.TRUE./))
        ENDIF
C
C SOLVE EPOCH PARAMETERS (SAVE NORMAL MATRIX FOR SINGULAR DETECTION)
C ------------------------------------------------------------------
        DO II=1,NPAEPO*(NPAEPO+1)/2
          ASAV(II)=ATMP(II)
        ENDDO
        CALL SYMINVG(NPAEPO,ATMP,1,ISING,PARHLP)
        CALL SOLVE(NPAEPO,ATMP,BTMP,XTMP)
C
C DETECT ALL SINGULAR CLOCKS
C --------------------------
        ISNG0=0
        DO 300 I=1,NPAEPO
          SNGLST(I)=0
          IF (PARHLP(I).EQ.1) THEN
            ISNG0=ISNG0+1
            SNGLST(ISNG0)=I
            ATMP(IKF(I,I))=0.D0
          ENDIF
300     CONTINUE
C
C DETECT AND REMOVE SINGULAR CLOCKS/CLOCK BLOCKS
C ----------------------------------------------
        ISNGTOT=ISNG0
310     IF (ISNG0.GT.0) THEN
          ISNG=0
          DO 320 I=1,ISNG0
            II=SNGLST(I)
            IF (LOCQ(1,INDE(II)).EQ.23.OR.LOCQ(1,INDE(II)).EQ.24) THEN
              DO 330 J=1,NPAEPO
                IF (ASAV(IKF(II,J)).NE.0.D0 .AND. II.NE.J .AND.
     1              ATMP(IKF( J,J)).NE.0.D0) THEN
                  ATMP(IKF(J,J))=0.D0
                  ISNG=ISNG+1
                  PARHLP(ISNG)=J
                ENDIF
330           CONTINUE
C
C REMOVE ALL COMPONENTS OF THE KINEMATIC COORDINATES
C --------------------------------------------------
            ELSE IF (LOCQ(1,INDE(II)).EQ.21) THEN
              DO 331 J=1,NPAEPO
                IF (LOCQ(2,INDE(II)).EQ.LOCQ(2,INDE(J)).AND.
     1              II.NE.J .AND.ATMP(IKF( J,J)).NE.0.D0) THEN
                  ATMP(IKF(J,J))=0.D0
                  ISNG=ISNG+1
                  PARHLP(ISNG)=J
                ENDIF
331           CONTINUE
            ENDIF
320       CONTINUE
        ENDIF
        IF (ISNG.NE.0) THEN
          ISNG0=ISNG
          DO 340 I=1,ISNG0
            SNGLST(I)=PARHLP(I)
340       CONTINUE
          ISNGTOT=ISNGTOT+ISNG0
          GOTO 310
        ENDIF
        IF (ISNGTOT.GT.0) THEN
          WRITE(LFNPRT,*)' ### SR RESEPO: SINGULARITIES',
     1                                    ISNGTOT,ISING,NPAEPO
          DO KII=1,NPAEPO
            IF (ATMP(IKF(KII,KII)).EQ.0D0) THEN
              PARHLP(KII)=1
            ELSE
              PARHLP(KII)=0
            ENDIF
          ENDDO
        ENDIF
C
C COMPUTE VAR/COV
C ---------------
        IF (IQXX.EQ.1) THEN
          CALL QXXLEO(NPAEPO,NPAR12,NPAR,ATMP,ATMP12,ANOR,
     1                INDE12,MAXPAE,QXX)
        END IF
C
C THERE ARE ANY CLOCK PARAMETERS FOR THIS EPOCH?
C ---------------------------------------------
        NCLK=0
        DO KII=1,NPAEPO
          ILCQ=INDE(KII)
          IF ((LOCQ(1,ILCQ).EQ.23.OR.LOCQ(1,ILCQ).EQ.24) .AND.
     1        ATMP(IKF(KII,KII)).GT.0D0) NCLK=NCLK+1
        ENDDO
C
C LOOP OVER EPOCH PARAMETERS
C --------------------------
        DO 360 KII=1,NPAEPO
          ILCQ=INDE(KII)
          IEPO=LOCQ(4,ILCQ)
          DSEC=DBLE(IEPO-1)*CLKREC%EPOCH(1)/CLKREC%NEPO
          TOBS=CLKHED%TFIRST+DSEC/86400.D0
C
C ON FIRST PARAMETER OF EPOCH WRITE REFERENCE CLOCK VALUE
C -------------------------------------------------------
          IF (KII.EQ.1 .AND. NCLK.GT.0 .AND. ISNGTOT.NE.NPAEPO) THEN

            DO 370 IREF=1,clkhed%ref(clkhed%numref)%nRef
              NREF=clkhed%numref
C
C Station clock is reference:
              IF (clkhed%ref(nRef)%clk(iRef)%Idx.LE.clkHed%nSta) THEN
                DO IFIL=1,MAXFLS
                  IF (STFIL(1,IFIL).EQ.
     1                clkhed%ref(nRef)%clk(iRef)%Idx)THEN
                    ITYP = 23
                    LCQ2(:,1) = 0
                    LCQ2(1,1) = ITYP
                    LCQ2(2,1) = clkhed%ref(nRef)%clk(iRef)%Idx
                    LCQ2(4,1) = IEPO
                    LCQ2(7,1) = -1
                    WRITE(LFNAUX) ITYP,clkhed%ref(nRef)%clk(iRef)%Idx
                    WRITE(LFNAUX) TOBS, (LCQ2(II,1),II=1,MAXLCQ),
     1                            undef,0D0,1D0
                    GOTO 370
                  ENDIF
                ENDDO
C
C Satellite clock is reference:
              ELSE
                ISVN=clkhed%ref(nRef)%clk(iRef)%Idx0
                CALL GTSCLK(RNXCLK,TOBS,ISVN,SECIPL,2,CLOCKC,IRC)
                IF (IRC.NE.0 .OR. CLOCKC.EQ.0D0) GOTO 370
                ITYP = 24
                LCQ2(:,1) = 0
                LCQ2(1,1) = ITYP
                LCQ2(3,1) = ISVN
                LCQ2(4,1) = IEPO
                LCQ2(7,1) = -1
                WRITE(LFNAUX) ITYP,ISVN
                WRITE(LFNAUX) TOBS, (LCQ2(II,1),II=1,MAXLCQ),
     1                        CLOCKC,0D0,1D0
              ENDIF
370         ENDDO
          ENDIF
C
C COMPUTE RMS OF EPOCH PARAMETER SOLUTION
C ---------------------------------------
C          IF (ATMP(IKF(KII,KII)).GT.0.D0) THEN
          ISINGUL=0
          IF (PARHLP(KII).NE.0) THEN
            ISINGUL=1
            IF (LOCQ(1,ILCQ).EQ.23) THEN
              WRITE(LFNAUX) LOCQ(1,ILCQ),LOCQ(2,ILCQ)
              WRITE(LFNAUX) TOBS,(LOCQ(II,ILCQ),II=1,MAXLCQ),
     1                      0d0,0d0,ATMP(IKF(KII,KII))
            ENDIF
            IF (LOCQ(1,ILCQ).EQ.24) THEN
              WRITE(LFNAUX) LOCQ(1,ILCQ),LOCQ(3,ILCQ)
              WRITE(LFNAUX) TOBS,(LOCQ(II,ILCQ),II=1,MAXLCQ),
     1                      0d0,0d0,ATMP(IKF(KII,KII))
            ENDIF
          ENDIF
C
          IF (LOCQ(1,ILCQ).EQ.21.AND.LOCQ(3,ILCQ).EQ.1) THEN
            DO I=0,2
              IF (PARHLP(KII+I).NE.0) ISINGUL=1
            END DO
C
            IF (ISINGUL.EQ.1) THEN
              WRITE(LFNAUX) LOCQ(1,ILCQ),LOCQ(2,ILCQ)
              DO JJ=0,2
                WRITE(LFNAUX)TOBS,(LOCQ(II,INDE(KII+JJ)),II=1,MAXLCQ),
     1                      0.d0,(0d0,II=1,3),1D20
              END DO
            END IF
          ENDIF
C
C WRITE ESTIMATED EPOCH PARAMETERS
C --------------------------------
          IF (ISINGUL .EQ. 0) THEN
            ITYP=LOCQ(1,ILCQ)
C
C STATION CLOCKS
C --------------
            IF (ITYP.EQ.23) THEN
              IFIL=LOCQ(5,ILCQ)
              WRITE(LFNAUX) ITYP,LOCQ(2,ILCQ)
              IF (IQXX.EQ.1) THEN
                WRITE(LFNAUX) TOBS,(LOCQ(II,ILCQ),II=1,MAXLCQ),
     1                      -XXX0(KII),XTMP(KII),
     2                      QXX(KII,KII)
              ELSE
                WRITE(LFNAUX) TOBS,(LOCQ(II,ILCQ),II=1,MAXLCQ),
     1                      -XXX0(KII),XTMP(KII),
     2                      ATMP(IKF(KII,KII))
              ENDIF
            ENDIF
C
C SATELLITE CLOCKS
C ----------------
            IF (ITYP.EQ.24) THEN
              ISVN=LOCQ(3,ILCQ)
              WRITE(LFNAUX) ITYP,ISVN
              IF (IQXX.EQ.1) THEN
                WRITE(LFNAUX) TOBS,(LOCQ(II,ILCQ),II=1,MAXLCQ),
     1                      XXX0(KII),XTMP(KII),QXX(KII,KII)
              ELSE
                WRITE(LFNAUX) TOBS,(LOCQ(II,ILCQ),II=1,MAXLCQ),
     1                      XXX0(KII),XTMP(KII),ATMP(IKF(KII,KII))
              ENDIF
            ENDIF
C
C EPOCH WISE COORDINATES
C ----------------------
            IF (ITYP.EQ.21) THEN
              ISTA=LOCQ(2,ILCQ)
              IDIR=LOCQ(3,ILCQ)
C
              IF (IDIR.EQ.1) THEN
                WRITE(LFNAUX) ITYP,ISTA
                DO JJ=0,2
                  IF (IQXX.EQ.1) THEN
                    WRITE(LFNAUX)TOBS,
     1                    (LOCQ(II,INDE(KII+JJ)),II=1,MAXLCQ),
     2                     XXX0(KII+JJ),(QXX(KII+JJ,KII+II),II=0,2),
     3                     XTMP(KII+JJ)
                  ELSE
                    WRITE(LFNAUX)TOBS,
     1                  (LOCQ(II,INDE(KII+JJ)),II=1,MAXLCQ),
     2                  XXX0(KII+JJ),(ATMP(IKF(KII+JJ,KII+II)),II=0,2),
     3                  XTMP(KII+JJ)
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
C
C END-RMS
C -------
          ENDIF
C
C NEXT EPOCH
C ----------
360     CONTINUE
C
C INVERT WEIGHT MATRIX FOR NORMALIZED RESIDUALS
C ---------------------------------------------
        IF (NORRES.EQ.2.OR.NORRES.EQ.3) THEN
          CALL SYMINVG(NDIM,WEIGHT,0,ISING)
        ENDIF
C
C COMPUTE RESIDUALS OF EPOCH
C --------------------------
        DO 400 I=1,NDIM
          IF (NORRES.EQ.2) THEN
            DO J=1,NPAEPO
              HELP1(J)=0D0
            ENDDO
          ENDIF
          VSIG(I)=-BSNG1(I)
C
C LOOP OVER BOTH PARAMETERS (STA & SAT CLK)
C -----------------------------------------
          DO 410 K=1,MXESNG
            IND=I+(K-1)*MAXOBS
            KPAR=IND1(IND)
            IF (KPAR.EQ.0) GOTO 415
            VSIG(I)=VSIG(I)+ASNG1(IND)*XTMP(KPAR)
C
C COMPUTE HELP MATRIX FOR THE RESIDUAL VARIANCES (HELP = AOBS*ANOR)
C -----------------------------------------------------------------
            IF (NORRES.EQ.2) THEN
              DO 420 J=1,NPAEPO
                HELP1(J)=HELP1(J)+ASNG1(IND)*ATMP(IKF(KPAR,J))
420           CONTINUE
            ENDIF
410       CONTINUE
C
415       CONTINUE
C                                               T
C COMPUTE THE RESIDUAL VARIANCES (AOBS*ANOR*AOBS ) (HELP1=AOBS*ANOR)
C ------------------------------------------------------------------
          IF (NORRES.EQ.2) THEN
            WGTT=0D0
            ISNGWG=0
            DO 430 K=1,MXESNG
              IND=I+(K-1)*MAXOBS
              KPAR=IND1(IND)
              IF (KPAR.EQ.0) GOTO 440
              WGTT=WGTT+HELP1(KPAR)*ASNG1(IND)
C
C HELP1==0D0: PARAMETER WITHOUT REDUNDANCY,
C             ONLY APRIORI WEIGHT MAY BE USED FOR NORMALIZATION
              IF (DABS(HELP1(KPAR)).LT.1D-12) ISNGWG=ISNGWG+1
430         CONTINUE
          ENDIF
C
440       CONTINUE
C
C RESIDUALS NORMILIZED ONLY ON APRIORI WEIGHTS
C --------------------------------------------
          IF (NORRES.EQ.3) THEN
            WGTT=0D0
            ISNGWG=0
          ENDIF
C
C COMPUTE NORMALIZED RESIDUAL
C ---------------------------
          IF (NORRES.EQ.2.OR.NORRES.EQ.3) THEN
            IF (IZEROD.EQ.1) THEN
              TEST=(WEIGHT(IKF(I,I))*WGTGEN(MEATYP(IFIL1(I)))-WGTT)
            ELSE
              TEST=WEIGHT(IKF(I,I))-WGTT*(WGTGEN(MEATYP(IFIL1(I))))
            ENDIF
            IF (TEST.LE.0.0001.OR.ISNGWG.GT.0) THEN
              TEST=1d0/WGTGEN(MEATYP(IFIL1(I)))
            ENDIF
            SIGV=DSQRT(TEST)
            VSIG(I)=VSIG(I)/SIGV
          ENDIF
C
C NEXT RESIDUAL
C -------------
400     CONTINUE
C
C WRITE RESIDUALS ON OUTPUT FILE
C ------------------------------
      IF (IRCRES.EQ.0) THEN
        DO 500 I=1,NDIM
            WRITE(LFNLOC) IFIL1(I),IOBNU1(I),IFRSE1(I),
     1           (ISV121(K,I),K=1,2),VSIG(I),FLGAMB
500     CONTINUE
      ENDIF
C
C EPOCH LOOP
C ----------
100   CONTINUE
C
C END OF FILE REACHED
C -------------------
1000  CONTINUE
C
      CLOSE(UNIT=LFNAUX)
      CLOSE(UNIT=LFNRES)
      IF (IRCRES.EQ.0) CLOSE(UNIT=LFNLOC)
C
C DEALLOCATE SOME OF THE BIG LOCAL ARRAYS
C ---------------------------------------
      DEALLOCATE(HADD,STAT=IAC)
      DEALLOCATE(ASNG1,STAT=IAC)
      DEALLOCATE(BSNG1,STAT=IAC)
      DEALLOCATE(ASAV,STAT=IAC)
      DEALLOCATE(ATMP,STAT=IAC)
      DEALLOCATE(BTMP,STAT=IAC)
      DEALLOCATE(XTMP,STAT=IAC)
C
      DEALLOCATE(PARHLP,STAT=IAC)
      DEALLOCATE(SNGLST,STAT=IAC)
      DEALLOCATE(INDE,STAT=IAC)
      DEALLOCATE(INDI,STAT=IAC)
      DEALLOCATE(INDK,STAT=IAC)
      DEALLOCATE(IND1,STAT=IAC)
C
      DEALLOCATE(INDE12,STAT=IAC)
      DEALLOCATE(IND12,STAT=IAC)
      DEALLOCATE(XXX0,STAT=IAC)
      DEALLOCATE(IND0,STAT=IAC)
C
      IF (IQXX.EQ.1) THEN
        DEALLOCATE(ASNG12,STAT=IAC)
        DEALLOCATE(ATMP12,STAT=IAC)
        DEALLOCATE(QXX,STAT=IAC)
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
