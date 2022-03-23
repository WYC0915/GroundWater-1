      MODULE s_NORINI
      CONTAINS
C*
      SUBROUTINE NORINI(STRAMB,NP    ,NPN   ,MAXPAR,MAXAMP,S0    ,
     1                  LOCQ  ,NSTWGT,ISTWGT,STWGT ,CW    ,PO    ,
     2                  SIGPOL,NSASTC1,NSASTC2,NUMSTC1,NUMSTC2,
     3                  SIGSTC1,SIGSTC2,SCASTC ,INTSTC1,INTSTC2,
     4                  AEL   ,BEL   ,XEL   ,A     ,B     ,OPTDCB,
     5                  SIGDCB,SIGTRP,SIGTRS,ISGTRS,NOBS  ,RMS   ,
     6                  INDP  ,TPOL  ,ISGPOL,SIGOFF,ISGOFF,SIGHIL,
     7                  SIGPOT,SCAHIL,SCAPOT,SIGALB,SIGCEN,SCAALB,
     8                  SCACEN,ISGNUT,SIGCAL,SIGGIM,SCAGIM,SIGRAO,
     9                  IFLAG ,OPTELI,MIXED ,IZEROD,PO2   ,SIGSPV,
     1                  SIGRGB,NOBSPA,OPLOAD,SIGHOI,TIMISB,NCLKSA,
     2                  NMXINT,NMXSAP,NMXARC,OPTGSP)
CC
CC NAME       :  NORINI
CC
CC PURPOSE    :  INITIALIZE NORMAL EQUATION MATRICES A,B AND IMPOSE
CC               A PRIORI WEIGHTS
CC
CC PARAMETERS :
CC         IN :  STRAMB : AMBIGUITY RESOLUTION STRATEGY       I*4(*)
CC               NP     : TOTAL NUMBER OF PARAMETERS          I*4
CC               NPN    : NUMBER OF PARAMETERS WITHOUT AMBI-  I*4
CC                        GUITIES
CC               MAXPAR : MAXIMUM NUMBER OF PARAMETERS        I*4
CC               MAXAMP : MAXIMUM NUMBER OF AMBIGUITY PARAM.  I*4
CC               S0     : A PRIORI SIGMA                      R*8
CC               LOCQ(K,I),I=1,2,..,NP,K=1,..,MAXLCQ: DEFINI- I*4
CC                        TION OF PARAMETER WITH NUMBER I
CC               NSTWGT : NUMBER OF STATIONS WITH WEIGHTS     I*4
CC               ISTWGT(I),I=1,2,...: STATION NUMBERS WITH    I*4
CC                        WEIGHTS
CC               STWGT(K,I),K=1,2,3,I=1,2,..: WEIGHTS FOR STA-R*8
CC                        TION COORDINATES
CC               CW(K,I),K=1,2;I=1,2,..: WEIGHTS FOR CLOCK    R*8
CC                        PARAMETERS
CC               PO(K),K=1,2,...,NORB: WEIGHTS FOR ELEMENTS   R*8
CC               SIGPOL : A PRIORI SIGMA OF POLE PARAMETERS   R*8(5,*)
CC                        1-5 := XP,YP,DT,EPS,PSI
CC                          * := 1..MAXPOL
CC               NSASTC1: NUMBER OF SATELLITES WITH STOCHAST. I*4
CC                        ORBITS  (GPS AND GLONASS)
CC               NSASTC2: NUMBER OF LEOS WITH STOCHAST.       I*4
CC                        ORBITS
CC               NUMSTC1: CORRESPONDING SATELLITE NUMBERS     I*4(*)
CC               NUMSTC2: CORRESPONDING LEO NUMBERS           I*4(*)
CC               SIGSTC1: A PRIORI SIGMAS FOR STOCHASTIC      R*8(3,*)
CC                        PARAM (GPS AND GLONASS)
CC               SIGSTC2: A PRIORI SIGMAS FOR STOCHASTIC      R*8(3,*)
CC                        PARAM (LEO)
CC               SCASTC : SCALING FACTOR FOR                  R*8(*)
CC                        (1): STOCHASTIC PULSES
CC                        (2): STOCHASTIC ACCELERATIONS
CC               INTSTC1: INTERVAL NUMBERS FOR STOCHASTIC     I*4(*,*,*)
CC                          PARAM (GPS AND GLONASS)
CC               INTSTC2: INTERVAL NUMBERS FOR STOCHASTIC     I*4(*,*,*)
CC                          PARAM (LEO)
CC               AEL,BEL: AXES OF ELLIPSOID                   R*8
CC               XEL(I,K),I=1,2,3 , K=1,2,3,... ELLIPSOIDAL   R*8
CC                        COORDINATES OF RECEIVERS
CC               A,B    : RESULTING NORMAL EQUATION MATRICES  R*8
CC                        MATRIX A ONLY CONSISTS OF THE UPPER
CC                        TRIANGLE, LINEARIZED COLUMNWISE
CC               OPTDCB : OPTIONS FOR ESTIMATION OF           I*4(*)
CC                        DIFFERENTIAL CODE BIASES
CC                        (1): ESTIMATE DCBS FOR SATELLITES
CC                        (2): ESTIMATE DSBS FOR RECEIVERS
CC                        (3): REFERENCE SATELLITE NUMBER
CC                             = 0: CONSTRAIN ALL SAT
CC                             =-1: CONSTRAIN SUM OF ALL SAT
CC                        (4): PROCESS NIGHT-TIME DATA ONLY
CC               SIGDCB : A PRIORI SIGMA FOR DCBS (IN NS)     R*8(*)
CC                        (1): REFERENCE SATELLITE BIASES
CC                        (2): RECEIVER BIASES
CC               SIGTRP(I),I=1,2,... : A PRIORI SIGMA FOR     R*8
CC                        TROPOSPHERE PARAMETERS
CC               SIGTRS(J,I),I=1,2,..,NTRSTA: A PRIORI SIGMA  R*8
CC                        IN M FOR INDIV. TROPOSPHERE PARAM.
CC                        J=1: NORTH (GRADIENT)
CC                        J=2: EAST (GRADIENT)
CC                        J=3: UP (ZENITH DELAY)
CC               ISGTRS(I),I=1,2,... : TYPE OF A PRIORI SIGMA I*4
CC                        FOR TROP.PARAMETERS:
CC                        =0: ABSOLUTE SIGMA
CC                        =1: SIGMA RELATIVE TO THE PREVIOUS
CC                            TROP.PARAMETER OF THE SAME SITE
CC               NOBS   : NUMBER OF OBSERVATIONS              I*4
CC               RMS    : RMS-ERROR                           R*8
CC               INDP(I),I=1,..,NPAR: INDEX FOR PARAMETER     I*4
CC                        LOCATION BEFORE AND AFTER REDUCTION
CC                        OF MATRICES ANOR,BNOR, AND LOCQ.
CC               TPOL   : INTERVAL BOUNDARIES FOR EARTH ROT.  R*8(2,*)
CC                        MODELS
CC               ISGPOL : EARTH ROTATION PARAMETER SIGMAS :   I*4(*)
CC                        =0 : APPLY FOR RELEVANT PARAMETER ONLY
CC                             THE ABSOLUTE CONSTRAINTS GIVEN IN
CC                             INPUT FILEL
CC                        =1 : ENSURE CONTINUITY WITH RESPECT TO
CC                             PREVIOUE POLYNOMIAL (IN ADDITION TO
CC                             ABSOLUTE CONSTRAINTS)
CC                        =4 : CONSTRAIN DRIFTS TO ZERO
CC                        =5 : ENSURE CONTINUITY WITH RESPECT
CC                             TO PREVIOUS POLYNOMIAL AND
CC                             CONSTRAIN DRIFTS TO ZERO
CC               SIGOFF(J,I),J=1,2,3,I=1,..,NRQOFF: A PRIORI  R*8
CC                        SIGMAS FOR COMP. J AND ANT. REQ. I
CC               ISGOFF(I),I=1,..,NRQOFF: TYPE OF SIGMA       I*4
CC                        =0: ABSOLUTE SIGMA
CC                        =1: SIGMA RELATIVE TO THE PREVIOUS
CC                            PARAMETER OF THE SAME GROUP
CC               SIGHIL : A PRIORI SIGMA FOR HILL PARMS       R*8(*)
CC               SIGPOT : A PRIORI SIGMA FOR POT PARMS        R*8(*)
CC               SCAHIL : SCALE FACTOR FOR HILL PARMS         R*8
CC               SCAPOT : SCALE FACTOR FOR POT PARMS          R*8
CC               SIGALB : A PRIORI SIGMAS FOR ALB PAR TYPES   R*8(*)
CC               SIGCEN : CORRESP. A PRIORI SIGMAS            R*8(*)
CC               ISGNUT : EARTH ORIENTATION PARAMETER SIGMAS: I*4(*)
CC                        =0 : APPLY FOR RELEVANT PARAMETER ONLY
CC                             THE ABSOLUTE CONSTRAINTS GIVEN IN
CC                             INPUT FILEL
CC                        =1 : ENSURE CONTINUITY WITH RESPECT TO
CC                             PREVIOUE POLYNOMIAL (IN ADDITION TO
CC                             ABSOLUTE CONSTRAINTS)
CC                        =4 : CONSTRAIN DRIFTS TO ZERO
CC                        =5 : ENSURE CONTINUITY WITH RESPECT
CC                             TO PREVIOUS POLYNOMIAL AND
CC                             CONSTRAIN DRIFTS TO ZERO
CC               SIGCAL(I),I=1,..,NANCAL: A PRIORI SIGMAS IN  R*8
CC                        METERS
CC               SIGGIM : ABSOLUTE SIGMA FOR                  R*8(*)
CC                        (1): ION. COEFFICIENTS (TECU)
CC                        (2): SINGLE-LAYER HEIGHT (M)
CC                        RELATIVE SIGMA FOR
CC                        (3): ION. COEFFICIENTS (TECU)
CC                        (4): SINGLE-LAYER HEIGHT (M)
CC               SCAGIM : SCALING FACTOR FOR                  R*8(*)
CC                        (1): ION. COEFFICIENTS
CC                        (2): SINGLE-LAYER HEIGHT
CC               SIGRAO(J,I),J=1,2,I=1,..,NANRAO: A PRIORI    R*8
CC                        SIGMAS IN METERS
CC                        J=1: HORIZONTAL COMPONENTS
CC                        J=2: VERTICAL COMPONENT
CC               IFLAG :  = 0 INITIALIZE THE NEQ AT THE VERY  I*4
CC                            BEGINNING OF THE PROGRAM
CC                        = 1 SET THE REMAINING WEIGHTS       I*4
CC                            BEFORE MATRIX INVERSION
CC               OPTELI(I),I=1,..,MAXTYP: OPTION FOR PRE-     I*4
CC                        ELIMINATION OF PARAMETER TYPES:
CC                        =0 : NOT PRE-ELIMINATED
CC                        =1 : PRE-ELIMINATED BEFORE INVERSION
CC                        =2 : PRE-ELIMINATED AFTER  INVERSION
CC                        =3 : PRE-ELIMINATED EPOCH-WISE
CC               MIXED  : SATELLITE SYSTEM                    I*4
CC                        =0: GPS ONLY
CC                        =1: MIXED (GPS AND GLONASS)
CC                        =2: GLONASS ONLY
CC                        =3: LEO ONLY
CC               IZEROD : OBSERVATION FILE TYPE               I*4
CC                        =1: ZERO DIFF. FILES
CC                        =2: SINGLE DIFF. FILES
CC               PO2(K),K=1,2,...,NORB2: WEIGHTS FOR LEO      R*8
CC                        ELEMENTS
CC               SIGSPV(I),I=1,..,NANSPV: A PRIORI SIGMAS IN  R*8
CC                        METERS
CC               SIGRGB : A PRIORI SIGMAS FOR RANGE BIASES    R*8
CC                        IN METERS
CC               NOBSPA : NUM.OF OBSERV PER PARAMETER         I*4(*,*)
CC                        NOBSPA(MAXMEA*ISYS+IMEA,IPAR)
CC               OPLOAD : SCALING FACTORS FOR VIENNA GRID FILES   T_OPTLOAD(3)
CC                        1: ATMOSPHERIC NON-TIDAL LOADING
CC                        2: OCEAN NON-TIDAL LOADING
CC                        3: HYDROSTATIC PRESSURE LOADING
CC               SIGHOI(I): HOI A PRIORI SIGMAS               R*8
CC               NMXINT : Max.NUMBER OF INTEGR. INTERVALS     I*4
CC               NMXSAP : MAX.NUMBER OF SATELLITES (INCL. MAN)I*4
CC                        (EITHER NMXSAT OR NFTOT FOR LEOS)
CC               NMXARC : NUMBER OF ARCS                      I*4
CC               OPTGSP   : GNSS-SPECIFIC PARAMETER OPTIONS   T_OPTGSP
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER, S.FANKHAUSER
CC
CC CREATED    :  87/11/02 15:52
CC
CC CHANGES    :  11-FEB-92 : SF: CHANGES DUE TO ERP-ESTIMATION
CC               03-APR-92 : ??: ADD RELATIVE TROPOSPHERE CONSTRAINTS
CC               30-MAY-92 : ??: INTRODUCE FLAG WITH ABS./REL. TROPOS.
CC               13-JUL-92 : ??: WEIGHT ZERO ALLOWED FOR ORBIT PARAM.
CC               02-AUG-92 : ??: ERP APRIORI WEIGHT ONLY FOR FIRST COEFF.
CC               20-MAR-93 : ??: WEIGHTS FOR STOCHASTIC ORBIT PARAMETERS
CC               20-MAR-93 : ??: CONTINUITY OF EARTH ROT. PARAMETERS
CC               03-APR-93 : ??: ESTIMATION OF SATELLITE ANTENNA OFFSETS
CC               14-MAY-93 : ??: NEW PARAMETER TYPES (POTENTIAL, HILL,
CC                               ALBEDO, CENTER OF MASS)
CC               01-JUN-93 : ??: WRONG INDEX USED IN A PRIORI SIGMAS FOR
CC                               COORDINATES
CC               28-DEC-93 : MR: TIME WINDOWS FOR SAT.ANT. OFFSETS
CC               13-APR-94 : SS: DIFFERENTIAL IONOSPHERE PARAMETERS
CC               19-APR-94 : RW: CPO-MODEL INCLUDED
CC               10-AUG-94 : MR: CALL EXITRC
CC               06-NOV-94 : MR: ANTENNA PHASE CENTER PARAMETERS
CC               25-NOV-94 : MR: CORRECT WEIGHTING OF A.P.C. PARAMETERS
CC               06-JUN-95 : SS: GLOBAL IONOSPHERE MODEL PARAMETERS
CC               08-JUN-95 : LM: KINEMATIC COORDINATES
CC               17-JUL-95 : LM: CHECK DIMENSION NHELP
CC               14-AUG-95 : LM: STRAMB=ARRAY
CC               22-AUG-95 : EB: CORRECT ADDITION OF STATION WEIGHTS
CC               05-DEC-95 : SS: NEW IONOSPHERE MODEL (TYPE 2)
CC               05-DEC-95 : SS: "IONO" REMOVED (SEE SR CHKION)
CC               05-DEC-95 : SS: APPLY "SCAGIM"
CC               26-MAR-96 : MR: RECEIVER ANTENNA OFFSETS
CC                3-APR-96 : TS: CLOCK ESTIMATION CHANGES
CC               09-MAY-96 : MR: CORRECT HANDLING OF RELATIVE TROPO.
CC                               SIGMAS = 0.D0
CC               09-SEP-06 : TS: CORRECT HANDLING OF ORBIT SIGMA'S
CC               08-APR-97 : SS: NIELL MAPPING, TROPOSPHERE GRADIENTS
CC               08-JUL-97 : LM: ADITIONAL PARAMETERS IFLAG, OPTELI
CC               14-AUG-97 : SS: DIFFERENTIAL CODE BIASES
CC               08-OCT-97 : MR: RATIO ZENITH/GRADIENT PARAMETERS
CC               08-JAN-98 : SS: SR DIMTST USED
CC               26-JAN-98 : SS: INCLUDE FILES "MAXGIM" AND "MAXGIT"
CC               26-JAN-98 : SS: RELATIVE SIGMA FOR GIMS
CC               26-MAY-98 : SS: "IONDEV" REDIMENSIONED
CC               25-JUN-98 : HH: MODIFICATIONS FOR GLONASS (AMB. SIGMAS)
CC               21-DEC-98 : SS: GPS/GLONASS DCBS FOR RECEIVERS
CC               21-MAY-01 : DS: 206264.D0 -> 206264.8D0
CC               05-JUN-01 : DS: NEW PARAMETERS FOR LEO STOCH. ORBIT: NSASTC1,
CC                               NSASTC2,NUMSTC1,NUMSTC2,SIGSTC1,SIGSTC2
CC               23-AUG-01 : DS: CONSTRAINTS FOR KIN COORDINATES
CC               14-MAR-02 : SS: DCB-RELATED ZERO-MEAN CONDITION WRT
CC                               OBSERVED SATELLITES ONLY
CC               07-MAY-02 : SS: DCB UPDATE
CC               23-JUN-02 : DS: SEPARATE LEO & GPS SIGMAS FOR ORBIT PARAMET.
CC               13-NOV-02 : RS: SATELLITE ANTENNA PHASE CENTER VARIATIONS
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               22-JUL-03 : RD: MOVE CONSTRAINING OF KIN.POS. TO SR WGTKIN
CC               16-OCT-03 : RS: CORRECT CONSTRAINING OF ANTENNA PCVS
CC               12-DEC-03 : AJ: DIFF. SCALING FOR DIFF. STOCH. ORBIT PAR.
CC               09-MAR-04 : RD: NO CONSTRAINTS FOR GLONASS-AMBIGUITIES ANYMORE
CC               14-APR-05 : AJ: A PRIORI STOCH. PARAMETERS INTRODUCED
CC               13-MAY-05 : RD: CORRECT REL. CONSTRAINING FOR TROPO.
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC               08-MAY-07 : SS: GLONASS AMBIGUITY RESOLUTION ENABLED
CC               24-JUN-08 : DT: ASSUMING SVN>=951 for SLR
CC               02-APR-09 : DT: ADD SIGMA FOR RANGE BIASES
CC               04-MAY-09 : RD: SCALING OF LOADING MODELS ADDED
CC               09-MAY-09 : RD: SAT/FRQ-SPECIFIC RECEIVER CLOCK BIASES
CC               09-MAY-09 : RD: SEPERATE RECEIVER CLOCKS FOR GPS/GLONASS
CC               04-JAN-10 : SL: SIGHOI ADDED
CC               15-APR-10 : DT: ADD NOBSPA; NO WEIGHTS FOR CRD AND RGB
CC               25-NOV-10 : MM: GNSS-SPECIFIC PARAMETERS
CC               03-DEC-10 : HB: ADD NMXINT,NMXSAP,NMXARC AS PARAMETER FOR
CC                               SMALLER ARRAYS
CC               17-FEB-11 : SS: STRAMB(3) FOR SELECTION OF GNSS
CC               01-MAY-11 : SL: M_BERN WITH ONLY, ALLOCATE LCQAPR[12]
CC               17-AUG-11 : LP: ADD SUM CONDITION FOR ISB (RECEIVER DCB)
CC               27-SEP-11 : AJ: A PRIORI PULSE CHECK
CC               18-SEP-12 : RD: DEALLOCATE ARRAYS AT THE END
CC               18-SEP-12 : RD: REMOVE UNUSED VARIABLES
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern,   ONLY: i4b, fileNameLength, lfnOrb, lfnErr, lfnOr1
      USE d_const,  ONLY: ars
      USE m_maxdim, ONLY: MAXGIM, MAXGIT, MAXVAR
      USE m_global, ONLY: MAXSYS
      USE p_gpsest, ONLY: MAXMEA, t_optLoad,t_optGsp
      USE s_chkion
      USE f_ikf
      USE s_dimtst
      USE s_err3d
      USE s_dminv
      USE s_ionosi
      USE s_exitrc
      USE s_wgterp
      USE s_gtflna
      USE s_opnfil
      USE s_opnerr
      USE s_prtder
      USE s_prtde2
      USE s_alcerr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I      , ICAL   , ICMP   , ICO2   , ICOM   , ICOR   ,
     1          IDEG   , IFLAG  , IFRQ   , IGRP   , II     , IK     ,
     2          IKL    , ILAT   , INDEX  , INTER  , IOFR   ,
     3          ION001 , IONTYP , IORB   , IORD   , IP     , IP1    ,
     4          IP1IP  , IP1IP1 , IP2    , IPE    , IPIP   , IPIP1  ,
     5          IPIP2  , IPIPN  , IPLST  , IPN    , IPNIPN , IRAO   ,
     6          IRC    , IREQ   , ISEQ   , ISPV   , IST    , ISTA   ,
     7          ISTC   , ISYS   , ITERM  , ITIME  , ITRSTA , K      ,
     8          KPAR   , MAXAMP , MAXPAR , MIXED  , MXCLCQ , NAMP   ,
     9          NAZI   , NHELP  , NMOD   , NMODEL , NOBS   ,
     1          NP     , NPN    , NSASTC1, NSASTC2, NSTWGT ,
     2          NZEN   , MXCSTC , MXCSAT , NARC   , NMXINT , NMXSAP ,
     3          IFMT   , NLIN   , LINE   , IARC   , NINT   , IQ     ,
     4          ISAT   , NPRSTC1, KSTC   , NSTCA  , KFRC   , NPRSTC2,
     5          IEXIST , KARC   , KSVN   , KSAT   , KPRSTC , KARC_A ,
     6          KSVN_A , KSTC_A , KFRC_A , INDFRC_A        , KSAT_A ,
     7          IRCRPR , NRAD   , NVAR   ,
     8          IORSYS , ICRARC , IOSTAT , IZEROD , IINT   , NCLKSA ,
     9          NMXARC , mSat   , IRCNEQ
C
      REAL*8    AEL    , BEL    , DEN    , DET    , E2     , FAK    ,
     1          RM     , RMS    , RN     , S0     , SCAALB , SCACEN ,
     2          SCAHIL , SCAPOT , SIGABS , SIGREL , SP     , ZERO   ,
     3          WDIF   , WEIGHT , WGT    , WGTABS , WGTREL , WOFDIF ,
     4          TDUMMY , DUMMY
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER(LEN=6), PARAMETER :: srName= 'NORINI'
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMSTC/MXCSTC,MXNSTC
C
      TYPE(t_optLoad), DIMENSION(:):: opLoad
      TYPE(t_optGsp)               :: OPTGSP
C
      REAL*8      A(*),B(*),STWGT(3,*),CW(2,*),PO(*),PO2(*),XEL(3,*)
      REAL*8      COVXYZ(3,3),COVPLH(3,3),SIGTRP(*),SIGTRS(3,*)
      REAL*8      IONCOE(MAXGIT,MAXGIM),IONSIG(MAXGIT,MAXGIM)
      REAL*8      IONDEV(10,MAXGIM),SIGPOL(5,*),SIGHIL(*),SIGPOT(*)
      REAL*8      TPOL(2,*),SIGOFF(3,*),SIGALB(*),SIGCEN(*),SIGHOI(*)
      REAL*8      SIGSTC1(3,*),SIGSTC2(3,*),TIMISB(3,*),SCASTC(*)
      REAL*8      WOFABS(3),SIGCAL(*),SIGGIM(*),SCAGIM(*),SIGRAO(2,*)
      REAL*8      SIGDCB(*)
      REAL*8      WABS(3),SIGSPV(*)
      REAL*8      DRDELE(3),ELESAT(7),RPRPAR(MAXVAR)
      REAL*8      TIMSTCA(NMXINT,NMXSAP,NMXARC)
      REAL*8      PARSTCA1(3,NMXINT,NMXSAP,NMXARC)
      REAL*8      PARSTCA2(3,NMXINT,NMXSAP,NMXARC)
      REAL*8      TOSC(NMXARC)
      REAL*8      SIGRGB
C
      INTEGER*4   LOCQ(MXCLCQ,*),ISTWGT(*),L1(3),L2(3),STRAMB(*)
      INTEGER*4   INTSTC1(MXCSTC,MXCSAT,*),INTSTC2(MXCSTC,MXCSAT,*)
      INTEGER*4   INDP(*),ISGTRS(*),ISGOFF(*),IP1OFF(3),IOFGRP(3)
      INTEGER*4   NUMSTC1(*),NUMSTC2(*),OPTDCB(*),ITRLST(3)
      INTEGER*4   ISGPOL(*),ISGNUT(*)
      INTEGER*4   IONREQ(6,MAXGIM),NM(MAXGIT,2,MAXGIM),NTERM(MAXGIM)
      INTEGER*4   OPTELI(*)
      INTEGER*4   SVN(NMXSAP,NMXARC),NSAT(NMXARC)
      INTEGER*4   INTSTCA1(NMXINT,NMXSAP,NMXARC)
      INTEGER*4   INTSTCA2(NMXINT,NMXSAP,NMXARC)
      INTEGER*4   NSTCEPA(NMXINT),FRCTYPA(3,NMXINT)
      INTEGER(i4b),DIMENSION(:,:),ALLOCATABLE :: lcqApr1
      INTEGER(i4b),DIMENSION(:,:),ALLOCATABLE :: lcqApr2
      INTEGER*4   NOBSPA(:,:)
      INTEGER*4   SUMOBS
C
      CHARACTER(LEN=fileNameLength)    :: FILSTD1
      CHARACTER(LEN=fileNameLength)    :: FILSTD2
      CHARACTER(LEN=fileNameLength)    :: FILRPR1
      CHARACTER(LEN=fileNameLength)    :: FILRPR2
      CHARACTER(LEN=fileNameLength)    :: FILNEQ
      CHARACTER*6 MXNLCQ,MXNSTC,MXNSAT
      CHARACTER*8 ANLTYP
C
      COMMON/CNORIN/ IONREQ,NM,IONCOE,IONSIG,IONDEV
      DATA ION001/1/
C
C FLAG FOR TROPOSPHERE STATION REQUESTS AND SATELLITE ANTENNA OFFSETS
C -------------------------------------------------------------------
      ITRSTA=0
      DO ICMP=1,3
        IOFGRP(ICMP)=0
      ENDDO
C
C ACTUAL SIZE OF NEQ-SYSTEM
C -------------------------
      IF(STRAMB(1).EQ.-1)THEN
        NHELP=NPN
      ELSE
        NHELP=NP
C
        NAMP=NP-NPN
        CALL DIMTST(1,1,2,srName,'MAXAMP','AMBIGUITY PARAMETERS',
     1    ' ',NAMP,MAXAMP,IRC)
      END IF
C
C CHECK MAXIMUM DIMENSIONS ALLOWED
C --------------------------------
      CALL DIMTST(1,1,2,srName,'MAXPAR','PARAMETERS',
     1  ' ',NHELP,MAXPAR,IRC)
C
C INITIALIZATION ONLY AT THE BEGINING OF THE PROGRAM
C --------------------------------------------------
      IF (IFLAG .EQ. 0) THEN
C
C INITIALIZE PARAMETER INDEX
C --------------------------
        DO 5 IP=1,NP
          INDP(IP)=IP
5       CONTINUE
C
C INITIALIZE NUMBER OF OBS., RMS, ACTUAL NUMBER OF AMBIGUITIES
C ------------------------------------------------------------
        RMS=0.0D0
        NOBS=0
C
C INITIALIZE MATRICES A,B
C -----------------------
        DO 10 I=1,NHELP
          B(I)=0.D0
          DO 10 K=1,I
            IK=I*(I-1)/2+K
            A(IK)=0.D0
10      CONTINUE
      END IF
C
C GET NEQ FILNAM FOR A PRIORI PULSE CHECK
C ---------------------------------------
      CALL GTFLNA(0,'NEQUARS',FILNEQ,IRCNEQ)
C
C GET TYPE CODE OF IONOSPHERE MODELS
C ----------------------------------
      CALL CHKION(IONTYP)
C
C GET STOCHASTIC SATELLITE A PRIORI INFORMATION
C ---------------------------------------------
      IF (NSASTC1.NE.0) THEN
C
C GET GNSS STD- AND RPR- FILE NAMES
        CALL GTFLNA(1,'STDORB ',FILSTD1,IRC)
        CALL GTFLNA(0,'RPRCOE ',FILRPR1,IRC)
C
C OPEN GNSS STANDARD ORBIT FILE
        CALL OPNFIL(LFNORB,FILSTD1,'OLD','UNFORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNORB,IOSTAT,FILSTD1,srName)
C
C GET NECESSARY INFORMATION FROM STD-FILE
        READ(LFNORB) NARC
C
        IF (NARC.LT.0) THEN
          READ(LFNORB) IFMT,NARC
          READ(LFNORB) NLIN
          DO I=1,NLIN
            READ(LFNORB) LINE
          ENDDO
        ENDIF
C
        mSat=0
        DO 15 IARC=1,NARC
          READ(LFNORB) NSAT(IARC),NINT,IQ,
     1                (SVN(I,IARC),I=1,NSAT(IARC))
          IF(nSat(iArc).GE.mSat) mSat=nSat(iArc)
          READ(LFNORB) TOSC(IARC),ZERO
          IF(ZERO.EQ.0.D0.OR.ZERO.EQ.2.D0) THEN
            DO 11 I=1,NINT+1
              READ(LFNORB) TDUMMY
11          CONTINUE
          ENDIF
C
          DO 12 ISAT=1,NSAT(IARC)
            READ(LFNORB) DUMMY
12        CONTINUE
          DO 14 IINT=1,NINT
            READ(LFNORB) DUMMY
            DO 13 K=1,IQ+1
              READ(LFNORB) DUMMY
13          CONTINUE
14        CONTINUE
15      CONTINUE
C
        CLOSE(UNIT=LFNORB)
C
C ALLOCATE STRUCTURES
C -------------------
        ALLOCATE(lcqApr1(mxcLcq,nmxInt*mSat),stat=irc)
        CALL ALCERR(irc,'lcqApr1',(/mxcLcq,nmxInt*mSat/),srName)
C
C GET NECESSARY INFORMATION FROM RPR-FILE
        NPRSTC1=0
        DO 19 IARC=1,NARC
          DO 18 ISAT=1,NSAT(IARC)
            CALL PRTDER(FILRPR1,SVN(ISAT,IARC),1,0,1,TOSC(IARC),NMXINT,
     1                  ICRARC,IORSYS,NVAR,NRAD,DRDELE,ELESAT,RPRPAR,
     2                  ANLTYP,IRCRPR,NSTCA,FRCTYPA,NSTCEPA,
     3                  INTSTCA1(1,ISAT,IARC),TIMSTCA(1,ISAT,IARC),
     4                  PARSTCA1(1,1,ISAT,IARC))
C
C GET LOCQ OF A PRIORI STOCHASTIC PARAMETERS
            DO 17 KSTC=1,NSTCA
              DO 16 KFRC=1,NSTCEPA(KSTC)
                NPRSTC1=NPRSTC1+1
                LCQAPR1(1,NPRSTC1) = 11
                LCQAPR1(2,NPRSTC1) = IARC
                LCQAPR1(3,NPRSTC1) = SVN(ISAT,IARC)
                LCQAPR1(4,NPRSTC1) = KSTC
                LCQAPR1(5,NPRSTC1) = FRCTYPA(KFRC,KSTC)
                LCQAPR1(6,NPRSTC1) = KFRC
                LCQAPR1(7,NPRSTC1) = ISAT
16            CONTINUE
17          CONTINUE
18        CONTINUE
19      CONTINUE
C
      END IF
C
C GET LEO STOCHASTIC A PRIORI INFORMATION
C ---------------------------------------
      IF (NSASTC2.NE.0) THEN
C
C GET LEO STD- AND RPR- FILE NAMES
        CALL GTFLNA(1,'LEOSTD ',FILSTD2,IRC)
        CALL GTFLNA(0,'LEORPR ',FILRPR2,IRC)
C
C OPEN LEO STANDARD ORBIT FILE
        CALL OPNFIL(LFNOR1,FILSTD2,'OLD','UNFORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNOR1,IOSTAT,FILSTD2,srName)
C
C GET NECESSARY INFORMATION FROM STD-FILE
        READ(LFNOR1) NARC
C
        IF (NARC.LT.0) THEN
          READ(LFNOR1) IFMT,NARC
          READ(LFNOR1) NLIN
          DO I=1,NLIN
            READ(LFNOR1) LINE
          ENDDO
        ENDIF
C
        mSat=0
        DO 25 IARC=1,NARC
          READ(LFNOR1) NSAT(IARC),NINT,IQ,
     1                (SVN(I,IARC),I=1,NSAT(IARC))
          IF(nSat(iArc).GE.mSat) mSat=nSat(iArc)
          READ(LFNOR1) TOSC(IARC),ZERO
          IF(ZERO.EQ.0.D0.OR.ZERO.EQ.2.D0) THEN
            DO 21 I=1,NINT+1
              READ(LFNOR1) TDUMMY
21          CONTINUE
          ENDIF
C
          DO 22 ISAT=1,NSAT(IARC)
            READ(LFNOR1) DUMMY
22        CONTINUE
          DO 24 IINT=1,NINT
            READ(LFNOR1) DUMMY
            DO 23 K=1,IQ+1
              READ(LFNOR1) DUMMY
23          CONTINUE
24        CONTINUE
25      CONTINUE
C
        CLOSE(UNIT=LFNOR1)
C
C ALLOCATE STRUCTURES
C -------------------
        ALLOCATE(lcqApr2(mxcLcq,nmxInt*mSat),stat=irc)
        CALL ALCERR(irc,'lcqApr2',(/mxcLcq,nmxInt*mSat/),srName)
C
C GET NECESSARY INFORMATION FROM RPR-FILE
        NPRSTC2=0
        DO 29 IARC=1,NARC
          DO 28 ISAT=1,NSAT(IARC)
            CALL PRTDE2(FILRPR2,SVN(ISAT,IARC),1,0,1,TOSC(IARC),NMXINT,
     1                  ICRARC,IORSYS,NVAR,NRAD,DRDELE,ELESAT,RPRPAR,
     2                  ANLTYP,IRCRPR,NSTCA,FRCTYPA,NSTCEPA,
     3                  INTSTCA2(1,ISAT,IARC),TIMSTCA(1,ISAT,IARC),
     4                  PARSTCA2(1,1,ISAT,IARC))
C
C GET LOCQ OF A PRIORI STOCHASTIC PARAMETERS
            DO 27 KSTC=1,NSTCA
              DO 26 KFRC=1,NSTCEPA(KSTC)
                NPRSTC2=NPRSTC2+1
                LCQAPR2(1,NPRSTC2) = 11
                LCQAPR2(2,NPRSTC2) = IARC
                LCQAPR2(3,NPRSTC2) = SVN(ISAT,IARC)
                LCQAPR2(4,NPRSTC2) = KSTC
                LCQAPR2(5,NPRSTC2) = FRCTYPA(KFRC,KSTC)
                LCQAPR2(6,NPRSTC2) = KFRC
                LCQAPR2(7,NPRSTC2) = ISAT
26            CONTINUE
27          CONTINUE
28        CONTINUE
29      CONTINUE
C
      END IF
C
C LOOP OVER ALL PARAMETERS
C ------------------------
      DO 5000 IP=1,NHELP
C
        IF ( (IFLAG.EQ.0 .AND.
     &       (OPTELI(LOCQ(1,IP)).EQ.0 .OR. OPTELI(LOCQ(1,IP)).EQ.2))
     &    .OR.
     &       (IFLAG.EQ.1 .AND.
     &       (OPTELI(LOCQ(1,IP)).EQ.1 .OR. OPTELI(LOCQ(1,IP)).EQ.3)) )
     &    GOTO 5000
C
C GET SUM OF ALL OBSERVATIONS FOR THE ACTUAL PARAMETER
C ----------------------------------------------------
        SUMOBS=0
        DO II=1,MAXMEA*MAXSYS
          SUMOBS = SUMOBS + NOBSPA(II,IP)
        END DO
C
C SELECT PARAMETER TYPE
C ---------------------
        GO TO ( 100, 200, 300, 400, 500, 600, 700, 800, 900,1000,
     1         1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,
     2         2100,2200,2300,2400,2500,2600,2700,5000,5000,3000)
     3         LOCQ(1,IP)
        WRITE(LFNERR,20) LOCQ(1,IP)
20      FORMAT(/,'*** SR NORINI: PARAMETER TYPE NOT DEFINED',
     1                      16X,'TYPE:',I3,/)
        CALL EXITRC(2)
C
C A PRIORI WEIGHTS FOR STATION COORDINATES
C ----------------------------------------
100     IST=LOCQ(2,IP)
        K=LOCQ(3,IP)
        IF(K.NE.1) GOTO 5000
C
C DO NOT APPLY CONSTRAINTS IF NO OBSERVATIONS
        IF (SUMOBS.EQ.0) GOTO 5000
C
C FIND CORRESPONDING WEIGHT (IF DEFINED)
        DO 30 II=1,NSTWGT
          IF (ISTWGT(II).EQ.IST) GOTO 40
30      CONTINUE
40      E2=(AEL**2-BEL**2)/(AEL**2)
        SP=DSIN(XEL(1,IST))
        DEN=DSQRT(1.D0-E2*SP**2)
        RN=AEL/DEN
        RM=AEL*(1.D0-E2)/DEN**3
        DO 50 I=1,3
          IF(I.EQ.1)FAK=(RM+XEL(3,IST))
          IF(I.EQ.2)FAK=(RN+XEL(3,IST))
          IF(I.EQ.3)FAK=1.D0
          DO 50 K=1,3
            COVPLH(I,K)=0.D0
            IF(I.EQ.K) COVPLH(I,K)=(STWGT(K,II)/FAK/S0)**2
            IF(I.EQ.2.AND.K.EQ.2) COVPLH(I,K)=
     1                            COVPLH(I,K)/DCOS(XEL(1,IST))**2
50      CONTINUE
        CALL ERR3D(XEL(1,IST),XEL(2,IST),XEL(3,IST),AEL,BEL,
     1             +1,COVXYZ,COVPLH)
        CALL DMINV(COVXYZ,3,DET,L1,L2)
        DO 60 I=1,3
          DO 60 K=1,I
            IK=IKF(I+IP-1,K+IP-1)
            A(IK)=A(IK)+COVXYZ(I,K)
60      CONTINUE
        GOTO 5000
C
C CLOCK PARAMETERS
C ----------------
200     IST=LOCQ(2,IP)
        IREQ=LOCQ(3,IP)
        K=LOCQ(4,IP)
C
C STATION SPECIFIC
        IF (LOCQ(6,IP).EQ.0) THEN
          IF(CW(K,IREQ).EQ.0.D0) GOTO 5000
          WEIGHT=(S0/CW(K,IREQ))**2
          GOTO 4000
C
C NOT OBSERVED
        ELSE IF (A(IKF(IP,IP)).EQ.0D0) THEN
          GOTO 5000
        ENDIF
C
C CONDITION OF SUM: ALL REQUESTS TO THE SAME STATION FOR
C       TIME-DEP. INTERSYSTEM BIASES
C ------------------------------------------------------
        IF (LOCQ(6,IP).EQ.5) THEN
          DO IP1=IP,NPN
            IF (LOCQ(1,IP1).GT.2) EXIT
            IF (LOCQ(1,IP1).EQ.2 .AND.
     1          LOCQ(2,IP1).EQ.LOCQ(2,IP).AND.
     2          LOCQ(6,IP1).EQ.LOCQ(6,IP)) THEN
!              IF (IP1.GT.IP.AND.LOCQ(5,IP1).LT.0) EXIT
              IF (A(IKF(IP1,IP1)).NE.0D0) THEN
                IP1IP1=IKF(IP,IP1)
                A(IP1IP1)=A(IP1IP1)+(S0*1000d0)**2
              ENDIF
              IF (LOCQ(5,IP1).LT.0) EXIT
            ENDIF
          ENDDO
C
          IF (ABS(LOCQ(5,IP)).EQ.LOCQ(7,IP)) THEN
            DO IP1=IP,NPN
              IF (LOCQ(1,IP1).GT.2) EXIT
              IF (LOCQ(1,IP1).EQ.2 .AND.
     1            LOCQ(5,IP1).EQ.LOCQ(7,IP1).AND.
     2            LOCQ(6,IP1).EQ.LOCQ(6,IP).AND.
     3            DABS(TIMISB(1,LOCQ(4,IP1))-
     4                 TIMISB(1,LOCQ(4,IP))).LT.0.1d0/86400d0.AND.
     5            A(IKF(IP1,IP1)).NE.0D0) THEN
                IP1IP1=IKF(IP,IP1)
                A(IP1IP1)=A(IP1IP1)+(S0*1000d0)**2
              ENDIF
            ENDDO
          ENDIF
          GOTO 5000
        ENDIF
C
C CONDITION OF SUM: ALL REQUESTS TO THE SAME SATELLITE/WITH THE
C       SAME FREQUENCY
C
        DO IP1=IP,NPN
          IF (NCLKSA.EQ.0) EXIT
          IF (LOCQ(1,IP1).GT.2) EXIT
          IF (LOCQ(1,IP1).EQ.2 .AND.
     1        LOCQ(4,IP1).EQ.LOCQ(4,IP).AND.
     2        LOCQ(6,IP1).EQ.LOCQ(6,IP).AND.
     3        A(IKF(IP1,IP1)).NE.0D0) THEN
            IP1IP1=IKF(IP,IP1)
            A(IP1IP1)=A(IP1IP1)+(S0*1000d0)**2
          ENDIF
        ENDDO
C
C CONDITION OF SUM: BETWEEN ALL GPS SATELLITES OF A REQUEST (FOR GPS-ONLY
C       OR GPS+GLONASS) OR BETWEEN ALL GLONASS SATELLITES OF A REQUEST (ONLY
C       FOR A GLONASS ONLY REQUEST)
C
C FREQUENCY SPECIFIC, GLONASS ONLY
        IF ( LOCQ(7,IP).EQ.2.AND.
     1      (LOCQ(6,IP).EQ.1.OR.LOCQ(6,IP).EQ.4)) THEN
          DO IP1=IP,NPN
            IF (LOCQ(1,IP1).NE.2) EXIT
            IF (LOCQ(1,IP1).EQ.2         .AND.
     1          LOCQ(3,IP1).EQ.LOCQ(3,IP).AND.
     2          A(IKF(IP1,IP1)).NE.0D0)   THEN
              IP1IP1=IKF(IP,IP1)
              A(IP1IP1)=A(IP1IP1)+(S0*1000d0)**2
            ENDIF
          ENDDO
C
C SATELLITE SPECIFIC, GLONASS-ONLY
        ELSE IF (LOCQ(7,IP).EQ.2.AND.LOCQ(6,IP).EQ.2) THEN
          DO IP1=IP,NPN
            IF (LOCQ(1,IP1).NE.2) EXIT
            IF (LOCQ(1,IP1).EQ.2         .AND.
     1          LOCQ(3,IP1).EQ.LOCQ(3,IP).AND.
     2          INT(LOCQ(4,IP1)/100).EQ.INT(LOCQ(4,IP)/100).AND.
     3          A(IKF(IP1,IP1)).NE.0D0)   THEN
              IP1IP1=IKF(IP,IP1)
              A(IP1IP1)=A(IP1IP1)+(S0*1000d0)**2
            ENDIF
          ENDDO
C
C SATELLITE SPECIFIC, GPS FOR GPS-ONLY AND GPS+GLONASS
        ELSE IF (LOCQ(6,IP).EQ.2.AND.LOCQ(4,IP).LT.100) THEN
          DO IP1=IP,NPN
            IF (LOCQ(1,IP1).NE.2) EXIT
            IF (LOCQ(1,IP1).EQ.2         .AND.
     1          LOCQ(3,IP1).EQ.LOCQ(3,IP).AND.
     2          LOCQ(4,IP1).LT.100       .AND.
     3          A(IKF(IP1,IP1)).NE.0D0)   THEN
              IP1IP1=IKF(IP,IP1)
              A(IP1IP1)=A(IP1IP1)+(S0*1000d0)**2
            ENDIF
          ENDDO
        ENDIF
        GOTO 5000
C
C ORBIT PARAMETER
C ---------------
300     ISEQ=LOCQ(4,IP)
        IORB=LOCQ(6,IP)
        IF (LOCQ(3,IP).LT.900 .OR. LOCQ(3,IP).GE.951)THEN
          IF (PO(IORB).EQ.0.D0) GOTO 5000
          WEIGHT=(S0/PO(IORB))**2
          IF(ISEQ.GT.1.AND.ISEQ.LE.6) WEIGHT=(S0/PO(IORB)/ars)**2
        ELSE
          IF (PO2(IORB).EQ.0.D0) GOTO 5000
          WEIGHT=(S0/PO2(IORB))**2
          IF(ISEQ.GT.1.AND.ISEQ.LE.6)WEIGHT=(S0/PO2(IORB)/ars)**2
        END IF
        GOTO 4000
C
C AMBIGUITY PARAMETERS
C --------------------
400   IF (MIXED.NE.0.AND.MIXED.NE.3.AND.IZEROD.NE.1) THEN
        WEIGHT=(S0/200.D0)**2
        GOTO 4000
      ELSE
        GOTO 5000
      ENDIF
C
C RECEIVER ANTENNA OFFSETS
C ------------------------
500   IRAO=LOCQ(2,IP)
      ICOR=LOCQ(4,IP)
      IF (ICOR.LT.3) THEN
        ICO2=1
      ELSE
        ICO2=2
      ENDIF
      IF (SIGRAO(ICO2,IRAO).EQ.0.D0) GOTO 5000
      WEIGHT=(S0/SIGRAO(ICO2,IRAO))**2
      GOTO 4000
C
C STATION SPECIFIC TROPOSPHERE PARAMETER
C --------------------------------------
600     IREQ=LOCQ(2,IP)
        ISTA=LOCQ(3,IP)
        ICOM=LOCQ(4,IP)
        IPIP=IKF(IP,IP)
C
C ABSOLUTE A PRIORI SIGMA
        IF(ISGTRS(IREQ).EQ.0) THEN
          IF(SIGTRS(ICOM,IREQ).NE.0.D0) THEN
            WABS(ICOM)=(S0/SIGTRS(ICOM,IREQ))**2
          ELSE
            WABS(ICOM)=0.D0
          ENDIF
        ENDIF
C
C RELATIVE A PRIORI SIGMA (THE PRECEDING TROP.PARAMETERS MUST BE
C FROM THE SAME SITE)
        IF (ISGTRS(IREQ).EQ.1) THEN
          IF(ISTA.EQ.ITRSTA) THEN
            IF(SIGTRS(ICOM,IREQ).NE.0.D0) THEN
              IPLST=ITRLST(ICOM)
              IF (IPLST.NE.0) THEN
                WDIF=(S0/SIGTRS(ICOM,IREQ))**2
                IP1IP1=IKF(IPLST,IPLST)
                IP1IP =IKF(IPLST,IP)
                A(IPIP)  =A(IPIP)  +WDIF
                A(IP1IP1)=A(IP1IP1)+WDIF
                A(IP1IP) =A(IP1IP) -WDIF
              ENDIF
            ENDIF
          ELSE
            WRITE(LFNERR,601) IREQ
601         FORMAT(/,' *** SR NORINI: RELATIVE A PRIORI SIGMA',
     1               ' COULD NOT BE SET',/,
     2             16X,'FOR INDIVIDUAL TROPOSPHERE PARAMETER',/,
     3             16X,'TROPOS. PARAMETER NUMBER:',I5,/)
            IF (IFLAG .EQ. 0) CALL EXITRC(2)
          ENDIF
        ENDIF
        A(IPIP)=A(IPIP)+WABS(ICOM)
        IF (ITRSTA.NE.ISTA) ITRLST(:)=0
        ITRSTA=ISTA
        ITRLST(ICOM)=IP
        GOTO 5000
C
C LOCAL IONOSPHERE MODEL PARAMETERS
C ---------------------------------
700     CONTINUE
        IF(IONTYP.NE.1) GOTO 5000
        IF(ION001.EQ.1)THEN
          CALL IONOSI(1,NMODEL,IONREQ,IONDEV,
     1                NTERM,NM,IONCOE,IONSIG)
          ION001=0
        END IF
        NMOD=LOCQ(2,IP)
        ILAT=LOCQ(3,IP)
        ITIME=LOCQ(4,IP)
        DO 710 ITERM=1,NTERM(NMOD)
          IF(NM(ITERM,1,NMOD).EQ.ILAT.AND.NM(ITERM,2,NMOD).EQ.ITIME)
     1      THEN
            WEIGHT=(S0/IONSIG(ITERM,NMOD))**2
            GOTO 4000
          END IF
710     CONTINUE
        GOTO 5000
C
C DIFFERENTIAL CODE BIASES
C ------------------------
800     CONTINUE
C
C SATELLITE BIASES
        IF (LOCQ(2,IP).EQ.1) THEN
          IF (SIGDCB(1).EQ.0.D0) GOTO 5000
C
          IF (OPTDCB(3).GT.0) THEN
C
C CONSTRAIN BIAS OF REFERENCE SATELLITE TO ZERO
            IF (LOCQ(3,IP).EQ.OPTDCB(3)) THEN
              WEIGHT=(S0/SIGDCB(1))**2
              GOTO 4000
            ELSE
              GOTO 5000
            ENDIF
          ELSEIF (OPTDCB(3).EQ.0) THEN
C
C CONSTRAIN ALL SATELLITE BIASES TO ZERO
            WEIGHT=(S0/SIGDCB(1))**2
            GOTO 4000
          ELSE
C
C CONSTRAIN SUM OF ALL SATELLITE BIASES TO ZERO
            IPIP=IKF(IP,IP)
            IF (A(IPIP).EQ.0.D0) GOTO 5000
            WEIGHT=(S0/SIGDCB(1))**2
            DO 810 IP1=IP,NPN
              ISYS=(LOCQ(3,IP)/100)-(LOCQ(3,IP1)/100)
              IF (LOCQ(1,IP1).EQ.8 .AND.
     1            LOCQ(2,IP1).EQ.1 .AND.
     2            ISYS.EQ.0) THEN
                IP1IP1=IKF(IP1,IP1)
                IF (A(IP1IP1).EQ.0.D0) GOTO 810
                IPIP1=IKF(IP,IP1)
                A(IPIP1)=A(IPIP1)+WEIGHT
              ELSE
                IF (LOCQ(1,IP1).NE.8) GOTO 5000
              ENDIF
810         CONTINUE
            GOTO 5000
          ENDIF
        ELSEIF (LOCQ(2,IP).EQ.2) THEN
C
C RECEIVER BIASES
C ---------------
C NO CONTRAINT
          IF (SIGDCB(2).EQ.0.D0) GOTO 5000
C
          WEIGHT=(S0/SIGDCB(2))**2
C
C CONSTRAIN SUM OF ALL STATION INTERSYSTEM BIASES TO ZERO
          IPIP=IKF(IP,IP)
          IF (A(IPIP).EQ.0.D0) GOTO 5000
          DO 820 IP1=IP,NPN
             ISYS = LOCQ(5,IP)-LOCQ(5,IP1)
             IF ((LOCQ(1,IP1).EQ.8).AND.(LOCQ(2,IP1).EQ.2).AND.
     1           (ISYS.EQ.0)) THEN
                IP1IP1=IKF(IP1,IP1)
                IF (A(IP1IP1).EQ.0.D0) GOTO 820
                IPIP1=IKF(IP,IP1)
                A(IPIP1)=A(IPIP1)+WEIGHT
             ELSE
                IF (LOCQ(1,IP1).NE.8) GOTO 5000
             ENDIF
820       CONTINUE
          GOTO 5000
        ELSE
          GOTO 5000
        ENDIF
C
C LOCAL TROPOSPHERE MODEL
C -----------------------
900     KPAR=LOCQ(3,IP)
        WEIGHT=1.D20
        IF(SIGTRP(KPAR).NE.0.D0)WEIGHT=(S0/SIGTRP(KPAR))**2
        GOTO 4000
C
C EARTH ROTATION PARAMETER
C ------------------------
1000    IF(LOCQ(2,IP).EQ.1) THEN
          IKL  =LOCQ(4,IP)
          INTER=LOCQ(3,IP)
C
C         IKL=1-3= X,Y,DUT1     IKL=4,5= EPS,PSI
C
          IF (IKL.LE.3) THEN
             CALL WGTERP(ISGPOL(INTER),IP,LOCQ,TPOL,S0,
     1                   SIGPOL(IKL,INTER),A)
          ELSE
             CALL WGTERP(ISGNUT(INTER),IP,LOCQ,TPOL,S0,
     1                   SIGPOL(IKL,INTER),A)
          ENDIF
          GOTO 5000
        ELSE
          WRITE(LFNERR,1010) LOCQ(2,IP)
1010      FORMAT(/,'*** SR NORINI: POLAR WOBBLE MODEL NOT DEFINED',
     1                        16X,'MODEL:',I3,/)
          CALL EXITRC(2)
        END IF
C
C STOCHASTIC ORBIT PARAMETERS
C ---------------------------
1100    CONTINUE
        IF (LOCQ(3,IP).LT.900)THEN
C
C NEQ WRITING NOT SUPPORTED WITH A PRIORI PULSES
          IF(NPRSTC1.NE.0.AND.IRCNEQ.EQ.0) THEN
            WRITE(LFNERR,1110)
1110        FORMAT(/,' *** SR NORINI: NEQ WRITING NOT SUPPORTED ',
     1                     'WITH A PRIORI PULSES',/)
            CALL EXITRC(2)
          ENDIF
C
          DO ISTC=1,NSASTC1
           IF(LOCQ(3,IP).EQ.NUMSTC1(ISTC)) GOTO 1121
          END DO
1121      CONTINUE
          IPE=LOCQ(6,IP)
          IF(SIGSTC1(IPE,ISTC).NE.0.D0)THEN
C
C WEIGHT FOR RHS OF NORMAL EQUATIONS
            IF(NPRSTC1.NE.0) THEN
              IEXIST=0
              KARC=LOCQ(2,IP)
              KSVN=LOCQ(3,IP)
              KSTC=LOCQ(4,IP)
              KFRC=LOCQ(5,IP)
              KSAT=LOCQ(7,IP)
C
              DO 1122 KPRSTC=1,NPRSTC1
                KARC_A=LCQAPR1(2,KPRSTC)
                KSVN_A=LCQAPR1(3,KPRSTC)
                KSTC_A=LCQAPR1(4,KPRSTC)
                KFRC_A=LCQAPR1(5,KPRSTC)
                INDFRC_A=LCQAPR1(6,KPRSTC)
                KSAT_A=LCQAPR1(7,KPRSTC)
                IF(KARC_A.EQ.KARC.AND.KSVN_A.EQ.KSVN.AND.
     1             KFRC_A.EQ.KFRC.AND.INTSTCA1(KSTC_A,KSAT_A,KARC_A).EQ.
     2             INTSTC1(KSTC,KSAT,KARC)) THEN
                  IEXIST=1
                  GOTO 1123
                END IF
1122          CONTINUE
1123          CONTINUE
C
              WEIGHT=0.D0
              IF(LOCQ(5,IP).LT.10.AND.IEXIST.EQ.1) THEN
                WEIGHT=-PARSTCA1(INDFRC_A,KSTC_A,KSAT_A,KARC_A)*
     1                  SCASTC(1)*(S0/SIGSTC1(IPE,ISTC)/SCASTC(1))**2
              ELSEIF(LOCQ(5,IP).GE.10.AND.IEXIST.EQ.1) THEN
                WEIGHT=-PARSTCA1(INDFRC_A,KSTC_A,KSAT_A,KARC_A)*
     1                  SCASTC(2)*(S0/SIGSTC1(IPE,ISTC)/SCASTC(2))**2
              END IF
              B(IP)=B(IP)+WEIGHT
            END IF
C
C WEIGHT FOR NORMAL EQN. MATRIX
            IF(LOCQ(5,IP).LT.10) THEN
              WEIGHT=(S0/SIGSTC1(IPE,ISTC)/SCASTC(1))**2
            ELSE
              WEIGHT=(S0/SIGSTC1(IPE,ISTC)/SCASTC(2))**2
            END IF
          ELSE
            WEIGHT=0.D0
          END IF
C
C LEO STOCHASTIC ORBIT PARAMETERS
        ELSE
C
C NEQ WRITING NOT SUPPORTED WITH A PRIORI PULSES
          IF(NPRSTC2.NE.0.AND.IRCNEQ.EQ.0) THEN
            WRITE(LFNERR,1124)
1124        FORMAT(/,' *** SR NORINI: NEQ WRITING NOT SUPPORTED ',
     1                     'WITH A PRIORI PULSES',/)
            CALL EXITRC(2)
          ENDIF
C
          DO ISTC=1,NSASTC2
            IF(LOCQ(3,IP).EQ.NUMSTC2(ISTC)) GOTO 1126
          END DO
1126      CONTINUE
          IPE=LOCQ(6,IP)
          IF(SIGSTC2(IPE,ISTC).NE.0.D0)THEN
C
C WEIGHT FOR RHS OF NORMAL EQUATIONS
            IF(NPRSTC2.NE.0) THEN
              IEXIST=0
              KARC=LOCQ(2,IP)
              KSVN=LOCQ(3,IP)
              KSTC=LOCQ(4,IP)
              KFRC=LOCQ(5,IP)
              KSAT=LOCQ(7,IP)
C
              DO 1127 KPRSTC=1,NPRSTC2
                KARC_A=LCQAPR2(2,KPRSTC)
                KSVN_A=LCQAPR2(3,KPRSTC)
                KSTC_A=LCQAPR2(4,KPRSTC)
                KFRC_A=LCQAPR2(5,KPRSTC)
                INDFRC_A=LCQAPR2(6,KPRSTC)
                KSAT_A=LCQAPR2(7,KPRSTC)
                IF(KARC_A.EQ.KARC.AND.KSVN_A.EQ.KSVN.AND.
     1             KFRC_A.EQ.KFRC.AND.INTSTCA2(KSTC_A,KSAT_A,KARC_A).EQ.
     2             INTSTC2(KSTC,KSAT,KARC)) THEN
                  IEXIST=1
                  GOTO 1128
                END IF
1127          CONTINUE
1128          CONTINUE
C
              WEIGHT=0.D0
              IF(LOCQ(5,IP).LT.10.AND.IEXIST.EQ.1) THEN
                WEIGHT=-PARSTCA2(INDFRC_A,KSTC_A,KSAT_A,KARC_A)*
     1                  SCASTC(1)*(S0/SIGSTC2(IPE,ISTC)/SCASTC(1))**2
              ELSEIF(LOCQ(5,IP).GE.10.AND.IEXIST.EQ.1) THEN
                WEIGHT=-PARSTCA2(INDFRC_A,KSTC_A,KSAT_A,KARC_A)*
     1                  SCASTC(2)*(S0/SIGSTC2(IPE,ISTC)/SCASTC(2))**2
              END IF
              B(IP)=B(IP)+WEIGHT
            END IF
C
C WEIGHT FOR NORMAL EQN. MATRIX
            IF(LOCQ(5,IP).LT.10) THEN
              WEIGHT=(S0/SIGSTC2(IPE,ISTC)/SCASTC(1))**2
            ELSE
              WEIGHT=(S0/SIGSTC2(IPE,ISTC)/SCASTC(2))**2
            END IF
          ELSE
            WEIGHT=0.D0
          END IF
        END IF
        GOTO 4000
C
C SATELLITE ANTENNA OFFSETS
C -------------------------
1200    CONTINUE
        IOFR=LOCQ(2,IP)
        ICOR=LOCQ(3,IP)
        IGRP=LOCQ(4,IP)
        IPIP=IP*(IP+1)/2
        IF (SIGOFF(ICOR,IOFR).NE.0.D0) THEN
C
C ABSOLUTE A PRIORI SIGMA
         IF(ISGOFF(IOFR).EQ.0) THEN
            WOFABS(ICOR)=(S0/SIGOFF(ICOR,IOFR))**2
C
C RELATIVE A PRIORI SIGMA (THE PRECEDING TROP.PARAMETERS MUST BE
C FROM THE SAME SITE)
          ELSE IF(IGRP.EQ.IOFGRP(ICOR)) THEN
            WOFDIF=(S0/SIGOFF(ICOR,IOFR))**2
            IP1=IP1OFF(ICOR)
            IP1IP1=IP1*(IP1+1)/2
            IP1IP =IPIP-(IP-IP1)
            A(IPIP)=A(IPIP)+WOFDIF
            A(IP1IP1)=A(IP1IP1)+WOFDIF
            A(IP1IP)=A(IP1IP)-WOFDIF
          ELSE
            WRITE(LFNERR,1201) IOFR
1201        FORMAT(/,' *** SR NORINI: INVALID RELATIVE A PRIORI ',
     1               'SIGMA',/,
     2              16X,'FOR SATELLITE ANTENNA OFFSET PARAMETER',/,
     3              16X,'SAT.ANT. OFFSET REQUEST NUMBER:',I5,/)
            CALL EXITRC(2)
          ENDIF
          A(IPIP)=A(IPIP)+WOFABS(ICOR)
        ELSE
          WOFABS(ICOR)=0.D0
        ENDIF
        IOFGRP(ICOR)=IGRP
        IP1OFF(ICOR)=IP
        GOTO 5000
C
C EARTH POTENTIAL TERMS
C ---------------------
1300    INDEX=LOCQ(7,IP)
        IF(SIGPOT(INDEX).NE.0.D0)THEN
          WEIGHT=(S0/SIGPOT(INDEX)/SCAPOT)**2
        ELSE
          WEIGHT=0.D0
        END IF
        GOTO 4000
C
C HILL'S RESONANCE TERMS
C ----------------------
1400    INDEX=LOCQ(7,IP)
        IF(SIGHIL(INDEX).NE.0.D0)THEN
          WEIGHT=(S0/SIGHIL(INDEX)/SCAHIL)**2
        ELSE
          WEIGHT=0.D0
        END IF
        GOTO 4000
C
C ALBEDO PARAMETERS
C -----------------
1500    INDEX=LOCQ(7,IP)
        IF(SIGALB(INDEX).NE.0.D0)THEN
          WEIGHT=(S0/SIGALB(INDEX)/SCAALB)**2
        ELSE
          WEIGHT=0.D0
        END IF
        GOTO 4000
C
C CENTER OF MASS COORDINATES
C --------------------------
1600    INDEX=LOCQ(7,IP)
        IF(SIGCEN(INDEX).NE.0.D0)THEN
          WEIGHT=(S0/SIGCEN(INDEX)/SCACEN)**2
        ELSE
          WEIGHT=0.D0
        END IF
        GOTO 4000
C
C DIFFERENTIAL IONOSPHERE PARAMETERS (SEE SR WGTDIP AND REDEPO)
C -------------------------------------------------------------
1700    GOTO 5000
C
C RECEIVER ANTENNA PHASE CENTER VARIATIONS
C ----------------------------------------
1800    ICAL=LOCQ(2,IP)
        IFRQ=LOCQ(3,IP)
        NZEN=LOCQ(6,IP)
        NAZI=LOCQ(7,IP)
        IPIP=IKF(IP,IP)
        IF (SIGCAL(ICAL).NE.0.D0) THEN
          A(IPIP)=A(IPIP)+(S0/SIGCAL(ICAL))**2
        ENDIF
C
C CONSTRAIN MEAN OF ALL VALUES OF ONE REQUEST
        IF (NZEN.GT.0) THEN
          WGT=(S0/1.D-3/(NZEN*(NAZI-1)))**2
          DO 1810 IP2=IP,NPN
            IF (LOCQ(1,IP2).EQ.18    .AND.
     1          LOCQ(2,IP2).EQ.ICAL  .AND.
     2          LOCQ(3,IP2).EQ.IFRQ) THEN
              IPIP2=IKF(IP,IP2)
              A(IPIP2)=A(IPIP2)+WGT
            ENDIF
1810      CONTINUE
        ENDIF
        GOTO 5000
C
C GLOBAL IONOSPHERE MODEL PARAMETERS
C ----------------------------------
1900    IREQ=LOCQ(2,IP)
        IDEG=LOCQ(4,IP)
        IORD=LOCQ(5,IP)
C
C ABSOLUTE SIGMA
        SIGABS=SIGGIM(IREQ)
        IF (SIGABS.GT.0.D0) THEN
          WGTABS=(S0/SIGABS/SCAGIM(IREQ))**2
          IPIP=IKF(IP,IP)
          A(IPIP)=A(IPIP)+WGTABS
        ENDIF
C
C RELATIVE SIGMA
        SIGREL=SIGGIM(IREQ+2)
        IF (SIGREL.GT.0.D0) THEN
          DO IPN=IP+1,NPN
            IF (LOCQ(1,IPN).EQ.19 .AND.
     1          LOCQ(2,IPN).EQ.IREQ .AND.
     2          LOCQ(4,IPN).EQ.IDEG .AND.
     3          LOCQ(5,IPN).EQ.IORD) THEN
              WGTREL=(S0/SIGREL/SCAGIM(IREQ))**2
              IPIP=IKF(IP,IP)
              A(IPIP)=A(IPIP)+WGTREL
              IPIPN=IKF(IP,IPN)
              A(IPIPN)=A(IPIPN)-WGTREL
              IPNIPN=IKF(IPN,IPN)
              A(IPNIPN)=A(IPNIPN)+WGTREL
              GOTO 5000
            ENDIF
          ENDDO
        ENDIF
C
        GOTO 5000
C
C STATION VELOCITIES (ESTIMATED ONLY IN ADDNEQ)
C ---------------------------------------------
2000    CONTINUE
        GOTO 5000
C
C COORDINATES IN KINEMATIC MODUS (SEE SR WGTKIN IN GPSEST AND RESEPO)
C -------------------------------------------------------------------
2100    CONTINUE
        GOTO 5000
C
C SCALING FACTORS FOR VIENNA GRID FILES
C -------------------------------------
2200    IF (OPLOAD(LOCQ(2,IP))%SIGMA(LOCQ(4,IP)) .NE. 0D0 ) THEN
          IPIP=IKF(IP,IP)
          A(IPIP)=A(IPIP)+(S0/OPLOAD(LOCQ(2,IP))%SIGMA(LOCQ(4,IP)))**2
        ENDIF
        GOTO 5000
C
C EPOCH WISE STATION CLOCKS
C -------------------------
2300    CONTINUE
        GOTO 5000
C
C EPOCH WISE SATELLITE CLOCKS
C ---------------------------
2400    CONTINUE
        GOTO 5000
C
C SATELLITE ANTENNA PHASE CENTER VARIATIONS
C -----------------------------------------
2500    CONTINUE
        ISPV=LOCQ(2,IP)
        IGRP=LOCQ(3,IP)
        NZEN=LOCQ(6,IP)
        NAZI=LOCQ(7,IP)
        IPIP=IKF(IP,IP)
C
        IF (SIGSPV(ISPV).NE.0.D0) THEN
          A(IPIP)=A(IPIP)+(S0/SIGSPV(ISPV))**2
        ENDIF
C
C CONSTRAIN MEAN OF ALL VALUES OF ONE GROUP
        IF (NZEN.GT.0) THEN
          WGT=(S0/1.D-3/(NZEN*(NAZI-1)))**2
          DO 2510 IP2=IP,NPN
            IF (LOCQ(1,IP2).EQ.25    .AND.
     1          LOCQ(2,IP2).EQ.ISPV  .AND.
     2          LOCQ(3,IP2).EQ.IGRP) THEN
              IPIP2=IKF(IP,IP2)
              A(IPIP2)=A(IPIP2)+WGT
            ENDIF
2510      CONTINUE
        ENDIF
        GOTO 5000
C
C RANGE BIASES
C ------------
2600    IF(SIGRGB.NE.0.D0)THEN
          WEIGHT=(S0/SIGRGB)**2
        ELSE
          WEIGHT=0.D0
        END IF
C
C DO NOT APPLY CONSTRAINTS IF NO OBSERVATIONS
        IF (SUMOBS.EQ.0) THEN
          GOTO 5000
        ELSE
          GOTO 4000
        ENDIF
C
C HOI SCALING FACTORS
C -------------------
2700    INDEX = LOCQ(2,IP)
        IF (SIGHOI(INDEX).NE.0D0) THEN
          WEIGHT = (S0/SIGHOI(INDEX))**2D0
        ELSE
          WEIGHT = 0D0
        END IF
        GOTO 4000
C
C GNSS-SPECIFIC PARAMETERS
C ------------------------
3000    IST  = LOCQ(2,IP)
C
C DO NOT APPLY CONSTRAINTS IF NO OBSERVATIONS
        IF (SUMOBS.EQ.0) GOTO 5000
C
C STATION TRANSLATIONS
        IF(LOCQ(3,IP).EQ.1) THEN
C
C COMPUTE WEIGHT (N/E/U -> X/Y/Z)
          E2  = (AEL**2-BEL**2)/(AEL**2)
          SP  = DSIN(XEL(1,IST))
          DEN = DSQRT(1.D0-E2*SP**2)
          RN  = AEL/DEN
          RM  = AEL*(1.D0-E2)/DEN**3
          DO I=1,3
            IF (I.EQ.1) FAK = (RM+XEL(3,IST))
            IF (I.EQ.2) FAK = (RN+XEL(3,IST))
            IF (I.EQ.3) FAK = 1.D0
            DO K=1,3
              COVPLH(I,K) = 0.D0
              IF (I.EQ.K) COVPLH(I,K) = (OPTGSP%TRASIG(K)/FAK/S0)**2
              IF (I.EQ.2.AND.K.EQ.2) COVPLH(I,K) =
     1                               COVPLH(I,K)/DCOS(XEL(1,IST))**2
            ENDDO
          ENDDO
C
          CALL ERR3D(XEL(1,IST),XEL(2,IST),XEL(3,IST),AEL,BEL,
     1               +1,COVXYZ,COVPLH)
          CALL DMINV(COVXYZ,3,DET,L1,L2)
          DO I=1,3
            DO K=1,I
              IK=IKF(I+IP-1,K+IP-1)
              A(IK)=A(IK)+COVXYZ(I,K)
            ENDDO
          ENDDO
          GOTO 5000
C
C TROPOSPHERE BIAS
        ELSEIF(LOCQ(3,IP).EQ.4) THEN
          WEIGHT = 0D0
          IF (OPTGSP%TRPSIG.NE.0D0) THEN
            WEIGHT = (S0/OPTGSP%TRPSIG)**2D0
          ENDIF
          GOTO 4000
        ELSE
          GOTO 5000
        ENDIF
C
C INTRODUCE WEIGHT
C ----------------
4000    IPIP=IP*(IP+1)/2
        A(IPIP)=A(IPIP)+WEIGHT
C
C NEXT PARAMETER
C --------------
5000  CONTINUE
C
C DELLOACTE ARRAYS
C ----------------
      IF(NSASTC1.NE.0) DEALLOCATE(lcqApr1,stat=irc)
      IF(NSASTC2.NE.0) DEALLOCATE(lcqApr2,stat=irc)
C
      RETURN
      END SUBROUTINE

      END MODULE
