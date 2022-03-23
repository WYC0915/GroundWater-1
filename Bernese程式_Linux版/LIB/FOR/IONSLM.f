      MODULE s_IONSLM
      CONTAINS

C*
      SUBROUTINE IONSLM(NFIL  ,HEDFIS,OBSFIS,MTYPS ,NEPOS ,NSATS ,
     1                  IDELTS,TIMRES,NUMSAS,XSTAS ,XSTELS,PRTLEV,
     2                  ICARR ,IQ    ,INTMAX,RMSOBS,NIONP ,MINELV,
     3                  IDEVLA,IDEVHA,IDEVMI,HION  ,LOCQ  ,IFRMAT,
     4                  INUMOB,MOBS  ,XX    ,RMS   ,RMSIP ,FLAG  ,
     5                  OBSFLG,INDA  ,INDP  ,IDEL  ,FILNUM,NRSAT ,
     6                  IFRQS ,AMBIEP,NUMAMB,OFFS  ,OFFSET,VSIG  ,
     7                  AII   ,A0I   ,AOBS  ,BOBS  ,ANOR  ,BNOR  ,
     8                  TIMOBS,DPHI  ,OBSERV,IONDEV,MODNR)
CC
CC NAME       :  IONSLM
CC
CC PURPOSE    :  COMPUTES A SINGLE LAYER IONOSPHERE MODEL
CC
CC PARAMETERS :
CC         IN :  NFIL   : NUMBER OF FILES OF CURRENT SESSION  I*4
CC               HEDFIS : HEADER FILE NAMES
CC                        HEDFIS(I),I=1,NFIL                 CH*32(*)
CC               OBSFIS : OBSERVATION FILE NAMES
CC                        OBSFIS(I),I=1,NFIL                 CH*32(*)
CC               MTYPS  : MEASUREMENT TYPE
CC                        MTYPS(I),I=1,NFIL                   I*4(*)
CC               NEPOS  : NUMBER OF EPOCHS
CC                        NEPOS(I),I=1,NFIL                   I*4(*)
CC               NSATS  : NUMBER OF SATELLITES
CC                        NSATS(I),I=1,NFIL                   I*4(*)
CC               IDELTS : SAMPLING RATE
CC                        IDELTS(I),I=1,NFIL                  I*4(*)
CC               TIMRES : REFERENCE EPOCH
CC                        TIMRES(I),I=1,NFIL                  I*4(*)
CC               NUMSAS : SATELLITE NUMBERS
CC                        NUMSAS(I,J),I=1,MAXSAT,J=1,NFIL     I*4(*,*)
CC               XSTAS  : STATION COORDINATES
CC                        XSTATS(I,J,K),I=1,3,J=1,2,K=1,NFIL R*8(*,*,*)
CC               XSTELS : STATION COORDINATES (ELLIPSOIDAL)
CC                        XSTELS(I,J,K),I=1,3,J=1,2,K=1,NFIL R*8(*,*,*)
CC               PRTLEV : PRINT LEVEL FOR PREPROCESSING
CC                        (0:NO MESSAGES PRINTED,1: PRINT)    I*4
CC               ICARR  : CARRIER TO BE USED IN PREPROCESSING I*4
CC               IQ     : POLYNOMIAL DEGREE FOR PREPROCESSING I*4
CC               INTMAX : MAXIMUN INTERVAL (MINUTES)          I*4
CC               RMSOBS : RMS OF A SINGLE OBSERVATION         R*8
CC               NIONP  : NUMBER OF IONOSPHERE PARAMETERS     I*4
CC               MINELV : MINIMAL ELEVATION                   I*4
CC               IDEVLA : DEGREE OF DEVELOPMENT IN LATITUDE   I*4
CC               IDEVHA :    "          "          HOUR ANGLE I*4
CC               IDEVMI :    "          "          MIXED      I*4
CC               HION   : HEIGHT OF IONOSPHERIC LAYER         R*8
CC               LOCQ   : PARAMETER DEFINITION
CC                        LOCQ(I,J),I=1,MAXLCQ, J=1,MAXPAR    I*4(*,*)
CC               IFRMAT : FILE FORMAT, IFRMAT(I),I=1,NFIL     I*4(*)
CC        OUT :  INUMOB : NUMBER OF OBSERVATIONS (ONE FILE)
CC                        INUMOB(I),I=1,NFIL                  I*4(*)
CC               MOBS   : TOTAL NUMBER OF OBSERVATIONS        I*4
CC               XX     : ESTIMATED PARAMETERS
CC                        XX(I),I=1,MAXPAR                    R*8(*)
CC               RMS    : RMS OF UNIT WEIGHT                  R*8
CC               RMSIP  : RMS OF IONOSPHERE PARAMETERS
CC                        RMSIP(I),I=1,MAXPAR                 R*8(*)
CC               FLAG   : OBSERVATION FLAG
CC                        FLAG(I,J,K),I=1,MAXEPO,J=1,MAXSAT,
CC                                    K=1,2                  CH*1(*,*,*)
CC               OBSFLG : OBSERVATION FLAG
CC                        OBSFLAG(I,J),I=1,MAXSAT,J=1,MAXFRQ CH*1(*,*)
CC               INDA   : AUXILIARY ARRAY; INDA(I),I=1,MAXPAR I*2(*)
CC               INDP   : AUXILIARY ARRAY; INDP(I),I=1,MAXPAR I*4(*)
CC               IDEL   : AUXILIARY ARRAY; IDEL(I),I=1,MAXPAR I*4(*)
CC               FILNUM : CURRENT FILE NUMBER
CC                        FILNUM(I),I=1,MAXFIL                I*4(*)
CC               NRSAT  : SATELLITE NUMBERS (ONE EPOCH)
CC                        NRSAT(I),I=1,MAXSAT                 I*4(*)
CC               IFRQS  : FREQUENCIES
CC                        IFRQS(I),I=1,MAXFRQ                 I*4(*)
CC               AMBIEP : NEW AMBIGUTIY AT EPOCH "AMBIEP"
CC                        AMBIEP(I,J),I=1,MAXAMP,J=1,MAXSAT   I*4(*,*)
CC               NUMAMB : NUMBER OF AMBIGUITIES PER SATELLITE
CC                        NUMAMB(I),I=1,MAXSAT                I*4(*)
CC               OFFS   : PHASE OBSERVATION OFFSET
CC                        OFFS(I),I=1,MAXSAT                  R*8(*)
CC               OFFSET : PHASE OBSERVATION OFFSET
CC                        OFFSET(I,J),I=1,MAXAMP,J=1,MAXSAT   R*8(*,*)
CC               VSIG   : AUXILIARY ARRAY; VSIG(I),I=1,MAXPAR R*8(*)
CC               AII    : AUXILIARY ARRAY
CC                        AII(I),I=1,MAXAMP*(MAXAMP+1)/2      R*8(*)
CC               A0I    : AUXILIARY ARRAY
CC                        A0I(I),I=1,MAXPAR*MAXSAT            R*8(*)
CC               AOBS   : ELEMENTS OF FIRST DESIGN MATRIX
CC                        (ELEMENTS FOR ONE OBS. EPOCH)
CC                        AOBS(I),I=1,MAXPAR                  R*8(*)
CC               BOBS   : RIGHT HAND SIDE OF OBS. EQUATION
CC                        BOBS(I),I=1,MAXPAR                  R*8(*)
CC               ANOR   : NORMAL EQUATION MATRIX
CC                        ANOR(I),I=1,MAXPAR*(MAXPAR+1)/2     R*8(*)
CC               BNOR   : RIGHT HAND SIDE OF NORMAL EQUATIONS
CC                        BNOR(I),I=1,MAXPAR                  R*8(*)
CC               TIMOBS : OBSERVATION EPOCHS
CC                        TIMOBS(I),I=1,MAXEPO                R*8(*)
CC               DPHI   : OBSERVATIONS (PHASE OR CODE)
CC                        DPHI(I,J,2),I=1,MAXEPO,J=1,MAXSAT   R*8(*,*,*)
CC               OBSERV : OBSERVATIONS OF ONE EPOCH
CC                        OBSERV(I,J),I=1,MAXSAT,J=1,MAXFRQ   R*8(*,*)
CC               IONDEV : ION. MODEL DESCRIPTION
CC                        IONDEV(8,I),I=1,MAXMOD              R*8(8,*)
CC               MODNR  : IONOSPHERE MODEL NUMBER             I*4
CC
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  U. WILD
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  89/08/04 14:29
CC
CC CHANGES    :  04-JUN-92 : ??: NEW PARAMETERS IN "GETORB" AND "COOTRA"
CC                               OPTION SYSTEM J2000.0
CC               15-JUN-93 : ??: TEST FOR TIMOBS(IOBS)=0.D0
CC               25-JUN-93 : ??: IDER=2, XSAT(9) NECESSARY FOR SR TOPSTA
CC               29-OCT-93 : ??: HANDLE SATELLITES MISSING IN STD.ORBIT
CC               08-APR-94 : MR: ADD PARAM. "AZI" TO CALL OF TOPSTA
CC               21-APR-94 : MR: NEW PARAMETER "SIGAPR" FOR SR SIGMA1
CC               10-AUG-94 : MR: CALL EXITRC
CC               14-JAN-97 : TS: CALL OF TOPSTA CORRECTED
CC               09-FEB-02 : RD: P: REAL*4->REAL*8 (BECAUSE ADDNOR)
CC               04-DEC-02 : RS: "NAD" AND "AZISAT" TO CALL OF TOPSTA
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               11-Jul-05 : MM: NAD CORRECTLY DECLARED AS REAL
CC               03-FEB-06 : RD: ENABLE MODULE FOR SIGMA1
CC               25-OCT-10 : CR: NEW CALL OF TOPSTA
CC               04-MAY-12 : RD: USE DMOD FROM MODULE, USE M_BERN WITH ONLY
CC               10-JUL-12 : RD: USE SYMINVG INSTEAD OF SYMIN8
CC               10-JUL-12 : RD: REMOVE UNUSED VARIABLES FROM REDSES
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnprt, lfnerr
      USE d_const,  ONLY: PI
      USE l_basfun, ONLY: dmod
      USE s_neqini
      USE f_sigma1
      USE f_tstflg
      USE s_cootra
      USE s_getorb
      USE s_ionbsf
      USE s_redses
      USE s_solve
      USE s_syminvg
      USE s_ionpar
      USE s_ionobs
      USE s_ionprp
      USE s_exitrc
      USE s_addnor
      USE s_topsta
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAMB  , ICARR , ICRARC, IDEG  , IDER  , IDEVHA, IDEVLA,
     1          IDEVMI, IEPOCH, IFIL  , IFRQ  , II    , INTMAX, IOBS  ,
     2          IOBSAV, IORSYS, IP    , IPAR  , IPTYP , IQ    , IRC   ,
     3          ISAT  , ISING , JCARR , KDEG  , MAXEQN, MINELV, MOBS  ,
     4          MODNR , MODTYP, MXCAMP, MXCEPO, MXCFIL, MXCFRQ, MXCLCQ,
     5          MXCMOD, MXCPAR, MXCSAT, NAMBP , NEQ   , NFIL  ,
     6          NFLSES, NIONP , NOBS  , NPAR  , NPARMS, NPARN
C
      REAL*8    AZI   , AZISAT, B0    , CZP1  , D     , DB    , DELTAT,
     1          DS    , DT    , ELEV  , FACTOR, HION  , RMS   , RMSOBS,
     2          RMSSUM, S0    , SIGAPR, SZ    , TIMEND, TOSC  ,
     3          UT1GPS, WGTGEN, XPOL  , YPOL  , ZD    , ZEN   , ZP, NAD,
     4          AZISOK
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
C DECLARATIONS
C ------------
      REAL*8        IONDEV(8,MXCMOD),OBSERV(MXCSAT,MXCFRQ)
      REAL*8        DPHI(MXCEPO,MXCSAT,2),TIMOBS(MXCEPO),DX(3)
      REAL*8        XSTAS(3,2,MXCFIL),XSTELS(3,2,MXCFIL),XSAT(9),ELE(7)
      REAL*8        ANOR(MXCPAR*(MXCPAR+1)/2),BNOR(MXCPAR)
      REAL*8        AOBS(MXCPAR),BOBS(MXCPAR),HELP(1)
      REAL*8        A0I(MXCPAR*MXCSAT),AII(MXCAMP*(MXCAMP+1)/2)
      REAL*8        XX(MXCPAR),VSIG(MXCPAR),OFFSET(MXCAMP,MXCSAT)
      REAL*8        OFFS(MXCSAT),TIMRES(MXCFIL)
      REAL*8        RMSIP(MXCPAR)
C
      REAL*8        P(1)
C
      INTEGER*4     NUMAMB(MXCSAT),AMBIEP(MXCAMP,MXCSAT)
      INTEGER*4     IFRQS(MXCFRQ),NRSAT(MXCSAT),INUMOB(MXCFIL)
      INTEGER*4     FILNUM(MXCFIL),IDEL(MXCPAR),INDP(MXCPAR)
      INTEGER*4     MTYPS(MXCFIL),NEPOS(MXCFIL),NSATS(MXCFIL)
      INTEGER*4     IDELTS(MXCFIL)
      INTEGER*4     NUMSAS(MXCSAT,MXCFIL),LOCQ(MXCLCQ,MXCPAR)
      INTEGER*4     PRTLEV,NOBSVN(100),IFRMAT(MXCFIL)
C
      INTEGER*4     INDI(1),INDK(1),INDA(MXCPAR)
C
      CHARACTER*32  HEDFIS(MXCFIL),OBSFIS(MXCFIL)
      CHARACTER*6   MXNSAT,MXNFIL,MXNLCQ,MXNPAR,MXNMOD,MXNFRQ
      CHARACTER*6   MXNEPO,MXNAMP
      CHARACTER*1   OBSFLG(MXCSAT,MXCFRQ),FLAG(MXCEPO,MXCSAT,2)
C
C COMMONS
C -------
      COMMON/MCMFIL/MXCFIL,MXNFIL
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMPAR/MXCPAR,MXNPAR
      COMMON/MCMMOD/MXCMOD,MXNMOD
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
      COMMON/MCMEPO/MXCEPO,MXNEPO
      COMMON/MCMAMP/MXCAMP,MXNAMP
C
C LOGICAL FILE NUMBERS, CONSTANTS
C -------------------------------
C
      DATA  MODTYP/1/
C
C INITIALIZATION
C --------------
      IF (ICARR .EQ. 3) THEN
        IFRQ = 2
      ELSE IF (ICARR .EQ. 4) THEN
        IFRQ = 1
      ENDIF
C
      IOBSAV = 0
      MOBS   = 0
      DO 10 IFIL = 1,MXCFIL
        INUMOB(IFIL) = 0
10    CONTINUE
      RMSSUM = 0.D0
C
C INITIALIZE IONOSPHERE PARAMETERS / COMPUTE START/END OF MODEL
C APPLICABILITY / COMPUTE DEVELOPMENT ORIGIN
C -------------------------------------------------------------
      CALL IONPAR(NFIL,TIMRES,NEPOS,IDELTS,XSTELS,IDEVLA,IDEVHA,
     1            IDEVMI,IONDEV,LOCQ,NIONP,MODNR)
      NPARMS = NIONP
C
C INITIALIZE NORMAL EQUATION MATRIX
C ---------------------------------
      IPTYP = 1
      NAMBP = 0
      CALL NEQINI(IPTYP,NIONP,NAMBP,MXCPAR,ANOR,BNOR)
C
C LOOP OVER ALL INPUT FILES
C -------------------------
      DO 1000 IFIL = 1,NFIL
C
        IDER = 2
        NEQ  = 0
        IEPOCH = 0
        DO 1010 ISAT = 1,MXCSAT
          OFFS(ISAT) = 0.D0
1010    CONTINUE
C
C INITIALIZE ARRAYS AND READ OBSERVATIONS OF ONE FILE
C ---------------------------------------------------
        CALL IONOBS(HEDFIS(IFIL),OBSFIS(IFIL),MTYPS(IFIL),TIMRES(IFIL),
     1              ICARR,IDELTS(IFIL),NSATS(IFIL),NUMSAS(1,IFIL),
     2              IFRMAT(IFIL),NOBS,DPHI,FLAG,OBSFLG,NRSAT,IFRQS,
     3              OBSERV,TIMOBS,NOBSVN,TIMEND)
C
C PREPROCESSING
C -------------
        IF (PRTLEV .GT. 0) THEN
          WRITE(LFNPRT,1011) HEDFIS(IFIL)
1011      FORMAT(//,1X,71('*'),/,' FILE NAME: ',A32,/,1X,71('*'),/)
        ENDIF
        CALL IONPRP(MODTYP,IFIL,MTYPS(IFIL),NOBS,PRTLEV,ICARR,
     1              NSATS(IFIL),NUMSAS(1,IFIL),DPHI,FLAG,IDELTS(IFIL),
     2              TIMRES(IFIL),TIMOBS,IQ,RMSOBS,INTMAX,
     3              LOCQ,NIONP,NUMAMB,AMBIEP,NAMBP,OFFSET)
C
C INITIALIZE NORMAL EQUATION MATRIX (AMBIGUITY PARAMETERS)
C --------------------------------------------------------
        IPTYP = 2
        CALL NEQINI(IPTYP,NIONP,NAMBP,MXCPAR,ANOR,BNOR)
C
C TOTAL NUMBER OF PARAMETERS
C --------------------------
        NPAR = NIONP+NAMBP
C
C COMPUTE ELEMENTS OF FIRST DESIGN MATRIX
C ---------------------------------------
C
        B0=IONDEV(1,MODNR)
        S0=DMOD(IONDEV(2,MODNR),1.D0)*2.D0*PI+IONDEV(3,MODNR)-PI
        S0=DMOD(S0+10.D0*PI,2.D0*PI)
C
C LOOP OVER ALL OBSERVATIONS
C --------------------------
        DO 300 IOBS=1,NOBS
C
          IF (TIMOBS(IOBS).EQ.0.D0) GOTO 300
C
          DELTAT = TIMOBS(IOBS) - TIMRES(IFIL)
          IEPOCH = IDNINT((DELTAT*86400.D0)/IDELTS(IFIL))+1
C
C LOOP OVER ALL SATELLITES
C ------------------------
          DO 200 ISAT=1,NSATS(IFIL)
C
            DO 110 II=1,NPAR
              AOBS(II) = 0.D0
              INDA(II) = 0
110         CONTINUE
C
            IF (MTYPS(IFIL) .EQ. 1) THEN
              DO 120 IAMB = 1,NUMAMB(ISAT)
                IF (IEPOCH .EQ. AMBIEP(IAMB,ISAT)) THEN
                  OFFS(ISAT) = OFFSET(IAMB,ISAT)
                ENDIF
120           CONTINUE
            ENDIF
C
C OBSERVATION IN TIME INTERVAL?
C -----------------------------
            IF(TIMOBS(IOBS).LT.IONDEV(4,MODNR).OR.
     1         TIMOBS(IOBS).GT.IONDEV(5,MODNR)) GOTO 200
C
C OBSERVATION EQUAL ZERO OR FLAGED?
C ---------------------------------
            IF (DPHI(IOBS,ISAT,IFRQ) .EQ. 0.D0 .OR.
     1          TSTFLG(FLAG(IOBS,ISAT,IFRQ),0)) GOTO 200
C
C IONOSPHERE PARAMETERS
C ---------------------
C
C GET SATELLITE COORDINATES
C -------------------------
            DO 130 II = 1,9
              XSAT(II) = 0.D0
130         CONTINUE
            CALL GETORB(NUMSAS(ISAT,IFIL),1,IDER,2,TIMOBS(IOBS),
     1                  ICRARC,IORSYS,XSAT,TOSC,ELE,IRC)
            IF (XSAT(1) .EQ. 0.D0 .OR. IRC.NE.0) GOTO 200
            CALL COOTRA(IORSYS,IDER,TIMOBS(IOBS),XSAT,SZ,XPOL,YPOL,
     1                  UT1GPS)
C
C COMPUTE ZENITH DISTANCE
C -----------------------
            DT = 0.D0
            CALL TOPSTA(XSTAS(1,1,IFIL),XSAT,SZ,DT,XPOL,YPOL,DX,D,
     1                  ZEN,AZI,2,NAD,AZISAT,TIMOBS(IOBS),
     2                  NUMSAS(ISAT,IFIL),IORSYS,AZISOK,0)
            ZD = ZEN*(180.D0/PI)
            ELEV = 90.D0 - ZD
            IF (IDINT(ELEV) .LE. MINELV) GOTO 200
C
C COMPUTE INTERSECTION POINT REC/SAT <--> IONOSPHERE LAYER
C --------------------------------------------------------
            JCARR = 4
            CALL IONBSF(XSTAS(1,1,IFIL),XSAT,TIMOBS(IOBS),SZ,ZEN,
     1                  HION,JCARR,B0,S0,IONDEV(6,1),IONDEV(7,1),
     2                  IONDEV(8,1),FACTOR,DB,DS,ZP)
            CZP1=DCOS(ZP)
C
C ELEMENTS OF FIRST DESIGN MATRIX
C -------------------------------
            DO 140 IP = 1,NIONP
              KDEG=LOCQ(3,IP)
              IDEG=LOCQ(4,IP)
              AOBS(IP)=FACTOR*(DS**IDEG*DB**KDEG)/CZP1
              IF (MTYPS(IFIL) .EQ. 2) AOBS(IP) = -AOBS(IP)
              INDA(IP) = IP
140         CONTINUE
C
C AMBIGUITY PARAMETERS FOR FILE (IFIL)
C ------------------------------------
            IF (MTYPS(IFIL) .EQ. 1) THEN
              DO 160 IPAR=NIONP+1,NPAR
                IF (LOCQ(3,IPAR) .EQ. NUMSAS(ISAT,IFIL)) THEN
                  IF (IEPOCH .GE. LOCQ(6,IPAR) .AND.
     1                IEPOCH .LE. LOCQ(7,IPAR)      ) THEN
                    AOBS(NIONP+1) = 1.
                    INDA(NIONP+1) = IPAR
                    GOTO 170
                  ENDIF
                ENDIF
160           CONTINUE
            ENDIF
C
C UPDATE NORMAL EQUATION SYSTEM
C -----------------------------
170         NEQ = 1
            MAXEQN=1
            WGTGEN = 1.
            P(1) = 1.D0
            DPHI(IOBS,ISAT,IFRQ) = DPHI(IOBS,ISAT,IFRQ)-OFFS(ISAT)
            CALL ADDNOR(NEQ,NPAR,WGTGEN,P,AOBS,INDA,INDI,INDK,HELP,
     1                  MAXEQN,DPHI(IOBS,ISAT,IFRQ),
     2                  ANOR,BNOR,RMSSUM,MOBS)
C
C NEXT SATELLITE
C --------------
200       CONTINUE
C
C NEXT EPOCH
C ----------
300     CONTINUE
C
C STATISTICS
C ----------
        INUMOB(IFIL) = MOBS - IOBSAV
        IOBSAV = MOBS
C
C PREELIMINATE AMBIGUITY PARAMETERS OF CURRENT FILE
C -------------------------------------------------
        NFLSES = 1
        FILNUM(1) = IFIL
        NPARN = NPAR - NAMBP
        NPARMS = NPARMS + NAMBP
        CALL REDSES(NFLSES,FILNUM,NPAR,NAMBP,NPARN,NPAR,NPARMS,
     1              ANOR,BNOR,LOCQ,RMSSUM,AII,A0I,IDEL)
C
C NEXT INPUT FILE
C ---------------
1000  CONTINUE
C
C INVERSION OF NORMAL EQUATION SYSTEM
C -----------------------------------
      CALL SYMINVG(NIONP,ANOR,0,ISING)
      IF (ISING .NE. 0) THEN
        WRITE(LFNERR,1001)
1001    FORMAT(/,' *** SR IONSLM: NEQ - MATRIX SINGULAR ',/)
        CALL EXITRC(2)
      ENDIF
C
C COMPUTE SOLUTION
C ----------------
      CALL SOLVE(NIONP,ANOR,BNOR,XX)
C
C COMPUTE RMS
C -----------
      SIGAPR=1.D0
      RMS=SIGMA1(0,NIONP,NPARMS,MOBS,XX,INDP,AOBS,INDA,BOBS,
     1          P(1),BNOR,RMSSUM,VSIG,SIGAPR)
C
C COMPUTE RMS OF THE IONOSPHERE PARAMETERS
C ----------------------------------------
      DO 1100 IPAR = 1,NIONP
        IF (ANOR(IPAR*(IPAR+1)/2) .GE. 0.D0) THEN
          RMSIP(IPAR) = RMS*DSQRT(ANOR(IPAR*(IPAR+1)/2))
        ELSE
          RMSIP(IPAR) = 999.D0
        ENDIF
1100  CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
