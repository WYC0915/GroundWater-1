      MODULE s_PDSTC2
      CONTAINS

C*
      SUBROUTINE PDSTC2(LOCQ,TOBS,TIMSTC,
     1                  TUV,SCAL,DRDELE,NDIFF,ONEOR2,DRDACC,DER)
CC
CC NAME       :  PDSTC2
CC
CC PURPOSE    :  COMPUTE PARTIALS WITH RESPECT TO STOCHASTIC ORBIT
CC               PARAMETERS USING THE PARTIALS W.R.T. TO THE INITIAL
CC               ORBIT ELEMENTS.
CC               BASED ON THE SUBROUTINE PDSTC.
CC
CC PARAMETERS :
CC        IN  :  LOCQ   : CHARACTERIZATION OF PARAMETER         I*4(*)
CC               TOBS   : OBSERVATION TIME                      R*8
CC               TIMSTC : STOCHASTIC EPOCHS                     R*8(*,*,*,*)
CC               TUV    : UNIT VECTORS STATION --> SATELLITE    R*8(*,*)
CC               SCAL   : SCALING FACTOR FOR                    R*8(*)
CC                        (1): STOCHASTIC PULSES
CC                        (2): STOCHASTIC ACCELERATIONS
CC               DRDELE : PARTIALS OF POSITION VECTOR OF        R*8(3,*)
CC                        SATELLITE WITH RESPECT TO ELEMENTS
CC                        A, E, I, NODE, PER, U0
CC               NDIFF  : DIFFERENCE TYPE                       I*4
CC                        NDIFF=0: ZERO DIFFERENCE
CC                        NDIFF=1: SINGLE DIFFERENCE
CC               ONEOR2 : INDICATOR FOR LEO AS A STATION        I*4
CC                        IN DD PROCESSING
CC                        ONEOR2=1 LEO IS FIRST STATION
CC                        ONEOR2=2 LEO IS SECOND STATION
CC               DRDACC : PARTIALS OF POSITION VECTOR OF        R*8(3,*)
CC                        SATELLITE WITH RESPECT TO
CC                        CONSTANT ACCELERATIONS
CC       OUT  :  DER    : RESULTING PARTIAL DERIVATIVE          R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G. BEUTLER
CC
CC VERSION    :  4.0 (JAN 96)
CC
CC CREATED    :  25-JAN-96
CC
CC CHANGES    :  23-SEP-97 : DI: USE INCLUDE 'MAXSAT.inc'
CC               21-MAY-01 : DS: USE SR GETORF, PRTDE2 AND GTFLNA FOR LEO
CC               02-JUN-01 : DS: NEW PARAMETERS: NDIFF, ONEOR2
CC               02-JUN-01 : DS: DER COMPUTATION FOR LEO (ZD AND DD PROCESSING)
CC                               DEPENDING ON NDIFF
CC               20-JUN-01 : DS: MAXSTC:30->100
CC               14-JAN-02 : DS: MAXSTC:100->245
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               23-FEB-03 : MR: ADD SLR TO LEO DATA CASE
CC               10-DEC-03 : AJ: ADD PARTIALS W.R.T. ACCELERATIONS
CC               10-MAR-04 : HB: REMOVE ONLY-STATEMENT FOR M_BERN
CC               16-FEB-05 : AJ: TOLERANCE DTTOL INTRODUCED
CC               06-APR-05 : AJ: INTERFACE TO PRTDER, PRTDE2
CC               16-JUN-05 : MM: UNUSED COMCONST.INC REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               23-JUL-05 : HU: MISSING DECLARATIONS ADDED
CC               30-MAY-07 : AG: USE S_SUNEFF
CC               24-JUN-08 : DT: SLR SATELLITES TREATED AS GNSS
CC                               (ASSUMING SVN>=951)
CC               28-OCT-08 : DT: USE MAXVAR FROM M_MAXDIM
CC               10-FEB-10 : HB: BUG FIXED FOR DYX-PULSES
CC               01-OCT-10 : CR: NEW CALL OF SUNEFF
CC               03-DEC-10 : HB: ADD PARAMETER FOR SR PRTDER AND PRTDE2
CC               28-MAR-12 : RD: USE SVNSYS AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnerr, fileNameLength
      USE m_maxdim, ONLY: MAXSAT, maxVar
      USE p_gpsest, ONLY: maxLcq, maxstc
      USE s_prtde2
      USE s_prtder
      USE s_cootra
      USE s_vprod
      USE s_getorb
      USE s_maxtst
      USE s_getorf
      USE s_dminv
      USE s_suneff
      USE f_svnsys
      USE s_exitrc
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IAOLD , IARC  , ICRARC, IFIRST, INDSTC, IORSYS,
     1          IP    , IPAR  , IPE   , IRC   , IRCORB, ISA   , ISAT  ,
     2          IST   , ISTART, K     , KPAR  , L     , MODSTC, MXCSAT,
     3          MXCSTC, MXCVAR, NDIFF , NRAD  , NREQ  , NUMSVN, NVAR
C
      REAL*8    DER   , DET   , DEW   , DEY   , DTTOL , DT21  , DT32  ,
     1          DT31  , RSAT  , RSUN  , SZ    , TDT   , TFAC  ,
     2          TOBS  , TOSC  , UT1GPS, XPOL  , YPOL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMSTC/MXCSTC,MXNSTC
      COMMON/MCMVAR/MXCVAR,MXNVAR
C
C GLOBAL DECLARATIONS
      CHARACTER*6 MXNSAT,MXNSTC,MXNVAR
      CHARACTER*8 ANLTYP
      CHARACTER*32 CHDUMM
      REAL*8      TIMSTC(3,MXCSTC,MXCSAT,*),TUV(3,*)
      REAL*8      DRDELE(3,*),DRDACC(3,*),SCAL(*)
      INTEGER*4   LOCQ(*),ONEOR2

C
C LOCAL DECLARATIONS

      CHARACTER(LEN=fileNameLength)   :: FILORB
C
      PARAMETER  (DTTOL=1.0D-8)
C
      REAL*8      ELESAT(7),Z0(6,6),RPRPAR(MAXVAR),XVSAT(6)
      REAL*8      XSUN(4),ESUN(3),ER(3),ES(3),EW(3),EX(3),EY(3)
      REAL*8      HELP(6),COEMAT(6,3,MAXSTC,MAXSAT),TSTC(3),DRDSTC(3)
      REAL*8      HELPO(6),HELPD(6),DUM3(3)
      REAL*8      COEMAT2(6,3,MAXSTC,MAXSAT),HLPACC(6)
      REAL*8      HLPACCO(6),HLPACCD(6)
      REAL*8      COEMAT3(6,3,MAXSTC,MAXSAT)
      INTEGER*4   FRCTYP,L1(6),M1(6),STCDEF(3,MAXSTC,MAXSAT)
      INTEGER*4   STCDEF2(3,MAXSTC,MAXSAT),STCDEF3(3,MAXSTC,MAXSAT)
C
CC      INCLUDE 'COMLFNUM.inc'
      DATA IAOLD/-1/,IFIRST/1/
C
C MAXIMUM DIMENSION
C -----------------
      IF(IFIRST.EQ.1)THEN
        IFIRST=0
        CALL MAXTST(1,'PDSTC2 ',MXNSAT,MAXSAT,MXCSAT,IRC)
        IF(IRC.NE.0) CALL EXITRC(2)
        CALL MAXTST(1,'PDSTC2 ',MXNSTC,MAXSTC,MXCSTC,IRC)
        IF(IRC.NE.0) CALL EXITRC(2)
        CALL MAXTST(1,'PDSTC2 ',MXNVAR,MAXVAR,MXCVAR,IRC)
        IF(IRC.NE.0) CALL EXITRC(2)
      END IF
C
C EXTRACT NECESSARY INFORMATION FROM LOCQ
C ---------------------------------------
      IARC     =LOCQ(2)
      NUMSVN   =LOCQ(3)
      INDSTC   =LOCQ(4)
      FRCTYP   =LOCQ(5)
      IPE      =LOCQ(6)
      ISAT     =LOCQ(7)
      TSTC(1)  =TIMSTC(1,INDSTC,ISAT,IARC)
      TSTC(2)  =TIMSTC(2,INDSTC,ISAT,IARC)
      TSTC(3)  =TIMSTC(3,INDSTC,ISAT,IARC)
C
      IF(FRCTYP.LT.10) THEN
        MODSTC=0
      ELSEIF(FRCTYP.LT.20) THEN
        MODSTC=1
      ELSEIF(FRCTYP.LT.30) THEN
        MODSTC=2
        DT21=TSTC(2)-TSTC(1)
        DT32=TSTC(3)-TSTC(2)
        DT31=TSTC(3)-TSTC(1)
        IF(DT21.EQ.0.D0.OR.DT32.EQ.0.D0) THEN
          TFAC=0.D0
        ELSE
          TFAC=DT31/(DT32*DT21)
        END IF
      END IF
C
C DEFINE DERIVATIVE
C -----------------
      IF(TOBS.LT.(TSTC(1)-DTTOL))THEN
        DO 2 K=1,3
          DRDSTC(K)=0.D0
2       CONTINUE
        DER=0.D0
      ELSE
C
C NEW ARC ?
        IF(IARC.NE.IAOLD)THEN
          IAOLD=IARC
          DO 1 IP=1,3
            DO 1 IST=1,MAXSTC
              DO 1 ISA=1,MAXSAT
                STCDEF(IP,IST,ISA)=0
                STCDEF2(IP,IST,ISA)=0
                STCDEF3(IP,IST,ISA)=0
1         CONTINUE
        END IF
C
C IS ARRAY DELEDP ALREADY DEFINED ?
        IF(STCDEF(IPE,INDSTC,ISAT).EQ.0)THEN
          STCDEF(IPE,INDSTC,ISAT)=1
C
C GET SATELLITE POSITION AND VELOCITY AT TIME TSTC(1)
C ------------------------------------------------
          IF(NUMSVN.LT.900 .OR. NUMSVN.GE.951) THEN
            CALL GETORB(NUMSVN,0,1,2,TSTC(1),ICRARC,IORSYS,XVSAT,
     1                  TOSC,ELESAT,IRCORB)
          ELSE
            CALL GTFLNA(1,'LEOSTD ',FILORB,IRC)
            CALL GETORF(FILORB,NUMSVN,0,1,2,TSTC(1),ICRARC,IORSYS,XVSAT,
     1                  TOSC,ELESAT,IRCORB)
          END IF
C
C GET PARTIAL DERIVATIVES W.R.T. OSC. ELEMENTS AT TIME TSTC(1)
C ------------------------------------------------------------
          DO 10 IPAR=1,6
            CHDUMM = ' '
            IF(NUMSVN.LT.900 .OR. NUMSVN.GE.951) THEN
              CALL PRTDER(CHDUMM,NUMSVN,IPAR,1,1,TSTC(1),1,ICRARC,
     1                    IORSYS,NVAR,NRAD,Z0(1,IPAR),ELESAT,RPRPAR,
     2                    ANLTYP,IRC)
            ELSE
              CALL PRTDE2(CHDUMM,NUMSVN,IPAR,1,1,TSTC(1),1,ICRARC,
     1                    IORSYS,NVAR,NRAD,Z0(1,IPAR),ELESAT,RPRPAR,
     2                    ANLTYP,IRC)
            END IF
10        CONTINUE
C
C GET NECESSARY INFORMATION FOR STOCHASTIC ACCELERATIONS
C ------------------------------------------------------
          IF(MODSTC.GT.0) THEN
C
C GET PARTIAL DERIVATIVES W.R.T. CONSTANT ACCELERATIONS AT TIME TSTC(1)
C ---------------------------------------------------------------------
            ISTART=6
            IF(MODSTC.EQ.1) THEN
              IF(FRCTYP.LE.13) THEN
                IPAR=FRCTYP-1
              ELSE
                IPAR=FRCTYP-13
              END IF
              IF(NUMSVN.LT.900) THEN
                CALL PRTDER(CHDUMM,NUMSVN,ISTART+IPAR,1,1,TSTC(1),1,
     1                      ICRARC,IORSYS,NVAR,NRAD,HELP,ELESAT,
     2                      RPRPAR,ANLTYP,IRC)
              ELSE
                CALL PRTDE2(CHDUMM,NUMSVN,ISTART+IPAR,1,1,TSTC(1),1,
     1                      ICRARC,IORSYS,NVAR,NRAD,HELP,ELESAT,
     2                      RPRPAR,ANLTYP,IRC)
              END IF
              DO 15 K=1,6
                HELP(K)=HELP(K)/SCAL(2)
15            CONTINUE
C
C GET THE SAME DERIVATIVES FOR MODSTC=2
C -------------------------------------
            ELSEIF(MODSTC.EQ.2) THEN
              IF(FRCTYP.LE.23) THEN
                IPAR=FRCTYP-11
              ELSE
                IPAR=FRCTYP-23
              END IF
              IF(NUMSVN.LT.900) THEN
                CALL PRTDER(CHDUMM,NUMSVN,ISTART+IPAR,1,1,TSTC(1),1,
     1                      ICRARC,IORSYS,NVAR,NRAD,HELPO,ELESAT,
     2                      RPRPAR,ANLTYP,IRC)
              ELSE
                CALL PRTDE2(CHDUMM,NUMSVN,ISTART+IPAR,1,1,TSTC(1),1,
     1                      ICRARC,IORSYS,NVAR,NRAD,HELPO,ELESAT,
     2                      RPRPAR,ANLTYP,IRC)
              END IF
C
C GET PARTIAL DERIVATIVES W.R.T. LINEAR ACCELERATIONS AT TIME TSTC(1)
C -------------------------------------------------------------------
              IF(FRCTYP.LE.23) THEN
                IPAR=FRCTYP-8
              ELSE
                IPAR=FRCTYP-11
              END IF
              IF(NUMSVN.LT.900) THEN
                CALL PRTDER(CHDUMM,NUMSVN,ISTART+IPAR,1,1,TSTC(1),1,
     1                      ICRARC,IORSYS,NVAR,NRAD,HELPD,ELESAT,
     2                      RPRPAR,ANLTYP,IRC)
              ELSE
                CALL PRTDE2(CHDUMM,NUMSVN,ISTART+IPAR,1,1,TSTC(1),1,
     1                      ICRARC,IORSYS,NVAR,NRAD,HELPD,ELESAT,
     2                      RPRPAR,ANLTYP,IRC)
              END IF
C
C PREPARE CONDITION EQUATIONS
C ---------------------------
              DO 17 K=1,6
                HELPO(K)=HELPO(K)/SCAL(2)
                HELPD(K)=HELPD(K)/SCAL(2)
                IF(DT21.NE.0.D0) THEN
                  HELP(K)=-(TSTC(1)-TOSC)/DT21*HELPO(K)+
     1                        1/DT21*HELPD(K)
                ELSE
                  HELP(K)=0.D0
                END IF
17            CONTINUE
            END IF
          END IF
C
C INVERT MATRIX Z0 CONTAINING VALUES OF PARTIALS W.R.T. INITIAL ORBIT
C ELEMENTS AT TIME TSTC
C -------------------------------------------------------------------
          CALL DMINV(Z0,6,DET,L1,M1)
C
C COMPUTE RELEVANT UNIT VECTORS (UVs):
C ER: UV IN RADIAL DIRECTION
C EW: UV NORMAL TO ORBITAL PLANE
C ES: UV IN S-DIRECTION (ES=EW*ER)
C ESUN: UV IN DIRECTION TO THE SUN
C EY: UV IN SPACE VEHICLE'S Y-DIRECTION
C EX: UV IN SPACE VEHICLE'S X-DIRECTION
C -------------------------------------
          TDT=TSTC(1)+(19.D0+32.184D0)/86400.D0
          CALL SUNEFF(IORSYS,2.D0,TDT,XSUN,DUM3)
          RSAT=DSQRT(XVSAT(1)**2+XVSAT(2)**2+XVSAT(3)**2)
          RSUN=DSQRT((XVSAT(1)-XSUN(1))**2+(XVSAT(2)-XSUN(2))**2+
     1               (XVSAT(3)-XSUN(3))**2)
          DO 20 K=1,3
            ER(K)=XVSAT(K)/RSAT
            ESUN(K)=(XSUN(K)-XVSAT(K))/RSUN
20        CONTINUE
          CALL VPROD(ER,ESUN,EY)
          DEY=DSQRT(EY(1)**2+EY(2)**2+EY(3)**2)
          CALL VPROD(XVSAT,XVSAT(4),EW)
          DEW=DSQRT(EW(1)**2+EW(2)**2+EW(3)**2)
          DO 30 K=1,3
            EY(K)=EY(K)/DEY
            EW(K)=EW(K)/DEW
30        CONTINUE
          CALL VPROD(EW,ER,ES)
          CALL VPROD(ESUN,EY,EX)
C
C COMPUTE COEFFICIENT MATRIX FOR TSTC(1) <= TOBS ( < TSTC2 )
C ----------------------------------------------------------
          IF(MODSTC.EQ.0) THEN
            DO 40 K=1,3
              HELP(K)=0.D0
40          CONTINUE
            IF(FRCTYP.EQ.1)THEN
              DO 50 K=1,3
                HELP(K+3)=ER(K)
50            CONTINUE
            ELSE IF(FRCTYP.EQ.2)THEN
              DO 60 K=1,3
                HELP(K+3)=ES(K)
60            CONTINUE
            ELSE IF(FRCTYP.EQ.3)THEN
              DO 70 K=1,3
                HELP(K+3)=EW(K)
70            CONTINUE
            ELSE IF(FRCTYP.EQ.4)THEN
              DO 80 K=1,3
                HELP(K+3)=ESUN(K)
80            CONTINUE
            ELSE IF(FRCTYP.EQ.5)THEN
              DO 90 K=1,3
                HELP(K+3)=EY(K)
90            CONTINUE
            ELSE IF(FRCTYP.EQ.6)THEN
              DO 100 K=1,3
                HELP(K+3)=EX(K)
100           CONTINUE
            END IF
          END IF
C
          DO 110 I=1,6
            COEMAT(I,IPE,INDSTC,ISAT)=0.D0
            DO 105 K=1,6
              COEMAT(I,IPE,INDSTC,ISAT)=COEMAT(I,IPE,INDSTC,ISAT)+
     1                                  Z0(I,K)*HELP(K)
105         CONTINUE
110       CONTINUE
        END IF
C
C COMPUTE PARTIAL DER. OF ORBIT VECTOR FOR TIME TSTC(1) <= TOBS ( < TSTC2 )
C -------------------------------------------------------------------------
        IF((TSTC(1)-DTTOL).LE.TOBS.AND.TOBS.LT.(TSTC(2)-DTTOL))THEN
calt        IF(TSTC(1).LE.TOBS.AND.TOBS.LT.TSTC(2))THEN
calt2        IF(TSTC(1).LT.TOBS.AND.TOBS.LT.TSTC(2))THEN
          IF(MODSTC.EQ.1) THEN
            IPAR=FRCTYP-10
          ELSEIF(MODSTC.EQ.2) THEN
            IPAR=FRCTYP-20
            IF(IPAR.LE.3) THEN
              KPAR=IPAR+6
            ELSE
              KPAR=IPAR+3
            END IF
          END IF
          DO 130 K=1,3
            DRDSTC(K)=0.D0
            DO 120 L=1,6
              IF(MODSTC.EQ.0) THEN
                DRDSTC(K)=DRDSTC(K)+DRDELE(K,L)*
     1                    COEMAT(L,IPE,INDSTC,ISAT)/SCAL(1)
              ELSEIF(MODSTC.GE.1) THEN
                DRDSTC(K)=DRDSTC(K)+DRDELE(K,L)*
     1                    COEMAT(L,IPE,INDSTC,ISAT)
              END IF
120         CONTINUE
C
C FORM LINEAR COMBINATION FOR STOCHASTIC ACCELERATIONS
C ----------------------------------------------------
            IF(MODSTC.EQ.1) THEN
              IF(NVAR.LT.18.AND.FRCTYP.LE.13) THEN
                NREQ=18
                WRITE(LFNERR,125) NREQ,NVAR
125             FORMAT(/,' *** SR PDSTC2: NOT ALL VARIATIONAL',
     1                   ' EQUATIONS AVAILABLE',
     2                 /,16X,'REQUESTED NUMBER:',I5,
     3                 /,16X,'AVAILABLE NUMBER:',I5,/)
                CALL EXITRC(2)
              ELSE
                DRDSTC(K)=DRDACC(K,IPAR)-DRDSTC(K)
              END IF
            ELSEIF(MODSTC.EQ.2) THEN
              IF(NVAR.LT.21.AND.FRCTYP.LE.23) THEN
                NREQ=21
                WRITE(LFNERR,127) NREQ,NVAR
127             FORMAT(/,' *** SR PDSTC2: NOT ALL VARIATIONAL',
     1                   ' EQUATIONS AVAILABLE',
     2                 /,16X,'REQUESTED NUMBER:',I5,
     3                 /,16X,'AVAILABLE NUMBER:',I5,/)
                CALL EXITRC(2)
              ELSE
                DRDSTC(K)=-(TSTC(1)-TOSC)/DT21*DRDACC(K,IPAR)+
     1                      1/DT21*DRDACC(K,KPAR)-
     2                     DRDSTC(K)
              END IF
            END IF
130       CONTINUE
C
C  TSTC(2) <= TOBS < TSTC(3) (ONLY FOR ACCELERATIONS)
C ---------------------------------------------------
        ELSEIF((TSTC(2)-DTTOL).LE.TOBS.AND.TOBS.LT.(TSTC(3)-DTTOL))THEN
calt       ELSEIF(TOBS.GE.TSTC(2))THEN
C
C NEW ARC ?
          IF(IARC.NE.IAOLD)THEN
            IAOLD=IARC
            DO 141 IP=1,3
              DO 141 IST=1,MAXSTC
                DO 141 ISA=1,MAXSAT
                  STCDEF2(IP,IST,ISA)=0
                  STCDEF3(IP,IST,ISA)=0
141         CONTINUE
          END IF
C
C IS ARRAY COEMAT2 ALREADY DEFINED ?
          IF(STCDEF2(IPE,INDSTC,ISAT).EQ.0)THEN
            STCDEF2(IPE,INDSTC,ISAT)=1
C
C GET PARTIAL DERIVATIVES W.R.T. OSC. ELEMENTS AT TIME TSTC(2)
C ------------------------------------------------------------
            DO 150 IPAR=1,6
              CHDUMM = ' '
              IF(NUMSVN.LT.900) THEN
               CALL PRTDER(CHDUMM,NUMSVN,IPAR,1,1,TSTC(2),1,ICRARC,
     1                    IORSYS,NVAR,NRAD,Z0(1,IPAR),ELESAT,RPRPAR,
     2                    ANLTYP,IRC)
              ELSE
               CALL PRTDE2(CHDUMM,NUMSVN,IPAR,1,1,TSTC(2),1,ICRARC,
     1                    IORSYS,NVAR,NRAD,Z0(1,IPAR),ELESAT,RPRPAR,
     2                    ANLTYP,IRC)
              END IF
150         CONTINUE
C
C GET PARTIAL DERIVATIVES W.R.T. CONSTANT ACCELERATIONS AT TIME TSTC(2)
C ---------------------------------------------------------------------
            ISTART=6
            IF(MODSTC.EQ.1) THEN
              IF(FRCTYP.LE.13) THEN
                IPAR=FRCTYP-1
              ELSE
                IPAR=FRCTYP-13
              END IF
              IF(NUMSVN.LT.900) THEN
                CALL PRTDER(CHDUMM,NUMSVN,ISTART+IPAR,1,1,TSTC(2),1,
     1                      ICRARC,IORSYS,NVAR,NRAD,HLPACC,ELESAT,
     2                      RPRPAR,ANLTYP,IRC)
              ELSE
                CALL PRTDE2(CHDUMM,NUMSVN,ISTART+IPAR,1,1,TSTC(2),1,
     1                      ICRARC,IORSYS,NVAR,NRAD,HLPACC,ELESAT,
     2                      RPRPAR,ANLTYP,IRC)
              END IF
              DO 160 K=1,6
                HLPACC(K)=HLPACC(K)/SCAL(2)
160           CONTINUE
C
C GET THE SAME DERIVATIVES FOR MODSTC=2
C -------------------------------------
            ELSEIF(MODSTC.EQ.2) THEN
              IF(FRCTYP.LE.23) THEN
                IPAR=FRCTYP-11
              ELSE
                IPAR=FRCTYP-23
              END IF
              IF(NUMSVN.LT.900) THEN
                CALL PRTDER(CHDUMM,NUMSVN,ISTART+IPAR,1,1,TSTC(2),1,
     1                      ICRARC,IORSYS,NVAR,NRAD,HLPACCO,ELESAT,
     2                      RPRPAR,ANLTYP,IRC)
              ELSE
                CALL PRTDE2(CHDUMM,NUMSVN,ISTART+IPAR,1,1,TSTC(2),1,
     1                      ICRARC,IORSYS,NVAR,NRAD,HLPACCO,ELESAT,
     2                      RPRPAR,ANLTYP,IRC)
              END IF
C
C GET PARTIAL DERIVATIVES W.R.T. LINEAR ACCELERATIONS AT TIME TSTC(2)
C -------------------------------------------------------------------
              IF(FRCTYP.LE.23) THEN
                IPAR=FRCTYP-8
              ELSE
                IPAR=FRCTYP-11
              END IF
              IF(NUMSVN.LT.900) THEN
                CALL PRTDER(CHDUMM,NUMSVN,ISTART+IPAR,1,1,TSTC(2),1,
     1                      ICRARC,IORSYS,NVAR,NRAD,HLPACCD,ELESAT,
     2                      RPRPAR,ANLTYP,IRC)
              ELSE
                CALL PRTDE2(CHDUMM,NUMSVN,ISTART+IPAR,1,1,TSTC(2),1,
     1                      ICRARC,IORSYS,NVAR,NRAD,HLPACCD,ELESAT,
     2                      RPRPAR,ANLTYP,IRC)
              END IF
C
              DO 165 K=1,6
                HLPACCD(K)=HLPACCD(K)/SCAL(2)
                HLPACCO(K)=HLPACCO(K)/SCAL(2)
165           CONTINUE
C
            END IF

C
C GET PARTIAL OF STOCH. ACCELERATION (AND 1.ST DERIV.) AT TIME TSTC(2)
C --------------------------------------------------------------------
            DO 180 K=1,6
              HELP(K)=0.D0
              DO 170 L=1,6
                HELP(K)=HELP(K)+Z0(K,L)*
     1                    COEMAT(L,IPE,INDSTC,ISAT)
170           CONTINUE
              IF(MODSTC.EQ.1) THEN
                HELP(K)=HLPACC(K)-HELP(K)
              ELSEIF(MODSTC.EQ.2) THEN
                HELP(K)=TFAC*(TSTC(2)-TOSC)*HLPACCO(K)-
     1                  TFAC*HLPACCD(K)+
     2                  HELP(K)
              END IF
180         CONTINUE
C
            CALL DMINV(Z0,6,DET,L1,M1)
C
C COMPUTE COEFFICIENT MATRIX FOR TSTC(2) <= TOBS < TSTC(3)
C --------------------------------------------------------
            DO 200 I=1,6
              COEMAT2(I,IPE,INDSTC,ISAT)=0.D0
              DO 190 K=1,6
                COEMAT2(I,IPE,INDSTC,ISAT)=COEMAT2(I,IPE,INDSTC,ISAT)+
     1                                    Z0(I,K)*HELP(K)
190           CONTINUE
200         CONTINUE
C
          END IF
C
C COMPUTE PARTIAL DER. OF ORBIT VECTOR FOR TIME TSTC(2) <= TOBS < TSTC(3)
C -----------------------------------------------------------------------
          IF(MODSTC.EQ.2) THEN
            IPAR=FRCTYP-20
            IF(IPAR.LE.3) THEN
              KPAR=IPAR+6
            ELSE
              KPAR=IPAR+3
            END IF
          END IF
          DO 220 K=1,3
            DRDSTC(K)=0.D0
            DO 210 L=1,6
              DRDSTC(K)=DRDSTC(K)+DRDELE(K,L)*
     1                  COEMAT2(L,IPE,INDSTC,ISAT)
210         CONTINUE
C
C FORM LINEAR COMBINATION FOR PIECEWISE LINEAR ACCELERATIONS
C ----------------------------------------------------------
            IF(MODSTC.EQ.2) THEN
              IF(NVAR.LT.21.AND.FRCTYP.LE.23) THEN
                NREQ=21
                WRITE(LFNERR,225) NREQ,NVAR
225             FORMAT(/,' *** SR PDSTC2: NOT ALL VARIATIONAL',
     1                   ' EQUATIONS AVAILABLE',
     2                 /,16X,'REQUESTED NUMBER:',I5,
     3                 /,16X,'AVAILABLE NUMBER:',I5,/)
                CALL EXITRC(2)
              ELSE
                DRDSTC(K)= (TSTC(3)-TOSC)/DT32*DRDACC(K,IPAR)-
     1                      1/DT32*DRDACC(K,KPAR)-
     2                     DRDSTC(K)
              END IF
            END IF
220       CONTINUE
C
C TOBS >= TSTC(3) (ONLY FOR PIECEWISE LINEAR ACCELERATIONS)
C ---------------------------------------------------------
        ELSEIF(TOBS.GE.(TSTC(3)-DTTOL))THEN
C
C NEW ARC ?
          IF(IARC.NE.IAOLD)THEN
            IAOLD=IARC
            DO 241 IP=1,3
              DO 241 IST=1,MAXSTC
                DO 241 ISA=1,MAXSAT
                  STCDEF3(IP,IST,ISA)=0
241         CONTINUE
          END IF
C
C IS ARRAY COEMAT3 ALREADY DEFINED ?
          IF(STCDEF3(IPE,INDSTC,ISAT).EQ.0)THEN
            STCDEF3(IPE,INDSTC,ISAT)=1
C
C GET PARTIAL DERIVATIVES W.R.T. OSC. ELEMENTS AT TIME TSTC(3)
C ------------------------------------------------------------
            DO 250 IPAR=1,6
              CHDUMM = ' '
              IF(NUMSVN.LT.900) THEN
               CALL PRTDER(CHDUMM,NUMSVN,IPAR,1,1,TSTC(3),1,ICRARC,
     1                    IORSYS,NVAR,NRAD,Z0(1,IPAR),ELESAT,RPRPAR,
     2                    ANLTYP,IRC)
              ELSE
               CALL PRTDE2(CHDUMM,NUMSVN,IPAR,1,1,TSTC(3),1,ICRARC,
     1                    IORSYS,NVAR,NRAD,Z0(1,IPAR),ELESAT,RPRPAR,
     2                    ANLTYP,IRC)
              END IF
250         CONTINUE
C
C GET PARTIAL DERIVATIVES W.R.T. CONSTANT ACCELERATIONS AT TIME TSTC(3)
C ---------------------------------------------------------------------
            ISTART=6
            IF(FRCTYP.LE.23) THEN
              IPAR=FRCTYP-11
            ELSE
              IPAR=FRCTYP-23
            END IF
            IF(NUMSVN.LT.900) THEN
              CALL PRTDER(CHDUMM,NUMSVN,ISTART+IPAR,1,1,TSTC(3),1,
     1                    ICRARC,IORSYS,NVAR,NRAD,HLPACCO,ELESAT,RPRPAR,
     2                    ANLTYP,IRC)
            ELSE
              CALL PRTDE2(CHDUMM,NUMSVN,ISTART+IPAR,1,1,TSTC(3),1,
     1                    ICRARC,IORSYS,NVAR,NRAD,HLPACCO,ELESAT,RPRPAR,
     2                    ANLTYP,IRC)
            END IF
C
C GET PARTIAL DERIVATIVES W.R.T. LINEAR ACCELERATIONS AT TIME TSTC(3)
C -------------------------------------------------------------------
            IF(FRCTYP.LE.23) THEN
              IPAR=FRCTYP-8
            ELSE
              IPAR=FRCTYP-11
            END IF
            IF(NUMSVN.LT.900) THEN
              CALL PRTDER(CHDUMM,NUMSVN,ISTART+IPAR,1,1,TSTC(3),1,
     1                    ICRARC,IORSYS,NVAR,NRAD,HLPACCD,ELESAT,RPRPAR,
     2                    ANLTYP,IRC)
            ELSE
              CALL PRTDE2(CHDUMM,NUMSVN,ISTART+IPAR,1,1,TSTC(3),1,
     1                    ICRARC,IORSYS,NVAR,NRAD,HLPACCD,ELESAT,RPRPAR,
     2                    ANLTYP,IRC)
            END IF
C
            DO 260 K=1,6
              HLPACCD(K)=HLPACCD(K)/SCAL(2)
              HLPACCO(K)=HLPACCO(K)/SCAL(2)
260         CONTINUE
C
C GET PARTIAL OF STOCH. ACCELERATION (AND 1.ST DERIV.) AT TIME TSTC(2)
C --------------------------------------------------------------------
            DO 280 K=1,6
              HELP(K)=0.D0
              DO 270 L=1,6
                HELP(K)=HELP(K)+Z0(K,L)*
     1                    COEMAT2(L,IPE,INDSTC,ISAT)
270           CONTINUE
              HELP(K)=(TSTC(3)-TOSC)/DT32*HLPACCO(K)-
     1                                 1/DT32*HLPACCD(K)-
     2                HELP(K)
280         CONTINUE
C
            CALL DMINV(Z0,6,DET,L1,M1)
C
C COMPUTE COEFFICIENT MATRIX FOR TOBS >= TSTC(3)
C ----------------------------------------------
            DO 300 I=1,6
              COEMAT3(I,IPE,INDSTC,ISAT)=0.D0
              DO 290 K=1,6
                COEMAT3(I,IPE,INDSTC,ISAT)=COEMAT3(I,IPE,INDSTC,ISAT)+
     1                                    Z0(I,K)*HELP(K)
290           CONTINUE
300         CONTINUE
          END IF
C
C COMPUTE PARTIAL DER. OF ORBIT VECTOR FOR TIME TOBS >= TSTC(3)
C -------------------------------------------------------------
          DO 320 K=1,3
            DRDSTC(K)=0.D0
            DO 310 L=1,6
              DRDSTC(K)=DRDSTC(K)+DRDELE(K,L)*
     1                  COEMAT3(L,IPE,INDSTC,ISAT)
310         CONTINUE
320       CONTINUE
C
        END IF
C
C COMPUTE PARTIAL DER. OF OBSERVATION
C -----------------------------------
        CALL COOTRA(IORSYS,0,TOBS,DRDSTC,SZ,XPOL,YPOL,UT1GPS)
C
        DER=0.D0
        IF(SVNSYS(9,1,(/NUMSVN/))) THEN
          DO K=1,3
            IF (NDIFF.EQ.1) THEN
              IF (ONEOR2.EQ.0) THEN
                DER=DER+TUV(K,1)*DRDSTC(K)
              ELSE IF (ONEOR2.EQ.1) THEN
                DER=DER-TUV(K,1)*DRDSTC(K)
              ELSE
                DER=DER+TUV(K,2)*DRDSTC(K)
              END IF
            ELSE
              IF (ONEOR2.EQ.0) THEN
                DER=DER+TUV(K,1)*DRDSTC(K)
              ELSE
                DER=DER-TUV(K,1)*DRDSTC(K)
              ENDIF
            ENDIF
          END DO
        ELSE
          DO K=1,3
            IF (NDIFF.EQ.1) THEN
              DER=DER+(TUV(K,1)-TUV(K,2))*DRDSTC(K)
            ELSE
              DER=DER+TUV(K,1)*DRDSTC(K)
            END IF
          END DO
        END IF
C
      END IF
C
      RETURN
      END SUBROUTINE

      END MODULE
