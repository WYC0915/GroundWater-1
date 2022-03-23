      MODULE s_EEAMAT
      CONTAINS

C*
      SUBROUTINE EEAMAT(ITYP  ,OBSSYS,OBSTYP,ERPEPO,ERPOBS,ERPSIG,
     1                  SIGMA0,WGTTYP,IUSSIG,NWIN  ,WINTIM,WINSIG,
     2                  XSTELL,NPAR  ,PARNAM,PARTIM,AMAT  ,
     3                  OMC   ,WEIGHT,REFEPO)
CC
CC NAME       :  EEAMAT
CC
CC PURPOSE    :  SET UP FIRST DESIGN MATRIX AND O-C
CC               PARAMETER DEFINITIONS ALLOWED:
CC                 xx OFFSET y          : OFFSET IN OBS.TYPE xx and
CC                                          OBS.SYSTEM y
CC                 xx DRIFT  y          : DRIFT  IN OBS.TYPE xx and
CC                                          OBS.SYSTEM y
CC                 xx DEG n  y          : POLY.DEG. n IN OBS.TYPE xx and
CC                                          OBS.SYSTEM y
CC                 xx LINEAR ffffff.fff : PIECE-WISE LINEAR FIT WITH
CC                                          EPOCH SPACING ffffff.fff HOURS
CC                 xx SIN fffffff.fffff : SIN TERM IN OBS.TYPE xx WITH
CC                                          PERIOD fffffff.fffff
CC                 xx COS fffffff.fffff : COS TERM IN OBS.TYPE xx WITH
CC                                          PERIOD fffffff.fffff
CC                 XY SIN fffffff.fffff : SIN TERM IN X AND Y (PRO-
CC                                        GRADE, IF PERIOD POSITIVE)
CC                 XY COS fffffff.fffff : COS TERM IN X AND Y (PRO-
CC                                        GRADE, IF PERIOD POSITIVE)
CC                 xx Ffff.ffggg.ghhh.h : FOURIER ANALYSIS OF OBS.TYPE
CC                                        xx (xx CAN BE "XY", TOO) WITH
CC                                        BASIC PERIOD fff.ff ESTIMATING
CC                                        PERIODS FROM ggg.g TO hhh.h
CC                 EC  n1 n2 n3 n4 n5   : EPSILON COS TERM WITH
CC                                        DELAUNAY NUMBERS (n1,..,n5)
CC                 ES  n1 n2 n3 n4 n5   : ESPILON SIN TERM WITH
CC                                        DELAUNAY NUMBERS (n1,..,n5)
CC                 PC  n1 n2 n3 n4 n5   : PSI COS TERM WITH
CC                                        DELAUNAY NUMBERS (n1,..,n5)
CC                 PS  n1 n2 n3 n4 n5   : PSI SIN TERM WITH
CC                                        DELAUNAY NUMBERS (n1,..,n5)
CC                 SC  n1 n2 n3 n4 n5 n6: SUB-DAILY COS TERM WITH
CC                                        DELAUNAY NUMBERS (n1,..,n5)
CC                                        n6=-2,-1,1,2 FOR RETROGRADE
CC                                        AND PROGRADE, SEMI-DIURNAL AND
CC                                        DIURNAL TIDES
CC                 SS  n1 n2 n3 n4 n5 n6: SUB-DAILY SIN TERM WITH
CC                                        DELAUNAY NUMBERS (n1,..,n5)
CC                                        n6=-2,-1,1,2 FOR RETROGRADE
CC                                        AND PROGRADE, SEMI-DIURNAL AND
CC                                        DIURNAL TIDES
CC                 UC  n1 n2 n3 n4 n5 n6: SUB-DAILY COS TERM WITH
CC                                        DELAUNAY NUMBERS (n1,..,n5)
CC                                        n6=-2,-1 FOR SEMI-DIURNAL AND
CC                                        DIURNAL TERMS
CC                 US  n1 n2 n3 n4 n5 n6: SUB-DAILY SIN TERM WITH
CC                                        DELAUNAY NUMBERS (n1,..,n5)
CC                                        n6=-2,-1 FOR SEMI-DIURNAL AND
CC                                        DIURNAL TERMS
CC
CC PARAMETERS :
CC        IN  :  ITYP   : OBSERVATION TYPE                       I*4
CC                        = 1: X-POLE (MAS)
CC                        = 2: Y-POLE (MAS)
CC                        = 3: UT1-UTC (MS)
CC                        = 4: DEPS (MAS)
CC                        = 5: DPSI (MAS)
CC                        = 6: X-POLE RATE (MAS/D)
CC                        = 7: Y-POLE RATE (MAS/D)
CC                        = 8: UT1-UTC RATE (MS/D)
CC                        = 9: DEPS RATE (MAS/D)
CC                        =10: DPSI RATE (MAS/D)
CC                        =11: RELATIVE SAGNAC FREQUENCY (PPM)
CC               OBSSYS : OBSERVATION TECHNIQUE                 CH*1
CC                        = ' ': UNDEFINED
CC                        = 'G': GPS
CC                        = 'V': VLBI
CC                        = 'R': G1-RINGLASER
CC                        = 'U': UG-RINGLASER
CC               OBSTYP : OBSERVATION TYPE                      CH*2
CC                        = 'X_': X-POLE (MAS)
CC                        = 'Y_': Y-POLE (MAS)
CC                        = 'U_': UT1-UTC (MS)
CC                        = 'E_': DEPS (MAS)
CC                        = 'P_': DPSI (MAS)
CC                        = 'DX': X-POLE RATE (MAS/D)
CC                        = 'DY': Y-POLE RATE (MAS/D)
CC                        = 'DU': UT1-UTC RATE (MS/D)
CC                        = 'DE': DEPS RATE (MAS/D)
CC                        = 'DP': DPSI RATE (MAS/D)
CC                        = 'SF': RELATIVE SAGNAC FREQUENCY (PPM)
CC               ERPEPO : ERP OBSERVATION EPOCH                  R*8
CC               ERPOBS : ERP OBSERVATION VALUE                  R*8
CC               ERPSIG : ERP OBSERVATION SIGMA                  R*8
CC               SIGMA0 : A PRIORI SIGMA OF OBS. OF UNIT WEIGHT  R*8
CC                        (MAS)
CC               WGTTYP : WEIGHT OF OBSERVATION                  R*8
CC               IUSSIG : USE ERP SIGMAS IN ESTIMATION           I*4
CC                        =0: NO
CC                        =1: YES
CC               NWIN   : NUMBER OF DATA WINDOWS                 I*4
CC               WINTIM(2,I),I=1,..,NWIN: DATA WINDOWS           R*8
CC                          FROM,TO IN MJD
CC               WINSIG(I),I=1,..,NWIN: SIGMA FACTOR FOR EACH    R*8
CC                          WINDOW
CC               XSTELL(I), I=1,2,3: ELLIPSOIDAL STATION COORD.  R*8
CC                        I=1: LATITUDE (RAD)
CC                        I=2: LONGITUDE (RAD)
CC                        I=3: HEIGHT (M)
CC               NPAR   : MAXIMUM NUMBER OF PARAMETERS ALLOWED   I*4
CC               PARNAM(I),I=1,..,NPAR: PARAMETER NAMES         CH*20
CC               PARTIM(2,I),I=1,..,NPAR: PARAMETER TIME         R*8
CC                        INTERVAL
CC        OUT :  AMAT(I),I=1,..,NPAR: PARTIAL DERIVATIVES W.R.T. R*8
CC                        PARAMETERS
CC               OMC    : OBSERVED-COMPUTED                      R*8
CC               WEIGHT : OBSERVATION WEIGHT                     R*8
CC               REFEPO(I),I=1,2: REFERENCE EPOCHS               R*8
CC                        (1): REFERENCE EPOCH FOR DRIFT
CC                             PARAMETERS, FIRST EPOCH PROCESSED
CC                        (2): LAST EPOCH PROCESSED
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  07-NOV-97
CC
CC CHANGES    :  01-DEC-98 : MR: ADD A PRIORI VALUE AND REFERENCE FOR
CC                               ITYP=4,5
CC               15-JAN-99 : JJ: RM UNUSED VAR NUTMAT
CC               30-MAY-03 : PS: CHANGED KEYWORD SCRATCH => AUXFIL
CC               02-JUN-03 : PS: CALL TO F90-SR RDNUTM
CC               11-JUN-03 : PS: USE SR NUTCOR
CC               02-JUL-03 : PS: ADDED INTERFACE FOR SR NUTCOR
CC               12-AUG-03 : MR: ADD RELATIVE SAGNAC FREQUENCY
CC               16-FEB-04 : PS: DIVIDE RATES BY DTDRFT
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC               21-OCT-08 : HB: ADD USE s_submod AND USE s_nutval
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*

! Modules
! -------
      USE m_bern
      USE d_nutmod, ONLY: t_nutat
      USE d_const, ONLY : PI,OMEGA,const_def,ars

      USE s_rdnutm
      USE s_submod
      USE s_opnfil
      USE s_poldef
      USE s_opnerr
      USE s_subval
      USE s_rdsubm
      USE s_exitrc
      USE s_nutval
      USE s_gtflna
      USE s_nutcor
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IARG   , IDEG   , IFIRST , II     , IOSTAT , IPAR   ,
     1          IRC    , IRCNUT , IRCPOL , IRCRES , IRCSUB , ISUBFL ,
     2          ITYP   , IUSSIG , IWIN   , NPAR   , NSUB   , NWIN
C
      REAL*8    AEPS   , APSI   , ARG    , ARGR   , DODX   , DODY   ,
     1          DOMPOL , DOMUT1 , DTDRFT , DTMOON , DTPART , DTREF  ,
     2          DUT1   , DUT11  , DUT12  , DUTAPR , DUTGPS , DUTGPS1,
     3          DUTGPS2, ERPAPR , ERPEPO , ERPOBS , ERPREF , ERPSIG ,
     4          OMC    , OMCCOR , OMEG   , PARDIF , PERIOD , PERUSR ,
     5          REPS   , RHO    , RPSI   , SIGMA0 , WEIGHT , WGTTYP ,
     6          XPOL   , XPOL1  , XPOL2  , YPOL   , YPOL1  , YPOL2
C
CCC       IMPLICIT REAL*8(A-H,O-Z)


! Local types
! -----------
      TYPE(t_nutat)             :: nutapr,nutref           ! Nutation model
                                                           ! parameters

! Local Parameters
! ----------------
      INTEGER(i4b)                   :: maxPer,maxTyp
      PARAMETER (MAXPER=1000,MAXTYP=11)

      CHARACTER*80 TITLES
      CHARACTER*32 FILNUA,FILNUR,FILRES,FILSCR,FILPOL,FILSUB
      CHARACTER*20 PARNAM(*)
      CHARACTER*16 SUBNAM
      CHARACTER*2  OBSTYP
      CHARACTER*1  OBSSYS
C
      REAL*8       PARTIM(2,*),AMAT(*)
      REAL*8       REFEPO(2)
      REAL*8       SUBFAR(6,6),SUBPER(MAXPER),SUBCOE(4,MAXPER)
      REAL*8       ERPSUB(3),ERPSUR(3)
      REAL*8       WINTIM(2,*),WINSIG(*)
      REAL*8       XSTELL(3),AS2RAD
      REAL(r8b)                      :: tu
      REAL(r8b)                      :: aPsiR,aEpsR
      REAL(r8b)                      :: rPsiR,rEpsR
C
      INTEGER*4    IFDARG(14)
      INTEGER*4    SUBMLT(6,MAXPER)
C
      DATA IFIRST /1/
      DATA DTDRFT /0.005D0/


! Pi defined?
! -----------
      IF (const_def /= 1) THEN
        WRITE(lfnerr,'(A)') 'SR EEAMAT: Dear programmer, ' //
     1    'constants are not defined - call SR DEFCON!'
        CALL exitrc(2)
      ENDIF


C
C GET NUTATION MODEL COEFFICIENTS
C -------------------------------
      IF (IFIRST.EQ.1) THEN
        IFIRST=0
        AS2RAD=1.D0/ars
C
C INITIALIZATION OF IFDARG
C ------------------------
        DO IARG=1,14
          IFDARG(IARG)=0
        ENDDO
C
C GET A PRIORI NUTATION MODEL
C ---------------------------
        CALL GTFLNA(0,'NUTAPR ',FILNUA,IRC)
        IF (IRC.EQ.0) THEN

! Read nutation model file
! ------------------------
          CALL rdnutm(filnua,nutapr)
        ELSE
          nutapr%nnut=0
        ENDIF
C
C GET REFERENCE NUTATION MODEL
C ----------------------------
        CALL GTFLNA(0,'NUTREF ',FILNUR,IRCNUT)
        IF (IRCNUT.EQ.0) THEN

! NEW SR RDNUTM
          CALL rdnutm(filnur,nutref)
        ELSE
          nutref%nnut=0
        ENDIF
C
C GET A PRIORI POLE FILE
C ----------------------
        CALL GTFLNA(0,'POLE   ',FILPOL,IRCPOL)
C
C GET REFERENCE SUBDAILY ERP MODEL
C --------------------------------
        CALL GTFLNA(0,'SUBREF ',FILSUB,IRCSUB)
        IF (IRCSUB.EQ.0) THEN
          CALL RDSUBM(MAXPER,FILSUB,TITLES,SUBNAM,SUBFAR,NSUB  ,
     1                  SUBPER,SUBMLT,SUBCOE)
        ELSE
          NSUB=0
        ENDIF
C
C SET REFERENCE EPOCH
C -------------------
        REFEPO(1)=ERPEPO
        REFEPO(2)=ERPEPO
C
C CHECK WHETHER RESIDUALS SHOULD BE COMPUTED (RESIDUAL FILE SPECIFIED)
C --------------------------------------------------------------------
        CALL GTFLNA(0,'RESID  ',FILRES,IRCRES)
        IF (IRCRES.EQ.0) THEN
C
C OPEN A-MATRIX FILE (SCRATCH FILE)
          CALL GTFLNA(1,'AUXFIL',FILSCR,IRC)
          CALL OPNFIL(LFNPLT,FILSCR,'UNKNOWN','UNFORMATTED',
     1                ' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNPLT,IOSTAT,FILSCR,'EEAMAT')
        ENDIF
C
C END INITIALIZATION
      ENDIF
C
C UPDATE LAST EPOCH PROCESSED
C ---------------------------
      IF (REFEPO(2).LT.ERPEPO) REFEPO(2)=ERPEPO
C
C INITIALIZE PARTIALS
C -------------------
      DO IPAR=1,NPAR
        AMAT(IPAR)=0.D0
      ENDDO
C
C COMPUTE PARTIALS IN MAS (LOOP OVER ALL PARAMETERS)
C --------------------------------------------------
      DO IPAR=1,NPAR

C
C CHECK TIME INTERVAL
        IF (ERPEPO.GE.PARTIM(1,IPAR) .AND.
     1      ERPEPO.LT.PARTIM(2,IPAR)) THEN
C
C OFFSETS
          IF (PARNAM(IPAR)(3:9).EQ.' OFFSET') THEN
            IF (PARNAM(IPAR)(1:2).EQ.OBSTYP .AND.
     1          (PARNAM(IPAR)(11:11).EQ.' '.OR.
     2           PARNAM(IPAR)(11:11).EQ.OBSSYS)) THEN
              AMAT(IPAR)=1.D0
            ENDIF
C
C DRIFTS
          ELSEIF (PARNAM(IPAR)(3:8).EQ.' DRIFT') THEN
            IF (PARNAM(IPAR)(1:2).EQ.OBSTYP .AND.
     1          (PARNAM(IPAR)(11:11).EQ.' '.OR.
     2           PARNAM(IPAR)(11:11).EQ.OBSSYS)) THEN
              AMAT(IPAR)=ERPEPO-REFEPO(1)
            ENDIF
C
C HIGHER DEGREES OF POLYNOMIAL
          ELSEIF (PARNAM(IPAR)(3:6).EQ.' DEG') THEN
            IF (PARNAM(IPAR)(1:2).EQ.OBSTYP .AND.
     1          (PARNAM(IPAR)(11:11).EQ.' '.OR.
     2           PARNAM(IPAR)(11:11).EQ.OBSSYS)) THEN
              READ(PARNAM(IPAR)(8:8),'(I1)') IDEG
              AMAT(IPAR)=(ERPEPO-REFEPO(1))**IDEG
            ENDIF
C
C PIECE-WISE LINEAR FIT
          ELSEIF (PARNAM(IPAR)(3:9).EQ.' LINEAR') THEN
            IF (PARNAM(IPAR)(1:2).EQ.OBSTYP) THEN
              DTPART=DABS(ERPEPO-(PARTIM(2,IPAR)+PARTIM(1,IPAR))/2.D0)
              PARDIF=(PARTIM(2,IPAR)-PARTIM(1,IPAR))/2.D0
              AMAT(IPAR)=1.D0-DTPART/PARDIF
            ENDIF
C
C USER-SPECIFIED FREQUENCIES: SIN TERMS
C FREQUENCY COMMON TO X- AND Y-POLE (PROGRADE):
C X =  XYS*SIN(OMEG*DT)-XYC*COS(OMEG*DT) = A*COS(-OMEG*DT+PHI)
C Y =  XYC*SIN(OMEG*DT)+XYS*COS(OMEG*DT) = A*SIN(-OMEG*DT+PHI)
          ELSEIF (PARNAM(IPAR)(3:6).EQ.' SIN') THEN
            IF (PARNAM(IPAR)(1:2).EQ.OBSTYP) THEN
              READ(PARNAM(IPAR)(7:20),'(F14.5)') PERUSR
              DTREF=ERPEPO-REFEPO(1)
              OMEG=2*PI/PERUSR
              AMAT(IPAR)=DSIN(OMEG*DTREF)
            ELSEIF (PARNAM(IPAR)(1:2).EQ.'XY') THEN
              READ(PARNAM(IPAR)(7:20),'(F14.5)') PERUSR
              DTREF=ERPEPO-REFEPO(1)
              OMEG=2*PI/PERUSR
              IF (OBSTYP.EQ.'X_') THEN
                AMAT(IPAR)=DSIN(OMEG*DTREF)
              ELSEIF (OBSTYP.EQ.'Y_') THEN
                AMAT(IPAR)=DCOS(OMEG*DTREF)
              ENDIF
            ENDIF
C
C USER-SPECIFIED FREQUENCIES: COS TERMS
C FREQUENCY COMMON TO X- AND Y-POLE (PROGRADE):
C X =  XYS*SIN(OMEG*DT)-XYC*COS(OMEG*DT) = A*COS(-OMEG*DT+PHI)
C Y =  XYC*SIN(OMEG*DT)+XYS*COS(OMEG*DT) = A*SIN(-OMEG*DT+PHI)
          ELSEIF (PARNAM(IPAR)(3:6).EQ.' COS') THEN
            IF (PARNAM(IPAR)(1:2).EQ.OBSTYP) THEN
              READ(PARNAM(IPAR)(7:20),'(F14.5)') PERUSR
              DTREF=ERPEPO-REFEPO(1)
              OMEG=2*PI/PERUSR
              AMAT(IPAR)=DCOS(OMEG*DTREF)
            ELSEIF (PARNAM(IPAR)(1:2).EQ.'XY') THEN
              READ(PARNAM(IPAR)(7:20),'(F14.5)') PERUSR
              DTREF=ERPEPO-REFEPO(1)
              OMEG=2*PI/PERUSR
              IF (OBSTYP.EQ.'X_') THEN
                AMAT(IPAR)=-DCOS(OMEG*DTREF)
              ELSEIF (OBSTYP.EQ.'Y_') THEN
                AMAT(IPAR)= DSIN(OMEG*DTREF)
              ENDIF
            ENDIF
C
C SUBDAILY AMPLITUDES IN X, Y, AND UT1
          ELSEIF (PARNAM(IPAR)(1:2).EQ.'SS' .OR.
     1            PARNAM(IPAR)(1:2).EQ.'SC' .OR.
     2            PARNAM(IPAR)(1:2).EQ.'US' .OR.
     3            PARNAM(IPAR)(1:2).EQ.'UC') THEN
C
            READ(PARNAM(IPAR)(3:20),'(6I3)') (IFDARG(II),II=1,6)
C
            IF (IRCSUB.EQ.0) THEN
              CALL SUBVAL(ERPEPO,SUBFAR,IFDARG,ARG,ARGR,PERIOD)
            ELSE
              WRITE(LFNERR,901)
901           FORMAT(/,' *** SR EEAMAT: NO FILENAME GIVEN FOR A ',
     1                 'REFERENCE SUBDAILY ERP MODEL',/)
              CALL EXITRC(2)
            ENDIF
C
C SUBDAILY POLAR MOTION (FROM X- AND Y-POLE OBSERVATIONS)
            IF (ITYP.EQ.1 .AND. PARNAM(IPAR)(1:2).EQ.'SS')
     1        AMAT(IPAR)= DSIN(ARG)
            IF (ITYP.EQ.1 .AND. PARNAM(IPAR)(1:2).EQ.'SC')
     1        AMAT(IPAR)=-DCOS(ARG)
            IF (ITYP.EQ.2 .AND. PARNAM(IPAR)(1:2).EQ.'SS')
     1        AMAT(IPAR)= DCOS(ARG)
            IF (ITYP.EQ.2 .AND. PARNAM(IPAR)(1:2).EQ.'SC')
     1        AMAT(IPAR)= DSIN(ARG)
C
C SUBDAILY POLAR MOTION (FROM X- AND Y-POLE RATES)
            IF (ITYP.EQ.6 .AND. PARNAM(IPAR)(1:2).EQ.'SS')
     1        AMAT(IPAR)= DCOS(ARG)*ARGR
            IF (ITYP.EQ.6 .AND. PARNAM(IPAR)(1:2).EQ.'SC')
     1        AMAT(IPAR)= DSIN(ARG)*ARGR
            IF (ITYP.EQ.7 .AND. PARNAM(IPAR)(1:2).EQ.'SS')
     1        AMAT(IPAR)=-DSIN(ARG)*ARGR
            IF (ITYP.EQ.7 .AND. PARNAM(IPAR)(1:2).EQ.'SC')
     1        AMAT(IPAR)= DCOS(ARG)*ARGR
C
C SUBDAILY POLAR MOTION (FROM SAGNAF FREQUENCY)
            IF (ITYP.EQ.11.AND. PARNAM(IPAR)(1:2).EQ.'SS') THEN
              DODX=-DCOS(XSTELL(1))/DSIN(XSTELL(1))*DCOS(XSTELL(2))
              DODY= DCOS(XSTELL(1))/DSIN(XSTELL(1))*DSIN(XSTELL(2))
              AMAT(IPAR)=(+DODX*DSIN(ARG)+DODY*DCOS(ARG))*
     1                   AS2RAD*1000.D0
            ENDIF
            IF (ITYP.EQ.11.AND. PARNAM(IPAR)(1:2).EQ.'SC') THEN
              DODX=-DCOS(XSTELL(1))/DSIN(XSTELL(1))*DCOS(XSTELL(2))
              DODY= DCOS(XSTELL(1))/DSIN(XSTELL(1))*DSIN(XSTELL(2))
              AMAT(IPAR)=(-DODX*DCOS(ARG)+DODY*DSIN(ARG))*
     1                    AS2RAD*1000.D0
            ENDIF
C
C SUBDAILY UT1 (FROM UT1 OBSERVATIONS)
            IF (ITYP.EQ.3 .AND. PARNAM(IPAR)(1:2).EQ.'US')
     1        AMAT(IPAR)= DSIN(ARG)
            IF (ITYP.EQ.3 .AND. PARNAM(IPAR)(1:2).EQ.'UC')
     1        AMAT(IPAR)= DCOS(ARG)
C
C SUBDAILY UT1 (FROM UT1-RATE OBSERVATIONS)
            IF (ITYP.EQ.8 .AND. PARNAM(IPAR)(1:2).EQ.'US')
     1        AMAT(IPAR)= DCOS(ARG)*ARGR
            IF (ITYP.EQ.8 .AND. PARNAM(IPAR)(1:2).EQ.'UC')
     1        AMAT(IPAR)=-DSIN(ARG)*ARGR
C
C AMPLITUDES OF FUNDAMENTAL ARGUMENTS (DEPS, DPSI)
          ELSEIF (PARNAM(IPAR)(1:2).EQ.'ES' .OR.
     1            PARNAM(IPAR)(1:2).EQ.'EC' .OR.
     2            PARNAM(IPAR)(1:2).EQ.'PS' .OR.
     3            PARNAM(IPAR)(1:2).EQ.'PC') THEN
C
            READ(PARNAM(IPAR)(3:17),'(5I3)') (IFDARG(II),II=1,5)
C
C
            IF (IRCNUT.EQ.0) THEN
              CALL NUTVAL(ERPEPO,nutref%NUTFAR,IFDARG,ARG,ARGR,PERIOD)
            ELSE
              WRITE(LFNERR,902)
902           FORMAT(/,' *** SR EEAMAT: NO FILENAME GIVEN FOR A ',
     1                 'REFERENCE NUTATION MODEL',/)
              CALL EXITRC(2)
            ENDIF
C
            IF (ITYP.EQ. 9 .AND. PARNAM(IPAR)(1:2).EQ.'ES')
     1        AMAT(IPAR)= DCOS(ARG)*ARGR
            IF (ITYP.EQ. 9 .AND. PARNAM(IPAR)(1:2).EQ.'EC')
     1        AMAT(IPAR)=-DSIN(ARG)*ARGR
            IF (ITYP.EQ.10 .AND. PARNAM(IPAR)(1:2).EQ.'PS')
     1        AMAT(IPAR)= DCOS(ARG)*ARGR
            IF (ITYP.EQ.10 .AND. PARNAM(IPAR)(1:2).EQ.'PC')
     1        AMAT(IPAR)=-DSIN(ARG)*ARGR
C
          ELSE
            WRITE(LFNERR,903) PARNAM(IPAR)
903         FORMAT(/,' *** SR EEAMAT: INVALID PARAMETER TYPE',
     1             /,16X,'PARAMETER TYPE: ',A,/)
            CALL EXITRC(2)
          ENDIF
C
        ENDIF
C
      ENDDO
C
C COMPUTE OBSERVED-COMPUTED (IN MAS)
C ----------------------------------
      ERPAPR=0.D0
      ERPREF=0.D0
C
      IF (ITYP.EQ.4 .OR. ITYP.EQ.5 .OR.
     1    ITYP.EQ.9 .OR. ITYP.EQ.10) THEN

! GET A PRIORI NUTATION MODEL
        CALL NUTCOR(erpepo,nutapr,tu,aPsi,aEps,dtmoon,aPsiR,aEpsR)
        rho=648000/pi
        aPsi=aPsi*rho
        aEps=aEps*rho
        aPsiR=aPsiR*rho
        aEpsR=aEpsR*rho

! GET REFERENCE NUTATION MODEL
        CALL NUTCOR(erpepo,nutref,tu,rPsi,rEps,dtmoon,rPsiR,rEpsR)
        rPsi=rPsi*rho
        rEps=rEps*rho
        rPsiR=rPsiR*rho
        rEpsR=rEpsR*rho
C
        IF (ITYP.EQ.4) THEN
          ERPAPR=AEPS
          ERPREF=REPS
        ELSEIF (ITYP.EQ.5) THEN
          ERPAPR=APSI
          ERPREF=RPSI
        ELSEIF (ITYP.EQ.9) THEN
          ERPAPR=AEPSR
          ERPREF=REPSR
        ELSE
          ERPAPR=APSIR
          ERPREF=RPSIR
        ENDIF
C
C USE A PRIORI POLE
      ELSEIF (ITYP.LE.3) THEN
C NO A PRIORI FOR VLBI !!!!
        IF (IRCPOL.EQ.0 .AND. OBSSYS.NE.'V') THEN
          ISUBFL=0
          CALL POLDEF(ERPEPO,ISUBFL,XPOL,YPOL,DUT1,DUTGPS)
          IF (ITYP.EQ.1) THEN
            ERPAPR=-XPOL*180.D0*3600.D0/PI
          ELSEIF (ITYP.EQ.2) THEN
            ERPAPR=-YPOL*180.D0*3600.D0/PI
          ELSE
            ERPAPR=-DUT1*86400.D0
          ENDIF
        ENDIF
C
C GET REFERENCE SUBDAILY ERP MODEL
        CALL SUBMOD(ERPEPO,SUBFAR,NSUB  ,SUBMLT,SUBCOE,
     1              ERPSUB,ERPSUR)
C
        ERPREF=ERPSUB(ITYP)
C
      ELSEIF (ITYP.GE.6. .AND. ITYP.LE.8) THEN
        IF (IRCPOL.EQ.0) THEN
          ISUBFL=0
          CALL POLDEF(ERPEPO-DTDRFT/2.D0,ISUBFL,XPOL1,YPOL1,
     1                DUT11,DUTGPS1)
          CALL POLDEF(ERPEPO+DTDRFT/2.D0,ISUBFL,XPOL2,YPOL2,
     1                DUT12,DUTGPS2)
          IF (ITYP.EQ.6) THEN
            ERPAPR=-(XPOL2-XPOL1)*180.D0*3600.D0/PI/DTDRFT
          ELSEIF (ITYP.EQ.7) THEN
            ERPAPR=-(YPOL2-YPOL1)*180.D0*3600.D0/PI/DTDRFT
          ELSE
            DUT11=DUT11*86400.D0
            DUT12=DUT12*86400.D0
            IF (DUT12-DUT11.GT.0.5D0) DUT11=DUT11+1.D0
            ERPAPR=-(DUT12-DUT11)/DTDRFT
          ENDIF
        ENDIF
C
C GET REFERENCE SUBDAILY ERP MODEL
        CALL SUBMOD(ERPEPO,SUBFAR,NSUB  ,SUBMLT,SUBCOE,
     1              ERPSUB,ERPSUR)
C
        ERPREF=ERPSUR(ITYP-5)
C
      ELSEIF (ITYP.EQ.11) THEN
        IF (IRCPOL.EQ.0) THEN
          ISUBFL=0
          CALL POLDEF(ERPEPO,ISUBFL,XPOL,YPOL,DUT1,DUTGPS)
          CALL POLDEF(ERPEPO-DTDRFT/2.D0,ISUBFL,XPOL1,YPOL1,
     1                DUT11,DUTGPS1)
          CALL POLDEF(ERPEPO+DTDRFT/2.D0,ISUBFL,XPOL2,YPOL2,
     1                DUT12,DUTGPS2)
C
C EFFECT OF POLAR MOTION ON RELATIVE SAGNAC FREQUENCY
          DOMPOL=DCOS(XSTELL(1))/DSIN(XSTELL(1))*1.D3*
     1           (-XPOL*DCOS(XSTELL(2))+YPOL*DSIN(XSTELL(2)))
C
C EFFECT OF LOD ON RELATIVE SAGNAC FREQUENCY
          DUT11=DUT11*86400.D0
          DUT12=DUT12*86400.D0
          IF (DUT12-DUT11.GT.0.5D0) DUT11=DUT11+1.D0
C DUTAPR=DUT/DT IN S/S
          DUTAPR=(DUT12-DUT11)/DTDRFT/86400.D0
C DOMEGA IN RAD/SEC
          DOMUT1=DUTAPR*15.D0*AS2RAD*1.D3/OMEGA
CCCCCCCCCCCCCCCCCCCCC          ERPAPR=-DOMPOL-DOMUT1
CCCCCCCCCCCCCCCCCCCCC          ERPAPR=-DOMUT1
          ERPAPR=0.D0
        ENDIF
C
C GET REFERENCE DIURNAL POLAR MOTION MODEL (OPPOLZER TERMS)
        CALL SUBMOD(ERPEPO,SUBFAR,NSUB  ,SUBMLT,SUBCOE,
     1              ERPSUB,ERPSUR)
        DOMPOL=DCOS(XSTELL(1))/DSIN(XSTELL(1))*AS2RAD*1.D3*
     1         (-ERPSUB(1)*DCOS(XSTELL(2))+ERPSUB(2)*DSIN(XSTELL(2)))
        DOMUT1=ERPSUR(3)/86400.D0*15.D0*AS2RAD*1.D3/OMEGA
        ERPREF=DOMPOL+DOMUT1
C
      ENDIF
C
C O-C (CONVERSION FROM ARCSEC TO MAS)
C
      OMC=(ERPOBS+ERPAPR-ERPREF)*1000.D0
      IF (ITYP.EQ.3) THEN
        OMCCOR=DNINT(OMC/1000.D0)
        ERPOBS=ERPOBS-OMCCOR
        OMC=OMC-OMCCOR*1000.D0
      ENDIF
C
C SET OBSERVATION WEIGHT (IN MAS)
C -------------------------------
      IF (IUSSIG.EQ.1) THEN
        WEIGHT=WGTTYP*SIGMA0**2/(ERPSIG*1000.D0)**2
      ELSE
        WEIGHT=WGTTYP
      ENDIF
C
C WINDOW-SPECIFIC WEIGHTS
C -----------------------
      DO IWIN=1,NWIN
        IF (ERPEPO.GE.WINTIM(1,IWIN) .AND. ERPEPO.LT.WINTIM(2,IWIN))
     1    WEIGHT=WEIGHT/(WINSIG(IWIN)**2)
      ENDDO

C
C WRITE A-MATRIX ETC. TO SCRATCH FILE (FOR RESIDUAL COMPUTATION)
C --------------------------------------------------------------
      IF (IRCRES.EQ.0) THEN
        WRITE(LFNPLT) ITYP,ERPEPO,ERPOBS*1000.D0,
     1                (ERPREF-ERPAPR)*1000.D0,
     2                (AMAT(II),II=1,NPAR),OMC,WEIGHT
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
