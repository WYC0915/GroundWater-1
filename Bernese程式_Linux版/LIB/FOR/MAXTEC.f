      MODULE s_MAXTEC
      CONTAINS

C*
      SUBROUTINE MAXTEC(IMODEL,NPAR  ,LOCQ  ,XXX   ,XXXAPR,ANOR  ,
     1                  RMS   ,SCAGIM,TECMAX,TECMIN,TEC0  ,LATBND)
CC
CC NAME       :  MAXTEC
CC
CC PURPOSE    :  SEARCH FOR THE MAXIMUM AND MINIMUM TEC OF GLOBAL
CC               IONOSPHERE MODELS (WITHIN LATITUDE BAND COVERED).
CC               BOTH TEC VALUES, THEIR ASSOCIATED RMS ERRORS, AND
CC               COORDINATES IN THE SUN-FIXED REFERENCE FRAME ARE
CC               RETURNED. FURTHERMORE A TEST VALUE IS COMPUTED.
CC
CC PARAMETERS :
CC         IN :  IMODEL : MODEL INDEX                         I*4
CC               NPAR   : NUMBER OF PARAMETERS                I*4
CC               LOCQ(K,I),K=1,..,MAXLCQ,I=1,..,NPAR:         I*4(*,*)
CC                        CHARACTERISTICS FOR EACH PARAMETER
CC               XXX(I),I=1,..,NPAR: SOLUTION VECTOR          R*8(*)
CC               XXXAPR(I),I=1,..,NPAR: A PRIORI VALUES       R*8(*)
CC               ANOR(I),I=1,..,NPAR*(NPAR+1)/2: INVERSE OF   R*8(*)
CC                        NORMAL EQUATION MATRIX
CC               RMS    : RMS ERROR OF UNIT WEIGHT            R*8
CC               SCAGIM : SCALING FACTOR FOR                  R*8(*)
CC                        (1): ION. COEFFICIENTS
CC                        (2): SINGLE-LAYER HEIGHT
CC                        (3): DTEC PARAMETERS
CC        OUT :  TECMAX : (1): MAXIMUM TEC VALUE (TECU)       R*8(*)
CC                        (2): RMS ERROR (TECU)
CC                        (3): LATITUDE
CC                        (4): SUN-FIXED LONGITUDE
CC               TECMIN : (1): MINIMUM TEC VALUE (TECU)       R*8(*)
CC                        (2): RMS ERROR (TECU)
CC                        (3): LATITUDE
CC                        (4): SUN-FIXED LONGITUDE
CC                        (5): TEST VALUE (NEGATIVE TEC)
CC               TEC0   : (1): MEAN TEC (TECU)                R*8(*)
CC                        (2): RMS (TECU)
CC               LATBND : (1): MINIMUM LATITUDE               R*8(*)
CC                        (2): MAXIMUM LATITUDE
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.0
CC
CC CREATED    :  07-NOV-95
CC
CC CHANGES    :  08-NOV-95 : SS: COMPUTE TEST VALUE (NEGATIVE TEC)
CC               22-NOV-95 : SS: APPLY "SCAGIM(1)"
CC               22-NOV-95 : SS: RETURN "LATBND"
CC               23-NOV-95 : SS: RETURN "TEC0"
CC               30-NOV-95 : SS: TAKE "XXXAPR" INTO ACCOUNT
CC               14-NOV-97 : SS: "IOK" REMOVED
CC               29-APR-98 : SS: DTEC LC
CC               25-MAY-98 : SS: R*8 LOOPS REPLACED
CC               16-JAN-01 : SS: DO NOT STOP IN CASE OF "IPAR1=0"
CC               04-SEP-01 : SS: SR ASLEFU REPLACED BY SR ASLEF2
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1995     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: PI
      USE f_ikf
      USE f_aslef2
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IDEG  , ILAT  , ILON  , IMODEL, IORD  ,
     1          IP1   , IP2   , IPAR  , IPAR1 , IPAR2 , IREQ  , ISTEP ,
     2          ITEC  , MXCLCQ, NARG  , NDEG  , NLAT  , NLON  , NORD  ,
     3          NPAR  , NSTEP
C
      REAL*8    DARG  , DDARG , EMAX  , EMIN  , ETST  , QSUM1 ,
     1          QSUM2 , RMS   , XLAT  , XLAT0 , XLON  , XLON0
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*6 MXNLCQ
C
      REAL*8 XXX(*),ANOR(*),SCAGIM(*),XXXAPR(*)
      REAL*8 TECMAX(*),TECMIN(*),LATBND(*),TEC0(*)
C
      INTEGER*4 LOCQ(MXCLCQ,*)
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
C
C INITIALIZE SOME OUTPUT PARAMETERS
      DO I=1,4
        TECMAX(I)=0.D0
      ENDDO
      DO I=1,5
        TECMIN(I)=0.D0
      ENDDO
C
C LOOK FOR PARAMETER SEQUENCE OF REQUESTED MODEL
      IPAR1=0
      NDEG=0
      NORD=0
C
      DO 110 IPAR=1,NPAR
        IF (LOCQ(1,IPAR).EQ.19 .AND.
     1      LOCQ(2,IPAR).NE. 2 .AND.
     2      LOCQ(3,IPAR).EQ.IMODEL) THEN
          IF (LOCQ(4,IPAR).EQ.0) THEN
            IPAR1=IPAR
            IREQ=LOCQ(2,IPAR)
C
            LATBND(1)=PI/648000.D0*LOCQ(6,IPAR)
            LATBND(2)=PI/648000.D0*LOCQ(7,IPAR)
C
            TEC0(1)=XXX(IPAR)/SCAGIM(IREQ)+XXXAPR(IPAR)
            TEC0(2)=RMS*DSQRT(ANOR(IKF(IPAR,IPAR)))/SCAGIM(IREQ)
          END IF
C
          IPAR2=IPAR
          NDEG=MAX0(LOCQ(4,IPAR),NDEG)
          NORD=MAX0(LOCQ(5,IPAR),NORD)
        END IF
110   CONTINUE
C
C EXIT, IF INFORMATION CONCERNING LATITUDE BAND NOT FOUND
      IF (IPAR1.EQ.0) THEN
        LATBND(1)=0.D0
        LATBND(2)=0.D0
C
        WRITE(LFNERR,120)
120     FORMAT(/,' ### SR MAXTEC: UNEXPECTED ERROR',/)
        RETURN
      END IF
C
C RETURN, IF CURRENT MODEL NOT OBSERVED
      IF (LOCQ(6,IPAR1).EQ. 324000 .AND.
     1    LOCQ(7,IPAR1).EQ.-324000) THEN
        LOCQ(6,IPAR1)=0
        LOCQ(7,IPAR1)=0
        LATBND(1)=0.D0
        LATBND(2)=0.D0
C
        WRITE(LFNERR,130)
130     FORMAT(/,' ### SR MAXTEC: UNOBSERVED TEC',/)
        RETURN
      ENDIF
C
C RETURN, IF CONSTANT TEC ESTIMATED
      IF (IPAR2.EQ.IPAR1) THEN
        DO I=1,2
          TECMAX(I)=TEC0(I)
          TECMIN(I)=TEC0(I)
        ENDDO
        IF (TECMIN(2).GT.0.D0) TECMIN(5)=TECMIN(1)/TECMIN(2)
        RETURN
      ENDIF
C
C PART 1: ROUGH SCANNING OF THE TEC STRUCTURE
C -------------------------------------------
      EMAX=-1.D10
      EMIN= 1.D10
C
      DARG=PI/180.D0*2.5D0
C
      NLAT=IDNINT(PI/2.D0/DARG)
      NLON=IDNINT(PI/DARG)
C
      DO 210 ILAT=-NLAT,NLAT
        XLAT=ILAT*DARG
        IF (XLAT.LT.LATBND(1).OR.
     1      XLAT.GT.LATBND(2)) GO TO 210
C
        DO 220 ILON=-NLON,NLON
          XLON=ILON*DARG
          ETST=0.D0
          DO 230 IPAR=IPAR1,IPAR2
            IDEG=LOCQ(4,IPAR)
            IORD=LOCQ(5,IPAR)
            ETST=ETST+(XXX(IPAR)/SCAGIM(IREQ)+XXXAPR(IPAR))*
     1        ASLEF2(XLAT,XLON,IDEG,IORD,NDEG,NORD)
230       CONTINUE
C
          IF (ETST.GT.EMAX) THEN
            EMAX=ETST
            TECMAX(3)=XLAT
            TECMAX(4)=XLON
          END IF
          IF (ETST.LT.EMIN) THEN
            EMIN=ETST
            TECMIN(3)=XLAT
            TECMIN(4)=XLON
          END IF
220     CONTINUE
210   CONTINUE
C
      TECMAX(1)=EMAX
      TECMIN(1)=EMIN
C
C PART 2: FINE SCANNING
C ---------------------
      NARG=10
C
      NSTEP=2
      DO 310 ISTEP=1,NSTEP
        DDARG=DARG/NARG
C
        DO 320 ITEC=1,2
          IF (ITEC.EQ.1) THEN
            XLAT0=TECMAX(3)
            XLON0=TECMAX(4)
          ELSE
            XLAT0=TECMIN(3)
            XLON0=TECMIN(4)
          END IF
C
          DO 330 ILAT=-NARG,NARG
            XLAT=XLAT0+ILAT*DDARG
            IF (XLAT.LT.LATBND(1).OR.
     1          XLAT.GT.LATBND(2)) GO TO 330
C
            DO 340 ILON=-NARG,NARG
              XLON=XLON0+ILON*DDARG
              ETST=0.D0
              DO 350 IPAR=IPAR1,IPAR2
                IDEG=LOCQ(4,IPAR)
                IORD=LOCQ(5,IPAR)
                ETST=ETST+(XXX(IPAR)/SCAGIM(IREQ)+XXXAPR(IPAR))*
     1            ASLEF2(XLAT,XLON,IDEG,IORD,NDEG,NORD)
350           CONTINUE
C
              IF (ITEC.EQ.1) THEN
                IF (ETST.GT.TECMAX(1)) THEN
                  TECMAX(1)=ETST
                  TECMAX(3)=XLAT
                  TECMAX(4)=XLON
                END IF
              ELSE
                IF (ETST.LT.TECMIN(1)) THEN
                  TECMIN(1)=ETST
                  TECMIN(3)=XLAT
                  TECMIN(4)=XLON
                END IF
              END IF
340         CONTINUE
330       CONTINUE
320     CONTINUE
C
        DARG=DARG/NARG
310   CONTINUE
C
      TECMAX(4)=DATAN2(DSIN(TECMAX(4)),DCOS(TECMAX(4)))
      TECMIN(4)=DATAN2(DSIN(TECMIN(4)),DCOS(TECMIN(4)))
C
C PART 3: COMPUTATION OF THE FORMAL ERRORS
C ----------------------------------------
      DO 410 ITEC=1,2
        IF (ITEC.EQ.1) THEN
          XLAT0=TECMAX(3)
          XLON0=TECMAX(4)
        ELSE
          XLAT0=TECMIN(3)
          XLON0=TECMIN(4)
        END IF
C
        QSUM1=0.D0
        DO 420 IP1=IPAR1,IPAR2
          QSUM2=0.D0
          DO 430 IP2=IPAR1,IPAR2
            IDEG=LOCQ(4,IP2)
            IORD=LOCQ(5,IP2)
            QSUM2=QSUM2+
     1        ASLEF2(XLAT0,XLON0,IDEG,IORD,NDEG,NORD)*ANOR(IKF(IP2,IP1))
430       CONTINUE
          IDEG=LOCQ(4,IP1)
          IORD=LOCQ(5,IP1)
          QSUM1=QSUM1+
     1      QSUM2*ASLEF2(XLAT0,XLON0,IDEG,IORD,NDEG,NORD)
420     CONTINUE
C
        IF (ITEC.EQ.1) THEN
          TECMAX(2)=RMS*DSQRT(QSUM1)/SCAGIM(IREQ)
        ELSE
          TECMIN(2)=RMS*DSQRT(QSUM1)/SCAGIM(IREQ)
        END IF
410   CONTINUE
C
C COMPUTE TEST VALUE (NEGATIVE TEC)
      IF (TECMIN(2).GT.0.D0) TECMIN(5)=TECMIN(1)/TECMIN(2)
C
      RETURN
      END SUBROUTINE

      END MODULE
