      MODULE s_PRIGIM
      CONTAINS

C*
      SUBROUTINE PRIGIM(IPART ,NPAR  ,LOCQ  ,XXX   ,ANOR  ,RMS   ,
     1                  OPTGIM,POLGIM,NAMGIM,EPOGIM,SCAGIM,IP    ,
     2                  INFGIM,MAXPAR)
CC
CC NAME       :  PRIGIM
CC
CC PURPOSE    :  PRINT GLOBAL IONOSPHERE MODEL PARAMETERS AND
CC               ADDITIONAL INFORMATION CONCERNING LATITUDE BAND,
CC               MAXIMUM AND MINIMUM TEC, MEAN TEC, SINGLE-LAYER
CC               HEIGHT PARAMETERS, MEAN RADIUS OF THE EARTH.
CC
CC PARAMETERS :
CC         IN :  IPART  : RESULTS PART 1(=1) OR 2 (=2)        I*4
CC               NPAR   : NUMBER OF PARAMETERS                I*4
CC               LOCQ(K,I),K=1,..,MAXLCQ,I=1,..,NPAR:         I*4(*,*)
CC                        CHARACTERISTICS FOR EACH PARAMETER
CC               XXX(I),I=1,..,NPAR: SOLUTION VECTOR          R*8(*)
CC               ANOR(I),I=1,..,NPAR*(NPAR+1)/2: INVERSE OF   R*8(*)
CC                        NORMAL EQUATION MATRIX
CC               RMS    : RMS ERROR OF UNIT WEIGHT            R*8
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
CC               IP     : PARAMETER INDEX                     I*4
CC        OUT :  INFGIM(I,J),I=1,2,J=1,..,OPTGIM(7):          R*8(2,*)
CC                        I=1: MAXIMUM TEC (TECU)
CC                        I=2: RMS ERROR (TECU)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.0
CC
CC CREATED    :  22-NOV-95
CC
CC CHANGES    :  23-NOV-95 : SS: HANDLE MODEL IMPROVEMENT
CC               30-NOV-95 : SS: DEFINE "XXXAPR"
CC               20-DEC-95 : SS: DECLARE "NUMGIM" AS CH*7
CC               20-DEC-95 : SS: "IONTXT" IN CALL OF SR GETGIM
CC               20-DEC-95 : SS: DO NOT PRINT MEAN TEC / ZERO-DEGREE
CC                               COEFFICIENT FOR REGIONAL MODELS
CC               28-DEC-95 : SS: TID INDICATOR INTRODUCED
CC               07-APR-97 : SS: HANDLE CASE OF NEGATIVE VARIANCES
CC               13-NOV-97 : SS: "IONFIL" IN CALL OF SR GETGIM
CC               14-NOV-97 : SS: SR MAXTEC MODIFIED
CC               17-NOV-97 : SS: "IONTIT" IN CALL OF SR GETGIM
CC               20-JAN-98 : SS: "IONINF" ADDED
CC               26-JAN-98 : SS: STATION-SPECIFIC GIMS
CC               29-APR-98 : SS: DTEC LC
CC               26-MAY-98 : SS: "INFGIM" ADDED
CC               06-SEP-01 : SS: "MAXPAR" FROM 3000 TO 4000
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               10-MAR-08 : MM: MAXPAR 4000 -> 5000
CC               14-SEP-11 : LP: TAKE MAXPAR FROM CALLING SR
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1995     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: LFNPRT, LFNERR
      USE m_maxdim, ONLY: MAXGIT, MAXGIM
      USE d_const,  ONLY: PI
      USE f_ikf
      USE s_dimtst
      USE s_getgim
      USE s_maxtec
      USE s_exitrc
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IDEG  , II    , IMOD  , IONTYP, IORD  , IP    ,
     1          IPAR  , IPART , IRC   , IREQ  , ITRM  , MAXPAR, MXCLCQ,
     2          NMOD  , NMODEL, NPAR
C
      REAL*8    AVAR  , RMS   , RMS1  , VALNEW, VALOLD, XLAT1 , XLAT2 ,
     1          XPOS1 , XPOS2 , XPOS3 , XPOS4 , XXX1
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      CHARACTER*80 IONTIT
      CHARACTER*32 IONFIL
      CHARACTER*20 STRTEC
      CHARACTER*16 NAMGIM(*),IONTXT(MAXGIM),STRMOD
      CHARACTER*6  MXNLCQ
C
      REAL*8 XXX(*),ANOR(*),XXXAPR(MAXPAR)
      REAL*8 POLGIM(3,*),EPOGIM(2,*),SCAGIM(*)
      REAL*8 TECMAX(4),TECMIN(5),LATBND(2),TEC0(2)
      REAL*8 IONCOE(MAXGIT,MAXGIM),IONSIG(MAXGIT,MAXGIM)
      REAL*8 IONDEV(10,MAXGIM),INFGIM(2,*)
C
      INTEGER*4 LOCQ(MXCLCQ,*),OPTGIM(*)
      INTEGER*4 IONREQ(6,MAXGIM),NTERM(MAXGIM),NM(MAXGIT,2,MAXGIM)
      INTEGER*4 IFIRST(2),IONINF(4)
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
      COMMON/CPRIGI/IONDEV,IONCOE,IONSIG,IONREQ,NTERM,NM,IONTXT
C
C
      DATA IFIRST/1,1/
C
      IF (IFIRST(IPART).EQ.1) THEN
C
C CHECK "MAXPAR"
C --------------
        CALL DIMTST(1,1,2,'PRIGIM','MAXPAR','PARAMETERS',
     1    ' ',NPAR,MAXPAR,IRC)
C
C INITIALIZE "XXXAPR"
C -------------------
        DO 610 IPAR=1,NPAR
          XXXAPR(IPAR)=0.D0
610     CONTINUE
C
C STORE A PRIORI VALUES OF IONOSPHERIC COEFFICIENTS
C -------------------------------------------------
        IF (OPTGIM(5).GT.0) THEN
C
C READ A PRIORI INFORMATION
          CALL GTFLNA(0,'IONOS  ',IONFIL,IRC)
C
          CALL GETGIM(IONFIL,1     ,NMODEL,IONREQ,IONDEV,NTERM ,
     1                NM    ,IONCOE,IONSIG,IONTXT,IONTIT,IONINF,
     2                IONTYP)
C
C UPDATE "XXXAPR"
          DO 710 IPAR=1,NPAR
            IF (LOCQ(1,IPAR).EQ.19.AND.
     1          LOCQ(2,IPAR).EQ. 1) THEN
              IMOD=LOCQ(3,IPAR)
              IDEG=LOCQ(4,IPAR)
              IORD=LOCQ(5,IPAR)
C
              DO 720 ITRM=1,NTERM(IMOD)
                IF (NM(ITRM,1,IMOD).EQ.IDEG.AND.
     1              NM(ITRM,2,IMOD).EQ.IORD) THEN
                  XXXAPR(IPAR)=IONCOE(ITRM,IMOD)
                  GO TO 710
                END IF
720           CONTINUE
              WRITE(LFNERR,920)
920           FORMAT(/,' *** SR PRIGIM: UNEXPECTED ERROR',/)
              CALL EXITRC(2)
            END IF
710       CONTINUE
        END IF
C
C PRINT ADDITIONAL INFORMATION
C ----------------------------
        WRITE(LFNPRT,210)
210     FORMAT(' MODEL / STATION     MIN LAT  MAX LAT   ',
     1    'MEAN TEC  RMS ERR   ',
     2    'MAX TEC  RMS ERR  LAT     LON       ',
     3    'MIN TEC  RMS ERR  LAT     LON',/,
     4    '                     (DEG)    (DEG)     ',
     5    '(TECU)    (TECU)    ',
     6    '(TECU)   (TECU)   (DEG)   (DEG)     ',
     7    '(TECU)   (TECU)   (DEG)   (DEG)',/,
     8    1X,131('-'),/)
C
        NMOD=OPTGIM(7)
        DO 220 IMOD=1,NMOD
          CALL MAXTEC(IMOD  ,NPAR  ,LOCQ  ,XXX   ,XXXAPR,ANOR  ,
     1                RMS   ,SCAGIM,TECMAX,TECMIN,TEC0  ,LATBND)
          XLAT1=180.D0/PI*LATBND(1)
          XLAT2=180.D0/PI*LATBND(2)
          IF (XLAT1.LE.-45.D0.AND.XLAT2.GE.45.D0) THEN
            WRITE(STRTEC,'(2F10.2)') (TEC0(II),II=1,2)
          ELSE
            STRTEC='      ---       --- '
          END IF
          XPOS1=180.D0/PI*TECMAX(3)
          XPOS2=180.D0/PI*TECMAX(4)
          XPOS3=180.D0/PI*TECMIN(3)
          XPOS4=180.D0/PI*TECMIN(4)
          WRITE(LFNPRT,230) NAMGIM(IMOD),XLAT1,XLAT2,STRTEC,
     1      TECMAX(1),TECMAX(2),XPOS1,XPOS2,
     2      TECMIN(1),TECMIN(2),XPOS3,XPOS4
230       FORMAT(1X,A16,1X,2F9.2,1X,A20,2(F10.2,F9.2,F8.2,F9.2))
C
          INFGIM(1,IMOD)=TECMAX(1)
          INFGIM(2,IMOD)=TECMAX(2)
220     CONTINUE
C
C PRINT SINGLE-LAYER HEIGHT PARAMETERS
C ------------------------------------
        IF (OPTGIM(5).GT.0) THEN
          WRITE(LFNPRT,310)
310       FORMAT(//,' SINGLE-LAYER HEIGHT PARAMETERS:',
     1      //,' MODEL / STATION      A PRIORI VALUE  NEW VALUE    ',
     2      'IMPROVEMENT  RMS ERROR (KM)',
     3      /,1X,131('-'),/)
C
          DO 320 IPAR=1,NPAR
            IF (LOCQ(1,IPAR).EQ.19.AND.
     1          LOCQ(2,IPAR).EQ.2) THEN
              IMOD=LOCQ(3,IPAR)
              XXX1=1.D-3*XXX(IPAR)/SCAGIM(2)
              AVAR=ANOR(IKF(IPAR,IPAR))
              IF (AVAR.GE.0.D0) THEN
                RMS1=1.D-3*RMS*DSQRT(AVAR)/SCAGIM(2)
              ELSE
                RMS1=0.D0
                WRITE(LFNERR,930)
930             FORMAT(/,' ### SR PRIGIM: RMS ERROR SET TO ZERO ',
     1            'DUE TO NEGATIVE VARIANCE',/)
              ENDIF
              IF (IMOD.EQ.0) THEN
                VALOLD=1.D-3*POLGIM(1,1)
              ELSE
                VALOLD=1.D-3*POLGIM(1,IMOD)
              END IF
              VALNEW=VALOLD+XXX1
C
              IF (IMOD.EQ.0) THEN
                STRMOD='       '
              ELSE
                STRMOD=NAMGIM(IMOD)
              ENDIF
              WRITE(LFNPRT,330) STRMOD,VALOLD,VALNEW,XXX1,RMS1
330           FORMAT(1X,A16,F12.2,F16.2,2F13.2)
            END IF
320       CONTINUE
        END IF
C
C PRINT HEADER LINE FOR COEFFICIENTS
C ----------------------------------
        IF (OPTGIM(10).EQ.1) THEN
          WRITE(LFNPRT,410)
410       FORMAT(//,' IONOSPHERIC COEFFICIENTS:',
     1      //,' MODEL / STATION      DEG  ORD    A PRIORI VALUE  ',
     2      'NEW VALUE      IMPROVEMENT  RMS (TECU)',
     3      /,1X,131('-'))
        ELSE
          WRITE(LFNPRT,420)
420       FORMAT(//,' DTEC PARAMETERS:',
     1      //,' MODEL / STATION      DEG  ORD    VALUE        ',
     2      'RMS (TECU**2)',
     3      /,1X,131('-'))
        ENDIF
C
        IFIRST(IPART)=0
      END IF
C
C PRINT COEFFICIENTS
C ------------------
      IREQ=LOCQ(2,IP)
      IF (IREQ.EQ.2) RETURN
C
      IMOD=LOCQ(3,IP)
      IDEG=LOCQ(4,IP)
      IORD=LOCQ(5,IP)
C
      XXX1=XXX(IP)/SCAGIM(IREQ)
      AVAR=ANOR(IKF(IP,IP))
      IF (AVAR.GE.0.D0) THEN
        RMS1=RMS*DSQRT(AVAR)/SCAGIM(IREQ)
      ELSE
        RMS1=0.D0
        WRITE(LFNERR,930)
      ENDIF
      VALOLD=XXXAPR(IP)
      VALNEW=VALOLD+XXX1
C
      IF (IDEG.EQ.0) THEN
        IF (IREQ.EQ.1) THEN
          WRITE(LFNPRT,510) NAMGIM(IMOD),IDEG,IORD,VALOLD,VALNEW,
     1      XXX1,RMS1
510       FORMAT(/,1X,A16,3X,2I5,F15.6,F16.6,F15.6,F11.4)
        ELSE
          WRITE(LFNPRT,515) NAMGIM(IMOD),IDEG,IORD,XXX1,RMS1
515       FORMAT(/,1X,A16,3X,2I5,F15.6,F11.4)
        ENDIF
      ELSE
        IF (IREQ.EQ.1) THEN
          WRITE(LFNPRT,520) IDEG,IORD,VALOLD,VALNEW,XXX1,RMS1
520       FORMAT(20X,2I5,F15.6,F16.6,F15.6,F11.4)
        ELSE
          WRITE(LFNPRT,525) IDEG,IORD,XXX1,RMS1
525       FORMAT(20X,2I5,F15.6,F11.4)
        ENDIF
      END IF
C
      RETURN
      END SUBROUTINE

      END MODULE
