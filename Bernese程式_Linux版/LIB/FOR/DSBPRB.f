      MODULE s_DSBPRB
      CONTAINS

C*
      SUBROUTINE DSBPRB(TBNDS ,XBNDS ,TBEG  ,DTPI  ,DTABD ,Q     ,
     1                  ORBMOD,NUMSAT,FAC   ,YCOE  ,XV    ,A     ,
     2                  E     ,XI    ,XKN   ,PER   ,TPER  ,TB12S )
CC
CC NAME       :  DSBPRB
CC
CC PURPOSE    :  1.) SOLVE BOUNDARY VALUE PROBLEM
CC               2.) INTEGRATE TO STARTING POINT TBEG
CC                   (FORWARD OR BACKWARD)
CC               3.) RETURN INITIAL CONDITIONS AND OSCULATING ELEMENTS
CC                   FOR TIME TBEG
CC
CC PARAMETERS :
CC         IN :  TBNDS(I),I=1,2: LEFT AND RIGHT BOUNDARY        R*8
CC               XBNDS(K,I),K=1,2,3,I=1,2: BOUNDARY POSITIONS   R*8
CC               TBEG   : STARTING TIME OF ARC                  R*8
CC               DTPI   : LENGTH OF PARTIAL INTERVAL            R*8
CC               DTABD  : TABULAR INTERVAL OF "OBSERVATIONS"    R*8
CC               Q      : POLYNOMIAL DEGREE                     I*4
CC               ORBMOD : ORBIT MODEL ARRAY                     I*4(*)
CC               NUMSAT : SATELLITE NUMBER                      I*4
CC        OUT :  FAC(I),I=1,2,..,Q+1: FACTORIALS                R*8
CC               YCOE(K,I),K=1,2,3, I=1,2,..,Q+1:               R*8
CC                        POLYNOMIAL COEFFICIENTS
CC               XV(K),K=1,2,..,6: POS. + VEL. AT TBEG          R*8
CC               A      : SEMIMAJOR AXIS AT TBEG                R*8
CC               E      : EXCENTRICITY AT TBEG                  R*8
CC               XI     : INCLINATION  AT TBEG                  R*8
CC               XKN    : R.A. OF NODE AT TBEG                  R*8
CC               PER    : PERIGEE      AT TBEG                  R*8
CC               TPER   : PERIGEE PASSING TIME (AT TBEG)        R*8
CC      LOCAL :  TB12S(K),K=1,2,...: SUBINTERVAL BOUNDARIES     R*8
CC
CC REMARKS    :  THIS SR USES A VERY EFFICIENT INTEGRATION
CC               ROUTINE (INTFST)
CC
CC AUTHOR     :  M.ROTHACHER, G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  90/07/27 10:48
CC
CC CHANGES    :  07-SEP-92 : ??: IF BEGIN OF ARC IS WITHIN "DTABD" OF A
CC                               BOUNDARY TIME, DO NOT INTEGRATE (ELSE THE
CC                               INTEGRATION INTERVAL BECOMES ZERO IN
CC                               "PRTINT")
CC               27-JUN-96 : TS: ADDED ORBIT MODEL ARRAY
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               18-OCT-06 : MP: PARAMETER TO CALL OF INTFST ADDED
CC               04-MAY-08 : RD: NUMSAT ADDED TO CALL OF SR XYZELE,INTFST
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1990     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const, ONLY: GM
      USE s_prtint
      USE s_intfst
      USE s_ypol
      USE s_xyzele
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , INT   , NINTS , NUMSAT
C
      REAL*8    A     , DTABD , DTPI  , E     , ERRMAX, H     , PER   ,
     1          T0    , TBEG  , TOSCS , TPER  , TSEC  , XI    , XKN   ,
     2          Z0(1)    , ZCOE(1)
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      REAL*8    TBNDS(2),XBNDS(3,2),YCOE(3,*),TB12S(*),FAC(*),XV(6)
      REAL*8    XSAT(6)
C
      INTEGER*4 Q,LOCQ(6,3),ORBMOD(*)
C
C COMMON BLOCKS
C -------------
C
C INITIALIZE FACTORIALS
C ---------------------
      FAC(1) = 1.D0
      DO 10 I = 2,Q+1
        FAC(I) = (I-1)*FAC(I-1)
10    CONTINUE
C
C 1.) SOLVE BOUNDARY-VALUE-PROBLEM
C --------------------------------
      CALL INTFST(1,0,TBNDS(1),TBNDS(2),XBNDS,Q,0,LOCQ,Z0,ORBMOD,
     1            NUMSAT,T0,H,YCOE,ZCOE,ERRMAX)
C
C 2.) INTEGRATE FORWARDS OR BACKWARDS TO START OF ARC
C ---------------------------------------------------
      IF (TBEG+DTABD .LT. TBNDS(1) .OR. TBEG-DTABD .GT. TBNDS(2)) THEN
        TSEC = (TBNDS(1)-T0)*86400
        IF (DABS(TSEC) .LT. 0.0001) TSEC=0.D0
        CALL YPOL(1,Q,3,H,FAC,TSEC,YCOE,XSAT)
        CALL PRTINT(DTPI,TBNDS(1),TBEG,DTABD,NINTS,TB12S,TOSCS)
C
        DO 80 INT = 1,NINTS
          CALL INTFST(0,0,TB12S(INT),TB12S(INT+1),XSAT,Q,0,LOCQ,Z0,
     1                ORBMOD,NUMSAT,T0,H,YCOE,ZCOE,ERRMAX)
          TSEC = (TB12S(INT+1)-T0)*86400
          IF (DABS(TSEC) .LT. 0.0001) TSEC=0.D0
          CALL YPOL(1,Q,3,H,FAC,TSEC,YCOE,XSAT)
80      CONTINUE
      ENDIF
C
C 3.) COMPUTE POSITION AND VELOCITY AT START OF ARC AND OSCULATING
C     ELEMENTS
C ----------------------------------------------------------------
      TSEC = (TBEG-T0)*86400.D0
      IF (DABS(TSEC) .LT. 0.0001) TSEC=0.D0
      CALL YPOL(1,Q,3,H,FAC,TSEC,YCOE,XV)
      CALL XYZELE(GM,0.D0,XV,XV(4),NUMSAT,A,E,XI,XKN,PER,TPER)
C
C END
C ---
      RETURN
      END SUBROUTINE

      END MODULE
