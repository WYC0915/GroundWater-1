      MODULE s_SNGDET
      CONTAINS

C*
      SUBROUTINE SNGDET(IORSYS,NUMSAT,TBEG,TEND,T0ARC,NPOINT,
     1                  T0,H,Q,FAC,YCOE,MAXBND,DSMAX,NINT,TBND)
CC
CC NAME       :  SNGDET
CC
CC PURPOSE    :  DETECT RAPID ROTATIONS OF THE SATELLITES
CC               (ALMOST SINGULAR)
CC
CC PARAMETERS :
CC         IN :  IORSYS : ORBIT SYSTEM                        I*4
CC                        =1: B1950.0
CC                        =2: J2000.0
CC               NUMSAT : SATELLITE NUMBER                    I*4
CC               TBEG   : START OF INTERVAL                   R*8
CC               TEND   : END OF INTERVAL                     R*8
CC               T0ARC  : TIME ORIGIN FOR ARC                 R*8
CC               NPOINT : NUMBER OF POINTS OF GRID FOR SEARCH R*8
CC               T0     : ORIGIN OF DEVELOPMENT (SAT POS)     R*8
CC               H      : NORMALIZATION CONSTANT (SAT POS)    R*8
CC               Q      : POLYNOMIAL DEGREE (SAT POS)         I*4
CC               FAC    : FACTORIALS                          R*8(*)
CC               YCOE   : POLYNOMIAL COEFFICIENTS             R*8(*,*)
CC               DSMAX  : MAXIMUM INCREMENT OF ANGLE (SIN)    R*8
CC                        ALLOWED BETWEEN SUBSEQUENT INTEGRATION STEPS
CC        OUT :  NINT   : NUMBER OF INTERVALS FOR INTEGRATION I*4
CC                        =1 : NORMAL CASE (NO RAPID INTEGRATION)
CC                        =2 : SPLIT UP INTO TWO INTERVALS
CC                        =3 : SPLIT UP INTO THREE INTERVALS
CC                        ETC
CC               TBND   : INTERVAL BOUNDARIES                 R*8(*,*)
CC                   (1,I) : START TIME FOR INTERVAL I
CC                   (2,I) : END TIME FOR INTERVAL I
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.5  (JAN 94)
CC
CC CREATED    :  93/08/19
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC            :  23-OCT-94 : GB: REPLACE SR SUNEFF BY SR MOSUPN.
CC               06-JUN-96 : TS: REMOVED UNUSED VARIABLES
CC               11-JUN-03 : HU: NEW CALL FOR MOSUPN
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               21-MAR-07 : CU/RD: PREVENT ZERO-INDEX FOR TBND(2,NINT)
CC               04-MAY-08 : RD: NUMSAT ADDED TO CALL OF SR XYZELE,DEQRHS
CC               01-OCT-10 : CR: NEW CALL OF MOSUPN
CC               01-OCT-10 : CR: CALL s_MOSUPN AS MODULE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1993     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: GM, PI
      USE s_mosupn
      USE s_vprod
      USE s_ypol
      USE s_exitrc
      USE s_xyzele
      USE s_ddreh
      USE s_dmlmav
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IFIRST, INT   , IORSYS, IPOINT, MAXBND, MAXPTS,
     1          NINT  , NPOINT, NUMSAT
C
      REAL*8    A     , DMAX  , DMIN  , DSMAX , DT    , DUM   , DUMMY ,
     1          E     , H     , PER   , PHI   , PHI1  , PHI2  , T0    ,
     2          T0ARC , TBEG  , TEND  , TEST  , TPER  , TSEC  , XDUM  ,
     3          XI    , XITER , XKN   , XKNTER, YDUM
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      PARAMETER (MAXPTS=50)
C
      REAL*8    YCOE(3,*),FAC(*),XSUN(4),XSAT(6),E2(3)
      REAL*8    TBND(2,*),DRNS(3,3),DRIS(3,3)
C
      INTEGER*4 Q
C
      REAL*8    T(MAXPTS),D(MAXPTS)
      REAL*8    DUMPRE(3,3),DUMNUT(3,3),DUMMON(4),DUMVEL(3)
C
C
      DATA IFIRST/1/
C
C INITIALIZE FACTORIALS, CHECK MAX DIM
C ------------------------------------
      IF(IFIRST.EQ.1)THEN
        IFIRST=0
        IF(NPOINT.GT.MAXPTS)THEN
          WRITE(LFNERR,5) NPOINT,MAXPTS
5         FORMAT(/,' *** SR SNGDET: TOO MANY GRID POINTS REQUESTED',/,
     1                         16X,'POINTS REQUESTED      :',I4,/,
     2                         16X,'MAXIMUM NUMBER ALLOWED:',I4,/)
          CALL EXITRC(2)
        END IF
C
C FACTORIALS
        FAC(1) = 1.D0
        DO 10 I = 2,Q+1
          FAC(I) = (I-1)*FAC(I-1)
10      CONTINUE
      END IF
C
C COMPUTE ANGLE BETWEEN VELOCITY VECTOR AND Y-AXIS OF SPACECRAFT
C (PROJECTION ONTO TERMINATOR PLANE)
C --------------------------------------------------------------
      DT=(TEND-TBEG)/(NPOINT-1)
      DMIN=+10000.
      DMAX=-10000.
C
C INITIALIZE NINT
C ---------------
      NINT=1
C
      DO 30 IPOINT=1,NPOINT
        T(IPOINT)=TBEG+(IPOINT-1)*DT
        TSEC=(T(IPOINT)-T0ARC-T0)*86400.D0
        IF(DABS(TSEC).LT.1.D-4) TSEC=0.D0
        CALL YPOL(1,Q,3,H,FAC,TSEC,YCOE,XSAT)
        CALL MOSUPN(T(IPOINT),2,DUM,DUM,DUM,XDUM,YDUM,DUM,DUM,
     1              DUMPRE,DUMNUT,XSUN,DUMMON,DUMMY,DUMVEL)
C
C Y-AXIS (ONLY DIRECTION IMPORTANT)
        CALL VPROD(XSAT,XSUN,E2)
C
C INCLINATION AND NODE IN TERMINATOR SYSTEM
        XKNTER=DATAN2(XSUN(1),-XSUN(2))
        XITER=DATAN2(DSQRT(XSUN(1)**2+XSUN(2)**2),XSUN(3))
C
C RETURN IF INCLINATION IS TOO SMALL
C ----------------------------------
        CALL DDREH(3,XKNTER,DRNS)
        CALL DDREH(1,XITER,DRIS)
C
C TRANSFORM POSITION, VELOCITY, AND Y-AXIS INTO TERMINATOR PLANE
C --------------------------------------------------------------
        CALL DMLMAV(XSAT(1),DRNS,XSAT(1))
        CALL DMLMAV(XSAT(1),DRIS,XSAT(1))
        CALL DMLMAV(XSAT(4),DRNS,XSAT(4))
        CALL DMLMAV(XSAT(4),DRIS,XSAT(4))
        IF(IPOINT.EQ.1)THEN
          CALL XYZELE(GM,0.D0,XSAT,XSAT(4),NUMSAT,A,E,XI,XKN,PER,TPER)
          TEST=XI*180/PI
          IF(TEST.LT.80.D0.OR.TEST.GT.100)GO TO 999
          IF(XSAT(3).LT.20000000.D0)GO TO 999
        END IF
C
C RETURN, IF SATELLITE IS FAR FROM CLOSEST POINT TO SUN
C -----------------------------------------------------
        CALL DMLMAV(E2,DRNS,E2)
        CALL DMLMAV(E2,DRIS,E2)
C
        PHI1=DATAN2(XSAT(5),XSAT(4))
        PHI2=DATAN2(E2(2),E2(1))
        PHI=PHI1-PHI2
        D(IPOINT)=DSIN(PHI)
30    CONTINUE
C
C DEFINE NEW INTEGRATION INTERVALS
C --------------------------------
      NINT=0
      DO 200 INT=2,NPOINT
        TEST=DABS(D(INT)-D(INT-1))
        IF(TEST.GT.DSMAX)THEN
          NINT=NINT+1
          IF(NINT.GT.MAXBND)THEN
            WRITE(LFNERR,35)MAXBND
35          FORMAT(/,' *** SR SNGDET : DIM OF ARRAY TBND TOO SMALL',
     1                ' (',I3,')',/)
            CALL EXITRC(2)
          END IF
          IF(NINT.EQ.1)THEN
            TBND(1,NINT)=TBEG
          ELSE
            TBND(1,NINT)=T(INT-1)
          END IF
          TBND(2,NINT)=T(INT)
        END IF
200   CONTINUE
      IF (NINT.EQ.0) THEN
        NINT=1
        TBND(1:2,NINT)=(/TBEG,TEND/)
      ELSE IF(TBND(2,NINT).NE.TEND)THEN
        NINT=NINT+1
          IF(NINT.GT.MAXBND)THEN
            WRITE(LFNERR,35)MAXBND
            CALL EXITRC(2)
          END IF
        TBND(1,NINT)=TBND(2,NINT-1)
        TBND(2,NINT)=TEND
      END IF
C
999   RETURN
      END SUBROUTINE

      END MODULE
