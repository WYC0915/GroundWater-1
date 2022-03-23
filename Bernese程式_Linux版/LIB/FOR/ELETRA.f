      MODULE s_ELETRA
      CONTAINS

C*
      SUBROUTINE ELETRA(INDEX,A,E,PER,TOSC,TPER,U0)
CC
CC NAME       :  ELETRA
CC
CC PURPOSE    : INDEX=1 : A, E, PER, TOSC, TPER --> U0
CC                   =2 : A, E, PER, TOSC, U0   --> TPER
CC
CC PARAMETERS :
CC        IN  : INDEX   : TYPE OF CONVERSION                        I*4
CC              A       : SEMIMAJOR AXIS                            R*8
CC              E       : ECCENTRICITY                              R*8
CC              PER     : ARGUMENT OF PERIGEE                       R*8
CC              TOSC    : OSCULATION TIME (SEC)                     R*8
CC     IN/OUT : TPER    : PERIGEE PASSING TIME                      R*8
CC              U0      : ARGUMENT OF LATITUDE AT TIME TOSC         R*8
CC
CC REMARKS    :
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.5  (MAY 94)
CC
CC CREATED    :  94/05/11
CC
CC CHANGES    :  16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const, ONLY: GM, PI
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 INDEX
C
      REAL*8    A    , E    , EX   , EX1  , PER  , TOSC , TPER , U0   ,
     1          V0   , XM   , XN
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C MEAN MOTION
C -----------
      XN=DSQRT(GM/A**3)
C
C A, E, PER, TOSC, TPER --> U0
C ----------------------------
      IF(INDEX.EQ.1)THEN
        XM=XN*(TOSC-TPER)
        EX1=XM
20        EX=EX1+(XM+E*DSIN(EX1)-EX1)/(1-E*DCOS(EX1))
          IF(DABS(EX-EX1).LT.1.D-12)GO TO 30
          EX1=EX
          GO TO 20
30      CONTINUE
        V0=2*DATAN(DSQRT((1+E)/(1-E))*DTAN(EX/2))
        U0=PER+V0
        IF(U0.LT.0.D0)U0=U0+2*PI
        IF(U0.LT.0.D0)U0=U0+2*PI
        IF(U0.GT.2*PI)U0=U0-2*PI
      ELSE
C
C A, E, PER, TOSC, U0   --> TPER
C ------------------------------
        V0=U0-PER
        EX=2*DATAN(DSQRT((1-E)/(1+E))*DTAN(V0/2))
        XM=EX-E*DSIN(EX)
        TPER=TOSC-XM/XN
      END IF
999   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
