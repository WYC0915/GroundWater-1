      MODULE s_GMSRAD
      CONTAINS

C*
      SUBROUTINE GMSRAD(I,V,IG,M,S,ARC)
CC
CC NAME       :  GMSRAD
CC
CC PURPOSE    :  TRANSFORMS AN ANGLE INTO RADIAN
CC               I=0 : ANGLE ASSUMED IN HOURS
CC               I=1 : ANGLE ASSUMED IN SEXAGESIMAL DEGREES
CC
CC PARAMETERS :
CC         IN :  I      : TYPE SELECTION                      I*4
CC               V      : SIGN (EVERY SIGN, WITH THE EXCEP-   CH*1
CC                        TION OF "-" IS ASSUMED TO BE A PLUS
CC                        SIGN
CC               IG     : DEGREES (HOURS)                     I*4
CC               M      : MINUTES                             I*4
CC               S      : SECONDS                             R*8
CC               ARC    : ARC IN RADIANS                      R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W.GURTNER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/02 11:54
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-FEB-07 : AG: Use rho from defcon
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const, ONLY: rho
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I  , IG , M
C
      REAL*8    ARC, FAK, S
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*1 V
C      PI=3.141592653589793D0
C      FAK=180/PI
      FAK=rho
      IF(I.EQ.0)FAK=FAK/15
      ARC=(IG+M/60.D0+S/3600.D0)/FAK
      IF(V.EQ.'-')ARC=-ARC
      RETURN
      END SUBROUTINE

      END MODULE
