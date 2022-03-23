      MODULE s_RESORB
      CONTAINS

C*
      SUBROUTINE RESORB(ROLD,X,V,RNEW)
CC
CC NAME       :  RESORB
CC
CC PURPOSE    :  COMPUTATION OF RESIDUALS IN ORBITAL SYSTEM (AXIS 1
CC               IN RADIAL DIRECTION, AXIS 2 ALONG TRACK, AXIS 3
CC               PERPENDICULAR TO OSCULATING ORBITAL PLANE
CC
CC PARAMETERS :
CC         IN :  ROLD(I),I=1,2,3: RESIDUALS IN SYSTEM 1950.0      R*8
CC                        (OR IN THE SYSTEM CHOSEN
CC                        FOR NUMERICAL INTEGRATION)
CC               X(I),I=1,2,3: SATELLITE POSITION IN SYSTEM 1950  R*8
CC               V(I),I=1,2,3: SATELLITE VELOCITY IN SYSTEM 1950  R*8
CC        OUT :  RNEW(I),I=1,2,3: RESIDUALS IN RADIAL DIRECTION,  R*8
CC                        ALONG TRACK, OUT OF PLANE
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/30 08:11
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE s_sprod
      USE s_vprod
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      REAL*8    CSATEL, RDUMMY, RSATEL, SCALC , SCALR , SCALV , VSATEL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 X(3),V(3),ROLD(3),RNEW(3),CROSS(3)
      CALL VPROD(X,V,CROSS)
      CALL SPROD(ROLD,X,SCALR,RDUMMY,RSATEL)
      CALL SPROD(ROLD,V,SCALV,RDUMMY,VSATEL)
      CALL SPROD(ROLD,CROSS,SCALC,RDUMMY,CSATEL)
      RNEW(1)=SCALR/RSATEL
      RNEW(2)=SCALV/VSATEL
      RNEW(3)=SCALC/CSATEL
      RETURN
      END SUBROUTINE

      END MODULE
