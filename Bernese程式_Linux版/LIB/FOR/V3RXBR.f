      MODULE s_V3RXBR
      CONTAINS

C*
      SUBROUTINE V3RXBR(EPH,CLOCK,EPHDAT)
CC
CC NAME       :  V3RXBR
CC
CC PURPOSE    :  TRANSFORM BERNESE VERSION 3
CC               NAVIGATION MESSAGE ARRAYS INTO RINEX ARRAY
CC
CC PARAMETERS :
CC         IN :  EPH    : ARRAY CONTAINING THE VERS.3         R*8(20)
CC                        EPHEMERIS-INFORMATION
CC               CLOCK  : ARRAY CONTAINING THE VERS.3         R*8(20)
CC                        SATELLITE CLOCK INFORMATION
CC        OUT :  EPHDAT : ARRAY CONTAINING THE                R*8(32)
CC                        RINEX NAVIGATION MESSAGE
CC
CC REMARKS    :  DEFINITIONS OF THE ARRAY ELEMENTS: SEE SR RXV3BR
CC
CC AUTHOR     :  W. GURTNER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  89/04/05
CC
CC CHANGES    :   6-JUL-93 : ??: EPHDAT(1:32)
CC                               EPHDAT(29) = CLOCK(15), TOW (SECONDS)
CC                               EPHDAT(30) = CLOCK(16), CURVE FIT
CC                                            INTERVAL (H) (SWITCHED OFF
CC                                            YET)
CC               23-JUN-05 : MM: IMPLICIT NONE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8      CLOCK(20),EPH(20),EPHDAT(32)
C
C COPY EPHEMERIS AND CLOCK CORRECTIONS TO RINEX ARRAY
C ---------------------------------------------------
      EPHDAT(1)  = CLOCK(11)
      EPHDAT(2)  = CLOCK(14)
      EPHDAT(3)  = CLOCK(13)
      EPHDAT(4)  = CLOCK(12)
      EPHDAT(5)  = EPH(17)
      EPHDAT(6)  = EPH(13)
      EPHDAT(7)  = EPH(9)
      EPHDAT(8)  = EPH(8)
      EPHDAT(9)  = EPH(12)
      EPHDAT(10) = EPH(4)
      EPHDAT(11) = EPH(11)
      EPHDAT(12) = DSQRT(EPH(3))
      EPHDAT(13) = EPH(2)
      EPHDAT(14) = EPH(16)
      EPHDAT(15) = EPH(6)
      EPHDAT(16) = EPH(15)
      EPHDAT(17) = EPH(5)
      EPHDAT(18) = EPH(14)
      EPHDAT(19) = EPH(7)
      EPHDAT(20) = EPH(10)
      EPHDAT(21) = EPH(18)
      EPHDAT(22) = CLOCK(2)
      EPHDAT(23) = EPH(1)
      EPHDAT(24) = CLOCK(6)
      EPHDAT(25) = CLOCK(3)
      EPHDAT(26) = CLOCK(4)
      EPHDAT(27) = CLOCK(9)
      EPHDAT(28) = CLOCK(10)
      EPHDAT(29) = CLOCK(15)
      EPHDAT(30) = CLOCK(16)
      EPHDAT(30) = 0.D0
      EPHDAT(31) = 0.D0
      EPHDAT(32) = 0.D0
C
      RETURN
      END SUBROUTINE

      END MODULE
