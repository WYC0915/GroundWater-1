      MODULE f_djul
      CONTAINS

C*
      FUNCTION DJUL(J1,M1,T)
CC
CC NAME       :  DJUL
CC
CC PURPOSE    :  COMPUTES THE MODIFIED JULIAN DATE (MJD)
CC               FROM YEAR,MONTH AND DAY
CC               MJD = JULIAN DATE - 2400000.5
CC
CC PARAMETERS :
CC         IN :  J1     : YEAR (E.G. 1984)                    I*4
CC               M1     : MONTH(E.G. 2 FOR FEBRUARY)          I*4
CC               T      : DAY OF MONTH                        R*8
CC        OUT :  DJUL   : MODIFIED JULIAN DATE                R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/30 17:23
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE l_basfun, ONLY: dmod
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I , J , J1, K , M , M1
C
      REAL*8 DJUL,T
      J=J1
      M=M1
      IF(M.GT.2)GO TO 1
      J=J-1
      M=M+12
1     I=J/100
      K=2-I+I/4
      DJUL=(365.25D0*J-DMOD(365.25D0*J,1.D0))-679006.D0
      DJUL=DJUL+AINT(30.6001*(M+1))+T+K
      RETURN
      END FUNCTION

      END MODULE
