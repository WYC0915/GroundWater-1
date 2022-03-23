      MODULE f_plgndr
      CONTAINS

C*
      FUNCTION PLGNDR(NN,MM,XX)
CC
CC NAME       :  PLGNDR
CC
CC PURPOSE    :  COMPUTE THE ASSOCIATED LEGENDRE POLYNOMIAL
CC                           MM
CC                          P   (XX)
CC                           NN
CC PARAMETERS :
CC         IN :  NN ... DEGREE                                I*4
CC               MM ... ORDER                                 I*4
CC               XX ... ARGUMENT                              R*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  L.MERVART
CC
CC VERSION    :  3.5
CC
CC CREATED    :  04-MAY-94
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , LL    , MM    , NN
C
      REAL*8    FACT  , PLGNDR, PLL   , PMM   , PMMP1 , SOMX2 , XX
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C CHECK THE INPUT PARAMETERS
C --------------------------
      IF (MM.LT.0 .OR. MM.GT.NN .OR. DABS(XX).GT.1.D0) THEN
        WRITE(LFNERR,1000) NN,MM,XX
1000    FORMAT(/,' *** SR PLGNDR : BAD ARGUMENTS',/,
     1                     16X,'N = ',  I6,/,
     2                     16X,'M = ',  I6,/,
     3                     16X,'X = ',F6.3,/)
        CALL EXITRC(2)
      END IF
C
      PMM=1.D0
      IF (MM.GT.0) THEN
        SOMX2=DSQRT((1.D0-XX)*(1.D0+XX))
        FACT=1.D0
        DO 100 I=1,MM
CCC          PMM=-PMM*FACT*SOMX2
          PMM=PMM*FACT*SOMX2
          FACT=FACT+2.D0
100     CONTINUE
      END IF
C
      IF (NN.EQ.MM) THEN
        PLGNDR=PMM
      ELSE
        PMMP1=XX*(2*MM+1)*PMM
        IF (NN.EQ.MM+1) THEN
          PLGNDR=PMMP1
        ELSE
          DO 200 LL=MM+2,NN
            PLL=(XX*(2*LL-1)*PMMP1-(LL+MM-1)*PMM)/(LL-MM)
            PMM=PMMP1
            PMMP1=PLL
200       CONTINUE
          PLGNDR=PLL
        END IF
      END IF
C
      RETURN
      END FUNCTION

      END MODULE
