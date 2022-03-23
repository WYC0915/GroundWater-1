      MODULE f_pvdeg2
      CONTAINS

C*
      FUNCTION PVDEG2(T,T2,VAL0,VAL1,VAL2)
CC
CC NAME       :  PVDEG2
CC
CC PURPOSE    :  QUADRATIC INTERPOLATION FOR TIME ARGUMENT T
CC               IN THE TABLE VAL0=VAL(-1),VAL1=VAL(0),VAL2=VAL(1)
CC               T2=T**2
CC
CC PARAMETERS :
CC        IN  :  T      : TIME ARGUMENT                             R*8
CC               T2     : T2=T**2                                   R*8
CC               VAL0   : VAL(-1)                                   R*8
CC               VAL1   : VAL(0)                                    R*8
CC               VAL2   : VAL(1)                                    R*8
CC
CC REMARKS    :
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.5  (MAY 94)
CC
CC CREATED    :  94/10/08
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      REAL*8    A1    , A2    , PVDEG2, T     , T2    , VAL0  , VAL1  ,
     1          VAL2
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C FIRST AND SECOND DERIVATIVE OF FUNCTION
C ---------------------------------------
      A1=(VAL2-VAL0)/2
      A2=(VAL2-2*VAL1+VAL0)/2
      PVDEG2=VAL1+A1*T+A2*T2
C
      RETURN
      END FUNCTION

      END MODULE
