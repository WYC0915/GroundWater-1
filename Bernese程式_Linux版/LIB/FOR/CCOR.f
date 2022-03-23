      MODULE f_CCOR
      CONTAINS

      FUNCTION  CCOR (ALT, R,H1,ZH)
C        CHEMISTRY/DISSOCIATION CORRECTION FOR MSIS MODELS
C        ALT - altitude
C        R - target ratio
C        H1 - transition scale length
C        ZH - altitude of 1/2 R
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
C
      SAVE
      E=(ALT-ZH)/H1
      IF(E.GT.70.D0) GO TO 20
      IF(E.LT.-70.D0) GO TO 10
        EX=DEXP(E)
        CCOR=R/(1.D0+EX)
        GO TO 50
   10   CCOR=R
        GO TO 50
   20   CCOR=0.D0
        GO TO 50
   50 CONTINUE
      CCOR=DEXP(CCOR)
       RETURN
      END FUNCTION

      END MODULE
