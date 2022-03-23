      MODULE f_DNET
      CONTAINS

      FUNCTION DNET(DD,DM,ZHM,XMM,XM)
C       TURBOPAUSE CORRECTION FOR MSIS MODELS
C         Root mean density
C       8/20/80
C          DD - diffusive density
C          DM - full mixed density
C          ZHM - transition scale length
C          XMM - full mixed molecular weight
C          XM  - species molecular weight
C          DNET - combined density
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
C
      SAVE
      A=ZHM/(XMM-XM)
      IF(DM.GT.0.D0.AND.DD.GT.0.D0) GOTO 5
        WRITE(6,*) 'DNET LOG ERROR',DM,DD,XM
        IF(DD.EQ.0.D0.AND.DM.EQ.0.D0) DD=1.D0
        IF(DM.EQ.0.D0) GOTO 10
        IF(DD.EQ.0.D0) GOTO 20
    5 CONTINUE
      YLOG=A*DLOG(DM/DD)
      IF(YLOG.LT.-10.D0) GO TO 10
      IF(YLOG.GT.10.D0)  GO TO 20
        DNET=DD*(1.D0+DEXP(YLOG))**(1/A)
        GO TO 50
   10 CONTINUE
        DNET=DD
        GO TO 50
   20 CONTINUE
        DNET=DM
        GO TO 50
   50 CONTINUE
      RETURN
      END FUNCTION

      END MODULE
