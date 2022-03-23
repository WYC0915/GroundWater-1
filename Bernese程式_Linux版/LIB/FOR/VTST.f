      MODULE f_vtst
      CONTAINS

      FUNCTION VTST(IYD,SEC,GLAT,GLONG,STL,F107A,F107,AP,IC)
C       Test if geophysical variables or switches changed and save
C       Return 0 if unchanged and 1 if changed
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
C
      DIMENSION AP(7),IYDL(2),SECL(2),GLATL(2),GLL(2),STLL(2)
      DIMENSION FAL(2),FL(2),APL(7,2),SWL(25,2),SWCL(25,2)
      COMMON/CSW/SW(25),ISW,SWC(25)
      SAVE
      DATA IYDL/2*-999/,SECL/2*-999.D0/,GLATL/2*-999.D0/,GLL/2*-999.D0/
      DATA STLL/2*-999.D0/,FAL/2*-999.D0/,FL/2*-999.D0/,APL/14*-999.D0/
      DATA SWL/50*-999.D0/,SWCL/50*-999.D0/
      VTST=0
      IF(IYD.NE.IYDL(IC)) GOTO 10
      IF(SEC.NE.SECL(IC)) GOTO 10
      IF(GLAT.NE.GLATL(IC)) GOTO 10
      IF(GLONG.NE.GLL(IC)) GOTO 10
      IF(STL.NE.STLL(IC)) GOTO 10
      IF(F107A.NE.FAL(IC)) GOTO 10
      IF(F107.NE.FL(IC)) GOTO 10
      DO 5 I=1,7
        IF(AP(I).NE.APL(I,IC)) GOTO 10
    5 CONTINUE
      DO 7 I=1,25
        IF(SW(I).NE.SWL(I,IC)) GOTO 10
        IF(SWC(I).NE.SWCL(I,IC)) GOTO 10
    7 CONTINUE
      GOTO 20
10    CONTINUE
      VTST=1
      IYDL(IC)=IYD
      SECL(IC)=SEC
      GLATL(IC)=GLAT
      GLL(IC)=GLONG
      STLL(IC)=STL
      FAL(IC)=F107A
      FL(IC)=F107
      DO 15 I=1,7
        APL(I,IC)=AP(I)
15    CONTINUE
      DO 16 I=1,25
        SWL(I,IC)=SW(I)
        SWCL(I,IC)=SWC(I)
16    CONTINUE
20    CONTINUE
      RETURN
      END FUNCTION

      END MODULE
