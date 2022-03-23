      MODULE s_OFFSET
      CONTAINS

C*
      SUBROUTINE OFFSET(X,Y,L,N,YMEAN,YRMS)
CC
CC NAME       :  OFFSET
CC
CC PURPOSE    :  ESTIMATE THE MEAN AND RMS OF A TIME SERIES
CC
CC PARAMETERS :
CC         IN :  X      : VECTOR WITH TIME                    R*8(N)
CC               Y      : VECTOR WITH OBSERVATIONS            R*8(N)
CC               L      : BIT 2=1: VALUE # I IS NOT USED      CH*1(N)
CC                                 FOR APPROXIMATION
CC                        BIT 0=1: VALUE # I IS NOT USED AT ALL
CC               N      : # OF VALUES (=DIMENSION OF X,Y,V)   I*4
CC        OUT :  YMEAN  : MEAN OF OBSERVATIONS                R*8
CC            :  YRMS   : RMS OF OBSERVATIONS                 R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  T.A.SPRINGER
CC
CC VERSION    :  4.1 (JUL 96)
CC
CC CREATED    :  25-JUL-96
CC
CC CHANGES    :  25-JUL-96 : TS: CREATED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               19-JUL-10 : TAB CHARACTERS REMOVED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE f_tstflg
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I    , N    , NTOT
C
      REAL*8    SUM  , SUM2 , YMEAN, YRMS
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8      X(*),Y(*)
C
      CHARACTER*1 L(*)
C
C INITIALIZE
C ----------
      SUM =0.D0
      SUM2=0.D0
      NTOT=0
C
C GET THE MEAN
C ------------
      DO 10 I=1,N
        IF(TSTFLG(L(I),0).OR.TSTFLG(L(I),2)) GOTO 10
        SUM=SUM+Y(I)
        NTOT=NTOT+1
10    CONTINUE
      YMEAN = SUM/(NTOT*1D0)
C
C GET THE RMS
C -----------
      SUM2=0.D0
      NTOT=0
      DO 20 I=1,N
        IF(TSTFLG(L(I),0).OR.TSTFLG(L(I),2)) GOTO 20
        SUM2=SUM2+(Y(I)-YMEAN)**2
        NTOT=NTOT+1
20    CONTINUE
      YRMS  = DSQRT(SUM2/(1D0*(NTOT-1)))
C
      RETURN
      END SUBROUTINE

      END MODULE
