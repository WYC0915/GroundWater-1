      MODULE s_LODRED
      CONTAINS

C*
      SUBROUTINE LODRED(TREQ,KORR)
CC
CC NAME       :  LODRED
CC
CC PURPOSE    :  COMPUTATION OF THE CORRECTION OF DURATION OF DAY
CC               (= SHORT PERIODIC TERMS UP TO 35 DAYS)
CC
CC
CC PARAMETERS :
CC         IN :  TREQ   : TIME OF REQUEST (MJD)               R*8
CC        OUT :  KORR   : D-DR (IN UNITS OF MSEC)             R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  R.WEBER
CC
CC VERSION    :  3.5  (JAN 94)
CC
CC CREATED    :  07-JAN-94
CC
CC CHANGES    :  28-JAN-03 : RS: L,S,F,D,O,ARCMOD REAL*4 -> REAL*8
CC               23-JUN-05 : MM: IMPLICIT NONE
CC               28-FEB-07 : AG: USE PI FROM DEFCON
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC               04-MAY-12 : RD: USE RHO INSTEAD OF PI FROM D_CONST
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const,  ONLY: rho
      USE l_basfun, ONLY: dmod
      IMPLICIT NONE
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4  (I-N)
C
C GLOBAL DECLARATION
C ------------------
      REAL*8  KORR,TREQ
C
C INTERNAL DECLARATION
C --------------------
      REAL*8  T,X,KOR1,KOR2,KOR3,KOR4
      REAL*8  L,S,F,D,O
      REAL*8  ARCMOD
C
C DEFINITION OF AN INTERNAL FUNCTION
C ----------------------------------
      ARCMOD(X)=DMOD(X,360.D0)/RHO
C
C      PI=3.141592653589793D0
      T=TREQ-51544.5D0
C
C CALCULATE THE ARGUMENTS
C -----------------------
      L=ARCMOD(134.96D0+13.064993*T)
      S=ARCMOD(357.53D0+ 0.985600*T)
      F=ARCMOD( 93.27D0+13.229350*T)
      D=ARCMOD(297.85D0+12.190749*T)
      O=ARCMOD(125.04D0- 0.052954*T)
C
C L= MEAN ANOMALY OF THE MOON
C S= MEAN ANOMALY OF THE SUN
C F= Z-O: Z: MEAN LONGITUDE OF THE MOON
C D= MEAN ELONGATION OF THE MOON FROM THE SUN
C O= MEAN LONGITUDE OF THE ASCENDING NODE
C
C CALCULATION OF THE SHORT PERIODIC TERMS
C ---------------------------------------
      KOR1=0.D0
      KOR2=0.D0
      KOR3=0.D0
      KOR4=0.D0
C
      KOR1=KOR1
     1     +18.84*COS(   L                )
     2     +35.68*COS(         2*F    +2*O)
     3     +14.79*COS(         2*F    +  O)
     4      +3.60*COS(-  L        +2*D    )
     5      +6.83*COS(   L    +2*F    +2*O)
     6      +3.12*COS(             2*D    )
     7      -1.24*COS(   L            +  O)
     8      -1.22*COS(   L            -  O)
     9      -1.01*COS(-  L    +2*F    +2*O)
      KOR2=KOR2
     1      +2.83*COS(   L    +2*F    +  O)
     2      +1.54*COS( 2*L                )
     3      +1.38*COS(         2*F        )
     4      +1.30*COS(-  L    +2*F+2*D+2*O)
     5      -0.41*COS(-  L    +2*F    +  O)
     6      -0.26*COS(-  L        +2*D+  O)
     7      +1.09*COS(         2*F+2*D+2*O)
     8      -0.23*COS(-  L        +2*D-  O)
     9      +0.90*COS( 2*L    +2*F    +2*O)
     1      -0.26*COS(   L    +2*F-2*D+2*O)
      KOR3=KOR3
     1      +0.15*COS(-  L-  S    +2*D    )
     2      +0.54*COS(-  L    +2*F+2*D+  O)
     3      +0.50*COS(   L        +2*D    )
     4      +0.12*COS(   L-  S            )
     5      +0.45*COS(        +2*F+2*D+  O)
     6      -0.20*COS(             2*D-  O)
     7      +0.22*COS(             2*D+  O)
     8      +0.21*COS(    -  S    +2*D    )
     9      -0.13*COS(   L    +2*F-2*D+  O)
     1      -0.11*COS(-  L    +2*F        )
      KOR4=KOR4
     1      -0.10*COS(               D    )
     2      +0.37*COS( 2*L    +2*F    +  O)
     3      +0.27*COS(   L    +2*F        )
     4      -0.10*COS(   L+  S            )
     5      -0.12*COS(       S+2*F    +2*O)
     6      +0.26*COS(   L    +2*F+2*D+2*O)
     7      +0.12*COS( 3*L                )
     8      -0.11*COS( 2*L    +2*F-2*D+2*O)
     9      -0.10*COS( 2*L            -  O)
     1      -0.08*COS( 2*L            +  O)
     2      +0.11*COS(    -  S+2*F    +2*O)
     3      -0.03*COS(   L    -2*F+2*D-  O)
C
      KORR=(KOR1+KOR2+KOR3+KOR4)/100.D0
C
      RETURN
      END SUBROUTINE

      END MODULE
