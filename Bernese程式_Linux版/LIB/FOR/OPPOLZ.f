      MODULE s_OPPOLZ
      CONTAINS

C*
      SUBROUTINE OPPOLZ(TREQ,DX,DY)
CC
CC NAME       :  OPPOLZ
CC
CC PURPOSE    :  COMPUTATION OF THE OPPOLZER NUTATION DX,DY
CC
CC PARAMETERS :
CC         IN :  TREQ   : TIME OF REQUEST (MJD)               R*8
CC        OUT :  DX     : OPP. NUT. IN X (IN RAD)             R*8
CC               DY     : OPP. NUT. IN Y (IN RAD)             R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.FANKHAUSER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  08-JUN-92
CC
CC CHANGES    :  10-JAN-94 : SF: IMPLICIT NONE REPLACED (SPECIAL UNIX
CC                                                       COMPILER)
CC               28-JAN-03 : RS: L,S,F,D,O,EPS,ARCMOD REAL*4 -> REAL*8
CC               11-JUN-03 : HU: USE GSTIME
CC               11-JUN-03 : HU: DESACTIVATE USE OF GMST2000
CC               06-AUG-03 : HU: NEW CALL FOR GSTIME
CC               23-JUN-05 : MM: IMPLICIT NONE
CC               07-JUL-05 : HB: USE T_EPOCH FOR GSTIME
CC               28-FEB-07 : AG: USE PI AND 206264... FROM DEFCON
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC               04-MAY-12 : RD: USE WHO FROM D_CONST
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_epoch,  ONLY : t_epoch, OPERATOR(.realToEpoch.)
      USE d_const,  ONLY: pi, ars, rho
      USE l_basfun, ONLY: dmod
      USE f_gstime
      IMPLICIT NONE
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C GLOBAL DECLARATION
C ------------------
      REAL*8  TREQ,DX,DY
C
C INTERNAL DECLARATION
C --------------------
      REAL*8  T,TT,X,DPSI,DPSI1,DTHET,DTHET1,THETA,CA
      REAL*8  L,S,F,D,O,EPS
      REAL*8  ARCMOD

      TYPE(t_epoch) :: TTIM
C
C DEFINITION OF AN INTERNAL FUNCTION
C ----------------------------------
      ARCMOD(X)=DMOD(X,360.D0)/RHO
C
C      PI=3.141592653589793D0
      CA=1.003285
      T=TREQ-51544.5D0
      TT=(TREQ-15019.5D0)/36525.D0
C
C CALCULATE THE MEAN OBLIQUITY OF THE ECLIPTIC AND GMST
      EPS=23.452294D0-.0130125D0*TT-1.64D-6*TT**2+5.03D-7*TT**3
      EPS=EPS/180.0*PI
      TTIM = .realToEpoch.TREQ
      THETA=GSTIME(2,TTIM,TTIM,0D0,0D0)
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
      DPSI  =0.D0
      DPSI1 =0.D0
      DTHET =0.D0
      DTHET1=0.D0
C
      DPSI=DPSI
     1      +1733*SIN(         2*F    +2*O)
     2      + 758*SIN(         2*F-2*D+2*O)
     3      + 355*SIN(         2*F    +  O)
     4      + 344*SIN(   L    +2*F    +2*O)
     5      - 338*SIN(                   O)
     6      -  70*SIN(   L    +2*F    +  O)
     7      +  65*SIN(-  L    +2*F+2*D+2*O)
     8      +  57*SIN(         2*F+2*D+2*O)
     9      -  47*SIN(-  L    +2*F    +2*O)
C
      DPSI1=DPSI1
     1      +  47*SIN( 2*L    +2*F    +2*O)
     1      +  45*SIN(       S+2*F-2*D+2*O)
     2      -  29*SIN(   L            +  O)
     3      -  27*SIN(-  L            +  O)
     4      -  14*SIN(   L    +2*F+2*D+2*O)
     5      +  13*SIN(-  L    +2*F+2*D+  O)
     6      +  12*SIN(         2*F+2*D+  O)
     7      +  12*SIN(   L    +2*F-2*D+2*O)
C
      DTHET=DTHET
     *      + 868*1.D0
     1      + 640*COS(         2*F    +2*O)
     2      + 277*COS(         2*F-2*D+2*O)
     3      + 128*COS(   L    +2*F    +2*O)
     4      + 110*COS(         2*F    +  O)
     5      - 100*COS(                   O)
     6      -  97*COS(   L                )
     7      +  24*COS(-  L    +2*F+2*D+2*O)
     8      +  22*COS(   L    +2*F    +  O)
     9      +  21*COS(         2*F+2*D+2*O)
     1      -  19*COS(   L        -2*D    )
C
      DTHET1=DTHET1
     1      +  18*COS( 2*L    +2*F    +2*O)
     2      -  17*COS(-  L    +2*F    +2*O)
     3      -  16*COS(             2*D    )
     4      +  16*COS(       S+2*F-2*D+2*O)
     5      -  14*COS(       S            )
     6      -   9*COS(   L            +  O)
     7      -   8*COS(-  L            +  O)
     8      -   5*COS(   L    +2*F-2*D+2*O)
     9      +   5*COS(   L    +2*F+2*D+2*O)
     1      +   4*COS(-  L    +2*F+2*D+  O)
     2      +   4*COS(         2*F+2*D+  O)
C
      DPSI=DPSI+DPSI1
      DTHET=DTHET+DTHET1
C
      DX=-CA*(DTHET*SIN(THETA)-DPSI*SIN(EPS)*COS(THETA))
      DY= CA*(DTHET*COS(THETA)+DPSI*SIN(EPS)*SIN(THETA))
      DX=DX/ars/100000.D0
      DY=DY/ars/100000.D0
C
      RETURN
      END SUBROUTINE

      END MODULE
