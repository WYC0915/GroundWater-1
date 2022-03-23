      MODULE s_NUTNEW
      CONTAINS

C*
      SUBROUTINE NUTNEW(XMJD,NUT)
CC
CC NAME       :  NUTNEW
CC
CC PURPOSE    :  COMPUTATION OF NUTATION-MATRIX FOR EPOCH XMJD
CC               (MODIFIED JULIAN DATE)
CC               SEE "USNO CIRCULAR NO 163",1981
CC               (IAU RESOLUTIONS)
CC
CC PARAMETERS :
CC         IN :  XMJD   : EPOCH IN MODIFIED JULIAN DATE       R*8
CC        OUT :  NUT    : NUTATION - MATRIX                   R*8(3,3)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/29 09:43
CC
CC CHANGES    :  28-JAN-03 : RS: L,L1,ARCMOD REAL*4 -> REAL*8
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-FEB-07 : AG: USE PI AND 206264... FROM DEFCON
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC               04-MAY-12 : RD: USE RHO INSTEAD OF PI FROM D_CONST
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const,  ONLY: rho, ars
      USE l_basfun, ONLY: dmod
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      REAL*8    COSE, COSM, COSN, D   , DEPS, DMUE, DNUE, DPSI, EPS ,
     1          F   , O   , SINE, SINM, SINN, TU4
C
      REAL*8 XMJD,TU,NUT(3,3),X,R
      REAL*8 L,L1,ARCMOD
C      DATA PI/3.141592653589793D0/,R/360.D0/
      DATA R/360.D0/
C
C  STATEMENT FUNCTION: MODULO 360 DEG OF ANGLE X, TRANSFORMED IN RAD
      ARCMOD(X)=DMOD(X,360.D0)/RHO
C
C  TIME INTERVAL (IN JUL. CENTURIES) BETWEEN XMJD AND J2000.0
      TU =(XMJD-51544.5)/36525.D0
      TU4=TU
C
C  FUNDAMENTAL ARGUMENTS
      L  =ARCMOD(134.9629814D0+(1325.D0*R+198.867398 D0)*TU)
      L1 =ARCMOD(357.5277233D0+(  99.D0*R+359.0503400D0)*TU)
      F  =ARCMOD( 93.2719103D0+(1342.D0*R+ 82.0175381D0)*TU)
      D  =ARCMOD(297.8503631D0+(1236.D0*R+307.1114800D0)*TU)
      O  =ARCMOD(125.0445222D0-(   5.D0*R+134.1362608D0)*TU)
      EPS=84381.45-46.815*TU4-0.0006*TU4*TU4+0.00181*TU4*TU4*TU4
      EPS=EPS/ars
C
C SERIES FOR NUTATION IN LONGITUDE D-PSI (ARCSECONDS)
      DPSI= (-17.1996-0.01742*TU4)*SIN(                  O)
     *     +   0.2062             *SIN(                2*O)
     *     +   0.0046             *SIN(-2*L   +2*F    +  O)
     *     +(- 1.3187-0.00016*TU4)*SIN(        2*F-2*D+2*O)
     *     +(  0.1426-0.00034*TU4)*SIN(     L1            )
     *     +(- 0.0517+0.00012*TU4)*SIN(     L1+2*F-2*D+2*O)
     *     +(  0.0217-0.00005*TU4)*SIN(    -L1+2*F-2*D+2*O)
     *     +   0.0129             *SIN(        2*F-2*D+  O)
     *     +   0.0048             *SIN( 2*L       -2*D    )
     *     -   0.2274             *SIN(        2*F    +2*O)
     *     +   0.0712             *SIN(   L               )
     *     -   0.0386             *SIN(        2*F    +  O)
     *     -   0.0301             *SIN(   L   +2*F    +2*O)
     *     -   0.0158             *SIN(   L       -2*D    )
     *     +   0.0123             *SIN(-  L   +2*F    +2*O)
     *     +   0.0063             *SIN(            2*D    )
     *     +   0.0063             *SIN(   L           +  O)
     *     -   0.0058             *SIN(-  L           +  O)
     *     -   0.0059             *SIN(-  L   +2*F+2*D+2*O)
     *     -   0.0051             *SIN(   L   +2*F    +  O)
C
      DMUE=COS(EPS)*DPSI/ars
      DNUE=SIN(EPS)*DPSI/ars
C
C SERIES FOR NUTATION IN OBLIQUITY D-EPS (ARCSECONDS)
      DEPS= (  9.2025+0.00089*TU4)*COS(                  O)
     *     +(- 0.0895+0.00005*TU4)*COS(                2*O)
     *     -   0.0024             *COS(-2*L   +2*F    +  O)
     *     +(  0.5736-0.00031*TU4)*COS(        2*F-2*D+2*O)
     *     +   0.0054             *COS(     L1            )
     *     +(  0.0224-0.00006*TU4)*COS(     L1+2*F-2*D+2*O)
     *     -   0.0095             *COS(    -L1+2*F-2*D+2*O)
     *     -   0.0070             *COS(        2*F-2*D+  O)
     *     +(  0.0977-0.00005*TU4)*COS(        2*F    +2*O)
     *     +   0.0200             *COS(        2*F    +  O)
     *     +   0.0129             *COS(   L   +2*F    +2*O)
     *     -   0.0053             *COS(-  L   +2*F    +2*O)
C  IN RADIANS
      DEPS=DEPS/ars
C
      SINM=SIN(DMUE)
      COSM=COS(DMUE)
      SINN=SIN(DNUE)
      COSN=COS(DNUE)
      SINE=SIN(DEPS)
      COSE=COS(DEPS)
C
C  ROTATION MATRIX
      NUT(1,1)= COSM*COSN
      NUT(2,1)= COSE*SINM*COSN-SINE*SINN
      NUT(3,1)= SINE*SINM*COSN+COSE*SINN
      NUT(1,2)=-SINM
      NUT(2,2)= COSE*COSM
      NUT(3,2)= SINE*COSM
      NUT(1,3)=-COSM*SINN
      NUT(2,3)=-COSE*SINM*SINN-SINE*COSN
      NUT(3,3)=-SINE*SINM*SINN+COSE*COSN
C
      RETURN
      END SUBROUTINE

      END MODULE
