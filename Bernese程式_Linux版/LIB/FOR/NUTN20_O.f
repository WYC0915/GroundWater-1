      MODULE s_NUTN20_O
      CONTAINS

C*
      SUBROUTINE NUTN20(XTDB,NUT)
CC
CC NAME       :  NUTN20
CC
CC PURPOSE    :  COMPUTATION OF NUTATION-MATRIX FOR EPOCH XTDB
CC               (JUL. DATE IN BARYCENTRIC DYNAMICAL TIME)
CC               SEE "USNO CIRCULAR NO 163",1981
CC               (IAU RESOLUTIONS) EDITED BY KAPLAN
CC
CC            -> OLD VARIABLE XMOD IS POSSIBLE TOO, BECAUSE
CC               DIFF. < 1 MIN CAUSES DIFFERENCIES < 0.1 MAS
CC
CC PARAMETERS :
CC         IN :  XTDB   : EPOCH IN BARYCENTRIC DYNAMICAL TIME  R*8
CC        OUT :  NUT    : NUTATION - MATRIX                    R*8(3,3)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  E. BROCKMANN
CC
CC VERSION    :  3.3
CC
CC CREATED    :  92/05/05 09:20
CC
CC CHANGES    :  25-AUG-92 : ??: USE CORRECT SIN AND COS FUNCTIONS TO
CC                               COMPUTE ROTATION MATRIX
CC               20-OCT-92 : ??: USE CORRECT DEPS AND DPSI INSTEAD OF
CC                               DMUE AND DNUE.
CC               29-OCT-93 : SF: UPDATE HEADER INFORMATION
CC               19-APR-94 : RW: CPO-MODEL INCLUDED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               29-JAN-07 : AG: EXTRAPOLATION TOLERANCE FOR LITPOL ADDED
CC               28-FEB-07 : AG: USE PI FROM DEFCON
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const,  ONLY: pi
      USE l_basfun, ONLY: dmod
      USE s_litpol
      USE s_getcpo
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , J
C
      REAL*8    COSDP , COSEM , COSET,DDEPS(2), DDPSI(2), DEPS, DPSI  ,
     1          EPS   , EPSTRU, PHI   , R     , ROH   , SINDP ,
     2          SINEM , SINET , TU    , TU4   , XTDB
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 NUT(3,3),ARG1(9,22),ARG2(7,84),ALP(5)
      REAL*8 TT(2),TTEPS(2),TTPSI(2)
C
C DATA
C ----
C      DATA PI/3.141592653589793D0/,R/1296000.D0/
      DATA R/1296000.D0/
C
C 22 COEFFICIENTS FOR THE STONGEST NUTATIONPERIODS (TIMEDEPENDENT)
C
C UNIT: ARCSEC/10000
C
      DATA ((ARG1(I,J),I=1,9),J=1,11)/
     1  0.d0, 0.d0, 0.d0,0.d0,1.d0,-171996.d0,-174.2d0,92025.d0,8.9d0,
     2  0.d0, 0.d0, 0.d0, 0.d0, 2.d0, 2062.d0  ,.2d0 , -895.d0 , .5d0,
     3 -2.d0, 0.d0, 2.d0, 0.d0, 1.d0,   46.d0  ,.0d0 ,  -24.d0 , .0d0,
     9  0.d0, 0.d0, 2.d0,-2.d0, 2.d0, -13187.d0,-1.6d0,5736.d0,-3.1d0,
C  10
     - 0.d0, 1.d0, 0.d0, 0.d0, 0.d0,1426.d0 ,-3.4d0,   54.d0 ,-0.1d0,
     1 0.d0, 1.d0, 2.d0,-2.d0, 2.d0,-517.d0 , 1.2d0,  224.d0 ,-0.6d0,
     2 0.d0,-1.d0, 2.d0,-2.d0, 2.d0, 217.d0 ,-0.5d0,  -95.d0 , 0.3d0,
     3 0.d0, 0.d0, 2.d0,-2.d0, 1.d0, 129.d0 , 0.1d0,  -70.d0 , 0.0d0,
     4 2.d0, 0.d0, 0.d0,-2.d0, 0.d0,  48.d0 , 0.0d0,    1.d0 , 0.0d0,
     6 0.d0, 2.d0, 0.d0, 0.d0, 0.d0,  17.d0 ,-0.1d0,    0.d0 , 0.0d0,
     8 0.d0, 2.d0, 2.d0,-2.d0, 2.d0, -16.d0 , 0.1d0,    7.d0 , 0.0d0/
C  30
      DATA ((ARG1(I,J),I=1,9),J=12,22)/
     1  0.d0, 0.d0, 2.d0, 0.d0, 2.d0,-2274.d0 ,-0.2d0,977.d0 ,-0.5d0,
     2  1.d0, 0.d0, 0.d0, 0.d0, 0.d0,  712.d0 , 0.1d0, -7.d0 , 0.0d0,
     3  0.d0, 0.d0, 2.d0, 0.d0, 1.d0, -386.d0 ,-0.4d0,200.d0 , 0.0d0,
     4  1.d0, 0.d0, 2.d0, 0.d0, 2.d0, -301.d0 , 0.0d0,129.d0 ,-0.1d0,
     5  1.d0, 0.d0, 0.d0,-2.d0, 0.d0, -158.d0 , 0.0d0, -1.d0 , 0.0d0,
     6 -1.d0, 0.d0, 2.d0, 0.d0, 2.d0,  123.d0 , 0.0d0,-53.d0 , 0.0d0,
     7  0.d0, 0.d0, 0.d0, 2.d0, 0.d0,   63.d0 , 0.0d0, -2.d0 , 0.0d0,
     8  1.d0, 0.d0, 0.d0, 0.d0, 1.d0,   63.d0 , 0.1d0,-33.d0 , 0.0d0,
     9 -1.d0, 0.d0, 0.d0, 0.d0, 1.d0,  -58.d0 ,-0.1d0, 32.d0 , 0.0d0,
C  40
     - -1.d0, 0.d0, 2.d0, 2.d0, 2.d0,  -59.d0 , 0.0d0, 26.d0 , 0.0d0,
     1  1.d0, 0.d0, 2.d0, 0.d0, 1.d0,  -51.d0 , 0.0d0, 27.d0 , 0.0d0/
C
C 84 ADDITIONAL AMPLITUDES WITH NO TIMEDEPENDENT COEFFICIENTS
C
      DATA ((ARG2(I,J),I=1,7),J=1,19)/
     4           2.d0, 0.d0,-2.d0, 0.d0, 0.d0,  11.d0,   0.d0,
     5          -2.d0, 0.d0, 2.d0, 0.d0, 2.d0,  -3.d0,   1.d0,
     6           1.d0,-1.d0, 0.d0,-1.d0, 0.d0,  -3.d0,   0.d0,
     7           0.d0,-2.d0, 2.d0,-2.d0, 1.d0,  -2.d0,   1.d0,
     8           2.d0, 0.d0,-2.d0, 0.d0, 1.d0,   1.d0,   0.d0,
C  10
     5           0.d0, 0.d0, 2.d0,-2.d0, 0.d0, -22.d0,   0.d0,
     7           0.d0, 1.d0, 0.d0, 0.d0, 1.d0, -15.d0,   9.d0,
     9           0.d0,-1.d0, 0.d0, 0.d0, 1.d0, -12.d0,   6.d0,
C  20
     -          -2.d0, 0.d0, 0.d0, 2.d0, 1.d0,  -6.d0,   3.d0,
     1           0.d0,-1.d0, 2.d0,-2.d0, 1.d0,  -5.d0,   3.d0,
     2           2.d0, 0.d0, 0.d0,-2.d0, 1.d0,   4.d0,  -2.d0,
     3           0.d0, 1.d0, 2.d0,-2.d0, 1.d0,   4.d0,  -2.d0,
     4           1.d0, 0.d0, 0.d0,-1.d0, 0.d0,  -4.d0,   0.d0,
     5           2.d0, 1.d0, 0.d0,-2.d0, 0.d0,   1.d0,   0.d0,
     6           0.d0, 0.d0,-2.d0, 2.d0, 1.d0,   1.d0,   0.d0,
     7           0.d0, 1.d0,-2.d0, 2.d0, 0.d0,  -1.d0,   0.d0,
     8           0.d0, 1.d0, 0.d0, 0.d0, 2.d0,   1.d0,   0.d0,
     9          -1.d0, 0.d0, 0.d0, 1.d0, 1.d0,   1.d0,   0.d0,
C  30
     -           0.d0, 1.d0, 2.d0,-2.d0, 0.d0,  -1.d0,   0.d0 /
      DATA ((ARG2(I,J),I=1,7),J=20,37)/
C  40
     2           0.d0, 0.d0, 2.d0, 2.d0, 2.d0, -38.d0,  16.d0,
     3           2.d0, 0.d0, 0.d0, 0.d0, 0.d0,  29.d0,  -1.d0,
     4           1.d0, 0.d0, 2.d0,-2.d0, 2.d0,  29.d0, -12.d0,
     5           2.d0, 0.d0, 2.d0, 0.d0, 2.d0, -31.d0,  13.d0,
     6           0.d0, 0.d0, 2.d0, 0.d0, 0.d0,  26.d0,  -1.d0,
     7          -1.d0, 0.d0, 2.d0, 0.d0, 1.d0,  21.d0, -10.d0,
     8          -1.d0, 0.d0, 0.d0, 2.d0, 1.d0,  16.d0,  -8.d0,
     9           1.d0, 0.d0, 0.d0,-2.d0, 1.d0, -13.d0,   7.d0,
C  50
     -          -1.d0, 0.d0, 2.d0, 2.d0, 1.d0, -10.d0,   5.d0,
     1           1.d0, 1.d0, 0.d0,-2.d0, 0.d0,  -7.d0,   0.d0,
     2           0.d0, 1.d0, 2.d0, 0.d0, 2.d0,   7.d0,  -3.d0,
     3           0.d0,-1.d0, 2.d0, 0.d0, 2.d0,  -7.d0,   3.d0,
     4           1.d0, 0.d0, 2.d0, 2.d0, 2.d0,  -8.d0,   3.d0,
     5           1.d0, 0.d0, 0.d0, 2.d0, 0.d0,   6.d0,   0.d0,
     6           2.d0, 0.d0, 2.d0,-2.d0, 2.d0,   6.d0,  -3.d0,
     7           0.d0, 0.d0, 0.d0, 2.d0, 1.d0,  -6.d0,   3.d0,
     8           0.d0, 0.d0, 2.d0, 2.d0, 1.d0,  -7.d0,   3.d0,
     9           1.d0, 0.d0, 2.d0,-2.d0, 1.d0,   6.d0,  -3.d0 /
      DATA ((ARG2(I,J),I=1,7),J=38,47)/
C  60
     -           0.d0, 0.d0, 0.d0,-2.d0, 1.d0,  -5.d0,   3.d0,
     1           1.d0,-1.d0, 0.d0, 0.d0, 0.d0,   5.d0,   0.d0,
     2           2.d0, 0.d0, 2.d0, 0.d0, 1.d0,  -5.d0,   3.d0,
     3           0.d0, 1.d0, 0.d0,-2.d0, 0.d0,  -4.d0,   0.d0,
     4           1.d0, 0.d0,-2.d0, 0.d0, 0.d0,   4.d0,   0.d0,
     5           0.d0, 0.d0, 0.d0, 1.d0, 0.d0,  -4.d0,   0.d0,
     6           1.d0, 1.d0, 0.d0, 0.d0, 0.d0,  -3.d0,   0.d0,
     7           1.d0, 0.d0, 2.d0, 0.d0, 0.d0,   3.d0,   0.d0,
     8           1.d0,-1.d0, 2.d0, 0.d0, 2.d0,  -3.d0,   1.d0,
     9          -1.d0,-1.d0, 2.d0, 2.d0, 2.d0,  -3.d0,   1.d0 /
C  70
      DATA ((ARG2(I,J),I=1,7),J=48,57)/
     -          -2.d0, 0.d0, 0.d0, 0.d0, 1.d0,  -2.d0,   1.d0,
     1           3.d0, 0.d0, 2.d0, 0.d0, 2.d0,  -3.d0,   1.d0,
     2           0.d0,-1.d0, 2.d0, 2.d0, 2.d0,  -3.d0,   1.d0,
     3           1.d0, 1.d0, 2.d0, 0.d0, 2.d0,   2.d0,  -1.d0,
     4          -1.d0, 0.d0, 2.d0,-2.d0, 1.d0,  -2.d0,   1.d0,
     5           2.d0, 0.d0, 0.d0, 0.d0, 1.d0,   2.d0,  -1.d0,
     6           1.d0, 0.d0, 0.d0, 0.d0, 2.d0,  -2.d0,   1.d0,
     7           3.d0, 0.d0, 0.d0, 0.d0, 0.d0,   2.d0,   0.d0,
     8           0.d0, 0.d0, 2.d0, 1.d0, 2.d0,   2.d0,  -1.d0,
     9          -1.d0, 0.d0, 0.d0, 0.d0, 2.d0,   1.d0,  -1.d0 /
      DATA ((ARG2(I,J),I=1,7),J=58,67)/
C  80
     -           1.d0, 0.d0, 0.d0,-4.d0, 0.d0,  -1.d0,   0.d0,
     1          -2.d0, 0.d0, 2.d0, 2.d0, 2.d0,   1.d0,  -1.d0,
     2          -1.d0, 0.d0, 2.d0, 4.d0, 2.d0,  -2.d0,   1.d0,
     3           2.d0, 0.d0, 0.d0,-4.d0, 0.d0,  -1.d0,   0.d0,
     4           1.d0, 1.d0, 2.d0,-2.d0, 2.d0,   1.d0,  -1.d0,
     5           1.d0, 0.d0, 2.d0, 2.d0, 1.d0,  -1.d0,   1.d0,
     6          -2.d0, 0.d0, 2.d0, 4.d0, 2.d0,  -1.d0,   1.d0,
     7          -1.d0, 0.d0, 4.d0, 0.d0, 2.d0,   1.d0,   0.d0,
     8           1.d0,-1.d0, 0.d0,-2.d0, 0.d0,   1.d0,   0.d0,
     9           2.d0, 0.d0, 2.d0,-2.d0, 1.d0,   1.d0,  -1.d0 /
C  90
      DATA ((ARG2(I,J),I=1,7),J=68,77)/
     -           2.d0, 0.d0, 2.d0, 2.d0, 2.d0,  -1.d0,   0.d0,
     1           1.d0, 0.d0, 0.d0, 2.d0, 1.d0,  -1.d0,   0.d0,
     2           0.d0, 0.d0, 4.d0,-2.d0, 2.d0,   1.d0,   0.d0,
     3           3.d0, 0.d0, 2.d0,-2.d0, 2.d0,   1.d0,   0.d0,
     4           1.d0, 0.d0, 2.d0,-2.d0, 0.d0,  -1.d0,   0.d0,
     5           0.d0, 1.d0, 2.d0, 0.d0, 1.d0,   1.d0,   0.d0,
     6          -1.d0,-1.d0, 0.d0, 2.d0, 1.d0,   1.d0,   0.d0,
     7           0.d0, 0.d0,-2.d0, 0.d0, 1.d0,  -1.d0,   0.d0,
     8           0.d0, 0.d0, 2.d0,-1.d0, 2.d0,  -1.d0,   0.d0,
     9           0.d0, 1.d0, 0.d0, 2.d0, 0.d0,  -1.d0,   0.d0 /
      DATA ((ARG2(I,J),I=1,7),J=78,84)/
C 100
     -           1.d0, 0.d0,-2.d0,-2.d0, 0.d0,  -1.d0,   0.d0,
     1           0.d0,-1.d0, 2.d0, 0.d0, 1.d0,  -1.d0,   0.d0,
     2           1.d0, 1.d0, 0.d0,-2.d0, 1.d0,  -1.d0,   0.d0,
     3           1.d0, 0.d0,-2.d0, 2.d0, 0.d0,  -1.d0,   0.d0,
     4           2.d0, 0.d0, 0.d0, 2.d0, 0.d0,   1.d0,   0.d0,
     5           0.d0, 0.d0, 2.d0, 4.d0, 2.d0,  -1.d0,   0.d0,
     6           0.d0, 1.d0, 0.d0, 1.d0, 0.d0,   1.d0,   0.d0 /
C
      ROH = PI/648000.D0
C
C  TIME INTERVAL (IN JUL. CENTURIES) BETWEEN XTDB AND J2000.0
      TU =(XTDB-51544.5d0)/36525.D0
C TU4 = MOD JUL. DATUM OF REFERENCE EPOCH ( XMOD = XTDB FOR EPS)
      TU4=TU
C
C  FUNDAMENTAL ARGUMENTS (IN RAD)
      ALP(1)=(485866.733d0+(1325.D0*R+715922.633d0)*TU+31.31d0*TU*TU+
     1       .064d0*TU*TU*TU)*ROH
      ALP(2)=(1287099.804d0+(99.D0*R+1292581.224d0)*TU-0.577d0*TU*TU-
     1       .012d0*TU*TU*TU)*ROH
      ALP(3)=(335778.877d0+(1342.D0*R+295263.137d0)*TU-13.257d0*TU*TU+
     1       .011d0*TU*TU*TU)*ROH
      ALP(4)=(1072261.307d0+(1236.D0*R+1105601.328d0)*TU-6.891d0*TU*TU+
     1       .019d0*TU*TU*TU)*ROH
      ALP(5)=(450160.28d0-(5.D0*R+482890.539d0)*TU+7.455d0*TU*TU+
     1       .008d0*TU*TU*TU)*ROH
C
C EPS-ZERO EPOCH J2000 (RAD)
      EPS=84381.448d0-46.815d0*TU4-0.00059d0*TU4*TU4+
     1 0.001813d0*TU4*TU4*TU4
      EPS=EPS*ROH
C
      DPSI=0.D0
      DEPS=0.D0
      DO 10 J=1,22
        PHI = 0.D0
        DO 5 I=1,5
          PHI = PHI + ARG1(I,J)*ALP(I)
5       CONTINUE
        PHI = DMOD (PHI,2.D0*PI)
C       NUTATION IN ARCSEC*10000
        DPSI = DPSI+(ARG1(6,J)+ARG1(7,J)*TU)*DSIN(PHI)
        DEPS = DEPS+(ARG1(8,J)+ARG1(9,J)*TU)*DCOS(PHI)
10    CONTINUE
      DO 20 J=1,84
        PHI = 0.D0
        DO 15 I=1,5
          PHI = PHI + ARG2(I,J)*ALP(I)
15      CONTINUE
        PHI = DMOD (PHI,2.D0*PI)
C SERIES FOR NUTATION IN ARCSEC*10000 IN LONGITUDE AND OBLIQUITY
        DPSI = DPSI+ARG2(6,J)*DSIN(PHI)
        DEPS = DEPS+ARG2(7,J)*DCOS(PHI)
20    CONTINUE
C
C DPSI AND DEPS IN RADIANS
      DEPS=DEPS*ROH/10000.D0
      DPSI=DPSI*ROH/10000.D0
C
C CELESTIAL POLE OFFSETS IN RADIANS
      CALL GETCPO(XTDB,TT,TTEPS,TTPSI)
      CALL LITPOL(2,1,TT,TTEPS,XTDB,0d0,DDEPS)
      CALL LITPOL(2,1,TT,TTPSI,XTDB,0d0,DDPSI)
      DEPS=DEPS+DDEPS(1)
      DPSI=DPSI+DDPSI(1)
C
C TRUE OBLIQUITY EPSTRU
      EPSTRU=EPS+DEPS
C
C SINE AND COSINE
      SINDP=DSIN(DPSI)
      COSDP=DCOS(DPSI)
      SINEM=DSIN(EPS)
      COSEM=DCOS(EPS)
      SINET=DSIN(EPSTRU)
      COSET=DCOS(EPSTRU)
C
C  ROTATION MATRIX
      NUT(1,1)= COSDP
      NUT(2,1)= COSET*SINDP
      NUT(3,1)= SINET*SINDP
      NUT(1,2)=-COSEM*SINDP
      NUT(2,2)= COSEM*COSET*COSDP + SINEM*SINET
      NUT(3,2)= COSEM*SINET*COSDP - SINEM*COSET
      NUT(1,3)=-SINEM*SINDP
      NUT(2,3)= SINEM*COSET*COSDP - COSEM*SINET
      NUT(3,3)= SINEM*SINET*COSDP + COSEM*COSET
C
      RETURN
      END SUBROUTINE

      END MODULE
