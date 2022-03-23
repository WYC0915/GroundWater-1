      MODULE s_DVDXYZ
      CONTAINS

C*
      SUBROUTINE DVDXYZ(SINCOS,N,M,PNM,PNM1,SINB,COSB,TANB,
     1                  R,XSAT,GRAD)
CC
CC NAME       :  DVDXYZ
CC
CC PURPOSE    :  PARTIAL DERIVATIVE OF ONE TERM (DEG N, ORDER M,
CC               SIN OR COSINE TERM) OF THE EARTH'S POTENTIAL
CC               WITH RESPECT TO X,Y,Z (GRADIENT OF POT)
CC
CC PARAMETERS :
CC         IN :  SINCOS : COSINE TERM (=1) OR SINE TERM (=2)     I*4
CC                        OF POTENTIAL
CC               N      : DEGREE OF TERM                         I*4
CC               M      : ORDER OF TERM                          I*4
CC               PNM    : VALUE OF LEGENDRE'S FUNCTION (N,M)     R*8
CC               PNM1   : ..                           (N,M+1)   R*8
CC               SINB   : SINE OF LATITUDE                       R*8
CC               COSB   : COSINE OF LATITUDE                     R*8
CC               TANB   : TAN OF LATITUDE                        R*8
CC               R      : GEOCENTRIC DISTANCE                    R*8
CC               XSAT   : SAT POS IN EARTH FIXED SYSTEM          R*8(*)
CC        OUT :  GRAD   : GRADIENT AT POSITION XSAT              R*8(*)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.2
CC
CC CREATED    :  87/12/10 18:57
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const, ONLY: AE, GM
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , J     , K     , M     , N
C
      REAL*8    ADR   , ADRN  , COSB  , COSMXL, DS1   , DS2   , HILF1 ,
     1          HILF2 , PNM   , PNM1  , R     , R2    , R3    , RXY   ,
     2          RXY2  , SINB  , SINMXL, SS10  , TANB  , XL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
      INTEGER*4 SINCOS
      REAL*8   XSAT(*),GRAD(*)
      REAL*8   SS(4),S(4),R1XYZ(3,3),GRADS(3)
C
      R2=R**2
      R3  =R2*R
      RXY2=XSAT(1)**2+XSAT(2)**2
      RXY =DSQRT(RXY2)
      XL  =DATAN2(XSAT(2),XSAT(1))
      TANB=XSAT(3)/RXY
      COSB=RXY/R
      ADR =AE/R
C
C COMPUTATION OF DU/DR,DU/DL, DU/DB
C ---------------------------------
      SINMXL=DSIN(M*XL)
      COSMXL=DCOS(M*XL)
      IF(SINCOS.EQ.1)THEN
        DS1=COSMXL
        DS2=-SINMXL
      ELSE
        DS1= SINMXL
        DS2= COSMXL
      END IF
      HILF1=PNM*DS1
      HILF2=PNM*DS2
      S(1)=+HILF1
      S(2)=+PNM1*DS1
      S(3)=+HILF1*M
      S(4)=+HILF2*M
      ADRN=ADR**N
      DO 10 J=1,4
        SS(J)= S(J)*ADRN
10    CONTINUE
      SS10=+S(1)*ADRN*N
C
C  FIRST DERIVATIVES WITH RESPECT TO R, LAMBDA, BETA
      GRADS(1)=-GM/R2*(SS10+SS(1))
      GRADS(2)= GM/R*SS(4)
      GRADS(3)= GM/R*(-TANB*SS(3)+SS(2))
C
C FIRST DERIVATIVES WITH RESPECT TO X, Y, Z
      DO 20 I=1,3
        R1XYZ(I,1)= XSAT(I)/R
20    CONTINUE
      R1XYZ(1,2)=-XSAT(2)/RXY2
      R1XYZ(1,3)=-XSAT(1)*XSAT(3)/R2/RXY
      R1XYZ(2,2)= XSAT(1)/RXY2
      R1XYZ(2,3)=-XSAT(2)*XSAT(3)/R2/RXY
      R1XYZ(3,2)=0.D0
      R1XYZ(3,3)=RXY/R2
C
C  TRANSFORMATION OF FIRST DERIVATIVES DU/DR ETC TO DU/DX ETC
      DO 40 I=1,3
        GRAD(I)=0.D0
        DO 30 K=1,3
          GRAD(I)=GRAD(I)+R1XYZ(I,K)*GRADS(K)
30      CONTINUE
40    CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
