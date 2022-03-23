      MODULE s_HERR87
      CONTAINS

C*
      SUBROUTINE HERR87(DMJD,DPSI,DEPS)
CC
CC NAME       :  HERR87
CC
CC PURPOSE    :  FUNCTION TO COMPUTE CELESTIAL POLE OFFSETS WITH
CC               RESPECT TO THE DIRECTIONS DEFINED BY THE IAU 1976
CC               PRECESSION AND THE IAU 1980 THEORY OF NUTATION,
CC               ACCORDING TO THE MODEL PUBLISHED BY T. HERRING IN
CC               THE ANNUAL REPORT OF THE BIH FOR 1987, P. D-108.
CC
CC PARAMETERS :
CC         IN :  DMJD   : EPOCH IN MODIFIED JULIAN DATE        R*8
CC        OUT :  DPSI   : NUTATION CORRECTION IN LONGITUDE     R*8
CC                        IN 0.001"
CC               DEPS   : NUTATION CORRECTION IN OBLIQUITY     R*8
CC                        IN 0.001"
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M. FEISSEL
CC
CC VERSION    :  3.4
CC
CC CREATED    :  29-OCT-92
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: D0s ADDED
CC               28-FEB-07 : AG: USE PI FROM DEFCON
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC
CC COPYRIGHT  :  NONE
CC
C*
      USE d_const,  ONLY: pi
      USE l_basfun, ONLY: dmod
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I   , J
C
      REAL*8    C   , CW  , DEPS, DMJD, DPSI, DPT , H   , T   , W
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8 ARG(5,4),AMP(2,4,2),AP(5),AD(5)
C
C NUTATION CORRECTION: DEFINITION OF ARGUMENTS
C --------------------------------------------
      DATA ARG / 0.d0,0.d0,2.d0, 0.d0,2.d0,
     1           0.d0,0.d0,2.d0,-2.d0,2.d0,
     2           0.d0,1.d0,0.d0, 0.d0,0.d0,
     3           0.d0,0.d0,0.d0, 0.d0,1.d0/
C
      DATA AMP /  -0.81d0,  0.00d0,
     1            +1.02d0, -1.18d0,
     2            +5.23d0,  0.61d0,
     3            -7.25d0,  4.17d0,
     4             0.00d0, +0.32d0,
     5            -0.47d0, -0.41d0,
     6            -0.24d0, +2.08d0,
     7            +2.24d0, +2.13d0/
C
      C  = 57.2957795D0
      CW = 72.921151463D0/86400D0
      IF (DMJD.GE.45700.D0)
     .CW = 72.921151467064D0/86400D0
      DPT=  2.D0*PI
C
      AP(1)=134.9629D0/C
      AP(2)=357.5254D0/C
      AP(3)= 93.2728D0/C
      AP(4)=297.8503D0/C
      AP(5)=125.0433D0/C
C
      AD(1)=13.06499295D0/C
      AD(2)=0.985600258D0/C
      AD(3)=13.22935027D0/C
      AD(4)=12.19074911D0/C
      AD(5)=-0.05295381D0/C
      T=DMJD-51544.5D0
C
C NUTATION CORRECTION: COMPUTATION
C --------------------------------
      DPSI=0.D0
      DEPS=0.D0
      DO 20 I=1,4
        W=0.D0
        DO 10 J=1,5
          W=W+ARG(J,I)*(AP(J)+AD(J)*T)
10      CONTINUE
        H=DMOD(W,DPT)
        DPSI=DPSI+(AMP(1,I,1)*DSIN(H)+AMP(2,I,1)*DCOS(H))
        DEPS=DEPS+(AMP(1,I,2)*DSIN(H)+AMP(2,I,2)*DCOS(H))
20    CONTINUE
C
C PRECESSION CORRECTION: REFERENCE EPOCH 1984.0 (MJD=45700.)
C ----------------------------------------------------------
      DPSI=DPSI+(-3.00)*(DMJD-45700.)/365.2422D0
C
      RETURN
      END SUBROUTINE

      END MODULE
