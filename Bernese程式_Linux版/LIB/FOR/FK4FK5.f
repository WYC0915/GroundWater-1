      MODULE s_FK4FK5
      CONTAINS

C*
      SUBROUTINE FK4FK5(XSTAR0,VSTAR0,PARAL0,VRAD0,
     1                  XSTAR1,VSTAR1,PARAL1,VRAD1)
CC
CC NAME       :  FK4FK5
CC
CC PURPOSE    :  CONVERSION OF STELLAR POSITIONS FROM STANDARD EPOCH
CC               B1950.0 (MEAN PLACE ON FK4) TO THE STANDARD EPOCH
CC               J2000.0 (MEAN PLACE ON FK5), IGNORING THE SYSTEMATIC
CC               ERRORS FK4-FK5 (WHATEVER THIS MEANS).
CC
CC                SEE ASTRONOMICAL ALMANAC 1988, B42 AND
CC                    ASTRON. J.,97, P. 274-279 (1989)
CC
CC PARAMETERS :
CC         IN :  XSTAR0 : RECTANGULAR STAR POSITION (UNIT       R*8(3)
CC                        VECTOR) IN THE FK4 B1950.0 SYSTEM
CC               VSTAR0 : TIME DERIVATIVE OF XSTAR0             R*8(3)
CC                        [RAD/TROP. CY] IN THE FK4 B1950.0
CC                        SYSTEM
CC               PARAL0 : PARALLAX [ARCSEC] IN THE              R*8
CC                        FK4 B1950.0 SYSTEM
CC               VRAD0  : RADIAL VELOCITY [KM/SEC] IN THE       R*8
CC                        FK4 B1950.0 SYSTEM
CC        OUT :  XSTAR1 : RECTANGULAR STAR POSITION (UNIT       R*8(3)
CC                        VECTOR) IN THE FK5 J2000.0 SYSTEM
CC               VSTAR1 : TIME DERIVATIVE OF XSTAR1             R*8(3)
CC                        [RAD/JUL. CY] IN THE FK5 J2000.0
CC                        SYSTEM
CC               PARAL1 : PARALLAX [ARCSEC] IN THE              R*8
CC                        FK5 J2000.0 SYSTEM
CC               VRAD1  : RADIAL VELOCITY [KM/SEC] IN THE       R*8
CC                        FK5 J2000.0 SYSTEM
CC
CC REMARKS    :  INPUT AND OUTPUT PARAMETERS CAN BE THE SAME
CC
CC AUTHOR     :  T.SCHILDKNECHT
CC
CC VERSION    :  3.3
CC
CC CREATED    :  31-MAY-92
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-FEB-07 : AG: USE 206264... FROM DEFCOND
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const, ONLY: ars
      USE s_sprod
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I, K
C
CCC       IMPLICIT INTEGER*4 (I-N)
      REAL*8   XSTAR0(3),VSTAR0(3),VRAD0,PARAL0
      REAL*8   XSTAR1(3),VSTAR1(3),VRAD1,PARAL1
      REAL*8   ABERR(3),ABEDOT(3),R(6),R1(6)
      REAL*8   M(6,6),M1(6),M2(6),M3(6),M4(6),M5(6),M6(6)
      REAL*8   SCAL,RR,RR1,RR2,RADTOS,KMSAUC
C
      EQUIVALENCE (M1(1),M(1,1)),(M2(1),M(1,2)),(M3(1),M(1,3))
      EQUIVALENCE (M4(1),M(1,4)),(M5(1),M(1,5)),(M6(1),M(1,6))
C  RADIANS TO SECONDS, KM/SEC TO AU/TROP. CENTURY
C      DATA RADTOS/206264.806247D0/
      RADTOS = ars
      DATA KMSAUC/21.0945021977D0/
C
C  E-TERMS OF ABERRATION
      DATA ABERR /-1.62557D-6,-0.31919D-6,-0.13843D-6/
      DATA ABEDOT/+1.244  D-3,-1.579  D-3,-0.660  D-3/
C
C  MATRIX M
      DATA M1/
     *       +0.999 925 6782 D0,
     *       +0.011 182 0610 D0,
     *       +0.004 857 9479 D0,
     *       -0.000 551      D0,
     *       +0.238 514      D0,
     *       -0.435 623      D0/
      DATA M2/
     *       -0.011 182 0611 D0,
     *       +0.999 937 4784 D0,
     *       -0.000 027 1474 D0,
     *       -0.238 565      D0,
     *       -0.002 667      D0,
     *       +0.012 254      D0/
      DATA M3/
     *       -0.004 857 9477 D0,
     *       -0.000 027 1765 D0,
     *       +0.999 988 1997 D0,
     *       +0.435 739      D0,
     *       -0.008 541      D0,
     *       +0.002 117      D0/
      DATA M4/
     *       +2.423 950 18 D-6,
     *       +0.027 106 63 D-6,
     *       +0.011 776 56 D-6,
     *       +0.999 947 04 D0 ,
     *       +0.011 182 51 D0 ,
     *       +0.004 857 67 D0 /
      DATA M5/
     *       -0.027 106 63 D-6,
     *       +2.423 978 78 D-6,
     *       -0.000 065 82 D-6,
     *       -0.011 182 51 D0 ,
     *       +0.999 958 83 D0 ,
     *       -0.000 027 14 D0 /
      DATA M6/
     *       -0.011 776 56 D-6,
     *       -0.000 065 87 D-6,
     *       +2.424 101 73 D-6,
     *       -0.004 857 67 D0,
     *       -0.000 027 18 D0,
     *       +1.000 009 56 D0/
C
C  REMOVE E-TERMS OF ABERRATION
C  (R1(1,2,3)=POSITION; R1(4,5,6)=VELOCITY IN ARCSEC/CENTURY)
      CALL SPROD(XSTAR0,ABERR,SCAL,RR1,RR2)
      DO 20 I=1,3
        R1(I)=XSTAR0(I)-ABERR(I)+SCAL*XSTAR0(I)
20    CONTINUE
      CALL SPROD(XSTAR0,ABEDOT,SCAL,RR1,RR2)
      DO 30 I=1,3
CX      R1(I+3)=VSTAR0(I)*RADTOS-ABEDOT(I)+SCAL*XSTAR0(I)
        R1(I+3)=(VSTAR0(I)+KMSAUC*PARAL0*VRAD0*XSTAR0(I))*RADTOS-
     1                                   ABEDOT(I)+SCAL*XSTAR0(I)
30    CONTINUE
C
C  TRANFORMATION R = M R1
      DO 50 I=1,6
        R(I)=0.D0
        DO 40 K=1,6
          R(I)=R(I)+M(I,K)*R1(K)
40      CONTINUE
50    CONTINUE
C
C  NORMALIZATION
      RR=SQRT(R(1)**2+R(2)**2+R(3)**2)
C
C  R --> XSTAR1,VSTAR1 (NORMALIZED)
      DO 60 I=1,3
        XSTAR1(I)=R(I)/RR
60    CONTINUE
      DO 70 I=1,3
        VSTAR1(I)=R(I+3)/RADTOS/RR
70    CONTINUE
C
C  VRAD1
      CALL SPROD(R(1),R(4),VRAD1,RR1,RR2)
      IF(PARAL0.EQ.0.D0) THEN
        VRAD1=VRAD0
      ELSE
        VRAD1=VRAD1/RR1/PARAL0/KMSAUC
      ENDIF
C
C  PARAL1
      PARAL1=PARAL0/RR1
C
999   RETURN
      END SUBROUTINE

      END MODULE
