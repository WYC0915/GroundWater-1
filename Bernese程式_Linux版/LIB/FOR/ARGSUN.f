      MODULE s_ARGSUN
      CONTAINS

C*
      SUBROUTINE ARGSUN(T,SUN,SUND,XV,NUMSAT,B0,B0D,U0,U0D)
CC
CC NAME       :  ARGSUN
CC
CC PURPOSE    :  THIS SUBROUTINE CALCULATES THE ARGUMENT OF LATITUDE
CC               OF THE SUN IN THE ORBITAL PLANE AND THE INCLINATION OF
CC               THE SUN ABOVE THE ORBITAL PLANE
CC
CC PARAMETERS :
CC         IN :  T      : TIME                                R*8
CC               SUN    : POSITION OF SUN                     R*8(4)
CC               SUND   : VELOCITY OF SUN (M/S)               R*8(3)
CC                        SET SUND TO ZERO IF B0D AND U0D NOT NEEDED
CC               XV     : POSITION AMND VELOCITY OF CELESTIAL R*8(3,2)
CC                        BODY AT TIME T
CC               NUMSAT : SATELLITE NUMBER                    I*4
CC        OUT :  BETA0  : "ELEVATION" OF SUN ABOVE ORBITAL    R*8
CC                        PLANE (RAD)
CC               B0D    : DERIVATIVE OF B0 (RAD/S)            R*8
CC               U0     : "LATITUDE" OF SUN IN ORBITAL PLANE  R*8
CC                        (RAD)
CC               U0D    : DERIVATIVE OF U0 (RAD/S)            R*8
CC
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  T.A.SPRINGER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  29-JUN-1998
CC
CC CHANGES    :  29-JUN-98 : TS: OFFICIAL CREATION
CC               15-AUG-99 : JJ: RM UNUSED VAR ROT3
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               04-MAY-08 : RD: NUMSAT ADDED TO CALL OF SR XYZELE
CC               03-OCT-10 : CR: OUTPUT OF B0 AND U0 DERIVATIVES
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1998     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const, ONLY: GM,AE,J2
      USE s_xyzele
      USE s_ddreh
      USE s_dmlmav
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 NUMSAT,K
      REAL*8    A   , B0  , E   , PER , SATI, SATO, SUN , T   , T0  ,
     1          U0  , XV  , B0D , U0D , SATOD
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION SUN(*),XV(*)
      REAL*8    ROT1(3,3),ROT2(3,3),XSUN1(3)
      REAL*8    SUND(3),ROT1D(3,3),XSUNDT1(3),XSUNDT2(3),XSUND(3)
      REAL*8    SUNVEL,CO,SO,XSUN2_XY
C
C
C COMPUTE RIGHT ASCENTION OF ASCENDING NODE
C -----------------------------------------
      CALL XYZELE(GM,T,XV(1),XV(4),NUMSAT,A,E,SATI,SATO,PER,T0)
C
C COMPUTE POSITION OF THE SUN IN ORBITAL SYSTEM (ANGLES BETA0, U0)
C ----------------------------------------------------------------
      CALL DDREH(3,SATO,ROT1)
      CALL DDREH(1,SATI,ROT2)
      CALL DMLMAV(SUN,ROT1,XSUN1)
      CALL DMLMAV(XSUN1,ROT2,XSUN1)
C
      B0=DATAN2(XSUN1(3),DSQRT(XSUN1(1)**2+XSUN1(2)**2))
      U0=DATAN2(XSUN1(2),XSUN1(1))
C
C COMPUTE B0 AND U0 DERIVATIVES
C -----------------------------

      SUNVEL = DABS(SUND(1)**2+SUND(2)**2+SUND(3)**2)
      IF((SUNVEL.GT.2D4).AND.(NUMSAT.LT.200))THEN

C     APPROXIMATE RAAN RATE [RAD/SEC]
         SATOD = -((3D0/2D0)*DSQRT(GM/(AE**3))*J2*DCOS(SATI))/
     1            (((A/AE)**(7D0/2D0))*((1D0-E**2)**2))
C        SATOD = -7.83D-9 FOR GPS
C        SATOD = -6.69D-9 FOR GLONASS
C        WRITE(*,*)SATOD

C     APPROXIMATE INC RATE = 0

C     ROT1 DERIVATIVE
         CO = DCOS(SATO)
         SO = DSIN(SATO)
         ROT1D(1,1) = -SO
         ROT1D(1,2) =  CO
         ROT1D(1,3) = 0D0
         ROT1D(2,1) = -CO
         ROT1D(2,2) = -SO
         ROT1D(2,3) = 0D0
         ROT1D(3,1) = 0D0
         ROT1D(3,2) = 0D0
         ROT1D(3,3) = 0D0

C     XSUN1 DERIVATIVE
         CALL DMLMAV(SUN,ROT1D,XSUNDT1)
         CALL DMLMAV(SUND,ROT1,XSUNDT2)
         DO K=1,3
            XSUNDT1(K)=XSUNDT1(K)*SATOD
         ENDDO
         XSUND = XSUNDT1 + XSUNDT2
         CALL DMLMAV(XSUND,ROT2,XSUND)

         XSUN2_XY = XSUN1(1)**2+XSUN1(2)**2

         B0D = (XSUND(3)*XSUN2_XY
     1         -XSUN1(3)*(XSUN1(1)*XSUND(1)+XSUN1(2)*XSUND(2)))/
     2         (DSQRT(XSUN2_XY)*(XSUN1(1)**2+XSUN1(2)**2+XSUN1(3)**2))

         U0D = (XSUND(2)*XSUN1(1)-XSUN1(2)*XSUND(1))/XSUN2_XY

      ELSE
         B0D = 0D0
         U0D = 0D0
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
