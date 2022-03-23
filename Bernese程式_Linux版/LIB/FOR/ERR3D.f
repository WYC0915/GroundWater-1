      MODULE s_ERR3D
      CONTAINS

C*
      SUBROUTINE ERR3D(PHI,RLAM,H,A,B,ICODE,CM,DM)
CC
CC NAME       :  ERR3D
CC
CC PURPOSE    :  THIS ROUTINE COMPUTES THE COVARIANCE MATRIX CM (3X3)
CC               OF THE CARTESIAN COORDINATES X,Y,Z GIVEN THE COVARI-
CC               ANCE MATRIX DM OF THE ELLIPSOIDAL COORD. PHI,RLAM,H
CC               (OF THE SAME POINT) WHEN ICODE IS 1.
CC               IF ICODE IS -1 THE ROUTINE COMPUTES DM FROM THE
CC               GIVEN CM.
CC
CC PARAMETERS :
CC         IN :  PHI    : ELLIPSOIDAL LATITUDE OF THE POINT   R*8
CC                        (IN RADIANS)
CC               RLAM   : ELLIPSOIDAL LONGITUDE OF THE POINT  R*8
CC                        (IN RADIANS,POSITIVE EAST OF GREENWICH)
CC               H      : ELLIPSOIDAL HEIGHT OF THE POINT     R*8
CC                        IN METRES
CC               A,B    : SEMI-MAJOR AND SEMI-MINOR AXES OF   R*8
CC                        THE REFERENCE ELLIPSOID IN METERS
CC               ICODE  : DIRECTION INDEX                     I*4
CC                        =+1 IF CM IS DESIRED FROM THE GIVEN DM
CC                        =-1 IF DM IS DESIRED FROM THE GIVEN CM
CC        OUT :  CM     : 3X3 COVARIANCE MATRIX OF THE        R*8(3,3)
CC                        CARTESIAN COORDINATES IN METERS SQUARED
CC               DM     : 3X3 COVARIANCE MATRIX OF THE        R*8(3,3)
CC                        ELLIPSOIDAL COORDINATES;
CC                        IN RADIANS SQUARED FOR PHI,RLAM AND
CC                        METRES SQUARED FOR H;
CC                        IN RADIAN-METRES FOR THE COVARIANCES
CC                        BETWEEN PHI,RLAM AND H.
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  R.R.STEEVES
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  77/06/01 10:59
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      REAL*8    A   , B   , CL  , CM  , CP  , DEN , DET , DM  , E2  ,
     1          H   , J1  , J11 , J12 , J13 , J2  , J21 , J22 , J23 ,
     2          J3  , J31 , J32 , J33 , J4  , J5  , J6  , J7  , M   ,
     3          N   , PHI , RLAM, SL  , SP
C
CCC       IMPLICIT REAL*8 (A-Z)
      INTEGER*4 ICODE
      DIMENSION CM(3,3),DM(3,3)
      E2=(A*A-B*B)/(A*A)
      SP=DSIN(PHI)
      CP=DCOS(PHI)
      DEN=DSQRT(1.D0-E2*SP**2)
      SL=DSIN(RLAM)
      CL=DCOS(RLAM)
      N=A/DEN
      M=A*(1.D0-E2)/DEN**3
      J11=-(M+H)*SP*CL
      J12=-(N+H)*CP*SL
      J13=CP*CL
      J21=-(M+H)*SP*SL
      J22=(N+H)*CP*CL
      J23=CP*SL
      J31=(M+H)*CP
      J32=0.D0
      J33=SP
      IF(ICODE.LT.0)GO TO 1
      CM(1,1)=J11**2*DM(1,1)+2.D0*J11*J12*DM(1,2)+2.D0*J11*J13*DM(1,3)+2
     1        .D0*J12*J13*DM(2,3)+J13**2*DM(3,3)+J12**2*DM(2,2)
      CM(1,2)=J11*J21*DM(1,1)+(J11*J22+J12*J21)*DM(1,2)+(J11*J23+J13*J21
     1        )*DM(1,3)+J12*J22*DM(2,2)+(J12*J23+J13*J22)*DM(2,3)+J13*J2
     2        3*DM(3,3)
      CM(2,1)=CM(1,2)
      CM(1,3)=J11*J31*DM(1,1)+(J11*J32+J12*J31)*DM(1,2)+(J11*J33+J13*J31
     1        )*DM(1,3)+J12*J32*DM(2,2)+(J12*J33+J13*J32)*DM(2,3)+J13*
     2        J33*DM(3,3)
      CM(3,1)=CM(1,3)
      CM(2,2)=J21**2*DM(1,1)+2.D0*J21*J22*DM(1,2)+2.D0*J21*J23*DM(1,3)+
     1        J22**2*DM(2,2)+2.D0*J22*J23*DM(2,3)+J23**2*DM(3,3)
      CM(2,3)=J21*J31*DM(1,1)+(J21*J32+J22*J31)*DM(1,2)+(J21*J33+J23*J31
     1        )*DM(1,3)+J22*J32*DM(2,2)+(J22*J33+J23*J32)*DM(2,3)+J23*
     2        J33*DM(3,3)
      CM(3,2)=CM(2,3)
      CM(3,3)=J31**2*DM(1,1)+2.D0*J31*J32*DM(1,2)+2.D0*J31*J33*DM(1,3)+
     1        J32**2*DM(2,2)+2.D0*J32*J33*DM(2,3)+J33**2*DM(3,3)
      GO TO 2
    1 CONTINUE
      DET=J11*J22*J33+J12*J23*J31+J13*J21*J32-J13*J22*J31-J12*J21*J33-J1
     1    1*J23*J32
      J1=J11
      J2=J12
      J3=J13
      J4=J21
      J5=J22
      J6=J23
      J7=J31
      J11=(J22*J33-J23*J32)/DET
      J21=-(J21*J33-J31*J23)/DET
      J31=(J4*J32-J22*J31)/DET
      J12=-(J2*J33-J3*J32)/DET
      J22=(J1*J33-J3*J7)/DET
      J32=-(J1*J32-J2*J7)/DET
      J13=(J2*J6-J3*J5)/DET
      J23=-(J1*J6-J3*J4)/DET
      J33=(J1*J5-J2*J4)/DET
      DM(1,1)=J11**2*CM(1,1)+2.D0*J11*J12*CM(1,2)+2.D0*J11*J13*CM(1,3)+2
     1        .D0*J12*J13*CM(2,3)+J13**2*CM(3,3)+J12**2*CM(2,2)
      DM(1,2)=J11*J21*CM(1,1)+(J11*J22+J12*J21)*CM(1,2)+(J11*J23+J13*J21
     1        )*CM(1,3)+J12*J22*CM(2,2)+(J12*J23+J13*J22)*CM(2,3)+J13*J2
     2        3*CM(3,3)
      DM(2,1)=DM(1,2)
      DM(1,3)=J11*J31*CM(1,1)+(J11*J32+J12*J31)*CM(1,2)+(J11*J33+J13*
     1        J31)*CM(1,3)+J12*J32*CM(2,2)+(J12*J33+J13*J32)*CM(2,3)+J13
     1        *J33*CM(3,3)
      DM(3,1)=DM(1,3)
      DM(2,2)=J21**2*CM(1,1)+2.D0*J21*J22*CM(1,2)+2.D0*J21*J23*CM(1,3)+
     1        J22**2*CM(2,2)+2.D0*J22*J23*CM(2,3)+J23**2*CM(3,3)
      DM(2,3)=J21*J31*CM(1,1)+(J21*J32+J22*J31)*CM(1,2)+(J21*J33+J23*J31
     1        )*CM(1,3)+J22*J32*CM(2,2)+(J22*J33+J23*J32)*CM(2,3)+J23*
     2        J33*CM(3,3)
      DM(3,2)=DM(2,3)
      DM(3,3)=J31**2*CM(1,1)+2.D0*J31*J32*CM(1,2)+2.D0*J31*J33*CM(1,3)+
     1        J32**2*CM(2,2)+2.D0*J32*J33*CM(2,3)+J33**2*CM(3,3)
    2 RETURN
      END SUBROUTINE

      END MODULE
