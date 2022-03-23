      MODULE s_XYZLOC
      CONTAINS

C*
      SUBROUTINE XYZLOC(PHI,RLAM,H,A,B,ICODE,CM,DM,HMAT)
CC
CC NAME       :  XYZLOC
CC
CC PURPOSE    :  THIS ROUTINE COMPUTES THE TRANSFORMATION
CC               OF COMPONENTS (3X1) IN THE XYZ SYSTEM
CC               TO THE ONES IN THE LOCAL SYSTEM (DM)
CC
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
CC        OUT :  CM     : 3X1 COMPONENTS OF THE               R*8(3)
CC                        CARTESIAN COORDINATES IN METERS
CC               DM     : 3X1 COMPONENTS OF THE               R*8(3)
CC                        ELLIPSOIDAL COORDINATES
CC                        IN M FOR ALL COMONENTS !!!
CC               HMAT   : 3X3 MATRIX OF TRANSFORMATION        R*8(3,3)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  E. BROCKMANN
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  13-SEP-95
CC
CC CHANGES    :  26-MAR-04 : DT: ADD TRANSFORMATION MATRIX HMAT AS
CC                               OUTPUT PARAMETER
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1995     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE s_dminv
      USE s_dmlmam
      USE s_dmlmav
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      REAL*8    A   , B   , CL  , CM  , CP  , DEN , DET , DM  , DN  ,
     1          E2  , H   , HMAT, J   , M   , N   , PHI , RH  , RLAM,
     2          SL  , SP
C
CCC       IMPLICIT REAL*8 (A-Z)
      INTEGER*4     L1(3),L2(3),ICODE
      DIMENSION     CM(3),DM(3),J(3,3),DN(3,3),HMAT(3,3),RH(3)
C
      E2=(A*A-B*B)/(A*A)
      SP=DSIN(PHI)
      CP=DCOS(PHI)
      DEN=DSQRT(1.D0-E2*SP**2)
      SL=DSIN(RLAM)
      CL=DCOS(RLAM)
      N=A/DEN
      M=A*(1.D0-E2)/DEN**3
      J(1,1)=-(M+H)*SP*CL
      J(1,2)=-(N+H)*CP*SL
      J(1,3)=CP*CL
      J(2,1)=-(M+H)*SP*SL
      J(2,2)=(N+H)*CP*CL
      J(2,3)=CP*SL
      J(3,1)=(M+H)*CP
      J(3,2)=0.D0
      J(3,3)=SP
      DN(1,1)=M+H
      DN(1,2)=0.D0
      DN(1,3)=0.D0
      DN(2,1)=0.D0
      DN(2,2)=(N+H)*CP
      DN(2,3)=0.D0
      DN(3,1)=0.D0
      DN(3,2)=0.D0
      DN(3,3)=1.D0
C
      IF(ICODE.LT.0)GO TO 1
C
C LOCAL --> XYZ
C
      CALL DMINV(DN,3,DET,L1,L2)
      CALL DMLMAV(DM,DN,RH)
      CALL DMLMAV(RH,J,CM)
      CALL DMLMAM(J,DN,HMAT)
      GO TO 2
C
C  XYZ --> LOCAL
C
    1 CONTINUE
C
      CALL DMINV(J,3,DET,L1,L2)
C
      CALL DMLMAM(DN,J,HMAT)
      CALL DMLMAV(CM,HMAT,DM)
C
2     CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
