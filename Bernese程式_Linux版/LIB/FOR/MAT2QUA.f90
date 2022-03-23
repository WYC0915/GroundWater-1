MODULE s_MAT2QUA
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE mat2qua(rot,q)

! -------------------------------------------------------------------------
! Purpose:    Conversion of a rotation matrix into a quaternion
!
! Remark:     q=(q1,q2,q3, q4=cos(THETA/2))
!
! Author:     Drazen Svehla
!
! Created:    27-Aug-2004
! Last mod.:  22-Jul-2005
!
! Changes:    01-Feb-2005 DS: Algorithm to avoid numerical inaccuracy
!             22-Jul-2005 HB: Check if argument for sqrt is < 0.D0
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern, only : r8b,i4b
  IMPLICIT NONE

!
! Parameters
! ----------
! In:
! ---
  REAL(r8b),DIMENSION(3,3) :: rot               ! Rotation matrix
!
! Out:
! ----
  REAL(r8b),DIMENSION(4)   :: q                 ! Quaternion
!
! Local Variables
! ---------------
  REAL(r8b),DIMENSION(4)   :: QQ
!
  INTEGER(i4b),DIMENSION(1)      :: k
!
! Computation of quaternion
! -------------------------
  IF (1.D0+rot(1,1)-rot(2,2)-rot(3,3) < 0.D0) THEN
    QQ(1) = 0.D0
  ELSE
    QQ(1) = 0.5D0*(1.D0+rot(1,1)-rot(2,2)-rot(3,3))**0.5D0
  ENDIF
  IF (1.D0+rot(2,2)-rot(1,1)-rot(3,3) < 0.D0) THEN
    QQ(2) = 0.D0
  ELSE
    QQ(2) = 0.5D0*(1.D0+rot(2,2)-rot(1,1)-rot(3,3))**0.5D0
  ENDIF
  IF (1.D0+rot(3,3)-rot(1,1)-rot(2,2) < 0.D0) THEN
    QQ(3) = 0.D0
  ELSE
    QQ(3) = 0.5D0*(1.D0+rot(3,3)-rot(1,1)-rot(2,2))**0.5D0
  ENDIF
  IF (1.D0+rot(1,1)+rot(2,2)+rot(3,3) < 0.D0) THEN
    QQ(4) = 0.D0
  ELSE
    QQ(4) = 0.5D0*(1.D0+rot(1,1)+rot(2,2)+rot(3,3))**0.5D0
  ENDIF

  k=MAXLOC(QQ)
!
  IF (k(1)==1) THEN
    q(1) = QQ(k(1))
    q(2) = (rot(1,2)+rot(2,1))/4.D0/q(1)
    q(3) = (rot(1,3)+rot(3,1))/4.D0/q(1)
    q(4) = (rot(2,3)-rot(3,2))/4.D0/q(1)
  ELSE IF (k(1)==2) THEN
    q(2) = QQ(k(1))
    q(1) = (rot(1,2)+rot(2,1))/4.D0/q(2)
    q(3) = (rot(2,3)+rot(3,2))/4.D0/q(2)
    q(4) = (rot(3,1)-rot(1,3))/4.D0/q(2)
  ELSE IF (k(1)==3) THEN
    q(3) = QQ(k(1))
    q(1) = (rot(1,3)+rot(3,1))/4.D0/q(3)
    q(2) = (rot(2,3)+rot(3,2))/4.D0/q(3)
    q(4) = (rot(1,2)-rot(2,1))/4.D0/q(3)
  ELSE IF (k(1)==4) THEN
    q(4) = QQ(k(1))
    q(1) = (rot(2,3)-rot(3,2))/4.D0/q(4)
    q(2) = (rot(3,1)-rot(1,3))/4.D0/q(4)
    q(3) = (rot(1,2)-rot(2,1))/4.D0/q(4)
  END IF
!
! Define sign of the quaternion
! -----------------------------
!  IF (q(4) .LT. 0.D0) q(1:4)=-q(1:4)
!
  RETURN
  END SUBROUTINE mat2qua

END MODULE
