MODULE s_QUAMAT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE quamat(quat,mat)

!!-------------------------------------------------------------------------
!! NAME       : quamat
!!
!! PURPOSE    : Conversion of a quaternion into a rotation matrix
!!
!!
!! PARAMETERS :
!!        IN  : quat: quaternion q1,q2,q3 => vector part    REAL(r8b)(4)
!!                               q4       => scalar part
!!        OUT : mat : rotation matrix                       REAL(r8b)(3,3)
!!
!! SR CALLED  : ---
!!
!! REMARKS    :
!!
!! AUTHOR     :  H.BOCK
!!
!! VERSION    :  4.3
!!
!! CREATED    :  00/08/17              LAST MODIFIED :  26-Mar-01
!!
!! CHANGES    :  26-MAR-01: DS: EXCLUDED DIVISION BY NORM NEAR THE LINE 73
!!
!! COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!!      2000      UNIVERSITY OF BERN
!!                    SWITZERLAND
!!-------------------------------------------------------------------------

  USE m_bern, only : r8b,lfnerr
  USE s_exitrc
  IMPLICIT NONE

! Dummy arguments
! ---------------
  REAL(r8b),DIMENSION(4)   :: quat
  REAL(r8b),DIMENSION(3,3) :: mat

! PARAMETERS
! ----------
  REAL(r8b) :: absquat

! TEST IF QUATERNIONS HAVE THE RIGHT VALUES (BETWEEN -1.0 ... +1.0)
! -----------------------------------------------------------------
  IF (dabs(quat(1))>1 .OR. dabs(quat(2))>1 .OR.&
       &dabs(quat(3))>1 .OR. dabs(quat(4))>1) THEN
     WRITE(lfnerr,'(A,4(/,F13.10))')&
          'SR QUAMAT: *** ONE OF THE QUATERNIONS HAS A VALUE > +-1: ',&
          quat(1),quat(2),quat(3),quat(4)
     CALL exitrc(2)
  END IF

! ABSOLUTE VALUE OF QUATERNION
! ----------------------------
  absquat = dsqrt(quat(1)**2+quat(2)**2+quat(3)**2+quat(4)**2)

! ELEMENTS OF ROTATION MATRIX
! ---------------------------
  mat(1,1) = quat(4)**2 + quat(1)**2 - quat(2)**2 - quat(3)**2
  mat(1,2) = 2*(quat(1)*quat(2) - quat(4)*quat(3))
  mat(1,3) = 2*(quat(4)*quat(2) + quat(1)*quat(3))
  mat(2,1) = 2*(quat(4)*quat(3) + quat(1)*quat(2))
  mat(2,2) = quat(4)**2 - quat(1)**2 + quat(2)**2 - quat(3)**2
  mat(2,3) = 2*(quat(2)*quat(3) - quat(4)*quat(1))
  mat(3,1) = 2*(quat(1)*quat(3) - quat(4)*quat(2))
  mat(3,2) = 2*(quat(4)*quat(1) + quat(2)*quat(3))
  mat(3,3) = quat(4)**2 - quat(1)**2 - quat(2)**2 + quat(3)**2

  mat(:,:) = mat(:,:)/absquat

  RETURN
  END SUBROUTINE quamat

END MODULE
