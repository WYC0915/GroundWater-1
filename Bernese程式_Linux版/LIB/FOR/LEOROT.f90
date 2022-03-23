MODULE s_LEOROT
CONTAINS

! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

  SUBROUTINE leorot(sensor,epo,ityp,idir,xleo,rotmat)

! ------------------------------------------------------------------------------
!
!  NAME       :  LEOROT
!
!  PURPOSE    :  Returns rotation matrix relating sensor fixed coordinate system
!                with true system of date and/or satellite body fixed system
!
!
!  SR CALLED  :  ATTITUDE, EXITRC, GTOFFUNI, VPROD
!
!
!  REMARKS    :  In the case that attitude information is not available for
!                specific epoch, velocity vector should be placed in xleo(4:6)
!
!
!  AUTHOR     :  D. Svehla
!
!
!  CREATED    :  20-Mar-2001                         LAST MODIFIED : 08-Mar-2003
!
!  CHANGES    :  08-Aug-2001  HB: add parameter to call of gtoffuni
!                16-Dec-2001  HU: Use implicit none, declare antoff
!                08-Mar-2003  HU: Interface for sr attiutde used
!                13-May-2003  AJ: Use array constructors to call gtoffuni
!
!
!  COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!                 UNIVERSITY OF BERN
!                     SWITZERLAND
! ------------------------------------------------------------------------------

  USE m_bern
  USE s_attitude
  USE s_gtoffuni
  USE s_vprod
  USE s_leoprn
  IMPLICIT NONE

! Parameters
! ==========
!
! IN :
! ----
  CHARACTER(LEN=staNameLength)    :: sensor ! Sensor name
  REAL(r8b)                       :: epo    ! Epoch in MJD
  INTEGER(i4b)                    :: ityp   ! Type of relating systems
                                            ! =0:sensor system --> true system
                                            ! =1:sensor system --> body system
  INTEGER(i4b)                    :: idir   ! Direction of rotation
                                            ! =0: from sensor to "other" system
                                            ! =1: from "other" to sensor system
  REAL(r8b),DIMENSION(*)          :: xleo   ! Position and velocities
                                            !   see remark

!
! OUT:
! ----
  REAL(r8b),DIMENSION(3,3)        :: rotmat ! Rotation matrix


!
! Local variables
! ---------------
  INTEGER(i4b)                    :: leosvn

  REAL(r8b),DIMENSION(3)          :: antoff ! Antenna offset
  REAL(r8b),DIMENSION(3)          :: bore   ! Sensor boresight vector (U)
  REAL(r8b),DIMENSION(3)          :: azim   ! Sensor reference azimuth vector
  REAL(r8b),DIMENSION(3)          :: Eunit  ! Sensor "East" vector
  REAL(r8b),DIMENSION(3,3)        :: attit  ! Attitude rotation matrix
  REAL(r8b),DIMENSION(3,3)        :: senso  ! Sensor rotation matrix

!
! Get LEO number
! --------------
  CALL LEOPRN(sensor,epo,leosvn)
!
! Get sensor offsets and sensor unit vector
! -----------------------------------------
  CALL GTOFFUNI(1,(/leosvn/),(/sensor/),epo,antoff,bore,azim)
!
! Sensor rotation matrix
! -----------------------
  CALL VPROD(azim,bore,Eunit)

  senso(:,1)=azim(:)
  senso(:,2)=Eunit(:)
  senso(:,3)=bore(:)
!
! Compute requested matrix
! ------------------------
  SELECT CASE (ityp)
  CASE (0)
    CALL ATTITUDE(leosvn,epo,xleo,0,attit)
    rotmat=matmul(attit,senso)
  CASE (1)
    rotmat(:,:)=senso(:,:)
  END SELECT
!
! Check direction
! ---------------
  IF (idir==1) rotmat=transpose(rotmat)

  RETURN

  END SUBROUTINE leorot

END MODULE
