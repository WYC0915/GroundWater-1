MODULE s_LEOANTE2
CONTAINS

! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

  SUBROUTINE LEOANTE2(leosvn,sensor,epo,antecc,ityp,xleo,truoff)

! ------------------------------------------------------------------------------
!
!  NAME       :  LEOANTE
!
!  PURPOSE    :  For LEO coordinates (center of mass or sensor) in true system
!                of epoch returns sensor offsets and/or sensor specific offsets
!                (e.g. phase center offsets) in true system of epoch
!
!
!  SR CALLED  :  ATTITUDE, EXITRC, GTOFFUNI, LEOPRN, VPROD
!
!
!  REMARKS    :  -In the case that attitude information is not available for
!                 specific epoch, velocity vector should be placed in xleo(4:6)
!                -antecc is DUMMY parameter if ityp=1
!
!  AUTHOR     :  D. Svehla
!
!
!  CREATED    :  20-Mar-01                             LAST MODIFIED : 21-Oct-08
!
!  CHANGES    :  08-Mar-2003 HU: Interface for sr attiutde used
!                13-May-2003 AJ: Use array constructors to call gtoffuni
!                21-Oct-2008 HB: Use xleo(*) instead of xleo(9)
!
!  COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!                 UNIVERSITY OF BERN
!                     SWITZERLAND
! ------------------------------------------------------------------------------

  USE m_bern
  USE s_attitude
  USE s_gtoffuni
  USE s_vprod
  IMPLICIT NONE

!
! Parameters
! ==========
!
! IN :
! ----
  INTEGER(i4b)                    :: leosvn ! Leo number
  CHARACTER(LEN=staNameLength)    :: sensor ! Sensor name
  REAL(r8b)                       :: epo    ! Epoch in MJD
  REAL(r8b),DIMENSION(3)          :: antecc ! Sensor specific offsets
                                            !   e.g. phase center offset
  INTEGER(i4b)                    :: ityp   ! Type of corrections
                                            ! =0:sensor offsets+specific
                                            !    sensor offsets
                                            ! =1:only sensor offsets
                                            !    e.g. antenna offsets
                                            ! =2:only specific sensor
                                            !    offsets e.g. phase center
                                            !    offsets
  REAL(r8b),DIMENSION(*)          :: xleo   ! Position and velocities
                                            ! see remark
!
! OUT
! ---
  REAL(r8b),DIMENSION(3)          :: truoff ! Requested offset in true system
                                            ! of epoch


!
! Local variables
! ---------------
  REAL(r8b),DIMENSION(3)          :: antoff ! Sensor offset
  REAL(r8b),DIMENSION(3)          :: truecc ! Sensor offsets in true system
  REAL(r8b),DIMENSION(3)          :: bore   ! Sensor boresight vector (U)
  REAL(r8b),DIMENSION(3)          :: azim   ! Sensor reference azimuth vector
  REAL(r8b),DIMENSION(3)          :: Eunit  ! Sensor "E" vector
  REAL(r8b),DIMENSION(3,3)        :: attit  ! Attitude rotation matrix
  REAL(r8b),DIMENSION(3,3)        :: senso  ! Sensor rotation matrix

!
! Get sensor offsets and sensor unit vector
! -----------------------------------------
  CALL GTOFFUNI(1,(/leosvn/),(/sensor/),epo,antoff,bore,azim)
!
! Get attitude rotation matrix
! ----------------------------
  CALL ATTITUDE(leosvn,epo,xleo,0,attit)
!
! Sensor rotation matrix
! -----------------------
  CALL VPROD(azim,bore,Eunit)

  senso(:,1)=azim
  senso(:,2)=Eunit
  senso(:,3)=bore
!
! Sensor correction in LEO body fixed system
! ------------------------------------------
  IF (ityp==0.OR.ityp==2) THEN
    truecc=matmul(senso,antecc)
  END IF
!
! Compute requested offset
! ------------------------
  SELECT CASE (ityp)
  CASE (0)
    antoff(:)=antoff(:)+truecc(:)
  CASE (1)
!     antoff(:)=antoff(:)
  CASE (2)
    antoff(:)=truecc(:)
  END SELECT

  truoff=matmul(attit,antoff)

  RETURN

  END SUBROUTINE LEOANTE2

END MODULE
