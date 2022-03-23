MODULE f_gtaprpol
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION gtaprpol(par,ISUBFL)

! -------------------------------------------------------------------------
! Purpose:    This functions returns the a priori value of the pole
!             parameter
!
! Author:     L. Mervart
!
! Created:    16-Dec-2008
! Last mod.:  19-Jul-2010
!
! Changes:    21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             22-Sep-2005 RD: Use new module D_PAR.f90
!             16-Dec-2008 PS: Consider leap seconds
!             19-Jul-2010 SL: tab characters removed
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_par,    ONLY: t_par
  USE d_const,  ONLY: pi

  USE s_poldef
  USE s_cpodef
  IMPLICIT NONE

  REAL(r8b)                    :: gtaprpol

! List of Parameters
! ------------------
  TYPE(t_par)                  :: par    ! Parameter description (see P_ADDNEQ)
  INTEGER(i4b)                 :: ISUBFL ! Subdaily flag
                                         ! 0 ... do not aply subdaily terms
                                         ! 1 ... aply subdaily model
! Local Variables
! ---------------
  REAL(r8b)                    :: tp1
  REAL(r8b)                    :: tp2
  REAL(r8b)                    :: dutgps1,dutgps2
  REAL(r8b),   DIMENSION(2,5)  :: rot
  REAL(r8b),   DIMENSION(5)    :: erpunit
  INTEGER(i4b)                 :: irot
  REAL(r8b),   PARAMETER       :: dT = 1.d0 / 24.d0 / 3600.d0


  erpunit(1) = 1000.d0 * 180.d0 * 3600.d0 / PI
  erpunit(2) = erpunit(1)
  erpunit(3) = 1000.d0 *  24.d0 * 3600.d0
  erpunit(4) = erpunit(1)
  erpunit(5) = erpunit(1)

  gtaprpol = 0.d0

  IF (par%locq(1) == 10 ) THEN

    IF ( par%locq(5) == 1 ) THEN                  ! offset
      tp1 = par%time%mean + dT
      CALL poldef(tp1,ISUBFL,rot(1,1),rot(1,2),rot(1,3),dutgps1)
      CALL cpodef(tp1,rot(1,4),rot(1,5))
      DO irot = 1, 5
        IF (irot == par%locq(4)) THEN
          gtaprpol = rot(1,irot) * erpunit(irot)
        END IF
      END DO
    ELSE IF ( par%locq(5) == 2 ) THEN             ! drift
      tp1 = par%time%mean - par%time%half + dT
      tp2 = par%time%mean + par%time%half - 10.d0*dT
      CALL poldef(tp1,ISUBFL,rot(1,1),rot(1,2),rot(1,3),dutgps1)
      CALL poldef(tp2,ISUBFL,rot(2,1),rot(2,2),rot(2,3),dutgps2)
      CALL cpodef(tp1,rot(1,4),rot(1,5))
      CALL cpodef(tp2,rot(2,4),rot(2,5))
      DO irot = 1, 5
        IF (irot == par%locq(4)) THEN
          gtaprpol = (rot(2,irot)-rot(1,irot))/(tp2-tp1) * erpunit(irot)
        END IF
      END DO

! Leap seconds
      IF (dutgps1 .ne.dutgps2 .and. par%locq(4) .eq. 3  ) THEN
        gtaprpol = (rot(2,3)-rot(1,3)-dT)/(tp2-tp1) * erpunit(3)
      END IF

    END IF

  END IF

END FUNCTION gtaprpol


END MODULE
