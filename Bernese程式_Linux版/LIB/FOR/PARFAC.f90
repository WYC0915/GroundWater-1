MODULE f_parfac
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION parfac(tobs,tpar,dtpar,dtobs,parTyp)

! -------------------------------------------------------------------------
! Purpose:    Computes interpolation factors with respect to the vertices
!             of a piece-wise linear parameter representation.
!
! Remark:     Unit of all input arguments may be arbitrary, but must be
!             consistent.
!
! Author:     S. Schaer
!
! Created:    14-May-2003
! Last mod.:  16-Nov-2010
!
! Changes:    02-Feb-2004 mm: dabs instead of abs
!             16-Nov-2010 RD: update interval for piece-wise linear param.
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE p_gpsest, ONLY: t_parTyp
  USE d_par,    ONLY: parType_linear, parType_linearleftPoint, &
                      parType_linearMiddlePoint,parType_linearRightPoint

  IMPLICIT NONE

  REAL(r8b)               :: parfac

! List of parameters
! ------------------
  REAL(r8b)               :: tobs   ! Observation epoch
  REAL(r8b)               :: tpar   ! Reference epoch of parameter
  REAL(r8b)               :: dtpar  ! Time interval between consecutive
                                    ! parameters
  REAL(r8b)               :: dtobs  ! Tolerance concerning observation
                                    ! epoch (to avoid boundary problems)
  TYPE(t_parTyp), OPTIONAL::parTyp  ! parameter type description


! Compute interpolation factor
! ----------------------------
  parfac = 1d0-DABS(tobs-tpar)/dtpar

! Consider given tolerance
! ------------------------
  IF (parfac < dtobs/dtpar) THEN

    parfac = 0d0

  ELSE IF (parfac > 1d0-dtobs/dtpar) THEN

    parfac = 1d0

  END IF


! Update the parameter type description
! -------------------------------------
  IF (PRESENT(partyp) .AND. parFac /= 0d0) THEN
    IF( partyp%type(1:1) == parType_linear(1:1)) THEN

      IF (tobs-dtpar > tpar) THEN

        IF (partyp%type == parType_linear) THEN
          partyp%type = parType_linearLeftPoint
        ELSEIF (partyp%type == parType_linearRightPoint) THEN
          partyp%type = parType_linearMiddlePoint
        ENDIF

      ELSEIF (tobs+dtpar < tpar) THEN

        IF (partyp%type == parType_linear) THEN
          partyp%type = parType_linearRightPoint
        ELSEIF (partyp%type == parType_linearLeftPoint) THEN
          partyp%type = parType_linearMiddlePoint
        ENDIF

      ENDIF

      partyp%omega=dtpar

    ENDIF
  ENDIF

END FUNCTION parfac

END MODULE
