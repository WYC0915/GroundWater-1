MODULE f_gtweight
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION gtweight(par,typFlg)

! -------------------------------------------------------------------------
! Purpose:    This function returns the weight which has to be added to
!             NEQ matrix according to input options.
!             Flag 'A' for absolute and 'R' for relative sigmas.
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
! Last mod.:  14-Aug-2009
!
! Changes:    12-Sep-2001  HU: Set weight=0 for stations to be elliminated
!                              through stacrux
!             21-Dec-2001  HU: Use m_bern, ONLY for modules
!             21-Dec-2001  HU: m_addneq replaced by p_addneq
!             07-Feb-2002  MM: Allow absolute and relative sigmas
!             09-Jul-2003  RD: Use staInfo instead of opt%crux
!             06-Jan-2005  HU: Check staProblem at beginning
!             10-May-2005  RD: Use staProblem via function isStacrx
!             22-Sep-2005  RD: Use new module D_PAR.f90
!             14-Aug-2009  SL: isStaCrx removed
!
! SRs called:
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_time,   ONLY: OPERATOR(.ISIN.)
  USE d_par,    ONLY: t_par,maxLcq
  USE p_addneq, ONLY: opt

!!!  USE f_isstacrx
  IMPLICIT NONE

  REAL(r8b)   :: gtweight

! List of Parameters
! ------------------
  TYPE(t_par)      :: par    ! Parameter description structure (see P_ADDNEQ)
  CHARACTER(LEN=1) :: typFlg ! 'A' for absolute, 'R' for relative sigmas


! Local types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER           :: srName = 'gtweight'

! Local Variables
! ---------------
  INTEGER(i4b) :: isig,ilcq
  LOGICAL      :: ok

  gtweight = 0.d0

! do not constrain stations removed by staInfo, return immediately
! IF ( par%locq(1).EQ. 1 ) THEN
!!!    IF (isStacrx(par)) RETURN
! END IF

  DO isig = 1, SIZE(opt%sigma)
    ok = .TRUE.
    IF ( par%locq(1)  /= ABS(opt%sigma(isig)%locq(1)) .AND. &  ! This is due
         opt%sigma(isig)%locq(1) /= 0 ) ok = .FALSE.           ! to freenet
    DO ilcq = 2, maxLcq
      IF ( par%locq(ilcq)  /= opt%sigma(isig)%locq(ilcq) .AND. &
           opt%sigma(isig)%locq(ilcq) /= 0 ) ok = .FALSE.
    END DO
    IF ( par%name  /= opt%sigma(isig)%name .AND. &
         opt%sigma(isig)%name /= '' ) ok = .FALSE.
    IF ( typFlg /= opt%sigma(iSig)%typFlg .AND. &
         opt%sigma(iSig)%typFlg /= '') ok = .FALSE.

    IF (ok) THEN
      gtweight=(opt%sigma0 / opt%sigma(isig)%value / par%scale)**2
      EXIT
    END IF

  END DO

END FUNCTION gtweight


END MODULE
