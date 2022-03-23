MODULE s_APUPDATE
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE apupdate(neq,neq_1,crsp)

! -------------------------------------------------------------------------
! Purpose:    Make sure that the apriori values of two NEQs are identical.
!
! Author:     R. Dach
!
! Created:    26-Nov-2002
!
! Changes:    04-Feb-2009 SL: if condition extended by time%half (murx)
!             16-Dec-2010 RD: t_neq is taken from D_NEQ instead of P_ADDNEQ
!             19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b
  USE d_neq,    ONLY: t_neq

  USE f_ikf
  USE s_alcerr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  TYPE(t_neq)                             :: neq    ! Main normal equation
  TYPE(t_neq)                             :: neq_1  ! Normal equation to stack
  INTEGER(i4b),DIMENSION(:)               :: crsp   ! Index liste

! output:

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER             :: srName = 'apupdate'

! Local Variables
! ---------------
  INTEGER(i4b)                            :: ii,i_1
  INTEGER(i4b)                            :: kk
  INTEGER(i4b)                            :: iDiff
  INTEGER(i4b)                            :: irc

  REAL(r8b)                               :: rHlp
  REAL(r8b),DIMENSION(:),ALLOCATABLE      :: dx0

! Check: apriori values are identical
! -----------------------------------
  ALLOCATE(dx0(neq_1%misc%npar),stat=irc)
  CALL alcerr(irc,'dx0',(/neq_1%misc%npar/),srName)

  iDiff = 0

  DO i_1 = 1, neq_1%misc%npar

    ii = crsp(i_1)
    IF (ii == 0) CYCLE

    IF (neq%par(ii)%x0 /= neq_1%par(i_1)%x0 .AND. &
        neq%par(ii)%time%half > 0d0) THEN

      dx0(i_1) = neq%par(ii)%x0 - neq_1%par(i_1)%x0
      neq_1%par(i_1)%x0 = neq%par(ii)%x0

      ! Sorry, but here we have a different sign...
      if (neq%par(ii)%locq(1) == 23) dx0(i_1) = -dx0(i_1)
      if (neq%par(ii)%locq(1) == 24) dx0(i_1) = -dx0(i_1)

      iDiff = iDiff + 1

    ELSE
      dx0(i_1) = 0d0
    ENDIF

  ENDDO

! Perform the transformation
! --------------------------
  IF (iDiff > 0) THEN

    DO i_1 = 1, neq_1%misc%npar
      rHlp = 0d0
      DO kk = 1, neq_1%misc%npar
        rHlp = rHlp + neq_1%aNor(ikf(i_1,kk)) * dx0(kk)
      END DO
      neq_1%misc%lTPl = neq_1%misc%lTPl + dx0(i_1) * &
                                      (-2.d0*neq_1%bNor(i_1) + rHlp)
      neq_1%bNor(i_1) = neq_1%bNor(i_1) - rHlp
    END DO

  ENDIF

  DEALLOCATE(dx0,stat=irc)

  RETURN

END SUBROUTINE apupdate

END MODULE s_APUPDATE
