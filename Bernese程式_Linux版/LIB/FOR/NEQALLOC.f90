MODULE s_NEQALLOC
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE neqalloc(neq,npar)

! -------------------------------------------------------------------------
! Purpose:    This subroutine allocates memory for the NEQ structure and
!             initializes the structure members. It may be used for
!             the deallocation of the memory, too.
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
! Last mod.:  05-Sep-2012
!
! Changes:    26-Jun-2001  RD: Use alcerr for allocation
!             21-Dec-2001  HU: Use m_bern, ONLY for modules
!             21-Dec-2001  HU: m_addneq replaced by p_addneq
!             22-Sep-2005  RD: Use new module D_NEQ.f90
!             15-Jun-2008  RD: Set technique flags
!             27-Apr-2009  LM/SL: type and omega added to neq%par
!             05-Sep-2012  HB: Initialize neq%par(ipar)%obstim
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_par,    ONLY: init_techn
  USE d_neq,    ONLY: t_neq

  USE s_alcerr
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)             :: neq  ! NEQ structure (see P_ADDNEQ)
  INTEGER(i4b),INTENT(IN) :: npar ! number of parameters

! Local Variables
! ---------------
  INTEGER(i4b)            :: ii
  INTEGER(i4b)            :: iac

  IF ( ASSOCIATED(neq%par) ) THEN
    DEALLOCATE( neq%bNor, stat=iac )
    DEALLOCATE( neq%aNor, stat=iac )
    DEALLOCATE( neq%par, stat=iac  )
  END IF

  IF (npar > 0) THEN
    ALLOCATE( neq%par(npar), stat=iac )
    CALL alcerr(iac, 'neq%par', (/npar/), 'neqalloc')
    ALLOCATE( neq%aNor(npar*(npar+1)/2), stat=iac )
    CALL alcerr(iac, 'neq%aNor', (/npar*(npar+1)/2/), 'neqalloc')
    ALLOCATE( neq%bNor(npar), stat=iac )
    CALL alcerr(iac, 'neq%bNor', (/npar/), 'neqalloc')

    DO ii = 1, npar
      neq%par(ii)%locq      = 0
      neq%par(ii)%name      = ''
      neq%par(ii)%time%mean = 0.d0
      neq%par(ii)%time%half = 0.d0
      neq%par(ii)%x0        = 0.d0
      neq%par(ii)%scale     = 0.d0
      CALL init_techn(neq%par(ii))
      neq%par(ii)%type      = ''
      neq%par(ii)%omega     = 0.0
      neq%par(ii)%obstim%t(1) = 0.D0
      neq%par(ii)%obstim%t(2) = 1.D20
    END DO

    neq%aNor = 0.d0
    neq%bNor = 0.d0
  END IF

END SUBROUTINE neqalloc

END MODULE
