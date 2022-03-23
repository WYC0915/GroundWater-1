MODULE s_NEQCKDIM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE neqckdim(neq,npar_new)

! -------------------------------------------------------------------------
! Purpose:    At present the subroutine just checks the dimension of the
!             NEQ structure. The subroutine is prepared to perform some
!             re-allocation operation in the future.
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
! Last mod.:  22-Sep-2005
!
! Changes:    24-Aug-2001  RD: more output
!             21-Dec-2001  HU: Use m_bern, ONLY for modules
!             21-Dec-2001  HU: m_addneq replaced by p_addneq
!             14-Jan-2004  RD: Correct error msg format
!             22-Sep-2005  RD: Use new module D_NEQ.f90
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_neq,    ONLY: t_neq

  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)  :: neq       ! NEQ structure (see P_ADDNEQ)
  INTEGER(i4b) :: npar_new  ! new number of parameters

  IF (SIZE(neq%par)  >= npar_new) THEN
    neq%misc%npar = npar_new
  ELSE
    WRITE(lfnerr,'(/,A,2(/,18X,A,I10),/)')                                 &
          ' *** SR NEQCKDIM: DIMENSION TOO SMALL',                         &
                            'Requested num. of parameters:  ',npar_new,    &
                            'Maximum size of the array   :  ',SIZE(neq%par)
    CALL EXITRC(2)
  END IF

END SUBROUTINE neqckdim


END MODULE
