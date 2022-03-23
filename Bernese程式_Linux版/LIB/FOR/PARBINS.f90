MODULE s_PARBINS
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE parbins(neq,req,iFil)

! -------------------------------------------------------------------------
! Purpose:    This subroutine performs the parameter transformation
!             (binning) according to user request.
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
! Last mod.:  22-Sep-2005
!
! Changes:    21-Dec-2001  HU: Use m_bern, ONLY for modules
!             21-Dec-2001  HU: m_addneq replaced by p_addneq
!             05-Nov-2002  RD: Check bin boundaries for consistency
!             22-Sep-2005  RD: Use new modules D_NEQ.f90 and D_PAR.f90
!
! SR used:    isin, ikf, reqcheck
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_time,   ONLY: OPERATOR(.ISIN.)
  USE d_par,    ONLY: maxLcq
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: t_req

  USE f_ikf
  USE s_reqcheck
  IMPLICIT NONE

! List of parameters
! ------------------
! input/output:
  TYPE(t_neq)                           :: neq

! input:
  TYPE(t_req),INTENT(IN)                :: req ! one request (see P_ADDNEQ)
  INTEGER(i4b)                          :: iFil ! Neq-file index

! Local Parameters
! ----------------
  CHARACTER(LEN=25),DIMENSION(4),PARAMETER :: parString = &
  (/  'unknown type             ','troposphere parameters   ', &
      'troposphere gradients (n)','troposphere gradients (e)' /)

! Local Variables
! ---------------
  INTEGER(i4b)                          :: ipar
  INTEGER(i4b)                          :: ipar_1
  INTEGER(i4b)                          :: ipar_2
  INTEGER(i4b)                          :: ilcq
  INTEGER(i4b)                          :: np
  INTEGER(i4b)                          :: ip
  INTEGER(i4b),DIMENSION(neq%misc%npar) :: crsp
  INTEGER(i4b)                          :: irc

  LOGICAL                               :: ok

! Find the corresponding parameters
! ---------------------------------
  np   = 0
  DO ipar = 1, neq%misc%npar

    ok = .TRUE.
    DO ilcq = 1, maxLcq
      IF ( neq%par(ipar)%locq(ilcq)  /= req%locq(ilcq) .AND. &
           req%locq(ilcq)            /= 0 ) ok = .FALSE.
    END DO

    IF(                           ok                 .AND. &
       (neq%par(ipar)%name        ==   req%name  )   .AND. &
       (neq%par(ipar)%time%mean .isIn. req%timint))  THEN

      np = np + 1
      crsp(np) = ipar

      CALL reqCheck(iFil,iPar,neq,req,irc)

    ENDIF ! End something to do for the parameter
  END DO ! Parameter loop

  IF (np < 2) RETURN

! Perform the transformation
! --------------------------
  ipar_1 = crsp(1)
  DO ip = 2, np
    ipar_2 = crsp(ip)

    DO ipar = 1, neq%misc%npar
      neq%aNor(ikf(ipar_1,ipar)) = neq%aNor(ikf(ipar_1,ipar))   + &
                                   neq%aNor(ikf(ipar_2,ipar))
    END DO
    neq%aNor(ikf(ipar_1,ipar_1)) = neq%aNor(ikf(ipar_1,ipar_1)) + &
                                   neq%aNor(ikf(ipar_2,ipar_1))

    neq%bNor(ipar_1)          = neq%bNor(ipar_1)     + neq%bNor(ipar_2)
    neq%par(ipar_2)%locq(1)   = 0
  END DO

  neq%par(ipar_1)%time%mean = ( req%timint%t(2) + req%timint%t(1) ) / 2.d0
  neq%par(ipar_1)%time%half = ( req%timint%t(2) - req%timint%t(1) ) / 2.d0

  RETURN
END SUBROUTINE parbins


END MODULE
