MODULE s_APRHELM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE aprhelm(neq, ifil)

! -------------------------------------------------------------------------
! Purpose:    This subroutine performs the Helmert transformation of the
!             a priori station coordinates
!
! Author:     L. Mervart
!
! Created:    19-May-2005
! Last mod.:  04-Jul-2013
!
! Changes:    26-Jun-2001  RD: Use alcerr for allocation
!             21-Dec-2001  HU: Use m_bern, ONLY for modules
!             21-Dec-2001  HU: m_addneq replaced by p_addneq
!             19-May-2005  CU: Write rescaling factors into protocol
!             23-May-2005  SS: Scale lTPl
!             11-Jun-2005  HU: Indicate estimated helmert parameters
!             22-Sep-2005  RD: Use new module D_NEQ.f90
!             29-Nov-2010  DT: Reading of WGT file moved to AOPTHLM;
!                              Rescaling contained in hlmFil%fact;
!                              Helmert Trafo only if "1"
!             04-Jul-2013  PW: Initialize scale parameter to '1'
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: opt, hlmFil

  USE f_ikf
  USE s_alcerr
  USE s_covcoi
  USE s_ddreh
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)  :: neq    ! Normal Equation Structure (see P_ADDNEQ)
  INTEGER(i4b) :: ifil   ! NEQ file number

! Local Variables
! ---------------
  INTEGER(i4b)                                        :: ip1
  INTEGER(i4b)                                        :: ip2
  INTEGER(i4b)                                        :: ipar

  REAL(r8b),        DIMENSION(3,3)                    :: r1
  REAL(r8b),        DIMENSION(3,3)                    :: r2
  REAL(r8b),        DIMENSION(3,3)                    :: r3
  REAL(r8b),        DIMENSION(3,3)                    :: rotMat
  REAL(r8b),        DIMENSION(3)                      :: xyz
  REAL(r8b),        DIMENSION(3)                      :: shift
  REAL(r8b),        DIMENSION(7)                      :: rhelm


  IF (opt%covcomi == ' ') RETURN


! Scale the NEQ System
! --------------------
  neq%misc%lTPl = hlmFil%fact(ifil) * neq%misc%lTPl
  DO ip1 = 1, neq%misc%npar
    neq%bnor(ip1) = hlmFil%fact(ifil) * neq%bnor(ip1)
    DO ip2 = ip1, neq%misc%npar
      neq%anor(ikf(ip1,ip2)) = hlmFil%fact(ifil) * neq%anor(ikf(ip1,ip2))
    END DO
  END DO


! Search for a priori Helmert parameters (not estimated!)
! -------------------------------------------------------
  rhelm(:) = 0d0
  rhelm(7) = 1d0
  ip1 = 0
  DO ipar = 1, 7
    IF ( hlmFil%ihelm(ipar,ifil) == 1 ) THEN
      rhelm(ipar) = hlmFil%rhelm(ipar,ifil)
      ip1 = ip1 + 1
    ENDIF
  ENDDO


! Apply the a priori Helmert Transformation
! -----------------------------------------
  IF ( ip1 > 0 ) THEN

    CALL ddreh(1,rhelm(4),r1)
    CALL ddreh(2,rhelm(5),r2)
    CALL ddreh(3,rhelm(6),r3)

    shift(:) = rhelm(1:3)

    rotMat = rhelm(7) * MATMUL( r1, MATMUL(r2,r3) )

    DO ipar = 1, neq%misc%npar
      IF ( neq%par(ipar)%locq(1) == 1   .AND. &
           neq%par(ipar)%locq(3) == 1 ) THEN
        xyz(1) = neq%par(ipar  )%x0
        xyz(2) = neq%par(ipar+1)%x0
        xyz(3) = neq%par(ipar+2)%x0

        xyz = MATMUL(rotMat,xyz) + shift

        neq%par(ipar  )%x0 = xyz(1)
        neq%par(ipar+1)%x0 = xyz(2)
        neq%par(ipar+2)%x0 = xyz(3)
      END IF
    END DO

  END IF

END SUBROUTINE aprhelm



END MODULE
