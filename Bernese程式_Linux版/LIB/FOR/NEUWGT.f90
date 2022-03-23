MODULE s_NEUWGT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE neuwgt(neq)

! -------------------------------------------------------------------------
! Purpose:    Constrain NEU components of the station positions according
!             to input options
!
! Author:     L. Mervart
!
! Created:    10-JAN-1999
! Last mod.:  13-Feb-2008
!
! Changes:    25-Oct-2001 RD: Use undef_s for undefined values in sta-info file
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             09-Jul-2003 RD: Use staInfo instead of opt%crux4
!             23-Feb-2005 SS: Adaptation of rel. vel. sig. values
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             13-Feb-2008 RD: Adapt to par%name == chr*20
!
! SRs called: crdPart
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: opt,staInfo
  USE d_stacrx, ONLY: undef_s

  USE f_ikf
  USE s_crdpart
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)  :: neq

! Local Variables
! ---------------
  INTEGER(i4b)                :: iopt
  INTEGER(i4b), DIMENSION(12) :: indp
  INTEGER(i4b)                :: ipar
  INTEGER(i4b)                :: ipar1
  INTEGER(i4b)                :: ipar2
  INTEGER(i4b)                :: ip1
  INTEGER(i4b)                :: ip2
  INTEGER(i4b)                :: icrd

  REAL(r8b),    DIMENSION(12) :: part
  REAL(r8b)                   :: wgt
  REAL(r8b)                   :: sig
  REAL(r8b)                   :: deltaT

! Constraints according to "opt%sigma" structure
! ----------------------------------------------
  DO iopt = 1, SIZE(opt%sigma)
    IF (opt%sigma(iopt)%locq(1) == 1 .AND. opt%sigma(iopt)%locq(3) < 0) THEN

      indp(:) = 0
      part(:) = 0.d0
      wgt     = (opt%sigma0 / opt%sigma(iopt)%value)**2

      DO ipar = 1, neq%misc%npar
        IF ( neq%par(ipar)%locq(1) == 1                      .AND. &
             neq%par(ipar)%locq(3) == 1                      .AND. &
             neq%par(ipar)%locq(4) == 1                      .AND. &
             neq%par(ipar)%name    == opt%sigma(iopt)%name ) THEN
          indp(1) = ipar
          indp(2) = ipar + 1
          indp(3) = ipar + 2
          EXIT
        END IF
      END DO

      IF (indp(1) /= 0) THEN
        CALL crdpart(neq, indp(1:3), opt%sigma(iopt)%locq(3), part(1:3))
        DO ip1 = 1, 3
          DO ip2 = ip1, 3
            ipar1 = indp(ip1)
            ipar2 = indp(ip2)

            neq%aNor( ikf(ipar1,ipar2) ) = &
                neq%aNor( ikf(ipar1,ipar2) ) + wgt*part(ip1)*part(ip2)
          END DO
        END DO
      END IF

   END IF
  END DO

! Constrain NEU according to "staInfo%coovel" structure (STAINFO file)
! --------------------------------------------------------------------
  DO iopt = 1, staInfo%nCoovel
    IF (staInfo%coovel(iopt)%constr(1) == undef_s .OR. &
        staInfo%coovel(iopt)%constr(2) == undef_s .OR. &
        staInfo%coovel(iopt)%constr(3) == undef_s) CYCLE
    indp(:) = 0
    DO ipar = 1, neq%misc%npar
      IF ( neq%par(ipar)%locq(1) == 1                        .AND. &
           neq%par(ipar)%locq(3) == 1                        .AND. &
           neq%par(ipar)%locq(4) == 1                        .AND. &
           neq%par(ipar)%name(1:staNameLength)==staInfo%coovel(iopt)%stanam(1) &
                                                              ) THEN
        indp(1) = ipar
        indp(2) = ipar + 1
        indp(3) = ipar + 2
        EXIT
      END IF
    END DO

    DO ipar = 1, neq%misc%npar
      IF ( neq%par(ipar)%locq(1) == 1                        .AND. &
           neq%par(ipar)%locq(3) == 1                        .AND. &
           neq%par(ipar)%locq(4) == 1                        .AND. &
           neq%par(ipar)%name(1:staNameLength)==staInfo%coovel(iopt)%stanam(2) &
                                                              ) THEN
        indp(4) = ipar
        indp(5) = ipar + 1
        indp(6) = ipar + 2
        EXIT
      END IF
    END DO

    IF (indp(1) * indp(4) /= 0) THEN
      DO icrd = 1, 3
        IF (staInfo%coovel(iopt)%constr(icrd) == 0.d0) CYCLE

        part(:) = 0.d0
        wgt     = (opt%sigma0 / staInfo%coovel(iopt)%constr(icrd))**2

        CALL crdpart(neq, indp(1:3), icrd, part(1:3))
        CALL crdpart(neq, indp(4:6), icrd, part(4:6))
        part(4:6) = -part(4:6)
        DO ip1 = 1, 6
          DO ip2 = ip1, 6
            ipar1 = indp(ip1)
            ipar2 = indp(ip2)

            neq%aNor( ikf(ipar1,ipar2) ) = &
                neq%aNor( ikf(ipar1,ipar2) ) + wgt*part(ip1)*part(ip2)
          END DO
        END DO
      END DO

    END IF
  END DO

! Constrain velocities according to "staInfo%coovel" structure
! ------------------------------------------------------------
  DO iopt = 1, staInfo%nCoovel
    IF (staInfo%coovel(iopt)%constr(4) == undef_s .OR. &
        staInfo%coovel(iopt)%constr(5) == undef_s .OR. &
        staInfo%coovel(iopt)%constr(6) == undef_s) CYCLE
    indp(:) = 0
    DO ipar = 1, neq%misc%npar
      IF ( neq%par(ipar)%locq(1) == 1                        .AND. &
           neq%par(ipar)%locq(3) == 1                        .AND. &
           neq%par(ipar)%locq(4) == 1                        .AND. &
           neq%par(ipar)%name(1:staNameLength)==staInfo%coovel(iopt)%stanam(1) &
                                                              ) THEN
        indp(1) = ipar
        indp(2) = ipar + 1
        indp(3) = ipar + 2
        EXIT
      END IF
    END DO
    DO ipar = 1, neq%misc%npar
      IF ( neq%par(ipar)%locq(1) == 1                        .AND. &
           neq%par(ipar)%locq(3) == 1                        .AND. &
           neq%par(ipar)%locq(4) == 2                        .AND. &
           neq%par(ipar)%name(1:staNameLength)==staInfo%coovel(iopt)%stanam(1) &
                                                              ) THEN
        indp(4) = ipar
        indp(5) = ipar + 1
        indp(6) = ipar + 2
        EXIT
      END IF
    END DO
    DO ipar = 1, neq%misc%npar
      IF ( neq%par(ipar)%locq(1) == 1                        .AND. &
           neq%par(ipar)%locq(3) == 1                        .AND. &
           neq%par(ipar)%locq(4) == 1                        .AND. &
           neq%par(ipar)%name(1:staNameLength)==staInfo%coovel(iopt)%stanam(2) &
                                                              ) THEN
        indp(7) = ipar
        indp(8) = ipar + 1
        indp(9) = ipar + 2
        EXIT
      END IF
    END DO
    DO ipar = 1, neq%misc%npar
      IF ( neq%par(ipar)%locq(1) == 1                        .AND. &
           neq%par(ipar)%locq(3) == 1                        .AND. &
           neq%par(ipar)%locq(4) == 2                        .AND. &
           neq%par(ipar)%name(1:staNameLength)==staInfo%coovel(iopt)%stanam(2) &
                                                              ) THEN
        indp(10) = ipar
        indp(11) = ipar + 1
        indp(12) = ipar + 2
        EXIT
      END IF
    END DO

    IF (indp(1) * indp(4) * indp(7) * indp(10) /= 0 ) THEN

      DO icrd = 1, 3
        IF (staInfo%coovel(iopt)%constr(3+icrd) == 0.d0) CYCLE

        part(:) = 0.d0
        deltaT  = neq%par(indp(4))%time%mean-neq%par(indp(1))%time%mean
        sig     = staInfo%coovel(iopt)%constr(3+icrd)/365.25d0*deltaT
        wgt     = (opt%sigma0/sig)**2

        CALL crdpart(neq, indp(1:3)  , icrd, part(1:3)  )
        CALL crdpart(neq, indp(4:6)  , icrd, part(4:6)  )
        CALL crdpart(neq, indp(7:9)  , icrd, part(7:9)  )
        CALL crdpart(neq, indp(10:12), icrd, part(10:12))
        part(4:6) = -part(4:6)
        part(7:9) = -part(7:9)

        DO ip1 = 1, 12
          DO ip2 = ip1, 12
            ipar1 = indp(ip1)
            ipar2 = indp(ip2)

            neq%aNor( ikf(ipar1,ipar2) ) = &
                neq%aNor( ikf(ipar1,ipar2) ) + wgt*part(ip1)*part(ip2)
          END DO
        END DO

      END DO
    END IF

  END DO

END SUBROUTINE neuwgt


END MODULE
