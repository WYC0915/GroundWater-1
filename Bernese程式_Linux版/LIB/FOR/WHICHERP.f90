MODULE s_WHICHERP
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE whicherp(neq,icrd,polTim,ErpFlag,ipar1,ipar2,dt1,dt2)

! -------------------------------------------------------------------------
! Purpose:    This subroutine looks for parameters which have to be used
!             for computation of the Earth's orientation parameter at
!             required time. The parameter indices and interpolation
!             coefficients are given.
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
! Last mod.:  20-Dec-2010
!
! Changes:    24-Aug-2001 RD: rounding problem to find polTim
!                             in neq%par(ipar)%time
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             06-Feb-2003 MR: reference epoch of offset parameter can be
!                             either the same or half of interval different
!                             from the drift parameter
!             15-Apr-2003 RD/SS: Tolerance
!             12-Aug-2003 RS: Allow ipar1, ipar2 = 0 for offsets only
!             28-Jun-2005 MM: Unused variables removed
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             13-Dec-2006 DT: time%half added in case of Offset-only
!             20-Dec-2010 DT: bugfix for piece-wise constant pole;
!                             Add ErpFlag to SR call
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
  TYPE(t_neq)                :: neq
  INTEGER(i4b), INTENT(IN)   :: icrd
  REAL(r8b),    INTENT(IN)   :: polTim
  INTEGER(i4b), DIMENSION(5) :: ErpFlag
  INTEGER(i4b), INTENT(OUT)  :: ipar1
  INTEGER(i4b), INTENT(OUT)  :: ipar2
  REAL(r8b),    INTENT(OUT)  :: dt1
  REAL(r8b),    INTENT(OUT)  :: dt2

! Local parameter
! ---------------
  REAL(r8b),     PARAMETER   :: dtSim = 1d0/1440d0

! Local Variables
! ---------------
  LOGICAL, DIMENSION(5),SAVE :: first
  INTEGER(i4b)               :: ipar
  REAL(r8b)                  :: hlp


! Offsets + Drifts in NEQ System
! ------------------------------
  IF ( ErpFlag(icrd) == 2 ) THEN
    ipar2 = 0
    DO ipar = 1,neq%misc%npar
      IF (neq%par(ipar)%locq(1) == 10                        .AND. &
          neq%par(ipar)%locq(4) == icrd                      .AND. &
          neq%par(ipar)%locq(5) == 2                         .AND. &
          polTim >= neq%par(ipar)%time%mean -                      &
                    neq%par(ipar)%time%half - dtSim          .AND. &
          polTim <= neq%par(ipar)%time%mean +                      &
                    neq%par(ipar)%time%half + dtSim)  THEN

        ipar2 = ipar
        IF ( .NOT.first(icrd) .AND. neq%par(ipar)%time%mean < polTim ) &
          EXIT

      END IF
    END DO

    IF (ipar2 == 0) THEN
      WRITE(lfnerr,*) ' *** SR WHICHERP: DRIFT NOT FOUND'
      CALL EXITRC(2)
    END IF

    ipar1 = 0
    DO ipar = 1,neq%misc%npar

      IF (neq%par(ipar)%locq(1) == 10                             .AND. &
          neq%par(ipar)%locq(4) == icrd                           .AND. &
          neq%par(ipar)%locq(5) == 1                              .AND. &
          ( ( neq%par(ipar)%time%mean - EPSILON(0.0) <=                   &
             neq%par(ipar2)%time%mean-neq%par(ipar2)%time%half  .AND.   &
          neq%par(ipar)%time%mean + EPSILON(0.0) >=                     &
             neq%par(ipar2)%time%mean-neq%par(ipar2)%time%half )  .OR.  &
          ( neq%par(ipar)%time%mean - EPSILON(0.0) <=                   &
             neq%par(ipar2)%time%mean                           .AND.   &
          neq%par(ipar)%time%mean + EPSILON(0.0) >=                     &
             neq%par(ipar2)%time%mean                          ) ) ) THEN
        ipar1 = ipar

        IF ( neq%par(ipar)%time%mean +  &
             2 * neq%par(ipar2)%time%half -dtSim <= polTim ) THEN
          first(icrd) = .TRUE.
        ELSE
          first(icrd) = .FALSE.
        END IF
        EXIT
      END IF
    END DO

    IF (ipar1 == 0) THEN
      WRITE(lfnerr,*) ' *** SR WHICHERP: OFFSET NOT FOUND'
      CALL EXITRC(2)
    END IF

    dt1 = 1.0d0
    dt2 = polTim - neq%par(ipar1)%time%mean

! Only Offsets in NEQ System (piece-wise linear)
! ----------------------------------------------
  ELSEIF ( ErpFlag(icrd) == 1 ) THEN
    hlp   = 0.d0
    ipar1 = 0
    DO ipar = 1,neq%misc%npar
      IF (neq%par(ipar)%locq(1)         == 10     .AND. &
          neq%par(ipar)%locq(4)         == icrd   .AND. &
          neq%par(ipar)%time%mean-neq%par(ipar)%time%half-dtSim <= polTim .AND. &
          neq%par(ipar)%time%mean-neq%par(ipar)%time%half > hlp           )    THEN
        hlp = neq%par(ipar)%time%mean-neq%par(ipar)%time%half
        ipar1 = ipar
      END IF
    END DO

    IF (ipar1 == 0) THEN
      WRITE(lfnerr,*) ' ### SR WHICHERP: OFFSET 1 NOT FOUND'
    END IF

    hlp = HUGE(0.d0)
    ipar2 = 0
    DO ipar = 1,neq%misc%npar
      IF (neq%par(ipar)%locq(1)         == 10     .AND. &
          neq%par(ipar)%locq(4)         == icrd   .AND. &
          neq%par(ipar)%time%mean+neq%par(ipar)%time%half+dtSim >= polTim .AND. &
          neq%par(ipar)%time%mean+neq%par(ipar)%time%half < hlp           )    THEN
        hlp = neq%par(ipar)%time%mean+neq%par(ipar)%time%half
        ipar2 = ipar
      END IF
    END DO

    IF (ipar2 == 0) THEN
      WRITE(lfnerr,*) ' ### SR WHICHERP: OFFSET 2 NOT FOUND'
    END IF

    IF (ipar1 /= ipar2 .AND. ipar1 /= 0) THEN
      dt1 = ( neq%par(ipar2)%time%mean - polTim ) / &
           ( neq%par(ipar2)%time%mean - neq%par(ipar1)%time%mean )
      dt2 = ( polTim - neq%par(ipar1)%time%mean ) / &
           ( neq%par(ipar2)%time%mean - neq%par(ipar1)%time%mean )
    ELSE
      dt1 = 1.d0
      dt2 = 0.d0
    END IF


! Only Offsets in NEQ System (piece-wise constant)
! ------------------------------------------------
  ELSE
    ipar1  = 0
    ipar2  = 0
    dt1    = 1.d0
    dt2    = 0.d0

    DO ipar = 1,neq%misc%npar
      IF (neq%par(ipar)%locq(1)         == 10     .AND. &
          neq%par(ipar)%locq(4)         == icrd   .AND. &
          neq%par(ipar)%time%mean-neq%par(ipar)%time%half-dtSim <= polTim .AND. &
          neq%par(ipar)%time%mean+neq%par(ipar)%time%half+dtSim >= polTim )  THEN

        IF ( ipar1 == 0 ) THEN
          ipar1 = ipar
        ELSE
          ipar2 = ipar
        END IF

      END IF
    END DO


  END IF

END SUBROUTINE whicherp

END MODULE
