
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE m_epoch

! -------------------------------------------------------------------------
! Purpose:    This module defines structures, operators and functions for
!             handling epochs given in modified julian date
!
! Author:     L. Mervart
!
! Created:    16-Nov-2004
! Last mod.:  15-Mar-2007
!
! Changes:    15-Jun-2005 HU: Fractions of day instead of seconds
!             05-Aug-2005 HB: Rename procedures: (pgf90)
!                             epochToReal -> epoch2Real
!                             realToEpoch -> real2Epoch
!             15-Mar-2007 MM: t_timWin, several operators, open window
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

! Used modules
  USE m_bern, ONLY: i4b, r8b

! No implicits
  IMPLICIT NONE

! Declare access rights
  PRIVATE
  PUBLIC :: t_epoch, t_timWin, isInWin,                                  &
            ASSIGNMENT(=), OPERATOR(.intersect.),                        &
            OPERATOR(.epochToReal.), OPERATOR(.realToEpoch.),            &
            OPERATOR(+) , OPERATOR(-) , OPERATOR(/), OPERATOR(.TMINUS.), &
            OPERATOR(>) , OPERATOR(>=),                                  &
            OPERATOR(<) , OPERATOR(<=),                                  &
            OPERATOR(==), OPERATOR(/=)


! Parameter declaration
! ---------------------
  INTEGER(i4b), PARAMETER       :: openVal = 999999


! Time epoch
! ----------
  TYPE t_epoch
    INTEGER(i4b) :: day
    REAL(r8b)    :: frac
  END TYPE t_epoch


! Time window
! -----------
  TYPE t_timWin
    TYPE(t_epoch), DIMENSION(2) :: t
  END TYPE t_timWin


! Operator definition
! -------------------
  INTERFACE ASSIGNMENT(=)         ! t_epoch = real, real = t_epoch
    MODULE PROCEDURE epochAsgnReal, realAsgnEpoch
  END INTERFACE

  INTERFACE OPERATOR(+)           ! t_epoch + real    -> t_epoch
    MODULE PROCEDURE addFrac
  END INTERFACE

  INTERFACE OPERATOR(+)           ! t_epoch + t_epoch -> t_epoch
    MODULE PROCEDURE addEpoch
  END INTERFACE

  INTERFACE OPERATOR(+)           ! t_timWin + t_timWin -> t_timWin
    MODULE PROCEDURE addTimWin
  END INTERFACE

  INTERFACE OPERATOR(-)           ! t_epoch - real    -> t_epoch
    MODULE PROCEDURE subFrac
  END INTERFACE

  INTERFACE OPERATOR(-)           ! t_epoch - t_epoch -> real
    MODULE PROCEDURE subEpoch2real
  END INTERFACE

  INTERFACE OPERATOR(.TMINUS.)     ! t_epoch - t_epoch -> t_epoch
    MODULE PROCEDURE subEpoch2epoch
  END INTERFACE

  INTERFACE OPERATOR(.epochToReal.)     ! t_epoch -> real
    MODULE PROCEDURE epoch2Real
  END INTERFACE

  INTERFACE OPERATOR(.realToEpoch.)     ! real -> t_epoch
    MODULE PROCEDURE real2Epoch
  END INTERFACE

  INTERFACE OPERATOR(/)           ! t_epoch / integer -> t_epoch
    MODULE PROCEDURE divide
  END INTERFACE

  INTERFACE OPERATOR(>)
    MODULE PROCEDURE gt
  END INTERFACE

  INTERFACE OPERATOR(>=)
    MODULE PROCEDURE ge
  END INTERFACE

  INTERFACE OPERATOR(<)
    MODULE PROCEDURE lt
  END INTERFACE

  INTERFACE OPERATOR(<=)
    MODULE PROCEDURE le
  END INTERFACE

  INTERFACE OPERATOR(==)
    MODULE PROCEDURE eqEpo, eqWin
  END INTERFACE

  INTERFACE OPERATOR(/=)
    MODULE PROCEDURE neEpo, neWin
  END INTERFACE

  INTERFACE OPERATOR(.intersect.)
    MODULE PROCEDURE intersectWin
  END INTERFACE

CONTAINS


! Auxiliary Subroutine
! --------------------
  SUBROUTINE canonize(epoch)
    TYPE(t_epoch),INTENT(INOUT) :: epoch
    DO WHILE (epoch%frac >= 1.0)
      epoch%day  = epoch%day  + 1
      epoch%frac = epoch%frac - 1
    END DO
    DO WHILE (epoch%frac < 0.0)
      epoch%day  = epoch%day  - 1
      epoch%frac = epoch%frac + 1
    END DO
  END SUBROUTINE


! IsInWin function
! ----------------
  LOGICAL FUNCTION isInWin(epoch, timWin, dT)
    TYPE(t_epoch),  INTENT(IN)           :: epoch
    TYPE(t_timWin), INTENT(IN)           :: timWin
    REAL(r8b),      INTENT(IN), OPTIONAL :: dT
    REAL(r8b)                            :: delta

    IF (PRESENT(dT)) THEN
      delta = dT
    ELSE
      delta = (1.d0/24.d0)/6.d0    ! 10 minutes
    END IF
    IF ( timWin%t(1) - delta <= epoch .AND. &
         timWin%t(2) + delta >= epoch      )  THEN
      isInWin = .TRUE.
    ELSE
      isInWin = .FALSE.
    END IF
  END FUNCTION isInWin


! Operator definitions
! --------------------
  SUBROUTINE epochAsgnReal(epoch,rEpoch)
    TYPE(t_epoch),INTENT(OUT) :: epoch
    REAL(r8b),INTENT(IN)      :: rEpoch
    epoch = .realToEpoch. rEpoch
  END SUBROUTINE epochAsgnReal

  SUBROUTINE realAsgnEpoch(rEpoch,epoch)
    REAL(r8b),INTENT(OUT)      :: rEpoch
    TYPE(t_epoch),INTENT(IN)   :: epoch
    rEpoch = .epochToReal. epoch
  END SUBROUTINE realAsgnEpoch

  TYPE(t_epoch) FUNCTION addFrac(epoch, frac)
    TYPE(t_epoch),INTENT(IN) :: epoch
    REAL(r8b),  INTENT(IN) :: frac
    addFrac%day  = epoch%day
    addFrac%frac = epoch%frac + frac
    CALL canonize(addFrac)
  END FUNCTION

  TYPE(t_epoch) FUNCTION addEpoch(epoch1, epoch2)
    TYPE(t_epoch),INTENT(IN) :: epoch1
    TYPE(t_epoch),INTENT(IN) :: epoch2
    addEpoch%day  = epoch1%day  + epoch2%day
    addEpoch%frac = epoch1%frac + epoch2%frac
    CALL canonize(addEpoch)
  END FUNCTION

  TYPE(t_timWin) FUNCTION addTimWin(timWin1,timWin2)
    TYPE(t_timWin),INTENT(IN)  :: timWin1
    TYPE(t_timWin),INTENT(IN)  :: timWin2

    IF ( timWin1%t(1) < timWin2%t(1) ) THEN
      addTimWin%t(1) = timWin1%t(1)
    ELSE
      addTimWin%t(1) = timWin2%t(1)
    END IF

    IF ( timWin1%t(2) > timWin2%t(2) ) THEN
      addTimWin%t(2) = timWin1%t(2)
    ELSE
      addTimWin%t(2) = timWin2%t(2)
    END IF

  END FUNCTION addTimWin

  TYPE(t_epoch) FUNCTION subFrac(epoch, frac)
    TYPE(t_epoch),INTENT(IN) :: epoch
    REAL(r8b),  INTENT(IN) :: frac
    subFrac%day  = epoch%day
    subFrac%frac = epoch%frac - frac
    CALL canonize(subFrac)
  END FUNCTION

  REAL(r8b) FUNCTION subEpoch2real(epoch1, epoch2)
    TYPE(t_epoch),INTENT(IN) :: epoch1
    TYPE(t_epoch),INTENT(IN) :: epoch2
    TYPE(t_epoch)            :: ans
    ans = epoch1 .TMINUS. epoch2
    subEpoch2real = ans%day + ans%frac
  END FUNCTION

  TYPE(t_epoch) FUNCTION subEpoch2epoch(epoch1, epoch2)
    TYPE(t_epoch),INTENT(IN) :: epoch1
    TYPE(t_epoch),INTENT(IN) :: epoch2
    subEpoch2epoch%day  = epoch1%day  - epoch2%day
    subEpoch2epoch%frac = epoch1%frac - epoch2%frac
    CALL canonize(subEpoch2epoch)
  END FUNCTION

  REAL(r8b) FUNCTION epoch2Real(epoch)
    TYPE(t_epoch),INTENT(IN) :: epoch
    epoch2Real = epoch%day + epoch%frac
    IF (epoch%day == openVal) epoch2Real = 1.d20
  END FUNCTION

  TYPE(t_epoch) FUNCTION real2Epoch(repoch)
    REAL(r8b),INTENT(IN) :: repoch
    IF (repoch == 1.d20) THEN
      real2Epoch%day  = openVal
      real2epoch%frac = 0.d0
    ELSE
      real2Epoch%day  = repoch
      real2Epoch%frac = repoch - real2Epoch%day
    ENDIF
!    CALL canonize(realToEpoch)
  END FUNCTION

  TYPE(t_epoch) FUNCTION divide(epoch, div)
    TYPE(t_epoch),INTENT(IN) :: epoch
    INTEGER(i4b), INTENT(IN) :: div
    divide%day  = epoch%day / div
    divide%frac = (epoch%day - divide%day * div) / div + &
                   epoch%frac / div
  END FUNCTION

  LOGICAL FUNCTION gt(epoch1, epoch2)
    TYPE(t_epoch),INTENT(IN) :: epoch1
    TYPE(t_epoch),INTENT(IN) :: epoch2
    TYPE(t_epoch)            :: hlp1
    TYPE(t_epoch)            :: hlp2
    hlp1 = epoch1; CALL canonize(hlp1)
    hlp2 = epoch2; CALL canonize(hlp2)
    IF      (hlp1%day > hlp2%day) THEN
      gt = .TRUE.
    ELSE IF (hlp1%day < hlp2%day) THEN
      gt = .FALSE.
    ELSE IF (hlp1%frac > hlp2%frac) THEN
      gt = .TRUE.
    ELSE
      gt = .FALSE.
    END IF
  END FUNCTION

  LOGICAL FUNCTION ge(epoch1, epoch2)
    TYPE(t_epoch),INTENT(IN) :: epoch1
    TYPE(t_epoch),INTENT(IN) :: epoch2
    ge = epoch1 > epoch2 .OR. epoch1 == epoch2
  END FUNCTION

  LOGICAL FUNCTION eqEpo(epoch1, epoch2)
    TYPE(t_epoch),INTENT(IN) :: epoch1
    TYPE(t_epoch),INTENT(IN) :: epoch2
    TYPE(t_epoch)            :: hlp1
    TYPE(t_epoch)            :: hlp2
    hlp1 = epoch1; CALL canonize(hlp1)
    hlp2 = epoch2; CALL canonize(hlp2)
    IF (hlp1%day == hlp2%day .AND. hlp1%frac == hlp2%frac) THEN
      eqEpo = .TRUE.
    ELSE
      eqEpo = .FALSE.
    END IF
  END FUNCTION

  LOGICAL FUNCTION eqWin(win1, win2)
    TYPE(t_timWin),INTENT(IN) :: win1
    TYPE(t_timWin),INTENT(IN) :: win2
    IF (win1%t(1) == win2%t(1) .AND. win1%t(2) == win2%t(2)) THEN
      eqWin = .TRUE.
    ELSE
      eqWin = .FALSE.
    END IF
  END FUNCTION eqWin

  LOGICAL FUNCTION lt(epoch1, epoch2)
    TYPE(t_epoch),INTENT(IN) :: epoch1
    TYPE(t_epoch),INTENT(IN) :: epoch2
    lt = .NOT. (epoch1 >= epoch2)
  END FUNCTION

  LOGICAL FUNCTION le(epoch1, epoch2)
    TYPE(t_epoch),INTENT(IN) :: epoch1
    TYPE(t_epoch),INTENT(IN) :: epoch2
    le = .NOT. (epoch1 > epoch2)
  END FUNCTION

  LOGICAL FUNCTION neEpo(epoch1, epoch2)
    TYPE(t_epoch),INTENT(IN) :: epoch1
    TYPE(t_epoch),INTENT(IN) :: epoch2
    neEpo = .NOT. (epoch1 == epoch2)
  END FUNCTION

  LOGICAL FUNCTION neWin(win1, win2)
    TYPE(t_timWin),INTENT(IN) :: win1
    TYPE(t_timWin),INTENT(IN) :: win2
    neWin = .NOT. (win1 == win2)
  END FUNCTION

  LOGICAL FUNCTION intersectWin(timWin1, timWin2)
    TYPE(t_timWin), INTENT(IN) :: timWin1
    TYPE(t_timWin), INTENT(IN) :: timWin2

    IF ( timWin1%t(1) < timWin2%t(2) .AND. &
         timWin1%t(2) > timWin2%t(1)      ) THEN
      intersectWin = .TRUE.
    ELSE
      intersectWin = .FALSE.
    END IF
  END FUNCTION intersectWin

END MODULE m_epoch
