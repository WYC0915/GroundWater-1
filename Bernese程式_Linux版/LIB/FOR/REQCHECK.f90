MODULE s_REQCHECK
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE reqcheck(iFil,iPar,neq,req,irCode)

! -------------------------------------------------------------------------
! Purpose:    Check a parameter of the neq for consitance with binning req.
!
! Author:     R. Dach
!
! Created:    06-Nov-2002
! Last mod.:  22-Sep-2005
!
! Changes:    15-Mar-2003 HU: Use OPERATOR at end of line
!             29-Oct-2003 HB: Correct format statement
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!
! SR used:    timst2
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_time,   ONLY: t_timint, OPERATOR(.ISIN.)
  USE d_par,    ONLY: maxLcq
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: t_req,opt

  USE s_timst2
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  INTEGER(i4b)                          :: iFil   ! Neq-file index
  INTEGER(i4b)                          :: iPar   ! Parameter index to check
  TYPE(t_neq)                           :: neq    ! Normal equation
  TYPE(t_req),INTENT(IN)                :: req    ! one request (see P_ADDNEQ)

! output:
  INTEGER(i4b)                          :: irCode ! Return code (0: OK)


! List of functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=25),DIMENSION(4),PARAMETER :: parString =     &
  (/  'unknown type             ','troposphere parameters   ', &
      'troposphere gradients (n)','troposphere gradients (e)' /)

! Local Variables
! ---------------
  TYPE(t_timint)                        :: neqTime
  TYPE(t_req),SAVE                      :: req0

  CHARACTER(LEN=timStrgLength2)         :: neqTimeStr,parTimeStr

  INTEGER(i4b)                          :: ilcq
  INTEGER(i4b), SAVE                    :: iTyp = 0

  REAL(r8b), SAVE                       :: neqTim0

  LOGICAL                               :: doPrint

! Init return code
! ----------------
  irCode = 0

! Get parameter boundaries
! ------------------------
  neqTime%t(1) = neq%par(ipar)%time%mean-neq%par(ipar)%time%half
  neqTime%t(2) = neq%par(ipar)%time%mean+neq%par(ipar)%time%half

! Check the time windows
! ----------------------
  IF ((neqTime%t(2) - req%timint%t(2)) * 86400d0 >= neq%misc%nsmpnq .OR. &
      (req%timint%t(1) - neqTime%t(1)) * 86400d0 >= neq%misc%nsmpnq) THEN

    irCode = 1

! Generate date strings for error message
! ---------------------------------------
    CALL timst2(1,2,neqTime%t,neqTimeStr)
    CALL timst2(1,2,req%timint%t,parTimeStr)

! Check what to print
! -------------------
    doPrint = ((req%timint%t(1) /= req0%timint%t(1)) .OR. &
               (req%timint%t(2) /= req0%timint%t(2)))

    DO ilcq = 1, maxLcq
      doPrint = doPrint .OR. &
                ( req%locq(ilcq) /= req0%locq(ilcq) .AND. &
                  req%locq(ilcq) /= 0 )
    END DO

! First or new request
! --------------------
    IF (iTyp == 0 .OR. doPrint) THEN

      iTyp = 1
      IF (req%locq(1) ==  6 .AND. req%locq(4) == 1) iTyp =  3
      IF (req%locq(1) ==  6 .AND. req%locq(4) == 2) iTyp =  4
      IF (req%locq(1) ==  6 .AND. req%locq(4) == 3) iTyp =  2

      WRITE(lfnerr,'(/,A,3(/,18X,A))')                             &
      ' ### SR REQCHECK: Inconsistent parameter binning for ' //   &
                                           TRIM(parString(iTyp)),  &
      'Normal equation file: ' // TRIM(opt%neqFileName(iFil)),     &
      'New param. interval:  ' // TRIM(parTimeStr),                &
      'NEQ param. interval:  ' // TRIM(neqTimeStr)

      neqTim0 = neq%par(ipar)%time%mean
      req0 = req

! Several neqs have "problems"
! ----------------------------
    ELSE IF (neqTim0 < neq%par(ipar)%time%mean) THEN

      WRITE(lfnerr,'(18X,A)')                                    &
            'NEQ param. interval:  ' // TRIM(neqTimeStr)

      neqTim0 = neq%par(ipar)%time%mean

    ENDIF

  ENDIF ! End inconsistency message

  RETURN
END SUBROUTINE reqcheck

END MODULE
