MODULE s_PARINT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE parInt(obsWin,dtSim,t_0,dt_0,dt_par,parTxt,nPar,parWin)

! -------------------------------------------------------------------------
! Purpose:    Computation of time interval per parameter
!
! Author:     R. Dach
!
! Created:    24-Mar-2003
!
! Changes:    08-Aug-2005 HB: Use new SR TIMST2 (module)
!             04-May-2012 RD: Use DMOD from module, use m_bern with only
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, &
                      timStrgLength, timStrgLength2
  USE m_time,   ONLY: t_timint
  USE l_basfun, ONLY: dmod

  USE s_alcerr
  USE s_timst2
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  TYPE(t_timint)                :: obsWin  ! Observation window
  REAL(r8b)                     :: dtSim   ! Rounding level for obsWin (in MJD)
  REAL(r8b)                     :: t_0     ! Special start epoch
                                           ! 1d20: Parameter definition wrt UTC
  REAL(r8b)                     :: dt_0    ! Offset wrt UTC-day (in hours)
                                           ! 0d0: not used
  REAL(r8b)                     :: dt_par  ! Length of a parameter (in hours)
  CHARACTER(LEN=*)              :: parTxt  ! Parameter description text

! output:
  INTEGER(i4b)                  :: nPar    ! Number of parameter intervals
  TYPE(t_timint),                &
      DIMENSION(:),  POINTER    :: parWin  ! Parameter interval definition

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6),             PARAMETER :: srName = 'parInt'

  ! Minimum interval length is 1 minute
  REAL(r8b),                    PARAMETER :: minInt = 1d0/60d0

! Local Variables
! ---------------
  CHARACTER(LEN=timStrgLength)            :: epoStr
  CHARACTER(LEN=timStrgLength2)           :: timStr,timSt1

  INTEGER(i4b)                            :: irc

  REAL(r8b)                               :: my_t0
  REAL(r8b)                               :: my_dt0
  REAL(r8b)                               :: my_dtp
  REAL(r8b)                               :: t_first
  REAL(r8b)                               :: t_last

! Copy the input values
! ---------------------
  my_dt0 = DMOD(dt_0/24d0,1d0)
  my_dtp = dt_par/24d0

! Check minimum interval length
! -----------------------------
  IF (dt_par < minInt) THEN
    WRITE(lfnerr,'(/,A,/16X,A,/)')                                &
         ' *** SR PARINT: Interval length for ' // TRIM(parTxt),  &
                         'is shorter than 1 min. The program stop.'
    CALL exitrc(2)

! A special start epoch was given
! -------------------------------
  ELSE IF (t_0 /= 1d20) THEN

    t_first = (DINT((obsWin%t(1)-t_0+dtSim/2d0) / my_dtp)-1d0) * my_dtp + t_0
    t_last  = (DINT((obsWin%t(2)-t_0+dtSim/2d0) / my_dtp)+2d0) * my_dtp + t_0

! Assignement to GPS days
! -----------------------
  ELSE IF (DABS(24d0/dt_par - INT(24d0/dt_par)) < dtSim*24d0/2d0) THEN

    t_first = (DINT((obsWin%t(1)-my_dt0+dtSim/2d0) / my_dtp)-1d0) * my_dtp + my_dt0
    t_last  = (DINT((obsWin%t(2)-my_dt0+dtSim/2d0) / my_dtp)+2d0) * my_dtp + my_dt0

! No assignement to GPS days possible
! -----------------------------------
  ELSE

    my_t0 = DINT(obsWin%t(1)+dtSim/2d0) + my_dt0

    CALL timst2(1,1,my_t0, epoStr)

    WRITE(lfnerr,'(/,A,2(/,16X,A,A),/,16X,A,F12.6,A,/)')                    &
    ' ### SR PARINT: Length of a parameter cannot be assigned to GPS days', &
         'Assumed start epoch: ',TRIM(epoStr),                              &
         'Parameter type:      ',TRIM(parTxt),                              &
         'Length of parameter: ',dt_par,' hours'

    t_first = (DINT((obsWin%t(1)-my_t0+dtSim/2d0) / my_dtp)-1d0) * my_dtp + my_t0
    t_last  = (DINT((obsWin%t(2)-my_t0+dtSim/2d0) / my_dtp)+2d0) * my_dtp + my_t0

  ENDIF

! Bring the parameter boundaries as close as possible
! to the observation boundary
! ---------------------------------------------------
  DO WHILE (t_first+my_dtp-dtSim/2d0 < obsWin%t(1))
    t_first=t_first+my_dtp
  ENDDO

  DO WHILE (t_last-my_dtp+dtSim/2d0 > obsWin%t(2))
    t_last=t_last-my_dtp
  ENDDO

! Compute the number of parameters
! --------------------------------
  nPar = NINT((t_last - t_first)/dt_par*24d0)+1

  ALLOCATE(parWin(nPar),stat=irc)
  CALL alcerr(irc,'parWin',(/nPar/),srName)

  parWin(:)%t(1) = 0d0
  parWin(:)%t(2) = 0d0

! Setup the parameter time windows
! --------------------------------
  nPar = 1
  DO WHILE(parWin(nPar)%t(2)+dtSim/2d0 < obsWin%t(2))

    IF (nPar == 1 .AND. parWin(nPar)%t(1) == 0d0) THEN
      parWin(nPar)%t(1) = t_first
      parWin(nPar)%t(2) = t_first + dt_par/24d0

    ELSE IF (nPar < SIZE(parWin)) THEN
      nPar = nPar+1
      parWin(nPar)%t(1) = parWin(nPar-1)%t(2)
      parWin(nPar)%t(2) = parWin(nPar)%t(1) + dt_par/24d0

    ELSE
      CALL timst2(1,2,obsWin%t,timStr)
      CALL timst2(1,2,(/t_first,t_last/),timSt1)
      WRITE(lfnerr,'(/,A,/,16X,A,2(/,16X,A,A),/,16X,A,F7.1,A,/,16X,A,I5)')  &
      ' *** SR PARINT: Should never happen!!!!',                            &
                      'Too few parameters allocated ('//TRIM(parTxt)//').', &
                      'Observation time interval:',TRIM(timstr),            &
                      'Parameter time interval:  ',TRIM(timst1),            &
                      'Parameter time interval:  ',dt_par,' hours',         &
                      'Comp. number of parameter:',nPar
      CALL exitrc(2)
    ENDIF
  ENDDO

  RETURN
END SUBROUTINE parInt

END MODULE
