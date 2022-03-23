MODULE s_WGTSTA
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE wgtSta(tobs,dtSim,meatyp,stanam,obswgt)

! -------------------------------------------------------------------------
! Purpose:    Get the weight of an observation for a
!             station/measurement type/epoch
!
! Author:     R. Dach
!
! Created:    10-Jun-2002
! Last mod.:  17-May-2003
!
! Changes:    17-May-2003 HU: Initialize structure
!
! SR called:  gtflna,readstwg
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_stawgt, ONLY: t_staWgt, init_stawgt

  USE s_readstwg
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  REAL(r8b)                     :: tobs              ! Observation epoch (MJD)
  REAL(r8b)                     :: dtSim             ! Time interval to
                                                     ! identify observ.
                                                     ! of the same epoch (sec)
  INTEGER(i4b)                  :: meatyp            ! Mesurement type
                                                     ! 1:phase, 2:code, 3:range
  CHARACTER(LEN=*)              :: stanam            ! Station name

! output:
  REAL(r8b)                     :: obswgt            ! Weight of the observation

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6),PARAMETER    :: srName = 'wgtsta'

! Local Variables
! ---------------
  TYPE(t_staWgt), SAVE          :: staWgt

  CHARACTER(LEN=fileNameLength) :: sosFil

  INTEGER(i4b)                  :: iWgt
  INTEGER(i4b),   SAVE          :: ircSos

  LOGICAL     ,   SAVE          :: first = .TRUE.


! Nothing to do
! -------------
  obswgt = 1d0

  IF (first) THEN
    CALL gtflna(0,'STAWGT',sosfil,ircSos)

    IF (LEN_TRIM(sosfil) == 0) ircSos = 1

! Read the station weight file
! ----------------------------
    CALL init_stawgt(staWgt)
    IF (ircSos == 0) CALL readStwg(sosFil,staWgt)

    first=.FALSE.
  ENDIF

  IF (ircSos /= 0) RETURN

! Is there a record for this observation?
! ---------------------------------------
  DO iWgt = 1,staWgt%nWgt

    IF (staWgt%wgt(iWgt)%staNam /= staNam) CYCLE

    IF (staWgt%wgt(iWgt)%meaTyp /= meaTyp) CYCLE

    IF (staWgt%wgt(iWgt)%timWin%t(1) > tObs+dtSim/86400d0) CYCLE

    IF (staWgt%wgt(iWgt)%timWin%t(2) < tObs-dtSim/86400d0) CYCLE

    obsWgt = staWgt%wgt(iWgt)%weight
    EXIT
  ENDDO

!  DEALLOCATE(staWgt%wgt,stat=irc)

  RETURN
END SUBROUTINE wgtSta

END MODULE
