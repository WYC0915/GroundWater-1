MODULE s_GETWAV
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE getwav(meatyp,stanam,frqnam,epoch,wl)

! -------------------------------------------------------------------------
! Purpose:    Get the value of the requested wavelength from the frequency
!             file. Used by GPSEST for RANGE measurements.
!
! Author:     C. Urschl
!
! Created:    14-Nov-2003
! Last mod.:  08-Aug-2005
!
! Changes:    08-Aug-2005 HB: Use new SR TIMST2 (module)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_frqfil, ONLY: t_frqinfo, init_frqfil

  USE s_timst2
  USE s_gtfreq
  USE s_exitrc
  USE s_rdfreq
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                      :: meatyp ! Measuerement type
                                              ! 1: Phase
                                              ! 2: Code
                                              ! 3: Range
  CHARACTER(LEN=staNameLength)      :: stanam ! Satellite channel/station name
                                              !    (Phase/Code)      (Range)
  CHARACTER(LEN=2)                  :: frqnam ! Name of frequency
  REAL(r8b)                         :: epoch  ! Epoch

! output:
  REAL(r8b)                         :: wl     ! Value of requested wavelength [micrometer]

! Local parameters
! ----------------
  LOGICAL, SAVE                     :: first = .TRUE.

! Local Variables
! ---------------
  TYPE(t_frqinfo), SAVE             :: frqinfo
  CHARACTER(LEN=timStrgLength)      :: tstrng


! Read frequency information file once
! ------------------------------------
  IF (first) THEN
    CALL init_frqfil(frqinfo)
    CALL rdfreq(frqinfo)
    first = .FALSE.
  ENDIF

! Get requested frequency (for type RANGE the freq.values are given in nm)
! -----------------------
  CALL gtfreq(frqinfo%freq,meatyp,stanam,frqnam,epoch,wl)

  IF (wl == 0D0) THEN
    CALL timst2(2,1,epoch,tstrng)
    WRITE(LFNERR,'(A,A,/,A,/,3(A,A,/))')                      &
      ' *** SR GETWAV: No frequency found in the frequency ', &
                                              'information',  &
      '                file for the request: ',               &
      '                Station name:   ',stanam,              &
      '                Frequency name: ',frqnam,              &
      '                Epoch:          ',tstrng
    CALL exitrc(2)
  ELSE
    wl = wl/1000D0
  ENDIF

  RETURN


END SUBROUTINE getwav

END MODULE
