MODULE s_GTFREQ
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE gtfreq(freq,meatyp,satsta,frqnam,epoch,reqfreq)

! -------------------------------------------------------------------------
! Purpose:    Get the value of the requested frequency from the frequency
!             file.
!
! Author:     C. Urschl
!
! Created:    23-Okt-2003
! Last mod.:  __-___-____
!
! Changes:    __-___-____ __:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_frqfil, ONLY: t_freq

  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  TYPE(t_freq), DIMENSION(:)        :: freq   ! Array of freq.info from file
  INTEGER(i4b)                      :: meatyp ! Observation type
                                              !  1 = PHASE
                                              !  2 = CODE
                                              !  3 = RANGE
  CHARACTER(LEN=staNameLength)      :: satsta ! Satellite channel/station name
                                              !    (Phase/Code)      (Range)
  CHARACTER(LEN=2)                  :: frqnam ! Name of frequency
  REAL(r8b)                         :: epoch  ! Epoch

! output:
  REAL(r8b)                         :: reqfreq! Value of requested frequency

! Local parameters
! ----------------
  CHARACTER(LEN=5), DIMENSION(3), PARAMETER :: typstrg = &
    (/'PHASE','CODE ','RANGE'/)

! Local Variables
! ---------------
  INTEGER(i4b)                              :: iline, nline


! Init
! ----
  nline   = SIZE(freq)
  reqfreq = 0D0

! Check meatyp
! ------------
  IF (meatyp < 1 .OR. meatyp > 3) THEN
    WRITE(lfnerr,'(A,I4,/)')' *** SR GTFREQ: Measurement type unknown: ',meatyp
    CALL exitrc(2)
  ENDIF

! Get requested frequency
! -----------------------
  DO iline = 1, nline
    IF (freq(iline)%typstrg   == typstrg(meatyp) .AND. &
        freq(iline)%satsta(1:16) == satsta(1:16) .AND. &
        freq(iline)%name      == frqnam          .AND. &
        freq(iline)%window(1) <= epoch           .AND. &
        freq(iline)%window(2) >= epoch) &
      reqFreq = freq(iline)%value
  ENDDO

  RETURN

END SUBROUTINE gtfreq

END MODULE
