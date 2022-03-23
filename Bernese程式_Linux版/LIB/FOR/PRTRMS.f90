MODULE f_prtRms
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.0
! -------------------------------------------------------------------------

FUNCTION prtRms(rms)

! -------------------------------------------------------------------------
! Purpose:    Prints the rms value within 5 character
!
! Author:     R. Dach
!
! Created:    17-Jun-2010
! Last mod.:  17-jun-2010
!
! Changes:
!
! Copyright:  Astronomical Institute
!             University of Berne
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  IMPLICIT NONE

! Variables from parameter list
! -----------------------------
! input:
  REAL(r8b)                             :: rms    ! RMS value to be printed

! output:
  CHARACTER(LEN=5)                      :: prtRms ! Resulting string

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER           :: srName = 'prtRms'

! Local Variables
! ---------------
  CHARACTER(LEN=6)                      :: hlpStr


! Small RMS: directly printed
! ---------------------------
  IF (rms < 999.5d0) THEN
    WRITE(prtRms,'(F5.1)')   rms

! Huge RMS: print in engeneering
! ------------------------------
  ELSE
    WRITE(hlpStr,'(1PD6.0)') rms
    prtRms = hlpStr(2:6)
    prtRms(1:1) = hlpStr(1:1)
  ENDIF

  END FUNCTION prtRms

END MODULE

