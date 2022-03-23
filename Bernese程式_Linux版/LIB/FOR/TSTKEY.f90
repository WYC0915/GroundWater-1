MODULE f_tstkey
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION tstKey(keyWord)

! -------------------------------------------------------------------------
! Purpose:    Checks the list of keywords in the input file for a given
!             keyword
!
! Author:     R. Dach
!
! Created:    18-Apr-2002
! Last mod.:  17-Nov-2003
!
! Changes:    23-May-2002 RD: Nullify the pointer
!             17-Nov-2003 RD: New algorithm, prevent "Panic-Loop"
!
! SR called:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_inpKey, ONLY: inpKey, myStatus_Run

  IMPLICIT NONE

! List of Parameters
! ------------------
! input
  CHARACTER(LEN=*) :: keyWord

! output
  LOGICAL          :: tstKey

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  INTEGER(i4b)     :: iKey

  LOGICAL          :: found


! Is keyword in this list?
! ------------------------
  found = .FALSE.
  DO iKey = 1, inpKey%nKeys

    IF (inpKey%status /= myStatus_Run) EXIT

    IF (keyWord == inpKey%keys(iKey)%name) found = .TRUE.

  ENDDO

! Give the result
! ---------------
  tstKey = found

  RETURN
END FUNCTION tstKey

END MODULE
