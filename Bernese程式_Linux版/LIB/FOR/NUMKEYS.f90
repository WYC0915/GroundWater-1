MODULE f_NUMKEYS
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION numkeys(keyName)

! -------------------------------------------------------------------------
! Purpose:    Returns the number of values assigned to a keyword
!             (numkeys = -1  ... key not found)
!
! Author:     R. Dach
!
! Created:    09-Jul-2008
! Last mod.:  09-Jul-2008
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
  USE d_inpkey, ONLY: inpkey, myStatus_Run

  IMPLICIT NONE

! List of Parameters
! ------------------
! input
  CHARACTER(LEN=*), INTENT(IN)          :: keyName  ! Keyword to get the values

! output
  INTEGER(i4b)                          :: numKeys  ! Number of values in
                                                    ! the keyword
                                                    ! -1: keyword not found
! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=7), PARAMETER                  :: srName = 'numKeys'


! Local Variables
! ---------------
  INTEGER(i4b)                                 :: ikey


! Default return code
! -------------------
  numKeys = -1

! Reading of input file is still not finished
! -------------------------------------------
  IF (inpKey%status /= myStatus_Run) RETURN

  DO ikey = 1, inpKey%nKeys
    IF (keyName == inpKey%keys(ikey)%name) THEN
      numKeys = SIZE(inpKey%keys(ikey)%value)
      EXIT
    END IF
  END DO


  RETURN
END FUNCTION numkeys


END MODULE
