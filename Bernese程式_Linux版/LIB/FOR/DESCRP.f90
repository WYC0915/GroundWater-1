MODULE s_DESCRP
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE descrp(keyWord, iLen, optTxt)

! -------------------------------------------------------------------------
! Purpose:    Finds MSG_ or DESCR_ of a keyword, cuts the resuting string
!             to the length iLen
!
! Author:     R. Dach
!
! Created:    12-May-2004
! Last mod.:  12-May-2004
!
! Changes:    __-___-____ __:
!
! SR used:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_inpkey, ONLY: inpKey, myStatus_Run
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)           :: keyWord    ! keyword to be checked
  INTEGER(i4b)               :: iLen       ! Length of the description

! output:
  CHARACTER(LEN=*)           :: optTxt     ! Description string for keyword


! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  INTEGER(i4b)               :: iKey, jKey


! inpKey is not ready for use
! ---------------------------
  IF (inpKey%status < myStatus_Run) THEN
    optTxt = ' '
    RETURN
  ENDIF

! Search for the MSG_/DESCR_-keyword
! ----------------------------------
  jKey = 0
  DO iKey = 1,inpKey%nKeys
    IF (inpKey%keys(iKey)%name == 'MSG_'   // keyWord .OR. &
        inpKey%keys(iKey)%name == 'DESCR_' // keyWord) THEN
      jKey = iKey
      EXIT
    ENDIF
  ENDDO

! No description found
! --------------------
  IF (jKey == 0) THEN
    optTxt = ' '

! Normal description found
! ------------------------
  ELSEIF (LEN_TRIM(inpKey%keys(jKey)%value(1)) <= iLen) THEN
    optTxt = inpKey%keys(jKey)%value(1)

! Description too long
! --------------------
  ELSEIF (iLen > 0 .AND. iLen <= 3) THEN
    optTxt = inpKey%keys(jKey)%value(1)(1:iLen)
  ELSEIF (iLen > 0) THEN
    optTxt = inpKey%keys(jKey)%value(1)(1:iLen-3) // ' ..'

! Illegal iLen
! ------------
  ELSE
    WRITE(lfnerr,'(/,A,I5,/16X,A,/)')                              &
    ' *** SR DESCRP: Illegal length of description string: ',iLen, &
                    'Keyword:  ' // TRIM(keyWord)
    CALL exitrc(2)

  ENDIF

  RETURN
END SUBROUTINE descrp

END MODULE
