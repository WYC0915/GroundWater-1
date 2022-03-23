MODULE s_READ1KEY
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE read1key(fileName, keyName, keyValue, irc)

! -------------------------------------------------------------------------
! Purpose:    This subroutine reads one key from a given input file
!             irc = 0  ... key found
!             irc = 1  ... key not found
!
! Author:     L. Mervart
!
! Created:    27-AUG-00
!
! Changes:    26-JUN-01 RD: Use alcerr for allocation
!             16-Apr-03 RD: No error for keywords with nore than one entry...
!             28-Jun-05 MM: Unused variables removed
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_inpKey, ONLY: t_inpKey, init_inpkey, myStatus_Run

  USE s_alcerr
  USE s_readinpf
  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=*)             , INTENT(IN)            :: fileName
  CHARACTER(LEN=*)             , INTENT(IN)            :: keyName
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  INTEGER(i4b)                                         :: irc

! Local Variables
! ---------------
  INTEGER(i4b)                                 :: nlines
  INTEGER(i4b)                                 :: ios
  INTEGER(i4b)                                 :: ii

  TYPE(t_inpKey)                               :: myInpKey
  INTEGER(i4b)                                 :: iKey

! Default (blank) value and default return code
! ---------------------------------------------
  DEALLOCATE(keyValue, STAT=ios)
  ALLOCATE( keyValue(1), STAT=ios )
  CALL alcerr( ios, 'keyValue', (/1/), 'read1key')
  keyValue(1) = ''
  irc         = 1

! Read the input file
! -------------------
  CALL init_inpkey(myInpKey)
  CALL readinpf(fileName,myInpKey)

! Find the keyword
! ----------------
  IF (myInpKey%status == myStatus_Run) THEN
    DO iKey = 1,myInpKey%nKeys
      IF (myInpKey%keys(iKey)%name /= keyName) CYCLE

! Resize keyvalue, if neccessary
! ------------------------------
      nLines = SIZE(myInpKey%keys(iKey)%value)
      IF (nLines > 1) THEN
        DEALLOCATE(keyValue, STAT=ios)
        ALLOCATE( keyValue(nLines), STAT=ios )
        CALL alcerr( ios, 'keyValue', (/nLines/), 'read1key')
      ENDIF

! Put the value into the keyvalue
! -------------------------------
      DO ii = 1,nLines
        keyValue(ii) = myInpKey%keys(iKey)%value(ii)
      ENDDO

      irc = 0

    ENDDO
  ENDIF

! Deallocate the privat buffer
! ----------------------------
  IF (ASSOCIATED(myInpKey%keys)) DEALLOCATE(myInpKey%keys,stat=irc)

END SUBROUTINE read1key


END MODULE
