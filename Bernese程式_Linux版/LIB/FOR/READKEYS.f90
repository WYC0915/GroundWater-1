MODULE s_READKEYS
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE readkeys(keyName, keyValue, irc)

! -------------------------------------------------------------------------
! Purpose:    If called for the first time this subroutine reads the
!             standard input file and stores its content into the static
!             array. If called with a non-blank parameter, the subroutine
!             returns the corresponding key value.
!             irc = 0  ... key found
!             irc = 1  ... key not found
!
! Author:     L. Mervart
!
! Created:    28-Mar-2000
! Last mod.:  22-Jan-2004
!
! Changes:    26-Jun-2001 RD: Use alcerr for allocation
!             24-Sep-2001 RD: Give inputFileName back if keyword not found
!             06-Dec-2001 RD: Enable "#..#" for inactive uniline fields
!             16-Aug-2002 LM: Use mygetarg
!             17-Feb-2003 LM: Optional argument forceGetarg,
!                             Solaris compiler bug cured,
!             18-Feb-2003 HU: Dummy keyword DUMMY_KEY
!             10-Mar-2003 LM: Optional argument removed
!             19-Mar-2003 RD: Correct SIZE statement (because of IFC)
!             10-Apr-2003 PS: Test if keyValue is allocated before
!                             deallocation
!             17-Nov-2003 RD: Split into READINPF and READKEYS
!             22-Jan-2004 HB: Change "tempi" into "ii" (SOLARIS Compiler)
!
! SRs called: alcerr
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_inpkey, ONLY: inpkey, myStatus_Run

  USE s_alcerr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input
  CHARACTER(LEN=*), INTENT(IN)                 :: keyName  ! Keyword to get the
                                                           ! values
                                                           ! ' ': inpuFileName
                                                           ! '*': all keywords

! output
  CHARACTER(LEN=keyValueLength), DIMENSION(:),  &
                                 POINTER       :: keyValue ! Value for the
                                                           ! keyword "keyName"
  INTEGER(i4b)                                 :: irc      ! 0: keyword found
                                                           ! 1: keyword not found

! List of functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER                  :: srName = 'readKeys'


! Local Variables
! ---------------
  INTEGER(i4b)                                 :: nlines
  INTEGER(i4b)                                 :: ikey
  INTEGER(i4b)                                 :: iac

#ifdef CMP_SOLARIS
  INTEGER(i4b) :: ii
#endif

! Default (blank) value and default return code
! ---------------------------------------------
  IF (ASSOCIATED(keyValue)) DEALLOCATE(keyValue, stat=iac)

  ALLOCATE( keyValue(1), STAT=iac )
  CALL alcerr(iac, 'keyValue', (/1/), srName)
  keyValue(1) = ''
  irc         = 1

! Reading of input file is still not finished
! -------------------------------------------
  IF (inpKey%status /= myStatus_Run) RETURN

! Return the Name of Input File
! -----------------------------
  IF      (keyName == '') THEN
    keyValue(1) = inpKey%inpFileName
    irc = 0

! Special Action - Return the List of All Keys
! --------------------------------------------
  ELSE IF (keyName == '*') THEN
    DEALLOCATE(keyValue, stat=iac)
    ALLOCATE( keyValue(inpKey%nKeys), stat=iac )
    CALL alcerr(iac, 'keyValue', (/inpKey%nKeys/), srName)
    DO ikey = 1, inpKey%nKeys
      keyValue(ikey) = TRIM(inpKey%keys(ikey)%name)
    END DO
    irc = 0

! Find the value of the key in the local hash table
! -------------------------------------------------
  ELSE
    DO ikey = 1, inpKey%nKeys
      IF (keyName == inpKey%keys(ikey)%name) THEN
        irc    = 0
        nlines = SIZE(inpKey%keys(ikey)%value)
        IF (nlines /= 1) THEN
          DEALLOCATE(keyValue, stat=iac)
          ALLOCATE( keyValue(nlines), stat=iac )
          CALL alcerr(iac, 'keyValue', (/nlines/), srName)
        END IF
#ifdef CMP_SOLARIS
        DO ii = 1, nlines
           keyValue(ii) = inpKey%keys(ikey)%value(ii)
        ENDDO
#else
        keyValue(:) = inpKey%keys(ikey)%value(:)
#endif
        EXIT
      END IF
    END DO
  END IF

! Error Message if Keyword not Found
! ----------------------------------
  IF (irc == 1) THEN
    IF (keyName /= "DUMMY_KEY") THEN
      WRITE(lfnerr,'(/,A,/18X,A,/)')                                    &
      ' *** SR READKEYS: Keyword "' // TRIM(keyName) // '" not found.', &
                        'Input file name:  ' // TRIM(inpKey%inpFileName)
    END IF
  END IF

  RETURN
END SUBROUTINE readkeys


END MODULE
