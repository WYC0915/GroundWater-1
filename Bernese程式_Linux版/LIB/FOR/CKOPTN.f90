MODULE s_CKOPTN
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE ckoptn(iStop,keyWord,keyValue,result,srName,descr,ircSum,maxItem)

! -------------------------------------------------------------------------
! Purpose:    Checks the size of keyValue no exceeds maxItem
!
! Remarks:    The following checks are done
!               1. The size of "keyValue" is compared with maxItem. If
!                  the array is bigger "ircSum" is incremented. The size
!                  of the "keyValue" array is given as result.
!
! Author:     R. Dach
!
! Created:    19-Sep-2001
! Last mod.:  21-May-2010
!
! Changes:    02-Oct-2001 RD: modified handling of srName
!             23-Apr-2003 CU: Nullify local pointers
!             13-May-2004 RD: Use MSG_/DESCR_ instead of "descr" (if avail.)
!             21-May-2010 MF: DEALLOCATE(inpFile) in IF block
!
! SR used:    exitrc, readkeys, descrp
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_descrp
  USE s_readkeys
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------

! input:
  INTEGER(i4b)               :: iStop      ! The problem handling is done as
                                           ! 0: warning, 1: error
  CHARACTER(LEN=*)           :: keyWord    ! keyword to be checked
  CHARACTER(LEN=*),           &
    DIMENSION(:)             :: keyValue   ! array of strings to be checked
  CHARACTER(LEN=*)           :: descr      ! description of the parameter
             ! Do not use more than 45 chr here to get a readable error msg.
             ! Ignored if MSG_ or DESCR_ is available in the input file
  INTEGER(i4b)               :: maxItem    ! Maximum number of items allowed
  CHARACTER(LEN=*)           :: srName     ! name of the calling SR or PG

!input/output
  INTEGER(i4b)               :: ircSum     ! error counter;
                                           ! it is incremented only if
                                           ! iStop is set to "error"

! output:
  INTEGER(i4b)               :: result     ! number of items in keyValue

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=16), DIMENSION(0:1), PARAMETER :: errStr = &
  (/ ' ### SR CKOPTN: ', ' *** SR CKOPTN: ' /)

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
           DIMENSION(:),POINTER :: inpFile  ! Name of the input file
  CHARACTER(LEN=shortLineLength),&
           DIMENSION(3)         :: hlpStr
  CHARACTER(LEN=45)             :: optTxt

  INTEGER(i4b)                  :: irc


  NULLIFY(inpFile)

! Program error: iStop = 0 or 1
! -----------------------------
  IF (iStop /= 0 .AND. iStop /= 1) THEN
    WRITE(lfnerr,'(/,A,/,16X,A,/)') &
    ' *** SR CKOPTN: Dear programmer, you did a FATAL ERROR!!!', &
                    'The first parameter has to be "0" or "1".'
    CALL exitrc(2)
  ENDIF

! Check the size of keyValue
! --------------------------
  result = SIZE(keyValue)

  IF (result > maxItem) THEN

    WRITE(lfnerr, '(/,A)') errStr(iStop) // &
          'Too many items found for keyword "' // TRIM(keyWord) // '".'

    CALL readKeys('', inpFile, irc)
    WRITE(lfnerr,'(16X,A,A)')  'Input file:  ',TRIM(inpFile(1))

    WRITE(lfnerr,'(16X,A,A)')  'Called by:   ',TRIM(srName)

    CALL descrp(keyWord,45,optTxt)
    IF (LEN_TRIM(optTxt) == 0) optTxt = descr
    WRITE(lfnerr, '(16X,A,A)') 'Option:      ', TRIM(optTxt)

    WRITE(hlpStr(1),*) result
    WRITE(hlpStr(2),*) maxItem

    DO WHILE (LEN_TRIM(hlpStr(1)) < LEN_TRIM(hlpStr(2)) .OR. &
              LEN_TRIM(hlpStr(1)) < 5)
      hlpStr(3) = ' ' // TRIM(hlpStr(1))
      hlpStr(1) = hlpStr(3)
    ENDDO
    DO WHILE (LEN_TRIM(hlpStr(1)) > LEN_TRIM(hlpStr(2)) .OR. &
              LEN_TRIM(hlpStr(2)) < 5)
      hlpStr(3) = ' ' // TRIM(hlpStr(2))
      hlpStr(2) = hlpStr(3)
    ENDDO

    WRITE(lfnerr,'(16X,A,A)')'Num of items:', TRIM(hlpStr(1))
    WRITE(lfnerr,'(16X,A,A)')'Max of items:', TRIM(hlpStr(2))

    WRITE(lfnerr,*)

    ircSum = ircSum + iStop

    DEALLOCATE(inpFile,stat=irc)
  ENDIF

  RETURN
END SUBROUTINE ckoptn

END MODULE
