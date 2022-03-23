MODULE s_CKOPTB
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE ckoptb(iStop, keyWords, srName, descr, ircSum,            &
                  colTit, iLine, linTit, init, error, result1, resultL)

! -------------------------------------------------------------------------
! Purpose:    Checks checkbox resp. radiobuttons
!
! Remarks:    The following checks are done
!               1. All "keywords" from the array are read using SR readKeys.
!                  It is expected that all of them are either "0" or "1".
!                  This will be checked using the SR ckopti.
!               2. If the size of "keyWords" is 1 a checkbox is assumed;
!                  the result may be 0 (FALSE) or 1 (TRUE).
!               3. If the size of "keyWords" is >1 a set of radiobuttons is
!                  assumed. It is checked that exactly one item is "1" whereas
!                  all others have to be "0".
!                  The "result1" gives back the index of the "1" in the list
!                  of "keyWords".
!
! Author:     R. Dach
!
! Created:    19-Sep-2001
! Last mod.:  13-May-2004
!
! Changes:    02-Oct-2001 RD: modified handling of srName
!             23-Apr-2003 CU: Nullify local pointers
!             13-May-2004 RD: Use MSG_/DESCR_ instead of "descr" (if avail.)
!
! SR used:    ckopti, exitrc, readkeys, descrp
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
  USE s_ckopti
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)               :: iStop      ! The problem handling is done as
                                           ! 0: warning, 1: error
  CHARACTER(LEN=*),           &
       DIMENSION(:)          :: keyWords   ! list of keywords to be checked
  CHARACTER(LEN=*)           :: descr      ! description of the parameter
             ! Do not use more than 45 chr here to get a readable error msg.
             ! Ignored if MSG_ or DESCR_ is available in the input file
  CHARACTER(LEN=*)           :: srName     ! name of the calling SR or PG

! input/output:
  INTEGER(i4b)               :: ircSum     ! error counter;
                                           ! it is incremented only if
                                           ! iStop is set to "error"

! optional input:
  CHARACTER(LEN=*), OPTIONAL :: colTit     ! title of the column
  CHARACTER(LEN=*), OPTIONAL :: linTit     ! title of a line
  INTEGER(i4b),     OPTIONAL :: iLine      ! number of a line
  INTEGER(i4b),     OPTIONAL :: init       ! used to init the results
  INTEGER(i4b),     OPTIONAL :: error      ! used if an error found

! optional output:
  INTEGER(i4b),     OPTIONAL :: result1    ! number of keywords in list which
                                           ! was selected
  LOGICAL,          OPTIONAL :: resultL    ! true if a "1" result was found


! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=16), DIMENSION(0:1), PARAMETER :: errStr = &
  (/ ' ### SR CKOPTB: ', ' *** SR CKOPTB: ' /)

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
       DIMENSION(:)  , POINTER  :: keyValue
  CHARACTER(LEN=keyValueLength), &
           DIMENSION(:),POINTER :: inpFile  ! Name of the input file
  CHARACTER(LEN=shortLineLength),&
           DIMENSION(3)         :: hlpStr
  CHARACTER(LEN=45)             :: descr1
  CHARACTER(LEN=45)             :: optTxt

  INTEGER(i4b)                  :: valSum
  INTEGER(i4b)                  :: iKey
  INTEGER(i4b)                  :: iVal
  INTEGER(i4b)                  :: irc, irCode


  NULLIFY(keyValue)
  NULLIFY(inpFile)

! A very long description line was given...
! -----------------------------------------
  descr1 = descr
  IF (LEN_TRIM(descr) > 45) &
    WRITE(lfnerr,'(/,A,2(/,16X,A),/,16X,A,I4,A,/,16X,A,/)') &
    ' ### SR CKOPTB: Dear programmer, please give a shorter description',  &
                    'for this option - not more than 45 chr. are possible',&
                    'to get a "nice" error message.',                      &
                    'You have a description with',LEN_TRIM(descr),         &
                    ' characters.','Keyword 1: ' // TRIM(keyWords(1))


! Program error: iStop = 0 or 1
! -----------------------------
  IF (iStop /= 0 .AND. iStop /= 1) THEN
    WRITE(lfnerr,'(/,A,/,16X,A,/)') &
    ' *** SR CKOPTB: Dear programmer, you did a FATAL ERROR!!!', &
                    'The first parameter has to be "0" or "1".'
    CALL exitrc(2)
  ENDIF

! Init varaibles
! --------------
  IF (PRESENT(result1) .AND. SIZE(keyWords) == 1) &
      result1 = 0                           ! checkbox assumed
  IF (PRESENT(result1) .AND. PRESENT(init)) result1 = init
  IF (PRESENT(resultL)) resultL = .FALSE.
  irCode = ircSum
  valSum = 0

! Only "0" or "1" is allowed for the entries
! ------------------------------------------
  DO iKey = 1, SIZE(keyWords)

    CALL readkeys(keyWords(iKey),keyValue,irc)

    CALL ckopti(iStop,keyWords(iKey),keyValue,srName,descr1,irc,ircSum, &
                maxVal=1,ge=0,le=1,error=0,result1=iVal)

    IF (iVal /= 0) THEN

      IF (PRESENT(result1)) result1 = iKey
      IF (PRESENT(resultL)) resultL = .TRUE.
      valSum = valSum + 1

    ENDIF

  ENDDO

! One or more errors reading the values
! -------------------------------------
  IF (irCode /= ircSum) THEN

    IF (PRESENT(result1) .AND. PRESENT(error)) result1 = error
    IF (PRESENT(resultL)) resultL = .FALSE.
    RETURN

  ENDIF

! Checkbox (result = 0 allowed)
! -----------------------------
  IF (valSum == 0 .AND. SIZE(keyWords) == 1) RETURN

! Only one item was selected
! --------------------------
  IF (valSum == 1) RETURN

! Give the error values back
! --------------------------
  IF (iStop == 1) THEN

    IF (PRESENT(result1) .AND. PRESENT(error)) result1 = error
    IF (PRESENT(resultL)) resultL = .FALSE.
    ircSum = ircSum + iStop

  ENDIF

! Write the error message
! -----------------------
  IF (valSum > 1) THEN
    WRITE(lfnerr, '(/,A)') errStr(iStop) // &
          'More than one item was selected for a set of radiobottons.'
  ELSE
    WRITE(lfnerr, '(/,A)') errStr(iStop) // &
          'No item was selected for a set of radiobottons.'
  ENDIF

  CALL readKeys('', inpFile, irc)
  WRITE(lfnerr,'(16X,A,A)') 'Input file:  ',TRIM(inpFile(1))

  WRITE(lfnerr,'(16X,A,A)') 'Called by:   ',TRIM(srName)

  CALL descrp(keyWords(1),45,optTxt)
  IF (LEN_TRIM(optTxt) == 0) optTxt = descr
  WRITE(lfnerr, '(16X,A)')  'Option:      '//TRIM(optTxt)

! Some optional additional information
! ------------------------------------
  IF (PRESENT(colTit)) &
    WRITE(lfnerr,'(16X,A,A)')   'Column:      ',TRIM(colTit)
  IF (PRESENT(linTit)) &
    WRITE(lfnerr,'(16X,A,A)')   'Entry for:   ',TRIM(linTit)
  IF (PRESENT(iLine))  &
    WRITE(lfnerr,'(16X,A,I5)')  'Line number: ',iLine


! List all keywords and entries
! -----------------------------
  WRITE(hlpStr(1),*) SIZE(keyWords)

  DO iKey = 1, SIZE(keyWords)

    WRITE(hlpStr(2),*) iKey
    DO WHILE (LEN_TRIM(hlpStr(2)) < LEN_TRIM(hlpStr(1)))
      hlpStr(3) = ' ' // TRIM(hlpStr(2))
      hlpStr(2) = hlpStr(3)
    ENDDO

    CALL readkeys(keyWords(iKey),keyValue,irc)

    hlpStr(3) = 'Item' // TRIM(hlpStr(2)) // ':'
    WRITE(lfnerr,'(16X,A,A)') hlpStr(3)(1:13), &
          TRIM(keyWords(iKey)) // ' = ' // TRIM(keyValue(1))
  ENDDO

  WRITE(lfnerr,*)

  DEALLOCATE(keyValue,stat=irc)
  DEALLOCATE(inpFile,stat=irc)

  RETURN
END SUBROUTINE ckoptb

END MODULE
