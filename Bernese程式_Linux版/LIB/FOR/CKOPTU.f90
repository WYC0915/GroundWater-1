MODULE s_CKOPTU
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE ckoptu(iStop,keyWord, keyString, srName, descr, irc0, ircSum,     &
                  numCol, colTit, iLine, linTit, maxVal,                     &
                  init, error, nError, nValid, result1, nResult, result2)

! -------------------------------------------------------------------------
! Purpose:    extract substrings from uniline record
!
! Remarks:    The following checks are done
!               1. irc0 is the return code of the prev. SR (usually readkeys)
!                  If it is different from zero "ircSum" is incremented, all
!                  "results" are set to the "error" value (if present)
!               2. The size of "result2" resp. the value of "maxVal" is
!                  compared to the size of the input strings "keyString".
!                  If too many input strings are found, all "results" are set
!                  to "error" (if present); ircSum is incremented.
!               3. The input string is converted into "numCol" strings. If this
!                  operation fails a warning/error is used.
!                  In the case of warning (iStop=0) the result is undefined;
!                  in the case of error (iStop=1) the "error" value is set
!                  (if present).
!
! Author:     R. Dach
!
! Created:    21-Sep-2001
!
! Changes:    02-Oct-2001 RD: modified handling of srName
!             29-Jan-2002 HB: Modified some IF-statements because of
!                             compiler problems
!             23-Apr-2003 CU: Nullify local pointers
!             13-May-2004 RD: Use MSG_/DESCR_ instead of "descr" (if avail.)
!             24-Feb-2008 RD: Increase hlpStr from shortLineLength->lineLength
!             19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, lfnerr, keyValueLength, lineLength
  USE s_alcerr
  USE s_descrp
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptn
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)               :: iStop      ! The problem handling is done as
                                           ! 0: warning, 1: error
  CHARACTER(LEN=*)           :: keyWord    ! keyword to be checked
  CHARACTER(LEN=*),           &
       DIMENSION(:)          :: keyString  ! array of strings to be checked
  CHARACTER(LEN=*)           :: descr      ! description of the parameter
             ! Do not use more than 45 chr here to get a readable error msg.
             ! Ignored if MSG_ or DESCR_ is available in the input file
  INTEGER(i4b)               :: irc0       ! readKeys return code;
             ! The checks of this SR are only done if it is equal to "0"
             ! else the result is either the optional error-value (error) or
             ! "something" else.
  INTEGER(i4b)               :: numCol     ! number of strings have to extract
                                           ! from the input string
  CHARACTER(LEN=*)           :: srName     ! name of the calling SR or PG

! input/output:
  INTEGER(i4b)               :: ircSum     ! error counter;
                                           ! it is incremented only if
                                           ! iStop is set to "error"

! optional input:
  CHARACTER(LEN=*), OPTIONAL :: colTit     ! title of the column
  CHARACTER(LEN=*), OPTIONAL :: linTit     ! title of a line
  INTEGER(i4b),     OPTIONAL :: iLine      ! number of a line
  INTEGER(i4b),     OPTIONAL :: maxVal     ! max. number of entries allowed
                                           ! for the input string array
  CHARACTER(LEN=*),           &
    DIMENSION(:),   OPTIONAL :: init       ! used to init the results
  CHARACTER(LEN=*),           &
    DIMENSION(:),   OPTIONAL :: error      ! used if an error found
            ! The size of "init" and "error" have to correspond to "numCol"!

! optional output:
  INTEGER(i4b),     OPTIONAL :: nError     ! number of errors found
  INTEGER(i4b),     OPTIONAL :: nValid     ! number of valid items found
            ! nError / nValid are set independently from the warning/error
            ! setting using "iStop"
  CHARACTER(LEN=*),           &
    DIMENSION(:),   OPTIONAL :: result1    ! only one result value;
            ! To set simple (1-d) array of character variables.
            ! The first valid entry of the strings.
            ! If only errors found either the optional error value or
            ! "something" else.
            ! The size of "result1" have to correspond to "numCol"!
  INTEGER(i4b),     OPTIONAL :: nResult    ! number of valid entries in result2
  CHARACTER(LEN=*),           &
    DIMENSION(:,:), OPTIONAL :: result2    ! array of results from all entries
            ! For every input string the corresponding result is given:
            ! depending on the optional parameters: empty, error, ...
            ! If the optional "init" is set, all entries behind the size of
            ! the keyString array are set to this value.
            ! The size of "result2" have to correspond to "numCol"!


! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=16), DIMENSION(0:1), PARAMETER :: errStr = &
  (/ ' ### SR CKOPTU: ', ' *** SR CKOPTU: ' /)

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
           DIMENSION(:),POINTER :: inpFile  ! Name of the input file
  CHARACTER(LEN=lineLength),&
           DIMENSION(3)         :: hlpStr
  CHARACTER(LEN=keyValueLength), &
       DIMENSION(:,:),POINTER   :: result  ! internal buffer for results
  CHARACTER(LEN=45)             :: optTxt


  INTEGER(i4b)                  :: maxItem, maxLen
  INTEGER(i4b)                  :: iItem, nItem
  INTEGER(i4b),                  &
      DIMENSION(:), ALLOCATABLE :: irCode
  INTEGER(i4b)                  :: errNum
  INTEGER(i4b)                  :: nOK
  INTEGER(i4b)                  :: i1, i2, ii
  INTEGER(i4b)                  :: ircSave
  INTEGER(i4b)                  :: irc, ios


  NULLIFY(inpFile)
  NULLIFY(result)

! A very long description line was given...
! -----------------------------------------
  IF (LEN_TRIM(descr) > 45) &
    WRITE(lfnerr,'(/,A,2(/,16X,A),/,16X,A,I4,A,/,16X,A,/)') &
    ' ### SR CKOPTU: Dear programmer, please give a shorter description',  &
                    'for this option - not more than 45 chr. are possible',&
                    'to get a "nice" error message.',                      &
                    'You have a description with',LEN_TRIM(descr),         &
                    ' characters.','Keyword:  ' // TRIM(keyWord)

! Check the size of the arrays: init
! ----------------------------------
  IF (PRESENT(init)) THEN
    IF (SIZE(init) < numCol) THEN
      WRITE(lfnerr,'(/,A,2(/,16X,A),/)') &
           ' *** SR CKOPTU: Dear programmer, you did a FATAL ERROR!!!',    &
                           'The size of "init" is smaller than "numCol".', &
                           'Keyword:  ' // TRIM(keyWord)
      CALL exitrc(2)
    ENDIF
  ENDIF


! Check the size of the arrays: init
! ----------------------------------
  IF (PRESENT(error)) THEN
    IF (SIZE(error) < numCol) THEN
      WRITE(lfnerr,'(/,A,2(/,16X,A),/)') &
           ' *** SR CKOPTU: Dear programmer, you did a FATAL ERROR!!!',     &
                           'The size of "error" is smaller than "numCol".', &
                           'Keyword:  ' // TRIM(keyWord)
      CALL exitrc(2)
    ENDIF
  ENDIF


! Check the size of the arrays: result1
! -------------------------------------
  IF (PRESENT(result1)) THEN
    IF (SIZE(result1) < numCol) THEN
      WRITE(lfnerr,'(/,A,2(/,16X,A),/)') &
           ' *** SR CKOPTU: Dear programmer, you did a FATAL ERROR!!!',       &
                           'The size of "result1" is smaller than "numCol".', &
                           'Keyword:  ' // TRIM(keyWord)
      CALL exitrc(2)
    ENDIF
  ENDIF


! Check the size of the arrays: result2
! -------------------------------------
  IF (PRESENT(result2)) THEN
    IF (SIZE(result2,1) < numCol) THEN
      WRITE(lfnerr,'(/,A,2(/,16X,A),/)') &
           ' *** SR CKOPTU: Dear programmer, you did a FATAL ERROR!!!',       &
                           'The size of "result2" is smaller than "numCol".', &
                           'Keyword:  ' // TRIM(keyWord)
      CALL exitrc(2)
    ENDIF
  ENDIF


! Init optional output variables
! ------------------------------
  IF (PRESENT(nResult)) nResult = 0
  IF (PRESENT(result1) .AND. PRESENT(init)) result1(1:numCol) = init(1:numCol)
  IF (PRESENT(result2) .AND. PRESENT(init)) THEN
    DO ii=1,SIZE(result2,2)
      result2(1:numCol,ii) = init(1:numCol)
    ENDDO
  ENDIF


! Go back if irc0 not zero (usually readkeys did not work)
! --------------------------------------------------------
  IF (irc0 /= 0) THEN

    ircSum = ircSum + 1

    IF (PRESENT(nValid))  nValid  = 0
    IF (PRESENT(nError))  nError  = 99
    IF (PRESENT(result1) .AND. PRESENT(error)) result1(1:numCol) = error(1:numCol)
    IF (PRESENT(result2) .AND. PRESENT(error))THEN
      DO ii=1,SIZE(result2,2)
        result2(1:numCol,ii) = error(1:numCol)
      ENDDO
    ENDIF

    RETURN

  ENDIF

! Program error: iStop = 0 or 1
! -----------------------------
  IF (iStop /= 0 .AND. iStop /= 1) THEN
    WRITE(lfnerr,'(/,A,2(/,16X,A),/)') &
    ' *** SR CKOPTU: Dear programmer, you did a FATAL ERROR!!!', &
                    'The first parameter has to be "0" or "1".', &
                    'Keyword:  ' // TRIM(keyWord)
    CALL exitrc(2)
  ENDIF


! Check the size of the keyString array
! -------------------------------------
  IF (PRESENT(result2)) THEN
    maxItem = SIZE(result2,2)
  ELSE
    maxItem = SIZE(keyString)
  ENDIF

  IF (PRESENT(maxVal)) THEN
    IF (PRESENT(result2)) THEN
      IF (maxVal > SIZE(result2,2)) THEN
        WRITE(lfnerr,'(/,A,3(/,16X,A),/)') &
             ' *** SR CKOPTU: Dear programmer, you did a FATAL ERROR!!!',          &
                             'The optional parameter "maxVal" is bigger that the', &
                             'size of the "result" array.',                        &
                             'Keyword:  ' // TRIM(keyWord)
        CALL exitrc(2)
      ENDIF
    ENDIF

    IF (maxVal < maxItem) maxItem = maxVal
  ENDIF

  ircSave = ircSum
  CALL ckoptn(1,keyWord,keyString,nItem,srName,descr,ircSum,maxItem)

  IF (ircSave /= ircSum) THEN

    IF (PRESENT(nError)) nError  = maxItem
    IF (PRESENT(nValid)) nValid  = 0
    IF (PRESENT(result1) .AND. PRESENT(error)) result1(1:numCol)   = error(1:numCol)
    IF (PRESENT(result2) .AND. PRESENT(error)) THEN
      DO ii=1,SIZE(result2,2)
        result2(1:numCol,ii) = error(1:numCol)
      ENDDO
    ENDIF

    RETURN
  ENDIF


! Allocate the internal result buffer
! -----------------------------------
  nOK  = 0
  ALLOCATE(result(numCol,nItem),stat=irc)
  CALL alcerr(irc, 'result', (/numCol,nItem/), 'ckoptu')


! Allocate error code array
! -------------------------
  IF (PRESENT(nError)) nError = 0
  IF (PRESENT(nValid)) nValid = 0
  errNum = 0
  ALLOCATE(irCode(nItem), stat=irc)
  CALL alcerr(irc,'irCode',(/nItem/),'ckoptu')
  irCode =  0
  maxLen = -1


! Loop all entries of input string
! --------------------------------
  DO iItem = 1, nItem

    IF (LEN_TRIM(keyString(iItem)) > maxLen) maxLen = LEN_TRIM(keyString(iItem))

! Read the string
! ---------------
    READ(keyString(iItem),*,ioStat=ios) (result(ii,iItem),ii=1,numCol)
    IF (ios /= 0) irCode(iItem) = 1


! Count the number of errors
! --------------------------
    IF (irCode(iItem) > 0)  errNum = errNum + 1

    IF (irCode(iItem) /= 0) ircSum = ircSum + iStop
    IF (irCode(iItem) == 0 .AND. PRESENT(nValid)) nValid = nValid + 1
    IF (irCode(iItem) /= 0 .AND. PRESENT(nError)) nError = nError + 1

    IF (irCode(iItem) == 0) nOK  = nOK  + 1


! Put the values into the result records
! --------------------------------------
    IF (irCode(iItem)*iStop == 0) THEN

      IF (PRESENT(result2)) result2(1:numCol,iItem) = result(:,iItem)

    ELSE IF (PRESENT(error)) THEN

      IF (PRESENT(result2)) result2(1:numCol,iItem) = error(1:numCol)

    ENDIF

  ENDDO


! Give the number of elements back
! --------------------------------
  IF (PRESENT(nResult)) nResult = nItem


! generate "result1"
! ------------------
  IF (PRESENT(result1)) THEN
    IF (nOK > 0) THEN
      DO iItem = 1,nItem
        IF (irCode(iItem) == 0) THEN
          result1(1:numCol) = result(:,iItem)
          EXIT
        ENDIF
      ENDDO
    ELSE IF (iStop == 1 .AND. PRESENT(error)) THEN
      result1(1:numCol) = error(1:numCol)
    ELSE
      result1(1:numCol) = result(:,1)
    ENDIF
  ENDIF


! Generate the error message
! --------------------------
  IF (errNum /= 0) THEN


! Write the start of the Msg.
! ---------------------------
    WRITE(lfnerr, '(/,A)') errStr(iStop) // &
       'A set of string is expected for keyword "' // TRIM(keyWord) // '"'


    CALL readKeys('', inpFile, irc)
    WRITE(lfnerr,'(16X,A,A)') 'Input file:  ',TRIM(inpFile(1))

    WRITE(lfnerr,'(16X,A,A)') 'Called by:   ',TRIM(srName)

    CALL descrp(keyWord,45,optTxt)
    IF (LEN_TRIM(optTxt) == 0) optTxt = descr
    WRITE(lfnerr, '(16X,A)')  'Option:      '//TRIM(optTxt)


! Some optional additional information
! ------------------------------------
    IF (PRESENT(colTit)) &
      WRITE(lfnerr,'(16X,A,A)')   'Column:      ',TRIM(colTit)
    IF (PRESENT(linTit) .AND. nItem == 1) &
      WRITE(lfnerr,'(16X,A,A)')   'Entry for:   ',TRIM(linTit)
    IF (PRESENT(iLine) .AND. nItem == 1)  &
      WRITE(lfnerr,'(16X,A,I5)')  'Line number: ',iLine

! Loop all items with errors
! --------------------------
    DO iItem = 1, nItem

      IF (nItem > 1) THEN
        WRITE(hlpStr(2),*) iItem
        hlpStr(1) = 'in line' // TRIM(hlpStr(2))
      ELSE
        hlpStr(1) = ' '
      ENDIF

      hlpStr(3) = 'Bad value:   ' // TRIM(keyString(iItem))

      IF (LEN_TRIM(TRIM(hlpStr(1))) > 0) THEN
        i1 = 20 + maxLen
        i2 = i1 + LEN_TRIM(TRIM(hlpStr(1))) - 1
        WRITE(hlpStr(3)(i1:i2),'(A)') TRIM(hlpStr(1))
      ENDIF

      WRITE(lfnerr,'(16X,A)')  TRIM(hlpStr(3))

    ENDDO

! Give an add. hint to the error
! ------------------------------
    WRITE(lfnerr,'(16X,A,I5,A)') 'Expected:    ', numCol, ' columns'

    WRITE(lfnerr,'(16X,A,A)') '             ', &
                              'The string could not be extracted!'

  ENDIF

  DEALLOCATE(irCode, stat=irc)
  DEALLOCATE(inpFile,stat=irc)
  DEALLOCATE(result, stat=irc)

  RETURN
END SUBROUTINE ckoptu

END MODULE
