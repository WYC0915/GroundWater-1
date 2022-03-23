MODULE s_CKOPTZ
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE ckoptz(iStop,keyWord, keyString, srName, descr, irc0, ircSum, &
                  empty, ge, le, gt, lt, colTit, iLine, linTit, maxVal,  &
                  init, error, nError, nValid, result1, nResult, result2)

! -------------------------------------------------------------------------
! Purpose:    Checks the input option as a date+time value
!
! Remarks:    The following checks are done
!               1. irc0 is the return code of the prev. SR (usually readkeys)
!                  If it is different from zero "ircSum" is incremented, all
!                  "results" are set to the "error" value (if present)
!               2. The size of "result2" resp. the value of "maxVal" is
!                  compared to the size of the input strings "keyString".
!                  If too many input strings are found, all "results" are set
!                  to "error" (if present); ircSum is incremented.
!               3. If the input string is empty either the optional "empty"
!                  value is assumed as result. If the "empty" value is not
!                  present a warning/error is used.
!                  In the case of warning (iStop=0) the result is undefined;
!                  in the case of error (iStop=1) the "error" value is set
!                  (if present).
!               4. The string is decoded with 6 integers. The 1st one has to be
!                  a 4-digit year. These integer values are transformed
!                  into MJD numbers (real).
!                  If the reading of the integer numbers fails or the 1st value
!                  is not a 4-digit year a warning/error is set (see 3).
!               5. The MJD result is taken and the range is checked using the
!                  optional "ge", "gt", "le", "lt" values (if present).
!                  If the MJD result is outside the range a warning/error is
!                  set (see 3).
!
! Author:     R. Dach
!
! Created:    22-Oct-2001
!
! Changes:    29-Jan-2002 HB: Modified some IF-statements because of
!                             compiler problems
!             23-Apr-2003 CU: Nullify local pointers
!             13-May-2004 RD: Use MSG_/DESCR_ instead of "descr" (if avail.)
!             19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, &
                      keyValueLength, shortLineLength
  USE s_alcerr
  USE s_descrp
  USE f_djul
  USE s_readkeys
  USE s_exitrc
  USE f_iyear4
  USE s_jmt
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
  CHARACTER(LEN=*)           :: srName     ! name of the calling SR or PG

! input/output:
  INTEGER(i4b)               :: ircSum     ! error counter;
                                           ! it is incremented only if
                                           ! iStop is set to "error"

! optional input:
  REAL(r8b),        OPTIONAL :: empty      ! result if string is empty
  REAL(r8b),        OPTIONAL :: ge         ! min. value accepted
  REAL(r8b),        OPTIONAL :: gt         ! min. value accepted
  REAL(r8b),        OPTIONAL :: le         ! max. value accepted
  REAL(r8b),        OPTIONAL :: lt         ! max. value accepted
  CHARACTER(LEN=*), OPTIONAL :: colTit     ! title of the column
  CHARACTER(LEN=*), OPTIONAL :: linTit     ! title of a line
  INTEGER(i4b),     OPTIONAL :: iLine      ! number of a line
  INTEGER(i4b),     OPTIONAL :: maxVal     ! max. number of entries allowed
                                           ! for the input string array
  REAL(r8b),        OPTIONAL :: init       ! used to init the results
  REAL(r8b),        OPTIONAL :: error      ! used if an error found

! optional output:
  INTEGER(i4b),     OPTIONAL :: nError     ! number of errors found
  INTEGER(i4b),     OPTIONAL :: nValid     ! number of valid items found
            ! nError / nValid are set independently from the warning/error
            ! setting using "iStop"
  REAL(r8b),        OPTIONAL :: result1    ! only one result value;
            ! To set simple real variables (MJD corresponding to keyString).
            ! The first valid entry of the strings.
            ! If only errors found either the optional error value or
            ! "something" else.
  INTEGER(i4b),     OPTIONAL :: nResult    ! number of entries in result2
  REAL(r8b),                  &
      DIMENSION(:), OPTIONAL :: result2    ! array of results from all entries
                                           ! (MJD corresponding to keyString)
            ! For every input string the corresponding result is given:
            ! depending on the optional parameters: empty, error, ...
            ! If the optional "init" is set, all entries behind the size of
            ! the keyString array are set to this value.

! Local Parameters
! ----------------
  CHARACTER(LEN=16), DIMENSION(0:1), PARAMETER :: errStr = &
  (/ ' ### SR CKOPTZ: ', ' *** SR CKOPTZ: ' /)

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
           DIMENSION(:),POINTER :: inpFile  ! Name of the input file
  CHARACTER(LEN=shortLineLength),&
           DIMENSION(5)         :: hlpStr
  CHARACTER(LEN=45)             :: optTxt

  INTEGER(i4b)                  :: maxItem, maxLen
  INTEGER(i4b)                  :: iItem, nItem
  INTEGER(i4b), DIMENSION(6)    :: zeit
  INTEGER(i4b),                  &
      DIMENSION(:), ALLOCATABLE :: irCode
  INTEGER(i4b), DIMENSION(0:4)  :: errNum
  INTEGER(i4b)                  :: iErr
  INTEGER(i4b)                  :: nOK
  INTEGER(i4b)                  :: ii, jj, i1, i2
  INTEGER(i4b)                  :: ircSave
  INTEGER(i4b)                  :: irc, ios

  REAL(r8b),                     &
         DIMENSION(:),POINTER   :: result  ! internal buffer for results
  REAL(r8b), DIMENSION(6)       :: rHlp

  NULLIFY(inpFile)
  NULLIFY(result)

! A very long description line was given...
! -----------------------------------------
  IF (LEN_TRIM(descr) > 45) &
    WRITE(lfnerr,'(/,A,2(/,16X,A),/,16X,A,I4,A,/,16X,A,/)') &
    ' ### SR CKOPTZ: Dear programmer, please give a shorter description',  &
                    'for this option - not more than 45 chr. are possible',&
                    'to get a "nice" error message.',                      &
                    'You have a description with',LEN_TRIM(descr),         &
                    ' characters.','Keyword:  ' // TRIM(keyWord)

! Init optional output variables
! ------------------------------
  IF (PRESENT(nResult)) nResult = 0
  IF (PRESENT(result1) .AND. PRESENT(init)) result1 = init
  IF (PRESENT(result2) .AND. PRESENT(init)) result2 = init


! Go back if irc0 not zero (usually readkeys did not work)
! --------------------------------------------------------
  IF (irc0 /= 0) THEN

    ircSum = ircSum + 1

    IF (PRESENT(nValid))  nValid  = 0
    IF (PRESENT(nError))  nError  = 99
    IF (PRESENT(result1) .AND. PRESENT(error)) result1 = error
    IF (PRESENT(result2) .AND. PRESENT(error)) result2 = error

    RETURN

  ENDIF


! Program error: iStop = 0 or 1
! -----------------------------
  IF (iStop /= 0 .AND. iStop /= 1) THEN
    WRITE(lfnerr,'(/,A,2(/,16X,A),/)') &
    ' *** SR CKOPTZ: Dear programmer, you did a FATAL ERROR!!!', &
                    'The first parameter has to be "0" or "1".', &
                    'Keyword:  ' // TRIM(keyWord)
    CALL exitrc(2)
  ENDIF

! Program error: ge and gt given!
! -------------------------------
  IF (PRESENT(ge) .AND. PRESENT(gt)) THEN
    WRITE(lfnerr,'(/,A,2(/,16X,A),/)') &
    ' *** SR CKOPTZ: Dear programmer, you did a FATAL ERROR!!!', &
                    'You have specified the optional arguments "ge" and "gt"',&
                    'in the same call of the subroutine.'
    CALL exitrc(2)
  ENDIF

! Program error: le and lt given!
! -----------------------------
  IF (PRESENT(le) .AND. PRESENT(lt)) THEN
    WRITE(lfnerr,'(/,A,2(/,16X,A),/)') &
    ' *** SR CKOPTZ: Dear programmer, you did a FATAL ERROR!!!', &
                    'You have specified the optional arguments "le" and "lt"',&
                    'in the same call of the subroutine.'
    CALL exitrc(2)
  ENDIF

! Check the size of the keyString array
! -------------------------------------
  IF (PRESENT(result2)) THEN
    maxItem = SIZE(result2)
  ELSE
    maxItem = SIZE(keyString)
  ENDIF

  IF (PRESENT(maxVal)) THEN
    IF (PRESENT(result2)) THEN
      IF (maxVal > SIZE(result2)) THEN
        WRITE(lfnerr,'(/,A,3(/,16X,A),/)') &
        ' *** SR CKOPTZ: Dear programmer, you did a FATAL ERROR!!!',          &
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

    IF (PRESENT(nValid)) nValid  = 0
    IF (PRESENT(nError)) nError  = maxItem
    IF (PRESENT(result1) .AND. PRESENT(error)) result1 = error
    IF (PRESENT(result2) .AND. PRESENT(error)) result2 = error

    RETURN
  ENDIF


! Allocate the internal result buffer
! -----------------------------------
  nOK  = 0
  ALLOCATE(result(nItem),stat=irc)
  CALL alcerr(irc, 'result', (/nItem/), 'ckoptz')


! Allocate error code array
! -------------------------
  IF (PRESENT(nError)) nError = 0
  IF (PRESENT(nValid)) nValid = 0
  errNum = 0
  ALLOCATE(irCode(nItem), stat=irc)
  CALL alcerr(irc,'irCode',(/nItem/),'ckoptz')
  irCode =  0
  maxLen = -1


! Loop all entries of input string
! --------------------------------
  DO iItem = 1, nItem

    IF (LEN_TRIM(keyString(iItem)) > maxLen) maxLen = LEN_TRIM(keyString(iItem))


! Is the input string empty?
! --------------------------
    IF (LEN_TRIM(keyString(iItem)) == 0) THEN

      IF (PRESENT(empty)) THEN
        result(iItem) = empty
      ELSE
        irCode(iItem) = 1
      ENDIF

    ENDIF


! Convert the string into a date
! ------------------------------
    IF (LEN_TRIM(keyString(iItem)) > 0 .AND. irCode(iItem) == 0) THEN

      READ(keyString(iItem),*,ioStat=ios) (zeit(ii),ii=1,6)
      IF (ios == 0 .AND. zeit(1) == IYEAR4(zeit(1))) THEN
        result(iItem) = djul(zeit(1), zeit(2), DBLE(zeit(3)) + &
                  (DBLE(zeit(4))+(DBLE(zeit(5))+DBLE(zeit(6))/60d0)/60d0)/24d0)
      ELSE IF (ios /= 0) THEN
        irCode(iItem) = 2
      ELSE IF (zeit(1) == IYEAR4(zeit(1))) THEN
        irCode(iItem) = 4
      ENDIF

    ENDIF


! Check the range
! ---------------
    IF (LEN_TRIM(keyString(iItem)) > 0 .AND. irCode(iItem) == 0) THEN

      IF (PRESENT(GE)) THEN
        IF (result(iItem) <  ge) irCode(iItem) = 3
      ENDIF
      IF (PRESENT(GT)) THEN
        IF (result(iItem) <= gt) irCode(iItem) = 3
      ENDIF
      IF (PRESENT(LE)) THEN
        IF (result(iItem) >  le) irCode(iItem) = 3
      ENDIF
      IF (PRESENT(LT)) THEN
        IF (result(iItem) >= lt) irCode(iItem) = 3
      ENDIF
    ENDIF


! Count the number of errors
! --------------------------
    IF (irCode(iItem) >= 0 .AND. irCode(iItem) < SIZE(errNum)) &
      errNum(irCode(iItem)) = errNum(irCode(iItem)) + 1

    IF (irCode(iItem) /= 0) ircSum = ircSum + iStop
    IF (irCode(iItem) == 0 .AND. PRESENT(nValid)) nValid = nValid + 1
    IF (irCode(iItem) /= 0 .AND. PRESENT(nError)) nError = nError + 1

    IF (irCode(iItem) == 0) nOK  = nOK  + 1

! Put the values into the result records
! --------------------------------------
    IF (irCode(iItem)*iStop == 0) THEN

      IF (PRESENT(result2)) result2(iItem) = result(iItem)

    ELSE IF (PRESENT(error)) THEN

      IF (PRESENT(result2)) result2(iItem) = error

    ENDIF

  ENDDO


! Give the number of accepted elements back
! -----------------------------------------
  IF (PRESENT(nResult)) nResult = nItem


! generate "result1"
! ------------------
  IF (PRESENT(result1)) THEN
    IF (nOK > 0) THEN
      DO iItem = 1,nItem
        IF (irCode(iItem) == 0) THEN
          result1 = result(iItem)
          EXIT
        ENDIF
      ENDDO
    ELSE IF (iStop == 1 .AND. PRESENT(error)) THEN
      result1 = error
    ELSE
      result1 = result(1)
    ENDIF
  ENDIF



! Generate the error message
! --------------------------
  DO iErr = 1, 4

    IF (errNum(iErr) == 0) CYCLE


! Start the error message
! -----------------------
    IF (iErr == 1 .OR. iErr == 2 .OR. iErr == 4) &
      WRITE(lfnerr, '(/,A)') errStr(iStop) // &
           'A date string is expected for keyword "' // TRIM(keyWord) // '"'

    IF (iErr == 3) WRITE(lfnerr, '(/,A)') errStr(iStop) // &
       'The date for keyword "' // TRIM(keyWord) // '" exceeds the range.'


    CALL readKeys('', inpFile, irc)
    WRITE(lfnerr,'(16X,A,A)') 'Input file:  ',TRIM(inpFile(1))

    WRITE(lfnerr,'(16X,A,A)') 'Called by:   ',TRIM(srName)

    CALL descrp(keyWord,45,optTxt)
    IF (LEN_TRIM(optTxt) == 0) optTxt = descr
    WRITE(lfnerr,'(16X,A)')   'Option:      '//TRIM(optTxt)


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

      IF (iErr /= irCode(iItem)) CYCLE

      IF (nItem > 1) THEN
        WRITE(hlpStr(2),*) iItem
        hlpStr(1) = 'in line' // TRIM(hlpStr(2))
      ELSE
        hlpStr(1) = ' '
      ENDIF

      IF (iErr == 1) &
        WRITE(lfnerr,'(16X,A)')  'Bad value:   " "  ' // TRIM(hlpStr(1))

      IF (iErr == 2 .OR. iErr == 4) THEN
        hlpStr(3) = 'Bad value:   " ' // TRIM(keyString(iItem)) // ' "'

        IF (LEN_TRIM(TRIM(hlpStr(1))) > 0) THEN
          i1 = 20 + maxLen
          i2 = i1 + LEN_TRIM(TRIM(hlpStr(1))) - 1
          WRITE(hlpStr(3)(i1:i2),'(A)') TRIM(hlpStr(1))
        ENDIF

        WRITE(lfnerr,'(16X,A)')  TRIM(hlpStr(3))
      ENDIF

      IF (iErr == 3) THEN
        CALL jmt(result(iItem), zeit(1), zeit(2), rHlp(3))
        zeit(3) = NINT(rHlp(3))
        rHlp(4) = DNINT((rHlp(3)-DBLE(zeit(3)))*24d0)
        zeit(4) = NINT(rHlp(4))
        rHlp(5) = DNINT((rHlp(4)-DBLE(zeit(4)))*60d0)
        zeit(5) = NINT(rHlp(5))
        rHlp(6) = DNINT((rHlp(5)-DBLE(zeit(5)))*60d0)
        zeit(6) = NINT(rHlp(6))

        hlpStr(2) = ' '
        IF (zeit(1) < 10000d0) THEN
          WRITE(hlpStr(2),'(1X,I4,2(1X,I2.2),I3,2(1X,I2.2))') (zeit(ii),ii=1,6)
        ELSE
          WRITE(hlpStr(2),*) zeit(1)
          i1 = LEN_TRIM(hlpStr(2)) + 1
          i2 = i1 + 6
          WRITE(hlpStr(2)(i1:i2),'(2(1X,I2.2),I3,2(1X,I2.2))') &
                (zeit(ii), ii=2,6)
        ENDIF

        hlpStr(3) = 'Bad value:   "' // TRIM(hlpStr(2)) // ' "'

        IF (LEN_TRIM(TRIM(hlpStr(1))) > 0) THEN
          i1 = 20 + maxLen
          i2 = i1 + LEN_TRIM(TRIM(hlpStr(1))) - 1
          WRITE(hlpStr(3)(i1:i2),'(A)') TRIM(hlpStr(1))
        ENDIF

        WRITE(lfnerr,'(16X,A)')  TRIM(hlpStr(3))
      ENDIF

    ENDDO

! Give an add. hint to the error
! ------------------------------
    IF (iErr == 1) WRITE(lfnerr,'(16X,A,A)') '             ', &
                         'An empty string is not allowed here!'

    IF (iErr == 2) THEN
      WRITE(lfnerr,'(16X,A,A)') '             ', &
                         'The string could not be converted into a date!'
      WRITE(lfnerr,'(16X,A,A)') '             ', &
                         'Expected:    " yyyy mm dd "  Format'
    ENDIF

    IF (iErr == 4) WRITE(lfnerr,'(16X,A,A)') '             ', &
                         'A 4-digit year is expected for the date!'


! Give the correct range
! ----------------------
    IF (iErr == 3) THEN

      IF (PRESENT(GE)) rHlp(1) = ge
      IF (PRESENT(GT)) rHlp(1) = gt

      IF (PRESENT(LE)) rHlp(2) = le
      IF (PRESENT(LT)) rHlp(2) = lt


      DO ii = 1,2
        IF (ii == 1 .AND. .NOT. (PRESENT(GE) .OR. PRESENT(GT))) CYCLE
        IF (ii == 2 .AND. .NOT. (PRESENT(LE) .OR. PRESENT(LT))) CYCLE

        CALL jmt(rHlp(ii), zeit(1), zeit(2), rHlp(3))
        zeit(3) = NINT(rHlp(3))
        rHlp(4) = DNINT((rHlp(3)-DBLE(zeit(3)))*24d0)
        zeit(4) = NINT(rHlp(4))
        rHlp(5) = DNINT((rHlp(4)-DBLE(zeit(4)))*60d0)
        zeit(5) = NINT(rHlp(5))
        rHlp(6) = DNINT((rHlp(5)-DBLE(zeit(5)))*60d0)
        zeit(6) = NINT(rHlp(6))

        hlpStr(ii) = ' '
        IF (zeit(1) < 10000d0) THEN
          WRITE(hlpStr(ii),'(1X,I4,2(1X,I2.2),I3,2(1X,I2.2))') (zeit(jj),jj=1,6)
        ELSE
          WRITE(hlpStr(ii),*) zeit(1)
          i1 = LEN_TRIM(hlpStr(ii)) + 1
          i2 = i1 + 6
          WRITE(hlpStr(ii)(i1:i2),'(2(1X,I2.2),I3,2(1X,I2.2))') &
               (zeit(jj), jj=2,6)
        ENDIF

      ENDDO


      IF ((PRESENT(GE) .OR. PRESENT(GT)) .AND. &
          (PRESENT(LE) .OR. PRESENT(LT))) THEN

        IF (PRESENT(GE)) THEN
          hlpStr(3) = ' <= value <'
        ELSE
          hlpStr(3) = ' < value <'
        ENDIF
        IF (PRESENT(LE)) THEN
          hlpStr(4) = TRIM(hlpStr(3)) // '='
          hlpStr(3) = hlpStr(4)
        ENDIF

        WRITE(lfnerr,'(16X,A,A)') 'Valid range: ', &
        '( "' // TRIM(hlpStr(1)) // ' "' // TRIM(hlpStr(3)) // ' "' // &
                                                      TRIM(hlpStr(2)) // ' " )'

      ELSE IF (PRESENT(GE)) THEN
        WRITE(lfnerr,'(16X,A,A)') 'Expected:    ', &
                                    '( value >= "' // TRIM(hlpStr(1)) // ' " )'

      ELSE IF (PRESENT(GT)) THEN
        WRITE(lfnerr,'(16X,A,A)') 'Expected:    ', &
                                    '( value > "'  // TRIM(hlpStr(1)) // ' " )'

      ELSE IF (PRESENT(LE)) THEN
        WRITE(lfnerr,'(16X,A,A)') 'Expected:    ', &
                                    '( value <= "' // TRIM(hlpStr(2)) // ' " )'

      ELSE IF (PRESENT(LT)) THEN
        WRITE(lfnerr,'(16X,A,A)') 'Expected:    ', &
                                    '( value < "'  // TRIM(hlpStr(2)) // ' " )'

      ENDIF ! write range to error msg.

    ENDIF ! Write valid range

    WRITE(lfnerr,*)

  ENDDO ! next error

  DEALLOCATE(irCode,stat=irc)
  DEALLOCATE(inpFile,stat=irc)
  DEALLOCATE(result,stat=irc)


  RETURN
END SUBROUTINE ckoptz


END MODULE
