MODULE s_CKOPTC
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE ckoptc(iStop, keyWord, keyString, values,                   &
                  srName, descr, irc0, ircSum,                         &
                  other, colTit, iLine, linTit, maxVal,  valList,      &
                  init, error, nError, nValid, result1, nResult, result2)

! -------------------------------------------------------------------------
! Purpose:    Checks a combobox
!
! Remarks:    The following checks are done
!               1. irc0 is the return code of the prev. SR (usually readkeys)
!                  If it is different from zero "ircSum" is incremented, all
!                  "results" are set to the "error" value (if present)
!               2. The size of "result2" resp. the value of "maxVal" is
!                  compared to the size of the input strings "keyString".
!                  If too many input strings are found, all "results" are set
!                  to "error" (if present); ircSum is incremented.
!               3. The input string is compared with the list of valid values
!                  ("values"). The index of the matching string is the result
!                  (integer). If a "valList" in specified the index is taken to
!                  give back the coresponding value from this list.
!                  If no value is identical with the input string the value
!                  from "other" is taken. If not "other" value is specified
!                  The result is undefined in the case of warning (iStop=0);
!                  in the case of error (iStop=1) the "error" value is set
!                  (if present).!
!
! Author:     R. Dach
!
! Created:    19-Sep-2001
! Last mod.:  13-May-2004
!
! Changes:    02-Oct-2001 RD: modified handling of srName
!             29-Jan-2002 HB: Modified some IF-statements because of
!                             compiler problems
!             23-Apr-2003 CU: Nullify local pointers
!             13-May-2004 RD: Use MSG_/DESCR_ instead of "descr" (if avail.)
!
! SR used:    alcerr, ckoptn, exitrc, readkeys, descrp
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
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
  CHARACTER(LEN=*),           &
       DIMENSION(:)          :: values     ! List of valid entries
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
  INTEGER(i4b),     OPTIONAL :: other      ! others allowed
  CHARACTER(LEN=*), OPTIONAL :: colTit     ! title of the column
  CHARACTER(LEN=*), OPTIONAL :: linTit     ! title of a line
  INTEGER(i4b),     OPTIONAL :: iLine      ! number of a line
  INTEGER(i4b),     OPTIONAL :: maxVal     ! max. number of entries allowed
                                           ! for the input string array
  INTEGER(i4b),     OPTIONAL :: init       ! used to init the results
  INTEGER(i4b),     OPTIONAL :: error      ! used if an error found
  INTEGER(i4b),               &
      DIMENSION(:), OPTIONAL :: valList    ! values if not default (1,2..)

! optional output:
  INTEGER(i4b),     OPTIONAL :: nError     ! number of errors found
  INTEGER(i4b),     OPTIONAL :: nValid     ! number of valid items found
            ! nError / nValid are set independently from the warning/error
            ! setting using "iStop"
  INTEGER(i4b),     OPTIONAL :: result1    ! only one result value.
            ! To set simple integer variables.
            ! The first valid entry of the strings.
            ! If only errors found either the optional error value or
            ! "something" else.
  INTEGER(i4b),     OPTIONAL :: nResult    ! number of valid entries in result2
  INTEGER(i4b),               &
      DIMENSION(:), OPTIONAL :: result2    ! array of results from all entries
            ! For every input string the corresponding result is given:
            ! depending on the optional parameters: empty, error, ...
            ! If the optional "init" is set, all entries behind the size of
            ! the keyString array are set to this value.

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=16), DIMENSION(0:1), PARAMETER :: errStr = &
  (/ ' ### SR CKOPTC: ', ' *** SR CKOPTC: ' /)

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
           DIMENSION(:),POINTER :: inpFile  ! Name of the input file
  CHARACTER(LEN=shortLineLength),&
           DIMENSION(3)         :: hlpStr
  CHARACTER(LEN=45)             :: optTxt

  INTEGER(i4b),                  &
         DIMENSION(:),POINTER   :: result  ! internal buffer for results
  INTEGER(i4b)                  :: maxItem
  INTEGER(i4b)                  :: iItem, nItem
  INTEGER(i4b)                  :: errNum
  INTEGER(i4b)                  :: nOK
  INTEGER(i4b), DIMENSION(2)    :: maxLen
  INTEGER(i4b)                  :: iVal
  INTEGER(i4b)                  :: ii, i1, i2
  INTEGER(i4b)                  :: irc, ircSave


  NULLIFY(inpFile)
  NULLIFY(result)

! A very long description line was given...
! -----------------------------------------
  IF (LEN_TRIM(descr) > 45) &
    WRITE(lfnerr,'(/,A,2(/,16X,A),/,16X,A,I4,A,/,16X,A,/)') &
    ' ### SR CKOPTC: Dear programmer, please give a shorter description',  &
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

    IF (PRESENT(nError))  nError  = 99
    IF (PRESENT(nValid))  nValid  = 0
    IF (PRESENT(result1) .AND. PRESENT(error)) result1 = error
    IF (PRESENT(result2) .AND. PRESENT(error)) result2 = error

    RETURN

  ENDIF

! Program error: iStop = 0 or 1
! -----------------------------
  IF (iStop /= 0 .AND. iStop /= 1) THEN
    WRITE(lfnerr,'(/,A,2(/,16X,A),/)') &
    ' *** SR CKOPTC: Dear programmer, you did a FATAL ERROR!!!', &
                    'The first parameter has to be "0" or "1".', &
                    'Keyword:  ' // TRIM(keyWord)
    CALL exitrc(2)
  ENDIF


! Program error: valList and values have different size
! -----------------------------------------------------
  IF (PRESENT(valList)) THEN
    IF ( SIZE(valList) /= SIZE(values)) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,2(/,16X,A,I5),/)') &
           ' *** SR CKOPTC: Dear programmer, you did a FATAL ERROR!!!', &
                           'The size of valid values und the value ' // &
                           'list is different:',                        &
                           'Number of valid values: ',SIZE(values),     &
                           'Size of value list:     ',SIZE(valList)
      CALL exitrc(2)
    ENDIF
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
      IF(maxVal > SIZE(result2)) THEN
        WRITE(lfnerr,'(/,A,3(/,16X,A),/)') &
             ' *** SR CKOPTC: Dear programmer, you did a FATAL ERROR!!!',&
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
    IF (PRESENT(result1) .AND. PRESENT(error)) result1 = error
    IF (PRESENT(result2) .AND. PRESENT(error)) result2 = error

    RETURN
  ENDIF


! Allocate the internal result buffer
! -----------------------------------
  nOK  = 0
  ALLOCATE(result(nItem),stat=irc)
  CALL alcerr(irc, 'result', (/nItem/), 'ckoptc')
  result = -1


! Init variables
! --------------
  IF (PRESENT(nError)) nError = 0
  IF (PRESENT(nValid)) nValid = 0
  errNum =  0
  maxLen =  0


! Loop all entries of input string
! --------------------------------
  DO iItem = 1, nItem

    IF (LEN_TRIM(keyString(iItem)) > maxLen(1)) &
      maxLen(1) = LEN_TRIM(keyString(iItem))

! Check the entries of the string
! -------------------------------
    DO iVal = 1, SIZE(values)

      IF (values(iVal) == keyString(iItem)) result(iItem) = iVal

      IF (LEN_TRIM(values(iVal)) > maxLen(2)) &
        maxLen(2) = LEN_TRIM(values(iVal))

    ENDDO

! Count errors
! ------------
    IF (result(iItem) == -1) errNum = errNum + 1

    IF (result(iItem) /= -1 .OR. PRESENT(other)) THEN
      IF (PRESENT(nValid)) nValid = nValid + 1
    ELSE
      IF (PRESENT(nError)) nError = nError + 1
    ENDIF

    IF (result(iItem) /= -1) nOK  = nOK  + 1

! Put the values into the result records
! --------------------------------------
    IF (result(iItem) /= -1 .OR. (iStop == 0 .AND. .NOT. PRESENT(other))) THEN

      IF (PRESENT(result2)) THEN
        result2(iItem) = result(iItem)
        IF (PRESENT(valList)) THEN
          IF (result(iItem) > 0 .AND. result(iItem) <= SIZE(valList)) THEN
            result2(iItem) = valList(result(iItem))
          ENDIF
        ENDIF
      ENDIF

    ELSE IF (PRESENT(other)) THEN

      IF (PRESENT(result2)) result2(iItem) = other

    ELSE

      ircSum = ircSum + iStop

      IF (PRESENT(error) .AND. PRESENT(result2)) result2(iItem) = error

    ENDIF

  ENDDO ! next entry


! Give the number of elements back
! --------------------------------
  IF (PRESENT(nResult)) nResult = nItem

! generate "result1"
! ------------------
  IF (PRESENT(result1)) THEN
    IF (nOK > 0) THEN
      DO iItem = 1,nItem
        IF (result(iItem) /= -1) THEN
          result1 = result(iItem)
          IF (PRESENT(valList)) THEN
            IF (result(iItem) > 0 .AND. result(iItem) <= SIZE(valList)) THEN
              result1 = valList(result(iItem))
            ENDIF
            EXIT
          ENDIF
        ENDIF
      ENDDO
    ELSE IF (PRESENT(other)) THEN
      result1 = other
    ELSE IF (iStop == 1 .AND. PRESENT(error)) THEN
      result1 = error
    ELSE
      result1 = result(1)
    ENDIF
  ENDIF


! Only valid entries found
! ------------------------
  IF (errNum == 0) RETURN

! Other values allowed
! --------------------
  IF (PRESENT(other)) RETURN

! Write the error message
! -----------------------
  WRITE(lfnerr, '(/,A)') errStr(iStop) // &
        'The input string for keyword "' // TRIM(keyWord) // '" is invalid.'

  CALL readKeys('', inpFile, irc)
  WRITE(lfnerr,'(16X,A,A)') 'Input file:  ',TRIM(inpFile(1))

  WRITE(lfnerr,'(16X,A,A)') 'Called by:   ',TRIM(srName)

  CALL descrp(keyWord,45,optTxt)
  IF (LEN_TRIM(optTxt) == 0) optTxt = descr
  WRITE(lfnerr,'(16X,A,A)') 'Option:      ',TRIM(optTxt)


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

    IF (result(iItem) == -1) THEN
      hlpStr(3) = 'Bad value:   " ' // TRIM(keyString(iItem)) // ' "'

      IF (LEN_TRIM(TRIM(hlpStr(1))) > 0) THEN
        i1 = 20 + maxLen(1)
        i2 = i1 + LEN_TRIM(TRIM(hlpStr(1))) - 1
        WRITE(hlpStr(3)(i1:i2),'(A)') TRIM(hlpStr(1))
      ENDIF

      WRITE(lfnerr,'(16X,A)')  TRIM(hlpStr(3))
    ENDIF
  ENDDO

! List all keywords and entries
! -----------------------------
  hlpStr(1) = 'Valid items: '
  ii = 0
  DO iVal = 1, SIZE(values)
    ii = ii + 1
    i1 = (ii-1) * (maxLen(2)+4) + 14
    i2 = ii * (maxLen(2)+4) + 14

    WRITE(hlpStr(1)(i1:i2),'(A)') '"' // TRIM(values(iVal)) // '",'

    IF (iVal == SIZE(values)) THEN
      i2 = LEN_TRIM(hlpStr(1))
      WRITE(hlpStr(1)(i2:i2),'(A)') ' '
    ENDIF

    IF (LEN_TRIM(hlpStr(1)) + maxLen(2)+3 > 60-16 .OR. iVal == SIZE(values)) THEN
      WRITE(lfnerr, '(16X,A)') TRIM(hlpStr(1))
      hlpStr(1) = ' '
      ii = 0
    ENDIF
  ENDDO

  WRITE(lfnerr,*)

  DEALLOCATE(inpFile,stat=irc)
  DEALLOCATE(result, stat=irc)

  RETURN
END SUBROUTINE ckoptc

END MODULE
