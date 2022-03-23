MODULE s_WRITEKEY
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE writeKey(panFil,keys,iUniline,irc)

! -------------------------------------------------------------------------
! Purpose:    This subroutine writes key values into input file
!
! Author:     L. Mervart
!
! Created:    11-Feb-2001
!
! Changes:    10-Apr-2001 LM: write SELECTED only if more than one value
!             11-May-2001 RD: bugfix for empty fields
!             26-Jun-2001 RD: Use alcerr for allocation
!             28-May-2002 HU: use I5 instead of I3 for number of values
!             25-Sep-2002 HU: Remove i_astlib
!             12-Dec-2002 RD: Correct quotes-handling (independent from widget)
!             10-Mar-2003 HU: SJUSTL before writing uniline
!             10-Apr-2003 RD: SJUSTL in all cases
!             28-Jun-2005 MM: Unused variables removed
!             24-Mar-2011 SL: Use m_bern with ONLY, BACKSPACE added at the end
!             18-May-2011 LM: Problem with BACKSPACE on Windows - removed again
!             29-Feb-2012 RD: Unreliable "EOF" reporting by iostat (workaround)
!             05-Jun-2012 RD: Prevent index nn==0
!             20-Sep-2012 RD: Correctly deallocate arrays
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, t_key, lfnLoc, lfnErr, &
                      lineLength, keyNameLength
  USE s_opnfil
  USE s_alcerr
  USE s_inquire
  USE s_sjustl
  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=*)                 :: panFil
  TYPE(t_key),      DIMENSION(:)   :: keys
  INTEGER(i4b)                     :: iUniline ! Add uniline quotes (0/1)
  INTEGER(i4b)                     :: irc

! Local Variables
! ---------------
  CHARACTER(LEN=1)                                         :: qt,qt1,qt2
  CHARACTER(LEN=lineLength)    , DIMENSION(:), ALLOCATABLE :: buffer
  CHARACTER(LEN=lineLength)                                :: hlpLine
  CHARACTER(LEN=lineLength), DIMENSION(:), ALLOCATABLE     :: descLine
  CHARACTER(LEN=keyNameLength)                             :: currKey
  INTEGER(i4b)                                             :: numDescLines
  INTEGER(i4b)                                             :: numLines
  INTEGER(i4b)                                             :: startPanelsLine
  INTEGER(i4b)                                             :: numValues
  INTEGER(i4b)                                             :: numKeys
  INTEGER(i4b)                                             :: ii,nn
  INTEGER(i4b)                                             :: ios, iac
  INTEGER(i4b)                                             :: istat
  INTEGER(i4b)                                             :: iBlank
  INTEGER(i4b)                                             :: iLine
  INTEGER(i4b)                                             :: iKey
  LOGICAL, DIMENSION(:), ALLOCATABLE                       :: found
  LOGICAL                                                  :: yes
  LOGICAL                                                  :: uniline
  LOGICAL                                                  :: selwin

! Read the Entire Panel File (if it exists)
! -----------------------------------------
  numLines = 0
  CALL INQUIRE(FILE=panFil, EXIST=yes)

  IF (yes) THEN
    CALL opnfil(lfnloc, panFil, 'OLD', 'FORMATTED', 'READONLY', ' ', irc)
    IF (irc /= 0) THEN
      WRITE(lfnerr,*) '#** writeKey: error opening ', TRIM(panFil)
      irc = 2
      RETURN
    END IF

    iBlank = 0
    DO
      READ(lfnloc,'(A)',iostat=ios) hlpLine

      IF (LEN_TRIM(hlpline) > 0) THEN
        iBlank = 0
      ELSE
        iBlank = iBlank + 1
      ENDIF

      IF (iBlank > 1000) THEN
        numLines = numLines - iBlank + 1
        EXIT
      ELSEIF (ios == 0) THEN
        numLines = numLines + 1
      ELSE
        EXIT
      END IF
    END DO

    IF (ALLOCATED(buffer)) DEALLOCATE(buffer, STAT=istat)
    ALLOCATE(buffer(numLines), stat=iac)
    CALL alcerr(iac, 'buffer', (/numLines/), 'writekey')

    REWIND(lfnloc)

    DO ii = 1, numLines
      READ(lfnloc,'(A)',iostat=ios) buffer(ii)
      IF (ios /= 0) buffer(ii) = ''
    END DO

    CLOSE(lfnloc)
  END IF

! Open the Panel File for Writing
! -------------------------------
  CALL opnfil(lfnloc, panFil, 'UNKNOWN', 'FORMATTED', ' ', ' ', irc)
  IF (irc /= 0) THEN
    WRITE(lfnerr,*) '#** writeKey: error writing ', TRIM(panFil)
    irc = 2
    DEALLOCATE(buffer, STAT=istat)
    RETURN
  END IF

! Allocate and Initialize found Flags
! -----------------------------------
  numKeys = SIZE(keys)
  DEALLOCATE(found, STAT=istat)
  ALLOCATE(found(numKeys), stat=iac)
  CALL alcerr(iac, 'found', (/numKeys/), 'writekey')
  found(:) = .FALSE.

! Loop over all Lines of Internal Buffer, Write the INP-File up to Panels
! -----------------------------------------------------------------------
  startPanelsLine = numLines + 1
  iLine = 0
  Loop_iLine: DO
    iLine = iLine + 1
    IF (iLine > numLines) EXIT Loop_iLine

    hlpLine = ADJUSTL( buffer(iLine) )

    IF ( hlpLine      == ''  .OR. hlpLine(1:1) == '!'   .OR. &
         hlpLine(1:1) == '#' .OR. hlpLine(1:1) == '"' ) THEN

      IF (hlpLine(1:1) == '#' .AND. INDEX(hlpLine, 'BEGIN_PANEL') /= 0) THEN
        startPanelsLine = iLine
        EXIT Loop_iLine
      END IF

      WRITE(lfnloc,'(A)') TRIM(buffer(iLine))
      CYCLE Loop_iLine
    END IF

    READ(hlpLine,*) currKey

    IF (ALLOCATED(descLine)) DEALLOCATE(descLine, STAT=iac)

    DO iKey = 1, numKeys
      IF (keys(iKey)%name == currKey) THEN

        found(iKey) = .TRUE.

        ! Skip all Lines of Current Key, Remember the Description Line(s)
        ! --------------------------------------------------------------
        numDescLines = 0
        DO ii = iLine+1, numLines
          hlpLine = ADJUSTL( buffer(ii) )
          IF ( hlpLine == '') EXIT
          IF ( ( numDescLines == 0 .AND. hlpLine(1:1) == '#' ) .OR. &
               hlpLine(1:2) == '##' ) THEN
            numDescLines = numDescLines + 1
          END IF
        END DO

        ALLOCATE(descLine(numDescLines), STAT=iac)
        CALL alcerr(iac, 'descLine', (/numDescLines/), 'writekey')

        numDescLines = 0
        DO ii = iLine+1, numLines
          hlpLine = ADJUSTL( buffer(ii) )
          IF ( hlpLine == '') EXIT
          IF ( ( numDescLines == 0 .AND. hlpLine(1:1) == '#' ) .OR. &
               hlpLine(1:2) == '##' ) THEN
            numDescLines = numDescLines + 1
            descLine(numDescLines) = buffer(ii)
          END IF
          iLine = ii
        END DO

        ! Write the Keyword Values
        ! ------------------------
        numValues = SIZE(keys(iKey)%value)
        uniline = .FALSE.
        IF (numDescLines > 0) THEN
          IF (INDEX(descLine(1), 'uniline') /= 0) uniline = .TRUE.
        END IF
        qt=' '
        IF (iUniline == 1) qt = '"'
        IF (numValues == 1 .AND. .NOT.uniline) THEN
          IF (LEN_TRIM(keys(ikey)%value(1)) == 0) numValues = 0
        END IF
!
        IF (numValues == 1) THEN

          qt1 = '"'
          qt2 = '"'
          CALL SJUSTL(keys(iKey)%value(1))
          nn = LEN_TRIM(keys(iKey)%value(1))
          IF (nn /= 0) THEN
            IF (qt == '"' .OR. keys(iKey)%value(1)(1:1)   == qt1) qt1=' '
            IF (qt == '"' .OR. keys(iKey)%value(1)(nn:nn) == qt2) qt2=' '
          ENDIF

          WRITE(lfnloc,'(A,1x,i5,2x,5A)') TRIM(keys(iKey)%name),numValues,&
          TRIM(qt),TRIM(qt1), TRIM( keys(iKey)%value(1) ), TRIM(qt2),TRIM(qt)
        ELSE
          WRITE(lfnloc,'(A,1x,i5)') TRIM(keys(iKey)%name), numValues
          DO ii = 1, numValues

            qt1 = '"'
            qt2 = '"'
            CALL SJUSTL(keys(iKey)%value(ii))
            nn = LEN_TRIM(keys(iKey)%value(ii))
            IF (nn > 0) THEN
               IF (qt == '"' .OR. keys(iKey)%value(ii)(1:1)   == qt1) qt1=' '
               IF (qt == '"' .OR. keys(iKey)%value(ii)(nn:nn) == qt2) qt2=' '
            ENDIF

            WRITE(lfnloc,'(2x,5A)') &
            TRIM(qt),TRIM(qt1), TRIM( keys(iKey)%value(ii) ), TRIM(qt2),TRIM(qt)
          END DO
        END IF
        DO ii = 1, SIZE(descLine)
          WRITE(lfnloc, '(A)') TRIM(descLine(ii))
        END DO
        selwin = .FALSE.
        IF (numDescLines > 0) THEN
          IF (INDEX(descLine(1), 'selwin') /= 0) selwin = .TRUE.
        END IF
        IF (selwin .AND. numValues > 1) WRITE(lfnloc, '(A)') "  # SELECTED"
        DEALLOCATE(descLine, STAT=iac)
        CYCLE Loop_iLine
      END IF
    END DO

    WRITE(lfnloc,'(A)') TRIM(hlpLine)
  END DO Loop_iLine

! Write the new Keywords
! ----------------------
  WRITE(lfnloc,*)

  qt=' '
!  IF (iUniline == 1) qt = '"'

  DO iKey = 1, numKeys
    IF (.NOT. found(iKey)) THEN
      numValues = SIZE(keys(iKey)%value)
      IF (numValues == 1) THEN

        qt1 = '"'
        qt2 = '"'
        CALL SJUSTL(keys(iKey)%value(1))
        nn = LEN_TRIM(keys(iKey)%value(1))
        IF (nn > 0) THEN
           IF (qt == '"' .OR. keys(iKey)%value(1)(1:1)   == qt1) qt1=' '
           IF (qt == '"' .OR. keys(iKey)%value(1)(nn:nn) == qt2) qt2=' '
        ENDIF

        WRITE(lfnloc,'(A,1x,i5,2x,5A)') TRIM(keys(iKey)%name),numValues, &
        TRIM(qt),TRIM(qt1), TRIM( keys(iKey)%value(1) ), TRIM(qt2),TRIM(qt)
      ELSE
        WRITE(lfnloc,'(A,1x,i5)') TRIM(keys(iKey)%name), numValues
        DO ii = 1, SIZE(keys(iKey)%value)

          qt1 = '"'
          qt2 = '"'
          CALL SJUSTL(keys(iKey)%value(ii))
          nn = LEN_TRIM(keys(iKey)%value(ii))
          IF (nn > 0) THEN
             IF (qt == '"' .OR. keys(iKey)%value(ii)(1:1)   == qt1) qt1=' '
             IF (qt == '"' .OR. keys(iKey)%value(ii)(nn:nn) == qt2) qt2=' '
          ENDIF

          WRITE(lfnloc,'(2x,5A)') &
          TRIM(qt),TRIM(qt1), TRIM( keys(iKey)%value(ii) ), TRIM(qt2),TRIM(qt)
        END DO
      END IF
      WRITE(lfnloc,*)
    END IF
  END DO

! Write the remaining Part of the INP-File (the panels)
! -----------------------------------------------------
!!!!!!!  BACKSPACE(lfnLoc)
  DO iLine = startPanelsLine, numLines
    WRITE(lfnloc,'(A)') TRIM(buffer(iLine))
  END DO

  CLOSE(lfnloc)
  DEALLOCATE(found, STAT=istat)
  IF ( numLines > 0 ) DEALLOCATE(buffer, STAT=istat)

END SUBROUTINE writeKey

END MODULE
