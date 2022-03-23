MODULE s_UPDABB
CONTAINS

!
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE updabb(optabb,stanam,abbFil,abbrev,irCode)

! -------------------------------------------------------------------------
! Purpose:    Update station abbreviation list
!
! Author:     R. Dach
!
! Created:    14-Mar-2003
! Last mod.:  15-Mar-2007
!
! Changes:    23-Apr-2003 RD: Deallocate local pointers
!             03-Nov-2003 RD: New call of SR getabb
!             07-Apr-2004 HU: Creation of 4-char abbrev corrected
!             15-Mar-2007 RD: Ignore badChr in the station name
!
! SR called:  alcerr,writabb
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_abbrev, ONLY: t_abbrev, t_abbsta

  USE s_alcerr
  USE s_getabb
  USE s_writabb
  USE s_upperc
  USE s_lowerc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                  :: optabb       ! Abbreviation update options
                                   ! 0: No update allowed
                                   ! 1: Use only first chr for abbrev
                                   ! 2: Use all chr of the station name
                                   ! 3: Use also small letters
  CHARACTER(LEN=*)              :: staNam       ! New station name
  CHARACTER(LEN=*)              :: abbFil       ! Abbreviation file name
                                                ! blank: no update of file,
                                                !        no warnings

! input/output
  TYPE(t_abbrev)                :: abbrev       ! Abbreviation record

! output
  INTEGER(i4b)                  :: irCode       ! Return code
                                                ! 0: List could be updated
                                                ! 1: Problem in update

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6),  PARAMETER  :: srName = 'updabb'

  CHARACTER(LEN=62), PARAMETER  :: addChr = &
  '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'

  CHARACTER(LEN=15), PARAMETER  :: badChr = ' .${}()/\*?[]"~'

! Local Variables
! ---------------
  TYPE(t_abbrev)                :: abbHlp
  TYPE(t_abbsta)                :: newSta

  CHARACTER(LEN=4)              :: chr4
  CHARACTER(LEN=2)              :: chr2
  CHARACTER(LEN=1)              :: chr1

  INTEGER(i4b), DIMENSION(:),    &
                        POINTER :: abbIdx
  INTEGER(i4b)                  :: iAbb,jAbb
  INTEGER(i4b)                  :: nAbb
  INTEGER(i4b)                  :: i1,i2,i3,i4
  INTEGER(i4b)                  :: j1,j2,j3,j4
  INTEGER(i4b)                  :: ii
  INTEGER(i4b)                  :: irc

  LOGICAL                       :: sorted


! Nothing to do
! -------------
  IF (optabb == 0 .OR. LEN_TRIM(stanam) == 0) THEN
    irCode = 1
    RETURN
  ENDIF


! Is the new station in the list?
! -------------------------------
  jAbb = 0
  DO iAbb = 1,abbrev%nAbb

    IF (staNam == abbrev%abb(iAbb)%staNam) THEN
      jAbb = iAbb
      EXIT
    ENDIF

  ENDDO

  IF (jAbb /= 0) THEN
    irCode = 0
    RETURN
  ENDIF


! Generate a new record for the station
! -------------------------------------
  newSta%staNam = staNam
  newSta%staAb4 = ' '
  newSta%staAb2 = ' '
  newSta%remark = 'Added by SR ' // TRIM(srName)

  NULLIFY(abbIdx)

  irCode = 1

! Use first 4 resp. 2 characters of the station name
! --------------------------------------------------
  IF (irCode == 1 .AND. optabb == 1) THEN

    newSta%staAb4 = staNam(1:4)
    newSta%staAb2 = staNam(1:2)

    ! Remove blanks in abbreviations
    DO ii = 1,4
      IF (index(badChr,newSta%staAb4(ii:ii)) /= 0) newSta%staAb4(ii:ii) = '_'
    ENDDO

    DO ii = 1,2
      IF (index(badChr,newSta%staAb2(ii:ii)) /= 0) newSta%staAb2(ii:ii) = '_'
    ENDDO

    ! Use big letters for abbreviations
    CALL upperc(newSta%staAb4)
    CALL upperc(newSta%staAb2)

    irCode = 0

  ENDIF

! Use all letters of the station name of abbreviation creation
! ------------------------------------------------------------
  IF (irCode == 1 .AND. (optabb == 2 .OR. optabb == 3)) THEN

    ! Search for a 4-ID abbreviation in the complete file name
    DO i1 = 1,LEN_TRIM(stanam)
      IF (LEN_TRIM(newSta%staAb4) > 0) EXIT
      IF (index(badChr,stanam(i1:i1)) /= 0) CYCLE

      DO i2 = i1+1,LEN_TRIM(stanam)
        IF (LEN_TRIM(newSta%staAb4) > 0) EXIT
        IF (index(badChr,stanam(i2:i2)) /= 0) CYCLE

        DO i3 = i2+1,LEN_TRIM(stanam)
          IF (LEN_TRIM(newSta%staAb4) > 0) EXIT
          IF (index(badChr,stanam(i3:i3)) /= 0) CYCLE

          DO i4 = i3+1,LEN_TRIM(stanam)
            IF (LEN_TRIM(newSta%staAb4) > 0) EXIT
            IF (index(badChr,stanam(i4:i4)) /= 0) CYCLE

            newSta%staAb4 = stanam(i1:i1) // stanam(i2:i2) // &
                            stanam(i3:i3) // stanam(i4:i4)

            ! Use only big letters
            IF (optabb == 2) CALL upperc(newSta%staAb4)

            ! Check for this particular 4-ID suggestion
            CALL getabb(newSta%staAb4,2,abbrev,nAbb,abbIdx)

            IF (nAbb /= 0) newSta%staAb4 = ' '

          ENDDO ! i4
        ENDDO ! i3
      ENDDO ! i2
    ENDDO ! i1

    ! Search for a 2-ID abbreviation in the complete file name
    DO i1 = 1,LEN_TRIM(stanam)
      IF (LEN_TRIM(newSta%staAb2) > 0) EXIT
      IF (index(badChr,stanam(i1:i1)) /= 0) CYCLE

      DO i2 = i1+1,LEN_TRIM(stanam)
        IF (LEN_TRIM(newSta%staAb2) > 0) EXIT
        IF (index(badChr,stanam(i2:i2)) /= 0) CYCLE

        newSta%staAb2 = stanam(i1:i1) // stanam(i2:i2)

        ! Use only big letters
        IF (optabb == 2) CALL upperc(newSta%staAb2)

        ! Check for this particular 2-ID suggestion
        CALL getabb(newSta%staAb2,3,abbrev,nAbb,abbIdx)

        IF (nAbb /= 0) newSta%staAb2 = ' '

      ENDDO ! i2
    ENDDO ! i1

    ! Abbreviations found
    IF (LEN_TRIM(newSta%staAb4) > 0 .AND. LEN_TRIM(newSta%staAb2) > 0) &
      irCode = 0

  ENDIF

! Use all letters of the station name of abbreviation creation
! (Variations in small and big letters)
! ------------------------------------------------------------
  IF (irCode == 1 .AND. optabb == 3) THEN

    ! Search for a 4-ID abbreviation in the complete file name
    DO i1 = 1,LEN_TRIM(stanam)
      IF (LEN_TRIM(newSta%staAb4) > 0) EXIT
      IF (index(badChr,stanam(i1:i1)) /= 0) CYCLE

      DO i2 = i1+1,LEN_TRIM(stanam)
        IF (LEN_TRIM(newSta%staAb4) > 0) EXIT
        IF (index(badChr,stanam(i2:i2)) /= 0) CYCLE

        DO i3 = i2+1,LEN_TRIM(stanam)
          IF (LEN_TRIM(newSta%staAb4) > 0) EXIT
          IF (index(badChr,stanam(i3:i3)) /= 0) CYCLE

          DO i4 = i3+1,LEN_TRIM(stanam)
            IF (LEN_TRIM(newSta%staAb4) > 0) EXIT
            IF (index(badChr,stanam(i4:i4)) /= 0) CYCLE

            ! Try all variations with big and small letters within the
            ! suggested 4-ID abbreviation
            chr4 = '    '

            DO j1 = 1,2
              IF (LEN_TRIM(newSta%staAb4) > 0) EXIT

              chr1 = stanam(i1:i1)
              IF (j1 == 2) THEN
                CALL upperc(chr1)
                IF (chr1 == stanam(i1:i1)) CALL lowerc(chr1)
              ENDIF
              chr4(1:1) = chr1

              DO j2 = 1,2
                IF (LEN_TRIM(newSta%staAb4) > 0) EXIT

                chr1 = stanam(i2:i2)
                IF (j2 == 2) THEN
                  CALL upperc(chr1)
                  IF (chr1 == stanam(i2:i2)) CALL lowerc(chr1)
                ENDIF
                chr4(2:2) = chr1

                DO j3 = 1,2
                  IF (LEN_TRIM(newSta%staAb4) > 0) EXIT

                  chr1 = stanam(i3:i3)
                  IF (j3 == 2) THEN
                    CALL upperc(chr1)
                    IF (chr1 == stanam(i3:i3)) CALL lowerc(chr1)
                  ENDIF
                  chr4(3:3) = chr1

                  DO j4 = 1,2
                    IF (LEN_TRIM(newSta%staAb4) > 0) EXIT

                    chr1 = stanam(i4:i4)
                    IF (j4 == 2) THEN
                      CALL upperc(chr1)
                      IF (chr1 == stanam(i4:i4)) CALL lowerc(chr1)
                    ENDIF
                    chr4(4:4) = chr1

                    ! Check for this particular 4-ID suggestion
                    CALL getabb(chr4,2,abbrev,nAbb,abbIdx)

                    IF (nAbb == 0) newSta%staAb4 = chr4

                  ENDDO ! j4
                ENDDO ! j3
              ENDDO ! j2
            ENDDO ! j1

          ENDDO ! i4
        ENDDO ! i3
      ENDDO ! i2
    ENDDO !i1

    ! Search for a 2-ID abbreviation in the complete file name
    DO i1 = 1,LEN_TRIM(stanam)
      IF (LEN_TRIM(newSta%staAb4) > 0) EXIT
      IF (index(badChr,stanam(i1:i1)) /= 0) CYCLE

      DO i2 = i1+1,LEN_TRIM(stanam)
        IF (LEN_TRIM(newSta%staAb4) > 0) EXIT
        IF (index(badChr,stanam(i2:i2)) /= 0) CYCLE

        ! Try all variations with big and small letters within the
        ! suggested 4-ID abbreviation
        chr2 = '  '

        DO j1 = 1,2
          IF (LEN_TRIM(newSta%staAb4) > 0) EXIT

          chr1 = stanam(i1:i1)
          IF (j1 == 2) THEN
            CALL upperc(chr1)
            IF (chr1 == stanam(i1:i1)) CALL lowerc(chr1)
          ENDIF
          chr2(1:1) = chr1

          DO j2 = 1,2
            IF (LEN_TRIM(newSta%staAb4) > 0) EXIT

            chr1 = stanam(i2:i2)
            IF (j2 == 2) THEN
              CALL upperc(chr1)
              IF (chr1 == stanam(i2:i2)) CALL lowerc(chr1)
            ENDIF
            chr2(2:2) = chr1

            ! Check for this particular 2-ID suggestion
            CALL getabb(chr2,3,abbrev,nAbb,abbIdx)

            IF (nAbb == 0) newSta%staAb2 = chr2

          ENDDO ! j2
        ENDDO ! j1

      ENDDO ! i2
    ENDDO !i1

    ! Abbreviations found
    IF (LEN_TRIM(newSta%staAb4) > 0 .AND. LEN_TRIM(newSta%staAb2) > 0) &
      irCode = 0

  ENDIF

! Use all letters of the station name of abbreviation creation
! Replace parts of the station name with additional characters
! ------------------------------------------------------------
  IF (irCode == 1 .AND. (optabb == 2 .OR. optabb == 3)) THEN

    ! Search for a 4-ID abbreviation in the complete file name
    DO i1 = 1,LEN_TRIM(stanam)
      IF (LEN_TRIM(newSta%staAb4) > 0) EXIT
      IF (index(badChr,stanam(i1:i1)) /= 0) CYCLE

      DO i2 = i1+1,LEN_TRIM(stanam)
        IF (LEN_TRIM(newSta%staAb4) > 0) EXIT
        IF (index(badChr,stanam(i2:i2)) /= 0) CYCLE

        DO i3 = i2+1,LEN_TRIM(stanam)
          IF (LEN_TRIM(newSta%staAb4) > 0) EXIT
          IF (index(badChr,stanam(i3:i3)) /= 0) CYCLE

          DO i4 = i3+1,LEN_TRIM(stanam)
            IF (LEN_TRIM(newSta%staAb4) > 0) EXIT
            IF (index(badChr,stanam(i4:i4)) /= 0) CYCLE

            ! Try all variations with big and small letters within the
            ! suggested 4-ID abbreviation, replace with add. characters
            chr4 = '    '

            DO j1 = -1,LEN_TRIM(addchr)
              IF (LEN_TRIM(newSta%staAb4) > 0) EXIT

              IF (j1 == -1) THEN
                chr4(1:1) = stanam(i1:i1)
              ELSE IF (j1 ==  0) THEN
                IF (optabb == 2) CYCLE
                chr1 = stanam(i1:i1)
                CALL upperc(chr1)
                IF (chr1 == stanam(i1:i1)) CALL lowerc(chr1)
                chr4(1:1) = chr1
              ELSE
                chr4(1:1) = addchr(j1:j1)
              ENDIF

              DO j2 = -1,LEN_TRIM(addchr)
                IF (LEN_TRIM(newSta%staAb4) > 0) EXIT

                IF (j2 == -1) THEN
                  chr4(2:2) = stanam(i2:i2)
                ELSE IF (j2 ==  0) THEN
                  IF (optabb == 2) CYCLE
                  chr1 = stanam(i2:i2)
                  CALL upperc(chr1)
                  IF (chr1 == stanam(i2:i2)) CALL lowerc(chr1)
                  chr4(2:2) = chr1
                ELSE
                  chr4(2:2) = addchr(j2:j2)
                ENDIF

                DO j3 = -1,LEN_TRIM(addchr)
                  IF (LEN_TRIM(newSta%staAb4) > 0) EXIT

                  IF (j3 == -1) THEN
                    chr4(3:3) = stanam(i3:i3)
                  ELSE IF (j3 ==  0) THEN
                    IF (optabb == 2) CYCLE
                    chr1 = stanam(i3:i3)
                    CALL upperc(chr1)
                    IF (chr1 == stanam(i3:i3)) CALL lowerc(chr1)
                    chr4(3:3) = chr1
                  ELSE
                    chr4(3:3) = addchr(j3:j3)
                  ENDIF

                  DO j4 = -1,LEN_TRIM(addchr)
                    IF (LEN_TRIM(newSta%staAb4) > 0) EXIT

                    IF (j4 == -1) THEN
                      chr4(4:4) = stanam(i4:i4)
                    ELSE IF (j4 ==  0) THEN
                      IF (optabb == 2) CYCLE
                      chr1 = stanam(i4:i4)
                      CALL upperc(chr1)
                      IF (chr1 == stanam(i4:i4)) CALL lowerc(chr1)
                      chr4(4:4) = chr1
                    ELSE
                      chr4(4:4) = addchr(j4:j4)
                    ENDIF

                    ! Use only big letters
                    IF (optabb == 2) CALL upperc(chr4)

                    ! Check for this particular 4-ID suggestion
                    CALL getabb(chr4,2,abbrev,nAbb,abbIdx)

                    IF (nAbb == 0) newSta%staAb4 = chr4

                  ENDDO ! j4
                ENDDO ! j3
              ENDDO ! j2
            ENDDO ! j1

          ENDDO ! i4
        ENDDO ! i3
      ENDDO ! i2
    ENDDO !i1

    ! Search for a 2-ID abbreviation in the complete file name
    DO i1 = 1,LEN_TRIM(stanam)
      IF (LEN_TRIM(newSta%staAb2) > 0) EXIT
      IF (index(badChr,stanam(i1:i1)) /= 0) CYCLE

      DO i2 = i1+1,LEN_TRIM(stanam)
        IF (LEN_TRIM(newSta%staAb2) > 0) EXIT
        IF (index(badChr,stanam(i2:i2)) /= 0) CYCLE

        ! Try all variations with big and small letters within the
        ! suggested 4-ID abbreviation, replace with add. characters
        chr2 = '    '

        DO j1 = -1,LEN_TRIM(addchr)
          IF (LEN_TRIM(newSta%staAb2) > 0) EXIT

          IF (j1 == -1) THEN
            chr2(1:1) = stanam(i1:i1)
          ELSE IF (j1 ==  0) THEN
            IF (optabb == 2) CYCLE
            chr1 = stanam(i1:i1)
            CALL upperc(chr1)
            IF (chr1 == stanam(i1:i1)) CALL lowerc(chr1)
            chr2(1:1) = chr1
          ELSE
            chr2(1:1) = addchr(j1:j1)
          ENDIF

          DO j2 = 1,2
            IF (LEN_TRIM(newSta%staAb2) > 0) EXIT

            IF (j2 == -1) THEN
              chr2(2:2) = stanam(i2:i2)
            ELSE IF (j2 ==  0) THEN
              IF (optabb == 2) CYCLE
              chr1 = stanam(i2:i2)
              CALL upperc(chr1)
              IF (chr1 == stanam(i2:i2)) CALL lowerc(chr1)
              chr2(2:2) = chr1
            ELSE
              chr2(2:2) = addchr(j2:j2)
            ENDIF

            ! Use only big letters
            IF (optabb == 2) CALL upperc(chr2)

            ! Check for this particular 4-ID suggestion
            CALL getabb(chr2,3,abbrev,nAbb,abbIdx)

            IF (nAbb == 0) newSta%staAb2 = chr2

          ENDDO ! j2
        ENDDO ! j1

      ENDDO ! i2
    ENDDO ! i1

    ! Abbreviations found
    IF (LEN_TRIM(newSta%staAb4) > 0 .AND. LEN_TRIM(newSta%staAb2) > 0) &
      irCode = 0

  ENDIF


! No new abbreviation could be found
! ----------------------------------
  IF (irCode == 1) THEN

    IF (LEN_TRIM(newSta%staAb4) == 0) THEN
      newSta%staAb4 = staNam(1:4)

      ! Remove blanks in abbreviations
      DO ii = 1,4
        IF (index(badChr,newSta%staAb4(ii:ii)) /= 0) newSta%staAb4(ii:ii) = '_'
      ENDDO

      ! Use big letters for abbreviations
      IF (optabb /= 3) CALL upperc(newSta%staAb4)
    ENDIF

    IF (LEN_TRIM(newSta%staAb2) == 0) THEN
      newSta%staAb2 = staNam(1:2)

      ! Remove blanks in abbreviations
      DO ii = 1,2
        IF (index(badChr,newSta%staAb2(ii:ii)) /= 0) newSta%staAb2(ii:ii) = '_'
      ENDDO

      ! Use big letters for abbreviations
      IF (optabb /= 3) CALL upperc(newSta%staAb2)
    ENDIF

    IF (LEN_TRIM(abbFil) > 0) THEN
      WRITE(lfnerr,'(/,A,/,3(16X,A,/))') &
      ' ### SR UPDABB: New abbreviation added to the abbreviation file', &
                      'File name: ' // TRIM(abbFil),                     &
                      'Station:   ' // newSta%stanam // ' ( ' //         &
                      newSta%staab4 // '  ' // newSta%staab2 //  ' )',   &
                      'ATTENTION:  The abbreviation is ambiguous!'
    ENDIF

  ELSE

    IF (LEN_TRIM(abbFil) > 0) THEN
      WRITE(lfnerr,'(/,A,/,2(16X,A,/))') &
      ' ### SR UPDABB: New abbreviation added to the abbreviation file', &
                      'File name: ' // TRIM(abbFil),                     &
                      'Station:   ' // newSta%stanam // ' ( ' //         &
                      newSta%staab4 // '  ' // newSta%staab2 //  ' )'
    ENDIF

  ENDIF


! Extent abbrev, add the new station
! ----------------------------------
  IF (abbrev%nAbb > 0) THEN
    ALLOCATE(abbHlp%abb(abbrev%nAbb),stat=irc)
    CALL alcerr(irc,'abbHlp%abb',(/abbrev%nAbb/),srName)

    abbHlp%abb(:) = abbrev%abb(:)

    DEALLOCATE(abbrev%abb,stat=irc)
  ENDIF

  ALLOCATE(abbrev%abb(abbrev%nAbb+1),stat=irc)
  CALL alcerr(irc,'abbrev%abb',(/abbrev%nAbb+1/),srName)

  abbrev%nAbb = abbrev%nAbb+1
  IF (abbrev%nAbb > 1) abbrev%abb(2:abbrev%nAbb) = abbHlp%abb(:)
  abbrev%abb(1) = newSta

! Sort stations
! -------------
  sorted = (abbrev%nAbb == 1)
  DO WHILE (.NOT. sorted)

    sorted = .TRUE.

    DO iAbb = 1,abbrev%nAbb-1

      IF (abbrev%abb(iAbb)%stanam > abbrev%abb(iAbb+1)%stanam) THEN
        abbHlp%abb(1)      = abbrev%abb(iAbb)
        abbrev%abb(iAbb)   = abbrev%abb(iAbb+1)
        abbrev%abb(iAbb+1) = abbHlp%abb(1)
        sorted = .FALSE.
      ENDIF

    ENDDO

  ENDDO

! Deallocate the buffer
! ---------------------
  IF (abbrev%nAbb > 1) DEALLOCATE(abbHlp%abb,stat=irc)

! Write the new abbreviation table into the file
! ----------------------------------------------
  IF (LEN_TRIM(abbFil) > 0) &
    CALL writabb(abbfil,abbrev)

  DEALLOCATE(abbIdx,stat=irc)

  RETURN
END SUBROUTINE updabb

END MODULE
