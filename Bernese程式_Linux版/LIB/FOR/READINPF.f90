MODULE s_READINPF
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE readInpf(inpFileName, inpkey, readMode)

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
! Last mod.:  07-Jun-2010
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
!             13-Jan-2009 RD: Use '' as EOF-identifier in NEXTLINE
!             21-May-2010 MF: Deallocate inpKey%delFil & init inpKey%keys
!             07-Jun-2010 RD: Init also inpKey%nDel
!
! SRs called: init_inpKey, mygetarg, opnfil, opnerr, exitrc, alcerr
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_inpkey, ONLY: t_inpkey, init_inpKey, myStatus_Read, myStatus_Run

  USE s_opnfil
  USE s_alcerr
  USE f_nextline
  USE s_mygetarg
  USE s_opnerr
  USE s_exitrc
  USE f_chrcount
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)       :: inpFileName ! Name of the input file to be read
                                        ! blank: get name from standard input

! output:
  TYPE(t_inpkey)         :: inpkey      ! Structure to buffer the content of the
                                        ! input file

! input
  INTEGER(i4b), OPTIONAL :: readMode    ! Read mode of input parameters:
                                        ! not present: MYGETARG-standard
                                        ! 1: MYGETARG in special interactive mode

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER                  :: srName = 'readInpf'


! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength)                :: chdummy
  CHARACTER(LEN=lineLength)                    :: line

  INTEGER(i4b)                                 :: nlines
  INTEGER(i4b)                                 :: ikey
  INTEGER(i4b)                                 :: nKeys
  INTEGER(i4b)                                 :: iDel
  INTEGER(i4b)                                 :: ii,i1,i2,i3
  INTEGER(i4b)                                 :: ios, iac
  INTEGER(i4b)                                 :: quoteIndex


! Nothing to do
! -------------
  IF (inpKey%status >= myStatus_Read) THEN

    IF (LEN_TRIM(inpFileName) > 0 .AND. &
        inpKey%inpFileName == inpFileName) RETURN

  ENDIF

! Deallocate input key buffer
! ---------------------------
  IF (inpKey%status >= myStatus_Read) THEN

    IF (ASSOCIATED(inpKey%keys)) THEN
      DO ii = 1, inpKey%nKeys
        IF (ASSOCIATED(inpKey%keys(ii)%value)) &
            DEALLOCATE(inpKey%keys(ii)%value, stat=iac)
      ENDDO
      DEALLOCATE(inpKey%keys, stat=iac)
    ENDIF

    IF (ASSOCIATED(inpKey%delFil)) THEN
      DEALLOCATE(inpKey%delFil, stat=iac)
      inpKey%nDel = 0
    ENDIF

  ENDIF

  CALL init_inpKey(inpKey)

! Init status of reading input file
! ---------------------------------
  inpKey%status = myStatus_Read

! Get file name to read
! ---------------------
  IF (LEN_TRIM(inpFileName) > 0) THEN
    inpKey%inpFileName = inpFileName
  ELSE IF (PRESENT(readMode)) THEN
    IF (readMode == 1) THEN
      CALL mygetarg(inpKey%inpFileName,.TRUE.)
    ELSE
      CALL mygetarg(inpKey%inpFileName)
    ENDIF
  ELSE
    CALL mygetarg(inpKey%inpFileName)
  ENDIF

! Open the input file
! -------------------
  CALL opnfil(lfnloc, inpKey%inpFileName, 'OLD', 'FORMATTED', &
              'READONLY', ' ', ios)
  CALL opnerr(lfnerr, lfnloc, ios, inpKey%inpFileName, srName)

! Count the number of keywords
! ----------------------------
  nKeys = 0
  DO
    line = nextline(lfnloc, 1)
    IF ( Line == '' ) EXIT

    nKeys = nKeys + 1

    ! Check the keyword syntax
    READ(line, *,IOSTAT=ios) chdummy, nlines
    IF (ios /= 0) THEN
      WRITE(lfnerr,"(/,A,/,A,A,/,A,A,/)") &
            '*** SR READKEYS: Error decoding keyword line',&
            '                 panel: ',TRIM(inpKey%inpFileName),&
            '                 line : ',TRIM(line)
      CALL exitrc(2)
    ENDIF

    ! Read data lines
    IF (nlines > 1) THEN
      DO ii = 1, nlines
        line = nextline(lfnloc, 1)
      END DO
    END IF

  END DO

! Allocate the memory
! -------------------
  inpKey%nKeys = nKeys

  ALLOCATE( inpKey%keys(nKeys), stat=iac )
  CALL alcerr(iac, 'inpKey%keys', (/nKeys/), srName)

  DO ii = 1, inpKey%nKeys
    CALL init_key(inpKey%keys(ii))
  END DO

! Start reading the value
! -----------------------
  REWIND(lfnloc)

  ikey = 0
  DO
    line = nextline(lfnloc, 1)
    IF ( line == '' ) EXIT

    ikey = ikey + 1

    READ(line, *) inpKey%keys(ikey)%name, nlines

    ! Keyword is empty
    IF (nlines == 0) THEN
      ALLOCATE( inpKey%keys(ikey)%value(1), stat=iac )
      CALL alcerr(iac, 'inpKey%keys(ikey)%value', (/1/), 'readkeys')
      inpKey%keys(ikey)%value(1) = ''

    ! Allocate keyword buffer
    ELSE
      ALLOCATE( inpKey%keys(ikey)%value(nlines), stat=iac )
      CALL alcerr(iac, 'inpKey%keys(ikey)%value', (/nlines/), 'readkeys')
      inpKey%keys(ikey)%value(1:nlines) = ''

      ! Read keyword with one value
      IF (nlines == 1) THEN

        ! Quotes handling
        IF (chrCount('"', line, quoteIndex) <= 2) THEN
          READ(line, *) inpKey%keys(ikey)%name, nlines, &
                        inpKey%keys(ikey)%value(1)
        ELSE
          inpKey%keys(ikey)%value(1) = line(quoteIndex:)
        END IF

      ! Read keyword with more than one value
      ELSE IF (nlines > 1) THEN
        DO ii = 1, nlines

          line = nextline(lfnloc, 1)

          ! Quotes handling
          IF (chrCount('"', line, quoteIndex) <= 2) THEN
            READ(line, *) inpKey%keys(ikey)%value(ii)
          ELSE
            inpKey%keys(ikey)%value(ii) = line
          END IF

        END DO
      ENDIF
    ENDIF

! Remove inactive marks: "#foo#" -> "foo"
! ---------------------
    DO ii = 1,nlines

      ! Result without "":
      i1 = LEN_TRIM(inpKey%keys(ikey)%value(ii))
      IF (i1 < 2) CYCLE

      IF (inpKey%keys(ikey)%value(ii)(1:1)   == '#'  .AND. &
          inpKey%keys(iKey)%value(ii)(i1:i1) == '#')   THEN
        chDummy = inpKey%keys(ikey)%value(ii)(2:i1-1)
        inpKey%keys(ikey)%value(ii) = chDummy
      ENDIF

      ! Result with "":
      i1 = 1
      DO WHILE (i1 <= LEN_TRIM(inpKey%keys(ikey)%value(ii))-3)
        IF (inpKey%keys(iKey)%value(ii)(i1:i1+1) /= '"#') THEN
          i1 = i1+1
          CYCLE
        ENDIF

        DO i2 = i1+2,LEN_TRIM(inpKey%keys(ikey)%value(ii))-1
          IF (inpKey%keys(iKey)%value(ii)(i2:i2) == '"') EXIT
          IF (inpKey%keys(iKey)%value(ii)(i2:i2+1) /= '#"') CYCLE
          i3 = LEN_TRIM(inpKey%keys(ikey)%value(ii))
          IF (i2-1 >= i1+2) THEN
            chDummy = inpKey%keys(ikey)%value(ii)(1:i1) // &
                      inpKey%keys(ikey)%value(ii)(i1+2:i2-1) // &
                      inpKey%keys(ikey)%value(ii)(i2+1:i3)
          ELSE
            chDummy = inpKey%keys(ikey)%value(ii)(1:i1) // &
                      inpKey%keys(ikey)%value(ii)(i2+1:i3)
          ENDIF
          inpKey%keys(ikey)%value(ii) = chDummy
          i1=i2-2
          EXIT
        ENDDO
        i1=i1+1
      ENDDO
    ENDDO

! Init delete of files array
! --------------------------
    IF (inpKey%keys(iKey)%name == 'DELETE_FILES') THEN
      ALLOCATE(inpKey%delFil(nLines),stat=iac)
      CALL alcerr(iac,'inpKey%delFil',(/nLines/),srName)

      inpKey%nDel   = nLines
      inpKey%delFil = inpKey%keys(ikey)%value
    ENDIF

  ENDDO ! iKey loop

! Close input file
! ----------------
  CLOSE(lfnloc)

! Resolve keywords fot "DELETE_FILES"
! -----------------------------------
  DO iDel = 1,inpKey%nDel
    DO iKey = 1,inpKey%nKeys
      IF (inpKey%delFil(iDel) == inpKey%keys(ikey)%name) THEN
        inpKey%delFil(iDel) = inpKey%keys(ikey)%value(1)
        EXIT
      ENDIF
    ENDDO
  ENDDO

! Set status
! ----------
  inpKey%status = myStatus_Run

  RETURN
END SUBROUTINE readinpf


END MODULE
