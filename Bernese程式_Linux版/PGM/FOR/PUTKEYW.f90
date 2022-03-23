
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM putkeyw

! -------------------------------------------------------------------------
! Purpose:    Store Value of a Keyword into an Input File
!
! Author:     L. Mervart
!
! Created:    21-Mar-2001
! Last mod.:  23-Sep-2010
!
! Changes:    14-Apr-2001 LM: Call SplArg
!             08-May-2001 RD: panFileName needs to be longer
!             10-May-2001 RD: selFileName needs to be longer
!             26-Jun-2001 RD: Use alcerr for allocation
!             18-Dec-2001 HU: Use implicit none
!             16-Aug-2002 LM: Use mygetarg
!             25-Sep-2002 HU: Remove i_astlib
!             13-Dec-2002 RD: New quotes-handling in SR writekey
!             23-Apr-2003 HU: Nullify local pointers
!             06-Aug-2003 RD: Write warning if a keywords will be added
!             22-Sep-2003 RD: Finish program with EXITRC(0)
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             13-Jan-2009 RD: Use '' as EOF-identifier in NEXTLINE
!             23-Sep-2010 RD: Enable CPU counter
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_cpu,    ONLY: cpu_start
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE s_alcerr
  USE s_opnfil
  USE s_mygetarg
  USE s_splarg
  USE s_inquire
  USE s_readinpf
  USE s_writekey
  USE s_readkeys
  USE s_exitrc
  USE f_tstkey
  USE f_nextline

  IMPLICIT NONE

! Local variables
! ---------------
  TYPE(t_key),  DIMENSION(1)                           :: keys
  INTEGER(i4b)                                         :: nVal
  INTEGER(i4b)                                         :: ii
  INTEGER(i4b)                                         :: kk
  INTEGER(i4b)                                         :: irc
  INTEGER(i4b)                                         :: ios, iac
  CHARACTER(LEN=lineLength)                            :: action
  CHARACTER(LEN=lineLength)                            :: line
  CHARACTER(LEN=lineLength)                            :: panFileName
  CHARACTER(LEN=lineLength)                            :: selFileName
  CHARACTER(LEN=lineLength)    , DIMENSION(:), POINTER :: argv
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: oldValues
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: hlpValues
  LOGICAL                                              :: yes


! Start CPU Counter
! -----------------
  CALL cpu_start(.FALSE.)

! Nullify pointers
! ----------------
  NULLIFY(argv)
  NULLIFY(oldValues)
  NULLIFY(hlpValues)
  CALL init_inpkey(inpKey)

  CALL mygetarg(line)
  CALL splArg(line, argv)

  panFileName  = argv(1)
  keys(1)%name = argv(2)

! Check the INP-file for the requested keyword
! --------------------------------------------
  CALL readinpf(panFileName,inpKey)
  CALL readkeys('*', oldValues, irc)
  IF (.NOT. tstKey(keys(1)%name)) THEN
    WRITE(*,'(/A,/,17X,A,/)')                                                  &
          ' ### PG PUTKEYW: Keyword "' // TRIM(keys(1)%name) // '" not found ',&
                          'in INP-file ' // TRIM(panFileName)
  ENDIF


  IF      (SIZE(argv) == 3) THEN
    ALLOCATE(keys(1)%value(1), stat=iac)
    CALL alcerr(iac, 'keys(1)%value', (/1/), 'putkeyw')
    keys(1)%value(1) = argv(3)
    action      = ''
    selFileName = ''
  ELSE IF (SIZE(argv) == 4) THEN
    action      = argv(3)
    selFileName = argv(4)

    CALL opnfil(lfnloc, selFileName, 'OLD', 'FORMATTED', 'READONLY', ' ', ios)
    IF (ios /= 0) THEN
      WRITE(*,*) ' #** putkey: cannot open file ', TRIM(selFileName)
      CALL exitrc(2)
    END IF

    nVal = 0
    DO
      line = nextline(lfnloc, 1)
      IF ( Line == '' ) EXIT
      nVal = nVal + 1
    END DO

    ALLOCATE( keys(1)%value(nVal), stat=iac )
    CALL alcerr(iac, 'keys(1)%value', (/nVal/), 'putkeyw')
    REWIND(lfnloc)

    nVal = 0
    DO
      line = nextline(lfnloc, 1)
      IF ( Line == '' ) EXIT
      nVal = nVal + 1
      keys(1)%value(nVal) = line
    END DO

    CLOSE(lfnloc)

  ELSE
    WRITE(*,*) ' #** putkey: wrong number of arguments'
    CALL exitrc(2)
  END IF

  IF (action == 'PREPEND' .OR. action == 'APPEND') THEN
    CALL INQUIRE(FILE=panFileName , EXIST=yes)
    IF (yes) THEN
      CALL readinpf(panFileName, inpKey)
      CALL readkeys(keys(1)%name, oldValues, irc)
      IF (oldValues(1) /= "") THEN
        ALLOCATE( hlpValues( SIZE(keys(1)%value) ), stat=iac )
        CALL alcerr(iac, 'hlpValues', (/SIZE(keys(1)%value)/), 'putkeyw')
        hlpValues(:) = keys(1)%value(:)
        DEALLOCATE(keys(1)%value, stat=iac)
        ALLOCATE( keys(1)%value(SIZE(oldValues) + SIZE(hlpValues)), stat=iac )
        CALL alcerr(iac, 'keys(1)%value',                   &
            (/SIZE(oldValues) + SIZE(hlpValues)/), 'putkeyw')

        IF (action == 'PREPEND') THEN
          DO ii = 1, SIZE(hlpValues)
            kk = ii
            keys(1)%value(kk) = hlpValues(ii)
          END DO
          DO ii = 1, SIZE(oldValues)
            kk = SIZE(hlpValues) + ii
            keys(1)%value(kk) = oldValues(ii)
          END DO
        ELSE
          DO ii = 1, SIZE(oldValues)
            kk = ii
            keys(1)%value(kk) = oldValues(ii)
          END DO
          DO ii = 1, SIZE(hlpValues)
            kk = SIZE(oldValues) + ii
            keys(1)%value(kk) = hlpValues(ii)
          END DO
        END IF
      END IF
    END IF
  END IF

  CALL writeKey(panFileName, keys, 0, irc)

  CALL exitrc(0)

END PROGRAM PUTKEYW
