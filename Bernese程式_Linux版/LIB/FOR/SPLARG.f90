MODULE s_SPLARG
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE splArg(line, argv)

! -------------------------------------------------------------------------
! Purpose:    Split line into a list of arguments (arguments are either
!             separated by blanks or enclosed in double quotes)
!
! Author:     L. Mervart
!
! Created:    13-APR-2001
!
! Changes:    26-JUN-2001 RD: Use alcerr for allocation
!             18-Feb-2003 LM: Use backslash from m_bern
!             18-Aug-2010 RD: Special handling for empty unlines
!             10-Feb-2012 RD: Handle empty strings
!             20-Sep-2012 RD: Correctly deallocate arrays
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, lfnerr, backslash

  USE s_alcerr
  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=*)                        :: line
  CHARACTER(LEN=*), DIMENSION(:), POINTER :: argv

! Local Variables
! ---------------
  INTEGER(i4b)                            :: ii
  INTEGER(i4b)                            :: iac
  INTEGER(i4b)                            :: iqt
  INTEGER(i4b)                            :: argc
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE :: beg
  LOGICAL                                 :: quotesOpen

! Remove Leading Blanks
! ---------------------
  line = ADJUSTL(line)

! Scan the Line, count the Arguments
! ----------------------------------
  argc        = 1
  IF (line(1:1) == '"') THEN
    quotesOpen  = .TRUE.
  ELSE
    quotesOpen  = .FALSE.
  END IF

  DO ii = 2, LEN_TRIM(line)
    IF (line(ii:ii) /= ' ') THEN
      IF ( line(ii-1:ii-1) == ' ' .AND. (.NOT. quotesOpen) ) THEN
        argc = argc + 1
      END IF
    END IF

    IF (line(ii:ii) == '"' .AND. line(ii-1:ii-1) /= backslash ) THEN
      quotesOpen = .NOT. quotesOpen
    END IF
  END DO

! Allocate Argument Values
! ------------------------
  IF (quotesOpen) THEN
    WRITE(lfnerr,*) '#** splArg: even number of double quotes'
    RETURN
  END IF

  ALLOCATE( argv(argc), stat=iac )
  CALL alcerr(iac, 'argv', (/argc/), 'splarg')
  ALLOCATE( beg(argc) , stat=iac )
  CALL alcerr(iac, 'beg',  (/argc/), 'splarg')

! Remember the Starting Indices of all Arguments
! ----------------------------------------------
  argc        = 1
  beg(1)      = 1
  IF (line(1:1) == '"') THEN
    quotesOpen  = .TRUE.
  ELSE
    quotesOpen  = .FALSE.
  END IF

  DO ii = 2, LEN_TRIM(line)
    IF (line(ii:ii) /= ' ') THEN
      IF ( line(ii-1:ii-1) == ' ' .AND. (.NOT. quotesOpen) ) THEN
        argc = argc + 1
        beg(argc) = ii
      END IF
    END IF

    IF (line(ii:ii) == '"' .AND. line(ii-1:ii-1) /= backslash ) THEN
      quotesOpen = .NOT. quotesOpen
    END IF
  END DO

! Assign the Argument Values
! --------------------------
  DO ii = 1, argc
    IF (ii < argc) THEN
      argv(ii) = line( beg(ii):beg(ii+1)-1 )
    ELSE
      argv(ii) = line( beg(ii):LEN_TRIM(line) )
    END IF

    ! Remove Leading and Trailing Double Quotes
    ! -----------------------------------------
    IF (LEN_TRIM(argv(ii)) > 0) THEN
      IF (argv(ii)(1:1) == '"') THEN
        argv(ii) = argv(ii)(2:)
      END IF
      IF ( argv(ii)( LEN_TRIM(argv(ii)):LEN_TRIM(argv(ii)) ) == '"' ) THEN
        IF ( LEN_TRIM(argv(ii)) == 1 ) THEN
          argv(ii) = ''
        ELSEIF (argv(ii)( LEN_TRIM(argv(ii))-1:LEN_TRIM(argv(ii))-1 ) /= backslash ) THEN
          argv(ii) = argv(ii)( 1:LEN_TRIM(argv(ii))-1 )
        ENDIF
      ENDIF
    ENDIF

    ! Replace '\"' by '"'
    ! -------------------
    DO
      iqt = INDEX(argv(ii), '\"')
      IF (iqt == 0) EXIT
      argv(ii) = argv(ii)(1:iqt-1) // argv(ii)(iqt+1:)
    END DO
  END DO

  DEALLOCATE(beg, stat=iac)

END SUBROUTINE splArg

END MODULE
