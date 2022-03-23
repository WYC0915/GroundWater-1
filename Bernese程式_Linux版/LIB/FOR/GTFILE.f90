MODULE s_GTFILE
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE gtfile(intnam, nflcol, maxfil, nfil, filnam)

! -------------------------------------------------------------------------
! Purpose:   This is a new version of the old subroutine GTFILE.f.
!            The subroutine prepares the list of file names. It assumes
!            that there is a list of filenames with keyword intnam in the
!            input file. If "nflcol" > 1 the routine uses the keywords
!            intnam // "_EXT_COL_2", intnam // "_EXT_COL_3" etc. to resolve
!            the remaining filenames. The resulting array is vectorized.
!
! Author:    L. Mervart
!
! Created:   13-Apr-2000
! Last mod.: 02-Oct-2007
!
! Changes:   14-Sep-2000  HU: Set nfil=0 if filename blank
!            07-Mar-2001  LM: Forward slash or backslash (Win32 Problem)
!            29-Oct-2001  DI: Increase maxcol 4 --> 5
!            06-Feb-2002  HU: Deallocate keyValue at end
!            17-Feb-2003  LM: Preprocessor problem
!            18-Feb-2003  LM: Use backslash from m_bern
!            23-Apr-2003  CU: Nullify local pointers
!            14-Oct-2003  RD: Do not read keyValue without iostat
!            03-Aug-2006  HU: Add delimiter to path if missing
!            02-Oct-2007  MM: lenPth(iFil) for path corrected
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  USE s_readkeys
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=*)               :: intnam  ! keyword name
  INTEGER(i4b)                   :: nflcol  ! number of columns
  INTEGER(i4b)                   :: maxfil  ! max. number of files per column
  INTEGER(i4b)                   :: nfil    ! number of files per column
  CHARACTER(LEN=*), DIMENSION(*) :: filnam

! Local Variables
! ---------------
  INTEGER(i4b), PARAMETER                              :: maxcol = 5
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=keyValueLength)                        :: keyHlp
  CHARACTER(LEN=fileExtLength), DIMENSION(maxcol)      :: ext
  CHARACTER(LEN=filePathLength), DIMENSION(maxcol)     :: path
  CHARACTER(LEN=1), PARAMETER                          :: fs = '/'
  INTEGER(i4b)                                         :: icol
  INTEGER(i4b)                                         :: irc
  INTEGER(i4b)                                         :: ii
  INTEGER(i4b)                                         :: lenAll
  INTEGER(i4b)                                         :: lenDot
  INTEGER(i4b)                                         :: lenPth
  INTEGER(i4b)                                         :: ifil

  NULLIFY(keyValue)

! Read all Extensions
! -------------------
  IF (nflcol > maxcol .OR. nflcol > 9) THEN
    WRITE(lfnerr,*) ' *** gtfile: too many columns: ', nflcol
    CALL exitrc(2)
  END IF

  DO icol = 2, nflcol
    WRITE(keyHlp,'(a,a,i1)') intnam(1:LEN_TRIM(intnam)), "_EXT_COL_", icol
    CALL readkeys(keyHlp, keyValue, irc)
    IF (irc == 0) THEN
!      READ(keyValue(1),*) ext(icol)
      ext(icol) = ADJUSTL(keyValue(1))
    ELSE
      ext(icol) = '.'
    END IF
  END DO

  DO icol = 2, nflcol
    WRITE(keyHlp,'(a,a,i1)') intnam(1:LEN_TRIM(intnam)), "_PTH_COL_", icol
    CALL readkeys(keyHlp, keyValue, irc)
    IF (irc == 0) THEN
      path(icol)=keyValue(1)

! Add delimiter
      ii = MAX( INDEX(path(icol), '/', BACK=.TRUE.), &
                INDEX(path(icol), backslash, BACK=.TRUE.) )
      IF (ii < LEN_TRIM(path(icol)) .AND. ii > 0) THEN
        path(icol)=TRIM(keyValue(1))//keyValue(1)(ii:ii)
      ENDIF

    ELSE
      path(icol) = '.'
    END IF
  END DO

! Read the original List of Files
! -------------------------------
  CALL readkeys(intnam, keyValue, irc)

  nfil = SIZE(keyValue)
  IF (keyValue(1) == ' ') nfil = 0

  IF (nfil > maxfil) THEN
    WRITE(lfnerr,*) ' *** gtfile: too many files: ', intnam, nfil
    CALL exitrc(2)
  END IF

! Compose the resulting List of Files (Extensions)
! -----------------------------------
  DO ii = 1, nfil
    lenAll = LEN_TRIM(keyValue(ii))
    lenDot = INDEX(keyValue(ii), '.', BACK=.TRUE.)
    DO icol = 1, nflcol
      ifil = (ii-1)*nflcol + icol
      IF (icol == 1) THEN
        filnam(ifil) = keyValue(ii)(1:lenAll)
      ELSE
        IF (ext(icol) /= '.') THEN
          filnam(ifil) = keyValue(ii)(1:lenDot) // ext(icol)
        ELSE
          filnam(ifil) = ' '
        END IF
      END IF
    END DO
  END DO

! Compose the resulting List of Files (Path)
! -----------------------------------
  DO ii = 1, nfil
    DO icol = 1, nflcol
      ifil = (ii-1)*nflcol + icol
      lenPth = MAX( INDEX(filnam(ifil), fs, BACK=.TRUE.), &
                    INDEX(filnam(ifil), backslash, BACK=.TRUE.) )
      IF (icol /= 1 .AND. path(icol) /= '.') THEN
        keyHlp       = TRIM(path(icol))//filnam(ifil)(lenPth+1:)
        filnam(ifil) = keyHlp
      END IF
    END DO
  END DO

  DEALLOCATE(keyValue,stat=irc)

END SUBROUTINE gtfile

END MODULE
