MODULE s_GTFLNA
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE gtflna(iexist, intnam, filnam, irc)

! -------------------------------------------------------------------------
! Purpose:    This is a new version of the old subroutine GTFLNA.f. This
!             new routine serves old programs as an interface to the new
!             routine READKEYS. New programs should use READKEYS directly.
!
! Author:     L. Mervart
!
! Created:    29-Mar-2000
! Last mod.:  12-May-2004
!
! Changes:    02-Nov-2000  CU: Remove COMMON/CCFLNA
!             08-Jun-2001  RD: No error msg if iexist == 0
!             04-Jan-2002  CU: Deallocate keyValue if keyword not exist,
!                              format change of error message (to the old
!                              format statment V4.2)
!             05-Feb-2002  HU: Deallocate keyValue in any case at end
!             23-May-2002  RD: Nullify keyValue, use tstkey
!             12-May-2004  RD: Add description to the error message
!
! SR called:  tstkey, readKeys, exitrc, descrp
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern

  USE s_descrp
  USE f_tstkey
  USE s_readkeys
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input
  INTEGER(i4b)                  :: iexist ! existence flag:
                                          ! 1 ... non-blank filnam must exist
  CHARACTER(LEN=*)              :: intnam ! internal file name

! output
  CHARACTER(LEN=*)              :: filnam ! external file name
  INTEGER(i4b)                  :: irc    ! return code (0 ... filnam found)

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
          DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=50)             :: optTxt

  INTEGER(i4b)                  :: iac


  IF (intnam == 'DEFNAM') RETURN

! Init
! ----
  filnam = ''

  NULLIFY(keyValue)

! Check existence of the keyword, if file is not requested
! --------------------------------------------------------
  IF (iexist == 1 .OR. tstKey(intnam)) THEN
    CALL readkeys(intnam, keyValue, irc)
    filnam = keyValue(1)
  ENDIF
  DEALLOCATE(keyValue,stat=iac)

  IF (filnam /= '') THEN
    irc = 0
  ELSE
    irc = 1
    IF (iexist == 1) THEN
      CALL descrp(intnam,50,optTxt)
      IF (LEN_TRIM(optTxt) == 0) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,/)')                        &
              ' *** SR GTFLNA: File name not found or blank',  &
                              'Keyword:  ' // TRIM(intnam)
      ELSE
        WRITE(lfnerr,'(/,A,2(/,16X,A),/)')                     &
              ' *** SR GTFLNA: File name not found or blank',  &
                              'Keyword:  ' // TRIM(intnam),    &
                              'Option:   ' // TRIM(optTxt)
      ENDIF
      CALL exitrc(2)
    END IF
  END IF

END SUBROUTINE gtflna


END MODULE
