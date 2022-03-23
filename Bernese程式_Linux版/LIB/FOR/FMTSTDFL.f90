MODULE s_FMTSTDFL
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fmtstdfl(maxfil, nfil, filnam)

! -------------------------------------------------------------------------
!
! Purpose:    This subroutine reads the list of files for program FMTSTD
!
! Author:     C. Urschl
!
! Created:    13-Oct-2000
! Last mod.:  23-Apr-2003
!
! Changes:    23-Apr-2003 CU: Nullify local pointers
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  USE s_readkeys
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b)                     :: maxfil  ! max.   number of files
  INTEGER(i4b)                     :: nfil    ! actual number of files
  CHARACTER(LEN=*), DIMENSION(4,*) :: filnam  ! file names

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=3)                                     :: ext_std
  CHARACTER(LEN=3)                                     :: ext_rpr
  INTEGER(i4b)                                         :: lenDot
  INTEGER(i4b)                                         :: ii
  INTEGER(i4b)                                         :: iFil       ! Counter
                                                                     ! for files
  INTEGER(i4b)                                         :: numfil
  INTEGER(i4b)                                         :: irc, ioerr


  NULLIFY(keyValue)

! Create Stand.Orbit and RPR Output Files
! ---------------------------------------

  ioerr  = 0

  DO iFil = 1, maxFil
    DO ii = 1, 4
      filnam(ii,iFil) = ' '
    END DO
  END DO

  CALL readkeys('EXT_STD' , keyValue, irc)
  ext_std = keyValue(1)
  CALL readkeys('EXT_RPR' , keyValue, irc)
  ext_rpr = keyValue(1)

  CALL readkeys('STDASCII' , keyValue, irc)
    nfil = 0
    DO ii = 1, SIZE(keyValue)
      IF (LEN_TRIM(keyValue(ii)) == 0) EXIT
      nfil = nfil + 1
      IF (nfil > maxfil) THEN
        WRITE(lfnerr,*) ' *** SR FMTSTDFL: too many files'
        ioerr = ioerr + 1
      END IF
      filnam(1,nfil) = keyValue(ii)
      lenDot = INDEX(filnam(1,nfil), '.', BACK=.TRUE.)
      filnam(3,nfil) = filnam(1,nfil)(1:lenDot) // ext_std
    END DO

  numfil = nfil

  CALL readkeys('RPRASCII' , keyValue, irc)
    nfil = 0
    DO ii = 1, SIZE(keyValue)
      IF (LEN_TRIM(keyValue(ii)) == 0) EXIT
      nfil = nfil + 1
      IF (nfil > maxfil) THEN
        WRITE(lfnerr,*) ' *** SR FMTSTDFL: too many files'
        ioerr = ioerr + 1
      END IF
      filnam(2,nfil) = keyValue(ii)
      lenDot = INDEX(filnam(2,nfil), '.', BACK=.TRUE.)
      filnam(4,nfil) = filnam(2,nfil)(1:lenDot) // ext_rpr
    END DO

  IF (numfil .GT. nfil) nfil = numfil

  DEALLOCATE(keyValue,stat=irc)

  IF (ioerr /= 0) CALL exitrc(2)

END SUBROUTINE fmtstdfl

END MODULE
