MODULE s_SATBLK
CONTAINS

! -----------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -----------------------------------------------------------------------------

SUBROUTINE satblk(prnNr,epoch,iFrq,blkNr)

! -----------------------------------------------------------------------------
! Purpose:    Returns the satellite block number from satellite information
!             file for a specific satellite number and epoch.
!             Subroutine returns 0 if satellite could not be found and -1 if
!             no SATELLIT file is specified
!
! Author:     M. Meindl
!
! Created:    15-Feb-2003
! Last mod.:  16-Sep-2005
!
! Changes:    10-Apr-2003 MM: Include satellite frequency
!             16-May-2003 HU: Initialize structure
!             16-Sep-2005 AG: Changed structure of SATELLIT.-file implemented
!
! SR used:    gtflna, rdsatfil
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -----------------------------------------------------------------------------

  USE m_bern
  USE d_satfil, ONLY: t_satfil, init_satfil

  USE s_gtflna
  USE s_rdsatfil
  IMPLICIT NONE


! Satellite information
! ---------------------
  TYPE(t_satfil), SAVE  :: satFil


! Variables
! ---------
  INTEGER(i4b)                      :: prnNr           ! PRN
  INTEGER(i4b)                      :: iFrq            ! frequency number
  INTEGER(i4b)                      :: blkNr           ! block number
                                                       ! (0: not found,
                                                       ! -1: no SATELL file)
  REAL(r8b)                         :: epoch           ! epoch in MJD

  CHARACTER(LEN=fileNameLength)     :: filNam
  INTEGER(i4b)                      :: irc, iSat
  LOGICAL, SAVE                     :: first = .TRUE.


! First call, read satellite info file
! ------------------------------------
  IF (first) THEN
    CALL gtflna(0,'SATELL',filNam,irc)
    IF (irc/=0) THEN
      blkNr = -1
      RETURN
    END IF
    CALL init_satfil(satFil)
    CALL rdsatfil(filNam,satFil)
    first = .FALSE.
  END IF


! Search for satellite block number
! ---------------------------------
  blkNr = 0
  iFrq  = 0
  DO iSat=1, satFil%nSatellite
    IF (satFil%satellite(iSat)%svn          ==  prnNr .AND.                  &
        satFil%satellite(iSat)%timInt%t(1) .LE. epoch .AND.                  &
        satFil%satellite(iSat)%timInt%t(2) .GE. epoch      ) THEN
      blkNr = satFil%satellite(iSat)%iBlock
      EXIT
    END IF
  END DO
  DO iSat=1, satFil%nSensor
    IF (satFil%sensor(iSat)%svn          ==  prnNr .AND.                  &
        satFil%sensor(iSat)%timInt%t(1) .LE. epoch .AND.                  &
        satFil%sensor(iSat)%timInt%t(2) .GE. epoch      ) THEN
      iFrq  = satFil%sensor(iSat)%iFrq
      EXIT
    END IF
  END DO

  RETURN
END SUBROUTINE satblk

END MODULE
