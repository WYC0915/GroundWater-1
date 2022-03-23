MODULE s_GTOFFUNI
CONTAINS

! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

SUBROUTINE gtoffuni(nsat,svn,sensor,epo,antoff,bore,azim)

! ------------------------------------------------------------------------------
! Purpose:    Reads structure satfil (file "SATELL") and for specific epoch
!             returns sensor offsets, sensor boresight vector and
!             sensor reference azimuth vector
!
! Author:     D. Svehla
!
! Created  :  16-Mar-2001
! Last mod.:  11-Aug-2003
!
! Changes:    13-Jun-2001 HB: USE m_bern
!             08-Aug-2001 HB: make capable for more than one satellite,
!                              add parameter nsat
!             16-Dec-2001 HU: Use implicit none
!             16-May-2003 CU: Initialize structure
!             11-Aug-2003 RS: Check whether sensor name is blank
!
! SR used:    exitrc, gtflna, rdsatfil
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ------------------------------------------------------------------------------

  USE m_bern
  USE d_satfil, ONLY: t_satfil, init_satfil
  USE s_exitrc
  USE s_gtflna
  USE s_rdsatfil
  IMPLICIT NONE

! Parameters
! ==========
!
! IN :
! ----
  INTEGER(i4b)                 :: nsat      ! number of satellites
  INTEGER(i4b),DIMENSION(*)    :: svn       ! number (PRN) of satellites
  CHARACTER(LEN=staNameLength),DIMENSION(*) :: sensor    ! Sensor
  REAL(r8b)                    :: epo       ! Epoch in MJD

!
! OUT :
! -----
  REAL(r8b),   DIMENSION(3,*)    :: antoff    ! Sensor offsets
  REAL(r8b),   DIMENSION(3,*)    :: bore      ! Sensor boresight vector
  REAL(r8b),   DIMENSION(3,*)    :: azim      ! Sensor reference azimuth vector

!
! Dummy list
! ----------
  TYPE(t_satfil), SAVE         :: satfil

!
! Local Variables
! ---------------
  INTEGER(i4b)                 :: isat
  INTEGER(i4b)                 :: ii,iok,irc

  CHARACTER(LEN=fileNameLength):: filename

  LOGICAL,      SAVE           :: first= .TRUE.

!
! If called for the first time, read the entire satellit file SATELL
! ==================================================================
  IF (first) THEN
    first = .FALSE.
!
! Get the satellite info file name
! ---------------------------------------------------
    CALL gtflna(1,'SATELL ',filename,IRC)
!
! Read satellite info file (SATELL)
! ---------------------------------
    CALL init_satfil(satfil)
    CALL rdsatfil(filename,satfil)
  END IF

!
! Read the structure
! ==================

  DO isat = 1, nsat
    iok=0
    DO ii = 1, satfil%nsensor
      IF (satfil%sensor(ii)%svn==svn(isat).AND.                              &
           &   satfil%sensor(ii)%sensor==sensor(isat).AND.                   &
           &   sensor(isat)/=' '.AND.                                        &
           &   epo.GE.satfil%sensor(ii)%timint%t(1).AND.                     &
           &   epo.LE.satfil%sensor(ii)%timint%t(2)) THEN

        antoff(:,isat)=satfil%sensor(ii)%antoff(:)
        bore(:,isat)=satfil%sensor(ii)%bore(:)
        azim(:,isat)=satfil%sensor(ii)%azim(:)
        iok=1
        EXIT
      END IF
    END DO
!
! Sensor information not found
    IF (iok == 0) THEN
      WRITE(lfnerr,'(A,/,16X,A,I4,/,16X,A,A,/)')                             &
            '*** SR GTOFFUNI: sensor offsets and unit vector not found',     &
                            '    SVN number: ',svn(isat),                    &
                            'Onboard sensor: ',sensor(isat)
      CALL exitrc(2)
    ENDIF
!
  END DO

!
! END
! ===

  RETURN

END SUBROUTINE gtoffuni

END MODULE
