MODULE s_LEOPRN
CONTAINS

! -----------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -----------------------------------------------------------------------------

SUBROUTINE leoprn(leoname,epo,leosvn)

! -----------------------------------------------------------------------------
! Purpose:    Searches the structure satfil (satellite info file SATELL)
!             and returns the LEO PRN, if LEO not found returns 0
!
!         in: leoname : Leo name (sensor)                         staNameLength
!             epo     : epoch in MJD                                  REAL(r8b)
!
!        out: leosvn  : Leo number (PRN)                                    i4b
!                       =0 if leoname not found
!
! Author:     D. Svehla
!
! Created:    09-Feb-2001                           Last modified : 28-Jul-2005
!
! Changes:    16-Dec-2001  HU: Use implicit none
!             21-Dec-2001  HU: Use m_bern, ONLY for modules
!             22-Jun-2002  DS: Use stanamlength
!             15-May-2003  HU: Initialize structures
!             28-Jul-2005  RD: Open string length for leoname
!
! SR used:    gtflna, rdsatfil
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -----------------------------------------------------------------------------

  USE m_bern
  USE d_satfil, ONLY: t_satfil, init_satfil

  USE s_gtflna
  USE s_rdsatfil
  IMPLICIT NONE

! Dummy list
! ----------
  TYPE(t_satfil), SAVE  :: satfil


! Variables
! ---------------
!  CHARACTER(LEN=staNameLength) :: leoname
  CHARACTER(LEN=*)             :: leoname
  CHARACTER(LEN=fileNameLength):: filename

  INTEGER(i4b)                 :: leosvn, icrx, IRC
  REAL(r8b)                    :: epo

  LOGICAL,      SAVE           :: first= .TRUE.


! If called for the first time, read the entire SATELL file
! =========================================================

  IF (first) THEN
    first = .FALSE.

! Get the satellite info file name
! ---------------------------------------------------
    CALL gtflna(1,'SATELL ',filename,IRC)

! Read satellite info file (SATELL)
! ---------------------------------------------------
    CALL init_satfil(satfil)
    CALL rdsatfil(filename,satfil)
  END IF


! Searching the station name in the SATELL-structure
! ==================================================
  leosvn=0
  DO icrx = 1, satfil%nsensor
    IF (satfil%sensor(icrx)%sensor(1:staNameLength)==leoname) THEN
      IF (epo.GE.satfil%sensor(icrx)%timint%t(1).AND.      &
      &   epo.LE.satfil%sensor(icrx)%timint%t(2)) THEN
        leosvn=satfil%sensor(icrx)%svn
        GOTO 100
      END IF
    END IF
  END DO

100  RETURN

END SUBROUTINE leoprn

END MODULE
