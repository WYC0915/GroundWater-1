MODULE s_GTATTFLG
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE gtattflg(leosvn,epo,attflag)

! -------------------------------------------------------------------------
! Purpose:    Reads structure satfil (file "SATELL") and for specific epoch
!             returns attitude flag
!
! Author:     D. Svehla
!
! Created:    19-Mar-2001
! Last mod.:  16-May-2003
!
! Changes:    16-Dec-2001 HU: Use implicit none
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             16-May-2003 CU: Initialize structure
!
! SR used:    exitrc, gtflna, rdsatfil
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

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
  INTEGER(i4b)                 :: leosvn    ! LEO number (PRN)
  REAL(r8b)                    :: epo       ! Epoch in MJD


!
! OUT :
! -----
  INTEGER(i4b)                 :: attflag   ! Attitude flag


!
! Dummy list
! ----------
  TYPE(t_satfil), SAVE         :: satfil

!
! Local Variables
! ---------------
  INTEGER(i4b)                 :: ii,irc

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
  DO ii = 1, satfil%nsatellite
    IF (satfil%satellite(ii)%svn==leosvn.AND.                             &
    &   epo.GE.satfil%satellite(ii)%timint%t(1).AND.                      &
    &   epo.LE.satfil%satellite(ii)%timint%t(2)) THEN
       attflag=satfil%satellite(ii)%attflag
      GOTO 900
    END IF
  END DO

  WRITE(lfnerr,'(A,/,16X,A,I4,/)')                                        &
        '*** SR GTATTFLG: attitude flag not found',                       &
                        '    LEO number: ',leosvn
  CALL exitrc(2)

!
! END
! ===

900  RETURN

END SUBROUTINE gtattflg

END MODULE
