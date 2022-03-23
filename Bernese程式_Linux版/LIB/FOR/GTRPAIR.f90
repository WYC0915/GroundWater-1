MODULE s_GTRPAIR
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE gtrpair(leosvn,epo,qdm,crad,airmod,cdrag)

! -------------------------------------------------------------------------
! Purpose:    Reads structure satfil (file "SATELL") and for specific epoch
!             returns parameters for non-conservative forces
!
! Author:     D. Svehla
!
! Created:    22-Feb-2002
! Last mod.:  16-May-2003
!
! Changes:    16-May-2003 CU: Initialize structure
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

  REAL(r8b)                    :: qdm       ! Area-mass ratio
  REAL(r8b)                    :: crad      ! Radiation pressure coefficient
  INTEGER(i4b)                 :: airmod    ! Air-drag model
  REAL(r8b)                    :: cdrag     ! Air-drag coefficient


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
!
      qdm   =satfil%satellite(ii)%formf
      crad  =satfil%satellite(ii)%radpres
      airmod=satfil%satellite(ii)%admodel
      cdrag =satfil%satellite(ii)%adrag
!
      GOTO 900
    END IF
  END DO

  WRITE(lfnerr,'(A,/,16X,A,I4,/,16X,A,F14.6,/)')                              &
 &  '*** SR GTRPAIR: Param. for non-cons. forces not found in Satellite file',&
 &                   '    LEO number: ',leosvn,                               &
 &                   'Requested time: ',epo
  CALL exitrc(2)

!
! END
! ===

900  RETURN

END SUBROUTINE gtrpair

END MODULE
