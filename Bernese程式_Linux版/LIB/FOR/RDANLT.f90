MODULE s_RDANLT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdanlt(ANLTYP)

! -------------------------------------------------------------------------
! Purpose:    This is the new version of SR RDANLT which reads structure
!             satfil instead of file "SATELL" and returns type of the file
!             "SATELL" (radiation pressure model for GPS)
!
! SR used:    gtflna, rdsatfil
!
! Author:     D. Svehla
!
! Created:    16-Mar-2001                       Last modified : 16-Dec-2001
!
! Changes:    16-Dec-2001 HU: Use implicit none
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             19-May-2003 HU: Initialize structure
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_satfil, ONLY: t_satfil, init_satfil

  USE s_gtflna
  USE s_rdsatfil
  IMPLICIT NONE

!
! Parameters
! ==========
!
! OUT :
! -----
  CHARACTER(LEN=8)             :: ANLTYP  ! TYPE OF FILE
                                          !  =T950101 : T-MODEL    (SCALED)
                                          !  =S950101 : S-MODEL    (SCALED)
                                          !  =Z950101 : Z-MODEL    (SCALED)
                                          !  =OLD     : S-MODEL  (UNSCALED)
                                          !  =C980101 : CODE-MODEL (SCALED)
                                          !  =J980101 : JPL-MODEL  (SCALED)

!
! Dummy list
! ----------
  TYPE(t_satfil), SAVE         :: satfil

!
! Local Variables
! ---------------
  INTEGER(i4b)                 :: IRC

  CHARACTER(LEN=fileNameLength):: filename

  LOGICAL,      SAVE           :: first= .TRUE.

!
! If called for the first time, read the entire satellite file SATELL
! ===================================================================
  IF (first) THEN
    first = .FALSE.

!
! Get the satellite info file name
! ---------------------------------------------------
    CALL gtflna(0,'SATELL ',filename,IRC)

    IF (IRC.NE.0) THEN
        ANLTYP='        '
        GOTO 900
    END IF
!
! Read satellite info file (SATELL)
! ---------------------------------
    CALL init_satfil(satfil)
    CALL rdsatfil(filename,satfil)
  END IF

!
! Read the structure
! ==================
  ANLTYP=satfil%rpmodel

900  RETURN

END SUBROUTINE rdanlt

END MODULE
