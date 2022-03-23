MODULE f_svnsys
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

LOGICAL FUNCTION svnsys(isvtyp,nsat,satnum)

! -------------------------------------------------------------------------
! Purpose:    Check the satellite list for GPS, GLONASS, LEO satellites.
!             The function returns .TRUE. or .FALSE. depending on whether
!             the satellite system to be checked is present.
!
!         in: isvtyp  : Satellite type to be checked for                i4b
!                       = -1 check whether GPS AND GLONASS present
!                       =  0 check whether GPS is present
!                       =  1 check whether GLONASS is present
!                       =  2 check whether GALILEO is present
!                       =  9 check whether a LEO is present
!                       = 10 check whether a mix of systems is present
!             nsat    : Number of satellites                            i4b
!             satnum  : Satellite numbers                             i4b(*)
!        out: svnsys  : Answer to the check. Either .TRUE. or .FALSE. logical
!
! Author:     M. Rothacher
!
! Created:    05-Jun-2001
!
! Changes:    08-Nov-2002 HU: GALILEO system added
!             28-Mar-2012 RD: Use SVN2CHR as module now
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, lfnerr
  USE m_global, ONLY: g_svnsys

  USE s_svn2chr
  USE s_exitrc
  IMPLICIT NONE

! Input/Output
! ------------
  INTEGER(i4b)                    :: isvtyp, nsat
  INTEGER(i4b),     DIMENSION(*)  :: satnum

! Local Variables
! ---------------
  LOGICAL                         :: gpsflg, gloflg
  INTEGER(i4b)                    :: isat, modsat
  CHARACTER(LEN=1)                :: svnchr, satchr

  svnsys = .FALSE.

! Check if GLONASS AND GPS present
! ================================
  IF (isvtyp == -1) THEN
    gpsflg = .FALSE.
    gloflg = .FALSE.
    DO isat=1,nsat
      CALL SVN2CHR(satnum(isat),modsat,svnchr)
       IF (svnchr == g_svnsys(0)) gpsflg = .TRUE.
       IF (svnchr == g_svnsys(1)) gloflg = .TRUE.
    ENDDO
    IF (gpsflg .AND. gloflg) svnsys = .TRUE.

! Check if mixed systems present
! ==============================
  ELSE IF(isvtyp == 10) THEN
    CALL SVN2CHR(satnum(1),modsat,satchr)
    DO isat=2,nsat
      CALL SVN2CHR(satnum(isat),modsat,svnchr)
      IF (svnchr .NE. satchr) THEN
        svnsys = .TRUE.
        EXIT
      ENDIF
    ENDDO

! Check for specific system
! =========================
  ELSEIF (isvtyp >= 0 .AND. isvtyp <= 9) THEN
    DO isat=1,nsat
      CALL SVN2CHR(satnum(isat),modsat,svnchr)
      IF (svnchr == g_svnsys(isvtyp)) THEN
        svnsys=.TRUE.
        EXIT
      ENDIF
    ENDDO

! Invalid system type entered: error message
! ==========================================
  ELSE
    WRITE(lfnerr,'(" *** SR SVNSYS: INVALID SATELLITE SYSTEM", &
                &  " IDENTIFIER",/, &
                &  17X,"SYSTEM IDENTIFIER: ",I4,/)') isvtyp
    CALL EXITRC(2)
  ENDIF

  RETURN

END FUNCTION svnsys

END MODULE
