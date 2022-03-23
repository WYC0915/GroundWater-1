MODULE s_CHR2SVN
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE chr2svn(svnmod,svnchr,svn)

! -------------------------------------------------------------------------
! Purpose:    Return the full (Bernese) satellite number including the
!             the satellite system identification with adding 100, 200, ...
!             E.g. svnmod=3, svnchr='L' --> svn=903 .
!
!         in: svnmod  : Satellite number modulo 100                     i4b
!             svnchr  : Character of satellite system                  ch*1
!                       Blanks stands for GPS
!        out: svn     : Satellite number (including sat. system)        i4b
!
! Author:     M. Rothacher
!
! Created:    07-JUN-2001
!
! SR used:
!
! Changes:    28-MAR-2012 RD: Remove unused modules
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, lfnerr
  USE m_global, ONLY: g_svnsys

  USE s_exitrc
  IMPLICIT NONE

! Input/Output
! ------------
  INTEGER(i4b)                    :: svn, svnmod
  CHARACTER(LEN=1)                :: svnchr
!
! Local Variables
! ---------------
  INTEGER(i4b)                    :: int100, ichr
  CHARACTER(LEN=1)                :: satchr

!
! Blank = GPS

  satchr = svnchr
  IF (satchr == ' ') satchr = g_svnsys(0)

! Search array "g_svnsys" for the satellite system character
! -----------------------------------------------------------
  int100=-1
  DO ichr=0,9
    IF (g_svnsys(ichr) == satchr) THEN
      int100=ichr
      EXIT
    ENDIF
  ENDDO

! Check if satellite system character found

  IF (int100 == -1) THEN
    WRITE(lfnerr,'(" *** SR CHR2SVN: INVALID SATELLITE SYSTEM", &
                & " CHARACTER",17X,"CHARACTER: ",A,/)') svnchr
    CALL EXITRC(2)
  ENDIF

! Full satellite number

  svn = int100*100+svnmod

  RETURN

END SUBROUTINE chr2svn

END MODULE
