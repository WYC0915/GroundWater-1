MODULE s_svn2chr
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE svn2chr(svn,svnmod,svnchr)

! -------------------------------------------------------------------------
! Purpose:    Return the satellite number modulo 100 and the character
!             of the satellite system the satellite "svn" belongs to.
!             E.g. svn=903 --> svnmod=3, svnchr='L'.
!
!         in: svn     : Satellite number to be checked                  i4b
!        out: svnmod  : Satellite number modulo 100                     i4b
!             svnchr  : Character of satellite system                  ch*1
!
! Author:     M.Rothacher
!
! Created:    07-Jun-2001
!
! Changes:    29-Sep-2011 SL: use m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, lfnErr
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
  INTEGER(i4b)                    :: int100

! Get correct satellite system character from global variable
! "g_svnsys"
! -----------------------------------------------------------
  int100 = svn/100

! Check validity of satellite number
  IF(int100 < 0 .OR. int100 > 10) THEN
    WRITE(lfnerr,'(" *** SR SVN2CHR: INVALID SATELLITE NUMBER",/, &
                & 17X,"SATELLITE NUMBER: ",I4,/)') svn
    CALL EXITRC(2)
  ENDIF

! Satellite System Character
  svnmod = MOD(svn,100)
  svnchr = g_svnsys(int100)

  RETURN

END SUBROUTINE svn2chr

END MODULE
