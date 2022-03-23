MODULE s_PRIVEL
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE privel(nAllSta,allStaName,allStaNum,xStell,xVel, &
                  nFix,veFix,nFree,veFree)

! -------------------------------------------------------------------------
! Purpose:    Print a priori station velocities
!
! Author:     R.Dach
!
! Created:    07-Sep-2001
! Last mod.:  03-Feb-2011
!
! Changes:    10-Dec-2002 CU: Change format of some title lines
!             03-Feb-2011 SL: use m_bern with ONLY, print 5 digits instead of 4
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b,r8b,staNameLength,fileNameLength,lfnPrt

  USE s_eccell
  USE s_gtflna

  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                 :: nAllSta    ! number of all stations
  CHARACTER(LEN=staNameLength), &
                DIMENSION(*)   :: allStaName ! station names
  INTEGER(i4b), DIMENSION(*)   :: allStaNum  ! station numbers
  REAL(r8b),    DIMENSION(3,*) :: xStell     ! Station coordinates (ell.)
  REAL(r8b),    DIMENSION(3,*) :: xVel       ! Station velocity
  INTEGER(i4b)                 :: nFix       ! Number of fixed velocities
  INTEGER(i4b), DIMENSION(*)   :: veFix      ! Index for fixed velocities
  INTEGER(i4b)                 :: nFree      ! Number of free net. velocities
  INTEGER(i4b), DIMENSION(*)   :: veFree     ! Index for free net. velocities

! output:

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength) :: velFil
  CHARACTER(LEN=5)              :: estFix

  INTEGER(i4b)                  :: iSta,iFix,iFree
  INTEGER(i4b)                  :: ii,irc

  REAL(r8b), DIMENSION(3)       :: xVel2     ! Topocentric velocities

! Write the title
! ---------------
  CALL gtflna(0,'VELAPR',velFil,irc)
!  IF (irc /= 0) RETURN

  WRITE(lfnprt,'(A,/,5(/,A))')                                           &
    ' A priori station velocities:               ' // TRIM(velFil),      &
    '                                         A priori station '      // &
    'velocities              A priori station velocities',               &
    '                                                  Geocentric'    // &
    '                              Topocentric',                         &
    ' -------------------------------------------------------'        // &
    '--------------------------------------------------------'        // &
    '--------------------',                                              &
    ' num  Station name         e/f/c   X (m/year)  Y (m/year)  '     // &
    'Z (m/year)       N (m/year)  E (m/year)  U (m/year)',               &
    ' -------------------------------------------------------'        // &
    '--------------------------------------------------------'        // &
    '--------------------'

! Loop the stations
! -----------------
  DO iSta = 1, nAllSta

! Velocity fixed?
! ---------------
    estFix='ESTIM'
    DO iFix = 1,nFix
      IF(veFix(iFix) == iSta) THEN
        estFix='FIXED'
        EXIT
      ENDIF
    ENDDO

! Free network?
! -------------
    DO iFree = 1, nFree
      IF(veFree(iFree) == iSta) THEN
        estFix='HELMR'
        EXIT
      ENDIF
    ENDDO

! Get the topocentric velocity
! ----------------------------
    CALL eccell(xStell(:,iSta),xVel(:,iSta),xVel2)

! Print the line
! --------------
    WRITE(lfnprt,'(I4,2X,A16,5X,A5,3F12.5,5X,3F12.5)') &
          allStaNum(iSta),allStaName(iSta),estFix, &
          (xVel(ii,iSta),ii=1,3),(xVel2(ii),ii=1,3)

  ENDDO

  WRITE(lfnprt,'(/)')

  RETURN

  END SUBROUTINE privel

END MODULE
