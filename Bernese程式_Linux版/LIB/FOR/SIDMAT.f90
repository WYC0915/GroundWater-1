MODULE s_SIDMAT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE sidmat(tt,xpol,ypol,sz,sid)

! -------------------------------------------------------------------------
! Purpose:    Computation of rotation matrix containing sidereal time,
!             polar motion and position of TEO on equator of CIP
!             (if IAU 2000).
!
! Remarks:    The name of the nutation model is checked using
!             sr rdnutsub, i.e., relying on the existence of the keywords
!             NUTMOD and SUBMOD in the input panel.
!             If the nutation name string contains IAU2000, the position
!             of TEO is taken into account.
!
! Author:     H. Bock
!
! Created:    28-Jun-2005
! Last mod.:  11-May-2011
!
! Changes:    18-Dec-2005 HU: Sign of ss corrected
!             18-Oct-2006 HU: Correct set up of pol matrix
!             11-May-2011 HB: Update nutmod-names
!
! SR used:    gtflna, rdnutsub, ddreh
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_const, ONLY : pi
  USE s_gtflna
  USE s_rdnutsub
  USE s_ddreh

  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  REAL(r8b)                 :: tt             ! MJD Epoch in TT
  REAL(r8b)                 :: xpol           ! x-pole coordinate
  REAL(r8b)                 :: ypol           ! y-pole coordinate
! in/output:
  REAL(r8b)                 :: sz             ! sidereal time
                                              ! output: for IAU2000, corrected
                                              !         with ss =>
                                              ! Position of TEO on equator
                                              ! of CIP

! output:
  REAL(r8b),DIMENSION(3,3)  :: sid            ! Rotation matrix containing
                                              ! sidereal time, polar motion,
                                              ! and position of TEO on equator
                                              ! of CIP (only IAU2000)

! Local parameters
! ----------------

! Local functions
! ---------------

! Local variables
! ---------------
  REAL(r8b)                 :: tim
  REAL(r8b)                 :: ss
  REAL(r8b),DIMENSION(3,3)  :: SID1
  REAL(r8b),DIMENSION(3,3)  :: PM,polx,poly
  INTEGER(i4b),SAVE         :: iuse
  INTEGER(i4b),SAVE         :: ifirst=1
  INTEGER(i4b)              :: ircnut
  INTEGER(i4b)              :: ircsub
  CHARACTER(LEN=16)         :: nutnam
  CHARACTER(LEN=16)         :: subnam
  CHARACTER(LEN=fileNameLength) :: filnam


! Initialization
! --------------
  sid = 0.D0

! Automatic detection of convention to be used for computing sidereal time
! ------------------------------------------------------------------------
  IF (ifirst == 1) THEN
    ifirst=0
    CALL gtflna(0,'NUTMOD',filnam,ircnut)
    CALL gtflna(0,'SUBMOD',filnam,ircsub)
    IF (ircnut==1 .OR. ircsub==1) THEN
      WRITE(lfnerr,"(/,' ### SR SIDMAT: Nutation model not found', &
           & /,'                Conventions 2000 used',/)")
      iuse=2
    ELSE
      CALL rdnutsub(nutnam,subnam)
      iuse=0
      IF (INDEX(nutnam,"IAU80")   > 0) iuse=1
      IF (INDEX(nutnam,"IAU2000") > 0) iuse=2
      IF (INDEX(nutnam,"IAU2000R06") > 0) iuse=2
      IF (INDEX(nutnam,"IAU2006") > 0) iuse=2
      IF (iuse == 0) THEN
        WRITE(lfnerr,"(/,' ### SR SIDMAT: Nutation model not identified', &
             & /,'                Conventions 2000 used', &
             & /,'                Name: ',A,/)") nutnam
        iuse=2
      ENDIF
    ENDIF
  ENDIF

  ss = 0.D0
  IF (iuse == 2) THEN
! Position of TEO on equator of CIP (for IAU2000)
    tim=(tt-51544.5D0)/36525D0
    ss = -0.000047*tim*pi/648000.D0
  ENDIF
  sz = sz+ss
  CALL ddreh(3,sz,SID1)

! Polar motion
! ------------
!!  PM=0.D0
!!  PM(1,1)= 1.D0
!!  PM(1,3)= xpol
!!  PM(2,2)= 1.D0
!!  PM(2,3)=-ypol
!!  PM(3,1)=-xpol
!!  PM(3,2)= ypol
!!  PM(3,3)= 1.D0
  CALL DDREH(2,-xpol,polx)
  CALL DDREH(1,-ypol,poly)
  PM = MATMUL(polx,poly)

! Rotation matrix
! ---------------
  SID=MATMUL(PM,SID1)

  RETURN

  END SUBROUTINE sidmat

END MODULE
