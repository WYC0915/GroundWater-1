MODULE s_RDSCRA
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdscra(mxclcq,lfnaux,jtyp,jnum,tobs,locq,xxx0,xtmp,atmp,ios)

! -------------------------------------------------------------------------
! Purpose:    This subroutine reads the scratch file. Search for parameter
!             type jtyp.
!
! Author:     C. Urschl
!
! Created:    10-Mar-2002
! Last mod.:  11-Jun-2006
!
! Changes:    25-Apr-2003 DS: Read TOBS for KIN estimates
!             11-Jun-2006 HB: Read also a priori values for KIN
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern

  USE f_ikf
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                   :: mxclcq    ! Size of locq
  INTEGER(i4b)                   :: lfnaux    ! Scratch file
  INTEGER(i4b)                   :: jtyp      ! Parameter type

! output:
  INTEGER(i4b)                   :: ios       ! = 0
                                              ! = 1: end of file
  INTEGER(i4b)                   :: jnum      ! Station number
  REAL(r8b)                      :: tobs      ! MJD for clock value
  INTEGER(i4b), DIMENSION(mxclcq,3) :: locq   ! Parameter characterization list
  REAL(r8b), DIMENSION(3)        :: xxx0      ! apriori value
  REAL(r8b), DIMENSION(3)        :: xtmp      ! solution
  REAL(r8b), DIMENSION(6)        :: atmp      ! element of covariance matrix

! Local Variables
! ---------------
  INTEGER(i4b)                    :: ii, jj


  ios = 0

! Read next line of scratch file
  READ(lfnaux,IOSTAT=ios) jtyp,jnum

  IF (ios /= 0) RETURN

! Read kinematic coordinates
  IF (jtyp == 21) THEN
    DO jj = 1, 3
      READ(lfnaux) tobs,(locq(ii,jj),ii=1,mxclcq), &
           xxx0(jj),(atmp(ikf(jj,ii)),ii=1,3),   &
                    xtmp(jj)
    ENDDO
  ENDIF

! Read epoch wise station or satellite clocks
  IF (jtyp == 23 .OR. jtyp == 24) THEN
    READ(lfnaux) tobs,(locq(ii,1),ii=1,mxclcq), &
                 xxx0(1),xtmp(1),atmp(1)
  ENDIF

  RETURN

END SUBROUTINE rdscra

END MODULE
