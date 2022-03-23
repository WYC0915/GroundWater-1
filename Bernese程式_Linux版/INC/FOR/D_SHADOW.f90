
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_shadow

! -------------------------------------------------------------------------
! Purpose:    Module containing satellite shadow information.
!             (currently only for Moon)
!
! Author:     U. Hugentobler
!
! Created:    12-Jan-2002
! Last mod.:  28-Jan-2011
!
! Changes:    04-Feb-2002 HU: Initialization of iecl removed
!             16-Aug-2002 DS: maxmev=14 -> 35
!             13-May-2003 CU: Nullify pointers
!             15-Jan-2004 HU: maxmev=35 -> 70
!             28-Jan-2011 SL: use m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b
  USE m_maxdim, ONLY: maxsat

  INTEGER(i4b),PARAMETER         :: maxmev=70 ! Maximum number of events
                                              ! for one satellite

! Moon shadow
! -------------
TYPE t_mooecl
  INTEGER(i4b)                   :: iecl   ! 1: Eclipse at least for one sat.
  INTEGER(i4b)                   :: nsat   ! Number of satellites
  TYPE(t_mooecl_sat), &
    DIMENSION(:),POINTER         :: sat    ! Infos for individual satellites
END TYPE t_mooecl

TYPE t_mooecl_sat
  INTEGER(i4b)                   :: svn    ! SVN number
  INTEGER(i4b)                   :: nevent ! Number of events
  INTEGER(i4b),DIMENSION(maxmev) :: ievent ! Type of event
                                           ! -3: entering into Earth shadow
                                           ! -2: entering into penumbra
                                           ! -1: entering umbra
                                           !  0: minimum penumbra
                                           !  1: exiting umbra
                                           !  2: exiting penumbra
                                           !  3: exiting Earth shadow
  REAL(r8b),DIMENSION(maxmev)    :: tevent ! Epoch of event (MJD)
  REAL(r8b),DIMENSION(maxmev)    :: eclmin ! Minimum eclipse factor
END TYPE t_mooecl_sat

! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_mooecl(mooecl)
    TYPE(t_mooecl)  :: mooecl

    NULLIFY(mooecl%sat)
    mooecl%nsat = 0
  END SUBROUTINE init_mooecl

END MODULE d_shadow
