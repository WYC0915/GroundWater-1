
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE p_rxobv3

! -------------------------------------------------------------------------
! Purpose:    This module defines structures for program RXOBV3
!
! Author:     R. Dach
!
! Created:    15-Jun-2003
!
! Changes:    18-JUN-2001 HB: Use d_stacrx instead of m_stacrux
!             23-OCT-2001 RD: Use the new tables only
!             18-May-2003 HU: Nullify pointers
!             28-May-2003 RD: Remove t_rxobv3_crx
!                             (not necessary anymore after RXOBV3 update)
!             23-Jun-2003 HB: Add tMrkTyp to t_rxobv3_err
!             08-Sep-2003 HU: Add radome to t_rxobv3_err
!             19-Dec-2003 SS: Add corRad to t_rxobv3_err
!             01-Apr-2004 CU: Add verFrq to t_rxobv3_err
!             28-Sep-2005 RD: Input option for event flag handling
!             02-Feb-2012 RD: Define requirements on a Bernese obs. file
!             15-May-2012 RD: Use m_bern with ONLY, remove unused modules
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b
  USE m_global, ONLY: maxsys

! Options for requirements on the Bernese observation files
! ---------------------------------------------------------
  TYPE t_rxobv3_req
    LOGICAL      :: ireq    ! Switch whether the requirements are active or not
    INTEGER(i4b) :: reqepo  ! Minimum number of epochs per file
    INTEGER(i4b) :: reqamb  ! Maxmimum number of ambiguities allowed per file
    INTEGER(i4b) :: phonly  ! Allow stations with only phase data
    INTEGER(i4b) :: cdonly  ! Allow stations with only code data
    INTEGER(i4b), DIMENSION(3,0:maxsys) &
                 :: minnum  ! minimum number per file and GNSS:
                            ! (1,iSys): satellites
                            ! (2,iSys): code observations
                            ! (3,iSys): phase observations
                            ! iSys == 0 means all systems
  END TYPE t_rxobv3_req

! Options for error handling in the RINEX files
! ---------------------------------------------
  TYPE t_rxobv3_err
    LOGICAL      :: wrtObs ! (do not) write obs. file
    LOGICAL      :: stperr ! (do not) stop pgm with error
    INTEGER(i4b) :: tStanam ! options for check stanam
    LOGICAL      :: abbStn  ! Try also RINEX file name
    INTEGER(i4b) :: tAnttyp ! options for check Rec/Ant type
    INTEGER(i4b) :: Trnchr  ! Length of String to check (16/20)
    INTEGER(i4b) :: Radome  ! Consider radome code
    INTEGER(i4b) :: corRad  ! Correct position of radome code
    INTEGER(i4b) :: tAntnum ! options for check Rec/Ant num.
    INTEGER(i4b) :: tAntpos ! options for check Ant. pos.
    INTEGER(i4b) :: tMrkTyp ! options for check Marker type
    INTEGER(i4b) :: verSta  ! 0: no check
                            ! 1: stanam == marker name
                            ! 2: stanam == marker number
                            ! 3: stanam == marker name+number (IGS like)
    LOGICAL      :: verFil  ! stanam(1:4) == filnam(1:4)
    INTEGER(i4b) :: verAnt  ! 0: no check
                            ! 1: warning
                            ! 2: skip file
                            ! 3: stop with error
    INTEGER(i4b) :: verFrq  ! 1: warning
                            ! 2: skip file
                            ! 3: stop with error
                            ! 4: update frequency file
    INTEGER(i4b) :: epoFlag ! What to do in case of an event flag (apart 1/4)
                            ! 1: warning
                            ! 2: skip file
                            ! 3: stop with error
  END TYPE t_rxobv3_err

CONTAINS

! Init requirements record
! ------------------------
  SUBROUTINE init_rxobv3_req(req)
    TYPE(t_rxobv3_req) :: req
    req%ireq   = .FALSE.
    req%reqepo = 0
    req%reqamb = 0
    req%phonly = 1
    req%cdonly = 1
    req%minnum = 0
  END SUBROUTINE init_rxobv3_req


END MODULE p_rxobv3
