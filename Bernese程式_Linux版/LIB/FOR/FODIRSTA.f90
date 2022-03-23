MODULE s_FODIRSTA
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodirsta(opt,datFil)

! -------------------------------------------------------------------------
! Purpose:    Read STA file
!
! Author:     Luca Ostini
!
! Created:    14-Aug-2008
! Last mod.:  14-Aug-2008
!
! Changes:    14-Aug-2008 LO: Created this file
!             02-Oct-2008 LO: First revision
!             09-Oct-2008 LO: Third revision
!             05-Dec-2008 LO: Fourth revisio: velocity changes allowed
!             11-Feb-2009 LO: Fifth revision: major changes
!             25-Sep-2009 LO: Changes for F90 consistency
!             21-Dec-2009 LO: FFT removed and several changes apported
!             26-Aug-2010 LO: Architectural changes
!             03-Jan-2010 LO: INTENT(INOUT) removed and bug fixed
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used Modules
! ------------
! structures, variables:
  USE m_bern,    ONLY: shortLineLength
  USE m_maxdim,  ONLY: maxsta
  USE p_fodits,  ONLY: t_opt, t_datFil

! operator, methods:
!  USE d_debug,   ONLY: debug_entry, debug_exit
  USE d_stacrx,  ONLY: init_stacrux

! subroutines, functions:
  USE s_readcrux

! no implicit
  IMPLICIT NONE

! subroutine name


! List of Arguments
! -----------------
! input:
  TYPE(t_opt)                    :: opt        ! Option structure
  TYPE(t_datFil)                 :: datFil     ! I/O files structure

! input/output:

! output:


! Local Types
! -----------


! Local Parameters
! ----------------


! Local Variables
! ---------------


! Call debug routine
! ------------------
!  CALL debug_entry(srName)


! Initialization of all variables
! -------------------------------
  ! Initialize Station Information File (STA-CRX)
  CALL init_stacrux(datFil%staIn)
  CALL init_stacrux(datFil%staOut)

  ! Return if the STA file is not present
  IF( LEN_TRIM(opt%inStaFile) == 0 )THEN
     RETURN
  END IF

  ! Read Station Information File (STA-CRX)
  CALL readCrux(opt%inStaFile,datFil%staIn)

! End of subroutine
! -----------------
!  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodirsta

END MODULE s_FODIRSTA
