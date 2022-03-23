MODULE d_dcbFil

! --------------------------------------------------------------------------
! Bernese Software Version 5.2
! --------------------------------------------------------------------------

! --------------------------------------------------------------------------
! Purpose:      Data types for Bernese DCB file
! --------------------------------------------------------------------------
! Module provides:
! ---------------
! Globals:      none
!
! Parameters:   maxDcbTyp
!
! Data types:   t_dcbVal, t_dcbFil
!
! Operators:    none
!
! Subroutines:  init_dcbFil
!
! Functions:    none
! --------------------------------------------------------------------------
!
! Author:    M. Meindl
!
! Created:   30-Mar-2008
!
! Changes:   08-Jul-2011 LP: Adapt to BSW5.2 (rmv DEBUG call)
!            21-Jul-2011 LP: Add obstypes to t_dcbval; add b_dcbtyp
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
! --------------------------------------------------------------------------

! Used modules
! ------------
! structures, variables
  USE m_bern,    ONLY: i4b, r8b, staNam2Length
!  USE m_epoch,   ONLY: t_timWin
  USE m_time,    ONLY: t_timint

! operator, methods:

! subroutines, functions:

! No implicit
  IMPLICIT NONE

! save variables
  SAVE

! access rights
  PUBLIC

! module name
  CHARACTER(LEN=8), PARAMETER    :: modNam = 'D_DCBFIL'


! =========================================================================
! Global variable definitions
! =========================================================================


! =========================================================================
! Parameter definitions
! =========================================================================
  INTEGER(i4b), PARAMETER             :: maxDcbTyp=5 ! Max # of DCB types
                                                     ! 1: P1-P2
                                                     ! 2: P1-C1
                                                     ! 3: P2-C2
                                                     ! 4: LC
                                                     ! 5: IFB
  CHARACTER(LEN=4), DIMENSION(1:5)         :: b_dcbtyp1 = &
                    (/'P1P2','P1C1','ISB ','P2C2','IFB '/)
  CHARACTER(LEN=3), DIMENSION(1:5)         :: b_dcbtyp2 = &
                    (/'DCB','DCB','ISB','DCB','IFB'/)


! =========================================================================
! Type definitions
! =========================================================================

! DCB value
! ---------
  TYPE t_dcbVal
    INTEGER(i4b)                      :: svn         ! PRN/system
    CHARACTER(LEN=staNam2Length)      :: staNam      ! Station name
    CHARACTER(LEN=1)                  :: sys         ! Satellite system
    INTEGER(i4b)                      :: typ         ! DCB type
    REAL(r8b)                         :: dcbVal      ! Value
    REAL(r8b)                         :: dcbRms      ! RMS
    CHARACTER(LEN=4)                  :: flg         ! Flag
                                                     ! 'R': relative
                                                     ! 'A': absolute
    TYPE(t_timint)                    :: tim         ! Time window
!    TYPE(t_timWin)                    :: tim         ! Time window
    CHARACTER(LEN=20)                 :: rem         ! Remark
    CHARACTER(LEN=3),DIMENSION(2)     :: obstyp      ! Obstype used
  END TYPE t_dcbVal


! DCB file
! --------
  TYPE t_dcbFil
    REAL(r8b)                           :: fmt       ! Format
    CHARACTER(LEN=80)                   :: title     ! Title line
    INTEGER(i4b)                        :: nVal      ! Number of DCBs
    TYPE(t_dcbVal),DIMENSION(:),POINTER :: dcbLst    ! Array containing DCBs
  END TYPE t_dcbFil


! =========================================================================
! Operator definitions
! =========================================================================


! =========================================================================
! Subroutine definitions
! =========================================================================

  CONTAINS

! -------------------------------------------------------------------------

  SUBROUTINE init_dcbFil(dcbFil)

! -------------------------------------------------------------------------
! Purpose:   initialize structure
!
! Author:    M. Meindl
!
! Created:   30-Mar-2008
!
! Changes:   08-Jul-2011 LP: Remove call to DEBUG SR
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
! -------------------------------------------------------------------------

! Used Modules
! ------------
! structures, variables

! operator, methods:

! subroutines, functions:

! no implicit
    IMPLICIT NONE

! subroutine name
    CHARACTER(LEN=11), PARAMETER   :: srName = 'INIT_DCBFIL'


! List of Arguments
! -----------------
! input:

! output:
    TYPE(t_dcbFil)                 :: dcbFil


! Local Types
! -----------


! Local Parameters
! ----------------


! Local Variables
! ---------------


! Call debug routine
! ------------------

! Initialization of all variables
! -------------------------------
    dcbFil%fmt   = 1.01
    dcbFil%title = ''
    dcbFil%nVal  = 0
    NULLIFY(dcbFil%dcbLst)

! End of subroutine
! -----------------
    RETURN
  END SUBROUTINE init_dcbFil



! =========================================================================
! Function definitions
! =========================================================================



END MODULE d_dcbFil
