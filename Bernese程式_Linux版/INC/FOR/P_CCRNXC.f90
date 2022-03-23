
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE p_ccrnxc

! -------------------------------------------------------------------------
! Purpose:    This module defines structures for program CCRNXC
!
! Author:     R. Dach
!
! Created:    09-Aug-2000
! Last mod.:  20-Jul-2011
!
! Changes:    10-Oct-2000 RD: New min. number of clocks for mean
!             13-Feb-2001 RD: Free selection of the reference clock
!             27-Feb-2001 RD: Inter/extrpolation
!             18-Jun-2001 RD: Select only one ref-clock
!             20-Aug-2001 RD: Time window for clock function components
!             27-Aug-2001 RD: Min number of data points for extrapolation
!             18-May-2003 HU: Nullify pointers
!             21-Jan-2004 RD: Adapt options to new input panel
!             09-Feb-2004 RD: No more overwriting when an outlier in CCOMBO
!             12-Mar-2004 RD: Extract only satellite clocks
!             21-Sep-2009 RD: Handle eclipse flags
!             20-Jul-2011 RD: Extract RXCBV3 function to pgm RNXCLK
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_time, ONLY: t_timint


! Dimensions
! ----------
  REAL(r8b),         PARAMETER   :: null = 1E-12 ! Constant for "==0D0"-test

  INTEGER(i4b),      PARAMETER   :: prtinp = 1   ! Print details on input files
  INTEGER(i4b),      PARAMETER   :: prtcmb = 2   ! Print combination details
  INTEGER(i4b),      PARAMETER   :: prtref = 3   ! Print ref-clock details
  INTEGER(i4b),      PARAMETER   :: prtjmp = 4   ! Print clk-jump details
  INTEGER(i4b),      PARAMETER   :: prtext = 5   ! Print extrapolation details
  INTEGER(i4b),      PARAMETER   :: prtres = 6   ! Print result statistic

! Type for CCRINEXC input options
! -------------------------------
TYPE t_ccrnxc_opt
  REAL(r8b)              :: maxDcrd   ! Coord Diff allowed between the files
  LOGICAL                :: refFil    ! Use first file as reference
  INTEGER(i4b)           :: nref      ! Number of ref. clocks in list
  CHARACTER(LEN=staNameLength),       &
  DIMENSION(:), POINTER  :: refClk    ! List of possible reference clocks
  CHARACTER(len=staNameLength),       &
  DIMENSION(:), POINTER  :: clkLst    ! List of clocks included
  INTEGER(i4b)           :: nAlig     ! Deg. of alignment polynom
  REAL(r8b)              :: maxRef    ! Max. RMS for ref. clk. align.
  LOGICAL                :: oneRef    ! Select only one reference clock
  LOGICAL                :: onlyRef   ! Use only reference clocks for
                                      ! combination
  LOGICAL                :: useSta    ! Use station clocks for combination
  LOGICAL                :: useSat    ! Use satellite clocks for combination
  INTEGER(i4b)           :: delEcl    ! Remove clocks with eclips flag
  REAL(r8b)              :: Sigma0    ! apriori unit of weight (nsec)
  REAL(r8b)              :: MaxResi   ! Maximal residual allowed
  INTEGER(i4b)           :: Combi     ! Combination strategy
                                      !    0: unweighted mean
                                      !    1: weighted mean, weights from
                                      !       combination
                                      !    2: weighted mean, weights from input
                                      !       file
  REAL(r8b)              :: MaxMean   ! Maximal deviation from mean allowed
  INTEGER(i4b)           :: MinClkSta ! Minimum number of valid clocks for mean
                                      ! (Station)
  INTEGER(i4b)           :: MinClkSat ! Minimum number of valid clocks for mean
                                      ! (Satellite)
  INTEGER(i4b)           :: OutSigma  ! Sigmas for the output files
                                      !    1: std. from combination
                                      !    2: error prop. from input file
  INTEGER(i4b),           &
  DIMENSION(prtres)      :: PrtDetail ! Print details into output file
                                      !    prt???: =1 for details
                                      !    prtres: =1 print (non-sorted)
                                      !            =2 print (alphabetic sorted)
                                      !            =3 print (rms linear fit
                                      !               sorted)
  REAL(r8b),DIMENSION(2) :: TimeWin   ! Time window for combination
  INTEGER(i4b)           :: DeltaT    ! Sampling rate for output file
  INTEGER(i4b)           :: iePoly    ! Polynom for inter/extrapolation
  TYPE(t_timint), DIMENSION(:),POINTER :: ieTPoly
  INTEGER(i4b)           :: iesico    ! Peridical terms for inter/extrapolation
  TYPE(t_timint), DIMENSION(:),POINTER :: ieTsico
  REAL(r8b),      DIMENSION(:),POINTER :: iePsico
  REAL(r8b)              :: iemRMS    ! Max. RMS for polynomial fit
  INTEGER(i4b)           :: iemObs    ! Min. # of data points for poly. fit
  LOGICAL                :: doInter   ! Interpolation allowed
  LOGICAL                :: doExtra   ! Extrapolation allowed
  REAL(r8b)              :: jumpRMS   ! Min. RMS for clock jump detection
  INTEGER(i4b)           :: nJmpSig   ! Limit for jump detect. of n*RMS
  INTEGER(i4b)           :: nJmpEpo   ! Number of epoch for an outlier gap
  LOGICAL                :: JmpDelSta ! Delete outliers from jump detection
  LOGICAL                :: JmpDelSat ! Delete outliers from jump detection
  INTEGER(i4b)           :: nJmpPoly  ! Poly. Deg. for jump size est.
  CHARACTER(LEN=60)      :: Title     ! Title line for satellite clock file
!
  INTEGER(i4b)           :: nInterval ! Number of intervals computed
  REAL(r8b)              :: SumRMS    ! Sum of the RMS for the  offset est.
                                      ! (for statistic only)
  INTEGER(i4b)           :: NumDel    ! Number of clock values removed (for
                                      ! statitsic only)
  REAL(r8b)              :: MaxRMS    ! Max. RMS of offset estimation (for
                                      ! statistic only)
  INTEGER(i4b)           :: MaxDel    ! Max. Number of removed obs. (for
                                      ! statistic only)
  CHARACTER(len=19), DIMENSION(2):: MaxRMSEpo ! Epoch for max. RMS of offset
                                              ! estimation
  CHARACTER(len=19), DIMENSION(2):: MaxDelEpo ! Epoch for max. Number of removed
                                              ! obs.
  TYPE(t_timint)         :: ClkData   ! Interval with clock data
END TYPE t_ccrnxc_opt


! Type for CCRNXC rinex clock file names
! --------------------------------------
TYPE t_ccrnxc_fil
  CHARACTER(len=fileNameLength),DIMENSION(:),POINTER   &
                                 :: ClkFilNam  ! List of the Clock Rinex files
  CHARACTER(len=fileNameLength)  :: OutFilNam  ! Result clock rinex file
  CHARACTER(len=fileNameLength)  :: SatFilNam  ! Result Bernese sat.clock file
END TYPE t_ccrnxc_fil


! Clock information record for combination
! ----------------------------------------
TYPE t_clkmean
  INTEGER(i4b)                 :: iFil       ! Clock from file iFil
  INTEGER(i4b)                 :: iClk       ! Index in SatNum or StaNam
  INTEGER(i4b)                 :: iTyp       ! Clock typ: (1=station; 2=satellite)
  REAL(r8b)                    :: Value      ! Clock value with resp to the ref.
                                             ! file
  REAL(r8b)                    :: Sigma      ! Sigma for the clock
  REAL(r8b)                    :: SigOff     ! Sigma for the offset to the ref.
                                             ! file
END TYPE t_clkmean


! Observation index for NEQs
! --------------------------
TYPE t_Index
  INTEGER(i4b), DIMENSION(2)   :: iFil       ! File where an outlier was found
  INTEGER(i4b)                 :: iEpo       ! Epoch index in file
  CHARACTER(len=staNameLength) :: ClkNam     ! Name for the clock
  INTEGER(i4b)                 :: ClkTyp     ! Clock type (1: Sta, 2: Sat)
END TYPE t_Index


! Clock jump statistic
! --------------------
TYPE t_Jump
  INTEGER(i4b)                 :: iClk       ! File where an outlier was found
  INTEGER(i4b)                 :: iEpo       ! Epoch index in file
  INTEGER(i4b)                 :: jEpo       ! Epoch index in file
  REAL(r8b)                    :: meanDT     ! mean clock drift
  REAL(r8b)                    :: sdevDT     ! s-dev for mean clock drift
  REAL(r8b)                    :: jSize      ! est. size of the clk jump
  REAL(r8b)                    :: sSize      ! est. size of the clk jump
END TYPE t_Jump

! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_ccrnxc_opt(ccrnxc_opt)
    TYPE(t_ccrnxc_opt) :: ccrnxc_opt

    NULLIFY(ccrnxc_opt%refClk)
    NULLIFY(ccrnxc_opt%clkLst)
    NULLIFY(ccrnxc_opt%ieTPoly)
    NULLIFY(ccrnxc_opt%ieTsico)
    NULLIFY(ccrnxc_opt%iePsico)
  END SUBROUTINE init_ccrnxc_opt

  SUBROUTINE init_ccrnxc_fil(ccrnxc_fil)
    TYPE(t_ccrnxc_fil) :: ccrnxc_fil

    NULLIFY(ccrnxc_fil%ClkFilNam)
  END SUBROUTINE init_ccrnxc_fil

END MODULE p_ccrnxc
