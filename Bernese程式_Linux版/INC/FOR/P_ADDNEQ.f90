
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE p_addneq

! -------------------------------------------------------------------------
! Purpose:    This module defines structures and global variables for
!             the program ADDNEQ2 and its subroutines
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
!
! Changes:    06-May-1999 HB: Extracting time informations and save as
!                             MODULE m_time
!             19-Aug-1999 JJ: REMOVE USE m_bern, it is called in m_time
!             08-Sep-2000 HB: use fileNameLength from m_bern
!             17-Sep-2001 RD: find bad solutions in comparison
!             24-Oct-2001 RD: Flags for station info file
!             29-Oct-2001 RD: "Boundary size" for preelimination
!             02-Dec-2001 HU: New variable 'setupgcc' in opt structure
!             28-Dec-2001 HU: MAXCOE increased from 5000 to 6000
!                             (used only in orbtrans)
!             23-Jan-2002 CU: New parameter definition types (t_parorb,
!                             t_parcom,t_parsaof)
!             07-Feb-2002 MM: t_sigma: change due to relative sigmas
!             26-Mar-2002 SS: Several input/output file names added
!             06-May-2002 SS: DCB input file added
!             06-Aug-2002 HU: ieuref,stasort added
!             05-Nov-2002 HB: New parameter definition type (t_paratm)
!             26-Nov-2002 DT: maxParTyp 24 -> 27 (sat.ant.pcv, range bias,
!                             time bias)
!             10-Dec-2002 CU: New parameter definition type (t_parErp),
!                             change variable names: parcom -> parGcc,
!                             parsaof -> parSao
!             27-Jan-2003 PS: Added new parameter definition type (t_parErp)
!                             like CU on 10-Dec-2002 in Bern but didn't
!                             change variable names!
!             28-Jan-2003 AJ: opt%snxinc added
!             04-Feb-2003 MM: option prtExt added
!             27-Feb-2003 HU: T_DATUM moved to D_DATUM
!             07-Mar-2003 HU: MAXPARTYP FROM 24 TO 26
!             21-May-2003 MM: dTs for relative sigmas added
!             22-May-2003 RD: Use maxsat from m_maxdim
!             09-Jul-2003 RD: Move station info from t_opt to t_staCrux
!             08-Sep-2003 HU: NEQ format version 2.0
!             29-Oct-2003 MM: new snxReg parameter
!             12-Nov-2003 PS: maxStaSin 350 -> 400
!             17-Nov-2003 RS: Add gnroff to t_parSao
!             17-Dec-2003 RS: New parameter definition type t_parsap, add
!                             maxSpv
!             27-Jan-2004 HU: Names of covariance file added
!             07-Feb-2004 HU: Option noabc added
!             26-Feb-2004 RD: Preeliminate all parameters outside a time window
!             18-Nov-2004 SS/LM: dx%rms introduced
!             06-Jan-2005 HU: New structure staProblem
!             18-Apr-2005 RD: Consider maneuvers for stc at neq boundaries
!             05-May-2005 HU: Option chrono added
!             19-May-2005 CU: Add npseu to t_misc
!             22-Sep-2005 RD: Create modules D_NEQ.f90, D_PAR.f90
!             20-Apr-2006 AG: Option antred added
!             09-Jun-2006 RD: Option noinv added
!             28-Jun-2006 HU: Options concerning step2 added
!             04-Oct-2006 AG: maxsat for satspv
!             13-Oct-2006 AG: opt%sincont added
!             11-Dec-2006 RD: Add SAVE to module variables
!             11-Jan-2007 MM: Option covMode introduced
!             13-Mar-2007 DT: Add loctie to comstat
!             01-May-2007 AG: opt%erprep added
!             25-Jan-2008 RD: add RAO/RAP parameters
!             26-Feb-2008 RD: extent parOrb structure
!             16-Jul-2008 RD: MAXLIN added
!             22-Aug-2008 DT: Increase maxstd 10->30
!             18-Sep-2008 DT: Add opt%splitDyn
!             28-Oct-2008 DT: Remove maxVar (will be taken from M_MAXDIM)
!             24-Mar-2009 SS: dx%rms with DIMENSION(3)
!             27-Apr-2009 LM/SL: parTypeLength added, type and omega to t_req
!             04-May-2009 RD: Scaling of loading models added
!             06-May-2009 RD: increase maxoff, maxspv (old maxsat)
!             06-May-2009 RD: new options for satellite antennas
!             10-May-2009 RD: Receiver clock offsets/biases implemented
!             28-May-2009 DT: Add range bias files (opt%rgbinp, opt%rgbout)
!             24-Jun-2007 DT: Merge LocTie to BERN version (13-Mar-2007)
!             13-Aug-2009 DT: Add type t_parRgb; opt%stackRGB
!             27-Nov-2009 RD: Station exceptions for pre-elimination
!             04-Jan-2010 SL: HOI scaling parameters added (t_optHoi,hoi,nHoi)
!             03-Mar-2010 RD: Remove obsolete staProblem entry
!             04-Mar-2010 RD: Remove parameters in case of equipment changes
!             13-Aug-2010 RD: Warning in case of huge IFB
!             08-Oct-2010 RD: Extension of parAtm
!             26-Oct-2010 SL: use m_bern with ONLY
!             08-Nov-2010 RD: Select HELMERT parameter for repeatability
!             29-Nov-2010 DT: Add Helmert parameters (hlmFil); opt%wgtout
!             30-Nov-2010 MM: GNSS-specific parameters, new output file
!             03-Feb-2011 MM: Dimension of helmSig from 2 -> 3
!             17-Mar-2011 MM: GNSS-specific parameters: stacking options
!             26-Jun-2012 RD: Add description field to t_elimi
!             16-Oct-2012 SS: opt%blkRet with user-defined sigma value
!             19-Jun-2013 RD: Indicate GLONASS-IFB from PPP
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, staNameLength, fileNameLength, keyValueLength
  USE m_global, ONLY: maxSys
  USE m_time,   ONLY: t_time, t_timint
  USE m_maxdim, ONLY: maxsat
  USE d_par,    ONLY: maxlcq, parTypeLength
  USE d_neq,    ONLY: maxstasin
  USE d_staCrx, ONLY: t_staCrux
  USE d_clkrnx, ONLY: t_clkHead
  USE d_grid,   ONLY: typLen
  IMPLICIT NONE

! Dimensions
! ----------
  INTEGER(i4b),PARAMETER  :: maxArc    =      1
  INTEGER(i4b),PARAMETER  :: maxStd    =     30

  INTEGER(i4b),PARAMETER  :: maxDyn    =     12
  INTEGER(i4b),PARAMETER  :: maxCoe    =  15000
  INTEGER(i4b),PARAMETER  :: maxStp    =     30
  INTEGER(i4b),PARAMETER  :: maxStc    =     60
  INTEGER(i4b),PARAMETER  :: maxOff    = 2*maxSat
  INTEGER(i4b),PARAMETER  :: maxCmp    =      1
  INTEGER(i4b),PARAMETER  :: maxOrb    =     15
  INTEGER(i4b),PARAMETER  :: maxErp    =      5
  INTEGER(i4b),PARAMETER  :: maxSpv    = 2*maxsat
  INTEGER(i4b),PARAMETER  :: maxMan    =    100
  INTEGER(i4b),PARAMETER  :: maxLin    =    200 ! needed in TRAFO1/TRAFO4

! Printing parameters
! -------------------
  INTEGER(i4b),PARAMETER  :: maxPrint  = 10
  INTEGER(i4b),PARAMETER  :: prtParlst =  1
  INTEGER(i4b),PARAMETER  :: prtExt    =  2
  INTEGER(i4b),PARAMETER  :: prtCrx    =  3

! Maneuver description
! --------------------
  TYPE t_man
    INTEGER(i4b)                       :: satman
    REAL(r8b)                          :: timman
  END TYPE t_man

! Orbit description
! -----------------
  TYPE t_orb
    CHARACTER(LEN=80)                                     :: title
    INTEGER(i4b)                                          :: nseff
    INTEGER(i4b),DIMENSION(maxSat)                        :: svneff
    INTEGER(i4b)                                          :: nstoch
    INTEGER(i4b),DIMENSION(maxSat,maxStd)                 :: arcnum
    INTEGER(i4b),DIMENSION(maxSat)                        :: manovr
    REAL(r8b),   DIMENSION(7,maxSat,maxStd)               :: ele
    REAL(r8b),   DIMENSION(maxDyn,maxSat,maxStd)          :: rpress
    REAL(r8b),   DIMENSION(maxDyn,maxSat,maxStd)          :: drpr
    REAL(r8b),   DIMENSION(maxStd)                        :: tosc
    REAL(r8b),   DIMENSION(maxStc*maxStp)                 :: tstoch
    REAL(r8b),   DIMENSION(6,6,maxSat,maxStd)             :: kmat
    REAL(r8b),   DIMENSION(6,maxDyn,maxSat,maxStd)        :: lmat
    REAL(r8b),   DIMENSION(6,maxDyn,maxSat,maxStd,maxStd) :: mmat
    REAL(r8b),   DIMENSION(6,maxSat,maxStd)               :: drho
    REAL(r8b),   DIMENSION(6,maxSat,maxStd)               :: drhot
    INTEGER(i4b)                                          :: nman
    TYPE(t_man), DIMENSION(maxman)                        :: man
  END TYPE t_orb

! Option structure
! ----------------
  TYPE t_req
    INTEGER(i4b),DIMENSION(maxLcq) :: locq        ! parameter type
    CHARACTER(LEN=20)              :: name        ! (usually) station name
    TYPE(t_timint)                 :: timint      ! interval (MJD)
    CHARACTER(LEN=parTypeLength)   :: type = ""   ! parameter type
    REAL(r8b)                      :: omega = 0.0 ! for periodic
  END TYPE t_req

  TYPE t_sigma
    INTEGER(i4b),DIMENSION(maxLcq) :: locq
    CHARACTER(LEN=20)              :: name
    REAL(r8b)                      :: value
    CHARACTER(LEN=1)               :: typFlg     ! 'A'=absolute, 'R'=relative
  END TYPE t_sigma

  TYPE t_elimi
    INTEGER(i4b),DIMENSION(maxLcq)      :: locq
    CHARACTER(LEN=20)                   :: name
    INTEGER(i4b)                        :: part
    INTEGER(i4b)                        :: mode
    INTEGER(i4b), DIMENSION(:), POINTER :: excp
    REAL(r8b)                           :: deltaT
    TYPE(t_timint)                      :: eliWin
    LOGICAL                             :: wParam
    CHARACTER(LEN=keyValueLength)       :: descr
  END TYPE t_elimi

  TYPE t_optLoad
    CHARACTER(LEN=typLen)               :: keyw
    INTEGER(i4b)                        :: nSta
    CHARACTER(LEN=staNameLength),        &
                  DIMENSION(:), POINTER :: staLst
    INTEGER(i4b), DIMENSION(:), POINTER :: staClu
    INTEGER(i4b)                        :: nPar
    INTEGER(i4b)                        :: x0
  END TYPE t_optLoad

  TYPE t_optHoi
    REAL(r8b)                           :: x0
    LOGICAL                             :: stack
  END TYPE t_optHoi

  TYPE t_optHelm
    INTEGER(i4b)                        :: stack   ! Stacking option
                                                   !  0: No stacking
                                                   !  1: within Group
                                                   !  2: all
  END TYPE t_optHelm

  TYPE t_opt
    CHARACTER(LEN=80)                                        :: title
    CHARACTER(LEN=fileNameLength)                            :: const
    CHARACTER(LEN=fileNameLength)                            :: datumfil
    CHARACTER(LEN=fileNameLength)                            :: stacrux
    CHARACTER(LEN=fileNameLength)                            :: satcrux
    CHARACTER(LEN=fileNameLength)                            :: satell
    CHARACTER(LEN=fileNameLength)                            :: phasecc
    CHARACTER(LEN=fileNameLength)                            :: pole
    CHARACTER(LEN=fileNameLength)                            :: sinexin
    CHARACTER(LEN=fileNameLength)                            :: ionexcf
    CHARACTER(LEN=fileNameLength)                            :: coord
    CHARACTER(LEN=fileNameLength)                            :: velapr
    CHARACTER(LEN=fileNameLength)                            :: covcomi
    CHARACTER(LEN=fileNameLength)                            :: gccinp
    CHARACTER(LEN=fileNameLength)                            :: gccout
    CHARACTER(LEN=fileNameLength)                            :: dcbinp
    CHARACTER(LEN=fileNameLength)                            :: rgbinp
    CHARACTER(LEN=fileNameLength)                            :: ionos
    CHARACTER(LEN=fileNameLength)                            :: neqout
    CHARACTER(LEN=fileNameLength)                            :: coordrs
    CHARACTER(LEN=fileNameLength)                            :: velors
    CHARACTER(LEN=fileNameLength)                            :: sinexrs
    CHARACTER(LEN=fileNameLength)                            :: paramin
    CHARACTER(LEN=fileNameLength)                            :: paramrs
    CHARACTER(LEN=fileNameLength)                            :: tropsav
    CHARACTER(LEN=fileNameLength)                            :: tropsnx
    CHARACTER(LEN=fileNameLength)                            :: dcbout
    CHARACTER(LEN=fileNameLength)                            :: gravout = ""
    CHARACTER(LEN=fileNameLength)                            :: ionosrs
    CHARACTER(LEN=fileNameLength)                            :: ionexrs
    CHARACTER(LEN=fileNameLength)                            :: polers
    CHARACTER(LEN=fileNameLength)                            :: ierspol
    CHARACTER(LEN=fileNameLength)                            :: plotrs
    CHARACTER(LEN=fileNameLength)                            :: covarrs
    CHARACTER(LEN=fileNameLength)                            :: covttrs
    CHARACTER(LEN=fileNameLength)                            :: rgbout
    CHARACTER(LEN=fileNameLength)                            :: wgtout
    CHARACTER(LEN=fileNameLength)                            :: sysout
    CHARACTER(LEN=fileNameLength)                            :: syserr
    CHARACTER(LEN=fileNameLength)                            :: auxfil
    INTEGER(i4b)                                             :: maxpar
    CHARACTER(LEN=fileNameLength),DIMENSION(:),  POINTER     :: neqFileName
    CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER     :: orbFil
    LOGICAL                                                  :: noinv
    TYPE(t_req)      ,DIMENSION(:),  POINTER                 :: req
    TYPE(t_elimi)    ,DIMENSION(:),  POINTER                 :: elimi
    INTEGER(i4b)                                             :: nstcep
    INTEGER(i4b)     ,DIMENSION(3)                           :: rsw
    REAL(r8b)        ,DIMENSION(3)                           :: sigrsw
    INTEGER(i4b)                                             :: numstc
    INTEGER(i4b)     ,DIMENSION(maxSat)                      :: svnstc
    REAL(r8b)                                                :: dtstc
    INTEGER(i4b)                                             :: numdyn
    INTEGER(i4b)     ,DIMENSION(maxSat)                      :: svndyn
    INTEGER(i4b)                                             :: numgap
    INTEGER(i4b)     ,DIMENSION(maxSat)                      :: svngap
    INTEGER(i4b)                                             :: numnac
    INTEGER(i4b)     ,DIMENSION(2,maxStd*maxSat)             :: newarc
    REAL(r8b)                                                :: sigma0
    TYPE(t_sigma)    ,DIMENSION(:),POINTER                   :: sigma
    INTEGER(i4b)                                             :: trafo
    INTEGER(i4b)                                             :: splitDyn
    INTEGER(i4b)                                             :: PoleStep
    INTEGER(i4b)                                             :: indvSol
    INTEGER(i4b)     ,DIMENSION(7)                           :: ipHelm
    INTEGER(i4b)                                             :: minSol
    REAL(r8b)        ,DIMENSION(2,3)                         :: badSol
    REAL(r8b)                                                :: blkRet
    REAL(r8b)                                                :: timRefCrd
    REAL(r8b), DIMENSION(3,7)                                :: helmSig
    integer(i4b)                           :: trpExceptDay ! remove, 10-08-01/hu
    INTEGER(i4b),DIMENSION(maxPrint)                         :: prt
    INTEGER(i4b)                                             :: setupgcc
    INTEGER(i4b)                                             :: ieuref
    INTEGER(i4b)                                             :: antred
    INTEGER(i4b)                                             :: erprep
    INTEGER(i4b),DIMENSION(2)                                :: staSort
    INTEGER(i4b)                                             :: snxinc
    REAL(r8b)                                                :: snxReg
    REAL(r8b),DIMENSION(3)                                   :: dtRelSig
    INTEGER(i4b)                                             :: noabc
    INTEGER(i4b)                                             :: chrono
    INTEGER(i4b)                                             :: step2
    REAL(r8b),DIMENSION(2)                                   :: step2_t
    INTEGER(i4b)                                             :: sincont
    INTEGER(i4b)                                             :: covMode
    INTEGER(i4b)                                             :: stackRGB
    INTEGER(i4b), DIMENSION(2)                               :: saoOpt
    INTEGER(i4b), DIMENSION(2)                               :: sapOpt
    TYPE(t_optLoad), DIMENSION(3)                            :: grdLoad
    TYPE(t_optHoi),DIMENSION(3)                              :: hoi
    TYPE(t_optHelm),DIMENSION(7)                             :: helmert
    INTEGER(i4b)                                             :: chgRecNam
    INTEGER(i4b)                                             :: chgAntNam
    INTEGER(i4b)                                             :: chgRecNum
    INTEGER(i4b)                                             :: chgAntNum
    INTEGER(i4b)                                             :: chgAntEcc
    CHARACTER(LEN=fileNameLength)                            :: isbout
    INTEGER(i4b)                                             :: rcoRmGPS
    INTEGER(i4b)                                             :: rcoToFrq
    INTEGER(i4b)                                             :: rcofromPPP
    REAL(r8b)                                                :: maxIfb
    INTEGER(i4b)                                             :: gTraDat
    INTEGER(i4b)                                             :: gTrpDat
    INTEGER(i4b)                                             :: gspStack
  END TYPE t_opt

  TYPE(t_opt), SAVE :: opt

! Station information
! -------------------
  TYPE(t_staCrux), SAVE :: staInfo

! Clock Results
! -------------
  TYPE(t_clkhead) :: clkHed

! Common statistics structure
! ---------------------------
  TYPE t_comstat
    INTEGER(i4b)                                      :: elimi
    REAL(r8b)                                         :: rms
    REAL(r8b),          DIMENSION(:,:,:),     POINTER :: taecml
    INTEGER(i4b),       DIMENSION(maxStaSin)          :: nflsta
    INTEGER(i4b),       DIMENSION(:,:),       POINTER :: indfil
    CHARACTER(LEN=9),   DIMENSION(:),         POINTER :: datcre
    CHARACTER(LEN=5),   DIMENSION(:),         POINTER :: timcre
    INTEGER(i4b),       DIMENSION(:),         POINTER :: nparl
    CHARACTER(LEN=132), DIMENSION(:),         POINTER :: titind
    INTEGER(i4b)                                      :: loctie     ! # local ties applied
  END TYPE t_comstat

  TYPE(t_comstat), SAVE :: comstat

! Differences between Solutions
! -----------------------------
  TYPE t_dx
    LOGICAL                 :: flag
    REAL(r8b)               :: diff
    TYPE(t_time)            :: t
    REAL(r8b), DIMENSION(3) :: rms
  END TYPE t_dx


! Parameter definition got from neq
! ---------------------------------
! List of parameter names per parameter type
  TYPE t_namLst
    INTEGER(i4b)                             :: nSta
    CHARACTER(LEN=20), DIMENSION(:), POINTER :: nam2
    CHARACTER(LEN=staNameLength),             &
                       DIMENSION(:), POINTER :: nam
    INTEGER(i4b),      DIMENSION(:), POINTER :: num
  END TYPE t_namLst

! Orbit parameters
  TYPE t_parOrb
    INTEGER(i4b)                    :: nOrb    ! # Orbit parameters
    INTEGER(i4b), DIMENSION(maxOrb) :: seqorb  ! Sequence of orbit parameters
    REAL(r8b), DIMENSION(:), POINTER:: epoOrb  ! osc. epoch of the elements
                                               ! within each NEQ
  END TYPE t_parOrb

! Geocenter coordinates
  TYPE t_parGcc
    INTEGER(i4b)                    :: nGcc    ! # Geocenter coord. requests
    INTEGER(i4b), DIMENSION(3)      :: gcc     ! Geocenter coordinate requests
  END TYPE t_parGcc

! Satellite antenna offsets
  TYPE t_parSao
    INTEGER(i4b)                    :: nanoff  ! # Sat. antenna offset groups
    INTEGER(i4b), DIMENSION(:),  POINTER :: nsaoff  ! # Sat. belonging to offset
                                                    ! group i = 1, nanoff
    INTEGER(i4b), DIMENSION(:,:),POINTER :: satoff  ! Sat.no. of each group
    INTEGER(i4b), DIMENSION(2*maxsat,3)    :: santoff ! Sat. antenna offset
                                                      ! requests
    INTEGER(i4b)                    :: nRqoff  ! # Satell. antenna offset
                                               ! requests
    INTEGER(i4b), DIMENSION(maxOff) :: grpoff  ! Antenna group for request
                                               ! number i
    INTEGER(i4b), DIMENSION(maxOff) :: gnroff  ! User-defined number for sat.
                                               ! ant. offset group i = 1,nanoff
    INTEGER(i4b), DIMENSION(maxOff) :: nCoreq  ! # Component requests for
                                               ! request number i
    REAL(r8b), DIMENSION(2,maxOff)  :: timoff  ! Time interval timReq(i,j)
                                               ! i=1,2 (mean,half)
  END TYPE t_parSao

! Satellite antenna patterns
  TYPE t_parSap
    INTEGER(i4b)                      :: nanspv ! # Sat. antenna pattern groups
    INTEGER(i4b), DIMENSION(:),  POINTER :: nsaspv ! # Sat. belonging to pattern
                                                   ! group i = 1, nanspv
    INTEGER(i4b), DIMENSION(:,:),POINTER :: satspv ! Sat.no. of each group
    INTEGER(i4b), DIMENSION(maxSpv)   :: gnrspv ! User-defined number for sat.
                                                ! ant. pattern group i = 1,nanspv
    INTEGER(i4b), DIMENSION(2,maxSpv) :: nptspv ! Number of points to be estimated
                                                ! in elevation (1) and azimuth (2)
                                                ! direction
!!    REAL(r8b)                         :: nadmax ! Maximum nadir angle for satellite
!!                                                ! antenna pattern estimation (in rad)
    REAL(r8b),    DIMENSION(2,maxSpv) :: timspv ! Time interval timReq(i,j)
                                                ! i=1,2 (mean,half)
  END TYPE t_parSap

! Receiver antenna offsets
  TYPE t_parRao
    INTEGER(i4b)                    :: nRao    ! # RAO requests
    INTEGER(i4b), DIMENSION(3)      :: rao     ! receiver antenna offsets
  END TYPE t_parRao

! Receiver antenna pattern
  TYPE t_parRap
    INTEGER(i4b)                    :: nRapGrd ! # RAP requests for grid
    INTEGER(i4b)                    :: nRapShm ! # RAP requests for sh
  END TYPE t_parRap

! Atmosphere parameters
  TYPE t_parTrp
    INTEGER(i4b), DIMENSION(2)      :: fromTo  ! NEQ-file numbers
    INTEGER(i4b)                    :: iTropo  ! Tropospheric model
    INTEGER(i4b)                    :: iExtra  ! Which trop. values
    INTEGER(i4b)                    :: iTrMap  ! Mapping function
    INTEGER(i4b)                    :: iTrGrd  ! Which gradient model
  END TYPE t_parTrp

  TYPE t_parAtm
! Troposphere
    INTEGER(i4b)                    :: nTro    ! # Troposphere parameters
    INTEGER(i4b)                    :: nModel  ! # troposphere models
    TYPE(t_parTrp), DIMENSION(:),    &
                    POINTER         :: trpMod  ! Troposphere model charact.
! Ionosphere
    INTEGER(i4b)                    :: nIon    ! # Ionosphere parameters
    INTEGER(i4b)                    :: nHoi    ! # HOI scaling parameters
  END TYPE t_parAtm

! Earth rotation parameters
  TYPE t_parErp
    INTEGER(i4b)                    :: nErp    ! # Earth rotation parameters
    INTEGER(i4b), DIMENSION(maxerp) :: erp     ! Earth rotation parameter request
  END TYPE t_parErp

! Receiver clock offset/bias
  TYPE t_parRco
    INTEGER(i4b)                    :: isSta   ! Station specific
    INTEGER(i4b)                    :: isFrq   ! Frequency specific
    INTEGER(i4b)                    :: isSat   ! Satellite specific
  END TYPE t_parRco

! Range Bias parameters
  TYPE t_parRgb
    INTEGER(i4b)                    :: nRgb    ! # Range bias parameters
!!!    INTEGER(i4b), DIMENSION(maxrgb) :: rgb     ! Range bias parameter request
  END TYPE t_parRgb

! GNSS-specific parameters
  TYPE t_parGsp
    INTEGER(i4b)                    :: nTra    ! # Translation parameters
    INTEGER(i4b)                    :: nSysTra ! # GNSS
    INTEGER(i4b),DIMENSION(maxSys)  :: sysTra  ! GNSS
    INTEGER(i4b)                    :: nTrp    ! # Troposphere biases
    INTEGER(i4b)                    :: nSysTrp ! # GNSS
    INTEGER(i4b),DIMENSION(maxSys)  :: sysTrp  ! GNSS
  END TYPE t_parGsp

! Structure for WGT file
  TYPE t_hlmFil
    INTEGER(i4b)                           :: nNeq    ! # NEQ files
    CHARACTER(LEN=fileNameLength),  &
                   DIMENSION(:), POINTER   :: filNam  ! NEQ files
    INTEGER(i4b),DIMENSION(:,:), POINTER   :: ihelm   ! apriori Helmert (=1)
                                                      !  or estimation (=2)
    REAL(r8b),DIMENSION(:,:), POINTER      :: rhelm   ! Helmert parameter
                                                      !  [m], [rad], [1+ppm]
    INTEGER(i4b),DIMENSION(:), POINTER     :: indgrp  ! Index of group
    CHARACTER(LEN=3),DIMENSION(:), POINTER :: grpnam  ! Name of group
    REAL(r8b),DIMENSION(:), POINTER        :: fact    ! NEQ re-scaling
  END TYPE t_hlmFil

  TYPE(t_hlmFil), SAVE :: hlmFil

! Helmert parameters estimated
  TYPE t_parHlm
    INTEGER(i4b)                    :: nHlm    ! # Helmert parameters
  END TYPE t_parHlm

END MODULE p_addneq


