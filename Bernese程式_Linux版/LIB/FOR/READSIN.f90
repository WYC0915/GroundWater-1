MODULE s_READSIN
CONTAINS

! --------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! --------------------------------------------------------------------------

SUBROUTINE readsin(snxFileName, neq, optrot, wfact, flg_neq,  &
                   berneq, sigapri, sintim, timCrd, noneq,    &
                   crdset, Tcrdset, snxSta)

! --------------------------------------------------------------------------
! Purpose:    Read the SINEX file
!
! Author:     L. Mervart
!
! Created:    08-Oct-1998
!
! Changes:    04-May-2000 HU: Call of 'syminv' corrected
!             08-Sep-2000 HB: use fileNameLength from m_bern
!             13-Nov-2000 DT: Antenna Serial Number with characters
!             16-Nov-2000 DT: Parameter type UTR, LODR
!             17-Nov-2000 DT: Correction of parameter UT with 32 leap seconds
!                             if abs(UT) > 1s
!             20-Nov-2000 DT: CORR-matrices -> COVA-matrices
!             22-Nov-2000 MR: last element of matrix not in third column
!             23-Nov-2000 DT: ignore transformation parameters
!                             in SOLUTION/APRIORI
!             27-Nov-2000 DT: take apriori solution only if parameter type
!                             and index are the same as in estimate solution
!             28-Nov-2000 DT: convert station-name to uppercase
!             30-Nov-2000 DT: allow Coordinates of Geocenter as parameter
!             15-Dez-2000 DT: compare estimate and apriori solution
!                             -> set apriori parameters according to
!                                indices of estimate solution
!             15-Dez-2000 DT: if no apriori value exists -> take estimate
!                             value
!             31-Jan-2001 DT: compute the mean maindiagonal-element of the
!                             normal-equation-matrix for a correct weighting
!                             of different solutions
!             27-Mar-2001 DT: remove inner-constraints (translation, rotation,
!                             scale) with subroutine 'freenet2'
!             10-Apr-2001 DT: introduce additional rotation-parameters and
!                             remove their influence on the normal-equation
!                             system with subroutine 'add_rot'
!             26-Jun-2001 RD: Use alcerr for allocation
!             01-Nov-2001 HU: Interface for ADD_ROT,NEQALLOC,FREENET2,NEQINIT
!             03-Nov-2001 HU: Parameter optrot added, deallocate arrays,
!                             get GPS-UTC, use lfn001 instead of lfnloc,
!                             count parameter types
!             21-Nov-2001 HU: Compute ut1_ut1r correction after computation
!                             of erpintLength. flgApr is no array.
!             03-Dec-2001 DT: Setting for 'nnend' modified
!             11-Dec-2001 HU: Format bugs corrected
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             20-Aug-2002 HU: Rescale variance factor
!             21-AUG-2002 DT: Add nutation parameters NUT_OB, NUT_LN;
!                             extend maxdesc to 13
!             22-AUG-2002 DT: Add SOLUTION blocks for normal equations
!                             NORMAL_EQUATION_MATRIX, NORMAL_EQUATION_VECTOR
!             22-AUG-2002 DT: Modify output
!             28-Aug-2002 HU: Write variance factor to protocol
!                             Keyword 'PHASE MEASUREMENTS SIGMA' corrected
!             29-AUG-2002 DT: Parameter flg_neq added to decide whether the
!                             normal equation system is generated from the
!                             covariance matrices or directly from the NEQ
!                             blocks
!             29-AUG-2002 DT: Reading of the two MATRIX blocks OR of the
!                             NEQ blocks, depending on parameter flg_neq
!             30-AUG-2002 DT: Division of the matrices by wfact only if
!                             invert is equal to 1
!             30-AUG-2002 DT: computation of the mean main-diagonal element
!                             independent of the parameter invert
!             30-AUG-2002 DT: Write name of actually processed file in output
!             10-SEP-2002 DT: Write new station name in output if solution
!                             number was added
!             11-SEP-2002 DT: Read sigmas in SOLUTION/APRIORI block (constr)
!                             and generate diagonal matrix regMat with these
!                             sigmas if neither the MATRIX_APRIORI nor the
!                             NEQ_MATRIX block is given (flgSig)
!             26-SEP-2002 DT: Save station coordinates of ESTIMATE block in
!                             new output parameter xstat
!             22-NOV-2002 DT: Save station velocities of ESTIMATE block in
!                             new output parameter xvel
!             25-NOV-2002 DT: Change format of parType_est in error message
!                             to 'A6'
!             26-NOV-2002 DT: Add range and time biases as new parameter
!                             types; maxdesc und numdesc: 13 -> 15
!             27-NOV-2002 DT: Read PointCode from SOLUTION/ESTIMATE and
!                             APRIORI block in a vector
!             17-DEC-2002 DT: Add troposphere parameters (zenith delays and
!                             gradients; but only total values, not wet and
!                             dry part); maxdesc und numdesc: 15 -> 17
!             17-DEC-2002 DT: Read siteID of SOLUTION/APRIORI in a vector
!             14-JAN-2003 DT: Setting for 'nnend' for reading all matrix
!                             blocks corrected
!             07-MAR-2003 MR: Locq for time and range biases changed
!             09-MAY-2003 HU: Layout of output file
!             18-JAN-2004 HU: Handling of additional solutions
!             16-Feb-2004 HB: domes = ' ' instead of domes = ''
!             10-Mar-2004 CU: Reconstruct original NEQ info from Bernese
!                             SINEX data - on request
!             16-Mar-2004 PF: Corrected Format Statement (extraneous comma)
!             17-MAR-2004 DT: Correct regMat if taken from SOLUTION/APRIORI
!                             block
!             17-MAR-2004 DT: Set locq(6) for ERPs (not yet for nutation)
!             17-MAR-2004 DT: Add nutation rates as parameter
!             17-MAR-2004 DT: Correct parameter name for troposphere
!             17-MAR-2004 DT: Set neq%misc%itrgrd=1 (= Tilted mapping; if
!                             troposphere gradients are present)
!             17-MAR-2004 DT: Set time information for troposphere
!             17-MAR-2004 DT: Check units for nutation parameters (nut_unit)
!             17-MAR-2004 DT: setting of locq(2) for troposphere corrected
!             17-MAR-2004 DT: Search for corresponding station coordinates
!                             generalized
!             22-MAR-2004 DT: Initialize neq%misc%nsmpnq = 30 seconds
!             23-May-2005 SS/CU: Retrieve unconstrained solution vector for
!                             initialization of lTPl
!             24-May-2005 HU: Read SOLUTION/EPOCHS block
!             13-Jun-2005 HU: Search for velocities generalized, too
!             13-Jun-2005 HU: Read indices for solution a priori independently
!                             Order regMat accordingly.
!                             Direct reference to constraints.
!                             All parameter identifications using siteCode,
!                             pointCode, solID, parType.
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!             20-Apr-2006 AG: Read Satellite antenna offsets
!                             Lenght of antenna name: 16 --> 20
!             09-Jul-2006 HU: Output start/end time of SINEX
!             17-Jul-2006 HU: Epoch for CRD file corrected
!             16-Aug-2006 AG: Format corrected
!             31-Oct-2006 AG: SR reblocq implemented, Reading of SINEX with
!                             NEQ improved
!             08-Nov-2006 AG: Error if neq%misc%npar /= ipar, stop if rates for
!                             transformation parameters in SOLUTION/APRIORI block
!             13-Nov-2006 AG: Individual antenna number changed from 0->999999
!             22-Jan-2007 AG: SVN instead of PRN in locq(5)
!             08-Feb-2007 RD: misc%nObs/nParms i4b->r8b
!             20-Aug-2007 AG: Noneq implemented, warning extended with file name
!             26-Jun-2008 RD: System-specific PCV in SINEX record
!             29-Jul-2008 RD/SL: Set individually calibrated antennas (individ)
!             11-Sep-2008 RD: Enables reading while antrec = ' '
!             07-Dec-2008 DT: Check for nStatist==3 instead of ==4;
!                             Count variance factor independent of size;
!                             UPPERC for receiver
!             13-Jan-2009 RD: Use '' as EOF-identifier in NEXTLINE
!             12-Mar-2009 SL: Exit condition set to %ENDSNX
!             16-Apr-2009 DT: Corrections for UT/LOD for a priori values as well
!             13-Jul-2009 DT: Add matrix type INFO (matType=3) (20-Apr-2006 in PDM-version)
!             13-Jul-2009 DT: Read technique from SITE/ID (sta_tech);
!                             GETRCV only for GNSS sites
!             31-Aug-2009 LO: Variance factor lower limit form 1d-1 to 1d-2
!             25-Feb-2010 DT: Correct time%mean for trop gradients
!             09-Aug-2010 RD: Use "syminvg" instead of "syminv"
!             15-Oct-2010 DT: Selecting set of coord for CRD/VEL (snxSta);
!                             Call of READSIN changed (xstat,xvel removed)
!             20-Oct-2010 DT: Re-define COVA/NEQ (flg_neq) as priority list;
!                             set 'invert' accordingly (remove from param.list);
!                             Size of numERPpar,erpIntLength,mxERPt,mnERPt 1->5;
!                             Use maxdesc from p_snx2nq0
!             02-Nov-2010 SL: use m_bern with ONLY, use undef_i, conv bug corr.,
!                             removal of unused vars
!             14-Jun-2011 PS: SOLUTION/EPOCH end time bugfix
!             20-Sep-2012 RD: Correctly deallocate arrays
!             05-Oct-2012 DT: Add nutation rates and Cnm/Snm, adopt numdesc;
!                             correct setting of time%half for EOPs
!             05-Oct-2012 DT: Get technique from header -> REBLOCQ
!             24-Oct-2012 SS/DT: Consider flgCon
!             31-Oct-2012 SS: Alternated sign for LOD/LODR bNor
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! --------------------------------------------------------------------------

  USE m_bern,    ONLY: i4b, r8b, fileNameLength, lfnPrt, lfn001, lfnErr
  USE m_global,  ONLY: maxsys
  USE d_neq,     ONLY: t_neq,maxFrq,maxStaSin,neqCurrentVersion
  USE d_const,   ONLY: pi
  USE m_time,    ONLY: t_timint, OPERATOR(.isIn.)
  USE p_snx2nq0, ONLY: t_snxSta, maxdesc
  USE d_stacrx,  ONLY: undef_i

  USE f_ikf
  USE s_opnfil
  USE s_alcerr
  USE f_nextline
  USE s_opnerr
  USE s_neqinit
  USE s_solve
  USE s_syminvg
  USE f_dgpsut
  USE s_neqalloc
  USE s_sindat
  USE s_add_rot
  USE s_exitrc
  USE f_ut1_ut1r
  USE s_upperc
  USE s_freenet2
  USE s_reblocq
  USE s_getrcv
  IMPLICIT NONE

! List of Parameters
! ------------------
! Input:
  CHARACTER(LEN=fileNameLength)      :: snxFileName ! Name of SINEX file
  INTEGER(i4b)                       :: invert      ! =1: Invert matrix
  INTEGER(i4b)                       :: optrot      ! =1: apply add_rot
  INTEGER(i4b)                       :: noneq       ! Do not write NEQ
  INTEGER(i4b)                       :: flg_neq     ! =1: from COVA
                                                    ! =2: from NEQ
  INTEGER(i4b)                       :: berneq      ! =1: Reconstruct original
                                                    !     Bernese NEQ info
  INTEGER(i4b)                       :: crdset      ! Set of coord in CRD/VEL:
                                                    !  1: latest set
                                                    !  2: set valid at epoch Tcrdset
  REAL(r8b)                          :: Tcrdset     ! Validity epoch to extract coord.
  REAL(r8b)                          :: sigapri     ! A priori sigma of unit wgt

! Output:
  TYPE(t_neq)                        :: neq         ! NEQ structure
  REAL(r8b)                          :: wfact       ! Variance factor
  TYPE(t_timint)                     :: sintim      ! Start/end epoch of SINEX
  REAL(r8b)                          :: timCrd      ! Epoch for coordinate file
  TYPE(t_snxSta)                     :: snxSta      ! Selected station coord. to be saved

! Local Variables
! ---------------
  TYPE(t_timint)                 :: validity

  INTEGER(i4b)                   :: ios, iac
  INTEGER(i4b)                   :: iant, nant
  INTEGER(i4b)                   :: ista
  INTEGER(i4b)                   :: ipar
  INTEGER(i4b)                   :: ipar1
  INTEGER(i4b)                   :: ipar2
  INTEGER(i4b)                   :: nsing
  INTEGER(i4b), DIMENSION(5,2)   :: eopFlg
  INTEGER(i4b)                   :: icrd
  INTEGER(i4b)                   :: ideg
  INTEGER(i4b)                   :: idummy
  INTEGER(i4b)                   :: idum1,idum2
  INTEGER(i4b), DIMENSION(5)     :: numERPpar
  INTEGER(i4b)                   :: matType
  INTEGER(i4b)                   :: jpar
  INTEGER(i4b)                   :: ij
  INTEGER(i4b)                   :: i, ii, jj
  INTEGER(i4b)                   :: nnend
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE :: flgUt1r
  INTEGER(i4b)                   :: flgHelm
  INTEGER(i4b)                   :: ncoord
  INTEGER(i4b)                   :: flgSig
  INTEGER(i4b)                   :: flgCon
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE :: solID_apr
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE :: solID_est
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE :: flgCon_est
  INTEGER(i4b)                   :: tropgrad
  INTEGER(i4b)                   :: nsitelst
  INTEGER(i4b)                   :: isol
  INTEGER(i4b)                   :: ifound
  INTEGER(i4b)                   :: iline
  INTEGER(i4b)                   :: nStatist
  INTEGER(i4b)                   :: oldobs
  INTEGER(i4b)                   :: oldpar
  INTEGER(i4b)                   :: bernold
  INTEGER(i4b)                   :: npar_apr
  INTEGER(i4b)                   :: idx1, idx2
  INTEGER(i4b)                   :: iFrom, iTo
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE :: indx_apr
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE :: ipar_apr
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE :: indx_hlp
  INTEGER(i4b), DIMENSION(maxStaSin)      :: antnum
  INTEGER(i4b)                       :: iSys,iSyst
  INTEGER(i4b)                       :: nFrq,iClass
  INTEGER(i4b), DIMENSION(MAXFRQ)    :: iCode,iwlFac

  CHARACTER(LEN=80)              :: line
  CHARACTER(LEN=4)               :: siteCode
  CHARACTER(LEN=2)               :: pointCode
  CHARACTER(LEN=9)               :: domes
  CHARACTER(LEN=4)               :: solID
  CHARACTER(LEN=1)               :: technique
  CHARACTER(LEN=1)               :: techSNX
  CHARACTER(LEN=12)              :: startTime
  CHARACTER(LEN=12)              :: refTime
  CHARACTER(LEN=12)              :: endTime
  CHARACTER(LEN=12)              :: meanTime
  CHARACTER(LEN=20)              :: antrec
  CHARACTER(LEN=20),DIMENSION(maxStaSin)      :: antsta
  CHARACTER(LEN=20)              :: staant
  CHARACTER(LEN=10),DIMENSION(maxStaSin)      :: atxStr
  CHARACTER(LEN=5)               :: numant
  CHARACTER(LEN=3)               :: refSys
  CHARACTER(LEN=4)               :: unit
  CHARACTER(len=7)               :: sbrName = 'READSIN'
  CHARACTER(LEN=6)               :: parType
  CHARACTER(LEN=4)               :: nut_unit
  CHARACTER(LEN=6), DIMENSION(:), ALLOCATABLE :: parType_est
  CHARACTER(LEN=6), DIMENSION(:), ALLOCATABLE :: parType_apr
  CHARACTER(LEN=4), DIMENSION(:), ALLOCATABLE :: site_apr
  CHARACTER(LEN=4), DIMENSION(:), ALLOCATABLE :: site_est
  CHARACTER(LEN=2), DIMENSION(:), ALLOCATABLE :: pointCode_apr
  CHARACTER(LEN=2), DIMENSION(:), ALLOCATABLE :: pointCode_est
  CHARACTER(LEN=1), DIMENSION(:), ALLOCATABLE :: sta_tech
  CHARACTER(LEN=15),DIMENSION(maxStaSin)      :: siteLst1

  REAL(r8b), DIMENSION(3,maxFrq,maxStaSin)    :: antphs
  REAL(r8b), DIMENSION(3)              :: antecc
  REAL(r8b), DIMENSION(:), ALLOCATABLE :: regMat
  REAL(r8b), DIMENSION(5)              :: erpIntLength
  REAL(r8b), DIMENSION(5)              :: mxERPt
  REAL(r8b), DIMENSION(5)              :: mnERPt
  REAL(r8b), DIMENSION(:), ALLOCATABLE :: hlp
  REAL(r8b), DIMENSION(7)              :: sigma
  REAL(r8b)                            :: sigma0
  REAL(r8b)                            :: neq_trace
  REAL(r8b)                            :: refMJD
  REAL(r8b)                            :: taiutc
  REAL(r8b)                            :: taigps=19D0
  REAL(r8b), DIMENSION(:), ALLOCATABLE :: x0_apr
  REAL(r8b), DIMENSION(:), ALLOCATABLE :: constr
  REAL(r8b)                            :: help
  REAL(r8b)                            :: mxTRPt
  REAL(r8b)                            :: mnTRPt
  REAL(r8b)                            :: trpIntLength1
  REAL(r8b)                            :: trpIntLength2
  REAL(r8b)                            :: oldrms
  REAL(r8b)                            :: rad2mas

  LOGICAL                              :: siteant
  LOGICAL                              :: gpsphac
  LOGICAL                              :: solest
  LOGICAL                              :: solapr
  LOGICAL                              :: first
  LOGICAL, DIMENSION(2)                :: foundCOV  ! 1: MATRIX_ESTIMATE
                                                    ! 2: MATRIX_APRIORI
  LOGICAL, DIMENSION(2)                :: foundNEQ  ! 1: NEQ_MATRIX
                                                    ! 2: NEQ_VECTOR
  LOGICAL, DIMENSION(5)                :: flgPWL

  INTEGER(i4b),DIMENSION(maxdesc)      :: numdesc
  CHARACTER(LEN=29),DIMENSION(maxdesc),SAVE &
                                       :: descTyp = &
                                       (/'Station coordinate sets      ', &
                                         'Station velocity sets        ', &
                                         'Polar coordinate sets        ', &
                                         'UT parameters                ', &
                                         'Set of polar coordinate rates', &
                                         'Length of day parameters     ', &
                                         'Reduced UT parameters        ', &
                                         'Red. length of day parameters', &
                                         'Geocenter coordinate sets    ', &
                                         'Translation constraint set   ', &
                                         'Rotational constraint set    ', &
                                         'Scale constraint             ', &
                                         'Nutation parameter sets      ', &
                                         'Nutation rate parameter sets ', &
                                         'Range biases                 ', &
                                         'Time biases                  ', &
                                         'Troposphere zenith delays    ', &
                                         'Troposphere gradient sets    ', &
                                         'Satellite offsets            ', &
                                         'Earth potential parameters   '/)

! Initialize the neq stucture
! ---------------------------
  CALL neqinit(neq)

  neq%version = neqCurrentVersion

! Write name of actually processed file in output
! -----------------------------------------------
  WRITE(lfnprt,"(1X,79('*'))")
  WRITE(lfnprt,"(1X,'Processed File: ',A)") TRIM(snxFileName)
  WRITE(lfnprt,"(1X,79('*'),/)")

! Open the input file
! -------------------
  CALL opnfil(lfn001,snxFileName,'OLD','FORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnerr,lfn001,ios,snxFileName,'readsin')

! Read the first line, allocate memory
! ------------------------------------
  line = nextline(lfn001,0)
  READ(line,'(32x,A12,1x,A12,1x,A1,1x,i5)') &
       startTime, endTime, techSNX, neq%misc%npar

  CALL sindat(1, sintim%t(1), startTime)
  CALL sindat(1, sintim%t(2), endTime)

  CALL neqalloc(neq,neq%misc%npar)
  ALLOCATE(neq%xxx(neq%misc%npar), stat=iac)
  CALL alcerr(iac, 'neq%xxx', (/neq%misc%npar/), 'readsin')
  neq%xxx = 0.d0
  ALLOCATE(regMat(neq%misc%npar*(neq%misc%npar+1)/2), stat=iac)
  CALL alcerr(iac, 'regMat', (/neq%misc%npar*(neq%misc%npar+1)/2/), 'readsin')
  regMat = 0.d0
  ALLOCATE(parType_est(neq%misc%npar), stat=iac)
  CALL alcerr(iac, 'parType_est', (/neq%misc%npar/), 'readsin')
  parType_est = '      '
  ALLOCATE(parType_apr(neq%misc%npar), stat=iac)
  CALL alcerr(iac, 'parType_apr' ,(/neq%misc%npar/), 'readsin')
  parType_apr = '      '
  ALLOCATE(site_apr(neq%misc%npar), stat=iac)
  CALL alcerr(iac, 'site_apr', (/neq%misc%npar/), 'readsin')
  site_apr = '    '
  ALLOCATE(site_est(neq%misc%npar), stat=iac)
  CALL alcerr(iac, 'site_est', (/neq%misc%npar/), 'readsin')
  site_est = '    '
  ALLOCATE(hlp(neq%misc%npar), stat=iac)
  CALL alcerr(iac, 'hlp', (/neq%misc%npar/), 'readsin')
  hlp = 0.d0
  ALLOCATE(flgUt1r(neq%misc%npar), stat=iac)
  CALL alcerr(iac, 'flgUt1r', (/neq%misc%npar/), 'readsin')
  flgUt1r = 0
  ALLOCATE(x0_apr(neq%misc%npar), stat=iac)
  CALL alcerr(iac, 'x0_apr', (/neq%misc%npar/), 'readsin')
  x0_apr = 0.d0
  ALLOCATE(constr(neq%misc%npar), stat=iac)
  CALL alcerr(iac, 'constr', (/neq%misc%npar/), 'readsin')
  constr = 0.d0
  ALLOCATE(pointCode_est(neq%misc%npar), stat=iac)
  CALL alcerr(iac, 'pointCode_est', (/neq%misc%npar/), 'readsin')
  pointCode_est = '  '
  ALLOCATE(pointCode_apr(neq%misc%npar), stat=iac)
  CALL alcerr(iac, 'pointCode_apr', (/neq%misc%npar/), 'readsin')
  pointCode_apr = '  '
  ALLOCATE(solID_apr(neq%misc%npar), stat=iac)
  CALL alcerr(iac, 'solID_apr', (/neq%misc%npar/), 'readsin')
  solID_apr = 0
  ALLOCATE(solID_est(neq%misc%npar), stat=iac)
  CALL alcerr(iac, 'solID_est', (/neq%misc%npar/), 'readsin')
  solID_est = 0
  ALLOCATE(flgCon_est(neq%misc%npar), stat=iac)
  CALL alcerr(iac, 'flgCon_est', (/neq%misc%npar/), 'readsin')
  flgCon_est = 2
  ALLOCATE(indx_apr(neq%misc%npar), stat=iac)
  CALL alcerr(iac, 'indx_apr', (/neq%misc%npar/), 'readsin')
  indx_apr = 0
  ALLOCATE(ipar_apr(neq%misc%npar), stat=iac)
  CALL alcerr(iac, 'ipar_apr', (/neq%misc%npar/), 'readsin')
  ipar_apr = 0
  ALLOCATE(indx_hlp(neq%misc%npar), stat=iac)
  CALL alcerr(iac, 'indx_hlp', (/neq%misc%npar/), 'readsin')
  indx_hlp = 0
  ALLOCATE(sta_tech(neq%misc%npar), stat=iac)
  CALL alcerr(iac, 'sta_tech', (/neq%misc%npar/), 'readsin')
  sta_tech = ' '



! Initialize several values (not already in neqinit)
! --------------------------------------------------
  rad2mas         = 3600 * 1000 * 180 / pi
  neq%misc%nparms = DBLE(neq%misc%npar)
  neq%misc%nsmpnq = 30.d0
  flgHelm         = 0
  wfact           = 1d0
  sigma           = 0d0
  sigma0          = 0d0
  neq_trace       = 0d0
  ncoord          = 0
  numdesc         = 0
  flgSig          = 0
  help            = 0d0
  nStatist        = 0
  oldobs          = 0
  oldpar          = 0
  oldrms          = 0d0
  bernold         = 0

  snxSta%nStaInt = 0
  snxSta%staInt(:)%staNam = ''
  snxSta%staInt(:)%epoch = 0d0
  DO ii=1,3
    snxSta%staInt(:)%crd(ii) = 0d0
    snxSta%staInt(:)%vel(ii) = 0d0
  ENDDO
  snxSta%staInt(:)%timInt%t(1) = 0d0
  snxSta%staInt(:)%timInt%t(2) = 0d0
  snxSta%staInt(:)%flgCon = 2

  timCrd  = 1D20
  numERPpar(:) = 0
  erpIntLength(:) = 0.d0
  mxERPt(:) = -HUGE(r8b)
  mnERPt(:) =  HUGE(r8b)
  mxTRPt    = -HUGE(r8b)
  mnTRPt    =  HUGE(r8b)

  siteant = .FALSE.
  gpsphac = .false.
  solest  = .false.
  solapr  = .false.
  foundCOV(:) = .FALSE.
  foundNEQ(:) = .FALSE.
  flgPWL(:)   = .TRUE.

! Read old style Bernese sinex header
! -----------------------------------
  DO iline = 1, 8
    READ(lfn001,'(A)') line
    IF (line(1:23) == '*   RMS OF UNIT WEIGHT:') THEN
      READ(line,"(23X,F8.4,8X,I11,13X,I9)",IOSTAT=ios) oldrms, oldobs,oldpar
      IF (ios /= 0) THEN
        oldrms = 0d0
        oldobs = 0
        oldpar = 0
      ELSE
        bernold = 1
      ENDIF
    ENDIF
  ENDDO
  REWIND(lfn001)

! Main Loop
! ---------
  Main_Loop: DO
    line = nextline(lfn001,0)
    IF (line      == '%ENDSNX') EXIT  Main_Loop
    IF (line(1:1) /= '+'  ) CYCLE Main_Loop

! Read SITE/ID Block
! ------------------
    IF (line == '+SITE/ID') THEN
      WRITE(lfnprt,"(1X,A)") TRIM(line(2:80))
      neq%misc%nstat_sinex = 0
      DO
        line = nextline(lfn001,0)
        IF (line(1:1) == '-') CYCLE Main_Loop

        READ(line,'(1x,a4, 1x,a2, 1x,a9, 1x,a1)') &
                  siteCode, pointCode, domes, technique

        neq%misc%nstat_sinex = neq%misc%nstat_sinex + 1

        IF (neq%misc%nstat_sinex > maxStaSin) THEN
          WRITE(lfnerr,"(/,' *** SR READSIN: Too many stations', &
                       & /,17X,'Maximum number  :',I6,     &
                       & /,17X,'Requested number:',I6,     &
                       & /,17X,'Sinex file: ',A,/)")       &
                       maxStaSin,neq%misc%nstat_sinex,snxFileName
          CALL exitrc(2)
        ENDIF

        IF (domes == '     M   ') domes = ' '

        WRITE(neq%misc%sinex(neq%misc%nstat_sinex)%stname,             &
                          '(A4,1X,A9,A2)') siteCode, domes, pointCode

        sta_tech(neq%misc%nstat_sinex) = technique

! Initialize
        neq%misc%sinex(neq%misc%nstat_sinex)%timint%t(1)=0D0
        neq%misc%sinex(neq%misc%nstat_sinex)%timint%t(2)=0D0
        neq%misc%sinex(neq%misc%nstat_sinex)%antrec     =' '
        neq%misc%sinex(neq%misc%nstat_sinex)%antsta     =' '
        neq%misc%sinex(neq%misc%nstat_sinex)%antecc     =0D0
        DO iSys = 0,maxSys-1
          neq%misc%sinex(neq%misc%nstat_sinex)%antpcv(iSys)%nFrq   = 0
          neq%misc%sinex(neq%misc%nstat_sinex)%antpcv(iSys)%antphs = 0d0
          neq%misc%sinex(neq%misc%nstat_sinex)%antpcv(iSys)%atxStr = ''
          neq%misc%sinex(neq%misc%nstat_sinex)%antpcv(iSys)%adopted= 0
          neq%misc%sinex(neq%misc%nstat_sinex)%antpcv(iSys)%individ= 0
        ENDDO
      ENDDO
    ENDIF


! Read SITE/RECEIVER Block
! ------------------------
    IF (line == '+SITE/RECEIVER') THEN
      WRITE(lfnprt,"(1X,A)") TRIM(line(2:80))
      DO
        line = nextline(lfn001,0)
        IF (line(1:1) == '-') CYCLE Main_Loop

        READ(line,'(1x,a4, 1x,a2, 1x,a4, 1x,a1, 1x,a12, 1x,a12,      &
          &         1x,a20, 1x,a5, 1x,a11)' )                        &
          siteCode, pointCode, solID, technique, startTime, endTime, &
          antrec

        DO ista = 1, neq%misc%nstat_sinex
          IF ( neq%misc%sinex(ista)%stname(1:4)   == siteCode    .AND. &
               neq%misc%sinex(ista)%stname(15:16) == pointCode ) THEN
            antrec = ADJUSTL(antrec)
            CALL upperc(antrec)
            neq%misc%sinex(ista)%antrec = antrec
            EXIT
          ENDIF
        ENDDO

      ENDDO
    ENDIF


! Read SITE/ANTENNA Block
! -----------------------
    IF (line == '+SITE/ANTENNA') THEN
      WRITE(lfnprt,"(1X,A)") TRIM(line(2:80))
      siteant=.true.
      DO
        line = nextline(lfn001,0)
        IF (line(1:1) == '-') CYCLE Main_Loop

        READ(line,'(1x,a4, 1x,a2, 1x,a4, 1x,a1, 1x,a12, 1x,a12,      &
          &         1x,a20, 1x,a5)' )                                &
          siteCode, pointCode, solID, technique, startTime, endTime, &
          staant, numant

        DO ista = 1, neq%misc%nstat_sinex
          IF ( neq%misc%sinex(ista)%stname(1:4)   == siteCode    .AND. &
               neq%misc%sinex(ista)%stname(15:16) == pointCode ) THEN
            staant = ADJUSTL(staant)
            neq%misc%sinex(ista)%antsta = staant

            IF (numant /= '-----' .AND. numant /= '   ') THEN
              READ(numant,'(i5)',IOSTAT=ios) neq%misc%sinex(ista)%antnum
              neq%misc%sinex(ista)%antpcv(0)%individ = 1
            ELSE
              neq%misc%sinex(ista)%antnum = undef_i
            ENDIF
            EXIT
          ENDIF
        ENDDO

      ENDDO
    ENDIF


! Read GPS_PHASE_CENTER Block
! ---------------------------
    IF (line == '+SITE/GPS_PHASE_CENTER') THEN
      WRITE(lfnprt,"(1X,A)") TRIM(line(2:80))
      gpsphac=.true.
      iant=0
      DO
        line = nextline(lfn001,0)
        IF (line(1:1) == '-') CYCLE Main_Loop
! Read Block in arrays
        iant=iant+1
        nant=iant
        IF (iant > maxStaSin) THEN
          WRITE(lfnerr,"(/,' *** SR READSIN: Too many GPS phase centers', &
                       & /,17X,'Maximum number  :',I6,    &
                       & /,17X,'Sinex file: ',A,/)")      &
               maxStaSin,snxFileName
          CALL exitrc(2)
        ENDIF

        READ(line, '(1x, A20, 1X,A5, 6(1X,F6.4), 1X,A10)')         &
             antsta(iant), numant,                                 &
             antphs(3,1,iant), antphs(1,1,iant), antphs(2,1,iant), &
             antphs(3,2,iant), antphs(1,2,iant), antphs(2,2,iant), &
             atxStr(iant)

        antsta(iant) = ADJUSTL(antsta(iant))
        IF (numant /= '-----' .AND. numant /= '   ') THEN
          READ(numant,'(i5)',IOSTAT=ios) antnum(iant)
        ELSE
          antnum(iant) = undef_i
        ENDIF
      ENDDO
    ENDIF

! If SITE/ANTENNA and GPS_PHASE_CENTER blocks are read find
! corresponding antenna offsets for station
! ---------------------------------------------------------
    IF(siteant .AND. gpsphac) THEN
      DO ista = 1, neq%misc%nstat_sinex
        ifound = 0
! First LOOP for correct antenna radome combination
        DO iant=1, nant
          IF ( neq%misc%sinex(ista)%antsta == antsta(iant)) THEN
            IF ((antnum(iant) /= undef_i .AND.                     &
                 neq%misc%sinex(ista)%antnum == antnum(iant)).OR. &
                 antnum(iant) == undef_i) THEN
              neq%misc%sinex(ista)%antpcv(0)%atxStr = atxStr(iant)
              neq%misc%sinex(ista)%antpcv(0)%adopted= 0
              DO ii=1,3
                DO jj=1,2
                  neq%misc%sinex(ista)%antpcv(0)%antphs(ii,jj) = antphs(ii,jj,iant)
                ENDDO
              ENDDO
              ifound = 1
            ENDIF
          ENDIF
        ENDDO
! Second LOOP for NONE antenna if antenna radome combination not found
        IF (ifound == 0) THEN
          DO iant = 1, nant
            IF ( neq%misc%sinex(ista)%antsta(1:16) == antsta(iant)(1:16) &
                 .AND. antsta(iant)(17:20)=='NONE'.AND.               &
                 antnum(iant) == undef_i) THEN
              neq%misc%sinex(ista)%antpcv(0)%atxStr = atxStr(iant)
              neq%misc%sinex(ista)%antpcv(0)%adopted= 1
              DO ii=1,3
                DO jj=1,2
                  neq%misc%sinex(ista)%antpcv(0)%antphs(ii,jj) = antphs(ii,jj,iant)
                ENDDO
              ENDDO
              ifound = 1
              EXIT
            ENDIF
          ENDDO
          IF (ifound == 0) THEN
            WRITE(lfnerr,"(/,' *** SR READSIN: No antenna found in ',  &
                      &      'GPS_PHASE_CENTER Block',                 &
                      & /,17X,'for station          : ',A4,            &
                      & /,17X,'with receiver antenna: ',A20,           &
                      & /,17X,'Sinex file: ',A,/)")                    &
                 neq%misc%sinex(ista)%stname,neq%misc%sinex(ista)%antsta, &
                 snxFileName
!            CALL exitrc(2)
          ENDIF
        ENDIF
      ENDDO
    ENDIF

! Read SITE/ECCENTRICITY Block
! -----------------------
    IF (line == '+SITE/ECCENTRICITY') THEN
      WRITE(lfnprt,"(1X,A)") TRIM(line(2:80))
      DO
        line = nextline(lfn001,0)
        IF (line(1:1) == '-') CYCLE Main_Loop

        READ(line, '(1x,a4, 1x,a2, 1x,a4, 1x,a1, 1x,a12, 1x,a12,         &
          &         1x,a3, 3(1x,f8.4))' )                                &
          siteCode, pointCode, solID, technique, startTime, endTime,     &
          refSys, antecc(3), antecc(1), antecc(2)

        DO ista = 1, neq%misc%nstat_sinex
          IF ( neq%misc%sinex(ista)%stname(1:4)   == siteCode    .AND. &
               neq%misc%sinex(ista)%stname(15:16) == pointCode ) THEN
            neq%misc%sinex(ista)%antecc = antecc
            EXIT
          ENDIF
        ENDDO

      ENDDO
    ENDIF

! Read SATELLITE/ID Block
! -----------------------
    IF (line == '+SATELLITE/ID') THEN
      WRITE(lfnprt,"(1X,A)") TRIM(line(2:80))
      DO
        line = nextline(lfn001,0)
        IF (line(1:1) == '-') CYCLE Main_Loop
      ENDDO
    ENDIF


! Solution/Statistics Block
! -------------------------
    IF (line == '+SOLUTION/STATISTICS') THEN
      WRITE(lfnprt,"(1X,A)") TRIM(line(2:80))
      DO
        line = nextline(lfn001,0)
        IF (line(1:1) == '-') CYCLE Main_Loop

        IF      (line(1:23) == ' NUMBER OF OBSERVATIONS')  THEN
          READ (line(32:),*) neq%misc%nobs
          nStatist = nStatist + 1

        ELSE IF (line(1:28) == ' SAMPLING INTERVAL (SECONDS)')  THEN
          READ (line(32:),*) help
          neq%misc%nsmpnq = NINT(help)

        ELSE IF (line(1:25) == ' PHASE MEASUREMENTS SIGMA')  THEN
          READ (line(32:),*) sigma0
          IF (sigma0 >= 1d-3 .AND. sigma0 <= 2d-3) &
            nStatist = nStatist + 1

        ELSE IF (line(1:16) == ' VARIANCE FACTOR')  THEN
          READ (line(32:),*) wfact
          IF (wfact >= 1d-2 .AND. wfact <= 10d0) &
            nStatist = nStatist + 1

        ELSE IF (line(1:27) == ' WEIGHTED SQUARE SUM OF O-C')  THEN
          READ (line(32:),*) neq%misc%lTPl
          wfact=0d0
          nStatist = nStatist + 1

        ELSE IF (line(1:19) == ' NUMBER OF UNKNOWNS')  THEN
          READ (line(32:),*) neq%misc%nparms
          nStatist = nStatist + 1

        ENDIF
      ENDDO
    ENDIF


! Read SOLUTION/EPOCHS Block
! --------------------------
    IF (line == '+SOLUTION/EPOCHS') THEN
      WRITE(lfnprt,"(1X,A)") TRIM(line(2:80))

      DO
        line = nextline(lfn001,0)
        IF (line(1:1) == '-') CYCLE Main_Loop

        READ(line,'(1x,a4, 1x,a2, 1x,a4, 1x,a1, 1x,a12, 1x,a12, 1x,a12)' ) &
          siteCode, pointCode, solID, technique, startTime, endTime,       &
          meanTime

      ! Fill SINEX info into neq structure
      ! ----------------------------------
        DO ista = 1, neq%misc%nstat_sinex
          IF ( neq%misc%sinex(ista)%stname(1:4)   == siteCode    .AND. &
               neq%misc%sinex(ista)%stname(15:16) == pointCode ) THEN
            CALL sindat(1, neq%misc%sinex(ista)%timint%t(1), startTime)
            CALL sindat(2, neq%misc%sinex(ista)%timint%t(2), endTime)
            idx2 = ista
            EXIT
          ENDIF
        ENDDO

      ! Check which coordinate set should be written to CRD/VEL
      ! -------------------------------------------------------
        CALL sindat(1, validity%t(1), startTime)
        CALL sindat(2, validity%t(2), endTime)

        idx1 = 0
        IF ( crdset == 0 ) THEN
           snxSta%nStaInt = snxSta%nStaInt + 1
           idx1 = snxSta%nStaInt

        ELSEIF ( crdset == 2 ) THEN                !"at Epoch" Tcrdset
          IF ( Tcrdset .isIn. validity )  THEN
             snxSta%nStaInt = snxSta%nStaInt + 1
             idx1 = snxSta%nStaInt
          ENDIF

        ELSEIF ( crdset == 1 ) THEN                !"Latest" set
         ! Check whether interval should be updated
          DO ista = 1, snxSta%nStaInt
            IF ( snxSta%staInt(ista)%staNam(1:4) == siteCode .AND. &
                 snxSta%staInt(ista)%ptCode == pointCode     .AND. &
                 validity%t(1) >= snxSta%staInt(ista)%timInt%t(1) ) THEN

              WRITE(lfnprt,'(9X,A,/,9X,A,1X,A16,1X,A4,/,9X,A,2(1X,A12),/)')       &
                           'New solution number found for CRD/VEL:',              &
                           'Station: ', neq%misc%sinex(ista)%stname(1:16), solID, &
                           'Interval:', startTime, endTime

              idx1 = ista
              EXIT
            ENDIF
          ENDDO  ! ista (snxSta)

          IF ( idx1 == 0 ) THEN
            snxSta%nStaInt = snxSta%nStaInt + 1
            idx1 = snxSta%nStaInt
          END IF

        ENDIF  !crdset

        IF ( idx1 > 0 ) THEN
          snxSta%staInt(idx1)%staNam(1:14)  = neq%misc%sinex(idx2)%stname(1:14)
          snxSta%staInt(idx1)%staNam(15:16) = '  '
          snxSta%staInt(idx1)%ptCode        = pointCode
          READ(solID, '(i4)',iostat=ios) snxSta%staInt(idx1)%solN

          snxSta%staInt(idx1)%timInt%t(1) = neq%misc%sinex(idx2)%timint%t(1)
          snxSta%staInt(idx1)%timInt%t(2) = neq%misc%sinex(idx2)%timint%t(2)

        ENDIF

      ENDDO  ! EPOCHS block
    ENDIF


! Solution/Estimate Block
! -----------------------
    IF (line == '+SOLUTION/ESTIMATE') THEN
      WRITE(lfnprt,"(1X,A)") TRIM(line(2:80))
      solest=.true.

      DO ipar1 = 1, neq%misc%npar
        line = nextline(lfn001,0)
        IF (line(1:1) == '-') THEN
          WRITE(lfnerr,"(/,' *** SR READSIN: Number of parameters does not ',&
                         &     'correspond to the number of ',               &
                         & /,17X,'parameters given in header line.',         &
                         & /,17X,'Number in header line:      ',I6,          &
                         & /,17X,'Number in SOLUTION/ESTIMATE:',I6,          &
                         & /,17X,'Sinex file: ',A,/)")     &
                                               neq%misc%npar,ipar,snxFileName
          CALL exitrc(2)
        ENDIF


        READ(line, '(1x,i5,1x,a6, 1x,a4, 1x,a2, 1x,a4, 1x,a12, 1x,a4, 1x,i1, &
           &         1x,e21.15, 1x,e11.6)')                                  &
           ipar, parType_est(ipar), siteCode, pointCode_est(ipar), solID,    &
           refTime, unit, flgCon, neq%xxx(ipar)
        site_est(ipar) = siteCode
        flgCon_est(ipar) = flgCon
        CALL upperc(site_est(ipar))
        IF ( solID /= '----' ) THEN
          READ(solID, '(i4)',iostat=ios) solID_est(ipar)
        ENDIF

        IF (.NOT. solapr)  &
          CALL reblocq(neq, ipar, parType_est(ipar), siteCode, pointCode_est(ipar),  &
                       solID, reftime, unit, sintim, techSNX, numdesc, snxFileName,  &
                       numERPpar, mxERPt, mnERPt, flgUT1r, nut_unit, mxTRPt, mnTRPt, &
                       timCrd)

        IF ( parType_est(ipar) == 'UT'   ) THEN
          IF     ( neq%xxx(ipar) > 1000D0 ) THEN
            taiutc=taigps+dgpsut(neq%par(ipar)%time%mean)
            neq%xxx(ipar) = taiutc*1000D0 - neq%xxx(ipar)
            WRITE(lfnerr,"(/,' ### SR READSIN: Parameter UT was TAI-UT1, ',  &
                         &                    'but is now corrected to UT1.',&
                         & /,'                 Parameter index:',I6,         &
                         & /,'                 Parameter value:',F12.5,/)")  &
                                               ipar, neq%xxx(ipar)
          ELSEIF ( neq%xxx(ipar) < -1000D0 ) THEN
            taiutc=taigps+dgpsut(neq%par(ipar)%time%mean)
            neq%xxx(ipar) = neq%xxx(ipar) + taiutc*1000D0
            WRITE(lfnerr,"(/,' ### SR READSIN: Parameter UT was UT1-TAI, ',  &
                         &                    'but is now corrected to UT1.',&
                         & /,'                 Parameter index:',I6,         &
                         & /,'                 Parameter value:',F12.5,/)")  &
                                               ipar, neq%xxx(ipar)
          ENDIF

        ELSEIF ( parType_est(ipar) == 'LOD'  ) THEN
          neq%xxx(ipar) = -neq%xxx(ipar)

        ELSEIF ( parType_est(ipar) == 'UTR' ) THEN
          IF     (neq%xxx(ipar) > 1000D0 ) THEN
            taiutc=taigps+dgpsut(neq%par(ipar)%time%mean)
            neq%xxx(ipar) = taiutc*1000D0 - neq%xxx(ipar)
            WRITE(lfnerr,"(/,' ### SR READSIN: Parameter UTR was TAI-UT1R, ', &
                         &                    'but is now corrected to UT1R.',&
                         & /,'                 Parameter index:',I6,          &
                         & /,'                 Parameter value:',F12.5,/)")   &
                                               ipar, neq%xxx(ipar)
          ELSEIF (neq%xxx(ipar) < -1000D0 ) THEN
            taiutc=taigps+dgpsut(neq%par(ipar)%time%mean)
            neq%xxx(ipar) = neq%xxx(ipar) + taiutc*000D0
            WRITE(lfnerr,"(/,' ### SR READSIN: Parameter UTR was UT1R-TAI, ', &
                         &                    'but is now corrected to UT1R.',&
                         & /,'                 Parameter index:',I6,          &
                         & /,'                 Parameter value:',F12.5,/)")   &
                                               ipar, neq%xxx(ipar)
          ENDIF

        ELSEIF ( parType_est(ipar) == 'LODR' ) THEN
          neq%xxx(ipar) = -neq%xxx(ipar)

        ENDIF

      ENDDO
      line = nextline(lfn001,0)
      IF (line(1:1) /= '-') THEN
        WRITE(lfnerr,"(/,' *** SR READSIN: Number of parameters in ',     &
                       &   'SOLUTION/ESTIMATE block is bigger than ',     &
                       & /,17X,'the number of parameters given in header',&
                       &   'line.',                                       &
                       & /,17X,'Number in header line:      ',I6,         &
                       & /,17X,'Sinex file: ',A,/)")     &
                                            neq%misc%npar,snxFileName
        CALL exitrc(2)
      ENDIF

    ENDIF


! Solution/Apriori Block
! ----------------------
    IF (line == '+SOLUTION/APRIORI') THEN
      WRITE(lfnprt,"(1X,A)") TRIM(line(2:80))
      solapr=.true.

      line = nextline(lfn001,0)
      DO WHILE (line(1:1) /= ' ')
        line = nextline(lfn001,0)
      ENDDO

      ipar=0
      DO WHILE(line(1:1) /= '-')

        READ(line,*) ipar1, parType

        IF     ( parType(1:3) == 'TX ' ) THEN
          numdesc(10) = numdesc(10) + 1
          READ(line, '(69x,e11.6)')  sigma(1)
          flgHelm = 1
          line = nextline(lfn001,0)

        ELSEIF ( parType(1:3) == 'TY ' ) THEN
          READ(line, '(69x,e11.6)')  sigma(2)
          flgHelm = 1
          line = nextline(lfn001,0)

        ELSEIF ( parType(1:3) == 'TZ ' ) THEN
          READ(line, '(69x,e11.6)')  sigma(3)
          flgHelm = 1
          line = nextline(lfn001,0)

        ELSEIF ( parType(1:3) == 'RX ' ) THEN
          numdesc(11) = numdesc(11) + 1
          READ(line, '(69x,e11.6)')  sigma(4)
          sigma(4) = sigma(4) * pi / (3600000*180)
          flgHelm = 1
          line = nextline(lfn001,0)

        ELSEIF ( parType(1:3) == 'RY ' ) THEN
          READ(line, '(69x,e11.6)')  sigma(5)
          sigma(5) = sigma(5) * pi / (3600000*180)
          flgHelm = 1
          line = nextline(lfn001,0)

        ELSEIF ( parType(1:3) == 'RZ ' ) THEN
          READ(line, '(69x,e11.6)')  sigma(6)
          sigma(6) = sigma(6) * pi / (3600000*180)
          flgHelm = 1
          line = nextline(lfn001,0)

        ELSEIF ( parType(1:3) == 'SC ' ) THEN
          numdesc(12) = numdesc(12) + 1
          READ(line, '(69x,e11.6)')  sigma(7)
          sigma(7) = sigma(7) * 1.0E-9
          flgHelm = 1
          line = nextline(lfn001,0)

        ELSEIF ( parType(1:3) == 'TXR' .OR. parType(1:3) == 'RXR' .OR. &
                 parType(1:3) == 'TYR' .OR. parType(1:3) == 'RYR' .OR. &
                 parType(1:3) == 'TZR' .OR. parType(1:3) == 'RZR' .OR. &
                 parType(1:3) == 'SCR' ) THEN
          WRITE(lfnerr,"(/,' *** SR READSIN: Rates for transformation ',  &
                           &   'parameters in SOLUTION/APRIORI block',    &
                           & /,17X,'not yet supported.',     &
                           & /,17X,'Sinex file: ',A,/)")snxFileName
          line = nextline(lfn001,0)
!            CALL exitrc(2)
        ELSE
          ipar=ipar+1

          IF ( ipar .GT. neq%misc%npar ) THEN
            WRITE(lfnerr,"(/,' *** SR READSIN: Number of parameters in ',  &
                 &   'SOLUTION/APRIORI block is bigger than ',             &
                 & /,17X,'the number of parameters given in header line.', &
                 & /,17X,'Number in header line:      ',I6,                &
                 & /,17X,'Sinex file: ',A,/)")     &
                 neq%misc%npar,snxFileName
            CALL exitrc(2)

!            line = nextline(lfn001,0)

          ELSE

            READ(line, '(1x,i5,1x,a6, 1x,a4, 1x,a2, 1x,a4, 1x,a12, 1x,a4,  &
              &         1x,i1,1x,e21.15, 1x,e11.6)')                       &
              idummy, parType_apr(ipar), site_apr(ipar), pointCode_apr(ipar), &
              solID, refTime, unit, flgCon, x0_apr(ipar), constr(ipar)

            CALL sindat(1, refMJD, refTime)

            CALL upperc(site_apr(ipar))

            IF ( solID /= '----' ) THEN
              READ(solID, '(i4)') solID_apr(ipar)
            ENDIF

            indx_apr(ipar)=ipar1

            line = nextline(lfn001,0)

            IF (.NOT. solest)  &
              CALL reblocq(neq, ipar, parType_apr(ipar), site_apr(ipar), pointCode_apr(ipar), &
                     solID, reftime, unit, sintim, techSNX, numdesc, snxFileName, numERPpar,  &
                     mxERPt, mnERPt, flgUT1r, nut_unit, mxTRPt, mnTRPt, timCrd)

            IF ( parType_apr(ipar) == 'UT'   ) THEN
              IF     ( x0_apr(ipar) > 1000D0 ) THEN
                taiutc=taigps+dgpsut(neq%par(ipar)%time%mean)
                x0_apr(ipar) = taiutc*1000D0 - x0_apr(ipar)

                WRITE(lfnerr,"(/,' ### SR READSIN: Parameter UT was TAI-UT1, ',  &
                             &                    'but is now corrected to UT1.',&
                             & /,'                 Parameter index:',I6,         &
                             & /,'                 Parameter value:',F12.5,/)")  &
                                                   ipar, x0_apr(ipar)
              ELSEIF ( x0_apr(ipar) < -1000D0 ) THEN
                taiutc=taigps+dgpsut(neq%par(ipar)%time%mean)
                x0_apr(ipar) = x0_apr(ipar) + taiutc*1000D0
                WRITE(lfnerr,"(/,' ### SR READSIN: Parameter UT was UT1-TAI, ',  &
                             &                    'but is now corrected to UT1.',&
                             & /,'                 Parameter index:',I6,         &
                             & /,'                 Parameter value:',F12.5,/)")  &
                                                   ipar, x0_apr(ipar)
              ENDIF

            ELSEIF ( parType_apr(ipar) == 'LOD'  ) THEN
              x0_apr(ipar) = -x0_apr(ipar)

            ELSEIF ( parType_apr(ipar) == 'UTR' ) THEN
              IF     (x0_apr(ipar) > 1000D0 ) THEN
                taiutc=taigps+dgpsut(neq%par(ipar)%time%mean)
                x0_apr(ipar) = taiutc*1000D0 - x0_apr(ipar)
                WRITE(lfnerr,"(/,' ### SR READSIN: Parameter UTR was TAI-UT1R, ', &
                             &                    'but is now corrected to UT1R.',&
                             & /,'                 Parameter index:',I6,          &
                             & /,'                 Parameter value:',F12.5,/)")   &
                                                   ipar, x0_apr(ipar)
              ELSEIF (x0_apr(ipar) < -1000D0 ) THEN
                taiutc=taigps+dgpsut(neq%par(ipar)%time%mean)
                x0_apr(ipar) = x0_apr(ipar) + taiutc*000D0
                WRITE(lfnerr,"(/,' ### SR READSIN: Parameter UTR was UT1R-TAI, ', &
                             &                    'but is now corrected to UT1R.',&
                             & /,'                 Parameter index:',I6,          &
                             & /,'                 Parameter value:',F12.5,/)")   &
                                                   ipar, x0_apr(ipar)
              ENDIF

            ELSEIF ( parType_apr(ipar) == 'LODR' ) THEN
              x0_apr(ipar) = -x0_apr(ipar)

            ENDIF

          ENDIF

        ENDIF
      ENDDO
      IF (ipar .LT. neq%misc%npar .AND. ipar > 0) THEN
        WRITE(lfnerr,"(/,' *** SR READSIN: Number of parameters does not ',&
                       &     'correspond to the number of ',               &
                       & /,17X,'parameters given in header line.',         &
                       & /,17X,'Number in header line:     ',I6,                &
                       & /,17X,'Number in SOLUTION/APRIORI:',I6,  &
                       & /,17X,'Sinex file: ',A,/)")     &
                                             neq%misc%npar,ipar,snxFileName
        CALL exitrc(2)
      ENDIF
      npar_apr=ipar

    ENDIF


! Matrix/Estimate Block
! ---------------------
    IF (line(1:25) == '+SOLUTION/MATRIX_ESTIMATE') THEN

      IF     ( line(27:32) == 'U COVA' .OR. line(27:32) == 'L COVA') THEN
        matType = 1
      ELSEIF ( line(27:32) == 'U CORR' .OR. line(27:32) == 'L CORR') THEN
        matType = 2
      ELSEIF ( line(27:32) == 'U INFO' .OR. line(27:32) == 'L INFO') THEN
        matType = 3
      ELSE
        WRITE(lfnerr,"(/,' *** SR READSIN: Type not supported ', &
                     &                    'for MATRIX_ESTIMATE', &
                     & /,'                 Type: ',A,/)") line(27:32)
        CALL exitrc(2)
      ENDIF

      foundCOV(1) = .TRUE.

      IF ( flg_neq == 2 .AND. foundNEQ(1) ) THEN
        WRITE(lfnprt,'(1X,A,A)') TRIM(line(2:80)), ' skipped ...'
        CYCLE Main_Loop
      ENDIF

      IF ( matType <= 2 ) THEN
        invert = 1
      ELSE
        invert = 0
      ENDIF

      WRITE(lfnprt,"(1X,A)") TRIM(line(2:80))

      line = nextline(lfn001,0)

      DO WHILE (line(1:1) /= '-')

        READ(line,*) ipar1, ipar2

        IF     (ipar2 == neq%misc%npar) THEN
          nnend = 0
        ELSEIF (ipar2 == neq%misc%npar-1) THEN
          nnend = 1
        ELSE
          nnend = 2
        ENDIF

        READ(line,'(2(1X,I5), 3(1X,E21.14))',iostat=ios) &
            idum1, idum2, (neq%aNor(ikf(ipar1,ipar2+i)), i=0,nnend)

        line = nextline(lfn001,0)

      ENDDO

! CORR-Matrix -> COVA-Matrix
! --------------------------
      IF ( matType == 2 ) THEN
! off diagonal elements
        DO ipar = 1, neq%misc%npar
          DO jpar = 1, ipar-1
            ij = ikf(ipar,jpar)
            neq%aNor(ij) = neq%aNor(ij) * neq%aNor(ikf(ipar,ipar)) &
                                        * neq%aNor(ikf(jpar,jpar))
          ENDDO
        ENDDO
! main diagonal elements
        DO ipar = 1, neq%misc%npar
          neq%aNor(ikf(ipar,ipar)) = neq%aNor(ikf(ipar,ipar))**2
        ENDDO
      ENDIF

    ENDIF


! Matrix/Apriori Block
! --------------------
    IF (line(1:24) == '+SOLUTION/MATRIX_APRIORI') THEN

      IF     ( line(26:31) == 'U COVA' .OR. line(26:31) == 'L COVA') THEN
        matType = 1
      ELSEIF ( line(26:31) == 'U CORR' .OR. line(26:31) == 'L CORR') THEN
        matType = 2
      ELSEIF ( line(26:31) == 'U INFO' .OR. line(26:31) == 'L INFO') THEN
        matType = 3
      ELSE
        WRITE(lfnerr,"(/,' *** SR READSIN: Type not supported ', &
                     &                    'for MATRIX_APRIORI',  &
                     & /,'                 Type: ',A,/)") line(27:32)
        CALL exitrc(2)
      ENDIF

      foundCOV(2) = .TRUE.

      IF ( flg_neq == 2 .AND. foundNEQ(1) ) THEN
        WRITE(lfnprt,'(1X,A,A)') TRIM(line(2:80)), ' skipped ...'
        CYCLE Main_Loop
      ENDIF

      WRITE(lfnprt,"(1X,A)") TRIM(line(2:80))

      IF ( flgSig < 1 )  flgSig = 1

      line = nextline(lfn001,0)

      DO WHILE (line(1:1) /= '-')

        READ(line,*) ipar1, ipar2

        IF     (ipar2 == neq%misc%npar) THEN
          nnend = 0
        ELSEIF (ipar2 == neq%misc%npar-1) THEN
          nnend = 1
        ELSE
          nnend = 2
        ENDIF

        READ(line,'(2(1X,I5), 3(1X,E21.14))',iostat=ios) &
            idum1, idum2, (regMat(ikf(ipar1,ipar2+i)), i=0,nnend)

        line = nextline(lfn001,0)

      ENDDO

! CORR-Matrix -> COVA-Matrix
! --------------------------
      IF ( matType == 2 ) THEN
! off diagonal elements
        DO ipar = 1, neq%misc%npar
          DO jpar = 1, ipar-1
            ij = ikf(ipar,jpar)
            regMat(ij) = regMat(ij) * regMat(ikf(ipar,ipar)) &
                                    * regMat(ikf(jpar,jpar))
          ENDDO
        ENDDO
! main diagonal elements
        DO ipar = 1, neq%misc%npar
          regMat(ikf(ipar,ipar)) = regMat(ikf(ipar,ipar))**2
        ENDDO
      ENDIF

    ENDIF


! Solution/Normal_Equation_Matrix Block
! -------------------------------------
    IF (line(1:32) == '+SOLUTION/NORMAL_EQUATION_MATRIX' .OR.  &
        line(1:20) == '+SOLUTION/NEQ_MATRIX'                  )  THEN

      foundNEQ(1) = .TRUE.

      IF ( flg_neq == 1 .AND. foundCOV(1) ) THEN
        WRITE(lfnprt,'(1X,A,A)') TRIM(line(2:80)), ' skipped ...'
        CYCLE Main_Loop
      ENDIF

      invert = 0

      WRITE(lfnprt,"(1X,A)") TRIM(line(2:80))

      line = nextline(lfn001,0)
      DO WHILE (line(1:1) /= ' ')
        line = nextline(lfn001,0)
      ENDDO

      DO WHILE (line(1:1) /= '-')

        READ(line,*) ipar1, ipar2

        IF (ipar2 == neq%misc%npar) THEN
          nnend = 0
        ELSEIF (ipar2 == neq%misc%npar-1) THEN
          nnend = 1
        ELSE
          nnend = 2
        ENDIF

        READ(line,'(2(1X,I5), 3(1X,E21.14))',iostat=ios) &
            idum1, idum2, (neq%aNor(ikf(ipar1,ipar2+i)), i=0,nnend)

        line = nextline(lfn001,0)

      ENDDO

    ENDIF

! Solution/Normal_Equation_Vector Block
! -------------------------------------
    IF (line(1:32) == '+SOLUTION/NORMAL_EQUATION_VECTOR' .OR.  &
        line(1:20) == '+SOLUTION/NEQ_VECTOR'                  ) THEN

      foundNEQ(2) = .TRUE.

      IF ( flg_neq == 1 .AND. foundCOV(1) ) THEN
        WRITE(lfnprt,'(1X,A,A)') TRIM(line(2:80)), ' skipped ...'
        CYCLE Main_Loop
      ENDIF

      WRITE(lfnprt,"(1X,A)") TRIM(line(2:80))

      line = nextline(lfn001,0)
      DO WHILE (line(1:1) /= ' ')
        line = nextline(lfn001,0)
      ENDDO

      DO WHILE(line(1:1) /= '-')

        READ(line,'(1X,I5,1X,A6,34X, E21.14)') ipar,parType,neq%bNor(ipar)
! Alternated sign for LOD/LODR bNor
        IF (parType == 'LOD' .OR. &
            parType == 'LODR') neq%bNor(ipar) = -neq%bNor(ipar)
        line = nextline(lfn001,0)

      ENDDO

    ENDIF


  ENDDO Main_Loop

  CLOSE(lfn001)

  IF (solest) THEN
! Sort matrix a priori
! --------------------
! Construct mapping index
    DO ipar=1,npar_apr
      DO ipar1=1,neq%misc%npar
        IF ( site_apr(ipar)      == site_est(ipar1)      .AND.   &
             pointCode_apr(ipar) == pointCode_est(ipar1) .AND.   &
             solID_apr(ipar)     == solID_est(ipar1)     .AND.   &
             parType_apr(ipar)   == parType_est(ipar1)   ) THEN
          ipar_apr(ipar1)=ipar
          indx_hlp(ipar1)=indx_apr(ipar)
          EXIT
        ENDIF
      ENDDO
    ENDDO

! Sort matrix
! -----------
    DO iTo=1,neq%misc%nPar-1
      iFrom=indx_hlp(iTo)
      IF (iFrom==0)   CYCLE
      IF (iFrom==iTo) CYCLE

! Switch columns
      DO iPar=1, neq%misc%nPar
        idx1 = ikf(iFrom, iPar)
        idx2 = ikf(iTo,   iPar)
        help = regMat(idx1)
        regMat(idx1) = regMat(idx2)
        regMat(idx2) = help
      END DO

! Exchange diagonal and off diagonal element
      idx1 = ikf(iFrom, iTo)
      idx2 = MIN(iFrom, iTo)
      idx2 = ikf(idx2, idx2)
      help = regMat(idx1)
      regMat(idx1) = regMat(idx2)
      regMat(idx2) = help

! Update index array
      indx_hlp(iTo) = iTo
      DO idx1=iTo+1, neq%misc%nPar
        IF (indx_hlp(idx1)==iTo) THEN
          indx_hlp(idx1) = iFrom
          EXIT
        END IF
      END DO
    ENDDO

  ELSE
    DO ipar=1,npar_apr
      ipar_apr(ipar)=ipar
    ENDDO

  ENDIF

! Remove solution ID of last Solution
! -----------------------------------
  nsitelst=1
  siteLst1(:)=''
  first=.TRUE.
  DO ipar=neq%misc%npar,1,-1
    IF (neq%par(ipar)%locq(1)==1)THEN
      IF (first) THEN
        first=.FALSE.
        siteLst1(1)=neq%par(ipar)%name(1:15)
      ENDIF
      DO isol=1,nsitelst
        IF (siteLst1(isol)==neq%par(ipar)%name(1:15)) THEN
          neq%par(ipar)%name(15:15)=' '
        ELSEIF (siteLst1(isol)(1:14)==neq%par(ipar)%name(1:14)) THEN
          EXIT
        ELSEIF (isol==nsitelst) THEN
          nsitelst=nsitelst+1
          siteLst1(nsitelst)=neq%par(ipar)%name(1:15)
          neq%par(ipar)%name(15:15)=' '
        ENDIF
      ENDDO
    ENDIF
  ENDDO

! Reconstruction of original NEQ info from Bernese SINEX data
! -----------------------------------------------------------
  IF (berneq == 1) THEN

    ! new style Bernese SINEX file (ADDNEQ2)
!!!    IF (nStatist == 4) THEN
    IF (nStatist == 3) THEN
      WRITE(lfnprt,'(/,A,/,A)')                                      &
        ' Reconstruction of original NEQ information using the    ', &
        ' SOLUTION/STATISTICS block'

    ! old style Bernese SINEX file (ADDNEQ2) or SINEX from other data center
    ! ERROR: no complete solution/statistics block found
    ELSEIF (nStatist < 4 .AND. bernold == 0) THEN
      WRITE(lfnerr,'(/,A,/,17X,3(A,/))')                                        &
        ' *** SR READSIN: SOLUTION/STATISTICS Block incomplete for SINEX file:',&
                          TRIM(snxFileName),                                    &
        '                 Reconstruction of original NEQ information is not  ', &
        '                 possible.'
      CALL exitrc(2)

    ! very old style Bernese SINEX file (ADDNEQ1)
    ELSEIF (bernold == 1) THEN
      neq%misc%nobs   = DBLE(oldobs)
      neq%misc%nparms = DBLE(oldpar)
      sigma0          = oldrms
      wfact           = 1d0
      WRITE(lfnprt,'(/,A,/,A)')                                      &
        ' Reconstruction of original NEQ information using the    ', &
        ' SINEX header'
    ENDIF

  ENDIF

! No reconstruction of original NEQ information requested
  IF (berneq == 0) THEN
    neq%misc%nobs   = DBLE(neq%misc%npar)
    neq%misc%nparms = DBLE(neq%misc%npar)
    sigma0          = sigapri
    wfact           = 1d0
    WRITE(lfnprt,'(/,A,/,A)')                                            &
      ' No reconstruction of original NEQ information.',                 &
      ' Number of observations is set to the number of total parameters.'
  ENDIF

  IF (sigma0 == 0d0) THEN
    WRITE(lfnerr,'(/,A,/)')' *** SR READSIN: Value of a priori sigma is zero.'
!!!    CALL exitrc(2)
    sigma0 = 1.d0
  ENDIF

! Print statistics, number of parameters
! --------------------------------------
  WRITE(lfnprt, '(/,2(A,/))') &
    ' STATISTICS',            &
    ' ----------'

  DO ipar=1,maxdesc
    IF (numdesc(ipar) > 0) THEN
      WRITE(lfnprt,"(1X,A,9X,I12)") descTyp(ipar),numdesc(ipar)
    ENDIF
  ENDDO

  WRITE(lfnprt, '(/,(A,I12,/),(A,F13.0,/),/,3(A,F13.0,/),/,A,F12.5,A,/,A,F12.2,/)')        &
    ' Total number of explicit parameters   ', neq%misc%nPar,                 &
    ' Total number of implicit parameters   ', neq%misc%nParms-neq%misc%nPar, &
    ' Total number of adjusted parameters   ', neq%misc%nParms,               &
    ' Total number of observations          ', neq%misc%nObs,                 &
    ' Degree of freedom (DOF)               ', neq%misc%nObs-neq%misc%nParms, &
    ' A priori sigma of unit weight         ', sigma0, ' m',                  &
    ' Variance factor (Chi**2/DOF)          ', wfact

! m0**2  =  wfact * sigma0**2
  wfact  =  wfact * sigma0**2


! Strip the pointCode from the station name and convert to uppercase
! ------------------------------------------------------------------
  IF(siteant .AND. gpsphac) THEN
    DO ista = 1, neq%misc%nstat_sinex
      IF (neq%misc%sinex(ista)%antrec == '') CYCLE

      neq%misc%sinex(ista)%stname = neq%misc%sinex(ista)%stname(1:14)
      CALL upperc(neq%misc%sinex(ista)%stname)

      ! Skip if not a GNSS station
      ! --------------------------
      IF ( sta_tech(ista) /= 'P' ) CYCLE

      ! Check the tracking capabilities of the receiver
      CALL getrcv(neq%misc%sinex(ista)%antrec,nFrq,iCode,iwlFac,iClass,iSyst)
      neq%misc%sinex(ista)%antpcv(0)%nFrq = nFrq

      DO iSys = 1,2 ! GPS, GLONASS, Galileo
                    ! Should never be bigger than maxsys in $I/M_GOBAL.f90

        IF (iSys == 1 .AND. (iSyst == 0 .OR. iSyst == 2)) CYCLE ! no GLONASS
        IF (iSys == 2 .AND. (iSyst == 0 .OR. iSyst == 1)) CYCLE ! no Galileo

        neq%misc%sinex(ista)%antpcv(iSys)%adopted = &
                                          neq%misc%sinex(ista)%antpcv(0)%adopted
        neq%misc%sinex(ista)%antpcv(iSys)%atxStr  = &
                                          neq%misc%sinex(ista)%antpcv(0)%atxStr
        neq%misc%sinex(ista)%antpcv(iSys)%nFrq    = &
                                          neq%misc%sinex(ista)%antpcv(0)%nFrq
        neq%misc%sinex(ista)%antpcv(iSys)%antphs  = &
                                          neq%misc%sinex(ista)%antpcv(0)%antphs
        neq%misc%sinex(ista)%antpcv(iSys)%individ = &
                                          neq%misc%sinex(ista)%antpcv(0)%individ
      ENDDO

    ENDDO

  ENDIF

! Complete locq array
! compare estimate and apriori solution:
!  - parType                      for ERPs
!  - parType, siteCode            for coordinates / velocities
!  - parType, siteCode, solID     for troposphere
!  - parType, siteCode, pointCode for biases
! set apriori = estimate if no apriori value exists
! ----------------------------------------------------------------
  eopFlg = 0

  IF ( numdesc(16) > 1 ) THEN
    trpIntLength1 = (mxTRPt - mnTRPt) / 2.d0 * &
                    neq%misc%nstat_sinex / (numdesc(16) - 1)
  ELSE
    trpIntLength1 = (mxTRPt - mnTRPt) / 2.d0
  ENDIF

  IF ( numdesc(17) .EQ. 0 ) THEN
    tropgrad = 1
  ELSE
    tropgrad = 3
    trpIntLength2 = (numdesc(16) / numdesc(17)) * trpIntLength1
  ENDIF

! Set time intervals for ERP's drifts
! -----------------------------------
  DO ipar = 1, 5
    IF (numERPpar(ipar) > 1) THEN
      erpIntLength(ipar) = &
           (mxERPt(ipar) - mnERPt(ipar)) / (numERPpar(ipar) - 1) / 2.d0

    ELSEIF (numERPpar(ipar) <= 1) THEN
      erpIntLength(ipar) = (sintim%t(2) - sintim%t(1)) / 2.d0

      ! No Polar motion rates available
      IF ( ipar <= 2 .AND. numdesc(5) == 0 ) THEN
        flgPWL(1:2) = .FALSE.

      ! No LOD / LODR available
      ELSEIF ( ipar == 3 .AND. &
               numdesc(6) == 0 .AND. numdesc(8) == 0 ) THEN
        flgPWL(3) = .FALSE.

      ! No Nutation rates available
      ELSEIF ( ipar >= 4 .AND. numdesc(14) == 0 ) THEN
        flgPWL(4:5) = .FALSE.
      ENDIF

    ENDIF
  ENDDO

  DO ipar = 1, neq%misc%npar

! Earth orientation parameters
!-----------------------------
    IF     ( neq%par(ipar)%locq(1) == 10 ) THEN
      icrd                  = neq%par(ipar)%locq(4)
      ideg                  = neq%par(ipar)%locq(5)
      eopFlg(icrd,ideg)     = eopFlg(icrd,ideg) + 1
      neq%par(ipar)%locq(3) = eopFlg(icrd,ideg)

      IF (neq%par(ipar)%locq(5) == 2 ) THEN
        neq%par(ipar)%time%half = erpIntLength(icrd)

      ELSE
        ! Offset with drift available, or PWL polygon
        IF ( flgPWL(icrd) ) THEN
          neq%par(ipar)%time%half = 0d0

        ! Only 1 Offset and no drift available
        ELSE
          neq%par(ipar)%time%half = erpIntLength(icrd)
        ENDIF

      ENDIF

      IF (neq%par(ipar)%locq(4) == 3 .AND. flgUt1r(ipar) == 1 .AND. solest) THEN
        neq%xxx(ipar) = neq%xxx(ipar) - ut1_ut1r(neq%par(ipar))
      ENDIF

!! set locq(6) to 2, why? and why not above?
!!      neq%par(ipar)%locq(6) = 1
!!      IF ( numdesc(5)>0 .AND. &
!!          (neq%par(ipar)%locq(4)==1 .OR. neq%par(ipar)%locq(4)==2) ) THEN
!!        neq%par(ipar)%locq(6) = 2
!!      ELSE IF ( (numdesc(6)>0 .OR. numdesc(8)>0) .AND. &
!!                neq%par(ipar)%locq(4)==3 ) THEN
!!        neq%par(ipar)%locq(6) = 2
!!      END IF
! Leap Seconds and ut1_ut1r correction
      IF (ipar_apr(ipar) > 0) THEN
        ipar1=ipar_apr(ipar)

!   UT1 or UT1R
        IF   ( neq%par(ipar)%locq(4) == 3 .AND.  &
               neq%par(ipar)%locq(5) == 1) THEN

          IF ( x0_apr(ipar1) > 1000 ) THEN
            taiutc=taigps+dgpsut(refMJD)
            x0_apr(ipar1) = taiutc*1000D0 - x0_apr(ipar1)
          ELSEIF ( x0_apr(ipar1) < -1000 ) THEN
            taiutc=taigps+dgpsut(refMJD)
            x0_apr(ipar1) = x0_apr(ipar1) + taiutc*1000D0
          ENDIF

          IF ( flgUt1r(ipar) == 1 ) THEN
            x0_apr(ipar1) = x0_apr(ipar1) - ut1_ut1r(neq%par(ipar))
          ENDIF

!   LOD
        ELSEIF   ( neq%par(ipar)%locq(4) == 3 .AND.   &
                   neq%par(ipar)%locq(5) == 2 .AND.   &
                   flgUt1r(ipar) == 1 ) THEN
          x0_apr(ipar1) = x0_apr(ipar1) - ut1_ut1r(neq%par(ipar))
        ENDIF

        hlp(ipar) = x0_apr(ipar1)
      ELSE
        hlp(ipar) = neq%xxx(ipar)
      ENDIF

! Change unit for nutation to [mas], [mas/d] (if necessary)
!----------------------------------------------------------
      IF ( numdesc(13) > 0 .AND. nut_unit == 'rad ' ) THEN
        IF (neq%par(ipar)%locq(4) == 4  .OR. neq%par(ipar)%locq(4) == 5 ) THEN

          IF (solest) neq%xxx(ipar)    = rad2mas * neq%xxx(ipar)
          neq%par(ipar)%x0 = rad2mas * neq%par(ipar)%x0
          DO ipar2=ipar, neq%misc%npar
            neq%aNor(ikf(ipar,ipar2)) = rad2mas * neq%aNor(ikf(ipar,ipar2))
            regMat(ikf(ipar,ipar2))   = rad2mas * regMat(ikf(ipar,ipar2))
          ENDDO
          neq%aNor(ikf(ipar,ipar)) = rad2mas * neq%aNor(ikf(ipar,ipar))   !diagonal
          regMat(ikf(ipar,ipar))   = rad2mas * regMat(ikf(ipar,ipar))
        ENDIF
      ENDIF


! Station coordinates and velocities
!-----------------------------------
    ELSEIF ( neq%par(ipar)%locq(1) == 1 ) THEN
      IF (ipar_apr(ipar) > 0) THEN
        hlp(ipar) = x0_apr(ipar_apr(ipar))
      ELSE
        hlp(ipar) = neq%xxx(ipar)
      ENDIF

      ! Check for sets to be saved
      ! --------------------------
      DO ista = 1, snxSta%nStaInt
        IF ( snxSta%staInt(ista)%stanam(1:4) == site_est(ipar) .AND. &
             snxSta%staInt(ista)%ptCode == pointCode_est(ipar) .AND. &
             snxSta%staInt(ista)%solN == solID_est(ipar)            ) THEN

          idx1 = neq%par(ipar)%locq(3)

          IF     ( neq%par(ipar)%locq(4) == 0 ) THEN          ! coordinates
             snxSta%staInt(ista)%crd(idx1) = neq%xxx(ipar)

          ELSEIF ( neq%par(ipar)%locq(4) == 3 ) THEN          ! velocities
             snxSta%staInt(ista)%vel(idx1) = neq%xxx(ipar)
          ENDIF

          snxSta%staInt(ista)%epoch = neq%par(ipar)%time%mean
          snxSta%staInt(ista)%flgCon = flgCon_est(ipar)

        ENDIF
      ENDDO

! Troposphere parameters
!-----------------------
    ELSEIF ( neq%par(ipar)%locq(1) == 6 ) THEN
      neq%par(ipar)%locq(5) = tropgrad

! Set time information
!----------------------
      IF ( neq%par(ipar)%locq(4) == 3 ) THEN
        neq%par(ipar)%time%half = trpIntLength1
      ELSE
        neq%par(ipar)%time%half = trpIntLength2
      ENDIF

!! Search gradients for each zenith delay
      IF ( numdesc(17)>0 .AND. neq%par(ipar)%locq(4)==3 ) THEN
        DO jpar=1, neq%misc%npar
          IF ( neq%par(jpar)%locq(1) == 6                     .AND. &
               (neq%par(jpar)%locq(4)==1 .OR. neq%par(jpar)%locq(4)==2) .AND. &
               neq%par(ipar)%name    == neq%par(jpar)%name    .AND. &
               neq%par(ipar)%locq(6) == neq%par(jpar)%locq(6) .AND. &
               neq%par(jpar)%locq(2) == 0                      ) THEN
            neq%par(jpar)%locq(2) = neq%par(ipar)%locq(2)
          ENDIF
        ENDDO
      ENDIF

! Search ariori values
      IF (ipar_apr(ipar) > 0) THEN
        hlp(ipar) = x0_apr(ipar_apr(ipar))
      ELSE
        hlp(ipar) = neq%xxx(ipar)
      ENDIF


! Bias parameters
!----------------
    ELSEIF ( neq%par(ipar)%locq(1) == 25 .OR. neq%par(ipar)%locq(1) == 26 ) THEN
      IF (ipar_apr(ipar) > 0) THEN
        hlp(ipar) = x0_apr(ipar_apr(ipar))
      ELSE
        hlp(ipar) = neq%xxx(ipar)
      ENDIF

! Set full station name
      DO ista = 1, neq%misc%nstat_sinex
        IF ( neq%misc%sinex(ista)%stname(1:4) /= neq%par(ipar)%name(1:4) ) CYCLE
        neq%par(ipar)%name = neq%misc%sinex(ista)%stname(1:14)
        EXIT
      END DO

! All other parameters
! --------------------
    ELSE
      IF (ipar_apr(ipar) > 0) THEN
        hlp(ipar) = x0_apr(ipar_apr(ipar))
      ELSE
        hlp(ipar) = neq%xxx(ipar)
      ENDIF

    ENDIF

  ENDDO

  neq%par%x0 = hlp

! compute delta_x (estimate - apriori)
! ----------------------------------------
  neq%xxx = neq%xxx - neq%par%x0

  IF (noneq == 1) RETURN

! Set constraints matrix regMat
! -----------------------------
  IF     ( flgSig == 0 .AND. solapr) THEN
    WRITE(lfnprt,"(/,' Apriori constraints matrix taken from SOLUTION/APRIORI block')")
    DO ipar=1, neq%misc%npar
      IF (ipar_apr(ipar) .NE. 0) THEN
        IF ( constr(ipar_apr(ipar)) .NE. 0.d0 ) THEN
          regMat(ikf(ipar,ipar)) = wfact / constr(ipar_apr(ipar))**2
        END IF
      ENDIF
    ENDDO
    foundCOV(2) = .TRUE.

  ELSEIF ( flgSig == 1 ) THEN
    WRITE(lfnprt,"(/,' Apriori constraints matrix taken from SOLUTION/MATRIX_APRIORI block')")

    DO ipar=1, neq%misc%npar
      IF ( regMat(ikf(ipar,ipar)) == 0.d0 .AND. constr(ipar_apr(ipar)) /= 0.d0 ) THEN

        regMat(ikf(ipar,ipar)) = wfact / constr(ipar_apr(ipar))**2
!!!        regMat(ikf(ipar,ipar)) = 1.D0 / constr(ipar_apr(ipar))**2
!!!        regMat(ikf(ipar,ipar)) = 1.D14

        WRITE(lfnprt, '(/,A,A,I5,A,F10.5)')  &
                      ' Apriori Sigma taken from SOLUTION/APRIORI block', &
                      ' for parameter: ', ipar, ', sigma: ', constr(ipar_apr(ipar))
      END IF
    END DO

  ENDIF

! Check priority (and availability) for COVA/NEQ
! ----------------------------------------------
  ! COVA selected and found
  IF ( flg_neq == 1 .AND. foundCOV(1) .AND. foundCOV(2) ) THEN
    WRITE(lfnprt, '(/,A,/,2X,A)')  &
          ' Generation of normal equation system from COVA requested:', &
          'SOLUTION/MATRIX_ESTIMATE and SOLUTION/MATRIX APRIORI found.'

  ! COVA selected but only NEQ found
  ELSEIF ( flg_neq == 1 .AND. &
           (.NOT.foundCOV(1) .OR. .NOT.foundCOV(2)) .AND. &
           foundNEQ(1) .AND. foundNEQ(2)                 ) THEN
    WRITE(lfnprt, '(/,A,/,2X,A,/,2X,A,/,2X,A)')  &
          ' Generation of normal equation system from COVA requested:', &
          'SOLUTION/MATRIX_ESTIMATE and SOLUTION/MATRIX APRIORI not found.', &
          'SOLUTION/NEQ_MATRIX and SOLUTION/NEQ_VECTOR found:', &
          '-> will be used for NEQ generation.'

  ! NEQ selected and found
  ELSEIF ( flg_neq == 2 .AND. foundNEQ(1) .AND. foundNEQ(2) ) THEN
    WRITE(lfnprt, '(/,A,/,2X,A)')  &
          ' Generation of normal equation system from NEQ requested:', &
          'SOLUTION/NEQ_MATRIX and SOLUTION/NEQ_VECTOR found.'

  ! NEQ selected but only COVA found
  ELSEIF ( flg_neq == 2 .AND. &
           (.NOT.foundNEQ(1) .OR. .NOT.foundNEQ(2)) .AND. &
           foundCOV(1) .AND. foundCOV(2)                 ) THEN
    WRITE(lfnprt, '(/,A,/,2X,A,/,2X,A,/,2X,A)')  &
          ' Generation of normal equation system from NEQ requested:', &
          'SOLUTION/NEQ_MATRIX and SOLUTION/NEQ_VECTOR not found.', &
          'SOLUTION/MATRIX_ESTIMATE and SOLUTION/MATRIX APRIORI found:', &
          '-> will be used for NEQ generation.'

  ! Information is incomplete -> NEQ cannot be retrieved
  ELSE
    WRITE(lfnprt, '(/,A,/,2X,A,/,2X,A)')  &
          ' Normal equation system cannot be generated:', &
          'neither SOLUTION/NEQ_MATRIX and SOLUTION/NEQ_VECTOR found,', &
          'nor SOLUTION/MATRIX_ESTIMATE and SOLUTION/MATRIX APRIORI found.'
    RETURN

  ENDIF


! Invert the matrices, compute right-hand side, and subtract the constraints
! --------------------------------------------------------------------------
  IF (invert == 1) THEN

    neq%aNor = neq%aNor / wfact

    CALL syminvg(neq%misc%npar,neq%aNor(:),0,nsing, sbrName=sbrName)
    IF ( flgSig .NE. 0 ) THEN
      regMat   = regMat   / wfact
      CALL syminvg(neq%misc%npar,regMat(:),0,nsing, sbrName=sbrName)
    ENDIF

    CALL solve(neq%misc%npar,neq%aNor(:),neq%xxx(:),neq%bNor(:))

    neq%aNor = neq%aNor - regMat

! remove inner constraints (Helmert-parameters)
!----------------------------------------------
    IF (flgHelm == 1) THEN
      CALL freenet2(neq, hlp, sigma, wfact)
    ENDIF

! additional rotation-parameters
!-------------------------------
    IF (optrot == 1) THEN
      CALL add_rot(neq)
    ENDIF

  ENDIF

! compute the mean main-diagonal-element of the normal equation matrix
! (only for station coordinates)
!---------------------------------------------------------------------
  DO ipar=1, neq%misc%npar
    IF ( neq%par(ipar)%locq(1) == 1 .AND. neq%par(ipar)%locq(4) == 0) THEN
      neq_trace = neq_trace + neq%aNor(ikf(ipar,ipar))
      ncoord    = ncoord + 1
    ENDIF
  ENDDO
  IF (ncoord /= 0D0) neq_trace = neq_trace / ncoord
  WRITE(lfnprt,"(/,' Mean main-diagonal-element of coordinates on NEQ: ', &
                 & ES13.4,/,/)") neq_trace

! Retrieve unconstrained solution vector for initialization of lTPl
! -----------------------------------------------------------------
  IF (berneq == 0 .AND. solest) THEN
    DO i = 1,neq%misc%npar*(neq%misc%npar+1)/2
      IF (regMat(i) /= 0.d0) THEN
        regMat(:) = neq%aNor(:)
        CALL syminvg(neq%misc%npar,regMat(:),0,nsing,sbrName=sbrName)
        CALL solve(neq%misc%npar,regMat(:),neq%bNor(:),neq%xxx(:))

        EXIT
      ENDIF
    ENDDO
  ENDIF

! Initialize lTPl
! ---------------
  IF (wfact /= 0.d0 .AND. neq%misc%nobs /= 0.d0) THEN
    neq%misc%lTPl = wfact * (neq%misc%nobs - neq%misc%nparms) +  &
                      DOT_PRODUCT(neq%xxx,neq%bNor)
  ENDIF

! complete neq-structure for troposphere parameters
!  attention: components set to
!  - use values from atmospheric model
!  - dry Saastamoinen with dry Niell mapping function as apriori model
!  - wet Niell mapping function for troposphere estimation
!---------------------------------------------------------------------
  IF ( numdesc(16) > 0 ) THEN
    neq%misc%itropo = 15
    neq%misc%iextra = 1
    neq%misc%itrmap = 4
  ENDIF
  IF ( numdesc(17) > 0 ) THEN
    neq%misc%itrgrd = 1
  ENDIF


  DEALLOCATE(sta_tech,      stat=iac)
  DEALLOCATE(indx_hlp,      stat=iac)
  DEALLOCATE(ipar_apr,      stat=iac)
  DEALLOCATE(indx_apr,      stat=iac)
  DEALLOCATE(flgCon_est,    stat=iac)
  DEALLOCATE(solID_est,     stat=iac)
  DEALLOCATE(solID_apr,     stat=iac)
  DEALLOCATE(pointCode_apr, stat=iac)
  DEALLOCATE(pointCode_est, stat=iac)
  DEALLOCATE(constr,        stat=iac)
  DEALLOCATE(x0_apr,        stat=iac)
  DEALLOCATE(flgUt1r,       stat=iac)
  DEALLOCATE(hlp,           stat=iac)
  DEALLOCATE(site_est,      stat=iac)
  DEALLOCATE(site_apr,      stat=iac)
  DEALLOCATE(parType_apr,   stat=iac)
  DEALLOCATE(parType_est,   stat=iac)
  DEALLOCATE(neq%xxx,       stat=iac)
  DEALLOCATE(regMat,        stat=iac)

END SUBROUTINE readsin

END MODULE

