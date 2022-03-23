
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE p_fodits

! -------------------------------------------------------------------------
! Purpose:    This module defines structures and global variables for
!             the program FODITS
!
! Author:     Luca Ostini
!
! Created:    14-Aug-2008
! Last mod.:  28-Oct-2010
!
! Changes:    14-Aug-2008 LO: First design
!             20-Sep-2008 LO: Earthquake constants changed.
!             02-Oct-2008 LO: First revision
!             09-Oct-2008 LO: Second revision
!             09-Oct-2008 LO: Third revision
!             09-Oct-2008 LO: warnTooManyOutlPerc = 40
!             05-Dec-2008 LO: Fourth revisio: velocity changes allowed
!             11-Feb-2009 LO: Fifth revision: major changes
!             11-Mar-2009 LO: Fourier Series implemented
!             17-Jun-2009 LO: FFT Introduced
!             13-Aug-2009 LO: Major modifications
!             19-Aug-2009 LO: MENUAUX variables included
!             20-Aug-2009 LO: Minimal number of observations in opt added
!             15-Sep-2009 LO: Write to external file added
!             25-Sep-2009 LO: Changes for F90 consistency
!             18-Nov-2009 LO: Major changes for consistency
!             21-Dec-2009 LO: FFT removed and several changes apported
!             01-Feb-2010 LO: Additional criteria addded for velo and peri
!             02-Mar-2010 LO: Major changes do to elimination of FODISUBR
!             07-Apr-2010 LO: Major changes do to algorithm and output file
!             16-Jun-2010 LO: Time series diveded by component
!             26-Aug-2010 LO: Architectural changes
!             28-Oct-2010 SL: flgStaFile (INTEGER->CHARACTER)
!             03-Jan-2010 LO: INTENT(INOUT) removed and bug fixed
!             24-May-2011 LO: New update of ADDNEQ2 meta-data
!             19-Jul-2011 LO: Test datum defintion added
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, staNameLength, fileNameLength, &
                      shortLineLength, fileNameLength80, lfnprt, lfnerr
  USE m_time,   ONLY: t_timint, OPERATOR(.isIn.)

  USE d_const,  ONLY: pi
  USE d_staCrx, ONLY: t_staCrux, init_stacrux

  USE s_alcerr

  IMPLICIT NONE

! -------------------------------------------------
! Global variables
! -------------------------------------------------
  INTEGER(i4b),PARAMETER     :: nMaxRenaming = 50  ! Max renamings per station
  CHARACTER(LEN=3),PARAMETER :: flgStaFileForFodits = '003'
  REAL(r8b),PARAMETER        :: defaultDVal = 0.001D0
  CHARACTER(LEN=staNameLength),PARAMETER :: type3SepLine = 'FODITS exclusion'

! -------------------------------------------------
! Parameters
! -------------------------------------------------
  INTEGER(i4b),PARAMETER     :: inpRes = 0   ! Input residual time series
  INTEGER(i4b),PARAMETER     :: inpCrd = 1   ! Input coordinates time series

  INTEGER(i4b),PARAMETER     :: typeNone = 0   ! None
  INTEGER(i4b),PARAMETER     :: typeJump = 1   ! Discontinuity
  INTEGER(i4b),PARAMETER     :: typeRate = 2   ! Rate
  INTEGER(i4b),PARAMETER     :: typeOutl = 3   ! Outlier
  INTEGER(i4b),PARAMETER     :: typePeri = 4   ! Periodic function
  INTEGER(i4b),PARAMETER     :: typeVelo = 5   ! Velocity change
  INTEGER(i4b),PARAMETER     :: typeJumpVelo = 6 ! Jump and velocity change

  INTEGER(i4b),PARAMETER     :: infoInit = 9   ! Begin of time series
  INTEGER(i4b),PARAMETER     :: infoUnkw = 0   ! Unkown reason
  INTEGER(i4b),PARAMETER     :: infoStaF = 1   ! STA-file (apriori)
  INTEGER(i4b),PARAMETER     :: infoErqF = 2   ! ERQ-file (apriori)
  INTEGER(i4b),PARAMETER     :: infoEvlF = 3   ! EVL-file (apriori)
  INTEGER(i4b),PARAMETER     :: infoPanl = 4   ! PANEL    (apriori)
  INTEGER(i4b),PARAMETER     :: infoDDef = 5   ! TEST DATUM DEF

  INTEGER(i4b),PARAMETER     :: nonSignificant = 0  ! Significance
  INTEGER(i4b),PARAMETER     :: significant = 1     ! Significance

  INTEGER(i4b),PARAMETER     :: elemTst = 0    ! All elements of event equal
  INTEGER(i4b),PARAMETER     :: elemInc = 1    ! Component included
  INTEGER(i4b),PARAMETER     :: elemExl = 2    ! Component excluded
  INTEGER(i4b),PARAMETER     :: elemAll = 3    ! No component tested
  INTEGER(i4b),PARAMETER     :: elemVel = 4    ! Minimal velocity test

  INTEGER(i4b),PARAMETER     :: candNon =  0   ! No candidate
  INTEGER(i4b),PARAMETER     :: candYes =  1   ! Candidate
  INTEGER(i4b),PARAMETER     :: candSub = -1   ! Substituted

  INTEGER(i4b),PARAMETER     :: mAmod = 0      ! Estimate all
  INTEGER(i4b),PARAMETER     :: mRemo = 1      ! Mode remove
  INTEGER(i4b),PARAMETER     :: mApri = 2      ! Mode apriori
  INTEGER(i4b),PARAMETER     :: mIden = 3      ! Mode identify
  INTEGER(i4b),PARAMETER     :: mRemFin = 4    ! Mode remove final

  INTEGER(i4b),PARAMETER     :: lScrn = 1      ! Screening Loop
  INTEGER(i4b),PARAMETER     :: lIter = 2      ! Iteration Loop

  INTEGER(i4b),PARAMETER     :: flagBoh = 0    ! Boh
  INTEGER(i4b),PARAMETER     :: flagTst = 1    ! Event to be estimated
  INTEGER(i4b),PARAMETER     :: flagSet = 2    ! Event to be set in model
  INTEGER(i4b),PARAMETER     :: flagEst = 3    ! New identified
  INTEGER(i4b),PARAMETER     :: flagNot = 4    ! Interval of no such event

  INTEGER(i4b),PARAMETER     :: veloRelCstr = 1 ! Rel. velocity constraints
  INTEGER(i4b),PARAMETER     :: nonVeloCstr = 0 ! No rel. velocity contraints

! -------------------------------------------------
! opt : Option structure
! -------------------------------------------------
! Opt struct contains all input informations from the input panel.

  TYPE t_opt
   ! Panel 0, Read Options (for MENUAUX)
     INTEGER(i4b)                         :: rdOnlyStaNames
   ! Panel 1, Input files
     ! INPUT FILES
     INTEGER(i4b)                         :: selInpTsType
     CHARACTER(LEN=fileNameLength)        :: inPltFile
     INTEGER(i4b)                         :: inPltFileVciEna
     REAL(r8b)                            :: inPltFileViM0
     CHARACTER(LEN=fileNameLength)        :: inCrdFile
     CHARACTER(LEN=fileNameLength)        :: inVelFile
     INTEGER(i4b)                         :: nCrds
     CHARACTER(LEN=fileNameLength),DIMENSION(:),POINTER  :: crdFileName
     ! PREDEFINED EVENTS
     CHARACTER(LEN=fileNameLength)        :: inEvlFile
     CHARACTER(LEN=fileNameLength)        :: inStaFile
     CHARACTER(LEN=fileNameLength80)      :: inErqFile

   ! Panel 1.1, General files
     ! GENERAL INPUT FILES
     CHARACTER(LEN=fileNameLength)        :: const
     CHARACTER(LEN=fileNameLength)        :: datum

   ! Panel 1.2, General variables
     ! VARIABLES FOR A PRIORI EVENTS
     REAL(r8b)                            :: gvarErqFactA
     REAL(r8b)                            :: gvarErqFactB
     REAL(r8b)                            :: gvarErqMinDist
     ! RESIDULAS TIME SERIES
     INTEGER(i4b)                         :: inCrdFileAddPlt
     INTEGER(i4b)                         :: inVelFileAddPlt
     ! ALGORITHM VARIABLES
     INTEGER(i4b)                         :: gvarMaxNrIterStep
     INTEGER(i4b)                         :: gvarMaxNrScrnStep
     ! NEW IDENTIFIED ELEMENTS
     INTEGER(i4b)                         :: modNNewOutl
     REAL(r8b)                            :: modNOutlRms

   ! Panel 2, Output files
     ! RESULT FILES
     CHARACTER(LEN=fileNameLength)        :: outEvlFile
     CHARACTER(LEN=fileNameLength)        :: outStaFileForAddneq2
   ! OUTPUT FILES
     CHARACTER(LEN=fileNameLength)        :: outPltFile
   ! GENERAL OUTPUT FILES
     INTEGER(i4b)                         :: outFileVerbose
   ! Special switches
     INTEGER(i4b)                         :: outFileVerboseT
     INTEGER(i4b)                         :: outFileVerboseTT

   ! Panel 3, Handling of information coming from the input files
     ! TITLE
     CHARACTER(LEN=80)                    :: title
     ! INPUT STATION NAMES
     INTEGER(i4b)                         :: inTruncStaName
     INTEGER(i4b)                         :: nStaSel
     CHARACTER(LEN=staNameLength),DIMENSION(:),POINTER :: inStaSelection
     INTEGER(i4b)                         :: inMinNrObs
     ! TEST EVENTS READ IN INPUT FILES
     INTEGER(i4b)                         :: tstEvlTst
     INTEGER(i4b)                         :: tstStaRenamings
     INTEGER(i4b)                         :: tstRecTypeChanges
     INTEGER(i4b)                         :: tstRecNumberChanges
     INTEGER(i4b)                         :: tstAntTypeChanges
     INTEGER(i4b)                         :: tstAntNumberChanges
     INTEGER(i4b)                         :: tstEccChanges

   ! Panel 4, Model definition
     ! PRE-ANALYSIS STEP
     INTEGER(i4b)                         :: modPreDatTest
     ! VELOCITY CHANGES
     REAL(r8b)                            :: minIntervForVel
     ! PERIODIC FUNCTIONS
     INTEGER(i4b)                         :: modPerAnn
     INTEGER(i4b)                         :: modAddEna
     INTEGER(i4b)                         :: nAddPerParam
     REAL(r8b),DIMENSION(:),POINTER       :: modAddPerParam

   ! Panel 4.1, Test datum definition by Helmert transformation
     ! REFERENCE DATUM
     CHARACTER(LEN=fileNameLength)        :: inDatumCrdFile
     CHARACTER(LEN=fileNameLength)        :: inDatumVelFile
     CHARACTER(LEN=fileNameLength)        :: inDatumFixFile
     ! HELMERT TRANSFORMATION
     INTEGER(i4b)                         :: datHlmTx
     INTEGER(i4b)                         :: datHlmTy
     INTEGER(i4b)                         :: datHlmTz
     INTEGER(i4b)                         :: datHlmRx
     INTEGER(i4b)                         :: datHlmRy
     INTEGER(i4b)                         :: datHlmRz
     INTEGER(i4b)                         :: datHlmSc
     ! OUTLIER REJECTION FOR STATION COORDINATES
     INTEGER(i4b)                         :: datRejectCrd
     REAL(r8b)                            :: datNLimitCrd
     REAL(r8b)                            :: datELimitCrd
     REAL(r8b)                            :: datULimitCrd
     INTEGER(i4b)                         :: datRejectOutlCrd

   ! Panel 5, Statistical tests
     ! STATISTICAL TESTS
     REAL(r8b)                            :: modTestValue
     REAL(r8b)                            :: modOutlTestValue
     ! MINIMAL ADDITIONAL CRITERIA TO TEST PARAMETERS
     REAL(r8b)                            :: modJumpSpac
     REAL(r8b)                            :: modJumpSing
     REAL(r8b)                            :: modJumpHori
     REAL(r8b)                            :: modJumpVert
     REAL(r8b)                            :: modVeloSpac
     REAL(r8b)                            :: modVeloSing
     REAL(r8b)                            :: modVeloHori
     REAL(r8b)                            :: modVeloVert
     REAL(r8b)                            :: modOutlSpac
     REAL(r8b)                            :: modOutlSing
     REAL(r8b)                            :: modOutlHori
     REAL(r8b)                            :: modOutlVert
     REAL(r8b)                            :: modPeriSpac
     REAL(r8b)                            :: modPeriSing
     REAL(r8b)                            :: modPeriHori
     REAL(r8b)                            :: modPeriVert

   ! Panel 6, Spectral analysis to identify unknown periodic functions
     ! IDENTIFICATION STEP
     INTEGER(i4b)                         :: modNewJumpIdentify
     INTEGER(i4b)                         :: modNewVeloIdentify
     INTEGER(i4b)                         :: modNewOutlIdentify
     INTEGER(i4b)                         :: modNewPeriIdentify
     ! NEW IDENTIFIED ELEMENTS
     INTEGER(i4b)                         :: modAddVeloAJ
     ! SEARCH FOR PERIODIC FUNCTIONS
     INTEGER(i4b)                         :: sanNLines
     REAL(r8b)                            :: sanMinPeriod
     REAL(r8b)                            :: sanMaxPeriod

   ! Panel 7, Options to generate files for ADDNEQ2
     ! GENERAL OPTIONS
     REAL(r8b)                            :: outTimRef
     ! REFERENCE SITES
     REAL(r8b)                            :: minTimeIntLength
     REAL(r8b)                            :: minPercDataFix
     ! UPDATE ADDNEQ2 INPUT FILES
     CHARACTER(LEN=fileNameLength)        :: inCrdFileForAddneq2
     CHARACTER(LEN=fileNameLength)        :: inVelFileForAddneq2
     CHARACTER(LEN=fileNameLength)        :: inFixFileForAddneq2
     CHARACTER(LEN=fileNameLength)        :: inSigFileForAddneq2
     CHARACTER(LEN=fileNameLength)        :: outCrdFileForAddneq2
     CHARACTER(LEN=fileNameLength)        :: outVelFileForAddneq2
     CHARACTER(LEN=fileNameLength)        :: outFixFileForAddneq2
     CHARACTER(LEN=fileNameLength)        :: outSigFileForAddneq2
     ! UPDATE STATION INFORMATION
     INTEGER(i4b)                         :: updStaNDays
     REAL(r8b)                            :: gvarCstrCrd
     REAL(r8b)                            :: gvarCstrVel

  END TYPE t_opt

! -------------------------------------------------
! datFil : Data contained in input and output files
! -------------------------------------------------
! This structure contains input and output data (form or towards files)
!
! -- Input files ----------------------------------
! datFil%staIn                                     ! Sta info file struct
!       %evlIn%nSta                                ! Evl info file struct
!             %sta(iSta)%name                      ! Station name
!                       %nEvnt                     ! Number of events
!                       %evnt(iEvnt)%type          ! DISC, OUTL, VELO, PERI
!                                   %omega         ! Omega (PERI)
!                                   %timint%t(:)   ! Epoch (DISC, OUTL, VELO)
!                                                  ! or Interval of event
!                                   %flag          ! EST, NEW, SET, or NOT
!                                   %remark        ! 8-chars information
!                                   %par(3)        ! North East Up values 3D
!                                   %val           ! Value
!                                   %dVal          ! Standard deviation
!                                   %stTst         ! Statistical Test
!                                   %siTst         ! Significant (=1)
!       %erqIn%nErq                                ! Erq info file struct
!             %erq(iErq)%mjd                       ! Epoch of the earthquake
!                       %lat                       ! Latitude
!                       %lon                       ! Longitude
!                       %crdXyz(3)                 ! xyz coordinates
!                       %dep                       ! Depth
!                       %mag                       ! Magnitude
!                       %magDescr                  ! Description Magnitude
! -- Output files ---------------------------------
!       %staOut                                    ! Sta info file struct
!       %evlOut%...                                ! Evl info file struct
! -------------------------------------------------!

  ! -- evl begin ----- !
  TYPE t_evl_sta_evnt
     CHARACTER(LEN=4)                        :: type
     REAL(r8b)                               :: omega
     TYPE(t_timint)                          :: timint
     CHARACTER(LEN=3)                        :: flag
     CHARACTER(LEN=8)                        :: remark
     REAL(r8b),DIMENSION(3)                  :: par
     REAL(r8b)                               :: val
     REAL(r8b)                               :: dVal
     REAL(r8b)                               :: stTst
     INTEGER(i4b)                            :: siTst
     REAL(r8b),DIMENSION(3)                  :: phi
  END TYPE t_evl_sta_evnt

  TYPE t_evl_sta
     CHARACTER(LEN=staNameLength)              :: name
     INTEGER(i4b)                              :: nEvnt
     TYPE(t_evl_sta_evnt),DIMENSION(:),POINTER :: evnt
  END TYPE t_evl_sta

  TYPE t_evl
     INTEGER(i4b)                            :: nSta
     TYPE(t_evl_sta),DIMENSION(:),POINTER    :: sta
  END TYPE t_evl

  ! -- erq begin ----- !
  TYPE t_erq_nr
     REAL(r8b)                               :: mjd
     REAL(r8b)                               :: lat
     REAL(r8b)                               :: lon
     REAL(r8b),DIMENSION(3)                  :: crdXyz
     INTEGER(i4b)                            :: dep
     REAL(r8b)                               :: mag
     CHARACTER(LEN=5)                        :: magDescr
  END TYPE t_erq_nr

  TYPE t_erq
     INTEGER(i4b)                            :: nErq
     TYPE(t_erq_nr),DIMENSION(:),POINTER     :: erq
  END TYPE t_erq

  ! -- main begin ----- !
  TYPE t_datFil
     TYPE(t_staCrux)                         :: staIn
     TYPE(t_evl)                             :: evlIn
     TYPE(t_erq)                             :: erqIn
     TYPE(t_staCrux)                         :: staOut
     TYPE(t_evl)                             :: evlOut
  END TYPE t_datFil

! -------------------------------------------------
! sCore : station time series struct
! -------------------------------------------------
! This struct contains all station related informations
!
! -- Time series (TS) station information struct ---
! sCore%nSta                                       ! Number of stations (or TS)
!      %nVal                                       ! Number of "crd"-components
!      %nTotMjd                                    ! Total num different samples
!      %begMjd                                     ! Very first epoch of all TS
!      %endMjd                                     ! Very last epoch of all TS
!      %deltaMjd                                   ! Sampling rate
!      %inTimRefCrd                                ! Reference epoch inRefCrd
!      %outTimRefCrd                               ! Reference epoch outRefCrd
!      %datumName                                  ! Name of the datum
!      ! -- Station struct -------------------------
!      %sta(iSta)%name                             ! Station name
!                %iRefSta                          ! Renamed reference station
!                %nStaSta                          ! Number of renamed stations
!                %staSta(iStaSta)%rename           ! Renaming letters,e.g.,'ZZ'
!                                %timint%t(2)      ! Interval of renaming
!                                %inCrdXyz(3)      ! Input CRD XYZ [m]
!                                %inCrdNeu(3)      ! Input NEU [deg,deg,m]
!                                %inCrdFlg         ! Input CRD flag
!                                %inVelXyz(3)      ! Input VEL VX,VY,VZ [m/y]
!                                %inVelNeu(3)      ! VN,VE,VU [m/y]
!                                %inVelFlg         ! Input VEL flag
!                                %inVelPlate       ! Input VEL plate
!                %outCrdXyz(3)                     ! Reconstructed CRD XYZ [m]
!                %outVelXyz(3)                     ! Reconstructed VEL XYZ [m/y]
!                %nIterSteps                       ! Number of ATI iteration
!                %modRms                           ! RMS of the model
!                %singular                         ! Singular solution
!                ! -- General time series struct ---
!                %ts%nMjd                          ! Number of epochs
!                   %nVal                          ! Number of crd-components
!                   %mjd(iMjd)                     ! Mjd information
!                   %val(iMjd,nVal)                ! Time series (NEU or nVal)
!                   %dVal(iMjd,nVal)               ! StdDev of Time Series
!                   %vci(iMjd,iCrd,jCrd)           ! (only!3x3) VarCov info
!                                                  ! 1: d11 d12 d13
!                                                  ! 2: d21 d22 d23
!                                                  ! 3: d31 d32 d33
!                   %dDef(iMjd,nVal)               ! Test datum defintion NEU
!                   %dDefRej(iMjd)                 ! Datum defintion rejected
!                                                  ! 0: no fiducial station
!                                                  ! 1: fiducial site
!                                                  ! 2: fiducial site rejected
!                ! -- Functional model struct ------
!                %mod%nEvnt                        ! Nr of known events
!                    %evnt(nPrbl)%type             ! Parameter type (see above)
!                                %info             ! Event's reason (see above)
!                                %mjd              ! Epoch of event
!                                %omega            ! Angular velocity of period
!                                %timint           ! Inverval of event
!                                %flag             ! Flag (see above)
!                                %remark           ! 8-characters information
!                                %index            ! Index for normal equation
!                                %cand             ! =1:candidate purpose
!                                %par(3)           ! Value single par - 3D only
!                                %val              ! value
!                                %dVal             ! Standard deviation of %val
!                                %vlTst            ! Value of overall test
!                                %stTst            ! Result of statistical test
!                                %siTst            ! Significance of stTst
!                                %rename           ! Two chars: 'ZZ,' ',AA,...
!                                %recChg(2)        ! Info from STA-file
!                                %antChg(2)        ! Info from STA-file
!                                %eccChg(3,2)      ! Info from STA-file
!                                %mjd2             ! Support variable - 3D only
!                                %dPar(3)          ! StdDev of par - 3D only
!                                %phi(3)           ! Phase of omega - 3D only
!                                %ppar(3,2)        ! Support variable for PERI
!                                %mod(3)           ! Model Value - 3D only
!                ! - Model update (single station) -
!                %upd%nRen                         ! Nr of renamed stations
!                    %ren(iRen)%type               ! Event type (see above)
!                              %info               ! Evnt info (see above)
!                              %timint%t(:)        ! Interval Begin-End time
!                              %rename             ! Two characters renaming
!                              %cnstr              ! Relative velocity constr.
!                              %group              ! Group assignment:
!                                                  ! 1 longest interval, 2,3,..
!                              %groupLength        ! Group length (delta time)
!                              %groupNumData       ! Num of data in group
!                              %remark             ! Remark
!                    %staStatusRef                 ! 0=noRefSta,1=refSta,2=Rej
!                    %maxTimeInterv                ! Max time interval covered
!      ! -- Test datum defintion -------------------
!      %ddf%mjd(iTotMjd)                           ! iTotMjd of nTotMjd
!          %hlmPar(iTotMjd,7)                      ! (Tx,Ty,Tz,Rx,Ry,Rz,Sc) CRD
!          %rmsHlmPar(iTotMjd,7)                   ! Rms (Tx,Ty,Tz,Rx,Ry,Rz,Sc)
!          %rmsHlmTra(iTotMjd)                     ! Rms transformation
!          %totSta(iTotMjd)                        ! Total number of stations
!          %refSta(iTotMjd)                        ! Number of reference sta
!          %rejSta(iTotMjd)                        ! Number of rejected sta
!          %hlmParOffst(7)                         ! Offsts Hlm Par
!          %hlmParOffstRms(7)                      ! Offsts Rms Hlm Par
!          %hlmParDrift(7)                         ! Drifts Hlm Par
!          %hlmParDriftRms(7)                      ! Drifts Rms Hlm Par
!      ! -- Dynamic functional model ---------------
!      %mdl%evnt                                   ! Temporary event
!          %nApri                                  !
!          %apri(nApri)                            ! Pan with apriori events
!          %nIden                                  !
!          %iden(nIden)                            ! Pan with identified events
!          %nAmod                                  !
!          %amod(nAmod)                            ! Functional model
!          %outlMjd(nMjd)                          ! mjd of OUTL, otherwise 0.0
!      ! -- Least squares adjustment struct --------
!      %lsa%nnPar (=nVal)                          ! Number of parameters
!          %nnMjd (=nVal*nMjd)                     ! Number of epochs
!          %A(nnPar,nnMjd)                         ! First design matrix
!          %Qxx(nnPar,nnPar)                       ! Cofactor matrix = inv(N)
!          %y(nnMjd)                               ! Pseudo-observation vector
!          %x(nnPar)                               ! Parameter solution vector
!          %mod(nnMjd)                             ! Model: Mod = Ax
!          %v(nnMjd)                               ! Residuals v = y - Mod
!          %P(nnMjd)                               ! Weight diagonal matrix
!          %iQyy(nMjd,nVal,nVal)                   ! Inverse of Qyy (nVal=3)
!          %dof                                    ! Degree of freedom
!          %detN                                   ! Determinant of N
!          %vtv                                    ! vt*v
!          %m0                                     ! Sigma of unit weight
!      ! -- Previous iteration step LSA ------------
!      %lsb%prevVtv                                ! vTv of iIter-1
!          %m0                                     ! m0 with all elements
!      ! -- Algorithm variables --------------------
!      %ctr%nIterLoop                              ! Number of iteration steps
!          %modNumIter                             ! Sum epochs in prev. model
!          %modNumIterOutl                         ! Sum epochs in prev. model
!          %nScrnLoop                              ! Number of screening steps
!          %modNumScrn                             ! Sum epochs in prev. model
!          %modNumScrnOutl                         ! Sum epochs in prev. model
!      ! -- Stacked Spectra ------------------------
!      %san%nPer                                   ! Number of functional peri
!          %per(iPer)                              ! Periods
! -------------------------------------------------!

  TYPE t_datumDef
     REAL(r8b), DIMENSION(:), POINTER       :: mjd
     REAL(r8b), DIMENSION(:,:), POINTER     :: hlmPar
     REAL(r8b), DIMENSION(:,:), POINTER     :: rmsHlmPar
     REAL(r8b), DIMENSION(:), POINTER       :: rmsHlmTra
     INTEGER(i4b), DIMENSION(:), POINTER    :: totSta
     INTEGER(i4b), DIMENSION(:), POINTER    :: refSta
     INTEGER(i4b), DIMENSION(:), POINTER    :: rejSta
     REAL(r8b), DIMENSION(7)                :: hlmParOffst
     REAL(r8b), DIMENSION(7)                :: hlmParOffstRms
     REAL(r8b), DIMENSION(7)                :: hlmParDrift
     REAL(r8b), DIMENSION(7)                :: hlmParDriftRms
  END TYPE t_datumDef

  TYPE t_upd_ren
     INTEGER(i4b)                            :: type
     INTEGER(i4b)                            :: info
     TYPE(t_timint)                          :: timint
     CHARACTER(LEN=2)                        :: rename
     INTEGER(i4b)                            :: cnstr
     INTEGER(i4b)                            :: group
     REAL(r8b)                               :: groupLength
     INTEGER(i4b)                            :: groupNumData
     CHARACTER(LEN=8)                        :: remark
  END TYPE t_upd_ren

  TYPE t_upd
     INTEGER(i4b)                            :: nRen
     TYPE(t_upd_ren),DIMENSION(:),POINTER    :: ren
     INTEGER(i4b)                            :: staStatusRef
     REAL(r8b)                               :: maxTimeInterv
  END TYPE t_upd

  TYPE t_evnt
     INTEGER(i4b)                            :: type
     INTEGER(i4b)                            :: info
     REAL(r8b)                               :: mjd
     REAL(r8b)                               :: omega
     TYPE(t_timint)                          :: timint
     INTEGER(i4b)                            :: flag
     CHARACTER(LEN=8)                        :: remark
     INTEGER(i4b)                            :: index
     INTEGER(i4b)                            :: cand
     REAL(r8b),DIMENSION(3)                  :: par
     REAL(r8b)                               :: val
     REAL(r8b)                               :: dVal
     REAL(r8b)                               :: vlTst
     REAL(r8b)                               :: stTst
     INTEGER(i4b)                            :: siTst
     CHARACTER(LEN=2)                        :: rename
     CHARACTER(LEN=20),DIMENSION(2)          :: recChg
     CHARACTER(LEN=20),DIMENSION(2)          :: antChg
     REAL(r8b),DIMENSION(3,2)                :: eccChg
     REAL(r8b)                               :: mjd2
     REAL(r8b),DIMENSION(3)                  :: dPar
     REAL(r8b),DIMENSION(3)                  :: phi
     REAL(r8b),DIMENSION(3,2)                :: ppar
     REAL(r8b),DIMENSION(3)                  :: mod
  END TYPE t_evnt

  TYPE t_san
     INTEGER(i4b)                            :: nPer
     REAL(r8b),DIMENSION(:),POINTER          :: per
  END TYPE t_san

  TYPE t_mdl
     TYPE(t_evnt)                            :: evnt
     INTEGER(i4b)                            :: nApri
     TYPE(t_evnt),DIMENSION(:),POINTER       :: apri
     INTEGER(i4b)                            :: nIden
     TYPE(t_evnt),DIMENSION(:),POINTER       :: iden
     INTEGER(i4b)                            :: nAmod
     TYPE(t_evnt),DIMENSION(:),POINTER       :: amod
     REAL(r8b),DIMENSION(:),POINTER          :: outlMjd
  END TYPE t_mdl

  TYPE t_lsa
     INTEGER(i4b)                            :: nnPar
     INTEGER(i4b)                            :: nnMjd
     REAL(r8b),DIMENSION(:,:),POINTER        :: A
     REAL(r8b),DIMENSION(:,:),POINTER        :: Qxx
     REAL(r8b),DIMENSION(:),POINTER          :: y
     REAL(r8b),DIMENSION(:),POINTER          :: x
     REAL(r8b),DIMENSION(:),POINTER          :: mod
     REAL(r8b),DIMENSION(:),POINTER          :: v
     REAL(r8b),DIMENSION(:),POINTER          :: P
     REAL(r8b),DIMENSION(:,:,:),POINTER      :: iQyy
     INTEGER(i4b)                            :: dof
     REAL(r8b)                               :: detN
     REAL(r8b)                               :: vtv
     REAL(r8b)                               :: m0
  END TYPE t_lsa

  TYPE t_lsb
     REAL(r8b)                               :: prevVtv
     REAL(r8b)                               :: m0
  END TYPE t_lsb

  TYPE t_ctr
     INTEGER(i4b)                            :: nIterLoop
     REAL(r8b),DIMENSION(:),POINTER          :: modNumIter
     REAL(r8b),DIMENSION(:),POINTER          :: modNumIterOutl
     INTEGER(i4b)                            :: nScrnLoop
     REAL(r8b),DIMENSION(:),POINTER          :: modNumScrn
     REAL(r8b),DIMENSION(:),POINTER          :: modNumScrnOutl
     INTEGER(i4b)                            :: nRemoLoop
  END TYPE t_ctr

  TYPE t_mod
     INTEGER(i4b)                            :: nEvnt
     TYPE(t_evnt),DIMENSION(:),POINTER       :: evnt
  END TYPE t_mod

  TYPE t_ts
     INTEGER(i4b)                            :: nVal
     INTEGER(i4b)                            :: nMjd
     REAL(r8b),DIMENSION(:),POINTER          :: mjd
     REAL(r8b),DIMENSION(:,:),POINTER        :: val
     REAL(r8b),DIMENSION(:,:),POINTER        :: dVal
     REAL(r8b),DIMENSION(:,:,:),POINTER      :: vci
     REAL(r8b),DIMENSION(:,:),POINTER        :: dDef
     INTEGER(i4b),DIMENSION(:),POINTER       :: dDefRej
  END TYPE t_ts

  TYPE t_staSta
     CHARACTER(LEN=staNameLength)            :: rename
     TYPE(t_timint)                          :: timint
     REAL(r8b),DIMENSION(3)                  :: inCrdXyz
     REAL(r8b),DIMENSION(3)                  :: inCrdNeu
     CHARACTER(LEN=1)                        :: inCrdFlg
     REAL(r8b),DIMENSION(3)                  :: inVelXyz
     REAL(r8b),DIMENSION(3)                  :: inVelNeu
     CHARACTER(LEN=5)                        :: inVelFlg
     CHARACTER(LEN=4)                        :: inVelPlate
  END TYPE t_staSta

  TYPE t_sCore_stat
     CHARACTER(LEN=staNameLength)            :: name
     INTEGER(i4b)                            :: iRefSta
     INTEGER(i4b)                            :: nStaSta
     TYPE(t_staSta),DIMENSION(:),POINTER     :: staSta
     REAL(r8b),DIMENSION(3)                  :: outCrdXyz
     REAL(r8b),DIMENSION(3)                  :: outVelXyz
     INTEGER(i4b)                            :: nIterSteps
     REAL(r8b)                               :: modRms
     INTEGER(i4b)                            :: singular
     TYPE(t_ts)                              :: ts
     TYPE(t_mod)                             :: mod
     TYPE(t_upd)                             :: upd
  END TYPE t_sCore_stat

  TYPE t_sCore
     INTEGER(i4b)                            :: nSta
     INTEGER(i4b)                            :: nVal
     INTEGER(i4b)                            :: nTotMjd
     REAL(r8b)                               :: begMjd
     REAL(r8b)                               :: endMjd
     REAL(r8b)                               :: deltaMjd
     REAL(r8b)                               :: inTimRefCrd
     REAL(r8b)                               :: outTimRefCrd
     CHARACTER(LEN=16)                       :: datumName
     TYPE(t_sCore_stat),DIMENSION(:),POINTER :: sta
     TYPE(t_mdl)                             :: mdl
     TYPE(t_lsa)                             :: lsa
     TYPE(t_lsb)                             :: lsb
     TYPE(t_ctr)                             :: ctr
     TYPE(t_san)                             :: san
     TYPE(t_datumDef)                        :: ddf
  END TYPE t_sCore

CONTAINS

! -------------------------------------------------------------------------
! Initialization of sCore struct
! -------------------------------------------------------------------------
  SUBROUTINE init_sCore( sCore )
    ! Subroutine Arguments
    ! --------------------
    TYPE(t_sCore)                            :: sCore
    ! Main
    ! ----
    ! sCore
    sCore%nVal = 3
    sCore%nTotMjd = 0
    sCore%begMjd = 44239.0D0       ! 01-Jan-1980 00:00:00
    sCore%endMjd = 58849.0D0       ! 01-Jan-2020 00:00:00
    sCore%deltaMjd = 1.0D0         ! One day
    sCore%inTimRefCrd  = 51544.0D0 ! 01-Jan-2000 00:00:00
    sCore%outTimRefCrd = 51544.0D0 ! 01-Jan-2000 00:00:00
    sCore%datumName = ''
    ! mod
    sCore%nSta = 0
    NULLIFY(sCore%sta)
    ! mdl
    sCore%mdl%nApri = 0
    NULLIFY(sCore%mdl%apri)
    sCore%mdl%nIden = 0
    NULLIFY(sCore%mdl%iden)
    sCore%mdl%nAmod = 0
    NULLIFY(sCore%mdl%amod)
    ! lsa
    sCore%lsa%nnPar = 0
    sCore%lsa%nnMjd = 0
    NULLIFY(sCore%lsa%A)
    NULLIFY(sCore%lsa%P)
    NULLIFY(sCore%lsa%iQyy)
    NULLIFY(sCore%lsa%Qxx)
    NULLIFY(sCore%lsa%y)
    NULLIFY(sCore%lsa%x)
    NULLIFY(sCore%lsa%mod)
    NULLIFY(sCore%lsa%v)
    ! lsb
    sCore%lsb%prevVtv = HUGE(0.0D0)
    sCore%lsb%m0 = 0.0D0
    ! ctr
    sCore%ctr%nIterLoop = 0
    NULLIFY(sCore%ctr%modNumIter)
    NULLIFY(sCore%ctr%modNumIterOutl)
    sCore%ctr%nScrnLoop = 0
    NULLIFY(sCore%ctr%modNumScrn)
    NULLIFY(sCore%ctr%modNumScrnOutl)
    ! san
    sCore%san%nPer = 0
    NULLIFY(sCore%san%per)
    RETURN
  END SUBROUTINE init_sCore

! -------------------------------------------------------------------------
! Initialization of datFil struct
! -------------------------------------------------------------------------
  SUBROUTINE init_datFil( datFil )
    ! Subroutine Arguments
    ! --------------------
    TYPE(t_datFil)                   :: datFil
    ! Main
    ! ----
    ! evl
    datFil%evlIn%nSta = 0
    NULLIFY(datFil%evlIn%sta)
    datFil%evlOut%nSta = 0
    NULLIFY(datFil%evlOut%sta)
    ! erq
    datFil%erqIn%nErq = 0
    NULLIFY(datFil%erqIn%erq)
    ! sta
    CALL init_stacrux(datFil%staIn)
    CALL init_stacrux(datFil%staOut)
    RETURN
  END SUBROUTINE init_datFil

! -------------------------------------------------------------------------
! Initializaton of evnt struct
! -------------------------------------------------------------------------
  SUBROUTINE sCoreInitNewEvnt( nVal, evnt )
    ! Subroutine Arguments
    ! --------------------
    INTEGER(i4b),INTENT(IN)                         :: nVal
    TYPE(t_evnt)                                    :: evnt
    ! Local variables
    ! ---------------
    ! Main
    ! ----
    evnt%type         = typeNone
    evnt%info         = infoUnkw
    evnt%mjd          = 0.0D0
    evnt%omega        = 2*pi/9999.9999D0
    evnt%timint%t(1)  = 0.0D0
    evnt%timint%t(2)  = HUGE(0.0D0)
    evnt%flag         = flagTst
    evnt%remark       = ''
    evnt%index        = 0
    evnt%cand         = 0
    evnt%par(:)       = 0.0D0
    evnt%val          = 0.0D0
    evnt%dVal         = 0.0D0
    evnt%vlTst        = 0.0D0
    evnt%stTst        = 0.0D0
    evnt%siTst        = 0
    evnt%rename       = ''
    evnt%recChg(:)    = ''
    evnt%antChg(:)    = ''
    evnt%eccChg(:,:)  = 0.0D0
    evnt%mjd2         = 1.0D20
    evnt%dPar(:)      = 0.0D0
    evnt%phi(:)       = 0.0D0
    evnt%ppar(:,:)    = 0.0D0
    evnt%mod(:)       = 0.0D0
    RETURN
  END SUBROUTINE sCoreInitNewEvnt

! -------------------------------------------------------------------------
! Add elem to evnt of nEvnt elements
! -------------------------------------------------------------------------
  SUBROUTINE sCoreModAddEvnt( elem, evnt, nEvnt )
    ! Subroutine Arguments
    ! --------------------
    TYPE(t_evnt),INTENT(IN)                           :: elem
    TYPE(t_evnt),DIMENSION(:),POINTER                 :: evnt
    INTEGER(i4b)                                      :: nEvnt
    ! Local Parameters
    ! ----------------
    CHARACTER(LEN=shortLineLength),PARAMETER:: srName = 'SubR'
    ! Local variables
    ! ---------------
    INTEGER(i4b)                             :: iac
    INTEGER(i4b)                             :: iTmp
    TYPE(t_evnt),DIMENSION(:),ALLOCATABLE    :: tmp
    ! Main
    ! ----
    nEvnt = nEvnt + 1
    ALLOCATE(tmp(nEvnt),stat=iac)
    CALL alcerr(iac,'tmp',(/nEvnt/),srName)
    DO iTmp = 1,nEvnt-1
       tmp(iTmp) = evnt(iTmp)
    END DO
    tmp(nEvnt) = elem
    DEALLOCATE(evnt,stat=iac)
    ALLOCATE(evnt(nEvnt),stat=iac)
    DO iTmp = 1,nEvnt
       evnt(iTmp) = tmp(iTmp)
    END DO
    DEALLOCATE(tmp,stat=iac)
    RETURN
  END SUBROUTINE sCoreModAddEvnt

! -------------------------------------------------------------------------
! Delete evnt srtuct of nEvnt or remove jjEvnt from it
! -------------------------------------------------------------------------
  SUBROUTINE sCoreModDelEvnt( evnt, nEvnt, jjEvnt )
    ! Subroutine Arguments
    ! --------------------
    TYPE(t_evnt),DIMENSION(:),POINTER                 :: evnt
    INTEGER(i4b)                                      :: nEvnt
    INTEGER(i4b),OPTIONAL,INTENT(IN)                  :: jjEvnt
    ! Local Parameters
    ! ----------------
    CHARACTER(LEN=shortLineLength),PARAMETER          :: srName = 'SubR'
    ! Local variables
    ! ---------------
    INTEGER(i4b)                                      :: iac
    INTEGER(i4b)                                      :: iTmp
    INTEGER(i4b)                                      :: jTmp
    TYPE(t_evnt),DIMENSION(:),ALLOCATABLE             :: tmp
    ! Main
    ! ----
    IF( nEvnt == 0 )THEN
       RETURN
    END IF
    ALLOCATE(tmp(nEvnt),stat=iac)
    CALL alcerr(iac,'tmp',(/nEvnt/),srName)
    DO iTmp = 1,nEvnt
       tmp(iTmp) = evnt(iTmp)
    END DO
    DEALLOCATE(evnt,stat=iac)
    IF( PRESENT(jjEvnt) .AND. nEvnt > 1 )THEN
       nEvnt = nEvnt - 1
       ALLOCATE(evnt(nEvnt),stat=iac)
       jTmp = 0
       DO iTmp = 1,nEvnt+1
          IF( iTmp == jjEvnt )CYCLE
          jTmp = jTmp+1
          evnt(jTmp) = tmp(iTmp)
       END DO
    ELSE
       nEvnt = 0
       NULLIFY(evnt)
    END IF
    DEALLOCATE(tmp,stat=iac)
    RETURN
  END SUBROUTINE sCoreModDelEvnt

! -------------------------------------------------------------------------
! Sort evnt of nEvnt
! -------------------------------------------------------------------------
  SUBROUTINE sCoreModSortElem( nVal, evnt, nEvnt )
    ! Subroutine Arguments
    ! --------------------
    INTEGER(i4b),INTENT(IN)                         :: nVal
    TYPE(t_evnt),DIMENSION(:),POINTER               :: evnt
    INTEGER(i4b)                                    :: nEvnt
    ! Local variables
    ! ---------------
    TYPE(t_evnt)                         :: dTmp
    INTEGER(i4b)                         :: iTmp
    INTEGER(i4b)                         :: jTmp
    INTEGER(i4b)                         :: kTmp
    INTEGER(i4b)                         :: iindex
    ! Main
    ! ----
    ! Sort: (1) type, (2) mjd, (3) omega
    DO iTmp = 1,nEvnt-1
       kTmp = iTmp
       DO jTmp = iTmp+1,nEvnt
          IF( ( evnt(jTmp)%type < evnt(kTmp)%type     ) .OR. &
              ( evnt(jTmp)%type == evnt(kTmp)%type   .AND. &
                evnt(jTmp)%mjd < evnt(kTmp)%mjd       ) .OR. &
              ( evnt(jTmp)%type == evnt(kTmp)%type   .AND. &
                evnt(jTmp)%mjd == evnt(kTmp)%mjd     .AND. &
                evnt(jTmp)%omega < evnt(kTmp)%omega   )  )THEN
             kTmp = jTmp
          END IF
       END DO
       IF( kTmp /= iTmp )THEN
          dTmp = evnt(iTmp)
          evnt(iTmp) = evnt(kTmp)
          evnt(kTmp) = dTmp
       END IF
    END DO
    ! Set parameter index for the normal equation
    iindex = 0
    DO iTmp = 1,nEvnt
       ! Filter
       IF( evnt(iTmp)%cand == candSub )CYCLE
       IF( evnt(iTmp)%type == typeOutl )CYCLE
       ! Set the index
       evnt(iTmp)%index = iindex
       ! Increment the index
       IF( evnt(iTmp)%type /= typePeri )THEN
          iindex = iindex + nVal
       ELSE
          iindex = iindex + 2*nVal
       END IF
    END DO
    RETURN
  END SUBROUTINE sCoreModSortElem

! -------------------------------------------------------------------------
! Check if elem is contained in evnt of nEvnt
! -------------------------------------------------------------------------
  SUBROUTINE sCoreChkPresEvntInMod( elem, evnt, nEvnt, jjEvnt )
    ! Subroutine Arguments
    ! --------------------
    TYPE(t_evnt)                                    :: elem
    TYPE(t_evnt),DIMENSION(:),POINTER               :: evnt
    INTEGER(i4b)                                    :: nEvnt
    INTEGER(i4b)                                    :: jjEvnt
    ! Local variables
    ! ---------------
    INTEGER(i4b)                                    :: iTmp
    ! Main
    ! ----
    jjEvnt = 0
    DO iTmp = 1,nEvnt
       IF( elem%type  == evnt(iTmp)%type  .AND. &
           elem%omega == evnt(iTmp)%omega .AND. &
           elem%mjd   == evnt(iTmp)%mjd   )THEN
          jjEvnt = iTmp
       END IF
    END DO
    RETURN
  END SUBROUTINE sCoreChkPresEvntInMod

! -------------------------------------------------------------------------
! Sort evnt of nEvnt in increasing order: evnt(1) has the highest vlTst
! -------------------------------------------------------------------------
  SUBROUTINE sCoreSortEventsTstUp( evnt, nElem )
    ! Subroutine Arguments
    ! --------------------
    TYPE(t_evnt),DIMENSION(:),POINTER               :: evnt
    INTEGER(i4b),INTENT(IN)                         :: nElem
    ! Local variables
    ! ---------------
    TYPE(t_evnt)                         :: dTmp
    INTEGER(i4b)                         :: iTmp
    INTEGER(i4b)                         :: jTmp
    INTEGER(i4b)                         :: kTmp
    ! Main
    ! ----
    DO iTmp = 1,nElem-1
       kTmp = iTmp
       DO jTmp = iTmp+1,nElem
          IF( evnt(jTmp)%vlTst > evnt(kTmp)%vlTst )THEN
             kTmp = jTmp
          END IF
       END DO
       IF( kTmp /= iTmp )THEN
          dTmp = evnt(iTmp)
          evnt(iTmp) = evnt(kTmp)
          evnt(kTmp) = dTmp
       END IF
    END DO
    RETURN
  END SUBROUTINE sCoreSortEventsTstUp

! -------------------------------------------------------------------------
! Sort evnt of nEvnt in decreasing order: evnt(1) has the lowest vlTst
! -------------------------------------------------------------------------
  SUBROUTINE sCoreSortEventsTstDown( evnt, nElem )
    ! Subroutine Arguments
    ! --------------------
    TYPE(t_evnt),DIMENSION(:),POINTER               :: evnt
    INTEGER(i4b),INTENT(IN)                         :: nElem
    ! Local variables
    ! ---------------
    TYPE(t_evnt)                         :: dTmp
    INTEGER(i4b)                         :: iTmp
    INTEGER(i4b)                         :: jTmp
    INTEGER(i4b)                         :: kTmp
    ! Main
    ! ----
    DO iTmp = 1,nElem-1
       kTmp = iTmp
       DO jTmp = iTmp+1,nElem
          IF( evnt(jTmp)%vlTst < evnt(kTmp)%vlTst )THEN
             kTmp = jTmp
          END IF
       END DO
       IF( kTmp /= iTmp )THEN
          dTmp = evnt(iTmp)
          evnt(iTmp) = evnt(kTmp)
          evnt(kTmp) = dTmp
       END IF
    END DO
    RETURN
  END SUBROUTINE sCoreSortEventsTstDown

! -------------------------------------------------------------------------
! Test additional criteria
! -------------------------------------------------------------------------
  SUBROUTINE sCoreTstAddCrit( opt, sCore, mode, evnt)
    ! Subroutine Arguments
    ! ---------------------
    TYPE(t_opt)                                     :: opt
    TYPE(t_sCore)                                   :: sCore
    INTEGER(i4b),INTENT(IN)                         :: mode
    TYPE(t_evnt)                                    :: evnt
    ! Local Variables
    ! ---------------
    INTEGER(i4b)                                    :: iVal
    INTEGER(i4b)                                    :: nVal
    INTEGER(i4b)                                    :: siTst
    INTEGER(i4b)                                    :: siTstA
    INTEGER(i4b)                                    :: siTstB
    INTEGER(i4b)                                    :: siTstB1
    INTEGER(i4b)                                    :: siTstB2
    INTEGER(i4b)                                    :: siTstB3
    INTEGER(i4b)                                    :: siTstB4
    REAL(r8b)                                       :: val
    ! Subroutine
    ! ----------

    ! Initializations
    nVal = sCore%nVal

    ! Overall test value to adapt the model
    siTstA = 0
    IF( evnt%type /= typeOutl )THEN
       IF( evnt%vlTst > opt%modTestValue )THEN
          siTstA = 1
       END IF
    ! Special case for outliers
    ELSE
       IF( evnt%vlTst > opt%modOutlTestValue  .OR. &
           evnt%val > opt%modNOutlRms * sCore%lsa%m0 )THEN
          siTstA = 1
       END IF
    END IF

    ! Additional criteria
    siTstB = 0
    siTstB1 = 0
    siTstB2 = 0
    siTstB3 = 0
    siTstB4 = 0
    ! Sizes for jump
    IF     ( evnt%type == typeJump )THEN
       ! Space
       siTstB1 = 0
       IF( opt%modJumpSpac > 0.0D0 )THEN
          siTstB1 = 2
          val = 0.0D0
          DO iVal = 1,nVal
             val = val + evnt%par(iVal)**2
          END DO
          val = SQRT( val )
          IF( val > opt%modJumpSpac ) siTstB1 = 1
       END IF
       ! Single
       siTstB2 = 0
       IF( opt%modJumpSing > 0.0D0 )THEN
          siTstB2 = 2
          DO iVal = 1,nVal
             val = ABS( evnt%par(iVal) )
             IF( val > opt%modJumpSing ) siTstB2 = 1
          END DO
       END IF
       ! Three dimensional time series: Horizontal
       siTstB3 = 0
       IF( nVal == 3 .AND. opt%modJumpHori > 0.0D0 )THEN
          siTstB3 = 2
          val = 0.0D0
          DO iVal = 1,2
             val = val + evnt%par(iVal)**2
          END DO
          val = SQRT( val )
          IF( val > opt%modJumpHori ) siTstB3 = 1
       END IF
       ! Three dimensional time series: Vertical
       siTstB4 = 0
       IF( nVal == 3 .AND. opt%modJumpVert > 0.0D0 )THEN
          siTstB4 = 2
          val = ABS( evnt%par(3) )
          IF( val > opt%modJumpVert ) siTstB4 = 1
       END IF

    ! Sizes for velo
    ELSE IF( evnt%type == typeVelo )THEN
       ! Space
       siTstB1 = 0
       IF( opt%modVeloSpac > 0.0D0 )THEN
          siTstB1 = 2
          val = 0.0D0
          DO iVal = 1,nVal
             val = val + evnt%par(iVal)**2
          END DO
          val = SQRT( val )
          IF( val > opt%modVeloSpac ) siTstB1 = 1
       END IF
       ! Single
       siTstB2 = 0
       IF( opt%modVeloSing > 0.0D0 )THEN
          siTstB2 = 2
          DO iVal = 1,nVal
             val = ABS( evnt%par(iVal) )
             IF( val > opt%modVeloSing ) siTstB2 = 1
          END DO
       END IF
       ! Three dimensional time series: Horizontal
       siTstB3 = 0
       IF( nVal == 3 .AND. opt%modVeloHori > 0.0D0 )THEN
          siTstB3 = 2
          val = 0.0D0
          DO iVal = 1,2
             val = val + evnt%par(iVal)**2
          END DO
          val = SQRT( val )
          IF( val > opt%modVeloHori ) siTstB3 = 1
       END IF
       ! Three dimensional time series: Vertical
       siTstB4 = 0
       IF( nVal == 3 .AND. opt%modVeloVert > 0.0D0 )THEN
          siTstB4 = 2
          val = ABS( evnt%par(3) )
          IF( val > opt%modVeloVert ) siTstB4 = 1
       END IF

    ! Sizes for outl
    ELSE IF( evnt%type == typeOutl )THEN
       ! Space
       siTstB1 = 0
       IF( opt%modOutlSpac > 0.0D0 )THEN
          siTstB1 = 2
          val = 0.0D0
          DO iVal = 1,nVal
             val = val + evnt%par(iVal)**2
          END DO
          val = SQRT( val )
          IF( val > opt%modOutlSpac ) siTstB1 = 1
       END IF
       ! Single
       siTstB2 = 0
       IF( opt%modOutlSing > 0.0D0 )THEN
          siTstB2 = 2
          DO iVal = 1,nVal
             val = ABS( evnt%par(iVal) )
             IF( val > opt%modOutlSing ) siTstB2 = 1
          END DO
       END IF
       ! Three dimensional time series: Horizontal
       siTstB3 = 0
       IF( nVal == 3 .AND. opt%modOutlHori > 0.0D0 )THEN
          siTstB3 = 2
          val = 0.0D0
          DO iVal = 1,2
             val = val + evnt%par(iVal)**2
          END DO
          val = SQRT( val )
          IF( val > opt%modOutlHori ) siTstB3 = 1
       END IF
       ! Three dimensional time series: Vertical
       siTstB4 = 0
       IF( nVal == 3 .AND. opt%modOutlVert > 0.0D0 )THEN
          siTstB4 = 2
          val = ABS( evnt%par(3) )
          IF( val > opt%modOutlVert ) siTstB4 = 1
       END IF

    ! Sizes for peri
    ELSE IF( evnt%type == typePeri )THEN
       ! Space
       siTstB1 = 0
       IF( opt%modPeriSpac > 0.0D0 )THEN
          siTstB1 = 2
          val = 0.0D0
          DO iVal = 1,nVal
             val = val + evnt%par(iVal)**2
          END DO
          val = SQRT( val )
          IF( val > opt%modPeriSpac ) siTstB1 = 1
       END IF
       ! Single
       siTstB2 = 0
       IF( opt%modPeriSing > 0.0D0 )THEN
          siTstB2 = 2
          DO iVal = 1,nVal
             val = ABS( evnt%par(iVal) )
             IF( val > opt%modPeriSing ) siTstB2 = 1
          END DO
       END IF
       ! Three dimensional time series: Horizontal
       siTstB3 = 0
       IF( nVal == 3 .AND. opt%modPeriHori > 0.0D0 )THEN
          siTstB3 = 2
          val = 0.0D0
          DO iVal = 1,2
             val = val + evnt%par(iVal)**2
          END DO
          val = SQRT( val )
          IF( val > opt%modPeriHori ) siTstB3 = 1
       END IF
       ! Three dimensional time series: Vertical
       siTstB4 = 0
       IF( nVal == 3 .AND. opt%modPeriVert > 0.0D0 )THEN
          siTstB4 = 2
          val = ABS( evnt%par(3) )
          IF( val > opt%modPeriVert ) siTstB4 = 1
       END IF

    END IF
    ! Complex logic for simple additional criteria: because 0.0D0 is void
    siTstB = 0
   IF     ( siTstB1==0 .AND. siTstB2==0 .AND. siTstB3==0 .AND. siTstB4==0 )THEN
      siTstB = 1
   ELSE IF( siTstB1/=0 .AND. siTstB2==0 .AND. siTstB3==0 .AND. siTstB4==0 )THEN
      IF( siTstB1==1 ) siTstB = 1
   ELSE IF( siTstB1==0 .AND. siTstB2/=0 .AND. siTstB3==0 .AND. siTstB4==0 )THEN
      IF( siTstB2==1 ) siTstB = 1
   ELSE IF( siTstB1/=0 .AND. siTstB2/=0 .AND. siTstB3==0 .AND. siTstB4==0 )THEN
      IF( siTstB1==1 .OR. siTstB2==1 ) siTstB = 1

   ELSE IF( siTstB1==0 .AND. siTstB2==0 .AND. siTstB3/=0 .AND. siTstB4==0 )THEN
      IF( siTstB3==1 ) siTstB = 1
   ELSE IF( siTstB1/=0 .AND. siTstB2==0 .AND. siTstB3/=0 .AND. siTstB4==0 )THEN
      IF( siTstB3==1 .OR. siTstB1==1 ) siTstB = 1
   ELSE IF( siTstB1==0 .AND. siTstB2/=0 .AND. siTstB3/=0 .AND. siTstB4==0 )THEN
      IF( siTstB3==1 .OR. siTstB2==1 ) siTstB = 1
   ELSE IF( siTstB1/=0 .AND. siTstB2/=0 .AND. siTstB3/=0 .AND. siTstB4==0 )THEN
      IF( siTstB3==1 .OR. siTstB1==1 .OR. siTstB2==1 ) siTstB = 1

   ELSE IF( siTstB1==0 .AND. siTstB2==0 .AND. siTstB3==0 .AND. siTstB4/=0 )THEN
      IF( siTstB4==1 ) siTstB = 1
   ELSE IF( siTstB1/=0 .AND. siTstB2==0 .AND. siTstB3==0 .AND. siTstB4/=0 )THEN
      IF( siTstB4==1 .OR. siTstB1==1 ) siTstB = 1
   ELSE IF( siTstB1==0 .AND. siTstB2/=0 .AND. siTstB3==0 .AND. siTstB4/=0 )THEN
      IF( siTstB4==1 .OR. siTstB2==1 ) siTstB = 1
   ELSE IF( siTstB1/=0 .AND. siTstB2/=0 .AND. siTstB3==0 .AND. siTstB4/=0 )THEN
      IF( siTstB4==1 .OR. siTstB1==1 .OR. siTstB2==1 ) siTstB = 1

   ELSE IF( siTstB1==0 .AND. siTstB2==0 .AND. siTstB3/=0 .AND. siTstB4/=0 )THEN
      IF( siTstB3==1 .OR. siTstB4==1 ) siTstB = 1
   ELSE IF( siTstB1/=0 .AND. siTstB2==0 .AND. siTstB3/=0 .AND. siTstB4/=0 )THEN
      IF( siTstB3==1 .OR. siTstB4==1 .OR. siTstB1==1 ) siTstB = 1
   ELSE IF( siTstB1==0 .AND. siTstB2/=0 .AND. siTstB3/=0 .AND. siTstB4/=0 )THEN
      IF( siTstB3==1 .OR. siTstB4==1 .OR. siTstB2==1 ) siTstB = 1
   ELSE IF( siTstB1/=0 .AND. siTstB2/=0 .AND. siTstB3/=0 .AND. siTstB4/=0 )THEN
      IF( siTstB3==1 .OR. siTstB4==1 .OR. siTstB1==1 .OR. siTstB2==1 )siTstB=1
   END IF

    ! Combination of overall test and additional criteria
    IF( siTstA == 1 .AND. siTstB == 1 .OR. evnt%flag == flagSet )THEN
       siTst = 1
    ELSE
       siTst = 0
    END IF

    ! Assignment
    evnt%siTst = siTst

    RETURN

  END SUBROUTINE sCoreTstAddCrit

! -------------------------------------------------------------------------
! Get the functional model value valMod for the given epoch mjd
! -------------------------------------------------------------------------
  SUBROUTINE sCoreGetValModMjd( nVal, evnt, nEvnt, mjd, valMod )
    ! Subroutine Arguments
    ! --------------------
    INTEGER(i4b),INTENT(IN)                         :: nVal
    TYPE(t_evnt),DIMENSION(:),POINTER               :: evnt
    INTEGER(i4b) ,INTENT(IN)                        :: nEvnt
    REAL(r8b),INTENT(IN)                            :: mjd
    REAL(r8b),DIMENSION(:),POINTER                  :: valMod
    ! Local variables
    ! ---------------
    INTEGER(i4b)                                    :: iVal
    INTEGER(i4b)                                    :: iEvnt
    INTEGER(i4b)                                    :: jEvnt
    INTEGER(i4b)                                    :: seen
    ! Main
    ! ----
    DO iVal = 1,nVal
       ! Reset value
       valMod(iVal) = 0.0D0
       ! Add contribution of discontinuities
       DO iEvnt = 1,nEvnt
          IF( evnt(iEvnt)%type /= typeJump )CYCLE
          IF( evnt(iEvnt)%mjd > mjd )CYCLE
          valMod(iVal) = valMod(iVal) + evnt(iEvnt)%par(iVal)
       END DO
       ! Add contribution of rates
       DO iEvnt = 1,nEvnt
          IF( evnt(iEvnt)%type /= typeRate )CYCLE
          IF( evnt(iEvnt)%mjd > mjd )CYCLE
          valMod(iVal) = valMod(iVal) + &
               evnt(iEvnt)%par(iVal)*(mjd-evnt(iEvnt)%mjd)
       END DO
       ! Add contribution of periodic functions
       DO iEvnt = 1,nEvnt
          IF( evnt(iEvnt)%type /= typePeri )CYCLE
          ! Filter
          seen = 0
          DO jEvnt = 1,nEvnt
             IF( ( evnt(jEvnt)%type == -typePeri ) .AND. &
                 ( mjd .isIn. evnt(jEvnt)%timint ) )THEN
                seen = 1
                EXIT
             END IF
          END DO
          IF( seen == 1 )CYCLE
          ! Val
          valMod(iVal) = valMod(iVal) + ( &
               evnt(iEvnt)%ppar(iVal,1)*DCOS(evnt(iEvnt)%omega*mjd) + &
               evnt(iEvnt)%ppar(iVal,2)*DSIN(evnt(iEvnt)%omega*mjd) )
       END DO
    END DO
    RETURN
END SUBROUTINE sCoreGetValModMjd

! -------------------------------------------------------------------------
! Write line for element monitoring in verbose mode
! -------------------------------------------------------------------------
  SUBROUTINE sCoreWriteElemLine( evnt, label, m0 )
    USE d_const,   ONLY: pi
    ! Subroutine Arguments
    ! --------------------
    TYPE(t_evnt)                              :: evnt
    CHARACTER(LEN=9),INTENT(IN)               :: label
    REAL(r8b),INTENT(IN)                      :: m0
    ! Local variables
    ! ---------------
    CHARACTER(LEN=12)              :: evntTxt
    REAL(r8b)                      :: evntOmega
    REAL(r8b)                      :: stTst
    ! Main
    ! ----
    evntTxt = ''
    IF     ( evnt%type == typeJump )THEN
       evntTxt = 'DISC: '
    ELSE IF( evnt%type == typeVelo )THEN
       evntTxt = 'VELO: '
    ELSE IF( evnt%type == typeOutl )THEN
       evntTxt = 'OUTL: '
    ELSE IF( evnt%type == typePeri )THEN
       evntTxt = 'PERI: '
    ELSE IF( evnt%type == typeRate )THEN
       evntTxt = 'RATE: '
    END IF
    evntOmega = evnt%mjd
    IF( evnt%type == typePeri )THEN
       evntOmega = 2*pi/evnt%omega
    END IF
    IF( evnt%stTst > 9999.98 )THEN
       stTst = 9999.99
    ELSE
       stTst = evnt%stTst
    END IF
    WRITE(lfnprt,'(A,A9,A,A6,F10.4,A,I1,&
         &2(A,E11.5),A,F7.2,A,E13.7,A,I1,A,E11.5)')&
         ' | ', label, ' ', evntTxt, evntOmega, &
         ',Info=', evnt%info, &
         ', val:', evnt%val, &
         ' dVal:', evnt%dVal, &
         ', stTst=', stTst, &
         ', vlTst=', evnt%vlTst, &
         ', siTst=', evnt%siTst, &
         ', m0=', m0
    RETURN
  END SUBROUTINE sCoreWriteElemLine

! -------------------------------------------------------------------------
! Number to Type
! -------------------------------------------------------------------------
  FUNCTION sCoreNumToType( num )
    ! Subroutine Arguments
    ! --------------------
    INTEGER(i4b),INTENT(IN)                   :: num
    ! Local variables
    ! ---------------
    CHARACTER(LEN=4)                          :: sCoreNumToType
    ! Main
    ! ----
    IF     ( ABS(num) == typeJump )THEN
       sCoreNumToType = 'DISC'
    ELSE IF( ABS(num) == typeRate )THEN
       sCoreNumToType = 'RATE'
    ELSE IF( ABS(num) == typeVelo )THEN
       sCoreNumToType = 'VELO'
    ELSE IF( ABS(num) == typeOutl )THEN
       sCoreNumToType = 'OUTL'
    ELSE IF( ABS(num) == typePeri )THEN
       sCoreNumToType = 'PERI'
    ELSE
       sCoreNumToType = 'NONE'
    END IF
    RETURN
  END FUNCTION sCoreNumToType

! -------------------------------------------------------------------------
! Type to Number
! -------------------------------------------------------------------------
  FUNCTION sCoreTypeToNum( type )
    ! Subroutine Arguments
    ! --------------------
    CHARACTER(LEN=4),INTENT(IN)               :: type
    ! Local variables
    ! ---------------
    INTEGER(i4b)                              :: sCoreTypeToNum
    ! Main
    ! ----
    IF     ( type == 'DISC' )THEN
       sCoreTypeToNum = typeJump
    ELSE IF( type == 'RATE' )THEN
       sCoreTypeToNum = typeRate
    ELSE IF( type == 'VELO' )THEN
       sCoreTypeToNum = typeVelo
    ELSE IF( type == 'OUTL' )THEN
       sCoreTypeToNum = typeOutl
    ELSE IF( type == 'PERI' )THEN
       sCoreTypeToNum = typePeri
    ELSE
       sCoreTypeToNum = typeNone
    END IF
    RETURN
  END FUNCTION sCoreTypeToNum

! -------------------------------------------------------------------------
! Number to Flag
! -------------------------------------------------------------------------
  FUNCTION sCoreNumToFlag( num )
    ! Subroutine Arguments
    ! --------------------
    INTEGER(i4b),INTENT(IN)                   :: num
    ! Local variables
    ! ---------------
    CHARACTER(LEN=3)                          :: sCoreNumToFlag
    ! Main
    ! ----
    IF     ( num == flagTst )THEN
       sCoreNumToFlag = 'TST'
    ELSE IF( num == flagSet )THEN
       sCoreNumToFlag = 'SET'
    ELSE IF( num == flagEst )THEN
       sCoreNumToFlag = 'EST'
    ELSE IF( num == flagNot )THEN
       sCoreNumToFlag = 'NOT'
    ELSE
       sCoreNumToFlag = 'BOH'
    END IF
    RETURN
  END FUNCTION sCoreNumToFlag

! -------------------------------------------------------------------------
! Flag to Number
! -------------------------------------------------------------------------
  FUNCTION sCoreFlagToNum( flag )
    ! Subroutine Arguments
    ! --------------------
    CHARACTER(LEN=3),INTENT(IN)               :: flag
    ! Local variables
    ! ---------------
    INTEGER(i4b)                              :: sCoreFlagToNum
    ! Main
    ! ----
    IF     ( flag == 'TST' )THEN
       sCoreFlagToNum = flagTst
    ELSE IF( flag == 'SET' )THEN
       sCoreFlagToNum = flagSet
    ELSE IF( flag == 'EST' )THEN
       sCoreFlagToNum = flagEst
    ELSE IF( flag == 'NOT' )THEN
       sCoreFlagToNum = flagNot
    ELSE
       sCoreFlagToNum = flagBoh
    END IF
    RETURN
  END FUNCTION sCoreFlagToNum

END MODULE p_fodits
