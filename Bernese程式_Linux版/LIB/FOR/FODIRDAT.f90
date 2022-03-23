MODULE s_FODIRDAT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodirdat(opt,sCore,part)

! -------------------------------------------------------------------------
! Purpose:    Read input time series (TS) from:
!             * ADDNEQ2 output files. Procedure:
!               - read TS from PLT in North,East,Up (NEU) to obtain NEU(PLT),
!               - add CRD and VEL if present 'XYZ(PLT)+CRD+VEL(t-t0), and
!                 Least Squares (LSA) to obtain estimates of NEU(PLT), CRD, VEL
!             * CRDs files. Procedure:
!               - read CRDs coordinate files in Bernese format and
!               - LSA to obtain estimates of NEU(PLT), CRD, VEL
!             All read TS information is stored into the sCore struct
!
! Author:     L. Ostini
!
! Created:    14-Aug-2008
!
! Changes:    14-Aug-2008 LO: Created this file
!             02-Oct-2008 LO: First revision
!             09-Oct-2008 LO: Third revision
!             05-Dec-2008 LO: Fourth revisio: velocity changes allowed
!             13-Jan-2009 RD: Use '' as EOF-identifier in NEXTLINE
!             11-Feb-2009 LO: Fifth revision: major changes
!             17-Jun-2009 LO: EXT file format introduced
!             15-Jul-2009 LO: Sampling rate information added
!             14-Aug-2009 LO: Getco3 changed
!             18-Aug-2009 LO: Residuals reinitialized if DOF < 1
!             19-Aug-2009 LO: MENUAUX variables included
!             25-Sep-2009 LO: Changes for F90 consistency
!             18-Nov-2009 LO: Major changes for consistency
!             21-Dec-2009 LO: FFT removed and several changes apported
!             05-Feb-2010 LO: Time series error corrected for the PLT case
!             02-Mar-2010 LO: Major changes do to elimination of FODISUBR
!             07-Apr-2010 LO: Major changes do to algorithm and output file
!             16-Jun-2010 LO: Time series diveded by component
!             26-Aug-2010 LO: Architectural changes
!             03-Jan-2010 LO: INTENT(INOUT) removed and bug fixed
!             24-May-2011 LO: New update of ADDNEQ2 meta-data
!             19-Jul-2011 LO: Test datum defintion added
!             31-Aug-2011 LO: typeNot intervals fixed + 1s sampling
!             19-Sep-2012 RD: Use P_FODITS with ONLY, remove unused modules
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used Modules
! ------------
! structures, variables:
  USE m_bern,    ONLY: i4b, r8b, linelength, shortLineLength,   &
                       staNameLength, filenameLength, lfn001, lfnErr, lfnPrt
  USE m_maxdim,  ONLY: maxsta
  USE d_const,   ONLY: pi
  USE d_datum,   ONLY: datum
  USE p_fodits,  ONLY: t_opt, t_sCore, t_lsa, &
                       INPRES, NMAXRENAMING, INPCRD

! operator, methods:
!  USE d_debug,   ONLY: debug_entry, debug_exit

! subroutines, functions:
  USE f_nextline
  USE s_exitrc
  USE s_alcerr
  USE s_eccell
  USE s_ellecc
  USE s_fodislsa
  USE s_getco3
  USE s_getdat
  USE s_opnerr
  USE s_opnfil
  USE s_timst2
  USE s_xyzell

! no implicit
  IMPLICIT NONE

! subroutine name
  CHARACTER(LEN=shortLineLength), PARAMETER    :: srName = 'fodirdat'


! List of Arguments
! -----------------
! input:
  TYPE(t_opt)                    :: opt          ! Option structure
  TYPE(t_sCore)                  :: sCore        ! Gen TS structure
  INTEGER(i4b),INTENT(IN)        :: part         ! Part:
                                                 ! 1 = read PLT or CRDs
                                                 ! 2 = convert TS XYZ to NEU

! input/output:

! output:


! Local Types
! -----------


! Local Parameters
! ----------------


! Local Variables
! ---------------
  TYPE(t_lsa)                    :: lsa

  CHARACTER(LEN=20)              :: datstr
  CHARACTER(LEN=lineLength)      :: line
  CHARACTER(LEN=staNameLength)   :: staRd
  CHARACTER(LEN=staNameLength)   :: staTmp
  CHARACTER(LEN=1),DIMENSION(:), &
                   POINTER       :: xStaFlg
  CHARACTER(LEN=4),DIMENSION(:), &
                   POINTER       :: xStaPlate
  CHARACTER(LEN=fileNameLength)  :: CrdsFileNameTmp
  CHARACTER(LEN=staNameLength), &
    DIMENSION(:,:), ALLOCATABLE  :: staNamTmp
  CHARACTER(LEN=staNameLength), &
    DIMENSION(:,:), ALLOCATABLE  :: staRenamTmp
  CHARACTER(LEN=staNameLength), &
    DIMENSION(:), ALLOCATABLE    :: staNamTmpSingle
  CHARACTER(LEN=staNameLength),  &
    DIMENSION(:),POINTER         :: staNamIn
  CHARACTER(LEN=20)              :: tsRefEpoch
  CHARACTER(LEN=20)              :: mjdBeginStr
  CHARACTER(LEN=20)              :: mjdEndStr
  CHARACTER(LEN=20)              :: mjdRefStr
  CHARACTER(LEN=40)              :: varCovInfoText
  CHARACTER(LEN=staNameLength),  &
    DIMENSION(:),POINTER         :: staNamInCrd
  CHARACTER(LEN=3)               :: tmpTxt
  CHARACTER(LEN=10)              :: begMjdRef
  CHARACTER(LEN=10)              :: endMjdRef
  CHARACTER(LEN=3)               :: inCrdFileAddPltTxt
  CHARACTER(LEN=3)               :: inVelFileAddPltTxt

  INTEGER(i4b)                   :: ios,iac
  INTEGER(i4b)                   :: iFil
  INTEGER(i4b)                   :: iVal
  INTEGER(i4b)                   :: iSta
  INTEGER(i4b)                   :: jSta
  INTEGER(i4b)                   :: kSta
  INTEGER(i4b)                   :: iMjd
  INTEGER(i4b)                   :: jMjd
  INTEGER(i4b)                   :: kMjd
  INTEGER(i4b)                   :: indM
  INTEGER(i4b)                   :: nSta
  INTEGER(i4b)                   :: nMjd
  INTEGER(i4b)                   :: nVal
  INTEGER(i4b)                   :: nStaSta
  INTEGER(i4b)                   :: iStaSta
  INTEGER(i4b)                   :: iRefSta
  INTEGER(i4b)                   :: iTer
  INTEGER(i4b)                   :: iOfs
  INTEGER(i4b)                   :: nMjdRd
  INTEGER(i4b)                   :: crdRd
  INTEGER(i4b)                   :: nNum
  INTEGER(i4b)                   :: nStaInCrd
  INTEGER(i4b), DIMENSION(:),    &
                POINTER          :: staNumIn
  INTEGER(i4b),                  &
    DIMENSION(:), ALLOCATABLE    :: nStaNamTmp
  INTEGER(i4b),                  &
    DIMENSION(:), ALLOCATABLE    :: staMjdTmp
  INTEGER(i4b)                   :: seen
  INTEGER(i4b)                   :: nStaIn
  INTEGER(i4b)                   :: nLine
  INTEGER(i4b)                   :: iLine
  INTEGER(i4b)                   :: iPeri
  INTEGER(i4b)                   :: nHsmpl
  INTEGER(i4b)                   :: iHsmpl
  INTEGER(i4b)                   :: maxHsmpl
  INTEGER(i4b)                   :: firstCrd
  INTEGER(i4b)                   :: reconstrCrd
  INTEGER(i4b)                   :: reconstrVel

  REAL(r8b)                      :: deltaMjd
  REAL(r8b)                      :: resRd
  REAL(r8b)                      :: mjdRd
  REAL(r8b)                      :: mjdTmp
  REAL(r8b), DIMENSION(:),       &
             ALLOCATABLE         :: CrdFileEpoch
  REAL(r8b)                      :: CrdFileEpochTmp
  REAL(r8b), &
    DIMENSION(:,:,:), ALLOCATABLE:: staMjdInt
  REAL(r8b)                      :: tMjdInCrd
  REAL(r8b), DIMENSION(:,:),     &
             POINTER             :: xStaIn
  REAL(r8b), DIMENSION(3)        :: resNeu
  REAL(r8b), DIMENSION(3)        :: resXyz
  REAL(r8b)                      :: tmpMjd
  REAL(r8b), DIMENSION(:),       &
             ALLOCATABLE         :: tmpVal
  REAL(r8b), DIMENSION(:),       &
             ALLOCATABLE         :: tmpdVal
  REAL(r8b), DIMENSION(3,3)      :: tmpVci
  REAL(r8b)                      :: m0
  REAL(r8b),DIMENSION(3)         :: vciRms
  REAL(r8b),DIMENSION(3)         :: vciCor
  REAL(r8b)                      :: minPeri
  REAL(r8b)                      :: maxPeri
  REAL(r8b)                      :: period
  REAL(r8b)                      :: refMjd
  REAL(r8b)                      :: halfDeltaMjd
  REAL(r8b)                      :: modDeltaMjd

  TYPE t_hist_smpl
     REAL(r8b)                   :: deltaMjd
     INTEGER(i4b)                :: nSmpl
  END TYPE t_hist_smpl

  TYPE(t_hist_smpl),DIMENSION(9999) :: hSmpl

! Call debug routine
! ------------------
!  CALL debug_entry(srName)

! Initialization of all variables
! -------------------------------
  nVal = 0
  nNum = opt%inTruncStaName
  ios = 0
  tMjdInCrd = 0
  tsRefEpoch = ''
  staRd = ''
  resRd  = 0.0D0
  nStaInCrd = 0
  nMjdRd = 0
  nStaIn = 0
  mjdEndStr = ''
  mjdBeginStr = ''
  mjdRefStr = ''
  mjdRd = 0.0D0
  mjdTmp = 0.0D0
  NULLIFY(xStaIn)
  NULLIFY(xStaPlate)
  NULLIFY(xStaFlg)
  NULLIFY(staNamInCrd)
  NULLIFY(staNamIn)
  NULLIFY(staNumIn)
  nHsmpl = 0
  hSmpl(:)%deltaMjd = 0.0D0
  hSmpl(:)%nSmpl = 0

  ! Set default datum name (used in case where only a PLT file is read)
  sCore%datumName = 'IGS08'

  ! Get datum information
  datum%name = sCore%datumName
  CALL getdat(datum%name, datum%aell, datum%bell, datum%dxell, &
       datum%drell, datum%scell)

  ! Creation of synthetic time series - hidden option to validate FODITS
  IF( opt%selInpTsType == inpRes   .AND. LEN_TRIM(opt%inPltFile) == 0 .AND. &
      LEN_TRIM(opt%inCrdFile) == 0 .AND. LEN_TRIM(opt%inEvlFile) /= 0 )THEN
     !  CALL debug_entry(srName)
     RETURN
  END IF

  ! Filter Coordiante and Velocity Files
  IF( opt%selInpTsType == inpRes   .AND. LEN_TRIM(opt%inPltFile) == 0 .AND. &
      LEN_TRIM(opt%inCrdFile) /= 0 .AND. LEN_TRIM(opt%inVelFile) /= 0 )THEN
     nVal = 3
  END IF

  ! Check reconstruction CRD
  reconstrCrd = 0
  IF( LEN_TRIM(opt%inCrdFile) /= 0 .AND. opt%inCrdFileAddPlt == 1 )THEN
     reconstrCrd = 1
  END IF

  ! Check reconstruction VEL
  reconstrVel = 0
  IF( LEN_TRIM(opt%inVelFile) /= 0 .AND. opt%inVelFileAddPlt == 1 )THEN
     reconstrVel = 1
  END IF

  ! Begin Part 1 = read PLT or CRDs
  ! ===============================
  ! ===============================
  IF( part == 1 )THEN

  MENUAUX_LOOP: DO iTer = 1,1

  ! Read PLT+(CRD+VEL) as input Time Series (TS)
  ! ============================================
  IF ( opt%selInpTsType == inpRes )THEN

     ! Read PLT file
     IF( LEN_TRIM(opt%inPltFile) /= 0 )THEN

        ! Open PLT file
        CALL opnfil(lfn001,opt%inPltFile, &
             'UNKNOWN','FORMATTED','READONLY',' ',ios)
        CALL opnerr(lfnerr,lfn001,ios,opt%inPltFile,srName)

        ! Count the number of lines and number of stations
        nLine = 0
        nSta = 0
        staTmp = ''
        DO WHILE ( 1 == 1 )
           line = nextline(lfn001,0)
           IF (line == '')EXIT
           CALL fodirdat_pltCrdLine(line,staRd,nMjdRd,crdRd,resRd,mjdRd)
           IF( staRd /= staTmp )THEN
              nSta = nSta + 1
              staTmp = staRd
           END IF
           nLine = nLine + 1
        END DO

        ! Allocate variables
        ALLOCATE(nStaNamTmp(nSta),stat=iac)
        CALL alcerr(iac,'nStaNamTmp',(/nSta/),srName)
        ALLOCATE(staNamTmp(nSta,nMaxRenaming),stat=iac)
        CALL alcerr(iac,'staNamTmp',(/nSta,nMaxRenaming/),srName)
        ALLOCATE(staRenamTmp(nSta,nMaxRenaming),stat=iac)
        CALL alcerr(iac,'staRenamTmp',(/nSta,nMaxRenaming/),srName)
        ALLOCATE(staNamTmpSingle(nSta),stat=iac)
        CALL alcerr(iac,'staNamTmpSingle',(/nSta/),srName)
        ALLOCATE(staMjdTmp(nSta),stat=iac)
        CALL alcerr(iac,'staMjdTmp',(/nSta/),srName)
        ALLOCATE(staMjdInt(nSta,nMaxRenaming,2),stat=iac)
        CALL alcerr(iac,'staMjdInt',(/nSta,nMaxRenaming,2/),srName)

        ! Determine the number of crd-components - with the assumption that
        ! epochs have the same number of components
        REWIND(lfn001)
        iLine = 0
        nVal = 0
        firstCrd = 0
        DO WHILE ( 1 == 1 )
           iLine = iLine + 1
           IF ( iLine > nLine )EXIT
           line = nextline(lfn001,0)
           CALL fodirdat_pltCrdLine(line,staRd,nMjdRd,crdRd,resRd,mjdRd)
           IF( iLine == 1 )THEN
              firstCrd = crdRd
           END IF
           IF( crdRd - firstCrd + 1 > nVal )THEN
              nVal = nVal + 1
           ELSE
              EXIT
           END IF
        END DO
        ! Store number of components
        sCore%nVal = nVal

        ! Rewind
        REWIND(lfn001)

        ! Determine the number of epochs (nMjd) for each station iSta
        ! Determine the time intervals of all renamed stations
        iLine = 0
        nSta = 0
        staTmp = ''
        DO WHILE ( 1 == 1 )
           iLine = iLine + 1
           IF ( iLine > nLine )THEN
              staMjdInt(nSta,nStaNamTmp(nSta),2) = mjdTmp
              EXIT
           END IF
           line = nextline(lfn001,0)
           CALL fodirdat_pltCrdLine(line,staRd,nMjdRd,crdRd,resRd,mjdRd)
           iSta = iSta + 1
           IF( staRd(1:nNum) /= staTmp(1:nNum) )THEN
              IF( nSta > 0 )THEN
                 staMjdInt(nSta,nStaNamTmp(nSta),2) = mjdTmp
              END IF
              iSta = 1
              nSta = nSta + 1
              nStaNamTmp(nSta) = 1
              staTmp = staRd
              staRenamTmp(nSta,1) = staRd(nNum+1:16)
              staMjdInt(nSta,1,1) = mjdRd
           ELSE IF( staRd(nNum+1:16) /= staTmp(nNum+1:16) )THEN
              staMjdInt(nSta,nStaNamTmp(nSta),2) = mjdTmp
              nStaNamTmp(nSta) = nStaNamTmp(nSta) + 1
              staTmp = staRd
              staRenamTmp(nSta,nStaNamTmp(nSta)) = staRd(nNum+1:16)
              staMjdInt(nSta,nStaNamTmp(nSta),1) = mjdRd
           END IF
           staMjdTmp(nSta) = iSta
           staNamTmp(nSta,nStaNamTmp(nSta)) = staRd
           mjdTmp = mjdRd
        END DO

        ! Allocate %sta
        sCore%nSta = nSta
        ALLOCATE(sCore%sta(nSta),stat=iac)
        CALL alcerr(iac, 'sCore%sta(nSta)', (/nSta/), srName)
        ! Initializations
        DO iSta = 1,sCore%nSta
           sCore%sta(iSta)%name = staNamTmp(iSta,1)
           sCore%sta(iSta)%name(nNum+1:16) = '  '
           sCore%sta(iSta)%nStaSta = nStaNamTmp(iSta)
           sCore%sta(iSta)%ts%nMjd = staMjdTmp(iSta)/nVal
           sCore%sta(iSta)%ts%nVal = nVal
           sCore%sta(iSta)%iRefSta = 1
        END DO

        ! Be sure that nVal > 3 (for this FODITS version)
        IF( nVal > 3 )THEN
           WRITE(lfnerr,'(/,A,/)') &
                ' *** SR FODIRDAT: Max 3 components in PLT-file.'
           CALL exitrc(2)
        END IF

        ! In case MENUAUX calls this subroutine EXIT at this point
        IF( opt%rdOnlyStaNames /= 0 )EXIT MENUAUX_LOOP

        ! Allocation of memory
        DO iSta = 1,sCore%nSta
           nMjd = sCore%sta(iSta)%ts%nMjd
           nVal = sCore%sta(iSta)%ts%nVal
           ALLOCATE(sCore%sta(iSta)%ts%mjd(nMjd),stat=iac)
           CALL alcerr(iac, 'sCore%sta(iSta)%ts%mjd',(/nMjd/),srName)
           ALLOCATE(sCore%sta(iSta)%ts%val(nMjd,nVal),stat=iac)
           CALL alcerr(iac, 'sCore%sta(iSta)%ts%val',(/nMjd,nVal/),srName)
           nStaSta = sCore%sta(iSta)%nStaSta
           ALLOCATE(sCore%sta(iSta)%staSta(nStaSta),stat=iac)
           CALL alcerr(iac, 'sCore%sta(iSta)%staSta',(/nStaSta/),srName)
           IF     ( opt%inPltFileVciEna == 1 )THEN
              ALLOCATE(sCore%sta(iSta)%ts%dVal(nMjd,nVal),stat=iac)
              CALL alcerr(iac, 'sCore%sta(iSta)%ts%dVal',(/nMjd,nVal/),srName)
           ELSE IF( opt%inPltFileVciEna == 2 )THEN
              ALLOCATE(sCore%sta(iSta)%ts%vci(nMjd,3,3),stat=iac)
              CALL alcerr(iac, 'sCore%sta(iSta)%ts%vci',(/nMjd,3,3/),srName)
           END IF
           IF( opt%modPreDatTest == 1 )THEN
              ALLOCATE(sCore%sta(iSta)%ts%dDef(nMjd,nVal),stat=iac)
              CALL alcerr(iac, 'sCore%sta(iSta)%ts%dDef',(/nMjd,nVal/),srName)
              ALLOCATE(sCore%sta(iSta)%ts%dDefRej(nMjd),stat=iac)
              CALL alcerr(iac, 'sCore%sta(iSta)%ts%dDefRej',(/nMjd/),srName)
           END IF
        END DO

        ! Initializations
        DO iSta = 1,sCore%nSta
           sCore%sta(iSta)%outCrdXyz(:)  = 0.0D0
           sCore%sta(iSta)%outVelXyz(:)  = 0.0D0
           sCore%sta(iSta)%ts%mjd(:)     = 0.0D0
           sCore%sta(iSta)%ts%val(:,:)   = 0.0D0
           IF     ( opt%inPltFileVciEna == 1 )THEN
              sCore%sta(iSta)%ts%dVal(:,:) = 1.0D0
           ELSE IF( opt%inPltFileVciEna == 2 )THEN
              sCore%sta(iSta)%ts%vci(:,:,:) = 0.0D0
              DO iVal = 1,nVal
                 sCore%sta(iSta)%ts%vci(:,iVal,iVal) = 1.0D0
              END DO
           END IF
           DO iStaSta = 1,sCore%sta(iSta)%nStaSta
              sCore%sta(iSta)%staSta(iStaSta)%rename = &
                   staRenamTmp(iSta,iStaSta)
              sCore%sta(iSta)%staSta(iStaSta)%timint%t(1) = &
                   staMjdInt(iSta,iStaSta,1)
              sCore%sta(iSta)%staSta(iStaSta)%timint%t(2) = &
                   staMjdInt(iSta,iStaSta,2)
              sCore%sta(iSta)%staSta(iStaSta)%inCrdXyz(:) = 0.0D0
              sCore%sta(iSta)%staSta(iStaSta)%inCrdNeu(:) = 0.0D0
              sCore%sta(iSta)%staSta(iStaSta)%inVelXyz(:) = 0.0D0
              sCore%sta(iSta)%staSta(iStaSta)%inVelNeu(:) = 0.0D0
              sCore%sta(iSta)%staSta(iStaSta)%inCrdFlg = ''
              sCore%sta(iSta)%staSta(iStaSta)%inVelFlg = ''
           END DO
           sCore%sta(iSta)%upd%staStatusRef = 0
           IF( opt%modPreDatTest == 1 )THEN
              sCore%sta(iSta)%ts%dDef(:,:)  = 0.0D0
              sCore%sta(iSta)%ts%dDefRej(:) = 0
           END IF
        END DO

        ! Rewind
        REWIND(lfn001)

        ! Read all lines of the PLT file
        iSta = 1
        iMjd = 1
        iVal = 0
        mjdTmp = 0.0D0
        DO iLine = 1,nLine
           line = nextline(lfn001,0)
           ! Filter
           CALL fodirdat_pltCrdLine(line,staRd,nMjdRd,crdRd,resRd,mjdRd)
           mjdTmp = mjdRd
           ! Increment
           iVal = iVal + 1
           IF( iVal > nVal )THEN
              iMjd = iMjd + 1
              iVal = 1
           END IF
           IF( iMjd > sCore%sta(iSta)%ts%nMjd )THEN
              iSta = iSta + 1
              iMjd = 1
           END IF
           IF( iSta > sCore%nSta )EXIT
           ! Read line
           IF     ( opt%inPltFileVciEna == 0 )THEN
              CALL fodirdat_pltCrdLine(line,staRd,nMjdRd,&
                   crdRd,resRd,mjdRd)
           ELSE IF( opt%inPltFileVciEna == 1 )THEN
              CALL fodirdat_pltCrdLine(line,staRd,nMjdRd,&
                   crdRd,resRd,mjdRd, &
                   stdDevInfo = sCore%sta(iSta)%ts%dVal(iMjd,iVal) )
           ELSE IF( opt%inPltFileVciEna == 2 )THEN
              crdRd = iVal
              CALL fodirdat_pltCrdLine(line,staRd,nMjdRd,&
                   crdRd,resRd,mjdRd, &
                   varCovInfo = sCore%sta(iSta)%ts%vci(iMjd,:,iVal) )
           END IF
           sCore%sta(iSta)%ts%mjd(iMjd)      = mjdRd
           sCore%sta(iSta)%ts%val(iMjd,iVal) = resRd
           ! Reconstruction of Var-Cov matrix according to DIFCOMP.f90
           IF( opt%inPltFileVciEna == 2 .AND. iVal == 3 .AND. nVal == 3 )THEN
              m0 = opt%inPltFileViM0
              vciRms(:) = sCore%sta(iSta)%ts%vci(iMjd,1,:)
              vciCor(:) = sCore%sta(iSta)%ts%vci(iMjd,2,:)
              sCore%sta(iSta)%ts%vci(iMjd,1,3) = &
                   vciCor(1)*vciRms(1)*vciRms(3)/m0**2
              sCore%sta(iSta)%ts%vci(iMjd,3,1) = &
                   sCore%sta(iSta)%ts%vci(iMjd,1,3)
              sCore%sta(iSta)%ts%vci(iMjd,2,3) = &
                   vciCor(2)*vciRms(2)*vciRms(3)/m0**2
              sCore%sta(iSta)%ts%vci(iMjd,3,2) = &
                   sCore%sta(iSta)%ts%vci(iMjd,2,3)
              sCore%sta(iSta)%ts%vci(iMjd,1,2) = &
                   vciCor(3)*vciRms(1)*vciRms(2)/m0**2
              sCore%sta(iSta)%ts%vci(iMjd,2,1) = &
                   sCore%sta(iSta)%ts%vci(iMjd,1,2)
              sCore%sta(iSta)%ts%vci(iMjd,1,1) = vciRms(1)**2/m0**2
              sCore%sta(iSta)%ts%vci(iMjd,2,2) = vciRms(2)**2/m0**2
              sCore%sta(iSta)%ts%vci(iMjd,3,3) = vciRms(3)**2/m0**2
           END IF
        END DO

        ! Sort time series after the epoch
        ALLOCATE(tmpVal(nVal),stat=iac)
        CALL alcerr(iac,'tmpVal',(/nVal/),srName)
        ALLOCATE(tmpdVal(nVal),stat=iac)
        CALL alcerr(iac,'tmpdVal',(/nVal/),srName)
        DO iSta = 1,nSta
           DO iMjd = 1,sCore%sta(iSta)%ts%nMjd-1
              kMjd = iMjd
              DO jMjd = iMjd+1,sCore%sta(iSta)%ts%nMjd
                 IF(  sCore%sta(iSta)%ts%mjd(jMjd) < &
                      sCore%sta(iSta)%ts%mjd(kMjd) )THEN
                    kMjd = jMjd
                 END IF
              END DO
              IF( kMjd /= iMjd )THEN
                 tmpMjd = sCore%sta(iSta)%ts%mjd(iMjd)
                 tmpVal(:) = sCore%sta(iSta)%ts%val(iMjd,:)
                 IF     ( opt%inPltFileVciEna == 1 )THEN
                    tmpdVal(:) = sCore%sta(iSta)%ts%dVal(iMjd,:)
                 ELSE IF( opt%inPltFileVciEna == 2 )THEN
                    tmpVci(:,:) = sCore%sta(iSta)%ts%vci(iMjd,:,:)
                 END IF

                 sCore%sta(iSta)%ts%mjd(iMjd) = &
                      sCore%sta(iSta)%ts%mjd(kMjd)
                 sCore%sta(iSta)%ts%val(iMjd,:) = &
                      sCore%sta(iSta)%ts%val(kMjd,:)
                 IF     ( opt%inPltFileVciEna == 1 )THEN
                    sCore%sta(iSta)%ts%dVal(iMjd,:) = &
                         sCore%sta(iSta)%ts%dVal(kMjd,:)
                 ELSE IF( opt%inPltFileVciEna == 2 )THEN
                    sCore%sta(iSta)%ts%vci(iMjd,:,:) = &
                         sCore%sta(iSta)%ts%vci(kMjd,:,:)
                 END IF

                 sCore%sta(iSta)%ts%mjd(kMjd) = tmpMjd
                 sCore%sta(iSta)%ts%val(kMjd,:) = tmpVal(:)
                 IF     ( opt%inPltFileVciEna == 1 )THEN
                    sCore%sta(iSta)%ts%dVal(kMjd,:) = tmpdVal(:)
                 ELSE IF( opt%inPltFileVciEna == 2 )THEN
                    sCore%sta(iSta)%ts%vci(kMjd,:,:) = tmpVci(:,:)
                 END IF
              END IF
           END DO
        END DO
        DEALLOCATE(tmpVal,stat=iac)
        DEALLOCATE(tmpdVal,stat=iac)

        ! Double of triple epochs due to multi-day solutions work-around
        DO iSta = 1,sCore%nSta
           iOfs = 0
           DO iMjd = 1,sCore%sta(iSta)%ts%nMjd-1
              IF( sCore%sta(iSta)%ts%mjd(iMjd-iOfs)   == &
                  sCore%sta(iSta)%ts%mjd(iMjd-iOfs+1)    )THEN
                 ! correct val
                 sCore%sta(iSta)%ts%val(iMjd-iOfs,:) = &
                      ( sCore%sta(iSta)%ts%val(iMjd-iOfs,:) + &
                        sCore%sta(iSta)%ts%val(iMjd-iOfs+1,:) )/2.0D0
                 ! correct dVal
                 IF     ( opt%inPltFileVciEna == 1 )THEN
                    DO iVal = 1,nVal
                       sCore%sta(iSta)%ts%dVal(iMjd-iOfs,iVal) = SQRT( &
                            sCore%sta(iSta)%ts%dVal(iMjd-iOfs,iVal)**2 + &
                            sCore%sta(iSta)%ts%dVal(iMjd-iOfs+1,iVal)**2 )
                    END DO
                 ! correct vci
                 ELSE IF( opt%inPltFileVciEna == 2 )THEN
                    sCore%sta(iSta)%ts%vci(iMjd-iOfs,:,:) = &
                         sCore%sta(iSta)%ts%vci(iMjd-iOfs,:,:) + &
                         sCore%sta(iSta)%ts%vci(iMjd-iOfs+1,:,:)
                 END IF
                 ! shift left all other observations
                 DO kMjd = iMjd-iOfs+1,sCore%sta(iSta)%ts%nMjd-iOfs-1
                    sCore%sta(iSta)%ts%mjd(kMjd) = &
                         sCore%sta(iSta)%ts%mjd(kMjd+1)
                    sCore%sta(iSta)%ts%val(kMjd,:) = &
                         sCore%sta(iSta)%ts%val(kMjd+1,:)
                    IF     ( opt%inPltFileVciEna == 1 )THEN
                       sCore%sta(iSta)%ts%dVal(kMjd,:) = &
                            sCore%sta(iSta)%ts%dVal(kMjd+1,:)
                    ELSE IF( opt%inPltFileVciEna == 2 )THEN
                       sCore%sta(iSta)%ts%vci(kMjd,:,:) = &
                            sCore%sta(iSta)%ts%vci(kMjd+1,:,:)
                    END IF
                 END DO
                 ! increment offset
                 iOfs = iOfs + 1
                 ! Exit
                 IF( iMjd == sCore%sta(iSta)%ts%nMjd-iOfs-1 )EXIT
              END IF
           END DO
           ! Correct the new number of epochs without the doubles or triples
           sCore%sta(iSta)%ts%nMjd = sCore%sta(iSta)%ts%nMjd - iOfs
           EXIT
        END DO

     ! End read PLT file
     END IF

     ! Read ADDNEQ2 solution CRD file (if present)
     sCore%inTimRefCrd = 0
     IF( LEN_TRIM(opt%inCrdFile) /= 0 .AND. nVal == 3 )THEN

        ! Read CRD file
        CALL getco3(opt%inCrdFile,1,(/'@'/),nStaIn,staNamIn,&
             staNum=staNumIn,datum=sCore%datumName,timcrd=tMjdInCrd,&
             staFlg=xStaFlg,xStat=xStaIn)
        sCore%inTimRefCrd = tMjdInCrd

        ! Read the information and store it into the sCore struct
        DO jSta = 1,nStaIn
           DO iSta = 1,sCore%nSta
              IF( sCore%sta(iSta)%name(1:nNum) == staNamIn(jSta)(1:nNum) )THEN
                 DO iStaSta = 1,sCore%sta(iSta)%nStaSta
                    IF(  sCore%sta(iSta)%staSta(iStaSta)%rename == &
                         staNamIn(jSta)(nNum+1:16) )THEN
                       sCore%sta(iSta)%staSta(iStaSta)%inCrdXyz(:) = &
                            xStaIn(:,jSta)
                       sCore%sta(iSta)%staSta(iStaSta)%inCrdFlg = &
                            xStaFlg(jSta)
                       IF( staNamIn(jSta)(nNum+1:16) == '  ' )THEN
                          ! Default case
                          sCore%sta(iSta)%iRefSta = iStaSta
                          sCore%sta(iSta)%outCrdXyz(:) = xStaIn(:,jSta)
                          sCore%sta(iSta)%outVelXyz(:) = 0.0D0
                       END IF
                       EXIT
                    END IF
                 END DO
              END IF
           END DO
        END DO

        ! Convert the CRD-XYZ to CRD-NEU
        DO iSta = 1,sCore%nSta
           IF( sCore%sta(iSta)%iRefSta == 0 )THEN
              WRITE(lfnerr,'(/,A,/)') &
                   ' *** SR FODIRDAT: Only renamed stations present in CRD.'
              CALL exitrc(2)
           END IF
           DO iStaSta = 1,sCore%sta(iSta)%nStaSta
              CALL xyzell(datum%aell,datum%bell, &
                   datum%dxell,datum%drell,datum%scell, &
                   sCore%sta(iSta)%staSta(iStaSta)%inCrdXyz(:) ,&
                   sCore%sta(iSta)%staSta(iStaSta)%inCrdNeu(:) )
           END DO
        END DO

     END IF

     ! Read the VEL file (if present, assuming that CRD has been read already)
     IF( nVal == 3 .AND. LEN_TRIM(opt%inVelFile) /= 0 .AND. &
                         LEN_TRIM(opt%inCrdFile) /= 0 )THEN

        ! Read VEL file
        CALL getco3(opt%inVelFile,1,(/'@'/),nStaIn,staNamIn,&
             staFlg=xStaFlg,xStat=xStaIn,plate=xStaPlate)

        ! Read the information and store it into the sCore struct
        DO jSta = 1,nStaIn
           DO iSta = 1,sCore%nSta
              IF( sCore%sta(iSta)%name(1:nNum) == staNamIn(jSta)(1:nNum) )THEN
                 DO iStaSta = 1,sCore%sta(iSta)%nStaSta
                    IF(  sCore%sta(iSta)%staSta(iStaSta)%rename == &
                         staNamIn(jSta)(nNum+1:16) )THEN
                       sCore%sta(iSta)%staSta(iStaSta)%inVelXyz(:) = &
                            xStaIn(:,jSta) / 365.25D0
                       sCore%sta(iSta)%staSta(iStaSta)%inVelFlg = &
                            xStaFlg(jSta)
                       sCore%sta(iSta)%staSta(iStaSta)%inVelPlate = &
                            xStaPlate(jSta)
                       EXIT
                    END IF
                 END DO
              END IF
           END DO
        END DO

        ! Convert the VEL-XYZ into VEL-NEU
        DO iSta = 1,sCore%nSta
           IF( sCore%sta(iSta)%iRefSta == 0 )THEN
              WRITE(lfnerr,'(/,A,/)') &
                   ' *** SR FODIRDAT: Only renamed stations present in VEL.'
              CALL exitrc(2)
           END IF
           DO iStaSta = 1,sCore%sta(iSta)%nStaSta
              CALL eccell(sCore%sta(iSta)%staSta(iStaSta)%inCrdNeu, &
                   sCore%sta(iSta)%staSta(iStaSta)%inVelXyz(:), &
                   sCore%sta(iSta)%staSta(iStaSta)%inVelNeu(:) )
           END DO
        END DO

     END IF

     ! Reconstruct residuals by applying CRD and VEL files
     IF( nVal == 3 .AND. ( reconstrCrd == 1 .OR. reconstrVel == 1 ) )THEN

        ! Loop over all stations
        DO iSta = 1,sCore%nSta

           ! Form ResXyz by applyin CRD (if present) and VEL (if present)
           DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
              DO iStaSta = 1,sCore%sta(iSta)%nStaSta
                 IF(  sCore%sta(iSta)%ts%mjd(iMjd) >= &
                      sCore%sta(iSta)%staSta(iStaSta)%timint%t(1) .AND. &
                      sCore%sta(iSta)%ts%mjd(iMjd) <= &
                      sCore%sta(iSta)%staSta(iStaSta)%timint%t(2) )THEN

                    ! ResNeu to ResXyz
                    resNeu(:) = sCore%sta(iSta)%ts%val(iMjd,:)
                    CALL ellecc(sCore%sta(iSta)%staSta(iStaSta)%inCrdNeu(:),&
                                resNeu(:), resXyz(:) )
                    sCore%sta(iSta)%ts%val(iMjd,:) = resXyz(:)

                    ! Add CRD: VAL=VAL+CRD
                    IF( reconstrCrd == 1 )THEN
                       sCore%sta(iSta)%ts%val(iMjd,:) = &
                            sCore%sta(iSta)%ts%val(iMjd,:) + &
                            sCore%sta(iSta)%staSta(iStaSta)%inCrdXyz(:)
                    END IF

                    ! Add VEL: VAL=VAL+CRD
                    IF( reconstrVel == 1 )THEN
                       sCore%sta(iSta)%ts%val(iMjd,:) = &
                            sCore%sta(iSta)%ts%val(iMjd,:) + &
                            sCore%sta(iSta)%staSta(iStaSta)%inVelXyz(:) * &
                            (sCore%sta(iSta)%ts%mjd(iMjd)-sCore%inTimRefCrd)
                    END IF

                    EXIT
                 END IF
              END DO
           END DO

        ! End Loop over all stations
        END DO

     ! End Reconstruct residuals by applying CRD and VEL files
     END IF

     CLOSE(lfn001)

  ! Read CRDs as input time series (TS)
  ! ===================================
  ELSE

     ! Define the number of coordinates
     nVal = 3

     ! Reconstruction CRD+VEL
     reconstrCrd = 1
     reconstrVel = 1

     ! Determine the number of stations present in the CRDs
     ALLOCATE(CrdFileEpoch(opt%nCrds),stat=iac)
     CALL alcerr(iac, 'CrdFileEpoch', (/opt%nCrds/), srName)
     ALLOCATE(staNamTmpSingle(maxsta),stat=iac)
     CALL alcerr(iac,'staNamTmpSingle',(/maxsta/),srName)
     nSta = 0
     DO iFil = 1,opt%nCrds
        CALL getco3(opt%CrdFileName(iFil),1,(/'#'/),nStaIn,staNamIn,&
                    timcrd=tMjdInCrd )
        CrdFileEpoch(iFil) = tMjdInCrd
        DO iSta = 1,nStaIn
           seen = 0
           DO jSta = 1,nSta
              IF( staNamIn(iSta)(1:nNum) == staNamTmpSingle(jSta)(1:nNum) )THEN
                 seen = 1
                 EXIT
              END IF
           END DO
           IF ( seen == 0 )THEN
              nSta = nSta + 1
              staNamTmpSingle(nSta) = ''
              staNamTmpSingle(nSta)(1:nNum) = staNamIn(iSta)(1:nNum)
           END IF
        END DO
     END DO
     sCore%nSta = nSta

     ! Store station names
     ALLOCATE(sCore%sta(sCore%nSta),stat=iac)
     CALL alcerr(iac, 'sCore%sta(sCore%nSta)', (/sCore%nSta/), srName)
     DO iSta = 1,nSta
        sCore%sta(iSta)%name = staNamTmpSingle(iSta)
     END DO

     ! Allocate variables
     ALLOCATE(nStaNamTmp(nSta),stat=iac)
     CALL alcerr(iac,'nStaNamTmp',(/nSta/),srName)
     ALLOCATE(staNamTmp(nSta,nMaxRenaming),stat=iac)
     CALL alcerr(iac,'staNamTmp',(/nSta,nMaxRenaming/),srName)
     ALLOCATE(staRenamTmp(nSta,nMaxRenaming),stat=iac)
     CALL alcerr(iac,'staRenamTmp',(/nSta,nMaxRenaming/),srName)
     ALLOCATE(staMjdTmp(nSta),stat=iac)
     CALL alcerr(iac,'staMjdTmp',(/nSta/),srName)
     ALLOCATE(staMjdInt(nSta,nMaxRenaming,2),stat=iac)
     CALL alcerr(iac,'staMjdInt',(/nSta,nMaxRenaming,2/),srName)
     DEALLOCATE(staNamTmpSingle,stat=iac)
     ALLOCATE(staNamTmpSingle(nSta),stat=iac)
     CALL alcerr(iac,'staNamTmpSingle',(/nSta/),srName)

     ! Sort files after epoch included in files
     DO iMjd = 1,opt%nCrds-1
        kMjd = iMjd
        DO jMjd = iMjd+1,opt%nCrds
           IF( CrdFileEpoch(jMjd) < CrdFileEpoch(kMjd) )THEN
              kMjd = jMjd
           END IF
        END DO
        IF( kMjd /= iMjd )THEN
           CrdFileEpochTmp = CrdFileEpoch(iMjd)
           CrdsFileNameTmp = opt%CrdFileName(iMjd)

           CrdFileEpoch(iMjd) = CrdFileEpoch(kMjd)
           opt%CrdFileName(iMjd) = opt%CrdFileName(kMjd)

           CrdFileEpoch(kMjd) = CrdFileEpochTmp
           opt%CrdFileName(kMjd) = CrdsFileNameTmp
        END IF
     END DO

     ! Compute the ref epoch and store the result into sCore%inTimRefCrd
     sCore%inTimRefCrd = CrdFileEpoch(1) + &
          (CrdFileEpoch(opt%nCrds) - CrdFileEpoch(1))/2.0D0

     DEALLOCATE(CrdFileEpoch, stat=iac)

     ! Sort alphabetically the station names and store them into sCore%sta
     ! and set sCore%sta(iSta)%ts%nVal and sCore%sta(iSta)%ts%nMjd to zero
     DO iSta = 1,sCore%nSta-1
        kSta = iSta
        DO jSta = iSta+1,sCore%nSta
           IF( sCore%sta(jSta)%name < sCore%sta(kSta)%name )THEN
              kSta = jSta
           END IF
        END DO
        IF( kSta /= iSta )THEN
           staTmp               = sCore%sta(iSta)%name
           sCore%sta(iSta)%name = sCore%sta(kSta)%name
           sCore%sta(kSta)%name = staTmp
        END IF
     END DO
     DO iSta = 1,sCore%nSta
        sCore%sta(iSta)%ts%nVal = 3
        sCore%sta(iSta)%ts%nMjd = 0
     END DO

     ! In case MENUAUX calls this subroutine EXIT at this point
     IF( opt%rdOnlyStaNames /= 0 )EXIT MENUAUX_LOOP

     ! Find nMjd for each sCore%sta(iSta)%ts%...
     ! (store the station name into sCore%sta(nSta)%name)
     DO iFil = 1,opt%nCrds
        CALL getco3(opt%CrdFileName(iFil),1,(/'#'/),nStaIn,staNamIn,&
             xStat=xStaIn)
        DO iSta = 1,nStaIn
           DO jSta = 1,sCore%nSta
              IF ( staNamIn(iSta)(1:nNum) == sCore%sta(jSta)%name(1:nNum) )THEN
                 sCore%sta(jSta)%ts%nMjd = sCore%sta(jSta)%ts%nMjd + 1
              END IF
           END DO
        END DO
     END DO

     ! Allocate memory of sCore%sta(iSta)%ts%mjd and %val
     DO iSta = 1,sCore%nSta
        nVal = sCore%sta(iSta)%ts%nVal
        nMjd = sCore%sta(iSta)%ts%nMjd
        ALLOCATE(sCore%sta(iSta)%ts%mjd(nMjd),stat=iac)
        CALL alcerr(iac, 'sCore%sta(iSta)%ts%mjd', (/nMjd/), srName)
        ALLOCATE(sCore%sta(iSta)%ts%val(nMjd,nVal),stat=iac)
        CALL alcerr(iac, 'sCore%sta(iSta)%ts%val', (/nMjd,nVal/), srName)
        sCore%sta(iSta)%ts%nMjd = 0
        IF( opt%modPreDatTest == 1 )THEN
           ALLOCATE(sCore%sta(iSta)%ts%dDef(nMjd,nVal),stat=iac)
           CALL alcerr(iac, 'sCore%sta(iSta)%ts%dDef',(/nMjd,nVal/),srName)
           ALLOCATE(sCore%sta(iSta)%ts%dDefRej(nMjd),stat=iac)
           CALL alcerr(iac, 'sCore%sta(iSta)%ts%dDefRej',(/nMjd/),srName)
        END IF
     END DO

     ! Initialize arrays
     DO iSta = 1,sCore%nSta
        sCore%sta(iSta)%upd%staStatusRef = 0
        IF( opt%modPreDatTest == 1 )THEN
           sCore%sta(iSta)%ts%dDef(:,:)  = 0.0D0
           sCore%sta(iSta)%ts%dDefRej(:) = 0
        END IF
     END DO

     ! Read each CRDs (opt%CrdFileName(iFil)) and store the X,Y,Z crds
     !   of each station and epoch into sCore%sta(iSta)%ts%nMjd and %val.
     DO iFil = 1,opt%nCrds
        CALL getco3(opt%CrdFileName(iFil),1,(/'#'/),nStaIn,staNamIn,&
             staNum=staNumIn,datum=sCore%datumName,timcrd=tMjdInCrd,&
             xStat=xStaIn)
        DO iSta = 1,nStaIn
           DO jSta = 1,sCore%nSta
              IF ( staNamIn(iSta)(1:nNum) == sCore%sta(jSta)%name(1:nNum) )THEN
                 sCore%sta(jSta)%ts%nMjd = sCore%sta(jSta)%ts%nMjd + 1
                 sCore%sta(jSta)%ts%mjd(sCore%sta(jSta)%ts%nMjd) = tMjdInCrd
                 sCore%sta(jSta)%ts%val(sCore%sta(jSta)%ts%nMjd,:) = &
                      xStaIn(:,iSta)
              END IF
           END DO
        END DO
     END DO

     ! Get datum parameters
     datum%name = sCore%datumName
     CALL getdat(datum%name, datum%aell, datum%bell, datum%dxell, &
          datum%drell, datum%scell)

     ! Only one reference station allowed (no renaming in CRDs)
     DO iSta = 1,sCore%nSta
        sCore%sta(iSta)%iRefSta = 1
        sCore%sta(iSta)%nStaSta = 1
        nStaSta = sCore%sta(iSta)%nStaSta
        ALLOCATE(sCore%sta(iSta)%staSta(nStaSta),stat=iac)
        CALL alcerr(iac, 'sCore%sta(iSta)%staSta', (/nStaSta/), srName)
     END DO

  ! End Read PLT+CRD+VEL or CRDs
  ! ============================
  END IF

  END DO MENUAUX_LOOP
  ! In case MENUAUX calls this subroutine RETURN at this point
  IF( opt%rdOnlyStaNames /= 0 )THEN
     !  CALL debug_entry(srName)
     RETURN
  END IF

  ! Deallocate variables
  DEALLOCATE(nStaNamTmp,stat=iac)
  DEALLOCATE(staNamTmp,stat=iac)
  DEALLOCATE(staRenamTmp,stat=iac)
  DEALLOCATE(staNamTmpSingle,stat=iac)
  DEALLOCATE(staMjdTmp,stat=iac)
  DEALLOCATE(staMjdInt,stat=iac)

  ! Find the sampling rate: 1) generate histogram, 2) max val of histogram
  ! ----------------------------------------------------------------------
  nHsmpl = 0
  hSmpl(:)%nSmpl = 0
  DO iSta = 1,sCore%nSta
     DO iMjd = 1,sCore%sta(iSta)%ts%nMjd-1
        deltaMjd = ABS( sCore%sta(iSta)%ts%mjd(iMjd+1) - &
                        sCore%sta(iSta)%ts%mjd(iMjd)     )
        seen = 0
        DO iHsmpl = 1,nHsmpl
           IF( deltaMjd == hSmpl(iHsmpl)%deltaMjd )THEN
              hSmpl(iHsmpl)%nSmpl = hSmpl(iHsmpl)%nSmpl + 1
              seen = 1
              EXIT
           END IF
        END DO
        IF( seen == 0 )THEN
           nHsmpl = nHsmpl + 1
           hSmpl(nHsmpl)%deltaMjd = deltaMjd
           hSmpl(nHsmpl)%nSmpl = 1
        END IF
     END DO
  END DO
  maxHsmpl = 0
  sCore%deltaMjd = 1.0D0
  DO iHsmpl = 1,nHsmpl
     IF( hSmpl(iHsmpl)%nSmpl > maxHsmpl )THEN
        maxHsmpl = hSmpl(iHsmpl)%nSmpl
        sCore%deltaMjd = hSmpl(iHsmpl)%deltaMjd
     END IF
  END DO

  ! Find and store sCore%begMjd, sCore%endMjd, sCore%nTotMjd
  ! ----------------------------------------------------------
  ! For coordinate time series from ADDNEQ2. Mandatory search procedure
  ! to test the datum definition when 3-days or 7-days time series are
  ! present.
  IF( ( opt%selInpTsType == inpRes   .AND. &
        reconstrCrd == 1             .AND. &
        reconstrVel == 1             .AND. &
        LEN_TRIM(opt%inPltFile) /= 0 .AND. &
        LEN_TRIM(opt%inCrdFile) /= 0 .AND. &
        LEN_TRIM(opt%inVelFile) /= 0       ) .OR. &
       opt%selInpTsType == inpRes                 )THEN

     ! Get the station with the largest number of data
     nHsmpl = 0
     kSta = 0
     DO iSta = 1,sCore%nSta
        IF( sCore%sta(iSta)%ts%nMjd > nHsmpl )THEN
           nHsmpl = sCore%sta(iSta)%ts%nMjd
           kSta = iSta
        END IF
     END DO
     IF( kSta == 0 )THEN
        WRITE(lfnerr,'(/,A,/)') &
             ' *** SR FODIRDAT: No time series found.'
        CALL exitrc(2)
     END IF
     ! Find the referece lag, important for sCore%deltaMjd > 1
     kMjd = 0
     refMjd = 0.0D0
     DO iMjd = 1,sCore%sta(kSta)%ts%nMjd-1
        deltaMjd = ABS( sCore%sta(kSta)%ts%mjd(iMjd+1) - &
             sCore%sta(kSta)%ts%mjd(iMjd)     )
        ! Find the synchro
        IF( INT(10000*deltaMjd) == INT(10000*sCore%deltaMjd) )THEN
           kMjd = kMjd + 1
        ELSE
           kMjd = 0
        END IF
        ! Synchro found
        IF( kMjd >= 5 )THEN
           refMjd = sCore%sta(kSta)%ts%mjd(iMjd)
           EXIT
        END IF
     END DO
     IF( refMjd == 0.0D0 )THEN
        WRITE(lfnerr,'(/,A,/)') &
             ' *** SR FODIRDAT: No reference epoch found.'
        CALL exitrc(2)
     END IF

     ! Find begMjd and endMjd
     sCore%begMjd = HUGE(0.D0)
     sCore%endMjd = 0.D0
     DO iSta = 1,sCore%nSta
        DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
           IF( sCore%sta(iSta)%ts%mjd(iMjd) > sCore%endMjd )THEN
              sCore%endMjd = sCore%sta(iSta)%ts%mjd(iMjd)
           END IF
           IF( sCore%sta(iSta)%ts%mjd(iMjd) < sCore%begMjd )THEN
              sCore%begMjd = sCore%sta(iSta)%ts%mjd(iMjd)
           END IF
        END DO
     END DO

     ! Sychronized begMjd and endMjd --- MOD gives always a positive value
     deltaMjd = refMjd - sCore%begMjd
     modDeltaMjd = MOD(deltaMjd,sCore%deltaMjd) ! * sCore%deltaMjd
     sCore%begMjd = sCore%begMjd - modDeltaMjd - sCore%deltaMjd
     deltaMjd = sCore%endMjd - refMjd
     modDeltaMjd = MOD(deltaMjd,sCore%deltaMjd) ! * sCore%deltaMjd
     sCore%endMjd = sCore%endMjd - modDeltaMjd + sCore%deltaMjd

     ! Find the first epoch --- with at leat 5% of all station
     IF( ( opt%selInpTsType == inpRes .AND. &
          reconstrCrd == 1 .AND. reconstrVel == 1 ) .OR. &
          opt%selInpTsType == inpCrd )THEN
        halfDeltaMjd = CEILING( sCore%deltaMjd / 2.0D0 )
     ELSE
        halfDeltaMjd = sCore%deltaMjd / 2.0D0
     END IF
     DO iTer = 1,5
        kMjd = 0
        DO iSta = 1,sCore%nSta
           IF( sCore%sta(iSta)%ts%mjd(1) > sCore%begMjd-halfDeltaMjd .AND. &
                sCore%sta(iSta)%ts%mjd(1) < sCore%begMjd+halfDeltaMjd )THEN
              kMjd = kMjd + 1
           END IF
        END DO
        IF( REAL(kMjd)/REAL(sCore%nSta) < 0.05D0 )THEN
           sCore%begMjd = sCore%begMjd + sCore%deltaMjd
        ELSE
           EXIT
        END IF
     END DO

     ! Find the last epoch --- with at leat 5% of all station
     DO iTer = 1,5
        kMjd = 0
        DO iSta = 1,sCore%nSta
           IF( sCore%sta(iSta)%ts%mjd(sCore%sta(iSta)%ts%nMjd) > &
               sCore%endMjd-halfDeltaMjd .AND. &
               sCore%sta(iSta)%ts%mjd(sCore%sta(iSta)%ts%nMjd) < &
               sCore%endMjd+halfDeltaMjd )THEN
              kMjd = kMjd + 1
           END IF
        END DO
        IF( REAL(kMjd)/REAL(sCore%nSta) < 0.05D0 )THEN
           sCore%endMjd = sCore%endMjd - sCore%deltaMjd
        ELSE
           EXIT
        END IF
     END DO
  ! For any coordinate time series else
  ELSE
     ! Find begMjd and endMjd
     sCore%begMjd = HUGE(0.D0)
     sCore%endMjd = 0.D0
     DO iSta = 1,sCore%nSta
        DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
           IF( sCore%sta(iSta)%ts%mjd(iMjd) > sCore%endMjd )THEN
              sCore%endMjd = sCore%sta(iSta)%ts%mjd(iMjd)
           END IF
           IF( sCore%sta(iSta)%ts%mjd(iMjd) < sCore%begMjd )THEN
              sCore%begMjd = sCore%sta(iSta)%ts%mjd(iMjd)
           END IF
        END DO
     END DO
  END IF

  ! Compute sCore%nTotMjd
  sCore%nTotMjd  = INT( ( sCore%endMjd - sCore%begMjd ) / sCore%deltaMjd ) + 1

  ! Check that VCV matrix is accepted only if nVal == 3
  IF( opt%inPltFileVciEna == 2 .AND. nVal /= 3 )THEN
     WRITE(lfnerr,'(/,A,/)') &
          ' *** SR FODIRDAT: Maximal 3 components in PLT-file.'
     CALL exitrc(2)
  END IF

  ! Define inTimRefCrd if not yet defined
  IF( sCore%inTimRefCrd == 0.0D0 )THEN
     sCore%inTimRefCrd = (sCore%endMjd - sCore%begMjd) / 2.0D0
  END IF

  ! Define outTimRefCrd if not yet defined
  sCore%outTimRefCrd = sCore%inTimRefCrd

  IF( opt%outTimRef /= 0.0D0 )THEN
     sCore%outTimRefCrd = opt%outTimRef
  ELSE IF( LEN_TRIM(opt%outStaFileForAddneq2) /= 0 .AND. &
           LEN_TRIM(opt%inCrdFileForAddneq2)  /= 0 .AND. &
           LEN_TRIM(opt%outCrdFileForAddneq2) /= 0 )THEN
     CALL getco3(opt%inCrdFileForAddneq2,1,(/'@'/),&
          nStat=nStaInCrd,stName=staNamInCrd,&
          timcrd=sCore%outTimRefCrd)
  END IF

  ! End Part 1 = read PLT or CRDs
  ! =============================
  ! =============================
  END IF


  ! Begin Part 2 = convert TS XYZ to NEU
  ! ====================================
  ! ====================================
  IF( part == 2 )THEN

  nVal = sCore%nVal

  ! From TS-XYZ to TS-NEU residuals
  IF( nVal == 3 .AND. reconstrCrd == 1 )THEN

     ! Loop over all stations
     DO iSta = 1,sCore%nSta

        nMjd = sCore%sta(iSta)%ts%nMjd

        ! Define parameters
        lsa%nnPar = 2 * nVal
        lsa%nnMjd = nVal * sCore%sta(iSta)%ts%nMjd
        lsa%dof = lsa%nnMjd - lsa%nnPar

        ! Check degree of freedom
        IF( lsa%dof < 1 )THEN
           sCore%sta(iSta)%ts%val(:,:) = 0.0D0
           CYCLE
        END IF

        ! Allocation memory
        ALLOCATE(lsa%A(lsa%nnMjd,lsa%nnPar),stat=iac)
        CALL alcerr(iac,'lsa%A',(/lsa%nnMjd,lsa%nnPar/),srName)
        ALLOCATE(lsa%Qxx(lsa%nnPar,lsa%nnPar),stat=iac)
        CALL alcerr(iac,'lsa%Qxx',(/lsa%nnPar,lsa%nnPar/),srName)
        ALLOCATE(lsa%y(lsa%nnMjd),stat=iac)
        CALL alcerr(iac,'lsa%y',(/lsa%nnMjd/),srName)
        ALLOCATE(lsa%x(lsa%nnPar),stat=iac)
        CALL alcerr(iac,'lsa%x',(/lsa%nnPar/),srName)
        ALLOCATE(lsa%mod(lsa%nnMjd),stat=iac)
        CALL alcerr(iac,'lsa%mod',(/lsa%nnMjd/),srName)
        ALLOCATE(lsa%v(lsa%nnMjd),stat=iac)
        CALL alcerr(iac,'lsa%v', (/lsa%nnMjd/),srName)
        ALLOCATE(lsa%P(lsa%nnMjd),stat=iac)
        CALL alcerr(iac,'lsa%P', (/lsa%nnMjd/),srName)
        ALLOCATE(lsa%iQyy(nMjd,nVal,nVal),stat=iac)
        CALL alcerr(iac,'lsa%iQyy', (/nMjd,nVal,nVal/),srName)

        ! Define the first design matrix
        lsa%A(:,:) = 0.0D0
        DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
           indM = nVal*(iMjd-1)
           DO iVal = 1,nVal
              lsa%A(indM+iVal,iVal) = 1.0D0
              lsa%A(indM+iVal,iVal+nVal) = &
                   sCore%sta(iSta)%ts%mjd(iMjd) - sCore%inTimRefCrd
              lsa%y(indM+iVal) = sCore%sta(iSta)%ts%val(iMjd,iVal)
           END DO
        END DO

        ! Least squares adjustment
        CALL fodislsa( 0, lsa )

        ! Store CRD and VEL results in XYZ
        IF( lsa%detN == 0.0D0 .OR. lsa%dof <= 0 )THEN
           sCore%sta(iSta)%outCrdXyz(:) = 0.0D0
           sCore%sta(iSta)%outVelXyz(:) = 0.0D0
           lsa%m0 = 0.0D0
        ELSE
           sCore%sta(iSta)%outCrdXyz(:) = lsa%x(1:nVal)
           sCore%sta(iSta)%outVelXyz(:) = lsa%x(nVal+1:2*nVal)
        END IF

        ! Add missing information in case of CRDs
        IF( opt%selInpTsType == inpCrd )THEN
           sCore%sta(iSta)%iRefSta = 1
           iStaSta = sCore%sta(iSta)%iRefSta
           sCore%sta(iSta)%staSta(iStaSta)%inCrdXyz(:) = &
                sCore%sta(iSta)%outCrdXyz(:)
           CALL xyzell(datum%aell,datum%bell, &
                datum%dxell,datum%drell,datum%scell, &
                sCore%sta(iSta)%staSta(iStaSta)%inCrdXyz(:) ,&
                sCore%sta(iSta)%staSta(iStaSta)%inCrdNeu(:) )
        END IF

        ! Transform residuals form XYZ to NEU
        DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
           indM = nVal*(iMjd-1)
           resXyz(:) = lsa%v(indM+1:indM+nVal)
           iRefSta = sCore%sta(iSta)%iRefSta
           CALL eccell(sCore%sta(iSta)%staSta(iRefSta)%inCrdNeu, &
                       resXyz(:),resNeu(:) )
           sCore%sta(iSta)%ts%val(iMjd,:) = resNeu(:)
        END DO

        ! Deallocation memory
        DEALLOCATE(lsa%A,stat=iac)
        DEALLOCATE(lsa%Qxx,stat=iac)
        DEALLOCATE(lsa%y,stat=iac)
        DEALLOCATE(lsa%x,stat=iac)
        DEALLOCATE(lsa%mod,stat=iac)
        DEALLOCATE(lsa%v,stat=iac)
        DEALLOCATE(lsa%P,stat=iac)
        DEALLOCATE(lsa%iQyy,stat=iac)

     ! End Loop over all stations
     END DO

  ! End From TS-XYZ to TS-NEU residuals
  END IF

  ! Filter out stations with too few epochs
  ! ---------------------------------------
  ! Filter Coordiante and Velocity Files
  IF( .NOT. ( opt%selInpTsType == inpRes .AND. &
              LEN_TRIM(opt%inPltFile) == 0 .AND. &
              LEN_TRIM(opt%inCrdFile) /= 0 .AND. &
              LEN_TRIM(opt%inVelFile) /= 0 ) )THEN
     DO iSta = 1,sCore%nSta
        IF( sCore%sta(iSta)%ts%nMjd < opt%inMinNrObs )THEN
           sCore%sta(iSta)%outCrdXyz(:) = 0.0D0
           sCore%sta(iSta)%outVelXyz(:) = 0.0D0
           sCore%sta(iSta)%staSta(sCore%sta(iSta)%iRefSta)%inCrdXyz(:) = 0.0D0
           sCore%sta(iSta)%staSta(sCore%sta(iSta)%iRefSta)%inVelXyz(:) = 0.0D0
           sCore%sta(iSta)%staSta(sCore%sta(iSta)%iRefSta)%inCrdNeu(:) = 0.0D0
           sCore%sta(iSta)%staSta(sCore%sta(iSta)%iRefSta)%inVelNeu(:) = 0.0D0
           sCore%sta(iSta)%staSta(sCore%sta(iSta)%iRefSta)%inCrdFlg = 'N'
           sCore%sta(iSta)%staSta(sCore%sta(iSta)%iRefSta)%inVelFlg = 'N'
        END IF
     END DO
  END IF

  ! Write informations to output file (lfnprt)
  ! ------------------------------------------

  WRITE(lfnprt,'(/,2(A,/))')                  &
       ' GENERAL OPTIONS',  &
       ' ---------------'

  WRITE(lfnprt,'(A,/,A)')                         &
       ' Main characteristics of the input data',  &
       ' --------------------------------------'
  CALL timst2(1,1,sCore%begMjd,mjdBeginStr)
  CALL timst2(1,1,sCore%endMjd,mjdEndStr)
  WRITE(lfnprt,'(3(A,1X,I6,/),1(A,1X,F12.5,/),&
       &1(A,2X,A20,1X,A,1X,F11.5,1X,A,/),1(A,2X,A20,1X,A,1X,F11.5,1X,A))') &
       ' Number of time series (stations)              :', sCore%nSta,     &
       ' Number of components                          :', nVal,           &
       ' Number of epochs                              :', sCore%nTotMjd,  &
       ' Sampling rate (days)                          :', sCore%deltaMjd, &
       ' Time series initial epoch                     :', mjdBeginStr,    &
       '( MJD:', sCore%begMjd,')',                                         &
       ' Time series last epoch                        :', mjdEndStr,      &
       '( MJD:', sCore%endMjd,')'

  IF( opt%inPltFileVciEna == 1 )THEN
     varCovInfoText = 'Variance-only'
  ELSE IF( opt%inPltFileVciEna == 2 )THEN
     varCovInfoText = 'Space variance-covariance'
  ELSE
     varCovInfoText = 'None'
  END IF
  WRITE(lfnprt,'(A,1X,I6,1X,A1,A,A1)') &
       ' Covariance information considered             :', &
       opt%inPltFileVciEna, '(', TRIM(varCovInfoText), ')'

  inCrdFileAddPltTxt = ' no'
  IF(  LEN_TRIM(opt%inCrdFile) /= 0 .AND. &
       opt%inCrdFileAddPlt == 1 )THEN
     inCrdFileAddPltTxt = 'yes'
  END IF
  WRITE(lfnprt,'(A,1X,I6,1X,A1,A,A1)') &
       ' Apply coordinates to the residuals            :', &
       opt%inCrdFileAddPlt, '(', TRIM(inCrdFileAddPltTxt), ')'
  inVelFileAddPltTxt = ' no'
  IF(  LEN_TRIM(opt%inVelFile) /= 0 .AND. &
       opt%inVelFileAddPlt == 1 )THEN
     inVelFileAddPltTxt = 'yes'
  END IF
  WRITE(lfnprt,'(A,1X,I6,1X,A1,A,A1,/)') &
       ' Apply velocities to the residuals             :', &
       opt%inVelFileAddPlt, '(', TRIM(inVelFileAddPltTxt), ')'

  WRITE(lfnprt,'(A,/,A)')                         &
       ' Main characteristics of the model',  &
       ' ---------------------------------'
  tmpTxt =  ' no'
  IF( opt%modNewJumpIdentify == 1 ) tmpTxt =  'yes'
  WRITE(lfnprt,'(A,1X,I6,1X,A1,A,A1)') &
       ' Identify new discontinuities                  :', &
       opt%modNewJumpIdentify, '(', tmpTxt, ')'

  tmpTxt =  ' no'
  IF( opt%modNewVeloIdentify == 1 ) tmpTxt =  'yes'
  WRITE(lfnprt,'(A,1X,I6,1X,A1,A,A1)') &
       ' Identify new velocities                       :', &
       opt%modNewVeloIdentify, '(', tmpTxt, ')'

  tmpTxt =  ' no'
  IF( opt%modNewOutlIdentify == 1 ) tmpTxt =  'yes'
  WRITE(lfnprt,'(A,1X,I6,1X,A1,A,A1)') &
       ' Identify new outliers                         :', &
       opt%modNewOutlIdentify, '(', tmpTxt, ')'

  tmpTxt =  ' no'
  IF( opt%modNewPeriIdentify == 1 ) tmpTxt =  'yes'
  WRITE(lfnprt,'(A,1X,I6,1X,A1,A,A1)') &
       ' Identify new functional periods               :', &
       opt%modNewPeriIdentify, '(', tmpTxt, ')'

  WRITE(lfnprt,'(A,2X,F8.4,A)') &
       ' Minimal interval length for all rate intervals:  ', &
       opt%minIntervForVel / 365.25D0, ' (years)'

  tmpTxt =  ' no'
  IF( opt%modAddVeloAJ == 1 ) tmpTxt =  'yes'
  WRITE(lfnprt,'(A,1X,I6,1X,A1,A,A1)') &
       ' Propose velocity change after new jump        :', &
       opt%modAddVeloAJ, '(', tmpTxt, ')'

  WRITE(lfnprt,'(A,I05)') &
       ' Number of outliers to be proposed             :  ', opt%modNNewOutl

  minPeri = 9999.9999D0
  maxPeri = 0.0D0
  IF( opt%modPerAnn == 1 )THEN
     maxPeri = 365.25D0
     minPeri = 365.25D0/2.0D0
  END IF
  DO iPeri = 1,opt%nAddPerParam
     period = 2*pi/opt%modAddPerParam(iPeri)
     IF( period > maxPeri ) maxPeri = period
     IF( period < minPeri ) minPeri = period
  END DO
  IF( opt%modNewPeriIdentify == 1 )THEN
     DO iPeri = 1,sCore%san%nPer
        period = 2*pi/sCore%san%per(iPeri)
        IF( period > maxPeri ) maxPeri = period
        IF( period < minPeri ) minPeri = period
     END DO
  END IF
  IF( maxPeri == 0.0D0 .AND. minPeri == 9999.9999D0 )THEN
     minPeri = 0.0D0
     maxPeri = 0.0D0
  END IF
  WRITE(lfnprt,'(A,F10.4)') &
       ' Minimal periodic function (days)              :  ', minPeri
  WRITE(lfnprt,'(A,F10.4)') &
       ' Maximal periodic function (days)              :  ', maxPeri

  IF( opt%modPreDatTest == 1 )THEN
  WRITE(lfnprt,'(/,A,/,A)')                         &
       ' Test datum definition',  &
       ' ---------------------'

  WRITE(lfnprt,'(1(A,2X,6(I1,A1),I1))' ) &
       ' Helmert parameters (Ty-Ty-Tz-Ry-Ry-Rz-Sc)     :',  &
       opt%datHlmTx,'-',opt%datHlmTy,'-',opt%datHlmTz,'-', &
       opt%datHlmRx,'-',opt%datHlmRy,'-',opt%datHlmRz,'-',opt%datHlmSc

  tmpTxt =  ' no'

  IF( opt%datRejectCrd == 1 ) tmpTxt =  'yes'
  WRITE(lfnprt,'(A,1X,I6,1X,A1,A,A1)') &
       ' Enable outlier rejection for coordinates      :',  &
       opt%datRejectCrd, '(', tmpTxt, ')'
  WRITE(lfnprt,'(1(A,F5.1,/),&
                &1(A,F5.1,/),&
                &1(A,F5.1))' ) &
       ' Outlier criteria north component [mm]         :    ',  &
       opt%datNLimitCrd * 1.0D3, &
       ' Outlier criteria east component [mm]          :    ',  &
       opt%datELimitCrd * 1.0D3 , &
       ' Outlier criteria up component [mm]            :    ',  &
       opt%datULimitCrd * 1.0D3
  IF( opt%datRejectOutlCrd == 1 )THEN
     varCovInfoText = 'TEST: test for significance'
  ELSE IF( opt%datRejectOutlCrd == 2 )THEN
     varCovInfoText = 'INSR: insert as significant'
  ELSE
     varCovInfoText = 'NONE'
  END IF
  WRITE(lfnprt,'(A,1X,I6,1X,A1,A,A1)') &
       ' Propse coordinate outliers new elements       :',  &
       opt%datRejectOutlCrd, '(', TRIM(varCovInfoText), ')'

  END IF

  WRITE(lfnprt,'(/,A,/,A)')                         &
       ' Main characteristics of the tests',  &
       ' ---------------------------------'
  WRITE(lfnprt,'(1(A,2X,F10.8,/),1(A,F7.3,/),4(4(A,1X,F9.3,A,/)))') &
       ' Minimal relative improvement (abort criterion):    ',  &
       opt%modTestValue, &
       ' Independent statistical test for outliers     :    ',  &
       opt%modOutlTestValue, &
       ' DISC: minimal size for space                  : ', &
       opt%modJumpSpac, ' (meter)', &
       ' DISC: minimal size for single                 : ', &
       opt%modJumpSing, ' (meter)', &
       ' DISC: minimal size for horizontal             : ', &
       opt%modJumpHori, ' (meter)', &
       ' DISC: minimal size for vertical               : ', &
       opt%modJumpVert, ' (meter)', &
       ' VELO: minimal size for space                  : ', &
       opt%modVeloSpac, ' (meter)', &
       ' VELO: minimal size for single                 : ', &
       opt%modVeloSing, ' (meter)', &
       ' VELO: minimal size for horizontal             : ', &
       opt%modVeloHori, ' (meter)', &
       ' VELO: minimal size for vertical               : ', &
       opt%modVeloVert, ' (meter)', &
       ' OUTL: minimal size for space                  : ', &
       opt%modOutlSpac, ' (meter)', &
       ' OUTL: minimal size for single                 : ', &
       opt%modOutlSing, ' (meter)', &
       ' OUTL: minimal size for horizontal             : ', &
       opt%modOutlHori, ' (meter)', &
       ' OUTL: minimal size for vertical               : ', &
       opt%modOutlVert, ' (meter)', &
       ' PERI: minimal size for space                  : ', &
       opt%modPeriSpac, ' (meter)', &
       ' PERI: minimal size for single                 : ', &
       opt%modPeriSing, ' (meter)', &
       ' PERI: minimal size for horizontal             : ', &
       opt%modPeriHori, ' (meter)', &
       ' PERI: minimal size for vertical               : ', &
       opt%modPeriVert, ' (meter)'

  WRITE(lfnprt,'(A,/,A)')                         &
       ' Options to generate files for ADDNEQ2',  &
       ' -------------------------------------'
  CALL timst2(1,1,sCore%outTimRefCrd,mjdRefStr)
  WRITE(lfnprt,'(1(A,2X,A20,1X,A,1X,F10.4,1X,A,/),&
                &1(A,1X,F6.3/),1(A,F7.3,/),/)') &
       ' Reference epoch for station coordinates       :', mjdRefStr,      &
       '( MJD:', sCore%outTimRefCrd,')',                                   &
       ' Minimal interval length (years)               :    ',  &
       opt%minTimeIntLength / 365.25D0, &
       ' Minimal percet data for reference sites       :    ',  &
       opt%minPercDataFix

  IF( opt%selInpTsType == inpCrd .OR. ( opt%selInpTsType == inpRes .AND. &
                                        LEN_TRIM(opt%inCrdFile) /= 0 .AND. &
                                        nVal == 3 ) )THEN

     WRITE(lfnprt,'(/,2(A,/))')                  &
          ' INPUT: COORDINATE FILES',  &
          ' -----------------------'
     CALL timst2(1,1,sCore%inTimRefCrd,tsRefEpoch)
     WRITE(lfnprt,'(1X,A,4X,A,5X,A,F12.5,A,/,1X,130("-"))')          &
          ' Reference Epoch :',                                      &
          tsRefEpoch,                                                &
          '( ',                                                      &
          sCore%inTimRefCrd,                                         &
          ' MJD )'

     WRITE(lfnprt,'(A,A)',ADVANCE='NO') &
     '   Nr Station        Samples         X(m)          Y(m)          '// &
     'Z(m)      VX(m/y)  VY(m/y)  VZ(m/y) KEYWORD '
     IF( opt%outFileVerbose == 1 )THEN
        WRITE(lfnprt,'(A)') '[Ref. site time span]'
     ELSE
        WRITE(lfnprt,'(A)') '                     '
     END IF
     WRITE(lfnprt,'(1X,130("-"))')
     DO iSta = 1,sCore%nSta
        WRITE(lfnprt,'(I4,1X,A16,1X,I6,1X,3(F14.4),1X,3(1X,F8.4),&
             &2X,A,1X)',ADVANCE='NO')  &
             iSta,                                                   &
             sCore%sta(iSta)%name,                                   &
             sCore%sta(iSta)%ts%nMjd,                                &
             sCore%sta(iSta)%outCrdXyz(:),                           &
             sCore%sta(iSta)%outVelXyz(:) * 365.25D0,                &
             'APRICRD'
        begMjdRef = ''
        endMjdRef = ''
        IF( opt%outFileVerbose == 1 )THEN
           DO iStaSta = 1,sCore%sta(iSta)%nStaSta
              IF( sCore%sta(iSta)%staSta(iStaSta)%inCrdFlg == 'W' )THEN
                 WRITE(begMjdRef,'(F10.4)') &
                      sCore%sta(iSta)%staSta(iStaSta)%timint%t(1)
                 WRITE(endMjdRef,'(F10.4)') &
                      sCore%sta(iSta)%staSta(iStaSta)%timint%t(2)
                 EXIT
              END IF
           END DO
        END IF
        WRITE(lfnprt,'(A10,1X,A10)') begMjdRef, endMjdRef
     END DO
  END IF

  ! Test datum definition
  IF( opt%modPreDatTest == 1 .AND. reconstrCrd == 1 .AND. &
                                   reconstrVel == 1 )THEN

     ! Title
     WRITE(lfnprt,'(/,/,A,/,A)')    &
          ' TEST DATUM DEFINITION', &
          ' ---------------------'

     WRITE(lfnprt,'(/,A,/,A)')                         &
       ' Test coordinates epoch-by-epoch',  &
       ' -------------------------------'

     WRITE(lfnprt,'(A,1X,E12.6,1X,A2,1X,E12.6)') &
          ' Offset Tx [m]       :',  &
          sCore%ddf%hlmParOffst(1), '+-', sCore%ddf%hlmParOffstRms(1)
     WRITE(lfnprt,'(A,1X,E12.6,1X,A2,1X,E12.6)') &
          ' Drift Tx [m/year]   :',  &
          sCore%ddf%hlmParDrift(1) * 365.25D0, '+-', &
          sCore%ddf%hlmParDriftRms(1) * 365.25D0
     WRITE(lfnprt,'(A,1X,E12.6,1X,A2,1X,E12.6)') &
          ' Offset Ty [m]       :',  &
          sCore%ddf%hlmParOffst(2), '+-', sCore%ddf%hlmParOffstRms(2)
     WRITE(lfnprt,'(A,1X,E12.6,1X,A2,1X,E12.6)') &
          ' Drift Ty [m/year]   :',  &
          sCore%ddf%hlmParDrift(2) * 365.25D0, '+-', &
          sCore%ddf%hlmParDriftRms(2) * 365.25D0
     WRITE(lfnprt,'(A,1X,E12.6,1X,A2,1X,E12.6)') &
          ' Offset Tz [m]       :',  &
          sCore%ddf%hlmParOffst(3), '+-', sCore%ddf%hlmParOffstRms(3)
     WRITE(lfnprt,'(A,1X,E12.6,1X,A2,1X,E12.6)') &
          ' Drift Tz [m/year]   :',  &
          sCore%ddf%hlmParDrift(3) * 365.25D0, '+-', &
          sCore%ddf%hlmParDriftRms(3) * 365.25D0

     WRITE(lfnprt,'(A,1X,E12.6,1X,A2,1X,E12.6)') &
          ' Offset Rx [mas]     :',  &
          sCore%ddf%hlmParOffst(4), '+-', sCore%ddf%hlmParOffstRms(4)
     WRITE(lfnprt,'(A,1X,E12.6,1X,A2,1X,E12.6)') &
          ' Drift Rx [mas/year] :',  &
          sCore%ddf%hlmParDrift(4) * 365.25D0, '+-', &
          sCore%ddf%hlmParDriftRms(4) * 365.25D0
     WRITE(lfnprt,'(A,1X,E12.6,1X,A2,1X,E12.6)') &
          ' Offset Ry [mas]     :',  &
          sCore%ddf%hlmParOffst(5), '+-', sCore%ddf%hlmParOffstRms(5)
     WRITE(lfnprt,'(A,1X,E12.6,1X,A2,1X,E12.6)') &
          ' Drift Ry [mas/year] :',  &
          sCore%ddf%hlmParDrift(5) * 365.25D0, '+-', &
          sCore%ddf%hlmParDriftRms(5) * 365.25D0
     WRITE(lfnprt,'(A,1X,E12.6,1X,A2,1X,E12.6)') &
          ' Offset Rz [mas]     :',  &
          sCore%ddf%hlmParOffst(6), '+-', sCore%ddf%hlmParOffstRms(6)
     WRITE(lfnprt,'(A,1X,E12.6,1X,A2,1X,E12.6)') &
          ' Drift Rz [mas/year] :',  &
          sCore%ddf%hlmParDrift(6) * 365.25D0, '+-', &
          sCore%ddf%hlmParDriftRms(6) * 365.25D0

     WRITE(lfnprt,'(A,1X,E12.6,1X,A2,1X,E12.6)') &
          ' Offset Sc [ppb]     :',  &
          sCore%ddf%hlmParOffst(7), '+-', sCore%ddf%hlmParOffstRms(7)
     WRITE(lfnprt,'(A,1X,E12.6,1X,A2,1X,E12.6)') &
          ' Drift Sc [ppb/year] :',  &
          sCore%ddf%hlmParDrift(7) * 365.25D0, '+-', &
          sCore%ddf%hlmParDriftRms(7) * 365.25D0

     WRITE(lfnprt,'(/,A,/,A)')  &
     '   Nr Epoch               nSta ref rej RMS[m] | Tx [m]     Ty [m]&
     &     Tz [m]     Rx [mas]   Ry [mas]   Rz [mas]   S[1-mm/km] | KYWD', &
     ' ----------------------------------------------------------------&
     &------------------------------------------------------------------'
     DO iMjd = 1,sCore%nTotMjd
        datstr = ''
        IF( sCore%ddf%mjd(iMjd) > 1.0D04 .AND. &
            sCore%ddf%mjd(iMjd) < 1.0D20 )THEN
           CALL timst2(1,1,sCore%ddf%mjd(iMjd),datstr)
        END IF
        WRITE(lfnprt,'(I05,1X,A20,&
                      &I04,1X,I03,1X,I03,1X,&
                      &F6.4,1X,A1,1X,7(E10.4,1X),A1,1X,A4)') &
                      iMjd, &
                      datstr, &
                      sCore%ddf%totSta(iMjd), &
                      sCore%ddf%refSta(iMjd), &
                      sCore%ddf%rejSta(iMjd), &
                      sCore%ddf%rmsHlmTra(iMjd), &
                      '|', &
                      sCore%ddf%hlmPar(iMjd,:), &
                      '|', &
                      'HPAR'
        WRITE(lfnprt,'(I05,1X,A20,&
                      &I04,1X,I03,1X,I03,1X,&
                      &F6.4,1X,A1,1X,7(E10.4,1X),A1,1X,A4)') &
                      iMjd, &
                      datstr, &
                      sCore%ddf%totSta(iMjd), &
                      sCore%ddf%refSta(iMjd), &
                      sCore%ddf%rejSta(iMjd), &
                      sCore%ddf%rmsHlmTra(iMjd), &
                      '|', &
                      sCore%ddf%rmsHlmPar(iMjd,:), &
                      '|', &
                      'HRMS'
     END DO

  ! End Test datum definition
  END IF

  ! End Part 2 = convert TS XYZ to NEU
  ! ==================================
  ! ==================================
  END IF


! End of subroutine
! -----------------
!  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodirdat

! -------------------------------------------------------------------------

SUBROUTINE fodirdat_pltCrdLine(line, staRd, nMjdRd, crdRd, resRd, mjdRd, &
                               stdDevInfo, varCovInfo)

! -------------------------------------------------------------------------
! Purpose:   Subroutine to read a line from the PLT with a given format
!
! Author:    Luca Ostini
!
! Created:   14-Aug-2008
! Last mod.:
!
! Changes:
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
! -------------------------------------------------------------------------

! Used Modules
! ------------
! structures, variables:
  USE m_bern,    ONLY: i4b, r8b, lineLength, staNameLength

! operator, methods:
!  USE d_debug,   ONLY: debug_entry, debug_exit

! subroutines, functions:

! no implicit
  IMPLICIT NONE

! List of Arguments
! -----------------
! input:
  CHARACTER(LEN=lineLength)                   :: line

! input/output:

! output:
  CHARACTER(LEN=staNameLength)                :: staRd
  INTEGER(i4b)                                :: nMjdRd
  INTEGER(i4b)                                :: crdRd
  REAL(r8b)                                   :: resRd
  REAL(r8b)                                   :: mjdRd
  REAL(r8b), OPTIONAL                         :: stdDevInfo
  REAL(r8b), DIMENSION(3),       &
    OPTIONAL                                  :: varCovInfo

  CHARACTER(LEN=47)                           :: line1stPart
  CHARACTER(LEN=lineLength-47)                :: line2ndPart


! Local Types
! -----------


! Local Parameters
! ----------------


! Local Variables
! ---------------
  REAL(r8b)                      :: rms
  REAL(r8b)                      :: cor

! Call debug routine
! ------------------
!  CALL debug_entry(srName)

! Initialization of all variables
! -------------------------------
 ! Divide the line in 1st and 2nd part
  READ(line,'(a16,a)') line1stPart, line2ndPart

  ! Read the first part of the line (common for all lines)
  READ(line1stPart,'(a16)') staRd

  ! Read the second part of the line
  IF( PRESENT( varCovInfo ) )THEN
     READ(line2ndPart,*) nMjdRd, crdRd, resRd, mjdRd, rms, cor
  ELSEIF( PRESENT( stdDevInfo ) )THEN
     READ(line2ndPart,*) nMjdRd, crdRd, resRd, mjdRd, rms
  ELSE
     READ(line2ndPart,*) nMjdRd, crdRd, resRd, mjdRd
  ENDIF

  ! Assumption of coordinates: rms and cov are not allowed to be equal zero
  ! Additionally, rms is not allowed to be bigger than 1.0 meters
  IF( rms < 0.0001D0 )THEN
     rms = 0.0001D0
  END IF
  IF( rms > 1.0D0 )THEN
     rms = 1.0D0
  END IF
  IF( cor >= 0.5D0 )THEN
     cor = 0.5D0
  END IF
  IF( cor <= -0.5D0 )THEN
     cor = -0.5D0
  END IF

  ! Report the results
  IF( PRESENT( varCovInfo ) )THEN
     varCovInfo(:) = (/rms,cor,0.0D0/)
  ELSEIF( PRESENT( stdDevInfo ) )THEN
    stdDevInfo = rms
  ENDIF

! End of subroutine
! -----------------
!  CALL debug_exit(srName)
  RETURN
END SUBROUTINE fodirdat_pltCrdLine


END MODULE s_FODIRDAT

