MODULE s_FODIUPDF
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodiupdf(opt,sCore,datFil)

! -------------------------------------------------------------------------
! Purpose:    Read/Write files for ADDNEQ2.
!             Update the FIX, SIG, CRD, and VEL files with renamed stations.
!
! Author:     Luca Ostini
!
! Created:    14-Aug-2008
!
! Changes:    14-Aug-2008 LO: Created this file
!             02-Oct-2008 LO: First revision
!             09-Oct-2008 LO: Second revision
!             09-Oct-2008 LO: Third revision
!             05-Dec-2008 LO: Fourth revisio: velocity changes allowed
!             11-Feb-2009 LO: Fifth revision: major changes
!             12-Mar-2009 LO: Redundancy of velocity constraints removed
!             13-Aug-2009 LO: Error added in case of missing station.
!             14-Aug-2009 LO: Getco3 changed
!             18-Aug-2009 LO: Cosmetics
!             20-Aug-2009 LO: Cosmetics
!             25-Sep-2009 LO: Changes for F90 consistency
!             18-Nov-2009 LO: Major changes for consistency
!             21-Dec-2009 LO: FFT removed and several changes apported
!             02-Mar-2010 LO: Major changes do to elimination of FODISUBR
!             07-Apr-2010 LO: Major changes do to algorithm and output file
!             16-Jun-2010 LO: Time series diveded by component
!             26-Aug-2010 LO: Architectural changes
!             30-Aug-2010 LO: Additional reports for fiducial stations
!             03-Jan-2011 LO: INTENT(INOUT) removed and bug fixed
!             18-Feb-2011 RD: Replace WTVELO by WTSTAT
!             20-May-2011 LO: New datum definition with all fiducial sites
!             24-May-2011 LO: New update of ADDNEQ2 meta-data
!             28-Apr-2012 RD: Nullify all pointers, use p_fodits with only
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------


! Used Modules
! ------------
! structures, variables:
  USE m_bern,    ONLY: i4b, r8b, lfnPrt, staNameLength, &
                       shortLineLength
  USE d_const,   ONLY: filTitle
  USE d_stalst,  ONLY: t_staList
  USE p_fodits,  ONLY: t_datFil, t_opt, t_score, &
                       veloRelCstr, typeJump, typeVelo, typeOutl, &
                       infoErqF, infoEvlF, infoStaF, infoPanl

! operator, methods:
  USE m_time,    ONLY: t_timint, OPERATOR(.isIn.)
!  USE d_debug,   ONLY: debug_entry, debug_exit

! subroutines, functions:
  USE s_alcerr
  USE s_getco3
  USE s_readstsg
  USE s_timst2
  USE s_writstsg
  USE s_wtstat

! no implicit
  IMPLICIT NONE

! subroutine name
  CHARACTER(LEN=shortLineLength), PARAMETER    :: srName = 'fodiupdf'


! List of Arguments
! -----------------
! input:
  TYPE(t_opt)                    :: opt         ! Option structure
  TYPE(t_sCore)                  :: sCore       ! Core structure of FODITS
  TYPE(t_datFil)                 :: datFil      ! I/O files structure

! input/output:

! output:


! Local Types
! -----------
  TYPE t_report_ren
     CHARACTER(LEN=staNameLength)      :: name
     CHARACTER(LEN=staNameLength)      :: rename
     CHARACTER(LEN=20)                 :: epochRenameTxt
     CHARACTER(LEN=20)                 :: epochEndRenameTxt
     INTEGER(i4b)                      :: group
     CHARACTER(LEN=8)                  :: remark
     CHARACTER(LEN=4)                  :: typeTxt
     CHARACTER(LEN=4)                  :: infoTxt
     CHARACTER(LEN=1)                  :: fidAft
     REAL(r8b)                         :: years
     REAL(r8b)                         :: perc
  END TYPE t_report_ren

  TYPE t_report
     CHARACTER(LEN=1)                  :: fidBef
     CHARACTER(LEN=1)                  :: fidAft
     REAL(r8b)                         :: years
     REAL(r8b)                         :: perc
     INTEGER(i4b)                      :: nRen
     TYPE(t_report_ren),DIMENSION(:),&
       POINTER                         :: ren
  END TYPE t_report

  TYPE t_statfid
     CHARACTER(LEN=staNameLength)      :: name
     CHARACTER(LEN=1)                  :: fidBef
     CHARACTER(LEN=1)                  :: fidAft
     TYPE(t_timint)                    :: timBef
     TYPE(t_timint)                    :: timAft
     TYPE(t_timint)                    :: tTimAft
     REAL(r8b)                         :: years
     REAL(r8b)                         :: perc
  END TYPE t_statfid

! Local Parameters
! ----------------


! Local Variables
! ---------------
  TYPE(t_staList)                :: inFixF
  TYPE(t_staList)                :: outFixF
  TYPE(t_report),DIMENSION(:),&
       ALLOCATABLE               :: report
  TYPE(t_statfid),DIMENSION(:),&
       ALLOCATABLE               :: statfid

  CHARACTER(LEN=staNameLength),  &
    DIMENSION(:),POINTER         :: staNamInCrd
  CHARACTER(LEN=1),              &
    DIMENSION(:),POINTER         :: staFlgInCrd
  CHARACTER(LEN=staNameLength),  &
    DIMENSION(:),POINTER         :: staNamInVel
  CHARACTER(LEN=1),              &
    DIMENSION(:),POINTER         :: staFlgInVel
  CHARACTER(LEN=4),              &
    DIMENSION(:),POINTER         :: staPlateInVel
  CHARACTER(LEN=staNameLength),  &
    DIMENSION(:),ALLOCATABLE     :: staNam
  CHARACTER(LEN=1),              &
    DIMENSION(:), ALLOCATABLE    :: velFlg
  CHARACTER(LEN=1),              &
    DIMENSION(:),ALLOCATABLE     :: crdFlg
  CHARACTER(LEN=4),              &
    DIMENSION(:), ALLOCATABLE    :: plate
  CHARACTER(LEN=staNameLength)   :: staNamTmp
  CHARACTER(LEN=1)               :: crdFlgTmp
  CHARACTER(LEN=1)               :: velFlgTmp
  CHARACTER(LEN=4)               :: plateTmp
  CHARACTER(LEN=20)              :: datstr
  CHARACTER(LEN=2)               :: prevSta1

  INTEGER(i4b)                   :: iac
  INTEGER(i4b)                   :: ii
  INTEGER(i4b)                   :: iSta
  INTEGER(i4b)                   :: jSta
  INTEGER(i4b)                   :: iCnt
  INTEGER(i4b)                   :: jCnt
  INTEGER(i4b)                   :: kCnt
  INTEGER(i4b)                   :: iRen
  INTEGER(i4b)                   :: jRen
  INTEGER(i4b)                   :: seen
  INTEGER(i4b)                   :: nRenTot
  INTEGER(i4b)                   :: nRejStaFixF
  INTEGER(i4b)                   :: nStaOutCrd
  INTEGER(i4b)                   :: iStaNum
  INTEGER(i4b)                   :: nStaInCrd
  INTEGER(i4b)                   :: nStaInVel
  INTEGER(i4b),                  &
    DIMENSION(:),POINTER         :: staNumInVel
  INTEGER(i4b),                  &
    DIMENSION(:), ALLOCATABLE    :: staNum
  INTEGER(i4b)                   :: staNumTmp
  INTEGER(i4b)                   :: prevGroup
  INTEGER(i4b)                   :: nReport
  INTEGER(i4b)                   :: nRenStaFile
  INTEGER(i4b)                   :: nCnstrStaFile
  INTEGER(i4b)                   :: nOutStaFile
  INTEGER(i4b)                   :: nTotSta
  INTEGER(i4b)                   :: nOutFixStaAft

  REAL(r8b)                      :: deltaMjd
  REAL(r8b)                      :: percObs
  REAL(r8b)                      :: tMjdInCrd
  REAL(r8b),                     &
    DIMENSION(:,:), POINTER      :: xStaInCrd
  REAL(r8b),                     &
    DIMENSION(:,:), POINTER      :: xStaInVel
  REAL(r8b),                     &
    DIMENSION(:,:), ALLOCATABLE  :: xStat
  REAL(r8b),                     &
    DIMENSION(:,:), ALLOCATABLE  :: xVelo
  REAL(r8b),DIMENSION(3)         :: xStatTmp
  REAL(r8b),DIMENSION(3)         :: xVeloTmp
  REAL(r8b),DIMENSION(3)         :: sigma

! Call debug routine
! ------------------
!  CALL debug_entry(srName)

! Initialization of all variables
! -------------------------------

  ! Return if the output STA file is not present.
  IF( LEN_TRIM(opt%outStaFileForAddneq2) == 0 )THEN
!  CALL debug_exit(srName)
     RETURN
  END IF

  ! Initializations
  nRejStaFixF  = 0
  outFixF%nSta = 0
  tMjdInCrd = 0.0
  nStaInCrd = 0
  nStaInVel = 0
  inFixF%nSta = 0
  seen = 0
  NULLIFY(xStaInCrd)
  NULLIFY(xStaInVel)
  NULLIFY(staNamInVel)
  NULLIFY(staNumInVel)
  NULLIFY(staPlateInVel)
  NULLIFY(staNamInCrd)
  NULLIFY(staFlgInCrd)
  NULLIFY(staFlgInVel)
  datstr = ''

  ! Total number of sta to allocate memory (more stations than worst case)
  nTotSta = 0
  DO iSta = 1,sCore%nSta
     nTotSta = nTotSta + sCore%sta(iSta)%upd%nRen
  END DO
  IF(  LEN_TRIM(opt%inCrdFileForAddneq2) /= 0 )THEN
     CALL getco3(opt%inCrdFileForAddneq2,1,(/'@'/), &
          nStat=nStaInCrd,stName=staNamInCrd)
  END IF
  nTotSta = nTotSta + nStaInCrd
  IF(  LEN_TRIM(opt%inVelFileForAddneq2) /= 0 )THEN
     CALL getco3(opt%inVelFileForAddneq2,1,(/'@'/), &
          nStat=nStaInVel,stName=staNamInVel )
  END IF
  nTotSta = nTotSta + nStaInVel

  ! Memory allocation for report
  nReport = sCore%nSta
  ALLOCATE(report(sCore%nSta),stat=iac)
  CALL alcerr(iac, 'report',(/sCore%nSta/),srName)
  DO ii = 1,sCore%nSta
    NULLIFY(report(ii)%ren)
  ENDDO
  ! Memory allocation for report
  ALLOCATE(statfid(sCore%nSta),stat=iac)
  CALL alcerr(iac, 'statfid',(/sCore%nSta/),srName)

  ! Add constrains into the STA-file TYPE 004
  nCnstrStaFile = 0
  DO iSta = 1,sCore%nSta
     prevGroup = 0
     prevSta1  = '  '
     DO iRen = 1,sCore%sta(iSta)%upd%nRen
        ! Memorize the first renaming (station) of the actual group
        IF( sCore%sta(iSta)%upd%ren(iRen)%group > prevGroup )THEN
           prevGroup = sCore%sta(iSta)%upd%ren(iRen)%group
           prevSta1 = sCore%sta(iSta)%upd%ren(iRen)%rename
        END IF
        ! Set relative velocity constraints
        IF( sCore%sta(iSta)%upd%ren(iRen)%cnstr == veloRelCstr )THEN
           nCnstrStaFile = nCnstrStaFile + 1
           datFil%staOut%ncoovel = datFil%staOut%ncoovel + 1
           ! Station 1
           datFil%staOut%coovel(datFil%staOut%ncoovel)%stanam(1) = &
                sCore%sta(iSta)%name
           datFil%staOut%coovel(datFil%staOut%ncoovel)%stanam(1)(15:16) = &
                prevSta1
           ! Station 2
           datFil%staOut%coovel(datFil%staOut%ncoovel)%stanam(2) = &
                sCore%sta(iSta)%name
           datFil%staOut%coovel(datFil%staOut%ncoovel)%stanam(2)(15:16) = &
                sCore%sta(iSta)%upd%ren(iRen)%rename
           ! Constraints
           datFil%staOut%coovel(datFil%staOut%ncoovel)%constr(1:3) = &
                opt%gvarCstrCrd
           datFil%staOut%coovel(datFil%staOut%ncoovel)%constr(4:6) = &
                opt%gvarCstrVel
        END IF
     END DO
  END DO

  ! Collect the information for the report
  nRenStaFile = 0
  DO iSta = 1,sCore%nSta
     ! Memory allocation for report
     report(iSta)%nRen = sCore%sta(iSta)%upd%nRen
     ALLOCATE(report(iSta)%ren(report(iSta)%nRen),stat=iac)
     CALL alcerr(iac, 'report(iSta)%ren',(/report(iSta)%nRen/),srName)
     ! Station renaming and velocity constraints
     DO iRen = 1,sCore%sta(iSta)%upd%nRen
        nRenStaFile = nRenStaFile + 1
        ! Init
        report(iSta)%ren(iRen)%years = 0.0D0
        report(iSta)%ren(iRen)%perc  = 0.0D0
        report(iSta)%ren(iRen)%fidAft = 'A'
        ! Rename
        report(iSta)%ren(iRen)%name   = sCore%sta(iSta)%name
        report(iSta)%ren(iRen)%rename = sCore%sta(iSta)%name
        report(iSta)%ren(iRen)%rename(15:16) = &
             sCore%sta(iSta)%upd%ren(iRen)%rename
        ! Report of group
        report(iSta)%ren(iRen)%group = sCore%sta(iSta)%upd%ren(iRen)%group
        ! Epoch
        CALL timst2(1,1,sCore%sta(iSta)%upd%ren(iRen)%timint%t(1),datstr)
        report(iSta)%ren(iRen)%epochRenameTxt = datstr
        CALL timst2(1,1,sCore%sta(iSta)%upd%ren(iRen)%timint%t(2),datstr)
        report(iSta)%ren(iRen)%epochEndRenameTxt = datstr
        ! Type
        IF( sCore%sta(iSta)%upd%ren(iRen)%type == typeJump )THEN
           report(iSta)%ren(iRen)%typeTxt = 'Disc'
        ELSE IF( sCore%sta(iSta)%upd%ren(iRen)%type == typeVelo )THEN
           report(iSta)%ren(iRen)%typeTxt = 'Velo'
        ELSE
           report(iSta)%ren(iRen)%typeTxt = 'DiVe'
        END IF
        ! Info
        IF     ( sCore%sta(iSta)%upd%ren(iRen)%info == infoErqF )THEN
           report(iSta)%ren(iRen)%infoTxt = 'ErqF'
        ELSE IF( sCore%sta(iSta)%upd%ren(iRen)%info == infoEvlF )THEN
           report(iSta)%ren(iRen)%infoTxt = 'EvlF'
        ELSE IF( sCore%sta(iSta)%upd%ren(iRen)%info == infoStaF )THEN
           report(iSta)%ren(iRen)%infoTxt = 'StaF'
        ELSE IF( sCore%sta(iSta)%upd%ren(iRen)%info == infoPanl )THEN
           report(iSta)%ren(iRen)%infoTxt = 'Panl'
        ELSE
           report(iSta)%ren(iRen)%infoTxt = 'Unkw'
        END IF
        ! Remark
        report(iSta)%ren(iRen)%remark = sCore%sta(iSta)%upd%ren(iRen)%remark
     END DO
     ! Interval
     report(iSta)%years = sCore%sta(iSta)%upd%maxTimeInterv / 365.25D0
     ! Perc
     report(iSta)%perc = sCore%deltaMjd * REAL(sCore%sta(iSta)%ts%nMjd)/ &
          ( sCore%sta(iSta)%ts%mjd(sCore%sta(iSta)%ts%nMjd) - &
          sCore%sta(iSta)%ts%mjd(1) )
  END DO

  ! Update CRD and VEL files according to the above renaming
  ! Allocate arrays to write the CRD and VEL files
  ALLOCATE(staNam(nTotSta),stat=iac)
  CALL alcerr(iac,'staNam',(/nTotSta/),srName)
  ALLOCATE(staNum(nTotSta),stat=iac)
  CALL alcerr(iac,'staNum',(/nTotSta/),srName)
  ALLOCATE(xStat(3,nTotSta),stat=iac)
  CALL alcerr(iac,'xStat',(/3,nTotSta/),srName)
  ALLOCATE(crdFlg(nTotSta),stat=iac)
  CALL alcerr(iac,'crdFlg',(/nTotSta/),srName)
  ALLOCATE(velFlg(nTotSta),stat=iac)
  CALL alcerr(iac,'velFlg',(/nTotSta/),srName)
  ALLOCATE(xVelo(3,nTotSta),stat=iac)
  CALL alcerr(iac,'xVelo',(/3,nTotSta/),srName)
  ALLOCATE(plate(nTotSta),stat=iac)
  CALL alcerr(iac,'plate',(/nTotSta/),srName)
  IF(  LEN_TRIM(opt%inCrdFileForAddneq2 ) /= 0 .AND. &
       LEN_TRIM(opt%outCrdFileForAddneq2) /= 0 .AND. &
       LEN_TRIM(opt%inVelFileForAddneq2 ) /= 0 .AND. &
       LEN_TRIM(opt%outVelFileForAddneq2) /= 0 )THEN
     ! Read the file opt%inCrdFileForAddneq2 (input CRD to update)
     CALL getco3(opt%inCrdFileForAddneq2,1,(/'@'/), &
          nStat=nStaInCrd,stName=staNamInCrd,&
          staFlg=staFlgInCrd, &
          datum=sCore%datumName,timcrd=tMjdInCrd,&
          xStat=xStaInCrd)
     ! Read the file opt%inVelFileForAddneq2 (input VEL to update)
     CALL getco3(opt%inVelFileForAddneq2,1,(/'@'/), &
          nStaInVel,staNamInVel,&
          staNum=staNumInVel,staFlg=staFlgInVel, &
          xStat=xStaInVel,plate=staPlateInVel )
     ! Copy the file opt%inCrdFileForAddneq2 to opt%outCrdFileForAddneq2
     ! ( copy the input CRD and VEL to update into the updated output
     ! CRD and VEL)
     nStaOutCrd = 0
     DO iSta = 1,nStaInCrd
        DO jSta = 1,nStaInVel
           IF( staNamInCrd(iSta) == staNamInVel(jSta) )THEN
              nStaOutCrd = nStaOutCrd + 1
              staNam(nStaOutCrd)  = staNamInCrd(iSta)
              staNum(nStaOutCrd)  = nStaOutCrd
              crdFlg(nStaOutCrd)  = staFlgInCrd(iSta)
              xStat(:,nStaOutCrd) = xStaInCrd(:,iSta)
              velFlg(nStaOutCrd)  = staFlgInVel(jSta)
              xVelo(:,nStaOutCrd) = xStaInVel(:,jSta)
              plate(nStaOutCrd)   = staPlateInVel(jSta)
           END IF
        END DO
     END DO
     ! Add the renamed stations to the CRD and VEL files
     ! Information taken from STA-file TYPE 001
     DO jSta = 1,datFil%staOut%nrenam
        ! Check whether jSta of TYPE 001 has been analyzed
        seen = 0
        DO iSta = 1,sCore%nSta
           IF( datFil%staOut%renamsta(jSta)%stanam(1:14) == &
               sCore%sta(iSta)%name(1:14) )THEN
              seen = 1
              EXIT
           END IF
        END DO
        IF( seen == 0 )CYCLE
        ! Check whether jSta of TYPE 001 is already in CRD and VEL
        seen = 0
        DO iSta = 1,nStaOutCrd
           IF( datFil%staOut%renamsta(jSta)%stanam == staNam(iSta) )THEN
              seen = 1
              EXIT
           END IF
        END DO
        IF( seen == 1 )CYCLE
        ! Station not yet in CRD/VEL files, therefore add it to them
        DO iSta = 1,nStaOutCrd
           IF( datFil%staOut%renamsta(jSta)%stanam(1:14) == &
               staNam(iSta)(1:14) )THEN
              nStaOutCrd  = nStaOutCrd + 1
              staNam(nStaOutCrd)  = datFil%staOut%renamsta(jSta)%stanam
              staNum(nStaOutCrd)  = nStaOutCrd
              xStat(:,nStaOutCrd) = xStat(:,iSta)
              xVelo(:,nStaOutCrd) = xVelo(:,iSta)
              crdFlg(nStaOutCrd)  = 'F'
              velFlg(nStaOutCrd)  = 'F'
              plate(nStaOutCrd)   = plate(iSta)
              EXIT
           END IF
        END DO
     END DO
     ! Sort the station alphabetically
     DO iCnt = 1,nStaOutCrd-1
        kCnt = iCnt
        DO jCnt = iCnt+1,nStaOutCrd
           IF(  staNam(jCnt) < staNam(kCnt) )THEN
              kCnt = jCnt
           ENDIF
        ENDDO
        IF( kCnt /= iCnt )THEN
           staNamTmp = staNam(iCnt)
           staNumTmp = staNum(iCnt)
           xStatTmp  = xStat(:,iCnt)
           xVeloTmp  = xVelo(:,iCnt)
           crdFlgTmp = crdFlg(iCnt)
           velFlgTmp = velFlg(iCnt)
           plateTmp  = plate(iCnt)

           staNam(iCnt)  = staNam(kCnt)
           staNum(iCnt)  = staNum(kCnt)
           xStat(:,iCnt) = xStat(:,kCnt)
           xVelo(:,iCnt) = xVelo(:,kCnt)
           velFlg(iCnt)  = velFlg(kCnt)
           crdFlg(iCnt)  = crdFlg(kCnt)
           plate(iCnt) = plate(kCnt)

           staNam(kCnt)  = staNamTmp
           staNum(kCnt)  = staNumTmp
           xStat(:,kCnt) = xStatTmp
           xVelo(:,kCnt) = xVeloTmp
           velFlg(kCnt)  = velFlgTmp
           crdFlg(kCnt)  = crdFlgTmp
           plate(kCnt)   = plateTmp
        ENDIF
     ENDDO
     ! Renumber the stations with a progressiv number
     iStaNum = 1
     DO iCnt = 1,nStaOutCrd-1
        staNum(iCnt) = iStaNum
        IF( staNam(iCnt)(1:14) == staNam(iCnt+1)(1:14) )CYCLE
        iStaNum = iStaNum + 1
     END DO
     staNum(nStaOutCrd) = iStaNum
     ! Write to files
     WRITE(filTitle,'(A80)') 'FODITS'
     ! Write CRD file
     CALL wtstat(1,opt%outCrdFileForAddneq2,filTitle,sCore%datumName, &
          nStaOutCrd,staNam,xStat,staNum,crdFlg,timcrd=tMjdInCrd)
     ! Write VEL file
     CALL wtstat(1,opt%outVelFileForAddneq2,filTitle,sCore%datumName, &
          nStaOutCrd,staNam,xVelo,staNum,velFlg,plate=plate)
  END IF

  ! Update the FIX and SIG files
  ! The list of original core stations is extended with the same renamed sites
  ! In addition, the new extended list is screened by two criteria:
  ! - minimal interval length
  ! - minimal percent data
  ! Count the number of renamings
  nRenTot = 0
  DO iSta = 1,sCore%nSta
     nRenTot = nRenTot + sCore%sta(iSta)%upd%nRen
  END DO
  ALLOCATE(inFixF%staNam(nTotSta),stat=iac)
  CALL alcerr(iac, 'inFixF%staNam',(/nTotSta/),srName)
  ALLOCATE(outFixF%staNam(nRenTot),stat=iac)
  CALL alcerr(iac, 'outFixF%staNam',(/nRenTot/),srName)
  ALLOCATE(inFixF%sigma(3,nTotSta),stat=iac)
  CALL alcerr(iac, 'inFixF%sigma',(/3,nTotSta/),srName)
  ALLOCATE(outFixF%sigma(3,nTotSta),stat=iac)
  CALL alcerr(iac, 'outFixF%sigma',(/3,nTotSta/),srName)
  IF( ( LEN_TRIM(opt%inFixFileForAddneq2 ) /= 0 .AND. &
        LEN_TRIM(opt%outFixFileForAddneq2) /= 0 ) .OR. &
      ( LEN_TRIM(opt%inSigFileForAddneq2 ) /= 0 .AND. &
        LEN_TRIM(opt%outSigFileForAddneq2) /= 0 ) )THEN
     ! Read the input SIG file
     IF     ( LEN_TRIM(opt%inSigFileForAddneq2)  /= 0 )THEN
        CALL readstsg(opt%inSigFileForAddneq2,3,inFixF)
     ! Read the input FIX file
     ELSE IF( LEN_TRIM(opt%inFixFileForAddneq2)  /= 0 )THEN
        CALL readstsg(opt%inFixFileForAddneq2,0,inFixF)
     END IF
     ! Extend the original list of core sites by renamed stations
     DO iSta = 1,sCore%nSta
        ! Cycle if iSta is not in the original list of core stations
        sigma(:) = 0.0D0
        seen = 0
        DO jSta = 1,inFixF%nSta
           IF( inFixF%staNam(jSta) == sCore%sta(iSta)%name )THEN
              seen = jSta
              IF( LEN_TRIM(opt%inSigFileForAddneq2 ) /= 0 )THEN
                 sigma(:) = inFixF%sigma(:,seen)
              END IF
              EXIT
           END IF
        END DO
        IF( seen == 0 )CYCLE
        ! For each renamed station check the criteria
        DO iRen = 1,sCore%sta(iSta)%upd%nRen
           seen = 1
           ! Minimal interval length of the group of sub-intervals with
           ! the same relative velocity constraints
           deltaMjd = sCore%sta(iSta)%upd%ren(iRen)%groupLength
           IF( deltaMjd < opt%minTimeIntLength )seen = 0
           ! Minimal percentual of data in the group of sub-intervals with
           ! the same relative velocity constraints
           percObs = REAL( sCore%sta(iSta)%upd%ren(iRen)%groupNumData / &
                REAL( sCore%sta(iSta)%upd%ren(iRen)%groupLength / &
                      sCore%deltaMjd ) ) * 100.0D0
           IF( percObs > 100.0D0 ) percObs = 100.0D0
           IF( percObs < opt%minPercDataFix )seen = 0
           ! Add the station iSta as well as all renamed stations as core sta.
           IF( seen == 1 )THEN
              outFixF%nSta = outFixF%nSta + 1
              outFixF%staNam(outFixF%nSta) = sCore%sta(iSta)%name
              outFixF%staNam(outFixF%nSta)(15:16) = &
                   sCore%sta(iSta)%upd%ren(iRen)%rename
              IF( LEN_TRIM(opt%inSigFileForAddneq2 ) /= 0 )THEN
                 outFixF%sigma(:,seen) = sigma(:)
              END IF
           END IF
           ! Report
           DO jRen = 1,report(iSta)%nRen
              IF( report(iSta)%ren(jRen)%rename(15:16) == &
                  sCore%sta(iSta)%upd%ren(iRen)%rename )THEN
                 report(iSta)%ren(jRen)%years = deltaMjd / 365.25D0
                 report(iSta)%ren(jRen)%perc = percObs
                 report(iSta)%ren(jRen)%fidAft = 'Y'
                 IF( seen == 0 )THEN
                    report(iSta)%ren(jRen)%fidAft = 'N'
                 END IF
                 EXIT
              END IF
           END DO
        END DO
     END DO
     ! Define status station for statistics
     DO iSta = 1,sCore%nSta
        ! Default
        sCore%sta(iSta)%upd%staStatusRef = 0
        DO jSta = 1,inFixF%nSta
           IF( inFixF%staNam(jSta) == sCore%sta(iSta)%name )THEN
              sCore%sta(iSta)%upd%staStatusRef = 1
              EXIT
           END IF
        END DO
        DO jSta = 1,outFixF%nSta
           seen = 0
           IF(  sCore%sta(iSta)%upd%staStatusRef == 1 .AND. &
                outFixF%staNam(jSta) == sCore%sta(iSta)%name )THEN
              seen = 1
              EXIT
           END IF
        END DO
        IF( seen == 0 .AND. sCore%sta(iSta)%upd%staStatusRef == 1 )THEN
           sCore%sta(iSta)%upd%staStatusRef = 2
        END IF
     END DO
     ! Copy the title
     outFixF%title = inFixF%title
     ! Write the input FIX file
     IF( LEN_TRIM(opt%inFixFileForAddneq2)  /= 0 )THEN
        CALL writstsg(opt%outfixFileForAddneq2, 0, outFixF)
     END IF
     ! Write the new SIG File
     IF( LEN_TRIM(opt%inSigFileForAddneq2)  /= 0 )THEN
        CALL writstsg(opt%outSigFileForAddneq2, 3, outFixF)
     END IF
  END IF
  ! Report
  DO iSta = 1,sCore%nSta
     report(iSta)%fidBef = 'N'
     IF( sCore%sta(iSta)%upd%staStatusRef > 0 )THEN
        report(iSta)%fidBef = 'Y'
     END IF
     report(iSta)%fidAft = 'N'
     IF( sCore%sta(iSta)%upd%staStatusRef == 1 )THEN
        report(iSta)%fidAft = 'Y'
     END IF
  END DO
  ! Number sta after screening
  nOutFixStaAft = 0
  DO jSta = 1,outFixF%nSta
     IF( outFixF%staNam(jSta)(15:16) == '  ' )THEN
        nOutFixStaAft = nOutFixStaAft + 1
     END IF
  END DO

  ! Count the number of outliers
  nOutStaFile = 0
  DO iSta = 1,sCore%nSta
     DO iCnt = 1,sCore%sta(iSta)%mod%nEvnt
        IF( sCore%sta(iSta)%mod%evnt(iCnt)%type /= typeOutl )CYCLE
        IF( sCore%sta(iSta)%mod%evnt(iCnt)%siTst /= 1 )CYCLE
        nOutStaFile = nOutStaFile + 1
     END DO
  END DO

  ! Write velocity constraint information
  IF( LEN_TRIM(opt%inStaFile) /= 0 )THEN
     ! Title
     WRITE(lfnprt,'(/,/,2(A,/))')  &
          ' MODEL UPDATE FOR ADDNEQ2', &
          ' ------------------------'
     WRITE(lfnprt,'(A,/,A)')  &
          ' Update of reference stations:', &
          ' -----------------------------'
     WRITE(lfnprt,'(A,1X,I4)')  &
     ' Number of fidicial sites before FODITS analysis     :', inFixF%nSta
     nRejStaFixF = inFixF%nSta - nOutFixStaAft
     WRITE(lfnprt,'(A,1X,I4)')  &
     ' Number of rejected fiducial sites                   :', nRejStaFixF
     WRITE(lfnprt,'(A,1X,I4)')  &
     ' Number of fidicial sites after FODITS analysis      :', nOutFixStaAft
     WRITE(lfnprt,'(A,1X,I4)')  &
     ' Number of resulting fidicial sub-interval sites     :', outFixF%nSta
     WRITE(lfnprt,'(A,1X,I4)')  &
     ' Number of renamed stations in STA file              :', nRenStaFile
     WRITE(lfnprt,'(A,I5)')  &
     ' Number of outliers in STA file                      :', nOutStaFile
     WRITE(lfnprt,'(A,1X,I4)')  &
     ' Number of relative velocity constraints in STA file :', nCnstrStaFile


     WRITE(lfnprt,'(/,A,/,A)')  &
     ' Update details concerning reference stations:', &
     ' ---------------------------------------------'
     WRITE(lfnprt,'(A)')  &
     ' Legend:  Group of relative velocity constraints (Gr), 1..nGr'
     WRITE(lfnprt,'(A)')  &
     '          R: reference station (Y), reference station rejected (N), &
     &(A) no original reference station,'
     WRITE(lfnprt,'(A)')  &
     '          year G: interval length of the group of sub-intervals &
     & with same relative velocity constraints,'
     WRITE(lfnprt,'(A)')  &
     '          perc G: percentual observations in the group of sub-intervals &
     & with same relative velocity constraints,'
     WRITE(lfnprt,'(A)')  &
     ' --------------------------------------------&
     &-----------------------------------------------------'
     WRITE(lfnprt,'(A)')  &
     '  Nr Station Sub-Int. | From                 &
     &To                  | Gr | R | year G | perc G | KYWD'
     WRITE(lfnprt,'(A)')  &
     ' --------------------------------------------&
     &-----------------------------------------------------'
     DO iSta = 1,nReport
        DO iRen = 1,report(iSta)%nRen
           WRITE(lfnprt,'(I04,1X,A16,1X,A1,&
                          &1X,A20,1X,A20,A1,&
                          &1X,I2,1X,A1,&
                          &1X,A1,1X,A1,&
                          &1X,F6.3,1X,A1,&
                          &1X,F6.2,1X,A1,&
                          &1X,A4)') &
             iSta, report(iSta)%ren(iRen)%rename, '|', &
             report(iSta)%ren(iRen)%epochRenameTxt, &
             report(iSta)%ren(iRen)%epochEndRenameTxt, '|', &
             report(iSta)%ren(iRen)%group, '|', &
             report(iSta)%ren(iRen)%fidAft, '|', &
             report(iSta)%ren(iRen)%years, '|', &
             report(iSta)%ren(iRen)%perc, '|', &
             'USTF'
        END DO
     END DO
  END IF

  ! Deallocate arrays to write the CRD and VEL files
  DEALLOCATE(staNam, stat=iac)
  DEALLOCATE(staNum, stat=iac)
  DEALLOCATE(xStat, stat=iac)
  DEALLOCATE(xVelo, stat=iac)
  DEALLOCATE(plate, stat=iac)
  DEALLOCATE(crdFlg, stat=iac)
  DEALLOCATE(velFlg, stat=iac)
  DEALLOCATE(inFixF%staNam,stat=iac)
  DEALLOCATE(outFixF%staNam,stat=iac)
  DEALLOCATE(inFixF%sigma,stat=iac)
  DEALLOCATE(outFixF%sigma,stat=iac)
  DEALLOCATE(report,stat=iac)
  DEALLOCATE(statfid,stat=iac)

! End of subroutine
! -----------------
!  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodiupdf

END MODULE s_FODIUPDF

