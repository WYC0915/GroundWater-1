MODULE s_FODIDDEF
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodiddef(opt,sCore)

! -------------------------------------------------------------------------
! Purpose:    Check the datum definition.
!
! Author:     Luca Ostini
!
! Created:    19-Jul-2011
!
! Changes:    19-Jul-2011 LO: Created this file
!             28-Apr-2012 RD: Nullify all pointers
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used Modules
! ------------
! structures, variables:
  USE m_bern,    ONLY: i4b, r8b, staNameLength, fileNameLength

  USE m_maxdim,  ONLY: maxsta

  USE d_datum,   ONLY: datum
  USE d_stalst,  ONLY: t_staList

! operator, methods:
!  USE d_debug,   ONLY: debug_entry, debug_exit

  USE p_fodits,  ONLY: t_opt, t_sCore, t_lsa,&
                       inpRes, lfnprt

  USE s_fodislsa
  USE s_getco3
  USE s_getdat
  USE s_readstsg
  USE s_eccell
  USE s_alcerr
  USE s_hlmtra

! no implicit
  IMPLICIT NONE

! subroutine name
  CHARACTER(LEN=8), PARAMETER    :: srName = 'fodiddef'

! List of Arguments
! -----------------
! input:
  TYPE(t_opt)                    :: opt        ! Option structure
  TYPE(t_sCore)                  :: sCore      ! Core structure of FODITS

! input/output:

! output:


! Local Types
! -----------
  TYPE t_RefSta
    INTEGER(i4b)                 :: nSta
    CHARACTER(LEN=staNameLength),&
               DIMENSION(:),   &
               POINTER           :: name
    REAL(r8b)                    :: refMjd
    REAL(r8b), DIMENSION(:,:),   &
               POINTER           :: crd
    REAL(r8b), DIMENSION(:,:),   &
               POINTER           :: vel
    REAL(r8b), DIMENSION(:,:),   &
               POINTER           :: crdVel
    REAL(r8b), DIMENSION(:,:),   &
               POINTER           :: crdVelRef
    REAL(r8b), DIMENSION(:,:),   &
               POINTER           :: res
    INTEGER(i4b),                &
               DIMENSION(:),     &
               POINTER           :: ref
    INTEGER(i4b),                &
               DIMENSION(:),     &
               POINTER           :: refReport
  END TYPE t_RefSta

! Local Parameters
! ----------------

! Local Variables
! ---------------
  TYPE(t_lsa)                    :: lsa

  CHARACTER(LEN=1),DIMENSION(:), &
                   POINTER       :: xStaFlgCrd
  CHARACTER(LEN=1),DIMENSION(:), &
                   POINTER       :: xStaFlgVel
  CHARACTER(LEN=4),DIMENSION(:), &
                   POINTER       :: xStaPlate
  CHARACTER(LEN=staNameLength),  &
    DIMENSION(:),POINTER         :: staNamInCrd
  CHARACTER(LEN=staNameLength),  &
    DIMENSION(:),POINTER         :: staNamInVel

  INTEGER(i4b)                   :: iac
  INTEGER(i4b)                   :: iPar
  INTEGER(i4b)                   :: indM
  INTEGER(i4b)                   :: iVal
  INTEGER(i4b)                   :: nVal
  INTEGER(i4b)                   :: iSta
  INTEGER(i4b)                   :: jSta
  INTEGER(i4b)                   :: iMjd
  INTEGER(i4b)                   :: jMjd
  INTEGER(i4b)                   :: iRefSta
  INTEGER(i4b)                   :: iStaTmp
  INTEGER(i4b)                   :: jStaTmp
  INTEGER(i4b)                   :: nStaRef
  INTEGER(i4b)                   :: nStaTmp
  INTEGER(i4b)                   :: nStaInCrd
  INTEGER(i4b)                   :: nStaInVel
  INTEGER(i4b), DIMENSION(:),    &
                POINTER          :: staNumIn
  INTEGER(i4b), DIMENSION(7)     :: hlmPar
  INTEGER(i4b)                   :: nStaRej
  INTEGER(i4b)                   :: iIterRej
  INTEGER(i4b)                   :: nParHlm
  INTEGER(i4b)                   :: nCrdHlm
  INTEGER(i4b)                   :: nIterRej
  INTEGER(i4b)                   :: maxResInd

  REAL(r8b)                      :: mjdHlmTra
  REAL(r8b), DIMENSION(:,:),     &
             POINTER             :: xStaInCrd
  REAL(r8b), DIMENSION(:,:),     &
             POINTER             :: xStaInVel
  REAL(r8b)                      :: halfDeltaMjd
  REAL(r8b), DIMENSION(3)        :: staResNeu
  REAL(r8b)                      :: rmsHlm
  REAL(r8b), DIMENSION(7)        :: parHlm
  REAL(r8b), DIMENSION(7)        :: rmsParHlm
  REAL(r8b)                      :: valResNeu
  REAL(r8b)                      :: maxResNeu

  TYPE(t_RefSta)                 :: refSta
  TYPE(t_RefSta)                 :: actSta
  TYPE(t_staList)                :: inFixF

! Call debug routine
! ------------------
!  CALL debug_entry(srName)

! Nullify pointers
! ----------------
  NULLIFY(xStaFlgCrd)
  NULLIFY(xStaFlgVel)
  NULLIFY(xStaPlate)
  NULLIFY(staNamInCrd)
  NULLIFY(staNamInVel)
  NULLIFY(staNumIn)
  NULLIFY(xStaInCrd)
  NULLIFY(xStaInVel)

  NULLIFY(refSta%name)
  NULLIFY(refSta%crd)
  NULLIFY(refSta%vel)
  NULLIFY(refSta%crdVel)
  NULLIFY(refSta%crdVelRef)
  NULLIFY(refSta%res)
  NULLIFY(refSta%ref)
  NULLIFY(refSta%refReport)

  NULLIFY(actSta%name)
  NULLIFY(actSta%crd)
  NULLIFY(actSta%vel)
  NULLIFY(actSta%crdVel)
  NULLIFY(actSta%crdVelRef)
  NULLIFY(actSta%res)
  NULLIFY(actSta%ref)
  NULLIFY(actSta%refReport)

! Cheak all inputs are present
  IF(  opt%selInpTsType == inpRes .AND. ( &
       LEN_TRIM(opt%inPltFile) == 0 .OR. &
       LEN_TRIM(opt%inCrdFile) == 0 .OR. &
       LEN_TRIM(opt%inVelFile) == 0    ) )THEN
     WRITE(lfnprt,'(/,A,/)') ' ### SR FODIDDEF: One of the files is missing.'
     !  CALL debug_exit(srName)
     RETURN
  END IF

! General of Helmert transformation
! ---------------------------------
  ALLOCATE(sCore%ddf%mjd(sCore%nTotMjd),stat=iac)
  CALL alcerr(iac,'sCore%ddf%mjd',(/sCore%nTotMjd/),srName)
  ALLOCATE(sCore%ddf%hlmPar(sCore%nTotMjd,7),stat=iac)
  CALL alcerr(iac,'sCore%ddf%hlmPar',(/sCore%nTotMjd,7/),srName)
  ALLOCATE(sCore%ddf%rmsHlmPar(sCore%nTotMjd,7),stat=iac)
  CALL alcerr(iac,'sCore%ddf%rmsHlmPar',(/sCore%nTotMjd,7/),srName)
  ALLOCATE(sCore%ddf%rmsHlmTra(sCore%nTotMjd),stat=iac)
  CALL alcerr(iac,'sCore%ddf%rmsHlmTra',(/sCore%nTotMjd/),srName)
  ALLOCATE(sCore%ddf%totSta(sCore%nTotMjd),stat=iac)
  CALL alcerr(iac,'sCore%ddf%totSta',(/sCore%nTotMjd/),srName)
  ALLOCATE(sCore%ddf%refSta(sCore%nTotMjd),stat=iac)
  CALL alcerr(iac,'sCore%ddf%refSta',(/sCore%nTotMjd/),srName)
  ALLOCATE(sCore%ddf%rejSta(sCore%nTotMjd),stat=iac)
  CALL alcerr(iac,'sCore%ddf%rejSta',(/sCore%nTotMjd/),srName)

! Initialization of all variables
! -------------------------------
  sCore%ddf%mjd(:)         = 0.0D0
  sCore%ddf%hlmPar(:,:)    = 0.0D0
  sCore%ddf%rmsHlmPar(:,:) = 0.0D0
  sCore%ddf%rmsHlmTra(:)   = 0.0D0
  sCore%ddf%totSta(:)      = 0
  sCore%ddf%refSta(:)      = 0
  sCore%ddf%rejSta(:)      = 0

  inFixF%nSta = 0

  halfDeltaMjd = sCore%deltaMjd / 2.0D0
  IF( halfDeltaMjd > 1.0D0 ) halfDeltaMjd = CEILING( halfDeltaMjd )

  NULLIFY(inFixF%staNam)
  actSta%nSta = 0
  hlmPar(:) = (/opt%datHlmTx,opt%datHlmTy,opt%datHlmTz,&
                opt%datHlmRx,opt%datHlmRy,opt%datHlmRz,&
                opt%datHlmSc/)

  ! Allocate variables actSta for the Helmert transformation
  ALLOCATE(actSta%name(maxsta),stat=iac)
  CALL alcerr(iac,'actSta%name',(/maxsta/),srName)
  ALLOCATE(actSta%crd(3,maxsta),stat=iac)
  CALL alcerr(iac,'actSta%crd',(/3,maxsta/),srName)
  ALLOCATE(actSta%vel(3,maxsta),stat=iac)
  CALL alcerr(iac,'actSta%vel',(/3,maxsta/),srName)
  ALLOCATE(actSta%crdVel(3,maxsta),stat=iac)
  CALL alcerr(iac,'actSta%crdVel',(/3,maxsta/),srName)
  ALLOCATE(actSta%crdVelRef(3,maxsta),stat=iac)
  CALL alcerr(iac,'actSta%crdVelRef',(/3,maxsta/),srName)
  ALLOCATE(actSta%res(3,maxsta),stat=iac)
  CALL alcerr(iac,'actSta%res',(/3,maxsta/),srName)
  ALLOCATE(actSta%ref(maxsta),stat=iac)
  CALL alcerr(iac,'actSta%ref',(/maxsta/),srName)
  ALLOCATE(actSta%refReport(maxsta),stat=iac)
  CALL alcerr(iac,'actSta%refReport',(/maxsta/),srName)

! Read reference files
! --------------------

  ! Read CRD ref file
  IF( LEN_TRIM(opt%inDatumCrdFile) /= 0 )THEN
     CALL getco3(opt%inDatumCrdFile,1,(/'@'/),nStaInCrd,staNamInCrd,&
          staNum=staNumIn,datum=datum%name,timcrd=refSta%refMjd,&
          staFlg=xStaFlgCrd,xStat=xStaInCrd)
  ELSE
     !  CALL debug_exit(srName)
     RETURN
  END IF

  ! Read VEL ref file
  IF( LEN_TRIM(opt%inDatumCrdFile) /= 0 .AND. &
      LEN_TRIM(opt%inDatumVelFile) /= 0 )THEN
     CALL getco3(opt%inDatumVelFile,1,(/'@'/),nStaInVel,staNamInVel,&
          staFlg=xStaFlgVel,xStat=xStaInVel,plate=xStaPlate)
  END IF

  ! Number of station (in common) in CRD and VEL ref files
  nStaTmp = 0
  DO iStaTmp = 1,nStaInCrd
     DO jStaTmp = 1,nStaInVel
        IF( staNamInCrd(iStaTmp) == staNamInVel(jStaTmp) )THEN
           nStaTmp = nStaTmp + 1
        END IF
     END DO
  END DO

  ! Allocate refSta struct to store CRD and VEL ref files
  ALLOCATE(refSta%name(nStaTmp),stat=iac)
  CALL alcerr(iac,'refSta%name',(/nStaTmp/),srName)
  ALLOCATE(refSta%crd(3,nStaTmp),stat=iac)
  CALL alcerr(iac,'refSta%crd',(/3,nStaTmp/),srName)
  ALLOCATE(refSta%vel(3,nStaTmp),stat=iac)
  CALL alcerr(iac,'refSta%vel',(/3,nStaTmp/),srName)
  ALLOCATE(refSta%crdVel(3,nStaTmp),stat=iac)
  CALL alcerr(iac,'refSta%crdVel',(/3,nStaTmp/),srName)
  ALLOCATE(refSta%ref(nStaTmp),stat=iac)
  CALL alcerr(iac,'refSta%ref',(/3,nStaTmp/),srName)

  ! Fill (allocated) refSta struct with CRD and VEL ref files info
  nStaTmp = 0
  DO iStaTmp = 1,nStaInCrd
     DO jStaTmp = 1,nStaInVel
        IF( staNamInCrd(iStaTmp) == staNamInVel(jStaTmp) )THEN
           nStaTmp = nStaTmp + 1
           refSta%name(nStaTmp)  = staNamInCrd(iStaTmp)
           refSta%crd(:,nStaTmp) = xStaInCrd(:,iStaTmp)
           refSta%vel(:,nStaTmp) = xStaInVel(:,jStaTmp)
           refSta%ref(nStaTmp)   = 1
        END IF
     END DO
  END DO
  refSta%nSta = nStaTmp

  ! Read ref FIX file
  IF( LEN_TRIM(opt%inDatumCrdFile) /= 0 .AND. &
      LEN_TRIM(opt%inDatumFixFile) /= 0 )THEN
     CALL readstsg(opt%inDatumFixFile,0,inFixF)
  END IF

  ! Store FIX file info in (allocated) refSta struct
  DO iStaTmp = 1,inFixF%nSta
     DO jStaTmp = 1,refSta%nSta
        IF( refSta%name(jStaTmp) == inFixF%staNam(iStaTmp) )THEN
           refSta%ref(jStaTmp) = 0
        END IF
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
  END DO

! Define geodetic datum
! ---------------------
  datum%name = sCore%datumName
  CALL getdat(datum%name, datum%aell, datum%bell, datum%dxell, &
              datum%drell, datum%scell)

! Check Helmert transformation for all epochs --- coordinates
! -----------------------------------------------------------

  DO iMjd = 1,sCore%nTotMjd

     ! Epoch in mjd
     mjdHlmTra = sCore%begMjd + (iMjd-1) * sCore%deltaMjd

     ! Propagate refCrd of refSta struct to mjdHlmTra by refVel
     DO iStaTmp = 1,refSta%nSta
        refSta%crdVel(:,iStaTmp) = &
             refSta%crd(:,iStaTmp) + &
             refSta%vel(:,iStaTmp) / 365.25D0 * ( mjdHlmTra - refSta%refMjd )
     END DO

     ! Get actSta%crdVel at epoch mjdHlmTra (of the Helmert transformation)
     actSta%ref(:) = 1
     actSta%refReport(:) = 0
     actSta%nSta = 0
     DO iSta = 1,sCore%nSta
        DO jMjd = 1,sCore%sta(iSta)%ts%nMjd
           IF( sCore%sta(iSta)%ts%mjd(jMjd) > mjdHlmTra-halfDeltaMjd .AND. &
               sCore%sta(iSta)%ts%mjd(jMjd) < mjdHlmTra+halfDeltaMjd )THEN
              actSta%nSta = actSta%nSta + 1
              actSta%name(actSta%nSta) = sCore%sta(iSta)%name
              actSta%crdVel(:,actSta%nSta) = sCore%sta(iSta)%ts%val(jMjd,:)
              EXIT
           END IF
        END DO
     END DO

     ! Common refSta stations actSta in actSta%crdVelRef
     ! Set up flags for fiducial stations in actSta%ref
     DO jStaTmp = 1,actSta%nSta
        DO iStaTmp = 1,refSta%nSta
           IF( refSta%name(iStaTmp) == actSta%name(jStaTmp) )THEN
              actSta%crdVelRef(:,jStaTmp) = refSta%crdVel(:,iStaTmp)
              actSta%ref(jStaTmp) = refSta%ref(iStaTmp)
              IF( refSta%ref(iStaTmp) == 0 ) actSta%refReport(jStaTmp) = 1
              EXIT
           END IF
        END DO
     END DO
     IF( LEN_TRIM(opt%inDatumFixFile) == 0 )THEN
        actSta%ref(:) = 0
        actSta%refReport(:) = 1
     END IF

     ! Repeat until all outliers are rejected
     nIterRej = 0
     DO iIterRej = 1,50

        ! Test feasibility
        IF( actSta%nSta == 0 )THEN
           EXIT
        END IF

        ! Increment
        nIterRej = nIterRej + 1

        ! Helmert transformation
        CALL hlmtra(actSta%nSta,actSta%crdVel,actSta%crdVelRef,actSta%ref,&
             hlmPar,2,&
             datum%aell,datum%bell,datum%dxell,datum%drell,datum%scell,&
             actSta%res,rmsHlm,nParHlm,nCrdHlm,parHlm,rmsParHlm)

        ! Check for outliers rejection for coordinates
        nStaRej = 0
        IF( opt%datRejectCrd == 1 )THEN
           staResNeu(:) = 0.0D0
           maxResNeu = 0.0D0
           maxResInd = 0
           DO iSta = 1,sCore%nSta
              iRefSta = sCore%sta(iSta)%iRefSta
              DO jStaTmp = 1,actSta%nSta
                 ! Check only reference stations
                 IF( actSta%ref(jStaTmp) /= 0 )CYCLE
                 ! Transform res from XYZ to NEU
                 IF( sCore%sta(iSta)%name /= actSta%name(jStaTmp) )CYCLE
                 CALL eccell(sCore%sta(iSta)%staSta(iRefSta)%inCrdNeu(:),&
                             actSta%res(:,jStaTmp),staResNeu)
                 ! Test for outliers
                 IF( ABS(staResNeu(1)) > opt%datNLimitCrd .OR.  &
                     ABS(staResNeu(2)) > opt%datELimitCrd .OR.  &
                     ABS(staResNeu(3)) > opt%datULimitCrd )THEN
                    ! Compute max
                    valResNeu = staResNeu(1)**2 + &
                                staResNeu(2)**2 + &
                                staResNeu(3)**2
                    ! Reject only the max outlier
                    IF( valResNeu > maxResNeu )THEN
                       maxResNeu = valResNeu
                       maxResInd = jStaTmp
                    END IF
                    EXIT
                 END IF
              END DO
           END DO
           ! Reject only the max outlier
           IF( maxResInd > 0 )THEN
              actSta%ref(maxResInd) = 1
              actSta%refReport(maxResInd) = 2
              nStaRej = 1
           END IF
        END IF

        ! Exit if not outliers were identified
        IF( nStaRej == 0 )THEN
           EXIT
        END IF

     ! END: If outliers rejection is enabled, repeat it until necessairly
     END DO

     ! Save results --- station specific
     nStaRef = 0
     nStaRej = 0
     DO iSta = 1,sCore%nSta
        DO jStaTmp = 1,actSta%nSta
           IF( sCore%sta(iSta)%name /= actSta%name(jStaTmp) )CYCLE
           DO jMjd = 1,sCore%sta(iSta)%ts%nMjd
              IF( sCore%sta(iSta)%ts%mjd(jMjd) > mjdHlmTra-halfDeltaMjd .AND.&
                  sCore%sta(iSta)%ts%mjd(jMjd) < mjdHlmTra+halfDeltaMjd )THEN
                 ! Station Residuals
                 sCore%sta(iSta)%ts%dDef(jMjd,:) = actSta%res(:,jStaTmp)
                 ! Station fiducial/rejection information
                 sCore%sta(iSta)%ts%dDefRej(jMjd) = actSta%refReport(jStaTmp)
                 IF     ( actSta%refReport(jStaTmp) == 1 )THEN
                    nStaRef = nStaRef + 1
                 ELSE IF( actSta%refReport(jStaTmp) == 2 )THEN
                    nStaRej = nStaRej + 1
                 END IF
                 EXIT
              END IF
           END DO
        END DO
     END DO

     ! Save results --- general of Helmert transformation
     sCore%ddf%mjd(iMjd)         = mjdHlmTra
     sCore%ddf%hlmPar(iMjd,:)    = parHlm(:)
     sCore%ddf%rmsHlmPar(iMjd,:) = rmsParHlm(:)
     sCore%ddf%rmsHlmTra(iMjd)   = rmsHlm
     sCore%ddf%totSta(iMjd)      = actSta%nSta
     sCore%ddf%refSta(iMjd)      = nStaRef
     sCore%ddf%rejSta(iMjd)      = nStaRej

  ! END: Check Helmert transformation for all epochs
  END DO

  ! Compute linear regressions fro all 7 parameters
  DO iPar = 1,7

     ! Define parameters
     nVal = 1
     lsa%nnPar = 2 * nVal
     lsa%nnMjd = nVal * sCore%nTotMjd
     lsa%dof = lsa%nnMjd - lsa%nnPar

     ! Check degree of freedom
     IF( lsa%dof < 1 )THEN
        sCore%ddf%hlmParOffst(iPar) = 0.0D0
        sCore%ddf%hlmParOffstRms(iPar) = 0.0D0
        sCore%ddf%hlmParDrift(iPar) = 0.0D0
        sCore%ddf%hlmParDriftRms(iPar) = 0.0D0
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
     ALLOCATE(lsa%iQyy(sCore%nTotMjd,nVal,nVal),stat=iac)
     CALL alcerr(iac,'lsa%iQyy', (/sCore%nTotMjd,nVal,nVal/),srName)

     ! Define the first design matrix
     lsa%A(:,:) = 0.0D0
     DO iMjd = 1,sCore%nTotMjd
        indM = nVal*(iMjd-1)
        DO iVal = 1,nVal
           lsa%A(indM+iVal,iVal) = 1.0D0
           lsa%A(indM+iVal,iVal+nVal) = sCore%ddf%mjd(iMjd) - sCore%inTimRefCrd
           lsa%y(indM+iVal) = sCore%ddf%hlmPar(iMjd,iPar)
           lsa%P(indM+iVal) = ( opt%inPltFileViM0 / &
                                sCore%ddf%rmsHlmPar(iMjd,iPar) )**2
        END DO
     END DO

     ! Least squares adjustment
     CALL fodislsa( 1, lsa )

     ! Store CRD and VEL results in XYZ
     IF( lsa%detN == 0.0D0 .OR. lsa%dof <= 0 )THEN
        sCore%ddf%hlmParOffst(iPar)    = 0.0D0
        sCore%ddf%hlmParOffstRms(iPar) = 0.0D0
        sCore%ddf%hlmParDrift(iPar)    = 0.0D0
        sCore%ddf%hlmParDriftRms(iPar) = 0.0D0
        lsa%m0 = 0.0D0
     ELSE
        sCore%ddf%hlmParOffst(iPar)    = lsa%x(1)
        sCore%ddf%hlmParOffstRms(iPar) = lsa%m0 * SQRT(lsa%Qxx(1,1))
        sCore%ddf%hlmParDrift(iPar)    = lsa%x(2)
        sCore%ddf%hlmParDriftRms(iPar) = lsa%m0 * SQRT(lsa%Qxx(2,2))
     END IF

     ! Deallocation memory
     DEALLOCATE(lsa%A,stat=iac)
     DEALLOCATE(lsa%Qxx,stat=iac)
     DEALLOCATE(lsa%y,stat=iac)
     DEALLOCATE(lsa%x,stat=iac)
     DEALLOCATE(lsa%mod,stat=iac)
     DEALLOCATE(lsa%v,stat=iac)
     DEALLOCATE(lsa%P,stat=iac)
     DEALLOCATE(lsa%iQyy,stat=iac)

  ! End Loop over all parameters
  END DO

  ! Deallocate refSta struct to store CRD and VEL ref files
  DEALLOCATE(refSta%name,stat=iac)
  DEALLOCATE(refSta%crd,stat=iac)
  DEALLOCATE(refSta%vel,stat=iac)
  DEALLOCATE(refSta%crdVel,stat=iac)
  DEALLOCATE(refSta%ref,stat=iac)
  DEALLOCATE(actSta%refReport,stat=iac)

  ! Deallocate variables actSta for the Helmert transformation
  DEALLOCATE(actSta%name,stat=iac)
  DEALLOCATE(actSta%crd,stat=iac)
  DEALLOCATE(actSta%vel,stat=iac)
  DEALLOCATE(actSta%crdVel,stat=iac)
  DEALLOCATE(actSta%crdVelRef,stat=iac)
  DEALLOCATE(actSta%res,stat=iac)
  DEALLOCATE(actSta%ref,stat=iac)

! End of subroutine
! -----------------
!  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodiddef

END MODULE s_FODIDDEF
