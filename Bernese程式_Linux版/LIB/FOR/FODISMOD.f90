MODULE s_FODISMOD
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodismod(opt,sCore,iSta,lsa,amod,nAmod)

! -------------------------------------------------------------------------
! Purpose:    Set the first design matrix for the FODITS model.
!
! Author:     Luca Ostini
!
! Created:    10-May-2010
!
! Changes:    10-May-2010 LO: Created this file
!             16-Jun-2010 LO: Time series diveded by component
!             26-Aug-2010 LO: Architectural changes
!             03-Jan-2010 LO: INTENT(INOUT) removed and bug fixed
!             19-Sep-2012 RD: Use P_FODITS with ONLY
!
! Copyright:  Astronomical Institute
!             University of Berne
!             Switzerland
! -------------------------------------------------------------------------


! Used Modules
! ------------
! structures, variables:
  USE m_bern,    ONLY: i4b, r8b, lfnerr, shortlinelength
  USE m_time,    ONLY: OPERATOR(.isIn.)
  USE p_fodits,  ONLY: t_opt, t_sCore, t_lsa, t_evnt, &
                       candsub, typejump, typerate, typeperi, infoinit

! operator, methods:
!  USE d_debug,   ONLY: debug_entry, debug_exit

! subroutines, functions:
  USE f_matinv
  USE s_exitrc
  USE s_alcerr

! no implicit
  IMPLICIT NONE

! subroutine name
  CHARACTER(LEN=shortLineLength), PARAMETER    :: srName = 'fodismod'


! List of Arguments
! -----------------
! input:
  INTEGER(i4b),INTENT(IN)                         :: iSta

! input/output:
  TYPE(t_opt)                                     :: opt
  TYPE(t_sCore)                                   :: sCore
  TYPE(t_lsa)                                     :: lsa
  TYPE(t_evnt),DIMENSION(:),POINTER               :: amod
  INTEGER(i4b)                                    :: nAmod

! output:


! Local Types
! -----------


! Local Parameters
! ----------------


! Local Variables
! ---------------
  INTEGER(i4b)                   :: iac
  INTEGER(i4b)                   :: iVal
  INTEGER(i4b)                   :: iMjd
  INTEGER(i4b)                   :: jMjd
  INTEGER(i4b)                   :: seen
  INTEGER(i4b)                   :: indM
  INTEGER(i4b)                   :: indP
  INTEGER(i4b)                   :: nVal
  INTEGER(i4b)                   :: nMjd
  INTEGER(i4b)                   :: mMjd
  INTEGER(i4b)                   :: iEvnt
  INTEGER(i4b)                   :: jEvnt
  INTEGER(i4b)                   :: ret

  REAL(r8b)                      :: mjd
  REAL(r8b)                      :: omega

! Call debug routine
! ------------------
!  CALL debug_entry(srName)


! Initialization of all variables
! -------------------------------
  nVal = sCore%nVal
  nMjd = sCore%sta(iSta)%ts%nMjd

  ! Total number of pseudo-observations
  lsa%nnMjd = 0
  mMjd = 0
  DO iMjd = 1,nMjd
     IF( sCore%mdl%outlMjd(iMjd) == 0.0D0 )THEN
        mMjd = mMjd + 1
        lsa%nnMjd = lsa%nnMjd + nVal
     END IF
  END DO

  ! Total number of parameters
  lsa%nnPar = 0
  DO iEvnt = 1,nAmod
     ! Filter
     IF( amod(iEvnt)%cand == candSub )CYCLE
     ! Number of parameters per type
     IF     ( amod(iEvnt)%type == typeJump )THEN
        lsa%nnPar = lsa%nnPar + nVal
     ELSE iF( amod(iEvnt)%type == typeRate )THEN
        lsa%nnPar = lsa%nnPar + nVal
     ELSE iF( amod(iEvnt)%type == typePeri )THEN
        lsa%nnPar = lsa%nnPar + 2*nVal
     END IF
  END DO

  ! Compute the degree of freedom (dof)
  lsa%dof = lsa%nnMjd - lsa%nnPar

  ! Check the degree of freedom
  IF( lsa%dof <= 0 )THEN
     !  CALL debug_exit(srName)
     RETURN
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
  ! Diagonal weight matrix (P)
  ALLOCATE(lsa%P(lsa%nnMjd),stat=iac)
  CALL alcerr(iac,'lsa%P',(/lsa%nnMjd/),srName)
  lsa%P(:)  = 0.0D0
  ! Variance-Covariance matrix P = inv(Qyy) = iQyy (if present)
  ALLOCATE(lsa%iQyy(mMjd,3,3),stat=iac)
  CALL alcerr(iac,'lsa%iQyy',(/mMjd,3,3/),srName)
  lsa%iQyy(:,:,:) = 0.0D0

  ! First design matrix and vector of pseudo-observations
  lsa%A(:,:) = 0.0D0
  lsa%y(:)   = 0.0D0
  jMjd = 0
  LoopEpochs: DO iMjd = 1,nMjd

     mjd = sCore%sta(iSta)%ts%mjd(iMjd)

     ! Outliers trick to optimize the procedure in terms of CPU-time:
     ! do not sete outliers in the normal equation matrix
     IF( sCore%mdl%outlMjd(iMjd) == 0.0D0 )THEN
        jMjd = jMjd + 1
     ELSE
        CYCLE LoopEpochs
     END IF

     LoopComp: DO iVal = 1,nVal

        indM  = nVal*(jMjd-1) + iVal
        lsa%y(indM) = sCore%sta(iSta)%ts%val(iMjd,iVal)

        SetUpJumpParams: DO iEvnt = 1,nAmod
           IF( amod(iEvnt)%cand == candSub )CYCLE
           IF( amod(iEvnt)%type /= typeJump )CYCLE
           ! Index
           indP = amod(iEvnt)%index + iVal
           IF( mjd >= amod(iEvnt)%mjd )THEN
              lsa%A(indM,indP) = 1.0D0
           END IF
        END DO SetUpJumpParams

        SetUpRateParams: DO iEvnt = 1,nAmod
           IF( amod(iEvnt)%cand == candSub )CYCLE
           IF( amod(iEvnt)%type /= typeRate )CYCLE
           ! Index
           indP = amod(iEvnt)%index + iVal
           IF( mjd >= amod(iEvnt)%mjd )THEN
              lsa%A(indM,indP) = mjd - amod(iEvnt)%mjd
           END IF
           ! Main (first) drift
           IF( amod(iEvnt)%info == infoInit )THEN
              lsa%A(indM,indP) = mjd - sCore%outTimRefCrd
           END IF
        END DO SetUpRateParams

        SetUpPeriParams: DO iEvnt = 1,nAmod
           IF( amod(iEvnt)%cand == candSub )CYCLE
           IF( amod(iEvnt)%type /= typePeri )CYCLE
           ! Filter
           seen = 0
           DO jEvnt = 1,nAmod
              IF( ( amod(jEvnt)%type == -typePeri ) .AND. &
                   ( mjd .isIn. amod(jEvnt)%timint ) )THEN
                 seen = 1
                 EXIT
              END IF
           END DO
           IF( seen == 1 )CYCLE
           ! Index
           indP = amod(iEvnt)%index + 2*(iVal-1)
           omega = amod(iEvnt)%omega
           lsa%A(indM,indP+1) = DCOS( omega * (mjd-sCore%inTimRefCrd) )
           lsa%A(indM,indP+2) = DSIN( omega * (mjd-sCore%inTimRefCrd) )
        END DO SetUpPeriParams

        ! Diagonal weight matrix (P)
        IF     ( opt%inPltFileVciEna == 1 )THEN
           lsa%P(indM) = ( opt%inPltFileViM0 / &
                           sCore%sta(iSta)%ts%dVal(iMjd,iVal) )**2
        END IF

     END DO LoopComp

     ! Variance-Covariance matrix P = inv(Qyy) = iQyy (if present)
     IF( opt%inPltFileVciEna == 2 .AND. nVal == 3  )THEN
        lsa%iQyy(jMjd,:,:) = sCore%sta(iSta)%ts%vci(iMjd,:,:)
        ret = matinv(lsa%iQyy(jMjd,:,:),3)
        IF( ret == 1 ) THEN
           WRITE(lfnerr,'(/,A,/)') ' *** SR FODISMOD: var-cov matrix singular'
           CALL exitrc(2)
        END IF
     END IF

  END DO LoopEpochs

  ! End of subroutine
  ! -----------------
  !  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodismod

END MODULE s_FODISMOD


