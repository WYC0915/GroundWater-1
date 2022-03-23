MODULE s_FODIADAS
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodiadas(mmod,opt,sCore,iSta,stateM)

! -------------------------------------------------------------------------
! Purpose:    Design matrix, least squares adjustiment, save results
!
! Author:     Luca Ostini
!
! Created:    14-Aug-2008
!
! Changes:    14-Aug-2008 LO: Created this file
!             02-Oct-2008 LO: First revision
!             09-Oct-2008 LO: VCI only for residuals
!             09-Oct-2008 LO: Third revision
!             05-Dec-2008 LO: Fourth revisio: velocity changes allowed
!             11-Feb-2009 LO: Fifth revision: major changes
!             18-Aug-2009 LO: Cosmetics
!             25-Sep-2009 LO: Changes for F90 consistency
!             21-Dec-2009 LO: FFT removed and several changes apported
!             02-Mar-2010 LO: Major changes do to elimination of FODISUBR
!             07-Apr-2010 LO: Major changes do to algorithm and output file
!             16-Jun-2010 LO: Time series diveded by component
!             26-Aug-2010 LO: Architectural changes
!             03-Jan-2010 LO: INTENT(INOUT) removed and bug fixed
!             28-Apr-2012 RD: Nullify all pointers, use p_fodits with only
!             04-Sep-2012 KS: Bug with the pointer nullification fixed
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------


! Used Modules
! ------------
! structures, variables:
  USE m_bern,    ONLY: i4b, r8b, lfnPrt, shortLineLength
  USE p_fodits,  ONLY: t_opt, t_sCore, infoInit, nonSignificant, &
                       typeJump, typeRate, typeVelo, typePeri, typeOutl, &
                       candSub, candNon, candYes, &
                       maMod, mremfin, mapri, miden, mRemo, &
                       sCoreModDelEvnt, sCoreModSortElem, sCoreModAddEvnt, &
                       sCoreGetValModMjd, sCoreTstAddCrit, &
                       sCoreChkPresEvntInMod, sCoreWriteElemLine
!  USE d_debug,   ONLY: debug_entry, debug_exit

! subroutines, functions:
  USE s_fodismod
  USE s_fodislsa
  USE s_alcerr

! no implicit
  IMPLICIT NONE

! subroutine name
  CHARACTER(LEN=shortLineLength), PARAMETER    :: srName = 'fodiadas'


! List of Arguments
! -----------------
! input:
  INTEGER(i4b),INTENT(IN)        :: mmod          ! Working mode

! input/output:
  TYPE(t_opt)                    :: opt           ! Option structure
  TYPE(t_sCore)                  :: sCore         ! Core structure of FODITS
  INTEGER(i4b)                   :: iSta          ! Station index
  INTEGER(i4b)                   :: stateM        ! state machine

! output:


! Local Types
! -----------


! Local Parameters
! ----------------


! Local Variables
! ---------------
  CHARACTER(LEN=9)               :: label

  INTEGER(i4b)                   :: iac
  INTEGER(i4b)                   :: iMjd
  INTEGER(i4b)                   :: jMjd
  INTEGER(i4b)                   :: nMjd
  INTEGER(i4b)                   :: seen
  INTEGER(i4b)                   :: seen2
  INTEGER(i4b)                   :: indP
  INTEGER(i4b)                   :: indQ
  INTEGER(i4b)                   :: indJ
  INTEGER(i4b)                   :: iVal
  INTEGER(i4b)                   :: nVal
  INTEGER(i4b)                   :: iEvnt
  INTEGER(i4b)                   :: jEvnt
  INTEGER(i4b)                   :: jjEvnt
  INTEGER(i4b)                   :: nJ,nV,nO,nP

  REAL(r8b),DIMENSION(:),&
            POINTER              :: valMod
  REAL(r8b),DIMENSION(:),&
            ALLOCATABLE          :: trVec

! Call debug routine
! ------------------
!  CALL debug_entry(srName)


! Initialization of all variables
! -------------------------------
  NULLIFY(valMod)
  nMjd = sCore%sta(iSta)%ts%nMjd
  nVal = sCore%nVal

  ! Allocate memory
  ALLOCATE(valMod(nVal),stat=iac)
  CALL alcerr(iac,'valMod',(/nVal/),srName)

  ! Velocity change parameters (%type == typeVelo) are implemented in the
  ! functional by setting up rate parameters (%type == typeRate). Namely,
  ! a velocity change is modelled with two rate parameteres.

  ! Delete all typeRate parameters from %amod (except the infoInit ones, i.e.,
  ! the intial ones that remain always in the functional model).
  DO
     seen = 0
     DO iEvnt = 1,sCore%mdl%nAmod
        IF( sCore%mdl%amod(iEvnt)%info == infoInit )CYCLE
        IF( sCore%mdl%amod(iEvnt)%type == typeRate )THEN
           CALL sCoreModDelEvnt( sCore%mdl%amod, sCore%mdl%nAmod, iEvnt )
           seen = 1
           EXIT
        END IF
     END DO
     IF( seen == 0 )EXIT
  END DO

  ! Sort %amod in order to setup the first design matrix - %index is redefined
  CALL sCoreModSortElem( nVal, sCore%mdl%amod, sCore%mdl%nAmod )

  ! Set typeRate parameters according to the the velocity changes
  ! (typeVelo parameters) in %amod.
  DO
     seen = 0
     DO iEvnt = 1,sCore%mdl%nAmod
        IF( sCore%mdl%amod(iEvnt)%type /= typeVelo )CYCLE
        IF( sCore%mdl%amod(iEvnt)%cand == candSub  )CYCLE
        ! Filter
        seen2 = 0
        DO jEvnt = 1,sCore%mdl%nAmod
           IF( sCore%mdl%amod(jEvnt)%type == typeRate .AND. &
               sCore%mdl%amod(iEvnt)%type == typeVelo .AND. &
               sCore%mdl%amod(jEvnt)%mjd == sCore%mdl%amod(iEvnt)%mjd )THEN
              seen2 = 1
              EXIT
           END IF
        END DO
        IF( seen2 == 1 )CYCLE
        ! Add
        CALL sCoreModAddEvnt( sCore%mdl%amod(iEvnt), &
             sCore%mdl%amod, sCore%mdl%nAmod )
        sCore%mdl%amod(sCore%mdl%nAmod)%type = typeRate
        sCore%mdl%amod(sCore%mdl%nAmod)%cand = candNon
        seen = 1
        EXIT
     END DO
     IF( seen == 0 )EXIT
  END DO

  ! Sort %amod in order to setup the first design matrix - %index is redefined
  CALL sCoreModSortElem( nVal, sCore%mdl%amod, sCore%mdl%nAmod )

  ! Get epochs of outliers for %outlMjd
  IF( mmod == mAmod )THEN
     sCore%mdl%outlMjd(:) = 0.0D0
     DO iMjd = 1,nMjd
        DO iEvnt = 1,sCore%mdl%nAmod
           IF( sCore%mdl%amod(iEvnt)%type == typeOutl .AND. &
               sCore%sta(iSta)%ts%mjd(iMjd) == sCore%mdl%amod(iEvnt)%mjd )THEN
              sCore%mdl%outlMjd(iMjd) = sCore%sta(iSta)%ts%mjd(iMjd)
              EXIT
           END IF
        END DO
     END DO
  END IF

  ! Set the first design matrix
  CALL fodismod(opt,sCore,iSta,sCore%lsa,sCore%mdl%amod,sCore%mdl%nAmod)

  ! Check the degree of freedom
  IF( sCore%lsa%dof <= 0 )THEN
     sCore%lsa%m0 = 0.0D0
     sCore%sta(iSta)%singular = 1
     IF( opt%outFileVerbose == 1 )THEN
        WRITE(lfnprt,'(A,1X,I07,A,1X)',ADVANCE='NO') &
             ' | Nr. of observations:', sCore%lsa%nnMjd, ','
        WRITE(lfnprt,'(A,1X,I07,A,1X)',ADVANCE='NO') &
             ' Nr. of parameters:', sCore%lsa%nnPar , ','
        WRITE(lfnprt,'(A,1X,I07,A,1X)',ADVANCE='NO') &
             ' Degree of freedom:', sCore%lsa%dof
        WRITE(lfnprt,'(A)') &
             '      => Algorithm stopped'
     END IF
     stateM = 9
     !  CALL debug_exit(srName)
     RETURN
  END IF

  ! Least squares adjustment (LSA)
  CALL fodislsa( opt%inPltFileVciEna, sCore%lsa )
  ! Check for the degree of freedom
  IF( sCore%lsa%detN == 0.0D0 .OR. sCore%lsa%dof <= 0 )THEN
  END IF

  ! Save m0 with all elements included in the functional model
  IF( mmod == mAmod )THEN
     sCore%lsb%m0 = sCore%lsa%m0
  END IF

  ! Save rate parameters
  SaveFirstRateParam: DO iEvnt = 1,sCore%mdl%nAmod
     IF( mmod == mRemo  )CYCLE
     IF( sCore%mdl%amod(iEvnt)%type /= typeRate )CYCLE
     ! %index
     indP = sCore%mdl%amod(iEvnt)%index
     ! %par
     sCore%mdl%amod(iEvnt)%par(:) = 0.0D0
     sCore%mdl%amod(iEvnt)%par(1:nVal) = sCore%lsa%x(indP+1:indP+nVal)
     ! %val
     sCore%mdl%amod(iEvnt)%val = 0.0D0
     DO iVal = 1,nVal
        sCore%mdl%amod(iEvnt)%val = &
             sCore%mdl%amod(iEvnt)%val + sCore%mdl%amod(iEvnt)%par(iVal)**2
     END DO
     sCore%mdl%amod(iEvnt)%val = SQRT(sCore%mdl%amod(iEvnt)%val)
     ! %dVal
     ALLOCATE(trVec(nVal),stat=iac)
     CALL alcerr(iac,'trVec',(/nVal/),srName)
     trVec(:) = 0.0D0
     DO iVal = 1,nVal
        IF( sCore%mdl%amod(iEvnt)%val > 0.0D0 )THEN
           trVec(iVal) = sCore%mdl%amod(iEvnt)%par(iVal) / &
                         sCore%mdl%amod(iEvnt)%val
        ELSE
           trVec(iVal) = 1.0D0
        END IF
     END DO
     sCore%mdl%amod(iEvnt)%dVal = DOT_PRODUCT(MATMUL(trVec,&
          sCore%lsa%Qxx(indP+1:indP+nVal,indP+1:indP+nVal)),trVec)
     IF( sCore%mdl%amod(iEvnt)%dVal > 0.0D0 )THEN
        sCore%mdl%amod(iEvnt)%dVal = &
             sCore%lsa%m0 * SQRT( sCore%mdl%amod(iEvnt)%dVal )
     ELSE
        sCore%mdl%amod(iEvnt)%dVal = sCore%lsa%m0
     END IF
     DEALLOCATE(trVec,stat=iac)
     ! %stTst and siTst
     sCore%mdl%amod(iEvnt)%stTst = 0.0D0
     sCore%mdl%amod(iEvnt)%siTst = 0
  END DO SaveFirstRateParam

  ! Save estimates of discontinuities (modelled as offsets in functional model)
  SaveJumpParams: DO iEvnt = 1,sCore%mdl%nAmod
     IF( mmod == mRemo  )CYCLE
     IF( sCore%mdl%amod(iEvnt)%cand == candSub  )CYCLE
     IF( sCore%mdl%amod(iEvnt)%type /= typeJump )CYCLE
     ! %index
     indP = sCore%mdl%amod(iEvnt)%index
     ! %par
     sCore%mdl%amod(iEvnt)%par(:) = 0.0D0
     sCore%mdl%amod(iEvnt)%par(1:nVal) = sCore%lsa%x(indP+1:indP+nVal)
     ! %val
     sCore%mdl%amod(iEvnt)%val = 0.0D0
     DO iVal = 1,nVal
        sCore%mdl%amod(iEvnt)%val = sCore%mdl%amod(iEvnt)%val + &
             sCore%mdl%amod(iEvnt)%par(iVal)**2
     END DO
     sCore%mdl%amod(iEvnt)%val = SQRT(sCore%mdl%amod(iEvnt)%val)
     ! %dVal
     ALLOCATE(trVec(nVal),stat=iac)
     CALL alcerr(iac,'trVec',(/nVal/),srName)
     trVec(:) = 0.0D0
     DO iVal = 1,nVal
        IF( sCore%mdl%amod(iEvnt)%val > 0.0D0 )THEN
           trVec(iVal) = sCore%mdl%amod(iEvnt)%par(iVal) / &
                         sCore%mdl%amod(iEvnt)%val
        ELSE
           trVec(iVal) = 1.0D0
        END IF
     END DO
     sCore%mdl%amod(iEvnt)%dVal = DOT_PRODUCT(MATMUL(trVec,&
          sCore%lsa%Qxx(indP+1:indP+nVal,indP+1:indP+nVal)),trVec)
     IF( sCore%mdl%amod(iEvnt)%dVal > 0.0D0 )THEN
        sCore%mdl%amod(iEvnt)%dVal = &
             sCore%lsa%m0 * SQRT( sCore%mdl%amod(iEvnt)%dVal )
     ELSE
        sCore%mdl%amod(iEvnt)%dVal = sCore%lsa%m0
     END IF
     ! %stTst
     IF( sCore%mdl%amod(iEvnt)%info /= infoInit )THEN
        sCore%mdl%amod(iEvnt)%stTst = sCore%mdl%amod(iEvnt)%val / &
                                      sCore%mdl%amod(iEvnt)%dVal
     END IF
     DEALLOCATE(trVec,stat=iac)
  END DO SaveJumpParams

  SaveVeloParams: DO iEvnt = 1,sCore%mdl%nAmod
     IF( mmod == mRemo  )CYCLE
     IF( sCore%mdl%amod(iEvnt)%type   /= typeRate )CYCLE
     ! Seek typeVelo corresponding to typeRate
     DO jEvnt = 1,sCore%mdl%nAmod
        IF( sCore%mdl%amod(jEvnt)%type /= typeVelo )CYCLE
        IF( sCore%mdl%amod(jEvnt)%mjd /= sCore%mdl%amod(iEvnt)%mjd )CYCLE
        ! %index
        indJ = sCore%mdl%amod(iEvnt)%index
        ! %val and %par
        sCore%mdl%amod(jEvnt)%par(:) = 0.0D0
        sCore%mdl%amod(jEvnt)%val = 0.0D0
        DO iVal = 1,nVal
           sCore%mdl%amod(jEvnt)%par(iVal) = sCore%lsa%x(indJ+iVal)
           sCore%mdl%amod(jEvnt)%val = sCore%mdl%amod(jEvnt)%val + &
                sCore%mdl%amod(jEvnt)%par(iVal)**2
        END DO
        sCore%mdl%amod(jEvnt)%val = SQRT(sCore%mdl%amod(jEvnt)%val)
        ! %dVal
        ALLOCATE(trVec(nVal),stat=iac)
        CALL alcerr(iac,'trVec',(/nVal/),srName)
        trVec(:) = 0.0D0
        DO iVal = 1,nVal
           IF( sCore%mdl%amod(jEvnt)%val > 0.0D0 )THEN
              trVec(iVal)      = sCore%lsa%x(indJ+iVal) / &
                                 sCore%mdl%amod(jEvnt)%val
           ELSE
              trVec(iVal)      = 1.0D0
           END IF
        END DO
        sCore%mdl%amod(jEvnt)%dVal = DOT_PRODUCT(MATMUL(trVec,&
             sCore%lsa%Qxx(indJ+1:indJ+nVal,indJ+1:indJ+nVal)),trVec)
        IF( sCore%mdl%amod(jEvnt)%dVal > 0.0D0 )THEN
           sCore%mdl%amod(jEvnt)%dVal = &
                sCore%lsa%m0 * SQRT( sCore%mdl%amod(jEvnt)%dVal )
        ELSE
           sCore%mdl%amod(jEvnt)%dVal = sCore%lsa%m0
        END IF
        ! %stTst
        sCore%mdl%amod(jEvnt)%stTst = sCore%mdl%amod(jEvnt)%val / &
                                      sCore%mdl%amod(jEvnt)%dVal
        DEALLOCATE(trVec,stat=iac)
        EXIT
     END DO
  END DO SaveVeloParams

  ! Save estimates of periodic functions
  SavePeriParams: DO iEvnt = 1,sCore%mdl%nAmod
     IF( mmod == mRemo  )CYCLE
     IF( sCore%mdl%amod(iEvnt)%cand == candSub  )CYCLE
     IF( sCore%mdl%amod(iEvnt)%type /= typePeri )CYCLE
     ! %index
     indP = sCore%mdl%amod(iEvnt)%index
     ! %val and %par
     sCore%mdl%amod(iEvnt)%par(:) = 0.0D0
     sCore%mdl%amod(iEvnt)%val = 0.0D0
     DO iVal = 1,nVal
        indQ = 2*(iVal-1)
        sCore%mdl%amod(iEvnt)%ppar(iVal,1) = sCore%lsa%x(indP+indQ+1) ! COS
        sCore%mdl%amod(iEvnt)%ppar(iVal,2) = sCore%lsa%x(indP+indQ+2) ! SIN
        sCore%mdl%amod(iEvnt)%par(iVal) = &
             SQRT( sCore%mdl%amod(iEvnt)%ppar(iVal,1)**2 + &
                   sCore%mdl%amod(iEvnt)%ppar(iVal,2)**2 )
        sCore%mdl%amod(iEvnt)%val = &
             sCore%mdl%amod(iEvnt)%val + sCore%mdl%amod(iEvnt)%par(iVal)**2
        sCore%mdl%amod(iEvnt)%phi(iVal) = &
             ATAN2( sCore%mdl%amod(iEvnt)%ppar(iVal,2) , &
                    sCore%mdl%amod(iEvnt)%ppar(iVal,1) )
     END DO
     sCore%mdl%amod(iEvnt)%val = SQRT(sCore%mdl%amod(iEvnt)%val)
     ! %dVal
     ALLOCATE(trVec(2*nVal),stat=iac)
     CALL alcerr(iac,'trVec',(/2*nVal/),srName)
     trVec(:) = 0.0D0
     DO iVal = 1,nVal
        indQ = 2*(iVal-1)
        IF( sCore%mdl%amod(iEvnt)%val > 0.0D0 )THEN
           trVec(indQ+1) = sCore%mdl%amod(iEvnt)%ppar(iVal,1) / &
                           sCore%mdl%amod(iEvnt)%val
           trVec(indQ+2) = sCore%mdl%amod(iEvnt)%ppar(iVal,2) / &
                           sCore%mdl%amod(iEvnt)%val
        ELSE
           trVec(indQ+1) = 1.0D0
           trVec(indQ+2) = 1.0D0
        END IF
     END DO
     sCore%mdl%amod(iEvnt)%dVal = DOT_PRODUCT(MATMUL(trVec,&
          sCore%lsa%Qxx(indP+1:indP+2*nVal,indP+1:indP+2*nVal)),trVec)
     IF( sCore%mdl%amod(iEvnt)%dVal > 0.0D0 )THEN
        sCore%mdl%amod(iEvnt)%dVal = &
             sCore%lsa%m0 * SQRT( sCore%mdl%amod(iEvnt)%dVal )
     ELSE
        sCore%mdl%amod(iEvnt)%dVal = sCore%lsa%m0
     END IF
     ! %stTst
     sCore%mdl%amod(iEvnt)%stTst = sCore%mdl%amod(iEvnt)%val / &
                                   sCore%mdl%amod(iEvnt)%dVal
     DEALLOCATE(trVec,stat=iac)
  END DO SavePeriParams

  ! Save estimates of outliers
  SaveOutlParams: DO iEvnt = 1,sCore%mdl%nAmod
     IF( sCore%mdl%amod(iEvnt)%type /= typeOutl )CYCLE
     ! Get the value of the model at sCore%mdl%amod(iEvnt)%mjd
     CALL sCoreGetValModMjd( nVal, sCore%mdl%amod, sCore%mdl%nAmod, &
          sCore%mdl%amod(iEvnt)%mjd, valMod )
     DO iVal = 1,nVal
        sCore%mdl%amod(iEvnt)%mod(iVal) = valMod(iVal)
     END DO
     ! Find the epoch of the observation for sCore%mdl%amod(iEvnt)%mjd
     jMjd = 0
     DO iMjd = 1,nMjd
        IF( sCore%sta(iSta)%ts%mjd(iMjd) == sCore%mdl%amod(iEvnt)%mjd )THEN
           jMjd = iMjd
           EXIT
        END IF
     END DO
     ! Compute the outlier value = obs(jMjd) - model(jMjd), for each iVal
     sCore%mdl%amod(iEvnt)%val = 0.0D0
     DO iVal = 1,nVal
        sCore%mdl%amod(iEvnt)%par(iVal) = &
             sCore%sta(iSta)%ts%val(jMjd,iVal) - valMod(iVal)
        sCore%mdl%amod(iEvnt)%val = sCore%mdl%amod(iEvnt)%val + &
             sCore%mdl%amod(iEvnt)%par(iVal)**2
     END DO
     sCore%mdl%amod(iEvnt)%val = SQRT( sCore%mdl%amod(iEvnt)%val )
     ! %dVal
     IF     ( opt%inPltFileVciEna == 0 )THEN
        sCore%mdl%amod(iEvnt)%dVal = opt%inPltFileViM0
     ELSE
        ALLOCATE(trVec(nVal),stat=iac)
        CALL alcerr(iac,'trVec',(/nVal/),srName)
        trVec(:) = 0.0D0
        DO iVal = 1,nVal
           IF( sCore%mdl%amod(iEvnt)%val > 0.0D0 )THEN
              trVec(iVal) = sCore%mdl%amod(iEvnt)%par(iVal) / &
                   sCore%mdl%amod(iEvnt)%val
           ELSE
              trVec(iVal) = 1.0D0
           END IF
        END DO
        IF     ( opt%inPltFileVciEna == 1 )THEN
           sCore%mdl%amod(iEvnt)%dVal = 0.0D0
           DO iVal = 1,nVal
              sCore%mdl%amod(iEvnt)%dVal = sCore%mdl%amod(iEvnt)%dVal + &
                   trVec(iVal)**2 * sCore%sta(iSta)%ts%dVal(jMjd,iVal)**2
           END DO
           sCore%mdl%amod(iEvnt)%dVal = SQRT( sCore%mdl%amod(iEvnt)%dVal )
        ELSE IF( opt%inPltFileVciEna == 2 )THEN
           sCore%mdl%amod(iEvnt)%dVal = DOT_PRODUCT(MATMUL(trVec,&
                sCore%sta(iSta)%ts%vci(jMjd,:,:)),trVec)
           sCore%mdl%amod(iEvnt)%dVal = &
                opt%inPltFileViM0 * SQRT( sCore%mdl%amod(iEvnt)%dVal )
        END IF
        DEALLOCATE(trVec,stat=iac)
     END IF
     ! %stTst
     sCore%mdl%amod(iEvnt)%stTst = sCore%mdl%amod(iEvnt)%val / &
          sCore%mdl%amod(iEvnt)%dVal
  END DO SaveOutlParams

  ! statistical test
  ! ================
  ! Test for outliers when mAmod, i.e., when all elements are in the model
  ! ----------------------------------------------------------------------
  IF     ( mmod == mAmod )THEN
     DO iEvnt = 1,sCore%mdl%nAmod
        IF( sCore%mdl%amod(iEvnt)%type == typeOutl )THEN
           sCore%mdl%amod(iEvnt)%vlTst = sCore%mdl%amod(iEvnt)%stTst
           CALL sCoreTstAddCrit( opt, sCore, 0, sCore%mdl%amod(iEvnt) )
        END IF
     END DO
  ! Test: removing iElem from %amod
  ! -------------------------------
  ELSE IF( mmod == mRemo .OR. mmod == mRemFin )THEN
     DO iEvnt = 1,sCore%mdl%nAmod
        IF( sCore%mdl%amod(iEvnt)%cand == candSub )THEN
           sCore%mdl%amod(iEvnt)%vlTst = &
                ABS(sCore%lsb%prevVtv/sCore%lsa%vtv-1) * &
                REAL(nMjd) / REAL(sCore%nTotMjd)
           IF( mmod == mRemo )THEN
              CALL sCoreTstAddCrit( opt, sCore, 0, sCore%mdl%amod(iEvnt) )
           ELSE
              CALL sCoreTstAddCrit( opt, sCore, 1, sCore%mdl%amod(iEvnt) )
           END IF
        END IF
     END DO
  ! Test: inserting iElem of %apri into %amod
  ! -----------------------------------------
  ELSE IF( mmod == mApri )THEN
     DO iEvnt = 1,sCore%mdl%nAmod
        IF( sCore%mdl%amod(iEvnt)%cand == candYes )THEN
           sCore%mdl%amod(iEvnt)%vlTst = &
                ABS(sCore%lsa%vtv/sCore%lsb%prevVtv-1) * &
                REAL(nMjd) / REAL(sCore%nTotMjd)
           CALL sCoreTstAddCrit( opt, sCore, 0, sCore%mdl%amod(iEvnt) )
        END IF
     END DO
  ! Test: inserting iElem of %iden into %amod
  ! -----------------------------------------
  ELSE IF( mmod == mIden )THEN
     DO iEvnt = 1,sCore%mdl%nAmod
        IF( sCore%mdl%amod(iEvnt)%cand == candYes )THEN
           sCore%mdl%amod(iEvnt)%vlTst = &
                ABS(sCore%lsa%vtv/sCore%lsb%prevVtv-1) * &
                REAL(nMjd) / REAL(sCore%nTotMjd)
           CALL sCoreTstAddCrit( opt, sCore, 0, sCore%mdl%amod(iEvnt) )
        END IF
     END DO
  END IF

  ! Delete all rate parameters from %amod (except the first ones - infoInit)
  ! ------------------------------------------------------------------------
  DO
     seen = 0
     DO iEvnt = 1,sCore%mdl%nAmod
        IF( sCore%mdl%amod(iEvnt)%info == infoInit )CYCLE
        IF( sCore%mdl%amod(iEvnt)%type == typeRate )THEN
           CALL sCoreModDelEvnt( sCore%mdl%amod, sCore%mdl%nAmod, iEvnt )
           seen = 1
           EXIT
        END IF
     END DO
     IF( seen == 0 )EXIT
  END DO

  ! Save results
  ! ------------

  ! Set previous vtv
  IF( mmod == mAmod )THEN
     sCore%lsb%prevVtv = sCore%lsa%vtv
  END IF

  ! Store the %amod model into %sta(iSta)%mod%evnt
  IF( mmod == mApri .OR. mmod == mIden )THEN
     DO iEvnt = 1,sCore%mdl%nAmod
        IF( sCore%mdl%amod(iEvnt)%cand /= candYes )CYCLE
        DO jEvnt = 1,sCore%sta(iSta)%mod%nEvnt
           CALL sCoreChkPresEvntInMod( sCore%mdl%amod(iEvnt), &
                sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt, jjEvnt )
           IF( jjEvnt > 0 )THEN
              sCore%sta(iSta)%mod%evnt(jjEvnt) = sCore%mdl%amod(iEvnt)
              sCore%sta(iSta)%mod%evnt(jjEvnt)%siTst = nonSignificant
              EXIT
           END IF
        END DO
     END DO
  END IF

  ! Deallocate memory
  DEALLOCATE(valMod,stat=iac)

  ! Reports
  ! -------

  ! Verbose mode (normal)
  IF( opt%outFileVerbose == 1 .AND. mmod == mAmod )THEN

     nJ  = 0
     nV  = 0
     nO = 0
     nP  = 0
     DO iEvnt = 1,sCore%mdl%nAmod
        IF( sCore%mdl%amod(iEvnt)%info == infoInit )CYCLE
        IF( sCore%mdl%amod(iEvnt)%type == typeRate )CYCLE
        IF     ( sCore%mdl%amod(iEvnt)%type == typeJump )THEN
           nJ = nJ + 1
        ELSE IF( sCore%mdl%amod(iEvnt)%type == typeVelo )THEN
           nV = nV + 1
        ELSE IF( sCore%mdl%amod(iEvnt)%type == typeOutl )THEN
           nO = nO + 1
        ELSE IF( sCore%mdl%amod(iEvnt)%type == typePeri )THEN
           nP = nP + 1
        END IF
     END DO

     ! Report of least squares adjustment (LSA)
     WRITE(lfnprt,'(A,1X,I06,A,1X)',ADVANCE='NO') &
          ' | LSA #obs:', sCore%lsa%nnMjd, ','
     WRITE(lfnprt,'(A,1X,I04,A,1X)',ADVANCE='NO') &
          '#par:', sCore%lsa%nnPar, ','
     WRITE(lfnprt,'(A,1X,I06,A,1X)',ADVANCE='NO') &
          'DoF:', sCore%lsa%dof, ','
     WRITE(lfnprt,'(2(A,I02),1(A,I04),1(A,I02),A,1X)',ADVANCE='NO') &
          ', nJ=:', nJ, ', nV=:', nV, ', nO=:', nO, ', nP=:', nP, ','
     WRITE(lfnprt,'(A,1X,E12.6)') &
          'm0:', sCore%lsa%m0

  END IF

  ! Write FuncModel
  IF( opt%outFileVerbose == 1 .AND. mmod == mAmod )THEN
     DO iEvnt = 1,sCore%mdl%nAmod
        IF( sCore%mdl%amod(iEvnt)%info == infoInit )CYCLE
        IF( sCore%mdl%amod(iEvnt)%type == typeRate )CYCLE
        label = 'ActuModl'
        CALL sCoreWriteElemLine(sCore%mdl%amod(iEvnt),label,sCore%lsa%m0)
     END DO
  END IF

! End of subroutine
! -----------------
!  DEALLOCATE(valMod)
!  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodiadas

END MODULE s_FODIADAS

