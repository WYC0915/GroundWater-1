MODULE s_FODIMPOD
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodimpod(opt,sCore,iSta)

! -------------------------------------------------------------------------
! Purpose:    Identification of the most probable elements in terms of:
!             - discontinuities (by sum of residuals),
!             - velocity changes (by two linear regressions),
!             - outliers (by outlier value w.r.t. the RMS),
!             - periodic functions (by Fourier analysis).
!
! Author:     Luca Ostini
!
! Created:    14-Aug-2008
!
! Changes:    14-Aug-2008 LO: Created this file
!             02-Oct-2008 LO: First revision
!             09-Oct-2008 LO: Outlier pre-detection changed
!             09-Oct-2008 LO: Third revision
!             09-Oct-2008 LO: Too many outliers problem solved.
!             05-Dec-2008 LO: Fourth revisio: velocity changes allowed
!             11-Feb-2009 LO: Fifth revision: major changes
!             25-Sep-2009 LO: Changes for F90 consistency
!             18-Nov-2009 LO: Major changes for consistency
!             21-Dec-2009 LO: FFT removed and several changes apported
!             02-Mar-2010 LO: Major changes do to elimination of FODISUBR
!             07-Apr-2010 LO: Major changes do to algorithm and output file
!             16-Jun-2010 LO: Time series diveded by component
!             09-Aug-2010 RD: New syminvg used
!             26-Aug-2010 LO: Architectural changes
!             03-Jan-2010 LO: INTENT(INOUT) removed and bug fixed
!             24-May-2011 LO: New update of ADDNEQ2 meta-data
!             05-Jul-2011 LO: EVL check in search
!             31-Aug-2011 LO: typeNot intervals fixed + 1s sampling
!             19-Sep-2012 RD: Use P_FODITS with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used Modules
! ------------
! structures, variables:
  USE m_bern,    ONLY: i4b, r8b, shortLineLength, lfnPrt
  USE d_const,   ONLY: PI
  USE p_fodits,  ONLY: t_opt, t_sCore, t_lsa, t_evnt, &
                       typejump, infounkw, flagtst, typevelo, &
                       typeoutl, significant, typeperi, flagnot, &
                       scorechkpresevntinmod, scoremodaddevnt, &
                       scoreinitnewevnt, scoremoddelevnt, scoremodsortelem, &
                       scoretstaddcrit

! operator, methods:
!  USE d_debug,   ONLY: debug_entry, debug_exit

! subroutines, functions:
  USE s_alcerr
  USE s_fodislsa
  USE f_ikf
  USE s_syminvg

! no implicit
  IMPLICIT NONE

! subroutine name
  CHARACTER(LEN=8), PARAMETER    :: srName = 'fodimpod'


! List of Arguments
! -----------------
! input:
  TYPE(t_opt)                    :: opt             ! Option structure
  TYPE(t_sCore)                  :: sCore           ! Core structure of FODITS
  INTEGER(i4b),INTENT(IN)        :: iSta            ! Station index

! input/output:

! output:


! Local Types
! -----------


! Local Parameters
! ----------------

! Local Variables
! ---------------
  TYPE(t_lsa)                    :: lsa
  TYPE(t_evnt)                   :: virtEvnt
  TYPE(t_evnt)                   :: virtEvnt2

  INTEGER(i4b)                   :: iac
  INTEGER(i4b)                   :: iLine
  INTEGER(i4b)                   :: nLine
  INTEGER(i4b)                   :: iMjd
  INTEGER(i4b)                   :: jMjd
  INTEGER(i4b)                   :: nMjd
  INTEGER(i4b)                   :: mMjd
  INTEGER(i4b)                   :: kMjd
  INTEGER(i4b)                   :: ret
  INTEGER(i4b)                   :: iVal
  INTEGER(i4b)                   :: iPar
  INTEGER(i4b)                   :: jPar
  INTEGER(i4b)                   :: nVal
  INTEGER(i4b)                   :: indM
  INTEGER(i4b)                   :: indP
  INTEGER(i4b)                   :: iEvnt
  INTEGER(i4b)                   :: jEvnt
  INTEGER(i4b)                   :: seen
  INTEGER(i4b)                   :: seen1
  INTEGER(i4b)                   :: seen2
  INTEGER(i4b)                   :: iMaxJump
  INTEGER(i4b)                   :: iMaxVelo
  INTEGER(i4b)                   :: iMaxPeri
  INTEGER(i4b)                   :: iTmp
  INTEGER(i4b)                   :: jTmp
  INTEGER(i4b)                   :: nParLin
  INTEGER(i4b)                   :: nOutProp
  INTEGER(i4b)                   :: kTmp
  INTEGER(i4b)                   :: offsetMjd

  REAL(r8b),DIMENSION(:),&
            ALLOCATABLE          :: mjdResampl
  REAL(r8b),DIMENSION(:,:),&
            ALLOCATABLE          :: valResampl
  REAL(r8b),DIMENSION(:),&
            ALLOCATABLE          :: testJump
  REAL(r8b),DIMENSION(:),&
            ALLOCATABLE          :: testJumpSub
  REAL(r8b),DIMENSION(:),&
            ALLOCATABLE          :: testVeloSub
  REAL(r8b),DIMENSION(:),&
            ALLOCATABLE          :: testVelo
  REAL(r8b),DIMENSION(:,:),&
            ALLOCATABLE          :: testOutl
  REAL(r8b),DIMENSION(:),&
            ALLOCATABLE          :: testPeri
  REAL(r8b),DIMENSION(:),&
            ALLOCATABLE          :: sumRes
  REAL(r8b),DIMENSION(:),&
            ALLOCATABLE          :: resid
  REAL(r8b)                      :: resv
  REAL(r8b)                      :: maxJump
  REAL(r8b)                      :: maxVelo
  REAL(r8b)                      :: maxPeri
  REAL(r8b),DIMENSION(:),&
            ALLOCATABLE          :: ddTmp
  REAL(r8b)                      :: omegat
  REAL(r8b)                      :: valTst
  REAL(r8b), DIMENSION(:),       &
             ALLOCATABLE         :: linQxx
  REAL(r8b), DIMENSION(:),       &
             ALLOCATABLE         :: hlpA
  REAL(r8b), DIMENSION(:),       &
             ALLOCATABLE         :: hlpAS
  REAL(r8b), DIMENSION(3,3)      :: hlpN
  REAL(r8b), DIMENSION(:),       &
             ALLOCATABLE         :: hlpB
  REAL(r8b)                      :: mjdVal
  REAL(r8b)                      :: perVal
  REAL(r8b)                      :: begMjdSubIntv
  REAL(r8b)                      :: endMjdSubIntv
  REAL(r8b),DIMENSION(:),&
            ALLOCATABLE          :: trVec

! Call debug routine
! ------------------
! CALL debug_entry(srName)

! Initialization of all variables
! -------------------------------
  nMjd = sCore%sta(iSta)%ts%nMjd
  nVal = sCore%nVal

  ! Count number of pseudo-observations without OUTL (outliers) parameters
  lsa%nnMjd = 0
  mMjd = 0
  DO iMjd = 1,nMjd
     IF( sCore%mdl%outlMjd(iMjd) == 0.0D0 )THEN
        mMjd = mMjd + 1
        lsa%nnMjd = lsa%nnMjd + nVal
     END IF
  END DO

  ! Allocation memory
  ALLOCATE(testJump(mMjd),stat=iac)
  CALL alcerr(iac, 'testJump', (/mMjd/), srName)
  ALLOCATE(testVelo(mMjd),stat=iac)
  CALL alcerr(iac, 'testVelo', (/mMjd/), srName)
  ALLOCATE(testOutl(mMjd,2+nVal),stat=iac)
  CALL alcerr(iac, 'testOutl', (/mMjd,2+nVal+nVal*nVal/), srName)
  ALLOCATE(ddTmp(2+nVal),stat=iac)
  CALL alcerr(iac, 'ddTmp', (/2+nVal/), srName)
  ALLOCATE(testPeri(sCore%san%nPer),stat=iac)
  CALL alcerr(iac, 'testPeri', (/sCore%san%nPer/), srName)

  ALLOCATE(mjdResampl(mMjd),stat=iac)
  CALL alcerr(iac, 'mjdResampl', (/mMjd/), srName)
  ALLOCATE(valResampl(mMjd,nVal),stat=iac)
  CALL alcerr(iac, 'valResampl', (/mMjd,nVal/), srName)

  ! Resample mjd without outlier parameters (outl)
  jMjd = 0
  DO iMjd = 1,nMjd
     IF( sCore%mdl%outlMjd(iMjd) == 0.0D0 )THEN
        jMjd = jMjd + 1
        indM = nVal*(jMjd-1)
        mjdResampl(jMjd) = sCore%sta(iSta)%ts%mjd(iMjd)
        valResampl(jMjd,:) = sCore%lsa%v(indM+1:indM+nVal)
     END IF
  END DO

  ! Initialize lsa
  lsa%nnPar = 0
  lsa%nnMjd = 0
  NULLIFY(lsa%A)
  NULLIFY(lsa%Qxx)
  NULLIFY(lsa%y)
  NULLIFY(lsa%x)
  NULLIFY(lsa%mod)
  NULLIFY(lsa%v)
  DEALLOCATE(lsa%A,stat=iac)
  DEALLOCATE(lsa%Qxx,stat=iac)
  DEALLOCATE(lsa%y,stat=iac)
  DEALLOCATE(lsa%x,stat=iac)
  DEALLOCATE(lsa%mod,stat=iac)
  DEALLOCATE(lsa%v,stat=iac)

  ! Remove all %iden elements
  CALL sCoreModDelEvnt(sCore%mdl%iden, sCore%mdl%nIden)

  ! DISCONTINUITY IDENTIFICATION
  ! ----------------------------
  ! Sum of residuals: the most probable discontinuity is identified at the
  ! epoch of the largest comulated sum of residuals.
  IF( opt%modNewJumpIdentify == 1 )THEN

     ! Add a virtual jump at the end of the TS to complete the search loop
     CALL sCoreInitNewEvnt( nVal, virtEvnt )
     virtEvnt%type = typeJump
     virtEvnt%info = infoUnkw
     virtEvnt%mjd = sCore%sta(iSta)%ts%mjd(nMjd)
     virtEvnt%flag = flagTst
     virtEvnt%remark = 'VIR-DISC'
     ! Check whether the identified jump exists already in %amod
     CALL sCoreChkPresEvntInMod( virtEvnt, &
                                 sCore%mdl%amod, sCore%mdl%nAmod, seen )
     ! Add the identified evnt
     IF( seen == 0 )THEN
        CALL sCoreModAddEvnt( virtEvnt, &
             sCore%mdl%amod, sCore%mdl%nAmod )
     END IF

     ! Sort %amod
     CALL sCoreModSortElem( nVal, sCore%mdl%amod, sCore%mdl%nAmod )

     ! Allocate memory
     ALLOCATE(sumRes(nVal),stat=iac)
     CALL alcerr(iac,'sumRes',(/nVal/),srName)

     ! Loop over all sub-intervals delimited by jumps and velos
     DO iEvnt = 1,sCore%mdl%nAmod
        IF( sCore%mdl%amod(iEvnt)%type /= typeJump .AND. &
            sCore%mdl%amod(iEvnt)%type /= typeVelo )CYCLE
        ! Define begMjdSubIntv of the sub-interval
        begMjdSubIntv = sCore%mdl%amod(iEvnt)%mjd

        DO jEvnt = iEvnt+1,sCore%mdl%nAmod
           IF( sCore%mdl%amod(jEvnt)%type /= typeJump .AND. &
               sCore%mdl%amod(jEvnt)%type /= typeVelo )CYCLE
           IF( sCore%mdl%amod(jEvnt)%mjd <= begMjdSubIntv )CYCLE
           ! Define endMjdSubIntv of the sub-interval
           endMjdSubIntv = sCore%mdl%amod(jEvnt)%mjd

           ! Count number of observations in the sub-interval
           offsetMjd = -1
           kMjd = 0
           DO iMjd = 1,mMjd
              IF( mjdResampl(iMjd) >= begMjdSubIntv .AND. offsetMjd == -1 )THEN
                 offsetMjd = iMjd - 1
              END IF
              IF( mjdResampl(iMjd) <  begMjdSubIntv )CYCLE
              IF( mjdResampl(iMjd) >= endMjdSubIntv )CYCLE
              kMjd = kMjd + 1
           END DO
           ! Minimal number of elements to define a jump
           IF( kMjd < 4 )CYCLE

           ! Resampling TS - remove data gaps to improve the search test
           lsa%nnPar = 2 * nVal
           lsa%nnMjd = nVal * kMjd
           lsa%dof = lsa%nnMjd - lsa%nnPar
           ! Allocation memory
           ALLOCATE(resid(lsa%nnMjd),stat=iac)
           CALL alcerr(iac,'resid',(/lsa%nnMjd/),srName)
           resid(:) = 0.0D0
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
           ALLOCATE(lsa%iQyy(kMjd,3,3),stat=iac)
           CALL alcerr(iac,'lsa%iQyy',(/kMjd,3,3/),srName)
           lsa%iQyy(:,:,:) = 0.0D0
           ! Define the first design matrix
           lsa%A(:,:) = 0.0D0
           DO iMjd = 1,kMjd
              indM  = nVal*(iMjd-1)
              DO iVal = 1,nVal
                 lsa%A(indM+iVal,iVal) = 1.0D0
                 lsa%A(indM+iVal,nVal+iVal) = iMjd
                 lsa%y(indM+iVal) = valResampl(offsetMjd+iMjd,iVal)
              END DO
           END DO
           ! Least squares adjustment (LSA)
           !! CALL fodislsa( opt%inPltFileVciEna, lsa )
           CALL fodislsa( 0, lsa )
           resid(:) = lsa%v(:)
           ! Deallocate memory
           DEALLOCATE(lsa%A,stat=iac)
           DEALLOCATE(lsa%Qxx,stat=iac)
           DEALLOCATE(lsa%y,stat=iac)
           DEALLOCATE(lsa%x,stat=iac)
           DEALLOCATE(lsa%mod,stat=iac)
           DEALLOCATE(lsa%v,stat=iac)

           ! Allocate memory
           ALLOCATE(testJumpSub(kMjd),stat=iac)
           CALL alcerr(iac, 'testJumpSub', (/kMjd/), srName)

           ! Identify the epoch of the most probable discontinuity
           testJumpSub(:) = 0.0D0
           sumRes(:) = 0.0D0
           DO iMjd = kMjd,1,-1
              ! Index
              indM = nVal*(iMjd-1)
              ! Increment
              DO iVal = 1,nVal
                 sumRes(iVal) = sumRes(iVal) + resid(indM+iVal)
              END DO
              ! Absolute value and Scaling factor
              valTst = 0.0D0
              DO iVal = 1,nVal
                 valTst = valTst + sumRes(iVal)**2
              END DO
              valTst = SQRT(valTst)
              testJumpSub(iMjd) = valTst
           END DO

           ! Deallocate memory
           DEALLOCATE(lsa%P,stat=iac)
           DEALLOCATE(lsa%iQyy,stat=iac)

           ! No jumps at first or last epoch
           testJumpSub(1) = 0.0D0
           testJumpSub(kMjd) = 0.0D0

           ! Localize the epoch of the most probable discontinuity
           maxJump  = MAXVAL( testJumpSub(:), DIM=1 )
           iMaxJump = MAXLOC( testJumpSub(:), DIM=1 )

           ! Report result to testJump
           IF( maxJump == 0.0D0 ) maxJump = 1.0D0
           testJump(offsetMjd+1:offsetMjd+kMjd) = testJumpSub(:) / maxJump

           ! Memory deallocation
           DEALLOCATE(resid,stat=iac)
           DEALLOCATE(testJumpSub,stat=iac)

           ! Prepare the information for the new evnt
           CALL sCoreInitNewEvnt( nVal, sCore%mdl%evnt )
           sCore%mdl%evnt%type = typeJump
           sCore%mdl%evnt%info = infoUnkw
           sCore%mdl%evnt%mjd = mjdResampl(offsetMjd+iMaxJump)
           sCore%mdl%evnt%flag = flagTst
           sCore%mdl%evnt%remark = 'NEW-DISC'
           ! Check whether the identified jump exists already in %iden
           CALL sCoreChkPresEvntInMod( sCore%mdl%evnt, &
                                       sCore%mdl%iden, sCore%mdl%nIden, seen )

           ! Check whether the identified elements is outside NOT intervals
           CALL fodimpod_chkPresEvntInNotMod( sCore%mdl%evnt, &
                sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt, seen2 )

           ! Add the identified evnt
           IF( seen == 0 .AND. seen2 == 0 )THEN

              CALL sCoreModAddEvnt( sCore%mdl%evnt, &
                                    sCore%mdl%iden, sCore%mdl%nIden )
           END IF

           ! Search only in between n and n+1 velos
           EXIT

        END DO

     END DO

     ! Memory deallocation
     DEALLOCATE(sumRes,stat=iac)

     ! Check whether the identified jump exists already in %amod
     CALL sCoreChkPresEvntInMod( virtEvnt, &
                                 sCore%mdl%amod, sCore%mdl%nAmod, seen )
     ! Delete the identified evnt - virtual disc
     IF( seen /= 0 )THEN
        CALL sCoreModDelEvnt(sCore%mdl%amod, sCore%mdl%nAmod, seen)
     END IF

     ! Correct the test - remove first and last epoch
     testJump(1) = 0.0D0
     testJump(mMjd) = 0.0D0

  END IF
  ! END: DISCONTINUITY IDENTIFICATION

  ! lsa
  lsa%nnPar = 0
  lsa%nnMjd = 0
  DEALLOCATE(lsa%A,stat=iac)
  DEALLOCATE(lsa%Qxx,stat=iac)
  DEALLOCATE(lsa%y,stat=iac)
  DEALLOCATE(lsa%x,stat=iac)
  DEALLOCATE(lsa%mod,stat=iac)
  DEALLOCATE(lsa%v,stat=iac)

  ! VELOCITY CHANGE DETECTION
  ! -------------------------
  ! Two linear regressions delimited by the test epoch are fitted to the
  ! time series for this test. Since LSA, i.e., matrix inversion, must be
  ! computed for each epoch, the procedure is improved in terms of CPU-time.
  IF( opt%modNewVeloIdentify == 1 .AND. mMjd > 4 )THEN

     ! Add a virtual velo at the begin of the TS to complete the search loop
     CALL sCoreInitNewEvnt( nVal, virtEvnt )
     virtEvnt%type = typeVelo
     virtEvnt%info = infoUnkw
     virtEvnt%mjd = sCore%sta(iSta)%ts%mjd(1)
     virtEvnt%flag = flagTst
     virtEvnt%remark = 'VIR-VEL1'
     ! Check whether the identified jump exists already in %amod
     CALL sCoreChkPresEvntInMod( virtEvnt, &
                                 sCore%mdl%amod, sCore%mdl%nAmod, seen )
     ! Add the identified evnt
     IF( seen == 0 )THEN
        CALL sCoreModAddEvnt( virtEvnt, &
             sCore%mdl%amod, sCore%mdl%nAmod )
     END IF

     ! Add a virtual velo at the end of the TS to complete the search loop
     CALL sCoreInitNewEvnt( nVal, virtEvnt2 )
     virtEvnt2%type = typeVelo
     virtEvnt2%info = infoUnkw
     virtEvnt2%mjd = sCore%sta(iSta)%ts%mjd(nMjd)
     virtEvnt2%flag = flagTst
     virtEvnt2%remark = 'VIR-VEL2'
     ! Check whether the identified jump exists already in %amod
     CALL sCoreChkPresEvntInMod( virtEvnt2, &
                                 sCore%mdl%amod, sCore%mdl%nAmod, seen )
     ! Add the identified evnt
     IF( seen == 0 )THEN
        CALL sCoreModAddEvnt( virtEvnt2, &
             sCore%mdl%amod, sCore%mdl%nAmod )
     END IF

     ! Sort %amod
     CALL sCoreModSortElem( nVal, sCore%mdl%amod, sCore%mdl%nAmod )

     ! Initialize testVelo
     testVelo(:) = 0.0D0

     ! Loop over all sub-intervals delimited by velos
     DO iEvnt = 1,sCore%mdl%nAmod
        IF( sCore%mdl%amod(iEvnt)%type /= typeVelo )CYCLE
        ! Define begMjdSubIntv of the sub-interval
        begMjdSubIntv = sCore%mdl%amod(iEvnt)%mjd

        DO jEvnt = iEvnt+1,sCore%mdl%nAmod
           IF( sCore%mdl%amod(jEvnt)%type /= typeVelo )CYCLE
           ! Define endMjdSubIntv of the sub-interval
           endMjdSubIntv = sCore%mdl%amod(jEvnt)%mjd

           ! Count number of observations in the sub-interval
           offsetMjd = -1
           kMjd = 0
           DO iMjd = 1,mMjd
              IF( mjdResampl(iMjd) >= begMjdSubIntv .AND. offsetMjd == -1 )THEN
                 offsetMjd = iMjd - 1
              END IF
              IF( mjdResampl(iMjd) <  begMjdSubIntv )CYCLE
              IF( mjdResampl(iMjd) >= endMjdSubIntv )CYCLE
              kMjd = kMjd + 1
           END DO
           ! Minimal number of elements to define a jump
           IF( kMjd < 4 )CYCLE

           ! Allocate memory
           ALLOCATE(testVeloSub(kMjd),stat=iac)
           CALL alcerr(iac, 'testVeloSub', (/kMjd/), srName)

           ! Two velocity parameters for each component
           lsa%nnPar = 3 * nVal
           lsa%nnMjd = nVal * kMjd
           lsa%dof = lsa%nnMjd - lsa%nnPar
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
           CALL alcerr(iac,'lsa%mod', (/lsa%nnMjd/),srName)
           ALLOCATE(lsa%v(lsa%nnMjd),stat=iac)
           CALL alcerr(iac,'lsa%v', (/lsa%nnMjd/),srName)

           ! Identify the epoch of the most probable discontinuity
           testVeloSub(:) = 0.0D0

           ! Allocation memory
           ALLOCATE(hlpA(kMjd),stat=iac)
           CALL alcerr(iac,'hlpA',(/kMjd/),srName)
           ALLOCATE(hlpAS(kMjd),stat=iac)
           CALL alcerr(iac,'hlpAS',(/kMjd/),srName)
           ALLOCATE(hlpB(lsa%nnPar),stat=iac)
           CALL alcerr(iac,'hlpB',(/lsa%nnPar/),srName)

           ! Normal matrix with one offset, one velocity, and one
           ! velocity change. In order to speed up the analysis these
           ! parameters are optimized at the NEQ level without
           ! processing the first design matrix from the beginning.

           ! N(1,1)
           hlpN(1,1) = kMjd

           ! Fill hlpA, part of hlpB, and lsa%y
           hlpN(1,2) = 0.0D0
           hlpA(:)   = 0.0D0
           hlpB(:)   = 0.0D0
           DO iMjd = 1,kMjd
              hlpA(iMjd) = mjdResampl(offsetMjd+iMjd)
              hlpN(1,2) = hlpN(1,2) + hlpA(iMjd)
              indM  = nVal*(iMjd-1)
              DO iVal = 1,nVal
                 indP = 3*(iVal-1)
                 resv = valResampl(offsetMjd+iMjd,iVal)
                 hlpB(indP+1) = hlpB(indP+1) + resv
                 hlpB(indP+2) = hlpB(indP+2) + hlpA(iMjd)*resv
                 lsa%y(indM+iVal) = resv
              END DO
           END DO

           ! N(1,1)... N(2,2)
           hlpN(2,1) = hlpN(1,2)
           hlpN(2,2) = DOT_PRODUCT(hlpA,hlpA)

           ! Compute test value for each epoch of the time series
           DO jMjd = 2,kMjd-2

              ! Shift
              hlpAS(:) = 0.0D0
              DO iMjd = 1,kMjd
                 IF( iMjd > jMjd )THEN
                    hlpAS(iMjd) = hlpA(iMjd) - hlpA(jMjd)
                 END IF
              END DO

              ! N(1,3), N(2,3), and N(3,3)
              hlpN(2,3) = DOT_PRODUCT(hlpA,hlpAS)
              hlpN(3,2) = hlpN(2,3)
              hlpN(1,3) = 0.0D0
              DO iMjd = 1,kMjd
                 hlpN(1,3) = hlpN(1,3) + hlpAS(iMjd)
              END DO
              hlpN(3,1) = hlpN(1,3)
              hlpN(3,3) = DOT_PRODUCT(hlpAS,hlpAS)

              ! Fill normal equation
              lsa%Qxx(:,:) = 0.0D0
              DO iVal = 1,nVal
                 indP = 3*(iVal-1)
                 lsa%Qxx(indP+1:indP+3,indP+1:indP+3) = hlpN(:,:)
              END DO

              ! B vector
              DO iVal = 1,nVal
                 indP = 3*(iVal-1)
                 hlpB(indP+3) = 0.0D0
                 DO iMjd = 1,kMjd
                    resv = valResampl(offsetMjd+iMjd,iVal)
                    hlpB(indP+3) = hlpB(indP+3) + hlpAS(iMjd)*resv
                 END DO
              END DO

              ! Convert the 2-D Qxx matrix to 1-D for syminvg
              nParLin = ((lsa%nnPar+1)*lsa%nnPar)/2
              ALLOCATE(linQxx(nParLin),stat=iac)
              CALL alcerr(iac, 'linQxx', (/nParLin/), srName)
              DO iPar = 1,lsa%nnPar
                 DO jPar = 1,iPar
                    linQxx(ikf(jPar,iPar)) = lsa%Qxx(jPar,iPar)
                 END DO
              END DO

              ! Matrix inversion
              CALL syminvg(lsa%nnPar,linQxx(:),0,ret)
              IF( ret > 0 ) THEN
                 lsa%detN = 0.0D0
              ELSE
                 lsa%detN = 1.0D0
              END IF

              ! Recompose the 1-D Qxx matrix to 2-D and store it into lsa%Qxx
              lsa%Qxx(:,:) = 0.0D0
              DO iPar = 1,lsa%nnPar
                 DO jPar = 1,iPar
                    lsa%Qxx(jPar,iPar) = linQxx(ikf(jPar,iPar))
                    lsa%Qxx(iPar,jPar) = linQxx(ikf(jPar,iPar))
                 END DO
              END DO
              DEALLOCATE(linQxx,stat=iac)

              ! Execute lsa%x = MATMUL(lsa%Qxx,b)
              lsa%x = MATMUL(lsa%Qxx,hlpB)

              ! Residuals
              lsa%v(:)   = 0.0D0
              lsa%mod(:) = 0.0D0
              DO iMjd = 1,kMjd
                 indM  = nVal*(iMjd-1)
                 DO iVal = 1,nVal
                    indP = 3*(iVal-1)
                    lsa%mod(indM+iVal) = &
                           lsa%x(indP+1) + &
                           lsa%x(indP+2)*hlpA(iMjd) + &
                           lsa%x(indP+3)*hlpAS(iMjd)
                 END DO
              END DO
              lsa%v(:) = lsa%y(:) - lsa%mod(:)

              ! Estimated standard deviation of unit weight
              lsa%m0 = SQRT(DOT_PRODUCT(lsa%v,lsa%v)/lsa%dof)

              ! Store the result
              testVeloSub(jMjd) = 0.0D0
              IF( lsa%m0 > 1.0D-20 )THEN
                 testVeloSub(jMjd) = 1.0D0/lsa%m0
              END IF

           END DO

           ! Deallocation
           DEALLOCATE(hlpA,stat=iac)
           DEALLOCATE(hlpAS,stat=iac)
           DEALLOCATE(hlpB,stat=iac)

           ! Localize the epoch of the most probable discontinuity
           maxVelo  = MAXVAL( testVeloSub(:), DIM=1 )
           iMaxVelo = MAXLOC( testVeloSub(:), DIM=1 )

           ! Prepare the information for the new evnt
           CALL sCoreInitNewEvnt( nVal, sCore%mdl%evnt )
           sCore%mdl%evnt%type = typeVelo
           sCore%mdl%evnt%info = infoUnkw
           sCore%mdl%evnt%mjd = mjdResampl(offsetMjd+iMaxVelo)
           sCore%mdl%evnt%flag = flagTst
           sCore%mdl%evnt%remark = 'NEW-VELO'
           ! Check whether the identified velo exists already in %iden
           CALL sCoreChkPresEvntInMod( sCore%mdl%evnt, &
                                       sCore%mdl%iden, sCore%mdl%nIden, seen )
           ! Check whether the identified velo exists already in %amod
           CALL sCoreChkPresEvntInMod( sCore%mdl%evnt, &
                                       sCore%mdl%amod, sCore%mdl%nAmod, seen1 )
           ! Check whether the identified elements is outside NOT intervals
           CALL fodimpod_chkPresEvntInNotMod( sCore%mdl%evnt, &
                   sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt, seen2 )
           ! Add the identified evnt
           IF( seen == 0 .AND. seen1 == 0 .AND. seen2 == 0 )THEN
              CALL sCoreModAddEvnt( sCore%mdl%evnt, &
                                    sCore%mdl%iden, sCore%mdl%nIden )
           END IF

           ! Report result to testVelo
           IF( maxVelo == 0.0D0 ) maxVelo = 1.0D0
           testVelo(offsetMjd+1:offsetMjd+kMjd) = testVeloSub(:) / maxVelo

           ! Memory deallocation
           DEALLOCATE(testVeloSub,stat=iac)

           ! lsa
           DEALLOCATE(lsa%A,stat=iac)
           DEALLOCATE(lsa%Qxx,stat=iac)
           DEALLOCATE(lsa%y,stat=iac)
           DEALLOCATE(lsa%x,stat=iac)
           DEALLOCATE(lsa%mod,stat=iac)
           DEALLOCATE(lsa%v,stat=iac)

           ! Search only in between n and n+1 velos
           EXIT

        END DO

     END DO

     ! Check whether the identified velo exists already in %amod
     CALL sCoreChkPresEvntInMod( virtEvnt, &
                                 sCore%mdl%amod, sCore%mdl%nAmod, seen )
     ! Delete the identified evnt - virtual velo
     IF( seen /= 0 )THEN
        CALL sCoreModDelEvnt(sCore%mdl%amod, sCore%mdl%nAmod, seen)
     END IF

     ! Check whether the identified velo exists already in %amod
     CALL sCoreChkPresEvntInMod( virtEvnt2, &
                                 sCore%mdl%amod, sCore%mdl%nAmod, seen )
     ! Delete the identified evnt - virtual velo
     IF( seen /= 0 )THEN
        CALL sCoreModDelEvnt(sCore%mdl%amod, sCore%mdl%nAmod, seen)
     END IF

  END IF
  ! END: VELOCITY CHANGE DETECTION

  ! lsa
  lsa%nnPar = 0
  lsa%nnMjd = 0
  DEALLOCATE(lsa%A,stat=iac)
  DEALLOCATE(lsa%Qxx,stat=iac)
  DEALLOCATE(lsa%y,stat=iac)
  DEALLOCATE(lsa%x,stat=iac)
  DEALLOCATE(lsa%mod,stat=iac)
  DEALLOCATE(lsa%v,stat=iac)

  ! OUTLIERS IDENTIFICATION
  ! -----------------------
  ALLOCATE(resid(sCore%lsa%nnMjd),stat=iac)
  CALL alcerr(iac,'resid',(/sCore%lsa%nnMjd/),srName)
  resid(:) = sCore%lsa%v(:)
  ! Identification test
  testOutl(:,:) = 0.0D0
  DO iMjd = 1,mMjd
  ! Index
     indM = nVal*(iMjd-1)
     ! Increment
     ! Time series selection
     valTst = 0.0D0
     DO iVal = 1,nVal
        valTst = valTst + resid(indM+iVal)**2
        testoutl(iMjd,2+iVal) = resid(indM+iVal)
     END DO
     testoutl(iMjd,1) = SQRT(valTst)
     testoutl(iMjd,2) = mjdResampl(iMjd)
  END DO
  ! Detect very large outliers: hard-wired feature
  DO iEvnt = 1,mMjd
     ! Test - opt%modNOutlRms times the m0
     IF( testoutl(iEvnt,1) > opt%modNOutlRms * sCore%lsa%m0 )THEN
        ! Prepare the information for the new evnt
        CALL sCoreInitNewEvnt( nVal, sCore%mdl%evnt )
        sCore%mdl%evnt%type = typeOutl
        sCore%mdl%evnt%info = infoUnkw
        sCore%mdl%evnt%mjd  = testoutl(iEvnt,2)
        sCore%mdl%evnt%flag = flagTst
        sCore%mdl%evnt%remark = 'NEW-OUTL'
        ! %val
        sCore%mdl%evnt%val = testoutl(iEvnt,1)
        ! vlTst
        sCore%mdl%evnt%vlTst = 0.0D0
        ! stTst
        sCore%mdl%evnt%stTst = sCore%mdl%evnt%vlTst
        ! siTst
        sCore%mdl%evnt%siTst = significant
        ! Check whether the identified outl exists already in %iden
        CALL sCoreChkPresEvntInMod( sCore%mdl%evnt, &
                                    sCore%mdl%iden, sCore%mdl%nIden, seen )
        ! Check whether the identified elements is outside NOT intervals
        CALL fodimpod_chkPresEvntInNotMod( sCore%mdl%evnt, &
             sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt, seen2 )

        IF( seen /= 0 .OR. seen2 /= 0 )CYCLE
        ! Add the identified evnt
        CALL sCoreModAddEvnt( sCore%mdl%evnt, &
                              sCore%mdl%iden, sCore%mdl%nIden )
     END IF
  END DO
  ! Values of outliers w.r.t. the RMS of time series.
  IF( opt%modNewOutlIdentify == 1 )THEN

     ! Sort the values of testOutl from the largest to the smallest
     DO iTmp = 1,mMjd-1
        kTmp = iTmp
        DO jTmp = iTmp+1,mMjd
           IF( testoutl(jTmp,1) > testoutl(kTmp,1) )THEN
              kTmp = jTmp
           END IF
        END DO
        IF( kTmp /= iTmp )THEN
           ddTmp = testoutl(iTmp,:)
           testoutl(iTmp,:) = testoutl(kTmp,:)
           testoutl(kTmp,:) = ddTmp
        END IF
     END DO

     ! Add the elements to %iden
     nOutProp = opt%modNNewOutl
     IF( mMjd < nOutProp )THEN
        nOutProp = mMjd
     END IF
     DO iEvnt = 1,nOutProp
        ! Prepare the information for the new evnt
        CALL sCoreInitNewEvnt( nVal, sCore%mdl%evnt )
        sCore%mdl%evnt%type = typeOutl
        sCore%mdl%evnt%info = infoUnkw
        sCore%mdl%evnt%mjd = testoutl(iEvnt,2)
        sCore%mdl%evnt%flag = flagTst
        sCore%mdl%evnt%remark = 'NEW-OUTL'
        ! %val
        sCore%mdl%evnt%val = 0.0D0
        DO iVal = 1,nVal
          sCore%mdl%evnt%par(iVal) = testoutl(iEvnt,2+iVal)
          sCore%mdl%evnt%val = sCore%mdl%evnt%val + sCore%mdl%evnt%par(iVal)**2
        END DO
        sCore%mdl%evnt%val = SQRT(sCore%mdl%evnt%val)

        ! Find the epoch of the observation for sCore%mdl%evnt%mjd
        jMjd = 0
        DO iMjd = 1,nMjd
           IF( sCore%sta(iSta)%ts%mjd(iMjd) == sCore%mdl%evnt%mjd )THEN
              jMjd = iMjd
              EXIT
           END IF
        END DO
        ! %dVal
        IF     ( opt%inPltFileVciEna == 0 )THEN
           sCore%mdl%evnt%dVal = opt%inPltFileViM0
        ELSE
           ALLOCATE(trVec(nVal),stat=iac)
           CALL alcerr(iac,'trVec',(/nVal/),srName)
           trVec(:) = 0.0D0
           DO iVal = 1,nVal
              IF( sCore%mdl%evnt%val > 0.0D0 )THEN
                 trVec(iVal) = sCore%mdl%evnt%par(iVal) / &
                               sCore%mdl%evnt%val
              ELSE
                 trVec(iVal) = 1.0D0
              END IF
           END DO
           IF     ( opt%inPltFileVciEna == 1 )THEN
              sCore%mdl%evnt%dVal = 0.0D0
              DO iVal = 1,nVal
                 sCore%mdl%evnt%dVal = sCore%mdl%evnt%dVal + &
                      trVec(iVal)**2 * sCore%sta(iSta)%ts%dVal(jMjd,iVal)**2
              END DO
              sCore%mdl%evnt%dVal = SQRT( sCore%mdl%evnt%dVal )
           ELSE IF( opt%inPltFileVciEna == 2 )THEN
              sCore%mdl%evnt%dVal = DOT_PRODUCT(MATMUL(trVec,&
                  sCore%sta(iSta)%ts%vci(jMjd,:,:)),trVec)
              sCore%mdl%evnt%dVal = &
                   opt%inPltFileViM0 * SQRT( sCore%mdl%evnt%dVal )
           END IF
           DEALLOCATE(trVec,stat=iac)
        END IF
        ! vlTst
        sCore%mdl%evnt%vlTst = sCore%mdl%evnt%val / sCore%mdl%evnt%dVal
        ! stTst
        sCore%mdl%evnt%stTst = sCore%mdl%evnt%vlTst
        ! Additional test
        CALL sCoreTstAddCrit( opt, sCore, 1, sCore%mdl%evnt )
        IF( sCore%mdl%evnt%siTst /= significant )CYCLE

        ! Check whether the identified outl exists already in %iden
        CALL sCoreChkPresEvntInMod( sCore%mdl%evnt, &
                                    sCore%mdl%iden, sCore%mdl%nIden, seen )
        IF( seen /= 0 )CYCLE

        ! Add the identified evnt
        CALL sCoreModAddEvnt( sCore%mdl%evnt, &
                              sCore%mdl%iden, sCore%mdl%nIden )
     END DO

     IF( opt%outFileVerboseT == 1 .OR. opt%outFileVerboseTT == 1 )THEN
        ! Identification test (only for verbose output purposes)
        testOutl(:,:) = 0.0D0
        DO iMjd = 1,mMjd
           ! Index
           indM = nVal*(iMjd-1)
           ! Time series selection
           valTst = 0.0D0
           DO iVal = 1,nVal
              valTst = valTst + resid(indM+iVal)**2
              testoutl(iMjd,2+iVal) = resid(indM+iVal)
           END DO
           testoutl(iMjd,1) = valTst
           testoutl(iMjd,2) = mjdResampl(iMjd)
        END DO
     END IF

  END IF
  ! Deallocation
  DEALLOCATE(resid,stat=iac)
  ! END: OUTLIERS IDENTIFICATION

  ! lsa
  lsa%nnPar = 0
  lsa%nnMjd = 0
  DEALLOCATE(lsa%A,stat=iac)
  DEALLOCATE(lsa%Qxx,stat=iac)
  DEALLOCATE(lsa%y,stat=iac)
  DEALLOCATE(lsa%x,stat=iac)
  DEALLOCATE(lsa%mod,stat=iac)
  DEALLOCATE(lsa%v,stat=iac)

  ! PERIODIC FUNCTION IDENTIFICATION
  ! --------------------------------
  ! Fourier analysis by least squares
  iMaxPeri = 0
  IF( opt%modNewPeriIdentify == 1 )THEN

     ! Two periodic function parameters for each component (sin and cos)
     lsa%nnPar = 2 * nVal
     lsa%nnMjd = nVal * mMjd
     lsa%dof = lsa%nnMjd - lsa%nnPar
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
     ! Estimate amplitude for each spectral line by Least Squares
     DO iEvnt = 1,sCore%san%nPer

        ! Define the design matrix and the pseudo-observation vector
        lsa%A(:,:) = 0.0D0
        DO iMjd = 1,mMjd
           indM  = nVal*(iMjd-1)
           omegat = sCore%san%per(iEvnt)*mjdResampl(iMjd)
           DO iVal = 1,nVal
              indP = 2*(iVal-1)
              lsa%A(indM+iVal,indP+1) = DCOS(omegat)
              lsa%A(indM+iVal,indP+2) = DSIN(omegat)
              lsa%y(indM+iVal) = sCore%lsa%v(indM+iVal)
           END DO
        END DO

        ! Least squares adjustment (LSA)
        CALL fodislsa( 0, lsa )

        ! Store the result
        testPeri(iEvnt) = 0.0D0
        IF( lsa%detN /= 0.0D0 .AND. lsa%dof > 0 .AND. lsa%m0 > 1.0D-10 )THEN
           testPeri(iEvnt) = 1.0D0/lsa%m0
        END IF

     END DO

     ! Localize the epoch of the most probable periodic function
     maxPeri  = MAXVAL( testPeri(:), DIM=1 )
     iMaxPeri = MAXLOC( testPeri(:), DIM=1 )

     ! Filter
     IF( maxPeri > 0.0D0 )THEN

        ! Prepare the information for the new evnt
        CALL sCoreInitNewEvnt( nVal, sCore%mdl%evnt )
        sCore%mdl%evnt%type = typePeri
        sCore%mdl%evnt%info = infoUnkw
        sCore%mdl%evnt%timint%t(1) = sCore%begMjd
        sCore%mdl%evnt%timint%t(2) = sCore%endMjd
        sCore%mdl%evnt%omega = sCore%san%per(iMaxPeri)
        sCore%mdl%evnt%flag = flagTst
        sCore%mdl%evnt%remark = 'NEW-PERI'

        ! Check whether the identified peri exists already in %iden
        CALL sCoreChkPresEvntInMod( sCore%mdl%evnt, &
             sCore%mdl%iden, sCore%mdl%nIden, seen )

        ! Add the identified evnt
        IF( seen == 0 )THEN
           CALL sCoreModAddEvnt( sCore%mdl%evnt, &
                sCore%mdl%iden, sCore%mdl%nIden )
        END IF

     END IF

     ! Deallocate
     DEALLOCATE(lsa%A,stat=iac)
     DEALLOCATE(lsa%Qxx,stat=iac)
     DEALLOCATE(lsa%y,stat=iac)
     DEALLOCATE(lsa%x,stat=iac)
     DEALLOCATE(lsa%mod,stat=iac)
     DEALLOCATE(lsa%v,stat=iac)

  END IF
  ! END: PERIODIC FUNCTION IDENTIFICATION

  ! lsa
  lsa%nnPar = 0
  lsa%nnMjd = 0
  DEALLOCATE(lsa%A,stat=iac)
  DEALLOCATE(lsa%Qxx,stat=iac)
  DEALLOCATE(lsa%y,stat=iac)
  DEALLOCATE(lsa%x,stat=iac)
  DEALLOCATE(lsa%mod,stat=iac)
  DEALLOCATE(lsa%v,stat=iac)

  ! Add a velocity change at epoch of discontinuities - if user defined
  IF( opt%modAddVeloAJ == 1 )THEN
     ! of %iden
     DO iEvnt = 1,sCore%mdl%nIden
        IF( sCore%mdl%iden(iEvnt)%type /= typeJump )CYCLE
        IF( sCore%mdl%iden(iEvnt)%info /= infoUnkw )CYCLE
        IF( sCore%mdl%iden(iEvnt)%flag == flagNot )CYCLE
        CALL sCoreInitNewEvnt( nVal, sCore%mdl%evnt )
        sCore%mdl%evnt = sCore%mdl%iden(iEvnt)
        sCore%mdl%evnt%type = typeVelo
        sCore%mdl%evnt%info = infoUnkw
        sCore%mdl%evnt%remark = 'NEW-VELO'
        ! Check whether the identified jump exists already in %iden
        CALL sCoreChkPresEvntInMod( sCore%mdl%evnt, &
             sCore%mdl%iden, sCore%mdl%nIden, seen )
        ! Add the identified evnt
        IF( seen == 0 )THEN
           CALL sCoreModAddEvnt( sCore%mdl%evnt, &
                sCore%mdl%iden, sCore%mdl%nIden )
        END IF
     END DO
     ! of %amod
     DO iEvnt = 1,sCore%mdl%nAmod
        IF( sCore%mdl%amod(iEvnt)%type /= typeJump )CYCLE
        IF( sCore%mdl%amod(iEvnt)%info /= infoUnkw )CYCLE
        IF( sCore%mdl%amod(iEvnt)%flag == flagNot )CYCLE
        CALL sCoreInitNewEvnt( nVal, sCore%mdl%evnt )
        sCore%mdl%evnt = sCore%mdl%amod(iEvnt)
        sCore%mdl%evnt%type = typeVelo
        sCore%mdl%evnt%info = infoUnkw
        sCore%mdl%evnt%remark = 'NEW-VELO'
        ! Check whether the identified jump exists already in %iden
        CALL sCoreChkPresEvntInMod( sCore%mdl%evnt, &
             sCore%mdl%iden, sCore%mdl%nIden, seen )
        ! Add the identified evnt
        IF( seen == 0 )THEN
           CALL sCoreModAddEvnt( sCore%mdl%evnt, &
                sCore%mdl%iden, sCore%mdl%nIden )
        END IF
     END DO
  END IF

  ! Sort %iden
  CALL sCoreModSortElem( nVal, sCore%mdl%iden, sCore%mdl%nIden )

  IF( opt%outFileVerboseT == 1 .OR. opt%outFileVerboseTT == 1 )THEN

     nLine = mMjd
     IF( sCore%san%nPer > nLine )THEN
        nLine = sCore%san%nPer
     END IF

     DO iLine = 1,nLine

        WRITE(lfnprt,'(A,1X,A16,1X,A,I02,1X,A,I02)',ADVANCE='NO') &
             'ITR-STP-TST', &
             sCore%sta(iSta)%name, &
             'I=',sCore%ctr%nIterLoop, &
             'S=',sCore%ctr%nScrnLoop

        IF( ( opt%modNewJumpIdentify == 1 .OR. &
              opt%modNewVeloIdentify == 1 .OR. &
              opt%modNewOutlIdentify == 1 ) .AND. iLine <= mMjd )THEN
           mjdVal = mjdResampl(iLine)
           WRITE(lfnprt,'(1X,F10.4,2X)',ADVANCE='NO') &
                mjdVal
        ELSE
           WRITE(lfnprt,'(1X,10X,2X)',ADVANCE='NO')
        END IF

        IF( opt%modNewJumpIdentify == 1  .AND. iLine <= mMjd )THEN
           WRITE(lfnprt,'(1X,A4,1X,E11.5)',ADVANCE='NO') &
                'JTST', testJump(iLine)
        ELSE
           WRITE(lfnprt,'(1X,4X,1X,11X)',ADVANCE='NO')
        END IF

        IF( opt%modNewVeloIdentify == 1  .AND. iLine <= mMjd )THEN
           WRITE(lfnprt,'(1X,A4,1X,E11.5)',ADVANCE='NO') &
                'VTST', testVelo(iLine)
        ELSE
           WRITE(lfnprt,'(1X,4X,1X,11X)',ADVANCE='NO')
        END IF

        IF( opt%modNewOutlIdentify == 1  .AND. iLine <= mMjd )THEN
           WRITE(lfnprt,'(1X,A4,1X,E11.5)',ADVANCE='NO') &
                'OTST', testOutl(iLine,1)
        ELSE
           WRITE(lfnprt,'(1X,4X,1X,11X)',ADVANCE='NO')
        END IF

        IF( opt%modNewPeriIdentify == 1 .AND. iLine <= sCore%san%nPer )THEN
           perVal = 2*pi/sCore%san%per(iLine)
           WRITE(lfnprt,'(A6,1X,F10.4,1X,E11.5)',ADVANCE='NO') &
                '  PTST', perVal, testPeri(iLine)
        ELSE
           WRITE(lfnprt,'(6X,1X,10X,1X,11X)',ADVANCE='NO')
        END IF

        WRITE(lfnprt,*)

     END DO

  END IF

  ! Deallocation memory
  DEALLOCATE(testJump,stat=iac)
  DEALLOCATE(testVelo,stat=iac)
  DEALLOCATE(testOutl,stat=iac)
  DEALLOCATE(testPeri,stat=iac)
  DEALLOCATE(ddTmp,stat=iac)

  DEALLOCATE(mjdResampl,stat=iac)
  DEALLOCATE(valResampl,stat=iac)

! End of subroutine
! -----------------
!  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodimpod

! -------------------------------------------------------------------------
! Screen model events with information of intervals of no-events
! -------------------------------------------------------------------------
SUBROUTINE fodimpod_chkPresEvntInNotMod( elem, evnt, nEvnt, jjEvnt )

  USE m_bern,     ONLY: i4b, r8b
  USE m_time,     ONLY: OPERATOR(.isIn.)

  USE p_fodits,   ONLY: t_evnt, &
                        infoinit, typeperi, flagnot

  IMPLICIT NONE

  ! Subroutine arguments
  ! --------------------
  TYPE(t_evnt)                                    :: elem
  TYPE(t_evnt),DIMENSION(:),POINTER               :: evnt
  INTEGER(i4b)                                    :: nEvnt
  INTEGER(i4b)                                    :: jjEvnt

  ! Local Variables
  ! ---------------
  INTEGER(i4b)                                    :: iEvnt

  ! Subroutine
  ! ----------

  jjEvnt = 0
  DO iEvnt = 1,nEvnt
     IF( elem%info == infoInit )CYCLE
     IF( elem%type == typePeri )CYCLE
     IF( elem%flag == flagNot )CYCLE
     IF( evnt(iEvnt)%flag /= flagNot )CYCLE
     IF( elem%mjd .isIn. evnt(iEvnt)%timint )THEN
        jjEvnt = iEvnt
        EXIT
     END IF
  END DO

  RETURN

END SUBROUTINE fodimpod_chkPresEvntInNotMod


END MODULE s_FODIMPOD


