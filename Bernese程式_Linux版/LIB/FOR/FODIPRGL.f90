MODULE s_FODIPRGL
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodiprgl(loop,opt,sCore,iSta,stateM)

! -------------------------------------------------------------------------
! Purpose:    Take decision to exit if the %amod model of screeing or
!             iteration step is equal of one of the previous steps.
!
! Author:     Luca Ostini
!
! Created:    14-Aug-2008
!
! Changes:    14-Aug-2008 LO: Created this file
!             02-Oct-2008 LO: First revision
!             09-Oct-2008 LO: Third revision
!             05-Dec-2008 LO: Fourth revisio: velocity changes allowed
!             11-Feb-2009 LO: Fifth revision: major changes
!             25-Sep-2009 LO: Changes for F90 consistency
!             21-Dec-2009 LO: FFT removed and several changes apported
!             16-Jun-2010 LO: Time series diveded by component
!             26-Aug-2010 LO: Architectural changes
!             03-Jan-2010 LO: INTENT(INOUT) removed and bug fixed
!             19-Sep-2012 RD: Use P_FODITS with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used Modules
! ------------
! structures, variables:
  USE m_bern,    ONLY: i4b, r8b, lfnprt, lfnerr, shortLineLength
  USE d_const,   ONLY: PI
  USE p_fodits,  ONLY: t_opt, t_sCore, &
                       typejump, typevelo, typeperi, typeoutl, &
                       lscrn, liter, infoinit, infounkw, &
                       scorechkpresevntinmod

! operator, methods:
!  USE d_debug,   ONLY: debug_entry, debug_exit

! subroutines, functions:
  USE s_alcerr
  USE s_exitrc

! no implicit
  IMPLICIT NONE

! subroutine name


! List of Arguments
! -----------------
! input:
  INTEGER(i4b),INTENT(IN)        :: loop            ! Loop

! input/output:
  TYPE(t_opt)                    :: opt             ! Option structure
  TYPE(t_sCore)                  :: sCore           ! Core structure of FODITS
  INTEGER(i4b),INTENT(IN)        :: iSta            ! Station index
  INTEGER(i4b)                   :: stateM          ! state machine

! output:


! subroutine name
  CHARACTER(LEN=shortLineLength), PARAMETER    :: srName = 'fodiprgl'

! Local Types
! -----------


! Local Parameters
! ----------------


! Local Variables
! ---------------
  CHARACTER(LEN=11)              :: outModeTxt

  INTEGER(i4b)                   :: iac
  INTEGER(i4b)                   :: iMjd
  INTEGER(i4b)                   :: jMjd
  INTEGER(i4b)                   :: iVal
  INTEGER(i4b)                   :: nVal
  INTEGER(i4b)                   :: indM
  INTEGER(i4b)                   :: iEvnt
  INTEGER(i4b)                   :: jjEvnt
  INTEGER(i4b)                   :: iIter
  INTEGER(i4b)                   :: seen
  INTEGER(i4b)                   :: prevModSeen

  REAL(r8b)                      :: sumEvnts
  REAL(r8b)                      :: sumOutl
  REAL(r8b)                      :: modVal
  REAL(r8b),DIMENSION(:),&
            ALLOCATABLE          :: valMod

! Call debug routine
! ------------------
!  CALL debug_entry(srName)


  ! Initialization of all variables
  ! -------------------------------
  stateM = 0
  nVal = sCore%nVal

  ! Check whether the new model is equal the previous one
  ! -----------------------------------------------------

  ! Allocate memory
  ALLOCATE(valMod(nVal),stat=iac)
  CALL alcerr(iac,'valMod',(/nVal/),srName)

  ! Collect info on jump, velo, peri
  sumEvnts = 0.0D0
  ! DISC
  sumEvnts = sumEvnts + 10.0D6
  DO iEvnt = 1,sCore%mdl%nAmod
     IF( sCore%mdl%amod(iEvnt)%type /= typeJump )CYCLE
     sumEvnts = sumEvnts + sCore%mdl%amod(iEvnt)%mjd
  END DO
  ! VELO
  sumEvnts = sumEvnts + 10.0D6
  DO iEvnt = 1,sCore%mdl%nAmod
     IF( sCore%mdl%amod(iEvnt)%type /= typeVelo )CYCLE
     sumEvnts = sumEvnts + sCore%mdl%amod(iEvnt)%mjd
  END DO
  ! PERI
  sumEvnts = sumEvnts + 10.0D6
  DO iEvnt = 1,sCore%mdl%nAmod
     IF( sCore%mdl%amod(iEvnt)%type /= typePeri )CYCLE
     sumEvnts = sumEvnts + 2*pi/sCore%mdl%amod(iEvnt)%omega
  END DO

  ! OUTL
  sumOutl = 0.0D0
  DO iEvnt = 1,sCore%mdl%nAmod
     IF( sCore%mdl%amod(iEvnt)%type /= typeOutl )CYCLE
     sumOutl = sumOutl + 1.0D0
  END DO

  prevModSeen = 0

  ! Screening loop
  IF( loop == lScrn )THEN

     ! Check whether the functional model is the same as one of the previous
     ! screening steps
     DO iIter = 1,sCore%ctr%nScrnLoop-1
        IF( sCore%ctr%modNumScrn(iIter) == sumEvnts )THEN
           prevModSeen = 1
           IF( sumOutl /= 0.0D0 )THEN
              IF( ABS(sCore%ctr%modNumScrnOutl(iIter)/sumOutl-1) < 0.2D0 )THEN
                 EXIT
              ELSE
                 prevModSeen = 0
              END IF
           END IF
        END IF
     END DO
     sCore%ctr%modNumScrn(sCore%ctr%nScrnLoop) = sumEvnts
     sCore%ctr%modNumScrnOutl(sCore%ctr%nScrnLoop) = sumOutl

     ! Decision to exit
     IF( prevModSeen == 1 )THEN
        stateM = 8
     END IF

  ! Iteration loop
  ELSE IF( loop == lIter )THEN

     ! Check whether the functional model is the same as one of the previous
     ! iteration steps
     DO iIter = 1,sCore%ctr%nIterLoop-1
        IF( sCore%ctr%modNumIter(iIter) == sumEvnts )THEN
           prevModSeen = 1
           IF( sumOutl /= 0.0D0 )THEN
              IF( ABS(sCore%ctr%modNumIterOutl(iIter)/sumOutl-1) < 0.2D0 )THEN
                 EXIT
              ELSE
                 prevModSeen = 0
              END IF
           END IF
        END IF
     END DO
     sCore%ctr%modNumIter(sCore%ctr%nIterLoop) = sumEvnts
     sCore%ctr%modNumIterOutl(sCore%ctr%nIterLoop) = sumOutl

     ! Decision to exit
     IF( ( opt%modNewJumpIdentify == 0 .AND. &
           opt%modNewVeloIdentify == 0 .AND. &
           opt%modNewOutlIdentify == 0 .AND. &
           opt%modNewPeriIdentify == 0 ) .OR. ( prevModSeen == 1 ) )THEN
        stateM = 8
     END IF

  END IF

  ! Report test results from %amod to %apri
  DO iEvnt = 1,sCore%mdl%nAmod
     IF( sCore%mdl%amod(iEvnt)%info == infoInit )CYCLE
     IF( sCore%mdl%amod(iEvnt)%info == infoUnkw )CYCLE
     ! Locate iElem in %apri
     CALL sCoreChkPresEvntInMod( sCore%mdl%amod(iEvnt) , &
          sCore%mdl%apri, sCore%mdl%nApri, jjEvnt )
     ! Report the values and test results
     IF( jjEvnt > 0 )THEN
        sCore%mdl%apri(jjEvnt) = sCore%mdl%amod(iEvnt)
     ELSE
        WRITE(lfnerr,'(/,A,/)') ' *** SR FODIPRGL: Element not found.'
        CALL exitrc(2)
     END IF
  END DO

  ! Verbose mode
  ! ------------

  IF( opt%outFileVerbose == 1 .AND. stateM == 8 .AND. loop == lScrn )THEN
     ! Iteration and screening steps
     WRITE(lfnprt,'(A)') ' | --- ENDS: End of screening loop'
  END IF

  ! Write observations and model
  IF( opt%outFileVerbose == 1 .AND. &
      ( ( loop == lIter .AND. opt%outFileVerboseT == 1  ) .OR. &
        ( loop == lScrn .AND. opt%outFileVerboseTT == 1 ) ) )THEN
     ! Verbosity mode: write Observations and Model
     jMjd = 0
     DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
        outModeTxt = 'O-M-ITR-SCR'
        WRITE(lfnprt,'(A,1X,A16,1X,A,I02,1X,A,I02,1X,F10.4)',ADVANCE='NO') &
             outModeTxt, &
             sCore%sta(iSta)%name, &
             'I=',sCore%ctr%nIterLoop, &
             'S=',sCore%ctr%nScrnLoop, &
             sCore%sta(iSta)%ts%mjd(iMjd)
        seen = 0
        DO iEvnt = 1,sCore%mdl%nAmod
           IF( sCore%mdl%amod(iEvnt)%type /= typeOutl )CYCLE
           IF( sCore%mdl%amod(iEvnt)%mjd /= sCore%sta(iSta)%ts%mjd(iMjd) )CYCLE
           seen = 1
           valMod(:) = sCore%mdl%amod(iEvnt)%mod(:) + &
                       sCore%mdl%amod(iEvnt)%par(:)
        END DO
        IF( seen == 0 )THEN
           jMjd = jMjd + 1
        END IF
        DO iVal = 1,nVal
           IF( seen == 1 )THEN
              modVal = valMod(iVal)
           ELSE
              indM = nVal*(jMjd-1)
              modVal = sCore%lsa%mod(indM+iVal)
           END IF
           WRITE(lfnprt,'(1X,I3,2(1X,E18.11))',ADVANCE='NO') &
                iVal, sCore%sta(iSta)%ts%val(iMjd,iVal), modVal
        END DO
        DO iVal = nVal+1,3
           WRITE(lfnprt,'(1X,I3,2(1X,E18.11))',ADVANCE='NO') &
                iVal, &
                0.0D0, &
                0.0D0
        END DO

        ! Line feed
        WRITE(lfnprt,*)
     END DO
  END IF

  ! End iteration step
  IF( opt%outFileVerbose == 1 .AND. stateM == 8 .AND. loop == lScrn )THEN
     WRITE(lfnprt,'(A)') ' | --- ENDI: End of iteration loop'
  END IF

  ! Deallocate memory
  DEALLOCATE(valMod,stat=iac)

! End of subroutine
! -----------------
!  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodiprgl

END MODULE s_FODIPRGL


