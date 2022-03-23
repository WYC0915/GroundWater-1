MODULE s_FODISSEV
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodissev(mmod, opt,sCore,iSta,iElem,stateM)

! -------------------------------------------------------------------------
! Purpose:    Insert iElem of %apri into %amod to be tested.
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
!             18-Nov-2009 LO: Major changes for consistency
!             21-Dec-2009 LO: FFT removed and several changes apported
!             02-Mar-2010 LO: Major changes do to elimination of FODISUBR
!             07-Apr-2010 LO: Major changes do to algorithm and output file
!             16-Jun-2010 LO: Time series diveded by component
!             26-Aug-2010 LO: Architectural changes
!             03-Jan-2010 LO: INTENT(INOUT) removed and bug fixed
!             19-Jul-2011 LO: Test datum defintion added
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
  USE m_bern,    ONLY: i4b, r8b, lfnprt
  USE p_fodits,  ONLY: t_opt, t_score, &
                       candsub, candyes, flagnot, flagset, infoinit, &
                       mapri, miden, mremo, nonsignificant, typeoutl, &
                       scorechkpresevntinmod, scoremodaddevnt

! operator, methods:
  USE m_time,    ONLY: OPERATOR(.isIn.)
!  USE d_debug,   ONLY: debug_entry, debug_exit

! subroutines, functions:

! no implicit
  IMPLICIT NONE

! subroutine name
!  CHARACTER(LEN=8), PARAMETER    :: srName = 'fodissev'


! List of Arguments
! -----------------
! input:
  INTEGER(i4b),INTENT(IN)        :: mmod          ! Working mode
  INTEGER(i4b),INTENT(IN)        :: iElem

! input/output:
  TYPE(t_opt)                    :: opt           ! Option structure
  TYPE(t_sCore)                  :: sCore         ! Core structure of FODITS
  INTEGER(i4b)                   :: iSta          ! Station index
  INTEGER(i4b)                   :: stateM

! output:


! Local Types
! -----------


! Local Parameters
! ----------------


! Local Variables
! ---------------
  INTEGER(i4b)                   :: jjEvnt

! Call debug routine
! ------------------
!  CALL debug_entry(srName)


! Initialization of all variables
! -------------------------------
  stateM = 0

  ! Remove iElem from %amod
  ! =======================
  IF( mmod == mRemo )THEN

     ! Verbose mode
     IF( opt%outFileVerbose == 1 .AND. iElem == 1 .AND. &
         sCore%ctr%nRemoLoop == 1 )THEN
        ! Iteration and screening steps
        WRITE(lfnprt,'(A,A,A,I04,5X,A,I04)') &
             ' | ', sCore%sta(iSta)%name, &
             ', Iteration step: ', sCore%ctr%nIterLoop, &
             ', Screening step: ', sCore%ctr%nScrnLoop
     END IF

     ! Verbose mode
     IF( opt%outFileVerbose == 1 .AND. iElem == 1 )THEN
        ! Iteration and screening steps
        WRITE(lfnprt,'(A)') &
             ' | --- REMO-BEG: Remove non-significant elements from model'
     END IF

     ! Only parameters that are not permanently set in %amod
     IF( sCore%mdl%amod(iElem)%flag == flagSet )THEN
        stateM = 1
        !  CALL debug_exit(srName)
        RETURN
     END IF

     ! No initial parameters set in %amod
     IF( sCore%mdl%amod(iElem)%info == infoInit )THEN
        stateM = 1
        !  CALL debug_exit(srName)
        RETURN
     END IF

     ! No outliers tested by %vtv
     IF( sCore%mdl%amod(iElem)%type == typeOutl )THEN
        stateM = 1
        !  CALL debug_exit(srName)
        RETURN
     END IF

     ! Temporaly unset %amod(iElem) of %amod
     sCore%mdl%amod(iElem)%cand = candSub

  ! Insert iElem of %apri into %amod
  ! ================================
  ELSE IF( mmod == mApri )THEN

     ! Verbose mode
     IF( opt%outFileVerbose == 1 .AND. iElem == 1 )THEN
        ! Iteration and screening steps
        WRITE(lfnprt,'(A)') &
             ' | --- APRI-BEG: Insert significant apriori elements in model'
     END IF

     ! Filter
     IF( sCore%mdl%apri(iElem)%flag == flagNot )THEN
        stateM = 1
        !  CALL debug_exit(srName)
        RETURN
     END IF

     !! ! No test for outliers
     !! IF( sCore%mdl%apri(iElem)%type == typeOutl )THEN
     !!    stateM = 1
     !!    !  CALL debug_exit(srName)
     !!    RETURN
     !! END IF

     ! Reset test and significance significance
     sCore%mdl%apri(iElem)%stTst = 0.0D0
     sCore%mdl%apri(iElem)%vlTst = 0.0D0
     sCore%mdl%apri(iElem)%siTst = nonSignificant

     ! Return if %apri(iElem) is included in %amod
     CALL sCoreChkPresEvntInMod( sCore%mdl%apri(iElem), &
                                 sCore%mdl%amod, sCore%mdl%nAmod, &
                                 jjEvnt)
     IF( jjEvnt > 0 )THEN
        stateM = 1
        !  CALL debug_exit(srName)
        RETURN
     END IF

     ! Insert %apri(iElem) into %amod
     CALL sCoreModAddEvnt(sCore%mdl%apri(iElem), &
                          sCore%mdl%amod, sCore%mdl%nAmod)

     ! Set the the iElem as candidate
     sCore%mdl%amod(sCore%mdl%nAmod)%cand = candYes

  ! Insert iElem of %iden into %amod
  ! ================================
  ELSE IF( mmod == mIden )THEN

     IF( opt%outFileVerbose == 1 .AND. iElem == 1 )THEN
        ! Iteration and screening steps
        WRITE(lfnprt,'(A)') &
             ' | --- IDEN-BEG: Insert significant identified elements in model'
     END IF

     ! No test for outliers
     IF( sCore%mdl%iden(iElem)%type == typeOutl )THEN
        stateM = 1
        !  CALL debug_exit(srName)
        RETURN
     END IF

     ! Reset test and significance significance
     sCore%mdl%iden(iElem)%stTst = 0.0D0
     sCore%mdl%iden(iElem)%vlTst = 0.0D0
     sCore%mdl%iden(iElem)%siTst = nonSignificant

     ! Return if %iden(iElem) is included in %amod
     CALL sCoreChkPresEvntInMod( sCore%mdl%iden(iElem), &
                                 sCore%mdl%amod, sCore%mdl%nAmod, &
                                 jjEvnt)
     IF( jjEvnt > 0 )THEN
        stateM = 1
        !  CALL debug_exit(srName)
        RETURN
     END IF

     ! Insert %iden(iElem) into %amod
     CALL sCoreModAddEvnt(sCore%mdl%iden(iElem), &
                          sCore%mdl%amod, sCore%mdl%nAmod)

     ! Set the the iElem as candidate
     sCore%mdl%amod(sCore%mdl%nAmod)%cand = candYes

  END IF

! End of subroutine
! -----------------
!  CALL debug_exit(srName)

 RETURN

END SUBROUTINE fodissev

END MODULE s_FODISSEV
