MODULE s_FODICHKS
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodichks(mmod,opt,sCore,iSta,stateM)

! -------------------------------------------------------------------------
! Purpose:    Collect, sort information, and define the most probable element
!             to be added/removed from the functional model.
!
! Author:     Luca Ostini
!
! Created:    20-Aug-2009
!
! Changes:    20-Aug-2009 LO: Created this file
!             25-Sep-2009 LO: Changes for F90 consistency
!             18-Nov-2009 LO: Major changes for consistency
!             21-Dec-2009 LO: FFT removed and several changes apported
!             02-Mar-2010 LO: Major changes do to elimination of FODISUBR
!             07-Apr-2010 LO: Major changes do to algorithm and output file
!             16-Jun-2010 LO: Time series diveded by component
!             26-Aug-2010 LO: Architectural changes
!             16-Oct-2010 LO: Remove velocity changes w/o discontinuities
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
  USE m_bern,    ONLY: i4b, r8b, lfnprt, shortLineLength
  USE p_fodits,  ONLY: t_opt, t_sCore, &
                       mremo, infoinit, flagset, typeoutl, significant, &
                       infounkw, typevelo, typejump, mapri, nonsignificant, &
                       miden, candnon, &
                       scoremodaddevnt, scorechkpresevntinmod, &
                       scoresorteventststup, scoresorteventststdown, &
                       scorewriteelemline, scoremoddelevnt, scoreinitnewevnt

! operator, methods:
!  USE d_debug,   ONLY: debug_entry, debug_exit

! subroutines, functions:

! no implicit
  IMPLICIT NONE

! subroutine name
  CHARACTER(LEN=shortLineLength), PARAMETER    :: srName = 'fodichks'

! List of Arguments
! -----------------
! input:
  INTEGER(i4b),INTENT(IN)        :: mmod          ! Working mode

! input/output:
  TYPE(t_opt)                    :: opt           ! Option struct
  TYPE(t_sCore)                  :: sCore         ! Core sturct of FODITS
  INTEGER(i4b)                   :: stateM        ! state machine
  INTEGER(i4b),INTENT(IN)        :: iSta            ! Station index

! output:


! Local Types
! -----------


! Local Parameters
! ----------------


! Local Variables
! ---------------
  CHARACTER(LEN=9)               :: label

  INTEGER(i4b)                   :: iMjd
  INTEGER(i4b)                   :: nVal
  INTEGER(i4b)                   :: iEvnt
  INTEGER(i4b)                   :: jEvnt
  INTEGER(i4b)                   :: jjEvnt
  INTEGER(i4b)                   :: kkEvnt
  INTEGER(i4b)                   :: nOutlRemoved

  REAL(r8b)                      :: deltaMjd

! Call debug routine
! ------------------
!  CALL debug_entry(srName)


! Initialization of all variables
! -------------------------------
  nVal = sCore%nVal
  stateM = 5

  ! Remove iElem from %amod
  ! =======================
  IF( mmod == mRemo )THEN

     ! Sort the tested elements increasing order
     CALL sCoreSortEventsTstDown( sCore%mdl%amod, sCore%mdl%nAmod )

     ! Remove all non-significnat outliers
     nOutlRemoved = 0
     DO
        jjEvnt = 0
        DO iEvnt = 1,sCore%mdl%nAmod
           ! Filter
           IF( sCore%mdl%amod(iEvnt)%info == infoInit )CYCLE
           IF( sCore%mdl%amod(iEvnt)%flag == flagSet )CYCLE
           IF( sCore%mdl%amod(iEvnt)%type /= typeOutl )CYCLE
           IF( sCore%mdl%amod(iEvnt)%siTst == significant )CYCLE
           ! Verbose mode (normal)
           IF( opt%outFileVerbose == 1 )THEN
              label = 'RemoAmod'
              CALL sCoreWriteElemLine( sCore%mdl%amod(iEvnt), &
                                       label, sCore%lsa%m0)
           END IF
           ! Remove the element form %amod
           CALL sCoreModDelEvnt(sCore%mdl%amod, sCore%mdl%nAmod, iEvnt)
           ! Found value - CONTINUE
           stateM = 0
           ! Increment
           jjEvnt = 1
           nOutlRemoved = nOutlRemoved + 1
           EXIT
        END DO
        IF( jjEvnt == 0 )EXIT
     END DO

     ! Remove the most non-significant element which is not an outlier
     IF( nOutlRemoved == 0 )THEN
        DO iEvnt = 1,sCore%mdl%nAmod
           ! Filter
           IF( sCore%mdl%amod(iEvnt)%info == infoInit )CYCLE
           IF( sCore%mdl%amod(iEvnt)%flag == flagSet )CYCLE
           IF( sCore%mdl%amod(iEvnt)%type == typeOutl )CYCLE
           IF( sCore%mdl%amod(iEvnt)%siTst == significant )CYCLE
           ! Verbose mode (normal)
           IF( opt%outFileVerbose == 1 )THEN
              label = 'RemoAmod'
              CALL sCoreWriteElemLine(sCore%mdl%amod(iEvnt),label,sCore%lsa%m0)
           END IF
           ! Remove the element form %amod
           CALL sCoreModDelEvnt(sCore%mdl%amod, sCore%mdl%nAmod, iEvnt)
           ! Found value - CONTINUE
           stateM = 0
           ! Only one element - the least non-significant one
           EXIT
        END DO
     END IF

     ! Remove the velocity changes not related to discontinuities if no
     ! search for new velocity changes is enabled
     IF( nOutlRemoved == 0 .AND. opt%modNewVeloIdentify == 0 )THEN
        DO
           jjEvnt = 0
           DO iEvnt = 1,sCore%mdl%nAmod
              ! Filter
              IF( sCore%mdl%amod(iEvnt)%info /= infoUnkw )CYCLE
              IF( sCore%mdl%amod(iEvnt)%type /= typeVelo )CYCLE
              IF( sCore%mdl%amod(iEvnt)%flag == flagSet )CYCLE
              ! Define element to test the presence in %amod
              CALL sCoreInitNewEvnt( nVal, sCore%mdl%evnt )
              sCore%mdl%evnt = sCore%mdl%amod(iEvnt)
              sCore%mdl%evnt%type = typeJump
              ! Test the presence in %amod
              CALL sCoreChkPresEvntInMod( sCore%mdl%evnt, &
                   sCore%mdl%amod, sCore%mdl%nAmod, jEvnt )
              ! Remove the identified evnt
              IF( jEvnt == 0 )THEN
                 CALL sCoreModDelEvnt( sCore%mdl%amod, sCore%mdl%nAmod, iEvnt )
                 jjEvnt = 1
                 EXIT
              END IF
           END DO
        IF( jjEvnt == 0 )EXIT
        END DO
     END IF

     ! Verbose mode
     IF( opt%outFileVerbose == 1 )THEN
        ! Iteration and screening steps
        WRITE(lfnprt,'(A)') &
             ' | --- REMO-END: Remove non-significant elements from model'
     END IF

  ! Insert iElem of %apri into %amod
  ! ================================
  ELSE IF( mmod == mApri )THEN

     ! Sort the tested elements increasing order
     CALL sCoreSortEventsTstUp( sCore%mdl%apri, sCore%mdl%nApri )

     ! All elements considered
     DO iEvnt = 1,sCore%mdl%nApri
        ! Cycle if non-significant
        IF( sCore%mdl%apri(iEvnt)%siTst == nonSignificant )CYCLE
        ! Cycle if %apri(iEvnt) exists already in %amod
        jjEvnt = 0
        CALL sCoreChkPresEvntInMod( sCore%mdl%apri(iEvnt), &
                                    sCore%mdl%amod, sCore%mdl%nAmod, &
                                    jjEvnt )
        IF( jjEvnt > 0 )CYCLE
        ! Make sure that minimal interval for velocity changes is fulfilled
        jjEvnt = 0
        DO jEvnt = 1,sCore%mdl%nAmod
           IF( sCore%mdl%apri(iEvnt)%type == typeVelo .AND. &
               sCore%mdl%amod(jEvnt)%type == typeVelo .AND. &
               ABS( sCore%mdl%apri(iEvnt)%mjd - &
                    sCore%mdl%amod(jEvnt)%mjd ) < opt%minIntervForVel )THEN
              jjEvnt = 1
              EXIT
           END IF
        END DO
        IF( jjEvnt > 0 )CYCLE
        ! Check the the minimal interval for velocities changes is fulfilled
        ! at the boundaries (left and right) of the time series, too.
        ! left
        deltaMjd = ABS( sCore%mdl%apri(iEvnt)%mjd - &
                        sCore%sta(iSta)%ts%mjd(1) )
        IF( deltaMjd < opt%minIntervForVel ) jjEvnt = 1
        ! right
        deltaMjd = ABS( sCore%mdl%apri(iEvnt)%mjd - &
                        sCore%sta(iSta)%ts%mjd(sCore%sta(ista)%ts%nMjd) )
        IF( deltaMjd < opt%minIntervForVel ) jjEvnt = 1
        IF( sCore%mdl%apri(iEvnt)%type == typeVelo .AND. jjEvnt > 0 )CYCLE
        ! Check minimal distance between two velocity changes (=3 epochs)
        kkEvnt = 0
        DO jEvnt = 1,sCore%mdl%nAmod
           jjEvnt = 0
           IF( sCore%mdl%apri(iEvnt)%type == typeVelo .AND. &
               sCore%mdl%amod(jEvnt)%type == typeVelo )THEN
              IF( sCore%mdl%apri(iEvnt)%mjd >= sCore%mdl%amod(jEvnt)%mjd )THEN
                 DO iMjd = 1,sCore%sta(ista)%ts%nMjd
                    IF( sCore%sta(iSta)%ts%mjd(iMjd) <= &
                        sCore%mdl%amod(jEvnt)%mjd )CYCLE
                    IF( sCore%sta(iSta)%ts%mjd(iMjd) > &
                        sCore%mdl%apri(iEvnt)%mjd )EXIT
                    jjEvnt = jjEvnt + 1
                 END DO
              ELSE
                 DO iMjd = 1,sCore%sta(ista)%ts%nMjd
                    IF( sCore%sta(iSta)%ts%mjd(iMjd) <= &
                        sCore%mdl%amod(jEvnt)%mjd )CYCLE
                    IF( sCore%sta(iSta)%ts%mjd(iMjd) > &
                        sCore%mdl%apri(iEvnt)%mjd )EXIT
                    jjEvnt = jjEvnt + 1
                 END DO
              END IF
           END IF
           IF( jjEvnt > 0 .AND. jjEvnt < 3 )THEN
              kkEvnt = 1
              EXIT
           END IF
        END DO
        IF( kkEvnt == 1 )CYCLE
        ! Add %apri(iEvnt) into %amod
        CALL sCoreModAddEvnt( sCore%mdl%apri(iEvnt), &
                              sCore%mdl%amod, sCore%mdl%nAmod )
        ! Found value - CONTINUE
        stateM = 0
        ! Verbose mode (normal)
        IF( opt%outFileVerbose == 1 )THEN
           label = 'InsrApri'
           CALL sCoreWriteElemLine( sCore%mdl%apri(iEvnt), label, sCore%lsa%m0)
        END IF
        ! Only one element - the first one
        EXIT
     END DO

     ! Verbose mode
     IF( opt%outFileVerbose == 1 )THEN
        ! Iteration and screening steps
        WRITE(lfnprt,'(A)') &
             ' | --- APRI-END: Insert significant apriori elements in model'
     END IF

     ! Report
     IF( stateM == 5 .AND. opt%outFileVerbose == 1 )THEN
        ! Iteration and screening steps
        WRITE(lfnprt,'(A)') ' | --- ENDS: End of screening loop'
     END IF

  ! Insert iElem of %iden into %amod
  ! ================================
  ELSE IF( mmod == mIden )THEN

     ! Sort the tested elements increasing order
     jjEvnt = 0
     CALL sCoreSortEventsTstUp( sCore%mdl%iden, sCore%mdl%nIden )

     ! Insert the most significant element which is not an outlier
     DO iEvnt = 1,sCore%mdl%nIden
        ! Filter
        IF( sCore%mdl%iden(iEvnt)%type == typeOutl )CYCLE
        ! Cycle if non-significant
        IF( sCore%mdl%iden(iEvnt)%siTst == nonSignificant )CYCLE
        ! Cycle if %iden(iEvnt) exists already in %amod
        CALL sCoreChkPresEvntInMod( sCore%mdl%iden(iEvnt), &
                                    sCore%mdl%amod, sCore%mdl%nAmod, &
                                    jjEvnt )
        IF( jjEvnt > 0 )CYCLE
        ! Make sure that minimal interval for velocity changes is fulfilled
        jjEvnt = 0
        DO jEvnt = 1,sCore%mdl%nAmod
           IF( sCore%mdl%iden(iEvnt)%type == typeVelo .AND. &
               sCore%mdl%amod(jEvnt)%type == typeVelo .AND. &
               ABS( sCore%mdl%iden(iEvnt)%mjd - &
                    sCore%mdl%amod(jEvnt)%mjd ) < opt%minIntervForVel )THEN
              jjEvnt = 1
              EXIT
           END IF
        END DO
        IF( jjEvnt > 0 )CYCLE
        ! Do not insert velocity changes not related with jumps if no
        ! search for new velocity changes is enabled
        IF( sCore%mdl%iden(iEvnt)%type == typeVelo .AND. &
            opt%modNewVeloIdentify == 0 )THEN
           ! Define element to test the presence in %amod
           CALL sCoreInitNewEvnt( nVal, sCore%mdl%evnt )
           sCore%mdl%evnt = sCore%mdl%iden(iEvnt)
           sCore%mdl%evnt%type = typeJump
           ! Test the presence in %amod
           CALL sCoreChkPresEvntInMod( sCore%mdl%evnt, &
                sCore%mdl%amod, sCore%mdl%nAmod, jEvnt )
           IF( jEvnt == 0 )CYCLE
        END IF
        ! Check the the minimal interval for velocities changes is fulfilled
        ! at the boundaries (left and right) of the time series, too.
        ! left
        deltaMjd = ABS( sCore%mdl%iden(iEvnt)%mjd - &
                        sCore%sta(iSta)%ts%mjd(1) )
        IF( deltaMjd < opt%minIntervForVel ) jjEvnt = 1
        ! right
        deltaMjd = ABS( sCore%mdl%iden(iEvnt)%mjd - &
                        sCore%sta(iSta)%ts%mjd(sCore%sta(ista)%ts%nMjd) )
        IF( deltaMjd < opt%minIntervForVel ) jjEvnt = 1
        IF( sCore%mdl%iden(iEvnt)%type == typeVelo .AND. jjEvnt > 0 )CYCLE
        ! Check minimal distance between two velocity changes (=3 epochs)
        kkEvnt = 0
        DO jEvnt = 1,sCore%mdl%nAmod
           jjEvnt = 0
           IF( sCore%mdl%iden(iEvnt)%type == typeVelo .AND. &
               sCore%mdl%amod(jEvnt)%type == typeVelo )THEN
              IF( sCore%mdl%iden(iEvnt)%mjd >= sCore%mdl%amod(jEvnt)%mjd )THEN
                 DO iMjd = 1,sCore%sta(ista)%ts%nMjd
                    IF( sCore%sta(iSta)%ts%mjd(iMjd) <= &
                        sCore%mdl%amod(jEvnt)%mjd )CYCLE
                    IF( sCore%sta(iSta)%ts%mjd(iMjd) > &
                        sCore%mdl%iden(iEvnt)%mjd )EXIT
                    jjEvnt = jjEvnt + 1
                 END DO
              ELSE
                 DO iMjd = 1,sCore%sta(ista)%ts%nMjd
                    IF( sCore%sta(iSta)%ts%mjd(iMjd) <= &
                        sCore%mdl%amod(jEvnt)%mjd )CYCLE
                    IF( sCore%sta(iSta)%ts%mjd(iMjd) > &
                        sCore%mdl%iden(iEvnt)%mjd )EXIT
                    jjEvnt = jjEvnt + 1
                 END DO
              END IF
           END IF
           IF( jjEvnt > 0 .AND. jjEvnt < 3 )THEN
              kkEvnt = 1
              EXIT
           END IF
        END DO
        IF( kkEvnt == 1 )CYCLE
        ! Add %iden(iEvnt) into %amod
        CALL sCoreModAddEvnt( sCore%mdl%iden(iEvnt), &
                              sCore%mdl%amod, sCore%mdl%nAmod )
        ! Found value - CONTINUE
        stateM = 0
        ! Verbose mode (normal)
        IF( opt%outFileVerbose == 1 )THEN
           label = 'InsrIden'
           CALL sCoreWriteElemLine( sCore%mdl%iden(iEvnt), label, sCore%lsa%m0)
        END IF
        ! Only one element - the first one
        EXIT
     END DO
     ! Insert the most significant outliers
     DO iEvnt = 1,sCore%mdl%nIden
        ! Filter
        IF( sCore%mdl%iden(iEvnt)%type /= typeOutl )CYCLE
        ! Cycle if non-significant
        IF( sCore%mdl%iden(iEvnt)%siTst == nonSignificant )CYCLE
        ! Cycle if %iden(iEvnt) exists already in %amod
        CALL sCoreChkPresEvntInMod( sCore%mdl%iden(iEvnt), &
                                    sCore%mdl%amod, sCore%mdl%nAmod, &
                                    jjEvnt )
        IF( jjEvnt > 0 )CYCLE
        ! Add %iden(iEvnt) into %amod
        CALL sCoreModAddEvnt( sCore%mdl%iden(iEvnt), &
                              sCore%mdl%amod, sCore%mdl%nAmod )
        ! Found value - CONTINUE
        stateM = 0
        ! Verbose mode (normal)
        IF( opt%outFileVerbose == 1 )THEN
           label = 'InsrOutl'
           CALL sCoreWriteElemLine( sCore%mdl%iden(iEvnt), label, sCore%lsa%m0)
        END IF
     END DO

     IF( opt%outFileVerbose == 1 )THEN
        ! Iteration and screening steps
        WRITE(lfnprt,'(A)') &
             ' | --- IDEN-END: Insert significant identified elements in model'
     END IF

  END IF

  ! Reset candidates (%cand) in %amod
  DO iEvnt = 1,sCore%mdl%nAmod
     sCore%mdl%amod(iEvnt)%cand  = candNon
  END DO

! End of subroutine
! -----------------
!  CALL debug_exit(srName)

  RETURN

END SUBROUTINE fodichks

END MODULE s_FODICHKS
