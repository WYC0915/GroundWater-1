MODULE s_FODIPTST
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodiptst(mmod,opt,sCore)

! -------------------------------------------------------------------------
! Purpose:    Remove canditate elements from %amod and report the result
!             to %apri.
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
!             13-Aug-2009 LO: Problem of velocity changes fixed.
!             18-Aug-2009 LO: Vertical and horizontal tested fixed.
!             25-Sep-2009 LO: Changes for F90 consistency
!             18-Nov-2009 LO: Major changes for consistency
!             21-Dec-2009 LO: FFT removed and several changes apported
!             01-Feb-2010 LO: Additional criteria addded for velo and peri
!             02-Mar-2010 LO: Major changes do to elimination of FODISUBR
!             07-Apr-2010 LO: Major changes do to algorithm and output file
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
  USE m_bern,    ONLY: i4b, r8b, lfnerr, shortLineLength
  USE p_fodits,  ONLY: t_opt, t_sCore, &
                       mremo, candsub, candnon, mapri, candyes, miden, &
                       scorewriteelemline, scorechkpresevntinmod, &
                       scoremoddelevnt

! operator, methods:
!  USE d_debug,   ONLY: debug_entry, debug_exit

! subroutines, functions:
  USE s_exitrc

! no implicit
  IMPLICIT NONE

! subroutine name
  CHARACTER(LEN=shortLineLength), PARAMETER    :: srName = 'fodiptst'


! List of Arguments
! -----------------
! input:
  INTEGER(i4b),INTENT(IN)        :: mmod          ! Working mode

! input/output:
  TYPE(t_opt)                    :: opt           ! Option structure
  TYPE(t_sCore)                  :: sCore         ! Core structure of FODITS

! output:


! Local Types
! -----------


! Local Parameters
! ----------------


! Local Variables
! ---------------
  CHARACTER(LEN=9)               :: label

  INTEGER(i4b)                   :: iEvnt
  INTEGER(i4b)                   :: jjEvnt


! Call debug routine
! ------------------
!  CALL debug_entry(srName)


! Initialization of all variables
! -------------------------------

  ! Remove iElem from %amod
  ! =======================
  IF( mmod == mRemo )THEN

     ! Reactivate %amod(iElem)==candSub
     ! (the report of the test result is in fodiadas)
     DO iEvnt = 1,sCore%mdl%nAmod
        IF( sCore%mdl%amod(iEvnt)%cand == candSub )THEN
           sCore%mdl%amod(iEvnt)%cand = candNon
           IF( opt%outFileVerbose == 1 )THEN
              label = 'RemoCand'
              CALL sCoreWriteElemLine( sCore%mdl%amod(iEvnt), &
                   label, sCore%lsa%m0 )
           END IF
        END IF
     END DO

  ! Insert iElem of %apri into %amod
  ! ================================
  ELSE IF( mmod == mApri )THEN

     ! Remove candidate element (iElem => %cand==candYes) form %amod and
     ! report the test result back to %apri
     DO
        jjEvnt = 0
        DO iEvnt = 1,sCore%mdl%nAmod
           IF( sCore%mdl%amod(iEvnt)%cand == candYes )THEN
              ! Remove jjEvnt as candidate
              sCore%mdl%amod(iEvnt)%cand = candNon
              ! Locate iElem in %apri
              CALL sCoreChkPresEvntInMod( sCore%mdl%amod(iEvnt), &
                                          sCore%mdl%apri, sCore%mdl%nApri, &
                                          jjEvnt )
              ! Report the values and test results
              IF( jjEvnt > 0 )THEN
                 sCore%mdl%apri(jjEvnt) = sCore%mdl%amod(iEvnt)
                 ! Evnt to remove in %amod
                 jjEvnt = iEvnt
                 ! Verbose mode (normal)
                 IF( opt%outFileVerbose == 1 )THEN
                    label = 'ApriCand'
                    CALL sCoreWriteElemLine( sCore%mdl%amod(iEvnt),&
                         label, sCore%lsa%m0 )
                 END IF
              ELSE
                 WRITE(lfnerr,'(/,A,/)') ' *** SR FODIPTST: Element not found.'
                 CALL exitrc(2)
              END IF
              EXIT
           END IF
        END DO
        IF( jjEvnt > 0 )THEN
           CALL sCoreModDelEvnt( sCore%mdl%amod, sCore%mdl%nAmod, jjEvnt)
        ELSE
           EXIT
        END IF
     END DO

  ! Insert iElem of %iden into %amod
  ! ================================
  ELSE IF( mmod == mIden )THEN

     ! Remove candidate element (iElem => %cand==candYes) form %amod and
     ! report the test result back to %iden
     DO
        jjEvnt = 0
        DO iEvnt = 1,sCore%mdl%nAmod
           IF( sCore%mdl%amod(iEvnt)%cand == candYes )THEN
              ! Remove jjEvnt as candidate
              sCore%mdl%amod(iEvnt)%cand = candNon
              ! Locate iElem in %iden
              CALL sCoreChkPresEvntInMod( sCore%mdl%amod(iEvnt), &
                                          sCore%mdl%iden, sCore%mdl%nIden, &
                                          jjEvnt )
              ! Report the values and test results
              IF( jjEvnt > 0 )THEN
                 sCore%mdl%iden(jjEvnt) = sCore%mdl%amod(iEvnt)
                 ! Evnt to remove in %amod
                 jjEvnt = iEvnt
                 ! Verbose mode (normal)
                 IF( opt%outFileVerbose == 1 )THEN
                    label = 'IdenCand'
                    CALL sCoreWriteElemLine( sCore%mdl%amod(iEvnt),&
                         label, sCore%lsa%m0 )
                 END IF
              ELSE
                 WRITE(lfnerr,'(/,A,/)') ' *** SR FODIPTST: Element not found.'
                 CALL exitrc(2)
              END IF
              EXIT
           END IF
        END DO
        IF( jjEvnt > 0 )THEN
           CALL sCoreModDelEvnt( sCore%mdl%amod, sCore%mdl%nAmod, jjEvnt)
        ELSE
           EXIT
        END IF
     END DO

  END IF

  ! Restore all candidates
  DO iEvnt = 1,sCore%mdl%nAmod
     sCore%mdl%amod(iEvnt)%cand = candNon
  END DO

! End of subroutine
! -----------------
!  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodiptst

END MODULE s_FODIPTST

