MODULE s_FODIIDIA
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodiidia(opt,sCore,iSta,stateM)

! -------------------------------------------------------------------------
! Purpose:    Initialization of ATI-procedure for the station iSta.
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
  USE m_bern,    ONLY: i4b, r8b, shortLineLength, lfnprt
  USE p_fodits,  ONLY: t_opt, t_sCore, &
                       typejump, infoinit, flagtst, typerate, &
                       flagset, significant, &
                       scoremodaddevnt, scoremodsortelem, scoreinitnewevnt

! operator, methods:
!  USE d_debug,   ONLY: debug_entry, debug_exit

! subroutines, functions:
  USE s_alcerr

! no implicit
  IMPLICIT NONE

! subroutine name
  CHARACTER(LEN=8), PARAMETER    :: srName = 'fodiidia'


! List of Arguments
! -----------------
! input:

! input/output:
  TYPE(t_opt)                    :: opt           ! Option structure
  TYPE(t_sCore)                  :: sCore         ! Core structure of FODITS
  INTEGER(i4b),INTENT(IN)        :: iSta
  INTEGER(i4b)                   :: stateM

! output:


! Local Types
! -----------


! Local Parameters
! ----------------


! Local Variables
! ---------------
  INTEGER(i4b)                   :: iac
  INTEGER(i4b)                   :: iEvnt
  INTEGER(i4b)                   :: jSta
  INTEGER(i4b)                   :: nVal
  INTEGER(i4b)                   :: nMjd


! Call debug routine
! ------------------
!  CALL debug_entry(srName)


! Initialization of all variables
! -------------------------------
  stateM = 0
  nVal = sCore%nVal
  nMjd = sCore%sta(iSta)%ts%nMjd
  sCore%lsa%nnMjd = nMjd * nVal
  sCore%sta(ista)%nIterSteps = 0
  sCore%sta(ista)%modRms = 0.0D0
  sCore%sta(iSta)%singular = 0
  sCore%ctr%modNumIter(:) = 0.0D0
  sCore%ctr%modNumIterOutl(:) = 0.0D0

  ! Reset models
  sCore%mdl%nApri = 0
  DEALLOCATE(sCore%mdl%apri,stat=iac)
  sCore%mdl%nAmod = 0
  DEALLOCATE(sCore%mdl%amod,stat=iac)
  sCore%mdl%nIden = 0
  DEALLOCATE(sCore%mdl%iden,stat=iac)

  ! Reset other variables
  sCore%sta(iSta)%upd%nRen = 0

  ! Manully selection of stations
  IF( opt%nStaSel /= 0 )THEN
     stateM = 1
     DO jSta = 1,opt%nStaSel
        IF( opt%inStaSelection(jSta) == sCore%sta(iSta)%name )THEN
           stateM = 0
           EXIT
        END IF
     END DO
  END IF
  IF( stateM == 1 )THEN
  !  CALL debug_exit(srName)
     RETURN
  END IF

  ! Check for minimal number of observations for (the station) iSta
  IF( nMjd < opt%inMinNrObs .AND. stateM == 0 )THEN
     stateM = 1
     WRITE(lfnprt,'(2(A,1X,I07,/))') &
                  ' Minimal number of observations :', opt%inMinNrObs, &
                  ' Number of observations         :', nMjd
     !  CALL debug_exit(srName)
     RETURN
  END IF

  ! Insert all known (apriori) predefined elements into %apri
  DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
     ! Add element
     CALL sCoreModAddEvnt( sCore%sta(iSta)%mod%evnt(iEvnt), &
                           sCore%mdl%apri, sCore%mdl%nApri )
  END DO

  ! First permanent offset into %amod
  CALL sCoreInitNewEvnt( nVal, sCore%mdl%evnt)
  sCore%mdl%evnt%type = typeJump
  sCore%mdl%evnt%info = infoInit
  sCore%mdl%evnt%mjd = sCore%sta(iSta)%ts%mjd(1)
  sCore%mdl%evnt%flag = flagTst
  sCore%mdl%evnt%remark = 'OFFST-TS'
  CALL sCoreModAddEvnt( sCore%mdl%evnt, sCore%mdl%amod, sCore%mdl%nAmod )

  ! First permanent drift into %amod
  CALL sCoreInitNewEvnt( nVal, sCore%mdl%evnt)
  sCore%mdl%evnt%type = typeRate
  sCore%mdl%evnt%info = infoInit
  ! sCore%outTimRefCrd
  sCore%mdl%evnt%mjd = sCore%sta(iSta)%ts%mjd(1)
  sCore%mdl%evnt%flag = flagTst
  sCore%mdl%evnt%remark = 'DRIFT-TS'
  CALL sCoreModAddEvnt( sCore%mdl%evnt, sCore%mdl%amod, sCore%mdl%nAmod )

  ! Insert all permanent known (apriori) elements of %apri into %amod
  DO iEvnt = 1,sCore%mdl%nApri
     ! Filter
     IF( sCore%mdl%apri(iEvnt)%flag /= flagSet )CYCLE
     ! Always significant
     sCore%mdl%apri(iEvnt)%siTst = significant
     ! Add element
     CALL sCoreModAddEvnt( sCore%mdl%apri(iEvnt), &
                           sCore%mdl%amod, sCore%mdl%nAmod )
  END DO

  ! Sort %apri
  CALL sCoreModSortElem( nVal, sCore%mdl%apri, sCore%mdl%nApri )

  ! Check for minimal number of observations for (the station) iSta
  IF( nMjd < opt%inMinNrObs .AND. stateM == 0 )THEN
     stateM = 1
     WRITE(lfnprt,'(2(A,1X,I07,/))') &
          ' Minimal number of observations :', opt%inMinNrObs, &
          ' Number of observations         :', nMjd
  END IF

  ! Set previous m0
  sCore%lsb%prevVtv = HUGE(0.0D0)

  ! Allocate %outlMjd
  ALLOCATE(sCore%mdl%outlMjd(nMjd),stat=iac)
  CALL alcerr(iac,'sCore%mdl%outlMjd',(/nMjd/),srName)

  ! Print station name
  IF( opt%outFileVerbose == 1 )THEN
     WRITE(lfnprt,'(/,/,A,1X,A16,/,A)') &
          ' Station:', sCore%sta(iSta)%name, &
          ' --------------------------'
  END IF

  ! End of subroutine
  ! -----------------
  !  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodiidia

END MODULE s_FODIIDIA
