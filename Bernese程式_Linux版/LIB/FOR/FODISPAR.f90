MODULE s_FODISPAR
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodispar(opt,sCore,iSta)

! -------------------------------------------------------------------------
! Purpose:    Store the ATI results into sCore for the station iSta.
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
!             19-Sep-2012 RD: Use P_FODITS with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used Modules
! ------------
! structures, variables:
  USE m_bern,    ONLY: i4b, r8b, lfnPrt, shortLineLength
  USE p_fodits,  ONLY: t_opt, t_sCore, t_lsa, &
                       typeoutl, &
                       scorechkpresevntinmod, scoremodaddevnt, scoremodsortelem

! operator, methods:
!  USE d_debug,   ONLY: debug_entry, debug_exit

! subroutines, functions:
  USE s_jmt
  USE s_alcerr

! no implicit
  IMPLICIT NONE

! subroutine name
  CHARACTER(LEN=8), PARAMETER    :: srName = 'fodispar'


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
  TYPE(t_lsa)                    :: lse

  CHARACTER(LEN=7)               :: outModeTxt

  INTEGER(i4b)                   :: iac
  INTEGER(i4b)                   :: nVal
  INTEGER(i4b)                   :: nVal2
  INTEGER(i4b)                   :: iMjd
  INTEGER(i4b)                   :: jMjd
  INTEGER(i4b)                   :: iVal
  INTEGER(i4b)                   :: indM
  INTEGER(i4b)                   :: jndM
  INTEGER(i4b)                   :: iEvnt
  INTEGER(i4b)                   :: jjEvnt
  INTEGER(i4b)                   :: yyyy
  INTEGER(i4b)                   :: mm

  REAL(r8b)                      :: dd
  REAL(r8b)                      :: yyyymmdd
  REAL(r8b)                      :: m0

! Call debug routine
! ------------------
!  CALL debug_entry(srName)


! Initialization of all variables
! -------------------------------
  nVal = sCore%nVal
  yyyy = 0
  mm = 0
  dd = 0.0D0
  yyyymmdd = 0.0D0

  ! Store the number of iteration steps
  sCore%sta(iSta)%nIterSteps = sCore%ctr%nIterLoop

  ! Store the model RMS after the ATI procedure
  sCore%sta(ista)%modRms = sCore%lsa%m0

  ! Store the %amod model into %sta(iSta)%mod%evnt
  DO iEvnt = 1,sCore%mdl%nAmod
     CALL sCoreChkPresEvntInMod( sCore%mdl%amod(iEvnt), &
          sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt, jjEvnt )
     IF( jjEvnt > 0 )THEN
        sCore%sta(iSta)%mod%evnt(jjEvnt) = sCore%mdl%amod(iEvnt)
     ELSE
        CALL sCoreModAddEvnt( sCore%mdl%amod(iEvnt), &
             sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt )
     END IF
  END DO

  ! Sort elements
  CALL sCoreModSortElem( nVal, &
                         sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt )

  ! Reconstruct proper sCore%lsa%mod (functional model) and
  ! sCore%lsa%v (residuals) with missing epochs of OUTL
  NULLIFY(lse%mod)
  NULLIFY(lse%v)
  ! Allocation memory
  ALLOCATE(lse%mod(sCore%lsa%nnMjd),stat=iac)
  CALL alcerr(iac,'lse%mod',(/sCore%lsa%nnMjd/),srName)
  ALLOCATE(lse%v(sCore%lsa%nnMjd),stat=iac)
  CALL alcerr(iac,'lse%v', (/sCore%lsa%nnMjd/),srName)
  ! Copy lse%mod = sCore%lsa%mod and lse%v = sCore%lsa%v
  DO iMjd = 1,sCore%lsa%nnMjd
     lse%mod(iMjd) = sCore%lsa%mod(iMjd)
     lse%v(iMjd) = sCore%lsa%v(iMjd)
  END DO
  ! Deallocate sCore%lsa%mod and sCore%lsa%v
  DEALLOCATE(sCore%lsa%mod,stat=iac)
  DEALLOCATE(sCore%lsa%v,stat=iac)
  ! Change sized of nnMjd
  sCore%lsa%nnMjd = sCore%sta(iSta)%ts%nMjd * nVal
  ! Allocation memory
  ALLOCATE(sCore%lsa%mod(sCore%lsa%nnMjd),stat=iac)
  CALL alcerr(iac,'sCore%lsa%mod',(/sCore%lsa%nnMjd/),srName)
  ALLOCATE(sCore%lsa%v(sCore%lsa%nnMjd),stat=iac)
  CALL alcerr(iac,'sCore%lsa%v', (/sCore%lsa%nnMjd/),srName)
  ! Copy back %mod and %v by inserting outliers OUTL
  jMjd = 0
  DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
     ! Index
     indM = nVal*(iMjd-1)
     ! No outlier
     IF( sCore%mdl%outlMjd(iMjd) == 0.0D0 )THEN
        jMjd = jMjd + 1
        jndM = nVal*(jMjd-1)
        sCore%lsa%mod(indM+1:indM+nVal) = lse%mod(jndM+1:jndM+nVal)
        sCore%lsa%v(indM+1:indM+nVal) = lse%v(jndM+1:jndM+nVal)
     ! Outlier
     ELSE
        DO iEvnt = 1,sCore%mdl%nAmod
           IF( sCore%mdl%amod(iEvnt)%mjd == &
               sCore%sta(iSta)%ts%mjd(iMjd) .AND. &
               sCore%mdl%amod(iEvnt)%type == typeOutl )THEN
              sCore%lsa%mod(indM+1:indM+nVal) = &
                   sCore%mdl%amod(iEvnt)%mod(1:nVal) + &
                   sCore%mdl%amod(iEvnt)%par(1:nVal)
              sCore%lsa%v(indM+1:indM+nVal) = 0.0D0
              EXIT
           END IF
        END DO
     END IF
  END DO
  ! Deallocate lse%mod and lse%v
  DEALLOCATE(lse%mod,stat=iac)
  DEALLOCATE(lse%v,stat=iac)

  ! Verbosity mode: write Observations and Mode
  IF( opt%outFileVerbose == 1 )THEN
     nVal2 = 3
     m0 = opt%inPltFileViM0
     WRITE(lfnprt,'(A)') &
          ' --------------------------'
     DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
        indM = nVal*(iMjd-1)
        CALL jmt(sCore%sta(iSta)%ts%mjd(iMjd),yyyy,mm ,dd )
        yyyymmdd = yyyy + (mm-1)/12.0D0 + (dd-1)/365.25D0
        outModeTxt = 'OBSMOD-'
        IF( opt%inPltFileVciEna >= 1 )THEN
           outModeTxt = 'OBSMODS'
        END IF
        WRITE(lfnprt,'(A,1X,A16,1X,F9.4,1X,F11.5)',ADVANCE='NO') &
             outModeTxt, &
             sCore%sta(iSta)%name, &
             yyyymmdd, &
             sCore%sta(iSta)%ts%mjd(iMjd)
        IF( opt%inPltFileVciEna == 2 .AND. nVal == 3 )THEN
           DO iVal = 1,nVal
              WRITE(lfnprt,'(I3,3(1X,E18.11))',ADVANCE='NO') &
                   iVal, &
                   sCore%sta(iSta)%ts%val(iMjd,iVal), &
                   sCore%lsa%mod(indM+iVal), &
                   m0*SQRT(sCore%sta(iSta)%ts%vci(iMjd,iVal,iVal))
           END DO
           DO iVal = nVal+1,nVal2
              WRITE(lfnprt,'(I3,3(1X,E18.11))',ADVANCE='NO') &
                   iVal, &
                   0.0D0, &
                   0.0D0, &
                   1.0D0
           END DO
        ELSE IF( opt%inPltFileVciEna == 1 )THEN
           DO iVal = 1,nVal
              WRITE(lfnprt,'(I3,3(1X,E18.11))',ADVANCE='NO') &
                   iVal, &
                   sCore%sta(iSta)%ts%val(iMjd,iVal), &
                   sCore%lsa%mod(indM+iVal), &
                   sCore%sta(iSta)%ts%dVal(iMjd,iVal)
           END DO
           DO iVal = nVal+1,nVal2
              WRITE(lfnprt,'(I3,3(1X,E18.11))',ADVANCE='NO') &
                   iVal, &
                   0.0D0, &
                   0.0D0, &
                   1.0D0
           END DO
        ELSE
           DO iVal = 1,nVal
              WRITE(lfnprt,'(I3,2(1X,E18.11))',ADVANCE='NO') &
                   iVal, &
                   sCore%sta(iSta)%ts%val(iMjd,iVal), &
                   sCore%lsa%mod(indM+iVal)
           END DO
           DO iVal = nVal+1,nVal2
              WRITE(lfnprt,'(I3,2(1X,E18.11))',ADVANCE='NO') &
                   iVal, &
                   0.0D0, &
                   0.0D0
           END DO
        END IF
        WRITE(lfnprt,*)
     END DO
  END IF

  ! Store the (new) residuals into %val(iMjd,iVal).
  ! Do not change %vci (variance-covariance info)
  IF( sCore%lsa%dof >= 1 )THEN
     DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
        indM = nVal*(iMjd-1)
        ! Val = v (residuals)
        sCore%sta(iSta)%ts%val(iMjd,:) = sCore%lsa%v(indM+1:indM+nVal)
     END DO
  END IF

  ! Deallocate %outlMjd
  DEALLOCATE(sCore%mdl%outlMjd,stat=iac)

! End of subroutine
! -----------------
!  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodispar

END MODULE s_FODISPAR
