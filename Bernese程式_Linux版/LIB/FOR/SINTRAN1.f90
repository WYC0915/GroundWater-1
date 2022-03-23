MODULE s_SINTRAN1
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE sintran1(neq)

! -------------------------------------------------------------------------
! Purpose:    The subroutine performs the parameter transformation from
!             the ADDNEQ2 convention (offset + offset) into the SINEX
!             convention (offset + drift)
!
! Author:     L. Mervart
!
! Created:    17-Aug-1998
!
! Changes:    04-May-2000 HU: Call of 'syminv' corrected
!             05-May-2000 SS: Do regularization of 'regMat' with 1.d-7
!             26-Jun-2001 RD: Use alcerr for allocation
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             29-Dec-2001 HU: Interfaces to adweight,neqsolve,neqprint added
!             25-Sep-2002 HU: Use interface to sinstore
!             27-Jan-2003 CU: New parameter parlst for NEQSOLVE
!             04-Feb-2003 SS: Problematic call of neqprint deactivated
!             27-May-2003 CU: Remove parlst
!             16-Jun-2003 RS: EOP loop: include nutation parameters
!             12-Aug-2003 RS: Omit transformation in 'offset only'-case
!             11-Dec-2003 MM: New option: snxReg
!             08-Apr-2004 HU: Do not invert a priori matrix if zero
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             12-Dec-2005 CU: Add 3rd parameter for CALL of SR neqsolve,
!                             save values of npseu, npseuel
!             27-Feb-2006 CU: New dummy2 variable for CALL of SR neqsolve
!             13-Oct-2006 AG: Store SINEX with NEQ
!             01-May-2007 AG: Store SINEX with piecewise linear ERP
!             22-May-2007 AG: Store SOLUTION ESTIMATE into SINEX with NEQ
!             09-Jul-2008 RD: Use readRec from D_NEQ
!             02-Mar-2009 SL: Store free aNor and bNor for sinstore call
!             09-Aug-2010 RD: Use "syminvg" instead of "syminv"
!             27-Oct-2010 SL: Use m_bern with ONLY, handling [ab]Nor_free
!             16-Dec-2010 DT: Call of NEQSOLVE changed (dummyHlm)
!             08-Feb-2012 RD: Solve "out-of-bounds" problem for anor_free
!             22-Apr-2012 RD: Do not allocate the big arrays on the fly
!             18-Jul-2012 RD: Transform ERP also in case of NEQ-SINEX
!             22-Oct-2012 DT: Really transform ERPs in case of NEQ-SINEX
!                             (new call SINTRAN2; for aNor_free, bNor_free)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnLoc, lfnErr
  USE d_neq,    ONLY: t_neq,readRec
  USE p_addneq, ONLY: opt, t_hlmFil

  USE f_ikf
  USE s_neqsolve
  USE s_opnfil
  USE s_alcerr
  USE s_sinstore
  USE s_opnerr
  USE s_sintran2
  USE s_syminvg
  USE s_exitrc
  USE s_adweight
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)              :: neq

! Local Variables
! ---------------
  TYPE(t_hlmFil)                          :: dummyHlm

  INTEGER(i4b)                            :: ip
  INTEGER(i4b)                            :: ii
  INTEGER(i4b)                            :: polCrd
  INTEGER(i4b)                            :: numPol
  INTEGER(i4b)                            :: iPol
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE :: indPol
  INTEGER(i4b)                            :: nParNew
  INTEGER(i4b)                            :: ipNew
  INTEGER(i4b)                            :: ipar1
  INTEGER(i4b)                            :: ipar2
  INTEGER(i4b)                            :: ip2old
  INTEGER(i4b)                            :: nsing, nozero
  INTEGER(i4b)                            :: ios, iac
  INTEGER(i4b)                            :: npseuSav, npseuelSav

  CHARACTER(LEN=8)                        :: sbrName = 'SINTRAN1'

  REAL(r8b),    DIMENSION(:), ALLOCATABLE :: regMat
  REAL(r8b),    DIMENSION(:), ALLOCATABLE :: aNor_free
  REAL(r8b),    DIMENSION(:), ALLOCATABLE :: bNor_free
  REAL(r8b),    DIMENSION(1)              :: dummy
  REAL(r8b),    DIMENSION(9)              :: dummy2
  REAL(r8b)                               :: weight

  LOGICAL                                 :: prtmsg = .FALSE.

  IF ( opt%sinexrs == '') RETURN

  ALLOCATE(regMat(IKF(opt%maxpar,opt%maxpar)),stat=iac)
  CALL alcerr(iac,'regMat',(/IKF(opt%maxpar,opt%maxpar)/),sbrName)

  ALLOCATE(aNor_free(IKF(neq%misc%npar,neq%misc%npar)),stat=iac)
  CALL alcerr(iac,'aNor_free',(/IKF(neq%misc%npar,neq%misc%npar)/),sbrName)

  ALLOCATE(bNor_free(neq%misc%npar),stat=iac)
  CALL alcerr(iac,'bNor_free',(/neq%misc%npar/),sbrName)

  ! Restore the NEQ system before inversion
  ! ---------------------------------------
  IF ( opt%noinv ) THEN
    regMat(1:neq%misc%npar*(neq%misc%npar+1)/2) = 1.d0
  ELSE
    neq%aNor(1:neq%misc%npar*(neq%misc%npar+1)/2) = 0.d0

    npseuSav   = neq%misc%npseu
    npseuelSav = neq%misc%npseuel

    CALL adweight(neq,0,0)

    neq%misc%npseu   = npseuSav
    neq%misc%npseuel = npseuelSav

    regMat(1:neq%misc%npar*(neq%misc%npar+1)/2) = &
                        neq%aNor(1:neq%misc%npar*(neq%misc%npar+1)/2)
  END IF

  CALL opnfil(lfnloc,opt%sinexrs,'OLD','UNFORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,opt%sinexrs,'SINTRAN1')
  CALL readRec(lfnloc,neq%misc%npar*(neq%misc%npar+1)/2,neq%aNor)
  CLOSE(lfnloc,status='DELETE')

! Transform the station coordinates into the coordinates + velocities
! -------------------------------------------------------------------
  LOOP_ipar2: DO ipar2 = 1, neq%misc%npar
    IF ( neq%par(ipar2)%locq(1) == 1  .AND. &
         neq%par(ipar2)%locq(4) == 2 ) THEN

      DO ipar1 = 1, neq%misc%npar
        IF ( neq%par(ipar1)%locq(1) == 1                        .AND. &
             neq%par(ipar1)%locq(4) == 1                        .AND. &
             neq%par(ipar1)%locq(3) == neq%par(ipar2)%locq(3)   .AND. &
             neq%par(ipar1)%name    == neq%par(ipar2)%name    ) THEN

          CALL sintran2(neq, regMat(:)  , dummy(:)   , ipar1, ipar2, 0, 0)
          IF ( opt%sincont == 1 )  &
            CALL sintran2(neq, aNor_free(:), bNor_free(:), ipar1, ipar2, 1, 0)
          CALL sintran2(neq, neq%aNor(:), neq%bNor(:), ipar1, ipar2, 1, 1)

          neq%par(ipar2)%locq(4) = 3

          CYCLE LOOP_ipar2
        END IF
      END DO

    END IF
  END DO LOOP_ipar2

! Get aNor and bNor for SINEX with NEQ
! ------------------------------------
  aNor_free = neq%aNor(1:neq%misc%npar*(neq%misc%npar+1)/2)
  bNor_free = neq%bNor(1:neq%misc%npar)

! Find Earth Orientation Parameters which have to be transformed
! --------------------------------------------------------------
  weight = ( opt%sigma0 / 0.0001d0  ) ** 2

  IF (opt%erprep == 1) THEN
    LOOP_polCrd: DO polCrd = 1, 5

    ! How many parameters are present ?
    ! ---------------------------------
      numPol = 0
      DO ip = 1, neq%misc%npar
        IF ( neq%par(ip)%locq(1) == 10       .AND. &
             neq%par(ip)%locq(4) == polCrd   .AND. &
             (.NOT.(neq%par(ip)%locq(6) == 1 .AND. &
               neq%par(ip)%time%half > 0d0))) THEN
          IF ( neq%par(ip)%locq(5) == 2 ) CYCLE LOOP_polCrd
          numPol = numPol + 1
        END IF
      END DO

      IF ( numPol < 2 ) CYCLE LOOP_polCrd
      IF ( opt%sincont == 1 .AND. numPol /= 2 ) THEN
        IF (.NOT. prtMsg) THEN
          WRITE(lfnerr,'(/,A,3(/,18X,A),/)')                            &
          ' ### SR SINTRAN1: When storing SINEX files containing NEQs', &
          'the Earth rotation parameters can only be transformed',      &
          'if exact two parameters are in the normal equation.'
          prtMsg = .TRUE.
        ENDIF
        CYCLE LOOP_polCrd
      ENDIF

    ! Check the dimension
    ! -------------------
      IF ( neq%misc%npar + numPol - 2 > opt%maxpar ) THEN
        WRITE(lfnerr,*) ' *** SR Sintran1: dimension too small'
        CALL exitrc(2)
      END IF

    ! Initialize aNor, bNor, regMat
    ! -----------------------------
      DO ipNew = neq%misc%npar + 1, neq%misc%npar + numPol - 2
        neq%bNor(ipNew) = 0.d0
        DO ip = 1, neq%misc%npar + numPol - 2
          neq%aNor(ikf(ipNew,ip)) = 0.d0
          regMat(ikf(ipNew,ip))   = 0.d0
        END DO
      END DO

      ALLOCATE(indPol(2*numPol-2), stat=iac)
      CALL alcerr(iac, 'indPol', (/2*numPol-2/), 'sintran1')

    ! Create index of parameters, expand the NEQ system
    ! -------------------------------------------------
      iPol    = 0
      nParNew = 0
      DO ip = 1, neq%misc%npar
        IF ( neq%par(ip)%locq(1) == 10       .AND. &
             neq%par(ip)%locq(4) == polCrd   .AND. &
             neq%par(ip)%locq(5) == 1      ) THEN

          iPol         = iPol + 1
          indPol(iPol) = ip

          IF ( iPol > 1 .AND. iPol < 2*numPol-2) THEN
            iPol                  = iPol + 1
            nParNew               = nParNew + 1
            indPol(iPol)          = neq%misc%npar + nParNew
            neq%par(indPol(iPol)) = neq%par(ip)
          END IF

        END IF
      END DO

      neq%misc%npar = neq%misc%npar + nParNew

    ! Transformation
    ! --------------
      ip2old = 0

      DO iPol = 1, 2*numPol-2, 2
        ipar1 = indPol(iPol)
        ipar2 = indPol(iPol+1)
        IF (ip2old /= 0) THEN
          regMat(ikf(ipar1,ipar1))   = regMat(ikf(ipar1,ipar1))   + weight
          regMat(ikf(ipar1,ip2old))  = regMat(ikf(ipar1,ip2old))  - weight
          regMat(ikf(ip2old,ip2old)) = regMat(ikf(ip2old,ip2old)) + weight
        END IF
        ip2old = ipar2
      END DO

      DO iPol = 1, 2*numPol-2, 2
        ipar1 = indPol(iPol)
        ipar2 = indPol(iPol+1)

        CALL sintran2(neq, regMat(:)  , dummy(:)   , ipar1, ipar2, 0, 0)

        IF ( opt%sincont == 1 )  &
          CALL sintran2(neq, aNor_free(:), bNor_free(:), ipar1, ipar2, 1, 0)

        CALL sintran2(neq, neq%aNor(:), neq%bNor(:), ipar1, ipar2, 1, 1)

        neq%par(ipar2)%locq(5) = 2
      END DO

      DEALLOCATE(indPol, stat=iac)

    END DO LOOP_polCrd
  ENDIF

! Compute the solution and store the SINEX file
! ---------------------------------------------
  IF (.NOT. opt%noinv ) THEN
    DO ii = 1, neq%misc%npar
      regMat(ikf(ii,ii)) = regMat(ikf(ii,ii)) + opt%snxReg
    END DO

    nozero=0
    DO ii = 1, neq%misc%npar * (neq%misc%npar + 1) / 2
      IF (regMat(ii) /= 0D0) nozero=1
      neq%aNor(ii) = neq%aNor(ii) + regMat(ii)
    END DO

    DEALLOCATE(neq%xxx, stat=iac)
    CALL neqsolve(neq,0,0,dummy2,dummyHlm)

! Do not invert zero matrix
    IF (nozero==1) THEN
      CALL syminvg(neq%misc%npar,regMat(:),0,nsing,sbrName=sbrName)
    ENDIF

  ENDIF

!!  CALL neqprint(neq)                          ! Print the results
  CALL sinstore(neq, regMat(:), aNor_free, bNor_free)

  DEALLOCATE(regMat,stat=iac)
  DEALLOCATE(aNor_free,stat=iac)
  DEALLOCATE(bNor_free,stat=iac)

END SUBROUTINE sintran1

END MODULE
