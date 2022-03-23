MODULE s_DIFFSP_SCREEN
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE diffsp_screen(nsat,aobs,bobs,obsflg,posApr,nPar,opt,&
     cPos,nseff_sol,pPosD,rmsest,iret)

!--------------------------------------------------------------------------
! Purpose:   compute position diferences using phase difference
!            observations at epochs epo12(i),i=1,2. Screen, if
!            resulting rms-error is too big or if difference w.r.t.
!            a priori positions is too big
!
! Author:    G.Beutler
!
! Created:   25-Dec-2000
!
! Changes:   07-Jan-2002 HB: Dynamic allocation of memory
!            13-Nov-2002 HB: Reviewed iterative screening algorithm
!            02-Sep-2005 HB: Save Variance-covariance matrix
!            10-Jul-2012 RD: Use syminvg instead of symin8
!            10-Jul-2012 RD: Use m_bern with ONLY, remove unused variables
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
!-------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b
  USE p_leokin, ONLY: t_leoopt, t_leopos, t_leodif
  USE f_ikf
  USE s_alcerr
  USE s_syminvg
  IMPLICIT NONE

! List of Parameters
! ------------------
! IN:
  TYPE(t_leoopt) :: opt
  TYPE(t_leopos) :: cPos
  TYPE(t_leodif) :: pPosD

  INTEGER(i4b)                       :: nsat      ! number of satellites
  INTEGER(i4b)                       :: nPar      ! number of parameters
  INTEGER(i4b)                       :: nplus_sol !
  REAL(r8b),DIMENSION(*)             :: bobs      ! observed-computed
  REAL(r8b),DIMENSION(4,*)           :: aobs      ! first design matrix
  REAL(r8b),DIMENSION(4,2)           :: posApr     ! positions and rec. clock corr.
                                                  ! corresponding to the two epochs
  REAL(r8b),DIMENSION(4)             :: rmsest    !

! IN/OUT:
  CHARACTER(LEN=1),DIMENSION(*)      :: obsflg    ! Marks for observations

! OUT:
  INTEGER(i4b) :: nseff,nseff_sol  ! Effective Number of Satellites
  INTEGER(i4b) :: iret             ! Return code
                                   ! =0, OK
                                   ! =1, OK, but only four satellites
                                   ! =2, dif-est failed (fewer than four GPS sats)

! Local Variables
! ---------------
  INTEGER(i4b)                  :: iSat,kSat,ik,ising,iter,nplus
  INTEGER(i4b)                  :: iPar
  INTEGER(i4b)                  :: kPar
  INTEGER(i4b)                  :: iac

  REAL(r8b),DIMENSION(:),ALLOCATABLE  :: qvv
  REAL(r8b)                     :: resmax,resmax_tot,rmsmin,test,test_min,test2
  REAL(r8b),DIMENSION(4)        :: bngl,sol,solmin
  REAL(r8b),DIMENSION(10)       :: angl
  REAL(r8b),DIMENSION(10)       :: angl_sav
  REAL(r8b),DIMENSION(10)       :: angl_min
  REAL(r8b)                     :: rmsmax_pha
  REAL(r8b)                     :: pdfmax

  kSat = 0

! Allocate memory
! ---------------
  ALLOCATE(qvv(nSat),stat=iac)
  CALL alcErr(iac, 'qvv', (/nSat/), 'diffsp_screen')

! Screen in niter steps
! ---------------------
  resmax_tot=0.D0
  rmsmin = 1.d30
  test_min=1.d30

  IF (cPos%iter==1) THEN
!    rmsmax_pha = 4*opt%rmsmax_pha
!    pdfmax     = 4*opt%pdfmax
    rmsmax_pha = opt%rmsmax_pha
    pdfmax     = opt%pdfmax
!!  ELSEIF (cPos%iter==2.OR.cPos%iter==3) THEN
!!    rmsmax_pha = 2*opt%rmsmax_pha
!!    pdfmax     = 2*opt%pdfmax
  ELSEIF (cPos%iter>1) THEN
    rmsmax_pha = opt%rmsmax_pha
    pdfmax     = opt%pdfmax
  ENDIF

  screenLoop: DO iter=1,nsat+1
! skip step, if mark is not appropriate
    IF(iter > 2)THEN
      IF(obsflg(iter-2) == 'M')obsflg(iter-2) = ' '
    ENDIF
    IF(iter > 1)THEN
     IF(obsflg(iter-1) == ' ')THEN
!!!      IF(obsflg(iter-1) == 'o'.OR.obsFlg(iter-1)== ' ')THEN
        obsflg(iter-1)='M'
      ELSE
        CYCLE screenLoop
      ENDIF
    ENDIF
!
! Initialize Normal Equation System
    angl=0.d0
    bngl=0.d0
    nseff=0
    nplus=0
!
! set up observation equations
! ----------------------------
    DO iSat=1,nsat
!
! Update NEQ-System
! -----------------
      IF(obsflg(iSat) == ' '.OR.obsflg(iSat) == '+')THEN
!TESTTT      IF(obsflg(iSat) == ' '.OR.obsflg(iSat) == '+'.OR.obsFlg(iSat)=='o')THEN
        nseff=nseff+1
        IF(obsflg(iSat) == '+')nplus=nplus+1
        DO iPar=1,nPar
          bngl(iPar)=bngl(iPar)+aobs(iPar,iSat)*bobs(iSat)*pPosD%wgt_phaobs(iSat)
          DO kPar=1,iPar
            ik=ikf(iPar,kPar)
            angl(ik)=angl(ik)+aobs(iPar,iSat)*aobs(kPar,iSat)*pPosD%wgt_phaobs(iSat)
            IF (opt%correl == 2) angl_sav(ik)=angl(ik)
          ENDDO
        ENDDO
!        pPosD%aobs(:,pPosD%svndif(iSat)) = aobs(:,iSat)
      ENDIF
    ENDDO

! Solve NEQ-System
! ----------------
    IF(iter == 1)nseff_sol=nseff
    IF(nseff-nPar < 0)THEN
      IRET=2
      GOTO 999
    ENDIF

    CALL syminvg(nPar,angl(:),0,ising)

    DO iPar=1,nPar
      sol(iPar)=0.d0
      DO kPar=1,nPar
        ik=IKF(iPar,kPar)
        sol(iPar)=sol(iPar)+angl(ik)*bngl(kPar)
      ENDDO
    ENDDO

! Solution relative to a priori orbit
! -----------------------------------
    test=dsqrt(sol(1)**2+sol(2)**2+sol(3)**2)

! Residuals, find max. residual
! -----------------------------
    resmax=0.d0
    pPosD%rmsdif=0.d0
    DO iSat=1,nsat
      pPosD%resdif(iSat)= bobs(iSat)
      DO kPar=1,nPar
        pPosD%resdif(iSat)=pPosD%resdif(iSat)-aobs(kPar,iSat)*sol(kPar)
      ENDDO
      IF(obsflg(iSat) == ' '.OR.obsflg(iSat) == '+') THEN
!TESTTT      IF(obsflg(iSat) == ' '.or.obsflg(iSat) == '+'.OR.obsFlg(iSat)== 'o') &
           pPosD%rmsdif=pPosD%rmsdif+pPosD%resdif(iSat)**2*pPosD%wgt_phaobs(iSat)
      ENDIF
      qvv(iSat)=1/pPosD%wgt_phaobs(iSat)
      DO iPar=1,nPar
        DO kPar=1,nPar
          ik=IKF(iPar,kPar)
          qvv(iSat)=qvv(iSat)-aobs(iPar,iSat)*angl(ik)*aobs(kPar,iSat)
        ENDDO
      ENDDO
!      write(*,*)cPos%iepo,iSat,qvv(iSat),obsflg(iSat)
      pPosD%mrk(iSat) = '-'
      IF (qvv(iSat) > 0) THEN
!TESTTT        IF((obsflg(iSat) == ' '.OR.obsflg(iSat) == '+'.OR.obsFlg(iSat)== 'o').AND.&
        IF((obsflg(iSat) == ' '.OR.obsflg(iSat) == '+').AND.&
             DABS(pPosD%resdif(iSat)/DSQRT(qvv(iSat)))>resmax)THEN
          resmax=dabs(pPosD%resdif(iSat))
        ENDIF
        IF (opt%elev_wgt == 1) THEN
          pPosD%resDif(iSat)=pPosD%resDif(isat)*opt%rms_phaObs/DSQRT(qvv(iSat))
        ENDIF
        pPosD%mrk(iSat) = obsFlg(iSat)
      ENDIF
    ENDDO

! RMS-Error
! ---------
    IF(nseff>nPar)THEN
      pPosD%rmsdif=dsqrt(pPosD%rmsdif/(nseff-nPar))
      pPosD%rmsdif=opt%rms_phaobs*pPosD%rmsdif
    ELSE
      pPosD%rmsdif=0.1D0
    ENDIF
!      write(*,*)'iter,nseff,rms=',iter,nseff,pPosD%rmsdif
! update best solution
    IF(nseff - nPar == 0)THEN
      IF(test < test_min)THEN
        nseff_sol=nseff
        nplus_sol=nplus
        test_min=test
        kSat=iter-1
        rmsmin=pPosD%rmsdif
        angl_min(:) = angl_sav(:)
        resmax_tot=resmax
        DO kPar=1,nPar
          solmin(kPar)=sol(kPar)
          iPar=ikf(kPar,kPar)
          rmsest(kPar)=pPosD%rmsdif*DSQRT(angl(iPar))
        ENDDO
        pPosD%Qxy(:) = angl(:)*pPosD%rmsdif**2/opt%rms_phaObs**2
      ENDIF
    ELSE
      IF (pPosD%rmsdif < rmsmin) THEN
        nseff_sol=nseff
        nplus_sol=nplus
        test_min=test
        rmsmin=pPosD%rmsdif
        angl_min(:) = angl_sav(:)
        resmax_tot=resmax
        kSat=iter-1
        DO kPar=1,nPar
          solmin(kPar)=sol(kPar)
          iPar=ikf(kPar,kPar)
          rmsest(kPar)=pPosD%rmsdif*DSQRT(angl(iPar))
        ENDDO
        pPosD%Qxy(:) = angl(:)*pPosD%rmsdif**2/opt%rms_phaObs**2
      ELSEIF (pPosD%rmsdif >= rmsmin .AND.&
           &pPosD%rmsdif < rmsmax_pha .AND. test < test_min) THEN
        nseff_sol=nseff
        nplus_sol=nplus
        test_min=test
        rmsmin=pPosD%rmsdif
        angl_min(:) = angl_sav(:)
        resmax_tot=resmax
        kSat=iter-1
        DO kPar=1,nPar
          solmin(kPar)=sol(kPar)
          iPar=ikf(kPar,kPar)
          rmsest(kPar)=pPosD%rmsdif*DSQRT(angl(iPar))
        ENDDO
        pPosD%Qxy(:) = angl(:)*pPosD%rmsdif**2/opt%rms_phaObs**2
      ENDIF
    ENDIF

! Mark unacceptable residuals
! ---------------------------
    IF(iter == 1)THEN
      IF(nseff == nPar.OR.nseff == nplus)THEN
        EXIT screenLoop
      ELSE
        IF(pPosD%rmsdif < rmsmax_pha.AND.test <= pdfmax)THEN
          EXIT screenLoop
        ENDIF
      ENDIF
    ENDIF
!TESTT    write(lfnprt,*)cPos%iepo,resmax,pPosD%rmsdif
  ENDDO screenLoop
  IF (iter==nsat+2)iter=iter-1
!
! *********************************************************************
!
  IF (obsflg(nsat) == 'M') obsflg(nsat) = ' '
!
! update solution
! ---------------
  pPosD%rmsdif=rmsmin

  IF(nPar == 1)THEN
    pPosD%posdif(4) = (posApr(4,2) - posApr(4,1)) + solmin(1)
    pPosD%posdif(1:3) = posApr(1:3,2) - posApr(1:3,1)
  ELSE
    pPosD%posdif(1:3) = posApr(1:3,2) - posApr(1:3,1) + solmin(1:3)
    pPosD%posdif(4) =  (posApr(4,2) - posApr(4,1)) + solmin(4)

! TEST OUTPUT
! -----------
    pPosD%nsat=nsat
  ENDIF
!
! Set mark corresponding to best solution
! ---------------------------------------
  if(kSat /= 0)obsflg(kSat)='M'

! Set Return Code
! ---------------
  IF(nseff_sol < opt%minsat_pha.OR.pPosD%rmsdif>=1.D30)THEN
    iret=2
    go to 999
  ENDIF

  test=dsqrt(solmin(1)**2+solmin(2)**2+solmin(3)**2)
  test2=dsqrt(rmsest(1)**2+rmsest(2)**2+rmsest(3)**2)

  IF(nseff_sol == nplus_sol)THEN
    iret=0
  ELSEIF(nseff_sol > nPar)THEN
    IF(pPosD%rmsdif < rmsmax_pha.AND.test <= pdfmax)THEN
      iret=0
    ELSE
      iret=2
    ENDIF
  ELSEIF(nseff_sol == nPar)THEN
    IF(test <= pdfmax)THEN
      iret=0
    ELSE
      iret=2
    ENDIF
  ELSEIF(nseff_sol < nPar)then
    iret=2
  ENDIF

! Handle case of extremely bad PDOP
! --------------------------------
!  IF(opt%orbtyp==1.AND.test2>pdfmax/5.D0) THEN
  IF(opt%orbtyp==1.AND.test2>pdfmax) THEN
    iret=2
  ENDIF

! Set weights for different correlation studies
! ---------------------------------------------
  IF (opt%correl == 0) THEN
    pPosD%angl = 1/pPosD%rmsdif**2
  ELSEIF (opt%correl == 1) THEN
    pPosD%angl(1:nPar) = 1/rmsest(1:nPar)**2
  ELSEIF (opt%correl == 2.AND.iret == 0.AND.pPosD%rmsdif /= 0.D0) THEN
    pPosD%angl(:) = angl_min(:)
  ENDIF

!RESMAX  write(lfnprt,*)cPos%iepo,resmax_tot,pPosD%rmsdif,rmsmax_pha

999 CONTINUE

! Deallocate memory
! -----------------
  DEALLOCATE(qvv)

  RETURN
END SUBROUTINE diffsp_screen

END MODULE
