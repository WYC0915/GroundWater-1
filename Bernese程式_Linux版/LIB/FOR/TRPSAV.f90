MODULE s_TRPSAV
CONTAINS

! --------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! --------------------------------------------------------------------------

SUBROUTINE trpsav(title,iTropo,iExtra,iCentr,staNam,staFlg,         &
                  nPar,locq,xStEll,trpLms,xxx,aNor,rms,nSmpNq,iElvnq,      &
                  nStat,nCentr,xStat,datum,iTrMap,iTrGrd)


! --------------------------------------------------------------------------
! Purpose:    Save troposphere estimates (new version of old subroutine
!             TRPSAV.f)
!
! Author:     M. Meindl
!
! Created:    07-May-2003
!
! Changes:    18-Nov-2003 HB: SIZE(trpLms,2) instead of SIZE(trpLms(1,:))
!             29-Mar-2004 CU: Add dummy variable to call of sr tropos
!             28-Jun-2005 MM: Unused variables removed
!             13-Dec-2005 CU: Adapt call of SR tropos
!             24-Aug-2006 AG: SR tdelay instead of tropos used
!             30-Jun-2008 RD: VMF added
!             16-Jan-2011 RD: STANUM removed
!             01-Sep-2011 LP: Enable writing of tropo gradients in TRO SINEX
!             20-Sep-2012 RD: Correctly deallocate arrays
!             20-Sep-2012 RD: Use M_BERN with ONLY
!             20-Sep-2012 RD: Remove unused variables, modules and parameters
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! --------------------------------------------------------------------------


! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, fileNameLength
  USE d_trpest, ONLY: t_trpest, init_trpest

  USE f_ikf
  USE s_tdelay
  USE s_alcerr
  USE s_wttrpe
  USE s_trpvec
  USE s_wttrpsnx
  IMPLICIT NONE


! List of parameters
! ------------------
! input
  CHARACTER(LEN=80)               :: title  ! Title line
  INTEGER(i4b)                    :: iTropo ! Tropospheric model
  INTEGER(i4b)                    :: iExtra ! Extrapolated meteo
                                            !  =0: no
                                            !  =1: yes
                                            !  =2: estimated values
  INTEGER(i4b),DIMENSION(:)       :: iCentr ! Index of center station
                                            ! for station i  (i=1,..,nStat)
  CHARACTER(LEN=16),DIMENSION(:)  :: staNam ! Station names  (i=1,..,nStat)
  CHARACTER(LEN=1),DIMENSION(:)   :: staFlg ! Station flags  (i=1,..,nStat)
  INTEGER(i4b)                    :: nPar   ! Total number of parameters
  INTEGER(i4b),DIMENSION(:,:)     :: locq   ! Parameter characterization
                                            !  (i=1,..,maxlocq;j=1,...)
  REAL(r8b),DIMENSION(:,:)        :: xStEll ! Ellipsoidal sta coordinates
                                            !  (i=1,..3;j=1,..,nStat)
  REAL(r8b),DIMENSION(:,:)        :: trpLms ! Parameter window (t1=epoch)
                                            !  (i=1,2;j=1,..,nTrSta)
  REAL(r8b),DIMENSION(:)          :: xxx    ! Solution vector (i=1,...)
  REAL(r8b),DIMENSION(:)          :: aNor   ! Normal equation
                                            !  (i=1,..,nPar*(nPar+1)/2)
  REAL(r8b)                       :: rms    ! rms of one phase observation
  INTEGER(i4b)                    :: nSmpNq ! Data sample rate (sec)
  INTEGER(i4b)                    :: iElvnq ! Min elevation angle (deg)
  INTEGER(i4b)                    :: nStat  ! Number of stations
  INTEGER(i4b)                    :: nCentr ! Number of center stations
  REAL(r8b),DIMENSION(:,:)        :: xStat  ! Geocentric sta coordinates
                                            !  (i=1,..3;j=1,..,nStat)
  CHARACTER(LEN=16)               :: datum  ! Local geodetic datum
  INTEGER(i4b)                    :: iTrMap ! Mapping function
                                            !  =1: 1/cos(z)
                                            !  =2: Hopfield
                                            !  =3: dry Niell
                                            !  =4: wet Niell
                                            !  =5: dry GMF
                                            !  =6: wet GMF
                                            !  =7: dry VMF
                                            !  =8: wet VMF
  INTEGER(i4b),DIMENSION(:)       :: iTrGrd ! (1) gradient estimation
                                            !      =0: not estimated
                                            !      =1: tilting
                                            !      =2: linear
                                            !      =3: tan(z)
                                            ! (2) ratio of number of zenith
                                            !      to gradient parameters


! Local variables
! ---------------
! general
  CHARACTER(LEN=fileNameLength)          :: filNam
  REAL(r8b)                              :: hlpTab
  INTEGER(i4b)                           :: iTropM

! troposphere estimates, aNor index, locq
  TYPE(t_trpest)                         :: trpEst
  INTEGER(i4b),DIMENSION(:,:),ALLOCATABLE:: aIdx
  REAL(r8b),DIMENSION(:,:),ALLOCATABLE   :: grdTim

! indices and loop variables
  INTEGER(i4b)                           :: iSta, nSta
  INTEGER(i4b)                           :: iTrp, nTrp
  INTEGER(i4b)                           :: iPar, nRec
  INTEGER(i4b)                           :: iReq
  INTEGER(i4b)                           :: iOldSta,ihelp1,ihelp2

! variables for meteo routines
  REAL(r8b),DIMENSION(12)                :: grdInf
  REAL(r8b)                              :: press, temp, hum
  REAL(r8b)                              :: zen
  REAL(r8b)                              :: dR

! error codes
  INTEGER(i4b)                           :: irc, iac, irCode



! Some initializations
! --------------------
  CALL init_trpest(trpEst)
  nRec    = 0
  nSta    = 0
  nTrp    = 0
  iOldSta = 0


! Count number of stations
! ------------------------
  DO iPar=1,nPar
    IF (locq(1,iPar)/=6 .OR. locq(4,iPar)/=3) CYCLE

! new station?
    IF (iOldSta/=locq(3,iPar)) THEN
      nSta    = nSta+1
      iOldSta = locq(3,iPar)
    END IF
    nRec = nRec+1
  END DO


! No troposphere estimated
! ------------------------
  IF (nRec==0) RETURN


! Adjust troposphere model
! ------------------------
  IF (iExtra==0) THEN
    iTropM = iTropo
  ELSE IF (iExtra==1) THEN
    iTropM = -iTropo
  ELSE IF (iExtra==2) THEN
    iTropM = iTropo+100
  END IF


! Allocate tropo structure and index array
! ----------------------------------------
  trpEst%nRec = nRec
  trpEst%nSta = nSta
  ALLOCATE(trpEst%sta(nSta),STAT=iac)
  CALL alcerr(iac,'trpEst%sta',(/nSta/),'sr trpsav')
  ALLOCATE(aIdx(nRec,5),STAT=iac)
  CALL alcerr(iac,'aIdx',(/nRec,5/),'sr trpsav')
  ALLOCATE(grdTim(nRec,2),STAT=iac)
  CALL alcerr(iac,'grdTim',(/nRec,2/),'sr trpsav')
  aIdx   = 0
  grdTim = 0

! Count number of troposphere parameters
! --------------------------------------
  iOldSta = 0
  nSta    = 0
  trpEst%sta(:)%nTrp = 0

  DO iPar=1,nPar
    IF (locq(1,iPar)/=6 .OR. locq(4,iPar)/=3) CYCLE

! new station?
    IF (iOldSta/=locq(3,iPar)) THEN
      nSta = nSta+1
      trpEst%sta(nSta)%staNam = staNam(locq(3,iPar))
      trpEst%sta(nSta)%staFlg = staFlg(locq(3,iPar))
      iOldSta      = locq(3,iPar)
    END IF

    trpEst%sta(nSta)%nTrp = trpEst%sta(nSta)%nTrp+1
  END DO


! Allocate memory
! ---------------
  DO iSta=1,trpEst%nSta
    nTrp = trpEst%sta(iSta)%nTrp
    ALLOCATE(trpEst%sta(iSta)%trp(nTrp),STAT=iac)
    CALL alcerr(iac,'trpEst%sta%trp',(/nTrp/),'sr trpsav')
    DO iTrp=1,nTrp
      trpEst%sta(iSta)%trp(iTrp)%corr   = 0.d0
      trpEst%sta(iSta)%trp(iTrp)%sigma  = 0.d0
      trpEst%sta(iSta)%trp(iTrp)%timInt = 0.d0
      trpEst%sta(iSta)%trp(iTrp)%total  = 0.d0
      trpEst%sta(iSta)%trp(iTrp)%model  = 0.d0
    END DO
  END DO


! Loop over all parameters
! ------------------------
  iOldSta = 0
  nRec    = 0
  nSta    = 0

  ParLoop: DO iPar=1,nPar
    IF (locq(1,iPar)/=6 .OR. locq(4,iPar)/=3) CYCLE
    iReq = locq(2,iPar)
    iSta = locq(3,iPar)

! new station
    IF (iOldSta/=iSta) THEN
      iOldSta = iSta
      nSta    = nSta+1
      iTrp    = 0
    END IF
    iTrp       = iTrp+1
    nRec       = nRec+1
    aIdx(nRec,5) = iPar

! time window
    trpEst%sta(nSta)%trp(iTrp)%timInt = trpLms(:,iReq)

! gradient for epoch
    IF (iTrGrd(1)>0) THEN
      CALL TRPVEC(iPar,nPar,locq,xxx,aNor,rms,iTrGrd,iExtra,               &
                  iTropo,xStEll(:,iSta),trpLms(1,iReq),grdInf,aIdx(nRec,1:4))
      trpEst%sta(nSta)%trp(iTrp)%corr(1:2)  = (/grdInf(9),grdInf(11)/)
      trpEst%sta(nSta)%trp(iTrp)%sigma(1:2) = (/grdInf(10),grdInf(12)/)

      ihelp1 = aIdx(nRec,1)
      ihelp2 = aIdx(nRec,3)
      grdTim(nRec,1:2) = (/trpLms(1,locq(2,ihelp1)),trpLms(1,locq(2,ihelp2))/)
    END IF

! ZPD parameter
    trpEst%sta(nSta)%trp(iTrp)%corr(3)  = xxx(iPar)
    trpEst%sta(nSta)%trp(iTrp)%sigma(3) = rms*sqrt(aNor(ikf(iPar,iPar)))

    IF (iExtra==0) THEN

! model set to zero if observed meteo was used
      trpEst%sta(nSta)%trp(iTrp)%model = 0.d0
    ELSE
      zen = 0.d0
      CALL tdelay(trpLms(1,iReq),zen,xStEll(:,iSta),iTropo,1,0d0,temp, &
                                                             press,hum,dR)
    END IF

    trpEst%sta(nSta)%trp(iTrp)%model = dR
    trpEst%sta(nSta)%trp(iTrp)%total = dR+xxx(iPar)

  END DO ParLoop


! Some information for header
! ---------------------------
  trpEst%head%title   = title
  trpEst%head%iFrmt  = 1
  trpEst%head%iTrpMd = iTropM
  trpEst%head%iTrMap = iTrMap
  trpEst%head%iTrGrd = iTrGrd
  trpEst%head%iElvnq = iElvnq
  trpEst%head%iTab   = 1.d20
  DO iTrp=2,SIZE(trpLms,2)
    hlpTab = abs(trpLms(1,iTrp)-trpLms(1,iTrp-1))
    IF (hlpTab==0.d0) CYCLE
    IF (hlpTab<trpEst%head%iTab) trpEst%head%iTab = hlpTab
  END DO


! Flag singular parameters with timInt(1)=0.d0
! --------------------------------------------
  DO iSta=1,trpEst%nSta
    DO iTrp=1,trpEst%sta(iSta)%nTrp
      IF (trpEst%sta(iSta)%trp(iTrp)%sigma(3)==0.d0) THEN
        trpEst%sta(iSta)%trp(iTrp)%timInt(1) = 0.d0
      END IF
    END DO
  END DO


! Write Bernese file
! ------------------
  filNam = " "
  CALL wttrpe(filNam,trpEst,irc)


! Write a troposphere SINEX file
! ------------------------------
  filNam = " "
  CALL wttrpsnx(1,filNam,trpEst,aIdx,aNor,xxx,locq,rms,nPar,               &
                nSmpNq,nStat,nCentr,xStat,iCentr,staNam,datum,irCode,grdTim)


! Free some memory
! ----------------
  DO iSta=1,trpEst%nSta
    DEALLOCATE(trpEst%sta(iSta)%trp)
  END DO
  DEALLOCATE(trpEst%sta)
  DEALLOCATE(aIdx)
  DEALLOCATE(grdTim)


! Subroutine ends here
! --------------------
  RETURN
END SUBROUTINE trpsav

END MODULE
