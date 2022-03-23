MODULE s_WGTKIN
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE wgtkin(ip0,nPar,aNor,locq,stName,clkhed,clkrec, &
                    aEll,bEll,dxEll,drEll,scEll,xStat,       &
                    sigapr,nstwgt,istwgt,stWgt)

! -------------------------------------------------------------------------
! Purpose:    Compute covariance matrix for XYZ coordinate constraining
!             from NEU sigmas for kinematic positioning
!
! Remark:     For non-ground stations (markerType /= " ") stWgt containts
!             constraints in XYZ (per default one value).
!             If a kinematic input file (KININP) is available only epochs
!             with flag "K" are constrained.
!
! Author:     R. Dach
!
! Created:    21-Jul-2003
! Last mod.:  21-Jul-2003
!
! Changes:    __-___-____ __:
!
! SR used:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_clkrnx, ONLY: t_clkHead, t_clkRec
  USE p_gpsest, ONLY: maxlcq

  USE f_ikf
  USE s_err3d
  USE s_staflg
  USE s_dminv
  USE s_readkin
  USE s_gtflna
  USE s_xyzell
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                   :: ip0         ! Start searching for kin param.
  INTEGER(i4b)                   :: nPar        ! Total number of parameters

! input/output
  REAL(r8b),    DIMENSION(*)     :: aNor        ! Normal equation

! input
  INTEGER(i4b), DIMENSION(MAXLCQ,*):: locq       ! definition of parameter
  CHARACTER(LEN=*), DIMENSION(*) :: stName      ! List of station names
  REAL(r8b)                      :: aEll,bEll   ! aEll,bEll: axes of ellipsoid
  REAL(r8b),    DIMENSION(3)     :: dxEll,drEll ! shift, rot. of ellipsoid
  REAL(r8b)                      :: scEll       ! scale of ellipsoid
  REAL(r8b),    DIMENSION(3,*)   :: xStat       ! coordinates of station
  INTEGER(i4b)                   :: nstwgt      ! # stations with a priori
                                                ! weights for coordinates
  INTEGER(i4b), DIMENSION(*)     :: istwgt      ! station numbers for weights
  REAL(r8b),    DIMENSION(3,*)   :: stwgt       ! a priori weights for stations
  TYPE(t_clkHead)                :: clkhed ! %tFirst: MJD of first epoch param.
  TYPE(t_clkRec)                 :: clkrec ! %nEpo: # epochs with highest sampl.
                                           ! %epoch(1): last epoch (sec. since
                                           !   clkhed%tFirst)
  REAL(r8b)                      :: sigapr ! a priori sigma


! Local Variables
! ---------------
  CHARACTER(LEN=20)                   :: marTyp
  CHARACTER(LEN=fileNameLength), SAVE :: kininp

  INTEGER(i4b)                        :: ip
  INTEGER(i4b)                        :: iSta,jSta
  INTEGER(i4b)                        :: iEpo
  INTEGER(i4b)                        :: iFlag
  INTEGER(i4b),DIMENSION(3)           :: l1,l2
  INTEGER(i4b)                        :: ii,kk,ik
  INTEGER(i4b),                  SAVE :: ircKin, ircPos

  REAL(r8b)                           :: dSec,tEpoch
  REAL(r8b)                           :: e2,sp
  REAL(r8b)                           :: fak
  REAL(r8b)                           :: den,det
  REAL(r8b)                           :: rn,rm
  REAL(r8b), DIMENSION(3)             :: xKin,xEll
  REAL(r8b), DIMENSION(3,3)           :: covplh
  REAL(r8b), DIMENSION(3,3)           :: covxyz

  LOGICAL,                       SAVE :: first = .TRUE.

! Get the name of a kinematic input file
! --------------------------------------
  IF (first) THEN
    CALL gtflna(0,'KININP',kininp,ircKin)
    first = .FALSE.
  ENDIF

! Loop all parameters
! -------------------
  DO ip = ip0,npar

! Find X-component of kinematic coordinates
! -----------------------------------------
    IF (locq(1,ip) /= 21) CYCLE
    IF (locq(3,ip) /=  1) CYCLE

! Check the other coordinate components
! -------------------------------------
    iSta = locq(2,ip)
    iEpo = locq(4,ip)

    IF (locq(1,ip+1) /=   21 .OR. locq(1,ip+2) /=   21 .OR. &
        locq(2,ip+1) /= iSta .OR. locq(2,ip+2) /= iSta .OR. &
        locq(3,ip+1) /=    2 .OR. locq(3,ip+2) /=    3 .OR. &
        locq(4,ip+1) /= iEpo .OR. locq(4,ip+2) /= iEpo) THEN
      WRITE(lfnerr,'(/,A,2(/,16X,A),/,16X,A,I10,/)')                 &
      ' ### SR WGTKIN: A kinematic coordinate component is missing.',&
                      '(may be due problems inverting the NEQ).',    &
                      'Station name:  '//TRIM(stName(iSta)),         &
                      'Epoch number:  ',iepo
      CYCLE
    ENDIF

! There are constarints for this stations?
! ----------------------------------------
    jSta = 0
    DO ii = 1,nstwgt
      IF (iSta == istwgt(ii)) jSta = ii
    ENDDO

    IF (jSta == 0) CYCLE

! Compute the epoch
! -----------------
    dsec   = DBLE(iEpo-1)*clkrec%epoch(1)/clkrec%nEpo
    tEpoch = clkhed%tFirst+dsec/86400.D0

! Constrain only with flag "K" in KININP
! --------------------------------------
    IF (ircKin == 0) THEN
      CALL readKin(kinInp,stName(iSta),tEpoch,1,0,xKin,ircPos)
      IF (ircPos == 1) CYCLE
    ENDIF

! Get the marker type
! -------------------
    CALL staFlg(stName(iSta),tEpoch,iFlag,marTyp)

! Terrestrial station: convert NEU->XYZ
! -------------------------------------
    IF (marTyp == ' ') THEN

      ! Get the actual coordinates:
      IF (ircKin /= 0 .OR. ircPos /= 0) &
        xKin(1:3) = xStat(1:3,iSta)

      CALL xyzell(aEll,bEll,dxell,drell,scell,xKin,xEll)

      ! Transform constraints (NEU->XYZ)
      e2  = (aEll**2-bEll**2)/(aEll**2)
      sp  = DSIN(xEll(1))
      den = DSQRT(1.D0-e2*sp**2)
      rn  = aEll/den
      rm  = aEll*(1.D0-e2)/den**3

      covplh = 0.D0
      DO ii = 1,3
        IF (ii == 1) fak = (rm+xEll(3))
        IF (ii == 2) fak = (rn+xEll(3))
        IF (ii == 3) fak = 1.D0

        covplh(ii,ii) = (stWgt(ii,jSta)/fak/sigapr)**2
        IF (ii == 2) covplh(ii,ii) = covplh(ii,ii)/DCOS(xEll(1))**2
      ENDDO

      CALL err3d(xEll(1),xEll(2),xEll(3),aEll,bEll,+1,covxyz,covplh)
      CALL dminv(covxyz,3,det,l1,l2)

      ! Put weights on normal equation
      DO ii =1,3
        DO kk = 1,ii
          ik=IKF(ii+ip-1,kk+ip-1)
          ANOR(ik)=ANOR(ik)+covxyz(ii,kk)
        ENDDO
      ENDDO

! Other marker types: constraints are in XYZ
! ------------------------------------------
    ELSE

      DO ii =1,3
        ik=IKF(ii+ip-1,ii+ip-1)
        ANOR(ik)=ANOR(ik)+(sigapr/stWgt(ii,jSta))**2
      ENDDO

    ENDIF

  ENDDO ! Next parameter

  RETURN
END SUBROUTINE wgtkin

END MODULE
