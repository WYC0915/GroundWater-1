MODULE s_CEALLIGN
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE ceallign(clkopt,InClkHead,InClkRec,nepo,epoch,nsat, &
                      ipar,mepo,clk,sckirc,irfrnx,brdepo,brdoff,brdrft)
! -------------------------------------------------------------------------
! Purpose:    Compute allignment to satellite clocks
!
! Remarks:    - Computation and application of allignment if clkopt%allign=1
!             - if clkopt%ifix=0: The allignment is realized as a linear fit
!               to the satellite clock corrections as given by clk (i.e. from
!               the clock combination). It is assumed that the clock
!               corrections are relative to an a priori (e.g. broadcast)
!             - if clkopt%ifix=1: The allignment is taken from the reference
!               station given in the Clock Rinex file to fix on.
!             - correction to be added to clocks
!                         corr = brdoff + brdrft*(epoch(it)-brdepo)
!             - Subroutine of CLKEST
!
! Author:     U. Hugentobler
!
! Created:    11-Aug-2001
! Last mod.:  30-Aug-2006
!
! Changes:    12-Aug-2002  HU: Get reference clock index from InClkHead
!             30-Aug-2002  HU: Get reference clock index from parameter list
!             30-Aug-2006  RD: sckirc added
!
! SR used:    dminv
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE p_clkest, ONLY: t_clkopt
  USE d_clkrnx, ONLY: t_clkhead,t_clkrec
  USE s_dminv
  IMPLICIT NONE

! DUMMY ARGUMENTS
! ---------------
! Input:
  TYPE(t_clkopt)              :: clkopt    ! option structure for clkest
  TYPE(t_clkhead)             :: InClkHead ! header structure of CLK RINEX file
  TYPE(t_clkrec)              :: InClkRec  ! data structure of CLK RINEX file
  INTEGER(i4b)                :: nepo      ! number of epochs
  REAL(r8b),DIMENSION(:)      :: epoch     ! epochs, i=1,nepo
  INTEGER(i4b)                :: nsat      ! number of satellites
  INTEGER(i4b),DIMENSION(:)   :: ipar      ! number of epoch params, i=1,nsat
  INTEGER(i4b),DIMENSION(:,:) :: mepo      ! epoch index for param i and
                                           ! satellite k
  INTEGER(i4b),DIMENSION(:)   :: sckirc    ! satclk in input file, i=1,nsat
  REAL(r8b),DIMENSION(:,:)    :: clk       ! clock corrections for param i and
                                           ! satellite k from combination
  INTEGER(i4b)                :: irfrnx    ! index of reference in CLK RINEX

! Output:
  REAL(r8b)                   :: brdepo    ! reference epoch (mjd)
  REAL(r8b)                   :: brdoff    ! clock offset (sec)
  REAL(r8b)                   :: brdrft    ! clock drift

! LOCAL VARIABLES
! ---------------
  INTEGER(i4b)              :: jsat,ipa
  INTEGER(i4b)              :: nclk
  INTEGER(i4b),DIMENSION(2) :: lh1,lh2

  REAL(r8b)                 :: clkrf
  REAL(r8b)                 :: epo,trel,corr
  REAL(r8b),DIMENSION(2)    :: bmat
  REAL(r8b),DIMENSION(2,2)  :: qmat
  REAL(r8b)                 :: det

! NO ALLIGNMENT REQUESTED
! -----------------------
  IF (clkopt%allign.EQ.0) RETURN

! ALLIGN TO SATELLITE CLOCKS
! --------------------------

! ifix=1: Take allignment from input RNX file
  IF (clkopt%ifix==1 .AND. irfrnx>0) THEN
    bmat=0D0
    qmat=0D0
    nclk=InClkRec%nEpo
    brdepo=InClkHead%TFirst+InClkRec%Epoch(1)/86400D0
    DO ipa=1,nclk
      epo=InClkHead%TFirst+InClkRec%Epoch(ipa)/86400D0
      trel=epo-brdepo
      clkrf=InClkRec%Clock(irfrnx,ipa)
      IF (clkrf==999999.999999D0) CYCLE
      corr=clkrf*1D-6
      bmat(1)=bmat(1)+corr
      bmat(2)=bmat(2)+corr*trel
      qmat(1,1)=qmat(1,1)+1D0
      qmat(1,2)=qmat(1,2)+trel
      qmat(2,2)=qmat(2,2)+trel**2
    ENDDO
    qmat(2,1)=qmat(1,2)
    CALL dminv(qmat,2,det,lh1,lh2)
    brdoff=qmat(1,1)*bmat(1)+qmat(1,2)*bmat(2)
    brdrft=qmat(2,1)*bmat(1)+qmat(2,2)*bmat(2)

!    epo1rf=InClkHead%TFirst+InClkRec%Epoch(1)/86400D0
!    clk1rf=InClkRec%Clock(irfrnx,1)*1D-6
!    epo2rf=InClkHead%TFirst+InClkRec%Epoch(NCLK)/86400D0
!    clk2rf=InClkRec%Clock(irfrnx,nclk)*1D-6
!    brdepo=epo1rf
!    brdoff=clk1rf
!    brdrft=(clk2rf-clk1rf)/(epo2rf-epo1rf)

! ifix=0: Take allignment from code clock corrections
  ELSE
    bmat=0D0
    qmat=0D0
    brdepo=epoch(1)
    DO jsat=1,nsat
      IF (sckirc(jsat) == 2) CYCLE
      DO ipa=1,ipar(jsat)
        epo =epoch(mepo(ipa,jsat))
        trel=epo-brdepo
        corr=-clk(ipa,jsat)
        bmat(1)=bmat(1)+corr
        bmat(2)=bmat(2)+corr*trel
        qmat(1,1)=qmat(1,1)+1D0
        qmat(1,2)=qmat(1,2)+trel
        qmat(2,2)=qmat(2,2)+trel**2
      ENDDO
    ENDDO
    qmat(2,1)=qmat(1,2)
    CALL dminv(qmat,2,det,lh1,lh2)
    brdoff=qmat(1,1)*bmat(1)+qmat(1,2)*bmat(2)
    brdrft=qmat(2,1)*bmat(1)+qmat(2,2)*bmat(2)
  ENDIF

! WRITE OUTPUT
! ------------
  WRITE(lfnprt,"(' ALLIGNMENT TO A PRIORI CLOCK CORRECTIONS', &
            &  /,' ----------------------------------------', &
            & //,' Reference Epoch :',F16.5, &
            &  /,' Offset (usec)   :',F16.5, &
            &  /,' Drift (usec/day):',F16.5)") brdepo,brdoff*1d6,brdrft*1D6

  IF (clkopt%ifix.EQ.1 .AND. irfrnx>0) THEN
    WRITE(lfnprt,"(' Taken from reference clock in input RNX file:  ', &
                    & A,/)") TRIM(InClkHead%clkName(irfrnx))
  ELSE
    WRITE(LFNPRT,"(/)")
  ENDIF

  RETURN
END SUBROUTINE ceallign

END MODULE
