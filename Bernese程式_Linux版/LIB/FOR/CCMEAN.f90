MODULE s_CCMEAN
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE ccmean(CCopt,CCfil,InClkHead,OutClkHead,Epoch,                 &
                  nClock,ClkMean,NewClock,NewSigma,irCode)

! -------------------------------------------------------------------------
! Purpose:    Computes the mean and sigma of the clocks in list
!
! Author:     R. Dach
!
! Created:    18-Aug-2000
!
! Changes:    01-Sep-2000 RD: correct error mes. for outliers
!             10-Oct-2000 RD: new min. number of clocks for mean
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             25-Nov-2003 RD: Some POINTER->ALLOCATABLE
!             26-Nov-2003 HB: Check also for sigma == unDef (999999.999999D0)
!             21-Jan-2004 RD: handle maxMean == 0d0 (new input panel)
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             19-Jul-2010 SL: tab characters removed
!             19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnerr
  USE d_clkrnx, ONLY: t_clkhead,unDef
  USE p_ccrnxc, ONLY: t_ccrnxc_opt,t_ccrnxc_fil,t_clkmean,null
  USE s_timst2
  USE s_exitrc
  IMPLICIT NONE
!
! Variables from parameter list
! -----------------------------
  TYPE(t_ccrnxc_opt)       :: CCopt      ! Input options for combination
  TYPE(t_ccrnxc_fil)       :: CCfil      ! Names of clock rinex files
  TYPE(t_clkhead),DIMENSION(:), POINTER  &
                           :: InClkHead  ! header of input files
  TYPE(t_clkhead)          :: OutClkHead ! header of output file
  REAL(r8b)                :: Epoch      ! Epoch (sec. since TFirst)
  INTEGER(i4b)             :: nClock     ! Number of ClkMean records
  TYPE(t_clkmean),DIMENSION(:)           &
                           :: ClkMean    ! Clock information for combin.
  REAL(r8b)                :: NewClock   ! New Clock value
  REAL(r8b)                :: NewSigma   ! Sigma for the new clock value
  INTEGER(i4b)             :: irCode     ! Return code from the sr
!
! Local variables
! ---------------
  CHARACTER(len=19)              :: SEpoch     ! String of the time
!
  INTEGER(i4b)                   :: iClock     ! Counts of solutions contributing to the combination
  INTEGER(i4b)                   :: mClock     ! Number of solutions contributing to the combination
!
  REAL(r8b)                      :: ClkSum     ! Mean clock from the files to combine
  REAL(r8b)                      :: ClkSum2    ! Mean clock from the files to combine
  REAL(r8b)                      :: ClkSigm    ! Help for mean clock from the files to combine
  REAL(r8b)                      :: ClkSig0    ! Sigma for the new clock
  REAL(r8b)                      :: ClkSig1    ! Sigma for the new clock (strategy 1)
  REAL(r8b)                      :: ClkSig2    ! Sigma for the new clock (strategy 2)
  REAL(r8b)                      :: ClkResi    ! Resi during comp. the mean of the clocks
  REAL(r8b)                      :: Sigma      ! Weight of a clock for computing the mean

! Initialize NewSigma
! -------------------
  NewSigma = unDef
!
! Compute a simple arith. mean as 1st iteration
! ---------------------------------------------
  ClkSum2=0D0
  DO iClock=1,nClock
    ClkSum2=ClkSum2+ClkMean(iClock)%Value/nClock
  ENDDO
!
! Compute a Mean including all clocks
! -----------------------------------
  mClock=0
  ClkSum=0D0
  ClkSigm=0D0
  DO iClock=1,nClock
    mClock=mClock+1
    IF (CCopt%Combi == 0) THEN
      Sigma=1D0
    ELSE IF (CCopt%Combi == 1) THEN
      Sigma=DABS(ClkMean(iClock)%Value-ClkSum2)
    ELSE IF (CCopt%Combi == 2) THEN
      Sigma=ClkMean(iClock)%Sigma
    ELSE
      WRITE(lfnerr,'(//,A,/,16X,A,/)')                               &
      ' *** SR CCMEAN: INVALID VALUE FOR CCopt%Combi DETECTED',      &
                      'THIS IS A BUG!!!'
      CALL EXITRC(2)
    ENDIF
    IF (Sigma < null .OR. Sigma == unDef) Sigma = CCopt%Sigma0*1D-3
    ClkSum=ClkSum+ClkMean(iClock)%Value/Sigma
    ClkSigm=ClkSigm+1/Sigma
  ENDDO
  ClkSum=ClkSum/ClkSigm
!
! Computing the final mean without the bad clocks
! -----------------------------------------------
  mClock=0
  ClkSum2=0D0
  ClkSigm=0D0
  ClkSig2=0D0
  DO iClock=1,nClock
    ClkResi=ClkMean(iClock)%Value-ClkSum
    IF (CCopt%MaxMean == 0d0 .OR. &
        DABS(ClkResi*1D3) <= CCopt%MaxMean) THEN
      mClock=mClock+1
      IF (CCopt%Combi == 0) THEN
        Sigma=1D0
      ELSE IF (CCopt%Combi == 1) THEN
        Sigma=DABS(ClkResi)
      ELSE IF (CCopt%Combi == 2) THEN
        Sigma=ClkMean(iClock)%Sigma
      ENDIF
      IF (Sigma < null .OR. Sigma == unDef) Sigma = CCopt%Sigma0
      ClkSum2=ClkSum2+ClkMean(iClock)%Value/Sigma
      ClkSigm=ClkSigm+1/Sigma
!
! Perform error probagation
! -------------------------
      IF (ClkMean(iClock)%Sigma /= unDef) THEN
        ClkSig2=ClkSig2+(ClkMean(iClock)%Sigma/Sigma)**2
      ENDIF
      ClkSig2=ClkSig2+(ClkMean(iClock)%SigOff/Sigma)**2
    ELSE
!
! Assumption of an outlier
! ------------------------
      ClkMean(iClock)%Value=unDef
      CCopt%NumDel=CCopt%NumDel+1
      CALL timst2(1,1,InClkHead(1)%TFirst+Epoch/86400D0, SEpoch)
      IF (ClkMean(iClock)%iTyp == 1) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,F10.3,A,/,16X,2A,2(/,16X,2A),/)') &
          ' ### SR CCMEAN: OUTLIER FOR A STATION CLOCK DETECTED',            &
          'IT WILL NOT BE USED FOR COMPUTING THE ARITH. MEAN',               &
          'DEV. FROM MEAN: ',ClkResi*1D3,' NSEC',                            &
          'STATION       : ',TRIM(OutClkHead%ClkName(ClkMean(iClock)%iClk)), &
          'FILE          : ',TRIM(CCfil%ClkFilNam(ClkMean(iClock)%iFil)),    &
          'EPOCH         : ',SEpoch
      ELSE IF (OutClkHead%ClkName(ClkMean(iClock)%iClk)(1:1) == 'G') THEN
        WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,F10.3,A,/,16X,2A,2(/,16X,2A),/)') &
          ' ### SR CCMEAN: OUTLIER FOR A SATELLITE CLOCK DETECTED',          &
          'IT WILL NOT BE USED FOR COMPUTING THE ARITH. MEAN',               &
          'DEV. FROM MEAN: ',ClkResi*1D3,' NSEC',                            &
          'SATELLITE     :  ',OutClkHead%ClkName(ClkMean(iClock)%iClk)(2:3), &
          'FILE          : ',TRIM(CCfil%ClkFilNam(ClkMean(iClock)%iFil)),    &
          'EPOCH         : ',SEpoch
      ELSE
        WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,F10.3,A,/,16X,2A,2(/,16X,2A),/)') &
          ' ### SR CCMEAN: OUTLIER FOR A SATELLITE CLOCK DETECTED',          &
          'IT WILL NOT BE USED FOR COMPUTING THE ARITH. MEAN',               &
          'DEV. FROM MEAN: ',ClkResi*1D3,' NSEC',                            &
          'SATELLITE     : 1',OutClkHead%ClkName(ClkMean(iClock)%iClk)(2:3), &
          'FILE          : ',TRIM(CCfil%ClkFilNam(ClkMean(iClock)%iFil)),    &
          'EPOCH         : ',SEpoch
      ENDIF
    ENDIF
  ENDDO
!
! Compute the mean
! ----------------
  IF(mClock>0 .AND.                                                    &
    ((ClkMean(1)%iTyp == 1 .AND. mClock >= CCopt%MinClkSta) .OR.       &
     (ClkMean(1)%iTyp == 2 .AND. mClock >= CCopt%MinClkSat))) THEN
    ClkSum2=ClkSum2/ClkSigm
!
! Compute the sigma for the new clock value
! -----------------------------------------
    IF (CCopt%OutSigma == 1) THEN
!
! Strategy 1: STD from the combination
! ------------------------------------
      ClkSig1=0D0
      iClkLoop: DO iClock=1,nClock
        IF (ClkMean(iClock)%Value == unDef) CYCLE iClkLoop
        ClkSig1=ClkSig1+(ClkMean(iClock)%Value-ClkSum2)**2
      ENDDO iClkLoop
      IF (mClock == 1) THEN
        ClkSig0=DSQRT(ClkSig1)
      ELSE
        ClkSig0=DSQRT(ClkSig1/(mClock*(mClock-1)))
      ENDIF
    ELSE IF (CCopt%OutSigma == 2) THEN
!
! Strategy 2: Error Probagation
! -----------------------------
      ClkSig0=DSQRT(ClkSig2)/ClkSigm
    ELSE
      WRITE(lfnerr,'(//,A,/,16X,A,/)')                                  &
      ' *** SR CCMEAN: INVALID VALUE FOR CCopt%OutSigma DETECTED',      &
                      'THIS IS A BUG!!!'
      CALL EXITRC(2)
    ENDIF
!
! Put the results into output
! ---------------------------
    NewClock=ClkSum2
    IF (ClkSig0 /= 0.D0) NewSigma=ClkSig0
    irCode=0
  ELSE
    NewClock=unDef
    NewSigma=unDef
    irCode=1
  ENDIF
!
  RETURN
  END SUBROUTINE

END MODULE
