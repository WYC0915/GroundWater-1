MODULE s_CCOMBO
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE ccombo(Epo1,Epo2,CCopt,CCfil,               &
           InClkHead,OutClkHead,InClkRec,OutClkRec,irCode)

! -------------------------------------------------------------------------
! Purpose:    Combines the clock record of the interval (Epo1,Epo2)
!
! Author:     R. Dach
!
! Created:    18-Aug-2000
!
! Changes:    01-Sep-2000 RD: Give typ of clocks into ClkMean record
!             07-Mar-2001 LM: Syntax correction (Win32 Problem)
!             14-Feb-2001 RD: Use SR ALCERR
!             16-Feb-2001 RD: Put some stuff into SRs (cclknq, ccincr)
!             14-May-2001 RD: Improved structure of reference clock array
!             18-May-2001 RD: Bugfix for only one file
!             22-May-2001 RD: Wrong error message
!             05-Jun-2001 RD: All clocks from one file are outliers
!             21-Jun-2001 RD: Removing outliers produce may a singularity
!             11-Sep-2001 RD: All files doe not much together for one epoch
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             15-Jan-2002 RD: Format error for marked clocks
!             04-Jun-2002 RD: Make the PC version work
!             13-Nov-2002 RD: Modified program output
!             23-Apr-2003 CU: Nullify local pointers
!             15-Jul-2003 RD: Supress index error
!             14-Aug-2003 RD: Update CCopt%clkData if clock is in 1 file only
!             12-Nov-2003 RD: Use SR syminvg instead of SR syming
!             25-Nov-2003 RD: Some POINTER->ALLOCATABLE
!             02-Dec-2003 HB: Initialize sigma and clock with
!                             unDef = 999999.999999D0
!             21-Jan-2004 RD: Handle maxResi == 0d0 (new input panel)
!             09-Feb-2004 RD: No more overwriting when an outlier detected
!             18-Apr-2005 RD: Norm. RMS of combination in the pgm output
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             21-Sep-2009 RD: Handle eclipse flags
!             09-Aug-2010 RD: New syminvg used
!             19-Sep-2012 RD: Correctly deallocate all arrays
!             19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnprt, lfnerr
  USE p_ccrnxc, ONLY: t_ccrnxc_opt,t_ccrnxc_fil,t_clkmean,t_index,prtcmb
  USE d_clkrnx, ONLY: t_clkhead,t_clkrec,unDef
  USE f_ikf
  USE s_alcerr
  USE s_ccmean
  USE s_cclknq
  USE s_timst2
  USE s_solve
  USE s_syminvg
  USE s_ccincr
  USE s_clrFlg
  USE s_setFlg
  USE f_tstFlg
  IMPLICIT NONE

! List of Parameters
! ------------------
! input
  INTEGER(i4b)                   :: Epo1,Epo2  ! Defines the window of this run
  TYPE(t_ccrnxc_opt)             :: CCopt      ! Input options for combination
  TYPE(t_ccrnxc_fil)             :: CCfil      ! Names of clock rinex files
  TYPE(t_clkhead),DIMENSION(:),   &
                  POINTER        :: InClkHead  ! header of input files
  TYPE(t_clkhead)                :: OutClkHead ! header of output file
  TYPE(t_clkrec) ,DIMENSION(:),   &
                  POINTER        :: InClkRec   ! input data records

! output
  TYPE(t_clkrec)                 :: OutClkRec  ! Combined data
  INTEGER(i4b)                   :: irCode     ! Return code from the sr

! Local variables
! ---------------
  TYPE(t_clkmean)  , DIMENSION(:),&
                     ALLOCATABLE :: ClkMean    ! Index for combuting the mean clock

  CHARACTER(len=19), DIMENSION(2):: EpoStr     ! String of the time
  CHARACTER(len=19)              :: SEpoch     ! String of the time

  INTEGER(i4b),      DIMENSION(:),&
                     ALLOCATABLE :: FilLst     ! Index of the files with valid entries
  INTEGER(i4b)                   :: numFil     ! Number of files in FilLst
  INTEGER(i4b)                   :: iFil,jFil  ! Counts the file in list
  INTEGER(i4b)                   :: nFil       ! Number of files in list
  INTEGER(i4b)                   :: iClk,jClk  ! Counts the clocks in list
  INTEGER(i4b)                   :: nSta       ! Number of stations for param-est.
  INTEGER(i4b)                   :: nSat       ! Number of satellites for param-est.
  INTEGER(i4b)                   :: iObs,jOBS  ! Counts the observations for param-est.
  INTEGER(i4b)                   :: nObs       ! Number of observations for param-est.
  INTEGER(i4b)                   :: numObs     ! Number of obs+pseudo-obs for param-est.
  INTEGER(i4b)                   :: iRef       ! Counts the reference clocks
  INTEGER(i4b)                   :: jRef       ! Counts the reference clocks
  INTEGER(i4b)                   :: iDel, jDel ! Counts the outliers to remove
  INTEGER(i4b)                   :: mDel, nDel ! Number of outliers removed
  INTEGER(i4b)                   :: iEpo, jEpo ! Counts the epoch numbers in list
  INTEGER(i4b)                   :: mSing      ! Number of singular param (1st iter.)
  INTEGER(i4b)                   :: nSing      ! Number of singular parameters
  INTEGER(i4b)                   :: nClock     ! Number of solutions contributing to the combination
  INTEGER(i4b)                   :: iDummy     ! A dummy variable
  INTEGER(i4b)                   :: ii
  INTEGER(i4b)                   :: ios        ! io status
!
  REAL(r8b)                      :: NewClock   ! New clock combination
  REAL(r8b)                      :: NewSigma   ! Sigma for new clock combination
!
! Matrices for offset computation
! -------------------------------
  TYPE(t_index), DIMENSION(:),    &
                 POINTER         :: Idx        ! Index for the observations
  TYPE(t_index), DIMENSION(:),    &
                 ALLOCATABLE     :: Idx1

  INTEGER(i4b),  DIMENSION(:,:),  &
                 POINTER         :: A          ! Design matrix
  REAL(r8b),     DIMENSION(:),    &
                 POINTER         :: P          ! Diagonal matrix with apriori weights
  REAL(r8b),     DIMENSION(:),    &
                 POINTER         :: Y          ! Observation vector
  REAL(r8b),     DIMENSION(:),    &
                 ALLOCATABLE     :: b          ! Vector with solution
  INTEGER(i4b),  DIMENSION(:,:),  &
                 ALLOCATABLE     :: A1
  REAL(r8b),     DIMENSION(:),    &
                 ALLOCATABLE     :: P1
  REAL(r8b),     DIMENSION(:),    &
                 ALLOCATABLE     :: Y1
  REAL(r8b),     DIMENSION(:),    &
                 ALLOCATABLE     :: APA        ! AT * P * A
  REAL(r8b),     DIMENSION(:),    &
                 ALLOCATABLE     :: APY        ! AT * P * Y
  REAL(r8b),     DIMENSION(:),    &
                 ALLOCATABLE     :: YAb        ! Y - A * b
  INTEGER(i4b),  DIMENSION(:),    &
                 ALLOCATABLE     :: ParLst     ! List of singular parameters
  REAL(r8b)                      :: RMS        ! RMS of the offset estimation
  REAL(r8b)                      :: RMS_P      ! RMS of the offset estimation
!
! Initialization
! --------------
  nFil=SIZE(CCfil%ClkFilNam)
!
  irCode=0
  iOBS=0
  nSat=0
  nSta=0
!
  NULLIFY(Idx)
  NULLIFY(A)
  NULLIFY(P)
  NULLIFY(Y)
!
  ALLOCATE(filLst(nFil), stat=ios)
  CALL alcerr(ios,'FilLst',(/nFil/),'ccombo')
!
  numfil=0
  filLst(:)=0
!
! Define EpoStr
! -------------
  CALL timst2(1,1,OutClkHead%TFirst+OutClkRec%Epoch(EPO1)/86400d0, EpoStr(1))
  CALL timst2(1,1,OutClkHead%TFirst+OutClkrec%Epoch(EPO2)/86400d0, EpoStr(2))
!  write(*,*) epoStr(1)//'  -  '//epoStr(2)
!
! Get all observations for the offset estimation
! ----------------------------------------------
  CALL cclknq(EPO1, EPO2, CCopt, InClkHead, OutClkHead, InClkRec, OutClkRec, &
    numFil, FilLst, nOBS, nSta, nSat, A,P,Y, Idx)
!
! Set the weights, pseudo observations
! ------------------------------------
  IF (nOBS > 0) THEN
    numOBS=nOBS+1
    IF (numOBS > SIZE(Y)) CALL ccincr(A,P,Y,Idx,numOBS)
!
    IF (CCopt%RefFil) THEN
      A(1,numOBS) = 1
      Y(numOBS)   = 0d0
      P(numOBS)   = (CCopt%Sigma0*1D-3)**2
    ELSE
      A(:,numOBS) = 1
      Y(numOBS)   = 0d0
      P(numOBS)   = (CCopt%Sigma0*1D-3)**2
    END IF
!
! Allocate NEQ-arrays
! -------------------
    nDEL        =  0
    mSing       = -1
    Idx%iFil(1) =  0
    Idx%iFil(2) =  0
!
    ALLOCATE(APA(IKF(nFil,nFil)),stat=ios)
    CALL alcerr(ios,'APA',(/IKF(nFil,nFil)/),'ccombo')
!
    ALLOCATE(APY(nFil),stat=ios)
    CALL alcerr(ios,'APY',(/nFil/),'ccombo')
!
    ALLOCATE(b(nFil),stat=ios)
    CALL alcerr(ios,'b',(/nFil/),'ccombo')
!
    ALLOCATE(ParLst(nFil),stat=ios)
    CALL alcerr(ios,'ParLst',(/nFil/),'ccombo')
!
    LSQLoop: DO
!
! No pseudo obs. for empty columns
! --------------------------------
      DO iFil = 1, nFil
        jOBS = 0
        DO iOBS = 1, nOBS
          jOBS = jOBS + ABS(A(iFil, iOBS))
        ENDDO
        IF (jOBS == 0) A(iFil,numOBS) = 0
      ENDDO
!
! Compute AT * P * A  resp.  AT * P * Y
! -------------------------------------
      APA=0D0
      APY=0D0
      b=0D0
      ParLst=0
      DO iFil=1,nFil
        DO iOBS=1,numOBS
          DO jFil=1,iFil
            IF (A(iFil,iOBS)*A(jFil,iOBS) /= 0)             &
            APA(IKF(iFil,jFil))=APA(IKF(iFil,jFil)) +       &
                                DBLE(A(iFil,iOBS)*A(jFil,iOBS))/P(iOBS)
          ENDDO
          APY(iFil)=APY(iFil)+DBLE(A(iFil,iOBS))*(Y(iOBS)/P(iOBS))
        ENDDO
      ENDDO
!
! Invert normal equation
! ----------------------
      CALL syminvg(nFil,APA,0,nSing,ParLst)
!
! A new singularity comes from the outier detection,
! another setup for the pseudo obs. becomes necessary
! (for concatenation only)
! ---------------------------------------------------
      IF (.NOT. CCopt%refFil .AND. CCopt%Combi==3 .AND. &
          mSing >= 0 .AND. nSing /= mSing) THEN
        mSing = -1
        numOBS=nOBS+1+nSing
        IF (numOBS > SIZE(Y)) CALL ccincr(A,P,Y,Idx,numOBS)
!
        DO jOBS=nOBS+1,nOBS+1+nSing
          A(:,jOBS) = 0
        ENDDO
        jOBS=nOBS+1
        DO iFil=1,nFil
          P(jOBS) = (CCopt%Sigma0*1D-3)**2
          IF (ParLst(iFil) /= 0) THEN
            jOBS=jOBS+1
          ELSE
            A(iFil,jOBS) = 1
          ENDIF
        ENDDO
        IF (jOBS-1 - nOBS == nFil) EXIT LSQLoop
        CYCLE LSQLoop
      ENDIF
!
      CALL solve (nFil,APA,APY,b)
!
! Compute the RMS value
! ---------------------
      RMS   = 0D0
      RMS_P = 0D0
!
      ALLOCATE(YAb(numOBS),stat=ios)
      CALL alcerr(ios,'YAb',(/numOBS/),'ccombo')
!
      YAb=0D0
      DO iOBS=1,numOBS
        DO iFil=1,nFil
          YAb(iOBS)=YAb(iOBS)+A(iFil,iOBS)*b(iFil)
        ENDDO
        YAb(iOBS)=Y(iOBS)-YAb(iOBS)
        RMS  =RMS  +Yab(iOBS)**2
        RMS_P=RMS_P+(Yab(iOBS)**2)/P(iOBS)
      ENDDO
!
      IF (numOBS-nFil+nSing > 0) THEN
        RMS_P=SQRT(RMS_P/(numOBS-nFil+nSing))
        RMS  =SQRT(RMS  /(numOBS-nFil+nSing))
      ELSE
        RMS_P=CCopt%Sigma0*1D-3
        RMS  =CCopt%Sigma0*1D-3
      ENDIF
!
! Check residuals for outliers, and remove them
! ---------------------------------------------
      ALLOCATE(A1(nFil,numOBS+nSing),stat=ios)
      CALL alcerr(ios,'A1',(/nFil,numOBS+nSing/),'ccombo')
!
      ALLOCATE(P1(numOBS+nSing),stat=ios)
      CALL alcerr(ios,'P1',(/nOBS+nSing/),'ccombo')
!
      ALLOCATE(Y1(numOBS+nSing),stat=ios)
      CALL alcerr(ios,'Y1',(/nOBS+nSing/),'ccombo')
!
      IF (nDel == 0) THEN

! Detect outliers:
! ---------------
        IF (CCopt%MaxResi /= 0d0) THEN
          DO iObs = 1,nObs
            IF (DABS(YAb(iOBS)*1D3) > CCopt%MaxResi) THEN
              DO jFil = 1,nFil
                IF (A(jFil,iObs) /= 0) THEN
                  IF (A(jFil,iObs) ==  1) Idx(iObs)%iFil(1) = jFil
                  IF (A(jFil,iObs) == -1) Idx(iObs)%iFil(2) = jFil
                ENDIF
              ENDDO

              DO jObs = 1,nObs
                IF (iObs == jObs) CYCLE
                IF (idx(iObs)%clkNam /= idx(jObs)%clkNam) CYCLE
                IF (idx(iObs)%iEpo   /= idx(jObs)%iEpo  ) CYCLE
                IF (DABS(YAb(jOBS)*1D3) <= CCopt%MaxResi) THEN
                  DO jFil = 1,nFil
                    IF (A(jFil,iObs) /= 0 .AND. A(jFil,jObs) /= 0) THEN
                      IF (A(jFil,iObs) ==  1) Idx(iObs)%iFil(1) = 0
                      IF (A(jFil,iObs) == -1) Idx(iObs)%iFil(2) = 0
                    ENDIF
                  ENDDO
                ENDIF
              ENDDO

            ENDIF
          ENDDO
        ENDIF

        jOBS=0
        DO iOBS=1,nOBS
          IF (Idx(iObs)%iFil(1) == 0 .AND. Idx(iObs)%iFil(2) == 0) THEN
            jOBS=jOBS+1
            A1(:,jOBS)=A(:,iOBS)
            P1(jOBS)=P(iOBS)
            Y1(jOBS)=Y(iOBS)
          ENDIF
        ENDDO
!
! Copy pseudo-observations
! ------------------------
        DO iOBS = nOBS+1,numOBS
          jOBS = jOBS + 1
          A1(:,jOBS) = A(:,iOBS)
          P1(jOBS)   = P(iOBS)
          Y1(jOBS)   = Y(iOBS)
        ENDDO
      ENDIF
!
      DEALLOCATE(A)
      DEALLOCATE(P)
      DEALLOCATE(Y)
      DEALLOCATE(YAb)
!
! Repeat if outliers found
! ------------------------
      IF (nDEL /= 0 .OR. jOBS == numOBS) THEN
        DEALLOCATE(A1)
        DEALLOCATE(P1)
        DEALLOCATE(Y1)
        EXIT LSQLoop
      ENDIF
!
      ALLOCATE(A(nFil,jOBS),stat=ios)
      CALL alcerr(ios,'A',(/nFil,jOBS/),'ccombo')
!
      ALLOCATE(P(jOBS),stat=ios)
      CALL alcerr(ios,'P',(/jOBS/),'ccombo')
!
      ALLOCATE(Y(jOBS),stat=ios)
      CALL alcerr(ios,'Y',(/jOBS/),'ccombo')
!
      A(:,1:jObs) = A1(:,1:jObs)
      P(1:jObs)   = P1(1:jObs)
      Y(1:jObs)   = Y1(1:jObs)
!
      DEALLOCATE(A1)
      DEALLOCATE(P1)
      DEALLOCATE(Y1)
!
      nDEL   = numOBS - jOBS
      nOBS   = nOBS - nDEL
      numOBS = jOBS
      mSing  = nSing
    ENDDO LSQLoop
!
! Optimize the outlier index
! --------------------------
    ALLOCATE(Idx1(2*nDel),stat=ios)
    CALL alcerr(ios,'Idx1',(/2*nDEL/),'ccombo')
!
    mDel=0
    IF (nDel > 0) THEN
      Idx1%iFil(1)=0
      Idx1%iFil(2)=0
      Idx1%iEpo=0
      Idx1%ClkNam=''
      Idx1%ClkTyp=0
      DO iOBS=1,nOBS+nDel
        DO jFil = 1,2
          IF (Idx(iOBS)%iFil(jFil) == 0) CYCLE
          DO iDel=1,mDel+1
            IF (iDel > mDel) THEN
              mDel=mDel+1
              Idx1(mDel)=Idx(iOBS)
              IF (jFil == 2) Idx1(mDel)%iFil(1) = Idx(iOBS)%iFil(jFil)
              EXIT
            ENDIF
            IF (Idx1(iDel)%clkNam  == Idx(iOBS)%clkNam .AND. &
                Idx1(iDel)%iEpo    == Idx(iOBS)%iEpo   .AND. &
                Idx1(iDel)%iFil(1) == Idx(iOBS)%iFil(jFil)) EXIT
          ENDDO
        ENDDO
      ENDDO
    ENDIF
    DEALLOCATE(Idx)
!
! Update summary statistic
! ------------------------
    CCopt%nInterval=CCopt%nInterval+1
    CCopt%SumRMS=CCopt%SumRMS+RMS*1D3
!
    IF (CCopt%MaxRMS<RMS*1D3) THEN
      CCopt%MaxRMS=RMS*1D3
      CCopt%MaxRMSEpo(1:2)=EpoStr(1:2)
    ENDIF
!
    IF (CCopt%MaxDEL<nDEL) THEN
      CCopt%MaxDEL=nDEL
      CCopt%MaxDelEpo(1:2)=EpoStr(1:2)
    ENDIF
!
! NO OBSERVATIONS FOUND
! ---------------------
  ELSE
    nSing=nFil
    ALLOCATE(ParLst(nFil),stat=ios)
    CALL alcerr(ios,'ParLst',(/nFil/),'ccombo')
    ParLst(:)=1
  ENDIF ! iOBS>0
!
! The normal equation is signular
! -------------------------------
  IF (nSing /= 0 .AND. numFil > 1) THEN
    WRITE(lfnerr,'(/,A,2(/,16X,2A))')                                          &
       ' ### SR CCOMBO: THE NORMAL EQUATION FOR OFFSET ESTIMATION IS SINGULAR',&
                       'INTERVAL START: ',EpoStr(1),                           &
                       'INTERVAL END:   ',EpoStr(2)
    DO iFil=1,nFil
      IF (ParLst(iFil)/=0)                                                     &
        WRITE(lfnerr,'(16X,2A)')                                               &
              'FILENAME:       ',TRIM(CCfil%ClkFilNam(iFil))
    ENDDO
    WRITE(lfnerr,'(16X,A,//)')                                                 &
          'PROBABLY NO COMMON CLOCKS FOUND FOR THIS EPOCH'
  ENDIF
!
! Take solution from an input file at least
! -----------------------------------------
  IF (nSing == nFil) THEN
    DO iEpo=EPO1, EPO2
      DO iFil=1,nFil
        IF (InClkRec(iFil)%iEpo == -1) CYCLE

        IF ((.NOT. CCopt%refFil .OR. iFil == 1)                   .AND. &
            filLst(iFil) /= 0                                     .AND. &
            DABS((OutClkHead%TFirst+OutClkRec%Epoch(iEpo)/86400D0)  -   &
                 (InClkHead(iFil)%TFirst+&
                  InClkRec(iFil)%Epoch(InClkRec(iFil)%iEpo)/86400D0)) * &
                                                           86400D0 < 1d0) THEN
          DO iClk=1,OutClkHead%nSta+OutClkHead%nSat
            DO jClk=1,InClkHead(iFil)%nSta+InClkHead(iFil)%nSat
              IF (OutClkHead%ClkName(iClk)   ==                    &
                             InClkHead(iFil)%ClkName(jClk) ) THEN
                OutClkRec%Clock(iClk,iEpo) =                   &
                             InClkRec(iFil)%Clock(jClk,InClkRec(iFil)%iEpo)
                OutClkRec%Sigma(iClk,iEpo) =                   &
                             InClkRec(iFil)%Sigma(jClk,InClkRec(iFil)%iEpo)
                OutClkRec%clkFlg(iClk,iEpo) =                  &
                             InClkRec(iFil)%clkFlg(jClk,InClkRec(iFil)%iEpo)
              ENDIF
            ENDDO
          ENDDO
!
! Check first/last epoch with data
! --------------------------------
          IF (CCopt%clkData%t(1) > &
              OutClkHead%TFirst+OutClkRec%Epoch(iEpo)/86400d0) THEN
            CCopt%clkData%t(1) = &
                  OutClkHead%TFirst+OutClkRec%Epoch(iEpo)/86400d0
          ENDIF
          IF (CCopt%clkData%t(2) < &
              OutClkHead%TFirst+OutClkRec%Epoch(iEpo)/86400d0) THEN
            CCopt%clkData%t(2) = &
                  OutClkHead%TFirst+OutClkRec%Epoch(iEpo)/86400d0
          ENDIF
        ENDIF
      ENDDO
    ENDDO
    irCode=irCode+1
!
! Write protocol for the result
! -----------------------------
  ELSE
    IF (CCopt%PrtDetail(prtcmb) == 1) THEN
      WRITE(lfnprt,'(//,1X,4A,/,1X,2A)')                                  &
           'RESULTS FOR THE INTERVAL from ',epostr(1),' to ',epostr(2),   &
           '---------------------------------------------------------',   &
           '---------------'

      WRITE(lfnprt,'(/,1X,A,F8.3,A,5(/,1X,A,I8))')                        &
                         'Rms:                        ',RMS*1D3,' ns',    &
                         'Number of stations:         ',nSTA,             &
                         'Number of satellites:       ',nSAT,             &
                         'Number of removed outliers: ',nDEL,             &
                         'Number of parameters:       ',nFil-nSing,       &
                         'Degree of freedom:          ',                  &
                                 (nSTA+nSAT-nDEL)-(nFil-nSing)+(numOBS-nOBS)
      WRITE(lfnprt,'(2(/,1X,A),A)')                                          &
            'Num  File name                           Offset      Rms    (ns) ', &
            '---------------------------------------------------------',    &
            '---------------'
      DO iFil=1,nFil
        IF (ParLst(iFil) == 0) THEN
          WRITE(lfnprt,'(1X,I3,2X,A32,2F10.3)')                             &
                       iFil,CCfil%ClkFilNam(iFil)(1:32),                    &
                       b(iFil)*1D3,                                         &
                       RMS_P*DSQRT(APA(IKF(iFil,iFil)))*1D3
        ELSE
          WRITE(lfnprt,'(1X,I3,2X,A32,A)')                                  &
                       iFil,CCfil%ClkFilNam(iFil)(1:32),                    &
                       '     singular  value'
        ENDIF
      ENDDO
      WRITE(lfnprt,'(1X,2A,/)')                                             &
           '---------------------------------------------------------',     &
           '---------------'
    ENDIF
!
! Combine the solutions
! ---------------------
    IF (.NOT. CCopt%refFil .OR. ParLst(1) == 0) THEN
      DO iEpo=EPO1, EPO2
!
! Update the epoch index in every data file
! -----------------------------------------
        DO iFil=1,nFil
          InClkRec(iFil)%iEpo=-1
          jEpoLoop1: DO jEpo=1,InClkRec(iFil)%nEpo
            IF (DABS((OutClkRec%Epoch(iEpo)/86400D0+OutClkHead%TFirst) -     &
                (InClkRec(iFil)%Epoch(jEpo)/86400D0+InClkHead(iFil)%TFirst)) &
                                                          * 86400d0 < 1d0) THEN
              InClkRec(iFil)%iEpo=jEpo
              EXIT jEpoLoop1
            ENDIF
            IF ((OutClkRec%Epoch(iEpo)+0.5)/86400D0+OutClkHead%TFirst <    &
                InClkRec(iFil)%Epoch(jEpo)/86400D0+InClkHead(iFil)%TFirst) &
              EXIT jEpoLoop1
          ENDDO jEpoLoop1
        ENDDO
!
! Loop the outrecord for all station clocks
! -----------------------------------------
        ClkLoop2: DO iClk=1,OutClkhead%nSta+OutClkhead%nSat
!
! Do not change values of the reference clocks
! --------------------------------------------
          DO jRef=1,OutClkHead%numRef
            DO iRef=1,OutClkHead%Ref(jRef)%nRef
              IF (CCopt%refFil                                            .AND. &
                  OutClkHead%ClkName(iClk) == &
                                       OutClkHead%Ref(jRef)%Clk(iRef)%name) THEN
                DO iFil=1,nFil
                  IF (DABS(OutClkRec%Epoch(iEpo) -                            &
                    InClkRec(iFil)%Epoch(InClkRec(iFil)%iEpo))*86400D0 < 1D0.AND.&
                    ((OutClkRec%Epoch(iEpo)>=OutClkHead%Ref(jRef)%refWin%t(1).AND.&
                      OutClkRec%Epoch(iEpo)<=OutClkHead%Ref(jRef)%refWin%t(2)).OR.&
                     OutClkHead%Ref(jRef)%refWin%t(1) + &
                                   OutClkHead%Ref(jRef)%refWin%t(2) == 0d0)) THEN
                    OutClkRec%Clock(iClk,iEpo)=                               &
                       InClkRec(1)%Clock(InClkHead(iFil)%Ref(jRef)%clk(iRef)%Idx,&
                                         InClkRec(iFil)%iEpo)
                    OutClkRec%Sigma(iClk,iEpo)=                               &
                       InClkRec(1)%Sigma(InClkHead(iFil)%Ref(jRef)%clk(iRef)%Idx,&
                                         InClkRec(iFil)%iEpo)
                    OutClkRec%clkFlg(iClk,iEpo)=                              &
                       InClkRec(1)%clkFlg(InClkHead(iFil)%Ref(jRef)%clk(iRef)%Idx,&
                                         InClkRec(iFil)%iEpo)
                    CYCLE ClkLoop2
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
          ENDDO
!
! Compute the mean of all occurancies of a satellite
! --------------------------------------------------
          nClock=0
          ALLOCATE(ClkMean(nFil),stat=ios)
          CALL alcerr(ios,'ClkMean',(/nFil/),'ccombo')
          DO ii = 0,7
            CALL clrflg(OutClkRec%clkFlg(iClk,iEpo),ii)
          ENDDO
          iFilLoop1: DO iFil=1,nFil
!
! Epoch not in file
! -----------------
            IF (InClkRec(iFil)%iEpo == -1) CYCLE iFilLoop1
!
! Offset computation was singular for this file
! ---------------------------------------------
            IF (ParLst(iFil) /= 0) THEN
              CYCLE iFilLoop1
            ENDIF
!
! Clock was detected as an outlier
! --------------------------------
            jFil=0
            IF (mDel > 0) THEN
              DO iDel=1,mDel
                IF (Idx1(iDel)%iEpo == iEpo .AND.    &
                  Idx1(iDel)%ClkNam == OutClkhead%ClkName(iClk)) THEN
                  jFil=jFil+1
                  jDel=iDel
                ENDIF
              ENDDO
            ENDIF
!
            IF (jFil /= 0) THEN
              IF ((jFil == nFil-nSing) .OR. &
                  (jFil == 1    .AND. Idx1(jDel)%iFil(1) == iFil)) THEN
                CCopt%NumDel=CCopt%NumDel+1
                CALL timst2(1,1,OutClkHead%TFirst+OutClkRec%Epoch(iEpo)/86400D0, &
                    SEpoch)
                IF (Idx1(jDel)%clkTyp == 1) THEN
                  WRITE(lfnerr,'(/,A)')                      &
                  ' ### SR CCOMBO: OUTLIER FOR A STATION CLOCK DETECTED'
                ELSE
                  WRITE(lfnerr,'(/,A)')                      &
                  ' ### SR CCOMBO: OUTLIER FOR A SATELLITE CLOCK DETECTED'
                ENDIF
                WRITE(lfnerr,'(16X,A,3(/,16X,2A),/)')               &
                     'IT WILL NOT BE INCLUDED INTO THE COMBINATION',&
                     'CLOCK NAME: ',Idx1(jDel)%ClkNam,              &
                     'FILE NAME : ',TRIM(CCfil%ClkFilNam(iFil)),    &
                     'EPOCH     : ',SEpoch
                CYCLE iFilLoop1
              ENDIF
            ENDIF
!
! Get the index for the station in the file
! -----------------------------------------
            jClkLoop: DO jClk=1,InCLkHead(iFil)%nSta+InCLkHead(iFil)%nSat
              IF (OutClkHead%ClkName(iClk)   ==                                 &
                             InClkHead(iFil)%ClkName(jClk)              .AND.   &
                  InClkRec(iFil)%Clock(jClk,InClkRec(iFil)%iEpo) /= unDef ) THEN
                nClock=nClock+1
                ClkMean(nClock)%iFil=iFil
                ClkMean(nClock)%iClk=iClk
                ClkMean(nClock)%iTyp=1
                IF (iClk>OutClkHead%nSta) ClkMean(nClock)%iTyp=2
                ClkMean(nClock)%Value=                                  &
                        InClkRec(iFil)%Clock(jClk,InClkRec(iFil)%iEpo)+b(iFil)
                ClkMean(nClock)%Sigma=                                  &
                        InClkRec(iFil)%Sigma(jClk,InClkRec(iFil)%iEpo)
                ClkMean(nClock)%SigOff=DSQRT(APA(IKF(iFil,iFil)))*RMS_P
                IF (CCopt%refFil .AND. iFil == 1) ClkMean(nClock)%SigOff=0D0
                DO ii = 0,7
                  IF (tstFlg(InClkRec(iFil)%clkFlg(jClk,InClkRec(iFil)%iEpo),ii)) &
                    CALL setflg(OutClkRec%clkFlg(iClk,iEpo),ii)
                ENDDO
              ENDIF
            ENDDO jClkLoop
          ENDDO iFilLoop1
          IF (nClock > 0) THEN
            CALL ccmean(CCopt,CCfil,InClkHead,OutClkHead,OutClkRec%Epoch(iEpo), &
                    nClock,ClkMean,NewClock,NewSigma,irCode)
!
! Put the results into the satellite clock array
! ----------------------------------------------
            OutClkRec%Clock(iClk,iEpo)=NewClock
            OutClkRec%Sigma(iClk,iEpo)=NewSigma
          ELSE
            OutClkRec%Clock(iClk,iEpo)=unDef
            OutClkRec%Sigma(iClk,iEpo)=unDef
          ENDIF
          DEALLOCATE(ClkMean)
        ENDDO ClkLoop2
      ENDDO
    ENDIF
  ENDIF
!
  DEALLOCATE(APA,   stat=iDummy)
  DEALLOCATE(APY,   stat=iDummy)
  DEALLOCATE(b,     stat=iDummy)
  DEALLOCATE(ParLst,stat=iDummy)
  IF ( ALLOCATED(Idx1) ) DEALLOCATE(Idx1  ,stat=iDummy)
  DEALLOCATE(filLst,stat=iDummy)
!
  RETURN
  END SUBROUTINE

END MODULE
