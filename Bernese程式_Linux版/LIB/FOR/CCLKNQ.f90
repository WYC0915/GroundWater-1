MODULE s_CCLKNQ
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE cclknq(EPO1, EPO2, CCopt,                          &
                    InClkHead, OutClkHead, InClkRec, OutClkRec, &
                    numFil, FilLst, nOBS, nSta, nSat, A,P,Y,Idx)

! -------------------------------------------------------------------------
! Purpose:    Searches common clocks in the record of the interval (Epo1,Epo2)
!
! Author:     R. Dach
!
! Created:    16-Feb-2001
!
! Changes:    18-Feb-2001 RD: More than one reference clock set
!             14-May-2001 RD: Improved structure od reference clock array
!             15-May-2001 RD: Also satellite may be reference clocks
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             25-Nov-2002 RD: More efficient epoch detection
!             25-Nov-2003 RD: Some POINTER->ALLOCATABLE
!             26-Nov-2003 HB: Check for sigma == unDef (999999.999999.D0)
!             21-Jan-2004 RD: Use m_bern, ONLY for modules
!             09-Feb-2004 RD: No more overwriting when an outlier in CCOMBO
!             18-Apr-2005 RD: Optimize finding common clocks in the inp files
!             19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b
  USE d_clkrnx, ONLY: t_clkhead,t_clkrec,unDef
  USE p_ccrnxc, ONLY: t_ccrnxc_opt,t_index,null
  USE s_alcerr
  USE s_ccincr
  IMPLICIT NONE
!
! Variables from parameter list
! -----------------------------
  INTEGER(i4b)                   :: Epo1,Epo2  ! Defines the window of this run
  TYPE(t_ccrnxc_opt)             :: CCopt      ! Input options for combination
  TYPE(t_clkhead),DIMENSION(:), POINTER :: InClkHead  ! header of input files
  TYPE(t_clkhead)                       :: OutClkHead ! header of output file
  TYPE(t_clkrec) ,DIMENSION(:), POINTER :: InClkRec   ! input data records
  TYPE(t_clkrec)                        :: OutClkRec  ! Combined data
  INTEGER(i4b)                   :: numFil     ! Number of files in FilLst
  INTEGER(i4b), DIMENSION(:)     :: FilLst     ! Index of the files with valid entries
  INTEGER(i4b)                   :: nOBS       ! Number of observations
  INTEGER(i4b)                   :: nSta       ! Number of station clocks in obs.
  INTEGER(i4b)                   :: nSat       ! Number of satellite clocks in obs.
  INTEGER(i4b), DIMENSION(:,:), POINTER    &
                                 :: A          ! Design matrix
  REAL(r8b)   , DIMENSION(:)  , POINTER    &
                                 :: P          ! Diagonal matrix with apriori weights
  REAL(r8b)   , DIMENSION(:)  , POINTER    &
                                 :: Y          ! Observation vector
  TYPE(t_index), DIMENSION(:) , POINTER    &
                                 :: Idx        ! Index for the observations
!
!******************************************************************************

! Local variables
! ---------------
  INTEGER(i4b),DIMENSION(:),ALLOCATABLE,SAVE :: jFil0
  INTEGER(i4b)                   :: iFil,jFil
  INTEGER(i4b)                   :: nFil
  INTEGER(i4b)                   :: clkTyp
  INTEGER(i4b)                   :: iEpo,jEpo
  INTEGER(i4b)                   :: iClock
  INTEGER(i4b)                   :: iClk,jClk
  INTEGER(i4b)                   :: iRef,jRef
  INTEGER(i4b)                   :: iObs
  INTEGER(i4b)                   :: ii, ii1, ii2
  INTEGER(i4b)                   :: ios

  LOGICAL                        :: isRef
  LOGICAL, SAVE                  :: first = .TRUE.


  iOBS=0
  nFil=SIZE(InCLkHead)

! Allocate file index in the first run
! ------------------------------------
  IF (first) THEN
    first = .FALSE.

    ALLOCATE(jFil0(nFil),stat=ios)
    CALL alcerr(ios,'jFil0',(/nFil/),'cclknq')

    jFil0 = 1
  ENDIF

! Allocate arrays for offset estimation
! -------------------------------------
  ALLOCATE(A(nFil,30),stat=ios)
  CALL alcerr(ios,'A',(/nFil,30/),'cclknq')

  ALLOCATE(P(30),stat=ios)
  CALL alcerr(ios,'P',(/30/),'cclknq')

  ALLOCATE(Y(30),stat=ios)
  CALL alcerr(ios,'Y',(/30/),'cclknq')

  ALLOCATE(Idx(30),stat=ios)
  CALL alcerr(ios,'Idx',(/30/),'cclknq')

  A(:,:) = 0
  P(:)   = 0D0
  Y(:)   = 0D0

  Idx(:)%iEpo   = 0
  Idx(:)%iFil(1)= 0
  Idx(:)%iFil(2)= 0
  Idx(:)%ClkNam = ''
  Idx(:)%ClkTyp = 0

  EpoLoop: DO iEpo=EPO1,EPO2

! Search the index for the same epochs in each data file
! ------------------------------------------------------
    DO iFil=1,nFil

      InClkRec(iFil)%iEpo=-1
      DO jEpo=jFil0(iFil),InClkRec(iFil)%nEpo

        IF (DABS((OutClkRec%Epoch(iEpo)/86400D0+OutClkHead%TFirst) -          &
                 (InClkRec(iFil)%Epoch(jEpo)/86400D0+InClkHead(iFil)%TFirst)) &
            < 1d0/86400d0) THEN
          InClkRec(iFil)%iEpo=jEpo
          IF (FilLst(iFil) == 0) numFil=numFil+1
          FilLst(iFil) = 1
          EXIT
        ENDIF

        IF ((OutClkRec%Epoch(iEpo)+0.5)/86400D0+OutClkHead%TFirst <    &
            InClkRec(iFil)%Epoch(jEpo)/86400D0+InClkHead(iFil)%TFirst) &
          EXIT

      ENDDO

      IF (jEpo > 1) jFil0(iFil)=jEpo-1

    ENDDO

! Loop all clocks
! ---------------
    DO iClock = 1,OutClkHead%nSta+OutClkHead%nSat

      IF (iClock <= OutClkHead%nSta) clkTyp = 1
      IF (iClock >  OutClkHead%nSta) clkTyp = 2

! Use only reference stations for combination
! -------------------------------------------
      IF (CCopt%onlyRef) THEN
        isRef=.FALSE.
        DO iFil = 1,nFil
          IF (InClkRec(iFil)%iEpo == -1) CYCLE

          DO jRef=1,InClkHead(iFil)%numRef

            DO iRef=1,InClkHead(iFil)%Ref(jRef)%nRef

              isRef=isRef .OR.                                                           &
                ((OutClkHead%ClkName(iClock)  ==                &
                      InClkHead(iFil)%Ref(jRef)%clk(iRef)%name) .AND.&
                 ((InClkHead(iFil)%ref(jRef)%refWin%T(1) + &
                      InClkHead(iFil)%ref(jRef)%refWin%T(2) == 0d0).OR.&
                  (InClkHead(iFil)%ref(jRef)%refWin%T(1) <= &
                      InClkRec(iFil)%Epoch(InClkRec(iFil)%iEpo) .AND.&
                   InClkHead(iFil)%ref(jRef)%refWin%T(2) >= &
                      InClkRec(iFil)%Epoch(InClkRec(iFil)%iEpo))))
            ENDDO
          ENDDO
          IF (isRef) EXIT
        ENDDO
      ENDIF

! Has the clock to be processed?
! ------------------------------
      IF (CCopt%onlyRef) THEN
        IF (.NOT. isRef) CYCLE

      ELSE IF (.NOT. CCopt%useSta .AND. clkTyp == 1) THEN
        CYCLE

      ELSE IF (.NOT. CCopt%useSat .AND. clkTyp == 2) THEN
        CYCLE

      ENDIF

! Find the first file with this epoch and clock
! ---------------------------------------------
      DO iFil=1,nFil-1
        IF (InClkRec(iFil)%iEpo == -1) CYCLE

        IF (clkTyp == 1) THEN
          ii1 = 1
          ii2 = InClkHead(iFil)%nSta
        ELSE
          ii1 = InClkHead(iFil)%nSta+1
          ii2 = InClkHead(iFil)%nSta+InClkHead(iFil)%nSat
        ENDIF

        iClk = 0
        DO ii = ii1,ii2
          IF (OutClkHead%clkName(iClock) == InClkHead(iFil)%clkName(ii)) &
            iClk = ii
        ENDDO

        IF (iCLk == 0) CYCLE
        IF (InClkRec(iFil)%Clock(iClk,InClkRec(iFil)%iEpo) == unDef) CYCLE

! Loop over all other files for this epoch and clock
! --------------------------------------------------
        DO jFil=iFil+1,nFil
          IF (InClkRec(jFil)%iEpo == -1) CYCLE

          IF (clkTyp == 1) THEN
            ii1 = 1
            ii2 = InClkHead(jFil)%nSta
          ELSE
            ii1 = InClkHead(jFil)%nSta+1
            ii2 = InClkHead(jFil)%nSta+InClkHead(jFil)%nSat
          ENDIF

          jClk = 0
          DO ii = ii1,ii2
            IF (OutClkHead%clkName(iClock) == InClkHead(jFil)%clkName(ii)) &
              jClk = ii
          ENDDO

          IF (jCLk == 0) CYCLE
          IF (InClkRec(jFil)%Clock(jClk,InClkRec(jFil)%iEpo) == unDef) CYCLE

          ! Increase matrices for parameter estimation
          ! ------------------------------------------
          iOBS=iOBS+1

          IF (iOBS > SIZE(Y)) CALL ccincr(A,P,Y,Idx,iOBS+9)

          ! Write values into the matrices
          ! ------------------------------
          A(iFil,iOBS) = -1
          A(jFil,iOBS) =  1

          IF (InClkRec(iFil)%Sigma(iClk,InClkRec(iFil)%iEpo) == unDef .OR. &
               InClkRec(jFil)%Sigma(jClk,InClkRec(jFil)%iEpo) == unDef ) THEN
            P(iOBS) = 0.D0
          ELSE
            P(iOBS) = InClkRec(iFil)%Sigma(iClk,InClkRec(iFil)%iEpo)**2+ &
                 InClkRec(jFil)%Sigma(jClk,InClkRec(jFil)%iEpo)**2
          ENDIF

          IF(DABS(P(iOBS))<null) P(iOBS)=(CCopt%Sigma0*1D-3)**2

          Y(iOBS) = InClkRec(iFil)%Clock(iClk,InClkRec(iFil)%iEpo)-    &
                    InClkRec(jFil)%Clock(jClk,InClkRec(jFil)%iEpo)

          Idx(iOBS)%iEpo   = iEpo
          Idx(iOBS)%ClkNam = OutClkHead%ClkName(iClock)
          Idx(iOBS)%ClkTyp = clkTyp

          IF (clkTyp == 1) nSta=nSta+1
          IF (clkTyp == 2) nSat=nSat+1

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

        ENDDO ! End of loop for all file

        EXIT  ! Loop to find the first valid file
      ENDDO

    ENDDO ! Next clock

  ENDDO EpoLoop

  nOBS=iOBS

  RETURN
END SUBROUTINE CCLKNQ

END MODULE
