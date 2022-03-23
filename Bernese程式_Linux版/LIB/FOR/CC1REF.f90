MODULE s_CC1REF
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE  CC1REF(CCopt, OutClkHead, OutClkRec, irCode)

! -------------------------------------------------------------------------
! Purpose:    Does an alignment for the new reference clocks of combined
!             clock rinex files, selects only one reference clock per
!             resulting file, but the best one
!
! Author:     R. Dach
!
! Created:    18-Jun-2001
!
! Changes:    21-Jun-2001 RD: Use an index for the best reference clock
!             03-Aug-2001 RD: Modified program output
!             27-Aug-2001 RD: Coorect format for error message
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             09-Jan-2001 RD: Bugfix for reference clock index
!             04-Jun-2002 RD: Make the PC version work
!             23-Apr-2003 CU: Nullify local pointers
!             08-Jul-2003 RD: Prevent array overflow in sort loop
!             22-Jul-2003 RD: Special handling for clocks with only epoch
!             12-Nov-2003 RD: Use SR syminvg instead of SR syming
!             25-Nov-2003 RD: Some POINTER->ALLOCATABLE
!             03-Dec-2003 HB: For refPoly ALLOCATABLE->POINTER
!             21-Jan-2004 RD: Adapt to new input panel
!             12-Apr-2004 HU: LF95 compiler bug bypassed
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             09-Aug-2010 RD: New syminvg used
!             27-Apr-2012 RD: Nullify all pointers, use m_bern with only
!             19-Sep-2012 RD: Unused variables removed
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnprt, lfnerr, lineLength
  USE d_clkrnx, ONLY: t_clkhead,t_clkrec
  USE p_ccrnxc, ONLY: t_ccrnxc_opt,prtref
  USE f_ikf
  USE s_alcerr
  USE s_timst2
  USE s_solve
  USE s_syminvg
  IMPLICIT NONE

! List of Parameters
! ------------------
! input
  TYPE(t_ccrnxc_opt)             :: CCopt      ! Input options for combination

! input/output
  TYPE(t_clkhead)                :: OutClkHead ! header of output file
  TYPE(t_clkrec)                 :: OutClkRec  ! Combined data

! output
  INTEGER(i4b)                   :: irCode     ! Return code from the sr

! Local Types
! -----------
  TYPE t_refLst
    INTEGER(i4b)               :: Idx        ! Index in clock list
    INTEGER(i4b)               :: numObs     ! Number of observations
    REAL(r8b)                  :: rms        ! RMS of the fit
    REAL(r8b)                  :: rms0       ! RMS for comparison
    INTEGER(i4b)               :: nClk       ! number of clocks for mean rms
    INTEGER(i4b), DIMENSION(5) :: Idx2       ! List of the best clock
    REAL(r8b),    DIMENSION(5) :: rms2       ! List of the best clock
    REAL(r8b),    DIMENSION(:), &
                  POINTER      :: refPoly    ! Polynom for ref-clock
  END TYPE

! Local Parameters
! ----------------

! Local Variables
! ---------------
  TYPE(t_refLst), DIMENSION(:), &
                  ALLOCATABLE  :: refLst ! Reference clock statitic

  CHARACTER(LEN=lineLength)    :: line   ! writing an output line
  CHARACTER(LEN=19)            :: epost1 ! epoch string
  CHARACTER(LEN=19)            :: epost2 ! epoch string

  INTEGER(i4b)                 :: iEpo   ! Counter for epochs
  INTEGER(i4b)                 :: iClk   ! Counter for clocks
  INTEGER(i4b)                 :: jClk   ! Counter for clocks
  INTEGER(i4b)                 :: iSta   ! Counter for stations
  INTEGER(i4b)                 :: iRef   ! Counter for reference records
  INTEGER(i4b)                 :: jRef   ! Counter for reference records
  INTEGER(i4b)                 :: kRef   ! Index for best reference clock
  INTEGER(i4b)                 :: iAlign ! Counter for alignment est.
  INTEGER(i4b)                 :: jAlign ! Counter for alignment est.
  INTEGER(i4b)                 :: nSing  ! Number of sing. elements
  INTEGER(i4b)                 :: numObs ! number of obs for ref clock check
  INTEGER(i4b)                 :: i1,i2  ! string index
  INTEGER(i4b)                 :: ios    ! IO status
  INTEGER(i4b)                 :: irc    ! IO status

  REAL(r8b)                    :: refVal ! Alignment model
  REAL(r8b)                    :: mRef   ! Alignment model
  REAL(r8b)                    :: rms    ! rms for ref. clock check
  REAL(r8b),      DIMENSION(:), &
                  ALLOCATABLE  :: ATA    ! Design matrix
  REAL(r8b),      DIMENSION(:), &
                  ALLOCATABLE  :: ATY    ! Vect. of obs.
  REAL(r8b),      DIMENSION(:), &
                  ALLOCATABLE  :: b       ! Solution vector
  REAL(r8b)                    :: DeltaT  ! Time term

  LOGICAL                      :: isOK   ! sort order OK?

! Init variables
! --------------
  irCode=0
!
!
! Prepare protocol file
! ---------------------
  WRITE(lfnprt,'(/,2(/,1X,A))')                           &
       'REFERENCE CLOCK SELECTION FOR OUTPUT FILE',       &
       '-----------------------------------------'
!
  IF (ASSOCIATED(OutClkHead%Ref)) THEN
    DO iRef=1,OutClkHead%numRef
      IF (ASSOCIATED(OutClkHead%Ref(iRef)%clk)) &
        DEALLOCATE(OutClkHead%Ref(iRef)%clk, stat=ios)
    ENDDO
    DEALLOCATE(OutClkHead%Ref, stat=ios)
  ENDIF
!
  ALLOCATE(OutClkHead%Ref(1),       stat=ios)
  CALL alcerr(ios,'OutClkHead%Ref',    (/1/),  'CC1REF')
  DO iRef=1,1
    ALLOCATE(OutClkHead%Ref(iRef)%clk(1),  stat=ios)
    CALL alcerr(ios,'OutClkHead%Ref(iRef)%clk', (/1/),'CC1REF')
  ENDDO
!
  ALLOCATE(refLst(CCopt%nRef+1), stat=ios)
  CALL alcerr(ios,'refLst',(/CCopt%nRef+1/),'CC1REF')
  DO iRef=1, CCopt%nRef+1
    NULLIFY(refLst(iRef)%refPoly)
    ALLOCATE(refLst(iRef)%refPoly(CCopt%nalig+1), stat=ios)
    CALL alcerr(ios, 'refLst(iRef)%refPoly',(/CCopt%nalig+1/),'CC1REF')
  ENDDO
!
  ALLOCATE(ATA(IKF(CCopt%nalig+1,CCopt%nalig+1)), stat=ios)
  CALL alcerr(ios,'ATA', (/IKF(CCopt%nalig+1,CCopt%nalig+1)/),'CC1REF')
  ALLOCATE(ATY(CCopt%nalig+1), stat=ios)
  CALL alcerr(ios,'ATY', (/CCopt%nalig+2/),'CC1REF')
  ALLOCATE(b(CCopt%nAlig+1),stat=ios)
  CALL alcerr(ios,'b',(/CCopt%nAlig+1/),'CC1REF')
!
  OutClkHead%numRef=0
!
! Loop all candicates for reference clocks
! ----------------------------------------
  iRef1Loop: DO iRef = 1,CCopt%nRef
!
! Find index for the reference clock candidate in the list of clocks
! ------------------------------------------------------------------
    refLst(iRef)%Idx = 0
    DO iSta = 1, OutClkHead%nSta+OutClkHead%nSat
      IF (CCopt%refClk(iRef) == OutClkHead%ClkName(iSta)) THEN
        refLst(iRef)%Idx = iSta
        EXIT
      ENDIF
    ENDDO
    IF (refLst(iRef)%Idx == 0) CYCLE iRef1Loop
!
! Loop all epochs to compute the RMS and the model
! ------------------------------------------------
    ATA=0d0
    ATY=0d0
    refLst(iRef)%numObs = 0
    refLst(iRef)%rms    = 0D0

    DO iEpo=1,OutClkRec%nEpo
      IF (OutClkRec%Clock(refLst(iRef)%Idx,iEpo) /= 999999.999999D0) THEN
        refLst(iRef)%numObs = refLst(iRef)%numObs + 1
!
! update ATA matrix
        DO iAlign=1,CCopt%nAlig+1
          DO jAlign=iAlign,CCopt%nAlig+1
            IF (iAlign+jAlign-2 > 0) THEN
              ATA(IKF(iAlign,jAlign)) = ATA(IKF(iAlign,jAlign)) + &
                               OutClkRec%Epoch(iEpo) ** (iAlign+jAlign-2)
            ELSE
              ATA(IKF(iAlign,jAlign)) = ATA(IKF(iAlign,jAlign)) + 1D0
            ENDIF
          ENDDO
        ENDDO
! update ATY vector
        iAlign=0
        DeltaT=1d0
        DO WHILE (iAlign <= CCopt%nAlig)
          iAlign=iAlign+1
          ATY(iAlign) = ATY(iAlign) + &
                        OutClkRec%Clock(refLst(iRef)%Idx,iEpo)*DeltaT
          DeltaT=DeltaT*OutClkRec%Epoch(iEpo)
        ENDDO
!
      ENDIF  ! valid clock found
    ENDDO    ! epoch loop
!
! Not enough clocks found for the requ. polynom
! ---------------------------------------------
    IF (refLst(iRef)%numObs <= CCopt%nAlig) THEN
      WRITE(lfnerr, '(/,A,/,16X,2A,2(/,16X,A,I6),/,16X,A,/)')           &
      ' ### SR CC1REF: ALIGNMENT OF THE REFERENCE CLOCK IS NOT POSSIBLE',&
           'SELECTED REFERENCE CLOCK        : ',                         &
                              TRIM(OutClkHead%ClkName(refLst(iRef)%Idx)),&
           'NUMBER OF CLOCKS FOUND          : ',refLst(iRef)%numObs,     &
           'REQU. DEG. OF ALIGNMENT POLYNOM : ',CCopt%nAlig,             &
           'TRY TO SELECT AN OTHER ONE.'
      refLst(iRef)%rms = HUGE(r8b)
!
! Invert normal equation
! ----------------------
    ELSE
      CALL syminvg(CCopt%nAlig+1,ATA,0,nSing)
      CALL solve (CCopt%nAlig+1,ATA,ATY,b)
      refLst(iRef)%refPoly = b
    ENDIF
!
! Compute the corresponding RMS
! -----------------------------
    IF (refLst(iRef)%rms == 0d0) THEN
      DO iEpo=1,OutClkRec%nEpo
        IF (OutClkRec%Clock(refLst(iRef)%Idx,iEpo) /= 999999.999999D0) THEN
          mRef=0d0
          iAlign=0
          DeltaT=1d0
          DO WHILE (iAlign <= CCopt%nAlig)
            iAlign=iAlign+1
            mRef = mRef + refLst(iRef)%refPoly(iAlign)*DeltaT
            DeltaT=DeltaT*OutClkRec%Epoch(iEpo)
          ENDDO
          refLst(iRef)%rms = refLst(iRef)%rms + &
                             (OutClkRec%Clock(refLst(iRef)%Idx,iEpo)-mRef)**2 /&
                             (refLst(iRef)%numObs-1)
        ENDIF
      ENDDO
      refLst(iRef)%rms=DSQRT(refLst(iRef)%rms)
    ENDIF

!
! Check reference clock with the best other clock
! -----------------------------------------------
    refLst(iRef)%nClk = 0
    refLst(iRef)%rms0 = 0d0
    refLst(iRef)%rms2 = HUGE(r8b)
    refLst(iRef)%Idx2 = 0

    iClkLoop: DO iClk = 1, OutClkHead%nSta+OutClkHead%nSat
      IF (refLst(iRef)%rms*1000d0 > CCopt%maxRef .AND. CCopt%maxRef /= 0d0) &
        EXIT iClkLoop
      IF (iClk == refLst(iRef)%Idx) CYCLE iClkLoop
!
! Loop all epochs to compute the RMS and the model
! ------------------------------------------------
      ATA=0d0
      ATY=0d0
      numObs = 0
      rms    = 0D0

      DO iEpo=1,OutClkRec%nEpo
        IF (OutClkRec%Clock(refLst(iRef)%Idx,iEpo) == 999999.999999d0) CYCLE
        IF (OutClkRec%Clock(iClk,iEpo)             == 999999.999999D0) CYCLE
        numObs = numObs + 1
!
! Compute the reference value
! ---------------------------
        refVal = 0d0
        iAlign = 0
        DeltaT = 1d0
        DO WHILE (iAlign <= CCopt%nAlig)
          iAlign=iAlign+1
          refVal = refVal + refLst(iRef)%refPoly(iAlign)*DeltaT
          DeltaT=DeltaT*OutClkRec%Epoch(iEpo)
        ENDDO
        refVal = refVal - OutClkRec%Clock(refLst(iRef)%Idx,iEpo)
!
! update ATA matrix
        DO iAlign=1,CCopt%nAlig+1
          DO jAlign=iAlign,CCopt%nAlig+1
            IF (iAlign+jAlign-2 > 0) THEN
              ATA(IKF(iAlign,jAlign)) = ATA(IKF(iAlign,jAlign)) + &
                               OutClkRec%Epoch(iEpo) ** (iAlign+jAlign-2)
            ELSE
              ATA(IKF(iAlign,jAlign)) = ATA(IKF(iAlign,jAlign)) + 1D0
            ENDIF
          ENDDO
        ENDDO
! update ATY vector
        iAlign=0
        DeltaT=1d0
        DO WHILE (iAlign <= CCopt%nAlig)
          iAlign=iAlign+1
          ATY(iAlign) = ATY(iAlign) + &
                        (OutClkRec%Clock(iClk,iEpo)+refVal)*DeltaT
          DeltaT=DeltaT*OutClkRec%Epoch(iEpo)
        ENDDO
!
      ENDDO    ! epoch loop
!
! Not enough clocks found for the requ. polynom
! ---------------------------------------------
      IF (numObs <= CCopt%nAlig .OR. numObs <= 1) CYCLE iClkLoop
!
! Invert normal equation
! ----------------------
      CALL syminvg(CCopt%nAlig+1,ATA,0,nSing)
      CALL solve (CCopt%nAlig+1,ATA,ATY,b)
!
! Compute the corresponding RMS
! -----------------------------
      DO iEpo=1,OutClkRec%nEpo
        IF (OutClkRec%Clock(refLst(iRef)%Idx,iEpo) == 999999.999999d0) CYCLE
        IF (OutClkRec%Clock(iClk,iEpo) == 999999.999999D0) CYCLE
!
! Compute the reference value
! ---------------------------
        refVal=0d0
        iAlign=0
        DeltaT=1d0
        DO WHILE (iAlign <= CCopt%nAlig)
          iAlign=iAlign+1
          refVal = refVal + refLst(iRef)%refPoly(iAlign)*DeltaT
          DeltaT=DeltaT*OutClkRec%Epoch(iEpo)
        ENDDO
        refVal = refVal - OutClkRec%Clock(refLst(iRef)%Idx,iEpo)
!
        mRef=-refVal
        iAlign=0
        DeltaT=1d0
        DO WHILE (iAlign <= CCopt%nAlig)
          iAlign=iAlign+1
          mRef = mRef + b(iAlign)*DeltaT
          DeltaT=DeltaT*OutClkRec%Epoch(iEpo)
        ENDDO
        rms = rms + (OutClkRec%Clock(iClk,iEpo)-mRef)**2 / (numObs-1)
      ENDDO
      DO jClk = 1,5
        IF (DSQRT(rms) < refLst(iRef)%rms2(jClk)) THEN
          IF (jClk < 5) THEN
            refLst(iRef)%rms2(jClk+1:5) = refLst(iRef)%rms2(jClk:4)
            refLst(iRef)%Idx2(jClk+1:5) = refLst(iRef)%Idx2(jClk:4)
          ENDIF
          refLst(iRef)%rms2(jClk) = DSQRT(rms)
          refLst(iRef)%Idx2(jClk) = iClk
          EXIT
        ENDIF
      ENDDO
!
      refLst(iRef)%rms0 = refLst(iRef)%rms0 + DSQRT(rms)
      refLst(iRef)%nClk = refLst(iRef)%nClk + 1

    ENDDO iClkLoop
!
! Compute mean rms for all clocks for comparison
! ----------------------------------------------
    IF (refLst(iRef)%nClk == 0) THEN
      refLst(iRef)%rms0 = HUGE(r8b)
    ELSE
      refLst(iRef)%rms0 = refLst(iRef)%rms0 / refLst(iRef)%nClk
    ENDIF
!
  ENDDO iRef1Loop
!
! Find out the best reference clock
! ---------------------------------
  isOK = .FALSE.
  DO WHILE (.NOT. isOK)
    isOK = .TRUE.
    iRef = 1
    jRef = 1
    DO WHILE (iRef < CCopt%nRef .AND. jRef <= CCopt%nRef)
      IF (refLst(iRef)%Idx == 0) THEN
        iRef = iRef + 1
        CYCLE
      ENDIF

      jRef = iRef + 1
      DO WHILE (jRef <= CCopt%nRef)
        IF (refLst(jRef)%Idx /= 0) EXIT
        jRef = jRef + 1
      ENDDO
      IF (jRef > CCopt%nRef) EXIT
      IF (                                                           &
! order 1: max. clock records
          (refLst(iRef)%numObs < refLst(jRef)%numObs    .AND. &
           ( (refLst(iRef)%rms*1000D0 <= CCopt%maxRef.AND. &
              refLst(jRef)%rms*1000D0 <= CCopt%maxRef) .OR. &
             CCopt%maxRef == 0                         )      ) .OR. &
! order 2: min rms for equal num. of clock records
          (refLst(iRef)%numObs == refLst(jRef)%numObs   .AND. &
           refLst(iRef)%rms0 > refLst(jRef)%rms0              ) .OR. &
! order 3: put "too big rms" at the end of the list
          (CCopt%maxRef /= 0                            .AND. &
           refLst(iRef)%rms*1000D0 > CCopt%maxRef       .AND. &
           refLst(jRef)%rms*1000D0 <= CCopt%maxRef            ) .OR. &
! order 4: min rms for all "too big" clocks
          (CCopt%maxRef /= 0d0                          .AND. &
           refLst(iRef)%rms > refLst(jRef)%rms          .AND. &
           refLst(iRef)%rms*1000D0 > CCopt%maxRef       .AND. &
           refLst(jRef)%rms*1000D0 > CCopt%maxRef             ) ) THEN
        isOK = .FALSE.
        refLst(SIZE(refLst)) = refLst(iRef)
        refLst(iRef)         = refLst(jRef)
        refLst(jRef)         = refLst(SIZE(refLst))
      ENDIF
      iRef = iRef + 1
    ENDDO
  ENDDO
!
! Write the results into the extended protocoll
! ---------------------------------------------
  IF (CCopt%PrtDetail(prtref) == 1) &
    WRITE(lfnprt,'(1X,A,2(/,1X,A,A),/,1X,A,A,A)')              &
    '                                             Mean  rms ', &
    '                          Num of   Alignm.    for all  ', &
    '     Fit of the best clocks',                             &
    'Num  Clock name            rec.    rms(ns)  clocks (ns)', &
    '     rms (ns)',                                           &
    '-------------------------------------------------------', &
    '-------------------------------------------------------', &
    '----------------------'

  jRef = 0
  kRef = 0
  DO iRef=1,CCopt%nRef
    IF (refLst(iRef)%Idx == 0) CYCLE
    IF (kRef == 0) kRef = iRef

    IF (CCopt%prtDetail(prtref) /= 1) CYCLE
    jRef = jRef + 1
    line = ''
    WRITE(line,'(I3,2X,A,4X,I6)')                            &
        jRef,outClkHead%clkName(refLst(iRef)%Idx)(1:16),refLst(iRef)%numObs
    IF (refLst(iRef)%rms == HUGE(r8b)) THEN
      WRITE(line(60:132),'(A)') '       (too few clock records)'

    ELSE
      i1 = 32
      i2 = 42
      IF (refLst(iRef)%rms >= 1D0) THEN
        WRITE(line(i1:i2),'(E11.3,A)') refLst(iRef)%rms*1000d0
      ELSE
        WRITE(line(i1:i2),'(F11.3,A)') refLst(iRef)%rms*1000d0
      ENDIF

      IF (CCOpt%maxRef /= 0d0 .AND.                       &
        refLst(iRef)%rms*1000d0 >= CCOpt%maxRef) THEN
        WRITE(line(60:132),'(A)') '(rms too big)'
      ELSE
        i1 = 43
        i2 = 53
        IF (refLst(iRef)%rms0 >= 1D0) THEN
          WRITE(line(i1:i2),'(E11.3,A)') refLst(iRef)%rms0*1000d0
        ELSE
          WRITE(line(i1:i2),'(F11.3,A)') refLst(iRef)%rms0*1000d0
        ENDIF

        DO jClk = 1,5
          IF (refLst(iRef)%Idx2(jClk) == 0) CYCLE
          i1 = 43 + jClk * 15
          i2 = 58 + jClk * 15
          IF (refLst(iRef)%rms2(jClk) >=1D0) THEN
            WRITE(line(i1:i2),'(E8.3,A)') refLst(iRef)%rms2(jClk)*1000d0,   &
                ' ('//OutClkHead%clkName(refLst(iRef)%Idx2(jClk))(1:4)//')'
          ELSE
            WRITE(line(i1:i2),'(F8.3,A)') refLst(iRef)%rms2(jClk)*1000d0,   &
                ' ('//OutClkHead%clkName(refLst(iRef)%Idx2(jClk))(1:4)//')'
          ENDIF
        ENDDO
      ENDIF
    ENDIF

    WRITE(lfnprt,'(1X,A)') TRIM(line)
  ENDDO

  IF (CCopt%prtDetail(prtref) == 1) &
    WRITE(lfnprt,'(1X,3A,/)')                                  &
    '-------------------------------------------------------', &
    '-------------------------------------------------------', &
    '----------------------'

!
! Best station has a too big rms
! ------------------------------
  IF (kRef == 0) THEN
    WRITE(lfnerr,'(/,A,2(/,16X,A),/)')                               &
    ' ### SR CC1REF: NO REFERENCE CLOCK FOUND IN THE FILE.',         &
                    'CHANGE THE LIST OF POSSIBLE REFERENCE CLOCKS',  &
                    'OR SELECT ANOTHER STRATEGY'
!
    WRITE(lfnprt,'(/,3X,A,/)') 'No suitable reference clock found'
    DO  iEpo=1,OutClkRec%nEpo
      DO iClk=1,OutClkHead%nSta+OutClkHead%nSat
        OutClkRec%Clock(iClk,iEpo)=999999.999999d0
      ENDDO
    ENDDO
    irCode=1
!
! Best station has a too big rms
! ------------------------------
  ELSE IF (refLst(kRef)%rms*1D3 > CCopt%maxRef .AND. CCopt%maxRef /= 0.0d0) THEN
    WRITE(lfnerr,'(/,A,/,16X,2A,/,16X,A,I8)')                        &
    ' ### SR CC1REF: BAD ALIGNMENT OF THE REFERENCE CLOCK DETECTED', &
                    'SELECTED REFERENCE CLOCK: ',                    &
                          TRIM(OutClkHead%ClkName(refLst(kRef)%Idx)),&
                    'NUMBER OF CLOCKS FOUND  : ',refLst(kRef)%numObs
    IF (refLst(kRef)%rms < 1D0) THEN
      WRITE(lfnerr,'(16X,A,F8.3,A)')                                 &
                    'RMS. OF ALIGNMENT       : ',refLst(kRef)%rms*1D3,' NSEC.'
    ELSE
      WRITE(lfnerr,'(16X,A,E8.3,A)')                                 &
                    'RMS. OF ALIGNMENT       : ',refLst(kRef)%rms*1D3,' NSEC.'
    ENDIF
    WRITE(lfnerr,'(16X,A,F8.3,A,2(/,16X,A),/)')                      &
                    'MAX. RMS. OF ALIGNMENT  : ',                    &
                                              CCopt%maxRef,' NSEC.', &
                    'CHANGE THE LIST OF POSSIBLE REFERENCE CLOCKS',  &
                    'OR SELECT ANOTHER STRATEGY'
!
    WRITE(lfnprt,'(/,3X,A,/)') 'NO SUITABLE REFERENCE CLOCK FOUND'
    DO  iEpo=1,OutClkRec%nEpo
      DO iClk=1,OutClkHead%nSta+OutClkHead%nSat
        OutClkRec%Clock(iClk,iEpo)=999999.999999d0
      ENDDO
    ENDDO
    irCode=1
  ELSE
!
! Set the new reference clock
! ---------------------------
    OutClkHead%numRef      = 1
    OutClkHead%Ref(1)%nRef = 1
    OutClkHead%Ref(1)%refWin%t(1)  = OutClkHead%TFirst
    OutClkHead%Ref(1)%refWin%t(2)  = OutClkHead%TFirst + &
                                     OutClkRec%Epoch(OutClkRec%nEpo)/86400d0
    OutClkHead%Ref(1)%clk(1)%name  = OutClkHead%ClkName(refLst(kRef)%Idx)
    OutClkHead%Ref(1)%clk(1)%Idx   = refLst(kRef)%Idx
    OutClkHead%Ref(1)%clk(1)%sigma = 0d0
!
! Write the protocol for the result
! ---------------------------------
    CALL timst2(1,1,OutClkHead%Ref(1)%refWin%t(1), epost1)
    CALL timst2(1,1,OutClkHead%Ref(1)%refWin%t(2), epost2)
!
    WRITE(lfnprt,'(/,2A,/,4A,/)')                                  &
    ' Selected reference station:     ',                           &
                     OutClkHead%Ref(OutClkHead%numRef)%clk(1)%name,&
    ' Valid for interval              ',epost1,' to ',epost2

!
! Apply the alignment results
! ---------------------------
    DO iEpo=1,OutClkRec%nEpo
      IF (OutClkRec%Clock(OutClkHead%Ref(1)%clk(1)%Idx,iEpo) /= 999999.999999d0) THEN
        mRef=0d0
        iAlign=0
        DeltaT=1d0
        DO WHILE (iAlign <= CCopt%nAlig)
          iAlign=iAlign+1
          mRef = mRef + refLst(kRef)%refPoly(iAlign)*DeltaT
          DeltaT=DeltaT*OutClkRec%Epoch(iEpo)
        ENDDO
        mRef = mRef - OutClkRec%Clock(OutClkHead%Ref(1)%clk(1)%Idx,iEpo)
!
        DO iClk=1,OutClkHead%nSta+OutClkHead%nSat
          IF (OutClkRec%Clock(iClk,iEpo) /= 999999.999999D0) THEN
            OutClkRec%Clock(iClk,iEpo) = OutClkRec%Clock(iClk,iEpo) + mRef
          ENDIF
        ENDDO
      ELSE
!
! No reference for this epoch
! ---------------------------
        DO iClk=1,OutClkHead%nSta+OutClkHead%nSat
          OutClkRec%Clock(iClk,iEpo)=999999.999999d0
        ENDDO
      ENDIF
    ENDDO

  ENDIF

!  DO iRef=1, CCopt%nRef+1  ! is correct line, but not working with LF95 !!!
  DO iRef=1, CCopt%nRef
    DEALLOCATE(refLst(iRef)%refPoly, stat=irc)
  ENDDO
  DEALLOCATE(refLst,stat=irc)
  DEALLOCATE(ATA,stat=irc)
  DEALLOCATE(ATY,stat=irc)
  DEALLOCATE(b,stat=irc)

  RETURN
  END SUBROUTINE

END MODULE
