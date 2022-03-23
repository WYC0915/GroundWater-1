MODULE s_CCALIG
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE  ccalig(CCopt, OutClkHead, OutClkRec, irCode)

! -------------------------------------------------------------------------
! Purpose:    Does an alignment for the new reference clocks of combined
!             clock rinex files
!
!
! Author:     R. Dach
!
! Created:    18-Feb-2001
!
! Changes:    14-May-2001 RD: Improved structure od reference clock array
!             22-May-2001 RD: Better handling if no ref. clock found
!             03-Aug-2001 RD: Modified program output
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             04-Jun-2002 RD: Make the PC version work
!             29-Oct-2002 MR: Correct format (1x)
!             23-Apr-2003 CU: Nullify local pointers
!             12-Nov-2003 RD: Use SR syminvg instead of SR syming
!             25-Nov-2003 RD: Some POINTER->ALLOCATABLE
!             21-Jan-2004 RD: Adapt to new input panel
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             09-Aug-2010 RD: New syminvg used
!             19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnprt, lfnerr
  USE d_clkrnx, ONLY: t_clkhead,t_clkrec,t_ref
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

! Local variables
! ---------------
  TYPE(t_ref),      DIMENSION(:), &
                    ALLOCATABLE  :: RefHlp ! Help for extracting

  CHARACTER(LEN=19)              :: epost1 ! epoch string
  CHARACTER(LEN=19)              :: epost2 ! epoch string

  INTEGER(i4b)                   :: iEpo   ! Counter for epochs
  INTEGER(i4b)                   :: jEpo   ! Counter for epochs
  INTEGER(i4b)                   :: iClk   ! Counter for clocks
  INTEGER(i4b)                   :: jClk   ! Counter for clocks
  INTEGER(i4b)                   :: iSta   ! Counter for stations
  INTEGER(i4b)                   :: jSta   ! Counter for stations
  INTEGER(i4b)                   :: iRef   ! Counter for reference records
  INTEGER(i4b)                   :: jRef   ! Index of a new reference clock
  INTEGER(i4b)                   :: kRef   ! Index of a new reference clock
  INTEGER(i4b)                   :: nRef   ! Number of epochs for alignment
  INTEGER(i4b)                   :: iAlign ! Counter for alignment est.
  INTEGER(i4b)                   :: jAlign ! Counter for alignment est.
  INTEGER(i4b)                   :: nSing  ! Number of sing. elements
  INTEGER(i4b),    DIMENSION(:),  &
                   ALLOCATABLE   :: ParLst ! List of singular parameters
  INTEGER(i4b)                   :: ios    ! IO status

  REAL(r8b)                      :: mRef   ! Alignment model
  REAL(r8b)                      :: sRef   ! RMS for comp. the align. model
  REAL(r8b),       DIMENSION(:,:),&
                   ALLOCATABLE   :: aPoly  ! Alignment polynoms
  REAL(r8b),       DIMENSION(:,:),&
                   ALLOCATABLE   :: hPoly  ! Alignment polynoms
  REAL(r8b),       DIMENSION(:),  &
                   ALLOCATABLE   :: ATA    ! Design matrix
  REAL(r8b),       DIMENSION(:),  &
                   ALLOCATABLE   :: ATY    ! Vect. of obs.
  REAL(r8b),       DIMENSION(:),  &
                   ALLOCATABLE   :: b      ! Solution vector
  REAL(r8b)                      :: DeltaT ! Time term

  LOGICAL           :: isClock ! a clock in the epoch found?
!
  irCode=0
!
! Prepare protocol file
! ---------------------
  WRITE(lfnprt,'(/,2(/,1X,A))')                          &
       'REFERENCE CLOCK SELECTION FOR OUTPUT FILE',      &
       '-----------------------------------------'
!
  DO iRef=1,OutClkHead%numRef
    DEALLOCATE(OutClkHead%Ref(iRef)%clk, stat=ios)
  ENDDO
  DEALLOCATE(OutClkHead%Ref, stat=ios)
!
  ALLOCATE(OutClkHead%Ref(1),       stat=ios)
  CALL alcerr(ios,'OutClkHead%Ref',    (/1/),  'ccalig')
  DO iRef=1,1
    ALLOCATE(OutClkHead%Ref(iRef)%clk(1),  stat=ios)
    CALL alcerr(ios,'OutClkHead%Ref(iRef)%clk', (/1/),'ccalig')
  ENDDO
!
  ALLOCATE(aPoly(1,CCopt%nalig+1), stat=ios)
  CALL alcerr(ios,'aPoly',(/1,CCopt%nalig+1/),'ccalig')
!
  ALLOCATE(ATA(IKF(CCopt%nalig+1,CCopt%nalig+1)), stat=ios)
  CALL alcerr(ios,'ATA', (/IKF(CCopt%nalig+1,CCopt%nalig+1)/),'ccalig')
  ALLOCATE(ATY(CCopt%nalig+1), stat=ios)
  CALL alcerr(ios,'ATY', (/CCopt%nalig+2/),'ccalig')
  ALLOCATE(b(CCopt%nAlig+1),stat=ios)
  CALL alcerr(ios,'b',(/CCopt%nAlig+1/),'ccalig')
!
  OutClkHead%numRef=0
!
! Loop all epochs of the output file to set the new reference clock
! -----------------------------------------------------------------
  iEpo=1
  DO WHILE (iEpo <= OutClkRec%nEpo)
    jRef=0
    kRef=0
!
! Find a valid reference clock for the next epoch(s)
! --------------------------------------------------
    iSta=1
    DO WHILE (kRef == 0 .AND. iSta <= CCOpt%nRef)
      DO jSta=1,OutClkHead%nSta
        IF (CCopt%refClk(iSta) == OutClkHead%ClkName(jSta) .AND. &
            OutClkRec%Clock(jSta,iEpo) /= 999999.999999D0) kRef=jSta
      ENDDO
!
! Search reference clock in list
! ------------------------------
      jRef = 0
      IF (kRef /= 0) THEN
        DO iRef=1,OutClkHead%numRef
          IF (OutClkHead%ClkName(kRef) == OutClkHead%ref(iRef)%clk(1)%name) THEN
             jRef=iRef
          ENDIF
        ENDDO
      ENDIF
!
! Compute a mean offset for this reference clock
! ----------------------------------------------
      IF (jRef == 0) THEN
        nRef=0
        sRef=0d0
        ATA=0d0
        ATY=0d0
!
        IF (kRef /= 0) THEN
          DO jEpo=1,OutClkRec%nEpo
            IF (OutClkRec%Clock(kRef,jEpo) /= 999999.999999D0) THEN
              nRef=nRef+1
! update ATA matrix
              DO iAlign=1,CCopt%nAlig+1
                DO jAlign=iAlign,CCopt%nAlig+1
                  IF (iAlign+jAlign-2 > 0) THEN
                    ATA(IKF(iAlign,jAlign)) = ATA(IKF(iAlign,jAlign)) + &
                               OutClkRec%Epoch(jEpo) ** (iAlign+jAlign-2)
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
                ATY(iAlign) = ATY(iAlign) + OutClkRec%Clock(kRef,jEpo)*DeltaT
                DeltaT=DeltaT*OutClkRec%Epoch(jEpo)
              ENDDO
!
            ENDIF  ! valid clock found
          ENDDO    ! epoch loop
!
! Not enough clocks found for the requ. polynom
! ---------------------------------------------
          IF (nRef <= CCopt%nAlig) THEN
            WRITE(lfnerr, '(/,2A,/,16X,2A,2(/,16X,A,I6),/,16X,A,/)')           &
            ' ### SR CCALIG: ALIGNMENT OF THE REFERENCE CLOCK ',               &
                            'IS NOT POSSIBLE',                                 &
                            'SELECTED REFERENCE CLOCK        : ',              &
                                            TRIM(OutClkHead%ClkName(kRef)),    &
                            'NUMBER OF CLOCKS FOUND          : ',nRef,         &
                            'REQU. DEG. OF ALIGNMENT POLYNOM : ',CCopt%nAlig, &
                            'TRY TO SELECT AN OTHER ONE.'
            kRef=0
!
! Invert normal equation
! ----------------------
          ELSE
            ALLOCATE(ParLst(CCopt%nAlig+1),stat=ios)
            CALL alcerr(ios,'ParLst',(/CCopt%nAlig+1/),'ccalig')

            CALL syminvg(CCopt%nAlig+1,ATA,0,nSing,ParLst)

            DEALLOCATE(ParLst)
!
            CALL solve (CCopt%nAlig+1,ATA,ATY,b)
          ENDIF
!
! Compute the corresponding RMS
! -----------------------------
          IF (nRef > 1) THEN
            DO jEpo=1,OutClkRec%nEpo
              IF (OutClkRec%Clock(kRef,jEpo) /= 999999.999999D0) THEN
                mRef=0d0
                iAlign=0
                DeltaT=1d0
                DO WHILE (iAlign <= CCopt%nAlig)
                  iAlign=iAlign+1
                  mRef = mRef + b(iAlign)*DeltaT
                  DeltaT=DeltaT*OutClkRec%Epoch(jEpo)
                ENDDO
                sRef = sRef + (OutClkRec%Clock(kRef,jEpo)-mRef)**2/(nRef-1)
              ENDIF
            ENDDO
            sRef=DSQRT(sRef)
            IF (sRef*1D3 > CCopt%maxRef .AND. CCopt%maxRef /= 0.0d0) THEN
              WRITE(lfnerr,'(/,A,/,16X,2A,/,16X,A,I8)')                        &
              ' ### SR CCALIG: BAD ALIGNMENT OF THE REFERENCE CLOCK DETECTED', &
                              'SELECTED REFERENCE CLOCK: ',                    &
                                              TRIM(OutClkHead%ClkName(kRef)),  &
                              'NUMBER OF CLOCKS FOUND  : ',nRef
              IF (sRef < 1D0) THEN
                WRITE(lfnerr,'(16X,A,F8.3,A)')                                 &
                              'RMS. OF ALIGNMENT       : ',sRef*1D3,' NSEC.'
              ELSE
                WRITE(lfnerr,'(16X,A,E8.3,A)')                                 &
                              'RMS. OF ALIGNMENT       : ',sRef*1D3,' NSEC.'
              ENDIF
              WRITE(lfnerr,'(16X,A,F8.3,A,/,16X,A,/)')                         &
                              'MAX. RMS. OF ALIGNMENT  : ',                    &
                                                        CCopt%maxRef,' NSEC.', &
                              'TRY TO SELECT AN OTHER ONE.'
              kRef=0
            ENDIF
          ENDIF
        ENDIF
      ENDIF
!
! Setup the record for this new reference clock
! ---------------------------------------------
      IF (kRef /= 0) THEN
!
! Extract the reference array if necessary
! ----------------------------------------
        IF (OutClkHead%numRef == SIZE(OutClkHead%Ref)) THEN

          ALLOCATE(RefHlp(OutClkHead%numRef),stat=ios)
          CALL alcerr(ios,'RefHlp',(/OutClkHead%numRef/),'ccalig')
!
          ALLOCATE(hPoly(OutClkHead%numRef,CCopt%nAlig+1),stat=ios)
          CALL alcerr(ios,'hPoly',                   &
                             (/OutClkHead%numRef,CCopt%nAlig+1/),'ccalig')
!
          RefHlp = OutClkHead%Ref
          hPoly  = aPoly
!
          DEALLOCATE(OutClkHead%Ref, stat=ios)
          DEALLOCATE(aPoly,          stat=ios)
!
          ALLOCATE(OutClkHead%Ref(OutClkHead%numRef+3),stat=ios)
          CALL alcerr(ios,'OutClkHead%Ref',(/OutClkHead%numRef+3/),'ccalig')
          DO jClk=1,3
            ALLOCATE(OutClkHead%Ref(OutClkHead%numRef+jClk)%clk(1),stat=ios)
            CALL alcerr(ios,'OutClkHead%Ref(:)%clk',(/1/),'ccalig')
          ENDDO
!
          ALLOCATE(aPoly(OutClkHead%numRef+3,CCopt%nAlig+1), stat=ios)
          CALL alcerr(ios,'aPoly',                   &
                            (/OutClkHead%numRef+3,CCopt%nAlig+1/),'ccalig')
!
          OutClkHead%Ref(1:OutClkHead%numRef) = RefHlp(:)
          aPoly(1:OutClkHead%numRef,:) = hPoly(:,:)
!
          DEALLOCATE(RefHlp,stat=ios)
          DEALLOCATE(hPoly,     stat=ios)
        ENDIF
!
        OutClkHead%numRef = OutClkHead%numRef + 1
        OutClkHead%Ref(OutClkHead%numRef)%nRef = 1
        OutClkHead%Ref(OutClkHead%numRef)%clk(1)%name = OutClkHead%ClkName(kRef)
        OutClkHead%Ref(OutClkHead%numRef)%clk(1)%Idx = kRef
        OutClkHead%Ref(OutClkHead%numRef)%clk(1)%sigma = 0d0
        aPoly(OutClkHead%numRef,:) = b(:)
!
! Check validity of the refernce clock
! ------------------------------------
        jEpo=iEpo
        isCLock=.TRUE.
        DO WHILE (jEpo <= OutClkRec%nEpo .AND. isClock)
          isClock = (OutClkRec%Clock(kRef,jEpo) /= 999999.999999d0)
          IF (.NOT. isClock) THEN
            isClock=.TRUE.
            DO iSta=1,OutClkHead%nSta+OutClkHead%nSat
              isClock=isClock .AND. &
                      (OutClkRec%Clock(iSta,jEpo) == 999999.999999d0)
            ENDDO
          ENDIF
          IF (isClock) jEpo=jEpo+1
        ENDDO
        OutClkHead%Ref(OutClkHead%numRef)%refWin%t(1) = OutClkRec%Epoch(iEpo)
        OutClkHead%Ref(OutClkHead%numRef)%refWin%t(2) = OutClkRec%Epoch(jEpo-1)
!
! Write the protocol for the result
! ---------------------------------
        CALL timst2(1,1,OutClkHead%TFirst+OutClkRec%Epoch(iEpo)/86400, epost1)
        CALL timst2(1,1,OutClkHead%TFirst+OutClkRec%Epoch(jEpo-1)/86400, epost2)
!
        WRITE(lfnprt,'(/,1X,2A,/,1X,4A)')                                   &
             'Selected reference station:     ',                            &
                             OutClkHead%Ref(OutClkHead%numRef)%clk(1)%name, &
             'Valid for interval              ',epost1,' to ',epost2
!
        IF (CCopt%PrtDetail(prtref) == 1 .AND. jRef == 0) THEN
          IF (sRef < 1D0) THEN
            WRITE(lfnprt,'(/,1X,A,F8.3,A)')                            &
              'Rms of the clock alignment:     ',sRef*1D3,' ns'
          ELSE
            WRITE(lfnprt,'(/,1X,A,E8.3,A)')                            &
              'Rms of the clock alignment:     ',sRef*1D3,' ns'
          ENDIF
          WRITE(lfnprt,'(1X,A,I8)')                                    &
            'Number of epoch contributed:    ',nRef
        ENDIF
        WRITE(lfnprt,*)
!
! Update epoch
! ------------
        iEpo=jEpo-1
      ENDIF
      iSta=iSta+1
    ENDDO ! Loop reference stations
!
! No reference clock found
! ------------------------
    IF (kRef == 0) THEN
!
! Is there a valid clock for this epoch?
! --------------------------------------
      jClk=0
      DO iClk=1,OutClkHead%nSta+OutClkHead%nSat
        IF (OutClkRec%Clock(iClk,iEpo) /= 999999.999999D0) jClk=iClk
      ENDDO
!
      IF (jClk /= 0) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,A)')                                &
              ' ### SR CCALIG: EPOCHS WITH NO REFERENCE CLOCK FOUND',  &
                              'NO CLOCKS WILL BE WRITTEN FOR ',        &
                              'THE FOLLOWING EPOCH:'
        CALL timst2(1,1,OutClkHead%TFirst+OutClkRec%Epoch(iEpo)/86400d0, epost1)
        WRITE(lfnerr,'(20X,A)') epost1
!
        irCode=1
      ENDIF
    ENDIF
!
    iEpo=iEpo+1
  ENDDO ! loop all epochs
!
  DEALLOCATE(ATA, stat=ios)
  DEALLOCATE(ATY, stat=ios)
  DEALLOCATE(b,   stat=ios)
!
! Apply the alignment results
! ---------------------------
  IF (OutClkHead%numRef > 0) THEN
    iRef=1
    DO iEpo=1,OutClkRec%nEpo
      DO WHILE (iRef <= OutClkHead%numRef  .AND. &
                OutClkRec%Epoch(iEpo)>OutClkHead%ref(iRef)%refWin%t(2))
        iRef=iRef+1
      ENDDO
!
      IF (iRef <= OutClkHead%numRef  .AND. &
          OutClkRec%Epoch(iEpo) >= OutClkHead%ref(iRef)%refWin%t(1)) THEN
        mRef=0d0
        iAlign=0
        DeltaT=1d0
        DO WHILE (iAlign <= CCopt%nAlig)
          iAlign=iAlign+1
          mRef = mRef + aPoly(iRef,iAlign)*DeltaT
          DeltaT=DeltaT*OutClkRec%Epoch(iEpo)
        ENDDO
        mRef = mRef - OutClkRec%Clock(OutClkHead%Ref(iRef)%clk(1)%Idx,iEpo)
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
  ELSE
    WRITE(lfnerr,'(/,A,2(/,16X,A),/)')                                     &
          ' *** SR CCALIG: NO SUITABLE REFERENCE CLOCK FOUND',             &
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
  ENDIF
!
  DEALLOCATE(aPoly, stat=ios)
!
  RETURN
  END SUBROUTINE

END MODULE
