MODULE s_CCJUMP
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE  ccjump(CCopt, OutClkHead, OutClkRec, nJump, clkJump, irCode)

! -------------------------------------------------------------------------
! Purpose:    Jump detection of combined clock rinex files
!
! Author:     R. Dach
!
! Created:    18-Feb-2001
!
! Changes:    02-May-2001 RD: use the DABS of DeltaT
!             05-Jun-2001 RD: at least 5 obs for jump detection
!             03-Aug-2001 RD: modified program output
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             29-Oct-2002 MR: Correct format (2x)
!             23-Apr-2003 CU: Nullify local pointers
!             25-Nov-2003 RD: Some POINTER->ALLOCATABLE
!             26-Nov-2003 HB: Initialize Sigma to 999999.999999D0 (unDef)
!             21-Jan-2004 RD: Adapt to new input panel
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnprt, lfnerr, lineLength
  USE d_clkrnx, ONLY: t_clkhead,t_clkrec,unDef
  USE p_ccrnxc, ONLY: t_ccrnxc_opt,t_Jump,prtjmp
  USE s_alcerr
  USE s_ccdelt
  USE s_timst2
  IMPLICIT NONE
!
! Variables from parameter list
! -----------------------------
  TYPE(t_ccrnxc_opt)             :: CCopt      ! Input options for combination
  TYPE(t_clkhead)                :: OutClkHead ! header of output file
  TYPE(t_clkrec)                 :: OutClkRec  ! Combined data
  INTEGER(i4b)                   :: nJump      ! Number of clock jumps
  TYPE(t_Jump), DIMENSION(:), POINTER          &
                                 :: ClkJump    ! Detected clock jumps
  INTEGER(i4b)                   :: irCode     ! Return code from the sr
!
!******************************************************************************
!
! Local Variables
! ---------------
  TYPE(t_Jump), DIMENSION(:), POINTER          &
                                 :: HlpJump    ! Detected clock jumps
  TYPE(t_Jump)                   :: sortJmp    ! for sorting the list
!
  CHARACTER(LEN=LineLength)      :: line       ! Protocol line
  CHARACTER(LEN=19) :: epost1  ! Epoch string
  CHARACTER(LEN=19) :: epost2  ! Epoch string
!
  INTEGER(i4b)      :: tooBig  ! Number of clocks with only jumps
  INTEGER(i4b)      :: iEpo    ! Counter for epochs
  INTEGER(i4b)      :: jEpo    ! Counter for epochs
  INTEGER(i4b)      :: kEpo    ! Counter for epochs
  INTEGER(i4b)      :: iJmp    ! Counter for clock jump list
  INTEGER(i4b)      :: jJmp    ! Counter for clock jump list
  INTEGER(i4b)      :: mJmp    ! Total number of jumps detected
  INTEGER(i4b)      :: nJmp    ! Number of jumps detected
  INTEGER(i4b)      :: iJump   ! Index for clock jump list
  INTEGER(i4b)      :: iClk    ! Counter for clocks
  INTEGER(i4b)      :: jClk    ! Counter for clocks
  INTEGER(i4b)      :: kClk    ! Counter for clocks
  INTEGER(i4b)      :: ios     ! IO status
  INTEGER(i4b)      :: ii
!
  LOGICAL           :: isJump  ! Jump allready in list
  LOGICAL, DIMENSION(:), &
           ALLOCATABLE :: delJump ! Flag for invalid jumps
!
  INTEGER(i4b)      :: nDeltaT ! Number of clk. diff. to detect jumps
  INTEGER(i4b)      :: nDeltT1 ! Number of clk. diff. to detect jumps
  REAL(r8b)         :: mDeltaT ! mean clk.-diff to detect jumps
  REAL(r8b)         :: mDeltT1 ! mean clk.-diff to detect jumps
  REAL(r8b)         :: sDeltaT ! s-dev. clk.-diff to detect jumps
  REAL(r8b)         :: sDeltT1 ! s-dev. clk.-diff to detect jumps
!
  irCode = 0
!
  NULLIFY(HlpJump)
!
! No jump detection active
! ------------------------
  IF (CCopt%nJmpSig == 0 .OR. outClkRec%nEpo < 5) THEN
    nJump=0
    ALLOCATE(ClkJump(1),stat=ios)
    CALL alcerr(ios,'ClkJump',(/1/),'ccjump')
    RETURN
  ENDIF
!==============================================================================
!
!  PART 1: Search all Possible Jumps
!
!==============================================================================
!
! Write the detailed protocol
! ---------------------------
  IF (CCopt%prtDetail(prtjmp) == 1)                                           &
    WRITE(lfnprt,'(/2(/,1X,A),4(/,2A))')                                      &
         'CLOCK JUMP HYPOTHESIS',                                             &
         '---------------------',                                             &
         '                          Change of the clock per epoch          ', &
         '   Number            Change of the clock with jumps',               &
         ' Clock name                     mean          sigma              ', &
         '  of jumps                 mean          sigma',                    &
         '                              [ns/s]         [ns/s]              ', &
         '  detected               [ns/s]         [ns/s]',                    &
         ' ----------------------------------------------------------------', &
         '-------------------------------------------------------------------'
!
! Search jumps in the clocks
! --------------------------
  nJump=0
  ALLOCATE(clkJump(5),stat=ios)
  CALL alcerr(ios,'clkJump',(/5/),'ccjump')
!
  iClk=1
  jClk=1
  nJmp=0
  tooBig=0
  DO WHILE (iClk <= OutClkHead%nSta+OutClkHead%nSat)
    IF (nJmp == 0) THEN
      line = OutClkHead%ClkName(iClk)
      mJmp=0
      ii = 0
    ELSE
!      line = ''
      ii = 60
    ENDIF
    nJmp=0
!
! Compute mean, s-dev from dT
! ---------------------------
    iEpo=1
    jEpo=OutClkRec%nEpo
    CALL ccdelt(iEpo, jEpo, iClk, OutClkHead, OutClkRec, nJump, clkJump, &
         nDeltaT, mDeltaT, sDeltaT)
!
! Write the detailed protocol
! ---------------------------
    IF (DABS(mDeltaT) < 100D0) THEN
      WRITE(line(24+ii:36+ii),'(F12.3)') mDeltaT * 1E3
    ELSE
      WRITE(line(24+ii:36+ii),'(E12.3)') mDeltaT * 1E3
    ENDIF
    IF (sDeltaT < 100D0) THEN
      WRITE(line(39+ii:51+ii),'(F12.3)') sDeltaT * 1E3
    ELSE
      WRITE(line(39+ii:51+ii),'(E12.3)') sDeltaT * 1E3
    ENDIF
!
! Use at least the min. RMS
! -------------------------
    sDeltT1 = sDeltaT
    IF (sDeltaT < CCopt%jumpRMS * 1D-3 / 300D0) THEN
      sDeltaT = CCopt%jumpRMS * 1D-3 / 300D0
      WRITE(line(53+ii:65+ii),'(A)') 'min. RMS used'
    ENDIF
!
! Search for epochs where mean +- n*sigma exceeded
! ------------------------------------------------
    iEpo=1
    DO WHILE (iEpo < OutClkRec%nEpo)
      IF (OutClkRec%Clock(iClk,iEpo) /= unDef) THEN
        jEpo=iEpo+1
        DO WHILE (OutClkRec%Clock(iClk,jEpo) == unDef)
          jEpo=jEpo+1
          IF (jEpo > OutClkRec%nEpo) EXIT
        ENDDO
        iJmp=1
        isJump=.FALSE.
        DO WHILE (iJmp <= nJump)
          isJump = isJump .OR.                        &
                   (clkJump(iJmp)%iClk == iClk .AND. &
                    clkJump(iJmp)%jEpo >  iEpo .AND. &
                    clkJump(iJmp)%iEpo <  jEpo)
          iJmp=iJmp+1
        ENDDO
        IF (jEpo <= OutClkRec%nEpo .AND. .NOT. isJump) THEN
          IF ((DABS((OutClkRec%Clock(iClk,jEpo)-OutClkRec%Clock(iClk,iEpo)) / &
              (OutClkRec%Epoch(jEpo)-OutClkRec%Epoch(iEpo))) &
                                      > mDeltaT + CCopt%nJmpSig * sDeltaT .OR. &
              (DABS((OutClkRec%Clock(iClk,jEpo)-OutClkRec%Clock(iClk,iEpo)) / &
              (OutClkRec%Epoch(jEpo)-OutClkRec%Epoch(iEpo))) &
                                      < mDeltaT - CCopt%nJmpSig * sDeltaT )))THEN
!
! Extent jump array
! -----------------
            IF (nJump >= SIZE(ClkJump)) THEN
              ALLOCATE(hlpJump(nJump), stat=ios)
              CALL alcerr(ios, 'hlpJump',(/nJump/),'ccjump')
              hlpJump(1:nJump)=clkJump(1:nJump)
              DEALLOCATE(clkJump)
              ALLOCATE(clkJump(nJump+5), stat=ios)
              CALL alcerr(ios, 'clkJump',(/nJump+5/),'ccjump')
              clkJump(1:nJump)=hlpJump(1:nJump)
              DEALLOCATE(hlpJump)
            ENDIF
!
! Store information into jump array
! ---------------------------------
            nJump=nJump+1
            ClkJump(nJump)%iClk=iClk
            ClkJump(nJump)%iEpo=iEpo
            ClkJump(nJump)%jEpo=jEpo
            ClkJump(nJump)%meanDT=mDeltaT
            ClkJump(nJump)%sdevDT=sDeltT1
            nJmp=nJmp+1
          ENDIF
        ENDIF
        iEpo=jEpo-1
      ENDIF
      iEpo=iEpo+1
    ENDDO
!
! Write the detailed protocol
! ---------------------------
    mJmp=mJmp+nJmp
    Write(line(69:74),'(I5)') mJmp
!
    IF (nJmp == 0) THEN
!
! Jump detection is done only, if remaining RMS is smaller than 100 minRMS
! ------------------------------------------------------------------------
      IF (sDeltT1 <= 1d2 /                                                  &
                     (OutClkRec%Epoch(OutClkRec%nEpo)-OutClkRec%Epoch(1)) * &
                     (OutClkRec%nEpo - 1) / CCopt%njmpSig) THEN
        DO iJmp=1,nJump
          IF (clkJump(iJmp)%iClk==iClk) ClkJump(iJmp)%sdevDT=sDeltT1
        ENDDO
      ELSE
        nJump=nJump-mJmp
        WRITE(line(53:65),'(A)') 'RMS too big  '
        WRITE(line(69:74),'(I5)') 0
        tooBig=tooBig+1
      ENDIF
      iClk = iClk+1
    ENDIF
    IF (CCopt%prtDetail(prtjmp) == 1 .AND. jClk+1 == iClk ) THEN
      WRITE(lfnprt,'(1X,A)') TRIM(line)
      jClk = iClk
    ENDIF
  ENDDO
!
! Write the detailed protocol
! ---------------------------
  IF (CCopt%prtDetail(prtjmp) == 1)                                           &
    WRITE(lfnprt,'(2A,/)')                                                    &
         ' ----------------------------------------------------------------', &
         '-------------------------------------------------------------------'
!
! Sort the jumps in list
! ----------------------
  isJump = .TRUE.
  DO WHILE (isJump)
    isJump = .FALSE.
    DO iJmp=1, nJump-1
      IF (clkJump(iJmp)%iClk == clkJump(iJmp+1)%iClk .AND. &
          clkJump(iJmp)%iEpo >=  clkJump(iJmp+1)%jEpo) THEN
        sortJmp=clkJump(iJmp+1)
        clkJump(iJmp+1)=clkJump(iJmp)
        clkJump(iJmp)=sortJmp
        isJump=.TRUE.
      ENDIF
    ENDDO
  ENDDO
!
! Is there a jump occuring in all clocks?
! ---------------------------------------
  iJmpLoop: DO iJmp=1,nJump
    DO iEpo=ClkJump(iJmp)%iEpo,ClkJump(iJmp)%jEpo
      iJump=tooBig
      DO jJmp=1,nJump
        IF (iEpo >= ClkJump(jJmp)%iEpo .AND. iEpo <= ClkJump(jJmp)%jEpo) &
          iJump=iJump+1
      ENDDO
      DO iClk=1,OutClkHead%nSta+OutClkHead%nSat
        IF (OutClkRec%Clock(iClk,iEpo) == unDef) &
          iJump=iJump+1
      ENDDO
      IF (iJump >= OutClkHead%nSta+OutClkHead%nSat) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,/)')                                     &
        ' ### SR CCJUMP: A JUMP IN ALL CLOCKS DETECTED',                    &
                        'IT MAY BE A PROBLEM WITH THE REFERENCE CLOCK'
        EXIT iJmpLoop
      ENDIF
    ENDDO
  ENDDO iJmpLoop

!==============================================================================
!
!  PART 2: Jump / Outlier
!
!==============================================================================
!
! Were two jumps an outlier?
! -------------------------
  IF (CCopt%prtDetail(prtjmp) == 1 .AND. nJump > 0)                  &
    WRITE(lfnprt,'(//,2(1X,A,/),2(/,1X,2A))')                        &
         'NOT REAL JUMPS BUT OUTLIERS','---------------------------',&
         'Clock name                    Size ns/s      from        ',&
         '         to','-------------------------------------------',&
         '-------------------------------------'
!
  isJump = .FALSE.
  iJmp=1
  DO WHILE (iJmp < nJump)
!
! Use the minimal Sigma
    sDeltaT = ClkJump(iJmp)%sdevDT
    IF (sDeltaT < CCopt%jumpRMS * 1D-3 / 300D0) THEN
      sDeltaT = CCopt%jumpRMS * 1D-3 / 300D0
    ENDIF
!
    IF (ClkJump(iJmp)%iClk == ClkJump(iJmp+1)%iClk                   .AND. &
        ClkJump(iJmp+1)%iEpo - ClkJump(iJmp)%jEpo <= CCopt%nJmpEpo   .AND. &
        (DABS((OutClkRec%Clock(ClkJump(iJmp+1)%iClk,ClkJump(iJmp+1)%jEpo) -  &
          OutClkRec%Clock(ClkJump(iJmp  )%iClk,ClkJump(iJmp  )%iEpo)) / &
         (OutClkRec%Epoch(ClkJump(iJmp+1)%jEpo)                      -  &
          OutClkRec%Epoch(ClkJump(iJmp  )%iEpo))) <=                     &
            ClkJump(iJmp)%meanDT + CCopt%nJmpSig * sDeltaT              &
                                                                    .AND. &
        (DABS((OutClkRec%Clock(ClkJump(iJmp+1)%iClk,ClkJump(iJmp+1)%jEpo) -  &
          OutClkRec%Clock(ClkJump(iJmp  )%iClk,ClkJump(iJmp  )%iEpo)) / &
         (OutClkRec%Epoch(ClkJump(iJmp+1)%jEpo)                      -  &
          OutClkRec%Epoch(ClkJump(iJmp  )%iEpo))) >=                     &
            ClkJump(iJmp)%meanDT - CCopt%nJmpSig * sDeltaT         ))) THEN
!
! Write a warning message if it is deleted
! ----------------------------------------
      IF ((ClkJump(iJmp)%iClk <= OutClkHead%nSta .AND. CCopt%jmpDelSta) .OR. &
          (ClkJump(iJmp)%iClk >  OutClkHead%nSta .AND. CCopt%jmpDelSat)) THEN
        CALL timst2(1,1,OutClkHead%TFirst + &
                        OutClkRec%Epoch(ClkJump(iJmp  )%jEpo)/86400d0,epost1)
        CALL timst2(1,1,OutClkHead%TFirst + &
                        OutClkRec%Epoch(ClkJump(iJmp+1)%iEpo)/86400d0,epost2)
        WRITE(lfnerr,'(/,A,3(/,16X,A,A),/)')                               &
             ' ### SJ CCJUMP: OUTLIER DETECTED IN THE CLOCK TIME SERIES',  &
                             'CLOCK NAME : ',                              &
                             TRIM(OutClkHead%ClkName(ClkJump(iJmp)%iClk)), &
                             'EPOCHS FROM: ',epost1,                       &
                             '       TO  : ',epost2
      ENDIF
!
! Write extended output
! ---------------------
      IF (CCopt%prtDetail(prtjmp) == 1) THEN
        IF (isJump) THEN
          WRITE(lfnprt,*)
        ELSE
          isJump=.TRUE.
        ENDIF
!
        CALL timst2(1,1,OutClkHead%TFirst + &
                        OutClkRec%Epoch(ClkJump(iJmp  )%iEpo)/86400d0,epost1)
        CALL timst2(1,1,OutClkHead%TFirst + &
                        OutClkRec%Epoch(ClkJump(iJmp  )%jEpo)/86400d0,epost2)
        mDeltaT = (OutClkrec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp  )%jEpo) -  &
                   OutClkrec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp  )%iEpo)) / &
                  (OutClkrec%Epoch(ClkJump(iJmp  )%jEpo) -  &
                   OutClkrec%Epoch(ClkJump(iJmp  )%iEpo))
        IF (DABS(mDeltaT) < 10D0) THEN
          WRITE(lfnprt,'(1X,A16,4X,A,F10.3,2X,A)')             &
            OutClkHead%ClkName(ClkJump(iJmp)%iClk),'  jmp 1:', &
            mDeltaT*1E3, epost1//'  '//epost2
        ELSE
          WRITE(lfnprt,'(1X,A16,4X,A,E10.3,2X,A)')             &
            OutClkHead%ClkName(ClkJump(iJmp)%iClk),'  jmp 1:', &
            mDeltaT*1E3, epost1//'  '//epost2
        ENDIF
!
        CALL timst2(1,1,OutClkHead%TFirst + &
                        OutClkRec%Epoch(ClkJump(iJmp+1)%iEpo)/86400d0,epost1)
        CALL timst2(1,1,OutClkHead%TFirst + &
                        OutClkRec%Epoch(ClkJump(iJmp+1)%jEpo)/86400d0,epost2)
        mDeltaT = (OutClkrec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp+1)%jEpo) -  &
                   OutClkrec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp+1)%iEpo)) / &
                  (OutClkrec%Epoch(ClkJump(iJmp+1)%jEpo) -  &
                   OutClkrec%Epoch(ClkJump(iJmp+1)%iEpo))
        IF (DABS(mDeltaT) < 10D0) THEN
          WRITE(lfnprt,'(21X,A,F10.3,2X,A)')                   &
            '  jmp 2:', mDeltaT*1E3, epost1//'  '//epost2
        ELSE
          WRITE(lfnprt,'(21X,A,E10.3,2X,A)')                   &
            '  jmp 2:', mDeltaT*1E3, epost1//'  '//epost2
        ENDIF
!
        CALL timst2(1,1,OutClkHead%TFirst + &
                        OutClkRec%Epoch(ClkJump(iJmp  )%jEpo)/86400d0,epost1)
        CALL timst2(1,1,OutClkHead%TFirst + &
                        OutClkRec%Epoch(ClkJump(iJmp+1)%iEpo)/86400d0,epost2)
        mDeltaT = (OutClkrec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp+1)%jEpo) -  &
                   OutClkrec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp  )%iEpo)) / &
                  (OutClkrec%Epoch(ClkJump(iJmp+1)%jEpo) -  &
                   OutClkrec%Epoch(ClkJump(iJmp  )%iEpo))
        IF (DABS(mDeltaT) < 10D0) THEN
          WRITE(lfnprt,'(21X,A,F10.3,2X,A)')                   &
            '  ->out:', mDeltaT*1E3, epost1//'  '//epost2
        ELSE
          WRITE(lfnprt,'(21X,A,E10.3,2X,A)')                   &
            '  ->out:', mDeltaT*1E3, epost1//'  '//epost2
        ENDIF
      ENDIF
!
! Mark outlier clocks (if requested)
! ----------------------------------
      DO iEpo=ClkJump(iJmp)%jEpo, ClkJump(iJmp+1)%iEpo
        IF ((ClkJump(iJmp)%iClk <= OutClkHead%nSta .AND. CCopt%jmpDelSta) .OR. &
            (ClkJump(iJmp)%iClk >  OutClkHead%nSta .AND. CCopt%jmpDelSat)) THEN
          OutClkRec%Clock(ClkJump(iJmp)%iClk,iEpo) = unDef
          OutClkRec%Sigma(ClkJump(iJmp)%iClk,iEpo) = unDef
        ENDIF
      ENDDO
!
! Remove "jumps" from list
! ------------------------
      IF (iJmp+2 <= nJump) THEN
        ClkJump(iJmp:nJump-2) = ClkJump(iJmp+2:nJump)
        iJmp=iJmp-1
      ENDIF
      nJump=nJump-2
    ENDIF
    iJmp=iJmp+1
  ENDDO
!
! Was a jump at the beginning or the end an outlier?
! --------------------------------------------------
  iJmp=1
  DO WHILE (iJmp <= nJump)
    jEpo = 0
    kEpo = OutClkRec%nEpo + CCopt%nJmpEpo + 1
!
    IF (iJmp == 1) THEN
      DO iEpo = 1,ClkJump(iJmp)%iEpo
        IF (jEpo == 0 .AND. &
            OutClkRec%Clock(ClkJump(iJmp)%iClk,iEpo) /= unDef) THEN
          jEpo =iEpo
        ENDIF
      ENDDO
    ELSE IF (ClkJump(iJmp-1)%iClk /= ClkJump(iJmp)%iClk) THEN
      DO iEpo = 1,ClkJump(iJmp)%iEpo
        IF (jEpo == 0 .AND. &
            OutClkRec%Clock(ClkJump(iJmp)%iClk,iEpo) /= unDef) THEN
          jEpo =iEpo
        ENDIF
      ENDDO
    ENDIF
!
    IF (iJmp == OutClkRec%nEpo) THEN
      DO iEpo = OutClkRec%nEpo, ClkJump(iJmp)%jEpo, -1
        IF (kEpo == 0 .AND. &
            OutClkRec%Clock(ClkJump(iJmp)%iClk,iEpo) /= unDef) THEN
          kEpo =iEpo
        ENDIF
      ENDDO
    ELSE IF (ClkJump(iJmp+1)%iClk /= ClkJump(iJmp)%iClk) THEN
      DO iEpo = OutClkRec%nEpo, ClkJump(iJmp)%jEpo, -1
        IF (kEpo == 0 .AND. &
            OutClkRec%Clock(ClkJump(iJmp)%iClk,iEpo) /= unDef) THEN
          kEpo =iEpo
        ENDIF
      ENDDO
    ENDIF
!
    IF (ClkJump(iJmp)%iEpo - jEpo + 1 <= CCopt%nJmpEpo .OR. &
        kEpo - ClkJump(iJmp)%jEpo + 1 <= CCopt%nJmpEpo) THEN
!
! Write a warning message if it is deleted
! ----------------------------------------
      IF ((ClkJump(iJmp)%iClk <= OutClkHead%nSta .AND. CCopt%jmpDelSta) .OR. &
          (ClkJump(iJmp)%iClk >  OutClkHead%nSta .AND. CCopt%jmpDelSat)) THEN
        IF (ClkJump(iJmp)%iEpo - jEpo <= CCopt%nJmpEpo) THEN
          CALL timst2(1,1,OutClkHead%TFirst + &
                          OutClkRec%Epoch(jEpo)/86400d0,epost1)
          CALL timst2(1,1,OutClkHead%TFirst + &
                          OutClkRec%Epoch(ClkJump(iJmp)%iEpo)/86400d0,epost2)
        ELSE
          CALL timst2(1,1,OutClkHead%TFirst + &
                          OutClkRec%Epoch(ClkJump(iJmp)%jEpo)/86400d0,epost1)
          CALL timst2(1,1,OutClkHead%TFirst + &
                          OutClkRec%Epoch(kEpo)/86400d0,epost2)
        ENDIF
        WRITE(lfnerr,'(/,A,3(/,16X,A,A),/)')                               &
             ' ### SJ CCJUMP: OUTLIER DETECTED IN THE CLOCK TIME SERIES',  &
                             'CLOCK NAME : ',                              &
                             TRIM(OutClkHead%ClkName(ClkJump(iJmp)%iClk)), &
                             'EPOCHS FROM: ',epost1,                       &
                             '       TO  : ',epost2
      ENDIF
!
! Write extended output
! ---------------------
      IF (CCopt%prtDetail(prtjmp) == 1) THEN
        IF (isJump) THEN
          WRITE(lfnprt,*)
        ELSE
          isJump=.TRUE.
        ENDIF
!
        IF (ClkJump(iJmp)%iEpo - jEpo <= CCopt%nJmpEpo) THEN
          CALL timst2(1,1,OutClkHead%TFirst + &
                          OutClkRec%Epoch(jEpo)/86400d0,epost1)
          CALL timst2(1,1,OutClkHead%TFirst + &
                          OutClkRec%Epoch(ClkJump(iJmp)%iEpo)/86400d0,epost2)
        ELSE
          CALL timst2(1,1,OutClkHead%TFirst + &
                          OutClkRec%Epoch(ClkJump(iJmp)%jEpo)/86400d0,epost1)
          CALL timst2(1,1,OutClkHead%TFirst + &
                          OutClkRec%Epoch(kEpo)/86400d0,epost2)
        ENDIF

        mDeltaT = (OutClkrec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp)%jEpo) -  &
                   OutClkrec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp)%iEpo)) / &
                  (OutClkrec%Epoch(ClkJump(iJmp)%jEpo) -  &
                   OutClkrec%Epoch(ClkJump(iJmp)%iEpo))
        IF (DABS(mDeltaT) < 10D0) THEN
          WRITE(lfnprt,'(1X,A16,4X,A,F10.3,2X,A)')             &
            OutClkHead%ClkName(ClkJump(iJmp)%iClk),'  ->out:', &
            mDeltaT*1E3, epost1//'  '//epost2
        ELSE
          WRITE(lfnprt,'(1X,A16,4X,A,E10.3,2X,A)')             &
            OutClkHead%ClkName(ClkJump(iJmp)%iClk),'  ->out:', &
            mDeltaT*1E3, epost1//'  '//epost2
        ENDIF
!
      ENDIF
!
! Mark outlier clocks (if requested)
! ----------------------------------
      IF (ClkJump(iJmp)%iEpo - jEpo <= CCopt%nJmpEpo) THEN
        DO iEpo=jEpo, ClkJump(iJmp)%iEpo
          IF ((ClkJump(iJmp)%iClk <= OutClkHead%nSta .AND. CCopt%jmpDelSta) .OR. &
              (ClkJump(iJmp)%iClk >  OutClkHead%nSta .AND. CCopt%jmpDelSat)) THEN
            OutClkRec%Clock(ClkJump(iJmp)%iClk,iEpo) = unDef
            OutClkRec%Sigma(ClkJump(iJmp)%iClk,iEpo) = unDef
          ENDIF
        ENDDO
      ELSE
        DO iEpo=ClkJump(iJmp)%jEpo, kEpo
          IF ((ClkJump(iJmp)%iClk <= OutClkHead%nSta .AND. CCopt%jmpDelSta) .OR. &
              (ClkJump(iJmp)%iClk >  OutClkHead%nSta .AND. CCopt%jmpDelSat)) THEN
            OutClkRec%Clock(ClkJump(iJmp)%iClk,iEpo) = unDef
            OutClkRec%Sigma(ClkJump(iJmp)%iClk,iEpo) = unDef
          ENDIF
        ENDDO
      ENDIF
!
! Remove "jumps" from list
! ------------------------
      IF (iJmp+1 <= nJump) THEN
        ClkJump(iJmp:nJump-1) = ClkJump(iJmp+1:nJump)
        iJmp=iJmp-1
      ENDIF
      nJump=nJump-1
    ENDIF
    iJmp=iJmp+1
  ENDDO
!
! Several jumps follow each other?
! --------------------------------
  iJmp=1
  DO WHILE (iJmp < nJump)
    IF (ClkJump(iJmp)%iClk == ClkJump(iJmp+1)%iClk .AND. &
        ClkJump(iJmp)%jEpo == ClkJump(iJmp+1)%iEpo) THEN
!
! Write a warning message if it is deleted
! ----------------------------------------
      IF ((ClkJump(iJmp)%iClk <= OutClkHead%nSta .AND. CCopt%jmpDelSta) .OR. &
          (ClkJump(iJmp)%iClk >  OutClkHead%nSta .AND. CCopt%jmpDelSat)) THEN
        CALL timst2(1,1,OutClkHead%TFirst + &
                        OutClkRec%Epoch(ClkJump(iJmp)%jEpo)/86400d0,epost1)
        WRITE(lfnerr,'(/,A,3(/,16X,A,A),/)')                               &
             ' ### SJ CCJUMP: OUTLIER DETECTED IN THE CLOCK TIME SERIES',  &
                             'CLOCK NAME : ',                              &
                             TRIM(OutClkHead%ClkName(ClkJump(iJmp)%iClk)), &
                             'EPOCHS FROM: ',epost1,                       &
                             '       TO  : ',epost1
        OutClkRec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp)%jEpo) = unDef
        OutClkRec%Sigma(ClkJump(iJmp)%iClk,ClkJump(iJmp)%jEpo) = unDef
      ENDIF
!
! Write extended output
! ---------------------
      IF (CCopt%prtDetail(prtjmp) == 1) THEN
        IF (isJump) THEN
          WRITE(lfnprt,*)
        ELSE
          isJump=.TRUE.
        ENDIF
!
        CALL timst2(1,1,OutClkHead%TFirst + &
                        OutClkRec%Epoch(ClkJump(iJmp)%iEpo)/86400d0,epost1)
        CALL timst2(1,1,OutClkHead%TFirst + &
                        OutClkRec%Epoch(ClkJump(iJmp)%jEpo)/86400d0,epost2)
!
        mDeltaT = (OutClkrec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp)%jEpo) -  &
                   OutClkrec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp)%iEpo)) / &
                  (OutClkrec%Epoch(ClkJump(iJmp)%jEpo) -  &
                   OutClkrec%Epoch(ClkJump(iJmp)%iEpo))

        IF (DABS(mDeltaT) < 10D0) THEN
          WRITE(lfnprt,'(1X,A16,4X,A,F10.3,2X,A)')             &
            OutClkHead%ClkName(ClkJump(iJmp)%iClk),'  jmp 1:', &
            mDeltaT*1E3, epost1//'  '//epost2
        ELSE
          WRITE(lfnprt,'(1X,A16,4X,A,E10.3,2X,A)')             &
            OutClkHead%ClkName(ClkJump(iJmp)%iClk),'  jmp 1:', &
            mDeltaT*1E3, epost1//'  '//epost2
        ENDIF
!
        CALL timst2(1,1,OutClkHead%TFirst + &
                        OutClkRec%Epoch(ClkJump(iJmp+1)%iEpo)/86400d0,epost1)
        CALL timst2(1,1,OutClkHead%TFirst + &
                        OutClkRec%Epoch(ClkJump(iJmp+1)%jEpo)/86400d0,epost2)
!
        mDeltT1 = (OutClkrec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp+1)%jEpo) -  &
                   OutClkrec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp+1)%iEpo)) / &
                  (OutClkrec%Epoch(ClkJump(iJmp+1)%jEpo) -  &
                   OutClkrec%Epoch(ClkJump(iJmp+1)%iEpo))

        IF (DABS(mDeltT1) < 10D0) THEN
          WRITE(lfnprt,'(21X,A,F10.3,2X,A)')                   &
            '  jmp 2:', mDeltT1*1E3, epost1//'  '//epost2
        ELSE
          WRITE(lfnprt,'(21X,A,E10.3,2X,A)')                   &
            '  jmp 2:', mDeltT1*1E3, epost1//'  '//epost2
        ENDIF
      ENDIF
!
! Remove "jumps" from list
! ------------------------
      ClkJump(iJmp+1)%iEpo = ClkJump(iJmp)%iEpo
      IF (iJmp+1 <= nJump) THEN
        ClkJump(iJmp:nJump-1) = ClkJump(iJmp+1:nJump)
!        iJmp=iJmp-1
      ENDIF
      nJump=nJump-1
!
! Is it still a jump or not?
! --------------------------
      mDeltaT = (OutClkrec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp)%jEpo) -  &
                 OutClkrec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp)%iEpo)) / &
                (OutClkrec%Epoch(ClkJump(iJmp)%jEpo) -  &
                 OutClkrec%Epoch(ClkJump(iJmp)%iEpo))
      IF (DABS(mDeltaT) <= &
          ClkJump(iJmp)%meanDT + CCopt%nJmpSig * sDeltaT) THEN
        IF (CCopt%prtDetail(prtjmp) == 1) THEN
          CALL timst2(1,1,OutClkHead%TFirst + &
                          OutClkRec%Epoch(ClkJump(iJmp)%iEpo)/86400d0,epost1)
          CALL timst2(1,1,OutClkHead%TFirst + &
                          OutClkRec%Epoch(ClkJump(iJmp)%jEpo)/86400d0,epost2)
!
          IF (DABS(mDeltaT) < 10D0) THEN
            WRITE(lfnprt,'(21X,A,F10.3,2X,A)')                   &
              '  ->out:', mDeltaT*1E3, epost1//'  '//epost2
          ELSE
            WRITE(lfnprt,'(21X,A,E10.3,2X,A)')                   &
              '  ->out:', mDeltaT*1E3, epost1//'  '//epost2
          ENDIF
        ENDIF
! remove the jump
        IF (iJmp+1 <= nJump) THEN
          ClkJump(iJmp:nJump-1) = ClkJump(iJmp+1:nJump)
!          iJmp=iJmp-1
        ENDIF
        nJump=nJump-1
      ELSE
        IF (CCopt%prtDetail(prtjmp) == 1) THEN
!
          CALL timst2(1,1,OutClkHead%TFirst + &
                          OutClkRec%Epoch(ClkJump(iJmp)%iEpo)/86400d0,epost1)
          CALL timst2(1,1,OutClkHead%TFirst + &
                          OutClkRec%Epoch(ClkJump(iJmp)%jEpo)/86400d0,epost2)
!
          IF (DABS(mDeltaT) < 10D0) THEN
            WRITE(lfnprt,'(21X,A,F10.3,2X,A)')                   &
              '  ->jmp:', mDeltaT*1E3, epost1//'  '//epost2
          ELSE
            WRITE(lfnprt,'(21X,A,E10.3,2X,A)')                   &
              '  ->jmp:', mDeltaT*1E3, epost1//'  '//epost2
          ENDIF
        ENDIF
      ENDIF
    ELSE
      iJmp=iJmp+1
    ENDIF
  ENDDO
  IF (CCopt%prtDetail(prtjmp) == 1 .AND. nJump > 0)                       &
    WRITE(lfnprt,'(2A,/)') ' -------------------------------------------',&
                           '-------------------------------------'

!==============================================================================
!
!  PART 3: Did the Introduced Jump Realy Improved the Time Series?
!
!==============================================================================
!
! Test jumps or pseudo-jumps
! --------------------------
  IF (CCopt%prtDetail(prtjmp) == 1 .AND. nJump > 0)                          &
    WRITE(lfnprt,'(//,2(1X,A,/),2(/,1X,A))')                                 &
         'JUMPS HAVE TO REDUCE THE NOISE LEVEL',                             &
         '------------------------------------',                             &
         'Station name                  from                  to        ' // &
         '   size (ns/s)     w/o the jump (ns/s)     with the jump (ns/s)',  &
         '----------------------------------------------------------------'//&
         '-------------------------------------------------------------------'
!
  ALLOCATE(delJump(nJump),stat=ios)
  CALL alcerr(ios,'delJump',(/nJump/),'ccjump')
  delJump=.FALSE.
!
  iJmp=1
  DO WHILE (iJmp <= nJump)
!
! Search an interval between the jumps
! ------------------------------------
    jClk = 0
    kEpo = 0
    DO WHILE (jClk < 10 .AND. kEpo >= 0)
      kEpo = kEpo + 1
      iEpo = ClkJump(iJmp)%iEpo-kEpo
      IF (iEpo < 1) THEN                      ! start of the time series
        iEpo =  1
        kEpo = -1
      ENDIF
      IF (iJmp > 1) THEN
        IF (ClkJump(iJmp-1)%iClk == ClkJump(iJmp)%iClk .AND. &
            iEpo <= ClkJump(iJmp-1)%jEpo) THEN  ! last jump
          iEpo = ClkJump(iJmp-1)%jEpo
          kEpo = -1
        ENDIF
      ENDIF
      IF (kEpo > 20) THEN                     ! limit the time window
        iEpo = ClkJump(iJmp)%iEpo - 20
        kEpo = -1
      ENDIF
      IF (kEpo >= 0 .AND. &
          OutClkRec%Clock(ClkJump(iJmp)%iClk,iEpo) /= unDef) THEN
        jClk =jClk + 1
      ENDIF
    ENDDO
!
    kClk = 0
    kEpo = 0
    DO WHILE (kClk < 10 .AND. kEpo >= 0)
      kEpo = kEpo + 1
      jEpo = ClkJump(iJmp)%jEpo + kEpo
      IF (jEpo > OutClkRec%nEpo) THEN        ! end of time series
        jEpo = OutClkRec%nEpo
        kEpo = -1
      ENDIF
      IF (iJmp < nJump) THEN
        IF (ClkJump(iJmp+1)%iClk == ClkJump(iJmp)%iClk .AND. &
            jEpo >= ClkJump(iJmp+1)%iEpo) THEN  ! next jump
          jEpo = ClkJump(iJmp+1)%iEpo
          kEpo = -1
        ENDIF
      ENDIF
      IF (kEpo > 20) THEN                    ! limit the time window
        jEpo = ClkJump(iJmp)%jEpo + 20
        kEpo = -1
      ENDIF
      IF (kEpo >= 0 .AND. &
          OutClkRec%Clock(ClkJump(iJmp)%iClk,jEpo) /= unDef) THEN
        kClk =kClk + 1
      ENDIF
    ENDDO
!
! Compute mean, s-dev from dT
! ---------------------------
    CALL ccdelt(iEpo, jEpo, ClkJump(iJmp)%iClk, OutClkHead, OutClkRec, &
         0, clkJump, nDeltaT, mDeltaT, sDeltaT)
!
    CALL ccdelt(iEpo, jEpo, ClkJump(iJmp)%iClk, OutClkHead, OutClkRec, &
         nJump, clkJump, nDeltT1, mDeltT1, sDeltT1)
!
! Write the detailed protocol
! ---------------------------
    IF (CCopt%prtDetail(prtjmp) == 1) THEN
      line = ''
      CALL timst2(1,1,OutClkHead%TFirst + &
                      OutClkRec%Epoch(ClkJump(iJmp)%iEpo)/86400d0,epost1)
      CALL timst2(1,1,OutClkHead%TFirst + &
                      OutClkRec%Epoch(ClkJump(iJmp)%jEpo)/86400d0,epost2)
      WRITE(line,'(1X,A,F13.3,2(F13.3,A,F8.3))')                               &
                    OutClkHead%ClkName(ClkJump(iJmp)%iClk)(1:16)//             &
                    '      '//epost1//'  '//epost2,                            &
                    (OutClkRec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp)%jEpo) -  &
                     OutClkRec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp)%iEpo)) / &
                    (OutClkRec%Epoch(ClkJump(iJmp)%jEpo)      -                &
                     OutClkRec%Epoch(ClkJump(iJmp)%iEpo))*1E3,                 &
                     mDeltaT * 1E3, ' +- ', sDeltaT * 1E3,                     &
                     mDeltT1 * 1E3, ' +- ', sDeltT1 * 1E3
    ENDIF
!
! It's not a real jump ...
! ------------------------
    IF ((sDeltaT < sDeltT1 * CCOpt%nJmpSig .OR. & ! improve sigma significantly
         mDeltaT < mDeltT1 .OR. &                 ! improve mean
         nDeltaT <= 1 .OR. &                      ! only 1 value between 2 jumps
         jClk <= 1 .OR. kClk <= 1) .AND. &        ! -- " --
        ClkJump(iJmp)%jEpo-ClkJump(iJmp)%iEpo <= CCopt%nJmpEpo) THEN
      delJump(iJmp) = .TRUE.
      IF (CCopt%prtDetail(prtjmp) == 1) &
        WRITE(lfnprt,'(A)') TRIM(line) // ' noise'
    ELSE
      IF (CCopt%prtDetail(prtjmp) == 1) &
        WRITE(lfnprt,'(A)') TRIM(line) // ' jump '
    ENDIF
!
    iJmp=iJmp+1
  ENDDO
!
  IF (CCopt%prtDetail(prtjmp) == 1 .AND. nJump > 0)                          &
    WRITE(lfnprt,'(1X,A,/)')                                                 &
         '----------------------------------------------------------------'//&
         '-------------------------------------------------------------------'
!
! Remove the invalid jumps
! ------------------------
  iJmp=1
  DO WHILE (iJmp <= nJump)
    IF (delJump(iJmp)) THEN
      IF (iJmp+1 <= nJump) THEN
        ClkJump(iJmp:nJump-1) = ClkJump(iJmp+1:nJump)
        delJump(iJmp:nJump-1) = delJump(iJmp+1:nJump)
        iJmp=iJmp-1
      ENDIF
      nJump=nJump-1
    ENDIF
    iJmp=iJmp+1
  ENDDO
!
  DEALLOCATE(delJump)
!
  RETURN
  END SUBROUTINE


END MODULE
