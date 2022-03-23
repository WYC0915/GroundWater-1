MODULE s_CCJEST
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE ccjest(CCopt, OutClkHead, OutClkRec, nJump, clkJump, irCode)

! -------------------------------------------------------------------------
! Purpose:    Estimated the size of the clock jumps
!
! Author:     R. Dach
!
! Created:    11-Apr-2001
!
! Changes:    02-May-2001 RD: this SR is only for the program output now
!             03-aug-2001 RD: Modified program output
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             04-Jun-2002 RD: Make the PC version work
!             29-Oct-2002 MR: Correct format
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
  USE m_bern,   ONLY: i4b, r8b, lfnprt
  USE d_clkrnx, ONLY: t_clkhead,t_clkrec
  USE p_ccrnxc, ONLY: t_ccrnxc_opt,t_Jump,prtjmp
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
  TYPE(t_clkhead)                :: OutClkHead ! header of output file
  TYPE(t_clkrec)                 :: OutClkRec  ! Combined data
  INTEGER(i4b)                   :: nJump      ! Number of clock jumps

! input/output
  TYPE(t_Jump), DIMENSION(:), POINTER          &
                                 :: ClkJump    ! Detected clock jumps

! output
  INTEGER(i4b)                   :: irCode     ! Return code from the sr

! Local variables
! ---------------
  CHARACTER(LEN=19)              :: epost1 ! Epoch string
  CHARACTER(LEN=19)              :: epost2 ! Epoch string

  INTEGER(i4b)                   :: iClk   ! Counter for clocks
  INTEGER(i4b)                   :: iEpo   ! Counter for epochs
  INTEGER(i4b)                   :: jPoly  ! Counter for deg. of polynom
  INTEGER(i4b)                   :: kPoly  ! Counter for deg. of polynom
  INTEGER(i4b)                   :: lPoly  ! Counter for deg. of polynom
  INTEGER(i4b)                   :: mPoly  ! Number of polynoms to compute
  INTEGER(i4b)                   :: nPoly  ! Max. poly. deg to compute
  INTEGER(i4b)                   :: nSing  ! Number of sing. elements
  INTEGER(i4b),    DIMENSION(:),  &
                   ALLOCATABLE   :: ParLst ! List of singular parameters
  INTEGER(i4b)                   :: iJmp   ! Counter for clk jumps
  INTEGER(i4b)                   :: nJmp   ! Number of clk jumps per clk
  INTEGER(i4b)                   :: iJump  ! Index for clk jumps
  INTEGER(i4b)                   :: ios    ! IO status
  INTEGER(i4b)                   :: numClk ! Number of entries for a clk

  REAL(r8b)                      :: rmsClk ! RMS of polynom fit for a clk
  REAL(r8b),       DIMENSION(:),  &
                   ALLOCATABLE   :: ATA    ! Design matrix
  REAL(r8b),       DIMENSION(:),  &
                   ALLOCATABLE   :: ATY    ! Vect. of obs.
  REAL(r8b),       DIMENSION(:),  &
                   ALLOCATABLE   :: b      ! Solution vector
  REAL(r8b)                      :: mRef   ! Computing polynom
  REAL(r8b)                      :: DeltaT ! Time term
  REAL(r8b)                      :: sDeltT ! RMS of noise computation

!
  irCode = 0
!
! Return if nothing to do
! -----------------------
  IF (nJump == 0 .OR. CCopt%nJmpPoly < 0) RETURN
!
! Initialization
! --------------
  nPoly=CCopt%nJmpPoly+1
!
  IF (CCopt%prtDetail(prtjmp) == 1 .OR. 1==1)                             &
    WRITE(lfnprt,'(/,2(/,1X,A),/,2(/,1X,A))')                               &
        'COMPARISON OF REAL AND ESTIMATED SIZE OF THE CLOCK JUMPS',         &
        '--------------------------------------------------------',         &
        'Station name                  from                  to          '//&
        '   Size of the jump at the epoch      Estimated size of the jump', &
        '----------------------------------------------------------------'//&
        '-------------------------------------------------------------------'

!
! Loop all clocks
! ---------------
  iClk = 1
  DO WHILE (iClk<=OutClkHead%nSta+OutClkHead%nSat)
!
! A clock jump detected for this clock?
! -------------------------------------
    nJmp=0
    DO iJmp=1,nJump
      IF (clkJump(iJmp)%iClk == iClk) nJmp=nJmp + 1
    ENDDO
!
    IF (nJmp > 0) THEN
!
! Allocate arrays for polynomial fit
! ----------------------------------
      ALLOCATE(ATA(IKF(nPoly+nJmp,nPoly+nJmp)), stat=ios)
      CALL alcerr(ios,'ATA',(/IKF(nPoly+nJmp,nPoly+nJmp)/),'ccjest')
      ALLOCATE(ATY(nPoly+nJmp), stat=ios)
      CALL alcerr(ios,'ATY',(/nPoly+nJmp/),'ccjest')
      ALLOCATE(b(nPoly+nJmp), stat=ios)
      CALL alcerr(ios,'b',(/nPoly+nJmp/),'ccjest')
!
      ALLOCATE(ParLst(nPoly+nJmp),stat=ios)
      CALL alcerr(ios,'ParLst',(/nPoly+nJmp/),'ccjest')
!
      ATA=0d0
      ATY=0d0
      b  =0d0
      rmsClk=0d0
      numClk=0

!
! Loop all epochs for this clock
! ------------------------------
      DO iEpo=1,OutClkRec%nEpo
        IF (OutClkRec%Clock(iClk,iEpo) /= 999999.999999D0) THEN
          numclk=numClk+1
!
! Compute ATA and ATY for polynomial fit
! --------------------------------------
          DeltaT = (OutClkRec%Epoch(iEpo) -                                   &
                  0.5*OutClkRec%Epoch(OutClkRec%nEpo)-1.5*OutClkRec%Epoch(1))/&
                  OutClkRec%Epoch(OutClkRec%nEpo)
!
! update ATA matrix
          jPoly=1
          DO WHILE (jPoly <= CCopt%nJmpPoly+1)
            kPoly=jPoly
            DO WHILE (kPoly <= CCopt%nJmpPoly+1)
              IF (jPoly+kPoly-2 > 0) THEN
                ATA(IKF(jPoly,kPoly)) = ATA(IKF(jPoly,kPoly)) + &
                                    DeltaT ** (jPoly+kPoly-2)
              ELSE
                ATA(IKF(jPoly,kPoly)) = ATA(IKF(jPoly,kPoly)) + 1D0
              ENDIF
              kPoly=kPoly+1
            ENDDO
            jPoly=jPoly+1
          ENDDO
!
! update ATY vector
          jPoly=1
          DO WHILE (jPoly <= CCopt%nJmpPoly+1)
            IF (jPoly == 1) THEN
              ATY(jPoly) = ATY(jPoly) + OutClkRec%Clock(iClk,iEpo)
            ELSE
              ATY(jPoly) = ATY(jPoly) + &
                                 OutClkRec%Clock(iClk,iEpo) * DeltaT**(jPoly-1)
            ENDIF
            jPoly=jPoly+1
          ENDDO
        ENDIF
      ENDDO
!
! Handle the clock jumps
! ----------------------
      iJump=0
      DO iJmp=1,nJump
        IF (ClkJump(iJmp)%iClk == iClk) THEN
          iJump=iJump+1
          DO iEpo=ClkJump(iJmp)%jEpo,OutClkRec%nEpo
            IF (OutClkRec%Clock(iClk,iEpo) /= 999999.999999D0) THEN
!
! Modify ATA and ATY for jump estim.
! ----------------------------------
              DeltaT = (OutClkRec%Epoch(iEpo) -                                &
                 0.5*OutClkRec%Epoch(OutClkRec%nEpo)-1.5*OutClkRec%Epoch(1)) / &
                 OutClkRec%Epoch(OutClkRec%nEpo)
!
! update ATA matrix
              jPoly = CCopt%nJmpPoly+1+iJump
!
              kPoly=1
              DO WHILE (kPoly <= CCopt%nJmpPoly+1)
                IF (kPoly-1 > 0) THEN
                  ATA(IKF(jPoly,kPoly)) = &
                        ATA(IKF(jPoly,kPoly)) + DeltaT ** (kPoly-1)
                ELSE
                  ATA(IKF(jPoly,kPoly)) =  ATA(IKF(jPoly,kPoly)) + 1D0
                ENDIF
                kPoly=kPoly+1
              ENDDO
              DO lPoly=kPoly,jPoly
                ATA(IKF(jPoly,lPoly)) = ATA(IKF(jPoly,lPoly)) + 1D0
              ENDDO
!
! update ATY vector
              jPoly = CCopt%nJmpPOly+1+iJump
!
              ATY(jPoly) = ATY(jPoly) + OutClkRec%Clock(iClk,iEpo)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
!
! NEQ-Inversion for poly. fit
      jPoly = CCopt%nJmpPoly+1+nJmp
!
!do kpoly=1,jpoly
! write(*,*) iClk, nJmp, (ata(ikf(ios,kpoly)),ios=1,jpoly), aty(kpoly)
!enddo
!
      CALL syminvg(jPoly,ATA,0,nSing,ParLst)
      CALL solve (jPoly,ATA,ATY,b)
!
! Compute the RMS of the fit
      DO iEpo=1,OutClkRec%nEpo
        IF (OutClkRec%Clock(iClk,iEpo) /= 999999.999999D0) THEN
          DeltaT = (OutClkRec%Epoch(iEpo) -                                    &
                  0.5*OutClkRec%Epoch(OutClkRec%nEpo)-1.5*OutClkRec%Epoch(1)) /&
                  OutClkRec%Epoch(OutClkRec%nEpo)
!
          jPoly=1
          mRef=b(jPoly)
          jPoly=jPoly+1
          DO WHILE (jPoly <= CCopt%nJmpPoly+1)
            mRef = mRef + b(jPoly) * DeltaT**(jPoly-1)
            jPoly=jPoly+1
          ENDDO
!
! Add est. clock jump sizes
          iJump=0
          DO iJmp=1,nJump
            IF (ClkJump(iJmp)%iClk == iClk) THEN
              iJump=iJump+1
              IF (ClkJump(iJmp)%jEpo <= iEpo) THEN
                jPoly = CCopt%nJmpPoly+1+iJump
                ClkJump(iJmp)%jSize=b(jPoly)
                ClkJump(iJmp)%sSize=ATA(IKF(jPoly,jPoly))
                mRef = mRef + b(jPoly)
              ENDIF
              IF (ClkJump(iJmp)%iEpo == iEpo) &
                ClkJump(iJmp)%meandT = mRef
              IF (ClkJump(iJmp)%jEpo == iEpo) &
                ClkJump(iJmp)%meandT = mRef - ClkJump(iJmp)%meandT
            ENDIF
          ENDDO
!
! Degr. of freedom, RMS comp.
! ---------------------------
          mPoly = CCopt%nJmpPoly + 1 + nJmp
!
          IF (numClk-mPoly > 1)                                      &
            rmsClk = rmsClk + (OutClkRec%Clock(iClk,iEpo)-mRef)**2 / &
                              (numClk-1-mPoly)
        ENDIF
      ENDDO
!
! Use RMS for sigma computation
! -----------------------------
      DO iJmp=1,nJump
        IF (ClkJump(iJmp)%iClk == iClk) THEN
          ClkJump(iJmp)%sSize = DSQRT(ClkJump(iJmp)%sSize * rmsClk)
        ENDIF
      ENDDO
!
! Compare real size and estimated size of a clock jump
! ----------------------------------------------------
      iJmp=1
      DO WHILE (iJmp<=nJump)
        sDeltT = ClkJump(iJmp)%sDevDT * 300D0
        IF (sDeltT < CCopt%jumpRMS * 1E-3) sDeltT = CCopt%jumpRMS * 1E-3
!
        IF (ClkJump(iJmp)%iClk == iClk) THEN
          IF (CCopt%prtDetail(prtjmp) == 1 .OR. 1==1) THEN
            CALL timst2(1,1,OutClkHead%TFirst + &
                          OutClkRec%Epoch(ClkJump(iJmp)%iEpo)/86400d0,epost1)
            CALL timst2(1,1,OutClkHead%TFirst + &
                          OutClkRec%Epoch(ClkJump(iJmp)%jEpo)/86400d0,epost2)
            IF (DABS(OutClkRec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp)%jEpo) -  &
                     OutClkRec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp)%iEpo))   &
                                                                   < 1D3 .AND. &
                DABS(ClkJump(iJmp)%meanDT) < 1D3) THEN
              WRITE(lfnprt,'(1X,A,2(3X,2(F12.3,A)))')                          &
              OutClkHead%ClkName(ClkJump(iJmp)%iClk)(1:16)//                   &
                   '      '//epost1//'  '//epost2,                             &
                  (OutClkRec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp)%jEpo) -    &
                   OutClkRec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp)%iEpo))*1E3,&
                          ' +- ',sDeltT * 1E3,' ns',                           &
                   ClkJump(iJmp)%meanDT*1E3,' +- ',ClkJump(iJmp)%sSize*1E3,' ns'
            ELSE
              WRITE(lfnprt,'(1X,A,2(3X,2(E12.3,A)))')                          &
              OutClkHead%ClkName(ClkJump(iJmp)%iClk)(1:16)//                   &
                   '      '//epost1//'  '//epost2,                             &
                  (OutClkRec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp)%jEpo) -    &
                   OutClkRec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp)%iEpo))*1E3,&
                          ' +- ',sDeltT * 1E3,' ns',                           &
                   ClkJump(iJmp)%meanDT*1E3,' +- ',ClkJump(iJmp)%sSize*1E3,' ns'
            ENDIF
          ENDIF

!          IF (clkJump(iJmp)%meandT <                                           &
!                     (OutClkRec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp)%jEpo) - &
!                      OutClkRec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp)%iEpo))- &
!                      ClkJump(iJmp)%sSize * CCopt%nJmpSig            .OR.      &
!                      sDeltT * CCopt%nJmpSig                         .OR.      &
!              ClkJump(iJmp)%meandT >                                           &
!                     (OutClkRec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp)%jEpo) - &
!                      OutClkRec%Clock(ClkJump(iJmp)%iClk,ClkJump(iJmp)%iEpo))+ &
!                      ClkJump(iJmp)%sSize * CCopt%nJmpSig)         THEN
!                      sDeltT * CCopt%nJmpSig)         THEN
!            IF (CCopt%prtDetail(prtjmp) == 1 .OR.1==1)   &
!              WRITE(lfnprt,'(4X,A)')    &
!                    '==>> THIS SEEMS NOT TO BE A REAL CLOCK JUMP'
!
!            IF (iJmp < nJump) THEN
!              ClkJump(iJmp:nJump-1)=ClkJump(iJmp+1:nJump)
!              iJmp=iJmp-1
!            ENDIF
!            nJump=nJump-1
!          ENDIF
        ENDIF
        iJmp=iJmp+1
      ENDDO
!
! Deallocate arrays for polynomial fit
! ------------------------------------
      DEALLOCATE(ATA)
      DEALLOCATE(ATY)
      DEALLOCATE(b)
      DEALLOCATE(ParLst)
!
    ENDIF
    iClk = iClk + 1
  ENDDO
  IF (CCopt%prtDetail(prtjmp) == 1.OR. 1==1) &
    WRITE(lfnprt,'(1X,A,/)')                                   &
        '----------------------------------------------------------------'//&
        '-------------------------------------------------------------------'
!
  RETURN
  END SUBROUTINE

END MODULE
