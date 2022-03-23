MODULE s_CCEXTR
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE ccextr(CCopt, OutClkHead, OutClkRec, nJump, clkJump, irCode)

! -------------------------------------------------------------------------
! Purpose:    Interpolation/Extrapolation of clocks
!
! Author:     R. Dach
!
! Created:    27-Feb-2001
!
! Changes:    01-Aug-2001 RD: Time window for model components
!             03-Aug-2001 RD: Modified program output
!             27-Aug-2001 RD: Min. num of data points for extrapol.
!             03-Sep-2001 RD: Handle clocks without values
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             04-Jun-2002 RD: Make the PC version work
!             29-Oct-2002 MR: Correct Formats (6x)
!             23-Apr-2003 CU: Nullify local pointers
!             14-Aug-2003 RD: "Sigma-weighting" if no sigma for the clocks
!             12-Nov-2003 RD: Use SR syminvg instead of SR syming
!             25-Nov-2003 RD: Some POINTER->ALLOCATABLE
!             26-Nov-2003 HB: Check for sigma /= unDef (999999.999999D0)
!             21-Jan-2004 RD: Adapt program output to new input panel
!             09-Aug-2010 RD: New syminvg used
!             19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnprt, lineLength
  USE d_clkrnx, ONLY: t_clkhead,t_clkrec,unDef
  USE p_ccrnxc, ONLY: t_ccrnxc_opt,t_Jump,null,prtext
  USE d_const,  ONLY: pi
  USE f_ikf
  USE s_solve
  USE s_alcerr
  USE s_syminvg
  IMPLICIT NONE

! List of Parameters
! ------------------
! input
  TYPE(t_ccrnxc_opt)             :: CCopt      ! Input options for combination

! input/output
  TYPE(t_clkhead)                :: OutClkHead ! header of output file
  TYPE(t_clkrec)                 :: OutClkRec  ! Combined data

! input
  INTEGER(i4b)                   :: nJump      ! Number of clock jumps
  TYPE(t_Jump),   DIMENSION(:),   &
                  POINTER        :: ClkJump    ! Detected clock jumps

! output
  INTEGER(i4b)                   :: irCode     ! Return code from the sr

! Local variables
! ---------------
  CHARACTER(LEN=lineLength),      &
    DIMENSION(:,:),  ALLOCATABLE :: IEsumm ! summary text: inter/extrapol.

  INTEGER(i4b)                   :: i1,i2  ! Index for writing strings
  INTEGER(i4b)                   :: iClk   ! Counter for clocks
  INTEGER(i4b)                   :: iEpo   ! Counter for epochs
  INTEGER(i4b)                   :: iLine  ! Counter for lines
  INTEGER(i4b)                   :: iPoly  ! Counter for deg. of polynom
  INTEGER(i4b)                   :: jPoly  ! Counter for deg. of polynom
  INTEGER(i4b)                   :: kPoly  ! Counter for deg. of polynom
  INTEGER(i4b)                   :: lPoly  ! Counter for deg. of polynom
  INTEGER(i4b)                   :: mPoly  ! Number of polynoms to compute
  INTEGER(i4b)                   :: nPoly  ! Max. poly. deg to compute
  INTEGER(i4b)                   :: iSiCo  ! Counts periodical elements
  INTEGER(i4b)                   :: jSiCo  ! Counts periodical elements
  INTEGER(i4b)                   :: nSing  ! Number of sing. elements
  INTEGER(i4b),     DIMENSION(:), &
                    ALLOCATABLE  :: ParLst ! List of singular parameters
  INTEGER(i4b)                   :: iJmp   ! Counter for clk jumps
  INTEGER(i4b)                   :: nJmp   ! Number of clk jumps per clk
  INTEGER(i4b)                   :: iJump  ! Index for clk jumps
  INTEGER(i4b)                   :: ios    ! IO status
  INTEGER(i4b)                   :: numClk ! Number of entries for a clk

  REAL(r8b)                      :: rmsClk ! RMS of polynom fit for a clk
  REAL(r8b)                      :: mRef   ! Computing polynom
  REAL(r8b)                      :: sRef   ! Computing sig. of polynom
  REAL(r8b)                      :: DeltaT ! Time term
  REAL(r8b)                      :: DeltTT ! Time term
  REAL(r8b)                      :: Sigma0 ! mean clk sigma, apriori wgt
  REAL(r8b)                      :: clkwgt ! Weight of the clock
  REAL(r8b)                      :: sumwgt ! Sum of weight factors
  REAL(r8b),        DIMENSION(:), &
                    ALLOCATABLE  :: ATA    ! Design matrix
  REAL(r8b),        DIMENSION(:), &
                    ALLOCATABLE  :: ATY    ! Vect. of obs.
  REAL(r8b),        DIMENSION(:), &
                    ALLOCATABLE  :: b      ! Solution vector

!
!
  irCode = 0
  IF (.NOT. CCopt%doInter .AND. .NOT. CCopt%doExtra) RETURN
!
! Allocate arrays for the statistic
! ---------------------------------
  iClk = OutClkHead%nSta + OutClkHead%nSat
  iLine=INT(CCopt%IEpoly/2)+ 2 + CCopt%IEsico
  ALLOCATE(IEsumm(iClk,iLine), stat=ios)
  CALL alcerr(ios, 'IEsumm', (/iClk,iLine/), 'ccextr')
  IEsumm=''
!
! Loop all clocks
! ----------------
  nPoly=CCopt%IEpoly+1 + 2*CCopt%IEsico
  iClk = 1
  DO WHILE (iClk<=OutClkHead%nSta+OutClkHead%nSat)
!
! A clock jump detected for this clock?
! -------------------------------------
    nJmp = 0
    DO iJmp=1,nJump
      IF (clkJump(iJmp)%iClk == iClk) nJmp = nJmp + 1
    ENDDO
!
! Allocate arrays for polynomial fit
! ----------------------------------
    ALLOCATE(ATA(IKF(nPoly+nJmp,nPoly+nJmp)), stat=ios)
    CALL alcerr(ios,'ATA',(/IKF(nPoly+nJmp,nPoly+nJmp)/),'ccextr')
    ALLOCATE(ATY(nPoly+nJmp), stat=ios)
    CALL alcerr(ios,'ATY',(/nPoly+nJmp/),'ccextr')
    ALLOCATE(b(nPoly+nJmp), stat=ios)
    CALL alcerr(ios,'b',(/nPoly+nJmp/),'ccextr')
!
    ALLOCATE(ParLst(nPoly+nJmp),stat=ios)
    CALL alcerr(ios,'ParLst',(/nPoly+nJmp/),'ccextr')
!
    ATA=0d0
    ATY=0d0
    b  =0d0
!
    sumwgt=0D0
    numClk=0
    rmsClk=0
!
! Count the number of clocks in the output file
! ---------------------------------------------
    Sigma0=0D0
    DO iEpo=1,OutClkRec%nEpo
      IF (OutClkRec%Clock(iClk,iEpo) /= unDef .AND.  &
          (OutClkRec%Sigma(iClk,iEpo) /= unDef .AND. &
          DABS(OutClkRec%Sigma(iClk,iEpo)) > null)) THEN
        numClk=numClk+1
        Sigma0=Sigma0+OutClkRec%Sigma(iClk,iEpo)
      ENDIF
    ENDDO
    IF (numClk > 0) THEN
      Sigma0=Sigma0/DBLE(numClk)
    ELSE
      Sigma0=1d0
    ENDIF

!
! Loop all epochs for this clock
! ------------------------------
    DO iEpo=1,OutClkRec%nEpo
      IF (OutClkRec%Clock(iClk,iEpo) /= unDef) THEN

!write(*,*) outclkhead%tfirst+outclkrec%epoch(iEpo)/86400d0,              &
!           OUtclkrec%clock(iclk,iepo)*1E3,Outclkrec%sigma(iclk,iepo)*1E3,&
!           outclkhead%clkname(iclk)(1:4), nint(outclkrec%epoch(iEpo)/300)

!
! Compute ATA and ATY for polynomial fit
! --------------------------------------
        DeltaT = (OutClkRec%Epoch(iEpo) -                                     &
                0.5*OutClkRec%Epoch(OutClkRec%nEpo)-1.5*OutClkRec%Epoch(1)) / &
                OutClkRec%Epoch(OutClkRec%nEpo)
        DeltTT = OutClkRec%Epoch(iEpo) / 86400D0 * 2D0*PI
!
! Set the weight for the clock
! ----------------------------
        clkwgt = 1d0
        IF (DABS(OutClkRec%Sigma(iClk,iEpo)) /= unDef .AND. &
             DABS(OutClkRec%Sigma(iClk,iEpo)) > null ) &
          clkwgt = (Sigma0 / OutClkRec%Sigma(iClk,iEpo))**2
        sumwgt = sumwgt + clkwgt
!
! update ATA matrix
! -----------------
!
! polynom/polynom
        jPoly=1
        DO WHILE (jPoly <= CCopt%IEpoly+1)
          IF (OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 >= &
                                             CCopt%ieTPoly(jPoly)%t(1) .AND. &
              OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 <= &
                                             CCopt%ieTPoly(jPoly)%t(2)) THEN

            kPoly=jPoly
            DO WHILE (kPoly <= CCopt%IEpoly+1)
              IF (OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 >= &
                                               CCopt%ieTPoly(kPoly)%t(1) .AND. &
                  OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 <= &
                                              CCopt%ieTPoly(kPoly)%t(2)) THEN
                IF (jPoly+kPoly-2 > 0) THEN
                  ATA(IKF(jPoly,kPoly)) = ATA(IKF(jPoly,kPoly)) + &
                                          clkwgt * (DeltaT ** (jPoly+kPoly-2))
                ELSE
                  ATA(IKF(jPoly,kPoly)) = ATA(IKF(jPoly,kPoly)) + clkwgt
                ENDIF
              ENDIF
              kPoly=kPoly+1
            ENDDO
!
! polynom/periodic
            iSiCo=1
            kPoly=CCopt%IEpoly+1
            DO WHILE (iSiCo <= CCopt%IEsico)
              IF (OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 >= &
                                               CCopt%ieTsico(iSiCo)%t(1) .AND. &
                  OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 <= &
                                              CCopt%ieTsico(iSiCo)%t(2)) THEN
                IF (jPoly - 1 > 0) THEN
                  ATA(IKF(jPoly,kPoly+2*iSiCo-1)) =     &
                      ATA(IKF(jPoly,kPoly+2*iSiCo-1)) + &
                      clkwgt * DeltaT ** (jPoly-1) *    &
                      DSIN(DeltTT*CCopt%iePsico(iSiCo))
                  ATA(IKF(jPoly,kPoly+2*iSiCo  )) =     &
                      ATA(IKF(jPoly,kPoly+2*iSiCo  )) + &
                      clkwgt * DeltaT ** (jPoly-1) *    &
                      DCOS(DeltTT*CCopt%iePsico(iSiCo))
                ELSE
                  ATA(IKF(jPoly,kPoly+2*iSiCo-1)) =     &
                      ATA(IKF(jPoly,kPoly+2*iSiCo-1)) + &
                      clkwgt * DSIN(DeltTT*CCopt%iePsico(iSiCo))
                  ATA(IKF(jPoly,kPoly+2*iSiCo  )) =     &
                      ATA(IKF(jPoly,kPoly+2*iSiCo  )) + &
                      clkwgt * DCOS(DeltTT*CCopt%iePsico(iSiCo))
                ENDIF
              ENDIF
              iSiCo = iSiCo + 1
            ENDDO
!
          ENDIF ! setup of polynom degree for this epoch
          jPoly=jPoly+1
        ENDDO
!
! periodic/periodic
        iSiCo=1
        jPoly=CCopt%IEpoly+1
        DO WHILE (iSiCo <= CCopt%IEsico)
          IF (OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 >= &
                                           CCopt%ieTsico(iSiCo)%t(1) .AND. &
              OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 <= &
                                          CCopt%ieTsico(iSiCo)%t(2)) THEN
            jSiCo=iSiCo
            DO WHILE (jSiCo <= CCopt%IEsico)
              IF (OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 >= &
                                               CCopt%ieTsico(jSiCo)%t(1) .AND. &
                  OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 <= &
                                              CCopt%ieTsico(jSiCo)%t(2)) THEN
                ATA(IKF(jPoly+2*iSiCo-1,jPoly+2*jSiCo-1))   = &
                        ATA(IKF(jPoly+2*iSiCo-1,jPoly+2*jSiCo-1))  + &
                        clkwgt * DSIN(DeltTT*CCopt%iePsico(iSiCo)) * &
                                 DSIN(DeltTT*CCopt%iePsico(jSiCo))
                ATA(IKF(jPoly+2*iSiCo  ,jPoly+2*jSiCo  ))   = &
                        ATA(IKF(jPoly+2*iSiCo  ,jPoly+2*jSiCo  ))  + &
                        clkwgt * DCOS(DeltTT*CCopt%iePsico(iSiCo)) * &
                                 DCOS(DeltTT*CCopt%iePsico(jSiCo))
                ATA(IKF(jPoly+2*iSiCo-1,jPoly+2*jSiCo  ))   = &
                        ATA(IKF(jPoly+2*iSiCo-1,jPoly+2*jSiCo  ))  + &
                        clkwgt * DSIN(DeltTT*CCopt%iePsico(iSiCo)) * &
                                 DCOS(DeltTT*CCopt%iePsico(jSiCo))
                IF (iSico /= jSiCo) &
                  ATA(IKF(jPoly+2*iSiCo  ,jPoly+2*jSiCo-1))   = &
                          ATA(IKF(jPoly+2*iSiCo  ,jPoly+2*jSiCo-1))  + &
                          clkwgt * DCOS(DeltTT*CCopt%iePsico(iSiCo)) * &
                                   DSIN(DeltTT*CCopt%iePsico(jSiCo))
              ENDIF
              jSiCo = jSiCo + 1
            ENDDO
          ENDIF
          iSiCo = iSiCo + 1
        ENDDO

!
! update ATY vector
        jPoly=1
        DO WHILE (jPoly <= CCopt%IEpoly+1)
          IF (OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 >= &
                                           CCopt%ieTPoly(jPoly)%t(1) .AND. &
              OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 <= &
                                          CCopt%ieTPoly(jPoly)%t(2)) THEN
            IF (jPoly == 1) THEN
              ATY(jPoly) = ATY(jPoly) + clkwgt * OutClkRec%Clock(iClk,iEpo)
            ELSE
              ATY(jPoly) = ATY(jPoly) + &
                           clkwgt * OutClkRec%Clock(iClk,iEpo) * &
                           DeltaT**(jPoly-1)
            ENDIF
          ENDIF
          jPoly=jPoly+1
        ENDDO
        jPoly=jPoly-1
!
        iSiCo=1
        DO WHILE (iSiCo <= CCopt%IEsico)
          IF (OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 >= &
                                           CCopt%ieTsico(iSiCo)%t(1) .AND. &
              OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 <= &
                                           CCopt%ieTsico(iSiCo)%t(2)) THEN
            ATY(jPoly+2*iSiCo-1) = ATY(jPoly+2*iSiCo-1) + &
                      clkwgt * DSIN(DeltTT*CCopt%iePsico(iSiCo)) * &
                      OutClkRec%Clock(iClk,iEpo)
            ATY(jPoly+2*iSiCo  ) = ATY(jPoly+2*iSiCo  ) + &
                      clkwgt * DCOS(DeltTT*CCopt%iePsico(iSiCo)) * &
                      OutClkRec%Clock(iClk,iEpo)
          ENDIF
          iSiCo = iSiCo + 1
        ENDDO

      ENDIF
    ENDDO
!
! Handle the clock jumps
! ----------------------
    IF (nJmp > 0) THEN
      iJump=0
      DO iJmp=1,nJump
        IF (ClkJump(iJmp)%iClk == iClk) THEN
          iJump=iJump+1
          DO iEpo=ClkJump(iJmp)%jEpo,OutClkRec%nEpo
            IF (OutClkRec%Clock(iClk,iEpo) /= unDef) THEN
!
! Modify ATA and ATY for jump estim.
! ----------------------------------
              DeltaT = (OutClkRec%Epoch(iEpo) -                                &
                 0.5*OutClkRec%Epoch(OutClkRec%nEpo)-1.5*OutClkRec%Epoch(1)) / &
                 OutClkRec%Epoch(OutClkRec%nEpo)
              DeltTT = OutClkRec%Epoch(iEpo) / 86400D0 * 2D0*PI
!
! Set the weight for the clock
! ----------------------------
              clkwgt = 1D0
              IF (DABS(OutClkRec%Sigma(iClk,iEpo)) /= unDef .AND. &
                   DABS(OutClkRec%Sigma(iClk,iEpo)) > null ) &
                clkwgt = (Sigma0 / OutClkRec%Sigma(iClk,iEpo))**2
!
! update ATA matrix
! -----------------
              jPoly = CCopt%IEpoly+1+2*CCopt%IEsico+iJump
!
              kPoly=1
              DO WHILE (kPoly <= CCopt%IEpoly+1)
                IF (OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 >= &
                                               CCopt%ieTPoly(kPoly)%t(1) .AND. &
                    OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 <= &
                                              CCopt%ieTPoly(kPoly)%t(2)) THEN
                  IF (kPoly-1 > 0) THEN
                    ATA(IKF(jPoly,kPoly)) = &
                          ATA(IKF(jPoly,kPoly)) + clkwgt * DeltaT ** (kPoly-1)
                  ELSE
                    ATA(IKF(jPoly,kPoly)) = ATA(IKF(jPoly,kPoly)) + clkwgt
                  ENDIF
                ENDIF
                kPoly=kPoly+1
              ENDDO
              kPoly=kPoly-1
!
              iSiCo=1
              DO WHILE (iSiCo <= CCopt%IEsico)
                IF (OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 >= &
                                               CCopt%ieTsico(iSiCo)%t(1) .AND. &
                    OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 <= &
                                              CCopt%ieTsico(iSiCo)%t(2)) THEN
                  ATA(IKF(jPoly,kPoly+2*iSiCo-1)) =    &
                     ATA(IKF(jPoly,kPoly+2*iSiCo-1)) + &
                     DSIN(DeltTT*CCopt%iePsico(iSiCo)) * clkwgt
                  ATA(IKF(jPoly,kPoly+2*iSiCo  )) =    &
                     ATA(IKF(jPoly,kPoly+2*iSiCo  )) + &
                     DCOS(DeltTT*CCopt%iePsico(iSiCo)) * clkwgt
                ENDIF
                iSiCo = iSiCo + 1
              ENDDO
              iSiCo=iSiCo-1
!
              DO lPoly=kPoly+2*iSiCo+1,jPoly
                ATA(IKF(jPoly,lPoly)) = ATA(IKF(jPoly,lPoly)) + clkwgt
              ENDDO
!
! update ATY vector
              jPoly = CCopt%IEpoly+1+2*iSiCo+iJump
              ATY(jPoly) = ATY(jPoly) + clkwgt * OutClkRec%Clock(iClk,iEpo)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
    ENDIF
!
! NEQ-Inversion for poly. fit
    jPoly = CCopt%IEpoly+1+2*CCopt%IEsico+nJmp

    CALL syminvg(jPoly,ATA,0,nSing,ParLst)
    CALL solve (jPoly,ATA,ATY,b)

! Compute the RMS of the fit
! --------------------------
    DO iEpo=1,OutClkRec%nEpo
      IF (OutClkRec%Clock(iClk,iEpo) /= unDef) THEN
        DeltaT = (OutClkRec%Epoch(iEpo) -                                     &
                0.5*OutClkRec%Epoch(OutClkRec%nEpo)-1.5*OutClkRec%Epoch(1)) / &
                OutClkRec%Epoch(OutClkRec%nEpo)
        DeltTT = OutClkRec%Epoch(iEpo) / 86400D0 * 2D0*PI

! polynomial part
        jPoly=1
        kPoly=0
        mRef=0D0
        DO WHILE (jPoly <= CCopt%IEpoly+1)
          IF (OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 >= &
                                           CCopt%ieTPoly(jPoly)%t(1) .AND. &
              OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 <= &
                                           CCopt%ieTPoly(jPoly)%t(2)) THEN
            kPoly = 1
            IF (jPoly == 1) THEN
              mRef = mRef + b(jPoly)
            ELSE
              mRef = mRef + b(jPoly) * DeltaT**(jPoly-1)
            ENDIF
          ENDIF
          jPoly=jPoly+1
        ENDDO
        jPoly=jPoly-1
! periodical part
        iSiCo=1
        DO WHILE (iSiCo <= CCopt%IEsico)
          IF (OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 >= &
                                           CCopt%ieTsico(iSiCo)%t(1) .AND. &
              OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 <= &
                                           CCopt%ieTsico(iSiCo)%t(2)) THEN
            kPoly = 1
            mRef = mRef + b(jPoly+2*iSiCo-1) * DSIN(DeltTT*CCopt%iePsico(iSiCo))
            mRef = mRef + b(jPoly+2*iSiCo  ) * DCOS(DeltTT*CCopt%iePsico(iSiCo))
          ENDIF
          iSiCo = iSiCo + 1
        ENDDO
        iSiCo=iSiCo-1
!
! Add est. clock jump sizes
        IF (nJmp > 0) THEN
          iJump=0
          DO iJmp=1,nJump
            IF (ClkJump(iJmp)%iClk == iClk) THEN
              iJump=iJump+1
              IF (ClkJump(iJmp)%jEpo <= iEpo) THEN
                jPoly = CCopt%IEpoly+1+2*CCopt%IEsico+iJump
                mRef = mRef + b(jPoly)
              ENDIF
            ENDIF
          ENDDO
        ENDIF
!
! Degr. of freedom, RMS comp.
! ---------------------------
        mPoly = CCopt%IEpoly+1 + 2*CCopt%IEsico + nJmp
!
! Set the weight for the clock
! ----------------------------
        clkwgt = 1D0
        IF (DABS(OutClkRec%Sigma(iClk,iEpo)) /= unDef .AND.&
             DABS(OutClkRec%Sigma(iClk,iEpo)) > null ) &
          clkwgt = (Sigma0 / OutClkRec%Sigma(iClk,iEpo))**2
!
        IF (numClk-mPoly > 1 .AND. kPoly == 1)                     &
          rmsClk = rmsClk +                                        &
                   (OutClkRec%Clock(iClk,iEpo)-mRef)**2 * clkwgt / &
                   (numClk-1-mPoly)

!write(*,*) outclkhead%tfirst+outclkrec%epoch(iEpo)/86400d0,          &
!           mRef*1E3,DSQRT(0d0*rmsClk)*1E3,                   &
!           outclkhead%clkname(iclk)(1:4), &
!           nint(outclkrec%epoch(iEpo)/300),'model'

      ENDIF
    ENDDO

!
! Do the inter-/extrapolation
! ---------------------------


!
! Write detailed report
! ---------------------
    IF (CCopt%prtDetail(prtext) == 1) THEN
      IF (DSQRT(rmsClk) < 1D0) THEN
        WRITE(IEsumm(iClk,1),'(A16,4X,3X,A,F8.3,A,I7,A)') &
            OutClkHead%ClkName(iClk),'rms = ',DSQRT(rmsClk)*1D3, &
            ' ns    from ',numClk,' epochs'
      ELSE
        WRITE(IEsumm(iClk,1),'(A16,4X,3X,A,E8.3,A,I7,A)') &
            OutClkHead%ClkName(iClk),'rms = ',DSQRT(rmsClk)*1D3, &
            ' ns    from ',numClk,' epochs'
      ENDIF
      WRITE(IEsumm(iClk,1)(72:95),'(A)')'model was not applied'
! Polynomial part
      iPoly = 1
      jPoly = 0
      DO WHILE (iPoly <= CCopt%IEpoly+1)
        IF (parLst(iPoly) == 0) THEN
          jPoly = jPoly + 1
          i1 = 24+MOD(jPoly-1,4)*28
          i2 = i1+17
          iLine=INT((jPoly-1)/4)+2
!
          IF (DABS(b(iPoly)) < 1D0) THEN
            WRITE(IEsumm(iClk,iLine)(i1:i2),'(A,I2.2,A,F9.3,A)')  &
                    'P',iPoly-1,' =',b(iPoly)*1D3,' +-'
          ELSE
            WRITE(IEsumm(iClk,iLine)(i1:i2),'(A,I2.2,A,E9.2,A)')  &
                    'P',iPoly-1,' =',b(iPoly)*1D3,' +-'
          ENDIF
!
          i1 = 41+MOD(jPoly-1,4)*28
          i2 = i1+8
          IF (DSQRT(ATA(IKF(iPoly,iPoly)) * rmsClk) < 1D0) THEN
            WRITE(IEsumm(iClk,iLine)(i1:i2),'(F8.3)')  &
                  DSQRT(ATA(IKF(iPoly,iPoly)) * rmsClk)*1D3
          ELSE
            WRITE(IEsumm(iClk,iLine)(i1:i2),'(E8.1)')  &
                  DSQRT(ATA(IKF(iPoly,iPoly)) * rmsClk)*1D3
          ENDIF
        ENDIF
        iPoly=iPoly+1
      ENDDO

! periodical part
      iPoly = CCopt%IEpoly + 1
      kPoly = 0
      DO iSiCo=1,CCopt%IEsico
        IF (parLst(iPoly+2*iSiCo) == 0) THEN
          kPoly = kPoly + 1
          i1 = 24+MOD(kPoly-1,4)*28
          i2 = i1+17
          iLine=INT((jPoly-1)/4)+2+INT((kPoly-1)/4)+1
!
          IF (DABS(b(iPoly+2*iSiCo-1)) < 1D0) THEN
            WRITE(IEsumm(iClk,iLine)(i1:i2),'(A,I2.2,A,F9.3,A)')  &
            'S',INT(CCopt%iePsico(iSiCo)),' =',b(iPoly+2*iSiCo-1)*1D3,' +- '
          ELSE
            WRITE(IEsumm(iClk,iLine)(i1:i2),'(A,I2.2,A,E9.2,A)')  &
            'S',INT(CCopt%iePsico(iSiCo)),' =',b(iPoly+2*iSiCo-1)*1D3,' +- '
          ENDIF
!
          i1 = 41+MOD(kPoly-1,4)*28
          i2 = i1+8
          IF (DSQRT(ATA(IKF(iPoly+2*iSiCo-1,iPoly+2*iSiCo-1)) * rmsClk) &
                                                                 < 1D0) THEN
            WRITE(IEsumm(iClk,iLine)(i1:i2),'(F8.3)')  &
                 DSQRT(ATA(IKF(iPoly+2*iSiCo-1,iPoly+2*iSiCo-1)) * rmsClk)*1D3
          ELSE
            WRITE(IEsumm(iClk,iLine)(i1:i2),'(E8.1)')  &
                 DSQRT(ATA(IKF(iPoly+2*iSiCo-1,iPoly+2*iSiCo-1)) * rmsClk)*1D3
          ENDIF
!
          kPoly = kPoly + 1
          i1 = 24+MOD(kPoly-1,4)*28
          i2 = i1+17
          IF (DABS(b(iPoly+2*iSiCo  )) < 1D0) THEN
            WRITE(IEsumm(iClk,iLine)(i1:i2),'(A,I2.2,A,F9.3,A)')  &
            'C',INT(CCopt%iePsico(iSiCo)),' =',b(iPoly+2*iSiCo  )*1D3,' +- '
          ELSE
            WRITE(IEsumm(iClk,iLine)(i1:i2),'(A,I2.2,A,E9.2,A)')  &
            'C',INT(CCopt%iePsico(iSiCo)),' =',b(iPoly+2*iSiCo  )*1D3,' +- '
          ENDIF
!
          i1 = 41+MOD(kPoly-1,4)*28
          i2 = i1+8
          IF (DSQRT(ATA(IKF(iPoly+2*iSiCo  ,iPoly+2*iSiCo  )) * rmsClk) &
                                                                 < 1D0) THEN
            WRITE(IEsumm(iClk,iLine)(i1:i2),'(F8.3)')  &
                 DSQRT(ATA(IKF(iPoly+2*iSiCo  ,iPoly+2*iSiCo  )) * rmsClk)*1D3
          ELSE
            WRITE(IEsumm(iClk,iLine)(i1:i2),'(E8.1)')  &
                 DSQRT(ATA(IKF(iPoly+2*iSiCo  ,iPoly+2*iSiCo  )) * rmsClk)*1D3
          ENDIF
        ENDIF
      ENDDO
    ENDIF
!
! Do the inter/extrapol. for all epochs
! -------------------------------------
    IF ((CCopt%IEmObs == 0 .OR. CCopt%IEmObs <= numClk) .AND. &
        (DSQRT(rmsClk)*1d3 <= CCopt%IEmRMS .OR. CCopt%IEmRMS == 0d0)) THEN
      IF (CCopt%prtDetail(prtext) == 1) &
        WRITE(IEsumm(iClk,1)(72:95),'(A)')'model was applied    '
!
      DO iEpo=1,OutClkRec%nEpo
!
! Interpolate/extrapolate
! -----------------------
        IF ((CCopt%doInter                                             .AND. &
             OutClkHead%TFirst+OutClkRec%Epoch(iEpo)/86400d0 >= &
                                                   CCopt%ClkData%t(1).AND. &
             OutClkHead%TFirst+OutClkRec%Epoch(iEpo)/86400d0 <= &
                                                   CCopt%ClkData%t(2))     &
                                                                          .OR. &
            (CCopt%doExtra                                             .AND. &
             (OutClkHead%TFirst+OutClkRec%Epoch(iEpo)/86400d0 < &
                                                   CCopt%ClkData%t(1) .OR. &
              OutClkHead%TFirst+OutClkRec%Epoch(iEpo)/86400d0 > &
                                                   CCopt%ClkData%t(2)))) THEN
!
! Do not interpolate "between" a jump
! -----------------------------------
          iJmp=0
          DO iJump=1,nJump
            IF (ClkJump(iJump)%iClk == iClk .AND. &
                ClkJump(iJump)%iEpo < iEpo  .AND. &
                ClkJump(iJump)%jEpo > iEpo        ) iJmp=iJump
          ENDDO
!
! Inter/Extrapolate with the polynom
! ----------------------------------
          IF (iJmp == 0 .AND.                                &
              OutClkRec%Clock(iClk,iEpo) == unDef) THEN
            DeltaT = (OutClkRec%Epoch(iEpo) -                                 &
                0.5*OutClkRec%Epoch(OutClkRec%nEpo)-1.5*OutClkRec%Epoch(1)) / &
                OutClkRec%Epoch(OutClkRec%nEpo)
            DeltTT = OutClkRec%Epoch(iEpo) / 86400D0 * 2D0*PI
!
            mRef = 0d0
            sRef = 0d0
            kPoly = 0
            DO jPoly=1,CCopt%IEpoly+1
              IF (OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 >= &
                                               CCopt%ieTPoly(jPoly)%t(1) .AND. &
                  OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 <= &
                                               CCopt%ieTPoly(jPoly)%t(2)) THEN
                kPoly = 1
                IF (jPoly == 1) THEN
                  mRef = mRef + b(jPoly)
                  sRef = sRef + ATA(IKF(jPoly,jPoly))
                ELSE
                  mRef = mRef + b(jPoly) * DeltaT**(jPoly-1)
                  sRef = sRef + ATA(IKF(jPoly,jPoly)) * DeltaT**(2*(jPoly-1))
                ENDIF
              ENDIF
            ENDDO
            jPoly=CCopt%IEpoly+1
!
            iSiCo=1
            DO WHILE (iSiCo <= CCopt%IEsico)
              IF (OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 >= &
                                               CCopt%ieTsico(iSiCo)%t(1) .AND. &
                  OutClkHead%TFirst + OutClkRec%Epoch(iEpo) / 86400D0 <= &
                                              CCopt%ieTsico(iSiCo)%t(2)) THEN
                kPoly = 1
                mRef = mRef + b(jPoly+2*iSiCo-1) * &
                       DSIN(DeltTT*CCopt%iePsico(iSiCo))
                sRef = sRef + &
                       ATA(IKF(jPoly+2*iSiCo-1,jPoly+2*iSiCo-1)) * &
                       DSIN(DeltTT*CCopt%iePsico(iSiCo))**2
                mRef = mRef + b(jPoly+2*iSiCo  ) * &
                       DCOS(DeltTT*CCopt%iePsico(iSiCo))
                sRef = sRef + &
                       ATA(IKF(jPoly+2*iSiCo  ,jPoly+2*iSiCo  )) * &
                       DCOS(DeltTT*CCopt%iePsico(iSiCo))**2
              ENDIF
              iSiCo = iSiCo + 1
            ENDDO
            iSiCo=iSiCo-1
!
! Correct the result with the est. size of a jump
! -----------------------------------------------
            IF (nJmp > 0) THEN
              iJump=0
              DO iJmp=1,nJump
                IF (ClkJump(iJmp)%iClk == iClk) THEN
                  iJump=iJump+1
                  jPoly = CCopt%IEpoly+1+2*CCopt%IEsico+iJump
                  IF (ClkJump(iJmp)%jEpo <= iEpo) THEN
                    mRef = mRef + b(jPoly)
                  ENDIF
                  sRef = sRef + ATA(IKF(jPoly,jPoly))
                ENDIF
              ENDDO
            ENDIF
!
! Write result into the record
! ----------------------------
            IF (kPoly == 1) THEN
              OutClkRec%Clock(iClk,iEpo)=mRef
              OutClkRec%Sigma(iClk,iEpo)=-DSQRT(sref * rmsClk)
!            OutClkRec%Sigma(iClk,iEpo)=-DSQRT(sref) * Sigma0

! write(*,*) outclkhead%tfirst+outclkrec%epoch(iEpo)/86400d0,              &
!            Outclkrec%clock(iclk,iepo)*1E3,DABS(Outclkrec%sigma(iclk,iepo)*1E3),&
!            outclkhead%clkname(iclk)(1:4), nint(outclkrec%epoch(iEpo)/300),'inter'

            ENDIF
!
          ENDIF
        ENDIF
      ENDDO
    ENDIF
!
! Deallocate arrays for polynomial fit
! ------------------------------------
    DEALLOCATE(ATA)
    DEALLOCATE(ATY)
    DEALLOCATE(b)
    DEALLOCATE(ParLst)
!
    IF (numclk == 0) IEsumm(iClk,1) = ''
    iClk = iClk + 1
  ENDDO

!
! PART II
!
! Write the protocol
! ------------------

!
! Report Inter/Extrapolation
! --------------------------
  IF (CCopt%prtDetail(prtext) == 1) THEN
    IF (CCopt%doInter) THEN
      WRITE(lfnprt,'(//,2(1X,A,/))')                                         &
      'RESULTS OF INTERPOLATION / EXTRAPOLATION',                            &
      '----------------------------------------'
    ELSE
      WRITE(lfnprt,'(//,2(1X,A,/))')                                         &
      'RESULTS OF THE CLOCK EXTRAPOLATION',                                  &
      '----------------------------------'
    ENDIF
    WRITE(lfnprt,'(1X,A,3(/,1X,2A))')                                        &
    'Clock name             Rms of pol. fit (ns)',                           &
    '                       ',                                               &
    'Pnn: polynom coefficients of deg. n and sigma (ns/s**n)',               &
    '                       ',                                               &
    'Snn/Cnn: sin/cos with n periods per day and sigma (ns)',                &
    '------------------------------------------------------------------',    &
    '-----------------------------------------------------------------'
    DO iClk = 1,OutClkHead%nSta+OutClkHead%nSat
      IF (LEN_TRIM(IEsumm(iClk,1)) > 0) THEN
        IF (iClk /= 1) WRITE(lfnprt,*)
        DO iLine=1,SIZE(IEsumm,2)
          IF (LEN_TRIM(IEsumm(iClk,iLine)) > 0) &
            WRITE(lfnprt,'(1X,A)') TRIM(IEsumm(iClk,iLine))
        ENDDO
      ENDIF
    ENDDO
    WRITE(lfnprt,'(1X,A,A,/)')                                               &
    '------------------------------------------------------------------',    &
    '-----------------------------------------------------------------'
  ENDIF
!
! Deallocate memory
! -----------------
  DEALLOCATE(IEsumm)
!
  RETURN
  END SUBROUTINE

END MODULE
