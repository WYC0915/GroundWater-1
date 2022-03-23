MODULE s_CCSTAT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE ccstat(CCopt, OutClkHead, OutClkRec, InClkHead, InClkRec, &
                  nJump, clkJump, irCode)

! -------------------------------------------------------------------------
! Purpose:    Do some statistics with the combined clock RINEX files
!
! Author:     R. Dach
!
! Created:    27-Feb-2001
!
! Changes:    05-Jun-2001 RD: Print more than one line in the statistic part
!             18-Jun-2001 RD: Handle case of exact 10 files correct
!             03-Aug-2001 RD: Modified program output
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             01-Feb-2002 RD: Write rms of lin. fit in a file
!             04-Jun-2002 RD: Make the PC version work
!             23-Apr-2003 CU: Nullify local pointers
!             12-Nov-2003 RD: Use SR syminvg instead of SR syming
!             25-Nov-2003 RD: Some POINTER->ALLOCATABLE
!             02-Dec-2003 HB: Check sigma for undef = 999999.999999D0
!             21-Jan-2004 RD: Adapt to new input panel
!             20-Mar-2006 RD: Do not request a clkrec%sigma
!             09-Aug-2010 RD: New syminvg used
!             19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnprt, &
                      lineLength, fileNameLength
  USE d_clkrnx, ONLY: t_clkhead,t_clkrec,unDef
  USE d_stalst, ONLY: t_staList
  USE p_ccrnxc, ONLY: t_ccrnxc_opt,t_Jump,prtres
  USE f_ikf
  USE s_alcerr
  USE s_solve
  USE s_writstsg
  USE s_syminvg
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  TYPE(t_ccrnxc_opt)             :: CCopt      ! Input options for combination
  TYPE(t_clkhead)                :: OutClkHead ! header of output file
  TYPE(t_clkrec)                 :: OutClkRec  ! Combined data
  TYPE(t_clkhead), DIMENSION(:),  &
                   POINTER       :: InClkHead  ! Head of input data
  TYPE(t_clkrec),  DIMENSION(:),  &
                   POINTER       :: InClkRec   ! Input data
  INTEGER(i4b)                   :: nJump      ! Number of clock jumps
  TYPE(t_Jump),    DIMENSION(:),  &
                   POINTER       :: ClkJump    ! Detected clock jumps
! output:
  INTEGER(i4b)                   :: irCode     ! Return code from the sr


! Local Parameters
! ----------------
  INTEGER(i4b),   PARAMETER      :: maxDeg = 3 ! Maximum number of polyn. deg.
                                               ! for statistic: t**0 ... t**2

! Local Variables
! ---------------
  TYPE(t_staList)                :: clkRms

  CHARACTER(LEN=fileNameLength)  :: filNam
  CHARACTER(LEN=lineLength),      &
    DIMENSION(3)                 :: str ! help string
  CHARACTER(LEN=lineLength),      &
    DIMENSION(:,:), ALLOCATABLE  :: sumtxt ! summary text: polynoms

  INTEGER(i4b)                   :: i1,i2  ! Index for writing strings
  INTEGER(i4b)                   :: iFil   ! Counter for files
  INTEGER(i4b)                   :: iClk   ! Counter for clocks
  INTEGER(i4b)                   :: jClk   ! Counter for clocks
  INTEGER(i4b)                   :: iEpo   ! Counter for epochs
  INTEGER(i4b)                   :: iLine  ! Counter for lines
  INTEGER(i4b)                   :: iPoly  ! Counter for deg. of polynom
  INTEGER(i4b)                   :: jPoly  ! Counter for deg. of polynom
  INTEGER(i4b)                   :: kPoly  ! Counter for deg. of polynom
  INTEGER(i4b)                   :: lPoly  ! Counter for deg. of polynom
  INTEGER(i4b)                   :: mPoly  ! Number of polynoms to compute
  INTEGER(i4b)                   :: nPoly  ! Max. poly. deg to compute
  INTEGER(i4b)                   :: nSing  ! Number of sing. elements
  INTEGER(i4b), DIMENSION(:),     &
                ALLOCATABLE      :: ParLst ! List of singular parameters
  INTEGER(i4b)                   :: iJmp   ! Counter for clk jumps
  INTEGER(i4b)                   :: nJmp   ! Number of clk jumps per clk
  INTEGER(i4b)                   :: iJump  ! Index for clk jumps
  INTEGER(i4b)                   :: ios    ! IO status
  INTEGER(i4b), DIMENSION(:),     &
                ALLOCATABLE      :: numClk ! Number of entries for a clk
  INTEGER(i4b), DIMENSION(2)     :: irc    ! RMS for sort order
  INTEGER(i4b)                   :: ircOut

  REAL(r8b),    DIMENSION(maxDeg):: rmsClk ! RMS of polynom fit for a clk
  REAL(r8b),    DIMENSION(2)     :: hlpchg ! RMS for sort order
  REAL(r8b),    DIMENSION(:,:),   &
                ALLOCATABLE      :: ATA    ! Design matrix
  REAL(r8b),    DIMENSION(:,:),   &
                ALLOCATABLE      :: ATY    ! Vect. of obs.
  REAL(r8b),    DIMENSION(:,:),   &
                ALLOCATABLE      :: b      ! Solution vector
  REAL(r8b)                      :: mRef   ! Computing polynom
  REAL(r8b)                      :: DeltaT ! Time term

  LOGICAL                        :: changed! sort order is OK


  irCode = 0

! Get the max. poly. degree requested
! -----------------------------------
  nPoly=maxDeg

! Allocate arrays for the statistic
! ---------------------------------
  ALLOCATE(numClk(SIZE(InclkHead)+1),stat=ios)
  CALL alcerr(ios,'numClk',(/SIZE(InClkHead)+1/),'ccstat')

  iLine=INT((SIZE(InclkHead)-1)/10)+1
  IF (iLine < 1) iLine = 1
  ALLOCATE(sumtxt(OutClkHead%nSta+OutClkHead%nSat+1,iLine), stat=ios)
  CALL alcerr(ios,'sumtxt',(/OutClkHead%nSta+OutClkHead%nSat+1,iLine/),'ccstat')
  sumtxt=''

! Init station list array for rms of lin. fit output
! --------------------------------------------------
  clkRms%title = CCopt%title
  clkRms%nSta  = OutClkHead%nSta+OutClkHead%nSat

  ALLOCATE(clkRms%staNam(clkRms%nSta),stat=ios)
  CALL alcerr(ios,'clkRms%staNam',(/clkRms%nSta/),'ccstat')

  ALLOCATE(clkRms%sigma(1,clkRms%nSta),stat=ios)
  CALL alcerr(ios,'clkRms%sigma',(/1,clkRms%nSta/),'ccstat')

  clkRms%staNam(1:clkRms%nSta) = OutClkHead%clkName(1:clkRms%nSta)
  clkRms%sigma  = undef

! Get the statistics for all clocks
! ---------------------------------
  iClk = 1
  DO WHILE (iClk<=OutClkHead%nSta+OutClkHead%nSat)
    numClk=0
    rmsClk=0

! A clock jump detected for this clock?
! -------------------------------------
    nJmp=0
    DO iJmp=1,nJump
      IF (clkJump(iJmp)%iClk == iClk) nJmp=nJmp + 1
    ENDDO

! Allocate arrays for polynomial fit
! ----------------------------------
    ALLOCATE(ATA(maxDeg,IKF(nPoly+nJmp,nPoly+nJmp)), stat=ios)
    CALL alcerr(ios,'ATA',(/maxDeg,IKF(nPoly+nJmp,nPoly+nJmp)/),'ccstat')
    ALLOCATE(ATY(maxDeg,nPoly+nJmp), stat=ios)
    CALL alcerr(ios,'ATY',(/maxDeg,nPoly+nJmp/),'ccstat')
    ALLOCATE(b(maxDeg,nPoly+nJmp), stat=ios)
    CALL alcerr(ios,'b',(/maxDeg,nPoly+nJmp/),'ccstat')

    ALLOCATE(ParLst(nPoly+nJmp),stat=ios)
    CALL alcerr(ios,'ParLst',(/nPoly+nJmp/),'ccstat')

    ATA=0d0
    ATY=0d0
    b  =0d0

! Count the number of clocks in the input files
! ---------------------------------------------
    DO iFil=1,SIZE(InClkHead)
      DO jClk=1,InClkHead(iFil)%nSta+InClkHead(iFil)%nSat
        IF (OutClkHead%ClkName(iClk) == InClkHead(iFil)%ClkName(jClk)) THEN
          DO iEpo=1,InClkRec(iFil)%nEpo
            IF (InClkRec(iFil)%Clock(jClk,iEpo) /= unDef) THEN
              numClk(iFil+1)=numClk(iFil+1)+1
            ENDIF
          ENDDO
        ENDIF
      ENDDO
    ENDDO

! Count the number of clocks in the output file
! ---------------------------------------------
    DO iEpo=1,OutClkRec%nEpo
      IF (OutClkRec%Clock(iClk,iEpo) /= unDef ) THEN
        numClk(1)=numClk(1)+1
      ENDIF
    ENDDO


! Loop all epochs for this clock
! ------------------------------
    DO iEpo=1,OutClkRec%nEpo
      IF (OutClkRec%Clock(iClk,iEpo) /= unDef ) THEN

! Compute ATA and ATY for polynomial fit
! --------------------------------------
        DeltaT = (OutClkRec%Epoch(iEpo) -                                     &
                0.5*OutClkRec%Epoch(OutClkRec%nEpo)-1.5*OutClkRec%Epoch(1)) / &
                OutClkRec%Epoch(OutClkRec%nEpo)
        DO iPoly=1,maxDeg

! update ATA matrix
          jPoly=1
          DO WHILE (jPoly <= iPoly)
            kPoly=jPoly
            DO WHILE (kPoly <= iPoly)
              IF (jPoly+kPoly-2 > 0) THEN
                ATA(iPoly,IKF(jPoly,kPoly)) = ATA(iPoly,IKF(jPoly,kPoly)) + &
                                              (DeltaT ** (jPoly+kPoly-2))
              ELSE
                ATA(iPoly,IKF(jPoly,kPoly)) = ATA(iPoly,IKF(jPoly,kPoly)) + 1D0
              ENDIF
              kPoly=kPoly+1
            ENDDO
            jPoly=jPoly+1
          ENDDO

! update ATY vector
          jPoly=1
          DO WHILE (jPoly <= iPoly)
            IF (jPoly == 1) THEN
              ATY(iPoly,jPoly) = ATY(iPoly,jPoly) + OutClkRec%Clock(iClk,iEpo)
            ELSE
              ATY(iPoly,jPoly) = ATY(iPoly,jPoly) + &
                                 OutClkRec%Clock(iClk,iEpo) * DeltaT**(jPoly-1)
            ENDIF
            jPoly=jPoly+1
          ENDDO
        ENDDO
      ENDIF
    ENDDO

! Handle the clock jumps
! ----------------------
    IF (nJmp > 0) THEN
      iJump=0
      DO iJmp=1,nJump
        IF (ClkJump(iJmp)%iClk == iClk) THEN
          iJump=iJump+1
          DO iEpo=ClkJump(iJmp)%jEpo,OutClkRec%nEpo
            IF (OutClkRec%Clock(iClk,iEpo) /= unDef ) THEN

! Modify ATA and ATY for jump estim.
! ----------------------------------
              DeltaT = (OutClkRec%Epoch(iEpo) -                                &
                 0.5*OutClkRec%Epoch(OutClkRec%nEpo)-1.5*OutClkRec%Epoch(1)) / &
                 OutClkRec%Epoch(OutClkRec%nEpo)
              DO iPoly=1,maxDeg

! update ATA matrix
                jPoly=iPoly+iJump

                kPoly=1
                DO WHILE (kPoly <= iPoly)
                  IF (kPoly-1 > 0) THEN
                    ATA(iPoly,IKF(jPoly,kPoly)) = &
                        ATA(iPoly,IKF(jPoly,kPoly)) + DeltaT ** (kPoly-1)
                  ELSE
                    ATA(iPoly,IKF(jPoly,kPoly)) = &
                        ATA(iPoly,IKF(jPoly,kPoly)) + 1D0
                  ENDIF
                  kPoly=kPoly+1
                ENDDO
                DO lPoly=kPoly,jPoly
                  ATA(iPoly,IKF(jPoly,lPoly)) = &
                      ATA(iPoly,IKF(jPoly,lPoly)) + 1D0
                ENDDO

! update ATY vector
                jPoly=iPoly+iJump

                ATY(iPoly,jPoly) = ATY(iPoly,jPoly) + OutClkRec%Clock(iClk,iEpo)
              ENDDO
            ENDIF
          ENDDO
        ENDIF
      ENDDO
    ENDIF

! NEQ-Inversion for poly. fit
    DO iPoly = 1,maxDeg
      jPoly = iPoly+nJmp
!
!do kpoly=1,jpoly
!  write(*,*) iClk, nJmp, (ata(ipoly,ikf(ios,kpoly)),ios=1,jpoly), aty(ipoly,kpoly)
!enddo
!
      CALL syminvg(jPoly,ATA(iPoly,:),0,nSing,ParLst)
      CALL solve  (jPoly,ATA(iPoly,:),ATY(iPoly,:),b(iPoly,:))
    ENDDO

! Compute the RMS of the fit
    DO iEpo=1,OutClkRec%nEpo
      IF (OutClkRec%Clock(iClk,iEpo) /= unDef ) THEN
        DeltaT = (OutClkRec%Epoch(iEpo) -                                     &
                0.5*OutClkRec%Epoch(OutClkRec%nEpo)-1.5*OutClkRec%Epoch(1)) / &
                OutClkRec%Epoch(OutClkRec%nEpo)
        DO iPoly=1,maxDeg
          jPoly=1
          mRef=b(iPoly,jPoly)
          jPoly=jPoly+1
          DO WHILE (jPoly <= iPoly)
            mRef = mRef + b(iPoly,jPoly) * DeltaT**(jPoly-1)
            jPoly=jPoly+1
          ENDDO

! Add est. clock jump sizes
          IF (nJmp > 0) THEN
            iJump=0
            DO iJmp=1,nJump
              IF (ClkJump(iJmp)%iClk == iClk) THEN
                iJump=iJump+1
                IF (ClkJump(iJmp)%jEpo <= iEpo) THEN
                  jPoly=iPoly+iJump
                  mRef = mRef + b(iPoly,jPoly)
                ENDIF
              ENDIF
            ENDDO
          ENDIF

! Degr. of freedom, RMS comp.
! ---------------------------
          mPoly = iPoly + nJmp

          IF (numClk(1)-mPoly > 1)                                          &
            rmsClk(iPoly) = rmsClk(iPoly) +                                 &
                   (OutClkRec%Clock(iClk,iEpo)-mRef)**2 / (numClk(1)-1-mPoly)
        ENDDO
      ENDIF
    ENDDO

! Store RMS of linear fit
! -----------------------
    IF (maxDeg >= 2) THEN
      IF (rmsClk(2) > 0d0) THEN
        clkRms%sigma(1,iClk) = DSQRT(rmsClk(2))
      ENDIF
    ENDIF


! Write results into the output string
! ------------------------------------
    sumtxt(iClk,1)=TRIM(OutClkHead%ClkName(iClk))
    IF (nJmp > 0) WRITE(sumtxt(iClk,1)(20:20),'(A)') '*'

    jClk = 0
    DO iFil=1,SIZE(InClkHead)+1
      i1=29+mod(iFil-2,10)*7
      i2=36+mod(iFil-2,10)*7
      iLine=INT((iFil-2)/10)+1
      IF (iFIl == 1) THEN
        i1=22+0*7
        i2=29+0*7
        iLine=1
      ENDIF
      WRITE(sumtxt(iClk,iLine)(i1:i2),'(I7)') numClk(iFil)
      jClk = jClk + numClk(iFil)
    ENDDO

    DO ipoly=1,maxDeg
      IF (iPoly <= maxDeg) THEN
        i1=25+SIZE(InClkHead)*7+iPoly*9
        i2=34+SIZE(InClkHead)*7+iPoly*9
        IF (10 < SIZE(InClkHead)) THEN
        i1=25+10*7+iPoly*9
        i2=34+10*7+iPoly*9
        ENDIF
        IF (rmsClk(iPoly) == 0d0) THEN
          WRITE(sumtxt(iClk,1)(i1:i2),'(2X,A)') '---'
        ELSE IF (DSQRT(rmsClk(iPoly)) < 1d0) THEN
          WRITE(sumtxt(iClk,1)(i1:i2),'(2X,F7.3)') DSQRT(rmsClk(iPoly))*1d3
        ELSE
          WRITE(sumtxt(iClk,1)(i1:i2),'(2X,E7.1)') DSQRT(rmsClk(iPoly))*1d3
        ENDIF
      ENDIF
    ENDDO

    IF (jClk == 0) sumtxt(iClk,1) = ''

! Deallocate arrays for polynomial fit
! ------------------------------------
    DEALLOCATE(ATA)
    DEALLOCATE(ATY)
    DEALLOCATE(b)
    DEALLOCATE(ParLst)

    iClk = iClk + 1
  ENDDO

! Put the RMS of lin. fit into the file
! -------------------------------------
  CALL gtflna(0,'LINFITRS',filNam,ircOut)

  IF (ircOut == 0 .AND. LEN_TRIM(filNam) > 0) THEN
    CALL writstsg(filNam,1,clkRms)
  ENDIF

  DEALLOCATE(clkRms%sigma,stat=ios)
  DEALLOCATE(clkRms%staNam,stat=ios)

!
! PART II
!
! Write the protocol
! ------------------


! Write the statistic of the combination
! --------------------------------------
  WRITE(lfnprt,'(/,2(/,1X,A),//,1X,A,I8,/)')                                 &
  'SUMMARY OF COMBINATION COMPUTATION',                                      &
  '----------------------------------',                                      &
  'Number of intervals:                    ',CCopt%nInterval
  IF (CCopt%nInterval > 0) THEN
    WRITE(lfnprt,                                                            &
    '(1X,A,I8,/,2(/,1X,A,F12.3,A,/),1X,5A,//,1X,A,I8,/,1X,5A,/)')            &
    'Number of clocks removed as outlier:    ',CCopt%NumDel,                 &
    'Mean rms for the offset computation:    ',                              &
                                   CCopt%SumRMS/CCopt%nInterval,' ns',       &
    'Max. rms for the offset computation:    ',CCopt%MaxRMS,' ns',           &
    '     (',CCopt%MaxRMSEpo(1),' to ',CCopt%MaxRMSEpo(2),')',               &
    'Max. number of removed observations:    ',CCopt%MaxDel,                 &
    '     (',CCopt%MaxDelEpo(1),' to ',CCopt%MaxDelEpo(2),')'
  ENDIF
  IF (CCopt%nJmpSig > 0)                                                     &
    WRITE(lfnprt,'(1X,A,I8,/)')                                              &
         'Number of jumps detected:               ',nJump

! Write the statistic/clock characteristic for the combined solution
! ------------------------------------------------------------------
  IF (CCopt%prtDetail(prtres) >= 1) THEN
    WRITE(lfnprt,'(/2(/,1X,A))')                     &
    'STATISTICS ON THE CLOCKS IN THE OUTPUT FILE',   &
    '-------------------------------------------'

! Construct the header of the table
! ---------------------------------
    str(1)='                         # per file'
    str(2)='Clock name               out'
    str(3)='----------------------------'
    DO iFil=1,SIZE(InClkHead)
      IF (iFil <= 10) THEN
        i1=22+iFil*7
        i2=29+iFil*7
        WRITE(str(2)(i1:i2),'(4X,I3.3)') iFil
        WRITE(str(3)(i1:i2),'(A7)') '-------'
      ENDIF
    ENDDO

    i1=38+SIZE(InClkHead)*7
    i2=61+SIZE(InClkHead)*7
    IF (10 < SIZE(InClkHead)) THEN
      i1=38+10*7
      i2=61+10*7
    ENDIF
    WRITE(str(1)(i1:i2),'(A21)') 'rms of poly. fit (ns)'

    i1=29+SIZE(InClkHead)*7
    i2=33+SIZE(InClkHead)*7
    IF (10 < SIZE(InClkHead)) THEN
      i1=29+10*7
      i2=33+10*7
    ENDIF
    WRITE(str(3)(i1:i2),'(A5)') '-----'

    DO iPoly=1,maxDeg
      i1=25+SIZE(InClkHead)*7+iPoly*9
      i2=34+SIZE(InClkHead)*7+iPoly*9
      IF (10 < SIZE(InClkHead)) THEN
        i1=25+10*7+iPoly*9
        i2=34+10*7+iPoly*9
      ENDIF
      WRITE(str(2)(i1:i2),'(4X,A3,I2)') 'n =',iPoly-1
      WRITE(str(3)(i1:i2),'(A9)') '---------'
    ENDDO
    DO iLine=1,2
      WRITE(lfnprt,'(1X,A)') TRIM(str(iLine))
    ENDDO
    str(2)=''
    DO iFil=11,SIZE(InClkHead)
      i1=29+MOD(iFil-1,10)*7
      i2=36+MOD(iFil-1,10)*7
      WRITE(str(2)(i1:i2),'(4X,I3.3)') iFil
      IF (MOD(iFil,10) == 0 .OR. iFil == SIZE(InClkHead)) THEN
        WRITE(lfnprt,'(1X,A)') TRIM(str(2))
        str(2) = ''
      ENDIF
    ENDDO
    WRITE(lfnprt,'(1X,A)') TRIM(str(3))

! Sort the statistic report
! -------------------------
    IF (CCopt%prtDetail(prtres) == 2) THEN
      i1=1
      i2=20
    ELSE IF (CCopt%PrtDetail(prtres) == 3) THEN
      i1=25+SIZE(InClkHead)*7+2*9
      i2=34+SIZE(InClkHead)*7+2*9
      IF (10 < SIZE(InClkHead)) THEN
        i1=25+10*7+2*9
        i2=34+10*7+2*9
      ENDIF
    ELSE
      i1=0
      i2=0
    ENDIF

    changed = .TRUE.
    DO WHILE (i1+i2 > 0 .AND. changed)
      changed = .FALSE.
      DO iClk=1,OutClkHead%nSta-1
        DO jClk=1,2
          READ(sumtxt(iClk+jClk-1,1)(i1:i2),*,iostat=irc(jClk)) hlpchg(jClk)
        ENDDO
        IF ((irc(1)+irc(2) == 0 .AND. hlpchg(1) > hlpchg(2)) .OR. &
            (irc(1)+irc(2) /= 0 .AND. &
             sumtxt(iClk,1)(i1:i2) > sumtxt(iClk+1,1)(i1:i2))) THEN
          changed = .TRUE.
          sumtxt(SIZE(sumtxt,1),:) = sumtxt(iClk+1,:)
          sumtxt(iClk+1,:)         = sumtxt(iClk,:)
          sumtxt(iClk,:)           = sumtxt(SIZE(sumtxt,1),:)
        ENDIF
      ENDDO

      DO iClk=OutClkHead%nSta+1,OutClkHead%nSta+OutClkHead%nSat-1
        DO jClk=1,2
          READ(sumtxt(iClk+jClk-1,1)(i1:i2),*,iostat=irc(jClk)) hlpchg(jClk)
        ENDDO
        IF ((irc(1)+irc(2) == 0 .AND. hlpchg(1) > hlpchg(2)) .OR. &
            (irc(1)+irc(2) /= 0 .AND. &
             sumtxt(iClk,1)(i1:i2) > sumtxt(iClk+1,1)(i1:i2))) THEN
          changed = .TRUE.
          sumtxt(SIZE(sumtxt,1),:) = sumtxt(iClk+1,:)
          sumtxt(iClk+1,:)         = sumtxt(iClk,:)
          sumtxt(iClk,:)           = sumtxt(SIZE(sumtxt,1),:)
        ENDIF
      ENDDO
    ENDDO

! Print the statistic part
! ------------------------
    DO iClk=1,SIZE(sumtxt,1)-1
      IF (LEN_TRIM(sumtxt(iClk,1)) > 0) THEN
        IF (iClk /= 1 .AND. SIZE(sumtxt,2) > 1) WRITE(lfnprt,*)
        DO iLine=1,SIZE(sumtxt,2)
          WRITE(lfnprt,'(1X,A)') TRIM(sumtxt(iClk,iLine))
        ENDDO
      ENDIF
    ENDDO
    WRITE(lfnprt,'(1X,A)') TRIM(str(3))
    IF (CCopt%nJmpSig > 0) &
      WRITE(lfnprt,'(1X,A)') 'Stations with clock jumps are flaged with "*"'
    WRITE(lfnprt,*)
  ENDIF

! Deallocate memory
! -----------------
  DEALLOCATE(numClk)
  DEALLOCATE(sumtxt)

  RETURN
  END SUBROUTINE

END MODULE
