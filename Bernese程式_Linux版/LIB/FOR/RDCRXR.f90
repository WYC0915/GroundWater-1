MODULE s_RDCRXR
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE rdcrxr(lfnclk,lfnmsg,TimeWin,ClkHead,ClkRec,irCode)

! -------------------------------------------------------------------------
! Purpose:    read  observation records of a rinex clock file (version 2.00)
!             (based on the f77 sr R2RDCR)
!
! Parameters:
!         in: lfnclk  : Logical file number of clock rinex file         i4b
!             lfnmsg  : Logical file number for error messages          i4b
!             ClkHead : Information from rinex file header           t_clkhead
!             ClkRec  : Data records from rinex file                 t_clkrec
!             irCode  : return code                                     i4b
!                         0: ok
!                         2: not anticipated version number
!                         8: data type not supported
!                         9: read error
!
! Author:     R. Dach
!
! Created:    09-Aug-2000
! Last mod.:  13-Mar-2012
!
! Changes:    14-Feb-2001 RD: Use SR ALCERR
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             25-Sep-2002 HU: Remove i_astlib
!             25-Nov-2003 HB: Read sigma only if existing
!             21-Sep-2009 RD: Eclipsing flag added
!             19-Aug-2011 RD: Allow to read version 3.0
!             13-Mar-2012 HB: Reading error corrected
!
! SR used:    djul, upperc, sjustl, alcerr
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_clkrnx, ONLY: t_clkhead,t_clkrec,unDef

  USE s_alcerr
  USE f_djul
  USE s_sjustl
  USE s_upperc
  USE s_setflg
  USE s_clrflg
  IMPLICIT NONE
!
! Variables from parameter list
! -----------------------------
  INTEGER(i4b)      :: lfnclk         ! Logical file number of clock rinex file
  INTEGER(i4b)      :: lfnmsg         ! Logical file number for error messages
  REAL(r8b)        , DIMENSION(2)     &
                    :: TimeWin        ! Time window to handle in one run
  TYPE(t_clkhead)   :: ClkHead        ! Clock rinex header
  TYPE(t_clkrec)    :: ClkRec         ! Clock rinex data records
  INTEGER(i4b)      :: irCode         ! Return code of this SR
!
! Local variables
! ---------------
  CHARACTER(len=90) :: string         ! values of a record line
  CHARACTER(len= 8) :: Flag
  CHARACTER(len= 4) :: Name           ! Name of the clock (Sat or Sta)
  CHARACTER(len= 2) :: DType          ! Data type ID
!
  INTEGER(i4b)      :: iEpo           ! Counts the epoch numbers in list
  INTEGER(i4b)      :: iyyy,mm,id     ! Date of a record (year, month, day)
  INTEGER(i4b)      :: ih,im          ! Date of a record (hour, min.)
  INTEGER(i4b)      :: iVal           ! Counts the values in the record
  INTEGER(i4b)      :: nVal           ! # of values in the record
  INTEGER(i4b)      :: iSta           ! Counts the stations in list
  INTEGER(i4b)      :: iClk
  INTEGER(i4b)      :: nClk           ! number of clocks in record
  INTEGER(i4b)      :: iTim           ! A counter
  INTEGER(i4b)      :: ii
  INTEGER(i4b)      :: ioRead         ! I/O status for reading operations
  INTEGER(i4b)      :: ios            ! io status
!
  REAL(r8b)         :: EpoTime           ! Epoch of the current line (sec since TFirst)
  REAL(r8b)         :: TimSav         ! Epoch index for compiling the records
  REAL(r8b)         :: sec            ! Date of a record (sec.)
  REAL(r8b)        , DIMENSION(6)     &
                    :: Value          ! One data record of values
  REAL(r8b)         :: Day            ! Day of month of a record
  REAL(r8b)         :: T              ! MJD of the record (full day only)
!
  TYPE(t_clkrec)    :: HlpClkRec      ! Clock rinex data record
!
! MAXIMUM RINEX FORMAT VERSION
! ----------------------------
  REAL(r8b)        , PARAMETER :: rxVers = 3.00D0
!
! INITIALIZE
! ----------
  irCode=0
  TimSav=-9000D0
  iEpo=0
  ClkRec%nEpo=0
  nClk=ClkHead%nSta+ClkHead%nSat
!
  ALLOCATE(ClkRec%Epoch(30),stat=ios)
  CALL alcerr(ios,'ClkRec%Epoch',(/30/),'rdcrxr')
  ALLOCATE(ClkRec%Clock(nClk,30),stat=ios)
  CALL alcerr(ios,'ClkRec%Clock',(/nClk,30/),'rdcrxr')
  ALLOCATE(ClkRec%Sigma(nClk,30),stat=ios)
  CALL alcerr(ios,'ClkRec%Sigma',(/nClk,30/),'rdcrxr')
  ALLOCATE(ClkRec%clkFlg(nClk,30),stat=ios)
  CALL alcerr(ios,'ClkRec%clkFlg',(/nClk,30/),'rdcrxr')
!
! TEST RINEX VERSION
! ------------------
  IF (ClkHead%RnxVers <= 0.0 .OR. ClkHead%RnxVers > rxVers) irCode=2
!
! READ ONE RECORD
! ---------------
  RecLoop: DO
    IF (irCode /= 0) EXIT RecLoop
    READ(LFNCLK,'(A)',iostat=ioRead) string
!
    IF (ioRead > 0) irCode=9
    IF (ioRead < 0) EXIT RecLoop
    IF (LEN_TRIM(string) == 0) EXIT RecLoop
    IF (ioRead == 0) THEN
      READ(string,'(A2,1X,A4,1X,I4,4I3,F10.6,I3)',iostat=ioRead) &
          DType,Name,iyyy,mm,id,ih,im,sec,nVal
      IF (nVal == 1) THEN
        READ(string,'(40X,E19.12)',iostat=ioRead) Value(1)
        Value(2) = 0.D0
      ELSEIF (nVal >= 2) THEN
        READ(string,'(40X,E19.12,1X,E19.12)',iostat=ioRead) &
             (Value(iVal),iVal=1,2)
      ENDIF
      IF (ioRead /= 0) irCode=9
      flag = string(82:89)
      IF (irCode == 0 .AND. nVal > 2) THEN
        READ(lfnclk,'(A)',iostat=ioRead) string
        IF (ioRead > 0) irCode=9
        IF (ioRead < 0) EXIT RecLoop
        IF (ioRead == 0) THEN
          READ(string,'(E19.12,1X,E19.12,1X,E19.12,1X,E19.12)',iostat=ioRead) &
              (Value(iVal),iVal=3,6)
          IF (ioRead /= 0) irCode=9
        ENDIF
      ENDIF
    ENDIF
    CALL upperc(DTYPE)
    CALL sjustl(DTYPE)
    CALL upperc(NAME)
    CALL sjustl(NAME)
!
! CHECK IF TIME INSIDE OF TIME WINDOW
! -----------------------------------
    day=id*1.D0
    T=DJUL(iyyy,mm,day+(ih+(im+sec/60d0)/60d0)/24d0)
    IF (TimeWin(1) /= 0D0 .AND. T < TimeWin(1)) CYCLE RecLoop
    IF (TimeWin(2) /= 0D0 .AND. T > TimeWin(2)) EXIT  RecLoop
!
    IF (ClkHead%TFirst == 0.0) ClkHead%TFirst=DINT(T)
    EpoTime = (T-ClkHead%TFirst) * 86400D0
!
! DETERMINE EPOCH
! ---------------
    IF (irCode /= 0) CYCLE RecLoop
    IF (EpoTime > TimSav) THEN
      iEpo=iEpo+1
!
! Epoch arrays have to be extented
! --------------------------------
      IF (iEpo>SIZE(ClkRec%Epoch)) THEN
        ALLOCATE(HlpClkRec%Epoch(iEpo-1),stat=ios)
        CALL alcerr(ios,'HlpClkRec%Epoch',(/iEpo-1/),'rdcrxr')
        ALLOCATE(HlpClkRec%Clock(nClk,iEpo-1),stat=ios)
        CALL alcerr(ios,'HlpClkRec%Clock',(/nClk,iEpo-1/),'rdcrxr')
        ALLOCATE(HlpClkRec%Sigma(nClk,iEpo-1),stat=ios)
        CALL alcerr(ios,'HlpClkRec%Sigma',(/nClk,iEpo-1/),'rdcrxr')
        ALLOCATE(HlpClkRec%clkFlg(nClk,iEpo-1),stat=ios)
        CALL alcerr(ios,'HlpClkRec%clkFlg',(/nClk,iEpo-1/),'rdcrxr')
!
        HlpClkRec%Epoch(:)=ClkRec%Epoch(:)
        HlpClkRec%Clock(:,:)=ClkRec%Clock(:,:)
        HlpClkRec%Sigma(:,:)=ClkRec%Sigma(:,:)
        HlpClkRec%clkFlg(:,:)=ClkRec%clkFlg(:,:)
!
        DEALLOCATE(ClkRec%Epoch)
        DEALLOCATE(ClkRec%Clock)
        DEALLOCATE(ClkRec%Sigma)
        DEALLOCATE(ClkRec%clkFlg)
!
        ALLOCATE(ClkRec%Epoch(iEpo+9),stat=ios)
        CALL alcerr(ios,'ClkRec%Epoch',(/iEpo+9/),'rdcrxr')
        ALLOCATE(ClkRec%Clock(nClk,iEpo+9),stat=ios)
        CALL alcerr(ios,'ClkRec%Clock',(/nClk,iEpo+9/),'rdcrxr')
        ALLOCATE(ClkRec%Sigma(nClk,iEpo+9),stat=ios)
        CALL alcerr(ios,'ClkRec%Sigma',(/nClk,iEpo+9/),'rdcrxr')
        ALLOCATE(ClkRec%clkFlg(nClk,iEpo+9),stat=ios)
        CALL alcerr(ios,'ClkRec%clkFlg',(/nClk,iEpo+9/),'rdcrxr')
!
        ClkRec%Epoch(1:iEpo-1)=HlpClkRec%Epoch(:)
        ClkRec%Clock(1:nClk,1:iEpo-1) =HlpClkRec%Clock(1:nClk,1:iEpo-1)
        ClkRec%Sigma(1:nClk,1:iEpo-1) =HlpClkRec%Sigma(1:nClk,1:iEpo-1)
        ClkRec%clkFlg(1:nClk,1:iEpo-1)=HlpClkRec%clkFlg(1:nClk,1:iEpo-1)
!
        DEALLOCATE(HlpClkRec%Epoch)
        DEALLOCATE(HlpClkRec%Clock)
        DEALLOCATE(HlpClkRec%Sigma)
        DEALLOCATE(HlpClkRec%clkFlg)
      ENDIF
!
      ClkRec%Epoch(iEpo)=EpoTime
      TimSav=EpoTime
!
! INITIALIZE ALL STATION AND SATELLITE CLOCKS (unDef=999999.999999)
! -----------------------------------------------------------------
      ClkRec%Clock(1:nClk,iEpo)=unDef
      ClkRec%Sigma(1:nClk,iEpo)=unDef
      DO iClk = 1,nClk
        DO ii = 0,7
          CALL clrflg(clkRec%clkFlg(iClk,iEpo),ii)
        ENDDO
      ENDDO
    ENDIF
!
! FIND STATION INDEX
! ------------------
    IF (DTYPE == 'AR' .OR. DTYPE == 'AS') THEN
      DO iSta=1,nClk
        IF (Name == ClkHead%ClkName(iSta)(1:4)) THEN
          ClkRec%Clock(iSta,iEpo)=Value(1)*1D6
          IF (Value(2) /= 0.D0) THEN
            ClkRec%Sigma(iSta,iEpo)=Value(2)*1D6
          ENDIF
          DO ii = 1,8
            IF (flag(ii:ii) /= ' ') CALL setFlg(clkrec%clkFLg(iSta,iEpo),ii-1)
          ENDDO
          CYCLE RecLoop
        ENDIF
      ENDDO
!
! EOF (from GFZ CLOCK FILES)
! --------------------------
    ELSE IF (DTYPE == 'EO') THEN
      EXIT RecLoop
!
! DATA TYPE NOT SUPPORTED
! -----------------------
    ELSE
      irCode=8
    ENDIF
!
! NEXT RECORD
! -----------
  END DO RecLoop

!
! NOT ANTICIPATED VERSION NUMBER
! ------------------------------
  IF (irCode == 2)                                                  &
    WRITE(lfnmsg,'(/,A,2(/,16X,A,F5.2),//)')                        &
          ' ### SR RDCRXR: INCORRECT RINEX VERSION NUMBER',         &
                          'EXPECTED NUMBER: ',RXVERS,               &
                          'DETECTED NUMBER: ',ClkHead%RnxVers
!
! EPOCH NOT CORRECT
! -----------------
  IF (irCode == 7)                                                       &
      WRITE(lfnmsg,'(/,A,2(/,16X,A,2F14.6),/,16X,A,F14.6,//)')           &
            ' ### SR RDCRXR: EPOCH OUTSIDE TIME WINDOW ',                &
                            'TFIRST AND T   : ',ClkHead%TFirst,T,        &
                            'EXPECTED WINDOW: ',(TimeWin(iTim),iTim=1,2),&
                            'DETECTED EPOCH : ',                         &
                                ClkHead%TFirst+ClkRec%Epoch(iEpo)/86400D0
!
! DATA TYPE NOT SUPPORTED
! -----------------------
  IF (irCode == 8)                                                  &
    WRITE(lfnmsg,'(/,A,/,16X,A,A,//)')                              &
          ' ### SR RDCRXR: DATA TYPE NOT SUPPORTED:',               &
                          'TYPE: ',DTYPE
!
! ERROR READING FILE
! ------------------
  IF (irCode == 9)                                                  &
      WRITE(lfnmsg,'(/,A,//)')                                      &
            ' ### SR RDCRXR: ERROR READING CLOCK RINEX FILE'
!
  ClkRec%nEpo=iEpo
!
  RETURN
  END SUBROUTINE

END MODULE
