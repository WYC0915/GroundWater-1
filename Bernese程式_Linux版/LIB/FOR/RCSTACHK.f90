MODULE s_RCSTACHK
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rcstachk(opt, nSta, rmssta, rmstot, nBad, BadSta, irCode)

! -------------------------------------------------------------------------
! Purpose:    Checks RESRMS summary for bad stations for RESCHK
!
! Parameters:
!       in:   opt       : input options                            t_reschk_opt
!             nSta      : number of station in list                i4b
!             rmssta    : station rms information record           t_rmssta(*)
!       out:  nBad      : number of bad stations in list           i4b
!             BadSta    : List of bad stations (index)             i4b(*)
!             irCode    : return code of this SR                   i4b
!
!
! Author:     R. Dach
!
! Created:    04-Dec-2000
! Last mod.:  18-Aug-2010
!
! Changes:    23-Mar-2001 RD: a bad station in the SD case was not detected
!             29-May-2001 RD: use sr alcerr for allocation
!             21-Dec-2001 HU: Use m_bern, other modules with ONLY
!             18-Aug-2010 RD: RMS ratio for satellites and stations
!
! SR used:    alcerr
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE p_reschk, ONLY: t_reschk_opt,t_rmssta,t_badsta

  USE s_alcerr
  IMPLICIT NONE
!
! Variables from parameter list
! -----------------------------
! input
  TYPE(t_reschk_opt)                    :: opt    ! input options
  INTEGER(i4b)                          :: nSta   ! # of stations in list
  TYPE(t_rmssta), DIMENSION(:), POINTER :: rmssta ! Sta. information rec.
  REAL(r8b)                             :: rmstot ! total RMS
! output
  INTEGER(i4b)                          :: nBad   ! # of bad stations
  TYPE(t_badsta), DIMENSION(:), POINTER :: BadSta ! List of bad stations
  INTEGER(i4b)                          :: irCode ! return code of this SR
!
! Local variables
! ---------------
  CHARACTER(len=shortLineLength)        :: Line   ! protocol output line
  CHARACTER(len=staNameLength)          :: stName ! station name
!
  INTEGER(i4b)                          :: iSta   ! Counter for station
  INTEGER(i4b)                          :: jSta   ! Counter for station
  INTEGER(i4b)                          :: kSta   ! Counter for station
  INTEGER(i4b)                          :: iBad   ! Counter for bad sta.
  INTEGER(i4b)                          :: jBad   ! Counter for bad sta.
  INTEGER(i4b)                          :: ios    ! IO status
  INTEGER(i4b)                          :: maxStat! max. number of bad sta.
!
  REAL(r8b)                             :: minBad ! Min. mean RMS from list
!
  LOGICAL                               :: OKflag ! bad solution (0-diff)
!
! Start the full protocoll
! ------------------------
  IF (opt%summary == 1)            &
    WRITE(lfnprt,'(//,A,/,A,/)')   &
    'PART 2: STATION CHECK',       &
    '---------------------'
!
! Iniatialization
! ---------------
  nBad=0
  irCode=0
  OKflag=.TRUE.
  ALLOCATE(BADSTA(2*nSTA), stat=ios)
  CALL alcerr(ios,'BADSTA',(/2*nSTA/),'rcstachk')
!
! Handle the zero difference case
! ---------------------------------
  IF (opt%nDiff == 0) THEN
!
! Search bad stations
! --------------------
!
! start the full protocoll
! ------------------------
    IF (opt%summary == 1)                                                   &
      WRITE(lfnprt,'(/,A,/,A,//,A,F7.2,A,//,A,/,A)')                        &
            'SCREEN FILE TO DETECT BAD STATIONS',                           &
            '----------------------------------',                           &
            '  MAX. RMS ALLOWED FOR A "GOOD" STATION:  ',opt%maxRMS0,' MM', &
            '  NUM   STATION 1                    RMS         STATUS',      &
            '------------------------------------------------------------------------------'
!
! Loop all baselines
! ------------------
    DO iSta=1,nSta
!
! Prepare a protocoll line
! ------------------------
      WRITE(Line,'(I5,3X,A20,3X,F9.1)')           &
        iSta, rmssta(iSta)%StaNam(1)(1:20), rmssta(iSta)%RMS
!
! Check for a bad solution
! ------------------------
      OKflag=OKflag .AND. rmsSta(iSta)%RMS <= opt%badSol0
      IF (rmssta(iSta)%RMS > opt%badSol0) &
        WRITE(Line(58:65),'(A)') 'SOLUTION'
!
! Check for a bad station
! -----------------------
      IF (rmssta(iSta)%RMS > opt%maxRMS0 .OR. &
          rmssta(iSta)%RMS > opt%badSol0 ) THEN
        WRITE(Line(54:56),'(A)') 'BAD'
!
! Add a station to the list of bad stations
! -----------------------------------------
        nBad=nBad+1
        BADSTA(nBad)%StaNam=RmsSta(iSta)%StaNam(1)
        BADSTA(nBad)%FilNam=RmsSta(iSta)%FilNam(1)
        badSta(nBad)%nSta=1
        badSta(nBad)%RMSSUM=RmsSta(iSta)%RMS
        badSta(nBad)%RMSratio=RmsSta(iSta)%RMS/rmstot
      ENDIF
      IF (opt%summary == 1) WRITE(lfnprt,'(A)') TRIM(Line)
    ENDDO ! loop all stations
    IF (opt%summary == 1)                                                &
      WRITE(lfnprt,'(A,/)')                                              &
            '------------------------------------------------------------------------------'
!
! Set "maxsta"
! ------------
    maxStat=1
    IF (OKflag) maxstat=opt%maxsta0

  ENDIF ! zero difference case
!
! Handle the Single difference case
! ---------------------------------
  IF (opt%nDiff == 1) THEN
!
! Search bad baselines
! --------------------
!
! start the full protocoll
! ------------------------
    IF (opt%summary == 1)                                                   &
      WRITE(lfnprt,'(/,A,/,A,//,A,F7.2,A,//,A,/,A)')                        &
            'CHECK ALL BASELINES TO DETECT BAD STATIONS',                   &
            '------------------------------------------',                   &
            '  MAX. RMS ALLOWED FOR A "GOOD" STATION:  ',opt%maxRMS1,' MM', &
            '  NUM   STATION 1              STATION 2                    RMS         STATUS',  &
            '------------------------------------------------------------------------------'
!
! Loop all baselines
! ------------------
    DO iSta=1,nSta
!
! Prepare a protocoll line
! ------------------------
      WRITE(Line,'(I5,2(3X,A20),3X,F9.1)')                                &
        iSta, rmssta(iSta)%StaNam(1)(1:20), rmssta(iSta)%StaNam(2)(1:20), &
        rmssta(iSta)%RMS
!
      IF (rmssta(iSta)%RMS > opt%maxRMS1) THEN
        WRITE(Line(74:76),'(A)') 'BAD'
        DO jSta=1,2
!
! Check the list of bad stations
! ------------------------------
          iBad=1
          DO WHILE (iBad<=nBad)
            IF (BADSTA(iBad)%StaNam == RMSSTA(iSta)%STANAM(jSta)) THEN
              badSta(iBad)%nSta=badSta(iBad)%nSta+1
              badSta(iBad)%RMSSUM=badSta(iBad)%RMSSUM+RMSSta(iSta)%RMS
              iBad=9999
            ENDIF
            iBad=iBad+1
          ENDDO
!
! Add a station to the list of bad stations
! -----------------------------------------
          IF (iBad < 10000) THEN
            nBad=nBad+1
            BADSTA(nBad)%StaNam=RmsSta(iSta)%StaNam(jSta)
            BADSTA(nBad)%FilNam=RmsSta(iSta)%FilNam(jSta)
            badSta(nBad)%nSta=1
            badSta(nBad)%RMSSUM=RmsSta(iSta)%RMS
          ENDIF
        ENDDO
      ENDIF
      IF (opt%summary == 1) WRITE(lfnprt,'(A)') TRIM(Line)
    ENDDO ! loop all baselines
    IF (opt%summary == 1)                                                &
      WRITE(lfnprt,'(A,/)')                                              &
            '------------------------------------------------------------------------------'

!
! Bad station in all cases?
! -------------------------
    DO iBad=1,nBad
      DO iSta=1,nSta
        DO jSta=1,2
          IF (badSta(iBad)%StaNam == RMSSta(iSta)%StaNam(jSta) .AND. &
              RMSSta(iSta)%RMS <= opt%maxRMS1) badSta(iBad)%StaNam=''
        ENDDO
      ENDDO
    ENDDO

!
! A station was only once in the network:
! It is OK if the other station is bad.
! --------------------------------------
    DO iBad=1,nBad
      IF (badSta(iBad)%StaNam /= '') THEN
        kSta=0
        DO iSta=1,nSta
          IF (badSta(iBad)%StaNam == RMSSta(iSta)%StaNam(1)) kSta=kSta+1
          IF (badSta(iBad)%StaNam == RMSSta(iSta)%StaNam(2)) kSta=kSta+1
        ENDDO
        IF (kSta == 1) THEN
          DO iSta=1,nSta
            stName = ''
            IF (badSta(iBad)%StaNam == RMSSta(iSta)%StaNam(1)) &
                stName=RMSSta(iSta)%StaNam(2)
            IF (badSta(iBad)%StaNam == RMSSta(iSta)%StaNam(2)) &
                stName=RMSSta(iSta)%StaNam(1)
            IF (stName /= '') THEN
              DO jBad=1,nBad
                IF (iBad /= jBad .AND. stName == badSta(jBad)%StaNam) &
                  badSta(iBad)%StaNam=''
              ENDDO
            ENDIF
          ENDDO
        ENDIF
      ENDIF
    ENDDO
!
! Remove non-bad stations from bad list
! -------------------------------------
    iBad=nBad
    DO WHILE (iBad >= 1)
      IF (badSta(iBad)%StaNam=='') THEN
        DO jBad=iBad,nBad-1
          badSta(jBad)%StaNam=badSta(jBad+1)%StaNam
          badSta(jBad)%Filnam=badSta(jBad+1)%Filnam
          badSta(jBad)%nSta  =badSta(jBad+1)%nSta
          badSta(jBad)%RMSsum=badSta(jBad+1)%RMSsum
        ENDDO
        nBad=nBad-1
      ENDIF
      iBad=iBad-1
    ENDDO

! Compute the RMS-ratio
! ---------------------
    DO ibad = 1,nBad
      badSta(iBad)%RMSratio = badSta(iBad)%RMSsum/badSta(jBad+1)%nSta/rmstot
    ENDDO


! Set "maxsta"
! ------------
    maxstat=opt%maxsta1
!
  ENDIF ! single difference case

!
! Do not delete more than maxsta stations
! ---------------------------------------
!
! Write the full protocoll
! ------------------------
  IF (opt%summary == 1 .AND. nBad > maxStat) THEN
    WRITE(lfnprt,'(/,A,/,A,//,A,/,A)')                                   &
            'LIST OF ALL BAD STATION(S) DETECTED: ',                     &
            '-----------------------------------',                       &
            '  NUM   STATION NAME              MEAN RMS     # OF BASELINES',&
            '------------------------------------------------------------------------------'
    DO iSta=1,nBad
      WRITE(lfnprt,'(I5,3X,A20,3X,F9.1,10X,I6)')                         &
        iSta, badsta(ista)%stanam(1:20),                                 &
        badsta(ista)%rmssum/badsta(ista)%nsta,badsta(ista)%nsta
    ENDDO
    WRITE(lfnprt,'(A,/)')                                                &
            '------------------------------------------------------------------------------'
  ENDIF
!
! Remove not more than MAXSTA0 stations
! -------------------------------------
  DO WHILE (nBad > maxStat)
    jBad=1
    minBad=99D99
    DO iBad=1,nBad
      IF (badSta(iBad)%RMSsum/badSta(iBad)%nSta < minBad) THEN
        jBad=iBad
        minBad=badSta(iBad)%RMSsum/badSta(iBad)%nSta
      ENDIF
    ENDDO
    DO iBad=jBad,nBad-1
      badSta(iBad)%StaNam=badSta(iBad+1)%StaNam
      badSta(iBad)%filNam=badSta(iBad+1)%FilNam
      badSta(iBad)%nSta  =badSta(iBad+1)%nSta
      badSta(iBad)%RMSsum=badSta(iBad+1)%RMSsum
      badSta(iBad)%RMSratio=badSta(iBad+1)%RMSratio
    ENDDO
    nBad=nBad-1
  ENDDO
!
  RETURN
  END SUBROUTINE rcstachk


END MODULE
