MODULE s_RDICLK
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdiclk(nAllSta, allStaNum, allStaName, nAllSat, allSatNum,    &
                  isasys, nclkst, nclksa, clksta, clksat, nepobs, irel2, &
                  clksys, clkhed)

! -------------------------------------------------------------------------
! Purpose:    Reads the epochwise clock estimation options for GPSEST
!
! Author:     R. Dach
!
! Created:    28-Jun-2001
!
! Changes:    22-Jan-2002 RD: Condition of sum for reference clock
!             04-Sep-2002 RD: Skip obs. if no sat-clk available
!             08-Oct-2002 RD: New call of SR gtStaNum
!             14-Oct-2002 RD: Cond. of sum, but no reference clock to est.
!             28-Jan-2003 RD: Number of obs. for kin. pos.(clkobs->nepobs)
!                             Allocate dummy for calling SR gtstanum
!             03-Feb-2003 RD: New call of GTSTANUM (weight is pointer now)
!             27-Mar-2003 RD: Remove keyword CLKSAT, source code review
!             15-Apr-2003 RD: Move reading of EDTLVL from RDICLK to RDIGEN
!             04-Nov-2003 HB: Declare allStaName with (:)
!             21-Jan-2004 RD: Adapt to SS/MMs input panel
!             24-Nov-2006 AG: TIMSYS and DCBLINE implemented
!             09-May-2009 RD: Seperate receiver clocks for GPS/GLONASS
!             21-May-2010 MF: Nullify pointers & call sr init_ref
!             26-Oct-2010 CR: Read flag for Periodic Relativistic J2-Correction
!             28-Mar-2012 RD: Use SVN2CHR as module now
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, &
                      fileNameLength, staNameLength, keyValueLength
  USE d_clkrnx, ONLY: t_clkhead, init_ref
  USE s_alcerr
  USE s_svn2chr
  USE s_gtstanum
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckopti
  USE s_gtflna
  USE s_ckoptl
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                 :: nAllSta    ! number of all stations
  INTEGER(i4b), DIMENSION(*)   :: allStaNum  ! station numbers
  CHARACTER(LEN=staNameLength),&
                DIMENSION(:)   :: allStaName ! station names
  INTEGER(i4b)                 :: nAllSat    ! number of all satellites
  INTEGER(i4b), DIMENSION(*)   :: allSatNum  ! satellite numbers
  INTEGER(i4b)                 :: isasys     ! satellite system to be considered
                                             ! = 0: ALL
                                             ! = 1: GPS
                                             ! = 2: GLONASS

! output:
  INTEGER(i4b)                 :: nclkst     ! # station epoch wise clocks
  INTEGER(i4b)                 :: nclksa     ! # satellite epoch wise clocks
  INTEGER(i4b), DIMENSION(*)   :: clksta     ! station numbers for
                                             ! epoch wise clock estimation
  INTEGER(i4b), DIMENSION(*)   :: clksat     ! sat. numbers for
                                             ! epoch wise clock estimation
  INTEGER(i4b), DIMENSION(*)   :: nepobs     ! Min # of obs. for epoch param.s
                                             ! 1: sta-clk / 2: sat-clk / 3: kin
  INTEGER(i4b)                 :: irel2      ! flag for Periodic Relativistic J2-Correction
  INTEGER(i4b)                 :: clksys     ! 1: One rec.clk for each sat.sys
  TYPE(t_clkHead)              :: clkhed     ! Header information for
                                             ! clock rinex output file
                                             ! clkhed%numRef == 0: fix ref.clock
                                             ! clkhed%numRef == 2: indicates
                                             !    condition of sum for reference

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'rdiclk'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
       DIMENSION(:),   POINTER  :: keyValue
  CHARACTER(LEN=staNameLength),  &
       DIMENSION(:),   POINTER  :: refNam
  CHARACTER(LEN=staNameLength),  &
       DIMENSION(:),   POINTER  :: refSat
  CHARACTER(LEN=staNameLength),&
       DIMENSION(:),   POINTER  :: allSatName
  CHARACTER(LEN=staNameLength)  :: hlpStr
  CHARACTER(LEN=fileNameLength) :: rxcFil
  CHARACTER(LEN=1)              :: satChr

  INTEGER(i4b)                  :: nRefSat, nRefSta
  INTEGER(i4b),                  &
        DIMENSION(:),   POINTER :: allSvn
  INTEGER(i4b),                  &
        DIMENSION(:),   POINTER :: refNum
  INTEGER(i4b),                  &
        DIMENSION(:),   POINTER :: refSvn
  INTEGER(i4b)                  :: iSta, jSta
  INTEGER(i4b)                  :: iSat, jSat
  INTEGER(i4b)                  :: satnum
  INTEGER(i4b)                  :: irCode
  INTEGER(i4b)                  :: irc, ios

  REAL(r8b),                     &
        DIMENSION(:,:), POINTER :: dummy

  LOGICAL                       :: isClkSt, isClkSa

! Init the variables
! ------------------
  irCode = 0
  NULLIFY(keyValue)
  NULLIFY(refNam)
  NULLIFY(refSat)
  NULLIFY(allSatName)
  NULLIFY(allSvn)
  NULLIFY(refNum)
  NULLIFY(refSvn)
  NULLIFY(dummy)

  nClkSt = 0
  nClkSa = 0

! Generate the strings of the satellites
! --------------------------------------
  ALLOCATE(allSatName(2*nAllSat),stat=irc)
  CALL alcerr(irc,'allSatName',(/2*nAllSat/),srName)

  ALLOCATE(allSvn(2*nAllSat),stat=irc)
  CALL alcerr(irc,'allSvn',(/2*nAllSat/),srName)

  DO iSat = 1,nAllSat
    CALL svn2chr(allSatNum(iSat),satNum,satChr)
    allSatName(iSat)=' '
    WRITE(allSatName(iSat),'(A1,I2.2)') satChr,satNum

    allSatName(iSat+nAllSat)=' '
    WRITE(allSatName(iSat+nAllSat),*) allSatNum(iSat)
    hlpStr = adjustl(allSatName(iSat+nAllSat))
    allSatName(iSat+nAllSat) = TRIM(hlpStr)

    allSvn(iSat) = allSatNum(iSat)
    allSvn(iSat+nAllSat) = allSatNum(iSat)
  ENDDO


! Get the clock setup
! -------------------
  CALL ckoptb(1,(/ 'CLOCKR' /),'rdiclk','Estimate station clocks',irCode, &
              error=0, resultL=isClkSt)
  CALL ckoptb(1,(/ 'CLOCKS' /),'rdiclk','Estimate satellite clocks',irCode, &
              error=0, resultL=isClkSa)


! Get all reference clocks
! ------------------------

  ! Station clocks
  nRefSta = 0
  IF (.NOT. isClkSt .AND. isClkSa) THEN
    ALLOCATE(refNam(nAllSta),stat=irc)
    CALL alcerr(irc,'refNam',(/nAllSta/),'rdiclk')

    ALLOCATE(refNum(nAllSta),stat=irc)
    CALL alcerr(irc,'refNum',(/nAllSta/),'rdiclk')

    nRefSta = nAllSta
    refNam(1:nRefSta) = allStaName(1:nAllSta)
    refNum(1:nRefSta) = allStaNum(1:nAllSta)

  ELSE IF (isClkSt .AND. isClkSa) THEN
    ALLOCATE(refNam(nAllSta),stat=irc)
    CALL alcerr(irc,'refNam',(/nAllSta/),'rdiclk')

    ALLOCATE(refNum(nAllSta),stat=irc)
    CALL alcerr(irc,'refNum',(/nAllSta/),'rdiclk')

    ! Get the list of station reference clocks
    CALL gtStaNum(nAllSta, allStaNum, allStaName,         &
                  'REFCLOCKR','STATION8','STAFILE8', ' ', &
                  nRefSta, refNum, refNam, 0, dummy)
  ENDIF

  ! Satellite clocks
  nRefSat = 0
  IF (isClkSt .AND. .NOT. isClkSa) THEN

    ALLOCATE(refSat(nAllSat),stat=irc)
    CALL alcerr(irc,'refSat',(/nAllSta/),'rdiclk')

    ALLOCATE(refsvn(nAllSat),stat=irc)
    CALL alcerr(irc,'refsvn',(/nAllSta/),'rdiclk')

    nRefSat = nAllSat
    refSat(1:nAllSat) = allSatName(1:nAllSat)
    refSvn(1:nAllSat) = allSvn(1:nAllSat)

  ELSE IF (isClkSt .AND. isClkSa) THEN
    ALLOCATE(refSat(2*nAllSat),stat=irc)
    CALL alcerr(irc,'refSat',(/nAllSta/),'rdiclk')

    ALLOCATE(refsvn(2*nAllSat),stat=irc)
    CALL alcerr(irc,'refsvn',(/nAllSta/),'rdiclk')

    ! Get the list of satellite reference clocks
    CALL gtStaNum(2*nAllSat, allSvn, allSatName,            &
                 'REFCLOCKS','SATELLITE8','SATFILE8', ' ',  &
                  nRefSat, refSvn, refSat, 0, dummy)

  ENDIF


! Check for reference clocks
! --------------------------
  IF (nRefSta + nRefSat == 0) THEN
    WRITE(lfnerr,'(/,A,/,16X,A,/)')                                         &
    ' *** SR RDICLK: For the clock estimation it is mandatory to select ',  &
                    'one or more reference clocks.'
    irCode = irCode + 1
  ENDIF

! Condition of sum for reference clocks
! -------------------------------------
  clkHed%numRef = 0
  IF (isClkSt .AND. isClkSa) THEN
    CALL readkeys('REFSUM',keyValue,irc)

    CALL ckoptc(1,'REFSUM',keyValue,                            &
               (/'REFERENCE_FIXED    ','ZERO-MEAN_CONDITION'/), &
               'rdiclk','type of reference clocks',irc,irCode,  &
                valList=(/0,2/),error=0, result1=clkHed%numRef)
  ENDIF

  IF (clkHed%numRef == 2) THEN

    ALLOCATE(clkHed%ref(clkHed%numRef),stat=irc)
    CALL alcerr(irc,'clkHed%ref',(/clkHed%numRef/),'rdiclk')

    DO iSta=1,clkHed%numRef
      CALL init_ref(clkHed%ref(iSta))
    END DO

    clkHed%ref(2)%nRef = 0
    clkHed%ref(1)%nRef = 0
    ALLOCATE(clkHed%ref(1)%clk(nrefSta+nrefSat),stat=irc)
    CALL alcerr(irc,'clkHed%ref(1)%clk',(/nrefSta+nrefSat/),'rdiclk')

  ENDIF

! Get the list of stations for clock estimation
! ---------------------------------------------
  iStaLoop: DO iSta = 1, nAllSta
    DO jSta = 1, nRefSta
      IF (allStaNum(iSta) == refNum(jSta) .AND. clkHed%numRef == 0) THEN
        CYCLE iStaLoop
      ELSE IF (allStaNum(iSta) == refNum(jSta) .AND. clkHed%numRef == 2) THEN
        clkHed%ref(1)%nRef = clkHed%ref(1)%nRef + 1
        clkHed%ref(1)%clk(clkHed%ref(1)%nRef)%name = allStaName(iSta)
        EXIT
      ENDIF
    ENDDO
    nClkSt = nClkSt + 1
    clkSta(nClkSt) = allStaNum(iSta)
  ENDDO iStaLoop

  IF (isClkSa) THEN
    DEALLOCATE(refNam,stat=irc)
    DEALLOCATE(refNum,stat=irc)
  ENDIF


! Get the list of satellite for clock estimation
! ----------------------------------------------
  iSatLoop: DO iSat = 1, nAllSat
    DO jSat = 1, nRefSat
      IF (allSatNum(iSat) == refSvn(jSat) .AND. clkHed%numRef == 0) THEN
        CYCLE iSatLoop
      ELSE IF (allSatNum(iSat) == refSvn(jSat) .AND. clkHed%numRef == 2) THEN
        clkHed%ref(1)%nRef = clkHed%ref(1)%nRef + 1
        clkHed%ref(1)%clk(clkHed%ref(1)%nRef)%name = allSatName(iSat)
        EXIT
      ENDIF
    ENDDO
    nClkSa = nClkSa + 1
    clkSat(nClkSa) = allSatNum(iSat)
  ENDDO iSatLoop

  IF (isClkSt) THEN
    DEALLOCATE(refSat,stat=irc)
    DEALLOCATE(refSvn,stat=irc)
  ENDIF

  DEALLOCATE(allSatName,stat=irc)
  DEALLOCATE(allSvn, stat=irc)

! Condition of sum, but no ref-clocks are setup
! ---------------------------------------------
  IF (clkHed%numRef == 2) THEN
    IF (clkHed%ref(1)%nRef == 0) clkHed%numRef = 0
  ENDIF

! Read minimum number of observations for station clocks
! ------------------------------------------------------
  nepobs(1:2) = 0
  IF (nClkSt > 0) THEN
    CALL readKeys('MINSTA',keyValue,irc)

    CALL ckopti(1,'MINSTA',keyValue,srName,                          &
                'Min. num. of observ. for station clock',irc,irCode, &
                maxVal=1,empty=0,ge=0,result1=nepobs(1))
  ENDIF

! Individual receiver clocks for each satellite system
! ----------------------------------------------------
  clksys = 0
  IF (nClkSt > 0 .AND. (isasys == 0 .OR. isasys > 3)) THEN
    CALL ckoptb(1,(/'RCLKSYS'/),srName,                              &
                'Receiver clocks for each ssatellite system',irCode, &
                result1=clksys)
  ENDIF

! Read minimum number of observations for satellite clocks
! --------------------------------------------------------
  IF (nClkSa > 0) THEN
    CALL readKeys('MINSAT',keyValue,irc)

    CALL ckopti(1,'MINSAT',keyValue,srName,                            &
                'Min. num. of observ. for satellite clock',irc,irCode, &
                maxVal=1,empty=0,ge=0,result1=nepobs(2))

  ENDIF

! Periodic Relativistic J2-Correction
! -----------------------------------
  CALL ckoptb(1,(/ 'PRELJ2' /),srName,                      &
              'Periodic Relativistic J2-Correction',irCode, &
              error=0, result1=irel2)

! Get Information for clock Rinex header
! --------------------------------------
  CALL gtflna(0,'CLKRNX',rxcFil,irc)

  IF (irc == 0 .AND. LEN_TRIM(rxcFil) > 0) THEN

    CALL readKeys('RUNBY',keyValue,irc)
    IF (irc == 0) clkhed%RunBy = keyValue(1)

    CALL readKeys('AC',keyValue,irc)
    IF (irc == 0) clkhed%AC = keyValue(1)

    CALL readKeys('ACNAME',keyValue,irc)
    IF (irc == 0) clkhed%ACName = keyValue(1)

    CALL readkeys('TIMESYS', keyValue, irc)
    IF (irc==0) &
         CALL ckoptl(1,'TIMESYS', keyValue, 'sr wtcrxh', 'Time system', irc,   &
              irCode, maxLength=LEN(ClkHed%timsys), maxval=1, result1=ClkHed%timsys)

    CALL readkeys('DCBLINE', keyValue, irc)
    IF (irc==0) &
         CALL ckoptl(0,'DCBLINE', keyValue, 'sr wtcrxh', 'DCB line', irc, irCode, &
            maxLength=LEN(ClkHed%dcbStr), empty=' ', maxval=1, result1=ClkHed%dcbStr)

    clkhed%nComment = 0
    CALL readKeys('COMMENT',keyValue,irc)

    clkhed%nComment = SIZE(keyValue)+clkHed%numRef/2
    ALLOCATE(clkhed%Comment(clkhed%nComment),stat=ios)
    CALL alcerr(ios, 'clkhed%Comment', (/clkhed%nComment/), 'rdiclk')

    CALL ckoptl(0,'COMMENT',keyValue,srName,        &
                'Clock RINEX comment',irc,irCode,   &
                empty=' ',maxVal=clkhed%nComment,result2=clkhed%Comment)

    IF (clkHed%numRef == 2) THEN
      clkhed%Comment(clkhed%nComment) = &
            'ATTENTION: The sum of the reference clock estimates is zero'
    ENDIF
  ENDIF

  DEALLOCATE(keyValue,stat=irc)

  IF (irCode /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE rdiclk

END MODULE
