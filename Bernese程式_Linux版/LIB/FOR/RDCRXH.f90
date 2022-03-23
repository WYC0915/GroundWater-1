MODULE s_RDCRXH
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdcrxh(lfnclk,lfnmsg,ClkHead,irCode)

! -------------------------------------------------------------------------
! Purpose:    read the entire header information of a rinex clock file
!             (based on the f77 sr R2RDCH)
!
! Remark:     ClkHead%tFirst must be defined. It will be set only if it is
!             ClkHead%tFirst == 0d0 .
!
! Parameters:
!         in: lfnclk  : Logical file number of clock rinex file         i4b
!             lfnmsg  : Logical file number for error messages          i4b
!        out: ClkHead : Information from rinex file header           t_clkhead
!             irCode  : return code                                     i4b
!                         0: ok
!                         1: wrong file type
!                         2: not anticipated version number
!                         3: end of file within header
!                         4: error decoding data
!                         5: first line not version line
!                         6: mandatory field(s) missing
!                         7: unknown record
!                         8: feature not yet handled
!                         9: end of file at beginning of header
!                        10: number of items found in header is
!                            inconsitent to the entry in header
!
!
! Author:     R. Dach
!
! Created:    09-Aug-2000
! Last mod.:  13-Jan-2012
!
! Changes:    14-Feb-2001 RD: use SR ALCERR
!             18-Feb-2001 RD: more than one reference clock set possible
!             14-May-2001 RD: Improved structure od reference clock array
!             22-May-2001 RD: Special case if no sat/sta clocks in file
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             25-Sep-2002 HU: Remove i_astlib
!             07-Jul-2003 RD: Correct format statement
!             06-Jan-2004 HU: Compare PRN numbers as given in the header
!             07-Jul-2005 RD: Upperc not for all keywords
!             24-Nov-2006 AG: timsys, dcbstr, pcvstr added to ClkHead
!             19-Jul-2010 SL: tab characters removed
!             19-Aug-2011 RD: Allow to read version 3.0
!             13-Jan-2012 RD: Read clock RINEX with only satellite clocks
!
! SR used:    djul, alcerr
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_clkrnx, ONLY: t_clkhead

  USE s_alcerr
  USE f_djul
  USE s_upperc
  IMPLICIT NONE
!
! Variables from parameter list
! -----------------------------
  INTEGER(i4b)      :: lfnclk         ! Logical file number of clock rinex file
  INTEGER(i4b)      :: lfnmsg         ! Logical file number for error messages
  TYPE(t_clkhead)   :: ClkHead        ! Clock rinex header
  INTEGER(i4b)      :: irCode         ! Return code of this SR
!
! Local variables
! ---------------
  CHARACTER(len=60) :: string         ! values of a header line
  CHARACTER(len=20) :: head           ! keyword of the header line
  CHARACTER(len= 2), DIMENSION(10)    &
                    :: DataTypes      ! Reads one line of data types
  CHARACTER(len= 1), DIMENSION(15)    &
                    :: SatChr         ! Read 1 line with SatNums
  CHARACTER(len= 3), DIMENSION(15)    &
                    :: cSat           ! Read 1 line with Satchars
  CHARACTER(len= 1) :: RnxTyp         ! Typ of rinex file ('C' expected)
!
  REAL(r8b)         :: secs, sece     ! Start/End date of file (sec.)
  REAL(r8b)         :: day,t0         ! Start date (day of month, mjd)
  REAL(r8b)         :: X,Y,Z          ! Station coordinates
  REAL(r8b)         :: Rad            ! Dist. to geocenter
!
  INTEGER(i4b)     , DIMENSION(15)    &
                    :: number         ! Read 1 line with SatNums
  INTEGER(i4b)      :: nLine          ! # of lines in rinex header
  INTEGER(i4b)      :: iTyp,jTyp      ! Counter for data types
  INTEGER(i4b)      :: nTyp           ! Counter for data types
  INTEGER(i4b)      :: iyyys,mms,ids  ! Start date of file (year, month, day)
  INTEGER(i4b)      :: ihs,ims        ! Start date of file (hour, min.)
  INTEGER(i4b)      :: iyyye,mme,ide  ! End date of file (year, month, day)
  INTEGER(i4b)      :: ihe,ime        ! End date of file (hour, min.)
  INTEGER(i4b)      :: iRef           ! Counter for ref. stations
  INTEGER(i4b)      :: jRef           ! Counter for ref. stations
  INTEGER(i4b)      :: kRef           ! Counter for ref. stations
  INTEGER(i4b)      :: nRef           ! # of ref. stations
  INTEGER(i4b)      :: numRef         ! # of ref. stations sets
  INTEGER(i4b)      :: iSta           ! Counter for stations
  INTEGER(i4b)      :: nSta           ! # of stations
  INTEGER(i4b)      :: iSat,jSat      ! Counter for satellites
  INTEGER(i4b)      :: nSat           ! # of satellites
  INTEGER(i4b)      :: iCom           ! Counter for comment lines
  INTEGER(i4b)      :: nCom           ! # of comment lines
  INTEGER(i4b)      :: iX,iY,iZ       ! station coordinates (integer part)
  INTEGER(i4b)      :: iXf,iYf,iZf    ! station coordinates (fract. part)
  INTEGER(i4b)      :: ii
  INTEGER(i4b)      :: ioRead         ! I/O status for reading operations
  INTEGER(i4b)      :: iDummy         ! A dummy variable
  INTEGER(i4b)      :: ios            ! io status
!
! CHECK SOME LINES IN HEADER
! --------------------------
  LOGICAL           :: found
  LOGICAL           :: isPGM = .FALSE.  ! Line PGM/RUN BY/DATE found
  LOGICAL           :: isAC  = .FALSE.  ! Line AC/ACName found
!
! RINEX FILE TYPE
! ---------------
  CHARACTER(len= 1), PARAMETER :: rxType = 'C'
!
! MAXIMUM RINEX FORMAT VERSION
! ----------------------------
  REAL(r8b)        , PARAMETER :: rxVers = 3.00D0
!
! INITIALIZE
! ----------
  irCode             = 0
  iTyp               = 0
  nTyp               = -1
  iCom               = 0
  nCom               = 0
  iRef               = 0
  nRef               = -1
  iSta               = 0
  nSta               = -1
  nSat               = -1
  ClkHead%LeapSec    = 0
  ClkHead%nComment   = 0
  ClkHead%NumTyp     = 0
  ClkHead%numRef     = 0
  ClkHead%nSta       = 0
  ClkHead%nSat       = 0
  ClkHead%RnxVers    = 0.0D0
  ClkHead%AC         = ' '
  ClkHead%ACName     = ' '
  ClkHead%pgmnam     = ' '
  ClkHead%timsys     = ' '
  ClkHead%dcbstr     = ' '
  ClkHead%pcvstr     = ' '
  ClkHead%ProgNam    = ' '
  ClkHead%RunBy      = ' '
  ClkHead%CrDate     = ' '
  ClkHead%Clk0Name   = ' '
  ClkHead%CalName    = ' '
  ClkHead%TRFName    = ' '
!
! READ FIRST LINE
! ---------------
  READ(LFNCLK,'(A60,A20)',iostat=ioRead) string,head
  IF (ioRead /= 0) THEN
    irCode=9
  ELSE
    CALL upperc(string)
    CALL upperc(head)
!
! RINEX VERSION / TYPE
! --------------------
    IF (head /= 'RINEX VERSION / TYPE') THEN
      irCode=5
    ELSE
      READ(string,'(F9.2,11X,A1,39X)',iostat=ioRead)                &
           ClkHead%RnxVers,RnxTyp
      IF (ioRead /= 0) irCode=4
    ENDIF
  ENDIF
  IF (irCode == 0) THEN
    IF (RnxTyp /= rxType) irCode=1
    IF (ClkHead%RnxVers <= 0.0 .OR. ClkHead%RnxVers > rxVers) irCode=2
  ENDIF
!
! GET NUMBER OF COMMENT LINES, DATA TYPES, STATIONS,
! SATELLITES, AND REFERNCE CLOCK TO ALLOCATE THE ARRAYS
! -----------------------------------------------------
  IF (irCode == 0) THEN
    nLine=0
    LoopHead0: DO
      IF (irCode /= 0) EXIT LoopHead0
      READ(lfnclk,'(A60,A20)',iostat=ioRead) string,head
      IF (ioRead /= 0) THEN
        irCode=3
        EXIT LoopHead0
      ENDIF
      CALL upperc(string)
      CALL upperc(head)
      nLine=nLine+1
!
! Count number of COMMENT lines
! -----------------------------
      IF (TRIM(head) == 'COMMENT') THEN
        nCom=nCom+1
!
! Get number of data types
! ------------------------
      ELSE IF (head == '# / TYPES OF DATA   ') THEN
        READ(string,'(I6)',iostat=ioRead) nTyp
        IF (ioRead /= 0) THEN
          irCode=4
          EXIT LoopHead0
        ENDIF
!
! Get number of reference clocks
! ------------------------------
      ELSE IF (head.EQ.'# OF CLK REF        ') THEN
        iRef=iRef+1
        READ(string,'(I6)',iostat=ioRead) kRef
        IF (ioRead /= 0) THEN
          irCode=4
          EXIT LoopHead0
        ENDIF
        IF (kRef>nRef) nRef=kRef
!
! Get number of stations
! ----------------------
      ELSE IF (head == '# OF SOLN STA / TRF ') THEN
        READ(string,'(I6)',iostat=ioRead) nSta
        IF (ioRead /= 0) THEN
          irCode=4
          EXIT LoopHead0
        ENDIF
!
! Get number of satellites
! ------------------------
      ELSE IF (head == '# OF SOLN SATS      ') THEN
          READ(string,'(I6)',iostat=ioRead) nSat
        IF (ioRead /= 0) THEN
          irCode=4
          EXIT LoopHead0
        ENDIF
!
! End of header
! -------------
      ELSE IF (head == 'END OF HEADER       ') THEN
        EXIT LoopHead0
      ENDIF
!
    ENDDO LoopHead0
!
! Check all information found
! ---------------------------
    ClkHead%nComment = nCom
    ClkHead%NumTyp   = nTyp
    ClkHead%nSta     = nSta   !(Problem with EMR clock rinex file: +1???)
    ClkHead%nSat     = nSat
!
! Give Number of data records into the array
! ------------------------------------------
    IF (irCode == 0) THEN
!
! Deallocate in the case of a 2nd run
! -----------------------------------
      DEALLOCATE (ClkHead%Comment,stat=iDummy)
      DEALLOCATE (ClkHead%DatTyp,stat=iDummy)
      DO jRef=1,ClkHead%numRef
        DEALLOCATE (ClkHead%Ref(jRef)%clk,stat=iDummy)
      ENDDO
      DEALLOCATE (ClkHead%Ref,stat=iDummy)
      DEALLOCATE (ClkHead%ClkName,stat=iDummy)
      DEALLOCATE (ClkHead%StaCoord,stat=iDummy)
!
! Allocate corresponding arrays
! -----------------------------
      IF (nCom<=0) nCom=1
      ALLOCATE(ClkHead%Comment(nCom),stat=ios)
      CALL alcerr(ios,'ClkHead%Comment',(/nCom/),'rdcrxh')
!
      IF (nTyp<=0) nTyp=1
      ALLOCATE(ClkHead%DatTyp(nTyp),stat=ios)
      CALL alcerr(ios,'ClkHead%DatTyp',(/nTyp/),'rdcrxh')
!
      IF (iRef<=0) iRef=1
      ClkHead%numRef=iRef
      ALLOCATE(ClkHead%Ref(ClkHead%numRef),stat=ios)
      CALL alcerr(ios,'ClkHead%Ref',(/ClkHead%numRef/),'rdcrxh')
      DO jRef=1,ClkHead%numRef
        ClkHead%Ref(jRef)%refWin%t=0d0
      ENDDO
!
      IF (nRef<=0) nRef=1
      DO jRef = 1,ClkHead%numRef
        ALLOCATE(ClkHead%Ref(jRef)%clk(nRef),stat=ios)
        CALL alcerr(ios,'ClkHead%Ref(jRef)%clk',(/nRef/),'rdcrxh')
        ClkHead%Ref(jRef)%nRef = 0
      ENDDO
!
      IF (nSta<=0) nSta=1
      IF (nSat<=0) nSat=1
      ALLOCATE(ClkHead%ClkName(nSta+nSat),stat=ios)
      CALL alcerr(ios,'ClkHead%ClkName',(/nSta+nSat/),'rdcrxh')
      ALLOCATE(ClkHead%StaCoord(3,nSta),stat=ios)
      CALL alcerr(ios,'ClkHead%StaCoord',(/3,nSta/),'rdcrxh')
    ENDIF
  ENDIF
!
! Rewind rinex clock file
! -----------------------
  IF (irCode == 0) THEN
    REWIND (lfnclk)
    READ(LFNCLK,'(A60,A20)',iostat=ioRead) string,head
  ENDIF
  iSat   = MAXVAL( (/ClkHead%nSta,0/) )
  iRef   = 0
  numRef = 0
!
! LOOP OVER ALL REMAINING LINES
! -----------------------------
  IF (irCode == 0) nLine=0
  LoopHead1: DO
    IF (irCode /= 0 .AND. irCode /= 10) EXIT LoopHead1
    READ(lfnclk,'(A60,A20)',iostat=ioRead) string,head
    IF (ioRead /= 0) THEN
      irCode=3
      EXIT LoopHead1
    ENDIF
    CALL upperc(head)
    nLine=nLine+1
!
! COMMENT LINES
! -------------
    IF (TRIM(head) == 'COMMENT') THEN
      iCom=iCom+1
      ClkHead%Comment(iCom)=STRING
!
! PGM / RUN BY / DATE
! -------------------
    ELSE IF (head == 'PGM / RUN BY / DATE ') THEN
      CALL upperc(string)
      READ(string,'(A20,A20,A20)',iostat=ioRead)              &
           ClkHead%ProgNam,ClkHead%RunBy,ClkHead%CrDate
      IF (ioRead /= 0) THEN
        irCode=4
        EXIT LoopHead1
      ENDIF
      isPGM  = .TRUE.
!
! LEAP SECONDS
! ------------
    ELSE IF (head == 'LEAP SECONDS        ') THEN
      CALL upperc(string)
      READ(string,'(I6)',iostat=ioRead) ClkHead%LeapSec
      IF (ioRead /= 0) THEN
        irCode=4
        EXIT LoopHead1
      ENDIF
!
! # / TYPES OF DATA
! -----------------
    ELSE IF (head == '# / TYPES OF DATA   ') THEN
      CALL upperc(string)
      READ(string,'(I6,9(4X,A2))',iostat=ioRead)             &
           nTyp,(DataTypes(jTyp),jTyp=1,9)
      IF (ioRead /= 0) THEN
        irCode=4
        EXIT LoopHead1
      ENDIF
      LoopTyp: DO
        IF (DataTypes(iTyp+1) == '  ') EXIT LoopTyp
        IF (iTyp == ClkHead%numTyp) THEN
          iTyp=0
          IF (ClkHead%numTyp /= 0) irCode=10
          EXIT LoopTyp
        ELSE
          iTyp=iTyp+1
          ClkHead%DatTyp(iTyp)=DataTypes(iTyp)
          IF (iTyp == 9) EXIT LoopTyp
        ENDIF
      ENDDO LoopTyp
!
! STATION NAME / NUM
! ------------------
    ELSE IF (head == 'STATION NAME / NUM  ') THEN
      CALL upperc(string)
      ClkHead%Clk0Name=string(1:staNameLength)
!
! STATION CLK REF
! ---------------
    ELSE IF (head == 'STATION CLK REF     ') THEN
      CALL upperc(string)
      ClkHead%CalName=string
!
! ANALYSIS CENTER
! ---------------
    ELSE IF (head == 'ANALYSIS CENTER     ') THEN
      READ(string,'(A3,2X,A55)',iostat=ioRead)              &
           ClkHead%AC,ClkHead%ACName
      IF (ioRead /= 0) THEN
        irCode=4
        EXIT LoopHead1
      ENDIF
      isAC=.TRUE.
!
! # OF CLK REF
! ------------
    ELSE IF (head.EQ.'# OF CLK REF        ') THEN
      CALL upperc(string)
      numRef=numRef+1
      iRef=0
      READ(string,'(I6,1X,I4,4I3,F10.6,1X,I4,4I3,F10.6)',iostat=ioRead)  &
           ClkHead%Ref(numRef)%nRef,                                     &
           iyyys,mms,ids,ihs,ims,secs, iyyye,mme,ide,ihe,ime,sece
      IF (ioRead /= 0) THEN
        irCode=4
        EXIT LoopHead1
      ENDIF
      IF (iyyys /= 0) THEN
        day=ids*1.D0
        T0=DJUL(iyyys,mms,day+(ihs+(ims+secs/60d0)/60d0)/24d0)
        IF (ClkHead%TFirst == 0d0) ClkHead%TFirst=DINT(T0)
        ClkHead%Ref(numRef)%refWin%t(1) = (T0-ClkHead%TFirst) * 86400D0
!
        day=ide*1.D0
        T0=DJUL(iyyye,mme,day+(ihe+(ime+sece/60d0)/60d0)/24d0)
        ClkHead%Ref(numRef)%refWin%t(2) = (T0-ClkHead%TFirst) * 86400D0
!
        IF (t0 < ClkHead%tfirst) THEN
          WRITE(lfnmsg,'(/,A,2(/,16X,A,F8.1),//)')                       &
                ' ### SR RDCRXH: WRONG EPOCH OF FILE',                   &
                'EXPECTED EPOCH: ',ClkHead%tfirst,                       &
                'DETECTED EPOCH: ',t0
!          CALL exitrc(2)
        ENDIF
      ENDIF
!
! ANALYSIS CLK REF
! ----------------
    ELSE IF (head == 'ANALYSIS CLK REF    ') THEN
      CALL upperc(string)
      IF (iRef > ClkHead%Ref(numRef)%nRef) THEN
        IF (ClkHead%Ref(numRef)%nRef /= 0) irCode=10
      ELSE
        iRef=iRef+1
        ClkHead%Ref(numRef)%clk(iRef)%Name=string(1:staNameLength)
        READ(string,'(25X,15X,E19.12,1X)',iostat=ioRead)            &
             ClkHead%Ref(numRef)%clk(iRef)%Sigma
        IF (ioRead /= 0) THEN
          irCode=4
          EXIT LoopHead1
        ENDIF
        ClkHead%Ref(numRef)%clk(iRef)%Sigma = &
                              ClkHead%Ref(numRef)%clk(iRef)%Sigma*1D6
      ENDIF
!
! # OF SOLN STA / TRF
! -------------------
    ELSE IF (head == '# OF SOLN STA / TRF ') THEN
      READ(string,'(I6,4X,A50)',iostat=ioRead) nSta,ClkHead%TRFName
      IF (ioRead /= 0) THEN
        irCode=4
        EXIT LoopHead1
      ENDIF
!
! SOLN STA NAME / NUM (loop over nsta+1 because of problems with emr)
! -------------------
    ELSE IF (head == 'SOLN STA NAME / NUM ') THEN
      CALL upperc(string)
      IF (iSta==ClkHead%nSta) THEN
        iSta=0
        IF (ClkHead%nSta /= 0) irCode=10
      ELSE
        iSta=iSta+1
        ClkHead%ClkName(iSta)=string(1:staNameLength)
        READ(string,'(25X,I8,I3,I9,I3,I9,I3)',iostat=ioRead)            &
             iX,iXf,iY,iYf,iZ,iZf
        IF (ioRead /= 0) THEN
          irCode=4
          EXIT LoopHead1
        ENDIF
        X=iX*1.0D0+SIGN(iXf,iX)*1.0D-3
        Y=iY*1.0D0+SIGN(iYf,iY)*1.0D-3
        Z=iZ*1.0D0+SIGN(iZf,iZ)*1.0D-3
!
! CHECK IF COORDINATES ARE SENSIBLE
! ---------------------------------
        rad=DSQRT(X**2+Y**2+Z**2)
        IF (rad >= 5000D3 .AND. rad < 12000D3) THEN
          ClkHead%StaCoord(1,iSta)=X
          ClkHead%StaCoord(2,iSta)=Y
          ClkHead%StaCoord(3,iSta)=Z
        ELSE
          ClkHead%StaCoord(1,iSta)=0.0D0
          ClkHead%StaCoord(2,iSta)=0.0D0
          ClkHead%StaCoord(3,iSta)=0.0D0
          IF (rad /= 0.0) THEN
            WRITE(lfnmsg,'(/,A,/,16X,A,A,/,16X,A,F8.1,//)')                   &
              ' ### SR RDCRXH: WRONG STATION COORDINATES',                    &
                              'STATION NAME  : ',TRIM(ClkHead%ClkName(iSta)), &
                              'STATION RADIUS: ',rad*1D-3
          ENDIF
        ENDIF
    ENDIF
!
! # OF SOLN SATS
! --------------
    ELSE IF (head == '# OF SOLN SATS      ') THEN
      CALL upperc(string)
        READ(string,'(I6)',iostat=ioRead) nSat
      IF (ioRead /= 0) THEN
        irCode=4
        EXIT LoopHead1
      ENDIF
!
! PRN LIST
! --------
    ELSE IF (head == 'PRN LIST            ') THEN
      CALL upperc(string)
      READ(string,'(15(A3,1X))',iostat=ioRead)                             &
           (cSat(jSat),jSat=1,15)
      IF (ioRead /= 0) THEN
        irCode=4
        EXIT LoopHead1
      ENDIF
!
! PUT PRNs INTO CLOCK LIST
! ------------------------
      DO jSat=1,15
        READ(cSat(jsat),'(A1,I2)',iostat=ioRead) satChr(jSat),number(jSat)
        IF (ioRead /= 0) THEN
          irCode=4
          EXIT LoopHead1
        ENDIF
        IF (NUMBER(jSat) /= 0) THEN
          IF (iSat==SIZE(ClkHead%ClkName)) THEN
            iSat=0
            IF (ClkHead%nSat /= 0) irCode=10
          ELSE
            iSat=iSat+1
            ClkHead%ClkName(iSat)=''
            write(ClkHead%ClkName(iSat)(1:4),'(A3,1X)') cSat(jSat)
          ENDIF
        ENDIF
      ENDDO
!
! OBS TYPES
! ---------
    ELSE IF (head == 'SYS / # / OBS TYPES ') THEN
      CYCLE
!
! CLK SYSTEM
! ----------
    ELSE IF (head == 'TIME SYSTEM ID      ') THEN
      READ(string,'(3X,A3)',iostat=ioRead) ClkHead%timsys
      IF (ioRead /= 0) THEN
        irCode=4
        EXIT LoopHead1
      ENDIF
!
! DCBs APPLIED
! ------------
    ELSE IF (head == 'SYS / DCBS APPLIED  ') THEN
      READ(string,'(2X,A17,1X,A40)',iostat=ioRead)ClkHead%pgmnam,ClkHead%dcbStr
      IF (ioRead /= 0) THEN
        irCode=4
        EXIT LoopHead1
      ENDIF
!
! PCVs APPLIED
! ------------
    ELSE IF (head == 'SYS / PCVS APPLIED  ') THEN
      READ(string,'(2X,A17,1X,A40)',iostat=ioRead)ClkHead%pgmnam,ClkHead%pcvStr
      IF (ioRead /= 0) THEN
        irCode=4
        EXIT LoopHead1
      ENDIF
!
! BLANK LINE OR END OF HEADER
! ---------------------------
    ELSE IF (head == 'END OF HEADER       ') THEN
      EXIT LoopHead1
    ELSE
!
! UNKNOWN RECORD TYPE
! -------------------
      irCode=7
      EXIT LoopHead1
    END IF
!
! READ NEXT RECORD
! ----------------
  ENDDO LoopHead1

! No station/satellite records announced in the DATA TYPE record
! --------------------------------------------------------------
  found = .FALSE.
  DO ii = 1,iTyp
    found = found .OR. (DataTypes(ii) == 'AR')
  ENDDO
  IF (.NOT. found) ClkHead%nSta = 0

  found = .FALSE.
  DO ii = 1,iTyp
    found = found .OR. (DataTypes(ii) == 'AS')
  ENDDO
  IF (.NOT. found) ClkHead%nSat = 0


! ARE ALL MANDATORY RECORDS AVAILABLE (not all checked)
! -----------------------------------
  IF (irCode == 0) THEN
    IF (.NOT. isAC           .OR.          &   ! No "AC" record
        ClkHead%NumTyp == -1 .OR.          &   ! No "# Data Types" record
!        ClkHead%nRef(1)== -1 .OR.          &   ! No "# Ref Stations" record
        ClkHead%nSta   == -1 .OR.          &   ! No "# Station" record
        ClkHead%nSat   == -1) irCode=6         ! No "# Satellite" record
  ENDIF
!
! CHECK NUMBER OF ITEMS FOUND IN HEADER
! -------------------------------------
  IF (irCode == 0) THEN
    IF (ClkHead%Numtyp /= iTyp .OR.       &  ! Check # of Data Types found
!        ClkHead%nRef(1)/= iRef .OR.       &  ! Check # of Ref Clocks found
        ClkHead%nSta   /= iSta .OR.       &  ! Check # of Station found
        ClkHead%nSat   /= iSat-iSta)      &  ! Check # of Satellites found
        irCode=10
  ENDIF
!
! Handle Error codes
! ------------------
!
! WRONG FILE TYPE
! ---------------
  IF (irCode == 1)                                                  &
    WRITE(lfnmsg,'(/,A,2(/,16X,A,A),//)')                           &
          ' ### SR RDCRXH: WRONG FILE TYPE',                        &
                          'EXPECTED TYPE: ',RXType,                 &
                          'DETECTED TYPE: ',RnxTyp
!
! NOT ANTICIPATED VERSION NUMBER
! ------------------------------
  IF (irCode == 2)                                                  &
    WRITE(lfnmsg,'(/,A,2(/,16X,A,F5.2),//)')                        &
          ' ### SR RDCRXH: INCORRECT RINEX VERSION NUMBER',         &
                          'EXPECTED NUMBER: ',RXVers,               &
                          'DETECTED NUMBER: ',ClkHead%RnxVers
!
! END OF FILE WITHIN HEADER
! -------------------------
  IF (irCode == 3)                                                  &
    WRITE(lfnmsg,'(/,A,//)')                                        &
          ' ### SR RDCRXH: END OF FILE WITHIN HEADER'
!
! ERROR DECODING DATA
! -------------------
  IF (irCode == 4)                                                  &
    WRITE(lfnmsg,'(/,A,/,1X,2A,/,A,I3,//)')                         &
          ' ### SR RDCRXH: ERROR DECODING THE FOLLOWING LINE',      &
          string,head,                                              &
          ' HEADERLINE ',nLine+1
!
! FIRST LINE NOT VERSION LINE
! ---------------------------
  IF (irCode == 5)                                                  &
    WRITE(lfnmsg,'(/,A,/,1X,2A,//)')                                &
          ' ### SR RDCRXH: FIRST LINE NOT "VERSION"-LINE:',         &
          string, head
!
! MANDATORY RECORD(S) MISSING
! ---------------------------
  IF (irCode == 6) THEN
    WRITE(lfnmsg,'(/,A)') ' ### SR RDCRXH: MANDATORY RECORD(S) MISSING:'
    IF (.NOT. isAC)           WRITE(lfnmsg,'(16X,A)') '"ANALYSIS CENTER"'
    IF (ClkHead%NumTyp == -1)  WRITE(lfnmsg,'(16X,A)') '"# / TYPES OF DATA"'
!    IF (ClkHead%nref(1)==-1)  WRITE(lfnmsg,'(16X,A)') '"# OF CLK REF"'
    IF (ClkHead%nSta   == -1)  WRITE(lfnmsg,'(16X,A)') '"# OF SOLN STA / TRF"'
    IF (ClkHead%nSat   == -1)  WRITE(lfnmsg,'(16X,A)') '"# OF SOLN SATS"'
    WRITE(lfnmsg,'(/)')
  ENDIF
!
! UNKNOWN RECORD TYPE
! -------------------
  IF (irCode == 7)                                                  &
    WRITE(lfnmsg,'(/,A,/,1X,2A,//)')                                &
          ' ### SR RDCRXH: UNKNOWN HEADER RECORD TYPE:',            &
          string,head
!
! FEATURE NOT YET HANDLED
! -----------------------
  IF (irCode == 8)                                                  &
      WRITE(lfnmsg,'(/,A,/,1X,2A,//)')                              &
            ' ### SR RDCRXH: FEATURE NOT YET HANDLED IN THE LINE:', &
            string, head
!
! ERROR READING FILE
! ------------------
  IF (irCode == 9)                                                  &
      WRITE(lfnmsg,'(/,A,//)')                                      &
            ' ### SR RDCRXH: ERROR READING CLOCK RINEX FILE'
!
! MANDATORY RECORD(S) MISSING
! ---------------------------
  IF (irCode == 10) THEN
    WRITE(lfnmsg,'(/,A)') ' ### SR RDCRXH: WRONG ENTRY IN RINEX HEADER'
    IF (ClkHead%numTyp /= iTyp)  WRITE(lfnmsg,'(16X,A)') '"# / TYPES OF DATA"'
!    IF (ClkHead%nref(1)/= iRef)  WRITE(lfnmsg,'(16X,A)') '"# OF CLK REF"'
    IF (ClkHead%nSta   /= iSta)  WRITE(lfnmsg,'(16X,A)') '"# OF SOLN STA / TRF"'
    IF (ClkHead%nSat   /= iSat-iSta)  WRITE(lfnmsg,'(16X,A)') '"# OF SOLN SATS"'
    WRITE(lfnmsg,'(/)')
  ENDIF
!
  RETURN
  END SUBROUTINE

END MODULE
