
! -------------------------------------------------------------------------
! Bernese GNSS Software Version 5.2
! -------------------------------------------------------------------------

PROGRAM ccrnxc

! -------------------------------------------------------------------------
! Purpose:    combination of clock rinex files
!
! Author:     R.Dach
!
! Created:    27-Apr-2000
!
! Changes:    22-May-2000 RD: writes combination also as Bernese Satellite
!                             Clock File
!             27-Jul-2000 RD: Program was prepared for the new menu system
!             14-Aug-2000 RD: all array are dynamic
!             13-Feb-2001 RD: flexibel handling of reference clock
!             14-Feb-2001 RD: use SR alcerr
!             18-Feb-2001 RD: referenc clock selection
!             27-Feb-2001 RD: interpolation/extrapolation
!             14-May-2001 RD: Improved structure od reference clock array
!             17-May-2001 RD: Bugfix for open window
!             18-Jun-2001 RD: select only one reference clock
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             01-Feb-2001 RD: Read header of input files in CCLKIN
!             25-Sep-2002 HU: Remove i_astlib
!             03-Oct-2002 RD: Check leap sec only if given in both files
!             23-Apr-2003 HU: Nullify local pointers
!             18-May-2003 HU: Initialize structures
!             19-May-2003 RD: Init time window to (/0d0,1d20/)
!             06-Jul-2003 RD: An input file may contain no data records
!             15-Jul-2003 RD: Compute nEpo with "NINT"
!             12-Nov-2003 RD: Bugfix: no epochs in users time window
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             02-Dec-2003 HB: Initialize clock-values and -sigmas with
!                             unDef = 999999.999999D0
!             09-Dec-2003 RD: Reset numRef only if searching a new ref-clock
!             15-Jan-2004 HB: Write reference clocks also in the case of
!                             selected NONE for this clock type
!             21-Jan-2004 RD: Run ccjest only on request (new panel)
!             12-Mar-2004 RD: Write output files even if no ref-clock avail.
!             12-Mar-2004 RD: Extract only satellite clocks
!             06-Jul-2004 RD: Correct transfer of epochs from input to output file
!             15-Jul-2004 RD: Read coordinates from input file
!             17-Aug-2004 HU: Initialize HlpClkHead
!             05-Apr-2005 RD: Source code cosmetics
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             27-Feb-2007 AG: Call DEFCON with parameter
!             12-Aug-2009 RD: Add check for timsys, leap sec err->warn.
!             21-Sep-2009 RD: Eclipsing flag added
!             19-Jul-2010 SL: tab characters removed
!             23-Sep-2010 RD: Enable CPU counter
!             20-Jul-2011 RD: Extract RXCBV3 function to pgm RNXCLK
!             01-Sep-2011 LP: Write also GALILEO records into the SAT CLK file
!             14-Nov-2011 SL: use m_bern with ONLY, PRITIT call added
!             01-May-2012 LP: Handle also SBAS, COMPASS, QZSS
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, fileNameLength, staNameLength, &
                      lfnLoc, lfnErr, lfn001, lfn002
  USE m_cpu,    ONLY: cpu_start
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE d_const,  ONLY: date,time
  USE d_clkrnx, ONLY: t_clkhead,t_clkrec,unDef,init_clkhead
  USE p_ccrnxc, ONLY: t_ccrnxc_opt,t_ccrnxc_fil,t_Jump,    &
                      init_ccrnxc_opt,init_ccrnxc_fil
  USE s_alcerr
  USE s_opnfil
  USE s_ccjump
  USE s_wtcrxr
  USE s_ccombo
  USE s_ccextr
  USE s_mjdgps
  USE s_ccalig
  USE s_readinpf
  USE s_opnerr
  USE s_cc1ref
  USE s_getco3
  USE s_cclkin
  USE s_ccjest
  USE s_ccnlst
  USE s_rdcrxh
  USE s_defcon
  USE s_pritit
  USE s_exitrc
  USE s_ccstat
  USE s_opnsys
  USE s_rdcrxr
  USE s_gtflna
  USE s_clrflg
  USE s_wtcrxh
  USE f_tstflg
  IMPLICIT NONE
!
! Local variables
! ---------------
  CHARACTER(LEN=fileNameLength)  :: crdFil     ! Name of the COORD-file
  CHARACTER(LEN=staNameLength),   &
         DIMENSION(:),   POINTER :: stName     ! Station list in COORD-file
  CHARACTER(LEN=16)              :: datum      ! Datum string in COORD-file

  INTEGER(i4b)                   :: iFil, jFil ! Counts the file in list
  INTEGER(i4b)                   :: nFil       ! Number of files in list
  INTEGER(i4b)                   :: iSta, jSta ! Counts the stations in list
  INTEGER(i4b)                   :: iCoord     ! Counter for coordinate
                                               ! components
  INTEGER(i4b)                   :: iSat       ! Counts the satellites in list
  INTEGER(i4b)                   :: iClk, jClk ! Counts clocks
  INTEGER(i4b)                   :: nClk       ! Number of clocks
  INTEGER(i4b)                   :: iEpo       ! Counts the epoch numbers in
                                               ! list
  INTEGER(i4b)                   :: irCode     ! Return code from the sr
  INTEGER(i4b)                   :: GPSWeek    ! GPS week for output in
                                               ! sat-clk file
  INTEGER(i4b)                   :: iDummy     ! A dummy variable
  INTEGER(i4b)                   :: ii
  INTEGER(i4b)                   :: ios, iac   ! io status
  INTEGER(i4b)                   :: deltaT     ! Sampling rate for output file
  INTEGER(i4b)                   :: nJump      ! Number of clock jumps
  INTEGER(i4b)                   :: iRef, jRef ! Counter for reference clocks
  INTEGER(i4b)                   :: ircCrd     ! Availablility of the COORD-file
  INTEGER(i4b)                   :: nStat      ! Num. of stations in COORD-file
!
  REAL(r8b)                      :: GPSSec     ! Sec. of GPS week for output
                                               ! in sat-clk file
  REAL(r8b)                      :: lastEpo    ! Laste epoch of clk rinex files
  REAL(r8b),                      &
         DIMENSION(:,:), POINTER :: xStat      ! Coordinates in COORD-file
!
  LOGICAL                        :: Combi=.TRUE. ! Combination possible?
  LOGICAL                        :: isRef
!
! Variables for clock rinex header
! --------------------------------
  TYPE(t_clkhead)  ,DIMENSION(:), POINTER     :: InClkHead
  TYPE(t_clkhead)                             :: OutClkHead
  TYPE(t_clkhead)                             :: HlpClkHead
!
! Variables for the clock rinex data records
! ------------------------------------------
  TYPE(t_clkrec)   ,DIMENSION(:), POINTER     :: InClkRec
  TYPE(t_clkrec)                              :: OutClkRec
!
! Program specific types
! ----------------------
  TYPE(t_ccrnxc_opt)    :: CCopt     ! all input options
  TYPE(t_ccrnxc_fil)    :: CCfil     ! clock rinex file names
  TYPE(t_Jump), DIMENSION(:), POINTER          &
                        :: ClkJump   ! Detected clock jumps

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Nullify pointers
! ----------------
  NULLIFY(InClkHead)
  NULLIFY(InClkRec)
  NULLIFY(ClkJump)
  NULLIFY(stName)
  NULLIFY(xstat)
  CALL init_ccrnxc_opt(CCopt)
  CALL init_ccrnxc_fil(CCfil)
  CALL init_clkhead(HlpClkHead)
  CALL init_inpkey(inpKey)

! Get the name of the input file
! ------------------------------
  CALL readinpf(' ',inpKey)

! Open system files, define constants
! -----------------------------------
  CALL opnsys
  CALL defcon(0)
  CALL pritit('CCRNXC','Combine/manipulate clock RINEX files',130)
!
! Get the new creation date
! -------------------------
   OutClkHead%CrDate = date // ' ' // time
!
! Read input file
! ---------------
  CALL cclkin(CCopt,CCfil,OutClkHead,InClkHead,irCode)
  IF (irCode /= 0) CALL exitrc(2)

! Do some initializations
! -----------------------
  CCopt%MaxRMS=-99D0
  CCopt%MaxDel=-99
  CCopt%ninterval=0
  CCopt%SumRMS=0D0
  CCopt%NumDel=0
!
  nFil=SIZE(CCfil%ClkFilNam)
  ALLOCATE (InClkRec(nFil),stat=ios)
  CALL alcerr(ios,'InClkRec',(/nFil/),'ccrnxc')
!
! Read the headers of all clock rinex files in list
! -------------------------------------------------
  FileLoop0: DO iFil=1,nFil
!
! Check station clocks from clock list
! ------------------------------------
    iSta=1
    DO WHILE (iSta <= inClkHead(iFil)%nSta)
      jClk=0
      DO iClk = 1,SIZE(CCopt%clklst)
        IF (CCopt%clklst(iClk) == 'STA:ALL' .OR. &
            CCopt%clklst(iClk) == InClkHead(iFil)%ClkName(iSta))   jClk = 1
      ENDDO
      IF (CCopt%refFil .AND. jClk == 0) THEN
        DO jFil=1,nFil
          DO iRef = 1,InClkHead(jfil)%numRef
            DO jRef = 1,InClkHead(jfil)%ref(iRef)%nRef
              IF (InClkHead(jfil)%Ref(iRef)%clk(jRef)%Name == InClkHead(iFil)%ClkName(iSta))&
                   jClk = 1
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF (jClk == 0) THEN
        InClkHead(iFil)%ClkName(iSta:inClkHead(iFil)%nSta+inClkHead(iFil)%nSat-1) = &
                     InClkHead(iFil)%ClkName(iSta+1:inClkHead(iFil)%nSta+inClkHead(iFil)%nSat)
        InClkHead(iFil)%StaCoord(1:3,iSta:inClkHead(iFil)%nSta-1) = &
                     InClkHead(iFil)%StaCoord(1:3,iSta+1:inClkHead(iFil)%nSta)
        InClkHead(iFil)%nSta=inClkHead(iFil)%nSta-1
      ELSE
        iSta=iSta+1
      ENDIF
    ENDDO
!
! Check satellite clocks from clock list
! ------------------------------------
    iSat=inClkHead(iFil)%nSta+1
    DO WHILE (iSat <= inClkHead(iFil)%nSta+inClkHead(iFil)%nSat)
      jClk=0
      DO iClk = 1,SIZE(CCopt%clklst)
        IF (CCopt%clklst(iClk) == 'SAT:ALL' .OR. &
            CCopt%clklst(iClk) == InClkHead(iFil)%ClkName(iSat)) jClk = 1
      ENDDO
      IF (CCopt%refFil .AND. jClk == 0) THEN
        DO jFil=1,nFil
          DO iRef = 1,InClkHead(jFil)%numRef
            DO jRef = 1,InClkHead(jFil)%ref(iRef)%nRef
              IF (InClkHead(jFil)%Ref(iRef)%clk(jRef)%Name == InClkHead(iFil)%ClkName(iSta))&
                   jClk = 1
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF (jClk == 0) THEN
        InClkHead(iFil)%ClkName(iSat:inClkHead(iFil)%nSta+inClkHead(iFil)%nSat-1) = &
                     InClkHead(iFil)%ClkName(iSat+1:inClkHead(iFil)%nSta+inClkHead(iFil)%nSat)
        inClkHead(iFil)%nSat=inClkHead(iFil)%nSat-1
      ELSE
        iSat=iSat+1
      ENDIF
    ENDDO
!
  ENDDO FileLoop0
  IF (irCode /= 0) CALL exitrc(2)

! Use an external coordinate file
! -------------------------------
  CALL gtflna(0,'COORD',crdFil,ircCrd)

  IF (ircCrd == 0) THEN
    CALL getco3(crdFil,1,(/'@'/),nStat,stName,datum=datum,xstat=xstat)

    DO iFil = 1,nFil

      IF (InClkHead(iFil)%TRFName /= datum)                          &
         WRITE(LFNERR,'(/,A,/,A,/,A,A,/,A,A,//)')                    &
         ' ### PGM CCRNXC: Adapt Terrestrial Reference Frame of',    &
         '                 the clock RINEX file to the coord. file', &
         '                 Clock RINEX file: ',TRIM(CCfil%ClkFilNam(iFil)),         &
         '                 Coordinate file : ',TRIM(crdFil)

      InClkHead(iFil)%TRFName = datum

      DO iSta = 1,InClkHead(iFil)%nSta
        DO jSta = 1,nStat
          IF (InClkHead(iFil)%ClkName(iSta) == stName(jSta)) THEN

            IF ((DABS(InClkHead(iFil)%StaCoord(1,iSta)-xstat(1,jSta))>= &
                                                         CCopt%maxDcrd) .OR. &
                (DABS(InClkHead(iFil)%StaCoord(2,iSta)-xstat(2,jSta))>= &
                                                         CCopt%maxDcrd) .OR. &
                (DABS(InClkHead(iFil)%StaCoord(3,iSta)-xstat(3,jSta))>= &
                                                          CCopt%maxDcrd))    &
              WRITE(LFNERR,'(/,A,/,16X,A,/,3(16X,A,A,/))')                   &
              ' ### PGM CCRNXC: Adapt station coordinates of the',           &
              'clock RINEX file to the coord. file',                         &
              'Clock RINEX file: ',TRIM(CCfil%ClkFilNam(iFil)),              &
              'Coordinate file : ',TRIM(crdFil),                             &
              'Station Name    : ',TRIM(InClkHead(iFil)%ClkName(iSta))

            InClkHead(iFil)%StaCoord(1:3,iSta) = xstat(1:3,jSta)
          ENDIF
        ENDDO
      ENDDO
    ENDDO

    DEALLOCATE(stName,stat=iac)
    DEALLOCATE(xstat,stat=iac)
  ENDIF

! Check the compatibitily
! -----------------------
  FileLoop1: DO iFil=1,nFil-1
    FileLoop2: DO jFil=iFil+1,nFil
      IF (iFil == 1) THEN
!
! Check the LeapSeconds (only once)
! ---------------------
        IF (InClkHead(iFil)%LeapSec /= 0D0 .AND. &
            InClkHead(jFil)%LeapSec /= 0D0) THEN
          ! Error in case of timescales containing leap seconds
          IF ( InClkHead(iFil)%timsys == 'GLO' .OR. &
               InClkHead(iFil)%timsys == 'UTC' .OR. &
               InClkHead(jFil)%timsys == 'GLO' .OR. &
               InClkHead(jFil)%timsys == 'UTC' ) THEN
            Combi = Combi .AND. (InClkHead(iFil)%LeapSec==InClkHead(jFil)%LeapSec)
            IF (InClkHead(iFil)%LeapSec /= InClkHead(jFil)%LeapSec)         &
               WRITE(LFNERR,'(/,A,A,/,A,A,/,A,A,//)')                       &
               ' *** PGM CCRNXC: Leap seconds from the following files ',   &
                                 'are not equal:',                          &
               '                 File 1: ',TRIM(CCfil%ClkFilNam(iFil)),     &
               '                 File 2: ',TRIM(CCfil%ClkFilNam(jFil))
          ELSE
            IF (InClkHead(iFil)%LeapSec /= InClkHead(jFil)%LeapSec)         &
               WRITE(LFNERR,'(/,A,A,/,A,A,/,A,A,//)')                       &
               ' ### PGM CCRNXC: Leap seconds from the following files ',   &
                                 'are not equal:',                          &
               '                 File 1: ',TRIM(CCfil%ClkFilNam(iFil)),     &
               '                 File 2: ',TRIM(CCfil%ClkFilNam(jFil))
          ENDIF
        ENDIF
!
! Time system (only once)
! -----------------------
        IF (InClkHead(iFil)%timsys /= ' ' .AND. &
            InClkHead(jFil)%timsys /= ' ' ) THEN
          Combi = Combi .AND. (InClkHead(iFil)%timsys==InClkHead(jFil)%timsys)
          IF (InClkHead(iFil)%timsys /= InClkHead(jFil)%timsys)       &
             WRITE(LFNERR,'(/,A,/,A,/,A,A,/,A,A,//)')                   &
             ' *** PGM CCRNXC: Time system tag for the epochsin the',   &
             '                 following files are not identical:',     &
             '                 File 1: ',TRIM(CCfil%ClkFilNam(iFil)),         &
             '                 File 2: ',TRIM(CCfil%ClkFilNam(jFil))
        ENDIF
!
! Check the Reference Frame (only once)
! -------------------------
        Combi = Combi .AND. (InClkHead(iFil)%TRFName==InClkHead(jFil)%TRFName)
        IF (InClkHead(iFil)%TRFName /= InClkHead(jFil)%TRFName)       &
           WRITE(LFNERR,'(/,A,/,A,/,A,A,/,A,A,//)')                   &
           ' *** PGM CCRNXC: Terrestrial Reference Frame from the',   &
           '                 following files are not identical:',     &
           '                 File 1: ',TRIM(CCfil%ClkFilNam(iFil)),         &
           '                 File 2: ',TRIM(CCfil%ClkFilNam(jFil))
      ENDIF
!
! Check the coordinates (for every combination)
! ---------------------
      IF (ircCrd == 0) CYCLE FileLoop2

      StaLoop1: DO iSta=1,InClkHead(iFil)%nSta
        StaLoop2: DO jSta=1,InClkHead(jFil)%nSta
          IF ((InClkHead(iFil)%ClkName(iSta) == &
               InClkHead(jFil)%ClkName(jSta))) THEN
            DO iCoord=1,3
              Combi = Combi .AND.                                   &
                      (DABS(InClkHead(iFil)%StaCoord(iCoord,iSta) - &
                            InClkHead(jFil)%StaCoord(iCoord,jSta))< &
                            CCopt%maxDcrd)
            ENDDO
            IF ((DABS(InClkHead(iFil)%StaCoord(1,iSta)-                      &
                      InClkHead(jFil)%StaCoord(1,jSta))>=CCopt%maxDcrd) .OR. &
                (DABS(InClkHead(iFil)%StaCoord(2,iSta)-                      &
                      InClkHead(jFil)%StaCoord(2,jSta))>=CCopt%maxDcrd) .OR. &
                (DABS(InClkHead(iFil)%StaCoord(3,iSta)-                      &
                      InClkHead(jFil)%StaCoord(3,jSta))>=CCopt%maxDcrd))     &
              WRITE(LFNERR,'(/,A,A,/,A,A,/,A,A,/,A,A,//)')                   &
              ' *** PGM CCRNXC: Station Coordinates from the following ',    &
                                'files are not equal:',                      &
              '                 File 1:       ',TRIM(CCfil%ClkFilNam(iFil)), &
              '                 File 2:       ',TRIM(CCfil%ClkFilNam(jFil)), &
              '                 Station Name: ',                             &
                                        TRIM(InClkHead(iFil)%ClkName(iSta))
          ENDIF
        ENDDO StaLoop2
      ENDDO StaLoop1
    ENDDO FileLoop2
  ENDDO FileLoop1
!
! Exit if the case of an error
! ----------------------------
  IF (.NOT. Combi) CALL EXITRC(2)
!
! Generate new lists for the combined clock rinex file
! ----------------------------------------------------
  IF (Combi) THEN
    CALL ccnlst(CCopt, CCfil, InClkHead, OutClkHead, irCode)
    Combi=Combi .AND. (irCode == 0)
  ENDIF
!
! Continue the processing only, if no error up to here
! ----------------------------------------------------
  IF (Combi) THEN
!
! Loop over all clock rinex files to read the data
! ------------------------------------------------
    FileLoop4: DO iFil=1,nFil

! Open the clock rinex file for reading the records
! -------------------------------------------------
      CALL opnfil(lfnloc,CCfil%ClkFilNam(iFil),'OLD','FORMATTED','READONLY',' ',irCode)
      CALL opnerr(lfnerr,lfnloc,irCode,CCfil%ClkFilNam(iFil),'CCRNXC')
!
! Read the header information into a dummy record
! -----------------------------------------------
      IF (irCode==0) THEN
        HlpClkHead%TFirst=0d0
        CALL rdcrxh(lfnloc,lfnerr,HlpClkHead,irCode)
!
        DEALLOCATE (HlpClkHead%Comment,stat=iDummy)
        DEALLOCATE (HlpClkHead%DatTyp,stat=iDummy)
        DEALLOCATE (HlpClkHead%Ref,stat=iDummy)
        DEALLOCATE (HlpClkHead%ClkName,stat=iDummy)
        DEALLOCATE (HlpClkHead%StaCoord,stat=iDummy)
      ENDIF
!
! Read the data records from clock rinex file
! -------------------------------------------
      IF (irCode==0) THEN
        CALL rdcrxr(lfnloc,lfnerr,CCopt%TimeWin,                     &
                    InClkHead(iFil),InClkRec(iFil),irCode)
      ENDIF
      IF (irCode/=0) InClkRec(iFil)%nEpo=-1
      CLOSE(lfnloc)
!
! Remove the clocks with eclips flags
! -----------------------------------
      IF (CCopt%delEcl /= 0) THEN
        DO iEpo = 1,InClkRec(iFil)%nEpo
          DO iClk = 1,InClkHead(iFil)%nSta + InClkHead(iFil)%nSat
            IF (tstflg(InClkRec(iFil)%clkFlg(iClk,iEpo),0)) THEN
              IF (CCopt%delEcl == 1) THEN
                CALL clrflg(InClkRec(iFil)%clkFlg(iClk,iEpo),0)
              ELSEIF (CCopt%delEcl == 2) THEN
                InClkRec(iFil)%clock(iClk,iEpo) = undef
                InClkRec(iFil)%sigma(iClk,iEpo) = undef
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDIF
    ENDDO FileLoop4
!
! Allocate the output data records
! --------------------------------
    IF (CCopt%refFil) THEN
      OutClkHead%TFirst=InClkHead(1)%TFirst+InClkRec(1)%Epoch(1)/86400d0
      OutClkRec%nEpo=InClkRec(1)%nEpo
      DeltaT=99999
      DO iEpo=1,InClkRec(1)%nEpo-1
        IF (DeltaT > &
            NINT(InClkRec(1)%Epoch(iEpo+1)-InClkRec(1)%Epoch(iEpo))) &
          DeltaT = NINT(InClkRec(1)%Epoch(iEpo+1) - &
                        InClkRec(1)%Epoch(iEpo))
      ENDDO
      IF (CCopt%DeltaT /= 0) DeltaT=CCopt%DeltaT
    ELSE
      lastEpo=0d0
      OutClkHead%TFirst=99999d0
      DeltaT=99999
      DO iFil=1,nFil
        IF (InClkRec(iFil)%nEpo <= 0) CYCLE
        IF (OutClkHead%TFirst > &
            inClkHead(iFil)%TFirst+inClkRec(iFil)%Epoch(1)/86400D0) THEN
          OutClkHead%TFirst=inClkHead(iFil)%TFirst + &
                            inClkRec(iFil)%Epoch(1)/86400D0
        ENDIF
        IF (lastEpo<inClkHead(iFil)%TFirst + &
                    InClkRec(iFil)%Epoch(InClkRec(iFil)%nEpo)/86400d0) THEN
          lastEpo=inClkHead(iFil)%TFirst + &
                  InClkRec(iFil)%Epoch(InClkRec(iFil)%nEpo)/86400d0
        ENDIF
        DO iEpo=1,InClkRec(iFil)%nEpo-1
          IF (DeltaT > &
              NINT(InClkRec(iFil)%Epoch(iEpo+1)-InClkRec(iFil)%Epoch(iEpo))) &
            DeltaT = NINT(InClkRec(iFil)%Epoch(iEpo+1) - &
                          InClkRec(iFil)%Epoch(iEpo))
        ENDDO
      ENDDO
      IF (CCopt%DeltaT /= 0) DeltaT=CCopt%DeltaT
      IF (OutClkHead%TFirst /= 99999d0) THEN
        OutClkRec%nEpo=NINT((lastEpo-OutClkHead%TFirst)*86400d0/DeltaT)+1
      ELSE
        OutClkRec%nEpo=0
      ENDIF
    ENDIF

! Use time window from input panel
! --------------------------------
    IF (OutClkHead%TFirst /= 99999d0) THEN
      IF (CCopt%TimeWin(1) /= 0D0 .AND. &
          (CCopt%doExtra .OR. CCopt%TimeWin(1) > OutClkHead%TFirst)) THEN
        OutClkRec%nEpo = OutClkRec%nEpo - &
                         NINT((CCopt%TimeWin(1) - OutClkHead%TFirst)*86400D0/DeltaT)
        OutClkHead%TFirst = CCopt%TimeWin(1)
      ENDIF
  !
      IF (CCopt%TimeWin(2) /= 1D20 .AND. &
          (CCopt%doExtra .OR. CCopt%TimeWin(2) < OutClkHead%TFirst + &
                             DBLE(OutClkRec%nEpo-1)*DBLE(DeltaT)/86400d0)) THEN
        OutClkRec%nEpo = NINT((CCopt%TimeWin(2)-OutClkHead%TFirst)*86400D0/DeltaT)+1
      ENDIF
    ENDIF
!
    ALLOCATE(OutClkRec%Epoch(OutClkRec%nEpo),stat=ios)
    CALL alcerr(ios,'OutClkRec%Epoch',(/OutClkRec%nEpo/),'ccrnxc')
    DO iEpo=1,OutClkRec%nEpo
      IF (CCopt%refFil) THEN
        OutClkRec%Epoch(iEpo)=InClkRec(1)%Epoch(iEpo)
      ELSE
        OutClkRec%Epoch(iEpo)=DeltaT*(iEpo-1)
      ENDIF
    ENDDO
!
    nClk = OutClkHead%nSta+OutClkHead%nSat
    ALLOCATE(OutClkRec%Clock(nClk,OutClkRec%nEpo),stat=ios)
    CALL alcerr(ios,'OutClkRec%StaClock',(/nClk,OutClkRec%nEpo/),'ccrnxc')
    ALLOCATE(OutClkRec%Sigma(nClk,OutClkRec%nEpo),stat=ios)
    CALL alcerr(ios,'OutClkRec%Sigma',(/nClk,OutClkRec%nEpo/),'ccrnxc')
    ALLOCATE(OutClkRec%clkFlg(nClk,OutClkRec%nEpo),stat=ios)
    CALL alcerr(ios,'OutClkRec%clkFlg',(/nClk,OutClkRec%nEpo/),'ccrnxc')

    OutClkRec%Clock=unDef
    OutClkRec%Sigma=unDef
    DO iEpo = 1,OutClkRec%nEpo
      DO iClk = 1,nClk
        DO ii = 0,7
          CALL clrflg(OutClkRec%clkFlg(iClk,iEpo),ii)
        ENDDO
      ENDDO
    ENDDO

! Init first/last epoch with clock data
! -------------------------------------
    CCopt%clkData%t(2) = 0d0
    CCopt%clkData%t(1) = 1d20

! Loop over the epochs of the output file
! (other epochs cannot be included because no information for the reference)
! --------------------------------------------------------------------------
    DO iEpo=1,OutClkRec%nEpo
      CALL ccombo(iEpo,iEpo,CCopt,CCfil,             &
           InClkHead,OutClkHead,InClkRec,OutClkRec,irCode)
    ENDDO
    IF (CCopt%nInterval == 0) irCode = 0
!
! Do an alignment for the new reference stations
! ----------------------------------------------
    IF (.NOT. CCopt%refFil) THEN
      OutClkHead%numRef = 0
      IF (outClkRec%nEpo > 0) THEN
        IF (CCopt%oneRef) THEN
          CALL cc1ref(CCopt, OutClkHead, OutClkRec, irCode)
        ELSE
          CALL ccalig(CCopt, OutClkHead, OutClkRec, irCode)
        ENDIF
      ENDIF
    ENDIF

!
! Detect clock jumps
! ------------------
    nJump=0
    CALL ccjump(CCopt, OutClkHead, OutClkRec, nJump, ClkJump, irCode)
    IF (irCode == 0 .AND. nJump > 0 .AND. CCopt%nJmpPoly >= 0) &
      CALL ccjest(CCopt, OutClkHead, OutClkRec, nJump, ClkJump, irCode)

! Do interpolation/extrapolation
! ------------------------------
    IF (irCode == 0) &
      CALL ccextr(CCopt, OutClkHead, OutClkRec, nJump, ClkJump, irCode)


! Write some statistics to the protocoll file
! -------------------------------------------
    IF (irCode == 0)                                                 &
      CALL ccstat(CCopt, OutClkHead, OutClkRec, InClkHead, InClkRec, &
                  nJump, ClkJump, irCode)
!
! Remove Interpolation/Extrapolation flag
! ---------------------------------------
    OutClkRec%Sigma = DABS(OutClkRec%Sigma)


! OPEN SATELLITE CLOCK OUTPUT FILE
! --------------------------------
    IF (irCode == 0 .AND. LEN_TRIM(CCfil%SatFilNam)>1) THEN
      CALL opnfil (lfn002,CCfil%SatFilNam,'UNKNOWN','FORMATTED',' ',' ',irCode)
      CALL opnerr(lfnerr,lfn002,irCode,CCfil%SatFilNam,'CCRNXC')
      IF (irCode.eq.0) THEN
        WRITE(lfn002,'(A64,1X,A15,/,80(''-''),//,A,7X,A,/)')                &
               CCopt%Title,OutClkHead%CrDate,                               &
               'SAT WEEK   TOC #PAR     A0 (SEC)',                          &
               'A1 (SEC/SEC)    A2 (SEC/SEC**2)'

        DO iEpo=1,OutClkRec%nEpo
          CALL mjdgps(OutClkHead%TFirst+OutClkRec%Epoch(iEpo)/86400D0,GPSSec,GPSWeek)
          DO iSat=OutClkHead%nSta+1,OutClkHead%nSta+OutClkHead%nSat
            IF (OutClkRec%Clock(iSat,iEpo) /= unDef) THEN
              IF (OutClkHead%ClkName(iSat)(1:1) == 'G') THEN
                WRITE(lfn002,'(A,1X,I4,1X,F8.0,2X,I1,1X,10D17.9)')           &
                   ' '//OutClkHead%ClkName(iSat)(2:3),GPSWeek,GPSSec,        &
                   1,OutClkRec%Clock(iSat,iEpo)*1D-6
              ELSE IF (OutClkHead%ClkName(iSat)(1:1) == 'R') THEN
                WRITE(lfn002,'(A,1X,I4,1X,F8.0,2X,I1,1X,10D17.9)')           &
                   '1'//OutClkHead%ClkName(iSat)(2:3),GPSWeek,GPSSec,        &
                   1,OutClkRec%Clock(iSat,iEpo)*1D-6
              ELSE IF (OutClkHead%ClkName(iSat)(1:1) == 'E') THEN
                WRITE(lfn002,'(A,1X,I4,1X,F8.0,2X,I1,1X,10D17.9)')           &
                   '2'//OutClkHead%ClkName(iSat)(2:3),GPSWeek,GPSSec,        &
                   1,OutClkRec%Clock(iSat,iEpo)*1D-6
              ELSE IF (OutClkHead%ClkName(iSat)(1:1) == 'S') THEN
                WRITE(lfn002,'(A,1X,I4,1X,F8.0,2X,I1,1X,10D17.9)')           &
                   '3'//OutClkHead%ClkName(iSat)(2:3),GPSWeek,GPSSec,        &
                   1,OutClkRec%Clock(iSat,iEpo)*1D-6
              ELSE IF (OutClkHead%ClkName(iSat)(1:1) == 'C') THEN
                WRITE(lfn002,'(A,1X,I4,1X,F8.0,2X,I1,1X,10D17.9)')           &
                   '4'//OutClkHead%ClkName(iSat)(2:3),GPSWeek,GPSSec,        &
                   1,OutClkRec%Clock(iSat,iEpo)*1D-6
              ELSE IF (OutClkHead%ClkName(iSat)(1:1) == 'J') THEN
                WRITE(lfn002,'(A,1X,I4,1X,F8.0,2X,I1,1X,10D17.9)')           &
                   '5'//OutClkHead%ClkName(iSat)(2:3),GPSWeek,GPSSec,        &
                   1,OutClkRec%Clock(iSat,iEpo)*1D-6
!
!             ADD_GNSS_HERE
!
              ENDIF
            ENDIF
          ENDDO
        ENDDO

       WRITE(lfn002,'(/)')
       CLOSE(lfn002)
      ENDIF
    ENDIF


! Write out data records into the combined file
! ---------------------------------------------
    IF (irCode == 0 .AND. LEN_TRIM(CCfil%OutFilNam)>1) THEN
      CALL opnfil(lfn001,CCfil%OutFilNam,'UNKNOWN','FORMATTED',' ',' ',irCode)
      CALL opnerr(lfnerr,lfn001,irCode,CCfil%OutFilNam,'CCRNXC')

! Write the file
! --------------
      CALL wtcrxh(lfn001, lfnerr, OutClkHead, irCode)
      CALL wtcrxr(lfn001,lfnerr,OutClkHead,OutClkRec,irCode)
      CLOSE(lfn001)

      isRef = (outClkHead%numRef > 0)
      DO iRef = 1,outClkHead%numRef
        isRef = isRef .AND. (outClkHead%ref(iRef)%nRef > 0)
      ENDDO
      IF (.NOT. isRef) THEN
        WRITE(lfnerr,'(/,A,2(/,16X,A),/)')  ' ### PG CCRNXC: ' //        &
              'No reference clock in the resulting clock RINEX file',    &
              'Change the options that CCRNXC may select one.',          &
              'File name: ' // TRIM(ccFil%outFilNam)
      ENDIF
    ENDIF

  ENDIF ! Combination is possible

! End up with an error if nor reference clock was found
! -----------------------------------------------------
  IF (irCode /= 0) CALL exitrc(2)

  CALL exitrc(0)

END PROGRAM ccrnxc
