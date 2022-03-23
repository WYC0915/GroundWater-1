! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM baslst

! -------------------------------------------------------------------------
! Purpose:    Automatic selection of single difference files from a given
!             list using diffrent criterions (baseline length, Rceiver name,
!             first letter of station names, max resolved ambiguities)
!
! Author:     M.Meindl
!
! Created:    19-Sep-2001
!
! Changes:    30-Sep-2001 HU: Interface of gtstab moved to I_ASTLIB
!             06-Oct-2001 HU: Interface of gtstab moved back to I_GPSLIB
!             14-Mar-2002 MM: New criteria: max resolved ambiguities and
!                                           min baseline length
!             22-Jul-2002 HB: Modified t_obsHead
!             30-Jul-2002 HU: Use interface for alcerr
!             31-Jan-2003 PS: Changed Dimension of sdFile
!             17-Mar-2003 RD: Use structure for station abbreviations
!             15-May-2003 HB: Initialize structures
!             02-Sep-2003 MM: Several changes (baseline list replaced
!                             by direct file selection, minAr added,
!                             firstChar removed)
!             09-Sep-2003 HU: Receiver names chr16 -> chr20
!             22-Oct-2003 MM: Baseline files not longer mandatory
!             03-Nov-2003 RD: Call SR gtabbv instead of SR getabb
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             11-Aug-2004 MM/SS: Delete baseline file if empty
!             18-Aug-2004 HU: Nullify inpFil
!             31-Mar-2005 MF: Exclude receiver name pattern
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             13-Jul-2005 LM: Use modules
!             19-Oct-2006 MM: New option: GPS-only and GNSS baselines
!             27-Feb-2007 AG: Call DEFCON with parameter
!             23-Sep-2010 RD: Enable CPU counter
!             19-Jan-2011 RD: Use GETSTA
!             19-Jul-2011 PS: Create sel file from filename instead
!                             of 2-char abbreviations
!             14-Nov-2011 SL: use m_bern with ONLY, PRITIT call added
!             24-Nov-2011 SL: new title string for pritit
!             28-Mar-2012 RD: Use SVN2CHR as module now
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lineLength, &
                      fileNameLength, fileNameLength80, staNameLength, &
                      lfnPrt, lfnErr, lfnLoc, lfn001, lfn002
  USE m_cpu,    ONLY: cpu_start
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE d_abbrev, ONLY: t_abbrev, init_abbrev
  USE d_gpsobs, ONLY: t_obshead, init_obsHead
  USE s_gtfile2
  USE s_blstin
  USE s_gtabbv
  USE s_opnfil
  USE s_alcerr
  USE s_readabb
  USE s_readinpf
  USE s_opnerr
  USE s_rdhead2
  USE s_stripdir
  USE s_getsta
  USE s_defcon
  USE s_pritit
  USE s_exitrc
  USE s_opnsys
  USE s_gtflna
  USE f_lincount
  USE s_svn2chr
  IMPLICIT NONE

! Local variables
! ---------------
! File names
  CHARACTER(LEN=fileNameLength)              :: filnon, filres, filabb
  CHARACTER(len=fileNameLength80)            :: sdFile
  CHARACTER(LEN=fileNameLength)              :: arSumFil
  CHARACTER(LEN=fileNameLength),                                          &
                      DIMENSION(:,:),POINTER :: inpFil

! Options
  INTEGER(i4b)                               :: meaTyp
  INTEGER(i4b)                               :: satSys
  REAL(r8b)                                  :: maxLen, minLen
  REAL(r8b)                                  :: maxAr, minAr
  CHARACTER(len=20)                          :: rcvrName
  CHARACTER(len=20)                          :: exclpatt       ! exclude pattern

! Stations
  TYPE(t_obshead)                            :: head
  CHARACTER(len=staNameLength),DIMENSION(2)  :: staName
  CHARACTER(len=20),DIMENSION(2)             :: recType
  REAL(r8b)                                  :: dist
  LOGICAL                                    :: goodSta, selBsl, notBsl

! Coordinates
  CHARACTER(len=16)                          :: datum
  REAL(r8b),DIMENSION(3,2)                   :: xStat, xStEll, xStEcc
  REAL(r8b)                                  :: aEll, bEll, scEll
  REAL(r8b),DIMENSION(3)                     :: dxEll, drEll
  INTEGER(i4b),DIMENSION(2)                  :: staNum, iCentr
  INTEGER(i4b)                               :: nStaCrd, nCentr

! Abbreviations
  TYPE(t_abbrev)                             :: abbrev
  INTEGER(i4b),DIMENSION(:),POINTER          :: abbIdx
  CHARACTER(LEN=4),DIMENSION(2)              :: staAbb4
  INTEGER(i4b)                               :: nStaAbb

! Ambiguity resolution
  CHARACTER(LEN=89)                          :: QIF
  CHARACTER(LEN=102)                         :: MWW, MWN
  CHARACTER(LEN=lineLength)                  :: line
  CHARACTER(LEN=staNameLength), &
                    DIMENSION(:),ALLOCATABLE :: arName
  REAL(r8b),DIMENSION(:),ALLOCATABLE         :: arSolv
  REAL(r8b)                                  :: percent
  LOGICAL                                    :: isQIF

! Counters
  INTEGER(i4b)                               :: nFil, iFil
  INTEGER(i4b)                               :: iComp, iSat
  INTEGER(i4b)                               :: numAr, ii, iIndex

! Satellite system
  CHARACTER(LEN=3)                           :: satStr
  CHARACTER(LEN=1)                           :: svnChr
  INTEGER(i4b)                               :: svnMod

! Error/return codes
  INTEGER(i4b)                               :: irc, ioStat, iac

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Nullify pointers
! ----------------
  CALL init_abbrev(abbrev)
  CALL init_obsHead(head)
  CALL init_inpkey(inpKey)
  NULLIFY(inpFil)

! Get the name of the input file
! ------------------------------
  CALL readinpf(' ',inpKey)

! Define system files
! -------------------
  CALL opnsys

! Define constants
! ----------------
  CALL defcon(1)

! Print title
! -----------
  CALL pritit('BASLST','Select baselines')

! Get input options
! -----------------
  CALL blstin(meaTyp,satSys,maxLen,minLen,maxAr,minAr,rcvrName,exclpatt)
  MWW = ' File       Length   #Amb  RMS0   Max/RMS L5 Amb   #Amb  &
         &RMS0   #Amb Res   RECEIVER 1       RECEIVER 2'
  MWN = ' File       Length   #Amb  RMS0   Max/RMS L5 Amb   #Amb  &
         &RMS0   #Amb Res   RECEIVER 1       RECEIVER 2'
  QIF = ' File       Length   #Amb  RMS0   Max/RMS L5 Amb  Max/RMS &
         &L3 Amb   #Amb  RMS0   #Amb Res'

! Get abbreviations
! -----------------
  CALL gtflna(0,'ABBREV',filabb,irc)
  CALL readAbb(filAbb,abbrev)
  NULLIFY(abbIdx)

! Get list with single difference files
! -------------------------------------
  IF (meaTyp==1) THEN
    CALL gtfile2('CSHFIL',1,nFil,inpFil)
  ELSE
    CALL gtfile2('PSHFIL',1,nFil,inpFil)
  ENDIF

! Open outputfile of accepted sd
! ------------------------------
  CALL gtflna(0,'SELBSL',filres,irc)
  IF (irc==0) THEN
    CALL opnfil(lfn001,filres,'NEW','FORMATTED',' ',' ',iostat)
    CALL opnerr(lfnerr,lfn001,iostat,filres,'BASLST')
  ENDIF

! Open outputfile of rejected sd
! ------------------------------
  CALL gtflna(0,'NOTBSL',filnon,irc)
  IF (irc==0) THEN
    CALL opnfil(lfn002,filnon,'NEW','FORMATTED',' ',' ',iostat)
    CALL opnerr(lfnerr,lfn002,iostat,filnon,'BASLST')
  ENDIF

! Get percentage of resolved ambiguities
! --------------------------------------
  IF (maxAr /= -1) THEN
    numAr = lincount('ARSUM',3)-2
    ALLOCATE(arName(numAr),stat=iac)
    CALL alcerr(iac,'arName',(/numAr/),'baslst')
    ALLOCATE(arSolv(numAr),stat=iac)
    CALL alcerr(iac,'arSolv',(/numAr/),'baslst')

! read summary
    CALL gtflna(1,'ARSUM',arSumFil,irc)
    CALL opnfil(lfnloc,arSumFil,'OLD','FORMATTED',' ',' ',iostat)
    CALL opnerr(lfnerr,lfnloc,iostat,arSumFil,'BASLST')

! Mel-Wuebb or QIF?
    READ(lfnloc,'(A)') line
    IF (line == MWN .OR. line == MWW) THEN
      isQIF = .FALSE.
    ELSEIF (line == QIF) THEN
      isQIF =.TRUE.
    ELSE
      WRITE(lfnerr,'(A,/,16X,A,//)')                                       &
        ' *** PG baslst: Invalid ambiguity summary file selected.',        &
                   'Use either a Mel.-Wuebb. or a QIF summary'
      CALL exitrc(2)
    ENDIF
    READ(lfnloc,'(/)')

! Read all lines
    DO ii=1, numAr
      READ(lfnloc,'(A)') line
      IF (isQIF) THEN
        READ(line,'(1X,A8,72X,F4.1)') arName(ii), arSolv(ii)
      ELSE
        READ(line,'(1X,A8,56X,F4.1)') arName(ii), arSolv(ii)
      ENDIF
    ENDDO
    CLOSE (lfnloc)
  ENDIF

  selBsl = .FALSE.
  notBsl = .FALSE.

! Loop over all single difference files
! -------------------------------------
  DO iFil=1,nFil
    goodSta = .TRUE.
    sdFile = inpFil(1,iFil)
    satStr = "   "

! Read header of single difference file
! -------------------------------------
    CALL rdhead2(sdFile, head)
    staName(1) = head%sta(1)%stanam
    staName(2) = head%sta(2)%stanam
    recType(1) = head%sta(1)%rectyp
    recType(2) = head%sta(2)%rectyp

! Check satellite system
! ----------------------
    DO iSat=1,head%nSatel
      CALL svn2chr(head%sat(iSat)%numSat,svnMod,svnChr)
      IF (svnChr == "G") THEN
        satStr(1:1) = "x"
      ELSEIF (svnChr == "R") THEN
        satStr(3:3) = "x"
      ENDIF
    ENDDO
    IF (satSys == 2 .AND. satStr /= "x  ") goodSta = .FALSE.
    IF (satSys == 3 .AND. satStr /= "x x") goodSta = .FALSE.

! Get abbreviations for both stations
! -----------------------------------
    DO iComp=1,2
      CALL gtabbv(0,staName(iComp),1,filAbb,abbrev,nStaAbb,abbIdx)
      IF (nStaAbb == 0) THEN
          WRITE(LFNERR,*) ' ### PG BASLST: NO ABBREVIATION FOUND FOR ',  &
                            staName(iComp)
      ELSE IF (nStaAbb > 1) THEN
          WRITE(LFNERR,*) ' ### PG BASLST: MORE THAN ONE ABBREVIATION ', &
                          'FOUND FOR ' // staName(iComp)
        staAbb4(iComp) = abbrev%abb(abbIdx(nStaAbb))%staAb4
      ELSE
        staAbb4(iComp) = abbrev%abb(abbIdx(1))%staAb4
      ENDIF
    ENDDO

! Check receiver names
! --------------------
    IF (rcvrName /= ' ') THEN
! Compatibility with obs format version < 5
      IF (recType(1)(17:20) == '????') recType(1)(17:20) = '    '
      IF (recType(2)(17:20) == '????') recType(2)(17:20) = '    '
      IF(rcvrName /= recType(1) .OR.                                      &
         rcvrName /= recType(2)     ) goodSta = .FALSE.
    ENDIF

! Check distance
! --------------
    nStaCrd=2
    CALL getsta(nStaCrd,staName,staNum,nCentr,iCentr,                    &
                xStat,xStEll,xStEcc,                                     &
                datum,aEll,bEll,dxEll,drEll,scEll)

! compute distance
    dist = 0.D0
    DO iComp=1,3
      dist = dist + (xStat(iComp,1) - xStat(iComp,2)) **2
    ENDDO
    dist = DSQRT(dist)/1000.D0

    IF (dist > maxLen) goodSta = .FALSE.
    IF (dist < minLen) goodSta = .FALSE.

! Check number of resolved ambiguities
    percent = -1
    IF (maxAr /= -1) THEN
      DO ii=1,numAr
        iIndex = INDEX(sdFile,TRIM(arName(ii)))
        IF (iIndex /= 0) THEN
          percent = arSolv(ii)
          IF (arSolv(ii) > maxAr) goodSta = .FALSE.
          IF (arSolv(ii) < minAr) goodSta = .FALSE.
          EXIT
        ENDIF
      ENDDO
      IF (percent == -1 .AND. minAr > 0) goodSta = .FALSE.
    ENDIF

! Check receiver name pattern for exclusion
! -----------------------------------------
    IF (exclpatt /= ' ' .AND. goodSta) THEN

      ! Compatibility with obs format version < 5
      IF (recType(1)(17:20) == '????') recType(1)(17:20) = '    '
      IF (recType(2)(17:20) == '????') recType(2)(17:20) = '    '

      IF (INDEX(recType(1),TRIM(exclpatt)) /= 0 .OR. &
          INDEX(recType(2),TRIM(exclpatt)) /= 0) goodSta = .FALSE.
    END IF

! Write baseline to according file and to output
! ----------------------------------------------
    CALL stripdir(sdFile)
    IF (goodSta) THEN
      IF (LEN_TRIM(filres)/=0)                                             &
           WRITE(lfn001,'(A4)') sdFile(1:4)
      WRITE(lfnprt,'(A2,2X,A16,2(A4,1X),F7.1,2(2X,A20),2X,F4.1,2X,A3)')    &
      ' S', sdFile, staAbb4(1),staAbb4(2),dist,recType(1),recType(2),      &
            percent,satStr
      selBsl = .TRUE.
    ELSE
      IF (LEN_TRIM(filnon)/=0)                                             &
           WRITE(lfn002,'(A4)') sdFile(1:4)
      WRITE(lfnprt,'(4X,A16,2(A4,1X),F7.1,2(2X,A20),2X,F4.1,2X,A3)')       &
           sdFile, staAbb4(1),staAbb4(2),dist,recType(1),recType(2),       &
           percent,satStr
      notBsl = .TRUE.
    ENDIF


! Next single difference file
! ---------------------------
  ENDDO

! Close all files
! ---------------
  IF (LEN_TRIM(filres)/=0) THEN
    IF (selBsl) THEN
      CLOSE(lfn001)
    ELSE
      CLOSE(lfn001,STATUS='DELETE')
    ENDIF
  ENDIF
  IF (LEN_TRIM(filnon)/=0) THEN
    IF (notBsl) THEN
      CLOSE(lfn002)
    ELSE
      CLOSE(lfn002,STATUS='DELETE')
    ENDIF
  ENDIF

! End program
! -----------
  CALL exitrc(0)

END PROGRAM baslst
