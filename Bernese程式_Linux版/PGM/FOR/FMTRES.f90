! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  PROGRAM fmtres

! -------------------------------------------------------------------------
! Purpose:    Read formatted residual files containing header and
!             residuals and write unformatted header and unformatted
!             residual from
!
! Author:     R.Dach
!
! Created:    21-Oct-1999
!
! Changes:    17-Feb-2000 RD: Abuse satellite number for azi and ele (ts)
!             23-Jan-2001 RD: Use the new menu system
!             15-May-2001 HU: Endless reading loop
!             28-Aug-2002 RD: Handle new formatted residual files
!             23-Apr-2003 HU: Nullify local pointers
!             15-May-2003 HB: Initialize structure
!             25-Aug-2003 CU: Change format string
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             07-Feb-2005 HB: Adopt for ifc-Compiler, Version 8.1
!             21-May-2005 HU: Declaration of 'line' from 255 to 500
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             27-Feb-2007 AG: Call DEFCON
!             25-May-2010 MF: Call sr init_filhead
!             19-Jul-2010 SL: tab characters removed
!             21-Sep-2010 RD: ST2TIM can be used as a module now
!             23-Sep-2010 RD: Enable CPU counter
!             24-Nov-2011 SL: new title string for pritit, m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, fileNameLength, lfnErr, lfnRes, lfn001
  USE m_cpu,    ONLY: cpu_start
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE d_resfil, ONLY: t_resHead,t_resRec,init_reshead,init_filhead
  USE s_gtfile2
  USE s_opnfil
  USE s_defcon
  USE s_alcerr
  USE s_pritit
  USE s_readinpf
  USE s_opnerr
  USE s_prfile
  USE s_st2tim
  USE s_strtim
  USE s_exitrc
  USE s_opnsys
  USE s_wtresh2
  IMPLICIT NONE

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER   :: pgName = 'FMTRES'

! Local Variables
! ---------------
  TYPE(t_resHead) resHed
  TYPE(t_resRec)  resRec
!
  CHARACTER(LEN=fileNameLength), DIMENSION(:,:),POINTER :: filnam
  CHARACTER(LEN=9)                                      :: eleazi
  CHARACTER(LEN=500)                                    :: line

  REAL(r8b)    :: ele,azi

  INTEGER(i4b) :: nFil
  INTEGER(i4b) :: iFil,jFil
  INTEGER(i4b) :: iEle,iAzi
  INTEGER(i4b) :: ios,irc
  INTEGER(i4b) :: nrSat

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Nullify pointers
! -----------------
  CALL init_reshead(resHed)
  CALL init_inpkey(inpKey)
  NULLIFY(filnam)

! Get the name of the input file
! ------------------------------
  CALL readinpf(' ',inpKey)

! Define system files and constants
! ---------------------------------
  CALL opnsys
  CALL defcon(0)

! Print title section
! -------------------
  CALL pritit(pgName,'Convert residual files (ASCII to binary)')

! Get observation file names
! --------------------------
  CALL gtfile2('RESFILE',2,nFil,filnam)
  CALL prfile('RESFILE',' ',2)

! Loop over all files to be unformatted
! -------------------------------------
  DO iFil=1,nFil

! Open formatted residual file
! ----------------------------
    CALL OPNFIL(lfn001,filnam(1,iFil),'OLD',' ','READONLY',' ',ios)
    CALL OPNERR(lfnerr,lfn001,ios,filnam(1,iFil),pgName)

! Read formatted header info (try old format)
! -------------------------------------------
    READ (LFN001,'(A80,//////,4(37X,I8,/),//)',iostat=ios) &
         resHed%title,resHed%dsc%iTyp,resHed%dsc%nDiff,    &
         resHed%dsc%nPar,resHed%nFil

! Assumptions for old residual file
    IF (ios == 0) THEN
      IF (reshed%dsc%nDiff == 0) THEN
        reshed%dsc%iRecFmt = 1
        reshed%dsc%pgName  = 'unknown'
        reshed%dsc%nresta  = 0
        reshed%dsc%nresat  = 0
        reshed%dsc%nresep  = 0
        reshed%dsc%iElvaz  = 0
      ELSE IF (reshed%dsc%nDiff == 2) THEN
        reshed%dsc%iRecFmt = 1
        reshed%dsc%pgName  = 'unknown'
        reshed%dsc%nresta  = 1
        reshed%dsc%nresat  = 1
        reshed%dsc%nresep  = 0
        reshed%dsc%iElvaz  = 0
      ENDIF

! Read formatted header info
! --------------------------
    ELSE
      REWIND(LFN001)

      reshed%dsc%nDiff = 99

      READ (LFN001,'(A80,//////,3(37X,I8,/),44X,A)',iostat=ios)     &
          reshed%title,reshed%dsc%iTyp,reshed%dsc%iRecFmt,          &
          reshed%dsc%iElvAz,reshed%dsc%pgName

      READ (LFN001,'(37X,3(I8,5X),2(/,37X,I8),///)',iostat=ios)     &
          reshed%dsc%nResta,reshed%dsc%nResat,reshed%dsc%nResep,    &
          reshed%dsc%nPar,reshed%nFil
    ENDIF

    IF (ios /= 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/)')                      &
      ' *** PG FMTRES: Error reading header information.', &
                      'Formatted residual file: ' // TRIM(filnam(1,iFil))
      CALL exitrc(2)
    ENDIF

! Read the file information records
! ---------------------------------
    ALLOCATE(resHed%filHead(resHed%nFil),stat=irc)
    CALL alcerr(irc,'resHed%filHead',(/resHed%nFil/),pgName)

    DO jFil = 1,resHed%nFil
      CALL init_filhead(resHed%filHead(jFil))

      READ(lfn001,'(A)',iostat=ios) line
      IF (resHed%dsc%nDiff == 99) THEN
        resHed%filHead(jFil)%staNam(1) = line(6:21)
        resHed%filHead(jFil)%staNam(2) = ' '
        IF (resHed%dsc%nResta == 1) &
          resHed%filHead(jFil)%staNam(2) = line(24:39)

        CALL st2tim(1,1,line(42:60),resHed%filHead(jFil)%timref)

        resHed%filHead(jFil)%csess(1) = line(64:67)
        resHed%filHead(jFil)%csess(2) = line(69:72)

        READ(line(73:97),*,iostat=ios) &
            resHed%filHead(jFil)%nFrfil,resHed%filHead(jFil)%iCarr(:), &
            resHed%filHead(jFil)%meatyp,resHed%filHead(jFil)%ideltt,   &
            resHed%filHead(jFil)%nSatel

        IF (ios == 0) THEN
!!!!!!!!!! Problems with ifc81 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CMP_IFC8
           nrSat = resHed%filHead(jFil)%nSatel
           ALLOCATE(resHed%filHead(jFil)%numSat(nrSat), &
                stat=irc)
           CALL alcerr(irc,'resHed%filHead(jFil)%numSat', &
                (/nrSat/),pgName)
#else
          ALLOCATE(resHed%filHead(jFil)%numSat(resHed%filHead(jFil)%nSatel), &
                      stat=irc)
          CALL alcerr(irc,'resHed%filHead(jFil)%numSat', &
                      (/resHed%filHead(jFil)%nSatel/),pgName)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#endif

          READ(line(98:),*,iostat=ios) &
               resHed%filHead(jFil)%numsat(1:resHed%filHead(jFil)%nSatel)
        ENDIF

      ELSE
        READ (line,'(3X,2A18,19X,2(1X,A4),2X,4I2,I4,I5,2X,50I3)',iostat=ios)  &
            resHed%filHead(jFil)%staNam(1:2),resHed%filHead(jFil)%csess(1:2), &
            resHed%filHead(jFil)%nFrfil,resHed%filHead(jFil)%iCarr(:),        &
            resHed%filHead(jFil)%meatyp,resHed%filHead(jFil)%ideltt,          &
            resHed%filHead(jFil)%nSatel

        IF (ios == 0) THEN
!!!!!!!!!! Problems with ifc81 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CMP_IFC8
          nrSat = resHed%filHead(jFil)%nSatel
          ALLOCATE(resHed%filHead(jFil)%numSat(nrSat), &
                      stat=irc)
          CALL alcerr(irc,'resHed%filHead(jFil)%numSat', &
                      (/nrSat/),pgName)
#else
          ALLOCATE(resHed%filHead(jFil)%numSat(resHed%filHead(jFil)%nSatel), &
                      stat=irc)
          CALL alcerr(irc,'resHed%filHead(jFil)%numSat', &
                      (/resHed%filHead(jFil)%nSatel/),pgName)
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          READ (line,'(3X,2A18,19X,2(1X,A4),2X,4I2,I4,I5,2X,50I3)',iostat=ios) &
              resHed%filHead(jFil)%staNam(1:2),resHed%filHead(jFil)%csess(1:2),&
              resHed%filHead(jFil)%nFrfil,resHed%filHead(jFil)%iCarr(:),       &
              resHed%filHead(jFil)%meatyp,resHed%filHead(jFil)%ideltt,         &
              resHed%filHead(jFil)%nSatel,                                     &
              resHed%filHead(jFil)%numsat(1:resHed%filHead(jFil)%nSatel)
        ENDIF

        CALL strtim(1,line(40:56),(/resHed%filHead(jFil)%timref/))
      ENDIF

      IF (ios /= 0) THEN
        WRITE(lfnerr,'(/,A,2(/,16X,A),/)')                                   &
        ' *** PG FMTRES: Error reading observation files information.',      &
                        'Formatted residual file: ' // TRIM(filnam(1,iFil)), &
                        'Data line: ' // TRIM(line)
        CALL exitrc(2)
      ENDIF

    ENDDO

! Open binary file
! ----------------
    CALL opnfil(lfnres,filnam(2,iFil),'UNKNOWN','UNFORMATTED',' ',' ',ios)
    CALL opnerr(lfnerr,lfnres,ios,filnam(2,iFil),pgName)

! Write header of residual file
! -----------------------------
    CALL wtresh2(lfnres,resHed)

    DO jFil = 1,resHed%nFil
      DEALLOCATE(resHed%filhead(jFil)%numSat,stat=irc)
    ENDDO
    DEALLOCATE(resHed%filHead,stat=irc)

! Loop over all residual records
! ------------------------------
    READ (lfn001,'(///)')

    ios = 0
    DO WHILE (ios == 0)

! Read data record from formatted residual file
! ---------------------------------------------
      READ(lfn001,'(A)',iostat=ios) line
      IF (ios /= 0 .OR. LEN_TRIM(line) == 0) EXIT

      READ(line,'(I3,I6,I4,2X,2(I5),D30.18,2X,A1,3X,2F8.2)',iostat=ios) &
           resRec%iFile,resRec%iEpoch,resRec%iFreq,resRec%svnNum(1:2),  &
           resRec%value,resRec%resflg,ele,azi

      IF (ios /= 0) THEN
        WRITE(lfnerr,'(/,A,2(/,16X,A),/)')                                   &
        ' *** PG FMTRES: Error reading residual data records.',              &
                        'Formatted residual file: ' // TRIM(filnam(1,iFil)), &
                        'Data line: ' // TRIM(line)
        CALL exitrc(2)
      ENDIF

! Put elevation and azimut from 2nd svn -- if it is written
! ---------------------------------------------------------
      IF ((resHed%dsc%nDiff ==  0 .AND. resRec%svnNum(2)  ==  0) .OR. &
          (resHed%dsc%nDiff == 99 .AND. resHed%dsc%iElvAz == -1)) THEN
        iazi=IDNINT(azi*100D0)
        iele=IDNINT(ele*100D0)
        WRITE(eleazi,'(I5.5,I4.4)')iazi,iele
        READ(eleazi,'(I9.9)') resRec%svnNum(2)
      ENDIF

! Write observation into unformatted residual file
! ------------------------------------------------
      WRITE(lfnres,iostat=ios) resRec%iFile,resRec%iEpoch, &
           resRec%iFreq,resRec%svnNum(1:2),resRec%value,resRec%resflg

      IF (ios /= 0) THEN
        WRITE(lfnerr,'(/,A,/)') &
        ' *** PGM FMTRES: ERROR WRITING RESIDUAL FILE:' // TRIM(FILNAM(2,IFIL))
        CALL exitrc(2)
      ENDIF

    ENDDO ! Next record

! Close files
! -----------
    CLOSE(UNIT=lfn001)
    CLOSE(UNIT=lfnres)

! Next file
! ---------
  ENDDO

  CALL exitrc(0)

END PROGRAM fmtres
