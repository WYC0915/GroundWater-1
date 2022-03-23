! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  PROGRAM RESFMT

! -------------------------------------------------------------------------
! Purpose:    Read unformatted header and unformatted residual from
!             files and write formatted residual files containing
!             header and observations
!
! Author:     R. Dach
!
! Created:    21-Oct-1999
!
! Changes:    17-Feb-2000 RD: Abuse satellite number for azi and ele (ts)
!             31-Aug-2000 RD: Set the station name array empty
!             23-Jan-2001 RD: Use the new menu system
!             27-Aug-2002 RD: Handle new formatted residual files
!             23-Apr-2003 HU: Nullify local pointers
!             16-May-2003 AJ: Initialize structure
!             25-Aug-2003 CU: Change format string
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             21-May-2005 HU: Declaration of 'line' from 255 to 500
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             27-Feb-2007 AG: Call DEFCON
!             19-Jul-2010 SL: tab characters removed
!             23-Sep-2010 RD: Enable CPU counter
!             01-Dec-2011 SL: new title string for pritit, m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, fileNameLength, lfnRes, lfnErr, lfn001
  USE m_cpu,    ONLY: cpu_start
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE d_resfil, ONLY: t_resHead,t_resRec,init_reshead
  USE s_opnfil
  USE s_pritit
  USE s_timst2
  USE s_opnsys
  USE s_defcon
  USE s_gtfile2
  USE s_rdresh2
  USE s_readinpf
  USE s_opnerr
  USE s_prfile
  USE s_exitrc
  IMPLICIT NONE

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER   :: pgName = 'RESFMT'

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

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)


! Nullify pointers
! ----------------
  CALL init_reshead(resHed)
  CALL init_inpkey(inpKey)
  NULLIFY(filnam)

! Get the name of the input file
! ------------------------------
  CALL readinpf(' ',inpKey)

! Define system files and copnstants
! ----------------------------------
  CALL opnsys
  CALL defcon(0)

! Print title section
! -------------------
  CALL pritit(pgName,'Convert residual files (binary to ASCII)')

! Get observation file names
! --------------------------
  CALL gtfile2('RESFILE',2,nFil,filnam)
  CALL prfile('RESFILE',' ',2)

! Loop over all files to be formatted
! -----------------------------------
  DO iFil=1,nFil

! Open binary residual file
! -------------------------
    CALL opnfil(lfnres,filnam(1,iFil),'OLD','UNFORMATTED','READONLY',' ',ios)
    CALL opnerr(lfnerr,lfnres,ios,filnam(1,iFil),pgName)

! Open formatted residual file
! ----------------------------
    CALL opnfil(lfn001,filnam(2,iFil),'UNKNOWN',' ',' ',' ',ios)
    CALL opnerr(lfnerr,lfn001,ios,filnam(2,iFil),pgName)

! Read residual header
! --------------------
    CALL rdresh2(lfnres,reshed)

! Write header into formatted file
! --------------------------------
    WRITE (LFN001,'(A,/,80("-"),//,A,/,20("-"),/)') &
          reshed%title,                           &
         'GENERAL INFORMATION:'
    WRITE (LFN001,'(3(A,I8,/),A,7X,A)')                               &
         '   Type of residual file:            ',reshed%dsc%iTyp,     &
         '   Format of residual records:       ',reshed%dsc%iRecFmt,  &
         '   Elevation/Azimuth flag:           ',reshed%dsc%iElvAz,   &
         '   Program created the file:         ',TRIM(reshed%dsc%pgName)
    WRITE (LFN001,'(A,3(I8,A),2(/,A,I8),//,A,/)')                           &
         '   Difference level of observations: ',reshed%dsc%nResta,' sta.', &
                                                 reshed%dsc%nResat,' sat.', &
                                                 reshed%dsc%nResep,' epo.', &
         '   Number of parameters:             ',reshed%dsc%nPar,           &
         '   Total number of residual files:   ',reshed%nFil,               &
         'Num  Station 1         Station 2         Reference epoch     ' // &
         '  Session     # Freq.  Type dt    # Satellite numbers'

    DO jFil=1,resHed%nFil
      line=' '
      WRITE (line,'(I3,A18,41X,2(1X,A4),2X,4I2,I4,I5,2X,100I4)')             &
        jFil,resHed%filHead(jFil)%stanam(1),resHed%filHead(jFil)%csess(1:2),&
        resHed%filHead(jFil)%nfrfil,resHed%filHead(jFil)%iCarr(:),          &
        resHed%filHead(jFil)%meatyp,resHed%filHead(jFil)%ideltt,            &
        resHed%filHead(jFil)%nSatel,                                        &
        resHed%filHead(jFil)%numsat(1:resHed%filHead(jFil)%nSatel)

      IF ((resHed%dsc%nDiff==99.AND.resHed%dsc%nResta==1) .OR. &
           resHed%dsc%nDiff==2) THEN
        WRITE(line(24:39),'(A)') resHed%filHead(jFil)%stanam(2)
      ENDIF

      CALL timst2(1,1,resHed%filHead(jFil)%timref,line(42:60))

      WRITE(LFN001,'(A)') TRIM(LINE)
    ENDDO

! Deallocate the header record
! ----------------------------
    DO jfil = 1,resHed%nFil
      DEALLOCATE(resHed%filHead(jFil)%numSat,stat=irc)
    ENDDO
    DEALLOCATE(resHed%filHead,stat=irc)

! Header line for data records
! ----------------------------
    WRITE (lfn001,'(//,A,/)')                      &
       'Num Epoch  Frq    Sat.         Value  ' // &
       '                  Flg      Elev    Azi'


! Loop over all residual records
! ------------------------------
    ios = 0
    DO WHILE (ios == 0)

! Read residuals from unformatted residual file
! ---------------------------------------------
      READ(lfnres,iostat=ios) resRec%iFile,resRec%iEpoch, &
           resRec%iFreq,resRec%svnNum(1:2),resRec%value,resRec%resflg

      IF (ios /= 0) CYCLE

! Get elevation and azimut from 2nd svn -- if it is written
! ---------------------------------------------------------
      azi = 0d0
      ele = 0d0
      IF ((resHed%dsc%nDiff ==  0 .AND. resRec%svnNum(2)  > 200) .OR. &
          (resHed%dsc%nDiff == 99 .AND. resHed%dsc%iElvAz == -1)) THEN
        WRITE(eleazi,'(I9.9)') resRec%svnNum(2)
        READ(eleazi,'(I5.5,I4.4)')iazi,iele
        azi=iazi/100D0
        ele=iele/100D0
        resRec%svnNum(2)=0
      ENDIF

! Write observation into formatted observation file
! -------------------------------------------------
      WRITE(LFN001,'(I3,I6,I4,2X,2(I5),D30.18,2X,A1,3X,2F8.2)') &
           resRec%iFile,resRec%iEpoch,resRec%iFreq,resRec%svnNum(1:2), &
           resRec%value,resRec%resflg,ele,azi

    ENDDO ! next epoch

! Close files
! -----------
    CLOSE(UNIT=lfn001)
    CLOSE(UNIT=lfnres)

! Next file
! ---------
  ENDDO

  CALL EXITRC(0)

END PROGRAM resfmt
