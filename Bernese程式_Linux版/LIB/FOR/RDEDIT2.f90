MODULE s_RDEDIT2
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE rdedit2(edt)

!-------------------------------------------------------------------------
! Purpose    :  Get all information from EDIT Information file (new
!               version of old SR RDEDIT(F77))
!
! Author     :  H.Bock
!
! Created    :  04-Oct-2001
! Last mod.  :  13-Jan-2009
!
! Changes    :  21-Dov-2001 RD: Name of the file becomes a parameter
!               29-Dec-2001 HU: Interface to alcerr added
!               25-Feb-2002 HB: USE d_edit and t_edit-structure
!               02-Pct-2002 RD: Separate RESMAX for each meatyp
!               17-may-2003 HU: Deallocate arrays
!               21-May-2003 RD: Make the deallocation safe
!               13-Jan-2009 RD: Use '' as EOF-identifier in NEXTLINE
!
! SR Used    :  gtflna, opnerr, opnfil, strtim, alcerr
!
! Copyright  :  Astronomical Institute
!               University of Bern
!               Switzerland
!-------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_global, ONLY: g_meaTyp
  USE d_edit,   ONLY: t_edit

  USE s_opnfil
  USE s_alcerr
  USE f_nextline
  USE s_opnerr
  USE s_strtim
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! IN/OUT:
  TYPE(t_edit) :: edt

! Local Variables
! ---------------
  CHARACTER(LEN=shortLineLength) :: line
  CHARACTER(LEN=timStrgLength)   :: tStrng
  CHARACTER(LEN=1)  :: typTxt

  INTEGER(i4b) :: ioStat
  INTEGER(i4b) :: iac
  INTEGER(i4b) :: iFil
  INTEGER(i4b) :: ii
  INTEGER(i4b) :: iEdt
  INTEGER(i4b) :: iFile

  REAL(r8b), DIMENSION(1) :: timEdt_hlp


! Get file name of editing info file
! ----------------------------------
  IF (LEN_TRIM(edt%filNam)==0) THEN
    edt%nEdFil=-1
  ELSE

! Open editing file
! -----------------
    CALL opnfil(lfnLoc,edt%filNam,'OLD','FORMATTED','READONLY',' ',ioStat)
    CALL opnerr(lfnErr,lfnLoc,ioStat,edt%filNam,'RDEDIT2')

! Read file information
! ---------------------
    edt%title = nextline(lfnloc,0)
    DO
      line = nextline(lfnLoc,0)
      IF (line(1:7) == 'FILE  S') THEN
        line = nextline(lfnLoc,0)
        EXIT
      ENDIF
      IF (line == '') GOTO 105
    ENDDO

! Count number of files in editing file
! -------------------------------------
    edt%nEdFil = 0
    FilCnt_Loop: DO
      line = nextline(lfnLoc,0)
      IF ( line(1:3) == '---') EXIT FilCnt_Loop
      IF (line == '') GOTO 105
      edt%nEdFil = edt%nEdFil + 1
    ENDDO FilCnt_Loop

! Read resrms options (if available)
! ----------------------------------
    DO
      line = nextline(lfnLoc,0)
      IF(line(1:7)==' RESRMS') THEN
        DO
          line = nextline(lfnLoc,1)
          IF (line(1:40) == 'MINIMUM NUMBER OF OBS. PER AMBIGUITY  :') &
            READ(line,'(40X,I9)') edt%minAmb

          IF (line(1:40) == 'SAMPLING RATE FOR COUNTING OBS.       :') THEN
            READ(line,'(40X,I9)') edt%iSampl
            EXIT
          ENDIF

          IF (line == '') GOTO 105
        ENDDO
      ENDIF
      IF(line(1:4)=='FILE') THEN
        line = nextLine(lfnLoc,0)
        EXIT
      ENDIF
      IF (line == '') GOTO 105
    ENDDO

! Count requests of editing file
! ------------------------------
    edt%nEdt = 0
    Count_Loop: DO
      line = nextline(lfnLoc,0)
      IF ( line(1:3) == '---') EXIT Count_Loop
      IF (line == '') GOTO 105
      edt%nEdt = edt%nEdt + 1
    ENDDO Count_Loop

    REWIND(lfnLoc)

! Allocate memory
! ---------------
    IF (ASSOCIATED(edt%head)) DEALLOCATE(edt%head,stat=iac)
    ALLOCATE(edt%head(edt%nEdFil),stat=iac)
    CALL alcerr(iac, 'edt%head', (/edt%nEdfil/), 'rdedit2')
    IF (ASSOCIATED(edt%rec)) DEALLOCATE(edt%rec,stat=iac)
    ALLOCATE(edt%rec(edt%nEdt),stat=iac)
    CALL alcerr(iac, 'edt%rec', (/edt%nEdt/), 'rdedit2')

    tStrng(18:19)='00'

! Read file list
! --------------
    DO ii=1,6
      line=nextline(lfnLoc,0)
    ENDDO
    File_Loop: DO iFil=1,edt%nEdfil
      edt%head(iFil)%cseEdt(2)=' '
      line=nextline(lfnLoc,0)
      READ(line,'(I3,3X,A4,2X,A1,4X,A17,I5,3X,A1,3X,A16,1X,A16)',END=105)&
           iFile,edt%head(iFil)%cseEdt(1),&
           edt%head(iFil)%cseEdt(2)(1:1),&
           tStrng(1:17),edt%head(iFil)%idtEdt,typTxt,&
           (edt%head(iFil)%staEdt(ii),ii=1,2)
      CALL STRTIM(1,tStrng,timEdt_hlp)
      edt%head(iFil)%timEdt = timEdt_hlp(1)

      edt%head(iFil)%meaEdt=0
      DO ii = 1,SIZE(g_meaTyp)
        IF (typTxt == g_meaTyp(ii)) THEN
          edt%head(iFil)%meaEdt=ii
          EXIT
        ENDIF
      ENDDO

      IF (edt%head(iFil)%meaEdt == 0) THEN
        WRITE(lfnErr,'(/,A,A,/,16X,A,A1,/,16X,A,I4,/,16X,A,A,/)')&
             ' *** SR RDEDIT2: ILLEGAL MEASUREMENT TYPE IN ',&
             'FILE LIST',&
             'MEASUREMENT TYPE: ',typTxt,&
             'FILE NUMBER     :',iFil,&
             'EDIT INFO FILE  : ',edt%filNam
        CALL exitrc(2)
      ENDIF
    ENDDO File_Loop

    DO
      line=nextLine(lfnLoc,0)
      IF (line(1:4) == 'FILE') THEN
        line=nextLine(lfnLoc,0)
        EXIT
      ENDIF
      IF (line == '') GOTO 105
    ENDDO

! Read editing requests
! ---------------------
    Edit_Loop: DO iEdt=1,edt%nEdt
      line=nextLine(lfnLoc,0)
      READ(line,'(I3,I6,2I5,I8,I6,F14.0,I5)',END=105) &
           edt%rec(iEdt)%lstEdt(6),edt%rec(iEdt)%lstEdt(1),&
           edt%rec(iEdt)%lstEdt(7),edt%rec(iEdt)%lstEdt(4),&
           edt%rec(iEdt)%lstEdt(2),edt%rec(iEdt)%lstEdt(3),&
           edt%rec(iEdt)%lstCyc,edt%rec(iEdt)%lstEdt(5)

! If no files given in the list, only editing requests concerning all
! files are allowed (all files: file no = 99)
      IF ((edt%nEdFil==0 .AND. edt%rec(iEdt)%lstEdt(6)/=99).OR.&
           (edt%rec(iEdt)%lstEdt(6)/=99 .AND.&
           edt%rec(iEdt)%lstEdt(6)>edt%nEdFil)) THEN
        WRITE(lfnErr,'(/,3(A,/,16X),2(A,I6,/,16X),A,A,/)')&
             ' *** SR RDEDIT2: ILLEGAL EDITING REQUEST.',&
             'IF NO FILES ARE SPECIFIED, ONLY REQUESTS',&
             'CONCERNING ALL FILES (FILE=99) ARE ALLOWED',&
             'REQUEST NUMBER   :', iEdt,&
             'FILE NUMBER      :',edt%rec(iEdt)%lstEdt(6),&
             'EDITING INFO FILE: ',edt%filNam
        CALL exitrc(2)
      ENDIF
    ENDDO Edit_Loop

! Close file
! ----------
    CLOSE(UNIT=lfnLoc)
  ENDIF

  GOTO 999

! End file detected
105 CONTINUE
  WRITE(lfnErr,'(/,A,A,/,16X,A,A,/)')&
       ' *** SR RDEDIT2: END OF FILE DETECTED WHILE READING',&
       'FILE INFORMATION',&
       'EDIT INFO FILE  : ',edt%filNam
  CALL exitrc(2)

999 CONTINUE
  RETURN
END SUBROUTINE rdedit2


END MODULE
