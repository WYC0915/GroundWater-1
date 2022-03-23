MODULE s_WTPCF2
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE wtpcf2(pcffil,pcf,irCode)

! -------------------------------------------------------------------------
! Purpose:    Writes a PCF File
!
! Author:     R. Dach
!
! Created:    10-Dec-2001
!
! Changes:    07-Feb-2002 RD: Do not write empty variable lines
!             06-Feb-2004 RD: Remove the length of the PCF-variables
!             10-Aug-2004 RD: Singleton-Flag instead of Priority
!             14-Jun-2012 RD: Increase length of variable default
!             14-Jun-2012 RD: Use m_bern with only
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, lfnloc, lfnerr, lineLength
  USE p_bpe,    ONLY: t_pcf, PCF_header
  USE s_opnfil
  USE s_opnerr
  USE f_lengt0
  USE f_lengt1
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)                       :: pcffil    ! PCFile to read
! output:
  TYPE(t_pcf)                            :: pcf       ! PCF content
  INTEGER(i4b)                           :: irCode    ! return code

! Local Parameters
! ----------------
  CHARACTER(LEN= 6), PARAMETER   :: srname = 'wtpcf2'

! Local Variables
! ---------------
  CHARACTER(LEN=linelength)      :: pcfpth
  CHARACTER(LEN=120)             :: line

  INTEGER(i4b)                   :: iSection
  INTEGER(i4b)                   :: iTxt
  INTEGER(i4b)                   :: ii,jj
  INTEGER(i4b)                   :: i1,i2
  INTEGER(i4b)                   :: irc


! Initialize parameters
! ---------------------
  irCode = 0

! Open the file
! -------------
  pcfpth = trim(pcffil)

  CALL opnfil(lfnloc,pcfpth,'UNKNOWN','FORMATTED',' ',' ',irc)
  CALL opnerr(lfnerr,lfnloc,irc,pcfpth,srName)

! Init the entries of the arrays
! ------------------------------
  pcf%job(:)%user   = ' '
  pcf%job(:)%paswrd = ' '

! Start writing the file
! ----------------------
  iSection = 0
  iTxt = 1
  IF (iTxt <= pcf%nTxt) THEN
    DO WHILE(iSection == pcf%txt(iTxt)%section)
      WRITE(lfnloc,'(A)') '#' // TRIM(pcf%txt(iTxt)%line)
      iTxt = iTxt+1
      IF (iTxt > pcf%nTxt) EXIT
    ENDDO
  ENDIF

! Write the list of scripts
! -------------------------
  iSection = 1
  WRITE(lfnloc,'(A,/,A)') (TRIM(PCF_header(ii)),ii=1,2)
  DO ii = 1,pcf%n_Pid

    ! Add comment lines
    IF (iTxt <= pcf%nTxt) THEN
      DO WHILE(iSection == pcf%txt(iTxt)%section .AND. &
               pcf%job(ii)%iPid > pcf%txt(iTxt)%pids(1))
        WRITE(lfnloc,'(A)') '#' // TRIM(pcf%txt(iTxt)%line)
        iTxt = iTxt+1
        IF (iTxt > pcf%nTxt) EXIT
      ENDDO
    ENDIF

    line = ' '
    WRITE(line,'(I3.3,4(1X,A8),1X,A1)')            &
          pcf%job(ii)%iPid,   pcf%job(ii)%script,  &
          pcf%job(ii)%option, pcf%job(ii)%camp,    &
          pcf%job(ii)%cpu,    pcf%job(ii)%jFlags
    DO jj = 1,pcf%job(ii)%n_Wait
      IF (pcf%job(ii)%iWait(jj) == 0) CYCLE
      i1 = 39 + jj*4
      i2 = i1 + 2
      WRITE(line(i1:i2),'(I3.3)') pcf%job(ii)%iWait(jj)
    ENDDO
    WRITE(lfnloc,'(A)') TRIM(line)
  ENDDO

! Add comment lines behind the section
! ------------------------------------
  IF (iTxt <= pcf%nTxt) THEN
    DO WHILE(iSection == pcf%txt(iTxt)%section)
      WRITE(lfnloc,'(A)') '#' // TRIM(pcf%txt(iTxt)%line)
      iTxt = iTxt+1
      IF (iTxt > pcf%nTxt) EXIT
    ENDDO
  ENDIF

! Write the parameter list
! ------------------------
  iSection = 2
  WRITE(lfnloc,'(A,/,A)') (TRIM(PCF_header(ii)),ii=3,4)
  DO ii = 1,pcf%n_Pid

    ! Add comment lines
    IF (iTxt <= pcf%nTxt) THEN
      DO WHILE(iSection == pcf%txt(iTxt)%section .AND. &
               pcf%job(ii)%iPid > pcf%txt(iTxt)%pids(1))
        WRITE(lfnloc,'(A)') '#' // TRIM(pcf%txt(iTxt)%line)
        iTxt = iTxt+1
        IF (iTxt > pcf%nTxt) EXIT
      ENDDO
    ENDIF

    line = ' '
    WRITE(line,'(I3.3,1X,A12,1X,A8)')         &
       pcf%job(ii)%iPid,   pcf%job(ii)%user,  &
       pcf%job(ii)%paswrd

    DO jj = 1,SIZE(pcf%job(ii)%params)
      IF (LEN_TRIM(pcf%job(ii)%params(jj)) > 0) THEN
        i1 = 27 + (jj-1)*9
        i2 = i1 + 7
        WRITE(line(i1:i2),'(A8)') pcf%job(ii)%params(jj)(1:8)
      ENDIF
    ENDDO

    IF (LENGT0(line) > 3) WRITE(lfnloc,'(A)') line(1:lengt1(line))

  ENDDO

! Add comment lines behind the section
! ------------------------------------
  IF (iTxt <= pcf%nTxt) THEN
    DO WHILE (iSection == pcf%txt(iTxt)%section)
      WRITE(lfnloc,'(A)') '#' // TRIM(pcf%txt(iTxt)%line)
      iTxt = iTxt+1
      IF (iTxt > pcf%nTxt) EXIT
    ENDDO
  ENDIF

! Write the PCF variables
! -----------------------
  iSection = 3
  WRITE(lfnloc,'(A,/,A)') (TRIM(PCF_header(ii)),ii=5,6)
  DO ii = 1,pcf%nVar
    IF (LEN_TRIM(pcf%var(ii)%varnam) == 0) CYCLE
    WRITE(line,'(A8,1X,A40,1X,A32)')          &
      pcf%var(ii)%varnam, pcf%var(ii)%vardsc, &
      pcf%var(ii)%vardef
    WRITE(lfnloc,'(A)') TRIM(line)
  ENDDO

! Add comment lines behind the section
! ------------------------------------
  DO WHILE(iTxt <= pcf%nTxt)
    IF (iSection /= pcf%txt(iTxt)%section) EXIT
    WRITE(lfnloc,'(A)') '#' // TRIM(pcf%txt(iTxt)%line)
    iTxt = iTxt+1
  ENDDO

! Close the outout file
! ---------------------
  CLOSE(lfnloc)

  RETURN
END SUBROUTINE wtpcf2

END MODULE
