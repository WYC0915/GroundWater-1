MODULE s_RDPCF2
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE rdpcf2(pcffil,pcf,irCode)

! -------------------------------------------------------------------------
! Purpose:    Reads a PCF File
!
! Remark:     It is an extended version of the old rdpcf.f subroutine
!
! Old changes:  25-MAR-95 : MR: INITIALIZE "PARAMS"
!               30-APR-95 : MR: OPNFIL WITH READONLY
!               10-SEP-95 : LM: INCLUDE AFTER DECLARATIONS
!               30-SEP-95 : JJ: ADD VARIABLE DESCRIPTIONS
!               01-OCT-95 : JJ: ADD VARIABLE DEFAULT
!               11-OCT-95 : JJ: DON'T LET V_O, V_U, V_V, V_W, V_X
!                               OR V_Z HAVE A LENGTH GREATER THAN 8
!               17-AUG-99 : RD: DIMENSIONS FROM MAXBPE.inc
!               17-MAR-00 : RD: NEW KEYWORD "NEXTJOB"
!               10-JUN-02 : LM: INTERFACE FOR ALCERR ADDED
!               17-FEB-03 : LM: USE DIMENSIONS FROM P_BPE
!
! Author:     R. Dach
!
! Created:    07-Dec-2001
!
! Changes:    24-Feb-2003 RD: Source code cosmetics
!             18-May-2003 HU: Deallocate array
!             21-May-2003 RD: Make the deallocation safe
!             16-Sep-2003 RD: Close "lfnloc" before starting "exitrc(2)"
!             06-Feb-2004 RD: Remove the length of the PCF-variables
!             10-Aug-2004 RD: Singleton-Flag instead of Priority
!             28-Jun-2005 MM: Unused variables removed
!             13-Dec-2007 RD: Add new special action "CONT_ERR"
!             02-Nov-2009 DT: Remove conversion to uppercase
!                            (except for special commands)
!             14-Jun-2012 RD: Increase length of variable default
!             14-Jun-2012 RD: Use m_bern with only
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, lfnloc, lfnerr, lineLength
  USE p_bpe,    ONLY: t_pcf, maxpid, maxwat, maxdsc, PCF_header, special
  USE s_dimtst
  USE s_opnfil
  USE s_alcerr
  USE s_opnerr
  USE s_exitrc
  USE s_upperc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)                       :: pcffil    ! PCFile to read
! output:
  TYPE(t_pcf)                            :: pcf       ! PCF content
  INTEGER(i4b)                           :: irCode    ! return code

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN= 6), PARAMETER   :: srname = 'rdpcf2'

! Local Variables
! ---------------
  CHARACTER(LEN=linelength)      :: pcfpth
  CHARACTER(LEN=120)             :: line
  CHARACTER(LEN=80),DIMENSION(9) :: paramt
  CHARACTER(LEN=80)              :: paramo
  CHARACTER(LEN=8)               :: user2
  CHARACTER(LEN=8)               :: paswr2

  INTEGER(i4b)                   :: iSection
  INTEGER(i4b)                   :: lncnt
  INTEGER(i4b)                   :: iPid2
  INTEGER(i4b)                   :: ii, jj
  INTEGER(i4b)                   :: irc,ios

! Initialize parameters
! ---------------------
  irCode = 0

  pcf%n_Pid = 0
  pcf%nVar = 0
  pcf%nTxt = 0

! Open the file
! -------------
  pcfpth = trim(pcffil)

  CALL opnfil(lfnloc,pcfpth,'OLD','FORMATTED','READONLY',' ',irc)
  CALL opnerr(lfnerr,lfnloc,irc,pcfpth,srName)

! Count the comment lines, scripts, and variables
! -----------------------------------------------
  iSection = 0
  DO
    READ(lfnloc,'(A120)',iostat=ios) line
    IF (ios /= 0 .OR. LEN_TRIM(line) == 0) EXIT

    IF (line(1:1) == '#') THEN
      pcf%nTxt  = pcf%nTxt+1
    ELSE IF (line(1:40) == PCF_header(2)(1:40)) THEN
      iSection = iSection+1
    ELSE IF (line(1:40) == PCF_header(3)(1:40)) THEN
      iSection = iSection+1
    ELSE IF (line(1:40) == PCF_header(6)(1:40)) THEN
      iSection = iSection+1
    ELSE IF (iSection == 1) THEN
      pcf%n_Pid = pcf%n_Pid+1
    ELSE IF (iSection == 3) THEN
      pcf%nVar = pcf%nVar+1
    ENDIF
  ENDDO

! No PIDs found
! -------------
  IF (pcf%n_Pid == 0) THEN
    WRITE(lfnerr,'(/,A,/,16X,A,A,/)')                       &
         ' *** SR RDPCF2: No scripts found in the PCFile.', &
                         'File name:  ',TRIM(pcfpth)
    irCode = 1
  ENDIF

! Allocate memory
! ---------------
  CALL dimtst(1,2,2,srname,'maxpid','number of BPE scripts', &
             'It is defined in MAXBPE.inc',pcf%n_Pid,maxpid,irc)

  CALL dimtst(1,2,2,srname,'maxdsc','number of PCF variables', &
              'It is defined in MAXBPE.inc',pcf%nVar,maxdsc,irc)

  IF (ASSOCIATED(pcf%txt)) DEALLOCATE(pcf%txt,stat=irc)
  ALLOCATE(pcf%txt(pcf%nTxt),stat=irc)
  CALL alcerr(irc,'pcf%txt',(/pcf%nTxt/),srName)

! Init the entries of the arrays
! ------------------------------
  DO ii=1,maxpid
    pcf%job(ii)%iPid   = 0
    pcf%job(ii)%script = ' '
    pcf%job(ii)%option = ' '
    pcf%job(ii)%camp   = ' '
    pcf%job(ii)%cpu    = ' '
    pcf%job(ii)%user   = ' '
    pcf%job(ii)%paswrd = ' '
    pcf%job(ii)%n_wait = 0
    pcf%job(ii)%iwait  = 0
    pcf%job(ii)%ptype  = 0
    pcf%job(ii)%params = ' '
    pcf%job(ii)%jFlags = ' '
  ENDDO

  DO ii=1,maxdsc
    pcf%var(ii)%varnam = ' '
    pcf%var(ii)%vardsc = ' '
    pcf%var(ii)%vardef = ' '
  ENDDO

  DO ii=1,pcf%nTxt
    pcf%txt(ii)%line    = ' '
    pcf%txt(ii)%section = 0
    pcf%txt(ii)%pids    = 0
  ENDDO

! Start reading the file
! ----------------------
  REWIND(lfnloc)

  lncnt = 0
  iSection = 0

  pcf%n_Pid = 0
  pcf%nVar  = 0
  pcf%nTxt  = 0

  iPid2 = 0

! Loop the PCFile
! ---------------
  DO
    READ(lfnloc,'(A120)',iostat=ios) line

    IF (ios /= 0 .OR. LEN_TRIM(line) == 0) EXIT
    lncnt = lncnt+1

! Store comment lines
! -------------------
    IF (line(1:1) == '#') THEN
      pcf%nTxt = pcf%nTxt+1

      pcf%txt(pcf%nTxt)%line    = ' '
      IF (LEN_TRIM(line) > 1) &
        pcf%txt(pcf%nTxt)%line  = line(2:LEN_TRIM(line))
      pcf%txt(pcf%nTxt)%section = iSection
      IF (iSection == 1 .AND. pcf%n_Pid > 0) &
        pcf%txt(pcf%nTxt)%pids    = pcf%job(pcf%n_Pid)%iPid
      IF (iSection == 2 .AND. pcf%n_Pid > 0) &
        pcf%txt(pcf%nTxt)%pids    = iPid2

! Header lines of list of scripts
! -------------------------------
    ELSE IF (line(1:40) == PCF_header(1)(1:40) .OR. &
             line(1:40) == PCF_header(2)(1:40)) THEN
      iSection = 1

! Header lines for parameter section
! ----------------------------------
    ELSE IF (line(1:40) == PCF_header(3)(1:40) .OR. &
             line(1:40) == PCF_header(4)(1:40)) THEN
      iSection = 2

! Header lines for PCF variables
! ------------------------------
    ELSE IF (line(1:40) == PCF_header(5)(1:40) .OR. &
             line(1:40) == PCF_header(6)(1:40)) THEN
      iSection = 3

! Read the list of scripts
! ------------------------
    ELSE IF (iSection == 1) THEN
      pcf%n_Pid = pcf%n_Pid+1

      READ(line,'(I3,4(1X,A8),1X,A1,10(1X,I3))',iostat=ios)       &
           pcf%job(pcf%n_Pid)%iPid,   pcf%job(pcf%n_Pid)%script,  &
           pcf%job(pcf%n_Pid)%option, pcf%job(pcf%n_Pid)%camp,    &
           pcf%job(pcf%n_Pid)%cpu,    pcf%job(pcf%n_Pid)%jFlags,  &
           (pcf%job(pcf%n_Pid)%iWait(jj),jj=1,10)

      ! Error reading the line
      IF (ios /= 0) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,A,/,16X,A,I5,/,16X,A,A,/)') &
        ' *** SR RDPCF2: Error reading list of scripts',       &
                        'File name:  ',TRIM(pcfpth),           &
                        'Line number:',lncnt,                  &
                        line(1:45),'...'
        CLOSE(lfnloc)
        CALL exitrc(2)
      ENDIF

      ! Convert string in upper case
!!!      CALL upperc(pcf%job(pcf%n_Pid)%script)
!!!      CALL upperc(pcf%job(pcf%n_Pid)%option)
!!!      CALL upperc(pcf%job(pcf%n_Pid)%camp)
!!!      CALL upperc(pcf%job(pcf%n_Pid)%cpu)
!!!      CALL upperc(pcf%job(pcf%n_Pid)%jFlags)

      ! Count the wait PIDs
      pcf%job(pcf%n_Pid)%n_wait = 0
      DO ii=1,maxwat
        IF (pcf%job(pcf%n_Pid)%iWait(ii) /= 0) &
          pcf%job(pcf%n_Pid)%n_wait = ii
      ENDDO

      ! Put PID into comment structure
      IF (pcf%nTxt > 0) THEN
        IF( pcf%txt(pcf%nTxt)%pids(1) == pcf%txt(pcf%nTxt)%pids(2)) &
          pcf%txt(pcf%nTxt)%pids(2) = pcf%job(pcf%n_pid)%iPid
      ENDIF

! Read the script parameters
! --------------------------
    ELSE IF (iSection == 2) THEN

      READ(line,'(I3,1X,A12,1X,A8,9(1X,A8))',iostat=ios) &
           iPid2,user2,paswr2,(paramt(ii),ii=1,9)

      ! Error reading
      IF (ios /= 0) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,A,/,16X,A,I5,/,16X,A,A,/)') &
        ' *** SR RDPCF2: Error reading script parameters',     &
                        'File name:  ',TRIM(pcfpth),           &
                        'Line number:',lncnt,                  &
                        line(1:45),'...'
        CLOSE(lfnloc)
        CALL exitrc(2)
      ENDIF

      ! Find the correct PID in main list
      DO ii = 1,pcf%n_pid+1
        IF (ii > pcf%n_pid) EXIT
        IF (pcf%job(ii)%iPid == iPid2) EXIT
      ENDDO

      ! Put infos into PCF record
      IF (ii <= pcf%n_pid) THEN
        pcf%job(ii)%user   = user2
        pcf%job(ii)%paswrd = paswr2
        pcf%job(ii)%params = paramt

      ! PID not found in  main list
      ELSE
        WRITE(lfnerr,'(/,A,2(/,16X,A),A,/,16X,A,I5,/,16X,A,A,/)') &
        ' *** SR RDPCF2: Error reading script parameters',     &
                        'PID not found in the main list.',     &
                        'File name:  ',TRIM(pcfpth),           &
                        'Line number:',lncnt,                  &
                        line(1:45),'...'
        CLOSE(lfnloc)
        CALL exitrc(2)
      ENDIF

      ! Put PID into comment structure
      IF (pcf%nTxt > 0) THEN
        IF (pcf%txt(pcf%nTxt)%pids(1) == pcf%txt(pcf%nTxt)%pids(2)) &
          pcf%txt(pcf%nTxt)%pids(2) = iPid2
      ENDIF

! Read the PCF variables
! ----------------------
    ELSE IF (iSection == 3) THEN
      pcf%nVar = pcf%nVar+1

      READ(line,'(A8,1X,A40,1X,A32)',iostat=ios)            &
        pcf%var(pcf%nVar)%varnam, pcf%var(pcf%nVar)%vardsc, &
        pcf%var(pcf%nVar)%vardef

      ! Error reading
      IF (ios /= 0) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,A,/,16X,A,I5,/,16X,A,A,/)') &
        ' *** SR RDPCF2: Error reading PCF variables',         &
                        'File name:  ',TRIM(pcfpth),           &
                        'Line number:',lncnt,                  &
                        line(1:45),'...'
        CLOSE(lfnloc)
        CALL exitrc(2)
      ENDIF

      ! Convert content into upper case
!!!      CALL upperc(pcf%var(pcf%nVar)%varnam)
!!!      CALL upperc(pcf%var(pcf%nVar)%vardef)

      ! Check to make sure the line read is valid
      IF (pcf%var(pcf%nVar)%varnam(1:2) /= 'V_') THEN
        WRITE(lfnerr,'(/,A,2(/,16X,A,A),/,16X,A,I5,/,16X,A,A,/)')   &
        ' *** SR RDPCF2: Variable names have to start with V_',     &
                        'File name:     ',TRIM(pcfpth),             &
                        'Variable name: ',pcf%var(pcf%nVar)%varnam, &
                        'Line number:',lncnt,                       &
                        line(1:45),'...'
        CLOSE(lfnloc)
        CALL exitrc(2)
      ENDIF

      ! Check the length of the variable
      IF (LEN_TRIM(pcf%var(pcf%nVar)%vardef) > 32) THEN
        WRITE(lfnerr,'(/,A,2(/,16X,A,A),/,16X,A,I5,/,16X,A,A,/)')              &
        ' *** SR RDPCF2: Variable length cannot be longer than 32 characters', &
                        'File name:       ',TRIM(pcfpth),                      &
                        'Variable name:   ',pcf%var(pcf%nVar)%varnam,          &
                        'Line number:',lncnt,                                  &
                        line(1:45),'...'
        CLOSE(lfnloc)
        CALL exitrc(2)
      ENDIF

    ENDIF
  ENDDO ! Read and extrat one line of the PCFile

! Look for special commands
! -------------------------
  DO ii=1,pcf%n_pid

    paramo = pcf%job(ii)%params(1)
    CALL upperc(paramo)
    IF (paramo == special(1)) THEN
       pcf%job(ii)%pType = -1
    ELSE IF  (paramo == special(3)) THEN
       pcf%job(ii)%pType =  1
    ELSE IF  (paramo == special(4)) THEN
       pcf%job(ii)%pType =  2
    ELSE IF  (paramo == special(5)) THEN
       pcf%job(ii)%pType =  3
    ELSE
       pcf%job(ii)%pType =  0
    ENDIF

  ENDDO

  CLOSE(lfnloc)

  RETURN
END SUBROUTINE rdpcf2

END MODULE
