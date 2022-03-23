MODULE s_CHINPT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE CHINPT(opt, crxfil, staCrx, nChvar, nInvar, nRevar,  &
              TITNEW, ChgChr, ChgInt, ChReal, nFil, filnam)

! -------------------------------------------------------------------------
! Purpose:    Reads input file for CHGHED
!
! Author:     R. Dach
!
! Created:    08-Dec-2000
! Last mod.:  30-Apr-2008
!
! Changes:    26-Jun-2001  RD: use alcerr for allocation
!             21-Dec-2001  HU: Use m_bern, other modules with ONLY
!             23-Apr-2003  CU: Nullify local pointers
!             08-Jul-2003  RD: Use SR GTSTAFLG for flags in staInfo file
!             09-Sep-2003  RD: Checkbox for station independent items
!             16-Sep-2003  RD: STACRUX->STAINFO
!             05-Dec-2007  LP: Process also SLR data; use SR OBSFILLST
!             30-Apr-2008  DT: Add keyWord list to OBSFILLST;
!                              dimension of fileNumber 1->5
!             30-Apr-2008  DT: Change order of files to PZ-CZ-PS-CS-RZ
!             05-Aug-2008  DT: Dimension of nFil must be 1
!
! SR used:    readkeys, exitrc, alcerr, gtfile, gtflna, readCrux, gtStaFlg
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_stacrx, ONLY: t_stacrux
  USE p_chghed, ONLY: t_chghed,t_chghed_chr,t_chghed_int,t_chghed_real

  USE s_alcerr
  USE s_gtstaflg
  USE s_readcrux
  USE s_readkeys
  USE s_exitrc
  USE s_gtflna
  USE s_ckoptc
  USE s_obsfillst
  IMPLICIT NONE
!
! Global Parameters
! -----------------
  TYPE(t_chghed), DIMENSION(:) :: opt       ! Input parameter
  CHARACTER(LEN=fileNameLength):: crxFil    ! Name of the staCrux file
  TYPE(t_staCrux)              :: staCrx    ! StaInfo records to be used
  INTEGER(i4b)                 :: nChvar    ! Number of chr-param to change
  INTEGER(i4b)                 :: nInvar    ! Number of int-param to change
  INTEGER(i4b)                 :: nRevar    ! Number of real-param to change
  CHARACTER(LEN=53)            :: TITNEW    ! new title
  TYPE(t_chghed_chr), DIMENSION(:), POINTER :: chgChr  ! Change of chr-var.
  TYPE(t_chghed_int), DIMENSION(:), POINTER :: chgInt  ! Change of int-var.
  TYPE(t_chghed_real),DIMENSION(:), POINTER :: chReal  ! Change of real-var.
  INTEGER(i4b)                              :: nFil    ! number of header files
  CHARACTER(LEN=fileNameLength), DIMENSION(:), POINTER :: filnam ! list of header file names

! Local Parameter
! ---------------
  CHARACTER(LEN=6), PARAMETER      :: srName = 'chinpt'

!
! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength) , DIMENSION(:)  , POINTER  :: keyValue
!
  CHARACTER(LEN=fileNameLength),   &
            DIMENSION(:,:), POINTER :: filhlp  ! temp. list of header files
  CHARACTER(LEN=fileNameLength),   &
            DIMENSION(:,:), POINTER :: dummy

!
  INTEGER(i4b)    :: iopt       ! Counter for options
  INTEGER(i4b)    :: nopt       ! Counter for options
  INTEGER(i4b)    :: ios        ! io status
  INTEGER(i4b)    :: irc        ! return code
  INTEGER(i4b)    :: iac        ! allocation status
  INTEGER(i4b)    :: irCode     ! error status
!!!  INTEGER(i4b)    :: obstype    ! 1: GNSS/2: Range obs
  INTEGER(i4b)    :: nflcol     ! columns of the obsfilelist
  INTEGER(i4b)    :: ii

  INTEGER(i4b),DIMENSION(5) :: filenumber

!
! Get the list of header files
! ----------------------------
  irCode=0
  NULLIFY(keyValue)

!
! a) get the number of files
! --------------------------
  nFil=0
  filenumber(:)=0
  nflcol=0
  NULLIFY(filhlp)
  NULLIFY(dummy)

!
! Get the list of header files
! ----------------------------
  CALL OBSFILLST('OBSTYPE',(/'GNSS ','Range'/),                  &
                 (/'PZHFIL  ','CZHFIL  ','BOTHZERO'/),           &
                 (/'PSHFIL  ','CSHFIL  ','BOTHSING'/),'RZHFIL',  &
                 0,1,0,nflcol,filenumber,filhlp,dummy)

  DO ii=1,5
    nFil = nFil + filenumber(ii)
  ENDDO

  ALLOCATE(filnam(nFil), stat=iac)
  CALL alcerr(iac, 'filnam', (/nFil/), srName)
  filnam=' '

  filnam(:)=filhlp(1,:)

  DEALLOCATE(filhlp, stat=iac)
  DEALLOCATE(dummy, stat=iac)

!
! Get the Number of variable types
! --------------------------------
  nopt=SIZE(opt)
  nChvar=0
  nInvar=0
  nRevar=0
  DO iopt=1,nopt
    IF (opt(iopt)%IdxChr > nChVar) nChvar = opt(iopt)%IdxChr
    IF (opt(iopt)%IdxInt > nInVar) nInvar = opt(iopt)%IdxInt
    IF (opt(iopt)%IdxRea > nReVar) nRevar = opt(iopt)%IdxRea
  ENDDO
!
  ALLOCATE(ChgChr(nChvar), stat=iac)
  CALL alcerr(iac, 'ChgChr', (/nChvar/),srName)
  ALLOCATE(ChgInt(nInvar), stat=iac)
  CALL alcerr(iac, 'ChgInt', (/nInvar/),srName)
  ALLOCATE(ChReal(nRevar), stat=iac)
  CALL alcerr(iac, 'ChReal', (/nRevar/),srName)
!
! Get the name of the staCrux file
! --------------------------------
  crxfil=' '
  CALL gtflna(0,'STAINFO', crxFil, irc)

  IF (irc /= 0) crxfil=' '
!
! Read station info file, adapt entries to the selected flags
! -----------------------------------------------------------
  IF (LEN_TRIM(crxFil) > 0) THEN

    CALL readCrux(crxfil,stacrx)

    CALL gtStaFlg('USEFLG',                                         &
                  (/'FLG001','FLG002','      ','      ','      '/), &
                  staCrx)

!
! no STACRUX file found, use information from input file
! ------------------------------------------------------
  ELSE
!
! Read all change options
! -----------------------
    nopt=1+nchvar+ninvar+nrevar
    DO iopt=1,nopt
      CALL readKeys(opt(iopt)%keyList(1), keyValue, irc)
      irCode=irCode+irc
      IF (irc == 0) THEN
        ! Station independet keywords:
        IF (opt(iopt)%iOptio == 1) THEN
          READ (keyValue(1), *, iostat=ios) opt(iopt)%ioptio
          irCode=irCode+ios
          IF (ios /= 0) THEN
            WRITE(lfnerr,'(A)')                               &
            ' ### SR CHINPT : WRONG ENTRY FOR KEYWORD "'//    &
              trim(opt(iopt)%keyList(1))//'" IN INPUT FILE'
          ENDIF
        ELSE IF (keyValue(1) == 'NONE') THEN
          opt(iopt)%ioptio = 0
        ELSE IF (keyValue(1) == '1ST') THEN
          opt(iopt)%ioptio = 1
        ELSE IF (keyValue(1) == '2ND') THEN
          opt(iopt)%ioptio = 2
        ELSE IF (keyValue(1) == 'BOTH') THEN
          opt(iopt)%ioptio = 3
        ELSE
          WRITE(lfnerr,'(A)')                               &
          ' ### SR CHINPT : WRONG ENTRY FOR KEYWORD "'//    &
            trim(opt(iopt)%keyList(1))//'" IN INPUT FILE: '//TRIM(keyValue(1))
        ENDIF
      ENDIF
    ENDDO

!
! Read the new title
! ------------------
    IF (opt(1)%ioptio > 0) THEN
      CALL readKeys(opt(1)%keyList(3), keyValue, irc)
      irCode=irCode+irc
      IF (irc == 0) THEN
        TITNEW=keyValue(1)
      ENDIF
    ENDIF
!
    nopt=1
!
! read old character variables
! ----------------------------
    DO iopt=1,nchvar
      IF (opt(iopt+nopt)%ioptio > 0) THEN
        CALL readKeys(opt(iopt+nopt)%keyList(2), keyValue, irc)
        irCode=irCode+irc
        IF (irc == 0 .AND. keyValue(1) /= 'ANY') THEN
          ChgChr(opt(iopt+nopt)%idxChr)%chrold=keyValue(1)
        ENDIF
        IF (keyValue(1) == 'ANY' .OR. LEN_TRIM(keyValue(1)) < 1) THEN
          opt(iopt+nopt)%OPTANY=1
        ENDIF
      ENDIF
    ENDDO

!
! read new character variables
! ----------------------------
    DO iopt=1,nchvar
      IF (opt(iopt+nopt)%ioptio > 0) THEN
        CALL readKeys(opt(iopt+nopt)%keyList(3), keyValue, irc)
        irCode=irCode+irc
        IF (irc == 0) THEN
          ChgChr(opt(iopt+nopt)%idxChr)%CHRNEW=keyValue(1)
        ENDIF
      ENDIF
    ENDDO
!
    nopt=nopt+nchvar
!
! read old integer variables
! --------------------------
    DO iopt=1,ninvar
      IF (opt(iopt+nopt)%ioptio > 0) THEN
        CALL readKeys(opt(iopt+nopt)%keyList(2), keyValue, irc)
        irCode=irCode+irc
        IF (irc == 0) THEN
          IF (keyValue(1) == 'ANY' .OR. LEN_TRIM(keyValue(1)) < 1) THEN
            opt(iopt+nopt)%OPTANY=1
          ELSE
            READ(keyValue(1),*,iostat=ios) ChgInt(opt(iopt+nopt)%idxInt)%INTOLD
            irCode=irCode+ios
            IF (ios /= 0) THEN
              WRITE(lfnerr,'(A)')                               &
              ' ### SR CHINPT : WRONG ENTRY FOR KEYWORD "'//    &
                trim(opt(iopt+nopt)%keyList(2))//'" IN INPUT FILE'
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDDO

!
! read new integer variables
! --------------------------
    DO iopt=1,ninvar
      IF (opt(iopt+nopt)%ioptio > 0) THEN
        CALL readKeys(opt(iopt+nopt)%keyList(3), keyValue, irc)
        irCode=irCode+irc
        IF (irc == 0) THEN
          READ(keyValue(1),*,iostat=ios) ChgInt(opt(iopt+nopt)%idxInt)%INTNEW
          irCode=irCode+ios
          IF (ios /= 0) THEN
            WRITE(lfnerr,'(A)')                               &
            ' ### SR CHINPT : WRONG ENTRY FOR KEYWORD "'//    &
              trim(opt(iopt+nopt)%keyList(3))//'" IN INPUT FILE'
          ENDIF
        ENDIF
      ENDIF
    ENDDO
!
    nopt=nopt+ninvar
!
! read old real variables
! --------------------------
    DO iopt=1,nrevar
      IF (opt(iopt+nopt)%ioptio > 0) THEN
        CALL readKeys(opt(iopt+nopt)%keyList(2), keyValue, irc)
        irCode=irCode+irc
        IF (irc == 0) THEN
          IF (keyValue(1) == 'ANY' .OR. LEN_TRIM(keyValue(1)) < 1) THEN
            opt(iopt+nopt)%OPTANY=1
          ELSE
            READ(keyValue(1),*,iostat=ios) ChReal(opt(iopt+nopt)%idxRea)%REAOLD
            irCode=irCode+ios
            IF (ios /= 0) THEN
              WRITE(lfnerr,'(A)')                               &
              ' ### SR CHINPT : WRONG ENTRY FOR KEYWORD "'//    &
                trim(opt(iopt+nopt)%keyList(2))//'" IN INPUT FILE'
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDDO

!
! read new real variables
! -----------------------
    DO iopt=1,nrevar
      IF (opt(iopt+nopt)%ioptio > 0) THEN
        CALL readKeys(opt(iopt+nopt)%keyList(3), keyValue, irc)
        irCode=irCode+irc
        IF (irc == 0) THEN
          READ(keyValue(1),*,iostat=ios) ChReal(opt(iopt+nopt)%idxRea)%REANEW
          irCode=irCode+ios
          IF (ios /= 0) THEN
            WRITE(lfnerr,'(A)')                               &
            ' ### SR CHINPT : WRONG ENTRY FOR KEYWORD "'//    &
              trim(opt(iopt+nopt)%keyList(3))//'" IN INPUT FILE'
          ENDIF
        ENDIF
      ENDIF
    ENDDO

    nopt=nopt+nrevar
!
  ENDIF
!
  DEALLOCATE(keyValue,stat=iac)
!
! An error occure during the reading
! ----------------------------------
  IF (irCode /= 0) CALL exitrc(2)

  RETURN
  END SUBROUTINE

END MODULE
