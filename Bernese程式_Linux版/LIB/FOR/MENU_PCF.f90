MODULE s_MENU_PCF
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_pcf(keyWord,menuauxinp,output)

! -------------------------------------------------------------------------
! Purpose:    Convert the PCF File (old format) into an editable (new)
!             format
!
! Author:     L. Mervart
!
! Created:    29-Aug-2001
! Last mod.:  16-Apr-2004
!
! Changes:    07-Dec-2001  RD: Redesigned to edit PCFiles in menu
!                              (some parts are necessary for the new BPE)
!             13-May-2002  RD: Use keywords from MENUAUX.INP
!             24-May-2002  MM: Replace MENU_EDIT by MENU_PCF
!             30-Jul-2002  HU: Use interface for alcerr
!             25-Sep-2002  HU: Remove i_astlib
!             13-Dec-2002  RD: New quotes-handling in SR writekey
!             23-Apr-2003  AJ: Nullify local pointers
!             16-May-2003  MM: Deallocate structure
!             18-May-2003  HU: Initialize structure
!             16-Apr-2004  HB: Initialize pcf%n_pid, %nVar, and %nTxt also
!                              for PCF_SPECIALS, _SAVE, and _PARAMS
!
! SR called:  gtflna, alcerr, inquire, tstKey, readKeys, writeKey,
!             rdpcf2, wtpcf2, menupcf1, menupcf2, menupcf3
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE p_menaux, ONLY: qt
  USE p_bpe,    ONLY: t_pcf

  USE s_alcerr
  USE s_menupcf1
  USE s_wtpcf2
  USE s_menupcf2
  USE s_menupcf3
  USE s_inquire
  USE f_tstkey
  USE s_writekey
  USE s_readkeys
  USE s_rdpcf2
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)               :: keyWord      ! what to do
  CHARACTER(LEN=*)               :: menuauxinp   ! MENUAUX.INP file name

! output:
  TYPE(t_key)                    :: output       ! value: Result to display

! Local Parameters
! ----------------
  CHARACTER(LEN= 8), PARAMETER   :: srname = 'menu_pcf'

! Local Variables
! ---------------
  TYPE(t_key),                    &
        DIMENSION(:), POINTER    :: keys
  TYPE(t_pcf)                    :: pcf

  CHARACTER(LEN=keyValueLength),     &
           DIMENSION(:), POINTER    :: keyValue
  CHARACTER(LEN=keyValueLength)  :: pcfFil  ! Plate file name
  CHARACTER(LEN=keyValueLength)  :: edtFil  ! EDIT PCF input panel

  INTEGER(i4b)                   :: ii
  INTEGER(i4b)                   :: irc
  INTEGER(i4b)                   :: irCode

  LOGICAL                        :: yes

! Nullify pointers
! ----------------
  NULLIFY(keys)
  NULLIFY(keyValue)
  NULLIFY(pcf%txt)

! Check the action
! ----------------
  IF (keyWord /= 'PCF_EDIT'     .AND. keyWord /= 'PCF_SAVE'    .AND. &
      keyWord /= 'PCF_SPECIALS' .AND. keyWord /= 'PCF_PARAMS') RETURN

! Get the file names
! ------------------
  IF (keyWord == 'PCF_EDIT') THEN

!    CALL gtflna(1,'FILE_EDIT',    pcfFil,irc)
    CALL gtflna(1,'FILE_PCF',     pcfFil,irc)
    CALL gtflna(1,'FILE_SKELETON',edtFil,irc)

  ENDIF

  IF (keyWord == 'PCF_SAVE') THEN

    CALL gtflna(1,'FILE_OUTPUT', pcfFil,irc)
    CALL gtflna(1,'FILE_INPUT',  edtFil,irc)

  ENDIF

! Have at least empty input lines
! -------------------------------
  IF (keyWord == 'PCF_PARAMS' .OR. keyWord == 'PCF_SPECIALS') THEN
    DEALLOCATE(output%value,stat=irc)

    ALLOCATE(output%value(1),stat=irc)
    CALL alcerr(irc,'output%value',(/1/),srName)
    output%value = ' '

    IF (keyWord == 'PCF_SPECIALS') THEN
      WRITE(output%value(1),'(14(A,1X))') (qt // qt, ii=1,14)
    ELSE IF (keyWord == 'PCF_PARAMS') THEN
      WRITE(output%value(1),'(12(A,1X))') (qt // qt, ii=1,12)
    ENDIF

    CALL writeKey(menuauxinp,(/output/),1,irc)

! Try to get the old values
! -------------------------
    irc = 1
    IF (keyWord == 'PCF_SPECIALS' .AND. tstKey('SPECIALS')) &
      CALL readKeys('SPECIALS',keyValue,irc)
    IF (keyWord == 'PCF_PARAMS' .AND. tstKey('PARAMETERS')) &
      CALL readKeys('PARAMETERS',keyValue,irc)

    IF (irc == 0) THEN
      DEALLOCATE(output%value,stat=irc)

      ALLOCATE(output%value(SIZE(keyValue)),stat=irc)
      CALL alcerr(irc,'output%value',(/SIZE(keyValue)/),srName)

      output%value = keyValue

      CALL writeKey(menuauxinp,(/output/),1,irc)
    ENDIF
  ENDIF

!
! PART 1: GET THE PCF INFORMATION
! =======
!
! Read from input file
! --------------------
  IF (keyWord == 'PCF_EDIT') THEN

    ! Does the PCF File Exist ?
    CALL INQUIRE(FILE=pcfFil , EXIST=yes)

    IF (yes) THEN

      ! Read the PCF file
      CALL rdpcf2(pcfFil, pcf, irCode)

    ! A new file will be created
    ELSE
      pcf%n_Pid = 0
      pcf%nVar  = 0
      pcf%nTxt  = 0
    ENDIF

! Use readKeys to read information from EDITPCF.INP
! -------------------------------------------------
  ELSE IF (keyWord == 'PCF_SAVE' .OR. &
!
! Get information from uniline structure
! --------------------------------------
           keyWord == 'PCF_SPECIALS' .OR. keyWord == 'PCF_PARAMS') THEN
    pcf%n_Pid = 0
    pcf%nVar  = 0
    pcf%nTxt  = 0

    CALL menupcf1(keyWord,edtFil,keys)

  ENDIF

!
! PART 2: Extract information from input lines
! ======
!
  IF (keyWord == 'PCF_SPECIALS' .OR. keyWord == 'PCF_PARAMS' .OR. &
      keyWord == 'PCF_SAVE'                                  ) THEN

    CALL menupcf2(keyWord,keys,pcf)

! Deallocate old keywords
! -----------------------
    DO ii = 1, SIZE(keys)
      DEALLOCATE(keys(ii)%value, STAT=irc)
    END DO
    DEALLOCATE(keys, STAT=irc)
  END IF

!
! PART 3: Put information into menu structure
! =======
!
  IF (keyWord == 'PCF_EDIT'     .OR. &
      keyWord == 'PCF_SPECIALS' .OR. keyWord == 'PCF_PARAMS') THEN

    CALL menupcf3(pcfFil,pcf,keys)

  ENDIF

!
! PART 4: Write the information into the output file
! ======
!
  IF (keyWord == 'PCF_EDIT') THEN
    CALL writeKey(edtFil, keys, 0, irc)

  ! Write the new PCFile
  ELSE IF (keyWord == 'PCF_SAVE') THEN
    CALL wtpcf2(pcfFil, pcf, irCode)

  ! Write the new "SPECIALS" uniline
  ELSE IF (keyWord == 'PCF_SPECIALS') THEN
    ALLOCATE(output%value(SIZE(keys(2)%value)),stat=irc)
    CALL alcerr(irc,'output%value',(/SIZE(keys(2)%value)/),srName)

    output%value = keys(2)%value

    DO ii = 1,SIZE(keys)
      DEALLOCATE(keys(ii)%value,stat=irc)
    ENDDO
    DEALLOCATE(keys,stat=irc)

  ! Write the new "PARAMETERS" uniline
  ELSE IF (keyWord == 'PCF_PARAMS') THEN
    ALLOCATE(output%value(SIZE(keys(3)%value)),stat=irc)
    CALL alcerr(irc,'output%value',(/SIZE(keys(3)%value)/),srName)

    output%value = keys(3)%value

    DO ii = 1,SIZE(keys)
      DEALLOCATE(keys(ii)%value,stat=irc)
    ENDDO
    DEALLOCATE(keys,stat=irc)

  ENDIF

  DEALLOCATE(keyValue,stat=irc)
  DO ii=1,pcf%nTxt
    DEALLOCATE(pcf%txt,stat=irc)
  END DO

END SUBROUTINE menu_pcf

END MODULE
