MODULE s_MENUPCF1
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menupcf1(keyWord,edtFil,keys)

! -------------------------------------------------------------------------
! Purpose:    Gets information to put them into the "keys" structure
!
! Author:     R. Dach
!
! Created:    07-Dec-2001
! Last mod.:  19-Nov-2003
!
! Changes:    13-May-2002 RD: Use keyWords from MENUAUX.INP
!             23-Apr-2003 AJ: Nullify local pointers
!             19-Nov-2003 RD: Reread INP-file using READINPF
!
! SR called:  readKeys, alcerr, ckoptn, readinpf
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_inpkey, ONLY: inpkey
  USE p_bpe,    ONLY: kWords, kDescr

  USE s_alcerr
  USE s_readinpf
  USE s_readkeys
  USE s_ckoptn
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)                       :: keyWord    ! what to do
  CHARACTER(LEN=*)                       :: edtFil     ! EDIT PCF input panel

! output:
  TYPE(t_key),      DIMENSION(:),POINTER :: keys       ! keys structure

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER :: srName = 'menupcf1'

! Local Variables
! ---------------

  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER    :: keyValue

  INTEGER(i4b)                                            :: numKey
  INTEGER(i4b)                                            :: ii
  INTEGER(i4b)                                            :: irc, irCode

  NULLIFY(keyValue)

! Check the action
! ----------------
  IF (keyWord /= 'PCF_SAVE'     .AND. &
      keyWord /= 'PCF_SPECIALS' .AND. keyWord /= 'PCF_PARAMS') RETURN


! Reset input file for store
! --------------------------
  IF (keyWord == 'PCF_SAVE') &
    CALL readinpf(edtFil,inpKey)
    CALL readkeys('', keyValue, irc)

! Allocate the key-structure
! --------------------------
  numKey = 0
  IF (keyWord == 'PCF_SAVE') THEN
    numKey = 5
  ELSE IF (keyWord == 'PCF_SPECIALS') THEN
    numKey = 2
  ELSE IF (keyWord == 'PCF_PARAMS') THEN
    numKey = 3
  ENDIF

  irCode = 0

  ALLOCATE(keys(numKey),stat=irc)
  CALL alcerr(irc,'keys',(/numKey/),srName)

! Read list of scripts
! --------------------
  IF (keyWord == 'PCF_SAVE'     .OR. &
      keyWord == 'PCF_SPECIALS' .OR. keyWord == 'PCF_PARAMS') THEN

    CALL readKeys(kWords(1),keyValue,irc)

    ALLOCATE(keys(1)%value(SIZE(keyValue)))
    CALL alcerr(irc,'keys(1)%value',(/SIZE(keyValue)/),srName)

    keys(1)%name  = kWords(1)
    keys(1)%value = keyValue

  ENDIF

! Read special actions
! --------------------
  IF (keyWord == 'PCF_SAVE'     .OR. &
      keyWord == 'PCF_SPECIALS' .OR. keyWord == 'PCF_PARAMS') THEN

    CALL readKeys(kWords(2),keyValue,irc)

    IF (keyWord == 'PCF_SAVE') THEN

      CALL ckoptn(0,kWords(2),keyValue,ii,srName,kDescr(2),irCode,SIZE(keys(1)%value))
      CALL ckoptn(0,kWords(2),keys(1)%value,ii,srName,kDescr(2),irCode,SIZE(keyValue))

    ENDIF

    ALLOCATE(keys(2)%value(SIZE(keyValue)))
    CALL alcerr(irc,'keys(2)%value',(/SIZE(keyValue)/),srName)

    keys(2)%name  = kWords(2)
    keys(2)%value = keyValue

  ENDIF


! Read other parameters
! ---------------------
  IF (keyWord == 'PCF_SAVE'     .OR. keyWord == 'PCF_PARAMS') THEN

    CALL readKeys(kWords(3),keyValue,irc)

    IF (keyWord == 'PCF_SAVE') THEN

      CALL ckoptn(0,kWords(3),keyValue,ii,srName,kDescr(3),irCode,SIZE(keys(1)%value))
      CALL ckoptn(0,kWords(3),keys(1)%value,ii,srName,kDescr(3),irCode,SIZE(keyValue))

    ENDIF

    ALLOCATE(keys(3)%value(SIZE(keyValue)))
    CALL alcerr(irc,'keys(3)%value',(/SIZE(keyValue)/),srName)

    keys(3)%name  = kWords(3)
    keys(3)%value = keyValue

  ENDIF

! Read PCF variables
! ------------------
  IF (keyWord == 'PCF_SAVE') THEN

    CALL readKeys(kWords(4),keyValue,irc)

    ALLOCATE(keys(4)%value(SIZE(keyValue)))
    CALL alcerr(irc,'keys(4)%value',(/SIZE(keyValue)/),srName)

    keys(4)%name  = kWords(4)
    keys(4)%value = keyValue

! Read the comment text
! ---------------------
    CALL readKeys('ADDITIONAL_INFO',keyValue,irc)

    ALLOCATE(keys(5)%value(SIZE(keyValue)))
    CALL alcerr(irc,'keys(5)%value',(/SIZE(keyValue)/),srName)

    keys(5)%name  = 'ADDITIONAL_INFO'
    keys(5)%value = keyValue

  ENDIF

  DEALLOCATE(keyValue,stat=irc)

RETURN
END SUBROUTINE menupcf1

END MODULE
