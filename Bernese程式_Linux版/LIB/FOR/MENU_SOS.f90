MODULE s_MENU_SOS
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_sos(keyWord, menuauxinp, output)

! -------------------------------------------------------------------------
! Purpose:    Is used for creating, editing, and storing of Bernese
!             station observation weighting file using the menu program
!             (called by the menu program via MENUAUX)
!
! Author:     R. Dach
!
! Created:    07-Jun-2002
!
! Changes:    23-Apr-2003 AJ: Nullify local pointers
!             16-May-2003 MM: Initialize and deallocate structure
!             18-Oct-2003 HU: Write date/time to title
!             06-Nov-2003 HB: Check if pointer (keys(ii)%value) is
!                             ASSOCIATED before DEALLOCATE
!             19-Nov-2003 RD: Reread INP-file using READINPF
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             20-Sep-2012 RD: Use M_BERN with ONLY
!             20-Sep-2012 RD: Correctly deallocate arrays
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, t_key, &
                      keyValueLength, staNameLength, lineLength
  USE m_global, ONLY: g_meaStr
  USE d_inpkey, ONLY: inpkey
  USE d_stawgt, ONLY: t_stawgt, init_stawgt
  USE p_menaux, ONLY: qt,menuCount
  USE s_ckoptr
  USE s_readstwg
  USE s_alcerr
  USE s_ckoptu
  USE s_ckoptz
  USE s_inquire
  USE s_readinpf
  USE s_dattim
  USE s_timst2
  USE s_writekey
  USE s_readkeys
  USE s_writstwg
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_gtflna
  USE s_ckoptl
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)                       :: keyWord     ! what to do
  CHARACTER(LEN=*)                       :: menuauxInp  ! INP file name

! output:
  TYPE(t_key)                            :: output   ! value: Result to display

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8),PARAMETER    :: srName = 'menu_sos'

! Local Variables
! ---------------
  TYPE(t_staWgt)                :: staWgt

  TYPE(t_key),                   &
      DIMENSION(:), ALLOCATABLE :: keys    ! Writing an EDIT panel

  CHARACTER(LEN=keyValueLength) :: sosFil  ! StaWgt file name
  CHARACTER(LEN=keyValueLength) :: edtFil  ! EDIT StaWgt input panel
  CHARACTER(LEN=keyValueLength) :: crdFil  ! Coodinate file name
  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:), POINTER     :: keyValue

  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:,:),ALLOCATABLE:: hlpStr
  CHARACTER(LEN=9)              :: date
  CHARACTER(LEN=5)              :: time

  INTEGER(i4b)                  :: numKeys ! Writing a STWGEDIT panel
  INTEGER(i4b)                  :: iWgt,nWgt
  INTEGER(i4b)                  :: ii
  INTEGER(i4b)                  :: i1, i2, i3
  INTEGER(i4b)                  :: irCode
  INTEGER(i4b)                  :: ios, irc, iac

  LOGICAL                       :: edit, yes

  NULLIFY(keyValue)
  CALL init_staWgt(staWgt)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'SOS_EDIT' .AND. keyWord /= 'SOS_SAVE'  .AND. &
      keyWord /= 'SOS_LIST') RETURN

! Editing/Create a StaWgt file
! ----------------------------
  IF (keyWord == 'SOS_EDIT') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_EDIT',    sosFil,irc)
    CALL gtflna(1,'FILE_SKELETON',edtFil,irc)
    CALL gtflna(0,'FILE_CRD',     crdFil,irc)


! Init the new key record
! -----------------------
    numKeys =  9

    ALLOCATE(keys(numKeys), STAT=iac)
    CALL alcerr(iac, 'keys', (/numKeys/), srName)

    DO ii = 1, numKeys
      ALLOCATE( keys(ii)%value(1), STAT=iac )
      CALL alcerr(iac, 'keys(ii)%value', (/1/), srName)
    END DO

    keys(1)%name = 'STAWGTRS '

    keys(2)%name = 'COORD'
    keys(3)%name = 'DATUM'
    keys(4)%name = 'STASEL'

    keys(5)%name = 'TITLE'

    keys(6)%name = 'NEWFILE'

    keys(7)%name = 'PHASE'
    keys(8)%name = 'CODE'
    keys(9)%name = 'RANGE'

! Init Entries for a station selection file panel
! -----------------------------------------------
    keys(1)%value(1) = sosFil

    keys(2)%value(1) = ''
    CALL gtflna(1,'DATUM',keys(3)%value(1),irc)
    keys(4)%value(1) = ''

    keys(5)%value(1) = ''

    keys(6)%value(1) = menuCount(1)

    keys(7)%value(1) = '0'
    keys(8)%value(1) = '0'
    keys(9)%value(1) = '0'

! Does the Station obs. weighting File Exist ?
! --------------------------------------------
    CALL INQUIRE(FILE=sosFil , EXIST=yes)

    IF (yes) THEN

      CALL readStwg(sosFil,staWgt)
      DEALLOCATE(staWgt%wgt,stat=iac)

      keys(5)%value(1) = staWgt%title(1:64)

      keys(6)%value(1) = 'last'

    ELSE

! Generate a new file from a coordinate file
! ------------------------------------------
      IF (LEN_TRIM(crdFil) > 0) &
        keys(2)%value(1) = crdFil

    ENDIF

! Write the input file
! --------------------
    CALL writeKey(edtFil, keys, 0, irc)

! Deallocate keys-record
! ----------------------
    DO ii = 1, SIZE(keys)
      IF ( ASSOCIATED(keys(ii)%value) ) THEN
        DEALLOCATE(keys(ii)%value, STAT=iac)
      ENDIF
    END DO
    DEALLOCATE(keys, STAT=iac)

! Generate the list of stations with defaul sigmas
! ------------------------------------------------
  ELSE IF (keyWord == 'SOS_LIST') THEN

! Generate at least an empty uniline
! ----------------------------------
    DEALLOCATE(output%value,stat=iac)

    ALLOCATE(output%value(1),stat=iac)
    CALL alcerr(iac,'output%value',(/1/),srName)

    output%value(1) = ' '
    WRITE(output%value(1),'(5(A,1X))') (qt // qt, ii=1,5)

    CALL writekey(menuauxinp,(/output/),1,irc)


! Get the name of the station weight file
! ---------------------------------------
    CALL gtflna(1,'STAWGTRS',sosFil,irc)
    IF (irc /= 0) CALL exitrc(2)

! Does the file allready exist?
! -----------------------------
    CALL INQUIRE(FILE=sosFil , EXIST=edit)

! Read an existing sta-wgt file
! -----------------------------
    IF (edit) THEN

      irCode = 0
      CALL readStwg(sosFil,staWgt)

! Create a new file
! -----------------
    ELSE

      irCode = 0

      CALL ckoptb(1,(/ 'PHASE' /), srName, &
                  'Default: Weights for phase obs.',irCode,result1=i1)

      CALL ckoptb(1,(/ 'CODE' /), srName,  &
                  'Default: Weights for code obs.', irCode,result1=i2)

      CALL ckoptb(1,(/ 'RANGE' /), srName, &
                  'Default: Weights for range obs.',irCode,result1=i3)

      ii = i1 + i2 + i3

      CALL readKeys('STASEL',keyValue,irc)

      staWgt%nWgt = SIZE(keyValue) * ii

      IF (SIZE(keyValue) == 1 .AND. LEN_TRIM(keyValue(1)) == 0) &
        staWgt%nWgt = 0

      IF (staWgt%nWgt > 0) THEN

        ALLOCATE(staWgt%wgt(staWgt%nWgt),stat=iac)
        CALL alcerr(iac,'staWgt%wgt',(/staWgt%nWgt/),srName)

        staWgt%wgt(:)%timWin%t(1) = 0d00
        staWgt%wgt(:)%timWin%t(2) = 1d20
        staWgt%wgt(:)%weight      = 1d0

        CALL ckoptl(1,'STASEL',keyValue,srName,                       &
                   'Sta-wgt file: default station names',irc,irCode,  &
                   maxVal=staWgt%nWgt,result2=staWgt%wgt(:)%staNam)

        nWgt = staWgt%nWgt
        DO iWgt = SIZE(keyValue),1,-1
          IF (i3 == 1) THEN
            IF (iWgt /= nWgt) staWgt%wgt(nWgt)%staNam = staWgt%wgt(iWgt)%staNam
            staWgt%wgt(nWgt)%meaTyp = 3
            nWgt = nWgt - 1
          ENDIF

          IF (i2 == 1) THEN
            IF (iWgt /= nWgt) staWgt%wgt(nWgt)%staNam = staWgt%wgt(iWgt)%staNam
            staWgt%wgt(nWgt)%meaTyp = 2
            nWgt = nWgt - 1
          ENDIF

          IF (i1 == 1) THEN
            IF (iWgt /= nWgt) staWgt%wgt(nWgt)%staNam = staWgt%wgt(iWgt)%staNam
            staWgt%wgt(nWgt)%meaTyp = 1
            nWgt = nWgt - 1
          ENDIF
        ENDDO
      ENDIF

    ENDIF ! edit an existing file or create a new one

! Generate the uniline for editing
! --------------------------------
    IF (staWgt%nWgt == 0 .OR. irCode /= 0) THEN

      DEALLOCATE(output%value,stat=iac)

      ALLOCATE(output%value(1),stat=iac)
      CALL alcerr(iac,'output%value',(/ 1 /),srName)

      output%value(1) = ' '
      WRITE(output%value(1),'(5(A,1X))') (qt//qt,ii=1,5)

    ELSE

      DEALLOCATE(output%value,stat=iac)

      ALLOCATE(output%value(staWgt%nWgt),stat=iac)
      CALL alcerr(iac,'output%value',(/ staWgt%nWgt /),srName)

      output%value = ' '

      ALLOCATE(hlpStr(5,1),stat=iac)
      CALL alcerr(iac,'hlpStr',(/5,1/),srName)

      DO iWgt = 1,staWgt%nWgt

        hlpStr = ' '

        hlpStr(1,1) = staWgt%wgt(iWgt)%staNam
        hlpStr(2,1) = g_meaStr(staWgt%wgt(iWgt)%meaTyp)

        IF (staWgt%wgt(iWgt)%weight /= 0d0) &
          WRITE(hlpStr(3,1),'(F8.3)') staWgt%wgt(iWgt)%weight

        CALL timst2(2,1,staWgt%wgt(iWgt)%timwin%t(1),hlpStr(4,1))
        CALL timst2(2,1,staWgt%wgt(iWgt)%timwin%t(2),hlpStr(5,1))

        WRITE(output%value(iWgt),'(5(A,1X))') &
              (qt // TRIM(hlpStr(ii,1)) // qt,  ii=1,5)
      ENDDO

      DEALLOCATE(staWgt%wgt,stat=iac)
      DEALLOCATE(hlpstr,stat=iac)

    ENDIF


! Store a station weight file
! ---------------------------
  ELSE IF (keyWord == 'SOS_SAVE') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_OUTPUT', sosFil,irc)
    CALL gtflna(1,'FILE_INPUT',  edtFil,irc)

! Reset the Name of the INP-File
! ------------------------------
    CALL readinpf(edtFil,inpKey)

! Which type has to be stored?
! ----------------------------
    irCode = 0

! Get the title line
! ------------------
    CALL readkeys('TITLE', keyValue, irc)

    CALL ckoptl(0,'TITLE', keyValue,srName, &
                'Sta-wgt file title line',irc,irCode, &
                empty=' ',maxVal=1,result1=staWgt%title)

! Get the list of entries
! -----------------------
    CALL readkeys('STAWGT', keyValue, irc)
    irCode = irCode + irc

    IF (irc == 0) THEN

! Allocate the corresponding memory
! ---------------------------------
      DEALLOCATE(staWgt%wgt,stat=iac)

      staWgt%nWgt = SIZE(keyValue)

      ALLOCATE(staWgt%wgt(staWgt%nWgt),stat=iac)
      CALL alcerr(iac,'staWgt%wgt',(/staWgt%nWgt/),srName)

      ALLOCATE(hlpStr(5,staWgt%nWgt),stat=ios)
      CALL alcerr(ios,'hlpStr',(/5,staWgt%nWgt/),srName)

! Extract the station weight records
! ----------------------------------
      CALL ckoptu(1,'STAWGT',keyValue,srName,          &
                  'Station weight file',irc,irCode,    &
                  numCol=5,maxVal=staWgt%nWgt,         &
                  result2=hlpStr)
    ENDIF

    IF (staWgt%nWgt == 1 .AND. LEN_TRIM(hlpStr(1,1)) == 0) staWgt%nWgt = 0

    IF (irCode == 0 .AND. staWgt%nWgt > 0) THEN
      CALL ckoptl(1,'STAWGT',hlpStr(1,:),srName,             &
                  'Station weight file',irc,irCode,          &
                  colTit='Station names',maxVal=staWgt%nWgt, &
                  empty=' ',result2=staWgt%wgt(:)%staNam)

      CALL ckoptc(1,'STAWGT',hlpStr(2,:),g_meaStr,              &
                  srName,'Station weight file',irc,irCode,      &
                  colTit='Measurement type',maxVal=staWgt%nWgt, &
                  result2=staWgt%wgt(:)%meaTyp)

      CALL ckoptr(1,'STAWGT',hlpStr(3,:),srName,              &
                  'Station weight file',irc,irCode,           &
                  colTit='Station weight',maxVal=staWgt%nWgt, &
                  ge=0d0,result2=staWgt%wgt(:)%weight)

      CALL ckoptz(1,'STAWGT',hlpStr(4,:),srName,              &
                  'Station weight file',irc,irCode,           &
                  colTit='Begin of validity',maxVal=staWgt%nWgt, &
                  empty=0d0,result2=staWgt%wgt(:)%timwin%t(1))

      CALL ckoptz(1,'STAWGT',hlpStr(5,:),srName,              &
                  'Station weight file',irc,irCode,           &
                  colTit='Snd of validity',maxVal=staWgt%nWgt, &
                  empty=1d20,result2=staWgt%wgt(:)%timwin%t(2))

    ENDIF

    IF (ALLOCATED(hlpstr)) DEALLOCATE(hlpStr,stat=ios)

    IF (irCode /= 0) CALL exitrc(2)

! Date and time in title line
! ---------------------------
    CALL dattim(date,time)
    staWgt%title(65:80) = ' '//date//' '//time

! Write the resulting file (or give an error)
! -------------------------------------------
    IF (LEN_TRIM(staWgt%wgt(1)%staNam) == 0) staWgt%nWgt = 0
    CALL writStwg(sosFil,staWgt)

  ENDIF ! which action

  DEALLOCATE(keyValue,stat=iac)
  DEALLOCATE(staWgt%wgt,stat=iac)

  RETURN
END SUBROUTINE menu_sos

END MODULE
