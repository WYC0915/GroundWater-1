MODULE s_MENU_ECC
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_ecc(keyWord, menuauxinp, output)

! -------------------------------------------------------------------------
! Purpose:    Is used for creating, editing, and storing of Bernese
!             station eccentricity files using the menu program
!             (called by the menu program via MENUAUX)
!
! Author:     R. Dach
!
! Created:    07-May-2002
!
! Changes:    25-Sep-2002 HU: Remove i_astlib
!             13-Dec-2002 RD: New quotes-handling in SR writekey
!             08-Jan-2003 RD: Adapt to the actual MENUAUX calling
!             19-Feb-2003 RD: Stop with error (new logic in menu)
!             23-Apr-2003 AJ: Nullify local pointers
!             16-May-2003 MM: Initialize structure
!             18-Oct-2003 HU: Write date/time to title
!             06-Nov-2003 HB: Check if pointer (keys(ii)%value) is
!                             ASSOCIATED before DEALLOCATE
!             19-Nov-2003 RD: Reread INP-file using READINPF
!             17-Feb-2011 RD: Remove MAXSTA-COMMON (unused)
!             20-Sep-2012 RD: Use M_BERN with ONLY
!             20-Sep-2012 RD: Correctly deallocate arrays
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnloc, lfnerr, t_key, &
                      keyValueLength, staNameLength, lineLength, shortLineLength
  USE d_inpkey, ONLY: inpkey
  USE d_eccent, ONLY: eccTyp,t_eccent, init_eccent
  USE p_menaux, ONLY: qt,menuCount
  USE s_ckoptr
  USE s_alcerr
  USE s_opnfil
  USE s_ckoptu
  USE s_inquire
  USE s_writecc
  USE s_opnerr
  USE s_readinpf
  USE s_dattim
  USE s_writekey
  USE s_readkeys
  USE s_getdat
  USE s_exitrc
  USE s_ckoptc
  USE s_gtflna
  USE s_ckopti
  USE s_ckoptl
  USE s_readecc
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
  CHARACTER(LEN=8),PARAMETER    :: srName = 'menu_ecc'

! Local Variables
! ---------------
  TYPE(t_eccent)                :: eccent

  TYPE(t_key),                   &
      DIMENSION(:), ALLOCATABLE :: keys    ! Writing an EDIT panel

  CHARACTER(LEN=keyValueLength) :: eccFil  ! Sigma file name
  CHARACTER(LEN=keyValueLength) :: edtFil  ! EDIT ABB input panel
  CHARACTER(LEN=keyValueLength) :: crdFil  ! Coodinate file name
  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:), POINTER     :: keyValue

  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:,:),ALLOCATABLE:: hlpStr
  CHARACTER(LEN=9)              :: date
  CHARACTER(LEN=5)              :: time

  INTEGER(i4b)                  :: numKeys ! Writing a ECCEDIT panel
  INTEGER(i4b)                  :: iEcc
  INTEGER(i4b)                  :: ii
  INTEGER(i4b)                  :: irCode
  INTEGER(i4b)                  :: ios, irc, iac

  LOGICAL                       :: edit, yes

! Init pointers and variables
! ---------------------------
  NULLIFY(keyValue)
  CALL init_eccent(eccent)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'ECC_EDIT' .AND. keyWord /= 'ECC_SAVE'  .AND. &
      keyWord /= 'ECC_LIST') RETURN

! Editing/Create a ECC file
! -------------------------
  IF (keyWord == 'ECC_EDIT') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_EDIT',    eccFil,irc)
    CALL gtflna(1,'FILE_SKELETON',edtFil,irc)
    CALL gtflna(0,'FILE_CRD' ,crdFil,irc)


! Does the Station Sigma File Exist ?
! -----------------------------------
    CALL INQUIRE(FILE=eccFil , EXIST=yes)

    IF (yes) THEN
      numKeys = 10
    ELSE
      numKeys = 11
    ENDIF

! Init the new key record
! -----------------------
    ALLOCATE(keys(numKeys), STAT=iac)
    CALL alcerr(iac, 'keys', (/numKeys/), srName)

    DO ii = 1, numKeys
      ALLOCATE( keys(ii)%value(1), STAT=iac )
      CALL alcerr(iac, 'keys(ii)%value', (/1/), srName)
    END DO

    keys(1)%name = 'ECCENRS '
    keys(2)%name = 'FILNAM1 '
    keys(3)%name = 'FILNAM2 '

    keys(4)%name = 'COORD'
    keys(5)%name = 'DATUM'

    keys(6)%name = 'NEWFILE'

    keys(7)%name = 'TITLE1'
    keys(8)%name = 'DATUM1_STRG'

    keys(9)%name = 'STASEL'

! Init Entries for a station selection file panel
! -----------------------------------------------
    keys(1)%value(1) = eccFil
    keys(2)%value(1) = eccFil
    keys(3)%value(1) = eccFil

    keys(4)%value(1) = ''
    CALL gtflna(1,'DATUM',keys(5)%value(1),irc)

    keys(6)%value(1) = menuCount(1)

    keys(7)%value(1) = ''
    keys(8)%value(1) = 'ITRF??'

    keys(9)%value(1) = ''

! Does the Station Eccentricity File Exist ?
! ------------------------------------------
    CALL INQUIRE(FILE=eccFil , EXIST=yes)

    IF (yes) THEN

      keys(6)%value(1) = 'last'

      CALL readEcc(eccFil,eccent)
      DEALLOCATE(eccent%ecc,stat=iac)

      keys(7)%value(1) = eccent%title(1:64)
      keys(8)%value(1) = eccent%datum%name

      IF (eccent%eccTyp == 1) THEN
        keys(7)%name = 'TITLE1'
        keys(8)%name = 'DATUM1_STRG'

        keys(10)%name = 'ECCTYP'
        keys(10)%value(1) = 'LOCAL'
      ELSE
        keys(7)%name = 'TITLE2'
        keys(8)%name = 'DATUM2_STRG'

        keys(10)%name = 'ECCTYP'
        keys(10)%value(1) = 'GEOCENTRIC'
      ENDIF

    ELSE

      keys(10)%name = 'TITLE2'
      keys(11)%name = 'DATUM2_STRG'

      keys(10)%value(1) = ''
      keys(11)%value(1) = 'ITRF??'

! Generate a new file from a coordinate file
! ------------------------------------------
      IF (LEN_TRIM(crdFil) > 0) THEN
        keys(4)%value(1) = crdFil

        CALL opnfil(lfnloc,crdFil,'OLD','FORMATTED','READONLY',' ',ios)
        CALL opnerr(lfnerr,lfnloc,ios,crdFil,srName)

        READ(lfnloc,'(//,22X,A16)') keys(8)%value(1)  ! datum string

        CLOSE(lfnloc)

        keys(11)%value(1) = keys(8)%value(1)
      ENDIF

    ENDIF

! Write the input file
! --------------------
    CALL writeKey(edtFil, keys, 0, irc)

! Generate the list of stations with defaul sigmas
! ------------------------------------------------
  ELSE IF (keyWord == 'ECC_LIST') THEN

! Generate at least an empty uniline
! ----------------------------------
    DEALLOCATE(output%value,stat=iac)

    ALLOCATE(output%value(1),stat=iac)
    CALL alcerr(iac,'output%value',(/1/),srName)

    output%value(1) = ' '
    WRITE(output%value(1),'(6(A,1X))') (qt // qt, ii=1,6)

    CALL writekey(menuauxinp,(/output/),1,irc)


! Get the name of the eccentricity file
! -------------------------------------
    CALL gtflna(1,'ECCENRS',eccFil,irc)
    IF (irc /= 0) RETURN

! Does the file allready exist?
! -----------------------------
    CALL INQUIRE(FILE=eccFil , EXIST=edit)

! Read an existing eccent. file
! -----------------------------
    IF (edit) THEN

      irCode = 0
      CALL readecc(eccFil,eccent)

! Create a new eccent. file
! -------------------------
    ELSE
      irCode = 0
      CALL readKeys('STASEL',keyValue,irc)

      eccent%nEcc = SIZE(keyValue)

      IF (eccent%nEcc == 1 .AND. LEN_TRIM(keyValue(1)) == 0) &
        eccent%nEcc = 0

      IF (eccent%nEcc > 0) THEN

        ALLOCATE(eccent%ecc(eccent%nEcc),stat=iac)
        CALL alcerr(iac,'eccent%ecc',(/ eccent%nEcc /),srName)

        CALL ckoptl(1,'STASEL',keyValue,srName,                      &
                   'Eccent.file: default station names',irc,irCode,  &
                   maxVal=eccent%nEcc,result2=eccent%ecc(:)%staNam)

        eccent%ecc(:)%staNum = 0
        eccent%ecc(:)%cenNam = eccent%ecc(:)%staNam
        DO ii = 1,3
          eccent%ecc(:)%xEccen(ii) = 0d0
        ENDDO
      ENDIF

    ENDIF ! edit an existing eccent. file or create a new one

! Generate the uniline for editing
! --------------------------------
    IF (eccent%nEcc == 0 .OR. irCode /= 0) THEN

      DEALLOCATE(output%value,stat=iac)

      ALLOCATE(output%value(1),stat=iac)
      CALL alcerr(iac,'output%value',(/ 1 /),srName)

      WRITE(output%value(1),'(6(A,1X))') (qt//qt,ii=1,6)

    ELSE

      DEALLOCATE(output%value,stat=iac)

      ALLOCATE(output%value(eccent%nEcc),stat=iac)
      CALL alcerr(iac,'output%value',(/ eccent%nEcc /),srName)

      output%value = ' '

      ALLOCATE(hlpStr(6,1),stat=iac)
      CALL alcerr(iac,'hlpStr',(/6,1/),srName)

      DO iEcc = 1,eccent%nEcc

        hlpStr = ' '

        IF (eccent%ecc(iEcc)%staNum > 0) &
          WRITE(hlpStr(1,1),'(I3)') eccent%ecc(iEcc)%staNum

        hlpStr(2,1) = eccent%ecc(iEcc)%staNam
        hlpStr(3,1) = eccent%ecc(iEcc)%cenNam

        IF (eccent%ecc(iEcc)%xEccen(1) /= 0d0 .OR. &
            eccent%ecc(iEcc)%xEccen(2) /= 0d0 .OR. &
            eccent%ecc(iEcc)%xEccen(3) /= 0d0) THEN
          DO ii = 1,3
            WRITE(hlpStr(3+ii,1),'(F11.4)') eccent%ecc(iEcc)%xEccen(ii)
          ENDDO
        ENDIF

        WRITE(output%value(iEcc),'(6(A,1X))') &
              (qt // TRIM(hlpStr(ii,1)) // qt,  ii=1,6)
      ENDDO

      DEALLOCATE(eccent%ecc,stat=iac)
      DEALLOCATE(hlpStr, stat=iac)

    ENDIF

! Deallocate keys-record
! ----------------------
    DO ii = 1, SIZE(keys)
      IF ( ASSOCIATED(keys(ii)%value) ) THEN
        DEALLOCATE(keys(ii)%value, STAT=iac)
      ENDIF
    END DO
    DEALLOCATE(keys, STAT=iac)


! Store a station selection file
! ------------------------------
  ELSE IF (keyWord == 'ECC_SAVE') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_OUTPUT', eccFil,irc)
    CALL gtflna(1,'FILE_INPUT',  edtFil,irc)

! Reset the Name of the INP-File
! ------------------------------
    CALL readinpf(edtFil,inpKey)
    CALL readkeys('', keyValue, irc)

! Which type has to be stored?
! ----------------------------
    irCode = 0

    CALL readKeys('ECCTYP',keyValue,irc)

    CALL ckoptc(1,'ECCTYP',keyValue,eccTyp,srName,  &
                'File type selection',irc,irCode,   &
                maxVal=1,result1=eccent%eccTyp)

! Allocate the memory for the key list
! ------------------------------------
    ALLOCATE(keys(3),stat=irc)
    CALL alcerr(irc,'keys',(/3/),srName)

! Put the keyword into the list
! -----------------------------
    IF (eccent%eccTyp == 1) THEN
      keys(1)%name = 'TITLE1'
      keys(2)%name = 'DATUM1_STRG'
      keys(3)%name = 'STAECC_NEU'
    ELSE
      keys(1)%name = 'TITLE2'
      keys(2)%name = 'DATUM2_STRG'
      keys(3)%name = 'STAECC_XYZ'
    ENDIF

! Get the title line
! ------------------
    CALL readkeys(keys(1)%name, keyValue, irc)

    CALL ckoptl(0,keys(1)%name, keyValue,srName, &
                'Eccent.file title line',irc,irCode, &
                empty=' ',maxVal=1,result1=eccent%title)

    ! Datum string
    CALL readkeys(keys(2)%name , keyValue, irc)

    CALL ckoptl(1,keys(2)%name,keyValue,srName,            &
                'Eccent file: local datum',irc,irCode, &
                maxVal=1,result1=eccent%datum%name)

! Get the list of entries
! -----------------------
    CALL readkeys(keys(3)%name, keyValue, irc)
    irCode = irCode + irc

    IF (irc == 0) THEN

! Allocate the corresponding memory
! ---------------------------------
      eccent%nEcc = SIZE(keyValue)

      ALLOCATE(eccent%ecc(eccent%nEcc),stat=iac)
      CALL alcerr(iac,'eccent%ecc',(/eccent%nEcc/),srName)

      ALLOCATE(hlpStr(6,eccent%nEcc),stat=ios)
      CALL alcerr(ios,'hlpStr',(/6,eccent%nEcc/),srName)

! Extract the station eccent. record
! ----------------------------------
      CALL ckoptu(1,keys(3)%name,keyValue,srName,      &
                  'Eccentricity file',irc,irCode,      &
                  numCol=6,maxVal=eccent%nEcc,         &
                  result2=hlpStr)
    ELSE
      ALLOCATE(hlpStr(6,1),stat=ios)
      CALL alcerr(ios,'hlpStr',(/6,1/),srName)
    ENDIF

    IF (eccent%nEcc == 1 .AND. LEN_TRIM(hlpStr(2,1)) == 0) eccent%nEcc = 0

    IF (irCode == 0 .AND. eccent%nEcc > 0) THEN
      CALL ckopti(1,keys(3)%name,hlpStr(1,:),srName,   &
                  'Eccentricity file',irc,irCode,      &
                  colTit='Station numbers',maxVal=eccent%nEcc, &
                  empty=0,result2=eccent%ecc(:)%staNum)

      CALL ckoptl(1,keys(3)%name,hlpStr(2,:),srName,   &
                  'Eccentricity file',irc,irCode,      &
                  colTit='Station names',maxVal=eccent%nEcc, &
                  empty=' ',result2=eccent%ecc(:)%staNam)

      CALL ckoptl(1,keys(3)%name,hlpStr(3,:),srName,   &
                  'Eccentricity file',irc,irCode,      &
                  colTit='Center names',maxVal=eccent%nEcc, &
                  empty=' ',result2=eccent%ecc(:)%cenNam)

      DO iEcc=1,3
        CALL ckoptr(1,keys(2)%name,hlpStr(3+iEcc,:),srName,     &
                    'Eccentricity file',irc,irCode,             &
                    colTit='Eccentricities',maxVal=eccent%nEcc, &
                    empty=0d0,result2=eccent%ecc(:)%xEccen(iEcc))
      ENDDO
    ENDIF

! Deallocate keys-record
! ----------------------
    DEALLOCATE(keys,  STAT=iac)
    DEALLOCATE(hlpStr,STAT=iac)

    IF  (irCode /= 0) CALL exitrc(2)

! Check the datum string
! ----------------------
    CALL getdat(eccent%datum%name,eccent%datum%aell,eccent%datum%bell, &
                eccent%datum%dxell,eccent%datum%drell,eccent%datum%scell)

! Station name and center name have to be different
! -------------------------------------------------
    DO iEcc=1,eccent%nEcc
      IF(eccent%ecc(iEcc)%staNam == eccent%ecc(iEcc)%cenNam) THEN
        WRITE(lfnerr,'(/,A,/,18X,2A,/)')                                    &
        ' *** SR MENU_ECC: Eccenter and center may not have the same name', &
                          'Station name: ',TRIM(eccent%ecc(iEcc)%cenNam)
        CALL exitrc(2)
      ENDIF
    ENDDO

! Date and time in title line
! ---------------------------
    CALL dattim(date,time)
    eccent%title(65:80) = ' '//date//' '//time

! Write the resulting file (or give an error)
! -------------------------------------------
    IF (LEN_TRIM(eccent%ecc(1)%staNam) == 0) eccent%nEcc = 0
    CALL writecc(eccFil,eccent)

  ENDIF ! which action

  DEALLOCATE(keyValue,stat=iac)

  RETURN
END SUBROUTINE menu_ecc

END MODULE
