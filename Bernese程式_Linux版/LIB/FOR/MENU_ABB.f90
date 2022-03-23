MODULE s_MENU_ABB
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_abb(keyWord)

! -------------------------------------------------------------------------
! Purpose:    Is used for creating, editing, and storing of Bernese
!             abbreviation tables using the menu program
!             (called by the menu program via MENUAUX)
!
! Author:     R. Dach
!
! Created:    03-May-2002
!
! Changes:    10-Jul-2002 RD: Activate ABB_SAVE
!             25-Sep-2002 HU: Remove i_astlib
!             12-Dec-2002 RD: Correct quotes-handling in SR writekey
!             14-Mar-2003 RD: New read ab write routines for abbreviations
!             23-Apr-2003 AJ: Nullify local pointers
!             19-May-2003 HU: Initialize structure
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
  USE m_bern,   ONLY: i4b, t_key, &
                      keyValueLength, staNameLength, lineLength
  USE d_inpkey, ONLY: inpkey
  USE d_abbrev, ONLY: t_abbrev, init_abbrev
  USE p_menaux, ONLY: qt
  USE s_readabb
  USE s_exitrc
  USE s_writekey
  USE s_alcerr
  USE s_ckoptu
  USE s_inquire
  USE s_readkeys
  USE s_gtflna
  USE s_updabb
  USE s_writabb
  USE s_readinpf
  USE s_dattim
  USE s_ckoptl
  USE s_getco3
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)                       :: keyWord     ! what to do

! output:


! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER   :: srName = 'menu_abb'

! Local Variables
! ---------------
  TYPE(t_key),                   &
      DIMENSION(:), ALLOCATABLE :: keys    ! Writing an EDIT panel
  TYPE(t_abbrev)                :: abbrev  ! Abbreviation structure

  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:), POINTER     :: keyValue

  CHARACTER(LEN=keyValueLength) :: abbFil  ! Abbrev. table file
  CHARACTER(LEN=keyValueLength) :: edtFil  ! EDIT ABB input panel
  CHARACTER(LEN=keyValueLength) :: crdFil  ! Coodinate file name
  CHARACTER(LEN=lineLength),     &
      DIMENSION(:,:),POINTER    :: hlpStr
  CHARACTER(LEN=staNameLength),  &
      DIMENSION(:), POINTER     :: stname  ! Station names from GETCOO
  CHARACTER(LEN=1),              &
      DIMENSION(1)              :: flags
  CHARACTER(LEN=9)              :: date
  CHARACTER(LEN=5)              :: time

  INTEGER(i4b)                  :: nstat   ! Number of stations from GETCOO
  INTEGER(i4b)                  :: nflag
  INTEGER(i4b)                  :: numKeys ! Writing a ABBEDIT panel
  INTEGER(i4b)                  :: iAbb
  INTEGER(i4b)                  :: iSta
  INTEGER(i4b)                  :: ii
  INTEGER(i4b)                  :: irCode
  INTEGER(i4b)                  :: irc, iac

  LOGICAL                       :: yes

! Init pointers and variables
! ---------------------------
  NULLIFY(keyValue)
  NULLIFY(hlpStr)
  NULLIFY(stname)
  CALL init_abbrev(abbrev)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'ABB_EDIT' .AND. keyWord /= 'ABB_SAVE') RETURN

! Editing/Create a CRD file
! -------------------------
  IF (keyWord == 'ABB_EDIT') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_EDIT',    abbFil,irc)
    CALL gtflna(1,'FILE_SKELETON',edtFil,irc)
    CALL gtflna(0,'FILE_CRD',     crdFil,irc)

! Init the new key record
! -----------------------
    numKeys = 3
    ALLOCATE(keys(numKeys), STAT=iac)
    CALL alcerr(iac, 'keys', (/numKeys/), srName)

    DO ii = 2, numKeys
      ALLOCATE( keys(ii)%value(1), STAT=iac )
      CALL alcerr(iac, 'keys(ii)%value', (/1/), srName)
    END DO
    keys(1)%name = 'LIST_OF_ABBREV'
    keys(2)%name = 'TITLE'
    keys(3)%name = 'ABBREVRS'

! Does the abbreviation table exist ?
! -----------------------------------
    CALL INQUIRE(FILE=abbFil , EXIST=yes)

    IF (yes) THEN

      CALL readAbb(abbFil,abbrev)

    ELSE

! Init Entries for a new station selection File
! ---------------------------------------------
      abbrev%title = ' '
      abbrev%nAbb  = 0

! Generate a new file from a coordinate file
! ------------------------------------------
      IF (LEN_TRIM(crdFil) > 0) THEN

! Read a Coordinate File
! ----------------------
        nflag    = 1
        flags(1) = '@'
        CALL getco3(crdFil, nflag, flags, nstat, stname)

! Put station names into the abbrev. list
! ---------------------------------------
        IF (nStat > 0) THEN
          abbrev%nAbb = 0
          ALLOCATE(abbrev%abb(nStat),stat=iac)
          CALL alcerr(iac,'abbrev%abb',(/nStat/),srName)

          DO iSta = 1,nStat
            CALL updabb(1,stName(iSta),' ',abbrev,irc)
          ENDDO
          abbrev%abb(:)%remark = ' '
        ENDIF
        DEALLOCATE(stName,stat=iac)
      ENDIF
    ENDIF

! No stations in list
! -------------------
    IF (abbrev%nAbb == 0) THEN
      ALLOCATE( keys(1)%value(1), STAT=iac )
      CALL alcerr(iac, 'keys(1)%value', (/1/), srName)
      keys(1)%value(1) = qt//qt // ' ' // qt//qt // ' ' // &
                         qt//qt // ' ' // qt//qt

! Put abbrevation list into the edit panel
! ----------------------------------------
    ELSE
      ALLOCATE( keys(1)%value(abbrev%nAbb), STAT=iac )
      CALL alcerr(iac, 'keys(1)%value', (/abbrev%nAbb/), srName)

      DO iAbb = 1,abbrev%nAbb
        WRITE(keys(1)%value(iAbb),'(4(A,1X))')  &
              qt // abbrev%abb(iAbb)%staNam // qt,    &
              qt // abbrev%abb(iAbb)%staAb4 // qt,    &
              qt // abbrev%abb(iAbb)%staAb2 // qt,    &
              qt // abbrev%abb(iAbb)%remark // qt
      ENDDO
    ENDIF

! Fill the other fields
! ---------------------
    keys(2)%value(1) = abbrev%title(1:64)
    keys(3)%value(1) = abbFil

! Write the input file
! --------------------
    CALL writeKey(edtFil, keys, 0, irc)

    DEALLOCATE(abbrev%abb,stat=irc)

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
  ELSE IF (keyWord == 'ABB_SAVE') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_OUTPUT', abbFil,irc)
    CALL gtflna(1,'FILE_INPUT',  edtFil,irc)

! Reset the Name of the INP-File
! ------------------------------
    irCode = 0
    CALL readinpf(edtFil,inpKey)
    CALL readkeys('',keyValue,irc)

! Read the abbrev. list from edit file
! ------------------------------------
    ! Title line
    CALL readkeys('TITLE' , keyValue, irc)

    CALL ckoptl(0,'TITLE',keyValue,srName,                      &
                'Abbreviation table: title line',irc,irCode,    &
                empty=' ',maxVal=1,result1=abbrev%title)

    ! Abbreviation record
    CALL readkeys('LIST_OF_ABBREV', keyValue, irc)
    abbrev%nAbb = SIZE( keyValue )

! Allocate the memory for the abbreviation records
! ------------------------------------------------
    ALLOCATE(hlpStr(4,abbrev%nAbb),stat=iac)
    CALL alcerr(iac, 'hlpStr', (/4,abbrev%nAbb/), srName)

    ALLOCATE(abbrev%abb(abbrev%nAbb), stat=iac)
    CALL alcerr(iac, 'abbrev%abb', (/abbrev%nAbb/),   srName)

! Extract the information
! -----------------------
    CALL ckoptu(1,'LIST_OF_ABBREV',keyValue,srName,                 &
                'Abbreviation records',irc,irCode,                  &
                numCol=SIZE(hlpStr,1),maxVal=SIZE(hlpStr,2),        &
                result2=hlpStr)

    IF (irCode /= 0) CALL exitrc(2)

    ! Station name
    CALL ckoptl(1,'LIST_OF_ABBREV',hlpStr(1,:),srName,              &
                'Abbreviation records',irc,irCode,                  &
                colTit='Station names',maxVal=SIZE(hlpStr,2),       &
                empty=' ',result2=abbrev%abb(:)%stanam)

    ! Stations 4-ID
    CALL ckoptl(1,'LIST_OF_ABBREV',hlpStr(2,:),srName,              &
                'Abbreviation records',irc,irCode,                  &
                colTit='4-Character ID',maxVal=SIZE(hlpStr,2),      &
                empty=' ',result2=abbrev%abb(:)%staab4)

    ! Stations 2-ID
    CALL ckoptl(1,'LIST_OF_ABBREV',hlpStr(3,:),srName,              &
                'Abbreviation records',irc,irCode,                  &
                colTit='2-Character ID',maxVal=SIZE(hlpStr,2),      &
                empty=' ',result2=abbrev%abb(:)%staab2)

    ! Remarks
    CALL ckoptl(1,'LIST_OF_ABBREV',hlpStr(4,:),srName,              &
                'Abbreviation records',irc,irCode,                  &
                colTit='Remark',maxVal=SIZE(hlpStr,2),              &
                empty=' ',result2=abbrev%abb(:)%remark)

    IF (irCode /= 0) CALL exitrc(2)

    DEALLOCATE(hlpStr,stat=iac)

    IF (LEN_TRIM(abbrev%abb(1)%stanam) == 0) abbrev%nAbb = 0

! Date and time in title line
! ---------------------------
    CALL dattim(date,time)
    abbrev%title(65:80) = ' '//date//' '//time

! Write the new abbrev. table
! ---------------------------
    CALL writAbb(abbFil, abbrev)

    DEALLOCATE(abbrev%abb,stat=irc)

  ENDIF

  DEALLOCATE(keyValue,stat=iac)
  DEALLOCATE(hlpStr,stat=iac)
  DEALLOCATE(stname,stat=iac)

  RETURN
END SUBROUTINE menu_abb

END MODULE
