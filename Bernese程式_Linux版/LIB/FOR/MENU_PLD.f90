MODULE s_MENU_PLD
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_pld(keyWord)

! -------------------------------------------------------------------------
! Purpose:    Is used for creating, editing, and storing of Bernese
!             plate files using the menu program
!             (called by the menu program via MENUAUX)
!
! Author:     R. Dach
!
! Created:    08-May-2002
!
! Changes:    25-Sep-2002 HU: Remove i_astlib
!             13-Dec-2002 RD: New quotes-handling in SR writekey
!             19-Feb-2003 RD: Stop with error (new logic in menu)
!             27-Feb-2003 HU: DATUM from D_DATUM
!             23-Apr-2003 AJ: Nullify local pointers
!             18-Oct-2003 HU: Write date/time to title
!             06-Nov-2003 HB: Check if pointer (keys(ii)%value) is
!                             ASSOCIATED before DEALLOCATE
!             19-Nov-2003 RD: Reread INP-file using READINPF
!             17-Feb-2011 RD: Remove MAXSTA-COMMON (unused)
!             17-FEb-2011 RD: WTSTAT replaces WTVELO
!             20-Sep-2012 RD: Use M_BERN with ONLY
!             20-Sep-2012 RD: Correctly deallocate arrays
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, t_key, &
                      keyValueLength, staNameLength, lineLength, shortLineLength
  USE d_inpkey, ONLY: inpkey
  USE d_datum,  ONLY: datum
  USE p_menaux, ONLY: qt
  USE s_alcerr
  USE s_ckoptu
  USE s_inquire
  USE s_readinpf
  USE s_dattim
  USE s_getco3
  USE s_writekey
  USE s_readkeys
  USE s_getdat
  USE s_exitrc
  USE s_wtstat
  USE s_gtflna
  USE s_ckopti
  USE s_ckoptl
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
  CHARACTER(LEN=8), PARAMETER   :: srName = 'menu_pld'

! Local Variables
! ---------------
  TYPE(t_key),                   &
      DIMENSION(:), ALLOCATABLE :: keys    ! Writing a PLDEDIT panel

  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:), POINTER     :: keyValue
  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:,:),ALLOCATABLE:: hlpStr

  CHARACTER(LEN=keyValueLength) :: pldFil  ! Plate file name
  CHARACTER(LEN=keyValueLength) :: auxFil  ! Scratch file name
  CHARACTER(LEN=keyValueLength) :: edtFil  ! EDIT VEL input panel
  CHARACTER(LEN=keyValueLength) :: crdFil  ! Coodinate file name
  CHARACTER(LEN=keyValueLength) :: datFil  ! datum file
  CHARACTER(LEN=staNameLength),  &
      DIMENSION(:), POINTER     :: stname  ! Station names from GTVELO
  CHARACTER(LEN=staNameLength)  :: stanam
  CHARACTER(LEN=4),              &
      DIMENSION(:), POINTER     :: plate   ! Tectonic plate from GTVELO
  CHARACTER(LEN=shortLineLength):: title
  CHARACTER(LEN=1),              &
      DIMENSION(:), POINTER     :: staflg  ! Station flags for WTVELO
  CHARACTER(LEN=1),              &
      DIMENSION(1)              :: flags
  CHARACTER(LEN=9)              :: date
  CHARACTER(LEN=5)              :: time

  INTEGER(i4b)                  :: maxStaLoc
  INTEGER(i4b)                  :: nstat   ! Number of stations from GTVELO
  INTEGER(i4b),                  &
      DIMENSION(:), POINTER     :: stanum  ! Station numbers from GTVELO
  INTEGER(i4b)                  :: nflag
  INTEGER(i4b)                  :: numKeys ! Writing a PLDEDIT panel
  INTEGER(i4b)                  :: ii,jj
  INTEGER(i4b)                  :: irCode
  INTEGER(i4b)                  :: irc, iac

  REAL(r8b),                     &
    DIMENSION(:,:), POINTER     :: xVel    ! Dummy for WTVELO

  LOGICAL                       :: yes
  LOGICAL                       :: sorted

! Init pointers and variables
! ---------------------------
  NULLIFY(keyValue)
  NULLIFY(stname)
  NULLIFY(plate)
  NULLIFY(staflg)
  NULLIFY(stanum)
  NULLIFY(xVel)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'PLD_EDIT' .AND. keyWord /= 'PLD_SAVE') RETURN

! Editing/Create a PLD file
! -------------------------
  IF (keyWord == 'PLD_EDIT') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_EDIT',    pldFil,irc)
    CALL gtflna(1,'FILE_SKELETON',edtFil,irc)
    CALL gtflna(1,'FILE_SCRATCH', auxFil,irc)
    CALL gtflna(0,'FILE_CRD',     crdFil,irc)
    CALL gtflna(1,'DATUM',        datFil,irc)

! Does the Velocity File Exist ?
! ------------------------------
    CALL INQUIRE(FILE=pldFil , EXIST=yes)

! Read an Old Velocity File
! -------------------------
    IF (yes) THEN

      nflag    = 1
      flags(1) = '@'
      CALL getco3(pldFil, nflag, flags, nstat, stname, &
                  datum=datum%name,title=title,        &
                  staNum=stanum, plate=plate)

! Create a new velocity file from crd Fil
! ---------------------------------------
    ELSE IF (LEN_TRIM(crdFil) > 0) THEN

      nflag    = 1
      flags(1) = '@'
      CALL getco3(crdFil, nflag, flags, nstat, stname, &
                  staNum = staNum, datum=datum%name)

      title = ''

      ALLOCATE(plate(nStat),stat=iac)
      CALL alcerr(iac,'plate',(/nStat/),srName)
      plate = ' '

      ! Sort the station names:
      sorted = .FALSE.
      DO WHILE (.NOT. sorted)
        sorted = .TRUE.
        DO ii = 1,nStat-1
          IF (stname(ii) > stName(ii+1)) THEN
            jj=stanum(ii)
            stanum(ii)=stanum(ii+1)
            stanum(ii+1)=jj

            stanam=stname(ii)
            stname(ii)=stname(ii+1)
            stname(ii+1)=stanam

            sorted = .FALSE.
          ENDIF
        ENDDO
      ENDDO

! Init Entries for a new empty Velocity File
! ------------------------------------------
    ELSE
      title        = ''
      datum%name   = 'ITRF??'
      nstat        = 0

    ENDIF

    numKeys = 6
    ALLOCATE(keys(numKeys), STAT=iac)
    CALL alcerr(iac, 'keys', (/numKeys/), srName)

    DO ii = 2, numKeys
      ALLOCATE( keys(ii)%value(1), STAT=iac )
      CALL alcerr(iac, 'keys(ii)%value', (/1/), srName)
    END DO

    keys(1)%name = 'STAPLD'
    keys(2)%name = 'PLATERS'   ;  keys(2)%value(1) = pldFil
    keys(3)%name = 'TITLE'     ;  keys(3)%value(1) = title(1:64)
    keys(4)%name = 'DATUM_STRG';  keys(4)%value(1) = datum%name
    keys(5)%name = 'AUXFIL'    ;  keys(5)%value(1) = auxFil
    keys(6)%name = 'DATUM'     ;  keys(6)%value(1) = datFil

    IF (nstat > 0) THEN
      ALLOCATE(keys(1)%value(nstat), stat=iac)
      CALL alcerr(iac, 'keys(1)%value', (/nstat/), srName)
      DO ii = 1, nstat
        WRITE(keys(1)%value(ii),'(a1,i3,a1, 1x, a1,a,a1, 2x, a1,a4,a1)') &
                         qt, stanum(ii),       qt,  &
                         qt, TRIM(stname(ii)), qt,  &
                         qt, plate(ii),        qt
      END DO
    ELSE
      ALLOCATE(keys(1)%value(1), stat=iac)
      CALL alcerr(iac, 'keys(1)%value', (/1/), srName)
      keys(1)%value(1)=''
      WRITE(keys(1)%value(1),'(3(A,1X))') (qt//qt, ii=1,3)
    ENDIF
    CALL writeKey(edtFil, keys, 0, irc)

    DEALLOCATE(plate,  stat=irc)
    DEALLOCATE(stanum, stat=irc)
    DEALLOCATE(stname, stat=irc)

! Deallocate keys-record
! ----------------------
    DO ii = 1, SIZE(keys)
      IF ( ASSOCIATED(keys(ii)%value) ) THEN
        DEALLOCATE(keys(ii)%value, STAT=iac)
      ENDIF
    END DO
    DEALLOCATE(keys, STAT=iac)


! Store a Velocity File
! ---------------------
  ELSE IF (keyWord == 'PLD_SAVE') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_OUTPUT', pldFil,irc)
    CALL gtflna(1,'FILE_INPUT',  edtFil,irc)

! Reset the Name of the INP-File
! ------------------------------
    irCode = 0
    CALL readinpf(edtFil,inpKey)
    CALL readkeys('', keyValue, irc)

    ! Title line
    CALL readkeys('TITLE' , keyValue, irc)

    CALL ckoptl(0,'TITLE',keyValue,srName,                 &
                'Tect. plate file: title line',irc,irCode,    &
                empty=' ',maxVal=1,result1=title)

    ! Datum string
    CALL readkeys('DATUM_STRG' , keyValue, irc)

    CALL ckoptl(1,'DATUM_STRG',keyValue,srName,            &
                'Tect. plate file: local datum',irc,irCode,   &
                empty=' ',maxVal=1,result1=datum%name)

    ! Velocity record
    CALL readkeys('STAPLD', keyValue, irc)
    nstat = SIZE( keyValue )

    maxStaLoc = nStat
    IF (maxStaLoc == 0) maxStaLoc = 1

! Allocate the memory for the velocity records
! --------------------------------------------
    ALLOCATE(hlpStr(3,maxStaLoc),stat=irc)
    CALL alcerr(irc, 'hlpStr', (/3,maxStaLoc/), srName)

    ALLOCATE(stname(maxStaLoc), stat=irc)
    CALL alcerr(irc, 'stname', (/maxStaLoc/),   srName)
    ALLOCATE(stanum(maxStaLoc), stat=irc)
    CALL alcerr(irc, 'stanum', (/maxStaLoc/),   srName)
    ALLOCATE(xVel(3,maxStaLoc), stat=irc)
    CALL alcerr(irc, 'xVel',   (/3,maxStaLoc/), srName)
    ALLOCATE(staflg(maxStaLoc), stat=irc)
    CALL alcerr(irc, 'staflg', (/maxStaLoc/),   srName)
    ALLOCATE(plate(maxStaLoc), stat=irc)
    CALL alcerr(irc, 'plate', (/maxStaLoc/),    srName)

    xVel = 0d0
    staFlg = ' '

! Extract the information
! -----------------------
    CALL ckoptu(1,'STAPLD',keyValue,srName,                 &
                'Tect. plate file: data records',irc,irCode,   &
                numCol=SIZE(hlpStr,1),maxVal=SIZE(hlpStr,2),&
                result2=hlpStr)

    IF (irCode /= 0) CALL exitrc(2)

    ! Station number
    CALL ckopti(1,'STAPLD',hlpStr(1,:),srName,              &
                'Tect. plate file: data records',irc,irCode,   &
                colTit='Station numbers',maxVal=SIZE(hlpStr,2),&
                ge=1,empty=0,result2=staNum)

    ! Station name
    CALL ckoptl(1,'STAPLD',hlpStr(2,:),srName,              &
                'Tect. plate file: data records',irc,irCode,   &
                colTit='Station names',maxVal=SIZE(hlpStr,2),&
                empty=' ',result2=stName)

    ! Tectonic plate
    CALL ckoptl(1,'STAPLD',hlpStr(3,:),srName,              &
                'Tect. plate file: data records',irc,irCode,   &
                colTit='Tectonic plate',maxVal=SIZE(hlpStr,2),&
                empty=' ',result2=plate)

    DEALLOCATE(hlpStr, stat=irc)
    IF (irCode /= 0) CALL exitrc(2)

    IF (LEN_TRIM(stname(1)) == 0) nstat = 0

! Check the datum string
! ----------------------
    IF (LEN_TRIM(datum%name) > 0) THEN
      CALL getdat(datum%name,datum%aell,datum%bell, &
                  datum%dxell,datum%drell,datum%scell)
    ENDIF

! Date and time in title line
! ---------------------------
    CALL DATTIM(date,time)
    title(65:80) = ' '//date//' '//time

! Write the new velocity file
! ---------------------------
    CALL wtstat(0,pldFil, title, datum%name, nstat, stname, xVel, &
                stanum=stanum, staflg=staflg, plate=plate, pldOnly=.TRUE.)

    DEALLOCATE(staflg, stat=irc)
    DEALLOCATE(xVel,   stat=irc)
    DEALLOCATE(plate,  stat=irc)
    DEALLOCATE(stanum, stat=irc)
    DEALLOCATE(stname, stat=irc)

  ENDIF

  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE menu_pld

END MODULE
