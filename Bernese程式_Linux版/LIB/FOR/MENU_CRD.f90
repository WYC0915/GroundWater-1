MODULE s_MENU_CRD
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_crd(keyWord)

! -------------------------------------------------------------------------
! Purpose:    Is used for creating, editing, and storing of Bernese
!             coordinates files using the menu program
!             (called by the menu program via MENUAUX)
!
! Author:     R. Dach
!
! Created:    04-Jul-2001
!
! Changes:    17-Sep-2001 RD: Allocatable size of coordinate file
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             02-May-2002 RD: Use ckopt-SRs
!                             New keyword structure in MENUAUX.INP
!             25-Sep-2002 HU: Remove i_astlib
!             13-Dec-2002 RD: New quotes-handling in SR writekey
!             19-Feb-2003 RD: Stop with error (new logic in menu)
!             27-Feb-2003 HU: DATUM from D_DATUM
!             23-Apr-2003 AJ: Nullify local pointers
!             18-Oct-2003 HU: Write date/time to title
!             06-Nov-2003 HB: Check if pointer (keys(ii)%value) is
!                             ASSOCIATED before DEALLOCATE
!             19-Nov-2003 RD: Reread INP-file using READINPF
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             11-Feb-2011 RD: New call of WTSTAT
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
  USE m_bern,   ONLY: i4b, r8b, t_key, &
                      keyValueLength, staNameLength, lineLength, shortLineLength
  USE d_inpkey, ONLY: inpkey
  USE d_datum,  ONLY: datum
  USE p_menaux, ONLY: qt
  USE s_wtstat
  USE s_ckoptr
  USE s_alcerr
  USE s_ckoptu
  USE s_ckoptz
  USE s_inquire
  USE s_readinpf
  USE s_dattim
  USE s_getco3
  USE s_timst2
  USE s_writekey
  USE s_readkeys
  USE s_getdat
  USE s_exitrc
  USE s_gtflna
  USE s_ckopti
  USE s_ckoptl
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)              :: keyWord     ! what to do

! output:

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER   :: srName = 'menu_crd'

! Local Variables
! ---------------
  TYPE(t_key),                   &
      DIMENSION(:), ALLOCATABLE :: keys    ! Writing a CRDEDIT panel

  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:), POINTER     :: keyValue
  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:,:),ALLOCATABLE:: hlpStr

  CHARACTER(LEN=keyValueLength) :: crdFil  ! Coodinate file name
  CHARACTER(LEN=keyValueLength) :: auxFil  ! Scratch file name
  CHARACTER(LEN=keyValueLength) :: edtFil  ! EDIT CRD input panel
  CHARACTER(LEN=keyValueLength) :: datFil  ! datum file
  CHARACTER(LEN=staNameLength),  &
      DIMENSION(:), POINTER     :: stname  ! Station names from GETCOO
  CHARACTER(LEN=1),              &
      DIMENSION(:), POINTER     :: staflg  ! Station flags from GETCOO
  CHARACTER(LEN=shortLineLength):: title
  CHARACTER(LEN=19)             :: epostr
  CHARACTER(LEN=1),              &
      DIMENSION(1)              :: flags
  CHARACTER(LEN=9)              :: date
  CHARACTER(LEN=5)              :: time

  INTEGER(i4b)                  :: maxStaLoc
  INTEGER(i4b)                  :: nstat   ! Number of stations from GETCOO
  INTEGER(i4b),                  &
      DIMENSION(:), POINTER     :: stanum  ! Station numbers from GETCOO
  INTEGER(i4b)                  :: nflag
  INTEGER(i4b)                  :: numKeys ! Writing a CRDEDIT panel
  INTEGER(i4b)                  :: ii
  INTEGER(i4b)                  :: irCode
  INTEGER(i4b)                  :: irc, iac

  REAL(r8b),                     &
    DIMENSION(:,:), POINTER     :: xstat   ! Station coordinates (XYZ)
  REAL(r8b)                     :: timcrd  ! Epoch of station coordinates

  LOGICAL                       :: yes

! Init pointers and variables
! ---------------------------
  NULLIFY(keyValue)
  NULLIFY(stname)
  NULLIFY(staflg)
  NULLIFY(stanum)
  NULLIFY(xstat)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'CRD_EDIT' .AND. keyWord /= 'CRD_SAVE') RETURN

! Editing/Create a CRD file
! -------------------------
  IF (keyWord == 'CRD_EDIT') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_EDIT',    crdFil,irc)
    CALL gtflna(1,'FILE_SKELETON',edtFil,irc)
    CALL gtflna(1,'FILE_SCRATCH', auxFil,irc)
    CALL gtflna(1,'DATUM',        datFil,irc)

! Does the Coordinate File Exist ?
! --------------------------------
    CALL INQUIRE(FILE=crdFil , EXIST=yes)

! Read an Old Coordinate File
! ---------------------------
    IF (yes) THEN
      nflag    = 1
      flags(1) = '@'
      CALL getco3(crdFil, nflag, flags, nstat, stname,   &
                  stanum = stanum, staFlg = staflg,      &
                  xStat  = xstat,  datum  = datum%name,  &
                  title  = title,  timcrd = timcrd)
      CALL timst2(2,1,timcrd,epostr)
    ELSE

! Init Entries for a new Coordinate File
! --------------------------------------
      title        = ''
      datum%name   = 'ITRF??'
      epostr       = ''
      nstat        = 0
    ENDIF

    numKeys = 7
    ALLOCATE(keys(numKeys), STAT=iac)
    CALL alcerr(iac, 'keys', (/numKeys/), srName)

    DO ii = 2, numKeys
      ALLOCATE( keys(ii)%value(1), STAT=iac )
      CALL alcerr(iac, 'keys(ii)%value', (/1/), srName)
    END DO

    keys(1)%name = 'STACRD'
    keys(2)%name = 'COORDRS'   ;  keys(2)%value(1) = crdFil
    keys(3)%name = 'TITLE'     ;  keys(3)%value(1) = title(1:64)
    keys(4)%name = 'DATUM_STRG';  keys(4)%value(1) = datum%name
    keys(5)%name = 'TIMCRD'    ;  keys(5)%value(1) = epostr
    keys(6)%name = 'AUXFIL'    ;  keys(6)%value(1) = auxFil
    keys(7)%name = 'DATUM'     ;  keys(7)%value(1) = datFil

    IF (nstat > 0) THEN
      ALLOCATE(keys(1)%value(nstat), stat=iac)
      CALL alcerr(iac, 'keys(1)%value', (/nstat/), srName)
      DO ii = 1, nstat
        WRITE(keys(1)%value(ii),                                     &
              '(a1,i3,a1, 1x, a1,a,a1, 2x, 3(a1,f13.4,a1,2x), 3a1)') &
                         qt, stanum(ii),       qt,  &
                         qt, TRIM(stname(ii)), qt,  &
                         qt, xstat(1,ii),      qt,  &
                         qt, xstat(2,ii),      qt,  &
                         qt, xstat(3,ii),      qt,  &
                         qt, staflg(ii),       qt
      END DO
    ELSE
      ALLOCATE(keys(1)%value(1), stat=iac)
      CALL alcerr(iac, 'keys(1)%value', (/1/), srName)
      keys(1)%value(1)=''
      WRITE(keys(1)%value(1),'(6(A,1X))') (qt//qt, ii=1,6)
    ENDIF

    CALL writeKey(edtFil, keys, 0, irc)

! Deallocate all memory
! ---------------------
    DO ii = 1, SIZE(keys)
      IF ( ASSOCIATED(keys(ii)%value) ) THEN
        DEALLOCATE(keys(ii)%value, STAT=iac)
      ENDIF
    END DO
    DEALLOCATE(keys, STAT=iac)

    IF (yes) THEN
      DEALLOCATE(staflg, stat=irc)
      DEALLOCATE(xstat,  stat=irc)
      DEALLOCATE(stanum, stat=irc)
      DEALLOCATE(stname, stat=irc)
    ENDIF

! Store a coordinate File
! -----------------------
  ELSE IF (keyWord == 'CRD_SAVE') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_OUTPUT', crdFil,irc)
    CALL gtflna(1,'FILE_INPUT',  edtFil,irc)

! Reset the Name of the INP-File
! ------------------------------
    irCode = 0
    CALL readinpf(edtFil,inpKey)
    CALL readkeys('', keyValue, irc)

    ! Title line
    CALL readkeys('TITLE' , keyValue, irc)

    CALL ckoptl(0,'TITLE',keyValue,srName,                 &
                'Coordinate file: title line',irc,irCode,  &
                empty=' ',maxVal=1,result1=title)

    ! Datum string
    CALL readkeys('DATUM_STRG' , keyValue, irc)

    CALL ckoptl(1,'DATUM_STRG',keyValue,srName,            &
                'Coordinate file: local datum',irc,irCode, &
                maxVal=1,result1=datum%name)

    ! Epoch time string
    CALL readkeys('TIMCRD', keyValue, irc)

    CALL ckoptz(1,'TIMCRD',keyValue,srName,               &
                'Coordinate file: epoch',irc,irCode,      &
                empty=0d0,ge=44251d0,maxVal=1,error=0d0,  &
                result1=timcrd)

    ! Coordinate record
    CALL readkeys('STACRD', keyValue, irc)
    nstat = SIZE( keyValue )

    maxStaLoc = nStat
    IF (maxStaLoc == 0) maxStaLoc = 1

! Allocate the memory for the coordinate records
! ----------------------------------------------
    ALLOCATE(hlpStr(6,maxStaLoc),stat=irc)
    CALL alcerr(irc, 'hlpStr', (/6,maxStaLoc/), srName)

    ALLOCATE(stname(maxStaLoc), stat=irc)
    CALL alcerr(irc, 'stname', (/maxStaLoc/),   srName)

    ALLOCATE(stanum(maxStaLoc), stat=irc)
    CALL alcerr(irc, 'stanum', (/maxStaLoc/),   srName)

    ALLOCATE(xstat(3,maxStaLoc), stat=irc)
    CALL alcerr(irc, 'xstat',  (/3,maxStaLoc/), srName)

    ALLOCATE(staflg(maxStaLoc), stat=irc)
    CALL alcerr(irc, 'staflg', (/maxStaLoc/),   srName)

! Extract the information
! -----------------------
    CALL ckoptu(1,'STACRD',keyValue,srName,                 &
                'Coordinate file: data records',irc,irCode, &
                numCol=SIZE(hlpStr,1),maxVal=SIZE(hlpStr,2),&
                result2=hlpStr)

    IF (irCode /= 0) CALL exitrc(2)

    ! Station number
    CALL ckopti(1,'STACRD',hlpStr(1,:),srName,              &
                'Coordinate file: data records',irc,irCode, &
                colTit='Station numbers',maxVal=SIZE(hlpStr,2),&
                ge=1,empty=0,result2=staNum)

    ! Station name
    CALL ckoptl(1,'STACRD',hlpStr(2,:),srName,              &
                'Coordinate file: data records',irc,irCode, &
                colTit='Station names',maxVal=SIZE(hlpStr,2),&
                empty=' ',result2=stName)

    ! Station cordinates
    DO ii=1,3
      CALL ckoptr(1,'STACRD',hlpStr(2+ii,:),srName,           &
                  'Coordinate file: data records',irc,irCode, &
                  colTit='Coordinates',maxVal=SIZE(hlpStr,2), &
                  empty=0d0,result2=xStat(ii,:))
    ENDDO

    ! Coordinate flag
    CALL ckoptl(1,'STACRD',hlpStr(6,:),srName,              &
                'Coordinate file: data records',irc,irCode, &
                colTit='Station flags',maxVal=SIZE(hlpStr,2),&
                empty=' ',result2=staFlg)

    DEALLOCATE(hlpStr,stat=irc)
    IF (irCode /= 0) CALL exitrc(2)

    IF (LEN_TRIM(stname(1)) == 0) nstat = 0

! Check the datum string
! ----------------------
    CALL getdat(datum%name,datum%aell,datum%bell, &
                datum%dxell,datum%drell,datum%scell)

! Date and time in title line
! ---------------------------
    CALL dattim(date,time)
    title(65:80) = ' '//date//' '//time

! Write the new coordinate file
! -----------------------------
    CALL wtstat(0, crdFil, title, datum%name, nstat, stname,  &
                xstat, stanum, staflg, timcrd)

    DEALLOCATE(staflg, stat=irc)
    DEALLOCATE(xstat, stat=irc)
    DEALLOCATE(stanum, stat=irc)
    DEALLOCATE(stname, stat=irc)

  ENDIF

  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE menu_crd

END MODULE
