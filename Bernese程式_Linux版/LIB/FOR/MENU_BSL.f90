MODULE s_MENU_BSL
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_bsl(keyWord)

! -------------------------------------------------------------------------
! Purpose:    Is used for creating, editing, and storing of Bernese
!             baseline files using the menu program
!             (called by the menu program via MENUAUX)
!
! Author:     R. Dach
!
! Created:    13-May-2002
!
! Changes:    25-Sep-2002 HU: Remove i_astlib
!             13-Dec-2002 RD: New quotes-handling in SR writekey
!             19-Feb-2003 RD: Stop with error (new logic in menu)
!             23-Apr-2003 AJ: Nullify local pointers
!             06-Nov-2003 HB: Check if pointer (keys(ii)%value) is
!                             ASSOCIATED before DEALLOCATE
!             19-Nov-2003 RD: Reread INP-file using READINPF
!             20-Sep-2012 RD: Use M_BERN with ONLY
!             20-Sep-2012 RD: Correctly deallocate arrays
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, lfnloc, lfnerr, &
                      t_key, keyValueLength, staNameLength, lineLength
  USE d_inpkey, ONLY: inpkey
  USE p_menaux, ONLY: qt
  USE s_alcerr
  USE s_opnfil
  USE s_ckoptu
  USE f_lincount
  USE s_inquire
  USE s_opnerr
  USE s_readinpf
  USE s_getco3
  USE s_writekey
  USE s_readkeys
  USE s_exitrc
  USE s_gtflna
  USE s_ckoptl
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)                       :: keyWord     ! what to do

! output:

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER   :: srName = 'menu_bsl'

! Local Variables
! ---------------
  TYPE(t_key),                   &
      DIMENSION(:), ALLOCATABLE :: keys    ! Writing a BSLEDIT panel

  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:), POINTER     :: keyValue
  CHARACTER(LEN=staNameLength),  &
      DIMENSION(:,:),POINTER    :: baslin
  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:,:),ALLOCATABLE:: hlpStr

  CHARACTER(LEN=keyValueLength) :: bslFil  ! baseline file name
  CHARACTER(LEN=keyValueLength) :: edtFil  ! EDIT BSL input panel
  CHARACTER(LEN=keyValueLength) :: crdFil  ! Coodinate file name
  CHARACTER(LEN=lineLength)     :: line
  CHARACTER(LEN=staNameLength)  :: stanam
  CHARACTER(LEN=staNameLength),  &
      DIMENSION(:), POINTER     :: stname  ! Station names
  CHARACTER(LEN=1),              &
      DIMENSION(1)              :: flags

  INTEGER(i4b)                  :: maxStaLoc
  INTEGER(i4b)                  :: nstat   ! Number of stations from GTVELO
  INTEGER(i4b)                  :: nflag
  INTEGER(i4b)                  :: numKeys ! Writing a BSLEDIT panel
  INTEGER(i4b)                  :: iSta
  INTEGER(i4b)                  :: ii
  INTEGER(i4b)                  :: irCode
  INTEGER(i4b)                  :: irc, iac, ios

  LOGICAL                       :: yes
  LOGICAL                       :: sorted

  NULLIFY(keyValue)
  NULLIFY(baslin)
  NULLIFY(stname)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'BSL_EDIT' .AND. keyWord /= 'BSL_SAVE') RETURN

! Editing/Create a BSL file
! -------------------------
  IF (keyWord == 'BSL_EDIT') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_EDIT',    bslFil,irc)
    CALL gtflna(1,'FILE_SKELETON',edtFil,irc)
    CALL gtflna(0,'FILE_CRD' ,crdFil,irc)

! Does the Baseline File Exist ?
! ------------------------------
    CALL INQUIRE(FILE=bslFil , EXIST=yes)

! Read an Old Baseline File
! -------------------------
    IF (yes) THEN

      nStat = linCount(bslFil,0)

      ALLOCATE(baslin(2,nStat),stat=iac)
      CALL alcerr(iac,'baslin',(/2,nStat/),srName)

      CALL opnfil(lfnloc,bslFil,'OLD','FORMATTED','READONLY',' ',ios)
      CALL opnerr(lfnerr,lfnloc,ios,bslFil,srName)

      iSta = 0
      DO WHILE (ios == 0 .AND. iSta < nStat)
        READ(lfnloc,'(A)',iostat=ios) line
        IF (ios /= 0 .OR. LEN_TRIM(line) == 0) EXIT

        iSta = iSta+1
        READ(line,'(A16,1X,A16)',iostat=irc) (baslin(ii,iSta),ii=1,2)

        IF (irc /= 0) THEN
          WRITE(lfnerr,'(/,A,/,18X,A,A,/,18X,A,I6,/)')             &
          ' *** SR MENU_BSL: Error reading baseline file records', &
                            'File name:     ', TRIM(bslFil),       &
                            'Record number: ',iSta
          CALL exitrc(2)
        ENDIF

      ENDDO

      CLOSE(lfnloc)

! Create a new baseline file from crd Fil
! ---------------------------------------
    ELSE IF (LEN_TRIM(crdFil) > 0) THEN

      nflag    = 1
      flags(1) = '@'
      CALL getco3(crdFil, nflag, flags, nstat, stname)

      ! Sort the station names:
      sorted = .FALSE.
      DO WHILE (.NOT. sorted)
        sorted = .TRUE.
        DO ii = 1,nStat-1
          IF (stname(ii) > stName(ii+1)) THEN
            stanam=stname(ii)
            stname(ii)=stname(ii+1)
            stname(ii+1)=stanam

            sorted = .FALSE.
          ENDIF
        ENDDO
      ENDDO

      ALLOCATE(baslin(2,nStat),stat=iac)
      CALL alcerr(iac,'baslin',(/2,nStat/),srName)

      baslin(1,:) = stName
      baslin(2,:) = ' '

      DEALLOCATE(stName,stat=iac)

! Init Entries for a new empty Velocity File
! ------------------------------------------
    ELSE
      nstat        = 0

    ENDIF

    numKeys = 2
    ALLOCATE(keys(numKeys), STAT=iac)
    CALL alcerr(iac, 'keys', (/numKeys/), srName)

    DO ii = 2, numKeys
      ALLOCATE( keys(ii)%value(1), STAT=iac )
      CALL alcerr(iac, 'keys(ii)%value', (/1/), srName)
    END DO

    keys(1)%name = 'STABSL'
    keys(2)%name = 'BASLINRS'  ;  keys(2)%value(1) = bslFil

    IF (nstat > 0) THEN
      ALLOCATE(keys(1)%value(nstat), stat=iac)
      CALL alcerr(iac, 'keys(1)%value', (/nstat/), srName)
      DO ii = 1, nstat
        WRITE(keys(1)%value(ii),'(2(a1,a,a1, 2x))')    &
                         qt, TRIM(baslin(1,ii)), qt,  &
                         qt, TRIM(baslin(2,ii)), qt
      END DO
    ELSE
      ALLOCATE(keys(1)%value(1), stat=iac)
      CALL alcerr(iac, 'keys(1)%value', (/1/), srName)
      keys(1)%value(1)=''
      WRITE(keys(1)%value(1),'(2(A,1X))') (qt//qt, ii=1,2)
    ENDIF
    CALL writeKey(edtFil, keys, 0, irc)

    DEALLOCATE(baslin,stat=irc)

! Deallocate keys-record
! ----------------------
    DO ii = 1, SIZE(keys)
      IF ( ASSOCIATED(keys(ii)%value) ) THEN
        DEALLOCATE(keys(ii)%value, STAT=iac)
      ENDIF
    END DO
    DEALLOCATE(keys, STAT=iac)

! Store a Baseline File
! ---------------------
  ELSE IF (keyWord == 'BSL_SAVE') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_OUTPUT', bslFil,irc)
    CALL gtflna(1,'FILE_INPUT',  edtFil,irc)

! Reset the Name of the INP-File
! ------------------------------
    irCode = 0
    CALL readinpf(edtFil,inpKey)
    CALL readkeys('',keyValue,irc)

    ! Baseline record
    CALL readkeys('STABSL', keyValue, irc)
    nstat = SIZE( keyValue )

    maxStaLoc = nStat
    IF (maxStaLoc == 0) maxStaLoc = 1

! Allocate the memory for the velocity records
! --------------------------------------------
    ALLOCATE(hlpStr(2,maxStaLoc),stat=irc)
    CALL alcerr(irc, 'hlpStr', (/2,maxStaLoc/), srName)

    ALLOCATE(baslin(2,maxStaLoc), stat=irc)
    CALL alcerr(irc, 'baslin', (/2,maxStaLoc/),   srName)

! Extract the information
! -----------------------
    CALL ckoptu(1,'STABSL',keyValue,srName,                 &
                'Baseline file: data records',irc,irCode,    &
                numCol=SIZE(hlpStr,1),maxVal=SIZE(hlpStr,2),&
                result2=hlpStr)

    IF (irCode /= 0) CALL exitrc(2)

    ! Station name 1
    CALL ckoptl(1,'STABSL',hlpStr(1,:),srName,              &
                'Baseline file: data records',irc,irCode,    &
                colTit='Station names - 1',maxVal=SIZE(hlpStr,2),&
                empty=' ',result2=baslin(1,:))

    IF (LEN_TRIM(baslin(1,1)) == 0) THEN
      nstat = 0
    ELSE
      ! Station name 2
      CALL ckoptl(1,'STABSL',hlpStr(2,:),srName,              &
                  'Baseline file: data records',irc,irCode,    &
                  colTit='Station names - 2',maxVal=SIZE(hlpStr,2),&
                  empty=' ',result2=baslin(2,:))

    ENDIF

    IF (irCode /= 0) CALL exitrc(2)


! Write the new baseline file
! ---------------------------
    CALL opnfil(lfnloc,bslFil,'UNKNOWN','FORMATTED',' ',' ',irc)
    CALL opnerr(lfnerr,lfnloc,irc,bslFil,srName)

    DO iSta = 1, nStat

      WRITE(lfnloc,'(A16,1X,A16)') (baslin(ii,iSta),ii=1,2)

    ENDDO

    CLOSE(lfnloc)

    DEALLOCATE(baslin, stat=irc)
    DEALLOCATE(hlpStr, stat=irc)

  ENDIF

  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE menu_bsl

END MODULE
