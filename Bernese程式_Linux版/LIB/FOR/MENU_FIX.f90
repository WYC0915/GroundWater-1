MODULE s_MENU_FIX
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_fix(keyWord)

! -------------------------------------------------------------------------
! Purpose:    Is used for creating, editing, and storing of Bernese
!             station selection files using the menu program
!             (called by the menu program via MENUAUX)
!
! Author:     R. Dach
!
! Created:    22-Aug-2001
!
! Changes:    17-Sep-2001 RD: getcoo variables allocatable
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             03-May-2002 RD: Use ckopt-SRs
!                             New keyword structure in MENUAUX.INP
!             25-Sep-2002 HU: Remove i_astlib
!             13-Dec-2002 RD: New quotes-handling in SR writekey
!             19-Feb-2003 RD: Stop with error (new logic in menu)
!             23-Apr-2003 AJ: Nullify local pointers
!             16-May-2003 MM: Initialize and deallocate structure
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
  USE m_bern,   ONLY: i4b, r8b, t_key, &
                      keyValueLength, staNameLength, lineLength, shortLineLength
  USE d_inpkey, ONLY: inpkey
  USE d_stalst, ONLY: t_staList, init_staList
  USE p_menaux, ONLY: qt
  USE s_alcerr
  USE s_inquire
  USE s_readinpf
  USE s_dattim
  USE s_getco3
  USE s_writstsg
  USE s_writekey
  USE s_readkeys
  USE s_exitrc
  USE s_gtflna
  USE s_readstsg
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
  CHARACTER(LEN=8), PARAMETER   :: srName = 'menu_fix'

! Local Variables
! ---------------
  TYPE(t_staList)               :: staList ! Station names from READSTSG

  TYPE(t_key),                   &
      DIMENSION(:), ALLOCATABLE :: keys    ! Writing an EDIT panel

  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:), POINTER     :: keyValue

  CHARACTER(LEN=keyValueLength) :: fixFil  ! Station selection file
  CHARACTER(LEN=keyValueLength) :: edtFil  ! EDIT FIX input panel
  CHARACTER(LEN=keyValueLength) :: crdFil  ! Coodinate file name
  CHARACTER(LEN=staNameLength)  :: staHlp
  CHARACTER(LEN=staNameLength),  &
      DIMENSION(:), POINTER     :: stname  ! Station names from GETCOO
  CHARACTER(LEN=1),              &
      DIMENSION(1)              :: flags
  CHARACTER(LEN=9)              :: date
  CHARACTER(LEN=5)              :: time

  INTEGER(i4b)                  :: nstat   ! Number of stations from GETCOO
  INTEGER(i4b)                  :: nflag
  INTEGER(i4b)                  :: numKeys ! Writing a FIXEDIT panel
  INTEGER(i4b)                  :: ii
  INTEGER(i4b)                  :: irCode
  INTEGER(i4b)                  :: ios, irc, iStat

  LOGICAL                       :: yes
  LOGICAL                       :: sorted

! Init pointers and variables
! ---------------------------
  NULLIFY(keyValue)
  NULLIFY(stname)
  CALL init_staList(staList)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'FIX_EDIT' .AND. keyWord /= 'FIX_SAVE') RETURN

! Editing/Create a CRD file
! -------------------------
  IF (keyWord == 'FIX_EDIT') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_EDIT',    fixFil,irc)
    CALL gtflna(1,'FILE_SKELETON',edtFil,irc)
    CALL gtflna(0,'FILE_CRD',     crdFil,irc)

! Init the new key record
! -----------------------
    numKeys = 3
    ALLOCATE(keys(numKeys), STAT=istat)
    CALL alcerr(iStat, 'keys', (/numKeys/), srName)

    DO ii = 2, numKeys
      ALLOCATE( keys(ii)%value(1), STAT=istat )
      CALL alcerr(iStat, 'keys(ii)%value', (/1/), srName)
    END DO
    keys(1)%name = 'STALIST'
    keys(2)%name = 'STALSTRS'
    keys(3)%name = 'TITLE'

! Does the Station Selection File Exist ?
! ---------------------------------------
    CALL INQUIRE(FILE=fixFil , EXIST=yes)

    IF (yes) THEN
      CALL readstSG(fixFil,0,staList)

    ELSE

! Init Entries for a new station selection File
! ---------------------------------------------
      staList%title = ''
      staList%nsta  = 0

! Generate a new file from a coordinate file
! ------------------------------------------
      IF (LEN_TRIM(crdFil) > 0) THEN

! Read an coordinate file
! -----------------------
        nflag    = 1
        flags(1) = '@'
        CALL getco3(crdFil, nflag, flags, nstat, stname)

! Put the list of stations into the record
! ----------------------------------------
        ALLOCATE(staList%staNam(nstat),STAT=ios)
        CALL alcerr(ios,'staList%staNam',(/nstat/),srName)

        staList%nSta = nstat
        staList%staNam(1:nstat) = stname(1:nstat)

        DEALLOCATE(stname, stat=irc)

! Sort station names
! ------------------
        sorted=.FALSE.
        DO WHILE (.NOT. sorted)
          sorted = .TRUE.
          DO ii=1,nStat-1
            IF (staList%staNam(ii) > staList%staNam(ii+1)) THEN
              staHlp               = staList%staNam(ii)
              staList%staNam(ii)   = staList%staNam(ii+1)
              staList%staNam(ii+1) = staHlp

              sorted = .FALSE.
            ENDIF
          ENDDO
        ENDDO
      ENDIF
    ENDIF

    IF (staList%nsta > 0) THEN
      ALLOCATE(keys(1)%value(staList%nsta), stat=iStat)
      CALL alcerr(iStat, 'keys(1)%value', (/staList%nsta/), srName)
      DO ii = 1, staList%nsta
        WRITE(keys(1)%value(ii), '(a1,a,a1)') qt, TRIM(staList%staNam(ii)), qt
      END DO
    ELSE
      ALLOCATE(keys(1)%value(1), stat=iStat)
      CALL alcerr(iStat, 'keys(1)%value', (/1/), srName)
      keys(1)%value(1)=''
      DO ii = 1,1
        keys(1)%value(1) = TRIM(keys(1)%value(1)) // ' ' // qt//qt
      END DO
    ENDIF

! Fill the other fields
! ---------------------
    keys(2)%value(1) = fixFil
    keys(3)%value(1) = staList%title(1:64)

! Write the input file
! --------------------
    CALL writeKey(edtFil, keys, 0, irc)

! Deallocate keys-record
! ----------------------
    DO ii = 1, SIZE(keys)
      IF ( ASSOCIATED(keys(ii)%value) ) THEN
        DEALLOCATE(keys(ii)%value, STAT=irc)
      ENDIF
    END DO
    DEALLOCATE(keys, STAT=irc)

! Store a station selection file
! ------------------------------
  ELSE IF (keyWord == 'FIX_SAVE') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_OUTPUT', fixFil,irc)
    CALL gtflna(1,'FILE_INPUT',  edtFil,irc)

! Reset the Name of the INP-File
! ------------------------------
    irCode = 0
    CALL readinpf(edtFil,inpKey)
    CALL readkeys('', keyValue, irc)

    ! Title line
    CALL readkeys('TITLE' , keyValue, irc)

    CALL ckoptl(0,'TITLE',keyValue,srName,                 &
                'Sta.selection file: title line',irc,irCode,  &
                empty=' ',maxVal=1,result1=staList%title)

    ! Station list
    CALL readkeys('STALIST', keyValue, irc)

    staList%nSta = SIZE(keyValue)

    DEALLOCATE(staList%stanam,stat=ios)
    ALLOCATE(staList%staNam(staList%nSta),stat=ios)
    CALL alcerr(ios,'staList%staNam',(/staList%nSta/),srName)

    CALL ckoptl(1,'STALIST',keyValue,srName,                 &
                'Sta.selection file: station list',irc,irCode,  &
                empty=' ',maxVal=staList%nSta,result2=staList%staNam)

    IF (irCode /= 0) CALL exitrc(2)

    IF (LEN_TRIM(staList%staNam(1)) == 0) staList%nSta = 0

! Date and time in title line
! ---------------------------
    CALL dattim(date,time)
    staList%title(65:80) = ' '//date//' '//time

! Write file
! ----------
    CALL writstsg(fixFil,0,staList)

  ENDIF

  DEALLOCATE(keyValue,stat=irc)
  DEALLOCATE(staList%sigma,stat=irc)

  RETURN
END SUBROUTINE menu_fix

END MODULE
