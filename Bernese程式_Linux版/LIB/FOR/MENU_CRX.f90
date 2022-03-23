MODULE s_MENU_CRX
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_crx(keyWord, output)

! -------------------------------------------------------------------------
! Purpose:    Is used for creating, editing, and storing of Bernese
!             station information files using the menu program
!             (called by the menu program via MENUAUX)
!
! Author:     R. Dach
!
! Created:    15-Aug-2001
!
! Changes:    04-Aug-2001 RD: A new file is created with a coord. file
!             05-Sep-2001 HU: Interfaces for readcrux, writcrux added
!             07-Sep-2001 RD: Enter a phasecc file to check recnam+antnam
!             17-Sep-2001 RD: Getcoo variables allocatable
!             22-Oct-2001 RD: Use "undef" parameters from d_stacrx
!             21-Dec-2001 HU: M_ADDNEQ replaced by p_addneq
!             03-May-2002 RD: New keyword structure in MENUAUX.INP
!             25-Sep-2002 HU: Remove i_astlib
!             13-Dec-2002 RD: New quotes-handling in SR writekey
!             19-Feb-2003 RD: Stop with error (new logic in menu)
!             10-Mar-2003 HU: Read and write station description
!             12-Mar-2003 HU: Error in section 5 corrected
!             23-Apr-2003 AJ: Nullify local pointers
!             19-Mai-2003 CU: Initialize structures
!             02-Jun-2003 RD: Use SR GPHECC for verification
!             11-Aug-2003 RS: Set maxazi 37 -> 73
!             14-Oct-2003 RD: GPHECC-test without receiver name
!             24-Oct-2003 HU: Remove date/time from title line
!             19-Nov-2003 RD: Reread INP-file using READINPF
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             14-Mar-2006 AG: Parameter declaration for RDAPHC removed
!             12-Jun-2007 AG: Use init_buf instead of GPHECC
!             22-Feb-2010 RD: Init nAnt before calling LISTC1
!             05-Oct-2010 SL: StaFlg(INTEGER->CHARACTER), antser+recser added
!             27-Oct-2010 SL: Use M_BERN with ONLY
!             17-Feb-2011 RD: Remove MAXSTA-COMMON (unused)
!             27-Mar-2012 RD: Use LISTC1 as module now
!             20-Sep-2012 RD: Correctly deallocate arrays
!             14-Jan-2013 SL: Number of fields for STAX002 (13->15)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, t_key, lfnErr, &
                      shortLineLength, lineLength, staNameLength, &
                      fileNameLength, keyValueLength
  USE m_maxdim, ONLY: maxrec
  USE d_inpkey, ONLY: inpkey
  USE d_stacrx, ONLY: t_stacrux,init_stacrux,undef_c,undef_i,undef_e,undef_s
  USE d_phaecc, ONLY: init_buf
  USE p_menaux, ONLY: qt, menuCount
  USE s_ckoptr
  USE f_listc1
  USE s_alcerr
  USE s_ckoptu
  USE s_readcrux
  USE s_ckoptz
  USE s_inquire
  USE s_readinpf
  USE s_timst2
  USE f_tstkey
  USE s_writekey
  USE s_readkeys
  USE s_writcrux
  USE s_exitrc
  USE s_ckoptb
  USE s_gtflna
  USE s_ckopti
  USE s_ckoptl
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)                       :: keyWord     ! what to do

! output:
  TYPE(t_key)                            :: output      ! name = keyWord, if OK

! Local Parameters
! ----------------
  CHARACTER(LEN=8),PARAMETER    :: srName = 'menu_crx'

! Keywords lists to create a new file from coord file
  CHARACTER(LEN=9),DIMENSION(3,6),PARAMETER :: newKeyw = &
  reshape( source= &
  (/'TYPE001  ','TYPE002  ','TYPE005  ','NEW001   ','NEW002   ','NEW005   ',   &
    'NEW001STA','NEW002STA','NEW005STA','NEW001FLG','NEW002FLG','NEW005FLG',   &
    'NEW001BEG','NEW002BEG','NEW005BEG','NEW001END','NEW002END','NEW005END'/), &
    shape = (/3,6/) )

! Local Variables
! ---------------

! Other Variables
  TYPE(t_key),                   &
      DIMENSION(:), ALLOCATABLE :: keys   ! Writing an EDIT panel
  TYPE(t_stacrux)               :: staCrux

  CHARACTER(LEN=keyValueLength) :: crxFil  ! Station information file
  CHARACTER(LEN=keyValueLength) :: edtFil  ! EDIT FIX input panel
  CHARACTER(LEN=keyValueLength) :: crdFil  ! Coodinate file name
  CHARACTER(LEN=keyValueLength) :: datFil  ! Datum file name
  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:), POINTER     :: keyValue
  CHARACTER(LEN=keyValueLength) :: hlpStr
  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:,:),POINTER    :: hlpString
  CHARACTER(LEN=shortLineLength):: title
  CHARACTER(LEN=lineLength)     :: line
  CHARACTER(LEN=19),DIMENSION(2):: epoStr
  CHARACTER(LEN=26)             :: anten
  CHARACTER(LEN=26),DIMENSION(maxrec) :: antlist
  CHARACTER(LEN=staNameLength),  &
      DIMENSION(:), ALLOCATABLE :: stname  ! Station names
  CHARACTER(LEN=fileNameLength) :: filphc  ! Name of phase center file

  INTEGER(i4b)                  :: nstat
  CHARACTER(LEN=3)              :: staFlg
  INTEGER(i4b)                  :: iKey
  INTEGER(i4b)                  :: numKeys
  INTEGER(i4b)                  :: iRenam
  INTEGER(i4b)                  :: iInfo
  INTEGER(i4b)                  :: iProb
  INTEGER(i4b)                  :: iCoovel
  INTEGER(i4b)                  :: iStaType
  INTEGER(i4b)                  :: i1,i2
  INTEGER(i4b)                  :: ii, iSta
  INTEGER(i4b)                  :: irc, ios, iac
  INTEGER(i4b)                  :: irCode,ircSave
  INTEGER(i4b)                  :: dummy
  INTEGER(i4b)                  :: nant

  REAL(r8b), DIMENSION(2)       :: defepo

  LOGICAL                       :: isEmpty
  LOGICAL                       :: yes

! Init pointers and variables
! ---------------------------
  NULLIFY(keyValue)
  NULLIFY(hlpString)
  CALL init_stacrux(stacrux)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'STAX_EDIT' .AND. keyWord /= 'STAX_SAVE' .AND. &
      keyWord /= 'STAX001'   .AND. keyWord /= 'STAX002'   .AND. &
      keyWord /= 'STAX005') RETURN

! STACRX editing
! --------------
  IF (keyWord == 'STAX_EDIT') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_EDIT',    crxFil,irc)
    CALL gtflna(1,'FILE_SKELETON',edtFil,irc)
    CALL gtflna(0,'FILE_CRD',     crdFil,irc)
    IF (LEN_TRIM(crdFil) == 0) THEN
      crdFil = ' '
      datFil = ' '
    ELSE
      CALL gtflna(1,'DATUM',        datFil,irc)
    ENDIF

! Does the File Exist ?
! ---------------------
    CALL INQUIRE(FILE=crxFil , EXIST=yes)
    IF (yes) THEN
      CALL readcrux(crxFil,StaCrux,title)
    ELSE
      title            = ' '
      StaCrux%nRenam   = 0
      StaCrux%nInfo    = 0
      StaCrux%nProb    = 0
      StaCrux%nCooVel  = 0
      StaCrux%nStaType = 0
    ENDIF


    ! Init the Panel Records
    ! ----------------------
    numKeys = 2*5 + 5
    ALLOCATE(keys(numKeys),stat=irc)
    CALL alcerr(irc,'keys',(/numKeys/),srName)


    ! Renaming the stations
    ! ---------------------
    iKey = 1
    keys(iKey)%name = 'TYPE001'
    IF (StaCrux%nRenam > 0) THEN
      ALLOCATE(keys(iKey)%value(StaCrux%nRenam),stat=irc)
      CALL alcerr(irc,'keys(iKey)%value',(/StaCrux%nRenam/),srName)
      keys(iKey)%value=''
      DO iRenam=1,StaCrux%nRenam
        DO ii=1,2
          CALL timst2(2,1,StaCrux%renamsta(iRenam)%timint%t(ii),epostr(ii))
        ENDDO

        WRITE(Keys(iKey)%Value(iRenam),'(A,A,A,A,A3,A,4(A,A,A))')    &
              qt,TRIM(StaCrux%renamsta(iRenam)%stanam), qt//' ',         &
              qt,StaCrux%renamsta(iRenam)%flg,          qt//' ',         &
              (qt,epostr(ii),                           qt//' ', ii=1,2),&
              qt,TRIM(StaCrux%renamsta(iRenam)%oldnam), qt//' ',         &
              qt,TRIM(StaCrux%renamsta(iRenam)%remark), qt
      ENDDO
    ELSE
      ALLOCATE(keys(iKey)%value(1),stat=irc)
      CALL alcerr(irc,'keys(iKey)%value',(/1/),srName)
      keys(iKey)%value(1)=''
      DO ii = 1,6
        keys(iKey)%value(1) = TRIM(keys(iKey)%value(1)) // ' ' // qt//qt
      END DO
    ENDIF

    ! Station information
    ! -------------------
    iKey = 2
    keys(iKey)%name = 'TYPE002'
    IF (StaCrux%nInfo > 0) THEN
      ALLOCATE(keys(iKey)%value(StaCrux%nInfo),stat=irc)
      CALL alcerr(irc,'keys(iKey)%value',(/StaCrux%nInfo/),srName)
      keys(iKey)%value=''
      DO iInfo=1,StaCrux%nInfo
        DO ii=1,2
          CALL timst2(2,1,StaCrux%staInfo(iInfo)%timint%t(ii),epostr(ii))
        ENDDO
        WRITE(Keys(iKey)%Value(iInfo),'(A,A,A,A,A3,A,2(A,A,A))')       &
              qt,TRIM(StaCrux%staInfo(iInfo)%stanam), qt//' ',             &
              qt,StaCrux%staInfo(iInfo)%flg         , qt//' ',             &
              (qt,epostr(ii),                         qt//' ', ii=1,2)

        IF (StaCrux%staInfo(iInfo)%recnam == undef_c) THEN
          line = TRIM(Keys(iKey)%Value(iInfo)) // ' ' // qt//qt
        ELSE
          line = TRIM(Keys(iKey)%Value(iInfo)) // ' ' // &
                 qt // TRIM(StaCrux%staInfo(iInfo)%recnam) // qt
        ENDIF
        Keys(iKey)%Value(iInfo) = line

        IF (StaCrux%staInfo(iInfo)%recser == undef_c) THEN
          line = TRIM(Keys(iKey)%Value(iInfo)) // ' ' // qt//qt
        ELSE
          line = TRIM(Keys(iKey)%Value(iInfo)) // ' ' // &
                 qt // TRIM(StaCrux%staInfo(iInfo)%recser) // qt
        ENDIF
        Keys(iKey)%Value(iInfo) = line

        IF (StaCrux%staInfo(iInfo)%recnum == undef_i) THEN
          line = TRIM(Keys(iKey)%Value(iInfo)) // ' ' // qt//qt
          Keys(iKey)%Value(iInfo) = line
        ELSE
          i1 = LEN_TRIM(Keys(iKey)%Value(iInfo))+2
          i2 = i1+8
          WRITE(Keys(iKey)%Value(iInfo)(i1:i2),'(A,I6,A)') &
                qt,StaCrux%staInfo(iInfo)%recnum,qt
        ENDIF

        IF (StaCrux%staInfo(iInfo)%antnam == undef_c) THEN
          line = TRIM(Keys(iKey)%Value(iInfo)) // ' ' // qt//qt
        ELSE
          line = TRIM(Keys(iKey)%Value(iInfo)) // ' ' // &
                 qt // TRIM(StaCrux%staInfo(iInfo)%antnam) // qt
        ENDIF
        Keys(iKey)%Value(iInfo) = line

        IF (StaCrux%staInfo(iInfo)%antser == undef_c) THEN
          line = TRIM(Keys(iKey)%Value(iInfo)) // ' ' // qt//qt
        ELSE
          line = TRIM(Keys(iKey)%Value(iInfo)) // ' ' // &
                 qt // TRIM(StaCrux%staInfo(iInfo)%antser) // qt
        ENDIF
        Keys(iKey)%Value(iInfo) = line

        IF (StaCrux%staInfo(iInfo)%antnum == undef_i) THEN
          line = TRIM(Keys(iKey)%Value(iInfo)) // ' ' // qt//qt
          Keys(iKey)%Value(iInfo) = line
        ELSE
          i1 = LEN_TRIM(Keys(iKey)%Value(iInfo))+2
          i2 = i1+8
          WRITE(Keys(iKey)%Value(iInfo)(i1:i2),'(A,I6,A)') &
                qt,StaCrux%staInfo(iInfo)%antnum,qt
        ENDIF

        DO ii = 1,3
          IF (StaCrux%staInfo(iInfo)%antecc(ii) == undef_e) THEN
            line = TRIM(Keys(iKey)%Value(iInfo)) // ' ' // qt//qt
            Keys(iKey)%Value(iInfo) = line
          ELSE
            i1 = LEN_TRIM(Keys(iKey)%Value(iInfo))+2
            i2 = i1+10
            WRITE(Keys(iKey)%Value(iInfo)(i1:i2),'(A,F8.4,A)') &
                  qt,StaCrux%staInfo(iInfo)%antecc(ii),qt
          ENDIF
        ENDDO

        line = TRIM(Keys(iKey)%Value(iInfo)) // ' ' // &
               qt//TRIM(StaCrux%staInfo(iInfo)%descri)//qt // ' ' // &
               qt//TRIM(StaCrux%staInfo(iInfo)%remark)//qt
        Keys(iKey)%Value(iInfo) = line

      ENDDO
    ELSE
      ALLOCATE(keys(iKey)%value(1),stat=irc)
      CALL alcerr(irc,'keys(iKey)%value',(/1/),srName)
      keys(iKey)%value(1)=' '
      DO ii = 1,13
        keys(iKey)%value(1) = TRIM(keys(iKey)%value(1)) // ' ' // qt//qt
      END DO
    ENDIF

    ! Station problems
    ! ----------------
    iKey = 3
    keys(iKey)%name = 'TYPE003'
    IF (StaCrux%nProb > 0) THEN
      ALLOCATE(keys(iKey)%value(StaCrux%nProb),stat=irc)
      CALL alcerr(irc,'keys(iKey)%value',(/StaCrux%nProb/),srName)
      keys(iKey)%value=' '
      DO iProb=1,StaCrux%nProb
        DO ii=1,2
          CALL timst2(2,1,StaCrux%staProb(iProb)%timint%t(ii),epostr(ii))
        ENDDO
        WRITE(Keys(iKey)%Value(iProb),'(A,A,A,A,A3,A,3(A,A,A))')   &
              qt,TRIM(StaCrux%staProb(iProb)%stanam), qt//' ',         &
              qt,StaCrux%staProb(iProb)%flg         , qt//' ',         &
              (qt,epostr(ii),                         qt//' ', ii=1,2),&
              qt,TRIM(StaCrux%staProb(iProb)%remark), qt
      ENDDO
    ELSE
      ALLOCATE(keys(iKey)%value(1),stat=irc)
      CALL alcerr(irc,'keys(iKey)%value',(/1/),srName)
      keys(iKey)%value(1)=' '
      DO ii = 1,5
        keys(iKey)%value(1) = TRIM(keys(iKey)%value(1)) // ' ' // qt//qt
      END DO
    ENDIF

    ! Station constraints
    ! -------------------
    iKey = 4
    keys(iKey)%name = 'TYPE004'
    IF (StaCrux%nCoovel > 0) THEN
      ALLOCATE(keys(iKey)%value(StaCrux%nCooVel),stat=irc)
      CALL alcerr(irc,'keys(iKey)%value',(/StaCrux%nCooVel/),srName)
      keys(iKey)%value=''
      DO iCooVel=1,StaCrux%nCooVel
        WRITE(Keys(iKey)%Value(iCoovel),'(2(A,A,A))')             &
             (qt, TRIM(StaCrux%cooVel(iCooVel)%stanam(ii)), qt//' ', ii=1,2)
        DO ii = 1,6
          IF (StaCrux%cooVel(iCooVel)%constr(ii) == undef_s) THEN
            line = TRIM(Keys(iKey)%Value(iCoovel)) // ' ' // qt//qt
            Keys(iKey)%Value(iCoovel) = line
          ELSE
            i1 = LEN_TRIM(Keys(iKey)%Value(iCoovel))+2
            i2 = i1+10
            WRITE(Keys(iKey)%Value(iCoovel)(i1:i2),'(A,F8.5,A)') &
                  qt,StaCrux%coovel(iCoovel)%constr(ii),qt
          ENDIF
        ENDDO

      ENDDO
    ELSE
      ALLOCATE(keys(iKey)%value(1),stat=irc)
      CALL alcerr(irc,'keys(iKey)%value',(/1/),srName)
      keys(iKey)%value(1)=''
      DO ii = 1,8
        keys(iKey)%value(1) = TRIM(keys(iKey)%value(1)) // ' ' // qt//qt
      END DO
    ENDIF

    ! Station Types
    ! -------------
    iKey = 5
    keys(iKey)%name = 'TYPE005'
    IF (StaCrux%nStaType > 0) THEN
      ALLOCATE(keys(iKey)%value(StaCrux%nstaType),stat=irc)
      CALL alcerr(irc,'keys(iKey)%value',(/StaCrux%nstaType/),srName)
      keys(iKey)%value=' '
      DO iStaType=1,StaCrux%nStaType
        DO ii=1,2
          CALL timst2(2,1,StaCrux%staType(iStaType)%timint%t(ii),epostr(ii))
        ENDDO

        WRITE(Keys(iKey)%Value(iStaType),'(A,A,A,A,A3,A,4(A,A,A))')   &
              qt,TRIM(StaCrux%StaType(iStaType)%stanam), qt//' ',         &
              qt,StaCrux%StaType(iStaType)%flg,          qt//' ',         &
              (qt,epostr(ii),                            qt//' ', ii=1,2),&
              qt,TRIM(StaCrux%Statype(iStaType)%markertype), qt//' ',     &
              qt,TRIM(StaCrux%StaType(iStaType)%remark), qt
      ENDDO
    ELSE
      ALLOCATE(keys(iKey)%value(1),stat=irc)
      CALL alcerr(irc,'keys(iKey)%value',(/1/),srName)
      keys(iKey)%value(1)=' '
      DO ii = 1,6
        keys(iKey)%value(1) = TRIM(keys(iKey)%value(1)) // ' ' // qt//qt
      END DO
    ENDIF

! Copy the entries into the "xxxHLP" keywords
! -------------------------------------------
    DO ii = 1,5
      ALLOCATE(keys(iKey+ii)%value(SIZE(keys(ii)%value)),stat=irc)
      CALL alcerr(irc,'keys(iKey+ii)%value',(/SIZE(keys(ii)%value)/),srName)

      keys(iKey+ii)%name  = TRIM(keys(ii)%name) // 'HLP'
      keys(iKey+ii)%value = keys(ii)%value
    ENDDO
    iKey=iKey+5

! Station info file name
! ----------------------
    iKey = iKey + 1
    keys(iKey)%name = "STACRXRS"
    ALLOCATE(keys(iKey)%value(1),stat=irc)
    CALL alcerr(irc,'keys(iKey)%value',(/1/),srName)
    keys(iKey)%value(1) = crxFil

! Title line
! ----------
    iKey = iKey + 1
    keys(iKey)%name = "TITLE"
    ALLOCATE(keys(iKey)%value(1),stat=irc)
    CALL alcerr(irc,'keys(iKey)%value',(/1/),srName)
    keys(iKey)%value(1) = title(1:64)

! Coordinate file
! ---------------
    iKey = iKey + 1
    keys(iKey)%name = 'COORD'
    ALLOCATE(keys(iKey)%value(1),stat=irc)
    CALL alcerr(irc,'keys(iKey)%value',(/1/),srName)
    keys(iKey)%value(1) = crdFil

! Datum file
! ----------
    iKey = iKey + 1
    keys(iKey)%name = 'DATUM'
    ALLOCATE(keys(iKey)%value(1),stat=irc)
    CALL alcerr(irc,'keys(iKey)%value',(/1/),srName)
    keys(iKey)%value(1) = datFil

! Does the File Exist ?
! ---------------------
    iKey = iKey + 1
    keys(iKey)%name = 'NEWFILE'
    ALLOCATE(keys(iKey)%value(1),stat=irc)
    CALL alcerr(irc,'keys(iKey)%value',(/1/),srName)
    CALL INQUIRE(FILE=crxFil , EXIST=yes)

    IF (yes .OR. LEN_TRIM(crdFil) == 0) THEN
      keys(iKey)%value(1) = 'last'
    ELSE
      keys(iKey)%value(1) = menuCount(1)
    ENDIF

! Write all entries into the INP panel
! ------------------------------------
    CALL writeKey(edtFil, keys, 0, irc)

! Deallocate memory
! -----------------
    DO ii = 1, SIZE(keys)
      IF ( ASSOCIATED(keys(ii)%value) ) THEN
        DEALLOCATE(keys(ii)%value, STAT=ios)
      ENDIF
    END DO
    DEALLOCATE(keys, STAT=ios)

    DEALLOCATE(staCrux%renamsta,stat=ios)
    DEALLOCATE(staCrux%staInfo ,stat=ios)
    DEALLOCATE(staCrux%staProb ,stat=ios)
    DEALLOCATE(staCrux%coovel  ,stat=ios)
    DEALLOCATE(staCrux%StaType ,stat=ios)
!
! STACRX store
! ------------
  ELSE IF (keyWord == 'STAX_SAVE') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_OUTPUT', crxFil,irc)
    CALL gtflna(1,'FILE_INPUT',  edtFil,irc)

! Reset the Name of the INP-File
! ------------------------------
    CALL readinpf(edtFil,inpKey)
    CALL readkeys('',keyValue,irc)
    irCode = 0

    ! Title line
    ! ----------
    CALL readKeys('TITLE',keyValue,irc)

    CALL ckoptl(0,'TITLE',keyValue,srName,                      &
                 'Title of this file',irc,irCode,                   &
                 maxVal=1,empty=' ',result1=title)

    ! Renaming the stations
    ! ---------------------
    CALL readKeys('TYPE001',keyValue,irc)

    StaCrux%nRenam = SIZE(keyValue)
    ALLOCATE(StaCrux%renamSta(StaCrux%nRenam), stat=ios)
    CALL alcerr(ios,'StaCrux%renamSta',(/StaCrux%nRenam/),srName)

    ALLOCATE(hlpString(6,StaCrux%nRenam),stat=ios)
    CALL alcerr(ios,'hlpString',(/6,staCrux%nRenam/),srName)

    ircSave = irCode
    CALL ckoptu(1,'TYPE001',keyValue,srName,                            &
                'Renaming of stations',irc,irCode,6,                        &
                maxVal=StaCrux%nRenam,result2=hlpString)
    ircSave = irCode - ircSave

    isEmpty = .TRUE.
    DO ii=1,6
      isEmpty = isEmpty .AND. (LEN_TRIM(hlpString(ii,1)) == 0)
    ENDDO
    IF (isEmpty) THEN
      StaCrux%nRenam = 0
    ELSE

      CALL ckoptl(1,'TYPE001',hlpString(1,:),srName,                      &
                 'Renaming of stations',ircSave,irCode,                       &
                 colTit='Station name',maxVal=StaCrux%nRenam,                 &
                 result2=StaCrux%renamsta(:)%stanam)

      CALL ckoptl(1,'TYPE001',hlpString(2,:),srName,                      &
                 'Renaming of stations',ircSave,irCode,                       &
                 colTit='Flag',maxVal=StaCrux%nRenam,                         &
                 result2=StaCrux%renamsta(:)%flg)

      CALL ckoptz(1,'TYPE001',hlpString(3,:),srName,                      &
                 'Renaming of stations',ircSave,irCode,                       &
                 colTit='start epoch',maxVal=StaCrux%nRenam,                  &
                 empty=44244D0,error=44244D0,                                 &
                 result2=StaCrux%renamsta(:)%timint%t(1))

      CALL ckoptz(1,'TYPE001',hlpString(4,:),srName,                      &
                 'Renaming of stations',ircSave,irCode,                       &
                 colTit='end epoch',maxVal=StaCrux%nRenam,                    &
                 empty=88068D0,error=88068D0,                                 &
                 result2=StaCrux%renamsta(:)%timint%t(2))

      CALL ckoptl(1,'TYPE001',hlpString(5,:),srName,                      &
                 'Renaming of stations',ircSave,irCode,                       &
                 colTit='Old station name',maxVal=StaCrux%nRenam,             &
                 result2=StaCrux%renamsta(:)%oldnam)

      CALL ckoptl(0,'TYPE001',hlpString(6,:),srName,                      &
                 'Renaming of stations',ircSave,irCode,                       &
                 colTit='Remark',maxVal=StaCrux%nRenam,                       &
                 empty=' ',result2=StaCrux%renamsta(:)%remark)
    ENDIF

    DEALLOCATE(hlpString,stat=ios)


    ! Station information
    ! -------------------
    CALL readKeys('TYPE002',keyValue,irc)
!
    StaCrux%nInfo = SIZE(keyValue)
    ALLOCATE(StaCrux%staInfo(StaCrux%nInfo), stat=ios)
    CALL alcerr(ios,'StaCrux%staInfo',(/StaCrux%nInfo/),srName)

    ALLOCATE(hlpString(15,StaCrux%nInfo),stat=ios)
    CALL alcerr(ios,'hlpString',(/15,staCrux%nInfo/),srName)

    ircSave = irCode
    CALL ckoptu(1,'TYPE002',keyValue,srName,                            &
                'Station information',irc,irCode,15,                        &
                maxVal=StaCrux%nInfo,result2=hlpString)
    ircSave = irCode - ircSave

    isEmpty = .TRUE.
    DO ii=1,15
      isEmpty = isEmpty .AND. (LEN_TRIM(hlpString(ii,1)) == 0)
    ENDDO
    IF (isEmpty) THEN
      StaCrux%nInfo = 0
    ELSE

      CALL ckoptl(1,'TYPE002',hlpString(1,:),srName,                      &
                 'Station information',ircSave,irCode,                        &
                 colTit='Station name',maxVal=StaCrux%nInfo,                  &
                 result2=StaCrux%staInfo(:)%stanam)

      CALL ckoptl(1,'TYPE002',hlpString(2,:),srName,                      &
                 'Station information',ircSave,irCode,                        &
                 colTit='Flag',maxVal=StaCrux%nInfo,                          &
                 result2=StaCrux%staInfo(:)%flg)

      CALL ckoptz(1,'TYPE002',hlpString(3,:),srName,                      &
                 'Station information',ircSave,irCode,                        &
                 colTit='start epoch',maxVal=StaCrux%nInfo,                   &
                 empty=44244D0,error=44244D0,                                 &
                 result2=StaCrux%staInfo(:)%timint%t(1))

      CALL ckoptz(1,'TYPE002',hlpString(4,:),srName,                      &
                 'Station information',ircSave,irCode,                        &
                 colTit='end epoch',maxVal=StaCrux%nInfo,                     &
                 empty=88068D0,error=88068D0,                                 &
                 result2=StaCrux%staInfo(:)%timint%t(2))

      CALL ckoptl(1,'TYPE002',hlpString(5,:),srName,                      &
                 'Station information',ircSave,irCode,                        &
                 colTit='Receiver type',maxVal=StaCrux%nInfo,                 &
                 empty=undef_c,result2=StaCrux%staInfo(:)%recnam)

      CALL ckoptl(1,'TYPE002',hlpString(6,:),srName,                      &
                 'Station information',ircSave,irCode,                        &
                 colTit='Receiver serial number',maxVal=StaCrux%nInfo,        &
                 empty=undef_c,result2=StaCrux%staInfo(:)%recser)

      CALL ckopti(1,'TYPE002',hlpString(7,:),srName,                      &
                 'Station information',ircSave,irCode,                        &
                 colTit='Receiver #',maxVal=StaCrux%nInfo,                    &
                 empty=undef_i,result2=StaCrux%staInfo(:)%recnum)

      CALL ckoptl(1,'TYPE002',hlpString(8,:),srName,                      &
                 'Station information',ircSave,irCode,                        &
                 colTit='Antenna type',maxVal=StaCrux%nInfo,                  &
                 empty=undef_c,result2=StaCrux%staInfo(:)%antnam)

      CALL ckoptl(1,'TYPE002',hlpString(9,:),srName,                      &
                 'Station information',ircSave,irCode,                        &
                 colTit='Antenna serial number',maxVal=StaCrux%nInfo,         &
                 empty=undef_c,result2=StaCrux%staInfo(:)%antser)

      CALL ckopti(1,'TYPE002',hlpString(10,:),srName,                     &
                 'Station information',ircSave,irCode,                        &
                 colTit='Antenna #',maxVal=StaCrux%nInfo,                     &
                 empty=undef_i,result2=StaCrux%staInfo(:)%antnum)

      CALL ckoptr(1,'TYPE002',hlpString(11,:),srName,                     &
                 'Station information',ircSave,irCode,                        &
                 colTit='Ant.ecc. - north',maxVal=StaCrux%nInfo,              &
                 empty=undef_e,result2=StaCrux%staInfo(:)%antecc(1))

      CALL ckoptr(1,'TYPE002',hlpString(12,:),srName,                     &
                 'Station information',ircSave,irCode,                        &
                 colTit='Ant.ecc. - east',maxVal=StaCrux%nInfo,               &
                 empty=undef_e,result2=StaCrux%staInfo(:)%antecc(2))

      CALL ckoptr(1,'TYPE002',hlpString(13,:),srName,                     &
                 'Station information',ircSave,irCode,                        &
                 colTit='Ant.ecc. - up',maxVal=StaCrux%nInfo,                 &
                 empty=undef_e,result2=StaCrux%staInfo(:)%antecc(3))

      CALL ckoptl(0,'TYPE002',hlpString(14,:),srName,                     &
                 'Station information',ircSave,irCode,                        &
                 colTit='Description',maxVal=StaCrux%nInfo,                   &
                 empty=' ',result2=StaCrux%staInfo(:)%descri)

      CALL ckoptl(0,'TYPE002',hlpString(15,:),srName,                     &
                 'Station information',ircSave,irCode,                        &
                 colTit='Remark',maxVal=StaCrux%nInfo,                        &
                 empty=' ',result2=StaCrux%staInfo(:)%remark)
    ENDIF

    DEALLOCATE(hlpString,stat=ios)

    ! Station problems
    ! ----------------
    CALL readKeys('TYPE003',keyValue,irc)

    StaCrux%nProb = SIZE(keyValue)
    ALLOCATE(StaCrux%staProb(StaCrux%nProb), stat=ios)
    CALL alcerr(ios,'StaCrux%staProb',(/StaCrux%nProb/),srName)

    ALLOCATE(hlpString(5,StaCrux%nProb),stat=ios)
    CALL alcerr(ios,'hlpString',(/5,staCrux%nProb/),srName)

    ircSave = irCode
    CALL ckoptu(1,'TYPE003',keyValue,srName,                            &
                'Hanndling of station problems',irc,irCode,5,               &
                maxVal=StaCrux%nProb,result2=hlpString)
    ircSave = irCode - ircSave

    isEmpty = .TRUE.
    DO ii=1,5
      isEmpty = isEmpty .AND. (LEN_TRIM(hlpString(ii,1)) == 0)
    ENDDO
    IF (isEmpty) THEN
      StaCrux%nProb = 0
    ELSE

      CALL ckoptl(1,'TYPE003',hlpString(1,:),srName,                      &
                 'Handling of station problems',ircSave,irCode,               &
                 colTit='Station name',maxVal=StaCrux%nProb,                  &
                 result2=StaCrux%staProb(:)%stanam)

      CALL ckoptl(1,'TYPE003',hlpString(2,:),srName,                      &
                 'Handling of station problems',ircSave,irCode,               &
                 colTit='Flag',maxVal=StaCrux%nProb,                          &
                 result2=StaCrux%staProb(:)%flg)

      CALL ckoptz(1,'TYPE003',hlpString(3,:),srName,                      &
                 'Handling of station problems',ircSave,irCode,               &
                 colTit='start epoch',maxVal=StaCrux%nProb,                   &
                 empty=44244D0,error=44244D0,                                 &
                 result2=StaCrux%staProb(:)%timint%t(1))

      CALL ckoptz(1,'TYPE003',hlpString(4,:),srName,                      &
                 'Handling of station problems',ircSave,irCode,               &
                 colTit='end epoch',maxVal=StaCrux%nProb,                     &
                 empty=88068D0,error=88068D0,                                 &
                 result2=StaCrux%staProb(:)%timint%t(2))

      CALL ckoptl(0,'TYPE003',hlpString(5,:),srName,                      &
                 'Handling of station problems',ircSave,irCode,               &
                 colTit='Remark',maxVal=StaCrux%nProb,                        &
                 empty=' ',result2=StaCrux%staProb(:)%remark)
    ENDIF

    DEALLOCATE(hlpString,stat=ios)

    ! Station constraints
    ! -------------------
    CALL readKeys('TYPE004',keyValue,irc)

    StaCrux%nCoovel = SIZE(keyValue)
    ALLOCATE(StaCrux%coovel(StaCrux%ncoovel), stat=ios)
    CALL alcerr(ios,'StaCrux%coovel',(/StaCrux%nCoovel/),srName)

    ALLOCATE(hlpString(8,StaCrux%nCoovel),stat=ios)
    CALL alcerr(ios,'hlpString',(/8,staCrux%nCoovel/),srName)

    ircSave = irCode
    CALL ckoptu(1,'TYPE004',keyValue,srName,                            &
                'Station coordinates and velocities',irc,irCode,8,          &
                maxVal=StaCrux%nCoovel,result2=hlpString)
    ircSave = irCode - ircSave

    isEmpty = .TRUE.
    DO ii=1,8
      isEmpty = isEmpty .AND. (LEN_TRIM(hlpString(ii,1)) == 0)
    ENDDO
    IF (isEmpty) THEN
      StaCrux%nCoovel = 0
    ELSE

      CALL ckoptl(1,'TYPE004',hlpString(1,:),srName,                      &
                 'Station oordinates and velocities',ircSave,irCode,          &
                 colTit='Station name 1',maxVal=StaCrux%nCoovel,              &
                 result2=StaCrux%coovel(:)%stanam(1))

      CALL ckoptl(1,'TYPE004',hlpString(2,:),srName,                      &
                 'Station oordinates and velocities',ircSave,irCode,          &
                 colTit='Station name 2',maxVal=StaCrux%nCoovel,              &
                 result2=StaCrux%coovel(:)%stanam(2))


      CALL ckoptr(1,'TYPE004',hlpString(3,:),srName,                      &
                 'Station oordinates and velocities',ircSave,irCode,          &
                 colTit='pos. - north',maxVal=StaCrux%nCoovel,                &
                 empty=undef_s,error=undef_s,                                 &
                 result2=StaCrux%coovel(:)%constr(1))

      CALL ckoptr(1,'TYPE004',hlpString(4,:),srName,                      &
                 'Station oordinates and velocities',ircSave,irCode,          &
                 colTit='pos. - east',maxVal=StaCrux%nCoovel,                 &
                 empty=undef_s,error=undef_s,                                 &
                 result2=StaCrux%coovel(:)%constr(2))

      CALL ckoptr(1,'TYPE004',hlpString(5,:),srName,                      &
                 'Station oordinates and velocities',ircSave,irCode,          &
                 colTit='pos. - up',maxVal=StaCrux%nCoovel,                   &
                 empty=undef_s,error=undef_s,                                 &
                 result2=StaCrux%coovel(:)%constr(3))

      CALL ckoptr(1,'TYPE004',hlpString(6,:),srName,                      &
                 'Station oordinates and velocities',ircSave,irCode,          &
                 colTit='vel. - north',maxVal=StaCrux%nCoovel,                &
                 empty=undef_s,error=undef_s,                                 &
                 result2=StaCrux%coovel(:)%constr(4))

      CALL ckoptr(1,'TYPE004',hlpString(7,:),srName,                      &
                 'Station oordinates and velocities',ircSave,irCode,          &
                 colTit='vel. - east',maxVal=StaCrux%nCoovel,                 &
                 empty=undef_s,error=undef_s,                                 &
                 result2=StaCrux%coovel(:)%constr(5))

      CALL ckoptr(1,'TYPE004',hlpString(8,:),srName,                      &
                 'Station oordinates and velocities',ircSave,irCode,          &
                 colTit='vel. - up',maxVal=StaCrux%nCoovel,                   &
                 empty=undef_s,error=undef_s,                                 &
                 result2=StaCrux%coovel(:)%constr(6))
    ENDIF

    DEALLOCATE(hlpString,stat=ios)


    ! Station Type
    ! ------------
    CALL readKeys('TYPE005',keyValue,irc)

    StaCrux%nStaType = SIZE(keyValue)

    ALLOCATE(StaCrux%staType(StaCrux%nStaType), stat=ios)
    CALL alcerr(ios,'StaCrux%StaType',(/StaCrux%nStaType/),srName)

    ALLOCATE(hlpString(6,StaCrux%nStaType),stat=ios)
    CALL alcerr(ios,'hlpString',(/6,staCrux%nStaType/),srName)

    ircSave = irCode
    CALL ckoptu(1,'TYPE005',keyValue,srName,                            &
                'Handling of station problems',irc,irCode,6,                &
                maxVal=StaCrux%nstaType,result2=hlpString)
    ircSave = irCode - ircSave

    isEmpty = .TRUE.
    DO ii=1,6
      isEmpty = isEmpty .AND. (LEN_TRIM(hlpString(ii,1)) == 0)
    ENDDO
    IF (isEmpty) THEN
      StaCrux%nStaType = 0
    ELSE

      CALL ckoptl(1,'TYPE005',hlpString(1,:),srName,                      &
                 'Marker Type',ircSave,irCode,                                &
                 colTit='Station name',maxVal=StaCrux%nStaType,               &
                 result2=StaCrux%StaType(:)%stanam)

      CALL ckoptl(1,'TYPE005',hlpString(2,:),srName,                      &
                 'Marker Type',ircSave,irCode,                                &
                 colTit='Flag',maxVal=StaCrux%nStaType,                       &
                 result2=StaCrux%StaType(:)%flg)

      CALL ckoptz(1,'TYPE005',hlpString(3,:),srName,                      &
                 'Marker Type',ircSave,irCode,                                &
                 colTit='start epoch',maxVal=StaCrux%nStaType,                &
                 empty=44244D0,error=44244D0,                                 &
                 result2=StaCrux%StaType(:)%timint%t(1))

      CALL ckoptz(1,'TYPE005',hlpString(4,:),srName,                      &
                 'Marker Type',ircSave,irCode,                                &
                 colTit='end epoch',maxVal=StaCrux%nStaType,                  &
                 empty=88068D0,error=88068D0,                                 &
                 result2=StaCrux%StaType(:)%timint%t(2))

      CALL ckoptl(1,'TYPE005',hlpString(5,:),srName,                      &
                 'Marker Type',ircSave,irCode,                                &
                 colTit='Marker type',maxVal=StaCrux%nStaType,                &
                 empty=' ',result2=StaCrux%StaType(:)%markerType)

      CALL ckoptl(0,'TYPE005',hlpString(6,:),srName,                      &
                 'Marker Type',ircSave,irCode,                                &
                 colTit='Remark',maxVal=StaCrux%nStaType,                     &
                 empty=' ',result2=StaCrux%StaType(:)%remark)
    ENDIF

    DEALLOCATE(hlpString,stat=ios)

! An error occured while decoding
! -------------------------------
    IF (irCode /= 0) CALL exitrc(2)

! Check receiver antenna pairs
! ----------------------------
    CALL gtflna(0,'PHASECC',filphc,irc)
    IF (irc == 0 .AND. LEN_TRIM(filphc) > 0) THEN
      nant=0
      antlist = ''
      DO iInfo = 1, staCrux%nInfo

!        IF (staCrux%staInfo(iInfo)%recnam == undef_c) CYCLE
        IF (staCrux%staInfo(iInfo)%antnam == undef_c) CYCLE
        IF (staCrux%staInfo(iInfo)%antnum == undef_i) CYCLE

        WRITE(anten,"(A20,I6)")staCrux%staInfo(iInfo)%antnam, &
                         staCrux%staInfo(iInfo)%antnum
        dummy = listc1(1,26,maxrec,antlist,anten,nant)
      ENDDO

      CALL init_buf(antlist=antlist)

    ENDIF

! Check station/velocity constraints
! ----------------------------------
    DO iCoovel=1,staCrux%nCoovel
      i1 = 0
      i2 = 0
      DO ii = 1,6
        IF (ii <= 3 .AND. staCrux%coovel(iCoovel)%constr(ii) == undef_s) &
            i1 = i1+1
        IF (ii >= 4 .AND. staCrux%coovel(iCoovel)%constr(ii) == undef_s) &
            i2 = i2+1
      ENDDO
      IF ((i1 /= 0 .AND. i1 /= 3) .OR. (i2 /= 0 .AND. i2 /= 3)) THEN
        WRITE(lfnerr,'(/,A,2(/,16X,A,A))')                                     &
              ' ### SR MENU_CRX: Invalid station pair constraining.',          &
                   'Station name 1: ',TRIM(staCrux%coovel(iCoovel)%stanam(1)), &
                   'Station name 2: ',TRIM(staCrux%coovel(iCoovel)%stanam(2))
        IF (i1 /= 0 .AND. i1 /= 3) &
          WRITE(lfnerr,'(16X,A)') 'Constrain all three position components.'
        IF (i2 /= 0 .AND. i2 /= 3) &
          WRITE(lfnerr,'(16X,A)') 'Constrain all three velocity components.'
        WRITE(lfnerr,*)
        irCode = irCode + 1
      ENDIF
    ENDDO

    IF (irCode /= 0) CALL exitrc(2)

    CALL writCrux(crxFil, StaCrux, title)

    DEALLOCATE(staCrux%renamsta,stat=ios)
    DEALLOCATE(staCrux%staInfo ,stat=ios)
    DEALLOCATE(staCrux%staProb ,stat=ios)
    DEALLOCATE(staCrux%coovel  ,stat=ios)
    DEALLOCATE(staCrux%staType ,stat=ios)

! Generate default unilines for a new file
! ----------------------------------------
  ELSE IF (keyWord == 'STAX001' .OR. keyWord == 'STAX002' .OR. &
           keyWord == 'STAX005') THEN

    ALLOCATE(output%value(1),stat=iac)
    CALL alcerr(iac,'output%value',(/1/),srName)
    output%value(1) = ' '

    ! Which type is selected
    SELECT CASE (keyWord)
      CASE('STAX001')
        iKey = 1
        WRITE(output%value(1),'(6(A,1X))') (qt//qt, ii=1,6)
      CASE('STAX002')
        iKey = 2
        WRITE(output%value(1),'(15(A,1X))') (qt//qt, ii=1,15)
      CASE('STAX005')
        iKey = 3
        WRITE(output%value(1),'(6(A,1X))') (qt//qt, ii=1,6)
      CASE DEFAULT
        RETURN
    END SELECT

    ! Is a coordinate file avail. (new file cond.)
    CALL gtflna(0,'COORD',crdFil,irc)

    nStat  = 0
    irCode = irc
    yes = .FALSE.

    IF (irCode == 0) THEN

      ! Is a default list given?
      CALL ckoptb(1,(/newKeyw(iKey,2)/),srName,            &
                  'Use default station list',irCode, &
                  resultL=yes)

      ! List of default station names
      IF (yes) THEN
        CALL readKeys(newKeyw(iKey,3),keyValue,irc)

        nStat = SIZE(keyValue)
        ALLOCATE(stName(nStat),stat=iac)
        CALL alcerr(iac,'stName',(/nStat/),srName)

        CALL ckoptl(1,newKeyw(iKey,3),keyValue,srName,   &
                    'Default station list',irc,irCode, &
                    empty=' ',maxVal=nStat,result2=stName)
        IF (LEN_TRIM(stName(1)) == 0) yes = .FALSE.
      ENDIF

      IF (.NOT. yes) RETURN

      ! Default flag for all selected stations
      CALL readKeys(newKeyw(iKey,4),keyValue,irc)

      CALL ckoptl(1,newKeyw(iKey,4),keyValue,srName,   &
                  'Default flag for station list',irc,irCode, &
                  maxVal=1,empty='-1',maxLength=3,result1=staFlg)

      ! Default from epoch for all selected stations
      CALL readKeys(newKeyw(iKey,5),keyValue,irc)

      CALL ckoptz(1,newKeyw(iKey,5),keyValue,srName,   &
                  'Default start epoch for station list',irc,irCode, &
                  maxVal=1,empty=0D0,result1=defepo(1))

      ! Default to epoch for all selected stations
      CALL readKeys(newKeyw(iKey,6),keyValue,irc)

      CALL ckoptz(1,newKeyw(iKey,6),keyValue,srName,   &
                  'Default end epoch for station list',irc,irCode, &
                  maxVal=1,empty=1d20,result1=defepo(2))

      IF (irCode == 0) THEN
        hlpStr = ' '
        WRITE(hlpStr,'(A,A3,A,2(1X,A,19X,A))') qt,staFlg,qt,qt,qt,qt,qt
        IF (staFlg == '-1') hlpStr(2:4) = ' '
        CALL timst2(2,1,defepo(1),hlpStr(8:26))
        CALL timst2(2,1,defepo(2),hlpStr(30:48))
      ENDIF
    ENDIF  ! Coordinate file found...

! Read the old entries
! --------------------
    IF (tstKey(newKeyw(iKey,1))) THEN

      CALL readKeys(newKeyw(iKey,1),keyValue,irc)

      IF (irc /= 0) CALL exitrc(2)

    ELSE

      DEALLOCATE(keyValue,stat=iac)

      ALLOCATE(keyValue(0),stat=iac)
      CALL alcerr(iac,'keyValue',(/0/),srName)

    ENDIF


    IF (yes .AND. irCode == 0) THEN

      ! Allocate the output array
      DEALLOCATE(output%value,stat=iac)
      ALLOCATE(output%value(nStat),stat=iac)
      CALL alcerr(iac,'output%value',(/nStat/),srName)

      DO iSta = 1,nStat

        output%value(iSta) = ' '

        DO ii=1,SIZE(keyValue)
          IF (INDEX(keyValue(ii),qt//TRIM(stName(iSta))//qt) == 1) THEN
            output%value(iSta) = keyValue(ii)
            EXIT
          ENDIF
        ENDDO

        IF (LEN_TRIM(output%value(iSta)) > 0) CYCLE

        IF (keyWord == 'STAX002') THEN
          WRITE(output%value(iSta),'(A,1X,A,11(1X,A))') &
            qt // TRIM(stName(iSta)) // qt, TRIM(hlpStr), (qt//qt, ii=1,11)
        ELSE
          WRITE(output%value(iSta),'(A,1X,A,2(1X,A))') &
            qt // TRIM(stName(iSta)) // qt, TRIM(hlpStr), (qt//qt, ii=1,2)
        ENDIF

      ENDDO

! Take the old entries as result:
! -------------------------------
    ELSE

      ! Allocate the output array
      DEALLOCATE(output%value,stat=iac)

      ALLOCATE(output%value(SIZE(keyValue)),stat=iac)
      CALL alcerr(iac,'output%value',(/SIZE(keyValue)/),srName)

      output%value=keyValue
    ENDIF

    IF (ALLOCATED(stName)) DEALLOCATE(stName)
  ENDIF

  RETURN
END SUBROUTINE menu_crx

END MODULE

