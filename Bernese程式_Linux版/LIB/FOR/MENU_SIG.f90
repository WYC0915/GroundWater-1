MODULE s_MENU_SIG
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_sig(keyWord, menuauxinp, output)

! -------------------------------------------------------------------------
! Purpose:    Is used for creating, editing, and storing of Bernese
!             station sigma files using the menu program
!             (called by the menu program via MENUAUX)
!
! Author:     R. Dach
!
! Created:    24-Aug-2001
!
! Changes:    17-Sep-2001 RD: crd-file variables allocatable
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             03-May-2002 RD: Use keyword structure in MENUAUX
!             25-Sep-2002 HU: Remove i_astlib
!             13-Dec-2002 RD: New quotes-handling in SR writekey
!             08-Jan-2003 RD: Adapt to the actual MENUAUX calling
!             19-Feb-2003 RD: Stop with error (new logic in menu)
!             23-Apr-2003 AJ: Nullify local pointers
!             16-May-2003 MM: Initialize and deallocate structure
!             18-Oct-2003 HU: Write date/time to title
!             06-Nov-2003 HB: Check if pointer (keys(ii)%value) is
!                             ASSOCIATED before DEALLOCATE
!             19-Nov-2003 RD: Reread INP-file using READINPF
!             17-Feb-2011 RD: Remove MAXSTA-COMMON (unused)
!             25-Aug-2011 UM: 6 sigma files introduced
!             20-Sep-2012 RD: Use M_BERN with ONLY
!             20-Sep-2012 RD: Correctly deallocate arrays
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, t_key, &
                      keyNameLength, keyValueLength, &
                      fileNameLength, staNameLength
  USE d_inpkey, ONLY: inpkey
  USE d_stalst, ONLY: t_staList, init_staList
  USE p_menaux, ONLY: qt,menuCount
  USE s_ckoptr
  USE s_alcerr
  USE s_ckoptu
  USE s_inquire
  USE s_readinpf
  USE s_dattim
  USE s_writstsg
  USE s_writekey
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_gtflna
  USE s_readstsg
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
  CHARACTER(LEN=8),PARAMETER    :: srName = 'menu_sig'

! Local Variables
! ---------------
  TYPE(t_staList)               :: staList ! Station names from READSTSG

  TYPE(t_key),                   &
      DIMENSION(:), ALLOCATABLE :: keys    ! Writing an EDIT panel

  CHARACTER(LEN=keyValueLength) :: sigFil  ! Sigma file name
  CHARACTER(LEN=keyValueLength) :: edtFil  ! EDIT ABB input panel
  CHARACTER(LEN=keyValueLength) :: crdFil  ! Coodinate file name
  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:), POINTER     :: keyValue

  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:,:),ALLOCATABLE:: hlpStr

  CHARACTER(LEN=keyNameLength),      &
                   DIMENSION(6) :: sigKey
  CHARACTER(LEN=9)              :: date
  CHARACTER(LEN=5)              :: time

  INTEGER(i4b)                  :: nstat   ! Number of stations from GETCOO
  INTEGER(i4b)                  :: nSigma  ! # of sigmas per station (1-4 or 6)
  INTEGER(i4b)                  :: numKeys ! Writing a SIGEDIT panel
  INTEGER(i4b)                  :: iSta, iSig
  INTEGER(i4b)                  :: ii, i1,i2
  INTEGER(i4b)                  :: irCode
  INTEGER(i4b)                  :: ios, irc, iac

  REAL(r8b),DIMENSION(6)        :: sigma

  LOGICAL                       :: edit, yes


! Init pointers and variables
! ---------------------------
  NULLIFY(keyValue)
  CALL init_staList(staList)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'SIG_EDIT' .AND. keyWord /= 'SIG_SAVE'  .AND. &
      keyWord /= 'SIG_LIST1'.AND. keyWord /= 'SIG_LIST2' .AND. &
      keyWord /= 'SIG_LIST3'.AND. keyWord /= 'SIG_LIST4' .AND. &
      keyWord /= 'SIG_LIST6') RETURN

! Editing/Create a CRD file
! -------------------------
  IF (keyWord == 'SIG_EDIT') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_EDIT',    sigFil,irc)
    CALL gtflna(1,'FILE_SKELETON',edtFil,irc)
    CALL gtflna(0,'FILE_CRD' ,crdFil,irc)

! Does the Station Sigma File Exist ?
! -----------------------------------
    CALL INQUIRE(FILE=sigFil , EXIST=yes)

    IF (yes) THEN
      numKeys = 22
    ELSE
      numKeys = 16
    ENDIF

! Init the new key record
! -----------------------
    ALLOCATE(keys(numKeys), STAT=iac)
    CALL alcerr(iac, 'keys', (/numKeys/), srName)

    DO ii = 1, numKeys
      ALLOCATE( keys(ii)%value(1), STAT=iac )
      CALL alcerr(iac, 'keys(ii)%value', (/1/), srName)
    END DO

    keys( 1)%name = 'SIGMARS '

    keys( 2)%name = 'FILNAM1 '
    keys( 3)%name = 'FILNAM2 '
    keys( 4)%name = 'FILNAM3 '
    keys( 5)%name = 'FILNAM4 '
    keys( 6)%name = 'FILNAM5 '
    keys( 7)%name = 'FILNAM6 '

    keys( 8)%name = 'COORD'

    keys( 9)%name = 'NEWFILE'

    keys(10)%name = 'TITLE1'
    keys(11)%name = 'TITLE2'
    keys(12)%name = 'TITLE3'
    keys(13)%name = 'TITLE4'
    keys(14)%name = 'TITLE5'
    keys(15)%name = 'TITLE6'

    keys(16)%name = 'STASEL'

! Init Entries for a station selection file panel
! -----------------------------------------------
    keys( 1)%value(1) = sigFil
    keys( 2)%value(1) = sigFil
    keys( 3)%value(1) = sigFil
    keys( 4)%value(1) = sigFil
    keys( 5)%value(1) = sigFil
    keys( 6)%value(1) = sigFil
    keys( 7)%value(1) = sigFil

    keys( 8)%value(1) = ''

    keys( 9)%value(1) = menuCount(1)

    keys(10)%value(1) = ''
    keys(11)%value(1) = ''
    keys(12)%value(1) = ''
    keys(13)%value(1) = ''
    keys(14)%value(1) = ''
    keys(15)%value(1) = ''

    keys(16)%value(1) = ''

! Does the Station Sigma File Exist ?
! -----------------------------------
    CALL INQUIRE(FILE=sigFil , EXIST=yes)

    IF (yes) THEN

      keys( 9)%value(1) = 'last'

      nSigma = -1
      CALL readstSG(sigFil,nSigma,staList)

      keys(nSigma+9)%value(1) = staList%title(1:64)

      keys(17)%name = 'RADIO_1'
      keys(18)%name = 'RADIO_2'
      keys(19)%name = 'RADIO_3'
      keys(20)%name = 'RADIO_4'
      keys(21)%name = 'RADIO_5'
      keys(22)%name = 'RADIO_6'

      keys(17)%value(1) = '0'
      keys(18)%value(1) = '0'
      keys(19)%value(1) = '0'
      keys(20)%value(1) = '0'
      keys(21)%value(1) = '0'
      keys(22)%value(1) = '0'
      keys(nSigma+16)%value(1) = '1'

! A station selection file was selected
! -------------------------------------
      IF (nSigma == 0) THEN
        WRITE(lfnerr,'(/,A,/,18X,A,/,18X,A,A,/)')                         &
        ' *** SR MENU_SIG: You have selected a station selection file.',  &
                          'A station sigma file is expected here!',       &
                          'File name:  ',TRIM(sigFil)
        CALL exitrc(2)
      ENDIF

! An unexpected number of columns was found
! -----------------------------------------
      IF (nSigma /= 1 .AND. nSigma /= 2 .AND. &
          nSigma /= 3 .AND. nSigma /= 4 .AND. &
          nSigma /= 6) THEN
        WRITE(lfnerr,'(/,A,/,18X,A,/,18X,A,A,/,5(/,18X,A),/)')            &
        ' *** SR MENU_SIG: Too many columns with sigmas found in the',    &
                          'station sigma file! Check the file format!',   &
                          'File name:  ',TRIM(sigFil),                &
                          '# of columns == 1: coord/veloc. for ADDNEQ2',  &
                          '# of columns == 2: vertical for tropos.',      &
                          '# of columns == 3: coord/veloc. for GPSEST',   &
                          '# of columns == 4: vertical/gradient for tropo.', &
                          '# of columns == 6: vertical/N-,E-gradients for tropo.'
        CALL exitrc(2)
      ENDIF

    ELSE

! Generate a new file from a coordinate file
! ------------------------------------------
      IF (LEN_TRIM(crdFil) > 0) keys(8)%value(1) = crdFil

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
  ELSE IF (keyWord == 'SIG_LIST1' .OR. keyWord == 'SIG_LIST2' .OR. &
           keyWord == 'SIG_LIST3' .OR. keyWord == 'SIG_LIST4' .OR. &
           keyWord == 'SIG_LIST6') THEN

! Generate at least an empty uniline
! ----------------------------------
    DEALLOCATE(output%value,stat=iac)

    ALLOCATE(output%value(1),stat=iac)
    CALL alcerr(iac,'output%value',(/1/),srName)

    output%value(1) = ' '

! Decide "crd/vel" or "trp" sigma file
! ------------------------------------
    IF (keyWord == 'SIG_LIST1') THEN
      nSigma = 1
      WRITE(output%value(1),'(2(A,1X))') (qt // qt, ii=1,2)

    ELSE IF (keyWord == 'SIG_LIST2') THEN
      nSigma = 2
      WRITE(output%value(1),'(3(A,1X))') (qt // qt, ii=1,3)

    ELSE IF (keyWord == 'SIG_LIST3') THEN
      nSigma = 3
      WRITE(output%value(1),'(4(A,1X))') (qt // qt, ii=1,4)

    ELSE IF (keyWord == 'SIG_LIST4') THEN
      nSigma = 4
      WRITE(output%value(1),'(5(A,1X))') (qt // qt, ii=1,5)

    ELSE IF (keyWord == 'SIG_LIST6') THEN
      nSigma = 6
      WRITE(output%value(1),'(7(A,1X))') (qt // qt, ii=1,7)

    ENDIF

    CALL writekey(menuauxinp,(/output/),1,irc)

! Get the name of the sigma file
! ------------------------------
    CALL gtflna(1,'SIGMARS',sigFil,irc)
    IF (irc /= 0) RETURN

! Does the sigma file allready exist?
! -----------------------------------
    CALL INQUIRE(FILE=sigFil , EXIST=edit)

! Read an existing sigma file
! ---------------------------
    IF (edit) THEN

      CALL readstSG(sigFil,nSigma,staList)

! Create a new sigma file: Is the 2nd entry an existing crd-file?
! ---------------------------------------------------------------
    ELSE
      irCode = 0
      CALL readKeys('STASEL',keyValue,irc)

      staList%nSta = SIZE(keyValue)

      ALLOCATE(staList%staNam(staList%nSta),stat=iac)
      CALL alcerr(iac,'staList',(/staList%nSta/),srName)

      CALL ckoptl(1,'STASEL',keyValue,srName,                  &
                  'Station list for default sigma',irc,irCode, &
                  empty=' ',maxVal=staList%nSta,               &
                  result2=staList%staNam)

! Create a new (empty) sigma file
! -------------------------------
      IF (irCode /= 0) THEN
        staList%nSta = 0
      ELSE

! Read the default sigmas
! -----------------------
        SELECT CASE(keyWord)
          CASE('SIG_LIST1')
            nSigma = 1
            sigKey(1:nSigma) = (/ 'SIGMA_0' /)
          CASE('SIG_LIST2')
            nSigma = 2
            sigKey(1:nSigma) = (/ 'SIGMA_TA','SIGMA_TR' /)
          CASE('SIG_LIST3')
            nSigma = 3
            sigKey(1:nSigma) = (/ 'SIGMA_N','SIGMA_E','SIGMA_U' /)
          CASE('SIG_LIST4')
            nSigma = 4
            sigKey(1:nSigma) = (/ 'SIGMA_VA','SIGMA_VR','SIGMA_GA','SIGMA_GR' /)
          CASE('SIG_LIST6')
            nSigma = 6
            sigKey(1:nSigma) = (/ &
             'SIGMA_AV','SIGMA_RV','SIGMA_AN','SIGMA_RN','SIGMA_AE','SIGMA_RE' /)
        END SELECT

        DO iSig=1,nSigma
          CALL readKeys(sigKey(iSig),keyValue,irc)

          CALL ckoptr(1,sigKey(iSig),keyValue,srName, &
                      'Default sigma',irc,irCode,     &
                      empty=0d0,ge=0d0,error=0d0,maxVal=1,&
                      result1=sigma(iSig))
        ENDDO

! Generate the corresponding sigma list
! -------------------------------------
        ALLOCATE(staList%sigma(nSigma,staList%nSta),stat=iac)
        CALL alcerr(iac,'staList%sigma',(/nSigma,staList%nSta/),srName)

        DO iSta = 1,staList%nSta
          DO iSig = 1,nSigma
            staList%sigma(iSig,iSta) = sigma(iSig)
          ENDDO
        ENDDO

      ENDIF ! No stations selected

    ENDIF ! edit an existing sigma file or create a new one

! Generate the uniline for editing
! --------------------------------
    nStat = staList%nSta
    IF (nStat == 0) nStat = 1

    ALLOCATE(output%value(nStat),stat=iac)
    CALL alcerr(iac,'output%value',(/nStat/),srName)

    output%value = ' '

    IF (staList%nSta == 0) THEN

! An empty station list:
! ----------------------
      DO ii = 1,nSigma+1
        i1 = (ii-1)*3 + 1
        i2 = i1+1
        WRITE(output%value(1)(i1:i2),'(A)') qt//qt
      ENDDO

! Edit the entries from a sigma file
! ----------------------------------
    ELSE
      DO iSta = 1, staList%nSta
        output%value(iSta) = qt // TRIM(staList%staNam(iSta)) // qt
        DO ii = 1,nSigma
          i1 = LEN_TRIM(output%value(iSta)) + 2
          i2 = i1 + 9
          IF (staList%sigma(ii,iSta) < 1D2) THEN
            WRITE(output%value(iSta)(i1:i2),'(A,F7.4,A)')  &
                  qt, staList%sigma(ii,iSta), qt
          ELSE
            WRITE(output%value(iSta)(i1:i2),'(A,E7.2,A)')  &
                  qt, staList%sigma(ii,iSta), qt
          ENDIF
        ENDDO
      ENDDO
    ENDIF

! Store a station selection file
! ------------------------------
  ELSE IF (keyWord == 'SIG_SAVE') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_OUTPUT', sigFil,irc)
    CALL gtflna(1,'FILE_INPUT',  edtFil,irc)

! Reset the Name of the INP-File
! ------------------------------
    CALL readinpf(edtFil,inpKey)
    CALL readkeys('', keyValue, irc)

! Which type has to be stored?
! ----------------------------
    irCode = 0

    CALL ckoptb(1, &
          (/'RADIO_1','RADIO_2','RADIO_3','RADIO_4','RADIO_5','RADIO_6'/), &
          srName,'File type selection',irCode, result1=nSigma)

    IF (irCode /= 0) CALL exitrc(2)

! Allocate the memory for the key list
! ------------------------------------
    ALLOCATE(keys(2),stat=irc)
    CALL alcerr(irc,'keys',(/2/),srName)

! Put the keyword into the list
! -----------------------------
    keys(1)%name = 'TITLE'
    keys(2)%name = 'STALIST'

    WRITE(keys(1)%name(6:6),'(I1)') nSigma
    WRITE(keys(2)%name(8:8),'(I1)') nSigma

! Get the title line
! ------------------
    CALL readkeys(keys(1)%name, keyValue, irc)

    CALL ckoptl(0,keys(1)%name, keyValue,srName, &
                'Sigma file title line',irc,irCode, &
                empty=' ',maxVal=1,result1=staList%title)

! Get the list of entries
! -----------------------
    CALL readkeys(keys(2)%name, keyValue, irc)
    irCode = irCode + irc

    IF (irc == 0) THEN

! Allocate the corresponding memory
! ---------------------------------
      IF (ASSOCIATED(staList%stanam)) DEALLOCATE(staList%stanam,stat=ios)
      IF (ASSOCIATED(staList%sigma))  DEALLOCATE(staList%sigma, stat=ios)

      staList%nSta = SIZE(keyValue)
      ALLOCATE(staList%staNam(staList%nSta),stat=ios)
      CALL alcerr(ios,'staList%staNam',(/staList%nSta/),srName)

      ALLOCATE(staList%sigma(nSigma,staList%nSta),stat=ios)
      CALL alcerr(ios,'staList%sigma',(/nSigma,staList%nSta/),srName)

      ALLOCATE(hlpStr(nSigma+1,staList%nSta),stat=ios)
      CALL alcerr(ios,'hlpStr',(/nSigma+1,staList%nSta/),srName)

! Extract the station sigma record
! --------------------------------
      CALL ckoptu(1,keys(2)%name,keyValue,srName,      &
                  'Station sigma file',irc,irCode,     &
                  numCol=nSigma+1,maxVal=staList%nSta, &
                  result2=hlpStr)

    ELSE
      ALLOCATE(hlpStr(nSigma+1,1),stat=ios)
      CALL alcerr(ios,'hlpStr',(/nSigma+1,1/),srName)
    ENDIF

    IF (irCode == 0) THEN
      CALL ckoptl(1,keys(2)%name,hlpStr(1,:),srName,   &
                  'Station sigma file',irc,irCode,     &
                  colTit='Station names',maxVal=staList%nSta, &
                  empty=' ',result2=staList%staNam)

      DO iSig=1,nSigma
        CALL ckoptr(1,keys(2)%name,hlpStr(1+iSig,:),srName,     &
                    'Station sigma file',irc,irCode,            &
                    colTit='Sigma values',maxVal=staList%nSta, &
                    empty=0d0,ge=0d0,result2=staList%sigma(iSig,:))
      ENDDO
    ENDIF

    DEALLOCATE(hlpStr,stat=ios)
    IF  (irCode /= 0) CALL exitrc(2)

! Date and time in title line
! ---------------------------
    CALL dattim(date,time)
    staList%title(65:80) = ' '//date//' '//time

! Write the resulting file (or give an error)
! -------------------------------------------
    IF (LEN_TRIM(staList%staNam(1)) == 0) staList%nSta = 0
    CALL writstsg(sigFil,nSigma,staList)

! Deallocate keys-record
! ----------------------
    DEALLOCATE(keys, STAT=iac)

  ENDIF ! which action

  DEALLOCATE(keyValue,stat=iac)
  DEALLOCATE(staList%stanam,stat=ios)
  DEALLOCATE(staList%sigma, stat=ios)

  RETURN
END SUBROUTINE menu_sig

END MODULE
