MODULE s_MENU_RAP
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_rap(keyWord, menuauxinp, output)

! -------------------------------------------------------------------------
! Purpose:    Set the default list for receiver antenna PCV estimation
!             in GPSEST (called by the menu program via MENUAUX)
!
! Author:     R. Dach
!
! Created:    20-Jun-2001
! Last mod.:  21-Jul-2008
!
! Changes:    05-Sep-2001 HU: Interface for rdhead2 added
!             19-Apr-2002 RD: Use keywords from MENUAUX.INP
!             16-May-2002 RD: Recover old values in the case of an error
!             13-Dec-2002 RD: New quotes-handling in SR writekey
!             23-Apr-2003 AJ: Nullify local pointers
!             08-Sep-2003 HU: antnam, recnam chr16 -> chr20
!             10-Mar-2008 RD: Consider GNSS selection
!             21-Jul-2008 PS: Format corrected
!
! SR called:  alcerr, readKeys, writekey, tstkey, menu_ant,
!             ckoptu, ckoptl, ckopti, ckoptr
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_global, ONLY: g_strsys3
  USE p_menaux, ONLY: qt
  USE s_ckoptr
  USE s_alcerr
  USE s_ckoptu
  USE s_menu_ant
  USE f_tstkey
  USE s_writekey
  USE s_readkeys
  USE s_ckopti
  USE s_ckoptc
  USE s_ckoptl
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)              :: keyWord     ! what to do
  CHARACTER(LEN=*)              :: menuauxInp  ! MENUAUX.INP file name

! output:
  TYPE(t_key)                   :: output      ! value: Result to display

! Local Types
! -----------
  TYPE t_antList
    CHARACTER(LEN=20)           :: antnam
    CHARACTER(LEN=20)           :: recnam
    INTEGER(i4b), DIMENSION(2)  :: antnum
    CHARACTER(LEN=5)            :: antfrq
    INTEGER(i4b)                :: antprn
    INTEGER(i4b)                :: numEle
    INTEGER(i4b)                :: numAzi
    REAL(r8b)                   :: sigma
    LOGICAL                     :: inList
  END TYPE t_antList

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER   :: srName = 'menu_rap'

! Local Variables
! ---------------
  TYPE(t_key)                   :: newList       ! Antenna list from obs.files
  TYPE(t_antList),               &
         DIMENSION(:),POINTER   :: antList       ! List of antennas resp.
                                                 ! antenna groups
  TYPE(t_antList)               :: antHelp

  CHARACTER(LEN=keyValueLength), &
         DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=keyValueLength), &
         DIMENSION(:,:),POINTER :: hlpStr

  INTEGER(i4b)                  :: numAnt          ! # in list of antennas
  INTEGER(i4b)                  :: iAnt,jAnt
  INTEGER(i4b)                  :: ii
  INTEGER(i4b)                  :: irc, irCode
  INTEGER(i4b)                  :: ios,iac

  LOGICAL                       :: inList

  NULLIFY(antList)
  NULLIFY(keyValue)
  NULLIFY(hlpStr)

! Check the keyword
! -----------------
  IF (keyWord /= 'RAPSETUP') RETURN


! Have at least an empty uniline
! ------------------------------
  DEALLOCATE(output%value, stat=iac)

  ALLOCATE(output%value(1),stat=iac)
  CALL alcerr(iac,'output%value',(/1/),srName)
  output%value(1) = ' '

  WRITE(output%value(1),'(9(A,1X))') ( qt//qt,ii=1,9 )

  CALL writekey(menuauxInp,(/output/),1,irc)


! Get the list of antennas from actual files
! ------------------------------------------
  CALL menu_ant('ANTLST',newList)

  IF (SIZE(newList%value) == 0) RETURN


! Read the old entries into a buffer
! ----------------------------------
  irCode = 0
  IF (tstkey('RAPSTR')) THEN

    CALL readKeys('RAPSTR',keyValue,irc)

    IF (irc == 0) THEN
      DEALLOCATE(output%value, stat=iac)

      ALLOCATE(output%value(SIZE(keyValue)),stat=iac)
      CALL alcerr(iac,'output%value',(/SIZE(keyValue)/),srName)
      output%value = keyValue

      CALL writekey(menuauxInp,(/output/),1,irc)
    ENDIF

    ALLOCATE(hlpStr(9,SIZE(keyValue)),stat=iac)
    CALL alcerr(iac,'hlpStr',(/9,SIZE(keyValue)/),srName)

    CALL ckoptu(1,'RAPSTR',keyValue,srName,                          &
                'Manual rec.ant.offset setup', irc, irCode,          &
                numCol=SIZE(hlpStr,1),maxVal=SIZE(hlpStr,2),result2=hlpStr)

  ELSE

    DEALLOCATE(keyValue,stat=irc)

    ALLOCATE(keyValue(0),stat=irc)
    CALL alcerr(irc,'keyValue',(/0/),srName)

  ENDIF


! Fill the old entries into the antenna list
! ------------------------------------------
  numAnt = SIZE(keyValue) + SIZE(newList%value)

  ALLOCATE(antList(numAnt),stat=iac)
  CALL alcerr(iac,'antList',(/numAnt/),srName)

  numAnt = SIZE(keyValue)


  IF (tstkey('RAPSTR') .AND. irCode == 0) THEN

    ! Receiver name
    CALL ckoptl(1,'RAPSTR',hlpStr(1,:),srName,                       &
                'Receiver name in rec.ant.pattern setup',irc,irCode, &
                maxVal=numAnt,empty=' ',result2=antList(1:numAnt)%recnam)

    ! Antenna name
    CALL ckoptl(1,'RAPSTR',hlpStr(2,:),srName,                       &
                'Antenna name in rec.ant.pattern setup',irc,irCode,  &
                maxVal=numAnt,empty=' ',result2=antList(1:numAnt)%antnam)

    ! Antenna number, from
    CALL ckopti(1,'RAPSTR',hlpStr(3,:),srName,                       &
                'Antenna num. in rec.ant.pattern setup',irc,irCode,  &
                maxVal=numAnt,ge=0,le=999999,empty=0,                &
                result2=antList(1:numAnt)%antnum(1))

    ! Antenna number, to
    CALL ckopti(1,'RAPSTR',hlpStr(4,:),srName,                       &
                'Antenna num. in rec.ant.pattern setup',irc,irCode,  &
                maxVal=numAnt,ge=0,le=999999,empty=0,                &
                result2=antList(1:numAnt)%antnum(2))

    ! Antenna frequency
    CALL ckoptc(1,'RAOSTR',hlpStr(5,:),(/ 'L1  ','L2  ','L3  ',      &
                'L4  ','L5  ','LC  ','BOTH','1   ','2   ','3   ',    &
                '4   ','5   ','    '/),srName,                       &
                'Frequency in rec.ant.offset setup',irc,irCode,      &
                maxVal=numAnt)
    antList(1:numAnt)%antfrq=hlpStr(5,:)

    ! Antenna satellite system
    CALL ckoptc(1,'RAPSTR',hlpStr(6,:),(/ 'GPS    ','GLO    ',           &
                'GLONASS','GAL    ','GALILEO','MIX    ','GNSS   ' /),    &
                 srName,'Sat.system in rec.ant.offset setup',irc,irCode, &
                 maxVal=numAnt,valList=(/0,1,1,2,2,10,10/),              &
                 result2=antList(:)%antprn)

    ! Pattern: # elev.
    CALL ckopti(1,'RAPSTR',hlpStr(7,:),srName,                       &
                '#Elev. in rec.ant.pattern setup',irc,irCode,        &
                maxVal=numAnt,ge=1,empty=0,                          &
                result2=antList(1:numAnt)%numEle)

    ! Pattern: # azi.
    CALL ckopti(1,'RAPSTR',hlpStr(8,:),srName,                       &
                '#Azi. in rec.ant.pattern setup',irc,irCode,         &
                maxVal=numAnt,ge=1,empty=0,                          &
                result2=antList(1:numAnt)%numAzi)

    ! Apriori sigma
    CALL ckoptr(1,'RAPSTR',hlpStr(9,:),srName,                       &
                'Sigma in rec.ant.pattern setup',irc,irCode,         &
                maxVal=numAnt,ge=0d0,empty=0d0,                      &
                result2=antList(1:numAnt)%sigma)

    antList(1:numAnt)%inList = .FALSE.

  ENDIF

  IF (irCode /= 0) THEN
    WRITE(lfnerr,'(/,A,3(/,18X,A),/)')                                       &
    ' ### SR MENU_RAP: The values in the (old) input setup are not valid.',  &
                      'They cannot be merged with the actual default list',  &
                      'generated by the actual selected obsveration files.', &
                      'The setup will NOT update your last input list!'
    RETURN
  ENDIF


! Get the list of antennas from the observation files
! ---------------------------------------------------
  DEALLOCATE(hlpStr,stat=iac)
  ALLOCATE(hlpStr(3,SIZE(newList%value)),stat=iac)
  CALL alcerr(iac,'hlpStr',(/3,SIZE(newList%value)/),srName)

  CALL ckoptu(1,'RAPSETUP',newList%value,srName,                   &
              'Antenna list from obs. files', irc, irCode,         &
              numCol=SIZE(hlpStr,1),maxVal=SIZE(hlpStr,2),result2=hlpStr)

  IF (irCode /= 0) RETURN

  DEALLOCATE(newList%value,stat=iac)

! Generate the new input list
! ---------------------------
  DO ii = 1,SIZE(hlpStr,2)

    CALL ckoptl(1,'RAPSETUP',(/ hlpStr(1,ii) /),srName,            &
                'Receiver type from obs. files', irc, irCode,      &
                linTit=hlpStr(1,ii),result1=antHelp%recNam)

    CALL ckoptl(1,'RAPSETUP',(/ hlpStr(2,ii) /),srName,            &
                'Antenna nam. from obs. files', irc, irCode,       &
                linTit=hlpStr(2,ii),result1=antHelp%antNam)

    CALL ckopti(1,'RAPSETUP',(/ hlpStr(3,ii) /),srName,            &
                'Antenna num. from obs. files', irc, irCode,       &
                ge=0,le=999999,linTit=hlpStr(1,ii),result1=antHelp%antNum(1))

! Is is it a new antenna?
! -----------------------
    inList = .FALSE.
    DO iAnt = 1, numAnt
      IF (  antList(iAnt)%antnam    == antHelp%antNam    .AND. &
            antList(iAnt)%antnum(1) <= antHelp%antNum(1) .AND. &
            antList(iAnt)%antnum(2) >= antHelp%antNum(1) .AND. &
            antList(iAnt)%recnam    == antHelp%recNam          ) THEN
        antList(iAnt)%inList = .TRUE.
        inList = .TRUE.
        EXIT
      ENDIF
    ENDDO

! Add a record to the list
! ------------------------
    IF ( .NOT. inList ) THEN
      numAnt = numAnt + 1

      antList(numAnt)%recnam    = antHelp%recNam
      antList(numAnt)%antnam    = antHelp%antNam
      antList(numAnt)%antnum(:) = antHelp%antNum(1)
      antList(numAnt)%antPrn    = 10
      antList(numAnt)%antFrq    = ''
      antList(numAnt)%numEle    = 0
      antList(numAnt)%numAzi    = 0
      antList(numAnt)%sigma     = 0d0
      antList(numAnt)%inList    = .TRUE.
    ENDIF

  ENDDO   ! Loops the list of obs. files

  DEALLOCATE(hlpStr,stat=irc)

! Sort the list of antennas
! -------------------------
  inList = .TRUE.
  DO WHILE (inList)
    inList = .FALSE.
    ii = 1
    DO WHILE (ii < numAnt)
      IF ( antList(ii)%recnam > antList(ii+1)%recnam      .OR. &
          (antList(ii)%recnam == antList(ii+1)%recnam .AND. &
           antList(ii)%antnam > antList(ii+1)%antnam)     .OR. &
          (antList(ii)%recnam == antList(ii+1)%recnam .AND. &
           antList(ii)%antnam == antList(ii+1)%antnam .AND. &
           antList(ii)%antnum(2) > antList(ii+1)%antnum(1))) THEN
        antHelp       = antList(ii)
        antList(ii)   = antList(ii+1)
        antList(ii+1) = antHelp
        inList = .TRUE.
      ENDIF
      ii = ii + 1
    ENDDO
  ENDDO

! Generate the new entry for the default uniline
! ----------------------------------------------
  iAnt = 0
  DO ii = 1,numAnt
    IF (antList(ii)%inList) iAnt = iAnt + 1
  ENDDO

  DEALLOCATE(output%value,stat=iac)
  ALLOCATE(output%value(iAnt),stat=iac)
  CALL alcerr(irc,'output%value',(/iAnt/),srName)

  ALLOCATE(hlpStr(9,1),stat=iac)
  CALL alcerr(iac,'hlpStr',(/9,1/),srName)

  jAnt = 0
  DO iAnt = 1, numAnt
    IF (.NOT. antList(iAnt)%inList) CYCLE

    hlpStr(:,1) = ''

    hlpStr(1,1) = qt // TRIM(antList(iAnt)%recnam) // qt // ' '
    hlpStr(2,1) = qt // TRIM(antList(iAnt)%antnam) // qt // ' '
    WRITE(hlpStr(3,1), '(A,I6,A)') qt, antList(iAnt)%antnum(1), qt // ' '
    WRITE(hlpStr(4,1), '(A,I6,A)') qt, antList(iAnt)%antnum(2), qt // ' '

    hlpStr(5,1) = qt // TRIM(antList(iAnt)%antfrq) // qt // ' '

    IF (antList(iAnt)%antprn >= 0 .AND. antList(iAnt)%antprn <= 10) THEN
      hlpStr(6,1) = qt // g_strsys3(antList(iAnt)%antprn) // qt // ' '
    ELSE
      hlpStr(6,1) = qt // qt // ' '
    ENDIF

    IF (antList(iAnt)%numEle > 0) THEN
      WRITE(hlpStr(7,1), '(A,I2,A)') qt, antList(iAnt)%numEle, qt // ' '
    ELSE
      hlpStr(7,1) = qt // qt // ' '
    ENDIF

    IF (antList(iAnt)%numAzi > 0) THEN
      WRITE(hlpStr(8,1), '(A,I2,A)') qt, antList(iAnt)%numAzi, qt // ' '
    ELSE
      hlpStr(8,1) = qt // qt // ' '
    ENDIF

    IF (antList(iAnt)%sigma > 0d0) THEN
      WRITE(hlpStr(9,1), '(A,F8.3,A)') qt, antList(iAnt)%sigma, qt // ' '
    ELSE
      hlpStr(9,1) = qt // qt // ' '
    ENDIF

    jAnt = jAnt + 1
    WRITE(output%value(jAnt),'(9(A,1X))') (TRIM(hlpStr(ii,1)),ii=1,9)

  ENDDO

  DEALLOCATE(antList, stat=ios)
  DEALLOCATE(keyValue,stat=ios)
  DEALLOCATE(hlpStr,stat=ios)


  RETURN
END SUBROUTINE menu_rap

END MODULE
