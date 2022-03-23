MODULE s_MENU_SIM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_sim(keyWord, menuauxInp, output)

! -------------------------------------------------------------------------
! Purpose:    Create station list out of CRD-file
!
! Author:     H.Bock
!
! Created:    15-Aug-2001
!
! Changes:    29-Apr-2002 RD: Use keywords from MENUAUX.INP
!             16-May-2002 RD: Replace old values in the case of an error
!             13-Dec-2002 RD: New quotes-handling in SR writekey
!             23-Apr-2003 AJ: Nullify local pointers
!             08-Sep-2003 HU: antnam, recnam, oprnam chr16 -> chr20
!             02-Oct-2003 RD: Abbreviation is not in uniline anymore
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, lfnerr, t_key, &
                      keyValueLength, shortLineLength, &
                      fileNameLength, staNameLength
  USE p_menaux, ONLY: qt

  USE s_alcerr
  USE s_ckoptu
  USE f_tstkey
  USE s_writekey
  USE s_readkeys
  USE s_ckopti
  USE s_ckoptl
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)                        :: keyWord  ! what to do
  CHARACTER(LEN=*)                        :: menuauxInp ! INP file name

! output:
  TYPE(t_key)                             :: output   ! value: Result to display

! Local Types
! -----------
  TYPE t_simList
    CHARACTER(LEN=16)                     :: stanam
    CHARACTER(LEN=20)                     :: antnam
    CHARACTER(LEN=20)                     :: recnam
    INTEGER(i4b)                          :: antnum
    INTEGER(i4b)                          :: recnum
    CHARACTER(LEN=20)                     :: operat
    CHARACTER(LEN=keyValueLength)         :: inpLin
    LOGICAL                               :: inList
  END TYPE t_simList


! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER             :: srName = 'menu_sim'

! Local Variables
! ---------------
  TYPE(t_simList),               &
      DIMENSION(:),ALLOCATABLE  :: simList
  TYPE(t_simList)               :: simHelp

  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:),  POINTER    :: keyValue
  CHARACTER(LEN=staNameLength),  &
      DIMENSION(:),  POINTER    :: staList
  CHARACTER(LEN=shortLineLength),&
      DIMENSION(:,:),ALLOCATABLE:: hlpStr

  INTEGER(i4b)                  :: nStat          ! Number of stations in list
  INTEGER(i4b)                  :: iSim
  INTEGER(i4b)                  :: iSta,jSta
  INTEGER(i4b)                  :: ii
  INTEGER(i4b)                  :: irCode
  INTEGER(i4b)                  :: irc,iac

  LOGICAL                       :: sorted

  NULLIFY(keyValue)
  NULLIFY(staList)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'SIMLST') RETURN

! Have at least an empty uniline
! ------------------------------
  ALLOCATE(output%value(1),stat=iac)
  CALL alcerr(iac,'output%value',(/1/),srName)
  output%value(1) = ' '

  WRITE(output%value(1),'(7(A,1X))') ( qt//qt,ii=1,7 )

  CALL writekey(menuauxInp,(/output/),1,irc)

  irCode = 0

! Get the actual list of stations
! -------------------------------
  CALL readKeys('SIMSTA',keyValue,irc)

  IF (irc /= 0 .OR. SIZE(keyValue) == 0) RETURN

  ALLOCATE(staList(SIZE(keyValue)),stat=irc)
  CALL alcerr(irc,'staList',(/SIZE(keyValue)/),srName)

  CALL ckoptl(1,'STASIM',keyValue,srName,           &
              'Stations for simulation',irc,irCode, &
              empty=' ',maxLength=staNameLength,result2=staList)

! Read the old entries into a buffer
! ----------------------------------
  IF (tstKey('STAINF')) THEN

    CALL readKeys('STAINF',keyValue,irc)

    IF (irc == 0) THEN
      DEALLOCATE(output%value,stat=iac)

      ALLOCATE(output%value(SIZE(keyValue)),stat=iac)
      CALL alcerr(iac,'output%value',(/SIZE(keyValue)/),srName)
      output%value = keyValue

      CALL writekey(menuauxinp,(/output/),1,irc)
    ENDIF

    ALLOCATE(hlpStr(6,SIZE(keyValue)),stat=iac)
    CALL alcerr(iac,'hlpStr',(/6,SIZE(keyValue)/),srName)

    CALL ckoptu(1,'STAINF',keyValue,srName,                         &
                'Station information setup', irc, irCode,           &
                numCol=SIZE(hlpStr,1),maxVal=SIZE(hlpStr,2),result2=hlpStr)

  ELSE

    DEALLOCATE(keyValue,stat=irc)

    ALLOCATE(keyValue(0),stat=irc)
    CALL alcerr(irc,'keyValue',(/0/),srName)

  ENDIF


! Fill old information into the station list
! ------------------------------------------
  nStat = SIZE(keyValue) + SIZE(staList)

  ALLOCATE(simList(nStat),stat=iac)
  CALL alcerr(iac,'simList',(/nStat/),srName)

  nStat = SIZE(keyValue)

  IF (tstKey('STAINF')) THEN

    simList(1:nStat)%inpLin = keyValue(:)

    ! Station name
    CALL ckoptl(1,'STAINF',hlpStr(1,:),srName,                    &
                'Station name in sta.info setup',irc,irCode,      &
                maxVal=nStat,empty=' ',result2=simList(1:nStat)%staNam)

    IF (irCode /= 0) THEN
      WRITE(lfnerr,'(/,A,3(/,18X,A),/)')                                       &
      ' ### SR MENU_SIM: The station names in the old simulation setup are.',  &
                        'not valid. All settings will be replaced by new',     &
                        'default values corresponding to the actual station',  &
                        'selection!'
      nStat = 0
    ENDIF

    ! This part not really necessary
    IF ( 1 == 2 ) THEN

      ! Receiver name
      CALL ckoptl(1,'STAINF',hlpStr(2,:),srName,                    &
                  'Receiver name in sta.info setup',irc,irCode,     &
                  maxVal=nStat,maxLength=9,empty=' ',               &
                  result2=simList(1:nStat)%recNam)

      ! Antenna name
      CALL ckoptl(1,'STAINF',hlpStr(3,:),srName,                    &
                  'Antenna name in sta.info setup',irc,irCode,      &
                  maxVal=nStat,empty=' ',result2=simList(1:nStat)%antNam)

      ! Receiver number
      CALL ckopti(1,'STAINF',hlpStr(4,:),srName,                      &
                  'Receiver num. in sta.info setup',irc,irCode,       &
                  maxVal=nStat,ge=0,le=999999,empty=-1,               &
                  result2=simList(1:nStat)%recnum)

      ! Antenna number
      CALL ckopti(1,'STAINF',hlpStr(5,:),srName,                      &
                  'Antenna num. in sta.info setup',irc,irCode,        &
                  maxVal=nStat,ge=0,le=999999,empty=-1,               &
                  result2=simList(1:nStat)%antnum)

      ! Operator name
      CALL ckoptl(1,'STAINF',hlpStr(6,:),srName,                    &
                  'Operator name in sta.info setup',irc,irCode,     &
                  maxVal=nStat,empty=' ',result2=simList(1:nStat)%operat)

    ENDIF

    DEALLOCATE(hlpStr,stat=iac)
  ENDIF

! Is the station still in list?
! -----------------------------
  simList(1:nStat)%inList = .FALSE.

  DO iSta = 1,SIZE(staList)
    jSta = 0
    DO iSim = 1,nStat
      IF (simList(iSim)%staNam == staList(iSta)) jSta = iSim
    ENDDO

    ! Add an empty new station
    IF (jSta == 0) THEN
      nStat=nStat+1
      simList(nStat)%staNam=staList(iSta)
      simList(nStat)%antNam=' '
      simList(nStat)%recNam=' '
      simList(nStat)%antNum=-1
      simList(nStat)%recNum=-1
      simList(nStat)%operat=' '
      simList(nStat)%inpLin=' '
      jSta = nStat
    ENDIF

    simList(jSta)%inList = .TRUE.
  ENDDO

  DEALLOCATE(staList,stat=iac)

! Sort the list of sim. stations
! ------------------------------
  sorted = .FALSE.
  DO WHILE (.NOT. sorted)
    sorted = .TRUE.
    DO iSta = 1,nStat-1
      IF (simList(iSta)%staNam > simList(iSta+1)%staNam) THEN
        simHelp         = simList(iSta)
        simList(iSta)   = simList(iSta+1)
        simList(iSta+1) = simHelp

        sorted = .FALSE.
      ENDIF
    ENDDO
  ENDDO

! Generate the new entry for the default uniline
! ----------------------------------------------
  ALLOCATE(hlpStr(6,1),stat=iac)
  CALL alcerr(iac,'hlpStr',(/6,1/),srName)

  DO iSta = 1, nStat
    IF (.NOT. simList(iSta)%inList) CYCLE
    IF (LEN_TRIM(simList(iSta)%inpLin) > 0) CYCLE

    hlpStr(:,1) = ''

    hlpStr(1,1) = qt // TRIM(simList(iSta)%staNam) // qt // ' '
    hlpStr(2,1) = qt // TRIM(simList(iSta)%recNam) // qt // ' '
    hlpStr(3,1) = qt // TRIM(simList(iSta)%antNam) // qt // ' '
    IF (simList(iSta)%recNum /= -1) THEN
      WRITE(hlpStr(4,1),'(A,I6,A)') qt,simList(iSta)%recNum,qt // ' '
    ELSE
      hlpStr(4,1) = qt // qt // ' '
    ENDIF
    IF (simList(iSta)%antNum /= -1) THEN
      WRITE(hlpStr(5,1),'(A,I6,A)') qt,simList(iSta)%antNum,qt // ' '
    ELSE
      hlpStr(5,1) = qt // qt // ' '
    ENDIF
    hlpStr(6,1) = qt // TRIM(simList(iSta)%operat) // qt // ' '

    WRITE(simList(iSta)%inpLin,'(6(A,1X))') (TRIM(hlpStr(ii,1)),ii=1,6)

  ENDDO

  DEALLOCATE(hlpStr,stat=iac)

! Put the default input lines into the output record
! --------------------------------------------------
  iSta = 0
  DO jSta = 1,nStat
    IF (simList(jSta)%inList) iSta = iSta + 1
  ENDDO

  DEALLOCATE(output%value,stat=iac)
  ALLOCATE(output%value(iSta),stat=iac)
  CALL alcerr(iac,'output%value',(/iSta/),srName)

  iSim = 0
  DO iSta = 1, nStat
    IF (.NOT. simList(iSta)%inList) CYCLE

    iSim = iSim + 1
    output%value(iSim) = simList(iSta)%inpLin

  ENDDO

  DEALLOCATE(simList,stat=iac)
  DEALLOCATE(keyValue,stat=iac)
  DEALLOCATE(staList,stat=iac)

  RETURN
END SUBROUTINE menu_sim

END MODULE
