MODULE s_MENU_POL
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_pol(keyWord, menuauxInp, output)

! -------------------------------------------------------------------------
! Purpose:    Generate uniline containing default settings for records for
!             each file
!
! Author:     U. Hugentobler
!
! Created:    16-Jul-2001
!
! Changes:    30-Apr-2002 RD: Use keywords from MENUAUX.INP
!             16-May-2002 RD: Replace old values in the case of an error
!             13-Dec-2002 RD: New quotes-handling in SR writekey
!             23-Apr-2003 AJ: Nullify local pointers
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, lfnerr, t_key, &
                      fileNameLength, keyValueLength, shortLineLength
  USE p_menaux, ONLY: qt

  USE s_gtfile2
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
  TYPE t_polList
    CHARACTER(LEN=fileNameLength)         :: filnam
    INTEGER(i4b),DIMENSION(5)             :: record
    CHARACTER(LEN=keyValueLength)         :: inpLin
    LOGICAL                               :: inList
  END TYPE t_polList

! Local Parameters
! ----------------
  CHARACTER(LEN=8),             PARAMETER :: srName = 'menu_pol'

  CHARACTER(LEN=7),DIMENSION(5),PARAMETER :: recKeyw = &
  (/ 'RECORD1','RECORD2','RECORD3','RECORD4','RECORD5' /)

! Local Variables
! ---------------
  TYPE(t_polList),               &
      DIMENSION(:),ALLOCATABLE  :: polList

  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:),  POINTER    :: keyValue
  CHARACTER(LEN=fileNameLength), &
      DIMENSION(:,:),  POINTER  :: filList
  CHARACTER(LEN=shortLineLength),&
      DIMENSION(:,:),ALLOCATABLE:: hlpStr

  INTEGER(i4b),                  &
      DIMENSION(:),  ALLOCATABLE:: recIdx
  INTEGER(i4b), DIMENSION(5)    :: newRec
  INTEGER(i4b)                  :: iFil, jFil
  INTEGER(i4b)                  :: nFil
  INTEGER(i4b)                  :: nRec
  INTEGER(i4b)                  :: ii,jj
  INTEGER(i4b)                  :: irCode
  INTEGER(i4b)                  :: iac,irc

  LOGICAL                       :: sorted

  NULLIFY(keyValue)
  NULLIFY(filList)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'POLXTR') RETURN

! Have at least an empty uniline
! ------------------------------
  ALLOCATE(output%value(1),stat=iac)
  CALL alcerr(iac,'output%value',(/1/),srName)
  output%value(1) = ' '

  WRITE(output%value(1),'(6(A,1X))') ( qt//qt,ii=1,6 )

  CALL writekey(menuauxInp,(/output/),1,irc)

! Get the new list of files
! -------------------------
  CALL gtfile2('EPFIL',1,nFil,filList)

  IF (nFil == 0) RETURN

! Get the new default record values
! ---------------------------------
  irCode = 0

  newRec = 0
  DO ii = 1,SIZE(recKeyw)
    IF (.NOT. tstKey(recKeyw(ii))) CYCLE

    CALL readKeys(recKeyw(ii),keyValue,irc)

    CALL ckopti(1,recKeyw(ii),keyValue,srName,                  &
                'Default records for extractions',irc,irCode,   &
                maxVal=1,ge=1,result1=newRec(ii))
  ENDDO

! Read the old entries into a buffer
! ----------------------------------
  IF (tstKey('RECLIST')) THEN

    CALL readKeys('RECLIST',keyValue,irc)

    IF (irc == 0) THEN
      DEALLOCATE(output%value, stat=iac)

      ALLOCATE(output%value(SIZE(keyValue)),stat=iac)
      CALL alcerr(iac,'output%value',(/SIZE(keyValue)/),srName)
      output%value = keyValue

      CALL writekey(menuauxInp,(/output/),1,irc)
    ENDIF

    ALLOCATE(hlpStr(6,SIZE(keyValue)),stat=iac)
    CALL alcerr(iac,'hlpStr',(/6,SIZE(keyValue)/),srName)

    CALL ckoptu(1,'RECLIST',keyValue,srName,                         &
                'File / Record setup', irc, irCode,                  &
                numCol=SIZE(hlpStr,1),maxVal=SIZE(hlpStr,2),result2=hlpStr)

  ELSE

    DEALLOCATE(keyValue,stat=iac)

    ALLOCATE(keyValue(0),stat=iac)
    CALL alcerr(iac,'keyValue',(/0/),srName)

  ENDIF

! Allocate the array
! ------------------
  nRec = SIZE(keyValue)

  ALLOCATE(polList(nRec+nFil),stat=iac)
  CALL alcerr(iac,'polList',(/ nRec+nFil /),srName)

  IF (tstKey('RECLIST')) THEN

    polList(1:nRec)%inpLin = keyValue(:)

    ! File names
    CALL ckoptl(1,'RECLIST',hlpStr(1,:),srName,                      &
                'File names in File/Record setup',irc,irCode,        &
                maxVal=nRec,empty=' ',result2=polList(1:nRec)%filnam)

    IF (irCode /= 0) THEN
      WRITE(lfnerr,'(/,A,2(/,18X,A),/)')                                       &
      ' ### SR MENU_POL: The file names in the old file/record setup are.',    &
                        'not valid. All settings will be replaced by new',     &
                        'default values corresponding to the actual pole',     &
                        'file selection!'
      nRec = 0
    ENDIF

    ! This part not really necessary
    IF ( 1 == 2 ) THEN

      ! Record numbers
      DO ii = 1,SIZE(recKeyw)
        CALL ckopti(1,'POLLIST',hlpStr(1+ii,:),srName,                 &
                    'Records in File/Record setup',irc,irCode,         &
                    maxVal=nRec,ge=1,empty=0,result2=polList(1:nRec)%record(ii))
      ENDDO

    ENDIF

    DEALLOCATE(hlpStr,stat=iac)
  ENDIF

  DEALLOCATE(keyValue,stat=iac)

! Is the file still in list?
! --------------------------
  polList(1:nRec)%inList = .FALSE.

  DO iFil = 1,nFil

    jFil = 0
    DO ii = 1,nRec
      IF (filList(1,iFil) == polList(ii)%filnam) jFil = ii
    ENDDO

    ! New file
    IF (jFil == 0) THEN
      nRec = nRec+1
      polList(nRec)%filNam = filList(1,iFil)
      polList(nRec)%record = newRec
      polList(nRec)%inpLin = ' '
      jFil = nRec
    ENDIF

    polList(jFil)%inList = .TRUE.
  ENDDO

  DEALLOCATE(filList,stat=iac)

! Sort the file records
! ---------------------
  ALLOCATE(recIdx(nRec),stat=iac)
  CALL alcerr(iac,'recIdx',(/nRec/),srName)

  recIdx(1:nRec) = (/ (ii,ii=1,nRec) /)

  sorted=.FALSE.
  DO WHILE (.NOT. sorted)
    sorted=.TRUE.
    DO ii = 1,nRec-1
      IF (polList(recIdx(ii))%filnam > polList(recIdx(ii+1))%filnam) THEN
        sorted = .FALSE.

        iFil = recIdx(ii)
        recIdx(ii) = recIdx(ii+1)
        recIdx(ii+1) = iFil
      ENDIF
    ENDDO
  ENDDO

! Generate the new entry for the default uniline
! ----------------------------------------------
  ALLOCATE(hlpStr(6,1),stat=iac)
  CALL alcerr(iac,'hlpStr',(/6,1/),srName)

  iFil = 0
  DO ii = 1,nRec
    IF (.NOT. polList(ii)%inList) CYCLE
    IF (LEN_TRIM(polList(ii)%inpLin) > 0) CYCLE

    hlpStr(:,1) = ' '

    hlpStr(1,1) = qt // TRIM(polList(ii)%filnam) // qt // ' '

    DO jj = 1,5
      IF (polList(ii)%record(jj) /= 0) THEN
        WRITE(hlpStr(1+jj,1),'(A,I3,A)') &
              qt,polList(ii)%record(jj),qt//' '
      ELSE
        hlpStr(1+jj,1) = qt // qt // ' '
      ENDIF
    ENDDO

    WRITE(polList(ii)%inpLin,'(6(A,1X))') (TRIM(hlpStr(jj,1)),jj=1,6)

  ENDDO

  DEALLOCATE(hlpStr, stat=iac)


! Put the default input lines into the output record
! --------------------------------------------------
  iFil = 0
  DO ii = 1,nRec
    IF (polList(ii)%inList) iFil = iFil + 1
  ENDDO

  DEALLOCATE(output%value,stat=iac)
  ALLOCATE(output%value(iFil),stat=iac)
  CALL alcerr(iac,'output%value',(/iFil/),srName)

  iFil = 0
  DO ii = 1,nRec

    IF (.NOT. polList(recIdx(ii))%inList) CYCLE

    iFil = iFil + 1
    output%value(iFil) = polList(recIdx(ii))%inpLin

  ENDDO

  DEALLOCATE(polList,stat=iac)
  DEALLOCATE(recIdx, stat=iac)

  RETURN
END SUBROUTINE menu_pol

END MODULE
