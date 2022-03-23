MODULE s_GTSTAFLG
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE gtStaflg(staflg,flgList,staInfo,flgStr)

! -------------------------------------------------------------------------
!
! Purpose:    Reads the list of flags considered from station information
!             file
!
! Author:     R. Dach
!
! Created:    05-May-2003
!
! Changes:    15-May-2003 HB: list of flags from TYPE 005
!             09-Jul-2003 RD: Switch flag handling permanently off
!             05-Oct-2010 SL: use m_bern with ONLY, flgLst(INTEGER->CHARACTER)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, keyValueLength
  USE d_stacrx, ONLY: t_stacrux

  USE s_alcerr
  USE s_splarg
  USE f_tstkey
  USE s_readkeys
  USE s_ckoptb
  USE s_ckoptl
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)               :: staflg  ! Keyword for "Use Flags"
  CHARACTER(LEN=*), DIMENSION(5) :: flgList ! Keywords for flag lines

! input/output
  TYPE(t_staCrux)                :: staInfo ! Station information, reduced
                                            ! to the selected entries
! output:
  CHARACTER(LEN=*), DIMENSION(5), &
                    OPTIONAL     :: flgStr  ! Input lines for flags
                                            ! may be used, e.g., for pgm output

! Local parameters
! ----------------
  CHARACTER(LEN=8),  PARAMETER               :: srName = 'gtstaflg'

  CHARACTER(LEN=30), PARAMETER, DIMENSION(5) :: errTxt = (/ &
               'Flags for station renaming    ',            &
               'Flags for station information ',            &
               'Flags for station problems    ',            &
               '                              ',            &
               'Flags for marker type         '/)

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: argv


  CHARACTER(LEN=3), DIMENSION(:), &
    ALLOCATABLE                          :: flgLst ! List of flags
  INTEGER(i4b)                           :: nFlg
  INTEGER(i4b)                           :: iTyp
  INTEGER(i4b)                           :: ii,jj
  INTEGER(i4b)                           :: irc     ! return code froms SRs
  INTEGER(i4b)                           :: iac     ! allocation status
  INTEGER(i4b)                           :: irCode  ! Counting errors

  LOGICAL                                :: useFlg
  LOGICAL                                :: inList

! Init variables
! --------------
  irCode=0

  NULLIFY(keyValue)
  NULLIFY(argv)


! Take care on flags in file
! --------------------------
  IF (1 == 1) THEN    ! Skip flag handling
    useFlg = .FALSE.
  ELSE IF (LEN_TRIM(staFlg) == 0) THEN
    useFlg = .TRUE.
  ELSE IF (tstkey(staFlg)) THEN
    useFlg = .TRUE.
  ELSE
    CALL ckoptb(1,(/staflg/),srName,                                 &
                'Consider flags in station information file',irCode, &
                resultL=useFlg)
  ENDIF

! Read flags for all types
! ------------------------
  DO iTyp = 1,SIZE(flgList)

    ! No flags to be considered
    IF (.NOT. useFlg) THEN
      IF (PRESENT(flgStr)) flgStr(iTyp) = '999'
      CYCLE
    ENDIF

    ! Keyword not in input file or empty
    IF (LEN_TRIM(flgList(iTyp)) == 0 .OR. &
        .NOT. TSTKEY(flgList(iTyp))) THEN
      IF (PRESENT(flgStr)) flgStr(iTyp) = ' '
      CYCLE
    ENDIF

    ! Read the list of flags
    IF ((iTyp == 1 .AND. staInfo%nrenam > 0) .OR. &
        (iTyp == 2 .AND. staInfo%ninfo  > 0) .OR. &
        (iTyp == 3 .AND. staInfo%nprob  > 0) .OR. &
        (iTyp == 5 .AND. staInfo%nstatype  > 0)) THEN

      CALL readKeys(flgList(iTyp),keyValue,irc)

      ! Empty flag line:
      IF (LEN_TRIM(keyValue(1)) == 0) THEN

        IF (PRESENT(flgStr)) flgStr(iTyp) = ' '

        IF (iTyp == 1) THEN
          DEALLOCATE(staInfo%renamSta,stat=iac)
          staInfo%nrenam = 0
        ELSEIF (iTyp == 2) THEN
          DEALLOCATE(staInfo%staInfo,stat=iac)
          staInfo%nInfo = 0
        ELSEIF (iTyp == 3) THEN
          DEALLOCATE(staInfo%staProb,stat=iac)
          staInfo%nProb = 0
        ELSEIF (iTyp == 5) THEN
          DEALLOCATE(staInfo%statype,stat=iac)
          staInfo%nstatype = 0
        ENDIF

        CYCLE

      ENDIF

      ! Extract the list of flags
      CALL splarg(keyValue(1),argv)

      nFlg = SIZE(argv)

      ALLOCATE(flgLst(nFlg),stat=iac)
      CALL alcerr(iac,'flgLst',(/nFlg/),srName)

      CALL ckoptl(1,flgList(iTyp),argv,srName,errTxt(iTyp),irc,irCode,     &
                  maxVal=nFlg,error='ERR',empty='ERR',result2=flgLst)

      ! Invalid element in list
      inList = .FALSE.
      DO jj = 1,nFlg
        inList = inList .OR. (flgLst(jj) == 'ERR')
      ENDDO

      IF (inList) THEN

        IF (PRESENT(flgStr)) flgStr(iTyp) = ' '

        IF (iTyp == 1) THEN
          DEALLOCATE(staInfo%renamSta,stat=iac)
          staInfo%nrenam = 0
        ELSEIF (iTyp == 2) THEN
          DEALLOCATE(staInfo%staInfo,stat=iac)
          staInfo%nInfo = 0
        ELSEIF (iTyp == 3) THEN
          DEALLOCATE(staInfo%staProb,stat=iac)
          staInfo%nProb = 0
        ELSEIF (iTyp == 5) THEN
          DEALLOCATE(staInfo%statype,stat=iac)
          staInfo%nstatype = 0
        ENDIF

        DEALLOCATE(flgLst,stat=iac)
        CYCLE

      ENDIF

      ! Use all flags
      inList = .FALSE.
      DO jj = 1,nFlg
        inList = inList .OR. (flgLst(jj) == '999')
      ENDDO

      IF (inList) THEN

        IF (PRESENT(flgStr)) flgStr(iTyp) = '999'

        DEALLOCATE(flgLst,stat=iac)
        CYCLE

      ENDIF

      ! Store flag list
      IF (PRESENT(flgStr)) flgStr(iTyp) = KeyValue(1)

      ! Update station renaming list for flag list
      IF (iTyp == 1) THEN
        ii = 1
        DO WHILE (ii <= staInfo%nrenam)

          inList = .FALSE.
          DO jj = 1,nFlg
            inList = inList .OR. (staInfo%renamSta(ii)%flg == flgLst(jj))
          ENDDO

          IF (inList) THEN
            ii = ii + 1
            CYCLE
          ENDIF

          staInfo%renamSta(ii:staInfo%nrenam-1) = &
                  staInfo%renamSta(ii+1:staInfo%nrenam)
          staInfo%nrenam = staInfo%nrenam - 1

        ENDDO ! Next entry in station renaming list

      ENDIF

      ! Update station information list for flag list
      IF (iTyp == 2) THEN
        ii = 1
        DO WHILE (ii <= staInfo%nInfo)

          inList = .FALSE.
          DO jj = 1,nFlg
            inList = inList .OR. (staInfo%staInfo(ii)%flg == flgLst(jj))
          ENDDO

          IF (inList) THEN
            ii = ii + 1
            CYCLE
          ENDIF

          staInfo%staInfo(ii:staInfo%nInfo-1) = &
                  staInfo%staInfo(ii+1:staInfo%nInfo)
          staInfo%nInfo = staInfo%nInfo - 1

        ENDDO ! Next entry in station information list

      ENDIF
      ! Update station problem list for flag list
      IF (iTyp == 3) THEN
        ii = 1
        DO WHILE (ii <= staInfo%nProb)

          inList = .FALSE.
          DO jj = 1,nFlg
            inList = inList .OR. (staInfo%staProb(ii)%flg == flgLst(jj))
          ENDDO

          IF (inList) THEN
            ii = ii + 1
            CYCLE
          ENDIF

          staInfo%staProb(ii:staInfo%nProb-1) = &
                  staInfo%staProb(ii+1:staInfo%nProb)
          staInfo%nProb = staInfo%nProb - 1

        ENDDO ! Next entry in station problem list

      ENDIF

      ! Update marker type list for flag list
      IF (iTyp == 5) THEN
        ii = 1
        DO WHILE (ii <= staInfo%nstatype)

          inList = .FALSE.
          DO jj = 1,nFlg
            inList = inList .OR. (staInfo%statype(ii)%flg == flgLst(jj))
          ENDDO

          IF (inList) THEN
            ii = ii + 1
            CYCLE
          ENDIF

          staInfo%statype(ii:staInfo%nstatype-1) = &
                  staInfo%statype(ii+1:staInfo%nstatype)
          staInfo%nstatype = staInfo%nstatype - 1

        ENDDO ! Next entry in station problem list

      ENDIF

      DEALLOCATE(flgLst,stat=iac)
    ENDIF
  ENDDO

  RETURN
END SUBROUTINE gtstaflg

END MODULE
