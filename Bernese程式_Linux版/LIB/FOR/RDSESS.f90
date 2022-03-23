MODULE s_RDSESS
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdsess(sesFil,sesTbl)

! -------------------------------------------------------------------------
! Purpose:    This is a new version of the old subroutine GTSESS that
!             reads the session table
!
! Author:     L. Mervart
!
! Created:    22-Nov-2000
!
! Changes:    28-Jan-2003 AJ: d0 for divisions added
!             11-Apr-2003 RD: Use data structure and ckopt-SRs
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             15-Oct-2009 SS: Allow more flexible session windows
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, &
                      keyValueLength, timStrgLength2
  USE d_sess,   ONLY: t_sesLst

  USE s_ckoptt
  USE s_alcerr
  USE s_ckoptu
  USE s_read1key
  USE s_exitrc
  USE s_ckoptd
  USE s_ckoptl
  USE s_timst2
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)               :: sesFil  ! session file name

! output
  TYPE(t_sesLst)                 :: sesTbl  ! Session table

! List of funtions
! ----------------

! Local types
! -----------

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER   :: srName = 'rdsess'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:),   POINTER     :: keyValue
  CHARACTER(LEN=keyValueLength), DIMENSION(:,:), ALLOCATABLE :: hlpStr
  CHARACTER(LEN=timStrgLength2)                              :: epoStr

  INTEGER(i4b)                           :: ii
  INTEGER(i4b)                           :: irCode
  INTEGER(i4b)                           :: irc,iac

  REAL(r8b), DIMENSION(:,:), ALLOCATABLE :: hlpTim

! Deallocate old session table
! ----------------------------
  IF (ASSOCIATED(sesTbl%sess)) DEALLOCATE(sesTbl%sess,stat=iac)

! Init variable
! -------------
  irCode = 0

  NULLIFY(keyValue)

! Read the session table
! ----------------------
  CALL read1key(sesFil, 'LIST_OF_SESSIONS', keyValue, irc)

! Get the number of sessions
! --------------------------
  sesTbl%nSess = SIZE(keyValue)

  IF (sesTbl%nSess == 0) THEN
    WRITE(lfnerr,'(/,A,/,16X,A,/)')                           &
    ' ### SR RDSESS: No sessions found in the session table', &
                    'Session file:  '//TRIM(sesFil)
    RETURN
  ENDIF

! Create the buffer for extracting the information
! ------------------------------------------------
  ALLOCATE(hlpStr(5,sesTbl%nSess),stat=iac)
  CALL alcerr(iac,'hlpStr',(/5,sesTbl%nSess/),srName)

! Extract the session table into the buffer
! -----------------------------------------
  CALL ckoptu(1,'LIST_OF_SESSIONS', keyValue, srName, &
              'Session table',irc,irCode,5,           &
              maxVal=sesTbl%nSess,result2=hlpStr)

  IF (irCode /= 0) CALL exitrc(2)

! An empty sessin list found
! --------------------------
  IF (LEN_TRIM(hlpStr(1,1)) == 0) THEN
    sesTbl%nSess = 0

    WRITE(lfnerr,'(/,A,/,16X,A,/)')                           &
    ' ### SR RDSESS: No sessions found in the session table', &
                    'Session file:  '//TRIM(sesFil)

    DEALLOCATE(hlpStr,stat=iac)

    RETURN
  ENDIF

! Allocate the session table
! --------------------------
  ALLOCATE(sesTbl%sess(sesTbl%nSess),stat=iac)
  CALL alcerr(iac,'sesTbl%sess',(/sesTbl%nSess/),srName)

! Get the session IDs
! -------------------
  CALL ckoptl(1,'LIST_OF_SESSIONS', hlpStr(1,:), srName, &
              'Session table',0,irCode,                  &
              maxLength=4,colTit='Session ID',           &
              maxVal=sesTbl%nSess,result2=sesTbl%sess(:)%sessID)


! Check for fixed or open session table
! -------------------------------------
  sesTbl%fix = (sesTbl%sess(1)%sessID(1:3) /= '???')

  DO ii = 2,sesTbl%nSess
    IF ((sesTbl%fix .AND. sesTbl%sess(ii)%sessID(1:3) == '???') .OR. &
        (.NOT. sesTbl%fix .AND. sesTbl%sess(1)%sessID(1:3) /= '???')) THEN
      WRITE(lfnerr,'(/,A,2(/,16X,A),/)')                            &
      ' *** SR RDSESS: No mixture between fixed and open session ', &
                      'definition is allowed.',                     &
                      'Session file:  '//TRIM(sesFil)
      CALL exitrc(2)
    ENDIF
  ENDDO

! Allocate the buffer for the date/time
! -------------------------------------
  ALLOCATE(hlpTim(4,sesTbl%nSess),stat=iac)
  CALL alcerr(iac,'hlpTim',(/4,sesTbl%nSess/),srName)

! Extract the start date
! ----------------------
  IF (sesTbl%fix) THEN
    CALL ckoptd(1,'LIST_OF_SESSIONS', hlpStr(2,:), srName, &
                'Session table',0,irCode,                  &
                colTit='Start date',ge=-24d0,              &
                maxVal=sesTbl%nSess,result2=hlpTim(1,:))
  ELSE
    CALL ckoptd(1,'LIST_OF_SESSIONS', hlpStr(2,:), srName, &
                'Session table',0,irCode,                  &
                colTit='Start date',empty=1d20,ge=-24d0,   &
                maxVal=sesTbl%nSess,result2=hlpTim(1,:))
  ENDIF

! Extract the start time
! ----------------------
  CALL ckoptt(1,'LIST_OF_SESSIONS', hlpStr(3,:), srName,   &
              'Session table',0,irCode,                    &
              colTit='Start time',ge=-24d0,                &
              maxVal=sesTbl%nSess,result2=hlpTim(2,:))

! Extract the end date
! ----------------------
  IF (sesTbl%fix) THEN
    CALL ckoptd(1,'LIST_OF_SESSIONS', hlpStr(4,:), srName, &
                'Session table',0,irCode,                  &
                colTit='end date',ge=-24d0,                &
                maxVal=sesTbl%nSess,result2=hlpTim(3,:))
  ELSE
    CALL ckoptd(1,'LIST_OF_SESSIONS', hlpStr(4,:), srName, &
                'Session table',0,irCode,                  &
                colTit='end date',empty=1d20,ge=-24d0,     &
                maxVal=sesTbl%nSess,result2=hlpTim(3,:))
  ENDIF

! Extract the start time
! ----------------------
  CALL ckoptt(1,'LIST_OF_SESSIONS', hlpStr(5,:), srName, &
              'Session table',0,irCode,                  &
              colTit='End time',ge=-24d0,                &
              maxVal=sesTbl%nSess,result2=hlpTim(4,:))

! Compute MJD for start and end epoch
! -----------------------------------
  IF (sesTbl%fix) THEN
    DO ii = 1,sesTbl%nSess
      sesTbl%sess(ii)%timint%t(1) = hlpTim(1,ii) + hlpTim(2,ii)/24d0
      sesTbl%sess(ii)%timint%t(2) = hlpTim(3,ii) + hlpTim(4,ii)/24d0

      IF (sesTbl%sess(ii)%timint%t(1) >= sesTbl%sess(ii)%timint%t(2)) THEN
        CALL timst2(1,2,sesTbl%sess(ii)%timint%t,epoStr)
        WRITE(lfnerr,'(/A,2(/,16X,A),/)')                             &
        ' *** SR RDSESS: Invalid session boundaries for session "' // &
                                       sesTbl%sess(ii)%sessID // '"', &
                        'Session table:  ' // TRIM(sesFil),           &
                        'Time interval:  ' // epoStr
        CALL exitrc(2)
      ENDIF
    ENDDO

! Time for open session definition
! --------------------------------
  ELSE
    DO ii = 1,sesTbl%nSess
      IF (hlpTim(1,ii) /= 1d20 .OR. hlpTim(3,ii) /= 1d20) THEN
        WRITE(lfnerr,'(/A,/,16X,A,/)')                                &
        ' *** SR RDSESS: A date was specified for session "' //       &
                                       sesTbl%sess(ii)%sessID // '"', &
                        'No date is allowed for open session definition'
        CALL exitrc(2)
      ENDIF

      sesTbl%sess(ii)%timint%t(1) = hlpTim(2,ii)/24d0
      sesTbl%sess(ii)%timint%t(2) = hlpTim(4,ii)/24d0

      IF (sesTbl%sess(ii)%timint%t(2) < sesTbl%sess(ii)%timint%t(1)) &
        sesTbl%sess(ii)%timint%t(2) = sesTbl%sess(ii)%timint%t(2)+1

    ENDDO
  ENDIF

! Deallocate memory
! -----------------
  DEALLOCATE(hlpStr,stat=iac)
  DEALLOCATE(hlpTim,stat=iac)
  DEALLOCATE(keyValue,stat=iac)

! Exit if input was not correct
! -----------------------------
  IF (irCode /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE rdsess

END MODULE
