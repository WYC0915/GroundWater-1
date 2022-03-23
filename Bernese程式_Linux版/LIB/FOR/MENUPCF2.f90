MODULE s_MENUPCF2
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menupcf2(keyWord,keys,pcf)

! -------------------------------------------------------------------------
! Purpose:    Extracts PCF content from uniline structure
!
! Author:     R. Dach
!
! Created:    07-Dec-2001
!
! Changes:    13-May-2002 RD: Handle empty PCFiles
!             04-Jun-2002 RD: Stop in the case of error in SPECIAL
!             28-Aug-2002 RD: Remove special entries from PARAMS before update
!             02-Oct-2002 RD: Statement corrected for SOLARIS 5.9
!             16-Oct-2002 RD: Index bug fixed
!             21-Feb-2003 RD: Set error instead of warning
!             23-Apr-2003 AJ: Nullify local pointers
!             26-Nov-2003 RD: Allow PRIORITY=0
!             06-Feb-2004 RD: Remove the length of the PCF-variables
!             10-Aug-2004 RD: Singleton-Flag instead of Priority
!             13-Dec-2007 RD: Add new special action "CONT_ERR"
!             14-Jun-2012 RD: Increase length of variable default
!             14-Jun-2012 RD: Use m_bern with only
!             20-Sep-2012 RD: Nullify pointer after deallocation
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, t_key, lfnerr, keyValueLength
  USE p_menaux, ONLY: qt
  USE p_bpe,    ONLY: t_pcf, maxpid, maxdsc, maxwat, kDescr, special, numVal, &
                      flgSingle

  USE s_dimtst
  USE s_alcerr
  USE s_ckoptu
  USE s_menupcf4
  USE s_exitrc
  USE s_ckoptc
  USE s_upperc
  USE s_ckopti
  USE s_ckoptl
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)                   :: keyWord  ! what to do
  TYPE(t_key), DIMENSION(:), POINTER :: keys     ! uniline structure
! output:
  TYPE(t_pcf)                        :: pcf      ! PCFile content

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER :: srname = 'menupcf2'

! Local Variables
! ---------------
  TYPE(t_pcf)                        :: pcf2   ! 2nd PCF structure

  CHARACTER(LEN=keyValueLength),DIMENSION(:,:),POINTER    :: hlpText
  CHARACTER(LEN=80),            DIMENSION(:),  ALLOCATABLE:: parmStr
  CHARACTER(LEN=80)                                       :: pidStr

  INTEGER(i4b),                 DIMENSION(:),  POINTER    :: idx
  INTEGER(i4b)                                            :: pid2
  INTEGER(i4b)                                            :: i1,i2
  INTEGER(i4b)                                            :: ii,jj,kk
  INTEGER(i4b)                                            :: irCode,irc

  NULLIFY(hlpText)
  NULLIFY(idx)

! Read the list of scripts, script parameters and special actions
! ---------------------------------------------------------------
  IF (keyWord == 'PCF_SPECIALS' .OR. keyWord == 'PCF_PARAMS' .OR. &
      keyWord == 'PCF_SAVE'                                  ) THEN

! Get the number of scripts in the file
! -------------------------------------
    pcf%n_Pid = SIZE(keys(1)%value)

    IF (pcf%n_Pid == 1) THEN
      IF (keys(1)%value(1)(1:2) == qt // qt) pcf%n_Pid = 0
    ENDIF

    CALL dimtst(1,2,2,srname,'maxpid','Number of BPE scripts', &
                'It is defined in MAXBPE.inc',pcf%n_Pid,maxpid,irc)

! Extract information from the list of scripts
! --------------------------------------------
    IF (pcf%n_Pid > 0) THEN
      ALLOCATE(hlpText(numVal(1),pcf%n_Pid),stat=irc)
      CALL alcerr(irc,'hlpText',(/ numVal(1),pcf%n_Pid /),srName)

      irCode = 0
      CALL ckoptu(1,keys(1)%name,keys(1)%value,srName,                     &
                  kDescr(1),0,irCode,numVal(1),                            &
                  maxVal=pcf%n_Pid,result2=hlpText)
      IF (irCode /= 0) CALL exitrc(2)

      ! Read PIDs
      CALL ckopti(1,keys(1)%name,hlpText(1,:),srName,kDescr(1),0,irCode,   &
                  ge=1,le=999,colTit='Process ID',maxVal=pcf%n_Pid,        &
                  init=0,error=0,result2=pcf%job(:)%iPid)

      ! Read Script names
      CALL ckoptl(1,keys(1)%name,hlpText(2,:),srname,kDescr(1),0,irCode,   &
                  maxLength=8,colTit='Script name',maxVal=pcf%n_Pid,       &
                  result2=pcf%job(:)%script)

      ! Read option directory
      CALL ckoptl(1,keys(1)%name,hlpText(3,:),srName,kDescr(1),0,irCode,   &
                  maxLength=8,colTit='Option directory',maxVal=pcf%n_Pid,  &
                  result2=pcf%job(:)%option)

      ! special campaign
      CALL ckoptl(1,keys(1)%name,hlpText(4,:),srName,kDescr(1),0,irCode,   &
                  maxLength=8,colTit='Campaign',maxVal=pcf%n_Pid,          &
                  empty='',result2=pcf%job(:)%camp)

      ! CPU name
      CALL ckoptl(1,keys(1)%name,hlpText(5,:),srName,kDescr(1),0,irCode,   &
                  maxLength=8,colTit='CPU name',maxVal=pcf%n_Pid,          &
                  empty='ANY',result2=pcf%job(:)%cpu)

      ! Singleton-flag
      CALL ckoptl(1,keys(1)%name,hlpText(6,:),srName,kDescr(1),0,irCode,   &
                  maxLength=1,colTit='Priority',maxVal=pcf%n_Pid,          &
                  empty=' ',result2=pcf%job(:)%jFlags)

      ! Read wait PIDs
      DO ii = 7,6+maxwat
        IF (ii > numVal(1)) EXIT
        CALL ckopti(1,keys(1)%name,hlpText(ii,:),srName,kDescr(1),0,irCode,&
                    empty=0,ge=1,le=999,colTit='Wait PID',maxVal=pcf%n_Pid,&
                    init=0,error=0,result2=pcf%job(:)%iWait(ii-6))
      ENDDO

      IF (irCode /= 0) CALL exitrc(2)

      DEALLOCATE(hlpText,stat=irc)

      ! Count the wait numbers
      DO ii = 1,pcf%n_Pid
        pcf%job(ii)%n_wait = SIZE(pcf%job(ii)%iWait)
        DO WHILE (pcf%job(ii)%n_wait > 0)
          IF (pcf%job(ii)%iWait(pcf%job(ii)%n_wait) == 0) THEN
            pcf%job(ii)%n_wait = pcf%job(ii)%n_wait-1
          ELSE
            EXIT
          ENDIF
        ENDDO
      ENDDO

      ! Check PIDs (RD: Not necessary for the new BPE)
!      irCode = 0
!      DO ii = 1,pcf%n_Pid-1
!        IF (pcf%job(ii)%iPid >= pcf%job(ii+1)%iPid) THEN
!          IF (irCode == 0) WRITE(lfnerr,'(/,A)') &
!                ' *** SR MENUPCF2: The PIDs must increase job by job:'
!          WRITE(lfnerr,'(18X,2(I3.3,2(1X,A),A))')                       &
!                pcf%job(ii)%iPid,pcf%job(ii)%script,pcf%job(ii)%option, &
!                ' <-> ',                                                &
!                pcf%job(ii+1)%iPid,pcf%job(ii+1)%script,pcf%job(ii+1)%option
!          irCode = irCode + 1
!        ENDIF
!      ENDDO
!      IF (irCode /= 0) CALL exitrc(2)

      ! Check wait PIDs
      irCode = 0
      DO ii = 1, pcf%n_Pid
        DO jj = 1, maxwat
          IF (pcf%job(ii)%iWait(jj) == 0) CYCLE
          i1 = 0
          DO i2 = 1, pcf%n_Pid
            IF (pcf%job(i2)%iPid == pcf%job(ii)%iWait(jj)) THEN
              i1 = i2
              EXIT
            ENDIF
          ENDDO
          ! Something is wrong...
          IF (i1 == 0 .OR. i1 == ii) THEN
            IF (irCode == 0) WRITE(lfnerr,'(/,A)') &
                  ' *** SR MENUPCF2: Invalid wait PID detected for'
            IF (i1 ==  0) WRITE(lfnerr,'(18X,I3.3,2(1X,A),A,I3.3,A)')       &
                  pcf%job(ii)%iPid,pcf%job(ii)%script,pcf%job(ii)%option,   &
                  ': ',pcf%job(ii)%iWait(jj),' (The wait PID does not exist.)'
            IF (i1 == ii) WRITE(lfnerr,'(18X,I3.3,2(1X,A),A,I3.3,A)')       &
                  pcf%job(ii)%iPid,pcf%job(ii)%script,pcf%job(ii)%option,   &
                  ': ',pcf%job(ii)%iWait(jj),' (The Process waits for itself.)'
            irCode = irCode + 1
          ENDIF
        ENDDO
      ENDDO
      IF (irCode /= 0) CALL exitrc(2)

      ! Check Singleton-Flags
      DO ii=1, pcf%n_Pid
        CALL upperc(pcf%job(ii)%jFlags)
      ENDDO

      DO ii=1, pcf%n_Pid
        IF (pcf%job(ii)%jFlags /= flgSingle) THEN
          i1 = ii
          EXIT
        ENDIF
      ENDDO

      DO ii=pcf%n_Pid,1,-1
        IF (pcf%job(ii)%jFlags /= flgSingle) THEN
          i2 = ii
          EXIT
        ENDIF
      ENDDO

      IF (i1 > 2) THEN
        WRITE(lfnerr,'(/,A,/,18X,A)') &
        ' *** SR MENUPCF2: Only one singleton-script is permitted at the ',     &
             'begin of the PCF. '
        i1=2
        irCode = irCode+1
      ENDIF

      DO ii=i1,i2
        IF (pcf%job(ii)%jFlags == flgSingle) THEN
          IF (irCode == 0) WRITE(lfnerr,'(/,A,/,18X,A)') &
             ' *** SR MENUPCF2: Singleton-Flag is permitted at the ',     &
             'begin or end of the PCF only. '
          WRITE(lfnerr,'(18X,I3.3,2(1X,A),A,A1)')                         &
                pcf%job(ii)%iPid,pcf%job(ii)%script,pcf%job(ii)%option,   &
                ': ',pcf%job(ii)%jFlags
          irCode = irCode+1
        ENDIF
      ENDDO
      IF (irCode /= 0) CALL exitrc(2)
    ENDIF

  ENDIF

! Read the list of scripts, and special actions
! ---------------------------------------------
  IF (keyWord == 'PCF_PARAMS' .OR. keyWord == 'PCF_SAVE') THEN

! Extract information for parameters
! ----------------------------------
    pcf2%n_Pid = SIZE(keys(3)%value)

    IF (pcf2%n_Pid == 1) THEN
      IF (keys(3)%value(1)(1:2) == qt // qt) pcf2%n_Pid = 0
    ENDIF


    CALL dimtst(1,2,2,srName,'MAXPID','number of BPE scripts', &
                'It is defined in MAXBPE.inc',pcf2%n_Pid,maxpid,irc)

    DO ii = 1,SIZE(pcf%job)
      pcf%job(ii)%params = ' '
    ENDDO

    IF (pcf%n_Pid > 0 .AND. pcf2%n_Pid > 0) THEN

      ALLOCATE(hlpText(numVal(3),pcf2%n_Pid),stat=irc)
      CALL alcerr(irc,'hlpText',(/ numVal(3),pcf2%n_Pid /),srName)

      irCode = 0
      CALL ckoptu(1,keys(3)%name,keys(3)%value,srname,kDescr(3),0,irCode,   &
                  numVal(3),maxVal=pcf2%n_Pid,result2=hlpText)
      IF (irc /= 0) CALL exitrc(2)

      ! Process ID
      CALL ckopti(1,keys(3)%name,hlpText(1,:),srName,kDescr(3),0,irCode,    &
                  ge=1,le=999,colTit='Process ID',maxVal=pcf2%n_Pid,        &
                  init=0,error=0,result2=pcf2%job(:)%iPid)

      ! Script name
      CALL ckoptl(1,keys(3)%name,hlpText(2,:),srName,kDescr(3),0,irCode,    &
                  maxLength=8,colTit='Script name',maxVal=pcf2%n_Pid,       &
                  result2=pcf2%job(:)%script)

      ! Option directory
      CALL ckoptl(1,keys(3)%name,hlpText(3,:),srname,kDescr(3),0,irCode,    &
                  maxLength=8,colTit='Option directory',maxVal=pcf2%n_Pid,  &
                  result2=pcf2%job(:)%option)

      ! List of parameters (using a buffer)
      ALLOCATE(parmStr(pcf2%n_Pid),stat=irc)
      CALL alcerr(irc,'parmStr',(/pcf2%n_Pid/),srName)

      DO ii = 4,3+SIZE(pcf%job(1)%params)
        IF (ii > numVal(3)) EXIT
        CALL ckoptl(1,keys(3)%name,hlpText(ii,:),srName,kDescr(3),0,irCode, &
                    maxLength=8,colTit='Parameters',maxVal=pcf2%n_Pid,      &
                    empty='',init='',error='',result2=parmStr)
        pcf2%job(1:pcf2%n_Pid)%params(ii-3)=parmStr(1:pcf2%n_Pid)
      ENDDO

      DEALLOCATE(parmStr,stat=irc)

      IF (irc /= 0) CALL exitrc(2)

      DO ii = 1,pcf2%n_Pid
        DO jj = 1,SIZE(special)
          IF (jj == 2) CYCLE
          IF (pcf2%job(ii)%params(1) == special(jj)) THEN
            pcf%job(ii)%pType = jj-2
            EXIT
          ENDIF
        ENDDO
      ENDDO

      CALL menupcf4(keyWord,pcf,pcf2,idx)

      DO ii=1,pcf%n_Pid
        IF (Idx(ii) == 0) CYCLE
        pcf%job(ii)%params(:) = pcf2%job(idx(ii))%params(:)
      ENDDO

! Remove old special actions from parameter part
      DO ii=1,pcf%n_Pid

        ! Remove old "SKIP" from parameter list
        IF (pcf%job(ii)%params(1) == special(1)) pcf%job(ii)%params(1) = ' '

        ! Remove old "PARALLEL" from parameter list
        IF (pcf%job(ii)%params(1) == special(3)) THEN

          i1 = 0
          DO kk=ii-1,1,-1
            IF (pcf%job(ii)%params(2) == pcf%job(kk)%params(1)) THEN
              IF (i1 == 0) THEN
                i1 = kk
              ELSE
                i1 = 0
                EXIT
              ENDIF
            ENDIF
          ENDDO

          IF (i1 /= 0) THEN
            i2 = 0
            DO kk=i1+1,pcf%n_Pid
              IF (kk==ii) CYCLE
              IF (pcf%job(ii)%params(2) == pcf%job(kk)%params(2)) THEN
                i2 = kk
                EXIT
              ENDIF
            ENDDO

            IF (i2 == 0) THEN
              pcf%job(i1)%params(1) = ' '
            ELSE
              DO kk=i2-1,i1+1,-1
                IF (pcf%job(ii)%params(2) == pcf%job(kk)%params(1)) THEN
                  pcf%job(i1)%params(1) = ' '
                  EXIT
                ENDIF
              ENDDO
            ENDIF
          ENDIF


          pcf%job(ii)%params(:) = ' '

        ENDIF ! Remove old "PARALLEL"

        ! Remove old "NEXTJOB" from parameter list
        IF (pcf%job(ii)%params(1) == special(4)) THEN

          pcf%job(ii)%params(1) = ' '

          DO kk=1,pcf%n_Pid
            pidStr=' '
            WRITE(pidStr,'(I3.3)') pcf%job(kk)%iPid

            DO jj=2,SIZE(pcf%job(ii)%params)
              IF (pidStr == pcf%job(ii)%params(jj)) &
                pcf%job(ii)%params(jj) = ' '
            ENDDO
          ENDDO

          DO kk=1,pcf2%n_Pid
            pidStr=' '
            WRITE(pidStr,'(I3.3)') pcf2%job(kk)%iPid

            DO jj=2,SIZE(pcf%job(ii)%params)
              IF (pidStr == pcf%job(ii)%params(jj)) &
                pcf%job(ii)%params(jj) = ' '
            ENDDO
          ENDDO

        ENDIF ! Remove old "NEXTJOB"

      ENDDO


      DEALLOCATE(idx,stat=irc)
      NULLIFY(idx)

      DEALLOCATE(hlpText,stat=irc)
    ENDIF

  ENDIF

! Read the list of scripts, script parameters and special actions
! ---------------------------------------------------------------
  IF (keyWord == 'PCF_SPECIALS' .OR. keyWord == 'PCF_PARAMS' .OR. &
      keyWord == 'PCF_SAVE'                                  ) THEN

! Extract information for special actions
! ---------------------------------------
    pcf2%n_Pid = SIZE(keys(2)%value)

    IF (pcf2%n_pid == 1) THEN
      IF (keys(2)%value(1)(1:2) == qt // qt) pcf2%n_Pid = 0
    ENDIF

    CALL dimtst(1,2,2,srName,'maxpid','number of BPE SCRIPTS', &
                'It is defined in MAXBPE.inc',pcf2%n_Pid,maxpid,irc)

    IF (pcf%n_Pid > 0 .AND. pcf2%n_Pid > 0) THEN

      ALLOCATE(hlpText(numVal(2),pcf2%n_Pid),stat=irc)
      CALL alcerr(irc,'hlpText',(/ numVal(2),pcf2%n_Pid /),srname)

      irCode = 0
      CALL ckoptu(1,keys(2)%name,keys(2)%value,srName,kDescr(2),0,irCode, &
                  numVal(2),maxVal=pcf2%n_Pid,result2=hlpText)
      IF (irc /= 0) CALL exitrc(2)

      ! Process ID
      CALL ckopti(1,keys(2)%name,hlpText(1,:),srName,kDescr(2),0,irCode,  &
                  ge=1,le=999,colTit='Process ID',maxVal=pcf2%n_Pid,      &
                  init=0,error=0,result2=pcf2%job(:)%iPid)

      ! Script name
      CALL ckoptl(1,keys(2)%name,hlpText(2,:),srName,kDescr(2),0,irCode,  &
                  maxLength=8,colTit='Script name',maxVal=pcf2%n_Pid,     &
                  result2=pcf2%job(:)%script)

      ! Option directory
      CALL ckoptl(1,keys(2)%name,hlpText(3,:),srName,kDescr(2),0,irCode,  &
                  maxLength=8,colTit='Option directory',maxVal=pcf2%n_Pid,&
                  result2=pcf2%job(:)%option)

      ! Process type
      CALL ckoptc(1,keys(2)%name,hlpText(4,:),special,srName,kDescr(2),   &
                  0,irCode,colTit='Action descr.',maxVal=pcf2%n_Pid,      &
                  init=0,error=0,result2=pcf2%job(:)%pType)
      IF (irCode /= 0) CALL exitrc(2)

      pcf2%job(:)%pType = pcf2%job(:)%pType-2

      ! Put all special actions into PCF2-parameters
      DO ii=1,pcf2%n_pid
        pcf2%job(ii)%params = ' '

        ! Skip
        IF (pcf2%job(ii)%pType == -1) pcf2%job(ii)%params(1) = special(1)

        ! Parallel
        IF (pcf2%job(ii)%pType ==  1) THEN
          pcf2%job(ii)%params(1) = special(3)
          pcf2%job(ii)%params(2) = hlpText(5,ii)
          irc = 0
          CALL ckopti(1,keys(2)%name,(/hlpText(6,ii)/),srName,kDescr(2), &
                      0,irCode,ge=1,le=999,colTit='Master PID',maxVal=1, &
                      result1=Pid2)
          IF (irc /= 0) CYCLE

          i1 = 0
          DO i2 = 1, pcf2%n_pid
            IF (pcf2%job(i2)%iPid == pid2) THEN
              i1 = i2
              IF (i2 >= ii) EXIT
              pcf2%job(i2)%params(1) = hlpText(5,ii)
              EXIT
            ENDIF
          ENDDO
          ! The master script was added in the first panel
          IF (i1 == 0) THEN
            DO i2=1,pcf%n_pid
              IF (pcf%job(i2)%iPid == pid2) THEN
                pcf%job(i2)%params(1) = hlpText(5,ii)
                i1 = -i2
                EXIT
              ENDIF
            ENDDO
          ENDIF
          ! Something is wrong...
          IF (i1 == 0 .OR. i1 >= ii) THEN
            IF (irCode == 0) WRITE(lfnerr,'(/,A)') &
                  ' *** SR MENUPCF2: Invalid PID for special actions ' //   &
                                                           'detected for'
            IF (i1 ==  0) WRITE(lfnerr,'(18X,I3.3,2(1X,A),A,I3.3,A)')       &
                  pcf2%job(ii)%iPid,pcf2%job(ii)%script,pcf2%job(ii)%option,&
                  ': ',pid2,' (PID for parallel master does not exist.)'
            IF (i1 == ii) WRITE(lfnerr,'(18X,I3.3,2(1X,A),A,I3.3,A)')       &
                  pcf2%job(ii)%iPid,pcf2%job(ii)%script,pcf2%job(ii)%option,&
                  ': ',pid2,' (A parallel script cannot be the master.)'
            IF (i1 >  ii) WRITE(lfnerr,'(18X,I3.3,2(1X,A),A,I3.3,A)')       &
                  pcf2%job(ii)%iPid,pcf2%job(ii)%script,pcf2%job(ii)%option,&
                  ': ',pid2,' (The master must be before the parallel script.)'
            irCode = irCode + 1
          ENDIF
        ENDIF

        ! Nextjob
        IF (pcf2%job(ii)%pType ==  2) THEN
          pcf2%job(ii)%params(1) = special(4)
          DO kk = 2,SIZE(pcf2%job(ii)%params)
            IF (kk+5 > numVal(2)) EXIT

            irc = 0
            CALL ckopti(1,keys(2)%name,(/hlpText(5+kk,ii)/),srName,kDescr(2),&
                      0,irCode,ge=1,le=999,colTit='Next job PID',maxVal=1,   &
                      empty=0,result1=Pid2)
            IF (irc /= 0) CYCLE

            IF (Pid2 == 0) CYCLE

            i1 = 0
            DO i2 = 1, pcf2%n_pid
              IF (pcf2%job(i2)%iPid == pid2) THEN
                i1 = i2
                pcf2%job(ii)%params(kk) = ' '
                WRITE(pcf2%job(ii)%params(kk),'(I3.3)') pid2
                EXIT
              ENDIF
            ENDDO

            ! Something is wrong...
            IF (i1 == 0) THEN
              IF (irCode == 0) WRITE(lfnerr,'(/,A)') &
                  ' *** SR MENUPCF2: Invalid PID for special actions ' //   &
                                                           'detected for'
              WRITE(lfnerr,'(18X,I3.3,2(1X,A),A,I3.3,A)')                   &
                  pcf2%job(ii)%iPid,pcf2%job(ii)%script,pcf2%job(ii)%option,&
                  ': ',pid2,' (The next job PID does not exist.)'
              irCode = irCode + 1
            ENDIF
          ENDDO
        ENDIF

        ! Continue in case of an error
        IF (pcf2%job(ii)%pType == 3) pcf2%job(ii)%params(1) = special(5)

      ENDDO
      IF (irCode /= 0) CALL exitrc(2)

      CALL menupcf4(keyWord,pcf,pcf2,idx)

      ! Put new special actions into the parameter list
      irCode = 0
      DO ii=1,pcf%n_Pid

        IF (idx(ii) == 0) CYCLE

        ! Remove a parallel request if the master was deleted
        IF (pcf2%job(idx(ii))%pType == 1) THEN
          i1 = 0
          DO jj = idx(ii),1,-1
            IF (pcf2%job(idx(ii))%params(2) == pcf2%job(jj)%params(1)) THEN
              DO kk=1,ii-1
                IF (idx(kk) == jj) i1 = 1
              ENDDO
            ENDIF
          ENDDO
          DO jj = ii,1,-1
            IF (pcf2%job(idx(ii))%params(2) == pcf%job(jj)%params(1)) i1 = 1
          ENDDO
          IF (i1 == 0) CYCLE
        ENDIF

        pcf%job(ii)%pType = pcf2%job(idx(ii))%pType

        DO jj=1,SIZE(pcf%job(ii)%params)
          IF (LEN_TRIM(pcf2%job(idx(ii))%params(jj)) > 0) &
            pcf%job(ii)%params(jj) = pcf2%job(idx(ii))%params(jj)
        ENDDO
      ENDDO

      DEALLOCATE(idx,stat=irc)
      NULLIFY(idx)

      DEALLOCATE(hlpText,stat=irc)
    ENDIF

  ENDIF

! Read the variable list
! ----------------------
  IF (keyWord == 'PCF_SAVE') THEN

! Get the number of scripts in the file
! -------------------------------------
    pcf%nVar = SIZE(keys(4)%value)

    IF (pcf%nVar == 1) THEN
      IF (keys(4)%value(1)(1:2) == qt // qt) pcf%nVar = 0
    ENDIF

    CALL dimtst(1,2,2,srname,'maxdsc','number of PCF variables', &
                'It is defined in MAXBPE.inc',pcf%nVar,maxdsc,irc)

! Extract information from the list of scripts
! --------------------------------------------
    IF (pcf%nVar > 0) THEN
      ALLOCATE(hlpText(numVal(4),pcf%nVar),stat=irc)
      CALL alcerr(irc,'hlpText',(/ numVal(4),pcf%nVar /),srName)

      irCode = 0
      CALL ckoptu(1,keys(4)%name,keys(4)%value,srName,                     &
                  kDescr(4),0,irCode,numVal(4),                            &
                  maxVal=pcf%nVar,result2=hlpText)
      IF (irCode /= 0) CALL exitrc(2)

      ! Read variable name
      CALL ckoptl(1,keys(4)%name,hlpText(1,:),srName,kDescr(4),0,irCode,   &
                  maxLength=8,colTit='Variable name',maxVal=pcf%nVar,      &
                  result2=pcf%var(:)%varnam)

      ! Read variable default value
      CALL ckoptl(1,keys(4)%name,hlpText(2,:),srname,kDescr(4),0,irCode,   &
                  maxLength=32,colTit='Default value',maxVal=pcf%nVar,     &
                  empty=' ',result2=pcf%var(:)%vardef)

      ! Read variable description
      CALL ckoptl(1,keys(4)%name,hlpText(3,:),srName,kDescr(4),0,irCode,     &
                  maxLength=40,colTit='Variable description',maxVal=pcf%nVar,&
                  empty=' ',result2=pcf%var(:)%vardsc)

      ! Varaiable name starts with V_
      DO ii = 1,pcf%nVar
        IF (LEN_TRIM(pcf%var(ii)%varnam) == 0) CYCLE
        IF (pcf%var(ii)%varnam(1:2) == 'v_') pcf%var(ii)%varnam(1:2) =  'V_'
        IF (pcf%var(ii)%varnam(1:2) /= 'V_') THEN
          WRITE(lfnerr,'(/,A,/,18X,A,A,/,18X,A,I5,/)')                &
          ' *** SR MENUPCF2: Variable names have to start with V_',   &
                            'Variable name: ',pcf%var(ii)%varnam,     &
                            'Line number:',ii
          irCode = irCode + 1
        ENDIF
      ENDDO

      IF (irCode /= 0) CALL exitrc(2)

      DEALLOCATE(hlpText,stat=irc)

    ENDIF

! Get back the comment lines from PCFile
! --------------------------------------
    pcf%nTxt = SIZE(keys(5)%value)

    IF (pcf%nTxt == 1) THEN
      IF (keys(5)%value(1)(1:2) == qt // qt) pcf%nTxt = 0
    ENDIF

    ALLOCATE(pcf%txt(pcf%nTxt),stat=irc)
    CALL alcerr(irc,'pcf%txt',(/ pcf%nTxt /),srName)

! Extract information from the list of scripts
! --------------------------------------------
    IF (pcf%nTxt > 0) THEN
      ALLOCATE(hlpText(4,pcf%nTxt),stat=irc)
      CALL alcerr(irc,'hlpText',(/ 4,pcf%nTxt /),srName)

      irCode = 0
      CALL ckoptu(1,keys(5)%name,keys(5)%value,srName,                     &
                  'PCF comment text',0,irCode,4,                           &
                  maxVal=pcf%nTxt,result2=hlpText)
      IF (irCode /= 0) CALL exitrc(2)

      ! Section
      CALL ckopti(1,keys(5)%name,hlpText(1,:),srName,                      &
                  'PCF comment text',0,irCode,                             &
                  ge=0,le=3,colTit='Section',maxVal=pcf%nTxt,              &
                  init=0,error=0,result2=pcf%txt(:)%section)

      ! PID before comment
      CALL ckopti(1,keys(5)%name,hlpText(2,:),srName,                      &
                  'PCF comment text',0,irCode,                             &
                  ge=0,le=999,colTit='PID before',maxVal=pcf%nTxt,         &
                  init=0,error=0,result2=pcf%txt(:)%pids(1))

      ! PID behind comment
      CALL ckopti(1,keys(5)%name,hlpText(3,:),srName,                      &
                  'PCF comment text',0,irCode,                             &
                  ge=0,le=999,colTit='PID behind',maxVal=pcf%nTxt,         &
                  init=0,error=0,result2=pcf%txt(:)%pids(2))

      ! Comment text
      CALL ckoptl(1,keys(5)%name,hlpText(4,:),srName,                      &
                  'PCF comment text',0,irCode,                             &
                  maxLength=120,colTit='Comment text',maxVal=pcf%nTxt,     &
                  empty=' ', result2=pcf%txt(:)%line)

      IF (irCode /= 0) CALL exitrc(2)

      DEALLOCATE(hlpText,stat=irc)

    ENDIF
  ENDIF


END SUBROUTINE menupcf2

END MODULE
