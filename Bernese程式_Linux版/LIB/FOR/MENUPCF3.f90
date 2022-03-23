MODULE s_MENUPCF3
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menupcf3(pcfFil,pcf,keys)

! -------------------------------------------------------------------------
! Purpose:    Write PCF information into unilines
!
! Author:     R. Dach
!
! Created:    07-Dec-2001
! Last mod.:  13-Dec-2007
!
! Changes:    18-Jan-2002  RD: Avoid parameter entries for parallel scripts
!             13-May-2002  RD: Use keywords from MENUAUX.INP
!             28-Aug-2002  RD: Check the number of parallel file names
!             21-Feb-2003  RD: Set error instead of warning
!             17-Nov-2003  RD: No TRIM of a parameter string ("special")
!             10-Aug-2004  RD: Singleton-Flag instead of Priority
!             13-Dec-2007  RD: Add new special action "CONT_ERR"
!
! SR called:  lengt0, alcerr
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE p_menaux, ONLY: qt, q2
  USE p_bpe,    ONLY: t_pcf, maxwat, kWords, special, numVal

  USE s_alcerr
  USE s_exitrc
  USE f_lengt0
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)                       :: pcfFil  ! pcf file name
  TYPE(t_pcf)                            :: pcf     ! PCFile content to write
! output:
  TYPE(t_key),      DIMENSION(:),POINTER :: keys    ! unilines

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER :: srName = 'menupcf3'

! Local Variables
! ---------------

  CHARACTER(LEN=keyValueLength)                           :: hlpStr

  INTEGER(i4b)                                            :: mPid, mVar, mTxt
  INTEGER(i4b)                                            :: iPar, jPar
  INTEGER(i4b)                                            :: pid2
  INTEGER(i4b)                                            :: ii,jj,i1
  INTEGER(i4b)                                            :: irc,ios


! Allocate and init the keywords
! ------------------------------
  mPid = pcf%n_Pid
  IF (pcf%n_Pid == 0) mPid = 1
  mVar = pcf%nVar
  IF (pcf%nVar == 0)  mVar = 1
  mTxt = pcf%nTxt
  IF (pcf%nTxt == 0)  mTxt = 1

  ALLOCATE(keys(7),stat=irc)
  CALL alcerr(irc,'keys',(/7/),srName)

  keys(1)%name = kWords(1)
  ALLOCATE(keys(1)%value(mPid))
  CALL alcerr(irc,'keys(1)%value',(/mPid/),srName)

  keys(2)%name = kWords(2)
  ALLOCATE(keys(2)%value(mPid))
  CALL alcerr(irc,'keys(2)%value',(/mPid/),srName)

  keys(3)%name = kWords(3)
  ALLOCATE(keys(3)%value(mPid))
  CALL alcerr(irc,'keys(3)%value',(/mPid/),srName)

  keys(4)%name = kWords(4)
  ALLOCATE(keys(4)%value(mVar))
  CALL alcerr(irc,'keys(4)%value',(/mVar/),srName)

  keys(5)%name = 'ADDITIONAL_INFO'
  ALLOCATE(keys(5)%value(mTxt))
  CALL alcerr(irc,'keys(5)%value',(/mTxt/),srName)

  keys(6)%name = 'PCFFILRS'
  ALLOCATE(keys(6)%value(1))
  CALL alcerr(irc,'keys(6)%value',(/1/),srName)
  keys(6)%value(1) = pcfFil

  keys(7)%name = 'FILNAM'
  ALLOCATE(keys(7)%value(1))
  CALL alcerr(irc,'keys(7)%value',(/1/),srName)
  keys(7)%value(1) = pcfFil

! A new file was created
! ----------------------
  IF (pcf%n_Pid == 0) THEN

    DO ii = 1,3
      IF (ii > SIZE(keys)) EXIT
      keys(ii)%value(1) = ''
      DO jj = 1,numVal(ii)
        hlpStr = TRIM(keys(ii)%value(1)) // ' ' // qt // qt
        keys(ii)%value(1) = hlpStr
      ENDDO
    ENDDO

  ELSE

! Loop all PIDs
! -------------
    DO ii = 1,pcf%n_Pid

! Write the part with scripts
! ---------------------------
      keys(1)%value(ii) = ''

      WRITE(keys(1)%value(ii),'(A,I3.3,A,4(1X,3A),1X,A,A1,A)') &
            qt,      pcf%job(ii)%ipid,    qt, &
            qt, TRIM(pcf%job(ii)%script), qt, &
            qt, TRIM(pcf%job(ii)%option), qt, &
            qt, TRIM(pcf%job(ii)%camp),   qt, &
            qt, TRIM(pcf%job(ii)%cpu),    qt, &
            qt,      pcf%job(ii)%jFlags,  qt

      DO jj = 1,maxwat

        IF (jj + 5 > numVal(1)) EXIT

        IF (pcf%job(ii)%iWait(jj) == 0) THEN
          hlpStr = TRIM(keys(1)%value(ii)) // ' ' // qt // qt
        ELSE
          hlpStr = ''
          WRITE(hlpStr,'(A,1X,A,I3.3,A)') &
                TRIM(keys(1)%value(ii)),qt,pcf%job(ii)%iWait(jj),qt
        ENDIF
        keys(1)%value(ii) = hlpStr

      ENDDO

! Write the part with the special actions
! ---------------------------------------
      keys(2)%value(ii) = ''

      ! Special action: skip
      IF (pcf%job(ii)%pType == -1) THEN
        hlpStr = special(1)
        WRITE(keys(2)%value(ii),'(A,I3.3,A,3(1X,3A),10(1X,2A))') &
              qt, pcf%job(ii)%ipid,           qt, &
              qt, TRIM(pcf%job(ii)%script),   qt, &
              qt, TRIM(pcf%job(ii)%option),   qt, &
              qt, TRIM(hlpStr),               qt, &
              (qt,qt,jj=1,numVal(2)-4)

      ! Special action: parallel
      ELSE IF (pcf%job(ii)%pType == 1) THEN
        DO jj = ii-1,0,-1
          IF (jj == 0) EXIT
          IF (pcf%job(ii)%params(2) == pcf%job(jj)%params(1)) EXIT
        ENDDO
        IF (jj > 0) THEN
          hlpStr = special(3)
          WRITE(keys(2)%value(ii),'(A,I3.3,A,4(1X,3A),1X,A,I3.3,A,8(1X,2A))') &
                qt, pcf%job(ii)%ipid,            qt, &
                qt, TRIM(pcf%job(ii)%script),    qt, &
                qt, TRIM(pcf%job(ii)%option),    qt, &
                qt, TRIM(hlpStr),                qt, &
                qt, TRIM(pcf%job(ii)%params(2)), qt, &
                qt, pcf%job(jj)%ipid,            qt, &
               (qt,qt,jj=1,numVal(2)-6)
        ELSE
          WRITE(lfnerr,'(/,A,/,18X,I3.3,1X,A,1X,A)')                          &
          ' *** SR MENUPCF3: Any parallel script needs a valid master script',&
                pcf%job(ii)%ipid,pcf%job(ii)%script,pcf%job(ii)%option
           CALL exitrc(2)
        ENDIF

      ! Special action: nexjob
      ELSE IF (pcf%job(ii)%pType ==2) THEN
        hlpStr = special(4)
        WRITE(keys(2)%value(ii),'(A,I3.3,A,3(1X,3A),2(1X,2A))') &
              qt, pcf%job(ii)%ipid,           qt, &
              qt, TRIM(pcf%job(ii)%script),   qt, &
              qt, TRIM(pcf%job(ii)%option),   qt, &
              qt, TRIM(hlpStr),               qt, &
             (qt,qt,jj=1,2)
        iParLoop: DO iPar = 2, SIZE(pcf%job(ii)%params)
          IF (iPar + 4 >= numVal(2)) EXIT
          READ(pcf%job(ii)%params(iPar),*,iostat=ios) pid2
          IF (ios == 0) THEN
            DO jj = 1, pcf%n_pid
              IF (pcf%job(jj)%iPid == pid2) THEN
                hlpStr = ' '
                WRITE(hlpStr,'(A,1X,A,I3.3,A)') &
                TRIM(keys(2)%value(ii)),qt,pcf%job(jj)%iPid,qt
                keys(2)%value(ii) = hlpStr
                CYCLE iParLoop
              ENDIF
            ENDDO
          ENDIF
          hlpStr = TRIM(keys(2)%value(ii)) // ' ' // qt // qt
          keys(2)%value(ii) = hlpStr
        ENDDO iParLoop

      ! Continuein case of an error
      ELSE IF (pcf%job(ii)%pType == 3) THEN
        hlpStr = special(5)
        WRITE(keys(2)%value(ii),'(A,I3.3,A,3(1X,3A),10(1X,2A))') &
              qt, pcf%job(ii)%ipid,           qt, &
              qt, TRIM(pcf%job(ii)%script),   qt, &
              qt, TRIM(pcf%job(ii)%option),   qt, &
              qt, TRIM(hlpStr),               qt, &
              (qt,qt,jj=1,numVal(2)-4)

      ! Normal job
      ELSE
        WRITE(keys(2)%value(ii),'(A,I3.3,A,2(1X,3A),11(1X,2A))') &
              qt, pcf%job(ii)%ipid,         qt, &
              qt, TRIM(pcf%job(ii)%script), qt, &
              qt, TRIM(pcf%job(ii)%option), qt, &
             (qt,qt,jj=1,numVal(2)-3)
      ENDIF

! Write the part with the parameters
! ----------------------------------
      keys(3)%value(ii) = ''
      WRITE(keys(3)%value(ii),'(A,I3.3,A,2(1X,3A))') &
            qt, pcf%job(ii)%ipid,         qt, &
            qt, TRIM(pcf%job(ii)%script), qt, &
            qt, TRIM(pcf%job(ii)%option), qt

      jParLoop: DO jPar = 1,SIZE(pcf%job(ii)%params)
        IF (jPar + 3 > numVal(3)) EXIT
        hlpStr = ' '

        ! Parallel: Tmp-file
        IF (jPar >= 3 .AND. pcf%job(ii)%pType == 1) THEN
          hlpStr = TRIM(keys(3)%value(ii)) // ' ' // qt//q2 // q2//qt

        ! Empty parameters
        ELSE IF (lengt0(pcf%job(ii)%params(jPar)) == 0) THEN
          hlpStr = TRIM(keys(3)%value(ii)) // ' ' // qt // qt

        ! Keywords
        ELSE IF (jPar == 1 .AND. pcf%job(ii)%pType /= 0) THEN
          hlpStr = TRIM(keys(3)%value(ii)) // ' ' // &
                   qt//q2 // TRIM(special(pcf%job(ii)%pType+2)) // q2//qt

        ! Parallel: Tmp-file
        ELSE IF (jPar == 2 .AND. pcf%job(ii)%pType == 1) THEN
          hlpStr = TRIM(keys(3)%value(ii)) // ' ' // &
                   qt//q2 // TRIM(pcf%job(ii)%params(jPar)) // q2//qt

        ! NEXTJOB numbers
        ELSE IF (pcf%job(ii)%pType == 2)  THEN
          READ(pcf%job(ii)%params(jPar),*,iostat=ios) pid2
          IF (ios == 0) THEN
            DO jj = 1,pcf%n_pid
              IF (pcf%job(jj)%iPid == pid2) THEN
                WRITE(hlpStr,'(A,1X,2A,I3.3,2A)') &
                     TRIM(keys(3)%value(ii)), qt,q2, pcf%job(jj)%iPid, q2,qt
                EXIT
              ENDIF
            ENDDO
          ENDIF

        ! Parallel master script
        ELSE IF (jPar == 1) THEN
          i1 = 0
          DO jj = ii+1,pcf%n_Pid
            IF (pcf%job(ii)%params(1) == pcf%job(jj)%params(1)) i1 = 1
            IF (pcf%job(jj)%params(1) /= 'PARALLEL')   CYCLE
            IF (pcf%job(ii)%params(1) == pcf%job(jj)%params(2)) THEN
              IF (i1 == 0) THEN
                hlpStr = TRIM(keys(3)%value(ii)) // ' ' // &
                         qt//q2 // TRIM(pcf%job(ii)%params(jPar)) // q2//qt
              ELSE
                WRITE(lfnerr,'(/,A,3(/,18X,A))')                                    &
                ' *** SR MENUPCF3: The parallel file name should not '    // &
                                                       'occure as PARAM1',   &
                'if the script is not the master of the PARALLEL script.',   &
                '(Each PARALLEL script cannot have more than one master!)',  &
                'Parallel file name: ' // TRIM(pcf%job(ii)%params(1))
                CALL exitrc(2)
              ENDIF
              EXIT
            ENDIF
          ENDDO
        ENDIF

        ! a "non-special" parameter
        IF (hlpStr == ' ') &
          hlpStr = TRIM(keys(3)%value(ii)) // ' ' // &
                       qt // TRIM(pcf%job(ii)%params(jPar)) // qt

        keys(3)%value(ii) = hlpStr
      ENDDO jParLoop

    ENDDO ! End of PID-loop
  ENDIF

! An empty variable record found
! ------------------------------
  IF (pcf%nVar == 0) THEN
    keys(4)%value(1) = ''
    DO ii = 1,numVal(4)
      hlpStr = TRIM(keys(4)%value(1)) // ' ' // qt // qt
      keys(4)%value(1) = hlpStr
    ENDDO
  ELSE

! Write the part with the variables
! ---------------------------------
    DO ii = 1,pcf%nVar
      keys(4)%value(ii) = ''
      WRITE(keys(4)%value(ii),'(3A,2(1X,3A))') &
            qt, TRIM(pcf%var(ii)%varnam), qt, &
            qt, TRIM(pcf%var(ii)%vardef), qt, &
            qt, TRIM(pcf%var(ii)%vardsc), qt
    ENDDO
  ENDIF

! An empty comment record found
! ------------------------------
  IF (pcf%nTxt == 0) THEN
    keys(5)%value(1) = ''
    DO ii = 1,4
      hlpStr = TRIM(keys(5)%value(1)) // ' ' // qt // qt
      keys(5)%value(1) = hlpStr
    ENDDO
  ELSE

! Write the comment lines
! -----------------------
    DO ii = 1,pcf%nTxt
      WRITE(keys(5)%value(ii),'(A,I1,A,1X,2(A,I3.3,A,1X),3A)') &
           qt, pcf%txt(ii)%section, qt,           &
          (qt, pcf%txt(ii)%pids(jj),qt, jj =1,2), &
           qt, TRIM(pcf%txt(ii)%line),    qt
    ENDDO
  ENDIF

  RETURN
END SUBROUTINE menupcf3

END MODULE
