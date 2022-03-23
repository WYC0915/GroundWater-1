
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  MODULE p_bpe2

! -------------------------------------------------------------------------
! Purpose:    This module defines structures for fortran - BPE parts
!
! Author:     R. Dach
!
! Created:    07-dec-2001
!
! Changes:    17-Feb-2003 LM: Use m_maxdim
!             06-Feb-2004 RD: Remove the length of the PCF-variables
!             10-Aug-2004 RD: Singleton-Flag instead of Priority
!             13-Dec-2007 RD: Add new special action "CONT_ERR"
!             26-Jul-2011 RD: General revision, new PCF format introduced
!             18-Aug-2011 RD: Make also t_job_v2,t_var_v2 public
!             24-Nov-2011 RD: Init pcfOld when reading old formatted PCF
!             10-Feb-2012 RD: A file must exist in readPCF
!             14-Jun-2012 RD: Variable-values may be longer than 16 characters
!             14-Jun-2012 RD: Use m_bern with only
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,  ONLY: i4b, lfnerr, lfnloc, &
                     longlinelength, shortlinelength, linelength

  IMPLICIT NONE

! Declare access rights
  PRIVATE
  PUBLIC  :: t_job_v2,t_var_v2,t_pcf_v2, init_pcf_v2, done_pcf_v2, &
             readPCF, writePcf


! Version numbers supported
! -------------------------
  INTEGER(i4b), PARAMETER :: maxVersion = 2

! Characterization of BPE jobs
! ----------------------------
  TYPE t_job_v2
    CHARACTER(LEN=shortLineLength)        :: pid    ! PID of the scripts
    CHARACTER(LEN=shortLineLength)        :: script ! Script name
    CHARACTER(LEN=shortLineLength)        :: option ! Option directory
  END TYPE t_job_v2

! PCF variables
! -------------
  TYPE t_var_v2
    CHARACTER(LEN=shortLineLength)        :: varnam ! Variable name
    CHARACTER(LEN=linelength)             :: vardsc ! Variable description
    CHARACTER(LEN=shortLineLength)        :: vardef ! Default value
  END TYPE t_var_v2

! Type for content of PCFile
! --------------------------
  TYPE t_pcf_v2
    INTEGER(i4b)                          :: ntxt    ! Number of lines
    CHARACTER(longLineLength),             &         ! Content of the file
                    DIMENSION(:), POINTER :: txt     ! as it is
!
    INTEGER(i4b)                          :: version ! version number
    INTEGER(i4b)                          :: njob    ! Number of scripts
    TYPE(t_job_v2), DIMENSION(:), POINTER :: job     ! List of all jobs
    INTEGER(i4b),   DIMENSION(3)          :: mxJob   ! max. length of the
                                                     !  components
    INTEGER(i4b)                          :: nvar    ! Number of variables
    TYPE(t_var_v2), DIMENSION(:), POINTER :: var     ! List of variables
    INTEGER(i4b),   DIMENSION(3)          :: mxVar   ! max. length of the
                                                     !  components
  END TYPE t_pcf_v2


CONTAINS


! -----------------------------------------------------------------------------
! Init PCF-structure
! -----------------------------------------------------------------------------
  SUBROUTINE init_pcf_v2(pcf)
    TYPE(t_pcf_v2) :: pcf

    pcf%nTxt = 0
    pcf%njob = 0
    pcf%nvar = 0
    pcf%version = 0

    NULLIFY(pcf%txt)
    NULLIFY(pcf%job)
    NULLIFY(pcf%var)
  END SUBROUTINE init_pcf_v2

! -----------------------------------------------------------------------------
! Deallocate PCF-structure
! -----------------------------------------------------------------------------
  SUBROUTINE done_pcf_v2(pcf)
    TYPE(t_pcf_v2) :: pcf

    IF (pcf%nTxt > 0) DEALLOCATE(pcf%txt)
    IF (pcf%nJob > 0) DEALLOCATE(pcf%job)
    IF (pcf%nVar > 0) DEALLOCATE(pcf%var)

    CALL init_pcf_v2(pcf)
  END SUBROUTINE done_pcf_v2


! -----------------------------------------------------------------------------
! Reads a PCFile
! -----------------------------------------------------------------------------
  SUBROUTINE readPCF(filnam,pcf)
    USE p_bpe,   ONLY: t_pcf, init_pcf
    USE f_lincount
    USE f_nextline
    USE s_alcerr
    USE s_opnfil
    USE s_opnerr
    USE s_exitrc
    USE s_rdpcf2
    USE s_upperc
    USE s_splarg

    ! List of parameters
    ! input:
    CHARACTER(LEN=*) :: filnam ! name of the PCFile

    ! output:
    TYPE(t_pcf_v2)   :: pcf    ! pcf

    ! Local variables
    TYPE(t_pcf)      :: pcfOld
    CHARACTER(LEN=lineLength), DIMENSION(:),POINTER :: argv
    INTEGER(i4b)     :: iTxt
    INTEGER(i4b)     :: iac
    INTEGER(i4b)     :: irCode
    INTEGER(i4b)     :: ios

    ! Init variables
    NULLIFY(argv)

    ! Allocate the memory
    pcf%nTxt = lincount(filnam,0)
    ALLOCATE(pcf%txt(pcf%nTxt),stat=iac)
    CALL alcerr(iac,'pcf%txt',(/pcf%nTxt/),'P_BPE2:readPCF')

    ! Open the file
    CALL opnfil(lfnloc,filnam,'OLD','FORMATTED','READONLY',' ',ios)
    CALL opnerr(lfnerr,lfnloc,ios,filnam,'P_BPE2:readPCF')

    ! Check the version number
    CALL splarg(nextline(lfnloc,1),argv)
    CALL upperc(argv(1))

    IF ( index(argv(1),'FORMAT') == 1 ) THEN
      READ(argv(2),*,iostat=ios) pcf%version
      IF (ios /= 0) THEN
        WRITE(lfnerr,'(/,A,/,15X,3A,/,15X,2A,/)')               &
        ' *** SR P_BPE2/readPCF: Invalid format number found.', &
        '"',argv(2),'" found but an integer number expected.',  &
        'Filename: ',TRIM(filnam)
        CALL exitrc(2)
      ELSEIF (pcf%version > maxVersion) THEN
        WRITE(lfnerr,'(/,A,2(/,15X,A,I5),/)')                       &
        ' *** SR P_BPE2/readPCF: Unsupported format number found.', &
        'Format number found:  ', pcf%version,                      &
        'Max. supported format:', maxVersion
        CALL exitrc(2)
      ENDIF

    ELSE
      pcf%version = 1
    ENDIF

    DEALLOCATE(argv)

    ! Read the file in the current version
    IF ( pcf%version == maxVersion ) THEN

      REWIND(lfnloc)

      ! Read the full file
      iTxt = 0
      ios  = 0
      DO WHILE (ios == 0 .AND. iTxt < pcf%nTxt)
        iTxt = iTxt + 1
        READ(lfnloc,'(A)',iostat=ios) pcf%txt(iTxt)
      ENDDO

      CLOSE(lfnloc)
      pcf%nTxt = iTxt

    ! Read and convert old formatted PCF
    ELSE
      CLOSE(lfnloc)
      CALL init_pcf(pcfOld)
      CALL rdpcf2(filnam,pcfOld,irCode)
      CALL pcf_v1v2(pcfOld,pcf)
    ENDIF

    CALL extPCF(pcf)

  END SUBROUTINE readPCF


! -----------------------------------------------------------------------------
! Reads a PCFile
! -----------------------------------------------------------------------------
  SUBROUTINE extPCF(pcf)
    USE s_alcerr
    USE s_splarg

    ! List of parameters
    ! input/output:
    TYPE(t_pcf_v2)   :: pcf    ! pcf

    ! Local variables
    CHARACTER(LEN=lineLength), DIMENSION(:),POINTER :: argv
    CHARACTER(LEN=lineLength) :: hlpStr
    INTEGER(i4b)     :: iTxt
    INTEGER(i4b)     :: iJob
    INTEGER(i4b)     :: iVar
    INTEGER(i4b)     :: ii
    INTEGER(i4b)     :: i1,i2
    INTEGER(i4b)     :: jj
    INTEGER(i4b)     :: iac

    ! Count number of elements
    iVar = 0
    iJob = 0
    DO iTxt = 1,pcf%nTxt

      ! Count variables and jobs
      DO ii = 1,LEN_TRIM(pcf%txt(iTxt))
        IF (pcf%txt(iTxt)(ii:ii) == '!') EXIT
        IF (pcf%txt(iTxt)(ii:ii) == ' ') CYCLE
        IF (pcf%txt(iTxt)(ii:ii) == '$') iVar = iVar + 1
        IF (pcf%txt(iTxt)(ii:ii) /= '$') iJob = iJob + 1
        EXIT
      ENDDO

    ENDDO

    ! Allocate the arrays
    ALLOCATE(pcf%job(iJob),stat=iac)
    CALL alcerr(iac,'pcf%job',(/iJob/),'P_BPE2:extPCF')
    ALLOCATE(pcf%var(iVar),stat=iac)
    CALL alcerr(iac,'pcf%var',(/iVar/),'P_BPE2:extPCF')


    ! Extract elements
    pcf%nVar = 0
    pcf%nJob = 0
    pcf%mxJob = (/ 3,8,8 /)
    pcf%mxVar = (/ 8,8,0 /)
    DO iTxt = 1,pcf%nTxt
      ii = index(pcf%txt(iTxt),'!')-1
      IF (ii == -1) ii = LEN_TRIM(pcf%txt(iTxt))
      IF (ii ==  0) CYCLE

      CALL splarg(pcf%txt(iTxt)(1:ii),argv)

      ! Format number
      IF ( index(argv(1),'FORMAT') == 1 ) CYCLE

      ! Header lines
      IF ( SIZE(argv) == 4) THEN
        IF ( argv(1) == 'PID' .AND. argv(2) == 'SCRIPT' .AND. &
             argv(3) == 'OPT_DIR' .AND. argv(4) == 'PARAMETERS' ) CYCLE
         IF (VERIFY(TRIM(argv(1)), '*') == 0 .AND. &
             VERIFY(TRIM(argv(2)), '*') == 0 .AND. &
             VERIFY(TRIM(argv(3)), '*') == 0 .AND. &
             VERIFY(TRIM(argv(4)),'.*') == 0) CYCLE
      ENDIF

      IF ( SIZE(argv) == 3) THEN
        IF ( argv(1) == 'VARIABLE' .AND. argv(2) == 'DEFAULT' .AND. &
             argv(3) == 'PARAMETERS' ) CYCLE
         IF (VERIFY(TRIM(argv(1)), '*') == 0 .AND. &
             VERIFY(TRIM(argv(2)), '*') == 0 .AND. &
             VERIFY(TRIM(argv(3)),'.*') == 0) CYCLE
      ENDIF


      ! Variable found:
      hlpStr = ADJUSTL(argv(1))
      IF (hlpStr(1:1) == '$') THEN
        pcf%nVar = pcf%nVar + 1
        pcf%var(pcf%nVar)%varnam = ADJUSTL(argv(1))
        ! Switch back to V_ for compatibility reasons
        IF (hlpStr(1:2) == '$(') THEN
          pcf%var(pcf%nVar)%varnam = 'V_' // TRIM(hlpStr(3:LEN_TRIM(hlpStr)-1))
        ELSE
          pcf%var(pcf%nVar)%varnam = 'V_' // TRIM(hlpStr(2:))
        ENDIF
        pcf%var(pcf%nVar)%vardef = ADJUSTL(argv(2))
        pcf%var(pcf%nVar)%vardsc = ''
        DO jj = 3,SIZE(argv)
          IF ( index(argv(jj),'DESCR="') /= 0 ) THEN
            i1 = index(argv(jj),'DESCR="') + 7
            i2 = LEN_TRIM(argv(jj))
            pcf%var(pcf%nVar)%vardsc = argv(jj)(i1:i2)
          ELSE IF ( index(argv(jj),'DESCR=') /= 0 ) THEN
            i1 = index(argv(jj),'DESCR=') + 6
            i2 = LEN_TRIM(argv(jj))
            pcf%var(pcf%nVar)%vardsc = argv(jj)(i1:i2)
          ENDIF
          IF (LEN_TRIM(pcf%var(pcf%nVar)%vardsc) > 0) EXIT
        ENDDO

        IF (pcf%mxVar(1) < LEN_TRIM(pcf%var(pcf%nVar)%varnam)) &
            pcf%mxVar(1) = LEN_TRIM(pcf%var(pcf%nVar)%varnam)
        IF (pcf%mxVar(2) < LEN_TRIM(pcf%var(pcf%nVar)%vardef)) &
            pcf%mxVar(2) = LEN_TRIM(pcf%var(pcf%nVar)%vardef)
        IF (pcf%mxVar(3) < LEN_TRIM(pcf%var(pcf%nVar)%vardsc)) &
            pcf%mxVar(3) = LEN_TRIM(pcf%var(pcf%nVar)%vardsc)
      ELSE
        pcf%nJob = pcf%nJob + 1
        pcf%job(pcf%nJob)%pid    = ADJUSTL(argv(1))
        pcf%job(pcf%nJob)%script = ADJUSTL(argv(2))
        pcf%job(pcf%nJob)%option = ADJUSTL(argv(3))

        IF (pcf%mxJob(1) < LEN_TRIM(pcf%job(pcf%nJob)%pid)) &
            pcf%mxJob(1) = LEN_TRIM(pcf%job(pcf%nJob)%pid)
        IF (pcf%mxJob(2) < LEN_TRIM(pcf%job(pcf%nJob)%script)) &
            pcf%mxJob(2) = LEN_TRIM(pcf%job(pcf%nJob)%script)
        IF (pcf%mxJob(3) < LEN_TRIM(pcf%job(pcf%nJob)%option)) &
            pcf%mxJob(3) = LEN_TRIM(pcf%job(pcf%nJob)%option)
      ENDIF

    ENDDO

  END SUBROUTINE extPCF




! -------------------------------------------------------------------------
! Covert PCF-structur version 1 to version 2
! -------------------------------------------------------------------------
  SUBROUTINE pcf_v1v2(pcfOld,pcf)

  USE p_bpe,  ONLY: t_pcf
  USE s_alcerr

! List of Parameters
! ------------------
! input:
  TYPE(t_pcf)                            :: pcfOld    ! PCF content, version1
! output:
  TYPE(t_pcf_v2)                         :: pcf       ! PCF content, version 2

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=15), PARAMETER           :: srName = 'P_BPE2:pcf_v1v2'

! Local Variables
! ---------------
  INTEGER(i4b)                           :: iLin
  INTEGER(i4b)                           :: ii
  INTEGER(i4b)                           :: jj
  INTEGER(i4b)                           :: kk
  INTEGER(i4b)                           :: iw
  INTEGER(i4b)                           :: ip
  INTEGER(i4b)                           :: iTxt
  INTEGER(i4b)                           :: iTx2
  INTEGER(i4b)                           :: iPos
  INTEGER(i4b)                           :: iac

  pcf%ntxt = 5 + pcfOld%n_pid + pcfOld%nvar + pcfOld%ntxt

  ALLOCATE(pcf%txt(pcf%ntxt),stat=iac)
  CALL alcerr(iac,'pcf%txt',(/pcf%ntxt/),srName)
  pcf%txt = ''

  iLin = 1
  WRITE(pcf%txt(iLin),'(A,I5)') 'FORMAT:',maxVersion

  iTxt = 1
  iTx2 = 1
  IF (iTx2 <= pcfOld%nTxt) THEN
    DO WHILE ( pcfOld%txt(iTx2)%section < 2 )
      iTx2 = iTx2 + 1
      IF (iTx2 > pcfOld%nTxt) EXIT
    ENDDO
  ENDIF

! Start with coments at the beginning
! -----------------------------------
  IF (iTxt <= pcfOld%nTxt) THEN
    DO WHILE(pcfOld%txt(iTxt)%section == 0)
      iLin = iLin + 1
      WRITE(pcf%txt(iLin),'(A)') '!' // TRIM(pcfOld%txt(iTxt)%line)
      iTxt = iTxt+1
      IF (iTxt > pcfOld%nTxt) EXIT
    ENDDO
  ENDIF

! Write the list of scripts into the new structure
! ------------------------------------------------
  pcf%txt(iLin+1) = 'PID SCRIPT   OPT_DIR  PARAMETERS'
  pcf%txt(iLin+2) = '*** ******** ******** **********' // &
                    '*******************************************...'
  iLin = iLin + 2

  DO ii = 1,pcfOld%n_Pid

    ! Add comment lines: old section 1
    IF (iTxt <= pcfOld%nTxt) THEN
      DO WHILE(pcfOld%txt(iTxt)%section == 1 .AND. &
               pcfOld%job(ii)%iPid > pcfOld%txt(iTxt)%pids(1))
        iLin = iLin + 1
        WRITE(pcf%txt(iLin),'(A)') '!' // TRIM(pcfOld%txt(iTxt)%line)
        iTxt = iTxt+1
        IF (iTxt > pcfOld%nTxt) EXIT
      ENDDO
    ENDIF

    ! Add comment lines: old section 2
    IF (iTx2 <= pcfOld%nTxt) THEN
      DO WHILE(pcfOld%txt(iTx2)%section == 2 .AND. &
               pcfOld%job(ii)%iPid > pcfOld%txt(iTx2)%pids(1))
        iLin = iLin + 1
        WRITE(pcf%txt(iLin),'(A)') '!' // TRIM(pcfOld%txt(iTx2)%line)
        iTx2 = iTx2+1
        IF (iTx2 > pcfOld%nTxt) EXIT
      ENDDO
    ENDIF

    ! Add the job with its parameters
    iLin = iLin + 1
    WRITE(pcf%txt(iLin),'(I3.3,2(1X,A8))')         &
          pcfOld%job(ii)%iPid,   pcfOld%job(ii)%script,  &
          pcfOld%job(ii)%option

    ! Specify CPU
    jj = 23
    pcf%txt(iLin)(23:) = 'CPU=' // pcfOld%job(ii)%cpu

    ! Translate waiting conditions
    jj = jj + 13
    IF (pcfOld%job(ii)%n_wait > 0) THEN
      IF (pcfOld%job(ii)%n_wait > 1) THEN
        pcf%txt(iLin)(jj:) = 'WAIT="'
        jj = jj + 6
        DO iw = 1,pcfOld%job(ii)%n_wait
          WRITE(pcf%txt(iLin)(jj:),'(I3.3)') pcfOld%job(ii)%iwait(iw)
          jj = jj + 4
        ENDDO
        pcf%txt(iLin)(jj-1:jj-1) = '"'
        jj = jj + 1
      ELSE IF (ii == 1) THEN
        WRITE(pcf%txt(iLin)(jj:),'(A,I3.3)') 'WAIT=',pcfOld%job(ii)%iwait(1)
        jj = jj + 9
      ELSE IF (pcfOld%job(ii)%iwait(1) == pcfOld%job(ii-1)%ipid) THEN
        pcf%txt(iLin)(jj:) = 'WAIT'
        jj = jj + 5
      ELSE
        WRITE(pcf%txt(iLin)(jj:),'(A,I3.3)') 'WAIT=',pcfOld%job(ii)%iwait(1)
        jj = jj + 9
      ENDIF
    ELSE
        jj = jj + 5
    ENDIF

    ! Special action: Skip
    IF (pcfOld%job(ii)%ptype == -1) THEN
      pcf%txt(iLin)(jj:) = 'SKIP'
      jj = jj + 5
    ENDIF

    ! Special action: CONT_ERR
    IF (pcfOld%job(ii)%ptype == 3) THEN
      pcf%txt(iLin)(jj:) = 'CONT_ERR'
      jj = jj + 9
    ENDIF

    ! Special action: Parallel
    IF (pcfOld%job(ii)%ptype == 1) THEN
      DO iw = ii-1,1,-1
        IF (pcfOld%job(ii)%params(2) == pcfOld%job(iw)%params(1)) THEN
          IF (ii == iw+1) THEN
            pcf%txt(iLin)(jj:) = 'PARALLEL'
            jj = jj + 9
          ELSE
            WRITE(pcf%txt(iLin)(jj:),'(A,I3.3)') 'PARALLEL=',pcfOld%job(iw)%ipid
            jj = jj + 13
          ENDIF
        ENDIF
      ENDDO
    ENDIF

    ! Script parameters
    IF (pcfOld%job(ii)%ptype /= 1) THEN
      kk = 0
      DO ip = 1,SIZE(pcfOld%job(ii)%params)
        IF ( ip == 1 .AND. pcfOld%job(ii)%params(ip)(1:1) == '$' ) CYCLE
        IF ( LEN_TRIM(pcfOld%job(ii)%params(ip)) > 0 ) kk = ip
      ENDDO

      IF (kk == 1) THEN
        pcf%txt(iLin)(jj:) = 'PARAMS=' // TRIM(pcfOld%job(ii)%params(1))
        jj = jj + 8 + LEN_TRIM(pcfOld%job(ii)%params(1))
      ELSE IF (kk > 1) THEN
        pcf%txt(iLin)(jj:) = 'PARAMS="'
        jj = jj + 8
        DO ip = 1,kk
          IF ( ip == 1 .AND. pcfOld%job(ii)%params(ip)(1:1) == '$' ) THEN
            pcf%txt(iLin)(jj:) = '_'
            jj = jj + 1
          ELSEIF ( LEN_TRIM(pcfOld%job(ii)%params(ip)) > 0 ) THEN
            pcf%txt(iLin)(jj:) = TRIM(pcfOld%job(ii)%params(ip))
            jj = jj + LEN_TRIM(pcfOld%job(ii)%params(ip))
          ELSE
            pcf%txt(iLin)(jj:) = '_'
            jj = jj + 1
          ENDIF
          IF ( ip < kk ) jj = jj + 1
        ENDDO
        pcf%txt(iLin)(jj:) = '"'
        jj = jj + 2
      ENDIF
    ENDIF


    ! Run on a special campaign
    IF ( LEN_TRIM(pcfOld%job(ii)%camp) > 0) THEN
      pcf%txt(iLin)(jj:) = 'CAMPAIGN=' // TRIM(pcfOld%job(ii)%camp)
      jj = jj + 10 + LEN_TRIM(pcfOld%job(ii)%camp)
    ENDIF

    ! Singleton flag
    IF ( pcfOld%job(ii)%jFlags == 'S' .OR. &
         pcfOld%job(ii)%jFlags == 's') THEN
      pcf%txt(iLin)(jj:) = 'SINGLETON'
      jj = jj + 10
    ENDIF
  ENDDO

! Add comment lines behind the section
! ------------------------------------
  IF (iTxt <= pcfOld%nTxt) THEN
    DO WHILE(pcfOld%txt(iTxt)%section == 1)
      iLin = iLin + 1
      WRITE(pcf%txt(iLin),'(A)') '!' // TRIM(pcfOld%txt(iTxt)%line)
      iTxt = iTxt+1
      IF (iTxt > pcfOld%nTxt) EXIT
    ENDDO
  ENDIF

  IF (iTx2 <= pcfOld%nTxt) THEN
    DO WHILE(pcfOld%txt(iTx2)%section == 2)
      iLin = iLin + 1
      WRITE(pcf%txt(iLin),'(A)') '!' // TRIM(pcfOld%txt(iTx2)%line)
      iTx2 = iTx2+1
      IF (iTx2 > pcfOld%nTxt) EXIT
    ENDDO
  ENDIF

! Write the list of scripts into the new structure
! ------------------------------------------------
  pcf%txt(iLin+1) = 'VARIABLE  DEFAULT            PARAMETERS'
  pcf%txt(iLin+2) = '********* ****************** **********' // &
                    '************************************...'
  iLin = iLin + 2

  DO ii = 1,pcfOld%nVar

    iLin = iLin + 1
    IF ( LEN_TRIM(pcfOld%var(ii)%varnam(3:)) == 1 ) THEN
      pcf%txt(iLin) = '$'//TRIM(pcfOld%var(ii)%varnam(3:))
    ELSE
      pcf%txt(iLin) = '$('//TRIM(pcfOld%var(ii)%varnam(3:))//')'
    ENDIF

    iPos = MAX0( 11,LEN_TRIM(pcf%txt(iLin))+2 )
    IF ( INDEX(TRIM(pcfOld%var(ii)%vardef),' ') /= 0 .OR. &
         LEN_TRIM(pcfOld%var(ii)%vardef) == 0) THEN
      pcf%txt(iLin)(iPos:) = '"' // TRIM(pcfOld%var(ii)%vardef) // '"'
    ELSE
      pcf%txt(iLin)(iPos:) = TRIM(pcfOld%var(ii)%vardef)
    ENDIF

    iPos = MAX0( 30,LEN_TRIM(pcf%txt(iLin))+2 )
    IF ( LEN_TRIM(pcfOld%var(ii)%vardsc) > 0 ) THEN
      IF ( INDEX(pcfOld%var(ii)%vardsc,' ') /= 0 ) THEN
        pcf%txt(iLin)(iPos:) = 'DESCR="' // TRIM(pcfOld%var(ii)%vardsc) // '"'
      ELSE
        pcf%txt(iLin)(iPos:) = 'DESCR=' // TRIM(pcfOld%var(ii)%vardsc)
      ENDIF
    ENDIF
  ENDDO

! Add comment lines behind the section
! ------------------------------------
  IF (iTx2 <= pcfOld%nTxt) THEN
    DO WHILE(pcfOld%txt(iTx2)%section == 3)
      iLin = iLin + 1
      WRITE(pcf%txt(iLin),'(A)') '!' // TRIM(pcfOld%txt(iTx2)%line)
      iTx2 = iTx2+1
      IF (iTx2 > pcfOld%nTxt) EXIT
    ENDDO
  ENDIF

  RETURN
END SUBROUTINE pcf_v1v2

! -----------------------------------------------------------------------------
! Writes a PCFile
! -----------------------------------------------------------------------------
  SUBROUTINE writePCF(filnam,pcf)
    USE s_opnfil
    USE s_opnerr

    ! List of parameters
    ! input:
    CHARACTER(LEN=*) :: filnam ! name of the PCFile

    ! output:
    TYPE(t_pcf_v2)   :: pcf    ! pcf

    ! Local variables
    INTEGER(i4b)     :: iTxt
    INTEGER(i4b)     :: ios

    ! Open the file
    CALL opnfil(lfnloc,filnam,'UNKNOWN','FORMATTED','READONLY',' ',ios)
    CALL opnerr(lfnerr,lfnloc,ios,filnam,'P_BPE2:writePCF')

    ! Write the lines
    DO iTxt = 1, pcf%nTxt
      WRITE(lfnloc,'(A)') TRIM(pcf%txt(iTxt))
    ENDDO

    CLOSE(lfnloc)

  END SUBROUTINE writePCF


END MODULE p_bpe2
