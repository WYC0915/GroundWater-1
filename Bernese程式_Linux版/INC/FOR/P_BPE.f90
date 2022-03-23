
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  MODULE p_bpe

! -------------------------------------------------------------------------
! Purpose:    This module defines structures for fortran - BPE parts
!
! Author:     R. Dach
!
! Created:    07-Dec-2001
!
! Changes:    17-Feb-2003 LM: Use m_maxdim
!             06-Feb-2004 RD: Remove the length of the PCF-variables
!             10-Aug-2004 RD: Singleton-Flag instead of Priority
!             13-Dec-2007 RD: Add new special action "CONT_ERR"
!             24-Nov-2011 RD: Add subroutine init_pcf
!             14-Jun-2012 RD: Increase length of variable default
!             14-Jun-2012 RD: Use m_bern with only
!             14-Jun-2012 RD: PCF-specific parameters from M_MAXDIM to P_BPE
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, lineLength
  IMPLICIT NONE

! Parameters
! ----------
  INTEGER(i4b),PARAMETER  :: maxpid=400
  INTEGER(i4b),PARAMETER  :: maxwat=10
  INTEGER(i4b),PARAMETER  :: maxdsc=60

! Header lines of PCFile
! ----------------------
  CHARACTER(LEN=120), DIMENSION(6), PARAMETER :: PCF_header = &
             (/ 'PID SCRIPT   OPT_DIR  CAMPAIGN CPU      ' // &
                'F WAIT FOR....                          ' // &
                '                                        ',   &
                '3** 8******* 8******* 8******* 8******* ' // &
                '1 3** 3** 3** 3** 3** 3** 3** 3** 3** 3*' // &
                '*                                       ',   &
                'PID USER         PASSWORD PARAM1   PARAM' // &
                '2   PARAM3   PARAM4   PARAM5   PARAM6   ' // &
                'PARAM7   PARAM8   PARAM9                ',   &
                '3** 12********** 8******* 8******* 8****' // &
                '*** 8******* 8******* 8******* 8******* ' // &
                '8******* 8******* 8*******              ',   &
                'VARIABLE DESCRIPTION                    ' // &
                '          DEFAULT                       ' // &
                '                                        ',   &
                '8******* 40*****************************' // &
                '********* 30****************************' // &
                '                                        ' /)

! Number of entries for the unilines
! ----------------------------------
  INTEGER(i4b),      DIMENSION(4), PARAMETER :: numVal = (/ 16, 14, 12,  3 /)

  CHARACTER(LEN=15), DIMENSION(4), PARAMETER :: kWords = &
            (/ 'LIST_OF_SCRIPTS', 'SPECIALS       ', &
               'PARAMETERS     ', 'PCF_VARIABLES  ' /)

  CHARACTER(LEN=31), DIMENSION(4), PARAMETER :: kDescr = &
            (/ 'List of BPE scripts            ', &
               'Special actions for the scripts', &
               'Parameters for the scripts     ', &
               'PCF default variables          ' /)

! Process type definition
! -----------------------
  CHARACTER(LEN=8), DIMENSION(5), PARAMETER :: special = &
            (/ 'SKIP    ', '        ', 'PARALLEL', 'NEXTJOB ', 'CONT_ERR'/)

  CHARACTER(LEN=1), PARAMETER :: flgSingle = 'S'


! Characterization of BPE jobs
! ----------------------------
  TYPE t_job
    INTEGER(i4b)                         :: ipid   ! PID of the scripts
    CHARACTER(LEN= 8)                    :: script ! Script name
    CHARACTER(LEN= 8)                    :: option ! Option directory
    CHARACTER(LEN= 8)                    :: camp   ! campaign to run
    CHARACTER(LEN= 8)                    :: cpu    ! CPU to run
    CHARACTER(LEN=12)                    :: user   ! User name for CPU
    CHARACTER(LEN= 8)                    :: paswrd ! Password for CPU
    INTEGER(i4b)                         :: n_wait ! Number of wait PIDs
    INTEGER(i4b),      DIMENSION(MAXWAT) :: iwait  ! List of wait PIDs
    INTEGER(i4b)                         :: ptype  ! Type of the script:
                                                   ! -1: skip
                                                   !  0: normal
                                                   !  1: parallel
                                                   !  2: nexjob
                                                   !  3: cont. in case of error
    CHARACTER(LEN=80), DIMENSION(9)      :: params ! Script parameter
    CHARACTER(LEN=1)                     :: jFlags ! Job-flag:
                                                   !  S: singleton for multi-sess.
  END TYPE t_job

! PCF variables
! -------------
  TYPE t_var
    CHARACTER(LEN=8)                     :: varnam ! Variable name
    CHARACTER(LEN=40)                    :: vardsc ! Variable description
    CHARACTER(LEN=32)                    :: vardef ! Default value
  END TYPE t_var

! PCFile comment lines
! --------------------
  TYPE t_txt
    INTEGER(i4b)                         :: section ! location of the comment
                                                    !  0: before list of scripts
                                                    !  1: within the list of scripts
                                                    !  2: within the parameters
                                                    !  3: within the variables
    CHARACTER(LEN=linelength)            :: line    ! Comment line
    INTEGER(i4b), DIMENSION(2)           :: pids    ! last PID before
                                                    ! first PID behind
  END TYPE t_txt

! Type for content of PCFile
! --------------------------
  TYPE t_pcf
    INTEGER(i4b)                         :: n_pid  ! Number of scripts
    TYPE(t_job), DIMENSION(MAXPID)       :: job    ! List of all jobs
    INTEGER(i4b)                         :: nvar   ! Number of variables
    TYPE(t_var), DIMENSION(MAXDSC)       :: var    ! List of variables
    INTEGER(i4b)                         :: ntxt   ! Number of comment lines
    TYPE(t_txt), DIMENSION(:),POINTER    :: txt    ! Comment lines
  END TYPE t_pcf

CONTAINS
! -----------------------------------------------------------------------------
! Init PCF-structure
! -----------------------------------------------------------------------------
  SUBROUTINE init_pcf(pcf)
    TYPE(t_pcf) :: pcf

    pcf%n_pid = 0
    pcf%nvar = 0
    pcf%nTxt = 0

    NULLIFY(pcf%txt)
  END SUBROUTINE init_pcf

END MODULE p_bpe
