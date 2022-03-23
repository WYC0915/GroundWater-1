MODULE s_MENU_FST
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_fst(keyWord, menuauxInp, output)

! -------------------------------------------------------------------------
! Purpose:    Generates a list of station from a PLT-file or
!             from a list of CRDs-files for FODITS
!
! Author:     L. Ostini
!
! Created:    18-Aug-2001
! Last mod.:  07-Apr-2010
!
! Changes:    07-Apr-2010 LO: Major changes do to algorithm and output file
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,     ONLY: i4b, keyValueLength, lfnerr, staNameLength, t_key

  USE p_fodits,   ONLY: t_opt, t_sCore

  USE s_fodirdat

  USE s_gtflna
  USE s_ckopti
  USE s_readkeys
  USE s_alcerr
  USE s_exitrc

  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*),INTENT(IN)     :: keyWord    ! what to do
  CHARACTER(LEN=*),INTENT(IN)     :: menuauxInp ! MENUAUX.INP file name

! output:
  TYPE(t_key),INTENT(INOUT)       :: output  ! name = keyWord, if OK
                                             ! value: Result to display

! Local Parameters
! ----------------
  CHARACTER(LEN=8),PARAMETER      :: srName  = 'menu_fst'

! Local Variables
! ---------------
  INTEGER(i4b)                    :: irc, iac, irCode

  TYPE(t_opt)                     :: opt          ! Option struct
  TYPE(t_sCore)                   :: sCore        ! Gen TS struct

  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue

  INTEGER(i4b)                    :: iCrd
  INTEGER(i4b)                    :: iSta

! Initialize
! ----------
  NULLIFY(keyValue)
  irCode = 0

! Incorrect keyword
! -----------------
  IF (keyWord /= 'FST')THEN
     RETURN
  END IF

! Read input keywords
! -------------------
  ! INPUT FILES
  CALL readkeys('SEL_INP_TS_TYPE', keyValue, irc)
  IF ( irc == 0 .AND. keyValue(1) == 'RESIDUALS' )THEN
     opt%selInpTsType = 0          ! CRD+VEL+PLT as input TS
     CALL gtflna(0, 'IN_PLT_FILE',  opt%inPltFile , irc)
     IF( LEN_TRIM(opt%inPltFile) == 0 )THEN
        RETURN
     END IF
  ELSE IF ( irc == 0 .AND. keyValue(1) == 'COORDINATES' )THEN
     opt%selInpTsType = 1          ! CRDs as input TS
     CALL readkeys('IN_CRD_FILES', keyValue, irc)
     opt%nCrds = SIZE(keyValue)
     ALLOCATE(opt%crdFileName(opt%nCrds), stat=iac)
     CALL alcerr(iac, 'opt%crdFileName', (/opt%nCrds/), srName)
     IF ( irc == 0 )THEN
        DO iCrd = 1, opt%nCrds
           opt%crdFileName(iCrd) = keyValue(iCrd)
        END DO
     ELSE
        WRITE(lfnerr,'(/,A,/)') &
             ' *** SR MENU_FST: Problem with filenames of the input CRDs'
        CALL exitrc(2)
     ENDIF
  END IF
  ! INPUT STATION NAMES
  CALL readkeys("IN_TRUNC_STA_NAME",keyValue,irc)
  IF (keyValue(1)=="NO") THEN
    opt%inTruncStaName = staNameLength
  ELSE IF (keyValue(1)=="YES") THEN
    opt%inTruncStaName = 14
  ELSE
    CALL ckopti(1,'IN_TRUNC_STA_NAME',keyValue,'sr aoptfil',&
                'Truncate station names',irc,irCode,empty=staNameLength, &
                ge=0,le=staNameLength,result1=opt%inTruncStaName)
  ENDIF

  ! Read input station list
  ! -----------------------
  ! Set the flag to read the names of stations in input files
  opt%rdOnlyStaNames = 1

  ! Various variables
  opt%inCrdFileAddPlt = 0
  opt%inVelFileAddPlt = 0

  ! Extract station names from input files
  CALL fodirdat(opt,sCore,1)

  ! Put the results into the output-key
  ! -----------------------------------
  ALLOCATE(output%value(sCore%nSta),stat=irc)
  CALL alcerr(irc,'output%value',(/sCore%nSta/),srName)
  output%value = ' '

  DO iSta = 1,sCore%nSta
    WRITE(output%value(iSta), *) sCore%sta(iSta)%name
  END DO

  RETURN
END SUBROUTINE menu_fst

END MODULE
