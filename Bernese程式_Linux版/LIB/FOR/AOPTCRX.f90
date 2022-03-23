MODULE s_AOPTCRX
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE aoptcrx(opt,staInfo)

! -------------------------------------------------------------------------
! Purpose:    Read input options for ADDNEQ2:
!             List of flags to be used from station info file
!
! Author:     R. Dach
!
! Created:    24-Oct-2001
!
! Changes:    21-Dec-2001 HU: m_addneq replaced by p_addneq
!             23-Apr-2003 CU: Nullify local pointers
!             09-Jul-2003 RD: Move station info from t_opt to t_staCrux
!             06-Jan-2005 HU: Allocate structure staProblem
!             13-Apr-2005 CU: Add interface for alcerr
!             03-Mar-2010 RD: Remove obsolete staProblem struture
!             04-Mar-2010 RD: Eliminate parameters in case of equipment changes
!             26-Oct-2010 SL: use m_bern with ONLY, removal of unused mods/vars
!             20-Apr-2012 RD: Nullify keyValue
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, keyValueLength
  USE d_stacrx, ONLY: t_staCrux
  USE p_addneq, ONLY: t_opt, prtCrx
  USE s_gtstaflg
  USE s_readkeys
  USE s_readcrux
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE f_tstkey
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:

! input/output:
  TYPE(t_opt)                     :: opt      ! ADDNEQ2 option record

! output:
  TYPE(t_staCrux)                 :: staInfo  ! staInfo for ADDNEQ2


! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8),PARAMETER      :: srName = 'aoptcrx'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue

  INTEGER(i4b)                                          :: irc, iac
  INTEGER(i4b)                                          :: irCode


! Init local variables
! --------------------
  NULLIFY(keyValue)
  irCode = 0

! Is a station info file selected?
! --------------------------------
  opt%prt(prtCrx) = 0
  opt%chgRecNam  = 0
  opt%chgRecNum  = 0
  opt%chgAntNam  = 0
  opt%chgAntEcc  = 0
  opt%chgAntNum  = 0
  IF (LEN_TRIM(opt%staCrux) == 0) RETURN

! Read the file
! -------------
  CALL readCrux(opt%staCrux,staInfo)

! Consider flags
! --------------
  CALL gtStaFlg('USEFLG',                                          &
                (/'FLG001','FLG002','FLG003','      ','      ' /), &
                staInfo)


! Print warnings (not in MENUAUX)
! -------------------------------
  IF ( tstkey('CRXWARN') ) THEN
    CALL ckoptb(1,(/ 'CRXWARN' /),srName,'Print warning for station info', &
                irCode,result1=opt%prt(prtCrx))
  ENDIF


! Exclude station in case of equipment changes
! --------------------------------------------
  CALL readKeys('CRXREC',keyValue,irc)
  CALL ckoptc(1,'CRXREC',keyValue, (/'NEVER         ',                   &
              'TYPE          ','TYPE_OR_NUMBER'/), srName,               &
              'Remove station in case of receiver changes',irc,irCode,   &
              valList = (/ 0,1,2 /), result1=opt%chgRecNam)
  IF (opt%chgRecNam == 2) THEN
    opt%chgRecNam = 1
    opt%chgRecNum = 1
  ENDIF

  CALL readKeys('CRXANT',keyValue,irc)
  CALL ckoptc(1,'CRXANT',keyValue,     (/'NEVER                   ',     &
              'TYPE                    ','TYPE_OR_ECCENT          ',     &
              'TYPE_OR_ECCENT_OR_NUMBER'/), srName,                      &
              'Remove station in case of antenna changes',irc,irCode,    &
              valList = (/ 0,1,2,3 /), result1=opt%chgAntNam)
  IF (opt%chgAntNam == 2) THEN
    opt%chgAntNam = 1
    opt%chgAntNum = 1
  ELSEIF (opt%chgAntNam == 3) THEN
    opt%chgAntNam = 1
    opt%chgAntNum = 1
    opt%chgAntEcc = 1
  ENDIF

  DEALLOCATE(keyValue,stat=iac)

  IF (irCode /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE aoptcrx

END MODULE
