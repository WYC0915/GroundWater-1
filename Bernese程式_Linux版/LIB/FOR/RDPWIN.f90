MODULE s_RDPWIN
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdpwin(key_dt,key_t0,t_0,dt_0)

! -------------------------------------------------------------------------
! Purpose:    Reads the parameter time window specification
!
!
! Author:     R. Dach
!
! Created:    25-Mar-2003
! Last mod.:  21-May-2010
!
! Changes:    21-May-2010 MF: Deallocate keyValue
!
! SR used:    readkeys,ckoptr,ckoptd,ckoptt,exitrc
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_ckoptt
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptd
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)               :: key_dt ! Keyword for offset wrt
                                           ! GPS days (empty: no request)
  CHARACTER(LEN=*), DIMENSION(2) :: key_t0 ! Keywords for start epoch
                                           ! (1): date
                                           ! (2): time
                                           ! (empty keywords: no request)
! output:
  REAL(r8b)                      :: t_0    ! Special start epoch
                                           ! 1d20: Parameter definition wrt UTC
  REAL(r8b)                      :: dt_0   ! Offset wrt UTC-day (in hours)
                                           ! 0d0: not used

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER     :: srName = 'rdpwin'


! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength) , &
      DIMENSION(:)  , POINTER     :: keyValue

  INTEGER(i4b)                    :: irCode
  INTEGER(i4b)                    :: irc
  INTEGER(i4b)                    :: iac

  REAL(r8b)                       :: hlpTim

! Init variables
! --------------
  irCode = 0
  NULLIFY(keyValue)

  t_0  = 1d20
  dt_0 = 0d0

! Offset wrt GPS day
! ------------------
  IF (LEN_TRIM(key_dt) > 0) THEN

    CALL readKeys(key_dt,keyValue,irc)

    CALL ckoptt(1,key_dt,keyValue,srName,                         &
                'Offset for parameter time window',irc,irCode,    &
                empty=0d0,maxVal=1,result1=dt_0)

  ENDIF

! Start epoch
! -----------
  IF (LEN_TRIM(key_t0(1)) > 0) THEN

    CALL readKeys(key_t0(1),keyValue,irc)

    CALL ckoptd(1,key_t0(1),keyValue,srName,                    &
                'Date for parameter time window',irc,irCode,    &
                empty=1d20,maxVal=1,result1=t_0)

    IF (t_0 /= 1d20) THEN
      CALL readKeys(key_t0(2),keyValue,irc)

      CALL ckoptt(1,key_t0(2),keyValue,srName,                    &
                  'Time for parameter time window',irc,irCode,    &
                  empty=0d0,maxVal=1,result1=hlpTim)

      t_0 = t_0 + hlpTim/24d0

! A start epoch without a date is an offset
! -----------------------------------------
    ELSE

      CALL readKeys(key_t0(2),keyValue,irc)

      CALL ckoptt(1,key_t0(2),keyValue,srName,                    &
                  'Time for parameter time window',irc,irCode,    &
                  empty=0d0,maxVal=1,result1=dt_0)

    ENDIF

  ENDIF

  DEALLOCATE(keyValue,stat=iac)

! Exit if reading of the input options failed
! -------------------------------------------
  IF (irCode /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE rdpwin

END MODULE
