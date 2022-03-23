MODULE s_RDIGSP
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdigsp(iSaSys,optGsp)

! -------------------------------------------------------------------------
! Purpose:    Reads the GNSS-specific parameter options
!
! Author:     M. Meindl
!
! Created:    25-Nov-2010
! Last mod.:  __-___-____
!
! Changes:    __-___-____ __:
!
! SR used:    exitrc, readKeys
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE p_gpsest, ONLY: t_optGsp
  USE s_readkeys
  USE s_ckoptc
  USE s_ckoptr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                 :: isasys ! satellite system to be considered
                                         ! = 0: ALL
                                         ! = 1: GPS
                                         ! = 2: GLONASS
                                         ! = 3: Galileo
                                         ! = 4: GPS+GLONASS
                                         ! = 5: GPS+Galileo
                                         ! = 6: GLONASS+Galileo

! output:
  TYPE(t_optGsp)               :: optGsp ! GNSS-specific parameter options


! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
         DIMENSION(:), POINTER  :: keyValue

  INTEGER(i4b)                  :: irc, irCode, iac


! Init the variables
! ------------------
  irCode        = 0
  optGsp%traSys = 0
  optGsp%traSig = 0.d0
  optGsp%trpSys = 0
  optGsp%trpSig = 0.d0
  NULLIFY(keyValue)


! Station translations
! --------------------

! GNSS
  CALL readKeys('SYSTRA',keyValue,irc)
  CALL ckoptc(1,'SYSTRA',keyValue,                                   &
              (/'NONE   ','GLONASS','GALILEO','GLO/GAL'/),           &
              'rdigsp','GNSS-spec translations: system',irc,irCode,  &
              valList=(/0,1,2,3/),result1=optGsp%traSys)

! A priori sigmas
  CALL readKeys('SYSTRA_N',keyValue,irc)
  CALL ckoptr(1,'SYSTRA_N',keyValue,'rdigsp',                         &
              'GNSS-spec translations: sigma north',                  &
              irc,irCode,empty=0d0,ge=0d0,result1=optGsp%traSig(1))
  CALL readKeys('SYSTRA_E',keyValue,irc)
  CALL ckoptr(1,'SYSTRA_E',keyValue,'rdigsp',                         &
              'GNSS-spec translations: sigma east',                   &
              irc,irCode,empty=0d0,ge=0d0,result1=optGsp%traSig(2))
  CALL readKeys('SYSTRA_U',keyValue,irc)
  CALL ckoptr(1,'SYSTRA_U',keyValue,'rdigsp',                         &
              'GNSS-spec translations: sigma up',                     &
              irc,irCode,empty=0d0,ge=0d0,result1=optGsp%traSig(3))


! Troposphere biases
! ------------------

! GNSS
  CALL readKeys('SYSTRP',keyValue,irc)
  CALL ckoptc(1,'SYSTRP',keyValue,                                       &
              (/'NONE   ','GLONASS','GALILEO','GLO/GAL'/),               &
              'rdigsp','GNSS-spec troposphere bias: system',irc,irCode,  &
              valList=(/0,1,2,3/),result1=optGsp%trpSys)

! A priori sigmas
  CALL readKeys('SYSTRP_A',keyValue,irc)
  CALL ckoptr(1,'SYSTRP_A',keyValue,'rdigsp',                       &
              'GNSS-spec troposphere bias: sigma',                  &
              irc,irCode,empty=0d0,ge=0d0,result1=optGsp%trpSig)


! Check GNSS selection
! --------------------
  IF (iSaSys == 1) THEN
    optGsp%traSys = 0
    optGsp%trpSys = 0
  ELSEIF (iSaSys == 2 .OR. iSaSys == 4) THEN
    IF (optGsp%traSys == 2) optGsp%traSys = 0
    IF (optGsp%traSys == 3) optGsp%traSys = 1
    IF (optGsp%trpSys == 2) optGsp%trpSys = 0
    IF (optGsp%trpSys == 3) optGsp%trpSys = 1
  ELSEIF (iSaSys == 3 .OR. iSaSys == 5) THEN
    IF (optGsp%traSys == 1) optGsp%traSys = 0
    IF (optGsp%traSys == 3) optGsp%traSys = 2
    IF (optGsp%trpSys == 1) optGsp%trpSys = 0
    IF (optGsp%trpSys == 3) optGsp%trpSys = 2
  ENDIF


! The end
! -------
  DEALLOCATE(keyValue,stat=iac)
  RETURN
END SUBROUTINE rdigsp

END MODULE s_RDIGSP
