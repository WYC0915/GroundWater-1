MODULE s_RDLAUI
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdlaui(opt)

!--------------------------------------------------------------------------
! Purpose:   Read options for program LEOAUX
!
! Author:    H.Bock
!
! Created:   05-Dec-2001
! Last mod.: 12-Jul-2011
!
! Changes:   23-Apr-2003 RD: Nullify local pointers
!            20-May-2003 RD: Use SR gttimwin instead of SR readsess
!            24-Jun-2003 HB: Remove TYP2, correct use of time window
!            08-Nov-2004 HB: Add GOCE as SATNAM
!            28-Jun-2005 MM: Unused variables removed
!            11-Mar-2008 HB: Add opt%strqua and opt%str_id
!                            remove opt%concat
!            12-Jul-2011 HB: Remove opt%str_id, no longer needed,
!                            is read directly from file
!
! SR used:   gttimwin
!
! Copyright  Astronomical Institute
!            University of Bern
!            Switzerland
!-------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE p_leoaux, ONLY: t_leoaux_opt
  USE s_gttimwin
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckoptl
  IMPLICIT NONE

! List of Parameters
! ------------------
! OUT:
  TYPE(t_leoaux_opt) :: opt ! Option structure

! List of Functions
! -----------------
! Local Types
! -----------
! Local Parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength),DIMENSION(:),POINTER :: keyValue
  CHARACTER(LEN=45)  :: srName

  INTEGER(i4b)       :: irc,iac
  INTEGER(i4b)       :: irCode
  INTEGER(i4b)       :: satHlp

  LOGICAL            :: hlp


! Initialization
! --------------
  irCode = 0
  srName = 'RDLAUI'

  NULLIFY(keyValue)

! General options
! ---------------
  CALL readKeys('SATNAM', keyValue, irc)
  CALL ckoptc(1,'SATNAM',keyValue,                       &
       (/'CHAMP', 'GRACE', 'GOCE ' /),                   &
       srName,'Satellite Name',irc,irCode,               &
       maxVal=1,result1=satHlp)

  IF (satHlp==1) THEN
    opt%satNam= 'CHAMP'
  ELSEIF (satHlp==2) THEN
    opt%satNam= 'GRACE'
  ELSEIF (satHlp==3) THEN
    opt%satNam= 'GOCE'
  ENDIF

  opt%strqua = -1
  IF (opt%satNam /= 'GOCE') THEN
    CALL readKeys('IXYZ', keyValue, irc)
    CALL ckoptc(1,'IXYZ',keyValue,                         &
         (/'XYZ', 'RSW' /),                                &
         srName,'System for storing',irc,irCode,           &
         maxVal=1,result1=opt%ixyz)

    CALL ckoptb(1,(/'TYP1'/),srName,'Typlst: First Part', &
         irCode,resultL=hlp)
    opt%typlst(:)=0
    IF (hlp) opt%typlst(1:6)=1
  ELSE
    CALL ckoptb(1,(/'STRQUA'/),srName,                      &
              'Star tracker quaternions',irCode,            &
              result1=opt%strqua)
  ENDIF

! Observation window
! ------------------
  CALL gtTimWin(' ',(/'RADIO_0','RADIO_1','RADIO_2'/),           &
                (/'SESSION_YEAR','SESSION_STRG'/),               &
                (/'STADAT', 'STATIM', 'ENDDAT', 'ENDTIM'/),      &
                opt%window(:))

  CALL readKeys('TITLE',keyValue,irc)
  CALL ckoptl(0,'TITLE',keyValue,srName,                                    &
              'Printing options: title line',irc,irCode,                    &
              maxVal=1,result1=opt%title)

! Stop if an error in the input options found
! -------------------------------------------
  IF (irCode /= 0) CALL exitrc(2)

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=iac)

  RETURN
  END SUBROUTINE rdlaui

END MODULE
