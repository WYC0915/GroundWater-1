MODULE s_CCINPT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE ccinpt(sigCod,q,iSave,iUsFlg,maxInt,iPrint)

! -------------------------------------------------------------------------
! Purpose:    This is a new version of the old subroutine CCINPT.f that
!             reads the input options of the program CODCHK
!
! Author:     D. Ineichen
!
! Created:    26-Oct-2001
! Last mod.:  23-Apr-2003
!
! Changes:    23-Apr-2003 CU: Nullify local pointers
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  USE s_ckoptr
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckopti
  IMPLICIT NONE

! List of parameters
! ------------------
! IN:

! OUT:
  REAL(r8b)             :: sigCod   ! Rms for one code observation
  INTEGER(i4b)          :: q        ! Polynomial degree used for screening
  INTEGER(i4b)          :: iSave    ! = 1 : Save marked observations
                                    ! = 0 : Do not change observation files
  INTEGER(i4b)          :: iUsFlg   ! Use (=1) or ignore (=0) obs file flags
  INTEGER(i4b)          :: maxInt   ! Maximum interval length for
                                    ! polynomial fit
  INTEGER(i4b)          :: iPrint   ! Print level
                                    ! = 0 : Print screening summary
                                    ! = 1 : Print extended screening info

! Local variables
! ---------------
  CHARACTER(LEN=fileNameLength)    :: srName
  INTEGER(i4b)                     :: irc, irCode

  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue

! Init variables
! --------------
  irCode = 0
  srName = 'ccinpt'

  NULLIFY(keyValue)

! Observation flags
! -----------------
  CALL readKeys('USEFLG',keyValue,irc)
  CALL ckoptb(1,(/'USEFLG'/),srName,'Use flags in file',irCode,             &
              result1=iUsFlg)

  CALL readKeys('SAVFLG',keyValue,irc)
  CALL ckoptb(1,(/'SAVFLG'/),srName,'Save flags in files',irCode,           &
              result1=iSave)

! Polynomial screening
! --------------------
  CALL readKeys('EXTINF',keyValue,irc)
  CALL ckoptb(1,(/'EXTINF'/),srName,'Print extended info',irCode,           &
              result1=iPrint)

  CALL readKeys('POLDEG',keyValue,irc)
  CALL ckopti(1,'POLDEG',keyValue,srName,'Polynomial degree',irc,irCode,    &
              maxVal=1,ge=1,le=99,error=0,result1=q)

  CALL readKeys('FITINT',keyValue,irc)
  CALL ckopti(1,'FITINT',keyValue,srName,'Max. fit interval',irc,irCode,    &
              maxVal=1,ge=1,error=0,result1=maxInt)

  CALL readKeys('OBSRMS',keyValue,irc)
  CALL ckoptr(1,'OBSRMS',keyValue,srName,'Rms of code observations',irc,    &
              irCode,maxVal=1,gt=0d0,error=99d0,result1=sigCod)

  DEALLOCATE(keyValue,stat=irc)

! Problems reading input options
! ------------------------------
  IF (irCode /= 0) CALL exitrc(2)

END SUBROUTINE ccinpt

END MODULE
