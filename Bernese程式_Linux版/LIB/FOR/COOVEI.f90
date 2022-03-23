MODULE s_COOVEI
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE coovei(title,tref)

! -------------------------------------------------------------------------
!
! Purpose:    This is a new version of the old subroutine COOVEI.f that
!             reads the input options of the program COOVEL
!
! Author:     C. Urschl
!
! Created:    13-Sep-2000
! Last mod.:  20-Jan-2011
!
! Changes:    23-Apr-2003 CU: Nullify local pointers
!             20-Jan-2011 RD: Use the CKOPT-routines
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  USE s_readkeys
  USE s_exitrc
  USE s_ckoptd
  USE s_ckoptt
  USE s_ckoptl
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:

! output:
  CHARACTER(LEN=shortLineLength)  :: title              ! Title line
  REAL(r8b)                       :: tRef               ! Reference epoch

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'coovei'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER     :: keyValue
  REAL(r8b)                                                :: thour
  INTEGER(i4b)                                             :: irc, ioerr


  ioerr = 0
  NULLIFY(keyValue)

! Get title line
! --------------
  CALL readkeys('TITLE', keyValue, irc)
  CALL ckoptl(0,'TITLE',keyValue,srName,                       &
              'Title line',irc,ioerr,                          &
              empty=' ',maxVal=1,result1=title)


! Read Reference Epoch
! --------------------
  CALL readkeys('REFDAT',keyValue,irc)
  CALL ckoptd(1,'REFDAT',keyValue,srName,                      &
              'Date for reference epoch',irc,ioerr,            &
              maxVal = 1,result1=tref)

  CALL readkeys('REFTIM',keyValue,irc)
  CALL ckoptt(1,'REFTIM',keyValue,srName,                      &
              'Time for reference epoch',irc,ioerr,            &
              maxVal = 1,result1=thour)

  tref=tref+thour/24d0

  DEALLOCATE(keyValue,stat=irc)

  IF (ioerr /= 0)  CALL exitrc(2)

END SUBROUTINE coovei

END MODULE
