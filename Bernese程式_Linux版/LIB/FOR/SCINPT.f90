MODULE s_SCINPT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE scinpt(title,window)

! -------------------------------------------------------------------------
!
! Purpose:    This is a new version of the old subroutine SCINPT.f that
!             reads the input options of the program SATCLK
!
! Author:     C. Urschl
!
! Created:    05-Oct-2000
! Last mod.:  20-May-2003
!
! Changes:    23-Apr-2003 RD: Nullify local pointers
!             20-May-2003 RD: Use SR gttimwin instead of readsess
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  USE s_readkeys
  USE s_priwin
  USE s_gttimwin
  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=53)          :: title       ! general title line
  REAL(r8b), DIMENSION(2)    :: window      ! window(1) = START (in MJD)
                                            ! window(2) = END   (in MJD)

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue
  INTEGER(i4b)                                          :: irc

! Init variables
! --------------
  NULLIFY(keyValue)

! Read Title
! ----------
  CALL readkeys('TITLE', keyValue, irc)
  title = keyValue(1)


! Read Time Options
! -----------------
  CALL gttimwin(' ',(/'RADIO_0','RADIO_1','RADIO_2'/),   &
                (/'SESSION_YEAR','SESSION_STRG'/),       &
                (/'STADAT','STATIM','ENDDAT','ENDTIM'/), &
                window)

  CALL priwin(1,window)


! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

END SUBROUTINE scinpt

END MODULE
