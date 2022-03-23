MODULE s_COS2SLR
CONTAINS

! -----------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -----------------------------------------------------------------------------

SUBROUTINE cos2slr(cospar,slrcos)

! -----------------------------------------------------------------------------
! Purpose:    Conversion of COSPAR number to ILRS number
!             e.g. 2000-039B (CHAMP) -> 0003902
!
! Author:     C. Urschl
!
! Created:    26-Nov-2003
! Last mod.:  __-___-____
!
! Changes:    __-___-____ __:
!
! SR used:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -----------------------------------------------------------------------------

  USE m_bern

  IMPLICIT NONE

! Parameters
! ----------
! In
  CHARACTER(LEN=9)             :: cospar  ! Cospar number

! Out
  CHARACTER(LEN=7)             :: slrcos  ! ILRS number

! Local Variables
! ---------------
  INTEGER(i4b)                 :: num
  CHARACTER(LEN=1)             :: abc


  slrcos(1:2) = cospar(3:4)
  slrcos(3:5) = cospar(6:8)

  READ(cospar(9:9),'(A)') abc
  num = ICHAR(abc)+1-ICHAR('A')

  IF (num < 10) THEN
    WRITE(slrcos(6:7),'(I1,I1)')0,num
  ELSE IF (num < 27) THEN
    WRITE(slrcos(6:7),'(I2)')num
  ELSE
    slrcos(6:7) = '??'
  ENDIF

  RETURN

END SUBROUTINE cos2slr

END MODULE
