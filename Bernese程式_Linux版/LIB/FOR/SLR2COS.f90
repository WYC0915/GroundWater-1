MODULE s_SLR2COS
CONTAINS

! -----------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -----------------------------------------------------------------------------

SUBROUTINE slr2cos(slrcos,cospar)

! -----------------------------------------------------------------------------
! Purpose:    Conversion of ILRS number to COSPAR number
!             e.g. 0003902 -> 2000-039B (CHAMP)
!
! Remark:     Y2K compliant
!
! Author:     D. Svehla
!
! Created:    06-Oct-2002
! Last mod.:  05-May-2003
!
! Changes:    05-May-2003 HU: General routine, not LEO specific
!                         HU: Write '?' if char not capital letter
!
! SR used:    IYEAR4
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -----------------------------------------------------------------------------

  USE m_bern

  USE f_iyear4
  IMPLICIT NONE

! Parameters
! ----------
! In
  CHARACTER(LEN=9)             :: slrcos  ! ILRS number

! Out
  CHARACTER(LEN=9)             :: cospar  ! Cospar number

! Local Variables
! ---------------
  INTEGER(i4b)                 :: IY,INUM


  READ(slrcos(1:2),'(I2)') IY

  WRITE(cospar(1:4),'(I4)') IYEAR4(IY)
  cospar(5:5)='-'
  cospar(6:8)=slrcos(3:5)
  READ(slrcos(6:7),'(I2)') INUM
  IF (INUM > 0 .AND. INUM < 27) THEN
    INUM=INUM+ICHAR('A')-1
    cospar(9:9)=ACHAR(INUM)
  ELSE
    cospar(9:9)='?'
  ENDIF

  RETURN

END SUBROUTINE slr2cos

END MODULE
