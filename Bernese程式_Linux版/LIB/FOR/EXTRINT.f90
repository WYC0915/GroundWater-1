MODULE f_EXTRINT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION extrint(string,size)

! -------------------------------------------------------------------------
! Purpose:    Extract integers from a string - from the right.
!
! Author:     S.Lutz, R.Dach
!
! Created:    04-Oct-2010
!
! Changes:    16-Sep-2011 SL: if condition expanded
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b

! List of Parameters
! ------------------
! Input:
  CHARACTER(LEN=*)        :: string
  INTEGER(i4b),OPTIONAL   :: size

! Output:
  INTEGER(i4b)            :: extrInt

! Variables
! ---------
  INTEGER(i4b)            :: i,j
  INTEGER(i4b)            :: x

! Set default number
! ------------------
  extrInt = 0

! Extract integers from string
! ----------------------------
  j = 1
  DO i = LEN_TRIM(string),1,-1
    x = IACHAR(string(i:i))
    IF(x>47.AND.x<58) THEN
      extrInt = extrInt+(x-48)*j
      j = j*10
      IF(PRESENT(size)) THEN
        IF(j == 10**size) EXIT
      ENDIF
    ENDIF
  ENDDO

END FUNCTION extrint

END MODULE
