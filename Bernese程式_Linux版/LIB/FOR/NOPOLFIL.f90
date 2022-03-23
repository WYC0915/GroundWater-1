MODULE f_NOPOLFIL
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION nopolfil(polfil)

! -------------------------------------------------------------------------
!
! Purpose:    Check filename for key "NOPOLFIL"
!
!
! Author:     U. Hugentobler
!
! Created:    06-Jan-2005
! Lasst mod.: __-___-____
!
! Changes:    __-___-____ __:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE s_stripext
  IMPLICIT NONE

! In:
  CHARACTER(LEN=*) :: polfil              ! Pole filename

! Out:
  LOGICAL          :: nopolfil            ! .TRUE. : filename is "NOPOLFIL"
                                          ! .FALSE.: else

! Local variables
! ---------------
  LOGICAL          :: first = .TRUE.
  CHARACTER(LEN=255) :: filnam

  nopolfil = .FALSE.

  IF (polfil == '') RETURN

  filnam=polfil
  CALL stripext(filnam)

  IF (filnam == 'NOPOLFIL') THEN
    nopolfil=.TRUE.

    IF (first) THEN
      WRITE(lfnprt,"(/,' ### SR NOPOLFIL: No pole file used, ', &
                                       & 'Keyword NOPOLFIL specified',/)")
      first=.FALSE.
    ENDIF
  ENDIF

END FUNCTION nopolfil

END MODULE
