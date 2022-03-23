MODULE f_asmode
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION asmode(mjd,prn)

! -------------------------------------------------------------------------
! Purpose:    This function returns the AS (Anti-Spoofing) mode of the GPS
!             constellation (or a selected GPS satellite).
!
! Author:     S. Schaer
!
! Created:    04-Feb-2009
!
! Changes:    09-Feb-2009/ss: Consider Anti-Spoof testing periods
!
! SR used:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  IMPLICIT NONE

! List of Parameters
! ------------------
! Input:
  REAL(r8b)    :: mjd     ! Time argument (MJD)
                          ! =0: undefined
  INTEGER(i4b) :: prn     ! PRN number
                          ! =0: GPS constellation in general

! Output:
  INTEGER(i4b) :: asmode  ! AS mode
                          ! =0: off
                          ! =1: on

! Set default AS mode
! -------------------
  asmode = 1

! Return if time argument undefined
! ---------------------------------
  IF (mjd == 0d0) RETURN

! Consider Anti-Spoof testing periods
! -----------------------------------
  IF (mjd > 48856.04167d0 .AND. mjd < 48858.41667d0 .AND. &
      (prn ==  0 .OR. prn ==  2 .OR. prn == 16 .OR. prn == 17 .OR. &
       prn == 20 .OR. prn == 21 .OR. prn == 23 .OR. prn == 24 .OR. &
       prn == 25 .OR. prn == 26 .OR. prn == 28               )) RETURN

  IF (mjd > 48884.04167d0 .AND. mjd < 48886.41667d0 .AND. &
      (prn ==  0 .OR. prn ==  2 .OR. prn == 16 .OR. prn == 17 .OR. &
       prn == 20 .OR. prn == 21 .OR. prn == 23 .OR. prn == 24 .OR. &
       prn == 25 .OR. prn == 26 .OR. prn == 28               )) RETURN

  IF (mjd > 48891.04167d0 .AND. mjd < 48893.41667d0 .AND. &
      (prn == 0  .OR. prn ==  2 .OR. prn == 16 .OR. prn == 17 .OR. &
       prn == 20 .OR. prn == 21 .OR. prn == 23 .OR. prn == 24 .OR. &
       prn == 25 .OR. prn == 26 .OR. prn == 28               )) RETURN

  IF (mjd > 48912.04167d0 .AND. mjd < 48914.41667d0 .AND. &
      (prn ==  0 .OR. prn ==  2 .OR. prn == 14 .OR. prn == 15 .OR. &
       prn == 16 .OR. prn == 17 .OR. prn == 20 .OR. prn == 21 .OR. &
       prn == 23 .OR. prn == 25 .OR. prn == 26 .OR. prn == 28)) RETURN

  IF (mjd > 48959.04167d0 .AND. mjd < 48963.41667d0 .AND. &
      (prn ==  0 .OR. prn == 14 .OR. prn == 15 .OR. prn == 16 .OR. &
       prn == 17 .OR. prn == 19 .OR. prn == 20 .OR. prn == 21 .OR. &
       prn == 23 .OR. prn == 24 .OR. prn == 25 .OR. prn == 26 .OR. &
       prn == 28 .OR. prn ==  0                              )) RETURN

  IF (mjd > 49044.50000d0 .AND. mjd < 49044.91667d0 .AND. &
      (prn ==  0 .OR. prn ==  1 .OR. prn ==  2 .OR. prn == 14 .OR. &
       prn == 15 .OR. prn == 17 .OR. prn == 19 .OR. prn == 20 .OR. &
       prn == 21 .OR. prn == 23 .OR. prn == 25 .OR. prn == 26 .OR. &
       prn == 27 .OR. prn == 28 .OR. prn == 29               )) RETURN

  IF (mjd > 49044.91667d0 .AND. mjd < 49046.08333d0 .AND. &
      (prn ==  0 .OR. prn ==  1 .OR. prn ==  2 .OR. prn == 14 .OR. &
       prn == 15 .OR. prn == 17 .OR. prn == 19 .OR. prn == 20 .OR. &
       prn == 21 .OR. prn == 23 .OR. prn == 25 .OR. prn == 26 .OR. &
       prn == 27 .OR. prn == 28 .OR. prn == 29               )) RETURN

! Consider non-AS satellites
! --------------------------
!!  IF (prn == 99 .AND. &
!!      mjd > 50480.99999d0 .AND. mjd < 50503.00000d0) THEN
!!    asmode = 0
!!    RETURN
!!  ENDIF

  IF (mjd >= 50503.00000d0) RETURN

! Check whether time argument in a AS-free period
! -----------------------------------------------
  IF ((mjd >           0d0 .AND. mjd < 49383.00000d0) .OR. &
      (mjd > 49826.87499d0 .AND. mjd < 49847.83334d0) .OR. &
      (mjd > 49886.99999d0 .AND. mjd < 49909.00002d0) .OR. &
      (mjd > 49999.99999d0 .AND. mjd < 50022.00001d0) .OR. &
      (mjd > 50480.99999d0 .AND. mjd < 50503.00000d0)) THEN
    asmode = 0
  ENDIF

END FUNCTION asmode

END MODULE

