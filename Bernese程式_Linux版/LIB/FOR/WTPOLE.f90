MODULE s_WTPOLE
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE wtpole(lfn, polTim, polcoo, rmspol, rate, rmsrat, correl, &
                  nsta, nfix, nsat)

! -------------------------------------------------------------------------
! Purpose:    Write one line of the pole file in IERS format
!
! Author:     L. Mervart
!
! Created:    22-NOV-1997
! Last mod.:  __-___-____
!
! Changes:    __-___-____ __:
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b)               :: lfn
  REAL(r8b)                  :: polTim
  REAL(r8b),    DIMENSION(5) :: polcoo
  REAL(r8b),    DIMENSION(5) :: rmspol
  REAL(r8b),    DIMENSION(5) :: rate
  REAL(r8b)   , DIMENSION(5) :: rmsrat
  REAL(r8b)   , DIMENSION(3) :: correl
  INTEGER(i4b)               :: nsta
  INTEGER(i4b)               :: nfix
  INTEGER(i4b)               :: nsat

! Local Variables
! ---------------
  INTEGER(i4b)               :: icrd
  INTEGER(i4b), DIMENSION(5) :: ipolco
  INTEGER(i4b), DIMENSION(5) :: irmspol
  INTEGER(i4b), DIMENSION(5) :: irate
  INTEGER(i4b), DIMENSION(5) :: irmsrat
  INTEGER(i4b), DIMENSION(3) :: icorre
  INTEGER(i4b)               :: ii

  DO  icrd = 1,5
    IF (icrd /= 3) THEN
      ipolco(icrd)  = IDNINT(polcoo(icrd)*1000000.d0)
      irmspol(icrd) = IDNINT(rmspol(icrd)*1000000.d0)
      IF (icrd <= 2) THEN
        irate(icrd)   = IDNINT(rate(icrd)*1000000.d0)
        irmsrat(icrd) = IDNINT(rmsrat(icrd)*1000000.d0)
      ENDIF
    ELSE
      ipolco(icrd)  =  IDNINT(polcoo(icrd)*10000000.d0)
      irmspol(icrd) =  IDNINT(rmspol(icrd)*10000000.d0)
      irate(icrd)   = -IDNINT(rate(icrd)*10000000.d0)
      irmsrat(icrd) =  IDNINT(rmsrat(icrd)*10000000.d0)
    ENDIF
  END DO

  DO ii = 1, 3
    icorre(ii)= NINT(correl(ii)*100.D0)
  END DO

  WRITE (lfn,'(f8.2, 3i9, i7, 4i6, 3i4, 2i7, 2i6, 3i5, 2i7, 2i6)')           &
         polTim, (ipolco(icrd),icrd=1,3),irate(3), (irmspol(icrd),icrd=1,3), &
         irmsrat(3), nsta, nfix, nsat, (irate(icrd),icrd=1,2),               &
         (irmsrat(icrd),icrd=1,2), (icorre(icrd),icrd=1,3),                  &
         ipolco(5), ipolco(4), irmspol(5), irmspol(4)

END SUBROUTINE wtpole

END MODULE
