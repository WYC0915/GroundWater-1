MODULE s_SINPARTU
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE sinpartu(par, parType, unit)

! -------------------------------------------------------------------------
! Purpose:    This subroutine returns the SINEX parameter type and unit.
!
! Author:     L. Mervart
!
! Created:    05-Sep-1998
!
! Changes:    21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             22-Apr-2002 HU: Geocenter coordinates
!             16-Jun-2003 RS: Nutation parameters added
!             09-Jul-2003 RS: Troposphere gradients added
!             12-Aug-2003 RS: Change unit for nutation parameters (not con-
!                             sistent with SINEX 2.00 format description!)
!             09-Mar-2004 HU: Improved error message for unhandled params
!             22-Sep-2005 RD: Use new module D_PAR.f90
!             20-Apr-2006 AG: Satellite antenna offsets added
!             06-Jul-2009 DT: Add range biases
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_par,    ONLY: t_par

  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_par), INTENT(IN) :: par
  CHARACTER(LEN=*)        :: parType
  CHARACTER(LEN=*)        :: unit

  IF      (par%locq(1) == 1 .AND. par%locq(3) == 1 .AND. par%locq(4) /= 3) THEN
    parType = 'STAX  '
    unit    = 'm   '
  ELSE IF (par%locq(1) == 1 .AND. par%locq(3) == 2 .AND. par%locq(4) /= 3) THEN
    parType = 'STAY  '
    unit    = 'm   '
  ELSE IF (par%locq(1) == 1 .AND. par%locq(3) == 3 .AND. par%locq(4) /= 3) THEN
    parType = 'STAZ  '
    unit    = 'm   '
  ELSE IF (par%locq(1) == 1 .AND. par%locq(3) == 1 .AND. par%locq(4) == 3) THEN
    parType = 'VELX  '
    unit    = 'm/y '
  ELSE IF (par%locq(1) == 1 .AND. par%locq(3) == 2 .AND. par%locq(4) == 3) THEN
    parType = 'VELY  '
    unit    = 'm/y '
  ELSE IF (par%locq(1) == 1 .AND. par%locq(3) == 3 .AND. par%locq(4) == 3) THEN
    parType = 'VELZ  '
    unit    = 'm/y '
  ELSE IF (par%locq(1) == 10  .AND. &
           par%locq(4) ==  1  .AND. &
           par%locq(5) ==  1) THEN
    parType = 'XPO   '
    unit    = 'mas '
  ELSE IF (par%locq(1) == 10  .AND. &
           par%locq(4) ==  2  .AND. &
           par%locq(5) ==  1) THEN
    parType = 'YPO   '
    unit    = 'mas '
  ELSE IF (par%locq(1) == 10  .AND. &
           par%locq(4) ==  3  .AND. &
           par%locq(5) ==  1) THEN
    parType = 'UT    '
    unit    = 'ms  '
  ELSE IF (par%locq(1) == 10  .AND. &
           par%locq(4) ==  1  .AND. &
           par%locq(5) ==  2) THEN
    parType = 'XPOR  '
    unit    = 'ma/d'
  ELSE IF (par%locq(1) == 10  .AND. &
           par%locq(4) ==  2  .AND. &
           par%locq(5) ==  2) THEN
    parType = 'YPOR  '
    unit    = 'ma/d'
  ELSE IF (par%locq(1) == 10  .AND. &
           par%locq(4) ==  3  .AND. &
           par%locq(5) ==  2) THEN
    parType = 'LOD   '
    unit    = 'ms  '
  ELSE IF (par%locq(1) == 10  .AND. &
           par%locq(4) ==  4  .AND. &
           par%locq(5) ==  1) THEN
    parType = 'NUT_OB'
    unit    = 'mas '
  ELSE IF (par%locq(1) == 10  .AND. &
           par%locq(4) ==  5  .AND. &
           par%locq(5) ==  1) THEN
    parType = 'NUT_LN'
    unit    = 'mas '
  ELSE IF (par%locq(1) == 10  .AND. &
           par%locq(4) ==  4  .AND. &
           par%locq(5) ==  2) THEN
    parType = 'NUTROB'
    unit    = 'ma/d'
  ELSE IF (par%locq(1) == 10  .AND. &
           par%locq(4) ==  5  .AND. &
           par%locq(5) ==  2) THEN
    parType = 'NUTRLN'
    unit    = 'ma/d'
  ELSE IF (par%locq(1) ==  6  .AND. &
           par%locq(4) ==  3) THEN
    parType = 'TROTOT'
    unit    = 'm   '
  ELSE IF (par%locq(1) ==  6  .AND. &
           par%locq(4) ==  1) THEN
    parType = 'TGNTOT'
    unit    = 'm   '
  ELSE IF (par%locq(1) ==  6  .AND. &
           par%locq(4) ==  2) THEN
    parType = 'TGETOT'
    unit    = 'm   '
  ELSE IF (par%locq(1) == 16  .AND. &
           par%locq(2) ==  1) THEN
    parType = 'XGC   '
    unit    = 'm   '
  ELSE IF (par%locq(1) == 16  .AND. &
           par%locq(2) ==  2) THEN
    parType = 'YGC   '
    unit    = 'm   '
  ELSE IF (par%locq(1) == 16  .AND. &
           par%locq(2) ==  3) THEN
    parType = 'ZGC   '
    unit    = 'm   '
  ELSE IF (par%locq(1) == 12  .AND. &
           par%locq(3) ==  1) THEN
    parType = 'SATA_X'
    unit    = 'm   '
  ELSE IF (par%locq(1) == 12  .AND. &
           par%locq(3) ==  2) THEN
    parType = 'SATA_Y'
    unit    = 'm   '
  ELSE IF (par%locq(1) == 12  .AND. &
           par%locq(3) ==  3) THEN
    parType = 'SATA_Z'
    unit    = 'm   '
  ELSE IF (par%locq(1) == 26 ) THEN
    parType = 'RBIAS '
    unit    = 'm   '
  ELSE
    WRITE(lfnerr, "(/,' *** Sinpartu: Parameter type not handled for SINEX:',I6, &
                 &  /,'               SINEX file cannot be written.',   &
                 &  /,'               Pre-eliminate parameter before',  &
                 &                  ' writing the SINEX.')") &
                     par%locq(1)
    CALL exitrc(2)
  END IF

END SUBROUTINE sinpartu


END MODULE
