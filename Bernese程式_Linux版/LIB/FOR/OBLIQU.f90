MODULE s_OBLIQU
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.0
! -------------------------------------------------------------------------

SUBROUTINE obliqu(xtdb,eps0,epsA)

! -------------------------------------------------------------------------
! Purpose:    Define mean obliquity at J2000.0 and at epoch
!
! Author:     U. Hugentobler
!
! Created:    05-Sep-2007
! Last mod.:  07-Jun-2011
!
! Changes:    06-May-2011 HB: IAU 2006 values added
!             19-May-2011 HB: Layout of error message corrected
!             07-Jun-2011 HB: Update of descriptions
!
! SR used:    --
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_const,  ONLY: pi, ars, const_def
  USE d_model,  ONLY: getModKey, mod_orb_nutmod, chrValLength
  USE s_exitrc
  IMPLICIT NONE

! List of parameters
! ------------------
! Input:
  REAL(r8b)          :: xtdb   ! Epoch in barycentric dynamical time
! Output:
  REAL(r8b)          :: eps0   ! Mean obliquity at J2000.0
  REAL(r8b)          :: epsA   ! Mean obliquity at epoch T

! Local variables
! ---------------
  REAL(r8b)          :: tu, numVal
  CHARACTER(LEN=16), SAVE :: nutNam
  CHARACTER(LEN=chrValLength) :: chrVal
  CHARACTER(LEN=8)  :: srNget

  LOGICAL, SAVE      :: first = .TRUE.


  IF (first) THEN
    first = .FALSE.

! Pi defined?
! -----------
    IF (const_def /= 1) THEN
      WRITE(lfnerr,'(A)') 'SR OBLIQU: Dear programmer, ' // &
           'constants are not defined - call SR DEFCON!'
      CALL exitrc(2)
    ENDIF

! Get nutation model
! ------------------
    chrVal = ' '
    CALL getModKey(mod_orb_nutMod,chrVal,srNget,numVal)
    nutNam = chrVal(1:16)
  ENDIF

! Time interval between epoch J2000.0 and given date (in Jul.Centuries)
! ---------------------------------------------------------------------
  tu = (xtdb - 51544.5d0) / 36525d0

! IAU2000
! =======
! Mean obliquity at epoch J2000 (rad)
! IERS TN No. 32, Ch. 5.5.2
! -----------------------------------
  IF (nutNam(1:10) == 'IAU2000   ' .OR. nutNam(1:10) == 'IAU80     ') THEN
    eps0 = 84381.448d0
    eps0 = eps0 / ars

! Mean obliquity at epoch J2000 (rad)
! IERS TN No. 32, Ch. 5.5.2, Eq.(32)
! -----------------------------------
    epsA = 84381.448d0   + &
         (  -46.84024d0 + &
         (   -0.00059d0 + &
              0.001813d0 *tu)*tu)*tu

! IAU2006
! =======
! Mean obliquity at epoch J2000 (rad),
! IERS TN No. 36, Ch. 5.6.2
! -----------------------------------
  ELSEIF (nutNam(1:10) == 'IAU2000R06' .OR. nutNam(1:10) == 'IAU2006   ') THEN
    eps0 = 84381.406d0
    eps0 = eps0 /ars

! Mean obliquity at epoch J2000 (rad)
! IERS TN No. 36, Ch. 5.6.2, Eq.(5.40)
! ------------------------------------
    epsA = 84381.406d0   + &
         (  -46.836769d0 + &
         (   -0.0001831d0 + &
         (    0.00200340d0 + &
         (   -0.000000576d0  &
             -0.0000000434d0 *tu)*tu)*tu)*tu)*tu
  ELSE
    WRITE(lfnerr,'(A,/,16X,A,A)') &
         ' *** SR OBLIQU: Nutation model is unknown ', &
                         'or not defined: ', TRIM(nutNam)
    CALL exitrc(2)
  ENDIF

  epsA = epsA / ars

  RETURN

END SUBROUTINE obliqu


END MODULE
