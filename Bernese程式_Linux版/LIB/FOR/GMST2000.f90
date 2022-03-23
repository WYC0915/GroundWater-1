MODULE f_gmst2000
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION gmst2000(ut1,tt)

! -------------------------------------------------------------------------
! Purpose:    Computation of Greenwich Mean Sidereal Time in accordance
!             with IERS 2000 Resolutions.
!
! Remarks:    Add NUT(2,1) and DTMOON from NUTCOR
!
!             IERS 2000 Conventions (McCarthy), Chapter 5
!
! Author:     U. Hugentobler
!
! Created:    06-Jun-2003
!
! Changes:    04-Jul-2005 HU: ut1 and tt as t_epoch-type
!             06-May-2011 HB: IERS2010 conventions for GST
!             07-Jun-2011 HB: Update of descriptions
!             04-May-2012 RD: Use DMOD from module, use m_bern with only
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnerr
  USE m_epoch,  ONLY: t_epoch
  USE d_const,  ONLY: pi, const_def
  USE d_model,  ONLY: getModKey, mod_orb_subMod, chrValLength
  USE l_basfun, ONLY: dmod

  USE s_exitrc
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  TYPE(t_epoch)             :: ut1            ! MJD Epoch in UT1
  TYPE(t_epoch)             :: tt             ! MJD Epoch in TT

! output:
  REAL(r8b)                 :: gmst2000       ! Mean sidereal time

! Local variables
! ---------------
  REAL(r8b)                 :: tu,t,gst

  LOGICAL, SAVE    :: first = .TRUE.
  CHARACTER(LEN=chrValLength), SAVE :: subMod
  CHARACTER(LEN=8) :: srNget
  REAL(r8b)        :: numVal

  IF (first) THEN
    first=.FALSE.

! Pi defined?
! -----------
    IF (const_def /= 1) THEN
      WRITE(lfnerr,'(A)') 'SR GMST2000: Dear programmer, ' // &
                          'constants are not defined - call SR DEFCON!'
      CALL exitrc(2)
    ENDIF

! Get IERS Conventions string
! ---------------------------
    CALL getModKey(mod_orb_subMod,subMod,srNget,numVal)

  ENDIF

! Julian days elapsed since 2000,1,1.5
! ------------------------------------
  tu = (ut1%day-51544.5D0)+ut1%frac

! Earth Rotation Angle, IERS TN No.36, Ch. 5.5.3 Eq.(5.14/15)
! -----------------------------------------------------------
  gmst2000 = 0.7790572732640D0 + 0.00273781191135448D0 * tu
  gmst2000 = DMOD (gmst2000,1D0) + DMOD (ut1%frac+0.5D0,1D0)
  gmst2000 = 2*pi * gmst2000

! Correction for Greenwich Sidereal Time referring to CEO
! -------------------------------------------------------
  t = ((tt%day-51544.5D0)+tt%frac)/36525D0

! IERS2003 convention: IERS TN No.32, Ch.5.7, Eq.(35)
! -------------------------------------------------------
  IF (subMod(1:8) /= 'IERS2010') THEN
    gst =    0.014506D0   +    &
         (4612.15739966D0 +    &
         (   1.39667721D0 +    &
         (  -0.00009344D0 +    &
             0.00001882D0 * t)*t)*t)*t

! IERS2010 convention: IERS TN No.36, Ch.5.5.7, Eq.(5.32)
! -------------------------------------------------------
  ELSE
    gst =    0.014506D0   +    &
         (4612.156534D0   +    &
         (   1.3915817D0  +    &
         (  -0.00000044D0 +    &
         ( -0.000029956D0      &
           -0.0000000368D0* t)*t)*t)*t)*t
  ENDIF

  gst = gst*pi/648000D0


  gmst2000 = gmst2000 + gst
  gmst2000 = DMOD (gmst2000,2*pi)
  IF (gmst2000<0D0) gmst2000 = gmst2000 + 2*pi

  RETURN

END FUNCTION gmst2000

END MODULE
