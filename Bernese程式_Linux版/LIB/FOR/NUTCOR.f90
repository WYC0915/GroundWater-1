MODULE s_NUTCOR
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE nutcor(xtdb,nutat,tu,dPsi,dEps,dtmoon,dPsiR,dEpsR)

! -------------------------------------------------------------------------
! Purpose:    Computation of nutation model correctins dPsi, dEps for
!             epoch xtdb.
!
! Author:     C. Urschl
!
! Created:    17-Dec-2002
! Last mod.:  07-Jun-2011
!
! Changes:    04-Jun-2003 PS: added rates for dPsi and dEps
!             11-Jun-2003 HU: Compute and output GST correction due to moon
!                             IERS Conv. 2000
!             14-Jun-2003 HU: R*4 problem corrected
!             16-Jun-2003 HU: Apply corrections to precession IAU76
!             01-Jul-2003 PS: rates are now optional
!             28-Jul-2003 PS: corrected initialization of rates
!             04-Jul-2005 HU: Move precession rates to SR NUTMTX
!                             Non-polynomial part of GST following IERS2003
!                             Conventions
!             06-May-2011 HB: Get model names through d_model,
!                             re-activate V50
!             07-Jun-2011 HB: Update of descriptions
!
! SR used:    exitrc
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_const,  ONLY: pi, const_def
  USE d_nutmod, ONLY: t_nutat, maxarg
  USE d_model,  ONLY: getModKey, mod_orb_nutMod, mod_orb_prcMod, &
                      chrValLength

  USE s_exitrc
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  TYPE(t_nutat)             :: nutat ! Nutation model parameters

  REAL(r8b)                 :: xtdb  ! Epoch in barycentric dynamical time

! input/output:

! output:
  REAL(r8b)                 :: tu     ! Time interval between J2000.0 and xtdb
  REAL(r8b)                 :: dPsi   ! Longitude
  REAL(r8b)                 :: dEps   ! Obliquity
  REAL(r8b)                 :: dtMoon ! Correction to GMST
  REAL(r8b),OPTIONAL        :: dPsiR  ! Rate in Longitude [rad/day]
  REAL(r8b),OPTIONAL        :: dEpsR  ! Rate in Obliquity [rad/day]

! List of functions
! -----------------

! Local types
! -----------

! Local parameters
! ----------------

! Local variables
! ---------------
  INTEGER(i4b)                 :: iarg
  INTEGER(i4b)                 :: iper
  INTEGER(i4b)                 :: icoeff
  INTEGER(i4b)                 :: ncoeff

  REAL(r8b), DIMENSION(maxarg) :: farg,fargr
  REAL(r8b)                    :: arg,argr
  REAL(r8b)                    :: sarg, carg
  REAL(r8b)                    :: tun
  REAL(r8b)                    :: roh

  LOGICAL, SAVE    :: first = .TRUE.
  CHARACTER(LEN=chrValLength), SAVE :: nutMod, prcMod
  CHARACTER(LEN=8) :: srNget
  REAL(r8b)        :: numVal

! Arguments and coefficients of nonpolynomial part of GST
! IERS TN NO.36, Ch.5.5.7, Table 5.2e (e-Version)
! -------------------------------------------------------
  INTEGER(i4b), DIMENSION(14,33), PARAMETER :: sidarg =                &
             reshape( source =                                         &
             (/0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  0,  2, -2,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  0,  2, -2,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  0,  2, -2,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  0,  2,  0,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  0,  2,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  0,  0,  0,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  1,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  1,  0,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               1,  0,  0,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               1,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  1,  2, -2,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  1,  2, -2,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  0,  4, -4,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  0,  1, -1,  1,  0, -8, 12,  0,  0,  0,  0,  0,  0,  &
               0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  0,  2,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               1,  0,  2,  0,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               1,  0,  2,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  0,  2, -2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  1, -2,  2, -3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  1, -2,  2, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  0,  0,  0,  0,  0,  8,-13,  0,  0,  0,  0,  0, -1,  &
               0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               2,  0, -2,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               1,  0,  0, -2,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  1,  2, -2,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               1,  0,  0, -2, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  0,  4, -2,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               0,  0,  2, -2,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               1,  0, -2,  0, -3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
               1,  0, -2,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0/),&
             shape = (/14,33/) )

  REAL(r8b), DIMENSION(2,33), PARAMETER :: sidcoe = &
             reshape( source =                      &
                           (/ 2640.96D0,  -0.39D0,  &
                                63.52D0,  -0.02D0,  &
                                11.75D0,   0.01D0,  &
                                11.21D0,   0.01D0,  &
                                -4.55D0,   0.00D0,  &
                                 2.02D0,   0.00D0,  &
                                 1.98D0,   0.00D0,  &
                                -1.72D0,   0.00D0,  &
                                -1.41D0,  -0.01D0,  &
                                -1.26D0,  -0.01D0,  &
                                -0.63D0,   0.00D0,  &
                                -0.63D0,   0.00D0,  &
                                 0.46D0,   0.00D0,  &
                                 0.45D0,   0.00D0,  &
                                 0.36D0,   0.00D0,  &
                                -0.24D0,  -0.12D0,  &
                                 0.32D0,   0.00D0,  &
                                 0.28D0,   0.00D0,  &
                                 0.27D0,   0.00D0,  &
                                 0.26D0,   0.00D0,  &
                                -0.21D0,   0.00D0,  &
                                 0.19D0,   0.00D0,  &
                                 0.18D0,   0.00D0,  &
                                -0.10D0,   0.05D0,  &
                                 0.15D0,   0.00D0,  &
                                -0.14D0,   0.00D0,  &
                                 0.14D0,   0.00D0,  &
                                -0.14D0,   0.00D0,  &
                                 0.14D0,   0.00D0,  &
                                 0.13D0,   0.00D0,  &
                                -0.11D0,   0.00D0,  &
                                 0.11D0,   0.00D0,  &
                                 0.11D0,   0.00D0/),&
                              shape = (/2,33/) )

  REAL(r8b), DIMENSION(2), PARAMETER :: sidcoet = (/ -0.87D0, 0.00D0 /)

  IF (first) THEN
    first=.FALSE.

! Pi defined?
! -----------
    IF (const_def /= 1) THEN
      WRITE(lfnerr,'(A)') 'SR NUTCOR: Dear programmer, ' // &
                          'constants are not defined - call SR DEFCON!'
      CALL exitrc(2)
    ENDIF

! Get nutMod- and prcMod-string
! -----------------------------
    CALL getModKey(mod_orb_nutMod,nutMod,srNget,numVal)
    CALL getModKey(mod_orb_prcMod,prcMod,srNget,numVal)

  ENDIF

  roh = pi/648000d0

! Time interval between epoch J2000.0 and given date (in Jul.Centuries)
! ---------------------------------------------------------------------
  tu = (xtdb - 51544.5d0) / 36525d0

! Compute fundamental arguments (in rad)
! --------------------------------------
! farg( 1)...Mean anomaly of the moon
! farg( 2)...Mean anomaly of the sun
! farg( 3)...Mean argument of the latitude of the moon
! farg( 4)...Mean elongation of the moon from the sun
! farg( 5)...Mean longitude of the ascending node of the moon
! farg( 6)...Mean longitude of merkur
! farg( 7)...Mean longitude of venus
! farg( 8)...Mean longitude of earth
! farg( 9)...Mean longitude of mars
! farg(10)...Mean longitude of jupiter
! farg(11)...Mean longitude of saturn
! farg(12)...Mean longitude of uranus
! farg(13)...Mean longitude of neptun
! farg(14)...Mean longitude of general precession in longitude

  ncoeff = 5

  DO iarg = 1, maxarg

    farg(iarg) = 0d0
    fargr(iarg)= 0d0
    tun = 1d0
    DO icoeff = 1, ncoeff
      farg(iarg) = farg(iarg) + nutat%nutfar(icoeff,iarg)*tun
      tun = tun*tu
    ENDDO

    tun = 1d0
    DO icoeff = 2, ncoeff
      fargr(iarg) = fargr(iarg) + nutat%nutfar(icoeff,iarg) &
                    * tun * icoeff    !! not icoef-1 ???
      tun = tun*tu
    ENDDO

    farg(iarg) = MOD(farg(iarg),1296000d0) * roh
    farg(iarg) = farg(iarg) + nutat%nutfar(ncoeff+1,iarg)*tu*2d0*pi

    fargr(iarg) = MOD(fargr(iarg),1296000d0) * roh
    fargr(iarg) = fargr(iarg) + nutat%nutfar(ncoeff+1,iarg)*2d0*pi
    fargr(iarg) = fargr(iarg)/36525.D0
  ENDDO

! dPsi, dEps
! ----------
! Summation of nutation series
  dPsi = 0d0
  dEps = 0d0

  IF(PRESENT(dPsiR).AND.PRESENT(dEpsR)) THEN
   dPsiR= 0d0
   dEpsR= 0d0
  ENDIF

  DO iper = 1, nutat%nnut

    arg = 0d0
    argr= 0d0
    DO iarg = 1, maxarg
      arg = arg + nutat%nutmlt(iarg,iper) * farg(iarg)
      argr=argr + nutat%nutmlt(iarg,iper) * fargr(iarg)
    ENDDO
    sarg = DSIN(arg)
    carg = DCOS(arg)

    dPsi = dPsi + (nutat%nutcoe(1,iper) + nutat%nutcoe(2,iper)*tu) * sarg  &
                + (nutat%nutcoe(3,iper) + nutat%nutcoe(4,iper)*tu) * carg
    dEps = dEps + (nutat%nutcoe(5,iper) + nutat%nutcoe(6,iper)*tu) * carg  &
                + (nutat%nutcoe(7,iper) + nutat%nutcoe(8,iper)*tu) * sarg

    IF(PRESENT(dPsiR).AND.PRESENT(dEpsR)) THEN
        dPsiR = dPsiR + (nutat%nutcoe(1,iper) + nutat%nutcoe(2,iper)*tu) &
                  * carg * argr &
                  +  nutat%nutcoe(2,iper) * sarg /36525.D0 &
                  - (nutat%nutcoe(3,iper) + nutat%nutcoe(4,iper)*tu) &
                  * sarg * argr &
                  +  nutat%nutcoe(4,iper) * carg /36525.D0

        dEpsR = dEpsR - (nutat%nutcoe(5,iper) + nutat%nutcoe(6,iper)*tu) &
                  * sarg * argr &
                  +  nutat%nutcoe(6,iper) * carg / 36525.D0 &
                  + (nutat%nutcoe(7,iper) + nutat%nutcoe(8,iper)*tu) &
                  * carg * argr &
                  +  nutat%nutcoe(8,iper) * sarg / 36525.D0
    ENDIF
  ENDDO


!!! Moved to NUTMTX
!!!! Correction of precession rates (as long as IAU76 precession is used)
!!!! ------------------------------
!!!  dPsi = dPsi + (nutat%nutpre(3)*tu )*1000d0
!!!  dEps = dEps + (nutat%nutpre(4)*tu )*1000d0

! dPsi and dEps in radians, dPsiR and dEpsR in radians/day
  dPsi = dPsi * roh/1000d0
  dEps = dEps * roh/1000d0

  IF(PRESENT(dPsiR).AND.PRESENT(dEpsR)) THEN
    dPsiR = dPsiR * roh/1000d0
    dEpsR = dEpsR * roh/1000d0
  ENDIF

! Non-polynomial part of GST
! --------------------------
  dtmoon = 0D0
  DO icoeff = 33, 1, -1
    arg = 0d0
    DO iarg = 1, maxarg
      arg = arg + sidarg(iarg,icoeff) * farg(iarg)
    ENDDO
    sarg = DSIN(arg)
    carg = DCOS(arg)

    dtmoon = dtmoon + sidcoe(1,icoeff) * sarg + sidcoe(2,icoeff) * carg
  ENDDO
  dtmoon = dtmoon + tu * (sidcoet(1) * sarg + sidcoet(2) * carg)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!! Activation for V50 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Correction to GMST, old method, before IERS Conventions 2003 !!!!!
  IF (nutMod(1:10) /= 'IAU2000R06' .AND. nutMod(1:10) /= 'IAU2006' .AND.&
      prcMod(1:4) == 'V50 ') THEN
    dtmoon = - (-2640.73D0 * DSIN(farg(5))                        &
                  -63.53D0 * DSIN(2*farg(5))                      &
                  -11.75D0 * DSIN(2*farg(3)-2*farg(4)+3*farg(5))  &
                  -11.21D0 * DSIN(2*farg(3)-2*farg(4)+farg(5))    &
                  +4.57D0 * DSIN(2*farg(3)-2*farg(4)+2*farg(5))  &
                  -2.02D0 * DSIN(2*farg(3)+3*farg(5))            &
                  -1.98D0 * DSIN(2*farg(3)+farg(5))              &
                  +1.72D0 * DSIN(3*farg(5))                      &
                  +1.41D0 * DSIN(farg(2)+farg(5))                &
                  +1.26D0 * DSIN(farg(2)-farg(5))                &
                  +0.63D0 * DSIN(farg(1)+farg(5))                &
                  +0.63D0 * DSIN(farg(1)-farg(5))                &
                  -0.87D0*tu * DSIN(farg(5)))
  ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  dtmoon = dtmoon * roh * 1D-6

  RETURN

END SUBROUTINE nutcor

END MODULE
