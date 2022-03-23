
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_nutmod

! -------------------------------------------------------------------------
! Purpose:    This module defines structures for nutation programs.
!
! Author:     C. Urschl
!
! Created:    13-Dec-2002
! Last mod.:  12-Aug-2003
!
! Changes:    13-May-2003 CU: Nullify pointers
!             16-Jun-2003 HU: Dimension of nutpre extended to 4
!             12-Aug-2003 PS: Corrected wrong comment
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern

! Dimensions
! ----------
  INTEGER(i4b), PARAMETER  :: maxarg   = 14   ! Max. number of fundamental arguments
  INTEGER(i4b), PARAMETER  :: maxcoeff = 8    ! Max. number of coefficients

! Parameter definition
! --------------------
  TYPE t_nutat
    INTEGER(i4b)                    :: ivers  ! Format version number (not used)
    INTEGER(i4b)                    :: nnut   ! Number of nutation periods
    INTEGER(i4b),DIMENSION(:,:), POINTER ::nutmlt ! Multipliers of the
                                              ! fundamental
                                              ! arguments for each nutation
                                              ! term
                                              ! (period)

    REAL(r8b), DIMENSION(4)         :: nutpre ! Corrections to precession
                                              ! offset in arcsec at J2000.0
                                              ! rate in arcsec per century
                                              ! i=1: offset in longitude
                                              ! i=2: offset in obliquity
                                              ! i=3: rate in longitude
                                              ! i=4: rate in obliquity
    REAL(r8b), DIMENSION(6,maxarg)  :: nutfar ! coefficients to compute
                                              ! fundamental arguments
                                              ! i=1,..5: terms with dt**(i-1) in
                                              !          arcsec per century etc.
                                              ! i=6    : number of full
                                              !          revolutions
                                              !          corresponding to
                                              !          1296000"
                                              ! j= 1: l = mean anomaly of the
                                              !           moon
                                              ! j= 2: l'= mean anomaly of the
                                              !           sun
                                              ! j= 3: f = mean longitude of the
                                              !           moon - o
                                              ! j= 4: d = mean longitude of the
                                              !           moon - mean longitude
                                              !           of the sun
                                              ! j= 5: o = mean longitude of
                                              !           the node of the moon
                                              ! j= 6: lq= mean longitude of
                                              !           merkur
                                              ! j= 7: lv= mean longitude of
                                              !           venus
                                              ! j= 8: le= mean longitude of
                                              !           earth
                                              ! j= 9: lm= mean longitude of mars
                                              ! j=10: lj= mean longitude of
                                              !           jupiter
                                              ! j=11: ls= mean longitude of
                                              !           saturn
                                              ! j=12: lu= mean longitude of
                                              !           uranus
                                              ! j=13: ln= mean longitude of
                                              !           neptun
                                              ! j=14: pa= mean longitude of
                                              !           general
                                              !           precession in
                                              !           longitude
    REAL(r8b), DIMENSION(:),   POINTER :: nutper ! Approx. nutation periods in
                                                 ! days
    REAL(r8b), DIMENSION(:,:), POINTER :: nutcoe ! Coefficients of nutation
                                                 ! model
                                              ! in mas and mas/c
                                              !      IERS BERNESE
                                              ! i=1: A      LS
                                              ! i=2: A'     LS'
                                              ! i=3: A''    LC
                                              ! i=4: A'''   LC'
                                              ! i=5: B      OC
                                              ! i=6: B'     OC'
                                              ! i=7: B''    OS
                                              ! i=8: B''',  OS'
    CHARACTER(LEN=fileNameLength)   :: filnut ! Nutation model file
    CHARACTER(LEN=shortLineLength)  :: title  ! Title in nutation model file
    CHARACTER(LEN=16)               :: nutnam ! Nutation model name
  END TYPE t_nutat

! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_nutat(nutat)
    TYPE(t_nutat)  :: nutat

    NULLIFY(nutat%nutmlt)
    NULLIFY(nutat%nutper)
    NULLIFY(nutat%nutcoe)
  END SUBROUTINE init_nutat

END MODULE d_nutmod


