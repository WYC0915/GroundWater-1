
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_const

! -------------------------------------------------------------------------
! Purpose:    This module defines global variables containing the
!             constants read from the CONST. file by SR DEFCON
!
! Author:     U. Hugentobler
!
! Created:    10-Sep-2001
! Last mod.:  12-Apr-2011
!
! Changes:    16-Jun-2005 MM: COMCONST.inc removed
!             27-Dec-2005 HU: const_def initialized
!             16-Oct-2006 AG: AU added
!             27-Feb-2007 AG: ars and rho added
!             27-May-2007 AG: freqe added
!             11-Mar-2010 SL: ONLY added to USE m_bern
!             12-Apr-2011 CR: J2 added
!             01-May-2012 LP: FREQS, FREQC, FREQJ, and FREQ(3) added
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,    ONLY: i4b, r8b

! Constants
! ---------
  CHARACTER(LEN=5)        :: time
  CHARACTER(LEN=9)        :: date
  CHARACTER(LEN=80)       :: filTitle
!
  INTEGER(i4b)            :: const_def = 0
!
  REAL(r8b)               :: pi
  REAL(r8b)               :: rho
  REAL(r8b)               :: ars
  REAL(r8b)               :: c
  REAL(r8b)               :: GM
  REAL(r8b)               :: GMM
  REAL(r8b)               :: GMS
  REAL(r8b)               :: AU
  REAL(r8b)               :: AE
  REAL(r8b)               :: J2
  REAL(r8b)               :: conRE
  REAL(r8b)               :: facTec
  REAL(r8b)               :: P0
  REAL(r8b)               :: freqP
  REAL(r8b)               :: omega
  REAL(r8b)               :: ETUT
  REAL(r8b)               :: wgtPha
  REAL(r8b)               :: wgtCod
  REAL(r8b)               :: hRef
  REAL(r8b)               :: pRef
  REAL(r8b)               :: tRef
  REAL(r8b)               :: humRef
  REAL(r8b), DIMENSION(3) :: freq     ! GPS frequencies
  REAL(r8b), DIMENSION(2) :: freqg    ! GLONASS frequencies somehow handled via COMFREQ.inc
  REAL(r8b), DIMENSION(5) :: freqe    ! GALILEO frequencies
  REAL(r8b), DIMENSION(2) :: freqs    ! SBAS frequencies
  REAL(r8b), DIMENSION(4) :: freqc    ! COMPASS/BEIDOU frequencies
  REAL(r8b), DIMENSION(4) :: freqj    ! QZSS frequencies
  REAL(r8b), DIMENSION(5) :: wlgth


END MODULE d_const

