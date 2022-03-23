MODULE f_prn2prn
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION prn2prn(prn,mjd)

! -------------------------------------------------------------------------
! Purpose:    This function returns true PRN number in case of GNSS
!             renaming, or anomalies.
!
! Author:     S. Schaer
!
! Created:    08-Nov-2006
!
! Changes:    26-Feb-2007/ss: Another GLONASS anomaly
!             03-Apr-2007/ss: Another GLONASS anomaly
!             09-Jul-2007/ss: Another GLONASS anomaly
!             09-Oct-2008/ss: Another GLONASS anomaly
!             31-May-2012/lp: Add GIOVE for 2011+2012
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
  INTEGER(i4b) :: prn     ! Recorded PRN number
  REAL(r8b)    :: mjd     ! Time argument (MJD)

! Output:
  INTEGER(i4b) :: prn2prn ! Corrected PRN number

! Set PRN number
! --------------
  prn2prn = prn

! GPS renaming
! ------------
!!  IF (prn ==  32 .AND. mjd >= 48948d0 .AND. mjd < 49016d0) prn2prn =   1
  IF (prn ==  32 .AND. mjd >= 48948d0 .AND. mjd < 49023d0) prn2prn =   1

! GLONASS anomalies
! -----------------
! - IGS Mail 5234
  IF (prn == 119 .AND. mjd >= 53662d0 .AND. mjd < 53695d0) prn2prn = 121

! - IGS Mail 5574
  IF (prn == 110 .AND. mjd >= 54143d0 .AND. mjd < 54146d0) prn2prn = 114

  IF (prn == 117 .AND. mjd >= 54189d0 .AND. mjd < 54193d0) prn2prn = 107

  IF (prn == 118 .AND. mjd >= 54287d0 .AND. mjd < 54291d0) prn2prn = 122

  IF (prn == 118 .AND. mjd >= 54413d0 .AND. mjd < 54416d0) prn2prn = 119

  IF (prn == 118 .AND. mjd >= 54419d0 .AND. mjd < 54433d0) prn2prn = 120

  IF (prn == 121 .AND. mjd >= 54480d0 .AND. mjd < 54482d0) prn2prn = 111

  IF (prn == 121 .AND. mjd >= 54747d0 .AND. mjd < 54748d0) prn2prn = 117

!!  IF (prn == 106 .AND. mjd >= 55469d0 .AND. mjd < 55471d0) prn2prn = 104

!!  IF (prn == 103 .AND. mjd >= 55547d0 .AND. mjd < 55548d0) prn2prn = 114


! GIOVE naming mess
! -----------------
  IF (prn == 231 .AND. mjd >= 55562d0 .AND. mjd < 56292d0) prn2prn = 201
  IF (prn == 251 .AND. mjd >= 55562d0 .AND. mjd < 56292d0) prn2prn = 201

  IF (prn == 202 .AND. mjd >= 55562d0 .AND. mjd < 56292d0) prn2prn = 216
  IF (prn == 232 .AND. mjd >= 55562d0 .AND. mjd < 56292d0) prn2prn = 216
  IF (prn == 252 .AND. mjd >= 55562d0 .AND. mjd < 56292d0) prn2prn = 216

END FUNCTION prn2prn

END MODULE

