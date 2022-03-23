
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE m_global

! -------------------------------------------------------------------------
! Purpose:    This module defines global variables concerning the
!             processing of satellite data to be used in various
!             routines of the software
!
! Author:     M.Rothacher
!
! Created:    06-Jun-2001
!
! Changes:    08-Nov-2002 HU: GALILEO system added
!             15-Jan-2003 RD: Add measurement type strings
!             27-Jan-2003 HU: Geostationary satellite identifier added
!             24-Aug-2005 AG: ANTEX strings added
!             04-Oct-2006 AG: Satellite system types for SVN added
!             27-Mar-2007 AG: Satellite system types for RINEX 3 added
!             24-Feb-2008 RD: Satellite system description added
!             17-Jun-2008 RD: MAXSYS added
!             11-Nov-2008 AS: antGal changed
!             12-Apr-2011 SS: g_syssvn adjusted for first GLONASS-K1
!             05-May-2011 LP: assign SVNs 2XX to GALILEO in g_syssvn
!             29-Sep-2011 SL: (SBAS,) Compass and QZSS added, maxSys 4->6
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  use m_bern, ONLY: i4b
  IMPLICIT NONE

! Satellite System Types: (0)=GPS, (1)=GLONASS, (2)=GALILEO, (3)=SBAS
!  (BERNESE intern)       (4)=COMPASS, (5)=QZSS, (9)=LEO, (10)=MIXED
! ----------------------------------------------------------
  INTEGER(i4b),     PARAMETER               :: MAXSYS=6  ! Max. number of
                                                         ! satellite systems
                                                         ! where observ. are
                                                         ! expected
  CHARACTER(LEN=1), DIMENSION(0:10)         :: g_svnsys = &
                    (/'G','R','E','S','C','J',' ',' ',' ','L','M'/)

  CHARACTER(LEN=3), DIMENSION(0:10)         :: g_strsys3= &
                    (/'GPS','GLO','GAL','GEO','COM','QZS',&
                      '   ','   ','   ','LEO','MIX'/)

  CHARACTER(LEN=7), DIMENSION(0:10)         :: g_strsys = &
                    (/'GPS    ','GLONASS','GALILEO','SBAS   ','COMPASS',&
                      'QZSS   ','       ','       ','       ','LEO    ',&
                      'GNSS   '/)

! Measurement Types: (1)=PHASE, (2)=CODE, (3)=RANGE
! -------------------------------------------------
  CHARACTER(LEN=1), DIMENSION(3)            :: g_meatyp = (/'P','C','R'/)

  CHARACTER(LEN=5), DIMENSION(3), PARAMETER :: g_meaStr = &
                    (/ 'PHASE', 'CODE ', 'RANGE' /)

! ANTEX Satellite System Types: (0)=GPS, (1)=GPS, (2)=GLONASS, (3)=GALILEO
!                               (4)=SBAS, (5)=COMPASS, (6)=QZSS, (10)=MIXED
! -----------------------------------------------------------
  CHARACTER(LEN=1), DIMENSION(0:10)         :: g_atxsys = &
                    (/' ','G','R','E','S','C','J',' ',' ',' ' ,'M'/)

! Satellite System Types for SVN: (0)=GPS, (2)=GALILEO, (7/8)=GLONASS
! -----------------------------------------------------------
  CHARACTER(LEN=1), DIMENSION(0:10)         :: g_syssvn = &
                    (/'G',' ','E',' ',' ',' ',' ','R','R',' ',' '/)

! RINEX3 Satellite System Types: (0)=GPS, (1)=GLONASS, (2)=GALILEO, (3)=SBAS
!                                (4)=COMPASS, (5)=QZSS
! -----------------------------------------------------------
  CHARACTER(LEN=1), DIMENSION(0:5)         :: g_rnxsys = &
                    (/'G','R','E','S','C','J'/)

! DECLARATION of SATELLITE STRING (see rcvr_ant.tab)
! -------------------------------
      CHARACTER(len=5),PARAMETER    ::antGPS='BLOCK'
      CHARACTER(len=7),PARAMETER    ::antGLO='GLONASS'
      CHARACTER(len=7),PARAMETER    ::antGAL='GALILEO'
      CHARACTER(len=4),PARAMETER    ::antGEO='SBAS'
      CHARACTER(len=7),PARAMETER    ::antCOM='COMPASS'
      CHARACTER(len=4),PARAMETER    ::antQZS='QZSS'

END MODULE m_global

