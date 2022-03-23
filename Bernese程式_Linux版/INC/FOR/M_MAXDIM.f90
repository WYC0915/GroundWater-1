
! -------------------------------------------------------------------------
! Bernese GNSS Software Version 5.2
! -------------------------------------------------------------------------

MODULE m_maxdim

! -------------------------------------------------------------------------
! Purpose:    This module defines several maximum dimension parameters
!
! Author:     L. Mervart
!
! Created:    12-Feb-2003
!
! Changes:    07-Mar-2003 HU: MAXINT added, MAXPOT changed from 120 to 140
!             08-Mar-2003 DS: maxamb=600->500 maxsat=48->42 in LARGE
!             17-Apr-2003 MR: maxint=510 --> maxint=1700 (LAGEOS)
!             22-May-2003 RD: maxsat=42->48 in LARGE
!             27-Jun-2003 SS: maxrec from 200 to 300
!             03-Nov-2003 HB: MAXSAS=10 ->15 in SMALL
!             29-Dec-2003 HU: MAXSAA 100 to 200
!             28-Jun-2004 RD: MAXSAS=40 ->42 in LARGE
!             28-Jun-2004 RD: Add parameter MAXCRD
!             07-Feb-2005 HB: LARGE: MAXSAT 48->60, MAXSAS 42->50
!             13-Apr-2005 CU: LARGE: MAXSTA 350->1000, MAXCRD 500->1000
!             03-Apr-2007 HB: MAXSAA 200 to 215
!             26-Apr-2007 SS: maxdsc from 30 to 60
!             19-Nov-2007 HB: MAXSAA 215 to 240
!             28-Feb-2008 RD: MAXSAA 240 to 340
!             23-Jul-2008 DT: MAXINT 1700->22000
!             02-Oct-2008 SS: maxrec from 300 to 500
!             28-Oct-2008 DT: MAXVAR 100->15 (reason for 100 ???)
!                             (remove maxvar from P_GPSEST, P_ADDNEQ and SR)
!             29-Jun-2009 RD: maxatm added (=2: S1/S2 atm tidal loading)
!             20-Aug-2009 LO: maxsta from 1000 to 3000
!             21-Sep-2009 RD: MAXSHD added from P_GPSEST
!             11-Mar-2010 SL: ONLY added to USE m_bern
!             26-Aug-2010 DT: MAXINT 22000->5100 (due to GPSEST)
!             16-Nov-2010 HB: MAXVAR 15=>21, MAXAMB 500=>1000
!             16-Apr-2011 HB: MAXSAA 340=>350
!             11-Jul-2011 HB: MAXSAA 350=>400
!             09-Feb-2012 LP: LARGE: MAXSAT 60->70, MAXSAS 60->70
!             12-May-2012 RD: Remove DIMENSIONS except of LARGE
!             06-Jun-2012 HB: MAXSAA 400=>450
!             14-Jun-2012 RD: PCF-specific parameters from M_MAXDIM to P_BPE
!             27-Jun-2012 LP: LARGE: MAXSAT 70->85
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! MAXAMB: Maximum number of ambiguities
! MAXFLS: Maximum number of files in a session
! MAXGIM: Maximum number of global/local ionosphere models
! MAXGIT: Maximum number of terms per global/local ionosphere models
! MAXPOT: Maximum number of geo-potential terms
! MAXREC: Maximum number of receivers that are processed
! MAXSAA: Maximum number of satellites in satellite information file
! MAXSAC: Maximum number of satellite clock parameters (polynomial degree + 1)
! MAXSAS: Maximum number of satellites at one epoch
! MAXSAT: Maximum number of satellites that are processed
! MAXSTA: Maximum number of stations allowed for processing and in neqs
! MAXCRD: Maximum number of stations allowed in coordinate file
! MAXINT: Maximum number of integration intervals
! MAXSHD: Maximum number of shadow transits of satellites

  USE m_bern,   ONLY: i4b

  IMPLICIT NONE

  INTEGER(i4b),PARAMETER  :: maxvar=21
  INTEGER(i4b),PARAMETER  :: maxbad=200
  INTEGER(i4b),PARAMETER  :: maxgit=300
  INTEGER(i4b),PARAMETER  :: maxocn=11
  INTEGER(i4b),PARAMETER  :: maxatm=2
  INTEGER(i4b),PARAMETER  :: maxrec=500
  INTEGER(i4b),PARAMETER  :: maxsaa=450
  INTEGER(i4b),PARAMETER  :: maxsac=5
  INTEGER(i4b),PARAMETER  :: maxint=5100
  INTEGER(i4b),PARAMETER  :: maxshd=100

  INTEGER(i4b),PARAMETER  :: maxamb=1000
  INTEGER(i4b),PARAMETER  :: maxfls=90
  INTEGER(i4b),PARAMETER  :: maxgim=200
  INTEGER(i4b),PARAMETER  :: maxpot=140
  INTEGER(i4b),PARAMETER  :: maxsas=70
  INTEGER(i4b),PARAMETER  :: maxsat=85
  INTEGER(i4b),PARAMETER  :: maxsta=3000
  INTEGER(i4b),PARAMETER  :: maxcrd=1000
  INTEGER(i4b),PARAMETER  :: maxstc=60

END MODULE m_maxdim
