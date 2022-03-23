
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_comjpl

! -------------------------------------------------------------------------
! Purpose:    Common JPLEP (JPL Ephemeris)
!
!             ksize  = size of ephemeris buffer (s.p. words)
!             irecsz = length of the records in the direct-access file.
!
!             If the 'recl' specification in the fortran compiler
!             on your machine expects the length to be in words, then
!                       irecsz=ksize,
!             If the length is expected in bytes the record length "irecsz"
!             should be
!                       irecsz=ksize*4
!
! Author:     L. Mervart
!
! Created:    12-Feb-2003
!
! Changes:    27-Feb-2003 HU: Renamed from M_COMJPL
!             13-Dec-2004 HB: Add DE405-ephemerides compiler option
!             31-Jan-2005 HU: Parameter JPLNAM added
!             29-Oct-2012 RD: Remove #ifdef OS_VMS
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  INTEGER*4   KSIZE,IRECSZ
  CHARACTER*5 JPLNAM

#ifdef EPH_DE405

  PARAMETER (KSIZE=2036,IRECSZ=4*2036)
  PARAMETER (JPLNAM='DE405')

#else

  PARAMETER (KSIZE=1652,IRECSZ=4*1652)
  PARAMETER (JPLNAM='DE200')

# endif

END MODULE d_comjpl
