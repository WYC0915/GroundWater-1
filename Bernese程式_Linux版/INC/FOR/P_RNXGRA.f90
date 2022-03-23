
! -------------------------------------------------------------------------
! Bernese GNSS Software Version 5.2
! -------------------------------------------------------------------------

MODULE p_rnxgra

! -------------------------------------------------------------------------
! Purpose:    This module defines structures for program RNXGRA
!
! Author:     R. Dach
!
! Created:    20-Nov-2000
!
! Changes:    22-Oct-2001 RD: Add flag for using station info file
!             14-Aug-2002 RD: Tolerance for missing epochs
!             09-Jul-2003 RD: opt%flag max be removed
!             16-Feb-2004 RD: New options: print cycle slip, min s/n-ratio
!             21-Apr-2004 HU: MAXSTA 200 -> 250
!             16-Aug-2006 HU: MAXTYP 10 -> 12
!             27-May-2007 AG: MAXSAT 124 -> 350
!             11-Oct-2008 SL: MAXFIL 250 -> 300
!             19-Jul-2010 SL: tab characters removed
!             03-Nov-2010 DT: MAXTYP 12->18
!             18-Jan-2011 SL: maxtyp from d_rinex3
!             09-Feb-2012 LP: MAXCOM 10 -> 60
!             09-May-2012 LP: MAXCOM 60 ->150
!             07-MAR-2013 SS: MAXCOM from 150 to 300 (due to BISK/POUS)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_rinex3,  ONLY: maxtyp
!
  INTEGER(i4b), PARAMETER :: MAXFIL=300     ! MAXIMUM NUMBER OF INPUT FILES
  INTEGER(i4b), PARAMETER :: MAXCHR= 72     ! MAXIMUM NUMBER OF CHARACTERS
  REAL(r8b),    PARAMETER :: TIMCHR=1200D0  ! NUMBER OF SECONDS PER 1 CHARACTER
  INTEGER(i4b), PARAMETER :: MAXCOM=300     ! MAXIMUM NUMBER OF COMMENT LINES
  INTEGER(i4b), PARAMETER :: MAXSAT=350     ! MAXIMUM NUMBER OF SATELLITES
  INTEGER(i4b), PARAMETER :: MAXREC=1000000 ! MAXIMUM NUMBER OF RECORDS IN ONE FILE
!
! Typ efor RNXGRA input options
! -----------------------------
TYPE t_rnxgra_opt
  INTEGER(i4b)                  :: iopt   !   1 .. L1 is checked only
                                          !   2 .. L2 is checked only
                                          !   3 .. L1 and L2 are checked
                                          !  -1 .. P1 (C1) is checked only
                                          !  -2 .. P2 (C2) is checked only
                                          !  -3 .. P1 (C1) and P2 (C2) are
                                          !        checked
  INTEGER(i4b)                  :: iobtol ! Tolerance between obs. in RINEX file
                                          ! and max. obs. per character
  INTEGER(i4b)                  :: istops ! stop if station not found in station
                                          ! name translation table
  INTEGER(i4b)                  :: minsig ! Minimum s/n-ratio to consider an obs.
  INTEGER(i4b)                  :: cycgra ! Add cycle slip flags to graphic
  INTEGER(i4b)                  :: getlst ! 1: get list of files
  CHARACTER(len=fileNameLength) :: lstFil ! name of the list of good files
  CHARACTER(len=fileNameLength) :: delFil ! name of the list of bad files
  INTEGER(i4b)                  :: minObs ! min # obs per station requ. for list
                                          ! of files
  INTEGER(i4b)                  :: maxSta ! max # of stations in the list of
                                          ! files
  INTEGER(i4b)                  :: maxBad ! Max. # of bad epochs per station
                                          ! allowed
  INTEGER(i4b)                  :: badObs ! Max. # of obs to define a bad epoch
END TYPE t_rnxgra_opt

END MODULE p_rnxgra

