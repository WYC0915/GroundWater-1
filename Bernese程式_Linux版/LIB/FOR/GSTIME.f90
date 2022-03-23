MODULE f_gstime
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION gstime(isys,ut1,tt,nut21,eqequi)

! -------------------------------------------------------------------------
! Purpose:    Computation of Greenwich Sidereal Time using either
!             THETAN (isys=1) or GMST2000 (isys=2).
!
! Remarks:    Argument tt is needed for GMST2000.
!             To get Greenwich Mean Sidereal Time: set eqequi=0
!             To get Greenwich Sidereal Time for isys=1: set eqequi=nut(2,1)
!
!             For isys=0 the name of the nutation model is checked using
!             sr rdnutsub, i.e., relying on the existence of the keywords
!             NUTMOD and SUBMOD in the input panel.
!             If the nutation name string contains IAU80, thetan is used,
!             if the name contains IAU2000, gmst2000 is used. If none of
!             the two strings appear, a warning is issued and gmst2000 is
!             used.
!
!             THETAN  : Consistent with IERS Conventions 1996
!             GMST2000: Consistent with IERS Conventions 2000
!
! Author:     U. Hugentobler
!
! Created:    11-Jun-2003
! Last mod.:  11-May-2011
!
! Changes:    06-Aug-2003 HU: Arguments nut21 and eqequi separated
!                             Check nutation model to get old or new GST
!             01-Sep-2003 HU: Use interface for rdnutsub
!             04-Jul-2005 HU: ut1 and tt as t_epoch-type
!             11-May-2011 HB: Update nutmod-names
!
! SR used:    exitrc
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_epoch,  ONLY: t_epoch, OPERATOR(.epochToReal.)

  USE f_thetan
  USE f_gmst2000
  USE s_rdnutsub
  USE s_gtflna
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  INTEGER(i4b)              :: isys           ! 0: CHECK NUTNAM
                                              ! 1: THETAN, 2: GMST2000
  TYPE(t_epoch)             :: ut1            ! MJD Epoch in UT1
  TYPE(t_epoch)             :: tt             ! MJD Epoch in TT
  REAL(r8b)                 :: nut21          ! NUT(2,1)
  REAL(r8b)                 :: eqequi         ! Equation of equinox

! output:
  REAL(r8b)                 :: gstime         ! Greenwich sidereal time

! Local parameters
! ----------------
  REAL(r8b)                 :: xut1
  INTEGER(i4b),SAVE         :: iuse
  INTEGER(i4b),SAVE         :: ifirst=1
  INTEGER(i4b)              :: ircnut
  INTEGER(i4b)              :: ircsub
  CHARACTER(LEN=16)         :: nutnam
  CHARACTER(LEN=16)         :: subnam
  CHARACTER(LEN=fileNameLength) :: filnam

  xut1 = .epochToReal.ut1

  IF (isys == 1) THEN
    gstime = thetan(xut1)+nut21
  ELSEIF (isys == 2) THEN
    gstime = gmst2000(ut1,tt)+eqequi
  ELSE

! Automatic detection of convention to be used for computing sidereal time
! ------------------------------------------------------------------------
    IF (ifirst == 1) THEN
      ifirst=0
      CALL gtflna(0,'NUTMOD',filnam,ircnut)
      CALL gtflna(0,'SUBMOD',filnam,ircsub)
      IF (ircnut==1 .OR. ircsub==1) THEN
        WRITE(lfnerr,"(/,' ### SR GSTIME: Nutation model not found', &
                     & /,'                Conventions 2000 used',/)")
        iuse=2
      ELSE
        CALL rdnutsub(nutnam,subnam)
        iuse=0
        IF (INDEX(nutnam,"IAU80")   > 0) iuse=1
        IF (INDEX(nutnam,"IAU2000") > 0) iuse=2
        IF (INDEX(nutnam,"IAU2000R06") > 0) iuse=2
        IF (INDEX(nutnam,"IAU2006") > 0) iuse=2
        IF (iuse == 0) THEN
          WRITE(lfnerr,"(/,' ### SR GSTIME: Nutation model not identified', &
                       & /,'                Conventions 2000 used', &
                       & /,'                Name: ',A,/)") nutnam
          iuse=2
        ENDIF
      ENDIF
    ENDIF

    IF (iuse == 1) THEN
      gstime = thetan(xut1)+nut21
    ELSEIF (iuse == 2) THEN
      gstime = gmst2000(ut1,tt)+eqequi
    ENDIF

  ENDIF

  RETURN

END FUNCTION gstime

END MODULE
