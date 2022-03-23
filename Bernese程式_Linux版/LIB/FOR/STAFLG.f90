MODULE s_STAFLG
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE staflg(staname,epo,flag,marktype,staInfFil)

! -------------------------------------------------------------------------
! Purpose:    Searches the TYPE 005 of STACRUX-structure for
!             station name (stanam) and returns the flag, returns 0
!             if staname not found
!
! Author:     D. Svehla
!
! Created:    09-Feb-2001
! Last mod.:  27-Oct-2010
!
! Changes:    13-Jun-2001: HB: USE m_bern
!             15-Jun-2001: HB: rename d_stacrux in d_stacrx
!             09-Aug-2001: MR: allow no stacrux file
!             16-Dec-2001: HU: Use implicit none
!             20-Sep-2002: TV: Correct time window for station type
!             17-May-2003: HU: Initialize structure
!             23-Jun-2003: HB: New optional parameter for filename of
!                              station info file, Layout for 5.0
!             16-Sep-2003  RD: STACRUX->STAINFO
!             04-Nov-2003  HB: Declare staname with (LEN=*)
!                              (PORTLAND LINUX compiler)
!             07-Oct-2010 SL: return flag = 1 if staname found
!             27-Oct-2010 SL: use m_bern with ONLY
!
! SR used:    gtflna, readcrux
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, fileNameLength
  USE d_stacrx, ONLY: t_stacrux, init_stacrux
  USE s_readcrux
  USE s_gtflna
  IMPLICIT NONE

! List of parameters
! ------------------
! IN:
  CHARACTER(LEN=*)                :: staname    ! Station name
  REAL(r8b)                       :: epo        ! Time in MJD

! OUT:
  INTEGER(i4b)                    :: flag       ! Flag: =0 if staname not found
  CHARACTER(LEN=20)               :: marktype   ! Marker type

! OPTIONAL:
  CHARACTER(LEN=fileNameLength),OPTIONAL :: staInfFil  ! Name of Station Info File

! List of Functions
! -----------------

! Local Types
! -----------
  TYPE(t_stacrux), SAVE           :: stacrux

! Local Parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength)   :: filename
  INTEGER(i4b)                    :: icrx
  INTEGER(i4b), SAVE              :: irccrx

  LOGICAL,      SAVE              :: first= .TRUE.

! If called for the first time, read the entire STACRUX file
! ----------------------------------------------------------
  IF (first) THEN
    first = .FALSE.

! Get the STACRUX file name
! -------------------------
    IF (PRESENT(staInfFil)) THEN
      filename = staInfFil
      ircCrx = 0
    ELSE
      CALL gtflna(0,'STAINFO',filename,irccrx)
    ENDIF

! Read STACRUX file
! -----------------
    IF (irccrx == 0) THEN
      CALL init_stacrux(stacrux)
      CALL readcrux(filename,stacrux)
    ENDIF
  ENDIF

! Search for the station name in the STACRUX-structure
! ----------------------------------------------------
  flag=0
  marktype=' '
  IF (irccrx == 0) THEN
    DO icrx = 1, stacrux%nstatype
      IF (stacrux%statype(icrx)%stanam==staname) THEN
        IF (epo.GE.stacrux%statype(icrx)%timint%t(1).AND.    &
        &   epo.LE.stacrux%statype(icrx)%timint%t(2)) THEN
!!!          flag=stacrux%statype(icrx)%flg
          flag=1
          marktype=stacrux%statype(icrx)%markertype
          EXIT
        ENDIF
      ENDIF
    ENDDO
  ENDIF

  RETURN

END SUBROUTINE staflg

END MODULE
