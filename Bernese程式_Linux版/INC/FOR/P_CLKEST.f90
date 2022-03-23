! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE p_clkest

! -------------------------------------------------------------------------
!
! Purpose:    Module for program clkest
!
! Author:     H. Bock
!
! Created:    08-Jan-2000
! Last mod.:  12-Apr-2011
!
! Changes:    18-Jul-2001  HU: irfsel and nocode added
!             06-Sep-2001  HU: remove added
!             23-Jul-2002  HU: cmbsel added
!             11-Aug-2002  HU: refsel added
!             08-Sep-2003  HU: delrnx added
!             24-Nov-2006  AG: timsys and dcbstr added
!             01-Nov-2007  HB: secIpl added
!             04-May-2009  RD: change opt%epowin to t_timint
!             12-Apr-2011  CR: irelJ2 added
!
! COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!      1999      UNIVERSITY OF BERN
!                    SWITZERLAND
! -----------------------------------------------------------------------

  USE m_bern
  USE m_time, ONLY: t_timint

! STRUCTURE FOR INPUT OPTIONS FOR PG LEOSPP
! -----------------------------------------
  TYPE t_clkopt
    INTEGER(i4b) :: nsampl
    INTEGER(i4b) :: lincom
    INTEGER(i4b) :: irfsel
    INTEGER(i4b) :: irfclk
    INTEGER(i4b) :: minsta
    INTEGER(i4b) :: irelJ2
    INTEGER(i4b) :: iextra
    INTEGER(i4b) :: itropo
    INTEGER(i4b) :: chktrp
    INTEGER(i4b) :: cmbsel      ! combination selection
                                !  1: tridiag
                                !  2: summation cond. on code (ifix=0)
    INTEGER(i4b) :: ifix
    INTEGER(i4b) :: allign
    INTEGER(i4b) :: maxitc
    INTEGER(i4b) :: maxitp
    INTEGER(i4b) :: nocode
    INTEGER(i4b) :: remove
    INTEGER(i4b) :: ncom
    INTEGER(i4b) :: nrfsta
    INTEGER(i4b) :: nrxsta
    INTEGER(i4b) :: clkwrt
    INTEGER(i4b) :: cpyrnx
    INTEGER(i4b) :: delrnx
    INTEGER(i4b) :: clksel
    INTEGER(i4b) :: debug
    REAL(r8b)    :: maxzen
    REAL(r8b)    :: omcmxc
    REAL(r8b)    :: aprsic
    REAL(r8b)    :: resmxc
    REAL(r8b)    :: rmsmxc
    REAL(r8b)    :: omcmax
    REAL(r8b)    :: aprsip
    REAL(r8b)    :: resmxp
    REAL(r8b)    :: rmsmxp
    REAL(r8b)    :: dtfix
    REAL(r8b)    :: dtsim
    REAL(r8b)    :: secIpl
    TYPE(t_timint)         :: epowin
    CHARACTER(LEN=80)      :: titclk
    CHARACTER(LEN=20)      :: runby
    CHARACTER(LEN=3)       :: ac
    CHARACTER(LEN=55)      :: acname
    CHARACTER(LEN= 3)      :: timsys   ! Time system
    CHARACTER(LEN=40)      :: dcbStr   ! Line for DCBs applied
    CHARACTER(LEN=60),DIMENSION(:),POINTER :: coment
    CHARACTER(LEN=4),DIMENSION(:),POINTER  :: refsta
    CHARACTER(LEN=4),DIMENSION(:),POINTER  :: rnxsta
    CHARACTER(LEN=4)                       :: refclk
  END TYPE t_clkopt

END MODULE p_clkest
