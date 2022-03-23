! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE p_leokin

! -------------------------------------------------------------------------
! Purpose    :  Module for PROGRAM LEOKIN
!
! Author     :  H.Bock
!
! Created    :  08-jan-2000
! Last mod.  :  02-Sep-2005
!
! Changes    :  15-Feb-2002: HB: Add phaOnly and dewgt to leoopt
!               25-Feb-2002: HB: Add kinpos and csess to leoopt
!               02-Jul-2002: HB: Add numobs to leoopt
!               08-Jul-2002: HB: Add beta to leoOpt
!               23-Jul-2002: HB: Remove d_gpsobs, INCLUDE 'MAXSAT.inc'
!               15-Jan-2003: HB: Add iOff to leoopt
!               17-Feb-2003: LM: Use m_maxdim
!               30-Jun-2003: HB: MaxEpo = 12000
!               17-Sep-2003: HB: Add ircPcv to leoopt
!               07-Apr-2004: HB: Merge versions from LEOKIN and
!                                official CVS
!               05-Aug-2005: HB: INcrease dimensions of arrays in
!                                t_leodif to MAXSAT (-C compilation)
!               02-Sep-2005: HB: Add Qxy to phase type
!
! Copyright  :  Astronomical Institute
!               University of Bern
!               Switzerland
! -----------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_maxdim, ONLY: maxsat

! Parameter
! ---------
  INTEGER(i4b),PARAMETER  :: maxepo=20000

! maxEpo : Maximum number of epochs
! maxSat : Maximum number of satellites

! Structure for input options for PG LEOKIN
! -----------------------------------------
  TYPE t_leoopt
    CHARACTER(LEN = 4),DIMENSION(2) :: csess
    INTEGER(i4b) :: lcspp
    INTEGER(i4b) :: ndeg
    INTEGER(i4b) :: numleo
    INTEGER(i4b) :: iant
    INTEGER(i4b) :: icomb
    INTEGER(i4b) :: niter
    INTEGER(i4b) :: ircPcv
    INTEGER(i4b) :: minsat_cod !! gb
    INTEGER(i4b) :: minsat_pha !! gb
    INTEGER(i4b) :: orbtyp     !! gb
    INTEGER(i4b) :: niter_cod  !! gb
    INTEGER(i4b) :: niter_pha  !! gb
    INTEGER(i4b) :: minpha_arc !! gb
    INTEGER(i4b) :: book_bernese !! gb
    INTEGER(i4b) :: use_clk_sp3 !! gb
    INTEGER(i4b) :: elev_wgt
    INTEGER(i4b) :: correl
    INTEGER(i4b) :: statyp
    INTEGER(i4b) :: phaOnly
    INTEGER(i4b) :: iOff
    INTEGER(i4b) :: kinpos
    INTEGER(i4b) :: ambLgt
    INTEGER(i4b),DIMENSION(2) :: numObs
    INTEGER(i4b),DIMENSION(2) :: beta
    REAL(r8b)    :: dfml12
    REAL(r8b)    :: rmsmax_cod !! gb
    REAL(r8b)    :: rmsmax_pha !! gb
    REAL(r8b)    :: rms_phaobs !! gb
    REAL(r8b)    :: rms_codobs !! gb
    REAL(r8b)    :: orbacc
    REAL(r8b)    :: zenmax
    REAL(r8b)    :: spacng
    REAL(r8b)    :: gapmax
    REAL(r8b)    :: pdfmax     !! gb
    REAL(r8b)    :: alpha
    REAL(r8b)    :: dewgt
    REAL(r8b),DIMENSION(2,1)       :: window
    INTEGER(i4b),DIMENSION(9)      :: nused
    CHARACTER(LEN=30)   ::  leo_sensor_name
  END TYPE t_leoopt

! Structure for variables concerning the observations of the LEO
! --------------------------------------------------------------
  TYPE t_leoobs
    CHARACTER(LEN=1),DIMENSION(maxsat,2,1) :: obsfle
    CHARACTER(LEN=1),DIMENSION(maxsat,2,1) :: obsflp
    CHARACTER(LEN=1)                     :: epoflg
    REAL(r8b)                            :: obtime
    REAL(r8b),DIMENSION(maxsat,4,1)      :: obsepo
    REAL(r8b),DIMENSION(maxsat,4,1)      :: obsepp
    REAL(r8b),DIMENSION(2)               :: deltat
    REAL(r8b),DIMENSION(2)               :: deltap
    INTEGER(i4b),DIMENSION(maxsat,1)     :: svnepo
    INTEGER(i4b),DIMENSION(maxsat,1)     :: svnepp
    INTEGER(i4b),DIMENSION(1,1)          :: nsatep
    INTEGER(i4b),DIMENSION(1,1)          :: nsatpp
  END TYPE t_leoobs

! Structure for position results
! ------------------------------
  TYPE t_leopos
    CHARACTER(LEN=1),DIMENSION(maxSat)   :: mrk
    REAL(r8b)                      :: obtime
    REAL(r8b),DIMENSION(3)         :: xstell
    REAL(r8b),DIMENSION(4)         :: posit     ! gb
    REAL(r8b),DIMENSION(4)         :: posit_apr ! gb
    REAL(r8b),DIMENSION(maxsat)    :: wgt_codobs
    REAL(r8b),DIMENSION(maxsat)    :: resid
    REAL(r8b),DIMENSION(maxsat)    :: azi
    REAL(r8b),DIMENSION(maxsat)    :: zen
    REAL(r8b),DIMENSION(10)        :: angl
    REAL(r8b)                      :: rmsspp
    REAL(r8b)                      :: xpol
    REAL(r8b)                      :: ypol
    REAL(r8b)                      :: sz
    INTEGER(i4b),DIMENSION(maxsat) :: satusd
    INTEGER(i4b)                   :: nsused
    INTEGER(i4b)                   :: iepo
    INTEGER(i4b)                   :: irtspp
    INTEGER(i4b)                   :: iter
  END TYPE t_leopos

! Structure for position difference results
! -----------------------------------------
  TYPE t_leodif
    CHARACTER(LEN=1),DIMENSION(maxSat)   :: mrk
    REAL(r8b),DIMENSION(4)         :: posdif
    REAL(r8b),DIMENSION(maxsat)    :: resdif
    REAL(r8b),DIMENSION(maxsat)    :: wgt_phaobs
    REAL(r8b),DIMENSION(10)        :: angl
    REAL(r8b),DIMENSION(10)        :: Qxy
    REAL(r8b)                      :: rmsdif
    REAL(r8b),DIMENSION(maxsat)         :: bobs
    REAL(r8b),DIMENSION(maxsat)         :: bobs_part
    REAL(r8b),DIMENSION(9,maxsat)       :: gps1
    REAL(r8b),DIMENSION(9,maxsat)       :: gps2
    REAL(r8b),DIMENSION(maxsat)         :: zen
    REAL(r8b),DIMENSION(maxsat)         :: clk1
    REAL(r8b),DIMENSION(maxsat)         :: clk2
    REAL(r8b),DIMENSION(3)         :: codpo1
    REAL(r8b),DIMENSION(4,maxsat)  :: aobs
    INTEGER(i4b),DIMENSION(maxsat) :: svndif
    INTEGER(i4b)                   :: nsat
    INTEGER(i4b)                   :: nsEff
    INTEGER(i4b)                   :: irtdif
  END TYPE t_leodif

! Structure for band-diagonal normal equation system
! --------------------------------------------------
  TYPE t_bandneq
    REAL(r8b),DIMENSION(:,:),POINTER  :: atpa
    REAL(r8b),DIMENSION(:,:),POINTER  :: al
    REAL(r8b),DIMENSION(:)  ,POINTER  :: rhs
    REAL(r8b),DIMENSION(:)  ,POINTER  :: sol
    REAL(r8b)                         :: d
    INTEGER(i4b),DIMENSION(:),POINTER :: indx
    INTEGER(i4b)                      :: m1
  END TYPE t_bandneq

 END MODULE p_leokin
