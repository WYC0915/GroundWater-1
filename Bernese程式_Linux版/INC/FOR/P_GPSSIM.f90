! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE p_gpssim

! -------------------------------------------------------------------------
! Purpose:    This module defines structures for program GPSSIM
!
! Author:     H. Bock
!
! Created:    01-Nov-2007
! Last mod.:  03-Feb-2011
!
! Changes:    23-Feb-2002 HU: Read mxosvn
!             27-Jun-2002 DS: Add the LEO input options
!             13-Sep-2002 MR: Add parameter "iextra"
!             27-Nov-2002 HB: Parameters for meteo-file simulation removed
!             18-May-2003 HU: Nullify pointers
!             08-Sep-2003 HU: recnam, antnam, oprnam chr16 -> chr20
!             24-Nov-2003 HU: Iono on/off flag
!             01-Nov-2007 HB: secIpl added
!             04-May-2009 RD: change opt%tfirst/tlast to t_timint
!             19-Jul-2010 SL: tab characters removed
!             03-Feb-2011 CR: Add periodic relativistic J2-correction
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_time, ONLY: t_timint
!
! Options
! -------
  TYPE t_gpssim_opt
    CHARACTER(LEN = 16)             :: campgn
    CHARACTER(LEN = 53)             :: title
    CHARACTER(LEN = staNameLength)  :: ionref
    CHARACTER(LEN = 4),DIMENSION(2) :: csess
    CHARACTER(LEN = 16),            DIMENSION(:),POINTER   :: fstnam
    CHARACTER(LEN = staNameLength), DIMENSION(:),POINTER   :: stanam
    CHARACTER(LEN = 20),            DIMENSION(:),POINTER   :: oprnam
    CHARACTER(LEN = fileNameLength),DIMENSION(:,:),POINTER :: filcod
    CHARACTER(LEN = fileNameLength),DIMENSION(:,:),POINTER :: filpha
    CHARACTER(LEN = 20),            DIMENSION(:),POINTER   :: rectyp
    CHARACTER(LEN = 20),            DIMENSION(:),POINTER   :: anttyp
    REAL(r8b),DIMENSION(2,2) :: rms
    REAL(r8b),DIMENSION(2,2) :: leorms         ! RMS of LEO observations
    TYPE(t_timint)::obswin
    REAL(r8b)    :: secIpl
    INTEGER(i4b),DIMENSION(:),POINTER :: recuni
    INTEGER(i4b),DIMENSION(:),POINTER :: antuni
    INTEGER(i4b),DIMENSION(4) :: iono
    INTEGER(i4b) :: nsta
    INTEGER(i4b) :: codsel
    INTEGER(i4b) :: phasel
    INTEGER(i4b) :: inter
    INTEGER(i4b) :: ielev
    INTEGER(i4b) :: iextra
    INTEGER(i4b) :: itropo
    INTEGER(i4b) :: ix
    INTEGER(i4b) :: icoelv
    INTEGER(i4b) :: leoelv                    ! Model for elev. dep. weight
                                              ! for leos
                                              ! 0: no weigthing
                                              ! >0: model number
    INTEGER(i4b) :: nslips
    INTEGER(i4b) :: isize
    INTEGER(i4b) :: jl12eq
    INTEGER(i4b) :: mxosvn
    INTEGER(i4b) :: irel2                     ! Flag for periodic
                                              ! relativistic J2 correction
  END TYPE t_gpssim_opt

! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_gpssim_opt(gpssim_opt)
    TYPE(t_gpssim_opt) :: gpssim_opt

    NULLIFY(gpssim_opt%fstnam)
    NULLIFY(gpssim_opt%stanam)
    NULLIFY(gpssim_opt%oprnam)
    NULLIFY(gpssim_opt%filcod)
    NULLIFY(gpssim_opt%filpha)
    NULLIFY(gpssim_opt%rectyp)
    NULLIFY(gpssim_opt%anttyp)
    NULLIFY(gpssim_opt%recuni)
    NULLIFY(gpssim_opt%antuni)
  END SUBROUTINE init_gpssim_opt

END MODULE p_gpssim
