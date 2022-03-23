
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_qlfil

! -------------------------------------------------------------------------
! Purpose:    This module defines the structure used for the
!             SLR quick look file (normal point format).
!
! Author:     C. Urschl
!
! Created:    05-Nov-2003
! Last mod.:  27-Jul-2009
!
! Changes:    16-Feb-2004 CU: Nullify pointer
!             28-Jun-2004 RD: Use maxsta from M_MAXDIM now
!             19-Aug-2004 CU: Add revnr (format revision number)
!             27-Jul-2009 DT: Add qlrec%versNr
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern

!  INTEGER(i4b), PARAMETER      :: maxsta = 100 ! Maximum number of stations

  TYPE t_qlrec
    INTEGER(i4b)               :: satnr  ! Satellite number
    INTEGER(i4b)               :: iobstp ! Observation type index
    INTEGER(i4b)               :: numnpt ! # of raw ranges compressed into
                                         ! normal point
    INTEGER(i4b)               :: llinpt ! Loss of lock indicators
    REAL(r8b)                  :: range  ! Range observation
    REAL(r8b),DIMENSION(2)     :: epoch  ! Time of signal reception
                                         ! 1 ... full seconds (MJD)
                                         ! 2 ... fraction of second (sec)
    REAL(r8b)                  :: press  ! Surface pressure
    REAL(r8b)                  :: temp   ! Surface temperature
    REAL(r8b)                  :: humid  ! Relative humidity at surface
    INTEGER(i4b)               :: versNr ! Version number for revision
  END TYPE t_qlrec

  TYPE t_qlobs
    INTEGER(i4b), DIMENSION(2) :: iwlfac ! Wavelength of the laser in (0.1 nm)
    INTEGER(i4b)               :: cdps   ! System number
    INTEGER(i4b)               :: cdpo   ! Occupancy sequence number
    INTEGER(i4b)               :: timsy  ! Epoch time scale indicator
    INTEGER(i4b)               :: syscf  ! System configuration indicator
    INTEGER(i4b)               :: revnr  ! Normal point format revision number
    TYPE(t_qlrec), DIMENSION(:), POINTER :: qlrec
  END TYPE t_qlobs

! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_qlfil(qlobs)
    TYPE(t_qlobs) :: qlobs
    NULLIFY(qlobs%qlrec)
  END SUBROUTINE init_qlfil

END MODULE d_qlfil


