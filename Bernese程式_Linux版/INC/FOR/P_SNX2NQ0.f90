
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE p_snx2nq0

! -------------------------------------------------------------------------
! Purpose:    This module defines structures for the program SNX2NQ0
!
! Author:     D. Thaller
!
! Created:    15-Oct-2010
!
! Changes:    05-Oct-2012 DT: Increase maxdesc 18->20
!             24-Oct-2012 SS: Consider flgCon
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_time,   ONLY: t_timint
  USE d_neq,    ONLY: maxStaSin

  IMPLICIT NONE


! Dimensions
! ----------
  INTEGER(i4b),PARAMETER   :: maxdesc = 20  ! SINEX parameter groups


! Station of one validity interval
! --------------------------------
  TYPE t_staInt
    CHARACTER(LEN=staNam2Length)          :: staNam  ! Station name
    CHARACTER(LEN=2)                      :: ptCode  ! Point Code
    INTEGER(i4b)                          :: solN    ! Solution number
    TYPE(t_timint)                        :: timInt  ! Validity interval
    REAL(r8b), DIMENSION(3)               :: crd     ! Station coordinates
    REAL(r8b), DIMENSION(3)               :: vel     ! Station velocities
    REAL(r8b)                             :: epoch   ! Reference epoch for coord.
    INTEGER(i4b)                          :: flgCon  ! Constraint code
  END TYPE t_staInt

! Station structure for all SINEX intervals
! -----------------------------------------
  TYPE t_snxSta
    INTEGER(i4b)                          :: nStaInt ! Number of entries
    TYPE(t_staInt), DIMENSION(maxStaSin)  :: staInt  ! One station validity interval
  END TYPE t_snxSta

! Option structure
! ----------------


END MODULE p_snx2nq0


