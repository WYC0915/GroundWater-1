
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_frqfil

! -------------------------------------------------------------------------
! Purpose:    This module defines the structure used for the
!             frequency information file.
!
! Author:     C. Urschl
!
! Created:    30-Oct-2003
! Last mod.:  21-Nov-2005
!
! Changes:    21-Nov-2005 CU: Change dimension of header lines: 6 -> 5
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  TYPE t_freq
    CHARACTER(LEN=5)                                 :: typstrg
    CHARACTER(LEN=staNam2Length)                     :: satsta
    CHARACTER(LEN=2)                                 :: name
    REAL(r8b)                                        :: value
    CHARACTER(LEN=4)                                 :: unit
    REAL(r8b), DIMENSION(2)                          :: window
    CHARACTER(LEN=23)                                :: remark
  END TYPE t_freq

  TYPE t_frqinfo
    TYPE(t_freq)             , DIMENSION(:), POINTER :: freq
    CHARACTER(LEN=lineLength), DIMENSION(5)          :: headline
    CHARACTER(LEN=lineLength), DIMENSION(:), POINTER :: footline
    CHARACTER(LEN=fileNameLength)                    :: frqfile
  END TYPE t_frqinfo

! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_frqfil(frqinfo)
    TYPE(t_frqinfo) :: frqinfo
    NULLIFY(frqinfo%freq)
    NULLIFY(frqinfo%footline)
  END SUBROUTINE init_frqfil

END MODULE d_frqfil


