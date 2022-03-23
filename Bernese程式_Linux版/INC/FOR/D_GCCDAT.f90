
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_gccdat

! -------------------------------------------------------------------------
! Purpose:    This module defines structures and global variables for
!             geocenter coordinate file.
!
! Author:     U. Hugentobler
!
! Created:    25-Mar-2002
! Last mod.:  13-May-2003
!
! Changes:    13-May-2003 CU: Nullify pointers
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern

! Type for gcc file
! -----------------
  TYPE t_gccdat
    INTEGER(i4b)                        :: nrec   ! Total number of records
    INTEGER(i4b)                        :: ifrmt  ! Format number
                                                  !  =1: piece-wise constant
                                                  !  =2: piece-wise linear
    TYPE(t_gccrec),DIMENSION(:),POINTER :: gccrec ! List of records
  END TYPE t_gccdat

! Type for gcc record
! -------------------
  TYPE t_gccrec
    REAL(r8b),DIMENSION(2)       :: timint ! Time interval (MJD)
    REAL(r8b),DIMENSION(3)       :: gcc    ! GCC values (X,Y,Z)
    REAL(r8b),DIMENSION(3)       :: siggcc ! Sigma value (Y,X,Z)
  END TYPE t_gccrec

! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_gccdat(gccdat)
    TYPE(t_gccdat)   :: gccdat

    NULLIFY(gccdat%gccrec)
    gccdat%nrec = 0
  END SUBROUTINE init_gccdat

END MODULE d_gccdat
