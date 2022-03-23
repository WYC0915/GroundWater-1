MODULE s_MATCOP
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE matcop(iflag,ndim,mat1,mat2)

! -------------------------------------------------------------------------
! Purpose:    Allocate matrix "mat2" (with dimension "ndim") and copy
!             "mat1" to "mat2"
!
! Author:     M. Rothacher, S. Schaer
!
! Created:    13-Sep-2002
! Last mod.:  __-___-____
!
! Changes:    __-___-____ __:
!
! SR used:    alcerr, exitrc
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  USE s_alcerr
  USE s_exitrc
  IMPLICIT NONE

! List of parameters
! ------------------
! Input:
  INTEGER(i4b)                    :: iflag  ! Flag
                                            ! =-1: Deallocate "mat2"
                                            ! = 0: Just copy "mat1" to
                                            !      "mat2"
                                            ! = 1: Allocate "mat2" and
                                            !      copy "mat1" to "mat2"
  INTEGER(i4b)                    :: ndim   ! Number of matrix elements
  REAL(r8b),DIMENSION(:)          :: mat1   ! Original matrix

! Output:
  REAL(r8b),DIMENSION(:),POINTER  :: mat2   ! Copy of "mat1"

! Local variables
! ---------------
  INTEGER(i4b)                    :: iac

  IF (iflag == -1) THEN

    DEALLOCATE( mat2, stat=iac )

  ELSEIF (iflag ==  0) THEN

    mat2(1:ndim) = mat1(1:ndim)

  ELSEIF (iflag ==  1) THEN

    ALLOCATE( mat2(ndim), stat=iac )
    CALL alcerr(iac, 'mat2', (/ndim/), 'matcop')

    mat2(1:ndim) = mat1(1:ndim)

  ELSE

    WRITE(lfnerr,'(/,A,/)') ' *** SR MATCOP: Invalid flag'
    CALL exitrc(2)

  ENDIF

END SUBROUTINE matcop


END MODULE
