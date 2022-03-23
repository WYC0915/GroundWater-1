MODULE s_LPCOEF
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

      SUBROUTINE LPCOEF(NTERM,COEFLP)

!!-------------------------------------------------------------------------
!! NAME       :  LPCOEF
!!
!! PURPOSE    :  COMPUTATION OF COEFFICIENTS OF LEGENDRE POLYNOMIALS
!!
!! PARAMETERS :
!!         IN :  NTERM  : MAXIMUM ORDER OF POTENTIAL          I*4
!!        OUT :  COEFLP(N-1) : COEFFICIENTS (2*N)!/(N!*2**N)  R*8
!!                                         N=2,3,...,NTERM
!! SR CALLED  :  ---
!!
!! REMARKS    :  REPLACES OLD SR LPCOEF BECAUSE OF NUMERICAL PROBLEMS WITH
!!               COMPUTING FACTORIAL OF (2*N),N>30
!!
!! AUTHOR     :  G.BEUTLER, M.ROTHACHER
!!
!! VERSION    :  3.4  (JAN 93)
!!
!! CREATED    :  87/11/30 08:11        LAST MODIFIED :
!!
!! CHANGES    :  30-NOV-99 : HB: CHANGE OF ALGORITHM BECAUSE OF NUMERICAL
!!                               PROBLEMS IF N>30 AND WRITE SR IN FORTRAN90
!!                               COEFLP(I)=1*3*5*....*(2*I-1)
!!
!! COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!!      1987      UNIVERSITY OF BERN
!!                    SWITZERLAND
!!-------------------------------------------------------------------------

  USE M_BERN
  IMPLICIT NONE

! DUMMY ARGUMENTS
! ---------------
  INTEGER(i4b) :: NTERM
  REAL(r8b),DIMENSION(*) :: COEFLP

! VARIABLES
! ---------
  INTEGER(i4b) :: I

! COMPUTATION OF COEFLP(I)
! ------------------------
  IF(NTERM<=1) GO TO 999
  COEFLP(1)=3.D0
  DO I=1,NTERM-2
    COEFLP(I+1)=COEFLP(I)*(2*(I+1)+1)
  END DO

999 CONTINUE
  RETURN
  END SUBROUTINE LPCOEF

END MODULE
