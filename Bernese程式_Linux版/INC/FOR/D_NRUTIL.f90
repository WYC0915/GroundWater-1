! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_nrutil

! -------------------------------------------------------------------------
! Purpose:   Module for banddiagonal approach, combination
!            (ALGORITHM: MODIFIED MODULE NRUTIL.f90 FROM
!            NUMERICAL RECIPES, W.H. Press et al.,
!            1992)
!
! Author:    H.Bock
!
! Created:   23-Feb-2001
! Last mod.: __-___-____
!
! Changes:   __-___-____
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
! -----------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_exitrc
  IMPLICIT NONE

! Parameters
! ----------
  INTEGER(i4b), PARAMETER :: NPAR_ARTH=16,NPAR2_ARTH=8
  INTEGER(i4b), PARAMETER :: NPAR_GEOP=4,NPAR2_GEOP=2
  INTEGER(i4b), PARAMETER :: NPAR_CUMSUM=16
  INTEGER(i4b), PARAMETER :: NPAR_CUMPROD=8
  INTEGER(i4b), PARAMETER :: NPAR_POLY=8
  INTEGER(i4b), PARAMETER :: NPAR_POLYTERM=8

! Interfaces
! ----------
  INTERFACE swap
    MODULE PROCEDURE swap_i,swap_r,swap_rv
  END INTERFACE
  INTERFACE imaxloc
    MODULE PROCEDURE imaxloc_r,imaxloc_i
  END INTERFACE
  INTERFACE arth
    MODULE PROCEDURE  arth_d, arth_i
  END INTERFACE
  INTERFACE assert_eq
    MODULE PROCEDURE assert_eq2,assert_eq3,assert_eq4,assert_eqn
  END INTERFACE
CONTAINS
!BL
  SUBROUTINE swap_i(a,b)
    INTEGER(i4b), INTENT(INOUT) :: a,b
    INTEGER(i4b) :: dum
    dum=a
    a=b
    b=dum
  END SUBROUTINE swap_i
!BL
  SUBROUTINE swap_r(a,b)
    REAL(r8b), INTENT(INOUT) :: a,b
    REAL(r8b) :: dum
    dum=a
    a=b
    b=dum
  END SUBROUTINE swap_r
!BL
  SUBROUTINE swap_rv(a,b)
    REAL(r8b), DIMENSION(:), INTENT(INOUT) :: a,b
    REAL(r8b), DIMENSION(SIZE(a)) :: dum
    dum=a
    a=b
    b=dum
  END SUBROUTINE swap_rv
!BL
  FUNCTION imaxloc_r(arr)
    REAL(r8b), DIMENSION(:), INTENT(IN) :: arr
    INTEGER(i4b) :: imaxloc_r
    INTEGER(i4b), DIMENSION(1) :: imax
    imax=maxloc(arr(:))
    imaxloc_r=imax(1)
  END FUNCTION imaxloc_r
!BL
  FUNCTION imaxloc_i(iarr)
    INTEGER(i4b), DIMENSION(:), INTENT(IN) :: iarr
    INTEGER(i4b), DIMENSION(1) :: imax
    INTEGER(i4b) :: imaxloc_i
    imax=maxloc(iarr(:))
    imaxloc_i=imax(1)
  END FUNCTION imaxloc_i
!BL
  FUNCTION assert_eq2(n1,n2,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    INTEGER(i4b), INTENT(IN) :: n1,n2
    INTEGER(i4b) :: assert_eq2
    IF (n1 == n2) then
      assert_eq2=n1
    ELSE
      write (lfnerr,'(A,A)')&
           '### Function assert_eq2 failed with this tag:',string
      CALL exitrc(2)
    ENDIF
  END FUNCTION assert_eq2
!BL
  FUNCTION assert_eq3(n1,n2,n3,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    INTEGER(i4b), INTENT(IN) :: n1,n2,n3
    INTEGER(i4b) :: assert_eq3
    IF (n1 == n2 .and. n2 == n3) then
      assert_eq3=n1
    ELSE
      write (lfnerr,'(A,A)')&
           '### Function assert_eq3 failed with this tag:',string
      CALL exitrc(2)
    ENDIF
  END FUNCTION assert_eq3
!BL
  FUNCTION assert_eq4(n1,n2,n3,n4,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    INTEGER(i4b), INTENT(IN) :: n1,n2,n3,n4
    INTEGER(i4b) :: assert_eq4
    IF (n1 == n2 .and. n2 == n3 .and. n3 == n4) then
      assert_eq4=n1
    ELSE
      write (lfnerr,'(A,A)')&
           '### Function assert_eq4 failed with this tag:',string
      CALL exitrc(2)
    ENDIF
  END FUNCTION assert_eq4
!BL
  FUNCTION assert_eqn(nn,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    INTEGER(i4b), DIMENSION(:), INTENT(IN) :: nn
    INTEGER(i4b) :: assert_eqn
    IF (all(nn(2:) == nn(1))) then
      assert_eqn=nn(1)
    ELSE
      write (lfnerr,'(A,A)')&
           '### Function assert_eqn failed with this tag:',string
      CALL exitrc(2)
    ENDIF
  END FUNCTION assert_eqn
!BL
  FUNCTION arth_d(first,increment,n)
    REAL(r8b), INTENT(IN) :: first,increment
    INTEGER(i4b), INTENT(IN) :: n
    REAL(r8b), DIMENSION(n) :: arth_d
    INTEGER(i4b) :: k,k2
    REAL(r8b) :: temp
    IF (n > 0) arth_d(1)=first
    IF (n <= NPAR_ARTH) then
      DO k=2,n
        arth_d(k)=arth_d(k-1)+increment
      ENDDO
    ELSE
      DO k=2,NPAR2_ARTH
        arth_d(k)=arth_d(k-1)+increment
      ENDDO
      temp=increment*NPAR2_ARTH
      k=NPAR2_ARTH
      DO
        IF (k >= n) EXIT
        k2=k+k
        arth_d(k+1:min(k2,n))=temp+arth_d(1:min(k,n-k))
        temp=temp+temp
        k=k2
      ENDDO
    ENDIF
  END FUNCTION arth_d
!BL
  FUNCTION arth_i(first,increment,n)
    INTEGER(i4b), INTENT(IN) :: first,increment,n
    INTEGER(i4b), DIMENSION(n) :: arth_i
    INTEGER(i4b) :: k,k2,temp
    IF (n > 0) arth_i(1)=first
    IF (n <= NPAR_ARTH) then
      DO k=2,n
        arth_i(k)=arth_i(k-1)+increment
      ENDDO
    ELSE
      DO k=2,NPAR2_ARTH
        arth_i(k)=arth_i(k-1)+increment
      ENDDO
      temp=increment*NPAR2_ARTH
      k=NPAR2_ARTH
      DO
        IF (k >= n) EXIT
        k2=k+k
        arth_i(k+1:min(k2,n))=temp+arth_i(1:min(k,n-k))
        temp=temp+temp
        k=k2
      ENDDO
    ENDIF
  END FUNCTION arth_i
END MODULE d_nrutil
