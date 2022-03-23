MODULE s_ITRF2ETRF
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE itrf2etrf(timcrd,datum,trafo,xstat,xstnew)

! -------------------------------------------------------------------------
! Purpose:    Transformation from ITRFxx to ETRFxx
!
! Author:     S.Lutz
!
! Created:    04-Nov-2011
!
! Changes:    __-___-____ __:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b
  USE d_const,  ONLY: ars
  USE f_djul
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  REAL(r8b)                          :: timcrd     ! Epoch of coordinates [MJD]
  CHARACTER(LEN=*)                   :: datum      ! Datum string
  INTEGER(i4b)                       :: trafo      ! Sign of tranformation
  REAL(r8b), DIMENSION(3)            :: xstat      ! Input coordinates

! output:
  REAL(r8b), DIMENSION(3)            :: xstnew     ! Output coordinates

! Local Parameters
! ----------------
  CHARACTER(LEN=9), PARAMETER        :: srname = 'ITRF2ETRF'

! Local Variables
! ---------------
  INTEGER(i4b), PARAMETER            :: ntrf = 10  ! Nbr of supported ITRFs
  CHARACTER(LEN=2), DIMENSION(ntrf)  :: trflst
  REAL(r8b), DIMENSION(ntrf,6)       :: param
  REAL(r8b), DIMENSION(6)            :: par
  REAL(r8b)                          :: dt
  INTEGER(i4b)                       :: i, j
  REAL(r8b), DIMENSION(3)            :: rot

! Default values
! --------------
  trflst = (/'89','90','91','92','93','94','96','97','00','05'/)

! Shift and rotation rates (C.Boucher, Z.Altamimi; 18-05-2011; Tables 3 & 4)
! ------------------------
!                  T1[cm]  T2[cm]  T3[cm]  R1[mas/y]  R2[mas/y]  R3[mas/y]
! ITRF89
  param( 1,:) = (/ 0.0d0,  0.0d0,  0.0d0,  0.11d0,    0.57d0,   -0.71d0/)
! ITRF90
  param( 2,:) = (/ 1.9d0,  2.8d0, -2.3d0,  0.11d0,    0.57d0,   -0.71d0/)
! ITRF91
  param( 3,:) = (/ 2.1d0,  2.5d0, -3.7d0,  0.21d0,    0.52d0,   -0.68d0/)
! ITRF92
  param( 4,:) = (/ 3.8d0,  4.0d0, -3.7d0,  0.21d0,    0.52d0,   -0.68d0/)
! ITRF93
  param( 5,:) = (/ 1.9d0,  5.3d0, -2.1d0,  0.32d0,    0.78d0,   -0.67d0/)
! ITRF94
  param( 6,:) = (/ 4.1d0,  4.1d0, -4.9d0,  0.20d0,    0.50d0,   -0.65d0/)
! ITRF96
  param( 7,:) = (/ 4.1d0,  4.1d0, -4.9d0,  0.20d0,    0.50d0,   -0.65d0/)
! ITRF97
  param( 8,:) = (/ 4.1d0,  4.1d0, -4.9d0,  0.20d0,    0.50d0,   -0.65d0/)
! ITRF2000
  param( 9,:) = (/ 5.4d0,  5.1d0, -4.8d0,  0.081d0,   0.490d0,  -0.792d0/)
! ITRF2005*
  param(10,:) = (/ 5.6d0,  4.8d0, -3.7d0,  0.054d0,   0.518d0,  -0.781d0/)

! Get transformation parameters
! -----------------------------
  par(:) = 0d0
  dt = (timcrd-DJUL(1989,1,0d0))/365.25d0
  DO j=1,ntrf
    IF(INDEX(datum,trflst(j))==0) CYCLE
    DO i=1,6
      IF(i==1.OR.i==2.OR.i==3) THEN
        par(i) = param(j,i)*1d-2
      ELSEIF(i==4.OR.i==5.OR.i==6) THEN
        par(i) = param(j,i)*dt*1d-3/ars
      ENDIF
    ENDDO
  ENDDO

! Apply sign of transformation
! ----------------------------
  par = trafo*par

! Compute rotation matrix elements
! --------------------------------
  rot(1) =         0        -par(6)*xstat(2) +par(5)*xstat(3)
  rot(2) =  par(6)*xstat(1)        +0        -par(4)*xstat(3)
  rot(3) = -par(5)*xstat(1) +par(4)*xstat(2)        +0

! Compute transformation
! ----------------------
  DO i=1,3
    xstnew(i) = xstat(i)+par(i)+rot(i)
  ENDDO

  RETURN

END SUBROUTINE itrf2etrf

END MODULE
