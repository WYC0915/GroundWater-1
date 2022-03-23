MODULE s_ITRF2ITRF
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE itrf2itrf(timcrd,datin,datout,xstat,xstnew)

! -------------------------------------------------------------------------
! Purpose:    Transformation from ITRFyy to ITRFxx
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
  CHARACTER(LEN=*)                   :: datin      ! Source datum string
  CHARACTER(LEN=*)                   :: datout     ! Target datum string
  REAL(r8b), DIMENSION(3)            :: xstat      ! Input coordinates

! output:
  REAL(r8b), DIMENSION(3)            :: xstnew     ! Output coordinates

! Local Parameters
! ----------------
  CHARACTER(LEN=9), PARAMETER        :: srname = 'ITRF2ITRF'

! Local Variables
! ---------------
  INTEGER(i4b), PARAMETER            :: ntrf = 11  ! Nbr of supported ITRFs
  CHARACTER(LEN=2), DIMENSION(ntrf)  :: trflst
  REAL(r8b), DIMENSION(ntrf,8)       :: param
  REAL(r8b), DIMENSION(ntrf,7)       :: rates
  REAL(r8b), DIMENSION(8)            :: pari, paro
  REAL(r8b), DIMENSION(7)            :: rati, rato, par
  REAL(r8b)                          :: dti, dto
  INTEGER(i4b)                       :: i, j
  REAL(r8b), DIMENSION(3)            :: rot

! Default values
! --------------
  trflst = (/'89','90','91','92','93','94','96','97','00','05','08'/)

! Transformation values (C.Boucher, Z.Altamimi; 18-05-2011; Tables 1 & 2)
! ---------------------
!                 T1[cm] T2[cm] T3[cm] D[10-8] R1[mas] R2[mas] R3[mas] t0
! ITRF89
  param( 1,:) = (/0.0d0, 0.0d0, 0.0d0, 0.0d0,  0.0d0,  0.0d0,  0.0d0, 1988d0/)
  rates( 1,:) = (/0.0d0, 0.0d0, 0.0d0, 0.0d0,  0.0d0,  0.0d0,  0.0d0/)
! ITRF90
  param( 2,:) = (/0.5d0, 2.4d0,-3.8d0, 0.34d0, 0.0d0,  0.0d0,  0.0d0, 1988d0/)
  rates( 2,:) = (/0.0d0, 0.0d0, 0.0d0, 0.0d0,  0.0d0,  0.0d0,  0.0d0/)
! ITRF91
  param( 3,:) = (/0.6d0, 2.0d0,-5.4d0, 0.37d0, 0.0d0,  0.0d0,  0.0d0, 1988d0/)
  rates( 3,:) = (/0.0d0, 0.0d0, 0.0d0, 0.0d0,  0.0d0,  0.0d0,  0.0d0/)
! ITRF92
  param( 4,:) = (/1.7d0, 3.4d0,-6.0d0, 0.51d0, 0.0d0,  0.0d0,  0.0d0, 1988d0/)
  rates( 4,:) = (/0.0d0, 0.0d0, 0.0d0, 0.0d0,  0.0d0,  0.0d0,  0.0d0/)
! ITRF93
  param( 5,:) = (/1.9d0, 4.1d0,-5.3d0, 0.39d0, 0.39d0,-0.80d0, 0.96d0,1988d0/)
  rates( 5,:) = (/0.29d0,-0.04d0,-0.08d0,0.0d0,0.11d0, 0.19d0,-0.05d0/)
! ITRF94
  param( 6,:) = (/2.3d0, 3.6d0,-6.8d0,0.43d0, 0.0d0,   0.0d0,  0.0d0, 1988d0/)
  rates( 6,:) = (/0.0d0, 0.0d0, 0.0d0,0.0d0,  0.0d0,   0.0d0,  0.0d0/)
! ITRF96
  param( 7,:) = (/2.3d0, 3.6d0,-6.8d0, 0.43d0,0.0d0,   0.0d0,  0.0d0, 1988d0/)
  rates( 7,:) = (/0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0,   0.0d0,  0.0d0/)
! ITRF97
  param( 8,:) = (/2.3d0, 3.6d0,-6.8d0, 0.43d0,0.0d0,   0.0d0,  0.0d0, 1988d0/)
  rates( 8,:) = (/0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0,   0.0d0,  0.0d0/)
! ITRF2000
  param( 9,:) = (/3.0d0, 4.2d0, -8.7d0, 0.59d0,0.0d0,  0.0d0,  0.0d0, 1997d0/)
  rates( 9,:) = (/0.0d0,-0.06d0,-0.14d0,0.0d0, 0.0d0,  0.0d0,  0.02d0/)
! ITRF2005
  param(10,:) = (/3.0d0, 3.9d0,-9.7d0,  0.63d0,0.0d0,  0.0d0,  0.06d0,2000d0/)
  rates(10,:) = (/-0.02d0,-0.05d0,-0.32d0,0.008d0,0.0d0,0.0d0, 0.02d0/)
! ITRF2008
  param(11,:) = (/2.80d0,3.81d0,-10.17d0,0.724d0,0.0d0,0.0d0, 0.060d0,2000d0/)
  rates(11,:) = (/0.01d0,-0.05d0,-0.32d0,0.008d0,0.0d0,0.0d0, 0.02d0/)

! Get transformation parameters
! -----------------------------
  pari(:) = 0d0
  paro(:) = 0d0
  DO j=1,ntrf
    IF(INDEX(datin,trflst(j))==0) CYCLE
    pari = param(j,:)
    rati = rates(j,:)
  ENDDO
  DO j=1,ntrf
    IF(INDEX(datout,trflst(j))==0) CYCLE
    paro = param(j,:)
    rato = rates(j,:)
  ENDDO
  dti = (timcrd-DJUL(NINT(pari(8)),1,0.0d0))/365.25d0
  dto = (timcrd-DJUL(NINT(paro(8)),1,0.0d0))/365.25d0
  DO i=1,7
    IF(i==1.OR.i==2.OR.i==3) THEN
      par(i) = ((pari(i)+rati(i)*dti)-(paro(i)+rato(i)*dto))*1d-2
    ELSEIF(i==4) THEN
      par(i) = ((pari(i)+rati(i)*dti)-(paro(i)+rato(i)*dto))*1d-8
    ELSEIF(i==5.OR.i==6.OR.i==7) THEN
      par(i) = ((pari(i)+rati(i)*dti)-(paro(i)+rato(i)*dto))*1d-3/ars
    ENDIF
  ENDDO

! Compute rotation matrix
! -----------------------
  rot(1) =  par(4)*xstat(1) -par(7)*xstat(2) +par(6)*xstat(3)
  rot(2) =  par(7)*xstat(1) +par(4)*xstat(2) -par(5)*xstat(3)
  rot(3) = -par(6)*xstat(1) +par(5)*xstat(2) +par(4)*xstat(3)

! Compute transformation
! ----------------------
  DO i=1,3
    xstnew(i) = xstat(i)+par(i)+rot(i)
  ENDDO

  RETURN

END SUBROUTINE itrf2itrf

END MODULE
