MODULE s_TRUEARTH
CONTAINS

! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

  SUBROUTINE truearth(xleo,xleoOUT,idir,ider,sz,xpol,ypol)

! ------------------------------------------------------------------------------
!
! NAME       :  TRUEARTH
!
! PURPOSE    :  Relates true system of date and earth-fixed system
!               (position, velocity and acceleration.
!
! SR CALLED  :  DDREH, EXITRC
!
! REMARKS    :
!
! AUTHOR     :  D. Svehla
!
! CREATED    :  20-Mar-2001                       LAST MODIFIED : 15-Dec-2001
!
! CHANGES    :  15-Dec-2001  HU: Use d_const and implicit none
!
! COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!                UNIVERSITY OF BERN
!                    SWITZERLAND
! ------------------------------------------------------------------------------

  USE m_bern
  USE d_const, ONLY: omega
  USE s_ddreh
  IMPLICIT NONE

!
! Parameters
! ==========
!
! IN :
! ----
  REAL(r8b),DIMENSION(*)          :: xleo   ! Position (m), velocity (m/s),
                                            !   acceleration(m/s**2)
  INTEGER(i4b)                    :: idir   ! Direction of transformation
                                            !  =0: true system --> earth-fixed
                                            !  =1: earth-fixed --> true system
  INTEGER(i4b)                    :: ider   ! Number of derivatives in xleo
                                            !  =0:only position
                                            !  =1:position+velocity
                                            !  =2:position+velocity+acceleration
  REAL(r8b)                       :: sz     ! True siderial time
  REAL(r8b)                       :: xpol   ! Pole coordinates
  REAL(r8b)                       :: ypol   !   in (rad)
!
! OUT:
! ----
  REAL(r8b),DIMENSION(*)          :: xleoOUT! Position (m), velocity (m/s),
                                            !   acceleration(m/s**2)

!
! Local variables
! ---------------
  INTEGER(i4b)                    :: i,k
  REAL(r8b)                       :: dOMdT
  REAL(r8b),DIMENSION(3)          :: xs1,xs2,xs3,xs4
  REAL(r8b),DIMENSION(9)          :: xs
  REAL(r8b),DIMENSION(3,3)        :: rmat,drmat,d2rmat,OM,OM2

!
! Time derivatives
! ----------------
  IF (ider.GT.0) THEN
    OM(:,:)=0.D0
    OM(2,1)=-OMEGA
    OM(1,2)=OMEGA
  END IF
  IF (ider.GT.1) THEN
    dOMdT=0.D0
    OM2(:,:)=0.D0
    OM2(1,1)=-OMEGA**2
    OM2(2,2)=-OMEGA**2
  END IF
!
! Select case
! -----------
  SELECT CASE (idir)
!
! True system of epoch --> earth-fixed
! =====================================
  CASE(0)
!
! Earth rotation
! --------------
    CALL DDREH(3,sz,rmat)
    IF (ider.GT.0) THEN
      drmat=MATMUL(OM,rmat)
    END IF
    IF (ider.GT.1) THEN
      d2rmat=MATMUL(OM2,rmat)
    END IF
!
! Position
! --------
    IF (ider.GE.0) THEN
      xs(1:3)=MATMUL(rmat,xleo(1:3))
    END IF
!
! Velocity
! --------
    IF (ider.GE.1) THEN
      xs1=MATMUL(rmat,xleo(4:6))
      xs2=MATMUL(drmat,xleo(1:3))
      xs(4:6)=xs1(:)+xs2(:)
    END IF
!
! Acceleration
! ------------
    IF (ider.GE.2) THEN
      xs1=MATMUL(rmat,xleo(7:9))
      xs2=MATMUL(drmat,xleo(4:6))
      xs3=MATMUL(d2rmat,xleo(1:3))
      xs4=MATMUL(drmat,xleo(1:3))
      xs(7:9)=xs1(:)+2.D0*xs2(:)+xs3(:)+xs4(:)*dOMdT/OMEGA

    END IF
!
! Polar motion
! ------------
    DO i=1,ider+1
      k=3*(i-1)
      xleoOUT(k+1)=      xs(k+1)             +xpol*xs(k+3)
      xleoOUT(k+2)=                   xs(k+2)-ypol*xs(k+3)
      xleoOUT(k+3)=-xpol*xs(k+1)+ypol*xs(k+2)     +xs(k+3)
    END DO
!
! Earth-fixed --> true system of date
! ===================================
  CASE(1)
!
! Polar motion
! ------------
    DO i=1,ider+1
      k=3*(i-1)
      xs(k+1)=     xleo(k+1)               -xpol*xleo(k+3)
      xs(k+2)=                    xleo(k+2)+ypol*xleo(k+3)
      xs(k+3)=xpol*xleo(k+1)-ypol*xleo(k+2)     +xleo(k+3)
    END DO
!
! Earth rotation
! --------------
    CALL DDREH(3,-sz,rmat)
    IF (ider.GT.0) THEN
      OM=-OM
      drmat=MATMUL(rmat,OM)
    END IF
    IF (ider.GT.1) THEN
      d2rmat=MATMUL(rmat,OM2)
    END IF
!
! Position
! --------
    IF (ider.GE.0) THEN
      xleoOUT(1:3)=MATMUL(rmat,xs(1:3))
    END IF
!
! Velocity
! --------
    IF (ider.GE.1) THEN
      xs1=MATMUL(rmat,xs(4:6))
      xs2=MATMUL(drmat,xs(1:3))
      xleoOUT(4:6)=xs1(:)+xs2(:)
    END IF
!
! Acceleration
! ------------
    IF (ider.GE.2) THEN
      xs1=MATMUL(rmat,xs(7:9))
      xs2=MATMUL(drmat,xs(4:6))
      xs3=MATMUL(d2rmat,xs(1:3))
      xs4=MATMUL(drmat,xs(1:3))
      xleoOUT(7:9)=xs1(:)+2.D0*xs2(:)+xs3(:)+xs4(:)*dOMdT/OMEGA
    END IF
  END SELECT

  RETURN

!
! END
! ---
  END SUBROUTINE truearth

END MODULE
