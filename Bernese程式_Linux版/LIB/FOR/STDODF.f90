MODULE s_STDODF
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE stdodf(filst1,tepo,dt,svn,nhelm,ihelm,helm,imano, &
                  dxv,dtran,coef,ircode)

! -------------------------------------------------------------------------
! Purpose:    Determine difference between two standard orbits for a
!             specified epoch in RSW w.r.t. first standard orbit.
!             Subroutine of STDDIF
!
! Author:     U. Hugentobler
!
! Created:    16-May-2001
! Last.mod:   15-Jan-2004
!
! Changes:    15-Jan-2004 HU: Compare maneuvering satellites
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  USE f_modsvn
  USE s_getorb
  USE s_getorf
  USE s_exitrc
  USE s_resorb
  IMPLICIT NONE

! List of Parameters
! ------------------
!
! IN
  CHARACTER(LEN=*)          :: filst1   ! name of second std file
  REAL(r8b)                 :: tepo     ! epoch in mjd
  REAL(r8b)                 :: dt       ! relative epoch (days)
  INTEGER(i4b)              :: svn      ! svn number of satellite
  INTEGER(i4b)              :: nhelm    ! number of helmert parameters
  INTEGER(i4b),DIMENSION(8) :: ihelm    ! set up helmert transformation for
                                        ! 1-3: transl, 4-6: rot,
                                        ! 7: rate in rotz, 8 scale
  REAL(r8b),DIMENSION(8)    :: helm     ! helmert parameters
  INTEGER(i4b)              :: imano    ! 1: compare maneuvering satellites
!
! OUT
  REAL(r8b),DIMENSION(6)    :: dxv      ! orbit difference in x and v in XYZ
  REAL(r8b),DIMENSION(6)    :: dtran    ! orbit difference in x and v in RSW
  REAL(r8b),DIMENSION(6,8)  :: coef     ! coefficients for helmert transf.
  INTEGER(i4b)              :: ircode   ! return code
                                        !  =1: tepo not found in std
                                        !  =2: satellite not found for tepo
! **************************************************************************
!
! Local Parameters
! ----------------
  INTEGER(i4b)  :: ideriv=1                 ! number of derivatives to compute
  INTEGER(i4b)  :: i
  INTEGER(i4b)  :: icrarc,iorsys,iorsy2,irc ! parameters of getorb/getorf
  INTEGER(i4b)  :: svnm

  REAL(r8b),DIMENSION(6)    :: xv1,xv2      ! position & velocity
  REAL(r8b)                 :: tosc
  REAL(r8b),DIMENSION(7)    :: ele

  CHARACTER(LEN=7),DIMENSION(2) ::  orbsys=(/'B1950.0','J2000.0'/)

  ircode=0
! ------------------------------------------------------------------------
!
! GET SATELLITE POSITIONS AND VELOCITIES
! --------------------------------------
! first standard orbit
  IF (imano==1) THEN
    svnm=MODSVN(svn)
!!  CALL getorf(filst1,svnm,0,ideriv,0,tepo,icrarc,iorsys,xv1,tosc,ele,irc)
    CALL getorf(filst1,svnm,0,ideriv,2,tepo,icrarc,iorsys,xv1,tosc,ele,irc)
  ELSE
!!  CALL getorb(svn,0,ideriv,0,tepo,icrarc,iorsys,xv1,tosc,ele,irc)
    CALL getorb(svn,0,ideriv,2,tepo,icrarc,iorsys,xv1,tosc,ele,irc)
  ENDIF
  IF (irc.NE.0) THEN
    ircode=irc
    RETURN
  ENDIF
!
! second standard orbit
!!CALL getorf(filst1,svn,0,ideriv,0,tepo,icrarc,iorsy2,xv2,tosc,ele,irc)
  CALL getorf(filst1,svn,0,ideriv,2,tepo,icrarc,iorsy2,xv2,tosc,ele,irc)
  IF (irc.NE.0) THEN
    ircode=irc
    RETURN
  ENDIF

! different reference system
  IF (iorsys.NE.iorsy2) THEN
    WRITE(lfnerr,"(/,' *** PG STDDIF: INCONSISTENT ORBIT SYSTEMS',/,&
                                & 16X,'SYSTEM 1: ',A,/,&
                                & 16X,'SYSTEM 2: ',A,/)") &
                                orbsys(iorsys),orbsys(iorsy2)
    CALL EXITRC(2)
  ENDIF

! orbit difference
  dxv(1:6)=xv2(1:6)-xv1(1:6)


!
! COEFFICIENT MATRIX FOR HELMERT PARAMETER ESTIMATION
! ---------------------------------------------------
!    x' =  x  +dx        +rz*y -ry*z  +a*t*y  +s*x
!    y' =  y  +dy  -rz*x       +rx*z  -a*t*x  +s*y
!    z' =  z  +dz  +ry*x -rx*y                +s*z
!
!   vx' = vx         +rz*vy -ry*vz  +a*y +a*t*vy  + s*vx
!   vy' = vy  -rz*vx        -rx*vz  -a*x -a*t*vx  + s*vy
!   vz' = vz  +ry*vx -rx*vy                       + s*vz
!
  IF(nhelm.GT.0)THEN
    coef=0D0
    coef(1,1)=1D0
    coef(2,2)=1D0
    coef(3,3)=1D0
    coef(2,4)= xv2(3)
    coef(3,4)=-xv2(2)
    coef(5,4)= xv2(6)
    coef(6,4)=-xv2(5)
    coef(1,5)=-xv2(3)
    coef(3,5)= xv2(1)
    coef(4,5)=-xv2(6)
    coef(6,5)= xv2(4)
    coef(1,6)= xv2(2)
    coef(2,6)=-xv2(1)
    coef(4,6)= xv2(5)
    coef(5,6)=-xv2(4)
    coef(1,7)= dt*xv2(2)
    coef(2,7)=-dt*xv2(1)
    coef(4,7)= dt*xv2(5)+xv2(2)
    coef(5,7)=-dt*xv2(4)-xv2(1)
    coef(1:6,8)= xv2(1:6)
    DO i=1,8
      IF(ihelm(i).EQ.0)coef(1:6,i)=0D0
    ENDDO
!
! APPLY HELMERT TRANSFORMATION
! ----------------------------
    DO i=1,8
      dxv(1:6)=dxv(1:6)-coef(1:6,i)*helm(i)
    ENDDO
  ENDIF

!
! TRANSFORMATION INTO RADIAL, ALONG TRACK AND OUT OF PLANE
! --------------------------------------------------------
  CALL resorb(dxv(1),xv1,xv1(4),dtran(1))
  CALL resorb(dxv(4),xv1,xv1(4),dtran(4))

  RETURN

END SUBROUTINE stdodf


END MODULE
