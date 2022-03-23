MODULE s_CORRTID
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE CORRTID(xsta,xmjdb,xmjde,dtide)

! -------------------------------------------------------------------------
! Purpose:    Correct step 2 error in BSW
!
! Author:     U. Hugentobler
!
! Created:    03-Jul-2004
! Last mod.:  06-May-2009
!
! Changes:    06-May-2009 RD: Adapt from version 5.0 to 5.1
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  USE s_step2diu
  IMPLICIT NONE

! List of parameters
! ------------------
  ! input
  REAL(r8b),DIMENSION(3)   :: xsta         ! Station coordinates
  REAL(r8b)                :: xmjdb        ! Begin of the interval
  REAL(r8b)                :: xmjde        ! End of the interval
  ! output
  REAL(r8b),DIMENSION(3)   :: dtide        ! Mean effect of the bug
                                           ! for the given interval

! Local variables
! ---------------
  INTEGER(i4b)             :: j,npos
  REAL(r8b),DIMENSION(3)   :: dxtidb,dxtide
  real(r8b)                :: dmjd,t,fhr,step

! integration for epochs every 30 min
! -----------------------------------
  npos=1440/30
  step=1D0/npos

! xmjdb=xmjde: average correction for day xmjdb, 0-24h
  IF (xmjdb == xmjde) THEN
    dmjd = DINT(xmjdb)+step/2d0

! xmjde=0:     correction for epoch xmjdb
  ELSEIF (xmjde < xmjdb) THEN
    npos = 1
    dmjd = xmjdb

! xmjde>xmjdb: average correction for epochs xmjdb to xmjde
  ELSE
    dmjd = xmjdb
    npos = IDNINT(npos*(xmjde-xmjdb))
  ENDIF

  dtide=0d0

  DO j=1,npos
    T = (DMJD-51544.5D0)/36525.D0

    FHR = DMJD-INT(DMJD)
    CALL STEP2DIU(XSTA,FHR,T,dxtide)
    FHR = (DMJD-INT(DMJD))*24d0
    CALL STEP2DIU(XSTA,FHR,T,dxtidb)

    dxtide=dxtidb-dxtide
! negative sign!
    dtide=dtide-dxtide

    dmjd=dmjd+step
  ENDDO
  dtide=dtide/npos

  RETURN
END SUBROUTINE corrtid

END MODULE
