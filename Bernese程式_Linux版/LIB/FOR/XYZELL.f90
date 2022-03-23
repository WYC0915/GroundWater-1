MODULE s_XYZELL
CONTAINS
!
  SUBROUTINE XYZELL(AELL,BELL,DXELL,DRELL,SCELL,XSTAT,XSTELL)
!
!  NAME       :  XYZELL
!
!     CALL XYZELL2(AELL,BELL,DXELL,DRELL,SCELL,XSTAT,XSTELL)
!
!  PURPOSE    :  COMPUTATION OF ELLIPSOIDAL COORDINATES "XSTELL"
!                GIVEN THE CARTESIAN COORDINATES "XSTAT" in a
!                closed formulation
!                [AD] H. Vermeille, 2002: "Direct transformation from
!                     geocentric coordinates to geodetic coordinates",
!                     Journal of geodesy (2002) 76: 451-454
!
!  PARAMETERS :
!          IN :  AELL   : SEMI-MAJOR AXIS OF THE REFERENCE    R*8
!                         ELLIPSOID IN METERS
!                BELL   : SEMI-MINOR AXIS OF THE REFERENCE    R*8
!                         ELLIPSOID IN METERS
!                DXELL(3): TRANSLATION COMPONENTS FROM THE    R*8
!                         ORIGIN OF THE CART. COORD. SYSTEM
!                         (X,Y,Z) TO THE CENTER OF THE REF.
!                         ELLIPSOID (IN METRES)
!                DRELL(3): ROTATIONS TO WGS-84 (RADIAN)       R*8
!                SCELL  : SCALE FACTOR BETWEEN REF. ELLIPSOID R*8
!                         AND WGS-84
!                XSTAT(3): CARTESIAN COORDINATES (M)          R*8
!         OUT :  XSTELL(3): ELLIPSOIDAL COORDINATES           R*8
!                         XSTELL(1): ELL. LATITUDE (RADIAN)
!                         XSTELL(2): ELL. LONGITUDE (RADIAN)
!                         XSTELL(3): ELL. HEIGHT (M)
!
!  SR CALLED  :  DMLMTV
!
!  REMARKS    :  ---
!
!  AUTHOR     :  A. Gäde
!
!  VERSION    :  5.1
!
!  CREATED    :  16-Aug-2006
!
!  LAST MOD.  :  08-Nov-2006
!
!  CHANGES    :  24-Aug-2006 AG: No warning if geocenter
!                08-Nov-2006 LO/AG: Use of STR instead of S in case of
!                                approximate latitude
!
!  COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!                 UNIVERSITY OF BERN
!                     SWITZERLAND
!
!
  USE m_bern
  USE s_dmlmtv
  IMPLICIT NONE
!
! DECLARATIONS INSTEAD OF IMPLICIT
! --------------------------------
  REAL(r8b)    :: AELL , BELL , SCELL, DXELL(3), DRELL(3)
  REAL(r8b)    :: H , PHI , RLAM
  REAL(r8b)    :: STR , SA , SB , SC , CA , CB , CC , E2
  REAL(r8b)    :: XSTAT(3),XSTELL(3)
  REAL(r8b)    :: XP(3),ROT(3,3)
  REAL(r8b)    :: p,q,r,s,t,u,v,w,k,D
!
  INTEGER(i4b) :: I
!
! Transformation
! --------------
  DO I=1,3
    XP(I)=(XSTAT(I)-DXELL(I))/SCELL
  END DO
!
  SA=SIN(DRELL(1))
  CA=COS(DRELL(1))
  SB=SIN(DRELL(2))
  CB=COS(DRELL(2))
  SC=SIN(DRELL(3))
  CC=COS(DRELL(3))
!
  ROT(1,1)=CB*CC
  ROT(1,2)=CA*SC+SA*SB*CC
  ROT(1,3)=SA*SC-CA*SB*CC
  ROT(2,1)=-CB*SC
  ROT(2,2)=CA*CC-SA*SB*SC
  ROT(2,3)=SA*CC+CA*SB*SC
  ROT(3,1)=SB
  ROT(3,2)=-SA*CB
  ROT(3,3)=CA*CB
!
  CALL DMLMTV(XP,ROT,XP)
!
! Compute e**2 and STR
  E2=(AELL**2-BELL**2)/AELL**2
  STR=SQRT(XP(1)**2+XP(2)**2)
!
! Check if X and Y are both zero, compute longitude
  IF (STR.NE.0.D0) THEN
    RLAM=ATAN2(XP(2),XP(1))
  ELSE
    RLAM=0.D0
  ENDIF

! Compute k and D
  p=(XP(1)**2+XP(2)**2)/AELL**2
  q=(1D0-E2)/AELL**2*XP(3)**2
  r=(p+q-E2**2)/6D0
! If coordinate is near the geocenter
  IF (r <= 0D0)THEN
    IF((STR**2+XP(3)**2)>=1D-4)             &
      WRITE(lfnerr,"(/,A,/,16X,A,/,16X,A,/)")                    &
      ' ### SR XYZELL: HOW DID YOU GET THE SIGNAL?',             &
                      'Coordinate is in the near of geocenter!', &
                      'Approximation algorithm used.'
! Compute approximate H
    H=DSQRT(XP(1)**2+XP(2)**2+XP(3)**2)-AELL
! Compute approximate latitude
    IF (AELL+H.NE.0.D0) THEN
      PHI=DATAN2(XP(3),STR*(1.D0-E2*AELL/(AELL+H)))
    ELSE
      PHI=0.D0
    ENDIF
! Normal case
  ELSE
    s=E2**2*p*q/(4*r**3)
    t=(1D0+s+SQRT(s*(2+s)))**(1D0/3D0)
    u=r*(1D0+t+1D0/t)
    v=SQRT(u**2+E2**2*q)
    w=E2*(u+v-q)/(2D0*v)
    k=SQRT(u+v+w**2)-w
    D=k*SQRT(XP(1)**2+XP(2)**2)/(k+E2)
!
! Compute latitude
    PHI=ATAN2(XP(3),D)
!
! Compute H
    H=((k+E2-1)/k)*SQRT(D**2+XP(3)**2)
!
  ENDIF
  XSTELL(1)=PHI
  XSTELL(2)=RLAM
  XSTELL(3)=H
!
  RETURN
END SUBROUTINE XYZELL
END MODULE s_XYZELL
