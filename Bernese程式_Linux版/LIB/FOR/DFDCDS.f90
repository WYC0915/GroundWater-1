MODULE s_DFDCDS
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE dfdcds(aeq,gm1,nterm,xgesat,dfdc,dfds)

!-------------------------------------------------------------------------
! Purpose    :  Explicit derivative of force vector (gradient of potential)
!               w.r.t. Cnm, Snm. All derivatives are calculated and stored
!               in the arrays dfdc and dfds
!               subroutine based on sr duprlb (author: H. Bock)
!
! PARAMETERS :
!         IN :  AEQ    : EQUATORIAL RADIUS                        R*8
!               GM     : GRAV.CONSTANT * EARTH'S MASS             R*8
!               NTERM  : MAX. ORDER OF POTENTIAL COEFFICIENTS     I*4
!               XGESAT(I): COORDINATES OF SATELLITE (EARTH FIXED)
!                            I=1,2,3                              R*8
!        OUT :  dfdc(i): partial of f w.r.t. Cnm                  R*8
!               dfds(i): partial of f w.r.t. Snm                  R*8
!
! Author     :  H. Bock / G. Beutler
!
! Created    :  04-Aug-2005
! Last mod.  :
!
!
! SR used    :  legpo2
!
! Copyright  :  Astronomical Institute
!               University of Bern
!               Switzerland
!------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,  ONLY: i4b,r8b
  USE m_maxdim,ONLY: MAXPOT
  USE d_const, ONLY: pi
  USE s_legpo2
  USE s_legpo2
  IMPLICIT NONE


! List of parameters
! ------------------
  INTEGER(i4b) :: nterm

  REAL(r8b)    :: aeq
  REAL(r8b)    :: gm1
  REAL(r8b)    :: upot
  REAL(r8b),DIMENSION(3,*)  :: dfdc
  REAL(r8b),DIMENSION(3,*)  :: dfds
  REAL(r8b),DIMENSION(*)    :: xgesat

! List of Functions
! -----------------
! Local Types
! -----------
! Local Parameters
! ----------------

! Local variables
! ---------------
  INTEGER(i4b) :: ii,kk,nn
  INTEGER(i4b) :: n,m,n1
  INTEGER(i4b) :: ipl
  INTEGER(i4b) :: ics
  INTEGER(i4b) :: in
  INTEGER(i4b) :: ityp

  REAL(r8b)    :: r,r2,r3
  REAL(r8b)    :: rxy,rxy2
  REAL(r8b)    :: adr
  REAL(r8b)    :: theta
  REAL(r8b)    :: silam
  REAL(r8b)    :: colam
  REAL(r8b)    :: ds1_c,ds2_c,ds1_s,ds2_s
  REAL(r8b)    :: term1_c,term2_c,term3_c,term1_s,term2_s,term3_s
  REAL(r8b),DIMENSION(maxpot+1)   :: pleg
  REAL(r8b),DIMENSION(maxpot+1)   :: dleg
  REAL(r8b),DIMENSION(maxpot+1)   :: sleg
  REAL(r8b),DIMENSION(maxpot+1) :: cm
  REAL(r8b),DIMENSION(maxpot+1) :: sm
  REAL(r8b),DIMENSION(maxpot+1)   :: adrn

! SOME STARTING VALUES
! --------------------
  r2    = xgesat(1)**2+xgesat(2)**2+xgesat(3)**2
  r     = DSQRT(r2)
  r3    = r2*r
  rxy2  = xgesat(1)**2+xgesat(2)**2
  rxy   = DSQRT(rxy2)
  silam = xgesat(2)/rxy
  colam = xgesat(1)/rxy
  adr   = aeq/r
  pi    = DACOS(-1.D0)
  theta = DACOS(xgesat(3)/r)

! INITIALIZE SOME PARAMETER
! -------------------------
  ics   = 0
  in    = 0

!  RECURSION ALGORITHM FOR COMPUTING SIN(M*LAMBDA) AND COS(M*LAMBDA)
  cm(1)=1.D0
  sm(1)=0.D0
  nn=nterm+1
  DO ii=2,nn
     cm(ii) = colam*cm(ii-1)-silam*sm(ii-1)
     sm(ii) = silam*cm(ii-1)+colam*sm(ii-1)
  END DO

! COMPUTATION OF ADR**N
  DO ii=1,nterm
     adrn(ii)=adr**ii
  END DO

! COMPUTATION OF DU/DR,DU/DL, DU/DB
! ---------------------------------
  ityp=1
  LOOP_M: DO m=nterm,0,-1
     CALL legpo2(in,m,nterm,theta,ityp,pleg,dleg,sleg)
     LOOP_N: DO n=MAX0(m,1),nterm
        n1=n+1
        ipl=n+1
        ics=(n*n1)/2+m+1
        ds1_c= cm(m+1)
        ds1_s= sm(m+1)
!
        ds2_c=-sm(m+1)
        ds2_s= cm(m+1)
!
        term1_c = pleg(ipl) *ds1_c*adrn(n)
        term1_s = pleg(ipl) *ds1_s*adrn(n)
!
        term2_c = pleg(ipl) *ds2_c*adrn(n)
        term2_s = pleg(ipl) *ds2_s*adrn(n)
!
        term3_s = dleg(ipl) *ds1_s*adrn(n)
        term3_c = dleg(ipl) *ds1_c*adrn(n)
!
        dfdc(1,ics)=-gm1/r2 * n1 * term1_c
        dfdc(2,ics)= gm1/r  *  m * term2_c
                dfdc(3,ics)=-gm1/r  *      term3_c
!
        dfds(1,ics)=-gm1/r2 * n1 * term1_s
        dfds(2,ics)= gm1/r  *  m * term2_s
        dfds(3,ics)=-gm1/r  *      term3_s
     END DO LOOP_N
  END DO LOOP_M

999 CONTINUE
  RETURN
  END SUBROUTINE dfdcds

END MODULE
