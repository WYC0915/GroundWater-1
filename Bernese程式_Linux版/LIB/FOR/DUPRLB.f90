MODULE s_DUPRLB
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE duprlb(aeq,gm1,nterm,cpot,spot,xgesat,ityp,upot,du1rlb,du2rlb)

!-------------------------------------------------------------------------
! Purpose    :  First and second derivatives of Earths potential
!               Earth fixed coordinates (radius,long,lat)
!               Replace SR dgprlb
!
! PARAMETERS :
!         IN :  AEQ    : EQUATORIAL RADIUS                        R*8
!               GM     : GRAV.CONSTANT * EARTH'S MASS             R*8
!               NTERM  : MAX. ORDER OF POTENTIAL COEFFICIENTS     I*4
!               CPOT(I),SPOT(I): COEFFICIENTS OF POTENTIAL        R*8
!                                (S. SUBR. 'POTINP')
!               XGESAT(I): COORDINATES OF SATELLITE (EARTH FIXED)
!                            I=1,2,3                              R*8
!               ITYP   : ITYP=0: ONLY POTENTIAL
!                        ITYP=1: POTENTIAL + FIRST DERIVATIVES
!                        ITYP=2: POTENTIAL + BOTH DERIVATIVES     I*4
!        OUT :  UPOT   : POTENTIAL                                R*8
!               DU1RLB(I):   DU/DR     DU/DL     DU/DB            R*8
!               DU2RLB(I,K): D2U/DRDR  D2U/DRDL  D2U/DRDB         R*8
!                            D2U/DRDL  D2U/DLDL  D2U/DLDB
!                            D2U/DRDB  D2U/DLDB  D2U/DBDB
!
! Author     :  H. Bock
!
! Created    :  28-Apr-2000
! Last mod.  :  10-Mar-2004
!
! Changes    :  12-May-2000  HU: Compute derivatives only for n>0
!               18-May-2000  HU: MAXPOT included, not taken from M_BERN
!               21-Dec-2001  HU: Use d_const
!               07-Jan-2002  HB: Use interface for SR legpo2
!               17-FEb-2003  LM: Use m_maxdim
!               10-Mar-2004  HB: Change order of modules
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
  IMPLICIT NONE


! List of parameters
! ------------------
  INTEGER(i4b) :: nterm
  INTEGER(i4b) :: ityp

  REAL(r8b)    :: aeq
  REAL(r8b)    :: gm1
  REAL(r8b)    :: upot
  REAL(r8b),DIMENSION(*)    :: cpot
  REAL(r8b),DIMENSION(*)    :: spot
  REAL(r8b),DIMENSION(*)    :: xgesat
  REAL(r8b),DIMENSION(*)    :: du1rlb
  REAL(r8b),DIMENSION(3,*)  :: du2rlb

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

  REAL(r8b)    :: r,r2,r3
  REAL(r8b)    :: rxy,rxy2
  REAL(r8b)    :: adr
  REAL(r8b)    :: theta
  REAL(r8b)    :: silam
  REAL(r8b)    :: colam
  REAL(r8b)    :: ds1,ds2
  REAL(r8b)    :: term1,term2,term3,term4,term5
  REAL(r8b),DIMENSION(14)         :: ss
  REAL(r8b),DIMENSION(maxpot+1)   :: pleg
  REAL(r8b),DIMENSION(maxpot+1)   :: dleg
  REAL(r8b),DIMENSION(maxpot+1)   :: sleg
  REAL(r8b),DIMENSION(maxpot+1)   :: cm
  REAL(r8b),DIMENSION(maxpot+1)   :: sm
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
  DO ii=1,14
     ss(ii)=0.D0
  END DO
  ics   = 0
  in    = 0

!  RECURSION ALGORITHM FOR COMPUTING SIN(M*LAMDA) AND COS(M*LAMDA)
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
  LOOP_M: DO m=nterm,0,-1
     CALL legpo2(in,m,nterm,theta,ityp,pleg,dleg,sleg)
     LOOP_N: DO n=MAX0(m,1),nterm
        n1=n+1
        ipl=n+1
        ics=(n*n1)/2+m+1
        ds1=cpot(ics)*cm(m+1)+spot(ics)*sm(m+1)
        ds2=spot(ics)*cm(m+1)-cpot(ics)*sm(m+1)
        term1 = pleg(ipl) *ds1*adrn(n)
        ss(1) = ss(1) +           term1
        IF (ityp >= 1) THEN
           term2 = pleg(ipl) *ds2*adrn(n)
           term3 = dleg(ipl) *ds1*adrn(n)
           term4 = dleg(ipl) *ds2*adrn(n)
           ss(2) = ss(2) +      n1 * term1
           ss(3) = ss(3) +       m * term2
           ss(4) = ss(4) +           term3
           IF (ityp >= 2) THEN
              term5 = sleg(ipl) *ds1*adrn(n)
              ss(5) = ss(5) + n1*(n+2)* term1
              ss(6) = ss(6) +    m*n1 * term2
              ss(7) = ss(7) +      n1 * term3
              ss(8) = ss(8) +    m**2 * term1
              ss(9) = ss(9) +       m * term4
              ss(10)= ss(10)+           term5
           END IF
        END IF
     END DO LOOP_N
  END DO LOOP_M

! POTENTIAL
  upot = ss(1)

!  FIRST DERIVATIVES
  IF (ityp >= 1) THEN
     du1rlb(1)=-gm1/r2 * ss(2)
     du1rlb(2)= gm1/r  * ss(3)
     du1rlb(3)=-gm1/r  * ss(4)

!  SECOND DERIVATIVES
     IF (ityp >= 2) THEN
        du2rlb(1,1)= gm1/r3 * ss(5)
        du2rlb(1,2)=-gm1/r2 * ss(6)
        du2rlb(1,3)= gm1/r2 * ss(7)
        du2rlb(2,2)=-gm1/r  * ss(8)
        du2rlb(2,3)=-gm1/r  * ss(9)
        du2rlb(3,3)= gm1/r  * ss(10)
     END IF

     DO ii=1,3
        DO kk=1,3
           du2rlb(kk,ii)=du2rlb(ii,kk)
        END DO
     END DO
  END IF

999 CONTINUE
  RETURN
  END SUBROUTINE duprlb

END MODULE
