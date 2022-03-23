MODULE f_aslef2
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION aslef2(xlat,xlon,inn,imm,mxn,mxm)

! -------------------------------------------------------------------------
! Purpose:    Compute the (normalized) associated Legendre function
!             of degree n and order m evaluated at x.
!
! Remark:     The function computes the Legendre function only if
!             necessary. Computed terms are buffered and returned without
!             recomputation if possible.
!             For an efficient follow the following recommendations:
!              - Do not increase the maximum degree mxn and order mxm
!                from one call to the next but call the routine with
!                the maximum expected values on the first call in order
!                to avoid reallocation of memory and recomputation of
!                Legendre functions.
!              - If values on a longitude-latitude grid have to be
!                computed, organize the inner loop along circles with
!                constant latitude. For calls with equal xlat the
!                Legendre are not computed anew.
!
! Author:     U. Hugentobler
!
! Created:    30-Jul-2001
!
! Changes:    21-Dec-2001  HU: Use d_const
!             30-Jul-2002  HU: Use interface for legpo2
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY : i4b,r8b,lfnerr
  USE d_const,  ONLY : pi
  USE s_alcerr
  USE s_legpo2
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  REAL(r8b)    :: xlat    ! latitude or colatitude (rad)
  REAL(r8b)    :: xlon    ! longitude (rad)
  INTEGER(i4b) :: inn     ! degree
                          !  =+: xlat is latitude
                          !  =-: xlat is colatitude
  INTEGER(i4b) :: imm     ! order
                          !  =+: cos-term
                          !  =-: sin-term
  INTEGER(i4b) :: mxn     ! maximum requested degree
  INTEGER(i4b) :: mxm     ! maximum requested order

! output:
  REAL(r8b)    :: aslef2  ! requested Legendre function Pnm(z)*cos(m*lon)
                          !                          or Pnm(z)*sin(m*lon)


! Local Variables
! ---------------
  INTEGER(i4b)           :: ios,ndim,ii,iip1,indx,in
  INTEGER(i4b)           :: nn,mm,m1
  INTEGER(i4b),SAVE      :: mxn2p1,mxn2p2,mxmp1
  INTEGER(i4b),SAVE      :: mxnold=-1,mxmold=-1
  INTEGER(i4b),DIMENSION(:),ALLOCATABLE,SAVE :: mok


  REAL(r8b)              :: colat,cosl,sinl
  REAL(r8b),SAVE         :: oldcol=9D9,oldlon=9D9
  REAL(r8b),DIMENSION(1) :: dleg,sleg
  REAL(r8b),DIMENSION(:),ALLOCATABLE,SAVE    :: pleg     ! Legendre polynomials
  REAL(r8b),DIMENSION(:),ALLOCATABLE,SAVE    :: cosm,sinm


! Indices: Dimension of array pleg:  (mxm+1)*(2*mxn+2-mxm)/2
!          Startindes of column m :  m*(2*mxn+3-m)/2+1
!          Index of element n,m   :  m*(2*mxn+1-m)/2+n+1
!
! Arrays:  pleg(i): i=1,...,(mxm+1)*(2*mxn+2-mxm)/2
!
!          sinm(i) = sin((i-1)*lon): i=1,...,mxm+1
!          cosm(i) = cos((i-1)*lon): i=1,...,mxm+1
!
!          mok(i)=1: term for m=i+1 computed, i=1,...,mxm+1

! Allocate array
! --------------
  IF (mxn > mxnold .OR. mxm > mxmold) THEN
!   write(*,*)'...initialize, allocate memory'
    mxnold=mxn
    mxmold=mxm
    IF (mxn < mxm) THEN
      WRITE(lfnerr,"(/,' *** SR ASLEF2: max(m) > max(n) unreasonable', &
           & /,'                mxn:',I6,'    mxm:',I6)") mxn,mxm
      CALL exitrc(2)
    ENDIF

! abbreviations
    mxn2p1=2*mxn+1
    mxn2p2=2*mxn+2
    mxmp1 =mxm+1

! allocate pleg
    IF (ALLOCATED(pleg)) DEALLOCATE(pleg)
    ndim=((mxm+1)*(mxn2p2-mxm))/2
    ALLOCATE(pleg(ndim),stat=ios)
    CALL alcerr(ios,'pleg',(/ndim/),'ASLEF2')

! allocate cosl, sinl
    IF (ALLOCATED(cosm)) DEALLOCATE(cosm)
    ALLOCATE(cosm(mxmp1),stat=ios)
    CALL alcerr(ios,'cosm',(/mxmp1/),'ASLEF2')

    IF (ALLOCATED(sinm)) DEALLOCATE(sinm)
    ALLOCATE(sinm(mxmp1),stat=ios)
    CALL alcerr(ios,'sinm',(/mxmp1/),'ASLEF2')

! allocate mok
    IF (ALLOCATED(mok)) DEALLOCATE(mok)
    ALLOCATE(mok(mxmp1),stat=ios)
    CALL alcerr(ios,'mok',(/mxmp1/),'ASLEF2')

! initialize
    mok   =0
  ENDIF

! Indices n, m
! ------------
  nn=ABS(inn)
  mm=ABS(imm)
  m1=mm+1

! Indices not allowed
! -------------------
  IF (mm > mxm .OR. nn > mxn .OR. mm > nn) THEN
    WRITE(lfnerr,"(/,' *** SR ASLEF2: Input indices not allowed:', &
           & /,'                Either m>mxm, n>mxn, or m>n', &
           & /,'                m:',I6,'    mxm:',I6, &
           & /,'                n:',I6,'    mxn:',I6)") mm,mxm, nn,mxn
    CALL exitrc(2)
  ENDIF

! Co-latitude
! -----------
  IF (inn >= 0) THEN
    colat=pi/2-xlat
  ELSE
    colat=xlat
  ENDIF

! Recursion algorithm for computing sin(m*lon) and cos(m*lon)
! -----------------------------------------------------------
  IF (xlon/= oldlon) THEN
!   write(*,*)'...compute trigonometric functions, m=',0,'...',mxm
    oldlon=xlon

    cosl=COS(xlon)
    sinl=SIN(xlon)

    cosm(1)=1.D0
    sinm(1)=0.D0
    DO ii=1,mxm
      iip1=ii+1
      cosm(iip1) = cosl*cosm(ii)-sinl*sinm(ii)
      sinm(iip1) = sinl*cosm(ii)+cosl*sinm(ii)
    ENDDO
  ENDIF

! Computation of Legendre polynomial for m and all n=0,...,mxn
! ------------------------------------------------------------
  IF (colat /= oldcol .OR. mok(m1) == 0) THEN
!   write(*,*)'...compute legendre polynomials, m=', &
!                 (mm*(mxn2p1+2-mm))/2,'...',(m1*(mxn2p1+2-m1))/2

! initialization not necessary
    IF (colat == oldcol) THEN
      in=1
    ELSE
      in =0
      mok=0
    ENDIF
    oldcol=colat

    indx=(mm*(mxn2p1-mm))/2+1
    CALL legpo2(in,mm,mxn,colat,0,pleg(indx),dleg,sleg)
    mok(m1)=1
  ENDIF

! Get Pnm(z)*cos(m*lon) resp Pnm(z)*sin(m*lon)
! --------------------------------------------
  indx=(mm*(mxn2p1-mm))/2+nn+1
  IF (imm < 0) THEN
    aslef2=pleg(indx)*sinm(m1)
  ELSE
    aslef2=pleg(indx)*cosm(m1)
  ENDIF

  RETURN
END FUNCTION aslef2

END MODULE
