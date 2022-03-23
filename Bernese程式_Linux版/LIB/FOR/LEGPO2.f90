MODULE s_LEGPO2
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE legpo2(ir,m,nmx,theta,iflag,rleg,dleg,sleg)

!!-------------------------------------------------------------------------
!! NAME       : LEGPO2
!!
!! PURPOSE    : COMPUTATION OF FULLY NORMALIZED SPHERICAL HARMONICS
!!              - THIS SUBROUTINE COMPUTES ALL NORMALIZED LEGENDRE
!!                FUNCTIONS AND THEIR DERIVATIVES FOR GIVEN ORDER M
!! PARAMETERS :
!!        IN  : IR    = INTEGER EQUAL TO ZERO BY THE FIRST CALL TO SR
!!              M     = ORDER OF LEGENDRE FUNCTIONS
!!              NMX   = MAXIMUM DEGREE OF LEGENDRE FUNCTIONS
!!              THETA = COLATITUDE IN RADIANS
!!              IFLAG = 0 NO DERIVATIVES
!!                    = 1 +  FIRST DERIVATIVES
!!                    = 2 +  SECOND DERIVATIVES
!!       OUT  : RLEG = NORMALIZED LEGENDRE FUNCTIONS
!!              DLEG = FIRST DERIVATIVES
!!              SLEG = SECOND DERIVATIVES
!!
!! SR CALLED  : EXITRC
!!
!! REMARKS    : REPLACE OF SR LEGPOL
!!              ORIGINAL SR LEGFDN FROM ROBERT WEBER + SECOND DERIVATIVES
!!              - THE DIMENSIONS OF THE ARRAYS RLEG,DLEG,RLNN MUST BE
!!               AT LEAST EQUAL TO NMX+1.
!!               IF THIS SUBROUTINE IS TO BE USED TO COMPUTE FUNCTIONS
!!               AND THEIR DERIVATIVES FOR MORE THAN ONE ORDER M, THEN
!!               THE HIGHEST ORDER SHOULD BE COMPUTED IN THE FIRST CALL.
!!              - SET IR=0 FOR EACH CALL WITH DIFFERENT THETA, NMX, OR IFLAG
!!
!! AUTHOR     : H. BOCK
!!
!! VERSION    : 4.3
!!
!! CREATED    : 00/04/28
!!
!! CHANGES    : 18-May-00 : HU: MAXPOT included, not taken from M_BERN
!!              29-Jul-01 : HU: Dynamic memory allocation
!!              20-Jul-05 : HB: Split if-statement in two separate if's
!!
!! COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!!      1999      UNIVERSITY OF BERN
!!                    SWITZERLAND
!!------------------------------------------------------------------------

  USE m_bern, ONLY : i4b,r8b,lfnerr
  USE s_alcerr
  USE s_exitrc
  IMPLICIT NONE

! DUMMY ARGUMENTS

  INTEGER(i4b) :: nmx
  INTEGER(i4b) :: m
  INTEGER(i4b) :: iflag
  INTEGER(i4b) :: ir

  REAL(r8b)    :: theta
  REAL(r8b),DIMENSION(*)      :: rleg
  REAL(r8b),DIMENSION(*)      :: dleg
  REAL(r8b),DIMENSION(*)      :: sleg

! PARAMETER

  INTEGER(i4b)      :: nmx1,nmx2p
  INTEGER(i4b)      :: n,n1,n2,m1,m2,m3
  INTEGER(i4b)      :: ios
  INTEGER(i4b),SAVE :: ifirst=1

  REAL(r8b),SAVE    :: cothet
  REAL(r8b),SAVE    :: sithet
  REAL(r8b),SAVE    :: sith1

  REAL(r8b),DIMENSION(:),ALLOCATABLE,SAVE :: rlnn
  REAL(r8b),DIMENSION(:),ALLOCATABLE,SAVE :: dlnn
  REAL(r8b),DIMENSION(:),ALLOCATABLE,SAVE :: slnn
  REAL(r8b),DIMENSION(:),ALLOCATABLE,SAVE :: drts
  REAL(r8b),DIMENSION(:),ALLOCATABLE,SAVE :: dirt

  nmx1=nmx+1
  nmx2p=2*nmx+1
  m1=m+1
  m2=m+2
  m3=m+3

! FIRST CALL OF SR FOR SPECIAL THETA
! ----------------------------------
  IF (ir /= 1) THEN
!   write(*,*)'...initialize computation of legendre polynomials'
    ir=1
! Check if arrays are allocated
    IF (ALLOCATED(rlnn)) THEN
      IF (nmx1 > SIZE(rlnn)) THEN
        DEALLOCATE(rlnn)
        DEALLOCATE(dlnn)
        DEALLOCATE(slnn)
        DEALLOCATE(drts)
        DEALLOCATE(dirt)
      ENDIF
    ENDIF
! Allocate arrays if necessary
    IF (.NOT.ALLOCATED(rlnn)) THEN
!     write(*,*)'...allocate memory'
      ALLOCATE(rlnn(nmx1),stat=ios)
      CALL alcerr(ios,'rlnn',(/nmx1/),'LEGPO2')
      ALLOCATE(dlnn(nmx1),stat=ios)
      CALL alcerr(ios,'dlnn',(/nmx1/),'LEGPO2')
      ALLOCATE(slnn(nmx1),stat=ios)
      CALL alcerr(ios,'slnn',(/nmx1/),'LEGPO2')
      ALLOCATE(drts(nmx2p),stat=ios)
      CALL alcerr(ios,'drts',(/2*nmx2p/),'LEGPO2')
      ALLOCATE(dirt(nmx2p),stat=ios)
      CALL alcerr(ios,'dirt',(/nmx2p/),'LEGPO2')
      ifirst=1
    ENDIF
    IF (ifirst==1) THEN
      DO n=1,nmx2p
        drts(n)=DSQRT(n*1.D0)
        dirt(n)=1.D0/drts(n)
      ENDDO
    ENDIF
    cothet=DCOS(theta)
    sithet=DSIN(theta)
    IF (iflag /= 0) THEN
      IF (sithet == 0.D0) THEN
        WRITE(lfnerr,"(/,' *** SR LEGPO2: sin(THETA) = 0D0', &
             & /,'                Division by zero will be forced!', &
             & /,'                Computation of Legendre polynomials stopped')")
        CALL exitrc(2)
      ENDIF
      sith1=1.D0/sithet
    ENDIF

! LEGENDRE FUNCTIONS FOR N=M
    rlnn(1)=1.D0
    rlnn(2)=sithet*drts(3)
    IF (nmx1>=3) THEN
      DO n1=3,nmx1
        n=n1-1
        n2=2*n
        rlnn(n1)=drts(n2+1)*dirt(n2)*sithet*rlnn(n)
      ENDDO
    ENDIF

! FIRST DERIVATIVES FOR N=M
    IF (iflag /= 0) THEN
      dlnn(1)=0.0D0
      dlnn(2)=drts(3)*cothet
      IF (nmx1>=3) THEN
        DO n1=3,nmx1
          n=n1-1
          n2=2*n
          dlnn(n1)=drts(n2+1)*dirt(n2)*(sithet*dlnn(n)+cothet*rlnn(n))
        ENDDO
      ENDIF

! SECOND DERIVATIVES FOR N=M
      IF (iflag == 2) THEN
        slnn(1)=0.D0
        slnn(2)=-drts(3)*sithet
        IF (nmx1>=3) THEN
          DO n1=3,nmx1
            n=n1-1
            n2=2*n
            slnn(n1)=drts(n2+1)*dirt(n2)*(-sithet*rlnn(n)+&
                     2*cothet*dlnn(n)+sithet*slnn(n))
          ENDDO
        ENDIF
      ENDIF
    ENDIF
    ifirst=0
  ENDIF
!
! COMPUTE THE  LEGENDRE FUNCTIONS
! -------------------------------
  IF (m <= 1) THEN
    IF (m == 0) THEN
      rleg(1)=1.D0
      rleg(2)=cothet*drts(3)
    ELSE
      rleg(2)=rlnn(2)
      rleg(3)=drts(5)*cothet*rleg(2)
    ENDIF
  ENDIF
  rleg(m1)=rlnn(m1)
  IF (m /= nmx ) THEN
    rleg(m2)=drts(m1*2+1)*cothet*rleg(m1)
    IF (m2 /= nmx1 )THEN
      DO n1=m3,nmx1
        n=n1-1
        n2=2*n
        rleg(n1)=drts(n2+1)*dirt(n+m)*dirt(n-m)*&
                (drts(n2-1)*cothet*rleg(n1-1)&
                -drts(n+m-1)*drts(n-m-1)*dirt(n2-3)*rleg(n1-2))
      ENDDO
    ENDIF
  ENDIF

  IF (iflag /= 0) THEN

! COMPUTE ALL FIRST DERIVATIVES OF THE LEGENDRE FUNCTIONS
! -------------------------------------------------------
    dleg(m1)=dlnn(m1)
    DO n1=m2,nmx1
      n=n1-1
      n2=n*2
      dleg(n1)=sith1*(n*rleg(n1)*cothet-drts(n-m)*drts(n+m)*&
               drts(n2+1)*dirt(n2-1)*rleg(n))
    ENDDO

! COMPUTE ALL SECOND DERIVATIVES OF THE LEGENDRE FUNCTIONS
! --------------------------------------------------------
    IF (iflag == 2) THEN
      sleg(m1)=slnn(m1)
      DO n1=m2,nmx1
        n=n1-1
        n2=n*2
        sleg(n1)=sith1*((n-1)*dleg(n1)*cothet-drts(n-m)*drts(n+m)*&
                 drts(n2+1)*dirt(n2-1)*dleg(n)-n*rleg(n1)*sithet)
      ENDDO
    ENDIF
  ENDIF

  RETURN
END SUBROUTINE legpo2

END MODULE
