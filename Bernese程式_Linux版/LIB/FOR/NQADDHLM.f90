MODULE s_NQADDHLM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.0
! -------------------------------------------------------------------------

SUBROUTINE nqaddhlm(neq,ifil, nhelm)

! -------------------------------------------------------------------------
! Purpose:    This subroutine expands the NEQ system to estimate Helmert
!             parameters for the input solutions (according to settings in
!             WGT-file)
!             Units: [m] for all parameters
!
! Author:     D. Thaller
!
! Created:    10-Nov-2003
!
! Changes:    28-Apr-2010 DT: Adopted to BSW v5.1, (locq(1)=28)
!             16-Nov-2010 RD: Distinguish between parameter types
!             30-Nov-2010 DT: only parameters requested in WGT file
!             20-Sep-2012 RD: Use M_BERN with ONLY, remove unused variables
!
! Copyright:  Astronomical Institute
!              University of Berne
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b
  USE d_datum,  ONLY: datum
  USE d_neq,    ONLY: t_neq
  USE d_par,    ONLY: parType_constant
  USE p_addneq, ONLY: comstat, hlmFil, opt

  USE s_alcerr
  USE f_ikf
  USE f_istobeel

  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)                :: neq
  INTEGER(i4b)               :: ifil     ! neq file number
  INTEGER(i4b)               :: nhelm    ! # Helmert parameters

! Local Variables
! ---------------
  INTEGER(i4b)                               :: ii,jj,ista,ipar,iac
  INTEGER(i4b)                               :: nsta, ihelm
  INTEGER(i4b), DIMENSION(:,:), ALLOCATABLE  :: indsta

  REAL(r8b), DIMENSION(:,:), ALLOCATABLE     :: B0
  REAL(r8b), DIMENSION(:,:), ALLOCATABLE     :: aNor
  REAL(r8b), DIMENSION(:,:), ALLOCATABLE     :: NB0, NB0_i, B0TNB0
  REAL(r8b)                                  :: x0,y0,z0
  REAL(r8b), DIMENSION(3,7)                  :: R_hlm
  REAL(r8b), DIMENSION(3)                    :: bNor
  REAL(r8b), DIMENSION(:),   ALLOCATABLE     :: hlp


! Count stations (not pre-eliminated before stacking)
!  and create station index
! ---------------------------------------------------
  nsta = 0
  DO ii=1, neq%misc%npar
    IF ( neq%par(ii)%locq(1) == 1      .AND. &
         neq%par(ii)%locq(3) == 1      .AND. &
         neq%par(ii)%locq(4) == 1      .AND. &
         .NOT. isToBeEl (neq%par(ii),1,ifil) ) THEN
       nsta = nsta + 1
    END IF
  END DO
  ALLOCATE( indsta(nsta,3), stat=iac )
  CALL alcerr(iac, 'indsta', (/nsta,3/), 'nqaddhlm')

  nsta = 0
  DO ii=1, neq%misc%npar
    IF ( neq%par(ii)%locq(1) == 1      .AND. &
         neq%par(ii)%locq(4) == 1      .AND. &
         .NOT. isToBeEl (neq%par(ii),1,ifil) ) THEN

      IF ( neq%par(ii)%locq(3) == 1 ) THEN
         nsta = nsta + 1
         indsta(nsta,1) = ii

      ELSE IF ( neq%par(ii)%locq(3) == 2 ) THEN
         DO ista=1, nsta
           IF ( neq%par(ii)%name      == neq%par(indsta(ista,1))%name     .AND. &
                neq%par(ii)%time%mean == neq%par(indsta(ista,1))%time%mean ) THEN
              indsta(ista,2) = ii
              EXIT
           END IF
         END DO

      ELSE IF ( neq%par(ii)%locq(3) == 3 ) THEN
         DO ista=1, nsta
           IF ( neq%par(ii)%name      == neq%par(indsta(ista,1))%name     .AND. &
                neq%par(ii)%time%mean == neq%par(indsta(ista,1))%time%mean ) THEN
              indsta(ista,3) = ii
              EXIT
           END IF
         END DO

      END IF


    END IF
  END DO


! Allocate memory for the matrices
! --------------------------------
  ALLOCATE( B0(3,nhelm), stat=iac )
  CALL alcerr(iac, 'B0', (/3,nhelm/), 'nqaddhlm')
  ALLOCATE( aNor(neq%misc%npar,3), stat=iac )
  CALL alcerr(iac, 'aNor', (/neq%misc%npar,3/), 'nqaddhlm')
  ALLOCATE( NB0(neq%misc%npar,nhelm), stat=iac )
  CALL alcerr(iac, 'NB0', (/neq%misc%npar,nhelm/), 'nqaddhlm')
  ALLOCATE( NB0_i(3,nhelm), stat=iac )
  CALL alcerr(iac, 'NB0_i', (/3,nhelm/), 'nqaddhlm')
  ALLOCATE( B0TNB0(nhelm,nhelm), stat=iac )
  CALL alcerr(iac, 'B0TNB0', (/nhelm,nhelm/), 'nqaddhlm')
  ALLOCATE( hlp(nhelm), stat=iac )
  CALL alcerr(iac, 'hlp', (/nhelm/), 'nqaddhlm')
  B0     = 0.D0
  aNor   = 0.D0
  NB0    = 0.D0
  NB0_i  = 0.D0
  B0TNB0 = 0.D0
  hlp    = 0.D0

! Expansion for the Helmert parameters
! ------------------------------------
  DO ista=1, nsta

    x0 = neq%par(indsta(ista,1))%x0 / datum%aell
    y0 = neq%par(indsta(ista,2))%x0 / datum%aell
    z0 = neq%par(indsta(ista,3))%x0 / datum%aell

    R_hlm = RESHAPE(SOURCE=(/ 1.D0, 0.D0, 0.D0,     &
                              0.D0, 1.D0, 0.D0,     &
                              0.D0, 0.D0, 1.D0,     &
                              0.D0,   z0,  -y0,     &
                               -z0, 0.D0,   x0,     &
                                y0,  -x0, 0.D0,     &
                                x0,   y0,  z0  /), SHAPE=(/ 3, 7 /) )

   ! Build B0-matrix (only for requested Helmert parameters)
   !--------------------------------------------------------
    nhelm = 0
    DO ii=1,7
      IF ( hlmFil%ihelm(ii,ifil) == 2 ) THEN
         nhelm = nhelm + 1
         DO jj=1,3
            B0(jj,nhelm) = R_hlm(jj,ii)
         END DO
      END IF
    END DO

   ! Compute matrix NB0 = N * B0
   !----------------------------
    DO ipar=1,neq%misc%npar
      DO jj=1,3
        aNor(ipar,jj) = neq%aNor( ikf(ipar, indsta(ista,jj)) )
      END DO
    END DO

    NB0 = NB0 +  MATMUL(aNor, B0)

  END DO    ! ista

! Compute matrix B0TNB0 = B0_T * N * B0
!--------------------------------------
  DO ista=1,nsta
    x0 = neq%par(indsta(ista,1))%x0 / datum%aell
    y0 = neq%par(indsta(ista,2))%x0 / datum%aell
    z0 = neq%par(indsta(ista,3))%x0 / datum%aell

    R_hlm = RESHAPE(SOURCE=(/ 1.D0,  0.D0,  0.D0,     &
                              0.D0,  1.D0,  0.D0,     &
                              0.D0,  0.D0,  1.D0,     &
                              0.D0,    z0,   -y0,     &
                               -z0,  0.D0,    x0,     &
                                y0,   -x0,  0.D0,     &
                                x0,    y0,    z0  /), SHAPE=(/ 3, 7 /) )
    nhelm = 0
    DO ii=1,7
      IF ( hlmFil%ihelm(ii,ifil) == 2 ) THEN
         nhelm = nhelm + 1
         DO jj=1,3
           B0(jj,nhelm)    = R_hlm(jj,ii)
           NB0_i(jj,nhelm) = NB0(indsta(ista,jj), nhelm)
          END DO
       END IF
    END DO

    B0TNB0 = B0TNB0 + MATMUL( TRANSPOSE(B0), NB0_i )

   ! Compute right hand side of normal equation
   !-------------------------------------------
    DO ii=1,3
      bNor(ii) = neq%bNor(indsta(ista,ii))
    END DO
    hlp = hlp + MATMUL( TRANSPOSE(B0), bNor )

  END DO


! Find index of first Helmert parameter
! -------------------------------------
  DO ihelm=1, neq%misc%npar
    IF ( neq%par(ihelm)%locq(1) == 28 ) EXIT
  END DO

! Update NEQ-System
! -----------------
  nhelm = 0
  DO ii=1,7
    IF ( hlmFil%ihelm(ii,ifil) == 2 ) THEN

       nhelm = nhelm + 1

       neq%par(ihelm)%locq(1)   = 28
       neq%par(ihelm)%locq(2)   = ii
       neq%par(ihelm)%locq(3)   = hlmFil%indgrp(ifil)
       neq%par(ihelm)%locq(4)   = 0
       neq%par(ihelm)%locq(5)   = 0
       neq%par(ihelm)%locq(6)   = 0
       neq%par(ihelm)%locq(7)   = 0

       neq%par(ihelm)%type      = parType_constant

       neq%par(ihelm)%name      = hlmFil%grpnam(ifil)

       neq%par(ihelm)%x0        = 0.D0

       neq%par(ihelm)%scale     = 1.D0
       neq%par(ihelm)%time%mean = (comstat%taecml(2,1,ifil) + &
                                   comstat%taecml(1,1,ifil))/2
       neq%par(ihelm)%time%half = (comstat%taecml(2,1,ifil) - &
                                   comstat%taecml(1,1,ifil))/2

       neq%bNor(ihelm) = hlp(nhelm)

  ! Off-diagonal block matrix
       DO ipar=1,neq%misc%npar
         IF ( neq%par(ipar)%locq(1) .NE. 28 ) THEN
            neq%aNor(ikf(ipar, ihelm)) = NB0(ipar,nhelm)
         END IF
       END DO

  ! Main-diagonal block matrix
       DO jj=nhelm, SIZE(hlp)
         neq%aNor(ikf(ihelm,ihelm-nhelm+jj)) = B0TNB0(nhelm,jj)
       END DO

       ihelm = ihelm + 1      ! index of next Helmert parameter

    END IF

  END DO

! Update number of parameters
! ---------------------------
  neq%misc%npar   = neq%misc%npar   + nhelm
  neq%misc%nparms = neq%misc%nparms + nhelm


  DEALLOCATE(hlp,    stat=iac)
  DEALLOCATE(B0TNB0, stat=iac)
  DEALLOCATE(NB0_i,  stat=iac)
  DEALLOCATE(NB0,    stat=iac)
  DEALLOCATE(aNor,   stat=iac)
  DEALLOCATE(B0,     stat=iac)
  DEALLOCATE(indsta, stat=iac)


END SUBROUTINE nqaddhlm

END MODULE
