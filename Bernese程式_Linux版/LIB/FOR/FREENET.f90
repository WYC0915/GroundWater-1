MODULE s_FREENET
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE freenet(neq, flglq4)

! -------------------------------------------------------------------------
! Purpose:    This subroutine adds the free network restriction to the
!             normal equation system
!
! Author:     L. Mervart
!
! Created:    12-Dec-2005
!
! Changes:    13-Aug-2099 JJ: Add variables one and zero for IRIX
!             26-Jun-2001 RD: Use alcerr for allocation
!             25-Sep-2001 LM: BTB pseudoinversion
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             08-Feb-2002 MM: New gtweight call
!             27-Feb-2003 HU: DATUM from D_DATUM
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             12-Dec-2005 CU: Count number of pseudo-observations npseu
!             03-Feb-2011 MM: GSPs added
!             05-Feb-2011 RD: Source code cosmetics
!             20-Jul-2012 RD: Use corrected types for number in formulas
!             06-Dec-2012 RD: Decompose nested matrix operations for PG_F90
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b
  USE d_datum,  ONLY: datum
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: opt

  USE f_ikf
  USE s_alcerr
  USE f_gtweight
  USE f_matinv
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)  :: neq
  INTEGER(i4b) :: flglq4

! Local Variables
! ---------------
#ifdef CMP_PG_F90
  REAL(r8b), DIMENSION(7,3)       :: Bhlp
  REAL(r8b), DIMENSION(7,3)       :: Phlp
  REAL(r8b), DIMENSION(7,7)       :: hlp
#endif

  REAL(r8b), DIMENSION(3,7)       :: Bmat1
  REAL(r8b), DIMENSION(3,7)       :: Bmat2
  REAL(r8b), DIMENSION(7,7)       :: Pmat
  REAL(r8b), DIMENSION(7,7)       :: BTB
  REAL(r8b), DIMENSION(3,3)       :: regMat
  REAL(r8b)                       :: X1, Y1, Z1, X2, Y2, Z2, one, zero

  INTEGER(i4b), DIMENSION(:), ALLOCATABLE :: indsta, indTra
  INTEGER(i4b)                            :: nstat, ista1, ista2
  INTEGER(i4b)                            :: ipar, ipar1, ipar2
  INTEGER(i4b)                            :: ip1, ip2, i1, i2, jp1,jp2
  INTEGER(i4b)                            :: ii, jj
  INTEGER(i4b)                            :: iac

! Count number of stations
! ------------------------
  nstat = 0

  IF (flglq4 == 1 .OR. flglq4 == 2) THEN
    DO ipar = 1, neq%misc%npar
      IF ( neq%par(ipar)%locq(1) == 1        .AND. &
           neq%par(ipar)%locq(3) == 1        .AND. &
           neq%par(ipar)%locq(4) == flglq4   .AND. &
           gtweight(neq%par(ipar),'A') /= 0.d0 ) THEN
        nstat = nstat + 1
      END IF
    END DO
  ELSEIF (flglq4 == 3) THEN
    DO ipar = 1, neq%misc%npar
      IF ( neq%par(ipar)%locq(1) == 30       .AND. &
           neq%par(ipar)%locq(3) == 1        .AND. &
           gtweight(neq%par(ipar),'A') /= 0.d0 ) THEN
        nstat = nstat + 1
      END IF
    END DO
  ENDIF


! Create station index
! --------------------
  ALLOCATE( indsta(nstat), stat=iac )
  CALL alcerr(iac, 'indsta', (/nstat/), 'freenet')
  ALLOCATE( indtra(nstat), stat=iac )
  CALL alcerr(iac, 'indtra', (/nstat/), 'freenet')
  nstat  = 0
  indSta = 0
  indTra = 0

! Coordinates/velocities
  IF (flglq4 == 1 .OR. flglq4 == 2) THEN
    DO ipar = 1, neq%misc%npar
      IF ( neq%par(ipar)%locq(1) == 1        .AND. &
           neq%par(ipar)%locq(3) == 1        .AND. &
           neq%par(ipar)%locq(4) == flglq4   .AND. &
           gtweight(neq%par(ipar),'A') /= 0.d0 ) THEN
        nstat = nstat + 1
        indsta(nstat) = ipar
      ENDIF
    ENDDO

! GNSS-specific translations
  ELSEIF (flglq4 == 3) THEN
    DO ipar = 1, neq%misc%npar
      IF ( neq%par(ipar)%locq(1) == 30       .AND. &
           neq%par(ipar)%locq(3) == 1        .AND. &
           gtweight(neq%par(ipar),'A') /= 0.d0 ) THEN
        nstat = nstat + 1
        indtra(nstat) = ipar

        ! Find corresponding coordinate
        DO ii=1,neq%misc%nPar
          IF (neq%par(ii)%name    == neq%par(ipar)%name .AND. &
              neq%par(ii)%locq(1) == 1                  .AND. &
              neq%par(ii)%locq(3) == 1                  .AND. &
              neq%par(ii)%locq(4) == 1                       )THEN
            indsta(nStat) = ii
          ENDIF
        ENDDO

        ! No station found
        IF (indsta(nStat) == 0) THEN
          indtra(nstat) = 0
          nStat = nStat-1
        ENDIF

      ENDIF
    ENDDO
  END IF


! Create the (B'B) Matrix
! -----------------------
  BTB = 0.d0
  DO ista1 = 1, nstat
    ip1 = indsta(ista1)
    X1  = neq%par(ip1  )%x0 / datum%aell
    Y1  = neq%par(ip1+1)%x0 / datum%aell
    Z1  = neq%par(ip1+2)%x0 / datum%aell

    zero = 0.d0
    one  = 1.d0
    Bmat1 = RESHAPE(SOURCE=(/ one , zero, zero,     &
                              zero, one , zero,     &
                              zero, zero, one ,     &
                              zero,   Z1,  -Y1,     &
                               -Z1, zero,   X1,     &
                                Y1,  -X1, zero,     &
                                X1,   Y1,   Z1  /), SHAPE=(/ 3, 7 /) )
#ifdef CMP_PG_F90
    Bhlp = TRANSPOSE(Bmat1)
    hlp  = MATMUL( Bhlp , Bmat1 )
    BTB  = BTB + hlp
#else
    BTB = BTB + MATMUL( TRANSPOSE(Bmat1), Bmat1 )
#endif
  END DO !ista1


! Compute the weight matrix (B'B)^-1 * P * (B'B)^1
! ------------------------------------------------
  Pmat      = 0.d0

  DO ii = 1, 7
    IF (opt%helmSig(flglq4, ii) /= 0.d0) THEN
      Pmat(ii,ii) = ( opt%sigma0 / opt%helmSig(flglq4, ii) )**2
    ! Count pseudo-observations
      neq%misc%npseu = neq%misc%npseu + 1
    ELSE
      DO jj = 1, 7
        BTB(ii,jj) = 0.d0
        BTB(jj,ii) = 0.d0
      END DO
      BTB(ii,ii) = 1.d0
    END IF
  END DO

  IF ( matinv(BTB, 7) == 0.d0 ) THEN
    DO ii = 1, 7
      IF (opt%helmSig(flglq4, ii) == 0.d0) THEN
        BTB(ii,ii) = 0.d0
      END IF
    END DO
#ifdef CMP_PG_F90
    hlp  = MATMUL( Pmat, BTB )
    Pmat = MATMUL( BTB, hlp )
#else
    Pmat = MATMUL( BTB, MATMUL( Pmat, BTB ) )
#endif
  END IF


! Create the regularization matrix and add the weights
! ----------------------------------------------------
  DO ista1 = 1, nstat
    ip1 = indsta(ista1)
    jp1 = indTra(iSta1)
    X1  = neq%par(ip1  )%x0 / datum%aell
    Y1  = neq%par(ip1+1)%x0 / datum%aell
    Z1  = neq%par(ip1+2)%x0 / datum%aell

    Bmat1 = RESHAPE(SOURCE=(/ one , zero, zero,     &
                              zero, one , zero,     &
                              zero, zero, one ,     &
                              zero,   Z1,  -Y1,     &
                               -Z1, zero,   X1,     &
                                Y1,  -X1, zero,     &
                                X1,   Y1,   Z1  /), SHAPE=(/ 3, 7 /) )

    DO ista2 = 1, nstat
      ip2 = indsta(ista2)
      jp2 = indTra(iSta2)
      X2  = neq%par(ip2  )%x0 / datum%aell
      Y2  = neq%par(ip2+1)%x0 / datum%aell
      Z2  = neq%par(ip2+2)%x0 / datum%aell

      Bmat2 = RESHAPE(SOURCE=(/ one , zero, zero,     &
                                zero, one , zero,     &
                                zero, zero, one ,     &
                                zero,   Z2,  -Y2,     &
                                 -Z2, zero,   X2,     &
                                  Y2,  -X2, zero,     &
                                  X2,   Y2,   Z2  /), SHAPE=(/ 3, 7 /) )

#ifdef CMP_PG_F90
      Bhlp   = TRANSPOSE(Bmat2)
      Phlp   = MATMUL( Pmat, Bhlp )
      regMat = MATMUL( Bmat1, Phlp )
#else
      regMat = MATMUL( Bmat1, MATMUL( Pmat, TRANSPOSE(Bmat2) ) )
#endif

! Appply constraints for coordinates/velocities
      IF (flglq4 == 1 .OR. flglq4 == 2) THEN
        DO i1 = 1, 3
          ipar1 = ip1+i1-1
          DO i2 = 1, 3
            ipar2 = ip2+i2-1
            IF (ipar2 < ipar1) CYCLE
            neq%aNor(ikf(ipar1,ipar2)) = neq%aNor(ikf(ipar1,ipar2)) + &
                                         regMat(i1,i2)
          END DO
        END DO
      ENDIF

! Appply constraints for GNSS-specific translations
      IF (flglq4 == 3) THEN
        DO i1=1,3
          ipar1 = jp1+i1-1
          DO i2=1,3
            ipar2 = jp2+i2-1
            IF (ipar2 < ipar1) CYCLE
            neq%aNor(ikf(ipar1,ipar2)) = neq%aNor(ikf(ipar1,ipar2)) + &
                                         regMat(i1,i2)
          END DO
        END DO
      ENDIF

    END DO
  END DO

  DEALLOCATE( indsta, stat=iac )
  DEALLOCATE( indtra, stat=iac )

END SUBROUTINE freenet

END MODULE
