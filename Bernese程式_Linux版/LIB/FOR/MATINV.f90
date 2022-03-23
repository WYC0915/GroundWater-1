MODULE f_matinv
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION matinv(aa,nn)

! -------------------------------------------------------------------------
! Purpose:    This function computes the matrix inversion using
!             Gauss-Jordan elimination. It returns 1 if matrix is singular
!             and 0 otherwise.
!
! Author:     L. Mervart
!
! Created:    22-SEP-1998
! Last mod.:  __-___-____
!
! Changes:    __-___-____ __:
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  IMPLICIT NONE

! Return value
! ------------
  INTEGER(i4b)                :: matinv

! List of Parameters
! ------------------
  INTEGER(i4b)                :: nn
  REAL(r8b), DIMENSION(nn,nn) :: aa

! Local Variables
! ---------------
  REAL(r8b)                   ::  big,dum,pivinv

  INTEGER(i4b)                :: ii,icol,irow,jj,kk,ll,mm
  INTEGER(i4b), DIMENSION(nn) :: indxc
  INTEGER(i4b), DIMENSION(nn) :: indxr
  INTEGER(i4b), DIMENSION(nn) :: ipiv

  ipiv(:) = 0
  matinv  = 0

  DO ii = 1, nn
    big = 0.d0

    DO jj = 1, nn
      IF (ipiv(jj) /= 1) THEN
        DO kk = 1, nn
          IF (ipiv(kk) == 0) THEN
            IF ( ABS(aa(jj,kk)) >= big) THEN
              big  = ABS(aa(jj,kk))
              irow = jj
              icol = kk
            END IF
          ELSE IF (ipiv(kk) > 1) THEN
            matinv = 1
            RETURN
          END IF
        END DO
      END IF
    END DO

    ipiv(icol) = ipiv(icol)+1
    IF (irow /= icol) THEN
      DO ll = 1, nn
        dum = aa(irow,ll)
        aa(irow,ll) = aa(icol,ll)
        aa(icol,ll) = dum
      END DO
    END IF
    indxr(ii) = irow
    indxc(ii) = icol
    IF (ABS(aa(icol,icol)) < 1.d-10) THEN
      matinv = 1
      RETURN
    END IF

    pivinv        = 1.d0 / aa(icol,icol)
    aa(icol,icol) = 1.d0
    aa(icol,:)    = aa(icol,:)*pivinv

    DO mm = 1, nn
      IF (mm /= icol) THEN
        dum = aa(mm,icol)
        aa(mm,icol) = 0.d0
        aa(mm,:) = aa(mm,:) - aa(icol,:) * dum
      END IF
    END DO
  END DO

  DO ll = nn, 1, -1
    IF (indxr(ll) /= indxc(ll)) THEN
      DO kk = 1,nn
        dum = aa(kk,indxr(ll))
        aa(kk,indxr(ll)) = aa(kk,indxc(ll))
        aa(kk,indxc(ll)) = dum
      END DO
    END IF
  END DO

END FUNCTION matinv

END MODULE
