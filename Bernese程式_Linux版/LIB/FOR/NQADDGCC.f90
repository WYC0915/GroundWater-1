MODULE s_NQADDGCC
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE nqaddgcc(neq,ifil)

! -------------------------------------------------------------------------
! Purpose:    This subroutine expands the NEQ system to include
!             geocenter coordinate parameters.
!
! Author:     U. Hugentobler
!
! Created:    03-Dec-2001
! Last mod.:  16-Nov-2010
!
! Changes:    22-Sep-2005 RD: Use new module D_NEQ.f90
!             16-Nov-2010 RD: Distinguish between parameter types
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_neq,    ONLY: t_neq
  USE d_par,    ONLY: parType_constant
  USE p_addneq, ONLY: comstat, opt

  USE f_ikf
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)   :: neq
  INTEGER(i4b)  :: ifil

! Local Variables
! ---------------
  INTEGER(i4b)  :: ii,jj,kk,ll
  INTEGER(i4b)  :: igcc,lgcc
  REAL(r8b)     :: scacen=1D0

  IF (opt%setupgcc == 0) RETURN

! Check whether geocenter coordinates already present
! ---------------------------------------------------
  igcc=0
  DO ii = 1, neq%misc%npar
    IF (neq%par(ii)%locq(1) == 16) igcc=1
  ENDDO

  IF (igcc == 0) THEN
! Initialize matrices
! -------------------
    DO ii=1,3
      igcc = neq%misc%npar+ii
      neq%bNor(igcc) = 0D0
      DO lgcc=1,neq%misc%npar+3
        neq%aNor(ikf(igcc,lgcc)) = 0D0
      ENDDO
    ENDDO

    DO ii=1,3
      igcc = neq%misc%npar+ii

! Setup LOCQ
! ----------
      neq%par(igcc)%locq(1)   = 16
      neq%par(igcc)%locq(2)   = ii
      neq%par(igcc)%locq(3)   =  0
      neq%par(igcc)%locq(4)   =  0
      neq%par(igcc)%locq(5)   =  0
      neq%par(igcc)%locq(6)   =  0
      neq%par(igcc)%locq(7)   = ii

      neq%par(igcc)%type      = parType_constant

      neq%par(igcc)%x0        = 0D0
      neq%par(igcc)%scale     = scacen

      neq%par(igcc)%time%mean = (comstat%taecml(2,1,ifil) + &
                                 comstat%taecml(1,1,ifil))/2
      neq%par(igcc)%time%half = (comstat%taecml(2,1,ifil) - &
                                 comstat%taecml(1,1,ifil))/2

! Update matrices
! ---------------
      DO jj=1,neq%misc%npar
        IF (neq%par(jj)%locq(1) == 1 .AND. neq%par(jj)%locq(3) == ii) THEN

! b-matrix
          neq%bNor(igcc) =  neq%bNor(igcc) - neq%bNor(jj)

          DO kk=1,neq%misc%npar

! NEQ offdiagonal submatrix
            neq%aNor(ikf(igcc,kk)) = neq%aNor(ikf(igcc,kk)) - &
                                     neq%aNor(ikf(jj,kk))

! NEQ diagonal submatrix
            DO ll = ii,3
              IF (neq%par(kk)%locq(1) == 1 .AND. &
                  neq%par(kk)%locq(3) == ll) THEN
                lgcc = neq%misc%npar+ll
                neq%aNor(ikf(igcc,lgcc)) = neq%aNor(ikf(igcc,lgcc)) + &
                                           neq%aNor(ikf(jj,kk))

              ENDIF
            ENDDO
          ENDDO

        ENDIF
      ENDDO

    ENDDO

! Update nuber of parameters
! --------------------------
    neq%misc%npar   = neq%misc%npar   + 3
    neq%misc%nparms = neq%misc%nparms + 3

  ENDIF

END SUBROUTINE nqaddgcc


END MODULE
