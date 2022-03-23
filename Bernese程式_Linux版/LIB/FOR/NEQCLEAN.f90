MODULE s_NEQCLEAN
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE neqclean(neq)

! -------------------------------------------------------------------------
! Purpose:    This subroutine cleans the NEQ system. It eliminates
!             parameters with no observations and those with locq(1)=0
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
!
! Changes:    21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             08-Oct-2002 CU: Remove elimination of parameters with no
!                             observations (already done by SR neqDelNo)
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             30-Oct-2009 SL: Clean bNor and aNor added
!             03-Mar-2010 RD: Clean SINEX record from eliminated stations
!             08-Oct-2010 RD: Init misc-tropo-parameters if no TRP-param in neq
!             24-Oct-2010 RD: Recover the parameter type flag
!             29-Oct-2010 RD: Shift misc-tropo-parameters section
!             14-Jun-2012 RD: Handle also empty NEQs (npar == 0)
!             14-Jun-2012 RD: Use m_bern with ONLY
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b
  USE d_neq,    ONLY: t_neq
  USE d_par,    ONLY: maxParTyp
  USE d_trpest, ONLY: undef_Trp

  USE s_dmatrd
  USE s_gtpartyp
  USE f_ikf
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq) :: neq  ! NEQ structure (see P_ADDNEQ)

! Local Variables
! ---------------
  INTEGER(i4b)                          :: npnew,indnew,ipar
  INTEGER(i4b)                          :: iSnx, jSnx
  INTEGER(i4b),DIMENSION(neq%misc%npar) :: idel
  INTEGER(i4b),DIMENSION(maxParTyp)     :: numPar


! Init variables
! --------------
  numPar(:) = 0

! Loop all parameters
! -------------------
  DO ipar = 1, neq%misc%npar
    IF (neq%par(ipar)%locq(1) == 0) THEN
      idel(ipar) = 1
    ELSE
      idel(ipar) = 0
      numPar(neq%par(ipar)%locq(1)) = numPar(neq%par(ipar)%locq(1)) + 1
    END IF
  ENDDO

! Remove troposphere information from the header
! if no troposphere parameters remain in the NEQ
! ----------------------------------------------
  IF (numPar(6) == 0) THEN
    neq%misc%itropo = undef_trp
    neq%misc%iextra = undef_trp
    neq%misc%itrmap = undef_trp
    neq%misc%itrgrd = undef_trp
  ENDIF

  IF ( MAXVAL(idel) == 0 .OR. neq%misc%npar == 0 ) RETURN

  npnew  = neq%misc%npar
  indNew = 0
  DO ipar = 1, neq%misc%npar
    IF ( idel(ipar) == 1 ) THEN
      npnew = npnew - 1
    ELSE
      indNew = indNew + 1
      neq%par(indNew)  = neq%par(ipar)
      neq%bNor(indNew) = neq%bNor(ipar)
    END IF
  END DO

  CALL dmatrd('S',neq%misc%npar,neq%misc%npar,idel, &
              neq%anor(:),npnew,npnew)

  neq%bNor(npnew+1:neq%misc%npar) = 0d0
  DO ipar = ikf(npnew,npnew)+1,ikf(neq%misc%npar,neq%misc%npar)
    neq%anor(ipar) = 0d0
  ENDDO

  neq%misc%npar = npnew

! Update the SINEX record
! -----------------------
  DO iSnx = 1,neq%misc%nstat_sinex
    jSnx = 0
    DO iPar = 1,neq%misc%npar
      IF (neq%misc%sinex(iSnx)%stName == neq%par(iPar)%name) THEN
        jSnx = iPar
        EXIT
      ENDIF
    ENDDO
    IF (jSnx /= 0) CYCLE

    neq%misc%sinex(iSnx)%stName = ''
  ENDDO

  jSnx = 0
  DO iSnx = 1,neq%misc%nstat_sinex
    IF (neq%misc%sinex(iSnx)%stName == '') CYCLE

    jSnx = jSnx + 1

    IF (iSnx /= jSnx) THEN
      neq%misc%sinex(jSnx) = neq%misc%sinex(iSnx)
    ENDIF
  ENDDO

  neq%misc%nstat_sinex = jSnx

! Recover the parameter type description
! --------------------------------------
  CALL gtpartyp(neq)

END SUBROUTINE neqclean


END MODULE
