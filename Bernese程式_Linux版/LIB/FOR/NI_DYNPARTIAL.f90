MODULE s_ni_dynpartial
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE ni_dynpartial(act_sat, iter,qvar,stpvar,nshad,tshad,tmjd,deledp,satpos, stdorb)

! -------------------------------------------------------------------------
! Purpose: This subroutine calculates the coefficients alpha_i(tmjd) for all
!          partial derivatives of the orbit w.r.t. the dynamical parameters
!          at time t=tmjd and stores them into the array deledp(i,j), i=1,2,
!          3,4,5,6, j=1,2,...,m_nvar.
!          The partial derivative of the orbit r(t) w.r.t. a dynamical
!          parameter p is calculated according to the formula
!
!          dr/dp (t) = sum_(i=1,6) [ alpha_i(t) * dr/dE_i(t) ]
!
!          where :     E_i are the six initial osculating elements
!                      alpha_i(t) are coefficients
!
!          The subroutine numerically integrates all variational equations
!          (if the requested time is not already in the currently active
!          subinterval)
!
!          Theory: Beutler, CM, Springer 2005, Vol. 1, Chapter 8.5 (pulses)
!                                        and   Vol. 1, Chapter 5.2 (accel)
!
!
!
! Parameters : act_sat: active satellite index
!              iter   : current iteration step
!              qvar   : integration order
!              stpvar : step size for integration (hours)
!              nshad  : number of shadow periods
!              tshad  : shadow entris, exits
!              tmjd   : requested time
!              deledp : array of coefficients
!
! Author:     Gerhard Beutler
!
! Created:    21-Sep-2007
!
! Changes:    21-Sep-2007 GB: ---
!             01-Oct-2007 AJ: use act_sat
!             20-Sep-2012 RD: Use modules with ONLY
!             20-Sep-2012 RD: Use alcerr when allocating arrays
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,    ONLY: i4b, r8b
  USE d_const,   ONLY:
  USE p_gravdet, ONLY: m_nvar, m_iarc, m_locq, m_fromto

  USE s_alcerr
  USE s_nquadr
  USE s_partev
  USE s_polevn
  USE s_stdorbit_t, ONLY: t_satPos, t_stdOrb

  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b)                              :: act_sat ! active satellite (index)
  INTEGER(i4b)                              :: iter   ! iteration number
  INTEGER(i4b)                              :: qvar   ! integration order
  INTEGER(i4b)                              :: nshad  !
  REAL(r8b)                                 :: stpvar ! step size for integration
  REAL(r8b)                                 :: tmjd   ! resulting array of coefficients
  REAL(r8b), DIMENSION(:,:)                 :: tshad  ! shadow epochs
  REAL(r8b), DIMENSION(:,:)                 :: deledp ! resulting array of coefficients
  TYPE(t_satPos),     DIMENSION(:)          :: satPos
  TYPE(t_stdOrb),     DIMENSION(:)          :: stdOrb ! standard orbit description

! Local Variables
! ---------------
  INTEGER(i4b), DIMENSION(2),           SAVE :: inter_prev = 0
  INTEGER(i4b)                               :: inter
  INTEGER(i4b), DIMENSION(2),           SAVE :: inter_act
  INTEGER(i4b),                         SAVE :: inter_ini
  INTEGER(i4b), DIMENSION(2),           SAVE :: itold = 0
  INTEGER(i4b),                         SAVE :: iarco = 0
  INTEGER(i4b)                               :: iret, kk, ivar
  INTEGER(i4b)                               :: iac
  REAL(r8b), DIMENSION(2),              SAVE :: hvar
  REAL(r8b), DIMENSION(2),              SAVE :: t0var
  REAL(r8b), DIMENSION(:,:), ALLOCATABLE, SAVE :: coevar ! coefficients of numerical integration
  REAL(r8b), DIMENSION(2),                SAVE :: ta, tb

  REAL(r8b), DIMENSION(6*m_nvar,2)            :: var0
  REAL(r8b), DIMENSION(6*m_nvar)              :: deledp_int ! aux array

  INTERFACE
  SUBROUTINE integrand(tmjd, rhs, satpos, stdorb, act_sat)
    USE m_bern,       ONLY: i4b, r8b
    USE s_stdorbit_t, ONLY: t_stdOrb, t_satPos
    REAL(r8b)                                             :: tmjd
    REAL(r8b)                                             :: rhs(*)
    TYPE(t_stdOrb), DIMENSION(:),                OPTIONAL :: stdOrb
    TYPE(t_satPos), DIMENSION(:), INTENT(INOUT), OPTIONAL :: satPos
    INTEGER(i4b),                                OPTIONAL :: act_sat
  END SUBROUTINE
  END INTERFACE

  IF (inter_prev(act_sat) == 0 .AND. .NOT.ALLOCATED(coevar)) THEN
    ALLOCATE(coevar(6*m_nvar*(qvar+1),2),stat=iac)
    CALL alcerr(iac, 'coevar',(/6*m_nvar*(qvar+1),2/),'ni_dynpartial')
  ELSE IF (SIZE(coevar, dim=1) /= 6*m_nvar*(qvar+1)) THEN
    DEALLOCATE(coevar,stat=iac)
    ALLOCATE(coevar(6*m_nvar*(qvar+1),2),stat=iac)
    CALL alcerr(iac, 'coevar',(/6*m_nvar*(qvar+1),2/),'ni_dynpartial')
  ENDIF
!
  inter_act(act_sat)=(tmjd-m_fromto(1))/(stpvar/24.d0)
  inter_act(act_sat)=inter_act(act_sat)+1
  if(iter /= itold(act_sat).or.m_iarc /= iarco .or. inter_act(act_sat) < inter_prev(act_sat))then
    iarco=m_iarc
    itold(act_sat)=iter
!
! initialize solution of variational equations
    inter_ini=1
    ta(act_sat)=m_fromto(1)
    tb(act_sat)=m_fromto(1)+stpvar/24
    IF(tb(act_sat) > m_fromto(2)+10.d0/86400)tb(act_sat)=m_fromto(2)
    var0(:,act_sat) = 0.d0
    CALL NQUADR(6*m_nvar,QVAR,integrand,TA(act_sat),TB(act_sat),VAR0(:,act_sat), &
                DELEDP_int,T0VAR(act_sat),HVAR(act_sat),coevar(:,act_sat),IRET, satpos, stdorb, act_sat)
  elseif(inter_act(act_sat) >= inter_prev(act_sat))then
    inter_ini=inter_prev(act_sat)
  endif
!
! integrate to interval inter_act
  if (inter_act(act_sat) > inter_ini) then
    do inter=inter_ini+1,inter_act(act_sat)
      CALL PARTEV(m_nvar,m_iarc,TB(act_sat),TA(act_sat),TB(act_sat),QVAR,HVAR(act_sat),T0VAR(act_sat), &
                  coevar(:,act_sat),NSHAD,TSHAD,m_locq,VAR0(:,act_sat))
      TA(act_sat)=TB(act_sat)
      TB(act_sat)=TA(act_sat)+STPVAR/24
      IF(TB(act_sat).GT.m_fromto(2))TB(act_sat)=m_fromto(2)
      CALL NQUADR(6*m_nvar,QVAR,integrand,TA(act_sat),TB(act_sat),VAR0(:,act_sat), &
                  DELEDP_int,T0VAR(act_sat),HVAR(act_sat),coevar(:,act_sat),IRET, satpos, stdorb, act_sat)
    enddo
  endif
!
! evaluate integral at tmjd
!!!  write(lfnprt,*)'act_sat,tmjd,hvar=',act_sat,tmjd,hvar(act_sat)
  deledp_int = 0.d0
  CALL POLEVN(1,QVAR,6*m_nvar,tmjd-t0var(act_sat),HVAR(act_sat),coeVAR(:,act_sat),DELEDP_int)

!
  do ivar=1,m_nvar
    do kk=1,6
      deledp(kk,ivar)=deledp_int(kk+6*(ivar-1))
    enddo
  enddo
!
! set index for next call

  inter_prev(act_sat) = inter_act(act_sat)

END SUBROUTINE ni_dynpartial

END MODULE
