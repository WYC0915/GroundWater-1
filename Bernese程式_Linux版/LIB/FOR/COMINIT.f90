MODULE s_COMINIT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE cominit(orb)

! -------------------------------------------------------------------------
! Purpose:    Initialize several common variables
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
! Last mod.:  27-Oct-2010
!
! Changes:    09-Mar-2000 LM: taecml initialized to zero
!             31-Jul-2000 LM: ititialize orb structure
!             26-Jun-2001 RD: use alcerr for allocation
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             08-Nov-2004 MF: Nullify pointers in 'comstat'
!             18-Apr-2005 RD: Consider maneuvers for stc at neq boundaries
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             26-Feb-2008 RD: Use GTSATM from D_SATCRX
!             24-Sep-2008 DT: Initialize opt%svndyn
!             27-Oct-2010 SL: use m_bern with ONLY
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b
  USE d_neq,    ONLY: maxStaSin
  USE d_satcrx, ONLY: gtsatm
  USE p_addneq, ONLY: t_orb,opt,comstat,maxCmp,maxman

  USE s_alcerr
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_orb)  :: orb   ! Orbit description structure (see P_ADDNEQ)

! Local Variables
! ---------------
  INTEGER(i4b) :: nfil
  INTEGER(i4b) :: iac

! Initialize the orb structure
! ----------------------------
  orb%title           = ''
  orb%nseff           = 0
  orb%svneff(:)       = 0
  orb%nstoch          = 0
  orb%arcnum(:,:)     = 0
  orb%manovr(:)       = 0
  orb%ele(:,:,:)      = 0.0
  orb%rpress(:,:,:)   = 0.0
  orb%drpr(:,:,:)     = 0.0
  orb%tosc(:)         = 0.0
  orb%tstoch(:)       = 0.0
  orb%kmat(:,:,:,:)   = 0.0
  orb%lmat(:,:,:,:)   = 0.0
  orb%mmat(:,:,:,:,:) = 0.0
  orb%drho(:,:,:)     = 0.0
  orb%drhot(:,:,:)    = 0.0

  opt%svndyn(:)       = 0

  CALL gtsatm(maxman,orb%nman,orb%man(:)%satman,orb%man(:)%timman)

! Initialize common statistics structure
! --------------------------------------
  comstat%rms = 0.002d0

  nfil = SIZE(opt%neqFileName)

  NULLIFY(comstat%taecml)
  NULLIFY(comstat%indfil)
  NULLIFY(comstat%datcre)
  NULLIFY(comstat%timcre)
  NULLIFY(comstat%nparl)
  NULLIFY(comstat%titind)

  ALLOCATE( comstat%taecml(2,maxCmp,nfil), stat=iac )
  CALL alcerr(iac, 'comstat%taecml', (/2,maxCmp,nfil/), 'cominit')
  ALLOCATE( comstat%indfil(maxStaSin,nfil), stat=iac )
  CALL alcerr(iac, 'comstat%indfil', (/maxStaSin,nfil/), 'cominit')
  ALLOCATE( comstat%datcre(nfil), stat=iac )
  CALL alcerr(iac, 'comstat%datcre', (/nfil/), 'cominit')
  ALLOCATE( comstat%timcre(nfil), stat=iac )
  CALL alcerr(iac, 'comstat%timcre', (/nfil/), 'cominit')
  ALLOCATE( comstat%nparl(nfil), stat=iac )
  CALL alcerr(iac, 'comstat%nparl', (/nfil/), 'cominit')
  ALLOCATE( comstat%titind(nfil), stat=iac )
  CALL alcerr(iac, 'comstat%titind', (/nfil/), 'cominit')

  comstat%taecml = 0.0
  comstat%nflsta = 0

END SUBROUTINE cominit


END MODULE
