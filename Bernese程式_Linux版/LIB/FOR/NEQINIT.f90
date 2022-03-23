MODULE s_NEQINIT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE neqinit(neq)

! -------------------------------------------------------------------------
! Purpose:    This subroutine initializes the members of the NEQ structure
!             which are not allocatable.
!
! Author:     L. Mervart
!
! Created:    12-Dec-2005
!
! Changes:    21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: M_ADDNEQ replaced by p_addneq
!             18-May-2003 HU: Nullify pointer
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             12-Dec-2005 CU: Add npseu, npseuel
!             08-Feb-2007 RD: Misc%nObs/nParms i4b->r8b
!             25-Aug-2008 DT: Initialize neq%version=4 (instead of =1)
!             30-Apr-2009 SL: Neq%version set to 5
!             04-May-2009 RD: Scaling of loading models added, neq%version=6
!             08-Oct-2010 RD: Introduce undef_Trp
!             12-Oct-2010 RD: Init nsmpnq and ielvnq
!             09-Dec-2010 SL: Use s_exitrc added
!             13-Jul-2011 LP: Sat-spec. obstypes; neq%version set to 8
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, lfnerr
  USE d_neq,    ONLY: t_neq, maxObst
  USE d_grid,   ONLY: grdNeq
  USE d_trpest, ONLY: undef_Trp

  USE s_exitrc

  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq) :: neq

! Local Variables
! ---------------
  INTEGER(i4b)         :: ii
  INTEGER(i4b)         :: kk

  neq%version          =  8

  neq%misc%title       = ''
  neq%misc%orbFil      = ''
  neq%misc%npar        =  0
  neq%misc%nobs        =  0d0
  neq%misc%npseu       =  0
  neq%misc%npseuel     =  0
  neq%misc%lTPl        =  0d0
  neq%misc%nparms      =  0d0
  neq%misc%nftot       =  0
  neq%misc%nstat_sinex =  0
  neq%misc%itropo      =  undef_trp
  neq%misc%iextra      =  undef_trp
  neq%misc%itrmap      =  undef_trp
  neq%misc%itrgrd      =  undef_trp
  neq%misc%nsmpnq      =  0
  neq%misc%ielvnq      =  0
  neq%misc%nobst       =  0
!  neq%misc%obst(:)%sat =  0
  DO ii = 1,maxObst
!     neq%misc%obst(ii)%timint%t(:) = (/0d0,0d0/)
    DO kk = 1,4
       neq%misc%obst(ii)%obstyp(kk) =  '   '
    ENDDO
  ENDDO


! Grid keywords for Vienna grid files
! -----------------------------------
  IF ( SIZE(grdNeq)-1 /= SIZE(neq%misc%grdNeq) ) THEN
    WRITE(LFNERR,'(/,A,2(/,18X,A),/)')                            &
    ' *** SR NQRDHEAD: Scaling factors of the Vienna grid files.',&
    'the size of the keyword records differs between the modules',&
    '"${I}/D_NEQ.f90" AND "${I}/D_GRID.f90".'
    CALL EXITRC(2)
  ENDIF
  neq%misc%grdNeq = grdNeq(1:)

  NULLIFY( neq%par  )
  NULLIFY( neq%aNor )
  NULLIFY( neq%bNor )
  NULLIFY( neq%xxx  )

END SUBROUTINE neqinit

END MODULE
