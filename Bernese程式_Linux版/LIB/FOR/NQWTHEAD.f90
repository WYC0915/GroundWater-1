MODULE s_NQWTHEAD
CONTAINS

! -------------------------------------------------------------------------
! Bernese Software Version 5.2
! -------------------------------------------------------------------------

SUBROUTINE nqwthead(lfn,neq)

! -------------------------------------------------------------------------
! Purpose:    Write the header part of the NEQ structure into file
!
! Author:     L. Mervart
!
! Created:    12-Dec-2005
!
! Changes:    21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             08-Sep-2003 HU: New NEQ version
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             12-Dec-2005 CU: Add pre-eliminated pseudo-observations npseuel
!             08-Feb-2007 RD: misc%nObs/nParms i4b->r8b
!             26-Jun-2008 RD: System-specific PCV in SINEX record
!             04-May-2009 RD: Scaling of loading models added, neq%version=6
!             13-Jul-2011 LP: Sat-spec. obstypes; neq%version set to 8
!             21-Nov-2012 RD: Do not write SINEX-PCV as a record
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_global, ONLY: maxsys
  USE d_neq,    ONLY: t_neq

  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b)  :: lfn  ! logical file number
  TYPE(t_neq)   :: neq

! Local Variables
! ---------------
  INTEGER(i4b)  :: ii,jj
  INTEGER(i4b)  :: iSys

! Version of the neq-file
! -----------------------
  WRITE(lfn) neq%version

! Miscelaneous information
! ------------------------
  WRITE(lfn) neq%misc%title  , &
             neq%misc%orbFil , &
             neq%misc%nobs+DBLE(neq%misc%npseuel), &
             neq%misc%npar   , &
             neq%misc%lTPl   , &
             neq%misc%nparms , &
             neq%misc%nftot  , &
             neq%misc%nsmpnq , &
             neq%misc%ielvnq , &
             neq%misc%itropo , &
             neq%misc%iextra , &
             neq%misc%itrmap , &
             neq%misc%itrgrd

! Neq version 2
! -------------
  WRITE(lfn) neq%misc%datum  , &
             neq%misc%nutmod , &
             neq%misc%submod

! Neq version 4
! -------------
  WRITE(lfn) neq%misc%gravFil

! Satellite antenna offset arrays
! -------------------------------
  WRITE(lfn) neq%misc%nanoff

  DO ii = 1, neq%misc%nanoff
    WRITE(lfn) neq%misc%nsaoff(ii)
  END DO

  WRITE(lfn) ( (neq%misc%satoff(jj,ii),jj=1,neq%misc%nsaoff(ii)), &
                ii=1,neq%misc%nanoff )

! Information needed for SINEX format
! -----------------------------------
  WRITE(lfn) neq%misc%nstat_sinex

  DO ii = 1, neq%misc%nstat_sinex
    WRITE(lfn) neq%misc%sinex(ii)%timint,  neq%misc%sinex(ii)%stname,  &
               neq%misc%sinex(ii)%antecc,  neq%misc%sinex(ii)%antnum,  &
               neq%misc%sinex(ii)%antsta,  neq%misc%sinex(ii)%antrec,  &
               -99,-99
    WRITE(lfn) 0,maxsys-1
    DO iSys = 0,maxsys-1
      WRITE(lfn) neq%misc%sinex(ii)%antpcv(iSys)%nFrq,    &
                 neq%misc%sinex(ii)%antpcv(iSys)%antphs,  &
                 neq%misc%sinex(ii)%antpcv(iSys)%adopted, &
                 neq%misc%sinex(ii)%antpcv(iSys)%individ, &
                 neq%misc%sinex(ii)%antpcv(iSys)%atxStr
    ENDDO
  END DO

! Keywords of for the scaling factors of the Vienna grid files
! ------------------------------------------------------------
  WRITE(lfn) SIZE(neq%misc%grdNeq),LEN(neq%misc%grdNeq(1))
  WRITE(lfn) neq%misc%grdNeq(:)

! Neq version 8
! -------------
  WRITE(lfn) neq%misc%nobst
  DO ii = 1, neq%misc%nobst
!    WRITE(lfn) neq%misc%obst(ii)%sat
    DO jj = 1,4
      WRITE(lfn) neq%misc%obst(ii)%obstyp(jj)
    ENDDO
!    DO jj = 1,2
!      WRITE(lfn) neq%misc%obst(ii)%timint%t(jj)
!    ENDDO
  ENDDO

END SUBROUTINE nqwthead

END MODULE
