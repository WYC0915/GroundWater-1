MODULE s_NEQSTORE
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE neqstore(fileName, neq)

! -------------------------------------------------------------------------
! Purpose:    This subroutine stores the NEQ system into the file
!
! Author:     L.Mervart
!
! Created:    22-Nov-1997
!
! Changes:    21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             29-Dec-2001 HU: Interface to nqwthead added
!             08-Sep-2003 HU: NEQ version 2
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             23-Jan-2008 RD: New NEQ-version (with name = chr*20)
!             13-Jun-2008 RD: Use writePar from D_PAR.f90
!             30-Apr-2009 SL: neq%version set to 5
!             04-May-2009 RD: Scaling of loading models added, neq%version=6
!             15-Nov-2010 RD: New NEQ version=7: D_PAR omega is written anytime
!             13-Jul-2011 LP: Sat-spec. obstypes; neq%version set to 8
!             13-Dec-2011 SL: m_bern with ONLY
!             14-Jun-2012 RD: Handle also empty NEQs (npar == 0)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, lfnLoc, lfnErr
  USE d_par,    ONLY: writePar
  USE d_neq,    ONLY: t_neq, writeRec
  USE s_opnfil
  USE s_opnerr
  USE s_nqwthead
  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=*) :: fileName
  TYPE(t_neq)      :: neq

! Local Variables
! ---------------
  INTEGER(i4b) :: ios
  INTEGER(i4b) :: ii, neqDim

! Write the entire NEQ system
! ---------------------------
  IF(fileName == '') RETURN

  neq%version = 8

  CALL opnfil(lfnloc,fileName,'UNKNOWN','UNFORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,fileName,'NEQSTORE')

  CALL nqwthead(lfnloc,neq)

  DO ii = 1, neq%misc%npar
    CALL writePar(lfnloc,1,neq%par(ii))
  END DO

  neqDim = neq%misc%npar*(neq%misc%npar+1)/2
  IF (neqDim > 0) CALL writeRec(lfnloc,neqDim,neq%aNor)

  neqDim = neq%misc%npar
  IF (neqDim > 0) CALL writeRec(lfnloc,neqDim,neq%bNor)

  CLOSE(lfnloc)

END SUBROUTINE neqstore

END MODULE
