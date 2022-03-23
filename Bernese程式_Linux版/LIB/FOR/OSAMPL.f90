MODULE s_OSAMPL
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE osampl(epoch,sampl,resol,tsampl,tprev,tnext)

! -------------------------------------------------------------------------
! Purpose:    Check whether "epoch" coincides with a sampling epoch
!             and return previous and next sampling epoch.
!
! Remarks:
!
! Author:     U. Hugentobler
!
! Created:    02-Jun-2006
! Last mod.:  __-___-____
!
! Changes:    __-___-____ __:
!
! SR used:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern

  IMPLICIT NONE
!
! Variables in parameter list
! ---------------------------
! IN:
  REAL(r8b)              :: epoch     ! Requested epoch (MJD)
  REAL(r8b)              :: refepo    ! Reference epoch (MJD)
  REAL(r8b)              :: sampl     ! Sampling (sec)
  REAL(r8b)              :: resol     ! Resolution (sec)
! OUT:
  REAL(r8b)              :: tsampl    ! sampling epoch (MJD)
                                      ! =0D0: no sampling epoch
  REAL(r8b)              :: tprev     ! previous sampling epoch (MJD)
  REAL(r8b)              :: tnext     ! next sampling epoch (MJD)

! Local Variables
! ---------------
  INTEGER(i4b)           :: num,nmod,numref
  REAL(r8b)              :: frac,sampk,xnum,epo

  epo   = epoch
  frac  = (epo - DINT(epo))*86400D0/resol
  sampk = DNINT(sampl/resol)

  xnum  = frac/sampk
  num   = NINT(xnum)
  nmod  = NINT((xnum-num)*sampk)

! Sampling epochs
  IF (nmod < 0) THEN
    tsampl = 0D0
    tnext  = DINT(epo) + num*sampl/86400D0
    tprev  = tnext - sampl/86400D0
  ELSEIF (nmod > 0) THEN
    tsampl = 0D0
    tprev  = DINT(epo) + num*sampl/86400D0
    tnext  = tprev + sampl/86400D0
  ELSE
    tsampl = DINT(epo) + num*sampl/86400D0
    tprev  = tsampl - sampl/86400D0
    tnext  = tsampl + sampl/86400D0
  ENDIF

  RETURN
END SUBROUTINE osampl

END MODULE
