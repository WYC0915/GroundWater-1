MODULE s_ni_getorb
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE ni_getorb(svn,ider,tmjd,xv)

! -------------------------------------------------------------------------
! Purpose: get orbit r(t), velocity v(t), etc. for time tmjd up to the
!          derivative defined by "ider" using the coefficients of the
!          numerically integrated orbit.
!
!
! Parameters : svn    : number of satellite
!              ider   : number of highest derivative
!              tmjd   : time for which position is required
!              position, velocity, etc.
!
! Author:     Gerhard Beutler
!
! Created:    11-Sep-2006
! Last mod.:
!
! Changes:    11-Feb-2008  GB: DT= corrected
!             17-Jun-2008  AJ: Allow for non-integer epochs
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_const
  USE p_gravdet
  USE s_polevn

  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b)                            :: svn         ! satellite number
  INTEGER(i4b)                            :: ider        ! number of highest derivative
  REAL(r8b)                               :: tmjd        ! time in mjd
  REAL(r8b), DIMENSION(:)                 :: xv          ! position, velocity, etc.

! Local Variables
! ---------------
  INTEGER(i4b)                            :: inter=1,inter_prev=1
  REAL(r8b)                               :: t_sec, dt

! time in seconds relative to arc-beginning
  t_sec=(tmjd-m_fromto(1))*86400
!
! check whether t_sec should be integer
  IF (m_prcopt(28) == 0) THEN
    if(abs(t_sec-dnint(t_sec)) < 1.d-5)then
      t_sec=dnint(t_sec)
    endif
  ENDIF
!
! check whether t_sec is in subinterval "inter_prev",
! define correct interval of current request
  if(t_sec >= m_ftsave(1,inter_prev) .and. t_sec < m_ftsave(2,inter_prev))then
    inter=inter_prev
  elseif(t_sec < m_ftsave(1,1))then
    write(lfnerr,10)t_sec,m_ftsave(1,1)
10  format(//,' *** sr ni_getorb: t(sec)', f20.5,' < left int. boundary',f20.5)
    inter=1
    inter_prev=inter
  elseif(t_sec >= m_ftsave(2,m_ninter))then
    write(lfnerr,20)t_sec,m_ftsave(2,m_ninter)
20  format(//,' *** sr ni_getorb: t(sec)', f20.5,' > right int. boundary',f20.5)
    inter=m_ninter
    inter_prev=inter
  else
    do inter=1,m_ninter
      if(t_sec >= m_ftsave(1,inter) .and. t_sec < m_ftsave(2,inter))then
        inter_prev=inter
        exit
      endif
    enddo
  endif
!
! TIME IN SEC REL TO CURRENT INT. BOUNDARY (OF INTEGRATION)
  DT=t_sec-m_t0save(inter)
!
! POSITION AT OBSERVATION TIME
  CALL POLEVN(2,m_q,m_idim,dt,m_hsave(inter),m_coe(1,inter),XV)

END SUBROUTINE ni_getorb

END MODULE
