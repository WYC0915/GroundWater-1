MODULE s_CLSRCV
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE clsrcv(mulval,mulrms,iclass,clsstr,clsfac)

! -------------------------------------------------------------------------
! Purpose:    Classify receiver based on DCB multiplier
!
! Author:     S. Schaer
!
! Created:    07-Aug-2002
!
! Changes:    26-Aug-2002 : SS: Allow xrat=0
!
! SR used:    dordup
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  USE s_dordup
  IMPLICIT NONE

! List of parameters
! ------------------
! Input:
  REAL(r8b)                       :: mulval ! DCB multiplier value
  REAL(r8b)                       :: mulrms ! DCB multiplier rms error

! Output:
  INTEGER(i4b)                    :: iclass ! Suggested receiver class
                                            ! =0: No suggestion
                                            ! =1: P1/P2
                                            ! =2: C1/X2
                                            ! =3: C1/P2

  CHARACTER(LEN=5)                :: clsstr ! Receiver class as string

  REAL(r8b), DIMENSION(3)         :: clsfac ! Statistical factors in
                                            ! ascending order

! Local variables
! ---------------
  REAL(r8b)                       :: xrat
  REAL(r8b), DIMENSION(3)         :: tstfac

  INTEGER(i4b), DIMENSION(3)      :: tstind

! Compute test factors
! --------------------
  IF (mulrms > 0d0) THEN

    tstfac(1) = ABS(mulval)/mulrms
    tstfac(2) = ABS(mulval-1d0)/mulrms
    tstfac(3) = ABS(mulval-77d0**2/(77d0**2-60d0**2))/mulrms

  ELSE

    tstfac = 0d0

  ENDIF

! Sort test factors in ascending order
! ------------------------------------
  CALL dordup(tstfac,3,tstind)

  IF (tstind(1) == 1) THEN
    clsstr = 'P1/P2'
  ELSEIF (tstind(1) == 2) THEN
    clsstr = 'C1/X2'
  ELSEIF (tstind(1) == 3) THEN
    clsstr = 'C1/P2'
  ENDIF

  clsfac = tstfac(tstind)
  IF (clsfac(1) > 0d0) THEN
    xrat = clsfac(2)/clsfac(1)
  ELSE
    xrat = 0d0
  ENDIF

  IF (clsfac(1) > 15d0 .OR. &
      clsfac(2) <  3d0 .OR. &
      xrat      <  2d0) THEN

    iclass = 0
    clsstr = '?1/?2'

  ELSE

    iclass = tstind(1)

  ENDIF

END SUBROUTINE clsrcv


END MODULE
