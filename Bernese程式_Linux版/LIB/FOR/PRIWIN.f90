MODULE s_PRIWIN
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE priwin(isel,window)

! -------------------------------------------------------------------------
! Purpose:    Write window to output file
!
! Remarks:    For window(1) < -1D9 and window(2) > 1D9 are considered
!             as open epoch definition.
!
! Author:     U. Hugentobler
!
! Created:    15-Jul-2001
! Last mod.:  08-Aug-2005
!
! Changes:    28-Nov-2001  HU: Check for window=0
!             16-May-2003  RD: Init window to (/0d0,1d20/)
!             08-Aug-2005  HB: Use new SR TIMST2 (module)
!
! SR used:    jmt, radgms, mjdgps, timst2
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  USE s_mjdgps
  USE s_timst2
  IMPLICIT NONE
!
! Variables in parameter list
! ---------------------------
! IN:
  INTEGER(i4b)           :: isel      ! Selection (dummy)
  REAL(r8b),DIMENSION(2) :: window    ! Window start and end (MJD)

! Local Variables
! ---------------
  INTEGER(i4b)              :: iwin,iweek,isec

  REAL(r8b)                 :: second

  CHARACTER(LEN=19)         :: tString
  CHARACTER(LEN=8),DIMENSION(2),PARAMETER :: fromto = (/'From    ','To      '/)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

! Title
! -----
  WRITE(lfnprt,"(' PROCESSING WINDOW', &
             & /,' -----------------'/)")


! Both window boundaries are open
! -------------------------------
  IF (window(1) == 0D0 .AND. window(2) == 1d20) THEN
    WRITE(lfnprt,"(' All epochs processed',//)")
  ELSE

! Header
! ------
    WRITE(lfnprt,"(1X,79('-'), &
               & /,1X,8X,'Date       Time    ',6X, &
               &   'MJD        ',5X,'GPSWeek Seconds', &
               & /,1X,79('-'))")

! Loop over window boundaries
! ---------------------------
    DO iwin=1,2
      IF (window(iwin) == 0d0 .OR. window(iwin) == 1d20) THEN
        WRITE(lfnprt,"(1X,A8,'OPEN')")fromto(iwin)
      ELSE
        CALL timst2(1,1,window(iwin),tString)

        CALL MJDGPS(window(iwin),second,iweek)
        isec=IDNINT(second)

        WRITE(lfnprt,"(1X,A8,A,6X,F11.5,6X,I4.4,I10)") &
              fromto(iwin),tString,window(iwin),iweek,isec
      ENDIF
    ENDDO

! Footer
    WRITE(lfnprt,"(1X,79('-'),//)")

  ENDIF

  RETURN
END SUBROUTINE priwin

END MODULE
