MODULE s_STAMP
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE stamp(lfn,string)

! -------------------------------------------------------------------------
! Purpose:    Write time stamp, time and time difference to previous stamp
!
! Remarks:
!
! Author:     U. Hugentobler
!
! Created:    31-Dec-2001
! Last mod.:  01-Mar-2002
!
! Changes:    28-Feb-2002  HU: Do not print for lfn<0
!             01-Mar-2002  HU: Error at midnight corrected
!
! SR used:    clocks
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_clocks
  IMPLICIT NONE

! DUMMY ARGUMENTS
! ---------------
! Input:
  INTEGER(i4b)                :: lfn       ! lfn
  CHARACTER(LEN=*)            :: string    ! output string

! LOCAL VARIABLES
! ---------------
  INTEGER(i4b),SAVE           :: ifirst=1
  INTEGER(i4b),DIMENSION(8)   :: mm
  REAL(r8b),SAVE              :: time
  REAL(r8b)                   :: seconds,delta

  CHARACTER(LEN=3),DIMENSION(12),SAVE :: &
                          month = (/'JAN','FEB','MAR','APR','MAY','JUN', &
                                    'JUL','AUG','SEP','OCT','NOV','DEC'/)
  CHARACTER(LEN=9)            :: datstr
  CHARACTER(LEN=12)           :: timstr

  CALL clocks(mm)
  seconds=mm(7)+mm(8)/1000D0+mm(6)*60D0+mm(5)*3600D0

  WRITE(datstr,"(I2.2,'-',A3,'-',I2.2)")mm(3),month(mm(2)),MOD(mm(1),100)

  WRITE(timstr,"(I2.2,':',I2.2,':',I2.2,'.',I3.3)")mm(5),mm(6),mm(7),mm(8)

  IF (ifirst == 1) THEN
    ifirst = 0
    IF (lfn>=0) &
      write(lfn,"(' Time Stamp: ',A9,1X,A12,27X,A)")datstr,timstr,TRIM(string)
  ELSE
    delta  =seconds-time
    IF (delta < 0D0) delta=delta+86400D0
    IF (lfn>=0) &
      write(lfn,"(' Time Stamp: ',A9,1X,A12, &
                & '   Elapsed:',F9.3,' sec   ',A)") &
                      datstr,timstr,delta,TRIM(string)
  ENDIF
  time = seconds

  RETURN
END SUBROUTINE stamp

END MODULE
