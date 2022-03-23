MODULE s_WTSTAX
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE wtstax(filename, stacrux, title)

! -------------------------------------------------------------------------
! Purpose:    Writes station crux file for RXOBV3 (inconsistencies in RINEX
!             header wrt. the station info file which are accepted without
!             warning/error)
!
! Author:     R. Dach
!
! Created:    27-May-2003
! Last mod.:  29-Oct-2010
!
! Changes:    17-Sep-2003 HU: Trim title
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             19-Jul-2010 SL: tab characters removed
!             29-Oct-2010 SL: use m_bern with ONLY, stacrux version 1.01
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b,lfnLoc,lfnErr
  USE d_stacrx, ONLY: t_stacrux,undef_c,undef_e
  USE s_opnfil
  USE s_opnerr
  USE s_timst2
  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=*)             :: filename
  TYPE(t_stacrux)              :: stacrux   ! Only section 2 is used
  CHARACTER(LEN=*),OPTIONAL    :: title

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER  :: srName = 'WTSTAX'

! Local Variables
! ---------------
  CHARACTER(LEN=206)           :: line
  CHARACTER(LEN=40)            :: datstr
  INTEGER(i4b)                 :: icrx
  INTEGER(i4b)                 :: ios

! Open the STACRUX file
! ---------------------
  CALL opnfil(lfnloc,filename,'UNKNOWN','FORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,filename,srName)

! Read the title
! --------------
  IF(PRESENT(title)) THEN
    line = title
  ELSE
    line = 'STATION PROBLEM FILE'
  ENDIF

  WRITE(lfnloc,'(A)') TRIM(line)
  WRITE(lfnloc,'(A)') '----------------------------------------&
                      &----------------------------------------'
  WRITE(lfnloc,'(A)')
  WRITE(lfnloc,'(A,1x,F4.2)') 'FORMAT VERSION:',stacrux%version
  WRITE(lfnloc,'(A)')
  WRITE(lfnloc,'(A)') &
    'STATION NAME          '                     // &
    '        FROM                   TO         ' // &
    'RECEIVER TYPE         '                     // &
    'RECEIVER SERIAL NBR   '                     // &
    'ANTENNA TYPE          '                     // &
    'ANTENNA SERIAL NBR    '                     // &
    ' NORTH    '                                 // &
    '  EAST    '                                 // &
    '  UP      '                                 // &
    'REMARK'
  WRITE(lfnloc,'(A)') &
    '****************      '                     // &
    'YYYY MM DD HH MM SS  YYYY MM DD HH MM SS  ' // &
    '********************  '                     // &
    '********************  '                     // &
    '********************  '                     // &
    '********************  '                     // &
    '***.****  '                                 // &
    '***.****  '                                 // &
    '***.****  '                                 // &
    '************************'

! Write the data records
! ----------------------
  DO icrx = 1,stacrux%nInfo
    CALL timst2(2,2,stacrux%stainfo(icrx)%timint%t,datstr)
    WRITE(lfnloc,'(A16,4X,2X,A40,2X,2(A20,2X,A20,2X),3(F8.4,2X),A)') &
      stacrux%stainfo(icrx)%stanam, &
      datstr, &
      stacrux%stainfo(icrx)%recnam, &
      stacrux%stainfo(icrx)%recser, &
      stacrux%stainfo(icrx)%antnam, &
      stacrux%stainfo(icrx)%antser, &
      stacrux%stainfo(icrx)%antecc(1), &
      stacrux%stainfo(icrx)%antecc(2), &
      stacrux%stainfo(icrx)%antecc(3), &
      TRIM(stacrux%stainfo(icrx)%remark)
  END DO

! Write empty line
! ----------------
  WRITE(lfnloc,'(//,A20,2X,A40,2X,2(A20,2X,A20,2X),3(F8.4,2X))') &
    'Undefined values:   ', &
    '1990 01 01 00 00 00  2099 12 31 23 59 59', &
    undef_c,undef_c,undef_c,undef_c,undef_e,undef_e,undef_e

! Close file
! ----------
  CLOSE(lfnloc)

  RETURN

END SUBROUTINE wtstax

END MODULE
