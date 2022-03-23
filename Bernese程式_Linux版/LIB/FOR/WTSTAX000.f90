MODULE s_WTSTAX000
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE wtstax000(filename,stacrux,title)

! -------------------------------------------------------------------------
! Purpose:    Writes station crux file for RXOBV3 (inconsistencies in RINEX
!             header wrt. the station info file which are accepted without
!             warning/error)
!
! Author:     R. Dach
!
! Created:    27-May-2003
! Last mod.:  19-Jul-2010
!
! Changes:    17-Sep-2003 HU: Trim title
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             19-Jul-2010 SL: tab characters removed
!             29-Oct-2010 SL: use m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, lfnLoc, lfnErr
  USE d_stacrx, ONLY: t_stacrux,undef_c,undef_i,undef_e
  USE s_opnfil
  USE s_opnerr
  USE s_timst2
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)             :: filename
  TYPE(t_stacrux)              :: stacrux   ! Only section 2 is used
  CHARACTER(LEN=*),OPTIONAL    :: title

! Local Parameters
! ----------------
  CHARACTER(LEN=9), PARAMETER  :: srName = 'WTSTAX000'

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
  line=' '
  IF (PRESENT(title)) line = title
  WRITE(lfnloc,'(A,/,A,//,A,/,A)') TRIM(line), &
       '----------------------------------------' // &
       '----------------------------------------',   &
       'STATION NAME                   FROM     ' // &
       '              TO         RECEIVER TYPE  ' // &
       '       ANTENNA TYPE          REC #   ANT' // &
       ' #    NORTH      EAST      UP        REM' // &
       'ARK',                                        &
       '****************       YYYY MM DD HH MM ' // &
       'SS  YYYY MM DD HH MM SS  ***************' // &
       '*****  ********************  ******  ***' // &
       '***  ***.****  ***.****  ***.****    ***' // &
       '*********************'

! Write the data records
! ---------------------
  DO icrx = 1,stacrux%nInfo
    CALL timst2(2,2,stacrux%stainfo(icrx)%timint%t,datstr)
    WRITE(lfnloc,                                                         &
         '( A16,4X,3X,A40,2X,2(A20,2X),2(I6,2X),3(F8.4,2X),2X,A)')      &
          stacrux%stainfo(icrx)%stanam,   datstr,                         &
          stacrux%stainfo(icrx)%recnam,   stacrux%stainfo(icrx)%antnam,   &
          stacrux%stainfo(icrx)%recnum,   stacrux%stainfo(icrx)%antnum,   &
          stacrux%stainfo(icrx)%antecc(1),stacrux%stainfo(icrx)%antecc(2),&
          stacrux%stainfo(icrx)%antecc(3), &
          TRIM(stacrux%stainfo(icrx)%remark)
  END DO

! Write empty line
! ----------------
  WRITE(lfnloc,'(//,A20,3X,A40,2X,2(A20,2X),2(I6,2X),3(F8.4,2X))') &
    'Undefined values:   ', &
    '1990 01 01 00 00 00  2099 12 31 23 59 59', &
    undef_c,undef_c,undef_i,undef_i,undef_e,undef_e,undef_e

! Close file
! ----------
  CLOSE(lfnloc)

  RETURN

END SUBROUTINE wtstax000

END MODULE
