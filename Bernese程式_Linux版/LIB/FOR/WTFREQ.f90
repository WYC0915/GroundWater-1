MODULE s_WTFREQ
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE wtfreq(frqinfo)

! -------------------------------------------------------------------------
! Purpose:    Write frequency information into the frequency information
!             file.
!
! Author:     C. Urschl
!
! Created:    23-Okt-2003
! Last mod.:  21-Nov-2005
!
! Changes:    09-Nov-2004 CU: Change format statement
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             21-Nov-2005 CU: Change dimension of header lines: 6 -> 5
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_const,  ONLY: date, time, const_def
  USE d_frqfil, ONLY: t_frqinfo

  USE s_opnfil
  USE s_opnerr
  USE s_dattim
  USE s_timst2
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  TYPE(t_frqinfo)                           :: frqinfo  ! frequency info

! Local Variables
! ---------------
  INTEGER(i4b)                              :: iline, nline, fline
  INTEGER(i4b)                              :: ios
  CHARACTER(LEN=timStrgLength2)             :: timstrg


! Open frequency file
! -------------------
  CALL opnfil(lfnloc, frqinfo%frqfile, 'UNKNOWN', 'FORMATTED', &
              ' ',' ',ios)
  CALL opnerr(lfnerr, lfnloc, ios, frqinfo%frqfile, 'WTFREQ')


! Get the creation date
! ---------------------
  IF (const_def /= 1) CALL dattim(date,time)


! Write records into frequency file
! ---------------------------------
  nline = SIZE(frqinfo%freq)
  fline = SIZE(frqinfo%footline)

  WRITE(lfnloc,'(A114,1X,A9,1X,A5)') &       ! Write first header line
    frqinfo%headline(1)(1:116),date,time     ! include creation date
  DO iline = 2, 5                            ! Write all header lines
    WRITE(lfnloc,'(A)') TRIM(frqinfo%headline(iline))
  ENDDO

  DO iLine = 1, nline                        ! Write all records
    CALL timst2(2,2,frqinfo%freq(iline)%window,timstrg)
    WRITE(lfnloc,'(1X,A5,2X,A20,2X,A2,2X,F13.1,2X,A4,2X,A40,2X,A23)') &
         frqinfo%freq(iline)%typstrg, &
         frqinfo%freq(iline)%satsta,  &
         frqinfo%freq(iline)%name,    &
         frqinfo%freq(iline)%value,   &
         frqinfo%freq(iline)%unit,    &
         timstrg,                     &
         frqinfo%freq(iline)%remark
  ENDDO

  DO iline = 1, fline                        ! Write foot lines
    WRITE(lfnloc,'(A)') TRIM(frqinfo%footline(iline))
  ENDDO

! Close frequency file
! --------------------
  CLOSE(lfnloc)

  RETURN

END SUBROUTINE wtfreq

END MODULE
