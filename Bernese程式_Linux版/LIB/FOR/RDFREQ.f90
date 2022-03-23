MODULE s_RDFREQ
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdfreq(frqinfo)

! -------------------------------------------------------------------------
! Purpose:    Read frequency information from the frequency information
!             file into an array.
!
! Author:     C. Urschl
!
! Created:    23-Okt-2003
! Last mod.:  21-Sep-2010
!
! Changes:    21-Nov-2005 CU: Change dimension of header lines: 6 -> 5
!             21-Sep-2010 RD: ST2TIM can be used as a module now
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_frqfil, ONLY: t_frqinfo

  USE s_opnfil
  USE s_alcerr
  USE s_opnerr
  USE s_st2tim
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! output:
  TYPE(t_frqinfo)                          :: frqinfo     ! frequency info

! Local Variables
! ---------------
  INTEGER(i4b)                             :: iline, nline, fline
  INTEGER(i4b)                             :: ios, irc
  CHARACTER(LEN=timStrgLength2)            :: timstrg
  CHARACTER(LEN=lineLength)                :: line



! Open frequency file
! -------------------
  CALL gtflna(1,'FRQINFO',frqinfo%frqfile,ios)
  CALL opnfil(lfnloc, frqinfo%frqfile, 'OLD', 'FORMATTED', &
              'READONLY',' ',ios)
  CALL opnerr(lfnerr, lfnloc, ios, frqinfo%frqfile, 'RDFREQ')


! Count lines
! -----------
  nline = 0
  fline = 0

  DO iline = 1, 5                            ! Read 5 header lines
    READ(lfnloc,'(A)',iostat=irc) frqinfo%headline(iline)
  ENDDO

  DO                                         ! Loop over all lines
    READ(lfnloc,'(A)',iostat=irc) line
    IF (line == '' .OR. irc /= 0) THEN
      BACKSPACE lfnloc
      EXIT                                   ! Stop if blank line/end of file
    ELSE
      nline = nline + 1                      ! Count line
      CYCLE                                  ! Loop
    ENDIF
  ENDDO

  DO                                        ! Loop over all "foot" lines
    READ(lfnloc,'(A)',iostat=irc) line
    IF (irc /= 0) THEN
      EXIT                                  ! Stop if end of file
    ELSE
      fline = fline + 1                     ! Count line
      CYCLE                                 ! Loop
    ENDIF
  ENDDO


! Allocate arrays
! ---------------
  ALLOCATE(frqinfo%freq(nline),stat=ios)
  CALL alcerr(ios,"frqinfo%freq",(/nline/),'RDFREQ')
  ALLOCATE(frqinfo%footline(fline),stat=ios)
  CALL alcerr(ios,"freq",(/fline/),'RDFREQ')


! Read all records, fill arrays
! -----------------------------
  REWIND lfnloc

  READ(lfnloc,'(////)',iostat=irc)          ! Skip header lines

  DO iLine = 1, nline                       ! Read all records of freq.file
    READ(lfnloc,'(A)',iostat=irc) line
    READ(line,'(1X,A5,2X,A20,2X,A2,2X,F13.1,2X,A4,2X,A40,2X,A23)') &
         frqinfo%freq(iline)%typstrg, &
         frqinfo%freq(iline)%satsta,  &
         frqinfo%freq(iline)%name,    &
         frqinfo%freq(iline)%value,   &
         frqinfo%freq(iline)%unit,    &
         timstrg,                     &
         frqinfo%freq(iline)%remark
    CALL st2tim(2,2,timstrg,frqinfo%freq(iline)%window)
  ENDDO

  DO iline = 1, fline                       ! Read foot lines
    READ(lfnloc,'(A)',iostat=irc) frqinfo%footline(iline)
  ENDDO

! Close frequency file
! --------------------
  CLOSE(lfnloc)

  RETURN

END SUBROUTINE rdfreq

END MODULE
