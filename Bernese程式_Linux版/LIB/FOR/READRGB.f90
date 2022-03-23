MODULE s_READRGB
CONTAINS

! ---------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ---------------------------------------------------------------------

SUBROUTINE readrgb(filRGB, slrInfo)

! ---------------------------------------------------------------------
! Purpose:    Read SLR data handling file (biases, Center-of-Mass)
!
! Author:     D. Thaller
!
! Created:    28-May-2009
! Last mod.:  21-Sep-2010
!
! Changes:    30-Sep-2009 DT: Changes for general SLR data handling file
!             21-Sep-2010 RD: ST2TIM can be used as a module now
!
! SR used:    opnfil, opnerr, alcerr, st2tim
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ---------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_time,   ONLY: t_timint
  USE m_epoch,  ONLY: ASSIGNMENT(=), OPERATOR(.RealToEpoch.)

  USE d_rgbfil, ONLY: t_slrInfo, init_slrFil

  USE s_opnerr
  USE s_opnfil
  USE s_alcerr
  USE s_st2tim

  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=fileNameLength)              :: filRGB  ! Name of SLR file

! output:
  TYPE(t_slrInfo)                            :: slrInfo ! File structure


! Local Variables
! ---------------
  TYPE(t_timint)                             :: timint

  CHARACTER(LEN=7), PARAMETER                :: srName = 'readRGB'
  CHARACTER(LEN=lineLength)                  :: line
  CHARACTER(LEN=timStrgLength2)              :: timStr

  INTEGER(i4b)                               :: irc, ios, iac
  INTEGER(i4b)                               :: iHead
  INTEGER(i4b)                               :: iRGB


! Initialization
! --------------
  CALL init_slrFil(slrInfo)

  iRGB = 0

! Open range bias file
! --------------------
  CALL opnfil(lfnloc,filRGB,'UNKNOWN','FORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,filRGB,'RGBINP')


! Read header lines
! -----------------
  DO iHead = 1, 7
    READ(lfnloc,'(A)',iostat=irc)  slrInfo%headline(iHead)
  ENDDO

  READ(slrInfo%headline(4), '(16X,F5.2)') slrInfo%version


! Count number of records
! -----------------------
  DO
    READ(lfnloc,'(A)',iostat=irc)  line

   ! Stop if blank line/end of file
    IF (line == '' .OR. irc /= 0) THEN
      EXIT
    ENDIF

    iRGB = iRGB + 1
  ENDDO

  slrInfo%nrgb = iRGB

! Allocate arrays
! ---------------
  ALLOCATE(slrInfo%rgb(iRGB), stat=iac)
  CALL alcerr(iac,'slrInfo%rgb',(/iRGB/),srName)

! Read all records, fill arrays
! -----------------------------
  REWIND lfnloc

  ! Skip header lines
  READ(lfnloc,'(//////)',iostat=irc)

  DO iRgb = 1, slrInfo%nrgb

    READ(lfnloc,'(A3,2X,A16,6X,I4,2X,A1,I1,2(2X,F11.6),2X,A40,2X,A6,2X,A23)',iostat=irc)  &
         slrInfo%rgb(irgb)%corrTyp,                           &
         slrInfo%rgb(irgb)%staNam, slrInfo%rgb(irgb)%satNum,  &
         slrInfo%rgb(irgb)%WLchar, slrInfo%rgb(irgb)%WLind,   &
         slrInfo%rgb(irgb)%value,  slrInfo%rgb(irgb)%sigma,   &
         timStr, slrInfo%rgb(irgb)%solFlag, slrInfo%rgb(irgb)%remark

    IF ( slrInfo%rgb(irgb)%satNum == 0 ) slrInfo%rgb(irgb)%satNum=1000

    CALL st2tim(1,2,timStr,timint%t)

    slrInfo%rgb(irgb)%timWin%t(1) = .realToEpoch.timint%t(1)
    slrInfo%rgb(irgb)%timWin%t(2) = .realToEpoch.timint%t(2)

  ENDDO

  CLOSE(lfnloc)

  RETURN

END SUBROUTINE readrgb

END MODULE
