MODULE s_GTTABHED
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE gttabhed(filnam,nutnam,subnam,cmcmod,harstr,cmcyn,timint)

! -------------------------------------------------------------------------
! Purpose:    Read header of TAB-file.
!
! Remarks:    The routine reads the header of TAB-files.
!
! Author:     A. GAEDE
!
! Created:    18-Jul-2006
! Last mod.:  12-Jan-2011
!
! Changes:    06-Dec-2010 RD: Additional comment line for CMC
!             12-Jan-2011 MF: Error due to READ attempt in empty 'line'
!                             to get start and stop time
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_time,  ONLY: t_timint
  USE s_opnfil
  USE s_opnerr
  USE s_exitrc
  USE f_djul

  IMPLICIT NONE
!
! Variables in parameter list
! ---------------------------
! IN:
  CHARACTER(LEN=fileNameLength)           :: filnam   ! name of tab file
! OUT:
  CHARACTER(LEN=16),             OPTIONAL :: nutnam   ! nutation model
  CHARACTER(LEN=16),             OPTIONAL :: subnam   ! subdaily pole model
  CHARACTER(LEN=16),DIMENSION(2),OPTIONAL :: cmcmod   ! CMC model
  CHARACTER(LEN=20),             OPTIONAL :: harstr   ! HARDISP identifier
  LOGICAL,          DIMENSION(2),OPTIONAL :: cmcyn    ! CMC applied?
                                                      ! 1: OTL, 2: ATL
  TYPE(t_timint),                OPTIONAL :: timint   ! TAB-interval

! Local Variables
! ---------------
  INTEGER(i4b)                    :: iostat=0
  INTEGER(i4b)                    :: year
  INTEGER(i4b)                    :: month
  REAL(r8b)                       :: hour
  REAL(r8b)                       :: min
  REAL(r8b)                       :: sec
  REAL(r8b)                       :: day
  CHARACTER(LEN=256)              :: line
  CHARACTER(LEN=1)                :: cmcchr

  IF (PRESENT(nutnam)) nutnam = ''
  IF (PRESENT(subnam)) subnam = ''
  IF (PRESENT(cmcmod)) cmcmod = ''
  IF (PRESENT(harstr)) harstr = ''
  IF (PRESENT(cmcyn)) cmcyn  = .false.
  IF (PRESENT(timint)) timint%t = (/ 0d0,1d20 /)
  cmcchr = ''

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! READ HEADER
! ===========
! ----------------------------------------------
  CALL opnfil(lfnloc,filnam,'OLD',' ', 'READONLY',' ',iostat)
  CALL opnerr(lfnerr,lfnloc,iostat,filnam,'GTTABHED')

! Search for model description in the header
! ------------------------------------------
  DO
    READ(lfnloc,'(A)',IOSTAT=iostat)line
    IF (iostat > 0) THEN
      WRITE(lfnerr,"(/,' *** SR GTTABHED: Header not found in TAB-', &
                     &  'file.',/,18X,'Filename: ',A32,/)")filnam
      CALL exitrc(2)
    ELSE IF (iostat < 0) THEN
      EXIT
    ENDIF

!   Header line "CELESTIAL"
    IF (line(1:9) .EQ. 'CELESTIAL') THEN
      IF (PRESENT(nutnam))  READ(line,"(23x,A16)")nutnam
      IF (PRESENT(subnam))  READ(line,"(69x,A16)")subnam
      IF (LEN_TRIM(line) > 95) THEN
        IF (PRESENT(cmcmod))  READ(line,"(95x,A16)")cmcmod
        IF (PRESENT(cmcyn)) THEN
          READ(line,"(118x,A1)")cmcchr
          IF (cmcchr=='Y')cmcyn(1)=.true.
        ENDIF
      ENDIF
    ENDIF
!

!   Header line "LOADING CMC"
!LOADING CMC - OTLOAD: 1234567890123456 / 12345678901234567890 - Y     ATLOAD: 1234567890123456 - Y
    IF (line(1:9) .EQ. 'LOADING C') THEN
      IF (PRESENT(cmcmod)) THEN
        READ(line,'(22x,A16)')cmcmod(1)
        READ(line,'(78x,A16)')cmcmod(2)
      ENDIF
      IF (PRESENT(harstr)) THEN
        READ(line,"(41x,A16)")harstr
      ENDIF
      IF (PRESENT(cmcyn)) THEN
        READ(line,'(64x,A1)')cmcchr
        IF (cmcchr=='Y')cmcyn(1)=.true.
        READ(line,'(97x,A1)')cmcchr
        IF (cmcchr=='Y')cmcyn(2)=.true.
      ENDIF
    ENDIF

!   Start and end time:
    IF (LEN_TRIM(line) .GE. 74 .AND. line(5:7) .EQ. '   ') THEN
      READ(line,*) month, day, year, hour, min, sec
      IF (PRESENT(timint)) THEN
        day = day + ((sec/60.d0 + min)/60.d0 + hour)/24.d0
        IF (timint%t(1) == 0d0) THEN
          timint%t(1) = djul(year, month, day)
        ELSE
          timint%t(2) = djul(year, month, day)
        ENDIF
      ENDIF
    ENDIF
  ENDDO

  CLOSE(lfnloc)

  RETURN
END SUBROUTINE gttabhed

END MODULE s_GTTABHED
