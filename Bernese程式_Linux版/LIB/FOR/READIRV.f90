! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE s_readirv

! -------------------------------------------------------------------------
! Purpose:    Read IRV file
!
! Author:     C. Urschl
!
! Created:    25-Feb-2005
! Last mod.:  21-Sep-2010
!
! Changes:    21-Sep-2010 RD: ST2TIM can be used as a module now
!
! SR used:    opnfil, opnerr, exitrc
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

CONTAINS

  SUBROUTINE readirv (irvfil,window,statevec,irvepo,ios)

! Modules
! -------
  USE m_bern
  USE s_opnerr
  USE s_exitrc
  USE s_opnfil
  USE s_st2tim

  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=fileNameLength)           :: irvfil   ! IRV file
  REAL(r8b), DIMENSION(2)                 :: window   ! Time window

! output:
  REAL(r8b), DIMENSION(6)                 :: statevec ! Statevector
  REAL(r8b)                               :: irvepo   ! Epoch
  INTEGER(i4b)                            :: ios      ! = 0 epoch found
                                                      ! = 1 no epoch found
! Local Variables
! ---------------
  INTEGER(i4b)                            :: irc
  INTEGER(i4b)                            :: irec, numrec

  REAL(r8b)                               :: epoch
  REAL(r8b)                               :: seconds
  REAL(r8b), DIMENSION(6)                 :: xsat

  CHARACTER(LEN=lineLength)               :: line
  CHARACTER(LEN=19)                       :: timstr


! Init
! ----
  irvepo   = 0d0
  statevec = 0d0
  ios      = 0
  timstr   = ''

! Open IRV file
! -------------
  CALL opnfil(lfnloc,irvfil,'OLD','FORMATTED','READONLY',' ',irc)
  CALL opnerr(lfnerr,lfnloc,irc,irvfil,'READIRV')


! Loop over all lines
! -------------------
  DO
    READ(lfnloc,'(A)',iostat=irc) line
    IF (irc > 0) THEN
      WRITE(lfnerr,'(/,A,/,2A,/)')                        &
        ' *** SR READIRV: Error while reading IRV file.', &
        '                 IRV file: ', TRIM(irvfil)
      CALL exitrc(2)
    ENDIF

  ! Found EOF, exit loop
    IF (irc == -1) EXIT

  ! Read number of records for each entry (day)
    IF (line(25:25) == '') THEN
      numrec = 1
    ELSE
      READ(line(25:25),'(I1)',iostat=irc) numrec
    ENDIF

    IF (irc > 0) THEN
      WRITE(lfnerr,'(/,A,/,2A,/)')                        &
        ' *** SR READIRV: Error while reading IRV file.', &
        '                 IRV file: ', TRIM(irvfil)
      CALL exitrc(2)
    ENDIF

  ! Loop over all records per day
    DO irec = 1, numrec

      IF (irec == 1) THEN

        READ(lfnloc,'(A)',iostat=irc) line
      ! Read epoch
        READ(line(2:22),'(A16,1X,F4.1)',iostat=irc) timstr(1:16), seconds
        CALL st2tim(1,1,timstr,epoch)
        epoch = epoch + seconds/86400d0
      ! Read satellite coordinates
        READ(line(24:76),'(3(1X,F17.6))',iostat=irc) xsat(1:3)

        READ(lfnloc,'(A)',iostat=irc) line
      ! Read satellite velocity
        READ(line(24:76),'(3(1X,F17.6))',iostat=irc) xsat(4:6)

        READ(lfnloc,'(A)',iostat=irc) line

      ELSE

        READ(lfnloc,'(A)',iostat=irc) line
        READ(lfnloc,'(A)',iostat=irc) line
        READ(lfnloc,'(A)',iostat=irc) line

      ENDIF

      IF (irc > 0) THEN
        WRITE(lfnerr,'(/,A,/,2A,/)')                        &
          ' *** SR READIRV: Error while reading IRV file.', &
          '                 IRV file: ', TRIM(irvfil)
        CALL exitrc(2)
      ENDIF

    ENDDO

  ! epoch found
    IF (epoch >= window(1) .AND. epoch <= window(2)) THEN
      ios = 1
      irvepo   = epoch
      statevec = xsat
      EXIT
    ENDIF

  ENDDO

  CLOSE(lfnloc)

  RETURN

  END SUBROUTINE readirv

END MODULE s_readirv
