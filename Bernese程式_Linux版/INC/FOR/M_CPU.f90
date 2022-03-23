
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE m_cpu

! -------------------------------------------------------------------------
! Purpose:    This module provides routines to access the CPU start and
!             end time status and prints the difference on the screen
!
! Author:     R. Dach
!
! Created:    23-Sep-2010
! Last mod.:  12-Oct-2010
!
! Changes:    12-Oct-2010 SL/RD: help string for function call added
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE s_clocks
  USE f_djul
  PRIVATE

  PUBLIC    :: cpu_start, cpu_now, cpu_end

  REAL(r8b),DIMENSION(2) :: start ! Variable to store the start epoch
  REAL(r8b),DIMENSION(2) :: last  ! Variable to store the last access epoch

CONTAINS

! -----------------------------------------------------------------------------
! Routine to access and store the start epoch
! -----------------------------------------------------------------------------
  SUBROUTINE cpu_start(run)
    LOGICAL                      :: run  ! run the counter or not
    INTEGER(i4b), DIMENSION(1:8) :: mm

    start = 1d20
    IF (run) THEN

      CALL cpu_time(start(1))

      CALL clocks(MM)
      start(2) = DJUL(MM(1),MM(2),DBLE(MM(3))+ &
               (DBLE(MM(5))+(DBLE(MM(6))+(DBLE(MM(7))+DBLE(MM(8))/1000d0)/60d0)/60d0)/24D0)

      last = start

    ENDIF
  END SUBROUTINE

! -----------------------------------------------------------------------------
! Routine to access the difference wrt. the prev. call
! -----------------------------------------------------------------------------
  SUBROUTINE cpu_now(myText)
    CHARACTER(LEN=*)               :: myText

    REAL(r8b),DIMENSION(2)         :: now
    INTEGER(i4b), DIMENSION(1:8)   :: mm
    CHARACTER(LEN=lineLength)      :: help

    IF (start(1) /= 1d20) THEN
      CALL cpu_time(now(1))

      CALL clocks(MM)
      now(2) = DJUL(MM(1),MM(2),DBLE(MM(3))+ &
               (DBLE(MM(5))+(DBLE(MM(6))+(DBLE(MM(7))+DBLE(MM(8))/1000d0)/60d0)/60d0)/24D0)

      IF (program_Name /= '') THEN
        help = &
          '>>> CPU/Real time at "' // TRIM(myText) // '": ' //      &
          TRIM(write_time((now(1)-start(1))/86400d0 )) // ' / ' //  &
          TRIM(write_time( now(2)-start(2) )) //          '  ( ' // &
          TRIM(write_time((now(1)-last(1))/86400d0 )) //  ' / ' //  &
          TRIM(write_time( now(2)-last(2)  )) //          ' )'
        WRITE(*,'(/,A,/)') TRIM(help)
      ENDIF

      last = now
    ENDIF
  END SUBROUTINE

! -----------------------------------------------------------------------------
! Routine to access and the end epoch and report the CPU-statistics
! -----------------------------------------------------------------------------
  SUBROUTINE cpu_end(irCode)
    INTEGER(i4b)                   :: irCode

    REAL(r8b),DIMENSION(2)         :: end
    INTEGER(i4b), DIMENSION(1:8)   :: mm
    CHARACTER(LEN=shortLineLength) :: help

    IF (start(1) /= 1d20) THEN
      call cpu_time(end(1))

      CALL clocks(MM)
      end(2) = DJUL(MM(1),MM(2),DBLE(MM(3))+ &
               (DBLE(MM(5))+(DBLE(MM(6))+    &
               (DBLE(MM(7))+DBLE(MM(8))/1000d0)/60d0)/60d0)/24D0)

      IF (program_Name /= '') THEN
        help = &
          '>>> CPU/Real time for pgm "' // TRIM(program_Name) // '": ' // &
          TRIM(write_time((end(1)-start(1))/86400d0 )) // ' / ' //        &
          TRIM(write_time( end(2)-start(2) ))
        WRITE(*,'(/,A,/)') TRIM(help)

        WRITE(lfnprt,'(//,A)') &
        '-----------------------------------------------------------------'
        help = &
          '>>> CPU/Real time for pgm "' // TRIM(program_Name) // '": ' // &
          TRIM(write_time((end(1)-start(1))/86400d0 )) // ' / ' //        &
          TRIM(write_time( end(2)-start(2) ))
        WRITE(lfnprt,'(A)') TRIM(help)

        IF(irCode == 0 .OR. irCode == 1) THEN
          WRITE(lfnprt,'(A,/)') '>>> Program finished successfully'
        ELSE
          WRITE(lfnprt,'(A,/)') '>>> Program ended with an error'
        ENDIF
      ENDIF
    ENDIF
  END SUBROUTINE

! -----------------------------------------------------------------------------
  FUNCTION write_time(inpTime)
! -----------------------------------------------------------------------------
    USE s_radgms

    REAL(r8b)         :: inpTime     ! Time in days
    CHARACTER(LEN=13) :: write_time  ! String for time hh:mm:ss.sss

    CHARACTER(LEN=13) :: hlp_time
    CHARACTER(LEN= 1) :: vorz
    INTEGER(i4b)      :: hh,mm
    INTEGER(i4b)      :: iChr
    REAL(r8b)         :: sec

    hlp_time = ''
    CALL radgms(3,inpTime,vorz,hh,mm,sec)

    IF (hh < 10) THEN
      WRITE(hlp_time,'(I1,":",I2.2,":",F6.3)') hh,mm,sec
      iChr = 6
    ELSE IF (hh < 100) THEN
      WRITE(hlp_time,'(I2,":",I2.2,":",F6.3)') hh,mm,sec
      iChr = 7
    ELSE
      WRITE(hlp_time,'(I3,":",I2.2,":",F6.3)') hh,mm,sec
      iChr = 8
    ENDIF
    IF (hlp_time(iChr:iChr) == ' ') hlp_time(iChr:iChr) = '0'

    write_time = hlp_time
  END FUNCTION
END MODULE m_cpu


