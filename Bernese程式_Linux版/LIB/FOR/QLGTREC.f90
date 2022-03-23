MODULE s_QLGTREC
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE qlgtrec (lfnscr,stanam,nrec,qlobs)

! -------------------------------------------------------------------------
! Purpose:    Get info and observation records for station stanam from
!             scratch file (copy of SLR quick look file)
!
! Author:     C. Urschl
!
! Created:    10-Nov-2003
! Last mod.:  13-Jan-2009
!
! Changes:    11-Feb-2004 CU: bug fixed
!             31-Mar-2004 CU: Add error handling
!             01-Apr-2004 CU: Write time of signal reception  with nano sec.
!                             accuracy into RINEX file (use last column for
!                             receiver clock offset)
!             19-AUG-2004 CU: Read format revision number revnr
!             14-Mar-2005 CU: Add isel to call of sr cos2prn
!             13-Jan-2009 RD: Use '' as EOF-identifier in NEXTLINE
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_const,  ONLY: c
  USE d_qlfil,  ONLY: t_qlobs

  USE s_alcerr
  USE f_nextline
  USE s_cos2prn
  USE s_slr2cos
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                   :: lfnscr ! Scratch file
  CHARACTER(LEN=4)               :: stanam ! Station name
  INTEGER(i4b)                   :: nrec   ! # of observation records for stanam

! output:
  TYPE(t_qlobs)                  :: qlobs  ! Station information and
                                           ! observations for stanam

! Local Variables
! ---------------
  INTEGER(i4b)                   :: irc
  INTEGER(i4b)                   :: irec
  INTEGER(i4b)                   :: satnum
  INTEGER(i4b)                   :: year2, day
  INTEGER(i4b)                   :: newpas
  INTEGER(i4b)                   :: iwavel
  INTEGER(i4b)                   :: iobstp
  INTEGER(i4b)                   :: micros, picos
  INTEGER(i4b)                   :: numnpt
  INTEGER(i4b)                   :: press, temp, humid
  INTEGER(i4b)                   :: secfr
  INTEGER(i4b)                   :: powten
  INTEGER(i4b)                   :: vers

  REAL(r8b)                      :: rsec, sec, fracsec

  CHARACTER(LEN=lineLength)      :: line
  CHARACTER(LEN=9)               :: slrcos, cospar
  CHARACTER(LEN=1)               :: revnr


! Init
! ----
  irec            = 0
  qlobs%iwlfac(:) = 0

! Allocate array (SIZE= # observation records for station istat)
! --------------
  IF (ASSOCIATED(qlobs%qlrec)) DEALLOCATE(qlobs%qlrec,stat=irc)
  ALLOCATE(qlobs%qlrec(nrec),stat=irc)
  CALL alcerr(irc,'qlobs%qlrec',(/nrec/),'QLGTREC')

! Fill array with observation records for station istat
! -----------------------------------------------------
  line = nextline(lfnscr,0)     ! Read first line from scratch file

  DO                            ! Loop over all lines
    IF (line == '') EXIT        ! Found end of file, exit loop

    IF (line == '99999') THEN   ! Next station record found
      line = nextline(lfnscr,0) ! Read first line from scratch file

      IF (line(13:16) == stanam) THEN
! Read station info record
        READ(line(1:55),'(I7,I2,I3,4X,I2,I2,I4,19X,I1,2X,I1,7X,A1)',iostat=irc)&
          satnum,      & ! Satellite identifier
          year2,       & ! Year (2 digit)
          day,         & ! Day of year
          qlobs%cdps,  & ! System number
          qlobs%cdpo,  & ! Occupancy sequence number
          iwavel,      & ! Wavelength of the laser in (0.1 nanometer)
          qlobs%timsy, & ! Epoch time scale indicator
          qlobs%syscf, & ! System configuration indicator
          revnr          ! Normal point format revision number

        IF (irc /= 0) THEN       ! Error while reading
          WRITE(lfnerr,'(/,A,A,/,A,A4,A,/)')                            &
            ' ### SR QLGTREC: Error while reading SLR quick look data ',&
                                                          'record for ',&
            '                 station: ', stanam, '. Some data may be lost.'
          CYCLE                  ! Read next line
        ENDIF

        newpas = 1

! Get laser wavelengths
        IF (iwavel < 4000 .OR. iwavel > 6000) THEN
          IF (qlobs%iwlfac(2) == 0 .OR. qlobs%iwlfac(2) == iwavel) THEN
            qlobs%iwlfac(2) = iwavel
            iobstp          = 2
          ELSEIF (qlobs%iwlfac(2) /= iwavel) THEN
            WRITE(lfnerr,'(A,/,2A,3(/,A),F5.1,A,/)')                      &
               ' ### SR QLGTREC: 2 second wavelengths found.',            &
               '                 Criteria for wavelength assignment',     &
                                                    ' (hardwired): ',     &
               '                    400 nm < First  WL > 600nm',          &
               '                    600 nm < Second WL > 400nm',          &
               '                 Write wavelength: ', iwavel/10D0,        &
               ' as first wavelength.'
            qlobs%iwlfac(1) = iwavel
            iobstp          = 1
          ENDIF
        ELSE
          IF (qlobs%iwlfac(1) == 0 .OR. qlobs%iwlfac(1) == iwavel) THEN
            qlobs%iwlfac(1) = iwavel
            iobstp          = 1
          ELSEIF (qlobs%iwlfac(1) /= iwavel) THEN
            WRITE(lfnerr,'(A,/,2A,3(/,A),F5.1,A,/)')                      &
               ' ### SR QLGTREC: 2 first wavelengths found.',             &
               '                 Criteria for wavelength assignment',     &
                                                    ' (hardwired): ',     &
               '                    400 nm < First  WL > 600nm',          &
               '                    600 nm < Second WL > 400nm',          &
               '                 Write wavelength: ', iwavel/10D0,        &
               ' as second wavelength.'
            qlobs%iwlfac(2) = iwavel
            iobstp          = 2
          ENDIF
        END IF

! Read observation record
        DO
          line = nextline(lfnscr,0)

          IF (line == '' .OR. line(1:5) == '99999') EXIT

          rsec = 0d0

          READ(line(1:60),'(F14.1,1X,I7,1X,I6,I6,7X,I5,I4,I3,I4,I1,I1)', &
               iostat=irc)                                               &
            rsec ,  & ! Time of day of laser firing from 0h UTC (MJD in full sec),
            secfr,  & !                                  (fraction of second)
            micros, & ! 2way-time-of-flight correct.for syst.delay (microsec)
            picos,  & !                                            (picosec)
            press,  & ! Surface pressure (0.1 millibar)
            temp,   & ! Surface temperatur (0.1 degree Kelvin)
            humid,  & ! Relative humidity at surface (percent)
            numnpt, & ! # of raw ranges compressed into normal point
            vers,   & ! Flag to indicate data release (version number of record)
            powten    ! power of ten to multiply with numnpt (format revis.nr.2)

          irec = irec + 1

          IF (irc /= 0) THEN       ! Error while reading
            WRITE(lfnerr,'(/,A,A,/,A,A4,A,/)')                            &
              ' ### SR QLGTREC: Error while reading SLR quick look data ',&
                                                            'record for ',&
              '                 station: ', stanam, '. Some data may be lost.'
            qlobs%qlrec(irec)%iobstp = 0
            CYCLE                  ! Read next line
          ENDIF

! Check release number of data
! ----------------------------
!!!          qlobs%qlrec(irec)%versNr = vers

          IF ( vers > 0 ) THEN
            write(*,*) 'Data with newer release number found: ', vers, stanam, line
          ENDIF


! Observation type index (first or second wavelength)
          qlobs%qlrec(irec)%iobstp = iobstp

! Compute range measurement
          qlobs%qlrec(irec)%range = 0.5D0 * (micros*1.d-6 + picos*1.d-12) * C

! Time of signal reception
!!!        ! microsecond accuracy for time of signal reception (epoch(1))
!!!        ! in units of MJD
!!!          qlobs%qlrec(irec)%epoch(1) = rsec/86400d0 + secfr/86400d7 + (micros*1d-6 + picos*1d-12)/86400d0

        ! nanosecond accuracy for time of signal reception (epoch(1),epoch(2))
          sec     = DNINT(secfr*1d-7 + micros*1d-6)
          fracsec =       secfr*1d-7 + micros*1d-6   -  sec

        ! full seconds - in units of MJD
          qlobs%qlrec(irec)%epoch(1) = (rsec + sec)/86400d0

        ! fraction of second - in units of seconds
          qlobs%qlrec(irec)%epoch(2) = fracsec + picos*1d-12

! Pressure, temperature
          qlobs%qlrec(irec)%press  = 0.1d0 * press
          qlobs%qlrec(irec)%temp   = 0.1d0 * temp - 273.16d0
          qlobs%qlrec(irec)%humid  = humid * 1d0
! Number of single shots / normal point
          IF (revnr == " " .OR. revnr == "0") THEN
            qlobs%revnr = 0
            qlobs%qlrec(irec)%numnpt = numnpt
          ELSEIF (revnr == "1") THEN
            qlobs%revnr = 1
            qlobs%qlrec(irec)%numnpt = numnpt
          ELSEIF (revnr == "2") THEN
            qlobs%revnr = 2
            qlobs%qlrec(irec)%numnpt = numnpt * 10**powten
          ELSE
            WRITE(lfnerr,'(2(A,/))') &
              ' *** SR QLGTREC: Unknown normal point format revision number: ',&
              revnr
          ENDIF
          qlobs%qlrec(irec)%llinpt = newpas
          newpas                   = 0

! Translate satellite numbers
          WRITE(slrcos,'(I7.7)') satnum
          CALL slr2cos(slrcos,cospar)
          CALL cos2prn(2,cospar,qlobs%qlrec(irec)%epoch(1),qlobs%qlrec(irec)%satnr,irc)

        ENDDO

      ENDIF

    ELSE                        ! IF stanam not found:
      line = nextline(lfnscr,0) ! Read next line from scratch file
    ENDIF

  ENDDO

  REWIND(lfnscr)

  RETURN

END SUBROUTINE qlgtrec

END MODULE
