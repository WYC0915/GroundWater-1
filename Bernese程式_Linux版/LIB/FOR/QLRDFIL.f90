MODULE s_QLRDFIL
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE qlrdfil (window,satlst,scrfil,staList,nlist,numrec)

! -------------------------------------------------------------------------
! Purpose:    Read quick look files, copy all records fitting the
!             observation window to scratch file, extract list of stations
!
! Author:     C. Urschl
!
! Created:    06-Nov-2003
!
! Changes:    16-Jan-2004 CU: Check added
!             31-Mar-2004 CU: Remove nextline, read lines using READ
!             28-Jun-2004 RD: Use maxsta from M_MAXDIM.f90
!             16-Aug-2004 CU: Replace exitrc(2) with exitrc(0) for warning
!                             message
!             14-Mar-2005 CU: Add isel to call of sr prn2cos
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             07-Oct-2010 RD: Do not close the lfnprt file
!             27-Mar-2012 RD: Use LISTC1 as module now
!             20-Sep-2012 RD: Correctly deallocate the arrays
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfnprt, lfn001, lfn002, &
                      fileNameLength80, keyValueLength, lineLength, &
                      timstrgLength2
  USE m_maxdim, ONLY: maxsta

  USE s_alcerr
  USE s_opnfil
  USE f_djul
  USE s_opnerr
  USE s_timst2
  USE f_listc1
  USE f_dgpsut
  USE s_readkeys
  USE s_cos2slr
  USE s_exitrc
  USE f_iyear4
  USE s_prn2cos
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  REAL(r8b), DIMENSION(2)                 :: window ! Time window
  INTEGER(i4b), DIMENSION(:), POINTER     :: satlst ! List of satellite numbers
  CHARACTER(LEN=fileNameLength80)         :: scrfil ! Name of scratch file

! output:
  CHARACTER(LEN=4), DIMENSION(maxsta)     :: staList! List of stations
  INTEGER(i4b)                            :: nlist  ! # of stations
  INTEGER(i4b),     DIMENSION(maxsta)     :: numrec ! # of records per station

! Local Parameters
! ----------------
  CHARACTER(LEN=7), PARAMETER             :: srName = 'QLRDFIL'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength),   DIMENSION(:), POINTER     :: keyValue
  CHARACTER(LEN=fileNameLength80), DIMENSION(:), ALLOCATABLE :: qlFile
  CHARACTER(LEN=7),                DIMENSION(:), ALLOCATABLE :: ilrs
  CHARACTER(LEN=lineLength)               :: line, staline
  CHARACTER(LEN=4)                        :: stanam
  CHARACTER(LEN=timStrgLength2)           :: tstrng
  CHARACTER(LEN=9)                        :: cospar

  INTEGER(i4b)                            :: iql, nql
  INTEGER(i4b)                            :: isat
  INTEGER(i4b)                            :: ipos
  INTEGER(i4b)                            :: irc,iac
  INTEGER(i4b)                            :: year, year2
  INTEGER(i4b)                            :: doy
  INTEGER(i4b)                            :: gpsutc
  INTEGER(i4b)                            :: sec, secfr
  INTEGER(i4b)                            :: ifepo
  INTEGER(i4b)                            :: nsat
  INTEGER(i4b)                            :: numsat
  INTEGER(i4b)                            :: sumrec

  REAL(r8b)                               :: tjul, tjulv, tjuldy
  REAL(r8b)                               :: rsec

  LOGICAL                                 :: cpStaline
  LOGICAL                                 :: satok


! Init
! ----
  NULLIFY(keyValue)
  nlist     = 0
  numrec(:) = 0
  sumrec    = 0

! Read file names of input QL files
! ---------------------------------
  CALL readkeys('QLFILE', keyValue, irc)
  IF (irc == 0) THEN

    nql = SIZE(keyValue)
    ALLOCATE (qlFile(nql), stat=iac)
    CALL alcerr(iac, 'qlFile',(/nql/), srName)

    DO iql = 1, nql
      qlFile(iql) = keyValue(iql)
    END DO

  ELSE
    WRITE(lfnerr,'(/,A,A,/)')                                             &
    ' *** SR QLRDFIL: A problem appeared while reading the filenames of ',&
    'the input quick look files'
    CALL exitrc(2)
  ENDIF

! Write input ql files and options to protocol file
! -------------------------------------------------
! Input filenames
  WRITE(lfnprt,'(A,/,A)')                                &
    ' File  SLR quick look files (normal point format)', &
    ' ------------------------------------------------'

  DO iql = 1, nql
    WRITE(lfnprt,'(1X,I4,2X,A)')iql,TRIM(qlFile(iql))
  END DO

! List of Satellites
  WRITE(lfnprt,'(/,2(/,A),/,2(/,A))')           &
    ' LIST OF SATELLITES',                      &
    ' ------------------',                      &
    ' PRN  COSPAR-ID  ILRS-ID',                 &
    ' -----------------------'

  numsat = 0
  nsat   = SIZE(satlst)
  ALLOCATE(ilrs(nsat), stat=irc)
  CALL alcerr(irc,'ilrs',(/nsat/),srName)

  DO isat = 1, nsat
  ! Translate prn number via sat.info file to cospar number
    CALL prn2cos(11,satlst(isat),window(1),cospar,irc)
    IF (irc /= 0) THEN
      CYCLE
    ENDIF
  ! Translate cospar number to ilrs number (used in ql files)
    numsat = numsat + 1
    CALL cos2slr(cospar,ilrs(numsat))
    WRITE(lfnprt,'(1X,I3,2X,A9,2X,A7)') satlst(isat), cospar, ilrs(numsat)
  ENDDO

! Observation window
  CALL timst2(1,2,window,tstrng)

  WRITE(lfnprt,'(/,2(/,A),/,2(/,A),/,1X,A)')    &
    ' OBSERVATION WINDOW',                      &
    ' ------------------',                      &
    ' Start                End                ',&
    ' ----------------------------------------',&
      tstrng

! Output filenames
  WRITE(lfnprt,'(/,2(/,A),/,2(/,A))')           &
    ' OUTPUT FILENAMES',                        &
    ' ----------------',                        &
    ' File  RINEX observation file            RINEX meteo file', &
    ' ---------------------------------------------------------------------'

! Open scratch file
! -----------------
  CALL opnfil(lfn001,scrFil,'NEW',' ',' ',' ',irc )
  CALL opnerr(lfnerr,lfn001,irc,scrFil,srName)

! Copy records from QL file to scratch file, extract station names
! ----------------------------------------------------------------
  DO iql = 1, nql                   ! Loop over all QL files

  ! Open QL file
    CALL opnfil(lfn002,qlFile(iql),'OLD','FORMATTED','READONLY',' ',irc )
    CALL opnerr(lfnerr,lfn002,irc,qlFile(iql),srName)

    DO                              ! Loop over all lines of the QL file
      READ(lfn002,'(A)',iostat=irc) line ! Read next line

    ! Error while reading
      IF (irc > 0) THEN
        WRITE(lfnerr,'(2(/,A),/,17X,A,/)')                                 &
          ' ### SR QLRDFIL: Error while reading SLR quick look file. ',    &
          '                 Some data may be lost. Please check the file:',&
                            TRIM(qlFile(iql))
        CYCLE                       ! Read next line
      ENDIF

    ! Found EOF, exit loop
      IF (irc == -1) EXIT

    ! Found next station record
      IF (line == '99999') THEN

        READ(lfn002,'(A)',iostat=irc) staline ! Read station header line

        IF (irc > 0) THEN           ! Error while reading
          WRITE(lfnerr,'(2(/,A),/,17X,A,/)')                                 &
            ' ### SR QLRDFIL: Error while reading SLR quick look file. ',    &
            '                 Some data may be lost. Please check the file:',&
                              TRIM(qlFile(iql))
          CYCLE                     ! Read next line
        ENDIF
        IF (irc == -1 .OR. staline(1:5) == '99999') THEN
          BACKSPACE(lfn002)
          CYCLE
        ENDIF

        cpStaline = .TRUE.

      ! Check satellite
        satok = .FALSE.
        DO isat = 1, nsat
          IF (staline(1:7) == ilrs(isat)) THEN
            satok = .TRUE.
            EXIT
          ENDIF
        ENDDO
        IF (.NOT. satok) CYCLE      ! No satellite found, go to next line

      ! Check time
        READ(staline(8:12),'(I2,I3)',iostat=irc) year2, doy

        IF (irc /= 0) THEN           ! Error while reading
          WRITE(lfnerr,'(2(/,A),/,17X,A,/)')                                 &
            ' ### SR QLRDFIL: Error while reading SLR quick look file. ',    &
            '                 Some data may be lost. Please check the file:',&
                              TRIM(qlFile(iql))
          CYCLE                       ! Read next line
        ENDIF

        year = iyear4(year2)
        tjuldy = djul(year,1,1d0) + doy - 1
        IF (tjuldy >= window(2)) CYCLE ! No matching epoch found, go to next line
        ifepo = 1

      ! Update staList
        stanam = staline(13:16)
        ipos   = listc1(1,4,maxsta,staList,stanam,nlist)

        IF (ipos == 0) THEN         ! Too many stations, error
          WRITE(lfnerr,'(2(A,/),A,I4,/,A,A)')                        &
            ' *** SR QLRDFIL: Maximum dimension exceeded.',          &
            '                 Too many station names found.',        &
            '                 Maximum dimension: ', maxsta,          &
            '                 Increase maximum dimension "maxsta" ', &
                             'or reduce number of input files.'
          CALL exitrc(2)
        ENDIF


        DO                          ! Loop over all lines for station stanam

          READ(lfn002,'(A)',iostat=irc) line
          IF (irc /= 0 .OR. line(1:5) == '99999') THEN
            BACKSPACE(lfn002)
            EXIT
          ENDIF

        ! Time of day of laser firing from 0h UTC (sec, microsec)
          READ(line(1:12),'(I5,I7)',iostat=irc) sec, secfr

          IF (irc /= 0) THEN        ! Error while reading
            WRITE(lfnerr,'(2(/,A),/,17X,A,/)')                                 &
              ' ### SR QLRDFIL: Error while reading SLR quick look file. ',    &
              '                 Some data may be lost. Please check the file:',&
                                TRIM(qlFile(iql))
            CYCLE                       ! Read next line
          ENDIF

        ! Get leap second, compute time
          gpsutc = dgpsut(tjuldy)
          tjul   = tjuldy + (sec+gpsutc)/86400d0 ! mjd

          IF (ifepo == 1) THEN
            tjulv = tjul
            ifepo = 0
          END IF

          IF (tjul-tjulv < -0.5D0) tjul = tjul + 1D0
          tjulv = tjul

          IF (tjul < window(1) .OR. tjul > window(2))  CYCLE ! No matching epoch
                                                             ! found, go to next line

          rsec = tjul*86400d0

        ! Copy "station header" line to scratch
          IF (cpStaline) THEN
            WRITE(lfn001,'(A,/,A)')'99999',staline
            cpStaline = .FALSE.
          ENDIF

        ! Copy line to scratch
          WRITE(lfn001,'(F14.1,1X,I7,1X,A)')rsec,secfr,line(13:LEN_TRIM(line))

        ! Count number of records per station
          numrec(ipos) = numrec(ipos) + 1
          sumrec       = sumrec + 1

        ENDDO                       ! End loop over all lines for station stanam

      ENDIF                         ! End of records for station stanam

    ENDDO                           ! End loop over all lines of the QL file

    CLOSE(lfn002)                   ! Close QL file

  ENDDO                             ! End loop over all QL files

  CLOSE(lfn001)                     ! Close scratch file
  DEALLOCATE(ilrs,stat=iac)
  DEALLOCATE(qlFile,stat=iac)       ! Deallocate array of QL file names

! Stop program if no stations found
! ---------------------------------
  IF (sumrec == 0) THEN
    WRITE(lfnerr,'(A,/,18X,A)') &
      ' ### SR QLRDFIL: No stations found in requested time interval: ',tstrng

    CALL exitrc(0)
  ENDIF

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

  RETURN

END SUBROUTINE qlrdfil

END MODULE
