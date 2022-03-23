MODULE s_CRDRDFIL
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE crdrdfil (window,satlst,scrfil,staList,nlist,numrec)

! -------------------------------------------------------------------------
! Purpose:    Read CRD normal point files, get station names and number of
!             observation records within the observation window
!             -> Copy to scratch file
!
!             based on QLRDFIL.f90
!
! Author:     M. Heinze
!
! Created:    20-Jan-2009
!
! Changes:    23-Jan-2009 MH: Convert gpsutc to mjd
!             05-Feb-2009 HB: dreal replaced with *1.D0
!             18-Jun-2010 DT: Check satellite ID;
!                             Copy relevant lines to scratch file
!             30-May-2012 RD: Use LISTC1 as module now, use m_bern with only
!             13-Jun-2012 DT: Bugfix for meteo data if day-switch within pass
!             22-Jun-2012 DT: mjdepo corrected with time-of-flight
!             19-Sep-2012 RD: Correctly deallocate all arrays
!             19-Sep-2012 RD: gpsutc is real instead of integer
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnprt, lfnerr, lfn001, lfn002, &
                      keyValueLength, fileNameLength80, timstrglength2, &
                      lineLength
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
  CHARACTER(LEN=8), PARAMETER             :: srName = 'CRDRDFIL'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength),   DIMENSION(:), POINTER     :: keyValue
  CHARACTER(LEN=fileNameLength80), DIMENSION(:), ALLOCATABLE :: qlFile
  CHARACTER(LEN=7),                DIMENSION(:), ALLOCATABLE :: ilrs
  CHARACTER(LEN=lineLength)               :: line
  CHARACTER(LEN=lineLength)               :: line_sta
  CHARACTER(LEN=4)                        :: stanam
  CHARACTER(LEN=timStrgLength2)           :: tstrng
  CHARACTER(LEN=9)                        :: cospar

  INTEGER(i4b)                            :: iql, nql
  INTEGER(i4b)                            :: isat
  INTEGER(i4b)                            :: ipos
  INTEGER(i4b)                            :: irc,iac
  INTEGER(i4b)                            :: nsat
  INTEGER(i4b)                            :: numsat
  INTEGER(i4b)                            :: sumrec

  CHARACTER(LEN=256),DIMENSION(15) :: temp_config_id
  CHARACTER(LEN=3)    :: crd_literal
  CHARACTER(LEN=8)    :: ilrs_id

  INTEGER(i4b)        :: format_version,cdp_pad_id,cdp_sys_num,cdp_occ_num,stn_timescale
  INTEGER(i4b)        :: data_release,data_qual_alert,n_arg,i,ii
  INTEGER(i4b)        :: num_ranges
  INTEGER(i4b)        :: year,month,day,hour,minute,second
  INTEGER(i4b)        :: newpass

  REAL(r8b)           :: d11_sec_of_day, d11_time_of_flight
  REAL(r8b)           :: gpsutc
  REAL(r8b)           :: mjd,mjdepo
  REAL(r8b)           :: addDay
  REAL(r8b)           :: prev_epoch

  LOGICAL             :: in_arg
  LOGICAL             :: satOK = .FALSE.
  LOGICAL             :: timOK = .FALSE.

! Init
! ----
  NULLIFY(keyValue)
  nlist     = 0
  numrec(:) = 0
  sumrec    = 0
  staList(:)= ""
  newpass   = 0

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
    ' *** SR CRDRDFIL: A problem appeared while reading the filenames of ',&
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

! Read records from CRD file, extract station names & number of records
! ---------------------------------------------------------------------
  DO iql = 1, nql                   ! Loop over all CRD files

  ! Open QL file
    CALL opnfil(lfn002,qlFile(iql),'OLD','FORMATTED','READONLY',' ',irc )
    CALL opnerr(lfnerr,lfn002,irc,qlFile(iql),srName)

    DO                              ! Loop over all lines of the CRD file
      READ(lfn002,'(A)',iostat=irc) line ! Read next line

    ! Error while reading
      IF (irc > 0) THEN
        WRITE(lfnerr,'(2(/,A),/,17X,A,/)')                                 &
          ' ### SR CRDRDFIL: Error while reading SLR CRD file. ',          &
          '                 Some data may be lost. Please check the file:',&
                            TRIM(qlFile(iql))
        CYCLE                       ! Read next line
      ENDIF

    ! Found EOF, exit loop
      IF (irc == -1) EXIT

! Format Header H1
! ################
      IF (line(1:2) == 'h1' .OR. line(1:2) == 'H1') THEN
        READ(line(4:10),1000,err=100) crd_literal, format_version
1000    FORMAT (a3,1x,i2)
        IF(crd_literal.NE.'CRD'.AND.crd_literal.NE.'crd') THEN
          WRITE(lfnerr,'(/,A,A,/,A,A,A,/,/)')                       &
             ' ### SR CRDRDFIL: Error in file : ',TRIM(qlFile(iql)), &
             '                 Record: ', crd_literal,              &
             ' should be `crd` or `CRD`. '
          CALL EXITRC(2)
        ENDIF
      ENDIF

! Station Header H2
! #################
      IF (line(1:2) == 'h2' .OR. line(1:2) == 'H2') THEN
        READ(line(15:27),1001,err=101) cdp_pad_id, cdp_sys_num,     &
             cdp_occ_num, stn_timescale
1001    FORMAT (i4,1x,i2,1x,i2,1x,i2)
        READ(line(15:18),'(A4)',err=101) stanam
        ipos   = listc1(1,4,maxsta,staList,stanam,nlist)
!       numrec(ipos)=0

        line_sta = line
      ENDIF

! Target Header H3
! ################
      IF (line(1:2) == 'h3' .OR. line(1:2) == 'H3') THEN
!!        READ (line(15:22),1002,err=102) ilrs_id
!! 1002    FORMAT (i8)
        READ (line(15:22),1002,err=102) ilrs_id
1002    FORMAT (a8)

      ! Check satellite
      ! ---------------
        satOK = .FALSE.
        DO isat = 1, nsat
          IF ( ilrs_id(2:8) == ilrs(isat) ) THEN
             satOK = .TRUE.
             EXIT
          END IF
        END DO
        IF ( .NOT. satOK ) CYCLE

      ! Copy station and satellite line to scratch
      ! ------------------------------------------
        WRITE(lfn001,'(A,/,A,/,A)') 'H8',                           &
                                    line_sta(1:LEN_TRIM(line_sta)), &
                                    line(1:LEN_TRIM(line))

      ENDIF

! Session/Pass Header H4
! ######################
      IF (line(1:2) == 'h4' .OR. line(1:2) == 'H4') THEN
        READ (line(1:62),1003,err=103) year,month,day,hour,minute,second, &
                                       data_release,data_qual_alert
1003    FORMAT (6x,i4,5(1x,i2),21x,i2,13x,i1)
        mjd = djul(year,month,day*1.D0)
        gpsutc = dgpsut(mjd)

        newpass = 1
        addDay  = 0d0
        timOK   = .TRUE.

       ! Copy to scratch
       ! ----------------
        IF ( satOK ) WRITE(lfn001,'(A)') line(1:LEN_TRIM(line))

      ENDIF

! Range Record (Normal Point) 11
! ##############################
      IF (line(1:2) == '11') THEN

       ! Store old observation epoch to check for day switch
        prev_epoch = d11_sec_of_day

        n_arg= 0
        do ii= 1,len(line)
          if (line(ii:ii) .ne. " ") then
            if (.not.in_arg) then
               in_arg= .true.
            endif
          else
            if (in_arg .eqv. .true.) then
               in_arg= .false.
               n_arg= n_arg+ 1;
            endif
          endif
        enddo

!  Create default values
        do i=1,15
          temp_config_id(i)= ""
        enddo

!  Read the correct number of parameters
        if (n_arg .eq. 7) then
          READ (line(3:120),*,err=111)                                  &
              d11_sec_of_day,    d11_time_of_flight,temp_config_id( 3), &
              temp_config_id( 4),temp_config_id( 5),num_ranges
        elseif (n_arg .eq. 8) then
          READ (line(3:120),*,err=111)                                  &
              d11_sec_of_day,    d11_time_of_flight,temp_config_id( 3), &
              temp_config_id( 4),temp_config_id( 5),num_ranges,         &
              temp_config_id( 7)
        elseif (n_arg .eq. 9) then
          READ (line(3:120),*,err=111)                                  &
              d11_sec_of_day,    d11_time_of_flight,temp_config_id( 3), &
              temp_config_id( 4),temp_config_id( 5),num_ranges,         &
              temp_config_id( 7),temp_config_id( 8)
        elseif (n_arg .eq.10) then
          READ (line(3:120),*,err=111)                                  &
              d11_sec_of_day,    d11_time_of_flight,temp_config_id( 3), &
              temp_config_id( 4),temp_config_id( 5),num_ranges,         &
              temp_config_id( 7),temp_config_id( 8),temp_config_id( 9)
        elseif (n_arg .eq.11) then
          READ (line(3:120),*,err=111)                                  &
              d11_sec_of_day,    d11_time_of_flight,temp_config_id( 3), &
              temp_config_id( 4),temp_config_id( 5),num_ranges,         &
              temp_config_id( 7),temp_config_id( 8),temp_config_id( 9), &
              temp_config_id(10)
        elseif (n_arg .eq.12) then
          READ (line(3:120),*,err=111)                                  &
              d11_sec_of_day,    d11_time_of_flight,temp_config_id( 3), &
              temp_config_id( 4),temp_config_id( 5),num_ranges,         &
              temp_config_id( 7),temp_config_id( 8),temp_config_id( 9), &
              temp_config_id(10),temp_config_id(11)
        elseif (n_arg .eq.13) then
          READ (line(3:120),*,err=111)                                  &
              d11_sec_of_day,    d11_time_of_flight,temp_config_id( 3), &
              temp_config_id( 4),temp_config_id( 5),num_ranges,         &
              temp_config_id( 7),temp_config_id( 8),temp_config_id( 9), &
              temp_config_id(10),temp_config_id(11),temp_config_id(12)
        elseif (n_arg .eq.14) then
          READ (line(3:120),*,err=111)                                  &
              d11_sec_of_day,    d11_time_of_flight,temp_config_id( 3), &
              temp_config_id( 4),temp_config_id( 5),num_ranges,         &
              temp_config_id( 7),temp_config_id( 8),temp_config_id( 9), &
              temp_config_id(10),temp_config_id(11),temp_config_id(12), &
              temp_config_id(13)
        elseif (n_arg .eq.15) then
          READ (line(3:120),*,err=111)                                  &
              d11_sec_of_day,    d11_time_of_flight,temp_config_id( 3), &
              temp_config_id( 4),temp_config_id( 5),num_ranges,         &
              temp_config_id( 7),temp_config_id( 8),temp_config_id( 9), &
              temp_config_id(10),temp_config_id(11),temp_config_id(12), &
              temp_config_id(13),temp_config_id(14)
        elseif (n_arg .eq.16) then
          READ (line(3:120),*,err=111)                                  &
              d11_sec_of_day,    d11_time_of_flight,temp_config_id( 3), &
              temp_config_id( 4),temp_config_id( 5),num_ranges,         &
              temp_config_id( 7),temp_config_id( 8),temp_config_id( 9), &
              temp_config_id(10),temp_config_id(11),temp_config_id(12), &
              temp_config_id(13),temp_config_id(14),temp_config_id(15)
        elseif (n_arg .gt.16) then
          write(lfnerr,'(/,A,/,5X,A,/)')                                &
             '### SR CRDRDFIL: More than 16 records in Record Type 11:',line
        elseif (n_arg .lt. 7) then
          write(lfnerr,'(/,A,/,5X,A,/)')                                &
             '### SR CRDRDFIL: Less than  7 records in Record Type 11:',line
        endif

      ! Check for day switch within pass by comparing with previous epoch
        IF ( newpass==0 .AND. d11_sec_of_day-prev_epoch .LT. 0 ) THEN
          addDay = 1d0
        ENDIF
        newpass = 0

      ! check if time is in requested interval
      ! and if satellite was okay
!!!        mjdepo = mjd + addDay + (gpsutc+d11_sec_of_day)/86400
        mjdepo = mjd + addDay +  &
                (gpsutc+d11_sec_of_day+d11_time_of_flight) / 86400d0

        IF( mjdepo.ge.window(1).and.mjdepo.le.window(2) ) THEN
          timOK = .TRUE.

          IF ( satOK ) THEN
            numrec(ipos) = numrec(ipos)+1
            sumrec = sumrec+1
            WRITE(lfn001,'(A)') line(1:LEN_TRIM(line))
          ENDIF

        ELSE
          timOK = .FALSE.
        ENDIF

      ENDIF

! System Configuration Record c0 -> copy only to scratch
! ##############################
      IF ( (line(1:2) == 'c0' .OR. line(1:2) == 'C0') .AND. &
           satOK  .AND. timOK ) THEN
        WRITE(lfn001,'(A)') line(1:LEN_TRIM(line))
      END IF

! Meteorological Record 20 -> copy only to scratch
! ########################
      IF ( line(1:2) == '20' .AND. satOK .AND. timOK ) THEN
        WRITE(lfn001,'(A)') line(1:LEN_TRIM(line))
      END IF

! Compatibility Record 60 -> copy only to scratch
! #######################
      IF ( line(1:2) == '60' .AND. satOK  .AND. timOK ) THEN
        WRITE(lfn001,'(A)') line(1:LEN_TRIM(line))
      END IF


! End of Header Record H9
! #######################
      IF (line(1:2) == 'h9' .OR. line(1:2) == 'H9') THEN
        CLOSE(lfn002)
        EXIT
      ENDIF
    ENDDO                           ! End loop over all lines of the CRD file
    CLOSE(lfn002)                   ! Close CRD file

  ENDDO                             ! End loop over all CRD files

! Finalize scratch file
! ---------------------
  WRITE(lfn001,'(A)') 'H9'
  CLOSE(lfn001)

  DEALLOCATE(qlFile,stat=iac)       ! Deallocate array of CRD file names
  DEALLOCATE(ilrs,stat=iac)

! Stop program if no stations found
! ---------------------------------
  IF (sumrec == 0) THEN
    WRITE(lfnerr,'(A,/,18X,A)') &
      ' ### SR CRDRDFIL: No stations found in requested time interval: ',tstrng
    CLOSE(lfnprt)
    CALL exitrc(0)
  ENDIF

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

  RETURN

100  WRITE(lfnerr,*) " ### SR CRDRDFIL: Error reading CRD record type h1"
     CLOSE(lfnprt)
     CALL EXITRC(2)
101  WRITE(lfnerr,*) " ### SR CRDRDFIL: Error reading CRD record type h2"
     CLOSE(lfnprt)
     CALL EXITRC(2)
102  WRITE(lfnerr,*) " ### SR CRDRDFIL: Error reading CRD record type h3"
     CLOSE(lfnprt)
     CALL EXITRC(2)
103  WRITE(lfnerr,*) " ### SR CRDRDFIL: Error reading CRD record type h4"
     CLOSE(lfnprt)
     CALL EXITRC(2)
111  WRITE(lfnerr,*) " ### SR CRDRDFIL: Error reading CRD record type 11"
     CLOSE(lfnprt)
     CALL EXITRC(2)

END SUBROUTINE crdrdfil

END MODULE
