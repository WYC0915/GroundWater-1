MODULE s_CRDGTREC
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE crdgtrec (lfnscr,window,stanam,nrec,qlobs)

! -------------------------------------------------------------------------
! Purpose:    Get info and observation records for station stanam from
!             scratch file (CRD files are read previously by SR CRDRDFIL)
!
!             based on QLGTREC.f90
!
! Author:     M. Heinze
!
! Created:    20-Jan-2009
!
! Changes:    21-Jan-2009 MH: Checks if time for meteo record is "identical"
!                             as for obs added
!             22-Jan-2009 MH: Problem: Day may change during one pass => solved
!             05-Feb-2009 HB: dreal replaced with *1.D0
!             18-Jun-2010 DT: Read from Scratch file (not again from CRD file)
!             19-Jan-2011 DT: M_BERN with ONLY
!             26-Oct-2011 DT: Deallocation added
!             12-Apr-2012 DT: Consider that qlobs%iwlfac is INT at 0.1nm level
!             12-Apr-2012 DT: Skip value_origin of meteo records
!                             (often missing in STL3 data)
!             22-Jun-2012 DT: No epoch test needed (is already done in
!                             CRDRDFIL)
!             19-Sep-2012 RD: Nullify all pointers, remove unused variables
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, keyValueLength, fileNameLength80, linelength, &
                      lfnerr, lfn002
  USE d_const,  ONLY: c
  USE d_qlfil,  ONLY: t_qlobs

  USE s_alcerr
!!!  USE f_nextline
  USE s_cos2prn
  USE s_slr2cos
  USE s_readkeys
  USE s_exitrc
  USE f_djul
  USE f_dgpsut
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                   :: lfnscr ! Scratch file
  REAL(r8b), DIMENSION(2)        :: window ! Time window
  CHARACTER(LEN=4)               :: stanam ! Station name
  INTEGER(i4b)                   :: nrec   ! # of observation records for stanam

! output:
  TYPE(t_qlobs)                  :: qlobs  ! Station information and
                                           ! observations for stanam

! Local Variables
! ---------------
  INTEGER(i4b)                   :: irc
  INTEGER(i4b)                   :: irec
  INTEGER(i4b)                   :: newpas
  INTEGER(i4b)                   :: iobstp
  CHARACTER(LEN=lineLength)      :: line
  CHARACTER(LEN=9)               :: slrcos, cospar

  INTEGER(i4b)        :: nql,iql,iac,n_arg,i,ii
  INTEGER(i4b)        :: cdp_pad_id,cdp_sys_num,cdp_occ_num,stn_timescale,ilrs_id
  INTEGER(i4b)        :: detail,value_origin,num_ranges,data_release,data_qual_alert,gpsutc
  INTEGER(i4b)        :: sys_change_id,sys_config_id,year,month,day,year2,month2,day2

  REAL(r8b)           :: prev_epoch
  REAL(r8b)           :: wavel,rectim,mjd,d11_sec_of_day, d11_time_of_flight
  REAL(r8b)           :: d20_sec_of_day, pressure, temperature, humidity

  CHARACTER(LEN=keyValueLength),   DIMENSION(:), POINTER     :: keyValue
  CHARACTER(LEN=fileNameLength80), DIMENSION(:), ALLOCATABLE :: qlFile
  CHARACTER(LEN=8), PARAMETER        :: srName = 'CRDGTREC'
  CHARACTER(LEN=4)                   :: cdp_pad_nam
  CHARACTER(LEN=256),DIMENSION(15)   :: temp_config_id
  CHARACTER(LEN=256)                 :: sysconfig_id

  LOGICAL                            :: in_arg

! Init
! ----
  NULLIFY(keyValue)
  irec            = 0
  qlobs%iwlfac(:) = 0

! Allocate array (SIZE= # observation records for station istat)
! --------------
  IF (ASSOCIATED(qlobs%qlrec)) DEALLOCATE(qlobs%qlrec,stat=irc)
  ALLOCATE(qlobs%qlrec(nrec),stat=irc)
  CALL alcerr(irc,'qlobs%qlrec',(/nrec/),'CRDGTREC')

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

! Read from Scratch file
! ----------------------
  ! Loop over all lines
  DO
!    line = nextline(lfnscr,0)   ! Read next line
!!!    READ(lfn001,'(A)',iostat=irc) line
    READ(lfnscr,'(A)',iostat=irc) line

! Found EOF, exit loop
      IF (irc == -1) EXIT

! Error while reading
      IF (irc > 0) THEN
        WRITE(lfnerr,'(3(/,A),/,17X,A,/)')                               &
          ' ### SR QLRDFIL: Error while reading SLR quick look file. ',  &
          '                 Some data may be lost.',                     &
          '                 Please check the station:',                  &
                            stanam
        CYCLE                       ! Read next line
      ENDIF

! Station Header H2
! #################
      IF (line(1:2) == 'h2' .OR. line(1:2) == 'H2') THEN
        READ(line(15:27),1001,err=101) cdp_pad_id, cdp_sys_num,     &
             cdp_occ_num, stn_timescale
1001    FORMAT (i4,1x,i2,1x,i2,1x,i2)
        READ(line(15:18),'(A4)',err=101) cdp_pad_nam
! If new station: delete meteo-values from previous station and set flag for new pass
        newpas=1
        pressure=0
        temperature=0
        humidity=0
        IF(stanam==cdp_pad_nam)THEN
          qlobs%cdps =cdp_sys_num
          qlobs%cdpo =cdp_occ_num
          qlobs%timsy=stn_timescale
        ENDIF
        CYCLE

! Target Header H3
! ################
      ELSEIF (line(1:2) == 'h3' .OR. line(1:2) == 'H3') THEN
        READ (line(15:22),1002,err=102) ilrs_id
1002    FORMAT (i8)
        CYCLE

! Session/Pass Header H4
! ######################
      ELSEIF (line(1:2) == 'h4' .OR. line(1:2) == 'H4') THEN
        READ (line(1:62),1003,err=103) year,month,day,year2,month2,day2, &
                                       data_release,data_qual_alert
1003    FORMAT (6x,i4,2(1x,i2),10x,i4,2(1x,i2),10x,i2,13x,i1)
        newpas=1
        mjd = djul(year,month,day*1.D0)
        CYCLE

! System Configuration Record C0
! ##############################
      ELSEIF (line(1:2) == 'c0' .OR. line(1:2) == 'C0') THEN
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
        do i=1,5
          temp_config_id(i)= ""
        enddo

!  Read the correct number of parameters
        if (n_arg .eq. 4) then
          READ (line(3:120),*,err=105)             &
              detail, wavel,                       &
              temp_config_id(1)
        elseif (n_arg .eq. 5) then
          READ (line(3:120),*,err=105)             &
              detail, wavel,                       &
              temp_config_id(1),temp_config_id(2)
        elseif (n_arg .eq. 6) then
          READ (line(3:120),*,err=105)             &
              detail, wavel,                       &
              temp_config_id(1),temp_config_id(2), &
              temp_config_id(3)
        elseif (n_arg .eq. 7) then
          READ (line(3:120),*,err=105)             &
              detail, wavel,                       &
              temp_config_id(1),temp_config_id(2), &
              temp_config_id(3),temp_config_id(4)
        elseif (n_arg .eq. 8) then
          READ (line(3:120),*,err=105)             &
              detail, wavel,                       &
              temp_config_id(1),temp_config_id(2), &
              temp_config_id(3),temp_config_id(4), &
              temp_config_id(5)
        endif
! set wavel to qlobs%iwlfac
        IF(stanam==cdp_pad_nam)THEN
          IF(wavel.GT.400.AND.wavel.LT.600) THEN

            IF(qlobs%iwlfac(1).EQ.0 .OR. qlobs%iwlfac(1).EQ.INT(wavel*10d0)) THEN
              qlobs%iwlfac(1)=wavel*10.D0
              iobstp=1
            ELSE
              WRITE(lfnerr,'(A,/,2A,3(/,A),F5.1,A,/)')                      &
                 ' ### SR CRDGTREC: 2 first  wavelengths found.',           &
                 '                  Criteria for wavelength assignment',    &
                                                      ' (hardwired): ',     &
                 '                    400 nm < First  WL < 600nm',          &
                 '                    600 nm < Second WL > 400nm',          &
                 '                 Write wavelength: ',wavel,' as second wavelength.'
              qlobs%iwlfac(2) = wavel*10.D0
              iobstp          = 2
            ENDIF

          ELSE
            IF(qlobs%iwlfac(2).EQ.0.OR.qlobs%iwlfac(2).EQ.INT(wavel*10d0)) THEN
              qlobs%iwlfac(2)=wavel*10.D0
              iobstp=2
            ELSE
              WRITE(lfnerr,'(A,/,2A,3(/,A),F5.1,A,/)')                      &
                 ' ### SR CRDGTREC: 2 second wavelengths found.',           &
                 '                  Criteria for wavelength assignment',    &
                                                      ' (hardwired): ',     &
                 '                    400 nm < First  WL < 600nm',          &
                 '                    600 nm < Second WL > 400nm',          &
                 '                 Write wavelength: ',wavel,' as first  wavelength.'
              qlobs%iwlfac(1) = wavel*10.D0
              iobstp          = 1
            ENDIF
          ENDIF
        ENDIF

        CYCLE


! Range Record (Normal Point) 11
! ##############################
      ELSEIF (line(1:2) == '11') THEN
! Store old observation epoch to check for day switch
        prev_epoch=d11_sec_of_day
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
             '### SR CRDGTREC: More than 16 records in Record Type 11:',line
        elseif (n_arg .lt. 7) then
          write(lfnerr,'(/,A,/,5X,A,/)')                                &
             '### SR CRDGTREC: Less than  7 records in Record Type 11:',line
        endif

        IF(stanam==cdp_pad_nam)THEN

! Check if day boundary is crossed
! Observation records must be sorted, otherwise everything gets messed up!
! (should already be done in CRDRDFIL)
          IF(newpas.NE.1.AND.d11_sec_of_day-prev_epoch.LT.0)THEN
            write(lfnerr,'(/,/,A,/,A,A4,/,A,F14.6,/,A,F14.6,/,A,/,/)')      &
               ' ### SR CRDGTREC: Problems with day boundary',              &
               '        Station name    : ',cdp_pad_nam,                    &
               '        Previous Record : ',mjd+(prev_epoch    )/86400d0,   &
               '        Actual Record   : ',mjd+(d11_sec_of_day)/86400d0,   &
               '        Please check original dataset. Now 1 day is added!'
            mjd=mjd+1
          ENDIF

! Get GPSUTC and add it to receiving epoch
          gpsutc = dgpsut(mjd)
          rectim = d11_sec_of_day + d11_time_of_flight + gpsutc

            irec=irec+1

            qlobs%qlrec(irec)%epoch(1)= mjd +  nint(rectim)/86400d0
            qlobs%qlrec(irec)%epoch(2)= rectim-nint(rectim)
            qlobs%qlrec(irec)%iobstp  = iobstp
            qlobs%qlrec(irec)%range   = 0.5*d11_time_of_flight*C
            qlobs%qlrec(irec)%numnpt  = num_ranges
            qlobs%qlrec(irec)%llinpt  = newpas
            newpas = 0
! Translate satellite numbers
            WRITE(slrcos,'(I7.7)') ilrs_id
            CALL slr2cos(slrcos,cospar)
            CALL cos2prn(2,cospar,qlobs%qlrec(irec)%epoch(1),qlobs%qlrec(irec)%satnr,irc)
            IF(pressure.NE.0.AND.temperature.NE.0.AND.humidity.NE.0.AND.qlobs%qlrec(irec)%llinpt.EQ.0) THEN
              qlobs%qlrec(irec)%press   = pressure
              qlobs%qlrec(irec)%temp    = temperature-273.16d0
              qlobs%qlrec(irec)%humid   = humidity
            ENDIF

        ENDIF
        CYCLE

! Meteorological Record 20
! ########################
      ELSEIF (line(1:2) == '20') THEN

!!!        READ (line(3:),*,err=120) d20_sec_of_day, &
!!!             pressure,temperature, &
!!!             humidity, value_origin
!!! Skip value_origin due to missing entries in STL3 data:
        READ (line(3:),*,err=120)  &
             d20_sec_of_day, pressure, temperature, humidity

        IF(stanam==cdp_pad_nam)THEN
! CASE1: New pass, new meteo record 20
! CASE1a: First meteo record 20, afterwards range records 11
          IF(newpas.EQ.1 .AND. irec<nrec) THEN
            qlobs%qlrec(irec+1)%press   = pressure
            qlobs%qlrec(irec+1)%temp    = temperature-273.16d0
            qlobs%qlrec(irec+1)%humid   = humidity
! CASE1b: Meteo record 20 after first range record 11
          ELSEIF(qlobs%qlrec(irec)%llinpt.EQ.1.AND.newpas.EQ.0) THEN
            qlobs%qlrec(irec  )%press   = pressure
            qlobs%qlrec(irec  )%temp    = temperature-273.16d0
            qlobs%qlrec(irec  )%humid   = humidity
! CASE2: No new pass, but meteo record 20 changed/available
          ELSEIF(qlobs%qlrec(irec)%llinpt.EQ.0.AND.newpas.EQ.0) THEN
! CASE2a: Meteo record 20 after corresponding range record 11
            IF((d20_sec_of_day-d11_sec_of_day).LT.1.D0) THEN
              qlobs%qlrec(irec)%press   = pressure
              qlobs%qlrec(irec)%temp    = temperature-273.16d0
              qlobs%qlrec(irec)%humid   = humidity
! CASE2b: Meteo record 20 before corresponding range record 11
!         => Done during range record 11 reading
            ENDIF
          ENDIF
        ENDIF
        CYCLE

! Compatibility Record 60
! #######################
      ELSEIF (line(1:2) == '60') THEN
        READ (line(3:),*,err=160) sysconfig_id, &
                     sys_change_id, sys_config_id
        IF(stanam==cdp_pad_nam)THEN
          qlobs%syscf=sys_config_id
          qlobs%revnr=1
        ENDIF
        CYCLE

! End of Header Record H9
! #######################
      ELSEIF (line(1:2) == 'h9' .OR. line(1:2) == 'H9') THEN
        CLOSE(lfn002)
        EXIT
      ENDIF

  ENDDO  ! Lines of CRD file

  REWIND(lfnscr)


! Deallocation
! ------------
  DEALLOCATE (qlFile, stat=irc)


  RETURN

101  WRITE(lfnerr,*) "SR CRDGTREC: Error reading CRD record type h2"
     CALL EXITRC(2)
102  WRITE(lfnerr,*) "SR CRDGTREC: Error reading CRD record type h3"
     CALL EXITRC(2)
103  WRITE(lfnerr,*) "SR CRDGTREC: Error reading CRD record type h4"
     CALL EXITRC(2)
105  WRITE(lfnerr,*) "SR CRDGTREC: Error reading CRD record type c0"
     CALL EXITRC(2)
111  WRITE(lfnerr,*) "SR CRDGTREC: Error reading CRD record type 11"
     CALL EXITRC(2)
120  WRITE(lfnerr,*) "SR CRDGTREC: Error reading CRD record type 20"
     CALL EXITRC(2)
160  WRITE(lfnerr,*) "SR CRDGTREC: Error reading CRD record type 60"
     CALL EXITRC(2)

END SUBROUTINE crdgtrec

END MODULE
