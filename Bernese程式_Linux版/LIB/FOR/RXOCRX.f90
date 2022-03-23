MODULE s_RXOCRX
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rxocrx(staInfo,flgStr,timint,rxfile,stName,irCode)

! -------------------------------------------------------------------------
! Purpose:    Do not write Bernese file for stations with problems over the
!             entire data time interval or equipment/antenna pos. change in
!             station info file
!
! Author:     R. Dach
!
! Created:    07-May-2003
!
! Changes:    11-Jun-2003 RD: Trim filenames for output
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             03-Jun-2009 SL: Check station information (type 002)
!             19-Jul-2010 EO: Allow the writting of files for sta with
!                             problems not covering the entire data span
!             26-Oct-2010 SL: use m_bern with ONLY, removal of unused pars
!
! SR called:  timst2, gtflna
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnErr, &
                      fileNameLength, staNameLength, timStrgLength
  USE m_time,   ONLY: t_timint
  USE d_stacrx, ONLY: t_stacrux

  USE s_timst2
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  TYPE(t_staCrux)              :: staInfo ! staInfo: entries for Bernese
  CHARACTER(LEN=*),DIMENSION(:):: flgStr  ! String with flags for staInfo
  TYPE(t_timint)               :: timint  ! Data time interval
  CHARACTER(LEN=fileNameLength):: rxFile  ! Name of the RINEX file
  CHARACTER(LEN=staNameLength) :: stName  ! Station name

! output:
  INTEGER(i4b)                 :: irCode  ! Return code
                                          ! 0: OK
                                          ! 2: warning, do not write file

! List of funtions
! ----------------

! Local types
! -----------

! Local parameters
! ----------------
  REAL(r8b),        PARAMETER   :: dtSim  = 1d0/86400d0
  REAL(r8b),        PARAMETER   :: dtSim2 = 0.5d0/86400d0

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength) :: staFil
  CHARACTER(LEN=timStrgLength)  :: timstr1,timstr2

  INTEGER(i4b)                  :: iProb,ii,jj
  INTEGER(i4b)                  :: irc

! Init variable
! -------------
  irCode = 0

  CALL gtflna(0,'STAINFO',staFil,irc)
  IF (irc /= 0 .OR. LEN_TRIM(staFil) == 0) staFil = '---'

! Check station name
! ------------------
  DO iProb = 1,staInfo%nProb

    ! Check station name
    IF (stName /= staInfo%staProb(iProb)%stanam) CYCLE

    ! Check time interval
    IF (timint%t(1) > staInfo%staProb(iProb)%timint%t(1) - dtsim2 .AND. &
        timint%t(2) < staInfo%staProb(iProb)%timint%t(2) + dtsim2) THEN

        CALL timst2(1,1,timint%t(1),timstr1)
        CALL timst2(1,1,timint%t(2),timstr2)

        WRITE(lfnerr,'(/,A,A/,16X,A/,16X,A,A)')                           &
             ' ### SR RXOCRX: Station with a problem over data interval ', &
             '(type 003) ',                                                &
             'Bernese obs. files will not be written.',                    &
             'Sta info file: ',TRIM(staFil)
        IF (flgStr(3) == '999') THEN
          WRITE(lfnerr,'(16X,A)')    'List of flags: all entries'
        ELSE
          WRITE(lfnerr,'(16X,A,A)')  'List of flags: ',TRIM(flgStr(3))
        ENDIF
        WRITE(lfnerr,'(4(16X,A,A,/))')                                   &
             'Rinex file   : ',TRIM(rxfile),                             &
             'Station name : ',TRIM(stName),                             &
             'First epoch  : ',TRIM(timstr1),                            &
             'Last epoch   : ',TRIM(timstr2)

        irCode = 2
        EXIT
      ENDIF
  ENDDO


! Check station information (type 002)
! ------------------------------------
  DO ii = 1,staInfo%nInfo

    ! Check station name
    IF (stName /= staInfo%staInfo(ii)%stanam) CYCLE

    ! Check time interval
    IF (timint%t(1)-dtSim < staInfo%staInfo(ii)%timint%t(2) .AND. &
        timint%t(2)+dtSim > staInfo%staInfo(ii)%timint%t(2)) THEN

      DO jj = 1,staInfo%nInfo

        IF (stName /= staInfo%staInfo(jj)%stanam) CYCLE
        IF (timint%t(1)-dtSim > staInfo%staInfo(jj)%timint%t(1) .OR. &
            timint%t(2)+dtSim < staInfo%staInfo(jj)%timint%t(1)) CYCLE

        IF ( &
          staInfo%staInfo(ii)%recnam == staInfo%staInfo(jj)%recnam .AND. &
          staInfo%staInfo(ii)%antnam == staInfo%staInfo(jj)%antnam .AND. &
          staInfo%staInfo(ii)%recnum == staInfo%staInfo(jj)%recnum .AND. &
          staInfo%staInfo(ii)%antnum == staInfo%staInfo(jj)%antnum .AND. &
          staInfo%staInfo(ii)%antecc(1) == staInfo%staInfo(jj)%antecc(1) .AND.&
          staInfo%staInfo(ii)%antecc(2) == staInfo%staInfo(jj)%antecc(2) .AND.&
          staInfo%staInfo(ii)%antecc(3) == staInfo%staInfo(jj)%antecc(3)) CYCLE

        CALL timst2(1,1,timint%t(1),timstr1)
        CALL timst2(1,1,timint%t(2),timstr2)

        WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A)')                                &
          ' ### SR RXOCRX: Equipment or antenna pos. change found (type 002)', &
                          'Bernese obs. files will not be written.',           &
                          'Sta info file: ',TRIM(staFil)

        IF (flgStr(2) == '999') THEN
          WRITE(lfnerr,'(16X,A)')    'List of flags: all entries'
        ELSE
          WRITE(lfnerr,'(16X,A,A)')  'List of flags: ',TRIM(flgStr(2))
        ENDIF

        WRITE(lfnerr,'(4(16X,A,A,/))')     &
          'Rinex file   : ',TRIM(rxfile),  &
          'Station name : ',TRIM(stName),  &
          'First epoch  : ',TRIM(timstr1), &
          'Last epoch   : ',TRIM(timstr2)

        irCode = 2

        EXIT

      ENDDO

    ENDIF

  ENDDO

  RETURN

END SUBROUTINE rxocrx

END MODULE
