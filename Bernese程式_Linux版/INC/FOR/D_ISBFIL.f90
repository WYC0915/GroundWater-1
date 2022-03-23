! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_isbFil

! -------------------------------------------------------------------------
! Purpose:    Inter-system bias
!
! Author:     R. Dach
!
! Created:    22-Nov-2009
! Last mod.:  21-Sep-2010
!
! Changes:    21-Sep-2010 RD: ST2TIM can be used as a module now
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  ! Modules
  ! -------
  USE m_bern
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: t_isbFil, t_isbSta, t_isbRec, writeIsb, getIsb

  ! Structure for an individual record
  ! ----------------------------------
  TYPE t_isbRec
    REAL(r8b)                             :: epoch  ! Epoch
    REAL(r8b)                             :: isb    ! inter-system bias
    REAL(r8b)                             :: rms    ! formal error
    INTEGER(i4b)                          :: iCarr  ! Frequency
    INTEGER(i4b)                          :: meatyp ! Measurement type
                                                    ! =1: phase-only
                                                    ! =2: code-only
                                                    ! =3: code+phase
  END TYPE t_isbRec


  ! Structure for all ISB per station
  ! ---------------------------------
  TYPE t_isbSta
    CHARACTER(LEN=staNameLength)          :: staNam ! Name of the station
    INTEGER(i4b)                          :: nIsb   ! Number of records
    TYPE(t_isbRec), DIMENSION(:), POINTER :: isbRec ! inter-system bias records
  END TYPE t_isbSta

  ! Structure for the file
  ! ----------------------
  TYPE t_isbFil
    CHARACTER(LEN=fileNameLength)         :: filnam ! Name of the file
    CHARACTER(LEN=shortLineLength)        :: title  ! Title of the file
    INTEGER(i4b)                          :: nSta   ! Number of stations
    TYPE(t_isbSta), DIMENSION(:), POINTER :: isbSta ! inter-system bias listed
                                                    ! station by station
  END TYPE t_isbFil

  TYPE(t_isbFil)                          :: isb = & ! Buffer of a file
    t_isbFil('','',0,NULL())

CONTAINS


! ---------------------------------------------------------------------------
! Read a parameter to a file
! ---------------------------------------------------------------------------
  SUBROUTINE readIsb(isb)

  USE s_gtflna
  USE s_opnfil
  USE s_opnerr
  USE s_alcerr
  USE s_exitrc
  USE s_st2tim

  TYPE(t_isbFil)               :: isb

  CHARACTER(LEN=16), PARAMETER :: srName = 'd_isbFil:readIsb'

  CHARACTER(LEN=shortLineLength) :: line
  CHARACTER(LEN=timStrgLength) :: epoStr
  CHARACTER(LEN=staNameLength) :: staHlp, staOld
  INTEGER(i4b)                 :: iSta, iIsb, nIsb
  INTEGER(i4b)                 :: numSta, numIsb
  INTEGER(i4b)                 :: ios,iac

! Deallocate an old structure
! ---------------------------
  IF (isb%nSta > 0) THEN
    DO iSta = 1,isb%nSta
      DEALLOCATE(isb%isbSta(iSta)%isbRec,stat=iac)
    ENDDO
    DEALLOCATE(isb%isbSta,stat=iac)
  ENDIF

! Get the filename
! ----------------
  isb%nSta = 0
  CALL gtflna(0,'ISBINP',isb%filnam,ios)
  IF (ios /= 0) isb%filnam = ''

! Nothing to do:
  IF (LEN_TRIM(isb%filnam) == 0) RETURN

! Open the file for reading
! -------------------------
  CALL opnfil(lfnloc,isb%filnam,'OLD','FORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,isb%filnam,srName)

! Count the number of stations/records
! ------------------------------------
  READ(lfnloc,'(A,////)') isb%title

  staOld = ''
  numIsb = 0
  nIsb   = 0

  ! Read the file
  ios = 0
  DO WHILE (ios == 0)
    READ(lfnloc,'(1X,A)',iostat=ios) staHlp
    IF (ios /= 0) EXIT

    ! Next station starts
    IF (LEN_TRIM(staOld) == 0 .AND. staHlp /= staOld) THEN
      isb%nSta = isb%nSta + 1
      IF (numIsb == 0 .OR. numIsb < nIsb) numIsb = nIsb
      nIsb = 1
    ! Another record for this station
    ELSE IF (staHlp == staOld) THEN
      nIsb = nIsb + 1
    ENDIF

    staOld = staHlp

  ENDDO

! Allocate the memory
! -------------------
  ALLOCATE(isb%isbSta(isb%nSta),stat=iac)
  CALL alcerr(iac,'isb%isbSta',(/isb%nSta/),srName)


  ! Read the data
  REWIND(lfnloc)
  READ(lfnloc,'(////)')
  isb%nSta = 0
  ios = 0
  DO WHILE (ios == 0)
    READ(lfnloc,'(A)',iostat=ios) line
    IF (ios /= 0) EXIT

    ! Next station starts
    IF (LEN_TRIM(staOld) == 0 .AND. line(2:17) /= staOld) THEN
      isb%nSta = isb%nSta + 1

      ALLOCATE(isb%isbSta(isb%nSta)%isbRec(numIsb),stat=iac)
      CALL alcerr(iac,'isb%isbSta(isb%nSta)%isbRec',(/numIsb/),srName)
      isb%isbSta(isb%nSta)%nIsb = 1

    ! Another record for this station
    ELSE IF (LEN_TRIM(line(2:17)) > 0 .AND. line(2:17) == staOld) THEN
      isb%isbSta(isb%nSta)%nIsb = isb%isbSta(isb%nSta)%nIsb + 1

    ELSE
      staOld = line(2:17)
      CYCLE
    ENDIF

    ! Decode the data
    READ(line,'(1X,A,4X,3X,A,2F10.4,4X,I1)',iostat=ios)                   &
              isb%isbSta(isb%nSta)%staNam,epoStr,                         &
              isb%isbSta(isb%nSta)%isbRec(isb%isbSta(isb%nSta)%nIsb)%isb, &
              isb%isbSta(isb%nSta)%isbRec(isb%isbSta(isb%nSta)%nIsb)%rms, &
              isb%isbSta(isb%nSta)%isbRec(isb%isbSta(isb%nSta)%nIsb)%iCarr
    CALL st2tim(2,1,epoStr, &
              isb%isbSta(isb%nSta)%isbRec(isb%isbSta(isb%nSta)%nIsb)%epoch)

    ! Error reading the last record
    IF (ios /= 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,2A,/)') &
      ' *** SR d_isbFil:readIsb: Error reading the following record:', &
      TRIM(line),'Filename:   ',TRIM(isb%filnam)
      CALL exitrc(2)
    ENDIF

    staOld = isb%isbSta(isb%nSta)%staNam

  ENDDO
  CLOSE(lfnloc)

  RETURN
  END SUBROUTINE readIsb



! ---------------------------------------------------------------------------
! Write a parameter to a file
! ---------------------------------------------------------------------------
  SUBROUTINE writeIsb(isb)

  USE s_opnfil
  USE s_opnerr
  USE s_timst2

  TYPE(t_isbFil)               :: isb

  CHARACTER(LEN=17), PARAMETER :: srName = 'd_isbFil:writeIsb'

  CHARACTER(LEN=timStrgLength) :: epoStr
  INTEGER(i4b)                 :: iSta, iIsb
  INTEGER(i4b)                 :: ios

! Nothing to do
! -------------
  IF (LEN_TRIM(isb%filnam) == 0) RETURN

! Open the file for writing
! -------------------------
  CALL opnfil(lfnloc,isb%filnam,'UNKNOWN','FORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,isb%filnam,srName)

! Write the header of the file
! ----------------------------
  WRITE(lfnloc,'(A,/,A,//,A,/,A)')                        &
          TRIM(isb%title),                                &
          '----------------------------------------' //   &
          '----------------------------------------',     &
          ' Station name           Epoch           ' //   &
          '       ISBias       RMS   Frq',                &
          ' ****************       ********** *****' //   &
          '***   **.****   **.****   **'

  DO iSta = 1,isb%nSta
    DO iIsb = 1, isb%isbSta(iSTa)%nIsb
      CALL timst2(2,1,isb%isbSTa(iSta)%isbRec(iIsb)%epoch,epoStr)
      WRITE(lfnloc,'(1X,A,4X,3X,A,2F10.4,3X,A,I1)') &
           isb%isbSta(iSta)%staNam,epoStr,    &
           isb%isbSta(iSta)%isbRec(iIsb)%isb, &
           isb%isbSta(iSta)%isbRec(iIsb)%rms, &
           'L',isb%isbSta(iSta)%isbRec(iIsb)%iCarr
    ENDDO
    WRITE(lfnloc,*)
  ENDDO
  WRITE(lfnloc,*)
  close(lfnloc)

  END SUBROUTINE writeIsb


! ---------------------------------------------------------------------------
! Provide the value for a request
! ---------------------------------------------------------------------------
  FUNCTION getIsb(mode,staNam,epoch,iCarr,irCode)


  USE s_gtflna
  USE s_timst2
  USE s_exitrc

  ! input
  INTEGER(i4b)                 :: mode   ! 0: no error reporting
                                         ! 1: write warning
                                         ! 2: stop with an error
  CHARACTER(LEN=staNameLength) :: staNam ! requested station name
  REAL(r8b)                    :: epoch  ! requested epoch
  INTEGER(i4b)                 :: iCarr  ! requested frequency
  ! output
  INTEGER(i4b)                 :: irCode ! 0: value found/interpolated
                                         ! 1: station found, but epoch outside
                                         !       of the interval
                                         ! 2: station not found
  REAL(r8b)                    :: getIsb ! the isb value


! Local parameter
! ---------------
  REAL(r8b),     PARAMETER     :: dtSim = 0.1d0/86400d0

! Local variables
! ---------------
  CHARACTER(LEN=fileNameLength):: filnam
  CHARACTER(LEN=timStrgLength) :: epoStr
  CHARACTER(LEN=timStrgLength2):: timStr

  INTEGER(i4b)                 :: iSta, iIsb
  INTEGER(i4b)                 :: irc

! Do we need to read another file
! -------------------------------
  CALL gtflna(0,'ISBINP',filnam,irc)
  IF (filnam /= isb%filnam) CALL readIsb(isb)

! Search the station
! ------------------
  irCode = 2
  getIsb = 0d0
  DO iSta = 1,isb%nSta
    IF (isb%isbSta(iSta)%stanam == stanam) THEN

      ! Search for the record
      irCode = 1
      DO iIsb = 1,isb%isbSta(iSta)%nIsb-1

        IF (epoch <= isb%isbSta(iSta)%isbRec(iIsb+1)%epoch+dtSim .AND. &
            epoch >= isb%isbSta(iSta)%isbRec(iIsb)%epoch-dtSim) THEN

          ! Record found: interpolation
          irCode = 0
          getIsb = isb%isbSta(iSta)%isbRec(iIsb)%isb +   &
                  (isb%isbSta(iSta)%isbRec(iIsb+1)%isb - &
                   isb%isbSta(iSta)%isbRec(iIsb)%isb)  / &
                  (isb%isbSta(iSta)%isbRec(iIsb+1)%epoch - &
                   isb%isbSta(iSta)%isbRec(iIsb)%epoch)  * &
                  (epoch-isb%isbSta(iSta)%isbRec(iIsb)%epoch)
          EXIT
        ENDIF
      ENDDO

! Station found, but epoch not in interval of the file
! ----------------------------------------------------
      IF (irCode == 1) THEN
        CALL timst2(2,2,(/isb%isbSta(iSta)%isbRec(1)%epoch,  &
             isb%isbSta(iSta)%isbRec(isb%isbSta(iSta)%nIsb)%epoch/),timStr)
        CALL timst2(2,1,epoch,epoStr)

        ! Warning
        IF (mode == 1) THEN
          WRITE(lfnerr,'(/,A,4(/,16X,2A),/)')                  &
          ' ### SR d_isbFil:getIsb: Station found but the ' // &
                               'epoch is outside the interval',&
          'Filename:          ',isb%filnam,                    &
          'Station name:      ',isb%isbSta(iSta)%stanam,       &
          'Interval in file:  ',epostr,                        &
          'Request. epoch:    ',epoch

        ! Error
        ELSEIF (mode == 2) THEN
          WRITE(lfnerr,'(/,A,4(/,16X,2A),/)')                  &
          ' *** SR d_isbFil:getIsb: Station found but the ' // &
                               'epoch is outside the interval',&
          'Filename:          ',isb%filnam,                    &
          'Station name:      ',isb%isbSta(iSta)%stanam,       &
          'Interval in file:  ',epostr,                        &
          'Request. epoch:    ',epoch
          CALL exitrc(2)
        ENDIF
      ENDIF
      IF (irCode /= 2) EXIT
    ENDIF
  ENDDO

! Station not found
! -----------------
  IF (irCode == 2 .AND. mode == 1) THEN
    WRITE(lfnerr,'(/,A,2(/,16X,2A),/)')                             &
         ' ### SR d_isbFil:getIsb: Station not found in the file',  &
         'Filename:          ',isb%filnam,                          &
         'Station name:      ',stanam
  ELSEIF (irCode == 2 .AND. mode == 2) THEN
    WRITE(lfnerr,'(/,A,4(/,16X,2A),/)')                             &
         ' *** SR d_isbFil:getIsb: Station not found in the file',  &
         'Filename:          ',isb%filnam,                          &
         'Station name:      ',stanam
    CALL exitrc(2)
  ENDIF

  RETURN
  END FUNCTION getIsb


END MODULE d_isbFil
