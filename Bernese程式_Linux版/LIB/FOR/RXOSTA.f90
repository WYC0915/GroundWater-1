MODULE s_RXOSTA
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rxosta(rnxsta,erropt,staInfo,flgStr,timint,rxfile, &
                  rxonam,rxonum,stName,line,irCode)

! -------------------------------------------------------------------------
! Purpose:    Rename/checks/verfify the station name of the RINEX files
!             in RXOBV3
!
! Author:     R.Dach
!
! Created:    07-May-2003
!
! Changes:    11-Jun-2003 RD: Trim filenames for output
!             23-Jun-2003 HB: Write marker type in line for output
!             06-Feb-2003 HU: Warning text modified
!             29-Apr-2005 SS: Ignore station number if indicated
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             05-Sep-2006 SS: Ignore station number if indicated
!             31-Jan-2008 RD: Init stNaNu (if RNX-domes is empty)
!             27-Oct-2010 SL: Use m_bern with ONLY, removal of unused pars
!             15-Dec-2011 SL: Use s_stripdir for rxfile
!             12-Feb-2012 RD: Unify massage layout
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnErr, &
                      fileNameLength, fileExtLength, staNameLength, &
                      timStrgLength
  USE m_time,   ONLY: t_timint
  USE d_stacrx, ONLY: t_stacrux
  USE p_rxobv3, ONLY: t_rxobv3_err
  USE s_fparse
  USE s_wildcd
  USE s_gtflna
  USE s_stripdir
  USE s_timst2
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                 :: rnxSta  ! Old station name from RINEX
                                          ! = 0: File name (4ID)
                                          ! = 1: Marker name
                                          ! = 2: Marker number
                                          ! = 3: Marker name+number (IGS like)
  TYPE(t_rxobv3_err)           :: erropt  ! error handling options
  TYPE(t_staCrux)              :: staInfo ! staInfo: entries for Bernese
  CHARACTER(LEN=*)             :: flgStr  ! String with flags for staInfo
  TYPE(t_timint)               :: timint  ! Data time interval
  CHARACTER(LEN=fileNameLength):: rxFile  ! Name of the RINEX file
  CHARACTER(LEN=*)             :: rxonam  ! Marker name from RINEX file
  CHARACTER(LEN=*)             :: rxonum  ! Marker number from RINEX file

! output:
  CHARACTER(LEN=staNameLength) :: stName  ! Station name
  CHARACTER(LEN=*)             :: line    ! Line for program output
  INTEGER(i4b)                 :: irCode  ! Return code
                                          ! 0: OK
                                          ! 1: warning, but write file
                                          ! 2: warning, do not write file
                                          ! 3: error, stop program

! List of funtions
! ----------------

! Local types
! -----------

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER   :: srName = 'rxosta'

  REAL(r8b),        PARAMETER   :: dtSim  = 1d0/86400d0

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength) :: node    ! used by FPARSE
  CHARACTER(LEN=fileNameLength) :: device  ! used by FPARSE
  CHARACTER(LEN=fileNameLength) :: dir     ! used by FPARSE
  CHARACTER(LEN=fileNameLength) :: name    ! used by FPARSE
  CHARACTER(LEN=fileExtLength)  :: ext     ! used by FPARSE
  CHARACTER(LEN=fileExtLength)  :: ver     ! used by FPARSE
  CHARACTER(LEN=fileNameLength) :: staFil
  CHARACTER(LEN=timStrgLength)  :: timstr1,timstr2
  CHARACTER(LEN=staNameLength)  :: stNaNu  ! Station name+number
  CHARACTER(LEN=fileNameLength) :: rxStrip

  INTEGER(i4b)                  :: iRenam
  INTEGER(i4b)                  :: iTest
  INTEGER(i4b)                  :: irc

  LOGICAL                       :: sta_OK


! Init variable
! -------------
  irCode = 0

  line          = ' '
  line(137:144) = '#OK'

! Get station name from RINEX
! ---------------------------
  stNaNu = ''
  IF (rxonum(1:20) == rxonam(1:20)) THEN
    stNaNu = rxonam
  ELSEIF (rxonum(6:6) == 'M' .OR. rxonum(6:6) == 'S') THEN
    stNaNu = rxonam(1:4) // ' ' // rxonum(1:9)
  ELSE
    stNaNu = rxonam
  ENDIF

  IF (rnxsta == 0) THEN
    CALL fparse(0,rxfile,node,device,dir,name,ext,ver,irc)
    stname = name(1:4)

  ELSE IF (rnxsta == 1) THEN
    stname = rxonam

  ELSE IF (rnxsta == 2) THEN
    stname = rxonum

  ELSE IF (rnxsta == 3) THEN
    stname = stNaNu

  ENDIF

! Check station name
! ------------------
  sta_ok = (erropt%tStanam == 0)

  DO iRenam = 1,staInfo%nRenam

    ! Check time interval
    IF (timint%t(1)+dtSim > staInfo%renamsta(irenam)%timint%t(2)) CYCLE
    IF (timint%t(2)-dtSim < staInfo%renamsta(irenam)%timint%t(1)) CYCLE

    ! Check old name with wild cards
    CALL wildcd(staInfo%renamsta(iRenam)%oldnam,stName,iTest)

    ! Station was found: perform renaming
    IF (iTest == 1) THEN
      sta_ok = .TRUE.
      stName = staInfo%renamsta(iRenam)%stanam
      EXIT
    ENDIF

  ENDDO

! Station not found in list
! -------------------------
  IF (.NOT. sta_ok) THEN

    ! Error flag
    line(137:144) = '#Problem'

    CALL gtflna(0,'STAINFO',staFil,irc)
    IF (irc /= 0 .OR. LEN_TRIM(staFil) == 0) staFil = '---'

    CALL timst2(1,1,timint%t(1),timstr1)
    CALL timst2(1,1,timint%t(2),timstr2)
    WRITE(lfnerr,'(/,A,A,/,16X,A,A)')                         &
      ' ### SR RXOSTA: RINEX station name not listed in ',    &
      'station info file (type 001)',                         &
      'Sta info file: ',TRIM(staFil)
    IF (flgStr == '999') THEN
      WRITE(lfnerr,'(16X,A)')    'List of flags: all entries'
    ELSE
      WRITE(lfnerr,'(16X,A,A)')  'List of flags: ',TRIM(flgStr)
    ENDIF
    WRITE(lfnerr,'(3(16X,A,A,/),16X,A,A)')                             &
      'RINEX file   : ',TRIM(rxfile),                                  &
      'Station name : ',TRIM(stName),                                  &
      'First epoch  : ',TRIM(timstr1),                                 &
      'Last epoch   : ',TRIM(timstr2)
  ENDIF

! Check RINEX file name as alternative station name
! -------------------------------------------------
  IF (.NOT. sta_ok .AND. erropt%abbStn .AND. rnxsta > 0) THEN

    ! Get the RINEX file name
    CALL fparse(0,rxfile,node,device,dir,name,ext,ver,irc)

    DO iRenam = 1,staInfo%nRenam

      ! Check time interval
      IF (timint%t(1)+dtSim > staInfo%renamsta(irenam)%timint%t(2)) CYCLE
      IF (timint%t(2)-dtSim < staInfo%renamsta(irenam)%timint%t(1)) CYCLE

      ! Check old name with wild cards
      CALL wildcd(staInfo%renamsta(iRenam)%oldnam,name(1:4),iTest)

      ! Station was found: perform renaming
      IF (iTest == 1) THEN
        sta_ok = .TRUE.
        stName = staInfo%renamsta(iRenam)%stanam
        WRITE(lfnerr,'(16X,A,/,16X,A,A,/)')                              &
          'Alternative 4-character station id used for translation !!!', &
          'Station name : ',TRIM(stName)
        EXIT
      ENDIF

    ENDDO

  ENDIF

! Really nothing found
! --------------------
  IF (.NOT. sta_ok) THEN
    WRITE(lfnerr,*)
    irCode = erropt%tStanam
  ENDIF

! Verfy station name
! ------------------

  ! RINEX Marker name:
  IF (erropt%verSta == 1 .AND. stName /= rxonam) THEN
    WRITE(lfnerr,'(/,A,/,3(16X,A,A,/))')                              &
    ' ### SR RXOSTA: Station name different from RINEX marker name:', &
                    'RINEX file       : ',TRIM(rxfile),               &
                    'Station name     : ',TRIM(stname),               &
                    'RINEX marker name: ',TRIM(rxonam)

    ! Error flags
    IF (line(137:139) == '#OK') line(137:144) = '#Verify'

  ENDIF

  ! RINEX marker number
  IF (erropt%verSta == 2 .AND. stName /= rxonum) THEN
    WRITE(lfnerr,'(/,A,/,3(16X,A,A,/))')                                &
    ' ### SR RXOSTA: Station name different from RINEX marker number:', &
                    'RINEX file         : ',TRIM(rxfile),               &
                    'Station name       : ',TRIM(stname),               &
                    'RINEX marker number: ',TRIM(rxonum)

    ! Error flags
    IF (line(137:139) == '#OK') line(137:144) = '#Verify'

  ENDIF

  ! RINEX marker name+number:
  IF (erropt%verSta == 3 .AND. stName(1:14) /= stNaNu(1:14)) THEN
    WRITE(lfnerr,'(/,A,/,3(16X,A,A,/))')                             &
    ' ### SR RXOSTA: Station name different from RINEX DOMES code:', &
                    'RINEX file      : ',TRIM(rxfile),               &
                    'Station name    : ',TRIM(stname),               &
                    'RINEX DOMES code: ',TRIM(stNaNu)
    ! Error flags
    IF (line(137:139) == '#OK') line(137:144) = '#Verify'

  ENDIF

  ! RINEX file name:
  CALL fparse(0,rxfile,node,device,dir,name,ext,ver,irc)
  IF (erropt%verFil .AND. stname(1:4) /= name(1:4)) THEN
    WRITE(lfnerr,'(/,A,/,2(16X,A,A,/))')                              &
    ' ### SR RXOSTA: Station name different from RINEX file name:',   &
                    'RINEX file  : ',TRIM(rxfile),                    &
                    'Station name: ',TRIM(stname)

    ! Error flags
    IF (line(137:139) == '#OK') line(137:144) = '#Verify'

  ENDIF

! Write the station renaming summary line
! ---------------------------------------
  rxStrip = rxFile
  CALL STRIPDIR(rxStrip)
  line(  6: 37) = TRIM(rxStrip)
  line( 42: 57) = TRIM(stname)
  line( 62: 81) = rxonam(1:20)
  line( 84:123) = rxonum

  RETURN

END SUBROUTINE rxosta

END MODULE
