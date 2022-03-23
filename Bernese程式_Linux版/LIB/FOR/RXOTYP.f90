MODULE s_RXOTYP
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rxotyp(erropt, flgStr , timint, rxFile, &
                  stName, staNum , line , mrkTex, irCode)

! -------------------------------------------------------------------------
! Purpose:    Changes/checks/verify the marker type of the RINEX files
!             in RXOBV3
!
! Author:     H. Bock
!
! Created:    15-May-2003
! Last mod.:  27-Oct-2010
!
! Changes:    31-Jul-2003 SS: Warning message corrected
!             16-Sep-2003 RD: Use STAINFO instead STACRUX in SR STAFLG
!             19-Feb-2004 HU: Warning message modified
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             27-Oct-2010 SL: use m_bern with ONLY, removal of unused pars
!
! SR called:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, lfnErr, fileNameLength, timStrgLength
  USE m_time,   ONLY: t_timint
  USE p_rxobv3, ONLY: t_rxobv3_err

  USE s_staflg
  USE s_timst2
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  TYPE(t_rxobv3_err)           :: erropt  ! error handling options
  CHARACTER(LEN=*)             :: flgStr  ! String with flags for staInfo
  TYPE(t_timint)               :: timint  ! Data time interval
  CHARACTER(LEN=fileNameLength):: rxFile  ! Name of the RINEX file
  CHARACTER(LEN=*)             :: stName  ! Station name
  CHARACTER(LEN=*)             :: staNum  ! Station number

! output:
  CHARACTER(LEN=*)             :: line    ! Line for program output
  INTEGER(i4b)                 :: mrkTex  ! marker type exists
  INTEGER(i4b)                 :: irCode  ! Return code
                                          ! 0: OK
                                          ! 1: warning, but write file
                                          ! 2: warning, do not write file
                                          ! 3: error, stop program

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength) :: staFil
  CHARACTER(LEN=timStrgLength)  :: timstr1,timstr2
  CHARACTER(LEN=20)             :: markTyp
  CHARACTER(LEN=20)             :: rnxmarkTyp
  CHARACTER(LEN=20)             :: stamarkTyp

  INTEGER(i4b)                  :: irc
  INTEGER(i4b)                  :: flag

  LOGICAL                       :: markTyp_OK


! Init variable
! -------------
  irCode = 0

  line          = ' '

  markTyp_ok = (erropt%tMrkTyp == 0)
  mrkTex     = 0

! Check if station is a LEO or not
! --------------------------------
  CALL gtflna(0,'STAINFO',staFil,irc)
  IF (irc == 0) THEN
    CALL staflg(stName,timint%t(1),flag,markTyp,staInfFil=staFil)

! Check if marker type is correct
! -------------------------------
    IF (.NOT. (flag == 0 .AND.staNum(21:40) == ' ')) THEN
      line(137:144) = '#OK'
      CALL timst2(1,1,timint%t(1),timstr1)
      CALL timst2(1,1,timint%t(2),timstr2)
      IF (markTyp /= staNum(21:40)) THEN
        markTyp_ok = .FALSE.
        rnxmarkTyp = ' '
        stamarkTyp = ' '
        IF (staNum(21:40) == ' ') THEN
          rnxmarkTyp = 'no entry'
        ELSE
          rnxmarkTyp = staNum(21:40)
        ENDIF
        IF (markTyp == ' ') THEN
          stamarkTyp = 'no entry'
        ELSE
          stamarkTyp = markTyp
        ENDIF
        WRITE(lfnerr,'(/,A,A,/,16X,A,A)')                                  &
             ' ### SR RXOTYP: Marker type in RINEX file ',                 &
             'inconsistent with sta info (type 005)',                      &
             'Sta info file: ',TRIM(staFil)
        IF (flgStr == '999') THEN
          WRITE(lfnerr,'(16X,A)')    'List of flags: all entries'
        ELSE
          WRITE(lfnerr,'(16X,A,A)')  'List of flags: ',TRIM(flgStr)
        ENDIF
        WRITE(lfnerr,'(4(16X,A,A,/),16X,A,A,A,/)')                         &
             'Rinex file   : ',TRIM(rxfile),                               &
             'Station name : ',TRIM(stName),                               &
             'First epoch  : ',TRIM(timstr1),                              &
             'Last epoch   : ',TRIM(timstr2),                              &
             'Marker type  : ',rnxmarkTyp,stamarkTyp
        line(137:144) = '#Problem'
      ELSE
        stamarkTyp = markTyp
        rnxmarkTyp = ' '
      ENDIF
      line( 1:16) = stName
      line(19:42) = stamarkTyp
      line(45:63) = rnxmarkTyp
      mrkTex = 1
    ELSE
      mrkTex = 0
    ENDIF
  ENDIF
  IF (.NOT. (marktyp_ok)) THEN
    IF (irCode < erropt%tMrkTyp) irCode = erropt%tMrkTyp
  ENDIF

  RETURN
END SUBROUTINE rxotyp

END MODULE
