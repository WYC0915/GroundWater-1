MODULE s_RXOANT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rxoant(erropt, rnxInfo,staInfo,flgStr, timint, rxFile, &
                  stName, rectyp, anttyp, &
                  antpos, meatyp, rectnw, anttnw, irecnw, iantnw, &
                  antnew, line,   irCode)

! -------------------------------------------------------------------------
! Purpose:    Changes/checks/verfify the receiver/antenna name, number,
!             antenna position of the RINEX files in RXOBV3
!
! Author:     R. Dach
!
! Created:    07-May-2003
! Last mod.:  27-Oct-2010
!
! Changes:    06-Jun-2003 HU: Trim filenames for output
!             23-Jun-2003 HB: change dimensions of antpos and antnew from
!                             (3) to (3,3)
!                             handle MARKER TYPE SPACEBORNE
!             24-Jun-2003 HU: Interface to RDSATFIL inserted
!             30-Jun-2003 HB: Format corrected for writing error message
!             12-Aug-2003 HU: Call gphecc with rectyp=' '
!             08-Sep-2003 HU: Do not consider radome code
!             16-Oct-2003 RD: Reading of radome option shifted
!             24-Nov-2003 HU: Check only martyp for SPACEBORNE
!             19-Dec-2003 SS: Correct position of radome code if necessary
!             23-Dec-2003 HU: Warning only if antoff not equal 0 for spaceborne
!             28-Jan-2004 HU: Warning text modified
!             29-Apr-2005 SS: Indicate unverified receiver and antenna numbers
!                             with 999999
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             23-Jun-2006 SS: Set NONE radome code if indicated
!             18-Jul-2006 SS: Do not set NONE if '*' at the end
!             26-Mar-2007 HB: Initialize satInf
!             12-Jun-2007 AG: Use sta_off instead off gphecc
!             24-Jul-2007 SS: Consider only last 5 digits of receiver/antenna
!                             number
!             09-Oct-2007 AG: Correction for antenna verification
!             11-Dec-2007 HB: meatyp as parameter for special handling of SLR
!             01-Feb-2008 HB: bug fix: SLR station eccentricities were ignored
!             04-Aug-2008 DT: Use MTypeSLR (D_STACRX) instead of sta_slr
!             05-Oct-2010 SL: staNum,irunit,ianten removed -> test S/N
!             27-Oct-2010 SL: use m_bern with ONLY, removal of unused pars
!
! SR called:  wildcd, timst2, staFlg
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnErr, fileNameLength, timStrgLength
  USE m_time,   ONLY: t_timint
  USE d_stacrx, ONLY: t_stacrux,undef_c,undef_i,undef_e,MTypeSPACE,MTypeSLR
  USE d_satfil, ONLY: t_satfil, init_satfil
  USE d_phaecc, ONLY: init_buf
  USE p_rxobv3, ONLY: t_rxobv3_err

  USE s_staflg
  USE s_timst2
  USE s_wildcd
  USE s_gtflna
  USE s_rdsatfil
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  TYPE(t_rxobv3_err)           :: erropt  ! error handling options
  TYPE(t_staCrux)              :: rnxInfo ! staCrux: entries in RINEX files
  TYPE(t_staCrux)              :: staInfo ! staInfo: entries for Bernese
  CHARACTER(LEN=*)             :: flgStr  ! String with flags for staInfo
  TYPE(t_timint)               :: timint  ! Data time interval
  CHARACTER(LEN=fileNameLength):: rxFile  ! Name of the RINEX file
  CHARACTER(LEN=*)             :: stName  ! Station name
  CHARACTER(LEN=*)             :: rectyp  ! Receiver name (RINEX)
  CHARACTER(LEN=*)             :: anttyp  ! Antenna  name (RINEX)
  REAL(r8b), DIMENSION(3,3)    :: antpos  ! Antenna position (RINEX)
  INTEGER(i4b)                 :: meatyp  ! Measurement type

! output:
  CHARACTER(LEN=*)             :: rectnw  ! Receiver name (Bernese)
  CHARACTER(LEN=*)             :: anttnw  ! Antenna  name (Bernese)
  INTEGER(i4b)                 :: irecnw  ! Receiver number (Bernese)
  INTEGER(i4b)                 :: iantnw  ! Antenna  number (Bernese)
  REAL(r8b), DIMENSION(3,3)    :: antnew  ! Antenna position (Bernese)
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
  TYPE(t_satFil) :: satInf

! Local parameters
! ----------------
  CHARACTER(LEN= 6), PARAMETER  :: srName = 'rxoant'

  REAL(r8b),         PARAMETER  :: dtSim  = 1d0/86400d0

  CHARACTER(LEN=20), PARAMETER  :: A20    = '                    '

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength) :: staFil, crxFil, satFil
  CHARACTER(LEN=timStrgLength)  :: timstr1,timstr2
  CHARACTER(LEN=40),DIMENSION(7):: expect
  CHARACTER(LEN=26),DIMENSION(1):: anten
  CHARACTER(LEN=20)             :: markTyp
  CHARACTER(LEN=4)              :: radCod

  REAL(r8b)                     :: diff

  INTEGER(i4b)                  :: iInfo
  INTEGER(i4b)                  :: iTest
  INTEGER(i4b)                  :: ii
  INTEGER(i4b)                  :: irc
  INTEGER(i4b)                  :: flag
  INTEGER(i4b)                  :: iSat
  INTEGER(i4b)                  :: iLen
  INTEGER(i4b), DIMENSION(2)    :: iPos

  LOGICAL                       :: anttyp_OK
  LOGICAL                       :: rectyp_OK
  LOGICAL                       :: antnum_OK
  LOGICAL                       :: recnum_OK
  LOGICAL, DIMENSION(3)         :: antpos_OK
  LOGICAL                       :: sat_OK


! Init variable
! -------------
  CALL init_satfil(satInf)
  irCode = 0

  line          = ' '
  line(137:144) = '#OK'

  rectyp_ok = (erropt%tAnttyp == 0)
  anttyp_ok = (erropt%tAnttyp == 0)

  recnum_ok = (erropt%tAntnum == 0)
  antnum_ok = (erropt%tAntnum == 0)

  antpos_ok = (erropt%tAntpos == 0)

  expect = 'not checked'

! Correct position of radome code if necessary
! --------------------------------------------
  IF (erropt%corRad == 1) THEN
    iPos = 0
    DO ii=20,1,-1
      IF (iPos(2) == 0) THEN
        IF (antTyp(ii:ii) /= ' ') iPos(2) = ii
      ELSEIF (iPos(1) == 0) THEN
        IF (antTyp(ii:ii) == ' ') iPos(1) = ii+1
      ENDIF
    ENDDO
    IF (iPos(1) > 1 .AND. iPos(1) < 17 .AND. iPos(2)-iPos(1) == 3) THEN
      WRITE(lfnerr,'(/,A,3(/,16X,A,A),/)')    &
        ' ### SR RXOANT: Position of radome code corrected', &
        'RINEX file   : ',TRIM(rxFile),                      &
        'Station name : ',TRIM(stName),                      &
        'Antenna name : ',TRIM(antTyp(1:20))
      radCod = anttyp(iPos(1):iPos(2))
      antTyp(iPos(1):iPos(2)) = '    '
      antTyp(17:20) = radCod
    ENDIF
    IF (iPos(1) > 17) THEN
      WRITE(lfnerr,'(/,A,3(/,16X,A,A),/)')    &
        ' ### SR RXOANT: Truncated radome code',             &
        'RINEX file   : ',TRIM(rxFile),                      &
        'Station name : ',TRIM(stName),                      &
        'Antenna name : ',TRIM(antTyp(1:20))
    ENDIF
  ENDIF

! Set NONE radome code if indicated
! ---------------------------------
  IF     (erropt%Radome == 1 .AND. anttyp(17:20) == '    ') THEN
    anttyp(17:20) = 'NONE'
  ELSEIF (erropt%Radome == 0) THEN
    anttyp(17:20) = '    '
  ENDIF

! Check if station is a LEO or not
! --------------------------------
  markTyp = ''
  CALL gtflna(0,'STAINFO',staFil,irc)
  IF (irc == 0) THEN
    CALL staflg(stName,timint%t(1),flag,markTyp,staInfFil=staFil)

! Check antenna offsets for LEO in SATELLIT. file
! -----------------------------------------------
    IF (markTyp == MTypeSPACE) THEN
      CALL gtflna(1,'SATELL',satFil,irc)
      CALL rdSatFil(satFil,satInf)
      sat_ok = .FALSE.
      DO iSat = 1, satInf%nSensor
        IF (satInf%sensor(iSat)%sensor == stName) THEN
          sat_ok = .TRUE.
          EXIT
        ENDIF
      ENDDO
      IF (sat_ok) THEN
        DO ii=1,3
          diff =(antpos(ii,1)-antpos(ii,3)) - satInf%sensor(iSat)%antoff(ii)
          antpos_ok(ii) = antpos_ok(ii) .OR. diff < 1.D-4
        ENDDO
      ELSE
        WRITE(lfnErr,'(A,/,A,A,/,A,A,/)')&
             ' ### SR RXOANT: Sensor name not found in ',  &
             '                Satellite info file: ',trim(satFil),&
             '                Sensor name : ',stName
      ENDIF
    ENDIF
  ENDIF

! Check receiver/antenna info in RINEX crux
! -----------------------------------------
  DO iInfo = 1,rnxInfo%nInfo

    ! Station name
    IF (stName /= rnxInfo%staInfo(iInfo)%stanam) CYCLE

    ! Check time interval
    IF (timint%t(1)+dtSim > rnxInfo%staInfo(iInfo)%timint%t(2)) CYCLE
    IF (timint%t(2)-dtSim < rnxInfo%staInfo(iInfo)%timint%t(1)) CYCLE

    ! Check receiver name
    CALL wildcd(rnxInfo%staInfo(iInfo)%recnam(1:erropt%trnChr), &
                rectyp(1:erropt%trnChr),iTest)
    rectyp_ok = rectyp_ok .OR. &
      (rnxInfo%staInfo(iInfo)%recnam /= undef_c .AND. iTest == 1)

    ! Check antenna name
    iLen = LEN_TRIM(rnxInfo%staInfo(iInfo)%antnam)
    IF (erropt%Radome == 1                             .AND. &
        rnxInfo%staInfo(iInfo)%antnam(17:20) == '    ' .AND. &
        rnxInfo%staInfo(iInfo)%antnam /= undef_c       .AND. &
        rnxInfo%staInfo(iInfo)%antnam(iLen:iLen) /= '*')     &
      rnxInfo%staInfo(iInfo)%antnam(17:20) = 'NONE'
    CALL wildcd(rnxInfo%staInfo(iInfo)%antnam(1:erropt%trnChr), &
                anttyp(1:erropt%trnChr),iTest)
    anttyp_ok = anttyp_ok .OR. &
      (rnxInfo%staInfo(iInfo)%antnam /= undef_c .AND. iTest == 1)

    ! Check receiver number
    CALL wildcd(rnxInfo%staInfo(iInfo)%recser,rectyp(21:40),iTest)
    recnum_ok = recnum_ok .OR. &
      (LEN_TRIM(rnxInfo%staInfo(iInfo)%recser) /= 0 .AND. iTest == 1)

    ! Check antenna number
    CALL wildcd(rnxInfo%staInfo(iInfo)%antser,anttyp(21:40),iTest)
    antnum_ok = antnum_ok .OR. &
      (LEN_TRIM(rnxInfo%staInfo(iInfo)%antser) /= 0 .AND. iTest == 1)

    ! Check antenna position
    IF (markTyp /= MTypeSPACE) THEN
      DO ii = 1,3
        antpos_ok(ii) = antpos_ok(ii) .OR. &
             (rnxInfo%staInfo(iInfo)%antecc(ii) /= undef_e .AND. &
             rnxInfo%staInfo(iInfo)%antecc(ii) == antpos(ii,1))
      ENDDO
    ENDIF

    ! Expected receiver name
    IF (.NOT. rectyp_ok .AND. rnxInfo%staInfo(iInfo)%recnam /= undef_c) &
      expect(1) = 'expected: ' // TRIM(rnxInfo%staInfo(iInfo)%recnam(1:erropt%trnChr))

    ! Expected antenna name
    IF (.NOT. anttyp_ok .AND. rnxInfo%staInfo(iInfo)%antnam /= undef_c) &
      expect(2) = 'expected: ' // TRIM(rnxInfo%staInfo(iInfo)%antnam(1:erropt%trnChr))

    ! Expected receiver number
    IF (.NOT. recnum_ok .AND. LEN_TRIM(rnxInfo%staInfo(iInfo)%recser) /= 0) THEN
      expect(3) = 'expected: ' // TRIM(rnxInfo%staInfo(iInfo)%recser)
    ENDIF

    ! Expected antenna number
    IF (.NOT. antnum_ok .AND. LEN_TRIM(rnxInfo%staInfo(iInfo)%antser) /= 0) THEN
      expect(4) = 'expected: ' // TRIM(rnxInfo%staInfo(iInfo)%antser)
    ENDIF

    ! Expected antenna position
    IF (markTyp /= MTypeSPACE) THEN
      DO ii = 1,3
        IF (.NOT. antpos_ok(ii) .AND. &
             rnxInfo%staInfo(iInfo)%antecc(ii) /= undef_e) THEN
          expect(4+ii) = 'expected: '
          WRITE(expect(4+ii),'(F17.4)') rnxInfo%staInfo(iInfo)%antecc(ii)
        ENDIF
      ENDDO
    ELSEIF (markTyp == MTypeSPACE .AND. antpos(1,1) /= 0.D0 .AND.&
      antpos(2,1) /= 0.D0 .AND. antpos(3,1) /= 0.D0) THEN
      DO ii = 1,3
        IF (.NOT. antpos_ok(ii) .AND. sat_ok) THEN
          expect(4+ii) = 'expected: '
          WRITE(expect(4+ii),'(F17.4)') satInf%sensor(iSat)%antOff(ii)
        ENDIF
      ENDDO
    ENDIF

  ENDDO


! Check receiver/antenna info wrt station info file
! -------------------------------------------------
  DO iInfo = 1,staInfo%nInfo

    ! Station name
    IF (stName /= staInfo%staInfo(iInfo)%stanam) CYCLE

    ! Check time interval
    IF (timint%t(1)+dtSim > staInfo%staInfo(iInfo)%timint%t(2)) CYCLE
    IF (timint%t(2)-dtSim < staInfo%staInfo(iInfo)%timint%t(1)) CYCLE

    ! Check receiver name
    rectyp_ok = rectyp_ok .OR. &
      (staInfo%staInfo(iInfo)%recnam /= undef_c              .AND. &
       staInfo%staInfo(iInfo)%recnam(1:erropt%trnChr) == &
                                             rectyp(1:erropt%trnChr))

    ! Check antenna name
    anttyp_ok = anttyp_ok .OR. &
      (staInfo%staInfo(iInfo)%antnam /= undef_c              .AND. &
       staInfo%staInfo(iInfo)%antnam(1:erropt%trnChr) == &
                                             anttyp(1:erropt%trnChr))

    ! Check receiver number
    CALL wildcd(staInfo%staInfo(iInfo)%recser,rectyp(21:40),iTest)
    recnum_ok = recnum_ok .OR. &
      (LEN_TRIM(staInfo%staInfo(iInfo)%recser) /= 0 .AND. iTest == 1)
    recnum_ok = recnum_ok .OR. &
       LEN_TRIM(staInfo%staInfo(iInfo)%recser) == 0

    ! Check antenna number
    CALL wildcd(staInfo%staInfo(iInfo)%antser,anttyp(21:40),iTest)
    antnum_ok = antnum_ok .OR. &
      (LEN_TRIM(staInfo%staInfo(iInfo)%antser) /= 0 .AND. iTest == 1)
    antnum_ok = antnum_ok .OR. &
       LEN_TRIM(staInfo%staInfo(iInfo)%antser) == 0

    ! Check antenna position
    IF (markTyp /= MTypeSPACE) THEN
      DO ii = 1,3
        antpos_ok(ii) = antpos_ok(ii) .OR. &
             (staInfo%staInfo(iInfo)%antecc(ii) /= undef_e .AND. &
             staInfo%staInfo(iInfo)%antecc(ii) == antpos(ii,1))
      ENDDO
    ENDIF

    ! Expected receiver name
    IF (.NOT. rectyp_ok .AND. staInfo%staInfo(iInfo)%recnam /= undef_c) &
      expect(1) = 'expected: ' // staInfo%staInfo(iInfo)%recnam(1:erropt%trnChr)

    ! Expected antenna name
    IF (.NOT. anttyp_ok .AND. staInfo%staInfo(iInfo)%antnam /= undef_c) &
      expect(2) = 'expected: ' // staInfo%staInfo(iInfo)%antnam(1:erropt%trnChr)

    ! Expected receiver number
    IF (.NOT. recnum_ok .AND. LEN_TRIM(staInfo%staInfo(iInfo)%recser) /= 0) THEN
      expect(3) = 'expected: ' // TRIM(staInfo%staInfo(iInfo)%recser)
    ENDIF

    ! Expected antenna number
    IF (.NOT. antnum_ok .AND. LEN_TRIM(staInfo%staInfo(iInfo)%antser) /= 0) THEN
      expect(4) = 'expected: ' // TRIM(staInfo%staInfo(iInfo)%antser)
    ENDIF

    ! Expected antenna position
    IF (markTyp /= MTypeSPACE) THEN
      DO ii = 1,3
        IF (.NOT. antpos_ok(ii) .AND. &
             staInfo%staInfo(iInfo)%antecc(ii) /= undef_e) THEN
          expect(4+ii) = 'expected: '
          WRITE(expect(4+ii),'(F17.4)') staInfo%staInfo(iInfo)%antecc(ii)
        ENDIF
      ENDDO
    ELSEIF (markTyp == MTypeSPACE .AND. antpos(1,1) /= 0.D0 .AND.&
         antpos(2,1) /= 0.D0 .AND. antpos(3,1) /= 0.D0) THEN
      DO ii = 1,3
        IF (.NOT. antpos_ok(ii) .AND. sat_ok) THEN
          expect(4+ii) = 'expected: '
          WRITE(expect(4+ii),'(F17.4)') satInf%sensor(iSat)%antOff(ii)
        ENDIF
      ENDDO
    ENDIF

  ENDDO


! Check failed for at least one component
! ---------------------------------------
  IF (.NOT. (anttyp_ok .AND. rectyp_ok .AND. antnum_ok .AND. recnum_ok .AND. &
             antpos_ok(1) .AND. antpos_ok(2) .AND. antpos_ok(3) )) THEN

    CALL gtflna(0,'STAINFO',staFil,irc)
    IF (irc /= 0 .OR. LEN_TRIM(staFil) == 0) staFil = '---'

    CALL gtflna(0,'RNXINFO',crxFil,irc)
    IF (irc /= 0 .OR. LEN_TRIM(crxFil) == 0) crxFil = '---'

    CALL timst2(1,1,timint%t(1),timstr1)
    CALL timst2(1,1,timint%t(2),timstr2)

    IF (.NOT. (markTyp == MTypeSPACE .AND. anttyp_ok .AND. rectyp_ok&
      .AND. antnum_ok .AND. recnum_ok)) THEN
      WRITE(lfnerr,'(/,A,A,2(/,16X,A,A))')                                  &
           ' ### SR RXOANT: RINEX header inconsistent with ',               &
           'station information (type 002)',                                &
           'Sta info file: ',staFil,                                        &
           'Sta crux file: ',crxFil
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

    ! Receiver type
    IF (.NOT. rectyp_ok) WRITE(lfnerr,'(16X,A,A20,2X,A)') &
      'Receiver type: ',TRIM(rectyp(1:erropt%trnChr))//A20,TRIM(expect(1))

    ! Antenna type
    IF (.NOT. anttyp_ok) WRITE(lfnerr,'(16X,A,A20,2X,A)') &
      'Antenna type : ',TRIM(anttyp(1:erropt%trnChr))//A20,TRIM(expect(2))

    ! Receiver number
    IF (.NOT. recnum_ok) WRITE(lfnerr,'(16X,A,A20,2X,A)') &
      'Receiver num.: ',rectyp(21:40),TRIM(expect(3))

    ! Antenna number
    IF (.NOT. antnum_ok) WRITE(lfnerr,'(16X,A,A20,2X,A)') &
      'Antenna num. : ',anttyp(21:40),TRIM(expect(4))

    ! Antenna position (N)
    IF (markTyp /= MTypeSPACE) THEN
      IF (.NOT. antpos_ok(1)) WRITE(lfnerr,'(16X,A,F17.4,3X,2X,A)') &
           'Antenna pos.N: ',antpos(1,1),TRIM(expect(5))

    ! Antenna position (E)
      IF (.NOT. antpos_ok(2)) WRITE(lfnerr,'(16X,A,F17.4,3X,2X,A)') &
           'Antenna pos.E: ',antpos(2,1),TRIM(expect(6))

    ! Antenna position (U)
      IF (.NOT. antpos_ok(3)) WRITE(lfnerr,'(16X,A,F17.4,3X,2X,A)') &
           'Antenna pos.U: ',antpos(3,1),TRIM(expect(7))

    ! Antenna position (X)
    ELSEIF (markTyp == MTypeSPACE .AND. &
         (.NOT.(antpos_ok(1).AND.antpos_ok(2).AND.antpos_ok(3)))) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A)')                         &
           ' ### SR RXOANT: RINEX antenna offset values inconsistent',&
           'with those from station info file (type 002)',            &
           'Sat info file: ',satFil
      WRITE(lfnerr,'(3(16X,A,A,/),16X,A,A)')                         &
           'RINEX file   : ',TRIM(rxfile),                           &
           'Station name : ',TRIM(stName),                           &
           'First epoch  : ',TRIM(timstr1),                          &
           'Last epoch   : ',TRIM(timstr2)
      IF (.NOT. antpos_ok(1)) WRITE(lfnerr,'(16X,A,/,16X,A,F17.4,3X,2X,A)') &
           'Antenna off.X  ',&
           ' minus center of mass X:',antpos(1,1)-antpos(1,3),TRIM(expect(5))

    ! Antenna position (Y)
      IF (.NOT. antpos_ok(2)) WRITE(lfnerr,'(16X,A,/,16X,A,F17.4,3X,2X,A)') &
           'Antenna off.Y  ',&
           ' minus center of mass Y:',antpos(2,1)-antpos(2,3),TRIM(expect(6))

    ! Antenna position (Z)
      IF (.NOT. antpos_ok(3)) WRITE(lfnerr,'(16X,A,/,16X,A,F17.4,3X,2X,A)') &
           'Antenna off.Z  ',&
           ' minus center of mass Z:',antpos(3,1)-antpos(3,3),TRIM(expect(7))
    ENDIF

    WRITE(lfnerr,*)

    line(137:144) = '#Problem'
  ENDIF


! Set return code
! ---------------
  IF (.NOT. (rectyp_ok .AND. anttyp_ok)) THEN
    IF (irCode < erropt%tAnttyp) irCode = erropt%tAnttyp
  ENDIF

  IF (.NOT. (recnum_ok .AND. antnum_ok)) THEN
    IF (irCode < erropt%tAntnum) irCode = erropt%tAntnum
  ENDIF

  IF (.NOT. (antpos_ok(1) .AND. antpos_ok(2) .AND. antpos_ok(3))) THEN
    IF (irCode < erropt%tAntpos) irCode = erropt%tAntpos
  ENDIF

! Init station info output values
! -------------------------------
  rectnw = rectyp(1:20)
  anttnw = anttyp(1:20)
  irecnw = undef_i
  iantnw = undef_i
  antnew = antpos

! Change receiver/antenna info wrt station info file
! --------------------------------------------------
  DO iInfo = 1,staInfo%nInfo

    ! Station name
    IF (stName /= staInfo%staInfo(iInfo)%stanam) CYCLE

    ! Check time interval
    IF (timint%t(1)+dtSim > staInfo%staInfo(iInfo)%timint%t(2)) CYCLE
    IF (timint%t(2)-dtSim < staInfo%staInfo(iInfo)%timint%t(1)) CYCLE

    ! Change receiver name
    IF (staInfo%staInfo(iInfo)%recnam /= undef_c) &
      rectnw = staInfo%staInfo(iInfo)%recnam

    ! Change antenna name
    IF (staInfo%staInfo(iInfo)%antnam /= undef_c) &
      anttnw = staInfo%staInfo(iInfo)%antnam

    ! Change receiver number
    irecnw = staInfo%staInfo(iInfo)%recnum

    ! Change antenna number
    iantnw = staInfo%staInfo(iInfo)%antnum

    ! Change antenna position
    IF (markTyp /= MTypeSPACE) THEN
      DO ii = 1,3
        IF (staInfo%staInfo(iInfo)%antecc(ii) /= undef_e) &
             antnew(ii,1) = staInfo%staInfo(iInfo)%antecc(ii)
      ENDDO
    ELSEIF (markTyp == MTypeSPACE) THEN
      IF (antnew(1,1)/=0D0 .OR. antnew(2,1)/=0D0 .OR. antnew(3,1)/=0D0) THEN
        DO ii = 1,3
          antnew(ii,1) = 0.D0
        ENDDO
        WRITE(lfnErr,'(A,A,/,16X,A,A,/,2(16X,A,A,/))')&
           ' ### SR RXOANT: Antenna offset values of SPACEBORNE ',  &
                           'receiver are set to zero',              &
                           'to avoid use of wrong corrections ',    &
                           'in further processing',                 &
                           'RINEX file   : ',trim(rxFile),          &
                           'Sensor name  : ',trim(stName)
      ENDIF
    ENDIF

  ENDDO


! Generate line with changes for table in program output
! ------------------------------------------------------
  line( 1:16) = stName
  line(19:38) = rectnw
  line(40:59) = anttnw
  WRITE(line(60:73),'(F12.4,1X,A)') antnew(3,1),'m'

  IF (rectnw(1:erropt%trnChr) /= rectyp(1:erropt%trnChr)) THEN
    line( 78: 97) = rectyp
  ENDIF

  IF (anttnw(1:erropt%trnChr) /= anttyp(1:erropt%trnChr)) THEN
    line( 99:118) = anttyp
  ENDIF

  IF (antnew(3,1) /= antpos(3,1) .AND. markTyp /= MTypeSPACE) THEN
    WRITE(line(119:132),'(F12.4,1X,A)') antpos(3,1),'m'
  ENDIF

! Make sure that the receiver/antenna number is in a valid range
! (valid means 0..999999 .. for Bernese;-)
! --------------------------------------------------------------
  IF (irecnw < 0 .OR. irecnw > undef_i) THEN
    WRITE(lfnerr,"(/,' ### SR RXOANT: Receiver number overflow,',&
                 &                  ' number set to zero', &
                 & /,'                Station        : ',A,      &
                 & /,'                Receiver number:',I15,/)") &
                       stName,irecnw
    irecnw = 0
  ENDIF
  IF (iantnw < 0 .OR. iantnw > undef_i) THEN
    WRITE(lfnerr,"(/,' ### SR RXOANT: Antenna number overflow,', &
                 &                  ' number set to zero', &
                 & /,'                Station        : ',A,      &
                 & /,'                Antenna number :',I15,/)") &
                       stName,iantnw
    iantnw = 0
  ENDIF

  IF (meatyp == 3) THEN
    anttnw = MTypeSLR
    rectnw = MTypeSLR
    RETURN
  ENDIF

! Check receiver/antenna pair with PHASECC file
! ---------------------------------------------
  IF (erropt%verAnt > 0) THEN
    irc = 0
    WRITE(anten(1),"(A20,I6)")anttnw,iantnw
    CALL init_buf(antlist=anten,err=irc)
    IF (irc /= 0) THEN
      IF (irCode < erropt%verAnt) irCode = erropt%verAnt
      IF (line(137:139) == '#OK') line(137:144) = '#Verify'
    ENDIF
  ENDIF


  RETURN
END SUBROUTINE rxoant

END MODULE

