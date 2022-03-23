MODULE s_RDIORB
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdiorb(orbtyp, maxstc, maxsat, globalWindow, nAllSat,allSatNum, &
                  norb, seqorb, prec,  nstcep, frctyp, nsastc, numstc, &
                  nstday, sigstc, nspec, numspc, timspc, sigspc, iorest, &
                  nEstSat,estSat)

! -------------------------------------------------------------------------
! Purpose:    Reads the orbit estimation input options for GPSEST
!
! Author:     R. Dach
!
! Created:    21-Jun-2001
!
! Changes:    28-Jun-2002 RD: Make it work for LEOs
!             14-Jan-2003 PF: Allow blank for sigma of stoch. param.
!             26-Mar-2003 RD: New data type for globalWindow
!             23-Apr-2003 RD: Nullify local pointers
!             19-Jan-2003 SS/MM: Revision of GPSEST input panels
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             16-Oct-2006 RD: Manual selection of satellites for orbit determ.
!             28-Feb-2007 AG: Use 206264... from DEFCON
!             19-Jul-2010 SL: tab characters removed
!             16-Nov-2010 HB: Stochastic accelerations added
!             08-May-2012 DT: Correct handling of empty sigmas for pulses
!             06-Jun-2012 MM: Adapted to new option PULSDEF2
!             28-Aug-2012 RD: Defined ORBEST for Galileo
!             28-Aug-2012 RD: Use M_BERN with ONLY; remove unused modules
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, &
                      keynamelength, keyvaluelength, shortlinelength
  USE d_const,  ONLY: ars
  USE m_time,   ONLY: t_timint,OPERATOR(.isin.)
  USE s_dimtst
  USE f_djul
  USE s_timst2
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptc
  USE s_ckopti
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                   :: orbTyp       ! Orbit type to read:
                                                 !  1: GPS/GLONASS
                                                 !  2: LEO
  INTEGER(i4b)                   :: maxstc       ! maximum number of special
                                                 ! stochast. requests
  INTEGER(i4b)                   :: maxsat       ! maximum number of satellites
  TYPE(t_timint)                 :: globalWindow ! window to be processed
                                                 ! (from - to, MJD)
  INTEGER(i4b)                   :: nAllSat      ! number of all satellites
  INTEGER(i4b), DIMENSION(*)     :: allSatNum    ! satellite numbers

! output:
  INTEGER(i4b)                   :: norb         ! number of orbital parameters
                                                 ! to be estimated
  INTEGER(i4b), DIMENSION(*)     :: seqorb       ! sequence for orbital elements
  REAL(r8b),    DIMENSION(*)     :: prec         ! a priori orbital precisions
  INTEGER(i4b)                   :: nstcep       ! number of stochastic forces
                                                 ! per epoch
  INTEGER(i4b), DIMENSION(*)     :: frctyp       ! stoch. force types for
                                                 ! max 3 forces per epoch
                                                 ! (1) force = r : radial
                                                 ! (2) force = s : normal to r
                                                 !                 in orb. plane
                                                 ! (3) force = w : normal to
                                                 !                 orb plane
                                                 ! (4) force = direction sun
                                                 !             --> satellite
                                                 ! (5) force = y direction of
                                                 !             space craft
                                                 ! (6) force = x direction of
                                                 !             space craft
                                                 ! (i)=(i)+10 for accelerations
  INTEGER(i4b)                   :: nsastc       ! number of sats with
                                                 ! stoch. forces
  INTEGER(i4b), DIMENSION(*)     :: numstc       ! satellite numbers
  INTEGER(i4b), DIMENSION(*)     :: nstday       ! number of "stoch epochs"
                                                 ! per day
  REAL(r8b),    DIMENSION(3,*)   :: sigstc       ! a priori sigmas for the
                                                 ! 3 force types
  INTEGER(i4b)                   :: nspec        ! number of special
                                                 ! stoch. requests
  INTEGER(i4b), DIMENSION(*)     :: numspc       ! satellite numbers
  REAL(r8b),    DIMENSION(*)     :: timspc       ! times for these requests
  REAL(r8b),    DIMENSION(3,*)   :: sigspc       ! corresponsing a priori sigmas
  INTEGER(i4b)                   :: iorest       ! orbit estimation for
                                                 ! 0: all satellites
                                                 ! 1: gps satellites only
                                                 ! 2: glonass satellites only
  INTEGER(i4b)                   :: nEstSat      ! #sat for orbit determination
  INTEGER(i4b), DIMENSION(:)     :: estSat       ! PRN for that orbits are requ.

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'rdiorb'

! Orbit-keywords
  CHARACTER(LEN=keyNameLength), DIMENSION(12,2,2), PARAMETER :: orbKeyw = &
   reshape( source =                                                           &
   (/ 'SEMIAXIS ','ECCENTR  ','INCLIN   ','ASCNODE  ','PERIGEE  ','ARGLAT   ', &
      'PARD0    ','PARY0    ','PARX0    ','PRPARD0  ','PRPARY0  ','PRPARX0  ', &
      'SIGSEMI  ','SIGECCE  ','SIGINCL  ','SIGASCN  ','SIGPERI  ','SIGARGL  ', &
      'SIGD0    ','SIGY0    ','SIGX0    ','SIGPRD0  ','SIGPRY0  ','SIGPRX0  ', &
      'SEMIAXIS2','ECCENTR2 ','INCLIN2  ','ASCNODE2 ','PERIGEE2 ','ARGLAT2  ', &
      'PARD02   ','PARY02   ','PARX02   ','PRPARD02 ','PRPARY02 ','PRPARX02 ', &
      'SIGSEMI2 ','SIGECCE2 ','SIGINCL2 ','SIGASCN2 ','SIGPERI2 ','SIGARGL2 ', &
      'SIGD02   ','SIGY02   ','SIGX02   ','SIGPRD02 ','SIGPRY02 ','SIGPRX02 '/),&
      shape = (/12, 2, 2/) )

! Stoch. Orbit-keywords
  CHARACTER(LEN=keyNameLength), DIMENSION(6,2,2), PARAMETER :: stcKeyw = &
   reshape( source =                                                        &
   (/ 'ESTRAD  ','ESTPRP  ','ESTNOR  ','ESTSUN  ','ESTYDIR ','ESTXDIR ' ,   &
      'SIGRAD  ','SIGPRP  ','SIGNOR  ','SIGSUN  ','SIGYDIR ','SIGXDIR ' ,   &
      'ESTRAD2 ','ESTPRP2 ','ESTNOR2 ','ESTSUN2 ','ESTYDIR2','ESTXDIR2' ,   &
      'SIGRAD2 ','SIGPRP2 ','SIGNOR2 ','SIGSUN2 ','SIGYDIR2','SIGXDIR2' /), &
      shape = (/ 6, 2, 2/) )

! Stoch. Orbit-definition
  CHARACTER(LEN=keyNameLength), DIMENSION(4,2), PARAMETER :: stcKeyw2 = &
   reshape( source =                                                        &
   (/ 'NUMSET1  ','SATGROUP ','STCSPEC  ','STCGROUP ',                      &
      'NUMSET12 ','SATGROUP2','STCSPEC2 ','STCGROUP2' /),                   &
      shape = (/ 4, 2/) )

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
         DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=shortLineLength),&
         DIMENSION(6)           :: hlpStr
  CHARACTER(LEN=40)             :: epost1, epost2

  INTEGER(i4b)                  :: iorbadj   ! orbital adjustment:
                                             ! 0=no, 1=non-stoch, 2=all
  INTEGER(i4b)                  :: istcmod
  INTEGER(i4b)                  :: nstdayDef ! # of param per day (default)
  INTEGER(i4b)                  :: iEst
  INTEGER(i4b)                  :: iSat, jSat
  INTEGER(i4b)                  :: yy1, mon1, hh1, min1, ss1
  INTEGER(i4b)                  :: irCode, irc, ios
  INTEGER(i4b)                  :: ii,jj

  REAL(r8b), DIMENSION(3)       :: defSig  ! default sigma for orbit
  REAL(r8b)                     :: rHlp, dd1

! Init all parameters
! -------------------
  irCode      = 0

  nstcep      = 0
  frctyp(1:3) = 0
  defSig(1:3) = 0.0
!
  nspec                = 0
  numspc(1:maxstc)     = 0
  timspc(1:maxstc)     = 0.0
  sigspc(1:3,1:maxstc) = 0.0

  NULLIFY(keyValue)

! Estimation of stochastic pulses
! --------------------------------
  IF (orbTyp == 1) THEN
    CALL readKeys('STCEST', keyValue, irc)
  ELSE IF (orbTyp == 2) THEN
    CALL readKeys('STCEST2', keyValue, irc)
  ELSE
    RETURN
  ENDIF

  IF (keyValue(1) == '1') THEN
    iorbadj = 2
  ELSE
    iorbadj = 1
  ENDIF

! Satellite type
! --------------
  IF (orbTyp == 1) THEN
    CALL readKeys('ORBEST', keyValue, irc)

    CALL ckoptc(1,'ORBEST', keyValue,                                       &
                (/ 'MANUAL ','ALL    ','GPS    ','GLONASS','GALILEO' /),    &
                'Orb.est: satellite system selection', srName, irc, irCode, &
                maxVal = 1, valList=(/-1,0,1,2,3/), result1=iorest )

    ! Read the manual selection list of satellites
    nEstSat = 0
    IF (iorest == -1) THEN
      iorest = 0

      CALL readKeys('ORBEST_LST', keyValue, irc)

      CALL ckopti(1,'ORBEST_LST', keyValue,srName,                   &
                  'Orb.est: manual satellite selection',irc,irCode,  &
                  ge=1,maxVal=nAllSat,nResult=nEstSat,result2=estSat)

      iEst = 1
      DO WHILE (iEst <= nEstSat)
        jSat = 0
        DO iSat = 1,nAllSat
          IF (estSat(iEst) == allSatNum(iSat)) THEN
            jSat = iSat
            EXIT
          ENDIF
        ENDDO
        IF (jSat == 0) THEN
          DO iSat = jSat,nEstSat-1
            estSat(iSat) = estSat(iSat+1)
          ENDDO
          nEstSat = nEstSat-1
        ELSE
          iEst = iEst+1
        ENDIF
      ENDDO

      IF (nEstSat == 0) iOrbAdj = 0

    ! Orbits for all satellites are estimated
    ELSE IF (iorest == 0) THEN
      nEstSat           = nAllSat
      estSat(1:nEstSat) = allSatNum(1:nAllSat)

    ! Orbits for all GPS-satellites are estimated
    ELSE IF (iorest == 1) THEN
      DO iSat = 1,nAllSat
        IF (allSatNum(iSat) < 100) THEN
          nEstSat = nEstSat+1
          estSat(nEstSat) = allSatNum(iSat)
        ENDIF
      ENDDO

    ! Orbits for all GLONASS-satellites are estimated
    ELSE IF (iorest == 2) THEN
      DO iSat = 1,nAllSat
        IF (allSatNum(iSat) > 100 .AND. allSatNum(iSat) < 200) THEN
          nEstSat = nEstSat+1
          estSat(nEstSat) = allSatNum(iSat)
        ENDIF
      ENDDO

    ! Orbits for all Galileo-satellites are estimated
    ELSE IF (iorest == 3) THEN
      DO iSat = 1,nAllSat
        IF (allSatNum(iSat) > 200 .AND. allSatNum(iSat) < 300) THEN
          nEstSat = nEstSat+1
          estSat(nEstSat) = allSatNum(iSat)
        ENDIF
      ENDDO
    ENDIF
  ENDIF

! Non-Stochastic Orbital Parameters
! ---------------------------------
  IF (iorbadj > 0) THEN
    DO ii = 1, SIZE(orbKeyw,1)
      CALL readKeys(orbKeyw(ii,1,orbTyp), keyValue, irc)
! is the parameter requ.:
      IF (irc == 0 .AND. keyValue(1) == '1') THEN
        norb = norb + 1
        seqorb(norb) = ii
! read the sigma:
        CALL readKeys(orbKeyw(ii,2,orbTyp), keyValue, irc)
        READ(keyValue(1),*, iostat=ios) prec(norb)
        IF (irc /= 0 .OR. ios /=  0) THEN
          prec(norb) = 0d0
          IF (irc /= 0 .OR. LEN_TRIM(keyValue(1)) > 0) THEN
            WRITE(lfnerr,'(/,A)')                    &
            ' *** SR RDIRAP: Wrong apriori sigma specificationd for'
            IF (ii == 1) THEN
              WRITE(lfnerr,'(16X,A)') 'Orbit estimation (Semi major axis)'
            ELSE IF (ii ==  2) THEN
              WRITE(lfnerr,'(16X,A)') 'Orbit estimation (Eccentricity)'
            ELSE IF (ii ==  3) THEN
              WRITE(lfnerr,'(16X,A)') 'Orbit estimation (Inclination)'
            ELSE IF (ii ==  4) THEN
              WRITE(lfnerr,'(16X,A)') 'Orbit estimation (Ascending node)'
            ELSE IF (ii ==  5) THEN
              WRITE(lfnerr,'(16X,A)') 'Orbit estimation (Perigee)'
            ELSE IF (ii ==  6) THEN
              WRITE(lfnerr,'(16X,A)') 'Orbit estimation (Arg. of latitude)'
            ELSE IF (ii ==  7) THEN
              WRITE(lfnerr,'(16X,A)') 'Orbit estimation (D0 Estimation (P0))'
            ELSE IF (ii ==  8) THEN
              WRITE(lfnerr,'(16X,A)') 'Orbit estimation (Y0 Estimation (P2))'
            ELSE IF (ii ==  9) THEN
              WRITE(lfnerr,'(16X,A)') 'Orbit estimation (X0 Estimation)'
            ELSE IF (ii == 10) THEN
              WRITE(lfnerr,'(16X,A)') 'Orbit estimation (Periodic D0 terms)'
            ELSE IF (ii == 11) THEN
              WRITE(lfnerr,'(16X,A)') 'Orbit estimation (Periodic Y0 terms)'
            ELSE IF (ii == 12) THEN
              WRITE(lfnerr,'(16X,A)') 'Orbit estimation (Periodic X0 terms)'
            ENDIF
            WRITE(lfnerr,'(16X,2A,/)') &
              'Specified value:       ',TRIM(keyValue(1))
            CALL exitrc(2)
          ENDIF
        ENDIF
! adapt units:
        IF (ii >=  3 .AND. ii <=  6) prec(norb) = prec(norb) / ars
        IF (ii >=  7 .AND. ii <= 12) prec(norb) = prec(norb) * 1d9
! periodical elem:
        IF (ii >= 10 .AND. ii <= 12) THEN
          norb = norb + 1
          seqorb(norb) = ii + 3
          prec(norb) = prec(norb-1)
        ENDIF
      ENDIF
    ENDDO
  ENDIF

! Stochastic Orbital Parameters
! -----------------------------
  IF (iorbadj == 2) THEN
    DO ii = 1, SIZE(stcKeyw,1)
! check which type of forces are requested
      CALL readKeys(stcKeyw(ii,1,orbTyp), keyValue, irc)
      IF (irc == 0 .AND. keyValue(1) == '1') THEN
        nstcep = nstcep + 1
        IF (nstcep <= 3) frctyp(nstcep) = ii

! get the corresponding (default) sigma values
        CALL readKeys(stcKeyw(ii,2,orbTyp), keyValue, irc)
        IF ( LEN_TRIM(keyValue(1)) > 0 ) THEN
          READ(keyValue(1),*,iostat=ios) rHlp
          IF (nstcep <= 3 .AND. ios == 0) defSig(nstcep) = rHlp
        ENDIF

! Error reading the default sigma
        IF (irc /= 0 .OR. ios /= 0) THEN
          IF (irc /= 0 .OR. LEN_TRIM(keyValue(1)) > 0) THEN
            WRITE(lfnerr,'(/,A)')                    &
            ' *** SR RDIRAP: Wrong apriori sigma specificationd for'
            IF (ii == 1) THEN
              WRITE(lfnerr,'(16X,A)') 'Stoch. orbit est. (Radial)'
            ELSE IF (ii ==  2) THEN
              WRITE(lfnerr,'(16X,A)') 'Stoch. orbit est. (Orbit Plane)'
            ELSE IF (ii ==  3) THEN
              WRITE(lfnerr,'(16X,A)') 'Stoch. orbit est. ' // &
                                      '(Normal to Orbit Plane)'
            ELSE IF (ii ==  4) THEN
              WRITE(lfnerr,'(16X,A)') 'Stoch. orbit est. ' // &
                                      '(Direction to the Sun)'
            ELSE IF (ii ==  5) THEN
              WRITE(lfnerr,'(16X,A)') 'Stoch. orbit est. ' // &
                                      '(Y-Direction in Satellite Frame)'
            ELSE IF (ii ==  6) THEN
              WRITE(lfnerr,'(16X,A)') 'Stoch. orbit est. ' // &
                                      '(X-Direction in Satellite Frame)'
            ENDIF
            WRITE(lfnerr,'(16X,2A,/)') &
              'Specified value:       ',TRIM(keyValue(1))
            CALL exitrc(2)
          ENDIF
        ENDIF
      ENDIF
    ENDDO

! Too many forces specified
    IF (nstcep > 3) THEN
      CALL dimtst(1,1,2,'rdiorb',' ',               &
           'stochastic orbit forces',               &
           'Dimension is hardwired in PG GPSEST.',  &
           nstcep,3,irc)
    ENDIF

! Read the default number of requests per day
    CALL readKeys(stcKeyw2(1,orbTyp), keyValue, irc)
    READ(keyValue(1),*,iostat=ios) nstdayDef
    IF (irc == 0 .AND. ios /= 0) THEN
      nstdayDef = 1
      IF (LEN_TRIM(keyValue(1)) > 0) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                      &
          ' *** SR RDIORB: Wrong number of parameters per day are set',&
                          'for the stochastic orbit parameter',        &
                          'Specified value:       ',TRIM(keyValue(1))
        CALL exitrc(2)
      ENDIF
    ENDIF

! Read the requests record (satellite list)
    CALL readKeys(stcKeyw2(2,orbTyp), keyValue, irc)
    nsastc = SIZE(keyValue)

! Too many satellites specified
    CALL dimtst(1,1,2,'rdiorb','maxsat',             &
               'stochastic orbit parameters',        &
               'Include file "MAXSAT.inc" is used.', &
               nsastc, maxsat,irc)

! extract the line
    DO ii = 1, SIZE(keyValue)
      READ(keyValue(ii),*,iostat=ios)  (hlpstr(jj), jj=1,5)

! satellite number
      READ(hlpstr(1),*,iostat=ios) numstc(ii)
      IF (ios /= 0) THEN
        IF (ii == 1) THEN  ! only one empty line
          nsastc = 0
          EXIT
        ELSE
          WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')           &
          ' *** SR RDIORB: Wrong satellite number specification for the',&
                          'Stochastic orbit parameter setup',            &
                          'Request number:        ',ii,                  &
                          'Specified value:       ',TRIM(hlpStr(1))
          CALL exitrc(2)
        ENDIF
      ENDIF

! number of pulses per day
      READ(hlpstr(2),*,iostat=ios) nstday(ii)
      IF (ios /= 0) THEN
        nstday(ii) = nstdayDef
        IF (LEN_TRIM(hlpstr(2)) > 0) THEN
          WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')           &
          ' *** SR RDIORB: Wrong number of parameters per day are set',  &
                          'for the stochastic orbit parameters',         &
                          'Request number:        ',ii,                  &
                          'Specified value:       ',TRIM(hlpStr(2))
          CALL exitrc(2)
        ENDIF
      ENDIF

! special sigma for 1st force
      READ(hlpstr(3),*,iostat=ios) sigstc(1,ii)
      IF (ios /= 0) THEN
        sigstc(1,ii) = defSig(1)
        IF (LEN_TRIM(hlpstr(3)) > 0) THEN
          WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')           &
          ' *** SR RDIORB: Wrong apriori sigma specification for the',   &
                          'Stochastic orbit parameter (1st force)',      &
                          'Request number:        ',ii,                  &
                          'Specified value:       ',TRIM(hlpStr(3))
          CALL exitrc(2)
        ENDIF
      ENDIF

! special sigma for 2nd force
      READ(hlpstr(4),*,iostat=ios) sigstc(2,ii)
      IF (ios /= 0) THEN
        sigstc(2,ii) = defSig(2)
        IF (LEN_TRIM(hlpstr(4)) > 0) THEN
          WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')           &
          ' *** SR RDIORB: Wrong apriori sigma specification for the',   &
                          'Stochastic orbit parameter (2nd force)',      &
                          'Request number:        ',ii,                  &
                          'Specified value:       ',TRIM(hlpStr(4))
          CALL exitrc(2)
        ENDIF
      ENDIF

! special sigma for 3rd force
      READ(hlpstr(5),*,iostat=ios) sigstc(3,ii)
      IF (ios /= 0) THEN
        sigstc(3,ii) = defSig(3)
        IF (LEN_TRIM(hlpstr(5)) > 0) THEN
          WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')           &
          ' *** SR RDIORB: Wrong apriori sigma specification for the',   &
                          'Stochastic orbit parameter (3rd force)',      &
                          'Request number:        ',ii,                  &
                          'Specified value:       ',TRIM(hlpStr(5))
          CALL exitrc(2)
        ENDIF
      ENDIF

    ENDDO

! Special Stochastic Requests
! ---------------------------
    CALL readKeys(stcKeyw2(3,orbTyp),keyValue,irc)
    IF (irc == 0 .AND. keyValue(1) == '1') THEN
      CALL readKeys(stcKeyw2(4,orbTyp),keyValue,irc)

! too many special requests
      CALL dimtst(1,1,2,'rdiorb','maxstc',                &
                 'stochastic orbit parameters',           &
                 'Include file "MAXSTC.inc" is used.',    &
                 SIZE(keyValue), maxstc,irc)

! extract the record
      nSpec = SIZE(keyValue)
      DO ii = 1, SIZE(keyValue)
        READ(keyValue(ii),*,iostat=ios) (hlpStr(jj),jj=1,6)

! satellite number
        READ(hlpStr(1), *, iostat=ios) numspc(ii)
        IF (ios /= 0) THEN
          IF (ii == 1) THEN  ! only one empty line
            nSpec = 0
            EXIT
          ELSE
            WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')            &
            ' *** SR RDIORB: Wrong satellite number specification for the', &
                            'Special stochastic orbit parameters',          &
                            'Request number:        ',ii,                   &
                            'Specified value:       ',TRIM(hlpStr(1))
            CALL exitrc(2)
          ENDIF
        ENDIF

! Get the epoch of the special request
        READ(hlpStr(2), *, iostat=ios) yy1, mon1, dd1
        IF (ios == 0) READ(hlpStr(3), *, iostat=ios) hh1, min1, ss1
        timspc(ii) = DJUL(yy1, mon1, dd1+(hh1+(min1+ss1/60d0)/60d0)/24d0)
        IF (ios /= 0) timspc(ii) = 0d0
        IF (.NOT. (timspc(ii) .isin. globalWindow)) THEN
          CALL timst2(1,1, timspc(ii),   epost1)
          CALL timst2(1,2, globalWindow%t, epost2)
          WRITE(lfnerr,'(/,A,/,16X,A,I5,2(/,16X,A,A),/)')                           &
             ' *** SR RDIORB: Wrong epoch for stochastic orbit parameters', &
                             'Number of requests: ',ii,                     &
                             'Epoch of request:   ',epost1,                 &
                             'Observations:       ',epost2
          CALL exitrc(2)
        ENDIF

! special sigma for 1st force
        READ(hlpstr(4),*,iostat=ios) sigspc(1,ii)
        IF (ios /= 0) THEN
          sigspc(1,ii) = defSig(1)
          IF (LEN_TRIM(hlpstr(4)) > 0) THEN
            WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')              &
            ' *** SR RDIORB: Wrong apriori sigma specification for the',      &
                            'Special stochastic orbit parameter (1st force)', &
                            'Request number:        ',ii,                     &
                            'Specified value:       ',TRIM(hlpStr(4))
            CALL exitrc(2)
          ENDIF
        ENDIF

! special sigma for 2nd force
        READ(hlpstr(5),*,iostat=ios) sigspc(2,ii)
        IF (ios /= 0) THEN
          sigspc(2,ii) = defSig(2)
          IF (LEN_TRIM(hlpstr(5)) > 0) THEN
            WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')              &
            ' *** SR RDIORB: Wrong apriori sigma specification for the',      &
                            'Special stochastic orbit parameter (2nd force)', &
                            'Request number:        ',ii,                     &
                            'Specified value:       ',TRIM(hlpStr(5))
            CALL exitrc(2)
          ENDIF
        ENDIF

! special sigma for 3rd force
        READ(hlpstr(6),*,iostat=ios) sigspc(3,ii)
        IF (ios /= 0) THEN
          sigspc(3,ii) = defSig(3)
          IF (LEN_TRIM(hlpstr(6)) > 0) THEN
            WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')              &
            ' *** SR RDIORB: Wrong apriori sigma specification for the',      &
                            'Special stochastic orbit parameter (3rd force)', &
                            'Request number:        ',ii,                     &
                            'Specified value:       ',TRIM(hlpStr(6))
            CALL exitrc(2)
          ENDIF
        ENDIF

      ENDDO
    ENDIF   ! get special requests for stoch orbit

! Stochastic Parameter Type
! -------------------------
    CALL readKeys('PULSDEF2', keyValue, irc)
    CALL ckoptc(1,'PULSDEF2', keyValue, (/'VELOCITY    ','ACCELERATION'/), &
                'Orb.est: pulse definition', srName, irc, irCode,          &
                maxVal = 1, valList=(/1,2/), result1=iStcMod )

! accelerations: frctyp=frctyp+10
    IF (iStcMod == 2) THEN
      DO ii=1,nstcep
        frctyp(ii)=frctyp(ii)+10
      END DO
    ENDIF
  ENDIF     ! get stochastic orbit

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

  IF (irCode /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE rdiorb

END MODULE
