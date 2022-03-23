MODULE s_RDIRAP
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdirap(maxcal, isasys, nancal, antcal, numcal, &
                  prncal, nfrcal, nptcal, sigcal, rapzenmax)

! -------------------------------------------------------------------------
! Purpose:    Reads the receiver antenna pcv input options for GPSEST
!
! Author:     R. Dach
!
! Created:    20-Jun-2001
! Last mod.:  10-Mar-2008
!
! Changes:    01-Apr-2003 HU: Comment in DIMTST adapted
!             23-Apr-2003 RD: Nullify local pointers
!             08-Sep-2003 HU: antnam, recnam chr16 -> chr20
!             29-Oct-2003 RS: Elevation and azimuth increments, add
!                             rapzenmax, selection of reference antennas,
!                             receiver-independent antenna groups, change
!                             recnam and antnam in lists
!             19-Jan-2003 SS/MM: Revision of GPSEST input panels
!             10-Mar-2008 RD: Update concerning GNSS and freq. selection
!
! SR used:    dimtst, exitrc, readKeys
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_const,  ONLY: pi
  USE s_dimtst
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b) :: maxcal ! maximum number of receiver antenna phase center
                         ! parameters
  INTEGER(i4b) :: isasys ! satellite system to be considered
                         ! = 0: ALL
                         ! = 1: GPS
                         ! = 2: GLONASS
                         ! = 3: Galileo
                         ! = 4: GPS+GLONASS
                         ! = 5: GPS+Galileo
                         ! = 6: GLONASS+Galileo

! output:
  INTEGER(i4b)                    :: nancal ! number of receiver antenna phase
                                            ! center requests
  CHARACTER(LEN=20), &
                DIMENSION(2,*)    :: antcal ! receiver and antenna
                                            ! name for request i
  INTEGER(i4b), DIMENSION(2,*)    :: numcal ! antenna numbers
                                            ! "from - to" for request i
  INTEGER(i4b), DIMENSION(*)      :: prncal ! satellite system (index in g_svnsys)
  INTEGER(i4b), DIMENSION(*)      :: nfrcal ! frequency for phase center req. i
  INTEGER(i4b), DIMENSION(2,*)    :: nptcal ! # of points to be estimated in
                                            ! elevation (j=1) and azimuth (j=2)
                                            ! (1) > 0 ... linear model
                                            ! (1) < 0 ... spherical harmonics
  REAL(r8b),    DIMENSION(*)      :: sigcal ! a priori sigmas in meters
  REAL(r8b)                       :: rapzenmax
                                            ! maximum zenith angle for receiver
                                            ! antenna pattern estimation (in rad)

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER     :: srName = 'rdirap'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength) , &
         DIMENSION(:)  , POINTER  :: keyValue
  CHARACTER(LEN=shortLineLength), &
         DIMENSION(9)             :: hlpStr

  INTEGER(i4b)                    :: rapSel
  INTEGER(i4b)                    :: rapFreq, rapFreq0
  INTEGER(i4b)                    :: rapDele, rapDazi
  INTEGER(i4b)                    :: indDele, indDazi
  INTEGER(i4b)                    :: rapNum,  rapPrn0
  INTEGER(i4b)                    :: ista, jSta
  INTEGER(i4b)                    :: irc, ios
  INTEGER(i4b)                    :: irCode = 0
  INTEGER(i4b)                    :: ii, jj
  INTEGER(i4b)                    :: zenmaxint, rapMod
  INTEGER(i4b)                    :: rapHarmn, rapHarmm

  REAL(r8b)                       :: rapSigma

! Init variables
! --------------
  NULLIFY(keyValue)

! Read the frequency to be estimated
! ----------------------------------
  CALL readKeys('FREQUENCY', keyValue, irc)
  CALL ckoptc(1,'FREQUENCY', keyValue,                                    &
              (/ 'L1   ', 'L2   ', 'L3   ', 'L4   ', 'L5   ', 'L1&L2' /), &
              srName, 'Frequency selection', irc, irCode,                 &
              other = 0, maxVal = 1, valList = (/ 1,2,4,5,4,3 /),         &
              result1 = rapFreq0)

  rapFreq = rapFreq0
  IF (rapFreq == 3) THEN
    CALL readKeys('RAPFRQ', keyValue, irc)
    CALL ckoptc(1,'RAPFRQ', keyValue,                                        &
              (/ 'L1   ', 'L2   ', 'BOTH ', 'L1&L2' /),                      &
              srName, 'Rec. ant. pattern: Frequency selection', irc, irCode, &
              maxVal = 1, valList = (/ 1,2,3,4 /), result1 = rapFreq)
  ENDIF


! Read the satellite system to be estimated
! -----------------------------------------
  rapPrn0 = isasys - 1
  IF (rapPrn0 == -1) rapPrn0 = 10
  IF (isasys /= 1 .AND. isasys /= 2 .AND. isasys /= 3) THEN
    CALL readKeys('RAPPRN', keyValue, irc)
    CALL ckoptc(1,'RAPPRN', keyValue, (/ 'GPS    ', 'GLONASS', 'GALILEO', 'GNSS   ' /), &
              srName, 'Rec. ant. patterns: Satellite system', irc, irCode,    &
              maxVal = 1, valList = (/ 0,1,2,10 /), result1 = rapPrn0)
    IF ( ( rapPrn0 == 0 .AND. isasys == 6 ) .OR. &
         ( rapPrn0 == 1 .AND. isasys == 5 ) .OR. &
         ( rapPrn0 == 2 .AND. isasys == 4 ) ) THEN
      nancal = 0
      RETURN
    ENDIF
  ENDIF



  CALL readKeys('RAPSIGMA', keyValue, irc)
  IF (irc == 0) READ(keyValue(1), *, iostat=ios) rapSigma
  IF (irc /= 0 .OR. ios /= 0) THEN
    rapSigma = 0d0
    IF (LEN_TRIM(keyValue(1)) > 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                    &
        ' *** SR RDIRAP: Wrong a priori sigma specified for',      &
                        'receiver antenna phase center variation', &
                        'Specified value:       ',TRIM(keyValue(1))
      CALL exitrc(2)
    ENDIF
  ENDIF

! Which model type was set?
! -------------------------
  rapMod = 0
  CALL readkeys('RAPMOD_1', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') rapMod = 1
  CALL readkeys('RAPMOD_2', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') rapMod = 2

  IF (rapMod == 1) THEN

    CALL readKeys('RAPZENMAX', keyValue, irc)
    IF (irc == 0) READ(keyValue(1), *, iostat=ios) zenmaxint
    rapzenmax = DBLE(zenmaxint)/180.D0*pi
    IF (irc /= 0 .OR. ios /= 0) THEN
      rapzenmax = 90.D0/180.D0*pi
      IF (LEN_TRIM(keyValue(1)) > 0) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                     &
          ' *** SR RDIRAP: Wrong maximum zenith angle specified for', &
                          'receiver antenna phase center variation',  &
                          'Specified value:       ',TRIM(keyValue(1))
        CALL exitrc(2)
      ENDIF
    ENDIF

    CALL readKeys('RAPDELE', keyValue, irc)
    IF (irc == 0) READ(keyValue(1), *, iostat=ios) rapDele
    IF (irc /= 0 .OR. ios /= 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                    &
        ' *** SR RDIRAP: Wrong elevation increment specified for', &
                        'receiver antenna phase center variation', &
                        'Specified value:       ',TRIM(keyValue(1))
      CALL exitrc(2)
    ENDIF

    CALL readKeys('RAPDAZI', keyValue, irc)
    IF (irc == 0) READ(keyValue(1), *, iostat=ios) rapDazi
    IF (irc /= 0 .OR. ios /= 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                    &
        ' *** SR RDIRAP: Wrong azimuth increment specified for',   &
                        'receiver antenna phase center variation', &
                        'Specified value:       ',TRIM(keyValue(1))
      CALL exitrc(2)
    ENDIF

  ELSEIF (rapMod == 2) THEN

    CALL readKeys('RAPHARMN', keyValue, irc)
    IF (irc == 0) READ(keyValue(1), *, iostat=ios) rapHarmn
    IF (irc /= 0 .OR. ios /= 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                    &
        ' *** SR RDIRAP: Wrong degree n specified for',            &
                        'receiver antenna phase center variation', &
                        'Specified value:       ',TRIM(keyValue(1))
      CALL exitrc(2)
    ENDIF

    CALL readKeys('RAPHARMM', keyValue, irc)
    IF (irc == 0) READ(keyValue(1), *, iostat=ios) rapHarmm
    IF (irc /= 0 .OR. ios /= 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                    &
        ' *** SR RDIRAP: Wrong order m specified for',             &
                        'receiver antenna phase center variation', &
                        'Specified value:       ',TRIM(keyValue(1))
      CALL exitrc(2)
    ENDIF

  ELSE
    WRITE(lfnerr,'(/,A,/,16X,A,/)')                              &
      ' *** SR RDIRAP: Wrong model type specified for',          &
                      'receiver antenna phase center variation'
    CALL exitrc(2)
  ENDIF

! Which type of selection was set?
! --------------------------------
  CALL readKeys('RAPSEL', keyValue, irc)
  CALL ckoptc(1,'RAPSEL', keyValue, &
              (/'RECEIVER-INDEPENDENT','RECEIVER-DEPENDENT  ', &
                'INDIVIDUAL          ','MANUAL              '/), &
              'rdirap', 'RAP setup', irc, irCode, &
              valList=(/3,4,2,1/), result1=rapSel)

  IF (irCode /= 0) CALL exitrc(2)

! read the manual input
! ---------------------
  IF (rapSel == 1) THEN
    CALL readKeys('RAPSTR', keyValue, irc)
    nancal = SIZE(keyValue)

! dimension test
! --------------
    CALL dimtst(1,1,2,'rdirap','maxcal',                          &
                'antenna phase center pattern',                   &
                'Parameter is defined in module "P_GPSEST.f90".', &
                nancal, maxcal, irc)

    DO ii = 1, SIZE(keyValue)
      READ(keyValue(ii),*, iostat=ios) (hlpStr(jj), jj=1,9)
      IF (ios /= 0) nancal = nancal - 1

      IF (ios == 0) THEN
        antcal(1,ii) = hlpStr(2)
        antcal(2,ii) = hlpStr(1)

        READ(hlpStr(3), *, iostat=ios) numcal(1,ii)
        IF (ios /= 0) THEN
          WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')         &
            ' *** SR RDIRAP: Wrong antenna number specification for ', &
                            'receiver antenna pattern estimation',     &
                            'Number of requests:    ',ii,              &
                            'Specified value:       ',TRIM(hlpStr(3))
          nancal = nancal - 1
        ENDIF

        READ(hlpStr(4), *, iostat=ios) numcal(2,ii)
        IF (ios /= 0) THEN
          WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')         &
            ' *** SR RDIRAP: Wrong antenna number specification for ', &
                            'receiver antenna pattern estimation',     &
                            'Number of requests:    ',ii,              &
                            'Specified value:       ',TRIM(hlpStr(4))
          nancal = nancal - 1
        ENDIF

        nfrcal(ii) = -1
        IF ((rapFreq0 == 1 .OR. rapFreq0 == 3) .AND. &
            (hlpStr(5) == 'L1' .OR. hlpStr(5) == '1')) THEN
          nfrcal(ii) = 1
        ELSE IF ((rapFreq0 == 2 .OR. rapFreq0 == 3) .AND. &
             (hlpStr(5) == 'L2' .OR. hlpStr(5) == '2')) THEN
          nfrcal(ii) = 2
        ELSE IF ( rapFreq0 == 3 .AND. hlpStr(5) == 'BOTH') THEN
          nfrcal(ii) = 3
        ELSE IF ((rapFreq0 == 3 .OR. rapFreq0 == 4) .AND. &
             (hlpStr(5) == 'L3' .OR. hlpStr(5) == 'L5' .OR. &
              hlpStr(5) == '3'  .OR. hlpStr(5) == '5'  .OR. &
              hlpStr(5) == 'L1&L2')) THEN
          nfrcal(ii) = 4
        ELSE IF (rapFreq0 == 5 .AND. &
             (hlpStr(5) == 'L4' .OR. hlpStr(5) == '4')) THEN
          nfrcal(ii) = 5
        ENDIF
        IF (nfrcal(ii) == -1) THEN
          nfrcal(ii) = rapfreq
          IF (LEN_TRIM(hlpStr(5)) > 0) THEN
            WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')         &
              ' *** SR RDIRAP: Wrong frequency specification for ',      &
                              'receiver antenna pattern estimation',     &
                              'Number of requests:    ',ii,              &
                              'Specified value:       ',TRIM(hlpStr(5))
            nancal = nancal - 1
          ENDIF
        ENDIF

        prncal(ii) = -1
        IF((isasys == 0 .OR. isasys == 1 .OR. &
            isasys == 4 .OR. isasys == 5 ) .AND. &
            hlpStr(6) == 'GPS') THEN
          prncal(ii) = 0
        ELSE IF((isasys == 0 .OR. isasys == 2 .OR. &
            isasys == 4 .OR. isasys == 6 ) .AND. &
            hlpStr(6) == 'GLO' .OR. hlpStr(6) == 'GLONASS' ) THEN
          prncal(ii) = 1
        ELSE IF((isasys == 0 .OR. isasys == 3 .OR. &
            isasys == 5 .OR. isasys == 6 ) .AND. &
            hlpStr(6) == 'GAL' .OR. hlpStr(6) == 'GALILEO' ) THEN
          prncal(ii) = 2
        ELSE IF(hlpStr(6) == 'MIX' .OR. hlpStr(6) == 'GNSS' .OR. &
                LEN_TRIM(hlpStr(6)) == 0) THEN
          prncal(ii) = 10
        ENDIF
        IF (prncal(ii) == -1) THEN
          prncal(ii) = rapPrn0
          IF (LEN_TRIM(hlpStr(6)) > 0) THEN
            WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')         &
              ' *** SR RDIRAO: Wrong satellite system specification for ',&
                              'receiver antenna pattern estimation',      &
                              'Number of requests:    ',ii,              &
                              'Specified value:       ',TRIM(hlpStr(6))
            nancal = nancal - 1
          ENDIF
        ENDIF

        READ(hlpStr(7), *, iostat=ios) indDele
        IF (rapMod == 1) THEN
          nptcal(1,ii) = IDNINT(rapzenmax/pi*180.D0/DBLE(indDele))+1
        ELSEIF (rapMod == 2) THEN
          nptcal(1,ii) = indDele
        ENDIF
        IF (ios /= 0) THEN
          IF (rapMod == 1) THEN
            nptcal(1,ii) = IDNINT(rapzenmax/pi*180.D0/DBLE(rapDele))+1
          ELSEIF (rapMod == 2) THEN
            nptcal(1,ii) = rapHarmn
          ENDIF
          IF (LEN_TRIM(hlpStr(7)) > 0) THEN
            WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')              &
              ' *** SR RDIRAP: Wrong elevation increment specification for ', &
                              'receiver antenna pattern estimation',          &
                              'Number of requests:    ',ii,                   &
                              'Specified value:       ',TRIM(hlpStr(6))
            nancal = nancal - 1
          ENDIF
        ENDIF

        READ(hlpStr(8), *, iostat=ios) indDazi
        IF (rapMod == 1) THEN
          nptcal(2,ii) = IDNINT(360.D0/DBLE(indDazi))+1
        ELSEIF (rapMod == 2) THEN
          nptcal(2,ii) = indDazi
        ENDIF
        IF (ios /= 0) THEN
          IF (rapMod == 1) THEN
            nptcal(2,ii) = IDNINT(360.D0/DBLE(rapDazi))+1
          ELSEIF (rapMod == 2) THEN
            nptcal(2,ii) = rapHarmm
          ENDIF
          IF (LEN_TRIM(hlpStr(8)) > 0) THEN
            WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')            &
              ' *** SR RDIRAP: Wrong azimuth increment specification for ', &
                              'receiver antenna pattern estimation',        &
                              'Number of requests:    ',ii,                 &
                              'Specified value:       ',TRIM(hlpStr(7))
            nancal = nancal - 1
          ENDIF
        ENDIF

        READ(hlpStr(9), *, iostat=ios) sigcal(ii)
        IF (ios /= 0) THEN
          sigcal(ii) = rapSigma
          IF (LEN_TRIM(hlpStr(9)) > 0) THEN
            WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')           &
              ' *** SR RDIRAP: Wrong a priori sigma specification for ',   &
                              'receiver antenna pattern estimation',       &
                              'Number of requests:    ',ii,                &
                              'Specified value:       ',TRIM(hlpStr(8))
            nancal = nancal - 1
          ENDIF
        ENDIF
      ENDIF
    ENDDO
    IF (nancal /= SIZE(keyValue)) CALL exitrc(2)

! individual antennas selected
! ----------------------------
  ELSE IF (rapSel == 2) THEN
    CALL readkeys('RAPANT',keyValue,irc)
    nancal = SIZE(keyValue)

! dimension test
! --------------
    CALL dimtst(1,1,2,'rdirap','maxcal',                          &
                'antenna phase center pattern',                   &
                'Parameter is defined in module "P_GPSEST.f90".', &
                nancal, maxcal, irc)

    DO ii = 1, SIZE(keyValue)
      READ(keyValue(ii),*, iostat=ios) (hlpStr(jj), jj=1,3)
      IF (ios /= 0) nancal = nancal - 1

      IF (ios == 0) THEN
        antcal(1,ii) = hlpStr(2)
        antcal(2,ii) = hlpStr(1)

        READ(hlpStr(3), *, iostat=ios) numcal(1,ii)
        IF (ios /= 0) THEN
          WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')         &
            ' *** SR RDIRAP: Wrong antenna number specification for ', &
                            'receiver antenna pattern estimation',     &
                            'Number of requests:    ',ii,              &
                            'Specified value:       ',TRIM(hlpStr(3))
          nancal = nancal - 1
        ENDIF

        numcal(2,ii) = numcal(1,ii)
        prncal(ii)   = rapPrn0
        nfrcal(ii)   = rapfreq
        sigcal(ii)   = rapSigma
        IF (rapMod == 1) THEN
          nptcal(1,ii) = IDNINT(rapzenmax/pi*180.D0/DBLE(rapDele))+1
          nptcal(2,ii) = IDNINT(360.D0/DBLE(rapDazi))+1
        ELSEIF (rapMod == 2) THEN
          nptcal(1,ii) = rapHarmn
          nptcal(2,ii) = rapHarmm
        ENDIF
      ENDIF
    ENDDO
    IF (nancal /= SIZE(keyValue)) CALL exitrc(2)

! read the individual reference antennas
! --------------------------------------
    CALL readkeys('RAPREFI',keyValue,irc)

    DO ii = 1, SIZE(keyValue)
      READ(keyValue(ii),*, iostat=ios) (hlpStr(jj), jj=1,3)
!      IF (ios /= 0)
      IF (ios == 0) THEN
        READ(hlpStr(3), *, iostat=ios) rapnum
        IF (ios /= 0) THEN
          WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')         &
            ' *** SR RDIRAP: Wrong antenna number specification for ', &
                            'receiver antenna pattern estimation',     &
                            'Number of requests:    ',ii,              &
                            'Specified value:       ',TRIM(hlpStr(3))
          CALL exitrc(2)
        ENDIF

        iSta = 1
        DO WHILE (iSta <= nancal)
          IF (hlpStr(2) == antcal(1,iSta) .AND. &
              hlpStr(1) == antcal(2,iSta) .AND. &
              rapnum    == numcal(1,iSta) .AND. &
              rapnum    == numcal(2,iSta)) THEN
            DO jSta = iSta, nancal-1
              antcal(1,jSta) = antcal(1,jSta+1)
              antcal(2,jSta) = antcal(2,jSta+1)
              numcal(1,jSta) = numcal(1,jSta+1)
              numcal(2,jSta) = numcal(2,jSta+1)
              nptcal(1,jSta) = nptcal(1,jSta+1)
              nptcal(2,jSta) = nptcal(2,jSta+1)
              sigcal(jSta)   = sigcal(jSta+1)
              prncal(jSta)   = prncal(jSta+1)
              nfrcal(jSta)   = nfrcal(jSta+1)
            ENDDO
            nancal = nancal - 1
          ENDIF
          iSta = iSta + 1
        ENDDO
      ENDIF
    ENDDO


! receiver-independent antenna group selection
! --------------------------------------------
  ELSE IF (rapSel == 3) THEN

    CALL readkeys('RAPGRPI',keyValue,irc)
    nancal = SIZE(keyValue)

! dimension test
! --------------
    CALL dimtst(1,1,2,'rdirap','maxcal',                          &
                'antenna phase center pattern',                   &
                'Parameter is defined in module "P_GPSEST.f90".', &
                nancal, maxcal, irc)

    DO ii = 1, SIZE(keyValue)
      READ(keyValue(ii),*, iostat=ios) hlpStr(1)
      IF (ios /= 0) nancal = nancal - 1

      IF (ios == 0) THEN
        antcal(1,ii) = ' '
        antcal(2,ii) = hlpStr(1)

        numcal(1,ii) = 0
        numcal(2,ii) = 999999
        prncal(ii)   = rapprn0
        nfrcal(ii)   = rapfreq
        sigcal(ii)   = rapSigma
        IF (rapMod == 1) THEN
          nptcal(1,ii) = IDNINT(rapzenmax/pi*180.D0/DBLE(rapDele))+1
          nptcal(2,ii) = IDNINT(360.D0/DBLE(rapDazi))+1
        ELSEIF (rapMod == 2) THEN
          nptcal(1,ii) = rapHarmn
          nptcal(2,ii) = rapHarmm
        ENDIF
      ENDIF
    ENDDO
    IF (nancal /= SIZE(keyValue)) CALL exitrc(2)

! read the reference antennas (rec.-dep. groups)
! ----------------------------------------------
    CALL readkeys('RAPREFGI',keyValue,irc)

    DO ii = 1, SIZE(keyValue)
      READ(keyValue(ii),*, iostat=ios) hlpStr(1)
!      IF (ios /= 0)
      IF (ios == 0) THEN
        iSta = 1
        DO WHILE (iSta <= nancal)
          IF (hlpStr(1) == antcal(2,iSta)) THEN
            DO jSta = iSta, nancal-1
              antcal(1,jSta) = antcal(1,jSta+1)
              antcal(2,jSta) = antcal(2,jSta+1)
              numcal(1,jSta) = numcal(1,jSta+1)
              numcal(2,jSta) = numcal(2,jSta+1)
              nptcal(1,jSta) = nptcal(1,jSta+1)
              nptcal(2,jSta) = nptcal(2,jSta+1)
              sigcal(jSta)   = sigcal(jSta+1)
              nfrcal(jSta)   = nfrcal(jSta+1)
              prncal(jSta)   = prncal(jSta+1)
            ENDDO
            nancal = nancal - 1
          ENDIF
          iSta = iSta + 1
        ENDDO
      ENDIF
    ENDDO

! receiver-dependent antenna group selection
! ------------------------------------------
  ELSEIF (rapSel == 4) THEN
    CALL readkeys('RAPGRP',keyValue,irc)
    nancal = SIZE(keyValue)

! dimension test
! --------------
    CALL dimtst(1,1,2,'rdirap','maxcal',                          &
                'antenna phase center pattern',                   &
                'Parameter is defined in module "P_GPSEST.f90".', &
                nancal, maxcal, irc)

    DO ii = 1, SIZE(keyValue)
      READ(keyValue(ii),*, iostat=ios) (hlpStr(jj), jj=1,2)
      IF (ios /= 0) nancal = nancal - 1

      IF (ios == 0) THEN
        antcal(1,ii) = hlpStr(2)
        antcal(2,ii) = hlpStr(1)

        numcal(1,ii) = 0
        numcal(2,ii) = 999999
        prncal(ii)   = rapprn0
        nfrcal(ii)   = rapfreq
        sigcal(ii)   = rapSigma
        IF (rapMod == 1) THEN
          nptcal(1,ii) = IDNINT(rapzenmax/pi*180.D0/DBLE(rapDele))+1
          nptcal(2,ii) = IDNINT(360.D0/DBLE(rapDazi))+1
        ELSEIF (rapMod == 2) THEN
          nptcal(1,ii) = rapHarmn
          nptcal(2,ii) = rapHarmm
        ENDIF
      ENDIF
    ENDDO
    IF (nancal /= SIZE(keyValue)) CALL exitrc(2)

! read the reference antennas (rec.-dep. groups)
! ----------------------------------------------
    CALL readkeys('RAPREFG',keyValue,irc)

    DO ii = 1, SIZE(keyValue)
      READ(keyValue(ii),*, iostat=ios) (hlpStr(jj), jj=1,2)
!      IF (ios /= 0)
      IF (ios == 0) THEN
        iSta = 1
        DO WHILE (iSta <= nancal)
          IF (hlpStr(2) == antcal(1,iSta) .AND. &
              hlpStr(1) == antcal(2,iSta)) THEN
            DO jSta = iSta, nancal-1
              antcal(1,jSta) = antcal(1,jSta+1)
              antcal(2,jSta) = antcal(2,jSta+1)
              numcal(1,jSta) = numcal(1,jSta+1)
              numcal(2,jSta) = numcal(2,jSta+1)
              nptcal(1,jSta) = nptcal(1,jSta+1)
              nptcal(2,jSta) = nptcal(2,jSta+1)
              sigcal(jSta)   = sigcal(jSta+1)
              prncal(jSta)   = prncal(jSta+1)
              nfrcal(jSta)   = nfrcal(jSta+1)
            ENDDO
            nancal = nancal - 1
          ENDIF
          iSta = iSta + 1
        ENDDO
      ENDIF
    ENDDO

  ENDIF

! Change sign in case of spherical harmonics mode
! -----------------------------------------------
  IF (rapMod == 2) THEN
    nptcal(1,1:nancal) = -nptcal(1,1:nancal)
  ENDIF

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE rdirap

END MODULE
