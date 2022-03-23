MODULE s_RDIRAO
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdirao(maxcal, isasys, nanrao, antrao, numrao, &
                  sigrao, prnrao, nfrrao, neurao)

! -------------------------------------------------------------------------
! Purpose:    Reads the receiver antenna offset input options for GPSEST
!
! Author:     R. Dach
!
! Created:    20-Jun-2001
! Last mod.:  10-Mar-2008
!
! Changes:    01-Mar-2003  DS: Set RAON,RAOE,RAOU to 1 when they are used
!             01-Apr-2003  HU: Comment in DIMTST adapted
!             23-Apr-2003  RD: Nullify local pointers
!             08-Sep-2003  HU: antnam, recnam chr16 -> chr20
!             28-Oct-2003  RS: Selection of reference antennas, receiver-
!                              independent antenna groups
!             19-Jan-2003  SS/MM: Revision of GPSEST input panels
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
  USE s_dimtst
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckoptr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                      :: maxcal ! maximum number of antenna
                                              ! phase center parameters
  INTEGER(i4b)                      :: isasys ! satellite system to be considered
                                              ! = 0: ALL
                                              ! = 1: GPS
                                              ! = 2: GLONASS
                                              ! = 3: Galileo
                                              ! = 4: GPS+GLONASS
                                              ! = 5: GPS+Galileo
                                              ! = 6: GLONASS+Galileo

! output:
  INTEGER(i4b)                      :: nanrao ! number of receiver
                                              ! antenna offsets
  CHARACTER(LEN=20), DIMENSION(2,*) :: antrao ! receiver and antenna
                                              ! name for request i
  INTEGER(i4b),      DIMENSION(2,*) :: numrao ! antenna numbers
                                              ! "from - to" for request i
  INTEGER(i4b),      DIMENSION(*)   :: prnrao ! satellite system (see g_svnsys)
  INTEGER(i4b),      DIMENSION(*)   :: nfrrao ! frequency for
                                              ! receiver ant. offset request i
  REAL(r8b),         DIMENSION(2,*) :: sigrao ! a priori sigmas in meters
                                              ! j=1: horizontal components
                                              ! j=2: vertical component
  INTEGER(i4b),      DIMENSION(3)   :: neurao ! components to be estimated
                                              ! (i=1: north, i=2: east, i=3: up)
                                              ! =1: estimation
                                              ! =0: no estimation

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER     :: srName = 'rdirao'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength) , &
         DIMENSION(:)  , POINTER  :: keyValue
  CHARACTER(LEN=shortLineLength), &
         DIMENSION(8)             :: hlpStr

  INTEGER(i4b)                    :: raoSel
  INTEGER(i4b)                    :: raoFreq, raoFreq0
  INTEGER(i4b)                    :: raoNum, raoPrn0
  INTEGER(i4b)                    :: ista, jSta
  INTEGER(i4b)                    :: irc, ios
  INTEGER(i4b)                    :: irCode = 0
  INTEGER(i4b)                    :: ii, jj

  REAL(r8b)                       :: raoSigH, raoSigV

! Init variables
! --------------
  NULLIFY(keyValue)
  irCode = 0

! Components to be estimated
! --------------------------
  neurao(1:3) = 0
  CALL ckoptb(1, (/ 'RAON' /), srName, &
              'Receiver antenna offset: north', irCode, result1=neurao(1))
  CALL ckoptb(1, (/ 'RAOE' /), srName, &
              'Receiver antenna offset: east',  irCode, result1=neurao(2))
  CALL ckoptb(1, (/ 'RAOU' /), srName, &
              'Receiver antenna offset: up',    irCode, result1=neurao(3))

  IF ( irCode /= 0 ) CALL exitrc(2)
  IF ( neurao(1) == 0 .AND. neurao(2) == 0 .AND. neurao(3) == 0 ) RETURN

! Read the frequency to be estimated
! ----------------------------------
  CALL readKeys('FREQUENCY', keyValue, irc)
  CALL ckoptc(1,'FREQUENCY', keyValue,                                    &
              (/ 'L1   ', 'L2   ', 'L3   ', 'L4   ', 'L5   ', 'L1&L2' /), &
              srName, 'Frequency selection', irc, irCode,                 &
              other = 0, maxVal = 1, valList = (/ 1,2,4,5,4,3 /),         &
              result1 = raoFreq0)

  raoFreq = raoFreq0
  IF (raoFreq == 3) THEN
    CALL readKeys('RAOFRQ', keyValue, irc)
    CALL ckoptc(1,'RAOFRQ', keyValue,                                        &
              (/ 'L1   ', 'L2   ', 'BOTH ', 'L1&L2' /),                      &
              srName, 'Rec. ant. offsets: Frequency selection', irc, irCode, &
              maxVal = 1, valList = (/ 1,2,3,4 /), result1 = raoFreq)
  ENDIF


! Read the satellite system to be estimated
! -----------------------------------------
  raoPrn0 = isasys - 1
  IF (raoPrn0 == -1) raoPrn0 = 10
  IF (isasys /= 1 .AND. isasys /= 2 .AND. isasys /= 3) THEN
    CALL readKeys('RAOPRN', keyValue, irc)
    CALL ckoptc(1,'RAOPRN', keyValue, (/ 'GPS    ', 'GLONASS', 'GALILEO', 'GNSS   ' /), &
              srName, 'Rec. ant. offsets: Satellite system', irc, irCode,    &
              maxVal = 1, valList = (/ 0,1,2,10 /), result1 = raoPrn0)
    IF ( ( raoPrn0 == 0 .AND. isasys == 6 ) .OR. &
         ( raoPrn0 == 1 .AND. isasys == 5 ) .OR. &
         ( raoPrn0 == 2 .AND. isasys == 4 ) ) THEN
      neurao = 0
      RETURN
    ENDIF
  ENDIF


! Read a priori sigmas
! --------------------
  raoSigH = 0d0
  IF (neurao(1) /= 0 .OR. neurao(2) /= 0) THEN
    CALL readKeys('RAOSIGH', keyValue, irc)
    CALL ckoptr(1,'RAOSIGH', keyValue, srName,                        &
                  'Rec. ant. offsets: Horiz. sigma', irc, irCode,     &
                  maxVal = 1, empty = 0d0, ge = 0d0, result1 = raoSigH )
  ENDIF

  raoSigV = 0d0
  IF (neurao(3) /= 0) THEN
    CALL readKeys('RAOSIGV', keyValue, irc)
    CALL ckoptr(1,'RAOSIGV', keyValue, srName,                        &
                  'Rec. ant. offsets: Vert. sigma', irc, irCode,      &
                  maxVal = 1, empty = 0d0, ge = 0d0, result1 = raoSigV )
  ENDIF


! Which type of selection was set?
! --------------------------------
  CALL readKeys('RAOSEL', keyValue, irc)
  CALL ckoptc(1,'RAOSEL', keyValue, &
              (/'RECEIVER-INDEPENDENT','RECEIVER-DEPENDENT  ', &
                'INDIVIDUAL          ','MANUAL              '/), &
              'rdirao', 'RAO setup', irc, irCode, &
              valList=(/3,4,2,1/), result1=raoSel)

  IF (irCode /= 0) CALL exitrc(2)

! read the manual input
! ---------------------
  IF (raoSel == 1) THEN
    CALL readKeys('RAOSTR', keyValue, irc)
    nanrao = SIZE(keyValue)

! dimension test
! --------------
    CALL dimtst(1,1,2,'rdirao','maxcal',                          &
                'receiver antenna offset',                        &
                'Parameter is defined in module "P_GPSEST.f90".', &
                nanrao, maxcal, irc)

! Extract the request strings
! ---------------------------
    DO ii = 1, SIZE(keyValue)
      READ(keyValue(ii),*, iostat=ios) (hlpStr(jj), jj=1,8)
      IF (ios /= 0) nanrao = nanrao - 1

      IF (ios == 0) THEN
        antrao(1,ii) = hlpStr(2)
        antrao(2,ii) = hlpStr(1)

        READ(hlpStr(3), *, iostat=ios) numrao(1,ii)
        IF (ios /= 0) THEN
          WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')         &
            ' *** SR RDIRAO: Wrong antenna number specification for ', &
                            'receiver antenna offset estimation',      &
                            'Number of requests:    ',ii,              &
                            'Specified value:       ',TRIM(hlpStr(3))
          nanrao = nanrao - 1
        ENDIF

        READ(hlpStr(4), *, iostat=ios) numrao(2,ii)
        IF (ios /= 0) THEN
          WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')         &
            ' *** SR RDIRAO: Wrong antenna number specification for ', &
                            'receiver antenna offset estimation',      &
                            'Number of requests:    ',ii,              &
                            'Specified value:       ',TRIM(hlpStr(4))
          nanrao = nanrao - 1
        ENDIF

        nfrrao(ii) = -1
        IF ((raoFreq0 == 1 .OR. raoFreq0 == 3) .AND. &
            (hlpStr(5) == 'L1' .OR. hlpStr(5) == '1')) THEN
          nfrrao(ii) = 1
        ELSE IF ((raoFreq0 == 2 .OR. raoFreq0 == 3) .AND. &
             (hlpStr(5) == 'L2' .OR. hlpStr(5) == '2')) THEN
          nfrrao(ii) = 2
        ELSE IF ( raoFreq0 == 3 .AND. hlpStr(5) == 'BOTH') THEN
          nfrrao(ii) = 3
        ELSE IF ((raoFreq0 == 3 .OR. raoFreq0 == 4) .AND. &
             (hlpStr(5) == 'L3' .OR. hlpStr(5) == 'L5' .OR. &
              hlpStr(5) == '3'  .OR. hlpStr(5) == '5'  .OR. &
              hlpStr(5) == 'L1&L2')) THEN
          nfrrao(ii) = 4
        ELSE IF (raoFreq0 == 5 .AND. &
             (hlpStr(5) == 'L4' .OR. hlpStr(5) == '4')) THEN
          nfrrao(ii) = 5
        ENDIF
        IF (nfrrao(ii) == -1) THEN
          nfrrao(ii) = raofreq
          IF (LEN_TRIM(hlpStr(5)) > 0) THEN
            WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')         &
              ' *** SR RDIRAO: Wrong frequency specification for ',      &
                              'receiver antenna offset estimation',      &
                              'Number of requests:    ',ii,              &
                              'Specified value:       ',TRIM(hlpStr(5))
            nanrao = nanrao - 1
          ENDIF
        ENDIF

        prnrao(ii) = -1
        IF((isasys == 0 .OR. isasys == 1 .OR. &
            isasys == 4 .OR. isasys == 5 ) .AND. &
            hlpStr(6) == 'GPS') THEN
          prnrao(ii) = 0
        ELSE IF((isasys == 0 .OR. isasys == 2 .OR. &
            isasys == 4 .OR. isasys == 6 ) .AND. &
            hlpStr(6) == 'GLO' .OR. hlpStr(6) == 'GLONASS' ) THEN
          prnrao(ii) = 1
        ELSE IF((isasys == 0 .OR. isasys == 3 .OR. &
            isasys == 5 .OR. isasys == 6 ) .AND. &
            hlpStr(6) == 'GAL' .OR. hlpStr(6) == 'GALILEO' ) THEN
          prnrao(ii) = 2
        ELSE IF(hlpStr(6) == 'MIX' .OR. hlpStr(6) == 'GNSS' .OR. &
                LEN_TRIM(hlpStr(6)) == 0) THEN
          prnrao(ii) = 10
        ENDIF
        IF (prnrao(ii) == -1) THEN
          prnrao(ii) = raoPrn0
          IF (LEN_TRIM(hlpStr(6)) > 0) THEN
            WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')         &
              ' *** SR RDIRAO: Wrong satellite system specification for ',&
                              'receiver antenna offset estimation',      &
                              'Number of requests:    ',ii,              &
                              'Specified value:       ',TRIM(hlpStr(6))
            nanrao = nanrao - 1
          ENDIF
        ENDIF

        READ(hlpStr(7), *, iostat=ios) sigrao(1,ii)
        IF (ios /= 0) THEN
          sigrao(1,ii) = raoSigH
          IF (LEN_TRIM(hlpStr(7)) > 0) THEN
            WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')         &
              ' *** SR RDIRAO: Wrong horiz. sigma specification for ',   &
                              'receiver antenna offset estimation',      &
                              'Number of requests:    ',ii,              &
                              'Specified value:       ',TRIM(hlpStr(7))
            nanrao = nanrao - 1
          ENDIF
        ENDIF

        READ(hlpStr(8), *, iostat=ios) sigrao(2,ii)
        IF (ios /= 0) THEN
          sigrao(2,ii) = raoSigV
          IF (LEN_TRIM(hlpStr(8)) > 0) THEN
            WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')         &
              ' *** SR RDIRAO: Wrong vert. sigma specification for ',    &
                              'receiver antenna offset estimation',      &
                              'Number of requests:    ',ii,              &
                              'Specified value:       ',TRIM(hlpStr(8))
            nanrao = nanrao - 1
          ENDIF
        ENDIF

      ENDIF
    ENDDO
    IF (nanrao /= SIZE(keyValue)) CALL exitrc(2)

! individual antennas selected
! ----------------------------
  ELSE IF (raoSel == 2) THEN
    CALL readkeys('RAOANT',keyValue,irc)
    nanrao = SIZE(keyValue)

    CALL dimtst(1,1,2,'rdirao','maxcal',                          &
                'receiver antenna offset',                        &
                'Parameter is defined in module "P_GPSEST.f90".', &
                nanrao, maxcal, irc)

    DO ii = 1, SIZE(keyValue)
      READ(keyValue(ii),*, iostat=ios) (hlpStr(jj), jj=1,3)
      IF (ios /= 0) nanrao = nanrao - 1

      IF (ios == 0) THEN
        antrao(1,ii) = hlpStr(2)
        antrao(2,ii) = hlpStr(1)

        READ(hlpStr(3), *, iostat=ios) numrao(1,ii)
        IF (ios /= 0) THEN
          WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')         &
            ' *** SR RDIRAO: Wrong antenna number specification for ', &
                            'receiver antenna offset estimation',      &
                            'Number of requests:    ',ii,              &
                            'Specified value:       ',TRIM(hlpStr(3))
          nanrao = nanrao - 1
        ENDIF

        numrao(2,ii) = numrao(1,ii)
        nfrrao(ii)   = raofreq
        prnrao(ii)   = raoPrn0
        sigrao(1,ii) = raoSigH
        sigrao(2,ii) = raoSigV
      ENDIF
    ENDDO
    IF (nanrao /= SIZE(keyValue)) CALL exitrc(2)

! read the individual reference antennas
! --------------------------------------
    CALL readkeys('RAOREFI',keyValue,irc)

    DO ii = 1, SIZE(keyValue)
      READ(keyValue(ii),*, iostat=ios) (hlpStr(jj), jj=1,3)
!      IF (ios /= 0)
      IF (ios == 0) THEN
        READ(hlpStr(3), *, iostat=ios) raonum
        IF (ios /= 0) THEN
          WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')         &
            ' *** SR RDIRAO: Wrong antenna number specification for ', &
                            'receiver antenna offset estimation',      &
                            'Number of requests:    ',ii,              &
                            'Specified value:       ',TRIM(hlpStr(3))
          CALL exitrc(2)
        ENDIF

        iSta = 1
        DO WHILE (iSta <= nanrao)
          IF (hlpStr(2) == antrao(1,iSta) .AND. &
              hlpStr(1) == antrao(2,iSta) .AND. &
              raonum    == numrao(1,iSta) .AND. &
              raonum    == numrao(2,iSta)) THEN
            DO jSta = iSta, nanrao-1
              antrao(1,jSta) = antrao(1,jSta+1)
              antrao(2,jSta) = antrao(2,jSta+1)
              numrao(1,jSta) = numrao(1,jSta+1)
              numrao(2,jSta) = numrao(2,jSta+1)
              sigrao(1,jSta) = sigrao(1,jSta+1)
              sigrao(2,jSta) = sigrao(2,jSta+1)
              nfrrao(jSta)   = nfrrao(jSta+1)
              prnrao(jSta)   = prnrao(jSta+1)
            ENDDO
            nanrao = nanrao - 1
          ENDIF
          iSta = iSta + 1
        ENDDO
      ENDIF
    ENDDO

! receiver-independent antenna group selection
! --------------------------------------------
  ELSE IF (raoSel == 3) THEN

    CALL readkeys('RAOGRPI',keyValue,irc)
    nanrao = SIZE(keyValue)

    CALL dimtst(1,1,2,'rdirao','maxcal',                          &
                'receiver antenna offset',                        &
                'Parameter is defined in module "P_GPSEST.f90".', &
                nanrao, maxcal, irc)

    DO ii = 1, SIZE(keyValue)
      READ(keyValue(ii),*, iostat=ios) hlpStr(1)
      IF (ios /= 0) nanrao = nanrao - 1

      IF (ios == 0) THEN
        antrao(1,ii) = ' '
        antrao(2,ii) = hlpStr(1)

        numrao(1,ii) = 0
        numrao(2,ii) = 999999
        nfrrao(ii)   = raofreq
        prnrao(ii)   = raoPrn0
        sigrao(1,ii) = raoSigH
        sigrao(2,ii) = raoSigV
      ENDIF
    ENDDO
    IF (nanrao /= SIZE(keyValue)) CALL exitrc(2)

! read the reference antennas (rec.-indep. groups)
! ------------------------------------------------
    CALL readkeys('RAOREFGI',keyValue,irc)

    DO ii = 1, SIZE(keyValue)
      READ(keyValue(ii),*, iostat=ios) hlpStr(1)
!      IF (ios /= 0)
      IF (ios == 0) THEN
        iSta = 1
        DO WHILE (iSta <= nanrao)
          IF (hlpStr(1) == antrao(2,iSta)) THEN
            DO jSta = iSta, nanrao-1
              antrao(1,jSta) = antrao(1,jSta+1)
              antrao(2,jSta) = antrao(2,jSta+1)
              numrao(1,jSta) = numrao(1,jSta+1)
              numrao(2,jSta) = numrao(2,jSta+1)
              sigrao(1,jSta) = sigrao(1,jSta+1)
              sigrao(2,jSta) = sigrao(2,jSta+1)
              nfrrao(jSta)   = nfrrao(jSta+1)
              prnrao(jSta)   = prnrao(jSta+1)
            ENDDO
            nanrao = nanrao - 1
          ENDIF
          iSta = iSta + 1
        ENDDO
      ENDIF
    ENDDO

! receiver-dependent antenna group selection
! ------------------------------------------
  ELSEIF (raoSel == 4) THEN
    CALL readkeys('RAOGRP',keyValue,irc)
    nanrao = SIZE(keyValue)

    CALL dimtst(1,1,2,'rdirao','maxcal',                          &
                'receiver antenna offset',                        &
                'Parameter is defined in module "P_GPSEST.f90".', &
                nanrao, maxcal, irc)

    DO ii = 1, SIZE(keyValue)
      READ(keyValue(ii),*, iostat=ios) (hlpStr(jj), jj=1,2)
      IF (ios /= 0) nanrao = nanrao - 1

      IF (ios == 0) THEN
        antrao(1,ii) = hlpStr(2)
        antrao(2,ii) = hlpStr(1)

        numrao(1,ii) = 0
        numrao(2,ii) = 999999
        nfrrao(ii)   = raofreq
        prnrao(ii)   = raoPrn0
        sigrao(1,ii) = raoSigH
        sigrao(2,ii) = raoSigV
      ENDIF
    ENDDO
    IF (nanrao /= SIZE(keyValue)) CALL exitrc(2)

! read the reference antennas (rec.-dep. groups)
! ----------------------------------------------
    CALL readkeys('RAOREFG',keyValue,irc)

    DO ii = 1, SIZE(keyValue)
      READ(keyValue(ii),*, iostat=ios) (hlpStr(jj), jj=1,2)
!      IF (ios /= 0)
      IF (ios == 0) THEN
        iSta = 1
        DO WHILE (iSta <= nanrao)
          IF (hlpStr(2) == antrao(1,iSta) .AND. &
              hlpStr(1) == antrao(2,iSta)) THEN
            DO jSta = iSta, nanrao-1
              antrao(1,jSta) = antrao(1,jSta+1)
              antrao(2,jSta) = antrao(2,jSta+1)
              numrao(1,jSta) = numrao(1,jSta+1)
              numrao(2,jSta) = numrao(2,jSta+1)
              sigrao(1,jSta) = sigrao(1,jSta+1)
              sigrao(2,jSta) = sigrao(2,jSta+1)
              nfrrao(jSta)   = nfrrao(jSta+1)
              prnrao(jSta)   = prnrao(jSta+1)
            ENDDO
            nanrao = nanrao - 1
          ENDIF
          iSta = iSta + 1
        ENDDO
      ENDIF
    ENDDO

  ENDIF

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE rdirao

END MODULE
