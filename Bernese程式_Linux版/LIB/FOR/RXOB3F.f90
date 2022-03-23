MODULE s_RXOB3F
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rxob3f(isasys, icsflg, nflinp, filinp, nflout, filcod, &
                  filpha, scrobs, window, iMea  , reqObs)

! -------------------------------------------------------------------------
!
! Purpose:    This is a new version of the old subroutine RXOB3F.f that
!             reads the names of input and output files of program RXOBV3
!
! Author:     L. Mervart
!
! Created:    11-Apr-2000
!
! Changes:    14-Dec-2000 ??: Added scratch files for the observations
!             20-Dec-2000 ??: Add type range,
!                             export session from input file (if it was active)
!             14-Mar-2001 ??: CSESS(2), only 1 character
!             26-Oct-2001 RD: Separate input fields for EXT_RXO and EXT_SMT
!             15-Apr-2003 RD: Use GTTIMWIN for reading the time window
!             24-Apr-2003 RD: General RXOBV3 update
!             08-Feb-2006 RD: Print the list of input RINEX files
!             24-Jul-2009 DT: Add iMea to parameter list
!             02-Feb-2012 RD: Define requirements on a Bernese obs. file
!             15-May-2012 RD: No epoch conditions for SLR, use m_bern with only
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules:
  USE m_bern,   ONLY: i4b, r8b, lfnprt, lfnerr,   &
                      fileNameLength, lineLength, &
                      keyNameLength, keyValueLength
  USE m_global, ONLY: maxsys, g_strsys3, g_strsys
  USE p_rxobv3, ONLY: t_rxobv3_req, init_rxobv3_req

  USE s_gtfile2
  USE s_dimtst
  USE s_gttimwin
  USE s_exitrc
  USE s_readkeys
  USE s_ckoptb
  USE s_ckopti
  USE s_gtflna
  USE s_prfile
  USE f_tstkey
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                     :: icsflg  ! accept rinex cycle slip flags
  INTEGER(i4b)                     :: isasys  ! satellite system to be considered
                                              ! 0: ALL
                                              ! 1: GPS
                                              ! 2: GLONASS
                                              ! 3: GALILEO
                                              ! 4: GPS/GAL
                                              ! 5: GPS/GLO
                                              ! 6: GAL/GLO

! output:
  INTEGER(i4b)                     :: nflinp  ! Number of input files
  CHARACTER(LEN=*), DIMENSION(*)   :: filinp  ! List of input files
  INTEGER(i4b)                     :: nflout  ! Number of output files
  CHARACTER(LEN=*), DIMENSION(2,*) :: filcod  ! List of code output files
  CHARACTER(LEN=*), DIMENSION(2,*) :: filpha  ! List of phase output files
  CHARACTER(LEN=*), DIMENSION(2)   :: scrobs  ! Names of the scratch files
  REAL(r8b),        DIMENSION(2,*) :: window  ! Time window for each file
  INTEGER(i4b)                     :: iMea    ! =1 GNSS observations
                                              ! =2 Range observations
  TYPE(t_rxobv3_req)               :: reqObs  ! Define requirements on an obs. file

! List of functions
! -----------------

! Local types
! -----------

! Local Parameter
! ---------------
  CHARACTER(LEN=6), PARAMETER      :: srName = 'rxob3f'

  INTEGER(i4b)    , PARAMETER      :: savcod = 1
  INTEGER(i4b)    , PARAMETER      :: savphs = 2
  INTEGER(i4b)    , PARAMETER      :: savrng = 4

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:),   POINTER :: keyValue
  CHARACTER(LEN=fileNameLength)                          :: auxfil
  CHARACTER(LEN=filenamelength), DIMENSION(:,:), POINTER :: filnam
  CHARACTER(LEN=lineLength)                              :: line
  CHARACTER(LEN=keyNameLength)                           :: satKey
  CHARACTER(LEN=keyNameLength)                           :: phaKey
  CHARACTER(LEN=keyNameLength)                           :: codKey

  INTEGER(i4b)                                           :: meaTyp
  INTEGER(i4b)                                           :: iType
  INTEGER(i4b)                                           :: iRadio
  INTEGER(i4b)                                           :: iSys
  INTEGER(i4b)                                           :: irCode
  INTEGER(i4b)                                           :: irc

  REAL(r8b),        DIMENSION(2)                         :: localWin

  LOGICAL,          DIMENSION(MAXSYS)                    :: doIt

! Commons
! -------
  CHARACTER(LEN=6) :: mxnfil
  INTEGER(i4b)     :: mxcfil

  COMMON/mcmfil/mxcfil,mxnfil


! Init variables
! --------------
  irCode=0

  NULLIFY(filnam)
  NULLIFY(keyValue)

! Read the Time Window
! --------------------
  CALL gtTimWin(' ',(/'RADIO_0','RADIO_1','RADIO_2'/),           &
                (/'SESSION_YEAR','SESSION_STRG'/),               &
                (/'STADAT', 'STATIM', 'ENDDAT', 'ENDTIM'/),      &
                localWin)


! Read the measurement type
! -------------------------
  meaTyp=0

  CALL ckoptb(1,(/'RADIO_G','RADIO_R'/),srName,   &
              'Measurement types to save',irCode, &
              result1=iMea)

  ! Range:
  IF (iMea == 2) THEN
    meaTyp = meaTyp + savrng

  ! Code / Phase:
  ELSE

    ! Code
    CALL ckoptb(1,(/'SAVCOD'/),srName,           &
                'Save code measurements',irCode, &
                result1=iRadio)

    meaTyp = MeaTyp + iRadio*savcod

    ! Phase
    CALL ckoptb(1,(/'SAVPHS'/),srName,            &
                'Save phase measurements',irCode, &
                result1=iRadio)

    meaTyp = MeaTyp + iRadio*savphs
  ENDIF


! Define requirements on a Bernese observation file
! -------------------------------------------------
  CALL init_rxobv3_req(reqObs)

! Minimum number of epochs per file
! (old style, before extension of the requirements)
! -------------------------------------------------
  IF ( .NOT. tstKey('IREQ') ) THEN

    IF (MOD(meaTyp/savrng,2) == 0) THEN
      CALL readkeys('REQEPO', keyValue, irc)

      CALL ckopti(1,'REQEPO', keyValue, srName,                             &
                  'Minimum number of epochs requested per file',irc,irCode, &
                  maxVal=1,empty=0,ge=0,result1=reqObs%reqepo)
      IF ( reqObs%reqepo > 0 ) reqObs%iReq = .TRUE.
    ENDIF

  ELSE

    IF (MOD(meaTyp/savrng,2) == 0) THEN
      CALL ckoptb(1,(/'IREQ'/),srName,                                     &
                  'Conditions to write a Bernese obs. file',irCode,        &
                  resultL=reqObs%iReq)
    ENDIF

! Minimum number of epochs requested
! ----------------------------------
    IF ( reqObs%iReq ) THEN
      CALL readkeys('REQEPO' , keyValue, irc)

      CALL ckopti(1,'REQEPO' , keyValue, srName,                             &
                  'Minimum number of epochs requested per file',irc,irCode,  &
                  maxVal=1,ge=0,empty=0,result1=reqObs%reqepo)
    ENDIF

! Maximumm number of ambiguities allowed per file
! -----------------------------------------------
    IF ( reqObs%iReq .AND. MOD(meaTyp/savphs,2) == 1 .AND. icsflg == 1) THEN
      CALL readkeys('REQAMB' , keyValue, irc)

      CALL ckopti(1,'REQAMB' , keyValue, srName,                            &
                  'Maximum number of ambiguities per file',irc,irCode,      &
                  maxVal=1,ge=0,empty=0,result1=reqObs%reqamb)
    ENDIF

! Allow stations with only phase data
! -----------------------------------
    IF ( reqObs%iReq .AND. MOD(meaTyp/savphs,2) == 1) THEN
      IF (MOD(meaTyp/savcod,2) == 1) THEN
        CALL ckoptb(1,(/'REQPHAONLY'/),srName,                    &
                    'Allow stations with only phase data',irCode, &
                    result1=reqObs%phonly)
      ELSE
        reqObs%phonly = 1
      ENDIF
    ENDIF

! Allow stations with only code data
! ----------------------------------
    IF ( reqObs%iReq .AND. MOD(meaTyp/savcod,2) == 1) THEN
      IF (MOD(meaTyp/savphs,2) == 1) THEN
        CALL ckoptb(1,(/'REQCODONLY'/),srName,                    &
                    'Allow stations with only code data',irCode,  &
                    result1=reqObs%cdonly)
      ELSE
        reqObs%cdonly = 1
      ENDIF
    ENDIF

! Minimum number of satellites/observaations per system
! -----------------------------------------------------
    IF ( reqObs%iReq ) THEN

      ! Number of satellites
      CALL readkeys('REQSATALL', keyValue, irc)

      CALL ckopti(1, 'REQSATALL', keyValue, srName,               &
                  'Minimum number of satellites for all systems', &
                  irc,irCode,                                     &
                  maxVal=1,ge=0,empty=0,result1=reqObs%minnum(1,0))

      ! Number of phase measurements
      IF ( MOD(meaTyp/savphs,2) == 1) THEN
        CALL readkeys('REQPHAALL' , keyValue, irc)

        CALL ckopti(1, 'REQPHAALL', keyValue, srName,                     &
                    'Minimum number of phase observ. for all GNSS',       &
                    irc,irCode,                                           &
                    maxVal=1,ge=0,empty=0,result1=reqObs%minnum(3,0))
      ENDIF

      ! Number of code measurements
      IF ( MOD(meaTyp/savcod,2) == 1) THEN
        CALL readkeys('REQCODALL' , keyValue, irc)

        CALL ckopti(1, 'REQCODALL', keyValue, srName,                     &
                    'Minimum number of code observ. for all GNSS',        &
                    irc,irCode,                                           &
                    maxVal=1,ge=0,empty=0,result1=reqObs%minnum(2,0))
      ENDIF


      DO iSys = 1,maxsys
        satKey = 'REQSAT' // g_strsys3(iSys-1)
        phaKey = 'REQPHA' // g_strsys3(iSys-1)
        codKey = 'REQCOD' // g_strsys3(iSys-1)

        IF (.NOT. tstkey(satKey) ) THEN
          doIt(iSys) = .FALSE.
          CYCLE
        ENDIF

        doIt(iSys) = (isasys == 0)               ! ALL

        IF ( .NOT. doIt(iSys) .AND. iSys == 1 ) THEN
          doIt(iSys) = doIt(iSys) .OR. (isasys == 1)   ! GPS
          doIt(iSys) = doIt(iSys) .OR. (isasys == 4)   ! GPS/GAL
          doIt(iSys) = doIt(iSys) .OR. (isaSys == 5)   ! GPS/GLO
        ENDIF

        IF ( .NOT. doIt(iSys) .AND. iSys == 2 ) THEN
          doIt(iSys) = doIt(iSys) .OR. (isasys == 2)   ! GLONASS
          doIt(iSys) = doIt(iSys) .OR. (isasys == 5)   ! GPS/GLO
          doIt(iSys) = doIt(iSys) .OR. (isaSys == 6)   ! GAL/GLO
        ENDIF

        IF ( .NOT. doIt(iSys) .AND. iSys == 3 ) THEN
          doIt(iSys) = doIt(iSys) .OR. (isasys == 3)   ! Galileo
          doIt(iSys) = doIt(iSys) .OR. (isasys == 4)   ! GPS/GAL
          doIt(iSys) = doIt(iSys) .OR. (isaSys == 6)   ! GAL/GLO
        ENDIF

        IF ( .NOT. doIt(iSys) ) CYCLE

        ! Number of satellites
        CALL readkeys(TRIM(satKey), keyValue, irc)

        CALL ckopti(1, satKey, keyValue, srName,                    &
                    'Minimum number of satellites for ' //          &
                    g_strsys(iSys-1),irc,irCode,                    &
                    maxVal=1,ge=0,empty=0,result1=reqObs%minnum(1,iSys))

        ! Number of phase measurements
        IF ( MOD(meaTyp/savphs,2) == 1) THEN
          CALL readkeys(TRIM(phaKey) , keyValue, irc)

          CALL ckopti(1, phaKey, keyValue, srName,                          &
                      'Minimum number of phase observ. for ' //             &
                      g_strsys(iSys-1),irc,irCode,                          &
                      maxVal=1,ge=0,empty=0,result1=reqObs%minnum(3,iSys))
        ENDIF

        ! Number of code measurements
        IF ( MOD(meaTyp/savcod,2) == 1) THEN
          CALL readkeys(TRIM(codKey) , keyValue, irc)

          CALL ckopti(1, codKey, keyValue, srName,                           &
                      'Minimum number of code observ. for ' //               &
                      g_strsys(iSys-1),irc,irCode,                           &
                      maxVal=1,ge=0,empty=0,result1=reqObs%minnum(2,iSys))
        ENDIF

      ENDDO

    ENDIF

  ENDIF

! Report input options
! --------------------
  IF ( reqObs%iReq ) THEN

    WRITE(lfnprt,'(2(/,A))') &
    ' REQUIREMENTS TO WRITE BERNESE OBSERVATION FILES:', &
    ' -----------------------------------------------'

    ! Minimum number of epochs per file
    line = ' Minimum number of epochs required per file    : disabled'
    IF (reqObs%reqEpo /= 0) WRITE(line(49:80),'(I9)') reqObs%reqEpo
    WRITE(lfnprt,'(A)') TRIM(line)

    ! Maximum number of ambiguities per file
    IF ( MOD(meaTyp/savphs,2) == 1 .AND. icsflg == 1) THEN
      line = ' Maximum number of ambiguities allowed per file: disabled'
      IF (reqObs%reqAmb /= 0) WRITE(line(49:80),'(I9)') reqObs%reqAmb
      WRITE(lfnprt,'(A)') TRIM(line)
    ENDIF

    ! Minimum number of satellites per file for each GNSS
    WRITE(lfnprt,'(/,A)') &
          ' Minimum number of satellites required per file and GNSS'
    line =  '                              for all systems  : disabled'
    IF (reqObs%minnum(1,0) /= 0) &
        WRITE(line(49:80),'(I9)') reqObs%minnum(1,0)
    WRITE(lfnprt,'(A)') TRIM(line)

    DO iSys = 1,maxSys
      IF (.NOT. doIt(iSys)) CYCLE

      line =  '                              for ' // g_strsys(iSys-1)
      IF (reqObs%minnum(1,iSys) /= 0) THEN
        WRITE(line(48:80),'(A,I9)') ':', reqObs%minnum(1,iSys)
      ELSE
        WRITE(line(48:80),'(A)') ': disabled'
      ENDIF
      WRITE(lfnprt,'(A)') TRIM(line)
    ENDDO

    ! Minimum number of phase observations per file for each GNSS
    WRITE(lfnprt,'(A)') &
    ' Minimum number of phase observations required per file and GNSS'
    line =  '                              for all systems  : disabled'
    IF (reqObs%minnum(3,0) /= 0) &
        WRITE(line(49:80),'(I9)') reqObs%minnum(3,0)
    WRITE(lfnprt,'(A)') TRIM(line)

    DO iSys = 1,maxSys
      IF (.NOT. doIt(iSys)) CYCLE

      line =  '                              for ' // g_strsys(iSys-1)
      IF (reqObs%minnum(3,iSys) /= 0) THEN
        WRITE(line(48:80),'(A,I9)') ':', reqObs%minnum(3,iSys)
      ELSE
        WRITE(line(48:80),'(A)') ': disabled'
      ENDIF
      WRITE(lfnprt,'(A)') TRIM(line)
    ENDDO

    ! Minimum number of code observations per file for each GNSS
    WRITE(lfnprt,'(A)') &
    ' Minimum number of code observations required per file and GNSS'
    line =  '                              for all systems  : disabled'
    IF (reqObs%minnum(2,0) /= 0) &
        WRITE(line(49:80),'(I9)') reqObs%minnum(2,0)
    WRITE(lfnprt,'(A)') TRIM(line)

    DO iSys = 1,maxSys
      IF (.NOT. doIt(iSys)) CYCLE

      line =  '                              for ' // g_strsys(iSys-1)
      IF (reqObs%minnum(2,iSys) /= 0) THEN
        WRITE(line(48:80),'(A,I9)') ':',reqObs%minnum(2,iSys)
      ELSE
        WRITE(line(48:80),'(A)') ': disabled'
      ENDIF
      WRITE(lfnprt,'(A)') TRIM(line)
    ENDDO

    WRITE(lfnprt,*)

    ! Allow stations with only phase data
    IF ( MOD(meaTyp/savphs,2) == 1) THEN
      line = ' Allow stations with only phase data           : no'
      IF (reqObs%phOnly == 1) WRITE(line(49:80),'(A)') 'yes'
      WRITE(lfnprt,'(A)') TRIM(line)
    ENDIF


    ! Allow stations with only code data
    IF ( MOD(meaTyp/savcod,2) == 1) THEN
      line = ' Allow stations with only code data            : no'
      IF (reqObs%cdOnly == 1) WRITE(line(49:80),'(A)') 'yes'
      WRITE(lfnprt,'(A)') TRIM(line)
    ENDIF

    IF ( MOD(meaTyp/savphs,2) == 1 .OR. MOD(meaTyp/savcod,2) == 1) &
      WRITE(lfnprt,'(A,/)') ''

  ELSE
    WRITE(lfnprt,'(A,//)') ' No requirements regarding a minimum ' // &
                          'content of Bernese observation files'
  ENDIF


! Read the Names of RINEX Files
! -----------------------------
  CALL ckoptb(1,(/ 'RADIO_O','RADIO_S' /), srName,              &
              'Selection for type of RINEX input files',irCode, &
              result1=iType)

  IF (iType == 1) THEN
    CALL gtfile2('RXOFILE',7,nflinp,filnam)
    CALL prfile('RXOFILE','RINEX files to be imported',1,132)
  ELSE
    CALL gtfile2('SMTFILE',7,nflinp,filnam)
    CALL prfile('SMTFILE','Smoothed RINEX files to be imported',1,132)
  ENDIF

  IF (nflinp == 0) CALL exitrc(0)

  CALL dimtst(1,2,2,srName,'mxcfil','number of files',' ',nflinp,mxcfil,irc)

! Set RINEX input file names
! --------------------------
  filinp(1:nflinp) = filnam(1,1:nflinp)

! Set all time windows
! --------------------
  window(1,1:nflinp) = localWin(1)
  window(2,1:nflinp) = localWin(2)

! Generate the output files
! -------------------------
  nflout = nflinp

  filcod(1:2,1:mxcfil) = ' '
  filpha(1:2,1:mxcfil) = ' '

  ! Code
  IF (MOD(meaTyp/savcod,2) == 1) THEN
    filcod(1,1:nflout) = filnam(2,1:nflout)
    filcod(2,1:nflout) = filnam(3,1:nflout)
  ENDIF

  ! Phase
  IF (MOD(meaTyp/savphs,2) == 1) THEN
    filpha(1,1:nflout) = filnam(4,1:nflout)
    filpha(2,1:nflout) = filnam(5,1:nflout)
  ENDIF

  ! Range
  IF (MOD(meaTyp/savrng,2) == 1) THEN
    filcod(1,1:nflout) = filnam(6,1:nflout)
    filcod(2,1:nflout) = filnam(7,1:nflout)
  ENDIF


! Read the scratch files
! ----------------------
  CALL gtflna(1,'AUXFIL',auxfil, irc)
  CALL gtflna(1,'AUXCOD',scrobs(1), irc)
  CALL gtflna(1,'AUXPHS',scrobs(2), irc)

! Check the names of the scratch files
! ------------------------------------
  IF (auxfil    == scrobs(1)  .OR.   &
      auxfil    == scrobs(2)  .OR.   &
      scrobs(1) == scrobs(2)) THEN
    WRITE (lfnerr,'(/,A,3(/,16X,A,A),/)')                              &
      ' *** SR RXOB3F: The scratch files for RXOBV3 are identical.',   &
      'scratch file 1: ',TRIM(auxfil),                                 &
      'scratch file 2: ',TRIM(scrobs(1)),                              &
      'scratch file 3: ',TRIM(scrobs(2))
    irCode=irCode+1
  ENDIF

! Stop if something was wrong
! ---------------------------
  IF (irCode /= 0) CALL exitrc(2)

! Deallocate local pointers
! -------------------------
  DEALLOCATE(filnam,stat=irc)
  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE rxob3f


END MODULE
