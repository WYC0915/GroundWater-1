MODULE s_MCINPT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE mcinpt(opt)


! -------------------------------------------------------------------------
! Purpose:    Read input options for program mkclus
!
! Author:     R. Dach
!
!
! Created:    13-Jun-2002
! Last mod.:  15-Dec-2005
!
! Changes:    13-Feb-2003  RD: Optimum number of stations
!             19-Mar-2003  RD: Write long string with format (because IFC)
!             23-Apr-2003  AJ: Nullify local pointers
!             15-Dec-2005  RD: A few new options added
!
! SR called:  gtflna,readKeys,ckoptb,ckoptc,ckopti,ckoptr,exitrc,prfile
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE p_mkclus, ONLY: t_mkclus_opt
  USE s_ckoptr
  USE s_prfile
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckopti
  USE s_gtflna
  IMPLICIT NONE

! List of parameters
! ------------------
! input:

! output
  TYPE(t_mkclus_opt)                   :: opt      ! Input options


! List of functions
! -----------------


! Local types
! -----------


! Local parameters
! ----------------
  CHARACTER(LEN=6),              PARAMETER   :: srName = 'mcinpt'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength),              &
                   DIMENSION(:), POINTER     :: keyValue
  CHARACTER(LEN=fileNameLength)              :: rmsFil
  CHARACTER(LEN=fileNameLength)              :: delFil
  CHARACTER(LEN=shortLineLength)             :: line

  INTEGER(i4b)                               :: iType    ! 1:ZD / 2:SD obs.files
  INTEGER(i4b)                               :: irc
  INTEGER(i4b)                               :: irCode

  NULLIFY(keyValue)

  irCode = 0

! Type of observation files
! -------------------------
  CALL ckoptb(1,(/'RADIO_Z','RADIO_S'/),srName,                    &
              'Selection of the obs. type', irCode,                &
              result1=iType)

! Cluster strategy
! ----------------
  IF (iType == 1) THEN
    CALL readKeys('CLUSTRAT_Z',keyValue,irc)
    CALL ckoptc(1,'CLUSTRAT_Z',keyValue,(/'GLOBAL  ','REGIONAL'/),   &
                srName,'Strategy for cluster definition',irc,irCode, &
                maxVal=1,result1=opt%cluStrat)
  ELSE
    opt%cluStrat = 3
  ENDIF


! Read options for global clusters from zero diff. files
! ------------------------------------------------------
  IF (opt%cluStrat == 1) THEN

    ! Number of clusters
    CALL readKeys('NUMCLU_ZG',keyValue,irc)
    CALL ckopti(1,'NUMCLU_ZG',keyValue,srName,                       &
                'Number of clusters',irc,irCode,                     &
                maxVal=1,empty=1,ge=1,result1=opt%numClu)

    ! Automatic number of stations
    CALL ckoptb(1,(/'AUTOSTA_ZG'/),srName,                           &
                'Minimum number of stations', irCode,                &
                result1=opt%maxSta)
    opt%maxSta = -opt%maxSta

    ! Max. number of stations per cluster
    IF (opt%maxSta /= -1) THEN
      CALL readKeys('MAXSTA_ZG',keyValue,irc)
      CALL ckopti(1,'MAXSTA_ZG',keyValue,srName,                       &
                  'Max. number of stations per cluster',irc,irCode,    &
                  maxVal=1,empty=0,ge=0,result1=opt%maxSta)
    ENDIF

    ! Station selection
    CALL readKeys('STADEL_ZG',keyValue,irc)
    CALL ckoptc(1,'STADEL_ZG',keyValue,                              &
                (/'NUM.OBS ','CLOCK   ','GEOMETRY','DENSITY '/),     &
                srName,'Station selection strategy',irc,irCode,      &
                maxVal=1,result1=opt%staStrat)

    ! Error if clock criterium is selected but no RMS file is given...
    IF (opt%staStrat == 2) THEN
      CALL gtflna(0,'RMSFIT',rmsFil,irc)
      IF (irc /= 0) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,/)')                                       &
        ' *** SR MCINPT: For station selection strategy "CLOCK" is a file ',  &
                        'containing the RMS of linear fit for the clocks ' // &
                        'required.'
        irCode = irCode + 1
      ENDIF
    ENDIF

    ! Min. number of obs. per satellite
    opt%satObs = 1
    IF (opt%numClu > 0) THEN

      CALL readKeys('SATOBS_ZG',keyValue,irc)
      CALL ckopti(1,'SATOBS_ZG',keyValue,srName,                       &
                  'Min number of obs. to one sat.',irc,irCode,         &
                  maxVal=1,empty=0,ge=0,result1=opt%satObs)
    ENDIF

    ! Min. number of obs. per satellite
    opt%nSolve = 1
    IF (opt%numClu > 1 .AND. opt%satObs > 1) THEN

      CALL readKeys('NSOLVE_ZG',keyValue,irc)
      CALL ckopti(1,'NSOLVE_ZG',keyValue,srName,                       &
                  'Numb. of cluster with #obs. condition',irc,irCode,  &
                  maxVal=1,empty=0,ge=0,le=opt%numClu,result1=opt%nSolve)
    ENDIF

    ! Satellites with a low number of observ.
    opt%obssat = 0d0
    IF (opt%numClu > 1 .AND. opt%satObs > 1) THEN

      CALL readKeys('OBSSAT_ZG',keyValue,irc)
      CALL ckoptr(1,'OBSSAT_ZG',keyValue,srName,                       &
                  'Satellites with a low number of observ.',irc,irCode,&
                  maxVal=1,empty=0d0,ge=0d0,le=100d0,result1=opt%obssat)
      opt%obssat = opt%obssat / 100d0
    ENDIF

    ! Select satellite system
    CALL readKeys('SATSYS_ZG',keyValue,irc)
    CALL ckoptc(1,'SATSYS_ZG',keyValue,                              &
                (/'ALL    ','GPS    ','GLONASS'/),srName,            &
                'Satellite systems to be considered',irc,irCode,     &
                maxVal=1,valList=(/0,1,2/),result1=opt%isasys)

    ! Sampling for reading the observations
    CALL readKeys('NSAMPL_ZG',keyValue,irc)
    CALL ckopti(1,'NSAMPL_ZG',keyValue,srName,                       &
                'Sampling interval to be considered',irc,irCode,     &
                maxVal=1,empty=0,ge=0,result1=opt%nSampl)

    ! Redundancy per observation in each cluster
    ! (not used for global clusters)
    opt%staObs = 1


! Read options for regional clusters from zero diff. files
! --------------------------------------------------------
  ELSE IF (opt%cluStrat == 2) THEN

    ! Regional cluster setup: MAXSTA or NUMCLU
    CALL ckoptb(1,(/'RADIO_Z1','RADIO_Z2'/),srName,                  &
              'Criterium for building regional clusters', irCode,    &
              result1=iType)

    IF (iType == 1) THEN

      ! Max. number of stations per cluster
      CALL readKeys('MAXSTA_ZR',keyValue,irc)
      CALL ckopti(1,'MAXSTA_ZR',keyValue,srName,                     &
                  'Max. number of stations per cluster',irc,irCode,  &
                  maxVal=1,empty=0,ge=0,result1=opt%maxSta)

      ! Init the other option parameters
      opt%numClu = 0
      opt%satObs = 1
      opt%staObs = 1
      opt%nSolve = 1
      opt%nSampl = 0
      opt%isasys = 0
      opt%obssat = 0d0

    ELSE IF (iType == 2) THEN

      ! Number of clusters
      CALL readKeys('NUMCLU_ZR',keyValue,irc)
      CALL ckopti(1,'NUMCLU_ZR',keyValue,srName,                     &
                  'Number of clusters',irc,irCode,                   &
                  maxVal=1,empty=1,ge=1,result1=opt%numClu)

      ! Redundancy per observation in each cluster
      CALL readKeys('STAOBS_ZR',keyValue,irc)
      CALL ckopti(1,'STAOBS_ZR',keyValue,srName,                       &
                  'Redundancy for each observation',irc,irCode,        &
                  maxVal=1,empty=0,ge=0,result1=opt%staObs)

      ! Select satellite system
      CALL readKeys('SATSYS_ZR',keyValue,irc)
      CALL ckoptc(1,'SATSYS_ZR',keyValue,                              &
                  (/'ALL    ','GPS    ','GLONASS'/),srName,            &
                  'Satellite systems to be considered',irc,irCode,     &
                  maxVal=1,valList=(/0,1,2/),result1=opt%isasys)

      ! Sampling for reading the observations
      CALL readKeys('NSAMPL_ZR',keyValue,irc)
      CALL ckopti(1,'NSAMPL_ZR',keyValue,srName,                       &
                  'Sampling interval to be considered',irc,irCode,     &
                  maxVal=1,empty=0,ge=0,result1=opt%nSampl)

      ! Init the other option parameters
      opt%maxSta = 0
      opt%satObs = 1
      opt%nSolve = 1
      opt%obssat = 0d0

    ENDIF


! Read options for regional clusters from baseline files
! ------------------------------------------------------
  ELSE IF (opt%cluStrat == 3) THEN

    ! Regional cluster setup: MAXSTA or NUMCLU
    CALL ckoptb(1,(/'RADIO_S1','RADIO_S2'/),srName,                  &
              'Criterium for building regional clusters', irCode,    &
              result1=iType)

    IF (iType == 1) THEN

      ! Max. number of stations per cluster
      CALL readKeys('MAXSTA_SR',keyValue,irc)
      CALL ckopti(1,'MAXSTA_SR',keyValue,srName,                     &
                  'Max. number of baselines per cluster',irc,irCode, &
                  maxVal=1,empty=0,ge=0,result1=opt%maxSta)

      ! Init the other option parameters
      opt%numClu = 0
      opt%satObs = 1
      opt%staObs = 1
      opt%nSolve = 1
      opt%nSampl = 0
      opt%isasys = 0
      opt%obssat = 0d0

    ELSE IF (iType == 2) THEN

      ! Number of clusters
      CALL readKeys('NUMCLU_SR',keyValue,irc)
      CALL ckopti(1,'NUMCLU_SR',keyValue,srName,                     &
                  'Number of clusters',irc,irCode,                   &
                  maxVal=1,empty=1,ge=1,result1=opt%numClu)

      ! Init the other option parameters
      opt%maxSta = 0
      opt%satObs = 1
      opt%staObs = 1
      opt%nSolve = 1
      opt%nSampl = 0
      opt%isasys = 0
      opt%obssat = 0d0

    ENDIF

  ENDIF


! Station exclusions (zero diff. case only)
! -----------------------------------------
  opt%maxAmb = 0
  opt%minObs = 0
  opt%maxRms = 0d0
  opt%codDel = 0

  IF (opt%cluStrat == 1 .OR. opt%cluStrat == 2) THEN

    ! Max number of ambiguities allowed per file
    CALL readKeys('MAXAMB',keyValue,irc)
    CALL ckopti(1,'MAXAMB',keyValue,srName,                       &
                'Max number of ambiguities per file',irc,irCode,  &
                maxVal=1,empty=0,ge=0,result1=opt%maxAmb)

    ! Min number of observations for a file
    CALL readKeys('MINOBS',keyValue,irc)
    CALL ckopti(1,'MINOBS',keyValue,srName,                       &
              'Min number of observations',irc,irCode,            &
              maxVal=1,empty=0,ge=0,result1=opt%minObs)

    ! Max RMS for lin fit of the clock allowed for a station
    CALL gtflna(0,'RMSFIT',rmsFil,irc)
    IF (irc == 0) THEN

      CALL readKeys('MAXRMS',keyValue,irc)
      CALL ckoptr(1,'MAXRMS',keyValue,srName,                     &
                  'Max RMS for a station clock',irc,irCode,       &
                  maxVal=1,empty=0d0,ge=0d0,result1=opt%maxRms)
    ENDIF

    ! Delete phase files only or code+phase files
    CALL gtflna(0,'NOTUSE',delFil,irc)
    IF (irc == 0) THEN
      CALL ckoptb(1,(/'DELALL'/),srName,                          &
              'Delete phase or code+phase files', irCode,         &
              result1=opt%codDel)
    ENDIF
  ENDIF

  IF (irCode /= 0) CALL exitrc(2)


! Print the list of files
! -----------------------
  IF (opt%cluStrat == 1 .OR. opt%cluStrat == 2) THEN
    CALL prfile('PZHFILES',' ',2)
  ELSE IF (opt%cluStrat == 3) THEN
    CALL prfile('PSHFILES',' ',2)
  ENDIF


! Print input options
! -------------------

  ! Station/file deletion criteria (zero diff. only)
  IF (opt%cluStrat == 1 .OR. opt%cluStrat == 2) THEN
    WRITE(lfnprt,'(2(1X,A,/))') &
          'STATION/FILE DELETION CRITERIA:','------------------------------'

    line = 'Maximum number of ambiguities allowed per file:           ' // &
           '   not used'
    IF (opt%maxAmb > 0) WRITE(line(60:69),'(I10)') opt%maxAmb
    WRITE(lfnprt,'(1X,A)') TRIM(line)

    line = 'Minimum number of observations expected in a file:        ' // &
           '   not used'
    IF (opt%minObs > 0) WRITE(line(60:69),'(I10)') opt%minObs
    WRITE(lfnprt,'(1X,A)') TRIM(line)

    line = 'Maximum RMS for linear fit of the station clock allowed   ' // &
           '   not used usec'
    IF (opt%maxRMS > 0D0) THEN
      WRITE(line(60:69),'(F10.0)') opt%maxRms
    ELSE
      line(70:75) = ' '
    ENDIF
    WRITE(lfnprt,'(1X,A,//)') TRIM(line)

  ENDIF

  ! Global zero diff. clusters
  IF (opt%cluStrat == 1) THEN
    WRITE(lfnprt,'(2(1X,A,/))') &
          'GLOBAL CLUSTERS FROM ZERO DIFFERENCE FILES:', &
          '------------------------------------------'

    line = 'Number of clusters:'
    WRITE(line(60:69),'(I10)') opt%numClu
    WRITE(lfnprt,'(1X,A,/)') TRIM(line)

    line = 'Maximal number of stations per cluster:                    ' // &
           '  not used'
    IF (opt%maxSta > 0) WRITE(line(60:69),'(I10)') opt%maxSta
    IF (opt%maxSta == -1) &
      line = 'Optimize number of stations per cluster:                 ' // &
             '        used'
    WRITE(lfnprt,'(1X,A)') TRIM(line)

    line = 'Station selection criteria for station selection:          ' // &
           '  not used'
    IF (opt%maxSta /= 0) THEN
      IF (opt%staStrat == 1) line(60:80) = '  max. obs'
      IF (opt%staStrat == 2) line(60:80) = 'best clock'
      IF (opt%staStrat == 3) line(60:80) = 'network geometry'
      IF (opt%staStrat == 4) line(60:80) = 'network density'
    ENDIF
    WRITE(lfnprt,'(1X,A,/)') TRIM(line)

    line = 'Minimum observations per satellite at each epoch:          ' // &
           '  not used'
    IF (opt%satObs > 0) WRITE(line(60:69),'(I10)') opt%satObs
    WRITE(lfnprt,'(1X,A)') TRIM(line)

    line = 'Number of clusters with minimum observations per satellite:' // &
           '  not used'
    IF (opt%satObs > 0) WRITE(line(60:69),'(I10)') opt%nSolve
    WRITE(lfnprt,'(1X,A)') TRIM(line)

    line = '  ... but nor for satellites with a low number of observ.: ' // &
           '  not used'
    IF (opt%satObs > 0) &
      WRITE(line(60:71),'(I10,A)') IDNINT(opt%obssat * 100d0),' %'
    WRITE(lfnprt,'(1X,A,/)') TRIM(line)

    line = 'Selected satellite system to be considered:                ' // &
           '       all'
    IF (opt%isasys == 1) line(60:80) = '       GPS'
    IF (opt%isasys == 2) line(60:80) = '   GLONASS'
    WRITE(lfnprt,'(1X,A)') TRIM(line)

    line = 'Sampling interval for the observations to be considered:   ' // &
           '  all epochs'
    IF (opt%nSampl > 0) WRITE(line(60:71),'(I10,A)') opt%nSampl,' s'
    WRITE(lfnprt,'(1X,A,//)') TRIM(line)

  ! Regional zero diff. clusters
  ELSE IF (opt%cluStrat == 2) THEN
    WRITE(lfnprt,'(2(1X,A,/))') &
          'REGIONAL CLUSTERS FROM ZERO DIFFERENCE FILES:', &
          '--------------------------------------------'

    IF (opt%maxSta > 0) THEN

      line = 'Maximal number of stations per cluster:'
      IF (opt%maxSta > 0) WRITE(line(60:69),'(I10)') opt%maxSta
      WRITE(lfnprt,'(1X,A,//)') TRIM(line)

    ELSE

      line = 'Number of clusters:'
      WRITE(line(60:69),'(I10)') opt%numClu
      WRITE(lfnprt,'(1X,A,/)') TRIM(line)

      line = 'Redundancy of the each observation:                        ' // &
             '  not used'
      IF (opt%staObs > 0) WRITE(line(60:69),'(I10)') opt%staObs
      WRITE(lfnprt,'(1X,A,/)') TRIM(line)

      line = 'Selected satellite system to be considered:                ' // &
             '       all'
      IF (opt%isasys == 1) line(60:80) = '       GPS'
      IF (opt%isasys == 2) line(60:80) = '   GLONASS'
      WRITE(lfnprt,'(1X,A)') TRIM(line)

      line = 'Sampling interval for the observations to be considered:   ' // &
             '  all epochs'
      IF (opt%nSampl > 0) WRITE(line(60:71),'(I10,A)') opt%nSampl,' s'
      WRITE(lfnprt,'(1X,A,//)') TRIM(line)

    ENDIF

  ! Regional baseline clusters
  ELSE IF (opt%cluStrat == 3) THEN
    WRITE(lfnprt,'(2(1X,A,/))') &
          'REGIONAL CLUSTERS FROM BASELINE FILES:', &
          '-------------------------------------'

    IF (opt%maxSta > 0) THEN

      line = 'Maximal number of baselines per cluster:'
      IF (opt%maxSta > 0) WRITE(line(60:69),'(I10)') opt%maxSta
      WRITE(lfnprt,'(1X,A,//)') TRIM(line)

    ELSE

      line = 'Number of clusters:'
      WRITE(line(60:69),'(I10)') opt%numClu
      WRITE(lfnprt,'(1X,A,//)') TRIM(line)

    ENDIF

  ENDIF

  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE mcinpt

END MODULE
