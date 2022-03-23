MODULE s_AOPTANT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE aoptant(opt, parSao, parSap, parRao, parRap)

! -------------------------------------------------------------------------
! Purpose:    Reads input options for ADDNEQ2:
!             options and constraining for antenna parameters
!
! Author:     M. Meindl
!
! Created:    10-Jul-2001
!
! Changes:    02-Dec-2001 HU: Read keyword SETUP_GCC
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             23-Jan-2002 CU: Use ckopt subroutines
!             23-Jan-2002 CU: Use pricom: print center of mass input opt.
!             23-Jan-2002 CU: Use priorp: print sat.antenna offset input opt.
!             23-Jan-2002 CU: DEALLOCATE local variables
!             29-Jan-2002 CU: Add condition for writing sat.ant.offset title
!             30-Jan-2002 CU: Change condition for writing sat.ant.off.title
!             07-Feb-2002 CU: Change condition for writing sat.ant.off.title
!             08-Feb-2002 MM: Use t_sigma for local sigmas
!                             Enable relative sigmas
!             26-Feb-2002 CU: Handle case: no satellite antenna offset found
!             04-Jun-2002 CU: Correct format statement for priorp
!             05-Jun-2002 CU: Array constructor in call of priorp, interface
!             17-Oct-2002 CU: Correct index mistake for parSao
!             27-Oct-2002 HU: Prevent overflow for too many sat group files
!             10-Dec-2002 CU: Use new SR prisao and prisig instead of priorp
!                             for printing a priori information of SAO, GCC
!             19-Mar-2003 RD: Correct SIZE statement (because of IFC)
!             23-Apr-2003 CU: Nullify local pointers
!             19-Dec-2003 RS: Add satellite antenna patterns, add parSap
!             09-Apr-2004 HU: Read gcc sigmas if parameters set up
!             04-Oct-2006 AG: Implementations for satellite specific aantenna
!                             PCO/PCV
!             09-Oct-2006 AG: Initialisation of noffgrp and nspvgrp shifted
!             25-Jan-2008 RD: add RAO/RAP parameters
!             06-May-2009 RD: Use maxOff instead of maxSat
!             06-May-2009 RD: new options for satellite antennas
!             27-Nov-2009 RD: Antenna parameters extracted from AOPTOTR panel
!             22-Jul-2010 RD: Read launch date directly from sat-info file
!             25-Nov-2010 SS: Bugfixes concerning RAO/RAP
!             05-Mar-2012 RD: Use listi4 as module now
!             05-Mar-2012 RD: Remove unused variables
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnprt, keyValueLength, fileNameLength
  USE m_global, ONLY: g_syssvn
  USE d_const,  ONLY: C
  USE d_satfil, ONLY: t_satfil,init_satfil
  USE p_addneq, ONLY: t_parSao, t_opt, t_sigma, t_parSap, &
                      t_parRao, t_parRap
  USE s_alcerr
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckoptd
  USE s_ckopti
  USE s_ckoptr
  USE s_ckoptu
  USE s_exitrc
  USE d_neq,    ONLY: maxOff
  USE s_prisao
  USE s_prisap
  USE s_prisig
  USE s_readkeys
  USE s_svn2prn
  USE f_listi4
  USE s_gtflna
  USE s_rdsatfil
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  TYPE(t_parSao), DIMENSION(:), POINTER  :: parSao ! Sat.ant.offset inp. opt.
  TYPE(t_parSap), DIMENSION(:), POINTER  :: parSap ! Sat.ant.pattern inp. opt.
  TYPE(t_parRao)                         :: parRao ! Rec.ant.offset inp. opt.
  TYPE(t_parRap)                         :: parRap ! Rec.ant.pattern inp. opt.

! input/output:
  TYPE(t_opt)                            :: opt    ! Options for ADDNEQ2

! output:


! List of Functions
! -----------------

! Local Types
! -----------
  TYPE(t_sigma),DIMENSION(:),ALLOCATABLE  :: locSig
  TYPE t_saoMore
    INTEGER(i4b)                          :: group
    REAL(r8b), DIMENSION(3)               :: sigma
    INTEGER(i4b)                          :: satnum
  END TYPE t_saoMore
  TYPE t_sapMore
    INTEGER(i4b)                          :: group
    REAL(r8b), DIMENSION(1)               :: sigma
    INTEGER(i4b)                          :: satnum
  END TYPE t_sapMore

! Local Parameters
! ----------------
  CHARACTER(LEN=7),PARAMETER              :: srName = 'aoptant'
  CHARACTER(LEN=7),DIMENSION(3),PARAMETER :: saoKeyw = &
  (/'SATANTX','SATANTY','SATANTZ'/)
  CHARACTER(LEN=7),PARAMETER              :: sapKeyw = 'SATSPV'
   CHARACTER(LEN=7),DIMENSION(3),PARAMETER:: raoKeyw = &
  (/'RECANTN','RECANTE','RECANTU'/)
  CHARACTER(LEN=7),PARAMETER              :: rapKeyw = 'RECSPV'

  INTEGER(i4b),    PARAMETER              :: saoSigTyp = 3
  INTEGER(i4b),    PARAMETER              :: nSao      = 3
  INTEGER(i4b),    PARAMETER              :: nRao      = 3
  INTEGER(i4b),    PARAMETER              :: nRap      = 2
  INTEGER(i4b),    PARAMETER              :: sapSigTyp = 1

! Local variables
! ---------------
  TYPE(t_satfil)                                        :: satfil
  TYPE(t_saoMore), DIMENSION(:), ALLOCATABLE            :: saoMore
  TYPE(t_sapMore), DIMENSION(:), ALLOCATABLE            :: sapMore
  CHARACTER(LEN=keyValueLength),  &
                            DIMENSION(:,:), ALLOCATABLE :: hlpStr
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=fileNameLength)                         :: fileName
  CHARACTER(LEN=maxoff*5)                               :: groupline
  CHARACTER(LEN=4)                                      :: svnchr

  INTEGER(i4b)                                          :: maxSigTyp
  INTEGER(i4b)                                          :: oldSigTyp
  INTEGER(i4b)                                          :: nSig, iSig, eSig
  INTEGER(i4b)                                          :: iSao, ignr
  INTEGER(i4b)                                          :: irc, irCode, ii
  INTEGER(i4b)                                          :: ircSav
  INTEGER(i4b)                                          :: iac, jj
  INTEGER(i4b)                                          :: iFil
  INTEGER(i4b), DIMENSION(nSao)                         :: seqSao
  INTEGER(i4b), DIMENSION(nRao)                         :: seqRao
  INTEGER(i4b), DIMENSION(1)                            :: seqSap
  INTEGER(i4b), DIMENSION(nRap)                         :: seqRap
  INTEGER(i4b)                                          :: ispec
  INTEGER(i4b)                                          :: isapm, isaom
  INTEGER(i4b), DIMENSION(maxoff)                       :: special
  INTEGER(i4b), DIMENSION(maxoff)                       :: offgrp, spvgrp
  INTEGER(i4b)                                          :: noffgrp, nspvgrp
  INTEGER(i4b)                                          :: ipSap, ipSao
  INTEGER(i4b)                                          :: iFrq
  INTEGER(i4b)                                          :: iRao
  INTEGER(i4b)                                          :: iRap
  INTEGER(i4b)                                          :: selSao
  INTEGER(i4b)                                          :: selSap


  REAL(r8b),    DIMENSION(nSao)                         :: sigSao
  REAL(r8b),    DIMENSION(1)                            :: sigSap
  REAL(r8b),    DIMENSION(nRao)                         :: sigRao
  REAL(r8b),    DIMENSION(nRap)                         :: sigRap
  REAL(r8b)                                             :: datSao
  REAL(r8b)                                             :: datSap

  LOGICAL                                               :: reqSao
  LOGICAL                                               :: reqSap
  LOGICAL                                               :: saoSpecial
  LOGICAL                                               :: sapSpecial
  LOGICAL                                               :: doIt

  NULLIFY(keyValue)

! Get number of satellite antenna groups from neqs
! ------------------------------------------------
  reqSao = .FALSE.
  reqSap = .FALSE.
! (if satellite antenna offsets available from neq)
  DO iFil = 1, SIZE(parSao)
    IF (parSao(ifil)%nanoff > 0) reqSao = .TRUE.
  ENDDO
  noffgrp=0
  nspvgrp=0
  IF (reqSao) THEN
    DO ipSao=1,SIZE(parSao)
      DO ignr = 1, parSao(ipSao)%nanoff
        isaom=listi4(1,maxoff,offgrp(:),parSao(ipSao)%gnroff(ignr),noffgrp)
      ENDDO
    ENDDO
  ENDIF
! (if satellite antenna patterns available from neq)
  DO iFil = 1, SIZE(parSap)
    IF (parSap(ifil)%nanspv > 0) reqSap = .TRUE.
  ENDDO
  IF (reqSap) THEN
    DO ipSap=1,SIZE(parSap)
      DO ignr = 1, parSap(ipSap)%nanspv
        isapm=listi4(1,maxoff,spvgrp(:),parSap(ipSap)%gnrspv(ignr),nspvgrp)
      ENDDO
    ENDDO
  ENDIF

! Get other sigmas from opt%sigma
! -------------------------------
  oldSigTyp = SIZE(opt%sigma)
  maxSigTyp = oldSigTyp + saoSigTyp * noffgrp + sapSigTyp * nspvgrp + 3 + nRao * 5 + nRap

  ALLOCATE(locSig(maxSigTyp),stat=iac)
  CALL alcerr(iac,'locSig',(/maxSigTyp/),'aoptotr')

! Initialize local sigma
  DO iSig = 1,maxSigTyp
    locSig(iSig)%locq(:)   = 0
  ENDDO
  locSig(:)%value     = 0.D0
  locSig(:)%name      = ''
  locSig(:)%typFlg    = 'A'
  locSig(1:oldSigTyp) = opt%sigma(:)

! Constraining of other parameters
! --------------------------------
  nSig   = 0
  irCode = 0

! Satellite antenna offsets
! -------------------------
! get a priori sigmas of satellite antenna offsets

  IF (reqSao) THEN

    CALL readKeys('SATANTGPS', keyValue, irc)
    CALL ckoptc(1,'SATANTGPS', keyValue,                               &
                (/'DELETE       ', 'NONE         ',                    &
                  'TO_BLOCK/TYPE'/), srName,                           &
                  'Modify satellite antenna offset param.',            &
                  irc, irCode, maxVal = 1, valList = (/-1,0,2/),       &
                  result1=opt%saoOpt(1))

    CALL readKeys('SATANTGLO', keyValue, irc)
    CALL ckoptc(1,'SATANTGLO', keyValue,                               &
                (/'DELETE       ', 'NONE         ',                    &
                  'TO_FREQUENCY ', 'TO_BLOCK/TYPE'/), srName,          &
                  'Modify satellite antenna offset param.',            &
                  irc, irCode, maxVal = 1, valList = (/-1,0,1,2/),     &
                  result1=opt%saoOpt(2))

    CALL readKeys('SATANTS', keyValue, irc)
    CALL ckoptc(1,'SATANTS', keyValue,                                 &
                (/'ALL            ', 'GPS            ',                &
                  'GLONASS        ', 'LAUNCHED_BEFORE'/), srName,      &
                  'Apply satellite antenna offset sigmas',             &
                  irc, irCode, maxVal = 1, valList = (/0,1,2,3/),      &
                  result1=selSao)

    datSao = 0d0
    IF (selSao == 3) THEN
      CALL readKeys('SATANTD', keyValue, irc)
      CALL ckoptd(1,'SATANTD', keyValue, srName,                         &
                    'Date satellite antenna offset sigmas', irc, irCode, &
                    empty = 0d0, maxVal = 1, result1=datSao)
    ENDIF

    sigSao(:) = 0d0
    DO iSao = 1, nSao
      CALL readKeys(saoKeyw(iSao), keyValue, irc)
      CALL ckoptr(1,saoKeyw(iSao), keyValue, srName,      &
                  'Satellite antenna offset constraints', &
                  irc, irCode, empty=0d0,ge=0d0,maxval=1, &
                  error=99.9999d0, result1=sigSao(iSao))

      seqSao(iSao) = iSao
      IF (sigSao(iSao) /= 0d0) THEN
        DO ignr=1,noffgrp
          WRITE(svnchr,'(A1,I3.3)')g_syssvn(INT(offgrp(ignr)/100)),offgrp(ignr)
          doit = (selSao == 0)
          doIt = doIt .OR. ( selSao == 1 .AND. svnchr(1:1) == 'G')
          doIt = doIt .OR. ( selSao == 2 .AND. svnchr(1:1) == 'R')
          doIt = doIt .OR. ( selSao == 3 .AND. datSao == 0d0 )
          IF ( selSao == 3 .AND. datSao /= 0d0 ) THEN

            ! Read satellite info file (SATELL)
            CALL gtflna(1,'SATELL ',filename,IRC)
            CALL init_satfil(satfil)
            CALL rdsatfil(filename,satfil)

            DO ii = 1,satfil%nsatellite
              doit = doit .OR. &
                   ( satfil%satellite(ii)%svnnr == svnchr .AND. &
                     satfil%satellite(ii)%timint%t(1) < datSao )
            ENDDO

            DEALLOCATE(satfil%satellite,stat=iac)
            DEALLOCATE(satfil%sensor,stat=iac)
            DEALLOCATE(satfil%rprmod,stat=iac)

          ENDIF
          IF ( doit ) THEN
            nSig                 = nSig + 1
            iSig                 = nSig + oldSigTyp
            locSig(iSig)%locq(1) = 12
            locSig(iSig)%locq(3) = iSao
            locSig(iSig)%locq(5) = offgrp(ignr)
            locSig(iSig)%value   = sigSao(iSao)
          ENDIF
        ENDDO
      ENDIF
    ENDDO

! Do not use the default setup
! ----------------------------
    CALL ckoptb(1,(/'SATANT0'/),srName,                            &
              'Modify the default setup',irCode,                   &
              resultL=saoSpecial)

    IF (saoSpecial) THEN

! Read the special request uniline
! --------------------------------
      CALL readKeys('SATANTSTR', keyValue, irc)

      ! Allocate the buffer string
      ALLOCATE(hlpStr(13,SIZE(keyValue)),stat=iac)
      CALL alcerr(iac,'hlpStr',(/13,SIZE(keyValue)/),srName)
      hlpStr = ' '

      ALLOCATE(saoMore(SIZE(keyValue)),stat=iac)
      CALL alcerr(iac,'saoMore',(/SIZE(keyValue)/), srName)

      ! Extract the special setup part
      ircSav = irCode
      CALL ckoptu(1,'SATANTSTR', keyValue,srName, &
                'Special setup for sat, antenna offsets',irc,irCode,8,&
                maxVal=SIZE(hlpStr,2),result2=hlpStr)

      ircSav = irCode-ircSav

      ! Extract the groups
      CALL ckopti(1,'SATANTSTR', hlpStr(1,:),srName, &
                'Special setup for sat antenna offsets',ircSav,irCode,&
                ge=0,colTit='Group',maxVal=SIZE(saoMore),&
                result2=saoMore(:)%group)

      ! Extract the sigmas
      DO jj = 1,3
        CALL ckoptr(1,'SATANTSTR', hlpStr(1+jj,:),srName, &
                  'Special setup for sat antenna offsets',ircSav,irCode,&
                  empty=sigSao(jj),ge=0d0,colTit='Sigma',maxVal=SIZE(saoMore),&
                  result2=saoMore(:)%sigma(jj))
      ENDDO

      ! Extract the satellite numbers
      DO ii = 1,9
        CALL ckopti(1,'SATANTSTR',hlpStr(4+ii,:),srName,       &
                   'Satellite group definition',ircSav,irCode, &
                    empty = 0,ge=0,maxVal = SIZE(saoMore),     &
                    result2=saoMore(:)%satnum)
      ENDDO

      DEALLOCATE(hlpStr,stat=iac)
      eSig=iSig
      DO ispec=1,SIZE(saoMore)
        DO iSao = 1, nSao
          DO iSig=1,eSig
            IF (locSig(iSig)%locq(1) == 12 .AND.                      &
              locSig(iSig)%locq(5) == saoMore(ispec)%group .AND.   &
              locSig(iSig)%locq(3) == iSao) THEN
              locSig(iSig)%value   = saoMore(ispec)%sigma(iSao)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
    ENDIF

! print a priori information of satellite antenna offsets
    WRITE(lfnprt,'(A,/,A)')          &
      ' Satellite antenna offsets:',  &
      ' -------------------------'
    CALL prisao(parSao)
! print a priori sigmas of satellite antenna offsets
! For special
    special(:)=0
    IF (saoSpecial) THEN
      DO isaom = 1, SIZE(saoMore)
        ignr=listi4(0,noffgrp,offgrp,saoMore(isaom)%group,noffgrp)
        IF (ignr /= 0) THEN
          special(ignr) = ignr
          WRITE(lfnprt,"(' Group: ',150I5)")offgrp(ignr)
          CALL prisig(12, saoMore(isaom)%sigma, nSao, (/1,2,3/))
        ENDIF
      ENDDO
    ENDIF
! For all (other) groups
    ii=0
    groupline = ''
    DO ignr = 1, noffgrp
      IF (special(ignr) == 0) THEN
        ii = ii + 1
        WRITE(groupline(ii*5-4:ii*5),'(I5)')offgrp(ignr)
      ENDIF
    ENDDO
    IF (ii /= 0) THEN
      WRITE(lfnprt,"(' Groups: ',A)")groupline
      CALL prisig(12, sigSao, nSao, seqSao)
    ENDIF
  ENDIF

! Satellite antenna patterns
! --------------------------
! get a priori sigmas of satellite antenna patterns

  IF (reqSap) THEN

    CALL readKeys('SATSPVGPS', keyValue, irc)
    CALL ckoptc(1,'SATSPVGPS', keyValue,                               &
                (/'DELETE       ', 'NONE         ',                    &
                  'TO_BLOCK/TYPE'/), srName,                           &
                  'Modify satellite antenna pattern param.',           &
                  irc, irCode, maxVal = 1, valList = (/-1,0,2/),       &
                  result1=opt%sapOpt(1))

    CALL readKeys('SATSPVGLO', keyValue, irc)
    CALL ckoptc(1,'SATSPVGLO', keyValue,                               &
                (/'DELETE       ', 'NONE         ',                    &
                  'TO_FREQUENCY ', 'TO_BLOCK/TYPE'/), srName,          &
                  'Modify satellite antenna pattern param.',           &
                  irc, irCode, maxVal = 1, valList = (/-1,0,1,2/),     &
                  result1=opt%sapOpt(2))

    CALL readKeys('SATSPVS', keyValue, irc)
    CALL ckoptc(1,'SATSPVS', keyValue,                                 &
                (/'ALL            ', 'GPS            ',                &
                  'GLONASS        ', 'LAUNCHED_BEFORE'/), srName,      &
                  'Apply satellite antenna pattern sigmas',            &
                  irc, irCode, maxVal = 1, valList = (/0,1,2,3/),      &
                  result1=selSap)

    datSap = 0d0
    IF (selSap == 3) THEN
      CALL readKeys('SATSPVD', keyValue, irc)
      CALL ckoptd(1,'SATSPVD', keyValue, srName,                          &
                    'Date satellite antenna pattern sigmas', irc, irCode, &
                    empty = 0d0, maxVal = 1, result1=datSap)
    ENDIF

    sigSap(1) = 0d0
    seqSap(1) = 1
    CALL readKeys(sapKeyw, keyValue, irc)
    CALL ckoptr(1, sapKeyw, keyValue, srname,             &
                'Satellite antenna pattern constraints',  &
                irc, irCode, empty=0d0, ge=0d0, maxval=1, &
                error=99.9999d0, result1=sigSap(1))
    IF (sigSap(1) /= 0d0) THEN
      DO ignr=1,nspvgrp
        WRITE(svnchr,'(A1,I3)')g_syssvn(INT(spvgrp(ignr)/100)),spvgrp(ignr)
        doit = (selSap == 0)
        doIt = doIt .OR. ( selSap == 1 .AND. svnchr(1:1) == 'G')
        doIt = doIt .OR. ( selSap == 2 .AND. svnchr(1:1) == 'R')
        doIt = doIt .OR. ( selSap == 3 .AND. datSap == 0d0 )
        IF ( selSap == 3 .AND. datSap /= 0d0 ) THEN

            ! Read satellite info file (SATELL)
            CALL gtflna(1,'SATELL ',filename,IRC)
            CALL init_satfil(satfil)
            CALL rdsatfil(filename,satfil)

            DO ii = 1,satfil%nsatellite
              doit = doit .OR. &
                   ( satfil%satellite(ii)%svnnr == svnchr .AND. &
                     satfil%satellite(ii)%timint%t(1) < datSap )
            ENDDO

            DEALLOCATE(satfil%satellite,stat=iac)
            DEALLOCATE(satfil%sensor,stat=iac)
            DEALLOCATE(satfil%rprmod,stat=iac)

        ENDIF
        IF ( doit ) THEN
          nSig                 = nSig + 1
          iSig                 = nSig + oldSigTyp
          locSig(iSig)%locq(1) = 25
          locSig(iSig)%locq(3) = spvgrp(ignr)
          locSig(iSig)%value   = sigSap(1)
        ENDIF
      ENDDO
    ENDIF

! Do not use the default setup
! ----------------------------
    CALL ckoptb(1,(/'SAPANT0'/),srName,                       &
         'Modify the default setup',irCode,                   &
         resultL=sapSpecial)

    IF (sapSpecial) THEN

! Read the special request uniline
! --------------------------------
      CALL readKeys('SAPGRP', keyValue, irc)

    ! Allocate the buffer string
      ALLOCATE(hlpStr(13,SIZE(keyValue)),stat=iac)
      CALL alcerr(iac,'hlpStr',(/13,SIZE(keyValue)/),srName)
      hlpStr = ' '
!      sapMore(:)%sigma(1)=0D0

      ALLOCATE(sapMore(SIZE(keyValue)),stat=iac)
      CALL alcerr(iac,'sapMore',(/SIZE(keyValue)/), srName)

    ! Extract the special setup part
      ircSav = irCode
      CALL ckoptu(1,'SAPGRP', keyValue,srName, &
           'Special setup for sat, antenna patterns',irc,irCode,8,&
           maxVal=SIZE(hlpStr,2),result2=hlpStr)

      ircSav = irCode-ircSav

    ! Extract the groups
      CALL ckopti(1,'SAPGRP', hlpStr(1,:),srName, &
           'Special setup for sat antenna patterns',ircSav,irCode,&
           ge=0,colTit='Group',maxVal=SIZE(sapMore),&
           result2=sapMore(:)%group)

    ! Extract the sigmas
      CALL ckoptr(1,'SAPGRP', hlpStr(2,:),srName, &
           'Special setup for sat antenna patterns',ircSav,irCode,&
           empty=sigSap(1),ge=0d0,colTit='Sigma',maxVal=SIZE(sapMore),&
           result2=sapMore(:)%sigma(1))

    ! Extract the satellite numbers
      DO ii = 1,9
        CALL ckopti(1,'SAPGRP',hlpStr(2+ii,:),srName,       &
             'Satellite group definition',ircSav,irCode, &
             empty = 0,ge=0,maxVal = SIZE(sapMore),     &
             result2=sapMore(:)%satnum)
      ENDDO

      DEALLOCATE(hlpStr,stat=iac)
      eSig=iSig
      DO ispec=1,SIZE(sapMore)
        DO iSig=1,eSig
          IF (locSig(iSig)%locq(1) == 25 .AND.                      &
               locSig(iSig)%locq(3) == sapMore(ispec)%group) THEN
              locSig(iSig)%value   = sapMore(ispec)%sigma(1)
          ENDIF
        ENDDO
      ENDDO
    ENDIF

! print a priori information of satellite antenna patterns
    WRITE(lfnprt,'(A,/,A)')          &
      ' Satellite antenna patterns:', &
      ' --------------------------'
    CALL prisap(parSap)
! print a priori sigmas of satellite antenna patterns
! For special
    special(:)=0
    IF (sapSpecial) THEN
      DO isapm = 1, SIZE(sapMore)
        ignr=listi4(0,nspvgrp,spvgrp,sapMore(isapm)%group,nspvgrp)
        IF (ignr /= 0) THEN
          special(ignr) = ignr
          WRITE(lfnprt,"(' Group: ',150I5)")spvgrp(ignr)
          CALL prisig(25, sapMore(isapm)%sigma, sapSigTyp, seqSap)
        ENDIF
      ENDDO
    ENDIF
! For all (other) groups
    ii=0
    groupline = ''
    DO ignr = 1, nspvgrp
      IF (special(ignr) == 0) THEN
        ii = ii + 1
        WRITE(groupline(ii*5-4:ii*5),'(I5)')spvgrp(ignr)
      ENDIF
    ENDDO
    IF (ii /= 0) THEN
      WRITE(lfnprt,"(' Groups: ',A)")groupline
      CALL prisig(25, sigSap, sapSigTyp, seqSap)
    ENDIF
  ENDIF

! Receiver antenna offsets
! ------------------------
  IF (parRao%nRao > 0) THEN
    sigRao(:) = 0d0
    DO iRao = 1, nRao
      IF (parRao%rao(iRao) == 0) CYCLE
      CALL readKeys(raoKeyw(iRao), keyValue, irc)
      CALL ckoptr(1,raoKeyw(iRao), keyValue, srName,      &
                  'Receiver antenna offset constraints', &
                  irc, irCode, empty=0d0,ge=0d0,maxval=1, &
                  error=99.9999d0, result1=sigRao(iRao))
      seqRao(iRao) = iRao
      IF (sigRao(iRao) /= 0d0) THEN
        DO iFrq = 1,5
          nSig                 = nSig + 1
          iSig                 = nSig + oldSigTyp
          locSig(iSig)%locq(1) =  5
          locSig(iSig)%locq(4) = iRao*100 + iFrq
          locSig(iSig)%value   = sigRao(iRao)
        ENDDO
      ENDIF
    ENDDO

! print a priori information of receiver antenna offsets
    WRITE(lfnprt,'(A,/,A)')          &
      ' Receiver antenna offsets:', &
      ' ------------------------'
    CALL prisig( 5, sigRao, nRao, seqRao)
  ENDIF

! Receiver antenna pattern
! ------------------------
  IF (parRap%nRapGrd + parRap%nRapShm > 0) THEN
    sigRap(:) = 0d0
    DO iRap = 1, nRap
      seqRap(iRap) = iRap
      IF ( iRap == 1 .AND. parRap%nRapGrd == 0 ) CYCLE
      IF ( iRap == 2 .AND. parRap%nRapShm == 0 ) CYCLE
      CALL readKeys(rapKeyw, keyValue, irc)
      CALL ckoptr(1,rapKeyw, keyValue, srName,            &
                  'Receiver antenna pattern constraints', &
                  irc, irCode, empty=0d0,ge=0d0,maxval=1, &
                  error=99.9999d0, result1=sigRap(iRap))
      IF (sigRap(iRap) /= 0d0) THEN
        nSig                 = nSig + 1
        iSig                 = nSig + oldSigTyp
        locSig(iSig)%locq(1) = 18
        locSig(iSig)%value   = sigRap(iRap)
      ENDIF
    ENDDO

! print a priori information of receiver antenna pattern
    WRITE(lfnprt,'(A,/,A)')          &
      ' Receiver antenna pattern:', &
      ' ------------------------'
    CALL prisig(18, sigRap, nRap, seqRap)
  ENDIF

! Stop the program if an input error found
! ----------------------------------------
  IF (irCode /= 0) CALL exitrc(2)

! Store sigmas in opt%sigma
! -------------------------
  DEALLOCATE(opt%sigma,stat=iac)
  ALLOCATE(opt%sigma(nSig+oldSigTyp),stat=iac)
  CALL alcerr(iac,'opt%sigma',(/nSig+oldSigTyp/),'aoptotr')

  nSig = 0
  DO iSig = 1,maxSigTyp
    IF (locSig(iSig)%value /= 0d0) THEN
      nSig            = nSig+1
      opt%sigma(nSig) = locSig(iSig)
    ENDIF
  ENDDO

  DO iFil = 1, SIZE(parSao)
    DEALLOCATE(parSao(iFil)%nsaoff,parSao(iFil)%satoff,stat=iac)
  ENDDO
  DEALLOCATE(parSao,stat=iac)
  DO iFil = 1, SIZE(parSap)
    DEALLOCATE(parSap(iFil)%nsaspv,parSap(iFil)%satspv,stat=iac)
  ENDDO
  DEALLOCATE(parSap,stat=iac)

! Deallocate the special request definition
! -----------------------------------------
  IF (saoSpecial) THEN
    DEALLOCATE(saoMore,stat=iac)
  ENDIF
  IF (sapSpecial) THEN
    DEALLOCATE(sapMore,stat=iac)
  ENDIF
  DEALLOCATE(locSig,stat=iac)
  DEALLOCATE(keyValue,stat=iac)

  RETURN
END SUBROUTINE aoptant

END MODULE
