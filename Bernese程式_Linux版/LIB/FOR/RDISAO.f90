MODULE s_RDISAO
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdisao(maxsgr, maxoff, maxofr, globalWindow, dtSim,  &
                  nanoff, nsaoff, satoff, paroff, nrqoff, grpoff, &
                  sigoff, timoff, isgoff, gnroff, nallsat, allsatnum)

! -------------------------------------------------------------------------
! Purpose:    Reads the satellite antenna offset input options for GPSEST
!
! Author:     R. Dach
!
! Created:    20-Jun-2001
!
! Changes:    01-Feb-2002 RD: Correct SR name for error mgs
!             25-Mar-2003 RD: New parameter time window definition
!             01-Apr-2003 HU: Comment in DIMTST adapted
!             03-Jun-2003 HU: Use Interface to alcerr
!             10-Nov-2003 RS: Block-specific grouping of satellites, use
!                             d_satfil, use d_const, check manual input,
!                             add calls of gtsata and timst2, use m_maxdim,
!                             add gnroff
!             19-Jan-2003 SS/MM: Revision of GPSEST input panels
!             26-May-2005 RD: Use dimension MAXSGR for SATOFF and SATSPV
!             28-Jun-2005 MM: Unused variables removed
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             21-Sep-2005 HU: Block-specific setup according to SATELLIT.
!             09-Nov-2005 AG: SENNUM for GTSATA CALL ADDED
!             04-Oct-2006 AG: Satellite specific antenna PCO implemented
!             08-Nov-2006 AG: Deallocation of Saomore forbidden in case of SaoSel=3
!             09-Jul-2007 AG: Use GTSENSOR instead of GTSATA
!             21-May-2010 MF: Nullify saoMore%parTim
!             03-Nov-2011 RD: Redefine the open time-window
!             05-Mar-2012 RD: Use LISTI4 as module now
!             27-Apr-2012 RD: Nullify all pointers
!             20-Sep-2012 RD: Correctly deallocate arrays
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, &
                      fileNameLength, keyNameLength, keyValueLength
  USE m_time,   ONLY: t_timint
  USE m_maxdim, ONLY: maxsaa
  USE d_satfil, ONLY: t_satfil,init_satfil,typeMWTR
  USE s_ckoptr
  USE s_dimtst
  USE s_ckoptt
  USE s_alcerr
  USE s_ckoptu
  USE s_parint
  USE s_rdpwin
  USE s_timst2
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckopti
  USE s_gtflna
  USE s_gtsensor
  USE s_iordup
  USE s_rdsatfil
  USE f_listi4
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                   :: maxsgr       ! maximum number of satellites
                                                 ! per group
  INTEGER(i4b)                   :: maxoff       ! max. number of sat. antenna
                                                 ! groups
  INTEGER(i4b)                   :: maxofr       ! max. number of sat. ant.
                                                 ! requests
  INTEGER(i4b)                   :: nAllSat      ! number of all satellites
  INTEGER(i4b), DIMENSION(*)     :: allSatNum    ! satellite numbers
  TYPE(t_timint)                 :: globalWindow ! window to be processed
                                                 ! (from-to, MJD)
  REAL(r8b)                      :: dtsim        ! max. interval to identify
                                                 ! epoch (in days)

! output:
  INTEGER(i4b)                   :: nanoff       ! number of satellite antenna
                                                 ! offset groups to be estimated
  INTEGER(i4b), DIMENSION(*)     :: nsaoff       ! number of satellites
                                                 ! belonging to group i
  INTEGER(i4b),                  &
             DIMENSION(maxsgr,*) :: satoff       ! satoff(j,i),j=1,..,nsaoff(i),
                                                 ! i=1,..,nanoff
                                                 ! satellite numbers of each
                                                 ! sat. ant. offset group
  INTEGER(i4b), DIMENSION(3)     :: paroff       ! antenna offset components to
                                                 ! be estimated
                                                 ! (x,y,z in satellite reference
                                                 !  frame, 1: estimated)
  INTEGER(i4b)                   :: nrqoff       ! # of sat. ant. offset requests
  INTEGER(i4b), DIMENSION(*)     :: grpoff       ! antenna group for request
                                                 ! number i
                                                 ! 0: satellite specific
  REAL(r8b),    DIMENSION(3,*)   :: sigoff       ! a priori sigmas for ant.
                                                 ! requests
  REAL(r8b),    DIMENSION(2,*)   :: timoff       ! time intervals for antenna
                                                 ! requests
  INTEGER(i4b), DIMENSION(*)     :: isgoff       ! type of sigma
                                                 ! 0: absolute sigma
                                                 ! 1: sigma relative to the
                                                 !    previous parameter of the
                                                 !    same group
  INTEGER(i4b), DIMENSION(*)     :: gnroff       ! gnroff(i),i=1,..,nanoff
                                                 ! user-defined number of satellite
                                                 ! antenna offset group i

! Local Types
! -----------
  TYPE t_saoMore
    INTEGER(i4b)                    :: group
    REAL(r8b)                       :: parLen
    INTEGER(i4b)                    :: nPar
    TYPE(t_timint),                  &
               DIMENSION(:),POINTER :: parTim
    REAL(r8b), DIMENSION(6)         :: sigma
  END TYPE t_saoMore

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER                  :: srName = 'rdisao'

! Satellite antenna offset - keywords
  CHARACTER(LEN=keyNameLength), DIMENSION(3,2), PARAMETER :: sofKeyw = &
   reshape( source =                                                         &
   (/ 'SATANTX ','SATANTY ','SATANTZ ', 'SATANTSX','SATANTSY','SATANTSZ' /), &
      shape = (/ 3, 2/) )

! Dummy list
! ----------
  TYPE(t_satfil), SAVE  :: satfil

! Local Variables
! ---------------
  TYPE(t_saoMore),                &
      DIMENSION(:), ALLOCATABLE   :: saoMore

  CHARACTER(LEN=keyValueLength),  &
      DIMENSION(:)  , POINTER     :: keyValue
  CHARACTER(LEN=keyValueLength),  &
      DIMENSION(:,:), ALLOCATABLE :: hlpStr
  CHARACTER(LEN=fileNameLength)   :: filename

  INTEGER(i4b),                   &
      DIMENSION(:,:), ALLOCATABLE :: hlpInt
  INTEGER(i4b)                    :: iGrp
  INTEGER(i4b)                    :: irqOff
  INTEGER(i4b)                    :: ii, jj, kk
  INTEGER(i4b)                    :: irCode
  INTEGER(i4b)                    :: ircSav
  INTEGER(i4b)                    :: irc,iac
  INTEGER(i4b)                    :: saoSel
  INTEGER(i4b)                    :: isat
  INTEGER(i4b), DIMENSION(maxoff) :: ihelp,indx

  REAL(r8b), DIMENSION(6)         :: sigoffHlp
  REAL(r8b), DIMENSION(3)         :: refoff
  REAL(r8b), DIMENSION(3)         :: offset
  REAL(r8b)                       :: t_0,dt_0
  REAL(r8b)                       :: epo

  LOGICAL                         :: newGroup,newReq
  LOGICAL                         :: saoSpecial
  LOGICAL, SAVE                   :: first = .TRUE.

! Init variables
! --------------
  irCode = 0
  NULLIFY(keyValue)

  parOff(1:3)    = 0
  sigOffHlp(1:3) = 0D0

! Read which components are requested
! -----------------------------------
  DO ii=1,3
    CALL ckoptb(1,(/sofKeyw(ii,1)/),srName,                         &
                'Setup satellite antenna offsets',irCode,           &
                result1 = parOff(ii))

    ! Read also the sigmas
    IF (parOff(ii) == 1) THEN
      CALL readKeys(sofKeyw(ii,2), keyValue, irc)

      CALL ckoptr(1,sofKeyw(ii,2),keyValue,srName,                  &
                  'Sigma for satellite antenna offsets',irc,irCode, &
                  maxVal = 1, empty = 0d0, ge = 0d0, result1 = sigOffHlp(ii))
    ENDIF
  ENDDO

! Read the satellite groups
! -------------------------

! Which type of selection was set?
! --------------------------------
  CALL readKeys('SAOSEL', keyValue, irc)
  CALL ckoptc(1,'SAOSEL', keyValue, &
              (/'BLOCK/TYPE  ','MANUAL      ','SAT_SPECIFIC'/), &
              'rdisao', 'SAO setup', irc, irCode, &
              valList=(/1,2,3/), result1=saoSel)

  IF (irCode /= 0) CALL exitrc(2)

!! Do not use the default setup
! ----------------------------
  IF (saoSel /= 3) THEN
    CALL ckoptb(1,(/'SATANT0'/),srName,                            &
              'Modify the default setup',irCode,                   &
              resultL=saoSpecial)


    IF (saoSpecial) THEN

! Get the parameter time window definition
! ----------------------------------------
      CALL rdpwin('PAR_OFF',(/' ',' '/),t_0,dt_0)

! Read the special request uniline
! --------------------------------
      CALL readKeys('SATANTSTR', keyValue, irc)

    ! Allocate the buffer string
      ALLOCATE(hlpStr(8,SIZE(keyValue)),stat=iac)
      CALL alcerr(iac,'hlpStr',(/8,SIZE(keyValue)/),srName)
      hlpStr = ' '

      ALLOCATE(saoMore(SIZE(keyValue)),stat=iac)
      CALL alcerr(iac,'saoMore',(/SIZE(keyValue)/), srName)
      DO ii = 1,SIZE(keyValue)
        NULLIFY(saoMore(ii)%parTim)
      END DO

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

! Extract the length of parameter interval
      CALL ckoptt(1,'SATANTSTR', hlpStr(2,:),srName, &
           'Special setup for sat antenna offsets',ircSav,irCode,&
           gt=0d0,colTit='Parameter length',maxVal=SIZE(saoMore),&
           result2=saoMore(:)%parLen)

! Extract the sigmas
      DO jj = 1,3
        CALL ckoptr(1,'SATANTSTR', hlpStr(2+jj,:),srName, &
             'Special setup for sat antenna offsets',ircSav,irCode,&
             empty=sigOffHlp(jj),ge=0d0,colTit='Sigma',maxVal=SIZE(saoMore),&
             result2=saoMore(:)%sigma(jj))
      ENDDO

      DO jj = 4,6
        CALL ckoptr(1,'SATANTSTR', hlpStr(2+jj,:),srName, &
             'Special setup for sat antenna offsets',ircSav,irCode,&
             empty=0d0,ge=0d0,colTit='Sigma',maxVal=SIZE(saoMore),&
             result2=saoMore(:)%sigma(jj))
      ENDDO

      DEALLOCATE(hlpStr,stat=iac)

! Get the time windows for the parameters
      irqOff = nrqOff
      DO ii = 1,SIZE(saoMore)
        IF (saoMore(ii)%parLen == 0d0) CYCLE
        CALL parInt(globalWindow,dtSim,t_0,dt_0,saoMore(ii)%parLen, &
             'Satellite antenna offsets',saoMore(ii)%nPar,saoMore(ii)%parTim)
        irqOff = irqOff + saoMore(ii)%nPar-1
      ENDDO

    ENDIF
  ENDIF
! read the block-specific input
! -----------------------------

  IF (saoSel == 1) THEN
! Read satellite info file (SATELL)
! ---------------------------------
    IF (first) THEN
      first = .FALSE.
      CALL gtflna(1,'SATELL ',filename,IRC)
      CALL init_satfil(satfil)
      CALL rdsatfil(filename,satfil)
    ENDIF

! Compile groups
! --------------
    nanoff=0
    DO isat = 1,satfil%nsatellite
      IF (satfil%satellite(isat)%iblock > 0 .AND. &
          satfil%satellite(isat)%iblock < 300) THEN
        IF (satfil%satellite(isat)%timint%t(1) < globalWindow%t(1) .AND. &
            satfil%satellite(isat)%timint%t(2) > globalWindow%t(2)) THEN
          iGrp=listi4(1,maxoff,ihelp,satfil%satellite(isat)%iblock,nanoff)
        ENDIF
      ENDIF
    ENDDO
    CALL iordup(ihelp,nanoff,indx)
    DO iGrp=1,nanoff
      gnroff(iGrp)=ihelp(indx(iGrp))
    ENDDO
    nsaoff(1:nanoff)=0
    DO isat = 1,satfil%nsatellite
      IF (satfil%satellite(isat)%iblock > 0 .AND. &
          satfil%satellite(isat)%iblock < 300) THEN
        IF (satfil%satellite(isat)%timint%t(1) < globalWindow%t(1) .AND. &
            satfil%satellite(isat)%timint%t(2) > globalWindow%t(2)) THEN
          iGrp=listi4(0,maxoff,gnroff,satfil%satellite(isat)%iblock,nanoff)
          IF (iGrp > 0) THEN
            nsaoff(iGrp) = nsaoff(iGrp) + 1
            satoff(nsaoff(iGrp),iGrp) = satfil%satellite(isat)%svn
          ENDIF
        ENDIF
      ENDIF
    ENDDO

! Define the satellite antenna offset requests
! --------------------------------------------
    DO iGrp = 1,nanoff
      nrqoff = nrqoff + 1

      CALL dimtst(1,1,2,srName,'maxofr',                            &
                    'satellite antenna offset requests',              &
                    'Parameter is defined in module "P_GPSEST.f90".', &
                    nrqoff,maxofr,irc)

! Write a "normal request"
      grpoff(nrqoff)      = iGrp
      sigoff(1:3, nrqoff) = sigOffHlp(1:3)
      timoff( 1, nrqoff)  = 0d0
      timoff( 2, nrqoff)  = 1d20
      isgoff(nrqoff)      = 0

! special request definition
      IF (saoSpecial) THEN
        DO jj = 1,SIZE(saoMore)
          IF (gnroff(iGrp) /= saoMore(jj)%group) CYCLE

          CALL dimtst(1,1,2,srName,'maxofr',                 &
                        'satellite antenna offset requests',   &
                        'Include file "GPSEST.inc" is used.',  &
                        nrqoff+saoMore(jj)%nPar-1,maxofr,irc)

          DO kk = 1,saoMore(jj)%nPar

            IF (kk > 1) nrqoff = nrqoff + 1

            grpoff(nrqoff)      = iGrp
            timoff(1:2, nrqoff) = saoMore(jj)%parTim(kk)%t

            IF (kk > 1 .AND. saoMore(jj)%sigma(4) /= 0d0) THEN
              sigoff(1:3, nrqoff) = saoMore(jj)%sigma(4:6)
              isgoff(nrqoff)      = 1
            ELSE
              sigoff(1:3, nrqoff) = saoMore(jj)%sigma(1:3)
              isgoff(nrqoff)      = 0
            ENDIF

          ENDDO
        ENDDO
      ENDIF  ! saoSpecial
    ENDDO

! Check whether all satellites of one group have the same offset in the
! satellite information file
    epo = (globalWindow%t(1)+globalWindow%t(2))/2
    DO igrp = 1,nanoff
      IF (nsaoff(igrp) > 1) THEN
        refoff = 0d0
        DO isat = 1,nsaoff(igrp)
          CALL gtsensor(satoff(isat,igrp),epo,typeMWTR,antoff=offset)
          IF (isat == 1) refoff = offset
          DO ii=1,3
            IF (offset(ii) /= refoff(ii)) THEN
              WRITE(lfnerr,'(/,A,A,/,16X,A,3(/,16X,A,I5),/)')             &
               ' *** SR RDISAO: Satellite antenna offset in satellite ',  &
                        'antenna offset group differs from',              &
                        'the offset of the first satellite of the group', &
                        'Internal group number :',igrp,                   &
                        'External group number :',gnroff(igrp),           &
                        'Satellite number      :',satoff(isat,igrp)
              CALL exitrc(2)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
    ENDDO

! read the manual input
! ---------------------
  ELSE IF (saoSel == 2) THEN

    CALL readKeys('SATANTGRP', keyValue, irc)

    ! Allocate a buffer area
    ALLOCATE(hlpStr(16,SIZE(keyValue)),stat=iac)
    CALL alcerr(iac,'hlpStr',(/16,SIZE(keyValue)/),srName)
    hlpStr = ' '

    ALLOCATE(hlpInt(16,SIZE(keyValue)),stat=iac)
    CALL alcerr(iac,'hlpInt',(/16,SIZE(keyValue)/),srName)
    hlpInt = 0

    ! Fill uniline entries into the buffer
    ircSav = irCode
    CALL ckoptu(1,'SATANTGRP',keyValue,srName,             &
                'Satellit group definition',irc,irCode,16, &
                maxVal = SIZE(hlpStr,2),result2=hlpStr)

    ! Fill the string buffer into an integer buffer
    ircSav = irCode-ircSav
    DO ii = 1,16
      CALL ckopti(1,'SATANTGRP',hlpStr(ii,:),srName,       &
                  'Satellite group definition',ircSav,irCode, &
                  empty = 0,ge=0,maxVal = SIZE(hlpInt,2),     &
                  result2=hlpInt(ii,:))
    ENDDO

    DEALLOCATE(hlpStr,stat=irc)

! Get the user-defined group numbers and the number of groups
! -----------------------------------------------------------
    DO ii = 1, SIZE(hlpInt,2)

      IF (hlpInt(1,ii) == 0) CYCLE

      newGroup = .TRUE.
      IF (nanoff > 0) THEN
        DO jj = 1,nanoff
          IF (hlpInt(1,ii) == gnroff(jj)) THEN
            newGroup = .FALSE.
            EXIT
          ENDIF
        ENDDO
      ENDIF

      IF (newGroup) THEN
        nanoff = nanoff + 1
        gnroff(nanoff) = hlpInt(1,ii)
      ENDIF
    ENDDO

! Check maximum dimension
! -----------------------
    CALL dimtst(1,1,2,srName,'maxoff',                            &
                'satellite antenna offset',                       &
                'Parameter is defined in module "P_GPSEST.f90".', &
                nanoff,maxoff,irc)

    DO ii = 1, SIZE(hlpInt,2)

      IF (hlpInt(1,ii) == 0) CYCLE

      DO jj = 1,nanoff
        IF (hlpInt(1,ii) == gnroff(jj)) THEN
          iGrp = jj
          EXIT
        ENDIF
      ENDDO

! Get the list of satellites per group
! ------------------------------------
      DO jj = 2, 16

        IF (hlpInt(jj,ii) == 0) CYCLE

        nsaoff(iGrp) = nsaoff(iGrp) + 1

        IF (nsaoff(iGrp) > maxsgr) THEN
          WRITE(lfnerr,'(/,A,3(/,16X,A,I5),/,16X,A,/)')                  &
           ' *** SR RDISAO: Too many satellites per group selected',     &
                    'Satellite antenna offset group:      ',igrp,        &
                    'Number of satellites:                ',nsaoff(igrp),&
                    'Maximum number allowed:              ',maxsgr,      &
                    'Increase maxsat or reduce the number of satellites.'
          CALL exitrc(2)
        ENDIF

        satoff(nsaoff(iGrp), iGrp) = hlpInt(jj,ii)

      ENDDO

! Define the satellite antenna offset requests
! --------------------------------------------
      newReq = .TRUE.
      IF (nrqoff > 0) THEN
        DO jj = 1,nrqoff
          IF (grpoff(jj) == iGrp) THEN
            newReq = .FALSE.
            EXIT
          ENDIF
        ENDDO
      ENDIF

      IF (newReq) THEN
        nrqoff = nrqoff + 1

        CALL dimtst(1,1,2,srName,'maxofr',                            &
                    'satellite antenna offset requests',              &
                    'Parameter is defined in module "P_GPSEST.f90".', &
                    nrqoff,maxofr,irc)

        ! Write a "normal request"
        grpoff(nrqoff)      = iGrp
        sigoff(1:3, nrqoff) = sigOffHlp(1:3)
        timoff( 1, nrqoff)  = 0d0
        timoff( 2, nrqoff)  = 1d20
        isgoff(nrqoff)      = 0

        IF (saoSpecial) THEN
          DO jj = 1,SIZE(saoMore)
            IF (gnroff(iGrp) /= saoMore(jj)%group) CYCLE

            CALL dimtst(1,1,2,srName,'maxofr',                 &
                        'satellite antenna offset requests',   &
                        'Include file "GPSEST.inc" is used.',  &
                        nrqoff+saoMore(jj)%nPar-1,maxofr,irc)

            DO kk = 1,saoMore(jj)%nPar

              IF (kk > 1) nrqoff = nrqoff + 1

              grpoff(nrqoff)      = iGrp
              timoff(1:2, nrqoff) = saoMore(jj)%parTim(kk)%t

              IF (kk > 1 .AND. saoMore(jj)%sigma(4) /= 0d0) THEN
                sigoff(1:3, nrqoff) = saoMore(jj)%sigma(4:6)
                isgoff(nrqoff)      = 1
              ELSE
                sigoff(1:3, nrqoff) = saoMore(jj)%sigma(1:3)
                isgoff(nrqoff)      = 0
              ENDIF

            ENDDO
          ENDDO
        ENDIF  ! saoSpecial
      ENDIF  ! newReq
    ENDDO

! Check whether all satellites of one group have the same offset in the
! satellite information file
    epo = (globalWindow%t(1)+globalWindow%t(2))/2
    DO igrp = 1,nanoff
      IF (nsaoff(igrp) > 1) THEN
        refoff = 0d0
        DO isat = 1,nsaoff(igrp)
          CALL gtsensor(satoff(isat,igrp),epo,typeMWTR,antoff=offset)
          IF (isat == 1) refoff = offset
          DO ii=1,3
            IF (offset(ii) /= refoff(ii)) THEN
              WRITE(lfnerr,'(/,A,A,/,16X,A,3(/,16X,A,I5),/)')             &
               ' *** SR RDISAO: Satellite antenna offset in satellite ',  &
                        'antenna offset group differs from',              &
                        'the offset of the first satellite of the group', &
                        'Internal group number :',igrp,                   &
                        'External group number :',gnroff(igrp),           &
                        'Satellite number      :',satoff(isat,igrp)
              CALL exitrc(2)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
    ENDDO

! read the satellite-specific input
! -----------------------------------

  ELSEIF (saoSel == 3) THEN
! Compile groups
! --------------
    nanoff=nallsat
    nsaoff(1:nanoff)=1
    gnroff(1:nanoff)=allSatnum(1:nanoff)
    satoff(1,1:nanoff)=gnroff(1:nanoff)
!
! Define the satellite antenna offset requests
! --------------------------------------------
    DO iGrp = 1,nanoff

      nrqoff = nrqoff + 1

      CALL dimtst(1,1,2,srName,'maxofr',                            &
           'satellite antenna offset requests',              &
           'Parameter is defined in module "P_GPSEST.f90".', &
           nrqoff,maxofr,irc)

! Write a "normal request"
      grpoff(nrqoff)      = 0
      sigoff(1:3, nrqoff) = sigOffHlp(1:3)
      timoff( 1, nrqoff)  = 0d0
      timoff( 2, nrqoff)  = 1d20
      isgoff(nrqoff)      = 0
    END DO

  ENDIF  ! saoSel


  ! Deallocate the special request definition
  ! -----------------------------------------
  IF (saoSpecial .AND. SaoSel/=3) THEN

    DO ii=1,SIZE(saoMore)
      DEALLOCATE(saoMore(ii)%parTim,stat=irc)
    ENDDO
    DEALLOCATE(saoMore,stat=irc)
  ENDIF

  IF (saoSel == 2) DEALLOCATE(hlpInt,stat=irc)
  DEALLOCATE(keyValue,stat=irc)

! Exit if reading of the input options failed
! -------------------------------------------
  IF (irCode /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE rdisao

END MODULE
