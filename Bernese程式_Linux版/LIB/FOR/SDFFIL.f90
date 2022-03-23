MODULE s_SDFFIL
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE sdffil(nSngFil, filInp, filOut, iCombi, nPreDef, &
                 crtsat, nSatFl, satfil)

! -------------------------------------------------------------------------
!
! Purpose:    This is a new version of the old subroutine SDFFIL.f that
!             reads the names of input and output files of program SNGDIF
!
! Author:     L. Mervart
!
! Created:    02-Jun-2000
!
! Changes:    26-Jun-2001 RD: Use alcerr for allocation
!             22-Oct-2001 MM: Several changes due to new menu (eg. ckoptx)
!             16-Nov-2001 MM: Several changes due to new strategies
!             22-Jul-2002 HB: Use modified t_obsHead
!             23-Apr-2003 RD: Nullify local pointers
!             17-May-2003 HU: Initialize structure
!             30-Oct-2003 RD: Deallocate coordinate arrays
!                             Menu panel update
!             22-Dec-2010 RD/MF: System selection
!             14-Feb-2011 RD: Remove MAXSTA-COMMON (not needed)
!             15-Feb-2011 RD: GETSTA is used as a module now
!             25-Feb-2011 RD: Handle marginally observ. satellites
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnprt, lfnerr, &
                      keyNameLength, keyValueLength, &
                      fileNameLength, fileExtLength, filePathLength, &
                      shortLineLength
  USE m_global, ONLY: maxsys,g_svnsys
  USE d_gpsobs, ONLY: t_obshead, init_obshead

  USE s_ckoptr
  USE s_alcerr
  USE s_gtfile
  USE s_rdhead2
  USE s_sddefb
  USE s_readkeys
  USE s_getsta
  USE s_exitrc
  USE s_sdastr
  USE s_dfsngnam
  USE s_ckoptb
  USE s_ckoptc
  USE s_sdshrt
  USE s_gtflna
  USE s_ckoptl
  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b)                     :: nSngFil  ! number of output files
  CHARACTER(LEN=fileNameLength),                                        &
            DIMENSION(2,*)         :: filinp   ! zero dif. (header and obs.)
  CHARACTER(LEN=fileNAmeLength),                                        &
            DIMENSION(2,*)         :: filout   ! single dif. (header and obs.)
  INTEGER(i4b), DIMENSION(2,*)     :: icombi   ! file numbers to be combined
                                               ! to form a single-diff. file
  INTEGER(i4b)                     :: nPreDef  ! number of predefined BLs
                                               ! to use for OBSMAX (the first
                                               ! nPreDef lines in iCombi are
                                               ! used)
  REAL(r8b)                        :: crtsat   ! Percentage of observations to
                                               ! identify a marginaly obs. sat.
  INTEGER(i4b)                     :: nsatfl   ! Number of elements in satfil
  INTEGER(i4b), DIMENSION(2,*)     :: satfil   ! List of sat margninally observ.
                                               ! 1: satellite number
                                               ! 2: number of stations observed
                                               !    this satellite

! Variables used for rdhead subroutine
! ------------------------------------
  TYPE(t_obshead) :: zeroHead

! Variables used for getsta subroutine
! ------------------------------------
  CHARACTER(LEN=16), DIMENSION(:)  , ALLOCATABLE   :: staZero
  INTEGER(i4b),      DIMENSION(:)  , ALLOCATABLE   :: stanum
  INTEGER(i4b)                                     :: ncentr
  INTEGER(i4b),      DIMENSION(:)  , ALLOCATABLE   :: icentr
  REAL(r8b),         DIMENSION(:,:), ALLOCATABLE   :: xstat
  REAL(r8b),         DIMENSION(:,:), ALLOCATABLE   :: xstell
  REAL(r8b),         DIMENSION(:,:), ALLOCATABLE   :: xstecc
  CHARACTER(LEN=16)                                :: datum
  REAL(r8b)                                        :: aell
  REAL(r8b)                                        :: bell
  REAL(r8b), DIMENSION(3)                          :: dxell
  REAL(r8b), DIMENSION(3)                          :: drell
  REAL(r8b)                                        :: scell

! Other Local Variables
! ---------------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=fileNameLength), DIMENSION(2)          :: refInp
  CHARACTER(LEN=fileNameLength)                        :: dummy
  CHARACTER(LEN=keyNameLength)                         :: fileKey, fileKey2
  CHARACTER(LEN=keyNameLength)                         :: outFileKey
  CHARACTER(LEN=keyNameLength)                         :: refFileKey
  CHARACTER(LEN=fileExtLength)                         :: ext_sh
  CHARACTER(LEN=fileExtLength)                         :: ext_so
  CHARACTER(LEN=filePathLength)                        :: dir_sh
  CHARACTER(LEN=filePathLength)                        :: dir_so
  CHARACTER(LEN=5)                                     :: sesName
  CHARACTER(LEN=4), DIMENSION(:), ALLOCATABLE          :: name4
  CHARACTER(LEN=FileNameLength)                        :: srName
  CHARACTER(LEN=shortLineLength)                       :: line
  INTEGER(i4b)                                         :: strategy
  REAL(r8b)                                            :: basLen, optMax
  INTEGER(i4b)                                         :: iSat,jSat, kSat
  INTEGER(i4b)                                         :: irc, iac, ircSum
  INTEGER(i4b)                                         :: ii, jj, kk, ilen
  INTEGER(i4b)                                         :: lenDot, good
  INTEGER(i4b)                                         :: nZeroFil, nRefFil
  INTEGER(i4b)                                         :: meaTyp
  INTEGER(i4b)                                         :: nrRef
  INTEGER(i4b)                                         :: isys
  INTEGER(i4b), DIMENSION(maxsys)                      :: iSysFl, nSysFl
  INTEGER(i4b), DIMENSION(0:maxsys-1)                  :: isSys
  INTEGER(i4b)                                         :: iFil,jFil
  INTEGER(i4b)                                         :: isasys
  LOGICAL                                              :: replace

! Commons for old subroutines
! ---------------------------
  CHARACTER*6  mxnfil, mxnsat, mxnamb
  INTEGER(i4b) mxcfil, mxcsat, mxcamb

  COMMON/mcmfil/mxcfil,mxnfil
  COMMON/mcmsat/mxcsat,mxnsat
  COMMON/mcmamb/mxcamb,mxnamb

! Initialize some variables
! -------------------------
  srName  = 'sdffil'
  nRefFil = 0
  ircSum  = 0
  nPreDef = 0
  nSngFil = 0

  NULLIFY(keyValue)
  CALL init_obshead(zerohead)

! Measurement type (code or phase) and strategy
! ---------------------------------------------
  CALL readkeys('MEATYP',keyValue,irc)
  CALL ckoptc(1,'MEATYP',keyValue,(/'PHASE','CODE '/),srName,             &
              'Measurement type',irc,ircSum,maxVal=1,result1=meaTyp)

  CALL readkeys('STRATEGY',keyValue,irc)
  CALL ckoptc(1,'STRATEGY',keyValue,                                      &
              (/'MANUAL  ','OBS-MAX ','SHORTEST',                         &
                'STAR    ','DEFINED '/),                                  &
              srName,'Processing strategy',irc,ircSum,                    &
              maxVal=1,result1=strategy)

! replace stations (former strategy: PLUS)?
  IF (strategy /= 1 .AND. strategy/=4) THEN
    CALL ckoptb(1,(/'REPLACE'/),srName,                                    &
                'Replace missing stations',ircSum,                         &
                resultL=replace)
    IF (replace .AND. strategy==5) strategy = 7
  ENDIF


! Max. baseline length for OBS-MAX
! --------------------------------
  IF (strategy == 2) THEN
    CALL readkeys('OPTMAX',keyValue,irc)
    CALL ckoptr(1,'OPTMAX',keyValue,srName,                              &
                'Maximal beseline length for OBS-MAX',                   &
                irc,ircSum,maxVal=1,gt=0d0,result1=optMax)
  ENDIF

! Set path and extension
! ----------------------
  IF (MEATYP == 1) THEN
    IF (strategy == 1) THEN
      fileKey  = 'PHASEZERO1'
      fileKey2 = 'PHASEZERO2'
    ELSE
      fileKey  = 'PHASEZERO'
    ENDIF
    outFileKey = 'PHASESNG'
    refFileKey = 'PHASEREF'
    CALL readKeys('EXT_PSH',keyValue,irc)
    CALL ckoptl(1,'EXT_PSH',keyValue,srName,'Extension EXT_PSH',         &
                irc,ircSum,maxVal=1,result1=ext_sh)
    CALL readKeys('DIR_PSH',keyValue,irc)
    CALL ckoptl(1,'DIR_PSH',keyValue,srName,'Path DIR_PSH',              &
                irc,ircSum,maxVal=1,result1=dir_sh)
    CALL readKeys('EXT_PSO',keyValue,irc)
    CALL ckoptl(1,'EXT_PSO',keyValue,srName,'Extension EXT_PSO',         &
                irc,ircSum,maxVal=1,result1=ext_so)
    CALL readKeys('DIR_PSO',keyValue,irc)
    CALL ckoptl(1,'DIR_PSO',keyValue,srName,'Path DIR_PSO',              &
                irc,ircSum,maxVal=1,result1=dir_so)
  ELSE
    IF (strategy == 1) THEN
      filekey  = 'CODEZERO1'
      filekey2 = 'CODEZERO2'
    ELSE
      filekey  = 'CODEZERO'
    ENDIF
    outFileKey = 'CODESNG'
    refFileKey = 'CODEREF'
    CALL readKeys('EXT_CSH',keyValue,irc)
    CALL ckoptl(1,'EXT_CSH',keyValue,srName,'Extension EXT_CSH',         &
                irc,ircSum,maxVal=1,result1=ext_sh)
    CALL readKeys('DIR_CSH',keyValue,irc)
    CALL ckoptl(1,'DIR_CSH',keyValue,srName,'Path DIR_CSH',              &
                irc,ircSum,maxVal=1,result1=dir_sh)
    CALL readKeys('EXT_CSO',keyValue,irc)
    CALL ckoptl(1,'EXT_CSO',keyValue,srName,'Extension EXT_CSO',         &
                irc,ircSum,maxVal=1,result1=ext_so)
    CALL readKeys('DIR_CSO',keyValue,irc)
    CALL ckoptl(1,'DIR_CSO',keyValue,srName,'Path DIR_CSO',              &
                irc,ircSum,maxVal=1,result1=dir_so)
  ENDIF

! on error exit
  IF (ircSum /= 0) CALL exitrc(2)

! Read zero-diff. observation files
! ---------------------------------
  IF (strategy /= 1) THEN
    CALL gtfile(fileKey,2,mxcfil,nZeroFil,filinp)

! Satellite System
! ----------------
    CALL readKeys('SATSYS', keyValue, irc)
    CALL ckoptc(1,'SATSYS', keyValue, &
                (/'GPS    ','GLONASS','GALILEO','GPS&GLO', &
                  'GPS&GAL','GLO&GAL','ANY    '/), &
                'RDIGEN', 'Satellite System', irc, ircSum, &
                valList=(/1,2,3,4,5,6,0/), result1=isasys)
    IF (isasys /= 0) THEN
      WRITE(lfnprt,'(A,/,A)')                                  &
     '  NUM HEADER FILE NAMES                 STATION NAME' // &
     '    #SAT  SYS    REMARK',                                &
     ' ---------------------------------------------------' // &
     '----------------------------'
      DO IFIL=1,nZeroFil
        CALL rdhead2(filinp(1,ifil),zeroHead)

        isSys = 0
        DO isat=1,zeroHead%nsatel
          isSys(zeroHead%sat(isat)%numSat/100) = &
            zeroHead%sat(isat)%numObs(1)+zeroHead%sat(isat)%numObs(zeroHead%nFreq)
        ENDDO

        IF (isasys == 1 .AND. isSys(0) == 0) THEN
          filinp(2,ifil) = ''
        ELSEIF (isasys == 2 .AND. isSys(1) == 0) THEN
          filinp(2,ifil) = ''
        ELSEIF (isasys == 3 .AND. isSys(2) == 0) THEN
          filinp(2,ifil) = ''
        ELSEIF (isasys == 4 .AND. isSys(0)*isSys(1) == 0) THEN
          filinp(2,ifil) = ''
        ELSEIF (isasys == 5 .AND. isSys(0)*isSys(2) == 0) THEN
          filinp(2,ifil) = ''
        ELSEIF (isasys == 6 .AND. isSys(1)*isSys(2) == 0) THEN
          filinp(2,ifil) = ''
        ENDIF

        line=''
        WRITE(line,'(I5,1X,A,2X,A,I4)') &
              ifil,filinp(1,ifil),zeroHead%sta(1)%staNam,zeroHead%nsatel
        DO iSys = 0,maxsys-1
          IF (isSys(iSys) /= 0) &
            WRITE(line(63+iSys:63+iSys),'(A1)') g_svnsys(iSys)
        ENDDO
        IF (filinp(2,ifil) == '') THEN
          WRITE(line(70:80),'(A)') 'excluded'
        ELSE
          WRITE(line(70:80),'(A)') 'included'
        ENDIF
        WRITE(lfnprt,'(A)') TRIM(line)
      ENDDO
      WRITE(lfnprt,'(//)')

      jFil = 0
      DO iFil=1,nZeroFil
        IF (filinp(2,iFil) /= '') THEN
          jFil = jFil+1
          IF (iFil /= jFil) filinp(1:2,jFil) = filinp(1:2,iFil)
        ENDIF
      ENDDO
      nZeroFil = jFil
    ENDIF


  ELSE
    CALL gtfile(fileKey2,2,1,jj,filInp)
    filInp(1:2,2) = filInp(1:2,1)
    CALL gtfile(fileKey, 2,1,ii,filInp)

    IF (ii /= 1 .OR. jj /= 1) THEN
      WRITE(LFNERR,'(/,A,/16X,A,/)')                                  &
      ' *** SR SDFFIL: No zero-difference observation files for ',    &
                      'strategy MANUAL selected.'
      CALL exitrc(2)
    ENDIF

    nZeroFil = 2
  ENDIF

! Read reference station file (strategy "manual star")
! ----------------------------------------------------
  IF (strategy == 4) THEN
    CALL gtfile(refFileKey,2,1,nRefFil,refInp)
    IF (nRefFil == 1) THEN
      strategy = 6
      nRefFil = 0
      DO ii=1,nZeroFil
        IF (filinp(1,ii)==refInp(1)) THEN
          nRefFil = ii
          EXIT
        ENDIF
      ENDDO
      IF (nRefFil == 0) THEN    ! Should never happen, after a normal menu run
        nZeroFil = nZeroFil+1
        filinp(:,nZeroFil) = refInp(:)
        nRefFil = nZeroFil
      ENDIF
    ENDIF
  ENDIF


! --------------------------------------------------------------------------
! 1:MANUAL, 2:OBS-MAX, 3:SHORTEST, 4:A-STAR, 5:DEFINED, 6:M-STAR, DEF+REPL |
!---------------------------------------------------------------------------

! Read station names
! ------------------
  ALLOCATE(staZero(nZeroFil),STAT=iac)
  CALL alcerr(iac,'staZero',(/nZeroFil/),'sdffil')

  nsatfl = 0
  nsysfl = 0
  DO ii = 1,nZeroFil
    CALL rdhead2(filinp(1,ii),zeroHead)
    staZero(ii)=zeroHead%sta(1)%stanam

    isysfl = 0
    ! Count the number of files with observations to the differenet satellites
    DO iSat = 1,zeroHead%nSatel
      kSat = 0
      DO jSat = 1,nsatfl
        IF (satfil(1,jsat) == zeroHead%sat(iSat)%numSat) THEN
          IF( zeroHead%sat(iSat)%numObs(1)              > 0 .AND. &
              zeroHead%sat(iSat)%numObs(zeroHead%nFreq) > 0) THEN
            kSat = jSat
          ELSE
            kSat = -1
          ENDIF
          EXIT
        ENDIF
      ENDDO
      IF (kSat == -1 ) CYCLE
      IF (kSat == 0) THEN
        nSatfl = nSatfl + 1
        kSat = nSatfl
        satfil(1,ksat) = zeroHead%sat(iSat)%numSat
        satfil(2,ksat) = 0
      ENDIF
      satfil(2,ksat) = satfil(2,ksat) + 1
      isysfl(satfil(1,jsat)/100+1) = isysfl(satfil(1,jsat)/100+1) + 1
    ENDDO
    DO iSys = 1,maxsys
      IF (isysfl(isys)>0) nsysfl(isys) = nsysfl(isys) + 1
    ENDDO
  END DO
!
  jSat = 0
  DO iSat = 1,nSatfl
    iSys = satFil(1,isat)/100 + 1
    IF ( nsysfl(iSys) == 0 ) CYCLE
    IF ( crtSat /= 0d0 .AND. &
         DBLE(satfil(2,isat))/DBLE(nsysfl(iSys)) < crtsat ) THEN
      jSat = jSat + 1
      IF ( iSat > jSat ) satfil(:,jSat) = satfil(:,iSat)
    ENDIF
  ENDDO
  nSatFl = jSat
  satfil(2,1:nSatFl)=0


! Read station coordinates (OBS-MAX, SHORTEST, A-STAR, DEF+REP)
! -------------------------------------------------------------
  IF (strategy /= 1 .AND. strategy /= 5 .AND. strategy /= 6) THEN
    ALLOCATE(stanum (nZeroFil)  , stat=iac)
    CALL alcerr(iac, 'stanum', (/nZeroFil/),   'sdffil')
    ALLOCATE(icentr (nZeroFil)  , stat=iac)
    CALL alcerr(iac, 'icentr', (/nZeroFil/),   'sdffil')
    ALLOCATE(xstat  (3,nZeroFil), stat=iac)
    CALL alcerr(iac, 'xstat',  (/3,nZeroFil/), 'sdffil')
    ALLOCATE(xstell (3,nZeroFil), stat=iac)
    CALL alcerr(iac, 'xstell', (/3,nZeroFil/), 'sdffil')
    ALLOCATE(xstecc (3,nZeroFil), stat=iac)
    CALL alcerr(iac, 'xstecc', (/3,nZeroFil/), 'sdffil')

    CALL GETSTA(nZeroFil, staZero, stanum, ncentr, icentr, xstat,       &
                xstell, xstecc, datum, aell, bell, dxell, drell, scell)
  ENDIF

! Read pre-defined baselines if BASDEF specified
! ----------------------------------------------
  IF (strategy /= 1 .AND. &                    ! Not for MANUAL and
      strategy /= 4 .AND. strategy /= 6) THEN  ! STAR strategies

    IF (strategy == 5 .OR. strategy == 7) THEN ! Mandatory for DEFINED
      CALL gtflna(1,'BASDEF',dummy, irc)
    ELSE
      CALL gtflna(0,'BASDEF',dummy, irc)
    ENDIF

    ! File exists
    IF (irc==0) THEN
      IF (replace) THEN
        CALL sddefb(1,nZeroFil,staZero,xstat,nSngFil,iCombi)
      ELSE
        CALL sddefb(0,nZeroFil,staZero,xstat,nSngFil,iCombi)
      ENDIF
    ENDIF

  ENDIF

! Define nSngFil and iCombi according to strategy
! -----------------------------------------------
  SELECT CASE (strategy)

! Strategy: MANUAL
    CASE (1)

      CALL gtfile(outFileKey,2,1,nSngFil,filout)
      IF (nSngFil /= 1) THEN
        filout(1:2,1) = ' '
        nSngFil = 1
      ENDIF

      iCombi(1,1) = 1
      iCombi(2,1) = 2

! Strategy: OBS-MAX
    CASE(2)
      nPreDef = nSngFil
      DO ii = 1,nZeroFil
        DO jj = ii+1,nZeroFil
! baseline length
          basLen = 0
          DO iLen = 1,3
            basLen = basLen+(xstat(iLen,ii)-xstat(iLen,jj))**2
          ENDDO
          basLen = DSQRT(basLen)
! use only baselines shorter than optMax and check if predefined
          IF (basLen/1000d0<=optMax) THEN
            good = 1
            DO kk=1,nPreDef
              IF ((ii==iCombi(1,kk).AND.jj==iCombi(2,kk)) .OR.             &
                  (ii==iCombi(2,kk).AND.jj==iCombi(1,kk))) good = 0
            ENDDO
            IF (good==1) THEN
              nSngFil = nSngFil+1
              iCombi(1,nSngFil) = ii
              iCombi(2,nSngFil) = jj
            ENDIF
          ENDIF

        ENDDO
      ENDDO

! Strategy: SHORTEST
    CASE(3)
      CALL sdshrt(nZeroFil,xstat,nSngFil,iCombi,irc)

! Strategy: AUTO-STAR
    CASE(4)
      CALL sdastr(nZeroFil,xstat,nrRef,irc)
      nSngFil = 0
      DO ii = 1,nZeroFil
        IF (ii /= nrRef) THEN
          nSngFil = nSngFil+1
          iCombi(1,nSngFil) = nrRef
          iCombi(2,nSngFil) = ii
        ENDIF
      ENDDO

! Strategy: DEFINED
    CASE(5)

! Strategy: MANUAL-STAR
    CASE(6)
      nSngFil = 0
      DO ii = 1,nZeroFil
        IF (ii == nRefFil) CYCLE
        nSngFil = nSngFil+1
        iCombi(1,nSngFil) = nRefFil
        iCombi(2,nSngFil) = ii
      ENDDO

! Strategy: DEF+REPLACE
    CASE(7)

  ENDSELECT

! Define the Names of Single-difference Files (if not MANUAL)
! -----------------------------------------------------------
  IF (strategy /= 1 .OR. LEN_TRIM(filout(1,1)) == 0) THEN
    ALLOCATE(name4(nSngFil),stat=iac)
    CALL alcerr(iac,'name4',(/nSngFil/),'sdffil')
    CALL dfSngNam(staZero,nSngFil,iCombi,name4)

! get session
    lenDot  = INDEX(filinp(1,nZeroFil),'.',BACK=.true.)
    sesName = filinp(1,nZeroFil)(lenDot-4:lenDot)

! set single-diff. filenames
    DO ii = 1,nSngFil
      filout(1,ii) = TRIM(dir_sh)//name4(ii)//sesName(1:5)//TRIM(ext_sh)
      filout(2,ii) = TRIM(dir_so)//name4(ii)//sesName(1:5)//TRIM(ext_so)
    ENDDO

! check for double filenames
    DO ii = 1,nSngFil
      DO jj = ii+1,nSngFil
        IF (name4(ii) == name4(jj)) THEN
          WRITE(LFNERR,'(A,/,16X,A,/,16X,A,A)')                              &
          ' ### SR SDFFIL: A single-diff. filename appears more than once.', &
                          'This may result in data loss.',                   &
                          'Double filename: ',filout(1,ii)
        ENDIF
      ENDDO
    ENDDO

  ENDIF

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

  DEALLOCATE(staZero,STAT=iac)

  IF (strategy /= 1 .AND. strategy /= 5 .AND. strategy /= 6) THEN
    DEALLOCATE(stanum, stat=iac)
    DEALLOCATE(icentr, stat=iac)
    DEALLOCATE(xstat , stat=iac)
    DEALLOCATE(xstell, stat=iac)
    DEALLOCATE(xstecc, stat=iac)
  ENDIF

! The end
! -------
END SUBROUTINE sdffil

END MODULE
