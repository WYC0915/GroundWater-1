
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM irv2stv

! -------------------------------------------------------------------------
! Purpose:    Transform IRVs or precise orbit files into state vectors
!             (J2000).
!
! Remark:     Name of IRV file must start with Satellite-System and PRN!
!             Example for Lageos-1: L51
!
! Author:     C. Urschl
!
! Created:    23-Feb-2005
!
! Changes:    14-Mar-2005 CU: Remove structure, add option: create stv files
!                             from precise orbit files
!             14-Apr-2005 HU: Use nterface to RDPREH
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             07-JUL-2005 HB: Use t_epoch for gstime,
!                             add bias to parameter list of sr nuteff
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             27-Feb-2007 AG: Call DEFCON with parameter!
!             23-Sep-2010 RD: Enable CPU counter
!             14-Jan-2011 DT: bugfix call of sidmat corrected
!             21-Apr-2011 DT,KS: Writing of ELE-file added
!             12-May-2011 HB: Set model names through d_model
!             24-Oct-2011 DT: Naming of IRV file generalized
!             24-Nov-2011 SL: new title string for pritit, m_bern with ONLY
!             30-Dec-2011 DT: Adapt to new orbit model description
!             28-Mar-2012 RD: Use SVN2CHR as module now
!             28-Mar-2012 RD: Use SVNSYS as module now
!             27-Apr-2012 RD: Nullify pointers
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnprt, lfnerr, fileNameLength, &
                      keyValueLength, timStrgLength2, shortLineLength
  USE m_cpu,    ONLY: cpu_start
  USE m_maxdim, ONLY: maxsat, maxocn
  USE m_epoch,  ONLY: t_epoch, OPERATOR(.realToEpoch.)
  USE d_const,  ONLY: omega, GM, PI
  USE d_datum,  ONLY: datum
  USE d_inpkey, ONLY: inpkey, init_inpkey
  USE d_model,  ONLY: setModKey, chrValLength, mod_orb_prcMod
  USE p_orbgen, ONLY: orbdsc
  USE s_readirv,ONLY: readirv
  USE s_writstv,ONLY: writstv

  USE s_alcerr
  USE s_chr2svn
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckopti
  USE s_ckoptr
  USE s_ckoptt
  USE s_defcon
  USE s_dmlmtv
  USE s_eletra
  USE s_exitrc
  USE s_getdat
  USE s_getpot
  USE s_getsp3
  USE s_gtfile2
  USE s_gtflna
  USE s_gttimwin
  USE s_nuteff
  USE s_opnsys
  USE s_orbmdnam
  USE s_otidnam
  USE s_poldef
  USE s_prceff
  USE s_prfile
  USE s_prflna
  USE s_pritit
  USE s_prn2cos
  USE s_rdnutsub
  USE s_rdpreh
  USE s_readinpf
  USE s_readkeys
  USE s_sidmat
  USE s_svn2chr
  USE s_timst2
  USE s_writele
  USE s_xyzele
  USE f_gstime
  USE f_svnsys

  IMPLICIT NONE

! Local Parameters
! ----------------
  CHARACTER(LEN=10),               PARAMETER :: pgNam    = 'PG IRV2STV'
  CHARACTER(LEN= 8),               PARAMETER :: pgName   = 'IRV2STV '
  CHARACTER(LEN=6),  DIMENSION(2), PARAMETER :: filkey   = &
    (/'IRVFIL', 'PREFIL'/)

  CHARACTER(LEN=20), DIMENSION(2), PARAMETER :: fildescr = &
    (/'Input IRV files     ','Input precise orbits'/)

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength), DIMENSION(:,:), POINTER     :: orbfil
  CHARACTER(LEN=fileNameLength)                              :: stvfil
  CHARACTER(LEN=fileNameLength)                              :: elefil
  CHARACTER(LEN=keyValueLength), DIMENSION(:),   POINTER     :: keyValue
  CHARACTER(LEN=timStrgLength2)                              :: timstring
  CHARACTER(LEN=chrValLength)                                :: chrVal
  CHARACTER(LEN=1)                                           :: sattyp
  CHARACTER(LEN=9)                                           :: satcos
  CHARACTER(LEN=7)                                           :: satchr
  CHARACTER(LEN=20)                                          :: satcom
  CHARACTER(LEN=16)                                          :: subnam
  CHARACTER(LEN=16)                                          :: nutnam
  CHARACTER(LEN=9),              DIMENSION(:),   ALLOCATABLE :: stvcos
  CHARACTER(LEN=20),             DIMENSION(:),   ALLOCATABLE :: stvcom
  CHARACTER(LEN=80)                                          :: titlee
  CHARACTER(LEN=shortLineLength)                             :: titPot
  CHARACTER(LEN=16)                                          :: potnam
  CHARACTER(LEN=16)                                          :: otidnm
  CHARACTER(LEN=16)                                          :: tponam

  INTEGER(i4b)                                               :: irc, ios, irCode
  INTEGER(i4b)                                               :: ichar
  INTEGER(i4b)                                               :: isat, nsat
  INTEGER(i4b)                                               :: numsat
  INTEGER(i4b)                                               :: ifil, nfil
  INTEGER(i4b)                                               :: idx1
  INTEGER(i4b)                                               :: satmod
  INTEGER(i4b),                  DIMENSION(:),   ALLOCATABLE :: satnum
  INTEGER(i4b)                                               :: filtyp
  INTEGER(i4b)                                               :: satsel
  INTEGER(i4b)                                               :: orbSys
  INTEGER(i4b)                                               :: qdeg, qvar
  INTEGER(i4b)                                               :: npotmx
  INTEGER(i4b)                                               :: otddeg
  INTEGER(i4b)                                               :: indtim
  INTEGER(i4b)                                               :: antthr
  INTEGER(i4b)                                               :: erpmod
!!!  INTEGER(i4b)                                               :: icmc
  INTEGER(i4b), DIMENSION(15)                                :: orbmod
  INTEGER(i4b)                                               :: dummy1,dummy2,dummy3

  REAL(r8b),                     DIMENSION(6)                :: statevec
  REAL(r8b),                     DIMENSION(:),   ALLOCATABLE :: stvtim
  REAL(r8b),                     DIMENSION(:),   ALLOCATABLE :: gpstim
  REAL(r8b),                     DIMENSION(:,:), ALLOCATABLE :: stvvec
  REAL(r8b),                     DIMENSION(:,:), ALLOCATABLE :: satele
  REAL(r8b)                                                  :: tobs
  REAL(r8b)                                                  :: tutc
  REAL(r8b)                                                  :: tdt
  REAL(r8b)                                                  :: ut1utc
  REAL(r8b)                                                  :: gpsutc
  REAL(r8b)                                                  :: sidtim
  REAL(r8b)                                                  :: eqequi
  REAL(r8b)                                                  :: xpol,ypol
  REAL(r8b),                     DIMENSION(3,3)              :: bias
  REAL(r8b),                     DIMENSION(3,3)              :: sid
  REAL(r8b),                     DIMENSION(3,3)              :: nut
  REAL(r8b),                     DIMENSION(3,3)              :: pre
  REAL(r8b),                     DIMENSION(2)                :: window
  REAL(r8b)                                                  :: ss, cs
  REAL(r8b),                     DIMENSION(3)                :: dvsat
  REAL(r8b)                                                  :: dtpi, dtpiv
  REAL(r8b)                                                  :: otdmin
  REAL(r8b)                                                  :: gm2, ae2
  REAL(r8b), DIMENSION(3)                                    :: dummyC, dummyS

  REAL(r8b)                                                  :: asat
  REAL(r8b)                                                  :: esat
  REAL(r8b)                                                  :: insat
  REAL(r8b)                                                  :: rasat
  REAL(r8b)                                                  :: t0sat
  REAL(r8b)                                                  :: u0sat
  REAL(r8b)                                                  :: persat
  REAL(r8b)                                                  :: tosc
  REAL(r8b),                     DIMENSION(7)                :: ele
  REAL(r8b)                                                  :: clkcor

  LOGICAL                                                    :: wrtStv, wrtEle
  LOGICAL,                       DIMENSION(2)                :: cmcyn

  TYPE(t_epoch)                                              :: ttobs
  TYPE(t_epoch)                                              :: ttdt

! Local Variables for sr rdpreh
! -----------------------------
  INTEGER(i4b)                                               :: ifrmat
  INTEGER(i4b),                  DIMENSION(:),   ALLOCATABLE :: satwgt
  INTEGER(i4b)                                               :: nepo
  REAL(r8b)                                                  :: tfirst
  REAL(r8b)                                                  :: dttab
  REAL(r8b)                                                  :: baspos
  REAL(r8b)                                                  :: basclk
  CHARACTER(LEN=57),             DIMENSION(4)                :: title
  CHARACTER(LEN=5)                                           :: datdes
  CHARACTER(LEN=5)                                           :: coosys
  CHARACTER(LEN=3)                                           :: orbtyp
  CHARACTER(LEN=4)                                           :: agency
  CHARACTER(LEN=2)                                           :: typfil
  CHARACTER(LEN=3)                                           :: timsys


! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Nullify pointer and initialization
! ----------------------------------
  NULLIFY(keyValue)
  NULLIFY(orbfil)
  numsat = 0

! Get the name of the input file
! ------------------------------
  CALL init_inpkey(inpkey)
  CALL readinpf('',inpkey)

! Open system files, define constants
! -----------------------------------
  CALL opnsys
  CALL defcon(1)

! Define geodetic datum
! ---------------------
  datum%name = 'WGS - 84'
  CALL getdat(datum%name, datum%aell, datum%bell, datum%dxell, &
              datum%drell, datum%scell)

! Automatic output generation
! ---------------------------
  CALL pritit(pgName,'Convert IRV or PRE files to state vectors',131)
  CALL prflna(131)

! Read Title line
! ---------------
  CALL readKeys('TITLE',keyValue,irc)
  IF (irc == 0) titlee = keyvalue(1)

! Get input file type
! -------------------
  CALL ckoptb(1, (/'RADIO_I','RADIO_P'/), pgNam, &
              'Select type of input file',irc,result1=filtyp)

! Print input file names into protocol file
! -----------------------------------------
  CALL prfile(filkey(filtyp),fildescr(filtyp),1)

! Get time window
! ---------------
  CALL gttimwin('WINDOW',(/'RADIO_1','RADIO_2'/),        &
                (/'SESSION_YEAR','SESSION_STRG'/),       &
                (/'STADAT','STATIM','ENDDAT','ENDTIM'/), &
                window)

! Print time window into protocol file
! ------------------------------------
  CALL timst2(1,2,window,timstring)
  WRITE(lfnprt,'(/,2(/,A),/,2(/,A),/,1X,A)')    &
    ' Observation window',                      &
    ' ------------------',                      &
    ' Start                End                ',&
    ' ----------------------------------------',&
      timstring

! Print satellite list header into protocol file
! ----------------------------------------------
  WRITE(lfnprt,'(/,2(/,A),/,2(/,A))')   &
    ' List of satellites',              &
    ' ------------------',              &
    ' PRN  COSPAR-ID',         &
    ' --------------'

! Get input orbit file names
! --------------------------
  CALL gtfile2(filkey(filtyp),1,nfil,orbfil)
  IF (nfil == 0) THEN
    WRITE(lfnerr,'(A,/)')' *** PG IRV2STV: No input orbit files found.'
    CALL exitrc(2)
  ENDIF

! Get name of state vector file
! -----------------------------
  CALL ckoptb(0, (/'RADIO_V'/), pgNam, &
              'Write STV file', irCode, resultL=wrtStv)

  IF (wrtStv) THEN
    CALL gtflna(0,'STVFIL',stvfil,irc)
    IF (irc /= 0) THEN
      WRITE(lfnerr,'(/,A,/,2A)')                               &
        ' *** PG IRV2STV: Error while reading STV file name.', &
        '                 STV file name: ', TRIM(stvfil)
      CALL exitrc(0)
    ENDIF
  ENDIF

! Get name of ELE file
! --------------------
  CALL ckoptb(0, (/'RADIO_E'/), pgNam, &
              'Write ELE file', irCode, resultL=wrtEle)

  IF (wrtEle) THEN
    CALL gtflna(0,'ELEFIL',elefil,irc)
    IF (irc /= 0) THEN
      WRITE(lfnerr,'(/,A,/,2A)')                               &
        ' *** PG IRV2STV: Error while reading ELE file name.', &
        '                 ELE file name: ', TRIM(elefil)
      CALL exitrc(0)
    ENDIF
  ENDIF

! Either STV or ELE file name has to exist
! ----------------------------------------
  IF ( stvfil=='' .AND. elefil=='' ) THEN
    WRITE(lfnerr,'(/,A,/,A)')                               &
      ' *** PG IRV2STV: Resulting file has to be specified:', &
      '                 either STV or ELE file name.'
    CALL exitrc(0)
  ENDIF


! Get list of satellites
! ----------------------
  IF (filtyp == 1) THEN

    nsat = nfil

  ELSEIF (filtyp == 2) THEN

    CALL ckoptb (1, (/'RADIO_A','RADIO_L'/), pgNam, &
               'Select satellite list',irc,result1=satsel)

  ! Use all satellites in precise orbit file
    IF (satsel == 1) THEN

      nsat = maxsat

  ! Use all satellites from satellite list
    ELSEIF (satsel == 2) THEN

      CALL readKeys('SATLST', keyValue, irc)
      nsat = SIZE(keyValue)
      nfil = 0

      DO isat = 1, nsat
        IF (LEN_TRIM(keyValue(isat)) == 0) nsat = 0
      ENDDO

    ! No satellites specified
      IF (nsat == 0 .OR. irc /= 0) THEN
        WRITE(lfnerr,'(A,A)')                                 &
          ' *** PG IRV2STV: There are no satellite numbers ', &
                           'specified in the input panel.'
        CALL exitrc(2)
      ENDIF

    ENDIF

  ENDIF

! Allocate arrays
! ---------------
  ALLOCATE (satnum(nsat),STAT=irc)
  CALL alcerr(irc,'satnum',(/nsat/),pgNam)
  ALLOCATE (stvtim(nsat),STAT=irc)
  CALL alcerr(irc,'stvtim',(/nsat/),pgNam)
  ALLOCATE (stvvec(nsat,6),STAT=irc)
  CALL alcerr(irc,'stvvec',(/nsat,6/),pgNam)
  ALLOCATE (stvcos(nsat),STAT=irc)
  CALL alcerr(irc,'stvcos',(/nsat/),pgNam)
  ALLOCATE (stvcom(nsat),STAT=irc)
  CALL alcerr(irc,'stvcom',(/nsat/),pgNam)
  ALLOCATE (satwgt(nsat),STAT=irc)
  CALL alcerr(irc,'satwgt',(/nsat/),pgNam)
  ALLOCATE (satele(nsat,6),STAT=irc)
  CALL alcerr(irc,'satele',(/nsat,6/),pgNam)
  ALLOCATE (gpstim(nsat),STAT=irc)
  CALL alcerr(irc,'gpstim',(/nsat/),pgNam)


! Set model names
! ---------------
  CALL rdnutsub(nutnam,subnam)
  chrVal = ' '
  chrVal(1:4)='BIAS'
  CALL setModKey(mod_orb_prcMod,chrVal,pgNam,0.D0)

! Get satellite names
! -------------------
  IF (filtyp == 1) THEN

    DO ifil = 1, nfil
      DO ichar = 1, filenamelength
        IF ( orbfil(1,ifil)(ichar:ichar) == "/" ) idx1 = ichar+1
      END DO

      sattyp = orbfil(1,ifil)(idx1:idx1)

      READ(orbfil(1,ifil)(idx1+1:idx1+2),'(I2)') satmod
      CALL chr2svn(satmod,sattyp,satnum(ifil))
    ENDDO

  ELSEIF (filtyp == 2) THEN

  ! Read header of precise orbit file
    IF (satsel == 1) THEN
      DO ifil = 1, nfil
        satnum = 0
        CALL rdpreh(orbfil(1,ifil),-1,ifrmat,nsat,satnum,satwgt,tfirst,    &
                    nepo,dttab,title,datdes,coosys,orbtyp,agency,typfil, &
                    timsys,baspos,basclk)
        IF (tfirst >= window(1) .AND. tfirst <= window(2)) EXIT
      ENDDO

  ! Read satellite list
    ELSEIF (satsel == 2) THEN
      DO isat = 1, nsat
        READ(keyValue(isat),*) satnum(isat)
      ENDDO
      DEALLOCATE(keyValue,stat=irc)
    ENDIF

  ENDIF


! Get description of ELE file
! -----------------------------
 IF (wrtEle) THEN

   ! Default
   orbmod(1:10) = (/99, 1, 1, 3, 1, 2, 1, 0, 0, 0 /)

   ! Get remaining models
   CALL readkeys('LENINTER', keyValue, irc)
   CALL ckoptt(1,'LENINTER',keyValue,pgNam,                        &
               'Length of interval for equ. of motion',irc,irCode, &
               ge=0d0,maxVal=1,result1=dtpi)

   CALL readkeys('POLDEGR', keyValue, irc)
   CALL ckopti(1,'POLDEGR', keyValue, pgNam,                       &
               'Polynomial degree for equ. of motion',irc,irCode,  &
               maxVal=1,ge=1,result1=qdeg)

   CALL readkeys('VARLNINT', keyValue, irc)
   CALL ckoptt(1,'VARLNINT', keyValue, pgNam,                      &
               'Length of interval for variation equ.',irc,irCode, &
               ge=0d0,maxVal=1,result1=dtpiv)

   CALL readkeys('VARPOLDG', keyValue, irc)
   CALL ckopti(1,'VARPOLDG', keyValue, pgNam,                      &
               'Polynomial degree for variation equ.',irc,irCode,  &
               maxVal=1,ge=1,result1=qvar)

   CALL getpot(2,window(1),2,dummy1,titPot,gm2,ae2,dummyC,dummyS, &
               dummy2,dummy3)
   potnam = titPot(3:18)

   CALL readkeys('MXPOTDEG', keyValue, irc)
   CALL ckopti(1,'MXPOTDEG', keyValue, pgNam,          &
               'Degree of earth potential',irc,irCode, &
               maxVal=1,ge=2,result1=npotmx)

   CALL otidnam(tponam,otidnm)

!!!   CALL readkeys('OTDMIN', keyValue, irc)
!!!   CALL ckoptr(1,'OTDMIN', keyValue, pgmName,                       &
!!!               'Min. size of ocean tides to be applied',irc,irCode, &
!!!               ge=0d0,result1=otdmin)
   otdmin = 0d0

   CALL readkeys('MXOCTI', keyValue, irc)
   CALL ckopti(1,'MXOCTI', keyValue, pgNam,        &
               'Degree of ocean tides',irc,irCode, &
               maxVal=1,ge=2,result1=otddeg)

   CALL readKeys('EMPIRI',keyValue,irc)
   CALL ckoptc(1,'EMPIRI',keyValue, (/'DRSW','DXY ','RSW '/), pgNam, &
              'Empirical orbit parameters', irc, irCode,             &
              valList=(/2,0,1/), maxVal=1, result1=orbSys)
   orbmod(6) = orbSys

   cmcyn(1) = .TRUE.
   cmcyn(2) = .FALSE.

   indtim = 1

   antthr = 0

   erpmod = 0

   CALL orbmdnam(orbmod,dtpi,qdeg,dtpiv,qvar,nutnam,subnam, &
                 potnam,npotmx,tponam,otidnm,otdmin,otddeg, &
                 cmcyn,indtim,antthr,erpmod)

 ENDIF

! Loop over all satellites
! ------------------------
  numsat = 0
  DO ifil = 1, nsat

  ! Read IRV files
  ! --------------
    IF (filtyp == 1) THEN
      CALL readirv(orbfil(1,ifil),window,statevec,tutc,ios)
      IF (ios == 0) THEN
        WRITE(lfnerr,'(/,A,/,2(2A,/))')                                   &
          ' ### PG IRV2STV: Could not find state vector in IRV file.',    &
          '                 IRV file:    ', TRIM(orbfil(1,ifil)),         &
          '                 Time window: ', timstring
        CYCLE
      ENDIF

    ELSEIF (filtyp == 2) THEN

      tutc = window(1)

    ENDIF

  ! Print satellite names into protocol file
  ! ----------------------------------------
    CALL svn2chr(satnum(ifil),satmod,sattyp)
    CALL prn2cos(11,satnum(ifil),window(1),satcos,irc)
    IF (irc /= 0) CYCLE

    WRITE(lfnprt,'(1X,A1,I2.2,2X,A9)') &
      sattyp,satmod,satcos

    satchr = ''
    IF (svnsys(0,1,satnum(ifil))) satchr='GPS'
    IF (svnsys(1,1,satnum(ifil))) satchr='GLONASS'
    IF (svnsys(9,1,satnum(ifil))) satchr='LEO'
    WRITE(satcom, '(A1,I2.2,"_",A)') sattyp,satmod,TRIM(satchr)

  ! Compute transformation matrices
  ! -------------------------------
    CALL poldef(tutc,1,xpol,ypol,ut1utc,gpsutc)

  ! UTC -> GPS
    tobs = tutc + gpsutc

    IF (filtyp == 1) THEN

    ! TT  = GPS  +  TAI-GPS + TT-TAI
      tdt  = tobs + (19d0    + 32.184d0)/86400d0

    ! Precession:                                  pre
      CALL prceff(2,5d0,tdt,pre)

    ! Nutation:                                    nut
      CALL nuteff(2,0.1d0,tdt,nut,eqequi,bias)

    ! Frame bias:                                  bias
      pre = matmul(pre,bias)

    ! Polar motion:                                xpol,ypol,gpsutc
      CALL poldef(tutc,1,xpol,ypol,ut1utc,gpsutc)

    ! GAST (rad):                                  sidtim
      ttobs =.realToEpoch.(tobs+ut1utc-gpsutc)
      ttdt  =.realToEpoch.tdt
      sidtim = gstime(0,ttobs,ttdt,nut(2,1),eqequi)

    ! Polar motion + GAST:                         sid
      CALL sidmat(tdt,xpol,ypol,sidtim,sid)


    ! Transformation of irv into Celestial Reference System
    ! -----------------------------------------------------

    ! Satellite coordinates:    TRF              TRF -> CRF
    ! ---------------------                      ==========
    ! (1) Polar motion, GAST                     -> True system of date

    ! v(inertial) = S*vsat + Sdot*xsat
      ss = DSIN(sidtim)
      cs = DCOS(sidtim)
    ! Sdot*xsat
      dvsat(1) = omega * (-ss*statevec(1) - cs*statevec(2))
      dvsat(2) = omega * ( cs*statevec(1) - ss*statevec(2))
      dvsat(3) = 0d0

    ! x(inertial) = S*xsat
      CALL dmlmtv(statevec(1:3),sid,statevec(1:3))
    ! v(inertial) = S*vsat + ...
      CALL dmlmtv(statevec(4:6),sid,statevec(4:6))
    ! ... Sdot*xsat
      statevec(4) = statevec(4) + dvsat(1)
      statevec(5) = statevec(5) + dvsat(2)
      statevec(6) = statevec(6) + dvsat(3)

    ! (2) Nutation and precession                -> J2000
      CALL dmlmtv(statevec(1:3),nut,statevec(1:3))
      CALL dmlmtv(statevec(4:6),nut,statevec(4:6))
      CALL dmlmtv(statevec(1:3),pre,statevec(1:3))
      CALL dmlmtv(statevec(4:6),pre,statevec(4:6))

    ELSEIF (filtyp == 2) THEN

      CALL getsp3(satnum(ifil),0,1,2,tobs,statevec,tosc,ele,clkcor,irc)
      IF (irc /= 0) CYCLE

    ENDIF

  ! Save satellite array
  ! --------------------
    numsat             = numsat + 1
    stvtim(numsat)     = tutc
    gpstim(numsat)     = tobs
    stvvec(numsat,1:6) = statevec(1:6)
    stvcos(numsat)     = satcos
    stvcom(numsat)     = satcom

  ! Compute satellite orbital elements ELE
  ! --------------------------------------
    CALL XYZELE(GM,stvtim(numsat),stvvec(numsat,1:3),stvvec(numsat,4:6), &
                numsat,asat,esat,insat,rasat,persat,t0sat)
    CALL ELETRA(1,asat,esat,persat,stvtim(numsat),t0sat,u0sat)
    IF (persat<0.0) THEN
      persat=persat+2d0*PI
    ENDIF

  ! Save satellite array
  ! --------------------
    satele(numsat,1) = asat
    satele(numsat,2) = esat
    satele(numsat,3) = insat *180d0/PI
    satele(numsat,4) = rasat *180d0/PI
    satele(numsat,5) = persat*180d0/PI
    satele(numsat,6) = u0sat *180d0/PI

  ENDDO


! Write STV file
! --------------
  IF (wrtStv) THEN
    CALL writstv(stvfil,numsat,stvtim,stvvec,stvcos,stvcom)
  ENDIF

! Write ELE file
! --------------
  IF (wrtEle) THEN
    CALL writele(titlee,elefil,numsat,gpstim,satele,satele,satnum, &
                 orbdsc,orbSys)
  ENDIF

  CALL exitrc(0)

END PROGRAM irv2stv



