MODULE s_AOPTNET
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE aoptnet(opt, namList)

! -------------------------------------------------------------------------
! Purpose:    Reads input options for ADDNEQ2:
!             constraining and free network solution for
!             coordinates and velocities.
!
! Remark:     It is assumed that this subroutine is the FIRST one
!             reading values for "opt%sigma"
!
! Author:     M. Meindl
!
! Created:    10-Jul-2001
!
! Changes:    29-Aug-2001 RD: Use the general SR gtStaNum
!             07-Sep-2001 RD: Genrate some pgm output
!             13-Sep-2001 RD: Sigma array is allocated to 3 (because output)
!             19-Oct-2001 RD: Really fix coordinates and velocities
!             11-Dec-2001 HU: Format statement corrected
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             22-Dec-2001 HU: Interface to readstsg added
!             08-Feb-2002 MM: Use t_sigma for local sigmas
!                             Enable relative sigmas
!             12-Feb-2002 MM: handle new preelimination modes
!             08-Oct-2002 RD: New call of SR gtStaNum
!             27-Nov-2002 CU: Remove locSig due to changing order of
!                             SR calls in SR addrdopt,
!                             print title line in protocol
!             10-Dec-2002 CU: Use SR prisig instead of prifree to print
!                             free network condition,
!                             use dummy variable in SR gtstanum call due to
!                             compiler problems (-C)
!             21-Dec-2002 HU: Only warning if datum not defined
!             27-Jan-2003 RD: A solution without any constraints is possible now
!             03-Feb-2003 RD: New call of GTSTANUM (weight is pointer now)
!             27-Feb-2003 HU: DATUM from D_DATUM
!             19-Mar-2003 RD: Correct SIZE statement (because of IFC)
!             03-Apr-2003 RD: Report relative constraints from STACRUX
!             23-Apr-2003 CU: Nullify local pointers
!             15-May-2003 AJ: Initialize structure
!             18-May-2003 HU: Deallocate array
!             22-May-2003 RD: Make deallocation safe
!             07-Jul-2003 MM: N, E, U constraining implemented
!             26-Aug-2003 RD: Correct pointer handling in opt%elimi
!             16-Sep-2003 RD: STACRUX->STAINFO
!             02-Oct-2003 RD: Do not allocate sub-pointer for the buffer
!             29-Oct-2003 MM: New datum definition type (NONE)
!                             New minimum constraint conditions
!             24-Nov-2003 HU: Additional argument for prista
!             04-Dec-2003 HB: Call of SR gtStaNum with hlpNam
!             11-Dec-2003 MM: New datum definition type (NONE)
!                             New minimum constraint conditions
!             03-Oct-2004 HU: Initialize ircSum
!             07-Oct-2004 HU: Typing error
!             27-Nov-2009 RD: Station exceptions for pre-elimination
!             20-May-2010 MF: Nullify hlpNam
!             27-Oct-2010 SL: use m_bern with ONLY
!             08-Nov-2010 RD: Select HELMERT parameter for repeatability
!             03-Jan-2011 DT: set crdList%sigma using d0
!             19-Jan-2011 RD: Use GETSTA, add flag to GTVELO
!             03-Feb-2011 SL: call prista with datum%name
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnErr, lfnPrt, staNameLength, &
                      keyValueLength, fileNameLength, staFlagLength, &
                      lineLength, shortLineLength
  USE d_stalst, ONLY: t_staList,init_stalist
  USE d_datum,  ONLY: t_datum
  USE d_stacrx, ONLY: t_stacrux, undef_s, init_stacrux
  USE p_addneq, ONLY: t_opt, t_elimi, t_namLst

  USE s_ckoptr
  USE s_alcerr
  USE s_privel
  USE s_readcrux
  USE s_gtvelo
  USE s_pridat
  USE s_prista
  USE s_gtstanum
  USE s_prisig
  USE s_readkeys
  USE s_prists
  USE s_getsta
  USE s_exitrc
  USE s_gtflna
  USE s_readstsg
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  TYPE(t_namLst)                              :: namList ! Station list

! input/output:

! output:
  TYPE(t_opt)                                 :: opt     ! Options

! List of Functions
! -----------------

! Local Types
! -----------
  TYPE(t_staList)                                         :: crdList, velList
  TYPE(t_datum)                                           :: datum
  TYPE(t_elimi),DIMENSION(:),POINTER                      :: oldElimi

! Local Parameters
! ----------------

! Local Variables
! ---------------
  TYPE(t_stacrux)                                         :: staCrux

  CHARACTER(LEN=keyValueLength),DIMENSION(:), POINTER     :: keyValue
  CHARACTER(LEN=fileNameLength)                           :: fixFil
  CHARACTER(LEN=fileNameLength)                           :: staFil
  CHARACTER(LEN=5)                                        :: hlpKey
  CHARACTER(LEN=staFlagLength), DIMENSION(:), ALLOCATABLE :: velFlg
  CHARACTER(LEN=shortLineLength)                          :: wgtFile
  CHARACTER(LEN=lineLength)                          :: line
  CHARACTER(LEN=7)                                   :: srName = 'aoptnet'
  CHARACTER(LEN=staNameLength),DIMENSION(:),POINTER       :: hlpNam

  INTEGER(i4b)                                            :: oldElmTyp
  INTEGER(i4b)                                            :: maxElmTyp
  INTEGER(i4b)                                            :: staSigTyp
  INTEGER(i4b)                                            :: nCentr
  INTEGER(i4b), DIMENSION(:),   ALLOCATABLE               :: iCentr
  INTEGER(i4b), DIMENSION(:),   POINTER                   :: crdNum, velNum
  INTEGER(i4b)                                            :: nElm
  INTEGER(i4b)                                            :: nSig, iSig
  INTEGER(i4b)                                            :: nSigCrd, nSigVel
  INTEGER(i4b)                                            :: iSigma
  INTEGER(i4b)                                            :: freeCrd, freeVel
  INTEGER(i4b)                                            :: modeCrd, modeVel
  INTEGER(i4b)                                            :: ircvel, ircSum
  INTEGER(i4b)                                            :: ii, i1,i2
  INTEGER(i4b)                                            :: irc, iac
  INTEGER(i4b)                                            :: iCrd, iVel
  INTEGER(i4b)                                            :: nSta
  INTEGER(i4b)                                            :: iSta,mSta
  INTEGER(i4b)                                            :: iCooVel
  INTEGER(i4b), DIMENSION(14)                             :: seqSta

  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE               :: xStat  ! xyz coord
  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE               :: xStell ! ell coord
  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE               :: xStecc ! ecc vetor
  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE               :: xVel   ! xyz veloc
  REAL(r8b),    DIMENSION(14)                             :: sigSta
  REAL(r8b),    DIMENSION(:,:), POINTER                   :: dummy

  LOGICAL                                                 :: isVelo
  LOGICAL,      DIMENSION(:),   ALLOCATABLE               :: vfound
  LOGICAL                                                 :: staPrt


! Allocate meomry for some arrays
! -------------------------------
  nSta    = 0
  wgtFile = ' '
  ircSum  = 0

  CALL init_stalist(crdList)
  CALL init_stalist(velList)
  CALL init_stacrux(staCrux)
  NULLIFY(oldElimi)
  NULLIFY(keyValue)
  NULLIFY(crdNum)
  NULLIFY(velNum)
  NULLIFY(dummy)
  NULLIFY(hlpNam)

! If no stations in NEQs, nothing to do
! -------------------------------------
  IF (namList%nSta == 0) THEN
    ALLOCATE(opt%sigma(0),stat=iac)
    CALL alcerr(iac,'opt%sigma',(/0/),'aoptnet')
    RETURN
  ENDIF

  ALLOCATE(crdNum(namList%nSta), stat=irc)
  CALL alcerr(irc, 'crdNum', (/namList%nSta/), 'aoptnet')

  ALLOCATE(velNum(namList%nSta), stat=irc)
  CALL alcerr(irc, 'velNum', (/namList%nSta/), 'aoptnet')

  ALLOCATE(crdList%staNam(namList%nSta), stat=irc)
  CALL alcerr(irc, 'crdList%staNam', (/namList%nSta/), 'aoptnet')

  ALLOCATE(hlpNam(namList%nSta), stat=irc)
  CALL alcerr(irc, 'hlpNam', (/namList%nSta/), 'aoptnet')

  ALLOCATE(velList%staNam(namList%nSta), stat=irc)
  CALL alcerr(irc, 'velList%staNam', (/namList%nSta/), 'aoptnet')

  ALLOCATE(xStat (3,namList%nSta), stat=irc)
  CALL alcerr(irc,'xStat' ,(/3,namList%nSta/), 'aoptnet')

  ALLOCATE(xStell(3,namList%nSta), stat=irc)
  CALL alcerr(irc,'xStell',(/3,namList%nSta/), 'aoptnet')

  ALLOCATE(xStecc(3,namList%nSta), stat=irc)
  CALL alcerr(irc,'xStecc',(/3,namList%nSta/), 'aoptnet')

  ALLOCATE(iCentr(namList%nSta),   stat=irc)
  CALL alcerr(irc,'iCentr',(/namList%nSta/),   'aoptnet')

  ALLOCATE(xVel(3,namList%nSta),   stat=irc)
  CALL alcerr(irc,'xVel',(/3,namList%nSta/),   'aoptnet')

  ALLOCATE(velFlg(namList%nSta),   stat=irc)
  CALL alcerr(irc,'velFlg',(/namList%nSta/),   'aoptnet')

  ALLOCATE(vfound(namList%nSta),   stat=irc)
  CALL alcerr(irc,'vfound',(/namList%nSta/),   'aoptnet')

! Velocity estimation?
! --------------------
  CALL readKeys('CRD_NINT',keyValue,irc)
  isVelo = (irc == 0 .AND. keyValue(1) == '1')

!  CALL readKeys('INPFILE',keyValue,irc)
!  IF (irc == 0 .AND. SIZE(keyValue) == 1) isVelo = .FALSE.


! Read apriori coordinates, station numbers etc.
! ----------------------------------------------
  CALL getSta(namList%nSta, namList%nam(:), namList%num(:), nCentr, iCentr, &
              xStat, xStell, xStecc,  datum%name, datum%aEll, datum%bEll,   &
              datum%dxEll, datum%drEll, datum%scEll)

  IF (isVelo) THEN
    xvel   = 0.d0
    CALL gtvelo('VELAPR ', 0, 1, (/ '@' /), namList%nSta, xStat, &
                namList%nam(:), xvel, velflg, vfound, ircvel)
  ENDIF

! Do not use the real station numbers (it used as a counter resp. index)
! -----------------------------------
  namList%num(:) = (/ (ii,ii=1,namList%nSta) /)

! ------------------------------------------------------------------------
! COORDINATES
! ------------------------------------------------------------------------

  crdList%nSta = 0

! Type of solution (fixing 1, constraining 2, free network 3)
! -----------------------------------------------------------
  modeCRD = 1
  freeCrd = 1
  CALL readkeys('RADIO2_1', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') modeCrd = 1
  CALL readkeys('RADIO2_2', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') modeCrd = 2
  CALL readkeys('RADIO2_3', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') THEN
    modeCrd = 3
    freeCrd = -1
  ENDIF
  CALL readkeys('RADIO2_4', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') modeCrd = 4


! Fixing or constraining coordinates or free network
! --------------------------------------------------

! No datum definition
! -------------------
  IF (modeCrd==4) THEN
    crdList%nSta = 0

! Fixing
! ------
  ELSE IF (modeCrd == 1) THEN
    CALL gtStaNum(namList%nSta, namList%num(:), namList%nam(:), &
                  'FIXSTA','STATION1','STAFILE1','STAFLAG1',    &
                  crdList%nSta, crdNum, hlpNam, 0, dummy)
    crdList%staNam(1:crdList%nSta) = hlpNam(1:crdList%nSta)
    ALLOCATE(crdList%sigma(3,crdList%nSta), stat=irc)
    CALL alcerr(irc,'crdList%sigma',(/3,crdList%nSta/), 'aoptnet')

    crdList%sigma = 0.0001D0

!    IF (crdList%nSta > 0)                                                     &
!    WRITE(lfnerr,'(/,A,/,17X,A,/)')                                           &
!    ' ### SR AOPTNET: Fixing of coordinates is not implemented (until now).', &
!                     'The coordinates are constraint with 0.0001 m!'


! Constraining
! ------------
  ELSEIF (modeCrd == 2) THEN
    iSigma = 3
    CALL readKeys('SIGSTA',keyValue,irc)
    IF (irc == 0 .AND. keyValue(1) == 'FROM_FILE') THEN
      CALL gtflna(0,'STAFILE4',fixFil,irc)
      wgtfile(1:32) = TRIM(fixFil)
      IF (irc == 0 .AND. LEN_TRIM(fixFil) > 0) THEN
        iSigma = -1
        CALL readstsg(fixFil,iSigma,crdList)
        IF (iSigma /= 3) iSigma = 3
      ENDIF
    ENDIF

    ALLOCATE(crdList%sigma(3,namList%nSta),stat=irc)
    CALL alcerr(irc, 'crdList%sigma', (/3,namList%nSta/), 'aoptnet')

! read north, east, up sigmas
    ircSum = 0
    CALL readkeys('C_SIGMAN',keyValue,irc)
    CALL ckoptr(1,'C_SIGMAN',keyValue,srName,'Coordinate sigma (north)',  &
                irc,ircSum,empty=0.d0,ge=0.d0,result1=crdList%sigma(1,1))
    CALL readkeys('C_SIGMAE',keyValue,irc)
    CALL ckoptr(1,'C_SIGMAE',keyValue,srName,'Coordinate sigma (east)',   &
                irc,ircSum,empty=0.d0,ge=0.d0,result1=crdList%sigma(2,1))
    CALL readkeys('C_SIGMAU',keyValue,irc)
    CALL ckoptr(1,'C_SIGMAU',keyValue,srName,'Coordinate sigma (up)',     &
                irc,ircSum,empty=0.d0,ge=0.d0,result1=crdList%sigma(3,1))

    CALL gtStaNum(namList%nSta, namList%num(:),namList%nam(:),            &
                  'SIGSTA','STATION4','STAFILE4','STAFLAG4',              &
                  crdList%nSta, crdNum, hlpNam, iSigma, crdList%sigma)
    crdList%staNam(1:crdList%nSta) = hlpNam(1:crdList%nSta)

    IF (iSigma == 1) THEN
      crdList%sigma(2,:) = crdList%sigma(1,:)
      crdList%sigma(3,:) = crdList%sigma(1,:)
    ENDIF

! Free network
! ------------
  ELSEIF (modeCrd == 3) THEN
    CALL gtStaNum(namList%nSta, namList%num(:), namList%nam(:),           &
                  'FREESTA','FREESTA_L','FREESTA_F','FREESTA_G',          &
                  crdList%nSta, crdNum, hlpNam, 0, dummy)
    crdList%staNam(1:crdList%nSta) = hlpNam(1:crdList%nSta)
! dummy only
    ALLOCATE(crdList%sigma(3,crdList%nSta), stat=irc)
    CALL alcerr(irc,'crdList%sigma',(/3,crdList%nSta/), 'aoptnet')

    crdList%sigma = 10.D0

! Error (should not happen)
! -------------------------
  ELSE
    WRITE(lfnerr,'(/,A,/,17X,A,I6,/)')                                    &
    ' *** SR AOPTNET: Wrong selection for handling of coordinates.',      &
                      'Specified option: ', modeCrd
    CALL exitrc(2)
  ENDIF


! Allocate for output
! -------------------
  IF (crdList%nSta == 0) THEN
    ALLOCATE(crdList%staNam(1), stat=irc)
    CALL alcerr(irc, 'crdList%staNam',(/1/),'atopnet')
    ALLOCATE(crdList%sigma(3,1), stat=irc)
    CALL alcerr(irc, 'crdList%sigma',(/3,1/),'atopnet')
  ENDIF

! ------------------------------------------------------------------------
! VELOCITIES
! ------------------------------------------------------------------------

  velList%nSta = 0

! Type of solution (fixing 1, constraining 2, free network 3)
! -----------------------------------------------------------
  modeVel = 1
  freeVel = 1
  CALL readkeys('RADIO3_1', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') modeVel = 1
  CALL readkeys('RADIO3_2', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') modeVel = 2
  CALL readkeys('RADIO3_3', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') THEN
    modeVel = 3
    freeVel = -1
  ENDIF
  CALL readkeys('RADIO3_4', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') modeVel = 4


! Fixing or constraining velocities or free network
! --------------------------------------------------

! No datum definition requested
  IF (isVelo .AND. modeVel==4) THEN
    velList%nSta = 0

! Fixing
  ELSE IF (isVelo .AND. modeVel == 1) THEN
    CALL gtStaNum(namList%nSta, namList%num(:), namList%nam(:),           &
                  'FIXVEL','VELOCITY1','VELFILE1','VELFLAG1',             &
                  velList%nSta, velNum, velList%staNam, 0, dummy)

    ALLOCATE(velList%sigma(3,velList%nSta), stat=irc)
    CALL alcerr(irc,'velList%sigma',(/3,velList%nSta/), 'aoptnet')

    velList%sigma = 0.0001

!    IF (velList%nSta > 0)                                                     &
!    WRITE(lfnerr,'(/,A,/,17X,A,/)')                                           &
!    ' ### SR AOPTNET: Fixing of velocities is not implemented (until now).',  &
!                     'The velocities are constraint with 0.0001 m!'

! Constraining
! ------------
  ELSEIF (isVelo .AND. modeVel == 2) THEN
    iSigma = 3
    CALL readKeys('SIGVEL',keyValue,irc)
    IF (irc == 0 .AND. keyValue(1) == 'FROM_FILE') THEN
      CALL gtflna(0,'VELFILE4',fixFil,irc)
      IF (wgtFile(1:32) == ' ') THEN
        wgtFile(1:32) = TRIM(fixFil)
      ELSEIF (wgtFile(1:32) /= TRIM(fixFil)) THEN
        wgtFile(35:66) = TRIM(fixFil)
      ENDIF
      IF (irc == 0 .AND. LEN_TRIM(fixFil) > 0) THEN
        iSigma = -1
        CALL readstsg(fixFil,iSigma,velList)
        IF (iSigma /= 3) iSigma = 3
      ENDIF
    ENDIF

    ALLOCATE(velList%sigma(3,namList%nSta),stat=irc)
    CALL alcerr(irc, 'velList%sigma', (/3,namList%nSta/), 'aoptnet')

! read velocity sigmas (north, east, up)
    CALL readkeys('V_SIGMAN',keyValue,irc)
    CALL ckoptr(1,'V_SIGMAN',keyValue,srName,'Velocity sigma (north)',    &
                irc,ircSum,empty=0.d0,ge=0.d0,result1=velList%sigma(1,1))
    CALL readkeys('V_SIGMAE',keyValue,irc)
    CALL ckoptr(1,'V_SIGMAE',keyValue,srName,'Velocity sigma (east)',     &
                irc,ircSum,empty=0.d0,ge=0.d0,result1=velList%sigma(2,1))
    CALL readkeys('V_SIGMAU',keyValue,irc)
    CALL ckoptr(1,'V_SIGMAU',keyValue,srName,'Velocity sigma (up)',       &
                irc,ircSum,empty=0.d0,ge=0.d0,result1=velList%sigma(3,1))

    IF (ircSum/=0) CALL exitrc(2)

    CALL gtStaNum(namList%nSta, namList%num(:), namList%nam(:),           &
                  'SIGVEL','VELOCITY4','VELFILE4','VELFLAG4',             &
                  velList%nSta, velNum, velList%staNam, iSigma, velList%sigma)

    IF (iSigma == 1) THEN
      velList%sigma(2,:) = velList%sigma(1,:)
      velList%sigma(3,:) = velList%sigma(1,:)
    ENDIF

! Free network
! ------------
  ELSEIF (isVelo .AND. modeVel == 3) THEN

    CALL gtStaNum(namList%nSta, namList%num(:), namList%nam(:),           &
                  'FREEVEL','FREEVEL_L','FREEVEL_F','FREEVEL_G',          &
                  velList%nSta, velNum, velList%staNam, 0, dummy)

! dummy only
    ALLOCATE(velList%sigma(3,velList%nSta), stat=irc)
    CALL alcerr(irc,'velList%sigma',(/3,velList%nSta/), 'aoptnet')

    velList%sigma = 0.0001

! Error (should never happen)
! ---------------------------
  ELSEIF (isVelo) THEN
    WRITE(lfnerr,'(/,A,/,17X,A,I6,/)')                               &
    ' *** SR AOPTNET: Wrong selection for handling of velocities.',  &
                      'Specified option: ', modeVel
    CALL exitrc(2)
  ENDIF

! Allocate for output
! -------------------
  IF (velList%nSta == 0) THEN
    ALLOCATE(velList%staNam(1), stat=irc)
    CALL alcerr(irc, 'velList%staNam',(/1/),'atopnet')
    ALLOCATE(velList%sigma(3,1), stat=irc)
    CALL alcerr(irc, 'velList%sigma',(/3,1/),'atopnet')
  ENDIF

!--------------------------------------------------------------------------
! Read helmert parameters for coordinates and velocities
! -------------------------------------------------------------------------

  opt%helmSig(:,:) = 0d0

  DO ii=1,2
    IF (ii == 1 .AND. modeCrd /= 3) CYCLE
    IF (ii == 2 .AND. modeVel /= 3) CYCLE

! Translation condition
    hlpKey = "C_TRA"
    IF (ii==2) hlpKey = "V_TRA"
    CALL readkeys(hlpKey,keyValue,irc)
    IF (keyValue(1)=="NO") THEN
      opt%helmSig(ii,1:3) = 0.d0
      IF (ii == 1 .AND. opt%indvSol == 3) opt%ipHelm(1:3) = 0
    ELSE IF (keyValue(1)=="YES") THEN
      opt%helmSig(ii,1:3) = 0.00001d0
    ELSE
      CALL ckoptr(1,hlpKey,keyValue,srName,                                &
                  "Translation condition",irc,ircSum,empty=0.d0,ge=0.d0,   &
                  result1=opt%helmSig(ii,1))
      opt%helmSig(ii,2:3) = opt%helmSig(ii,1)
    END IF

! Rotation condition
    hlpKey = "C_ROT"
    IF (ii==2) hlpKey = "V_ROT"
    CALL readkeys(hlpKey,keyValue,irc)
    IF (keyValue(1)=="NO") THEN
      opt%helmSig(ii,4:6) = 0.d0
      IF (ii == 1 .AND. opt%indvSol == 3) opt%ipHelm(4:6) = 0
    ELSE IF (keyValue(1)=="YES") THEN
      opt%helmSig(ii,4:6) = 0.0001d0
    ELSE
      CALL ckoptr(1,hlpKey,keyValue,srName,                                &
                  "Translation condition",irc,ircSum,empty=0.d0,ge=0.d0,   &
                  result1=opt%helmSig(ii,4))
      opt%helmSig(ii,4:6) = opt%helmSig(ii,4)
    END IF

! Scale condition
    hlpKey = "C_SCL"
    IF (ii==2) hlpKey = "V_SCL"
    CALL readkeys(hlpKey,keyValue,irc)
    IF (keyValue(1)=="NO") THEN
      opt%helmSig(ii,7) = 0.d0
      IF (ii == 1 .AND. opt%indvSol == 3) opt%ipHelm(7) = 0
    ELSE IF (keyValue(1)=="YES") THEN
      opt%helmSig(ii,7) = 0.001d0
    ELSE
      CALL ckoptr(1,hlpKey,keyValue,srName,                                &
                  "Translation condition",irc,ircSum,empty=0.d0,ge=0.d0,   &
                  result1=opt%helmSig(ii,7))
    END IF

! Handle rotation only condition
    IF (opt%helmSig(ii,4)/=0d0 .AND. opt%helmSig(ii,1)==0d0) THEN
      opt%helmSig(ii,1:3) = 1.0d0
    ENDIF

  END DO
  IF (opt%indvSol == 3) opt%indvSol = 2



! ------------------------------------------------------------
! Generate the option structure for ADDNEQ2
! ------------------------------------------------------------

! Get elimination from opt%elimi
! ------------------------------
  oldElmTyp = SIZE(opt%elimi)
  maxElmTyp = oldElmTyp
  IF (modeCrd == 1) maxElmTyp = maxElmTyp+crdList%nSta
  IF (modeVel == 1) maxElmTyp = maxElmTyp+velList%nSta

  IF (maxElmTyp /= oldElmTyp) THEN
    ALLOCATE(oldElimi(oldElmTyp), stat=iac)
    CALL alcerr(iac,'oldElimi', (/oldElmTyp/), 'aoptnet')

    oldElimi = opt%elimi

! Reallocate and store back other requests
! ----------------------------------------
    DEALLOCATE(opt%elimi, stat=iac)

    ALLOCATE(opt%elimi(maxElmTyp), stat=iac)
    CALL alcerr(iac, 'opt%elimi', (/maxElmTyp/), 'aoptnet')

    opt%elimi(1:oldElmTyp) = oldElimi(:)

! Store station deletion requests
! -------------------------------
    nElm = oldElmTyp
    IF (modeCrd == 1) THEN
      DO iSig = 1, crdList%nSta
        nElm = nElm + 1
        opt%elimi(nElm)%name    = crdList%stanam(iSig)
        opt%elimi(nElm)%part    = 1
        opt%elimi(nElm)%mode    = -1
        opt%elimi(nElm)%locq(:) = 0
        opt%elimi(nElm)%locq(1) = 1
        opt%elimi(nElm)%locq(4) = 1
        opt%elimi(nElm)%deltaT  = 0d0
        NULLIFY(opt%elimi(nElm)%excp)
      END DO
    ENDIF

! Store velocity deletion requests
! -------------------------------
    IF (modeVel == 1) THEN
      DO iSig = 1, velList%nSta
        nElm = nElm + 1
        opt%elimi(nElm)%name    = velList%stanam(iSig)
        opt%elimi(nElm)%part    = 1
        opt%elimi(nElm)%mode    = -1
        opt%elimi(nElm)%locq(:) = 0
        opt%elimi(nElm)%locq(1) = 1
        opt%elimi(nElm)%locq(4) = 2
        opt%elimi(nElm)%deltaT  = 0d0
        NULLIFY(opt%elimi(nElm)%excp)
      END DO
    ENDIF
  ENDIF  ! deletion

! Get other sigmas from opt%sigma
! -------------------------------

! count number of non zero sigmas
  nSigCrd = 0
  DO iSig=1,crdList%nSta
    DO ii=1,3
      IF (crdList%sigma(ii,iSig)/=0.d0) nSigCrd = nSigCrd+1
    END DO
  END DO
  nSigVel = 0
  DO iSig=1,velList%nSta
    DO ii=1,3
      IF (velList%sigma(ii,iSig)/=0.d0) nSigVel = nSigVel+1
    END DO
  END DO

  staSigTyp = 0
  IF (modeCrd /= 1) staSigTyp = staSigTyp+nSigCrd
  IF (modeVel /= 1) staSigTyp = staSigTyp+nSigVel


! Store sigmas in opt%sigma
! -------------------------
  ALLOCATE(opt%sigma(staSigTyp),stat=iac)
  CALL alcerr(iac,'opt%sigma',(/staSigTyp/),'aoptnet')
  IF (staSigTyp > 0) THEN

! store previous sigmas
    nSig = 0

! store station sigmas
    IF (modeCrd /= 1) THEN
      DO iSig = 1,crdList%nSta
        DO ii=1,3
          IF (crdList%sigma(ii,iSig)==0.d0) CYCLE
          nSig = nSig+1
          opt%sigma(nSig)%name    = crdList%stanam(iSig)
          opt%sigma(nSig)%value   = crdList%sigma(ii,iSig)
          opt%sigma(nSig)%locq(:) = 0
          opt%sigma(nSig)%locq(1) = freeCrd
          opt%sigma(nSig)%locq(3) = ii
          opt%sigma(nSig)%locq(4) = 1
          opt%sigma(nSig)%typFlg  = 'A'
        END DO
      END DO
    ENDIF

! store velocity sigmas
    IF (modeVel /= 1) THEN
      DO iSig = 1,velList%nSta
        DO ii=1,3
          IF (velList%sigma(ii,iSig)==0.d0) CYCLE
          nSig = nSig+1
          opt%sigma(nSig)%name  = velList%staNam(iSig)
          opt%sigma(nSig)%value = velList%sigma(ii,iSig)
          opt%sigma(nSig)%locq(:) = 0
          opt%sigma(nSig)%locq(1) = freeVel
          opt%sigma(nSig)%locq(3) = ii
          opt%sigma(nSig)%locq(4) = 2
          opt%sigma(nSig)%typFlg  = 'A'
        END DO
      END DO
    ENDIF

  ENDIF  ! Store coord/veloc sigmas

! ---------------------------------------------------------------------
! Write a protocol
! ---------------------------------------------------------------------
  IF (namList%nSta > 0) THEN

! geodetic datum
    WRITE(lfnprt,'(2(A,/))')                  &
      ' Station coordinates and velocities:', &
      ' ----------------------------------'
    CALL pridat(datum%name, datum%aEll, datum%bEll, &
                datum%dxEll, datum%drEll, datum%scEll)

! apriori coordinates
    IF (modeCrd==1 .OR. modeCrd==4) THEN
      CALL prista(namList%nSta,namList%nam(:),namList%num(:),           &
                  xStat,xStell,xStecc,nCentr,iCentr,                    &
                  crdList%nSta,crdNum,0,crdNum,opt%timRefCrd,datum%name)
    ELSE IF (modeCrd == 2) THEN
      CALL prista(namList%nSta,namList%nam(:),namList%num(:),           &
                  xStat,xStell,xStecc,nCentr,iCentr,                    &
                  0,crdNum,0,crdNum,opt%timRefCrd,datum%name)
    ELSE IF (modeCrd == 3) THEN
      CALL prista(namList%nSta,namList%nam(:),namList%num(:),           &
                  xStat,xStell,xStecc,nCentr,iCentr,                    &
                  0,crdNum,crdList%nSta,crdNum,opt%timRefCrd,datum%name)
      DO iCrd = 1, 7
        IF (opt%helmSig(1,iCrd) /= 0) THEN
          nSta         = nSta + 1
          sigSta(nSta) = opt%helmSig(1,iCrd)
          seqSta(nSta) = iCrd
        ENDIF
      ENDDO
    ENDIF

! a priori velocities
    IF (isVelo .AND. (modeVel==1 .OR. modeVel==4)) THEN
      CALL privel(namList%nSta, namList%nam(:), namList%num(:),           &
                  xStell, xVel, velList%nSta, velNum, 0, velNum)
    ELSE IF (isVelo .AND. modeVel == 2) THEN
      CALL privel(namList%nSta, namList%nam(:), namList%num(:),           &
                  xStell, xVel, 0, velNum, 0, velNum)
    ELSE IF (isVelo .AND. modeVel == 3) THEN
      CALL privel(namList%nSta, namList%nam(:), namList%num(:),           &
                  xStell, xVel, 0, velNum, velList%nSta, velNum)
      DO iVel = 1, 7
        IF (opt%helmSig(2,iVel) /= 0) THEN
          nSta         = nSta + 1
          sigSta(nSta) = opt%helmSig(2,iVel)
          seqSta(nSta) = iVel + 7
        ENDIF
      ENDDO
    ENDIF

! a priori constraints
    IF (isVelo) THEN
      IF (modeCrd == 2 .AND. modeVel /= 2) THEN
        CALL prists(crdList%nSta, crdNum, crdList%sigma, &
                    0,            velNum, velList%sigma, &
                    namList%nam(:), namList%num(:), wgtFile, 2)
      ELSEIF (modeCrd /= 2 .AND. modeVel == 2) THEN
        CALL prists(0           , crdNum, crdList%sigma, &
                    velList%nSta, velNum, velList%sigma, &
                    namList%nam(:), namList%num(:), wgtFile, 2)
      ELSEIF (modeCrd == 2 .AND. modeVel == 2) THEN
        CALL prists(crdList%nSta, crdNum, crdList%sigma, &
                    velList%nSta, velNum, velList%sigma, &
                    namList%nam(:), namList%num(:), wgtFile, 2)
      ENDIF
    ELSEIF (modeCrd == 2) THEN
      CALL prists(crdList%nSta, crdNum, crdList%sigma,   &
                  0,            velNum, velList%sigma,   &
                  namList%nam(:), namList%num(:), wgtFile, 2)
    ENDIF

! Network constraints
    IF (nSta > 0) THEN
      WRITE(lfnprt,'(A,/)') ' Network constraints: '
      CALL prisig(1, sigSta, nSta, seqSta)
    ENDIF

! Report relative constraints from STACRUX
! ----------------------------------------
    CALL gtflna(0,'STAINFO',staFil,irc)
    IF (irc == 0 .AND. LEN_TRIM(staFil) > 0) THEN
      CALL readcrux(staFil,stacrux)
      staPrt = .FALSE.

      DO iCooVel = 1,staCrux%nCooVel
        mSta = 0
        DO iSta = 1,namList%nSta
          IF (namList%nam(iSta) == stacrux%coovel(iCooVel)%stanam(1)) &
            mSta = mSta + 1
          IF (namList%nam(iSta) == stacrux%coovel(iCooVel)%stanam(2)) &
            mSta = mSta + 1
        ENDDO
        IF (mSta /= 2) CYCLE
        line = ' '
        DO ii = 1,6
          IF (stacrux%coovel(iCooVel)%constr(ii) == undef_s) CYCLE
          IF (ii > 3 .AND. .NOT. isVelo) EXIT
          i1 = (ii-1)*14+1
          i2 = ii*14
          WRITE(line(i1:i2),'(4X,F10.5)') &
               stacrux%coovel(iCooVel)%constr(ii)
        ENDDO
        IF (LEN_TRIM(line) == 0) CYCLE
        IF (.NOT. staPrt) THEN
          staPrt = .TRUE.
          WRITE(lfnprt,'(A,3(/,A))') &
             ' Relative constraints between stations:',         &
             '                                             ' // &
             '         relative constraints for coordinates' // &
             '      relative constraints for velocities',       &
             ' Station names                               ' // &
             '         N (m)         E (m)         U (m)   ' // &
             '     N (m/year)    E (m/year)    U (m/year)',     &
             ' --------------------------------------------' // &
             '---------------------------------------------' // &
             '--------------------------------------------'
          WRITE(lfnerr,'(/,A,4(/,17X,A),/)') &
             ' ### SR AOPTNET: You are going to use relative ' // &
                                       'constraints for station', &
             'coordinates/velocities from station info file.',    &
             'Please keep in mind that you will NOT constrain the',&
             'estimated results but only the improvements of the',&
             'apriori values.'
        ENDIF
        WRITE(lfnprt,'(1X,2(A,4X,2X),2X,A)') &
             stacrux%coovel(iCooVel)%stanam(:),TRIM(line)
      ENDDO
      IF (staPrt) WRITE(lfnprt,'(/)')
      DEALLOCATE(stacrux%renamsta,stat=irc)
      DEALLOCATE(stacrux%stainfo ,stat=irc)
      DEALLOCATE(stacrux%staprob ,stat=irc)
      DEALLOCATE(stacrux%coovel  ,stat=irc)
      DEALLOCATE(stacrux%statype ,stat=irc)
    ENDIF
  ENDIF

! Deallocate station number arrays
! --------------------------------
  DEALLOCATE(xStat,  stat=irc)
  DEALLOCATE(xStell, stat=irc)
  DEALLOCATE(xStecc, stat=irc)
  DEALLOCATE(iCentr, stat=irc)
  DEALLOCATE(xVel ,  stat=irc)
  DEALLOCATE(velFlg, stat=irc)
  DEALLOCATE(vfound, stat=irc)

  DEALLOCATE(crdNum, stat=irc)
  DEALLOCATE(velNum, stat=irc)

  DEALLOCATE(crdList%stanam, stat=irc)
  DEALLOCATE(crdList%sigma, stat=irc)
  DEALLOCATE(velList%stanam, stat=irc)
  DEALLOCATE(velList%sigma, stat=irc)
  DEALLOCATE(hlpNam, stat=irc)

  IF (ASSOCIATED(oldElimi)) DEALLOCATE(oldElimi,stat=iac)
  DEALLOCATE(keyValue,stat=iac)
  DEALLOCATE(dummy,stat=iac)

  RETURN
END SUBROUTINE aoptnet


END MODULE
