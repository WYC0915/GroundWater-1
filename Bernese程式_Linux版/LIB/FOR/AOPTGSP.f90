MODULE s_AOPTGSP
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE aoptgsp(opt,parGsp,namLst)

! -------------------------------------------------------------------------
! Purpose:    Reads input options for ADDNEQ2:
!             options and constraining for GNSS-specific parameters
!
! Author:     M.Meindl
!
! Created:    08-Dec-2010
!
! Changes:    03-Feb-2011 MM: New datum definition types
!             03-Feb-2011 SL: add line break at the end, m_bern with ONLY
!             27-Mar-2012 RD: Use LISTC1 as module now
!             20-Aug-2012 SL: Shape of listc1 parameter changed
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b,r8b,keyValueLength,fileNameLength,lfnPrt
  USE m_global, ONLY: g_strSys3
  USE p_addneq, ONLY: t_opt,t_sigma,t_parGsp,t_namLst
  USE d_staLst, ONLY: t_staList,init_stalist

  USE s_alcerr
  USE s_readstsg
  USE s_gtflna
  USE s_ckoptc
  USE s_ckoptb
  USE s_ckoptr
  USE s_cordup
  USE s_exitrc
  USE s_readkeys
  USE f_listc1

  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  TYPE(t_parGsp)                         :: parGsp ! GNSS-spec parameters
  TYPE(t_namLst)                         :: namLst ! Station names for GSP

! input/output:
  TYPE(t_opt)                            :: opt    ! Options for ADDNEQ2

! output:

! List of Functions
! -----------------

! Local Types
! -----------
  TYPE(t_sigma)  ,DIMENSION(:),ALLOCATABLE  :: locSig
  TYPE(t_staList)                           :: traLst, trpLst
  TYPE(t_staList)                           :: staLst

! Local Parameters
! ----------------
  CHARACTER(LEN=7),PARAMETER                :: srName = 'aoptgsp'

! Local variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=fileNameLength)                         :: fixTra, fixTrp
  CHARACTER(LEN=57)                                     :: line
  CHARACTER(LEN=10),DIMENSION(2)                        :: xclStr

  INTEGER(i4b)                                          :: maxSigTyp
  INTEGER(i4b)                                          :: oldSigTyp
  INTEGER(i4b)                                          :: iSig, nSig, nSta
  INTEGER(i4b)                                          :: irc, irCode
  INTEGER(i4b)                                          :: iac , ii, jj, iDmy
  INTEGER(i4b),DIMENSION(2)                             :: xcl
  INTEGER(i4b), DIMENSION(:,:), ALLOCATABLE             :: cmpLst
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE               :: idx

  REAL(r8b),    DIMENSION(3)                            :: sigTra
  REAL(r8b),    DIMENSION(1)                            :: sigTrp

  LOGICAL,      DIMENSION(3)                            :: hlpL

! Some initializations
! --------------------
  NULLIFY(keyValue)
!
  CALL init_stalist(traLst)
  CALL init_stalist(trpLst)
  CALL init_stalist(staLst)
  traLst%nSta = -1
  trpLst%nSta = -1
  staLst%nSta =  0
!
  nSig   = 0
  sigTra = 0d0
  sigTrp = 0d0
!
  fixTra = ""
  fixTrp = ""
!
  irCode      = 0
!
  xclStr(1) = "(included)"
  xclStr(2) = "(excluded)"

! Nothing to do at all
! --------------------
  IF (parGsp%nTra == 0 .AND. parGsp%nTrp == 0) RETURN

! Get sigmas from opt%sigma
! -------------------------
  oldSigTyp = SIZE(opt%sigma)
  maxSigTyp = oldSigTyp + parGsp%nTra + parGsp%nTrp

! Allocate local sigma structure
  ALLOCATE(locSig(maxSigTyp),stat=iac)
  CALL alcerr(iac,'locSig',(/maxSigTyp/),srName)

! Initialize local sigma strucutre
  DO iSig=1,maxSigTyp
    locSig(iSig)%locq(:) = 0
  ENDDO
  locSig(:)%value     = 0.d0
  locSig(:)%name      = ''
  locSig(:)%typFlg    = 'A'
  locSig(1:oldSigTyp) = opt%sigma(:)

! Station translations
! --------------------
  IF (parGsp%nTra > 0) THEN

! Type of datum definition
    CALL readKeys("GTRADAT",keyValue,irc)
    CALL ckoptc(1,"GTRADAT",keyValue,                                       &
                (/"NONE     ","ABSOLUTE ","MIN_CONST"/),srName,             &
                "GNSS-spec sta translation: type of constraint",irc,irCode, &
                valList=(/0,1,2/),result1=opt%gTraDat)

! Get station list from file
    IF (opt%gTraDat /= 0) THEN
      CALL gtflna(0,'GTRAFIX',fixTra,irc)
      IF (irc==0 .AND. LEN_TRIM(fixTra)>0) THEN
        CALL readKeys("GTRAXCL",keyValue,irc)
        CALL ckoptc(1,"GTRAXCL",keyValue,(/"INCLUDED","EXCLUDED"/),srName, &
                    "GNSS-spec sta translation: In-/Exclusion",irc,irCode, &
                    valList=(/1,2/),result1=xcl(1))
        CALL readstsg(fixTra,0,traLst)
      ENDIF
    ENDIF

! Get sigmas
    IF (opt%gTraDat == 1) THEN
      CALL readKeys("GTRASIG_N",keyValue,irc)
      CALL ckoptr(1,"GTRASIG_N",keyValue,srName,                             &
                  'GNSS-specific station translations: sigma N',irc,irCode, &
                  empty=0d0,ge=0d0,result1=sigTra(1))
      CALL readKeys("GTRASIG_E",keyValue,irc)
      CALL ckoptr(1,"GTRASIG_E",keyValue,srName,                             &
                  'GNSS-specific station translations: sigma E',irc,irCode, &
                  empty=0d0,ge=0d0,result1=sigTra(2))
      CALL readKeys("GTRASIG_U",keyValue,irc)
      CALL ckoptr(1,"GTRASIG_U",keyValue,srName,                             &
                  'GNSS-specific station translations: sigma U',irc,irCode, &
                  empty=0d0,ge=0d0,result1=sigTra(3))
    ENDIF

! Get minimum constraint conditions
    IF (opt%gTraDat == 2) THEN
      CALL ckoptb(1,(/"GTRATRA"/),srName,'GNSS-spec sta tra: translation', &
                  irCode,resultL=hlpL(1))
      IF (hlpL(1)) opt%helmSig(3,1:3) = 1.d-5
      CALL ckoptb(1,(/"GTRAROT"/),srName,'GNSS-spec sta tra: rotation', &
                  irCode,resultL=hlpL(2))
      IF (hlpL(2)) opt%helmSig(3,4:6) = 1.d-4
      CALL ckoptb(1,(/"GTRASCL"/),srName,'GNSS-spec sta tra: scale', &
                  irCode,resultL=hlpL(3))
      IF (hlpL(3)) opt%helmSig(3,7) = 1.d-3

      ! Handle rotation-only condition
      IF (opt%helmSig(3,4) /= 0.d0 .AND. opt%helmSig(3,1) == 0.d0) &
                                               opt%helmSig(3,1:3) = 1.d0
    ENDIF

! Create sigma list: Absolute sigmas
    IF (opt%gTraDat == 1) THEN
      IF (traLst%nSta == -1) THEN
        DO ii=1,3
          IF (sigTra(ii) <= 0.d0) CYCLE
          nSig = nSig+1
          iSig = nSig+oldSigTyp
          locSig(iSig)%locq(1) = 30
          locSig(iSig)%locq(3) = ii
          locSig(iSig)%value   = sigTra(ii)
          locSig(iSig)%typFlg  = 'A'
        ENDDO
      ELSE
        nSta = traLst%nSta
        DO jj=1,namLst%nSta
          iDmy = listc1(0,16,nSta,traLst%staNam,namLst%nam(jj),nSta)
          IF (xcl(1) == 1 .AND. iDmy == 0) CYCLE
          IF (xcl(1) == 2 .AND. iDmy /= 0) CYCLE
          DO ii=1,3
            IF (sigTra(ii) <= 0.d0) CYCLE
            nSig = nSig+1
            iSig = nSig+oldSigTyp
            locSig(iSig)%locq(1) = 30
            locSig(iSig)%locq(3) = ii
            locSig(iSig)%value   = sigTra(ii)
            locSig(iSig)%name    = namLst%nam(jj)
            locSig(iSig)%typFlg  = 'A'
          ENDDO
        ENDDO
      ENDIF
    ENDIF

! Create sigma list: Minimum constraint solution
    IF (opt%gTraDat == 2) THEN
      IF (traLst%nSta == -1) THEN
        DO ii=1,3
          nSig = nSig+1
          iSig = nSig+oldSigTyp
          locSig(iSig)%locq(1) = -30
          locSig(iSig)%locq(3) = ii
          locSig(iSig)%value   = 1.d0
          locSig(iSig)%typFlg  = 'A'
        ENDDO
      ELSE
        nSta = traLst%nSta
        DO jj=1,namLst%nSta
          iDmy = listc1(0,16,nSta,traLst%staNam,namLst%nam(jj),nSta)
          IF (xcl(1) == 1 .AND. iDmy == 0) CYCLE
          IF (xcl(1) == 2 .AND. iDmy /= 0) CYCLE
          DO ii=1,3
            nSig = nSig+1
            iSig = nSig+oldSigTyp
            locSig(iSig)%locq(1) = -30
            locSig(iSig)%locq(3) = ii
            locSig(iSig)%name    = namLst%nam(jj)
            locSig(iSig)%value   = 1.d0
            locSig(iSig)%typFlg  = 'A'
          ENDDO
        ENDDO
      ENDIF
    ENDIF
  ENDIF

! Troposphere biases
! ------------------
  IF (parGsp%nTrp > 0) THEN

! Type of datum definition
    CALL readKeys("GTRPDAT",keyValue,irc)
    CALL ckoptc(1,"GTRPDAT",keyValue,                                   &
                (/"NONE    ","SUM     ","ABSOLUTE"/),srName, &
                "GNSS-spec trop biases: type of constraint",irc,irCode, &
                valList=(/0,1,2/),result1=opt%gTrpDat)

! Get station list from file
    IF (opt%gTrpDat == 1 .OR. opt%gTrpDat == 2) THEN
      CALL gtflna(0,'GTRPFIX',fixTrp,irc)
      IF (irc==0 .AND. LEN_TRIM(fixTrp)>0) THEN
        CALL readKeys("GTRPXCL",keyValue,irc)
        CALL ckoptc(1,"GTRPXCL",keyValue,(/"INCLUDED","EXCLUDED"/),srName, &
                    "GNSS-specific trop bias: In-/Exclusion",irc,irCode,   &
                    valList=(/1,2/),result1=xcl(2))
        CALL readstsg(fixTrp,0,trpLst)
      ENDIF
    ENDIF

! Get sigmas
    IF (opt%gTrpDat == 2) THEN
      CALL readKeys("GTRPSIG",keyValue,irc)
      CALL ckoptr(1,"GTRPSIG",keyValue,srName,                        &
                  'GNSS-specific troposphere bias: sigma',irc,irCode, &
                  empty=0d0,ge=0d0,result1=sigTrp(1))
    ELSEIF (opt%gTrpDat == 1) THEN
      sigTrp(1) = 1d-6
    ENDIF

! Save local sigmas
    IF (opt%gTrpDat /= 0 .AND. sigTrp(1) > 0.d0) THEN
      IF (trpLst%nSta == -1) THEN
        nSig = nSig+1
        iSig = nSig+oldSigTyp
        locSig(iSig)%locq(1) = 30
        locSig(iSig)%locq(3) = 4
        locSig(iSig)%value   = sigTrp(1)
        IF (opt%gTrpDat == 1) locSig(iSig)%typFlg = 'S'
      ELSE
        nSta = trpLst%nSta
        DO jj=1,namLst%nSta
          iDmy = listc1(0,16,nSta,trpLst%staNam,namLst%nam(jj),nSta)
          IF (xcl(2) == 1 .AND. iDmy == 0) CYCLE
          IF (xcl(2) == 2 .AND. iDmy /= 0) CYCLE
          nSig = nSig+1
          iSig = nSig+oldSigTyp
          locSig(iSig)%locq(1) = 30
          locSig(iSig)%locq(3) = 4
          locSig(iSig)%value   = sigTrp(1)
          locSig(iSig)%name    = namLst%nam(jj)
          IF (opt%gTrpDat == 1) locSig(iSig)%typFlg = 'S'
        ENDDO
      ENDIF
    ENDIF
  ENDIF

! Compile station overview
! ------------------------
  ALLOCATE(staLst%staNam(namLst%nSta),STAT=iac)
  CALL alcErr(iac,'stalst%stanam',(/namLst%nSta/),srName)
  staLst%nSta = 0

  ALLOCATE(cmpLst(namLst%nSta,2),STAT=iac)
  CALL alcErr(iac,'cmplst',(/namLst%nSta,2/),srName)
  cmpLst = 0

  ALLOCATE(idx(namLst%nSta),STAT=iac)
  CALL alcErr(iac,'idx',(/namLst%nSta/),srName)
  idx = 0

  DO ii=1,maxSigTyp
    IF (ABS(locSig(ii)%locq(1)) /= 30) CYCLE
    IF (TRIM(locSig(ii)%name) /= "") THEN
      iDmy = listc1(1,16,namLst%nSta,staLst%staNam,locSig(ii)%name,staLst%nSta)
      IF (locSig(ii)%locq(3) == 1) cmpLst(iDmy,1) = 1
      IF (locSig(ii)%locq(3) == 4) cmpLst(iDmy,2) = 1
    ELSE
      DO jj=1,namLst%nSta
        iDmy = listc1(1,16,namLst%nSta,staLst%staNam, &
                      namLst%nam(jj),staLst%nSta)
        IF (locSig(ii)%locq(3) == 1) cmpLst(iDmy,1) = 1
        IF (locSig(ii)%locq(3) == 4) cmpLst(iDmy,2) = 1
      ENDDO
    ENDIF
  ENDDO

! Sort list
  CALL cordup(staLst%staNam,staLst%nSta,1,16,idx)

! Print parameter information
! ---------------------------
  WRITE(lfnPrt,'(2(A,/))')         &
    ' GNSS-specific parameters:',  &
    ' ------------------------'

! General information
  IF (parGsp%nTra > 0) THEN
    WRITE(lfnPrt,'(A)') " GNSS-specific station translations"
    WRITE(lfnPrt,'(A,A)') ' Satellite systems   : ', &
         (g_strSys3(parGsp%sysTra(ii)),ii=1,parGsp%nSysTra)
    IF (opt%gTraDat == 2) THEN
      WRITE(lfnPrt,'(A)') ' Datum definition    : minimum constraint conditions'
    ELSEIF (opt%gTraDat == 1) THEN
      WRITE(lfnPrt,'(A)') ' Datum definition    : absolute constraining'
    ELSE
      WRITE(lfnPrt,'(A)') ' Datum definition    : none'
    ENDIF
    IF (fixTra /= "") &
     WRITE(lfnPrt,'(4A)') ' Stations ',xclStr(xcl(1)),' : ',TRIM(fixTra)
   ENDIF

  IF (parGsp%nTrp > 0) THEN
    WRITE(lfnPrt,'(/,A)') " GNSS-specific troposphere biases"
    WRITE(lfnPrt,'(A,A)') ' Satellite systems   : ', &
         (g_strSys3(parGsp%sysTrp(ii)),ii=1,parGsp%nSysTrp)
    IF (opt%gTrpDat == 1) THEN
      WRITE(lfnPrt,'(A)') ' Datum definition    : condition of sum'
    ELSEIF (opt%gTrpDat == 2) THEN
      WRITE(lfnPrt,'(A)') ' Datum definition    : absolute constraining'
    ELSE
      WRITE(lfnPrt,'(A)') ' Datum definition    : none'
    ENDIF
    IF (fixTrp /= "") &
     WRITE(lfnPrt,'(4A)') ' Stations ',xclStr(xcl(2)),' : ',TRIM(fixTrp)
    WRITE(lfnPrt,'(/)')
  ENDIF

! List of stations
  IF (staLst%nSta > 0) THEN
    WRITE(lfnPrt,'(/,1X,56("-"),/,A,/,1X,56("-"))') &
      " Station name          N (m)    E (m)    U (m)    Abs (m)"
    DO jj=1,staLst%nSta
      line = ""
      ii = idx(jj)
      WRITE(line,'(1X,A16,4X)') staLst%staNam(ii)
      IF (cmpLst(ii,1) == 1) THEN
        IF (opt%gTraDat == 1) WRITE(line(22:48),'(3(2X,F7.5))') sigTra
        IF (opt%gTraDat == 2) THEN
          WRITE(line(22:32),'(2X,A9)') "MIN_CONST"
          IF (hlpL(1)) WRITE(line(35:37),'(A3)') "Tra"
          IF (hlpL(2)) WRITE(line(40:42),'(A3)') "Rot"
          IF (hlpL(3)) WRITE(line(45:47),'(A3)') "Scl"
        ENDIF
      ENDIF
      IF (cmpLst(ii,2) == 1) THEN
        IF (opt%gTrpDat == 1) WRITE(line(49:53),'(2X,A3)') "SUM"
        IF (opt%gTrpDat == 2) WRITE(line(49:57),'(2X,F7.5)') sigTrp(1)
      ENDIF
      WRITE(lfnPrt,'(A)') line
    ENDDO
    WRITE(lfnPrt,'(/)')
  ENDIF

! Stop the program if an input error found
! ----------------------------------------
  IF (irCode /= 0) CALL exitrc(2)

! Store sigmas in opt%sigma
! -------------------------
  DEALLOCATE(opt%sigma,stat=iac)
  ALLOCATE(opt%sigma(nSig+oldSigTyp),stat=iac)
  CALL alcerr(iac,'opt%sigma',(/nSig+oldSigTyp/),srName)

  nSig = 0
  DO iSig=1,maxSigTyp
    IF (locSig(iSig)%value /= 0d0) THEN
      nSig            = nSig+1
      opt%sigma(nSig) = locSig(iSig)
    ENDIF
  ENDDO

! Deallocate the special request definition
! -----------------------------------------
  DEALLOCATE(locSig,stat=iac)
  DEALLOCATE(keyValue,stat=iac)
  DEALLOCATE(cmpLst,stat=iac)
  DEALLOCATE(idx,stat=iac)
  DEALLOCATE(staLst%staNam,stat=iac)

  RETURN

END SUBROUTINE aoptgsp

END MODULE s_AOPTGSP
