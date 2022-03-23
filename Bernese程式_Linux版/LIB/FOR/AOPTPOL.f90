MODULE s_AOPTPOL
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE aoptpol(opt, parErp)

! -------------------------------------------------------------------------
! Purpose:    Reads input options for ADDNEQ2:
!             options and constraining for ERPs
!
! Author:     M. Meindl
!
! Created:    10-Jul-2001
!
! Changes:    21-Dec-2001 HU: m_addneq replaced by p_addneq
!             08-Feb-2002 MM: Use t_sigma for local sigmas
!                             Enable relative sigmas
!             10-Dec-2002 CU: Print a priori sigmas of ERP using SR prisig
!             19-Mar-2003 RD: Correct SIZE statement (because of IFC)
!             04-Apr-2003 RD: opt%polestep in sec now
!             23-Apr-2003 CU: Nullify local pointers
!             27-May-2003 CU: Print a priori information: block retro.terms
!             11-Dec-2003 MM: Xp, Yp combined
!             19-Feb-2004 HU: Activate BLKRET only if ERPs in NEQs
!             19-May-2009 DT: Apply "UT others" only for UT parameters;
!                             Add LOD constraint (erpSigTyp 8->9)
!             19-Sep-2012 RD: Use M_BERN with ONLY
!             16-Oct-2012 SS: opt%blkRet with user-defined sigma value
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnprt, lfnerr, &
                      keyValueLength, fileNameLength
  USE p_addneq, ONLY: t_opt, t_sigma, t_parErp
  USE s_ckoptt
  USE s_ckoptr
  USE s_alcerr
  USE s_prisig
  USE s_readkeys
  USE s_exitrc
  USE s_gtflna
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  Type(t_parErp)               :: parErp         ! ERP input options

! input/output:

! output:
  TYPE(t_opt)                  :: opt            ! Options for ADDNEQ2

! List of functions
! -----------------

! Local types
! -----------
  TYPE(t_sigma),DIMENSION(:),ALLOCATABLE                :: locSig

! Local parameters
! ----------------
  CHARACTER(LEN=7), PARAMETER                           :: srName = 'aoptpol'

  INTEGER(i4b), PARAMETER                               :: erpSigTyp = 9

! Local variables
! ---------------
  INTEGER(i4b)                                          :: maxSigTyp
  INTEGER(i4b)                                          :: oldSigTyp
  INTEGER(i4b)                                          :: help
  INTEGER(i4b)                                          :: nSig, iSig
  INTEGER(i4b)                                          :: irCode
  INTEGER(i4b)                                          :: irc, iac, ios
  INTEGER(i4b) , DIMENSION(erpSigTyp)                   :: seqErp
  INTEGER(i4b)                                          :: iErp, nErp

  REAL(r8b)                                             :: rHlp
  REAL(r8b)    , DIMENSION(erpSigTyp)                   :: sigErp

  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=fileNameLength)                         :: polFil


  NULLIFY(keyValue)

! Get other sigmas from opt%sigma
! -------------------------------
  oldSigTyp = SIZE(opt%sigma)
  maxSigTyp = oldSigTyp + erpSigTyp

  ALLOCATE(locSig(maxSigTyp),stat=iac)
  CALL alcerr(iac,'locSig',(/maxSigTyp/),'aoptpol')

! Initialize local sigma
  DO iSig = 1,maxSigTyp
    locSig(iSig)%locq(:) = 0
  ENDDO
  locSig(:)%value     = 0.D0
  locSig(:)%name      = ''
  locSig(:)%typFlg    = 'A'
  locSig(1:oldSigTyp) = opt%sigma(:)

! Read options
! ------------
  opt%PoleStep = 0
  opt%blkRet   = 0.D0

! Time resolution for storing pole values
! ---------------------------------------
  opt%PoleStep = 0

  CALL gtflna(0,'POLERS',polFil,irc)
  IF (irc /= 0 .OR. LEN_TRIM(polFil) == 0) &
    CALL gtflna(0,'IERSPOL',polFil,irc)

  irCode = 0
  IF (LEN_TRIM(polFil) > 0 .AND. irc == 0) THEN
    CALL readkeys('POLESTEP',keyValue,irc)
    CALL ckoptt(1,'POLESTEP',keyValue,srName, &
                'Time resolution of extracted ERP values',irc,irCode, &
                maxVal=1,gt=0d0,result1=rHlp)
    IF (irCode /= 0) CALL exitrc(2)
    opt%PoleStep = NINT(rHlp*3600d0)
  ENDIF

  CALL readkeys('BLKRET', keyValue, irc)
!!  IF (irc == 0 .AND. keyValue(1) == '1' .AND. parErp%nErp > 0) opt%blkRet = 1
  CALL readkeys('BLKRET', keyValue, irc)
  IF (keyValue(1) == 'NO' .OR. parErp%nErp == 0) THEN
    opt%blkRet = 0.D0
  ELSEIF (keyValue(1) == 'YES') THEN
    opt%blkRet = 1.D-6
  ELSE
    CALL ckoptr(1,'BLKRET',keyValue,srName, &
                'Blocking of retrograde terms',irc,irCode,empty=0.d0,ge=0.d0, &
                result1=opt%blkRet)
    IF (irCode /= 0) CALL exitrc(2)
  ENDIF

! Constraining of ERP parameters
! ------------------------------
  nSig = 0

! loop over all sigmas
  DO iSig = oldSigTyp+1,maxSigTyp
    help = iSig-oldSigTyp
    locSig(iSig)%locq(1) = 10
    SELECT CASE (help)
      CASE (1)
        CALL readkeys('POL_XY'  , keyValue,irc)
        locSig(iSig)%locq(4) = 1
        locSig(iSig)%locq(5) = 1
      CASE (2)
        CALL readkeys('POL_XY'  , keyValue,irc)
        locSig(iSig)%locq(4) = 2
        locSig(iSig)%locq(5) = 1
      CASE (3)
        CALL readkeys('POL_UTF', keyValue,irc)
        locSig(iSig)%locq(3) = 1
        locSig(iSig)%locq(4) = 3
        locSig(iSig)%locq(5) = 1
      CASE (4)
        CALL readkeys('POL_UTN', keyValue,irc)
        locSig(iSig)%locq(4) = 3
!!! only for UT:
        locSig(iSig)%locq(5) = 1
!!!
! LOD
      CASE (5)
        CALL readkeys('POL_LOD', keyValue,irc)
        locSig(iSig)%locq(4) = 3
        locSig(iSig)%locq(5) = 2

      CASE (6)
        CALL readkeys('POL_EPF', keyValue,irc)
        locSig(iSig)%locq(3) = 1
        locSig(iSig)%locq(4) = 4
      CASE (7)
        CALL readkeys('POL_EPN', keyValue,irc)
        locSig(iSig)%locq(4) = 4
      CASE (8)
        CALL readkeys('POL_PSF', keyValue,irc)
        locSig(iSig)%locq(3) = 1
        locSig(iSig)%locq(4) = 5
      CASE (9)
        CALL readkeys('POL_PSN', keyValue,irc)
        locSig(iSig)%locq(4) = 5
      CASE DEFAULT
        CYCLE
    ENDSELECT

! Read sigmas (empty means 0.0)
    IF (TRIM(keyValue(1)) == '') keyValue(1) = '0.0'
    READ(keyValue(1),*,iostat=ios) locSig(iSig)%value

    IF (irc /= 0 .OR. ios /= 0 .OR. locSig(iSig)%value < 0.D0) THEN
      WRITE(lfnerr,'(/,A,/,17X,A,A,/)')                                   &
      ' *** SR AOPTPOL: Wrong sigma for earth rotation parameters.',      &
                       'Specified Value: ',TRIM(keyValue(1))
      CALL exitrc(2)
    ENDIF

    IF (locSig(iSig)%value /= 0.D0)  nSig = nSig+1

! Warning if UTF, EPF and PSF are zero
    IF ((help == 3 .OR. help == 6 .OR. help == 8) .AND.                   &
         locSig(iSig)%value == 0.D0) THEN
      WRITE(lfnerr,'(/,A,/)')                                             &
      ' ### SR AOPTPOL: UT_first, EPS_first and PSI_first may not be 0.0'
    ENDIF
  ENDDO

! create sigma array "sigErp" for printing a priori sigmas into protocol
  IF (parErp%nErp > 0) THEN
    nErp = 0
    DO iErp = 1, parErp%nErp
      IF (parErp%erp(iErp) < 3) THEN
        nErp = nErp + 1
        sigErp(nErp) = locSig(oldSigTyp+parErp%erp(iErp))%value
        seqErp(nErp) = parErp%erp(iErp)
      ELSEIF (parErp%erp(iErp) == 3) THEN
        nErp = nErp + 2
        sigErp(nErp-1) = locSig(oldSigTyp+3)%value
        sigErp(nErp)   = locSig(oldSigTyp+4)%value
        seqErp(nErp-1) = 3
        seqErp(nErp)   = 4

       ! LOD
       !----
        nErp = nErp + 1
        sigErp(nErp) = locSig(oldSigTyp+5)%value
        seqErp(nErp) = 5

      ELSEIF (parErp%erp(iErp) == 4) THEN
        nErp = nErp + 2
        sigErp(nErp-1) = locSig(oldSigTyp+6)%value
        sigErp(nErp)   = locSig(oldSigTyp+7)%value
        seqErp(nErp-1) = 6
        seqErp(nErp)   = 7
      ELSEIF (parErp%erp(iErp) == 5) THEN
        nErp = nErp + 2
        sigErp(nErp-1) = locSig(oldSigTyp+8)%value
        sigErp(nErp  ) = locSig(oldSigTyp+9)%value
        seqErp(nErp-1) = 8
        seqErp(nErp)   = 9
      ELSE
        WRITE(lfnerr,'(A,I4)') &
          ' *** SR AOPTPOL: unexpected earth rotation parameter: ', &
          parErp%erp(iErp)
        CALL exitrc(2)
      ENDIF
    ENDDO

! Print a priopri sigma for earth rotation parameters
! ---------------------------------------------------
    WRITE(lfnprt,'(2(A,/))')         &
      ' Earth rotation parameters:', &
      ' -------------------------'

    IF (opt%blkRet > 0.D0)                                                    &
      WRITE(lfnprt,'(A,F11.9,A,/)')                                           &
        ' Blocking of the retrograde terms in X and Y polar wobble series: ', &
        opt%blkRet,' mas'

    CALL prisig(10, sigErp, nErp, seqErp)

  ENDIF

! Store sigmas in opt%sigma
! -------------------------
  DEALLOCATE(opt%sigma,stat=iac)
  ALLOCATE(opt%sigma(nSig+oldSigTyp),stat=iac)
  CALL alcerr(iac,'opt%sigma',(/nSig+oldSigTyp/),'aoptpol')

  nSig = 0
  DO iSig = 1,maxSigTyp
    IF (locSig(iSig)%value /= 0.D0) THEN
      nSig = nSig+1
      opt%sigma(nSig)=locSig(iSig)
    ENDIF
  ENDDO

! Clean up
! --------
  DEALLOCATE(locSig)
  DEALLOCATE(keyValue,stat=iac)

  RETURN
END SUBROUTINE aoptpol

END MODULE
