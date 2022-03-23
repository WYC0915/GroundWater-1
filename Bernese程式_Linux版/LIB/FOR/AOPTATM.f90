MODULE s_AOPTATM
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE aoptatm(opt, parAtm)

! -------------------------------------------------------------------------
! Purpose:    Reads input options for ADDNEQ2:
!             constraining of troposphere parameters
!
! Author:     M. Meindl
!
! Created:    10-Jul-2001
!
! Changes:    05-Sep-2001 HU: Interface for alcerr added
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             08-Feb-2002 MM: Use t_sigma for local sigmas
!                             Enable relative sigmas
!             13-Feb-2002 MM: Enable GIM parameters and gradients
!             04-Nov-2002 HB: New parameter parAtm, write a priori
!                              information in output file
!             10-Dec-2002 CU: Use SR prisig to print a priori sigmas of
!                             tropo. and iono. parameters,
!                             save old opt%sigma values due to changing
!                             order of SR call in SR addrdopt
!             19-Mar-2003 RD: Correct SIZE statement (because of IFC)
!             23-Apr-2003 CU: Nullify local pointers
!             21-Sep-2004 CU: Order of sigTro corrected for SR prisig
!             24-Aug-2006 AG: GMF and TAN(z) implemented
!             30-Jun-2008 RD: VMF added
!             04-Jan-2010 SL: HOI scaling parameters added
!             08-Oct-2010 RD: Extension of parAtm
!             04-Jan-2011 PS: Chen/Herring gradient mapping
!             19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfnprt, &
                      keynameLength, keyValueLength
  USE p_addneq, ONLY: t_opt, t_sigma, t_parAtm
  USE s_alcerr
  USE s_prisig
  USE s_readkeys
  USE s_ckoptr
  USE s_ckoptb
  USE s_exitrc
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  TYPE(t_parAtm)               :: parAtm  ! Atmosphere parameter

! input/output:

! output:
  TYPE(t_opt)                  :: opt     ! Options for ADDNEQ2

! Local types
! -----------
  TYPE(t_sigma), DIMENSION(:), ALLOCATABLE              :: locSig

! Local parameters
! ----------------
  CHARACTER(LEN=7),PARAMETER                            :: srName = 'aoptatm'
  INTEGER(i4b), PARAMETER                               :: nGim = 2
  INTEGER(i4b), PARAMETER                               :: nHoi = 3
  INTEGER(i4b), PARAMETER                               :: atmSigTyp = 11

! Local variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=keyNameLength),DIMENSION(3),PARAMETER   :: hoiKeyw = &
    (/'HOI1','HOI2','HOI3'/)
  CHARACTER(LEN=keyNameLength),DIMENSION(3),PARAMETER   :: hoiKeyw2 = &
    (/'STKHOI1','STKHOI2','STKHOI3'/)
  CHARACTER(LEN=132)                                    :: text

  INTEGER(i4b)                                          :: maxSigTyp
  INTEGER(i4b)                                          :: oldSigTyp
  INTEGER(i4b)                                          :: nSig, iSig
  INTEGER(i4b)                                          :: irCode
  INTEGER(i4b)                                          :: irc, iac, ios
  INTEGER(i4b),  DIMENSION(2)                           :: iTrGrd
  INTEGER(i4b),  DIMENSION(6)                           :: seqTro
  INTEGER(i4b),  DIMENSION(nGim)                        :: seqGim
  INTEGER(i4b),  DIMENSION(nHoi)                        :: seqHoi
  INTEGER(i4b)                                          :: iTrp
  INTEGER(i4b)                                          :: iTro, nTro
  INTEGER(i4b)                                          :: iGim
  INTEGER(i4b)                                          :: iHoi

  REAL(r8b),     DIMENSION(6)                           :: sigTro
  REAL(r8b),     DIMENSION(nGim)                        :: sigGim
  REAL(r8b),     DIMENSION(nHoi)                        :: sigHoi


  irCode = 0
  NULLIFY(keyValue)

! Get other sigmas from opt%sigma
! -------------------------------
  oldSigTyp = SIZE(opt%sigma)
  maxSigTyp = oldSigTyp + atmSigTyp

  ALLOCATE(locSig(maxSigTyp),stat=iac)
  CALL alcerr(iac,'locSig',(/maxSigTyp/),'aoptatm')

! Initialize local sigma
  DO iSig = 1,maxSigTyp
    locSig(iSig)%locq(:) = 0
  ENDDO
  locSig(:)%value     = 0.D0
  locSig(:)%name      = ''
  locSig(:)%typFlg    = 'A'
  locSig(1:oldSigTyp) = opt%sigma(:)

! Constraining of atmosphere parameters
! -------------------------------------
  nSig = 0

! Loop over all sigmas
  DO iSig = 1, atmSigTyp
    SELECT CASE (iSig)
      CASE (1)                                    ! Tropo zenith absolute
        CALL readkeys('TRP_ASIG',keyValue,irc)
        locSig(oldSigTyp+iSig)%locq(1) = 6
        locSig(oldSigTyp+iSig)%locq(4) = 3
        locSig(oldSigTyp+iSig)%typFlg = 'A'
      CASE (2)                                    ! Tropo zenith relative
        CALL readkeys('TRP_RSIG',keyValue,irc)
        locSig(oldSigTyp+iSig)%locq(1) = 6
        locSig(oldSigTyp+iSig)%locq(4) = 3
        locSig(oldSigTyp+iSig)%typFlg = 'R'
      CASE (3)                                    ! Gradient NS absolute
        CALL readkeys('GRD_ASIG',keyValue,irc)
        locSig(oldSigTyp+iSig)%locq(1) = 6
        locSig(oldSigTyp+iSig)%locq(4) = 1
        locSig(oldSigTyp+iSig)%typFlg = 'A'
      CASE (4)                                    ! Gradient NS relative
        CALL readkeys('GRD_RSIG',keyValue,irc)
        locSig(oldSigTyp+iSig)%locq(1) = 6
        locSig(oldSigTyp+iSig)%locq(4) = 1
        locSig(oldSigTyp+iSig)%typFlg = 'R'
      CASE (5)                                    ! Gradient EW absolute
        CALL readkeys('GRD_ASIG',keyValue,irc)
        locSig(oldSigTyp+iSig)%locq(1) = 6
        locSig(oldSigTyp+iSig)%locq(4) = 2
        locSig(oldSigTyp+iSig)%typFlg = 'A'
      CASE (6)                                    ! Gradient EW relative
        CALL readkeys('GRD_RSIG',keyValue,irc)
        locSig(oldSigTyp+iSig)%locq(1) = 6
        locSig(oldSigTyp+iSig)%locq(4) = 2
        locSig(oldSigTyp+iSig)%typFlg = 'R'
      CASE (7)                                    ! GIM absolute
        CALL readkeys('GIM_ASIG',keyValue,irc)
        locSig(oldSigTyp+iSig)%locq(1) = 19
        locSig(oldSigTyp+iSig)%typFlg = 'A'
      CASE (8)                                    ! GIM relative
        CALL readkeys('GIM_RSIG',keyValue,irc)
        locSig(oldSigTyp+iSig)%locq(1) = 19
        locSig(oldSigTyp+iSig)%typFlg = 'R'
      CASE (9)                                    ! second-order HOI term
        CALL readkeys('SIGHOI1',keyValue,irc)
        locSig(oldSigTyp+iSig)%locq(1) = 27
        locSig(oldSigTyp+iSig)%locq(2) = 1
        locSig(oldSigTyp+iSig)%typFlg = 'A'
      CASE (10)                                   ! third-order HOI term
        CALL readkeys('SIGHOI2',keyValue,irc)
        locSig(oldSigTyp+iSig)%locq(1) = 27
        locSig(oldSigTyp+iSig)%locq(2) = 2
        locSig(oldSigTyp+iSig)%typFlg = 'A'
      CASE (11)                                   ! ray path bending term
        CALL readkeys('SIGHOI3',keyValue,irc)
        locSig(oldSigTyp+iSig)%locq(1) = 27
        locSig(oldSigTyp+iSig)%locq(2) = 3
        locSig(oldSigTyp+iSig)%typFlg = 'A'
      CASE DEFAULT
        CYCLE
    ENDSELECT

! Read the sigmas (empty means 0.0)
    IF (TRIM(keyValue(1)) == '') keyValue(1) = '0.0'

    READ(keyValue(1),*,iostat=ios) locSig(oldSigTyp+iSig)%value

    IF (irc /= 0 .OR. ios /= 0 .OR. locSig(oldSigTyp+iSig)%value < 0.D0) THEN
      WRITE(lfnerr,'(/,A,/,17X,A,A,/)')                                   &
      ' *** SR AOPTATM: Wrong sigma for troposphere constraining.',       &
                       'Specified Value: ',TRIM(keyValue(1))
      CALL exitrc(2)
    ENDIF

    IF (locSig(oldSigTyp+iSig)%value /= 0.D0) THEN
      nSig = nSig+1
    ENDIF
  ENDDO

! Print a priori information of troposphere parameters
! ----------------------------------------------------
  IF (parAtm%nTro /= 0) THEN
    DO iTrp = 1, parAtm%nModel
      iTrGrd(1) = parAtm%trpMod(iTrp)%iTrGrd
      iTrGrd(2) = 1
      WRITE(lfnPrt,'(A,7X,A,I5,A,I5,/,A,/)')                       &
           ' Site-specific troposphere parameters:',               &
           ' used in NEQ-files: ',parAtm%trpMod(iTrp)%fromto(1),   &
           ' to ',parAtm%trpMod(iTrp)%fromto(2),                   &
           ' ------------------------------------'
! print troposphere model
      text = ' A priori troposphere model: '
      IF (parAtm%trpMod(iTrp)%iTropo == 0) THEN
        text(1:35) = ' No troposphere corrections applied'
      ELSEIF (parAtm%trpMod(iTrp)%iTropo == 1) THEN
        text(46:58) = ' Saastamoinen'
      ELSEIF(parAtm%trpMod(iTrp)%iTropo == 2) THEN
        text(46:64) = ' Hopfield (Remondi)'
      ELSEIF(parAtm%trpMod(iTrp)%iTropo == 3) THEN
        text(46:60) = ' Essen + Froome'
      ELSEIF(parAtm%trpMod(iTrp)%iTropo == 4) THEN
        text(46:59) = ' Marini Murray'
      ELSEIF(parAtm%trpMod(iTrp)%iTropo == 5) THEN
        text(46:51) = ' Niell'
      ELSEIF(parAtm%trpMod(iTrp)%iTropo == 6) THEN
        text(46:51) = ' GMF'
      ELSEIF(parAtm%trpMod(iTrp)%iTropo == 7) THEN
        text(46:51) = ' VMF'
      ELSEIF((parAtm%trpMod(iTrp)%iTropo > 5 .AND. parAtm%trpMod(iTrp)%iTropo < 11).OR.&
           (parAtm%trpMod(iTrp)%iTropo == 13 .OR.parAtm%trpMod(iTrp)%iTropo == 14)) THEN
        text(46:73) = ' '
      ELSEIF(parAtm%trpMod(iTrp)%iTropo == 11) THEN
        text(46:73) = ' Saastamoinen, dry part only'
      ELSEIF(parAtm%trpMod(iTrp)%iTropo == 12) THEN
        text(46:67) = ' Hopfield, dry part only'
      ELSEIF(parAtm%trpMod(iTrp)%iTropo == 15) THEN
        text(46:66) = ' Niell, dry part only'
      ELSEIF(parAtm%trpMod(iTrp)%iTropo == 16) THEN
        text(46:66) = ' GMF, dry part only'
      ELSEIF(parAtm%trpMod(iTrp)%iTropo == 17) THEN
        text(46:66) = ' VMF, dry part only'
      ENDIF
      WRITE(lfnPrt,'(A)') TRIM(text)

! print meteo/tropo delay values from ...
      text = ' Meteo/Trop.delay values:'
      IF (parAtm%trpMod(iTrp)%iExtra == 0) THEN
        text(46:73) = ' Observed (from meteo files)'
      ELSEIF (parAtm%trpMod(iTrp)%iExtra == 1) THEN
        text(46:59) = ' Extrapolated'
      ELSEIF (parAtm%trpMod(iTrp)%iExtra == 2) THEN
        text(46:110) = ' Estimated values introduced if available (from GPSEST or ADDNEQ)'
      ENDIF
      WRITE(lfnPrt,'(A,/)') TRIM(text)

! print troposphere parameters for individual stations
      text =' Mapping function used for delay estimation: '
      IF (parAtm%trpMod(iTrp)%itrMap == 1) THEN
        text(46:68) = ' 1/cos(zenith-distance)'
      ELSEIF (parAtm%trpMod(iTrp)%itrMap == 2) THEN
        text(46:68) = ' Hopfield              '
      ELSEIF (parAtm%trpMod(iTrp)%itrMap == 3) THEN
        text(46:68) = ' Dry Niell             '
      ELSEIF (parAtm%trpMod(iTrp)%itrMap == 4) THEN
        text(46:68) = ' Wet Niell             '
      ELSEIF (parAtm%trpMod(iTrp)%itrMap == 5) THEN
        text(46:68) = ' Dry GMF               '
      ELSEIF (parAtm%trpMod(iTrp)%itrMap == 6) THEN
        text(46:68) = ' Wet GMF               '
      ELSEIF (parAtm%trpMod(iTrp)%itrMap == 7) THEN
        text(46:68) = ' Dry VMF               '
      ELSEIF (parAtm%trpMod(iTrp)%itrMap == 8) THEN
        text(46:68) = ' Wet VMF               '
      ENDIF
      WRITE(lfnPrt,'(A)') TRIM(text)

! print gradient option
      text =' Troposphere gradient estimation: '
      IF (itrGrd(1) == 0) THEN
        text(46:132)=' No'
      ELSEIF (itrGrd(1) == 1) THEN
        text(46:132)=' Tilted mapping'
      ELSEIF (itrGrd(1) == 2) THEN
        text(46:132)=' Linear correction with distance'
      ELSEIF (itrGrd(1) == 3) THEN
        TEXT(47:132)='Tan(z)'
      ELSEIF (itrGrd(1) == 4) THEN
        TEXT(46:132)=' Chen/Herring'
      ENDIF
      WRITE(lfnPrt,'(A,/,/)') TRIM(text)
    ENDDO

! print a priori sigma
    IF (itrGrd(1) /= 0) THEN
      nTro = 6
      seqTro(1) = 1
      seqTro(2) = 3
      seqTro(3) = 5
      seqTro(4) = 2
      seqTro(5) = 4
      seqTro(6) = 6
      DO iTro = 1, nTro
        sigTro(iTro) = locSig(oldSigTyp+seqTro(iTro))%value
      ENDDO
    ELSE
      nTro   = 2
      DO iTro = 1, nTro
        sigTro(iTro) = locSig(oldSigTyp+iTro)%value
        seqTro(iTro) = iTro
      ENDDO
    ENDIF
    CALL prisig(6, sigTro, nTro, seqTro)

! Finish the part on the troposphere
    IF ( parAtm%nModel > 1 ) THEN
      WRITE(lfnErr,'(/,2A,2(/,17X,A),/)')&
      ' ### SR aoptatm: There are different troposphere models used',      &
                       'in the NEQ-files.',                                &
                       'It is not recommended to stack these parameters.', &
                       'Be aware of this by interpreting the results.'
    ENDIF

  ENDIF
  DEALLOCATE(parAtm%trpMod,stat=iac)

! Print a priori sigmas of global ionosphere model parameters
! -----------------------------------------------------------
  IF (parAtm%nIon > 0) THEN
    DO iGim = 1, nGim
      sigGim(iGim) = locSig(oldSigTyp+6+iGim)%value
      seqGim(iGim) = iGim
    ENDDO
    WRITE(lfnPrt,'(2(A,/))')                  &
      ' Global ionosphere model parameters: ',&
      ' ---------------------------------- '
    CALL prisig(19, sigGim, nGim, seqGim)
  ENDIF

! Print a priori sigmas of HOI scaling factors
! --------------------------------------------
  IF (parAtm%nHoi > 0) THEN
    DO iHoi = 1,nHoi
      CALL ckoptb(1,(/hoiKeyw2(iHoi)/),srName, &
        'Stacking HOIs',irCode,resultL = opt%hoi(iHoi)%stack)
      sigHoi(iHoi) = locSig(oldSigTyp+8+iHoi)%value
      seqHoi(iHoi) = iHoi
      CALL readKeys(hoiKeyw(iHoi),keyValue,irc)
      IF(TRIM(keyValue(1)) == "ZERO") THEN
        opt%hoi(iHoi)%x0 = 0d0
      ELSEIF(TRIM(keyValue(1)) == "ONE") THEN
        opt%hoi(iHoi)%x0 = 1d0
      ELSE
        CALL ckoptr(1,hoiKeyw(iHoi),keyValue,srName, &
          'Scaling factor for HOI: default value',irc,irCode, &
          result1=opt%hoi(iHoi)%x0)
      ENDIF
    ENDDO
    WRITE(lfnPrt,'(2(A,/))')                  &
      ' Higher-order ionosphere scaling factors: ',&
      ' --------------------------------------- '
    CALL prisig(27,sigHoi,nHoi,seqHoi)
  ENDIF

! Store sigmas in opt%sigma
! -------------------------
  DEALLOCATE(opt%sigma,stat=iac)
  ALLOCATE(opt%sigma(nSig+oldSigTyp),stat=iac)
  CALL alcerr(iac,'opt%sigma',(/nSig+oldSigTyp/),'aoptatm')

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
END SUBROUTINE aoptatm

END MODULE
