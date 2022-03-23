MODULE s_AOPTOTR
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE aoptotr(opt, parGcc, parRco, parDcb)

! -------------------------------------------------------------------------
! Purpose:    Reads input options for ADDNEQ2:
!             options and constraining for remaining parameters
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
!             10-May-2009 RD: Receiver clock offsets/biases implemented
!             22-Jun-2009 RD: Antenna parameters extracted from AOPTOTR panel
!             24-Jun-2009 RD: Put direct constraints on DCBs
!             13-Aug-2009 DT: Add range bias parameters
!             27-Nov-2009 RD: Antenna parameters extracted from AOPTOTR panel
!             13-Aug-2010 RD: Warning in case of huge IFB
!             08-Sep-2010 RD: Move Range biases to AOPTRGB
!             18-Aug-2011 LP: Add keyword LCREF for ISB constraints
!             19-Sep-2012 RD: Use M_BERN with ONLY, remove unused variables
!             19-Jun-2013 RD: Indicate GLONASS-IFB from PPP
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnprt, keyValueLength
  USE d_const,  ONLY: C
  USE p_addneq, ONLY: t_parGcc, t_opt, t_sigma, t_parRco

  USE s_alcerr
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckoptr
  USE s_exitrc
  USE s_prisig
  USE s_readkeys
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  TYPE(t_parGcc)                         :: parGcc ! Geocenter coo. inp. opt.
  TYPE(t_parRco)                         :: parRco ! Receiver clock offset/bias
  INTEGER(i4b)  , DIMENSION(:,:)         :: parDcb ! DCB-Parameters

! input/output:
  TYPE(t_opt)                            :: opt    ! Options for ADDNEQ2

! output:


! List of Functions
! -----------------

! Local Types
! -----------
  TYPE(t_sigma),DIMENSION(:),ALLOCATABLE  :: locSig

! Local Parameters
! ----------------
  CHARACTER(LEN=7),PARAMETER                :: srName = 'aoptotr'

  CHARACTER(LEN=8),DIMENSION(3),PARAMETER   :: gccKeyw = &
  (/'COFMASSX','COFMASSY','COFMASSZ'/)

  CHARACTER(LEN=8),DIMENSION(2,2,3),PARAMETER :: dcbKeyw =   &
  reshape ( source = (/'P1P2REF ','P1P2SIG ',   &
                       '        ','P1P2SIGR',   &
                       'P1C1REF ','P1C1SIG ',   &
                       '        ','        ',   &
                       '        ','        ',   &
                       'LCREF   ','LCSIGR  ' /), shape = (/2,2,3/) )

  CHARACTER(LEN=16),DIMENSION(2,3), PARAMETER :: dcbTxt  =     &
  reshape ( source = (/ 'P1-P2 satellites','P1-P2 receivers ', &
                        'P1-C1 satellites','P1-C1 receivers ', &
                        'LC satellites   ','LC receivers    ' /), &
                        shape = (/2,3/) )

! Local variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue

  INTEGER(i4b)                                          :: maxSigTyp
  INTEGER(i4b)                                          :: oldSigTyp
  INTEGER(i4b)                                          :: nSig, iSig
  INTEGER(i4b)                                          :: iGcc
  INTEGER(i4b)                                          :: irc, irCode
  INTEGER(i4b)                                          :: iac
  INTEGER(i4b), DIMENSION(3)                            :: seqGcc
  INTEGER(i4b)                                          :: nGcc
  INTEGER(i4b), DIMENSION(1)                            :: seqRco = (/ 1 /)
  INTEGER(i4b)                                          :: iDcb,jDcb,kDcb
  INTEGER(i4b)                                          :: nDcb
  INTEGER(i4b)                                          :: refDcb
  INTEGER(i4b), DIMENSION(12)                           :: seqDcb

  REAL(r8b),    DIMENSION(3)                            :: sigGcc
  REAL(r8b),    DIMENSION(1)                            :: sigRco
  REAL(r8b),    DIMENSION(12)                           :: sigDcb

  NULLIFY(keyValue)

! Get other sigmas from opt%sigma
! -------------------------------
  oldSigTyp = SIZE(opt%sigma)
  maxSigTyp = oldSigTyp + 3 + 1 + 5             ! GCC, RCO, DCB

  ALLOCATE(locSig(maxSigTyp),stat=iac)
  CALL alcerr(iac,'locSig',(/maxSigTyp/),srName)

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

! Geocenter coordinates
! ---------------------
! set up geocenter coordinates
  CALL ckoptb(1, (/'SETUP_GCC'/), srName,                           &
              'Set up geocenter coordinates', irCode,               &
              result1=opt%setupgcc)
  IF (opt%setupgcc == 1) THEN
    parGcc%nGcc = 3
    parGcc%gcc  = (/1,2,3/)
  ENDIF

! get a priori sigmas of geocenter coordinates
! (if geocenter coordinates available from neq or set up)
  IF (parGcc%nGcc > 0) THEN
    sigGcc(:) = 0d0
    DO iGcc = 1, parGcc%nGcc
      nGcc = parGcc%gcc(iGcc)
      CALL readKeys(gccKeyw(nGcc), keyValue, irc)
      CALL ckoptr(1,gccKeyw(nGcc), keyValue, srName,                &
                  'Geocenter coordinates constraint', irc, irCode,  &
                  empty=0d0,ge=0d0,maxVal=1,error=99.9999d0,        &
                  result1=sigGcc(iGcc))
      seqGcc(iGcc) = nGcc
      IF (sigGcc(iGcc) /= 0d0) THEN
        nSig                 = nSig + 1
        iSig                 = nSig + oldSigTyp
        locSig(iSig)%locq(1) = 16
        locSig(iSig)%locq(2) = nGcc
        locSig(iSig)%value   = sigGcc(iGcc)
      ENDIF
    ENDDO

! print a priori sigmas of geocenter coordinates
    WRITE(lfnprt,'(2(A,/))')      &
      ' Geocenter coordinates:',  &
      ' ---------------------'
    CALL prisig(16, sigGcc, parGcc%nGcc, seqGcc)
  ENDIF

! GLONASS biases result from a PPP
! --------------------------------
  IF (parRco%isSat > 0) THEN
    CALL ckoptb(1,(/ 'RCOFROMPPP' /), srName,                      &
                'Receiver clock biases from a PPP', irCode,        &
                result1 = opt%rcoFromPPP)

! Delete all receiver clock biases for GPS satellites
! ---------------------------------------------------
    CALL ckoptb(1,(/ 'RCORMGPS' /), srName,                        &
                'Fix biases for all GPS satellites', irCode,       &
                result1 = opt%rcoRmGPS)

! Transform receiver clock satellite to frequency biases
! ------------------------------------------------------
    CALL ckoptb(1,(/ 'RCOTOFRQ' /), srName,                        &
                'Transform satellite to frequency biases', irCode, &
                result1 = opt%rcoToFrq)

! Report the requests
! -------------------
    IF (opt%rcoFromPPP + opt%rcoRmGPS + opt%rcoToFrq > 0) THEN
      WRITE(lfnprt,'(2(A,/))')      &
        ' Receiver clock biases:',  &
        ' ---------------------'
      IF (opt%rcoFromPPP == 1) WRITE(lfnprt,'(A)') &
          '   Biases in the NEQ stem from a PPP (to be considered for constraining).'
      IF (opt%rcoRmGPS == 1) WRITE(lfnprt,'(A)') &
          '   Fix receiver clock biases for all GPS satellites.'
      IF (opt%rcoToFrq == 1) WRITE(lfnprt,'(A)') &
          '   Transform satellite to frequency specific receiver clock biases.'
    ENDIF
  ENDIF

! Maximum expected IFB
! --------------------
  CALL readKeys('IFBMAX', keyValue, irc)
  CALL ckoptr(1,'IFBMAX', keyValue, srName,                   &
                'Max. expected IFB estimate', irc, irCode,    &
                empty=1d20,ge=0d0,maxVal=1,error=1d20,        &
                result1=opt%maxIfb)

! Sigma for receiver clock offset
! -------------------------------
  IF (parRco%isSta > 0) THEN
    CALL readKeys('RCOSIGMA', keyValue, irc)
    CALL ckoptr(1,'RCOSIGMA', keyValue, srName,                   &
                'Receiver clock offset constraint', irc, irCode,  &
                empty=0d0,ge=0d0,maxVal=1,error=99.9999d0,        &
                result1=sigRco(1))
      IF (sigRco(1) /= 0d0) THEN
        nSig                 = nSig + 1
        iSig                 = nSig + oldSigTyp
        locSig(iSig)%locq(1) = 2
        locSig(iSig)%value   = sigRco(1)/C*1d3
      ENDIF

! print a priori sigmas of receiver clock offsets
    WRITE(lfnprt,'(2(A,/))')      &
      ' Receiver clock offset:',  &
      ' ---------------------'
    CALL prisig(2, sigRco, 1, seqRco )
  ENDIF

! Differential code biases
! ------------------------
  seqDcb(:) = 0
  sigDcb(:) = 0d0
  nDcb      = 0
  DO iDcb = 1, SIZE(dcbKeyw,2)
    DO jDcb = 1, SIZE(dcbKeyw,3)
      kDcb = iDcb+(jDcb-1)*SIZE(dcbKeyw,2)
      IF (parDcb(iDcb,jDcb) == 0) CYCLE

      refDcb = 1
      IF ( LEN_TRIM(dcbKeyw(1,iDcb,jDcb)) > 0 ) THEN
        CALL readKeys(dcbKeyw(1,iDcb,jDcb), keyValue, irc)
        CALL ckoptc(1,dcbKeyw(1,iDcb,jDcb), keyValue, (/'SUM','ALL'/), srName, &
                    TRIM(dcbTxt(iDcb,jDcb)) // ': type of sigma', irc, irCode, &
                    maxVal=1,valList = (/ 2,1 /),result1=refDcb )
      ENDIF

      IF ( LEN_TRIM(dcbKeyw(2,iDcb,jDcb)) > 0 ) THEN
        nDcb = nDcb+1
        seqDcb(nDcb) = kDcb

        CALL readKeys(dcbKeyw(2,iDcb,jDcb), keyValue, irc)
        CALL ckoptr(1,dcbKeyw(2,iDcb,jDcb), keyValue, srName,             &
                      TRIM(dcbTxt(iDcb,jDcb)) // ': sigma', irc, irCode,  &
                      empty=0d0,ge=0d0,maxVal=1,error=99.9999d0,          &
                      result1=sigDcb(nDcb))

        IF (sigDcb(nDcb) == 0d0 .AND. refDcb == 2) sigDcb(nDcb) = 0.001d0

        IF (sigDcb(nDcb) /= 0d0) THEN
          nSig                 = nSig + 1
          iSig                 = nSig + oldSigTyp
          locSig(iSig)%value   = sigDcb(nDcb)
          locSig(iSig)%locq(1) = 8
          locSig(iSig)%locq(2) = iDcb
          IF ( iDcb == 1 )   locSig(iSig)%locq(5) = jDcb
          IF ( iDcb == 2 )   locSig(iSig)%locq(6) = jDcb
          IF ( refDcb == 2 ) THEN
            locSig(iSig)%typFlg  = 'S'
            seqDcb(nDcb) = seqDcb(nDcb)+SIZE(dcbKeyw,2)*SIZE(dcbKeyw,3)
          ENDIF
        ENDIF
      ENDIF
    ENDDO
  ENDDO

! print a priori sigmas of receiver clock offsets
  IF (nDcb > 0) THEN
    WRITE(lfnprt,'(2(A,/))')      &
      ' Differential code biases:',  &
      ' ------------------------'
    CALL prisig(8, sigDcb, nDcb, seqDcb )
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

! Deallocate the special request definition
! -----------------------------------------
  DEALLOCATE(locSig,stat=iac)
  DEALLOCATE(keyValue,stat=iac)

  RETURN
END SUBROUTINE aoptotr

END MODULE
