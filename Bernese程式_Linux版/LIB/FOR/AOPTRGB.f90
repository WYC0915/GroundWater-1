MODULE s_AOPTRGB
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE aoptrgb(opt, parRgb)

! -------------------------------------------------------------------------
! Purpose:    Reads input options for ADDNEQ2:
!             options and constraining for range bias parameters
!
! Author:     R. Dach
!
! Created:    08-Sep-2010
!
! Changes:    19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnprt, keyValueLength
  USE p_addneq, ONLY: t_opt, t_sigma, t_parRgb

  USE s_alcerr
  USE s_ckoptr
  USE s_exitrc
  USE s_prisig
  USE s_readkeys
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  TYPE(t_parRgb)                         :: parRgb ! Range bias parameters

! input/output:
  TYPE(t_opt)                            :: opt    ! Options for ADDNEQ2

! output:


! List of Functions
! -----------------

! Local Types
! -----------
  TYPE(t_sigma),   DIMENSION(:), ALLOCATABLE  :: locSig

! Local Parameters
! ----------------
  CHARACTER(LEN=7),PARAMETER                  :: srName = 'aoptrgb'

  CHARACTER(LEN=6),PARAMETER                  :: rgbKeyw = 'RGBSIG'

  INTEGER(i4b),    PARAMETER                  :: rgbSigTyp = 1

! Local variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue

  INTEGER(i4b)                                          :: maxSigTyp
  INTEGER(i4b)                                          :: oldSigTyp
  INTEGER(i4b)                                          :: nSig, iSig
  INTEGER(i4b)                                          :: irc, irCode
  INTEGER(i4b)                                          :: iac

  INTEGER(i4b), DIMENSION(1)                            :: seqRgb

  REAL(r8b),    DIMENSION(1)                            :: sigRgb

  NULLIFY(keyValue)

! Get other sigmas from opt%sigma
! -------------------------------
  oldSigTyp = SIZE(opt%sigma)
  maxSigTyp = oldSigTyp + rgbSigTyp             ! RGB

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


! Range bias parameters
! ---------------------
  IF (parRgb%nRgb > 0) THEN
    sigRgb(1) = 0d0
    seqRgb(1) = 1
    CALL readKeys(rgbKeyw, keyValue, irc)
    CALL ckoptr(1,rgbKeyw, keyValue, srName,                &
                'Range bias constraint', irc, irCode,       &
                empty=0d0,ge=0d0,maxVal=1,error=99.9999d0,  &
                result1=sigRgb(1))

    IF (sigRgb(1) /= 0d0) THEN
      nSig                 = nSig + 1
      iSig                 = nSig + oldSigTyp
      locSig(iSig)%locq(1) = 26
      locSig(iSig)%value   = sigRgb(1)
    ENDIF

    WRITE(lfnprt,'(2(A,/))')  &
      ' SLR Range biases:',    &
      ' ----------------'

! Get stacking option for range bias parameters
    opt%stackRGB = 0
    CALL readKeys('RGB_STK1', keyValue, irc)
    IF (irc == 0 .AND. keyValue(1) == '1') THEN
      opt%stackRGB = 1
      WRITE(lfnprt, '(A,/)') &
            ' Stacking of range biases: Identical parameters only'
    END IF

    CALL readKeys('RGB_STK2', keyValue, irc)
    IF (irc == 0 .AND. keyValue(1) == '1') THEN
      opt%stackRGB = 2
      WRITE(lfnprt, '(A,/)') &
            ' Stacking of range biases: Satellites of one group'
    END IF

    CALL readKeys('RGB_STK3', keyValue, irc)
    IF (irc == 0 .AND. keyValue(1) == '1') THEN
      opt%stackRGB = 3
      WRITE(lfnprt, '(A,/)') &
            ' Stacking of range biases: All satellites for one station'
    END IF

! print a priori sigmas of range bias parameters
!!!    CALL prisig(26, sigRgb, parRgb%nRgb, seqRgb)
    CALL prisig(26, sigRgb, 1, seqRgb)

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
END SUBROUTINE aoptrgb

END MODULE
