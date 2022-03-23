MODULE s_RDIDCB
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdidcb(nAllSat, allSatNum, optdcb, sigdcb)

! -------------------------------------------------------------------------
! Purpose:    Reads the differential code bias input options for GPSEST
!
! Author:     R. Dach
!
! Created:    26-Jun-2001
! Last mod.:  19-Jan-2004
!
! Changes:    29-Jun-2001  RD: was the reference satellite observed?
!             07-May-2002  SS: DCB update
!             23-Apr-2003  RD: Nullify local pointers
!             19-Jan-2003  SS/MM: Revision of GPSEST input panels
!
! SR used:    exitrc, readKeys
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_ckoptr
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)               :: nAllSat   ! number of all satellites
  INTEGER(i4b), DIMENSION(*) :: allSatNum ! satellite numbers

! output:
  INTEGER(i4b), DIMENSION(*) :: optdcb    ! options for estimation of
                                          ! differential code biases
                                          ! (1): estimate dcbs for satellites
                                          !      = 0: NO
                                          !      = 1: P1-P2
                                          !      = 2: P1-C1
                                          !      = 3: LC
                                          ! (2): estimate dsbs for receivers
                                          !      = 0: NO
                                          !      = 1: P1-P2
                                          !      = 2: P1-C1
                                          !      = 3: LC
                                          !      =-2: P1-C1_MULTIPLIER
                                          ! (3): reference satellite number
                                          !      = 0: constrain all sat
                                          !      =-1: constrain sum of all sat
                                          ! (4): process night-time data only
  REAL(r8b),    DIMENSION(*) :: sigdcb    ! a priori sigma for dcbs (in ns)
                                          ! (1): reference satellite biases
                                          ! (2): receiver biases

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER     :: srName = 'rdidcb'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength) , &
         DIMENSION(:)  , POINTER  :: keyValue

  INTEGER(i4b)                    :: iSat, iTyp
  INTEGER(i4b)                    :: irc, ios, iac
  INTEGER(i4b)                    :: irCode

  LOGICAL                         :: satFound

! Initialization
! --------------
  irCode = 0

  NULLIFY(keyValue)

! Estimate DCB for satellites
! ----------------------------
  CALL readKeys('DCBSAT', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') optdcb(1) = 1

! Estimate DCB for receivers
! --------------------------
  CALL readKeys('DCBREC', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') optdcb(2) = 1

! Type of DCB to be estimated
! ---------------------------
  CALL readKeys('DCBTYP', keyValue, irc)
  CALL ckoptc(1,'DCBTYP', keyValue, &
              (/'P1-P2           ','P1-C1           ','LC              ','P1-C1_MULTIPLIER'/), &
              srName, 'Type of DCB', irc, irCode, &
              valList=(/1,2,3,-2/), result1=iTyp)

  optdcb(1:2) = iTyp * optdcb(1:2)

  IF (optdcb(1) < 0) optdcb(1) = 0

! DCB reference
! -------------
  ios = 0
  CALL readKeys('DCBREF', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == 'SUM') THEN
    optdcb(3) = -1
  ELSE IF (irc == 0 .AND. keyValue(1) == 'ALL') THEN
    optdcb(3) = 0
  ELSE IF (irc == 0) THEN
    READ(keyValue(1),*,iostat=ios) optdcb(3)
  ENDIF

  IF (irc /= 0 .OR. ios /= 0) THEN
    WRITE(lfnerr, '(/,A,/,16X,A,A,/)')                                     &
    ' *** SR RDIDCB: Wrong reference satellite entry for DCB estimation.', &
                    'Specified Value: ',TRIM(keyValue(1))
    CALL exitrc(2)
  ENDIF

! Was the reference satellite observed?
! -------------------------------------
  IF (optdcb(3) > 0) THEN
    satFound = .FALSE.
    DO iSat = 1, nAllSat
      IF (allSatNum(iSat) == optdcb(3)) satFound = .TRUE.
    ENDDO
    IF (.NOT. satFound) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/)')                           &
      ' *** SR RDIDCB: The reference satellite for DCB estimation is not ',&
                      'in the obervation files! Select another one!',      &
                      'Selected reference satellite:  ',optdcb(3)
      CALL exitrc(2)
    ENDIF
  ENDIF

! apriori sigma
! -------------
  sigdcb(1:2) = 0d0

  CALL readKeys('DCBSIG', keyValue, irc)
  CALL ckoptr(1,'DCBSIG',keyValue,srName,                                 &
              'A priori sigma for reference satellite biases',irc,irCode, &
              gt=0d0,empty=0d0,result1=sigdcb(1))

  CALL readKeys('DCBSIGR', keyValue, irc)
  CALL ckoptr(1,'DCBSIGR',keyValue,srName,                                &
              'A priori sigma for receiver biases',irc,irCode,            &
              gt=0d0,empty=0d0,result1=sigdcb(2))

  IF (optdcb(2) < 0) sigdcb(2) = 0d0

  IF (irCode /= 0) CALL exitrc(2)

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=iac)

  RETURN
END SUBROUTINE rdidcb

END MODULE
