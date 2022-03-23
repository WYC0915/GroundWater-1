MODULE s_RDIDIP
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdidip(optdip, sigdip)

! -------------------------------------------------------------------------
! Purpose:    Reads the stochastic ionosphere input options for GPSEST
!
! Author:     R. Dach
!
! Created:    27-jun-2001
! Last mod.:  19-Jan-2004
!
! Changes:    23-Apr-2003  RD: Nullify local pointers
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
  USE s_readkeys
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:

! output:
  INTEGER(i4b), DIMENSION(*) :: optdip ! options for diff. ion. parameters
                                       ! (1): =0: no diff. ion. parameters
                                       !      =1: one par. per epoch and sat.
                                       !      =2: parameters epoch-wise pre-
                                       !          eliminated
                                       ! (2): elimination of ref. ion. par.
                                       ! (3): elev.-dep. par. constraining
  REAL(r8b),    DIMENSION(2) :: sigdip ! a priori sigmas for diff. ion. par.
                                       ! (1): absolute sigma
                                       ! (2): relative sigma in m/min**1/2

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
         DIMENSION(:), POINTER  :: keyValue

  INTEGER(i4b)                  :: irc, ios, iac

! Init the variables
! ------------------
  optdip(1:3) = 0
  sigdip(1:2) = 0d0

  NULLIFY(keyValue)

  optdip(1) = 1

! Preeliminate reference parameter
! --------------------------------
  CALL readKeys('DIONELI', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') optdip(2) = 1

! Elevation-dependent parameter constraining
! ------------------------------------------
  CALL readKeys('DIONCON', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') optdip(3) = 1

! Absolute apriori sigma
! ----------------------
  CALL readKeys('DIONSIG', keyValue, irc)
  IF (irc == 0)  READ(keyValue(1),*,iostat=ios) sigdip(1)
  IF ( irc /= 0                                  .OR. &
      (ios /= 0 .AND. LEN_TRIM(keyValue(1)) > 0) .OR. &
       sigdip(1) < 0d0 )                           THEN
    WRITE(lfnerr, '(/,A,/16X,A,/,16X,A,A,/)')             &
    ' *** SR RDIDIP: Wrong absolute a priori sigma for ', &
                    'stoachastic ionosphere parameters',  &
                    'Specified Value: ',TRIM(keyValue(1))
    CALL exitrc(2)
  ENDIF

! Relative apriori sigma
! ----------------------
  CALL readKeys('DIONSIGR', keyValue, irc)
  IF (irc == 0)  READ(keyValue(1),*,iostat=ios) sigdip(2)
  IF ( irc /= 0                                  .OR. &
      (ios /= 0 .AND. LEN_TRIM(keyValue(1)) > 0) .OR. &
       sigdip(1) < 0d0 )                           THEN
    WRITE(lfnerr, '(/,A,/16X,A,/,16X,A,A,/)')             &
    ' *** SR RDIDIP: Wrong relative a priori sigma for ', &
                    'stoachastic ionosphere parameters',  &
                    'Specified Value: ',TRIM(keyValue(1))
    CALL exitrc(2)
  ENDIF

! Deallocate loacl pointers
! -------------------------
  DEALLOCATE(keyValue,stat=iac)

  RETURN
END SUBROUTINE rdidip

END MODULE
