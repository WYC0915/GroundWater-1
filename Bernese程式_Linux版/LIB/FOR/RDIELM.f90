MODULE s_RDIELM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdielm(maxtyp, opteli)

! -------------------------------------------------------------------------
! Purpose:    Reads the pre-elimination options for GPSEST
!
! Author:     R. Dach
!
! Created:    29-Jun-2001
! Last mod.:  03-May-2011
!
! Changes:    23-Apr-2003 RD: Nullify local pointers
!             19-Jan-2003 SS/MM: Revision of GPSEST input panels
!             04-Jan-2010 SL: pre-elimination for case 27 (HOI) added
!             15-Dec-2010 MM: GNSS-specific parameters (30) added
!             03-May-2011 RD: Correct list of ADDNEQ2-only parameters
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_dimtst
  USE s_readkeys
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)               :: maxtyp ! maximum number of parameter types

! output:
  INTEGER(i4b), DIMENSION(*) :: opteli ! option for preelimination of
                                       ! parameter types:
                                       ! 0 : not pre-eliminated
                                       ! 1 : pre-eliminated before inversion
                                       ! 2 : pre-eliminated after  inversion
                                       ! 3 : pre-eliminated epoch-wise

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
! Center of mass-keywords
  CHARACTER(LEN=keyNameLength), DIMENSION(30), PARAMETER :: elmKeyw = &
  (/ 'PRE01', 'PRE02', 'PRE03', 'PRE04', 'PRE05', 'PRE06', 'PRE07', 'PRE08', &
     'PRE09', 'PRE10', 'PRE11', 'PRE12', 'PRE13', 'PRE14', 'PRE15', 'PRE16', &
     'PRE17', 'PRE18', 'PRE19', 'PRE20', 'PRE21', 'PRE22', 'PRE23', 'PRE24', &
     'PRE25', 'PRE26', 'PRE27', 'PRE28', 'PRE29', 'PRE30'                   /)

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
         DIMENSION(:), POINTER  :: keyValue

  INTEGER(i4b)                  :: irc,iac
  INTEGER(i4b)                  :: ii

! Init the variables
! ------------------
  opteli(1:maxtyp) = 0

  NULLIFY(keyValue)

! Check the number of parameters
! ------------------------------
  CALL dimtst(1,1,2,'rdielm','maxtyp',          &
       'number of pre-elimination keywords',    &
       'This is an error in the source code!',  &
       SIZE(elmKeyw),maxtyp,irc)

! Loop all entries
! ----------------
  DO ii = 1, SIZE(elmKeyw)
    IF (ii == 11) THEN
      opteli(ii) = opteli(3)
      CYCLE
    ENDIF
    ! Parameters only available in ADDNEQ2
    IF (ii == 20 .OR. ii == 28 .OR. ii == 29  ) CYCLE
    CALL readKeys(elmKeyw(ii), keyValue, irc)
    IF (irc == 0 .AND. keyValue(1) == 'EVERY_SESSION')       opteli(ii) = 1
    IF (irc == 0 .AND. keyValue(1) == 'PRIOR_TO_NEQ_SAVING') opteli(ii) = 2
    IF (irc == 0 .AND. keyValue(1) == 'EVERY_EPOCH')         opteli(ii) = 3
  ENDDO

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=iac)

  RETURN
END SUBROUTINE rdielm

END MODULE
