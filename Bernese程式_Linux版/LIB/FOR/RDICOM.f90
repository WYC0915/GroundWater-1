MODULE s_RDICOM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdicom(ncenm, cenmas, sigcen)

! -------------------------------------------------------------------------
! Purpose:    Reads the center of mass input options for GPSEST
!
! Author:     R. Dach
!
! Created:    27-Jun-2001
! Last mod.:  23-Apr-2003
!
! Changes:    23-Apr-2003  RD: Nullify local pointers
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
  INTEGER(i4b)               :: ncenm     ! number of center of mass parameter
  INTEGER(i4b), DIMENSION(*) :: cenmas    ! corresp. coordinate numbers
  REAL(r8b),    DIMENSION(*) :: sigcen    ! corresp. a priori sigmas

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
! Center of mass-keywords
  CHARACTER(LEN=keyNameLength), DIMENSION(3), PARAMETER :: cenKeyw = &
           (/ 'COMX', 'COMY', 'COMZ' /)

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
         DIMENSION(:), POINTER  :: keyValue

  INTEGER(i4b)                  :: irc, ios, iac
  INTEGER(i4b)                  :: ii

! Init the variables
! ------------------
  ncenm       = 0
  cenmas(1:3) = 0
  sigcen(1:3) = 0d0

  NULLIFY(keyValue)

! Read keywords
! -------------
  DO ii = 1, SIZE(cenKeyw)
    CALL readKeys(cenKeyw(ii), keyValue, irc)
    IF (irc == 0 .AND. keyValue(1) == '1') THEN
      ncenm         = ncenm + 1
      cenmas(ncenm) = ii
      CALL readKeys('SIG'//TRIM(cenKeyw(ii)), keyValue, irc)
      IF (irc == 0) READ(keyValue(1),*, iostat = ios) sigcen(ncenm)
      IF ( irc /= 0                                     .OR. &
           (ios /= 0 .AND. LEN_TRIM(keyValue(1)) > 0)   .OR. &
           sigcen(ncenm) < 0d0  )                         THEN
        WRITE(lfnerr,'(/,A,16X,A,A,/)')                          &
        ' *** SR RDICOM: Wrong apriori sigma for the center ' // &
                                            'of mass estimation',&
                        'Specified Value: ',TRIM(keyValue(1))
        CALL exitrc(2)
      ENDIF
    ENDIF
  ENDDO

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=iac)

  RETURN
END SUBROUTINE rdicom

END MODULE
