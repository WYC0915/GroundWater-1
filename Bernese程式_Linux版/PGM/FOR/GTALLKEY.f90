
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM gtAllKey

! -------------------------------------------------------------------------
! Purpose:    Get all keys (and their values) from a given list of panels
!
! Author:     L. Mervart
!
! Created:    29-Jul-2000
! Last mod.:  06-Oct-2010
!
! Changes:    26-Jun-2001 RD: Use alcerr for allocation
!             09-Aug-2001 MM: now creates an (more or less) useable file
!                             for PANO2N
!             21-Dec-2001 HU: Use m_bern
!             23-Apr-2003 HU: Nullify local pointers
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             27-Feb-2007 AG: Call DEFCON
!             23-Sep-2010 RD: Enable CPU counter
!             06-Oct-2010 RD: Exitrc added at the end
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_cpu,    ONLY: cpu_start
  USE d_inpkey, ONLY: inpKey, init_inpkey

  USE s_readkeys
  USE s_opnsys
  USE s_defcon
  USE s_getpan
  USE s_alcerr
  USE s_readinpf
  USE s_exitrc

  IMPLICIT NONE

! Local variables
! ---------------
  CHARACTER(LEN=keyValueLength) , DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=8),  DIMENSION(:), ALLOCATABLE          :: oldKeyName
  CHARACTER(LEN=10), DIMENSION(:), ALLOCATABLE          :: newKeyName
  CHARACTER(LEN=12), DIMENSION(:), ALLOCATABLE          :: newElement
  CHARACTER(LEN=80), DIMENSION(:), ALLOCATABLE          :: oldKeyPan
  INTEGER(i4b)                                          :: irc, iac
  INTEGER(i4b)                                          :: ii
  INTEGER(i4b)                                          :: jj
  INTEGER(i4b)                                          :: numOldKey
  INTEGER(i4b)                                          :: numNewKey
  INTEGER(i4b)                                          :: numFound
  INTEGER(i4b)                                          :: lenDot

! Parameters for the Subroutine GETPAN
! ------------------------------------
  INTEGER(i4b)     , PARAMETER         :: maxkey = 2000
  INTEGER(i4b)                         :: mxckey
  INTEGER(i4b)                         :: nkeywd
  CHARACTER(LEN=88), DIMENSION(maxkey) :: valstr
  CHARACTER(LEN=80)                    :: oldpan
  CHARACTER(LEN=10)                    :: pgmnam


  COMMON /cgtpan/ mxckey,nkeywd,oldpan,valstr
  mxckey = maxkey

! Start CPU Counter
! -----------------
  CALL cpu_start(.FALSE.)

! Nullify pointers
! ----------------
  NULLIFY(keyValue)

! Define Logical File Numbers
! ---------------------------

! Get the name of the input file
! ------------------------------
  CALL init_inpkey(inpKey)
  CALL readinpf(' ',inpKey)

  CALL opnsys
  CALL defcon(0)

! Read Program Name
! -----------------
  CALL readkeys('PGMNAME', keyValue, irc)
  pgmnam=keyValue(1)

! Read all old Keywords
! ---------------------
  CALL readkeys('PANLIST', keyValue, irc)

  IF (irc /= 0) THEN
! Print the List of All Keys
    CALL readKeys('*', keyValue, irc)
    DO ii = 1, SIZE(keyValue)
      WRITE(*,'(A)') TRIM(keyValue(ii))
    END DO
    CALL exitrc(0)
  END IF

  numOldKey = 0
  DO ii = 1, SIZE(keyValue)
    CALL GETPAN(keyValue(ii), irc)
    numOldKey = numOldKey + nkeywd
  END DO

  ALLOCATE(oldKeyName(numOldKey), stat=iac)
  CALL alcerr(iac, 'oldKeyName', (/numOldKey/), 'gtallkey')
  ALLOCATE(oldKeyPan(numOldKey), stat=iac)
  CALL alcerr(iac, 'oldKeyPan', (/numOldKey/), 'gtallkey')

  numOldKey = 0
  DO ii = 1, SIZE(keyValue)
    CALL GETPAN(keyValue(ii), irc)
    lenDot = INDEX(keyValue(ii), '.')
    keyValue(ii) = keyValue(ii)(lenDot-8:lenDot+3)
    DO jj = 1, nkeywd
      numOldKey = numOldKey + 1
      oldKeyName(numOldKey) = valstr(jj)(1:8)
      oldKeyPan(numOldKey)  = TRIM(keyValue(ii))
    END DO
  END DO

! Read all new keywords
! ---------------------
  CALL readkeys('KEYLIST', keyValue, irc)
  numNewKey = SIZE(keyValue)

  ALLOCATE(newKeyName(numNewKey), stat=iac)
  CALL alcerr(iac, 'newKeyName', (/numNewKey/), 'gtallkey')
  ALLOCATE(newElement(numNewKey), stat=iac)
  CALL alcerr(iac, 'newElement', (/numNewKey/), 'gtallkey')

  DO ii=1, numNewKey
    READ (keyValue(ii),'(A,A)') newKeyName(ii), newElement(ii)
  ENDDO

! Write Header
! ------------

   WRITE(*,"(/,'# Input File of the Program PANO2N', &
               &  /,'##################################', &
     & //,'# pgm      oldPanel      oldKey      newKey       desc  &
     &       value', &
     &  /,'# ------------------------------------------------------&
     &------------')")

! Loop over all new Keywords
! --------------------------
  DO ii = 1, numNewKey

    numFound = 0
! Old keyword exists
    DO jj = 1, numOldKey
      IF (oldKeyName(jj) == newKeyName(ii)) numFound = numFound + 1
    END DO

    IF (numFound == 0) THEN
      WRITE(*, '(A10,1X,A,2X,A,2X,T38,A,T50,A)') pgmnam, '<NEW>       ', &
       '<NEW>       ', TRIM(newKeyName(ii)), TRIM(newElement(ii))
    ELSE
      WRITE(*, '(A10,1X)', ADVANCE='NO') pgmnam
      DO jj = 1, numOldKey
        IF (oldKeyName(jj) == newKeyName(ii) ) THEN
          WRITE(*, '(A,2X,A,T27)', ADVANCE='NO') TRIM(oldKeyPan(jj)), &
              TRIM(newKeyName(ii))
        END IF
      END DO
      WRITE(*,'(A,T13,A)') TRIM(newKeyName(ii)), TRIM(newElement(ii))
    END IF
  END DO

! Loop over all old keywords
! --------------------------
  DO ii = 1, numOldKey
    numFound = 0

    DO jj = 1, numNewKey
      IF (newKeyName(jj) == oldKeyName(ii)) numFound = numFound + 1
    END DO

    IF (numFound == 0) THEN
      WRITE(*, '(A10,1X,A,2X,A,T38,A)') pgmnam, TRIM(oldKeyPan(ii)), &
       oldKeyName(ii), '<OLD>'
    ENDIF
  ENDDO

  CALL exitrc(0)

END PROGRAM gtAllKey
