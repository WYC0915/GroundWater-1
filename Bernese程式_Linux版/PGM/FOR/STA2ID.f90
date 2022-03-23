
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM sta2id

! -------------------------------------------------------------------------
! Purpose:    Translation of a list between station name and 4-ID
!             (usefull for BPE)
!
! Author:     R. Dach
!
! Created:    05-Jun-2001
! Last mod.:  27-Oct-2011
!
! Changes:    22-Dec-2001 HU: Interface to rxobabbr added
!             30-Dec-2001 HU: Dummy(1) must be set to an arbitrary value
!                             in order to get the stanam from rxobabbr
!             19-Mar-2003 RD: Use structure t_abbrev
!             15-May-2003 AJ: Initialize structure
!             03-Nov-2003 RD: Call SR gtabbv instead of SR getabb
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             27-Feb-2007 AG: Call DEFCON
!             23-Sep-2010 RD: Enable CPU counter
!             27-Oct-2011 RD: Write the ID-lists into keyword
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_cpu,    ONLY: cpu_start
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE d_abbrev, ONLY: t_abbrev,init_abbrev
  USE s_defcon
  USE s_gtabbv
  USE s_readkeys
  USE s_gtflna
  USE s_readabb
  USE s_readinpf
  USE s_exitrc
  USE s_alcerr
  USE s_writekey

  IMPLICIT NONE

! Local paramters
! ---------------
  CHARACTER(LEN=6), PARAMETER                 :: pgName = 'sta2id'

! Local variables
! ---------------
  TYPE(t_key)                                 :: keyResult2, keyResult4
  TYPE(t_abbrev)                              :: abbrev

  CHARACTER(LEN=fileNameLength)               :: abbFil
  CHARACTER(LEN=keyValueLength)               :: inpFile
  CHARACTER(LEN=keyValueLength),         &
                        DIMENSION(:), POINTER :: keyValue

  INTEGER(i4b)                                :: nAbb
  INTEGER(i4b),         DIMENSION(:), POINTER :: abbIdx
  INTEGER(i4b)                                :: iSta
  INTEGER(i4b)                                :: iac
  INTEGER(i4b)                                :: irc
!
! Start CPU Counter
! -----------------
  CALL cpu_start(.FALSE.)


  CALL init_abbrev(abbrev)
  CALL init_inpkey(inpKey)

! Get the name of the input file
! ------------------------------
  CALL readinpf(' ',inpKey)
  CALL defcon(0)

! Read abbreviation table
! -----------------------
  CALL gtflna(1,'ABBREV',abbFil,irc)
  CALL readAbb(abbFil,abbrev)

  NULLIFY(abbIdx)
  CALL init_key(keyResult2)
  CALL init_key(keyResult4)

! Read input file
! ---------------
  NULLIFY(keyValue)
  CALL readkeys('STANAM',keyValue,irc)
  IF (irc /= 0) CALL exitrc(2)

! Allocate the result
! -------------------
  ALLOCATE(keyResult2%value(SIZE(keyValue)),stat=iac)
  CALL alcerr(iac,'keyResult2%value',(/ SIZE(keyValue) /), pgName)
  keyResult2%name  = 'STA2ID'
  keyResult2%value = 'name'

  ALLOCATE(keyResult4%value(SIZE(keyValue)),stat=iac)
  CALL alcerr(iac,'keyResult4%value',(/ SIZE(keyValue) /), pgName)
  keyResult4%name  = 'STA4ID'
  keyResult4%value = 'name'

! Loop all station names
! ----------------------
  DO iSta= 1, SIZE(keyValue)
    CALL gtabbv(0,keyValue(iSta),1,abbFil,abbrev,nAbb,abbIdx)
    IF (nAbb > 0) THEN
      keyResult2%value(iSta) = abbrev%abb(abbIdx(1))%staab2
      keyResult4%value(iSta) = abbrev%abb(abbIdx(1))%staab4
      WRITE(*,*) TRIM(keyResult4%value(iSta))
    ENDIF
  ENDDO

! Write the two result records to the INP file
! --------------------------------------------
  CALL readkeys('', keyValue, irc)
  inpFile = TRIM( keyValue(1) )

  CALL writeKey(inpFile,(/ keyResult2,keyResult4 /),1,irc )

  CALL exitrc(0)

END PROGRAM sta2id
