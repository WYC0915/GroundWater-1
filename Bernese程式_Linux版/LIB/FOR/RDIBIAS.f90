MODULE s_RDIBIAS
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdibias(maxsta,nAllSta,allStaNum,allStaName,nrgb,stargb, &
                   satSpec,sigrgb)

! -------------------------------------------------------------------------
! Purpose:    Reads the input options for SLR range biases for GPSEST
!
! Author:     D. Thaller
!
! Created:    02-Apr-2009
! Last mod.:
!
! Changes:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern

  USE s_readKeys
  USE s_ckoptr
  USE s_ckoptc
  USE s_alcerr
  USE s_gtStaNum

  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                               :: maxsta     ! maximum number of stations
  INTEGER(i4b)                               :: nAllSta    ! number of all stations
  INTEGER(i4b), DIMENSION(*)                 :: allStaNum  ! station numbers
  CHARACTER(LEN=staNameLength), DIMENSION(:) :: allStaName ! all station names

! output:
  INTEGER(i4b)                               :: nrgb       ! number of range bias parameter
  INTEGER(i4b), DIMENSION(*)                 :: stargb     ! station numbers for range bias
                                                           ! requests
  INTEGER(i4b)                               :: satSpec    ! satellite numbers for range bias
                                                           ! requests
  REAL(r8b)                                  :: sigrgb     ! corresp. a priori sigmas


! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER  :: srName = 'rdibias'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:),   POINTER :: keyValue
  CHARACTER(LEN=staNameLength),  DIMENSION(:),   POINTER :: StaRgbNam

  INTEGER(i4b)                                           :: irc, irCode
  INTEGER(i4b)                                           :: iSta
  INTEGER(i4b)                                           :: nStaRgb
  INTEGER(i4b), DIMENSION(maxsta)                        :: StaRgbNum

  REAL(r8b),    DIMENSION(:,:), POINTER                  :: hlpWeight


! Initialization
! --------------
  nrgb = 0

  NULLIFY(keyValue)
  NULLIFY(hlpWeight)
  NULLIFY(StaRgbNam)

  ALLOCATE(staRgbNam(nAllSta), stat=irc)
  CALL alcerr(irc, 'staRgbNam', (/nAllSta/), srName)


! Apriori sigma for range biases
! ------------------------------
  CALL readKeys('SIGRGB', keyValue, irc)
  CALL ckoptr(1,'SIGRGB', keyValue,srName,                   &
              'A priori sigma for range biases',irc,irCode,  &
              empty=0d0,ge=0d0,maxVal=1,result1=sigrgb)

! Stations to be set up with range biases
! ---------------------------------------
  CALL gtStaNum(nAllSta, allStaNum, allStaName,  &
       'RGBLST', 'STATION9', 'STAFILE9', ' ',   &
       nStaRgb, StaRgbNum, StaRgbNam, 0, hlpWeight)

  DO iSta = 1, nStaRgb
    staRgb(iSta) = StaRgbNum(iSta)
  END DO

! Number of requests for range biases (i.e., number of stations)
! --------------------------------------------------------------
  nrgb = nStaRgb

! Set-up for satellites
! ---------------------
  CALL readKeys('SATRGB', keyValue, irc)
  CALL ckoptc(1, 'SATRGB', keyValue,                 &
              (/'SAT_SPECIFIC','SYS_SPECIFIC','PER_STATION '/),   &
              srName, 'Satellites for Range Biases', irc, irCode, &
              result1=satSpec)

  RETURN

END SUBROUTINE rdibias

END MODULE
