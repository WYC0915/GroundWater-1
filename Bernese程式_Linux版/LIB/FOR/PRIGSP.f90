MODULE s_PRIGSP
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE prigsp(nSta,staNam,nPar,locq,opt)

! ------------------------------------------------------------------------
! Purpose:    Print the information on GNSS-specific parameters
!
! Author:     M. Meindl
!
! Created:    25-Nov-2010
!
! Changes:    27-Mar-2012 RD: Use LISTC1 as module now
!             20-Sep-2012 RD: Correctly deallocate the arrays
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, lfnprt
  USE d_par,    ONLY: maxLcq
  USE p_gpsest, ONLY: t_optGsp

  USE s_alcerr
  USE f_listc1

  IMPLICIT NONE


! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                     :: nSta   ! Number of stations
  CHARACTER(LEN=16),DIMENSION(*)   :: staNam ! Station names
  INTEGER(i4b)                     :: nPar   ! Number of parameters
  INTEGER(i4b),DIMENSION(maxLcq,*) :: locq   ! locq
  TYPE(t_optGsp)                   :: opt    ! Pparameter options

! output:


! Local parameters
! ----------------
  CHARACTER(LEN=6),PARAMETER                 :: srName = 'prigsp'


! Local variables
! ---------------
  CHARACTER(LEN=16),DIMENSION(:),ALLOCATABLE :: staLst
  CHARACTER(LEN=7) ,DIMENSION(4)             :: sysStr
  CHARACTER(LEN=1) ,DIMENSION(2)             :: ynStr
  CHARACTER(LEN=75)                          :: line

  INTEGER(i4b),DIMENSION(:,:),ALLOCATABLE    :: cmpLst
  INTEGER(i4b)                               :: maxGsp, nGsp
  INTEGER(i4b)                               :: iac
  INTEGER(i4b)                               :: ii, iSta


! Init variables
! --------------
  sysStr = (/'NONE   ','GLONASS','GALILEO','GLO+GAL'/)
  ynStr  = (/'N','Y'/)


! Nothing to do at all
! --------------------
  IF (opt%traSys == 0 .AND. opt%trpSys == 0) RETURN


! Compose station overview
! ------------------------
  maxGsp = 0
  DO ii=1,nPar
    IF (locq(1,ii)==30 .AND. &
        (locq(3,ii)==1 .OR. locq(3,ii)==4)) maxGsp = maxGsp+1
  ENDDO

  ALLOCATE(staLst(maxGsp),STAT=iac)
  CALL alcErr(iac,'staLst',(/maxGsp/),srName)
  staLst = ""

  ALLOCATE(cmpLst(4,maxGsp),STAT=iac)
  CALL alcErr(iac,'cmpLst',(/4,maxGsp/),srName)
  cmpLst = 0

! Fill lists
  nGsp = 0
  DO ii=1,nPar
    IF (locq(1,ii) /= 30) CYCLE
    iSta = listc1(1,16,maxGsp,staLst,staNam(locq(2,ii)),nGsp)
    IF (locq(3,ii) == 1) THEN
      cmpLst(1,iSta) = 1
      cmpLst(2+locq(4,ii),iSta) = 1
    ELSEIF (locq(3,ii) == 4) THEN
      cmpLst(2,iSta) = 1
      cmpLst(2+locq(4,ii),iSta) = 1
    ENDIF
  ENDDO


! Print information
! -----------------
  WRITE(lfnPrt,'(A,/,A)') " GNSS-SPECIFIC PARAMETERS:", &
                          " ------------------------"
  WRITE(lfnPrt,'(/,2A)') " SET UP STATION TRANSLATIONS FOR : ", &
                          sysStr(opt%traSys+1)
  WRITE(lfnPrt,'(2A)')  " SET UP TROPOSPHERE BIASES FOR   : ", &
                          sysStr(opt%trpSys+1)
  WRITE(lfnPrt,'(/,1X,74("-"),/,2A,/,1X,74("-"))')             &
    " Station name          Tra Trp  GNSS     ",               &
    "N (m)    E (m)    U (m)    Abs (m)"
  DO ii=1,nGsp
    line = ""
    WRITE(line,'(1X,A16,4X,2(3X,A1),3X,A7)') &
      staNam(ii),ynStr(cmpLst(1,ii)+1),ynStr(cmpLst(2,ii)+1), &
      sysStr(cmpLst(3,ii)+cmpLst(4,ii)*2+1)
    IF (cmpLst(1,ii) == 1) WRITE(line(40:66),'(3(2X,F7.5))') opt%traSig
    IF (cmpLst(2,ii) == 1) WRITE(line(67:75),'(2X,F7.5)')    opt%trpSig
    WRITE(lfnPrt,'(A)') line
  ENDDO

  DEALLOCATE(staLst,STAT=iac)
  DEALLOCATE(cmpLst,STAT=iac)

! The end
! -------
  RETURN
END SUBROUTINE prigsp
END MODULE s_PRIGSP
