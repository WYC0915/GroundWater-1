MODULE s_CEWCKD
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE cewckd(isel,irfsat,irfsit,epoch, &
                  svncmb,svnc,  ipar ,mepo ,clk  ,wgtclk,numclk, &
                  dclk  ,wgtcld, numcld, &
                  nftot ,stname,iparf,mepof,stclk,wgtsta,dstclk,wgtstd)

! -------------------------------------------------------------------------
! Purpose:    Write file with code clocks and phase clock differences
!
! Remarks:    - Subroutine of CLKEST
!
! Author:     U. Hugentobler
!
! Created:    02-Jan-2002
! Last mod.:  18-Aug-2009
!
! Changes:    18-Aug-2006 RD: change format for svn >= 100 (GNSS)
!             18-Aug-2009 RD: add numclk/numcld to the output
!
! SR used:    gtflna,opnfil,opnerr
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_opnfil
  USE s_opnerr
  USE s_gtflna
  IMPLICIT NONE

! List of arguments
! -----------------
! Input:
  INTEGER(i4b)                :: isel      ! =1: write only sat clocks
                                           ! =2: write only sta clocks
                                           ! =3: write sat and sta clocks
  INTEGER(i4b)                :: irfsat    ! Number of ref. sat, or zero
  INTEGER(i4b)                :: irfsit    ! Number of ref. sta, or zero
  REAL(r8b),DIMENSION(:)      :: epoch     ! Array of epochs
  INTEGER(i4b)                :: svncmb    ! Number of satellites
! For satellite i=1,...,svcmp:
  INTEGER(i4b),DIMENSION(:)   :: svnc      ! SVN number
  INTEGER(i4b),DIMENSION(:)   :: ipar      ! Number of clock parameters
! For epoch i and satellite k=1,...,svcmp:
  INTEGER(i4b),DIMENSION(:,:) :: mepo      ! Epoch indices
! Input/Output:
! For epoch i and satellite k=1,...,svcmp:
  REAL(r8b),DIMENSION(:,:)    :: clk       ! code clock values
  REAL(r8b),DIMENSION(:,:)    :: wgtclk    ! wgt of code clock values
  INTEGER(i4b),DIMENSION(:,:) :: numclk    ! num of code clock values
  REAL(r8b),DIMENSION(:,:)    :: dclk      ! phase clock differences
  REAL(r8b),DIMENSION(:,:)    :: wgtcld    ! wgt of phase clock differences
  INTEGER(i4b),DIMENSION(:,:) :: numcld    ! num of phase clock differences
! Input:
  INTEGER(i4b)                :: nftot     ! Number of stations (files)
! For station i=1,...,nftot:
  CHARACTER(LEN=16),DIMENSION(:) :: stname ! Station name
  INTEGER(i4b),DIMENSION(:)   :: iparf     ! Number of clock parameters
! For epoch i and station k=1,...,nftot:
  INTEGER(i4b),DIMENSION(:,:) :: mepof     ! Epoch indices
! Input/Output:
! For epoch i and station k=1,...,nftot:
  REAL(r8b),DIMENSION(:,:)    :: stclk     ! code clock values
  REAL(r8b),DIMENSION(:,:)    :: wgtsta    ! wgt of code clock values
  REAL(r8b),DIMENSION(:,:)    :: dstclk    ! phase clock differences
  REAL(r8b),DIMENSION(:,:)    :: wgtstd    ! wgt of phase clock differences

! Local variables
! ---------------
  INTEGER(i4b)                  :: irc,iostat,jsat,jfil,iepo
  REAL(r8b)                     :: clkj,rmsj,dclkj,rmsdj
  CHARACTER(LEN=fileNameLength) :: filaux

! Write ckd-file
! --------------
  CALL GTFLNA(0,'CLKDIFF',filaux,irc)
  IF (irc==0) THEN
    CALL opnfil(lfnloc,filaux,'UNKNOWN','FORMATTED',' ',' ',iostat)
    CALL opnerr(lfnerr,lfnloc,iostat,filaux,'CEWCKD')
    WRITE(lfnloc,"('# Clocks and Clock Differences', &
               & /,'# ----------------------------')")
    IF (irfsat /= 0) THEN
      WRITE(lfnloc,"('# Reference: Satellite ',I4)") svnc(irfsat)
    ELSEIF (irfsit /= 0) THEN
      WRITE(lfnloc,"('# Reference: Station ',A)") stname(irfsit)
    ENDIF
    WRITE(lfnloc,"('#',/,'# epoch clk rmsclk dclk rmsdclk')")

! Satellites
    IF (isel /= 2) THEN
      DO jsat=1,svncmb
        IF (JSAT==irfsat) CYCLE
        WRITE(lfnloc,"(/,'# isat=',I3,'  SVN ',I3.2)") jsat,svnc(jsat)
        DO iepo=1,ipar(jsat)
          IF (wgtclk(iepo,jsat) > 0D0) THEN
            rmsj=1D0/SQRT(wgtclk(iepo,jsat))
            clkj=clk(iepo,jsat)
          ELSE
            rmsj=0D0
            clkj=0D0
          ENDIF
          IF (wgtcld(iepo,jsat) > 0D0) THEN
            rmsdj=1D0/SQRT(wgtcld(iepo,jsat))
            dclkj=dclk(iepo,jsat)
          ELSE
            rmsdj=0D0
            dclkj=0D0
          ENDIF
          WRITE(lfnloc,'(F15.6,2(I6,2E30.18))') &
                epoch(mepo(iepo,jsat)),numclk(iepo,jsat),clkj,rmsj, &
                numcld(iepo,jsat),dclkj,rmsdj
        ENDDO
      ENDDO
    ENDIF

! Stations
    IF (isel /= 1) THEN
      DO jfil=1,nftot
        IF (iparf(jfil)==0 .OR. jfil==irfsit) CYCLE
        IF (iparf(jfil)==0) CYCLE
        WRITE(lfnloc,"(/,'# ifil=',I3,'   ',A)") jfil,stname(jfil)
        DO iepo=1,iparf(jfil)
          IF (wgtsta(iepo,jfil) > 0D0) THEN
            rmsj=1D0/SQRT(wgtsta(iepo,jfil))
            clkj=stclk(iepo,jfil)
          ELSE
            rmsj=0D0
            clkj=0D0
          ENDIF
          IF (wgtstd(iepo,jfil) > 0D0) THEN
            rmsdj=1D0/SQRT(wgtstd(iepo,jfil))
            dclkj=dstclk(iepo,jfil)
          ELSE
            rmsdj=0D0
            dclkj=0D0
          ENDIF
          WRITE(lfnloc,*)epoch(mepof(iepo,jfil)),clkj,rmsj,dclkj,rmsdj
        ENDDO
      ENDDO
      CLOSE(lfnloc)
    ENDIF
  ENDIF

  RETURN
END SUBROUTINE cewckd

END MODULE
