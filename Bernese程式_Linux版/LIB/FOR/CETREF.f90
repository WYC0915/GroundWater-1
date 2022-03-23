MODULE s_CETREF
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE cetref(irfsat,irfsit,nepo, &
                  svncmb,ipar ,mepo ,clk  ,wgtclk,dclk  ,wgtcld, &
                  nftot ,iparf,mepof,stclk,wgtsta,dstclk,wgtstd)

! -------------------------------------------------------------------------
! Purpose:    Transform clocks to new reference
!
! Remarks:    - Subroutine of CLKEST
!
! Author:     U. Hugentobler
!
! Created:    02-Jan-2002
!
! Changes:    04-Oct-2002 RD: Correct format statement
!             05-Mar-2012 RD: Use listi4 as module now
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,  ONLY: i4b, r8b, lfnerr
  USE s_exitrc
  USE f_listi4
  IMPLICIT NONE

! List of arguments
! -----------------
! Input:
  INTEGER(i4b)                :: irfsat    ! Number of ref. sat, or zero
  INTEGER(i4b)                :: irfsit    ! Number of ref. sta, or zero
                                           ! (either irfsat or irfsit has
                                           !  to be different from zero)
  INTEGER(i4b)                :: nepo      ! Total number of epochs
  INTEGER(i4b)                :: svncmb    ! Number of satellites
! For satellite i=1,...,svcmp:
  INTEGER(i4b),DIMENSION(:)   :: ipar      ! Number of clock parameters
! For epoch i and satellite k=1,...,svcmp:
  INTEGER(i4b),DIMENSION(:,:) :: mepo      ! Epoch indices
! Input/Output:
! For epoch i and satellite k=1,...,svcmp:
  REAL(r8b),DIMENSION(:,:)    :: clk       ! code clock values
  REAL(r8b),DIMENSION(:,:)    :: wgtclk    ! wgt of code clock values
  REAL(r8b),DIMENSION(:,:)    :: dclk      ! phase clock differences
  REAL(r8b),DIMENSION(:,:)    :: wgtcld    ! wgt of phase clock differences
! Input:
  INTEGER(i4b)                :: nftot     ! Number of stations (files)
! For station i=1,...,nftot:
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
  INTEGER(i4b)                :: iref,ipa,jsat,jsta

! Check input
! -----------
  IF (irfsat == 0 .AND. irfsit == 0) THEN
    WRITE(lfnerr,'(/,A,/)') &
    ' *** SR CETREF: Neither reference satellite nor station specified'
    CALL exitrc(2)
  ENDIF

! Transform all satellite clock and clock differences
! ---------------------------------------------------
  DO jsat=1,svncmb
    iref=0
! reference satellite
    IF (irfsat /= 0 .AND. irfsat /= jsat) THEN
      DO ipa=1,ipar(jsat)
        iref=listi4(0,nepo,mepo(iref+1:nepo,irfsat),mepo(ipa,jsat), &
                                               nepo-iref)+iref
        IF (iref==0) THEN
          wgtclk(ipa,jsat)=0D0
          wgtcld(ipa,jsat)=0D0
        ELSE
          clk(ipa,jsat) =clk(ipa,jsat) -clk(iref,irfsat)
          dclk(ipa,jsat)=dclk(ipa,jsat)-dclk(iref,irfsat)
        ENDIF
      ENDDO
! reference station
    ELSEIF (irfsit /= 0) THEN
      DO ipa=1,ipar(jsat)
        iref=listi4(0,nepo,mepof(iref+1:nepo,irfsit),mepo(ipa,jsat), &
                                                nepo-iref)+iref
        IF (iref==0) THEN
          wgtclk(ipa,jsat)=0D0
          wgtcld(ipa,jsat)=0D0
        ELSE
          clk(ipa,jsat) =clk(ipa,jsat) -stclk(iref,irfsit)
          dclk(ipa,jsat)=dclk(ipa,jsat)-dstclk(iref,irfsit)
        ENDIF
      ENDDO
    ENDIF
  ENDDO

! Transform all station clock and clock differences
! -------------------------------------------------
  DO jsta=1,nftot
    iref=0
    IF (irfsat /= 0) THEN
      DO ipa=1,iparf(jsta)
        iref=listi4(0,nepo,mepo(iref+1:nepo,irfsat),mepof(ipa,jsta), &
                                               nepo-iref)+iref
        IF (iref==0) THEN
          wgtsta(ipa,jsta)=0D0
          wgtstd(ipa,jsta)=0D0
        ELSE
          stclk(ipa,jsta) =stclk(ipa,jsta) -clk(iref,irfsat)
          dstclk(ipa,jsta)=dstclk(ipa,jsta)-dclk(iref,irfsat)
        ENDIF
      ENDDO
    ELSEIF (irfsit /= jsta)THEN
      DO ipa=1,iparf(jsta)
        iref=listi4(0,nepo,mepof(iref+1:nepo,irfsit),mepof(ipa,jsta), &
                                                nepo-iref)+iref
        IF (iref==0) THEN
          wgtsta(ipa,jsta)=0D0
          wgtstd(ipa,jsta)=0D0
        ELSE
          stclk(ipa,jsta) =stclk(ipa,jsta) -stclk(iref,irfsit)
          dstclk(ipa,jsta)=dstclk(ipa,jsta)-dstclk(iref,irfsit)
        ENDIF
      ENDDO
    ENDIF
  ENDDO

! Set reference clocks and clock differences to zero
! --------------------------------------------------
  IF (irfsat /= 0) THEN
    DO ipa=1,ipar(irfsat)
      clk(ipa,irfsat) =0D0
      dclk(ipa,irfsat)=0D0
    ENDDO
  ELSE
    DO ipa=1,iparf(irfsit)
      stclk(ipa,irfsit) =0D0
      dstclk(ipa,irfsit)=0D0
    ENDDO
  ENDIF

  RETURN
END SUBROUTINE CETREF

END MODULE
