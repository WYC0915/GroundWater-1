MODULE s_PRE_SCREEN
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE pre_screen(iTyp,nSat,beta,bobs,rms,obsflg,iepo)

!--------------------------------------------------------------------------
! Purpose:   Screen nSat observations with the same expectation value
!            with expected rms of "rms". Do not consider observations
!            with flags "obsflg /= ' '.
!            If an observation is considered as o.k., set obsflg='+'
!            If an observation is considered as bad,  set obsflg='-'
!            When in doubt, do not modify flag
!
! Author:    G.Beutler
!
! Created:   16-JAN-2001
!
! Changes:   01-MAR-2001: HB: modified algorithm to find good
!                             observation, set no flag '+'
!            07-jan-2002: HB: Dynamic allocation of memory
!            13-nov-2002: HB: New parameter beta
!            14-Apr-2005: HU: Use interface to ALCERR
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
!
!-------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnprt
  USE s_alcerr
  IMPLICIT NONE

! List of Parameters
! ------------------
! IN:
  INTEGER(i4b)                  :: iTyp    ! =1: code; =2: phase
  INTEGER(i4b)                  :: nSat    ! number of satellites
  INTEGER(i4b)                  :: beta    ! factor for screening threshold
  REAL(r8b),DIMENSION(*)        :: bobs    ! observed - computed
  REAL(r8b)                     :: rms     ! rms of observation

! IN/OUT:
  CHARACTER(LEN=1),DIMENSION(*) :: obsflg  ! Marks for observations

  INTEGER(i4b) :: iEpo

! List of Functions
! -----------------
! Local Types
! -----------
! Local Parameters
! ----------------

! Local Variables
! ---------------
  INTEGER(i4b),DIMENSION(:),ALLOCATABLE :: obsFlg_int
  INTEGER(i4b),DIMENSION(:),ALLOCATABLE :: nFlg
  INTEGER(i4b)                 :: iSat,kSat,jSat
  INTEGER(i4b)                 :: nseff
  INTEGER(i4b)                 :: ii,iMax,iFlg,iac

  REAL(r8b)                    :: test,obs_mean,rms_obs
  REAL(r8b)                    :: diff

! Allocate memory
! ---------------
  ALLOCATE(obsFlg_int(nsat),stat=iac)
  CALL alcErr(iac, 'obsFlg_int', (/nsat/), 'pre_screen')
  ALLOCATE(nFlg(nsat),stat=iac)
  CALL alcErr(iac, 'nFlg', (/nsat/), 'pre_screen')

! Detect good observations
! ------------------------
  obsflg_int(1:nSat) = 0
  iFlg = 0
  nFlg(1:nSat) = 0
  DO iSat = 1,nSat
    IF(obsflg(iSat) /=' ')CYCLE
    DO kSat = iSat+1,nSat
      IF(obsflg(kSat) /=' ')CYCLE
      test = dabs(bobs(iSat)-bobs(kSat))
      IF(test <= 3*rms)THEN
        IF (obsflg_int(iSat) == 0.and.obsflg_int(kSat) == 0) THEN
          iFlg = iFlg+1
          obsflg_int(iSat) = iFlg
          obsflg_int(kSat) = iFlg
        ELSEIF (obsflg_int(iSat) == 0.and.obsflg_int(kSat) /= 0) THEN
          obsflg_int(iSat)= obsflg_int(kSat)
        ELSEIF (obsflg_int(iSat) /= 0.and.obsflg_int(kSat) == 0) THEN
          obsflg_int(kSat) = obsflg_int(iSat)
        ELSEIF (obsflg_int(iSat) /= 0.and.obsflg_int(kSat) /= 0.AND.&
             obsflg_int(iSat) /= obsflg_int(kSat)) THEN
         IF (obsflg_int(iSat) < obsflg_int(kSat)) THEN
            DO jSat = 1,nSat
              IF (obsflg_int(jSat) == obsflg_int(kSat).AND.jSat /= kSat) THEN
                obsflg_int(jSat) = obsflg_int(iSat)
              ENDIF
            ENDDO
            obsflg_int(kSat) = obsflg_int(iSat)
          ELSEIF (obsflg_int(iSat) > obsflg_int(kSat)) THEN
            DO jSat = 1,nSat
              IF (obsflg_int(jSat) == obsflg_int(iSat).AND.jSat /= iSat) THEN
                obsflg_int(jSat) = obsflg_int(kSat)
              ENDIF
            ENDDO
            obsflg_int(iSat) = obsflg_int(kSat)
          ENDIF
        ENDIF
      ENDIF
    ENDDO
  ENDDO

! Look for index with the most observations
! ----------------------------------------
  nseff = 0
  obs_mean = 0.d0
  rms_obs = -1.D0
  test = 0.D0
  DO ii = 1,iFlg
    DO iSat = 1,nSat
      IF (obsflg_int(iSat) == ii) nFlg(ii) = nFlg(ii)+1
    ENDDO
    IF (ii == 1) iMax = ii
    IF (ii /= 1.AND.nFlg(ii) > nFlg(iMax)) iMax = ii
  ENDDO
  IF (iFlg /= 0) THEN
    IF (nFlg(iMax) <= 2) THEN
      DO iSat = 1,nSat
        IF (obsFlg_int(iSat) /= 0) THEN
          obsflg(iSat) = ' '
        ELSE
          obsflg(iSat) = '-'
        ENDIF
      ENDDO
      GOTO 999
    ENDIF
  ELSE
    GOTO 999
  ENDIF

! Count good observations, compute mean value
! -------------------------------------------
  nseff = 0
  obs_mean = 0.d0
  rms_obs = 0.d0

  DO iSat = 1,nSat
    IF(obsflg_int(iSat) == iMax)THEN
      nseff = nseff+1
      obs_mean = obs_mean+bobs(iSat)
!!      rms_obs = rms_obs+bobs(iSat)**2
    ENDIF
  ENDDO
  IF(nseff >= 2)THEN
    obs_mean = obs_mean/nseff
    DO iSat = 1,nSat
      IF(obsflg_int(iSat) == iMax)THEN
        diff = (obs_mean-bobs(isat))**2
        rms_obs=rms_obs+diff
      ENDIF
    ENDDO
    rms_obs = dsqrt(rms_obs/(nseff-1))
  ELSE
    GO TO 999
  ENDIF

! Set final flags
! ---------------
  DO iSat = 1,nSat
    IF(obsflg(iSat) /= ' ') CYCLE
    test = dabs(bobs(iSat)-obs_mean)
    IF (iTyp <= 3) THEN
      IF(test <= beta* rms_obs)THEN
        obsflg(iSat) = ' '
      ELSEIF(test > beta*rms_obs)THEN
        obsflg(iSat) = '-'
      ENDIF
    ELSEIF (iTyp == 4) THEN
      IF(test <= 3* rms_obs)THEN
        obsflg(iSat) = '+'
      ELSEIF(test <= beta* rms_obs)THEN
        obsflg(iSat) = ' '
      ELSEIF(test > beta*rms_obs)THEN
        obsflg(iSat) = '-'
      ENDIF
    ENDIF
!!    IF(iTyp==1)write(lfnprt,'(I6,2F18.7,A1,I2,2F18.7)')iepo,test,bobs(iSat),&
!!         obsflg(iSat),obsflg_int(iSat),rms_obs,test/rms_obs
!!    IF(iTyp==2.AND.iepo<10)write(*,*)iepo,test/rms_obs
!!    IF (iTyp==1)write(lfnprt,*)iSat,test/rms_obs,obsflg(iSat)
!!    write(lfnprt,*)iepo,iTyp,test/rms_obs
  ENDDO
999 CONTINUE
!!  write(lfnprt,*)iepo,iTyp,rms_obs,nseff,nsat
  DEALLOCATE(obsFlg_int,nFlg)
  RETURN
END SUBROUTINE pre_screen

END MODULE
