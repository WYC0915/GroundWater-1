MODULE s_ESTWGT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE estwgt(edt,weight,wgtCls,staWgt)

! -------------------------------------------------------------------------
! Purpose:    Estimates station weights in RESRMS
!
! Author:     R. Dach
!
! Created:    20-Jan-2003
!
! Changes:    16-May-2003 HU: Deallocate array
!             21-May-2003 RD: Make the deallocation safe
!             10-Jul-2012 RD: Use SYMINVG instead of SYMING
!             10-Jul-2012 RD: Use m_bern with ONLY
!             30-Jul-2012 RD: SR STATIS with optional arguments
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b
  USE d_edit,   ONLY: t_edit
  USE d_stawgt, ONLY: t_stawgt
  USE f_ikf
  USE s_alcerr
  USE s_syminvg
  USE s_solve
  USE s_statis
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  TYPE(t_edit)                      :: edt      ! Edit information
                                                ! (got from residual files)
  REAL(r8b), DIMENSION(:)           :: weight   ! baseline/station weights
  REAL(r8b), DIMENSION(:,:),POINTER :: wgtCls   ! Class definition for
                                                ! observation sigmas
                                                ! (1,i): sigma factor
                                                ! (2,i): measurement noise

! output:
  TYPE(t_stawgt)                    :: stawgt   ! Station sigma factors

! Local Parameters
! ----------------
  CHARACTER(LEN=6),             PARAMETER   :: srName = 'estwgt'

! Local Variables
! ---------------
  INTEGER(i4b), DIMENSION(:,:), ALLOCATABLE :: staIdx
  INTEGER(i4b), DIMENSION(:),   ALLOCATABLE :: staClu
  INTEGER(i4b)                              :: iEdt,jEdt
  INTEGER(i4b)                              :: iWgt
  INTEGER(i4b)                              :: mWgt
  INTEGER(i4b)                              :: iSta
  INTEGER(i4b)                              :: i1,i2
  INTEGER(i4b)                              :: nSing
  INTEGER(i4b), DIMENSION(:),   ALLOCATABLE :: parFlg
  INTEGER(i4b)                              :: irc

  REAL(r8b),    DIMENSION(:),   ALLOCATABLE :: wgtVal
  REAL(r8b)                                 :: wgtMed
  REAL(r8b),    DIMENSION(:),   ALLOCATABLE :: ANOR
  REAL(r8b),    DIMENSION(:),   ALLOCATABLE :: BNOR


! Allocate arrays
! ---------------
  ALLOCATE(staIdx(2,edt%nEdFil),stat=irc)
  CALL alcerr(irc,'staIdx',(/2,edt%nEdFil/),srName)

  IF (ASSOCIATED(staWgt%wgt)) &
    DEALLOCATE(staWgt%wgt,stat=irc)
  ALLOCATE(staWgt%wgt(2*edt%nEdFil),stat=irc)
  CALL alcerr(irc,'staWgt%wgt',(/2*edt%nEdFil/),srName)

! Get the station weighting list
! ------------------------------
  staWgt%nWgt = 0

  ! Loop all edit header files
  DO iEdt = 1,edt%nEdFil

    staIdx(:,iEdt) = 0

    ! There is a weight for the baseline/station availabel
    IF (weight(iEdt) == 0d0) CYCLE

    ! Both stations
    DO jEdt = 1,2

      ! Zero difference case
      IF (jEdt == 2 .AND. LEN_TRIM(edt%head(iEdt)%staEdt(2)) == 0) CYCLE

      ! Is the station (mea-type) already in list?
      DO iWgt = 1,staWgt%nWgt

        IF (staWgt%wgt(iWgt)%staNam == edt%head(iEdt)%staEdt(jEdt) .AND. &
            staWgt%wgt(iWgt)%meaTyp == edt%head(iEdt)%meaEdt)      THEN
          staIdx(jEdt,iEdt) = iWgt
          EXIT
        ENDIF

      ENDDO

      IF (staIdx(jEdt,iEdt) /= 0) CYCLE

      ! Add a new station weight item to the list
      staWgt%nWgt = staWgt%nWgt+1

      staWgt%wgt(staWgt%nWgt)%staNam = edt%head(iEdt)%staEdt(jEdt)
      staWgt%wgt(staWgt%nWgt)%meaTyp = edt%head(iEdt)%meaEdt
      staWgt%wgt(staWgt%nWgt)%timWin%t(:) = (/ 0d0,1d20 /)

      staIdx(jEdt,iEdt) = staWgt%nWgt

    ENDDO
  ENDDO


! Allocate and compute ANOR und BNOR
! ----------------------------------
  ALLOCATE(ANOR(ikf(staWgt%nWgt,staWgt%nWgt)),stat=irc)
  CALL alcerr(irc,'ANOR',(/ikf(staWgt%nWgt,staWgt%nWgt)/),srName)

  ALLOCATE(BNOR(staWgt%nWgt),stat=irc)
  CALL alcerr(irc,'BNOR',(/staWgt%nWgt/),srName)

  ALLOCATE(parFlg(staWgt%nWgt),stat=irc)
  CALL alcerr(irc,'parFlg',(/staWgt%nWgt/),srName)

  ANOR = 0d0
  BNOR = 0d0

  DO iEdt = 1,edt%nEdFil

    ! There is a weight for the baseline/station availabel
    IF (weight(iEdt) == 0d0) CYCLE

    i1 = staIdx(1,iEdt)
    i2 = staIdx(2,iEdt)

    ANOR(ikf(i1,i1)) = ANOR(ikf(i1,i1)) + 1d0
    BNOR(i1) = BNOR(i1) + weight(iEdt)**2

    IF (i2 /= 0) THEN
      ANOR(ikf(i1,i2)) = ANOR(ikf(i1,i2)) + 1d0
      ANOR(ikf(i2,i2)) = ANOR(ikf(i2,i2)) + 1d0
      BNOR(i2) = BNOR(i2) + weight(iEdt)**2
    ENDIF

  ENDDO


! Get all independent baseline clusters
! -------------------------------------
  ALLOCATE(staClu(staWgt%nWgt),stat=irc)
  CALL alcerr(irc,'staClu',(/staWgt%nWgt/),srName)

  staClu(:) = (/ (iSta , iSta=1,staWgt%nWgt) /)

  ! Get all baselines
  DO iEdt = 1,edt%nEdFil

    ! There is a weight for the baseline/station availabel
    IF (weight(iEdt) == 0d0) CYCLE

    IF (staIdx(1,iEdt) == 0 .OR. staIdx(2,iEdt) == 0) CYCLE

    i1 = staClu(staIdx(1,iEdt))
    i2 = staClu(staIdx(2,iEdt))

    ! Change cluster number
    DO iSta = 1,staWgt%nWgt
      IF (staClu(iSta) == i2) staClu(iSta) = i1
    ENDDO
  ENDDO

! The sigma of both station is equal
! for the baseline with the smallest noise in the cluster
! -------------------------------------------------------
  ALLOCATE(wgtVal(edt%nEdFil),stat=irc)
  CALL alcerr(irc,'wgtVal',(/edt%nEdFil/), srName)

  DO iSta = 1,staWgt%nWgt

    jEdt = 0
    DO iEdt = 1,edt%nEdFil

      IF (staIdx(1,iEdt) == 0) CYCLE
      IF (staClu(staIdx(1,iEdt)) /= iSta) CYCLE

      ! There is a weight for the baseline/station availabel
      IF (weight(iEdt) == 0d0) CYCLE


      ! A zero difference residual
      IF (staIdx(2,iEdt) == 0) CYCLE

      jEdt = jEdt+1
      wgtVal(jEdt) = weight(iEdt)

    ENDDO

    ! Stations have the same sigmas
    IF (jEdt > 0) THEN

      CALL statis(jEdt,wgtVal,xMed=wgtMed)

      DO iEdt = 1,edt%nEdFil

        IF (staIdx(1,iEdt) == 0) CYCLE
        IF (staClu(staIdx(1,iEdt)) /= iSta) CYCLE

        ! There is a weight for the baseline/station availabel
        IF (weight(iEdt) == 0d0) CYCLE

        ! A zero difference residual
        IF (staIdx(2,iEdt) == 0) CYCLE

        IF (jEdt == 1 .OR. weight(iEdt) <= 1.5d0*wgtMed) THEN

          i1 = staIdx(1,iEdt)
          i2 = staIdx(2,iEdt)

          ANOR(ikf(i1,i2)) = ANOR(ikf(i1,i2)) - 1d0

          BNOR(i1) = BNOR(i1) - 0.5d0*weight(iEdt)**2
          BNOR(i2) = BNOR(i2) - 0.5d0*weight(iEdt)**2

        ENDIF

      ENDDO

    ENDIF
  ENDDO

  DEALLOCATE(wgtVal,stat=irc)

! Invert and solve
! ----------------
  CALL syminvg(staWgt%nWgt,ANOR,0,nSing,parFlg)

  CALL solve(staWgt%nWgt,ANOR,BNOR,staWgt%wgt(:)%weight)


! Get the corresponding sigma factor
! ----------------------------------
  DO iSta = 1,staWgt%nWgt

    mWgt = SIZE(wgtCls,2)
    DO iWgt = SIZE(wgtCls,2),2,-1
      IF (wgtCls(2,iWgt)*1000D0 > DSQRT(staWgt%wgt(iSta)%weight)) mWgt = iWgt-1
    ENDDO

    staWgt%wgt(iSta)%weight = wgtCls(1,mWgt)

  ENDDO


! Deallocate memory
! -----------------
  DEALLOCATE(ANOR,stat=irc)
  DEALLOCATE(BNOR,stat=irc)
  DEALLOCATE(parFlg,stat=irc)

  DEALLOCATE(staIdx,stat=irc)
  DEALLOCATE(staClu,stat=irc)

  RETURN
END SUBROUTINE estwgt

END MODULE
