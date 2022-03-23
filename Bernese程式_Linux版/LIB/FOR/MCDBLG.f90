MODULE s_MCDBLG
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE mcdblg(opt,nFil,filLst,filInfo,nClu,cluList,nDel,delSta)

! -------------------------------------------------------------------------
! Purpose:    Read all observation files, check the min. number of obs.
!             for each satellite (global clusters)
!
! Author:     R. Dach
!
! Created:    13-Jun-2002
!
! Changes:    21-Nov-2002 RD: Improved logic for satellites with few observ.
!             23-Apr-2003 AJ: Nullify local pointers
!             29-Oct-2003 RD: Consider already deleted stations
!             15-Dec-2005 RD: No redundancy for badly observed satellites
!             06-Dec-2006 RD: Completely ignore badly observed satellites
!             28-Apr-2012 RD: Nullify all pointers, use m_bern with only
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, fileNameLength
  USE d_gpsobs, ONLY: t_obsepo
  USE p_mkclus, ONLY: t_mkclus_opt, t_cluster, t_staFil
  USE s_alcerr
  USE f_tstflg
  USE s_mcobsb
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  TYPE(t_mkclus_opt)                   :: opt      ! Program input options
  INTEGER(i4b)                         :: nFil     ! Number of files
  CHARACTER(LEN=fileNameLength),        &
               DIMENSION(:,:), POINTER :: filLst   ! Observation files
  TYPE(t_staFil),   DIMENSION(:),  POINTER :: filInfo  ! File information

! input/output
  INTEGER(i4b)                         :: nClu     ! Number of files to
                                                   ! put into the clusters
  TYPE(t_cluster),DIMENSION(:),POINTER :: cluList  ! Cluster record
  INTEGER(i4b)                         :: nDel     ! Number of stations to
                                                   ! be deleted from clusters
  TYPE(t_cluster),DIMENSION(:),POINTER :: delSta   ! Station deletion list

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER          :: srName  = 'mcdblg'
  INTEGER(i4b)                         :: maxIter = 10

! Local Variables
! ---------------
  TYPE(t_obsepo), DIMENSION(:),     POINTER     :: obsEpo

  INTEGER(i4b)                                  :: nSat
  INTEGER(i4b),   DIMENSION(:),     POINTER     :: allSat
  INTEGER(i4b),   DIMENSION(:),     ALLOCATABLE :: satSum
  INTEGER(i4b),   DIMENSION(:,:),   ALLOCATABLE :: satLst
  INTEGER(i4b),   DIMENSION(:,:),   ALLOCATABLE :: staLst
  INTEGER(i4b),   DIMENSION(:,:,:), ALLOCATABLE :: dblNxt
  INTEGER(i4b),   DIMENSION(:),     ALLOCATABLE :: cluSum
  INTEGER(i4b),   DIMENSION(:),     ALLOCATABLE :: cluIdx
  INTEGER(i4b)                                  :: jIter
  INTEGER(i4b)                                  :: nSolve
  INTEGER(i4b),   DIMENSION(3)                  :: mSolve
  INTEGER(i4b)                                  :: iFil
  INTEGER(i4b)                                  :: iSat,jSat
  INTEGER(i4b)                                  :: iObs
  INTEGER(i4b)                                  :: iSta
  INTEGER(i4b)                                  :: mSta
  INTEGER(i4b)                                  :: iClu,jClu
  INTEGER(i4b)                                  :: mClu
  INTEGER(i4b)                                  :: iDel
  INTEGER(i4b)                                  :: irc

  REAL(r8b),      DIMENSION(:),     POINTER     :: satobs

  LOGICAL                                       :: sorted

  NULLIFY(obsEpo)
  NULLIFY(allSat)
  NULLIFY(satObs)

! Check cluster type
! ------------------
  IF (opt%cluStrat /= 1) RETURN

! Nothing to do
! -------------
  IF (opt%satObs < 1) RETURN

! Init GOBSEP
! -----------
  CALL mcobsb(1,opt,nFil,filLst,nSat,allSat,satobs,obsEpo,irc)

! Allocate Observation statistics
! -------------------------------

  ! Total number of obs. per satellite
  ALLOCATE(satSum(nSat),stat=irc)
  CALL alcerr(irc,'satSum',(/nSat/),srName)

  ! Actual clustering
  ALLOCATE(staLst(nFil,opt%numClu),stat=irc)
  CALL alcerr(irc,'staLst',(/nFil,opt%numClu/),srName)

  staLst = 0
  DO iSta = 1,nClu
    IF (cluList(iSta)%cluster == 0) CYCLE
    staLst(cluList(iSta)%filIdx,cluList(iSta)%cluster) = 1
  ENDDO

  ! Number of stations in each cluster
  ALLOCATE(cluSum(opt%numClu),stat=irc)
  CALL alcerr(irc,'cluSum',(/opt%numClu/),srName)

  cluSum = 0
  DO iClu = 1,opt%numClu
    DO iSta = 1,nFil
      IF (filInfo(iSta)%cluFlg < 0) CYCLE
      IF (staLst(iSta,iClu) /= 0) cluSum(iClu) = cluSum(iClu)+1
    ENDDO
  ENDDO

  ! Index to order the clusters
  ALLOCATE(cluIdx(opt%numClu),stat=irc)
  CALL alcerr(irc,'cluIdx',(/opt%numClu/),srName)

  cluIdx = (/ (iClu,iClu=1,opt%numClu) /)

  ! Satellite statistic
  ALLOCATE(satLst(nSat,opt%numClu),stat=irc)
  CALL alcerr(irc,'satLst',(/nSat,opt%numClu/),srName)

  satLst = 0

  ! Duplication statistic
  ALLOCATE(dblNxt(nFil,opt%numClu,opt%satObs),stat=irc)
  CALL alcerr(irc,'dblNxt',(/nFil,opt%numClu,opt%satObs/),srName)

  dblNxt = 0

! Loop until no more stations to duplicate
! ----------------------------------------
  mSta = 1
  jIter = 0
  DO WHILE (mSta /= 0 .AND. jIter < maxIter)
    jIter = jIter+1

! Remove deletion flags
! ---------------------
    DO iSta = 1,nFil
      IF (filInfo(iSta)%cluFlg < 0) CYCLE
      DO iClu = 1,opt%numClu
        IF (staLst(iSta,iClu) > 1) staLst(iSta,iClu) = 1
      ENDDO
    ENDDO

! Init the epoch loop
! -------------------
    dblNxt = 0

    CALL mcobsb(2,opt,nFil,filLst,nSat,allSat,satobs,obsEpo,irc)

! Loop all epochs
! ---------------
    irc = 0
    DO WHILE (irc == 0)

      CALL mcobsb(0,opt,nFil,filLst,nSat,allSat,satobs,obsEpo,irc)

      IF (irc /= 0) EXIT

! Count the number of stations observing each satellite
! -----------------------------------------------------
      satLst = 0

      ! Loop all stations / baselines in all clusters
      DO iClu = 1,opt%numClu
        DO iFil = 1,nFil
          IF (filInfo(iFil)%cluFlg < 0) CYCLE

          ! Station / baseline is not in this cluster
          IF (staLst(iFil,iClu) == 0) CYCLE

          ! Loop all satellites observed by the station
          DO iSat = 1,obsEpo(iFil)%nSat

            ! Is it a valid observation?
            IF (obsEpo(iFil)%obsrec(iSat)%numSat        == 0  ) CYCLE
            IF (tstflg(obsEpo(iFil)%obsrec(iSat)%obsflg(1),0) ) CYCLE
            IF (obsEpo(iFil)%obsrec(iSat)%observ(1)     == 0d0) CYCLE

            ! Search the index of the satellite
            DO jSat = 1,nSat
              IF (allSat(jSat) == obsEpo(iFil)%obsrec(iSat)%numSat) &
                satLst(jSat,iClu) = satLst(jSat,iClu) + 1
            ENDDO ! jSat
          ENDDO ! iSat
        ENDDO ! iFil
      ENDDO ! iClu


! Count total number of observations per satellite and epoch
! ----------------------------------------------------------
      satSum=0

      ! Loop all stations / baselines in all clusters
      DO iFil = 1,nFil
        IF (filInfo(iFil)%cluFlg < 0) CYCLE

        DO iClu = 1,opt%numClu
          IF (staLst(iFil,iClu) /= 0) THEN

            ! Loop all satellites observed by the station
            DO iSat = 1,obsEpo(iFil)%nSat

              ! Is it a valid observation?
              IF (obsEpo(iFil)%obsrec(iSat)%numSat        == 0  ) CYCLE
              IF (tstflg(obsEpo(iFil)%obsrec(iSat)%obsflg(1),0) ) CYCLE
              IF (obsEpo(iFil)%obsrec(iSat)%observ(1)     == 0d0) CYCLE

              ! Search the index of the satellite
              DO jSat = 1,nSat
                IF (allSat(jSat) == obsEpo(iFil)%obsrec(iSat)%numSat) &
                  satSum(jSat) = satSum(jSat) + 1
              ENDDO ! jSat
            ENDDO ! iSat

            EXIT ! Each file only once
          ENDIF
        ENDDO ! iClu
      ENDDO ! iFil

! Stations should not be deleted because of satObs
! ------------------------------------------------
      DO iSat = 1,nSat

        ! Ignore badly observed satellites
        IF (satObs(iSat) == 0d0) CYCLE

        ! Count the number of critical satellite numbers per cluster
        mSolve = 0

        DO iClu = 1,opt%numClu

          ! Clusters with more than opt%satObs observations
          IF (satLst(iSat,iClu) >  opt%satObs) mSolve(1) = mSolve(1)+1

          ! Clusters with more or equal than opt%satObs observations
          IF (satLst(iSat,iClu) >= opt%satObs) mSolve(2) = mSolve(2)+1

          ! Cluster where less than opt%satObs observations available
          IF (satLst(iSat,iClu) <  opt%satObs .AND. &
              satLst(iSat,iClu) <= satSum(iSat)) mSolve(3) = mSolve(3)+1

        ENDDO

        IF (mSolve(1) >= opt%nSolve) CYCLE
        IF (mSolve(2) >  opt%nSolve) CYCLE
        IF (mSolve(1) <  opt%nSolve .AND. mSolve(2) < opt%nSolve .AND. &
            mSolve(3) >  opt%nSolve) CYCLE
        IF (satObs(iSat) == 0d0) THEN
          IF (mSolve(1) == 1 .OR.  mSolve(2) == 1) CYCLE
          IF (mSolve(1) == 0 .AND. mSolve(2) == 0 .AND. mSolve(3) == 1) CYCLE
        ENDIF

! Loop all clusters
! -----------------
        DO iClu = 1,opt%numClu

          IF (satLst(iSat,iClu) /= opt%satObs .AND. &
              satLst(iSat,iClu) /= satSum(iSat)) CYCLE

          DO iFil = 1,nFil
            IF (filInfo(iFil)%cluFlg < 0) CYCLE

            IF (staLst(iFil,iClu) == 0) CYCLE

            ! Loop all satellites observed by the station
            DO jSat = 1,obsEpo(iFil)%nSat

              ! Is it a valid observation?
              IF (obsEpo(iFil)%obsrec(jSat)%numSat /= allSat(iSat)) CYCLE
              IF (tstflg(obsEpo(iFil)%obsrec(jSat)%obsflg(1),0)   ) CYCLE
              IF (obsEpo(iFil)%obsrec(jSat)%observ(1)     == 0d0  ) CYCLE

              staLst(iFil,iClu) = 3

            ENDDO ! jSat
          ENDDO ! iFil
        ENDDO ! iClu
      ENDDO ! iSat


! Indicate missing observations for global clusters
! -------------------------------------------------
      DO iSat = 1,nSat

        ! Count the number of critical satellite numbers per cluster
        nSolve = opt%numClu

        DO iClu = 1,opt%numClu
          IF (satLst(iSat,iClu) < opt%satObs .and. &
              satLst(iSat,iClu) < satSum(iSat)) &
            nSolve = nSolve-1
        ENDDO

        IF (nSolve >= opt%nSolve) CYCLE

        DO iClu = 1,opt%numClu

          IF (satLst(iSat,iClu) >= opt%satObs) CYCLE
          IF (satLst(iSat,iClu) >= satSum(iSat)) CYCLE

          DO iFil = 1,nFil
            IF (filInfo(iFil)%cluFlg < 0) CYCLE

            IF (staLst(iFil,iClu) /= 0) CYCLE

            ! Loop all satellites observed by the station
            DO jSat = 1,obsEpo(iFil)%nSat

              ! Is it a valid observation?
              IF (obsEpo(iFil)%obsrec(jSat)%numSat /= allSat(iSat)) CYCLE
              IF (tstflg(obsEpo(iFil)%obsrec(jSat)%obsflg(1),0)   ) CYCLE
              IF (obsEpo(iFil)%obsrec(jSat)%observ(1)     == 0d0  ) CYCLE

              iObs = opt%satObs-satLst(iSat,iClu)
              IF (opt%satObs>satSum(iSat)) &
                iObs = satSum(iSat)-satLst(iSat,iClu)
              dblNxt(iFil,iClu,iObs) = dblNxt(iFil,iClu,iObs)+1

              EXIT

            ENDDO ! jSat
          ENDDO ! iFil
        ENDDO ! iClu
      ENDDO ! iSat

    ENDDO  ! next epoch

! Start duplication
! -----------------
    mSta = 0

! Order the cluster, start searching for stations to duplicate in the
! cluster with the smallest number of stations
! -------------------------------------------------------------------
    IF (opt%numClu > 1) THEN
      cluIdx = (/ (iClu,iClu=1,opt%numClu) /)

      sorted = .FALSE.
      DO WHILE (.NOT. sorted)
        sorted = .TRUE.
        DO iClu = 1,opt%numClu-1
          IF (cluSum(cluIdx(iClu)) > cluSum(cluIdx(iClu+1))) THEN
            jClu = cluIdx(iCLu)
            cluIdx(iClu) = cluIdx(iClu+1)
            cluIdx(iClu+1) = jClu

            sorted = .FALSE.
          ENDIF
        ENDDO
      ENDDO
    ENDIF


! Duplicate the station in the cluster which can "help" in the most
! cases, start with cluster containing the smallest number of stations
! Do not add stations to clusters where they are removed in a previous iteration
! --------------------------------------------------------------------
    IF (opt%numClu > 1) THEN
      DO iObs = 1,opt%satObs
        IF (mSta /= 0) EXIT

        DO iClu = 1,opt%numClu

          iFilLoop: DO iFil = 1,nFil
            IF (filInfo(iFil)%cluFlg < 0) CYCLE
            IF (dblNxt(iFil,cluIdx(iClu),iObs) == 0) CYCLE

            DO iDel = 1,nDel
              IF (delSta(iDel)%filIdx  == iFil .AND. &
                  delSta(iDel)%cluster == cluIdx(iClu)) CYCLE iFilLoop
            ENDDO

            IF (mSta == 0) THEN
              mSta = iFil
              mClu = cluIdx(iClu)
            ELSE IF (dblNxt(iFil,cluIdx(iClu),iObs) > &
                     dblNxt(mSta,mClu,iObs)) THEN
              mSta = iFil
              mClu = cluIdx(iClu)
            ENDIF
          ENDDO iFilLoop
        ENDDO
      ENDDO
    ENDIF

! A station found for duplication
! -------------------------------
    IF (mSta /= 0) THEN
      staLst(mSta,mClu) = 2
      cluSum(mClu)=cluSum(mClu)+1

! Remove stations which are not necessary in the original cluster
! because of duplication
! ---------------------------------------------------------------
    ELSE IF (opt%numClu > 1) THEN
      DO iSta = 1,nFil
        IF (filInfo(iSta)%cluFlg < 0) CYCLE
        DO iClu = 1,opt%numClu
          IF (staLst(iSta,iClu) == 1) THEN
            DO jClu = 1,opt%numClu
              IF (iClu == jClu) CYCLE
              IF (staLst(iSta,jClu) == 2 .OR. &
                  staLst(iSta,jClu) == 3) THEN
                staLst(iSta,iClu) = 0
                staLst(iSta,jClu) = 3
                cluSum(iClu) = cluSum(iClu)-1
                mSta = iSta
                EXIT
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO
    ENDIF

  ENDDO ! Next iteration,
        ! start reading all observations with the new cluster configuration

! Close observation files, deallocate all "GOBSEP"-arrays
! -------------------------------------------------------
  CALL mcobsb(-1,opt,nFil,filLst,nSat,allSat,satobs,obsEpo,irc)

! Copy the new cluster definition into the cluster record
! -------------------------------------------------------
  DEALLOCATE(cluList,stat=irc)

  nClu = 0
  DO iClu = 1,opt%numClu
    nClu = nClu+cluSum(iClu)
  ENDDO

  ALLOCATE(cluList(nClu),stat=irc)
  CALL alcerr(irc,'cluList',(/nClu/),srName)

  nClu = 0
  DO iFil = 1,nFil
    IF (filInfo(iFil)%cluFlg < 0) CYCLE
    DO iClu = 1,opt%numClu
      IF (staLst(iFil,iClu) == 0) CYCLE

      nClu = nClu+1

      cluList(nClu)%filIdx  = iFil
      cluList(nClu)%cluster = iClu
      cluList(nClu)%delFlg  = 0
      IF (staLst(iFil,iClu) > 1) &
        cluList(nClu)%delFlg  = staLst(iFil,iClu)

    ENDDO
  ENDDO

! Deallocate all arrays
! ---------------------
  DEALLOCATE(satSum,stat=irc)
  DEALLOCATE(staLst,stat=irc)
  DEALLOCATE(dblNxt,stat=irc)
  DEALLOCATE(cluSum,stat=irc)
  DEALLOCATE(cluIdx,stat=irc)
  DEALLOCATE(obsEpo,stat=irc)
  DEALLOCATE(allSat,stat=irc)

  RETURN
END SUBROUTINE mcdblg

END MODULE
