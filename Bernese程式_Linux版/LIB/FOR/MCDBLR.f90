MODULE s_MCDBLR
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE mcdblr(opt,nSta,station,nFil,filLst,filInfo,nClu,cluList)

! -------------------------------------------------------------------------
! Purpose:    Read all observation files, check the min. number of obs.
!             for each satellite (regional clusters)
!
! Author:     R. Dach
!
! Created:    13-Jun-2002
!
! Changes:    23-Apr-2003 AJ: Nullify local pointers
!             15-Dec-2005 RD: New call of SR mcobsb
!             28-Apr-2012 RD: Nullify all pointers, use m_bern with only
!             28-Apr-2012 RD: Remove unused variables
!             20-Sep-2012 RD: Correctly deallocate arrays
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, fileNameLength
  USE d_gpsobs, ONLY: t_obsepo
  USE p_mkclus, ONLY: t_mkclus_opt,t_staFil,t_cluster,t_station
  USE f_ikf
  USE s_alcerr
  USE f_tstflg
  USE s_mcdist
  USE s_mcobsb
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  TYPE(t_mkclus_opt)                   :: opt      ! Program input options
  INTEGER(i4b)                         :: nFil     ! Number of files
  CHARACTER(LEN=fileNameLength),        &
               DIMENSION(:,:), POINTER :: filLst   ! Observation files
  TYPE(t_staFil),DIMENSION(:), POINTER :: filInfo  ! File information
  integer(i4b)::nSta
  type(t_station),DIMENSION(:),POINTER::station

! input/output
  INTEGER(i4b)                         :: nClu     ! Number of files to
                                                   ! put into the clusters
  TYPE(t_cluster),DIMENSION(:),POINTER :: cluList  ! Cluster record

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER          :: srName  = 'mcdblr'
  INTEGER(i4b)                         :: maxIter = 15

! Local Variables
! ---------------
  TYPE(t_obsepo), DIMENSION(:),     POINTER     :: obsEpo

  INTEGER(i4b)                                  :: nSat
  INTEGER(i4b),   DIMENSION(:),     POINTER     :: allSat
  INTEGER(i4b),   DIMENSION(:,:),   ALLOCATABLE :: satLst
  INTEGER(i4b),   DIMENSION(:,:),   ALLOCATABLE :: staLst
  INTEGER(i4b),   DIMENSION(:,:,:), ALLOCATABLE :: dblNxt
  INTEGER(i4b),   DIMENSION(:),     ALLOCATABLE :: cluSum
  INTEGER(i4b)                                  :: iIter,jIter
  INTEGER(i4b)                                  :: nSolve
  INTEGER(i4b)                                  :: iFil,jFil,kFil
  INTEGER(i4b)                                  :: iSat,jSat
  INTEGER(i4b)                                  :: iSta,jSta
  INTEGER(i4b)                                  :: mSta
  INTEGER(i4b)                                  :: iClu,jClu
  INTEGER(i4b)                                  :: mClu
  INTEGER(i4b)                                  :: irc

  REAL(r8b),      DIMENSION(:),     POINTER     :: dist
  REAL(r8b)                                     :: distClu
  REAL(r8b)                                     :: distMin
  REAL(r8b),      DIMENSION(:),     POINTER     :: satobs

  NULLIFY(obsEpo)
  NULLIFY(allSat)
  NULLIFY(dist)
  NULLIFY(satobs)

! Check cluster strategy
! ----------------------
  IF (opt%cluStrat /= 2) RETURN

! Nothing to do
! -------------
  IF (opt%staObs <= 1) RETURN
  opt%nSolve = 1

! Init GOBSEP
! -----------
  CALL mcobsb(1,opt,nFil,filLst,nSat,allSat,satObs,obsEpo,irc)

! Allocate Observation statistics
! -------------------------------

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
      IF (staLst(iSta,iClu) /= 0) cluSum(iClu) = cluSum(iClu)+1
    ENDDO
  ENDDO

  ! Satellite statistic
  ALLOCATE(satLst(nSat,opt%numClu),stat=irc)
  CALL alcerr(irc,'satLst',(/nSat,opt%numClu/),srName)

  satLst = 0

  ! Duplication statistic
  ALLOCATE(dblNxt(nFil,opt%numClu,2),stat=irc)
  CALL alcerr(irc,'dblNxt',(/nFil,opt%numClu,2/),srName)

  dblNxt = 0

  ! Compute the distances between the stations
  CALL mcdist(nSta,station,dist)


! Loop until no more stations to duplicate
! ----------------------------------------
  mSta = 1
  jIter = 0
  DO WHILE (mSta /= 0 .AND. jIter <= maxIter)
    jIter = jIter+1

! Reset the "non-remove"-Flags from the previous iteration
! --------------------------------------------------------
    DO iSta = 1,nFil
      DO iClu = 1,opt%numClu
        IF (staLst(iSta,iClu) == 3) staLst(iSta,iClu) = 1
        IF (staLst(iSta,iClu) == 2) staLst(iSta,iClu) = 3
      ENDDO
    ENDDO

! Init the epoch loop
! -------------------
    dblNxt = 0

    CALL mcobsb(2,opt,nFil,filLst,nSat,allSat,satObs,obsEpo,irc)

! Loop all epochs
! ---------------
    irc = 0
    DO WHILE (irc == 0)

      CALL mcobsb(0,opt,nFil,filLst,nSat,allSat,satObs,obsEpo,irc)

      IF (irc /= 0) EXIT

! Count the number of stations observing each satellite
! -----------------------------------------------------
      satLst = 0

      ! Loop all stations / baselines in all clusters
      DO iClu = 1,opt%numClu
        DO iFil = 1,nFil

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


! Indicate missing observations for regional clusters
! ---------------------------------------------------
      DO iFil = 1,nFil

        ! Loop all satellites observed by the station
        DO iSat = 1,obsEpo(iFil)%nSat

          ! Is it a valid observation?
          IF (tstflg(obsEpo(iFil)%obsrec(iSat)%obsflg(1),0) ) CYCLE
          IF (obsEpo(iFil)%obsrec(iSat)%observ(1)     == 0d0) CYCLE

          ! Get the index in satLst
          DO jSat = 1,nSat
            IF (obsEpo(iFil)%obsrec(iSat)%numSat == allSat(jSat)) EXIT
          ENDDO

          ! Find clusters with problems for this satellite
          nSolve = 0
          DO iClu = 1,opt%numClu

            IF (staLst(iFil,iClu) == 0) CYCLE

            IF (satLst(jSat,iClu) >= opt%staObs) &
              nSolve = nSolve+1

          ENDDO ! iClu

          IF (nSolve >= opt%nSolve) CYCLE

          ! Loop all clusters of this station
          DO iClu = 1,opt%numClu

            IF (staLst(iFil,iClu) == 0) CYCLE
            IF (satLst(jSat,iClu) >= opt%staObs) CYCLE

            ! Indicate the clusters, need help
            dblNxt(iFil,iClu,1) = 1

            ! Search for other stations observing this satellite
            DO jFil = 1,nFil

              IF (iFil == jFil) CYCLE

              DO jClu = 1,opt%numClu
                IF (staLst(jFil,jClu) == 0) CYCLE

                IF (staLst(jFil,iClu) /= 0) CYCLE

                ! Loop all satellites observed by the station
                DO jSat = 1,obsEpo(jFil)%nSat

                  IF (obsEpo(iFil)%obsrec(iSat)%numSat /= &
                      obsEpo(jFil)%obsrec(jSat)%numSat) CYCLE

                  ! Is it a valid observation?
                  IF (tstflg(obsEpo(jFil)%obsrec(jSat)%obsflg(1),0) ) CYCLE
                  IF (obsEpo(jFil)%obsrec(jSat)%observ(1)     == 0d0) CYCLE


                  dblNxt(jFil,iClu,2) = dblNxt(jFil,iClu,2)+1

                ENDDO ! jSat
              ENDDO ! jClu
            ENDDO ! jFil
          ENDDO ! iClu
        ENDDO ! iSat
      ENDDO ! iFil


! Indicate observations should not be deleted for regional clusters
! -----------------------------------------------------------------
      DO iFil = 1,nFil

        ! Loop all satellites observed by the station
        DO iSat = 1,obsEpo(iFil)%nSat

          ! Is it a valid observation?
          IF (tstflg(obsEpo(iFil)%obsrec(iSat)%obsflg(1),0) ) CYCLE
          IF (obsEpo(iFil)%obsrec(iSat)%observ(1)     == 0d0) CYCLE


          ! Get the index in satLst
          DO jSat = 1,nSat
            IF (obsEpo(iFil)%obsrec(iSat)%numSat == allSat(jSat)) EXIT
          ENDDO

          ! Check for observations which should not removed
          nSolve = 0
          DO iClu = 1,opt%numClu
            IF (staLst(iFil,iClu) == 0) CYCLE
            IF (satLst(jSat,iClu) > opt%staObs) nSolve = nSolve+1
          ENDDO ! iClu

          IF (nSolve <= opt%nSolve) THEN
            DO iClu = 1,opt%numClu
              IF (staLst(iFil,iClu) == 0) CYCLE
              staLst(iFil,iClu) = 3
            ENDDO
          ENDIF

          ! Loop all clusters of this station
          DO iClu = 1,opt%numClu

            IF (staLst(iFil,iClu) == 0) CYCLE

            ! Get the index in satLst
            DO jSat = 1,nSat

              IF (obsEpo(iFil)%obsrec(iSat)%numSat == allSat(jSat)) EXIT

            ENDDO ! jSat

            IF (satLst(jSat,iClu) > opt%staObs) CYCLE

            ! Search for other stations observing this satellite
            DO jFil = 1,nFil

              IF (iFil == jFil) CYCLE

              IF (staLst(jFil,iClu) == 0) CYCLE

              ! Loop all satellites observed by the station
              DO jSat = 1,obsEpo(jFil)%nSat

                IF (obsEpo(iFil)%obsrec(iSat)%numSat /= &
                    obsEpo(jFil)%obsrec(jSat)%numSat) CYCLE

                ! Is it a valid observation?
                IF (tstflg(obsEpo(jFil)%obsrec(jSat)%obsflg(1),0) ) CYCLE
                IF (obsEpo(jFil)%obsrec(jSat)%observ(1)     == 0d0) CYCLE


                dblNxt(iFil,iClu,2) = dblNxt(iFil,iClu,2)+1

              ENDDO ! jSat
            ENDDO ! jFil
          ENDDO ! iClu
        ENDDO ! iSat
      ENDDO ! iFil


    ENDDO  ! next epoch

! Start duplication
! -----------------

    ! Get the actual number of stations
    nClu = 0
    DO iClu = 1,opt%numClu
      nClu = nClu+cluSum(iClu)
    ENDDO


! Duplicate/delete stations from regional clusters
! ------------------------------------------------

    ! Delete/duplicate -- iteration
    iIter = 1
    DO WHILE(iIter /= 0)
      iIter = 0

! Loop stations to find stations may be deleted
! ---------------------------------------------
      mSta = 0
      DO iFil = 1,nFil

        ! No deletions anymore if the end of the iteration comes into view
        IF (maxIter - jIter < 3) EXIT

        ! Candidates for deletions have staLst == 1
        ! (Find the station with the lowest number of "critical" obs.)
        mClu = 0
        DO iClu = 1,opt%numClu
          IF (staLst(iFil,iClu) /= 1) CYCLE
          IF (mClu == 0) THEN
            mClu = iClu
          ELSE IF (dblNxt(iFil,iClu,2) < dblNxt(iFil,mClu,2)) THEN
            mClu = iClu
          ENDIF
        ENDDO

         ! Stations are not yet removed, but flaged only...
         ! (to prevent deletion/duplication loops)
        IF (mClu /= 0) THEN
          staLst(iFil,mClu) = -1
        ENDIF
      ENDDO

! Scale the number of observations with the number of clusters
! containing the station already
! ------------------------------------------------------------
      DO iFil = 1,nFil

        mClu = 0
        DO iClu = 1,opt%numClu
          IF (staLst(iFil,iClu) > 0) mClu = mClu+1
        ENDDO

        IF (mClu <= 1) CYCLE

        DO iClu = 1,opt%numClu
          IF (staLst(iFil,iClu) > 0) CYCLE
          IF (dblNxt(iFil,iClu,2) == 0) CYCLE
          dblNxt(iFil,iClu,2) = (dblNxt(iFil,iClu,2)/mClu**2)+1
        ENDDO
      ENDDO

! Loop for stations with problems in the number of observations
! -------------------------------------------------------------
      DO iFil = 1,nFil

        mClu = 0
        DO iClu = 1,opt%numClu

          ! Check all clusters:
          ! start with the cluster with the smallest number of stations
          IF (dblNxt(iFil,iClu,1) /= 1 .AND. &
              dblNxt(iFil,iClu,1) /= 3) CYCLE

          iSta = filInfo(iFil)%staIdx(1)

          ! Check all stations in the cluster
          ! Find a station close to the cluster with
          ! a big amount of common data
!          mSta = 0
          DO jFil = 1,nFil
            IF (staLst(jFil,iClu) /= -1 .AND. &
                staLst(jFil,iClu) /=  0 .AND. &
                staLst(jFil,iClu) /=  2) CYCLE

            IF (dblNxt(jFil,iClu,2) == 0) CYCLE

            jSta = filInfo(jFil)%staIdx(1)

            distClu = 0d0
            DO kFil = 1,nFil
              IF (staLst(kFil,iClu) /= -1 .AND. &
                  staLst(kFil,iClu) /=  0 .AND. &
                  staLst(kFil,iClu) /=  2) CYCLE
              IF (iFil == kFil) THEN
                distClu = distClu + &
                          dist(IKF(jSta,filInfo(kFil)%staIdx(1)))**cluSum(iClu)
              ELSE
                distClu = distClu + dist(IKF(jSta,filInfo(kFil)%staIdx(1)))
              ENDIF
            ENDDO

            distClu = distClu ** &
                      (DBLE(cluSum(iClu)*opt%numClu)/DBLE(nClu))

            IF (mClu == 0) THEN
!            IF (mSta == 0) THEN
              mSta = filInfo(jFil)%staIdx(1)
              mClu = iClu
              distMin = distClu/dblNxt(jFil,iClu,2)**2
            ELSE IF(distClu/dblNxt(jFil,iClu,2)**2 < distMin) THEN
              mSta = filInfo(jFil)%staIdx(1)
              mClu = iClu
              distMin = distClu/dblNxt(jFil,iClu,2)**2
            ENDIF
          ENDDO
        ENDDO

        ! A station can be added...
        IF (mClu /= 0) THEN
          IF (staLst(mSta,mClu) ==  0) cluSum(mClu) = cluSum(mClu)+1
          IF (staLst(mSta,mClu) == -1) iIter = 1
          staLst(mSta,mClu) = 2
        ENDIF

      ENDDO

      ! Delete the station definitive
      DO iSta = 1,nFil
        DO iClu = 1,opt%numClu
          IF (staLst(iSta,iClu) == -1) THEN
            staLst(iSta,iClu) = 0
            cluSum(iClu) = cluSum(iClu)-1
            mSta = iSta
          ENDIF
        ENDDO
      ENDDO

    ENDDO


  ENDDO ! Next iteration,
        ! start reading all observations with the new cluster configuration

! Close observation files, deallocate all "GOBSEP"-arrays
! -------------------------------------------------------
  CALL mcobsb(-1,opt,nFil,filLst,nSat,allSat,satObs,obsEpo,irc)

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

! Update number of cluster if maxsta exceeded (regional only)
! -----------------------------------------------------------
  IF (opt%maxSta > 0) THEN

    mClu = 0
    DO iClu = 1,opt%numClu
      IF (cluSum(iClu) > opt%maxSta) &
        mClu = mClu + (cluSum(iClu) - opt%maxSta)
    ENDDO

    IF (mClu > 0) THEN
      DEALLOCATE(cluList,stat=irc)

      ALLOCATE(cluList(nFil),stat=irc)
      CALL alcerr(irc,'cluList',(/nFil/),srName)

      nClu = 0
      DO iFil = 1,nFil
        DO iClu = 1,opt%numClu
          IF (staLst(iFil,iClu) == 0) CYCLE

          nClu = nClu+1

          cluList(nClu)%filIdx  = iFil
          cluList(nClu)%cluster = 0
          cluList(nClu)%delFlg  = 0

          EXIT

        ENDDO
      ENDDO

      opt%numClu = opt%numClu + (mClu-1)/opt%maxSta+1

    ENDIF
  ENDIF

! Deallocate all arrays
! ---------------------
  DEALLOCATE(dist,stat=irc)
  DEALLOCATE(staLst,stat=irc)
  DEALLOCATE(satLst,stat=irc)
  DEALLOCATE(dblNxt,stat=irc)
  DEALLOCATE(cluSum,stat=irc)
  DEALLOCATE(obsEpo,stat=irc)
  DEALLOCATE(allSat,stat=irc)

  RETURN
END SUBROUTINE mcdblr

END MODULE
