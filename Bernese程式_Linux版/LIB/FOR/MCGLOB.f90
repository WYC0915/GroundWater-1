MODULE s_MCGLOB
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE mcglob(opt,nSta,station,nFil,filInfo,nClu,cluList)

! -------------------------------------------------------------------------
! Purpose:    Fill stations into global clusters
!
! Author:     R. Dach
!
! Created:    13-Jun-2002
! Last mod.:  18-May-2003
!
! Changes:    23-Apr-2003 AJ: Nullify local pointers
!             18-May-2003 HU: Initialize structure
!
! SR called:  alcerr,ikf,mcnext
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE p_mkclus, ONLY: t_mkclus_opt,t_station,t_stafil,t_cluster
  USE f_ikf
  USE s_alcerr
  USE s_mcdist
  USE s_mcnext
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  TYPE(t_mkclus_opt)                   :: opt      ! Program input options
  INTEGER(i4b)                         :: nSta     ! Number of stations
  TYPE(t_station),DIMENSION(:),POINTER :: station  ! Station information record
  INTEGER(i4b)                         :: nFil     ! Number of files
  TYPE(t_staFil), DIMENSION(:),POINTER :: filInfo  ! File information
  INTEGER(i4b)                         :: nClu     ! Number of files to
                                                   ! put into the clusters
! input/output:
  TYPE(t_cluster),DIMENSION(:),POINTER :: cluList  ! Cluster record

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER          :: srName = 'mcglob'

! Local Variables
! ---------------
  TYPE(t_station),DIMENSION(:),POINTER :: staList

  INTEGER(i4b) , DIMENSION(:), POINTER :: nextSta
  INTEGER(i4b)                         :: numSta
  INTEGER(i4b)                         :: iSta,jSta
  INTEGER(i4b)                         :: mSta
  INTEGER(i4b)                         :: iClu
  INTEGER(i4b)                         :: mClu
  INTEGER(i4b)                         :: irc

  REAL(r8b),      DIMENSION(:),POINTER :: dist     ! Array with dist. between
                                                   ! stations
  REAL(r8b)                            :: distij
  REAL(r8b)                            :: distSum
  REAL(r8b)                            :: distMin,distMax

  NULLIFY(staList)
  NULLIFY(nextSta)
  NULLIFY(dist)

! Short way, if only one cluster
! ------------------------------
  IF (opt%numClu == 1) THEN
    cluList(:)%cluster = 1
    RETURN
  ENDIF

! Generate a local station list
! -----------------------------
  ALLOCATE(staList(nClu),stat=irc)
  CALL alcerr(irc,'staList',(/nClu/),srName)

  DO iClu = 1,nClu
    NULLIFY(staList(iClu)%nxtSta)
    staList(iClu) = station(filInfo(cluList(iClu)%filIdx)%staIdx(1))
  ENDDO

! Compute the distances between the stations
! ------------------------------------------
  CALL mcdist(nClu,staList,dist)

!
! STEP 1: Put one station in each cluster
! ======
!
! These stations should be close together
! ---------------------------------------
  mSta = 0

! Loop all stations to find a minimum sum of dist. to the next stations
! ---------------------------------------------------------------------
  DO iSta = 1,nClu

    CALL mcnext(iSta,opt%numClu-1,nClu,staList,dist,nextSta)

    distSum = 0d0
    DO jSta = 1,opt%numClu-1
      distSum = distSum + dist(IKF(iSta,jSta))
    ENDDO

! Store the index of the neighbours
! ---------------------------------
    IF (mSta == 0 .OR. distSum < distMin) THEN

      IF (mSta /= 0) DEALLOCATE(staList(mSta)%nxtSta,stat=irc)

      mSta = iSta
      distMin = distSum

      ALLOCATE(staList(mSta)%nxtSta(opt%numClu-1),stat=irc)
      CALL alcerr(irc,'staList(mSta)%nxtSta',(/opt%numClu-1/),srName)

      staList(mSta)%nxtSta = nextSta

    ENDIF

  ENDDO ! Next station

! Put the first stations into the clusters
! ----------------------------------------
  DO iClu = 1,opt%numClu-1

    IF (staList(mSta)%nxtSta(iClu) == 0) CYCLE

    cluList(staList(mSta)%nxtSta(iClu))%cluster = iClu

  ENDDO

  DEALLOCATE(staList(mSta)%nxtSta,stat=irc)

  cluList(mSta)%cluster = opt%numClu


!
! Part 2: Put all other stations into the clusters
! ======
!
! This job is done one by one station
! -----------------------------------
  numSta = opt%numClu   ! Number od stations already in clusters

  mSta = 1
  DO WHILE (mSta /= 0 .AND. numSta < nClu)

! Select next station: close to the stations already defined
! -----------------------------------------------------------
    mSta = 0

    DO iSta = 1,nClu

      IF (cluList(iSta)%cluster /= 0) CYCLE

      ! Get the sum of dist. to all station already in clusters
      distSum = 0d0
      DO jSta = 1,nClu

        IF (cluList(iSta)%cluster /= 0) &
          distSum = distSum + dist(IKF(iSta,jSta))

      ENDDO

      IF (mSta == 0 .OR. distSum < distMin) THEN

        mSta = iSta
        distMin = distSum

      ENDIF

    ENDDO  ! Next station

! Select the cluster for the station
! ----------------------------------
    IF (mSta /= 0) THEN

      numSta = numSta + 1

      distMax = 0d0

      ! Try all clusters
      mClu = 0
      DO iClu = 1,opt%numClu

        distSum = 0d0
        DO jSta = 1,nClu

          IF (mSta == jSta) CYCLE

          distij = dist(IKF(mSta,jSta))**2
          IF (distij < 1d0) distij = 1d0

          IF (cluList(jSta)%cluster == iClu) THEN
            distSum = distSum - 1/distij

          ELSE IF (cluList(jSta)%cluster /= 0) THEN
            distSum = distSum + 1/distij

          ENDIF
        ENDDO

        IF (mClu == 0 .OR. distSum > distMax) THEN
          mClu = iClu
          distMax = distSum
        ENDIF

      ENDDO ! try the next cluster

      cluList(mSta)%cluster = mClu
    ENDIF

! Next iteration
! --------------
  ENDDO

  DEALLOCATE(dist,stat=irc)
  DEALLOCATE(staList,stat=irc)
  DEALLOCATE(nextSta,stat=irc)

  RETURN
END SUBROUTINE mcglob

END MODULE
