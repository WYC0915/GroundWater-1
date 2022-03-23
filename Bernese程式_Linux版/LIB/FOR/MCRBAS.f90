MODULE s_MCRBAS
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE mcrbas(opt,nSta,station,nFil,filInfo,nClu,cluList)

! -------------------------------------------------------------------------
! Purpose:    Put baselines into regional clusters
!
! Author:     R. Dach
!
!
! Created:    13-Jun-2002
! Last mod.:  23-Apr-2003
!
! Changes:    25-Sep-2002 HU: Use interface to alcerr
!             23-Apr-2003 AJ: Nullify local pointers
!
! SR called:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE p_mkclus, ONLY: t_mkclus_opt,t_staFil,t_station,t_cluster
  USE f_ikf
  USE s_alcerr
  USE s_mcdist
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  TYPE(t_mkclus_opt)                    :: opt      ! Input options
  INTEGER(i4b)                          :: nSta     ! Number of stations
  TYPE(t_station), DIMENSION(:),POINTER :: station  ! Station information record
  INTEGER(i4b)                          :: nFil     ! Number of files
  TYPE(t_staFil),  DIMENSION(:),POINTER :: filInfo  ! File information
  INTEGER(i4b)                          :: nClu     ! Number of files to
                                                    ! put into the clusters
! input/output:
  TYPE(t_cluster), DIMENSION(:),POINTER :: cluList  ! Cluster record

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER          :: srName = 'mcrbas'

! Local Variables
! ---------------
  INTEGER(i4b),   DIMENSION(:),POINTER :: nStaClu
  INTEGER(i4b)                         :: numBsl
  INTEGER(i4b)                         :: anzBsl
  INTEGER(i4b)                         :: iClu,jClu
  INTEGER(i4b)                         :: mBsl
  INTEGER(i4b)                         :: iBsl,jBsl,kBsl
  INTEGER(i4b)                         :: iSta1, iSta2
  INTEGER(i4b)                         :: jSta1, jSta2
  INTEGER(i4b)                         :: mClu,mClu0
  INTEGER(i4b)                         :: irc

  REAL(r8b),      DIMENSION(:),POINTER :: dist
  REAL(r8b),      DIMENSION(:),POINTER :: distClu
  REAL(r8b)                            :: distMax
  REAL(r8b)                            :: distMin
  REAL(r8b)                            :: distSum

  LOGICAL,        DIMENSION(:),POINTER :: lStaClu

  NULLIFY(nStaClu)
  NULLIFY(dist)
  NULLIFY(distClu)
  NULLIFY(lStaClu)

! Short way, if only one cluster
! ------------------------------
  IF (opt%numClu == 1) THEN
    cluList(:)%cluster = 1
    RETURN
  ENDIF

! Get the distances between the stations
! --------------------------------------
  CALL mcdist(nSta,station,dist)

! Number of baselines per cluster
! (the last cluster contains only 1 baseline --
!  to make sure that all baselines in each cluster are connected)
! ---------------------------------------------------------------
  anzBsl = nClu / (opt%numClu-1)

! Init all baselines to the last cluster number
! ---------------------------------------------
  cluList(:)%cluster = 0

! Start with the longest baseline
! -------------------------------
  mBsl = 0

  DO iBsl = 1,nClu

    IF (mBsl == 0) THEN
      mBsl = iBsl
    ELSE IF (filInfo(cluList(mBsl)%filIdx)%length < &
             filInfo(cluList(iBsl)%filIdx)%length) THEN
      mBsl = iBsl
    ENDIF

  ENDDO

! Put this first baseline in the first cluster
! --------------------------------------------
  iClu = 1
  cluList(mBsl)%cluster = iClu
  numBsl = 1

! Loop all cluster
! ----------------
  DO numBsl = 2,nClu

    mClu = iClu

    ! Switch to the next cluster
    IF (numBsl > anzBsl*iClu .AND. &
        iClu+1 < opt%numClu) iClu = iClu+1
    IF (numBsl == nClu) iClu = opt%numClu

    mBsl = 0
    distMin = 0d0

    ! Find all baselines in the actual cluster
    DO iBsl = 1,nClu
      IF (cluList(iBsl)%cluster /= mClu) CYCLE

      ! Check all baselines which are still not in clusters
      DO jBsl = 1,nClu
        IF (cluList(jBsl)%cluster /= 0) CYCLE

        ! Has the baseline a link to the actual cluster (only 1st iteration)
        iSta1 = filInfo(cluList(iBsl)%filIdx)%staIdx(1)
        iSta2 = filInfo(cluList(iBsl)%filIdx)%staIdx(2)
        jSta1 = filInfo(cluList(jBsl)%filIdx)%staIdx(1)
        jSta2 = filInfo(cluList(jBsl)%filIdx)%staIdx(2)
        IF (iSta1 == jSta1 .OR. iSta1 == jSta2 .OR. &
            iSta2 == jSta1 .OR. iSta2 == jSta2 .OR. &
            mClu /= iClu) THEN

          iSta1 = filInfo(cluList(jBsl)%filIdx)%staIdx(1)
          iSta2 = filInfo(cluList(jBsl)%filIdx)%staIdx(2)

          ! Find the baseline close to the current cluster
          distSum = 0d0
          DO kBsl = 1,nClu
            IF (cluList(kBsl)%cluster /= iClu) CYCLE

            jSta1 = filInfo(cluList(kBsl)%filIdx)%staIdx(1)
            jSta2 = filInfo(cluList(kBsl)%filIdx)%staIdx(2)

            distSum = distSum + (dist(IKF(iSta1,jSta1)) + &
                                 dist(IKF(iSta1,jSta2)) + &
                                 dist(IKF(iSta2,jSta1)) + &
                                 dist(IKF(iSta2,jSta2))) / 4d0
          ENDDO

          IF (mBsl == 0) THEN
            mBsl = jBsl
            distMin = distSum
          ELSE IF (distMin > distSum) THEN
            mBsl = jBsl
            distMin = distSum
          ENDIF
        ENDIF

      ENDDO
    ENDDO

    IF (mBsl /= 0) cluList(mBsl)%cluster = iClu

  ENDDO ! End of cluster loop

! All baselines without connection in the first (geometric) iteration are
! put into clusters where they are connected
! ------------------------------------------
  DO iBsl = 1,nClu
    IF (cluList(iBsl)%cluster /= 0) CYCLE

    iSta1 = filInfo(cluList(iBsl)%filIdx)%staIdx(1)
    iSta2 = filInfo(cluList(iBsl)%filIdx)%staIdx(2)

    DO jBsl = 1,nClu
      IF (cluList(jBsl)%cluster == 0) CYCLE

      jSta1 = filInfo(cluList(jBsl)%filIdx)%staIdx(1)
      jSta2 = filInfo(cluList(jBsl)%filIdx)%staIdx(2)

      IF (iSta1 == jSta1 .OR. iSta1 == jSta2 .OR. &
          iSta2 == jSta1 .OR. iSta2 == jSta2) THEN

        cluList(iBsl)%cluster = cluList(jBsl)%cluster
        EXIT

      ENDIF
    ENDDO
  ENDDO

! Isolated baselines without any connections
! ------------------------------------------
  mBsl = 0
  distMin = 0d0

  ! Find all baselines which are still not in a cluster
  DO iBsl = 1,nClu
    IF (cluList(iBsl)%cluster /= 0) CYCLE

    ! Check all baselines which are in clusters
    DO jBsl = 1,nClu
      IF (cluList(jBsl)%cluster == 0) CYCLE

      iSta1 = filInfo(cluList(iBsl)%filIdx)%staIdx(1)
      iSta2 = filInfo(cluList(iBsl)%filIdx)%staIdx(2)
      jSta1 = filInfo(cluList(jBsl)%filIdx)%staIdx(1)
      jSta2 = filInfo(cluList(jBsl)%filIdx)%staIdx(2)

      distSum = dist(IKF(iSta1,jSta1)) + &
                dist(IKF(iSta1,jSta2)) + &
                dist(IKF(iSta2,jSta1)) + &
                dist(IKF(iSta2,jSta2)) / 4d0

      ! Find the baseline located close to the isolated baseline
      IF (mBsl == 0) THEN
        mBsl = jBsl
        mClu = cluList(jBsl)%cluster
        distMin = distSum
      ELSE IF (distMin > distSum) THEN
        mBsl = jBsl
        mClu = cluList(jBsl)%cluster
        distMin = distSum
      ENDIF

    ENDDO

    IF (mBsl /= 0) cluList(iBsl)%cluster = mClu
  ENDDO


! Allocate some statistic arrays
! ------------------------------
  ALLOCATE(distClu(opt%numClu),stat=irc)
  CALL alcerr(irc,'distClu',(/opt%numClu/),srName)

  ALLOCATE(nStaClu(opt%numClu),stat=irc)
  CALL alcerr(irc,'nStaClu',(/opt%numClu/),srName)

  ALLOCATE(lStaClu(opt%numClu),stat=irc)
  CALL alcerr(irc,'lStaClu',(/opt%numClu/),srName)

! Check this apriori clustering to optimize the geometry
! ------------------------------------------------------
  mClu = -1
  DO WHILE (mClu /= 0)

    distMax = 0d0
    mBsl = 0
    mClu = 0

    ! Loop all baselines
    DO iBsl = 1,nClu

      iClu = cluList(iBsl)%cluster

      ! Build the sum of distances from the stations
      !     to all stations in the different clusters
      distClu = 0d0
      nStaClu = 0
      lStaClu = .FALSE.
      DO jBsl = 1,nClu

        IF (iBsl == jBsl) CYCLE

        jClu = cluList(jBsl)%cluster

        iSta1 = filInfo(cluList(iBsl)%filIdx)%staIdx(1)
        iSta2 = filInfo(cluList(iBsl)%filIdx)%staIdx(2)
        jSta1 = filInfo(cluList(jBsl)%filIdx)%staIdx(1)
        jSta2 = filInfo(cluList(jBsl)%filIdx)%staIdx(2)

        distClu(jClu) = distClu(jClu) + &
                       (dist(IKF(iSta1,jSta1)) + &
                        dist(IKF(iSta1,jSta2)) + &
                        dist(IKF(iSta2,jSta1)) + &
                        dist(IKF(iSta2,jSta2))) / 4d0

        nStaClu(jClu) = nStaClu(jClu) + 1

        IF (iSta1 == jSta1 .OR. iSta1 == jSta2 .OR. &
            iSta2 == jSta1 .OR. iSta2 == jSta2) THEN
          lStaClu(jClu) = .TRUE.
        ENDIF

      ENDDO

      DO jClu = 1,opt%numClu
        distClu(jClu) = distClu(jClu) ** &
                        (DBLE(nStaClu(jClu)*opt%numClu)/DBLE(nClu))
      ENDDO


      ! Get the ratio between the sum of distances for the own cluster
      !    with respect the the other clusters
      !    (>1, baseline should better be in another cluster)
      distSum = 1d0
      mClu0 = iClu
      DO jClu = 1,opt%numClu

        ! Prevent division by zero
        IF (nStaClu(iClu) == 0 .OR. nStaClu(jClu) == 0 ) CYCLE

        ! Baseline needs connection
        IF (.NOT. lStaClu(jClu)) CYCLE

        ! Improvement of geometry is possible
        IF (distClu(iClu)/distClu(jClu) > distSum) THEN
          distSum = distClu(iClu)/distClu(jClu)
          mClu0 = jClu
        ENDIF
      ENDDO
      IF (mClu0 == iClu) CYCLE

      ! Flag the baseline with should change the cluster
      IF (mBsl == 0 .OR. distSum > distMax) THEN
        mBsl    = iBsl
        mClu    = mClu0
        distMax = distSum
      ENDIF
    ENDDO

    IF (mBsl /= 0) THEN
      cluList(mBsl)%cluster = mClu
    ENDIF

  ENDDO

! Deallocate the arrays
! ---------------------
  DEALLOCATE(distClu,stat=irc)
  DEALLOCATE(nStaClu,stat=irc)
  DEALLOCATE(lStaClu,stat=irc)

  DEALLOCATE(dist,   stat=irc)

  RETURN
END SUBROUTINE mcrbas

END MODULE
