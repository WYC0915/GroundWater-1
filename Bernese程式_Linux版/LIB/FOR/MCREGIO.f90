MODULE s_MCREGIO
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE mcregio(opt,nSta,station,nFil,filInfo,nClu,cluList)

! -------------------------------------------------------------------------
! Purpose:    Put stations into regional clusters
!
! Author:     R. Dach
!
! Created:    13-Jun-2002
! Last mod.:  07-Jul-2003
!
! Changes:    23-Apr-2003 AJ: Nullify local pointers
!             18-May-2003 HU: Nullify pointer
!             07-Jul-2003 RD: Several iterations for improving the geometry
!
! SR called:  alcerr,ikf,mcnext,mcdist
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE p_mkclus, ONLY: t_mkclus_opt,t_station,t_cluster,t_staFil
  USE f_ikf
  USE s_alcerr
  USE s_mcdist
  USE s_mcnext
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  TYPE(t_mkclus_opt)                   :: opt      ! Input options
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
  CHARACTER(LEN=7), PARAMETER          :: srName = 'mcregio'

! Local Variables
! ---------------
  TYPE(t_station),DIMENSION(:),POINTER :: helpSta

  INTEGER(i4b),   DIMENSION(:),POINTER :: nextSta
  INTEGER(i4b),   DIMENSION(:),POINTER :: nStaClu
  INTEGER(i4b)                         :: numSta
  INTEGER(i4b)                         :: anzSta
  INTEGER(i4b)                         :: nxtSta
  INTEGER(i4b)                         :: iClu,jClu
  INTEGER(i4b)                         :: iSta,jSta,kSta
  INTEGER(i4b)                         :: mSta
  INTEGER(i4b)                         :: mClu,mClu0
  INTEGER(i4b)                         :: iIter
  INTEGER(i4b)                         :: i1,i2
  INTEGER(i4b)                         :: irc

  REAL(r8b),      DIMENSION(:),POINTER :: dist
  REAL(r8b),      DIMENSION(:),POINTER :: distClu
  REAL(r8b)                            :: distMax
  REAL(r8b)                            :: distSum

  NULLIFY(helpSta)
  NULLIFY(nextSta)
  NULLIFY(nStaClu)
  NULLIFY(dist)
  NULLIFY(distClu)

! Short way, if only one cluster
! ------------------------------
  IF (opt%numClu == 1) THEN
    cluList(:)%cluster = 1
    RETURN
  ENDIF

! Copy the station list
! ---------------------
  numSta = nClu
  cluList(:)%cluster = opt%numClu

  ALLOCATE(helpSta(numSta),stat=irc)
  CALL alcerr(irc,'helpSta',(/numSta/),srName)
  DO iSta=1,numSta
    NULLIFY(helpSta(iSta)%nxtSta)
  ENDDO

! Loop all clusters
! -----------------
  DO iClu = 1,opt%numClu-1

! Copy the station list
! ---------------------
    numSta = 0
    DO iSta = 1,nClu
      IF (cluList(iSta)%cluster /= opt%numClu) CYCLE
      numSta = numSta+1
      helpSta(numSta) = station(filInfo(cluList(iSta)%filIdx)%staIdx(1))
      IF (iSta == nxtSta) nxtSta = numSta
    ENDDO

! Compute the distances between the stations
! ------------------------------------------
    CALL mcdist(numSta,helpSta,dist)

    anzSta = (numSta-1) / (opt%numClu-iClu+1)

! Start with the cluster with the biggest size
! --------------------------------------------
    IF (iClu == 1) THEN

      distMax = 0d0
      mSta = 0

      DO iSta = 1,numSta

        CALL mcnext(iSta,anzSta+1,numSta,helpSta,dist,nextSta)

        distSum = 0d0
        DO jSta = 1,anzSta
          distSum = distSum + dist(IKF(iSta,nextSta(jSta)))
        ENDDO

        IF (mSta == 0 .OR. distMax < distSum) THEN

          IF (mSta /= 0) &
            DEALLOCATE(helpSta(mSta)%nxtSta, stat=irc)

          ALLOCATE(helpSta(iSta)%nxtSta(anzSta),stat=irc)
          CALL alcerr(irc,'helpSta(iSta)%nxtSta',(/anzSta/),srName)
          helpSta(iSta)%nxtSta(:) = nextSta(1:anzSta)

          distMax = distSum
          mSta    = iSta
          nxtSta  = nextSta(anzSta+1)

        ENDIF

        DEALLOCATE(nextSta,stat=irc)

      ENDDO

! Following clusters start close to the last one
! ----------------------------------------------
    ELSE

      mSta = nxtSta

      CALL mcnext(mSta,anzSta+1,numSta,helpSta,dist,nextSta)

      ALLOCATE(helpSta(mSta)%nxtSta(anzSta),stat=irc)
      CALL alcerr(irc,'helpSta(mSta)%nxtSta',(/anzSta/),srName)
      helpSta(mSta)%nxtSta(:) = nextSta(1:anzSta)

      nxtSta = nextSta(anzSta+1)

      DEALLOCATE(nextSta,stat=irc)

    ENDIF

    DEALLOCATE(dist,stat=irc)

! Put the stations into the clusters
! ----------------------------------
    DO jSta = 0,anzSta

      IF (jSta == 0) THEN
        iSta = mSta
      ELSE
        iSta = helpSta(mSta)%nxtSta(jSta)
      ENDIF

      DO kSta = 1,nClu
        IF (station(filInfo(cluList(kSta)%filIdx)%staIdx(1))%staNam == &
            helpSta(iSta)%staNam) THEN
          cluList(kSta)%cluster = iClu
          EXIT
        ENDIF
      ENDDO

    ENDDO

    DO kSta = 1,nClu
      IF (station(filInfo(cluList(kSta)%filIdx)%staIdx(1))%staNam == &
          helpSta(nxtSta)%staNam) THEN
        nxtSta = kSta
        EXIT
      ENDIF
    ENDDO

  ENDDO

  DEALLOCATE(helpSta,stat=irc)


! Check this apriori clustering:
! -----------------------------
  ALLOCATE(distClu(opt%numClu),stat=irc)
  CALL alcerr(irc,'distClu',(/opt%numClu/),srName)

  ALLOCATE(nStaClu(opt%numClu),stat=irc)
  CALL alcerr(irc,'nStaClu',(/opt%numClu/),srName)

  CALL mcdist(nSta,station,dist)

  ! Several iterations to increase the weight for the number of
  ! stations per cluster w.r.t. the geometry step by step
  DO iIter = 1,nClu

    IF (iIter >= 6) EXIT ! Not more than 6 iterations...

    mClu  = -1
    DO WHILE (mClu /= 0)

      distMax = 0d0
      mSta = 0
      mClu = 0

      ! Loop all stations
      DO iSta = 1,nClu

        ! Build the sum of distances from the stations
        !     to all stations in the different clusters
        iClu = cluList(iSta)%cluster
        distClu = 0d0
        nStaClu = 0

        DO jSta = 1,nClu

          IF (iSta == jSta) CYCLE

          jClu = cluList(jSta)%cluster
          i1 = filInfo(cluList(iSta)%filIdx)%staIdx(1)
          i2 = filInfo(cluList(jSta)%filIdx)%staIdx(1)
          distClu(jClu) = distClu(jClu) + dist(IKF(i1,i2))
          nStaClu(jClu) = nStaClu(jClu) + 1

        ENDDO

        DO jClu = 1,opt%numClu
!          distClu(jClu) = distClu(jClu) ** &
!                          (DBLE((nStaClu(jClu))*opt%numClu)/DBLE(nClu))
          distClu(jClu) = distClu(jClu) * &
                          (DBLE((nStaClu(jClu))*opt%numClu)/DBLE(nClu))**iIter
        ENDDO

        ! Get the ratio between the sum of distances for the own cluster
        !    with respect the the other clusters
        !    (>1, station should better be in another cluster)
        distSum = 1d0
        mClu0 = iClu
        DO jClu = 1,opt%numClu

          ! Prevent division by zero
          IF (nStaClu(jClu) == 0) CYCLE

          ! Improvement of geometry is possible
          IF (distClu(iClu)/distClu(jClu) > distSum) THEN
            distSum = distClu(iClu)/distClu(jClu)
            mClu0 = jClu
          ENDIF
        ENDDO
        IF (mClu0 == iClu) CYCLE

        ! Flag the station with should change the cluster
        IF (mSta == 0 .OR. distSum > distMax) THEN
          mSta    = iSta
          mClu    = mClu0
          distMax = distSum
        ENDIF
      ENDDO

      IF (mSta /= 0) THEN
        cluList(mSta)%cluster = mClu
      ENDIF

    ENDDO
  ENDDO

! Deallocate the arrays
! ---------------------
  DEALLOCATE(distClu,stat=irc)
  DEALLOCATE(nStaClu,stat=irc)
  DEALLOCATE(dist   ,stat=irc)

  RETURN
END SUBROUTINE mcregio

END MODULE
