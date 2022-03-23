MODULE s_MCMAXSTA
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE mcMaxsta(opt,nFil,filLst,filInfo,nSta,station, &
                      nClu,cluList,nDel,delSta,ircSta)

! -------------------------------------------------------------------------
! Purpose:    Remove stations from clusters to have not more than opt%maxsta
!             stations in each cluster
!
! Author:     R. Dach
!
! Created:    13-Jun-2002
!
! Changes:    22-Nov-2002 RD: Source code cosmetics
!             13-Feb-2003 RD: Optimum number of stations
!             23-Apr-2003 AJ: Nullify local pointers
!             16-May-2003 CU: Initialize structure
!             28-Jun-2005 MM: Unused variables removed
!             15-Dec-2005 RD: New call of SR mcobsb
!             28-APr-2012 RD: Nullify all pointers, use m_bern with only
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, fileNameLength
  USE p_mkclus, ONLY: t_mkclus_opt,t_staFil,t_station,t_cluster
  USE d_gpsobs, ONLY: t_obsepo
  USE d_staLst, ONLY: t_staList, init_stalist
  USE f_ikf
  USE s_alcerr
  USE f_tstflg
  USE s_mcdist
  USE s_gtflna
  USE s_mcobsb
  USE s_readstsg
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  TYPE(t_mkclus_opt)                     :: opt      ! Input options
  INTEGER(i4b)                           :: nFil     ! Number of files
  CHARACTER(LEN=fileNameLength),          &
                 DIMENSION(:,:), POINTER :: filLst   ! Observation files
  TYPE(t_staFil),  DIMENSION(:), POINTER :: filInfo  ! File information
  INTEGER(i4b)                           :: nSta     ! Number of stations to
                                                     ! put into the clusters
  TYPE(t_station), DIMENSION(:), POINTER :: station  ! Station records

! input/output
  INTEGER(i4b)                           :: nClu     ! Number of files to
                                                     ! put into the clusters
  TYPE(t_cluster), DIMENSION(:), POINTER :: cluList  ! Cluster record
  INTEGER(i4b)                           :: nDel     ! Number of stations to
                                                     ! be deleted from clusters
  TYPE(t_cluster), DIMENSION(:), POINTER :: delSta   ! Station deletion list

! output
  INTEGER(i4b)                           :: ircSta   ! Station deletion status:
                                         ! 0: maxsta is reached in all clusters
                                         ! 1: too many stations, update delFlg
                                         ! 2: too many stations, do not check
                                         !             observations anymore

! Local parameters
! ----------------
  CHARACTER(LEN=8),              PARAMETER   :: srName = 'mcMaxsta'


! Local Variables
! ---------------
  TYPE(t_staList)                            :: corSta
  TYPE(t_cluster), DIMENSION(:),   POINTER   :: hlpSta
  TYPE(t_obsepo),  DIMENSION(:),   POINTER   :: obsEpo

  CHARACTER(LEN=fileNameLength)              :: lstFil

  INTEGER(i4b), DIMENSION(:),   ALLOCATABLE  :: toBeDel
  INTEGER(i4b)                               :: numSta
  INTEGER(i4b)                               :: dSta
  INTEGER(i4b)                               :: iDel
  INTEGER(i4b)                               :: numDel
  INTEGER(i4b)                               :: iIter
  INTEGER(i4b)                               :: iClu
  INTEGER(i4b)                               :: iSta,jSta,kSta
  INTEGER(i4b)                               :: iSta1,iSta2
  INTEGER(i4b)                               :: jSta1,jSta2
  INTEGER(i4b)                               :: iSat,jSat
  INTEGER(i4b)                               :: nSat
  INTEGER(i4b), DIMENSION(:),  POINTER       :: allSat
  INTEGER(i4b), DIMENSION(:),  ALLOCATABLE   :: numSat
  INTEGER(i4b)                               :: iFil,jFil
  INTEGER(i4b)                               :: irc

  REAL(r8b),    DIMENSION(:),   POINTER      :: dist
  REAL(r8b),    DIMENSION(:),   ALLOCATABLE  :: obsStat
  REAL(r8b)                                  :: distSum
  REAL(r8b)                                  :: distMax
  REAL(r8b),    DIMENSION(:),   POINTER      :: satobs

  NULLIFY(hlpSta)
  NULLIFY(obsEpo)
  NULLIFY(allSat)
  NULLIFY(dist)
  NULLIFY(satObs)
  CALL init_stalist(corSta)

! Nothing to do in this subroutine
! --------------------------------
  IF (opt%maxSta == 0) THEN
    ircSta = 0
    RETURN
  ENDIF

! Read the list of core sites
! ---------------------------
  CALL gtflna(0,'CORESITE',lstFil,irc)
  corSta%nSta = 0
  IF (irc == 0 .AND. LEN_TRIM(lstFil) > 0) THEN
    CALL readStsg(lstFil,0,corSta)

    DO iSta = 1,nClu
      DO jSta = 1,corSta%nSta
        IF (station(filInfo(cluList(iSta)%filIdx)%staIdx(1))%staNam == &
            corSta%staNam(jSta)) THEN
          cluList(iSta)%delFlg=4
        ELSE IF (filInfo(cluList(iSta)%filIdx)%nDiff > 0) THEN
          IF (station(filInfo(cluList(iSta)%filIdx)%staIdx(2))%staNam == &
              corSta%staNam(jSta)) THEN
            cluList(iSta)%delFlg=4
          ENDIF
        ENDIF
      ENDDO
    ENDDO

    DEALLOCATE(corSta%staNam,stat=irc)
  ENDIF

! Allocate the deletion request array
! -----------------------------------
  ALLOCATE(toBeDel(nClu),stat=irc)
  CALL alcerr(irc,'toBeDel',(/nClu/),srName)

  toBeDel = 0

! Distances for network density strategy
! --------------------------------------
  IF (opt%staStrat == 3) &
    CALL mcdist(nSta,station,dist)

! Init GOBSEP for density with obs
! --------------------------------
  IF (opt%staStrat == 4) THEN
    CALL mcobsb(1,opt,nFil,filLst,nSat,allSat,satObs,obsEpo,irc)

    ALLOCATE(obsStat(nClu),stat=irc)
    CALL alcerr(irc,'obsStat',(/nClu/),srName)

    obsStat=0d0

    ALLOCATE(numSat(nSat),stat=irc)
    CALL alcerr(irc,'numSat',(/nSat/),srName)

  ENDIF

! Loop all clusters
! -----------------
  IF (ircSta <  2) ircSta = 0
  IF (ircSta == 2) ircSta = 3

  iClu = 0
  DO WHILE (iClu < opt%numClu)
    iClu = iClu+1

! Do iterations until opt%maxSta is achieved
! ------------------------------------------
    iDel = 0   ! Count number of deletions in this run
    numSta = opt%maxSta + 1
    DO WHILE (numSta > opt%maxSta .AND. &
!              (iDel < NINT(opt%maxSta*0.05)+1 &  ! Delete not more than 5% of
              (iDel < NINT(nClu/opt%numClu*0.05)+1 &  ! Delete not more than 5% of
                                         .OR. &  ! maxSta per iteration
              (ircSta >= 2 .AND. opt%maxSta /= -1)))

! Network density with maxObs
! ---------------------------
      IF (opt%staStrat == 4) THEN

        ! Rewind GOBSEP
        CALL mcobsb(2,opt,nFil,filLst,nSat,allSat,satObs,obsEpo,irc)
        obsStat = 0d0

        ! Loop all epochs
        DO WHILE (irc == 0)

          ! Read next epoch
          CALL mcobsb(0,opt,nFil,filLst,nSat,allSat,satObs,obsEpo,irc)
          IF (irc /= 0) CYCLE

          ! Find all valid observations for the satellites
          numSat = 0
          DO iFil = 1,nFil
            DO iSta = 1,nClu
              IF (toBeDel(iSta) /= 0) CYCLE
              IF (cluList(iSta)%cluster /= iClu) CYCLE
              IF (iFil == cluList(iSta)%filIdx) THEN
                DO jSat = 1,obsEpo(iFil)%nSat
                  IF (tstflg(obsEpo(iFil)%obsrec(jSat)%obsflg(1),0) ) CYCLE
                  IF (obsEpo(iFil)%obsrec(jSat)%observ(1)    ==  0d0) CYCLE
                  DO iSat = 1,nSat
                    IF (obsEpo(iFil)%obsRec(jSat)%numSat == allSat(iSat)) THEN
                      numSat(iSat) = numSat(iSat)+1
                      EXIT
                    ENDIF
                  ENDDO ! iSat
                ENDDO ! jSat
                EXIT
              ENDIF
            ENDDO ! iSta
          ENDDO ! iFil

          ! Update station statistic
          DO iSta = 1,nClu
            IF (toBeDel(iSta) /= 0) CYCLE
            IF (cluList(iSta)%cluster /= iClu) CYCLE

            iFil = cluList(iSta)%filIdx
            DO jSat = 1,obsEpo(iFil)%nSat
              IF (tstflg(obsEpo(iFil)%obsrec(jSat)%obsflg(1),0) ) CYCLE
              IF (obsEpo(iFil)%obsrec(jSat)%observ(1)    ==  0d0) CYCLE
              DO iSat = 1,nSat
                IF (obsEpo(iFil)%obsRec(jSat)%numSat == allSat(iSat) .AND. &
                    numSat(iSat) > 0) THEN
                  obsStat(iSta) = obsStat(iSta)+1d0/DBLE(numSat(iSat))
                  EXIT
                ENDIF
              ENDDO ! iSat
            ENDDO ! jSta
          ENDDO ! iSta

        ENDDO ! Epoch loop
      ENDIF

! Loop all stations
! -----------------
      dSta = 0

      DO iIter = 1,3

        IF (iIter == 1 .AND. opt%numClu == 1) CYCLE

        IF (iIter == 3 .AND. opt%maxSta == -1) THEN
          ircSta = 2
          iClu = opt%numClu
          iDel = nClu
          EXIT
        ENDIF

        IF (iIter == 3 .AND. ircSta < 2) THEN
          ircSta = 2
          iClu = 0
          EXIT
        ENDIF

        numSta = 0
        DO iSta = 1,nClu

          ! Is the station already deleted?
          IF (toBeDel(iSta) /= 0) CYCLE

          ! Is the station in the actual cluster?
          IF (cluList(iSta)%cluster /= iClu) CYCLE
          numSta = numSta+1

          ! The station may be deleted?
          IF (iIter <= 2 .AND. cluList(iSta)%delFlg /= 0) CYCLE

          ! Check only stations which are in other clusters too
          IF (iIter == 1) THEN
            kSta = 0
            DO jSta = 1,nClu
              IF (iSta == jSta) CYCLE
              IF (toBeDel(jSta) /= 0) CYCLE
              iFil = cluList(iSta)%filIdx
              jFil = cluList(jSta)%filIdx
              IF (filInfo(iFil)%staIdx(1) == filInfo(jFil)%staIdx(1)) THEN
                kSta = jSta
              ELSE IF (filInfo(iFil)%nDiff > 0 .AND. &
                       filInfo(jFil)%nDiff > 0 .AND. &
                       filInfo(iFil)%staIdx(2) == filInfo(jFil)%staIdx(2)) THEN
                kSta = jSta
              ENDIF
            ENDDO
            IF (kSta == 0) CYCLE
          ENDIF

          ! First station
          IF (dSta == 0) THEN
            dSta = iSta
            distMax = 0d0
          ENDIF

          ! Most observation optimization
          IF (opt%staStrat == 1) THEN
            iFil = cluList(iSta)%filIdx
            jFil = cluList(dSta)%filIdx

            IF (filInfo(iFil)%numObs < filInfo(jFil)%numObs) dSta = iSta

          ! Best clock optmimization
          ELSE IF (opt%staStrat == 2) THEN
            iSta1 = filInfo(cluList(iSta)%filIdx)%staIdx(1)
            iSta2 = filInfo(cluList(dSta)%filIdx)%staIdx(1)

            IF (station(iSta1)%clkRms > station(iSta2)%clkRms) dSta = iSta

          ! Network density
          ELSE IF (opt%staStrat == 3) THEN

            distSum = 0d0
            DO jSta = 1,nClu

              IF (cluList(jSta)%cluster /= iClu) CYCLE
              IF (iSta == jSta) CYCLE
              IF (toBeDel(jSta) /= 0) CYCLE

              iSta1 = filInfo(cluList(iSta)%filIdx)%staIdx(1)
              iSta2 = filInfo(cluList(iSta)%filIdx)%staIdx(2)
              jSta1 = filInfo(cluList(jSta)%filIdx)%staIdx(1)
              jSta2 = filInfo(cluList(jSta)%filIdx)%staIdx(2)

              ! First stations
              IF (dist(IKF(iSta1,jSta1)) < 1D0) THEN
                distSum = distSum + 1D99
              ELSE
                distSum = distSum + 1/dist(IKF(iSta1,jSta1))**2
              ENDIF

              ! First baseline
              IF (filInfo(cluList(iSta)%filIdx)%nDiff > 0) THEN
                IF (dist(IKF(iSta2,jSta1)) < 1D0) THEN
                  distSum = distSum + 1D99
                ELSE
                  distSum = distSum + 1/dist(IKF(iSta2,jSta1))**2
                ENDIF
              ENDIF

              ! Second baseline
              IF (filInfo(cluList(jSta)%filIdx)%nDiff > 0) THEN
                IF (dist(IKF(iSta1,jSta2)) < 1D0) THEN
                  distSum = distSum + 1D99
                ELSE
                  distSum = distSum + 1/dist(IKF(iSta1,jSta2))**2
                ENDIF
              ENDIF

              ! Second stations (both baselines)
              IF (filInfo(cluList(iSta)%filIdx)%nDiff > 0 .AND. &
                  filInfo(cluList(jSta)%filIdx)%nDiff > 0) THEN
                IF (dist(IKF(iSta2,jSta2)) < 1D0) THEN
                  distSum = distSum + 1D99
                ELSE
                  distSum = distSum + 1/dist(IKF(iSta2,jSta2))**2
                ENDIF
              ENDIF

            ENDDO

            IF (distSum > distMax) THEN
              dSta = iSta
              distMax = distSum
            ENDIF

          ! Network density with maxObs
          ELSE IF (opt%staStrat == 4) THEN

            IF (obsStat(iSta) < obsStat(dSta)) dSta = iSta

          ENDIF

        ENDDO  ! Station loop

! Number of station in the cluster is smaller or equal than requested
! -------------------------------------------------------------------
        IF (numSta <= opt%maxSta) EXIT

! A station for deletion found
! ----------------------------
        IF (dSta /= 0) THEN

          toBeDel(dSta) = 1
          iDel = iDel + 1

          ! Skip the other runs if this one was already sucessful
          EXIT
        ENDIF

      ENDDO  ! A 2nd run deleting also stations which are in one cluster only
             ! A 3rd run ignoring the deletion flags

    ENDDO  ! Next iteration (Delete more stations)

    ! There is still something to do
    IF (ircSta == 0 .AND. numSta > opt%maxSta) ircSta = 1
  ENDDO  ! Cluster loop

  IF (ircSta == 2) ircSta = 0

! Deallocate distances
! --------------------
  IF (opt%staStrat == 3) &
    DEALLOCATE(dist, stat=irc)

! Init GOBSEP for density with obs
! --------------------------------
  IF (opt%staStrat == 4) THEN
    CALL mcobsb(-1,opt,nFil,filLst,nSat,allSat,satObs,obsEpo,irc)
    DEALLOCATE(obsStat,stat=irc)
    DEALLOCATE(numSat,stat=irc)
  ENDIF

! Move stations from station to deletion list
! -------------------------------------------
  numDel = nDel
  DO iSta = 1,nClu
    numDel = numDel + toBeDel(iSta)
  ENDDO

! Extent the deletion list
! ------------------------
  IF (nDel == 0) THEN

    ALLOCATE(delSta(numDel),stat=irc)
    CALL alcerr(irc,'delSta',(/numDel/),srName)

  ELSE IF (numDel > SIZE(delSta)) THEN

    ALLOCATE(hlpSta(nDel),stat=irc)
    CALL alcerr(irc,'hlpSta',(/nDel/),srName)

    hlpSta(1:nDel) = delSta(1:nDel)

    DEALLOCATE(delSta,stat=irc)

    ALLOCATE(delSta(numDel),stat=irc)
    CALL alcerr(irc,'delSta',(/numDel/),srName)

    delSta(1:nDel) = hlpSta(1:nDel)
    DEALLOCATE(hlpSta,stat=irc)

  ENDIF

! Update the delSta-Array
! -----------------------
  DO iSta = 1,nClu
    IF (toBeDel(iSta) == 0) CYCLE

    nDel = nDel+1
    delSta(nDel) = cluList(iSta)
  ENDDO

! Update the station-list
! -----------------------
  jSta = 0
  DO iSta = 1,nClu
    IF (toBeDel(iSta) == 1) CYCLE

    jSta = jSta+1
    IF (iSta == jSta) CYCLE

    cluList(jSta) = cluList(iSta)
  ENDDO

  nClu = jSta

! Deallocate local arrays
! -----------------------
  DEALLOCATE(toBeDel,stat=irc)
  DEALLOCATE(obsEpo,stat=irc)
  DEALLOCATE(allSat,stat=irc)


  RETURN
END SUBROUTINE mcMaxsta

END MODULE
