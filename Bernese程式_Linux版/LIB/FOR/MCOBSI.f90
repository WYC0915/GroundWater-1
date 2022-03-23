MODULE s_MCOBSI
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE mcobsi(iMode,opt,nFil,filLst,nSat,allSat,satobs,obsEpo,irc)

! -------------------------------------------------------------------------
! Purpose:    Read all observation files, check the min. number of obs.
!             for each satellite
!
! Author:     R. Dach
!
! Created:    13-Jun-2002
!
! Changes:    22-Nov-2002 RD: Correct init of "numsat"
!             13-Feb-2003 RD: Bugfix in CALL of GOBSEP
!             16-May-2003 CU: Initialize structure
!             27-Jan-2004 HU: Call of GOBSEP changed
!             07-Feb-2005 HB: Adopt for ifc-Compiler, Version 8.1
!             15-Dec-2005 RD: A few new options added
!             17-Aug-2006 HU: Use for s_gobsep
!             25-May-2009 RD: Consider independently opt%obsSat for each GNSS
!             21-Sep-2009 RD: New call of GOBSEP
!             20-Sep-2012 D: Nullify pointers after deallocation
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfn001, &
                      fileNameLength, staNameLength
  USE m_time,   ONLY: t_timint
  USE m_global, ONLY: maxsys
  USE d_gpsobs, ONLY: t_obshead,t_obsEpo,init_obshead
  USE p_mkclus, ONLY: t_mkclus_opt
  USE s_alcerr
  USE s_opnfil
  USE f_tstflg
  USE s_opnerr
  USE s_rdhead2
  USE s_gobsep
  USE s_cksizei1
  USE s_mjdgps
  USE s_defreq
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  INTEGER(i4b)                         :: iMode    ! Mode of the SR:
                                                   !  1: init and allocate
                                                   !  2: Restart with 1st epoch
                                                   !  0: process an epoch
                                                   ! -1: deallocation
  TYPE(t_mkclus_opt)                   :: opt      ! Input options
  INTEGER(i4b)                         :: nFil     ! Number of files
  CHARACTER(LEN=fileNameLength),        &
               DIMENSION(:,:), POINTER :: filLst   ! Observation files

! input/output:
  INTEGER(i4b)                         :: nSat     ! Number of sats in allSat
  INTEGER(i4b),DIMENSION(:),   POINTER :: allSat   ! List of all satellites
  REAL(r8b),   DIMENSION(:),   POINTER :: satobs   ! Number of observ. from all
                                                   ! files for each sat.
                                                   ! (0d0: sat with few obs.)

! output:
  TYPE(t_obsepo),DIMENSION(:), POINTER :: obsEpo   ! Observation record
  INTEGER(i4b)                         :: irc      ! Return code of GOBSEP

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER          :: srName = 'mcobsi'

! Local Variables
! ---------------
  TYPE(t_obshead)                                     :: obsHed
  TYPE(t_timint)                                      :: timint

  CHARACTER(LEN=staNameLength), DIMENSION(1)          :: dummy1
  CHARACTER(LEN=6)                                    :: mxnSat,mxnfrq
  CHARACTER(LEN=1),DIMENSION(:,:,:),ALLOCATABLE, SAVE :: obsFlg ! Used by GOBSEP
  CHARACTER(LEN=1),DIMENSION(:,:,:),ALLOCATABLE, SAVE :: obsFl1 ! Used by GOBSEP
  CHARACTER(LEN=1),DIMENSION(:),    ALLOCATABLE, SAVE :: filAct ! Used by GOBSEP
  INTEGER(i4b),    DIMENSION(1)                       :: flgshd ! Used by GOBSEP

  INTEGER(i4b)                                        :: iFil
  INTEGER(i4b)                                        :: iSat,jSat,kSat
  INTEGER(i4b)                                        :: iSys
  INTEGER(i4b)                                        :: nrSat
  INTEGER(i4b),                                  SAVE :: maxSat
  INTEGER(i4b),    DIMENSION(maxsys)                  :: maxobs
  INTEGER(i4b)                                        :: mxcsat,mxcfrq
  INTEGER(i4b),    DIMENSION(:),    ALLOCATABLE, SAVE :: filnum ! Used by GOBSEP
  INTEGER(i4b),    DIMENSION(:),    ALLOCATABLE, SAVE :: ideltt ! Used by GOBSEP
  INTEGER(i4b),    DIMENSION(:),    ALLOCATABLE, SAVE :: ifrmat ! Used by GOBSEP
  INTEGER(i4b),    DIMENSION(:),    ALLOCATABLE, SAVE :: meatyp ! Used by GOBSEP
  INTEGER(i4b),    DIMENSION(:),    ALLOCATABLE, SAVE :: nSatel ! Used by GOBSEP
  INTEGER(i4b),    DIMENSION(:,:),  ALLOCATABLE, SAVE :: numSat ! Used by GOBSEP
  INTEGER(i4b),    DIMENSION(:),    ALLOCATABLE, SAVE :: nFrFil ! Used by GOBSEP
  INTEGER(i4b),    DIMENSION(:,:),  ALLOCATABLE, SAVE :: iCarr  ! Used by GOBSEP
  INTEGER(i4b),    DIMENSION(:),    ALLOCATABLE, SAVE :: nDiff  ! Used by GOBSEP
  INTEGER(i4b),    DIMENSION(:),    ALLOCATABLE, SAVE :: obsnum ! Used by GOBSEP
  INTEGER(i4b),    DIMENSION(:),    ALLOCATABLE, SAVE :: nSatFl ! Used by GOBSEP
  INTEGER(i4b),    DIMENSION(:,:),  ALLOCATABLE, SAVE :: svnFil ! Used by GOBSEP
  INTEGER(i4b),    DIMENSION(:,:),  ALLOCATABLE, SAVE :: svnFi1 ! Used by GOBSEP
  INTEGER(i4b),    DIMENSION(2,1)                     :: dummy2
  INTEGER(i4b)                                        :: nweek

  REAL(r8b),                                     SAVE :: tLast,tObs
  REAL(r8b)                                           :: gpssec
  REAL(r8b),       DIMENSION(:,:),  ALLOCATABLE, SAVE :: window ! Used by GOBSEP
  REAL(r8b),       DIMENSION(:),    ALLOCATABLE, SAVE :: timref ! Used by GOBSEP
  REAL(r8b),                                     SAVE :: dtSim  ! Used by GOBSEP
  REAL(r8b),       DIMENSION(:,:,:),ALLOCATABLE, SAVE :: observ ! Used by GOBSEP
  REAL(r8b),       DIMENSION(:,:,:),ALLOCATABLE, SAVE :: obser1 ! Used by GOBSEP
  REAL(r8b),       DIMENSION(:,:),  ALLOCATABLE, SAVE :: dtFil  ! Used by GOBSEP
  INTEGER(i4b),    DIMENSION(1)                       :: satshd ! Used by GOBSEP
  REAL(r8b),       DIMENSION(1)                       :: timshd ! Used by GOBSEP

  COMMON/MCMSAT/MXCSAT,MXNSAT
  COMMON/MCMFRQ/MXCFRQ,MXNFRQ


!
! PART 1: Init the reading of the observation files
! ====
!
  IF (iMode == 1) THEN

    CALL init_obshead(obshed)

! Generate a list of all satellites
! ---------------------------------
    timint%t = 0d0
    nSat   = 0
    maxSat = 0
    DO iFil = 1,nFil

      CALL rdhead2(filLst(1,iFil),obshed)

      ! Get MAXSAT (for GOBSEP)
      IF (obsHed%nSatel > maxSat) maxSat = obsHed%nSatel

      DO iSat = 1,obsHed%nSatel

        ! Is the satellite already in the list?
        kSat = 0
        DO jSat = 1,nSat
          IF (allSat(jSat) == obsHed%sat(iSat)%numSat) THEN
            kSat = jSat
            EXIT
          ENDIF
        ENDDO

        ! Add a new satellite to the list
        IF (kSat == 0) THEN

          IF (nSat == 0) THEN
            ALLOCATE(allSat(obsHed%nSatel),stat=irc)
            CALL alcerr(irc,'allSat',(/obsHed%nSatel/),srName)
          ENDIF

          nSat = nSat + 1

          ! Check the size (extent) of the list
          CALL cksizei1(allSat,nSat,10)

          allSat(nSat) = obsHed%sat(iSat)%numSat
        ENDIF

      ENDDO

      IF (timint%t(1) == 0d0 .OR. &
          timint%t(1) > obsHed%timref) THEN
        timint%t(1) = obsHed%timref
      ENDIF

      IF (timint%t(2) == 0d0 .OR. &
          timint%t(2) < obsHed%timref+obshed%ideltt*(obshed%nEpoch-1)/86400d0) THEN
        timint%t(2) = obsHed%timref+obshed%ideltt*(obshed%nEpoch-1)/86400d0
      ENDIF

      DEALLOCATE(obsHed%sat,stat=irc)
      DEALLOCATE(obsHed%ambigu,stat=irc)

    ENDDO

! Allocate and init arrays for the use of GOBSEP
! ----------------------------------------------

    ! File index
    ALLOCATE(filnum(nFil),stat=irc)
    CALL alcerr(irc,'filnum',(/nFil/),srName)

    ! Observation window
    ALLOCATE(window(2,nFil),stat=irc)
    CALL alcerr(irc,'window',(/2,nFil/),srName)

    ! Reference epoch
    ALLOCATE(timref(nFil),stat=irc)
    CALL alcerr(irc,'timref',(/nFil/),srName)

    ! Sampling rate (file)
    ALLOCATE(ideltt(nFil),stat=irc)
    CALL alcerr(irc,'ideltt',(/nFil/),srName)

    ! File format
    ALLOCATE(ifrmat(nFil),stat=irc)
    CALL alcerr(irc,'ifrmat',(/nFil/),srName)

    ! Measurement type
    ALLOCATE(meatyp(nFil),stat=irc)
    CALL alcerr(irc,'meatyp',(/nFil/),srName)

    ! Number of satellites per file
    ALLOCATE(nSatel(nFil),stat=irc)
    CALL alcerr(irc,'nSatel',(/nFil/),srName)

    ! Satellite number in file
    ALLOCATE(numSat(maxSat,nFil),stat=irc)
    CALL alcerr(irc,'numSat',(/maxSat,nFil/),srName)

    ! Number of frequencies per file
    ALLOCATE(nFrFil(nFil),stat=irc)
    CALL alcerr(irc,'nFrFil',(/nFil/),srName)

    ! Frequencies in file
    ALLOCATE(iCarr(1,nFil),stat=irc)
    CALL alcerr(irc,'iCarr',(/1,nFil/),srName)

    ! Difference level of the file
    ALLOCATE(nDiff(nFil),stat=irc)
    CALL alcerr(irc,'nDiff',(/nFil/),srName)

    ! Count observations
    ALLOCATE(satobs(nSat),stat=irc)
    CALL alcerr(irc,'satobs',(/nSat/),srName)
    satobs = 0d0


! Loop all file headers to get the input information to use GOBSEP
! ----------------------------------------------------------------
    DO iFil = 1,nFil

      CALL rdhead2(filLst(1,iFil),obshed)

! Set all other values
! --------------------
      filNum(iFil) = iFil

      window(:,iFil) = (/ 0d0, 1d20 /)

      timref(iFil) = obshed%timref
      ideltt(iFil) = obshed%ideltt
      ifrmat(iFil) = obshed%ifrmat
      meatyp(iFil) = obshed%meatyp
      nSatel(iFil) = obshed%nSatel
      numSat(1:maxsat,iFil) = 0
      numSat(1:nSatel(iFil),iFil) = obshed%sat(1:obshed%nSatel)%numSat
      nDiff(iFil)  = obshed%nDiff

      nFrFil(iFil) = 1
      iCarr(1,iFil)= 3

      ! Add the number of observations for each satellite
      DO iSat = 1,obsHed%nSatel
        DO jSat = 1,nSat
          IF (allSat(jSat) == obsHed%sat(iSat)%numSat) THEN
            satobs(jSat) = satobs(jSat) + obshed%sat(iSat)%numObs(1) + &
                                          obshed%sat(iSat)%numMrk(1)
            EXIT
          ENDIF
        ENDDO
      ENDDO

      DEALLOCATE(obsHed%sat,stat=irc)
      DEALLOCATE(obsHed%ambigu,stat=irc)

! Open the observation files
! --------------------------
      CALL opnfil(lfn001+iFil-1,filLst(2,iFIl),'OLD','UNFORMATTED', &
                  'READONLY',' ',irc)
      CALL opnerr(lfnerr,lfn001+iFil-1,irc,filLst(2,iFIl),srName)
    ENDDO

! Allocate and init all other arrays for GOBSEP
! ---------------------------------------------
    ! Synchro. interval (sec)
    dtSim = 1d0/86400d0

    ! Number of observations (per epoch and file)
    ALLOCATE(obsnum(nFil),stat=irc)
    CALL alcerr(irc,'obsnum',(/nFil/),srName)
    obsnum = 0

    ! Number of satellites (per epoch and file)
    ALLOCATE(nSatFl(nFil),stat=irc)
    CALL alcerr(irc,'nSatFl',(/nFil/),srName)
    nSatFl = 0

    ! Satellite numbers (per epoch and file)
    ALLOCATE(svnFil(maxSat,nFil),stat=irc)
    CALL alcerr(irc,'svnFil',(/maxSat,nFil/),srName)

    ! Observation flag (per epoch and file)
    ALLOCATE(obsFlg(maxSat,1,nFil),stat=irc)
    CALL alcerr(irc,'obsFlg',(/maxSat,1,nFil/),srName)

    ! Observation value (per epoch and file)
    ALLOCATE(observ(maxSat,1,nFil),stat=irc)
    CALL alcerr(irc,'observ',(/maxSat,1,nFil/),srName)

    ! Satellite numbers (per epoch and file, internal)
    ALLOCATE(svnFi1(maxSat,nFil),stat=irc)
    CALL alcerr(irc,'svnFi1',(/maxSat,nFil/),srName)

    ! Observation flag (per epoch and file, internal)
    ALLOCATE(obsFl1(maxSat,1,nFil),stat=irc)
    CALL alcerr(irc,'obsFl1',(/maxSat,1,nFil/),srName)

    ! Observation value (per epoch and file, internal)
    ALLOCATE(obser1(maxSat,1,nFil),stat=irc)
    CALL alcerr(irc,'obser1',(/maxSat,1,nFil/),srName)

    ! Clock corrections (per epoch and file)
    ALLOCATE(dtFil(2,nFil),stat=irc)
    CALL alcerr(irc,'dtFil',(/2,nFil/),srName)

    ! File Action (internal)
    ALLOCATE(filAct(nFil),stat=irc)
    CALL alcerr(irc,'filAct',(/nFil/),srName)

    ! Set the COMMON values
    mxcsat = maxsat
    mxcfrq = 1

    ! Observation records
    ALLOCATE(obsEpo(nFil),stat=irc)
    CALL alcerr(irc,'obsEpo',(/nFil/),srName)

    DO iFil = 1,nFil
      ALLOCATE(obsEpo(iFil)%obsRec(1),stat=irc)
      CALL alcerr(irc,'obsEpo(iFil)%obsRec',(/1/),srName)
    ENDDO

! Flag satellites with a "small" number of observations
! -----------------------------------------------------
    maxobs = 0
    DO iSat = 1,nSat
      iSys = allSat(iSat)/100+1
      IF (maxobs(iSys) == 0) THEN
        maxobs(iSys) = iSat
      ELSE IF (satobs(maxobs(iSys)) < satobs(iSat)) THEN
        maxobs(iSys) = iSat
      ENDIF
    ENDDO

    DO iSat = 1,nSat
      iSys = allSat(iSat)/100+1
      IF (maxobs(iSys) == 0) CYCLE
      IF (satobs(iSat) < satobs(maxobs(iSys)) * opt%obsSat) THEN
        satobs(iSat) = 0d0
        WRITE(lfnerr,'(/,A,I3,A,/)') &
        ' ### SR MCOBSI: Satellite ',allSat(iSat), &
        ' excluded from redundancy condition.'
      ENDIF
    ENDDO

! Define the frequencies
! ----------------------
    CALL defreq(timint%t,nSat,allSat)
  ENDIF

!
! PART 2: Restart with first epoch
! ======
!

  IF (iMode == 2) THEN

    ! Rewind the observation files
    DO iFil = 1,nFil
      REWIND(lfn001+iFil-1)
    ENDDO

    ! Init GOBSEP
    tLast = 0d0
    irc   = 0
  ENDIF

!
! PART 3: Read an epoch from obs. files
! ======
!

  IF (iMode == 0) THEN

    DO
      CALL gobsep(1     ,nFil  ,filNum,tLast ,window,dtSim , &
                  timref,ideltt,iFrmat,meaTyp,nSatel,numSat, &
                  nFrFil,iCarr ,     0,satshd,timshd,flgshd, &
                  tObs,  obsnum,nsatfl,svnfil,obsflg,observ, &
                  svnfi1,obsfl1,obser1,dtfil ,filact,ndiff , &
                  -1,0d0,dummy1,dummy2,irc)

      IF (irc /= 0) RETURN

      IF (opt%nsampl == 0) EXIT

      CALL mjdgps(tobs,gpssec,nweek)
      IF (DABS(gpssec-DNINT(gpssec/opt%nsampl)*opt%nsampl)<0.5d0) EXIT
    ENDDO

    DO iFil = 1,nFil
      obsEpo(iFil)%obsTim = tObs
      obsEpo(iFil)%epoFlg = ' '
      obsEpo(iFil)%deltat = 0D0
      IF (obsnum(iFil) == 0 .OR. filact(iFil) /= 'R') THEN
        obsEpo(iFil)%nSat = 0
      ELSE
        obsEpo(iFil)%nSat = 0
        DO iSat = 1,nsatfl(iFil)

          IF (opt%isasys /= 0 .AND. &
              opt%isasys /= svnFil(iSat,iFil)/100+1) CYCLE
          IF (tstflg(obsflg(iSat,1,iFil),0)) CYCLE
          IF (observ(iSat,1,iFil)   == 0d0 ) CYCLE

          obsEpo(iFil)%nSat = obsEpo(iFil)%nSat + 1
        ENDDO
      ENDIF

      IF (obsEpo(iFil)%nSat > 0) THEN
        IF (obsEpo(iFil)%nSat > SIZE(obsEpo(iFil)%obsRec)) THEN
          DEALLOCATE(obsEpo(iFil)%obsRec,stat=irc)

!!!!!!!!! Problems with ifc81 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CMP_IFC8
          nrSat = obsEpo(iFil)%nSat
          ALLOCATE(obsEpo(iFil)%obsRec(nrSat),stat=irc)
          CALL alcerr(irc,'obsEpo(iFil)%obsRec',(/nrSat/),srName)
#else
          ALLOCATE(obsEpo(iFil)%obsRec(obsEpo(iFil)%nSat),stat=irc)
          CALL alcerr(irc,'obsEpo(iFil)%obsRec',(/obsEpo(iFil)%nSat/),srName)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#endif
        ENDIF

        jSat = 0
        DO iSat = 1,nsatfl(iFil)
          IF (tstflg(obsflg(iSat,1,iFil),0)) CYCLE
          IF (observ(iSat,1,iFil)   == 0d0 ) CYCLE
          IF (opt%isasys /= 0 .AND. &
              opt%isasys /= svnFil(iSat,iFil)/100+1) CYCLE

          jSat = jSat + 1
          obsEpo(iFil)%obsRec(jSat)%numSat    = svnFil(iSat,iFil)
          obsEpo(iFil)%obsRec(jSat)%obsFlg(1) = obsflg(iSat,1,iFil)
          obsEpo(iFil)%obsRec(jSat)%observ(1) = observ(iSat,1,iFil)
        ENDDO
      ENDIF
    ENDDO
  ENDIF


!
! PART 4: Deallocate all local arrays
! ======
!
  IF (iMode == -1) THEN

    ! Close the observation files
    DO iFil = 1,nFil
      CLOSE(lfn001+iFil-1)
    ENDDO

    DEALLOCATE(filnum,stat=irc)
    DEALLOCATE(window,stat=irc)
    DEALLOCATE(timref,stat=irc)
    DEALLOCATE(ideltt,stat=irc)
    DEALLOCATE(ifrmat,stat=irc)
    DEALLOCATE(meatyp,stat=irc)
    DEALLOCATE(nSatel,stat=irc)
    DEALLOCATE(numSat,stat=irc)
    DEALLOCATE(nFrFil,stat=irc)
    DEALLOCATE(iCarr ,stat=irc)
    DEALLOCATE(nDiff ,stat=irc)
    DEALLOCATE(allSat,stat=irc)
    DEALLOCATE(obsnum,stat=irc)
    DEALLOCATE(nSatFl,stat=irc)
    DEALLOCATE(svnFil,stat=irc)
    DEALLOCATE(obsFlg,stat=irc)
    DEALLOCATE(observ,stat=irc)
    DEALLOCATE(svnFi1,stat=irc)
    DEALLOCATE(obsFl1,stat=irc)
    DEALLOCATE(obser1,stat=irc)
    DEALLOCATE(dtFil ,stat=irc)
    DEALLOCATE(filAct,stat=irc)
    DEALLOCATE(satobs,stat=irc)

    DO iFil = 1,nFil
      DEALLOCATE(obsEpo(iFil)%obsRec,stat=irc)
    ENDDO
    DEALLOCATE(obsEpo,stat=irc)
    NULLIFY(obsEpo)
  ENDIF

  RETURN
END SUBROUTINE mcobsi

END MODULE
