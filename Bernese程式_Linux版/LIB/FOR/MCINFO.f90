MODULE s_MCINFO
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE mcinfo(opt,nFil,filLst,filInfo,nSta,station,nClu,cluList)


! -------------------------------------------------------------------------
! Purpose:    Read file information for pgm mkclus
!             Find stations/baselines which are not to be included
!
! Author:     R. Dach
!
! Created:    13-Jun-2002
! Last mod.:  14-Mar-2012
!
! Changes:    20-Sep-2002 HU: Use interface to readstsg
!             23-Apr-2003 AJ: Nullify local pointers
!             16-May-2003 CU: Initialize structures
!             03-Feb-2006 RD: Read in any case 4 columns in GTFILE2
!             03-Nov-2010 PS: Error if nClu==0
!             14-Mar-2012 HB: Correct typo abservations => observations
!
! SR called:  init_stalist
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_gpsobs, ONLY: t_obshead, init_obshead
  USE d_stalst, ONLY: t_staList, init_stalist
  USE p_mkclus, ONLY: t_mkclus_opt,t_stafil,t_station,t_cluster
  USE s_gtfile2
  USE f_ikf
  USE s_alcerr
  USE s_mcdist
  USE s_rdhead2
  USE s_gtflna
  USE s_readstsg
  USE s_exitrc
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  TYPE(t_mkclus_opt)                       :: opt      ! Input options

! output
  INTEGER(i4b)                             :: nFil     ! Number of files
  CHARACTER(LEN=fileNameLength),            &
                    DIMENSION(:,:),POINTER :: filLst   ! File names
  TYPE(t_staFil),   DIMENSION(:),  POINTER :: filInfo  ! File information
  INTEGER(i4b)                             :: nSta     ! Number of stations to
                                                       ! put into the clusters
  TYPE(t_station),  DIMENSION(:),  POINTER :: station  ! Station records
  INTEGER(i4b)                             :: nClu     ! Number of files to
                                                       ! put into the clusters
  TYPE(t_cluster),  DIMENSION(:),  POINTER :: cluList  ! Cluster record

! Local parameters
! ----------------
  CHARACTER(LEN=6),              PARAMETER   :: srName = 'mcinfo'


! Local Variables
! ---------------
  TYPE(t_obshead)                            :: obsHed
  TYPE(t_staList)                            :: clkSig

  CHARACTER(LEN=fileNameLength)              :: rmsFil
  CHARACTER(LEN=lineLength)                  :: line

  INTEGER(i4b)                               :: iFil
  INTEGER(i4b)                               :: iSta,jSta
  INTEGER(i4b)                               :: iSat
  INTEGER(i4b)                               :: i1,i2
  INTEGER(i4b)                               :: irc

  REAL(r8b),   DIMENSION(:), POINTER         :: dist

  NULLIFY(dist)
  CALL init_obshead(obsHed)

! Get all input file names
! ------------------------
  IF (opt%cluStrat == 1 .OR. opt%cluStrat == 2) THEN
    CALL gtfile2('PZHFILES',4,nFil,filLst)

  ELSE IF (opt%cluStrat == 3) THEN
    CALL gtfile2('PSHFILES',4,nFil,filLst)

  ENDIF

! Allocate the file info record
! -----------------------------
  ALLOCATE(filInfo(nFil),stat=irc)
  CALL alcerr(irc,'filInfo',(/nFil/),srName)

  filInfo(:)%staIdx(1) = 0
  filInfo(:)%staIdx(2) = 0
  filInfo(:)%cluFlg    = 0

! Loop all files and get the station names
! ----------------------------------------
  DO iFil = 1,nFil

    CALL rdHead2(filLst(1,iFil),obsHed)

    ! Allocate the station record
    IF (iFil == 1) THEN
      nSta = 0
      IF (obsHed%nDiff == 0) THEN
        ALLOCATE(station(nFil),stat=irc)
        CALL alcerr(irc,'station',(/nFil/),srName)
      ELSE
        ALLOCATE(station(2*nFil),stat=irc)
        CALL alcerr(irc,'station',(/2*nFil/),srName)
      ENDIF
    ENDIF


    ! Copy difference level
    filInfo(iFil)%nDiff = obsHed%nDiff


    ! Get station names
    DO iSta = 1,obsHed%nDiff+1
      DO jSta = 1,nSta
        IF (station(jSta)%stanam == obsHed%sta(iSta)%staNam) &
          filInfo(iFil)%staIdx(iSta) = jSta
      ENDDO
      IF (filInfo(iFil)%staIdx(iSta) == 0) THEN
        nSta = nSta+1
        station(nSta)%staNam = obsHed%sta(iSta)%staNam
        filInfo(iFil)%staIdx(iSta) = nSta
      ENDIF
    ENDDO

    ! Get number of ambiguties
    filInfo(iFil)%numAmb = obsHed%numAmb

    ! Get number of observations
    filInfo(iFil)%numObs = 0
    DO iSat = 1,obshed%nSatel
      IF (obshed%sat(iSta)%numObs(1) <= obshed%sat(iSat)%numObs(2)) THEN
        filInfo(iFil)%numObs = filInfo(iFil)%numObs + obshed%sat(iSat)%numObs(1)
      ELSE
        filInfo(iFil)%numObs = filInfo(iFil)%numObs + obshed%sat(iSat)%numObs(2)
      ENDIF
    ENDDO

    DEALLOCATE(obsHed%sat,stat=irc)
    DEALLOCATE(obsHed%ambigu,stat=irc)

  ENDDO

! Read the clock sigmas
! ---------------------
  station(:)%clkRMS = 0d0

  IF (opt%cluStrat == 1 .OR. opt%cluStrat == 2) THEN

    CALL gtflna(0,'RMSFIT',rmsFil,irc)

    IF (irc == 0 .AND. LEN_TRIM(rmsFil) > 0) THEN
      CALL init_stalist(clkSig)
      CALL readstsg(rmsFil,1,clkSig)

      DO iSta = 1, clkSig%nSta
        DO jSta = 1,nSta
          IF (clkSig%staNam(iSta) == station(jSta)%staNam) &
            station(jSta)%clkRMS = clkSig%sigma(1,iSta)
        ENDDO
      ENDDO

      DEALLOCATE(clkSig%staNam, stat=irc)
      DEALLOCATE(clkSig%sigma, stat=irc)
    ENDIF

  ENDIF

! Flag files with problems
! ------------------------
  nClu = 0
  DO iFil = 1,nFil

! Too many ambiguities
    IF (opt%maxamb /= 0 .AND. filInfo(iFil)%numAmb > opt%maxamb) THEN
      filInfo(iFil)%cluFlg = -1

! Too few observations
    ELSE IF (opt%minobs /= 0 .AND. filInfo(iFil)%numObs < opt%minObs) THEN
      filInfo(iFil)%cluFlg = -2

! Too bad clock RMS
    ELSE IF (opt%maxRMS /= 0d0 .AND. &
             station(filInfo(iFil)%staIdx(1))%clkRMS > opt%maxRMS) THEN
      filInfo(iFil)%cluFlg = -3

! Station is OK
    ELSE
      filInfo(iFil)%cluFlg = 0

      nClu = nClu + 1
    ENDIF

  ENDDO

! REPORT THE ERRORS in the program output
! ---------------------------------------
  IF (nClu /= nFil) THEN
    WRITE(lfnprt,'(/,2(/,A))') &
    ' STATIONS TO BE DELETED:',' ----------------------'
  ENDIF

  line = ' '

! error -1: too many ambiguities
! ------------------------------
  i1 = -25
  line = ' '
  DO iFil = 1,nFil

    IF (filInfo(iFil)%cluFlg /= -1) CYCLE

    iSta = filInfo(iFil)%staIdx(1)
    jSta = filInfo(iFil)%staIdx(2)

    ! Output for zero difference case
    IF (opt%cluStrat == 1 .OR. opt%cluStrat == 2) THEN
      IF (i1 == -25) WRITE(lfnprt,'(2(/,A))')                        &
          ' Stations with too many ambiguities:           (numAmb)', &
          ' ----------------------------------'

      i1 = i1 + 27
      IF (i1 > 80-23) THEN
        WRITE(lfnprt,'(A)') TRIM(line)
        line = ' '
        i1 = 2
      ENDIF
      i2 = i1+23
      WRITE(line(i1:i2),'(A,I5,A)') &
            station(iSta)%stanam // ' (',filInfo(iFil)%numAmb,')'

    ! Output for baseline case
    ELSE IF (opt%cluStrat == 3) THEN
      IF (i1 == -25) THEN
        i1 = 0

        WRITE(lfnprt,'(2(/,A))')                                     &
          ' Baselines with too many ambiguities:          (numAmb)', &
          ' -----------------------------------'

        WRITE(lfnerr,'(/,A,2(/,16X,A),/)')                                    &
          ' ### SR MCINFO: At least one baseline will not be in the network', &
          'because too many ambiguities found.',                              &
          'The network configuration will not be optimal anymore.'
      ENDIF

      WRITE(lfnprt,'(1X,A,I5,A)') &
            station(iSta)%staNam // ' -- ' // station(jSta)%staNam // &
            ' (',filInfo(iFil)%numAmb,')'

    ENDIF
  ENDDO

  IF (LEN_TRIM(line) > 0) WRITE(lfnprt,'(A)') TRIM(line)
  IF (i1 /= -25) WRITE(lfnprt,*)

! error -2: too few observations
! ------------------------------
  i1 = -25
  line = ' '
  DO iFil = 1,nFil

    IF (filInfo(iFil)%cluFlg /= -2) CYCLE

    iSta = filInfo(iFil)%staIdx(1)
    jSta = filInfo(iFil)%staIdx(2)

    ! Output for zero difference case
    IF (opt%cluStrat == 1 .OR. opt%cluStrat == 2) THEN
      IF (i1 == -25) WRITE(lfnprt,'(2(/,A))')                        &
          ' Stations with a small number of observations: (numObs)', &
          ' --------------------------------------------'

      i1 = i1 + 27
      IF (i1 > 80-23) THEN
        WRITE(lfnprt,'(A)') TRIM(line)
        line = ' '
        i1 = 2
      ENDIF
      i2 = i1+23
      WRITE(line(i1:i2),'(A,I5,A)') &
            station(iSta)%staNam // ' (',filInfo(iFil)%numObs,')'

    ! Output for baseline case
    ELSE IF (opt%cluStrat == 3) THEN
      IF (i1 == -25) THEN
        i1 = 0

        WRITE(lfnprt,'(2(/,A))')                                     &
          ' Baselines with a small number of observations:(numObs)', &
          ' ---------------------------------------------'

        WRITE(lfnerr,'(/,A,2(/,16X,A),/)')                                    &
          ' ### SR MCINFO: At least one baseline will not be in the network', &
          'because a small number of observations found.',                    &
          'The network configuration will not be optimal anymore.'
      ENDIF

      WRITE(lfnprt,'(1X,A,I5,A)') &
            station(iSta)%staNam // ' -- ' // station(jSta)%staNam // &
            ' (',filInfo(iFil)%numObs,')'

    ENDIF
  ENDDO
  IF (LEN_TRIM(line) > 0) WRITE(lfnprt,'(A)') TRIM(line)
  IF (i1 /= -25) WRITE(lfnprt,*)

! error -3: big clock RMS
! -----------------------
  i1 = -25
  line = ' '
  DO iFil = 1,nFil

    IF (filInfo(iFil)%cluFlg /= -3) CYCLE

    iSta = filInfo(iFil)%staIdx(1)

    IF (i1 == -25) WRITE(lfnprt,'(2(/,A))')           &
        ' Stations with a bad clock:             ' // &
                 '       (rms of lin. fit - usec)',   &
        ' -------------------------'

    i1 = i1 + 27
    IF (i1 > 80-23) THEN
      WRITE(lfnprt,'(A)') TRIM(line)
      line = ' '
      i1 = 2
    ENDIF
    i2 = i1+23
    IF (station(iSta)%clkRMS < 100000d0) THEN
      WRITE(line(i1:i2),'(A,I5,A)') &
            station(iSta)%staNam // ' (',IDNINT(station(iSta)%clkRMS),')'
    ELSE
      WRITE(line(i1:i2),'(A)') station(iSta)%staNam // ' (-> oo)'
    ENDIF
  ENDDO
  IF (LEN_TRIM(line) > 0) WRITE(lfnprt,'(A)') TRIM(line)
  IF (i1 /= -25) WRITE(lfnprt,*)


! Allocate the cluster record
! ---------------------------
  ALLOCATE(cluList(nClu),stat=irc)
  CALL alcerr(irc,'cluList',(/nClu/),srName)

  cluList(:)%cluster = 0
  cluList(:)%delFlg  = 0

  nClu = 0
  DO iFil = 1,nFil
    IF (filInfo(iFil)%cluFlg == 0) THEN
      nClu = nClu+1
      cluList(nClu)%filIdx = iFil
    ENDIF
  ENDDO

  IF (nClu == 0) THEN
    WRITE(lfnerr,'(/,A,/)')           &
      ' *** SR MCINFO: nClu == 0. Adjust your input options.'
    CALL exitrc(2)
  ENDIF


! Compute the baseline length
! ---------------------------
  CALL mcdist(nSta,station,dist)

  DO iFil = 1,nFil

    IF (filInfo(iFIl)%nDiff == 0) CYCLE

    iSta = filInfo(iFil)%staIdx(1)
    jSta = filInfo(iFil)%staIdx(2)

    filInfo(iFil)%length = dist(IKF(iSta,jSta))
  ENDDO

  DEALLOCATE(dist,stat=irc)

END SUBROUTINE mcinfo

END MODULE
