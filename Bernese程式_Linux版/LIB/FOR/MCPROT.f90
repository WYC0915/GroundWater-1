MODULE s_MCPROT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE mcprot(opt,nFil,filLst,filInfo, &
                    nSta,station,nClu,cluList,nDel,delList)

! -------------------------------------------------------------------------
! Purpose:    Generate the program output of the results
!
! Author:     R. Dach
!
! Created:    13-Jun-2002
!
! Changes:    22-Aug-2002 JJ: Format statement corrected
!             13-Feb-2003 RD: Optimum number of stations
!             19-Mar-2003 RD: Write long string with format (because IFC)
!                             Replace environment variables in file name
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnprt, lfnerr, lfnloc, &
                      fileNameLength, fileExtLength, lineLength
  USE p_mkclus, ONLY: t_mkclus_opt,t_staFil,t_station,t_cluster
  USE s_alcerr
  USE s_opnfil
  USE s_opnerr
  USE s_rplenvar
  USE s_fparse
  USE s_gtflna
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  TYPE(t_mkclus_opt)                    :: opt      ! Input options
  INTEGER(i4b)                          :: nFil     ! Number of files
  CHARACTER(LEN=fileNameLength),            &
                 DIMENSION(:,:),POINTER :: filLst   ! File names
  TYPE(t_staFil),DIMENSION(:),  POINTER :: filInfo  ! File information
  INTEGER(i4b)                          :: nSta     ! Number of stations
  TYPE(t_station),DIMENSION(:), POINTER :: station  ! Station information record
  INTEGER(i4b)                          :: nClu     ! Number of files to
                                                    ! put into the clusters
  TYPE(t_cluster),DIMENSION(:), POINTER :: cluList  ! Cluster record
  INTEGER(i4b)                          :: nDel     ! Number of stations to
                                                    ! be deleted from clusters
  TYPE(t_cluster), DIMENSION(:),POINTER :: delList  ! Station deletion list

! output


! List of functions
! -----------------

! Local types
! -----------

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER                :: srName = 'mcprot'

! Local Variables
! ---------------
  CHARACTER(LEN=lineLength)                  :: line
  CHARACTER(LEN=lineLength)                  :: filNam
  CHARACTER(LEN=fileNameLength)              :: delFil
  CHARACTER(LEN=fileNameLength)              :: clbFil
  CHARACTER(LEN=fileNameLength)              :: clbFil0,clbFil1
  CHARACTER(LEN=fileNameLength)              :: node     ! Used by FPARSE
  CHARACTER(LEN=fileNameLength)              :: device   ! Used by FPARSE
  CHARACTER(LEN=fileNameLength)              :: dir      ! Used by FPARSE
  CHARACTER(LEN=fileNameLength)              :: name     ! Used by FPARSE
  CHARACTER(LEN=fileExtLength)               :: ext      ! Used by FPARSE
  CHARACTER(LEN=fileExtLength)               :: ver      ! Used by FPARSE

  INTEGER(i4b), DIMENSION(:),  ALLOCATABLE   :: staIdx
  INTEGER(i4b), DIMENSION(:,:),ALLOCATABLE   :: staClu
  INTEGER(i4b)                               :: iClu
  INTEGER(i4b)                               :: iFil
  INTEGER(i4b)                               :: iSta,jSta,kSta
  INTEGER(i4b)                               :: mSta
  INTEGER(i4b)                               :: allSta
  INTEGER(i4b)                               :: i1,i2,ii
  INTEGER(i4b)                               :: ircClb
  INTEGER(i4b)                               :: irc

  LOGICAL                                    :: sorted

!
! PART 1:
! ======
!
! Report deleted stations
! -----------------------
  IF (nDel > 0 .AND. &
      (opt%cluStrat == 1 .OR. opt%cluStrat == 2)) THEN

! Make an index for stations in deletion list (for sorted output)
! ---------------------------------------------------------------
    ALLOCATE(staIdx(nDel),stat=irc)
    CALL alcerr(irc,'staIdx',(/nDel/),srName)

    staIdx = (/ (iSta,iSta=1,nDel) /)

    sorted = .FALSE.
    DO WHILE (.NOT. sorted)
      sorted = .TRUE.
      DO iSta = 1,nDel-1
        i1 = filInfo(delList(staIdx(iSta))%filIdx)%staIdx(1)
        i2 = filInfo(delList(staIdx(iSta+1))%filIdx)%staIdx(1)
        IF (station(i1)%staNam > station(i2)%staNam) THEN
          sorted = .FALSE.

          i1 = staIdx(iSta)
          staIdx(iSta) = staIdx(iSta+1)
          staIdx(iSta+1) = i1
        ENDIF
      ENDDO
    ENDDO

! Write the protocol title
! ------------------------
    IF (opt%staStrat == 1) WRITE(lfnprt,'(/,2(/,A))')                       &
      ' EXCLUDED BECAUSE TOO MANY STATIONS IN LIST:   (numObs)',            &
      ' ------------------------------------------'
    IF (opt%staStrat == 2) WRITE(lfnprt,'(/,2(/,A))')                       &
      ' EXCLUDED BECAUSE TOO MANY STATIONS IN LIST:   '//&
                                    '(rms of lin. fit - usec)',             &
      ' ------------------------------------------'
    IF (opt%staStrat == 3) WRITE(lfnprt,'(/,2(/,A))')                       &
      ' EXCLUDED BECAUSE TOO MANY STATIONS IN LIST:   (numObs)',            &
      ' ------------------------------------------'
    IF (opt%staStrat == 4) WRITE(lfnprt,'(/,2(/,A))')                       &
      ' EXCLUDED BECAUSE TOO MANY STATIONS IN LIST:   (numObs)',            &
      ' ------------------------------------------'

! Write the protocol lines:
! -------------------------
    DO iClu = 1,opt%numClu

      i1 = -25
      line = ' '

      DO iSta = 1,nDel

        IF (delList(staIdx(iSta))%cluster /= iClu) CYCLE

        IF (i1 == -25) WRITE(lfnprt,'(/,A,I3,/,A)') &
                       ' Stations deleted from cluster ',iClu, &
                       ' ---------------------------------'

        iFil = delList(staIdx(iSta))%filIdx
        jSta = filInfo(iFil)%staIdx(1)

        i1 = i1 + 27
        IF (i1 > 80-23) THEN
          WRITE(lfnprt,'(A)') TRIM(line)
          line = ' '
          i1 = 2
        ENDIF

        i2 = i1+23

        ! Max observations strategy or
        ! Network geometry strategy or
        ! Network density strategy
        IF (opt%staStrat == 1 .OR. opt%staStrat == 3 .OR. opt%staStrat == 4)    &
          WRITE(line(i1:i2),'(A,I5,A)')                  &
            station(jSta)%staNam // ' (',filInfo(iFil)%numObs,')'

        ! Best clock strategy
        IF (opt%staStrat == 2) THEN
          IF (station(jSta)%clkRMS < 100000d0) THEN
            WRITE(line(i1:i2),'(A,I5,A)')        &
                station(jSta)%staNam // ' (',IDNINT(station(jSta)%clkRMS),')'
          ELSE
            WRITE(line(i1:i2),'(A)') station(jSta)%staNam // ' (-> oo)'
          ENDIF
        ENDIF
      ENDDO
      IF (LEN_TRIM(line) > 0) WRITE(lfnprt,'(A)') TRIM(line)
    ENDDO ! Next cluster

    WRITE(lfnprt,*)

    DEALLOCATE(staIdx,stat=irc)
  ENDIF

! Make the station index (for sorted output)
! ------------------------------------------
  ALLOCATE(staIdx(nSta),stat=irc)
  CALL alcerr(irc,'staIdx',(/nSta/),srName)

  staIdx = (/ (iSta,iSta=1,nSta) /)

  sorted = .FALSE.
  DO WHILE (.NOT. sorted)
    sorted = .TRUE.
    DO iSta = 1,nSta-1
      IF (station(staIdx(iSta))%staNam > station(staIdx(iSta+1))%staNam) THEN
        sorted = .FALSE.

        i1 = staIdx(iSta)
        staIdx(iSta) = staIdx(iSta+1)
        staIdx(iSta+1) = i1
      ENDIF
    ENDDO
  ENDDO

!
! PART 2:
! =======
!
! Report stations added to the clusters
! -------------------------------------
  line = ' '
  i1 = -25

  IF (opt%cluStrat == 1 .OR. opt%cluStrat == 2) THEN

    DO iSta = 1, nSta

      mSta = 0
      DO jSta = 1, nClu

        IF (staIdx(iSta) == filInfo(cluList(jSta)%filIdx)%staIdx(1)) &
          mSta = mSta+1

      ENDDO

      ! Station is only once in list
      IF (mSta <= 1) CYCLE

      IF (i1 == -25)  THEN
        WRITE(lfnprt,*)
        WRITE(lfnprt,*)
        WRITE(lfnprt,*) 'MULTIPLE USED STATIONS:                     ' // &
                        '(number of cluster)'
        WRITE(lfnprt,*) '----------------------'
        WRITE(lfnprt,*)
      ENDIF

      i1 = i1 + 27
      IF (i1 > 80-23) THEN
        WRITE(lfnprt,'(A)') TRIM(line)
        line = ' '
        i1 = 2
      ENDIF

      i2 = i1+23
      WRITE(line(i1:i2),'(A,I5,A)') &
           station(staIdx(iSta))%staNam // ' (',mSta,')'
    ENDDO

!
! Report baselines added to be clusters
! -------------------------------------
  ELSE IF (opt%cluStrat == 3) THEN
    DO iSta = 1, nFil

      mSta = 0
      DO jSta = 1, nClu

        IF (iSta == cluList(jSta)%filIdx) mSta = mSta+1

      ENDDO

      ! Baseline is only once in list
      IF (mSta <= 1) CYCLE

      IF (i1 == -25)  THEN
        WRITE(lfnprt,*)
        WRITE(lfnprt,*)
        WRITE(lfnprt,*) 'MULTIPLE USED BASELINES:                    ' // &
                        '(number of cluster)'
        WRITE(lfnprt,*) '----------------------'
        WRITE(lfnprt,*)
        i1 = 0
      ENDIF

      WRITE(lfnprt,'(1X,A,I5,A)') &
            station(filInfo(iSta)%staIdx(1))%staNam // ' -- ' // &
            station(filInfo(iSta)%staIdx(2))%staNam  // ' (',mSta,')'

    ENDDO
  ENDIF

  IF (LEN_TRIM(line) > 0) WRITE(lfnprt,'(A)') TRIM(line)

!
! PART 3:
! ======
!
! Station / Cluster summary: Write title
! --------------------------------------
  WRITE(lfnprt,*)
  WRITE(lfnprt,*)
  WRITE(lfnprt,*) 'STATION / CLUSTER SUMMARY:'
  WRITE(lfnprt,*) '-------------------------'
  WRITE(lfnprt,*)

  line = '     Station name'

  DO iClu = 1,opt%numClu

    i2 = iClu*4 + 23
    i1 = i2-2

    WRITE(line(i1:i2),'(I3)') iClu

  ENDDO

  i1 = opt%numClu*4 + 24
  i2 = i1 + 4
  WRITE(line(i1:i2),'(2X,A)') 'tot'

  WRITE(lfnprt,'(1X,A)') TRIM(line)

  i1 = LEN_TRIM(line)
  line = '-'
  DO WHILE (LEN_TRIM(line) < i1)
    i2 = LEN_TRIM(line)+1
    line(i2:i2) = '-'
  ENDDO

  WRITE(lfnprt,'(1X,A)') TRIM(line)


! Allocate and init the statistic array
! -------------------------------------
  ALLOCATE(staClu(opt%numClu,nSta),stat=irc)
  CALL alcerr(irc,'staClu',(/opt%numClu,nSta/),srName)

  staClu = 0

! Generate the statistic
! (One station may be twice or more in station list)
! --------------------------------------------------
  DO iSta = 1,nClu
    IF (cluList(iSta)%cluster == 0) CYCLE
    iFil = cluList(iSta)%filIdx
    DO jSta = 1,filInfo(iFil)%nDiff+1
      staClu(cluList(iSta)%cluster,filInfo(iFil)%staIdx(jSta)) = 1
    ENDDO
  ENDDO

! Print the summary list
! ----------------------
  kSta = 0
  DO iSta = 1,nSta

    mSta = 0
    DO iClu = 1,opt%numClu
      mSta = mSta + staClu(iClu,staIdx(iSta))
    ENDDO

    IF (mSta == 0) CYCLE

    kSta = kSta+1
    line = ' '
    WRITE(line,'(I3,2X,A)') kSta,station(staIdx(iSta))%staNam

    DO iClu = 1,opt%numClu
      IF (staClu(iClu,staIdx(iSta)) == 0) CYCLE

      i1 = iClu*4 + 23
      WRITE(line(i1:i1),'(A)') 'x'

    ENDDO

    i1 = opt%numClu*4 + 24
    i2 = i1 + 4
    WRITE(line(i1:i2),'(I5)') mSta

    WRITE(lfnprt,'(1X,A)') TRIM(line)

  ENDDO

! Finish the table
! ----------------
  ! Line
  i1 = LEN_TRIM(line)
  line = '-'
  DO WHILE (LEN_TRIM(line) < i1)
    i2 = LEN_TRIM(line)+1
    line(i2:i2) = '-'
  ENDDO

  WRITE(lfnprt,'(1X,A)') TRIM(line)

  ! Total
  line = 'Number of stations'

  allSta = 0
  DO iClu = 1,opt%numClu

    mSta = 0
    DO iSta = 1,nSta
      mSta = mSta + staClu(iClu,iSta)
    ENDDO

    i2 = iClu*4 + 23
    i1 = i2-2

    WRITE(line(i1:i2),'(I3)') mSta

    allSta = allSta+mSta

  ENDDO

  i1 = opt%numClu*4 + 24
  i2 = i1 + 4
  WRITE(line(i1:i2),'(I5)') allSta

  WRITE(lfnprt,'(1X,A)') TRIM(line)

  ! Number of baselines per cluster
  IF (opt%cluStrat == 3) THEN
    ! Total
    line = 'Number of baselines'

    allSta = 0
    DO iClu = 1,opt%numClu

      mSta = 0
      DO iSTa = 1,nClu
        IF (cluList(iSta)%cluster == iClu) mSta = mSta+1
      ENDDO

      i2 = iClu*4 + 23
      i1 = i2-2

      WRITE(line(i1:i2),'(I3)') mSta

      allSta = allSta+mSta

    ENDDO

    i1 = opt%numClu*4 + 24
    i2 = i1 + 4
    WRITE(line(i1:i2),'(I5)') allSta

    WRITE(lfnprt,'(1X,A)') TRIM(line)

  ENDIF

  ! Line
  i1 = LEN_TRIM(line)
  line = '-'
  DO WHILE (LEN_TRIM(line) < i1)
    i2 = LEN_TRIM(line)+1
    line(i2:i2) = '-'
  ENDDO

  WRITE(lfnprt,'(1X,A,//)') TRIM(line)


!
! PART 4:
! ======
!
! Prepare cluster assignement file
! --------------------------------
  CALL gtflna(0,'SELBSL',clbFil,ircClb)
  IF (ircClb == 0) THEN
    ircClb = -1
    CALL fparse(1,clbFil,node,device,dir,name,ext,ver,irc)
    clbFil0 = ' '
    IF (irc == 0) THEN
      clbFil0 = TRIM(node) // TRIM(device) // TRIM(dir) // TRIM(name)
      ircClb = LEN_TRIM(clbFil0)
      IF (clbFil0(ircClb:ircClb) == '.') THEN
        clbFil1 = clbFil0(ircClb:ircClb) // TRIM(ext) // TRIM(ver)
        clbFil0(ircClb:ircClb) = ' '
      ELSE
        clbFil1 = TRIM(ext) // TRIM(ver)
      ENDIF
    ENDIF
    IF (LEN_TRIM(clbFil0) == 0 .OR. LEN_TRIM(clbFil1) == 0) ircClb = -1
  ELSE
    ircClb = -1
  ENDIF

! Report the result: Station list per cluster
! -------------------------------------------
  WRITE(lfnprt,*) 'CLUSTER LISTING:'
  WRITE(lfnprt,*) '---------------'

  DO iClu = 1,opt%numClu

! Generate the cluster assign. file
! ---------------------------------
    clbFil = '(not saved)'
    IF (ircClb /= -1) THEN
      line = ' '
      WRITE(line,'(A,I2.2,A)') TRIM(clbFil0),iClu,TRIM(clbFil1)
      clbFil = line

      CALL opnfil(lfnloc,clbFil,'UNKNOWN','FORMATTED',' ',' ',irc)
      CALL opnerr(lfnerr,lfnloc,irc,clbFil,srName)

      DO iSta = 1,nClu
        IF (cluList(iSta)%cluster == iClu) THEN
          filnam = filLst(1,cluList(iSta)%filIdx)
          CALL rplenvar(1,filnam)
          WRITE(lfnloc,'(A)') TRIM(filnam)
        ENDIF
      ENDDO

      CLOSE(lfnloc)
    ENDIF

! Write the stations/baselines into protocol
! ------------------------------------------
    WRITE(lfnprt,'(/,A,I3,A,/,A)')          &
          ' Stations in Cluster ',iClu, ':     ' // TRIM(clbFil),&
          ' -----------------------'
    line = ' '
    i1 = -18
    DO iSta = 1, nSta

      DO jSta = 1,nClu
        IF (cluList(jSta)%cluster /= iClu) CYCLE

        iFil = cluList(jSta)%filIdx
        IF (filInfo(iFil)%nDiff == 0) THEN
          IF (filInfo(iFil)%staIdx(1) /= staIdx(iSta)) CYCLE
        ELSE
          IF (filInfo(iFil)%staIdx(1) /= staIdx(iSta) .AND. &
              filInfo(iFil)%staIdx(2) /= staIdx(iSta)) CYCLE
        ENDIF

        i1 = i1 + 20
        IF (i1 > 80-15) THEN
          WRITE(lfnprt,'(A)') TRIM(line)
          line = ' '
          i1 = 2
        ENDIF
        i2 = i1+15
        WRITE(line(i1:i2),'(A)') station(staIdx(iSta))%staNam
        EXIT
      ENDDO
    ENDDO
    WRITE(lfnprt,'(A)') TRIM(line)
  ENDDO

  WRITE(lfnprt,*)
  WRITE(lfnprt,*)

! Deallocate the local arrays
! ---------------------------
  DEALLOCATE(staClu,stat=irc)
  DEALLOCATE(staIdx,stat=irc)

! Write the files with the observation files to be deleted
! --------------------------------------------------------
  IF (opt%cluStrat == 1 .OR. opt%cluStrat == 2) THEN
    CALL gtflna(0,'NOTUSE',delFil,irc)
    IF (LEN_TRIM(delFil) > 0 .AND. irc == 0) THEN
      CALL opnfil(lfnloc,delFil,'UNKNOWN','FORMATTED',' ',' ',irc)
      CALL opnerr(lfnerr,lfnloc,irc,delFil,srName)

      ! Too many ambiguities
      ! Too few observations
      ! Bad station clock
      DO iFil = 1,nFil
        IF (filInfo(iFil)%cluFlg < 0) THEN
          DO ii=1,2+opt%codDel*2
            filnam = filLst(ii,iFil)
            CALL rplenvar(1,filnam)
            WRITE(lfnloc,'(A)') TRIM(filnam)
          ENDDO
        ENDIF
      ENDDO

      ! Removed because too many stations
      DO iSta = 1,nDel
        IF (opt%cluStrat == 3) EXIT   ! Not relevant for baselines
        iFil = delList(iSta)%filIdx
        DO jSta = 1,nClu
          IF (cluList(jSta)%filIdx == iFil) iFil = 0
        ENDDO
        IF (iFil == 0) CYCLE
        DO ii=1,2+opt%codDel*2
          filnam = filLst(ii,iFil)
          CALL rplenvar(1,filnam)
          WRITE(lfnloc,'(A)') TRIM(filnam)
        ENDDO
      ENDDO

      CLOSE(lfnloc)
    ENDIF
  ENDIF

  RETURN
END SUBROUTINE mcprot

END MODULE
