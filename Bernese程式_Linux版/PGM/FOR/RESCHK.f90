
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM reschk

! -------------------------------------------------------------------------
! Purpose:    Check RESRMS summary to detect bad satellites and bad stations
!
! Author:     R. Dach
!
! Created:    04-Dec-2000
!
! Changes:    24-Apr-2001 RD: Satellites with no observations
!             05-Sep-2001 HU: Dynamic allocation of stanam arrays
!             30-Sep-2001 HU: Interface of gtstab moved to I_ASTLIB
!             06-Oct-2001 HU: Interface of gtstab moved back to I_GPSLIB
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             30-Apr-2002 MM: Add a short summary
!             01-May-2002 MM: Different summary if only one iteration
!             25-Sep-2002 HU: Remove i_astlib
!             04-Nov-2002 RD: Bad sat. and sat w/o obs. in one run
!             11-Mar-2003 RD: No update of a station info file
!             18-Mar-2003 RD: Use structure for station abbreviations
!             26-Mar-2003 RD: Only one warning per abbreviation set
!             23-Apr-2003 HU: Nullify local pointers
!             16-May-2003 AJ: Initialize structure
!             11-Aug-2003 RD: Remove "READONLY" for summary file
!             03-Nov-2003 RD: Call SR gtabbv instead of SR getabb
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             30-Aug-2005 MM: Minimum observations implemented
!             27-Feb-2007 AG: Call DEFCON
!             18-Aug-2010 RD: RMS ratio for satellites and stations
!             23-Sep-2010 RD: Enable CPU counter
!             16-May-2011 HB/SL: tmpRms1/2 included due to problems with pgf90
!             14-Nov-2011 SL: use m_bern with ONLY, PRITIT call added
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, shortLineLength, lfnPrt, lfnErr, lfnLoc
  USE m_cpu,    ONLY: cpu_start
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE d_abbrev, ONLY: t_abbrev,init_abbrev
  USE p_reschk, ONLY: t_reschk_opt,t_rmssta,t_rmssat,t_badsta,t_badsat
  USE f_prtRms
  USE s_gtabbv
  USE s_opnfil
  USE s_rcsatchk
  USE s_readabb
  USE s_readinpf
  USE s_opnerr
  USE s_rcstachk
  USE s_rcressum
  USE s_rcsatcrx
  USE s_rcinpt
  USE s_exitrc
  USE s_opnsys
  USE s_defcon
  USE s_pritit

  IMPLICIT NONE

!
! Local Variables
! ---------------
  TYPE(t_abbrev)                        :: abbrev
  TYPE(t_reschk_opt)                    :: opt
  TYPE(t_rmssta), DIMENSION(:), POINTER :: RMSSTA
  TYPE(t_rmssat), DIMENSION(:), POINTER :: RMSSAT

  INTEGER(i4b)                          :: iSat   ! Counter for satellites
  INTEGER(i4b)                          :: jSat   ! Counter for satellites
  INTEGER(i4b)                          :: nSat   ! Number of satellites in list
  INTEGER(i4b)                          :: iSta   ! Counter for stations
  INTEGER(i4b)                          :: kSta   ! Counter for stations
  INTEGER(i4b)                          :: nSta   ! Number of stations in list
  INTEGER(i4b)                          :: mSta   ! Number of stations in abb. list
  INTEGER(i4b),   DIMENSION(:), POINTER :: abbIdx ! Index in abbreviation list
  INTEGER(i4b)                          :: nMan   ! Number of maneuver satellites
  TYPE(t_badsat), DIMENSION(:), POINTER :: MANSAT ! index of man. satellites
  INTEGER(i4b)                          :: iBad   ! Counter
  INTEGER(i4b)                          :: nBad   ! Number of bad satellites
  TYPE(t_badsat), DIMENSION(:), POINTER :: BADSAT ! index of bad satellites
  INTEGER(i4b)                          :: sBad   ! Number of bad stations
  TYPE(t_badsta), DIMENSION(:), POINTER :: BADSTA ! index of bad stations
  INTEGER(i4b)                          :: i1
  INTEGER(i4b)                          :: irc    ! SR return code
  INTEGER(i4b)                          :: dummy  ! a dummy variable
  REAL(r8b), DIMENSION(2)               :: obs    ! perc. of observations
  INTEGER(i4b), DIMENSION(2)            :: obsAll ! sum of observations
  INTEGER(i4b), DIMENSION(2)            :: numObs ! number of observations
  INTEGER(i4b)                          :: nIter  ! num of iterations
  CHARACTER(LEN=shortLineLength)        :: line
  CHARACTER(LEN=5)                      :: tmpRms1,tmpRms2

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Nullify pointers
! ----------------
  CALL init_abbrev(abbrev)
  CALL init_inpkey(inpKey)
  NULLIFY(RMSSTA)
  NULLIFY(RMSSAT)
  NULLIFY(MANSAT)
  NULLIFY(BADSAT)
  NULLIFY(BADSTA)

! Get the name of the input file
! ------------------------------
  CALL readinpf(' ',inpKey)

! Open system files, define constants
! -----------------------------------
  CALL opnsys
  CALL defcon(0)
  CALL pritit('RESCHK','Detect misbehaving stations/satellites')
!
! Read the input options
! ----------------------
  CALL rcinpt(opt, irc)
  IF (irc /= 0) CALL exitrc(2)

!
! Read the RESRMS Summary File
! ----------------------------
  CALL rcressum(opt, nSta, rmssta, nSat, rmssat, nIter, irc)
  IF (irc /= 0) CALL EXITRC(2)

!
! Get station name abbrev.table
! -----------------------------
  IF (LEN_TRIM(opt%ABBPAN) > 0) THEN
    CALL readAbb(opt%abbpan,abbrev)
    abbrev%abb(:)%remark = ' '
    NULLIFY(abbIdx)
    DO iSta=1,nSta
      DO kSta=1,2
        IF (kSta == 2 .AND. opt%nDiff == 0) EXIT
        CALL gtabbv(0,rmsSta(iSta)%StaNam(kSta),2,opt%abbpan,abbrev,mSta,abbIdx)
        IF (mSta == 0) THEN
          WRITE(lfnerr,'(/,A,/,16X,A,/)') &
                ' ### PG RESCHK: Station not found in abbreviation table', &
                                'Abbreviation:  ' // TRIM(rmsSta(iSta)%StaNam(kSta))
        ELSE IF (mSta > 1) THEN
          IF (LEN_TRIM(abbrev%abb(abbIdx(1))%remark) == 0) THEN
            WRITE(lfnerr,'(/,A,/,16X,A,/)') &
                ' ### PG RESCHK: More than one station found in abbrevia' // &
                                                             'tion table',   &
                      'Abbreviation:  ' // TRIM(rmsSta(iSta)%StaNam(kSta))
            abbrev%abb(abbIdx(1))%remark = 'warning wrote'
          ENDIF
          rmsSta(iSta)%StaNam(kSta) = abbrev%abb(abbIdx(1))%stanam
        ELSE
          rmsSta(iSta)%StaNam(kSta) = abbrev%abb(abbIdx(1))%stanam
        ENDIF
      ENDDO
    ENDDO
    DEALLOCATE(abbrev%abb,stat=irc)
    DEALLOCATE(abbIdx,stat=irc)
  ENDIF

!
! Get a list of bad/man. satellites
! ---------------------------------
  nMan=0
  nBad=0
  IF (opt%mansat) THEN
    CALL rcsatchk(opt, nSat, rmssat, nMan, ManSat, nBad, BadSat, irc)
!
    IF (irc /= 0) THEN
      WRITE(lfnerr,'(/,A,/)')  &
        ' ### PGM RESCHK: THE SATELLITE CHECK COULD NOT BE DONE CORRECT.'
      nMan=0
      nBad=0
    ENDIF
!
  ENDIF

!
! Get a list of bad stations
! --------------------------
  sBad=0
  IF (opt%badsta) THEN
    CALL rcstachk(opt, nSta, rmssta, rmssat(nSat+1)%rms(1), sBad, BadSta, irc)
!
    IF (irc /= 0) THEN
      WRITE(lfnerr,'(/,A,/)')  &
        ' ### PGM RESCHK: THE STATION CHECK COULD NOT BE DONE CORRECT.'
      sBad=0
    ENDIF
!
  ENDIF

! Decide whether bad station/satellite has the preference
! -------------------------------------------------------
  IF (nMan == 0 .AND. sBad > 0 .AND. nBad > 0) THEN

    WRITE(lfnprt,'(/,A,/,A,//,A,/,A)')                                   &
            'SPECIAL EVENTS FOUND IN THIS SOLUTION:',                    &
            '-------------------------------------',                     &
            '  NUM   STATION NAME/SATELLITE          RMS/RMSTOT',        &
            '------------------------------------------------------------------------------'
    iSta = 0
    DO iBad = 1,sBad
      WRITE(lfnprt,'(I5,3X,A20,F19.3)') &
            iBad,badSta(iBad)%stanam(1:20),badSta(iBad)%rmsRatio
      IF (iSta == 0) THEN
        iSta = iBad
      ELSEIF ( badSta(iBad)%rmsRatio > badSta(iSta)%rmsRatio) THEN
        iSta = iBad
      ENDIF
    ENDDO

    iSat = 0
    DO iBad = 1,nBad
      WRITE(lfnprt,'(I5,I6,17X,F19.3)') &
            sBad+iBad,badsat(iBad)%satnum,badSat(iBad)%rmsRatio
      IF (iSat == 0) THEN
        iSat = iBad
      ELSEIF ( badSat(iBad)%rmsRatio > badSat(iSat)%rmsRatio) THEN
        iSat = iBad
      ENDIF
    ENDDO

    IF (badSat(iSat)%rmsRatio > badSta(iSta)%rmsRatio) THEN
      WRITE(lfnprt,'(A,/,3X,A,/,A,/)') &
            '------------------------------------------------------------------------------', &
            'Preference: remove bad satellite(s)',&
            '------------------------------------------------------------------------------'
      sBad = 0
    ELSE
      WRITE(lfnprt,'(A,/,3X,A,/,A,/)') &
            '------------------------------------------------------------------------------', &
            'Preference: remove bad station(s)',&
            '------------------------------------------------------------------------------'
    ENDIF
  ENDIF

! A maneuver was detected
! -----------------------
  IF (nMan > 0) THEN
!
! write protocol
    WRITE(lfnprt,'(/,A,/,A,//,A,50I5)')                                  &
            'SPECIAL EVENTS FOUND IN THIS SOLUTION:',                    &
            '-------------------------------------',                     &
            'MANEUVER DETECTED FOR SATELLITE(s): ',                      &
            (mansat(isat)%satnum,isat=1,nMan)
    WRITE(lfnprt,*)
!
! update satellite problem file
    IF (LEN_TRIM(opt%SATCRUX) > 0) THEN
      CALL rcsatcrx(opt%SATCRUX, 'MAN', nSat, RMSsat, nMan, manSat, irc)
!
      IF (irc /= 0) THEN
        WRITE(lfnerr,'(/,A,/,17X,A,A,/)')                                    &
          ' ### PGM RESCHK: THE SATELLITE PROBLEM FILE WAS NOT MODIFIED.', &
                           'FILE NAME: ',TRIM(opt%SATCRUX)
      ENDIF
    ENDIF
!
! A bad station was detected
! -----------------------
  ELSE IF (sBad > 0) THEN
!
! write protocol
    WRITE(lfnprt,'(/,A,/,A,//,A,/,A,//,A,/,A)')                          &
            'SPECIAL EVENTS FOUND IN THIS SOLUTION:',                    &
            '-------------------------------------',                     &
            'BAD STATION(S) DETECTED: ',                                 &
            '-----------------------',                                   &
            '  NUM   STATION NAME           FILE NAME',                  &
            '------------------------------------------------------------------------------'
    DO iSta=1,sBad
      WRITE(lfnprt,'(I5,3X,A20,3X,A)')                                   &
        iSta, badsta(ista)%stanam(1:20),                                 &
        TRIM(badsta(ista)%filnam)//'.???'
    ENDDO
    WRITE(lfnprt,'(A,/)')                                                &
            '------------------------------------------------------------------------------'
!
! write station delete file
    IF (LEN_TRIM(opt%delsta) > 0) THEN
!
      CALL opnfil(lfnloc,opt%delsta,'NEW','FORMATTED', ' ',' ',irc)
      CALL opnerr(lfnerr,lfnloc,irc,opt%delsta,'RESCHK')
!
      IF (irc == 0) THEN
        DO iSta=1,sBad
          IF (LEN_TRIM(opt%ext_czh) > 0) &
            WRITE(lfnloc,*)TRIM(opt%dir_czh)// &
                 TRIM(badsta(ista)%filnam)//'.'//TRIM(opt%ext_czh)
!
          IF (LEN_TRIM(opt%ext_czo) > 0) &
            WRITE(lfnloc,*)TRIM(opt%dir_czo)// &
                  TRIM(badsta(ista)%filnam)//'.'//TRIM(opt%ext_czo)
!
          IF (LEN_TRIM(opt%ext_pzh) > 0) &
            WRITE(lfnloc,*)TRIM(opt%dir_pzh)// &
                  TRIM(badsta(ista)%filnam)//'.'//TRIM(opt%ext_pzh)
!
          IF (LEN_TRIM(opt%ext_pzo) > 0) &
            WRITE(lfnloc,*)TRIM(opt%dir_pzo)// &
                  TRIM(badsta(ista)%filnam)//'.'//TRIM(opt%ext_pzo)
        ENDDO
      ENDIF
      CLOSE(lfnloc)
    ENDIF

!
! A bad satellites detected
! -------------------------
  ELSE IF (nBad > 0) THEN

    ! write protocol
    WRITE(lfnprt,'(/,A,/,A,/)')                                  &
           'SPECIAL EVENTS FOUND IN THIS SOLUTION:',             &
           '-------------------------------------'

    ! Bad satellites (RMS)
    jSat=1
    line = 'BAD SATELLITE(s)             :'
    DO iSat=1,nBad
      jSat=jSat*badSat(iSat)%Flag
      IF (badSat(iSat)%Flag == 0) THEN
        i1 = LEN_TRIM(line)+1
        IF ( i1+5 > LEN(line) ) THEN
          WRITE(lfnprt,'(A)') TRIM(line)
          line = ''
          i1 = 31
        ENDIF
        WRITE(line(i1:i1+5),'(I5,1X)') badsat(isat)%satnum
      ENDIF
    ENDDO

    IF (jSat == 0) WRITE(lfnprt,'(A)') TRIM(line)

    ! Bad satellites (no obs.)
    jSat=0
    line = 'SATELLITE(s) W/O OBSERVATIONS:'
    DO iSat=1,nBad
      IF (badSat(iSat)%Flag == 1) THEN
        jSat= jSat+1
        i1 = LEN_TRIM(line)+1
        IF ( i1+5 > LEN(line) ) THEN
          WRITE(lfnprt,'(A)') TRIM(line)
          line = ''
          i1 = 31
        ENDIF
        WRITE(line(i1:i1+5),'(I5,1X)') badsat(isat)%satnum
      ENDIF
    ENDDO

    IF (jSat > 0) WRITE(lfnprt,'(A)') TRIM(line)

    ! Bad satellites (not enough obs.)
    jSat=0
    line = 'SATELLITE(s) WITH FEW OBSERVATIONS:'
    DO iSat=1,nBad
      IF (badSat(iSat)%Flag == 2) THEN
        jSat=jSat+1
        i1 = LEN_TRIM(line)+1
        IF ( i1+5 > LEN(line) ) THEN
          WRITE(lfnprt,'(A)') TRIM(line)
          line = ''
          i1 = 36
        ENDIF
        WRITE(line(i1:i1+5),'(I5,1X)') badsat(isat)%satnum
      ENDIF
    ENDDO

    IF (jSat > 0) WRITE(lfnprt,'(A)') TRIM(line)

    WRITE(lfnprt,*)
!
! update satellite problem file
    IF (LEN_TRIM(opt%SATCRUX) > 0) THEN
      CALL rcsatcrx(opt%SATCRUX, 'BAD', nSat, RMSsat, nBad, BadSat, irc)
!
      IF (irc /= 0) THEN
        WRITE(lfnerr,'(/,A,/,17X,A,A,/)')                                    &
          ' ### PGM RESCHK: THE SATELLITE PROBLEM FILE WAS NOT MODIFIED.', &
                           'FILE NAME: ',TRIM(opt%SATCRUX)
      ENDIF
    ENDIF

!
! Nothing detected
! ----------------
  ELSE
!
! write protocol
    WRITE(lfnprt,'(/,A,/)') 'NO SPECIAL EVENTS FOUND IN THIS SOLUTION'
!
  ENDIF
!
! Write the short summary file (if name available)
! ------------------------------------------------
  IF (LEN_TRIM(opt%sumout) > 0) THEN
    CALL OPNFIL(lfnloc,opt%sumout,'NEW','FORMATTED', ' ',' ',irc)
    CALL OPNERR(lfnerr,lfnloc,irc,opt%sumout,'RESCHK')

    WRITE(lfnloc,'(3(A,/),A)')                                                          &
     '--------------------------------------------------------------------------------',&
     'PRN | % Observations   Difference   | # Observations   Difference   | RMS       ',&
     '    | before   after   abs      rel | before    after  abs      rel | bef    aft',&
     '----+-------------------------------+-------------------------------+-----------'

! Loop over all satellites
    DO iSat=1,nSat
      obs(1)    = rmsSat(iSat)%obs(1)
      obs(2)    = rmsSat(iSat)%obs(2)
      numObs(1) = rmsSat(iSat)%numObs(1)
      numObs(2) = rmsSat(iSat)%numObs(2)
      IF (obs(1) == 0) CYCLE

      IF (nIter == 2) THEN
        tmpRms1 = prtRms(rmsSat(iSat)%rms(1))
        tmpRms2 = prtRms(rmsSat(iSat)%rms(2))
        WRITE(lfnloc,'(I3,A,2(F7.2,1X),2F7.2,A,2(I7,1X),I7,F7.2,A,A5,1X,A5)') &
          rmsSat(iSat)%satNum, ' |',                                              &
          obs(1), obs(2), obs(2)-obs(1), (obs(2)-obs(1))/obs(1)*100.D0, ' |',     &
          numObs(1), numObs(2),                                                   &
          numObs(2)-numObs(1), (numObs(2)-numObs(1))*100.D0/numObs(1), ' |',      &
          tmpRms1 , tmpRms2
      ELSEIF (nIter == 1) THEN
        tmpRms1 = prtRms(rmsSat(iSat)%rms(1))
        WRITE(lfnloc,'(I3,A,F7.2,A,I7,A,A5,6X)')                                &
          rmsSat(iSat)%satNum, ' |',                                              &
          obs(1), '                        |',                                    &
          numObs(1), '                        |',                                 &
          tmpRms1
      ENDIF
    ENDDO

    obsAll(1) = SUM(rmssat(1:nSat)%numObs(1))
    obsAll(2) = SUM(rmssat(1:nSat)%numObs(2))

    WRITE(lfnloc,'(A)')                                                      &
     '----+-------------------------------+-------------------------------+-----------'

    IF (nIter == 2) THEN
      tmpRms1 = prtRms(rmsSat(nSat+1)%rms(1))
      tmpRms2 = prtRms(rmsSat(nSat+1)%rms(2))
      WRITE(lfnloc,'(A,3(I7,1X),F6.2,A,A5,1X,A5)')                           &
        'TOT | 100.00  100.00    0.00   0.00 |',                             &
        obsAll(1), obsAll(2),                                                &
        obsAll(2)-obsAll(1), (obsAll(2)-obsAll(1))*100.D0/obsAll(1), ' |',   &
        tmpRms1, tmpRms2
    ELSEIF (nIter == 1) THEN
      tmpRms1 = prtRms(rmsSat(nSat+1)%rms(1))
      WRITE(lfnloc,'(A,I7,A,A5,6X)')                                         &
        'TOT | 100.00                        |',                             &
        obsAll(1), '                        |',                              &
        tmpRms1
    ENDIF

    WRITE(lfnloc,'(A,/)')                                                    &
     '--------------------------------------------------------------------------------'
    CLOSE(lfnloc)
  ENDIF
!
! Clean up
! --------
  DEALLOCATE(mansat,stat=Dummy)
  DEALLOCATE(badsat,stat=Dummy)
  DEALLOCATE(badsta,stat=Dummy)
  DEALLOCATE(rmssat,stat=Dummy)
  DEALLOCATE(rmssta,stat=Dummy)
!
  CALL exitrc(0)
!
END PROGRAM reschk
