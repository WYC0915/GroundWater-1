MODULE s_RCRESSUM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rcressum(opt, nSta, rmssta, nSat, rmssat, nIter, irCode)

! -------------------------------------------------------------------------
! Purpose:    Reads RESRMS summary file file for RESCHK
!
! Author:     R. Dach
!
! Created:    04-Dec-2000
!
! Changes:    18-Jan-2001 RD: Handle a '****' output
!             25-Jan-2001 RD: Fix a bug from '****' handling
!             29-May-2001 RD: use sr alcerr for allocation
!             15-Aug-2001 RD: New MENU_EXT (separate path for the file types)
!             21-Dec-2001 HU: Use m_bern, other modules with ONLY
!             24-Apr-2002 MM: Read "TOT OBS", compute perc. from "TOT OBS"
!             26-Apr-2002 MM: Read "before" RMS
!             01-May-2002 MM: number of iterations in RESRMS summary
!             25-Sep-2002 HU: Remove i_astlib
!             26-Mar-2003 MM: Allow for no observations
!             14-Apr-2003 RD: Use structure for session table
!             20-Sep-2006 RD: Increase linelength for RESRMS summary file
!             20-Jul-2010 RD: More digits in RESRMS output table
!             24-May-2012 RD: Init variables in pointer structure
!             24-May-2012 RD: Use m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnloc, lfnerr, longLineLength
  USE d_sess,   ONLY: t_sesLst,init_sesLst
  USE p_reschk, ONLY: t_reschk_opt,t_rmssta,t_rmssat
  USE s_opnfil
  USE s_alcerr
  USE s_sestim
  USE s_rdsess
  USE s_exitrc
  USE s_opnerr
  IMPLICIT NONE
!
! Variables from parameter list
! -----------------------------
  TYPE(t_reschk_opt)                    :: opt    ! program input options
  INTEGER(i4b)                          :: nSta   ! Number of Stations
  TYPE(t_rmssta), DIMENSION(:), POINTER :: RMSSTA ! Sta. information rec.
  INTEGER(i4b)                          :: nSat   ! Number of Satellites
  TYPE(t_rmssat), DIMENSION(:), POINTER :: RMSSAT ! Sat. information rec.
  INTEGER(i4b)                          :: irCode ! return code
!
! Local Parameters
! ----------------
  INTEGER(i4b),     PARAMETER         :: maxses = 100 ! max. number of sessions
!
! Local Variables
! ---------------
  TYPE(t_sesLst)                      :: sesTbl   ! Session table

  CHARACTER(LEN=4)                    :: sessid   ! list of session numbers
!
  CHARACTER(LEN=longLineLength)       :: line     ! A file input line
!
  INTEGER(i4b)              :: iSta  ! Counter for Stations
  INTEGER(i4b)              :: numSta! Counter for Stations
  INTEGER(i4b)              :: iSat  ! Counter for Satellites
  INTEGER(i4b)              :: jSat  ! Counter for Satellites
  INTEGER(i4b)              :: rstat ! read status of the file
  INTEGER(i4b)              :: irc   ! return code from called SR
  INTEGER(i4b)              :: ios   ! io-status
  INTEGER(i4b)              :: i1,i2 ! string index variables
  INTEGER(i4b)              :: dummy ! a dummy variable
  INTEGER(i4b)              :: expo  ! exponent for tot obs
  INTEGER(i4b)              :: redObs! "reduced" number of obs
  INTEGER(i4b)              :: nIter ! number of iterations in RESRMS summary
!
  REAL(r8b), DIMENSION(2)   :: obsAll! sum of observations

!
! Initialization
! --------------
  irCode=0
  rstat=0
  DEALLOCATE(rmssat,stat=Dummy)
  DEALLOCATE(rmssta,stat=Dummy)

  CALL init_sesLst(sesTbl)

! Read the session table
! ----------------------
  CALL rdsess(opt%sesFile, sesTbl)

!
! open the RESRMS summary file
! ----------------------------
  CALL OPNFIL(lfnloc,opt%RESRMS,'OLD','FORMATTED', 'READONLY',' ',irc)
  CALL OPNERR(lfnerr,lfnloc,irc,opt%RESRMS,'RCRESSUM')
!
  IF (irc /= 0) CALL EXITRC(2)
!
! Count the number of stations
! ----------------------------
  nIter  = 1
  numSta = 0
  iSta   = 0
  irc    = 0
  DO WHILE (irc == 0)
    READ(lfnloc,'(A)',iostat=irc) Line
    iSta=iSta+1
    IF (INDEX(Line,'BASELINE  SESS') /= 0) iSta=0
    IF (INDEX(Line,'TOTAL RMS:') /= 0 .AND. iSta>numSta) numSta=iSta
  ENDDO

  ALLOCATE(rmssta(numSta), stat=ios)
  CALL alcerr(ios,'rmssta',(/numSta/),'RCRESSUM')
  iSta=0
!
! Reopen the summary file
! -----------------------
  CLOSE(lfnloc)
  CALL OPNFIL(lfnloc,opt%RESRMS,'OLD','FORMATTED', 'READONLY',' ',irc)
  CALL OPNERR(lfnerr,lfnloc,irc,opt%RESRMS,'RCRESSUM')
  IF (irc /= 0) CALL EXITRC(2)
!
! Read the data from file
! -----------------------
  DO WHILE (rstat /= 99)
    READ(lfnloc,'(A)',iostat=irc) Line
!
! End of file
! -----------
    IF (irc/=0) THEN
      rstat=99
!
! Read the numbers of satellites
! ------------------------------
    ELSE IF (INDEX(Line,'BASELINE  SESS') /= 0 .AND. rstat == 0) THEN
!
! A further GPSEDT iteration was done
! -----------------------------------
      rstat=rstat+1
      nSat=(LEN_TRIM(Line)-21)/6
      iSta=0
      DEALLOCATE(rmssat,stat=Dummy)
      DEALLOCATE(rmssta,stat=Dummy)
      ALLOCATE(rmssta(numSta), stat=ios)
      CALL alcerr(ios,'rmssta',(/numSta/),'RCRESSUM')
      DO i1 = 1,2
        rmssta(:)%stanam(i1)='' ! Station IDs
        rmssta(:)%filnam(i1)='' ! Session ID
      ENDDO
      rmssta(:)%RMS  = 0d0  ! Baseline/Station total

      ALLOCATE(rmssat(nsat+1), stat=ios)
      CALL alcerr(ios,'rmssat',(/nsat+1/),'RCRESSUM')
      rmssat(:)%SATNUM = 0
      DO i1 = 1,2
        rmssat(:)%rms(i1) = 0d0
        rmssat(:)%obs(i1) = 0d0
        rmssat(:)%numobs(i1) = 0
        rmssat(:)%satarc(i1) = 0d0
      ENDDO

      sessid='AAAA'
!
      DO iSat=1,nSat
        I1=10+iSat*6
        I2=15+iSat*6
        READ(Line(I1:I2),*,iostat=ios) rmssat(iSat)%satnum
        IF (ios /= 0) THEN
          write(lfnerr,'(/,A,/,18X,A,A,/)')                              &
                ' *** SR RCRESSUM: ERROR READING RESRMS SUMMARY FILE',   &
                                  'FILE NAME: ',TRIM(opt%RESRMS)
          irCode=1
          rstat=99
        ENDIF
        rmssat(iSat)%satarc(1)=99D99
        rmssat(iSat)%satarc(2)=0D0
      ENDDO
!
! Read the total RMS per satellite
! --------------------------------
    ELSE IF (INDEX(Line,'TOTAL RMS:') /= 0 .AND.   &
             rstat >= 1 .AND. rstat <= 2) THEN
      DO iSat=1,nSat+1
        I1=10+iSat*6
        I2=15+iSat*6
        IF ( Line(I1:I2) == '******' ) THEN
          rmssat(iSat)%rms(rstat) = 99999
          ios=0
        ELSE
          READ(Line(I1:I2),*,iostat=ios) rmssat(iSat)%rms(rstat)
        ENDIF
        IF (ios /= 0) THEN
          write(lfnerr,'(/,A,/,18X,A,A,/)')                              &
                ' *** SR RCRESSUM: ERROR READING RESRMS SUMMARY FILE',   &
                                  'FILE NAME: ',TRIM(opt%RESRMS)
          irCode=1
          rstat=99
        ENDIF
      ENDDO
!
! Read the OBS percentage of a satellite (1)
! ------------------------------------------
    ELSE IF (INDEX(Line,'TOTAL OBS:') /= 0 .AND.   &
             rstat >= 1 .AND. rstat <= 2) THEN
!
! DO NOTHING
!
!      DO iSat=1,nSat
!        I1=10+iSat*6
!        I2=15+iSat*6
!        READ(Line(I1:I2),*,iostat=ios) rmssat(iSat)%obs(rstat)
!        IF (ios /= 0) THEN
!          write(lfnerr,'(/,A,/,18X,A,A,/)')                              &
!                ' *** SR RCRESSUM: ERROR READING RESRMS SUMMARY FILE',   &
!                                  'FILE NAME: ',TRIM(opt%RESRMS)
!          irCode=1
!          rstat=99
!        ENDIF
!      ENDDO
!      rstat = rstat+1
!      IF (rstat == 3 .AND. .NOT. opt%tstMode) rstat=0
!      IF (rstat == 3 .AND.       opt%tstMode) rstat=99

!
! Read total observations for a satellite
! ---------------------------------------
    ELSE IF (INDEX(Line,'TOT OBS') /= 0 .AND.        &
             rstat >= 1 .AND. rstat <=2) THEN
      IF (rstat == 2) nIter = 2
      rmsSat(:)%numObs(rstat) = 0
      READ(Line(13:13),"(I1)",iostat=ios) expo
      DO iSat=1,nSat
        I1=10+iSat*6
        I2=15+iSat*6
        READ(Line(I1:I2),*,iostat=ios) redObs
        rmssat(iSat)%numObs(rstat) = redObs*10**expo
        IF (ios /= 0) THEN
          write(lfnerr,'(/,A,/,18X,A,A,/)')                              &
                ' *** SR RCRESSUM: ERROR READING RESRMS SUMMARY FILE',   &
                                  'FILE NAME: ',TRIM(opt%RESRMS)
          irCode=1
          rstat=99
        ENDIF
      ENDDO
      rstat = rstat+1
      IF (rstat == 3 .AND. .NOT. opt%tstMode) rstat=0
      IF (rstat == 3 .AND.       opt%tstMode) rstat=99
!
! Read the total RMS for stations/baselines
! -----------------------------------------
    ELSE IF (Line(2:10) /= '---------' .AND. rstat == 1) THEN
      iSta=iSta+1
!
! A new session was found
! -----------------------
      IF (SessID /= Line(12:15)) THEN
        SessID=Line(12:15)
        CALL sestim(opt%sesFile,sesTbl,sessID,opt%iyear4,RMSsat(1)%satarc)

        IF (RMSSat(1)%satarc(1) == 0d0 .OR. RMSSat(1)%satarc(2) == 1d20) THEN
          CALL EXITRC(2)
        ELSE
          DO iSat=2,nSat
            DO jSat=1,2
              RMSSat(iSat)%satarc(jSat)=RMSSat(1)%satarc(jSat)
            ENDDO
          ENDDO
        ENDIF
      ENDIF

      rmssta(iSta)%staNam(1)=Line(2:5)
      rmssta(iSta)%filnam(1)=Line(2:5)//SessID
      IF (Line(7:10) /= '    ') THEN
        rmssta(iSta)%staNam(2)=Line(7:10)
        rmssta(iSta)%filnam(2)=Line(7:10)//SessID
      ENDIF
      I1=16+nSat*6
      I2=23+nSat*6
      IF (Line(I2:I2) == '*') Line(I2:I2)=' '
      READ(Line(I1:I2),*,iostat=ios) rmssta(iSta)%RMS
      IF (ios /= 0) THEN
        write(lfnerr,'(/,A,/,18X,A,A/)')                              &
              ' *** SR RCRESSUM: ERROR READING RESRMS SUMMARY FILE',   &
                                'FILE NAME: ',TRIM(opt%RESRMS)
        irCode=1
        rstat=99
      ENDIF

    ENDIF
  ENDDO
  nSta=iSta
  CLOSE(lfnloc)

! Deallocate session table
! ------------------------
  DEALLOCATE(sesTbl%sess,stat=irc)

! Compute relative obs percentage
! -------------------------------
  obsAll(1) = SUM(rmssat(:)%numObs(1))
  obsAll(2) = SUM(rmssat(:)%numObs(2))
  rmsSat(:)%obs(1) = 0.D0
  rmsSat(:)%obs(2) = 0.D0
  DO iSat=1,nSat
    IF (obsAll(1) /= 0) &
      rmssat(iSat)%obs(1) = rmssat(iSat)%numObs(1)*100.D0/obsAll(1)
    IF (obsAll(2) /= 0) &
      rmssat(iSat)%obs(2) = rmssat(iSat)%numObs(2)*100.D0/obsAll(2)
  ENDDO
!
  RETURN
END SUBROUTINE rcressum


END MODULE
