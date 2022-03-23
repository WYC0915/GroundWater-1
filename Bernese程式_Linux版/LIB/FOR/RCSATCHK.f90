MODULE s_RCSATCHK
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rcsatchk(opt, nSat, rmssat, nMan, ManSat, nBad, BadSat, irCode)

! -------------------------------------------------------------------------
! Purpose:    Checks RESRMS summary for bad/maneuver satellites for RESCHK
!
!
! Author:     R. Dach
!
! Created:    04-Dec-2000
!
! Changes:    15-Mar-2001 RD: Handle case without arcsplit file
!             24-Apr-2001 RD: Satellite w/o observations
!             14-May-2001 RD: Modified marked observation criterium
!             29-May-2001 RD: Use sr alcerr for allocation
!             28-Nov-2001 RD: Only 1 sat may be flaged as bad
!             12-Dec-2001 RD: Correct reading of CODXTR for several man-sats
!             21-Dec-2001 HU: Use m_bern, other modules with ONLY
!             27-Feb-2002 RD: Read CODXTR after maneuvers correct
!             25-Apr-2002 MM: Changes due to RMS before and after
!             04-Nov-2002 RD: Hold satellites w/o obs. in list if only
!                             1 sat. has to be marked
!             11-Feb-2003 MM: Warning text changed
!             19-Mar-2003 RD: Write long string with format (because IFC)
!             06-Apr-2005 RD: Handle satellites with "quasi no" observ.
!             18-Aug-2010 RD: RMS ratio for satellites and stations
!             05-Sep-2011 SL: new ASPLIT summary format, use m_bern with ONLY
!             27-Apr-2012 RD: Nullify all pointers
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lineLength, lfnPrt, lfnLoc, lfnErr
  USE p_reschk, ONLY: t_reschk_opt,t_rmssat,t_badsat,t_asplit

  USE s_alcerr
  USE s_opnfil
  USE s_opnerr
  IMPLICIT NONE
!
! Variables from parameter list
! -----------------------------
  TYPE(t_reschk_opt)                    :: opt    ! input options
  INTEGER(i4b)                          :: nSat   ! # of satellites in list
  TYPE(t_rmssat), DIMENSION(:), POINTER :: rmssat ! Sat. information rec.
  INTEGER(i4b)                          :: nMan   ! # of maneuver satellites
  TYPE(t_badsat), DIMENSION(:), POINTER :: ManSat ! List of maneuver satelites
  INTEGER(i4b)                          :: nBad   ! # of bad satellites
  TYPE(t_badsat), DIMENSION(:), POINTER :: BadSat ! List of bad satellites
  INTEGER(i4b)                          :: irCode ! return code of this SR
!
! Local variables
! ---------------
  TYPE(t_asplit), DIMENSION(:), POINTER :: asplit ! arc split information rec.
!
  CHARACTER(LEN=lineLength)             :: line   ! A file input line
!
  INTEGER(i4b)                          :: iSat   ! Counter for RMSSAT
  INTEGER(i4b)                          :: jSat   ! Counter for RMSSAT
  INTEGER(i4b)                          :: iMan   ! Counter for ManSat
  INTEGER(i4b)                          :: kMan   ! Counter for ManSat
  INTEGER(i4b)                          :: iBad   ! Counter for BadSat
  INTEGER(i4b)                          :: mBad   ! Counter for BadSat
  INTEGER(i4b)                          :: iSplit ! Counter for arc split rec.
  INTEGER(i4b)                          :: jSplit ! Counter for arc split rec.
  INTEGER(i4b)                          :: nSplit ! # of arc split rec.
  INTEGER(i4b)                          :: rstat  ! read status of the file
  INTEGER(i4b)                          :: irc,ios! SR return code
  INTEGER(i4b)                          :: i1,i2  ! string index variables
  INTEGER(i4b)                          :: satTmp ! sat number
!
  REAL(r8b)                             :: bigRMS ! Biggest RMS for a sat.
  REAL(r8b)                             :: maxDel ! Maximum percentage
  REAL(r8b)                             :: maxRRMS! Maximum RMS ratio
!
! Start the full protocoll
! ------------------------
  IF (opt%summary == 1)            &
    WRITE(lfnprt,'(//,A,/,A,/)')   &
    'PART 1: SATELLITE CHECK',     &
    '-----------------------'
!
! Iniatialization
! ---------------
  irCode=0
  NULLIFY(asplit)
  ALLOCATE(MANSAT(nSAT),stat=ios)
  CALL alcerr(ios,'MANSAT',(/nSAT/),'RCSATCHK')
  ALLOCATE(BADSAT(nSAT),stat=ios)
  CALL alcerr(ios,'BADSAT',(/nSAT/),'RCSATCHK')
  BADSAT%Flag=0
  MANSAT%Flag=0
!
! open the CODXTR summary file
! ----------------------------
  CALL OPNFIL(lfnloc,opt%CODXTR,'OLD','FORMATTED', 'READONLY',' ',irc)
  CALL OPNERR(lfnerr,lfnloc,irc,opt%CODXTR,'RCSATCHK')
!
  rstat= 0
  iMan = 0
  DO WHILE (irc == 0)
    READ(lfnloc, '(A)',iostat=irc) Line

    IF (irc /= 0) THEN
      rstat=999
!
! MANEUVER STRING DETECTED
! ------------------------
    ELSE IF (INDEX(Line,'### SATELLITE REPOSITIONING OR CLOCK RESET DETECTED!') /= 0) THEN
        rstat=rstat+1
!
! READ THE NUMBER OF MAN. SAT IN THIS LIST
! ----------------------------------------
    ELSE IF (INDEX(Line,'# SATELLITES    :') /= 0 .AND. rstat == 1) THEN
      READ(Line(23:LEN_TRIM(Line)), *, iostat=irc) nMAN
      IF (irc /= 0) THEN
        write(lfnerr,'(/,A,/,18X,A,A,/)')                              &
              ' *** SR RCSATCHK: ERROR READING CODXTR SUMMARY FILE',   &
                                'FILE NAME: ',TRIM(opt%CODXTR)
        irCode=1
        rstat=99
      ENDIF
!
! READ THE LIST OF MAN. SAT
! -------------------------
    ELSE IF (INDEX(Line,'SATELLITE NUMBER:') /= 0 .AND. rstat == 1) THEN
      iMan = iMan + 1
      READ(Line(23:LEN_TRIM(Line)), *, iostat=irc) MANSAT(iMan)%Satnum
      IF (iMan == nMan) rstat=1
      IF (irc /= 0) THEN
        write(lfnerr,'(/,A,/,18X,A,A,/)')                              &
              ' *** SR RCSATCHK: ERROR READING CODXTR SUMMARY FILE',   &
                                'FILE NAME: ',TRIM(opt%CODXTR)
        irCode=1
        rstat=99
      ENDIF
!
! Normal CODXTR output, stop reading in the test mode
! ---------------------------------------------------
    ELSE IF (INDEX(Line,'FILES, MAX. RMS:') /= 0 .AND. opt%tstMode) THEN
      rstat=99
!
! NORMAL CODXTR OUTPUT
! --------------------
    ELSE IF (INDEX(Line,'FILES, MAX. RMS:') /= 0 .AND. rstat == 1) THEN
      rstat=0
!
! MANEUVER WAS DETECTED IN AN EARLIER RUN
! ---------------------------------------
    ELSE IF (INDEX(Line,'FILES, MAX. RMS:') /= 0 .AND. rstat == 0) THEN
      nMan =0
    ENDIF
  ENDDO
!
  CLOSE(lfnloc)
!
! Get the index for the maneuver satellites
! -----------------------------------------
  ManSat(:)%iSat=-1
  DO iMan=1,nMan
    iSat=1
    DO WHILE (iSat <= nSat)
      IF (MANSAT(iMAN)%SatNum == RMSSAT(iSAT)%SATNUM) THEN
        MANSAT(iMan)%iSat=iSAT
        iSat=nSat
      ENDIF
      iSat=iSat+1
    ENDDO
    IF (MANSAT(iMan)%iSat < 0) THEN
      write(lfnerr,'(/,A,2(/,18X,A,A),/)')                                      &
            ' *** SR RCSATCHK: CODXTR AND RESRMS SUMMARY FILES DOES NOT MATCH', &
                              'CODXTR FILE NAME: ',TRIM(opt%CODXTR),            &
                              'RESRMS FILE NAME: ',TRIM(opt%RESRMS)
      irCode=2
    ENDIF
  ENDDO
!
! Write the full protocoll
! ------------------------
  IF (opt%summary == 1 .AND. nMan > 0 .AND. irCode == 0) THEN
    WRITE(lfnprt,'(/A,/,A,//,A,A,/,A,50I5)')                        &
            'MANEUVER HYPOTHESIS FROM CODXTR:',                     &
            '-------------------------------',                      &
            '  CODXTR FILE NAME: ',TRIM(opt%CODXTR),                &
            '  FOR SATELLITE(s): ',                                 &
            (mansat(isat)%satnum,isat=1,nMan)
    WRITE(lfnprt,*)
  ENDIF
!
!
! open the aSplit summary file
! ----------------------------
  ALLOCATE(aSplit(nSat),stat=ios)
  CALL alcerr(ios,'aSplit',(/nSat/),'RCSATCHK')
!
  nSplit=0
  rstat=0
!
  IF (LEN_TRIM(opt%aSplit) > 0) THEN
    CALL OPNFIL(lfnloc,opt%aSplit,'OLD','FORMATTED', 'READONLY',' ',irc)
    CALL OPNERR(lfnerr,lfnloc,irc,opt%aSplit,'RCSATCHK')
!
    DO WHILE (irc == 0 .AND. rstat == 0)
      READ(lfnloc, '(A)',iostat=irc) Line
!
      IF (irc /= 0) THEN
        rstat=999
!
! Stop reading in the test mode
! -----------------------------
      ELSE IF (INDEX(Line,'Arc splitting for') /= 0 &
               .AND. opt%tstMode .AND. nSplit /= 0) THEN
        rstat=99
!
! Read Number of Satellites in the aSplit file
! --------------------------------------------
      ELSE IF (INDEX(Line,'Arc splitting for') /= 0) THEN
        READ(Line(18:20), *, iostat=irc) nSplit
        IF (irc /= 0) THEN
          write(lfnerr,'(/,A,/,18X,A,A,/)')                              &
                ' *** SR RCSATCHK: ERROR READING aSplit SUMMARY FILE',   &
                                  'FILE NAME: ',TRIM(opt%aSplit)
          irCode=1
          rstat=99
        ENDIF
!
! Read list of satellite numbers from the aSplit file
! ---------------------------------------------------
      ELSE IF (INDEX(Line,'  Sat.    :') /= 0) THEN
        READ(Line(13:LEN_TRIM(Line)), *, iostat=irc) &
        (aSplit(iSplit)%SATNUM,iSplit=1,nSplit)
        IF (irc /= 0) THEN
          write(lfnerr,'(/,A,/,18X,A,A,/)')                              &
                ' *** SR RCSATCHK: ERROR READING aSplit SUMMARY FILE',   &
                                  'FILE NAME: ',TRIM(opt%aSplit)
          irCode=1
          rstat=99
        ENDIF
!
! Read list of splitting rms from the aSplit file
! -----------------------------------------------
      ELSE IF (INDEX(Line,'  RMS (mm):') /= 0) THEN
        DO WHILE (INDEX(Line,'****') /= 0)
          I1=INDEX(Line,'****')
          I2=I1+3
          WRITE(Line(I1:I2),'(I4)') 9999
        ENDDO
!
        DO iSplit=1,nSplit
          I1=9+iSplit*4
          I2=13+iSplit*4
          READ(Line(I1:I2), *, iostat=irc) aSplit(iSplit)%RMS
          IF (irc /= 0 .AND. rstat /= 99) THEN
            write(lfnerr,'(/,A,/,18X,A,A,/)')                              &
                  ' *** SR RCSATCHK: ERROR READING aSplit SUMMARY FILE',   &
                                    'FILE NAME: ',TRIM(opt%aSplit)
            irCode=1
            rstat=99
          ENDIF
        ENDDO
      ENDIF
    ENDDO
!
    CLOSE(lfnloc)
  ENDIF
!
! Check aSplit RMS values for Maneuvers
! -------------------------------------
  jSplit=0
  DO iSplit=1,nSplit
    aSplit(iSplit)%SatNum=-aSplit(iSplit)%SatNum
    IF (aSplit(iSplit)%RMS >= opt%MAXORB) THEN
!
! Write the full protocoll
! ------------------------
      IF (opt%summary == 1 .AND. irCode == 0) THEN
        jSplit=jSplit+1
        IF (jSplit == 1) THEN
          WRITE(lfnprt,'(/,A,/,A,//,A,A,/,A,/,A)')             &
                'MANEUVER HYPOTHESIS FROM ARC SPLIT: ',        &
                '----------------------------------',          &
                '  ARC SPLIT FILE NAME:',TRIM(opt%ASPLIT),     &
                '  NUM   SATELLITE   RMS FROM ARC SPLIT     LIMIT FOR MANEUVER', &
                '------------------------------------------------------------------------------'
        ENDIF
        WRITE(lfnprt,'(I5,3X,I5,F18.0,F22.0)') &
             jSplit,-aSplit(iSplit)%SatNum,aSplit(iSplit)%RMS,opt%MAXORB
      ENDIF
      iSat=1
      DO WHILE (iSat <= nSat)
!
! Add a sat. to the maneuver list
! -------------------------------
        IF (RMSSAT(iSat)%SatNum == -aSplit(iSplit)%SatNum) THEN
          aSplit(iSplit)%satNum=iSat
          iMan=1
          DO WHILE (iMan <= nMan)
            IF (ManSat(iMan)%iSat == iSat) iMan=nMan+9999
            iMan=iMan+1
          ENDDO
          IF (iMan <= 10000) THEN
            nMan=nMan+1
            ManSat(nMan)%SatNum=RMSSat(iSat)%SatNum
            ManSat(nMan)%iSat=iSat
          ENDIF
          iSat=nSat
        ENDIF
        iSat=iSat+1
      ENDDO
      IF (aSplit(iSplit)%SatNum < 0) THEN
        write(lfnerr,'(/,A,2(/,18X,A,A),/)')                                      &
              ' *** SR RCSATCHK: ASPLIT AND RESRMS SUMMARY FILES DOES NOT MATCH', &
                                'ASPLIT FILE NAME: ',TRIM(opt%ASPLIT),            &
                                'RESRMS FILE NAME: ',TRIM(opt%RESRMS)
        irCode=2
      ENDIF
    ENDIF
  ENDDO
!
  DEALLOCATE(aSplit)
!
! Finish this part of the full protocoll
! --------------------------------------
  IF (opt%summary == 1 .AND. jSplit > 0 .AND. irCode == 0)          &
    WRITE(lfnprt,'(A,/)')                                           &
            '------------------------------------------------------------------------------'
!
! A "MANEUVER" sat shows a normal behaviour (e.g. clock maintaintance)
! --------------------------------------------------------------------
  kMan=0
  iMan=nMan
  DO WHILE (iMan>=1)
    IF (RMSSAT(ManSat(iMan)%iSat)%RMS(1)/RMSSAT(nSat+1)%RMS(1) < opt%MAXRRMS) THEN
!
! Report it in the full protocoll
! -------------------------------
      IF (opt%summary == 1 .AND. irCode == 0) THEN
        kMan=kMan+1
        IF (kMan == 1) THEN
          WRITE(lfnprt,'(/,A,/,A,//,A,/,A)')                                     &
                'DISMISS THE MANEUVER HYPOTHESIS (NORMAL RESULTS IN RESRMS): ',  &
                '----------------------------------------------------------',    &
                '  NUM   SATELLITE   RMS RATIO IN RESRMS    LIMIT FOR BAD SAT.', &
                '------------------------------------------------------------------------------'
        ENDIF
        WRITE(lfnprt,'(I5,3X,I5,F18.1,F22.1)') &
          kMan,ManSat(iMan)%satNum,RMSSAT(ManSat(iMan)%iSat)%RMS(1)/RMSSAT(nSat+1)%RMS(1), opt%MAXRRMS
      ENDIF
!
      IF (iMan<nMan) ManSat(iMan:nMan-1)=ManSat(iMan+1:nMan)
!
      nMan=nMan-1
    ENDIF
    iMan=iMan-1
  ENDDO
!
! Finish this part of the full protocoll
! --------------------------------------
  IF (opt%summary == 1 .AND. kMan > 0)                                   &
    WRITE(lfnprt,'(A,/)')                                                &
            '------------------------------------------------------------------------------'
!
! Use "Maneuver options" in the case of a maneuver
! ------------------------------------------------
  IF (nMan > 0) THEN
    opt%MAXRRMS=opt%MAXMRMS
    opt%MAXDEL=999.99
!
! Write a notice into the full protocoll
! --------------------------------------
    IF (opt%summary == 1) THEN
      WRITE(lfnprt,'(/,A,/,A,//,A,F7.2,/,A,/)')                        &
            'USE THE MANEUVER HYPOTHESIS FOR CHECKING THE RESULTS: ',  &
            '----------------------------------------------------',    &
            '  RMS RATIO IN RESRMS               : ',opt%maxRRMS,      &
            '  PERCENTAGE OF DELETED OBSERVATIONS: NO CHECK'
    ENDIF
  ENDIF
!
! Check number of deleted observations
! ------------------------------------
! start the full protocoll
! ------------------------
  IF (opt%summary == 1)                                                          &
    WRITE(lfnprt,'(/,A,/,A,//,A,F7.2,A,//,A,/,A)')                               &
          'CHECK PERCENTAGE OF DELETED OBSERVATIONS PER SATELLITE:',             &
          '------------------------------------------------------',              &
          '  MAX. PERCENTAGE ALLOWED FOR A "GOOD" SATELLITE:  ',opt%maxDel,' %', &
          '  NUM   SATELLITE       % OBS BEFORE         % OBS. AFTER           STATUS',  &
          '------------------------------------------------------------------------------'
!
! Loop all satellites
! -------------------
  DO iSat=1,nSat
    satTmp = rmsSat(iSat)%satNum
    WRITE(Line,'(I4,5X,I3,2F20.2)')    &
      iSat, satTmp, RMSSat(iSat)%OBS(1), RMSSat(iSat)%OBS(2)
    IF (satTmp < 100) THEN
      maxDel=opt%maxDel
    ELSE
      maxDel=4d0*opt%maxDel
    ENDIF

! No observations for this satellite
    IF (RMSSat(iSat)%OBS(1) == 0.D0 .AND. RMSSat(iSat)%RMS(1) == 0.D0) THEN
      WRITE(Line(68:73),'(A)') 'NO OBS'
      nBad=nBad+1
      BadSat(nBad)%SatNum=satTmp
      BadSat(nBad)%iSat=iSat
      BadSat(nBad)%Flag=1
      BadSat(nBad)%rmsRatio = RMSsat(iSat)%rms(1)/RMSsat(nSat+1)%rms(1)

    ELSEIF ( (rmsSat(iSat)%numObs(1)<opt%minObsG .AND. satTmp<100) .OR.  &
             (rmsSat(iSat)%numObs(1)<opt%minObsR .AND. satTmp>100) .OR.  &
             (rmsSat(iSat)%obs(1) == 0.d0)                             ) THEN
      WRITE(Line(67:73),'(A)') 'FEW OBS'
      nBad=nBad+1
      BadSat(nBad)%SatNum=satTmp
      BadSat(nBad)%iSat=iSat
      BadSat(nBad)%Flag=2
      BadSat(nBad)%rmsRatio = RMSsat(iSat)%rms(1)/RMSsat(nSat+1)%rms(1)

!! "Quasi-no" observations for this satellite
!    ELSE IF (RMSSat(iSat)%OBS(1) < 0.1D0 .AND. RMSSat(iSat)%SatNum < 100) THEN
!      WRITE(Line(67:73),'(A)') 'FEW OBS'
!      nBad=nBad+1
!      BadSat(nBad)%SatNum=RMSsat(iSat)%SatNum
!      BadSat(nBad)%iSat=iSat
!      BadSat(nBad)%Flag=0
!      BadSat(nBad)%rmsRatio = RMSsat(iSat)%rms(1)/RMSsat(nSat+1)%rms(1)

! RMS ratio exceeded
!!    ELSE IF (RMSSat(iSat)%OBS(1) < 0.1 .OR. &
!!        (RMSSat(iSat)%OBS(1)-RMSSat(iSat)%OBS(2))/RMSSat(iSat)%OBS(1)*100 &
!!                                                             > opt%maxDel) THEN
!!    ELSE IF ((RMSSat(iSat)%OBS(1)-RMSSat(iSat)%OBS(2))/RMSSat(iSat)%OBS(1)*100 &
!!                                                             > opt%maxDel) THEN

    ELSEIF (RMSSat(iSat)%OBS(2) /= 0 .AND. &
           (RMSSat(iSat)%OBS(1)-RMSSat(iSat)%OBS(2))/RMSSat(iSat)%OBS(1)*100 &
                                                             > maxDel) THEN
      WRITE(Line(70:72),'(A)') 'BAD'
      nBad=nBad+1
      BadSat(nBad)%SatNum=RMSsat(iSat)%SatNum
      BadSat(nBad)%iSat=iSat
      BadSat(nBad)%Flag=0
      BadSat(nBad)%rmsRatio = RMSsat(iSat)%rms(1)/RMSsat(nSat+1)%rms(1)
    ENDIF

    IF (opt%summary == 1) WRITE(lfnprt,'(1X,A)') TRIM(Line)
  ENDDO
  IF (opt%summary == 1)                                                &
    WRITE(lfnprt,'(A,/)')                                              &
          '------------------------------------------------------------------------------'
!
! Large RMS in RESRMS
! -------------------
!
! start the full protocoll
! ------------------------
  IF (opt%summary == 1)                                                      &
    WRITE(lfnprt,'(/,A,/,A,//,A,F7.2,//,A,/,A)')                             &
          'CHECK RMS RATIO FOR THE FIRST RESRMS PER SATELLITE:',             &
          '---------------------------------------------------',             &
          '  MAX. RMS RATIO ALLOWED FOR A "GOOD" SATELLITE:   ',opt%maxRRMS, &
          '  NUM   SATELLITE       TOTAL RMS (MM)         RMS RATIO            STATUS',  &
          '------------------------------------------------------------------------------'

!
! Loop all satellites
! -------------------
  iSatLoop: DO iSat=1,nSat
    WRITE(Line,'(I4,5X,I3,F20.1,F20.2)')           &
      iSat, RMSsat(iSat)%SatNum, RMSSat(iSat)%RMS(1), &
      RMSSat(iSat)%RMS(1)/RMSSat(nSat+1)%RMS(1)
    IF (RMSsat(iSat)%SatNum < 100) THEN
      maxRRMS=opt%maxRRMS
    ELSE
      maxRRMS=4d0*opt%maxRRMS
    ENDIF
!!    IF (RMSSat(iSat)%RMS(1)/RMSSat(nSat+1)%RMS(1) > opt%maxRRMS) THEN
    IF (RMSSat(iSat)%RMS(1)/RMSSat(nSat+1)%RMS(1) > maxRRMS) THEN
      WRITE(Line(70:72),'(A)') 'BAD'
      DO iBad=1,nBad
        IF (BadSat(iBad)%iSat == iSat) THEN
          IF (opt%summary == 1) WRITE(lfnprt,'(1X,A)') TRIM(Line)
          CYCLE iSatLoop
        ENDIF
      ENDDO
      nBad=nBad+1
      BadSat(nBad)%SatNum=RMSsat(iSat)%SatNum
      BadSat(nBad)%iSat=iSat
      BadSat(nBad)%Flag=0
      BadSat(nBad)%rmsRatio = RMSsat(iSat)%rms(1)/RMSsat(nSat+1)%rms(1)
    ENDIF
    IF (opt%summary == 1) WRITE(lfnprt,'(1X,A)') TRIM(Line)
  ENDDO iSatLoop
  IF (opt%summary == 1)                                                &
    WRITE(lfnprt,'(A,/)')                                              &
          '------------------------------------------------------------------------------'
!
! Huge RMS in RESRMS
! -------------------
  bigRMS=opt%maxARMS
  jSat=-1
  DO iSat=1,nSat
    IF (RMSSat(iSat)%RMS(1) > bigRMS) THEN
      jSat=iSat
      bigRMS=RMSSat(iSat)%RMS(1)
    ENDIF
  ENDDO
!

  IF (jSat /= -1) THEN
    mBad  =   0
    DO iSat=1,nBad
      IF (BadSat(iSat)%Flag /= 0) THEN
        mBad = mBad+1
        IF (mBad /= iSat) BadSat(mBad) = BadSat(iSat)
      ENDIF
    ENDDO

    nBad=mBad+1
    BadSat(nBad)%SatNum = RMSSat(jSat)%SatNum
    BadSat(nBad)%iSat = jSat
    BadSat(nBad)%Flag = 0
    BadSat(nBad)%rmsRatio = RMSsat(jSat)%rms(1)/RMSsat(nSat+1)%rms(1)

!
! Write the full protocoll
! ------------------------
    IF (opt%summary == 1) THEN
      WRITE(lfnprt,'(/,A,/,A,//,A,F7.2,A,//,A,//,A,/,A)')                       &
            'CHECK ABSOLUTE RMS FOR THE FIRST RESRMS PER SATELLITE:',           &
            '-----------------------------------------------------',            &
            '  MAX. ABSOLUTE RMS ALLOWED IN THE SOLUTION:  ',opt%maxARMS, ' MM',&
            'LIST OF SATELLITES WITH BIG ABSOLUTE RMS:',                        &
            '  NUM   SATELLITE       TOTAL RMS (MM)         STATUS',            &
            '------------------------------------------------------------------------------'
      DO iSat=1,nSat
        IF (RMSSat(iSat)%RMS(1) > opt%maxARMS) THEN
          WRITE(Line,'(I4,5X,I3,F20.1)')                &
            iSat, RMSsat(iSat)%SatNum, RMSSat(iSat)%RMS(1)
          IF (iSat == jSat) THEN
            WRITE(Line(47:65),'(A)') 'ASSUMED AS BAD SAT.'
          ENDIF
          WRITE(lfnprt,'(1X,A)') TRIM(Line)
        ENDIF
      ENDDO
      WRITE(lfnprt,'(A,/)')                                              &
            '------------------------------------------------------------------------------'
    ENDIF
  ENDIF
!
! Only one satellite may be deleted
! ---------------------------------
  IF (opt%del1sat .AND. nBad > 1) THEN

! Which bad satellite has the biggest rms
    bigRMS= 0d0
    jSat  =  -1
    mBad  =   0
    DO iSat=1,nBad
      IF (BadSat(iSat)%Flag /= 0) THEN
        mBad = mBAD+1
      ELSE IF (RMSSat(BadSat(iSat)%iSat)%RMS(1) > bigRMS) THEN
        jSat=BadSat(iSat)%iSat
        bigRMS=RMSSat(BadSat(iSat)%iSat)%RMS(1)
      ENDIF
    ENDDO
!
    IF (jSat /= -1) THEN
!
! Write the full protocoll
! ------------------------
      IF (opt%summary == 1) THEN
        WRITE(lfnprt,'(/,A,/,A,//,A,//,A,/,A)')                                   &
              'ONLY ONE SATELLITE CAN BE BAD:',                                   &
              '-----------------------------',                                    &
              'LIST OF BAD SATELLITES:',                                          &
              '  NUM   SATELLITE       TOTAL RMS (MM)         STATUS',            &
              '------------------------------------------------------------------------------'
        DO iSat=1,nBad
          WRITE(Line,'(I4,5X,I3,F20.1)')                         &
            badSat(iSat)%iSat, RMSsat(badSat(iSat)%iSat)%SatNum, &
            RMSSat(badSat(iSat)%iSat)%RMS(1)
          IF (badSat(iSat)%iSat == jSat) THEN
            WRITE(Line(47:65),'(A)') 'ASSUMED AS BAD SAT.'
          ENDIF
          IF (badSat(iSat)%Flag == 1) THEN
            WRITE(Line(47:65),'(A)') 'SAT W/O OBSERVATION'
          ENDIF
          IF (badSat(iSat)%Flag == 2) THEN
            WRITE(Line(47:65),'(A)') 'SAT WITH FEW OBSERV'
          ENDIF
          WRITE(lfnprt,'(1X,A)') TRIM(Line)
        ENDDO
        WRITE(lfnprt,'(A,/)')                                              &
              '------------------------------------------------------------------------------'
      ENDIF

      ! Copy "bad" satellites without observations
      mBad = 0
      DO iSat = 1,nBad
        IF (BadSat(iSat)%Flag /= 0) THEN
          mBad = mBad+1
          IF (mBad /= iSat) BadSat(mBad) = BadSat(iSat)
        ENDIF
      ENDDO

      ! Add the "real" bad satellite
      nBad=mBad+1
      BadSat(nBad)%SatNum = RMSSat(jSat)%SatNum
      BadSat(nBad)%Flag = 0
      BadSat(nBad)%iSat = jSat
      BadSat(nBad)%rmsRatio = RMSsat(jSat)%rms(1)/RMSsat(nSat+1)%rms(1)
    ENDIF
  ENDIF

  RETURN
  END SUBROUTINE rcsatchk


END MODULE
