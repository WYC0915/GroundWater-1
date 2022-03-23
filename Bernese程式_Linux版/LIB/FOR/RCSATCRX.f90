MODULE s_RCSATCRX
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rcsatcrx(filnam, todo, nSat, rmssat, nBad, BadSat, irCode)

! -------------------------------------------------------------------------
! Purpose:    Checks RESRMS summary for bad/maneuver satellites for RESCHK
!
! Author:     R. Dach
!
! Created:    04-Dec-2000
!
! Changes:    28-Mar-2001 RD: Bugfix rewriting sat-crux file
!             24-Apr-2001 RD: Satellites with no observations
!             29-May-2001 RD: Use alcerr, handle end of data area in file
!             21-Dec-2001 HU: Use m_bern, other modules with ONLY
!             25-Sep-2002 HU: Remove i_astlib
!             06-Nov-2002 RD: Sat. w/o obs. is not written into SATCRUX anymore
!             26-Mar-2004 PS: Close satcrux file after reading into buffer
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             30-Aug-2005 MM: Sat with few obs written to SATCRUX file
!             14-Oct-2010 RD: Corrected call of SR timst2
!             20-Sep-2012 RD: Use M_BERN with ONLY
!             05-Mar-2013 RD: Change format for writing satellite numbers
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, lfnerr, lfnloc, &
                      fileNameLength, lineLength
  USE p_reschk, ONLY: t_rmssat,t_badsat,t_satcrx

  USE s_opnfil
  USE s_alcerr
  USE f_djul
  USE s_opnerr
  USE s_timst2
  IMPLICIT NONE
!
! Variables from parameter list
! -----------------------------
  CHARACTER(len=fileNameLength)         :: filnam ! input options
  CHARACTER(len=3)                      :: todo   ! type of action
  INTEGER(i4b)                          :: nSat   ! # of satellites in list
  TYPE(t_rmssat), DIMENSION(:), POINTER :: rmssat ! Sat. information rec.
  INTEGER(i4b)                          :: nBad   ! # of bad satellites
  TYPE(t_badsat), DIMENSION(:), POINTER :: BadSat ! List of bad satellites
  INTEGER(i4b)                          :: irCode ! return code of this SR
!
! Local variables
! ---------------
!
  INTEGER(i4b)                              :: numBad  ! # of entries in SATCRUX
  TYPE(t_satcrx), DIMENSION(:), ALLOCATABLE :: SATCRUX ! some records from the SATCRUX file
!
  CHARACTER(len=LineLength)                 :: Line    ! a single line from file
  CHARACTER(len=LineLength),                 &
                  DIMENSION(:), ALLOCATABLE :: buffer  ! buffer for the SATCRUX file
  INTEGER(i4b)                              :: lLine   ! # of data lines in SATCRUX
  INTEGER(i4b)                              :: nLine   ! # of entries in buffer
  INTEGER(i4b)                              :: iLine   ! line counter
  INTEGER(i4b)                              :: SatNum  ! Satellite number
  INTEGER(i4b)                              :: iBad    ! counter for bad sat.
  INTEGER(i4b)                              :: jBad    ! counter for bad sat.
  INTEGER(i4b)                              :: year1,month1,day1,hh1,mm1,ss1
  INTEGER(i4b)                              :: year2,month2,day2,hh2,mm2,ss2
  INTEGER(i4b)                              :: irc     ! SR return code
  INTEGER(i4b)                              :: ios     ! io status
  INTEGER(i4b)                              :: Dummy   ! a dummy variable
!
  LOGICAL                                   :: rdData  ! Valid data area in file
!
! open the satellite problem file
! -------------------------------
  CALL OPNFIL(lfnloc,filnam,'OLD','FORMATTED', 'READONLY',' ',irc)
  CALL OPNERR(lfnerr,lfnloc,irc,filnam,'RCSATCRX')
!
! Count the number of lines
! -------------------------
  nLine=0
  ios=0
  rdData = .TRUE.
  DO WHILE (ios == 0)
    nLine=nLine+1
    READ (lfnloc, '(A)', iostat=ios) Line
    IF (ios == 0 .AND. nLine > 7) THEN
      READ(Line, *, iostat=irc) SatNum
      IF (irc == 0 .AND. rdData) THEN
        lLine=nLine
      ELSE
        rdData = .FALSE.
      ENDIF
    ENDIF
  ENDDO
  nLine=nLine-1
  lLine=lLine+1
!
  CLOSE(lfnloc)
!
! Allocate the satcrux records
! ----------------------------
  ALLOCATE(BUFFER(nLine+2*nBad), stat=ios)
  CALL alcerr(ios,'BUFFER',(/nLine+2*nBad/),'rcsatcrx')
  ALLOCATE(SATCRUX(nLine+2*nBad), stat=ios)
  CALL alcerr(ios,'SATCRUX',(/nLine+2*nBad/),'rcsatchk')
!
! open the satellite problem file
! -------------------------------
  CALL OPNFIL(lfnloc,filnam,'OLD','FORMATTED', 'READONLY',' ',irc)
  CALL OPNERR(lfnerr,lfnloc,irc,filnam,'RCSATCRX')
!
! Read the original SatCrux file into a buffer
! --------------------------------------------
  numBad=0
  rdData = .TRUE.
  DO iLine=1, nLine
    IF (irCode==0) THEN
      READ (lfnloc,'(A)',iostat=ios) buffer(iLine)
      IF (ios /= 0) THEN
        WRITE(lfnerr,'(/,A,/,18X,A,A,/)')                                &
              ' *** SR RCSATCRX: ERROR READING SATELLITE PROBLEM FILE',  &
                                'FILE NAME : ', TRIM(filnam)
        irCode=2
      ELSE
        IF (iLine > 7) rdData = (rdData .AND. LEN_TRIM(buffer(iLine)) > 1)
        IF (rdData .AND.&
            buffer(iLine)(15:15) == '3' .AND. buffer(iLine)(25:25) == '2') THEN
          numbad=numbad+1
          READ (buffer(iLine),*,iostat=ios) SatCrux(numbad)%satNum,      &
              satCrux(numBad)%iobbad, satCrux(numBad)%iacbad,            &
              year1,month1,day1,hh1,mm1,ss1,                             &
              year2,month2,day2,hh2,mm2,ss2
          satCrux(numBad)%timbad(1)=DJUL(year1,month1,day1+(hh1+(mm1+ss1/60D0)/60D0)/24D0)
          satCrux(numBad)%timbad(2)=DJUL(year2,month2,day2+(hh2+(mm2+ss2/60D0)/60D0)/24D0)
          satCrux(numBad)%iLine=iLine
        ENDIF
        IF (rdData .AND. &
            buffer(iLine)(15:15) == '0' .AND. buffer(iLine)(25:25) == '0') THEN
          numbad=numbad+1
          READ (buffer(iLine),*,iostat=ios) SatCrux(numbad)%satNum,      &
              satCrux(numBad)%iobbad, satCrux(numBad)%iacbad,            &
              year1,month1,day1,hh1,mm1,ss1
          satCrux(numBad)%timbad(1)=DJUL(year1,month1,day1+(hh1+(mm1+ss1/60D0)/60D0)/24D0)
          satCrux(numBad)%iLine=iLine
        ENDIF
      ENDIF
    ENDIF
  ENDDO

  CLOSE(lfnloc)
!
! Set observations as non usable for bad & maneuver satellites
! ------------------------------------------------------------
  IF (irCode == 0 .AND. (todo == 'BAD' .or. todo == 'MAN')) THEN
    iBadLoop: DO iBad=1,nBad
      IF (badSat(iBad)%flag == 1) CYCLE  ! Sat w/o obs.
      DO jBad=1,numBad
!
! Satellite was found in file and marked as bad
! ---------------------------------------------
        IF (satCrux(jbad)%satnum == badsat(iBad)%satnum .AND. &
            satCrux(jbad)%iobbad == 3 .AND. satCrux(jbad)%iacbad == 2) THEN
!
! Satellite is allready excluded
! ------------------------------
          IF (satCrux(jbad)%timbad(2) >= RMSsat(badsat(iBad)%iSat)%satarc(2)  .AND. &
              satCrux(jbad)%timbad(1) <= RMSsat(badsat(iBad)%iSat)%satarc(1)) THEN
            CYCLE iBadLoop
          ENDIF
!
! The excluded arc started before, but the end is in the interval
! ---------------------------------------------------------------
          IF (satCrux(jbad)%timbad(1) <= RMSsat(badsat(iBad)%iSat)%satarc(1)             .AND. &
              satCrux(jbad)%timbad(2)+1.5/86400D0 >= RMSsat(badsat(iBad)%iSat)%satarc(1) .AND. &
              satCrux(jbad)%timbad(2) <  RMSsat(badsat(iBad)%iSat)%satarc(2)) THEN
            satCrux(jbad)%timbad(2)=RMSsat(badsat(iBad)%iSat)%satarc(2)
            CYCLE iBadLoop
          ENDIF
!
! The excluded arc started in the interval, but end is behind
! -----------------------------------------------------------
          IF (satCrux(jbad)%timbad(2) >= RMSsat(badsat(iBad)%iSat)%satarc(2) .AND. &
              satCrux(jbad)%timbad(1) >= RMSsat(badsat(iBad)%iSat)%satarc(1) .AND. &
              satCrux(jbad)%timbad(1) <  RMSsat(badsat(iBad)%iSat)%satarc(2)) THEN
            satCrux(jbad)%timbad(1)=RMSsat(badsat(iBad)%iSat)%satarc(1)
            CYCLE iBadLoop
          ENDIF
!
! The interval is included in the satarc
! --------------------------------------
          IF (satCrux(jbad)%timbad(1) >  RMSsat(badsat(iBad)%iSat)%satarc(1) .AND. &
              satCrux(jbad)%timbad(2) <  RMSsat(badsat(iBad)%iSat)%satarc(2)) THEN
            satCrux(jbad)%timbad(1)=RMSsat(badsat(iBad)%iSat)%satarc(1)
            satCrux(jbad)%timbad(2)=RMSsat(badsat(iBad)%iSat)%satarc(2)
            CYCLE iBadLoop
          ENDIF
        ENDIF
      ENDDO
!
! Add a new excluding interval
! ----------------------------
      numbad=numbad+1
!
      satCrux(numbad)%satnum    = badsat(iBad)%satnum
      satCrux(numbad)%iobbad    = 3
      satCrux(numbad)%iacbad    = 2
      satCrux(numbad)%timbad(1) = RMSsat(badsat(iBad)%iSat)%satarc(1)
      satCrux(numbad)%timbad(2) = RMSsat(badsat(iBad)%iSat)%satarc(2)
      satCrux(numbad)%iLine     = lLine
      DO iLine=nLine,lline,-1
        buffer(iLine+1)=buffer(iline)
      ENDDO
      buffer(lLine)=''
      lLine=lLine+1
      nLine=nLine+1
    ENDDO iBadLoop
  ENDIF
!
!
! New arc setup for maneuver satellites
! -------------------------------------
  IF (irCode == 0 .AND. todo == 'MAN') THEN
    jBadLoop: DO jBad=1,nBad
      IF (badSat(iBad)%flag == 1) CYCLE  ! Sat w/o obs.
      IF (badSat(iBad)%flag == 2) CYCLE  ! Sat few obs.
      DO iBad=1,numBad
!
! Satellite was found in file and marked as bad
! ---------------------------------------------
        IF (satCrux(ibad)%satnum == badsat(jBad)%satnum                    .AND. &
            satCrux(ibad)%iobbad == 0 .AND. satCrux(ibad)%iacbad == 0      .AND. &
            RMSsat(badSat(jBad)%iSat)%satarc(1) <= satCrux(ibad)%timbad(1) .AND. &
            RMSsat(badSat(jBad)%iSat)%satarc(2) >= satCrux(ibad)%timbad(1) ) THEN
          CYCLE jBadLoop
        ENDIF
      ENDDO
!
! Add a arc setup
! ---------------
      numbad=numbad+1
!
      satCrux(numbad)%satnum    = badsat(jBad)%satnum
      satCrux(numbad)%iobbad    = 0
      satCrux(numbad)%iacbad    = 0
      satCrux(numbad)%timbad(1) = (RMSsat(badsat(jBad)%iSat)%satarc(1)+RMSsat(badsat(jBad)%iSat)%satarc(2))/2D0
      satCrux(numbad)%iLine     = lLine
!
      DO iLine=nLine,lline,-1
        buffer(iLine+1)=buffer(iline)
      ENDDO
      buffer(lLine)=''
      lLine=lLine+1
      nLine=nLine+1
    ENDDO jBadLoop
  ENDIF
!
! Write the changes back into the Buffer
! --------------------------------------
  DO iBad=1,numBad
    IF (satCrux(ibad)%iLine /= -1) THEN
      WRITE(buffer(satCrux(ibad)%iLine) (1:5) , '(I5)') satCrux(ibad)%satnum
      WRITE(buffer(satCrux(ibad)%iLine)(15:15), '(I1)') satCrux(ibad)%iobbad
      WRITE(buffer(satCrux(ibad)%iLine)(25:25), '(I1)') satCrux(ibad)%iacbad
      IF (satCrux(ibad)%iacbad == 0) THEN
        CALL timst2(2,1,satCrux(ibad)%timbad(1),buffer(satCrux(ibad)%iLine)(33:51))
      ELSE
        CALL timst2(2,2,satCrux(ibad)%timbad,buffer(satCrux(ibad)%iLine)(33:72))
      ENDIF
    ENDIF
  ENDDO
!
! Write the new SATCRX file
! -------------------------
  CALL OPNFIL(lfnloc,filnam,' ','FORMATTED', ' ',' ',irc)
  CALL OPNERR(lfnerr,lfnloc,irc,filnam,'RCSATCRX')
!
  IF (irc == 0) THEN
    DO iLine=1,nLine
      WRITE(lfnloc,'(A)') TRIM(Buffer(iLine))
    ENDDO
  ELSE
    WRITE(lfnerr,'(/,A,/,18X,A,A,/)')                                        &
          ' *** SR RCSATCRX: ERROR WRITING THE NEW SATELLITE PROBLEM FILE',  &
                            'FILE NAME : ', TRIM(filnam)
    irCode=2
  ENDIF
!
  CLOSE(lfnloc)
!
  DEALLOCATE(SATCRUX, stat=Dummy)
  DEALLOCATE(BUFFER, stat=Dummy)
!
  RETURN
  END SUBROUTINE rcsatcrx


END MODULE
