MODULE s_EXTCL2
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.0
! -------------------------------------------------------------------------

SUBROUTINE extCl2(nFil,filnam,filtyp,clkNam)

! -------------------------------------------------------------------------
! Purpose:    Allan deviation for clock estimates from GPSEST
!
! Author:     R. Dach
!
! Created:    27-May-2009
!
! Changes:    10-Jun-2009 RD: Use "undef" to init. clocks
!             28-Mar-2012 RD: Use SVN2CHR as module now
!             27-Apr-2012 RD: Nullify all pointers
!
! Copyright:  Astronomical Institute
!             University of Berne
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfn001, lfnloc, &
                      staNameLength, lineLength, fileNameLength
  USE d_clkrnx, ONLY: undef

  USE f_lincount
  USE s_alcerr
  USE s_cksizer1
  USE s_gtflna
  USE s_opnerr
  USE s_opnfil
  USE s_svn2chr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                     :: nFil   ! Total number of output files
  CHARACTER(LEN=*), DIMENSION(*)   :: filnam ! List of file names
  INTEGER(i4b),     DIMENSION(*)   :: filtyp ! Output file index:
                                             !  0: bad output file
                                             !  1: gpsest
                                             !  2: addneq
                                             !  3: addneq2
                                             ! 11: gpsest  (v5.0)
                                             ! 13: addneq2 (v5.0)
  CHARACTER(LEN=*), DIMENSION(:,:) :: clkNam ! Names of the baselines
                                             ! (sat-clocks like "G28")

! output:

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER      :: srName = 'extcl2'
  INTEGER(i4b),     PARAMETER      :: maxEpo = 300

! Local Variables
! ---------------
  CHARACTER(LEN=lineLength)        :: line
  CHARACTER(LEN=fileNamelength)    :: filSum,filBsl
  CHARACTER(LEN=staNameLength)     :: stanam
  CHARACTER(LEN=1)                 :: flag,sys

  INTEGER(i4b)                     :: iFil
  INTEGER(i4b)                     :: iBsl
  INTEGER(i4b)                     :: nBsl
  INTEGER(i4b)                     :: iClk,jClk
  INTEGER(i4b)                     :: nClk
  INTEGER(i4b)                     :: iVal
  INTEGER(i4b)                     :: nVal
  INTEGER(i4b)                     :: nVar,mVar
  INTEGER(i4b)                     :: iPrt
  INTEGER(i4b)                     :: satNum
  INTEGER(i4b)                     :: ii,mm
  INTEGER(i4b)                     :: ircSum
  INTEGER(i4b)                     :: ios,irc

  REAL(r8b), DIMENSION(:), POINTER :: epoch
  REAL(r8b), DIMENSION(:), POINTER :: clock1
  REAL(r8b), DIMENSION(:), POINTER :: clock2
  REAL(r8b)                        :: tobs
  REAL(r8b)                        :: clock
  REAL(r8b)                        :: dTau,tMax
  REAL(r8b), DIMENSION(:,:),ALLOCATABLE :: value
  REAL(r8b)                        :: aVar

  LOGICAL                          :: sorted


! Nullify pointers
! ----------------
  NULLIFY(epoch)
  NULLIFY(clock1)
  NULLIFY(clock2)

! Get the name of the output file
! -------------------------------
  CALL gtflna(0,'CLKDEV',filSum,ircSum)
  IF (ircSum /= 0) RETURN

  CALL opnfil(lfn001,filSum,'UNKNOWN','FORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfn001,ios,filSum,srName)

! Read the file with the baselines
! --------------------------------
  CALL gtflna(1,'CLKBSL',filBsl,irc)

  nBsl = linCount(filBsl,0)
  IF (nBsl > SIZE(clkNam,2)) nBsl = SIZE(clkNam,2)

  CALL opnfil(lfnloc,filBsl,'OLD','FORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,filBsl,srName)

  iBsl = 0
  DO WHILE (ios == 0 .AND. iBsl < nBsl)
    READ(lfnloc,'(A)',iostat=ios) line
    IF (ios /= 0 .OR. LEN_TRIM(line) == 0) EXIT

    iBsl = iBsl+1
    READ(line,'(A16,1X,A16)',iostat=ios) clkNam(1:2,iBsl)
  ENDDO
  nBsl = iBsl

  CLOSE(lfnloc)

! Loop all baselines
! ------------------
  DO iBsl = 1,nBsl
    IF (LEN_TRIM(clkNam(1,iBsl)) == 0 .OR. LEN_TRIM(clkNam(2,iBsl)) == 0) EXIT

! Init variables
! --------------
    nClk = 0

! Loop all input files
! --------------------
    DO iFil = 1,nFil
      IF (filTyp(iFil) /= 11) CYCLE

! Open gpsest output file (input)
! -------------------------------
      CALL opnfil(lfnloc,filnam(iFil),'OLD','FORMATTED','READONLY',' ',ios)
      CALL opnerr(lfnerr,lfnloc,ios,filnam(iFil),srName)

      ! Read the intput file
      line = ' '
      iPrt = 0
      DO WHILE (ios == 0 .AND. iPrt /= -1)
        READ(lfnloc,'(A)',iostat=ios) line

        ! Find epochwise station clock results
        IF (index(line,' EPOCH WISE STATION CLOCKS:')   /= 0) iPrt = 1
        IF (index(line,' EPOCH WISE SATELLITE CLOCKS:') /= 0) iPrt = 2

        IF (iPrt == 0) CYCLE

! Read the epochwise station/satellite clocks
! -------------------------------------------
        ! Read to the first data record
        DO ii = 1,6
          READ(lfnloc,'(A)') line
        ENDDO

        ! Read more lines to the first empty line
        ios = 0
        DO WHILE (LEN_TRIM(line) > 0 .AND. ios == 0)
          ! Extract one line
          IF (iPrt == 1) THEN
            READ(line,'(13X,F17.6,40X,F17.6,22X,A2,A1, 3X,A16)',iostat=ios) &
                 tObs, clock, flag, sys, staNam
            IF (ios /= 0) EXIT
          ELSE
            READ(line,'(7X,I4,2X,F17.6,40X,F17.6,22X,A2)',iostat=ios) &
                 satNum, tObs, clock, flag
            IF (ios /= 0) EXIT
            staNam=' '
            CALL svn2chr(satNum,satNum,staNam)
            WRITE(staNam(2:3),'(I2.2)') satNum
          ENDIF

          ! Clock must be extracted?
          IF ( flag /= '*' .AND. &
              (staNam == clkNam(1,iBsl) .OR. staNam == clkNam(2,iBsl))) THEN
            jClk = 0
            DO iClk = nClk,1,-1
              IF (DABS(tObs-epoch(iClk)) < 0.5d0/86400d0) THEN
                jClk = iClk
                EXIT
              ENDIF
            ENDDO

            ! First record, allocate arrays
            IF (jClk == 0 .AND. nClk == 0) THEN
              ALLOCATE(epoch(maxEpo),stat=irc)
              CALL alcerr(irc,'epoch',(/maxEpo/),srName)

              ALLOCATE(clock1(maxEpo),stat=irc)
              CALL alcerr(irc,'clock1',(/maxEpo/),srName)
              clock1 = undef

              ALLOCATE(clock2(maxEpo),stat=irc)
              CALL alcerr(irc,'clock2',(/maxEpo/),srName)
              clock2 = undef

              jClk = 1
              nClk = 1

            ! Reallocate arrays (if necessary)
            ELSE IF (jClk == 0 .AND. nClk > 0) THEN

              nClk = nClk + 1
              CALL cksizer1(epoch, nClk,maxEpo,init=undef)
              CALL cksizer1(clock1,nClk,maxEpo,init=undef)
              CALL cksizer1(clock2,nClk,maxEpo,init=undef)

              jClk = nClk
            ENDIF

            epoch(jClk) = tObs

            IF (staNam == clkNam(1,iBsl)) clock1(jClk) = clock*1d-6
            IF (staNam == clkNam(2,iBsl)) clock2(jClk) = clock*1d-6

          ENDIF

          ! Read the next line
          READ(lfnloc,'(A)',iostat=ios) line
        ENDDO

! Reading of station/Satellite clocks has been finished
! -----------------------------------------------------
        IF (iPrt == 1) iPrt =  0
        IF (iPrt == 2) iPrt = -1

      ENDDO

      CLOSE(lfnloc)

      ! Check for invalid epochs
      DO iClk = nClk,1,-1
        IF (epoch(iClk) == 0d0) EXIT  ! previous file
        IF (clock1(iClk) == undef .OR. clock2(iClk) == undef) THEN
          epoch(iClk)  = 0d0
          clock1(iClk) = undef
          clock2(iClk) = undef
        ENDIF
      ENDDO

    ENDDO ! Next input file

! Compute differences
! -------------------
    IF (nClk == 0) CYCLE

    jClk = 0
    DO iClk = 1,nClk
      IF (epoch(iClk) == 0d0) CYCLE
      jClk = jClk + 1
      epoch(jClk)  = epoch(iClk)
      clock1(jClk) = clock2(iClk)-clock1(iClk)
    ENDDO
    nClk = jClk

    IF (nClk == 0) CYCLE

! Compute Allan deviations
! ------------------------
    ! Sort the clocks
    sorted = .FALSE.
    DO WHILE (.NOT. sorted)
      sorted = .TRUE.
      DO iClk = 1,nClk-1
        IF (epoch(iClk) > epoch(iClk+1)) THEN
          sorted = .FALSE.
          clock=epoch(iClk); epoch(iClk) =epoch(iClk+1); epoch(iClk+1) =clock
          clock=clock1(iClk);clock1(iClk)=clock1(iClk+1);clock1(iClk+1)=clock
        ENDIF
      ENDDO
    ENDDO

    ! Get the sampling
    dTau = 0d0
    DO iClk = 1,nClk-1
      IF (DABS(epoch(iClk+1)-epoch(iClk)) < 0.1d0/86400d0) CYCLE
      IF (dTau == 0d0 .OR. dTau > epoch(iClk+1)-epoch(iClk)) &
        dTau = epoch(iClk+1)-epoch(iClk)
    ENDDO

    dTau = DNINT(dTau*86400d0)
    tMax = (epoch(nClk)-epoch(1))*86400d0/8d0

    ! Allocate the array for computing the Allan deviation
    nVal = NINT((epoch(nClk)-epoch(1))*86400d0/dTau)+1

    ALLOCATE(value(3,nVal),stat=irc)
    CALL alcerr(irc,'value',(/3,nVal/),srName)

    value = undef

    ! Put the clocks into the array
    DO iClk = 1,nClk
      IF (MOD(NINT((epoch(iClk)-epoch(1))*86400d1),NINT(dTau*1d1))==0) THEN

        iVal = INT(((epoch(iClk)-epoch(1))*86400d0+0.4d0)/dTau)+1

        value(1,iVal) = clock1(iClk)
        IF (iVal > 1) value(2,iVal-1) = clock1(iClk)
        IF (iVal > 2) value(3,iVal-2) = clock1(iClk)
      ENDIF
    ENDDO

! Init computation of allan deviation
! -----------------------------------
    nVal = SIZE(value,2)-2
    mm = 1
    mVar = 0

! Start summary protocol
! ----------------------
    write(lfn001,'(/,2(/,A))')                                  &
    '   Interval (s)           ADEV (s/s)         Baseline  ',  &
    '-------------------------------------------------------'

! Loop all record lengths
! -----------------------
    DO WHILE (nVal > 2 .AND. mm*dTau <= tMax)

! Compute the allan deviation for the current records length
! ----------------------------------------------------------
      aVar = 0d0
      nVar = 0
      iValLoop: DO iVal = 1,nVal
        DO ii = 1,3
          IF (value(ii,iVal) == undef) CYCLE iValLoop
        ENDDO
        nVar = nVar+1
        aVar = aVar + (value(1,iVal)-2*value(2,iVal)+value(3,iVal))**2
      ENDDO iValLoop

! Store the number of valid entries for the shortest interval
! -----------------------------------------------------------
      IF (mVar == 0) mVar = nVar

! Allan deviation may be computed
! -------------------------------
      IF (mVar-mm > 0 .AND. nVar > 2) THEN
        aVar = DSQRT(aVar/(2*(mVar-mm)*(mm*dTau)**2))
        WRITE(lfn001,'(F15.4,E25.15,4X,A," - ",A)')       &
              mm*dTau,aVar,(clkNam(ii,iBsl)(1:4),ii=2,1,-1)
      ENDIF

! Shift the clock values in the array
! -----------------------------------
      DO iVal = 1,nVal
        value(2,iVal) = value(2,iVal+1)
      ENDDO

      DO iVal = 1,nVal
        value(3,iVal) = value(3,iVal+2)
      ENDDO
      nVal = nVal-2

! Next record length
! ------------------
      mm=mm+1
    ENDDO

! Finish summary protocol
! ----------------------
    write(lfn001,'(A)')                                 &
    '-------------------------------------------------------'

    DEALLOCATE(value,stat=irc)
    DEALLOCATE(epoch,stat=irc)
    DEALLOCATE(clock1,stat=irc)
    DEALLOCATE(clock2,stat=irc)
  ENDDO ! Next station

  CLOSE(lfn001)

END SUBROUTINE extcl2

END MODULE s_EXTCL2
