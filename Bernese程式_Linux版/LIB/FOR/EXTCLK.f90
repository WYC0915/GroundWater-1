MODULE s_EXTCLK
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.0
! -------------------------------------------------------------------------

SUBROUTINE extClk(nFil,filnam,filtyp,clkNam)

! -------------------------------------------------------------------------
! Purpose:    Extracts clock estimates from GPSEST
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
  USE s_major1
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
  CHARACTER(LEN=6), PARAMETER      :: srName = 'extclk'
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
  INTEGER(i4b)                     :: nClk,nClkr
  INTEGER(i4b)                     :: iPrt
  INTEGER(i4b)                     :: satNum
  INTEGER(i4b)                     :: ii
  INTEGER(i4b)                     :: ircSum
  INTEGER(i4b)                     :: ios,irc

  REAL(r8b), DIMENSION(:), POINTER :: epoch
  REAL(r8b), DIMENSION(:), POINTER :: clock1
  REAL(r8b), DIMENSION(:), POINTER :: clock2
  REAL(r8b), DIMENSION(:), POINTER :: epochr
  REAL(r8b), DIMENSION(:), POINTER :: clock1r
  REAL(r8b), DIMENSION(:), POINTER :: clock2r
  REAL(r8b)                        :: tobs
  REAL(r8b)                        :: clock

  LOGICAL                          :: sorted


! Nullify pointers
! ----------------
  NULLIFY(epoch)
  NULLIFY(clock1)
  NULLIFY(clock2)
  NULLIFY(epochr)
  NULLIFY(clock1r)
  NULLIFY(clock2r)


! Get the name of the output file
! -------------------------------
  CALL gtflna(0,'CLKOUT',filSum,ircSum)
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
    nClkr = 0

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
            READ(line,'(13X,F17.6,40X,F17.6,22X,A2,A1,3X,A16)',iostat=ios) &
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
          IF ( flag /= '*' .AND. sys /= 'r' .AND. &
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

            IF (staNam == clkNam(1,iBsl)) clock1(jClk) = clock*1d3
            IF (staNam == clkNam(2,iBsl)) clock2(jClk) = clock*1d3

          ! Clock must be extracted?
          ELSE IF ( flag /= '*' .AND. sys == 'r' .AND. &
              (staNam == clkNam(1,iBsl) .OR. staNam == clkNam(2,iBsl))) THEN
            jClk = 0
            DO iClk = nClkr,1,-1
              IF (DABS(tObs-epochr(iClk)) < 0.5d0/86400d0) THEN
                jClk = iClk
                EXIT
              ENDIF
            ENDDO

            ! First record, allocate arrays
            IF (jClk == 0 .AND. nClkr == 0) THEN
              ALLOCATE(epochr(maxEpo),stat=irc)
              CALL alcerr(irc,'epochr',(/maxEpo/),srName)

              ALLOCATE(clock1r(maxEpo),stat=irc)
              CALL alcerr(irc,'clock1r',(/maxEpo/),srName)
              clock1r = undef

              ALLOCATE(clock2r(maxEpo),stat=irc)
              CALL alcerr(irc,'clock2r',(/maxEpo/),srName)
              clock2r = undef

              jClk = 1
              nClkr = 1

            ! Reallocate arrays (if necessary)
            ELSE IF (jClk == 0 .AND. nClkr > 0) THEN

              nClkr = nClkr + 1
              CALL cksizer1(epochr, nClkr,maxEpo,init=undef)
              CALL cksizer1(clock1r,nClkr,maxEpo,init=undef)
              CALL cksizer1(clock2r,nClkr,maxEpo,init=undef)

              jClk = nClkr
            ENDIF

            epochr(jClk) = tObs

            IF (staNam == clkNam(1,iBsl)) clock1r(jClk) = clock*1d3
            IF (staNam == clkNam(2,iBsl)) clock2r(jClk) = clock*1d3

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

      ! Check for invalid epochs
      DO iClk = nClkr,1,-1
        IF (epochr(iClk) == 0d0) EXIT  ! previous file
        IF (clock1r(iClk) == undef .OR. clock2r(iClk) == undef) THEN
          epochr(iClk)  = 0d0
          clock1r(iClk) = undef
          clock2r(iClk) = undef
        ENDIF
      ENDDO

    ENDDO ! Next input file

! Produce summary of the clocks
! -----------------------------
    IF (nClk > 0) THEN

      ! Check for invalid epochs
      DO iClk = nClk,1,-1
        IF (epoch(iClk) == 0d0) EXIT  ! previous file
        IF (clock1(iClk) == undef .OR. clock2(iClk) == undef) THEN
          epoch(iClk)  = 0d0
          clock1(iClk) = undef
          clock2(iClk) = undef
        ENDIF
      ENDDO

      ! Compute differences
      jClk = 0
      DO iClk = 1,nClk
        IF (epoch(iClk) == 0d0) CYCLE
        jClk = jClk + 1
        epoch(jClk)  = epoch(iClk)
        clock1(jClk) = clock2(iClk)-clock1(iClk)
      ENDDO
      nClk = jClk

      ! Sort the clocks
      sorted = .FALSE.
      DO WHILE (.NOT. sorted)
        sorted = .TRUE.
        DO iClk = 1,nClk-1
          IF (epoch(iClk) > epoch(iClk+1)) THEN
            sorted = .FALSE.
            clock = epoch(iClk); epoch(iClk) =epoch(iClk+1); epoch(iClk+1)=clock
            clock = clock1(iClk);clock1(iClk)=clock1(iClk+1);clock1(iClk+1)=clock
          ENDIF
        ENDDO
      ENDDO

      ! Eliminate a drift
      DO iClk = 1,nClk-10
        IF (DABS(epoch(iClk+10)-epoch(iClk)) > 0.1d0/86400d0) THEN
          clock2(iClk) = (clock1(iClk+10)-clock1(iClk)) / &
                         (epoch(iClk+10) - epoch(iClk))
        ELSE
          clock2(iClk) = 1d20
        ENDIF
      ENDDO

      ! Get the drift using the majority vote
      CALL major1(nClk-10,clock2,jClk,clock)

      ! Correct for the drift
      IF (jClk /= 0) THEN

        clock = clock2(jClk)
        DO iClk = 1,nClk
          clock2(iClk) = clock1(iClk) - clock*(epoch(iClk)-epoch(1))
        ENDDO
      ENDIF

      ! Compute the mean
      clock = 0d0
      DO iClk = 1,nClk
        clock = clock + clock2(iClk)/DBLE(nClk)
      ENDDO

  ! Print the summary
  ! -----------------
      WRITE(lfn001,'(/,3(/,A))')                          &
            '  Num      Epoch          Clock diff   D' // &
            'iff w/o drift       Offset    Baseline  ',   &
            '           (MJD)                (ns)    ' // &
            '       (ns)           (ns)              ',   &
            '----------------------------------------' // &
            '----------------------------------------'
      DO iClk = 1,nClk
        clock2(iClk) = clock2(iClk)-clock
        WRITE(lfn001,'(I5,F16.5,3F15.3,3X,A," - ",A)')    &
              iClk,epoch(iClk),clock1(iClk),clock2(iClk), &
              clock,(clkNam(ii,iBsl)(1:4),ii=2,1,-1)
      ENDDO
      WRITE(lfn001,'(A)')                                 &
            '----------------------------------------' // &
            '----------------------------------------'

      DEALLOCATE(epoch ,stat=irc)
      DEALLOCATE(clock1,stat=irc)
      DEALLOCATE(clock2,stat=irc)
    ENDIF


! Produce summary of the clocks
! -----------------------------
    IF (nClkr > 0) THEN

      ! Check for invalid epochs
      DO iClk = nClkr,1,-1
        IF (epochr(iClk) == 0d0) EXIT  ! previous file
        IF (clock1r(iClk) == undef .OR. clock2r(iClk) == undef) THEN
          epochr(iClk)  = 0d0
          clock1r(iClk) = undef
          clock2r(iClk) = undef
        ENDIF
      ENDDO

      ! Compute differences
      jClk = 0
      DO iClk = 1,nClkr
        IF (epochr(iClk) == 0d0) CYCLE
        jClk = jClk + 1
        epochr(jClk)  = epochr(iClk)
        clock1r(jClk) = clock2r(iClk)-clock1r(iClk)
      ENDDO
      nClkr = jClk

      ! Sort the clocks
      sorted = .FALSE.
      DO WHILE (.NOT. sorted)
        sorted = .TRUE.
        DO iClk = 1,nClkr-1
          IF (epochr(iClk) > epochr(iClk+1)) THEN
            sorted = .FALSE.
            clock = epochr(iClk); epochr(iClk) =epochr(iClk+1); epochr(iClk+1)=clock
            clock = clock1r(iClk);clock1r(iClk)=clock1r(iClk+1);clock1r(iClk+1)=clock
          ENDIF
        ENDDO
      ENDDO

      ! Eliminate a drift
      DO iClk = 1,nClkr-10
        IF (DABS(epochr(iClk+10)-epochr(iClk)) > 0.1d0/86400d0) THEN
          clock2r(iClk) = (clock1r(iClk+10)-clock1r(iClk)) / &
                         (epochr(iClk+10) - epochr(iClk))
        ELSE
          clock2r(iClk) = 1d20
        ENDIF
      ENDDO

      ! Get the drift using the majority vote
      CALL major1(nClkr-10,clock2r,jClk,clock)

      ! Correct for the drift
      IF (jClk /= 0) THEN

        clock = clock2r(jClk)
        DO iClk = 1,nClkr
          clock2r(iClk) = clock1r(iClk) - clock*(epochr(iClk)-epochr(1))
        ENDDO
      ENDIF

      ! Compute the mean
      clock = 0d0
      DO iClk = 1,nClkr
        clock = clock + clock2r(iClk)/DBLE(nClkr)
      ENDDO

  ! Print the summary
  ! -----------------
      WRITE(lfn001,'(/,3(/,A))')                          &
            '  Num      Epoch          Clock diff   D' // &
            'iff w/o drift       Offset    Baseline  ',   &
            '           (MJD)                (ns)    ' // &
            '       (ns)           (ns)              ',   &
            '----------------------------------------' // &
            '----------------------------------------'
      DO iClk = 1,nClkr
        clock2r(iClk) = clock2r(iClk)-clock
        WRITE(lfn001,'(I5,F16.5,3F15.3,1X,"r",1X,A," - ",A)')    &
              iClk,epochr(iClk),clock1r(iClk),clock2r(iClk), &
              clock,(clkNam(ii,iBsl)(1:4),ii=2,1,-1)
      ENDDO
      WRITE(lfn001,'(A)')                                 &
            '----------------------------------------' // &
            '----------------------------------------'

      DEALLOCATE(epochr ,stat=irc)
      DEALLOCATE(clock1r,stat=irc)
      DEALLOCATE(clock2r,stat=irc)
    ENDIF


  ENDDO ! Next station

  CLOSE(lfn001)

END SUBROUTINE extclk

END MODULE s_EXTCLK
