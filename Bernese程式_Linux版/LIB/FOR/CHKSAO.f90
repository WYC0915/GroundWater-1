MODULE s_CHKSAO
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE chksao(nsaoff, satoff,timint, list, irCode)

! -------------------------------------------------------------------------
! Purpose:    Checks that all satellites in satoff have identical entries
!             for satellite antenna offsets in the satellite file within
!             the time interval of timint
!
! Author:     R. Dach
!
! Created:    06-Apr-2005
! Last mod.:  01-Mar-2006
!
! Changes:    27-Jun-2005 CU/RD: Implement "the second chance"
!             08-Aug-2005 HB:    Use new SR TIMST2 (module)
!             09-Nov-2005 AG: SENNUM for GTSATA CALL ADDED
!             01-Mar-2006 HU: SATELLIT file is optional
!
! SR used:    gtflna, gtsata, timst2
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_maxdim, ONLY: maxsaa
  USE s_timst2
  USE s_gtflna
  USE s_gtsata
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                    :: nsaoff ! Number of satellites
  INTEGER(i4b), DIMENSION(nsaoff) :: satoff ! Satellite numbers
  REAL(r8b),    DIMENSION(2)      :: timint ! Time interval
  INTEGER(i4b), DIMENSION(3)      :: list   ! List what has to be checked:
                                            ! 1: sensor offsets
                                            ! 2: sensor names
                                            ! 3: block numbers

! output:
  INTEGER(i4b)                    :: irCode ! return code
                                            ! 0: OK
                                            ! 1: Entry for a sat. is missing
                                            ! 2: Inconsistent sensor corrections
                                            ! 3: Inconsistent sensor names
                                            ! 4: Inconsistent block numbers


! List of Functions
! -----------------


! Local Types
! -----------
  TYPE t_sata
    CHARACTER(LEN=fileNameLength)   :: satFil ! Name of the satellite file
    INTEGER(i4b)                    :: NSAANT ! Number of satellites
    INTEGER(i4b),DIMENSION(MAXSAA)  :: SATANT ! Satellite numbers
    REAL(r8b),   DIMENSION(6,MAXSAA):: ANTOFF ! Sensor offsets(1:3) &
                                              ! only SLR offsets(4:6)
    REAL(r8b),   DIMENSION(2,MAXSAA):: TIMINT ! Time intervals given in MJD
                                              ! begin & end
    CHARACTER(LEN=staNam2Length), DIMENSION(2,MAXSAA) :: SATNAM
                                              ! Sensor names
    INTEGER(i4b),DIMENSION(MAXSAA)  :: SATBLK ! Block numbers
    INTEGER(i4b),DIMENSION(MAXSAA)  :: SENNUM ! Sensor numbers
  END TYPE t_sata

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER                  :: srName = 'chksao'


! Local Variables
! ---------------
  TYPE(t_sata), SAVE              :: sata

  CHARACTER(LEN=timStrgLength2)   :: epostr

  INTEGER(i4b), DIMENSION(nsaoff) :: saoidx
  INTEGER(i4b)                    :: isaoff, jsaoff
  INTEGER(i4b)                    :: isaant, jsaant
  INTEGER(i4b)                    :: iSata, jSata
  INTEGER(i4b), SAVE              :: ircSat= 0

  LOGICAL, SAVE                   :: first = .TRUE.

! Init variables
! --------------
  irCode = 0

! Read the SATELLITE file
! -----------------------
  IF (first) THEN
    first = .FALSE.
    CALL gtflna(0,'SATELL',sata%satFil,ircSat)
    IF (ircSat == 0) THEN
      CALL gtsata(maxsaa,sata%nsaant,sata%satant,sata%antoff, &
                  sata%timint,sata%satnam,sata%satblk,sata%sennum)
    ENDIF
  ENDIF

! No satellite file specified
! ---------------------------
  IF (ircSat == 1) RETURN

  CALL timst2(1,2,timint,epostr)

! Find the satellites in the sensor list
! --------------------------------------
  DO isaoff=1,nsaoff
    saoidx(isaoff) = 0
    DO isaant = 1,sata%nsaant
      IF (satoff(isaoff) == sata%satant(isaant).AND. &
          timint(1) .GE. sata%timint(1,isaant) .AND. &
          timint(2) .LE. sata%timint(2,isaant)) THEN
        saoidx(isaoff) = isaant
        EXIT
      ENDIF
    ENDDO

    ! Second chance
    IF (saoidx(isaoff) == 0) THEN
      DO isaant=1,sata%nsaant

        IF (satoff(isaoff) == sata%satant(isaant)) THEN

          ! Start within an interval in SATINFO
          ! but the parameter ends behind the STAINFO interval
          IF ( timint(1) >= sata%timint(1,isaant) .AND. &
               timint(1) <= sata%timint(2,isaant) .AND. &
               timint(2) >  sata%timint(2,isaant) ) THEN
            saoidx(isaoff) = isaant

            ! Is allowed if the end of the parameter is before
            ! the next interval in the SATINFO for this satellite
            DO jsaant=1,sata%nsaant
              IF (jsaant == isaant) CYCLE
              IF (satoff(isaoff)        == sata%satant(jsaant)    .AND. &
                  timint(2)             >  sata%timint(1,jsaant)  .AND. &
                  sata%timint(2,isaant) <  sata%timint(1,jsaant)) THEN
                saoidx(isaoff) = 0
                EXIT
              ENDIF
            ENDDO
            IF (saoidx(isaoff) == 0) EXIT
          ENDIF

          ! End within an interval in SATINFO
          ! but the parameter starts before the STAINFO interval
          IF ( timint(2) >= sata%timint(1,isaant) .AND. &
               timint(2) <= sata%timint(2,isaant) .AND. &
               timint(1) <  sata%timint(1,isaant) ) THEN
            saoidx(isaoff) = isaant

            ! Is allowed if the begin of the parameter is behind
            ! the prev. interval in the SATINFO for this satellite
            DO jsaant=1,sata%nsaant
              IF (jsaant == isaant) cycle
              IF (satoff(isaoff)        == sata%satant(jsaant)    .AND. &
                  timint(1)             <  sata%timint(2,jsaant)  .AND. &
                  sata%timint(1,isaant) >  sata%timint(2,jsaant)) THEN
                saoidx(isaoff) = 0
                EXIT
              ENDIF
            ENDDO
            IF (saoidx(isaoff) == 0) EXIT
          ENDIF
        ENDIF
      ENDDO
    ENDIF

    ! Check that all satellites in list have their index
    IF (saoidx(isaoff) /= 0) CYCLE

    ! The satellite was not defined in the time interval
    DO isaant = 1,sata%nsaant
      IF (satoff(isaoff) /= sata%satant(isaant)) CYCLE

      IF (timint(1) .LT. sata%timint(1,isaant) .AND. &
          timint(2) .LT. sata%timint(1,isaant))  CYCLE

      IF (timint(1) .GT. sata%timint(2,isaant) .AND. &
          timint(2) .GT. sata%timint(2,isaant))  CYCLE

      ! The satellite file entry changes within the time interval
      irCode = 1

      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,2(/,16X,A,A),/)')                  &
      ' *** SR CHKSAO: There is no entry in the satellite file that covers ',  &
                      'the complete time interval observed for the satellite.',&
                      'Satellite number:     ',satoff(isaoff),                 &
                      'Observation interval: ',epostr,                         &
                      'Satellite file:       ',sata%satFil

      EXIT
    ENDDO
  ENDDO


! Check the sensor offsets
! ------------------------
  IF (list(1) == 1 .AND. irCode == 0) THEN
    DO isaoff = 1,nsaoff-1
      isata = saoidx(isaoff)
      IF (isata == 0) CYCLE

      DO jsaoff = isaoff+1,nsaoff
        jsata = saoidx(jsaoff)
        IF (jsata == 0) CYCLE

        IF ( sata%antoff(1,isata) /= sata%antoff(1,jsata) .OR. &
             sata%antoff(2,isata) /= sata%antoff(2,jsata) .OR. &
             sata%antoff(3,isata) /= sata%antoff(3,jsata) .OR. &
            ( (sata%satnam(2,isata) /= '' .AND. sata%satnam(2,jsata) /= '').AND.&
              (sata%antoff(4,isata) /= sata%antoff(4,jsata) .OR. &
               sata%antoff(5,isata) /= sata%antoff(5,jsata) .OR. &
               sata%antoff(6,isata) /= sata%antoff(6,jsata)) ) ) THEN

          irCode = 2

          WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,100I5)')                        &
          ' *** SR CHKSAO: There are different sensor corrections in the ',  &
                          'satellite file for the satellites of one group.', &
                          'Satellite numbers:    ',satoff(1:nsaoff)
          WRITE(lfnerr,'(2(16X,A,A,/))')                                     &
                          'Observation interval: ',epostr,                   &
                          'Satellite file:       ',sata%satFil
        ENDIF
      ENDDO
      EXIT
    ENDDO
  ENDIF


! Check the sensor names
! ----------------------
  IF (list(2) == 1 .AND. irCode == 0) THEN
    DO isaoff = 1,nsaoff-1
      isata = saoidx(isaoff)
      IF (isata == 0) CYCLE

      DO jsaoff = isaoff+1,nsaoff
        jsata = saoidx(jsaoff)
        IF (jsata == 0) CYCLE

        IF ( sata%satnam(1,isata) /= sata%satnam(1,jsata) .OR. &
            ((sata%satnam(2,isata) /= '' .AND. sata%satnam(2,jsata) /= '').AND.&
              sata%satnam(2,isata) /= sata%satnam(2,jsata) ) ) THEN

          irCode = 3

          WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,100I5)')                        &
          ' *** SR CHKSAO: There are different sensor names in the ',        &
                          'satellite file for the satellites of one group.', &
                          'Satellite numbers:    ',satoff(1:nsaoff)
          WRITE(lfnerr,'(2(16X,A,A,/))')                                     &
                          'Observation interval: ',epostr,                   &
                          'Satellite file:       ',sata%satFil
        ENDIF
      ENDDO
      EXIT
    ENDDO
  ENDIF


! Check the block numbers
! -----------------------
  IF (list(3) == 1 .AND. irCode == 0) THEN
    DO isaoff = 1,nsaoff-1
      isata = saoidx(isaoff)
      IF (isata == 0) CYCLE

      DO jsaoff = isaoff+1,nsaoff
        jsata = saoidx(jsaoff)
        IF (jsata == 0) CYCLE

        IF ( sata%satblk(isata) /= sata%satblk(jsata) ) THEN

          irCode = 4

          WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,100I5)')                        &
          ' *** SR CHKSAO: There are different block numbers in the ',       &
                          'satellite file for the satellites of one group.', &
                          'Satellite numbers:    ',satoff(1:nsaoff)
          WRITE(lfnerr,'(2(16X,A,A,/))')                                     &
                          'Observation interval: ',epostr,                   &
                          'Satellite file:       ',sata%satFil
        ENDIF
      ENDDO
      EXIT
    ENDDO
  ENDIF


  RETURN
END SUBROUTINE chksao

END MODULE
