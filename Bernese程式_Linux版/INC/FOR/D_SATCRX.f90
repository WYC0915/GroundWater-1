! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_satcrx

! -------------------------------------------------------------------------
! Purpose:    This module provides the information from the satellite
!             problem file
!
! Author:     R. Dach
!
! Created:    26-Feb-2008
! Last mod.:  06-Oct-2011
!
! Changes:    18-Nov-2009 RD: Add SR gtsatm2
!             04-Mar-2010 SL: charachter LEN for srName corrected
!             09-Aug-2010 RD: SR gtsatp included for pulses (ORBGEN)
!             21-Sep-2010 RD: ST2TIM can be used as a module now
!             06-Oct-2011 RD: Delete pulses within an interval
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_time,   ONLY : t_timInt

  IMPLICIT NONE

  PRIVATE
  PUBLIC  :: crx_addneq, &  ! Special initialization for ADDNEQ2
             gtsatb,  &     ! Returns the list of bad data intervals
             gtsatm,  &     ! Returns the list of maneuvers
             gtsatm2, &     ! Returns the original list of maneuvers, ADDNEQ2
             gtsats,  &     ! Returns the list of arc split events
             gtsatp,  &     ! Returns the list of pulses events
             gtsatd         ! Returns the list of intervals
                            !         where pulses have to be deleted

! Define the supported problem/actions
! ------------------------------------
  ! bad satellite:
  INTEGER(i4b), DIMENSION(2), PARAMETER :: t_crx_badSat = (/ 3,2 /)

  ! maneuver satellite:
  INTEGER(i4b), DIMENSION(2), PARAMETER :: t_crx_manSat = (/ 0,0 /)

  ! arc splits
  INTEGER(i4b), DIMENSION(2), PARAMETER :: t_crx_splSat = (/ 4,0 /)
  INTEGER(i4b), DIMENSION(2), PARAMETER :: t_crx_splHlp = (/ 4,2 /)

  ! pulses
  INTEGER(i4b), DIMENSION(2), PARAMETER :: t_crx_plsSat = (/ 4,4 /) ! add
  INTEGER(i4b), DIMENSION(2), PARAMETER :: t_crx_plsDel = (/ 4,5 /) ! delete

! Problem and action record
! -------------------------
  TYPE t_crux
    INTEGER(i4b)                             :: prn     ! satellite
    INTEGER(i4b), DIMENSION(2)               :: action  ! problem/action code
    TYPE(t_timint)                           :: epoch   ! epoch/interval
  END TYPE t_crux

! Type for satellites
! -------------------
  TYPE t_satcrux
  CHARACTER(LEN=fileNameLength)              :: crxNam
    INTEGER(i4b)                             :: nCrux   ! number of actions
    TYPE(t_crux),DIMENSION(:), POINTER       :: crxLst  ! list of actions
  END TYPE t_satcrux

  TYPE(t_satcrux), SAVE                      :: satCrux
  TYPE(t_satcrux), SAVE                      :: crxOrig
  LOGICAL, SAVE                              :: first  = .TRUE.


CONTAINS


! Initialize structure
! --------------------
  SUBROUTINE init_satcrux(satcrux)
    TYPE(t_satcrux)  :: satcrux

    satCrux%crxnam = ' '
    satCrux%nCrux = 0
    NULLIFY(satcrux%crxLst)
  END SUBROUTINE init_satcrux


! Read the satcrux file into the buffer
! -------------------------------------
  SUBROUTINE read_crux(satCrux,filnam)

    ! modules
    USE s_gtflna
    USE s_alcerr
    USE s_opnfil
    USE s_opnerr
    USE s_st2tim
    USE f_lincount

    ! input/output
    TYPE(t_satcrux)               :: satCrux
    CHARACTER(LEN=fileNameLength), &
                         OPTIONAL :: filnam

    ! Local parameters
    CHARACTER(LEN=18), PARAMETER  :: srName = 'd_satcrx:read_crux'

    ! Local variables
    CHARACTER(LEN=timStrgLength2) :: epostr
    INTEGER(i4b)                  :: iCrx
    INTEGER(i4b)                  :: irc, iac, ios

    ! Deallocate a possible old record
    IF ( .NOT. first ) THEN
      IF ( satCrux%nCrux > 0 .AND. ASSOCIATED(satcrux%crxLst) ) THEN
        DEALLOCATE(satcrux%crxLst,stat=iac)
      ENDIF
    ENDIF

    ! Init the structure
    CALL init_satcrux(satcrux)
    first = .FALSE.

    ! Start reading the file
    CALL gtflna(0,'SATCRUX',satCrux%crxnam,irc)
    IF ( PRESENT(filnam) ) THEN
      IF ( LEN_TRIM(filnam) > 0 ) THEN
        satCrux%crxnam = filnam
      ENDIF
    ENDIF

    IF (LEN_TRIM(satCrux%crxnam)  > 0 .AND. irc == 0) THEN

      ! Allocate the memory for the buffer
      satCrux%nCrux = linCount(satCrux%crxnam,6)
      ALLOCATE(satCrux%crxLst(satCrux%nCrux+10),stat=iac)
      CALL alcerr(iac,'satCrux%crxLst',(/satCrux%nCrux/),srName)

      ! Open file
      CALL opnfil(lfnloc,satCrux%crxnam,'OLD','FORMATTED','READONLY',' ',ios)
      CALL opnerr(lfnerr,lfnloc,ios,satCrux%crxnam,srName)
      READ(LFNLOC,'(/////)')

      ! Read all problems/actions from the file
      DO iCrx = 1, satCrux%nCrux
        READ(LFNLOC,'(I5,2I10,7X,A)',iostat=ios) &
          satCrux%crxLst(iCrx)%prn,satCrux%crxLst(iCrx)%action(1:2),epostr
        IF (ios /= 0 .OR. satCrux%crxLst(iCrx)%prn == 0) EXIT
        CALL st2tim(1,2,epostr,satCrux%crxLst(iCrx)%epoch%t)
      ENDDO

      ! Close the file
      CLOSE(lfnloc)
    ENDIF
  END SUBROUTINE read_crux


! Returns the list of bad data intervals
! --------------------------------------
  SUBROUTINE crx_addneq(epoOrb)

    ! modules
    USE s_dimtst
    USE s_alcerr

    ! input
    REAL(r8b), DIMENSION(:)    :: epoOrb ! Start of the satellite arcs

    ! Local parameter
    REAL(r8b), PARAMETER       :: dt = 1.1d0/86400d0

    ! Local parameter
    CHARACTER(LEN=19), PARAMETER  :: srName = 'd_satcrx:crx_addneq'

    ! Local variables
    INTEGER(i4b)               :: nCrux
    INTEGER(i4b)               :: iCrx,jCrx
    INTEGER(i4b)               :: iArc
    INTEGER(i4b)               :: iac

    ! Read the original satellite problem file
    CALL read_crux(satcrux)

    ! Make a copy of the real maneuver epochs first
    crxOrig%nCrux = 0
    DO icrx = 1, satCrux%ncrux
      IF ( satCrux%crxlst(iCrx)%action(1) /= t_crx_manSat(1) ) CYCLE
      crxOrig%nCrux = crxOrig%nCrux+1
    ENDDO

    ALLOCATE(crxOrig%crxlst(crxOrig%nCrux),stat=iac)
    CALL alcerr(iac,'crxOrig%crxlst',(/crxOrig%nCrux/),srName)
    crxOrig%nCrux = 0


    ! Check whether there are events to be converted
    nCrux = satCrux%ncrux
    DO icrx = 1, nCrux
      IF ( satCrux%crxlst(iCrx)%action(1) /= t_crx_manSat(1) ) CYCLE
      DO jcrx = 1, nCrux
        IF ( satCrux%crxlst(jCrx)%prn == satCrux%crxlst(iCrx)%prn .AND. &
             satCrux%crxlst(jCrx)%action(1) == t_crx_badSat(1)    .AND. &
             satCrux%crxlst(jCrx)%action(2) == t_crx_badSat(2)    .AND. &
             satCrux%crxlst(jCrx)%epoch%t(1) - dt <= &
                                  satCrux%crxlst(iCrx)%epoch%t(1) .AND. &
             satCrux%crxlst(jCrx)%epoch%t(2) + dt >= &
                                  satCrux%crxlst(iCrx)%epoch%t(1) ) THEN
          DO iArc = 1,SIZE(epoOrb)
            IF ( satCrux%crxlst(jCrx)%epoch%t(1) - dt <= epoOrb(iArc) .AND. &
                 satCrux%crxlst(jCrx)%epoch%t(2) + dt >= epoOrb(iArc) ) THEN

              ! Make a copy in the original list
              IF (epoOrb(iArc) < satCrux%crxlst(iCrx)%epoch%t(1)) THEN
                crxOrig%nCrux = crxOrig%nCrux+1
                crxOrig%crxLst(crxOrig%nCrux) = satCrux%crxlst(iCrx)
                crxOrig%crxLst(crxOrig%nCrux)%epoch%t(1) = epoOrb(iArc)
                satCrux%nCrux = satCrux%nCrux+1
                satCrux%crxLst(satCrux%nCrux) = satCrux%crxlst(iCrx)
                satCrux%crxLst(satCrux%nCrux)%epoch%t(1) = epoOrb(iArc)
              ENDIF
              satCrux%crxlst(iCrx)%action     = t_crx_splHlp
              satCrux%crxlst(iCrx)%epoch%t(1) = epoOrb(iArc)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
    ENDDO
  END SUBROUTINE crx_addneq


! Returns the list of bad data intervals
! --------------------------------------
  SUBROUTINE gtsatb(maxbad,filnam,nbad,satbad,iobbad,iacbad,timbad)

    ! modules
    USE s_dimtst

    ! input
    INTEGER(i4b)               :: maxbad ! Maximum number of bad time intervals
    CHARACTER(LEN=*)           :: filnam ! Name of satcrux file
    ! output
    INTEGER(i4b)               :: nbad   ! Number of bad time intervals
    INTEGER(i4b), DIMENSION(*) :: satbad ! Numbers of bad satellites
    INTEGER(i4b), DIMENSION(*) :: iobbad ! Bad observation type
                                         ! =1: phase observations
                                         ! =2: code observations
                                         ! =3: phase and code observations
    INTEGER(i4b), DIMENSION(*) :: iacbad ! Type of action
                                         ! =1: mark observations
                                         ! =2: remove observations
    REAL(r8b), DIMENSION(2,*)  :: timbad ! Start and end of time interval
                                         ! with bad observations in MJD

    ! Local parameters
    CHARACTER(LEN=15), PARAMETER  :: srName = 'd_satcrx:gtsatb'

    ! Local variables
    INTEGER(i4b)                  :: iCrx
    INTEGER(i4b)                  :: irc

    ! Nothing to do
    nBad = 0
    IF (filnam == ' ') RETURN
    IF ( first .OR. filnam /= satCrux%crxnam) THEN
      CALL read_Crux(satCrux,filnam)
    ENDIF

    ! Loop all events
    DO iCrx = 1,satCrux%nCrux
      IF ( satCrux%crxLst(iCrx)%action(1) == t_crx_manSat(1) .OR.  &
          (satCrux%crxLst(iCrx)%action(1) == t_crx_splHlp(1) .AND. &
           satCrux%crxLst(iCrx)%action(2) /= t_crx_splHlp(2)) ) CYCLE

      nbad=nbad+1
      CALL dimtst(1,2,2,srName,'maxbad','bad observation intervals',             &
           'Increase "maxbad" in module "M_MAXDIM".', nbad,maxbad,irc)
      satbad(nbad)   = satCrux%crxLst(iCrx)%prn
      iobbad(nbad)   = satCrux%crxLst(iCrx)%action(1)
      iacbad(nbad)   = satCrux%crxLst(iCrx)%action(2)
      timbad(:,nbad) = satCrux%crxLst(iCrx)%epoch%t(:)
    ENDDO
  END SUBROUTINE gtsatb



! Returns the list of maneuvers
! -----------------------------
  SUBROUTINE gtsatm(maxman,nman,satman,timman)

    ! modules
    USE s_dimtst
    USE s_timst2
    USE s_exitrc

    ! input
    INTEGER(i4b)               :: maxman ! Maximum number of manoeuvres
    ! output
    INTEGER(i4b)               :: nman   ! Number of manoeuvres
    INTEGER(i4b), DIMENSION(*) :: satman ! Numbers of the shifted satellites
    REAL(r8b),    DIMENSION(*) :: timman ! Time of manoeuvre in MJD

    ! Local parameters
    CHARACTER(LEN=15), PARAMETER  :: srName = 'd_satcrx:gtsatm'

    ! Local variables
    CHARACTER(LEN=timStrgLength)  :: epostr
    INTEGER(i4b)                  :: iCrx, iMan
    INTEGER(i4b)                  :: irc

    ! Nothing to do
    nMan = 0
    IF ( first ) THEN
      CALL read_Crux(satCrux)
    ENDIF

    ! Loop all events
    DO iCrx = 1,satCrux%nCrux
      IF ( satCrux%crxLst(iCrx)%action(1) /= t_crx_manSat(1) ) CYCLE

      ! Check whether the event is already in the list
      DO iMan = 1,nMan
        IF (satman(iman) == satCrux%crxLst(iCrx)%prn .AND. &
          DABS(timman(iman) - satCrux%crxLst(iCrx)%epoch%t(1)) < 0.5d0/86400d0) THEN
          CALL timst2(1,1,timman(iman),epostr)
          WRITE(lfnerr,'(/,A,/,16X,A,I3,/,16X,A,/)')                         &
               ' *** SR ' // srName // ': Duplicated entries for maneuver',  &
                        'Satellite number:  ',satman(iman),                  &
                        'Maneuver epoch:    ' // TRIM(epostr)
          CALL exitrc(2)
        ENDIF
      ENDDO

      nMan=nMan+1
      CALL dimtst(1,2,2,srName,'maxman', 'number of manoeuvres', &
           'Increase "maxman" in module "M_MAXDIM".', nman,maxman,irc)
      satman(nman) = satCrux%crxLst(iCrx)%prn
      timman(nman) = satCrux%crxLst(iCrx)%epoch%t(1)
    ENDDO
  END SUBROUTINE gtsatm


! Returns the list of arc split events
! ------------------------------------
  SUBROUTINE gtsats(maxspl,nspl,satspl,timspl)

    ! modules
    USE s_dimtst

    ! input
    INTEGER(i4b)               :: maxspl ! Maximum number of splits
    ! output
    INTEGER(i4b)               :: nspl   ! Number of split events
    INTEGER(i4b), DIMENSION(*) :: satspl ! Numbers of the splited satellites
    REAL(r8b),    DIMENSION(*) :: timspl ! Time of splits in MJD

    ! Local parameters
    CHARACTER(LEN=15), PARAMETER  :: srName = 'd_satcrx:gtsats'

    ! Local variables
    INTEGER(i4b)                  :: iCrx
    INTEGER(i4b)                  :: irc

    ! Nothing to do
    nspl = 0
    IF ( first ) THEN
      CALL read_Crux(satCrux)
    ENDIF

    ! Loop all events
    DO iCrx = 1,satCrux%nCrux
      IF ( satCrux%crxLst(iCrx)%action(1) == t_crx_splSat(1) .AND. &
          (satCrux%crxLst(iCrx)%action(2) == t_crx_splSat(2).OR. &
           satCrux%crxLst(iCrx)%action(2) == t_crx_splHlp(2))) THEN
        nspl=nspl+1
        CALL dimtst(1,2,2,srName,'maxspl', 'number of arc split events', &
             'Increase "maxman" in module "M_MAXDIM".', nspl,maxspl,irc)
        satspl(nspl) = satCrux%crxLst(iCrx)%prn
        timspl(nspl) = satCrux%crxLst(iCrx)%epoch%t(1)
      ENDIF
    ENDDO
  END SUBROUTINE gtsats

! Returns the list of pulse epochs
! --------------------------------
  SUBROUTINE gtsatp(maxpls,npls,satpls,timpls)

    ! modules
    USE s_dimtst

    ! input
    INTEGER(i4b)               :: maxpls ! Maximum number of pulses
    ! output
    INTEGER(i4b)               :: npls   ! Number of pulses
    INTEGER(i4b), DIMENSION(*) :: satpls ! Numbers of the satellites
    REAL(r8b),    DIMENSION(*) :: timpls ! Time of pulses in MJD

    ! Local parameters
    CHARACTER(LEN=15), PARAMETER  :: srName = 'd_satcrx:gtsatp'

    ! Local variables
    INTEGER(i4b)                  :: iCrx
    INTEGER(i4b)                  :: irc

    ! Nothing to do
    npls = 0
    IF ( first ) THEN
      CALL read_Crux(satCrux)
    ENDIF

    ! Loop all events
    DO iCrx = 1,satCrux%nCrux
      IF ( satCrux%crxLst(iCrx)%action(1) /= t_crx_plsSat(1) .OR. &
           satCrux%crxLst(iCrx)%action(2) /= t_crx_plsSat(2) ) CYCLE

      npls=npls+1
      CALL dimtst(1,2,2,srName,'maxpls', 'number of pulses events', &
           'Increase "maxman" in module "M_MAXDIM".', npls,maxpls,irc)
      satpls(npls) = satCrux%crxLst(iCrx)%prn
      timpls(npls) = satCrux%crxLst(iCrx)%epoch%t(1)
    ENDDO
  END SUBROUTINE gtsatp


! Returns the list of intervals where the pulses are deleted
! ----------------------------------------------------------
  SUBROUTINE gtsatd(maxdel,filnam,ndel,plsdel,timdel)

    ! modules
    USE s_dimtst

    ! input
    INTEGER(i4b)                 :: maxdel ! Maximum number of intervals
    CHARACTER(LEN=*)             :: filnam ! Name of satcrux file
    ! output
    INTEGER(i4b)                 :: nDel   ! Number of intervals
    INTEGER(i4b),   DIMENSION(*) :: plsDel ! Numbers of bad satellites
    TYPE(t_timint), DIMENSION(*) :: timDel ! Start and end of time interval
                                         ! with bad observations in MJD

    ! Local parameters
    CHARACTER(LEN=15), PARAMETER  :: srName = 'd_satcrx:gtsatd'

    ! Local variables
    INTEGER(i4b)                  :: iCrx
    INTEGER(i4b)                  :: irc

    ! Nothing to do
    nDel = 0
    IF (filnam == ' ') RETURN
    IF ( first .OR. filnam /= satCrux%crxnam) THEN
      CALL read_Crux(satCrux,filnam)
    ENDIF

    ! Loop all events
    DO iCrx = 1,satCrux%nCrux
      IF ( satCrux%crxLst(iCrx)%action(1) /= t_crx_plsDel(1) .OR. &
           satCrux%crxLst(iCrx)%action(2) /= t_crx_plsDel(2) ) CYCLE

      nDel=nDel+1
      CALL dimtst(1,2,2,srName,'maxDel','pulse deletion intervals',  &
           'Increase "maxbad" in module "M_MAXDIM".', nDel,maxDel,irc)
      plsDel(ndel) = satCrux%crxLst(iCrx)%prn
      timdel(ndel) = satCrux%crxLst(iCrx)%epoch
    ENDDO
  END SUBROUTINE gtsatd



! Returns the original list of maneuvers in case of ADDNEQ2
! ---------------------------------------------------------
  SUBROUTINE gtsatm2(maxman,nman,satman,timman)

    ! modules
    USE s_dimtst
    USE s_timst2
    USE s_exitrc

    ! input
    INTEGER(i4b)               :: maxman ! Maximum number of manoeuvres
    ! output
    INTEGER(i4b)               :: nman   ! Number of manoeuvres
    INTEGER(i4b), DIMENSION(*) :: satman ! Numbers of the shifted satellites
    REAL(r8b),    DIMENSION(*) :: timman ! Time of manoeuvre in MJD

    ! Local parameters
    CHARACTER(LEN=16), PARAMETER  :: srName = 'd_satcrx:gtsatm2'

    ! Local variables
    CHARACTER(LEN=timStrgLength)  :: epostr
    INTEGER(i4b)                  :: iCrx, iMan
    INTEGER(i4b)                  :: irc

    ! Nothing to do
    nMan = 0
    IF ( first ) THEN
      CALL read_Crux(satCrux)
    ENDIF

    ! Loop all events
    DO iCrx = 1,crxOrig%nCrux
      IF ( crxOrig%crxLst(iCrx)%action(1) /= t_crx_manSat(1) ) CYCLE

      ! Check whether the event is already in the list
      DO iMan = 1,nMan
        IF (satman(iman) == crxOrig%crxLst(iCrx)%prn .AND. &
          DABS(timman(iman) - crxOrig%crxLst(iCrx)%epoch%t(1)) < 0.5d0/86400d0) THEN
          CALL timst2(1,1,timman(iman),epostr)
          WRITE(lfnerr,'(/,A,/,16X,A,I3,/,16X,A,/)')                         &
               ' *** SR ' // srName // ': Duplicated entries for maneuver',  &
                        'Satellite number:  ',satman(iman),                  &
                        'Maneuver epoch:    ' // TRIM(epostr)
          CALL exitrc(2)
        ENDIF
      ENDDO

      nMan=nMan+1
      CALL dimtst(1,2,2,srName,'maxman', 'number of manoeuvres', &
           'Increase "maxman" in module "M_MAXDIM".', nman,maxman,irc)
      satman(nman) = crxOrig%crxLst(iCrx)%prn
      timman(nman) = crxOrig%crxLst(iCrx)%epoch%t(1)
    ENDDO
  END SUBROUTINE gtsatm2


END MODULE d_satcrx


