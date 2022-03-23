MODULE s_OBSINF
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE obsinf(maxfil, nftot , header, nfrfil, filfrq, ambdef, ambsav, &
                  isasys, window, numobs, ifdone, melwub, globalwindow,   &
                  nList, numLst, namLst, isObSt, nSat, satLst, isObSa,    &
                  nmxsta, nmxamb, timfil)

! -------------------------------------------------------------------------
!
! Purpose:    This is a new version of the old subroutine OBSINF.f that
!             reads the names of input observation files of program GPSEST
!             and several file-specific options
!
! Author:     L. Mervart
!
! Created:    04-Jun-2000
!
! Changes:    17-Apr-2001 JD: Use SJ READSESS, correct keyword ECCENT
!             31-May-2001 RD: Use GTFILE for reading the obs files
!                             Better handling of the observation window
!             07-Jun-2001 RD: Switch from obs-win to sess-win
!                             Use only one session table
!             28-Jun-2001 RD: Generate also a list of satellites
!             23-Aug-2001 RD: Write an error if no observation files found
!             05-Jun-2002 RD: SR gestin uses LFNxxx from GPSEST now
!             03-Feb-2003 SC: Either zero or double diff. solution allowed
!             25-Mar-2003 RD: "globalwindow" will not adapted to the session
!             23-Apr-2003 AJ: Nullify local pointers
!             19-May-2003 RD: Use SR gttimwin instead of SR readsess
!             27-Jun-2003 SS: Pass maxsta to SR GESTIN
!             08-Sep-2003 HU: antnam, recnam chr16 -> chr20
!             14-Oct-2003 RD: Do not read keyValue without iostat
!             10-Jan-2004 HU: Error output modified
!             19-Jan-2003 SS/MM: Revision of GPSEST input panels
!             11-Oct-2004 RD: allSta/allSat-lists are POINTERS now
!             28-Jun-2005 MM: Unused SR import removed
!             29-Apr-2008 DT: Range observations added (check frequency
!                             selection)
!             22-Jul-2009 DT: Add timfil to parameter list
!             27-May-2010 RD: Read ISASYS already in OBSINF
!             06-Jun-2012 MM: Adapted to new input option GEOTEC
!             29-Jul-2012 RD: Single-frequency file only supports "L1"
!             29-Jul-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, keyValueLength
  USE m_time,   ONLY: t_timint

  USE s_gttimwin
  USE s_gestin
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptc
  USE s_gtflna
  IMPLICIT NONE

! Variables for COMMON
! -------------------
  CHARACTER(LEN=6) :: mxnfrq
  INTEGER(i4b)     :: mxcfrq

! List of Parameters
! ------------------
! input
  INTEGER(i4b)                          :: maxfil ! max. number of obs. files

! output
  INTEGER(i4b)                          :: nftot  ! total number of files
  CHARACTER(LEN=*), DIMENSION(*)        :: header ! header file names
  INTEGER(i4b)    , DIMENSION(*)        :: nfrfil ! number of frequencies to be
  INTEGER(i4b)    , DIMENSION(mxcfrq,*) :: filfrq ! frequencies to be processed
  INTEGER(i4b)    , DIMENSION(*)        :: ambdef ! a priori amb. definition
                                                  ! 1: new initialization
                                                  ! 2: introduce widelane only
                                                  ! 3: intr. L1/L2 and widelane
                                                  ! 4: intr. L1/L2 only
  INTEGER(i4b)    , DIMENSION(*)        :: ambsav ! save ambiguities (1: YES)
  INTEGER(i4b)                          :: isasys ! satellite system selection
                                                  ! = 0: ALL
                                                  ! = 1: GPS
                                                  ! = 2: GLONASS
                                                  ! = 3: Galileo
                                                  ! = 4: GPS+GLONASS
                                                  ! = 5: GPS+Galileo
                                                  ! = 6: GLONASS+Galileo
  REAL(r8b)       , DIMENSION(2,*)      :: window ! observation window (MJD)
  INTEGER(i4b)    , DIMENSION(mxcfrq,*) :: numobs ! num. of double diff. obs.
  INTEGER(i4b)    , DIMENSION(*)        :: ifdone ! file already processed
                                                  ! 1: already processed
  INTEGER(i4b)                          :: melwub ! Melbourne-Wuebbena LC
                                                  ! 0: NO, 1: YES, 2: DTEC LC
  TYPE(t_timint)                        :: globalWindow
                                                  ! observation window from
                                                  ! files or from input (MJD)
  INTEGER(i4b)                          :: nList! number of all stations
  INTEGER(i4b),     DIMENSION(:),POINTER:: numLst
                                                  ! station numbers
  CHARACTER(LEN=*), DIMENSION(:),POINTER:: namLst
                                                  ! station names
  INTEGER(i4b),     DIMENSION(:),POINTER:: isObSt ! station was observed 0/1
  INTEGER(i4b)                          :: nSat   ! number of all satellites
  INTEGER(i4b),     DIMENSION(:),POINTER:: satLst ! satellite numbers
  INTEGER(i4b),     DIMENSION(:),POINTER:: isObSa ! satellite was observed 0/1
  INTEGER(i4b)                          :: nmxsta ! computed value for MAXSTA
  INTEGER(i4b)                          :: nmxamb ! MAX(numamb(1:nftot))

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=16)            , DIMENSION(2,maxfil)   :: stafil
  CHARACTER(LEN=20)            , DIMENSION(2*maxfil)   :: recnam
  CHARACTER(LEN=20)            , DIMENSION(2*maxfil)   :: antnam
  CHARACTER(LEN=80)                                    :: coofil
  CHARACTER(LEN=80)                                    :: eccfil

  INTEGER(i4b)                 , DIMENSION(2,maxfil)   :: nstfil
  INTEGER(i4b)                 , DIMENSION(maxfil)     :: nfrfilLocal
  INTEGER(i4b)                 , DIMENSION(2*maxfil)   :: antnum
  INTEGER(i4b)                                         :: iFil
  INTEGER(i4b)                                         :: nant
  INTEGER(i4b)                                         :: ircode
  INTEGER(i4b)                                         :: irc
  INTEGER(i4b)                                         :: iwide
  INTEGER(i4b)                                         :: il1l2
  INTEGER(i4b)                                         :: isave
  INTEGER(i4b)                                         :: solTyp
  INTEGER(i4b)                                         :: FreqErr

  REAL(r8b)                    , DIMENSION(2,maxfil)   :: timfil
  REAL(r8b)                    , DIMENSION(2)          :: obsWindow, localWindow

! Commons
! -------
  COMMON/mcmfrq/mxcfrq,mxnfrq

  NULLIFY(keyValue)

! Init return code
! ----------------
  irCode  = 0
  FreqErr = 0

! Get the type of solution
! ------------------------
  CALL readKeys('GEOTEC', keyValue, irc)
  CALL ckoptc(1,'GEOTEC', keyValue, (/'GNSS','SLR '/), 'obsinf', &
              'Space geodetic technique', irc, irCode,           &
              valList=(/1,0/), result1=solTyp)

  IF (irCode /= 0) CALL exitrc(2)

! Processed Frequencies
! ---------------------
  melwub          = 0
  nfrfil(1:nftot) = 1
  CALL readkeys('FREQUENCY' , keyValue, irc)
  IF      (keyValue(1) == 'L1')       THEN
    filfrq(1,1:nftot) = 1

  ELSE IF (keyValue(1) == 'L2')       THEN
    filfrq(1,1:nftot) = 2

  ELSE IF (keyValue(1) == 'L3')       THEN
    filfrq(1,1:nftot) = 3
    IF ( solTyp == 0 )  FreqErr = 1

  ELSE IF (keyValue(1) == 'L4')       THEN
    filfrq(1,1:nftot) = 4
    IF ( solTyp == 0 )  FreqErr = 1

  ELSE IF (keyValue(1) == 'L5')       THEN
    filfrq(1,1:nftot) = 5
    IF ( solTyp == 0 )  FreqErr = 1

  ELSE IF (keyValue(1) == 'L1&L2')    THEN
    nfrfil(1:nftot)   = 2
    filfrq(1,1:nftot) = 1
    filfrq(2,1:nftot) = 2

  ELSE IF (keyValue(1) == 'L3&L4')    THEN
    nfrfil(1:nftot)   = 2
    filfrq(1,1:nftot) = 3
    filfrq(2,1:nftot) = 4
    IF ( solTyp == 0 )  FreqErr = 1

  ELSE IF (keyValue(1) == 'MELWUEBB') THEN
    melwub            = 1
    filfrq(1,1:nftot) = 5
    IF ( solTyp == 0 )  FreqErr = 1

  ELSE IF (keyValue(1) == 'DTEC')     THEN
    melwub            = 2
    filfrq(1,1:nftot) = 4
    IF ( solTyp == 0 )  FreqErr = 1

  END IF

! Check frequency for range observations
! --------------------------------------
  IF ( FreqErr == 1 ) THEN
    WRITE (lfnerr, '(/,A, 2(/,16X,A),A,/)') &
           ' *** SR OBSINF: You are processing range observations!', &
                           'Allowed frequencies: L1, L2, L1&L2',     &
                           'Your selection: ', keyValue(1)
    CALL exitrc(2)

  END IF

! Ambiguities-related Options
! ---------------------------
  iwide = 0
  CALL readkeys('APRWIDE' , keyValue, irc)
  IF (irc == 0) THEN
    IF (keyValue(1) == '1') iwide = 1
  ENDIF

  il1l2 = 0
  CALL readkeys('APRL1L2' , keyValue, irc)
  IF (irc == 0) THEN
    IF (keyValue(1) == '1') il1l2 = 1
  ENDIF

  IF      (iwide == 0 .AND. il1l2 == 0) THEN
    ambdef(1:nftot) = 1
  ELSE IF (iwide == 1 .AND. il1l2 == 0) THEN
    ambdef(1:nftot) = 2
  ELSE IF (iwide == 1 .AND. il1l2 == 1) THEN
    ambdef(1:nftot) = 3
  ELSE IF (iwide == 0 .AND. il1l2 == 1) THEN
    ambdef(1:nftot) = 4
  END IF

  isave = 0
  CALL readkeys('AMBSAVE' , keyValue, irc)
  IF (irc == 0) THEN
    IF (keyValue(1) == '1') isave = 1
  ENDIF

  IF (isave == 1) THEN
    ambsav(1:nftot) = 1
  ELSE
    ambsav(1:nftot) = 0
  END IF

! Satellite System
! ----------------
  CALL readKeys('SATSYS', keyValue, irc)
  CALL ckoptc(1,'SATSYS', keyValue, &
              (/'GPS    ','GLONASS','GALILEO','GPS/GLO', &
                'GPS/GAL','GLO/GAL','ALL    '/), &
              'RDIGEN', 'Satellite System', irc, irCode, &
              valList=(/1,2,3,4,5,6,0/), result1=isasys)

! Initialize numobs and ifdone
! ----------------------------
  numobs(1:mxcfrq,1:nftot) = 0
  ifdone(1:nftot)          = 0

! Read Information about Stations and Time Intervals
! --------------------------------------------------
  CALL gtflna(1,'COORD' , coofil, irc)
  CALL gtflna(0,'ECCENT', eccfil, irc)

  CALL gestin(header, nftot, coofil, eccfil, isasys, nList , numLst, &
              namLst,isObSt,  timfil, stafil, nstfil, nfrfilLocal,   &
              nant, recnam, antnam, antnum, nSat, satLst,isObSa,     &
              nmxsta, nmxamb, ircode)

  IF (irCode /= 0) CALL exitrc(2)

! Read the Time Window
! --------------------
  localWindow(1) = MINVAL(timfil(1,1:nftot))
  localWindow(2) = MAXVAL(timfil(2,1:nftot))

  globalWindow%t = localWindow

! Read observation window from input
! ----------------------------------
  CALL gttimwin('WINDOW',(/'RADIO1_1','RADIO1_2'/),          &
                (/'SESSION_YEAR1','SESSION_STRG1'/),         &
                (/'STADAT1','STATIM1','ENDDAT1','ENDTIM1'/), &
                obsWindow)

  window(1,1:nftot) = obsWindow(1)
  IF (obsWindow(1) /= 0d0)  globalWindow%t(1) = obsWindow(1)

  window(2,1:nftot) = obsWindow(2)
  IF (obsWindow(2) /= 1d20) globalWindow%t(2) = obsWindow(2)

! Single-frequency files can only support frequency L1
! ----------------------------------------------------
  DO iFil = 1,nftot
    IF (nfrfilLocal(iFil) == 1) THEN
      nfrFil(ifil) = 1
      filfrq(1,iFil) = 1
    ENDIF
  ENDDO

  DEALLOCATE(keyValue,stat=irc)
  RETURN
END SUBROUTINE obsinf


END MODULE
