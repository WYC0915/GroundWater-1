MODULE s_RDIAMB
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdiamb(stramb, sigamb, ar2mod, ar2inf)

! -------------------------------------------------------------------------
! Purpose:    Reads the ambiguity input options for GPSEST
!
! Author:     R. Dach
!
! Created:    22-Jun-2001
!
! Changes:    23-Apr-2003 RD: Nullify local pointers
!             19-Jan-2003 SS/MM: Revision of GPSEST input panels
!             16-Feb-2011 SS: sigamb(5) for GLONASS ambiguity resolution
!             17-Feb-2011 SS: stramb(3) for selection of GNSS
!             17-Feb-2011 SS: LAMBDA ambiguity resolution strategy
!             18-Feb-2011 SS: stramb(4) to ignore quarter-cycle biases
!             15-Feb-2012 SS: Refined quarter-cycle bias handling
!             16-Feb-2012 SS: Negative stramb(4) (PRN number)
!             06-Jun-2012 MM: Adapted to new option GEOTEC
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_readkeys
  USE s_ckoptc
  USE s_ckoptb
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:

! output:
  INTEGER(i4b), DIMENSION(*) :: stramb
                                       ! i=1: ambiguity resolution strategy
                                       !   -1: ambiguity pre-elimination
                                       !    0: no ambiguity resolution
                                       !    1: round-to-nearest-integer
                                       !    2: general search
                                       !    3: sigma-dependent
                                       !    4: quasi-ionosphere-free
                                       !    5: LAMBDA
                                       ! i=2: ambiguity pre-elimination
                                       !   -1: once per session
                                       !    0: every epoch
                                       !    n: every n seconds
                                       ! i=3: selection of GNSS
                                       !    0: all
                                       !    1: GPS
                                       !    2: GLONASS
                                       !    3: GALILEO
                                       ! i=4: considered gps quarter-cycle biases
                                       !    0: never
                                       !    1: if indicated
                                       !    2: always
                                       !   -n: PRN number n
  REAL(r8b), DIMENSION(*)   :: sigamb
                                       ! options for SIGMA-dependent or QIF
                                       ! ambiguity resolution strategy
                                       ! if stramb(1)=3 (SIGMA)
                                       !   sigamb(1): at least 1 integer within
                                       !              sigamb(1)*sigma allowed
                                       !   sigamb(2): maximum  sigma allowed for
                                       !              the ambiguity which should
                                       !              be resolved
                                       !   sigamb(3): minimal sigma used in test
                                       !   sigamb(4): max. number of ambiguities
                                       !      to be solved in one iteration step
                                       !   sigamb(5): GLONASS ambiguity resolution
                                       !              between different frequency
                                       !              channels:
                                       !              = 0: never
                                       !              = 1: same reveiver type
                                       !              = 2: same reveiver model
                                       !              = 3: same reveiver group
                                       !              =-1: always
                                       ! if stramb(1)=4  (QIF)
                                       !   sigamb(1): search width in wide-lane
                                       !              cycles
                                       !   sigamb(2): maximal allowed rms error
                                       !              of narrow-lane ambiguity
                                       !              (bet13*x1+bet23*x2) in
                                       !              narrow-lane cycles
                                       !   sigamb(3): max. allowed distance in
                                       !              l1&l2 space from grid
                                       !              point which is supposed
                                       !              to be the correct solution
                                       !              (in narrow-lane cycles)
                                       !   sigamb(4): max. number of ambiguities
                                       !      to be solved in one iteration step
                                       ! if stramb(1)=5 (LAMBDA)
                                       !   sigamb(1): maximum allowed rms ratio
                                       !              concerning fixed to float
                                       !   sigamb(2): maximum allowed rms of unit
                                       !              weight for fixed solution
                                       !   sigamb(3): resolution mode concerning
                                       !              involved GNSS
                                       !              =1: GNSS by GNSS
                                       !              =2: mixed-GNSS
                                       !              =3: separate-GNSS
  INTEGER(i4b)               :: ar2mod ! mode for amb. res. strategy 2
                                       ! (SEARCH) 0: resolve all present
                                       ! ambiguities, 1: baseline-wise
                                       !                 ambiguity resolution
  REAL(r8b), DIMENSION(*)    :: ar2inf ! options for amb. res. strategy 2
                                       ! (SEARCH)
                                       ! (1) : search width in units of std dev
                                       ! (2) : max allowed rms(fixed)/rms(float)
                                       ! (3) : min allowed
                                       !       rms(2-nd amb)/rms(1-st amb)
                                       ! (4) : search width for geometry-
                                       !       free lc (in l1 cycles)
                                       !       =x : use this value
                                       !       =0 : compute formal width

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
! Ambiguity-keywords
  CHARACTER(LEN=keyNameLength), DIMENSION(0:5), PARAMETER :: ambKeyw = &
    (/'NONE  ', 'ROUND ', 'SEARCH', 'SIGMA ', 'QIF   ','LAMBDA'/)

  CHARACTER(LEN=keyNameLength), DIMENSION(4,2:5), PARAMETER :: addKeyw = &
   reshape( source =                                      &
   (/ 'IFSWID  ', 'IFFXFL  ', 'IF21    ', 'IFSWLC  ',     &
      'LEAST   ', 'MOST    ', 'SIGMAMIN', 'MXAMBIT ',     &
      'SEARWID ', 'RMSNARR ', 'MAXFRAC ', 'MXAQIF  ',     &
      'LAFXFL  ', 'LAMXFX  ', '        ', '        '  /), &
      shape = (/4,4/) )

  CHARACTER(LEN=shortLineLength), DIMENSION(4,2:5), PARAMETER :: errString = &
   reshape( source =                                      &
   (/ '"Search Width in Unit of Std. Dev."               ',     &
      '"Maximum Allowed RMS(Fixed)/RMS(Float)"           ',     &
      '"Minimum Allowed RMS(2-nd Amb)/RMS(1-st Best Amb)"',     &
      '"Search Width for Geometry-Free LC (L1 Cycles)"   ',     &

      '"Ambiguity Resolvable if Exactly 1 Integer within"',     &
      '"Maximal Sigma of a Resolvable Ambiguity"         ',     &
      '"Minimal Sigma of Ambiguity used for Tests"       ',     &
      '"Max. Number of Amb. Solved"                      ',     &

      '"Search Width in Wide-Lane Cycles"                ',     &
      '"Max. RMS of Resolvable Narrow-Lane Ambiguity"    ',     &
      '"Max. Fract. Part of Resolvable NL Ambiguity"     ',     &
      '"Max. Number of Amb. Solved in One Iteration Step"',     &

      '"Maximum allowed rms ratio re fixed to float"     ',     &
      '"Maximum allowed rms of unit weight for fixed"    ',     &
      '"                                                "',     &
      '"                                                "'  /), &
      shape = (/4,4/) )


! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength) , DIMENSION(:)  , POINTER  :: keyValue

  INTEGER(i4b)                                             :: ii
  INTEGER(i4b)                                             :: irCode
  INTEGER(i4b)                                             :: irc, ios

! Init some variables
! -------------------
  irCode = 0
  stramb(1:4) = 0
  sigamb(1:5) = 0d0

  NULLIFY(keyValue)

! Read the ambiguity strategy
! ---------------------------
  CALL readKeys('AMBSTRAT', keyValue, irc)
  IF (irc == 0) THEN
    DO ii = 0, 5
      IF (keyValue(1) == ambKeyw(ii)) stramb(1) = ii
    ENDDO
  ENDIF

  CALL readKeys('GEOTEC', keyValue, irc)
  IF (keyValue(1) == "SLR") stramb(1) = 0

  CALL readKeys('DIFLVL', keyValue, irc)
  IF (keyValue(1) == "ZERO") stramb(1) = 0

  CALL readKeys('PRE04', keyValue, irc)
  IF (keyValue(1) /= 'PRIOR_TO_NEQ_SAVING' .AND. &
      keyValue(1) /= 'NO') THEN
    IF (stramb(1) > 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/)')                   &
      ' ### SR RDIAMB: Ambiguity resolution not possible due to', &
      'pre-eliminated ambiguity parameters'
      stramb(1) = -1
    ENDIF
  ENDIF
  IF      (keyValue(1) == 'EVERY_SESSION') THEN
    stramb(1) = -1
    stramb(2) = -1
  ELSE IF (keyValue(1) == 'AS_SOON_AS_POSSIBLE' ) THEN
    stramb(1) = -1
    stramb(2) =  0
  ELSE IF (keyValue(1) /= 'PRIOR_TO_NEQ_SAVING' .AND. &
           keyValue(1) /= 'NO') THEN
    stramb(1) = -1
    READ(keyValue(1),*,iostat=ios) stramb(2)
    IF (ios /= 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                    &
      ' *** SR RDIAMB: Wrong additional specification for the ' // &
                                           'ambiguity strategy',   &
                      '"Pre-elimination every n seconds"',         &
                      'Value specified:   ',TRIM(keyValue(1))
      CALL exitrc(2)
    ENDIF
  ENDIF

! Selection of GNSS for ambiguity resolution
! ------------------------------------------
  CALL readKeys('SATSYS_AR', keyValue, irc)
  CALL ckoptc(1,'SATSYS_AR', keyValue, (/'ALL    ', &
    'GPS    ','GLONASS','GALILEO'/), &
    'SR RDIAMB', 'Selecton of GNSS for ambiguity resolution', &
    irc, irCode, valList=(/0,1,2,3/), result1=stramb(3))

  CALL readKeys('SATSYS', keyValue, irc)
  IF (keyValue(1) == "GPS") stramb(3) = 1

  IF (stramb(1) <= 0) stramb(3) = 0

! Considered GPS quarter-cycle biases
! -----------------------------------
  CALL readKeys('AMBQCPB', keyValue, irc)
  CALL ckoptc(1,'AMBQCPB', keyValue, (/'NEVER       ', &
    'IF_INDICATED','ALWAYS      '/), &
    'SR RDIAMB', 'Considered GPS quarter-cycle biases', &
    irc, irCode, other=-999, valList=(/0,1,2/), result1=stramb(4))
  IF (stramb(4) == -999) THEN
    READ(keyvalue(1),'(I4)') stramb(4)
    stramb(4) = -stramb(4)
  ENDIF

! Additional Info for SEARCH Strategy
! -----------------------------------
  IF (stramb(1) == 2) THEN
    CALL readKeys('IFBASL', keyValue, irc)
    READ(keyValue(1), *, iostat=ios) ar2mod

    DO ii = 1,4
      CALL readKeys(addKeyw(ii,stramb(1)), keyValue, irc)
      IF (irc == 0) READ(keyValue(1), *, iostat=ios) ar2inf(ii)
      IF (irc /= 0 .OR. ios /= 0) THEN
        WRITE(lfnerr,'(/,A,2(/,16X,A,A),/)')                    &
        ' *** SR RDIAMB: Wrong additional specification for the ' // &
                                             'ambiguity strategy',   &
                        'Field: ',TRIM(errString(ii,stramb(1))),     &
                        'Value: ',TRIM(keyValue(1))
        irCode = irCode + 1
      ENDIF
    ENDDO
  ENDIF

! Additional Info for SIGMA Strategy
! ----------------------------------
  IF (stramb(1) == 3 .OR. &

! Additional Info for QIF Strategy
! --------------------------------
      stramb(1) == 4 .OR. &

! Additional Info for LAMBDA Strategy
! -----------------------------------
      stramb(1) == 5) THEN

    DO ii = 1,4
      IF (addKeyw(ii,stramb(1))(1:1) /= ' ') THEN
        CALL readKeys(addKeyw(ii,stramb(1)), keyValue, irc)
        IF (irc == 0) READ(keyValue(1), *, iostat=ios) sigamb(ii)
        IF (irc /= 0 .OR. ios /= 0) THEN
          WRITE(lfnerr,'(/,A,2(/,16X,A,A),/)')                    &
          ' *** SR RDIAMB: Wrong additional specification for the ' // &
                                               'ambiguity strategy',   &
                          'Field: ',TRIM(errString(ii,stramb(1))),     &
                          'Value: ',keyValue(1)
          irCode = irCode + 1
        ENDIF
      ENDIF
    ENDDO
  ENDIF

! Extra Info for SIGMA Strategy (for GLONASS ambiguity resolution)
! ----------------------------------------------------------------
  IF (stramb(1) == 3) THEN
    CALL readKeys('GLORES', keyValue, irc)
    CALL ckoptc(1,'GLORES', keyValue, (/'NEVER          ', &
      'SAME_RCVR_TYPE ','SAME_RCVR_MODEL','SAME_RCVR_GROUP', &
      'ALWAYS         '/), 'SR RDIAMB', &
      'GLONASS resolution for different freq.', &
      irc, irCode, valList=(/0,1,2,3,-1/), result1=ii)
    sigamb(5) = ii
  ENDIF

! Extra Info for LAMBDA Strategy
! ------------------------------
  IF (stramb(1) == 5) THEN
    CALL readKeys('LAMRES', keyValue, irc)
    CALL ckoptc(1,'LAMRES', keyValue, (/'GNSS_BY_GNSS ', &
      'MIXED_GNSS   ','SEPARATE_GNSS'/), 'SR RDIAMB', &
      'Resolution mode concerning involved GNSS', &
      irc, irCode, valList=(/1,2,3/), result1=ii)
    sigamb(3) = ii

    CALL readKeys('SATSYS', keyValue, irc)
    IF (keyValue(1) == "GPS") sigamb(3) = 0
  ENDIF

! Stop if an error occured
! ------------------------
  IF (irCode /= 0) CALL exitrc(2)

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE rdiamb

END MODULE
