MODULE s_RDIGEN
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdigen(nInpFiles, globalWindow,isLeo,stitle, priopt, &
                  corstr, dtsim , isyncr, sigapr, icoelv, &
                  norres, nsampl, noInClk,secipl, zenmax, &
                  zmxleo, itropo, iextra, itrmap, itrgrd, &
                  edtlvl, isosel, ipolar )

! -------------------------------------------------------------------------
! Purpose:    Reads general input options for GPSEST
!
! Author:     R. Dach
!
! Created:    29-Jun-2001
! Last mod.:  06-Jun-2012
!
! Changes:    30-Jul-2001 RD: Troposphere model "NONE"
!             21-Dec-2001 HU: Use d_const
!             29-Jan-2002 RD: Optional printing of epoch parameters
!             27-Jun-2002 DS: Add LEO options
!             15-Nov-2002 RS: Add observation statistics (nadir angle)
!             15-Apr-2003 RD: Move reading of EDTLVL from RDICLK to RDIGEN
!             30-Apr-2003 SS: Satellite system selection
!             22-Jul-2003 RD: Normalized on apriori weights only
!             19-Jan-2003 SS/MM: Revision of GPSEST input panels
!             28-Oct-2004 RD  Statistics on phase-connected epochs
!             09-Nov-2004 RD: Print statistic on parameter dimensions
!             28-Jul-2005 HU: priopt=0 changed to priopt(1:18)=0
!             24-Aug-2006 AG: GMF implemented
!             18-Sep-2006 HU: Polarization effect option
!             18-Jun-2007 RD: New model icoelv=2: 1/sin(eleva)**4
!             13-Sep-2007 RD: Ignore polari. opt. in DD-mode (apply geom. part)
!             01-Nov-2007 HB: Add parameter secIpl
!             29-Jan-2008 SL: More printing options
!             29-Apr-2008 DT: Add range observations; check trop model
!                             ipolar=0 for SLR ????
!             07-May-2008 DT: Sampling interval should be 0 for SLR
!             30-Jun-2008 RD: VMF added
!             11-Nov-2008 DT: Set correlation strategy = BASELINE for Ranges
!             11-Nov-2008 DT: Add trop.model Mendes-Pavlis (=8)
!             04-Mar-2010 SL: error message typo corrected
!             27-May-2010 RD: Read ISASYS already in OBSINF
!             19-Jul-2010 SL: tab characters removed
!             20-Sep-2010 RD: Ambiguity interval only for phase files
!             14-Oct-2010 RD: Read tropo-model from SR trpopt
!             26-Nov-2010 DT: obstyp added to call of trpopt
!             11-Jul-2011 HB: Two obsolete settings of secipl removed
!             17-Nov-2011 HB: Add isLeo as parameter and set correlation
!                             strategy for LEO+SLR processing
!             06-Jun-2012 MM: Adapted to new options GEOTEC, PRINT, USERXCLK
!
! SR used:    exitrc, readkeys,ckoptr
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_const,  ONLY: pi
  USE p_gpsest, ONLY: maxmea
  USE m_time,   ONLY: t_timint
  USE s_ckoptr
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckoptd
  USE s_gtflna
  USE s_trpopt
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b),     &
      DIMENSION(:) :: nInpFiles    ! number of input files
                                   ! i=1: phase zero differences
                                   ! i=2: code zero differences
                                   ! i=3: phase single differences
                                   ! i=4: code single differences
                                   ! i=5: slr range observations
  TYPE(t_timint)   :: globalWindow ! window to be processed
  LOGICAL          :: isLeo        ! LEO processing or not

! output:
  CHARACTER(LEN=*) :: stitle ! short title
  INTEGER(i4b),     &
      DIMENSION(*) :: priopt ! print options (1: YES)
                             ! i= 1: number of observations
                             ! i= 2: pos. eccentr. and receiver info
                             ! i= 3: clock coefficients (code)
                             ! i= 4: ambiguities
                             ! i= 5: parameter charac. list
                             ! i= 6: constants, antenna offsets,
                             !       ionospheric coefficients
                             ! i= 7: satellite elevations
                             ! i= 8: synchronization errors
                             ! i= 9: number of observ. per file
                             ! i=10: ambiguities every iteration
                             ! i=11: observation statistics (elevation)
                             ! i=12: suppress print of epoch param. solutions
                             ! i=13: observation statistics (nadir angle)
                             ! i=14: print no troposphere parameters
                             ! i=15: print no coordinate parameters
                             ! i=16: print no ambiguity parameters
                             ! i=17: rms of coordinates+coordinate differences
                             ! i=18: slope distances
                             ! i=19: Print array dimensions
                             ! i=20: statistics on phase-connected epochs
  INTEGER(i4b)     :: corstr ! correlation strategy
  REAL(r8b)        :: dtsim  ! max. interval to identify epoch (in days)
  REAL(r8b)        :: sigapr ! a priori sigma
  INTEGER(i4b),     &
    DIMENSION(:,:) :: icoelv ! model for elev.-dep. obs. weighting
                             !  0: equal weighting for all obs.
                             ! >0: model number (see sr WGTELV)
                             ! i=1: station, i=2: LEOs
                             ! j=meatyp
  INTEGER(i4b)     :: norres ! residual computation
                             ! =1: real residuals saved
                             ! =2: l1-normalized residuals saved
                             ! =3: normalized with apriori weights only
  INTEGER(i4b),     &
      DIMENSION(3) :: nsampl ! sampling rate (sec)
                             ! 1: observations
                             ! 2: resubstitution of epoch parameters
                             ! 3: preeliminate of epoch parameters
  INTEGER(i4b),     &
      DIMENSION(3) :: noInClk! What to do if no input clock:
                             ! noInClk(1): rec from clk rnx
                             !  -1: ignore clock rinex file
                             !   0: Use obs. (rec from obs-file)
                             !   1: Use obs. (interpol. clk rnx)
                             !   2: Skip obs.
                             ! noInClk(2): sat from clk rnx
                             !  -1: ignore clock rinex file
                             !   0: Try also sat clk file
                             !   1: Use obs. (interpol. clk rnx)
                             !   2: Skip obs.
                             !   3: Use obs. (sat clk = zero)
                             ! noInClk(3): sat from sat clk file
                             !   2: Skip obs.
                             !   3: Use obs. (sat clk = zero)
  REAL(r8b)        :: secipl ! maximum interval allowed for clock interpolation (sec)
  REAL(r8b)        :: zenmax ! maximum zenith distance (in rad)
  REAL(r8b)        :: zmxLeo ! maximum zenith distance (in rad) LEOs
  INTEGER(i4b)     :: itropo ! tropospheric model
  INTEGER(i4b)     :: iextra ! =0 : use measured values
                             ! =1 : use atm. model values
                             ! =2 : use est. bernese values
  INTEGER(i4b)     :: itrmap ! mapping function for troposp.est.
                             ! 1:   1/cos(z)
                             ! 2:   hopfield
                             ! 3,4: dry/wet niell
                             ! 5,6: dry/wet gmf
                             ! 7,8: dry/wet vmf
                             ! undef_Trp if no troposphere input file
  INTEGER(i4b),     &
    DIMENSION(*)   :: itrgrd ! i=1: est. of tropospheric gradients
                             !      0: no estimation
                             !      1: tilting
                             !      2: linear
                             ! i=2: ratio of number of zenith
                             !      to gradient parameters
  INTEGER(i4b)     :: isyncr ! apply synchronization errors (1: YES)
                             ! do not apply synchr. errors   = 0
  REAL(r8b)        :: edtlvl ! o-c edit level for apriori editting (meter)
  INTEGER(i4b)     :: isasys ! satellite system to be considered
                             ! = 0: ALL
                             ! = 1: GPS
                             ! = 2: GLONASS
  INTEGER(i4b)     :: isosel ! special observation data selection
                             ! = 0: NO
                             ! = 1: NIGHT-TIME
                             ! = 2: NON-ECLIPSING
                             ! = 3: NON-ECLIPSING only for GPS block I, II, IIA
  INTEGER(i4b)     :: ipolar ! Polarization effect
                             ! = 0: none
                             ! = 1: only geometrical effect
                             ! = 2: full effect, incl. satellite attitude

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6),PARAMETER :: srName = 'rdigen'

! Print-keywords
  CHARACTER(LEN=keyNameLength), DIMENSION(20), PARAMETER :: prtKeyw =     &
           (/ 'PRTNUMOB', 'PRTPOSEC', '        ', 'PRTAMBFL', 'PRTPARAM', &
              'PRTPOLE ', 'PRTELEV ', 'PRTSYNCH', 'PRTNUMDD', 'PRTAMBIT', &
              'PRTNOBEL', 'PRTEPPAR', 'PRTNOBNA', 'PRTTRP  ', 'PRTCRD  ', &
              'PRTAMB  ', 'PRTBLH  ', 'PRTDIS  ', 'PRTMAX  ', 'PRTTIMFL' /)

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength) :: rxcFil,satFil
  CHARACTER(LEN=keyValueLength), &
         DIMENSION(:), POINTER  :: keyValue

  INTEGER(i4b)                  :: irCode
  INTEGER(i4b)                  :: irc, ios
  INTEGER(i4b)                  :: ircRxc, ircSat
  INTEGER(i4b)                  :: ii, iPrint
  INTEGER(i4b)                  :: difLvl
  INTEGER(i4b)                  :: geoTec
  INTEGER(i4b)                  :: hlpRxclk

  REAL(r8b)                     :: epoch

  LOGICAL                       :: useRxClk

! Init variables
! --------------
  irCode = 0

  NULLIFY(keyValue)

! General Title
! -------------
  CALL readkeys('TITLE' , keyValue, irc)
  stitle = keyValue(1)(1:LEN_TRIM(keyValue(1)))

! Observation type
! ----------------
  CALL readKeys('GEOTEC', keyValue, irc)
  CALL ckoptc(1,'GEOTEC', keyValue, (/'GNSS','SLR '/), 'RDIGEN', &
              'Space geodetic technique', irc, irCode,                              &
              valList=(/1,0/), result1=geoTec)

! Correlation Strategy
! --------------------
  CALL readKeys('CORREL', keyValue, irc)
  IF      (irc == 0 .AND. keyValue(1) == 'CORRECT'  ) THEN
    corstr = 3
  ELSE IF (irc == 0 .AND. keyValue(1) == 'FREQUENCY') THEN
    corstr = 2
  ELSE IF (irc == 0 .AND. keyValue(1) == 'BASELINE' ) THEN
    corstr = 1
  ELSE
    WRITE(lfnerr,'(/,A,/,16X,A,/)')                            &
    ' *** SR RDIGEN: Wrong entry for correlation strategy.',   &
                    'Specified Value: ',TRIM(keyValue(1))
    CALL exitrc(2)
  ENDIF

  ! Range: only with BASELINE strategy
  ! Range+LEO: only with CORRECT strategy
  ! -------------------------------------
  IF ( geoTec == 0 ) THEN
    IF (isLeo) THEN
      corstr = 3
    ELSE
      corstr = 1
    ENDIF
  ENDIF

! Correlation Interval
! --------------------
  CALL readKeys('CORRINTV', keyValue, irc)
  IF (irc == 0) READ(keyValue(1), *, iostat=ios) dtsim
  IF (ios /= 0 .OR. irc /= 0) THEN
    WRITE(lfnerr,'(/,A,/,16X,A,A,/)')                                   &
    ' *** SR RDIGEN: Wrong entry for the correlation interval detected',&
                    'Specified value:  ',TRIM(keyValue(1))
    CALL exitrc(2)
  ENDIF
  dtsim = dtsim / 86400.0 / 1000.0

! Correlation Interval - Clock Synchronization
! --------------------------------------------
  isyncr = 0
  CALL readKeys('USESYNCR', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') isyncr = 1

! A Priori Sigma
! --------------
  CALL readKeys('SIGAPR1', keyValue, irc)
  IF (irc == 0) READ(keyValue(1), *, iostat=ios) sigapr
  IF (ios /= 0 .OR. irc /= 0 .OR. sigapr < 0d0) THEN
    WRITE(lfnerr,'(/,A,/,16X,A,A,/)')                                   &
    ' *** SR RDIGEN: Wrong entry for the apriori sigma detected',       &
                    'Specified value:  ',TRIM(keyValue(1))
    CALL exitrc(2)
  ENDIF

! Satellite Weighting
! -------------------
  icoelv(1:2,1:maxmea) = 0

  ! for stations
  CALL readKeys('ELVWGT', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == 'COSZ')  icoelv(1,1:2) = 1
  IF (irc == 0 .AND. keyValue(1) == 'COS2Z') icoelv(1,1:2) = 2
  IF (irc == 0 .AND. keyValue(1) == 'COS2C') THEN
    icoelv(1,1) = 1
    icoelv(1,2) = 2
  ENDIF

  ! for LEOs
  CALL readKeys('LEOEDW', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == 'COSZ')  icoelv(2,1:2) = 1
  IF (irc == 0 .AND. keyValue(1) == 'COS2Z') icoelv(2,1:2) = 2
  IF (irc == 0 .AND. keyValue(1) == 'COS2C') THEN
    icoelv(2,1) = 1
    icoelv(2,2) = 2
  ENDIF

! Type of Residuals
! -----------------
  CALL readKeys('NORRES', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == 'REAL') THEN
    norres = 1
  ELSE IF (irc == 0 .AND. keyValue(1) == 'NORMALIZED') THEN
    norres = 2
  ELSE IF (irc == 0 .AND. keyValue(1) == 'NORM_APRIORI') THEN
    norres = 3
  ELSE
    WRITE(lfnerr,'(/,A,/,16X,A,A,/)')                            &
    ' *** SR RDIGEN: Wrong type of residuals detected',           &
                    'Specified value:  ',TRIM(keyValue(1))
    CALL exitrc(2)
  ENDIF

! Sampling Rate (only if not range observations)
! -------------
  nSampl = 0
  IF ( geoTEc /= 0 ) THEN
    CALL readKeys('SAMPLE', keyValue, irc)
    IF (irc == 0) READ(keyValue(1), *, iostat=ios) nsampl(1)
    IF ((ios /= 0 .AND. LEN_TRIM(keyValue(1)) > 0) .OR. &
        irc /= 0 .OR. nSampl(1) < 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,A,/)')                                   &
      ' *** SR RDIGEN: Wrong entry for the sampling rate detected',       &
                      'Specified value:  ',TRIM(keyValue(1))
      CALL exitrc(2)
    ENDIF
  ENDIF

! Sampling Rate: resubst. of epoch parameters
! -------------
  IF ( geoTec /= 0 ) THEN
    CALL readKeys('SAMPLEPO', keyValue, irc)
    IF (irc == 0) READ(keyValue(1), *, iostat=ios) nsampl(2)
    IF ((ios /= 0 .AND. LEN_TRIM(keyValue(1)) > 0) .OR. &
        irc /= 0 .OR. nSampl(2) < 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                                &
      ' *** SR RDIGEN: Wrong entry for the sampling rate for resubstitution ', &
                      'of epoch parameters detected',                          &
                      'Specified value:  ',TRIM(keyValue(1))
      CALL exitrc(2)
    ENDIF
  ENDIF

! Sampling Rate: preeliminate of epoch parameters
! -------------
  IF ( geoTec /= 0 ) THEN
    CALL readKeys('SAMPLEP2', keyValue, irc)
    IF (irc == 0) READ(keyValue(1), *, iostat=ios) nsampl(3)

    IF ((ios /= 0 .AND. LEN_TRIM(keyValue(1)) > 0) .OR. &
        irc /= 0 .OR. nSampl(3) < 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                                &
      ' *** SR RDIGEN: Wrong entry for the sampling rate for preelimination ', &
                      'of epoch parameters detected',                          &
                      'Specified value:  ',TRIM(keyValue(1))
      CALL exitrc(2)
    ENDIF
  ENDIF

! Check which input clock files are available
! -------------------------------------------
  CALL gtflna(0,'RXCLKIN',rxcFil,ircRxc)
  CALL gtflna(0,'SATCLK', satFil,ircSat)

! Skip observation if no receiver clock is available in a clock RINEX file
! ------------------------------------------------------------------------
  noInClk(1) = -1
  IF (ircRxc == 0) THEN

    noInClk(1) = 2

    CALL readKeys('NORXCLKR',keyValue,irc)

    CALL ckoptc(1,'NORXCLKR',keyValue,(/'DO_NOT_USE_OBSERV',                &
                'CLK_FROM_OBS_FILE','INTERPOLATE      '/), srName,          &
                'Receiver clock not in input clock rinex file',irc,irCode,  &
                maxVal = 1, valList = (/2,0,1/), result1=noInClk(1))
  ENDIF

! Take satellite clocks from clock RINEX or Bernese clock file?
! -------------------------------------------------------------
  useRxclk = (ircRxc == 0)
  IF (ircRxc == 0 .AND. ircSat == 0) THEN
    CALL readKeys('USERXCLK',keyValue,irc)
    CALL ckoptc(1,'USERXCLK',keyValue,(/'BERNESE_FILE','CLOCK_RINEX '/),   &
                srName,'Primary source for satellite clocks',irc,irCode,   &
                valList = (/0,1/), result1=hlpRxclk)
    useRxclk = (hlpRxclk == 1)
  ENDIF

! Skip observation if no satellite clock is available in a clock RINEX file
! -------------------------------------------------------------------------
  noInClk(2) = -1
  IF (useRxclk .AND. ircRxc == 0) THEN

    noInClk(2) = 3

    IF (ircSat == 0) noInClk(2) = 0

    CALL readKeys('NORXCLKS',keyValue,irc)

    CALL ckoptc(1,'NORXCLKS',keyValue,                                      &
                (/'DO_NOT_USE_OBSERV','INTERPOLATE      ',                  &
                'TRY_SAT_CLK_FILE ','SAT_CLK_IS_ZERO  '/), srName,          &
                'Satellite clock not in input clock rinex file',irc,irCode, &
                maxVal = 1, valList = (/2,1,0,3/), result1=noInClk(2))

    IF (noInClk(2) == 0 .AND. ircSat /= 0) THEN
      irCode = irCode+1

      WRITE(lfnerr,'(/,A,2(/,16X,A),/)')  ' *** SR RDIGEN: ' //             &
           'No Bernese satellite clock file has been specified.',           &
           'So, "TRY_SAT_CLK_FILE" is no choice if a satellite',            &
           'clock value is missing in the input clock RINEX file.'
    ENDIF
  ENDIF

! Skip observation if no satellite clock is available
! ---------------------------------------------------
  noInClk(3) = 2
  IF (ircSat == 0 .AND. &
      (noInClk(2) == -1 .OR. noInClk(2) == 0)) THEN

    CALL readKeys('NOSATCLK',keyValue,irc)

    CALL ckoptc(1,'NOSATCLK',keyValue,(/'DO_NOT_USE_OBSERV',                &
                'INTERPOLATE      ','SAT_CLK_IS_ZERO  '/), srName,          &
                'Satellite clock not in satellite clock file',irc,irCode,   &
                maxVal = 1, valList = (/2,1,3/), result1=noInClk(3))

  ENDIF

! Clock interpolation interval
! ----------------------------
  secIpl = 0.D0
  IF ( (ircRxc == 0 .AND. noInClk(1) == 1) .OR. &
       (ircRxc == 0 .AND. noInClk(2) == 1) .OR. &
       (ircSat == 0 .AND. noInClk(3) == 1)) THEN
    CALL readKeys('SECIPL', keyValue, irc)
    IF (irc == 0) READ(keyValue(1), *, iostat=ios) secIpl
    IF ((ios /= 0 .AND. LEN_TRIM(keyValue(1)) > 0) .OR. &
        irc /= 0 .OR. secIpl < 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                                   &
      ' *** SR RDIGEN: Wrong entry for the clock ',            &
                      'interpolation interval detected',       &
                      'Specified value:  ',TRIM(keyValue(1))
      CALL exitrc(2)
    ENDIF
  ENDIF

! Elevation Mask (Stations)
! -------------------------
  CALL readKeys('MINEL', keyValue, irc)
  IF (irc == 0) READ(keyValue(1), *, iostat=ios) zenmax
  IF (ios /= 0 .OR. irc /= 0) THEN
    WRITE(lfnerr,'(/,A,/,16X,A,A,/)')                                   &
    ' *** SR RDIGEN: Wrong entry for the minium elevation detected',    &
                    'Specified value:  ',TRIM(keyValue(1))
    CALL exitrc(2)
  ENDIF
  zenmax = (90.0-zenmax)/180.0*pi


! Elevation Mask (LEOs)
! ---------------------
  CALL readKeys('MINELEO', keyValue, irc)

  CALL ckoptr(1,'MINELEO',keyValue,'RDIGEN',        &
              'Elevation mask for LEOs',irc,irCode, &
              maxVal=1,ge=-45d0,le=90d0,result1=zmxleo)

  zmxleo = (90.0-zmxleo)/180.0*pi

! Satellite System
! ----------------
  CALL readKeys('SATSYS', keyValue, irc)
  CALL ckoptc(1,'SATSYS', keyValue, &
              (/'GPS    ','GLONASS','GALILEO','GPS/GLO', &
                'GPS/GAL','GLO/GAL','ALL    '/), &
              'RDIGEN', 'Satellite System', irc, irCode, &
              valList=(/1,2,3,4,5,6,0/), result1=isasys)

! Tropospheric Model
! ------------------
  CALL trpopt(geoTec, itropo, iextra, itrmap, itrgrd)


! Check troposphere models allowed for microwave or optical measurements
! ----------------------------------------------------------------------
  IF ( geoTec /= 0 .AND.                     &
       (itropo == 4 .OR. itropo == 8) ) THEN
    WRITE (lfnerr, '(/,A, 2(/,16X,A),A,/)') &
           ' *** SR RDIGEN: Wrong selection of troposphere model!', &
                           'You are processing microwave observations,',     &
                           'but selected: ', keyValue(1)
    CALL exitrc(2)

  ELSEIF ( geoTec == 0 .AND.                    &
           itropo /= 4 .AND. itropo /= 8 ) THEN
    WRITE (lfnerr, '(/,A, 2(/,16X,A),A,/)') &
           ' *** SR RDIGEN: Wrong selection of troposphere model!', &
                           'You are processing optical observations,',     &
                           'but selected: ', keyValue(1)
    CALL exitrc(2)

  END IF

! Deactivate troposphere model if indicated
! -----------------------------------------
  CALL readKeys('FREQUENCY', keyValue, irc)
  IF (keyValue(1) == 'L4' .OR. &
      keyValue(1) == 'MELWUEBB' .OR. &
      keyValue(1) == 'DTEC') itropo = 0

! Printing Options
! ----------------
  priopt(1:20) = 0

  CALL ckoptb(1,(/'PRINT'/),srName,'Extended printing options', &
              irCode,result1 = iPrint)

  IF (iPrint == 1) THEN
    DO ii = 1, SIZE(prtKeyw)
      IF (ii == 3) CYCLE
      CALL readKeys(prtKeyw(ii), keyValue, irc)
      IF (irc == 0 .AND. keyValue(1) == '1') priopt(ii) = 1
    ENDDO
    IF (priopt(16) == 1) priopt(10) = 0

    ! No phase measurements: print no phase intervals
    IF (nInpFiles(1)+nInpFiles(3) == 0) priopt(20) = 0

  ENDIF

! Special observation data selection
! ----------------------------------
  CALL readKeys('SODSEL', keyValue, irc)
  CALL ckoptc(1,'SODSEL', keyValue, &
              (/'NO           ','NIGHT-TIME   ',   &
                'NON-ECLIPSING','SPEC.NON-ECL '/), &
              srName, 'Special observation data selection', irc, irCode, &
              valList=(/0,1,2,3/), result1=isosel)

! Read differencing level
  CALL readKeys('DIFLVL', keyValue, irc)
  CALL ckoptc(1,'DIFLVL', keyValue, (/'DOUBLE','ZERO  '/), srName, &
              'Differencing level', irc, irCode,                   &
              valList=(/1,0/), result1=difLvl)


! Read edition level
! ------------------
  CALL readKeys('EDTLVL', keyValue, irc)

  CALL ckoptr(1,'EDTLVL', keyValue, srName,                     &
              'Editing level for code observations',irc,irCode, &
              maxVal=1,empty=0d0,ge=0d0,result1=edtlvl)

! Deactivate editing level for GNSS double diff
  IF (difLvl == 1 .AND. geoTec == 1) edtlvl = 0.d0


! Use the geometrical part of the polarizaiton effect in the double
! difference case independent from the input options
! -----------------------------------------------------------------
  iPolar = 0
  IF (geoTec == 1 .AND. difLvl == 1) iPolar = 1


! Read polarization effect start epochs
! -------------------------------------
  IF (ipolar == 0 .AND. geoTec == 1) THEN

    CALL readKeys('POLARI2D', keyValue, irc)
    IF (keyValue(1) == "ALWAYS") THEN
      ipolar = 2
    ELSEIF (keyValue(1) == "NEVER") THEN
      ipolar = 0
    ELSE
      CALL ckoptd(1,'POLARI2D', keyValue, srName,                  &
                  'Polarization effect, total',irc,irCode,         &
                  maxVal=1,empty=0d0,result1=epoch)
      IF (globalWindow%t(1) > epoch-1D-6) ipolar = 2
    ENDIF

    IF (ipolar /= 2) THEN
      CALL readKeys('POLARI1D', keyValue, irc)
      IF (keyValue(1) == "ALWAYS") THEN
        ipolar = 1
      ELSEIF (keyValue(1) == "NEVER") THEN
        ipolar = 0
      ELSE
        CALL ckoptd(1,'POLARI1D', keyValue, srName,                &
                    'Polarization effect, geom.',irc,irCode,       &
                    maxVal=1,empty=0d0,result1=epoch)
        IF (globalWindow%t(1) > epoch-1D-6) ipolar = 1
      ENDIF
    ENDIF

  ENDIF


! Exit if an input failed
! -----------------------
  IF (irCode /= 0) CALL exitrc(2)

  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE rdigen

END MODULE

