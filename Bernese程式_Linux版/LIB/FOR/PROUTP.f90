MODULE s_PROUTP
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE proutp(iAuto,  iSetOp, nFrChk, minlen, iSave,  iIono,  iUsFlg, &
                  mrk1o2, maxZen, mxHole, mnCont, iPrnt1, mxIntr, ipProc, &
                  qq,     discLv, lTrip,  sigWgs, staFix, iPrnt2, sigL12, &
                  iWlScr, sWidth, minCyc, iRject, mxOGap, mxIonD, iAmNew, &
                  ntOnly, l5Clea, omcMax, mxAmbs, iZerod, iLeos,  iTropo, &
                  iExtra, kinSta, mxzLeo, rmsmax, jmpopt, toljmp)

! -------------------------------------------------------------------------
!
! Purpose:    This subroutine prints the input options of program MAUPRP
!             into the output file.
!
! Author:     D. Ineichen
!
! Created:    27-Sep-2001
! Last mod:   30-Jun-2008
!
! Changes:    26-Jun-2002  RD: Add new MAUPRP options
!             06-Dec-2002  RD: Add clock event handling
!             24-Aug-2006  AG: GMF added
!             07-Dec-2006  RD: Adjust MRK1O2 in DSRDBL.f
!             08-Sep-2007  RD: Advanced MXIOND (dep. from bsl-length)
!             08-Sep-2007  RD: Automatic switch between COMBINED/BOTH
!             30-Jun-2008  RD: VMF added
!
! SR used:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  IMPLICIT NONE

! List of parameters
! ------------------
! Input:

  INTEGER(i4b)               :: iAuto  ! Flag for interactive or automatic
                                       ! preprocessing (0: interactive)
  INTEGER(i4b)               :: iSetop ! Options adjusted to file header info
                                       ! 0: take input file options in any case
                                       ! 1: adjust options to file header
  INTEGER(i4b)               :: nFrChk ! Frequency(ies) to be checked
                                       ! 0: AUTO: L1 or COMBINED or BOTH
                                       ! 1: L1
                                       ! 2: L2
                                       ! 3: L1 and L2 together (via L5 and L3)
                                       ! 4: L1 and L2 separately
  REAL(r8b)                  :: minlen ! baseline length to decide COMBINED/BOTH
  INTEGER(i4b)               :: iSave  ! Save file option (1: YES)
  INTEGER(i4b)               :: iIono  ! Apply ionosphere model (1: YES)
  INTEGER(i4b)               :: iUsFlg ! Use or ignore obs file flags
  INTEGER(i4b)               :: mrk1o2 ! Mark unpaired observations (1:YES)
  INTEGER(i4b)               :: maxZen ! Max. satellite zenith distance (degree)
  INTEGER(i4b)               :: mxHole ! Max. gap in observations allowed
                                       ! to be considered as continuous (sec)
  INTEGER(i4b)               :: mnCont ! Minimal time interval of continuous
                                       ! observations (sec)
  INTEGER(i4b)               :: iPrnt1 ! Print level for non-param. screening
                                       ! 0: NO, 1: SUMMARY, 2: ALL
  INTEGER(i4b)               :: mxIntr ! Maximum interval length for
                                       ! polynomial fit (min)
  INTEGER(i4b), DIMENSION(2) :: ipProc ! Screening flag (1: YES)
                                       ! ipproc(1): screening single diff.
                                       ! ipproc(2): screening double diff.
  INTEGER(i4b), DIMENSION(2) :: qq     ! Pol. degree used for screening
                                       ! qq(1): for single diff. screening
                                       ! qq(2): for double diff. screening
  REAL(r8b)   , DIMENSION(2) :: discLv ! Max. allowed discontinuity
                                       ! discLv(1) for single diff. screening
                                       ! discLv(2) for double diff. screening
  INTEGER(i4b)               :: lTrip  ! Carrier for triple diff. solution
                                       ! 1: L1, 2: L2, 3: L3, 5: L5
  REAL(r8b)   , DIMENSION(3) :: sigWgs ! A priori sigmas for coordinates (m)
  CHARACTER(LEN=16)          :: staFix ! Name of station to be fixed when
                                       ! saving triple diff. coordinates
  INTEGER(i4b)               :: iPrnt2 ! Print level for cycle slip detection
                                       ! 0: NO, 1: SUMMARY, 2: ALL
  REAL(r8b)   , DIMENSION(2) :: sigL12 ! Rms errors for frequencies L1 and L2
  INTEGER(i4b), DIMENSION(2) :: iWlScr ! Wavelength factors for L1 and L2
                                       ! 1: full cycles, 2: half cycles
  INTEGER(i4b), DIMENSION(2) :: sWidth ! Number of nearest integers to be
                                       ! tested in L1/L2 and in L5
  INTEGER(i4b)               :: minCyc ! Accept cycle slips > "mincyc" cycles
  INTEGER(i4b)               :: iRject ! Reject outliers (1: YES)
  INTEGER(i4b)               :: mxOGap ! Maximum observation gap allowed in
                                       ! outlier rejection (sec)
  INTEGER(i4b), DIMENSION(3) :: mxiond ! maximum ionosphere change between
                                       ! epochs (in % of L1 cycles)
                                       ! i=1 : for BOTH
                                       ! i=2 : for COMBINED
                                       ! i=3 : length where COMBINED value is used
  INTEGER(i4b), DIMENSION(5) :: iAmNew ! Setting of new ambiguities
                                       ! i=1 : use cycle slip flag (0/1)
                                       ! i=2 : if problem in slip-fixing (0/1)
                                       ! i=3 : after gap larger than (sec)
                                       ! i=4 : use ambiguities in file (0/1)
                                       ! i=5 : min. time interval per amb. (sec)
  INTEGER(i4b)               :: ntOnly ! Test obs. with cycle slip flag only
  INTEGER(i4b)               :: l5Clea ! L5 is clean (except flagged epochs)
  REAL(r8b)                  :: omcMax ! Max. OBSERVED-COMPUTED value (m)
  INTEGER(i4b)               :: mxAmbs ! Max. number of ambiguities
  INTEGER(i4b)               :: iZerod ! Observation file type
                                       ! =1 : Zero diff. files
                                       ! =2 : Signle diff. files
  INTEGER(i4b)               :: iLeos  ! Select only LEOs/Non-LEOs files
                                       ! =0 : no LEOs in the file list
                                       ! =1 : only files with LEOs in list
  INTEGER(i4b)               :: itropo ! tropospheric model
  INTEGER(i4b)               :: iextra ! =0 : use measured values
                                       ! =1 : use atm. model values
                                       ! =2 : use est. bernese values
                                       ! =3 : Same as 2, but calling GETTRP2
                                       !      (special version for piecewise
                                       !      linear interpolat. of troposph.
                                       !      for CLKEST)
                                       !      IF (iextra==2) iextra=3
  INTEGER(i4b)               :: kinsta ! Kinematic coordinate estimation
  INTEGER(i4b)               :: mxzleo ! LEO max. zenith distance (degree)
  REAL(r8b)                  :: rmsmax ! Max rms allowed, otherwise sol.flag
  INTEGER(i4b),DIMENSION(:)  :: jmpopt ! Clock event options
                                       ! (1): 0/1 allow ms-jump cycle slips
                                       ! (2): min. size of a clock event (ns)
                                       ! (3): mark epochs with clock events
                                       !      up to (in s)
                                       ! (4): 0/1 ambiguities for all satellites
                                       ! (5): 0/1 flag if ms-jump in file
                                       ! (6): 0/1 flag if a clock event in file
  REAL(r8b)                  :: toljmp ! Tolerance to detect a ms-jump

! Output:

! Used functions
! --------------

! Local parameters
! ----------------

! Local types
! -----------

! Local variables
! ---------------
  INTEGER(i4b)               :: maxEle,mxeLeo

! Write input options
! -------------------

  WRITE(lfnprt,'(A,/,A,/,A,///,A,/,A,/,4(/,A,I1),//)')                         &
  ' ************************************************************************ ',&
  ' AUTOMATIC PREPROCESSING: PROCESSING OPTIONS '                             ,&
  ' ************************************************************************ ',&
  ' PROCESSING MODE:                                                         ',&
  ' ---------------                                         *                ',&
  ' MANUAL (=0) OR AUTOMATIC (=1) MODE               --> :  ', iAuto,          &
  ' USE ONLY FILE CONTAINING LEOS     (NO=0, YES=1)  --> :  ', iLeos,          &
  ' USE ONLY FILE CONTAINING STATIONS (NO=0, YES=1)  --> :  ', 1-iLeos,        &
  ' PROCESS ZERO (=0) OR SINGLE (=1) DIFF. FILES     --> :  ', iZerod-1

  WRITE(lfnprt,'(A,/,A,//,A,I1,//)')                                           &
  ' SET SOME OPTIONS ACCORDING TO HEADER FILE:                               ',&
  ' -----------------------------------------               *                ',&
  ' ADJUST OPTIONS ACCORDING TO FILE HEADER INFO.    --> :  ', iSetOp

  IF (nFrChk /= 0) THEN
    WRITE(lfnprt,'(A,/,A,//,A,I1,//)')                                         &
    ' FREQUENCIES:                                                           ',&
    ' -----------                                             *              ',&
    ' CHECK FREQUENCIES (L1=1, L2=2, L1&L2=3, L1,L2=4) --> :  ', nFrChk
  ELSE
    WRITE(lfnprt,'(A,/,A,//,A,/,A,/A,F6.0,A,/,A,//)')                          &
    ' FREQUENCIES:                                                           ',&
    ' -----------                                             ****           ',&
    ' CHECK FREQUENCIES (L1=1, L2=2, L1&L2=3, L1,L2=4) --> :  AUTO',           &
    '   SINGLE FREQUNCY FILES:                                L1',             &
    '   BOTH (L1,L2) MODE FOR BASELINES SHORTER THAN          ', MINLEN,' km', &
    '   OTHERWISE COMBINED (L1&L2) MODE'
  ENDIF

  WRITE(lfnprt,'(A,/,A,//,A,I1,//)')                                           &
  ' SAVE OPTION:                                                             ',&
  ' -----------                                             *                ',&
  ' SAVE CHANGES (FOR AUTOMATIC MODE ONLY)           --> :  ',iSave

  WRITE(lfnprt,'(A,/,A,//,A,I1,//)')                                           &
  ' IONOSPHERE MODEL:                                                        ',&
  ' ----------------                                        *                ',&
  ' APPLY IONOSPHERE MODEL                           --> :  ', iIono

  WRITE(lfnprt,'(A,/,A,/,12(/,A),/,A,I2,/,3(/,A),/,A,I2,//)')                  &
  ' TROPOSPHERE MODEL:                                                       ',&
  ' -----------------                                                        ',&
  ' TROPOSPHERE MODEL                                      **                ',&
  '    NO METEO                          =  0                                ',&
  '    SAASTAMOINEN                      =  1                                ',&
  '    HOPFIELD (REMONDI)                =  2                                ',&
  '    ESSEN + FROOME                    =  3                                ',&
  '    NIELL                             =  5                                ',&
  '    GMF                               =  6                                ',&
  '    VMF                               =  7                                ',&
  '    SAASTAMOINEN DRY PART ONLY        = 11                                ',&
  '    HOPFIELD DRY PART ONLY            = 12                                ',&
  '    NIELL DRY PART ONLY               = 15                                ',&
  '    GMF DRY PART ONLY                 = 16                                ',&
  '    VMF DRY PART ONLY                 = 17        --> : ',iTropo,           &
  ' METEO DATA                                             **                ',&
  '    USE MET FILES                     =  0                                ',&
  '    STANDARD MODEL                    =  1                                ',&
  '    BERNESE TROPOSPHERE ESTIMATES     =  2        --> : ',iExtra

  maxEle = 90 - maxZen
  mxeLeo = 90 - mxzLeo
!  WRITE(lfnprt,'(A,/,A,//,A,I5,/,A,I5,/,A,I5,A,/,A,I5,A,/,A,I5,/,A,I5,//)')    &
  WRITE(lfnprt,'(A,/,A,//,A,I5,/,A,I5,A,/,A,I5,A,/,A,I5,/,A,I5,//)')           &
  ' MARKING OF OBSERVATIONS:                                                 ',&
  ' -----------------------                                *****             ',&
  ' USE MARKING FLAGS IN OBSERVATION FILES     (0/1) --> : ', iUsFlg,          &
!  ' MARK L1 WITHOUT L2, L2 WITHOUT L1 OBSERV.  (0/1) --> : ', mrk1o2,          &
  ' MARK OBSERVATIONS BELOW MINIMAL ELEVATION  (DEG) --> : ', maxEle,' stations',&
  ' MARK OBSERVATIONS BELOW MINIMAL ELEVATION  (DEG) --> : ', mxeLeo,' for LEOs',&
  ' OBS. STILL CONTINUOUS IF GAPS SMALLER THAN (SEC) --> : ', mxHole,          &
  ' MINIMUM TIME INTERVAL OF CONTINUOUS OBS.   (SEC) --> : ', mnCont

  WRITE(lfnprt,'(A,/,A,/,A,/)')                                                &
  ' ------------------------------------------------------------------------ ',&
  ' PART 1: NON-PARAMETRIC SCREENING AND TRIPLE DIFFERENCE SOLUTION          ',&
  ' ------------------------------------------------------------------------ '

  WRITE(lfnprt,'(A,/,A,//,A,I2,/,A,I2,//)')                                    &
  ' GENERAL PARAMETERS:                                                      ',&
  ' ------------------                                     **                ',&
  ' PRINT MESSAGES (NO=0, SUMMARY=1, ALL=2)          --> : ', iPrnt1,          &
  ' MAX. INTERVAL LENGTH (MIN) FOR POLYNOMIAL FIT    --> : ', mxIntr

  WRITE(lfnprt,'(A,/,A,//,A,I2,/,A,I2,/,A,/,A,F6.3//)')                        &
  ' SINGLE DIFFERENCE SCREENING:                                             ',&
  ' ---------------------------                            **                ',&
  ' SCREEN SINGLE DIFFERENCES                        --> : ', ipProc(1),       &
  ' POLYNOMIAL DEGREE                                --> : ', qq(1),           &
  '                                                        **.***            ',&
  ' DISCONTINUITY LEVEL (M)                          --> : ', discLv(1)

  WRITE(lfnprt,'(A,/,A,//,A,I2,/,A,I2,/,A,/,A,F6.3//)')                        &
  ' DOUBLE DIFFERENCE SCREENING:                                             ',&
  ' ---------------------------                            **                ',&
  ' SCREEN DOUBLE DIFFERENCES                        --> : ', ipProc(2),       &
  ' POLYNOMIAL DEGREE                                --> : ', qq(2),           &
  '                                                        **.***            ',&
  ' DISCONTINUITY LEVEL (M)                          --> : ', discLv(2)

  WRITE(lfnprt,'(A,/,A,//,2(A,I2,/),A,/,A,F6.3,/,A,F6.3,/,A,F6.3)')            &
  ' TRIPLE DIFFERENCE SOLUTION:                                              ',&
  ' --------------------------                             **                ',&
  ' FREQUENCY TO BE USED (L1=1, L2=2, L3=3, L5=5)    --> : ', lTrip,           &
  ' KINEMATIC COORDINATE ESTIMATION (NO=0, YES=1)    --> : ', kinSta,          &
  '                                                        **.***            ',&
  ' A PRIORI SIGMA FOR STATION COORDINATE X (M)      --> : ', sigWgs(1),       &
  ' A PRIORI SIGMA FOR STATION COORDINATE Y (M)      --> : ', sigWgs(2),       &
  ' A PRIORI SIGMA FOR STATION COORDINATE Z (M)      --> : ', sigWgs(3)

  WRITE(lfnprt,'(A,2(/,A,F8.4),//,A,/,A,A,//)')                                   &
  '                                                        ***.****          ',&
  ' MAXIMUM ALLOWED VALUE FOR OBSERVED-COMPUTED (M)  --> : ', omcMax,          &
  ' RMS LIMIT FOR EPOCH SOLUTION (ELSE FLAG)    (M)  --> : ', rmsMax,          &
  '                                                        ****************  ',&
  ' STATION TO BE FIXED WHEN SAVING COORDINATES      --> : ', TRIM(staFix)

  WRITE(lfnprt,'(A,/,A,//,A,I5,/,A,F10.4,/,2(/,A,I5),/,3(/,A,I5),//)')         &
  ' CLOCK EVENT HANDLING:                                                    ',&
  ' --------------------                                                     ',&
  ' MINIMUM SIZE OF A CLOCK EVENT               (NS) --> : ',jmpopt(2),        &
  ' TOLERANCE FOR MS-JUMP DETECTION             (MS) --> : ',toljmp,           &
  ' FLAG THE OBSERVATION FILE FOR DELETION     (0/1) --> : ',jmpopt(5),        &
  ' REPAIR THE MS-JUMP AS CYCLE SLIP           (0/1) --> : ',jmpopt(1),        &
  ' FLAG THE OBSERVATION FILE FOR DELETION     (0/1) --> : ',jmpopt(6),        &
  ' MARK OBSERVATIONS UP TO A TIME INTERVAL      (S) --> : ',jmpopt(3),        &
  ' SET AMBIGUITIES FOR ALL SATELLITES         (0/1) --> : ',jmpopt(4)

  WRITE(lfnprt,'(A,/,A,/,A,/)')                                                &
  ' ------------------------------------------------------------------------ ',&
  ' PART 2: CYCLE SLIP DETECTION AND OUTLIER REJECTION                       ',&
  ' ------------------------------------------------------------------------ '

  WRITE(lfnprt,'(A,/,A,//,A,I1,//)')                                           &
  ' PRINT MESSAGES:                                                          ',&
  ' --------------                                          *                ',&
  ' PRINT MESSAGES (NO=0, SUMMARY=1, ALL=2)          --> :  ', iPrnt2

  WRITE(lfnprt,'(A,/,A,//,A,F7.4,/,A,F7.4,//)')                                &
  ' SIGMA OF PHASE OBSERVATIONS:                                             ',&
  ' ---------------------------                            **.****           ',&
  ' SIGMA FOR L1 OBSERVATIONS (M)                    --> : ', sigL12(1),       &
  ' SIGMA FOR L2 OBSERVATIONS (M)                    --> : ', sigL12(2)


  WRITE(lfnprt,'(A,/,A,//,A,I1,/,A,I1,/)')                                     &
  ' WAVELENGTH FACTORS:                                                      ',&
  ' ------------------                                      *                ',&
  ' DETECT CYCLES (=1) OR HALF CYCLES (=2) IN L1     --> :  ', iWlScr(1),      &
  ' DETECT CYCLES (=1) OR HALF CYCLES (=2) IN L2     --> :  ', iWlScr(2)

  WRITE(lfnprt,'(A,/,A,//,A,I2,/,A,I2,/,A,I2,/,A,I2,//)')                      &
  ' CYCLE SLIP DETECTION:                                                    ',&
  ' ---------------------                                  **                ',&
  ' TEST OBS. WITH CYCLE SLIP FLAG ONLY (0/1)        --> : ', ntOnly,          &
  ' L5 IS CLEAN (EXCEPT FLAGGED EPOCHS) (0/1)        --> : ', l5Clea,          &
  ' NUMBER OF INTEGERS TO BE TESTED IN L1 (OR L2)    --> : ', sWidth(1),       &
  ' NUMBER OF INTEGERS TO BE TESTED IN L5            --> : ', sWidth(2)

  WRITE(lfnprt,'(A,/,A,//,A,I7,/,A,//)')                                       &
  ' SIZE OF CYCLE SLIPS TO BE ACCEPTED:                                      ',&
  ' ----------------------------------                     *******           ',&
  ' ACCEPT ALL CYCLE SLIPS LARGER THAN (CYCLES)      --> : ',minCyc,           &
  '    (-1: ACCEPT NO CYCLE SLIPS)                                           '

  WRITE(lfnprt,'(A,/,A,//,A,I5,/,A,I5,/,A,I5,A,/,56X,I5,A,/,A,I5,A,//)')       &
  ' OUTLIER REJECTION:                                                       ',&
  ' -----------------                                      *****             ',&
  ' OUTLIER REJECTION                          (0/1) --> : ', iRject,          &
  ' MAXIMUM OBSERVATION GAP ALLOWED            (SEC) --> : ', mxOGap,          &
  ' MAX. IONOS.DIFF. BETW. EPOCHS (o/o OF L1 CYCLES) --> : ', mxIonD(1), ' FOR BOTH', &
                                                              mxIonD(2), ' FOR COMBINED', &
  ' USE THE COMBINED MODE VALUE FOR BSL LONGER THAN  --> : ', mxIonD(3), ' KM'

  WRITE(lfnprt,'(A,/,A,//,A,I5,/,A,I5,/,A,I5,/A,I5,/,A,I5,//)')                &
  ' SETTING OF NEW AMBIGUITIES:                                              ',&
  ' --------------------------                             *****             ',&
  ' NEW AMBIG. IF CYCLE SLIP FLAG SET IN FILE  (0/1) --> : ', iAmNew(1),       &
  ' NEW AMBIG. IF CYCLE SLIP DETECTION PROBLEM (0/1) --> : ', iAmNew(2),       &
  ' NEW AMBIGUITY AFTER A GAP LARGER THAN      (SEC) --> : ', iAmNew(3),       &
  ' USE AMBIGUITIES FROM FILE                  (0/1) --> : ', iAmNew(4),       &
  ' MINIMUM TIME INTERVAL PER AMBIGUITY        (SEC) --> : ', iAmNew(5)

  WRITE(lfnprt,'(A,/,A,//,A,I5,///,A)')                                        &
  ' REMOVE OBSERVATIONS IF TOO MANY AMBIGUITIES:                             ',&
  ' -------------------------------------------            *****             ',&
  ' MAXIMUM NUMBER OF ALLOWED AMBIGUITIES            --> : ', mxAmbs,          &
  ' ------------------------------------------------------------------------ '

END SUBROUTINE proutp

END MODULE
