MODULE s_RDORBI
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdorbi(narc  , nunb,   locq,   mod,    npotmx, iprres, iantof, &
                  dtpi  , niter,  qq,     arcfil, tfl   , window, indtim, &
                  dtpiv , qvar  , addvar, rprfrm, genupd, iupd  , orbmod, &
                  modflg, cmcyn,  itim  , nfil  , tabfil, prefil, stcopt, &
                  otdmin, otddeg, antthr, erpmod)

! -------------------------------------------------------------------------
! Purpose:    This is a new version of the old subroutine RDORBI.f that
!             reads the input options of the program ORBGEN
!
! Author:     L. Mervart
!
! Created:    12-May-2000
!
! Changes:    09-Nov-2000 CU: switch to new menu system
!             10-Nov-2000 CU: add error message
!             13-Nov-2000 RD: "B" is the default orbit model
!             14-Nov-2000 RD: cosmetics for the error messages
!             14-Nov-2000 RD: bugfix in error handling
!             23-Nov-2000 RD: handle wildcard time windows
!                             warning for extrapolation
!             16-Jan-2001 RD: new call of SR SESWIN
!             26-Jun-2001 RD: Use alcerr for allocation
!             06-Jul-2001 DS: "D" model
!             01-Feb-2001 DS: orbmod(6)=1 for "D"
!             21-Feb-2002 DS: TEG4 and EIGEN1S gravity models
!             21-Feb-2002 DS: Use modflg for model
!             25-Feb-2002 DS: "L" model for LEO
!             16-Oct-2002 DS: "G" model for LEO
!             02-Nov-2002 HU: New parameters for rdpreh
!             22-Jan-2003 HU: Read tosc with higher precision
!             28-JAN-2003 AJ: d0 for divisions added
!             17-FEB-2003 LM: Use m_maxdim
!             25-Feb-2003 SC: Radiobuttons for selection of input files
!             15-Apr-2003 RD: Use GTTIMWIN for reading the time window
!             17-Apr-2003 RD: Use ckopt-SR set
!             23-Apr-2003 RD: Nullify local pointers
!             06-Aug-2003 HU: New STD format
!             05-Nov-2003 RD: No checks for "unknown" orbit model
!             12-Dec-2003 HU: Allow backward integration
!             20-Apr-2005 AJ: Extended RPR format, additional var. eqns
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             22-Jan-2006 HU: Round dtpi and dtpiv to integer sec
!             09-Feb-2006 HU: Time for arcs has to be multiple of dtpi
!                         HB: Add time window as new parameter
!             18-Jul-2006 AG: CMC option added
!             21-Sep-2006 HU: Mean pole handling (disabled)
!             18-Oct-2006 MP: ITIM option added
!             09-Nov-2006 AG: Mean pole handling enabled
!             10-Sep-2007 GB: Major review: module STCOGN.F90 added
!             21-Jul-2008 DT: System of dynamical orbit parameters:
!                             DYX=1, RSW=2, DRSW=3
!             30-Jul-2008 DT: Check TIMSYS for PRE files, correct UTC->GPS
!             05-Aug-2008 DT: Get min. size of OT from panel (otdmin)
!             13-Aug-2010 CR: Earth radiation pressure added
!             01-Sep-2008 DT: LENINTER, VARLNINT in hours/minutes/seconds
!             29-Sep-2008 RD: Sort PRE- and TAB- files already here
!             30-Sep-2008 RD: Residuals for LAST iteration
!             14-Nov-2008 DT: Time system according to panel (indtim); correct
!                             start/end time if PRE file not in GPS time
!             03-Dec-2010 KS: Add otddeg (max degree of ocean tides model)
!             03-Dec-2010 RD: CMC for ATL added
!             13-Jan-2011 MF: Nullify(filist)
!             06-May-2011 HB: Use always model identifier '?'
!             16-May-2011 RD: odtmin is zero or taken from the ELE file
!                             (otddeg from "MXOCTI" is now the only limit)
!             05-Aug-2011 HB: Consider also 'R0' to get modFlg
!             15-Feb-2012 RD: Read option only when active CMCYN_?
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfnloc, &
                      fileNameLength, keyValueLength, timStrgLength2
  USE m_maxdim, ONLY: maxsat
  USE m_time,   ONLY: t_timint
  USE s_stcogn, ONLY: t_stcopt
  USE s_ckoptr
  USE s_dordup
  USE s_opnfil
  USE s_alcerr
  USE s_seswin
  USE s_gtfile2
  USE s_opnerr
  USE s_rdpreh
  USE s_gttimwin
  USE f_tstkey
  USE s_timst2
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckopti
  USE s_ckoptt
  USE s_gtflna
  USE s_splarg
  USE s_dimtst
  USE f_dgpsut
  USE s_gttabhed
  IMPLICIT NONE


! List of Parameters
! ------------------
  INTEGER(i4b)                 :: narc   ! number of arcs to be processed
  INTEGER(i4b)                 :: nunb   ! number of unknowns in orbit determ.
  INTEGER(i4b), DIMENSION(6,*) :: locq   ! description of parameters
                                         ! locq(4,ipar) = 1,..6 : six elements
                                         !              =  7: d0 (p0-parameter)
                                         !              =  8: y0 (y-bias)
                                         !              =  9: x0-parameter
                                         !              = 10: dc-parameter
                                         !              = 11: yc-parameter
                                         !              = 12: xc-parameter
                                         !              = 13: ds-parameter
                                         !              = 14: ys-parameter
                                         !              = 15: xs-parameter
  INTEGER(i4b)                 :: mod    ! terms set up for numerical
                                         ! integration and saved in output
                                         ! = 1 : only time-independent terms
                                         !       set up for integration (7-9)
                                         ! = 2 : all dynamical terms (7-15)
                                         ! = 3 : like 2 plus partials W.R.T.
                                         !       (1-15) osculating elements
  INTEGER(i4b)                 :: npotmx ! maximum degree/order for earth pot.
  INTEGER(i4b)                 :: iprres ! residual print option
                                         ! = -1 : print all iterations
                                         ! =  0 : no residuals printed
                                         ! =  i : print iteration i (i > 0)
  LOGICAL, DIMENSION(2)        :: cmcyn  ! CMC correction
                                         ! =0 do not apply
                                         ! =1 apply OTL CMC correction
  INTEGER(i4b)                 :: itim   ! RPR Springer model
                                         ! =0 all other models
                                         ! =1 RPR Springer model
  INTEGER(i4b)                 :: iantof ! correction for antenna offsets
                                         ! =0: tabular file positions with
                                         !     respect to center of mass (no
                                         !     offset correction)
                                         ! =1: antenna offset correction
  INTEGER(i4b)                 :: antthr ! =0: no antenna thrust
                                         ! =1: with antenna thrust
  INTEGER(i4b)                 :: erpmod ! Earth Radiation Pressure
                                         ! =0: no model
                                         ! =1: analytical
                                         ! =2: numerical
  REAL(r8b)                    :: dtpi   ! length of partial integration int.
  INTEGER(i4b)                 :: niter  ! number of iterations
  INTEGER(i4b)                 :: qq     ! degree of the approximating polynom.
  INTEGER(i4b), DIMENSION(*)   :: arcfil ! TAB-file file numbers
  REAL(r8b),    DIMENSION(2,*) :: tfl    ! first/last requested time for arc
  REAL(r8b),    DIMENSION(2)   :: window ! requested time window
  INTEGER(i4b)                 :: indtim ! time definition for tab. orbit files / result STD orbit
                                         ! =1: time in GPS time
                                         ! =2: time in UTC
  REAL(r8b)                    :: dtpiv  ! time interval for var eqns (output)
  INTEGER(i4b)                 :: qvar   ! pol. degree for var eqns (output)
  INTEGER(i4b)                 :: addvar ! additional var eqns
  INTEGER(i4b)                 :: rprfrm ! extended RPR format
  INTEGER(i4b)                 :: genupd ! =1: generate orbit from TAB-files
                                         ! =2: generate orbit from PRE-files
                                         ! =3: update orbit using *.ELE file
  INTEGER(i4b)                 :: iupd   ! redundant - nonsense
  INTEGER(i4b), DIMENSION(*)   :: orbmod ! orbit model array
                                         ! 1) =0 : " " (OLD MODEL)
                                         !    =1 : "A" (K2,JGM3,TIDPOT,GENREL)
                                         !    =2 : "B" (DE200,PLAPOS,TIDPT2,
                                         !              OTIDES)
                                         !    =3 : "C" ("A"+COLUMBO PARAMETERS)
                                         !    =4 : "L"
                                         !    =99: "?" (FREE MODEL)
                                         ! 2) =0 : GEMT3
                                         !    =1 : JGM3
                                         !    =2 : EGM96
                                         !    =3 : TEG-4
                                         !    =4 : EIGEN1S
                                         ! 3) =0 : K2=0.285
                                         !    =1 : K2=0.300
                                         ! 4) =0 : USE "TIDALF"
                                         !    =1 : USE "TIDPOT"
                                         !    =2 : USE "TIDPT2"
                                         !    =3 : USE "TIDPT2" IERS2003 MEAN POLE
                                         ! 5) =0 : NO GENERAL REL. CORRECTIONS
                                         !    =1 : USE "GENREL"
                                         ! 6) =0 : USE STANDARD RPR-PARAMETERS
                                         !    =1 : USE COLOMBO 1-REV PAR.
                                         !                (D-->R,Y-->S,X-->W)
                                         !    =2 : USE SLR PARAMETERIZATION
                                         !         D-->Rad, Y-->along-track, X-->Cross-track
                                         ! 7) =1 : INCLUDE GRAV. PERTURBATIONS
                                         !         BY VENUS AND JUPITER, USE
                                         !         MODEL FOR SOLID EARTH TIDES
                                         ! 8) =1 : EARTH TIDES
                                         ! 9) =1 : EARTH ALBEDO
                                         !10) =1 : SIMPLE MODEL FOR DIRECT RPR
                                         !11) =1 : AIR DRAG USING MSIS-E MODEL
                                         !12) =1 : CONSIDER SHADOW OF MOON
                                         !13) =1 : ROCK IV A PRIORI MODEL
                                         !14) =1 : NON-GRAVITY ACCELERATIONS FROM EXT.FILE
                                         !15) =1 : ESTIMATE GEOPOTENTIAL COEFFICIENTS
  REAL(r8b)                    :: otdmin ! Min. size of ocean tides to be applied
  INTEGER(i4b)                 :: otddeg ! Max. degree of ocean tide model
  INTEGER(i4b)                 :: nFil   ! Number of input files

  CHARACTER(LEN=fileNameLength), DIMENSION(:)   :: tabfil ! tabular input files
  CHARACTER(LEN=fileNameLength), DIMENSION(:,:) :: prefil ! precise input files
  TYPE(t_stcopt)               :: stcopt ! options for stoch. pulses

  CHARACTER(LEN=1)             :: modflg ! model flag: " ","A","B","C","L"

! Local parameters
! ----------------
  CHARACTER(LEN=6),  PARAMETER :: srName = 'rdorbi'


! Local Variables
! ---------------
  TYPE(t_timint)                                             :: timint

  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER       :: keyValue
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER       :: argv
  CHARACTER(LEN=fileNameLength)                              :: filnam
  CHARACTER(LEN=fileNameLength), DIMENSION(:,:), POINTER     :: filist
  CHARACTER(LEN=80)                                          :: strg80
  CHARACTER(LEN=57), DIMENSION(4)                            :: title
  CHARACTER(LEN=19)                                          :: tstr1,tstr2
  CHARACTER(LEN=5)                                           :: coosys
  CHARACTER(LEN=5)                                           :: datdes
  CHARACTER(LEN=3)                                           :: orbtyp
  CHARACTER(LEN=3)                                           :: timsys
  CHARACTER(LEN=2)                                           :: filtyp
  CHARACTER(LEN=4)                                           :: agency
  CHARACTER(LEN=4)                                           :: help1
  CHARACTER(LEN=4)                                           :: help2
  INTEGER(i4b)                                               :: ios
  INTEGER(i4b)                                               :: irCode
  INTEGER(i4b), DIMENSION(15)                                :: list
  INTEGER(i4b)                                               :: ipar
  INTEGER(i4b)                                               :: irc
  INTEGER(i4b)                                               :: iarc
  INTEGER(i4b)                                               :: ifil
  INTEGER(i4b)                                               :: ifrmat
  INTEGER(i4b)                                               :: nsat
  INTEGER(i4b), DIMENSION(maxSat)                            :: satnum
  INTEGER(i4b), DIMENSION(maxSat)                            :: satwgt
  INTEGER(i4b)                                               :: nepo
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE                    :: indext
!  INTEGER(i4b)                                               :: mpol
  INTEGER(i4b)                                               :: iac
  INTEGER(i4b)                                               :: orbsys  ! 1: DYX
                                                                        ! 2: RSW
                                                                        ! 3: DRSW
  REAL(r8b)                                                  :: tmin
  REAL(r8b)                                                  :: tmax
  REAL(r8b)                                                  :: dt
  REAL(r8b)                                                  :: tfirst
  REAL(r8b)                                                  :: dttab
  REAL(r8b)                                                  :: baspos
  REAL(r8b)                                                  :: basclk
  REAL(r8b), DIMENSION(:), ALLOCATABLE                       :: tjbeg
  REAL(r8b), DIMENSION(:), ALLOCATABLE                       :: tjend
  REAL(r8b)                                                  :: interv
  REAL(r8b)                                                  :: gpsutc
  REAL(r8b)                                                  :: timcor
  REAL(r8b)                                                  :: numVal

! Commons
! -------
  CHARACTER(LEN=6) :: mxnarc,mxnfil
  INTEGER(i4b)     :: mxcarc,mxcfil
  COMMON/mcmarc/mxcarc,mxnarc
  COMMON/mcmfil/mxcfil,mxnfil

! Init variables
! --------------
  irCode = 0
  list(:) = 0

  NULLIFY(keyValue)
  NULLIFY(argv)
  NULLIFY(filist)

! Use mean pole: mpol=1: no, mpol=2: yes
! -------------
!!  mpol=1
!!  mpol=2

! Create or Update
! ----------------
  CALL ckoptb (1, (/ 'RADIO_T','RADIO_P','RADIO_E' /), srName, &
               'Select type of input file',irCode,result1=genupd)

  IF (genupd == 3) THEN
    iupd = 1
  ELSE
    iupd = 0
  ENDIF

! System for dynamical parameters
! -------------------------------
  CALL ckoptb (1, (/ 'SYSDYX','SYSRSW','SYSDSW' /), srName, &
               'Select system of dynamical parameters',irCode,result1=orbsys)

! Dynamical Parameters
! --------------------
  list(1:6) = 1

  IF ( orbsys == 1 .OR. orbsys == 3 ) THEN
    CALL ckoptb(1,(/ 'PARD0' /),srName, &
                'Setup solar radiation pressure',irCode,result1=list(7))

  ELSE IF ( orbsys == 2 ) THEN
    CALL ckoptb(1,(/ 'PARR0' /),srName, &
                'Setup solar radiation pressure',irCode,result1=list(7))
  END IF

  IF ( orbsys == 1 ) THEN
    CALL ckoptb(1,(/ 'PARY0' /),srName, &
                'Setup solar radiation pressure',irCode,result1=list(8))

    CALL ckoptb(1,(/ 'PARX0' /),srName, &
                'Setup solar radiation pressure',irCode,result1=list(9))

    CALL ckoptb(1,(/ 'PARDP' /),srName, &
                'Setup periodic solar radiation pressure',irCode,result1=list(10))

    CALL ckoptb(1,(/ 'PARYP' /),srName, &
                'Setup periodic solar radiation pressure',irCode,result1=list(11))

    CALL ckoptb(1,(/ 'PARXP' /),srName, &
                'Setup periodic solar radiation pressure',irCode,result1=list(12))

  ELSE IF (orbsys == 2 ) THEN
    CALL ckoptb(1,(/ 'PARS0' /),srName, &
                'Setup solar radiation pressure',irCode,result1=list(8))

    CALL ckoptb(1,(/ 'PARW0' /),srName, &
                'Setup solar radiation pressure',irCode,result1=list(9))

    CALL ckoptb(1,(/ 'PARRP' /),srName, &
                'Setup periodic solar radiation pressure',irCode,result1=list(10))

    CALL ckoptb(1,(/ 'PARSP' /),srName, &
                'Setup periodic solar radiation pressure',irCode,result1=list(11))

    CALL ckoptb(1,(/ 'PARWP' /),srName, &
                'Setup periodic solar radiation pressure',irCode,result1=list(12))

  ELSE IF (orbsys == 3 ) THEN
    CALL ckoptb(1,(/ 'PARS0' /),srName, &
                'Setup solar radiation pressure',irCode,result1=list(8))

    CALL ckoptb(1,(/ 'PARW0' /),srName, &
                'Setup solar radiation pressure',irCode,result1=list(9))

    CALL ckoptb(1,(/ 'PARRP' /),srName, &
                'Setup periodic solar radiation pressure',irCode,result1=list(10))

    CALL ckoptb(1,(/ 'PARSP' /),srName, &
                'Setup periodic solar radiation pressure',irCode,result1=list(11))

    CALL ckoptb(1,(/ 'PARWP' /),srName, &
                'Setup periodic solar radiation pressure',irCode,result1=list(12))

  END IF

  list(13:15) = list(10:12)

! Estimate/Store all Partials
! ---------------------------
  mod  =  3

! Number of Unknowns, locq
! ------------------------
  nunb = 0
  DO ipar = 1, 15
    IF (list(ipar) == 1) THEN
      nunb = nunb + 1
      locq(1, nunb) = 3
      locq(2, nunb) = 0
      locq(3, nunb) = 0
      locq(4, nunb) = ipar
    END IF
  END DO


! Write nunb into locq(5,*)
! -------------------------
  DO ipar=1,nunb
    locq(5,ipar)=nunb
  END DO

! Potential Degree
! ----------------
  CALL readkeys('MXPOTDEG', keyValue, irc)

  CALL ckopti(1,'MXPOTDEG', keyValue, srName,         &
              'Degree of earth potential',irc,irCode, &
              maxVal=1,ge=2,result1=npotmx)

! Number of Iterations
! --------------------
  IF (genupd == 3) THEN
    niter =1
  ELSE

    CALL readkeys('NUMITER', keyValue, irc)

    CALL ckopti(1,'NUMITER', keyValue, srName,                &
                'Number of iterations',irc,irCode,            &
                maxVal=1,ge=1,result1=niter)
  ENDIF

! Print Residuals
! ---------------
  IF (genupd == 3) THEN
    iprres = 0
  ELSE
    CALL readkeys('PRTRESID', keyValue, irc)

    CALL ckoptc(1,'PRTRESID', keyValue, (/'NO  ','ALL ','LAST'/), srName, &
                'Print residuals',irc,irCode,                             &
                other=-999,valList=(/0,-1,-2/),error=-999,maxVal=1,       &
                result1=iprres)

    IF (iprres == -2) THEN
      iprres=niter
    ELSE IF (iprres == -999 .AND. irc == 0) THEN
      CALL ckopti(1,'PRTRESID', keyValue,srName,                 &
                  'Print residuals',irc,irCode,                  &
                  maxVal=1,ge=1,result1=iprres)
    ENDIF
  ENDIF


! Apply OTL CMC correction
! ------------------------
  IF  (genupd == 2) THEN
    CALL gtflna(0,'OCNLOAD',filnam,irc)
    IF ( irc == 0 ) THEN
      CALL ckoptb(1,(/'CMCYN_O'/),srName,                           &
                  'Apply OTL CMC correction for OTL',irCode,        &
                  resultL=cmcyn(1))
    ELSE
      cmcyn(1) = .FALSE.
    ENDIF
  ENDIF

! Apply OTL CMC correction
! ------------------------
  IF  (genupd == 2) THEN
    CALL gtflna(0,'ATMLOAD',filnam,irc)
    IF ( irc == 0 ) THEN
      CALL ckoptb(1,(/'CMCYN_A'/),srName,                           &
                  'Apply ATL CMC correction for ATL',irCode,        &
                  resultL=cmcyn(2))
    ELSE
      cmcyn(2) = .FALSE.
    ENDIF
  ENDIF

! Apply Antenna Offsets
! ---------------------
  CALL ckoptb(1,(/'ANTOFF'/),srName,                           &
              'Apply antenna offsets',irCode,                  &
              result1=iantof)

! Earth Radiation Pressure
! ------------------------
  IF(tstkey('ERPMODFL'))THEN
    CALL readkeys('ERPMODFL', keyValue, irc)

    CALL ckoptc(1,'ERPMODFL',keyValue,                         &
                (/ 'None      ','Analytical','Numerical ' /),  &
                srName,'Earth Radiation Pressure',irc,irCode,  &
                maxVal=1,result1=erpmod)
    erpmod = erpmod - 1
  ENDIF

! Antenna Thrust
! --------------
  IF(tstkey('ANTTHRFL'))THEN
    CALL ckoptb(1,(/'ANTTHRFL'/),srName,                       &
                'Navigation Antenna Thrust',irCode,            &
                result1=antthr)
  ENDIF

! Max. degree of ocean tides model
! --------------------------------
  CALL readKeys('MXOCTI', keyValue, irc)

  CALL ckopti(1,'MXOCTI', keyValue, srName,         &
             'Ocean tides upper limits, degree n=', &
              irc,irCode,maxVal=1,result1=otddeg)

! Min. size of ocean tides to be applied [cm]
! -------------------------------------------
!  CALL readkeys('OTDMIN', keyValue, irc)
!
!  CALL ckoptr(1,'OTDMIN', keyValue, srName,                        &
!              'Min. size of ocean tides to be applied',irc,irCode, &
!              ge=0d0,result1=otdmin)
  otdmin = 0d0

! RPR Springer model
! ------------------
  itim = 0
  IF ( tstkey('ITIM') )                                       &
    CALL ckoptb(1,(/'ITIM'/),srName,                          &
                'RPR Springer',irCode,                        &
                result1=itim)

! Eq. of Motion - Interval Length
! -------------------------------
  CALL readkeys('LENINTER', keyValue, irc)

  CALL ckoptt(1,'LENINTER',keyValue,srName,                       &
              'Length of interval for equ. of motion',irc,irCode, &
              ge=0d0,maxVal=1,result1=dtpi)

! Eq. of Motion - Polynomial Degree
! --------------------------------
  CALL readkeys('POLDEGR', keyValue, irc)

  CALL ckopti(1,'POLDEGR', keyValue, srName,                      &
              'Polynomial degree for equ. of motion',irc,irCode,  &
              maxVal=1,ge=1,result1=qq)

! Time Frame
! ----------
  CALL readkeys('TIMFRAME', keyValue, irc)

  CALL ckoptc(1,'TIMFRAME', keyValue, (/ 'GPS','UTC' /), srName,  &
              'Time frame of tabluar orbits', irc,irCode,         &
              maxVal=1,result1=indtim)

! Var. Eq. - Interval Length
! --------------------------
  CALL readkeys('VARLNINT', keyValue, irc)

  CALL ckoptt(1,'VARLNINT', keyValue, srName,                     &
              'Length of interval for variation equ.',irc,irCode, &
              ge=0d0,maxVal=1,result1=dtpiv)

! Var. Eq. - Polynomial Degree
! ---------------------------
  CALL readkeys('VARPOLDG', keyValue, irc)

  CALL ckopti(1,'VARPOLDG', keyValue, srName,                     &
              'Polynomial degree for variation equ.',irc,irCode,  &
              maxVal=1,ge=1,result1=qvar)

! Var. Eq. - Additional Sets
! --------------------------
  CALL readkeys('ADDVAR', keyValue, irc)

  CALL ckopti(1,'ADDVAR', keyValue, srName,                       &
              'Additional sets of variation equ.',irc,irCode,     &
              maxVal=1,ge=0,result1=addvar)

! Extended RPR Format
! -------------------
  CALL ckoptb(1,(/'RPRFRM'/),srName,                              &
              'Extended RPR format',irCode,                       &
              result1=rprfrm)

! Read satellite selection for pulses
! -----------------------------------
  stcopt%useStc = 0
  stcopt%prnstc = 0
  CALL GTFLNA(0,'RPROUT ',filnam,irc)
  IF (irc == 1 .AND. (genupd == 1 .OR. genupd == 2)) THEN

    CALL readKeys('STOCH_NUMBER', keyValue, irc)

    CALL ckoptc(1,'STOCH_NUMBER', keyValue, &
                (/'NONE   ','ALL    ','SATCRUX','MANUAL '/),srName,   &
                'Pulses: Satellite selection',irc,irCode,             &
                valList=(/0,99,-1,1/),maxVal=1,result1=stcopt%useStc)

  ENDIF

! Read satellite list for pulses
! ------------------------------
  IF (stcopt%usestc == 99) THEN
    stcopt%useStc =  1
    stcopt%prnstc = 99
  ELSE IF (stcopt%useStc == 1) THEN
    CALL readKeys('STOCH_MANUAL', keyValue, irc)

    IF (LEN_TRIM(keyValue(1)) == 0) THEN
      stcopt%useStc = 0
    ELSE
      CALL splarg(keyValue(1),argv)

      CALL ckopti(1,'STOCH_MANUAL',argv,srName,                        &
                 'List of satellites with pulses',irc,irCode,          &
                 maxVal=SIZE(argv),ge=1,le=999,                        &
                 nResult=stcopt%useStc,result2=stcopt%prnstc)
      DEALLOCATE(argv)
    ENDIF
  ENDIF

! Get the sampling of the pulses
! ------------------------------
  IF (stcopt%usestc > 0) THEN
    CALL readKeys('STOCH_INTER', keyValue, irc)

    CALL ckoptt(1,'STOCH_INTER', keyValue, srName,                     &
                'Sampling for pulses',irc,irCode,                      &
                maxVal=1,gt=0d0,result1=stcopt%interv)
    stcopt%interv = stcopt%interv * 60d0

!    CALL ckoptb(1,(/'STOCH_ALLIGN'/), srName, &
!                'Allign pulses with boundaries of integration',irCode, &
!                resultL=stcopt%algStc)
    stcopt%algStc = .TRUE.
  ENDIF


! Update: Use a priori or updated elements
! ----------------------------------------
  iupd=0
  IF (genupd == 3) THEN
    CALL readkeys('NEWOLD', keyValue, irc)

    CALL ckoptc(1,'NEWOLD', keyValue, (/'OLD','NEW'/), srName,    &
                'Use a priori or updated elements',irc,irCode,    &
                maxVal=1,valList=(/0,1/),result1=iupd)
  ENDIF

! Orbital Model
! -------------
  IF (genupd == 1 .OR. genupd == 2) THEN
!!    CALL readkeys('ORBMODFL', keyValue, irc)
!!
!!    CALL ckoptc(1,'ORBMODFL', keyValue, (/'O','A','B','C','D','E','F','G','?'/), &
!!                srName,'Orbit model flag',irc,irCode,                        &
!!                maxVal=1,valList=(/0,1,2,3,4,5,6,7,99/),result1=orbmod(1))
!!    modFlg=keyValue(1)
    modFlg='?'
    orbmod(1)=99
  ELSE
    CALL gtflna(1,'IMPORB ', filnam, irc)
    CALL opnfil(lfnloc, filnam, 'OLD', 'FORMATTED', 'READONLY',' ', ios)
    CALL opnerr(lfnerr, lfnloc, ios, filnam, 'RDORBI')
    DO
      READ (lfnloc, '(A)')STRG80
      IF (STRG80(1:2)== 'P0' .OR. STRG80(1:2) == 'D0' .OR. STRG80(1:2) == 'R0') THEN
        modflg = STRG80(80:80)
        EXIT
      END IF
    END DO
    CLOSE (lfnloc)
  END IF

  orbmod(1:10) = 0

  IF      (modFlg == 'O') THEN
    orbmod(1:10) =  (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
    modFlg = ' '

  ELSE IF (modFlg == 'A')  THEN
    orbmod(1:10) =  (/ 1, 1, 1, 1, 1, 0, 1, 0, 0, 0 /)

  ELSE IF (modFlg == 'B') THEN
    orbmod(1:10) =  (/ 2, 1, 1, 3, 1, 0, 1, 0, 0, 0 /)

  ELSE IF (modFlg == 'C') THEN
    orbmod(1:10) =  (/ 3, 1, 1, 3, 1, 1, 1, 0, 0, 0 /)

  ELSE IF (modFlg == 'D') THEN
!    orbmod(1:10) =  (/ 4, 2, 1, 3, 1, 1, 1, 0, 0, 0 /)
    orbmod(1:10) =  (/ 4, 2, 1, 3, 1, 0, 1, 0, 0, 0 /)

  ELSE IF (modFlg == 'E') THEN
!    orbmod(1:10) =  (/ 5, 3, 1, 3, 1, 1, 1, 0, 0, 0 /)
    orbmod(1:10) =  (/ 5, 3, 1, 3, 1, 0, 1, 0, 0, 0 /)

  ELSE IF (modFlg == 'F') THEN
!    orbmod(1:10) =  (/ 6, 4, 1, 3, 1, 1, 1, 0, 0, 0 /)
    orbmod(1:10) =  (/ 6, 4, 1, 3, 1, 0, 1, 0, 0, 0 /)

  ELSE IF (modFlg == 'G') THEN
    orbmod(1:10) =  (/ 7, 4, 1, 3, 1, 1, 1, 0, 0, 0 /)

!  ELSE IF (modFlg == 'L') THEN
!    orbmod(1:10) =  (/ 4, 2, 1, 3, 1, 1, 1, 0, 0, 0 /)

  ELSE IF (modFlg == '?') THEN
    orbmod(1:10) = (/99, 1, 1, 4, 1, 0, 1, 0, 0, 0 /)

  END IF

! Set system correctly according to option in panel
! -------------------------------------------------
  IF ( orbsys == 1) THEN
    orbmod(6) = 0
  ELSE IF ( orbsys == 2 ) THEN
    orbmod(6) = 1
  ELSE IF ( orbsys == 3 ) THEN
    orbmod(6) = 2
  END IF

!!! Mean pole
!!! ---------
!!  chrVal=''
!!  IF ( orbmod(4)==4) THEN
!!    chrVal(1:8)='IERS2010'
!!  ENDIF
!!  numVal=orbmod(4)*1.D0-1
!!  CALL setModKey(mod_orb_meaPol,chrVal,srName,numVal)

!!! Check if "DE200" is used for Model "B", "C", "L"
!!! ------------------------------------------------
!!  IF (orbmod(1) /= 99) THEN
!!    CALL gtflna(0, 'JPLEPH ', filnam, irc)
!!    IF      (orbmod(1) >= 2 .AND. IRC /= 0) THEN
!!      WRITE(lfnerr,*) ' *** SR RDORBI: planetary ephemeris not defined '
!!      irCode = irCode + 1
!!    ELSE IF (orbmod(1) <  2 .AND. IRC == 0) THEN
!!      WRITE(lfnerr,*) ' *** SR RDORBI: planetary ephemeris defined '
!!      irCode = irCode + 1
!!    ENDIF
!!  ENDIF

!!! Check if Ocean Tides are used for Model "B", "L"
!!! ------------------------------------------------
!!  IF (orbmod(1) /= 99) THEN
!!    CALL gtflna(0,'OTIDES ', filnam, irc)
!!    IF      (orbmod(1) >= 2 .AND. IRC /= 0) THEN
!!      WRITE(lfnerr,*) ' *** SR RDORBI: no ocean tides defined '
!!      irCode = irCode + 1
!!    ELSE IF (orbmod(1) <  2 .AND. IRC == 0) THEN
!!      WRITE(lfnerr,*) ' *** SR RDORBI: ocean tides defined '
!!      irCode = irCode + 1
!!    ENDIF
!!  ENDIF

! Arc(s) Definition
! -----------------
  ALLOCATE(tjbeg(mxcfil), stat=iac)
  CALL alcerr(iac, 'tjbeg', (/mxcfil/), 'rdorbi')
  ALLOCATE(tjend(mxcfil), stat=iac)
  CALL alcerr(iac, 'tjend', (/mxcfil/), 'rdorbi')
  ALLOCATE(indext(mxcfil), stat=iac)
  CALL alcerr(iac, 'indext', (/mxcfil/), 'rdorbi')

! Collect Start and End Time from Tabular Files
! ---------------------------------------------
  IF (genupd == 1) THEN
    CALL gtfile2('TABFIL ', 1, nfil, filist)
    CALL dimtst(1,1,2,'RDORBI','MAXFIL','TABULAR FILES',&
                'Parameter is defined in "ORBGEN.f".',nfil,mxcfil,irc)
    DO ifil = 1, nfil
      CALL gttabhed(filist(1,ifil),timint=timint)
      tjbeg(ifil) = timint%t(1)
      tjend(ifil) = timint%t(2)
    END DO

    CALL dordup(tjend, nfil, indext)
    tmax = tjend( indext(nfil) )
    CALL dordup(tjbeg, nfil, indext)
    tmin = tjbeg( indext(1) )

! Collect Start and End Time from Precise Files
! ---------------------------------------------
  ELSEIF (genupd == 2) THEN
    CALL gtfile2('PREFIL ', 2, nfil, filist)
    CALL dimtst(1,1,2,'RDORBI','MAXFIL','PRECISE FILES',&
                'Parameter is defined in "ORBGEN.f".',nfil,mxcfil,irc)
    DO ifil = 1, nfil
      CALL rdpreh(filist(1,ifil),-1,ifrmat,nsat,satnum,satwgt,tfirst,       &
                  nepo,dttab,title,datdes,coosys,orbtyp,agency,filtyp,      &
                  timsys,baspos,basclk)

    ! Correct for time system (first part: to GPS time)
    ! -------------------------------------------------
      timcor = 0.0D0
      gpsutc = dgpsut(tfirst)/86400.D0
      IF ( timsys == 'UTC' .OR. timsys == 'GLO' ) THEN
        timcor = gpsutc
      ELSEIF (timsys == 'TAI' ) THEN
        timcor = -19.D0/86400.D0
      END IF
    ! Output in UTC
    ! --------------
      IF ( indtim == 2 ) timcor = timcor - gpsutc
    ! Apply time correction
    ! ---------------------
      tfirst = tfirst + timcor

      tjbeg(ifil) = tfirst
      tjend(ifil) = tfirst+(nepo-1)*dttab/86400.d0
    END DO

    CALL dordup(tjend, nfil, indext)
    tmax = tjend( indext(nfil) )
    CALL dordup(tjbeg, nfil, indext)
    tmin = tjbeg( indext(1) )

! Read Start and End Time from ELE-File
! -------------------------------------
  ELSE
    timsys='GPS'

    CALL gtflna(1, 'IMPORB ', filnam, ios)
    CALL opnfil(lfnloc, filnam, 'OLD', ' ', 'READONLY', ' ', ios)
    CALL opnerr(lfnerr, lfnloc, ios, filnam, 'RDORBI')
    READ(LFNLOC, '(A80)') strg80
    DO
      READ(LFNLOC, '(A80)') strg80

      IF (strg80(1:7)=='TIMSYS:') timsys=strg80(9:11)

      IF (strg80(1:7)=='OTIDES:' .AND. strg80(40:47)=='XMIN    ') &
         READ(strg80(48:55), *) otdmin

      IF (strg80(1:21)=='ARC-NUMBER          =') EXIT
    ENDDO
    READ(strg80(56:75), '(F20.12)') tmin
    CLOSE(lfnloc)

  ! Correct time system according to panel settings
  ! -----------------------------------------------
    timcor = 0.0D0
    gpsutc = dgpsut(tmin)/86400.D0
    IF ( timsys == 'UTC' .OR. timsys == 'GLO' ) THEN
      timcor = gpsutc
    ELSEIF ( timsys == 'TAI' ) THEN
      timcor = -19.D0/86400.D0
    END IF
  ! Output in UTC
  ! --------------
    IF ( indtim == 2 ) timcor = timcor - gpsutc
  ! Apply time correction
  ! ---------------------
    tmin = tmin + timcor

    CALL readkeys('ARCINT', keyValue, irc)
    READ(keyValue(1),*,iostat=ios) interv
    tmax = tmin + interv
    IF (ios /= 0 .OR. interv == 0) THEN
      WRITE (lfnerr,*) ' *** SR RDORBI: invalid entry for length of time '  &
                        //'interval: ', TRIM(keyValue(1))
      irCode = irCode + 1
    END IF
  ENDIF

! Read tmin and tmax from input File
! ----------------------------------
  IF (genupd == 1) THEN
    DO iFil = 1, nFil
      tabfil(indext(ifil)) = filist(1,iFil)
    ENDDO
  ENDIF
  IF (genupd == 2) THEN
    DO iFil = 1, nFil
      prefil(1:2,indext(ifil)) = filist(1:2,iFil)
    ENDDO
  ENDIF

! Read tmin and tmax from input File
! ----------------------------------
  IF (genupd == 1 .OR. genupd == 2) THEN

    CALL gtTimWin(' ',(/'RADIO_1','RADIO_2'/),                 &
                  (/'SESSION_YEAR','SESSION_STRG'/),           &
                  (/'STADAT', 'STATIM', 'ENDDAT', 'ENDTIM' /), &
                  window)

    IF (window(1) == 0d0 .OR. window(2) == 1d20) THEN
      CALL seswin('SESSION_TABLE',tmin,tmax,                   &
                  timint%t(1),timint%t(2),help1,help2,irc)

      IF (irc == 1) THEN
        WRITE(lfnerr, '(/,A,/,16X,A,/)')   &
        ' *** SR RDORBI: Error reading the session table to get a time window',&
                        'covering the specified input files'
        CALL exitrc(2)
      ENDIF
      IF (irc == 2) THEN
        WRITE(lfnerr, '(/,A,A,/,16X,A,/)')                              &
        ' ### SR RDORBI: You session definition is ambiguous for the ', &
                        'arc definition', &
                        'Please, check the arc boundaries carefully!'
      ENDIF
      IF (window(1) ==  0d0) window(1) = timint%t(1)
      IF (window(2) == 1d20) window(2) = timint%t(2)
    END IF
!
    IF (window(1) /= 0d0) THEN
      IF (window(1)+0.5d0/86400D0<tmin) THEN
        CALL timst2(1,1,window(1),tstr1)
        CALL timst2(1,1,tmin     ,tstr2)
        WRITE(lfnerr,'(/,A,/,16X,A,2(/,16X,A,A),/)')                        &
        ' ### SR RDORBI: The program has to integrate backwards.',          &
                        'You may have to increase the number of iterations',&
                        'Start of data from input file: ',trim(tstr2),      &
                        'Specified start of the arc   : ',trim(tstr1)
      ENDIF
      tmin = window(1)
    ENDIF
!
    IF (window(2) /= 1d20) THEN
      IF ((window(2)-tmax)*24D0 > 1D0) THEN
        CALL timst2(1,1,window(2),tstr1)
        CALL timst2(1,1,tmax     ,tstr2)
        WRITE(lfnerr,'(/,A,2(/,16X,A,A),/)')                       &
        ' ### SR RDORBI: Your arc definition is an extrapolation.',        &
                        'End of data from input files: ',TRIM(tstr2),      &
                        'Specified end of the arc    : ',TRIM(tstr1)
      END IF
      tmax = window(2)
    END IF
  END IF

! Read the Number of Arcs
! -----------------------
  IF (genupd == 1 .OR. genupd == 2) THEN
    CALL readkeys('NUMARCS', keyValue, irc)

    CALL ckopti(1,'NUMARCS', keyValue, srName, &
                'Number of acrs',irc,irCode,   &
                maxVal=1,ge=1,le=mxcarc,result1=narc)

  ELSE
    narc = 1
  ENDIF

! Define Start and End of each Arc
! --------------------------------
  IF (narc /= 0) THEN
    dt = INT((tmax-tmin-1d0/86400d0)/(dtpi/24d0*narc)+1)*dtpi/24d0
    DO iarc = 1, narc
      tfl(1,iarc) = tmin + (iarc-1)*dt
      tfl(2,iarc) = tmin + iarc*dt
    END DO
  END IF

! Define the Starting TAB Files
! -----------------------------
  IF (genupd == 1 .OR. genupd == 2) THEN
    DO iarc = 1, narc
      arcfil(iarc)=1
      DO ifil = 1, nfil
        IF ( tjbeg(indext(ifil)) <= tfl(1,iarc) .AND.                       &
             tjend(indext(ifil)) >  tfl(1,iarc) ) THEN
          arcfil(iarc) = ifil
          EXIT
        END IF
      END DO
    END DO
  ELSE
    arcfil(1:narc) = 1
  END IF

  DEALLOCATE(filist,   stat=iac)
  DEALLOCATE(indext,   stat=iac)
  DEALLOCATE(tjend,    stat=iac)
  DEALLOCATE(tjbeg,    stat=iac)
  DEALLOCATE(keyValue, stat=iac)

  IF (irCode /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE rdorbi

END MODULE
