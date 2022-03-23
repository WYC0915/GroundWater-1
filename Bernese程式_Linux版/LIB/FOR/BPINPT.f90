MODULE s_BPINPT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE bpinpt(title,datdes,tfirst,tlast,dttab,ifrmat,iwrte,isys, &
                  coosys,timsys,orbtyp,agency,fitorb,fitclk,dopred,  &
                  baspos,basclk,titopt,clkinf)

! -------------------------------------------------------------------------
!
! Purpose:    This subroutine reads all input informations for the program
!             STDPRE from the input file
!
! Author:     C. Urschl
!
! Created:    14-Nov-2002
! Last mod.:  06-May-2011
!
! Changes:    07-Mar-2001 LM: Syntax correction (Win32 Problem)
!             18-Oct-2001 MM: New option (use SATCRUX file)
!             08-Nov-2002 HU: SP3-c implemented, check routines used
!             14-Nov-2002 HU: Initialize orbtyp and agency strings
!             06-Feb-2003 SS: "useCrx" parameter removed
!             23-Apr-2003 CU: Nullify local pointers
!             20-May-2003 RD: Use SR gttimwin instead of readsess
!             04-Aug-2003 HU: New option to exclude sat. for prediction
!             18-Jul-2006 AG: New option (titopt) for title line 4
!             18-Jul-2007 AG: Galileo system implemented
!             03-Aug-2009 DT: Data descriptor for SLR added
!             06-May-2011 HB: Add rdstdh to initialize model names
!             23-Jan-2012 UM: rename ifrmat keyword NEW by SP3
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_stdorb, ONLY: t_stdhead, init_stdHead

  USE s_ckoptr
  USE s_readkeys
  USE s_ckoptb
  USE s_ckoptc
  USE s_priwin
  USE s_ckoptl
  USE s_gttimwin
  USE s_rdstdh
  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=57), DIMENSION(4) :: title    ! TITLE FOR THE PRECISE ORBIT FILE
                                              ! (FOR THE NEW FORMAT ONLY)
  CHARACTER(LEN=5)                :: datdes   ! DATA DESCRIPTOR
                                              ! u ... phase zero diff.
                                              ! U ... code  zero diff.
                                              ! s ... phase single diff.
                                              ! S ... code  single diff.
                                              ! d ... phase double diff.
                                              ! D ... code  double diff.
                                              ! SLR.. range observations
  REAL(r8b)                       :: tfirst   ! MJD OF FIRST TIME OF PRECISE ORBIT
  REAL(r8b)                       :: tlast    ! MJD OF LAST  TIME OF PRECISE ORBIT
  REAL(r8b)                       :: dttab    ! TABULAR TIME INTERVAL FOR PRECISE
  INTEGER(i4b)                    :: ifrmat   ! FORMAT OF PRECISE ORBIT FILE
                                              ! 0...OLD FORMAT POSITIONS ONLY
                                              ! 1...OLD FORMAT POSITIONS AND VELOCITIES
                                              ! 2...SP3 FORMAT POSITIONS ONLY
                                              ! 3...SP3 FORMAT POSITIONS AND VELOCITIES
                                              ! 4...SP3C FORMAT POSITIONS ONLY
                                              ! 5...SP3C FORMAT POSITIONS AND
                                              ! VELOCITIES
  INTEGER(i4b),DIMENSION(2)       :: iwrte    ! =1: WRITE OPTIONAL E-RECORDS (SP3c)
  INTEGER(i4b)                    :: isys     ! SATELLITES OF PRECISE ORBIT FILE
                                              ! 0...ALL SATELLITES
                                              ! 1...GPS SATELLITES ONLY
                                              ! 2...GLONASS SATELLITES ONLY
  CHARACTER(LEN=3)                :: clkinf   ! CLK info
  CHARACTER(LEN=5)                :: coosys   ! COORDINATE SYSTEM FOR PRECISE ORBITS
  CHARACTER(LEN=3)                :: timsys   ! TIME SYSTEM, 'GPS?,'UTC' (SP3c)
  CHARACTER(LEN=3)                :: orbtyp   ! TYPE OF ORBITS
  CHARACTER(LEN=4)                :: agency   ! AGENCY
  REAL(r8b)                       :: fitorb   ! LENGTH OF FITTED ORBIT INTERVAL (hours, SP3c)
  REAL(r8b)                       :: fitclk   ! LENGTH OF FITTED CLOCK INTERVAL (hours, SP3c)
  REAL(r8b)                       :: dopred   ! MINIMUM LENGTH OF ORBIT FIT
                                              ! INTERVAL FOR PREDICTION (hours, SP3c)
  REAL(r8b)                       :: baspos   ! BASE FOR POS/VEL ACCURACY CODES (SP3c)
  REAL(r8b)                       :: basclk   ! BASE FOR CLK/CLKRATE ACCURACY CODES (SP3c)

  LOGICAL                         :: titopt   ! WRITE MODEL IDENTIFIER IN TITLE LINE 4

! Local Variables
! ---------------
  TYPE(t_stdhead)                                          :: stdHead
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER     :: keyValue
  CHARACTER(LEN=fileNameLength)                            :: filnam
  INTEGER(i4b)                                             :: irc
  INTEGER(i4b)                                             :: irCode, ivelo
  REAL(r8b), DIMENSION(2)                                  :: window
  CHARACTER(LEN=6)                                         :: typdat, typdif

  irCode = 0
  NULLIFY(keyValue)
  CALL init_stdHead(stdHead)

! Read Satellites of Precise Orbit Files
! --------------------------------------
  CALL readkeys('SSYSTEM', keyValue, irc)
  CALL ckoptc(1,'SSYSTEM', keyValue,(/'ALL    ','GPS    ','GLONASS','GALILEO'/),    &
              'sr bpinpt', 'Satellite system', irc, irCode,               &
              valList=(/0,1,2,3/), maxVal=1, result1=isys)

! Read Format of Precise Orbit Files
! ----------------------------------
  CALL readkeys('FORMAT', keyValue, irc)
  CALL ckoptc(1,'FORMAT', keyValue,(/'OLD ','SP3 ','SP3C'/), 'sr bpinpt', &
              'Format', irc, irCode, valList=(/0,2,4/), maxVal=1,         &
              result1=ifrmat)

  CALL ckoptb(1, (/'VELOCITIES'/),'sr bpinpt', 'Write velocities', &
                irCode, result1 = ivelo)
  ifrmat=ifrmat+ivelo
  fitorb=99999d0
  fitclk=99999d0
  dopred=0d0

  IF (ifrmat == 2 .OR. ifrmat == 3 .OR. ifrmat == 4 .OR. ifrmat == 5) THEN

! Read Title for the Precise Orbit File (new)
! -------------------------------------------
    CALL readkeys('TITLE1', keyValue, irc)
    CALL ckoptl(0,'TITLE1', keyValue, 'sr bpinpt', 'Title 1', irc, irCode, &
                maxLength=LEN(title(1)), empty=' ', maxval=1, result1=title(1))
    CALL readkeys('TITLE2', keyValue, irc)
    CALL ckoptl(0,'TITLE2', keyValue, 'sr bpinpt', 'Title 2', irc, irCode, &
                maxLength=LEN(title(2)), empty=' ', maxval=1, result1=title(2))
    CALL readkeys('TITLE3', keyValue, irc)
    CALL ckoptl(0,'TITLE3', keyValue, 'sr bpinpt', 'Title 3', irc, irCode, &
                maxLength=LEN(title(3)), empty=' ', maxval=1, result1=title(3))

    CALL ckoptb(1, (/'TITOPT4'/),'sr bpinpt', 'Model identifier', &
                irCode, resultL = titopt)

    IF (titopt) THEN
      TITLE(4)=''
    ELSE
      CALL readkeys('TITLE4', keyValue, irc)
      CALL ckoptl(0,'TITLE4', keyValue, 'sr bpinpt', 'Title 4', irc, irCode, &
                maxLength=LEN(title(4)), empty=' ', maxval=1, result1=title(4))
    ENDIF

! Read Data Description (new)
! ---------------------------
    CALL readkeys('DATTYP' , keyValue, irc)
    CALL ckoptl(0,'DATTYP', keyValue, 'sr bpinpt', 'Data type', irc, irCode, &
                maxLength=LEN(typdat), maxval=1, result1=typdat)
    CALL readkeys('DATDIF' , keyValue, irc)
    CALL ckoptl(0,'DATDIF', keyValue, 'sr bpinpt', 'Difference type', irc,   &
                irCode, maxLength=LEN(typdif), maxval=1, result1=typdif)

    datdes = ''

    IF (typdat == 'PHASE'.AND.typdif == 'ZERO') THEN
      datdes = 'u'
    ELSE IF (typdat == 'CODE'.AND.typdif == 'ZERO') THEN
      datdes = 'U'
    ELSE IF (typdat == 'PHASE'.AND.typdif == 'SINGLE') THEN
      datdes = 's'
    ELSE IF (typdat == 'CODE'.AND.typdif == 'SINGLE') THEN
      datdes = 'S'
    ELSE IF (typdat == 'PHASE'.AND.typdif == 'DOUBLE') THEN
      datdes = 'd'
    ELSE IF (typdat == 'CODE'.AND.typdif == 'DOUBLE') THEN
      datdes = 'D'
    ELSE IF (typdat == 'BOTH'.AND.typdif == 'ZERO') THEN
      datdes = 'u+U'
    ELSE IF (typdat == 'BOTH'.AND.typdif == 'SINGLE') THEN
      datdes = 's+S'
    ELSE IF (typdat == 'BOTH'.AND.typdif == 'DOUBLE') THEN
      datdes = 'd+D'
    ELSE IF (typdat == 'RANGE') THEN
      datdes = 'SLR'
    ELSE
      WRITE(lfnerr,*) ' *** SR BPINPT: invalid entry for data description'
      irCode = irCode + 1
    END IF

! Read CLK info
! -------------
    CALL readkeys('CLKINFO', keyValue, irc)
    CALL ckoptl(0,'CLKINFO', keyValue, 'sr bpinpt', 'CLK info', irc,     &
                irCode, maxLength=LEN(clkinf), maxval=1, empty='   ',    &
                result1=clkinf)

! Read Coordinate System for Precise Orbit (new)
! ----------------------------------------------
    CALL readkeys('CSYSTEM', keyValue, irc)
    CALL ckoptl(0,'CSYSTEN', keyValue, 'sr bpinpt', 'Orbit system', irc,   &
                irCode, maxLength=LEN(coosys), maxval=1, result1=coosys)

! Read Type of Orbits (new)
! -------------------------
    CALL readkeys('ORBTYPN', keyValue, irc)
    CALL ckoptl(0,'ORBTYPN', keyValue, 'sr bpinpt', 'Orbit type', irc,   &
                irCode, maxLength=LEN(orbtyp), maxval=1, empty='   ',    &
                result1=orbtyp)

! Read Agency (new)
! -----------------
    CALL readkeys('AGENCYN', keyValue, irc)
    CALL ckoptl(0,'AGENCYN', keyValue, 'sr bpinpt', 'Agency', irc,   &
                irCode, maxLength=LEN(agency), maxval=1, empty='    ', &
                result1=agency)

! SP3-c
! -----
    IF (ifrmat == 4 .OR. ifrmat == 5) THEN

! Time System
! -----------
      CALL readkeys('TIMESYS', keyValue, irc)
      CALL ckoptl(1,'TIMESYS', keyValue, 'sr bpinpt', 'Time system', irc,   &
                  irCode, maxLength=LEN(timsys), maxval=1, result1=timsys)

! Length of fit interval
! ----------------------
      CALL readkeys('FITORB', keyValue, irc)
      CALL ckoptr(1,'FITORB', keyValue, 'sr bpinpt','Fit interval for orbit',&
                  irc, irCode, maxval=1, result1=fitorb)

      CALL readkeys('FITCLK', keyValue, irc)
      CALL ckoptr(1,'FITCLK', keyValue, 'sr bpinpt','Fit interval for clocks',&
                  irc, irCode, maxval=1, result1=fitclk)

      CALL readkeys('NOPRED', keyValue, irc)
      CALL ckoptr(1,'NOPRED', keyValue, 'sr bpinpt','Minimum fit for prediction',&
                  irc, irCode, maxval=1, result1=dopred)

! Write optional E-records
! ------------------------
!     CALL ckoptb(1,(/'WRITEE'/),'sr bpinpt',    &
!                   'Write optional E-records',irCode,resultL=useCor)
    END IF

  ELSE IF (ifrmat == 0 .OR. ifrmat == 1) THEN

! Read Coordinate System for Precise Orbit (old)
! ----------------------------------------------
    CALL readkeys('RSYSTEM', keyValue, irc)
    CALL ckoptl(0,'RSYSTEN', keyValue, 'sr bpinpt', 'Orbit system', irc,   &
                irCode, maxLength=LEN(coosys), maxval=1, result1=coosys)

! Read Type of Orbits (old)
! -------------------------
    CALL readkeys('ORBTYPO', keyValue, irc)
    CALL ckoptl(0,'ORBTYPO', keyValue, 'sr bpinpt', 'Orbit type', irc,   &
                irCode, maxLength=LEN(orbtyp), maxval=1, result1=orbtyp)

! Read Agency (old)
! -----------------
    CALL readkeys('AGENCYO', keyValue, irc)
    CALL ckoptl(0,'AGENCYO', keyValue, 'sr bpinpt', 'Agency', irc,   &
                irCode, maxLength=LEN(agency), maxval=1, result1=agency)

  ELSE
    WRITE(lfnerr,*) ' *** SR BPINPT: invalid entry for format type: ',        &
                      keyValue(1)
    irCode = irCode + 1

  END IF

! Read Time Options
! -----------------
  CALL gttimwin('USEWIN',(/'RADIO_1','RADIO_2'/),        &
                (/'SESSION_YEAR','SESSION_STRG'/),       &
                (/'STADAT','STATIM','ENDDAT','ENDTIM'/), &
                window)

  CALL priwin(1,window)

  tfirst = window(1)
  tlast  = window(2)

  CALL readkeys('TABINT', keyValue, irc)
  CALL ckoptr(1,'TABINT', keyValue, 'sr bpinpt', 'Tabular interval', irc,   &
                irCode, gt=0D0, maxval=1, result1=dttab)

! Get model information from standard orbit file header
! -----------------------------------------------------
  filNam = ' '
  CALL rdstdh(filNam,stdHead,irc)

! Base for position/velocity and clock/clock rate accuracy codes (SP3c)
! ---------------------------------------------------------------------
  baspos=1.25D0
  basclk=1.025D0
  iwrte =(/0,0/)

  DEALLOCATE(keyValue,stat=irc)

END SUBROUTINE bpinpt

END MODULE
