MODULE s_RXGINP
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rxginp(opt,staCrux)

! -------------------------------------------------------------------------
!
! Purpose:    This is a new version of the old subroutine RXGINP.f that
!             reads the input options of the program RNXGRA
!
! Author:     C. Urschl
!
! Created:    15-Aug-2000
! Last mod.:  16-Feb-2004
!
! Changes:    20-Nov-2000  RD: Use p_rnxgra module,
!                              add new options for list of file
!             22-Oct-2001  RD: Add flag for using station info file
!             21-Dec-2001  HU: Use m_bern, other modules with ONLY
!             14-Aug-2002  RD: Tolerance for missing epochs
!             23-Apr-2003  RD: Nullify local pointers
!             09-Jul-2003  RD: Read staCrux here, handle flags
!             13-Oct-2003  RD: Do not read integer without iostat
!             16-Feb-2004  RD: New options: print cycle slip, min s/n-ratio
!                              Use ckopt-SR for reading the input file
!
! SRs used:   readkeys, ckoptb, ckoptc, ckopti, gtflna, readCrux,
!             gtstaflg, exitrc
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_stacrx, ONLY: t_staCrux
  USE p_rnxgra, ONLY: t_rnxgra_opt

  USE s_gtstaflg
  USE s_readcrux
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckopti
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
  ! input:

  ! output:
  TYPE(t_rnxgra_opt) :: opt     ! rnxgra-options
  TYPE(t_staCrux)    :: staCrux ! STAINFO-record for station renaming

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6),PARAMETER :: srName = 'rxginp'

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength)                        :: staFil
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  INTEGER(i4b)                                         :: meatyp
  INTEGER(i4b)                                         :: irc, ioerr

! Init variables
! --------------
  ioerr = 0

  NULLIFY(keyValue)

! Read Frequency
! --------------
  CALL readkeys('FREQ' , keyValue, irc)

  CALL ckoptc(1,'FREQ',keyValue,(/'L1  ','L2  ','BOTH'/),srName,      &
              'Frequency to be displayed',irc,ioerr,                  &
              maxVal=1,result1=opt%iopt)


! Read Measurement type
! ---------------------
  CALL readkeys('MEATYP' , keyValue, irc)

  CALL ckoptc(1,'MEATYP',keyValue,(/'PHASE','CODE '/),srName,         &
              'Measurement type to be displayed',irc,ioerr,           &
              maxVal=1,valList=(/1,-1/),result1=meatyp)

  opt%iOpt = meatyp * opt%iOpt


! Read tolerance for observations
! -------------------------------
  CALL readKeys('IOBTOL',keyValue,irc)

  CALL ckopti(1,'IOBTOL',keyValue,srName,'Observation tolerance',     &
              irc,ioerr,empty=0,ge=0,maxVal=1,result1=opt%iobtol)

! Get minimum s/n-ratio to be considered
! --------------------------------------
  CALL readKeys('MINSIG',keyValue,irc)

  CALL ckopti(1,'MINSIG',keyValue,srName,                             &
              'Minimum s/n-ratio to be considered',irc,ioerr,         &
              empty=0,ge=0,le=9,result1=opt%minsig)

! Display cycle slip epochs
! -------------------------
  opt%cycgra = 0
  IF (meatyp == 1)                                                    &
    CALL ckoptb(1,(/'CYCGRA'/),srName,                                &
                'Display cycle slip epochs',ioerr,                    &
                result1=opt%cycgra)

! Get the list of files?
! ----------------------
  CALL ckoptb(1,(/'GETLST'/),srName,                                  &
              'Get list of files',ioerr,                              &
              result1=opt%getlst)

  IF (opt%getlst==1) THEN

! Get the name of the list file
! -----------------------------
    CALL gtflna(0,'LSTFILE',opt%lstfil,irc)

! Get the name of the list bad file
! ---------------------------------
    CALL gtflna(0,'DELFILE',opt%delfil,irc)

! Get the max. number of stations in list
! ---------------------------------------
    CALL readkeys('MAXSTA',keyValue,irc)

    CALL ckopti(1,'MAXSTA',keyValue,srName,                          &
                'Maximum number of selected files',irc,ioerr,        &
                maxVal=1,ge=1,empty=0,result1=opt%maxsta)

! Get the min. number of observations in file
! -------------------------------------------
    CALL readkeys('MINOBS',keyValue,irc)

    CALL ckopti(1,'MINOBS',keyValue,srName,                          &
                'Minimum number of observations per file',irc,ioerr, &
                maxVal=1,ge=0,empty=0,result1=opt%minobs)

! Get the max. number of bad epochs allowed for list
! --------------------------------------------------
    CALL readkeys('MAXBAD',keyValue,irc)

    CALL ckopti(1,'MAXBAD',keyValue,srName,                          &
                'Maximum number of bad epoch per file',irc,ioerr,    &
                maxVal=1,ge=1,empty=0,result1=opt%maxbad)

! Get the max. number of obs. to define a bad epoch
! -------------------------------------------------
    opt%badobs = 0
    IF (opt%maxbad > 0) THEN
      CALL readkeys('BADOBS',keyValue,irc)

      CALL ckopti(1,'BADOBS',keyValue,srName,                        &
                  'Number of obs. in a bad epoch',irc,ioerr,         &
                  maxVal=1,ge=1,empty=0,result1=opt%badobs)
    ENDIF
  ENDIF


! Read staCrux
! ------------
  CALL gtflna(0,'STAINFO',staFil,irc)

  IF (irc == 0 .AND. LEN_TRIM(staFil) > 0) THEN
    CALL readCrux(staFil,staCrux)

    CALL gtstaflg('USEFLG',                                          &
                  (/ 'FLG001','      ','      ','      ','      '/), &
                  staCrux)

    CALL ckoptb(1,(/'STNMSTOP'/),srName,                             &
               'Stop if station name not found',ioerr,               &
               result1=opt%istops)
  ENDIF

  IF (ioerr /= 0)  CALL exitrc(2)

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

END SUBROUTINE  rxginp

END MODULE
