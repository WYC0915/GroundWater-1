MODULE s_RCINPT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rcinpt(opt, irCode)

! -------------------------------------------------------------------------
! Purpose:    Reads input file for RESCHK
!
! Author:     R. Dach
!
! Created:    04-Dec-2000
!
! Changes:    14-Aug-2001 RD: Separate directories for all data types
!             21-Dec-2001 HU: Use m_bern, other modules with ONLY
!             25-Apr-2002 MM: Use pritit, introduce short summary
!             12-Mar-2003 RD: Remove name of stacrx file
!                             Replace variables in obs. file name
!             23-Apr-2003 RD: Nullify local pointers
!             03-Dec-2003 RD: INP-file review, use ckopt-routines
!             30-Aug-2005 MM: Minimum number of observations added
!             14-Nov-2011 SL: m_bern w/ ONLY, no PRITIT call (moved to program)
!             10-Feb-2012 SL: keyword ABBPAN changed to ABBREV
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,    ONLY: i4b, keyValueLength, lfnPrt
  USE p_reschk,  ONLY: t_reschk_opt

  USE s_ckoptr
  USE s_prflna
!!!  USE s_pritit
  USE s_readkeys
  USE s_rplenvar
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckopti
  USE s_gtflna
  USE s_ckoptl
  IMPLICIT NONE

! Variables from parameter list
! -----------------------------
! input

! output
  TYPE(t_reschk_opt)             :: opt        ! program input options
  INTEGER(i4b)                   :: irCode     ! Check the input options


! Used functions
! --------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'rcinpt'
!
! Local variables
! ---------------
  CHARACTER(LEN=keyValueLength) , DIMENSION(:)  , POINTER  :: keyValue

  INTEGER(i4b)                                             :: irc


! Initialization
! --------------
  irCode=0

  NULLIFY(keyValue)

! General options
! ---------------
! Title:
  CALL readkeys('TITLE',keyValue,irc)

  CALL ckoptl(0,'TITLE',keyValue,srName,   &
              'Title line',irc,irCode,     &
              empty=' ',maxVal=1,result1=opt%title)

! Year of session in RESRMS.SUM
! -----------------------------
  CALL readkeys('YR4_INFO',keyValue,irc)

  CALL ckopti(1,'YR4_INFO',keyValue,srName,                         &
              'Year for session in residual summary',irc,irCode,    &
              maxVal=1,result1=opt%iyear4)

! Content of program output
! -------------------------
  CALL readkeys('SUMMARY',keyValue,irc)

  CALL ckoptc(1,'SUMMARY',keyValue,                                 &
              (/'DETAILS','SUMMARY','FULL   ','SUMM   '/),srName,   &
              'Content of program output',irc,irCode,               &
              maxVal=1,valList=(/1,0,1,0/),result1=opt%summary)

! Content of program output
! -------------------------
  CALL readkeys('DIFRESI',keyValue,irc)

  CALL ckoptc(1,'DIFRESI',keyValue,(/'ZERO  ','SINGLE'/),srName,    &
              'Difference level of residual summary',irc,irCode,    &
              maxVal=1,valList=(/0,1/),result1=opt%nDiff)

! Bad station detection
! ---------------------
  CALL ckoptb(1,(/'BADSTA'/),srName,                                &
              'Detect bad stations',irCode,                         &
              resultl=opt%badsta)

! Bad station detection
! ---------------------
  CALL ckoptb(1,(/'MANSAT'/),srName,                                &
              'Detect misbehaving satellites',irCode,               &
              resultl=opt%mansat)

! Run program in debugging mode
! -----------------------------
  CALL ckoptb(1,(/'TSTMODE'/),srName,                               &
              'Debugging mode',irCode,                              &
              resultl=opt%tstMode)

! Read file names
! ---------------
  CALL gtflna(1,'SESSION_TABLE',opt%sesFile,irc)
  CALL gtflna(0,'ABBREV',       opt%abbpan, irc)
  CALL gtflna(0,'SUMOUT',       opt%sumout, irc)
  CALL gtflna(1,'RESRMS',       opt%resrms, irc)

  IF(opt%mansat) THEN
    CALL gtflna(0,'SATCRUX',opt%satcrux,irc)
    CALL gtflna(0,'ASPLIT', opt%asplit, irc)
    CALL gtflna(1,'CODXTR', opt%codxtr, irc)
  ELSE
    opt%satcrux = ' '
    opt%asplit  = ' '
    opt%codxtr  = ' '
  ENDIF

  IF (opt%badsta) THEN
    CALL gtflna(0,'DELSTA', opt%delsta, irc)
  ELSE
    opt%delsta  = ' '
  ENDIF

! Path and extensions for DELSTA
! ------------------------------
  IF (LEN_TRIM(opt%delsta) > 0) THEN
    CALL readkeys('DIR_CZH',keyValue,irc)
    irCode=irCode+irc
    opt%dir_czh=keyValue(1)
    CALL rplEnVar(1,opt%dir_czh)

    CALL readkeys('DIR_CZO',keyValue,irc)
    irCode=irCode+irc
    opt%dir_czo=keyValue(1)
    CALL rplEnVar(1,opt%dir_czo)

    CALL readkeys('DIR_PZH',keyValue,irc)
    irCode=irCode+irc
    opt%dir_pzh=keyValue(1)
    CALL rplEnVar(1,opt%dir_pzh)

    CALL readkeys('DIR_PZO',keyValue,irc)
    irCode=irCode+irc
    opt%dir_pzo=keyValue(1)
    CALL rplEnVar(1,opt%dir_pzo)

    CALL readkeys('EXT_CZH',keyValue,irc)
    irCode=irCode+irc
    opt%ext_czh=keyValue(1)

    CALL readkeys('EXT_CZO',keyValue,irc)
    irCode=irCode+irc
    opt%ext_czo=keyValue(1)

    CALL readkeys('EXT_PZH',keyValue,irc)
    irCode=irCode+irc
    opt%ext_pzh=keyValue(1)

    CALL readkeys('EXT_PZO',keyValue,irc)
    irCode=irCode+irc
    opt%ext_pzo=keyValue(1)

  ELSE
    opt%DIR_CZH=''
    opt%DIR_CZO=''
    opt%DIR_PZH=''
    opt%DIR_PZO=''
    opt%EXT_CZH=''
    opt%EXT_CZO=''
    opt%EXT_PZH=''
    opt%EXT_PZO=''
  ENDIF

! Bad station detection (0-Diff case)
! -----------------------------------
  IF (opt%badsta .AND. opt%nDiff == 0) THEN
    opt%maxsta1 = 0
    opt%maxrms1 = 0d0

! Maximum number of stations to be deleted
! ----------------------------------------
    CALL readkeys('MAXSTA',keyValue,irc)

    CALL ckopti(1,'MAXSTA',keyValue,srName,                            &
                'Maximum number of bad stations to delete',irc,irCode, &
                maxVal=1,ge=1,result1=opt%maxsta0)

! Maximum total RMS in RESRMS.SUM
! -------------------------------
    CALL readkeys('MAXRMS',keyValue,irc)

    CALL ckoptr(1,'MAXRMS',keyValue,srName,                            &
                'Maximum total RMS allowed',irc,irCode,                &
                maxVal=1,gt=0d0,result1=opt%maxrms0)

! Bad solution indicator
! ----------------------
    CALL readkeys('BADSOL',keyValue,irc)

    CALL ckoptr(1,'BADSOL',keyValue,srName,                            &
                'RMS limit when a bad solution assumed',irc,irCode,    &
                maxVal=1,gt=0d0,result1=opt%badsol0)

  ENDIF ! Bad station detection (0-Diff case)

! Bad station detection (1-Diff case)
! -----------------------------------
  IF (opt%badsta .AND. opt%nDiff == 1) THEN
    opt%maxsta0 = 0
    opt%maxrms0 = 0d0
    opt%badsol0 = 0d0

! Maximum number of stations to be deleted
! ----------------------------------------
    CALL readkeys('MAXSTA',keyValue,irc)

    CALL ckopti(1,'MAXSTA',keyValue,srName,                            &
                'Maximum number of bad stations to delete',irc,irCode, &
                maxVal=1,ge=1,result1=opt%maxsta1)

! Maximum total RMS in RESRMS.SUM
! -------------------------------
    CALL readkeys('MAXRMS',keyValue,irc)

    CALL ckoptr(1,'MAXRMS',keyValue,srName,                            &
                'Maximum total RMS allowed',irc,irCode,                &
                maxVal=1,gt=0d0,result1=opt%maxrms1)

  ENDIF ! Bad station detection (1-Diff case)


! Satellite Maneuvers
! -------------------

! Arc split RMS
! -------------
  opt%maxorb = 0d0
  IF (opt%mansat .AND. LEN_TRIM(opt%asplit) > 0) THEN
    CALL readkeys('MAXORB',keyValue,irc)

    CALL ckoptr(1,'MAXORB',keyValue,srName,                            &
                'Maximum allowed arc split RMS',irc,irCode,            &
                maxVal=1,gt=0d0,result1=opt%maxorb)
  ENDIF

! RMS ratio (maneuver case)
! -------------------------
  IF (opt%mansat) THEN
    CALL readkeys('MAXMRMS',keyValue,irc)

    CALL ckoptr(1,'MAXMRMS',keyValue,srName,                             &
                'Maximum ratio of satellite RMS to total RMS',irc,irCode,&
                maxVal=1,gt=0d0,result1=opt%maxmrms)
  ENDIF

! Bad satellite detection
! -----------------------

! Maximum RMS to indicate a bad solution
! --------------------------------------
  IF (opt%mansat) THEN
    CALL readkeys('MAXARMS',keyValue,irc)

    CALL ckoptr(1,'MAXARMS',keyValue,srName,                            &
                'RMS threshold to remove only one satellite',irc,irCode,&
                maxVal=1,gt=0d0,result1=opt%maxarms)

! RMS ration (normal case)
! ------------------------
    CALL readkeys('MAXRRMS',keyValue,irc)

    CALL ckoptr(1,'MAXRRMS',keyValue,srName,                             &
                'Maximum ratio of satellite RMS to total RMS',irc,irCode,&
                maxVal=1,gt=0d0,result1=opt%maxrrms)

! Maximum percentage of deleted data
! ----------------------------------
    CALL readkeys('MAXDEL',keyValue,irc)

    CALL ckoptr(1,'MAXDEL',keyValue,srName,                              &
                'Maximum percentage of deleted data',irc,irCode,         &
                maxVal=1,gt=0d0,result1=opt%maxdel)

! Minimum number of observations required
! ---------------------------------------
    CALL readkeys('MINOBSG',keyValue,irc)

    CALL ckoptr(1,'MINOBSG',keyValue,srName,                               &
                'Minimum # of observations required (GPS)',irc,irCode,     &
                ge=0d0,empty=0d0,result1=opt%minObsG)

    CALL readkeys('MINOBSR',keyValue,irc)

    CALL ckoptr(1,'MINOBSR',keyValue,srName,                               &
                'Minimum # of observations required (GLONASS)',irc,irCode, &
                ge=0d0,empty=0d0,result1=opt%minObsR)

! Remove only one satellite
! -------------------------
    CALL ckoptb(1,(/'DEL1SAT'/),srName,'Only one bad satellite',irCode, &
                resultL=opt%del1sat)


  ENDIF ! Bad satellite detection

! Stop program in the case of an input error
! ------------------------------------------
  DEALLOCATE(keyValue,stat=irc)
  IF (irCode /= 0) CALL exitrc(2)

! Prepare the protocoll
! ---------------------
!!!  CALL pritit('RESCHK','Check residuals for bad stations and/or satellites')
  CALL prflna

  WRITE(lfnprt,'(/A,A,/)') 'RESRMS SUMMARY FILE NAME:  ', opt%RESRMS
  IF (opt%MANSAT .AND. opt%summary == 1) &
    WRITE(lfnprt,'(A,/)') '  FILE WAS CHECKED FOR BAD/MANEUVER SATELLITES'
  IF (opt%BADSTA .AND. opt%summary == 1) &
    WRITE(lfnprt,'(A,/)') '  FILE WAS CHECKED FOR BAD STATIONS'
  IF (opt%TSTMODE) &
    WRITE(lfnprt,'(5(/,10X,A),/)')                        &
    '**************************************************', &
    '***                                            ***', &
    '***    ONLY THE FIRST ITERATION IS ANALYSED    ***', &
    '***                                            ***', &
    '**************************************************'
!
  RETURN
  END SUBROUTINE rcinpt


END MODULE
