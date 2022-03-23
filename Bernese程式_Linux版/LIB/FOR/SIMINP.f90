MODULE s_siminp
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE siminp(opt)

! -------------------------------------------------------------------------
! Purpose:    Read option input file for program STAMERGE
!
! Author:     A. Steinbach
!
! Created:    07-Nov-2007
! Last mod.:  07-Nov-2007
!
! Changes:    06-Jan-2011 SL: use m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b,keyValueLength,fileNameLength,lfnPrt
  USE p_stamrg, ONLY: t_stamrg_opt

  USE s_readKeys
  USE s_ckoptc
  USE s_ckoptb
  USE s_exitrc
  USE s_gtflna
  USE s_readstsg
  USE s_gttimwin
  USE s_priwin

  IMPLICIT NONE
!
! List of Parameters
! ------------------
! input/output:
  TYPE(t_stamrg_opt) :: opt ! Record of input option

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6),PARAMETER                  :: srName = 'SIMINP'
  CHARACTER(LEN=7),DIMENSION(4),PARAMETER     :: action = &
                    (/'NONE   ','COMPARE','MERGE  ','DELETE '/)

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
    DIMENSION(:),POINTER              :: keyValue
  CHARACTER(LEN=fileNameLength)       :: fixFil
  INTEGER(i4b)                        :: ios            ! IO status
  INTEGER(i4b)                        :: irc            ! return code
  INTEGER(i4b)                        :: irCode         ! return code
  INTEGER(i4b)                        :: i,j

! Init local variables
! --------------------
  irCode=0

  NULLIFY(keyValue)

! Get filenames
! -------------
  CALL gtflna(1,'STAMTR',opt%mtrFil,irc)
  CALL gtflna(0,'STASEC',opt%secFil,irc)
  CALL gtflna(0,'STARESM',opt%resFil,irc)

! Read the action for the master file
! -----------------------------------
  CALL ckoptb(1,(/'SORTMA'/),srName,'Sort master file',  &
              irCode,result1=opt%csort)

! Read the action for section 3 entries
! -------------------------------------
  CALL ckoptb(1,(/'CONSID'/),srName,'Consider entries with _ in station name',  &
              irCode,result1=opt%cconsid)

! Read the actions for the sections
! ---------------------------------
  IF(LEN_TRIM(opt%secFil)>0) THEN
    CALL readKeys('COMBO01',keyValue,irc)
    CALL ckoptc(1,'COMBO01',keyValue,                                  &
                (/'NONE   ','COMPARE','MERGE  ','DELETE '/),srName,      &
                'Action for section 001',irc,irCode,                   &
                maxVal=1,valList=(/0,1,2,3/),result1=opt%ctype01)
  ELSE
    opt%ctype01 = 0
  ENDIF

  IF(LEN_TRIM(opt%secFil)>0) THEN
    CALL readKeys('COMBO02',keyValue,irc)
    CALL ckoptc(1,'COMBO02',keyValue,                                 &
                (/'NONE   ','COMPARE','MERGE  ','DELETE '/),srName,      &
                'Action for section 002',irc,irCode,                   &
                maxVal=1,valList=(/0,1,2,3/),result1=opt%ctype02)
  ELSE
    opt%ctype02 = 0
  ENDIF

  IF(LEN_TRIM(opt%secFil)>0) THEN
    CALL readKeys('COMBO03',keyValue,irc)
    CALL ckoptc(1,'COMBO03',keyValue,                                  &
                (/'NONE   ','COMPARE','MERGE  ','DELETE '/),srName,      &
                'Action for section 003',irc,irCode,                   &
                maxVal=1,valList=(/0,1,2,3/),result1=opt%ctype03)
  ELSE
    opt%ctype03 = 0
  ENDIF

  IF(LEN_TRIM(opt%secFil)>0) THEN
    CALL readKeys('COMBO04',keyValue,irc)
    CALL ckoptc(1,'COMBO04',keyValue,                                  &
                (/'NONE   ','COMPARE','MERGE  ','DELETE '/),srName,      &
                'Action for section 004',irc,irCode,                   &
                maxVal=1,valList=(/0,1,2,3/),result1=opt%ctype04)
  ELSE
    opt%ctype04 = 0
  ENDIF

  IF(LEN_TRIM(opt%secFil)>0) THEN
    CALL readKeys('COMBO05',keyValue,irc)
    CALL ckoptc(1,'COMBO05',keyValue,                                  &
                (/'NONE   ','COMPARE','MERGE  ','DELETE '/),srName,      &
                'Action for section 005',irc,irCode,                   &
                maxVal=1,valList=(/0,1,2,3/),result1=opt%ctype05)
  ELSE
    opt%ctype05 = 0
  ENDIF

  CALL ckoptb(1,(/'REDANT'/),srName,'Reduce antenna numbers in section 002', &
              irCode,result1=opt%crant)

  CALL ckoptb(1,(/'REDREC'/),srName,'Reduce receiver numbers in section 002', &
              irCode,result1=opt%crrec)

! Get the list of selected stations
! ---------------------------------
  CALL gtflna(0,'FIXSTA',fixFil,irc)
  IF(irc == 0) THEN
    CALL readstsg(fixFil,0,opt%staList)
  ELSE
    opt%staList%nSta = 0
  ENDIF

! Get the selected time window
! ----------------------------
  CALL gttimwin('WINDOW',(/'RADIO_2','RADIO_3'/),            &
                (/'SESSION_YEAR','SESSION_STRG'/),                &
                (/'STADAT','STATIM','ENDDAT','ENDTIM'/),          &
                opt%timwin)

! Exit in case of an error
! ------------------------
  IF(irCode /= 0) CALL exitrc(2)

! Report input options
! ---------------------
! Action options settings
! -----------------------
  IF(LEN_TRIM(opt%secFil)>0) THEN
    WRITE(LFNPRT,'(/,2(A,/),/,A,/,A,/,A,/,A,/,A,/,/)')                       &
    ' ACTION OPTIONS:',                                                      &
    ' ------------------------------------------------------------------' // &
    '------------------------------------------------------------------',    &
    ' Section 001                           :  '//action(opt%ctype01+1),     &
    ' Section 002                           :  '//action(opt%ctype02+1),     &
    ' Section 003                           :  '//action(opt%ctype03+1),     &
    ' Section 004                           :  '//action(opt%ctype04+1),     &
    ' Section 005                           :  '//action(opt%ctype05+1)
  ELSE
    WRITE(LFNPRT,'(/,2(A,/),/,A,/,/)') &
    ' ACTION OPTIONS:',                &
    ' ---------------',                &
    ' No options set  '
  ENDIF

! Content of selection file
! -------------------------
  IF(opt%staList%nSta > 0) THEN
    WRITE(LFNPRT,'(/,2(A,/),/,A,I3,/)') &
    ' STATION SELECTION:',              &
    ' -----------------',               &
    ' Number of stations listed in station selection file  :  ',opt%staList%nSta
    WRITE(LFNPRT,'(A,/,A,/,A)')                                              &
    ' ------------------------------------------------------------------' // &
    '------------------------------------------------------------------',    &
    ' Stations in station selection file',                                   &
    ' ------------------------------------------------------------------' // &
    '------------------------------------------------------------------'
    i=1
    DO WHILE (i <= opt%staList%nSta)
      j = i+5
      IF (j > opt%staList%nSta) j = opt%staList%nSta
      WRITE(LFNPRT,'(1X,5(A16,6X),A16)') opt%staList%stanam(i:j)
      i = j+1
    END DO
    WRITE(LFNPRT,'(A,//)')                                                   &
    ' ------------------------------------------------------------------' // &
    '------------------------------------------------------------------'
  ELSE
    WRITE(LFNPRT,'(/,2(A,/),/)')                      &
    ' STATION SELECTION:                       none', &
    ' -----------------'
  END IF

! Time window settings
! --------------------
  CALL priwin(1,opt%timwin)

  DEALLOCATE(keyValue,stat=irc)

  RETURN

END SUBROUTINE siminp

END MODULE
