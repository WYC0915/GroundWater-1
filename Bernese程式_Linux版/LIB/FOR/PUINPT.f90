MODULE s_PUINPT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE puinpt(title,ierb1d,window,poltyp,iusrat,idouble,inutoff, &
                  subnam,nutnam)

! -------------------------------------------------------------------------
!
! Purpose:    This is a new version of the old subroutine PUINPT.f that
!             reads the input options of the program POLUPD
!
! Author:     C. Urschl
!
! Created:    16-Oct-2000
! Last mod.:  11-May-2011
!
! Changes:    27-Sep-2001  HU: Use check routines, print options
!             20-Nov-2002  PS: Use Name for Subdaily ERP Model
!                              Checkbox for Nutation Offsets
!             11-Feb-2003  PS: Use SR rdnutsub
!             23-Apr-2003  AJ: Nullify local pointers
!             19-May-2003  RD: Use SR gttimwin instead of readsess
!             20-May-2003  HU: Do not import maxarg, maxcoeff from d_nutmod
!             01-Nov-2003  HU: Additional output
!             18-May-2005  HU: irc removed, idouble added
!             11-May-2011  HB: Set model key 'prcMod' through d_model
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_model,  ONLY: setModKey, chrValLength, mod_orb_prcMod

  USE s_gttimwin
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_rdnutsub
  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=80)           :: title            ! title line
  INTEGER(i4b)                :: ierb1d           ! if input bulletin b
                                                  ! 1 = take 1day values
                                                  ! 5 = take 5 day values
  REAL(r8b), DIMENSION(2)     :: window           ! window for pole values
                                                  ! (time=mjd)
  INTEGER(i4b), DIMENSION(2)  :: poltyp           ! i = 1 nutation model
                                                  ! ...1 = no
                                                  ! ...2 = observed
                                                  ! ...3 = herring
                                                  ! i = 2 subdaily pole model
                                                  ! ...1 = no
                                                  ! ...2 = ray
  INTEGER(i4b)                :: iusrat           ! use rates to compute erp
                                                  ! values at beginning and
                                                  ! end of day (=1)
  INTEGER(i4b)                :: idouble          ! allow for double epochs
                                                  ! (only for iusrat=0)
  INTEGER(i4b)                :: inutoff          ! include nutation offsets
                                                  ! (=1)
  CHARACTER(LEN=16)           :: subnam           ! Subdaily Pole Model Name
  CHARACTER(LEN=16)           :: nutnam           ! Nutation Model Name

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=45)             :: srName= 'SR puinpt (PG polupd)'
  CHARACTER(LEN=chrValLength)   :: chrVal
  CHARACTER(LEN=8),PARAMETER    :: srNam= 'puinpt  '
  INTEGER(i4b)                  :: irc, irCode

  NULLIFY(keyValue)

  irCode = 0
  poltyp(1) = 0
  poltyp(2) = 0

! Read Title for Result Files
! ---------------------------
  CALL readKeys('TITLE', keyValue, irc)
  title = keyValue(1)

! Read Values of IERS Bulletin B
! ------------------------------
  CALL readKeys('NDAYS', keyValue, irc)
  CALL ckoptc(1,'NDAYS',keyValue, (/'1', '5'/), &
              srName, 'Number of values for Bulletin B',irc,irCode,  &
              valList=(/1,5/), maxVal=1, result1=ierb1d)

! Read Time Options
! -----------------
  CALL gttimwin('IWIN',(/'RADIO_1','RADIO_2'/),          &
                (/'SESSION_YEAR','SESSION_STRG'/),       &
                (/'STADAT','STATIM','ENDDAT','ENDTIM'/), &
                window)


! Read Model Names
! ----------------
  CALL rdnutsub(nutnam,subnam)
  chrVal = ' '
  chrVal(1:4)='BIAS'
  CALL setModKey(mod_orb_prcMod,chrVal,srNam,0.D0)

! Read Use of ERP Rates
! ---------------------
  CALL ckoptb(1,(/'USERATE'/),srName,'Use rates', irCode, result1=iusrat)


! Read Allow double epochs
! ------------------------
  CALL ckoptb(1,(/'DBLEPO'/),srName,'Allow double epochs', &
              irCode, result1=idouble)
  IF (iusrat==1) idouble=0

! Read Inclusion of Nutation Offsets
! ----------------------------------
  CALL ckoptb(1,(/'NUTOFF'/),srName,'Include Nutation Offsets', &
              irCode, result1=inutoff)

  IF (irCode /= 0) THEN
    WRITE(lfnerr,"(/,' *** SR PUINPT: Number of errors: ',I2)")irCode
    CALL exitrc(2)
  END IF


! Write options
! -------------
  WRITE(lfnprt,"(' OPTIONS', &
           &   /,' -------', &
           &  //,1X,79('-'), &
           &   /,' Default nutation model          :    ',A, &
           &   /,' Default subdaily model          :    ',A, &
           &   /,' Number of values for Bulletin B :',I5, &
           &   /,' Use ERP rates                   :',I5, &
           &   /,' Allow double epochs             :',I5, &
           &   /,' Include nutation offsets        :',I5, &
           &   /,1X,79('-'))") &
           nutnam,subnam,ierb1d,iusrat,idouble,inutoff

  DEALLOCATE(keyValue,stat=irc)

END SUBROUTINE puinpt

END MODULE
