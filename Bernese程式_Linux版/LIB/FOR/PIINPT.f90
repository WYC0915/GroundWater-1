MODULE s_PIINPT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE piinpt(titgen,icont,isubap,idouble,nutnam,subnam,iline,sampl)

! -------------------------------------------------------------------------
!
! Purpose:    This is a new version of the old subroutine PIINPT.f that
!             reads the input options of the program POLINT
!
! Author:     P. Steigenberger
!
! Created:    05-Mar-2003
! Last mod.:  11-May-2011
!
! Changes:    13-Jan-2004 PS: Added iline
!             17-May-2005 HU: Read sampling
!             20-Jul-2005 HB: Nullify keyValue
!             11-May-2011 HB: Set model key 'prcMod' through d_model
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_model,  ONLY: setModKey, chrValLength, mod_orb_prcMod

  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_rdnutsub
  USE s_ckopti
  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=80)           :: titgen      ! title line
  INTEGER(i4b)                :: icont       ! Continuity between sets
                                             ! =0: no continuity
                                             ! =1: continuity in
                                             !     UT1-UTC, DEPS,DPSI
  INTEGER(i4b)                :: isubap      ! Restitute subdaily pole
                                             ! =0: no
                                             ! =1: yes
  INTEGER(i4b)                :: idouble     ! Allow record with same epoch
                                             ! =0: no
                                             ! =1: yes

  CHARACTER(LEN=16)           :: subnam      ! Subdaily Pole Model Name
  CHARACTER(LEN=16)           :: nutnam      ! Nutation Model Name
  INTEGER(i4b),DIMENSION(2)   :: iline       ! Start and end record
  INTEGER(i4b)                :: sampl       ! Sampling


! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=45)             :: srName= 'SR piinpt (PG polint)'
  CHARACTER(LEN=chrValLength)   :: chrVal
  CHARACTER(LEN=8),PARAMETER    :: srNam= 'piinpt  '
  INTEGER(i4b)                  :: irc, irCode


  INTEGER(i4b), DIMENSION(2)  :: poltyp           ! i = 1 nutation model
                                                  ! ... 1 = no
                                                  ! ... 2 = observed
                                                  ! ... 3 = herring
                                                  ! i = 2 subdaily pole model
                                                  ! ... 1 = no
                                                  ! ... 2 = ray

! Nullify pointer
! ---------------
  NULLIFY(keyValue)

  irCode = 0
  poltyp(1) = 0
  poltyp(2) = 0

! Read Title for Result Files
! ---------------------------
  CALL readKeys('TITLE', keyValue, irc)
  titgen = keyValue(1)


! Read Model Names
! ----------------
  CALL rdnutsub(nutnam,subnam)
  chrVal = ' '
  chrVal(1:4)='BIAS'
  CALL setModKey(mod_orb_prcMod,chrVal,srNam,0.D0)


! Read Continuity between Sets
! ----------------------------
  CALL ckoptb(1,(/'CONT'/),srName,'Continuity', irCode, result1=icont)


! Read Restitution of subdaily Pole Model
! ---------------------------------------
  CALL ckoptb(1,(/'RESTSUB'/),srName,'Restitution of subdaily Pole', irCode, result1=isubap)


! Read double Epochs
! ----------------------------
  CALL ckoptb(1,(/'DBLEPO'/),srName,'Double Epochs', irCode, result1=idouble)

! Read start and end epoch
! ------------------------
  CALL readKeys('ILINEA', keyValue, irc)
  CALL ckopti(1,'ILINEA', keyValue, srName, 'Default record start', &
                  irc, irCode, ge=1, result1=iline(1))

  CALL readKeys('ILINEB', keyValue, irc)
  CALL ckopti(1,'ILINEB', keyValue, srName, 'Default record end', &
                  irc, irCode, ge=1, result1=iline(2))

! Read sampling
! -------------
  CALL readKeys('SAMPLING', keyValue, irc)
  CALL ckopti(1,'SAMPLING', keyValue, srName, 'Sampling', &
                  irc, irCode, ge=1, result1=sampl)


  IF (irCode /= 0) THEN
    WRITE(lfnerr,"(/,' *** SR PIINPT: Number of errors: ',I2)")irCode
    CALL exitrc(2)
  END IF



! Write options
! -------------
  WRITE(lfnprt,"(' OPTIONS', &
           &   /,' -------', &
           &  //,1X,79('-'), &
           &   /,' Nutation model                  :    ',A, &
           &   /,' Subdaily model                  :    ',A, &
           &   /,' Continuity between Sets         :',I5, &
           &   /,' Restitute subdaily Pole         :',I5, &
           &   /,' Double epochs                   :',I5, &
           &   /,' Start record                    :',I5, &
           &   /,' End record                      :',I5, &
           &   /,' Sampling                        :',I5, &
           &   /,1X,79('-'))") &
           nutnam,subnam,icont,isubap,idouble,iline(1),iline(2),sampl


END SUBROUTINE piinpt

END MODULE
