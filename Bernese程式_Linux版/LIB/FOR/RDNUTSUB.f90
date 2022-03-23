MODULE s_RDNUTSUB
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdnutsub(nutnam,subnam)

! -------------------------------------------------------------------------
! Purpose:    This SR reads the names of the Subdaily Pole Model and the
!             Nutation Model from the files specified in the Input Panel
!
! Author:     P. Steigenberger
!
! Created:    03-Feb-2003
! Last mod.:  19-May-2011
!
! Changes:    23-Apr-2003 RD: Use SR gtflna to get file name
!             15-May-2003 HU: Initialize structures
!             06-Aug-2003 HU: Buffering of model names
!             18-Aug-2003 RD: Close file
!             06-May-2011 HB: Use d_model to set model information
!             19-May-2011 HB: SR chkModKey called without srname,
!                             small modifications
!
! SR called:  gtflna, opnfil, opnerr, rdnutm
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_nutmod, ONLY: t_nutat,init_nutat
  USE d_model,  ONLY: setModKey, chkModKey, chrValLength,&
                      mod_orb_nutmod, mod_orb_submod

  USE s_rdnutm
  USE s_opnfil
  USE s_opnerr
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:

! output:
  CHARACTER(LEN=16)           :: nutnam ! name of the nutation model
  CHARACTER(LEN=16)           :: subnam ! name of the subdaily model

! List of Functions
! -----------------

! Local Types
! -----------
  TYPE(t_nutat)               :: nutat           ! Nutation model parameters

! Local Parameters
! ----------------


! Local Variables
! ---------------
  CHARACTER(LEN=80)             :: title
  CHARACTER(LEN=45),PARAMETER   :: srName= 'SR rdnutsub'
  CHARACTER(LEN=8),PARAMETER    :: srNam= 'rdnutsub'
  CHARACTER(LEN=fileNameLength) :: subfil     ! Filename of Subdaily ERP Model
  CHARACTER(LEN=fileNameLength) :: nutfil     ! Filename of Nutation Model
  CHARACTER(LEN=chrValLength)   :: chrVal
  CHARACTER(LEN=16),SAVE        :: nutsav
  CHARACTER(LEN=16),SAVE        :: subsav

  REAL(r8b)                     :: numVal

  INTEGER(i4b)                  :: irc, iostat, irChk
  INTEGER(i4b),SAVE             :: ifirst=1

  IF (ifirst==1) THEN
    ifirst=0

! Read Nutation Model
! -------------------
    CALL gtflna(1,'NUTMOD',nutfil,irc)

    CALL init_nutat(nutat)
    CALL rdnutm(nutfil,nutat)
    nutsav=nutat%nutnam

    DEALLOCATE(nutat%nutmlt,STAT=irc)
    DEALLOCATE(nutat%nutper,STAT=irc)
    DEALLOCATE(nutat%nutcoe,STAT=irc)

! Read Subdaily Model
! -------------------
    CALL gtflna(1,'SUBMOD',subfil,irc)

    CALL opnfil(lfnloc,subfil,'OLD','FORMATTED',          &
                  'READONLY',' ',IOSTAT)
    CALL opnerr(lfnerr,lfnloc,iostat,subfil,srName)
    READ(lfnloc, "(A80,///,25X,A16,////////)") title,subsav
    CLOSE(lfnloc)

    chrVal = ' '
    CALL chkModKey(2,mod_orb_nutMod,chrVal,numVal,irChk)
    IF (irChk /= 2 ) THEN
      numVal= 0.D0
      chrVal(1:16)=nutsav
      CALL setModKey(mod_orb_nutMod,chrVal,srNam,numVal)
    ENDIF
    CALL chkModKey(2,mod_orb_subMod,chrVal,numVal,irChk)
    IF (irChk /= 2 ) THEN
      chrVal(1:16)=subsav
      CALL setModKey(mod_orb_subMod,chrVal,srNam,numVal)
    ENDIF

  ENDIF

  nutnam=nutsav
  subnam=subsav

END SUBROUTINE rdnutsub



END MODULE
