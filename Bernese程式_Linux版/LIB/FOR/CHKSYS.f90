MODULE s_CHKSYS
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE chksys(isel,line,irc)

! -------------------------------------------------------------------------
! Purpose:    Check consistency of systems.
!
! Remarks:    The routine compares the input names nutnam and subnam with
!             the corresponding entries in the input panel using routine
!             rdnutsub. In case of inconsistency:
!             for isel=0: A warning is issued
!             for isel=1: An error is issued and the program stopped.
!
!             If the keywords NUTMOD or SUBMOD are not specified in the
!             input panel, no test is performed.
!
!             If nutnam is blank, nutnam='IAU80' is assumed.
!             If subnam is blank, subnam='RAY' is assumed.
!
! Author:     U. Hugentobler
!
! Created:    18-Jul-2003
! Last mod.:  19-May-2011
!
! Changes:    01-Sep-2003 HU: Use interface for rdnutsub
!             29-Oct-2003 HU: Only warning if wrong subdaily
!             07-Jan-2005 HU: No stop if isel=0
!             28-Mar-2007 HB/HU: Replace check with check for NUTSUB-line
!                             (IERS2003 Standards)
!             06-May-2011 HB: Check for compatibility of NUT and SUB
!                             Activate V50 STD-file again
!             19-May-2011 HB: SR chkModKey without parameter srName
!
! SR used:    exitrc
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_model, ONLY : setModKey, chkModKey, chrValLength,&
                      mod_orb_prcMod
  USE s_exitrc
  USE s_rdnutsub
  USE s_gtflna
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  INTEGER(i4b)              :: isel           ! 0: warning, 1: error
  CHARACTER(LEN=*)          :: line           ! NUTSUB-line

! output:
  INTEGER(i4b)              :: irc            ! Return code
                                              ! 0: consistent
                                              ! 1: inconsistent

! Local parameters
! ----------------
  INTEGER(i4b),SAVE         :: ifirst=1
  INTEGER(i4b),SAVE         :: ircnut
  INTEGER(i4b),SAVE         :: ircsub
  INTEGER(i4b),SAVE         :: ircold
  INTEGER(i4b)              :: irChk
  CHARACTER(LEN=3)          :: chrerr='***'
  CHARACTER(LEN=8),PARAMETER :: srName='chkSys  '
  CHARACTER(LEN=16),SAVE    :: nutnm1='@               '
  CHARACTER(LEN=16),SAVE    :: subnm1='@               '
  CHARACTER(LEN=fileNameLength) :: filnam
  CHARACTER(LEN=80),SAVE    :: linsav
  CHARACTER(LEN=chrValLength) :: chrVal,getVal
  REAL(r8b)                 :: numVal

! Get name of nutation model and subdaily model from input panel
! --------------------------------------------------------------
  IF (ifirst==1) THEN
    ifirst=0
    CALL gtflna(0,'NUTMOD',filnam,ircnut)
    CALL gtflna(0,'SUBMOD',filnam,ircsub)
    IF (ircnut==0 .AND. ircsub==0) THEN
      CALL rdnutsub(nutnm1,subnm1)

! Add 'BIAS' to characterize >= IERS2003 standards
! ------------------------------------------------
      WRITE(linsav,"('NUTSUB: ',A16,1X,A16,1X,A)") nutnm1,subnm1,'BIAS'
    ENDIF
    ircold=0
  ENDIF

  irc=ircold
  IF (nutnm1 == '@'.AND.subnm1 == '@')RETURN

! Check SUB- and NUT-file for compatibility
! -----------------------------------------
  IF (nutnm1(1:10) == 'IAU2000R06' .OR. nutnm1(1:7) == 'IAU2006' ) THEN
    IF (subnm1(1:8) /= 'IERS2010') THEN
      IF (isel==0) chrerr='###'
      WRITE(lfnerr,"(/,1X,A3,' SR CHKSYS: Inconsistent systems',  &
             & /,'                found in nutation and subdaily model', &
             & /,'                NUT: ',A,     &
             & /,'                SUB expected: IERS2010',&
             & /,'                SUB found:   ',A,/)") &
             chrerr,trim(nutnm1),trim(subnm1)
      IF (isel/=0) CALL exitrc(2)
    ENDIF
  ENDIF

  IF (subnm1(1:8) == 'IERS2010') THEN
    IF (nutnm1(1:10) /= 'IAU2000R06' .AND. nutnm1(1:7) /= 'IAU2006' ) THEN
      IF (isel==0) chrerr='###'
      WRITE(lfnerr,"(/,1X,A3,' SR CHKSYS: Inconsistent systems',  &
             & /,'                found in nutation and subdaily model', &
             & /,'                SUB: ',A,     &
             & /,'                NUT expected: IAU2000R06 or IAU2006',&
             & /,'                NUT found:   ',A,/)") &
             chrerr,trim(subnm1),trim(nutnm1)
      IF (isel/=0) CALL exitrc(2)
    ENDIF
  ENDIF

! Check model names for consistency (without 'BIAS')
! --------------------------------------------------
  IF (line(1:41) /= linsav(1:41)) THEN
     ircnut=ircnut+1
     IF (ircnut<2) THEN
        IF (isel==0) chrerr='###'
        WRITE(lfnerr,"(/,1X,A3,' SR CHKSYS: Inconsistent systems',  &
             & /,'                Found in ', &
             & /,'                orbit file:       ',A,     &
             & /,'                INP panel/CHKSYS: ',A,/)") &
             chrerr,trim(line),trim(linsav)
        IF (isel/=0) CALL exitrc(2)
     ENDIF
     ircold=1
  ENDIF

! Check prcMod
! ------------
  chrVal = ' '
  IF (line(43:46) /= 'BIAS') THEN
    chrVal(1:4)='V50 '
    chrerr='###'
    WRITE(lfnerr,"(/,1X,A3,' SR CHKSYS: Version 5.0 STD-file is processed',  &
         & /,'                according to IERS2000 conventions',/)") &
         chrerr
    IF (subnm1(1:8) == 'IERS2010'.OR. nutnm1(1:10) == 'IAU2000R06'.OR.&
      & nutnm1(1:7) == 'IAU2006') THEN
      IF (isel==0) chrerr='###'
      WRITE(lfnerr,"(/,1X,A3,' SR CHKSYS: Inconsistent systems',  &
           & /,'                Found in ', &
           & /,'                orbit file:       ',A,     &
           & /,'                INP panel/CHKSYS: ',A,/)") &
             chrerr,trim(line),trim(linsav)
        IF (isel/=0) CALL exitrc(2)
    ENDIF
  ELSEIF (line(43:46) == 'BIAS') THEN
    chrVal(1:4)='BIAS'
  ENDIF

  CALL chkModKey(2,mod_orb_prcMod,getVal,numVal,irChk)
  IF (irChk == 2) THEN
    IF (chrVal(1:4) /= getVal(1:4)) irChk = 1
  ENDIF
  IF (irChk /= 2 ) THEN
    CALL setModKey(mod_orb_prcMod,chrVal,srName,0.D0)
  ENDIF

  irc=ircold

  RETURN

END SUBROUTINE chksys

END MODULE
