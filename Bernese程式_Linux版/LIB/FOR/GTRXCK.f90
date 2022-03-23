MODULE s_GTRXCK
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE gtrxck(epoch,dtSim,clkNam,iStop,secipl,clock,sigma,irCode,locq)

! -------------------------------------------------------------------------
! Purpose:    Get a clock value from clock RINEX file
!
! Author:     R. Dach
!
! Created:    18-Jun-2003
! Last mod.:  10-Jun-2009
!
! Changes:    26-Sep-2003 HB: Read clock RINEX file at first call,
!                             save epoch index, so that not always the hole
!                             array has to looped
!             03-Oct-2003 HB: Linear interpolation of clock corrections
!             14-Apr-2005 HU: Use interfaces to RDCRXH, RDCRXR
!             10-Jun-2009 RD: Use "undef" to init. clocks
!
! SR used:    init_clkHead,init_clkRed,gtFlna,alcErr,opnFil,opnErr,rdCrxH,
!             rdCrxR
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_maxdim, ONLY: maxsta,maxsat
  USE d_clkrnx, ONLY: t_clkHead, t_clkrec, init_clkHead, init_clkrec, undef
  USE s_alcerr
  USE s_opnfil
  USE s_opnerr
  USE s_rdcrxh
  USE s_rdcrxr
  USE s_gtflna
  USE f_tstkey
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  REAL(r8b)                            :: epoch    ! Epoch of the request (MJD)
  REAL(r8b)                            :: dtSim    ! Tolerance for epoch (days)
  CHARACTER(LEN=*)                     :: clkNam   ! Name of the clock
  INTEGER(i4b)                         :: iStop    ! Error handling
                                                   ! 0: no stop, no message
                                                   ! 1: stop
                                                   ! 2: no stop but message
                                                   ! (1,2 not implemented)
  REAL(r8b)                            :: secipl   ! Clock interpolation
                                                   ! 0d0: no interpolation
                                                   ! else tolerance for
                                                   !   linear interpolation

! output:
  REAL(r8b)                            :: clock    ! clock value (ms)
  REAL(r8b)                            :: sigma    ! sigma of the clock (ms)
  INTEGER(i4b)                         :: irCode   ! Return code
                                                   ! 0: value found
                                                   ! 1: no value for epoch
                                                   ! 2: clock not in file
                                                   ! 3: no file

! input:
  INTEGER(i4b), DIMENSION(:), OPTIONAL :: locq     ! locq for ityp 23 or 24
                              ! it is used to speed up the procedure;
                              ! it is not required.

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER                   :: srName = 'gtrxck'

! Local Variables
! ---------------
  TYPE(t_clkHead),                         SAVE :: clkHead
  TYPE(t_clkRec),                          SAVE :: clkRec

  CHARACTER(LEN=fileNameLength),           SAVE :: clkFil

  INTEGER(i4b),                            SAVE :: ircClk
  INTEGER(i4b),SAVE                             :: iEpo
  INTEGER(i4b)                                  :: iClk
  INTEGER(i4b)                                  :: ii,lEpo,kEpo
  INTEGER(i4b)                                  :: irc,ios

  REAL(r8b)                                     :: dEpoch

  LOGICAL,                                 SAVE :: first = .TRUE.

! Init variables
! --------------
  irCode = 3

  clock = undef
  sigma = 0d0

! Init the variables in the first call
! ------------------------------------
  IF (first) THEN

    CALL init_clkHead(clkHead)
    CALL init_clkRec(clkRec)

    IF (tstkey('RXCLKIN')) THEN
      CALL gtflna(0,'RXCLKIN',clkFil,ircClk)
    ELSE
      CALL gtflna(0,'SATCLK',clkFil,ircClk)
    ENDIF

    IF (ircClk == 0) THEN
      CALL opnfil(lfnor1,clkFil,'OLD','FORMATTED','READONLY',' ',ios)
      CALL opnerr(lfnerr,lfnor1,ios,clkFil,srName)
      CALL rdcrxh(lfnor1,lfnerr,clkHead,ircClk)
      IF (ircClk == 0) &
        CALL rdcrxr(lfnor1,lfnerr,(/0.D0,1.D20/),clkHead,clkRec,ircClk)
      CLOSE(lfnor1)
    ENDIF
    iEpo = 1
    first = .FALSE.
  ENDIF

! If there a clock file
! ---------------------
  IF (ircClk /= 0 .OR. LEN_TRIM(clkFil) == 0) RETURN

  irCode = 2
  iClk = 0
  kEpo = 0

  dEpoch = (epoch - clkHead%tFirst) * 86400d0

! Still the same epoch
  IF ((secipl == 0d0 .AND. &
       dEpoch >= clkRec%epoch(iEpo)-dtsim .AND. &
       dEpoch <= clkRec%epoch(iEpo)+dtsim)       .OR. &
      (secipl /= 0d0 .AND. &
       dEpoch >= clkRec%epoch(iEpo)-dtsim .AND. &
       dEpoch <= clkRec%epoch(iEpo+1)+dtsim)) THEN
    kEpo = iEpo
    DO ii = 1,clkHead%nSta+clkHead%nSat
      IF (clkNam == clkHead%clkName(ii)) THEN
        iClk = ii
        EXIT
      ENDIF
    ENDDO

! The last epoch
  ELSE IF (secipl == 0d0 .AND. iEpo == clkRec%nEpo .AND. &
       dEpoch >= clkRec%epoch(iEpo)-dtsim .AND. &
       dEpoch <= clkRec%epoch(iEpo)+dtsim) THEN
    kEpo = iEpo
    DO ii = 1,clkHead%nSta+clkHead%nSat
      IF (clkNam == clkHead%clkName(ii)) THEN
        iClk = ii
        EXIT
      ENDIF
    ENDDO

! Search epochs backward
  ELSE IF (iEpo == clkRec%nEpo .OR. &
      dEpoch <= clkRec%epoch(iEpo)) THEN
    epoLoop2: DO lEpo = iEpo-1,1,-1
      IF (dEpoch <  clkRec%epoch(lEpo+1) .AND.&
          dEpoch >= clkRec%epoch(lEpo)) THEN
        kEpo = lEpo
        DO ii = 1,clkHead%nSta+clkHead%nSat
          IF (clkNam == clkHead%clkName(ii)) THEN
            iClk = ii
            EXIT epoLoop2
          ENDIF
        ENDDO
      ENDIF
    ENDDO epoLoop2

! Search epochs foreward
  ELSEIF ( (secipl == 0d0 .AND. dEpoch > clkRec%epoch(iEpo)) .OR. &
           (secipl /= 0d0 .AND. dEpoch > clkRec%epoch(iEpo+1)) ) THEN
    epoLoop1: DO lEpo = iEpo+1,clkRec%nEpo-1
      IF (dEpoch >= clkRec%epoch(lEpo) .AND.&
          dEpoch <  clkRec%epoch(lEpo+1)) THEN
        kEpo = lEpo
        DO ii = 1,clkHead%nSta+clkHead%nSat
          IF (clkNam == clkHead%clkName(ii)) THEN
            iClk = ii
            EXIT epoLoop1
          ENDIF
        ENDDO
      ENDIF
    ENDDO epoLoop1
  ENDIF

  IF (kEpo /= 0) iEpo = kEpo


! Clock not in list
! -----------------
  IF (iClk == 0) RETURN

! Find the clock value for the epoch
! ----------------------------------
  irCode = 1

  IF (secipl == 0D0) THEN
    IF (clkRec%clock(iClk,iEpo) /= undef ) THEN
      clock = clkRec%clock(iClk,iEpo)
      sigma = clkRec%sigma(iClk,iEpo)
      irCode = 0
    ENDIF
  ELSEIF (DABS(clkRec%epoch(iEpo+1) - clkRec%epoch(iEpo)) <= secipl) THEN
    IF (clkRec%clock(iClk,iEpo)   /= undef .AND. &
        clkRec%clock(iClk,iEpo+1) /= undef) THEN
      clock = (clkRec%clock(iClk,iEpo+1)*(dEpoch-clkRec%epoch(iEpo)/86400.D0) +&
               clkRec%clock(iClk,iEpo)*(clkRec%epoch(iEpo+1)/86400.D0-dEpoch))/&
             ((clkRec%epoch(iEpo+1) - clkRec%epoch(iEpo))/86400.D0)
      sigma = clkRec%sigma(iClk,iEpo)
      irCode = 0
    ENDIF
  ENDIF

  RETURN
END SUBROUTINE gtrxck

END MODULE
