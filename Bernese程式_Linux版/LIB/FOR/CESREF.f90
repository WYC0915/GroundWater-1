MODULE s_CESREF
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE cesref(isel,icond,irnx,ipref,opt,InClkHead,InClkRec,   &
                  nepo, epoch,nsvn,svnnum,ncksat,ndfsat,mepsat,   &
                  nsta,stanam,ncksit,ndfsit,mepsit,               &
                  irfsat,irfsit,irfrnx,islclk,refclk,svnrf,irc)

! -------------------------------------------------------------------------
! Purpose:    Select reference clock
!
! Remarks:    - Subroutine of CLKEST
!
! Author:     U. Hugentobler
!
! Created:    02-Jul-2002
!
! Changes:    31-Aug-2002 HU: Count number of clocks in CLK RINEX
!             04-May-2004 HB: Allow for satellite as reference for isel=4
!             07-Jun-2004 HB: Correct handling of selected reference
!                             station (list)
!             11-Dec-2007 HB: Call exitrc(2) if reference clock is not
!                             present in data
!             28-Mar-2012 RD: Use SVN2CHR as module now
!             28-Mar-2012 RD: Remove unused variables
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, staNameLength
  USE p_clkest, ONLY: t_clkopt
  USE d_clkrnx, ONLY: t_clkhead,t_clkrec

  USE s_svn2chr
  USE s_exitrc
  IMPLICIT NONE

! List of arguments
! -----------------
! Input:
  INTEGER(i4b)                :: isel      ! Selection:
                                           !  1: specified clock
                                           !  2: clock from list
                                           !  3: automatic selection
                                           !  4: same clock as in clock RINEX
  INTEGER(i4b)                :: icond     ! Condition
                                           !  1: No missing clocks
                                           !  2: No missing clock differences
                                           !  3: Minimum number of missing clk
                                           !  4: Minimum number of missing dclk
  INTEGER(i4b)                :: irnx      ! Clock available in RNX file
                                           !  1: required
  INTEGER(i4b)                :: ipref     ! Preference:
                                           !  1: station, 2: satellite
  TYPE(t_clkopt)              :: opt       ! Options
  TYPE(t_clkhead)             :: InClkHead ! ClkRinex header
  TYPE(t_clkrec)              :: InClkRec  ! ClkRinex records
  INTEGER(i4b)                :: nepo      ! Total number of epochs
  REAL(r8b),DIMENSION(:)      :: epoch     ! Epochs (MJD)
! Satellites:
  INTEGER(i4b)                :: nsvn      ! Number of satellites
! For satellite i=1,...,svcmb:
  INTEGER(i4b),DIMENSION(:)   :: svnnum    ! SVN numbers for satellites
  INTEGER(i4b),DIMENSION(:)   :: ncksat    ! Number of clock parameters
  INTEGER(i4b),DIMENSION(:)   :: ndfsat    ! Number of clock differences
! For epoch i and satellite k=1,...,svcmp:
  INTEGER(i4b),DIMENSION(:,:) :: mepsat    ! Epoch indices
! Stations:
  INTEGER(i4b)                :: nsta      ! Number of stations (files)
! For station i=1,...,nsta:
  CHARACTER(LEN=staNameLength), &
               DIMENSION(:)   :: stanam    ! Station name
  INTEGER(i4b),DIMENSION(:)   :: ncksit    ! Number of clock parameters
  INTEGER(i4b),DIMENSION(:)   :: ndfsit    ! Number of clock differences
! For epoch i and station k=1,...,nsta:
  INTEGER(i4b),DIMENSION(:,:) :: mepsit    ! Epoch indices
! Output:
  INTEGER(i4b)                :: irfsat    ! Number of ref. sat, or zero
  INTEGER(i4b)                :: irfsit    ! Number of ref. sta, or zero
  INTEGER(i4b)                :: irfrnx    ! Number of ref. clk in rnx
  INTEGER(i4b)                :: islclk    ! Reference clock is
                                           !  1: satellite
                                           !  2: station
  CHARACTER(LEN=staNameLength):: refclk    ! Name of reference clock
  INTEGER(i4b)                :: svnrf     ! SVN nr for ref. sat, or zero
  INTEGER(i4b)                :: irc       ! Return code
                                           ! =1: no reference found

! Local variables
! ---------------
  INTEGER(i4b)                :: iclk,icrx,ilst,irnx1,svnmod,iFind
  INTEGER(i4b)                :: iepo,kepo,jepo,icrnx
  INTEGER(i4b)                :: izsit,izsat,lzsit,lzsat
  INTEGER(i4b)                :: imsit,imsat,lmsit,lmsat
  INTEGER(i4b)                :: minsit,minsat
  INTEGER(i4b)                :: mxzsit,mxxsit,mxzsat,mxxsat
  REAL(r8b)                   :: epornx
  CHARACTER(LEN=1)            :: svnchr
  CHARACTER(LEN=staNameLength):: clknam

  INTEGER(i4b),DIMENSION(nsvn):: msat,ixsat,nxsat
  INTEGER(i4b),DIMENSION(nsta):: msit,ixsit,nxsit
! REAL(r8b),DIMENSION(nsvn)   :: rmssvn
! REAL(r8b),DIMENSION(nsta)   :: rmssit

! Undefined CLK Rinex value
  REAL(r8b)                   :: UNDEF=999999.999999D0

  irfsit = 0
  irfsat = 0
  irfrnx = 0
  svnrf  = 0
  iFind  = 0
  irc    = 0

  irnx1=irnx
  IF (InClkHead%nSta+InClkHead%nSat==0) irnx1=0

! Check number of missing epochs
! ------------------------------
! Station clock
  DO iclk=1,nsta
    IF (icond==1.OR.icond==3) THEN
      msit(iclk)=nepo-ncksit(iclk)
    ELSEIF (icond==2.OR.icond==4) THEN
      msit(iclk)=nepo-(ndfsit(iclk)+1)
    ELSE
      WRITE(lfnerr,"(/,' *** SR CESREF: icond=',I4,' not defined',/)") icond
      CALL exitrc(2)
    ENDIF
  ENDDO

! Satellite clock
  DO iclk=1,nsvn
    IF (icond==1.OR.icond==3) THEN
      msat(iclk)=nepo-ncksat(iclk)
    ELSEIF (icond==2.OR.icond==4) THEN
      msat(iclk)=nepo-(ndfsat(iclk)+1)
    ENDIF
  ENDDO

! Check existence of clock in RINEX
! ---------------------------------
  IF (irnx1==1) THEN
    DO iclk=1,nsta
      ixsit(iclk)=0
      DO icrx=1,InClkHead%nSta+InClkHead%nSat
        IF (InClkHead%ClkName(icrx)==stanam(iclk)) ixsit(iclk)=icrx
      ENDDO
    ENDDO

    DO iclk=1,nsvn
      ixsat(iclk)=0
      CALL svn2chr(svnnum(iclk),svnmod,svnchr)
      WRITE(clknam,"(A1,I2.2)") svnchr,svnmod
      DO icrx=1,InClkHead%nSta+InClkHead%nSat
        IF (InClkHead%ClkName(icrx)==clknam) ixsat(iclk)=icrx
      ENDDO
    ENDDO
  ENDIF

! Count number of clocks in RINEX
! -------------------------------
  IF (irnx1==1 .AND. opt%ifix==1 .AND. (icond==2 .OR. icond==4)) THEN
    icrnx=1
    DO iclk=1,nsta
      nxsit(iclk)=0
      kepo=1
      DO iepo=1,ncksit(iclk)
        IF (ixsit(iclk)==0) CYCLE
! Find epoch in rnx file
        DO jepo=kepo,InClkRec%nEpo
          epornx=InClkHead%Tfirst+InClkRec%Epoch(jepo)/86400D0
          IF (ABS(epoch(mepsit(iepo,iclk))-epornx).LT.1D-8) THEN
            IF(InClkRec%Clock(ixsit(iclk),jepo) /= UNDEF) THEN
              nxsit(iclk)=nxsit(iclk)+1
            ENDIF
            kepo=jepo
            EXIT
          ELSEIF (epornx > epoch(mepsit(iepo,iclk))) THEN
            EXIT
          ENDIF
        ENDDO
      ENDDO
    ENDDO

    DO iclk=1,nsvn
      nxsat(iclk)=0
      kepo=1
      DO iepo=1,ncksat(iclk)
        IF (ixsat(iclk)==0) CYCLE
! Find epoch in rnx file
        DO jepo=kepo,InClkRec%nEpo
          epornx=InClkHead%Tfirst+InClkRec%Epoch(jepo)/86400D0
          IF (ABS(epoch(mepsat(iepo,iclk))-epornx).LT.1D-8) THEN
            IF(InClkRec%Clock(ixsat(iclk),jepo) /= UNDEF) THEN
              nxsat(iclk)=nxsat(iclk)+1
            ENDIF
            kepo=jepo
            EXIT
          ELSEIF (epornx > epoch(mepsat(iepo,iclk))) THEN
            EXIT
          ENDIF
        ENDDO
      ENDDO
    ENDDO
  ELSE
    icrnx=0
  ENDIF

! Defined clock
! -------------
  IF (isel==1) THEN
! Station clock
    DO iclk=1,nsta
      IF (stanam(iclk)(1:4)==opt%refclk) irfsit=iclk
    ENDDO

! Satellite clock
    DO iclk=1,nsvn
      CALL svn2chr(svnnum(iclk),svnmod,svnchr)
      WRITE(clknam,"(A1,I2.2)") svnchr,svnmod
      IF (clknam==opt%refclk) irfsat=iclk
    ENDDO

! Conditions fulfilled?
    IF (irfsit /= 0) THEN
      IF ((icond==1.OR.icond==2).AND.msit(irfsit)>0) irfsit=0
      IF ( irnx1==1.AND.ixsit(irfsit)==0) irfsit=0
    ENDIF
    IF (irfsat /= 0) THEN
      IF ((icond==1.OR.icond==2).AND.msat(irfsat)>0) irfsat=0
      IF ( irnx1==1.AND.ixsat(irfsat)==0) irfsat=0
    ENDIF
  ENDIF

! Clock from list
! ---------------
  IF (isel > 1 .AND. iSel < 5) THEN
! Station clocks
    izsit=0
    minsit=HUGE(minsit)
    mxzsit=0
    mxxsit=0
    DO ilst=1,opt%nrfsta
      DO iclk=1,nsta
        IF (irnx1==1.AND.ixsit(iclk)==0) CYCLE
        IF (stanam(iclk)(1:4)==opt%refsta(ilst)) THEN
          IF (msit(iclk) == 0 .AND. &
             (icrnx==0 .AND. izsit==0 .OR. &
              icrnx==1 .AND. nxsit(iclk)>mxzsit)) THEN
            izsit =iclk
            lzsit =ilst
            mxzsit=nxsit(iclk)
          ENDIF
          IF (msit(iclk) < minsit .OR. &
              icrnx==1 .AND. msit(iclk) == minsit &
                       .AND. nxsit(iclk)>mxxsit) THEN
            minsit=msit(iclk)
            imsit =iclk
            lmsit =ilst
            mxxsit=nxsit(iclk)
          ENDIF
        ENDIF
      ENDDO
    ENDDO

! Satellite clocks
    izsat=0
    minsat=HUGE(minsat)
    mxzsat=0
    mxxsat=0
    DO ilst=1,opt%nrfsta
      DO iclk=1,nsvn
        IF (irnx1==1.AND.ixsat(iclk)==0) CYCLE
        CALL svn2chr(svnnum(iclk),svnmod,svnchr)
        WRITE(clknam,"(A1,I2.2)") svnchr,svnmod
        IF (clknam==opt%refsta(ilst)) THEN
          IF (msat(iclk) == 0 .AND. &
             (icrnx==0 .AND. izsat==0 .OR. &
              icrnx==1 .AND. nxsat(iclk)>mxzsat)) THEN
            izsat =iclk
            lzsat =ilst
            mxzsat=nxsat(iclk)
          ENDIF
          IF (msat(iclk) < minsat .OR. &
              icrnx==1 .AND. msat(iclk) == minsat &
                       .AND. nxsat(iclk)>mxxsat) THEN
            minsat=msat(iclk)
            imsat =iclk
            lmsat =ilst
            mxxsat=nxsat(iclk)
          ENDIF
        ENDIF
      ENDDO
    ENDDO
  ENDIF

! Select from all clocks
! ----------------------
  IF (isel > 2.AND.isel <= 4) THEN
! Station clocks
    DO iclk=1,nsta
      IF (irnx1==1.AND.ixsit(iclk)==0) CYCLE
      IF (msit(iclk) == 0 .AND. &
         (icrnx==0 .AND. izsit==0 .OR. &
          icrnx==1 .AND. nxsit(iclk)>mxzsit)) THEN
        izsit =iclk
        mxzsit=nxsit(iclk)
      ENDIF
      IF (msit(iclk) < minsit .OR. &
          icrnx==1 .AND. msit(iclk) == minsit &
                   .AND. nxsit(iclk)>mxxsit) THEN
        minsit=msit(iclk)
        imsit =iclk
        mxxsit=nxsit(iclk)
      ENDIF
    ENDDO

! Satellite clocks
    DO iclk=1,nsvn
      IF (irnx1==1.AND.ixsat(iclk)==0) CYCLE
      IF (msat(iclk) == 0 .AND. &
         (icrnx==0 .AND. izsat==0 .OR. &
          icrnx==1 .AND. nxsat(iclk)>mxzsat)) THEN
        izsat =iclk
        mxzsat=nxsat(iclk)
      ENDIF
      IF (msat(iclk) < minsat .OR. &
         (icrnx==1 .AND. msat(iclk) == minsat &
                   .AND. nxsat(iclk)>mxxsat)) THEN
        minsat=msat(iclk)
        imsat =iclk
        mxxsat=nxsat(iclk)
      ENDIF
    ENDDO
  ENDIF

! Take the same reference clock as in the clock RINEX file
! --------------------------------------------------------
  IF (iSel == 4) THEN
    refClk = InClkHead%ref(1)%clk(1)%Name
    irfRnx = InClkHead%ref(1)%clk(1)%Idx
    DO iClk = 1, nSta
      IF (staNam(iClk) == refClk) THEN
        irfSit = iClk
        islClk = 2
        iFind = 1
      ENDIF
    ENDDO
    DO iClk=1,nSvn
      CALL svn2chr(svnnum(iclk),svnmod,svnchr)
      WRITE(clknam,"(A1,I2.2)") svnchr,svnmod
      IF (clknam==refClk) THEN
        irfSat = iClk
        islClk = 1
        iFind = 1
      ENDIF
    ENDDO

    IF (irfSit == 0.AND.irfSat == 0) THEN
      iFind = 0
      write(lfnErr,'(A,/,A,A,/,A)')&
           ' *** SR cesref: Reference clock from ',&
           '                Clock Rinex File:    ', refClk,&
           '                cannot be found in observation data.'
      CALL exitrc(2)
    ENDIF
  ELSEIF (iSel == 1) THEN
     iFind = 1
  ENDIF

! Select the clock according to the criteria
! ------------------------------------------
  IF ((isel > 1 .AND. iSel < 4) .OR. iFind == 0) THEN
    IF (icond==1.OR.icond==2) THEN
      irfsit=izsit
      irfsat=izsat
    ELSE
      IF (minsit<minsat) THEN
        irfsit=imsit
      ELSEIF (minsit>minsat) THEN
        irfsat=imsat
      ELSE
        irfsit=imsit
        irfsat=imsat
      ENDIF
    ENDIF
  ENDIF

! Select from list: take first occurence
! --------------------------------------
  IF (isel==2.AND.irfsit/=0 .AND. irfsat/=0) THEN
    IF (icond==1.OR.icond==2) THEN
      IF (lzsit>lzsat) irfsit=0
      IF (lzsit<lzsat) irfsat=0
    ELSE
      IF (lmsit>lmsat) irfsit=0
      IF (lmsit<lmsat) irfsat=0
    ENDIF
  ENDIF

! Preference: Station or satellite
! --------------------------------
  IF (irfsit/=0 .AND. irfsat/=0) THEN
    IF (ipref==1) THEN
      irfsat=0
    ELSE
      irfsit=0
    ENDIF
  ENDIF

! Get rnx clock index of reference
! --------------------------------
  IF (irnx1==1) THEN
    IF (irfsit/=0) irfrnx=ixsit(irfsit)
    IF (irfsat/=0) irfrnx=ixsat(irfsat)
  ENDIF

! Type of reference
! -----------------
  IF (irfsat /= 0) THEN
    islclk=1
    svnrf=svnnum(irfsat)
    CALL svn2chr(svnrf,svnmod,svnchr)
    WRITE(refclk,"(A1,I2.2)") svnchr,svnmod
  ELSEIF (irfsit /= 0.AND.(iSel /= 4.OR.iFind == 0)) THEN
    islclk=2
    refclk=stanam(irfsit)
  ENDIF

  IF ( iSel == 4 .AND. iFind == 0 ) THEN
    write(lfnErr,'(/,A,/,A,A,/,A)')&
         ' *** SR cesref: Alternative reference clock',&
         '                is selected automatically:  ', refClk
  ENDIF

! No reference found
! ------------------
  IF (irfsit==0.AND.irfsat==0) irc=1

  RETURN
END SUBROUTINE cesref

END MODULE
