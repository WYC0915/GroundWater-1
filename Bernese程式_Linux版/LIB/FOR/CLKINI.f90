MODULE s_CLKINI
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE clkini(title, nstat, stname,xstat, datum,           &
                    nftot, timref,ideltt,nEpoch,window,          &
                    nsampl,dtsim, opteli,nallsat, allsatnum,     &
                    nclkst,clksta,nclksa,clksat,clkhed,clkrec)

! -------------------------------------------------------------------------
! Purpose:    Init some clock parameters
!               - list of all station/satellite clocks
!               - list of (fixed) reference clocks
!               - some other stuff for clock rinex header
!
! Remark:     It was developed from the SR CLKRNX.f90
!
! Author:     R. Dach
!
! Created:    25-Jan-2002
!
! Changes:    19-Nov-2002 RD: Allocate clkrec%clock to 0 if not used
!             11-Dec-2002 RD: Correct tFirst if nSampl is used
!             21-Jan-2004 RD: Stronger weight for the sum
!             07-Feb-2005 HB: Adopt for ifc-Compiler, Version 8.1
!             30-Nov-2005 RD: Satellite clock name using svn2chr
!             24-Nov-2006 AG: 'V'//PGMVER ADDED TO PROGNAM
!             27-May-2009 RD: Special sampling for resubstition of epoch param.
!             10-Jun-2009 RD: Use "undef" to init. clocks
!             25-May-2010 MF: Call init_ref
!             28-Mar-2012 RD: Use SVN2CHR as module now
!             28-Mar-2012 RD: Remove unused variables
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, pgmver, staNameLength
  USE d_const,  ONLY: date,time,C
  USE d_clkrnx, ONLY: t_clkhead, t_clkrec, undef, init_ref

  USE s_alcerr
  USE s_mjdgps
  USE s_svn2chr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)             :: title     ! Title line
  INTEGER(i4b)                 :: nstat     ! Number of stations
  CHARACTER(LEN=staNameLength), &
                DIMENSION(:)   :: stname    ! List of station names
  REAL(r8b),    DIMENSION(:,:) :: xstat     ! XYZ coordinates of the stations
  CHARACTER(LEN=*)             :: datum     ! Geodetic datum
  INTEGER(i4b)                 :: nftot     ! Number of obs files
  REAL(r8b),    DIMENSION(:)   :: timref    ! Reference time for obs. files
  INTEGER(i4b), DIMENSION(:)   :: ideltt    ! Sampling rate for the obs. files
  INTEGER(i4b), DIMENSION(:)   :: nEpoch    ! Number of epoh in the obs files
  REAL(r8b),    DIMENSION(:,:) :: window    ! Time window for the obs. files
  INTEGER(i4b), DIMENSION(3)   :: nsampl    ! sampling rate (sec)
                                            ! 1: observations
                                            ! 2: resubstitution of epoch param.
                                            ! 3: preeliminate of epoch param.
  REAL(r8b)                    :: dtsim     ! Maximal interval to identify
                                            ! epoch (day fraction)
  INTEGER(i4b), DIMENSION(:)   :: opteli    ! Preelimination handling
  INTEGER(i4b)                 :: nAllSat   ! number of all satellites
  INTEGER(i4b), DIMENSION(:)   :: allSatNum ! satellite numbers
  INTEGER(i4b)                 :: nclkst    ! # Station clock requests
  INTEGER(i4b), DIMENSION(:)   :: clksta    ! Station clock requests
  INTEGER(i4b)                 :: nclksa    ! # of satellite clock requests
  INTEGER(i4b), DIMENSION(:)   :: clksat    ! Satellite clock requests

! in/output
  TYPE(t_clkHead)              :: clkhed    ! Header information for
                                            ! clock rinex output file
                                            ! clkhed%numRef == 0: fix ref.clock
                                            ! clkhed%numRef == 2: indicates
                                            !    condition of sum for reference
! output:
  TYPE(t_clkRec)               :: clkrec    ! %nEpo: # epochs with highest sampl.
                                            ! %epoch(1): last epoch (sec. since
                                            !   clkhed%tFirst)
                                            ! %clock: Apriori station clocks
                                            !   (if necessary)

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER                :: srName = 'clkini'

! Local Variables
! ---------------
  CHARACTER(LEN=1)                           :: svnchr

  INTEGER(i4b)                               :: svnnum
  INTEGER(i4b)                               :: numEpo,iEpo
  INTEGER(i4b)                               :: nweek
  INTEGER(i4b)                               :: iFil
  INTEGER(i4b)                               :: iSta, jSta
  INTEGER(i4b)                               :: iSat, jSat
  INTEGER(i4b)                               :: iRef
  INTEGER(i4b)                               :: ios, irc
  INTEGER(i4b)                               :: numRef

  REAL(r8b)                                  :: tObs
  REAL(r8b)                                  :: gpssec

! Estimate all satellite clocks
! -----------------------------
  IF (nclksa > 0 .AND. clksat(1) == 99) THEN
    nclksa=nAllSat
    clksat(1:nClkSa)=allSatNum(1:nAllSat)
  ENDIF

! Figure out data types
! ---------------------
  clkhed%numTyp=0
  IF (nclkst+nclksa /= 0) THEN
    ALLOCATE(clkhed%datTyp(2), stat=ios)
    CALL alcerr(ios,'clkhed%datTyp',(/2/),srName)

    clkhed%numTyp = 2
    clkhed%datTyp = (/'AR','AS'/)
  ENDIF

! Copy station clock information
! ------------------------------
  clkhed%nSta = nStat

  ALLOCATE(clkhed%clkname(nStat+nAllSat),stat=ios)
  CALL alcerr(ios,'clkhed%clkname',(/nstat+nAllSat/),srName)

  clkHed%clkName = ' '

  clkhed%clkName(1:nStat) = stName(1:nStat)

  ALLOCATE(clkhed%staCoord(3,nstat),stat=ios)
  CALL alcerr(ios,'clkhed%StaCoord',(/3,nstat/),srName)

  clkhed%staCoord = xStat(1:3,1:nStat)

! Copy satellite clock information
! --------------------------------
  clkhed%nSat = nAllSat

  DO iSat=1,nAllSat
    CALL svn2chr(allSatNum(iSat),svnnum,svnchr)
    WRITE(clkhed%clkname(nStat+iSat),'(A,I2.2)') svnchr,svnnum
  ENDDO

! Figure out reference clock(s)
! Only if fixed reference; reference with sum in handled in RDICLK
! -----------------------------
  IF (clkhed%numRef == 0) THEN
    clkhed%numRef = 1

    ALLOCATE(clkhed%ref(1),stat=ios)
    CALL alcerr(ios,'clkhed%ref',(/1/),srName)

    CALL init_ref(clkhed%ref(1))

    clkhed%ref(1)%refWin%t=0d0

    clkhed%ref(1)%nRef = 0
    iStaLoop: DO iSta=1,clkHed%nSta

      DO jSta=1,nclkst
        IF (clksta(jSta) == iSta) CYCLE iStaLoop
      ENDDO

! Station not found --> must be reference station
      clkhed%ref(1)%nRef = clkhed%ref(1)%nRef + 1

    ENDDO iStaLoop

! There are satellite reference clocks?
! -------------------------------------
    IF (nAllSat - nClkSa > 0) &
      clkhed%ref(1)%nRef = clkhed%ref(1)%nRef + (nAllSat - nClkSa)

! Copy reference clock information
! --------------------------------
!!!!!!!! Problems with ifc81 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CMP_IFC8
    numRef = clkhed%ref(1)%nRef
    ALLOCATE(clkhed%ref(1)%clk(numRef), stat=ios)
    CALL alcerr(ios,'clkhed%ref(1)%clk',(/numRef/),srName)
#else
    ALLOCATE(clkhed%ref(1)%clk(clkhed%ref(1)%nRef), stat=ios)
    CALL alcerr(ios,'clkhed%ref(1)%clk',(/clkhed%ref(1)%nRef/),srName)
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    clkhed%ref(1)%clk(:)%name = ' '
    clkhed%ref(1)%clk(:)%sigma = 0d0

    iRef = 0
    iStaLoop2: DO iSta=1,clkHed%nSta

      DO jSta=1,nclkst
        IF (clksta(jSta) == iSta) CYCLE iStaLoop2
      ENDDO

      iRef = iRef + 1
      clkhed%ref(1)%clk(iRef)%Idx  = iSta
      clkhed%ref(1)%clk(iRef)%Idx0 = 0
      clkhed%ref(1)%clk(iRef)%Name = clkhed%clkName(iSta)

    ENDDO iStaLoop2

    IF (iRef < clkhed%ref(1)%nRef) THEN

      iSatLoop2: DO iSat=1,clkHed%nSat

        DO jSat=1,nclksa
          IF (clksat(jSat) == allSatNum(iSat)) CYCLE iSatLoop2
        ENDDO

        iRef = iRef + 1
        clkhed%ref(1)%clk(iRef)%Idx  = nStat+iSat
        clkhed%ref(1)%clk(iRef)%Idx0 = allSatNum(iSat)
        clkhed%ref(1)%clk(iRef)%Name = clkhed%clkName(nStat+iSat)

      ENDDO iSatLoop2

    ENDIF

! Set the sigma for the reference clocks for cond. of sum
! -------------------------------------------------------
  ELSE
    clkhed%ref(1)%clk(:)%sigma = clkhed%ref(1)%nRef/1000d0/C
  ENDIF

! Loop all obs.files, get first/last epoch
! ----------------------------------------
  clkhed%tfirst = 0d0

  ALLOCATE(clkrec%epoch(1),stat=irc)
  CALL alcerr(irc,'clkrec%epoch',(/1/),srName)
  clkrec%epoch=0d0

  DO iFil = 1,nftot
    DO iEpo = 1,nEpoch(iFil)

      tObs = timref(iFil)+(iEpo-1)*iDeltT(iFil)/86400D0

      IF (tObs < (window(1,iFil)-dtsim) .OR. &
          tObs > (window(2,iFil)+dtSim)) CYCLE

      IF (nSampl(1) /= 0) THEN
        CALL mjdgps(tObs,gpssec,nweek)
        IF (DABS(gpssec-DNINT(gpssec/nSampl(1))*nSampl(1)) > dtSim*86400.D0) CYCLE
      ENDIF

      IF (clkhed%tFirst   == 0d0 .OR. clkhed%tFirst   > tObs) &
          clkhed%tFirst = tObs
      IF (clkrec%epoch(1) == 0d0 .OR. clkrec%epoch(1) < tObs) &
          clkrec%epoch(1) = tObs

    ENDDO
  ENDDO

  clkrec%epoch = (clkrec%epoch(1)-clkhed%tFirst)*86400d0

! Get the number of epochs for this time interval
! -----------------------------------------------
  IF (nSampl(1) /= 0) THEN
    clkrec%nEpo = NINT(clkrec%epoch(1)/DBLE(nSampl(1)))+1
    tObs = (clkrec%nEpo)*nSampl(1)
  ELSE
    clkrec%nEpo = 0
    DO iFil = 1,nftot
      numEpo = NINT(clkrec%epoch(1)/DBLE(iDeltT(iFil)))+1
      IF (numEpo > clkrec%nEpo) THEN
        clkrec%nEpo = numEpo
        tObs = (clkrec%nEpo)*iDeltT(iFil)
      ENDIF
    ENDDO
  ENDIF
  clkrec%epoch(1) = tObs


! Now set program and such stupid information
! -------------------------------------------
  clkhed%LeapSec = 0.0D0
  clkhed%CrDate  = DATE//' '//TIME
  clkhed%TRFName = DATUM

  clkhed%prognam = 'GPSEST V'//PGMVER
  IF (LEN_TRIM(clkhed%Runby)  == 0) clkhed%Runby   = 'AIUB/CODE'
  IF (LEN_TRIM(clkhed%AC)     == 0) clkhed%AC      = 'COD'
  IF (LEN_TRIM(clkhed%ACname) == 0) clkhed%ACName  = &
               'CENTER FOR ORBIT DETERMINATION IN EUROPE'

  IF (clkhed%nComment == 0) THEN
    clkhed%nComment= 1
    ALLOCATE(clkhed%Comment(clkhed%nComment),stat=ios)
    CALL alcerr(ios,'clkhed%Comment',(/clkhed%nComment/),srName)
    clkhed%Comment(1)=title(1:60)
  ENDIF


! Allocate record for apriori station clocks
! ------------------------------------------
  IF (opteli(23) == 0 .OR. opteli(23) == 2) THEN
    ALLOCATE(clkrec%clock(nStat,clkrec%nEpo),stat=irc)
    CALL alcerr(irc,'clkrec%clock',(/nStat,clkrec%nEpo/),srName)
    clkrec%clock = unDef
  ELSE IF ( opteli(23) == 3 .AND. nSampl(3) /= 0) THEN
    numEpo = NINT(clkrec%epoch(1)/DBLE(nSampl(3)))+1
    ALLOCATE(clkrec%clock(nStat,numEpo),stat=irc)
    CALL alcerr(irc,'clkrec%clock',(/nStat,numEpo/),srName)
    clkrec%clock = unDef
  ELSE
    ALLOCATE(clkrec%clock(nStat,0),stat=irc)
    CALL alcerr(irc,'clkrec%clock',(/nStat,0/),srName)
  ENDIF


  RETURN
END SUBROUTINE clkini

END MODULE
