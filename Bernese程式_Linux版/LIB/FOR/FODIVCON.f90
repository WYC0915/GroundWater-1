MODULE s_FODIVCON
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodivcon(opt,sCore,iSta)

! -------------------------------------------------------------------------
! Purpose:    Setup the relative constrains on velocities.
!
! Author:     Luca Ostini
!
! Created:    14-Aug-2008
!
! Changes:    14-Aug-2008 LO: Created this file
!             02-Oct-2008 LO: First revision
!             09-Oct-2008 LO: Second revision
!             09-Oct-2008 LO: Third revision
!             13-Oct-2008 LO: Interval correction for STA-file added
!             05-Dec-2008 LO: Fourth revisio: velocity changes allowed
!             11-Feb-2009 LO: Fifth revision: major changes
!             12-Mar-2009 LO: Redundancy of velocity constraints removed
!             20-Mar-2009 LO: Bug in events array fixed
!             14-Aug-2009 LO: Getco3 changed
!             25-Sep-2009 LO: Changes for F90 consistency
!             18-Nov-2009 LO: Major changes for consistency
!             21-Dec-2009 LO: FFT removed and several changes apported
!             02-Mar-2010 LO: Major changes do to elimination of FODISUBR
!             26-Aug-2010 LO: Architectural changes
!             03-Jan-2010 LO: INTENT(INOUT) removed and bug fixed
!             24-May-2011 LO: New update of ADDNEQ2 meta-data
!             19-Jul-2011 LO: Test datum defintion added
!             19-Sep-2012 RD: Use P_FODITS with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used Modules
! ------------
! structures, variables:
  USE m_bern,    ONLY: i4b, r8b, shortLineLength
  USE p_fodits,  ONLY: t_opt, t_score, t_upd_ren, &
                       nmaxrenaming, typenone, infoinit, typevelo, &
                       significant, typejump, typejumpvelo, &
                       infoerqf, nonvelocstr, velorelcstr

! operator, methods:
  USE m_time,    ONLY: OPERATOR(.isIn.)
!  USE d_debug,   ONLY: debug_entry, debug_exit

! subroutines, functions:
  USE s_alcerr

! no implicit
  IMPLICIT NONE

! subroutine name
  CHARACTER(LEN=8), PARAMETER    :: srName = 'fodivcon'


! List of Arguments
! -----------------
! input:
  TYPE(t_opt)                    :: opt         ! Option structure
  TYPE(t_sCore)                  :: sCore       ! Core structure of FODITS
  INTEGER(i4b)                   :: iSta        ! Station index

! input/output:

! output:


! Local Types
! -----------
  TYPE t_events
     REAL(r8b)                         :: mjd
     INTEGER(i4b)                      :: type
     INTEGER(i4b)                      :: info
     CHARACTER(LEN=8)                  :: remark
  END TYPE t_events

  TYPE t_groups
     INTEGER(i4b)                      :: group
     REAL(r8b)                         :: deltaMjd
  END TYPE t_groups

! Local Parameters
! ----------------


! Local Variables
! ---------------
  TYPE(t_upd_ren)                :: tempRen

  INTEGER(i4b)                   :: iac
  INTEGER(i4b)                   :: iMjd
  INTEGER(i4b)                   :: nMjd
  INTEGER(i4b)                   :: iRen
  INTEGER(i4b)                   :: jRen
  INTEGER(i4b)                   :: kRen
  INTEGER(i4b)                   :: nRen
  INTEGER(i4b)                   :: seen
  INTEGER(i4b)                   :: iiRen
  INTEGER(i4b)                   :: selectedRen
  INTEGER(i4b)                   :: iEvnt
  INTEGER(i4b)                   :: jEvnt
  INTEGER(i4b)                   :: nGroup
  INTEGER(i4b)                   :: iGroup
  INTEGER(i4b)                   :: kGroup

  REAL(r8b)                      :: mjd
  REAL(r8b)                      :: maxDeltaMjd
  REAL(r8b)                      :: oneSecInDays
  REAL(r8b)                      :: oneMinInDays
  REAL(r8b)                      :: outlTimeHalf

  TYPE(t_events),DIMENSION(:), &
     ALLOCATABLE                 :: events

  TYPE(t_groups),DIMENSION(:), &
     ALLOCATABLE                 :: groups

! Call debug routine
! ------------------
!  CALL debug_entry(srName)


! Initialization of all variables
! -------------------------------
  nMjd = sCore%sta(iSta)%ts%nMjd

  ! Define 1 minute in days
  oneMinInDays = 1.0D0 / ( 60.0D0 * 24.0D0)
  ! Define 1 second in days
  oneSecInDays = oneMinInDays / 60.0D0
  ! Define the half interval for STA-File TYPE 003 (= 5 seconds)
  outlTimeHalf = 15*oneMinInDays
  ! Correction due to 3-days and weekly solutions
  IF     ( opt%updStaNDays == 3 )THEN
     outlTimeHalf = outlTimeHalf + 1.0D0
  ELSE IF( opt%updStaNDays == 7 )THEN
     outlTimeHalf = outlTimeHalf + 4.0D0
  END IF

  ! Allocation memory
  ALLOCATE(events(nMaxRenaming),stat=iac)
  CALL alcerr(iac,'events',(/nMaxRenaming/),srName)

  ! First interval
  nRen = 1
  events(nRen)%mjd  = 0.0D0
  events(nRen)%type = typeNone
  events(nRen)%info = infoInit
  events(nRen)%remark = 'BEGIN-TS'
  ! Add the contribution of the significant velocity changes
  DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
     IF( sCore%sta(iSta)%mod%evnt(iEvnt)%type /= typeVelo )CYCLE
     IF( sCore%sta(iSta)%mod%evnt(iEvnt)%info == infoInit )CYCLE
     IF( sCore%sta(iSta)%mod%evnt(iEvnt)%siTst == significant )THEN
        nRen = nRen + 1
        events(nRen)%mjd  = sCore%sta(iSta)%mod%evnt(iEvnt)%mjd
        events(nRen)%type = typeVelo
        events(nRen)%info = sCore%sta(iSta)%mod%evnt(iEvnt)%info
        events(nRen)%remark = sCore%sta(iSta)%mod%evnt(iEvnt)%remark
     END IF
  END DO
  ! Add the contribution of the significant jumps
  DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
     IF( sCore%sta(iSta)%mod%evnt(iEvnt)%type /= typeJump )CYCLE
     IF( sCore%sta(iSta)%mod%evnt(iEvnt)%info == infoInit )CYCLE
     IF( sCore%sta(iSta)%mod%evnt(iEvnt)%siTst /= significant )CYCLE
     seen = 0
     DO jEvnt = 1,nRen
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%mjd == events(jEvnt)%mjd )THEN
           events(jEvnt)%type = typeJumpVelo
           seen = 1
           EXIT
        END IF
     END DO
     IF( seen == 0 )THEN
        nRen = nRen + 1
        events(nRen)%mjd  = sCore%sta(iSta)%mod%evnt(iEvnt)%mjd
        events(nRen)%type = typeJump
        events(nRen)%info = sCore%sta(iSta)%mod%evnt(iEvnt)%info
        events(nRen)%remark = sCore%sta(iSta)%mod%evnt(iEvnt)%remark
     END IF
  END DO
  sCore%sta(iSta)%upd%nRen = nRen
  ! Allocation memory
  NULLIFY(sCore%sta(iSta)%upd%ren)
  ALLOCATE(sCore%sta(iSta)%upd%ren(nRen),stat=iac)
  CALL alcerr(iac,'sCore%...%ren',(/nRen/),srName)

  ! Synchronize the events to the observations (to avoid creating outliers)
  DO iRen = 1,nRen
     DO iMjd = 1,nMjd
        IF( sCore%sta(iSta)%ts%mjd(iMjd) < events(iRen)%mjd )CYCLE
        events(iRen)%mjd = sCore%sta(iSta)%ts%mjd(iMjd)
        EXIT
     END DO
  END DO

  ! Copy the events into %ren struct and make correction due to time%mean
  DO iRen = 1,nRen
     sCore%sta(iSta)%upd%ren(iRen)%timint%t(1) = events(iRen)%mjd
     sCore%sta(iSta)%upd%ren(iRen)%type = events(iRen)%type
     sCore%sta(iSta)%upd%ren(iRen)%info = events(iRen)%info
     sCore%sta(iSta)%upd%ren(iRen)%remark = events(iRen)%remark
  END DO
  ! Sort %ren struct in terms of time %mjd
  DO iRen = 1,nRen-1
     kRen = iRen
     DO jRen = iRen+1,nRen
        IF(  sCore%sta(iSta)%upd%ren(jRen)%timint%t(1) < &
             sCore%sta(iSta)%upd%ren(kRen)%timint%t(1) )THEN
           kRen = jRen
        END IF
     END DO
     IF( kRen /= iRen )THEN
        tempRen = sCore%sta(iSta)%upd%ren(iRen)
        sCore%sta(iSta)%upd%ren(iRen) = sCore%sta(iSta)%upd%ren(kRen)
        sCore%sta(iSta)%upd%ren(kRen) = tempRen
     END IF
  END DO
  ! Make %timint%t(:) of %ren consistent
  DO iRen = 1,nRen-1
     sCore%sta(iSta)%upd%ren(iRen)%timint%t(2) = &
          sCore%sta(iSta)%upd%ren(iRen+1)%timint%t(1)
  END DO
  sCore%sta(iSta)%upd%ren(1)%timint%t(1) = sCore%sta(iSta)%ts%mjd(1)
  sCore%sta(iSta)%upd%ren(nRen)%timint%t(2) = sCore%sta(iSta)%ts%mjd(nMjd)

  ! Remove one minute from t(1) and two minutes from t(2) for STA-file TYPE-001
  DO iRen = 1,nRen
     sCore%sta(iSta)%upd%ren(iRen)%timint%t(1) = &
          sCore%sta(iSta)%upd%ren(iRen)%timint%t(1) - outlTimeHalf
     sCore%sta(iSta)%upd%ren(iRen)%timint%t(2) = &
          sCore%sta(iSta)%upd%ren(iRen)%timint%t(2) - outlTimeHalf - &
                                                      oneSecInDays
  END DO

  ! Classify the station renaming into groups with the following rules:
  ! (1) Never set up relative velocity constraints after earthquake.
  ! (2) Set up relative velocity constraints after each equipment change.
  ALLOCATE(groups(nMaxRenaming),stat=iac)
  CALL alcerr(iac,'groups',(/nMaxRenaming/),srName)
  groups(:)%deltaMjd = 0.0D0
  nGroup = 1
  DO iRen = 1,nRen
     IF( sCore%sta(iSta)%upd%ren(iRen)%info == infoErqF .OR. &
         sCore%sta(iSta)%upd%ren(iRen)%type == typeVelo .OR. &
         sCore%sta(iSta)%upd%ren(iRen)%type == typeJumpVelo )THEN
        nGroup = nGroup + 1
     END IF
     sCore%sta(iSta)%upd%ren(iRen)%group = nGroup
     groups(nGroup)%deltaMjd = groups(nGroup)%deltaMjd + &
          ( sCore%sta(iSta)%upd%ren(iRen)%timint%t(2) - &
            sCore%sta(iSta)%upd%ren(iRen)%timint%t(1) )
     sCore%sta(iSta)%upd%ren(iRen)%groupLength = groups(nGroup)%deltaMjd
  END DO
  maxDeltaMjd = sCore%sta(iSta)%upd%ren(nRen)%groupLength
  DO iRen = nRen-1,1,-1
     IF( sCore%sta(iSta)%upd%ren(iRen)%group /= &
         sCore%sta(iSta)%upd%ren(iRen+1)%group )THEN
        maxDeltaMjd = sCore%sta(iSta)%upd%ren(iRen)%groupLength
     END IF
     sCore%sta(iSta)%upd%ren(iRen)%groupLength = maxDeltaMjd
  END DO

  ! Compute the number of data in group of sub-intervals with same
  ! relative velocity constraints
  iGroup = 0
  kGroup = 0
  DO iRen = 1,nRen
     IF( iGroup /= sCore%sta(iSta)%upd%ren(iRen)%group )THEN
        iGroup = sCore%sta(iSta)%upd%ren(iRen)%group
        kGroup = 0
     END IF
     DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
        mjd = sCore%sta(iSta)%ts%mjd(iMjd)
        IF( mjd < sCore%sta(iSta)%upd%ren(iRen)%timint%t(1) )CYCLE
        IF( mjd > sCore%sta(iSta)%upd%ren(iRen)%timint%t(2) )EXIT
        kGroup = kGroup + 1
     END DO
     sCore%sta(iSta)%upd%ren(iRen)%groupNumData = kGroup
  END DO
  nMjd = sCore%sta(iSta)%upd%ren(nRen)%groupNumData
  DO iRen = nRen-1,1,-1
     IF( sCore%sta(iSta)%upd%ren(iRen)%group /= &
         sCore%sta(iSta)%upd%ren(iRen+1)%group )THEN
        nMjd = sCore%sta(iSta)%upd%ren(iRen)%groupNumData
     END IF
     sCore%sta(iSta)%upd%ren(iRen)%groupNumData = nMjd
  END DO

  ! Set up relative velocity constraints on velocities (w.r.t. the first
  ! interval of each group)
  sCore%sta(iSta)%upd%ren(:)%cnstr = nonVeloCstr
  DO iRen = 2,nRen
     IF( sCore%sta(iSta)%upd%ren(iRen-1)%group == &
         sCore%sta(iSta)%upd%ren(iRen)%group )THEN
        sCore%sta(iSta)%upd%ren(iRen)%cnstr = veloRelCstr
     END IF
  END DO

  ! Offset selection: group zero contain the referece Epoch
  selectedRen = 1
  DO iRen = 1,nRen
     IF( sCore%outTimRefCrd >= sCore%sta(iSta)%upd%ren(iRen)%timint%t(1) .AND.&
         sCore%outTimRefCrd <  sCore%sta(iSta)%upd%ren(iRen)%timint%t(2) )THEN
        selectedRen = iRen
        EXIT
     END IF
  END DO

  ! Rename the stations
  DO iRen = 1,nRen
     iiRen = iRen-selectedRen
     CALL fodivcon_defStaRen(iiRen,sCore%sta(iSta)%upd%ren(iRen)%rename)
  END DO

  ! Deallocate events
  DEALLOCATE(events,stat=iac)
  DEALLOCATE(groups,stat=iac)

! End of subroutine
! -----------------
!  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodivcon

! -------------------------------------------------------------------------
! Define station postfix
! -------------------------------------------------------------------------
  ! 'ZZ' = defStaRen(-1),
  ! '  ' = defStaRen( 0),
  ! 'AA' = defStaRen( 1),
  ! 'AB' = defStaRen( 2)
SUBROUTINE fodivcon_defStaRen( iiRen, defStaRen)

  USE m_bern,   ONLY: i4b

  IMPLICIT NONE

  ! Function arguments
  ! ------------------
  INTEGER(i4b),INTENT(IN)                  :: iiRen
  CHARACTER(LEN=2)                         :: defStaRen
  ! Local Parameters
  ! ---------------
  INTEGER(i4b),PARAMETER                   :: zetaNum = 26
  ! Local Variables
  ! ---------------
  INTEGER(i4b),DIMENSION(2)                :: decNum
  CHARACTER(LEN=1),DIMENSION(2)            :: suR
  INTEGER(i4b)                             :: iCnt
  INTEGER(i4b)                             :: staPostfixNum
  ! Function
  ! --------
  suR(1) = ' '
  suR(2) = ' '
  staPostfixNum = iiRen
  IF( staPostfixNum > 0 )THEN
     staPostfixNum = staPostfixNum - 1
     decNum(1) = INT(staPostfixNum / zetaNum)
     suR(1) = 'A'
     DO iCnt = 1,decNum(1)
        suR(1) = CHAR(ICHAR(suR(1))+1)
     END DO
     decNum(2) = MOD(staPostfixNum , zetaNum)
     suR(2) = 'A'
     DO iCnt = 1,decNum(2)
        suR(2) = CHAR(ICHAR(suR(2))+1)
     END DO
  ELSE IF( staPostfixNum < 0 )THEN
     staPostfixNum = -staPostfixNum - 1
     decNum(1) = INT(staPostfixNum / zetaNum)
     suR(1) = 'Z'
     DO iCnt = 1,decNum(1)
        suR(1) = CHAR(ICHAR(suR(1))-1)
     END DO
     decNum(2) = MOD(staPostfixNum , zetaNum)
     suR(2) = CHAR(ICHAR('Z'))
     DO iCnt = 1,decNum(2)
        suR(2) = CHAR(ICHAR(suR(2))-1)
     END DO
  END IF
  WRITE(defStaRen,'(A,A)') suR(1), suR(2)

END SUBROUTINE fodivcon_defStaRen

END MODULE s_FODIVCON
