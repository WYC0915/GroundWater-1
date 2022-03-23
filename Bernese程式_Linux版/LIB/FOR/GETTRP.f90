MODULE s_GETTRP
CONTAINS

! --------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! --------------------------------------------------------------------------

SUBROUTINE gettrp(filNam,epoch,staNam,iStop,iMod,iTrpMd,iTrMap,            &
                  iTrGrd,drTrop,irCode)

! --------------------------------------------------------------------------
! Purpose:    Get troposphere correction for one epoch and one station
!             from a BERNESE troposphere file (either piecewise constant
!             or linear). The file is read on first call, all arrays
!             may be deallocated.
!
! Author:     M. Meindl
!
! Created:    07-May-2003
!
! Changes:    12-Jun-2003 MM: Bugfix wrt unallocated array
!             18-Jun-2003 MM: Bugfix wrt drTrop(3)=99.99d0
!             07-Jul-2003 MM: New option iMod (return total UP instead of
!                             Correction)
!             11-Dec-2003 MM: Ignore parameters with same epoch
!             22-Sep-2005 MM: Allocate prtsta with maxsta
!             30-Jun-2008 RD: VMF added
!             19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! --------------------------------------------------------------------------


! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, &
                      fileNameLength, staNameLength
  USE m_maxdim, ONLY: maxSta
  USE d_trpest, ONLY: t_trpEst, dtMax

  USE s_alcerr
  USE f_parfac
  USE s_rdtrpe
  USE s_exitrc
  USE s_dimtst
  IMPLICIT NONE


! List of parameters
! ------------------
! input
  CHARACTER(LEN=fileNameLength) :: filNam  ! Name of troposphere file
                                           ! =blank: use keyword TROPEST
  REAL(r8b)                     :: epoch   ! Requested epoch
  CHARACTER(LEN=16)             :: staNam  ! Requested station
  INTEGER(i4b)                  :: iStop   ! =-1: deallocate all arrays
                                           ! = 0: no stop, use irCode
                                           ! = 1: stop on error
                                           ! = 2: no stop, print warning
  INTEGER(i4b)                  :: iMod    ! = 0: return correction up
                                           ! = 1: return total up
! output
  INTEGER(i4b)                  :: iTrpMd  ! Troposphere model
  INTEGER(i4b)                  :: iTrMap  ! Mapping function
                                           ! = 1: 1/COS(Z)
                                           ! = 2: HOPFIELD
                                           ! = 3,4: DRY/WET NIELL
                                           ! = 5,6: DRY/WET GMF
                                           ! = 7,8: DRY/WET VMF
  INTEGER(i4b)                  :: iTrGrd  ! Gradient model
                                           ! = 0: not estimated
                                           ! = 1: TILTING
                                           ! = 2: LINEAR
  REAL(r8b),DIMENSION(3)        :: drTrop  ! Tropospheric correction (m)
                                           ! (north, east, up)
  INTEGER(i4b)                  :: irCode  ! Return code
                                           ! = 0: ok
                                           ! = 1: station not found
                                           ! = 2: station in file, w/o tropo

! Local variables
! ---------------
! general
  TYPE(t_trpEst), SAVE                         :: trpEst
  CHARACTER(LEN=fileNameLength), SAVE          :: filTrp
  CHARACTER(LEN=staNameLength),                                            &
               DIMENSION(:), ALLOCATABLE, SAVE :: prtSta
  LOGICAL, SAVE                                :: filFlg = .FALSE.
  LOGICAL, SAVE                                :: first  = .TRUE.
  INTEGER(i4b), SAVE                           :: nPrtSta
  LOGICAL                                      :: found
  LOGICAL                                      :: prtWarn

! interpolation
  REAL(r8b)                                    :: t1, t2
  REAL(r8b)                                    :: xFact
  REAL(r8b),DIMENSION(3)                       :: corr1, corr2

! indices and loop variables
  INTEGER(i4b)                                 :: idx, iSta
  INTEGER(i4b)                                 :: iTrp, iTrp1

! error codes
  INTEGER(i4b)                                 :: irc, iac



! Some initializations
! --------------------
  irCode = 0
  drTrop = 0.d0


! Deallocate arrays on request
! ----------------------------
  IF (iStop==-1 .AND. filFlg) THEN
    DO iSta=1,trpEst%nSta
      DEALLOCATE(trpEst%sta(iSta)%trp)
    END DO
    DEALLOCATE(trpEst%sta)
    NULLIFY(trpEst%sta)
    IF (ALLOCATED(prtSta)) DEALLOCATE(prtSta)
    filTrp = " "
    filFlg = .FALSE.
    first  = .TRUE.
    RETURN
  END IF


! Read troposphere file on first call
! -----------------------------------
  IF (first) THEN
    CALL rdtrpe(filNam,trpEst,irc)
    IF (irc==0) THEN
      filFlg = .TRUE.

! Allocate array for warnings
      ALLOCATE(prtSta(maxSta),STAT=iac)
      CALL alcerr(iac,'prtSta',(/maxSta/),'sr gettrp')
    END IF

    filTrp  = filNam
    nPrtSta = 0
    first   = .FALSE.
  END IF


! Get troposphere correction for epoch and station
! ------------------------------------------------
  IF (filFlg) THEN

! model, mapping, and gradient
    iTrpMd = trpEst%head%iTrpMd
    iTrMap = trpEst%head%iTrMap
    iTrGrd = trpEst%head%iTrGrd(1)

! find station in file
    found = .FALSE.
    DO iSta=1,trpEst%nSta
      IF (trpEst%sta(iSta)%staNam==staNam) THEN
        found = .TRUE.
        idx   = iSta
      END IF
    END DO


! Station not found in file
! -------------------------
    IF (.NOT.found) THEN
      irCode = 1

! no stop
      IF (iStop==0) THEN

! stop
      ELSE IF (iStop==1) THEN
        WRITE(lfnerr,"(/,' *** SR GETTRP: No troposphere corrections found', &
                     & /,'                Station : ',A,     &
                     & /,'                File    : ',A,/)") &
                     TRIM(staNam),TRIM(filTrp)
        CALL exitrc(2)

! no stop but warning (once per station)
      ELSE
        prtWarn = .TRUE.
        DO iSta=1,nPrtSta
          IF (prtSta(iSta)==staNam) prtWarn = .FALSE.
        END DO
        IF (prtWarn) THEN
          WRITE(lfnerr,"(/,' ### SR GETTRP: No troposphere corrections found',&
                       & /,'                Station : ',A,     &
                       & /,'                File    : ',A,/)") &
                       TRIM(staNam),TRIM(filTrp)
          nPrtSta         = nPrtSta+1
          CALL DIMTST(1,2,2,'GETTRP','maxSta','STATIONS MISSING IN TRP FILE',&
                      '',nPrtSta,maxSta,irc)
          prtSta(nPrtSta) = staNam
        END IF
      END IF

! return
      RETURN
    END IF


! Station found in file
! ---------------------
    drTrop(3) = 99.99d0

! piecewise constant
    IF (trpEst%head%iTab==0.d0) THEN
      DO iTrp=1,trpEst%sta(idx)%nTrp
        t1 = trpEst%sta(idx)%trp(iTrp)%timInt(1)
        t2 = trpEst%sta(idx)%trp(iTrp)%timInt(2)

! special handling of "boundary problem"
        IF (iTrp==1) t1=t1-dTMax
        IF (iTrp==trpEst%sta(idx)%nTrp) t2=t2+dTMax
        IF (epoch>=t1 .AND. epoch<t2) THEN
          drTrop = trpEst%sta(idx)%trp(iTrp)%corr
          IF (iMod==1) drTrop(3) = trpEst%sta(idx)%trp(iTrp)%total
          EXIT
        END IF
      END DO

! piecewise linear
    ELSE

! loop over all troposphere parameters
      DO iTrp=1,trpEst%sta(idx)%nTrp
        iTrp1 = iTrp+1
        IF (iTrp==trpEst%sta(idx)%nTrp) iTrp1 = iTrp     ! last parameter
        t1    = trpEst%sta(idx)%trp(iTrp)%timInt(1)
        t2    = trpEst%sta(idx)%trp(iTrp1)%timInt(1)
        xFact = parfac(epoch,t1,trpEst%head%iTab,dtMax)

! same epoch, parameter spacing too big, trpEpo out of window, or xFact=0
        IF (iTrp /= iTrp1 .AND. t1 == t2) CYCLE
        IF (t2-t1>trpEst%head%iTab+dTmax .AND. xFact/=1.d0)  CYCLE
        IF (epoch<t1-dtMax)                CYCLE
        IF (xFact/=1.d0 .AND. iTrp==iTrp1) CYCLE
        IF (xFact==0.d0)                   CYCLE

! compute parameter and rms
        corr1  = trpEst%sta(idx)%trp(iTrp)%corr
        corr2  = trpEst%sta(idx)%trp(iTrp1)%corr
        IF (iMod==1) THEN
          corr1(3) = trpEst%sta(idx)%trp(iTrp)%total
          corr2(3) = trpEst%sta(idx)%trp(iTrp1)%total
        END IF
        drTrop = corr1*xFact+corr2*(1.d0-xFact)
        EXIT
      END DO
    END IF


! No suitable interval found
! --------------------------
    IF (drTrop(3)==99.99d0) THEN
      drTrop(3) = 0.d0
      irCode = 2
      IF (iStop>0) THEN
        write(lfnerr,"(/,' ### SR GETTRP: No suitable interval found',   &
                     & /,'                Station              :',A16,   &
                     & /,'                Epoch request (mjd)  :',F10.4, &
                     & /,'                First available epoch:',F10.4, &
                     & /,'                Last available epoch :',F10.4,/)")&
                 TRIM(trpEst%sta(idx)%staNam),epoch,                        &
                 trpEst%sta(idx)%trp(1)%timInt(1),   &
                 trpEst%sta(idx)%trp(trpEst%sta(idx)%nTrp)%timInt(2)
      ENDIF
    END IF
  END IF


! Subroutine ends here
! --------------------
  RETURN
END SUBROUTINE gettrp

END MODULE
