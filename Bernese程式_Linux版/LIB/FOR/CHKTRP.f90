MODULE s_CHKTRP
CONTAINS

! --------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! --------------------------------------------------------------------------

SUBROUTINE chktrp(iMod,irCode,filNam,timInt,staNam)

! --------------------------------------------------------------------------
! Purpose:    Creates and checks a list of stations for which all
!             troposphere parameters are available.
!             If called without prior initialization, SR does nothing.
!             Used in SR RDITRP and METEO for GPSEST option "WITH_TROP"
!
! Author:     M. Meindl
!
! Created:    20-Jun-2003
!
! Changes:    15-Jul-2003 mm: Small bugfix
!             31-Jul-2003 mm: Small bugfix
!             27-Mar-2012 RD: Use LISTC1 as module now
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! --------------------------------------------------------------------------


! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, staNameLength, fileNameLength
  USE d_trpest, ONLY: t_trpEst
  USE m_time,   ONLY: t_timint

  USE s_alcerr
  USE s_rdtrpe
  USE f_listc1
  IMPLICIT NONE

! List of parameters
! ------------------
! input
  INTEGER(i4b)                            :: iMod    ! Mode:
                                                     !  0=initialize
                                                     !  1=check a station
  CHARACTER(LEN=fileNameLength), OPTIONAL :: filNam  ! Name of trop file
                                                     ! =blank: use TROPEST
                                                     ! (used for iMod=0)
  TYPE(t_timint), OPTIONAL                :: timInt  ! Time window to check
                                                     ! (used for iMod=0)
  CHARACTER(LEN=staNameLength), OPTIONAL  :: staNam  ! Station to check
                                                     ! (used for iMod=1)

! output
  INTEGER(i4b)                            :: irCode  ! Return code:
                                                     !  0=staion ok
                                                     !  1=station not ok


! Local variables
! ---------------
! general
  TYPE(t_trpEst)                               :: trpEst
  CHARACTER(LEN=staNameLength),&
               DIMENSION(:), ALLOCATABLE, SAVE :: okSta
  INTEGER(i4b), SAVE                           :: nList
  LOGICAL, SAVE                                :: first = .TRUE.
  LOGICAL, SAVE                                :: init  = .FALSE.
  LOGICAL, SAVE                                :: wTrp  = .FALSE.

! loop variables
  INTEGER(i4b)                                 :: iSta, iTrp

! tropo parameters
  CHARACTER(LEN=staNameLength)                 :: trpSta
  REAL(r8b)                                    :: iTab
  INTEGER(i4b)                                 :: n1, n2
  INTEGER(i4b)                                 :: numTrp
  INTEGER(i4b)                                 :: iPos


! error and return codes
  INTEGER(i4b)                                 :: irc, iac




! First call
! ----------
  IF (first) THEN
    first = .FALSE.

! first call is initialization
    IF (iMod==0) THEN
      init = .TRUE.

! read tropo file
      CALL rdtrpe(filNam,trpEst,irc)
      IF (irc==0) wTrp =.TRUE.
    END IF

! Allocate some memory
! --------------------
    IF (wTrp) THEN
      ALLOCATE(okSta(trpEst%nSta),stat=iac)
      CALL alcerr(iac,'okSta',(/trpEst%nSta/),'sr chktrp')


! Check stations
! --------------
      DO iSta=1,trpEst%nSta
        trpSta = trpEst%sta(iSta)%staNam
        n1 = trpEst%sta(iSta)%nTrp
        n2 = 1

! find first and last value
        DO iTrp=1,trpEst%sta(iSta)%nTrp
          IF (trpEst%sta(iSta)%trp(iTrp)%timInt(1)<=timInt%t(1)) n1 = iTrp
        END DO
        DO iTrp=trpEst%sta(iSta)%nTrp,1,-1
          IF (trpEst%head%iTab==0.d0) THEN
            IF (trpEst%sta(iSta)%trp(iTrp)%timInt(2)>=timInt%t(2)) n2 = iTrp
          ELSE
            IF (trpEst%sta(iSta)%trp(iTrp)%timInt(1)>=timInt%t(2)) n2 = iTrp
          END IF
        END DO

        IF (n1>n2) CYCLE

! check if all parameters are available
        IF (trpEst%head%iTab==0.d0) THEN
          iTab = trpEst%sta(iSta)%trp(n1)%timInt(2)-                       &
                 trpEst%sta(iSta)%trp(n1)%timInt(1)
        ELSE
          iTab = trpEst%head%iTab
        END IF

        numTrp = NINT((trpEst%sta(iSta)%trp(n2)%timInt(1)-                 &
                       trpEst%sta(iSta)%trp(n1)%timInt(1))/iTab)

        IF (numTrp==0 .AND. trpEst%head%iTab/=0.d0) CYCLE

        IF(numTrp==n2-n1) THEN
          iPos = listc1(1,staNameLength,trpEst%nSta,okSta,trpSta,nList)
        END IF
      END DO


! Free some memory
! ----------------
      DO iSta=1,trpEst%nSta
        DEALLOCATE(trpEst%sta(iSta)%trp,stat=iac)
      END DO
      DEALLOCATE(trpEst%sta,stat=iac)
    END IF

    irCode = 0
    RETURN
  END IF


! Check stations
! --------------
! not initialized, do nothing
  IF (.NOT. init) THEN
    irCode = 0
    RETURN
  END IF

! no tropo file, station is marked as bad
  IF (.NOT. wTrp) THEN
    irCode = 1
    RETURN
  END IF

! initialized and file available
  iPos = listc1(0,staNameLength,nList,okSta,staNam,nList)
  IF (iPos/=0) THEN
    irCode = 0
  ELSE
    irCode = 1
  END IF


! Subroutine ends here
! --------------------
  RETURN
END SUBROUTINE chktrp

END MODULE
