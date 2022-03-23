MODULE s_MCDIST
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE mcdist(nSta,station,dist)

! -------------------------------------------------------------------------
! Purpose:    Compute distances between the station
!
! Author:     R. Dach
!
!
! Created:    13-Jun-2002
! Last mod.:  04-Feb-2011
!
! Changes:    27-Feb-2003 HU: DATUM from D_DATUM
!             23-Apr-2003 AJ: Nullify local pointers
!             04-Feb-2011 RD: GETSTA is used as a module now
!
! SR called:  alcerr, getsta
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE p_mkclus, ONLY: t_station
  USE d_datum,  ONLY: t_datum
  USE f_ikf
  USE s_alcerr
  USE s_getsta
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  INTEGER(i4b)                         :: nSta     ! Number of stations
  TYPE(t_station),DIMENSION(:),POINTER :: station  ! Station information record

! output
  REAL(r8b),      DIMENSION(:),POINTER :: dist     ! Array with dist. between
                                                   ! stations

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER                :: srName = 'mcdist'

! Local Variables
! ---------------
  TYPE(t_datum)                              :: datum

  CHARACTER(LEN=staNameLength),               &
                   DIMENSION(:), POINTER     :: stName

  INTEGER(i4b)                               :: nCentr
  INTEGER(i4b),    DIMENSION(:), POINTER     :: staNum,iCentr
  INTEGER(i4b)                               :: iSta,jSta
  INTEGER(i4b)                               :: ii,ij
  INTEGER(i4b)                               :: irc

  REAL(r8b),       DIMENSION(:,:), POINTER   :: xStat,xStell,xStecc

  LOGICAL,         SAVE                      :: first = .TRUE.


! Read the station coordinates
! ----------------------------
  IF (first) THEN

    NULLIFY(stName)
    NULLIFY(staNum)
    NULLIFY(iCentr)
    NULLIFY(xStat)
    NULLIFY(xStell)
    NULLIFY(xStecc)

    ! Allocate the station name array
    ALLOCATE(stName(nSta),stat=irc)
    CALL alcerr(irc,'stName',(/nSta/),srName)

    ! Put stations in list
    stName(1:nSta) = station(1:nSta)%stanam

    ! Allocate the other arrays
    ALLOCATE(staNum(nSta),stat=irc)
    CALL alcerr(irc,'staNum',(/nSta/),srName)

    ALLOCATE(iCentr(nSta),stat=irc)
    CALL alcerr(irc,'iCentr',(/nSta/),srName)

    ALLOCATE(xStat(3,nSta),stat=irc)
    CALL alcerr(irc,'xStat',(/3,nSta/),srName)

    ALLOCATE(xStell(3,nSta),stat=irc)
    CALL alcerr(irc,'xStell',(/3,nSta/),srName)

    ALLOCATE(xStecc(3,nSta),stat=irc)
    CALL alcerr(irc,'xStecc',(/3,nSta/),srName)

    CALL getsta(nSta,stName,staNum,nCentr,iCentr,xStat,xStell,xStecc,     &
                datum%name,datum%aEll,datum%bEll,datum%dxEll,datum%drEll, &
                datum%scEll)

    DO iSta = 1,nSta

      station(iSta)%lb(1) = xStell(1,iSta)   ! lat.  (rad)
      station(iSta)%lb(2) = xStell(2,iSta)   ! long. (rad)

      station(iSta)%lb(3) = 0
      DO ii = 1,3
        station(iSta)%lb(3) = station(iSta)%lb(3) + xStat(ii,iSta)**2
      ENDDO
      station(iSta)%lb(3) = DSQRT(station(iSta)%lb(3)) - &
                              xStell(3,iSta) ! radius
    ENDDO

! Deallocate memory
! -----------------
    DEALLOCATE(stName,stat=irc)
    DEALLOCATE(staNum,stat=irc)
    DEALLOCATE(iCentr,stat=irc)
    DEALLOCATE(xStat ,stat=irc)
    DEALLOCATE(xStell,stat=irc)
    DEALLOCATE(xStecc,stat=irc)

    first = .FALSE.

  ENDIF

! Allocate distance array
! -----------------------
  DEALLOCATE(dist, stat=irc)

  ALLOCATE(dist(IKF(nSta,nSta)), stat=irc)
  CALL alcerr(irc,'dist',(/ IKF(nSta,nSta) /),srName)

  dist = 0d0

! Loop all stations
! -----------------
  DO iSta = 1,nSta-1
    DO jSta = iSta+1,nSta

      ij = IKF(iSta,jSta)

      dist(ij) = 0d0


! Spherical COS formula:
       dist(ij) = DSIN(station(iSta)%lb(1))*DSIN(station(jSta)%lb(1)) + &
                  DCOS(station(iSta)%lb(1))*DCOS(station(jSta)%lb(1))*  &
                  DCOS(station(iSta)%lb(2)-station(jSta)%lb(2))

       dist(ij) = DNINT(dist(ij)*1d12)/1d12  ! to prevent rounding problems

       dist(ij) = DACOS(dist(ij))*&
                  (station(iSta)%lb(3)+station(jSta)%lb(3))/2d0
    ENDDO
  ENDDO

  RETURN
END SUBROUTINE mcdist

END MODULE
