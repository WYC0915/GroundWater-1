MODULE s_MCNEXT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE mcnext(iSta,nNext,nSta,station,dist,nextSta)


! -------------------------------------------------------------------------
! Purpose:    Find the nearest nNext stations to station iSta
!
! Author:     R. Dach
!
!
! Created:    13-Jun-2002
! Last mod.:  13-Jun-2002
!
! Changes:    __-___-____ __:
!
! SR called:  alcerr,ikf
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE p_mkclus, ONLY: t_station
  USE f_ikf
  USE s_alcerr
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  INTEGER(i4b)                         :: iSta     ! Index of actual station
  INTEGER(i4b)                         :: nNext    ! Number of stations to be
                                                   ! found for list nextSta
  INTEGER(i4b)                         :: nSta     ! Number of stations
  TYPE(t_station),DIMENSION(:),POINTER :: station  ! Station information record
  REAL(r8b),      DIMENSION(:),POINTER :: dist     ! Array with dist. between
                                                   ! stations

! output:
  INTEGER(i4b),   DIMENSION(:),POINTER :: nextSta  ! Index to nearest stations

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER          :: srName = 'mcnext'

! Local Variables
! ---------------
  INTEGER(i4b)                         :: jSta,mSta
  INTEGER(i4b)                         :: iNxt
  INTEGER(i4b)                         :: irc

! Init the index
! --------------
  DEALLOCATE(nextSta,stat=irc)

  ALLOCATE(nextSta(nNext),stat=irc)
  CALL alcerr(irc,'nextSta',(/nNext/),srName)

  nextSta = 0

! Loop all stations
! -----------------
  DO jSta = 1,nSta

    IF (jSta == iSta) CYCLE

! Search for the nearst stations
! ------------------------------
    mSta = 0
    DO iNxt = 1,nNext

      IF (nextSta(iNxt) == 0) THEN
        mSta = jSta
        EXIT
      ENDIF

      IF (dist(IKF(iSta,jSta)) < dist(IKF(iSta,nextSta(iNxt)))) THEN
        mSta = jSta
        EXIT
      ENDIF

    ENDDO

    IF (mSta == 0) CYCLE

! Order the dist. in index list
! -----------------------------
    DO iNxt = nNext,1,-1

      IF (nextSta(iNxt) == 0) THEN
        mSta = iNxt
        CYCLE
      ENDIF

      IF (dist(IKF(iSta,jSta)) < dist(IKF(iSta,nextSta(iNxt)))) &
        mSta = iNxt

    ENDDO

! Put new station in list
! -----------------------
    IF (mSta < nNext)                      &
      nextSta(mSta+1:nNext) = nextSta(mSta:nNext-1)

    nextSta(mSta) = jSta

  ENDDO


  RETURN
END SUBROUTINE mcnext

END MODULE
