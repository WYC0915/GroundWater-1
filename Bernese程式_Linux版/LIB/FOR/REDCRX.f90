MODULE s_REDCRX
CONTAINS

! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

SUBROUTINE redcrx(limits,parNam,renamList,delList)

! ------------------------------------------------------------------------------
! Purpose:    Reduce staInfo structure from p_addneq for further use.
!
! Author:     S.Lutz
!
! Created:    17-Nov-2011
!
! Changes:    23-Nov-2011 SL: use delList from isstacrx for Type 003
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, staNameLength
  USE m_time,   ONLY: t_timint
  USE p_addneq, ONLY: staInfo
  IMPLICIT NONE

! List of Parameters
! ------------------
! input
  TYPE(t_timint), DIMENSION(:)             :: limits
  CHARACTER(LEN=staNameLength), DIMENSION(:) :: parNam
  INTEGER(i4b), DIMENSION(:)               :: renamList
  INTEGER(i4b), DIMENSION(:,:)             :: delList

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER              :: srName = 'REDCRX'

! Local variables
! ---------------
  REAL(r8b)                                :: tMin, tMax
  INTEGER(i4b)                             :: nRed, iSta
  INTEGER(i4b)                             :: iTyp, iInfo, iNam
  LOGICAL                                  :: verb = .FALSE.     ! verbose mode

  IF(verb) WRITE(*,*) ''

! Get tMin and tMax from limits
! -----------------------------
  tMin = HUGE(0d0)
  tMax = 0d0
  DO iTyp=1,SIZE(limits)
    IF(limits(iTyp)%t(1)<tMin) tMin = limits(iTyp)%t(1)
    IF(limits(iTyp)%t(2)>tMax) tMax = limits(iTyp)%t(2)
  ENDDO
  IF(verb) WRITE(*,*) srName,' tMin =',tMin,' tMax =',tMax

! Type 001
! --------
  nRed=0
  DO iInfo=1,staInfo%nRenam
! Ignore wildcards in old names
    IF(INDEX(staInfo%renamSta(iInfo)%oldnam,'*') > 0 .OR. &
       INDEX(staInfo%renamSta(iInfo)%oldnam,'?') > 0 .OR. &
       INDEX(staInfo%renamSta(iInfo)%oldnam,'%') > 0) CYCLE
! Ignore epochs outside time interval
    IF(staInfo%renamSta(iInfo)%timint%t(2) < tMin .OR. &
       staInfo%renamSta(iInfo)%timint%t(1) > tMax) THEN
      CYCLE
    ENDIF
! Test station name
    iSta=0
    IF(renamList(iInfo)>0) THEN
      iSta=1
    ENDIF
    IF(iSta == 0) CYCLE
! Sort staInfo structure
    nRed=nRed+1
    IF(nRed /= iInfo) THEN
      staInfo%renamSta(nRed) = staInfo%renamSta(iInfo)
    ENDIF
  ENDDO
! Update nRenam
  IF(verb) WRITE(*,*) srName,' nRenam: ',staInfo%nRenam,'->',nRed
  staInfo%nRenam = nRed

! Type 002
! --------
  nRed=0
  DO iInfo=1,staInfo%nInfo
    IF(staInfo%staInfo(iInfo)%timint%t(2) < tMin .OR. &
       staInfo%staInfo(iInfo)%timint%t(1) > tMax) THEN
      CYCLE
    ENDIF
    iSta=0
    DO iNam=1,SIZE(parNam)
      IF(staInfo%staInfo(iInfo)%staNam == parNam(iNam)) THEN
        iSta=SIZE(parNam)
      ENDIF
    ENDDO
    IF(iSta == 0) CYCLE
    nRed=nRed+1
    IF(nRed /= iInfo) THEN
      staInfo%staInfo(nRed) = staInfo%staInfo(iInfo)
    ENDIF
  ENDDO
  IF(verb) WRITE(*,*) srName,' nInfo: ',staInfo%nInfo,'->',nRed
  staInfo%nInfo = nRed

! Type 003
! --------
  nRed=0
  DO iInfo=1,staInfo%nProb
    iSta=0
    DO iTyp=1,SIZE(delList,2)
      IF(delList(iInfo,iTyp)>0) THEN
        iSta=SIZE(delList,2)
      ENDIF
    ENDDO
    IF(iSta == 0) CYCLE
    nRed=nRed+1
    IF(nRed /= iInfo) THEN
      staInfo%staProb(nRed) = staInfo%staProb(iInfo)
    ENDIF
  ENDDO
  IF(verb) WRITE(*,*) srName,' nProb: ',staInfo%nProb,'->',nRed
  staInfo%nProb = nRed

! Type 004
! --------
  nRed=0
  DO iInfo=1,staInfo%nCoovel
    iSta=0
    DO iNam=1,SIZE(parNam)
      IF(staInfo%coovel(iInfo)%staNam(1) == parNam(iNam) .OR. &
         staInfo%coovel(iInfo)%staNam(2) == parNam(iNam)) THEN
        iSta=SIZE(parNam)
      ENDIF
    ENDDO
    IF(iSta == 0) CYCLE
    nRed=nRed+1
    IF(nRed /= iInfo) THEN
      staInfo%coovel(nRed) = staInfo%coovel(iInfo)
    ENDIF
  ENDDO
  IF(verb) WRITE(*,*) srName,' nCoovel: ',staInfo%nCoovel,'->',nRed
  staInfo%nCoovel = nRed

  nRed=0
  DO iInfo=1,staInfo%nRelPar
    IF(staInfo%staRelPar(iInfo)%timWin%t(2) < tMin .OR. &
       staInfo%staRelPar(iInfo)%timWin%t(1) > tMax) THEN
      CYCLE
    ENDIF
    iSta=0
    DO iNam=1,SIZE(parNam)
      IF(staInfo%staRelPar(iInfo)%staNam(1) == parNam(iNam) .OR. &
         staInfo%staRelPar(iInfo)%staNam(2) == parNam(iNam)) THEN
        iSta=SIZE(parNam)
      ENDIF
    ENDDO
    IF(iSta == 0) CYCLE
    nRed=nRed+1
    IF(nRed /= iInfo) THEN
      staInfo%staRelPar(nRed) = staInfo%staRelPar(iInfo)
    ENDIF
  ENDDO
  IF(verb) WRITE(*,*) srName,' nRelPar: ',staInfo%nRelPar,'->',nRed
  staInfo%nRelPar = nRed

! Type 005
! --------
  nRed=0
  DO iInfo=1,staInfo%nStaType
    IF(staInfo%staType(iInfo)%timint%t(2) < tMin .OR. &
       staInfo%staType(iInfo)%timint%t(1) > tMax) THEN
      CYCLE
    ENDIF
    iSta=0
    DO iNam=1,SIZE(parNam)
      IF(staInfo%staType(iInfo)%staNam == parNam(iNam)) THEN
        iSta=SIZE(parNam)
      ENDIF
    ENDDO
    IF(iSta == 0) CYCLE
    nRed=nRed+1
    IF(nRed /= iInfo) THEN
      staInfo%staType(nRed) = staInfo%staType(iInfo)
    ENDIF
  ENDDO
  IF(verb) WRITE(*,*) srName,' nStaType: ',staInfo%nStaType,'->',nRed
  staInfo%nStaType = nRed

  RETURN

END SUBROUTINE redcrx

END MODULE
