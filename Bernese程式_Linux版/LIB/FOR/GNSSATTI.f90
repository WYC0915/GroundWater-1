MODULE s_gnssatti
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE gnssatti(iorsys, numsat, tj ,iEarth, xsat, &
                    ez_sat, ey_sat, ex_sat)

! -------------------------------------------------------------------------
! Purpose:    Compute the attitute for GNSS satellites
!
! Author:     R. Dach
!
! Created:    11-Oct-2011
! Last mod.:  11-Nov-2011
!
! Changes:    11-Nov-2011 MM: Transformation of xSun corrected
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_maxdim,  ONLY: MAXSAT

  USE s_cootra
  USE s_suneff
  USE s_vprod
  IMPLICIT NONE

  INCLUDE 'COMFREQ.inc'

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                      :: iorsys  ! ORBIT SYSTEM
                                               ! =1 : B1950.0
                                               ! =2 : J2000.0
  INTEGER(i4b)                      :: numsat  ! Satellite number
  REAL(r8b)                         :: tj      ! Epoch
  INTEGER(i4b)                      :: iEarth  ! System of the satellite vector
                                               ! =0: inertial
                                               ! =1: Earth fixed
  REAL(r8b), DIMENSION(6)           :: xsat    ! satellite position/velocity

! output:
  REAL(r8b), DIMENSION(3), OPTIONAL :: ex_sat  ! Attitude unit vector in X
  REAL(r8b), DIMENSION(3), OPTIONAL :: ey_sat  !                      in Y
  REAL(r8b), DIMENSION(3), OPTIONAL :: ez_sat  !                      in Z


! Local Parameters
! ----------------
  CHARACTER(LEN=8),PARAMETER :: srName = 'gnssatti'


! Local Variables
! ---------------
  INTEGER(i4b), DIMENSION(MAXSVN),   SAVE :: svnIdx
  INTEGER(i4b), DIMENSION(0:MAXSAT), SAVE :: svnLst
  INTEGER(i4b), DIMENSION(0:MAXSAT), SAVE :: sysLst
  INTEGER(i4b)                            :: iSat
  INTEGER(i4b)                            :: ideriv

  REAL(r8b), DIMENSION(0:MAXSAT),    SAVE :: EPsave
  REAL(r8b), DIMENSION(3,MAXSAT),    SAVE :: EXsave
  REAL(r8b), DIMENSION(3,MAXSAT),    SAVE :: EYsave
  REAL(r8b), DIMENSION(3,MAXSAT),    SAVE :: EZsave
  REAL(r8b), DIMENSION(4)                 :: xSun
  REAL(r8b), DIMENSION(3)                 :: vSun
  REAL(r8b), DIMENSION(3)                 :: myEX
  REAL(r8b), DIMENSION(3)                 :: myEY
  REAL(r8b), DIMENSION(3)                 :: myEZ
  REAL(r8b)                               :: rsat
  REAL(r8b)                               :: rey
  REAL(r8b)                               :: tdt
  REAL(r8b)                               :: xpol
  REAL(r8b)                               :: ypol
  REAL(r8b)                               :: ut1gps
  REAL(r8b)                               :: sz

  LOGICAL,                           SAVE :: first = .TRUE.


! Init variables
! --------------
  IF ( first) THEN
    svnIdx(:) = 0
    sysLst(0) = -1
    svnLst(:) = 0
    EPsave(0) = 1d20
    first = .FALSE.
  ENDIF

! Check whether we have the result in the buffer
! ----------------------------------------------
  IF ( svnLst(svnIdx(numSat)) == numSat .AND. &
       EPsave(svnIdx(numSat)) == tj     .AND. &
       sysLst(svnIdx(numSat)) == iEarth ) THEN
    myEX(:) = EXsave(:,svnIdx(numsat))
    myEY(:) = EYsave(:,svnIdx(numsat))
    myEZ(:) = EZsave(:,svnIdx(numsat))
  ELSE

! unit vector ez
! --------------
    rsat=DSQRT(xsat(1)**2+xsat(2)**2+xsat(3)**2)
    myEZ(:)=-xsat(1:3)/rsat

! Get the position of the Sun
! ---------------------------
    tdt=tj+(19.D0+32.184D0)/86400.D0
    CALL suneff(iorsys,2.D0,tdt,xSun,vSun)

! Transform the position of the Sun to Earth-fixed frame
! ------------------------------------------------------
    IF ( iEarth == 1 ) THEN
       ideriv=0
       CALL cootra(iorsys,ideriv,tj,xSun,sz,xPol,yPol,ut1gps)
    ENDIF

! unit vector ey
! --------------
    CALL vprod(myEZ,xSun,myEY)

    rey=DSQRT(myEY(1)**2+myEY(2)**2+myEY(3)**2)
    myEY=myEY/rey

! unit vector ex
! --------------
    CALL vprod(myEY,myEZ,myEX)

! Check svnIdx: needs a free slot
! -------------------------------
    IF (svnIdx(numSat) == 0) THEN
      iSat = 1
      DO WHILE (iSat < MAXSAT .AND. svnLst(iSat) /= 0)
        iSat=iSat+1
      ENDDO
      svnIdx(numSat) = iSat
      svnLst(svnIdx(numSat)) = numSat
    ENDIF

! Put the results in the buffer
! -----------------------------
    EPsave(svnIdx(numSat)) = tj
    sysLst(svnIdx(numSat)) = iEarth

    EXsave(:,svnIdx(numsat)) = myEX(:)
    EYsave(:,svnIdx(numsat)) = myEY(:)
    EZsave(:,svnIdx(numsat)) = myEZ(:)
  ENDIF

! Copy the results
! ----------------
  IF (PRESENT(EX_sat)) EX_sat = myEX
  IF (PRESENT(EY_sat)) EY_sat = myEY
  IF (PRESENT(EZ_sat)) EZ_sat = myEZ

  RETURN
END SUBROUTINE gnssatti

END MODULE

