MODULE s_MCOBSB
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE mcobsb(iMode,opt,nFil,filLst,nSat,allSat,satObs,obsEpo,irc)

! -------------------------------------------------------------------------
! Purpose:    Read all observation files, buffer epochs if possible
!
! Author:     R. Dach
!
! Created:    07-Feb-2003
!
! Changes:    07-Feb-2005 HB: Adopt for ifc-Compiler, Version 8.1
!             15-Dec-2005 RD: New call of SR mcobsi
!             20-Sep-2012 RD: Nullify pointers after deallocation
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, fileNameLength
  USE d_gpsobs, ONLY: t_obsEpo
  USE p_mkclus, ONLY: t_mkclus_opt

  USE s_mcobsi
  USE s_alcerr
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  INTEGER(i4b)                         :: iMode    ! Mode of the SR:
                                                   !  1: init and allocate
                                                   !  2: Restart with 1st epoch
                                                   !  0: process an epoch
                                                   ! -1: deallocation
  TYPE(t_mkclus_opt)                   :: opt      ! Input options
  INTEGER(i4b)                         :: nFil     ! Number of files
  CHARACTER(LEN=fileNameLength),        &
               DIMENSION(:,:), POINTER :: filLst   ! Observation files

! input/output:
  INTEGER(i4b)                         :: nSat     ! Number of sats in allSat
  INTEGER(i4b),DIMENSION(:),   POINTER :: allSat   ! List of all satellites
  REAL(r8b),   DIMENSION(:),   POINTER :: satobs   ! Number of observ. from all
                                                   ! files for each sat.
                                                   ! (0d0: sat with few obs.)

! output:
  TYPE(t_obsepo),DIMENSION(:), POINTER :: obsEpo   ! Observation record
  INTEGER(i4b)                         :: irc      ! Return code of GOBSEP

! List of functions
! -----------------


! Local types
! -----------


! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER          :: srName = 'mcobsb'

! Local Variables
! ---------------
  TYPE(t_obsEpo), DIMENSION(:,:), ALLOCATABLE, SAVE :: obsBuffer

  INTEGER(i4b),                                SAVE :: nEpo
  INTEGER(i4b),                                SAVE :: iEpo
  INTEGER(i4b),                                SAVE :: nSat0
  INTEGER(i4b),   DIMENSION(:),   ALLOCATABLE, SAVE :: allSat0
  INTEGER(i4b)                                      :: iFil
  INTEGER(i4b)                                      :: iSat
  INTEGER(i4b),                                SAVE :: numSat
  INTEGER(i4b)                                      :: nrSat

  REAL(r8b),      DIMENSION(:),   ALLOCATABLE, SAVE :: satob0

  LOGICAL,                                     SAVE :: buffer = .TRUE.
  LOGICAL,                                     SAVE :: first  = .TRUE.


! No buffering allowed
! --------------------
  IF (.NOT. buffer) THEN

    CALL mcobsi(iMode,opt,nFil,filLst,nSat,allSat,satObs,obsEpo,irc)
    RETURN

  ENDIF

! Init return code
! ----------------
  irc = 0

! Load buffer for the first call
! ------------------------------
  IF (first) THEN

    ! Init GOBSEP
    CALL mcobsi(1,opt,nFil,filLst,nSat,allSat,satObs,obsEpo,irc)

    ! Rewind GOBSEP
    CALL mcobsi(2,opt,nFil,filLst,nSat,allSat,satObs,obsEpo,irc)

    ! Read all epochs to get the number of epochs
    irc  = 0
    nEpo = 0
    numSat = 0
    DO WHILE (irc == 0)
      CALL mcobsi(0,opt,nFil,filLst,nSat,allSat,satObs,obsEpo,irc)
      IF  (irc == 0) THEN
        nEpo = nEpo + 1
        DO iFil = 1,nFil
          IF (obsEpo(iFil)%nSat > numSat) numSat = obsEpo(iFil)%nSat
        ENDDO
      ENDIF
    ENDDO

    ! Allocate the buffer
    ALLOCATE(obsBuffer(nEpo,nFil),stat=irc)
    CALL alcerr(irc,'obsBuffer',(/nEpo,nFil/),srName)

    ! Rewind GOBSEP
    CALL mcobsi(2,opt,nFil,filLst,nSat,allSat,satObs,obsEpo,irc)

    ! Read all epochs to fill the buffer
    irc  = 0
    nEpo = 0
    DO WHILE (irc == 0)
      CALL mcobsi(0,opt,nFil,filLst,nSat,allSat,satObs,obsEpo,irc)
      IF  (irc == 0) THEN
        nEpo = nEpo + 1

        DO iFil = 1,nFil
          obsBuffer(nEpo,iFil)%obsTim = obsEpo(iFil)%obsTim
          obsBuffer(nEpo,iFil)%epoFlg = obsEpo(iFil)%epoFlg
          obsBuffer(nEpo,iFil)%deltat = obsEpo(iFil)%deltat
          obsBuffer(nEpo,iFil)%nSat   = obsEpo(iFil)%nSat

          NULLIFY(obsBuffer(nEpo,iFil)%obsRec)
          IF (obsEpo(iFil)%nSat > 0) THEN
!!!!!!!!!! Problems with ifc81 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CMP_IFC8
             nrSat = obsEpo(iFil)%nSat
             ALLOCATE(obsBuffer(nEpo,iFil)%obsRec(nrSat),stat=irc)
             CALL alcerr(irc,'obsBuffer(nEpo,iFil)%obsRec',(/nrSat/),srName)
#else
            ALLOCATE(obsBuffer(nEpo,iFil)%obsRec(obsEpo(iFil)%nSat),stat=irc)
            CALL alcerr(irc,'obsBuffer(nEpo,iFil)%obsRec',(/obsEpo(iFil)%nSat/),srName)
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            DO iSat = 1,obsEpo(iFil)%nSat
              obsBuffer(nEpo,iFil)%obsRec(iSat)%numSat    = obsEpo(iFil)%obsRec(iSat)%numSat
              obsBuffer(nEpo,iFil)%obsRec(iSat)%obsFlg(1) = obsEpo(iFil)%obsRec(iSat)%obsFlg(1)
              obsBuffer(nEpo,iFil)%obsRec(iSat)%observ(1) = obsEpo(iFil)%obsRec(iSat)%observ(1)
            ENDDO
          ENDIF
        ENDDO
      ENDIF
    ENDDO

    ! Store the satellite list
    ! ------------------------
    ALLOCATE(allSat0(nSat),stat=irc)
    CALL alcerr(irc,'allSat0',(/nSat/),srName)
    nSat0 = nSat
    allSat0(1:nSat0) = allSat(1:nSat)

    ALLOCATE(satob0(nSat),stat=irc)
    CALL alcerr(irc,'satob0',(/nSat/),srName)
    satob0(1:nSat0) = satobs(1:nSat)

    ! Deallocate mcobsi
    CALL mcobsi(-1,opt,nFil,filLst,nSat,allSat,satObs,obsEpo,irc)

    iEpo = 1
    first = .FALSE.
  ENDIF

! Init some array
! ---------------
  IF (iMode == 1) THEN

    ! Satellite list
    ALLOCATE(allSat(nSat0),stat=irc)
    CALL alcerr(irc,'allSat',(/nSat0/),srName)

    nSat = nSat0
    allSat(1:nSat) = allSat0(1:nSat0)

    ! Satellite list
    ALLOCATE(satobs(nSat0),stat=irc)
    CALL alcerr(irc,'satobs',(/nSat0/),srName)

    satobs(1:nSat) = satob0(1:nSat0)

    ! Allocate obsEpo array
    ALLOCATE(obsEpo(nFil),stat=irc)
    CALL alcerr(irc,'obsEpo',(/nFil/),srName)

    DO iFil = 1,nFil
      ALLOCATE(obsEpo(iFil)%obsRec(numSat),stat=irc)
      CALL alcerr(irc,'obsEpo(iFil)%obsRec',(/numSat/),srName)
    ENDDO

    iEpo = 1
  ENDIF

! Rewind buffer
! -------------
  IF (iMode == 2) THEN

    iEpo = 1

  ENDIF

! Next epoch
! ----------
  IF (iMode == 0) THEN

    ! After last epoch
    IF (iEpo > nEpo) THEN
      irc = 1
      RETURN
    ENDIF

    DO iFil = 1,nFil
      obsEpo(iFil)%obsTim = obsBuffer(iEpo,iFil)%obsTim
      obsEpo(iFil)%epoFlg = obsBuffer(iEpo,iFil)%epoFlg
      obsEpo(iFil)%deltat = obsBuffer(iEpo,iFil)%deltat
      obsEpo(iFil)%nSat   = obsBuffer(iEpo,iFil)%nSat

      IF (obsEpo(iFil)%nSat > 0) THEN
        DO iSat = 1,obsEpo(iFil)%nSat
          obsEpo(iFil)%obsRec(iSat)%numSat    = obsBuffer(iEpo,iFil)%obsRec(iSat)%numSat
          obsEpo(iFil)%obsRec(iSat)%obsFlg(1) = obsBuffer(iEpo,iFil)%obsRec(iSat)%obsFlg(1)
          obsEpo(iFil)%obsRec(iSat)%observ(1) = obsBuffer(iEpo,iFil)%obsRec(iSat)%observ(1)
        ENDDO
      ENDIF

    ENDDO

    iEpo = iEpo + 1
  ENDIF

! Remove some array
! -----------------
  IF (iMode == -1) THEN

    ! Satellite list
    DEALLOCATE(allSat,stat=irc)
    NULLIFY(allSat)

    ! Allocate obsEpo array
    DO iFil = 1,nFil
      DEALLOCATE(obsEpo(iFil)%obsRec,stat=irc)
    ENDDO
    DEALLOCATE(obsEpo,stat=irc)
    NULLIFY(obsEpo)

  ENDIF

  RETURN
END SUBROUTINE mcobsb

END MODULE
