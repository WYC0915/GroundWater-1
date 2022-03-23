MODULE s_RDIHIL
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdihil(maxhil, nAllSat, allSatNum, nhill, hilTyp, sigHil)

! -------------------------------------------------------------------------
! Purpose:    Reads the hill resonance input options for GPSEST
!
! Author:     R. Dach
!
! Created:    03-Jul-2001
!
! Changes:    01-Apr-2003 HU: Comment in DIMTST adapted
!             23-Apr-2003 RD: Nullify local pointers
!             27-Apr-2012 RD: Nullify all pointers
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,  ONLY: i4b, r8b, lfnerr, &
                     keyValueLength, keyNameLength
  USE s_dimtst
  USE s_alcerr
  USE s_readkeys
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                 :: maxhil    ! maximum number of hill parameters
  INTEGER(i4b)                 :: nAllSat   ! number of all satellites
  INTEGER(i4b), DIMENSION(*)   :: allSatNum ! satellite numbers

! output:
  INTEGER(i4b)                 :: nhill     ! number of hill parameters
  INTEGER(i4b), DIMENSION(3,*) :: hiltyp    ! char. of hill parms
  REAL(r8b),    DIMENSION(*)   :: sighil    ! a priori sigmas for hill parms

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
! Hill-keywords
  CHARACTER(LEN=keyNameLength), DIMENSION(6), PARAMETER :: hilKeyw = &
           (/ 'HILC1', 'HILC2', 'HILC3', 'HILP1', 'HILP2', 'HILP3' /)

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
         DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=keyValueLength), &
         DIMENSION(16)          :: hlpStr

  INTEGER(i4b)                  :: nSat        ! Number of all "Hill" satellites
  INTEGER(i4b),                  &
        DIMENSION(:), POINTER   :: satList     ! List of all "Hill" satellites
  INTEGER(i4b)                  :: satNum

  INTEGER(i4b)                  :: numHil      ! Number of forces
  INTEGER(i4b), DIMENSION(6)    :: forceH      ! Define default forces
  INTEGER(i4b)                  :: irc, ios
  INTEGER(i4b)                  :: iSat, jSat
  INTEGER(i4b)                  :: ii, jj

  REAL(r8b),    DIMENSION(6)    :: sigmaH      ! Define default sigmas

  LOGICAL                       :: isSort

! Init the variables
! ------------------
  nhill                = 0
  hiltyp(1:3,1:maxhil) = 0
  sighil(1:maxhil)     = 0.0

  NULLIFY(keyValue)
  NULLIFY(satList)

! Default input type
! ------------------
  CALL readKeys('HILL_D', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') THEN

! Get Types to estimate and apriori sigmas
! ----------------------------------------
    numHil = 0
    forceH = 0
    sigmaH = 0d0
!
    DO ii = 1, SIZE(hilKeyw)
! Read the force type
      CALL readKeys(hilKeyw(ii), keyValue, irc)
      IF (irc == 0 .AND. keyValue(1) == '1') THEN
        numHil = numHil + 1
        IF (ii > 3) numHil = numHil + 1  ! Periodic forces
        forceH (ii) = 1
! Get the default sigma
        CALL readKeys('SIG'//TRIM(hilKeyw(ii)), keyValue, irc)
        IF (irc == 0) READ(keyValue(1),*, iostat = ios) sigmaH(ii)
        IF ( irc /= 0                                     .OR. &
             (ios /= 0 .AND. LEN_TRIM(keyValue(1)) > 0)   .OR. &
             sigmaH(ii) < 0d0  )                            THEN
          WRITE(lfnerr,'(/,A,/,16X,A,A,/)')                       &
          ' *** SR RDIHIL: Wrong apriori sigma for the Hills ' // &
                                              'resonance terms',&
                          'Specified Value: ',TRIM(keyValue(1))
          CALL exitrc(2)
        ENDIF
      ENDIF
    ENDDO


! Read the satellite for Hill resonance
! -------------------------------------
    ALLOCATE(satList(nAllSat), stat=ios)
    CALL alcerr(ios, 'satList', (/nAllSat/), 'rdihil')

    nSat = 0
    CALL readKeys('HILSATSTR', keyValue, irc)
    DO ii = 1, SIZE(keyValue)
      READ(keyValue(ii), *, iostat=ios) (hlpStr(jj), jj=1,16)
      IF (ios == 0) THEN
! Get the list of satellites
        satLoop: DO jj = 1, 16
! An empty string found
          IF (LEN_TRIM(hlpStr(jj)) == 0) CYCLE satLoop

! Read the satellite number
          READ(hlpStr(jj),*,iostat=ios) satNum
          IF (ios /= 0) CYCLE satLoop

! Is the satellite in the observation files?
          DO iSat = 1, nAllSat
            IF (satNum == allSatNum(iSat)) THEN

! Is the satellite allready in the list?
              DO jSat = 1, nSat
                IF (satList(jSat) == satNum) CYCLE satLoop
              ENDDO

! Add a new stallite to the list
              nSat = nSat + 1
              satList(nSat) = satNum
              CYCLE satLoop
            ENDIF
          ENDDO
        ENDDO satLoop

      ENDIF ! line was readable
    ENDDO   ! all unilines

! Check maximum dimension
! -----------------------
    CALL dimtst(1,1,2,'rdihil','maxhil',                          &
                'Hill resonance terms',                           &
                'Parameter is defined in module "P_GPSEST.f90".', &
                numHil*nSat,maxhil,irc)

! Sort satellites in list
! -----------------------
    isSort = .FALSE.
    DO WHILE (isSort)
      isSort = .TRUE.
      DO iSat = 1, nSat-1
        IF (satList(iSat) > satList(iSat+1)) THEN
          satNum = satList(iSat+1)
          satList(iSat+1) = satList(iSat)
          satList(iSat) = satNum
          isSort = .FALSE.
        ENDIF
      ENDDO
    ENDDO

! Generate the list of hill parameters
! ------------------------------------
    DO ii = 1,6
      DO iSat = 1, nSat
        IF (forceH(ii) /= 0) THEN
          nHill = nHill + 1
          hilTyp(1,nHill) = satList(iSat)
          hilTyp(2,nHill) = MOD((ii-1),3) + 1
          hilTyp(3,nHill) = INT((ii-1)/3) + 1
          sigHil(nHill)   = sigmaH(ii)
          IF (ii > 3) THEN
            nHill = nHill + 1
            hilTyp(1,nHill) = satList(iSat)
            hilTyp(2,nHill) = MOD(ii-1,3)+1
            hilTyp(3,nHill) = 3
            sigHil(nHill)   = sigmaH(ii)
          ENDIF
        ENDIF
      ENDDO
    ENDDO

    DEALLOCATE(satList)
  ENDIF

! Manual input type
! -----------------
  CALL readKeys('HILL_M', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') THEN

! Read the parameter spec. string
! -------------------------------
    CALL readKeys('HILLSTR', keyValue, irc)
    IF (irc == 0) THEN

! Check maximum dimension
! -----------------------
      CALL dimtst(1,1,2,'rdihil','maxhil',                          &
                  'Hill resonance terms',                           &
                  'Parameter is defined in module "P_GPSEST.f90".', &
                  SIZE(keyValue),maxhil,irc)

! Extract the information
! -----------------------
      DO ii = 1, SIZE(keyValue)
        READ(keyValue(ii),*,iostat=ios) (hlpStr(jj), jj=1,4)


! Is the satellite in the observation files?
        READ(hlpStr(1),*,iostat=ios) satNum
        DO iSat = 1, nAllSat
          IF (satNum == allSatNum(iSat)) THEN
            nHill = nHill + 1
            hilTyp(1,nHill) = satNum

! Read the force ID
! -----------------
            READ(hlpStr(2),*,iostat=ios) hilTyp(2,nHill)
            IF (ios /= 0 .OR.                                    &
                hilTyp(2,nHill) > 3 .OR. hilTyp(2,nHill) < 1) THEN
              WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I6,/,16X,A,A,/)')            &
              ' *** SR RDIHIL: Wrong specification of the direction of the '//&
                                                                     'force', &
                              'for Hills resonance terms (manual setup)',     &
                              'Number of entry:  ',ii,                        &
                              'Specified value:  ',TRIM(hlpStr(2))
              IF (nHill > 0) nHill = nHill - 1
            ENDIF

! Read the force type
! -------------------
            READ(hlpStr(3),*,iostat=ios) hilTyp(3,nHill)
            IF (ios /= 0 .OR.                                    &
                hilTyp(3,nHill) > 3 .OR. hilTyp(3,nHill) < 1) THEN
              WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I6,/,16X,A,A,/)')            &
              ' *** SR RDIHIL: Wrong specification of the type of the force', &
                              'for Hills resonance terms (manual setup)',     &
                              'Number of entry:  ',ii,                        &
                              'Specified value:  ',TRIM(hlpStr(3))
              IF (nHill > 0) nHill = nHill - 1
            ENDIF

! Read the force sigma
! --------------------
            READ(hlpStr(4),*,iostat=ios) sigHil(nHill)
            IF ((ios /= 0 .AND. LEN_TRIM(hlpStr(4)) > 0) .OR. &
                sigHil(nHill) < 0d0) THEN
              WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I6,/,16X,A,A,/)')            &
              ' *** SR RDIHIL: Wrong specification of the sigma of the force',&
                              'for Hills resonance terms (manual setup)',     &
                              'Number of entry:  ',ii,                        &
                              'Specified value:  ',TRIM(hlpStr(4))
              IF (nHill > 0) nHill = nHill - 1
            ENDIF

            EXIT   ! Loop of all satellites
          ENDIF  ! Spec. sat found in file
        ENDDO  ! Loop of all satellites
      ENDDO  ! Loop all requests

! Stop if invalid requests found
      IF (nHill /= SIZE(keyValue)) CALL exitrc(2)

    ENDIF  ! Request was readable
  ENDIF  ! Manual selection of the requests

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE rdihil

END MODULE
