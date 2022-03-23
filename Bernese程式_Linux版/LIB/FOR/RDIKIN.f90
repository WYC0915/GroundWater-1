MODULE s_RDIKIN
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdikin(nAllSta, allStaNum, allStaName,     &
                  eccStaNum,eccStaName,globalWindow,  &
                  nKin,stKin,nEpObs,nStwgt,iStwgt,stWgt)

! -------------------------------------------------------------------------
! Purpose:    Reads the input options for kinematic positioning in GPSEST
!
! Author:     R. Dach
!
! Created:    27-Jun-2003
!
! Changes:    22-Jul-2003 RD: Read constraints for kin. pos. in RDIKIN
!             04-Nov-2003 HB: Declare allStaName with (:)
!             19-Jan-2003 SS/MM: Revision of GPSEST input panels
!             27-Apr-2012 RD: Nullify all pointers
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, &
                      keyValueLength, staNameLength
  USE m_time,   ONLY: t_timint
  USE s_ckoptr
  USE s_alcerr
  USE s_gtstanum
  USE s_staflg
  USE s_readkeys
  USE s_exitrc
  USE s_ckopti
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                 :: nAllSta      ! number of all stations
  INTEGER(i4b), DIMENSION(*)   :: allStaNum    ! station numbers
  CHARACTER(LEN=staNameLength), &
                DIMENSION(:)   :: allStaName   ! station names
  INTEGER(i4b), DIMENSION(*)   :: eccStaNum    ! station numbers, eccenter
  CHARACTER(LEN=staNameLength), &
                DIMENSION(*)   :: eccStaName   ! station names, eccenter
  TYPE(t_timint)               :: globalWindow ! window to be processed
                                               ! (from - to, MJD)

! output:
  INTEGER(i4b)                 :: nkin   ! number of stations estimated
                                         ! in kin. modus
  INTEGER(i4b), DIMENSION(:)   :: stkin  ! numbers of the kin. stations
  INTEGER(i4b), DIMENSION(*)   :: nepobs ! Min # of obs. for epoch param.s
                                         ! 1: sta-clk / 2: sat-clk / 3: kin

! input/output
  INTEGER(i4b)                 :: nstwgt ! # stations with a priori weights
                                         ! for coordinates
  INTEGER(i4b), DIMENSION(:)   :: istwgt ! numbers of the stat. with weights
  REAL(r8b),    DIMENSION(:,:) :: stwgt  ! a priori weights for stations

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER   :: srName = 'rdikin'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
       DIMENSION(:),POINTER     :: keyValue
  CHARACTER(LEN=staNameLength),  &
       DIMENSION(:),POINTER     :: staNam
  CHARACTER(LEN=20)             :: marTyp

  INTEGER(i4b)                  :: iSta,jSta
  INTEGER(i4b)                  :: iKin
  INTEGER(i4b)                  :: iFlag
  INTEGER(i4b)                  :: irc, iac
  INTEGER(i4b)                  :: irCode

  REAL(r8b),                     &
       DIMENSION(:,:), POINTER  :: rHlp
  REAL(r8b)                     :: mEpoch
  REAL(r8b)                     :: sigmaH,sigmaV
  REAL(r8b)                     :: sigmaL

! Init the variables
! ------------------
  irCode    = 0

  nkin      = 0
  nEpObs(3) = 0

  NULLIFY(keyValue)
  NULLIFY(rHlp)
  NULLIFY(staNam)

! Read list of kinematic stations
! -------------------------------
  ALLOCATE(staNam(nAllSta), stat=iac)
  CALL alcerr(iac, 'staNam', (/nAllSta/), srName)

  CALL gtStaNum(nAllSta, allStaNum, allStaName,       &
               'KINSTA', 'STATION2', 'STAFILE2', ' ', &
                nkin, stkin, stanam, 0, rHlp)

! Check that there is no eccentricity
! -----------------------------------
  DO iKin = 1,nKin
    DO iSta = 1,nAllSta
      IF (stanam(iKin) /= allStaName(iSta)) CYCLE
      IF (stanam(iKin) /= eccStaName(iSta) .OR. &
          stKin(iKin)  /= eccStaNum(iSta)) THEN
        WRITE(lfnerr,'(/,A,2(/,16X,A),/)')                                    &
        ' *** SR RDIKIN: It is not permitted to estimate kinematic positions',&
                        'for stations/objects with an eccentricity!',         &
                        'Station name: ' // TRIM(stanam(iKin))
        irCode = irCode+1
      ENDIF
    ENDDO
  ENDDO

! Get constraints for kinematic stations/objects
! ----------------------------------------------
  sigmaH = 1d20
  sigmaV = 1d20
  sigmaL = 1d20

  ! Loop all kinematic stations
  DO iKin = 1,nKin

    ! Loop all sigmas already set
    jSta = 0
    DO iSta = 1,nStwgt

      ! Constraints come already from "RDIPOS"
      IF (iStwgt(iSta) == stKin(iKin)) jSta = iSta

    ENDDO

    ! Compute the mean epoch
    mEpoch = (globalWindow%t(2)-globalWindow%t(1))/2d0
    CALL staFlg(staNam(iKin),mEpoch,iFlag,marTyp)

    CALL readKeys('SIGKIN',keyValue,irc)

!!    ! Terrestrial kinematic stations
!!    IF (marTyp == ' ') THEN
    IF (keyValue(1) == 'NEU') THEN

      ! Read horizontal and vertical sigmas
      IF (sigmaH == 1d20 .OR. sigmaV == 1d20) THEN

        CALL readKeys('SIGKIN_H',keyValue,irc)

        CALL ckoptr(1,'SIGKIN_H',keyValue,srName,                        &
                    'Horizontal constraints for kin. pos.',irc,irCode,   &
                    empty=0d0,gt=0d0,maxVal=1,result1=sigmaH)

        CALL readKeys('SIGKIN_V',keyValue,irc)

        CALL ckoptr(1,'SIGKIN_V',keyValue,srName,                        &
                    'Vertical constraints for kin. pos.',irc,irCode,     &
                    empty=0d0,gt=0d0,maxVal=1,result1=sigmaV)

      ENDIF

      ! Add sigmas to the list of constraints
      IF (sigmaV /= 0d0 .AND. sigmaH /= 0d0) THEN

        IF (jSta == 0) THEN
          jSta   = nStwgt+1
          nStwgt = nStwgt+1
        ENDIF

        istwgt(jSta)    = stKin(iKin)
        stwgt(1:2,jSta) = sigmaH
        stwgt(3,jSta)   = sigmaV

      ELSE IF (sigmaV == 0d0 .AND. sigmaH == 0d0) THEN
        CYCLE
      ELSE
        WRITE(lfnerr,'(/,A,/,16X,A,/)')                                    &
        ' *** SR RDIKIN: Specify sigma for both: horizontal and vertical', &
                        'components to constrain kinematic positions.'
        irCode = irCode + 1
      ENDIF

    ! Non-terrestrial object (e.g., LEO)
    ELSE

      ! Read one sigma for all components
      IF (sigmaL == 1d20) THEN

        CALL readKeys('SIGKIN_L',keyValue,irc)

        CALL ckoptr(1,'SIGKIN_L',keyValue,srName,                        &
                    'Constraints for kin. pos.',irc,irCode,              &
                    empty=0d0,gt=0d0,maxVal=1,result1=sigmaL)

      ENDIF

      ! Add sigma to the list of constraints
      IF (sigmaL /= 0d0) THEN

        IF (jSta == 0) THEN
          jSta   = nStwgt+1
          nStwgt = nStwgt+1
        ENDIF

        istwgt(jSta) = stKin(iKin)
        stwgt(1:3,jSta) = sigmaL
      ENDIF

    ENDIF

  ENDDO ! Next kinematic station/object

  DEALLOCATE(staNam, stat=irc)

! Get minimum number of observations per parameter
! ------------------------------------------------
  CALL readKeys('MINKIN',keyValue,irc)
  CALL ckopti(1,'MINKIN',keyValue,srName,                          &
              'Minimum nuber of observ. for kin. pos.',irc,irCode, &
               empty=0,ge=0,maxVal=1,result1=nepobs(3))

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=iac)

! Stop if an input error was found
! --------------------------------
   IF (irCode /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE rdikin

END MODULE
