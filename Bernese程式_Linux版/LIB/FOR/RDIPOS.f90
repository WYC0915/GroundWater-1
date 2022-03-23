MODULE s_RDIPOS
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdipos(nAllSta, allStaNum, allStaName, &
                  nfix, stfix, nstwgt, istwgt, stwgt, iloc, ihelm, wgtFile)

! -------------------------------------------------------------------------
! Purpose:    Reads the positioning options for GPSEST
!
! Author:     R. Dach
!
! Created:    29-Jun-2001
!
! Changes:    04-Jul-2001 RD: New call of gtStaNum (for ADDNEQ2)
!             25-Jul-2001 DI: Some typos corrected
!             30-Jul-2001 RD: Read default sigma only, if stations elected
!             21-Aug-2001 RD: rHlp must be allocatable
!             28-Aug-2001 RD: New call of SR gtStaNum
!             08-Oct-2002 RD: New call of SR gtStaNum
!             10-Dec-2002 CU: New parameter wgtFile (a priori crd sigma file)
!             22-Jan-2003 SC: Radiobutton for positioning options
!             03-Feb-2003 RD: New call of GTSTANUM (weight is pointer now)
!             23-Apr-2003 RD: Nullify local pointers
!             04-Nov-2003 HB: Declare allStaName with (:)
!             19-Jan-2003 SS/MM: Revision of GPSEST input panels
!             05-May-2004 HB: Initialize wgtFile
!             28-Jun-2005 MM: Unused variables removed
!             21-May-2010 MF: Nullify staNam & rHlp
!             20-Sep-2012 RD: Correctly deallocate arrays
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, &
                      keyNameLength, keyValueLength, &
                      stanameLength, shortLineLength
  USE s_alcerr
  USE s_gtstanum
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                  :: nAllSta    ! number of all stations
  INTEGER(i4b),    DIMENSION(*) :: allStaNum  ! station numbers
  CHARACTER(LEN=staNameLength), &
                   DIMENSION(:) :: allStaName ! station names

! output:
  INTEGER(i4b)                  :: nfix       ! number of stations held fixed
  INTEGER(i4b), DIMENSION(:)    :: stfix      ! numbers of the stations
                                              ! held fixed
  INTEGER(i4b)                  :: nstwgt     ! # stations with a priori weights
                                              ! for coordinates
  INTEGER(i4b), DIMENSION(:)    :: istwgt     ! numbers of the stat.
                                              ! with weights
  REAL(r8b),    DIMENSION(:,:)  :: stwgt      ! a priori weights for stations
  CHARACTER(LEN=shortLineLength):: wgtFile    ! sigma file
  INTEGER(i4b)                  :: iloc       ! local(n,e,u) or geocentric syst.
                                              ! for helmert transformation
  INTEGER(i4b), DIMENSION(:)    :: ihelm      ! flags for helmert-parameters

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
! Default coordinate sigma keywords
  CHARACTER(LEN=keyNameLength), DIMENSION(3), PARAMETER :: sigKeyw = &
           (/ 'SIGCRDN', 'SIGCRDE', 'SIGCRDU' /)

! Component for error message
  CHARACTER(LEN=5), DIMENSION(3), PARAMETER :: errStr = &
           (/ 'north', 'east ', 'up   ' /)

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
         DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=staNameLength),  &
         DIMENSION(:), POINTER  :: staNam    ! Station names, dummy for GPSEST

  INTEGER(i4b)                  :: irc, ios
  INTEGER(i4b)                  :: irCode = 0
  INTEGER(i4b)                  :: iSta, jSta
  INTEGER(i4b)                  :: jj
  INTEGER(i4b)                  :: iRadio

  REAL(r8b),                     &
        DIMENSION(:,:), POINTER :: rHlp

! Init the variables
! ------------------
  ios = 0
  wgtFile = ' '

  NULLIFY(keyValue)
  NULLIFY(staNam)
  NULLIFY(rHlp)

  ALLOCATE(staNam(nAllSta), stat=irc)
  CALL alcerr(irc, 'staNam', (/nAllSta/), 'rdipos')

! Options of radiobuttons
!------------------------
  CALL ckoptb(1,(/'RADIO8_1','RADIO8_2','RADIO8_3'/),'rdipos', &
              'Positioning options',irCode,result1=iRadio)

! Fixed Stations
! --------------
  IF (iRadio == 1) THEN
    ALLOCATE(rHlp(3,nAllSta), stat=irc)
    CALL alcerr(irc, 'rHlp', (/3,nAllSta/), 'rdipos')

    nFix = 0
    CALL gtStaNum(nAllSta, allStaNum, allStaName,                &
                  'FIXSTA', 'STATION1', 'STAFILE1', 'STAFLAG1',  &
                  nfix, stfix, stanam, 0, rHlp)

    DEALLOCATE(rHlp)
  ENDIF

! Get the default sigma for station constraining
! ----------------------------------------------
  IF (iRadio == 2) THEN

    ALLOCATE(rHlp(3,nAllSta),stat=irc)
    CALL alcerr(irc,'rHlp',(/3,nAllSta/),'rdipos')
    rHlp = 0

    CALL readKeys('SIGSTA', keyValue, irc)
    wgtFile = ' '
    IF (irc == 0 .AND. keyValue(1) == 'FROM_FILE') THEN
      CALL gtflna(0,'STAFILE4',wgtFile,irc)
    ELSEIF (irc == 0 .AND. &
      keyValue(1) /= 'FROM_FILE' .AND. keyValue(1) /= 'NONE') THEN
      rHlp(1:3,1) = 0d0
      DO jj = 1,3
        CALL readKeys(sigKeyw(jj), keyValue, irc)
        IF (irc == 0) READ (keyValue(1),*,iostat=ios) rHlp(jj,1)
        IF (irc /= 0 .OR. ios /= 0 .OR. rHlp(jj,1) < 0d0) THEN
          WRITE(lfnerr,'(/,A,A,/,16X,A,/,16X,A,A,/)')                          &
          ' *** SR RDIPOS: Wrong default sigma for the coordinate ',           &
          'constraining ',                                                     &
          'in the '//TRIM(errstr(jj))//' component detected.',                 &
          'Specified Value: ',TRIM(keyValue(1))
          CALL exitrc(2)
        ENDIF
      ENDDO
    ENDIF

! Stations with Constrained Coordinates
! -------------------------------------
    nstwgt = 0
    CALL gtStaNum(nAllSta, allStaNum, allStaName,               &
                  'SIGSTA', 'STATION4', 'STAFILE4', 'STAFLAG4', &
                  nstwgt, istwgt, stanam, 3, rHlp)
    DO jj = 1,3
      stWgt(jj,1:nstwgt) = rHlp(jj,1:nstwgt)
    ENDDO
    DEALLOCATE(rHlp,   stat=irc)

! Remove fixed stations from "sigma list"
! ---------------------------------------
    iSta = 1
    DO WHILE (iSta <= nstwgt)
      DO jSta = 1, nFix
        IF (istwgt(iSta) == stfix(jSta)) THEN
          IF (iSta < nstwgt) THEN
            istwgt(iSta:nstwgt-1)     = istwgt(iSta+1:nstwgt)
            stwgt (1:3,iSta:nstwgt-1) = stwgt (1:3,iSta+1:nstwgt)
            iSta = iSta - 1
          ENDIF
          nstwgt = nstwgt - 1
        ENDIF
      ENDDO
      iSta = iSta + 1
    ENDDO

  ENDIF ! Station constraining

! Free network solution
! ---------------------
  IF (iRadio == 3) THEN

    nfix = 0
    nstwgt = 0

  ENDIF

  DEALLOCATE(stanam, stat=irc)


! Helmert Parameters
! ------------------
  iloc       = 0
  ihelm(1:7) = 0
  CALL readKeys('PRTHELM', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') THEN
    ihelm(1:3) = 1

    CALL readKeys('ROTXYZ', keyValue, irc)
    IF (irc == 0 .AND. keyValue(1) == '1') ihelm(4:6) = 1
    CALL readKeys('SCALE', keyValue, irc)
    IF (irc == 0 .AND. keyValue(1) == '1') ihelm(7) = 1

    CALL readKeys('COORDSYS', keyValue, irc)
    IF (keyValue(1) == 'LOCAL') THEN
      iloc = 1
    ELSE IF (keyValue(1) == 'GEOCENTRIC') THEN
      iloc = 2
    ENDIF
  ENDIF

! Deactivate Helmert transformation if indicated
  CALL readKeys('FREQUENCY', keyValue, irc)
  IF (keyValue(1) == 'L4' .OR. &
      keyValue(1) == 'MELWUEBB' .OR. &
      keyValue(1) == 'DTEC') THEN
    iloc       = 0
    ihelm(1:7) = 0
  ENDIF

  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE rdipos

END MODULE
