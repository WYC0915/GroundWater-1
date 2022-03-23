MODULE s_MENU_OBS
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_obs(keyWord, output)

! -------------------------------------------------------------------------
! Purpose:    Generates a list of all rstations from the obs. files
!             in GPSEST (called by the menu program via MENUAUX)
!
! Author:     R. Dach
!
! Created:    04-Jul-2001
!
! Changes:    04-Jul-2001 RD: make the station list allocatable
!             05-Sep-2001 HU: Interface for rdhead2 added
!             13-Feb-2002 RD: File names may be empty
!             18-Apr-2002 RD: Use keywords from MENUAUX.INP
!             22-Jul-2002 HB: Use modified t_obsHead
!             23-Apr-2003 AJ: Nullify local pointers
!             16-May-2003 MM: Initialize and deallocate structure
!             06-May-2008 DT: Range observations added
!             20-Sep-2012 RD: Use M_BERN with ONLY
!             20-Sep-2012 RD: Correctly deallocate arrays
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, lfnerr, t_key, &
                      fileNameLength, keyValueLength, staNameLength
  USE d_gpsobs, ONLY: t_obshead, init_obsHead
  USE f_tstkey
  USE s_gtfile2
  USE s_rdhead2
  USE s_staecc
  USE s_alcerr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)                        :: keyWord  ! what to do

! output:
  TYPE(t_key)                             :: output   ! name = keyWord, if OK
                                                      ! value: Result to display

! Local Parameters
! ----------------
  CHARACTER(LEN=8),PARAMETER              :: srName  = 'menu_obs'

  CHARACTER(LEN=7),DIMENSION(6),PARAMETER :: filKeyw = &
           (/ 'PSFILES','CSFILES','PZFILES','CZFILES','OBSFIL ','RZFILES' /)

! Local Variables
! ---------------
  TYPE(t_obshead)               :: obshead    ! Header of obs. files

  CHARACTER(LEN=fileNameLength), &
      DIMENSION(:,:),POINTER    :: filList    ! list of obs-files
  CHARACTER(LEN=staNameLength),  &
      DIMENSION(:),ALLOCATABLE  :: staList    ! List of stations

  INTEGER(i4b),                  &
      DIMENSION(:),ALLOCATABLE  :: staIdx ! Index for sorted stations
  INTEGER(i4b)                  :: numFil     ! # of filList
  INTEGER(i4b)                  :: numSta     ! # of staList
  INTEGER(i4b)                  :: iKey
  INTEGER(i4b)                  :: iSta
  INTEGER(i4b)                  :: iFil
  INTEGER(i4b)                  :: ii, kk
  INTEGER(i4b)                  :: ios, irc

  LOGICAL                       :: inList

  NULLIFY(filList)
  CALL init_obsHead(obsHead)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'OBS') RETURN

! Count the files
! ---------------
  numSta = 0
  DO iKey = 1,SIZE(filKeyw)

    IF (.NOT. tstKey(filKeyw(iKey))) CYCLE

    CALL gtfile2(filKeyw(iKey),1,numFil,filList)

    DEALLOCATE(filList,stat=irc)

    numSta = numSta + 2*numFil
  ENDDO

! Allocate the station list
! -------------------------
  ALLOCATE(staList(numSta),stat=ios)
  CALL alcerr(ios,'staList',(/numSta/),srName)
  staList = ' '

  numSta = 0

! Get the list of stations from the files
! ---------------------------------------
  iKeyLoop: DO iKey = 1,SIZE(filKeyw)
    IF (.NOT. tstKey(filKeyw(iKey))) CYCLE

    CALL gtfile2(filKeyw(iKey),1,numFil,filList)

    DO iFil = 1, numFil

      IF (LEN_TRIM(filList(1,iFil)) == 0) CYCLE

      CALL rdhead2(filList(1,iFil), obshead)

      DO ista = 1, obshead%ndiff + 1

! Is the station in the list?
! ---------------------------
        inList = .FALSE.
        DO kk = 1, numSta
          IF ( staList(kk) == obshead%sta(ista)%stanam ) THEN
            inList = .TRUE.
            EXIT
          END IF
        END DO

! Add new stations to the list
! ----------------------------
        IF ( .NOT. inList ) THEN

          numSta = numSta + 1

          IF (numSta > SIZE(staList)) THEN
            WRITE(lfnerr,'(/,A,2(/,18X,A),/)')                       &
            ' *** SR MENU_OBS: Should never happen.',                &
            'There must be more than two stations in at least one ', &
            'of the observation files.'
            EXIT iKeyLoop
          ENDIF

          staList(numSta) = obshead%sta(ista)%stanam
        ENDIF
      ENDDO ! next station

    ENDDO ! next file

    DEALLOCATE(filList,stat=ios)

  ENDDO iKeyLoop

! Rename stations for ecc. file
! -----------------------------
  IF (numSta > 0) THEN

    CALL staecc(numSta,staList)

! Sort stations
! -------------
    ALLOCATE(staIdx(numSta),stat=irc)
    CALL alcerr(irc,'staIdx',(/numSta/),srName)

    staIdx = (/ (ii, ii=1,numSta) /)

    inList = .TRUE.
    DO WHILE (inList)
      inList = .FALSE.
      DO iSta = 1,numSta-1
        IF (staList(staIdx(iSta)) > staList(staIdx(iSta+1))) THEN
          ii             = staIdx(iSta)
          staIdx(iSta)   = staIdx(iSta+1)
          staIdx(iSta+1) = ii

          inList = .TRUE.
        ENDIF
      ENDDO
    ENDDO
  ENDIF

! Put the results into the output-key
! -----------------------------------
  ALLOCATE(output%value(numSta),stat=irc)
  CALL alcerr(irc,'output%value',(/ numSta /),srName)
  output%value = ' '

  DO iSta = 1, numSta
    WRITE(output%value(iSta), *) TRIM(staList(staIdx(iSta)))
  END DO

  IF (numSta > 0) DEALLOCATE(staIdx,  stat=ios)
  DEALLOCATE(staList, stat=ios)
  DEALLOCATE(obsHead%sat,stat=ios)
  DEALLOCATE(obsHead%ambigu,stat=ios)

  RETURN
END SUBROUTINE menu_obs

END MODULE
