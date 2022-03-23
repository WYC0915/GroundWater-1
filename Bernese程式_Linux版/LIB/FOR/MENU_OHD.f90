MODULE s_MENU_OHD
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_ohd(keyWord)

! -------------------------------------------------------------------------
! Purpose:    Is used for editing, and storing of Bernese
!             observation header/observation files using the menu program
!             (called by the menu program via MENUAUX)
!
! Author:     R. Dach
!
! Created:    16-May-2002
!
! Changes:    11-Feb-2003 RD: Browse Bernese observation files
!             16-May-2003 MM: Initialize and deallocate structure
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnloc, lfnres, lfnerr, &
                      keyValueLength
  USE d_gpsobs, ONLY: t_obsHead, init_obsHead
  USE s_rdhead2
  USE s_rdobsi
  USE s_opnfil
  USE s_alcerr
  USE s_wthead2
  USE s_wtfmti
  USE s_gtflna
  USE s_opnerr
  USE s_rdfmth2
  USE s_wtfmth2
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)                       :: keyWord     ! what to do

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER   :: srName = 'menu_ohd'

! Local Variables
! ---------------
  TYPE(t_obsHead)                              :: obsHead ! Observation header

  CHARACTER(LEN=keyValueLength)                :: obsFil  ! Observation file
  CHARACTER(LEN=keyValueLength)                :: auxFil  ! Scratch file name
  CHARACTER(LEN=6)                             :: mxnSat
  CHARACTER(LEN=1),DIMENSION(:,:), ALLOCATABLE :: obsFlg
  CHARACTER(LEN=1)                             :: epoFlg

  INTEGER(i4b)                                 :: nSat
  INTEGER(i4b),    DIMENSION(:),   ALLOCATABLE :: nrSat
  INTEGER(i4b)                                 :: mxcSat
  INTEGER(i4b),    DIMENSION(2)                :: iFrqs = (/ 1,2 /)
  INTEGER(i4b)                                 :: init,iretrn
  INTEGER(i4b)                                 :: irc
  INTEGER(i4b)                                 :: ioStat

  REAL(r8b),       DIMENSION(:,:), ALLOCATABLE :: observ
  REAL(r8b),       DIMENSION(2)                :: deltat
  REAL(r8b)                                    :: obsTim
  REAL(r8b)                                    :: dtBlnk = 1d-9

! COMMON FOR MAXIMAL DIMENSIONS, COMMON FOR CONSTANTS
! ---------------------------------------------------
  COMMON/MCMSAT/MXCSAT,MXNSAT

  CALL init_obshead(obsHead)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'B2A_OBSHEAD' .AND. keyWord /= 'A2B_OBSHEAD' .AND. &
      keyWord /= 'B2A_OBSFILE'                               ) RETURN

! INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
! -----------------------------------------------
  MXNSAT='MAXSAT'

! Get the file names
! ------------------
  CALL gtflna(1,'BINARY_FILE',obsFil,irc)
  CALL gtflna(1,'ASCII_FILE', auxFil,irc)

  CALL opnFil(lfnRes,auxFil,'NEW',' ',' ',' ',ioStat)
  CALL opnErr(lfnErr,lfnRes,ioStat,auxFil,srName)


! Convert the header (bin->asc)
! -----------------------------
  IF (keyWord == 'B2A_OBSHEAD') THEN

    CALL rdhead2(obsfil, obshead)
    CALL wtfmth2(lfnRes, obshead)
    CLOSE(UNIT=lfnRes)

! Convert the header (asc->bin)
! -----------------------------
  ELSEIF (keyWord == 'A2B_OBSHEAD') THEN

    CALL rdfmth2(lfnRes, obshead)
    CALL wthead2(obsFil, obshead)
    CLOSE(UNIT=lfnRes)


! Convert the observation (bin->asc)
! ----------------------------------
  ELSE IF (keyWord == 'B2A_OBSFILE') THEN

    CALL rdhead2(obsfil, obshead)
    CALL wtfmth2(lfnRes, obshead)

    ! Allocate arrays
    MXCSAT=obsHead%nSatel

    ALLOCATE(nrSat(obsHead%nSatel),stat=irc)
    CALL alcerr(irc,'nrSat',(/obsHead%nSatel/),srName)

    ALLOCATE(observ(obsHead%nSatel,2),stat=irc)
    CALL alcerr(irc,'observ',(/obsHead%nSatel,2/),srName)

    ALLOCATE(obsFlg(obsHead%nSatel,2),stat=irc)
    CALL alcerr(irc,'obsFlg',(/obsHead%nSatel,2/),srName)


    CALL gtflna(1,'BINARY_FILE_2',obsFil,irc)

    CALL opnFil(lfnloc,obsFil,'OLD','UNFORMATTED',' ',' ',ioStat)
    CALL opnErr(lfnErr,lfnloc,ioStat,obsFil,srName)

    ! Loop over all observations
    init   = 1
    iretrn = 0
    DO WHILE (iretrn == 0)

      ! Read observation from unformatted observation file
      CALL RDOBSI(lfnLoc,obsHead%iFrmat,obsHead%nFreq,iFrqs,obsTim,&
           deltat,epoFlg,nSat,nrSat,obsFlg,observ,iretrn)

      ! End of file reached
      IF (iretrn == 1) EXIT

      ! Write observation into formatted observation file
      CALL wtfmti(lfnRes,init,obsHead%iFrmat,obsHead%nFreq,obsHead%timRef,&
           obsHead%iDeltt,dtBlnk,obsTim,deltat,epoFlg,nSat,nrSat,obsFlg,observ)
    ENDDO

    CLOSE(UNIT=lfnRes)
    CLOSE(UNIT=lfnLoc)

    DEALLOCATE(nrSat ,stat=irc)
    DEALLOCATE(observ,stat=irc)
    DEALLOCATE(obsflg,stat=irc)
    DEALLOCATE(obsHead%sat,stat=irc)
    DEALLOCATE(obsHead%ambigu,stat=irc)



  ENDIF

  RETURN
END SUBROUTINE menu_ohd

END MODULE
