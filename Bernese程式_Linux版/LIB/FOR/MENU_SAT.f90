MODULE s_MENU_SAT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_sat(keyWord, output)

! -------------------------------------------------------------------------
! Purpose:    Generates a list of all recever/antennas from the obs. files
!             in GPSEST (called by the menu program via MENUAUX) and SATMRK
!
! Author:     R. Dach
!
! Created:    20-Jun-2001
! Last mod.:  06-May-2008
!
! Changes:    28-Jun-2001  RD: Write also a "normal" satellite list
!             05-Sep-2001  HU: Interface for rdhead2 added
!             28-Jan-2002  MM: Remove DCB functionality
!             30-Jan-2002  HB: Exclude possibilty of no filname available
!                              (Problem of SATMRK)
!             18-Apr-2002  RD: Use keywords from MENUAUX.INP
!             22-Jul-2002  HB: Use modified t_obsHead
!             23-Apr-2003  RD: Nullify local pointers
!             16-May-2003  MM: Initialize and deallocate structure
!             06-May-2008  DT: Range observations added
!
! SR called:  alcerr, gtfile2, rdhead2, tstkey, init_obshead
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_gpsobs, ONLY: t_obshead, init_obsHead
  USE s_gtfile2
  USE s_alcerr
  USE f_tstkey
  USE s_rdhead2
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
  CHARACTER(LEN=8),PARAMETER              :: srName  = 'menu_sat'

  CHARACTER(LEN=7),DIMENSION(9),PARAMETER :: filKeyw = &
           (/ 'PSFILES','CSFILES','PZFILES','CZFILES','RZFILES',&
              'PSHFIL ','CSHFIL ','PZHFIL ','CZHFIL ' /)


! Local Variables
! ---------------
  TYPE(t_obshead)               :: obshead           ! Header of obs. files

  CHARACTER(LEN=fileNameLength), &
         DIMENSION(:,:),POINTER :: filList           ! list of inputs

  INTEGER(i4b),                &
         DIMENSION(:),POINTER   :: satList           ! List of satellites
  INTEGER(i4b),                &
         DIMENSION(:),POINTER   :: hlpList

  INTEGER(i4b)                  :: numFil            ! # in FilList
  INTEGER(i4b)                  :: nSat              ! # in satList
  INTEGER(i4b)                  :: iKey, iFil
  INTEGER(i4b)                  :: iSat, jSat
  INTEGER(i4b)                  :: satHelp
  INTEGER(i4b)                  :: ii
  INTEGER(i4b)                  :: ios, irc

  LOGICAL                       :: inList

  NULLIFY(filList)
  NULLIFY(satList)
  NULLIFY(hlpList)
  CALL init_obsHead(obsHead)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'SATLST') RETURN

! Allocate the list of antennas
! -----------------------------
  ALLOCATE(satList(20),stat=ios)
  CALL alcerr(ios,'satList',(/20/),srName)

  nSat = 0

! Get the list of satellites from the observation files
! -----------------------------------------------------
  iKeyLoop: DO iKey = 1,SIZE(filKeyw)
    IF (.NOT. tstKey(filKeyw(iKey))) CYCLE

    CALL gtfile2(filKeyw(iKey),1,numFil,filList)

    DO iFil = 1, numFil

      IF (LEN_TRIM(filList(1,iFil)) == 0) CYCLE

      CALL rdhead2(filList(1,iFil), obshead)

      DO iSat = 1, obshead%nSatel

! Is is it a new satellite?
! -------------------------
        inList = .FALSE.
        DO jSat = 1, nSat
          IF ( satList(jSat) == obshead%sat(iSat)%numSat ) THEN
            inList = .TRUE.
            EXIT
          ENDIF
        ENDDO

! Add a record to the list
! ------------------------
        IF ( .NOT. inList ) THEN

! Extent the size of the satellite list
          IF (nSat >= SIZE(satList)) THEN
            ALLOCATE(hlpList(SIZE(satList)), stat = irc)
            CALL alcerr(irc, 'hlpList', (/SIZE(satList)/), srName)

            hlpList = satList

            DEALLOCATE(satList, stat=irc)
            ALLOCATE(satList(nSat+5), stat = irc)
            CALL alcerr(irc, 'satList', (/nSat+5/), srName)

            satList(1:nSat) = hlpList(1:nSat)

            DEALLOCATE(hlpList)
          ENDIF

          nSat = nSat + 1
          satList(nSat) = obshead%sat(iSat)%numSat
        ENDIF
      ENDDO ! next satellite

    ENDDO ! next file

    DEALLOCATE(filList,stat=ios)

  ENDDO iKeyLoop


! Sort the list of satellites
! ---------------------------
  inList = .TRUE.
  DO WHILE (inList)
    inList = .FALSE.
    ii = 1
    DO WHILE (ii < nSat)
      IF ( satList(ii) > satList(ii+1) ) THEN
        satHelp       = satList(ii)
        satList(ii)   = satList(ii+1)
        satList(ii+1) = satHelp
        inList = .TRUE.
      ENDIF
      ii = ii + 1
    ENDDO
  ENDDO

! Put the results into the output-key
! -----------------------------------
  ALLOCATE(output%value(nSat),stat=irc)
  CALL alcerr(irc,'output%value',(/ nSat /),srName)
  output%value = ' '

  DO iSat = 1, nSat
    WRITE(output%value(iSat), '(I3)') satList(iSat)
  ENDDO

  DEALLOCATE (satList, stat=ios)
  DEALLOCATE(filList,stat=ios)
  DEALLOCATE(hlpList,stat=ios)
  DEALLOCATE(obsHead%sat,stat=ios)
  DEALLOCATE(obsHead%ambigu,stat=ios)

  RETURN
END SUBROUTINE menu_sat

END MODULE
