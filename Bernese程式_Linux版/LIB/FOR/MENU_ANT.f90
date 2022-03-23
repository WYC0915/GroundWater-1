MODULE s_MENU_ANT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_ant(keyWord, output)

! -------------------------------------------------------------------------
! Purpose:    Generates a list of all receiver/antennas from the obs. files
!             in GPSEST (called by the menu program via MENUAUX)
!
! Author:     R. Dach
!
! Created:    20-Jun-2001
! Last mod.:  29-Oct-2003
!
! Changes:    05-Sep-2001  HU: Interface for rdhead2 added
!             18-Apr-2002  RD: Use keywords from MENUAUX.INP
!             22-Jul-2002  HB: Use modified t_obsHead
!             09-Jan-2003  RD: Use single quotes as separators
!             23-Apr-2003  AJ: Nullify local pointers
!             19-May-2003  CU: Initialize structure
!             08-Sep-2003  HU: antnam, recnam chr16 -> chr20
!             29-Oct-2003  RS: Receiver-independent antenna lists, change
!                              recnam and antnam in lists
!
! SR called:  alcerr, gtfile2, rdhead2, tstkey
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_gpsobs, ONLY: t_obshead, init_obshead
  USE f_tstkey
  USE s_gtfile2
  USE s_rdhead2
  USE s_alcerr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)                        :: keyWord  ! what to do

! output:
  TYPE(t_key)                             :: output   ! value: Result to display

! Local Types
! -----------
  TYPE t_antList
    CHARACTER(LEN=20)                     :: antnam
    CHARACTER(LEN=20)                     :: recnam
    INTEGER(i4b)                          :: antnum
  END TYPE t_antList

! Local Parameters
! ----------------
  CHARACTER(LEN=8),PARAMETER              :: srName  = 'menu_ant'

  CHARACTER(LEN=7),DIMENSION(4),PARAMETER :: filKeyw = &
           (/ 'PSFILES','CSFILES','PZFILES','CZFILES' /)

! Local Variables
! ---------------
  TYPE(t_obshead)               :: obshead           ! Header of obs. files
  TYPE(t_antList),               &
         DIMENSION(:),POINTER   :: antList           ! List of antennas resp.
                                                     ! antenna groups
  TYPE(t_antList)               :: antHelp

  CHARACTER(LEN=fileNameLength), &
         DIMENSION(:,:),POINTER :: filList           ! List of obs. files

  INTEGER(i4b)                  :: numFil            ! # of entries
  INTEGER(i4b)                  :: numAnt            ! # in list of antennas
  INTEGER(i4b)                  :: iKey,iFil
  INTEGER(i4b)                  :: iSta, jAnt
  INTEGER(i4b)                  :: ii
  INTEGER(i4b)                  :: ios,irc

  LOGICAL                       :: inList

  NULLIFY(antList)
  NULLIFY(filList)
  CALL init_obshead(obshead)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'ANTLST' .AND. keyWord /= 'ANTGRP' .AND. &
      keyWord /= 'ANTLSTRI' .AND. keyWord /= 'ANTGRPRI') RETURN

! Count the files
! ---------------
  numAnt = 0
  DO iKey = 1,SIZE(filKeyw)

    IF (.NOT. tstKey(filKeyw(iKey))) CYCLE

    CALL gtfile2(filKeyw(iKey),1,numFil,filList)

    DEALLOCATE(filList,stat=irc)

    numAnt = numAnt + 2*numFil
  ENDDO

! Allocate the list of antennas
! -----------------------------
  ALLOCATE(antList(numAnt),stat=ios)
  CALL alcerr(ios,'antList',(/numAnt/),srName)

  numAnt = 0

! Get the list of antennas from the observation files
! ---------------------------------------------------
  iKeyLoop: DO iKey = 1,SIZE(filKeyw)
    IF (.NOT. tstKey(filKeyw(iKey))) CYCLE

    CALL gtfile2(filKeyw(iKey),1,numFil,filList)

    DO iFil = 1, numFil

      IF (LEN_TRIM(filList(1,iFil)) == 0) CYCLE

      CALL rdhead2(filList(1,iFil), obshead)

      DO ista = 1, obshead%ndiff + 1

! Is it a new antenna?
        inList = .FALSE.
        DO jAnt = 1, numAnt
          IF ( antList(jAnt)%antnam == obshead%sta(ista)%anttyp    .AND. &
              (antList(jAnt)%antnum == obshead%sta(ista)%ianten .OR.     &
               keyWord == 'ANTGRP' .OR. keyWord == 'ANTGRPRI')     .AND. &
              (antList(jAnt)%recnam == obshead%sta(ista)%rectyp .OR.     &
               keyWord == 'ANTLSTRI' .OR. keyWord == 'ANTGRPRI')    ) THEN
            inList = .TRUE.
            EXIT
          ENDIF
        ENDDO

! Add a record to the list
! ------------------------
        IF ( .NOT. inList ) THEN

          numAnt = numAnt + 1

          IF (numAnt > SIZE(antList)) THEN
            WRITE(lfnerr,'(/,A,2(/,18X,A),/)')                       &
            ' *** SR MENU_ANT: Should never happen.',                &
            'There must be more than two stations in at least one ', &
            'of the observation files.'
            EXIT iKeyLoop
          ENDIF

          antList(numAnt)%antnam = obshead%sta(ista)%anttyp
          antList(numAnt)%antnum = obshead%sta(ista)%ianten
          antList(numAnt)%recnam = obshead%sta(ista)%rectyp
        ENDIF
      ENDDO ! next station

    ENDDO ! next file

    DEALLOCATE(filList,stat=ios)

  ENDDO iKeyLoop
  DEALLOCATE(obshead%sat,obshead%ambigu,stat=ios)

!  IF (numAnt == 0) RETURN

! Sort the list of antennas
! -------------------------
  inList = .TRUE.
  DO WHILE (inList)
    inList = .FALSE.
    ii = 1
    DO WHILE (ii < numAnt)
      IF ( antList(ii)%antnam > antList(ii+1)%antnam      .OR. &
          (antList(ii)%antnam == antList(ii+1)%antnam .AND. &
           antList(ii)%recnam > antList(ii+1)%recnam)     .OR. &
          (antList(ii)%antnam == antList(ii+1)%antnam .AND. &
           antList(ii)%recnam == antList(ii+1)%recnam .AND. &
           antList(ii)%antnum > antList(ii+1)%antnum)     ) THEN
        antHelp       = antList(ii)
        antList(ii)   = antList(ii+1)
        antList(ii+1) = antHelp
        inList = .TRUE.
      ENDIF
      ii = ii + 1
    ENDDO
  ENDDO

! Put the results into the output-key
! -----------------------------------
  ALLOCATE(output%value(numAnt),stat=irc)
  CALL alcerr(irc,'output%value',(/ numAnt /),srName)
  output%value = ' '

  IF (keyWord == 'ANTLST') THEN
    DO jAnt = 1, numAnt
      WRITE(output%value(jAnt), '(2(A,4X,2X),A,I6,A)')  &
            "'"//antList(jAnt)%antnam(1:20)//"'",       &
            "'"//antList(jAnt)%recnam(1:20)//"'",       &
            "'", antList(jAnt)%antnum,       "'"
    ENDDO
  ELSEIF (keyWord == 'ANTLSTRI') THEN
    DO jAnt = 1, numAnt
      WRITE(output%value(jAnt), '(A,4X,2X,A,I6,A)')     &
            "'"//antList(jAnt)%antnam(1:20)//"'",       &
            "'", antList(jAnt)%antnum,       "'"
    ENDDO
  ELSEIF (keyWord == 'ANTGRP') THEN
    DO jAnt = 1, numAnt
      WRITE(output%value(jAnt), '(A,4X,2X,A,4X)')       &
            "'"//antList(jAnt)%antnam(1:20)//"'",       &
            "'"//antList(jAnt)%recnam(1:20)//"'"
    ENDDO
  ELSEIF (keyWord == 'ANTGRPRI') THEN
    DO jAnt = 1, numAnt
      WRITE(output%value(jAnt), '(A,4X)')       &
            "'"//antList(jAnt)%antnam(1:20)//"'"
    ENDDO
  ENDIF

  DEALLOCATE (antList, stat=ios)

  RETURN
END SUBROUTINE menu_ant

END MODULE
