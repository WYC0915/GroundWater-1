MODULE s_GETCO3
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE getco3(filnam, nFlag,  flags,  nStat,  stName, &
                    title,  datum,  timcrd, staNum, xStat,  &
                    staFlg, staFlg5,plate,  footer)

! -------------------------------------------------------------------------
! Purpose:    Get all stations names and coordinates with flag in
!             "flags"
!             if one of the flags is '@' all stations are returned
!             if one of the flags is '#' all stations with non-
!             blank flags are returned
!
! Remark:     Converted to F90 from GETCO3.f (created by C. Rocken)
!
! Author:     R. Dach
!
! Created:    08-May-2002
! Last mod.:  20-Jul-2011
!
! Changes:    17-Jun-2003 MM: staFlg5 added
!             29-Dec-2003 HU: implicit none added
!             21-Sep-2010 RD: ST2TIM can be used as a module now
!             02-Feb-2011 RD: reading of footer lines added
!             23-Feb-2011 RD: Stop if more than one entry per station found
!             20-Jul-2011 RD: More digits
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_alcerr
  USE s_opnfil
  USE f_lincount
  USE s_st2tim
  USE s_exitrc
  USE s_opnerr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)               :: filnam ! Name of coordinate file
  INTEGER(i4b)                   :: nFlag  ! Number of flags
  CHARACTER(LEN=1), DIMENSION(*) :: flags  ! 1,nflag: flags to be searched for

! output:
  INTEGER(i4b)                   :: nStat  ! #stations with flag in "flags"
  CHARACTER(LEN=staNameLength),   &
         DIMENSION(:), POINTER   :: stName ! 1,nstat: station names
!
  CHARACTER(LEN=80),OPTIONAL     :: title  ! Title line of the file
  CHARACTER(LEN=16),OPTIONAL     :: datum  ! Name of the local geod. datum
  REAL(r8b),        OPTIONAL     :: timcrd ! Epoch of the coordinates
  INTEGER(i4b),    OPTIONAL,      &
         DIMENSION(:), POINTER   :: staNum ! 1,nstat: station numbers
  REAL(r8b)       ,OPTIONAL,      &
         DIMENSION(:,:), POINTER :: xStat  ! 1,nstat: station xyz coordinates
  CHARACTER(LEN=1),OPTIONAL,      &
         DIMENSION(:), POINTER   :: staFlg ! 1,nstat: station flags
  CHARACTER(LEN=staFla2Length),OPTIONAL,      &
         DIMENSION(:), POINTER   :: staFlg5! 1,nstat: station flags (5 char)
  CHARACTER(LEN=4), OPTIONAL,     &
         DIMENSION(:),POINTER    :: plate  ! 1,nstat: tectonic plate
  CHARACTER(LEN=*), OPTIONAL,     &
         DIMENSION(:),POINTER    :: footer ! footer (comment) lines

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER   :: srName = 'getco3'

! Local Variables
! ---------------
  CHARACTER(LEN=LineLength)     :: line

  INTEGER(i4b)                  :: maxSta   ! Number of lines in file
  INTEGER(i4b)                  :: nComment ! Number of comment lines below
                                            ! the data area
  INTEGER(i4b)                  :: iSta,jSta
  INTEGER(i4b)                  :: iFlg
  INTEGER(i4b)                  :: ii
  INTEGER(i4b)                  :: ios
  INTEGER(i4b)                  :: irc
  INTEGER(i4b)                  :: ityp


! Get the size of the file
! ------------------------
  maxSta = linCount(filNam,6,nComment)

! Allocate the memory
! -------------------
  IF (ASSOCIATED(stname)) DEALLOCATE(stname, stat=irc)
  ALLOCATE(stname(maxSta), stat=irc)
  CALL alcerr(irc, 'stname', (/maxSta/), srName)

  IF (PRESENT(staNum)) THEN
    IF (ASSOCIATED(stanum)) DEALLOCATE(stanum, stat=irc)
    ALLOCATE(stanum(maxSta), stat=irc)
    CALL alcerr(irc, 'stanum', (/maxSta/), srName)
    staNum = 0
  ENDIF

  IF (PRESENT(xStat)) THEN
    IF (ASSOCIATED(xstat)) DEALLOCATE(xstat, stat=irc)
    ALLOCATE(xstat(3,maxSta), stat=irc)
    CALL alcerr(irc, 'xstat', (/3,maxSta/), srName)
    xStat = 0d0
  ENDIF

  IF (PRESENT(staFlg)) THEN
    IF (ASSOCIATED(staflg)) DEALLOCATE(staflg, stat=irc)
    ALLOCATE(staflg(maxSta), stat=irc)
    CALL alcerr(irc, 'staflg', (/maxSta/), srName)
    staFlg = ' '
  ENDIF

  IF (PRESENT(staFlg5)) THEN
    IF (ASSOCIATED(staflg5)) DEALLOCATE(staflg5, stat=irc)
    ALLOCATE(staflg5(maxSta), stat=irc)
    CALL alcerr(irc, 'staflg5', (/maxSta/), srName)
    staFlg5 = '     '
  ENDIF

  IF (PRESENT(plate)) THEN
    IF (ASSOCIATED(plate)) DEALLOCATE(plate, stat=irc)
    ALLOCATE(plate(maxSta), stat=irc)
    CALL alcerr(irc, 'plate', (/maxSta/), srName)
    plate = ' '
  ENDIF

  IF (PRESENT(footer)) THEN
    IF (ASSOCIATED(footer)) DEALLOCATE(footer, stat=irc)
    ALLOCATE(footer(nComment), stat=irc)
    CALL alcerr(irc, 'footer', (/nComment/), srName)
    footer = ' '
  ENDIF

! Open coordinate file
! --------------------
  CALL opnfil(lfnloc,filnam,'OLD','FORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,filnam,srName)

! Read title line and datum
! -------------------------
  READ(lfnloc,'(A)',iostat=ios) line
  IF (PRESENT(title))  title = line

  READ(lfnloc,'(/,A,///)',iostat=ios) line
  IF (PRESENT(datum))  datum = line(23:38)
  IF (PRESENT(timcrd)) CALL st2tim(1,1,line(48:66),timcrd)

  IF (ios /= 0) THEN
    WRITE(lfnerr,'(/,A,/,16X,A,/)')                             &
    ' *** SR GETCO3: Error reading header of coordinate file',  &
                    'File name:  ' // TRIM(filnam)
    CALL exitrc(2)
  ENDIF

! Determine type of station coordinates (x,y,z or ellipt.)
! --------------------------------------------------------
  READ(lfnloc,'(A)',iostat=ios) line

  iTyp = 2                               ! BLH file
  IF (line(31:31) == '.' .OR. line(32:32) == '.') iTyp = 1       ! XYZ file

! Loop over all station in the file
! ---------------------------------
  nStat = 0
  DO WHILE(ios == 0 .AND. nStat < maxSta)

! End of file reached ?
! ---------------------
    IF (LEN_TRIM(line(6:22)) == 0) EXIT

! Flag of station ok ?
! --------------------
    DO iFlg=1,nFlag
      IF((flags(iFlg) == '@')                          .OR. &
         (flags(iFlg) == '#' .AND. line(71:71) /= ' ') .OR. &
         (flags(iFlg) == line(71:71))                 ) THEN

! Add new station to station list to be returned
! ----------------------------------------------
        nStat = nStat + 1

        stName(nStat)=line(6:22)

        IF (ios == 0 .AND. PRESENT(staNum)) &
          READ(line(1:3),'(I3)',iostat=ios) staNum(nStat)

        IF (ios == 0 .AND. iTyp == 1 .AND. PRESENT(xStat))  &
          READ(line(22:66),'(3F15.5)',iostat=ios) (xStat(ii,nStat),ii=1,3)

        IF (ios == 0 .AND. PRESENT(staFlg)) &
          staFlg(nStat) = line(71:71)

        IF (ios == 0 .AND. PRESENT(staFlg5)) &
          staFlg5(nStat) = line(71:75)

        IF (ios == 0 .AND. PRESENT(plate)) &
          plate(nStat) = line(76:79)

        IF (ios /= 0) THEN
          WRITE(lfnerr,'(/,A,16X,2A,/,16X,A,I6,/)')                        &
          ' *** SR GETCO3: Error reading data lines for coordinate file',  &
                          'File name:   ', TRIM(filnam),                   &
                          'Line number: ', nStat
          CALL exitrc(2)
        ENDIF

        EXIT
      ENDIF
    ENDDO ! Loop flag list

! Read the next data line
! -----------------------
    READ(lfnloc,'(A)',iostat=ios) line

  ENDDO ! Loop coord file

! Read the commenting lines below the data area
! ---------------------------------------------
  IF (PRESENT(footer)) THEN
    nComment = 0
    DO WHILE (ios == 0 .AND. nComment < SIZE(footer))
      nComment = nComment + 1
      footer(nComment) = line
      READ(lfnloc,'(A)',iostat=ios) line
    ENDDO
  ENDIF

! Ready: close the file
  CLOSE(lfnloc)

! Check for double-entries of stations
! ------------------------------------
  DO iSta = 1,nStat
    DO jSta = iSta+1,nStat
      IF (stName(iSta) == stName(jSta)) THEN
        WRITE(lfnerr,'(/,A,2(/,16X,A),/)')                          &
             ' *** SR GETCO3: More than one entry for station "' // &
                              TRIM(stName(iSta))//'"',              &
                             'found in file: '//TRIM(filnam),       &
                             'Processing is stopped.'
        CALL exitrc(2)
      ENDIF
    ENDDO
  ENDDO

  RETURN
END SUBROUTINE getco3

END MODULE
