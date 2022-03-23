MODULE s_READSTSG
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE readstsg(filnam, nSigma, staList)

! -------------------------------------------------------------------------
! Purpose:    Reads station selection / station sigma files
!               - It reads also old formatted .FIX and .SIG files
!               - nSigma == 0: for a station list without sigmas
!                              (the staList%sigma array is not allocated!)
!               - nSigma == 1: Coordinate/Velocity sigmas (ADDNEQ2)
!               - nSigma == 2: Troposhere sigmas (abs_vert, rel_vert)
!               - nSigma == 3: Coordinate/Velocity sigmas (north, east, up)
!               - nSigma == 4: Troposhere sigmas
!                              (abs_vert, rel_vert, abs_grd, rel_grd)
!               - nSigma == -1: number of sigmas unknown
!               - nSigma must be equal to the number of sigma columns in the
!                      file (otherwise the SR will stop the pgm with an err!)
!
! Remark:     staList%nSta may be smaller than the allcated size of the
!             arrays in the staList%stanam resp. staList%sigma
!             (it occures only, if the old format is red)
!
! Author:     R. Dach
!
! Created:    23-Aug-2001
! Last mod.:  18-Oct-2003
!
! Changes:    27-Aug-2001  RD: nSigma == -1 means unknown number of columns
!             01-Feb-2002  RD: Replace "*" by "9" for sigma values
!             07-May-2002  RD: Read empty files correct
!             18-Aug-2003  RD: Close file
!             18-Oct-2003  HU: Truncate filename in title
!
! SR used:    opnfil, opnerr, alcerr, exitrc
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_stalst, ONLY: t_staList
  USE s_opnfil
  USE s_alcerr
  USE s_opnerr
  USE s_dattim
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)  :: filNam    ! Name of the file

! input/output:
  INTEGER(i4b)      :: nSigma    ! Number of sigmas expected

! output:
  TYPE(t_staList)   :: staList   ! Station list with sigmas

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=lineLength) :: Line   ! Buffer to read a line from file
  CHARACTER(LEN=9)          :: date
  CHARACTER(LEN=5)          :: time

  INTEGER(i4b)              :: iFmt   ! Format 1: Version 4.x
                                      !        2: Version 5.0 (or higher)
  INTEGER(i4b)              :: nSta   ! Number of stations for allocation
  INTEGER(i4b)              :: mLen   ! the length of the longest line in the
                                      ! data area
  INTEGER(i4b)              :: iSig
  INTEGER(i4b)              :: ii, i1, i2
  INTEGER(i4b)              :: ios

! Init variables
! --------------
  iFmt = 0
  nSta = 0
  mLen = 0

! Open the file
! -------------
  CALL opnfil(lfnloc,filnam,'OLD','FORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,filnam,'readstsg')

! Get the 1st and 2nd line from the file to detect the format
! -----------------------------------------------------------
  READ(lfnloc,'(A)',iostat=ios) line

! Check 1st line
  IF (ios == 0 .AND. LEN_TRIM(line) >= 5) THEN
    iFmt = 1
    nSta = nSta + 1
    IF (LEN_TRIM(line) > mLen) mLen = LEN_TRIM(line)
  ENDIF

  READ(lfnloc,'(A)',iostat=ios) line

! Check 2nd line - new format
  IF (ios == 0 .AND. line(1:5) == '-----') THEN
    iFmt = 2
    nSta = 0
    mLen = 0
    DO ii = 1, 3
      READ(lfnloc,'(A)',iostat=ios) line
      IF (LEN_TRIM(line) > mLen) mLen = LEN_TRIM(line)
    ENDDO
  ELSE IF (ios == 0 .AND. LEN_TRIM(line) >= 5) THEN
    nSta = nSta + 1
    IF (iFmt == 1 .AND. LEN_TRIM(line) > mLen) mLen = LEN_TRIM(line)
  ENDIF

! Get the number of entries
! -------------------------
  DO WHILE (ios == 0)
    READ(lfnloc,'(A)',iostat=ios) line

! emty line: no more valid entries
    IF (LEN_TRIM(line) == 0) ios = 1

! old format
    IF (ios == 0 .AND. iFmt == 1 .AND. LEN_TRIM(line) >= 5) nSta = nSta + 1

! new format
    IF (ios == 0 .AND. iFmt == 2 .AND. LEN_TRIM(line) >  0) nSta = nSta + 1

! Get the longest line
    IF (iFmt == 1 .AND. LEN_TRIM(line) > mLen) mLen = LEN_TRIM(line)
  ENDDO

! Compute the number of sigmas if it is unknown
! ---------------------------------------------
  IF (nSigma == -1) THEN
    IF (iFmt == 1) nSigma = (mLen - 15) /  7
    IF (iFmt == 2) nSigma = (mLen - 11) / 10
  ENDIF

! Allocate the memory
! -------------------
  DEALLOCATE(staList%staNam, stat=ios)

  ALLOCATE(staList%staNam(nSta), stat=ios)
  CALL alcerr(ios, 'staList%staNam', (/nSta/), 'readstsg')

  IF (nSigma > 0) THEN
    DEALLOCATE(staList%sigma, stat=ios)

    ALLOCATE(staList%sigma(nSigma,nSta), stat=ios)
    CALL alcerr(ios, 'staList%sigma', (/nSigma,nSta/), 'readstsg')
  ENDIF

! Start reading the file
! ----------------------
  REWIND(lfnloc)

  staList%nSta = 0
  IF (iFmt == 1) THEN

    ii=LEN_TRIM(filnam)
    IF (ii > 44) THEN
      staList%Title = 'Converted from file ..' // filnam(ii-40:ii)
    ELSE
      staList%Title = 'Converted from file ' // TRIM(filnam)
    ENDIF

! Date and time in title line
    CALL dattim(date,time)
    staList%Title(65:80) = ' '//date//' '//time

  ELSE IF (iFmt == 2) THEN
    READ(lfnloc,'(A)') staList%Title
    DO ii = 1,4
      READ(lfnloc,*,iostat=ios)
    ENDDO
  ENDIF

! Read the data records
! ---------------------
  ios = 0
  DO WHILE (ios == 0 .AND. staList%nSta < nSta)
    READ(lfnloc,'(A)',iostat=ios) line

! Stop reading at an empty line
! -----------------------------
    IF (ios == 0 .AND. LEN_TRIM(line) == 0) ios = 1

! Read a data record from the old format
! --------------------------------------
    IF (ios == 0 .AND. iFmt == 1) THEN

      staList%staNam(staList%nSta+1) = line(6:21)

! Replace "*" by "9"
      DO ii = 22,LEN_TRIM(line)
        IF (line(ii:ii) == '*') line(ii:ii) = '9'
      ENDDO

! Station selection list (only stations without sigma behind
      IF (nSigma == 0 .AND. LEN_TRIM(line) <= 21) THEN
        staList%nSta = staList%nSta + 1

! Coordinate/Velocity sigmas, but ADDNEQ2 -- only 1 sigma for all components
      ELSE IF ((nSigma == 1 .OR. nSigma == 3) .AND. &
               LEN_TRIM(line) >= 22.AND. LEN_TRIM(line) <= 28) THEN
        READ(line(22:28),*,iostat=ios) staList%sigma(1,staList%nSta+1)

        IF ( ios /= 0 .OR. staList%sigma(1,staList%nSta+1) < 0d0) THEN
          WRITE(lfnerr,'(/,A,2(/,18X,A,A),/,18X,A,A,/)')                       &
          ' *** SR READSTSG: Error reading sigmas from file (old format)',     &
                            'File name:           ',TRIM(filnam),              &
                            'Station name:        ',&
                                         TRIM(staList%staNam(staList%nSta+1)), &
                            'Sigma in column  1:  ',line(22:28)
          CALL exitrc(2)
        ENDIF

        staList%nSta = staList%nSta + 1
        IF (nSigma == 3) THEN
          staList%sigma(2,staList%nSta+1) = staList%sigma(1,staList%nSta+1)
          staList%sigma(3,staList%nSta+1) = staList%sigma(1,staList%nSta+1)
        ENDIF

! Coordinate/Velocity sigmas, separate sigmas for all components
      ELSE IF (nSigma == 3 .AND. &
               LEN_TRIM(line) >= 36.AND. LEN_TRIM(line) <= 42) THEN
        DO iSig = 1, nSigma
          i1 = 22 + 7 * (iSig-1)
          i2 = 22 + 7 *  iSig
          READ(line(i1:i2),*,iostat=ios) staList%sigma(iSig, staList%nSta+1)

          IF ( ios /= 0 .OR. staList%sigma(iSig,staList%nSta+1) < 0d0) THEN
            WRITE(lfnerr,'(/,A,2(/,18X,A,A),/,18X,A,I2,A,A,/)')                &
            ' *** SR READSTSG: Error reading sigmas from file (old format)',   &
                              'File name:           ',TRIM(filnam),            &
                              'Station name:        ',&
                                         TRIM(staList%staNam(staList%nSta+1)), &
                              'Sigma in column ',iSig,':  ',line(i1:i2)
            CALL exitrc(2)
          ENDIF
        ENDDO

        staList%nSta = staList%nSta + 1

! Troposphere sigmas (only 2 column sigma file; without gradients)
      ELSE IF (nSigma == 2 .AND. &
               LEN_TRIM(line) >= 29 .AND. LEN_TRIM(line) <= 35) THEN
        DO iSig = 1, 2
          i1 = 22 + 7 * (iSig-1)
          i2 = 22 + 7 *  iSig
          READ(line(i1:i2),*,iostat=ios) staList%sigma(iSig,staList%nSta+1)

          IF ( ios /= 0 .OR. staList%sigma(iSig,staList%nSta+1) < 0d0) THEN
            WRITE(lfnerr,'(/,A,2(/,18X,A,A),/,18X,A,I2,A,A,/)')                &
            ' *** SR READSTSG: Error reading sigmas from file (old format)',   &
                              'File name:           ',TRIM(filnam),            &
                              'Station name:        ',&
                                         TRIM(staList%staNam(staList%nSta+1)), &
                              'Sigma in column ',iSig,':  ',line(i1:i2)
            CALL exitrc(2)
          ENDIF
        ENDDO

        staList%nSta = staList%nSta + 1

! Troposphere sigmas with gradients
      ELSE IF ((nSigma == 2 .OR. nSigma == 4 ).AND. &
               LEN_TRIM(line) >= 43.AND. LEN_TRIM(line) <= 49) THEN
        DO iSig = 1, nSigma
          i1 = 22 + 7 * (iSig-1)
          i2 = 22 + 7 *  iSig
          READ(line(i1:i2),*,iostat=ios) staList%sigma(iSig,staList%nSta+1)

          IF ( ios /= 0 .OR. staList%sigma(iSig,staList%nSta+1) < 0d0) THEN
            WRITE(lfnerr,'(/,A,2(/,18X,A,A),/,18X,A,I2,A,A,/)')                &
            ' *** SR READSTSG: Error reading sigmas from file (old format)',   &
                              'File name:           ',TRIM(filnam),            &
                              'Station name:        ',&
                                         TRIM(staList%staNam(staList%nSta+1)), &
                              'Sigma in column ',iSig,':  ',line(22:28)
            CALL exitrc(2)
          ENDIF
        ENDDO

        staList%nSta = staList%nSta + 1

! Unknown or invalid format
      ELSE IF (nSigma /= 0 .AND. LEN_TRIM(line) > 21) THEN
        WRITE(lfnerr,'(/,A,2(/,18X,A,A),/,18X,A,I2,/)')                        &
        ' *** SR READSTSG: Error reading station sigma file (old format).',    &
                          'File name:                 ', TRIM(filnam),         &
                          'Station name:              ', &
                                         TRIM(staList%staNam(staList%nSta+1)), &
                          'Number of colums expected: ',nSigma
        CALL exitrc(2)
      ENDIF

! Read a data record from the new format
! --------------------------------------
    ELSE IF (ios == 0 .AND. iFmt == 2) THEN

      staList%staNam(staList%nSta+1) = line(1:16)

! Replace "*" by "9"
      DO ii = 17,LEN_TRIM(line)
        IF (line(ii:ii) == '*') line(ii:ii) = '9'
      ENDDO

      DO iSig = 1, nSigma

        i1 = 21 + 10 * (iSig-1)
        i2 = 20 + 10 *  iSig
        READ(line(i1:i2),*,iostat=ios) staList%sigma(iSig, staList%nSta+1)

        IF ((ios /= 0 .AND. LEN_TRIM(line(i1:i2)) > 0) .OR. &
            staList%sigma(iSig,staList%nSta+1) < 0d0   ) THEN
          WRITE(lfnerr,'(/,A,2(/,18X,A,A),/,18X,A,I2,A,A,/)')                  &
          ' *** SR READSTSG: Error reading sigmas from file',                  &
                            'File name:           ',TRIM(filnam),              &
                            'Station name:        ',&
                                         TRIM(staList%staNam(staList%nSta+1)), &
                            'Sigma in column ',iSig,':  ',line(i1:i2)
          CALL exitrc(2)
        ENDIF
      ENDDO ! all sigmas are red

! A station selection file has only 1 col. with the station names
      IF (nSigma == 0 .AND. LEN_TRIM(line) > 20) THEN
        WRITE(lfnerr,'(/,A,2(/,18X,A,A),/,18X,A,/)')                           &
        ' *** SR READSTSG: Error reading station selection file.',             &
                          'File name:     ', TRIM(filnam),                     &
                          'Station name:  ', &
                                         TRIM(staList%staNam(staList%nSta+1)), &
                          'Only the station name is expected on the line.'
        CALL exitrc(2)

! Check the number of columns in the station sigma file
      ELSE IF (nSigma /= 0 .AND. &
               (LEN_TRIM(line) < 21 + 10 * (nSigma-1) .OR. &
                LEN_TRIM(line) > 20 + 10 * nSigma)) THEN
        WRITE(lfnerr,'(/,A,2(/,18X,A,A),/18X,A,I10,/)')                        &
        ' *** SR READSTSG: Error reading station sigma file.',                 &
                          'File name:                 ', TRIM(filnam),         &
                          'Station name:              ', &
                                         TRIM(staList%staNam(staList%nSta+1)), &
                          'Number of colums expected: ',nSigma
        CALL exitrc(2)
      ENDIF

! All was OK
      staList%nSta = staList%nSta + 1

! Unknown format
! --------------
    ELSE IF (ios == 0) THEN
      WRITE(lfnerr,'(/,A)') &
      ' *** SR READSTSG: The format of the file is unknown.'
      IF (nSigma == 0) WRITE(lfnerr,'(18X,A)') &
        'A station selection file is expected here.'
      IF (nSigma > 0) WRITE(lfnerr,'(18X,A,I2,A)') &
        'A station sigma file with ',nSigma,' columns is expected here.'
      WRITE(lfnerr,'(18X,A,A,/)') 'File name:           ', TRIM(filnam)

      CALL exitrc(2)
    ENDIF ! which format
  ENDDO ! next line

! Close file
! ----------
  CLOSE(lfnloc)

  RETURN
END SUBROUTINE readstsg

END MODULE
