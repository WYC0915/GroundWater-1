MODULE s_PRITIT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE pritit(pgmnam,pgmDescr,nCol)

! -------------------------------------------------------------------------
! Purpose:    Prints the program name and the title in the protocol
!
! Author:     R.Dach
!
! Created:    30-May-2001
!
! Changes:    29-Jun-2001 RD: Include program description
!             10-Jul-2001 PF: Include PCF and User info, changed output format
!             26-Mar-2002 HU: Set filtitle (title with date and time)
!             06-Feb-2003 RD: Set Program_name and Program_descr in m_bern
!             18-Feb-2003 HU: Use pgmver from m_bern
!             19-Feb-2003 RD: New title section including default session
!             19-Mar-2003 RD: Write long string with format (because IFC)
!             23-Apr-2003 AJ: Nullify local pointers
!             17-Sep-2003 HU: Prepare filTitle if defcon not called
!             29-Dec-2003 HU: Warning modified
!             15-Dec-2011 SL: use m_bern with ONLY, write format * changed to A
!             24-Jan-2012 RD: Switch to version 5.2, remove unused variables
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, lineLength, keyValueLength, shortlineLength, &
                      lfnPrt, lfnErr, program_name, program_dscr, pgmver
  USE d_const,  ONLY: date,time,filTitle,const_def
  USE s_dattim
  USE s_readkeys
  USE s_rplenvar
  USE f_iyear4
  IMPLICIT NONE
!
! Variables from parameter list
! -----------------------------
! input:
  CHARACTER(LEN=*)       :: pgmnam   ! program name
  CHARACTER(LEN=*)       :: pgmDescr ! program descr.
  INTEGER(i4b), OPTIONAL :: nCol     ! Number of colums (if different from 79)

! output:

! Local Parameters
! ----------------
  Character(LEN=3),DIMENSION(12),PARAMETER ::    &
  Month = (/'Jan','Feb','Mar','Apr','May','Jun', &
            'Jul','Aug','Sep','Oct','Nov','Dec'/)

! Local Variables
! ---------------
  CHARACTER(LEN=lineLength),      &
          DIMENSION(7)           :: Line         ! lines to contract the strings
                                                 ! 1: '==='
                                                 ! 2: '---'
                                                 ! 3: program name and version
                                                 ! 4: program description
                                                 ! 5: campaign and pcf info
                                                 ! 6: date and user info
                                                 ! 7: user supplied title
  CHARACTER(LEN=lineLength),      &
          DIMENSION(7)           :: Line2        ! lines to contract the strings
                                                 ! 1: Bernese Software: Version 5.2
                                                 ! 2: Program name
                                                 ! 3: program description
                                                 ! 4: campaign
                                                 ! 5: default session
                                                 ! 6: date
                                                 ! 7: user name
  CHARACTER(LEN=keyValueLength),  &
          DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=keyValueLength),  &
          DIMENSION(:), POINTER  :: keyWords     ! All keywords from INP file
  CHARACTER(LEN=LineLength)      :: title        ! Title
  CHARACTER(LEN=shortlineLength) :: campgn       ! Campaign name
  CHARACTER(LEN=shortlineLength) :: datstr       ! time stamp
  CHARACTER(LEN=shortlineLength) :: sesinf       ! Session info
  CHARACTER(LEN=shortlineLength) :: usrinf       ! User info
  CHARACTER(LEN=shortlineLength), &
          DIMENSION(2)           :: seshlp
  CHARACTER(len=10),              &
          DIMENSION(2)           :: DateTime

  INTEGER(i4b)                   :: iCol, iKey
  INTEGER(i4b)                   :: ii, i1, i2, i3, i4, i5
  INTEGER(i4b)                   :: iY2
  INTEGER(i4b)                   :: irc, ios

  NULLIFY(keyValue)
  NULLIFY(keyWords)

! Set global program variables
! ----------------------------
  program_name = pgmnam
  program_dscr = pgmDescr

! Get the size of the output
! --------------------------
  Line = ''
  IF (PRESENT(nCol)) THEN
    DO iCol=1,nCol
      Line(1)=TRIM(Line(1))//'='
      Line(2)=TRIM(Line(2))//'-'
    ENDDO
  ELSE
    DO iCol=1,79
      Line(1)=TRIM(Line(1))//'='
      Line(2)=TRIM(Line(2))//'-'
    ENDDO
  ENDIF
!
! Search the campaign resp. title string
! --------------------------------------
  title    = ' '
  campgn   = ' '
  sesinf   = 'unknown'
  seshlp   = ' '
  usrinf   = ' '
  CALL readKeys('*', keyWords, irc)
  DO iKey=1,SIZE(keyWords)
    IF (keyWords(iKey) == 'TITLE') THEN
      CALL Readkeys('TITLE',keyValue,irc)
      IF (irc == 0) title = keyvalue(1)
    ENDIF
    IF (keyWords(iKey) == 'CAMPAIGN') THEN
      CALL Readkeys('CAMPAIGN',keyValue,irc)
      IF (irc == 0)  campgn = keyValue(1)
    ENDIF
    IF (keyWords(iKey) == 'YR4_INFO') THEN
      CALL Readkeys('YR4_INFO', keyValue,irc)
      IF (irc == 0) seshlp(1) = keyValue(1)
    ENDIF
    IF (keyWords(iKey) == 'SES_INFO') THEN
      CALL Readkeys('SES_INFO', keyValue,irc)
      IF (irc == 0) seshlp(2) = keyValue(1)
    ENDIF

  ENDDO

! Compose Session String
! ----------------------
  IF (LEN_TRIM(sesHlp(1))*LEN_TRIM(sesHlp(2)) > 0) THEN
    sesInf = TRIM(sesHlp(2)) // ' (' // TRIM(sesHlp(1)) // ')'
    sesInf = TRIM(sesHlp(2)) // ' year ' // TRIM(sesHlp(1))
  ENDIF

! User name
! ---------
  usrinf='${USER}'
  CALL rplEnVar(0,usrinf)
  IF (LEN_TRIM(usrinf)==0) usrinf='none'
!  IF (usrinf(1:1)=='$') usrinf='none'

! "There must be a title"
! -----------------------
  IF (LEN_TRIM(title) == 0) THEN
    WRITE(lfnerr,'(/,A,/)')                                               &
    ' ### SR PRITIT: Please enter a title to characterize the program run.'
  ENDIF

! Set the general file title
! --------------------------
  IF (const_def /= 1) CALL dattim(date,time)
  filTitle=title
  filTitle(65:80)=' '//date//' '//time

! No program description found
! ----------------------------
  IF (LEN_TRIM(pgmDescr) == 0) THEN
    WRITE(lfnerr,'(/,A,/)')                                             &
    ' ### SR PRITIT: There should be a short description of the program.'
  ENDIF

! Generate the datum / time string
! --------------------------------
  CALL date_and_time(DateTime(1),DateTime(2))
  READ(DateTime(1)(5:6),'(i2)') ii

  datstr=''
  READ(DateTime(1)(3:4),*,iostat=ios) iY2
  WRITE(datstr,'(A7,I4,1X,A8)')                                 &
        DateTime(1)(7:8)//'-'//Month(ii)//'-',iYear4(iY2),      &
        DateTime(2)(1:2)//':'//DateTime(2)(3:4)//':'//DateTime(2)(5:6)

! Width of the output (i2)
! ------------------------
  i2 = LEN_TRIM(Line(1))
  i1 = i2 - 30

! First line: Program name and software version
! ---------------------------------------------
  Line(3) = 'Program : '//TRIM(pgmnam)
  WRITE(line(3)(i1:i2),'(A)') 'Bernese Software,  Version '//TRIM(pgmver)
  line2(1) = 'Bernese GNSS Software, Version '//TRIM(pgmver)
  line2(2)= 'Program        : '//TRIM(pgmnam)

! Second line: Program description
! --------------------------------
  IF (LEN_TRIM(pgmDescr) > 0) THEN
    line(4) = 'Purpose : '//TRIM(pgmDescr)
    line2(3)= 'Purpose        : '//TRIM(pgmDescr)
  ENDIF

! Third line: Campaign and Session info (if available)
! ----------------------------------------------------
  i4 = LEN_TRIM(campgn)              ! length of campaign info
  i5 = i1 - 13                       ! width of first col. info field
  IF (i4 > 0) THEN                   ! info available
    IF (i4 > i5) THEN  ! info string too long
       i3 = i4 - i5                  ! cut from beginning
       WRITE(line(5),'(A)') 'Campaign: '//campgn(i3:i4)
    ELSE
       WRITE(line(5),'(A)') 'Campaign: '//TRIM(campgn)
    ENDIF
  ELSE
    WRITE(line(5), '(A)') 'Campaign: '
  ENDIF
  line2(4)= 'Campaign       : '//TRIM(campgn)

!
  i4 = LEN_TRIM(sesinf)
  i5 = i2 - i1 - 17                  ! width of second col info field

  IF (i4 > 0) THEN
    IF (i4 > i5) THEN
       i3 = i4 - i5
       WRITE(line(5)(i1:i2),'(A)') 'Default session: '//sesinf(i3:i4)
    ELSE
       WRITE(line(5)(i1:i2),'(A)') 'Default session: '//TRIM(sesinf)
    ENDIF
  ELSE
    WRITE(line(5)(i1:i2),'(A)') 'Default session: '
  ENDIF
  line2(5)= 'Default session: '//TRIM(sesinf)

! Fourth line: Date and User info
! -------------------------------
  write(line(6),'(A)')  'Date    : '//TRIM(datstr)
  line2(6) = 'Date           : '//TRIM(datstr)

  i4 = LEN_TRIM (usrinf)
  IF (i4 > 0) THEN
    IF (i4 > i5) THEN
       i3 = i4 - i5
       WRITE (line(6)(i1:i2),'(A)') 'User name      : '//usrinf(i3:i4)
    ELSE
       WRITE (line(6)(i1:i2),'(A)') 'User name      : '//TRIM(usrinf)
    ENDIF
  ELSE
    WRITE(line(6)(i1:i2),'(A)') 'User name      : '
  ENDIF
  line2(7) = 'User name      : '//TRIM(usrinf)

! Title line
! ----------
  Line(7) = title

! Write the title lines
! ---------------------
  i1 = LEN_TRIM(line(1))

  WRITE(lfnprt,'(A)') ''

  IF (1 == 2 ) THEN
    IF(LEN_TRIM(line(1)) > 0) WRITE(lfnprt,'(1X,A)') line(1)(1:i1)
    IF(LEN_TRIM(line(3)) > 0) WRITE(lfnprt,'(1X,A)') TRIM(line(3)(1:i1))
    IF(LEN_TRIM(line(4)) > 0) WRITE(lfnprt,'(1X,A)') TRIM(line(4)(1:i1))
    IF(LEN_TRIM(line(5)) > 0) WRITE(lfnprt,'(1X,A)') TRIM(line(5)(1:i1))
    IF(LEN_TRIM(line(6)) > 0) WRITE(lfnprt,'(1X,A)') TRIM(line(6)(1:i1))
    IF(LEN_TRIM(line(1)) > 0) WRITE(lfnprt,'(1X,A)') line(1)(1:i1)
  ELSE
    IF(LEN_TRIM(line(1))  > 0) WRITE(lfnprt,'(1X,A)') line(1)(1:i1)
    IF(LEN_TRIM(line2(1)) > 0) WRITE(lfnprt,'(1X,A)') TRIM(line2(1)(1:i1))
    IF(LEN_TRIM(line(2))  > 0) WRITE(lfnprt,'(1X,A)') line(2)(1:i1)
    IF(LEN_TRIM(line2(2)) > 0) WRITE(lfnprt,'(1X,A)') TRIM(line2(2)(1:i1))
    IF(LEN_TRIM(line2(3)) > 0) WRITE(lfnprt,'(1X,A)') TRIM(line2(3)(1:i1))
    IF(LEN_TRIM(line(2))  > 0) WRITE(lfnprt,'(1X,A)') line(2)(1:i1)
    IF(LEN_TRIM(line2(4)) > 0) WRITE(lfnprt,'(1X,A)') TRIM(line2(4)(1:i1))
    IF(LEN_TRIM(line2(5)) > 0) WRITE(lfnprt,'(1X,A)') TRIM(line2(5)(1:i1))
    IF(LEN_TRIM(line2(6)) > 0) WRITE(lfnprt,'(1X,A)') TRIM(line2(6)(1:i1))
    IF(LEN_TRIM(line2(7)) > 0) WRITE(lfnprt,'(1X,A)') TRIM(line2(7)(1:i1))
    IF(LEN_TRIM(line(1)) > 0) WRITE(lfnprt,'(1X,A)') line(1)(1:i1)
  ENDIF

! Continue only, if there is really a title
! -----------------------------------------
  IF (LEN_TRIM(title) > 0) THEN
    WRITE(lfnprt,'(/)')

    IF (LEN_TRIM(line(7)) > 0) WRITE(lfnprt,'(1X,A)') TRIM(line(7)(1:i1))
    IF (LEN_TRIM(line(2)) > 0) WRITE(lfnprt,'(1X,A)') line(2)(1:i1)
  ENDIF

  WRITE(lfnprt,'(/)')

  DEALLOCATE(keyValue,stat=irc)
  DEALLOCATE(keyWords,stat=irc)

  RETURN

  END SUBROUTINE pritit

END MODULE
