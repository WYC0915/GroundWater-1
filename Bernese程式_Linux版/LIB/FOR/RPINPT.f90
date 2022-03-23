MODULE s_RPINPT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rpinpt(title,tfirst,tlast,dttab,coosys,agency,maxfil,nfil,     &
                  filtab,iExShi,ifrmat)

! -------------------------------------------------------------------------
!
! Purpose:    This subroutine reads all input parameters for the program
!             RXNPRE from the corresponding input file
!
! Author:     D. Ineichen
!
! Created:    11-Apr-2001
!
! Changes:    26-Jun-2001 RD: Use alcerr for allocation
!             16-Feb-2002 DI: Add option iExShi (exclude shifted GLONASS sats)
!             12-Nov-2002 HU: Read output orbit format
!             23-Apr-2003 RD: Nullify local pointers
!             20-May-2003 RD: Use SR gttimwin instead of readsess
!             09-Aug-2010 SL/RD: filtab number of filgal and filpre changed
!             25-Aug-2010 RD: Check the number of files from each GNSS
!             11-May-2011 HB: Set model key for SR GMST2000
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,  ONLY: i4b, r8b, lfnerr, fileNameLength, keyValueLength
  USE d_model, ONLY: setModKey,chrValLength,mod_orb_subMod

  USE s_alcerr
  USE s_gtfile
  USE s_gttimwin
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptc
  IMPLICIT NONE

! List of Parameters
! ------------------
  REAL(r8b)                       :: tfirst ! MJD of first epoch of precise orb
  REAL(r8b)                       :: tlast  ! MJD of last epoch of precise orb
  REAL(r8b)                       :: dttab  ! Tabular interval for precise orb
  CHARACTER(LEN=5)                :: coosys ! Coordinate system for precise orb
  CHARACTER(LEN=4)                :: agency ! Responsible agency for the orbits
  INTEGER(i4b)                    :: maxfil ! Maximum number of processed files
  INTEGER(i4b)                    :: nfil   ! Number of processed files
  INTEGER(i4b)                    :: iExShi ! Exclude GLONASS sat. with shifts
  INTEGER(i4b)                    :: ifrmat ! Format of precise orbit file
                                            !  =2: SP3, =4: SP3c

  CHARACTER(LEN=57), DIMENSION(4)    :: title  ! 4 comment lines for the
                                               ! precise orbit file
  CHARACTER(LEN=32), DIMENSION(4,*)  :: filtab ! Files to be processed

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=32), DIMENSION(:,:),ALLOCATABLE  :: filgps,filglo,filgal
  CHARACTER(LEN=32), DIMENSION(:),ALLOCATABLE    :: filpre
  CHARACTER(LEN=chrValLength) :: chrVal
  CHARACTER(LEN=8),PARAMETER :: srName = 'rpinpt  '

  INTEGER(i4b)               :: irc, ioerr, ios, nfilgps, nfilglo, nfilgal
  INTEGER(i4b)               :: nflcol, nfilpre, ii
  INTEGER(i4b)               :: iac
  REAL(r8b), DIMENSION(2)    :: window
  REAL(r8b)                  :: numVal

  INTEGER(i4b), PARAMETER    :: maxday = 30

! Init variables
! --------------
  ioerr = 0

  NULLIFY(keyValue)


  ALLOCATE(filgps(2,maxfil), stat=iac)
  CALL alcerr(iac, 'filgps', (/2,maxfil/), 'rpinpt')
  ALLOCATE(filglo(2,maxfil), stat=iac)
  CALL alcerr(iac, 'filglo', (/2,maxfil/), 'rpinpt')
  ALLOCATE(filgal(2,maxfil), stat=iac)
  CALL alcerr(iac, 'filgal', (/2,maxfil/), 'rpinpt')
  ALLOCATE(filpre(maxfil), stat=iac)
  CALL alcerr(iac, 'filpre', (/maxfil/), 'rpinpt')

  filgps = ''
  filglo = ''
  filgal = ''
  filpre = ''

! Read Input and Output Files
! ---------------------------
  nflcol = 2
  CALL gtfile('RNXGPS ',nflcol,maxfil,nfilgps,filgps)
  CALL gtfile('RNXGLO ',nflcol,maxfil,nfilglo,filglo)
  CALL gtfile('RNXGAL ',nflcol,maxfil,nfilgal,filgal)

  nflcol = 1
  CALL gtfile('PREFIL ',nflcol,maxfil,nfilpre,filpre)

! Check that we have the same number of files from each GNSS
! ----------------------------------------------------------
  IF ( ( nfilgps > 0 .AND. nfilgps /= MAX(nfilgps,nfilglo,nfilgal)) .OR. &
       ( nfilglo > 0 .AND. nfilglo /= MAX(nfilgps,nfilglo,nfilgal)) .OR. &
       ( nfilgal > 0 .AND. nfilgal /= MAX(nfilgps,nfilglo,nfilgal)) ) THEN
    WRITE(lfnerr,'(/,A)') &
    ' *** SR RPINPT: Different number of input files from different GNSS'
    IF ( nfilgps > 0 ) WRITE(lfnerr,'(16X,A,I5)') &
                             'GPS     Navigation files:',nfilgps
    IF ( nfilglo > 0 ) WRITE(lfnerr,'(16X,A,I5)') &
                             'GLONASS Navigation files:',nfilglo
    IF ( nfilgal > 0 ) WRITE(lfnerr,'(16X,A,I5)') &
                             'Galileo Navigation files:',nfilgal
    WRITE(lfnerr,*)
    CALL exitrc(2)
  ENDIF

! Fill the filename record
! ------------------------
  IF (nfilpre /= 0) THEN
!   outputfilename defined
    IF (nfilpre == MAX(nfilgps,nfilglo,nfilgal)) THEN
      DO ii = 1,nfilpre
        filtab(1,ii) = filgps(1,ii)
        filtab(2,ii) = filglo(1,ii)
        filtab(3,ii) = filgal(1,ii)
        filtab(4,ii) = filpre(ii)
      ENDDO
    ELSE
      WRITE(lfnerr,'(/,A,/,A,//)') &
        ' *** SR RPINPT: Number of output files different', &
        '                from number of input files!'
        CALL exitrc(2)
    ENDIF
  ELSE
!   same outputname as input file
    DO ii = 1, MAX(nfilgps,nfilglo,nfilgal)
      filtab(1,ii) = filgps(1,ii)
      filtab(2,ii) = filglo(1,ii)
      filtab(3,ii) = filgal(1,ii)
      IF (filgps(1,ii) /= '') THEN
        filtab(4,ii) = filgps(2,ii)
      ELSEIF (filglo(1,ii) /= '') THEN
        filtab(4,ii) = filglo(2,ii)
      ELSEIF (filgal(1,ii) /= '') THEN
        filtab(4,ii) = filgal(2,ii)
      ELSE
      WRITE(lfnerr,'(/,A,/,A,//)')                      &
        ' *** SR RPINPT: Either a GPS, a GLONASS or a GALILEO ',   &
        '                RINEX file must be specified!'
        CALL exitrc(2)
      ENDIF
    ENDDO
  ENDIF
  nfil = MAX(nfilgps,nfilglo,nfilgal)

! Precise Orbit Format
! --------------------
  CALL readkeys('FORMAT', keyValue, irc)
  CALL ckoptc(1,'FORMAT', keyValue,(/'SP3 ','SP3C'/), 'sr rpinpt', &
              'Orbit format', irc, ioerr, valList=(/2,4/),        &
              maxVal=1,result1=ifrmat)

! Read Title for the Precise Orbit File
! -------------------------------------
  CALL readkeys('TITLE1', keyValue, irc)
  title(1) = keyValue(1)
  CALL readkeys('TITLE2', keyValue, irc)
  title(2) = keyValue(1)
  CALL readkeys('TITLE3', keyValue, irc)
  title(3) = keyValue(1)
  CALL readkeys('TITLE4', keyValue, irc)
  title(4) = keyValue(1)

! Read Time Window
! ----------------
  CALL gttimwin(' ',(/'RADIO_2','RADIO_1'/),                &
                (/'SESSION_YEAR', 'SESSION_STRG'/),         &
                (/'STADAT', 'STATIM', 'ENDDAT', 'ENDTIM'/), &
                window)

  tfirst = window(1)
  tlast  = window(2)

  IF (window(1) == 0.D0 .OR. window(2) == 1.D20) THEN
    WRITE(lfnerr,'(/,A,/,A,//)')                                    &
      ' *** SR RPINPT: Specify start time end end time',            &
      '                for precise orbit file!'
      CALL exitrc(2)
  ENDIF

  IF ((tlast-tfirst) > maxday) THEN
    WRITE(lfnerr,'(/,A,/,A,//)')                                    &
      ' *** SR RPINPT: Specified time window longer than 30 days.', &
      '                Increase parameter maxday!'
      CALL exitrc(2)
  ENDIF

! Read Tabular Interval
! ---------------------
  CALL readkeys('TABINT' , keyValue, irc)
  READ(keyValue(1),*,iostat = ios) dttab
  IF (ios /= 0) THEN
    WRITE(lfnerr,*) ' *** SR RPINPT: invalid entry for tabular interval :',   &
                      keyValue(1)
    ioerr = ioerr + 1
  END IF

! Read GLONASS option
! -------------------
  CALL readkeys('EXSHFT' , keyValue, irc)
  READ(keyValue(1),*,iostat = ios) iExShi
  IF (ios /= 0) THEN
    WRITE(lfnerr,*) ' *** SR RPINPT: invalid entry for GLONASS exclusions :', &
                      keyValue(1)
    ioerr = ioerr + 1
  END IF

! Read Coordinate System for Precise Orbit
! ----------------------------------------
  CALL readkeys('SYSTEM', keyValue, irc)
  coosys = keyValue(1)

! Read Agency
! -----------
  CALL readkeys('AGENCY', keyValue, irc)
  agency = keyValue(1)

  IF (ioerr /= 0) THEN
    WRITE (lfnerr,"(/,'  Number of errors: ',I2)")ioerr
    CALL exitrc(2)
  END IF

! Set model key for gmst2000
! --------------------------
  chrVal = ' '
  chrVal = 'IERS2000'
  numVal = 0.D0
  CALL setModKey(mod_orb_subMod,chrVal,srName,numVal)


  DEALLOCATE(filgps, stat=iac)
  DEALLOCATE(filglo, stat=iac)
  DEALLOCATE(filgal, stat=iac)
  DEALLOCATE(filpre, stat=iac)

  DEALLOCATE(keyValue,stat=iac)

END SUBROUTINE rpinpt

END MODULE
