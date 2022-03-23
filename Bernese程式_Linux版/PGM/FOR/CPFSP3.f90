
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM cpfsp3

! -------------------------------------------------------------------------
! Purpose:    Convert CPF slr prediction files into SP3 files.
!
! Author:     C.Urschl
!
! Created:    09-Nov-2005
!
! Changes:    31-Jul-2006 CU: Read CPF with free format (SR splstr),
!                             Remove unused variables
!             10-Aug-2006 HB: WTPREI called with t_epoch-structure
!             11-Sep-2006 CU: Print message, if EOF, read all header units,
!                             read velocities (but don't print)
!             27-Feb-2007 AG: Call DEFCON with parameter
!             20-May-2008 DT: Correct title of program output;
!                             improve error messages;
!                             Allow empty value for SAMPLE in ckopti
!             21-May-2008 DT: Verify start time given in header vs. data
!             15-May-2009 DT: Add time window selection
!             12-Mar-2010 SL: data descriptors added to WRITE statements,
!                             ONLY added to USE m_bern
!             21-Sep-2010 RD: ST2TIM can be used as a module now
!             23-Sep-2010 RD: Enable CPU counter
!             07-Oct-2010 RD: Do not close the lfnprt file
!             24-Nov-2011 SL: new title string for pritit
!             12-Mar-2012 RD: Use SPLSTR as module now
!             12-Mar-2012 RD: Use SVN2CHR as module now
!             27-Apr-2012 RD: Nullify pointers
!             27-Apr-2012 RD: Remove unused variables and modules
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnErr, lfnPrt, lfn001, lfnOrb, &
                      keyValueLength, fileNameLength80, timStrgLength, &
                      lineLength
  USE m_cpu,    ONLY: cpu_start
  USE m_epoch,  ONLY: t_epoch, t_timWin, OPERATOR(.realToEpoch.), isInWin
  USE d_inpkey, ONLY: inpkey, init_inpkey
  USE m_maxdim, ONLY: maxsat
  USE f_dgpsut
  USE f_nextline

  USE s_readinpf
  USE s_opnsys
  USE s_defcon
  USE s_pritit
  USE s_prflna
  USE s_gtflna
  USE s_readkeys
  USE s_ckopti
  USE s_ckoptl
  USE s_exitrc
  USE s_cos2prn
  USE s_wtpreh
  USE s_wtprei
  USE s_gttimwin
  USE s_opnfil
  USE s_opnerr
  USE s_st2tim
  USE s_splstr
  USE s_svn2chr

  IMPLICIT NONE

! Local Parameters
! ----------------
  CHARACTER(LEN=9), PARAMETER           :: pgName   = 'PG CPFSP3'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
       DIMENSION(:), POINTER            :: keyValue
  CHARACTER(LEN=fileNameLength80)       :: filsp3
  CHARACTER(LEN=fileNameLength80)       :: filcpf
  CHARACTER(LEN=57), DIMENSION(4)       :: title
  CHARACTER(LEN=4)                      :: agency
  CHARACTER(LEN=3)                      :: timsys
  CHARACTER(LEN=7)                      :: cospar
  CHARACTER(LEN=9)                      :: satcos
  CHARACTER(LEN=1)                      :: clast
  CHARACTER(LEN=1)                      :: satchr
  CHARACTER(LEN=timStrgLength)          :: timstrfirst, timstrlast
  CHARACTER(LEN=lineLength)             :: line
  CHARACTER(LEN=20), DIMENSION(8)       :: substr

  INTEGER(i4b)                          :: y1, y2
  INTEGER(i4b)                          :: satprn
  INTEGER(i4b)                          :: idelta ! Sampling interval in CPF
                                                  ! file (sec)
                                                  ! 0 = take from SP3 file
  INTEGER(i4b)                          :: idtsp3
  INTEGER(i4b)                          :: irc, irCode, iostat
  INTEGER(i4b)                          :: iepo, nepo, nrec
  INTEGER(i4b)                          :: ifrmat
  INTEGER(i4b)                          :: ilast
  INTEGER(i4b)                          :: satnum
  INTEGER(i4b)                          :: mjd
  INTEGER(i4b)                          :: nsub

  REAL(r8b)                             :: tprev, tepoch
  REAL(r8b)                             :: tfirst, tlast, t_header
  REAL(r8b)                             :: utccor
  REAL(r8b)                             :: dttab, dtsp3
  REAL(r8b)                             :: sec
  REAL(r8b), DIMENSION(3,1)             :: pos, vel
  REAL(r8b)                             :: fac, fract
  REAL(r8b), DIMENSION(2)               :: window ! Time window from panel

  TYPE(t_epoch)                         :: tEpo
  TYPE(t_timWin)                        :: winEpo ! Time window from panel

  LOGICAL                               :: posok, prtHead

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Nullify pointers
! ----------------
  NULLIFY(keyValue)


! Get the name of the input file
! ------------------------------
  CALL init_inpkey(inpkey)
  CALL readinpf('',inpkey)


! Open system files, define constants
! -----------------------------------
  CALL opnsys
  CALL defcon(1)


! Automatic output generation
! ---------------------------
  CALL pritit(pgName,'Convert CPF to precise orbit files',131)
  CALL prflna(131)


! Get name of CPF files
! ---------------------
  CALL gtflna(0,'CPFFIL',filcpf,irc)
  IF (irc /= 0) THEN
    WRITE(lfnerr,'(/,A,/,2A)')                              &
      ' *** PG SP3CPF: Error while reading CPF file name.', &
      '                CPF file name: ', TRIM(filcpf)
    CALL exitrc(2)
  ENDIF


! Get name of SP3 file
! --------------------
  CALL gtflna(0,'PREFIL',filsp3,irc)
  IF (irc /= 0) THEN
    WRITE(lfnerr,'(/,A,/,2A)')                              &
      ' *** PG SP3CPF: Error while reading SP3 file name.', &
      '                SP3 file name: ', TRIM(filsp3)
    CALL exitrc(2)
  ENDIF


! Print orbit file names into protocol file
! -----------------------------------------
  WRITE(lfnprt,'(A,/,2A,/,1X,A40,2X,A40,/)')                              &
    ' CPF file name                             SP3 file name',           &
    ' ------------------------------------------------------------------',&
    '--------', filcpf, filsp3


! Read Title for the Precise Orbit File (new)
! -------------------------------------------
  CALL readkeys('TITLE1', keyValue, irc)
  CALL ckoptl(0,'TITLE1', keyValue, pgName, 'Title 1', irc, irCode, &
       maxLength=LEN(title(1)), empty=' ', maxval=1, result1=title(1))
  CALL readkeys('TITLE2', keyValue, irc)
  CALL ckoptl(0,'TITLE2', keyValue, pgName, 'Title 2', irc, irCode, &
       maxLength=LEN(title(2)), empty=' ', maxval=1, result1=title(2))
  CALL readkeys('TITLE3', keyValue, irc)
  CALL ckoptl(0,'TITLE3', keyValue, pgName, 'Title 3', irc, irCode, &
       maxLength=LEN(title(3)), empty=' ', maxval=1, result1=title(3))
  CALL readkeys('TITLE4', keyValue, irc)
  CALL ckoptl(0,'TITLE4', keyValue, pgName, 'Title 4', irc, irCode, &
       maxLength=LEN(title(4)), empty=' ', maxval=1, result1=title(4))


! Read the time window
! --------------------
  CALL gttimwin(' ',(/'RADIO_0','RADIO_1','RADIO_2'/),              &
                    (/'SESSION_YEAR','SESSION_STRG'/),              &
                    (/'STADAT','STATIM','ENDDAT','ENDTIM'/),window)

! Conversion to epoch
! -------------------
  winEpo%t(1) = .realToEpoch. window(1)
  winEpo%t(2) = .realToEpoch. window(2)


! Open CPF file
! -------------
  CALL opnfil(lfn001,filcpf,'OLD','FORMATTED','READONLY',' ',iostat)
  CALL opnerr(lfnerr,lfn001,iostat,filcpf,pgName)

! Read CPF header
! ---------------
  line = nextline(lfn001,0)
  IF (line(1:2) == 'H1') THEN
    READ(line,'(10X,A4)') agency
  ELSE
    WRITE(lfnerr,'(/,2A)') &
      ' *** PG CPFSP3: Header line H1 not found in CPF file: ', filcpf
    CALL exitrc(2)
  ENDIF

  line = nextline(lfn001,0)
  IF (line(1:2) == 'H2') THEN
    READ(line,'(4X,A7,15X,2(A19,1X),I5)') cospar,timstrfirst,timstrlast,idelta
  ELSE
    WRITE(lfnerr,'(/,2A)') &
      ' *** PG CPFSP3: Header line H2 not found in CPF file: ', filcpf
    CALL exitrc(2)
  ENDIF

  DO
    line = nextline(lfn001,0)
    IF (line(1:2) == 'H9') EXIT
  ENDDO

  CALL st2tim(2,1,timstrfirst,tfirst)
  CALL st2tim(2,1,timstrlast,tlast)
  nepo = IDINT((tlast-tfirst)/idelta*86400d0)+1

! Get sampling interval
! ---------------------
  nrec   = nepo
  idtsp3 = 0
  CALL readKeys('SAMPLE', keyValue, irc)
  CALL ckopti(1,'SAMPLE', keyValue, pgName, 'Sampling interval', irc,     &
       irCode, empty=0, ge=0, maxval=1, result1=idtsp3)

  IF(idtsp3 /= 0) THEN

    IF(idtsp3 < idelta) THEN
      WRITE(lfnerr,'(/,A,/,2(16X,A,I4,A,/),16X,A,I4,A,/)')                       &
           ' ### PG CPFSP3: Requested sampling interval smaller than data set:', &
           'Spacing requested for SP3: ', idtsp3, ' seconds',                    &
           'Data spacing in CPF file:  ', idelta, ' seconds',                    &
           'Sampling interval set to ', idelta, ' seconds!'

      idtsp3 = idelta
    ELSE
      fract = idtsp3*1d0/idelta - INT(idtsp3*1d0/idelta)

      IF (fract /= 0d0) THEN
        fac = 1d0
        DO
          fac = fac + 1d0
          IF ((idtsp3*1d0/idelta) <= fac) THEN
            idtsp3 = idelta * fac
            WRITE(lfnerr,'(/,A,I4,A,/)')                                &
                 ' ### PG CPFSP3: Requested sampling interval set to ', &
                 idtsp3, ' seconds.'
            EXIT
          ENDIF
        ENDDO
      ENDIF

      nrec = IDINT((tlast-tfirst)/idtsp3*86400d0)+1
      tlast = tfirst + (nrec-1)*idtsp3/86400d0

    END IF

  ELSE
    idtsp3 = idelta
  END IF

  dttab = idtsp3 * 1d0

! Get PRN
! -------
  y2 = 20
  READ(cospar(1:1),'(I1)') y1
  IF (y1 >= 6) y2 = 19

  READ(cospar(6:7),'(I2)') ilast
  clast = CHAR(ilast+64)
  WRITE(satcos,'(I2,A2,"-",A3,A1)') y2,cospar(1:2),cospar(3:5),clast

  CALL cos2prn(2,satcos,tfirst,satprn,irc)
  CALL svn2chr(satprn,satnum,satchr)

  WRITE(lfnprt,'(/,A,A1,I2.2,2X,A9,//,A,I6,A)')    &
    ' Satellite number:    ',satchr,satnum,satcos, &
    ' Sampling interval:', idtsp3,' seconds'

! Time System
! -----------
  CALL readkeys('TIMESYS', keyValue, irc)
  CALL ckoptl(1,'TIMESYS', keyValue, pgName, 'Time system', irc,   &
       irCode, maxLength=LEN(timsys), maxval=1, result1=timsys)

  WRITE(lfnprt,'(/,2A)') ' Time system:         ', timsys

! Correct for leap second
! -----------------------
  IF(timsys == 'GPS') THEN
    utccor = DGPSUT(tfirst)/86400d0
  ELSE
    utccor = 0d0
  END IF
  tfirst = tfirst + utccor

  ifrmat = 4

! Initialization
! --------------
  tprev  = 0d0
  posok  = .FALSE.

  prtHead = .TRUE.
  t_header = tfirst

! Read all epochs of CPF file
! ---------------------------
  DO iepo = 1, nepo

  ! Read observation concerning one epoch (free format)
    READ(lfn001,'(A)',iostat=irc) line
    IF (irc /= 0) THEN
      WRITE(lfnerr,'(/,2A)') &
        ' *** PG CPFSP3: Error reading CPF file: ',filcpf
      CALL exitrc(2)
    ENDIF

    IF (posok) THEN

    ! Read velocity record
      IF(line(1:2) == '20') THEN
        CALL splstr(line,5,' ',nsub,substr,irCode)
        READ(substr(3),'(F19.6)')vel(1,1)
        READ(substr(4),'(F19.6)')vel(2,1)
        READ(substr(5),'(F19.6)')vel(3,1)
        IF (irCode == 2)     &
          WRITE(lfnerr,'(/,A,/)') &
            ' ### PG CPFSP3: Too many substrings found while reading the CPF file.'
!!! for printing velocities in PRE file
!!!       ifrmat = 5
      ELSE
        vel(1:3,1) = 0d0
      ENDIF

    ! Print records of precise orbit file
      tepoch = mjd + sec/86400d0 + utccor

      tEpo = .realToEpoch.tepoch


    ! Check epoch vs. time window from input panel
    ! --------------------------------------------
      dtsp3 = (dttab / 86400d0) / 2d0

      IF ( isInWin(tEpo,winEpo,dtsp3) ) THEN


      ! Verify epoch vs. header information
      ! -----------------------------------
        IF ( tepoch < t_header ) THEN
           IF ( prtHead .AND. posok )  THEN
              t_header = tepoch
              WRITE(lfnerr,'(/,A,/,16X,2A,/,3(16X,A,F15.5,/))')                          &
                    ' ### PG CPFSP3: Epoch of data earlier than start epoch in header!', &
                    'CPF file: ', TRIM(filcpf),                                          &
                    'Epoch of data set:           ', tepoch,                             &
                    'Start epoch given in header: ', tfirst,                             &
                    'New epoch for header:        ', t_header

              WRITE(lfnprt,'(/,A,F15.5)') ' New start epoch for header: ', t_header


           ELSE
              WRITE(lfnerr,'(/,A,/,2A,/,2(16X,A,F15.5,/),16X,A,/)')                      &
                    ' ### PG CPFSP3: Epoch of data earlier than start epoch in header!', &
                    'CPF file:                    ', TRIM(filcpf),                       &
                    'Epoch of data set:           ', tepoch,                             &
                    'Start epoch given in header: ', tfirst,                             &
                    'Data of this epoch may be skipped in other programs!'

           END IF

        END IF

      ! Write header of precise orbit
      ! -----------------------------
        IF ( prtHead .AND. posok ) THEN
          CALL wtpreh(filsp3,lfnorb,ifrmat,1,(/satprn/),(/0/),t_header, &
                      nrec,dttab,title,'U    ','SLR  ','EXT',agency,timsys,0d0,0d0)
          prtHead = .FALSE.
        END IF


      ! Print records of precise orbit file
      ! -----------------------------------
        IF( (tepoch-tprev+1d-6)*86400d0 >= idtsp3) THEN
          CALL wtprei(lfnorb,ifrmat,(/0,0/),1,(/satprn/),tEpo,pos,vel,&
             (/999999.999999d0/),(/999999.999999d0/),                 &
             (/0,0,0,0/),(/0,0,0,0/),(/' ',' ',' ',' '/),             &
             (/0d0,0d0,0d0,0d0/),(/0d0,0d0,0d0,0d0/),                 &
             (/0d0,0d0,0d0,0d0,0d0,0d0/),(/0d0,0d0,0d0,0d0,0d0,0d0/),ircode)
          tprev = tepoch
        END IF

        posok = .FALSE.


      ENDIF ! Window

    ENDIF ! posOK


  ! Read next position record
    IF (line(1:2) == '10') THEN
      CALL splstr(line,8,' ',nsub,substr,irCode)
      READ(substr(3),'(I5)')mjd
      READ(substr(4),'(F13.6)')sec
      READ(substr(6),'(F17.3)')pos(1,1)
      READ(substr(7),'(F17.3)')pos(2,1)
      READ(substr(8),'(F17.3)')pos(3,1)
      IF (irCode == 2)     &
        WRITE(lfnerr,'(/,A,/)') &
          ' ### PG CPFSP3: Too many substrings found while reading the CPF file.'
      posok = .TRUE.
    ENDIF




  ENDDO

  WRITE(lfnorb,'("EOF")')
  CLOSE(lfnorb)

  CLOSE(lfn001)

!  CLOSE(lfnprt)

  CALL exitrc(0)

END PROGRAM cpfsp3
