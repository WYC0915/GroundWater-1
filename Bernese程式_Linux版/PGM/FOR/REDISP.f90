
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM redisp

! -------------------------------------------------------------------------
! Purpose:    Display Residuals
!
! Author:     L. Mervart
!
! Created:    22-Mar-2001
!
! Changes:    15-Oct-2001 RD: improved handling for new menu
!             29-Aug-2002 RD: Handle new formatted residual files
!             25-Sep-2002 HU: Remove i_astlib
!             23-Apr-2003 HU: Nullify local pointers
!             16-May-2003 AJ: Initialize structure
!             20-May-2003 RD: Use SR gttimwin instead of SR readsess
!             13-Sep-2003 HU: Interface for defreq
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             08-Jan-2004 RD: Modify program output
!             07-Feb-2005 HB: Adopt for ifc-Compiler, Version 8.1
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             27-Feb-2007 AG: Call DEFCON with parameter
!             23-Sep-2010 RD: Enable CPU counter
!             06-Oct-2010 RD: Exitrc added at the end
!             30-Nov-2011 SL: new title string for pritit, m_bern with ONLY
!             31-Aug-2012 RD/LP: Handle empty IFILE-entries
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, keyValueLength, fileNameLength, &
                      lfnPrt, lfnRes, lfnErr
  USE m_cpu,    ONLY: cpu_start
  USE m_time,   ONLY: t_timint
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE d_resfil, ONLY: t_resHead,init_resHead
  USE p_redisp, ONLY: freqID,filTyp,difTyp
  USE s_ckoptr
  USE s_opnfil
  USE s_prflna
  USE s_pritit
  USE s_rdresh2
  USE s_readinpf
  USE s_opnerr
  USE s_gttimwin
  USE s_dspres
  USE s_timst2
  USE s_readkeys
  USE s_defreq
  USE s_defcon
  USE s_exitrc
  USE s_ckoptc
  USE s_opnsys
  USE s_gtflna
  IMPLICIT NONE

! Local parameter
! ---------------
  CHARACTER(LEN=6), PARAMETER    :: pgName = 'redisp'

  CHARACTER(LEN=5), DIMENSION(3), PARAMETER :: cycles = &
                    (/ 'L1   ','L2   ','L1/L2' /)
  CHARACTER(LEN=4), DIMENSION(2), PARAMETER :: wlFact = (/ 'full','half' /)

! Local variables
! ---------------
  TYPE(t_resHead)                                      :: resHed
  TYPE(t_timint)                                       :: timwin

  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=fileNameLength)                        :: resFil
  CHARACTER(LEN=19)                                    :: epostr

  INTEGER(i4b)                                         :: dspFrq
  INTEGER(i4b)                                         :: ifil
  INTEGER(i4b)                                         :: iFreq,jFrq
  INTEGER(i4b)                                         :: iwlfac
  INTEGER(i4b)                                         :: icycle
  INTEGER(i4b)                                         :: iunit
  INTEGER(i4b), DIMENSION(2)                           :: fromTo
  INTEGER(i4b)                                         :: mDiff
  INTEGER(i4b)                                         :: ii,jj
  INTEGER(i4b)                                         :: irc,irCode

  REAL(r8b)                                            :: resSiz
  REAL(r8b)                                            :: timRef

  LOGICAL                                              :: isOrbit
  LOGICAL                                              :: lineOK

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Nullify pointers
! ----------------
  CALL init_reshead(resHed)
  CALL init_inpkey(inpKey)
  NULLIFY(keyValue)

! Get the name of the input file
! ------------------------------
  CALL readinpf(' ',inpKey)

! Open system files, define constants
! -----------------------------------
  CALL opnsys
  CALL defcon(1)
  CALL pritit(pgName,'Display residual file',131)
  CALL prflna(131)

! Read general input options
! --------------------------
  irCode = 0

! Residual unit
! -------------
  CALL readKeys('IUNIT', keyValue, irc)

  CALL ckoptc(1,'IUNIT', keyValue,                                          &
              (/'CYCLES     ','MILLIMETERS','METERS     '/), pgName,        &
              'Unit to display residuals',irc,irCode,                       &
              maxVal=1,result1=iUnit)

  IF (iUnit == 1) THEN
! Cycle Type
! ----------
    CALL readKeys('ICYCLE', keyValue, irc)

    CALL ckoptc(1,'ICYCLE', keyValue,(/'L1   ','L2   ','L1/L2'/), pgName,   &
                'Unit cyles: cycle type',irc,irCode,                        &
                maxVal=1,result1=iCycle)

! Wavelength factor
! -----------------
    CALL readKeys('IWLFAC', keyValue, irc)

    CALL ckoptc(1,'IWLFAC', keyValue,(/'FULL','HALF'/), pgName,             &
                'Unit cyles: wavelength factor',irc,irCode,                 &
                maxVal=1,result1=iWlfac)

  ENDIF

! Frequency
! ---------
  CALL readKeys('ICARR', keyValue, irc)

  CALL ckoptc(1,'ICARR', keyValue,                                          &
              (/ 'L1 ','L2 ','L3 ','L4 ','L5 ','RAD','LON','OUT' /), pgName,&
              'Frequency for residual display',irc,irCode,                  &
              maxVal=1,result1=iFreq)


! Minimal Size of Residuals
! -------------------------
  CALL readKeys('RESSIZ', keyValue, irc)

  CALL ckoptr(1,'RESSIZ',keyValue,pgName,                                   &
              'Minimum size of residual to display',irc,irCode,             &
              maxVal=1,empty=0d0,ge=0d0,result1=resSiz)

! Time window
! -----------
  CALL gttimwin(' ',(/'RADIO_0','RADIO_1','RADIO_2'/),        &
                (/'SESSION_YEAR','SESSION_STRG'/),            &
                (/'STADAT','STATIM','ENDDAT','ENDTIM'/),      &
                timWin%t)

! A wrong input option found
! --------------------------
  IF (irCode /= 0) CALL exitrc(2)


! Write some options into program output file
! -------------------------------------------
  IF (iUnit == 1) WRITE(lfnprt,'(1X,A)') 'Unit of residuals:          ' // &
                  wlFact(iWlfac) // ' ' // TRIM(cycles(iCycle)) // ' cycles'

  IF (iUnit == 2) WRITE(lfnprt,'(1X,A)') 'Unit of residuals:          ' // &
                  'Millimeters'

  IF (iUnit == 3) WRITE(lfnprt,'(1X,A)') 'Unit of residuals:          ' // &
                  'Meters'

  WRITE(lfnprt,'(1X,A,F10.4,//)')  'Minimum residual displayed:',resSiz


! Read the residual file header
! -----------------------------
  CALL gtflna(1,'RESIDUA',resfil,irc)

  CALL opnfil(lfnres, resfil, 'OLD', 'UNFORMATTED', 'READONLY', ' ', irc)
  CALL opnerr(lfnerr, lfnres, irc, resfil, 'redisp')

  CALL rdresh2(lfnres,reshed)

  CLOSE(lfnres)

! Print some information on the residual file:
! --------------------------------------------
  IF (resHed%dsc%nDiff == 99) THEN
    WRITE(lfnprt,'(A,/,A,//,A,/,1X,20("-"),/)')                              &
    ' Residual file to display:  ' // TRIM(resfil),                          &
    ' Residual file title:       ' // TRIM(resHed%title),                    &
    ' GENERAL INFORMATION:'
    WRITE (lfnprt,'(A,7X,A,/A,3(I8,A),/,A,I8,//)')                           &
         '    Program that created the file:    ',TRIM(reshed%dsc%pgName),   &
         '    Difference level of observations: ',reshed%dsc%nResta,' sta.', &
                                                 reshed%dsc%nResat,' sat.',  &
                                                 reshed%dsc%nResep,' epo.',  &
         '    Number of parameters:             ',reshed%dsc%nPar
  ENDIF

! Select file
! -----------
  CALL readKeys('IFILE', keyValue, irc)

! Loop all selected files
! -----------------------
  DO ii = 1, SIZE(keyValue)
    IF (LEN_TRIM(keyValue(ii)) == 0) CYCLE

! Get the file number
    READ(keyValue(ii),*) iFil

! Compare the entry with the residual file content
    lineOK = .TRUE.
    lineOK = lineOK .AND. &
             (INDEX(keyValue(ii),resHed%filHead(iFil)%stanam(1)) /= 0)
    lineOK = lineOK .AND. &
             (INDEX(keyValue(ii),resHed%filHead(iFil)%stanam(2)) /= 0 .OR. &
              resHed%dsc%nResta == 0)
    CALL timst2(1,1,resHed%filHead(iFil)%timref,epostr)
    lineOK = lineOK .AND. (INDEX(keyValue(ii),epostr) /= 0)
    IF (INDEX(keyValue(ii),'Orbit') == 0) THEN
      lineOK = lineOK .AND. &
               (INDEX(keyValue(ii),filTyp(resHed%filHead(iFil)%meaTyp)) /= 0)
      mDiff  = resHed%dsc%nResta + resHed%dsc%nResat + resHed%dsc%nResep
      lineOK = lineOK .AND. &
               (INDEX(keyValue(ii),difTyp(mDiff+1)) /= 0)
      DO jj = 1, resHed%filHead(iFil)%nfrfil
        lineOK = lineOK .AND. &
               (INDEX(keyValue(ii),freqID(resHed%filHead(iFil)%iCarr(jj))) /= 0)
      ENDDO
    ELSE
      DO jj = 1, resHed%filHead(iFil)%nfrfil
        lineOK = lineOK .AND. &
             (INDEX(keyValue(ii),freqID(resHed%filHead(iFil)%iCarr(jj)+5)) /= 0)
      ENDDO
    ENDIF
    IF (.NOT. lineOK) THEN
      WRITE(lfnerr,'(/,A,2(/,16X,A,A),/)')                                &
            ' *** PG REDISP: Wrong display selection for residual file.', &
                            'File name: ',TRIM(resfil),                   &
                            'Selection: ',keyValue(ii)(1:50) // '...'
      CALL exitrc(2)
    ENDIF


! Get the time interval
    fromTo = (/ 0, 100000 /)

    IF (timWin%t(1) /= 0d0) &
      fromTo(1) = NINT((timWin%t(1)-resHed%filHead(iFil)%timref) * 86400d0 / &
                        resHed%filHead(iFil)%iDeltT) + 1

    IF (timWin%t(2) /= 1d20) &
       fromTo(2) = NINT((timWin%t(2)-resHed%filHead(iFil)%timref) * 86400d0 / &
                        resHed%filHead(iFil)%iDeltT) + 1

! Is it an orbit
    isOrbit = ((resHed%dsc%nDiff == 0 .AND. &
                resHed%filHead(iFil)%stanam(1)(1:3) == 'ARC') .OR. &
               resHed%dsc%pgName == 'ORBGEN')

! Replace bad frequency requests
    dspFrq = iFreq

    IF (resHed%filHead(iFil)%nfrFil == 1) THEN
      dspFrq = resHed%filHead(iFil)%iCarr(1)
      IF (isOrbit) dspFrq = resHed%filHead(iFil)%iCarr(1) + 5
    ENDIF

! Check frequency request
    IF (isOrbit .AND. dspFrq <= 5) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/,16X,A,A,/)')                  &
           ' *** PG REDISP: An orbit residual file found but an ',         &
                           'observation frequency specified for display.', &
                           'Residual file name: ',TRIM(resfil),            &
                           'Frequency:          ',freqID(dspFrq)
      CALL exitrc(2)
    ENDIF

    IF (.NOT. isOrbit .AND. dspFrq > 5) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/,16X,A,A,/)')                  &
           ' *** PG REDISP: An observation residual file found but',       &
                           'an orbit component specified for display.',    &
                           'Residual file name: ',TRIM(resfil),            &
                           'Frequency:          ',freqID(dspFrq)
      CALL exitrc(2)
    ENDIF

! Only frequencies from resi-file may be displayed
    IF (resHed%dsc%iTyp == 1) THEN
      jFrq = 0
      DO jj = 1, resHed%filHead(iFil)%nfrFil
        IF ((.NOT. isOrbit .AND. resHed%filHead(iFil)%iCarr(jj) == dspFrq) .OR.&
            (isOrbit  .AND.  resHed%filHead(iFil)%iCarr(jj) + 5 == dspFrq)) THEN
          jFrq = 1
          EXIT
        ENDIF
      ENDDO

      IF (jFrq == 0 .AND. .NOT. isOrbit) THEN
        WRITE(lfnerr,'(/,A,2(/,16X,A,A),/)') &
             ' *** PG REDISP: A wrong frequency specified to display the'//&
                             'residual file.',                             &
                             'Residual file name: ',TRIM(resfil),          &
                             'Frequency:          ',freqID(dspFrq)
        CALL exitrc(2)
      ENDIF

      IF (jFrq == 0 .AND. isOrbit) THEN
        WRITE(lfnerr,'(/,A,2(/,16X,A,A),/)') &
             ' *** PG REDISP: A wrong component specified to display the', &
                             'orbit residual file.',                       &
                             'Residual file name: ',TRIM(resfil),          &
                             'Frequency:          ',freqID(dspFrq)
        CALL exitrc(2)
      ENDIF
    ENDIF

! Set gps and glonass frequencies
!!!!!!!!!! Problems with ifc81 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CMP_IFC8
    timRef = resHed%filHead(iFil)%timref
    CALL defreq((/ timRef,timRef /), &
                resHed%filHead(iFil)%nsatel,resHed%filHead(iFil)%numsat)
#else
    CALL defreq((/ resHed%filHead(iFil)%timref,resHed%filHead(iFil)%timref /), &
                resHed%filHead(iFil)%nsatel,resHed%filHead(iFil)%numsat)
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! Write the residuals
! -------------------
    IF (resHed%filHead(iFil)%nfrFil > 1) THEN
      WRITE(lfnprt,'(1X,A,A,/)') 'File: ' // TRIM(keyValue(ii)), &
            '  (displayed frequency: ' // TRIM(freqID(dspFrq)) // ')'
    ELSE
      WRITE(lfnprt,'(1X,A/)') 'File: ' // TRIM(keyValue(ii))
    ENDIF

    IF (isOrbit) dspFrq = dspFrq - 5

    CALL dspres(lfnprt, ifil, fromTo, resSiz, dspFrq, iwlfac, icycle, &
                iunit, irc)

    WRITE(lfnprt,'(/)')

  ENDDO

  CALL exitrc(0)

END PROGRAM redisp

