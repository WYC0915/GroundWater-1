MODULE s_RDR3OH
CONTAINS

! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

SUBROUTINE rdr3oh(lfnobs,first)

! ------------------------------------------------------------------------------
! Purpose:    This subroutine reads the header of a RINEX 3.0 observation file
!
! Parameters:
!         in: filename: RINEX file name
!             lfnobs:   File number
!             first:    for reading the first header
!        out: rxohead : Header information in structure t_rxohead
!
!
! Remarks:
!
! Author:     A. Gäde
!
! Created:    29-Feb-2007
!
! Changes:    11-Jul-2007 AG: Format of warning message corrected
!             28-Aug-2007 AG: SYS / # OF OBS section corrected,
!                              warning for end of file statement excluded
!             05-Mar-2008 HB: Modifications for loop variables
!             14-Nov-2011 RD: Allow also for error in LAST EPOCH record
!             27-Mar-2012 RD: Use LISTC1 as module now
!             24-Apr-2012 LP: Generalization for all sat-systems using maxsys
!             31-May-2012 LP: Use prn2prn
!             01-Jun-2012 LP: Dummy read of wavelength factors, quarter-cycles;
!                             accept also RNXVERS 3.XX and not only 3.00
!             04-Jun-2012 RD: Correct typo in message
!             26-Jun-2012 LP: Additional dummy reads
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ------------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnerr
  USE m_global, ONLY: g_rnxsys, maxsys
  USE d_rinex3, ONLY: rxohead, rxoobs, rnxvers, init_rxohead
  USE s_upperc
  USE s_opnfil
  USE s_alcerr
  USE s_opnerr
  USE s_stripdir
  USE f_listc1
  USE f_djul
  USE s_exitrc
  USE f_prn2prn
  IMPLICIT NONE
!
! Dummy list
! ----------
  INTEGER(i4b)                          :: lfnobs
  LOGICAL                               :: first
  CHARACTER(LEN=60)                     :: filnam
  CHARACTER(LEN=60)                     :: dummyslot
  CHARACTER(LEN=60)                     :: dummynumobsprn
!
! Local Variables
! ---------------
  CHARACTER(LEN=60)                     :: line
  CHARACTER(LEN=60),DIMENSION(50), SAVE :: phaseli
  CHARACTER(LEN=60),DIMENSION(50), SAVE :: factorli
  CHARACTER(LEN=20)                     :: head
  CHARACTER(LEN=3),DIMENSION(13)        :: list
  CHARACTER(LEN=1)                      :: satsys
  CHARACTER(LEN=1)                      :: qsys
  CHARACTER(LEN=3)                      :: qobs
  INTEGER(i4b),DIMENSION(9)             :: listi
  INTEGER(i4b)                          :: obslin
  INTEGER(i4b)                          :: iac
  INTEGER(i4b)                          :: ii,jj,kk
  INTEGER(i4b)                          :: iobs
  INTEGER(i4b)                          :: icom
  INTEGER(i4b)                          :: ipha
  INTEGER(i4b)                          :: ifac
  INTEGER(i4b)                          :: iprn,prnhelp
  INTEGER(i4b)                          :: ipcv
  INTEGER(i4b)                          :: idcb
  INTEGER(i4b)                          :: isys
  INTEGER(i4b)                          :: ilist
  INTEGER(i4b)                          :: irow
  INTEGER(i4b)                          :: items
  INTEGER(i4b)                          :: factor
  INTEGER(i4b)                          :: year
  INTEGER(i4b)                          :: month
  INTEGER(i4b)                          :: day
  INTEGER(i4b)                          :: hour
  INTEGER(i4b)                          :: minute
  INTEGER(i4b)                          :: ilin
  INTEGER(i4b)                          :: maxlin = 999
  INTEGER(i4b)                          :: wfac1,wfac2
  REAL(r8b)                             :: second
  REAL(r8b)                             :: rday
  REAL(r8b)                             :: qcycle
  LOGICAL                               :: allocat
!
! Initialize RINEX header structure
! ---------------------------------
  INQUIRE(unit = lfnobs, name = filnam)
  CALL stripdir(filnam)
  IF (first) THEN
    CALL init_rxohead(rxohead)
    REWIND(lfnobs)
!
! Read and check RINEX version
! ----------------------------
    READ (lfnobs,'(A60,A20)', iostat = iac) line,head
    CALL UPPERC(line)
    CALL UPPERC(head)

    IF (iac /= 0) THEN
      WRITE(lfnerr,"(' ### SR RDR3OH: Error reading first line in RINEX file:', &
                             & /,16X,'Filename: ',A,                  &
                             & /,16X,'File skipped!',/)")          &
                             TRIM(ADJUSTL(filnam))
      rxohead%end = 5
      RETURN
    ELSEIF (iac < 0) THEN
      WRITE(lfnerr,"(' ### SR RDR3OH: Empty RINEX file.', &
                             & /,16X,'Filename: ',A,      &
                             & /,16X,'File skipped!',/)")  &
                     TRIM(ADJUSTL(filnam))
      rxohead%end = 9
      RETURN
    ELSEIF(head .EQ. 'RINEX VERSION / TYPE') THEN
      READ (line, '(F9.2,11X,A1,19X,A1)', iostat = iac) rxohead%rnxvers, &
           rxohead%filtyp,rxohead%satsys
      IF (iac /= 0) THEN
        WRITE(lfnerr,"(' ### SR RDR3OH: Error decoding first line in RINEX file:',/,/,A,/, &
                             & /,16X,'Filename: ',A,               &
                             & /,16X,'File skipped!',/)")          &
                             line,TRIM(ADJUSTL(filnam))
        rxohead%end = 4
        RETURN
      ENDIF
    ELSE
      WRITE(lfnerr,"(/,' *** SR RDR3OH: File is not a RINEX file.', &
                               & /,16X,'Filename: ',A,                 &
                               & /,16X,'File skipped!',/)")        &
                                             TRIM(ADJUSTL(filnam))
      rxohead%end = 5
      RETURN
    ENDIF
!    IF (rxohead%rnxvers > rnxvers) THEN
    IF (INT(rxohead%rnxvers) > INT(rnxvers)) THEN
      WRITE(lfnerr,"(/,' *** SR RDR3OH: Wrong format version of',&
                  &        ' RINEX file.',                       &
                  &  /,16X,'Filename: ',A,                       &
                  &  /,16X,'Supported format version:',F8.2,     &
                  &  /,16X,'Format version in file  :',F8.2,/)") &
                        TRIM(adjustl(filnam)),rnxvers, rxohead%rnxvers
      rxohead%end = 2
      RETURN
    ENDIF
    REWIND(lfnobs)
    phaseli = ''
    factorli = ''
    icom = 0
    idcb = 0
    ipcv = 0
  ENDIF
!
! Read entire RINEX header and fill the structure rxohead
! -------------------------------------------------------
  ipha = 0
  ifac = 0
  obslin = 0
  rxohead%end = 0
  IF (.NOT. first) maxlin = rxoobs%nusat
  DO ilin=1,maxlin
    READ (lfnobs,'(A60,A20)', iostat = iac) line,head
    CALL UPPERC(line)
    CALL UPPERC(head)
    IF (iac > 0) THEN
      WRITE(lfnerr,"(' ### SR RDR3OH: Error reading RINEX header in file:', &
                             & /,16X,'Filename: ',A,                &
                             & /,16X,'File skipped!',/)")          &
                             TRIM(ADJUSTL(filnam))
      rxohead%end = 4
      RETURN
    ELSEIF (iac < 0) THEN
!      WRITE(lfnerr,"(' ### SR RDR3OH: End of RINEX file without ',&
!                     &   'END OF FILE statement.',      &
!                     & /,16X,'Filename: ',A,/)")        &
!                     TRIM(ADJUSTL(filnam))
      rxohead%end = 3
      RETURN
    ENDIF

    IF(head .EQ. 'RINEX VERSION / TYPE') THEN
      CYCLE

    ELSEIF (head .EQ. 'PGM / RUN BY / DATE') THEN
      READ (line, '(3A20)', iostat = iac)rxohead%prognam, rxohead%runby ,rxohead%crdate

    ELSEIF (head .EQ. 'COMMENT') THEN
      icom = icom + 1
      IF (line(1:11) == 'END OF FILE') THEN
        rxohead%end = 999
        EXIT
      ENDIF
      CYCLE

    ELSEIF (head .EQ. 'MARKER NAME') THEN
      READ (line, '(A60)', iostat = iac)rxohead%mrknam
      rxohead%mrknam = ADJUSTL(rxohead%mrknam)

    ELSEIF (head .EQ. 'MARKER NUMBER') THEN
      READ (line, '(A20)', iostat = iac)rxohead%mrknum
      rxohead%mrknum = ADJUSTL(rxohead%mrknum)

    ELSEIF (head .EQ. 'MARKER TYPE') THEN
      READ (line, '(A20)', iostat = iac)rxohead%mrktyp
      rxohead%mrktyp = ADJUSTL(rxohead%mrktyp)

    ELSEIF (head .EQ. 'OBSERVER / AGENCY') THEN
      READ (line, '(A20,A40)', iostat = iac)rxohead%observ,rxohead%agency
      rxohead%observ = ADJUSTL(rxohead%observ)
      rxohead%agency = ADJUSTL(rxohead%agency)

    ELSEIF (head .EQ. 'REC # / TYPE / VERS') THEN
      READ (line, '(3A20)', iostat = iac)rxohead%recnum,rxohead%rectyp,rxohead%recvers
      rxohead%recnum = ADJUSTL(rxohead%recnum)
      rxohead%rectyp = ADJUSTL(rxohead%rectyp)
      rxohead%recvers = ADJUSTL(rxohead%recvers)

    ELSEIF (head .EQ. 'ANT # / TYPE') THEN
      READ (line, '(2A20)', iostat = iac)rxohead%antnum,rxohead%anttyp
      rxohead%antnum = ADJUSTL(rxohead%antnum)
      rxohead%anttyp = ADJUSTL(rxohead%anttyp)

    ELSEIF (head .EQ. 'APPROX POSITION XYZ') THEN
      READ (line, '(3F14.4)', iostat = iac)(rxohead%aprpos(ii),ii=1,3)

    ELSEIF (head .EQ. 'ANTENNA: DELTA H/E/N') THEN
      READ (line, '(3F14.4)', iostat = iac)(rxohead%anthen(ii),ii=3,1,-1)

    ELSEIF (head .EQ. 'ANTENNA: DELTA X/Y/Z') THEN
      READ (line, '(3F14.4)', iostat = iac)(rxohead%antxyz(ii),ii=1,3)

    ELSEIF (head .EQ. 'ANTENNA: PHASECENTER') THEN
      ipha=ipha + 1
      phaseli(ipha) = line

    ELSEIF (head .EQ. 'ANTENNA: B.SIGHT XYZ') THEN
      READ (line, '(3F14.4)', iostat = iac)(rxohead%antbore(ii),ii=1,3)

    ELSEIF (head .EQ. 'ANTENNA: ZERODIR AZI') THEN
      READ (line, '(F14.4)', iostat = iac)rxohead%zeroazi

    ELSEIF (head .EQ. 'ANTENNA: ZERODIR XYZ') THEN
      READ (line, '(3F14.4)', iostat = iac)(rxohead%zeroxyz(ii),ii=1,3)

    ELSEIF (head .EQ. 'CENTER OF MASS: XYZ') THEN
      READ (line, '(3F14.4)', iostat = iac)(rxohead%comxyz(ii),ii=1,3)

!   DUMMY_READ
    ELSEIF (head .EQ. 'WAVELENGTH FACT L1/2') THEN
      READ (line, '(2(5X,I1))', iostat = iac) wfac1,wfac2

    ELSEIF (head .EQ. 'SYS / # / OBS TYPES') THEN
      READ (line, '(A1,2X,I3,13(1X,A3))', iostat = iac)satsys,items,(list(ii),ii=1,13)
      IF (satsys /= " ") THEN
        obslin = obslin + 1
! Allocation
        DO ii=0,(maxsys-1)
          IF (satsys == g_rnxsys(ii)) THEN
            isys = ii
            rxohead%otyp(isys)%obsnum = items
            allocat = .FALSE.
            IF (ASSOCIATED(rxohead%otyp(isys)%obstyp)) THEN
              IF (rxohead%otyp(isys)%obsnum /= SIZE(rxohead%otyp(isys)%obstyp)) THEN
                DEALLOCATE(rxohead%otyp(isys)%obstyp,stat=iac)
                DEALLOCATE(rxohead%otyp(isys)%antpha,stat=iac)
                DEALLOCATE(rxohead%otyp(isys)%factor,stat=iac)
                DEALLOCATE(rxohead%otyp(isys)%numobs,stat=iac)
                allocat = .TRUE.
              ENDIF
            ELSE
              allocat = .TRUE.
            ENDIF
            IF (allocat) THEN
              ALLOCATE(rxohead%otyp(isys)%obstyp(rxohead%otyp(isys)%obsnum),stat=iac)
              CALL alcerr(iac, 'rxohead%otyp(isys)%obstyp', (/rxohead%otyp(isys)%obsnum/), 'rdr3oh')
              rxohead%otyp(isys)%obstyp = ''
              ALLOCATE(rxohead%otyp(isys)%antpha(rxohead%otyp(isys)%obsnum,3),stat=iac)
              CALL alcerr(iac, 'rxohead%otyp(isys)%antpha', (/rxohead%otyp(isys)%obsnum,3/), 'rdr3oh')
              rxohead%otyp(isys)%antpha = 0d0
              ALLOCATE(rxohead%otyp(isys)%factor(rxohead%otyp(isys)%obsnum),stat=iac)
              CALL alcerr(iac, 'rxohead%otyp(isys)%factor', (/rxohead%otyp(isys)%obsnum/), 'rdr3oh')
              rxohead%otyp(isys)%factor = 1
              ALLOCATE(rxohead%otyp(isys)%numobs(rxohead%otyp(isys)%obsnum,49),stat=iac)
              CALL alcerr(iac, 'rxohead%otyp(isys)%numobs', (/rxohead%otyp(isys)%obsnum,49/), 'rdr3oh')
              rxohead%otyp(isys)%numobs = 0
            ENDIF
            EXIT
          ENDIF
        ENDDO
! Filling
        irow = 1
        DO ii=1,MIN(items,13)
          rxohead%otyp(isys)%obstyp(ii) = list(ii)
        ENDDO
      ELSE
        ilist = irow * 13
        irow = irow + 1
        DO ii=ilist+1,MIN(rxohead%otyp(isys)%obsnum,irow*13)
          rxohead%otyp(isys)%obstyp(ii) = list(ii-(irow-1)*13)
        ENDDO
      ENDIF

!   DUMMY_READ
    ELSEIF ((head .EQ. 'SYS / PHASE SHIFTS').or.(head .EQ. 'SYS / PHASE SHIFT')) THEN
      READ (line, '(A1,1X,A3,1X,F8.5)', iostat = iac) qsys,qobs,qcycle

!   DUMMY_READ
    ELSEIF (head .EQ. 'GLONASS SLOT / FRQ #') THEN
      READ (line, '(A60)', iostat = iac) dummyslot

    ELSEIF (head .EQ. 'SIGNAL STRENGTH UNIT') THEN
      READ (line, '(A20)', iostat = iac)rxohead%streng
      rxohead%streng = ADJUSTL(rxohead%streng)

    ELSEIF (head .EQ. 'INTERVAL') THEN
      READ (line, '(F10.3)', iostat = iac)rxohead%obsint

    ELSEIF (head .EQ. 'TIME OF FIRST OBS') THEN
      READ (line, '(5I6,F13.7,5X,A3)', iostat = iac)year,month,day,hour,minute,second, &
                                                         rxohead%timsys
      rday = day + hour / 24d0 + minute / 1440d0 + second / 86400d0
      rxohead%tfirst = djul(year,month,rday)

    ELSEIF (head .EQ. 'TIME OF LAST OBS') THEN
      READ (line, '(5I6,F13.7,5X,A3)', iostat = iac)year,month,day,hour,minute,second, &
                                                         rxohead%timsys
      rday = day + hour / 24d0 + minute / 1440d0 + second / 86400d0
      rxohead%tlast = djul(year,month,rday)

    ELSEIF (head .EQ. 'TIME OF LAST  OBS') THEN
      READ (line, '(5I6,F13.7,5X,A3)', iostat = iac)year,month,day,hour,minute,second, &
                                                         rxohead%timsys
      rday = day + hour / 24d0 + minute / 1440d0 + second / 86400d0
      rxohead%tlast = djul(year,month,rday)

    ELSEIF (head .EQ. 'RCV CLOCK OFFS APPL') THEN
      READ (line, '(I6)', iostat = iac)rxohead%recloff

    ELSEIF (head .EQ. 'SYS / DCBS APPLIED') THEN
      IF (idcb == 0) THEN
        IF (ASSOCIATED(rxohead%dcb)) DEALLOCATE(rxohead%dcb,stat=iac)
        ALLOCATE(rxohead%dcb(maxsys-1),stat=iac)
        CALL alcerr(iac, 'rxohead%dcb', (/4/), 'rdr3oh')
        DO isys=0,(maxsys-1)
          rxohead%dcb(isys)%prognam = ''
          rxohead%dcb(isys)%source  = ''
        ENDDO
      ENDIF
      idcb = idcb + 1
      DO ii=0,(maxsys-1)
        IF (line(1:1) == g_rnxsys(ii)) THEN
          isys = ii
          READ (line, '(A1,1X,A17,1X,A40)', iostat = iac) &
                rxohead%dcb(isys)%prognam,rxohead%dcb(isys)%source
          EXIT
        ENDIF
      ENDDO

    ELSEIF (head .EQ. 'SYS / PCVS APPLIED') THEN
      IF (ipcv == 0) THEN
        IF (ASSOCIATED(rxohead%pcv)) DEALLOCATE(rxohead%pcv,stat=iac)
        ALLOCATE(rxohead%pcv(maxsys-1),stat=iac)
        CALL alcerr(iac, 'rxohead%pcv', (/4/), 'rdr3oh')
        DO isys=0,(maxsys-1)
          rxohead%pcv(isys)%prognam = ''
          rxohead%pcv(isys)%source  = ''
        ENDDO
      ENDIF
      ipcv = ipcv + 1
      DO ii=0,(maxsys-1)
        IF (line(1:1) == g_rnxsys(ii)) THEN
          isys = ii
          READ (line, '(A1,1X,A17,1X,A40)', iostat = iac) &
                rxohead%pcv(isys)%prognam,rxohead%pcv(isys)%source
          EXIT
        ENDIF
      ENDDO

    ELSEIF (head .EQ. 'SYS / SCALE FACTOR' ) THEN
      ifac=ifac + 1
      factorli(ifac) = line

    ELSEIF (head .EQ. 'LEAP SECONDS') THEN
      READ (line, '(I6)', iostat = iac)rxohead%leapsec

    ELSEIF (head .EQ. '# OF SATELLITES') THEN
      READ (line, '(I6)', iostat = iac)rxohead%nsat

    ELSEIF (head .EQ. 'SYS / # OF OBS' .AND. first) THEN
      READ (line, '(3X,A1,I2.2,9I6)', iostat = iac)satsys,iprn,(listi(ii),ii=1,9)
      IF (satsys /= " ") THEN
        DO ii=0,(maxsys-1)
          IF (satsys == g_rnxsys(ii)) THEN
            isys = ii
            EXIT
          ENDIF
        ENDDO
!
        prnhelp = iprn + isys*100
        iprn = prn2prn(prnhelp,ABS((rxohead%tlast-rxohead%tfirst)/2))-isys*100
!
        irow = 1
        DO ii=1,MIN(rxohead%otyp(isys)%obsnum,9)
          rxohead%otyp(isys)%numobs(ii,iprn) = listi(ii)
        ENDDO
      ELSE
        ilist = irow * 9
        irow = irow + 1
        DO ii=ilist+1,MIN(rxohead%otyp(isys)%obsnum,irow*9)
          rxohead%otyp(isys)%numobs(ii,iprn) = listi(ii-(irow-1)*9)
        ENDDO
      ENDIF

!   DUMMY_READ
    ELSEIF (head .EQ. 'PRN / # OF OBS') THEN
      READ (line, '(A60)', iostat = iac) dummynumobsprn

    ELSEIF(head .EQ. 'END OF HEADER') THEN
      EXIT

    ELSE
      WRITE(lfnerr,"(' *** SR RDR3OH: Unknown header label in RINEX ', &
                             &       'file.',                          &
                             & /,16X,'Header label: ',A,               &
                             & /,16X,'Filename    : ',A,               &
                             & /,16X,'File reading stopped!',/)")              &
                                          head,TRIM(ADJUSTL(filnam))
      rxohead%end = 4
      RETURN
    ENDIF
    IF (iac /= 0) THEN
      WRITE(lfnerr,"(' ### SR RDR3OH: Error decoding line:',/,/,A,A,/, &
                             & /,16X,'Filename: ',A,                 &
                             & /,16X,'File reading stopped!',/)")    &
                             line,head,TRIM(ADJUSTL(filnam))
      rxohead%end = 4
      RETURN
    ENDIF
  ENDDO

  IF (obslin > 0) THEN
    IF (ipha /= 0) THEN
      DO ii=1,ipha
        DO jj=0,(maxsys-1)
          IF (phaseli(ii)(1:1) == g_rnxsys(jj)) THEN
            isys = jj
            IF (.NOT. ASSOCIATED(rxohead%otyp(isys)%antpha)) THEN
              ALLOCATE(rxohead%otyp(isys)%antpha(rxohead%otyp(isys)%obsnum,3),stat=iac)
              CALL alcerr(iac, 'rxohead%otyp(isys)%antpha', (/rxohead%otyp(isys)%obsnum,3/), 'rdr3oh')
              rxohead%otyp(isys)%antpha = 0d0
            ENDIF
            iobs = listc1(0,3,rxohead%otyp(isys)%obsnum,rxohead%otyp(isys)%obstyp, &
                 phaseli(ii)(3:5),rxohead%otyp(isys)%obsnum)
            READ (phaseli(ii),'(5X,F9.4,2F14.4)', iostat = iac)(rxohead%otyp(isys)%antpha(iobs,kk),kk=1,3)
            IF (iac /= 0) THEN
              WRITE(lfnerr,"(' ### SR RDR3OH: Error decoding PHASECENTER line:',/,/,A,/, &
                             & /,16X,'Filename: ',A,                 &
                             & /,16X,'File reading stopped!',/)")    &
                             line,TRIM(ADJUSTL(filnam))
              rxohead%end = 4
              RETURN
            ENDIF
            EXIT
          ENDIF
        ENDDO
      ENDDO
    ENDIF
!
    IF (ifac /= 0) THEN
      DO kk=1,ifac
        READ (factorli(kk), '(A1,1X,I4,2X,I2,12(1X,A3))', iostat = iac) &
                                          satsys,factor,items,(list(ii),ii=1,12)
        IF (iac /= 0) THEN
          WRITE(lfnerr,"(' ### SR RDR3OH: Error decoding SCALE FACTOR line:',/,/,A,/, &
                             & /,16X,'Filename: ',A,                 &
                             & /,16X,'File reading stopped!',/)")    &
                             line,TRIM(ADJUSTL(filnam))
          rxohead%end = 4
          RETURN
        ENDIF
        IF (satsys /= " ") THEN
          DO jj=0,(maxsys-1)
            IF (satsys == g_rnxsys(jj)) THEN
              isys = jj
              irow = 1
              DO ii=1,MIN(items,12)
                iobs = listc1(0,3,rxohead%otyp(isys)%obsnum,rxohead%otyp(isys)%obstyp,list(ii), &
                                                     rxohead%otyp(isys)%obsnum)
                rxohead%otyp(isys)%factor(iobs) = factor
              ENDDO
              EXIT
            ENDIF
          ENDDO
        ELSE
          ilist = irow * 12
          irow = irow + 1
          DO ii=ilist+1,MIN(rxohead%otyp(isys)%obsnum,irow*12)
            jj = listc1(0,3,rxohead%otyp(isys)%obsnum,rxohead%otyp(isys)%obstyp,list(ii), &
                                            rxohead%otyp(isys)%obsnum)
            rxohead%otyp(isys)%factor(jj) = factor
          ENDDO
        ENDIF
      ENDDO
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE rdr3oh

END MODULE
