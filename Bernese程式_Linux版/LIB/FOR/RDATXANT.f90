! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE s_rdatxant
CONTAINS

SUBROUTINE rdatxant(issat,antbuf,iant,filext,AOADMT,maxsys,AOAPCV,timint)

! -------------------------------------------------------------------------
! Purpose:    Reads one antenna block in the ANTEX file
!
! Author:     A. Gaede
!
! Created:    09-Jul-2007
!
! Changes:    08-Aug-2007 AG: GPS values only and some more warning/error
!                             messages implemented
!             20-Apr-2010 HB: Initialization of variables
!             04-Oct-2011 SL: use m_bern with ONLY
!             28-APr-2012 RD: Nullify all pointers
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------
! Used modules:
  USE m_bern,     ONLY: i4b, r8b, fileNameLength, lfn001, lfnErr
  USE m_global,   ONLY: g_svnsys
  USE m_time,     ONLY: t_timint
  USE d_phaecc,   ONLY: t_phasfil, alcfrq
  USE f_djul
  USE s_alcerr
  USE s_exitrc
  USE s_rdacvinp, ONLY: opt

! No implicits
  IMPLICIT NONE

! In:
  INTEGER(i4b)                               :: issat
  TYPE(t_phasfil), DIMENSION(:), POINTER     :: antbuf
  INTEGER(i4b)                               :: iant
  CHARACTER(LEN=fileNameLength)              :: filext
  INTEGER(i4b)                               :: AOADMT
  INTEGER(i4b)                               :: maxsys

! Out:
  REAL(r8b), DIMENSION(:,:), POINTER         :: AOAPCV
  TYPE(t_timint), OPTIONAL                   :: timint

! Local variables
  CHARACTER(LEN=1)                           :: gnss
  CHARACTER(LEN=5)                           :: noazi
  CHARACTER(LEN=8)                           :: frmt1 = '(##F8.2)'
  CHARACTER(LEN=10)                          :: atxDate,sinex
  CHARACTER(LEN=13)                          :: frmt2 = '(F8.1,##F8.2)'
  CHARACTER(LEN=20)                          :: head
  CHARACTER(LEN=20)                          :: method
  CHARACTER(LEN=20)                          :: by
  CHARACTER(LEN=60)                          :: string
  CHARACTER(LEN=152)                         :: xline

  INTEGER(i4b), DIMENSION(0:2)               :: ifr
  INTEGER(i4b)                               :: ifrq
  INTEGER(i4b)                               :: ios
  INTEGER(i4b)                               :: iac
  INTEGER(i4b)                               :: isys
  INTEGER(i4b)                               :: icor
  INTEGER(i4b)                               :: yyyy
  INTEGER(i4b)                               :: mm
  INTEGER(i4b)                               :: day
  INTEGER(i4b)                               :: hh
  INTEGER(i4b)                               :: mi
  INTEGER(i4b)                               :: nfrant
  INTEGER(i4b)                               :: ielv
  INTEGER(i4b)                               :: nelv
  INTEGER(i4b)                               :: maxelv = 19
  INTEGER(i4b)                               :: iazi
  INTEGER(i4b)                               :: nazi
  INTEGER(i4b)                               :: maxazi = 73

  REAL(r8b)                                  :: DAZI
  REAL(r8b)                                  :: ZEN1
  REAL(r8b)                                  :: ZEN2
  REAL(r8b)                                  :: DZEN
  REAL(r8b)                                  :: AZI
  REAL(r8b)                                  :: DD
  REAL(r8b)                                  :: SEC

! Initialization of variables
! ---------------------------
  gnss    = ''
  noazi   = ''
  atxDate = ''
  sinex   = ''
  head    = ''
  method  = ''
  by      = ''
  string  = ''
  xline   = ''

  ifr    = 0
  ifrq   = 0
  ios    = 0
  iac    = 0
  isys   = 0
  icor   = 0
  yyyy   = 0
  mm     = 0
  day    = 0
  hh     = 0
  mi     = 0
  nfrant = 0
  ielv   = 0
  nelv   = 0
  iazi   = 0
  nazi   = 0

  DAZI = 0.D0
  ZEN1 = 0.D0
  ZEN2 = 0.D0
  DZEN = 0.D0
  AZI  = 0.D0
  DD   = 0.D0
  SEC  = 0.D0

  IF (PRESENT(timint)) THEN
    timint%t(1) = 0.D0
    timint%t(2) = 1.D20
  ENDIF

  MAIN: DO
    READ(lfn001,"(A60,A20)",iostat=ios) string,head
    IF (head == 'METH / BY / # / DATE') THEN
      READ(string,"(2A20,10X,A10)",iostat=ios) method,by,atxDate
!
    ELSEIF (head == 'DAZI                ') THEN
      READ(string,"(2X,F6.1,52X)",iostat=ios) dazi

! Number of azimuth values
      IF (dazi == 0.D0) THEN
        nazi = 1
      ELSE
        nazi = IDNINT(360.D0/dazi) + 1
      ENDIF
      IF (nazi > MAXAZI) THEN
        WRITE(lfnerr,"(/,' *** SR RDATXANT: Too many different', &
                     &   ' azimuth angles.',                     &
                     & /,18X,'File not converted!',              &
                     & /,18X,'File name          : ',A32,        &
                     & /,18X,'Antenna type       : ',A20,        &
                     & /,18X,'# of azimuth angles:',I3,          &
                     & /,18X,'MAX # allowed      :',I3,          &
                     & /,18X,'Increase MAXAZI!',/)")             &
                         filext,antbuf(iant)%name,nazi,MAXAZI
        CALL exitrc(2)
      ENDIF

    ELSEIF (head == 'ZEN1 / ZEN2 / DZEN  ') THEN
      READ(string,"(2X,3F6.1,40X)",iostat=ios) zen1,zen2,dzen
      IF (dzen  ==  0.D0) THEN
        WRITE(lfnerr,"(/,' *** SR RDATXANT: Invalid value for',  &
                     &   ' zenith increment in external file.',  &
                     & /,18X,'File not converted!',              &
                     & /,18X,'File name    : ',A32,              &
                     & /,18X,'Antenna type : ',A20,              &
                     & /,18X,'ZEN increment:',F6.1,/)")          &
                                         filext,antbuf(iant)%name,dzen
        CALL exitrc(2)
      ELSE

! Number of elevation values
        nelv = IDNINT((zen2-zen1)/dzen) + 1
      ENDIF
      IF (nelv > MAXELV) THEN
        WRITE(lfnerr,"(/,' *** SR RDATXANT: Too many different', &
                     &   ' zenith/nadir angles.',                &
                     & /,18X,'File not converted!',              &
                     & /,18X,'File name         : ',A32,         &
                     & /,18X,'Antenna type      : ',A20,         &
                     & /,18X,'# of zenith angles:',I3,           &
                     & /,18X,'MAX # allowed     :',I3,           &
                     & /,18X,'Increase MAXELV and XLINE!',/)")   &
                               filext,antbuf(iant)%name,nelv,MAXELV
        CALL exitrc(2)
      ENDIF

! Number of frequencies
    ELSEIF (head == '# OF FREQUENCIES    ') THEN
      READ(string,"(I6,54X)",iostat=ios) nfrant

    ELSEIF (head == 'VALID FROM          ') THEN
      IF (string /= ' ') THEN
        READ(string,"(2I6,I6,2I6,F13.7)",iostat=ios) yyyy,mm,day,hh,mi,sec
        dd=day+hh/24.d0+mi/1440.d0+sec/86400.d0
        IF (PRESENT(timint)) timint%t(1) = djul(yyyy,mm,dd)
      ENDIF

    ELSEIF (head == 'VALID UNTIL         ') THEN
      IF (string /= ' ') THEN
        READ(string,"(2I6,I6,2I6,F13.7)",iostat=ios) yyyy,mm,day,hh,mi,sec
        dd=day+hh/24.d0+mi/1440.d0+sec/86400.d0
        IF (PRESENT(timint)) timint%t(2) = djul(yyyy,mm,dd)
      ENDIF

    ELSEIF (head == 'SINEX CODE          ') THEN
      READ (string,"(A10)") sinex

    ELSEIF (head == 'COMMENT             ') THEN
      CYCLE

    ELSEIF (head == 'START OF FREQUENCY  ') THEN
      READ(string,"(3X,A1,I2,54X)",iostat=ios) gnss,ifrq
      IF (issat /= 0 .OR. gnss == ' ') THEN
        isys = 0
      ELSE
        DO isys=0,maxsys
          IF (gnss == g_svnsys(isys)) EXIT
! Skip unknown frequencies
          IF (isys == maxsys) THEN
            WRITE(lfnerr,"(/,' ### SR RDATXANT: Unknown frequency skipped!', &
                         & /,18X,'Antenna name: ',A20,                       &
                         & /,18X,'Frequency   : ',A1,I2.2,                   &
                         & /,18X,'File name   : ',A,/)")                     &
                                     antbuf(iant)%name,gnss,ifrq,filext
            CYCLE MAIN
          ENDIF
        ENDDO
        IF (opt%onlygps == 1 .AND. isys /= 0) THEN
          WRITE(lfnerr,"(/,' ### SR RDATXANT: Non-GPS frequency skipped!', &
                       & /,18X,'Antenna name: ',A20,                       &
                       & /,18X,'Frequency   : ',A1,I2.2,                   &
                       & /,18X,'File name   : ',A,/)")                     &
                               antbuf(iant)%name,gnss,ifrq,filext
          DO
            READ(lfn001,"(A60,A20)",iostat=ios) string,head
            IF (head == 'END OF FREQUENCY    ') CYCLE MAIN
          ENDDO
        ENDIF
      ENDIF
! Start filling antenna buffer for system isys
      IF (.NOT. ASSOCIATED(antbuf(iant)%sys(isys)%freq)) THEN
        antbuf(iant)%sys(isys)%typ    = 1
        antbuf(iant)%sys(isys)%sinex  = sinex
        antbuf(iant)%sys(isys)%method = method
        antbuf(iant)%sys(isys)%date   = atxdate
        antbuf(iant)%sys(isys)%resolu(1) = 0
        antbuf(iant)%sys(isys)%resolu(2) = dzen
        IF (opt%onlyele == 1 .OR. dazi == 0.d0) THEN
          antbuf(iant)%sys(isys)%resolu(3) = 360
        ELSE
          antbuf(iant)%sys(isys)%resolu(3) = NINT(dazi)
        ENDIF
        antbuf(iant)%sys(isys)%resolu(4) = zen2
! Allocate the buffer for freq, fac, off and pat
        CALL alcfrq(isys,nfrant,1,antbuf(iant)%sys(isys)%resolu,antbuf,iant)
        IF(issat == 0 .AND. AOADMT == iant .AND. isys == 0) THEN
          NULLIFY(AOAPCV)
          ALLOCATE(AOAPCV(nelv,2),stat=iac)
          CALL alcerr(iac,'AOAPCV(nelv,2)',(/nelv,2/),'RDATXANT')
          AOAPCV = 0d0
        ENDIF
      ENDIF

      ifr(isys) = ifr(isys) + 1
      antbuf(iant)%sys(isys)%nfreq  = antbuf(iant)%sys(isys)%nfreq + 1
      antbuf(iant)%sys(isys)%freq(ifr(isys))%freq = ifrq

! Read all information within frequency section
! ---------------------------------------------
      DO
        READ(lfn001,"(A60,A20)",iostat=ios) string,head
        IF (head == 'NORTH / EAST / UP   ') THEN
          READ(string,"(3F10.2)",iostat=ios) &
               (antbuf(iant)%sys(isys)%freq(ifr(isys))%off(0,icor),icor=1,3)

          READ(lfn001,"(3X,A5,A152)",iostat=ios) noazi,xline
          IF (noazi /= 'NOAZI') THEN
            WRITE(lfnerr,"(/,' *** SR RDATXANT: NOAZI line not found in ', &
                         &   'frequency section!',                         &
                         & /,18X,'Antenna name: ',A20,                     &
                         & /,18X,'Frequency   : ',A1,I2.2,                 &
                         & /,18X,'File name   : ',A,/)")                   &
                               antbuf(iant)%name,gnss,ifrq,filext
            CALL exitrc(2)
          ENDIF
! Save non azimuth dependent pattern
          IF (nazi == 1 .OR. opt%onlyele == 1 .OR. &
             (issat == 0 .AND. AOADMT == iant)) THEN
            WRITE(frmt1(2:3),'(I2)',iostat=ios) nelv
            READ(xline,frmt1,iostat=ios) &
                 (antbuf(iant)%sys(isys)%freq(ifr(isys))%pat(0,ielv,1),ielv=1,nelv)
              IF (ios /= 0) THEN
                WRITE(lfnerr,"(/,' *** SR RDATXANT: Error while reading', &
                     &   ' NOAZI line!',                                  &
                     & /,18X,'Antenna name: ',A20,                        &
                     & /,18X,'Frequency   : ',A1,I2.2,                    &
                     & /,18X,'File name   : ',A,/)")                      &
                               antbuf(iant)%name,gnss,ifrq,filext
                CALL exitrc(2)
              ENDIF
! Save non azimuth dependent pattern of "AOAD/M_T   NONE"
            IF(issat == 0 .AND. AOADMT == iant .AND. isys == 0) THEN
              DO ielv=1,nelv
                AOAPCV(ielv,ifr(isys)) = antbuf(iant)%sys(isys)%freq(ifr(isys))%pat(0,ielv,1)
              ENDDO
            ENDIF

            IF (opt%onlyele == 1 .AND. nazi /= 1) THEN
              DO iazi=1,nazi
                READ(lfn001,'(A152)',iostat=ios) xline
              ENDDO
            ENDIF
          ENDIF
! Save azimuth and elevation dependent pattern
          IF(nazi /= 1 .AND. opt%onlyele /= 1) THEN
            WRITE(frmt2(7:8),'(I2)',iostat=ios) nelv
            DO iazi=1,nazi
              READ(lfn001,frmt2,iostat=ios) azi, &
                   (antbuf(iant)%sys(isys)%freq(ifr(isys))%pat(0,ielv,iazi),ielv=1,nelv)
              IF ((DBLE(iazi-1)*dazi) /= azi .OR. ios /= 0) THEN
                WRITE(lfnerr,"(/,' *** SR RDATXANT: Error while reading', &
                     &   ' PCV section!',                                 &
                     & /,18X,'Antenna name: ',A20,                        &
                     & /,18X,'Frequency   : ',A1,I2.2,                    &
                     & /,18X,'File name   : ',A,/)")                      &
                               antbuf(iant)%name,gnss,ifrq,filext
                CALL exitrc(2)
              ENDIF
            ENDDO
          ENDIF

        ELSEIF (head == 'END OF FREQUENCY    ') THEN
          CYCLE MAIN

        ELSEIF (head == 'COMMENT             ') THEN
          CYCLE

        ELSE
          WRITE(lfnerr,"(/,' *** SR RDATXANT: Unknown header lable in', &
                       &   ' frequency section!',                       &
                       & /,18X,'Antenna name: ',A20,                    &
                       & /,18X,'Frequency   : ',A1,I2.2,                &
                       & /,18X,'Header lable: ',A20,                    &
                       & /,18X,'File name   : ',A,/)")                  &
                               antbuf(iant)%name,gnss,ifrq,head,filext
          CALL exitrc(2)
        ENDIF
      ENDDO
! End of frequency section
! ------------------------

    ELSEIF (head == 'START OF FREQ RMS   ') THEN
      DO
        READ(lfn001,"(A60,A20)",iostat=ios) string,head
        IF (head == 'END OF FREQ RMS     ') EXIT
      ENDDO
      CYCLE

    ELSEIF (head == 'END OF ANTENNA      ') THEN
      EXIT

    ELSEIF (head(1:16) == 'TYPE / SERIAL NO'  .OR. &
            head(1:14) == 'END OF ANTENNA'    .OR. &
            head(1:16) == '# OF FREQUENCIES'  .OR. &
            head(1:16) == 'END OF FREQUENCY'  .OR. &
            head(1:18) == 'START OF FREQUENCY'.OR. &
            head(1:4)  == 'DAZI'              .OR. &
            head(1:18) == 'ZEN1 / ZEN2 / DZEN'.OR. &
            head(1:17) == 'NORTH / EAST / UP')THEN
      WRITE(lfnerr,"(/,' *** SR RDATXANT: Missing blanks at the end of', &
                   &   ' header label in ANTEX file.',                   &
                   & /,18X,'ANTEX file does not correspond to ',         &
                   &       'ANTEX format description.',                  &
                   & /,18X,'File not converted!',                        &
                   & /,18X,'File name   : ',A32,                         &
                   & /,18X,'Header label: ',A20,/)") filext,head
      CALL exitrc(2)
    ELSE
      WRITE(lfnerr,"(/,' *** SR RDATXANT: Unknown header label in', &
                   &   ' external file.',                           &
                   & /,18X,'File not converted!',                   &
                   & /,18X,'File name   : ',A32,                    &
                   & /,18X,'Header label: ',A20,/)") filext,head
      CALL exitrc(2)
    ENDIF
  ENDDO MAIN

END SUBROUTINE rdatxant

END MODULE
