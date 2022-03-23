
! ------------------------------------------------------------------------------
! Bernese GNSS Software Version 5.2
! ------------------------------------------------------------------------------

PROGRAM ATX2PCV

! ------------------------------------------------------------------------------
! Purpose:    Conversion of ANTEX to Bernese formated PCV file.
!
! Author:     A.Gaede
!
! Created:    09-Jul-2007
!
! Changes:    09-Aug-2007 AG: Special case for "SLR     " antenna
!             30-Jan-2008 HB: Correct length of string for writing
!                             input ANTEX-file into Bernese PCV-file
!             31-Jan-2008 HB: Use stripdir for extracting ANTEX-file string
!             25-Mar-2008 SS: MAXRCV from 500 to 1000
!             11-Nov-2008 AS: String length of antGal (GIOVE instead of GALILEO)
!             26-Mar-2009 RD: Correct handling of seperate GLONASS pattern
!             27-Mar-2009 RD: Sufficient dimension for NONEANT
!             11-Mar-2010 SL: ONLY added for USE m_bern
!             23-Sep-2010 RD: Enable CPU counter
!             05-Oct-2010 SL: use extrInt to get antenna number, use undef_i
!             06-Oct-2010 RD: Exitrc added at the end
!             25-Oct-2010 SL: int to real conv bugs, sensor name trimmed
!             26-Oct-2010 SL: removal of unused modules, use undef_c
!             26-Nov-2010 SL: get antenna number from STAINFO
!             29-Sep-2011 SL: support of ANTEX V1.4, maxSys from m_global,
!                             write dANTOFF to PCV file
!             24-Nov-2011 SL: new title string for pritit
!             05-Mar-2012 LP: maxelv 19 -> 91
!             28-Mar-2012 RD: Use SVN2CHR as module now
!             27-Apr-2012 RD: Nullify pointers
!             01-Nov-2012 LP: Add BSW5.2 to title string
!             18-Feb-2013 SS: Added method: CHAMBER
!             18-Feb-2013 SS: Convert ANTEX date to YYYY-MM-DD format
!                             (in order to allow comparison of ATX/PCV dates)
!             19-Feb-2013 SS: All read statements with iostat
!                             (in particular first one and those for dates)
!             19-Feb-2013 SS: idate to control date format of PCV file
!             19-Feb-2013 SS/SL: Find AOAD/M_T with 0 or undef_i number
!             19-Feb-2013 SS: Mandatory correction concerning abs2rel case
!                             (PCV conversion with extrea instead of recant)
!             19-Feb-2013 SS: Ignore ANTEX satellite info in case of abs2rel
!             19-Feb-2013 SS: opt%convert to be implemented (...)
!             19-Feb-2013 SS: Activated CALL EXITRC(2)
!             21-Feb-2013 SS: Stop if unknown method found in ATX/PCV file
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ------------------------------------------------------------------------------
! Used modules
! ------------
  USE m_bern,   ONLY: fileNameLength, i4b, r8b, lfn001, lfnErr, lfnPrt
  USE m_cpu,    ONLY: cpu_start
  USE m_global, ONLY: maxSys, g_atxsys, g_svnsys, antGPS, antGLO, antGAL, &
                      antGEO, antCOM, antQZS
  USE m_time,   ONLY: t_timint
  USE m_maxdim, ONLY: maxsaa, maxrec
  USE d_const,  ONLY: date,time
  USE d_inpkey, ONLY: inpKey,init_inpkey
  USE d_phaecc, ONLY: t_phasfil,init_buf, wtphafil, recant, satant, alcantbu, &
                      alcfrq
  USE d_satfil, ONLY: t_satfil,init_satfil,typeMWTR
  USE d_stacrx, ONLY: t_stacrux, undef_c, undef_i
  USE f_djul
  USE f_extrint
  USE f_iyear4
  USE f_listc1
  USE s_addant
  USE s_alcerr
  USE s_chr2svn
  USE s_cordup
  USE s_defcon
  USE s_exitrc
  USE s_getrcv
  USE s_gtflna
  USE s_opnerr
  USE s_opnfil
  USE s_opnsys
  USE s_prflna
  USE s_pritit
  USE s_prn2svn
  USE s_rdacvinp, ONLY: opt, rdacvinp
  USE s_rdatxant
  USE s_rdsatfil
  USE s_readcrux
  USE s_readinpf
  USE s_stripdir
  USE s_svn2chr
  USE s_svn2prn
  USE s_upperc
  IMPLICIT NONE

! Local variables
! ---------------
  TYPE(t_stacrux), SAVE                              :: stacrux
  TYPE(t_satfil),  SAVE                              :: satfil
  TYPE(t_phasfil), DIMENSION(:), POINTER             :: extrea
  TYPE(t_phasfil), DIMENSION(:), POINTER             :: extsaa
  TYPE(t_timint),  DIMENSION(:), ALLOCATABLE         :: timint
  TYPE(t_timint)                                     :: timcheck

  CHARACTER(LEN=fileNameLength)                      :: filsat
  CHARACTER(LEN=fileNameLength)                      :: filsta
  CHARACTER(LEN=fileNameLength)                      :: filext
  CHARACTER(LEN=fileNameLength)                      :: filpcv
  CHARACTER(LEN=fileNameLength)                      :: filphc
  CHARACTER(LEN=fileNameLength)                      :: filrcv
  CHARACTER(LEN=132)                                 :: line
  CHARACTER(LEN=80)                                  :: TITLE
  CHARACTER(LEN=60)                                  :: FILINFO
  CHARACTER(LEN=60)                                  :: string
  CHARACTER(LEN=26), DIMENSION(:), ALLOCATABLE       :: ANTHLP
  CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE       :: outant
  CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE       :: noneant
  CHARACTER(LEN=20)                                  :: head
  CHARACTER(LEN=20)                                  :: name
  CHARACTER(LEN=20)                                  :: lastant
  CHARACTER(LEN=20)                                  :: INDTEST
  CHARACTER(LEN=10)                                  :: cosp
  CHARACTER(LEN=10)                                  :: csvn
  CHARACTER(LEN=10), DIMENSION(:), ALLOCATABLE       :: cospar
  CHARACTER(LEN=4),  DIMENSION(:), ALLOCATABLE       :: svnatx
  CHARACTER(LEN=5),  DIMENSION(7)                    :: method
  CHARACTER(LEN=4)                                   :: svnnr
  CHARACTER(LEN=4)                                   :: diffoff
  CHARACTER(LEN=4)                                   :: diffpcv
  CHARACTER(LEN=4)                                   :: difnpcv
  CHARACTER(LEN=3), DIMENSION(12)                    :: month
  CHARACTER(LEN=1)                                   :: syst
  CHARACTER(LEN=1)                                   :: pcvtyp
  CHARACTER(LEN=1)                                   :: sysflag
  CHARACTER(LEN=1)                                   :: filtyp
  CHARACTER(LEN=197)                                 :: forma1
  CHARACTER(LEN=354)                                 :: forma2
  CHARACTER(LEN=329)                                 :: forma3
  CHARACTER(LEN=247)                                 :: forma4

  INTEGER(i4b), DIMENSION(:), ALLOCATABLE            :: linatx
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE            :: linsat
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE            :: sindx
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE            :: rindx
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE            :: scode
  INTEGER(i4b)                                       :: rcode
  INTEGER(i4b)                                       :: isyst
  INTEGER(i4b), DIMENSION(0:maxSys)                  :: recsyst
  INTEGER(i4b)                                       :: esat
  INTEGER(i4b)                                       :: gpscop
  INTEGER(i4b)                                       :: find
  INTEGER(i4b)                                       :: arcv
  INTEGER(i4b)                                       :: prn
  INTEGER(i4b)                                       :: prncheck
  INTEGER(i4b)                                       :: forget
  INTEGER(i4b)                                       :: antcnt
  INTEGER(i4b)                                       :: antcnt2
  INTEGER(i4b)                                       :: found
  INTEGER(i4b)                                       :: irc
  INTEGER(i4b)                                       :: ios
  INTEGER(i4b)                                       :: ii
  INTEGER(i4b)                                       :: isys
  INTEGER(i4b)                                       :: inone
  INTEGER(i4b)                                       :: iszero
  INTEGER(i4b)                                       :: idome
  INTEGER(i4b)                                       :: icrx
  INTEGER(i4b)                                       :: imeth
  INTEGER(i4b)                                       :: oimeth
  INTEGER(i4b)                                       :: add
  INTEGER(i4b)                                       :: oldelv
  INTEGER(i4b)                                       :: extelv
  INTEGER(i4b)                                       :: oldazi
  INTEGER(i4b)                                       :: extazi
  INTEGER(i4b)                                       :: iazi
  INTEGER(i4b)                                       :: nazi
  INTEGER(i4b)                                       :: help
  INTEGER(i4b)                                       :: help1
  INTEGER(i4b)                                       :: iac
  INTEGER(i4b)                                       :: irec
  INTEGER(i4b)                                       :: irec2
  INTEGER(i4b)                                       :: nrec = 0
  INTEGER(i4b)                                       :: nrec1 = 0
  INTEGER(i4b)                                       :: nrec2 = 0
  INTEGER(i4b)                                       :: iatx
  INTEGER(i4b)                                       :: iord
  INTEGER(i4b)                                       :: isat
  INTEGER(i4b)                                       :: ren
  INTEGER(i4b)                                       :: sat
  INTEGER(i4b)                                       :: nsat = 0
  INTEGER(i4b)                                       :: nsat1 = 0
  INTEGER(i4b)                                       :: nsat2 = 0
  INTEGER(i4b)                                       :: oant
  INTEGER(i4b)                                       :: ielv
  INTEGER(i4b)                                       :: nelv
  INTEGER(i4b)                                       :: nfpcv
  INTEGER(i4b)                                       :: icor
  INTEGER(i4b)                                       :: iform
  INTEGER(i4b)                                       :: idate = 1
  INTEGER(i4b)                                       :: ifrq
  INTEGER(i4b)                                       :: svnr
  INTEGER(i4b)                                       :: YYYY
  INTEGER(i4b)                                       :: MM
  INTEGER(i4b)                                       :: DD
  INTEGER(i4b)                                       :: aoadmt
  INTEGER(i4b)                                       :: count = 0
  INTEGER(i4b)                                       :: ngpssat
  INTEGER(i4b)                                       :: nglosat
  INTEGER(i4b)                                       :: ngalsat
  INTEGER(i4b)                                       :: ngeosat
  INTEGER(i4b)                                       :: ncomsat
  INTEGER(i4b)                                       :: nqzssat
  INTEGER(i4b)                                       :: maxrcv = 1000
  INTEGER(i4b)                                       :: maxelv = 91
  INTEGER(i4b)                                       :: maxazi = 73
  INTEGER(i4b)                                       :: NFREQ
  INTEGER(i4b), DIMENSION(2)                         :: ICODE
  INTEGER(i4b), DIMENSION(2)                         :: IWLFAC
  INTEGER(i4b)                                       :: ICLASS

  REAL(r8b), DIMENSION(:,:,:), ALLOCATABLE           :: dANTOFF
  REAL(r8b), DIMENSION(:,:,:), ALLOCATABLE           :: hlppcv
  REAL(r8b), DIMENSION(:,:), POINTER                 :: AOAPCV
  REAL(r8b)                                          :: epo
  REAL(r8b)                                          :: omjd
  REAL(r8b)                                          :: amjd

  LOGICAL                                            :: first
  LOGICAL                                            :: first2

  Month = (/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'/)
  Method = (/'     ','ADOPT','COPIE','CONVE','FIELD','ROBOT','CHAMB'/)

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Nullify pointers
! ----------------
  NULLIFY(extrea)
  NULLIFY(extsaa)
  NULLIFY(aoapcv)

! Get the name of the input file
! ------------------------------
  CALL init_inpkey(inpKey)
  CALL readinpf(' ',inpKey)

! Define system files
! -------------------
  CALL opnsys

! Define constants
! ----------------
  CALL defcon(1)

! Write title and file list
! -------------------------
  CALL pritit('ATX2PCV','Convert ANTEX to Bernese format')
  CALL prflna

! Get filename of external phc file
! ---------------------------------
  CALL gtflna(1,'PHASEXT',filext,irc)

! Get filename of old phase file
! ------------------------------
  CALL gtflna(0,'PHASECC',filpcv,irc)

! Get filename of station infiormation file
! -----------------------------------------
  CALL gtflna(0,'STAINFO',filsta,irc)

! Get filename of station infiormation file
! -----------------------------------------
  CALL gtflna(0,'RECEIVR',filrcv,irc)

! Get antenna phase center file name for output
! ---------------------------------------------
  CALL gtflna(0,'PHASRSG',filphc,irc)

! Read input panels
! -----------------
  CALL rdacvinp

! Open external file and detect format
! ------------------------------------
  CALL opnfil(lfn001,filext,'OLD','FORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnerr,lfn001,ios   ,filext,'ATX2PCV')

  iform=0
  READ(lfn001,'(A)',iostat=irc) line
  IF (line(61:80) == 'ANTEX VERSION / SYST') THEN
    IF (line(6:8) == '1.1'.OR. &
        line(6:8) == '1.2'.OR. &
        line(6:8) == '1.3'.OR. &
        line(6:8) == '1.4') THEN
! ANTEX (Version 1.1, 1.2, 1.3, 1.4)
      iform=2
      READ(line(21:21),'(A1)',iostat=irc) syst
    ELSE
      WRITE(lfnerr,"(/,' *** PG ATX2PCV: Unknown ANTEX format version.', &
               &  /,17X,'File not converted!',                           &
               &  /,17X,'File name    : ',A,                             &
               &  /,17X,'ANTEX version: ',A,/)") TRIM(filext),line(6:8)
      CALL exitrc(2)
    ENDIF
  ELSE
    DO
      READ(lfn001,'(A)',iostat=irc) line
      IF (irc /= 0) THEN
        CLOSE(UNIT=lfn001)
        WRITE(lfnerr,"(/,' *** PG ATX2PCV: Unknown antenna ', &
             &     'phase center file format.',               &
             &     /,17X,'File not converted!',               &
             &     /,17X,'File name: ',A,/)") TRIM(filext)
        CALL exitrc(2)
      ELSEIF (line(1:26) == ' [north]  [ east]  [  up ]') THEN
! Format by Gerry Mader
        iform=1
        EXIT
      ENDIF
    ENDDO
  ENDIF
  IF (opt%debug == 1) WRITE(*,*)'Format detection ok'

! Input format by Gerry Mader (NGS)
! =================================
  IF (iform == 1) THEN

! Read once to count the antennas
    DO
      READ(lfn001,'(A)',iostat=irc) LINE
      IF (LINE == ' ') EXIT
    ENDDO
    DO
      READ(lfn001,"(A20)",iostat=irc) INDTEST
      IF (INDTEST == ' ') EXIT
      nrec = nrec + 1
      DO ii=1,3
        READ(lfn001,"()",iostat=irc)
      ENDDO
    ENDDO
    REWIND(lfn001)
! Allocate the buffer
    CALL alcantbu(nrec,maxsys,extrea)

! Skip header lines
! -----------------
    DO
      READ(LFN001,'(A)',iostat=irc) LINE
      IF (LINE.EQ.' ') EXIT
    ENDDO

! Loop over all antennas available in external file
! -------------------------------------------------
    irec = 0
    DO
      READ(LFN001,"(A20)",iostat=irc) INDTEST
      IF (INDTEST.EQ.' ') EXIT

      irec = irec + 1
      CALL alcfrq(0,2,1,(/0,5,360,90/),extrea,irec)
      IF (irec > MAXRCV) THEN
        WRITE(LFNERR,"(/,' *** PG ATX2PCV: Too many antennas in file.', &
                     & /,17X,'File name    : ',A,                       &
                     & /,17X,'MAXRCV       : ',I6,/)") filext,maxrcv
        CALL exitrc(2)
      ENDIF

      extrea(irec)%name = INDTEST
      DO ifrq=1,2
        READ(lfn001,*,iostat=irc) &
          (extrea(irec)%sys(0)%freq(ifrq)%off(0,icor),icor=1,3)
        READ(lfn001,*,iostat=irc) &
          (extrea(irec)%sys(0)%freq(ifrq)%pat(0,ielv,1),ielv= 1,10)
        READ(lfn001,*,iostat=irc) &
          (extrea(irec)%sys(0)%freq(ifrq)%pat(0,ielv,1),ielv=11,19)

      ENDDO
    ENDDO

! End of loop, close file
! -----------------------
    CLOSE(UNIT=LFN001)
    IF (opt%debug == 1) WRITE(*,*)'NGS format reading ok'

! ANTEX (Version 1.3) input format
! ================================
! Not read: - HEADER
!           - COMMENTS
!           - RMS VALUE SECTION
! SERIAL No. HARD-WIRED if < 0 or > 999999 or no INTEGER

  ELSEIF (iform == 2) THEN

! Get and read the satellite information file
    CALL gtflna(1,'SATELL ',filsat,irc)
    CALL init_satfil(satfil)
    CALL rdsatfil(filsat,satfil)

! Read complete ANTEX and count # of receiver and # of satellite antennas
! -----------------------------------------------------------------------
    DO
      READ(lfn001,'(A)',iostat=irc) line
      IF (irc < 0) THEN
        EXIT
      ELSEIF (line(61:80) == 'START OF ANTENNA    ') THEN
        DO
          READ(lfn001,'(A)',iostat=irc) line
          IF (irc < 0) THEN
            WRITE(lfnerr,"(/,' *** PG ATX2PCV: Unexpected end of phase ', &
                 &          'center file within antenna block.',          &
                 &    /,17X,'File not converted!',                        &
                 &    /,17X,'File name           : ',A,/)")TRIM(filext)
            CALL exitrc(2)
          ELSEIF (line(61:80) == 'START OF ANTENNA    ') THEN
            WRITE(lfnerr,"(/,' *** PG ATX2PCV: Error while searching for', &
                 &          'antenna name in phase center file.',          &
                 &    /,17X,'File not converted!',                         &
                 &    /,17X,'Last read antenna: ',A20,/                    &
                 &    /,17X,'File name        : ',A,/)")lastant,TRIM(filext)
            CALL exitrc(2)
          ELSEIF (line(61:80) == 'TYPE / SERIAL NO    ') THEN
            IF (line(1:LEN(antGPS)) == antGPS .OR. &
                line(1:LEN(antGLO)) == antGLO .OR. &
                line(1:LEN(antGAL)) == antGAL .OR. &
                line(1:LEN(antGEO)) == antGEO .OR. &
                line(1:LEN(antCOM)) == antCOM .OR. &
                line(1:LEN(antQZS)) == antQZS) THEN
              nsat = nsat + 1
            ELSE
              nrec = nrec + 1
            ENDIF
            lastant = line(1:20)
            EXIT
          ENDIF
        ENDDO
      ENDIF
    ENDDO
    REWIND(lfn001)
    IF (opt%debug == 1) WRITE(*,*)'Count nsat and nrec ok'

! Allocate antenna buffers
! ------------------------
    CALL alcantbu(maxrec,maxsys,extrea)
    CALL alcantbu(maxsaa,0,extsaa)

! Allocate special arrays
    ALLOCATE(scode(nsat),stat=iac)
    CALL alcerr(iac,'scode(nsat)',(/nsat/),'ATX2PCV')
    ALLOCATE(svnatx(nsat),stat=iac)
    CALL alcerr(iac,'svnatx(nsat)',(/nsat/),'ATX2PCV')
    ALLOCATE(cospar(nsat),stat=iac)
    CALL alcerr(iac,'cospar(nsat)',(/nsat/),'ATX2PCV')
    ALLOCATE(timint(nsat),stat=iac)
    CALL alcerr(iac,'timint(nsat)',(/nsat/),'ATX2PCV')

    ALLOCATE(outant(nrec),stat=iac)
    CALL alcerr(iac,'outant(nrec)',(/nrec/),'ATX2PCV')

! Save file type and find end of header in ANTEX file
! ---------------------------------------------------
    DO
      READ(lfn001,'(A)',iostat=irc) line
      IF (irc /= 0) THEN
        WRITE(lfnerr,"(/,' *** PG ATX2PCV: Error while reading header block.', &
           &    /,17X,'File not converted!',                                   &
           &    /,17X,'File name           : ',A,/)")TRIM(filext)
        CALL exitrc(2)
      ELSEIF (line(61:80) == 'PCV TYPE / REFANT   ')THEN
        READ(line,'(A1)',iostat=irc) pcvtyp
      ELSEIF (line(61:80) == 'END OF HEADER       ') THEN

! Initialize iatx, AOADMT, frmt1, frmt2
        iatx    = 0
        isat    = 0
        irec    = 0
        nGPSSAT = 0
        nGLOSAT = 0
        nGALSAT = 0
        nGEOSAT = 0
        nCOMSAT = 0
        nQZSSAT = 0
        AOADMT  = 0
        EXIT
      ENDIF
    ENDDO
    IF (opt%debug == 1) WRITE(*,*)'Skip header ok'

! Loop over all lines of the file
! -------------------------------
    DO
      READ(lfn001,"(A60,A20)",iostat=irc) string,head
      IF (irc > 0) THEN
        WRITE(lfnerr,"(/,' *** PG ATX2PCV: Error while searching for an', &
                     &   'tenna block in ANTEX file.',                    &
                     & /,17X,'File not converted!',                       &
                     & /,17X,'File name           : ',A,/)")TRIM(filext)
        CALL exitrc(2)
      ELSEIF (irc < 0) THEN
        EXIT
      ENDIF
      IF (head == 'START OF ANTENNA    ') THEN
        iatx=iatx+1
        IF (iatx > MAXRCV) THEN
          WRITE(lfnerr,"(/,' *** PG ATX2PCV: Too many antennas in file!', &
                      &  /,17X,'File name    : ',A,                       &
                      &  /,17X,'MAXRCV       : ',I6,/)") filext,MAXRCV
          CALL exitrc(2)
        ENDIF

! Loop over all lines of one antenna section
! ------------------------------------------
        DO
          READ(lfn001,"(A60,A20)",iostat=irc) string,head

          IF (head == 'TYPE / SERIAL NO    ') THEN
            iatx = iatx + 1
            READ(string,"(2A20,2A10)",iostat=irc)name,INDTEST,csvn,cosp
            INDTEST = ADJUSTL(INDTEST)

! Count number of satellite antennas and read the values in buffer
            IF (name(1:LEN(antGPS)) == antGPS .OR. &
                name(1:LEN(antGLO)) == antGLO .OR. &
                name(1:LEN(antGAL)) == antGAL .OR. &
                name(1:LEN(antGEO)) == antGEO .OR. &
                name(1:LEN(antCOM)) == antCOM .OR. &
                name(1:LEN(antQZS)) == antQZS) THEN
              IF(name(1:LEN(antGPS)) == antGPS) nGPSSAT = nGPSSAT + 1
              IF(name(1:LEN(antGLO)) == antGLO) nGLOSAT = nGLOSAT + 1
              IF(name(1:LEN(antGAL)) == antGAL) nGALSAT = nGALSAT + 1
              IF(name(1:LEN(antGEO)) == antGEO) nGEOSAT = nGEOSAT + 1
              IF(name(1:LEN(antCOM)) == antCOM) nCOMSAT = nCOMSAT + 1
              IF(name(1:LEN(antQZS)) == antQZS) nQZSSAT = nQZSSAT + 1
              isat = isat + 1
              extsaa(isat)%name = name
              timint(isat)%t(1) = 0.0D0
              timint(isat)%t(2) = 1.0D20
              CALL rdatxant(1,extsaa,isat,filext,AOADMT,0,AOAPCV,timint(isat))
! Convert ANTEX date to YYYY-MM-DD format
              MM = 0
              READ(extsaa(isat)%sys(0)%date,'(I2,5X,I2)',iostat=ios) DD,YYYY
              IF (ios == 0) THEN
                YYYY = IYEAR4(YYYY)
                CALL UPPERC(extsaa(isat)%sys(0)%date)
                MM = LISTC1(0,3,12,Month,extsaa(isat)%sys(0)%date(4:6),12)
              ENDIF
              IF (MM /= 0) THEN
                amjd = djul(YYYY,MM,DD*1.D0)
              ELSE
                READ(extsaa(isat)%sys(0)%date,'(I4,1X,I2,1X,I2)',iostat=ios) YYYY,MM,DD
                IF (ios == 0 .AND. MM /= 0) THEN
                  amjd = djul(YYYY,MM,DD*1.D0)
                ELSE
                  WRITE(lfnerr,"(/,' *** PG ATX2PCV: Unknown date', &
                               &   ' format in ANTEX file.',        &
                               & /,17X,'Antenna: ',A20,             &
                               & /,17X,'Date   : ',A10,             &
                               & /,17X,'File not converted!')")     &
                     extsaa(isat)%name,extsaa(isat)%sys(0)%date
                  CALL exitrc(2)
                ENDIF
              ENDIF
              IF (idate == 1) WRITE(extsaa(isat)%sys(0)%date,"(I2.2,'-',A3,'-',I2.2,1X)") DD,month(MM),MOD(YYYY,100)
              IF (idate == 2) WRITE(extsaa(isat)%sys(0)%date,"(I4.4,'-',I2.2,'-',I2.2)") YYYY,MM,DD
! Read PRN and SVN numbers
              IF (INDTEST(1:1) /= ' ') THEN
                READ(INDTEST,"(A1,I2)",iostat=irc)sysflag,prn
                IF (irc == 0 .AND. INDTEST(4:20)=='                 ') THEN
                  CALL chr2svn(prn,sysflag,scode(isat))
                ELSE
                  WRITE(lfnerr,"(/,' *** PG ATX2PCV: Wrong satellite code ', &
                                &   '(PRN).',/,16x,'Read code:',A)")INDTEST
                  CALL exitrc(2)
                ENDIF
              ELSE
                scode(isat)=0
              ENDIF
              IF (csvn /= '          ') THEN
                READ(csvn(1:4),'(A4)',iostat=irc) svnatx(isat)
                IF (csvn(1:1) /= INDTEST(1:1)) THEN
                  WRITE(lfnerr,"(/,' ### PG ATX2PCV: Wrong satellite code ', &
                               &   '(SVN).',/,16x,'SVN set to blank.',      &
                               & /,16x,'Read code:',A)")INDTEST
                  svnatx(isat)='    '
                ENDIF
              ELSE
                svnatx(isat)='    '
              ENDIF
              cospar(isat) = cosp
! Read the values of receiver antennas
            ELSE
              irec = irec + 1
              extrea(irec)%name = name
              outant(irec)      = INDTEST
! Read antanna number
              rcode = extrInt(INDTEST,size=5)
              IF (filsta /= ' ' .AND. INDTEST(1:1) /= ' ') THEN
                CALL readcrux(filsta,stacrux)
                DO icrx = 1,stacrux%ninfo
                  IF (stacrux%stainfo(icrx)%antnam == extrea(irec)%name .AND. &
                      ADJUSTL(staCrux%stainfo(icrx)%antser) == INDTEST) THEN
                      rcode = staCrux%staInfo(icrx)%antnum
                    IF(rcode == undef_i) THEN
                      WRITE(lfnErr, &
                        "(/,' *** PG ATX2PCV: ', &
                        &       'Antenna in ANTEX file has individual values', &
                        & /,17X,'but in the STAINFO file ANT # is set to ',I6, &
                        & /,17X,'Name : ',A20, &
                        & /,17X,'S/N  : ',A20, &
                        & /)") undef_i,extrea(irec)%name,INDTEST
                      CALL exitrc(2)
                    ENDIF
                  ENDIF
                ENDDO
              ELSE
              ENDIF
              extrea(irec)%numb = rcode
! Set antenna numbers to undef_i
              IF(opt%antnum == 1 .AND. extrInt(INDTEST) == 0) &
                extrea(irec)%numb = undef_i
! Save indices of AOAD/M_T
              IF (INDTEST(1:1) == ' ' .AND. &
                 (extrea(irec)%name == 'AOAD/M_T            ' .OR.  &
                  extrea(irec)%name == 'AOAD/M_T        NONE')) THEN
                AOADMT=irec
              ENDIF
              CALL rdatxant(0,extrea,irec,filext,AOADMT,maxsys,AOAPCV)
! Convert ANTEX date to YYYY-MM-DD format
              DO isys=0,maxsys-1
                IF (extrea(irec)%sys(isys)%nfreq == 0) CYCLE
                MM = 0
                READ(extrea(irec)%sys(isys)%date,'(I2,5X,I2)',iostat=ios) DD,YYYY
                IF (ios == 0) THEN
                  YYYY = IYEAR4(YYYY)
                  CALL UPPERC(extrea(irec)%sys(isys)%date)
                  MM = LISTC1(0,3,12,Month,extrea(irec)%sys(isys)%date(4:6),12)
                ENDIF
! Accept additional date format of EPN ANTEX
                IF (MM == 0) THEN
                  READ(extrea(irec)%sys(isys)%date,'(I2,1X,I2,1X,I4)',iostat=ios) DD,MM,YYYY
                  IF (ios /= 0) MM = 0
                ENDIF
                IF (MM /= 0) THEN
                  amjd = djul(YYYY,MM,DD*1.D0)
                ELSE
                  READ(extrea(irec)%sys(isys)%date,'(I4,1X,I2,1X,I2)',iostat=ios) YYYY,MM,DD
                  IF (ios == 0 .AND. MM /= 0) THEN
                    amjd = djul(YYYY,MM,DD*1.D0)
                  ELSE
                    WRITE(lfnerr,"(/,' *** PG ATX2PCV: Unknown date', &
                                 &   ' format in ANTEX file.',        &
                                 & /,17X,'Antenna: ',A20,             &
                                 & /,17X,'System : ',A1,              &
                                 & /,17X,'Date   : ',A10,             &
                                 & /,17X,'File not converted!')")     &
                       extrea(irec)%name,g_svnsys(isys),extrea(irec)%sys(isys)%date
                    CALL exitrc(2)
                  ENDIF
                ENDIF
                IF (idate == 1) WRITE(extrea(irec)%sys(isys)%date,"(I2.2,'-',A3,'-',I2.2,1X)") DD,month(MM),MOD(YYYY,100)
                IF (idate == 2) WRITE(extrea(irec)%sys(isys)%date,"(I4.4,'-',I2.2,'-',I2.2)") YYYY,MM,DD
              ENDDO
!
            ENDIF
            EXIT
          ENDIF
        ENDDO
      ELSE IF (head(1:16).EQ.'START OF ANTENNA') THEN
        WRITE(lfnerr,"(/,' *** PG ATX2PCV: Missing blanks at the end of', &
                     &   ' header label in ANTEX file.',                  &
                     & /,17X,'Antex file does not correspond to ',        &
                     &       'ANTEX format description.',                 &
                     & /,17X,'File not converted!',                       &
                     & /,17X,'File name   : ',A32,                        &
                     & /,17X,'Header label: ',A20,/)") FILEXT,HEAD
        CALL exitrc(2)
      ENDIF
    ENDDO

    CLOSE(UNIT=lfn001)
    IF (opt%debug == 1) WRITE(*,*)'ANTEX input ok'
! ==================
! End of ANTEX input

    IF (AOADMT == 0 .AND. (opt%convert == 1 .OR. opt%abs2rel == 1))THEN
      WRITE(lfnerr,"(/,' *** PG ATX2PCV: Antenna [AOAD/M_T    ', &
                   &  '   NONE] not found in ANTEX file.',       &
                   &  /,17X,'Phase file name: ',A40,             &
                   &  /,17X,'Phase file not convertable!')")     &
                             filext
      CALL exitrc(2)
    ENDIF

    IF (nGPSSAT == 0 .AND. nGLOSAT == 0 .AND. nGALSAT == 0 .AND. &
        nGEOSAT == 0 .AND. nCOMSAT == 0 .AND. nQZSSAT == 0) THEN
      WRITE(lfnerr,"(/,' ### PG ATX2PCV: No satellite PCV ',      &
                   &  'found in ANTEX file.'                      &
                   &  /,17X,'ANTEX file name: ',A40,              &
                   &  /,17X,'No entry in new phase file if no ',  &
                   &        'entry in input phase file!')")       &
                            filext
    ENDIF

! Leading lines for satellite antenna output
    WRITE(lfnprt,"(' Summary of translated patterns for sensors in', &
                 & ' satellite file',/,                              &
                 & ' ---------------------------------------------', &
                 & '----------------',/,                             &
                 & '             SATELLIT.          ',               &
                 & '                     .ATX                   ',   &
                 & 'ANTOFF FRQ 1            ANTOFF FRQ 2',/,         &
                 & ' sensor name         number PRN  SVN <-- ',      &
                 & 'sensor name          PRN  SVN   NORTH   EAST ',  &
                 & '    UP     NORTH   EAST     UP')")
    WRITE(lfnprt,"(' -------------------- ----- --- ----     ',      &
                 & '-------------------- --- ----  ------- -------', &
                 & ' ------- ------- ------- -------',/)")

! Find internal sensor name in satellite info file
! ------------------------------------------------
    forma1 = "(/,' *** PG ATX2PCV: More than one entry', &
             &     ' in ANTEX file for sensor in',       &
             & /,17X,' satellite information file!',     &
             & /,17X,'File not converted!',              &
             & /,17X,'sensor name: ',A)"
    forma2 = "(/,' ### PG ATX2PCV: No entry found in ANTEX file', &
             &   ' for sensor in satellite information file.',    &
             & /,17X,'Sensor name: ',A,                           &
             & /,17X,'Antex name : ',A,                           &
             & /,17X,'PRN number : ',I3,                          &
             & /,17X,'Antenna not included in new phase file ',   &
             &       'if no entry in input phase file!')"

    ALLOCATE(linatx(SIZE(satfil%sensor)),stat=iac)
    CALL alcerr(iac,'linatx',(/SIZE(satfil%sensor)/),'ATX2PCV')
    linatx = 0
    ALLOCATE(linsat(nsat),stat=iac)
    CALL alcerr(iac,'linsat',(/nsat/),'ATX2PCV')
    linsat = 0
    ALLOCATE(dANTOFF(maxelv,maxazi,SIZE(satfil%sensor)),stat=iac)
    CALL alcerr(iac,'dANTOFF',(/maxelv,maxazi,SIZE(satfil%sensor)/),'ATX2PCV')
    dANTOFF = 0d0

    DO esat=1,satfil%nsensor
      IF (satfil%sensor(esat)%sensor(1:12)=='MW TRANSM. B' .OR. &
          satfil%sensor(esat)%sensor(1:12)=='MW TRANSM. G') THEN
        WRITE(lfnerr,"(/,' *** PG ATX2PCV: Satellite information ',     &
                     & 'file with old satellite antenna names.',        &
                     & /,17X,'Satellite file name: ',A40,               &
                     & /,17X,'Please use new satellite file from CODE!',&
                     & /,17X,'Phase file not converted!')")             &
                           filsat
        CALL exitrc(2)
      ENDIF

      epo  = 0.D0
      find = 0

      DO isat=1,nsat
        IF(satfil%sensor(esat)%name == extsaa(isat)%name) THEN

! If SVN is given, check consistency between PRN and time window
          IF (svnatx(isat) /= '    ' .AND. &
              svnatx(isat)(2:4) == satfil%sensor(esat)%sensor(18:20)) THEN
            IF (timint(isat)%t(1) /= 0.0) THEN
              epo = timint(isat)%t(1) ! + timint(isat)%t(2)) / 2
            ENDIF
            CALL svn2prn(0,svnatx(isat),epo,prncheck,timcheck,irc)
            IF (scode(isat) /= 0) THEN
              IF (prncheck /= scode(isat))THEN
                WRITE(lfnerr,"(/,' *** PG ATX2PCV: Given SVN and', &
                             & ' PRN inconsistent in ANTEX file.', &
                             & /,17X,'File not converted!',        &
                             & /,17X,'PRN: ',I3,/,17X,'SVN: ',A4)")&
                                         scode(isat),svnatx(isat)
                CALL exitrc(2)
              ENDIF
!          ELSE
!            scode(isat)=prncheck
            ENDIF
            IF (timint(isat)%t(1) /= 0.0D0 .AND. &
                 satfil%sensor(esat)%timint%t(1) == timint(isat)%t(1)) THEN
              IF (satfil%sensor(esat)%timint%t(2) > timint(isat)%t(2)) THEN
                WRITE(lfnerr,"(/,' *** PG ATX2PCV: Time window ',      &
                             &   'in satellite information file not ', &
                             & /,17X,'equal to those in ANTEX file.',  &
                             & /,17X,'File not converted!',            &
                             & /,17X,'ANTEX sensor name: ',A,I3)")     &
                                    extsaa(isat)%name,prncheck
                CALL exitrc(2)
              ENDIF
            ENDIF
          ENDIF

! keep in mind the corresponding line index of ATX array for sensor in
! satellite info file
          IF (satfil%sensor(esat)%timint%t(1) >= timint(isat)%t(1) .AND. &
              satfil%sensor(esat)%timint%t(2) <= timint(isat)%t(2) .AND. &
              satfil%sensor(esat)%svn == scode(isat)) THEN
            IF (find < 4) THEN
              linatx(esat) = isat
              linsat(isat) = esat
              find = 4
            ELSEIF (find == 4) THEN
              WRITE(lfnerr,forma1) TRIM(satfil%sensor(esat)%sensor)
              CALL exitrc(2)
            ENDIF
          ENDIF
          IF (satfil%sensor(esat)%timint%t(1) >= timint(isat)%t(1) .AND. &
              satfil%sensor(esat)%timint%t(2) <= timint(isat)%t(2)) THEN
            IF (find < 3 .AND. scode(isat) == 0) THEN
              linatx(esat) = isat
              linsat(isat) = esat
              find = 3
            ELSEIF (find == 3 .AND. scode(isat) == 0) THEN
              WRITE(lfnerr,forma1) TRIM(satfil%sensor(esat)%sensor)
              CALL exitrc(2)
            ENDIF
          ENDIF
          IF (satfil%sensor(esat)%svn == scode(isat)) THEN
            IF (find < 2 .AND. svnatx(isat) == '    ' .AND. &
                timint(isat)%t(1) == 0) THEN
              linatx(esat) = isat
              linsat(isat) = esat
              find = 2
            ELSEIF (find == 2 .AND. svnatx(isat) == '    ' .AND. &
                    timint(isat)%t(1) == 0) THEN
              WRITE(lfnerr,forma1) TRIM(satfil%sensor(esat)%sensor)
              CALL exitrc(2)
            ENDIF
          ENDIF
          IF (find == 0 .AND. scode(isat) == 0 .AND.  &
              svnatx(isat)=='   ' .AND. timint(isat)%t(1) == 0) THEN
            linatx(esat) = isat
            linsat(isat) = esat
            find = 1
          ELSEIF (find == 1 .AND. scode(isat) == 0 .AND. &
                  svnatx(isat)=='    ' .AND. timint(isat)%t(1) == 0) THEN
            WRITE(lfnerr,forma1) TRIM(satfil%sensor(esat)%sensor)
            CALL exitrc(2)
          ENDIF
        ENDIF
        IF (isat == nsat .AND. find == 0 .AND.  &
            satfil%sensor(esat)%name /= ' ') THEN
! GPS
          IF ((syst == g_atxsys(0) .OR. syst == g_atxsys(1).OR. &
               syst == g_atxsys(10)) .AND. &
               satfil%sensor(esat)%name(1:LEN(antGPS)) == antGPS .AND. .NOT. &
               (nGPSSAT == 0 .AND. nGLOSAT == 0 .AND. nGALSAT == 0 .AND. &
                nGEOSAT == 0 .AND. nCOMSAT == 0 .AND. nQZSSAT == 0)) THEN
            WRITE(lfnerr,forma2)satfil%sensor(esat)%sensor, &
                  satfil%sensor(esat)%name,satfil%sensor(esat)%svn
!          CALL exitrc(2)
! GLONASS
          ELSEIF ((syst == g_atxsys(2) .OR. syst == g_atxsys(10)) .AND. &
                   satfil%sensor(esat)%name(1:LEN(antGLO)) == antGLO) THEN
            WRITE(lfnerr,forma2)satfil%sensor(esat)%sensor, &
                   satfil%sensor(esat)%name,satfil%sensor(esat)%svn
!          CALL exitrc(2)
! Galileo
          ELSEIF ((syst == g_atxsys(3) .OR. syst == g_atxsys(10)) .AND. &
                   satfil%sensor(esat)%name(1:LEN(antGAL)) == antGAL) THEN
            WRITE(lfnerr,forma2)satfil%sensor(esat)%sensor, &
                 satfil%sensor(esat)%name,satfil%sensor(esat)%svn
!          CALL exitrc(2)
! SBAS
          ELSEIF ((syst == g_atxsys(4) .OR. syst == g_atxsys(10)) .AND. &
                   satfil%sensor(esat)%name(1:LEN(antGEO)) == antGEO) THEN
            WRITE(lfnerr,forma2)satfil%sensor(esat)%sensor, &
                 satfil%sensor(esat)%name,satfil%sensor(esat)%svn
!          CALL exitrc(2)
! Compass
          ELSEIF ((syst == g_atxsys(5) .OR. syst == g_atxsys(10)) .AND. &
                   satfil%sensor(esat)%name(1:LEN(antCOM)) == antCOM) THEN
            WRITE(lfnerr,forma2)satfil%sensor(esat)%sensor, &
                 satfil%sensor(esat)%name,satfil%sensor(esat)%svn
!          CALL exitrc(2)
! QZSS
          ELSEIF ((syst == g_atxsys(6) .OR. syst == g_atxsys(10)) .AND. &
                   satfil%sensor(esat)%name(1:LEN(antQZS)) == antQZS) THEN
            WRITE(lfnerr,forma2)satfil%sensor(esat)%sensor, &
                 satfil%sensor(esat)%name,satfil%sensor(esat)%svn
!          CALL exitrc(2)
          ENDIF
        ENDIF
      ENDDO

! Write output for satellite antennas and calculating the differences to
! offsets in satellite info file
      epo=(satfil%sensor(esat)%timint%t(1)+satfil%sensor(esat)%timint%t(2))/2
      CALL prn2svn(1,satfil%sensor(esat)%svn,epo,svnnr,timcheck,irc)
      IF(linatx(esat) /= 0) THEN
        DO ifrq=1,extsaa(linatx(esat))%sys(0)%nfreq
          DO icor=1,3
            IF(opt%abs2rel == 1) THEN
              extsaa(linatx(esat))%sys(0)%freq(ifrq)%off(0,icor) = &
                satfil%sensor(esat)%antoff(icor)
              extsaa(linatx(esat))%sys(0)%freq(ifrq)%pat(:,:,:) = 0d0
            ENDIF
            dANTOFF(icor,ifrq,esat) = 1.D-15 +  &
                        extsaa(linatx(esat))%sys(0)%freq(ifrq)%off(0,icor) - &
                        satfil%sensor(esat)%antoff(icor) * 1000d0
            IF(ABS(dANTOFF(icor,ifrq,esat)) > 1.D-10) THEN
              WRITE(lfnerr,"(/,' ### PG ATX2PCV: Antenna offsets are ',     &
                           &  'different in satellite info and ANTEX file!',&
                           & /,17X,'Sensor name:     ',A20,                 &
                           & /,17X,'Frequency:       ',I1,                  &
                           & /,17X,'Component:       ',I1,                  &
                           & /,17X,'Difference [mm]: ',F8.2,/)")            &
                satfil%sensor(esat)%sensor,ifrq,icor,dANTOFF(icor,ifrq,esat)
!!              CALL exitrc(2)
              CALL exitrc(2)
            ENDIF
          ENDDO
        ENDDO
        IF(scode(linatx(esat))/=0 .AND. svnatx(linatx(esat))/='    ') THEN
          WRITE(lfnprt,"(' ',A20,I6,I4,1X,A4,5X,A20,I4,1x,A4,1X,6F8.4)")    &
                   satfil%sensor(esat)%sensor,satfil%sensor(esat)%numb,     &
                   satfil%sensor(esat)%svn,svnnr,extsaa(linatx(esat))%name, &
                   scode(linatx(esat)),svnatx(linatx(esat)),                &
                   (dANTOFF(icor,1,esat),icor=1,3),                         &
                   (dANTOFF(icor,2,esat),icor=1,3)
        ELSEIF(scode(linatx(esat)) /= 0 .AND. svnatx(linatx(esat)) == '    ') THEN
          WRITE(lfnprt,"(' ',A20,I6,I4,1X,A4,5X,A20,I4,6X,6F8.4)")          &
                   satfil%sensor(esat)%sensor,satfil%sensor(esat)%numb,     &
                   satfil%sensor(esat)%svn,svnnr,extsaa(linatx(esat))%name, &
                   scode(linatx(esat)),(dANTOFF(icor,1,esat),icor=1,3),     &
                   (dANTOFF(icor,2,esat),icor=1,3)
        ELSE
          WRITE(lfnprt,"(' ',A20,I6,I4,1X,A4,5X,A20,'  generic',1X,6F8.4)") &
                   satfil%sensor(esat)%sensor,satfil%sensor(esat)%numb,     &
                   satfil%sensor(esat)%svn,svnnr,extsaa(linatx(esat))%name, &
                   (dANTOFF(icor,1,esat),icor=1,3),                         &
                   (dANTOFF(icor,2,esat),icor=1,3)
        ENDIF
      ELSEIF(linatx(esat) == 0 .AND. &
           satfil%sensor(esat)%sensor(18:20) /= '   ' .AND. &
           satfil%sensor(esat)%sensor(1:3) == typeMWTR) THEN
        WRITE(lfnprt,"(' ',A20,I6,I4,1X,A4,5X,'no entry in ANTEX!')") &
                satfil%sensor(esat)%sensor,satfil%sensor(esat)%numb,  &
                satfil%sensor(esat)%svn,svnnr
      ENDIF
    ENDDO

! Check for unknown satellite antenna entry in ATX file and write output
! ----------------------------------------------------------------------
    first = .TRUE.
    DO isat=1,nsat
      DO sat=1,satfil%nsensor
        IF (linsat(isat) == sat) EXIT
        IF (sat == satfil%nsensor) THEN
          IF (first) THEN
            first = .FALSE.
            WRITE (lfnprt,"(//,' Sensor patterns in ANTEX file ',              &
                          & 'with no corresponding entry in satellite',        &
                          & ' file:',/,' ------------------------------',      &
                          & '-------------------------------------------',/,   &
                          & ' entry ANTEX sensor name    PRN  SVN',/,' ----- ',&
                          & '-------------------- --- ----',/)")
          ENDIF
          WRITE(lfnerr,"(/,' ### PG ATX2PCV: Sensor pattern in', &
                       &   ' ANTEX file with no corresponding',  &
                       & /,17X,'entry in satellite file.',       &
                       & /,17X,'Entry in ANTEX file: ',I4,       &
                       & /,17X,'ANTEX sensor name: ',A)") isat,extsaa(isat)%name
          IF (scode(isat) /= 0 .AND. svnatx(isat) /= '    ') THEN
            WRITE(string,'(I6,1X,A20,I4,1X,A4)') &
              isat,extsaa(isat)%name,scode(isat),svnatx(isat)
          ELSEIF (scode(isat) /= 0 .AND. svnatx(isat) == '    ') THEN
            WRITE(string,'(I6,1X,A20,I4,5X)')isat,extsaa(isat)%name,scode(isat)
          ELSE
            WRITE(string,"(I6,1X,A20,'  generic')")isat,extsaa(isat)%name
          ENDIF
          WRITE(lfnprt,'(A36)')string
        ENDIF
      ENDDO
    ENDDO
    DEALLOCATE(scode)
    DEALLOCATE(svnatx)
    DEALLOCATE(cospar)
    DEALLOCATE(linsat)
    IF (opt%debug == 1) WRITE(*,*)'Satellite antennas ok'

! Start of Bernese PCV file
! =========================
    IF (filpcv /= ' ') THEN

! Read PHASE file that has to be updated resp. converted
! ------------------------------------------------------
      CALL init_buf(all=1)

! Find AOAD/M_T and do some checks
      ARCV   = 0
      filtyp = 'R'
      DO oant=1,SIZE(recant)
        IF ((recant(oant)%name == 'AOAD/M_T        NONE' .OR. &
             recant(oant)%name == 'AOAD/M_T            ') .AND.  &
            (recant(oant)%numb == 0 .OR. &
             recant(oant)%numb == undef_i)) THEN
          IF (recant(oant)%sys(0)%typ /= 0) THEN
            DO IFRQ=1,2
              ELEV: DO ielv=1,SIZE(recant(oant)%sys(0)%freq(ifrq)%pat,2)
                DO IAZI=1,SIZE(recant(oant)%sys(0)%freq(ifrq)%pat,3)
                  IF (ABS(recant(oant)%sys(0)%freq(ifrq)%pat(0,ielv,iazi)) > 1.D-6) THEN
                    filtyp='A'
                    EXIT ELEV
                  ENDIF
                ENDDO
              ENDDO ELEV
            ENDDO
          ENDIF
          IF (pcvtyp == 'A' .AND. filtyp == 'R' .AND. &
             opt%abs2rel == 0 .AND. opt%convert == 0) THEN
            WRITE(lfnerr,"(/,' *** PG ATX2PCV: Wrong file type', &
                         &   ' of input phase file.',            &
                         & /,17X,'Phase file name: ',A40,        &
                         & /,17X,'Expected type  : ',A1,         &
                         & /,17X,'File not converted!')") filpcv,pcvtyp
            CALL exitrc(2)
          ELSEIF (pcvtyp == 'R' .AND. filtyp == 'A') THEN
            WRITE(lfnerr,"(/,' *** PG ATX2PCV: Wrong file type', &
                         &   ' of input phase file.',            &
                         & /,17X,'Phase file name: ',A40,        &
                         & /,17X,'Expected type  : ',A1,         &
                         & /,17X,'Phase file not merged/updated!')") &
                               filpcv,pcvtyp
            CALL exitrc(2)
          ELSEIF (pcvtyp == 'R' .AND. filtyp == 'R'.AND. &
                 (opt%convert == 1 .OR. opt%abs2rel == 1)) THEN
            WRITE(lfnerr,"(/,' *** PG ATX2PCV: Wrong file type', &
                         &   ' of ANTEX file.',                  &
                         & /,17X,'ANTEX file name: ',A40,        &
                         & /,17X,'Expected type  : A',           &
                         & /,17X,'Program stopped!')") filext
            CALL exitrc(2)
!          convert = 0
          ELSEIF (pcvtyp == 'A' .AND. filtyp == 'A' .AND. opt%convert == 1) THEN
            WRITE(lfnerr,"(/,' ### PG ATX2PCV: Nothing to convert!',  &
                         & /,17X,'Phase file name: ',A40,             &
                         & /,17X,'Expected type  : R',                &
                         & /,17X,'Phase file just merged/updated!')") filpcv
            opt%convert = 0
          ELSEIF (pcvtyp == 'A' .AND. filtyp == 'A' .AND. opt%abs2rel == 1) THEN
            WRITE(lfnerr,"(/,' ### PG ATX2PCV: Wrong file type',  &
                         &   ' of input phase file.',             &
                         & /,17X,'Phase file name: ',A40,         &
                         & /,17X,'Expected type  : R',            &
                         & /,17X,'ANTEX file not converted!')") filpcv
            CALL exitrc(2)
          ELSEIF (pcvtyp == 'A' .AND. filtyp == 'R' .AND. opt%convert == 1 .AND. &
                                                              nGPSSAT == 0) THEN
            WRITE(lfnerr,"(/,' *** PG ATX2PCV: No GPS satellite PCV', &
                         &   ' included in ANTEX file.',              &
                         & /,17X,'ANTEX file name: ',A40,             &
                         & /,17X,'Phase file not converted!')") filext
            CALL exitrc(2)
          ENDIF
          ARCV = oant
        ELSEIF (recant(oant)%name == '' .AND. opt%convert == 1 .AND. ARCV == 0 ) THEN
          WRITE(lfnerr,"(/,' *** PG ATX2PCV: Antenna [AOAD/M_T    ',  &
                       &   '   NONE] not found in input phase file.', &
                       & /,17X,'Phase file name: ',A40,               &
                       & /,17X,'Phase file not convertable!')") filpcv
          CALL exitrc(2)
        ENDIF
      ENDDO
      DO oant=1,SIZE(satant)
        IF (satant(oant)%name == '') EXIT
        IF ((satant(oant)%name(1:12) == 'MW TRANSM. B' .AND. nGPSSAT == 0) .OR. &
                (satant(oant)%name(1:12) == 'MW TRANSM. G' .AND. nGLOSAT == 0)) THEN
          WRITE(lfnerr,"(/,' *** PG ATX2PCV: No(t all) satellite ',      &
                       &   'antenna names in ANTEX file and ',           &
                       & /,17X,'old satellite antenna names in input ',  &
                       &   'phase file.',                                &
                       & /,17X,'Phase file name: ',A40,                  &
                       & /,17X,'Please use new phase file and corresp',  &
                       &   'onding satellite info file',/,17X,'OR',      &
                       & /,17X,'update your phase file first with a ',   &
                       &   'new satellite info file from CODE',          &
                       & /,17X,'and a ANTEX file including satellites!', &
                       & /,17X,'Phase file not converted!')") filpcv
          CALL exitrc(2)
        ENDIF
      ENDDO

! Convert abs to rel if requested and save index of AOAD/M_T NONE from ANTEX array
! --------------------------------------------------------------------------------
      IF (opt%abs2rel == 1 ) THEN
        forget = 0
        DO irec=1,nrec
          IF (irec == AOADMT) THEN
            forget = irec
            CYCLE
          ENDIF
          DO ifrq=1,extrea(irec)%sys(0)%nfreq
            DO icor=1,3
              extrea(irec)%sys(0)%freq(ifrq)%off(0,icor) = &
                  (extrea(irec)%sys(0)%freq(ifrq)%off(0,icor) * &
                   extrea(irec)%sys(0)%freq(ifrq)%fac(0) - &
                   extrea(aoadmt)%sys(0)%freq(ifrq)%off(0,icor) * &
                   extrea(aoadmt)%sys(0)%freq(ifrq)%fac(0) + &
                   recant(ARCV)%sys(0)%freq(ifrq)%off(0,icor) * &
                   recant(ARCV)%sys(0)%freq(ifrq)%fac(0)) / &
                   extrea(irec)%sys(0)%freq(ifrq)%fac(0)
            ENDDO
            DO ielv=1,SIZE(extrea(irec)%sys(0)%freq(ifrq)%pat,2)
              DO iazi=1,SIZE(extrea(irec)%sys(0)%freq(ifrq)%pat,3)
                IF (SIZE(extrea(irec)%sys(0)%freq(ifrq)%pat,3) == 1) THEN
                  extrea(irec)%sys(0)%freq(ifrq)%pat(0,ielv,iazi) = &
                      (extrea(irec)%sys(0)%freq(ifrq)%pat(0,ielv,iazi) * &
                       extrea(irec)%sys(0)%freq(ifrq)%fac(0) - &
                       AOAPCV(ielv,ifrq) * &
                       extrea(aoadmt)%sys(0)%freq(ifrq)%fac(0)) / &
                       extrea(irec)%sys(0)%freq(ifrq)%fac(0)
                ELSE
                  extrea(irec)%sys(0)%freq(ifrq)%pat(0,ielv,iazi) = &
                      (extrea(irec)%sys(0)%freq(ifrq)%pat(0,ielv,iazi) * &
                       extrea(irec)%sys(0)%freq(ifrq)%fac(0) - &
                       extrea(aoadmt)%sys(0)%freq(ifrq)%pat(0,ielv,iazi) * &
                       extrea(aoadmt)%sys(0)%freq(ifrq)%fac(0)) / &
                       extrea(irec)%sys(0)%freq(ifrq)%fac(0)
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF

! Find corresponding antenna in satellite info file and save index
! for satellite antennas in input phase file
      first = .true.
      ALLOCATE(linsat(SIZE(satant)),stat=iac)
      CALL alcerr(iac,'linsat',(/SIZE(satant)/),'ATX2PCV')
      linsat = 0

      DO oant=1,SIZE(satant)
        IF (satant(oant)%name == '') EXIT
        svnnr  = ' '
        prn    = 0
        READ(satant(oant)%name(18:20),'(i3)',iostat=ios) svnr
        IF (ios == 0 .AND. svnr /= 0) THEN
          IF     (svnr > 700 .AND. svnr < 800) svnr = svnr - 600
          CALL svn2chr(svnr,help,syst)
          IF (satant(oant)%name(1:11)=='MW TRANSM I')THEN
            WRITE(svnnr,'(A1,A3)')syst,satant(oant)%name(18:20)
          ELSEIF (satant(oant)%name(1:11)=='MW TRANSM S')THEN
            WRITE(svnnr,'(A1,A3)')syst,satant(oant)%name(18:20)
          ELSEIF (satant(oant)%name(1:11)=='MW TRANSM O')THEN
            WRITE(svnnr,"('E',A3)")satant(oant)%name(18:20)
          ENDIF
          DO isat=1,satfil%nsensor
            IF (satant(oant)%name == satfil%sensor(isat)%sensor)THEN
              linsat(oant) = isat
              EXIT
            ENDIF
          ENDDO
          IF(linsat(oant) == 0 .AND. svnnr /= ' ') THEN
            CALL svn2prn(4,svnnr,0d0,prn,timcheck,irc)
            IF (irc == 6) prn = 0
            DO isat=1,satfil%nsensor
              IF (satfil%sensor(isat)%svn == prn .AND.  &
                  satfil%sensor(isat)%type == typeMWTR .AND. &
                  timcheck%t(1) == satfil%sensor(isat)%timint%t(1)) THEN
                linsat(oant) = isat
              ENDIF
            ENDDO
            IF (linsat(oant) /= 0) THEN
              IF (first) THEN
                first = .false.
                WRITE (lfnprt,"(//,' Translated satellite antenna ',        &
                              &    'names from input phase file:',          &
                              &  /,' ------------------------------------', &
                              &    '---------------------',                 &
                              & //,' Old Antenna name     translated to:',  &
                              &  /,' --------------------')")
              ENDIF
              WRITE(LFNPRT,"(1X,A20,' --> ',A20)")satant(oant)%name, &
                       satfil%sensor(linsat(oant))%sensor
            ENDIF
          ENDIF
          IF (linsat(oant) == 0) THEN
            WRITE(lfnerr,"(/,' ### PG ATX2PCV: Antenna from input phase', &
                         &   ' file not found in satellite file.',        &
                         & /,17X,'Antenna name: ',A20,                    &
                         & /,17X,'Antenna can not be translated and ',    &
                         &   'will be not included',/,17X,'in new ',      &
                         &   'phase file!')")satant(oant)%name
            linsat(oant) = -1
          ENDIF
        ENDIF
      ENDDO
      DEALLOCATE(timint)

! Loop over all receiver antennas in recant (old PCV file) and comparison with extrea (ANTEX file)
      first = .true.
      antcnt= 0
      antcnt2= 0
      nrec1 = nrec
      OLDREC: DO oant=1,SIZE(recant)
        IF (recant(oant)%name == '') EXIT
        IF (first) THEN
          first=.FALSE.
          WRITE (lfnprt,"(//,' Values for receiver antennas and ant',   &
                        &    'ennas that are not in ANTEX taken from:', &
                        &  /,' -------------------------------------',  &
                        &    '--------------------------------------',  &
                        & //,' Antenna name         number         ',   &
                        &    '                                     ',   &
                        &    '        ',                                &
                        &    ' Differences: ele/azi offsets pattern',   &
                        &  /,' -------------------- ------')")
        ENDIF

        IF (recant(oant)%numb == 0 .AND. opt%antnum == 1) &
          recant(oant)%numb = undef_i
        IF (recant(oant)%numb == undef_i .AND. opt%antnum == 0) &
          recant(oant)%numb = 0
        IF (recant(oant)%name(17:20) == '    ' .AND. isys == 0) THEN
          antcnt = antcnt + 1
          IF (opt%radcod == 1 .AND. recant(oant)%name(1:5) /= 'SLR  ') &
            recant(oant)%name(17:20) = 'NONE'
        ENDIF
! If similar entries found in both files check values, method and date
        DO irec=1,nrec
          IF (irec == forget) CYCLE
          IF (recant(oant)%numb == extrea(irec)%numb .AND. &
              recant(oant)%name == extrea(irec)%name) THEN

! Loop over all systems
            DO isys=0,maxsys-1
              IF (recant(oant)%sys(isys)%nfreq == 0 .OR. &
                  extrea(irec)%sys(isys)%nfreq == 0) CYCLE
              IF (recant(oant)%sys(isys)%typ == 0) THEN
                oldelv = 0
                oldazi = 0
              ELSEIF (recant(oant)%sys(isys)%typ == 1) THEN
                IF (recant(oant)%sys(isys)%resolu(2) == recant(oant)%sys(isys)%resolu(4)) THEN
                  oldelv = 1
                ELSE
                  oldelv = recant(oant)%sys(isys)%resolu(4) / recant(oant)%sys(isys)%resolu(2) + 1
                ENDIF
                IF (recant(oant)%sys(isys)%resolu(3) == 360) THEN
                  oldazi = 1
                ELSE
                  oldazi = 360 / recant(oant)%sys(isys)%resolu(3) + 1
                ENDIF
              ELSEIF (recant(oant)%sys(isys)%typ >= 2 .AND. recant(oant)%sys(isys)%typ <= 4) THEN
                oldelv = recant(oant)%sys(isys)%resolu(2)
                oldazi = 2 * recant(oant)%sys(isys)%resolu(3) + 1
              ENDIF

              MM = 0
              READ(recant(oant)%sys(isys)%date,'(I2,5X,I2)',iostat=ios) DD,YYYY
              IF (ios == 0) THEN
                YYYY = IYEAR4(YYYY)
                CALL UPPERC(recant(oant)%sys(isys)%date)
                MM = LISTC1(0,3,12,Month,recant(oant)%sys(isys)%date(4:6),12)
              ENDIF
              IF (MM /= 0) THEN
                omjd = djul(YYYY,MM,DD*1.D0)
              ELSE
                READ(recant(oant)%sys(isys)%date,'(I4,1X,I2,1X,I2)',iostat=ios) YYYY,MM,DD
                IF (ios == 0 .AND. MM /= 0) THEN
                  omjd = djul(YYYY,MM,DD*1.D0)
                ELSE
                  omjd = 0
                  IF(recant(oant)%sys(isys)%date(4:6) /= '   ')THEN
                    WRITE(lfnerr,"(/,' ### PG ATX2PCV: Unknown date',    &
                                 &   ' format in input phase file.',     &
                                 & /,17X,'Antenna: ',A20,                &
                                 & /,17X,'System : ',A1,                 &
                                 & /,17X,'Date   : ',A10,                &
                                 & /,17X,'Date replaced by zero!',/)")   &
                       recant(oant)%name,g_svnsys(isys),recant(oant)%sys(isys)%date
                  ENDIF
                ENDIF
              ENDIF
              IF (idate == 1) WRITE(recant(oant)%sys(isys)%date,"(I2.2,'-',A3,'-',I2.2,1X)") DD,month(MM),MOD(YYYY,100)
              IF (idate == 2) WRITE(recant(oant)%sys(isys)%date,"(I4.4,'-',I2.2,'-',I2.2)") YYYY,MM,DD
              oiMETH = LISTC1(0,5,7,Method,recant(oant)%sys(isys)%method(1:5),7)
              IF (oiMETH == 0) THEN
                WRITE(lfnerr,"(/,' *** PG ATX2PCV: Unknown method found in PCV file', &
                & /,17X,'Method       : ',A,                                          &
                & /,17X,'PCV file name: ',A,/)") TRIM(recant(oant)%sys(isys)%method),TRIM(filpcv)
                CALL EXITRC(2)
              ENDIF

              IF (extrea(irec)%sys(isys)%typ == 0) THEN
                extelv = 0
                extazi = 0
              ELSEIF (extrea(irec)%sys(isys)%typ == 1) THEN
                IF (extrea(irec)%sys(isys)%resolu(2) == extrea(irec)%sys(isys)%resolu(4)) THEN
                  extelv = 1
                ELSE
                  extelv = extrea(irec)%sys(isys)%resolu(4) / extrea(irec)%sys(isys)%resolu(2) + 1
                ENDIF
                IF (extrea(irec)%sys(isys)%resolu(3) == 360) THEN
                  extazi = 1
                ELSE
                  extazi = 360 / extrea(irec)%sys(isys)%resolu(3) + 1
                ENDIF
              ENDIF
              add = 0
              iMETH = LISTC1(0,5,7,Method,extrea(irec)%sys(isys)%method(1:5),7)
              IF (iMETH == 0) THEN
                WRITE(lfnerr,"(/,' *** PG ATX2PCV: Unknown method found in ATX file', &
                & /,17X,'Method       : ',A,                                          &
                & /,17X,'ATX file name: ',A,/)") TRIM(extrea(irec)%sys(isys)%method),TRIM(filext)
                CALL EXITRC(2)
              ENDIF

! Check if same values in ANTEX and input PHASE file
! --------------------------------------------------
              IF (opt%convert == 0 .AND. isys == 0) THEN
                difnpcv = ' no '
                diffoff = ' no '
                diffpcv = ' no '
! different offset values?
                DO ifrq=1, recant(oant)%sys(isys)%nfreq
                  DO icor=1,3
                    help1 = &
                      NINT(recant(oant)%sys(isys)%freq(ifrq)%off(0,ICOR) - &
                      ANINT(extrea(irec)%sys(isys)%freq(ifrq)%off(0,ICOR)*10000d0)/10000d0)
                    IF (ABS(help1) > 1.D-6) diffoff = 'yes '
                  ENDDO
                ENDDO
! different number of elevation/azimut values?
                IF(oldelv == 0) THEN
                  DO ifrq=1,extrea(irec)%sys(isys)%nfreq
                    DO ielv=1,extelv
                      DO iazi=1,extazi
                        IF (ABS(extrea(irec)%sys(isys)%freq(ifrq)%pat(0,ielv,iazi)) > 1.D-7) THEN
                          difnpcv='n.a.'
                          diffpcv='yes '
                        ENDIF
                      ENDDO
                    ENDDO
                  ENDDO
                ELSE
                  IF (oldelv < extelv .OR. oldazi /= extazi) THEN
                    difnpcv = 'yes '
                  ELSE
! different PCV values?
                    DO ifrq=1,extrea(irec)%sys(isys)%nfreq
                      DO ielv=1,extelv
                        DO iazi=1,extazi
                          help1 = &
                            NINT(recant(oant)%sys(isys)%freq(ifrq)%pat(0,ielv,iazi) - &
                            extrea(irec)%sys(isys)%freq(ifrq)%pat(0,ielv,iazi))
                          IF (ABS(help1) > 1.D-7) diffpcv = 'yes '
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDIF
                ENDIF
                IF (diffpcv == 'yes ' .OR. difnpcv == 'yes '.OR. &
                     diffoff == 'yes ') antcnt2 = antcnt2 + 1
              ELSE
                difnpcv = 'n.a.'
                diffoff = 'n.a.'
                diffpcv = 'n.a.'
              ENDIF
! Check date in ANTEX
! -------------------
              IF (idate == 1) THEN
                READ(extrea(irec)%sys(isys)%date,'(I2,5X,I2)') DD,YYYY
                YYYY = IYEAR4(YYYY)
                MM = LISTC1(0,3,12,Month,recant(oant)%sys(isys)%date(4:6),12)
              ELSE
                READ(extrea(irec)%sys(isys)%date,'(I4,1X,I2,1X,I2)')  YYYY,MM,DD
              ENDIF
              amjd = djul(YYYY,MM,DD*1.D0)

! Take values from input Bernese PCV file
              IF (((omjd > amjd .AND. oiMETH == iMETH) .OR. &
                   oiMETH > iMETH) .AND. opt%convert /= 1 .AND. filrcv == '') THEN
                extrea(irec)%sys(isys)%typ    = recant(oant)%sys(isys)%typ
                extrea(irec)%sys(isys)%resolu = recant(oant)%sys(isys)%resolu
                extrea(irec)%sys(isys)%sinex  = recant(oant)%sys(isys)%sinex
                extrea(irec)%sys(isys)%method = recant(oant)%sys(isys)%method
                extrea(irec)%sys(isys)%date   = recant(oant)%sys(isys)%date
                extrea(irec)%sys(isys)%nfreq  = recant(oant)%sys(isys)%nfreq
                CALL alcfrq(isys,recant(oant)%sys(isys)%nfreq,recant(oant)%sys(isys)%typ, &
                            recant(oant)%sys(isys)%resolu,extrea,irec)
                DO ifrq=1,recant(oant)%sys(isys)%nfreq
                  DO icor=1,3
                    extrea(irec)%sys(isys)%freq(ifrq)%off(0,icor) = &
                         recant(oant)%sys(isys)%freq(ifrq)%off(0,icor)
                  ENDDO
                ENDDO
                IF (recant(oant)%sys(isys)%typ /= 0) THEN
                  DO ifrq=1,recant(oant)%sys(isys)%nfreq
                    DO ielv=1,oldelv
                      DO iazi=1,oldazi
                        extrea(irec)%sys(isys)%freq(ifrq)%pat(0,ielv,iazi) = &
                           recant(oant)%sys(isys)%freq(ifrq)%pat(0,ielv,iazi)
                      ENDDO
                    ENDDO
                  ENDDO
                ENDIF
                WRITE(lfnprt,"(1X,A20,I7,1X,A1,' from input PHASE file ', &
                     & '(newer date and/or newer calibration method)',    &
                     & 3X,A4,2(4x,A4))") extrea(irec)%name, &
                          extrea(irec)%numb,g_svnsys(isys), &
                          difnpcv,diffoff,diffpcv
! Take values from ANTEX file
              ELSE
                IF (omjd == amjd .AND. oiMETH <= iMETH .AND. iMETH > 1) THEN
                  WRITE(lfnprt,"(1X,A20,I7,1X,A1,' from ANTEX file ', &
                       & '(same date and same or newer calibration',  &
                       & ' method) ',3(4X,A4))")extrea(irec)%name,extrea(irec)%numb,g_svnsys(isys), &
                       difnpcv,diffoff,diffpcv
                ELSEIF (omjd < amjd .AND. oiMETH <= iMETH .AND. iMETH > 1) THEN
                  WRITE(lfnprt,"(1X,A20,I7,1X,A1,' from ANTEX file ', &
                       & '(newer date and same or newer calibration', &
                       & ' method)',3(4X,A4))")extrea(irec)%name,extrea(irec)%numb,g_svnsys(isys), &
                       difnpcv,diffoff,diffpcv
                ELSEIF (oiMETH <= iMETH .AND. iMETH > 1) THEN
                  WRITE(lfnprt,"(1X,A20,I7,1X,A1,' from ANTEX file ', &
                       & '(older date but same or newer calibration', &
                       & ' method)',3(4X,A4))")extrea(irec)%name,extrea(irec)%numb,g_svnsys(isys), &
                       difnpcv,diffoff,diffpcv
                ELSEIF (oiMETH > 2) THEN
                  WRITE(lfnerr,"(/,' ### PG ATX2PCV: Downgraded calibration values for:', &
                               & /,17X,'Antenna: ',A20,' Number: ',I6,  &
                               & /,17X,'System : ',A1,                  &
                               & /,17X,'Old method: ',A20,              &
                               & /,17X,'New method: ',A20,              &
                               & /,17X,'Old date: ',A10,                &
                               & /,17X,'New date: ',A10,/)") extrea(irec)%name,extrea(irec)%numb, &
                      g_svnsys(isys),recant(oant)%sys(isys)%method,extrea(irec)%sys(isys)%method, &
                                     recant(oant)%sys(isys)%date,extrea(irec)%sys(isys)%date
                ENDIF
              ENDIF
            ENDDO ! loop over systems
            CYCLE OLDREC

! Add into list if no entry in ANTEX file
! ---------------------------------------
          ELSEIF (irec == nrec) THEN
            first2 = .true.
            DO isys=0,maxsys-1
              IF (recant(oant)%sys(isys)%nfreq == 0 .OR. &
                  recant(oant)%sys(isys)%method(1:7) == "ADOPTED") CYCLE
              IF (first2) THEN
                first2 = .false.
                nrec1 = nrec1 + 1
                IF (nrec1 > maxrec) THEN
                  WRITE(lfnerr,"(/,' *** PG ATX2PCV: Too many receiver antennas', &
                               & /,17X,'Increase MAXREC',/)")
                  CALL exitrc(2)
                ENDIF
              ENDIF
              extrea(nrec1)%name = recant(oant)%name
              extrea(nrec1)%numb = recant(oant)%numb
              CALL addant(2,recant,oant,isys,oldelv,oldazi,aoadmt,ARCV,AOAPCV,extrea,nrec1)
            ENDDO
          ENDIF
        ENDDO ! loop over extrea
      ENDDO OLDREC

! for satellite antennas
! ----------------------
      nsat1 = nsat
      OLDSAT: DO oant=1,SIZE(satant)
        IF (satant(oant)%name == '') EXIT
        add = 0
        IF (linsat(oant) == 0 .AND. &
           (extsaa(oant)%name(1:6) /= 'MW  BL' .AND. &
            extsaa(oant)%name(1:6) /= 'MW  GL' .AND. &
            extsaa(oant)%name(1:6) /= 'MW  GA' .AND. &
            extsaa(oant)%name(1:6) /= 'MW  SB' .AND. &
            extsaa(oant)%name(1:6) /= 'MW  CO' .AND. &
            extsaa(oant)%name(1:6) /= 'MW  QZ')) THEN
          add = 1
        ELSEIF(linsat(oant) > 0) THEN
          IF ((satfil%sensor(linsat(oant))%name(1:LEN(antGPS)) == antGPS .AND. &
                nGPSSAT == 0) .OR. &
              (satfil%sensor(linsat(oant))%name(1:LEN(antGLO)) == antGLO .AND. &
                nGLOSAT == 0) .OR. &
              (satfil%sensor(linsat(oant))%name(1:LEN(antGAL)) == antGAL .AND. &
                nGALSAT == 0) .OR. &
              (satfil%sensor(linsat(oant))%name(1:LEN(antGEO)) == antGEO .AND. &
                nGEOSAT == 0) .OR. &
              (satfil%sensor(linsat(oant))%name(1:LEN(antCOM)) == antCOM .AND. &
                nCOMSAT == 0) .OR. &
              (satfil%sensor(linsat(oant))%name(1:LEN(antQZS)) == antQZS .AND. &
                nQZSSAT == 0)) THEN
            add = 1
          ENDIF
        ELSE
          CYCLE
        ENDIF
        IF (add == 1) THEN
          IF (satant(oant)%sys(0)%typ == 0) THEN
            oldelv = 0
            oldazi = 0
          ELSEIF (satant(oant)%sys(0)%typ == 1) THEN
            oldelv = satant(oant)%sys(0)%resolu(4) / satant(oant)%sys(0)%resolu(2) + 1
            oldazi = 360 / satant(oant)%sys(0)%resolu(3) + 1
          ELSEIF (satant(oant)%sys(0)%typ >= 2 .AND. satant(oant)%sys(0)%typ <= 4) THEN
            oldelv = satant(oant)%sys(0)%resolu(2)
            oldazi = 2 * satant(oant)%sys(0)%resolu(3) + 1
          ENDIF
          nsat1 = nsat1 + 1
          IF (nsat1 > maxsaa) THEN
            WRITE(lfnerr,"(/,' *** PG ATX2PCV: Too many satellite antennas', &
                         & /,17X,'Increase MAXREC',/)")
            CALL exitrc(2)
          ENDIF
          extsaa(nsat1)%name = satant(oant)%name
          extsaa(nsat1)%numb = satant(oant)%numb
          CALL addant(add,satant,oant,0,oldelv,oldazi,aoadmt,ARCV,AOAPCV,extsaa,nsat1)
        ENDIF
      ENDDO OLDSAT
      DEALLOCATE(linsat)

      IF (antcnt /= 0 .AND. opt%radcod == 1) THEN
        WRITE(LFNERR,"(/,' ### PG ATX2PCV: ',i3,' receiver antenna(s)', &
                     &   ' without radome code in input ',              &
                     & /,17X,'Bernese PCV file.',                       &
                     & /,17X,'Antenna radome set to NONE!',             &
                     & /,17X,'Check output and result file!',/)")antcnt
      ELSEIF (antcnt /= 0 .AND. opt%radcod /= 1) THEN
        WRITE(LFNERR,"(/,' ### PG ATX2PCV: ',i3,' receiver antenna(s)', &
                     &   ' without radome code in input ',              &
                     & /,17X,'Bernese PCV file.',                       &
                     & /,17X,'Antenna(s) not included in new Phase file!',/)")antcnt
      ENDIF
      IF (antcnt2 /= 0 .AND. opt%onlyele == 1) THEN
        WRITE(LFNERR,"(/,' ### PG ATX2PCV: ',i3,' receiver antenna(s)', &
                     &   ' with different values in ANTEX ',            &
                     & /,17X,'and input Bernese PCV file.',             &
                     & /,17X,'This may caused by the option writing ',  &
                     &   'only elevation dependent pattern',/)")antcnt2
      ELSEIF (antcnt2 /= 0) THEN
        WRITE(LFNERR,"(/,' ### PG ATX2PCV: ',i3,' receiver antenna(s)', &
                     &  ' with different values in ANTEX ',             &
                     & /,17X,'and input Bernese PCV file.',             &
                     & /,17X,'Check output and result file!',/)")antcnt2
      ENDIF

! Write output for entries in ANTEX file with no entry in input PHASE file
! ------------------------------------------------------------------------
      DO irec=1,nrec
        IF (irec == forget) CYCLE
        DO oant=1,SIZE(recant)
          IF (recant(oant)%name == '') EXIT
          IF (recant(oant)%name == extrea(irec)%name .AND. recant(oant)%numb == extrea(irec)%numb) EXIT
          IF (oant == SIZE(recant)) THEN
            WRITE(lfnprt,"(1X,A20,I7,' from ANTEX file (no entry in input phase file)')") &
                 extrea(irec)%name,recant(irec)%numb
          ENDIF
        ENDDO
      ENDDO

      DEALLOCATE(satant)
      DEALLOCATE(recant)
    ENDIF
    IF (opt%debug == 1) WRITE(*,*)'Bernese PCV input ok'
! =======================================
! End of input Bernese PCV file

! Write output for translated receiver antenna numbers of receiver antennas
    WRITE(lfnprt,"(/,/,' Summary of translated receiver antenna numbers', &
                 &   /,' ---------------------------------------------',  &
                 &   /,' Receiver antenna name read number          ',    &
                 &     '--> translated number',                           &
                 &   /,' -------------------- --------------------',      &
                 &     '          ------',/)")
    DO irec=1,nrec
      WRITE(lfnprt,'(2(1X,A20),10X,I6)')extrea(irec)%name,outant(irec), &
                                                          extrea(irec)%numb
    ENDDO
    DEALLOCATE(outant)

    IF (nrec1 == 0) nrec1 = nrec
    IF (nsat1 == 0) nsat1 = nsat

! Enlarge phase file with antennas not included in ANTEX from STACRX
! ------------------------------------------------------------------
    IF (filsta /= ' ') THEN
      CALL readcrux(filsta,stacrux)
      count = 0
      DO icrx = 1,stacrux%ninfo
        IF (stacrux%stainfo(icrx)%antnam(17:20) == 'NONE') CYCLE
        count = count+1
        DO irec = 1,icrx-1
          IF (stacrux%stainfo(irec)%antnam(17:20) == 'NONE') CYCLE
          IF (stacrux%stainfo(icrx)%antnam == stacrux%stainfo(irec)%antnam) THEN
            count=count-1
            EXIT
          ENDIF
        ENDDO
      ENDDO
      ALLOCATE(noneant(count),stat=iac)
      CALL alcerr(iac,'noneant',(/count/),'ANTCNV')
      noneant = ''
      idome = 0
      count = 0
      DO icrx=1,stacrux%ninfo
        found=0
        IF (stacrux%stainfo(icrx)%antnam(17:20) == '    ') THEN
          idome = idome + 1
          CYCLE
        ENDIF
        DO irec=1,nrec1
          IF (stacrux%stainfo(icrx)%antnam == extrea(irec)%name .AND. &
               (extrea(irec)%numb == 0 .OR. &
                extrea(irec)%numb == undef_i)) EXIT
          IF(stacrux%stainfo(icrx)%antnam(1:16) == extrea(irec)%name(1:16).AND.&
              extrea(irec)%name(17:20) == 'NONE' .AND. &
                (extrea(irec)%numb == 0 .OR. &
                 extrea(irec)%numb == undef_i)) found = 1
          IF (irec == nrec1 .AND. found == 1) THEN
            count = count + 1
            DO ii=1,count
              IF (NONEANT(ii) == stacrux%stainfo(icrx)%antnam) THEN
                count = count - 1
                EXIT
              ELSEIF (ii == count) THEN
                NONEANT(count) = stacrux%stainfo(icrx)%antnam
             ENDIF
            ENDDO
          ELSEIF (irec == nrec1 .AND. found == 0 .AND. &
                  stacrux%stainfo(icrx)%antnam /= undef_c) THEN
            WRITE(lfnerr,"(/,' ### PG ATX2PCV: No corresponding NONE ',     &
                         &   'antenna found in ANTEX file',                 &
                         & /,17X,'for antenna in station information file.',&
                         & /,17X,'Antenna name: ',A)") &
                         TRIM(stacrux%stainfo(icrx)%antnam)
            EXIT
          ENDIF
        ENDDO
      ENDDO
      IF (idome /= 0) THEN
        WRITE(lfnerr,"(/,' ### PG ATX2PCV: ',i3,' receiver antenna(s)', &
                     &   ' without radome code in station ',            &
                     & /,17X,'information file.',                       &
                     & /,17X,'Antenna(s) not included in new ',         &
                     &   'phase file!')")idome
      ENDIF
    ENDIF
    IF (opt%debug == 1) WRITE(*,*)'STACRX input ok'
! End of STACRX

! Fill patterns, if desired
! -------------------------
    IF (opt%atxfil == 1) THEN

      forma3 = "(/,' *** PG ATX2PCV: Too many different',  &
               &   ' zenith/nadir angles due to filling.', &
               & /,17X,'File not converted!',              &
               & /,17X,'File name         : ',A32,         &
               & /,17X,'Antenna type      : ',A20,         &
               & /,17X,'# of zenith angles:',I3,           &
               & /,17X,'Max # allowed     :',I3,           &
               & /,17X,'Increase maxelv!',/)"

      IF (opt%fizmod == 3 .AND. AOADMT == 0) THEN
        WRITE(lfnerr,"(/,' ### PG ATX2PCV: Antenna AOAD/M_T not available.', &
                     & /,17X,'Patterns filled with zeros.',/)")
        opt%fizmod = 1
      ENDIF

! Loop over all receiver antennas
      DO irec=1,nrec1
        IF (irec == forget) CYCLE
        DO isys=0,maxsys-1
          IF (extrea(irec)%sys(isys)%nfreq == 0) CYCLE
          NFPCV = 0
          IF (extrea(irec)%sys(isys)%typ == 1) THEN
            IF (extrea(irec)%sys(isys)%resolu(2) == extrea(irec)%sys(isys)%resolu(4)) THEN
              nelv = 1
            ELSE
              nelv  = extrea(irec)%sys(isys)%resolu(4) / extrea(irec)%sys(isys)%resolu(2) + 1
            ENDIF
            IF (extrea(irec)%sys(isys)%resolu(3) == 360) THEN
              nazi = 1
            ELSE
              nazi  = 360 / extrea(irec)%sys(isys)%resolu(3) + 1
            ENDIF
          ELSE
            CYCLE
          ENDIF

          IF (extrea(irec)%sys(isys)%resolu(4) >= opt%mxfzen) THEN
            CYCLE
          ELSE
            NFPCV=IDNINT(DBLE(opt%mxfzen - extrea(irec)%sys(isys)%resolu(4)) / &
                 extrea(irec)%sys(isys)%resolu(2))
            IF (NFPCV < 1) CYCLE

            ALLOCATE(hlppcv(0:extrea(irec)%sys(isys)%resolu(1),nelv,nazi),stat=iac)
            CALL alcerr(iac,'hlppcv',(/extrea(irec)%sys(isys)%resolu(1)+1,nelv,nazi/),'ATX2PCV')

            IF ((nelv+NFPCV) > MAXELV) THEN
              WRITE(lfnerr,forma3) filext,extrea(irec)%name,(nelv+NFPCV),MAXELV
              CALL exitrc(2)
            ENDIF

            DO IFRQ=1,extrea(irec)%sys(isys)%nfreq
              hlppcv = extrea(irec)%sys(isys)%freq(ifrq)%pat
              DEALLOCATE(extrea(irec)%sys(isys)%freq(ifrq)%pat)
              ALLOCATE(extrea(irec)%sys(isys)%freq(ifrq)%pat(0:extrea(irec)%sys(isys)%resolu(1), &
                   nelv+NFPCV,nazi),stat=iac)
              CALL alcerr(iac,'extrea(irec)%sys(isys)%freq(ifrq)%pat',(/extrea(irec)%sys(isys)%resolu(1),&
                   nelv+NFPCV,nazi/),'ATX2PCV')
              DO iord=0,extrea(irec)%sys(isys)%resolu(1)
                DO IAZI=1,nazi
                  DO IELV=1,nelv
                    extrea(irec)%sys(isys)%freq(ifrq)%pat(iord,ielv,iazi) = hlppcv(iord,ielv,iazi)
                  ENDDO
                ENDDO
              ENDDO
              DO iord=0,extrea(irec)%sys(isys)%resolu(1)
                DO IAZI=1,nazi
                  DO IELV=nelv+1,nelv+NFPCV
                    IF (opt%fizmod == 1) THEN
                      extrea(irec)%sys(isys)%freq(IFRQ)%pat(iord,IELV,IAZI) = 0.D0
                    ELSEIF (opt%fizmod == 2) THEN
                      extrea(irec)%sys(isys)%freq(IFRQ)%pat(iord,IELV,IAZI) = &
                           extrea(irec)%sys(isys)%freq(IFRQ)%pat(iord,nelv,IAZI)
                    ELSEIF (opt%fizmod == 3) THEN
                      IF (nazi == 1) THEN
                        extrea(irec)%sys(isys)%freq(IFRQ)%pat(0,IELV,IAZI) = AOAPCV(IELV,IFRQ)
                      ELSE
                        extrea(irec)%sys(isys)%freq(IFRQ)%pat(0,IELV,IAZI) = &
                             extrea(aoadmt)%sys(isys)%freq(IFRQ)%pat(0,IELV,IAZI)
                      ENDIF
                    ENDIF
                  ENDDO
                ENDDO
              ENDDO
            ENDDO
            extrea(irec)%sys(isys)%resolu(4) = opt%mxfzen
            DEALLOCATE(hlppcv)
          ENDIF
        ENDDO
      ENDDO

! Loop over all satellite antennas
      DO isat=1,nsat1
        NFPCV = 0
        IF (extsaa(isat)%sys(0)%typ == 1) THEN
          IF (extsaa(isat)%sys(0)%resolu(2) == extsaa(isat)%sys(0)%resolu(4)) THEN
            nelv = 1
          ELSE
            nelv  = extsaa(isat)%sys(0)%resolu(4) / extsaa(isat)%sys(0)%resolu(2) + 1
          ENDIF
          IF (extsaa(isat)%sys(0)%resolu(3) == 360) THEN
            nazi = 1
          ELSE
            nazi  = 360 / extsaa(isat)%sys(0)%resolu(3) + 1
          ENDIF
        ELSE
          CYCLE
        ENDIF
        IF (extsaa(isat)%sys(0)%resolu(4) >= opt%mxfnad) THEN
          CYCLE
        ELSE
          NFPCV=IDNINT(DBLE(opt%mxfnad - extsaa(isat)%sys(0)%resolu(4)) / &
                                         extsaa(isat)%sys(0)%resolu(2))
          IF (NFPCV < 1) CYCLE

          ALLOCATE(hlppcv(0:extsaa(isat)%sys(0)%resolu(1),nelv,nazi),stat=iac)
          CALL alcerr(iac,'hlppcv',(/extsaa(isat)%sys(0)%resolu(1),nelv,nazi/),'ATX2PCV')

          IF ((nelv+NFPCV) > MAXELV) THEN
            WRITE(lfnerr,forma3) filext,extsaa(isat)%name,(nelv+NFPCV),MAXELV
            CALL exitrc(2)
          ENDIF

          DO IFRQ=1,extsaa(isat)%sys(0)%nfreq
            hlppcv = extsaa(isat)%sys(0)%freq(ifrq)%pat
            DEALLOCATE(extsaa(isat)%sys(0)%freq(ifrq)%pat)
            ALLOCATE(extsaa(isat)%sys(0)%freq(ifrq)%pat(0:extsaa(isat)%sys(0)%resolu(1), &
                                                                        nelv+NFPCV,nazi),stat=iac)
            CALL alcerr(iac,'extsaa(isat)%sys(0)%freq(ifrq)%pat',(/extsaa(isat)%sys(0)%resolu(1), &
                                                                       nelv+NFPCV,nazi/),'ATX2PCV')
            DO iord=0,extsaa(isat)%sys(0)%resolu(1)
              DO IAZI=1,nazi
                DO IELV=1,nelv
                  extsaa(isat)%sys(0)%freq(ifrq)%pat(iord,ielv,iazi) = hlppcv(iord,ielv,iazi)
                ENDDO
              ENDDO
            ENDDO
            DO iazi=1,nazi
              DO ielv=nelv+1,nelv+NFPCV
                IF (opt%finmod == 1) THEN
                  extsaa(isat)%sys(0)%freq(ifrq)%pat(0,ielv,iazi) = 0.D0
                ELSE
                  extsaa(isat)%sys(0)%freq(ifrq)%pat(0,ielv,iazi) = &
                       extsaa(isat)%sys(0)%freq(ifrq)%pat(0,nelv,iazi)
                ENDIF
              ENDDO
            ENDDO
          ENDDO
          extsaa(isat)%sys(0)%resolu(4) = opt%mxfnad
          DEALLOCATE(hlppcv)
        ENDIF
      ENDDO
    ENDIF
    IF (opt%debug == 1) WRITE(*,*)'Fill patterns ok'
! End of fill patterns

! Do not write PCV if all are zero
! --------------------------------
    IF (opt%nozero == 1) THEN
! For receiver antennas
      DO irec=1,nrec1
        IF (extrea(irec)%sys(0)%typ == 0) CYCLE
        iszero = 1
        DO ifrq=1,extrea(irec)%sys(0)%nfreq
          DO iord=0,extrea(irec)%sys(0)%resolu(1)
            DO iazi=1,SIZE(extrea(irec)%sys(0)%freq(ifrq)%pat,3)
              DO ielv=1,SIZE(extrea(irec)%sys(0)%freq(ifrq)%pat,2)
                IF (extrea(irec)%sys(0)%freq(ifrq)%pat(iord,ielv,iazi) /= 0.D0) &
                                                                 iszero = 0
              ENDDO
            ENDDO
          ENDDO
        ENDDO
        IF (iszero == 1) THEN
          extrea(irec)%sys(0)%typ = 0
          DO IFRQ=1,extrea(irec)%sys(0)%nfreq
            DEALLOCATE(extrea(irec)%sys(0)%freq(ifrq)%pat)
          ENDDO
        ENDIF
      ENDDO
! For satellite antennas
      DO isat=1,nsat1
        IF (extsaa(isat)%sys(0)%typ == 0) CYCLE
        iszero = 1
        DO ifrq=1,extsaa(isat)%sys(0)%nfreq
          DO iord=0,extsaa(isat)%sys(0)%resolu(1)
            DO iazi=1,SIZE(extsaa(isat)%sys(0)%freq(ifrq)%pat,3)
              DO ielv=1,SIZE(extsaa(isat)%sys(0)%freq(ifrq)%pat,2)
                IF (extsaa(isat)%sys(0)%freq(ifrq)%pat(iord,ielv,iazi) /= 0.D0) &
                                                                 iszero = 0
              ENDDO
            ENDDO
          ENDDO
        ENDDO
        IF (iszero == 1) THEN
          extsaa(isat)%sys(0)%typ = 0
          DO IFRQ=1,extsaa(isat)%sys(0)%nfreq
            DEALLOCATE(extsaa(isat)%sys(0)%freq(ifrq)%pat)
          ENDDO
        ENDIF
      ENDDO
    ENDIF
    IF (opt%debug == 1) WRITE(*,*)'No zero ok'
! End of nozero

! Copy receiver antennas from NONE antennas if they are not in ANTEX
    IF (ALLOCATED(noneant)) THEN
      first = .TRUE.
      nrec2 = nrec1
      DO irec=1,nrec1
        IF (extrea(irec)%name(17:20) /= 'NONE') CYCLE
! If NONE look whether antennas existing that are not in ANTEX file
        DO inone=1,count
          IF (extrea(irec)%name(1:16) == noneant(inone)(1:16) .AND. &
              (extrea(irec)%numb == 0 .OR. extrea(irec)%numb == undef_i)) THEN
            IF (first) THEN
              first=.FALSE.
              WRITE (lfnprt,"(//,' Receiver antennas with no',             &
                       &  ' corresponding entry in ANTEX or input phase ', &
                       &  'file',/,' -----------------------------',       &
                       &  '---------------------------------------------', &
                       &  '------------',//,' Rec. antenna name   -->',    &
                       &  ' filled with NONE antenna values from ANTEX',/, &
                       &  ' --------------------')")
            ENDIF
            WRITE(lfnprt,"(' ',A20)")noneant(inone)
            nrec2 = nrec2 + 1
            extrea(nrec2)%name = noneant(inone)
            extrea(nrec2)%numb = extrea(irec)%numb
            DO isys=0,maxsys-1
              IF (isys == 0 .OR. extrea(irec)%sys(isys)%nfreq /= 0) &
                CALL alcfrq(isys,extrea(irec)%sys(isys)%nfreq, &
                     extrea(irec)%sys(isys)%typ,               &
                     extrea(irec)%sys(isys)%resolu,extrea,nrec2)
              extrea(nrec2)%sys(isys)%nfreq  = extrea(irec)%sys(isys)%nfreq
              extrea(nrec2)%sys(isys)%typ    = extrea(irec)%sys(isys)%typ
              extrea(nrec2)%sys(isys)%sinex  = extrea(irec)%sys(isys)%sinex
              extrea(nrec2)%sys(isys)%resolu = extrea(irec)%sys(isys)%resolu
              extrea(nrec2)%sys(isys)%date   = extrea(irec)%sys(isys)%date
              extrea(nrec2)%sys(isys)%remark = extrea(irec)%sys(isys)%remark
              IF (extrea(irec)%sys(isys)%nfreq /= 0) THEN
                DO iFrq = 1,extrea(irec)%sys(isys)%nfreq
                  extrea(nrec2)%sys(isys)%freq(ifrq)%freq  = &
                    extrea(irec)%sys(isys)%freq(ifrq)%freq
                  extrea(nrec2)%sys(isys)%freq(ifrq)%fac   = &
                    extrea(irec)%sys(isys)%freq(ifrq)%fac
                  extrea(nrec2)%sys(isys)%freq(ifrq)%off   = &
                    extrea(irec)%sys(isys)%freq(ifrq)%off
                  IF (extrea(irec)%sys(isys)%typ /= 0) &
                    extrea(nrec2)%sys(isys)%freq(ifrq)%pat = &
                      extrea(irec)%sys(isys)%freq(ifrq)%pat
                ENDDO
              ENDIF
              extrea(nrec2)%sys(isys)%method = 'ADOPTED from NONE'
            ENDDO
          ENDIF
        ENDDO
      ENDDO
      IF (filsta /= ' ') DEALLOCATE(noneant)
    ENDIF
    IF (opt%debug == 1) WRITE(*,*)'Enlargement ok'

! Rename ANTEX satellite antenna names with those from satellite info file
! and double entries if necessary
    nsat2 = nsat1
    DO isat=1,nsat1
      ren = 0
      DO sat=1,satfil%nsensor
        IF (linatx(sat) == isat) THEN
          IF (ren == 0) THEN
            extsaa(isat)%name = satfil%sensor(sat)%sensor
            extsaa(isat)%numb = satfil%sensor(sat)%numb
            DO ifrq=1,extsaa(isat)%sys(0)%nfreq
              DO icor=1,3
                extsaa(isat)%sys(0)%freq(ifrq)%off(0,icor) = &
                dANTOFF(icor,ifrq,sat)
              ENDDO
            ENDDO
            ren = 1
          ELSE
            nsat2 = nsat2 + 1
            CALL alcfrq(0,extsaa(isat)%sys(0)%nfreq,extsaa(isat)%sys(0)%typ, &
                 extsaa(isat)%sys(0)%resolu,extsaa,nsat2)
            extsaa(nsat2)      = extsaa(isat)
            extsaa(nsat2)%name = satfil%sensor(sat)%sensor
            extsaa(nsat2)%numb = satfil%sensor(sat)%numb
          ENDIF
        ENDIF
      ENDDO
    ENDDO
    DEALLOCATE(linatx)
    IF (opt%debug == 1) WRITE(*,*)'Satellite antenna renaming ok'

! Add additional information necessary
! ------------------------------------
    title   = ' '
    filinfo = ' '
    title(1:45)  = 'ANTENNA PHASE CENTER VARIATIONS DERIVED FROM '
    title(46:64) = 'ANTEX FILE, BSW5.2'
    title(66:74) = DATE
    title(76:80) = TIME
    CALL stripdir(filext)
    WRITE(filinfo(1:LEN_TRIM(filext)),'(A)') filext(1:LEN_TRIM(filext))

    IF (opt%debug == 1) WRITE(*,*)'End of ANTEX'
  ENDIF
! =====================
! End of ANTEX section!

! Fill missing values for GLONASS and/or GALILEO with values from GPS
  IF (filrcv /= '') THEN
    forma4 = "(//,' Receiver antennas with copied values from GPS or group values', &
             &  /,' -------------------------------------------------------------', &
             & //,' Rec. antenna name    Number Sys  --> filled with values from:', &
             &  /,' -------------------- ------ -')"
    first = .true.
    DO irec=1,nrec2
      recsyst(:) = 0
      DO icrx=1,stacrux%ninfo
        IF (extrea(irec)%name == stacrux%stainfo(icrx)%antnam) THEN
          IF (stacrux%stainfo(icrx)%recnam /= undef_c) THEN
            CALL getrcv(stacrux%stainfo(icrx)%recnam,NFREQ,ICODE,IWLFAC,ICLASS,ISYST)
            IF (ISYST == -1) THEN
              recsyst(0:2) = 1
            ELSEIF (ISYST == 0) THEN
              recsyst(0) = 1
            ELSEIF (ISYST == 1) THEN
              recsyst(0) = 1
              recsyst(1) = 1
            ELSEIF (ISYST == 2) THEN
              recsyst(0) = 1
              recsyst(2) = 1
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      DO isys=1,maxsys-1
        gpscop = 0
        IF (recsyst(isys) == 1 .AND. extrea(irec)%sys(isys)%nfreq == 0) THEN
          IF (opt%fiindv >= 2 .AND. &
              extrea(irec)%numb /= 0 .AND. &
              extrea(irec)%numb /= undef_i) THEN
            DO irec2=1,nrec2
              IF (irec2 /= irec .AND. &
                  extrea(irec)%name == extrea(irec2)%name .AND. &
                  (extrea(irec2)%numb == 0 .OR. &
                   extrea(irec2)%numb == undef_i)) THEN
                IF (extrea(irec2)%sys(isys)%nfreq /= 0 .AND. &
                    extrea(irec2)%sys(isys)%method /= 'ADOPTED from GPS    ') THEN
                  IF (extrea(irec2)%sys(isys)%typ > 0) THEN
                    nelv = SIZE(extrea(irec2)%sys(isys)%freq(1)%pat,2)
                    nazi = SIZE(extrea(irec2)%sys(isys)%freq(1)%pat,3)
                  ELSE
                    nelv = 0
                    nazi = 0
                  ENDIF
                  IF (first) THEN
                    WRITE(lfnprt,forma4)
                    first = .FALSE.
                  ENDIF
! Simple copy from same system
                  CALL alcfrq(isys,extrea(irec2)%sys(isys)%nfreq, &
                                   extrea(irec2)%sys(isys)%typ, &
                                   extrea(irec2)%sys(isys)%resolu, &
                              extrea,irec)
                  extrea(irec)%sys(isys)%nfreq  = extrea(irec2)%sys(isys)%nfreq
                  extrea(irec)%sys(isys)%typ    = extrea(irec2)%sys(isys)%typ
                  extrea(irec)%sys(isys)%resolu = extrea(irec2)%sys(isys)%resolu
                  extrea(irec)%sys(isys)%sinex  = extrea(irec2)%sys(isys)%sinex
                  extrea(irec)%sys(isys)%date   = extrea(irec2)%sys(isys)%date
                  extrea(irec)%sys(isys)%remark = extrea(irec2)%sys(isys)%remark
                  extrea(irec)%sys(isys)%method = 'ADOPTED from group  '
                  DO ifrq=1,extrea(irec2)%sys(isys)%nfreq
                    extrea(irec)%sys(isys)%freq(ifrq)%freq  = &
                      extrea(irec2)%sys(isys)%freq(ifrq)%freq
                    extrea(irec)%sys(isys)%freq(ifrq)%fac   = &
                      extrea(irec2)%sys(isys)%freq(ifrq)%fac
                    extrea(irec)%sys(isys)%freq(ifrq)%off   = &
                      extrea(irec2)%sys(isys)%freq(ifrq)%off
                    IF(extrea(irec2)%sys(isys)%typ /= 0) &
                      extrea(irec)%sys(isys)%freq(ifrq)%pat = &
                        extrea(irec2)%sys(isys)%freq(ifrq)%pat
                  ENDDO
! Compute difference and apply to GPS values
                  IF (opt%fiindv == 3) THEN
                    DO ifrq=1,extrea(irec)%sys(isys)%nfreq
                      DO icor=1,3
                        extrea(irec)%sys(isys)%freq(ifrq)%off(0,icor) = &
                          (extrea(irec2)%sys(isys)%freq(ifrq)%off(0,icor) * &
                           extrea(irec2)%sys(isys)%freq(ifrq)%fac(0) - &
                           extrea(irec2)%sys(0)%freq(ifrq)%off(0,icor) * &
                           extrea(irec2)%sys(0)%freq(ifrq)%fac(0) + &
                           extrea(irec)%sys(0)%freq(ifrq)%off(0,icor) * &
                           extrea(irec)%sys(0)%freq(ifrq)%fac(0)) / &
                           extrea(irec)%sys(isys)%freq(ifrq)%fac(0)
                      ENDDO
                      DO ielv=1,nelv
                        DO iazi=1,nazi
                          extrea(irec)%sys(isys)%freq(ifrq)%pat(0,ielv,iazi) = &
                            (extrea(irec2)%sys(isys)%freq(ifrq)%pat(0,ielv,iazi) * &
                             extrea(irec2)%sys(isys)%freq(ifrq)%fac(0) - &
                             extrea(irec2)%sys(0)%freq(ifrq)%pat(0,ielv,iazi) * &
                             extrea(irec2)%sys(0)%freq(ifrq)%fac(0) + &
                             extrea(irec)%sys(0)%freq(ifrq)%pat(0,ielv,iazi) * &
                             extrea(irec)%sys(0)%freq(ifrq)%fac(0)) / &
                             extrea(irec)%sys(isys)%freq(ifrq)%fac(0)
                        ENDDO
                      ENDDO
                    ENDDO
                    DO iord=1,extrea(irec)%sys(isys)%resolu(1)
                      DO ifrq=1,extrea(irec)%sys(isys)%nfreq
                        DO icor=1,3
                          extrea(irec)%sys(isys)%freq(ifrq)%off(iord,icor) = &
                            extrea(irec2)%sys(isys)%freq(ifrq)%off(iord,icor)
                        ENDDO
                        DO ielv=1,nelv
                          DO iazi=1,nazi
                            extrea(irec)%sys(isys)%freq(ifrq)%pat(iord,ielv,iazi) = &
                              extrea(irec2)%sys(isys)%freq(ifrq)%pat(iord,ielv,iazi)
                          ENDDO
                        ENDDO
                      ENDDO
                    ENDDO
                    WRITE (lfnprt,"(1X,A20,1X,I6,1X,A1,10X,A27,1X,A20)") &
                           extrea(irec)%name,extrea(irec)%numb, &
                           g_svnsys(isys),'group value differences of', &
                           extrea(irec2)%name
                  ELSE
                    WRITE (lfnprt,"(1X,A20,1X,I6,1X,A1,10X,A15,1X,A20)") &
                           extrea(irec)%name,extrea(irec)%numb, &
                           g_svnsys(isys),'group values of', &
                           extrea(irec2)%name
                  ENDIF
                  EXIT
                ELSE
                  gpscop = 1
                  EXIT
                ENDIF
              ELSEIF (irec2 == nrec2) THEN
                gpscop = 1
              ENDIF
            ENDDO
          ENDIF
! GPS copy
          IF (opt%fiindv == 1 .OR. gpscop == 1 .OR. (opt%fiindv >= 2 .AND. &
             (extrea(irec)%numb == 0 .OR. extrea(irec)%numb == undef_i))) THEN
            IF (extrea(irec)%sys(0)%nfreq == 0) THEN
              WRITE(lfnerr,"(/,' ### PG ATX2PCV: No GPS values available for copy', &
                           & /,17X,'Antenna: ',A20,                           &
                           & /,17X,'System: ',A1,                             &
                           & /,17X,'No values written for that system!',/)")  &
                    extrea(irec)%name,g_svnsys(isys)
              CYCLE
            ENDIF
            IF (first) THEN
              WRITE(lfnprt,forma4)
              first = .FALSE.
            ENDIF
            CALL alcfrq(isys,extrea(irec)%sys(0)%nfreq, &
                             extrea(irec)%sys(0)%typ, &
                             extrea(irec)%sys(0)%resolu, &
                        extrea,irec)
            extrea(irec)%sys(isys)%nfreq  = extrea(irec)%sys(0)%nfreq
            extrea(irec)%sys(isys)%typ    = extrea(irec)%sys(0)%typ
            extrea(irec)%sys(isys)%resolu = extrea(irec)%sys(0)%resolu
            extrea(irec)%sys(isys)%sinex  = extrea(irec)%sys(0)%sinex
            extrea(irec)%sys(isys)%date   = extrea(irec)%sys(0)%date
            extrea(irec)%sys(isys)%remark = extrea(irec)%sys(0)%remark
            extrea(irec)%sys(isys)%method = 'ADOPTED from GPS    '
            DO ifrq=1,extrea(irec)%sys(0)%nfreq
              extrea(irec)%sys(isys)%freq(ifrq)%freq  = &
                extrea(irec)%sys(0)%freq(ifrq)%freq
              extrea(irec)%sys(isys)%freq(ifrq)%fac   = &
                extrea(irec)%sys(0)%freq(ifrq)%fac
              extrea(irec)%sys(isys)%freq(ifrq)%off   = &
                extrea(irec)%sys(0)%freq(ifrq)%off
              IF (extrea(irec)%sys(isys)%typ /= 0) &
                extrea(irec)%sys(isys)%freq(ifrq)%pat = &
                  extrea(irec)%sys(0)%freq(ifrq)%pat
              IF(isys == 2 .AND. ifrq == 2) &
                extrea(irec)%sys(isys)%freq(ifrq)%freq = 7
            ENDDO
            WRITE(lfnprt,"(1X,A20,1X,I6,1X,A1,10X,A19)") &
              extrea(irec)%name,extrea(irec)%numb, &
              g_svnsys(isys),'GPS of same antenna'
          ENDIF
        ENDIF
      ENDDO
    ENDDO
  ENDIF
  IF (opt%debug == 1) WRITE(*,*)'Filling missing system values ok'

! Sort all entries in arrays (satellite antennas, receiver antennas, SLR etc.)
! ----------------------------------------------------------------------------
  ALLOCATE(ANTHLP(nrec2),stat=iac)
  CALL alcerr(iac,'ANTHLP(nrec2)',(/nrec2/),'ATX2PCV')
  ALLOCATE(rindx(nrec2),stat=iac)
  CALL alcerr(iac,'rindx(nrec2)',(/nrec2/),'ATX2PCV')
  DO irec=1,nrec2
    ANTHLP(irec)(1:20) = extrea(irec)%name
    WRITE(ANTHLP(irec)(21:26),'(I6)') extrea(irec)%numb
  ENDDO
  CALL cordup(ANTHLP,nrec2,1,26,rindx)
  DEALLOCATE(ANTHLP)

  IF (iform == 2) THEN
    ALLOCATE(ANTHLP(nsat2),stat=iac)
    CALL alcerr(iac,'ANTHLP(nsat2)',(/nsat2/),'ATX2PCV')
    ALLOCATE(sindx(nsat2),stat=iac)
    CALL alcerr(iac,'sindx(nsat2)',(/nsat2/),'ATX2PCV')
    DO isat=1,nsat2
      ANTHLP(isat)(1:20) = extsaa(isat)%name
      WRITE(ANTHLP(isat)(21:26),'(I6)') extsaa(isat)%numb
    ENDDO
    CALL cordup(ANTHLP,nsat2,1,26,sindx)
    DEALLOCATE(ANTHLP)
  ENDIF
  IF (opt%debug == 1) WRITE(*,*)'Sorting ok'

! Write phase center variations in bernese format
! -----------------------------------------------
  IF (iform == 1) THEN
    CALL wtphafil(filphc,extrea,title=title,model=satfil%pcvmod,filinfo=filinfo,rindx=rindx)
  ELSE
    CALL wtphafil(filphc,extsaa,extrea,title,satfil%pcvmod,filinfo,sindx,rindx)

    DEALLOCATE(extsaa)
    DEALLOCATE(sindx)

  ENDIF

  DEALLOCATE(extrea)
  DEALLOCATE(rindx)
  IF (opt%debug == 1) WRITE(*,*)'END of program'

  CALL exitrc(0)

END PROGRAM ATX2PCV

