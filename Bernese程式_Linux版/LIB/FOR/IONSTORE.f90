MODULE s_IONSTORE
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE ionstore(neq)

! -------------------------------------------------------------------------
! Purpose:    Store ION and INX results computed by ADDNEQ2
!
! Author:     S. Schaer
!
! Created:    26-Mar-2002
!
! Changes:    28-Mar-2002 SS: Compute formal errors for max TEC values
!             28-Mar-2002 SS: Give peak TEC values for the maps
!             03-Apr-2002 SS: Refined computation of RMS maps
!             21-Aug-2002 SS: Allow more than one DCB set in file
!             13-Nov-2002 SS: "maxlin" from 30 to 40
!             17-Feb-2003 LM: Use m_maxdim
!             18-FEB-2003 HU: Use pgmver from m_bern
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             09-May-2009 RD: Freq-dep. code bias in DCB files
!             26-MAR-2012 RD: Use GETGIM as module now
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnerr, pgmver, lfnprt
  USE m_maxdim, ONLY: maxgit, maxgim, maxsat, maxrec
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: opt,comstat
  USE d_const,  ONLY: filtitle,date,time,conre,pi

  USE f_ikf
  USE s_dimtst
  USE s_alcerr
  USE s_wtixfl
  USE s_rdixcf
  USE s_ionmax
  USE s_savgim
  USE s_getgim
  USE s_rdcbfl
  USE s_dcbadt
  USE s_eflsfl
  USE s_exitrc
  USE f_aslef2
  USE s_lowerc
  IMPLICIT NONE

! List of parameters
! ------------------
  TYPE(t_neq) :: neq

! Local variables
! ---------------
  INTEGER(i4b), PARAMETER                         :: maxlin = 40
  INTEGER(i4b)                                    :: iac
  INTEGER(i4b)                                    :: nmodel
  INTEGER(i4b)                                    :: iontyp
  INTEGER(i4b)                                    :: icbtyp
  INTEGER(i4b)                                    :: numsat
  INTEGER(i4b)                                    :: numrec
  INTEGER(i4b)                                    :: numifb
  INTEGER(i4b)                                    :: irec
  INTEGER(i4b)                                    :: numsta
  INTEGER(i4b)                                    :: imod
  INTEGER(i4b)                                    :: imod0
  INTEGER(i4b)                                    :: itrm
  INTEGER(i4b)                                    :: itrm1
  INTEGER(i4b)                                    :: itrm2
  INTEGER(i4b)                                    :: ipar
  INTEGER(i4b)                                    :: ipar0
  INTEGER(i4b)                                    :: iexp
  INTEGER(i4b)                                    :: irc
  INTEGER(i4b)                                    :: numlin
  INTEGER(i4b)                                    :: numval
  INTEGER(i4b)                                    :: numadt
  INTEGER(i4b)                                    :: numsa0
  INTEGER(i4b)                                    :: numre0
  INTEGER(i4b)                                    :: nmap
  INTEGER(i4b)                                    :: nlat
  INTEGER(i4b)                                    :: nlon
  INTEGER(i4b)                                    :: nlin
  INTEGER(i4b)                                    :: ilin
  INTEGER(i4b)                                    :: ilin0
  INTEGER(i4b)                                    :: ista
  INTEGER(i4b)                                    :: ista1
  INTEGER(i4b)                                    :: ista2
  INTEGER(i4b)                                    :: ndeg
  INTEGER(i4b)                                    :: nord
  INTEGER(i4b)                                    :: ideg
  INTEGER(i4b)                                    :: iord
  INTEGER(i4b)                                    :: iflg1
  INTEGER(i4b)                                    :: iflg2
  INTEGER(i4b)                                    :: ityp
  INTEGER(i4b)                                    :: ival
  INTEGER(i4b)                                    :: imap
  INTEGER(i4b)                                    :: imap1
  INTEGER(i4b)                                    :: imap2
  INTEGER(i4b)                                    :: ilat
  INTEGER(i4b)                                    :: ilon
  INTEGER(i4b)                                    :: iind
  INTEGER(i4b)                                    :: nbad1
  INTEGER(i4b)                                    :: nbad2
  INTEGER(i4b), DIMENSION(:,:),   ALLOCATABLE     :: ionreq
  INTEGER(i4b), DIMENSION(:),     ALLOCATABLE     :: nterm
  INTEGER(i4b), DIMENSION(:,:,:), ALLOCATABLE     :: nm
  INTEGER(i4b), DIMENSION(4)                      :: ioninf
  INTEGER(i4b), DIMENSION(:),     ALLOCATABLE     :: dcbid1
  INTEGER(i4b), DIMENSION(4)                      :: inxinf
  INTEGER(i4b), DIMENSION(2)                      :: inxsp2
  INTEGER(i4b), DIMENSION(:,:),   ALLOCATABLE     :: ionind
  INTEGER(i4b), DIMENSION(:),     ALLOCATABLE     :: itec

  REAL(r8b)                                       :: xepo
  REAL(r8b)                                       :: xepo1
  REAL(r8b)                                       :: xepo2
  REAL(r8b)                                       :: xint
  REAL(r8b)                                       :: xdint
  REAL(r8b)                                       :: xlat1
  REAL(r8b)                                       :: xlat2
  REAL(r8b)                                       :: xlon1
  REAL(r8b)                                       :: xlon2
  REAL(r8b)                                       :: xtec
  REAL(r8b)                                       :: qsum1
  REAL(r8b)                                       :: qsum2
  REAL(r8b)                                       :: xbad1
  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE       :: iondev
  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE       :: ioncoe
  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE       :: ionsig
  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE       :: dcbva1
  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE       :: dcbva2
  REAL(r8b),    DIMENSION(4,1)                    :: dcbva3
  REAL(r8b),    DIMENSION(12)                     :: inxdef
  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE       :: tecmap
  REAL(r8b),    DIMENSION(14)                     :: inxsp1
  REAL(r8b),    DIMENSION(2)                      :: pole
  REAL(r8b),    DIMENSION(:),   ALLOCATABLE       :: alfhlp
  REAL(r8b),    DIMENSION(4)                      :: tecmax
  REAL(r8b),    DIMENSION(4)                      :: tecmin
  REAL(r8b),    DIMENSION(2)                      :: xlat
  REAL(r8b),    DIMENSION(4)                      :: xpos

  CHARACTER(LEN=80)                               :: iontit
  CHARACTER(LEN=80), DIMENSION(:), ALLOCATABLE    :: adtlst
  CHARACTER(LEN=60), DIMENSION(:), ALLOCATABLE    :: destxt
  CHARACTER(LEN=60), DIMENSION(:), ALLOCATABLE    :: comtxt
  CHARACTER(LEN=60)                               :: dcbtxt
  CHARACTER(LEN=60)                               :: obstxt
  CHARACTER(LEN=60)                               :: tittxt
  CHARACTER(LEN=60)                               :: adtlbl
  CHARACTER(LEN=20)                               :: satstr
  CHARACTER(LEN=20)                               :: agestr
  CHARACTER(LEN=20)                               :: pgmstr
  CHARACTER(LEN=20)                               :: datstr
  CHARACTER(LEN=16), DIMENSION(:), ALLOCATABLE    :: iontxt
  CHARACTER(LEN=16), DIMENSION(:), ALLOCATABLE    :: dcbid2
  CHARACTER(LEN=16), DIMENSION(2,1)               :: dcbid3
  CHARACTER(LEN=4),  DIMENSION(:), ALLOCATABLE    :: stanam
  CHARACTER(LEN=4)                                :: mapstr
  CHARACTER(LEN=1),  DIMENSION(:), ALLOCATABLE    :: dcbsys

! Return, if neither ION nor INX output file specified
! ----------------------------------------------------

  IF ( opt%ionosrs == ' ' .AND. opt%ionexrs == ' ' ) RETURN

! Allocate the arrays
! -------------------

  ALLOCATE( ionreq(6,maxgim), stat=iac )
  CALL alcerr(iac, 'ionreq', (/6,maxgim/), 'ionstore')
  ALLOCATE( iondev(10,maxgim), stat=iac )
  CALL alcerr(iac, 'iondev', (/10,maxgim/), 'ionstore')
  ALLOCATE( nterm(maxgim), stat=iac )
  CALL alcerr(iac, 'nterm', (/maxgim/), 'ionstore')
  ALLOCATE( nm(maxgit,2,maxgim), stat=iac )
  CALL alcerr(iac, 'nm', (/maxgit,2,maxgim/), 'ionstore')
  ALLOCATE( ioncoe(maxgit,maxgim), stat=iac )
  CALL alcerr(iac, 'ioncoe', (/maxgit,maxgim/), 'ionstore')
  ALLOCATE( ionsig(maxgit,maxgim), stat=iac )
  CALL alcerr(iac, 'ionsig', (/maxgit,maxgim/), 'ionstore')
  ALLOCATE( iontxt(maxgim), stat=iac )
  CALL alcerr(iac, 'iontxt', (/maxgim/), 'ionstore')
  ALLOCATE( ionind(maxgit,maxgim), stat=iac )
  CALL alcerr(iac, 'ionind', (/maxgit,maxgim/), 'ionstore')

  ALLOCATE( dcbid1(maxsat), stat=iac )
  CALL alcerr(iac, 'dcbid1', (/maxsat/), 'ionstore')
  ALLOCATE( dcbva1(2,maxsat), stat=iac )
  CALL alcerr(iac, 'dcbva1', (/2,maxsat/), 'ionstore')
  ALLOCATE( dcbid2(maxrec), stat=iac )
  CALL alcerr(iac, 'dcbid2', (/maxrec/), 'ionstore')
  ALLOCATE( dcbva2(2,maxrec), stat=iac )
  CALL alcerr(iac, 'dcbva2', (/2,maxrec/), 'ionstore')
  ALLOCATE( dcbsys(maxrec), stat=iac )
  CALL alcerr(iac, 'dcbsys', (/maxrec/), 'ionstore')
  ALLOCATE( stanam(maxrec), stat=iac )
  CALL alcerr(iac, 'stanam', (/maxrec/), 'ionstore')

  numlin = maxlin+(maxrec-1)/12+4
  ALLOCATE( destxt(numlin), stat=iac )
  CALL alcerr(iac, 'destxt', (/numlin/), 'ionstore')
  ALLOCATE( comtxt(numlin), stat=iac )
  CALL alcerr(iac, 'comtxt', (/numlin/), 'ionstore')
  numadt = maxsat+maxrec+1
  ALLOCATE( adtlst(numadt), stat=iac )
  CALL alcerr(iac, 'adtlst', (/numadt/), 'ionstore')
  ALLOCATE( alfhlp(maxgit), stat=iac )
  CALL alcerr(iac, 'alfhlp', (/maxgit/), 'ionstore')

! Read ION master file
! --------------------

  IF ( opt%ionos == ' ' ) THEN
    WRITE(lfnerr,'(/,A,/)') ' *** SR IONSTORE: ION master file required'
    CALL exitrc(2)
  ENDIF

  CALL getgim(opt%ionos,1,nmodel,ionreq,iondev,nterm, &
              nm,ioncoe,ionsig,iontxt,iontit,ioninf, &
              iontyp)

  IF ( iontyp /= 2) THEN
    WRITE(lfnerr,'(/,A,A,/,18X,A,A,/)') ' *** SR IONSTORE: ', &
      'ION master file not accepted', &
      'File name: ',opt%ionos
    CALL exitrc(2)
  ENDIF

! Read DCB output file
! --------------------

  IF ( opt%dcbout /= ' ') THEN
    icbtyp = 1
    CALL rdcbfl(opt%dcbout,maxsat,maxrec,0,icbtyp,numsat,numrec,  &
                numifb,dcbid1,dcbva1,dcbid2,dcbva2,dcbsys,dcbid3, &
                dcbva3)

    IF ( icbtyp == 0) THEN
      WRITE(lfnerr,'(/,A,A,/,18X,A,A,/)') ' *** SR IONSTORE: ', &
        'Unexpected error with respect to DCB file', &
        'File name: ',opt%dcbout
      CALL exitrc(2)
    ENDIF

! Update total number of satellites and stations
! ----------------------------------------------

    IF ( numsat > 0) ioninf(2) = numsat

    IF ( numrec > 0 ) THEN
      numsta = 1
      stanam(1) = dcbid2(1)(1:4)
      DO irec = 2,numrec
        IF ( dcbid2(irec)(1:4) /= stanam(numsta) ) THEN
          numsta = numsta + 1
          stanam(numsta) = dcbid2(irec)(1:4)
        ENDIF
      ENDDO
      ioninf(1) = numsta
    ENDIF

  ELSE

    numsat = 0
    numrec = 0
    numsta = 0

  ENDIF

! Update ionospheric coefficients
! -------------------------------

  DO imod = 1,nmodel

    DO itrm = 1,nterm(imod)

      ipar0 = 0
      DO ipar = 1,neq%misc%npar

        IF ( neq%par(ipar)%locq(1) /= 19 ) CYCLE

        IF ( neq%par(ipar)%locq(4) == nm(itrm,1,imod) .AND. &
             neq%par(ipar)%locq(5) == nm(itrm,2,imod) ) THEN

          IF ( neq%par(ipar)%time%half > 0.d0 ) THEN
            xepo1 = neq%par(ipar)%time%mean - neq%par(ipar)%time%half
            xepo2 = neq%par(ipar)%time%mean + neq%par(ipar)%time%half
          ELSE
            xepo1 = neq%par(ipar)%time%mean
            xepo2 = 0.d0
          ENDIF

          IF ( ABS(iondev(5,imod)-xepo1) < 1.d0/1440.d0 .AND. &
               ABS(iondev(6,imod)-xepo2) < 1.d0/1440.d0 ) ipar0 = ipar

        ENDIF

      ENDDO

      IF ( ipar0 /= 0 ) THEN
        ioncoe(itrm,imod) = neq%xxx(ipar0) + neq%par(ipar0)%x0
        ionsig(itrm,imod) = comstat%rms * SQRT(ABS(neq%anor(ikf(ipar0,ipar0))))

        ionind(itrm,imod) = ipar0
      ELSE
        WRITE(lfnerr,'(/,A,/)') ' *** SR IONSTORE: GIM parameter missing'
        CALL exitrc(2)
      ENDIF

    ENDDO

    ndeg = ionreq(2,1)
    nord = ionreq(3,1)

! Compute maximum TEC value and formal error
! ------------------------------------------

    CALL ionmax(imod,ionreq,iondev,nterm,nm,ioncoe, &
                tecmax,tecmin)

    DO ityp = 1,2

      DO itrm = 1,nterm(imod)
        ideg = nm(itrm,1,imod)
        iord = nm(itrm,2,imod)
        IF ( ityp == 1 ) THEN
          alfhlp(itrm) = aslef2(tecmax(2),tecmax(3),ideg,iord,ndeg,nord)
        ELSE
          alfhlp(itrm) = aslef2(tecmin(2),tecmin(3),ideg,iord,ndeg,nord)
        ENDIF
      ENDDO

      qsum1 = 0.d0
      DO itrm1 = 1,nterm(imod)
        qsum2 = 0.d0
        DO itrm2 = 1,nterm(imod)
          iind = ikf(ionind(itrm2,imod),ionind(itrm1,imod))
          qsum2 = qsum2+alfhlp(itrm2)*neq%anor(iind)
        ENDDO
        qsum1 = qsum1+qsum2*alfhlp(itrm1)
      ENDDO

      IF ( ityp == 1 ) THEN
        tecmax(4) = comstat%rms*SQRT(qsum1)
      ELSE
        tecmin(4) = comstat%rms*SQRT(qsum1)
      ENDIF

    ENDDO

    iondev( 9,imod) = tecmax(1)
    iondev(10,imod) = tecmax(4)

! Print extractable ADDNEQ2 GIM results summary
! ---------------------------------------------

    IF ( imod == 1 ) WRITE(lfnprt,"(//,A,/,A,//,A,A,/,A,A,/,1X,131('-'))") &
      ' GIM Results Summary:', &
      ' --------------------', &
      ' Model / station     Min lat  Max lat   Mean TEC  RMS err   ', &
      'Max TEC  RMS err  Lat     Lon       Min TEC  RMS err  Lat     Lon', &
      '                     (deg)    (deg)     (TECU)    (TECU)    ', &
      '(TECU)   (TECU)   (deg)   (deg)     (TECU)   (TECU)   (deg)   (deg)'

    xlat(1) = 180.d0/pi*iondev(7,imod)
    xlat(2) = 180.d0/pi*iondev(8,imod)

    xpos(1) = 180.d0/pi*tecmax(2)
    xpos(2) = 180.d0/pi*tecmax(3)
    xpos(3) = 180.d0/pi*tecmin(2)
    xpos(4) = 180.d0/pi*tecmin(3)

    WRITE(lfnprt,'(1X,A16,1X,2F9.2,1X,2F10.2,2(F10.2,F9.2,F8.2,F9.2))') &
      iontxt(imod),xlat(1),xlat(2),ioncoe(1,imod),ionsig(1,imod), &
      tecmax(1),tecmax(4),xpos(1),xpos(2), &
      tecmin(1),tecmin(4),xpos(3),xpos(4)

  ENDDO

! Write ION output file
! ---------------------

  IF ( opt%ionosrs /= ' ' ) THEN
    CALL savgim(opt%ionosrs,iontyp,nmodel,ionreq,iondev,nterm, &
                nm,ioncoe,ionsig,iontxt,filtitle,ioninf)
  ENDIF

! Prepare IONEX information, if requested
! ---------------------------------------

  IF ( opt%ionexrs /= ' ' ) THEN

    IF ( opt%ionexcf == ' ' ) THEN
      WRITE(lfnerr,'(/,A,A,/)') ' *** SR IONSTORE: ', &
        'IONEX control file required'
      CALL exitrc(2)
    ENDIF

    destxt = ' '
    comtxt = ' '

! Read IONEX control file
! -----------------------

    CALL rdixcf(maxlin,satstr,agestr,destxt,obstxt,comtxt, &
                dcbtxt,inxinf,iexp,inxdef,irc)

    IF ( irc /= 1 ) THEN
      WRITE(lfnerr,'(/,A,A,/,18X,A,A,/)') ' *** SR IONSTORE: ', &
        'IONEX control file not accepted', &
        'File name: ',opt%ionexcf
      CALL exitrc(2)
    ENDIF

! Define general IONEX specs
! --------------------------

    pgmstr = 'ADDNEQ2 V'//PGMVER
    datstr = date//' '//time
    tittxt = opt%title(1:60)

    IF ( ionreq(6,1) == 0 ) THEN
      mapstr = 'NONE'
    ELSEIF ( ionreq(6,1) == 1 ) THEN
      mapstr = 'COSZ'
    ELSEIF ( ionreq(6,1) == 2 ) THEN
      mapstr = 'NONE'
    ELSEIF ( ionreq(6,1) == 3 ) THEN
      mapstr = 'NONE'
    ELSE
      mapstr = 'NONE'

      WRITE(lfnerr,'(/,A,/)') ' ### SR IONSTORE: Mapping function unknown'
    ENDIF

    IF ( iondev(6,1) > 0.d0 ) THEN
      xint = inxdef(3)/86400.d0
      inxsp1(1) = INT((iondev(5,1)+iondev(6,1))/2.d0)+inxdef(1)
      inxsp1(2) = INT((iondev(5,nmodel)+iondev(6,nmodel))/2.d0)+inxdef(2)
    ELSE
      inxdef(3) = NINT((iondev(5,nmodel)-iondev(5,1))/(nmodel-1)*24.d0)*3600.d0
      xint = inxdef(3)/86400.d0
      inxsp1(1) = iondev(5,1)
      inxsp1(2) = iondev(5,1)+xint*(nmodel-1)

      WRITE(lfnerr,'(/,A,A,/)') ' ### SR IONSTORE: ', &
        'Options concerning requested snapshots ignored'
    ENDIF

    inxsp1(3:9) = inxdef(3:9)
    inxsp1(10) = ioninf(3)
    inxsp1(11) = 1.d-3*conre
    inxsp1(12:13) = 1.d-3*iondev(1,1)
    inxsp1(14) = 0.d0

    inxsp2(1:2) = ioninf(1:2)

! Prepare satellite and receiver DCB results
! ------------------------------------------

!!    dcbva1(2,:) = dcbva1(2,:) * inxdef(12)
!!    dcbva2(2,:) = dcbva2(2,:) * inxdef(12)

    numsa0 = numsat
    IF ( inxinf(3) == 0 ) numsa0 = 0
    numre0 = numrec
    IF ( inxinf(4) == 0 ) numre0 = 0

    IF ( numsa0+numre0 > 0 ) THEN
      CALL dcbadt(numsa0,dcbid1,dcbva1,numre0,dcbid2,dcbva2, &
                  dcbsys,dcbtxt,numadt,adtlst)

      adtlbl = 'DIFFERENTIAL CODE BIASES'
    ELSE
      adtlbl = ' '
    ENDIF

! Generate TEC and RMS maps in IONEX form
! ---------------------------------------

    nmap = NINT((inxsp1(2)-inxsp1(1))/xint)+1

    nlat = NINT((inxsp1(5)-inxsp1(4))/inxsp1(6))+1
    nlon = NINT((inxsp1(8)-inxsp1(7))/inxsp1(9))+1

    numval = nmap * nlat * nlon
    ALLOCATE( tecmap(numval,2), stat=iac )
    CALL alcerr(iac, 'tecmap', (/numval,2/), 'ionstore')
    ALLOCATE( itec(nmap), stat=iac )
    CALL alcerr(iac, 'itec', (/nmap/), 'ionstore')

    pole(1) = iondev(3,1)
    pole(2) = iondev(4,1)

    iflg1 = ionreq(4,1)
    iflg2 = ionreq(5,1)

    xdint = 2.d0/86400.d0

    itec = 0

    DO ityp = 1,2
      IF ( inxinf(ityp) == 0 ) CYCLE

      tecmap(:,ityp) = 999.9d0
      ival = 1

      DO imap = 1,nmap
        xepo = inxsp1(1)+(imap-1)*xint

        IF ( iondev(6,1) == 0.d0 ) THEN
          imod0 = imap
        ELSE
          imod0 = 0
          DO imod = 1,nmodel
            IF ( xepo >= iondev(5,imod)-xdint .AND. &
                 xepo <  iondev(6,imod)+xdint ) imod0 = imod
          ENDDO
          IF ( imod0 == 0) THEN
            WRITE(lfnerr,'(/,A,A,/)') ' *** SR IONSTORE: ', &
              'Desired GIM information not found'
            CALL exitrc(2)
          ENDIF
        ENDIF

        DO ilat = 1,nlat
          xlat1 = (inxsp1(4)+(ilat-1)*inxsp1(6))*pi/180.d0

          DO ilon = 1,nlon
            xlon1 = (inxsp1(7)+(ilon-1)*inxsp1(9))*pi/180.d0

            CALL eflsfl(xlat1,xlon1,xepo,pole,iflg1,iflg2, &
                        2,xlat2,xlon2)

            IF ( ityp == 1 ) THEN
              xtec = 0.d0
              DO itrm = 1,nterm(imod0)
                ideg = nm(itrm,1,imod0)
                iord = nm(itrm,2,imod0)
                xtec = xtec+ioncoe(itrm,imod0)* &
                  aslef2(xlat2,xlon2,ideg,iord,ndeg,nord)
              ENDDO

              itec(imap) = MAX(NINT(1.d1*xtec),itec(imap))
            ELSE
              DO itrm = 1,nterm(imod0)
                ideg = nm(itrm,1,imod0)
                iord = nm(itrm,2,imod0)
                alfhlp(itrm) = aslef2(xlat2,xlon2,ideg,iord,ndeg,nord)
              ENDDO

              qsum1 = 0.d0
              DO itrm1 = 1,nterm(imod0)
                qsum2 = 0.d0
                DO itrm2 = 1,nterm(imod0)
                  iind = ikf(ionind(itrm2,imod0),ionind(itrm1,imod0))
                  qsum2 = qsum2+alfhlp(itrm2)*neq%anor(iind)
                ENDDO
                qsum1 = qsum1+qsum2*alfhlp(itrm1)
              ENDDO

              IF ( qsum1 > 0.d0 ) THEN
                xtec = comstat%rms*SQRT(qsum1)*inxdef(12)
!!                xtec = SQRT(xtec**2+(tecmap(ival,1)*0.02d0)**2)
              ELSE
                xtec = 999.9d0
              ENDIF

            ENDIF

            IF ( ABS(xtec) > 999.9d0) xtec = 999.9d0

            tecmap(ival,ityp) = xtec
            ival = ival + 1

          ENDDO

        ENDDO

      ENDDO

    ENDDO

! Verify TEC/RMS maps and reset or reject bad values
! --------------------------------------------------

    IF ( inxinf(2) == 1 ) THEN
      nbad2 = 0
      DO ival = 1,numval
        IF ( tecmap(ival,2) > inxdef(11) .AND. &
             tecmap(ival,2) /= 999.9d0 ) THEN
          tecmap(ival,1) = 999.9d0
          tecmap(ival,2) = 999.9d0
          nbad2 = nbad2 + 1
        ENDIF
      ENDDO

      IF ( nbad2 > 0 ) &
        WRITE(lfnerr,'(/,A,I8,A,/)') ' ### SR IONSTORE: ', &
        nbad2,' bad TEC/RMS values rejected'
    ENDIF

    IF ( inxinf(1) == 1 ) THEN
      xbad1 = inxdef(10)
      nbad1 = 0
      DO ival = 1,numval
        IF ( tecmap(ival,1) < inxdef(10) ) THEN
          xbad1 = MIN(tecmap(ival,1),xbad1)
          tecmap(ival,1) = inxdef(10)
          nbad1 = nbad1 + 1
        ENDIF
      ENDDO

      IF ( nbad1 > 0 ) &
        WRITE(lfnerr,'(/,A,I8,A,F7.2,A,/)') ' ### SR IONSTORE: ', &
        nbad1,' bad TEC values down to ',xbad1,' TECU reset'
    ENDIF

! Give peak TEC values for the maps
! ---------------------------------

    ilin0 = numlin
    DO ilin = 1,numlin
      IF ( comtxt(ilin) == ' ' ) ilin0 = MIN(ilin0,ilin)
    ENDDO

    nlin = (nmap-1)/12+1
    CALL dimtst(1,2,2,'ionstore','numlin','number of comment lines', &
                ' ',nlin+ilin0,numlin,irc)

    comtxt(ilin0) = 'Peak TEC values for the included maps:'
    DO imap = 1,nmap
      IF ( itec(imap) > 9999 ) itec(imap) = 9999
    ENDDO
    DO ilin = 1,nlin
      imap1 = 12*ilin-11
      imap2 = 12*ilin
      IF ( imap2 > nmap ) imap2 = nmap
      WRITE(comtxt(ilin+ilin0),'(12(I4,1X))') &
        (itec(imap),imap=imap1,imap2)
    ENDDO

! Create list of stations
! -----------------------

    IF ( numrec > 0 ) THEN
      ilin0 = numlin
      DO ilin = 1,numlin
        IF ( comtxt(ilin) == ' ' ) ilin0 = MIN(ilin0,ilin)
      ENDDO

      nlin = (numsta-1)/12+1
      CALL dimtst(1,2,2,'ionstore','numlin','number of comment lines', &
                  ' ',nlin+ilin0,numlin,irc)

      comtxt(ilin0) = 'List of stations:'
      DO ista = 1,numsta
        CALL lowerc(stanam(ista))
      ENDDO
      DO ilin = 1,nlin
        ista1 = 12*ilin-11
        ista2 = 12*ilin
        IF ( ista2 > numsta ) ista2 = numsta
        WRITE(comtxt(ilin+ilin0),'(12(A4,1X))') &
          (stanam(ista),ista=ista1,ista2)
      ENDDO
    ENDIF

! Write INX output file
! ---------------------

    CALL wtixfl(opt%ionexrs,numval,numlin,numadt,satstr,pgmstr, &
                agestr,datstr,tittxt,destxt,mapstr,obstxt, &
                comtxt,inxsp1,inxsp2,adtlbl,adtlst,inxinf, &
                tecmap,iexp)

    DEALLOCATE( tecmap, stat=iac )
    DEALLOCATE( itec, stat=iac )

  ENDIF

! Deallocate the arrays
! ---------------------

  DEALLOCATE( ionreq, stat=iac )
  DEALLOCATE( iondev, stat=iac )
  DEALLOCATE( nterm,  stat=iac )
  DEALLOCATE( nm,     stat=iac )
  DEALLOCATE( ioncoe, stat=iac )
  DEALLOCATE( ionsig, stat=iac )
  DEALLOCATE( iontxt, stat=iac )
  DEALLOCATE( ionind, stat=iac )

  DEALLOCATE( dcbid1, stat=iac )
  DEALLOCATE( dcbva1, stat=iac )
  DEALLOCATE( dcbid2, stat=iac )
  DEALLOCATE( dcbva2, stat=iac )
  DEALLOCATE( dcbsys, stat=iac )
  DEALLOCATE( stanam, stat=iac )

  DEALLOCATE( destxt, stat=iac )
  DEALLOCATE( comtxt, stat=iac )
  DEALLOCATE( adtlst, stat=iac )
  DEALLOCATE( alfhlp, stat=iac )

END SUBROUTINE ionstore


END MODULE
