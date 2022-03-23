! -------------------------------------------------------------------------
! Bernese GNSS Software
! -------------------------------------------------------------------------

MODULE s_GOBSDEF

! -------------------------------------------------------------------------
! Purpose:    Contains subroutines to handle the Galileo external observation
!             selection (GEOS) using a dedicated geos-file.
!
! Author:     L. Prange
!
! Created:    26-Jan-2011
!
! Changes:    20-Jul-2011 LP: sr setgeos2 and sr tstobst added
!             09-May-2012 LP: sr init_rinstat added
!             29-May-2012 LP: sr readgeos2 added
!             20-Aug-2012 LP: Bugfix: obsindR212 index added to t_satsig for RINEX2.12
!             08-Nov-2012 LP: Bugfix: correct length of srname
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------


PUBLIC :: init_geos, readgeos, readgeos2, setgeos, setgeos2, tstobst, init_rinstat

! Global Variables
! ----------------

CONTAINS

! ----------------------------------------------------------------------

SUBROUTINE init_geos(norec,gobsdef)

! -------------------------------------------------------------------------
! Purpose:    Initialization of the gobsdef structure.
!
! Author:     L. Prange
!
! Created:    26-Jan-2011
!
! Changes:    24-Apr-2012 LP: loop over obstyp changed (4->8)
!             23-May-2012 LP: Scale factor for observation types added
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b
  USE m_maxdim, ONLY: MAXSAT
  USE d_rinex3, ONLY: t_gobsdef
  USE s_alcerr
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                 :: norec

! output:
  TYPE(t_gobsdef)              :: gobsdef ! Structure containing the
                                          ! external signal type
                                          ! selection for GIOVE, Galileo,
                                          ! and SBAS

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=9),PARAMETER   :: srName = 'init_geos'

! Local Variables
! ---------------
  INTEGER(i4b)                 :: iac
  INTEGER(i4b)                 :: iLin
  INTEGER(i4b)                 :: j

! =========================================
! Initialize and allocate gobsdef structure
! =========================================

  iLin=0
  IF (norec>0) THEN
     IF (norec>maxsat) THEN
       WRITE(*,*) ' *** SR ',srName,': MAXSAT (=',maxsat,') exceeded.',&
                  '                  Number of satellites needed: ',norec
       CALL EXITRC(2)
     ENDIF

     NULLIFY(gobsdef%sat)
     IF ( ASSOCIATED(gobsdef%sat) ) THEN
          DEALLOCATE( gobsdef%sat, stat=iac )
     ENDIF
     ALLOCATE(gobsdef%sat(norec),stat=iac)
     CALL alcerr(iac,'gobsdef%sat',(/norec/),srName)

     gobsdef%norec              = norec
     gobsdef%nfreqc             = 0
     gobsdef%nfreqp             = 0

     gobsdef%sat(:)%satname     = '   '
     gobsdef%sat(:)%sysnum      = 999
     gobsdef%sat(:)%syschar     = ' '
     gobsdef%sat(:)%satnum      = 999
     gobsdef%sat(:)%nfreqc      = 0
     gobsdef%sat(:)%nfreqp      = 0
     gobsdef%sat(:)%eposatind   = 0
     DO iLin=1,norec
        DO j=1,8
           gobsdef%sat(iLin)%obstyp(j)    = '   '
           gobsdef%sat(iLin)%obstyp2(j)   = '  '
           gobsdef%sat(iLin)%factor(j)    = 1
           gobsdef%sat(iLin)%obsindR212(j)= 0
        ENDDO
     ENDDO
  ELSE
     write(*,*) 'SR: ',srName,': No external obstype info available.'
  ENDIF

END SUBROUTINE init_geos


! ====================================


SUBROUTINE readgeos(filename, gobsdef, usegeos)

! -------------------------------------------------------------------------
! Purpose:    This subroutine reads the Galileo external observation selection
!             (GEOS) file into the structure gobsdef.
!
! Author:     L. Prange
!
! Created:    26-Jan-2011
! Changes  :  16-Feb-2012 LP: GFS format change (receiver column as dummy)
!             29-May-2012 LP: Systems C and J added
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnLoc, lfnErr
  USE d_rinex3, ONLY: t_gobsdef

  USE s_opnfil
  USE s_opnerr
  USE s_exitrc
  IMPLICIT NONE


! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)             :: filename

! output:
  TYPE(t_gobsdef)              :: gobsdef ! Structure containing the
                                          ! external signal type
                                          ! selction for GIOVE, Galileo,
                                          ! and SBAS
  INTEGER(i4b)                 :: usegeos ! 1: use external signal type
                                          ! selction for GIOVE, Galileo,
                                          ! and SBAS;
                                          ! 0: don't use it

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8),PARAMETER   :: srName = 'READGEOS'

! Local Variables
! ---------------
  CHARACTER(LEN=100)           :: line
  INTEGER(i4b)                 :: ios
  INTEGER(i4b)                 :: iLin
  CHARACTER(LEN=20)            :: receiver


  usegeos=0
  iLin=0


! Open the geos file
! ------------------
  CALL OPNFIL(lfnloc,filename,'OLD','FORMATTED','READONLY',' ',ios)
  CALL OPNERR(lfnerr,lfnloc,ios,filename,srName)

! Read file and fill the structure
! --------------------------------
  READ (lfnloc,'(A)',iostat=ios) line
  READ (lfnloc,'(////)',iostat=ios)
  IF (line(1:50) ==                         &
      'Signal selection file for GIOVE, Galileo, and SBAS') THEN
      DO WHILE (ios == 0)
          READ (lfnloc,'(A)',iostat=ios) line
          IF (ios /= 0) CYCLE
          IF (LEN_TRIM(line) == 0) EXIT
          iLin=iLin+1
!          READ (line,'(A3,9X,A3,4X,A3,4X,A3,4X,A3)',iostat=ios)        &
          READ (line,'(A3,9X,4(A3,4X),A20)',iostat=ios)        &
               gobsdef%sat(iLin)%satname,gobsdef%sat(iLin)%obstyp(1),  &
               gobsdef%sat(iLin)%obstyp(2),gobsdef%sat(iLin)%obstyp(3),&
               gobsdef%sat(iLin)%obstyp(4),receiver
          gobsdef%norec = iLin

          gobsdef%sat(iLin)%syschar = gobsdef%sat(iLin)%satname(1:1)


          IF (gobsdef%sat(iLin)%syschar == 'G') gobsdef%sat(iLin)%sysnum = 0
          IF (gobsdef%sat(iLin)%syschar == 'R') gobsdef%sat(iLin)%sysnum = 1
          IF (gobsdef%sat(iLin)%syschar == 'E') gobsdef%sat(iLin)%sysnum = 2
          IF (gobsdef%sat(iLin)%syschar == 'S') gobsdef%sat(iLin)%sysnum = 3
          IF (gobsdef%sat(iLin)%syschar == 'C') gobsdef%sat(iLin)%sysnum = 4
          IF (gobsdef%sat(iLin)%syschar == 'J') gobsdef%sat(iLin)%sysnum = 5
          READ (gobsdef%sat(iLin)%satname(2:3),'(I2)') gobsdef%sat(iLin)%satnum

!         Check for consistency of Code and Phase observations
          IF ((gobsdef%sat(iLin)%obstyp(1)(2:2).NE. &
               gobsdef%sat(iLin)%obstyp(3)(2:2)).OR. &
           (gobsdef%sat(iLin)%obstyp(2)(2:2).NE. &
            gobsdef%sat(iLin)%obstyp(4)(2:2))) THEN
           WRITE(lfnerr,'(/,A,/,A,/,A3,/,A3,/,A3,/,A3,/,A3,/)')            &
                 ' *** SR READGEOS: Inconsistent Code and Phase obs types',&
                 filename,gobsdef%sat(iLin)%satname,                       &
                 gobsdef%sat(iLin)%obstyp(1),gobsdef%sat(iLin)%obstyp(2),  &
                 gobsdef%sat(iLin)%obstyp(3),gobsdef%sat(iLin)%obstyp(4)
           CALL EXITRC(2)
          ENDIF
      ENDDO
  ENDIF
  IF (gobsdef%norec > 0) usegeos=1
  CLOSE(lfnloc)

END SUBROUTINE readgeos

! ====================================


SUBROUTINE readgeos2(rinstat, rectyp, gobsdef, usegeos)

! -------------------------------------------------------------------------
! Purpose:    Select observation types to be used considering the
!             receiver-, system-, and satellite-dependent priorities
!             (new receiver or OBSSEL file) and the number of observations
!             actually available for each satellite in the RINEX file
!             (RINSTAT).
!
! Author:     L. Prange
!
! Created:    29-May-2012
! Changes:    07-Jun-2012 LP: Print selection results only if requested
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnErr, lfnPrt
  USE m_maxdim, ONLY: maxsat
  USE m_global, ONLY: maxsys
  USE d_rinex3, ONLY: t_gobsdef,maxtyp,t_rinstat,obstypesr3
  USE d_rcvr,   ONLY: maxTypr, rcvObs
  USE s_ckoptb
  USE s_exitrc
  IMPLICIT NONE


! List of Parameters
! ------------------
! input:
  TYPE(t_rinstat)     :: rinstat ! structure containing satellite-wise
                                 ! observation statistics for one RINEX
                                 ! file
  CHARACTER(LEN=20)   :: rectyp  ! Receiver type in RINEX file

! output:
  INTEGER(i4b)        :: usegeos ! 1: use sat-specific obstype info
                                 ! 0: don't
  TYPE(t_gobsdef)     :: gobsdef ! Structure containing the
                                 ! sat-specific obstype info

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=9),PARAMETER    :: srName = 'READGEOS2'

! Local Variables
! ---------------
  INTEGER(i4b)                  :: iusesat,usesat
  INTEGER(i4b)                  :: isys,isat,prn
  INTEGER(i4b)                  :: indL1,indL2,indC1,indC2
  INTEGER(i4b)                  :: rtyp,ityp
  INTEGER(i4b)                  :: numobsL1,numobsL2,numobsC1,numobsC2
  INTEGER(i4b)                  :: irCode,prtostat

  REAL(r8b)                     :: obsfac

  CHARACTER(LEN=3),DIMENSION(maxTypr) :: L1,L2,C1,C2
  CHARACTER(LEN=1),DIMENSION(36):: helpstr
  CHARACTER(LEN=20)             :: recsel = ""
  CHARACTER(LEN=1)              :: freqL1,freqL2,freqC1,freqC2

  LOGICAL                       :: skipsamefreq = .true.

! Initialization
! ==============
  irCode   = 0
  obsfac   = 0.6
  usegeos  = 0
  iusesat  = 0
  prtostat = 0


! Obstype selection
! =================
  DO isys=0,(MAXSYS-1)
   IF (rinstat%sys(isys)%syschar.eq.' ') CYCLE
   DO isat=1,49

    IF (rinstat%sys(isys)%sat(isat)%satname.eq.'   ') CYCLE
    prn = isys*100+isat
!
!   Read new (modified BSW6) receiver file with obstype priority list
!   -----------------------------------------------------------------
    CALL rcvObs(RECTYP,prn,L1,L2,C1,C2,RECSEL)
    indL1  = 0
    indL2  = 0
    indC1  = 0
    indC2  = 0
    usesat = 0
    freqL1 = ' '
    freqL2 = ' '
    freqC1 = ' '
    freqC2 = ' '
    numobsL1 = 0
    numobsL2 = 0
    numobsC1 = 0
    numobsC2 = 0

!   Find index of first phase obs
!   -----------------------------
    DO rtyp = maxTypr,1,-1
     IF (L1(rtyp).eq.'') CYCLE
     DO ityp=1,MAXTYP
      IF (rinstat%sys(isys)%indxs(ityp).eq.0) CYCLE
      IF (rinstat%sys(isys)%sat(isat)%numobs(ityp).eq.0) CYCLE
      IF (L1(rtyp).eq.OBSTYPESR3(ityp)) THEN
        freqL1 = L1(rtyp)(2:2)
        IF (freqL1.eq.' ') CYCLE
        IF (rinstat%sys(isys)%sat(isat)%numobs(ityp)>(obsfac*numobsL1)) THEN
          usesat = 1
          indL1  = ityp
          IF (rinstat%sys(isys)%sat(isat)%numobs(ityp)>numobsL1) &
              numobsL1 = rinstat%sys(isys)%sat(isat)%numobs(ityp)
        ELSE
          WRITE(lfnErr,'(2(/,2A,I6.6,A),/,2A,/)')                            &
            ' ### SR READGEOS2: Too few '//TRIM(L1(rtyp)),' observations (',rinstat%sys(isys)%sat(isat)%numobs(ityp),')', &
            '                   compared to the number of '//TRIM(OBSTYPESR3(indL1)),' observations (',numobsL1,')', &
            '                   for satellite '//TRIM(rinstat%sys(isys)%sat(isat)%satname),'.'
          EXIT
        ENDIF
      ENDIF
     ENDDO
    ENDDO

!   Find index of second phase obs
!   ------------------------------
    DO rtyp = maxTypr,1,-1
     IF (L2(rtyp).eq.'') CYCLE
     DO ityp=1,MAXTYP
      IF (rinstat%sys(isys)%indxs(ityp).eq.0) CYCLE
      IF (rinstat%sys(isys)%sat(isat)%numobs(ityp).eq.0) CYCLE
      IF (L2(rtyp).eq.OBSTYPESR3(ityp)) THEN
        freqL2 = L2(rtyp)(2:2)
        IF (freqL2.eq.' ') CYCLE
        IF (skipsamefreq.and.(freqL1.eq.freqL2)) THEN
          WRITE(lfnErr,'(3(/,A),/)')                               &
            ' ### SR READGEOS2: Phase observations of same frequency rejected.',&
            '                   Satellite:  '//TRIM(rinstat%sys(isys)%sat(isat)%satname),&
            '                   Frequency: L'//TRIM(freqL2)
          EXIT
        ELSE
          IF (rinstat%sys(isys)%sat(isat)%numobs(ityp)>(obsfac*numobsL2)) THEN
            usesat = 1
            indL2  = ityp
            IF (rinstat%sys(isys)%sat(isat)%numobs(ityp)>numobsL2) &
                numobsL2 = rinstat%sys(isys)%sat(isat)%numobs(ityp)
          ELSE
            WRITE(lfnErr,'(2(/,2A,I6.6,A),/,2A,/)')                            &
              ' ### SR READGEOS2: Too few '//TRIM(L2(rtyp)),' observations (',rinstat%sys(isys)%sat(isat)%numobs(ityp),')', &
              '                   compared to the number of '//TRIM(OBSTYPESR3(indL2)),' observations (',numobsL2,')', &
              '                   for satellite '//TRIM(rinstat%sys(isys)%sat(isat)%satname),'.'
            EXIT
          ENDIF
        ENDIF
      ENDIF
     ENDDO
    ENDDO

!   Find index of first code obs
!   ----------------------------
    DO rtyp = maxTypr,1,-1
     IF (C1(rtyp).eq.'') CYCLE
     DO ityp=1,MAXTYP
      IF (rinstat%sys(isys)%indxs(ityp).eq.0) CYCLE
      IF (rinstat%sys(isys)%sat(isat)%numobs(ityp).eq.0) CYCLE
      IF (C1(rtyp).eq.OBSTYPESR3(ityp)) THEN
        freqC1 = C1(rtyp)(2:2)
        IF (freqC1.eq.' ') CYCLE
        IF (rinstat%sys(isys)%sat(isat)%numobs(ityp)>(obsfac*numobsC1)) THEN
          usesat = 1
          indC1  = ityp
          IF (rinstat%sys(isys)%sat(isat)%numobs(ityp)>numobsC1) &
              numobsC1 = rinstat%sys(isys)%sat(isat)%numobs(ityp)
        ELSE
          WRITE(lfnErr,'(2(/,2A,I6.6,A),/,2A,/)')                            &
            ' ### SR READGEOS2: Too few '//TRIM(C1(rtyp)),' observations (',rinstat%sys(isys)%sat(isat)%numobs(ityp),')', &
            '                   compared to the number of '//TRIM(OBSTYPESR3(indC1)),' observations (',numobsC1,')', &
            '                   for satellite '//TRIM(rinstat%sys(isys)%sat(isat)%satname),'.'
          EXIT
        ENDIF
      ENDIF
     ENDDO
    ENDDO

!   Find index of second code obs
!   -----------------------------
    DO rtyp = maxTypr,1,-1
     IF (C2(rtyp).eq.'') CYCLE
     DO ityp=1,MAXTYP
      IF (rinstat%sys(isys)%indxs(ityp).eq.0) CYCLE
      IF (rinstat%sys(isys)%sat(isat)%numobs(ityp).eq.0) CYCLE
      IF (C2(rtyp).eq.OBSTYPESR3(ityp)) THEN
        freqC2 = C2(rtyp)(2:2)
        IF (freqC2.eq.' ') CYCLE
        IF (skipsamefreq.and.(freqC1.eq.freqC2)) THEN
          WRITE(lfnErr,'(3(/,A),/)')                               &
            ' ### SR READGEOS2: Code observations of same frequency rejected.', &
            '                   Satellite:  '//TRIM(rinstat%sys(isys)%sat(isat)%satname),&
            '                   Frequency: L'//TRIM(freqC2)
          EXIT
        ELSE
          IF (rinstat%sys(isys)%sat(isat)%numobs(ityp)>(obsfac*numobsC2)) THEN
            usesat = 1
            indC2  = ityp
            IF (rinstat%sys(isys)%sat(isat)%numobs(ityp)>numobsC2) &
                numobsC2 = rinstat%sys(isys)%sat(isat)%numobs(ityp)
          ELSE
            WRITE(lfnErr,'(2(/,2A,I6.6,A),/,2A,/)')                            &
              ' ### SR READGEOS2: Too few '//TRIM(C2(rtyp)),' observations (',rinstat%sys(isys)%sat(isat)%numobs(ityp),')', &
              '                   compared to the number of '//TRIM(OBSTYPESR3(indC2)),' observations (',numobsC2,')', &
              '                   for satellite '//TRIM(rinstat%sys(isys)%sat(isat)%satname),'.'
            EXIT
          ENDIF
        ENDIF
      ENDIF
     ENDDO
    ENDDO

    IF (usesat.EQ.0) CYCLE


!   Skip satellites with obstype defined on only one frequency
!   (To be changed here and in DEFREQ for single frequency receivers,
!    if required!!!,
!    => Single FREQ receivers can currently only be processed in RINEX2
!       mode and without OBSSEL-file!)
!   ----------------------------------------------------------------
    IF ((indL1.eq.0).or.(indL2.eq.0)) THEN
      indL1 = 0
      indL2 = 0
    ENDIF
    IF ((indC1.eq.0).or.(indC2.eq.0)) THEN
      indC1 = 0
      indC2 = 0
    ENDIF
    IF ((indL1.eq.0).and.(indL2.eq.0).and.(indC1.eq.0).and.(indC2.eq.0)) cycle

!   Fill up gobsdef
!   ---------------
    iusesat = iusesat + 1
    IF ((iusesat>rinstat%numsat).or.(iusesat>maxsat)) THEN
      WRITE(lfnerr,'(/,A,/,A,I3.3,/,A,I3.3,/,A,I3.3,/)')                   &
                 '*** SR RNXSTAT: Too many satellites indicated for use.', &
                 '    # of satellites to be used: ',iusesat,               &
                 '    # of satellites with observations: ',rinstat%numsat, &
                 '    maxsat: ',maxsat
      CALL EXITRC(2)
    ENDIF
    gobsdef%norec                 = iusesat
    gobsdef%sat(iusesat)%syschar  = rinstat%sys(isys)%syschar
    gobsdef%sat(iusesat)%sysnum   = isys
    gobsdef%sat(iusesat)%satname  = rinstat%sys(isys)%sat(isat)%satname
    gobsdef%sat(iusesat)%satnum   = isat
    IF (indC1.ne.0) gobsdef%sat(iusesat)%obstyp(1)=OBSTYPESR3(indC1)
    IF (indC2.ne.0) gobsdef%sat(iusesat)%obstyp(2)=OBSTYPESR3(indC2)
    IF (indL1.ne.0) gobsdef%sat(iusesat)%obstyp(3)=OBSTYPESR3(indL1)
    IF (indL2.ne.0) gobsdef%sat(iusesat)%obstyp(4)=OBSTYPESR3(indL2)

    IF (indC1.ne.0) gobsdef%sat(iusesat)%obsindR212(1)=indC1
    IF (indC2.ne.0) gobsdef%sat(iusesat)%obsindR212(2)=indC2
    IF (indL1.ne.0) gobsdef%sat(iusesat)%obsindR212(3)=indL1
    IF (indL2.ne.0) gobsdef%sat(iusesat)%obsindR212(4)=indL2
   ENDDO
  ENDDO
  IF (gobsdef%norec>0) usegeos = 1

!
! Check if statistics should be printed
! =====================================
  CALL ckoptb(1,(/'PRTOSTAT'/),srname,                        &
                'Print RINEX observation statistics?',irCode, &
                result1=prtostat)

! Stop if the input value is corrupt
! ----------------------------------
  IF (irCode /= 0) CALL exitrc(2)

!
! Print selected obstypes if requested
! ====================================
  IF ((usegeos.EQ.1).AND.(prtostat.EQ.1)) THEN
    WRITE(lfnPrt,'(2(/,A),/)')                                         &
            ' SR READGEOS2: Receiver in RINEX file: ' // TRIM(rectyp), &
            '               Selected receiver:      ' // TRIM(recsel)

    helpstr(:) = '*'
    WRITE(lfnprt,'(2(/,A))')' Selected observation types: ', &
                            ' --------------------------- '
    WRITE(lfnprt,'(/,36A1)')(helpstr(ityp),ityp=1,36)
    WRITE(lfnprt,'(A4,4A8)')'SAT ',' Code1  ',' Code2  ',' Phase1 ',' Phase2'
    WRITE(lfnprt,'(36A1)')(helpstr(ityp),ityp=1,36)
    DO isat = 1,gobsdef%norec
      WRITE(lfnprt,'(A4,4(3X,A3,2X))') gobsdef%sat(isat)%satname, &
           (gobsdef%sat(isat)%obstyp(ityp),ityp=1,4)
    ENDDO
    WRITE(lfnprt,'(36A1,/)')(helpstr(ityp),ityp=1,36)
  ENDIF

END SUBROUTINE readgeos2


! ====================================


SUBROUTINE setgeos(head, gobsdef, usegeos)

! -------------------------------------------------------------------------
! Purpose:    This subroutine reads information from the observation file
!             header into the structure gobsdef.
!
! Author:     L. Prange
!
! Created:    26-Jan-2011
!
! Changes:    24-Apr-2012 LP: loop over obstyp changed (4->8)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b
  USE d_rinex3, ONLY: t_gobsdef
  USE d_gpsObs, ONLY: t_obsHead
  USE m_global, ONLY: g_svnsys

  IMPLICIT NONE


! List of Parameters
! ------------------
! input:
  TYPE(t_obsHead)              :: head    ! BSW obs. file header


! output:
  TYPE(t_gobsdef)              :: gobsdef ! Structure containing the
                                          ! external signal type
                                          ! selction for GIOVE, Galileo,
                                          ! and SBAS
  INTEGER(i4b)                 :: usegeos ! 1: use external signal type
                                          ! selction for GIOVE, Galileo,
                                          ! and SBAS;
                                          ! 0: don't use it

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8),PARAMETER   :: srName = 'SETGEOS'

! Local Variables
! ---------------
  INTEGER(i4b)                 :: irec
  INTEGER(i4b)                 :: iSat
  INTEGER(i4b)                 :: obs
  INTEGER(i4b)                 :: hit

! Read the structure "head" and fill the structure "gobsdef"
! ---------------------------------------------------------
  usegeos=0
  irec=0

  DO iSat = 1,head%nSatel
    hit = 0
    DO obs = 1,8
     IF (head%sat(iSat)%obstyp(obs).NE.'   ') THEN
         hit = 1
         EXIT
     ENDIF
    ENDDO

    IF (hit == 1) THEN
       irec=irec+1
       gobsdef%sat(irec)%satnum = MOD(head%sat(iSat)%numSat,100) ! 1..32..49
       gobsdef%sat(irec)%sysnum = &
            (head%sat(iSat)%numSat-gobsdef%sat(irec)%satnum)/100 ! 0..5
       gobsdef%sat(irec)%syschar = g_svnsys(gobsdef%sat(irec)%sysnum)

       DO obs = 1,8
          gobsdef%sat(irec)%obstyp(obs) = head%sat(iSat)%obstyp(obs)
       ENDDO
    ENDIF
  ENDDO
  gobsdef%norec = irec

  IF (gobsdef%norec > 0) usegeos=1

END SUBROUTINE setgeos


! ====================================

SUBROUTINE setgeos2(obst, SVN, gobsdef, usegeos)

! -------------------------------------------------------------------------
! Purpose:    This subroutine reads information from the NEQ file
!             MISC part into the structure gobsdef.
!
! Author:     L. Prange
!
! Created:    20-Jul-2011
!
! Changes:    DD-MMM-YYYY NN:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b
  USE d_rinex3, ONLY: t_gobsdef
  USE d_gpsObs, ONLY: t_obsHead
  USE d_neq,    ONLY: t_obst
  USE m_global, ONLY: g_svnsys

  IMPLICIT NONE


! List of Parameters
! ------------------
! input:
  TYPE(t_obst)                 :: obst    ! Sat-specific obstype data set
  INTEGER(i4b)                 :: SVN

! output:
  TYPE(t_gobsdef)              :: gobsdef ! Structure containing the
                                          ! external signal type
                                          ! selction for GIOVE, Galileo,
                                          ! and SBAS
  INTEGER(i4b)                 :: usegeos ! 1: use external signal type
                                          ! selction for GIOVE, Galileo,
                                          ! and SBAS;
                                          ! 0: don't use it
! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8),PARAMETER   :: srName = 'SETGEOS2'

! Local Variables
! ---------------
  INTEGER(i4b)                 :: irec
  INTEGER(i4b)                 :: obs
  INTEGER(i4b)                 :: hit

! Read the structure "obst" and fill the structure "gobsdef"
! ---------------------------------------------------------
  usegeos=0
  irec=0
  hit = 0

! Check whether there is data in the requested data set
  DO obs = 1,4
     IF (obst%obstyp(obs).NE.'   ') THEN
         hit = 1
         EXIT
     ENDIF
  ENDDO

  IF (hit == 1) THEN
    irec=irec+1
    gobsdef%sat(irec)%satnum = MOD(SVN,100) ! 1..32
    gobsdef%sat(irec)%sysnum = (SVN-gobsdef%sat(irec)%satnum)/100 ! 0..3

    gobsdef%sat(irec)%syschar = g_svnsys(gobsdef%sat(irec)%sysnum)
!    WRITE(*,*) 'SETGEOS2: SYSNUM, SYSCHAR, SATNUM: ', &
!         gobsdef%sat(irec)%sysnum ,gobsdef%sat(irec)%syschar, &
!         gobsdef%sat(irec)%satnum

    DO obs = 1,4
      gobsdef%sat(irec)%obstyp(obs) = obst%obstyp(obs)
    ENDDO
  ENDIF
!  WRITE(*,*) 'SETGEOS2: OBSTYP: ', gobsdef%sat(irec)%obstyp(1), &
!       gobsdef%sat(irec)%obstyp(2), gobsdef%sat(irec)%obstyp(3), &
!       gobsdef%sat(irec)%obstyp(4)
  gobsdef%norec = irec

  IF (gobsdef%norec > 0) usegeos=1

END SUBROUTINE setgeos2


! ====================================
!
!SUBROUTINE setgeos3(obst, gobsdef, usegeos)
!
!! -------------------------------------------------------------------------
!! Purpose:    This subroutine streams information from the NEQ file
!!             MISC part into the structure gobsdef without checking the
!!             time window.
!!
!! Author:     L. Prange
!!
!! Created:    18-Jul-2011
!! Last mod.:
!!
!! Changes:    DD-MMM-YYYY NN:
!!
!! Copyright:  Astronomical Institute
!!             University of Bern
!!             Switzerland
!! -------------------------------------------------------------------------
!
!! Modules
!! -------
!  USE m_bern,   ONLY: i4b, r8b
!  USE d_rinex3, ONLY: t_gobsdef
!  USE d_gpsObs, ONLY: t_obsHead
!  USE d_neq,    ONLY: t_obst
!  USE m_global, ONLY: g_svnsys
!
!  IMPLICIT NONE
!
!
!! List of Parameters
!! ------------------
!! input:
!  TYPE(t_obst)                 :: obst    ! Sat-specific obstype data set
!
!
!! output:
!  TYPE(t_gobsdef)              :: gobsdef ! Structure containing the
!                                          ! external signal type
!                                          ! selction for GIOVE, Galileo,
!                                          ! and SBAS
!  INTEGER(i4b)                 :: usegeos ! 1: use external signal type
!                                          ! selction for GIOVE, Galileo,
!                                          ! and SBAS;
!                                          ! 0: don't use it
!
!! List of Functions
!! -----------------
!
!! Local Types
!! -----------
!
!! Local Parameters
!! ----------------
!  CHARACTER(LEN=8),PARAMETER   :: srName = 'SETGEOS3'
!
!! Local Variables
!! ---------------
!  INTEGER(i4b)                 :: irec
!  INTEGER(i4b)                 :: obs
!  INTEGER(i4b)                 :: hit
!
!! Read the structure "obst" and fill the structure "gobsdef"
!! ---------------------------------------------------------
!  usegeos=0
!  irec=0
!  hit = 0
!
!! Check whether there is a satellite and data in the requested data set
!  IF (obst%sat>0) THEN
!    DO obs = 1,4
!     IF (obst%obstyp(obs).NE.'   ') THEN
!         hit = 1
!         EXIT
!     ENDIF
!    ENDDO
!  ENDIF
!
!  IF (hit == 1) THEN
!    irec=irec+1
!    gobsdef%sat(irec)%satnum = MOD(obst%sat,100) ! 1..32
!    gobsdef%sat(irec)%sysnum = (obst%sat-gobsdef%sat(irec)%satnum)/100 ! 0..3
!
!!       IF (gobsdef%sat(irec)%sysnum.EQ.0) gobsdef%sat(irec)%syschar = 'G'
!!       IF (gobsdef%sat(irec)%sysnum.EQ.1) gobsdef%sat(irec)%syschar = 'R'
!!       IF (gobsdef%sat(irec)%sysnum.EQ.2) gobsdef%sat(irec)%syschar = 'E'
!!       IF (gobsdef%sat(irec)%sysnum.EQ.3) gobsdef%sat(irec)%syschar = 'S'
!
!    gobsdef%sat(irec)%syschar = g_svnsys(gobsdef%sat(irec)%sysnum)
!    WRITE(*,*) 'SETGEOS3, SYSNUM, SYSCHAR: ',gobsdef%sat(irec)%sysnum , &
!                gobsdef%sat(irec)%syschar
!
!    DO obs = 1,4
!      gobsdef%sat(irec)%obstyp(obs) = obst%obstyp(obs)
!    ENDDO
!  ENDIF
!  gobsdef%norec = irec
!
!  IF (gobsdef%norec > 0) usegeos=1
!
!END SUBROUTINE setgeos3
!
!
!! ======================
!
!
!SUBROUTINE setgeos4(misc, timint, gobsdef, usegeos)
!
!! -------------------------------------------------------------------------
!! Purpose:    This subroutine streams information from the NEQ file
!!             MISC part into the structure gobsdef.
!!
!! Author:     L. Prange
!!
!! Created:    18-Jul-2011
!! Last mod.:
!!
!! Changes:    DD-MMM-YYYY NN:
!!
!! Copyright:  Astronomical Institute
!!             University of Bern
!!             Switzerland
!! -------------------------------------------------------------------------
!
!! Modules
!! -------
!  USE m_bern,   ONLY: i4b, r8b
!  USE d_rinex3, ONLY: t_gobsdef
!  USE d_gpsObs, ONLY: t_obsHead
!  USE m_time,   ONLY: t_timint
!  USE d_neq,    ONLY: t_misc
!  USE m_global, ONLY: g_svnsys
!
!  IMPLICIT NONE
!
!
!! List of Parameters
!! ------------------
!! input:
!  TYPE(t_misc)                 :: misc    ! NEQ misc info
!  TYPE(t_timint)               :: timint  ! Time interval
!
!
!! output:
!  TYPE(t_gobsdef)              :: gobsdef ! Structure containing the
!                                          ! external signal type
!                                          ! selction for GIOVE, Galileo,
!                                          ! and SBAS
!  INTEGER(i4b)                 :: usegeos ! 1: use external signal type
!                                          ! selction for GIOVE, Galileo,
!                                          ! and SBAS;
!                                          ! 0: don't use it
!
!! List of Functions
!! -----------------
!
!! Local Types
!! -----------
!
!! Local Parameters
!! ----------------
!  CHARACTER(LEN=8),PARAMETER   :: srName = 'SETGEOS4'
!
!! Local Variables
!! ---------------
!  INTEGER(i4b)                 :: irec
!  INTEGER(i4b)                 :: iSat
!  INTEGER(i4b)                 :: obs
!  INTEGER(i4b)                 :: hit
!
!! Read the structure "head" and fill the structure "gobsdef"
!! ---------------------------------------------------------
!  usegeos=0
!  irec=0
!
!  DO iSat = 1,misc%nobst
!   hit = 0
!!  Check whether there is a SVN number in the record and whether the record
!!  is relevant for the time interval
!   IF ((misc%obst(iSat)%sat>0).AND.(timint%t(1)>= &
!        misc%obst(iSat)%timint%t(1)) &
!       .AND.(timint%t(2)<=misc%obst(iSat)%timint%t(2))) THEN
!    DO obs = 1,4
!     IF (misc%obst(iSat)%obstyp(obs).NE.'   ') THEN
!         hit = 1
!         EXIT
!     ENDIF
!    ENDDO
!   ENDIF
!
!   IF (hit == 1) THEN
!       irec=irec+1
!       gobsdef%sat(irec)%satnum = MOD(misc%obst(iSat)%sat,100) ! 1..32
!       gobsdef%sat(irec)%sysnum = (misc%obst(iSat)%sat- &
!                                 gobsdef%sat(irec)%satnum)/100 ! 0..3
!
!!       IF (gobsdef%sat(irec)%sysnum.EQ.0) gobsdef%sat(irec)%syschar = 'G'
!!       IF (gobsdef%sat(irec)%sysnum.EQ.1) gobsdef%sat(irec)%syschar = 'R'
!!       IF (gobsdef%sat(irec)%sysnum.EQ.2) gobsdef%sat(irec)%syschar = 'E'
!!       IF (gobsdef%sat(irec)%sysnum.EQ.3) gobsdef%sat(irec)%syschar = 'S'
!
!       gobsdef%sat(irec)%syschar = g_svnsys(gobsdef%sat(irec)%sysnum)
!       WRITE(*,*) 'SETGEOS4, SYSNUM, SYSCHAR: ',gobsdef%sat(irec)%sysnum ,&
!                   gobsdef%sat(irec)%syschar
!
!       DO obs = 1,4
!          gobsdef%sat(irec)%obstyp(obs) = misc%obst(iSat)%obstyp(obs)
!       ENDDO
!   ENDIF
!  ENDDO
!  gobsdef%norec = irec
!
!  IF (gobsdef%norec > 0) usegeos=1
!
!END SUBROUTINE setgeos4
!
! ====================================

SUBROUTINE tstobst(misc, obst_1, indobst_new)

! -------------------------------------------------------------------------
! Purpose:    The sr checks whether obstypes belonging to a new parameter
!             are already stored in the misc part of a stacked neq.
!             IF yes: returns the index of the obstype info in the stacked
!                     neq's par%locq.
!             IF not: updates obstype info in the stacked neq%misc and
!                     returns the index of the new obstype data set.
!
! Author:     L. Prange
!
! Created:    20-Jul-2011
! Last mod.:
!
! Changes:    DD-MMM-YYYY NN:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b
  USE d_neq,    ONLY: t_misc, t_obst

  IMPLICIT NONE


! List of Parameters
! ------------------
! input:
  TYPE(t_obst)          :: obst_1     ! Sat-specific obstype info of new
                                      ! parameter

! output:
  INTEGER(i4b)          :: indobst_new! Obstype index of new parameter
                                      ! (to be used in misc%obst)

! in/out:
  TYPE(t_misc)          :: misc       ! Misc info of stacked NEQ

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8),PARAMETER   :: srName = 'TSTOBST'

! Local Variables
! ---------------
  INTEGER(i4b)                 :: iobst, diffobst, obs

! Read each record of the misc%obst structure of the stacked NEQ and compare
! it to the obstypes belonging to the new parameter
! --------------------------------------------------------------------------
  indobst_new = 0

  DO iobst = 1,misc%nobst
    diffobst = 0
    DO obs = 1,4
      IF (misc%obst(iobst)%obstyp(obs).NE.obst_1%obstyp(obs)) THEN
        diffobst = 1
        EXIT
      ENDIF
    ENDDO
    IF (diffobst == 0) THEN
      indobst_new = iobst
      EXIT
    ENDIF
  ENDDO

! In case of new obstypes for the new parameter -> update the misc%obst
! structure of the stacked NEQ and increase misc%nobst
! ---------------------------------------------------------------------
  IF (indobst_new == 0) THEN
    misc%nobst = misc%nobst + 1
    indobst_new = misc%nobst
    misc%obst(indobst_new) = obst_1
  ENDIF


END SUBROUTINE tstobst

! ----------------------------------------------------------------------
! ====================================

SUBROUTINE init_rinstat(rinstat)

! -------------------------------------------------------------------------
! Purpose:    Initialization of the rinstat structure.
!
! Author:     L. Prange
!
! Created:    09-May-2012
!
! Changes:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b
  USE m_global, ONLY: MAXSYS
  USE d_rinex3, ONLY: t_rinstat, MAXTYP
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:

! output:
  TYPE(t_rinstat)              :: rinstat ! Structure for collecting
                                          ! satellite-specific observation
                                          ! statistics for a RINEX obsfile

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=12),PARAMETER  :: srName = 'init_rinstat'

! Local Variables
! ---------------
  INTEGER(i4b)                 :: isat, isys

! =========================================
! Initialize and allocate structure
! =========================================

  IF ((MAXTYP>0).AND.(MAXSYS>0)) THEN
    rinstat%numsat    = 0
    rinstat%obssum    = 0
    rinstat%numobs(:) = 0

    DO isys=0,(MAXSYS-1)
      rinstat%sys(isys)%syschar     = ' '
      rinstat%sys(isys)%sysnum      = 0
      rinstat%sys(isys)%obssum      = 0
      rinstat%sys(isys)%indxs(:)    = 0
      rinstat%sys(isys)%numobs(:)   = 0

      DO isat=1,49
        rinstat%sys(isys)%sat(isat)%satname     = '   '
        rinstat%sys(isys)%sat(isat)%satnum      = 0
        rinstat%sys(isys)%sat(isat)%eposatind   = 0
        rinstat%sys(isys)%sat(isat)%obssum      = 0
        rinstat%sys(isys)%sat(isat)%numobs(:)   = 0
      ENDDO
    ENDDO
  ELSE
    WRITE(*,*) 'SR: ',srName,': MAXTYP(',&
                    MAXTYP,') or MAXSYS(',MAXSYS,') too small.'
    CALL EXITRC(2)
  ENDIF
END SUBROUTINE init_rinstat

! ====================================

END MODULE
