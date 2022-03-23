MODULE s_APRTRANS
CONTAINS


! -------------------------------------------------------------------------
! Bernese GNSS Software Version 5.2
! -------------------------------------------------------------------------

SUBROUTINE aprtrans(neq,ipart,iFil,neq0)

! -------------------------------------------------------------------------
! Purpose:    This subroutine performs the transformation of the entire
!             NEQ system due to (possible) changes of a priori values of
!             the estimated parameters
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
!
! Changes:    04-Aug-1999 PF: Use SR GETCOO (new version)
!             09-Mar-1999 LM: Warning instead of exit if station not found
!             26-Jun-2001 RD: Use alcerr for allocation
!             06-Sep-2001 RD: Use real dynamic coord arrays
!             17-Sep-2001 RD: Make sure that maxStaLoc > null!!
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             26-Mar-2002 CU: Get a priori geocenter coordinates
!             27-Mar-2002 SS: Retransform DCB parameters to zero values
!             07-May-2002 SS: Consider DCB input values
!             27-Feb-2003 HU: DATUM from D_DATUM
!             07-Jul-2003 MM: A priori tropo possible (+delete parameters)
!             18-Dec-2003 MM: Consider vel in case of fixed stations
!             08-Oct-2004 SS,MM,HU: A priori transf. for velocities corrected
!             07-Apr-2005 RD: A priori for sao (type 12) and sap (type 25)
!             10-May-2005 RD: No not transform "sta-crux" parameters
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             09-Nov-2005 AG: SENNUM for GTSATA & GPHECC CALLs ADDED
!             01-Mar-2006 HU: SATELLIT file is optional
!             16-Feb-2006 HU: CHKSAO decommented
!             15-May-2006 HU: No stop after CHKSAO
!             24-Aug-2006 RD: Use GETCO3 instead of GETCOO
!             04-Aug-2006 AG: Implementations for satellite specific antenna
!                             PCO/PCV
!             13-Nov-2006 AG: Hardwired L4 satellite antenna offset
!             18-Jan-2007 AG: GTSATA replaced by GTSENSOR
!             07-May-2007 AG: Check wheter a satellite is active or not
!             12-Jun-2007 AG: Use sat_pcv instead of gphecc
!             13-Aug-2007 AG: Call defreq for sat_pcv
!             26-Jan-2008 RD: Add RAO/RAP parameters
!             13-Feb-2008 RD: Adapt to par%name == chr*20
!             15-Mar-2009 RD: Manage GLONASS frequency change
!             04-May-2009 RD: Scaling of loading models added
!             24-May-2009 RD/DT: Correct error message for Sat.Ant.Offsets
!             10-May-2009 RD: Receiver clock offsets/biases implemented
!             28-May-2009 DT: Get a priori values for Range biases (GETRGB)
!             10-Jun-2009 RD: Add epoch satellite/receiver clocks
!             14-Aug-2009 SL: isStaCrx removed from CASE 6
!             20-Aug-2009 LO: Leap-second problem fixed
!             04-Jan-2010 SL: HOI scaling parameters (CASE 27) added
!             15-Oct-2010 RD: hlp does not need to be an array
!             02-Dec-2010 DT: apriori values for Helmert parameters
!             15-Dec-2010 MM: A priori values from EST file
!             19-Jan-2011 RD: Flag to GTVELO added
!             03-May-2011 RD: Use "srName" at all locations
!             03-May-2011 RD: Change "iTyp"->"iReq" in parameter type 28
!             22-Jun-2011 RD: Bugfix for block-sie SAO/SAP
!             20-Jul-2011 LP: Sat-spec. obstypes; call sr setgeos2
!             09-Sep-2011 HU/PS: Leap second bug hopefully fixed
!             20-Jan-2012 RD: Reset apriori value to the solution (partyp=2)
!             16-Jul-2013 RD: Correct sign for introducing clock RINEX files
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_global, ONLY: g_syssvn
  USE m_time,   ONLY: t_timint
  USE m_epoch,  ONLY: t_epoch, OPERATOR(.RealToEpoch.)
  USE d_isbFil, ONLY: getIsb
  USE d_const,  ONLY: pi,C
  USE d_datum,  ONLY: datum
  USE d_neq,    ONLY: t_neq
  USE d_satfil, ONLY: typeMWTR,typeSLR
  USE d_phaecc, ONLY: init_buf,sat_pcv,sta_off,sta_pcv,antinfo,recant
  USE d_rgbfil, ONLY: t_rgb
  USE p_addneq, ONLY: opt, hlmFil
  USE d_rinex3, ONLY: t_gobsdef

  USE f_ikf
  USE s_chksao
  USE s_defreq
  USE s_prparlst
  USE s_getgcc
  USE s_alcerr
  USE f_lincount
  USE s_gtvelo
  USE s_gtsensor
  USE s_gettrp
  USE f_gtaprpol
  USE s_dcbcor
  USE s_getco3
  USE f_ut1_ut1r
  USE s_exitrc
  USE s_gtflna
  USE s_gtrxck
  USE s_gtsclk
  USE s_getdat
  USE s_svn2prn
  USE s_prn2svn
  USE s_getrgb
  USE s_getEst
  USE s_gobsdef,ONLY: init_geos, setgeos2
  USE s_satblk
  USE f_isstacrx
  USE f_tstequiv
  IMPLICIT NONE

! List of Parameters
! ------------------
! input/output:
  TYPE(t_neq)           :: neq    ! Normal Equation Structure (see D_NEQ)

! input
  INTEGER(i4b)          :: ipart  ! Part of the ADDNEQ2 program (1 or 2)
  INTEGER(i4b)          :: iFil   ! NQ0 file number
  TYPE(t_neq),OPTIONAL  :: neq0   ! Normal Equation Structure (see D_NEQ)

! Local types
! -----------
  TYPE t_rot
    REAL(r8b) :: val
    LOGICAL   :: flag
  END TYPE t_rot

  TYPE t_sata
    LOGICAL                         :: first
    INTEGER(i4b)                    :: doit
    CHARACTER(LEN=fileNameLength)   :: satFil ! Name of the SATELL file
  END TYPE t_sata

  TYPE t_satp
    LOGICAL                         :: first
    INTEGER(i4b)                    :: doit
    CHARACTER(LEN=fileNameLength)   :: phsFil ! Name of the PHASECC file
  END TYPE t_satp


! Local parameters
! ----------------
  REAL(r8b),         PARAMETER                   :: dtSim = 0.5d0/86400d0
  CHARACTER(LEN=8),  PARAMETER                   :: srName = 'aprtrans'


! List of functions
! -----------------


! Local Variables
! ---------------
  TYPE(t_sata),      SAVE                        :: sata
  TYPE(t_satp),      SAVE                        :: satp
  TYPE(t_timint)                                 :: timint ! Time intervals (MJD)
  TYPE(t_epoch)                                  :: epoch
  TYPE(t_rgb)                                    :: rgbReq
  TYPE(t_rot), DIMENSION(5,2),SAVE               :: rot
  TYPE(t_gobsdef)                                :: gobsdef

  CHARACTER(LEN=fileNameLength)                  :: filNam = " "
  CHARACTER(LEN=fileNameLength)                  :: filHlp = " "
  CHARACTER(LEN=staNameLength),                   &
                     DIMENSION(:), POINTER       :: stname
  CHARACTER(LEN=4)                               :: svnnr
  CHARACTER(LEN=1),  DIMENSION(1)                :: flags
  CHARACTER(LEN=1),  DIMENSION(:),   ALLOCATABLE :: velflg

  REAL(r8b),         DIMENSION(:,:), POINTER     :: xstat
  REAL(r8b),         DIMENSION(:,:), ALLOCATABLE :: xvel
  REAL(r8b)                                      :: hlp
  REAL(r8b),         DIMENSION(neq%misc%npar)    :: dx0
  REAL(r8b)                                      :: x0_old
  REAL(r8b)                                      :: x0_new
  REAL(r8b)                                      :: timcrd
  REAL(r8b)                                      :: crdnew
  REAL(r8b),         DIMENSION(3)                :: gccval0
  REAL(r8b),         DIMENSION(3)                :: siggcc
  REAL(r8b)                                      :: clock
  REAL(r8b)                                      :: sigma
  REAL(r8b),DIMENSION(3)                         :: drTrop
  REAL(r8b)                                      :: timmin
  REAL(r8b)                                      :: timmax
  REAL(r8b)                                      :: dazi, dnad
  REAL(r8b)                                      :: azi, nad
  REAL(r8b), DIMENSION(3)                        :: elvcor
  REAL(r8b), DIMENSION(3)                        :: antoff
  REAL(r8b), DIMENSION(3)                        :: offset
  REAL(r8b)                                      :: corr
  REAL(r8b)                                      :: zen
  REAL(r8b)                                      :: tprev
  REAL(r8b)                                      :: ut1prev

  INTEGER(i4b)                                   :: nstat
  INTEGER(i4b)                                   :: nflag
  INTEGER(i4b)                                   :: ipol
  INTEGER(i4b)                                   :: ider
  INTEGER(i4b)                                   :: ipar
  INTEGER(i4b)                                   :: ipar1
  INTEGER(i4b)                                   :: ip
  INTEGER(i4b)                                   :: kk
  INTEGER(i4b)                                   :: istat
  INTEGER(i4b)                                   :: icrd
  INTEGER(i4b)                                   :: ircvel
  INTEGER(i4b)                                   :: inuv1a
  INTEGER(i4b)                                   :: iac, irc
  INTEGER(i4b)                                   :: irCode
  INTEGER(i4b)                                   :: iTyp
  INTEGER(i4b)                                   :: iReq
  INTEGER(i4b)                                   :: iTrpMd
  INTEGER(i4b)                                   :: iTrMap
  INTEGER(i4b)                                   :: iTrGrd
  INTEGER(i4b)                                   :: ircTrp
  INTEGER(i4b)                                   :: iFrq
  INTEGER(i4b)                                   :: blknr
  INTEGER(i4b)                                   :: iGrd
  INTEGER(i4b)                                   :: iHoi
  INTEGER(i4b)                                   :: iSao
  INTEGER(i4b)                                   :: iCor
  INTEGER(i4b)                                   :: iSaoff, nsao, nsap
  INTEGER(i4b)                                   :: iSap
  INTEGER(i4b)                                   :: iFreq
  INTEGER(i4b)                                   :: inad,iazi
  INTEGER(i4b)                                   :: iadmax
  INTEGER(i4b)                                   :: grpnr, iGrp
  INTEGER(i4b)                                   :: frq, prn, cor
  INTEGER(i4b)                                   :: izen, nzen, nazi
  INTEGER(i4b)                                   :: iAnt
  INTEGER(i4b),SAVE                              :: ircAnt
  INTEGER(i4b)                                   :: rgbFound
  INTEGER(i4b)                                   :: usegeos
  INTEGER(i4b)                                   :: usegeos1
  INTEGER(i4b)                                   :: indobst

  LOGICAL                                        :: polreqfl
  LOGICAL                                        :: statOK
  LOGICAL,           DIMENSION(:),   ALLOCATABLE :: vfound
  LOGICAL, SAVE                                  :: first = .TRUE.

! Initialize rot structure
! ------------------------
  IF (first) THEN
    first = .FALSE.
    DO kk = 1, 5
      rot(kk,1)%val  = 0.d0
      rot(kk,1)%flag = .TRUE.
      rot(kk,2)%val  = 0.d0
      rot(kk,2)%flag = .TRUE.
    END DO
    sata%first = .TRUE.
    satp%first = .TRUE.
    CALL gtflna(0,'PHASECC',filhlp,ircAnt)
  END IF

! Station coordinates and velocities
! ----------------------------------
  NULLIFY(stname)
  NULLIFY(xstat)

  nflag    = 1
  flags(1) = '@'
  timcrd   = 0.d0
  CALL getco3(opt%coord, nflag, flags, nstat, stname, xstat= xstat, &
              timcrd= timcrd, datum=datum%name)

  CALL getdat(datum%name, datum%aell, datum%bell, datum%dxell, &
              datum%drell, datum%scell)


  ALLOCATE(xvel(3,nStat), stat=iac)
  CALL alcerr(iac, 'xvel', (/3,nStat/), srName)
  ALLOCATE(vfound(nStat), stat=iac)
  CALL alcerr(iac, 'vfound', (/nStat/), srName)
  ALLOCATE(velflg(nStat), stat=iac)
  CALL alcerr(iac, 'velflg', (/nStat/), srName)

  inuv1a = 0
  xvel   = 0.d0

  CALL gtvelo('VELAPR ', inuv1a, nflag, flags, nstat, xstat, stname, &
               xvel, velflg, vfound, ircvel)

! Are there any transformation requests for EOP parameters ?
! ----------------------------------------------------------
  polreqfl = .FALSE.

  IF (ipart == 1) THEN
    DO ireq = 1, SIZE(opt%req)
      IF ( opt%req(ireq)%locq(1) == 10 ) THEN
        polreqfl = .TRUE.
        EXIT
      END IF
    END DO
  END IF

! Initialize dx0 array
! --------------------
  dx0 = 0.d0

! Loop over all parameters
! ------------------------
  DO ipar=1,neq%misc%npar
    iTyp = neq%par(iPar)%locq(1)


! Transformation requests due to EST file
! ---------------------------------------
    IF (TRIM(opt%paramIn) /= "" .AND. iTyp == 30) THEN
      CALL getEst(opt%paramIn,neq%par(iPar),x0_new)
      dx0(iPar)        = x0_new-neq%par(iPar)%x0
      neq%par(iPar)%x0 = x0_new
      CYCLE
    ENDIF


! Other transformation requests
! -----------------------------
    SELECT CASE( iTyp )

! Station coordinates
! -------------------
    CASE (1)
      statOK = .FALSE.
      DO istat = 1, nstat
        IF (neq%par(ipar)%name(1:staNameLength) == stname(istat)) THEN
          statOK = .TRUE.
          icrd             = neq%par(ipar)%locq(3)
          IF (timcrd /= 0.d0) THEN
            crdnew = xstat(icrd,istat) + &
              xvel(icrd,istat) * (neq%par(ipar)%time%mean - timcrd) / 365.25d0
          ELSE
            crdnew = xstat(icrd,istat)
          END IF
          dx0(ipar)        = crdnew - neq%par(ipar)%x0
          IF (ipart==2) dx0(ipar) = 0D0
          neq%par(ipar)%x0 = crdnew
        END IF
      END DO
      IF (.NOT. statOK) THEN
        WRITE(lfnerr,*) ' *** aprtrans: station not found in a priori', &
                        ' coordinate file: ', TRIM(neq%par(ipar)%name)
      END IF


! Frequency/Satellite specific receiver clock biases
! --------------------------------------------------
    CASE(2)
      x0_old = neq%par(ipar)%x0
      IF (neq%par(ipar)%locq(6) == 1) THEN
        CALL dcbcor(2,0,0,neq%par(ipar)%name,'',neq%par(ipar)%locq(4), &
                    5,neq%par(ipar)%time%mean,neq%par(ipar)%x0)
      ELSE IF (neq%par(ipar)%locq(6) == 2) THEN
        CALL dcbcor(2,0,neq%par(ipar)%locq(4),neq%par(ipar)%name,'',0, &
                    5,neq%par(ipar)%time%mean,neq%par(ipar)%x0)
      ELSE IF (neq%par(ipar)%locq(6) == 5) THEN
        neq%par(ipar)%x0 = getIsb(0,neq%par(ipar)%name, &
                           neq%par(ipar)%time%mean,neq%par(ipar)%locq(3),irc)
        IF (irc /= 0) neq%par(ipar)%x0 = x0_old *1D9/C
      ENDIF
      neq%par(ipar)%x0 = neq%par(ipar)%x0 * C/1D9
      dx0(iPar) = neq%par(ipar)%x0-x0_old


! Receiver antenna offsets
! ------------------------
    CASE (5)
      IF (ircAnt /= 0) CYCLE

      prn = MOD(neq%par(iPar)%locq(5) , 100) * 100
      IF (prn > 999 ) prn = 0

      frq = MOD(neq%par(iPar)%locq(4) , 100)
      cor = neq%par(iPar)%locq(4) / 100

      CALL sta_off(neq%par(iPar)%name,neq%par(iPar)%locq(3), &
                   '(estimated)     ', prn,frq,offset=offset)

      dx0(ipar)        = offset(cor) - neq%par(ipar)%x0
      neq%par(ipar)%x0 = offset(cor)


! Troposphere parameters
! ----------------------
    CASE (6)
      CALL gettrp(filNam,neq%par(iPar)%time%mean,neq%par(iPar)%name(1:16),  &
                  0,1,iTrpMd,iTrMap,iTrGrd,drTrop,ircTrp)
      IF (ircTrp/=0 .OR. SUM(drTrop)==0.d0) CYCLE
      IF (iTrGrd/=neq%misc%iTrGrd .AND. neq%par(iPar)%locq(4)/=3) CYCLE

! set new a priori value
      x0_old = neq%par(ipar)%x0
      neq%par(iPar)%x0 = drTrop(neq%par(iPar)%locq(4))
      dx0(iPar) = neq%par(ipar)%x0-x0_old

! prepare deletion of parameter
      IF (iFil>0) CALL prparlst(1,4,iFil,neq%par(iPar)%name,               &
                                neq%par(iPar)%locq,neq%par(iPar)%time)
      neq%par(ipar)%locq(1) = 0
      neq%misc%nparms       = neq%misc%nparms - 1


! DCB parameters
! --------------
    CASE (8)
      x0_old = neq%par(ipar)%x0

      IF (neq%par(ipar)%locq(2) == 1) THEN
        iReq = neq%par(ipar)%locq(5)
        CALL dcbcor(2,0,neq%par(ipar)%locq(3),' ',' ',0,iReq, &
                    neq%par(ipar)%time%mean,neq%par(ipar)%x0)
      ELSE
        iReq = MAX(neq%par(ipar)%locq(6),0)
        CALL dcbcor(2,0,neq%par(ipar)%locq(5),                       &
                    neq%par(ipar)%name(1:staNameLength),             &
                    ' ',0,iReq,neq%par(ipar)%time%mean,neq%par(ipar)%x0)
      ENDIF


      dx0(ipar) = neq%par(ipar)%x0 - x0_old

! Earth orientation parameters
! ----------------------------
    CASE (10)
       ipol = neq%par(ipar)%locq(4)
       ider = neq%par(ipar)%locq(5)

       IF ( rot(ipol,ider)%flag ) THEN
          rot(ipol,ider)%flag = .FALSE.
          IF (neq%par(ipar)%locq(5) == 1) THEN
             rot(ipol,ider)%val = gtaprpol(neq%par(ipar),0) - &
                  ut1_ut1r(neq%par(ipar))
          ELSE
             rot(ipol,ider)%val = 0.d0
          END IF
       END IF

       x0_old = neq%par(ipar)%x0
       IF ( polreqfl ) THEN
         IF( ipol == 3 .AND. rot(ipol,ider)%val-neq%par(ipar)%x0 > 500.d0 &
             .AND. ipart==1)THEN
           neq%par(ipar)%x0 = rot(ipol,ider)%val - 1000.d0
         ELSEIF (ipol == 3 .AND. rot(ipol,ider)%val-neq%par(ipar)%x0 < -500.d0 &
             .AND. ipart==1)THEN
           neq%par(ipar)%x0 = rot(ipol,ider)%val + 1000.d0
         ELSE
           neq%par(ipar)%x0 = rot(ipol,ider)%val
         ENDIF
       ELSE
         neq%par(ipar)%x0 = gtaprpol(neq%par(ipar),0) - &
              ut1_ut1r(neq%par(ipar))
       END IF
       dx0(ipar) = neq%par(ipar)%x0 - x0_old

! Satellite antenna offsets
! -------------------------
    CASE(12)
      IF (sata%first) THEN
        sata%first = .FALSE.
        CALL gtflna(0,'SATELL',sata%satFil,sata%doit)
      ENDIF

      IF (sata%doit /= 0) CYCLE

      x0_old = neq%par(ipar)%x0

      timmin = neq%par(iPar)%time%mean - neq%par(iPar)%time%half
      timmax = neq%par(iPar)%time%mean + neq%par(iPar)%time%half

      iSao = neq%par(ipar)%locq(2)
      iCor = neq%par(ipar)%locq(3)
      iGrp = neq%par(ipar)%locq(4)
      grpnr = neq%par(ipar)%locq(5)

!      IF(iFil>0 .AND. iGrp /= 0)                                      &
!                 CALL chksao(neq%misc%nsaoff(isao),                   &
!                              neq%misc%satoff(:,isao),                &
!                              (/ timmin, timmax /), (/ 1,0,0 /), irc)
!      IF (irc /= 0) CALL exitrc(2)

      ! find antenna offset for a satellite of the group
      x0_new = 1d20
      nsao = 1
      IF (iGrp /= 0) nsao = neq%misc%nsaoff(iSao)
      DO isaoff = 1,nsao

        IF (iGrp /= 0) THEN
          grpnr = neq%misc%satoff(isaoff,iSao)
        ELSE
          WRITE(svnnr,"(A1,I3)")g_syssvn(INT(grpnr/100)),grpnr
          CALL svn2prn(4,svnnr,neq%par(iPar)%time%mean,grpnr,timint,irc)
        ENDIF

        ! Check whether the satellite of the group is active or not
        CALL prn2svn(0,grpnr,neq%par(iPar)%time%mean,svnnr,timint,irc)
        IF (iSap /= 0) THEN
          CALL satblk(grpnr,neq%par(ipar)%time%mean,iFrq,blkNr)
          IF ( blkNr /= neq%par(ipar)%locq(5)) CYCLE
        ENDIF
        IF (irc /= 0 .AND. isaoff == nsao) &
             WRITE(lfnerr,'(/,A,A,/,18X,A,I3,/)')             &
             ' ### SR APRTRANS: No active satellite ',        &
                  'found in satellite antenna offset group.', &
                  'Group: ',iSap

        IF (irc /= 0) CYCLE
            ! Microwave
        IF (neq%par(ipar)%name(1:4) == typeMWTR) THEN
          CALL gtsensor(prn=grpnr,epo=neq%par(iPar)%time%mean, &
                        type1=typeMWTR,antoff=antoff)
!! hardwired L4 offset!! needs to be changed if different L1 L2 offsets
          IF (neq%par(ipar)%locq(6) == 4) THEN
            x0_new = 0D0
          ELSE
            x0_new = antoff(icor)
          ENDIF

            ! Range
        ELSE IF (neq%par(ipar)%name(1:4) == typeSLR) THEN
          CALL gtsensor(prn=grpnr,epo=neq%par(iPar)%time%mean, &
                        type1=typeSLR,antoff=antoff)
          x0_new = antoff(icor)

        ENDIF

        IF (x0_new /= 1d20) THEN
          neq%par(ipar)%x0 = x0_new
          EXIT
        ENDIF

      ENDDO

      dx0(ipar) = neq%par(ipar)%x0 - x0_old

! Geocenter coordinates
! ---------------------
    CASE(16)
      x0_old = neq%par(ipar)%x0
      CALL getgcc(opt%gccinp,neq%par(ipar)%time%mean,gccval0,siggcc,irCode)
      IF (irCode == 0) THEN
        neq%par(ipar)%x0 = gccval0(neq%par(ipar)%locq(2))
      END IF
      dx0(ipar) = neq%par(ipar)%x0 - x0_old

! Receiver antenna pattern
! ------------------------
    CASE (18)
      IF (ircAnt /= 0) CYCLE

      prn = MOD(neq%par(iPar)%locq(5) , 100) * 100
      IF (prn > 999 ) prn = 0

      frq = MOD(neq%par(iPar)%locq(4) , 100)
      izen = neq%par(iPar)%locq(4) / 100
      iazi = neq%par(iPar)%locq(5) / 100
      nzen = neq%par(iPar)%locq(6)
      nazi = neq%par(iPar)%locq(7)

      corr=1d0
      IF (nzen > 0) THEN
        zen=pi/2.d0 / DBLE(nzen-1) * (izen-1)
        azi=2.d0*pi / DBLE(nazi-1) * (iazi-1)
        CALL sta_pcv(neq%par(iPar)%name,neq%par(iPar)%locq(3), &
                     '(estimated)     ',0,1,zen=zen,azi=azi,corr=corr)
      ELSE IF (frq <= 2) THEN
        CALL antInfo(neq%par(iPar)%name,neq%par(iPar)%locq(3),0,index=iAnt)
        corr=recant(iant)%sys(prn)%freq(frq)%pat(0,izen,iazi)
      ELSE
        corr=0d0
      ENDIF

      dx0(ipar)        = corr - neq%par(ipar)%x0
      neq%par(ipar)%x0 = corr


! Scaling factors for Vienna grid files
! -------------------------------------
    CASE(22)
      iGrd = neq%par(ipar)%locq(2)

      dx0(ipar) = DBLE(opt%grdLoad(iGrd)%x0) - neq%par(ipar)%x0
      neq%par(ipar)%x0 = DBLE(opt%grdLoad(iGrd)%x0)

      IF (PRESENT(neq0)) THEN
        DO ip = 1,neq0%misc%npar
          IF ( neq0%par(ip)%locq(1) /= 22 ) CYCLE
          IF ( tstequiv(neq,ipar,neq0,ip) ) THEN
            neq%par(ipar)%x0 = neq%par(ipar)%x0+neq0%xxx(ip)
            dx0(ipar) = dx0(ipar)+neq0%xxx(ip)
          ENDIF
        ENDDO
      ENDIF


! Receiver clocks
! ---------------
    CASE(23)
      x0_old = neq%par(ipar)%x0
      neq%par(ipar)%locq(4) = 0
      neq%par(ipar)%locq(7) = 0
      CALL gtrxck(neq%par(ipar)%time%mean,neq%misc%nsmpnq/86400d0/2d0, &
                  neq%par(ipar)%name,0,0D0,clock,sigma,irCode)
      IF (irCode == 0) THEN
        neq%par(ipar)%x0 = clock/1d6*C
        neq%par(ipar)%locq(7) = 1
      END IF
      dx0(ipar) = x0_old - neq%par(ipar)%x0    ! negative sign for result

! Satellite clocks
! ----------------
    CASE(24)
      x0_old = neq%par(ipar)%x0
      neq%par(ipar)%locq(4) = 0
      neq%par(ipar)%locq(7) = 0

      CALL gtrxck(neq%par(ipar)%time%mean,neq%misc%nsmpnq/86400d0/2d0, &
                  neq%par(ipar)%name,0,0D0,clock,sigma,irCode)
      IF (irCode == 0) THEN
        neq%par(ipar)%x0 = clock/1d6*C
        neq%par(ipar)%locq(7) = 1
      ELSE IF (irCode == 3) THEN ! No mixture between two clock files
        CALL gtsclk(-1,neq%par(ipar)%time%mean,neq%par(ipar)%locq(3), &
                    0D0,0,clock,irCode)
        IF (irCode == 0 .AND. clock /= 0d0) THEN
          neq%par(ipar)%x0 = clock*C
          neq%par(ipar)%locq(7) = 1
        END IF
      END IF
      dx0(ipar) = neq%par(ipar)%x0 - x0_old


! Satellite antenna pattern
! -------------------------
    CASE(25)
      IF (ircAnt /= 0) CYCLE

      IF (satp%first) THEN
        satp%first = .FALSE.
        CALL gtflna(0,'PHASECC',satp%phsFil,satp%doit)
        IF (satp%doit == 0) CALL init_buf(bufsize=(/0,1/))
      ENDIF

      IF (satp%doit /= 0) CYCLE

      x0_old = neq%par(ipar)%x0

      timmin = neq%par(iPar)%time%mean - neq%par(iPar)%time%half
      timmax = neq%par(iPar)%time%mean + neq%par(iPar)%time%half

      iSap = neq%par(ipar)%locq(2)
      grpnr = neq%par(ipar)%locq(3)
!      IF (iFIL > 0 .AND. iSap /= 0)               &
!          CALL chksap(neq%misc%nsaoff(isap),neq%misc%satoff(:,isap), &
!                  (/ timmin, timmax /), irc)
!      IF (irc /= 0) CALL exitrc(2)

      ! find antenna name for a satellite of the group
      x0_new = 1d20


      nsap = 1
      IF (iSap /= 0) nsap = neq%misc%nsaoff(iSap)
      DO isaoff = 1,nsap

        IF (iSap /= 0) THEN
          grpnr = neq%misc%satoff(isaoff,iSap)
        ELSE
          WRITE(svnnr,"(A1,I3)")g_syssvn(INT(grpnr/100)),grpnr
          CALL svn2prn(4,svnnr,neq%par(iPar)%time%mean,grpnr,timint,irc)
        ENDIF

        ! Check whether the satellite of the group is active or not
        CALL prn2svn(0,grpnr,neq%par(iPar)%time%mean,svnnr,timint,irc)
        IF (iSap /= 0) THEN
          CALL satblk(grpnr,neq%par(ipar)%time%mean,iFrq,blkNr)
          IF ( blkNr /= neq%par(ipar)%locq(3)) CYCLE
        ENDIF
        IF (irc /= 0 .AND. isaoff == nsap) &
             WRITE(lfnerr,"(' ### SR APRTRANS: No active satellite '//&
                           &'found in satellite antenna pattern group.' &
                           & /,18X,'Group: ',I3,/)")iSap
        IF (irc /= 0) CYCLE

        ! Read the coded a priori information into name
        indobst=0
        IF (neq%version >= 8) THEN
           READ(neq%par(ipar)%name,'(I1,I2,I4,I3,I5)') ifreq, inad, iazi, iadmax,indobst
        ELSE
           READ(neq%par(ipar)%name,'(I1,I2,I4,I3)') ifreq, inad, iazi, iadmax
        ENDIF
        dnad   = DBLE(inad)/10d0*pi/180D0
        dazi   = DBLE(iazi)/10d0*pi/180D0

        ! Compute the nadir/azimuth of the parameter
        iNad   = neq%par(ipar)%locq(4)
        iAzi   = neq%par(ipar)%locq(5)

        nad=dnad*(inad-1)
        azi=dazi*(iazi-1)

        ! Get pattern values for first satellite of the group (nad, azi)
! RD: This simplification works as long as we have the same corrections for
!     both frequencies -- but it is able to bridge GLONASS frequency changes.
!        CALL defreq((/timmin,timmax/),1,(/grpnr/))
! LP: Check for Sat-specific obstype info before
        usegeos = 0
!        WRITE(*,*)'SR APRTRANS, PCV; indobst, satellite: ',indobst, grpnr
        IF ((neq%version >= 8).AND.(indobst>0).AND.(neq%misc%nobst>=indobst)) THEN
           CALL init_geos(1,gobsdef)
           CALL setgeos2(neq%misc%obst(indobst),grpnr,gobsdef,usegeos1)
           IF (usegeos1==1) usegeos=1
        ENDIF

        IF (usegeos==1) THEN
          CALL defreq((/neq%par(iPar)%time%mean,neq%par(iPar)%time%mean/),&
                    1,(/grpnr/),USEGEOS=USEGEOS,GOBSDEF=GOBSDEF,MEATYPC='U')
        ELSE
          CALL defreq((/neq%par(iPar)%time%mean,neq%par(iPar)%time%mean/),&
                    1,(/grpnr/))
        ENDIF

        CALL sat_pcv(grpnr,neq%par(iPar)%time%mean,typeMWTR, &
                                                   1,nad,azi,elvcor(1))

        CALL sat_pcv(grpnr,neq%par(iPar)%time%mean,typeMWTR, &
                                                   2,nad,azi,elvcor(2))
        IF (elvcor(1) /= elvcor(2)) THEN
          IF (usegeos==1) THEN
            CALL defreq((/timmin,timmax/),1,(/grpnr/),USEGEOS=USEGEOS,  &
                 GOBSDEF=GOBSDEF,MEATYPC='U')
          ELSE
            CALL defreq((/timmin,timmax/),1,(/grpnr/))
          ENDIF
        ENDIF
        CALL sat_pcv(grpnr,neq%par(iPar)%time%mean,typeMWTR, &
                                                   ifreq,nad,azi,elvcor(3))

        x0_new = elvcor(3)

        IF (x0_new /= 1d20) THEN
          neq%par(ipar)%x0 = x0_new
          EXIT
        ENDIF

      ENDDO

      dx0(ipar) = neq%par(ipar)%x0 - x0_old

! Range biases
! ------------
    CASE(26)

      IF ( opt%rgbinp == '' ) CYCLE

      epoch = .RealToEpoch.neq%par(ipar)%time%mean

      CALL getrgb('RGB', neq%par(ipar)%name, neq%par(ipar)%locq(5), &
                  neq%par(ipar)%locq(4), epoch, rgbReq, rgbFound)

      ! No valid a priori value found in file
      ! -------------------------------------
      IF ( rgbFound == 1 ) THEN
        write(lfnerr,'(/,A,/,18X,A,A,/,18X,A,I5,/)')                 &
              ' ### SR APRTRANS: No a priori value found for RGB. ', &
              'Station: ', neq%par(ipar)%name,                       &
              'Satellite group: ', neq%par(ipar)%locq(5)
        CYCLE
      END IF

      x0_old           = neq%par(ipar)%x0
      neq%par(ipar)%x0 = rgbReq%value
      dx0(ipar)        = neq%par(ipar)%x0 - x0_old

! Higher-order ionosphere (HOI) scaling factors
! ---------------------------------------------
    CASE(27)
      iHoi = neq%par(ipar)%locq(2)
      dx0(ipar)        = DBLE(opt%hoi(iHoi)%x0) - neq%par(ipar)%x0
      neq%par(ipar)%x0 = DBLE(opt%hoi(iHoi)%x0)

! Helmert transformation parameters
! ---------------------------------
    CASE(28)
      IF ( iFil == 0 ) CYCLE

      iReq   = neq%par(ipar)%locq(2)
      x0_old = neq%par(ipar)%x0

      IF ( iReq <= 3 ) THEN
        neq%par(ipar)%x0 = hlmFil%rhelm(iReq,iFil)

      ELSEIF ( iReq > 3 .AND. iReq <= 6 ) THEN
        neq%par(ipar)%x0 = hlmFil%rhelm(iReq,iFil) * datum%aell

      ELSEIF ( iReq == 7 ) THEN
        neq%par(ipar)%x0 = (hlmFil%rhelm(iReq,iFil) - 1.d0) * datum%aell

      END IF

      dx0(ipar) = neq%par(ipar)%x0 - x0_old


    END SELECT

! End of loop over all parameters
! -------------------------------
  END DO

! Perform the transformation
! --------------------------
  DO ipar = 1, neq%misc%npar
    hlp = 0.d0
    DO kk = 1, neq%misc%npar
      hlp = hlp + neq%aNor(ikf(ipar,kk)) * dx0(kk)
    END DO
    neq%misc%lTPl  = neq%misc%lTPl  + dx0(ipar) * &
                                      (-2.d0*neq%bNor(ipar) + hlp)
    neq%bNor(ipar) = neq%bNor(ipar) - hlp
  END DO

  DEALLOCATE(vfound, stat=iac)
  DEALLOCATE(velflg, stat=iac)
  DEALLOCATE(xvel, stat=iac)
  DEALLOCATE(xstat, stat=iac)
  DEALLOCATE(stname, stat=iac)

END SUBROUTINE aprtrans


END MODULE

