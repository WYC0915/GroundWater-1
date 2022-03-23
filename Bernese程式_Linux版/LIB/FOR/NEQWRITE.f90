MODULE s_NEQWRITE
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE neqwrite(filneq,title,npar,nstat,lTPl,locq,partyp,    &
                    aNor,bNor,xstat,stname,xstell,trplms,        &
                    itropo,iextra,xxx,ncamp,taecmp,scastc,       &
                    tbound,tstoch,tpol,scacen,nobs,nparms,       &
                    filstd,filrpr,antsnx,nftot,nsmpnq,ielvnq,    &
                    nanoff,nsaoff,satoff,itrmap,itrgrd,namgim,   &
                    epogim,scagim,nutnam,subnam,datum,nadmax,    &
                    ifreq ,nanspv,nsaspv,satspv,meatyp,rnxclk,   &
                    secipl,ClkHed,clkRec,nEpSam,antcal,numcal,   &
                    prncal,antrao,numrao,prnrao,nobspa,opLoad,   &
                    timisb,USEGEOS,GOBSDEF)

! -------------------------------------------------------------------------
! Purpose:    Write the NEQ system into a file (this subroutine is
!             called from program GPSEST)
!
! Author:     L.Mervart
!
! Created:    22-Nov-1997
!
! Changes:    17-May-2000 SS: Save DCB-related information
!             28-Aug-2000 MR: Add troposphere models 5 and 15
!             08-Sep-2000 HB: use fileNameLength from m_bern
!             18-Dec-2001 MM: itrgrd now DIMENSION(2) (just like in GPSEST)
!             20-Dec-2001 SS: Save GIM-related information
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             22-Dec-2001 HU: itrgrd is array
!             29-Dec-2001 HU: Interface to nqwthead added
!             09-Jun-2001 DS: 206264.d0 --> 206264.8d0
!             23-Jan-2002 CU: Write no neq if parameter types included
!                             which are (still) not handled in ADDNEQ2
!             28-Jan-2002 CU: Write neq, but print warning, if parameter
!                             types included which are not handled in ADDNEQ2
!             30-Jan-2002 CU: Support stoch. pulses
!             06-Feb-2002 CU: Add parameter type 20,22 for warning message
!             07-May-2002 SS: DCB update
!             22-Sep-2002 RD: Add SR opnerr after SR opnfil
!             28-Nov-2002 DT: Add parameter types 'Range biases' and 'Time
!                             biases' for partxt
!             04-Feb-2003 PS: Save name of Subdaily Pole and Nutation Model
!             07-Mar-2003 HU: Extend partxt to 26
!             20-May-2003 MM: Troposphere now piecewise linear
!             12-Aug-2003 RS: Add satellite antenna patterns
!             08-Sep-2003 HU: New NEQ version:
!                             Write datum, nutnam, subnam to NEQ
!                             Antnam, recnam chr16 -> chr20
!             10-Sep-2003 HU: Merged
!             17-Dec-2003 RS: Write a priori satellite antenna offsets
!             29-Mar-2004 CU: Write a priori satellite reflector offsets,
!                             Add dummy variable to call of sr tropos
!             29-Jul-2004 HU: Format statement corrected
!             22-Dec-2004 RD: Do not use MAXSAT anymore in the parameter list
!             28-Jun-2005 MM: Unused variables removed
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!             09-Nov-2005 AG: SENNUM for GTSATA & GPHECC CALLs added
!             13-Dec-2005 CU: Adapt call of SR tropos
!             24-Aug-2006 AG: SR tdelay instead of tropos used
!             04-Oct-2006 AG: Satellite specific antenna PCO/PCV implemented
!             13-Nov-2006 AG: Hardwired L4 satellite antenna offset
!             08-Feb-2007 RD: misc%nObs/nParms i4b->r8b
!             28-Feb-2007 AG: Use 206264... from DEFCON
!             12-Jun-2007 AG: Use sat_pcv instead of gphecc
!             23-Jan-2008 RD: RAO+RAP added to NEQ
!             07-May-2008 DT: Allow receiver clocks (type=2)
!             13-Jun-2008 RD: Use writePar from D_PAR.f90
!             15-Jun-2008 RD: Set technique flags
!             17-Jun-2008 RD: Counter for observ. per parameter added
!             13-Dec-2008 RD: Remove dummy parameters
!             02-Apr-2009 DT: Add Range Biases (26); call GETRGB
!             30-Apr-2009 SL: neq%version set to 5
!             04-May-2009 RD: Scaling of loading models added
!             09-Nov-2005 RD: Sat/frq-specific receiver clock biases
!             29-May-2009 RD: Add epoch satellite/receiver clocks
!             04-Jan-2010 SL: HOI scaling parameters added
!             08-Sep-2010 RD: Merge SLR-time bias option
!             16-Nov-2010 RD: Distinguish between piece-wise linear param.
!             16-Nov-2010 HB: scastc is an array
!             30-Nov-2010 DT: Add Helmert parameters
!             06-Dec-2010 MM: GNSS-specific parameters
!             16-Jan-2011 RD: STANUM removed
!             05-May-2011 HB: Remove USE s_gtsata
!             13-Jul-2011 LP: Sat-spec. obstypes; neq%version set to 8
!             13-Dec-2011 SL: m_bern with ONLY, unused modules removed
!             28-Mar-2012 RD: Use SVN2CHR as module now
!             05-Jun-2012 LP: Filling up of misc%obst%obstype changed
!             14-Jun-2012 RD: Save also empty NEQs (npar == 0)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, fileNameLength, staNam2Length, &
                      lfnPrt, lfnErr
  USE m_maxdim, ONLY: maxsat
  USE m_time,   ONLY: t_timint
  USE m_epoch,  ONLY: t_epoch, OPERATOR(.RealToEpoch.)
  USE m_global, ONLY: maxsys,g_svnsys
  USE d_isbFil, ONLY: getIsb
  USE d_grid,   ONLY: grdNeq
  USE d_par,    ONLY: maxlcq,maxParTyp,add_techn
  USE d_neq,    ONLY: t_neq,t_sinex,maxfrq,maxOff,maxStaSin,maxObst
  USE d_const,  ONLY: pi, ars, C
  USE d_satfil, ONLY: typeMWTR,typeSLR
  USE d_phaecc, ONLY: sat_pcv, sta_off, sta_pcv, antinfo, recant
  USE d_clkrnx, ONLY: t_clkhead, t_clkrec
  USE d_rgbfil, ONLY: t_rgb
  USE p_gpsest, ONLY: maxspv,maxmea,t_optLoad,t_partyp
  USE d_rinex3, ONLY: t_gobsdef
  USE s_tdelay
!  USE s_opnfil
  USE f_gtaprpol
  USE s_gtsensor
  USE s_prn2svn
  USE s_dcbcor
!  USE s_opnerr
  USE s_timst2
!  USE s_nqwthead
  USE s_exitrc
  USE f_ut1_ut1r
!  USE s_gtflna
  USE s_neqinit
  USE s_neqalloc
  USE s_neqstore
  USE f_ikf
  USE s_neqclean
  USE s_gtsclk
  USE s_getrgb
  USE s_svn2chr
  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=fileNameLength)                 :: filneq
  CHARACTER(LEN=132),DIMENSION(2)               :: title
  INTEGER(i4b)                                  :: npar
  INTEGER(i4b)                                  :: nstat
  REAL(r8b)                                     :: lTPl
  INTEGER(i4b),      DIMENSION(maxlcq,npar)     :: locq
  TYPE(t_partyp),    DIMENSION(npar)            :: partyp
  REAL(r8b),         DIMENSION(npar*(npar+1)/2) :: aNor
  REAL(r8b),         DIMENSION(npar)            :: bNor
  REAL(r8b),         DIMENSION(3,nstat)         :: xstat
  CHARACTER(LEN=16), DIMENSION(nstat)           :: stname
  REAL(r8b),         DIMENSION(3,nstat)         :: xstell
  REAL(r8b),         DIMENSION(2,*)             :: trplms
  INTEGER(i4b)                                  :: itropo
  INTEGER(i4b)                                  :: iextra
  REAL(r8b),         DIMENSION(npar)            :: xxx
  INTEGER(i4b)                                  :: ncamp
  REAL(r8b),         DIMENSION(2,*)             :: taecmp
  REAL(r8b),         DIMENSION(*)               :: scastc
  REAL(r8b),         DIMENSION(2,*)             :: tbound
  REAL(r8b),         DIMENSION(*)               :: tstoch
  REAL(r8b),         DIMENSION(2,*)             :: tpol
  REAL(r8b)                                     :: scacen
  INTEGER(i4b)                                  :: nobs
  INTEGER(i4b)                                  :: nparms
  CHARACTER(LEN=fileNameLength)                 :: filstd
  CHARACTER(LEN=fileNameLength)                 :: filrpr
  TYPE(t_sinex), DIMENSION(:)                   :: antsnx
  INTEGER(i4b)                                  :: nftot
  INTEGER(i4b)                                  :: nsmpnq
  INTEGER(i4b)                                  :: ielvnq
  INTEGER(i4b)                                  :: nanoff
  INTEGER(i4b),      DIMENSION(*)               :: nsaoff
  INTEGER(i4b),      DIMENSION(:,:)             :: satoff
  INTEGER(i4b)                                  :: itrmap
  INTEGER(i4b),      DIMENSION(*)               :: itrgrd
  CHARACTER(LEN=16), DIMENSION(*)               :: namgim
  REAL(r8b),         DIMENSION(2,*)             :: epogim
  REAL(r8b),         DIMENSION(3)               :: scagim
  CHARACTER(LEN=16)                             :: nutnam
  CHARACTER(LEN=16)                             :: subnam
  CHARACTER(LEN=16)                             :: datum
  CHARACTER(LEN=4)                              :: svnnr
  REAL(r8b)                                     :: nadmax
  INTEGER(i4b)                                  :: ifreq
  INTEGER(i4b)                                  :: nanspv
  INTEGER(i4b),      DIMENSION(maxspv)          :: nsaspv
  INTEGER(i4b),      DIMENSION(:,:)             :: satspv
  INTEGER(i4b)                                  :: meatyp
  CHARACTER(LEN=*),  DIMENSION(2,*)             :: antcal
  INTEGER(i4b),      DIMENSION(2,*)             :: numcal
  INTEGER(i4b),      DIMENSION(:)               :: prncal
  CHARACTER(LEN=*),  DIMENSION(2,*)             :: antrao
  INTEGER(i4b),      DIMENSION(2,*)             :: numrao
  INTEGER(i4b),      DIMENSION(:)               :: prnrao
  INTEGER(i4b),      DIMENSION(:,:)             :: nobspa
  INTEGER(i4b)                                  :: rnxclk
                                                ! What to do if no input clock
                                                ! rinex for satellite clocks:
                                                !  -1: ignore clock rinex file
                                                !   0: Try also sat clk file
                                                !   1: Use obs. (interpol. clk rnx)
                                                !   2: Skip obs.
                                                !   3: Use obs. (sat clk = zero)
  REAL(r8b)                    :: secIpl    ! Interval for clock interpolation
  TYPE(t_clkHead)              :: clkhed    ! Header information for
                                            ! clock rinex output file
  TYPE(t_clkRec)               :: clkrec    ! %nEpo: # epochs with highest sampl.
                                            ! %epoch(1): last epoch (sec. since
                                            !   clkhed%tFirst)
                                            ! %clock: Apriori station clocks
                                            !   (if necessary)
  INTEGER(i4b)                 :: nEpSam    ! Sampling of preeliminated epoch
                                            ! parameters
  TYPE(t_optLoad),   DIMENSION(:)         :: opLoad
  REAL(r8b),         DIMENSION(3,*)       :: timISb
  TYPE(t_gobsdef),OPTIONAL         :: GOBSDEF ! Giove External Obs. Selection info
  INTEGER(i4b),OPTIONAL            :: USEGEOS ! =1: GOBSDEF data set available


! Local Parameters
! ----------------
  CHARACTER(LEN=37),DIMENSION(maxParTyp)        :: partxt = &
      (/ 'Station coordinates                  ', &
         'Receiver clocks                      ', &
         'Orbital elements                     ', &
         'Ambiguities                          ', &
         'Receiver antenna offset parameters   ', &
         'Site-specific troposphere parameters ', &
         'Local ionosphere models              ', &
         'Differential code biases             ', &
         'Local troposphere models             ', &
         'Earth rotation parameters            ', &
         'Stochastic orbit parameters          ', &
         'Satellite antenna offset parameters  ', &
         'Earth potential parameters           ', &
         'Resonance terms (hill theory)        ', &
         'Albedo parameters                    ', &
         'Center of mass                       ', &
         'Stochastic ionosphere parameters     ', &
         'Receiver phase center variations     ', &
         'Global ionosphere model parameters   ', &
         'Station velocities                   ', &
         'Kinematic coordinates                ', &
         'Scaling factors for vienna grid files', &
         'Epoch wise clocks station clocks     ', &
         'Epoch wise clocks satellite clocks   ', &
         'Satellite phase center variations    ', &
         'Range biases                         ', &
         'Higher-order iono scaling parameters ', &
         'Helmert transformation parameters    ', &
         'Not used                             ', &
         'GNSS-specific parameters             ' /)

! Local Variables
! ---------------
  TYPE(t_neq)                                   :: neq
  TYPE(t_rgb)                                   :: rgbReq
  TYPE(t_epoch)                                 :: epoch

  REAL(r8b)                                     :: zen
  REAL(r8b)                                     :: dr
  REAL(r8b)                                     :: temp
  REAL(r8b)                                     :: press
  REAL(r8b)                                     :: hum
  REAL(r8b)                                     :: timmin
  REAL(r8b)                                     :: timmax
  REAL(r8b)                                     :: tmean
  REAL(r8b)                                     :: tp1
  REAL(r8b)                                     :: tp2
  REAL(r8b),         DIMENSION(3)               :: antoff
  TYPE(t_timint)                                :: timint
  REAL(r8b)                                     :: dnad
  REAL(r8b)                                     :: nad
  REAL(r8b)                                     :: dazi
  REAL(r8b)                                     :: azi
  REAL(r8b)                                     :: elvcor
  REAL(r8b), DIMENSION(3)                       :: offset
  REAL(r8b)                                     :: corr
  REAL(r8b)                                     :: dSec

  INTEGER(i4b)                                  :: ipar
  INTEGER(i4b)                                  :: iSys
  INTEGER(i4b)                                  :: imod
  INTEGER(i4b)                                  :: ireq1
  INTEGER(i4b)                                  :: ii
  INTEGER(i4b)                                  :: irc
  INTEGER(i4b)                                  :: iseq
  INTEGER(i4b)                                  :: ISUBFL
  INTEGER(i4b)                                  :: ityp
  INTEGER(i4b)                                  :: ispv
  INTEGER(i4b)                                  :: izen
  INTEGER(i4b)                                  :: iazi
  INTEGER(i4b)                                  :: nptzen
  INTEGER(i4b)                                  :: nptazi
  INTEGER(i4b)                                  :: isao
  INTEGER(i4b)                                  :: icor
  INTEGER(i4b)                                  :: grpoff
  INTEGER(i4b)                                  :: irao
  INTEGER(i4b)                                  :: prn
  INTEGER(i4b)                                  :: irap
  INTEGER(i4b)                                  :: iAnt
  INTEGER(i4b)                                  :: iEpo,jEpo
  INTEGER(i4b)                                  :: iSvn
  INTEGER(i4b)                                  :: iSat
  INTEGER(i4b)                                  :: iSta
  INTEGER(i4b)                                  :: numEpo
  INTEGER(i4b)                                  :: found
  INTEGER(i4b)                                  :: satnumg
  CHARACTER(LEN=3)                              :: satnumc
  INTEGER(i4b)                                  :: igeos,obs
  INTEGER(i4b)                                  :: indgeos

!  CHARACTER(LEN=fileNameLength)                 :: phasecc
  CHARACTER(LEN=staNam2Length)                  :: satnam
  CHARACTER(LEN=20)                             :: string1
  CHARACTER(LEN=1)                              :: cSvn
  CHARACTER(LEN=1)                              :: satsys

  LOGICAL, DIMENSION(maxParTyp)                 :: errchk
  LOGICAL                                       :: satspec
  LOGICAL                                       :: first


! Check parameter types
! ---------------------
  DO iPar = 1,maxParTyp
    errchk(iPar) = .FALSE.
  ENDDO

  DO iPar = 1,nPar
    IF (maxParTyp < locq(1,iPar)) CYCLE

    IF (.NOT. errchk(locq(1,iPar)) .AND. &  ! Parameter type reported

        ( locq(1,iPar) ==  4     .OR. &     ! Ambiguities
          locq(1,iPar) ==  7     .OR. &     ! Local ionosphere
          locq(1,iPar) ==  9     .OR. &     ! Local troposphere
!         (locq(1,iPar) == 11 .AND. &
!               locq(5,iPar) > 3) .OR. &     ! Stoch pulse
          locq(1,iPar) == 13     .OR. &     ! Earth potential
          locq(1,iPar) == 14     .OR. &     ! Hill resonance
          locq(1,iPar) == 15     .OR. &     ! Earth albedo
          locq(1,iPar) == 17     .OR. &     ! Diff iono. parameters
          locq(1,iPar) == 20     .OR. &     ! Station velocities (old ADDNEQ)
          locq(1,iPar) == 21)) THEN         ! Kinematic positions

      errchk(locq(1,iPar)) = .TRUE.
      WRITE(lfnerr,'(/,A,/,18X,A,/)')                                     &
        ' ### SR NEQWRITE: The parameter type '//partxt(locq(1,iPar)),    &
                          'is (still) not supported in the NEQ format.'
    ENDIF
  ENDDO

! Start writing the neq
! ---------------------
  CALL neqinit(neq)
  CALL neqalloc(neq,npar)

  grpoff = 0
  timmin = MINVAL( taecmp(1,1:ncamp) )
  timmax = MAXVAL( taecmp(2,1:ncamp) )
  first  = .TRUE.

  DO ipar=1,npar

    neq%par(ipar)%locq(:) = locq(:,ipar)
    neq%par(ipar)%type    = partyp(ipar)%type
    neq%par(ipar)%omega   = partyp(ipar)%omega

! Set technique flags
! -------------------
    IF (nobspa(3,ipar)>0) THEN
      CALL add_techn(neq%par(ipar),slr=1)
    ELSE
      DO iSys = 1,maxsys
        IF (nobspa(maxmea*(iSys-1)+1,ipar)>0 .OR. &
            nobspa(maxmea*(iSys-1)+2,ipar)>0) THEN
          CALL add_techn(neq%par(ipar),gnss=1)
          IF (iSys == 1) CALL add_techn(neq%par(ipar),gps =1)
          IF (iSys == 2) CALL add_techn(neq%par(ipar),glo =1)
          IF (iSys == 3) CALL add_techn(neq%par(ipar),gal =1)
          IF (iSys == 4) CALL add_techn(neq%par(ipar),sbas=1)
!         ADD_GNSS_HERE
        ENDIF
      ENDDO
    ENDIF



    SELECT CASE ( locq(1,ipar) )

! Dummy parameters (to be removed later)
! --------------------------------------
    CASE (0)

! Station coordinates
! -------------------
    CASE (1)
      neq%par(ipar)%x0         = xstat(locq(3,ipar),locq(2,ipar))
      neq%par(ipar)%name       = stname(locq(2,ipar))
      neq%par(ipar)%time%mean  = (timmax + timmin) / 2.d0
      neq%par(ipar)%time%half  = (timmax - timmin) / 2.d0
      neq%par(ipar)%scale      = 1.d0

! Receiver clock offsets
! ----------------------
    CASE (2)
      neq%par(ipar)%x0         = 0d0
      neq%par(ipar)%name       = stname(locq(2,ipar))
      neq%par(ipar)%time%mean  = (timmax + timmin) / 2.d0
      neq%par(ipar)%time%half  = (timmax - timmin) / 2.d0
      neq%par(ipar)%scale      = 1.d0

      IF (neq%par(ipar)%locq(6) == 1) THEN
        CALL dcbcor(2,0,0,neq%par(ipar)%name,'',neq%par(ipar)%locq(4),  &
                    5,neq%par(ipar)%time%mean,neq%par(ipar)%x0)
      ENDIF
      IF (neq%par(ipar)%locq(6) == 2) THEN
        CALL dcbcor(2,0,neq%par(ipar)%locq(4),neq%par(ipar)%name,'',0,  &
                    5,neq%par(ipar)%time%mean,neq%par(ipar)%x0)
      ENDIF
      IF (neq%par(ipar)%locq(6) == 5) THEN
        neq%par(ipar)%time%mean  = TIMISB(1,neq%par(ipar)%locq(4))
        neq%par(ipar)%time%half  = 0d0
        neq%par(ipar)%locq(4)    = 0
        neq%par(ipar)%x0         = getisb(0,neq%par(ipar)%name, &
                                   neq%par(ipar)%time%mean,     &
                                   neq%par(ipar)%locq(3),irc)
      ENDIF
      neq%par(ipar)%x0 = neq%par(ipar)%x0 * C/1D9

! Orbits
! ------
    CASE (3)
      neq%par(ipar)%x0   = 0.0
      neq%par(ipar)%name = ''
      neq%par(ipar)%time%mean = tbound(1,locq(2,ipar))
      neq%par(ipar)%time%half = 0.d0
      iseq = locq(4,ipar)
      IF (iseq == 1) THEN
        neq%par(ipar)%scale = 1.d0
      ELSE IF (iseq <= 6) THEN
        neq%par(ipar)%scale = ars
      ELSE
        neq%par(ipar)%scale = 1.d9
      END IF

! Receiver antenna offsets
! ------------------------
    CASE (5)
      irao                 = locq(2,ipar)

      prn = prnrao(irao)*100
      IF (prn > 999 ) prn = 0

      iFreq = locq(3,ipar)
      CALL sta_off(antrao(2,irao),numrao(2,irao),'(estimated)     ', &
              prn,iFreq,offset=offset)
      neq%par(ipar)%x0         = offset(locq(4,ipar))

      neq%par(ipar)%name       = antrao(2,irao)
      neq%par(ipar)%time%mean  = (timmax + timmin) / 2.d0
      neq%par(ipar)%time%half  = (timmax - timmin) / 2.d0
      neq%par(ipar)%scale      = 1.d0

      neq%par(ipar)%locq(5)    = prnrao(irao)
      neq%par(ipar)%locq(4)    = locq(4,ipar)*100+locq(3,ipar)
      neq%par(ipar)%locq(2:3)  = numrao(1:2,irao)

! Troposphere
! -----------
    CASE (6)
      IF ( locq(4,ipar) == 3 ) THEN    ! zenith delay
        ireq1 = locq(2,ipar)

        IF (iextra.eq.0) THEN
          dr  = 0.d0
        ELSE
          zen = 0.d0
          CALL tdelay(trplms(1,ireq1),zen,xstell(:,locq(3,ipar)),itropo, &
                                                  1,0d0,temp,press,hum,dr)
        END IF

      ELSE                                  ! gradient
        dr    = 0.d0
        ireq1 = locq(2,ipar)
      END IF

      neq%par(ipar)%x0        = dr
      neq%par(ipar)%name      = stname(locq(3,ipar))
      neq%par(ipar)%scale     = 1.d0
      neq%par(ipar)%time%mean = trplms(1,ireq1)
      neq%par(ipar)%time%half = 0.d0

! Differential code biases
! ------------------------
    CASE (8)
      tmean = (timmax+timmin)/2d0
!     Satellite DCB
      IF (locq(2,ipar) == 1) THEN
        neq%par(ipar)%name = ''
        ityp = locq(5,ipar)
        CALL dcbcor(2,0,locq(3,ipar),' ',' ',0,ityp, &
                    tmean,neq%par(ipar)%x0)

!       Sat-spec. obstypes
        neq%par(ipar)%locq(7) = 0
        IF ((PRESENT(USEGEOS)).AND.(PRESENT(GOBSDEF))) THEN
         IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
          DO igeos=1,gobsdef%norec
            satnumg=gobsdef%sat(igeos)%sysnum*100+gobsdef%sat(igeos)%satnum
            IF (satnumg==locq(3,ipar)) neq%par(ipar)%locq(7) = igeos
          ENDDO
         ENDIF
        ENDIF

!     Station DCB
      ELSE
        neq%par(ipar)%name = stname(locq(3,ipar))
        ityp = MAX(locq(6,ipar),0)
        CALL dcbcor(2,0,locq(5,ipar),stname(locq(3,ipar)),' ',0,ityp, &
                    tmean,neq%par(ipar)%x0)

        satsys = g_svnsys(locq(5,ipar)-1)
        IF ((ityp == 1).AND.(satsys.NE.'G').AND.(satsys.NE.'R')) THEN
!        ADD_GNSS_HERE
         IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
          DO igeos=1,gobsdef%norec
            IF (satsys.EQ.gobsdef%sat(igeos)%syschar) THEN
              neq%par(ipar)%locq(7) = igeos
              exit
            ENDIF
          ENDDO
         ENDIF
        ENDIF

      END IF
      neq%par(ipar)%time%mean  = (timmax + timmin) / 2.d0
      neq%par(ipar)%time%half  = (timmax - timmin) / 2.d0
      neq%par(ipar)%scale = 1.0d0

! Earth rotation parameters
! -------------------------
    CASE (10)
      tp1 = tpol(1,locq(3,ipar))
      tp2 = tpol(2,locq(3,ipar))

      IF      ( locq(5,ipar) == 1 .AND. locq(6,ipar) == 1 ) THEN   ! offset
        neq%par(ipar)%time%mean = ( tp2 + tp1 ) / 2.d0             ! only
        neq%par(ipar)%time%half = ( tp2 - tp1 ) / 2.d0
      ELSE IF ( locq(5,ipar) == 1 .AND. locq(6,ipar) == 2 ) THEN   ! offset
        neq%par(ipar)%time%mean = tp1
        neq%par(ipar)%time%half = 0.d0
      ELSE IF ( locq(5,ipar) == 2 .AND. locq(6,ipar) == 2 ) THEN   ! drift
        neq%par(ipar)%time%mean = ( tp2 + tp1 ) / 2.d0
        neq%par(ipar)%time%half = ( tp2 - tp1 ) / 2.d0
      ELSE
        WRITE(lfnerr,*) ' *** SR NEQWRITE - ERP ERROR'
        CALL EXITRC(2)
      END IF

      IF ( locq(4,ipar) == 1 .OR. locq(4,ipar) == 2 .OR. &         ! Pol/UT1
           locq (4,ipar)== 3) THEN
               neq%par(ipar)%name  = subnam
      ELSE IF ( locq(4,ipar) == 4 .OR. locq(4,ipar) == 5) THEN     ! Nutation
               neq%par(ipar)%name  = nutnam
      ELSE
        WRITE(lfnerr,*) ' *** SR NEQWRITE - ERP ERROR'
        CALL EXITRC(2)
      END IF

      neq%par(ipar)%scale = 1.0d0
      isubfl = 0
      neq%par(ipar)%x0    = gtaprpol(neq%par(ipar),isubfl) - ut1_ut1r(neq%par(ipar))

! Stochastic orbital parameters
! -----------------------------
    CASE (11)
      neq%par(ipar)%x0   = 0.0
      neq%par(ipar)%name = ''
      neq%par(ipar)%time%mean = tstoch(locq(4,ipar))
      neq%par(ipar)%time%half = 0.d0
      IF(locq(5,ipar) < 10) THEN
        neq%par(ipar)%scale = scastc(1)
      ELSE
        neq%par(ipar)%scale = scastc(2)
      END IF

! Satellite antenna offsets
! -------------------------
    CASE (12)
      isao = locq(2,ipar)
      icor = locq(3,ipar)
      grpoff = locq(4,ipar)

! find first satellite of group and replace PRN with SVN if satellite specific
      CALL prn2svn(4,satoff(1,isao),(timmax + timmin)/2.d0,svnnr,timint,irc)
      IF(grpoff==0) READ(svnnr,"(1X,I3)")neq%par(ipar)%locq(5)

! phase / code measurement: satellite antenna offset
      IF (meatyp == 1 .OR. meatyp == 2) THEN
! get satellite antenna offsets
        CALL gtsensor(prn=satoff(1,isao),epo=(timmax+timmin)/2.d0, &
                     type1=typeMWTR,sensor=satnam,antoff=antoff)
! range measurement: satellite reflector offset
      ELSEIF (meatyp == 3) THEN
! get satellite antenna offsets
        CALL gtsensor(prn=satoff(1,isao),epo=(timmax+timmin)/2.d0, &
                      type1=typeSLR,sensor=satnam,antoff=antoff)
! unknown measurement type -> error
      ELSE
        WRITE(lfnprt,'(A,I4)')' *** SR NEQWRITE: unkown meatyp: ', meatyp
        CALL exitrc(2)
      ENDIF

!! hardwired L4 offset!! needs to be changed if different L1 L2 offsets
      IF (ifreq == 4 .AND. meatyp /= 3) THEN
        neq%par(ipar)%x0         = 0D0
      ELSE
        neq%par(ipar)%x0         = antoff(icor)
      ENDIF
      neq%par(ipar)%name       = satnam
      neq%par(ipar)%time%mean  = (timmax + timmin) / 2.d0
      neq%par(ipar)%time%half  = (timmax - timmin) / 2.d0
      neq%par(ipar)%scale      = 1.0d0

!     Sat-spec. obstypes
      neq%par(ipar)%locq(7) = 0
      IF ((PRESENT(USEGEOS)).AND.(PRESENT(GOBSDEF))) THEN
       IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
        DO igeos=1,gobsdef%norec
           satnumg=gobsdef%sat(igeos)%sysnum*100+gobsdef%sat(igeos)%satnum
           WRITE (satnumc,'(I3)') satnumg
           IF (satnumc.eq.svnnr(2:4)) neq%par(ipar)%locq(7) = igeos
        ENDDO
       ENDIF
      ENDIF


! Center of mass
! --------------
    CASE (16)
      neq%par(ipar)%x0   = 0.0
      neq%par(ipar)%name = ''
      neq%par(ipar)%time%mean  = (timmax + timmin) / 2.d0
      neq%par(ipar)%time%half  = (timmax - timmin) / 2.d0
      neq%par(ipar)%scale = scacen

! Receiver antenna pattern
! ------------------------
    CASE (18)
      irap = locq(2,ipar)

      prn = prncal(irap)*100
      IF (prn > 999 ) prn = 0

      iFreq = locq(3,ipar)
      IF (locq(6,ipar) > 0) THEN
        zen=pi/2.d0 / DBLE(locq(6,ipar)-1) * (locq(4,ipar)-1)
        azi=2.d0*pi / DBLE(locq(7,ipar)-1) * (locq(5,ipar)-1)
        CALL sta_pcv(antcal(2,irap),numcal(2,irap),'(estimated)     ', &
                prn,iFreq,zen=zen,azi=azi,corr=corr)
      ELSE
        CALL antInfo(antcal(2,irap),numcal(2,irap),0,index=iAnt)
        corr=recant(iant)%sys(MOD(prncal(irap),10))%freq(iFreq)%pat(0,locq(4,ipar),locq(5,ipar))
      ENDIF

      neq%par(ipar)%x0         = corr

      neq%par(ipar)%name       = antcal(2,irap)
      neq%par(ipar)%time%mean  = (timmax + timmin) / 2.d0
      neq%par(ipar)%time%half  = (timmax - timmin) / 2.d0
      neq%par(ipar)%scale      = 1.d0

      neq%par(ipar)%locq(5)    = locq(5,ipar)*100+prnrao(irao)
      neq%par(ipar)%locq(4)    = locq(4,ipar)*100+locq(3,ipar)
      neq%par(ipar)%locq(2:3)  = numcal(1:2,irap)

! Global ionosphere model parameters
! ----------------------------------
    CASE (19)
      IF (locq(2,ipar) /= 1) THEN
        WRITE(lfnerr,*) ' *** SR NEQWRITE: Unsupported GIM parameters'
        CALL EXITRC(2)
      ENDIF
      neq%par(ipar)%x0   = 0.0
      imod = locq(3,ipar)
      neq%par(ipar)%name = namgim(imod)
      IF (epogim(2,imod) == 0.0) THEN
        neq%par(ipar)%time%mean = epogim(1,imod)
        neq%par(ipar)%time%half = 0.0
      ELSE
        neq%par(ipar)%time%mean = (epogim(2,imod) + epogim(1,imod)) / 2.d0
        neq%par(ipar)%time%half = (epogim(2,imod) - epogim(1,imod)) / 2.d0
      ENDIF
      neq%par(ipar)%scale = scagim(1)

! Scaling factors for Vienna grid files
! -------------------------------------
    CASE (22)
      neq%par(ipar)%name    = opLoad(neq%par(ipar)%locq(2))%staLst(neq%par(ipar)%locq(3))
      neq%par(ipar)%locq(3) = opLoad(neq%par(ipar)%locq(2))%staClu(neq%par(ipar)%locq(3))
      IF ( neq%par(ipar)%locq(3) /= 0 ) neq%par(ipar)%name = ''

      neq%par(ipar)%x0      = 1d0
      neq%par(ipar)%scale   = 1d0

      neq%par(ipar)%time%mean = (timmax + timmin) / 2.d0
      neq%par(ipar)%time%half = (timmax - timmin) / 2.d0

! Receiver clocks
! ---------------
    CASE (23)

      iEpo = locq(4,iPar)
      iSta = locq(2,iPar)

      IF (nEpSam /= 0) THEN
        dSec   = DBLE(iEpo-1)*clkrec%epoch(1)/clkrec%nEpo
        numEpo = NINT(clkrec%epoch(1)/DBLE(nEpSam))+1
        jEpo   = NINT(dSec/clkrec%epoch(1)*(numEpo-1))+1

        neq%par(ipar)%x0 = clkrec%clock(iSta,jEpo) * C
      ELSE
        neq%par(ipar)%x0 = clkrec%clock(iSta,iEpo) * C

      ENDIF

      neq%par(ipar)%name = stName(iSta)
      neq%par(ipar)%time%mean = clkhed%tFirst + &
                            DBLE(iEpo-1)*clkRec%epoch(1)/clkRec%nEpo/86400.D0
      neq%par(ipar)%time%half = 0.d0
      neq%par(ipar)%scale = 1d0

! Satellite clocks
! ----------------
    CASE (24)
      iEpo = locq(4,iPar)
      iSat = locq(3,iPar)

      tMean = clkhed%tFirst + DBLE(iEpo-1)*clkRec%epoch(1)/clkRec%nEpo/86400.D0
      CALL gtsclk(rnxclk,tMean,iSat,secipl,2,neq%par(ipar)%x0,irc)
      neq%par(iPar)%x0 = neq%par(iPar)%x0 * C
      IF (irc /= 0) neq%par(ipar)%x0 = 0d0

      string1=' '
      CALL svn2chr(iSat,iSvn,cSvn)
      WRITE(string1,'(A,I2.2)') cSvn,iSvn
      neq%par(ipar)%name=string1

      neq%par(ipar)%time%mean = tMean
      neq%par(ipar)%time%half = 0.d0
      neq%par(ipar)%scale = 1d0

! Satellite antenna patterns
! --------------------------
    CASE (25)

! phase / code measurement: satellite antenna patterns
      IF (meatyp == 1 .OR. meatyp == 2) THEN

        ispv  =locq(2,ipar)
        izen  =locq(4,ipar)
        iazi  =locq(5,ipar)
        nptzen=locq(6,ipar)
        nptazi=locq(7,ipar)

        dnad=nadmax/DBLE(nptzen-1)
        nad=dnad*(izen-1)

        dazi=2.d0*pi/DBLE(nptazi-1)
        azi=dazi*(iazi-1)
!
! satellite specific antenna patterns?
        IF (first) THEN
          satspec = .TRUE.
          DO ii = 1, nanspv
            IF(nsaspv(ii) > 1) satspec = .FALSE.
          END DO
          first = .FALSE.
        ENDIF
        IF (satspec) neq%par(ipar)%locq(2) = 0

! find first satellite of group and replace PRN with SVN if satellite specific
        CALL prn2svn(4,satspv(1,ispv),(timmax + timmin)/2.d0,svnnr,timint,irc)
        IF(satspec) READ(svnnr,"(1X,I3)")neq%par(ipar)%locq(3)


!     Sat-spec. obstypes
        indgeos = 0
        IF ((PRESENT(USEGEOS)).AND.(PRESENT(GOBSDEF))) THEN
         IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
          DO igeos=1,gobsdef%norec
             satnumg=gobsdef%sat(igeos)%sysnum*100+gobsdef%sat(igeos)%satnum
           WRITE (satnumc,'(I3)') satnumg
           IF (satnumc.eq.svnnr(2:4)) indgeos = igeos
          ENDDO
         ENDIF
        ENDIF
! code a priori information into name
        WRITE(neq%par(ipar)%name,"(I1,I2,I4,I3,I5)") ifreq, &
          IDNINT(dnad*180.D0/pi*10d0), &
          IDNINT(dazi*180.D0/pi*10d0), &
          IDNINT(nadmax*180.D0/pi*10D0),&
          indgeos

! get a priori pattern values for first satellite of the group (nad, azi)
        CALL sat_pcv(satspv(1,ispv),(timmax + timmin)/2.d0,typeMWTR, &
                                                       ifreq,nad,azi,elvcor)
        neq%par(ipar)%x0         = elvcor
        neq%par(ipar)%time%mean  = (timmax + timmin) / 2.d0
        neq%par(ipar)%time%half  = (timmax - timmin) / 2.d0
        neq%par(ipar)%scale      = 1.0d0

! range measurement: satellite reflector patterns
      ELSEIF (meatyp == 3) THEN
        WRITE(lfnprt,'(/,A,/)') &
        ' *** SR NEQWRITE: satellite reflector ' // &
                                   'pattern is not implemented.'
      ELSE
        WRITE(lfnprt,'(/,A,I4,/)')' *** SR NEQWRITE: unkown meatyp: ', meatyp
        CALL exitrc(2)
      ENDIF

! SLR Range Biases
! ----------------
    CASE (26)

      neq%par(ipar)%name       = stname(locq(2,ipar))
      neq%par(ipar)%time%mean  = (timmax + timmin) / 2.d0
      neq%par(ipar)%time%half  = (timmax - timmin) / 2.d0
      neq%par(ipar)%scale      = 1.d0

      neq%par(ipar)%x0         = 0.d0

      epoch = .realToEpoch.neq%par(ipar)%time%mean
      found = 1

      CALL getrgb('RGB', neq%par(ipar)%name, neq%par(ipar)%locq(5),  &
                  neq%par(ipar)%locq(4), epoch, rgbReq, found)

      IF ( found == 0 )  neq%par(ipar)%x0 = rgbReq%value


! Higher-order ionosphere (HOI) scaling factors
! ---------------------------------------------
    CASE (27)
      neq%par(ipar)%x0   = 1d0
      neq%par(ipar)%name = ''
      IF (locq(3,ipar) == 2) THEN
        neq%par(ipar)%name = stname(locq(4,ipar))
      ENDIF
      neq%par(ipar)%time%mean  = (timmax + timmin) / 2d0
      neq%par(ipar)%time%half  = (timmax - timmin) / 2d0
      neq%par(ipar)%scale      = 1d0


! Helmert transformation parameters
! ---------------------------------
    CASE (28)
      neq%par(ipar)%x0   = 0d0
      neq%par(ipar)%name = ''

      neq%par(ipar)%time%mean  = (timmax + timmin) / 2d0
      neq%par(ipar)%time%half  = (timmax - timmin) / 2d0
      neq%par(ipar)%scale      = 1d0


! GNSS-specific station translations
! ----------------------------------
      CASE(30)
        neq%par(ipar)%x0         = 0.d0
        neq%par(ipar)%name       = stname(locq(2,ipar))
        neq%par(ipar)%time%mean  = (timmax + timmin) / 2.d0
        neq%par(ipar)%time%half  = (timmax - timmin) / 2.d0
        neq%par(ipar)%scale      = 1.d0
        neq%par(iPar)%locq(2)    = 0


! Wrong parameter type
! --------------------
    CASE DEFAULT
      WRITE(lfnerr,*) ' *** SR NEQWRITE : WRONG PARAMETER TYPE ', locq(1,ipar)
      CALL EXITRC(2)
    END SELECT

    neq%par(ipar)%obstim%t = (/ 0d0, 1d20 /)

  END DO


! Copy miscellaneous information
! ------------------------------
! Version 2: Long characters for stanam, antnam
! Version 4: Long characters for par%name (t_par, version <=2: t_par_v2)

  neq%version = 8         ! be careful, it has to be synchronized with
                          ! the development version
! RD: Remark: neq%version is defined in NEQSTORE

  neq%misc%title     = title
  neq%misc%datum     = datum
  neq%misc%nutmod    = nutnam
  neq%misc%submod    = subnam
  neq%misc%orbFil(1) = filstd
  neq%misc%orbFil(2) = filrpr
  neq%misc%gravFil   = ''
  neq%misc%npar      = npar
  neq%misc%nobs      = DBLE(nobs)
  neq%misc%nparms    = DBLE(nparms)
  neq%misc%lTPl      = lTPl
  neq%misc%nftot     = nftot
  neq%misc%nsmpnq    = nsmpnq
  neq%misc%ielvnq    = ielvnq
  neq%misc%itropo    = itropo
  neq%misc%iextra    = iextra
  neq%misc%itrmap    = itrmap
  neq%misc%itrgrd    = itrgrd(1)
  IF ( SIZE(neq%misc%grdNeq) /= SIZE(grdNeq)-1 ) THEN
    WRITE(LFNERR,'(/,A,2(/,18X,A),2(/,18X,A,I6),/)')                      &
    ' *** SR NEQWRITE: Scaling factors of the Vienna grid files.',        &
      'The records for the keywords is not compatible in ',               &
      'D_NEQ.f90 (t_neq) and D_GRID.f90 (grdNeq).:',                      &
      'Size of grdNeq in "${I}/D_GRID.f90":         ', SIZE(grdNeq)-1,    &
      'Size of neq%misc%grdNeq in "${I}/D_NEQ.f90": ', SIZE(neq%misc%grdNeq)
    CALL EXITRC(2)
  ENDIF
  neq%misc%grdNeq    = grdNeq(1:)


! Information concerning satellite antenna offsets (resp. patterns)
! -----------------------------------------------------------------
  IF ( nanoff > maxOff ) THEN
    WRITE(lfnerr,*) ' *** SR NEQWRITE: maxOff < ', nanoff
    CALL exitrc(2)
  END IF
!
  DO ii = 1, nanoff
    IF ( nsaoff(ii) > maxSat ) THEN
      WRITE(lfnerr,*) ' *** SR NEQWRITE: maxSat < ', nsaoff(ii)
      CALL exitrc(2)
    END IF
  END DO

! copy pattern group definition to "nanoff" and "satoff" if
!                                     satellite specific offset groups
  IF (grpoff == 0) nanoff = nanspv
!
  IF ( nanspv > maxOff ) THEN
    WRITE(lfnerr,*) ' *** SR NEQWRITE: maxOff < ',nanspv
    CALL exitrc(2)
  END IF

  DO ii = 1, nanspv
    IF ( nsaspv(ii) > maxSat ) THEN
      WRITE(lfnerr,*) ' *** SR NEQWRITE: maxSat < ',nsaspv(ii)
      CALL exitrc(2)
    END IF
    IF (grpoff == 0) THEN
      nsaoff(ii) = nsaspv(ii)
      satoff(1:nsaoff(ii),ii) = satspv(1:nsaspv(ii),ii)
    ENDIF
  END DO
!
  IF(grpoff /= 0 .AND..NOT. satspec .AND. nanoff /= nanspv .AND.    &
     nanoff /= 0 .AND. nanspv /= 0) THEN
    WRITE(lfnerr,"(' *** SR NEQWRITE: Different group definitions', &
                   &           /,18X,'offset groups :',I3,          &
                   &     /,18X,'pattern groups:',I3)")nanoff,nanspv
    CALL exitrc(2)
  ENDIF
!
  IF (grpoff == 0 .AND. satspec) nanoff = 0

  neq%misc%nanoff = nanoff
  IF (nanoff /= 0) THEN
    DO ii = 1, nanoff
      neq%misc%satoff(1:nsaoff(ii),ii) = satoff(1:nsaoff(ii),ii)
      neq%misc%nsaoff(ii)              = nsaoff(ii)
    ENDDO
  ENDIF

! Information needed for SINEX format
! -----------------------------------
  neq%misc%nstat_sinex = nstat

  IF ( neq%misc%nstat_sinex > maxStaSin ) THEN
    WRITE(lfnerr,*) ' *** SR NEQWRITE: maxStaSin < ', neq%misc%nstat_sinex
    CALL exitrc(2)
  END IF

  neq%misc%sinex(1:nstat) = antsnx(1:nstat)

  IF ( neq%misc%npar > 0 ) THEN
    ii = IKF(neq%misc%npar,neq%misc%npar)
    neq%anor(1:ii) = anor(1:ii)
    ii = neq%misc%npar
    neq%bnor(1:ii) = bnor(1:ii)
  ENDIF


! Information needed for sat-spec obstype selection
! -------------------------------------------------
  IF ((PRESENT(USEGEOS)).AND.(PRESENT(GOBSDEF))) THEN
   IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
    IF ( gobsdef%norec > maxObst ) THEN
      WRITE(lfnerr,*) ' *** SR NEQWRITE: maxObst < ', gobsdef%norec
      CALL exitrc(2)
    END IF

    neq%misc%nobst = gobsdef%norec

    DO igeos=1,gobsdef%norec
      DO obs=1,4
       neq%misc%obst(igeos)%obstyp(obs) = gobsdef%sat(igeos)%obstyp(obs)
      ENDDO
    ENDDO
   ENDIF
  ENDIF


! Clean the NEQ from the dummy parameters
! ---------------------------------------
  CALL neqclean(neq)

! Write the neq-file
! ------------------
  CALL neqstore(filneq, neq)

END SUBROUTINE neqwrite

END MODULE
