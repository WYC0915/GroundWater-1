MODULE s_RDINPT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdinpt(maxsta,maxfil,maxtrm,maxtrp,maxpol,maxwgt,maxsat,maxstc, &
                  maxoff,maxofr,maxhil,maxpot,maxalb,maxtyp,maxcal,maxgim, &
                  maxsgr,nInpFiles, globalWindow, nAllSta, allStaNum,      &
                  allStaName,nAllSat,allSatNum,stitle,priopt,stramb,sigamb,&
                  ar2mod,ar2inf,corstr,dtsim ,sigapr,icoelv,nsampl,noInClk,&
                  secIpl,zenmax,zmxleo,itropo,iextra,isyncr,iloc  ,ihelm , &
                  norb  ,seqorb,prec  ,optdcb,sigdcb,nfix  ,stfix ,nkin  , &
                  stkin ,nstwgt,istwgt,stwgt,wgtFile,nclreq,istclk,isaclk, &
                  nclk  ,ibias ,clkwgt,clfrto,nRec  ,isbTim,ntrsta,statrp, &
                  trplms,sigtrs,isgtrs,ntrreq,npartr,trplim,sigtrp,itrmap, &
                  itrgrd,nioreq,ionmod,ionreq,polmod,polpar,hpsave,npol  , &
                  tpol  ,sigpol,isgpol,isgnut,nwgt  ,satwgt,timwgt,wgtwgt, &
                  nstcep,frctyp,nsastc,numstc,nstday,sigstc,nspec ,numspc, &
                  timspc,sigspc,nanoff,nsaoff,satoff,paroff,nrqoff,grpoff, &
                  sigoff,timoff,isgoff,nhill ,hiltyp,sighil,npot  ,pottyp, &
                  sigpot,nalb  ,albtyp,sigalb,nalbgr,nsaalb,satalb,ncenm , &
                  cenmas,sigcen,nrgb  ,stargb,satSpec,sigrgb,opthoi,sighoi,&
                  opteli,optpar,optdip,sigdip,nancal,antcal,numcal,prncal, &
                  nfrcal,nptcal,sigcal,optgim,polgim,siggim,namgim,epogim, &
                  nanrao,antrao,numrao,prnrao,nfrrao,sigrao,neurao,nclkst, &
                  nclksa,clksta,clksat,edtlvl,izerod,nepobs,clksys,clkhed, &
                  norb2,seqorb2,nstcep2,frctyp2,prec2,nstday2,numstc2,nsastc2,&
                  sigstc2,nspec2,numspc2,timspc2,sigspc2,ieppar,norres,iraux2,&
                  iorest,nEstSat,estSat,noSol,iqxx  ,iphsep,maxspv,nanspv, &
                  nsaspv,satspv,gnrspv,nptspv,sigspv,nadmax,nutnam,subnam, &
                  isasys,rapzenmax,gnroff,ipolar,opLoad,optGsp,irel2)

! -------------------------------------------------------------------------
! Purpose:    This is a new version of the old subroutine RDINPT.f that
!             reads the input options of the program GPSEST
!
! Author:     L. Mervart
!
! Created:    06-Jun-2000
!
! Changes:    17-Apr-2001 JD: Correct handling of preelim. inputs
!             31-May-2001 RD: Correct keywords ELVWGT and RESIDRS
!                             Read clkobs and clk RINEX informations
!                             Allow niell mapping functions
!                             Extent boundaries for tropo param.
!                             Correct handling of the time window for trp-param.
!                             Handle empty field and sigma = 0d0 for trp.
!             01-Jun-2001 RD: Extent boundaries for erp
!                             Correct handling of the time window for erp
!                             Allow empty fields for no sigma for erp
!                             Correct handling of cont. erp (as it was GPSEST_P)
!                             Read "special requ." only if selected
!                             Put more information into the error messages
!             07-Jun-2001 RD: Use only one session table
!             08-Jun-2001 RD: Read a special first sigma(UT1-UTC) and sigma(nut)
!                             Make sure that never a gap between trp win occure
!                             Enable P1-C1 DCB estimation
!                             Allow an empty field for no sigma for dcb
!                             Correct keyword COORDSYS (helmert)
!                             Enable global system for helmert
!                             Allow empty fields for no sigma for geocenter
!                             Allow empty fields for no sigma for stoch iono.
!                             Allow empty fields for no sigma for orbit est.
!                             Error handling for stoch. orbital elem.
!             13-Jun-2001 RD: Enable special requ. for stoch. orbit param.
!             15-Jun-2001 RD: Correct handling of time window for rec. clock
!                             Allow empty field for no sigma for rec. clock
!                             Correct reading of the sat.ant.offset line
!                             Recover the advanced setup from GPSESTI file
!                                (time window, relative sigma, ...)
!                             Correct reading of the rec.ant.offset line
!             19-Jun-2001 RD: Correct reading of the rec.ant.PCV line
!             20-Jun-2001 RD: A more comfortable menu input for rec.ant.offset
!                             A more comfortable menu input for rec.ant.pcv
!                             Put reading of rec.ant.offset options into a SR
!                             Put reading of rec.ant.pcv options into a SR
!                             Put reading of sat.ant.offset options into a SR
!             21-Jun-2001 RD: Put reading of orbit estimation options into a SR
!                             No core dumps for bad print options
!             22-Jun-2001 RD: Error handling for ambiguity parameters
!                             Put reading of ambiguity options into a SR
!                             Put reading of rec.clk.offset options into a SR
!             25-Jun-2001 RD: Recover special station setup for tropo-param.
!                             Put reading of troposphere parameter opt into a SR
!             26-Jun-2001 RD: Recover local troposphere
!                             Put reading of local troposphere options into a SR
!                             Put reading of erp. options into a SR
!                             DCB reference satellite specification
!                             Put reading of DCB options into a SR
!             27-Jun-2001 RD: Correct unit for glob. ionosphere characterization
!                             Add a maximum dimension check for glob. ionosphere
!                             Handle empty fields for sigma for glob. ionosphere
!                             Better handling of the time interval for GIM
!                             Correct default setting of optgim for "HGT-GIM"
!                             Stop if "HGT-GIM" without apriori model
!                             Put reading of GIM options into a SR
!                             Use SR DIMTST for "max-checks"
!                             Put reading of center of mass options into a SR
!                             Recover the local ionosphere modelling
!                             Put reading of stoch. ionosphere options into a SR
!             28-Jun-2001 RD: Reference clock selection via FIRST, LAST, ...
!                             Read clk rinex header information from input file
!                             Put reading of clock est. options into a SR
!                             New call of SR gtStaNum
!                             Restructure the input panels
!             29-Jun-2001 RD: Separate coord sigma for north, east, up
!                             Make the satellite spec. weighting work
!                             Put some more stuff into SRs
!                             Reactivate earth albedo radiation estimation
!             03-Jul-2001 RD: Reactivate Hill's resonance estimation
!                             Reactivate gravity field estimation
!             04-Jul-2001 RD: New call of gtstanum
!             31-Jul-2001 RD: Interface for sr rdirco necessary
!             28-Aug-2001 RD: New call for SR gtStaNum
!             29-Jan-2002 RD: Residual file is not requ. to get results
!                               for epoch parameters (clk, kin.crd)
!             30-Jan-2001 RD: Set IZEROD also for kin. coord. (opteli(21)==3)
!             07-May-2002 SS: DCB update
!             27-Jun-2002 DS: Add LEO options: zmxleo, icoelv(2)
!             28-Jun-2002 DS: Add LEO orbit parameters
!             18-Jul-2002 MR: izerod for single difference files, remove check
!             04-Sep-2002 RD: Skip obs. if no sat-clk available
!             08-Oct-2002 RD: New call for SR gtStaNum
!             11-Nov-2002 RS: Add options for satellite antenna phase center
!                             variations
!             10-Dec-2002 CU: New parameter wgtFile (a priori crd sigma file)
!             14-Jan-2003 RS: Add default value for nadmax
!             28-Jan-2003 RD: Number of obs. for kin. pos.(clkobs->nepobs)
!                             Init iraux2
!                             Allow kin.pos. from baselines via RESEPO
!             30-Jan-2003 RD: Allow clock est. only if ZD-files are selected
!             03-Feb-2003 PS: Add nutnam and subnam
!             03-Feb-2003 RD: Correct handling of the ECCENT file
!                             New call of GTSTANUM (weight is pointer now)
!             15-Apr-2003 RD: Move reading of EDTLVL from RDICLK to RDIGEN
!             23-Apr-2003 RD: Nullify local pointers
!             30-Apr-2003 SS: Satellite system selection
!             15-May-2003 HU: Initialize structures
!             27-Jun-2003 RD: Move input for kin. coord. into RDIKIN
!             22-Jul-2003 RD: Read constraints for kin. pos. in RDIKIN
!             22-Jul-2003 RD: Normalized on apriori weights only
!             08-Sep-2003 HU: antnam, recnam chr16 -> chr20
!             04-Nov-2003 HB: Declare allStaName with (:)
!             10-Nov-2003 RS: Add rapzenmax, initialization of nsaoff and
!                             nsaspv, add gnroff, grpspv -> gnrspv, change
!                             call of rdisao
!             19-Jan-2004 SS/MM: Revision of GPSEST input panels
!             26-Jan-2004 HU: '1' instead of 'NO' for ORBADJ
!             09-Nov-2004 RD: Print statistic on parameter dimensions
!             03-Jan-2005 RD: Correct correl. for epo-param, see CHKOPT.f
!             26-May-2005 RD: Use dimension MAXSGR for SATOFF and SATSPV
!             07-Aug-2005 CU: Change initial value of RAPZENMAX from 90 to 0
!             10-Jan-2006 RD/HB: Statistics on phase-connected epochs
!             18-Sep-2006 HU: Read polarization effect option
!             04-Oct-2006 AG: Satellite specific antenna PCO/PCV implemented
!             16-Oct-2006 RD: Manual selection of satellites for orbit determ.
!             15-Jun-2007 RD: Only phase for resubstitution of epo-param.
!             01-Nov-2007 HB: Add parameter secIpl
!             23-Jan-2008 RD: System-specific RAO+RAP
!             29-Jan-2008 SL: More printing options
!             30-Jun-2008 RD: VMF added
!             02-Apr-2009 DT: Add range biases (nrgb,stargb,satSpec,sigrgb)
!             30-Apr-2009 RD: Scaling of loading models added
!             09-May-2009 RD: Sat/frq-specific receiver clock biases
!             09-May-2009 RD: Seperate receiver clocks for GPS/GLONASS
!             27-May-2009 RD: Special sampling for resubstition of epoch param.
!             29-May-2009 RD: Input clocks also from input clk rnx file
!             04-Jan-2010 SL: HOI scaling parameters for L1&L2 and L3 added
!             27-May-2010 RD: Read ISASYS already in OBSINF
!             08-Sep-2010 RD: Merge SLR-time bias option
!             20-Sep-2010 RD: Ambiguity interval only for phase files
!             14-Oct-2010 RD: Read tropo-model from SR trpopt
!             26-Oct-2010 CR: Read flag for Periodic Relativistic J2-Correction
!             25-Nov-2010 MM: GNSS-specific parameters
!             16-Feb-2011 SS: sigamb(5) for GLONASS ambiguity resolution
!             17-Feb-2011 SS: stramb(3) for selection of GNSS
!             17-Feb-2011 SS: LAMBDA ambiguity resolution strategy
!             06-May-2011 HB: Add rdstdh to initialize model names
!             17-Nov-2011 HB: Add isLeo as parameter to SR rdigen
!             15-Feb-2012 RD: RCO are not in "moreOpt" anymore
!             06-Jun-2012 MM: Adapted to new option GEOTEC, GLOBIAS
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, &
                      keyNameLength, keyValueLength, &
                      staNameLength, shortLineLength, fileNameLength
  USE m_time,   ONLY: t_timint
  USE d_const,  ONLY: pi
  USE d_clkrnx, ONLY: t_clkhead
  USE d_eccent, ONLY: t_eccent,init_eccent
  USE d_grid,   ONLY: grdNeq
  USE d_stdorb, ONLY: t_stdhead, init_stdHead
  USE p_gpsest, ONLY: t_optload, t_isbTime, t_optGsp
  USE s_rdirap
  USE s_rdirco
  USE s_rdigen
  USE s_rditrm
  USE s_alcerr
  USE s_rdiswg
  USE s_rditrp
  USE s_rdiorb
  USE s_rdigim
  USE s_rdicom
  USE s_rdidcb
  USE s_rdialb
  USE s_rdipos
  USE s_rdikin
  USE s_rdipot
  USE s_rdiion
  USE s_rdisao
  USE s_rdisap
  USE s_rdiclk
  USE s_rdidip
  USE s_rdihoi
  USE s_rdigsp
  USE s_readkeys
  USE s_rdielm
  USE s_rdihil
  USE s_exitrc
  USE s_ckoptb
  USE s_rdiamb
  USE s_rdnutsub
  USE s_gtflna
  USE s_rdierp
  USE s_readecc
  USE s_rdirao
  USE s_rdibias
  USE s_rdigrd
  USE s_rdstdh

  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b) :: maxsta ! maximum number of stations
  INTEGER(i4b) :: maxfil ! maximum number of files
  INTEGER(i4b) :: maxtrm ! maximum number of troposphere models
  INTEGER(i4b) :: maxtrp ! maximum number of trop. parameters per model
  INTEGER(i4b) :: maxpol ! maximum sets of earth orientation parameters
  INTEGER(i4b) :: maxwgt ! maximum number of satellite specific sigmas
  INTEGER(i4b) :: maxsat ! maximum number of satellites
  INTEGER(i4b) :: maxstc ! maximum number of special stochast. requests
  INTEGER(i4b) :: maxoff ! maximum number of sat. antenna groups
  INTEGER(i4b) :: maxofr ! maximum number of sat. ant. requests
  INTEGER(i4b) :: maxhil ! maximum number of hill parameters
  INTEGER(i4b) :: maxpot ! maximum number of potential parms
  INTEGER(i4b) :: maxalb ! maximum number of albedo groups
  INTEGER(i4b) :: maxtyp ! maximum number of parameter types
  INTEGER(i4b) :: maxcal ! maximum number of antenna phase center parameters
  INTEGER(i4b) :: maxgim ! maximum number of global/local ionosphere models
  INTEGER(i4b) :: maxspv ! maximum number of satellite antenna phase center
                         ! variation groups
  INTEGER(i4b) :: maxsgr ! maximum number of satellites per group (SATOFF,SATSPV)

  INTEGER(i4b), DIMENSION(:)   :: nInpFiles    ! number of input files
                                               ! i=1: phase zero differences
                                               ! i=2: code zero differences
                                               ! i=3: phase single differences
                                               ! i=4: code single differences
                                               ! i=5: slr range observations
  TYPE(t_timint)               :: globalWindow ! window to be processed
                                               ! (from - to, MJD)
  INTEGER(i4b)                 :: nAllSta      ! number of all stations
  INTEGER(i4b), DIMENSION(*)   :: allStaNum    ! station numbers
  CHARACTER(LEN=staNameLength), &
                DIMENSION(:)   :: allStaName   ! station names
  INTEGER(i4b)                 :: nAllSat      ! number of all satellites
  INTEGER(i4b), DIMENSION(*)   :: allSatNum    ! satellite numbers

! output:
  CHARACTER(LEN=*)             :: stitle ! short title
  INTEGER(i4b), DIMENSION(*)   :: priopt ! print options (1: YES)
                                         ! i= 1: number of observations
                                         ! i= 2: pos. ecc. and receiver info
                                         ! i= 3: clock coefficients (code)
                                         ! i= 4: ambiguities
                                         ! i= 5: parameter charac. list
                                         ! i= 6: constants, antenna offsets,
                                         !       ionospheric coefficients
                                         ! i= 7: satellite elevations
                                         ! i= 8: synchronization errors
                                         ! i= 9: number of observ. per file
                                         ! i=10: ambiguities every iteration
                                         ! i=11: observation statistics
                                         !       (elevation)
                                         ! i=12: suppress print of epoch param.
                                         ! i=13: observation statistics
                                         !       (nadir angle)
                                         ! i=14: print no troposphere parameters
                                         ! i=15: print no coordinate parameters
                                         ! i=16: print no ambiguity parameters
                                         ! i=17: rms of crd+crd diff.
                                         ! i=18: slope distances
                                         ! i=19: Print array dimensions
                                         ! i=20: statistics on phase-connected epochs
  INTEGER(i4b), DIMENSION(*)   :: stramb ! i=1: ambiguity resolution strategy
                                         !   -1: ambiguity pre-elimination
                                         !    0: no ambiguity resolution
                                         !    1: round-to-nearest-integer
                                         !    2: general search
                                         !    3: sigma-dependent
                                         !    4: quasi-ionosphere-free
                                         !    5: LAMBDA
                                         ! i=2: ambiguity pre-elimination
                                         !   -1: once per session
                                         !    0: every epoch
                                         !    n: every n seconds
                                         ! i=3: selection of GNSS
                                         !    0: all
                                         !    1: GPS
                                         !    2: GLONASS
                                         !    3: GALILEO
  REAL(r8b),    DIMENSION(*)   :: sigamb
                                         ! options for SIGMA-dependent or QIF
                                         ! ambiguity resolution strategy
                                         ! if stramb(1)=3 (SIGMA)
                                         !   sigamb(1): at least 1 integer
                                         !              within sigamb(1)*sigma
                                         !              allowed
                                         !   sigamb(2): maximum  sigma allowed
                                         !              for the ambiguity which
                                         !              should be resolved
                                         !   sigamb(3): minimal sigma used in
                                         !              test
                                         !   sigamb(4): max. number of
                                         !              ambiguities to be
                                         !              solved in one iteration
                                         !              step
                                         !   sigamb(5): GLONASS ambiguity resolution
                                         !              between different frequency
                                         !              channels:
                                         !              = 0: never
                                         !              = 1: same reveiver type
                                         !              = 2: same reveiver model
                                         !              = 3: same reveiver group
                                         !              =-1: always
                                         ! if stramb(1)=4 (QIF)
                                         !   sigamb(1): search width in
                                         !              wide-lane cycles
                                         !   sigamb(2): maximal allowed rms
                                         !              error of narrow-lane
                                         !              ambiguity
                                         !              (bet13*x1+bet23*x2) in
                                         !              narrow-lane cycles
                                         !   sigamb(3): maximal allowed
                                         !              distance in l1&l2 space
                                         !              from grid point which
                                         !              is supposed to be the
                                         !              correct solution
                                         !              (in narrow-lane cycles)
                                         ! if stramb(1)=5 (LAMBDA)
                                         !   sigamb(1): maximum allowed rms ratio
                                         !              concerning fixed to float
                                         !   sigamb(2): maximum allowed rms of unit
                                         !              weight for fixed solution
                                         !   sigamb(3): resolution mode concerning
                                         !              involved GNSS
                                         !              =1: GNSS by GNSS
                                         !              =2: mixed-GNSS
                                         !              =3: separate-GNSS
  INTEGER(i4b)                 :: ar2mod ! mode for amb. res. strategy 2
                                         ! (SEARCH)
                                         ! 0: resolve all present ambiguities,
                                         ! 1: baseline-wise ambiguity resolution
  REAL(r8b),    DIMENSION(*)   :: ar2inf ! options for amb. res. strategy 2
                                         ! (SEARCH)
                                         ! 1: search width in units of std dev
                                         ! 2: max allowed rms(fixed)/rms(float)
                                         ! 3: min allowed
                                         !    rms(2-nd amb)/rms(1-st amb)
                                         ! 4: search width for geometry-
                                         !    free lc (in l1 cycles)
                                         !    =x : use this value
                                         !    =0 : compute formal width
  INTEGER(i4b)                 :: corstr ! correlation strategy
  REAL(r8b)                    :: dtsim  ! max. interval to identify epoch
                                         ! (in days)
  REAL(r8b)                    :: sigapr ! a priori sigma
  INTEGER(i4b),DIMENSION(:,:)  :: icoelv ! model for elev.-dep. obs. weighting
                                         !  0: equal weighting for all obs.
                                         ! >0: model number (see sr WGTELV)
                                         ! i=1: station i=2: LEOs
                                         ! j=meatyp
  INTEGER(i4b),DIMENSION(3)    :: nsampl ! sampling rate (sec)
                                         ! 1: observations
                                         ! 2: resubstitution of epoch parameters
                                         ! 3: preeliminate of epoch parameters
  INTEGER(i4b),DIMENSION(3)    :: noInClk! What to do if no input clock:
                                         ! noInClk(1): rec from clk rnx
                                         !  -1: ignore clock rinex file
                                         !   0: Use obs. (rec from obs-file)
                                         !   1: Use obs. (interpol. clk rnx)
                                         !   2: Skip obs.
                                         ! noInClk(2): sat from clk rnx
                                         !  -1: ignore clock rinex file
                                         !   0: Try also sat clk file
                                         !   1: Use obs. (interpol. clk rnx)
                                         !   2: Skip obs.
                                         !   3: Use obs. (sat clk = zero)
                                         ! noInClk(3): sat from sat clk file
                                         !   2: Skip obs.
                                         !   3: Use obs. (sat clk = zero)
  REAL(r8b)                    :: secipl ! maximum interval allowed for clock
                                         ! interpolation (sec)
  REAL(r8b)                    :: zenmax ! maximum zenith distance (in rad)
  REAL(r8b)                    :: zmxLeo ! maximum zenith distance for LEOs
  INTEGER(i4b)                 :: itropo ! tropospheric model
  INTEGER(i4b)                 :: iextra ! =0 : use measured values
                                         ! =1 : use atm. model values
                                         ! =2 : use est. bernese values
  INTEGER(i4b)                 :: isyncr ! apply synchronization errors (1: YES)
                                         ! do not apply synchr. errors   = 0
  INTEGER(i4b)                 :: iloc   ! local(n,e,up) or geocentric system
                                         ! for helmert transformation
  INTEGER(i4b), DIMENSION(:)   :: ihelm  ! flags for helmert-parameters
  INTEGER(i4b)                 :: norb   ! number of orbital parameters to
                                         ! be estimated
  INTEGER(i4b), DIMENSION(*)   :: seqorb ! sequence for orbital elements
  REAL(r8b),    DIMENSION(*)   :: prec   ! a priori orbital precisions
  INTEGER(i4b), DIMENSION(*)   :: optdcb ! options for estimation of
                                         ! differential code biases
                                         ! (1): estimate dcbs for satellites
                                         !      = 0: NO
                                         !      = 1: P1-P2
                                         !      = 2: P1-C1
                                         !      = 3: LC
                                         ! (2): estimate dsbs for receivers
                                         !      = 0: NO
                                         !      = 1: P1-P2
                                         !      = 2: P1-C1
                                         !      = 3: LC
                                         !      =-2: P1-C1_MULTIPLIER
                                         ! (3): reference satellite number
                                         !      = 0: constrain all sat
                                         !      =-1: constrain sum of all sat
                                         ! (4): special observ. data selection
                                         !      = 0: NO
                                         !      = 1: NIGHT-TIME
                                         !      = 2: NON-ECLIPSING
                                         !      = 3: NON-ECLIPSING only for
                                         !           GPS block I, II, IIA
  REAL(r8b),    DIMENSION(*)   :: sigdcb ! a priori sigma for dcbs (in ns)
                                         ! (1): reference satellite biases
                                         ! (2): receiver biases
  INTEGER(i4b)                 :: nfix   ! number of stations held fixed
  INTEGER(i4b), DIMENSION(:)   :: stfix  ! numbers of the stations held fixed
  INTEGER(i4b)                 :: nkin   ! number of stations estimated
                                         ! in kin. modus
  INTEGER(i4b), DIMENSION(:)   :: stkin  ! numbers of the kin. stations
  INTEGER(i4b)                 :: nstwgt ! # stations with a priori weights
                                         ! for coordinates
  INTEGER(i4b), DIMENSION(:)   :: istwgt ! numbers of the stat. with weights
  REAL(r8b),    DIMENSION(:,:) :: stwgt  ! a priori weights for stations
  CHARACTER(LEN=shortLineLength) :: wgtFile! sigma file
  INTEGER(i4b)                 :: nclreq ! # clock error requests
  INTEGER(i4b), DIMENSION(:)   :: istclk ! station numbers for clock requests
  INTEGER(i4b), DIMENSION(:)   :: isaclk ! satellite numbers for clock requests
  INTEGER(i4b), DIMENSION(*)   :: nclk   ! # clock parameters to be estimated
                                         ! nclk(i)=1 : offset estimated for
                                         ! station i
  INTEGER(i4b), DIMENSION(*)   :: iBias  ! 0: station specific
                                         ! 1: frequency specific
                                         ! 2: satellite specific
                                         ! 3: sat. specific for non-GPS
                                         ! 4: frequency specific with polynom
  REAL(r8b),    DIMENSION(2,*) :: clkwgt ! a priori weight for clocks (in sec)
  REAL(r8b),    DIMENSION(2,*) :: clfrto ! time interval for clock errors
                                         ! clfrto(1,k): start of the interval
                                         ! clfrto(2,k): end of the interval
                                         ! in julian date for clock request k
  INTEGER(i4b)                 :: nRec   ! Number of isb-requests
  TYPE(t_isbTime),                 &
          DIMENSION(:), POINTER:: isbTim ! Time-dep. inter-syst. biases
  INTEGER(i4b)                 :: ntrsta ! number of troposphere requests
                                         ! for individual stations
  INTEGER(i4b), DIMENSION(*)   :: statrp ! station numbers for trop. requests
  REAL(r8b),    DIMENSION(2,*) :: trplms ! troposphere parameter est. from .. to
  REAL(r8b),    DIMENSION(3,*) :: sigtrs ! a priori sigma
                                         ! in m for indiv. troposphere param.
                                         ! j=1: north (gradient)
                                         ! j=2: east (gradient)
                                         ! j=3: up (zenith delay)
  INTEGER(i4b), DIMENSION(*)   :: isgtrs ! type of sigma
                                         ! 0: absolute sigma
                                         ! 1: sigma relative to the previous
                                         !    parameter of the same site
  INTEGER(i4b)                 :: ntrreq ! number of local tropos. model requests
  INTEGER(i4b), DIMENSION(*)   :: npartr ! number of parameters in request i
  REAL(r8b),    DIMENSION(2,*) :: trplim ! time interval (mjd) for request i
  REAL(r8b),    DIMENSION(*)   :: sigtrp ! a priori sigmas for troposphere
                                         ! parameters in m, m/(100m),
                                         ! m/(100m)**2, ...
  INTEGER(i4b)                 :: itrmap ! mapping function for troposp.est.
                                         ! 1:   1/cos(z)
                                         ! 2:   hopfield
                                         ! 3,4: dry/wet niell
                                         ! 5,6: dry/wet gmf
                                         ! 7,8: dry/wet vmf
  INTEGER(i4b), DIMENSION(*)   :: itrgrd ! i=1: est. of tropospheric gradients
                                         !      0: no estimation
                                         !      1: tilting
                                         !      2: linear
                                         ! i=2: ratio of number of zenith
                                         !      to gradient parameters
  INTEGER(i4b)                 :: nioreq ! # ionosphere requests
  INTEGER(i4b), DIMENSION(*)   :: ionmod ! number  of ionosphere model to be
                                         ! improved for request i
  INTEGER(i4b), DIMENSION(3,*) :: ionreq ! degree of development
                                         ! 1: in latitude,
                                         ! 2: deg. of development in hour angle,
                                         ! 3: max. degree of mixed coefficients
  INTEGER(i4b)                 :: polmod ! model of polar wobble
  INTEGER(i4b), DIMENSION(*)   :: polpar ! parameters to be estimated
                                         ! (1)=xp, (2)=yp, (3)=dt (4)=eps,(5)=psi
                                         ! 1 := estimated 0 := not estimated
  REAL(r8b),    DIMENSION(*)   :: hpsave ! time resolution (hours)
                                         ! (1): for Bernese pole format
                                         ! (2): for IERS pole format
  INTEGER(i4b)                 :: npol   ! number of pole parameter sets
  REAL(r8b),    DIMENSION(2,*) :: tpol   ! time intervals of pole parameter sets
  REAL(r8b),    DIMENSION(5,*) :: sigpol ! a priori sigma of pole parameters
                                         ! 1-5 := xp,yp,dt,de,dp *:= 1..maxpol
                                         ! 1,2,4,5 given in mas, 3 in msec
  INTEGER(i4b), DIMENSION(*)   :: isgpol ! earth rotation parameter sigmas
                                         ! 0 : apply for relevant parameter
                                         !     only the absolute constraints
                                         !     given in input option file
                                         ! 1 : ensure continuity with respect
                                         !     to previous polynomial (in add.
                                         !     to absolute constraints)
                                         ! 4 : ensure continuity with respect
                                         !     to previous polynomial and
                                         !     constrain drifts to zero
  INTEGER(i4b), DIMENSION(*)   :: isgnut ! earth orientation parameter sigmas
                                         ! 0 : apply for relevant parameter
                                         !     only the absolute constraints
                                         !     given in input option file
                                         ! 1 : ensure continuity with respect
                                         !     to previous polynomial (in add.
                                         !     to absolute constraints)
                                         ! 4 : ensure continuity with respect
                                         !     to previous polynomial and
                                         !     constrain drifts to zero
  INTEGER(i4b)                 :: nwgt   ! number of intervals with
                                         ! weighted satellites
  INTEGER(i4b), DIMENSION(*)   :: satwgt ! numbers of weighted satellites
  REAL(r8b),    DIMENSION(2,*) :: timwgt ! time intervals with weighted
                                         ! satellites (MJD)
  REAL(r8b),    DIMENSION(*)   :: wgtwgt ! satellite specific sigma
  INTEGER(i4b)                 :: nstcep ! number of stochastic forces per epoch
  INTEGER(i4b), DIMENSION(*)   :: frctyp ! stoch. force types for
                                         ! max 3 forces per epoch
                                         ! (1) force = r : radial
                                         ! (2) force = s : normal to r
                                         !                 in orb. plane
                                         ! (3) force = w : normal to orb plane
                                         ! (4) force = direction sun -->
                                         !             satellite
                                         ! (5) force = y direction of
                                         !             space craft
                                         ! (6) force = x direction of
                                         !             space craft
  INTEGER(i4b)                 :: nsastc ! number of sats with stoch. forces
  INTEGER(i4b), DIMENSION(*)   :: numstc ! satellite numbers
  INTEGER(i4b), DIMENSION(*)   :: nstday ! number of "stoch epochs" per day
  REAL(r8b),    DIMENSION(3,*) :: sigstc ! a priori sigmas for the 3 force types
  INTEGER(i4b)                 :: nspec  ! number of special stoch. requests
  INTEGER(i4b), DIMENSION(*)   :: numspc ! satellite numbers
  REAL(r8b),    DIMENSION(*)   :: timspc ! times for these requests
  REAL(r8b),    DIMENSION(3,*) :: sigspc ! corresponsing a priori sigmas
  INTEGER(i4b)                 :: nanoff ! number of satellite antenna offset
                                         ! groups to be estimated
  INTEGER(i4b), DIMENSION(*)   :: nsaoff ! number of satellites belonging to
                                         ! group i
  INTEGER(i4b),                 &
           DIMENSION(maxsgr,*) :: satoff ! satoff(j,i),j=1,..,nsaoff(i),
                                         ! i=1,..,nanoff
                                         ! satellite numbers of each antenna
                                         ! offset group
  INTEGER(i4b), DIMENSION(3)   :: paroff ! antenna offset components to
                                         ! be estimated (x,y,z in satellite
                                         ! reference frame, 1: estimated)
  INTEGER(i4b)                 :: nrqoff ! number of sat. ant. offset requests
  INTEGER(i4b), DIMENSION(*)   :: grpoff ! antenna group for request number i
                                         ! 0: satellite specific
  REAL(r8b),    DIMENSION(3,*) :: sigoff ! a priori sigmas for ant. requests
  REAL(r8b),    DIMENSION(2,*) :: timoff ! time intervals for antenna requests
  INTEGER(i4b), DIMENSION(*)   :: isgoff ! type of sigma
                                         ! 0: absolute sigma
                                         ! 1: sigma relative to the previous
                                         !    parameter of the same group
  INTEGER(i4b)                 :: nhill  ! number of hill parameters
  INTEGER(i4b), DIMENSION(3,*) :: hiltyp ! char. of hill parms
  REAL(r8b),    DIMENSION(*)   :: sighil ! a priori sigmas for hill parms
  INTEGER(i4b)                 :: npot   ! number of parms of earth's pot.
  INTEGER(i4b), DIMENSION(3,*) :: pottyp ! char of pot parms
  REAL(r8b),    DIMENSION(*)   :: sigpot
  INTEGER(i4b)                 :: nalb   ! number of albedo parameters/group
  INTEGER(i4b), DIMENSION(*)   :: albtyp ! parameter type (1, 2, or 3)
  REAL(r8b),    DIMENSION(*)   :: sigalb ! a priori sigmas for alb par types
  INTEGER(i4b)                 :: nalbgr ! number of albedo groups
  INTEGER(i4b), DIMENSION(*)   :: nsaalb ! number of satellites per group
  INTEGER(i4b),                 &
         DIMENSION(maxsat,*)   :: satalb ! satalb(j,i),j=1,..,nsaalb(i),
                                         ! i=1,..,nalbgr
                                         ! satellite numbers of each
                                         ! albedo group
  INTEGER(i4b)                 :: ncenm  ! number of center of mass parameter
  INTEGER(i4b), DIMENSION(*)   :: cenmas ! corresp. coordinate numbers
  REAL(r8b),    DIMENSION(*)   :: sigcen ! corresp. a priori sigmas
  INTEGER(i4b)                 :: nrgb   ! number of SLR range bias parameters
  INTEGER(i4b), DIMENSION(*)   :: stargb ! Station numbers for RGB
  INTEGER(i4b)                 :: satSpec ! Satellite selection for RGB
  REAL(r8b)                    :: sigrgb ! Default a priori sigma for RGB
  INTEGER(i4b), DIMENSION(*)   :: opteli ! option for preelimination of
                                         ! parameter types:
                                         ! 0 : not pre-eliminated
                                         ! 1 : pre-eliminated before inversion
                                         ! 2 : pre-eliminated after  inversion
                                         ! 3 : pre-eliminated epoch-wise
  INTEGER(i4b), DIMENSION(*)   :: optpar ! flag whether parameter type i is
                                         ! estimated (1: estimated)
  INTEGER(i4b), DIMENSION(*)   :: optdip ! options for diff. ion. parameters
                                         ! (1): =0: no diff. ion. parameters
                                         !      =1: one par. per epoch and sat.
                                         !      =2: parameters epoch-wise pre-
                                         !          eliminated
                                         ! (2): elimination of ref. ion. par.
                                         ! (3): elev.-dep. par. constraining
  REAL(r8b),      DIMENSION(2) :: sigdip ! a priori sigmas for diff. ion. par.
                                         ! (1): absolute sigma
                                         ! (2): relative sigma in m/min**1/2
  INTEGER(i4b), DIMENSION(*)   :: opthoi ! options for hoi scaling par.
                                         ! (1-3): 1.- order
                                         !      =0: no
                                         !      =1: one for all
                                         !      =2: station specific
  REAL(r8b),      DIMENSION(*) :: sighoi ! a priori sigmas for hoi scaling par.
                                         ! (1-3): 1.- order
  INTEGER(i4b)                 :: nancal ! number of receiver antenna phase
                                         ! center requests
  CHARACTER(LEN=20),            &
                DIMENSION(2,*) :: antcal ! receiver and antenna name for
                                         ! request i
  INTEGER(i4b), DIMENSION(2,*) :: numcal ! antenna numbers
                                         ! "from - to" for request i
  INTEGER(i4b), DIMENSION(*)   :: prncal ! satellite system (index in g_svnsys)
  INTEGER(i4b), DIMENSION(*)   :: nfrcal ! frequency for receiver antenna
                                         ! phase center request i
  INTEGER(i4b), DIMENSION(2,*) :: nptcal ! number of points to be estimated in
                                         ! elevation (j=1) and azimuth (j=2)
                                         ! (1) > 0 ... linear model
                                         ! (1) < 0 ... spherical harmonics
  REAL(r8b),    DIMENSION(*)   :: sigcal ! a priori sigmas in meters
  INTEGER(i4b), DIMENSION(*)   :: optgim ! options for global ionosphere model
                                         ! (1): maximum degree
                                         ! (2): maximum order
                                         ! (3): flag for reference frame
                                         !      =1: geographical
                                         !      =2: geomagnetic
                                         ! (4): flag for position of the sun
                                         !      =1: mean
                                         !      =2: true
                                         ! (5): estimation of layer height
                                         !      =0: no
                                         !      =1: one parameter in all
                                         !      =2: one parameter per model
                                         ! (6): mode of temporal modeling
                                         !      =1: static model
                                         !      =2: dynamic model
                                         ! (7): total number of models
                                         ! (8): mapping function
                                         !      =0: none
                                         !      =1: 1/cos
                                         ! (9): station-specific models
                                         ! (10): component to be estimated
                                         !       =1: deterministic
                                         !       =2: stochastic
  REAL(r8b),    DIMENSION(3,*) :: polgim ! i=1: height of single layer (m)
                                         ! i=2: lat. of north geomagnetic pole
                                         ! i=3: east longitude
  REAL(r8b),    DIMENSION(*)   :: siggim ! absolute sigma for
                                         ! (1): ion. coefficients (tecu)
                                         ! (2): single-layer height (m)
                                         ! relative sigma for
                                         ! (3): ion. coefficients (tecu)
                                         ! (4): single-layer height (m)
  CHARACTER(LEN=16),            &
                DIMENSION(*)   :: namgim ! model numbers
  REAL(r8b),    DIMENSION(2,*) :: epogim ! periods
                                         ! of validity / ref epochs (mjd)
  INTEGER(i4b)                 :: nanrao ! number of receiver antenna offsets
  CHARACTER(LEN=20),            &
                DIMENSION(2,*) :: antrao ! receiver and antenna
                                         ! name for request i
  INTEGER(i4b), DIMENSION(2,*) :: numrao ! antenna numbers
                                         ! "from - to" for request i
  INTEGER(i4b), DIMENSION(*)   :: prnrao ! satellite system (index in g_svnsys)
  INTEGER(i4b), DIMENSION(*)   :: nfrrao ! frequency for
                                         ! receiver ant. offset request i
  REAL(r8b),    DIMENSION(2,*) :: sigrao ! a priori sigmas in meters
                                         ! j=1: horizontal components
                                         ! j=2: vertical component
  INTEGER(i4b), DIMENSION(3)   :: neurao ! components to be estimated
                                         ! (i=1: north, i=2: east, i=3: up)
                                         ! =1: estimation
                                         ! =0: no estimation
  INTEGER(i4b)                 :: nclkst ! # station epoch wise clocks
  INTEGER(i4b)                 :: nclksa ! # satellite epoch wise clocks
  INTEGER(i4b), DIMENSION(*)   :: clksta ! station numbers for
                                         ! epoch wise clock estimation
  INTEGER(i4b), DIMENSION(*)   :: clksat ! sat. numbers for
                                         ! epoch wise clock estimation
  REAL(r8b)                    :: edtlvl ! o-c edit level for apriori editting
  INTEGER(i4b)                 :: izerod ! For epoch parameters available:
                                         ! =1: code and phase together
                                         ! =2: code or phase files specified
  INTEGER(i4b), DIMENSION(*)   :: nepobs ! Min # of obs. for epoch param.s
                                         ! 1: sta-clk / 2: sat-clk / 3: kin
  INTEGER(i4b)                 :: clksys ! 1: One rec.clk for each sat.sys
  TYPE(t_clkHead)              :: clkhed ! Header information for clock
                                         ! rinex output file
                                         ! clkhed%numRef == 0: fix ref-clock
                                         ! clkhed%numRef == 2: condition of sum
  INTEGER(i4b)                 :: norb2  ! number of orbital parameters to
                                         ! be estimated
  INTEGER(i4b), DIMENSION(*)   :: seqorb2! sequence for orbital elements
  REAL(r8b),    DIMENSION(*)   :: prec2  ! a priori orbital precisions
  INTEGER(i4b)                 :: nstcep2! number of stochastic forces per epoch
  INTEGER(i4b), DIMENSION(*)   :: frctyp2! stoch. force types for
                                         ! max 3 forces per epoch
                                         ! (1) force = r : radial
                                         ! (2) force = s : normal to r
                                         !                 in orb. plane
                                         ! (3) force = w : normal to orb plane
                                         ! (4) force = direction sun -->
                                         !             satellite
                                         ! (5) force = y direction of
                                         !             space craft
                                         ! (6) force = x direction of
                                         !             space craft
  INTEGER(i4b)                 :: nsastc2! number of sats with stoch. forces
  INTEGER(i4b), DIMENSION(*)   :: numstc2! satellite numbers
  INTEGER(i4b), DIMENSION(*)   :: nstday2! number of "stoch epochs" per day
  REAL(r8b),    DIMENSION(3,*) :: sigstc2! a priori sigmas for the 3 force types
  INTEGER(i4b)                 :: nspec2 ! number of special stoch. requests
  INTEGER(i4b), DIMENSION(*)   :: numspc2! satellite numbers
  REAL(r8b),    DIMENSION(*)   :: timspc2! times for these requests
  REAL(r8b),    DIMENSION(3,*) :: sigspc2! corresponsing a priori sigmas
  INTEGER(i4b)                 :: ieppar ! at least one parameter type
                                         ! preeliminated epoch by epoch (1=yes)
  INTEGER(i4b)                 :: norres ! residual computation
                                         ! =1: real residuals saved
                                         ! =2: l1-normalized residuals saved
                                         ! =3: normalized with apriori wgt only
  INTEGER(i4b)                 :: iraux2 ! Epoch result scratch file is
                                         ! 0: available / 1: not avail.
  INTEGER(i4b)                 :: iorest ! orbit estimation for
                                         ! 0: all satellites
                                         ! 1: gps satellites only
                                         ! 2: glonass satellites only
                                         ! 3: LEO
  INTEGER(i4b)                 :: nEstSat! #sat for orbit determination
  INTEGER(i4b), DIMENSION(:)   :: estSat ! PRN for that orbits are requ.
  INTEGER(i4b)                 :: noSol  ! Compute no solution
                                         ! 0: compute
                                         ! 1: do not compute
  INTEGER(i4b)                 :: iqxx   ! provide correct var/cov information
                                         ! 1: NO
                                         ! 2: YES
  INTEGER(i4b)                 :: iphsep ! only phase for resubst of epo-param.
                                         ! 0: NO
                                         ! 1: YES
  INTEGER(i4b)                 :: nanspv ! number of satellite antenna phase
                                         ! center groups to be estimated
  INTEGER(i4b), DIMENSION(*)   :: nsaspv ! nsaspv(i),i=1,..,nanspv
                                         ! number of satellites belonging to
                                         ! antenna phase center group i
  INTEGER(i4b),                 &
         DIMENSION(maxsgr,*)   :: satspv ! satspv(j,i),j=1,..,nsaspv(i)
                                         !             i=1,..,nanspv
                                         ! satellite numbers of each antenna
                                         ! phase center group
  INTEGER(i4b), DIMENSION(*)   :: gnrspv ! gnrspv(i),i=1,..,nanspv
                                         ! user-defined number of satellite
                                         ! antenna phase center group i
  INTEGER(i4b), DIMENSION(2,*) :: nptspv ! nptspv(j,i),j=1,2
                                         !             i=1,..,nanspv
                                         ! number of points to be estimated
                                         ! in elevation (j=1) and azimuth (j=2)
                                         ! direction
  REAL(r8b),    DIMENSION(*)   :: sigspv ! sigspv(i),i=1,..,nanspv
                                         ! a priori sigmas (in meters)
  REAL(r8b)                    :: nadmax ! maximum nadir angle for satellite
                                         ! antenna pattern estimation (in rad)
  CHARACTER(LEN=16)            :: nutnam ! name of the nutation model
  CHARACTER(LEN=16)            :: subnam ! name of the subdaily pole model
  INTEGER(i4b)                 :: isasys ! satellite system to be considered
                                         ! = 0: ALL
                                         ! = 1: GPS
                                         ! = 2: GLONASS
                                         ! = 3: Galileo
                                         ! = 4: GPS+GLONASS
                                         ! = 5: GPS+Galileo
                                         ! = 6: GLONASS+Galileo
  REAL(r8b)                 :: rapzenmax ! maximum zenith angle for receiver
                                         ! antenna pattern estimation (in rad)
  INTEGER(i4b), DIMENSION(*)   :: gnroff ! gnroff(i),i=1,..,nanoff
                                         ! user-defined number of satellite
                                         ! antenna offset group i
  INTEGER(i4b)                 :: ipolar ! Polarization effect
                                         ! = 0: none
                                         ! = 1: only geometrical effect
                                         ! = 2: full effect, incl. satellite attitude
  TYPE(t_optLoad), DIMENSION(3):: opLoad ! Scaling factors for vienna grid files
                                         ! 1: Atmospheric non-tidal loading
                                         ! 2: Ocean non-tidal loading
                                         ! 3: Hydrostatic pressure loading
  TYPE(t_optGsp)               :: optGsp ! GNSS-specific parameters
  INTEGER(i4b)                 :: irel2  ! flag for Periodic Relativistic J2-Correction


! List of Functions
! -----------------

! Local Types
! -----------
  TYPE(t_eccent)         :: eccent
  TYPE(t_stdhead)        :: stdHead, stdLHead   ! Structure of std header info


! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'rdinpt'


! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength),DIMENSION(:),POINTER     :: keyValue
  CHARACTER(LEN=fileNameLength)                          :: eccFil
  CHARACTER(LEN=fileNameLength)                          :: neqFil
  CHARACTER(LEN=fileNameLength)                          :: ionFil
  CHARACTER(LEN=fileNameLength)                          :: filNam
  CHARACTER(LEN=staNameLength), DIMENSION(:),ALLOCATABLE :: eccStaName

  INTEGER(i4b)                                           :: iSta
  INTEGER(i4b)                                           :: iEcc
  INTEGER(i4b),                 DIMENSION(:),ALLOCATABLE :: eccStaNum
  INTEGER(i4b)                                           :: irCode
  INTEGER(i4b)                                           :: ircNeq,ircIon
  INTEGER(i4b)                                           :: irc,ircl
  INTEGER(i4b)                                           :: ii

  LOGICAL                                                :: moreOpt
  LOGICAL                                                :: rcvClk
  LOGICAL                                                :: isSLR  = .false.
  LOGICAL                                                :: isZd   = .false.
  LOGICAL                                                :: isLL   = .false.
  LOGICAL                                                :: isL3   = .false.
  LOGICAL                                                :: isL4   = .false.
  LOGICAL                                                :: isL6   = .false.
  LOGICAL                                                :: isL7   = .false.
  LOGICAL                                                :: isRpr  = .false.
  LOGICAL                                                :: isRpr2 = .false.
  LOGICAL                                                :: isLeo  = .false.
  LOGICAL                                                :: isClk  = .false.

! Init return code counter
! ------------------------
  irCode = 0

  NULLIFY(keyValue)
  CALL init_stdHead(stdHead)
  CALL init_eccent(eccent)

! Get important options relevant to setup of parameters
! -----------------------------------------------------
  CALL readKeys('GEOTEC', keyValue, irc)
  IF (keyValue(1) == "SLR") isSLR = .true.

  CALL readKeys('DIFLVL', keyValue, irc)
  IF (keyValue(1) == "ZERO" .AND. .NOT. isSLR) isZD = .true.

  CALL readKeys('FREQUENCY', keyValue, irc)
  IF (keyValue(1) == "L1&L2")    isLL = .true.
  IF (keyValue(1) == "L3")       isL3 = .true.
  IF (keyValue(1) == "L4")       isL4 = .true.
  IF (keyValue(1) == "MELWUEBB") isL6 = .true.
  IF (keyValue(1) == "DTEC")     isL7 = .true.

  CALL readKeys('RPRCOE', keyValue, irc)
  IF (LEN_TRIM(keyValue(1)) > 0) isRPR  = .true.
  CALL readKeys('LEORPR', keyValue, irc)
  IF (LEN_TRIM(keyValue(1)) > 0) isRPR2 = .true.

  CALL readKeys('LEOPROC', keyValue, irc)
  IF (keyValue(1) == '1') isLeo = .true.

! If LEO should be processed, check STD-file
! ------------------------------------------
  IF (isLeo) THEN
    filNam = ' '
    CALL gtflna(0,'LEOSTD', filNam, ircl)
    IF (ircl == 0) THEN
      CALL init_stdHead(stdLHead)
      CALL rdstdh(filNam,stdLHead,ircl)
    ENDIF
  ENDIF


! Use experimental parameters
! ---------------------------
  CALL ckoptb(1,(/'MOREOPT'/),'rdinpt',      &
       'Use experimental parameters',irCode, &
       resultL=moreOpt)

! Get the list of center station names and numbers
! ------------------------------------------------
  ALLOCATE(eccStaName(nAllSta),stat=irc)
  CALL alcerr(irc,'eccStaName',(/nAllSta/),srName)

  ALLOCATE(eccStaNum(nAllSta),stat=irc)
  CALL alcerr(irc,'eccStaNum',(/nAllSta/),srName)

  eccStaName(1:nAllSta) = allStaName(1:nAllSta)
  eccStaNum(1:nAllSta)  = allStaNum(1:nAllSta)

  CALL gtflna(0,'ECCENT',eccFil,irc)
  IF (LEN_TRIM(eccFil) > 0 .AND. irc == 0) THEN
    CALL readEcc(eccFil,eccent)
    DO iSta = 1,nAllSta
      DO iEcc = 1,eccent%nEcc
        IF (eccStaName(iSta) == eccent%ecc(Iecc)%cenNam) THEN
          eccStaName(iSta) = eccent%ecc(Iecc)%staNam
          eccStaNum(iSta)  = eccent%ecc(Iecc)%staNum
        ENDIF
      ENDDO
    ENDDO
    DEALLOCATE(eccent%ecc,stat=irc)
  ENDIF

! General input options
! ---------------------
  CALL rdigen(nInpFiles, globalWindow,isLeo,stitle, priopt, &
              corstr, dtsim , isyncr, sigapr, icoelv, &
              norres, nsampl, noInClk,secipl, zenmax, &
              zmxLeo, itropo, iextra, itrmap, itrgrd, &
              edtlvl, optdcb(4),ipolar)

! Get model information from standard orbit file header
! -----------------------------------------------------
  filNam = ' '
  CALL rdstdh(filNam,stdHead,irc)

  CALL rdnutsub(nutnam,subnam)

! Ambiguity Handling
! ------------------
  CALL rdiamb(stramb, sigamb, ar2mod, ar2inf)

! Positioning options (fix/sigma) stations
! ----------------------------------------
  CALL rdipos(nAllSta, allStaNum, allStaName, &
              nfix, stfix, nstwgt, istwgt, stwgt, iloc, ihelm, wgtFile)

! Orbit Estimation (GPS/GLONASS)
! ------------------------------
  norb   = 0
  nstcep = 0
  nspec  = 0
  CALL readKeys('ORBADJ', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1' .AND. &
      isRpr .AND. .NOT.(isL4 .OR. isL6 .OR. isL7)) THEN
    CALL rdiorb(1,maxstc, maxsat, globalWindow, nAllSat,allSatNum, &
                norb, seqorb, prec,nstcep, frctyp, nsastc, numstc, &
                nstday, sigstc, nspec, numspc, timspc, sigspc, &
                iorest, nEstSat, estSat)
  ENDIF

! Orbit Estimation (LEO)
! ----------------------
  norb2   = 0
  nstcep2 = 0
  nspec2  = 0
  CALL readKeys('ORBADJ', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1' .AND. &
      isLeo .AND. isRpr2 .AND. .NOT.(isL4 .OR. isL6 .OR. isL7)) THEN
    CALL rdiorb(2,maxstc, maxsat, globalWindow, nAllSat,allSatNum, &
                norb2, seqorb2, prec2, nstcep2, frctyp2, nsastc2,  &
                numstc2, nstday2, sigstc2, nspec2, numspc2, timspc2, &
                sigspc2, ii, nEstSat, estSat)

    ! only LEO orbits
    IF (nOrb2 + nstcep2 + nspec2 /= 0 .AND. &
        nOrb  + nstcep  + nspec  == 0) iorest = 3
  ENDIF

! Station-Specific Troposphere Parameters
! ---------------------------------------
  ntrsta = 0
  CALL readKeys('SITETROP', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1' .AND. &
      .NOT.(isL4 .OR. isL6 .OR. isL7)) THEN
    CALL rditrp(maxsta, maxtrm,                                       &
                nAllSta, eccStaNum, eccStaName, globalWindow, dtSim,  &
                ntrsta, statrp, trplms, sigtrs, isgtrs, itrmap, itrgrd)
  ENDIF

! Local Troposphere Models
! ------------------------
  ntrreq = 0

  CALL readKeys('LOCTROP', keyValue, irc)
  IF (moreOpt.AND.irc == 0 .AND. keyValue(1) == '1') THEN
    CALL rditrm(maxtrp, maxtrm, globalWindow, dtSim,     &
                ntrreq, sigtrp, trplim, npartr)
  ENDIF

! Global Ionosphere Model
! -----------------------
  optgim(1:10) = 0
  CALL readKeys('GLOBION', keyValue, irc)
  IF ( irc == 0 .AND. keyValue(1) == '1' .AND. .NOT.(isL3 .OR. isL6)) THEN
    CALL rdigim(maxgim, globalwindow, dtSim, &
                optgim, polgim, siggim, namgim, epogim)
  ENDIF


! Local Ionosphere Models
! -----------------------
  nioreq = 0
  CALL readKeys('LOCIONO', keyValue, irc)
  IF ( irc == 0 .AND. keyValue(1) == '1' ) THEN
    CALL rdiion(maxfil, nioreq, ionmod, ionreq)
  ENDIF


! Stochastic Differential Ionosphere Parameters
! ---------------------------------------------
  optdip(1:3) = 0
  CALL readKeys('DIFFION', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1' .AND. .NOT.(isL3 .OR. isL6)) THEN
    CALL rdidip(optdip, sigdip)
  ENDIF

! Higher-order ionosphere scaling parameters
! ------------------------------------------
  opthoi(1:3) = 0
  CALL gtflna(0,'IONOS',ionFil,ircIon)
  CALL readKeys('SCALHOI', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1' .AND. ircIon == 0 .AND. &
         (isL3 .OR. isLL)) THEN
    CALL rdihoi(opthoi, sighoi)
  ENDIF

! Earth Orientation Parameters
! ----------------------------
  polmod      = 0
  polpar(1:5) = 0
  CALL readKeys('ERP', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1' .AND. &
      .NOT.(isL4 .OR. isL6 .OR. isL7)) THEN
    CALL rdierp(maxpol, globalWindow, dtSim, &
                polmod, polpar, hpsave, npol, tpol, sigpol, isgpol, isgnut)
  ENDIF

! Center of Mass
! --------------
  ncenm = 0
  CALL readKeys('GEOCENT', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1' .AND. &
      .NOT.(isL4 .OR. isL6 .OR. isL7)) THEN
    CALL rdicom(ncenm, cenmas, sigcen)
  ENDIF

! GNSS-specific parameters
! ------------------------
  optGsp%traSys = 0
  optGsp%traSig = 0.d0
  optGsp%trpSys = 0
  optGsp%trpSig = 0.d0
  CALL readKeys('GNSSPEC',keyValue,irc)
  IF (irc == 0 .AND. keyValue(1) == '1' .AND. &
      .NOT.(isL4 .OR. isL6 .OR. isL7)   .AND. iSaSys /= 1 ) THEN
    CALL rdigsp(iSaSys,optGsp)
  ENDIF

! Satellite Antenna Offsets
! -------------------------
  nanoff = 0
  nrqoff = 0
  nsaoff(1:maxoff) = 0
  GNROFF(1:MAXOFF) = 0
  CALL readKeys('SANTOFF', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1' .AND. &
      .NOT.(isL6 .OR. isL7)) THEN
    CALL rdisao(maxsgr, maxoff, maxofr, globalWindow, dtSim,     &
                nanoff, nsaoff, satoff, paroff, nrqoff, grpoff,  &
                 sigoff, timoff, isgoff, gnroff, nallsat,allsatnum)
!
  ENDIF

! Satellite Antenna Phase Center Pattern (Variations)
! ---------------------------------------------------
  nanspv = 0
  nsaspv(1:maxspv) = 0
  nadmax = 0.5D0*pi
  CALL readKeys('SANTPAT', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1' .AND. &
      .NOT.(isL6 .OR. isL7)) THEN
    CALL rdisap(maxsgr, maxspv, nanspv, nsaspv, satspv, gnrspv, nptspv,&
                sigspv, nadmax, globalWindow, nallsat, allsatnum)
  ENDIF

! Receiver Antenna Phase Center Offsets
! -------------------------------------
  nanrao = 0
  CALL readKeys('RANTOFF', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1' .AND. &
      .NOT.(isL6 .OR. isL7)) THEN
    CALL rdirao(maxcal, isasys, nanrao, antrao, numrao, &
                sigrao, prnrao, nfrrao, neurao)
  ENDIF

! Receiver Antenna Phase Center Pattern (Variations)
! --------------------------------------------------
  nancal = 0
  rapzenmax = 0d0
  CALL readKeys('RANTPAT', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1' .AND. &
      .NOT.(isL6 .OR. isL7)) THEN
    CALL rdirap(maxcal, isasys, nancal, antcal, numcal, &
                prncal, nfrcal, nptcal, sigcal, rapzenmax)
  ENDIF

! SLR range biases
! ----------------
  nrgb = 0
  CALL readKeys('RBIAS', keyValue, irc)
  IF ( irc == 0 .AND. keyValue(1) == '1' ) THEN

    CALL rdibias(maxsta,nAllSta,allStaNum,allStaName,nrgb,stargb, &
                 satSpec,sigrgb)

  ENDIF


! Satellite/Receiver Epoch-Wise Clock Estimations using Zero Differences
! ----------------------------------------------------------------------
  CALL readKeys('CLOCKR', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') isClk = .true.
  CALL readKeys('CLOCKS', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') isClk = .true.

  nclkst = 0
  nclksa = 0
  irel2  = 0
  IF (isClk .AND. isZd .AND. .NOT.(isL4 .OR. isL6 .OR. isL7)) THEN
    CALL rdiclk(nAllSta, allStaNum, allStaName, nAllSat, allSatNum,      &
                isasys, nclkst, nclksa, clksta, clksat, nepobs, irel2,   &
                clksys,clkhed)

! See CHKOPT
!    IF (corstr /= 3) THEN
!      WRITE(lfnerr,'(/,A,1(/,16X,A),/)')                             &
!        ' *** SR RDINPT: CORRECT correlation strategy required for', &
!        'clock estimation'
!      CALL exitrc(2)
!    ENDIF
  ENDIF

! Scaling factors for vienna grid files
! -------------------------------------
  CALL readKeys('GRDLOAD', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1' .AND. &
      .NOT.(isL4 .OR. isL6 .OR. isL7)) THEN
    CALL rdiGrd(nAllSta, allStaNum, allStaName,opLoad)
  ELSE
    DO ii = 1,SIZE(grdNeq)-1
      opLoad(ii)%nSta = 0
      opLoad(ii)%keyw = grdNeq(ii)
    ENDDO
  ENDIF

! GLONASS biases/station-specific receiver clock offsets
! ------------------------------------------------------
  nclreq = 0
  nRec   = 0

  CALL ckoptb(1,(/'RCVCLOCK'/),'rdinpt','Receiver clock offsets', &
              irCode,resultL=rcvClk)
  rcvClk = (rcvClk .AND. moreOpt)

  IF (rcvClk .OR. &
      (isClk .AND. isZd .AND. .NOT.(isL4 .OR. isL6 .OR. isL7))) THEN
    CALL rdirco(nAllSta, eccStaNum, eccStaName, globalWindow,        &
                clksys,nclreq, istclk, isaclk, nclk, ibias, clkwgt,  &
                clfrto, nRec, isbTim)
  ENDIF

! Differential Code Biases
! ------------------------
  optdcb(1:3) = 0
  CALL readKeys('DCBEST', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1' .AND. .NOT.isL7) THEN
    CALL rdidcb(nAllSat, allSatNum, optdcb, sigdcb)
  ENDIF

! Kinematic Coordinates
! ---------------------
  nKin = 0
  CALL readKeys('KINEST',keyValue,irc)
  IF (irc == 0 .AND. keyValue(1) == '1' .AND. &
      .NOT.(isL4 .OR. isL6 .OR. isL7)) THEN
    CALL rdikin(nAllSta, allStaNum, allStaName,eccStaNum,eccStaName,   &
                globalWindow,nKin,stKin,nEpObs,nStwgt,iStwgt,stWgt)
  ENDIF

! Correct Var/Cov computation for epoch parameters
! ------------------------------------------------
  iqxx=0
  CALL readKeys('QXXEPO', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == 'CORRECT') iqxx = 1

! Compute no solution (only NEQ saving)
! -------------------------------------
  noSol=0

  CALL gtflna(0,'NEQUARS',neqFil,ircNeq)

  IF (ircNeq == 0.AND. &
      (stramb(1) .LE. 0 .OR. nInpFiles(1)+nInpFiles(2) > 0)) THEN
    CALL readKeys('OBS2NEQ', keyValue, irc)
    IF (irc == 0 .AND. keyValue(1) == '1') nosol = 1
  ENDIF

! Satellite-specific Weighting
! ----------------------------
  nwgt = 0
  CALL readKeys('SVNAPR', keyValue, irc)
  IF (moreOpt .AND. irc == 0 .AND. keyValue(1) == '1') THEN
    CALL rdiswg(maxwgt, sigapr, globalWindow, nwgt, satwgt, wgtwgt, timwgt)
  ENDIF

! Hill resonance terms
! --------------------
  nhill         = 0
  CALL readKeys('HILL',keyValue,irc)
  IF (moreOpt .AND. irc == 0 .AND. keyValue(1) == '1') THEN
    CALL rdihil(maxhil, nAllSat, allSatNum, nhill, hilTyp, sigHil)
  ENDIF

! Earth Potential (Gravity Field)
! -------------------------------
  npot = 0
  CALL readKeys('GRAVITY',keyValue,irc)
  IF (moreOpt .AND. irc == 0 .AND. keyValue(1) == '1') THEN
   CALL rdipot(maxpot,npot,pottyp,sigpot)
  ENDIF


! Earth Albedo Radiation
! ----------------------
  nalb            = 0
  nalbgr          = 0
  CALL readKeys('ALBEDO',keyValue,irc)
  IF (moreOpt .AND. irc == 0 .AND. keyValue(1) == '1') THEN
    CALL rdialb(maxalb, maxsat, nAllSat, allSatNum, &
                nalb, albtyp, sigalb, nalbgr, nsaalb, satalb)
  ENDIF

! Parameter Pre-Elimination
! -------------------------
  CALL rdielm(maxtyp,opteli)

! An error in one of the inputs was found
! ---------------------------------------
  IF (irCode /= 0) CALL exitrc(2)

! Deallocate center name/number
! -----------------------------
  DEALLOCATE(eccStaName,stat=irc)
  DEALLOCATE(eccStaNum, stat=irc)

  DEALLOCATE(keyValue,stat=irc)

! Flags whether Parameter Types Are Estimated or Not
! --------------------------------------------------
  optpar(1:maxTyp) = 0
  optpar( 1) = 1
  optpar( 2) = nclreq
  optpar( 3) = norb
  optpar( 4) = 1
  optpar( 5) = nanrao
  optpar( 6) = ntrsta
  optpar( 7) = nioreq
  optpar( 8) = optdcb(1)+ABS(optdcb(2))
  optpar( 9) = ntrreq
  optpar(10) = polmod
  optpar(11) = nstcep
  optpar(12) = nrqoff
  optpar(13) = npot
  optpar(14) = nhill
  optpar(15) = nalb
  optpar(16) = ncenm
  optpar(17) = optdip(1)
  optpar(18) = nancal
  optpar(19) = optgim(7)
  optpar(20) = 0
  optpar(21) = nkin
  optpar(22) = 0
  DO ii = 1,SIZE(grdNeq)-1
    optpar(22) = optpar(22) + opLoad(ii)%nSta
  ENDDO
  optpar(23) = nclkst
  optpar(24) = nclksa
  optpar(25) = nanspv
  optpar(26) = nRgb
  optpar(27) = SUM(optHoi(1:3))
  optpar(30) = optGsp%traSys+optGsp%trpSys

  IF (optdip(1) == 1 .AND. opteli(17) == 3) optdip(1) = 2
  IF (optdip(1) == 2) opteli(17) = 3

! Check whether Parameters are Pre-Eliminated Epoch by Epoch
! ----------------------------------------------------------
  ieppar = 0
  DO ii = 1, maxtyp
    IF (optpar(ii) > 0) THEN
      IF (opteli(ii) == 3) ieppar = 1
    ELSE
      opteli(ii) = 0
    ENDIF
  ENDDO

! Resolve izerod
! --------------
  izerod = 0
! Parameters for RESEPO ?
  IF ((opteli(21) == 3 .AND. nKin   > 0) .OR. &
      (opteli(23) == 3 .AND. nClkSt > 0) .OR. &
      (opteli(24) == 3 .AND. nClkSa > 0)) THEN
    IF      (nInpFiles(1) /= 0 .AND. nInpFiles(2) /= 0) THEN
      izerod = 1
    ELSE IF (nInpFiles(1) /= 0 .OR.  nInpFiles(2) /= 0) THEN
      izerod = 2
    ENDIF

! Allow kinematic positining from DD-approach too (but not code+phase)
    IF      (nInpFiles(3) /= 0 .AND. nInpFiles(4) /= 0) THEN
      WRITE(lfnerr,'(/,A,2(/,16X,A),/)')                                     &
        ' ### SR RDINPT: The resubstitution of epoch-wise pre-eliminated ',  &
                        'parameters is not available if code and phase ',    &
                        'observation baseline files are unsed together.'
    ELSE IF (nInpFiles(3) /= 0 .OR.  nInpFiles(4) /= 0) THEN
      izerod = 2
    ENDIF

! Use only phase observations for epoch parameter computation
! -----------------------------------------------------------
    iphsep=0
    CALL readKeys('PHASEPO', keyValue, irc)
    IF (irc == 0 .AND. keyValue(1) == '1') iphsep = 1

  ELSE

    iqxx=0
    iphsep=0

  ENDIF

! Init epoch result scratch file
! ------------------------------
  iraux2 = 1

END SUBROUTINE rdinpt



END MODULE



