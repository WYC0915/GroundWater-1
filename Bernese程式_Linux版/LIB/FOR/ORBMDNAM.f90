MODULE s_ORBMDNAM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE orbmdnam(orbmod,dtpi,qdeg,dtpiv,qvar,nutnam,subnam, &
                      potnam,npotmx,tponam,otidnm,otdmin,otddeg, &
                      cmcyn,indtim,antthr,erpmod)

! -------------------------------------------------------------------------
! Purpose:    Set orbit model description.
!
! Author:     U. Hugentobler
!
! Created:    13-Jan-2005
! Last mod.:  06-May-2011
!
! Changes:    03-Feb-2005 HU: Default RAY instead of RAY_96
!             21-Feb-2005 AJ: (LEO defaults)
!             18-May-2005 HU: GPS and LEO defaults
!             17-Nov-2005 HU: Correct sign in GENREL, 'IERS1996 P'
!                             3*USAT-U0 replaced by 3*(USAT-U0) in ROCKMD
!             18-Jul-2006 AG: CMC implemented
!             21-Sep-2006 HU: Mean pole handling added
!             08-Feb-2007 HB: Add tponam to parameter list and write
!                             into model description
!             28-Mar-2007 HB/UH: Add 'K20=0.30' in TIDPOT line and
!                             'BIAS' in NUTSUB line to characterize
!                             IERS2003 standards
!             21-Jul-2008 DT: Add "DRSW" model for dynamical parameters
!             05-Aug-2008 DT: Set orbdsc%otdmin; use orbdsc from P_ORBGEN
!                             and remove from parameter list
!             02-Sep-2008 DT: Store interval lengths in [s]; INTEGR->INTEG2
!             14-Nov-2008 DT: Add TIMSYS to orbit description; indtim as input
!             13-Aug-2010 CR: Earth radiation pressure added
!             03-Dec-2010 RD: CMC for ATL added
!             03-Dec-2010 KS: Add otddeg (max degree of ocean tides model)
!             09-Feb-2011 DT: otdmin [cm]->[m]
!             06-May-2011 HB: Set model keys through d_model
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_const,  ONLY: filtitle
  USE d_comjpl, ONLY: jplnam
  USE d_model,  ONLY: setModKey, chrValLength, &
                      mod_orb_eqmpol, mod_orb_eqmInt, mod_orb_veqPol, &
                      mod_orb_veqInt, mod_orb_prcMod, mod_orb_jplEph, &
                      mod_orb_3bodAt
  USE p_orbgen, ONLY: maxomd, orbmdft, orbdsc
  USE s_cmc,    ONLY: getcmc
  USE s_dimtst
  USE s_rdanlt
  USE s_gtflna
  IMPLICIT NONE

! List of parameters
! ------------------
! IN:
  INTEGER(i4b),DIMENSION(*)           :: orbmod      ! Orbit model flags
  REAL(r8b)                           :: dtpi, dtpiv ! Integration
  INTEGER(i4b)                        :: qdeg, qvar  ! Integration
  CHARACTER(LEN=16)                   :: nutnam      ! Nutation model
  CHARACTER(LEN=16)                   :: subnam      ! Subdaily ERP model
  CHARACTER(LEN=16)                   :: potnam      ! Geopotential name
  INTEGER(i4b)                        :: npotmx      ! Max pot degree
  CHARACTER(LEN=16)                   :: tponam      ! Solid Earth tide model name
  CHARACTER(LEN=16)                   :: otidnm      ! Ocean tide model name
  REAL(r8b)                           :: otdmin      ! Min magn otides
  LOGICAL, DIMENSION(2)               :: cmcyn       ! CMC applied?
  INTEGER(i4b)                        :: antthr      ! Antenna thrust
  INTEGER(i4b)                        :: erpmod      ! Earth radiation pressure
  INTEGER(i4b)                        :: indtim      ! Time system

! OUT:
!!!  TYPE(t_orbmodel)                    :: orbdsc      ! Description lines

! Local Variables
! ---------------
  INTEGER(i4b)                        :: irc
  INTEGER(i4b)                        :: nlin
  INTEGER(i4b)                        :: isleo
  INTEGER(i4b),DIMENSION(2)           :: help
  INTEGER(i4b)                        :: irCode
  INTEGER(i4b)                        :: otddeg
  REAL(r8b)                           :: xquer,yquer
  REAL(r8b)                           :: love
  CHARACTER(LEN=8)                    :: anltyp
  CHARACTER(LEN=16),DIMENSION(2)      :: cmcmod      ! CMC model name
  CHARACTER(LEN=20)                   :: harstr      ! harload indentifier
  CHARACTER(LEN=fileNameLength)       :: filnam
  CHARACTER(LEN=chrValLength)         :: prcnam
  CHARACTER(LEN=chrValLength)         :: setChr = ' '

  CHARACTER(LEN=8),PARAMETER          :: srName='orbMdNam'

  DATA xquer/0.033D0/,yquer/0.331D0/

! Initialize
! ----------
  IF (orbmod(1) > 3) THEN
    isleo=1
  ELSE
    isleo=0
  ENDIF

! Defaults
! --------
  orbmdft%nlin=12
  IF (isleo==0) THEN                                 ! GPS
    orbmdft%orbmod( 1) = 'TITLE :'
    orbmdft%orbmod( 2) = 'CREATE:'
!!    orbmdft%orbmod( 3) = 'INTEGR:   1.00000   10  6.00000   12'
    orbmdft%orbmod( 3) = 'INTEG2:      3600   10    21600   12'
    orbmdft%orbmod( 4) = 'NUTSUB: IAU80            RAY'
    orbmdft%orbmod( 5) = 'GRAVIT: JGM-3              12'
    orbmdft%orbmod( 6) = 'TIDPOT: IERS1996    ELAS STEP_1+2      POLTID   0.03300   0.33100'
    orbmdft%orbmod( 7) = 'OTIDES: OT_CSRC          IERS1996      XMIN     0.05000   DEG   8'
    orbmdft%orbmod( 8) = 'JPLEPH: DE200'
    orbmdft%orbmod( 9) = 'PLANET: JUPITER VENUS MARS'
    orbmdft%orbmod(10) = 'RELATV: PPN IERS1996'
!!!    orbmdft%orbmod(11) = 'EMPIRI: DYX ONCE-PER-REV T980301'
    orbmdft%orbmod(12) = 'SHADOW: STEP SPHERE MOON'
    orbmdft%orbmod(13) = 'OTLOAD: FES2004          CMC: Y'
    orbmdft%orbmod(14) = 'TIMSYS: GPS'
  ELSE                                               ! LEO
    orbmdft%orbmod( 1) = 'TITLE :'
    orbmdft%orbmod( 2) = 'CREATE:'
!!    orbmdft%orbmod( 3) = 'INTEGR:   0.05000   10  0.20000   10'
    orbmdft%orbmod( 3) = 'INTEG2:       180   10      720   10'
    orbmdft%orbmod( 4) = 'NUTSUB: IAU80            RAY'
    orbmdft%orbmod( 5) = 'GRAVIT: EIGEN-2           120'
    orbmdft%orbmod( 6) = 'TIDPOT: IERS2000    ELAS STEP_1+2      POLTID   0.03300   0.33100'
    orbmdft%orbmod( 7) = 'OTIDES: OT_CSRC          IERS1996      XMIN     0.05000   DEG 120'
    orbmdft%orbmod( 8) = 'JPLEPH: DE200'
    orbmdft%orbmod( 9) = 'PLANET: JUPITER VENUS MARS'
    orbmdft%orbmod(10) = 'RELATV: PPN IERS1996'
!!!    orbmdft%orbmod(11) = 'EMPIRI: RSW ONCE-PER-REV'
    orbmdft%orbmod(12) = 'SHADOW: STEP SPHERE MOON'
  ENDIF

! System for dynamical orbit parameters
! -------------------------------------
  IF ( orbmod(6) == 0 ) THEN
    orbmdft%orbmod(11) = 'EMPIRI: DYX ONCE-PER-REV T980301'

  ELSE IF ( orbmod(6) == 1 ) THEN
    orbmdft%orbmod(11) = 'EMPIRI: RSW ONCE-PER-REV'

  ELSE IF ( orbmod(6) == 2 ) THEN
    orbmdft%orbmod(11) = 'EMPIRI: DRSW ONCE-PER-REV'

  END IF


  nlin=0

! Title
! -----
  nlin=nlin+1
  CALL dimtst(1,2,2,srName,'MAXOMD','ORBIT MODEL LINES',' ', &
              nlin,maxomd,irc)
  WRITE(orbdsc%orbmod(nlin),"('TITLE : ',A)") filtitle(1:64)

! Creation date
! -------------
  nlin=nlin+1
  CALL dimtst(1,2,2,srName,'MAXOMD','ORBIT MODEL LINES',' ', &
              nlin,maxomd,irc)
  WRITE(orbdsc%orbmod(nlin),"('CREATE: ',A)") filtitle(66:80)

! Integration
! -----------
  nlin=nlin+1
  CALL dimtst(1,2,2,srName,'MAXOMD','ORBIT MODEL LINES',' ', &
              nlin,maxomd,irc)
!!  WRITE(orbdsc%orbmod(nlin),"('INTEGR: ',2(F9.5,I5))") dtpi,qdeg,dtpiv,qvar
!! New format: intervals in seconds
  help(1) = DNINT(dtpi * 3600)
  help(2) = DNINT(dtpiv * 3600)
  WRITE(orbdsc%orbmod(nlin),"('INTEG2: ',2(I9,I5))") help(1),qdeg,help(2),qvar

  setChr = ' '
  CALL setModKey(mod_orb_eqmPol,setChr,srName,10*1.D0)
  CALL setModKey(mod_orb_eqmInt,setChr,srName,3600*1.D0)
  CALL setModKey(mod_orb_veqPol,setChr,srName,12*1.D0)
  CALL setModKey(mod_orb_veqInt,setChr,srName,21600*1.D0)

! Nutation, subdaily ERP
! ----------------------
  nlin=nlin+1
  CALL dimtst(1,2,2,srName,'MAXOMD','ORBIT MODEL LINES',' ', &
              nlin,maxomd,irc)
  WRITE(orbdsc%orbmod(nlin),"('NUTSUB: ',A16,1X,A16,1X,A)") &
       nutnam,subnam,'BIAS'
  prcNam = ' '
  prcNam(1:4) = 'BIAS'
  CALL setModKey(mod_orb_prcMod,prcNam,srName,0.D0)

! Geopotential model
! ------------------
  nlin=nlin+1
  CALL dimtst(1,2,2,srName,'MAXOMD','ORBIT MODEL LINES',' ', &
              nlin,maxomd,irc)
  IF (orbmod(4) == 4) THEN
    WRITE(orbdsc%orbmod(nlin),"('GRAVIT: ',A16,I5,10X,'MEANPOLE ',A)") &
              potnam,npotmx,'IERS2010'
  ELSEIF (orbmod(4) == 3) THEN
    WRITE(orbdsc%orbmod(nlin),"('GRAVIT: ',A16,I5,10X,'MEANPOLE ',A)") &
              potnam,npotmx,'IERS2003'
  ELSEIF (orbmod(4) == 2) THEN
    WRITE(orbdsc%orbmod(nlin),"('GRAVIT: ',A16,I5)")     potnam,npotmx
  ENDIF

! Tidal potential
! ---------------
  nlin=nlin+1
  CALL dimtst(1,2,2,srName,'MAXOMD','ORBIT MODEL LINES',' ', &
              nlin,maxomd,irc)
  IF (orbmod(3) == 0) THEN
    love=.285D0
  ELSEIF (orbmod(3) == 1) THEN
    love=.300D0
  ENDIF
  IF (orbmod(4) == 0) THEN
    WRITE(orbdsc%orbmod(nlin),"('TIDPOT: ',A11,6X,A11,'   LOVENR',F10.5)") &
                         'FIRST ORDER','           ',love
  ELSEIF (orbmod(4) == 1) THEN
    WRITE(orbdsc%orbmod(nlin),"('TIDPOT: ',A16,1X,A11,'   LOVENR',F10.5)") &
                         tponam,'STEP_1     ',love
  ELSEIF (orbmod(4) == 2) THEN
    WRITE(orbdsc%orbmod(nlin),"('TIDPOT: ',A16,1X,A11,'   POLTID',2F10.5)") &
                         tponam,'STEP_1+2   ',xquer,yquer
  ELSEIF (orbmod(4) == 3) THEN
    WRITE(orbdsc%orbmod(nlin),"('TIDPOT: ',A16,1X,A11,'   POLTID   ',A,1X,A)") &
                         tponam,'STEP_1+2   ','IERS2003','K20=0.30'
  ELSEIF (orbmod(4) == 4) THEN
    WRITE(orbdsc%orbmod(nlin),"('TIDPOT: ',A16,1X,A11,'   POLTID   ',A,1X,A)") &
                         tponam,'STEP_1+2   ','IERS2010','K20=0.30'

  ENDIF

! Ocean tides
! -----------
  nlin=nlin+1
  CALL dimtst(1,2,2,srName,'MAXOMD','ORBIT MODEL LINES',' ', &
              nlin,maxomd,irc)
  WRITE(orbdsc%orbmod(nlin),"(A,A16,1X,A11,A,F10.5,A,I3)") &
               'OTIDES: ',  otidnm(1:16),'IERS2003   ','   XMIN  ',otdmin, &
               ' DEG ', otddeg


! Max. degree of ocean tides
! --------------------------
  orbdsc%otddeg = otddeg

! Min. size of ocean tides
! ------------------------
  orbdsc%otdmin = otdmin * 1.d-2  ! [m]

! JPL ephemerides
! ---------------
  nlin=nlin+1
  CALL dimtst(1,2,2,srName,'MAXOMD','ORBIT MODEL LINES',' ', &
              nlin,maxomd,irc)
  CALL gtflna(0,'JPLEPH ',filnam,irc)
  IF (IRC == 0) THEN
    WRITE(orbdsc%orbmod(nlin),"('JPLEPH: ',A)") jplnam
    setChr = ' '
    setChr(1:5) = jplNam
    CALL setModKey(mod_orb_jplEph,setChr,srName,0.D0)
  ELSE
    WRITE(orbdsc%orbmod(nlin),"('JPLEPH: ',A)") 'NO'
    setChr = ' '
    setChr(1:2)='NO'
    CALL setModKey(mod_orb_jplEph,setChr,srName,0.D0)
  ENDIF


! Planetary perturbations
! -----------------------
  nlin=nlin+1
  CALL dimtst(1,2,2,srName,'MAXOMD','ORBIT MODEL LINES',' ', &
              nlin,maxomd,irc)
  IF(orbmod(7) == 1)THEN
    WRITE(orbdsc%orbmod(nlin),"('PLANET: ',A)") 'JUPITER VENUS MARS'
    setChr = ' '
    setChr(1:18) = 'JUPITER VENUS MARS'
    CALL setModKey(mod_orb_3bodAt,setChr,srName,0.D0)
  ELSE
    WRITE(orbdsc%orbmod(nlin),"('PLANET: ',A)") 'NO'
  ENDIF

! General relativity
! ------------------
  nlin=nlin+1
  CALL dimtst(1,2,2,srName,'MAXOMD','ORBIT MODEL LINES',' ', &
              nlin,maxomd,irc)
  IF(orbmod(5) == 1)THEN
    WRITE(orbdsc%orbmod(nlin),"('RELATV: ',A)") 'PPN IERS1996 P'
  ELSE
    WRITE(orbdsc%orbmod(nlin),"('RELATV: ',A)") 'NO'
  ENDIF

! Empirical parameters
! --------------------
  nlin=nlin+1
  CALL dimtst(1,2,2,srName,'MAXOMD','ORBIT MODEL LINES',' ', &
              nlin,maxomd,irc)
  IF(orbmod(6) == 0)THEN
    CALL rdanlt(anltyp)
    IF (ANLTYP(1:1).EQ.'C') THEN
      WRITE(orbdsc%orbmod(nlin),"('EMPIRI: DYX ONCE-PER-REV',1X,A,' 3')") anltyp
    ELSE
      WRITE(orbdsc%orbmod(nlin),"('EMPIRI: DYX ONCE-PER-REV',1X,A)") anltyp
    ENDIF
  ELSE IF ( orbmod(6) == 1 )THEN
    WRITE(orbdsc%orbmod(nlin),"('EMPIRI: RSW ONCE-PER-REV')")
  ELSE IF ( orbmod(6) == 2 )THEN
    WRITE(orbdsc%orbmod(nlin),"('EMPIRI: DRSW ONCE-PER-REV')")
  ENDIF

! Shadow
! ------
  nlin=nlin+1
  CALL dimtst(1,2,2,srName,'MAXOMD','ORBIT MODEL LINES',' ', &
              nlin,maxomd,irc)
  WRITE(orbdsc%orbmod(nlin),"('SHADOW: STEP SPHERE MOON')")

! CMC: OTL
! --------
  CALL getcmc(cmcyn,cmcmod=cmcmod,harstr=harstr)
  IF (cmcyn(1)) THEN
    nlin=nlin+1
    CALL dimtst(1,2,2,srName,'MAXOMD','ORBIT MODEL LINES',' ', &
                nlin,maxomd,irc)
    WRITE(orbdsc%orbmod(nlin),"('OTLOAD: ',A16,' CMC: Y',5X,A20)") &
    cmcmod(1),harstr
  ENDIF

! CMC: ATL
! --------
  IF (cmcyn(2)) THEN
    nlin=nlin+1
    CALL dimtst(1,2,2,srName,'MAXOMD','ORBIT MODEL LINES',' ', &
                nlin,maxomd,irc)
    WRITE(orbdsc%orbmod(nlin),"('ATLOAD: ',A16,' CMC: Y')")cmcmod(2)
  ENDIF

! Earth Radiation Pressure
! ------------------------
  nlin=nlin+1
  CALL dimtst(1,2,2,srName,'MAXOMD','ORBIT MODEL LINES',' ', &
              nlin,maxomd,irc)
  IF(erpmod.EQ.0)THEN
    WRITE(orbdsc%orbmod(nlin),"('ERPMOD: NONE')")
  ELSEIF(erpmod.EQ.1)THEN
    WRITE(orbdsc%orbmod(nlin),"('ERPMOD: ANALYTICAL')")
  ELSEIF(erpmod.EQ.2)THEN
    WRITE(orbdsc%orbmod(nlin),"('ERPMOD: NUMERICAL')")
  ENDIF

! Antenna thrust
! --------------
  nlin=nlin+1
  CALL dimtst(1,2,2,srName,'MAXOMD','ORBIT MODEL LINES',' ', &
              nlin,maxomd,irc)
  IF(antthr.EQ.0)THEN
    WRITE(orbdsc%orbmod(nlin),"('ANTTHR: NO')")
  ELSEIF(antthr.EQ.1)THEN
    WRITE(orbdsc%orbmod(nlin),"('ANTTHR: YES')")
  ENDIF

! Time system
! -----------
  nlin = nlin+1
  CALL dimtst(1,2,2,srName,'MAXOMD','ORBIT MODEL LINES',' ', &
              nlin,maxomd,irc)
  IF ( indtim == 1 ) THEN
    WRITE(orbdsc%orbmod(nlin),"('TIMSYS: GPS')")
  ELSEIF ( indtim == 2 ) THEN
    WRITE(orbdsc%orbmod(nlin),"('TIMSYS: UTC')")
  ENDIF

! Number of lines
! ---------------
  orbdsc%nlin=nlin

  RETURN

END SUBROUTINE orbmdnam

END MODULE
