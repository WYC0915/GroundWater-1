MODULE s_SAVNEQ
CONTAINS

! -------------------------------------------------------------------------
! Bernese Software Version 5.2
! -------------------------------------------------------------------------

SUBROUTINE savneq(filneq ,titles ,npar   ,nstat  ,lTPl   ,locq   ,aNor   ,  &
                  bNor   ,partyp ,xstat  ,stname ,xstell ,rectyp ,anttyp ,  &
                  ianten ,csess  ,nfreq  ,stfil  ,posecc ,trplms ,itropo ,  &
                  iextra ,xxx    ,ncamp  ,taecmp ,nstcep ,nstcep2,nstcef ,  &
                  nstcef2,nsastc1,nsastc2,timstc ,timstc2,numstc1,numstc2,  &
                  scastc ,tbound ,tpol   ,scacen ,nobs   ,nparms ,nftot  ,  &
                  nsmpnq ,ielvnq ,nanoff ,nsaoff ,satoff ,itrmap ,itrgrd ,  &
                  namgim ,epogim ,scagim ,nutnam ,subnam ,datum  ,nadmax ,  &
                  ifreq  ,nanspv ,nsaspv ,satspv ,meatyp ,rnxclk ,secipl ,  &
                  clkhed ,clkrec ,nepSam ,antcal ,numcal ,prncal ,antrao ,  &
                  numrao ,prnrao ,iorest ,nobspa ,opLoad ,timisb ,timIntF,  &
                  USEGEOS,GOBSDEF)

! -------------------------------------------------------------------------
! Purpose:    Prepare the writing of the NEQ files using the SR NEQWRITE
!             in program GPSEST
!
! Author:     R.Dach
!
! Created:    07-Jun-2006
!
! Changes:    16-Jan-2008 HB: Add iorest to distinguish between GNSS and
!                             LEO orbits
!             23-Jan-2008 RD: RAO+RAP added to NEQ
!             17-Jun-2008 RD: Counter for observ. per parameter added
!             04-May-2009 RD: Scaling of loading models added
!             29-May-2009 RD: Add epoch satellite/receiver clocks
!             22-Jul-2009 DT: timIntF added to parameter list and added to
!                             Call of NEQPRE
!             16-Jan-2011 RD: STANUM removed
!             13-Jul-2011 LP: Sat.-specific obstypes; neqver removed
!             25-Jul-2011 HB: Re-activate iorest
!             13-Dec-2011 SL: use m_bern with ONLY, unused types removed
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, fileNameLength
!  USE m_time,   ONLY: t_timint
  USE d_neq,    ONLY: t_sinex
  USE d_clkrnx, ONLY: t_clkhead, t_clkrec
  USE p_gpsest, ONLY: maxfrq,maxstc,maxstp,maxlcq,maxspv,t_optLoad,t_partyp
  USE d_rinex3, ONLY: t_gobsdef
  USE s_gtflna
  USE s_neqpre
  USE s_neqwrite
  USE s_stotra
  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=fileNameLength)                 :: filneq
  CHARACTER(LEN=132),DIMENSION(2)               :: titles
  INTEGER(i4b)                                  :: npar
  INTEGER(i4b)                                  :: nstat
  REAL(r8b)                                     :: lTPl
  INTEGER(i4b),      DIMENSION(maxlcq,npar)     :: locq
  REAL(r8b),         DIMENSION(npar*(npar+1)/2) :: aNor
  REAL(r8b),         DIMENSION(npar)            :: bNor
  TYPE(t_parTyp),    DIMENSION(npar)            :: partyp

  REAL(r8b),         DIMENSION(3,nstat)         :: xstat
  CHARACTER(LEN=16), DIMENSION(nstat)           :: stname
  REAL(r8b),         DIMENSION(3,nstat)         :: xstell

  CHARACTER(LEN= 20),DIMENSION(:,:)             :: rectyp
  CHARACTER(LEN= 20),DIMENSION(:,:)             :: anttyp
  INTEGER(i4b),      DIMENSION(:,:)             :: ianten
  CHARACTER(LEN=  4),DIMENSION(:,:)             :: csess
  INTEGER(i4b),      DIMENSION(:)               :: nfreq
  INTEGER(i4b),      DIMENSION(:,:)             :: stfil
  REAL(r8b),         DIMENSION(:,:,:)           :: posecc

  REAL(r8b),         DIMENSION(2,*)             :: trplms
  INTEGER(i4b)                                  :: itropo
  INTEGER(i4b)                                  :: iextra
  REAL(r8b),         DIMENSION(npar)            :: xxx
  INTEGER(i4b)                                  :: ncamp
  REAL(r8b),         DIMENSION(2,*)             :: taecmp

  INTEGER(i4b)                                  :: nstcep
  INTEGER(i4b)                                  :: nstcep2
  INTEGER(i4b),      DIMENSION(:,:)             :: nstcef
  INTEGER(i4b),      DIMENSION(:,:)             :: nstcef2
  INTEGER(i4b)                                  :: nsastc1
  INTEGER(i4b)                                  :: nsastc2
  REAL(r8b),         DIMENSION(:,:,:,:)         :: timstc
  REAL(r8b),         DIMENSION(:,:,:,:)         :: timstc2
  INTEGER(i4b),      DIMENSION(:)               :: numstc1
  INTEGER(i4b),      DIMENSION(:)               :: numstc2
  REAL(r8b),         DIMENSION(*)               :: scastc
  REAL(r8b),         DIMENSION(2,*)             :: tbound
  REAL(r8b),         DIMENSION(2,*)             :: tpol
  REAL(r8b)                                     :: scacen
  INTEGER(i4b)                                  :: nobs
  INTEGER(i4b)                                  :: nparms
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
  REAL(r8b)                                     :: nadmax
  INTEGER(i4b)                                  :: ifreq
  INTEGER(i4b)                                  :: nanspv
  INTEGER(i4b),      DIMENSION(maxspv)          :: nsaspv
  INTEGER(i4b),      DIMENSION(:,:)             :: satspv
  INTEGER(i4b)                                  :: meatyp
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
  CHARACTER(LEN=*),  DIMENSION(2,*)             :: antcal
  INTEGER(i4b),      DIMENSION(2,*)             :: numcal
  INTEGER(i4b),      DIMENSION(:)               :: prncal
  CHARACTER(LEN=*),  DIMENSION(2,*)             :: antrao
  INTEGER(i4b),      DIMENSION(2,*)             :: numrao
  INTEGER(i4b),      DIMENSION(:)               :: prnrao
  INTEGER(i4b)                                  :: iorest
  INTEGER(i4b),      DIMENSION(:,:)             :: nobspa
  TYPE(t_optLoad),   DIMENSION(:)               :: opLoad
  REAL(r8b),         DIMENSION(3,*)             :: timisb
  REAL(r8b),         DIMENSION(2,*)             :: timIntF
  TYPE(t_gobsdef) :: GOBSDEF ! Giove External Obs. Selection info
  INTEGER(i4b)    :: USEGEOS ! =1: GOBSDEF data set available; =0: not

! Local Parameters
! ----------------

! Local Variables
! ---------------
  TYPE(t_sinex), DIMENSION(nStat)               :: antsnx

  CHARACTER(LEN=fileNameLength)                 :: filstd
  CHARACTER(LEN=fileNameLength)                 :: filrpr

  INTEGER(i4b),      DIMENSION(maxstc*maxstp)   :: stotyp
  INTEGER(i4b),      DIMENSION(maxstc*maxstp)   :: svnsto
  INTEGER(i4b),      DIMENSION(maxstc*maxstp)   :: stobnd
  INTEGER(i4b)                                  :: iOrbit
  INTEGER(i4b)                                  :: iPar
  INTEGER(i4b)                                  :: ircStd
  INTEGER(i4b)                                  :: ircRpr
  INTEGER(i4b)                                  :: iDir
  INTEGER(i4b)                                  :: iFil
  INTEGER(i4b)                                  :: nStoch

  REAL(r8b),         DIMENSION(maxstc*maxstp)   :: tstoch


! Prepare the SINEX record
! ------------------------
  CALL neqpre(rectyp,anttyp,ianten,csess ,nfreq ,stfil ,stname, &
              nftot ,nstat ,posecc,ncamp,taecmp,timIntF,antsnx)

! Init some more variables
! ------------------------
  filstd=' '
  filrpr=' '

! Orbit parameters available?
! ---------------------------
  iOrbit=0
  DO iPar=1,nPar
    IF (locq(1,iPar) == 3) THEN
      iOrbit=1
      EXIT
    ENDIF
  ENDDO

  IF (iOrbit == 1) THEN
    IF (iorest == 3) THEN
      CALL gtflna(0,'LEOSTD ',filstd,ircstd)
      IF(ircstd /= 0) filstd='------'

      CALL gtflna(0,'LEORPR ',filrpr,ircrpr)
      IF(Ircrpr /= 0) filrpr='------'
    ELSE
      CALL gtflna(0,'STDORB ',filstd,ircstd)
      IF(ircstd /= 0) filstd='------'

      CALL gtflna(0,'RPRCOE ',filrpr,ircrpr)
      IF(Ircrpr /= 0) filrpr='------'
    ENDIF
  ENDIF

  iDir = 1
  iFil = 1
  nStoch = 0
  CALL stotra(idir,npar,locq,nstcep,nstcef,nstcef2,             &
              nsastc1,nsastc2,timstc,timstc2,numstc1,numstc2,   &
              nstoch,stotyp,svnsto,tstoch,iFil,stobnd,nstcep2)

! Write the NEQ
! -------------
  CALL neqwrite(filneq,titles,npar,nstat,lTPl,locq,partyp,      &
                aNor,bNor,xstat,stname,xstell,trplms,           &
                itropo,iextra,xxx,ncamp,taecmp,scastc,          &
                tbound,tstoch,tpol,scacen,nobs,nparms,          &
                filstd,filrpr,antsnx,nftot,nsmpnq,ielvnq,nanoff,&
                nsaoff,satoff,itrmap,itrgrd,namgim,epogim,      &
                scagim,nutnam,subnam,datum,nadmax,ifreq,        &
                nanspv,nsaspv,satspv,meatyp,rnxclk,secipl,      &
                ClkHed,clkRec,nEpSam,antcal,numcal,             &
                prncal,antrao,numrao,prnrao,nobspa,opLoad,      &
                timisb,USEGEOS,GOBSDEF)

END SUBROUTINE savneq


END MODULE
