MODULE s_RDITRP
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rditrp(maxsta,maxtrm,nAllSta,allStaNum,allStaName,       &
                  globalWindow,dtSim,ntrsta,statrp,trplms,sigtrs,   &
                  isgtrs,itrmap,itrgrd)

! -------------------------------------------------------------------------
! Purpose:    Reads the troposphere parameter input options for GPSEST
!
! Author:     R. Dach
!
! Created:    22-Jun-2001
!
! Changes:    30-Jul-2001 RD: "Time window" is a special option
!             10-Sep-2001 HU: initialize itrpmd for gettrp
!             08-Oct-2002 RD: New call of gtStaNum
!             16-Jan-2003 RD: Adapt to new GPSEST panel
!             28-Jan-2003 RD: "WITH_TROP" is invalid if no TROPOS-file
!             03-Feb-2003 RD: New call of GTSTANUM (weight is pointer now)
!             26-Mar-2003 RD: New parameter time window definition
!             01-Apr-2003 HU: Comment in DIMTST adapted
!             23-Apr-2003 RD: Nullify local pointers
!             24-Apr-2003 RD: Corrected type for parameter
!             11-May-2003 MM: piecewise linear troposphere
!             15-May-2003 HU: Initialize structures
!             07-Jul-2003 MM: new GETTRP call
!             08-Jul-2003 MM: chktrp mechanism included
!             04-Nov-2003 HB: Declare allStaName with (:)
!             19-Jan-2003 SS/MM: Revision of GPSEST input panels
!             02-Feb-2004 RD: No "Bus error" if no tropo to estimate
!             28-Jun-2005 MM: Unused variables removed
!             24-Aug-2006 AG: GMF and TAN(z) implemented
!             30-Jun-2008 RD: VMF added
!             21-May-2010 MF: Nullify hlpnam
!             19-Jul-2010 SL: tab characters removed
!             14-Oct-2010 RD: Read tropo-model from SR trpopt
!             04-Jan-2011 PS: Chen/Herring gradient mapping added
!             20-Sep-2012 RD: Use M_BERN with ONLY
!             20-Sep-2012 RD: Remove unused modules
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, &
                      keyValueLength, fileNameLength, staNameLength
  USE m_time,   ONLY: t_timint
  USE d_stalst, ONLY: t_staList, init_stalist
  USE d_trpest, ONLY: undef_Trp
  USE s_ckoptr
  USE s_dimtst
  USE s_ckoptt
  USE s_alcerr
  USE s_parint
  USE s_rdpwin
  USE s_gtstanum
  USE s_readkeys
  USE s_chktrp
  USE s_exitrc
  USE s_ckoptc
  USE s_gtflna
  USE s_readstsg
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                    :: maxsta       ! maximum number of stations
  INTEGER(i4b)                    :: maxtrm       ! max. number of tropo models
  INTEGER(i4b)                    :: nAllSta      ! number of all stations
  INTEGER(i4b), DIMENSION(*)      :: allStaNum    ! station numbers
  CHARACTER(LEN=staNameLength),   &
                DIMENSION(:)      :: allStaName   ! all station names
  TYPE(t_timint)                  :: globalWindow ! window to be processed
                                                  ! (from - to, MJD)
  REAL(r8b)                       :: dtsim        ! max. interval to identify
                                                  ! epoch (in days)

! output:
  INTEGER(i4b)                    :: ntrsta       ! # of troposphere requests
                                                  ! for individual stations
  INTEGER(i4b), DIMENSION(*)      :: statrp       ! station numbers for
                                                  ! troposphere requests
  REAL(r8b),    DIMENSION(2,*)    :: trplms       ! troposphere parameter
                                                  ! est. from .. to
  REAL(r8b),    DIMENSION(3,*)    :: sigtrs       ! a priori sigma in m
                                                  ! for indiv. tropo-param.
                                                  ! j=1: north (gradient)
                                                  ! j=2: east (gradient)
                                                  ! j=3: up (zenith delay)
  INTEGER(i4b), DIMENSION(*)      :: isgtrs       ! type of sigma
                                                  ! 0: absolute sigma
                                                  ! 1: sigma relative to the
                                                  !    previous parameter of
                                                  !    the same site
  INTEGER(i4b)                    :: itrmap       ! mapping function for
                                                  ! troposp.est.
                                                  ! 1: 1/cos(z)
                                                  ! 2: hopfield
                                                  ! 3: dry niell
                                                  ! 4: wet niell
                                                  ! 5: dry gmf
                                                  ! 6: wet gmf
                                                  ! 7: dry vmf
                                                  ! 8: wet vmf
  INTEGER(i4b), DIMENSION(*)      :: itrgrd       ! i=1:
                                                  ! est. of tropo. gradients
                                                  !    0: no estimation
                                                  !    1: tilting
                                                  !    2: linear
                                                  !    3: tanz
                                                  !    4: Chen/Herring
                                                  ! i=2: ratio of number of
                                                  ! zenith to gradient param.

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'rditrp'


! Local Variables
! ---------------
  TYPE(t_staList)                 :: hlpList
  TYPE(t_timint),                  &
            DIMENSION(:), POINTER :: trpWin
  TYPE(t_timint),                  &
            DIMENSION(:), POINTER :: grdWin

  CHARACTER(LEN=keyValueLength) , &
        DIMENSION(:)  , POINTER   :: keyValue
  CHARACTER(LEN=fileNameLength)   :: trpFilNam
  CHARACTER(LEN=fileNameLength)   :: fixFilNam
  CHARACTER(LEN=staNameLength)  , &
        DIMENSION(:)  , POINTER   :: hlpNam

  INTEGER(i4b)                    :: nNoTropo
  INTEGER(i4b), DIMENSION(maxsta) :: noTrpSta    ! Stations with no troposphere
  INTEGER(i4b)                    :: nStat
  INTEGER(i4b), DIMENSION(maxsta) :: sSigNum     ! Stations with special sigma
  INTEGER(i4b)                    :: ntrstaLocal
  INTEGER(i4b), DIMENSION(maxsta) :: statrpLocal
  INTEGER(i4b)                    :: ngrsta
  INTEGER(i4b)                    :: iSigma
  INTEGER(i4b)                    :: iSta, jSta
  INTEGER(i4b)                    :: irCode
  INTEGER(i4b)                    :: irc
  INTEGER(i4b)                    :: ii, jj
  INTEGER(i4b)                    :: iTrpOk

  REAL(r8b), DIMENSION(2)         :: sigabs     ! absolute sigma vertical
                                                ! (1: normal, 2: special)
  REAL(r8b), DIMENSION(2)         :: sigabs_g   ! absolute sigma gradient
                                                ! (1: normal, 2: special)
  REAL(r8b), DIMENSION(2)         :: sigrel     ! relative sigma vertical
                                                ! (1: normal, 2: special)
  REAL(r8b), DIMENSION(2)         :: sigrel_g   ! relative sigma gradient
                                                ! (1: normal, 2: special)
  REAL(r8b),                       &
      DIMENSION(:,:), ALLOCATABLE :: staWgt     ! weights for stations
                                                ! (1: vert-abs, 2: vert-rel
                                                !  3: grad-abs, 4: grad-rel)
                                                !  5: grad-abs, 6: grad-rel)
  REAL(r8b),                       &
      DIMENSION(:,:), POINTER     :: hlpWeight
  REAL(r8b)                       :: trpSta
  REAL(r8b)                       :: grdSta
  REAL(r8b)                       :: t_0,dt_0

  LOGICAL                         :: setSpec    ! setup special list or not


! Init some variables
! -------------------
  irCode = 0

  NULLIFY(keyValue)
  NULLIFY(hlpWeight)
  NULLIFY(trpWin)
  NULLIFY(grdWin)
  NULLIFY(hlpnam)
  CALL init_stalist(hlpList)

! Check whether a tropo file is introduced
! ----------------------------------------
  IF (itrmap == undef_Trp) THEN

! Get the mapping function
! ------------------------
    CALL readKeys('MAPPNG', keyValue, irc)

    CALL ckoptc(1,'MAPPNG', keyValue,                                  &
                (/'WET_VMF  ','DRY_VMF  ','WET_GMF  ','DRY_GMF  ',     &
                  'WET_NIELL','DRY_NIELL','COSZ     ','HOPFIELD '/),   &
                'Troposphere mapping function',srName,irc,irCode,      &
                maxVal=1, valList=(/8,7,6,5,4,3,1,2/), result1=itrmap)

! Is a gradient estimation requested
! ----------------------------------
    CALL readKeys('TRPGRD', keyValue, irc)

    CALL ckoptc(1,'TRPGRD', keyValue,                                  &
                (/'NONE   ','TANZ   ','CHENHER','TILTING','LINEAR '/), &
                'Troposphere gradient model',srName,irc,irCode,        &
                maxVal=1, valList=(/0,3,4,1,2/), result1=itrgrd(1))
  ENDIF

! Read the absolute sigma for the vert. Parameters
! ------------------------------------------------
  sigabs = 0d0
  CALL readKeys('SIGABS', keyValue, irc)

  CALL ckoptr(1,'SIGABS', keyValue,srName,                        &
              'Absolute sigma for vert. troposphere',irc,irCode,  &
              empty=0d0,ge=0d0,maxVal=1,result1=sigabs(1))

! Read the relative sigma for the vert. Parameters
! ------------------------------------------------
  sigrel = 0d0
  CALL readKeys('SIGREL', keyValue, irc)

  CALL ckoptr(1,'SIGREL', keyValue,srName,                        &
              'Relative sigma for vert. troposphere',irc,irCode,  &
              empty=0d0,ge=0d0,maxVal=1,result1=sigrel(1))


! Read the absolute sigma for the gradients
! -----------------------------------------
  sigabs_g = 0d0
  IF (itrgrd(1) /= 0) THEN

    CALL readKeys('SIGABS_G', keyValue, irc)

    CALL ckoptr(1,'SIGABS_G', keyValue,srName,                      &
                'Absolute sigma for grad. troposphere',irc,irCode,  &
                empty=0d0,ge=0d0,maxVal=1,result1=sigabs_g(1))

  ENDIF

! Read the relative sigma for the gradients
! -----------------------------------------
  sigrel_g = 0d0
  IF (itrgrd(1) /= 0) THEN

    CALL readKeys('SIGREL_G', keyValue, irc)

    CALL ckoptr(1,'SIGREL_G', keyValue,srName,                      &
                'Relative sigma for grad. troposphere',irc,irCode,  &
                empty=0d0,ge=0d0,maxVal=1,result1=sigrel_g(1))

  ENDIF

! Setup troposphere parameters for special stations
! -------------------------------------------------
  call readKeys('SPESEL',keyValue,irc)
  setSpec = (irc == 0 .AND. keyValue(1) /= 'NONE')

! Read the special absolute sigma for the vert. Parameters
! --------------------------------------------------------
  call readKeys('SPESEL',keyValue,irc)
  IF (setSpec .AND. irc == 0 .AND. &
      keyValue(1) /= 'NONE' .AND. keyValue(1) /= 'FROM_FILE') THEN

    CALL readKeys('SPEABS', keyValue, irc)

    CALL ckoptr(1,'SPEABS', keyValue,srName,                            &
                'Special abs. sigma for vert. troposphere',irc,irCode,  &
                empty=0d0,ge=0d0,maxVal=1,result1=sigabs(2))

! Read the special relative sigma for the vert. Parameters
! --------------------------------------------------------
    CALL readKeys('SPEREL', keyValue, irc)

    CALL ckoptr(1,'SPEREL', keyValue,srName,                            &
                'Special rel. sigma for vert. troposphere',irc,irCode,  &
                empty=0d0,ge=0d0,maxVal=1,result1=sigrel(2))

! Read the special absolute sigma for the gradients
! -------------------------------------------------
    IF (itrgrd(1) /= 0) THEN
      CALL readKeys('SPEABS_G', keyValue, irc)

      CALL ckoptr(1,'SPEABS_G', keyValue,srName,                          &
                  'Special abs. sigma for grad. troposphere',irc,irCode,  &
                  empty=0d0,ge=0d0,maxVal=1,result1=sigabs_g(2))
    ENDIF

! Read the special relative sigma for the gradients
! -------------------------------------------------
    IF (itrgrd(1) /= 0) THEN
      CALL readKeys('SPEREL_G', keyValue, irc)

      CALL ckoptr(1,'SPEREL_G', keyValue,srName,                          &
                  'Special rel. sigma for grad. troposphere',irc,irCode,  &
                  empty=0d0,ge=0d0,maxVal=1,result1=sigrel_g(2))
    ENDIF

  ENDIF ! Special setting necessary

! Allocate a "dummy variable"
! ---------------------------
  ALLOCATE(hlpNam(nAllSta),stat=irc)
  CALL alcerr(irc,'hlpNam',(/nAllSta/),'rditrp')

! Get the list of stations with no troposphere
! --------------------------------------------
  call readKeys('NTRPSEL',keyValue,irc)

! Special Handling for all stations with no values in TRP file
  IF (irc == 0 .AND. keyValue(1) == 'WITH_TROPO') THEN
    nNoTropo = 0
    trpFilNam = ' '

! initialize no tropo stations
    CALL chktrp(0,iTrpOk,filNam=trpFilNam,timInt=globalWindow)

    DO iSta = 1, nAllSta
      CALL chktrp(1,iTrpOk,staNam=allStaName(iSta))
      IF (iTrpOk==0) THEN
        nNoTropo = nNoTropo + 1
        noTrpSta(nNoTropo) = allStaNum(iSta)
      END IF
    ENDDO

! All other keywords can be handles by the standard routine
  ELSE
    CALL gtStaNum(nAllSta, allStaNum, allStaName,  &
         'NTRPSEL', 'STATION6', 'STAFILE6', ' ',   &
         nNoTropo, noTrpSta, hlpNam, 0, hlpWeight)
  ENDIF

! Get a list of stations with special sigmas
! ------------------------------------------
  iSigma = 6
  call readKeys('SPESEL',keyValue,irc)

! A 2-column sigma file is accepted if no gradient estimation was set
  IF (irc == 0 .AND. keyValue(1) == 'FROM_FILE') THEN
    CALL gtflna(0,'STAFILE3', fixFilNam, irc)
    iSigma = -1
    IF (irc == 0 .AND. LEN_TRIM(fixFilNam) > 0) THEN
      CALL readstsg(fixfilnam,iSigma,hlpList)
      DEALLOCATE(hlpList%staNam, stat=irc)
      DEALLOCATE(hlpList%sigma , stat=irc)
    ENDIF

    IF ((itrgrd(1) /= 0 .OR. iSigma /= 2) .AND. iSigma /= 6) iSigma = 4

  ENDIF

! Store special sigmas for SR gtStaNum
! ------------------------------------
  ALLOCATE(hlpWeight(iSigma,nAllSta),stat=irc)
  CALL alcerr(irc, 'hlpWeight', (/iSigma,nAllSta/), 'rditrp')

  hlpWeight(1,1) = sigabs(2)
  hlpWeight(2,1) = sigrel(2)
  IF (iSigma > 2) THEN
    hlpWeight(3,1) = sigabs_g(2)
    hlpWeight(4,1) = sigrel_g(2)
  ENDIF
  IF (iSigma > 4) THEN
    hlpWeight(5,1) = sigabs_g(2)
    hlpWeight(6,1) = sigrel_g(2)
  ENDIF

! Get the list of stations with special sigmas
! --------------------------------------------
  CALL gtStaNum(nAllSta, allStaNum, allStaName,  &
       'SPESEL', 'STATION3', 'STAFILE3', ' ',    &
       nStat, sSigNum, hlpNam, iSigma, hlpWeight)

! Generate a list of "station sigmas"
! -----------------------------------
  ALLOCATE(staWgt(6,nAllSta-nNoTropo), stat=irc)
  CALL alcerr(irc,'staWgt',(/6,nAllSta-nNoTropo/),'rditrp')
  staWgt = 0d0

  ntrstaLocal = 0
  iStaLoop: DO iSta = 1,nAllSta

! Station without tropo
    DO jSta = 1, nNoTropo
      IF (allStaNum(iSta) == noTrpSta(jSta)) CYCLE iStaLoop
    ENDDO

! Station with special sigma
    DO jSta = 1, nStat
      IF (allStaNum(iSta) == sSigNum(jSta)) THEN
        ntrstaLocal = ntrstaLocal + 1
        statrpLocal(ntrstaLocal) = allStaNum(iSta)
        staWgt(1:iSigma,ntrstaLocal) = hlpWeight(1:iSigma,jSta)
        IF (iSigma == 4) &
          staWgt(5:6,ntrstaLocal) = hlpWeight(3:4,jSta)
        CYCLE iStaLoop
      ENDIF
    ENDDO

! All other station get the normal sigma
    ntrstaLocal = ntrstaLocal + 1
    statrpLocal(ntrstaLocal) = allStaNum(iSta)
    staWgt(1,ntrstaLocal) = sigabs(1)
    staWgt(2,ntrstaLocal) = sigrel(1)
    staWgt(3,ntrstaLocal) = sigabs_g(1)
    staWgt(4,ntrstaLocal) = sigrel_g(1)
    staWgt(5,ntrstaLocal) = sigabs_g(1)
    staWgt(6,ntrstaLocal) = sigrel_g(1)
  ENDDO iStaLoop

  DEALLOCATE(hlpWeight, stat=irc)
  DEALLOCATE(hlpNam, stat=irc)

! Get the length of vertical parameters
! -------------------------------------
  CALL readKeys('NUMPAR', keyValue, irc)

  CALL ckoptt(1,'NUMPAR',keyValue,srName,                       &
              'Length of vert. tropos. param.',irc,irCode,      &
              maxVal=1,gt=0d0,result1=trpSta)

! Get the number of gradient parameters
! -------------------------------------
  IF (itrgrd(1) > 0 .AND. trpSta > 0d0) THEN

    CALL readKeys('NUMGRD', keyValue, irc)

    CALL ckoptt(1,'NUMGRD',keyValue,srName,                       &
                'Length of grad. tropos. param.',irc,irCode,      &
                maxVal=1,gt=0d0,result1=grdSta)

    IF (grdSta > 0d0) THEN

      IF (DABS(grdSta/trpSta - INT(grdSta/trpSta)) > dtSim*24d0/2d0) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,/)')                                  &
        ' *** SR RDITRP: The length of gradient troposphere parameters', &
        'must be a multiple of the length of vertical parameters.'
        irCode = irCode + 1
      ENDIF

      itrgrd(2) = NINT(grdSta/trpSta)

    ENDIF

  ENDIF


! Get the parameter time window definition
! ----------------------------------------
  CALL rdpwin('PAR_OFF',(/' ',' '/),t_0,dt_0)

! Get the time windows for vertical troposphere
! ---------------------------------------------
  ntrsta = 0
  IF (itrgrd(1) == 0  .AND. trpSta > 0d0) THEN
    CALL parint(globalWindow,dtSim,t_0,dt_0,trpSta, &
                'Troposphere zenith parameters',ntrsta,trpWin)

! Get the time windows for vertical and gradient troposphere
! ----------------------------------------------------------
  ELSE IF (grdSta > 0d0 .AND. trpSta > 0d0) THEN
    ngrsta = 0
    CALL parint(globalWindow,dtSim,t_0,dt_0,grdSta, &
                'Troposphere gradient parameters',ngrsta,grdWin)

    IF (ngrsta > 0) THEN
      grdWin(1)%t(2) = grdWin(ngrSta)%t(2)-dtSim
      CALL parint(grdWin(1),dtSim,grdWin(1)%t(1),0d0,trpSta, &
                  'Troposphere zenith parameters',ntrsta,trpWin)

      DEALLOCATE(grdWin,stat=irc)
    ELSE
      CALL parint(globalWindow,dtSim,t_0,dt_0,trpSta, &
                  'Troposphere zenith parameters',ntrsta,trpWin)
    ENDIF
  ENDIF


! Increase number of parameters by one (piecewise linear)
! -------------------------------------------------------
  IF (nTrSta > 0) nTrSta = nTrSta+1


! Too many troposphere requests
! -----------------------------
  CALL dimtst(1,1,2,'rditrp','maxtrm',                          &
              'troposphere parameter per station',              &
              'Parameter is defined in module "P_GPSEST.f90".', &
              ntrsta*ntrstaLocal, maxtrm, irc)

! Generate the setup for the parameters
! -------------------------------------
  ista   = 0
  DO ii = 1, ntrstaLocal
    DO jj = 1, ntrsta
      ista = ista + 1
      statrp(ista) = statrpLocal(ii)

! Special handling of last parameter
      IF (jj<nTrSta) THEN
        trpLms(1:2,iSta) = trpWin(jj)%t(1:2)
      ELSE
        trpLms(1,iSta) = trpWin(jj-1)%t(2)
        trpLms(2,iSta) = trpWin(jj-1)%t(1)
      END IF

      IF (jj == 1) THEN
        isgtrs(ista)   = 0
        sigtrs(1,ista) = staWgt(3,ii)
        sigtrs(2,ista) = staWgt(5,ii)
        sigtrs(3,ista) = staWgt(1,ii)
      ELSE
        isgtrs(ista)   = 1
        sigtrs(1,ista) = staWgt(4,ii)
        sigtrs(2,ista) = staWgt(6,ii)
        sigtrs(3,ista) = staWgt(2,ii)
      ENDIF
    ENDDO
  ENDDO

  ntrsta = iSta

  DEALLOCATE(trpWin,  stat=irc)
  DEALLOCATE(staWgt,  stat=irc)
  DEALLOCATE(keyValue,stat=irc)

! Exit if there was an error in a ckopt-SR
! ----------------------------------------
  IF (irCode /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE rditrp

END MODULE
