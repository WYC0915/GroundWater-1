MODULE s_STANEQ
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE staneq(iPrt  , neqFileName   , orbFil,                  &
                  namList, clkList, limits, parOrb, parGcc, parSao,&
                  parAtm, parErp, parSap, parRao, parRap, parRco,  &
                  parDcb, parGrd, parRgb, parGsp)

! -------------------------------------------------------------------------
! Purpose:    Collect information from NEQ files
!
! Author:     L.Mervart
!
! Created:    09-Aug-1999
!
! Changes:    26-Jun-2001 RD: Use alcerr for allocation
!             30-Oct-2001 RD: Rename par%name using sta-info file
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             29-Dec-2001 HU: Interface to nqrdhead added
!             23-Jan-2002 CU: Get some parameter input options from the neqs:
!                             parOrb, parcom, parsaof
!             30-Jan-2002 CU: Error if same STD for different NEQs used
!             04-Feb-2002 CU: Write locq and miscellaneous information in
!                             the protocol
!             05-Feb-2002 CU: Removed: Error if same STD for different NEQs
!             06-Feb-2002 CU: Print error message if there are different
!                             sets of stoch. pulses in the NEQs
!             07-Feb-2002 CU: Change format for miscellaneous information
!             28-Feb-2002 CU: If iGrp == 0 THEN iGrp = 1 (locq(4)sat.ant.off)
!             08-Apr-2002 CU: Replaced "center of mass" with "geocenter"
!             15-Apr-2002 RD: Init time intervals with 1D20 (for SR timst2)
!             19-Apr-2002 RD: Optional output
!             25-Sep-2002 HU: Remove i_astlib
!             12-Nov-2002 CU: Change write statements for misc. neq inform.
!             13-Nov-2002 HB: Get some parameter input options from the neqs:
!                             parAtm
!             10-Dec-2002 CU: Get parameter input options from neqs: parErp,
!                             change format of some title lines
!             20-Apr-2003 HU: Order input files chronologically
!             01-May-2003 MM: Initialize opt%timRefCrd to a mean value
!             17-May-2003 HU: Initialize structure
!             27-May-2003 CU: Change output format
!             28-May-2003 HU: Parstrg extended to 29
!             16-Jun-2003 RD: Sort neqFileName (instead of opt%neqFileName)
!             09-Jul-2003 RD: staInfo comes in a an argument
!             11-Nov-2003 RS: parSao: read user-defined group number
!             17-Dec-2003 RS: Add parSap
!             22-Jan-2004 HB: Avoid 'ADVANCE=NO' after '(nX)' format
!             07-Feb-2004 HU: Truncate station names
!             13-Feb-2004 HU: Error concerning noabc corrected
!             26-Sep-2004 HU: Error concerning noabc corrected
!             08-Nov-2004 MF: Nullify pointers (parSao%..., parSap%...)
!             05-May-2005 HU: Order input NEQs according to opt%chrono
!             24-May-2005 SS: Truncate 4-character station names
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!             04-Oct-2006 AG: Satellite specific antenna PCO/PCV implemented
!             08-Feb-2007 RD: misc%nObs/nParms i4b->r8b
!             16-Apr-2007 AG: parsao%santoff initialized
!             06-Jul-2007 AG: Call DEFREQ to initialize faclin for d_phaecc
!             08-Aug-2007 AG: PRN list compilation corrected
!             13-Aug-2007 AG: PRN list compilation removed
!             23-Jan-2008 RD: Read t_par using a function from the module
!             25-Jan-2008 RD: add RAO/RAP parameters
!             26-Feb-2008 RD: Extent the parorb structure
!             04-May-2009 RD: Scaling of loading models added
!             10-May-2009 RD: Receiver clock offsets/biases implemented
!             10-Jun-2009 RD: Read receiver/satellite clock lists from NEQ
!             24-Jun-2009 RD: Put direct constraints on DCBs
!             13-Aug-2009 DT: Add range bias parameters
!             27-Nov-2009 RD: Station exceptions for pre-elimination
!             28-Nov-2009 RD: Allocate sat. antenna arrays: maxOff (old maxSat)
!             15-Nov-2009 RD: Correct counting of parameters
!             04-Jan-2010 SL: HOI scaling parameters (Type 27) added
!             04-Mar-2010 RD: Eliminate parameters in case of equipment changes
!             08-Oct-2010 RD: Extension of parAtm
!             11-Oct-2010 RD: Remove unused variables
!             26-Oct-2010 SL: two truncation bugs corrected, m_bern with ONLY,
!                             removal of unused modules/vars
!             29-Oct-2010 SL: problem text in %remark (not %flg = 1001)
!             30-Nov-2010 DT: Helmert parameters added
!             30-Nov-2010 MM: GNSS-specific parameters
!             03-May-2011 RD: Correct list for stations for "noabc"
!             17-Nov-2011 SL: use srName, redcrx to reduce staInfo structure
!             23-Jan-2012 RD: Correct logic to update parAtm
!             05-Mar-2012 RD: Use listi4 as module now
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnErr, lfnLoc, lfnPrt, &
                      staNameLength, timStrgLength2, keyValueLength, lineLength
  USE m_global, ONLY: maxSys
  USE m_time,   ONLY: t_timint, OPERATOR(+)
  USE d_par,    ONLY: t_par,maxParTyp,readpar
  USE d_neq,    ONLY: t_neq,maxStaSin,maxSat,maxOff
  USE p_addneq, ONLY: t_parOrb,t_parGcc,t_parSao,t_paratm,t_parerp,t_parSap,&
                      t_parRao,t_parRap,t_parRco,t_parRgb,t_optLoad,t_namLst, &
                      opt, staInfo, t_parGsp
  USE d_stacrx, ONLY: t_staProb, undef_c, undef_i, undef_e
  USE d_trpest, ONLY: undef_Trp
  USE f_isstacrx
  USE s_dordup
  USE s_alcerr
  USE s_opnfil
  USE f_listi4
  USE s_gtstna
  USE s_nqrdhead
  USE s_opnerr
  USE s_prfile
  USE s_timst2
  USE s_exitrc
  USE s_dimtst
  USE s_redcrx
  IMPLICIT NONE

! List of Parameters
! ------------------
! input
  INTEGER(i4b)                               :: iPrt  ! contribute to pgm output
                                                      ! 0: no, e.g. MENUAUX
                                                      ! 1: yes, incl. redcrx
  CHARACTER(LEN=*) , DIMENSION(:)            :: neqFileName

! output
  CHARACTER(LEN=*) , DIMENSION(:,:)          :: orbFil
  TYPE(t_namLst)   , DIMENSION(:)            :: namList ! List of parameter
                                                        !   names per param.type
  CHARACTER(LEN=*) , DIMENSION(:,:), POINTER :: clkList ! List of clocks
  TYPE(t_timint)   , DIMENSION(:)            :: limits
  TYPE(t_parOrb)                             :: parOrb  ! orbit parameters
  TYPE(t_parGcc)                             :: parGcc  ! geocenter coordinates
  TYPE(t_parSao)   , DIMENSION(:),   POINTER :: parSao  ! sat. ant. offsets
  TYPE(t_parSap)   , DIMENSION(:),   POINTER :: parSap  ! sat. ant. patterns
  TYPE(t_parAtm)                             :: parAtm  ! atmosphere parameters
  TYPE(t_parErp)                             :: parErp  ! earth rotation param.
  TYPE(t_parRco)                             :: parRco  ! Receiver clock offset/bias
  TYPE(t_parRao)                             :: parRao  ! rec. ant. offsets
  TYPE(t_parRap)                             :: parRap  ! rec. ant. patterns
  TYPE(t_parRgb)                             :: parRgb  ! Range bias parameters
  TYPE(t_parGsp)                             :: parGsp  ! GNSS-spec parameters
  TYPE(t_optLoad) , DIMENSION(:)             :: parGrd  ! scaling of Vienna grid
  INTEGER(i4b)    , DIMENSION(:,:)           :: parDcb  ! DCB-Parameters

! List of functions
! -----------------

! Local types
! -----------
  TYPE t_parNam
    CHARACTER(LEN=20)                      :: name
    INTEGER(i4b)                           :: toBeDel
    TYPE(t_timint), DIMENSION(maxParTyp)   :: parInt
  END TYPE t_parNam

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER              :: srName = 'STANEQ'

! Local variables
! ---------------
  TYPE(t_parNam), DIMENSION(:), ALLOCATABLE:: parNam
  TYPE(t_parNam), DIMENSION(:), ALLOCATABLE:: parHlp
  TYPE(t_timint)                           :: hlpInt
  TYPE(t_staProb),DIMENSION(:), ALLOCATABLE:: hlpProb
  TYPE(t_neq)                              :: neq
  TYPE(t_par)                              :: par
  TYPE(t_parSao)                           :: hlpSao
  TYPE(t_parSap)                           :: hlpSap

  CHARACTER(LEN=staNameLength),DIMENSION(1):: newNam
  CHARACTER(LEN=timStrgLength2)            :: tStrg
  CHARACTER(LEN=timStrgLength2)            :: tStrgt
  CHARACTER(LEN=keyValueLength), DIMENSION(:,:), ALLOCATABLE :: filNam
  CHARACTER(LEN=lineLength)                :: hlpStr

  INTEGER(i4b)                             :: iNam
  INTEGER(i4b)                             :: nParNam
  INTEGER(i4b)                             :: nSta
  INTEGER(i4b)                             :: iFil
  INTEGER(i4b)                             :: jFil
  INTEGER(i4b)                             :: lFil
  INTEGER(i4b)                             :: nFil
  INTEGER(i4b)                             :: nClk
  INTEGER(i4b)                             :: iClk
  INTEGER(i4b)                             :: ipar
  INTEGER(i4b)                             :: iReq
  INTEGER(i4b)                             :: iInfo
  INTEGER(i4b)                             :: jInfo
  INTEGER(i4b)                             :: nProb
  INTEGER(i4b)                             :: ios, iac
  INTEGER(i4b)                             :: ityp
  INTEGER(i4b)                             :: lfnneq
  INTEGER(i4b)                             :: iGrp
  INTEGER(i4b)                             :: iGrd
  INTEGER(i4b)                             :: ii, iDmy
  INTEGER(i4b)                             :: irc
  INTEGER(i4b)                             :: ohelp
  INTEGER(i4b)                             :: sat_old
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE  :: indx
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE  :: ind1
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE  :: ind2
  REAL(r8b)   , DIMENSION(:), ALLOCATABLE  :: nobs
  REAL(r8b)   , DIMENSION(:), ALLOCATABLE  :: npar
  REAL(r8b)   , DIMENSION(:), ALLOCATABLE  :: nparms
  REAL(r8b)   , DIMENSION(:,:),ALLOCATABLE :: parNum
  INTEGER(i4b)                             :: iParTyp
  INTEGER(i4b)                             :: iLoop, nLoop
  INTEGER(i4b)                             :: iStart, iEnd
  INTEGER(i4b)                             :: newstc
  INTEGER(i4b)                             :: error
  INTEGER(i4b)                             :: numstc
  INTEGER(i4b),DIMENSION(staInfo%nRenam)   :: renamList
  INTEGER(i4b),DIMENSION(staInfo%nProb,maxParTyp) :: delList

  REAL(r8b), DIMENSION(2)                  :: timObs
  REAL(r8b), DIMENSION(:,:), ALLOCATABLE   :: timFil
  REAL(r8b)                                :: nobsTot
  REAL(r8b)                                :: numLoop

  LOGICAL                                  :: sorted
  LOGICAL                                  :: printPar
  LOGICAL                                  :: newModel

  CHARACTER(LEN=37),DIMENSION(maxParTyp+3), PARAMETER :: parStrg =  &
     (/' Station coordinates                 ', &  !  1
       ' Receiver clocks                     ', &  !  2
       ' Orbital elements                    ', &  !  3
       '                                     ', &  !  4
       ' Receiver antenna offset parameters  ', &  !  5
       ' Site-specific troposphere parameters', &  !  6
       '                                     ', &  !  7
       ' Differential code bias parameters   ', &  !  8
       '                                     ', &  !  9
       ' Earth rotation parameters           ', &  ! 10
       ' Stochastic orbit parameters         ', &  ! 11
       ' Satellite antenna offset parameters ', &  ! 12
       ' Earth potential parameters          ', &  ! 13
       '                                     ', &  ! 14
       '                                     ', &  ! 15
       ' Geocenter coordinates               ', &  ! 16
       '                                     ', &  ! 17
       ' Receiver antenna patterns           ', &  ! 18
       ' Global ionosphere model parameters  ', &  ! 19
       '                                     ', &  ! 20
       '                                     ', &  ! 21
       ' Scaling factors for Vienna grid file', &  ! 22
       ' Epochwise receiver clocks           ', &  ! 23
       ' Epochwise satellite clocks          ', &  ! 24
       ' Satellite antenna patterns          ', &  ! 25
       ' Range biases (from SLR)             ', &  ! 26
       ' Higher-order iono scaling parameters', &  ! 27
       ' Helmert transformation parameters   ', &  ! 28
       ' Not used                            ', &  ! 29
       ' GNSS-specific parameters            ', &  ! 30
       ' Total number of explicit parameters ', &
       ' Total number of implicit parameters ', &
       ' Total number of adjusted parameters '/)

! Initialize time intervals, number of stations
! ---------------------------------------------
  lfnneq = lfnloc-10  ! lfnloc is used in gtstna
  DO ityp = 1, maxParTyp
    limits(ityp)%t(1) = HUGE(0.d0)
    limits(ityp)%t(2) = 0.d0
  ENDDO

! Init some counter
! -----------------
  nParNam         = 0
  parGcc%nGcc     = 0
  parOrb%nOrb     = 0
  parOrb%seqorb   = 0
  parAtm%nTro     = 0
  parAtm%nModel   = 0
  parAtm%nIon     = 0
  parAtm%nHoi     = 0
  parErp%nErp     = 0
  parRco%isSta    = 0
  parRco%isFrq    = 0
  parRco%isSat    = 0
  parRao%nRao     = 0
  parRao%rao      = 0
  parRap%nRapGrd  = 0
  parRap%nRapShm  = 0
  parRgb%nRgb     = 0
  parGsp%nTra     = 0
  parGsp%nSysTra  = 0
  parGsp%sysTra   = 0
  parGsp%nTrp     = 0
  parGsp%nSysTrp  = 0
  parGsp%sysTrp   = 0
  opt%nStcep      = 0
  opt%rsw         = 0
  nobsTot         = 0.D0
  timObs(1)       = 1.D20
  timObs(2)       = 0.D0
  newstc          = 1
  error           = 0
  parGrd(:)%keyw  = ''
  parGrd(:)%nSta  = -1
  parGrd(:)%nPar  = 1

  nFil=SIZE(neqFileName)
  ALLOCATE(parSao(nFil),stat=iac)
  CALL alcerr(iac,'parSao',(/ nFil /),srName)
  ALLOCATE(parSap(nFil),stat=iac)
  CALL alcerr(iac,'parSap',(/ nFil /),srName)
  ALLOCATE(parAtm%trpMod(nFil),stat=iac)
  CALL alcerr(iac,'parAtm%trpMod',(/ nFil /),srName)
  ALLOCATE(parOrb%epoOrb(nFil),stat=iac)
  CALL alcerr(iac,'parOrb%epoOrb',(/ nFil /),srName)
  ALLOCATE (filNam(1,nFil), stat=iac)
  CALL alcerr(iac, 'filNam',(/1,nFil/),srName)
  ALLOCATE (indx(nFil), stat=iac)
  CALL alcerr(iac, 'indx',(/nFil/),srName)
  ALLOCATE (ind1(nFil), stat=iac)
  CALL alcerr(iac, 'ind1',(/nFil/),srName)
  ALLOCATE (ind2(nFil), stat=iac)
  CALL alcerr(iac, 'ind2',(/nFil/),srName)
  ALLOCATE (timFil(2,nFil), stat=iac)
  CALL alcerr(iac, 'timFil',(/2,nFil/),srName)
  ALLOCATE (nobs(nFil), stat=iac)
  CALL alcerr(iac, 'nobs',(/nFil/),srName)
  ALLOCATE (npar(nFil), stat=iac)
  CALL alcerr(iac, 'npar',(/nFil/),srName)
  ALLOCATE (nparms(nFil), stat=iac)
  CALL alcerr(iac, 'nparms',(/nFil/),srName)
  ALLOCATE (parNum(maxParTyp+3,nFil), stat = iac)
  CALL alcerr(iac, 'parNum',(/maxPartyp+3,nFil/),srName)
  ALLOCATE (parNam(maxStaSin), stat = iac)
  CALL alcerr(iac, 'parNam',(/maxStaSin/),srName)

  parNum = 0
  parDcb = 0

  renamList(:) = 0
  delList(:,:) = 0

! Loop over all NEQ-files
! -----------------------
  DO iFil = 1, nFil

! Read the NEQ header
! -------------------
    CALL opnfil(lfnneq,neqFileName(iFil),'OLD','UNFORMATTED', &
         'READONLY',' ',ios)
    CALL opnerr(lfnerr,lfnneq,ios,neqFileName(iFil),srName)
    CALL nqrdhead(lfnneq,neq)

    NULLIFY(parSao(iFil)%nsaoff)
    NULLIFY(parSao(iFil)%satoff)
    NULLIFY(parSap(iFil)%nsaspv)
    NULLIFY(parSap(iFil)%satspv)

    ALLOCATE(parSao(iFil)%nsaoff(maxOff),stat=iac)
    CALL alcerr(iac,'parSao(iFil)%nsaoff',(/maxOff/),srName)

    ALLOCATE(parSao(iFil)%satoff(maxSat,maxOff),stat=iac)
    CALL alcerr(iac,'parSao(iFil)%satoff',(/maxSat,maxOff/),srName)

    ALLOCATE(parSap(iFil)%nsaspv(maxOff),stat=iac)
    CALL alcerr(iac,'parSap(iFil)%nsaspv',(/maxOff/),srName)

    ALLOCATE(parSap(iFil)%satspv(maxSat,maxOff),stat=iac)
    CALL alcerr(iac,'parSap(iFil)%satspv',(/maxSat,maxOff/),srName)

! Read the Parameters, Count number of Stations
! ---------------------------------------------
    parSao(iFil)%nsaoff  = 0
    parSao(iFil)%nanoff  = 0
    parSao(iFil)%satoff  = 0
    parSao(iFil)%gnroff  = 0
    parSap(iFil)%nsaspv  = 0
    parSap(iFil)%nanspv  = 0
    parSap(iFil)%satspv  = 0
    parSap(iFil)%gnrspv  = 0
    parSao(iFil)%santoff = 0
    parOrb%epoOrb(iFil)  = 0d0
    iReq                 = 0
    ohelp                = 0
    sat_old              = 0
    numstc               = 0
    timFil(1,iFil)       = 1.D20
    timFil(2,iFil)       = 0.D0

    DO ipar = 1, neq%misc%npar
      par = readPar(lfnneq,1,neq%version)

! Truncate station names after position noabc
! -------------------------------------------
      IF (opt%noabc < staNameLength .AND.                         &
          (par%locq(1) ==  1 .OR.                                 &
           par%locq(1) ==  2 .OR.                                 &
           par%locq(1) ==  6 .OR.                                 &
           par%locq(1) ==  8 .AND. par%locq(2) ==  2 .OR.         &
           par%locq(1) == 19 .OR.                                 &
           par%locq(1) == 22 .OR.                                 &
           par%locq(1) == 23 .OR.                                 &
           par%locq(1) == 26 .OR.                                 &
           par%locq(1) == 27 .OR.                                 &
           par%locq(1) == 30)) THEN
        par%name(opt%noabc+1:)=' '
! Truncate 4-character station names (at request of swisstopo)
!!        IF (par%name(7:) .EQ. ' ') par%name(5:)=' '
      ENDIF

! Rename stations using station info file
! ---------------------------------------
      IF ( staInfo%nRenam > 0 .AND. LEN_TRIM(par%name) > 0) THEN
        CALL gtstna(0,0,par%time%mean, &
             1,(/par%name(1:staNameLength)/),(/' '/),opt%stacrux,staInfo, &
             newnam,renamList)
        par%name = newnam(1)
      ENDIF

! Count parameters per type and file
! ----------------------------------
      parNum(par%locq(1),iFil) = parNum(par%locq(1),iFil) + 1

! Is the parameter in the list of bad stations?
! ---------------------------------------------
      IF(isStaCrx(par,delList)) CYCLE

! Time intervals
! --------------
      IF ( par%time%mean-par%time%half < limits(par%locq(1))%t(1) ) THEN
        limits(par%locq(1))%t(1) = par%time%mean-par%time%half
      ENDIF
      IF ( par%time%mean+par%time%half > limits(par%locq(1))%t(2) ) THEN
        limits(par%locq(1))%t(2) = par%time%mean+par%time%half
      ENDIF

! Update the list of parameter names per type
! -------------------------------------------
      IF (par%name /= '') THEN

        DO iNam = 1,nParNam+1
          ! Add a new value to the list
          IF (iNam == nParNam+1) THEN

            ! Extend the array if necessary
            IF (SIZE(parNam) == nParNam) THEN
              ALLOCATE(parHlp(nParNam), stat=iac)
              CALL alcerr(iac,'parHlp',(/nParNam/),srName)
              parHlp(1:nParNam) = parNam(1:nParNam)

              DEALLOCATE(parNam,stat=iac)
              ALLOCATE(parNam(nParNam+maxStaSin), stat=iac)
              CALL alcerr(iac,'parNam',(/nParNam+maxStaSin/),srName)
              parNam(1:nParNam) = parHlp(1:nParNam)

              DEALLOCATE(parHlp,stat=iac)
            ENDIF

            nParNam = nParNam + 1

            parNam(iNam)%name = par%name
            parNam(iNam)%toBeDel = 0
            DO ii = 1,maxParTyp
              parNam(iNam)%parInt(ii)%t(1:2) = (/1d20,1d20/)
            ENDDO
            parNam(iNam)%parInt(par%locq(1))%t(1) = par%time%mean - par%time%half
            parNam(iNam)%parInt(par%locq(1))%t(2) = par%time%mean + par%time%half

          ! The name is already in the list
          ELSEIF (parNam(iNam)%name == par%name .AND. &
                  parNam(iNam)%parInt(par%locq(1))%t(1) == 1d20) THEN
            parNam(iNam)%parInt(par%locq(1))%t(1) = par%time%mean - par%time%half
            parNam(iNam)%parInt(par%locq(1))%t(2) = par%time%mean + par%time%half
            EXIT

          ! The name is already in the list
          ELSEIF (parNam(iNam)%name == par%name) THEN
            hlpInt%t(1) = par%time%mean - par%time%half
            hlpInt%t(2) = par%time%mean + par%time%half
            parNam(iNam)%parInt(par%locq(1)) = &
                         parNam(iNam)%parInt(par%locq(1)) + hlpInt
            EXIT
          ENDIF
        ENDDO

      ENDIF

! Orbit Parameters
! ----------------
      IF ( par%locq(1) == 3 ) parOrb%epoOrb(iFil) = par%time%mean
      IF ( par%locq(1) == 3 .AND. ohelp /= 1) THEN
        DO ii = 1,parOrb%nOrb+1
          IF (ii > parOrb%nOrb) EXIT
          IF (parOrb%seqorb(ii) == par%locq(4)) EXIT
        ENDDO
        IF (ii > parOrb%nOrb) THEN
          parOrb%nOrb = parOrb%nOrb + 1
          parOrb%seqorb(parOrb%nOrb) = par%locq(4)
        ENDIF
        IF (ohelp == 0 .AND. sat_old /= 0 .AND. sat_old /= par%locq(3)) &
           ohelp = 1
        sat_old = par%locq(3)
      ENDIF

! Stochastic orbit parameters
      IF ( par%locq(1) == 11 ) THEN
        DO ii = 1,opt%nStcep+1
          IF (ii > opt%nStcep ) EXIT
          IF (opt%rsw(ii) == par%locq(5)) EXIT
        ENDDO

        IF (ii > opt%nStcep) THEN
          opt%nStcep          = opt%nStcep+1
          opt%rsw(opt%nStcep) = par%locq(5)
          numStc              = numStc + 1

! Error if different sets of stochastic orbit parameters in NEQs
          IF (newstc == 0 .AND. error == 0) THEN
            IF ((opt%rsw(opt%nStcep)>3 .AND. opt%rsw(opt%nStcep-1)<4).OR.&
                 (opt%rsw(opt%nStcep)<4 .AND. opt%rsw(opt%nStcep-1)>3)) THEN
              WRITE(lfnerr,'(2(/,A))')                                  &
                   ' *** SR STANEQ: There are different sets of '   //  &
                   'stochastic orbit parameters in ',                   &
                   '                the normal equation files. '   //   &
                   'This is not supported by ADDNEQ2.'
              CALL exitrc(2)
            ELSE
              WRITE(lfnerr,'(4(/,A))')                                  &
                   ' ### SR STANEQ: There are different sets of '  //   &
                   'stochastic orbit parameters in ',                   &
                   '                the normal equation files. '   //   &
                   'This is not supported by ADDNEQ2',                  &
                   '                and could cause some problems' //   &
                   ' if new stochastic orbit ',                         &
                   '                parameters shall be added on ' //   &
                   'the NEQ boundaries. '
              error = 1
            ENDIF
          ENDIF
          newstc = 1

        ENDIF

      ENDIF

      IF (numstc > 0) THEN
        IF (numstc /= opt%nStcep .AND. error == 0) THEN
          WRITE(lfnerr,'(4(/,A))')                                        &
               ' ### SR STANEQ: There are different sets of stochastic' //&
               ' orbit parameters in ',                        &
               '                the normal equation files. This is not' //&
               ' supported by ADDNEQ2',                        &
               '                and could cause some problems if new'   //&
               ' stochastic orbit ',                           &
               '                parameters shall be added on the NEQ'   //&
               ' boundaries. '
          error = 1
        ENDIF
      ENDIF

! Differential code biases
! ------------------------
      IF ( par%locq(1) == 8 ) THEN
        CALL dimtst(1,2,2,srName,'for parDcb','DCB-parameter code',     &
             'Variable parDcb is defined in "ADDRDOPT" and "MENU_NEQ"',   &
             par%locq(2),SIZE(parDcb,1),irc)
        IF ( par%locq(2) == 1 ) THEN
          IF (par%locq(5) < 0) CYCLE
          CALL dimtst(1,2,2,srName,'for parDcb','DCB-parameter code',   &
               'Variable parDcb is defined in "ADDRDOPT" and "MENU_NEQ"', &
               par%locq(5),SIZE(parDcb,2),irc)
          parDcb(par%locq(2),par%locq(5)) = 1
        ELSE IF  ( par%locq(2) == 2 ) THEN
          IF (par%locq(6) < 0) CYCLE
          CALL dimtst(1,2,2,srName,'for parDcb','DCB-parameter code',   &
               'Variable parDcb is defined in "ADDRDOPT" and "MENU_NEQ"', &
               par%locq(6),SIZE(parDcb,2),irc)
          parDcb(par%locq(2),par%locq(6)) = 1
        ENDIF
      ENDIF

! Satellite antenna offset parameters
! -----------------------------------
      IF ( par%locq(1) == 12 ) THEN
        iGrp = par%locq(4)

        IF (iGrp == 0) THEN

          iGrp = listi4(1,maxOff,parSao(iFil)%gnroff(:), &
                          par%locq(5),parSao(iFil)%nanoff)
          parSao(iFil)%nsaoff(iGrp)    = 1
          parSao(iFil)%satoff(1,iGrp)  = par%locq(5)
        ELSE

          parSao(iFil)%nanoff          = neq%misc%nanoff
          parSao(iFil)%gnroff(iGrp)    = par%locq(5)
          parSao(iFil)%nsaoff(iGrp)    = neq%misc%nsaoff(iGrp)
          parSao(iFil)%satoff(:,iGrp)  = neq%misc%satoff(:,iGrp)
        ENDIF

        parSao(iFil)%santoff(iGrp,par%locq(3))= 1

        iReq                         = par%locq(2)
        parSao(iFil)%nRqoff          = iReq
        parSao(iFil)%grpoff(iReq)    = iGrp
        parSao(iFil)%timoff(1,iReq)  = par%time%mean-par%time%half
        parSao(iFil)%timoff(2,iReq)  = par%time%mean+par%time%half

      ENDIF

! Satellite antenna patterns
! --------------------------
      IF ( par%locq(1) == 25 ) THEN
        iGrp = par%locq(2)

        IF (par%locq(2) == 0 ) THEN

          iGrp = listi4(1,maxOff,parSap(iFil)%gnrspv(:), &
                          par%locq(3),parSap(iFil)%nanspv)
          parSap(iFil)%nsaspv(iGrp)    = 1
          parSap(iFil)%satspv(1,iGrp)  = par%locq(3)
        ELSE

          parSap(iFil)%nanspv          = neq%misc%nanoff
          parSap(iFil)%gnrspv(iGrp)    = par%locq(3)
          parSap(iFil)%nsaspv(iGrp)    = neq%misc%nsaoff(iGrp)
          parSap(iFil)%satspv(:,iGrp)  = neq%misc%satoff(:,iGrp)
        ENDIF

        parSap(iFil)%nptspv(1,iGrp)  = par%locq(6)
        parSap(iFil)%nptspv(2,iGrp)  = par%locq(7)
        parSap(iFil)%timspv(1,iGrp)  = par%time%mean-par%time%half
        parSap(iFil)%timspv(2,iGrp)  = par%time%mean+par%time%half

      ENDIF

! Receiver antenna offsets
! ------------------------
      IF ( par%locq(1) ==  5 ) THEN
        parRao%nRao = parRao%nRao + 1
        parRao%rao( par%locq(4) / 100 ) = 1
      ENDIF

! Receiver antenna pattern
! ------------------------
      IF ( par%locq(1) == 18 ) THEN
        IF ( par%locq(6) > 0 ) parRap%nRapGrd = parRap%nRapGrd + 1
        IF ( par%locq(6) < 0 ) parRap%nRapShm = parRap%nRapShm + 1
      ENDIF

! Earth rotation parameters
! -------------------------
      IF ( par%locq(1) == 10 ) THEN
        DO ii = 1,parErp%nErp+1
          IF (ii > parErp%nErp) EXIT
          IF (parErp%erp(ii) == par%locq(4)) EXIT
        ENDDO
        IF (ii > parErp%nErp) THEN
          parErp%nErp = parErp%nErp + 1
          parErp%erp(parErp%nErp) = par%locq(4)
        ENDIF
      ENDIF

! Geocenter coordinates
! ---------------------
      IF ( par%locq(1) == 16 ) THEN
        DO ii = 1,parGcc%nGcc+1
          IF (ii > parGcc%nGcc) EXIT
          IF (parGcc%gcc(ii) == par%locq(2)) EXIT
        ENDDO
        IF (ii > parGcc%nGcc) THEN
          parGcc%nGcc             = parGcc%nGcc + 1
          parGcc%gcc(parGcc%nGcc) = par%locq(2)
        ENDIF
      ENDIF

! Troposphere parameter
! ---------------------
      IF ( par%locq(1) == 6 ) THEN
        parAtm%nTro    = parAtm%nTro + 1

        newModel = .FALSE.
        newModel = ( parAtm%nModel == 0 );
        IF ( .NOT. newModel ) newModel = &
             ( neq%misc%iTropo /= undef_Trp .AND. &
               parAtm%trpMod(parAtm%nModel)%iTropo /= neq%misc%iTropo ) .OR.&
             ( neq%misc%iExtra /= undef_Trp .AND. &
               parAtm%trpMod(parAtm%nModel)%iExtra /= neq%misc%iExtra ) .OR.&
             ( neq%misc%iTrMap /= undef_Trp .AND. &
               parAtm%trpMod(parAtm%nModel)%iTrMap /= neq%misc%iTrMap ) .OR.&
             ( neq%misc%iTrGrd /= undef_Trp .AND. &
               parAtm%trpMod(parAtm%nModel)%iTrGrd /= neq%misc%iTrGrd )
        IF ( newModel ) THEN
          parAtm%nModel = parAtm%nModel + 1

          parAtm%trpMod(parAtm%nModel)%iTropo = neq%misc%iTropo
          parAtm%trpMod(parAtm%nModel)%iExtra = neq%misc%iExtra
          parAtm%trpMod(parAtm%nModel)%iTrMap = neq%misc%iTrMap
          parAtm%trpMod(parAtm%nModel)%iTrGrd = neq%misc%iTrGrd
          parAtm%trpMod(parAtm%nModel)%fromto = (/iFil,iFil/)
        ELSEIF ( parAtm%trpMod(parAtm%nModel)%iTropo == neq%misc%iTropo .AND. &
                 parAtm%trpMod(parAtm%nModel)%iExtra == neq%misc%iExtra .AND. &
                 parAtm%trpMod(parAtm%nModel)%iTrMap == neq%misc%iTrMap .AND. &
                 parAtm%trpMod(parAtm%nModel)%iTrGrd == neq%misc%iTrGrd .AND. &
                 neq%misc%iTropo /= undef_Trp .AND.     &
                 neq%misc%iExtra /= undef_Trp .AND.     &
                 neq%misc%iTrMap /= undef_Trp .AND.     &
                 neq%misc%iTrGrd /= undef_Trp ) THEN
          parAtm%trpMod(parAtm%nModel)%fromto(2) = iFil
        ENDIF
      ENDIF

! Ionosphere parameter
! --------------------
      IF (par%locq(1) == 19) THEN
        parAtm%nIon = parAtm%nIon + 1
      ENDIF

! Range bias parameter
! --------------------
      IF (par%locq(1) == 26) THEN
        parRgb%nRgb = parRgb%nRgb + 1
      ENDIF

! GNSS-specific parameters
! ------------------------

! Station translations
      IF (par%locq(1) == 30 .AND. par%locq(3) < 4) THEN
        parGsp%nTra = parGsp%nTra+1
        iDmy = listi4(1,maxSys,parGsp%sysTra,par%locq(4),parGsp%nSysTra)
      ENDIF

! Troposphere baises
      IF (par%locq(1) == 30 .AND. par%locq(3) == 4) THEN
        parGsp%nTrp = parGsp%nTrp+1
        iDmy = listi4(1,maxSys,parGsp%sysTrp,par%locq(4),parGsp%nSysTrp)
      ENDIF


      IF (timFil(1,iFil) > par%time%mean - par%time%half) &
           timFil(1,iFil) = par%time%mean - par%time%half
      IF (timFil(2,iFil) < par%time%mean + par%time%half) &
           timFil(2,iFil) = par%time%mean + par%time%half

! Receiver clock offset/bias
! --------------------------
      IF (par%locq(1) == 2) THEN
        IF (par%locq(6) == 0)      parRco%isSta = parRco%isSta+1
        IF (abs(par%locq(6)) == 1) parRco%isFrq = parRco%isFrq+1
        IF (abs(par%locq(6)) == 2) parRco%isSat = parRco%isSat+1
      ENDIF

! Scaling factors for Vienna grid files
! -------------------------------------
      IF (par%locq(1) == 22) THEN
        iGrd = par%locq(2)
        IF (iGrd > SIZE(parGrd)) THEN
          WRITE(lfnerr,'(/,A,2(/,16X,A,I6),2(/,A),/)')                     &
          ' *** SR STANEQ: Too many grid types in the NEQ file.',          &
                          'Grid type in NEQ file:    ',iGrd,               &
                          'Size of the parGrd array: ',SIZE(parGrd),       &
                          'Size of parGrd is defined in the subroutines ', &
                          '"${LG}/MENU_NEQ.f90" and "${LG}/ADDRDOPT.f90".'
          CALL exitrc(2)
        ENDIF
        IF (par%locq(3) == 0 .AND. parGrd(iGrd)%nSta /= 0) &
          parGrd(iGrd)%nSta = 0
        IF (par%locq(3) > 0 .AND. parGrd(iGrd)%nSta == -1) &
          parGrd(iGrd)%nSta = 1
        IF (par%locq(5) > parGrd(iGrd)%nPar) &
          parGrd(iGrd)%nPar = par%locq(5)
        parGrd(iGrd)%keyw = neq%misc%grdNeq(iGrd)
      ENDIF

! HOI scaling factors
! -------------------
      IF (par%locq(1) == 27) THEN
        parAtm%nHoi = parAtm%nHoi + 1
        opt%hoi(par%locq(2))%stack = opt%hoi(par%locq(2))%stack .OR. &
        par%locq(3) == 2
      ENDIF

    ENDDO
    CLOSE(lfnneq)

! Store the Names of STD and RPR Files
! ------------------------------------
    orbFil(1,iFil) = neq%misc%orbfil(1)(1:LEN_TRIM(neq%misc%orbfil(1)))
    orbFil(2,iFil) = neq%misc%orbfil(2)(1:LEN_TRIM(neq%misc%orbfil(2)))
    orbFil(3,iFil) = ''
    orbFil(4,iFil) = ''

    nobs(iFil)   = neq%misc%nobs
    npar(iFil)   = neq%misc%npar
    nparms(iFil) = neq%misc%nparms

    parNum(maxParTyp + 1,iFil) = npar(iFil)
    parNum(maxParTyp + 2,iFil) = nparms(iFil) - npar(iFil)
    parNum(maxParTyp + 3,iFil) = nparms(iFil)

    nobsTot = nobsTot + neq%misc%nobs

    IF (timFil(1,iFil) < timObs(1)) timObs(1) = timFil(1,iFil)
    IF (timFil(2,iFil) > timObs(2)) timObs(2) = timFil(2,iFil)
    CALL TIMST2(1,2,timObs,tstrgt)

    newstc = 0

  ENDDO

! Initialize opt%timRefCrd
! ------------------------
  timObs(1) = nint(timObs(1)*288.d0)/288.d0
  timObs(2) = nint(timObs(2)*288.d0)/288.d0
  opt%timRefCrd = (timObs(1)+timObs(2))/2.D0

! Order NEQ Files
! ---------------
  IF (opt%chrono == 1.AND.nFil > 1) THEN
    CALL DORDUP(timFil(1,1:nFil),nFil,indx)
  ELSE
    DO iFil=1,nFil
      indx(iFil)=iFil
    ENDDO
  ENDIF

  DO iFil = 1, nFil
    filNam(1,iFil) = neqFileName(indx(iFil))
    ind1(iFil) = indx(iFil)
    ind2(indx(iFil)) = iFil
  END DO

  DO iFil = 1, nFil
    neqFileName(iFil) = filNam(1,iFil)
    jFil=ind1(iFil)
    lFil=ind2(iFil)

    DO ii=1,4
      hlpStr=orbFil(ii,iFil)
      orbFil(ii,iFil)=orbFil(ii,jFil)
      orbFil(ii,jFil)=hlpStr
    ENDDO
    hlpSao=parSao(iFil)
    parSao(iFil)=parSao(jFil)
    parSao(jFil)=hlpSao

    hlpSap=parSap(iFil)
    parSap(iFil)=parSap(jFil)
    parSap(jFil)=hlpSap

    ind1(iFil)=ind1(lFil)
    ind1(lFil)=jFil
    ind2(iFil)=ind2(jFil)
    ind2(jFil)=lFil
  ENDDO

  IF(iPrt == 1) THEN
    CALL prfile('INPFILE','INPUT NORMAL EQUATION FILES',1,131,nFil,filNam)
  ENDIF

! Write main characteristics of neq files into protocol
! -----------------------------------------------------
! title
  IF (nFil >= 1 .AND. iPrt == 1) THEN
    WRITE(lfnprt,'(2(A,/))')                              &
      ' Main characteristics of normal equation files:',  &
      ' ---------------------------------------------'
    WRITE(lfnprt,'(A,/,A)')                                              &
      ' File  From                 To                   '             // &
      'Number of observations / parameters / degree of freedom',         &
      ' -------------------------------------------------------------'// &
      '--------------------------------------------------------------'// &
      '--------'

! per neq
    DO jFil = 1, nFil
      iFil = indx(jFil)
      CALL TIMST2(1,2,timFil(1:2,iFil),tstrg)

!      IF (iPrt == 1) THEN   obsolete
        IF (nobsTot < 999999999.D0) THEN
          WRITE(lfnprt,'(1X,I4,2X,A40,10X,3(I13))')           &
          jFil, tStrg, NINT(nobs(iFil)), NINT(nparms(iFil)), NINT(nobs(iFil)-nparms(iFil))
        ELSE
          WRITE(lfnprt,'(1X,I4,2X,A40,10X,3(ES13.4))')        &
          jFil, tStrg, nobs(iFil), nparms(iFil), nobs(iFil)-nparms(iFil)
        ENDIF
!      ENDIF
    ENDDO
  ENDIF

! sum of observations
  IF (nFil > 1 .AND. iPrt == 1) THEN
    WRITE(lfnprt,'(A)')                                                     &
      ' ----------------------------------------------------------------'// &
      '-------------------------------------------------------------------'
    IF (nobsTot < 999999999.D0) THEN
      WRITE(lfnprt,'(1X,A,1X,A40,10X,I13)') 'Total', tStrgt,NINT(nobsTot)
    ELSE
      WRITE(lfnprt,'(1X,A,1X,A40,10X,ES13.4)') 'Total', tStrgt,nobsTot
    ENDIF
  ENDIF

! Write list of parameters
! ------------------------
  IF (iPrt == 1) THEN

! Print title line
    WRITE(lfnprt,'(//,2(A,/),/,A)',ADVANCE='NO') &
      ' Number of parameters:',     &
      ' --------------------',      &
      ' Parameter type'

    numLoop = nFil/10.D0       ! number of necessary columns in output file
    nloop   = CEILING(numloop) ! (10 records per column possible)

    DO iLoop = 1, nLoop

      IF (nLoop == 1) THEN
        iStart = 1
        iEnd   = nFil
      ELSE IF (iLoop < nLoop) THEN
        iStart = 1  + (iLoop-1) * 10
        iEnd   = 10 + (iLoop-1) * 10
      ELSE IF (iLoop == nLoop) THEN
        iStart = 1  + (iLoop-1) * 10
        iEnd   = nFil
      ENDIF

      DO jFil = iStart, iEnd
        IF (jFil == iStart) THEN
          IF (iLoop == 1) THEN
            WRITE(lfnprt,'(24X,I9)',ADVANCE='NO') jFil
          ELSE
            WRITE(lfnprt,'(/,39X,I9)',ADVANCE='NO') jFil
          ENDIF
        ELSE
          WRITE(lfnprt,'(I9)',ADVANCE='NO') jFil
        ENDIF
      ENDDO

      IF (iLoop == nLoop) THEN
        WRITE(lfnprt,'(/,1X,131("-"))',ADVANCE='NO')
      ENDIF

    ENDDO

! Print number of parameters for each parameter type and file ...
    DO iParTyp = 1, maxParTyp+3 ! loop over all parameter types
      printPar = .FALSE.

! ... if number of parameters per parameter type > 0
      DO iFil = 1, nFil
        IF (parNum(iParTyp,iFil) > 0) printPar = .TRUE.
      ENDDO

      IF (printPar) THEN

        IF (iParTyp == maxParTyp+1 .OR. iParTyp == maxParTyp+3 ) &
          WRITE(lfnprt,'(/,1X,131("-"))',ADVANCE='NO')

        WRITE(lfnprt,'(/,A)',ADVANCE='NO') parStrg(iParTyp)

        DO iLoop = 1, nLoop ! loop over all columns
          IF (nLoop == 1) THEN
            iStart = 1
            iEnd   = nFil
          ELSE IF (iLoop < nLoop) THEN
            iStart = 1  + (iLoop-1) * 10
            iEnd   = 10 + (iLoop-1) * 10
          ELSE IF (iLoop == nLoop) THEN
            iStart = 1  + (iLoop-1) * 10
            iEnd   = nFil
          ENDIF

          DO jFil = iStart, iEnd
            iFil = indx(jFil)
            IF (jFil == iStart) THEN
              IF (iLoop == 1) THEN
                IF (nobsTot < 999999999.D0 .OR. iParTyp <= maxParTyp) THEN
                  WRITE(lfnprt,'(2X,I9)',ADVANCE='NO') NINT(parNum(iParTyp,iFil))
                ELSE
                  WRITE(lfnprt,'(2X,ES9.2)',ADVANCE='NO') parNum(iParTyp,iFil)
                ENDIF
              ELSE
                IF (nobsTot < 999999999.D0 .OR. iParTyp <= maxParTyp) THEN
                  WRITE(lfnprt,'(/,39X,I9)',ADVANCE='NO') NINT(parNum(iParTyp,iFil))
                ELSE
                  WRITE(lfnprt,'(/,39X,ES9.2)',ADVANCE='NO') parNum(iParTyp,iFil)
                ENDIF
              ENDIF
            ELSE
              IF (nobsTot < 999999999.D0 .OR. iParTyp <= maxParTyp) THEN
                WRITE(lfnprt,'(I9)',ADVANCE='NO') NINT(parNum(iParTyp,iFil))
              ELSE
                WRITE(lfnprt,'(ES9.2)',ADVANCE='NO') parNum(iParTyp,iFil)
              ENDIF
            ENDIF
          ENDDO

!!          IF (iLoop < nLoop) WRITE(lfnprt,'(/,39X)',ADVANCE='NO')
        ENDDO

      ENDIF

    ENDDO

  ENDIF

! Reduce staInfo structure
! ------------------------
  IF(iPrt == 1) CALL redcrx(limits,parNam(:)%name(1:staNameLength), &
                            renamList,delList)

! Check all station information records
! -------------------------------------
  DO iInfo=1,staInfo%nInfo

    ! Check time interval of the record
    IF ( (staInfo%staInfo(iInfo)%timint%t(2) < timobs(1) .OR. &
          staInfo%staInfo(iInfo)%timint%t(1) > timobs(2)) ) CYCLE

    ! Check the station name
    DO iNam = 1,nParNam

      IF (parNam(iNam)%toBeDel /= 0) CYCLE
      IF (staInfo%staInfo(iInfo)%stanam /= &
                                    parNam(iNam)%name(1:staNameLength)) CYCLE

      ! The station has been observed during the interval
      DO iTyp = 1,maxParTyp
        IF (parNam(iNam)%parInt(iTyp)%t(1) == 1d20) CYCLE

        IF ( (staInfo%staInfo(iInfo)%timint%t(2) < &
                                     parNam(iNam)%parInt(iTyp)%t(1) .OR. &
              staInfo%staInfo(iInfo)%timint%t(1) > &
                                     parNam(iNam)%parInt(iTyp)%t(2)) ) CYCLE

        ! Is there another station information record for this station
        DO jInfo = 1,staInfo%nInfo
          IF (iInfo == jInfo) CYCLE
          IF (staInfo%staInfo(iInfo)%stanam /= &
                                        staInfo%staInfo(jInfo)%stanam) CYCLE

          IF ( (staInfo%staInfo(jInfo)%timint%t(2) < &
                                     parNam(iNam)%parInt(iTyp)%t(1) .OR. &
                staInfo%staInfo(jInfo)%timint%t(1) > &
                                     parNam(iNam)%parInt(iTyp)%t(2)) ) CYCLE

          IF ((opt%chgRecNam == 1 .AND. &
               staInfo%staInfo(iInfo)%recNam /= undef_c .AND. &
               staInfo%staInfo(jInfo)%recNam /= undef_c .AND. &
               staInfo%staInfo(iInfo)%recNam /= &
                              staInfo%staInfo(jInfo)%recNam   ) .OR. &

              (opt%chgAntNam == 1 .AND. &
               staInfo%staInfo(iInfo)%antNam /= undef_c .AND. &
               staInfo%staInfo(jInfo)%antNam /= undef_c .AND. &
               staInfo%staInfo(iInfo)%antNam /= &
                              staInfo%staInfo(jInfo)%antNam   ) .OR. &

              (opt%chgRecNum == 1 .AND. &
               staInfo%staInfo(iInfo)%recNum /= undef_i .AND. &
               staInfo%staInfo(jInfo)%recNum /= undef_i .AND. &
               staInfo%staInfo(iInfo)%recNum /= &
                              staInfo%staInfo(jInfo)%recNum   ) .OR. &

              (opt%chgAntNum == 1 .AND. &
               staInfo%staInfo(iInfo)%antNum /= undef_i .AND. &
               staInfo%staInfo(jInfo)%antNum /= undef_i .AND. &
               staInfo%staInfo(iInfo)%antNum /= &
                              staInfo%staInfo(jInfo)%antNum   ) .OR. &

              (opt%chgAntEcc == 1 .AND. &
               staInfo%staInfo(iInfo)%antEcc(1) /= undef_e .AND. &
               staInfo%staInfo(jInfo)%antEcc(1) /= undef_e .AND. &
               staInfo%staInfo(iInfo)%antEcc(1) /= &
                              staInfo%staInfo(jInfo)%antEcc(1)) .OR. &

              (opt%chgAntEcc == 1 .AND. &
               staInfo%staInfo(iInfo)%antEcc(2) /= undef_e .AND. &
               staInfo%staInfo(jInfo)%antEcc(2) /= undef_e .AND. &
               staInfo%staInfo(iInfo)%antEcc(2) /= &
                              staInfo%staInfo(jInfo)%antEcc(2)) .OR. &

              (opt%chgAntEcc == 1 .AND. &
               staInfo%staInfo(iInfo)%antEcc(3) /= undef_e .AND. &
               staInfo%staInfo(jInfo)%antEcc(3) /= undef_e .AND. &
               staInfo%staInfo(iInfo)%antEcc(3) /= &
                              staInfo%staInfo(jInfo)%antEcc(3))) THEN

            parNam(iNam)%toBeDel = 1

          ENDIF
        ENDDO
      ENDDO
    ENDDO
  ENDDO

! Extent the list of station problems
! -----------------------------------
  nProb = 0
  DO iNam = 1,nParNam
    IF (parNam(iNam)%toBeDel /= 0) nProb = nProb + 1
  ENDDO

  IF (nProb > 0) THEN
    ALLOCATE(hlpProb(staInfo%nProb),stat=iac)
    CALL alcerr(iac,'hlpProb',(/staInfo%nProb/),srName)
    hlpProb(1:staInfo%nProb) = staInfo%staProb(1:staInfo%nProb)
    DEALLOCATE(staInfo%staProb,stat=iac)

    ALLOCATE(staInfo%staProb(staInfo%nProb+nProb),stat=iac)
    CALL alcerr(iac,'staInfo%staProb',(/staInfo%nProb+nProb/),srName)
    staInfo%staProb(1:staInfo%nProb) = hlpProb(1:staInfo%nProb)
    DEALLOCATE(hlpProb,stat=iac)

    staInfo%staProb(1:staInfo%nProb)%remark = 'station problem '

    DO iNam = 1,nParNam
      IF (parNam(iNam)%toBeDel /= 0) THEN
        staInfo%nProb = staInfo%nProb + 1
        staInfo%staProb(staInfo%nProb)%stanam      = parNam(iNam)%name(1:staNameLength)
        staInfo%staProb(staInfo%nProb)%flg         = ''
        staInfo%staProb(staInfo%nProb)%timint%t(1) = 0d0
        staInfo%staProb(staInfo%nProb)%timint%t(2) = 1d20
        staInfo%staProb(staInfo%nProb)%remark      = 'equipment change'
      ENDIF
    ENDDO
  ENDIF

! Sort the parameter names according to the parameter types
! ---------------------------------------------------------
  DO iTyp = 1,maxParTyp
    nSta = 0
    DO iNam = 1,nParNam
      IF (parNam(iNam)%toBeDel /= 0) CYCLE
      IF (parNam(iNam)%parInt(iTyp)%t(1) /= 1d20) nSta = nSta + 1
    ENDDO

    ALLOCATE(namList(iTyp)%nam2(nSta),stat=iac)
    CALL alcerr(iac,'namList(iTyp)%nam2',(/nSta/),srName)
    ALLOCATE(namList(iTyp)%nam(nSta),stat=iac)
    CALL alcerr(iac,'namList(iTyp)%nam',(/nSta/),srName)
    ALLOCATE(namList(iTyp)%num(nSta),stat=iac)
    CALL alcerr(iac,'namList(iTyp)%num',(/nSta/),srName)

    nSta = 0
    DO iNam = 1,nParNam
      IF (parNam(iNam)%toBeDel /= 0) CYCLE
      IF (parNam(iNam)%parInt(iTyp)%t(1) /= 1d20) THEN
        nSta = nSta + 1
        namList(iTyp)%nam2(nSta) = parNam(iNam)%name
        namList(iTyp)%num(nSta)  = nSta
      ENDIF
    ENDDO

    namList(iTyp)%nSta = nSta

    ! Sort the names in the list
    sorted = .FALSE.
    DO WHILE(.NOT. sorted)
      sorted = .TRUE.
      DO iNam = 1,namList(iTyp)%nSta-1
        IF (namList(iTyp)%nam2(iNam) > namList(iTyp)%nam2(iNam+1)) THEN
          sorted = .FALSE.
          par%name                   = namList(iTyp)%nam2(iNam)
          namList(iTyp)%nam2(iNam)   = namList(iTyp)%nam2(iNam+1)
          namList(iTyp)%nam2(iNam+1) = par%name
        ENDIF
      ENDDO
    ENDDO

    DO iNam = 1,namList(iTyp)%nSta
      namList(iTyp)%nam(iNam) = namList(iTyp)%nam2(iNam)(1:staNameLength)
    ENDDO

  ENDDO

  nClk = MAX(namList(23)%nSta,namList(24)%nSta)
  ALLOCATE(clkList(nClk,2),stat=iac)
  CALL alcerr(iac,'clkList',(/nClk,2/),srName)

  clkList = ' '

  DO iClk = 1, namList(23)%nSta
    clkList(iClk,1) = namList(23)%nam(iClk)(1:staNameLength)
  ENDDO

  DO iClk = 1, namList(24)%nSta
    clkList(iClk,2) = namList(24)%nam(iClk)
  ENDDO

  DEALLOCATE(filNam,indx,ind1,ind2,stat=iac)
  DEALLOCATE(timFil,nobs,npar,nparms,stat=iac)
  DEALLOCATE(parNum,stat=iac)
  DEALLOCATE(parNam,stat=iac)

  RETURN

END SUBROUTINE staneq

END MODULE
