MODULE s_S2N0IN
CONTAINS

! --------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! --------------------------------------------------------------------------

SUBROUTINE s2n0in (nfiles, files, optrot, flg_neq, optcrd, optfix, berneq, &
                   sigma0, noneq, step2, step2t, crdset, Tcrdset)

! --------------------------------------------------------------------------
! Purpose   : Read option input file for program SNX2NQ0
!
! Author    : L. Mervart
!
! Created   : 08-Oct-2001
! Last mod.:  22-Aug-2011
!
! Changes   : 13-Oct-2001  HU: Modifications
!             03-Nov-2001  HU: Parameter optrot added
!             21-Dec-2001  HU: Use m_bern
!             21-Dec-2001  HU: m_addneq replaced by p_addneq
!             28-Aug-2002  DT: Parameter flg_neq added
!             29-Aug-2002  DT: Call of SR ckoptc
!             26-Sep-2002  DT: Set parameter nflcol=3 (for coordinate file)
!             27-Sep-2002  DT: Check if a CRD file should be saved:
!                              additional output argument 'optcrd'
!             22-Nov-2002  DT: Set parameter nflcol=4 (for velocity file)
!             23-Apr-2003  RD: Nullify local pointers
!             10-Mar-2004  CU: Reconstruct original NEQ info - on request
!             20-Aug-2007  AG: Noneq option implemented
!             06-May-2009  HU/RD: Read step2 options
!             15-Oct-2010  DT: Options for selecting set of coord in CRD/VEL
!             20-Oct-2010  DT: Write FIX file (optfix added to SR call)
!             11-Aug-2011  RD: Empty CRDEPOCH also means last epoch
!             22-Aug-2011  HB: Set model key for nutation model (UT1RED)
!             29-Oct-2012  SS: Generalized optfix
!
! SR called : gtfile2, exitrc, readkeys, ckoptb, ckoptc
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
!---------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_nutmod, ONLY: t_nutat,init_nutat
  USE d_model,  ONLY: setModKey, chrValLength, mod_orb_nutmod

  USE s_gtfile2
  USE s_ckoptr
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckoptd
  USE f_djul
  USE s_gtflna
  USE s_rdnutm
  IMPLICIT NONE

! List of Parameters
! ------------------
! Output:
  INTEGER(i4b)                      :: nfiles
  CHARACTER(LEN=fileNameLength), &
           DIMENSION(:,:), POINTER  :: files
  INTEGER(i4b)                      :: optrot   ! 1: Additional rotation
  INTEGER(i4b)                      :: flg_neq  ! Normal equation from:
                                                ! 1: COVA
                                                ! 2: NEQ
  INTEGER(i4b)                      :: optcrd   ! 1: Save station coordinates
                                                !     and station velocities
  INTEGER(i4b)                      :: optfix   ! 1: Yes, saved FIX file
                                                !    (ignored constraint code 2)
                                                ! 0: No
                                                ! 2: All (all constraint codes)
                                                ! 3: Fixed (0 only)
                                                ! 4: Constrained (1 only)
                                                ! 5: Unconstrained (2 only)
  INTEGER(i4b)                      :: noneq    ! Do not save NEQ
  INTEGER(i4b)                      :: berneq   ! 1: Reconstruct original NEQ
                                                !    information
  REAL(r8b)                         :: sigma0   ! A priori sigma of unit weight
  INTEGER(i4b)                      :: step2    ! 1: Correct step2 bug
  REAL(r8b),DIMENSION(2)            :: step2t   ! Start/end epoch (mjd, incl)
  INTEGER(i4b)                      :: crdset   ! Set of coord in CRD/VEL:
                                                !  1: latest set
                                                !  2: set valid at epoch Tcrdset
  REAL(r8b)                         :: Tcrdset  ! Validity epoch to extract coord.

! Local Types
! -----------
  TYPE(t_nutat)                     :: nutat    ! Nutation model parameters

! Local Parameters
! ----------------
  INTEGER(i4b)                      :: nflcol   ! # of columns
  INTEGER(i4b)                      :: irCode=0 ! Return code
  INTEGER(i4b)                      :: irc      ! Return code
  INTEGER(i4b)                      :: ios      ! Return code
  INTEGER(i4b)                      :: year, month
  REAL(r8b)                         :: day, numVal

  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER   :: keyValue
  CHARACTER(LEN=fileNameLength)                          :: nutFil
  CHARACTER(LEN=chrValLength)                            :: chrVal
  CHARACTER(LEN=8),PARAMETER                             :: srNam= 's2n0in'

! Init variables
! --------------
  NULLIFY(keyValue)
  sigma0  = 0d0
  crdset  = 0
  Tcrdset = 0d0

! Get Input/Output Files
! ----------------------
  nflcol=5
  CALL gtfile2('INPFIL',nflcol,nfiles,files)

  IF (nfiles==0) THEN
    WRITE (lfnerr, '(/,A,/)') ' *** SR S2N0IN: No input files selected '
    CALL exitrc (2)
  ENDIF

! Save station coodinates
! -----------------------
  CALL ckoptb(1,(/'CRDFIL'/), 'sr s2n0in (pg snx2nq0)', &
              'Save station coordinates', ircode, result1=optcrd)
  IF (irCode==1) CALL exitrc(2)

  IF ( optcrd == 1) THEN

    CALL readkeys('RADIO_1', keyValue, ircode)
    IF (ircode == 0 .AND. keyValue(1) == '1') crdset = 1
    CALL readkeys('RADIO_2', keyValue, ircode)
    IF (ircode == 0 .AND. keyValue(1) == '1') crdset = 2

    IF ( crdset == 2 ) THEN
      CALL readkeys('CRDEPOCH', keyValue, ircode)
      IF (ircode == 0) READ(keyValue(1), *, iostat=ios) year, month, day

      IF ( year < 0    .OR. month < 0   .OR. month > 12 .OR. &
           day < 0.d0 .OR. day > 31.d0 .OR.                  &
           ircode /= 0   .OR. ios /= 0                      ) THEN
        IF ( LEN_TRIM(keyValue(1)) > 0 ) THEN
          WRITE(lfnerr,'(/,A,/,17X,A,A,/)')                        &
          ' *** SR S2N0IN: Wrong entry for epoch detected',        &
                          'Specified value:  ', TRIM(keyValue(1))
          CALL exitrc(2)
        ELSE
          crdset = 1
        ENDIF

      ELSE
        Tcrdset = djul(year, month, day)
      ENDIF

    ENDIF

  ENDIF

! Saved FIX file
! --------------
  IF (optcrd == 0) THEN
    optfix = 0
  ELSE
    CALL readKeys('FIXFIL',keyValue,irc)
    CALL ckoptc(1,'FIXFIL',keyValue, &
                (/'YES          ','NO           ','ALL          ','FIXED        ','CONSTRAINED  ','UNCONSTRAINED'/), &
                'SR S2N0IN (PG SNX2NQ0)','Saved FIX file', irc, irCode, &
                 maxVal=1,valList=(/1,0,2,3,4,5/),result1=optfix)
  ENDIF

! Do not save NEQ
! ---------------
  CALL ckoptb(1,(/'NONEQ'/), 'sr s2n0in (pg snx2nq0)', &
              'Do not save NEQ', ircode, result1=noneq)
  IF (irCode==1) CALL exitrc(2)

! Reconstruct original NEQ information from Bernese SINEX data
! ------------------------------------------------------------
  IF (noneq == 1) THEN
    berneq = 0

  ELSE
    CALL ckoptb(1,(/'BERNEQ'/),'sr s2n0in (pg snx2nq0)', &
                'Reconstruct original NEQ info',irCode,result1=berneq)
    IF (irCode==1) CALL exitrc(2)
  ENDIF

! A priori sigma of unit weight
! -----------------------------
  IF (berneq == 0) THEN
    CALL readkeys('SIGMA0',keyValue,irc)
    CALL ckoptr(1,'SIGMA0', keyValue,'sr s2n0in (pg snx2nq0)', &
                'A priori sigma of unit weight', irc, irCode,  &
                maxVal=1,result1=sigma0)
    IF (irCode==1) CALL exitrc(2)
  ENDIF

! Additional rotation
! -------------------
  IF (noneq == 1) THEN
    optrot = 0

  ELSE
    CALL ckoptb(1,(/'ADDROT'/),'sr s2n0in (pg snx2nq0)', &
                'Additional rotation', irCode,result1=optrot)
    IF (irCode==1) CALL exitrc(2)
  ENDIF

! Generation of normal equation system (neq-structure)
! ----------------------------------------------------
  CALL readKeys('FLG_NEQ', keyValue, irc)

  CALL ckoptc( 1, 'FLG_NEQ', keyValue, (/ 'COVA', 'NEQ ' /),            &
               'SR s2n0in (PG snx2Nq0)',                                &
               'Normal equation system from COVA or NEQ', irc, irCode,  &
               maxVal=1, valList=(/1,2/), result1=flg_neq)

! Correct step2 tide bug
! ----------------------
  CALL readkeys('STEP2',keyValue,irc)
  CALL ckoptb(1, (/'STEP2'/), 'sr s2n0in (pg snx2Nq0)',                 &
              'Correct step2 tide bug', irCode,                         &
              result1=step2)

  CALL readkeys('STEP2A',keyValue,irc)
  CALL ckoptd(1,'STEP2A',keyValue,'sr s2n0in (pg snx2Nq0)',             &
              'Correct step2 tide bug, start mjd', irc, irCode,         &
              empty=0D0,maxVal=1,result1=step2t(1))

  CALL readkeys('STEP2B',keyValue,irc)
  CALL ckoptd(1,'STEP2B',keyValue,'sr s2n0in (pg snx2Nq0)',             &
              'Correct step2 tide bug, end mjd', irc, irCode,           &
              empty=1D20,maxVal=1,result1=step2t(2))

  IF (step2 == 1) THEN
    WRITE(lfnprt,"(' ### SR S2N0IN: Step 2 correction', &
                                 & ' from ',F8.1,' to ',F8.1,/)") step2t(1:2)
  ENDIF

! Read Nutation Model
! -------------------
  CALL gtflna(1,'NUTMOD',nutfil,irc)
  CALL init_nutat(nutat)
  CALL rdnutm(nutfil,nutat)
  chrVal = ''
  chrVal = nutat%nutnam
  numVal=0.D0
  CALL setModKey(mod_orb_nutMod,chrVal,srNam,numVal)

! Deallocate local variables
! --------------------------
  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE s2n0in

END MODULE

