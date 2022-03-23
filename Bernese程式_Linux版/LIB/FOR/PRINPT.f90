MODULE s_PRINPT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE prinpt(iauto , isetop, nfrchk, minlen, isave , iiono,  iusflg, &
                  mrk1o2, maxzen, secIpl, mxhole, mncont, iprnt1, mxintr, &
                  ipproc, qq    , disclv, ltrip , sigwgs, stafix, iprnt2, &
                  sigl12, iwlscr, swidth, mincyc, irject, mxogap, mxiond, &
                  iamnew, ntonly, l5clea, omcmax, mxambs, kinsta, mxzleo, &
                  itropo, iextra, iZerod, iLeos,  rmsmax, jmpopt, toljmp, &
                  nFil  , filist)

! -------------------------------------------------------------------------
! Purpose:    This is a new version of the old subroutine PRINPT.f that
!             reads the input options of the program MAUPRP
!
! Author:     L. Mervart
!
! Created:    03-Jun-2001
!
! Changes:    17-Sep-2001 DI: Add option mxambs
!             18-Sep-2001 DI: Checks for reading the input options added
!             04-Jan-2001 DS: Kinematic coordinate estimation
!             05-Jan-2002 DS: LEO max. zenith angle
!             05-Jan-2002 DS: Itropo and iextra added
!             12-Jan-2002 DS: Mxzleo check disabled
!             16-Jan-2002 MR: Mincyc=-1
!             25-Jun-2002 RD: Use ckopt SRs, new INPUT file for MAUPRP
!             16-Oct-2002 RD: ZD case: SATCLK file is mandatory
!             06-Dec-2002 RD: Add clock event handling
!             12-Mar-2003 RD: Warning for iLeos selection
!             23-Apr-2003 AJ: Nullify local pointers
!             16-May-2003 MM: Initialize structure
!             23-Jun-2003 HB: Interface for SR staFlg
!             10-Nov-2003 RD: Adapt to new input file
!             21-Jan-2004 RD: Set IEXTRA from TROPEST/METFIL
!             27-Jan-2004 RD: Remove check for old IEXTRA-keyword
!             03-Jun-2005 HU/RD: Allow empty MXAMBS user input
!             23-Jun-2005 HU: maxamb from m_maxdim
!             24-Aug-2006 AG: GMF implemented
!             07-Dec-2006 RD: Adjust MRK1O2 in DSRDBL.f
!             08-Sep-2007 RD: Advanced MXIOND (dep. from bsl-length)
!             08-Sep-2007 RD: Automatic switch between COMBINED/BOTH
!             01-Nov-2007 HB: Add option SECIPL for clock interpolation
!             30-Jun-2008 RD: VMF added
!             01-Oct-2008 HB: Single-frequency processing updated
!             14-Oct-2010 RD: Read tropo-model from SR trpopt
!             30-Nov-2010 DT: iZerod added to call of TRPOPT
!             06-May-2011 HB: Add rdstdh to initialize model names
!             18-Jun-2012 RD: SECIPL may be empty (no interpolation)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, &
                      staNameLength, fileNameLength, keyValueLength
  USE m_maxdim, ONLY: maxamb
  USE d_gpsobs, ONLY: t_obshead, init_obsHead
  USE d_stacrx, ONLY: MtypeSPACE
  USE d_stdorb, ONLY: t_stdhead, init_stdHead

  USE s_gtfile2
  USE s_ckoptr
  USE s_staflg
  USE s_rdhead2
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_gtflna
  USE s_ckopti
  USE s_ckoptl
  USE s_trpopt
  USE s_rdstdh
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:

! output:
  INTEGER(i4b)               :: iauto  ! flag for interactive or automatic
                                       ! preprocessing (0: interactive)
  INTEGER(i4b)               :: isetop ! options adjusted to file header info
                                       ! 0: take input file options in any case
                                       ! 1: adjust options to file header
  INTEGER(i4b)               :: nfrchk ! frequency(ies) to be checked
                                       ! 0: AUTO: L1 or COMBINED or BOTH
                                       ! 1: L1
                                       ! 2: L2
                                       ! 3: L1 and L2 together (via L5 and L3)
                                       ! 4: L1 and L2 separately
  REAL(r8b)                  :: minlen ! baseline length to decide COMBINED/BOTH
  INTEGER(i4b)               :: isave  ! save file option (1: YES)
  INTEGER(i4b)               :: iiono  ! apply ionosphere model (1: YES)
  INTEGER(i4b)               :: iusflg ! use or ignore obs file flags
  INTEGER(i4b)               :: mrk1o2 ! mark unpaired observations (1:YES)
  INTEGER(i4b)               :: maxzen ! max. satellite zenith distance (degree)
  REAL(r8b)                  :: secIpl ! max. interval for clock interpolation (seconds)
  INTEGER(i4b)               :: mxhole ! max. gap in observations allowed
                                       ! to be considered as continuous (sec)
  INTEGER(i4b)               :: mncont ! minimal time interval of continuous
                                       ! observations (sec)
  INTEGER(i4b)               :: iprnt1 ! print level for non-param. screening
                                       ! 0: NO, 1: SUMMARY, 2: DETAILS
  INTEGER(i4b)               :: mxintr ! maximum interval length for
                                       ! polynomial fit (min)
  INTEGER(i4b), DIMENSION(2) :: ipproc ! screening flag (1: YES)
                                       ! ipproc(1): screening single diff.
                                       ! ipproc(2): screening double diff.
  INTEGER(i4b), DIMENSION(2) :: qq     ! pol. degree used for screening
                                       ! qq(1): for single diff. screening
                                       ! qq(2): for double diff. screening
  REAL(r8b)   , DIMENSION(2) :: disclv ! max. allowed discontinuity
                                       ! disclv(1) for single diff. screening
                                       ! disclv(2) for double diff. screening
  INTEGER(i4b)               :: ltrip  ! carrier for triple diff. solution
                                       ! 1: L1, 2: L2, 3: L3, 5: L5
  REAL(r8b)   , DIMENSION(3) :: sigwgs ! a priori sigmas for coordinates (m)
  CHARACTER(LEN=staNameLength)::stafix ! name of station to be fixed when
                                       ! saving triple diff. coordinates
  INTEGER(i4b)               :: iprnt2 ! print level for cycle slip detection
                                       ! 0: NO, 1: SUMMARY, 2: DETAILS
  REAL(r8b)   , DIMENSION(2) :: sigl12 ! rms errors for frequencies L1 and L2
  INTEGER(i4b), DIMENSION(2) :: iwlscr ! wavelength factors for L1 and L2
                                       ! 1: full cycles, 2: half cycles
  INTEGER(i4b), DIMENSION(2) :: swidth ! number of nearest integers to be
                                       ! tested in L1/L2 and in L5
  INTEGER(i4b)               :: mincyc ! accept cycle slips > "mincyc" cycles
  INTEGER(i4b)               :: irject ! reject outliers (1: YES)
  INTEGER(i4b)               :: mxogap ! maximum observation gap allowed in
                                       ! outlier rejection (sec)
  INTEGER(i4b), DIMENSION(3) :: mxiond ! maximum ionosphere change between
                                       ! epochs (in % of L1 cycles)
                                       ! i=1 : for BOTH
                                       ! i=2 : for COMBINED
                                       ! i=3 : length where COMBINED value is used
  INTEGER(i4b), DIMENSION(5) :: iamnew ! setting of new ambiguities
                                       ! i=1 : use cycle slip flag (0/1)
                                       ! i=2 : if problem in slip-fixing (0/1)
                                       ! i=3 : after gap larger than (sec)
                                       ! i=4 : use ambiguities in file (0/1)
                                       ! i=5 : min. time interval per amb. (sec)
  INTEGER(i4b)               :: ntonly ! test obs. with cycle slip flag only
  INTEGER(i4b)               :: l5clea ! L5 is clean (except flagged epochs)
  REAL(r8b)                  :: omcmax ! max. OBSERVED-COMPUTED value (m)
  INTEGER(i4b)               :: mxambs ! Max. number of ambiguities
  INTEGER(i4b)               :: kinsta ! Kinematic coordinate estimation
  INTEGER(i4b)               :: mxzleo ! LEO max. zenith distance (degree)
  INTEGER(i4b)               :: itropo ! tropospheric model
  INTEGER(i4b)               :: iextra ! =0 : use measured values
                                       ! =1 : use atm. model values
                                       ! =2 : use est. bernese values
                                       ! =3 : Same as 2, but calling GETTRP2
                                       !      (special version for piecewise
                                       !      linear interpolat. of troposph.
                                       !      for CLKEST)
                                       !      IF (iextra==2) iextra=3
  INTEGER(i4b)               :: iZerod ! Observation file type
                                       ! =1 : Zero diff. files
                                       ! =2 : Single diff. files
  INTEGER(i4b)               :: iLeos  ! Select only LEOs/Non-LEOs files
                                       ! =0 : no LEOs in the file list
                                       ! =1 : only files with LEOs in list
  REAL(r8b)                  :: rmsmax ! Max rms allowed, otherwise sol.flag
  INTEGER(i4b),DIMENSION(6)  :: jmpopt ! Clock event options
                                       ! (1): 0/1 allow ms-jump cycle slips
                                       ! (2): min. size of a clock event (ns)
                                       ! (3): mark epochs with clock events
                                       !      up to (in s)
                                       ! (4): 0/1 ambiguities for all satellites
                                       ! (5): 0/1 flag if ms-jump in file
                                       ! (6): 0/1 flag if a clock event in file
  REAL(r8b)                  :: toljmp ! Tolerance to detect a ms-jump
  INTEGER(i4b)               :: nFil   ! Number of files
  CHARACTER(LEN=fileNameLength),       &
      DIMENSION(:,:),POINTER :: filist ! List of file names

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6),PARAMETER    :: srName = 'prinpt'

! Local Variables
! ---------------
  TYPE(t_obshead)               :: obsHead
  TYPE(t_stdhead)               :: stdHead, stdLHead  ! Structure of std header info

  CHARACTER(LEN=keyValueLength), &
       DIMENSION(:), POINTER    :: keyValue
  CHARACTER(LEN=fileNameLength) :: filnam
  CHARACTER(LEN=20),DIMENSION(2):: martyp

  INTEGER(i4b)                  :: itrmap
  INTEGER(i4b), DIMENSION(2)    :: itrgrd
  INTEGER(i4b)                  :: iCrdSig
  INTEGER(i4b)                  :: iFil
  INTEGER(i4b)                  :: mFil
  INTEGER(i4b)                  :: iFlag
  INTEGER(i4b)                  :: irCode
  INTEGER(i4b)                  :: irc

  NULLIFY(keyValue)
  CALL init_stdHead(stdHead)
  CALL init_obshead(obsHead)


! Init variables
! --------------
  irCode = 0

! Check zero or single files for screening
! ----------------------------------------
  CALL ckoptb(1,(/'RADIO_Z','RADIO_S'/),srName,                            &
              'Observation file type',irCode,                              &
              result1=iZerod)

! Leo processing
! --------------
  CALL ckoptb(1,(/'LEOPROC'/),srName,                                      &
              'Leo processing',irCode,                                     &
              result1=iLeos)

! Get the ist of files
! --------------------
  IF (iZerod == 1) THEN

    CALL gtfile2('PZFILES',2,nFil,filist)

  ELSE

    CALL gtfile2('OBSFIL' ,2,nFil,filist)

  ENDIF

! Check file list according to the LEO request
! (either only files with LEOs or without LEOs)
! ---------------------------------------------
  mFil = 0
  DO iFil = 1,nFil

    CALL rdhead2(filist(1,iFil),obsHead)

    CALL staflg(obsHead%sta(1)%staNam,obsHead%timref,iFlag,marTyp(1))
    CALL staflg(obsHead%sta(obsHead%nDiff+1)%staNam,obsHead%timref, &
                                                     iFlag,marTyp(2))

    IF ( iLeos == 1 .AND. &
         (marTyp(1) == MTypeSPACE .OR. marTyp(2) == MTypeSPACE)) THEN

      mFil = mFil+1
      IF (iFil /= mFil) filist(:,mFil) = filist(:,iFil)

    ELSE IF ( iLeos == 0 .AND. &
             (marTyp(1) /= MTypeSPACE .AND. marTyp(2) /= MTypeSPACE)) THEN

      mFil = mFil+1
      IF (iFil /= mFil) filist(:,mFil) = filist(:,iFil)

    ELSE IF (iLeos == 1) THEN

      WRITE(lfnerr,'(/,A,/,16X,A,/)') &
      ' ### SR PRINPT: File not used because LEO processing was selected.',   &
                      'File name: ' // TRIM(filist(1,iFil))

    ELSE

      WRITE(lfnerr,'(/,A,/,16X,A,/)') &
      ' ### SR PRINPT: File not used because no LEO processing was selected.', &
                      'File name: ' // TRIM(filist(1,iFil))

    ENDIF

    DEALLOCATE(obsHead%sat,   stat=irc)
    DEALLOCATE(obsHead%ambigu,stat=irc)
  ENDDO

  nFil = mFil


! Automatic mode, only
! --------------------
! ---------------------------------------------------
! Allow only for AUTOMATIC mode
! MANUAL mode not yet implemented in new menu program
! No zero-diff. available for MANUAL mode
! ---------------------------------------------------
  CALL readkeys('IAUTO', keyValue, irc)

  CALL ckoptc(1,'IAUTO',keyValue,(/'AUTOMATIC'/),srName,                   &
              'Run automatic mode',irc,irCode,                             &
              maxVal=1,result1=iAuto)

! Adjust Frequencies/Wave-length Factors
! --------------------------------------
  CALL ckoptb(1,(/'ISETOP'/),srName,                                       &
       'Adjust Frequencies/Wave-length Factors',irCode,                    &
       result1=isetop)

! Frequencies
! -----------
  CALL readkeys('NFRCHK', keyValue, irc)

  CALL ckoptc(1,'NFRCHK',keyValue,                                         &
              (/'AUTO    ','L1      ','L2      ','COMBINED','BOTH    '/),  &
              srName,'Frequency/Mode to check',irc,irCode,                 &
              maxVal=1,result1=nfrchk)
  nfrchk=nfrchk-1

!!  IF (iZerod == 1 .AND. nfrchk /= 3) THEN
!!    WRITE(lfnerr,'(/,A,/,16X,A,/)')                                           &
!!         ' ### SR PRINPT: For zero difference files only the "COMBINED" mode',&
!!                         'is allowed to check the observations.'
!!    nfrchk = 3
!!  ENDIF

! AUTO: baseline length: COMBINED/BOTH
! ------------------------------------
  minlen=0d0
  IF (nfrchk == 0) THEN
    CALL readkeys('MINLEN', keyValue, irc)

    CALL ckoptr(1,'MINLEN',keyValue,srName,                                &
                  'Max. baseline length for BOTH (else COMBINED)',         &
                  irc,irCode,maxVal=1,gt=0d0,result1=minlen)
  ENDIF


! Store resulting files
! ---------------------
  CALL ckoptb(1,(/'ISAVE'/),srName,                                        &
       'Store changes in the files',irCode,                                &
       result1=iSave)

! Use ionosphere model?
! ---------------------
  CALL gtflna(0,'IONOS',filnam,irc)

  iIono = 0
  IF (irc == 0 .AND. LEN_TRIM(filnam) > 0) iIono = 1

! Tropospheric Model
! ------------------
  CALL trpopt(iZerod, itropo, iextra, itrmap, itrgrd)

! Hardwired iextra in the case of iextra==2
  IF (iextra == 2) iextra=3

! (No SLR in MAUPRP)
  IF (iTropo == 4) THEN
    WRITE(lfnerr,'(/,A,/,16X,A,/)')                                          &
         ' ### SR PRINPT: Troposphere model "MARINI-MUR" is not implemented',&
                         'in the program. Model "SAASTAMOINEN" is used instead.'
    iTropo = 1
  ENDIF

! Fixed station for saving coordinates
! ------------------------------------
  staFix = ' '
  CALL gtflna(0,'COORDRS',filNam,irc)

  IF (iZerod /= 1 .AND. irc == 0 .AND. LEN_TRIM(filNam) > 0) THEN

    CALL readkeys('STAFIX' , keyValue, irc)

    CALL ckoptl(1,'STAFIX' , keyValue, srName,                             &
                'Fixed stat. for storing baseline vectors', irc,irCode,    &
                empty=' ', maxVal=1, maxLength=staNameLength, result1=staFix)

  ENDIF

! Marking of Observations
! -----------------------

! Use marking flags from files
! ----------------------------
  CALL ckoptb(1,(/'IUSFLG'/),srName,                                       &
              'Use marking flags from obs. files',irCode,                  &
              result1 = iusflg)

! Mark unpaired observations
! --------------------------
!  CALL ckoptb(1,(/'MRK1O2'/),srName,                                       &
!              'Mark unpaired observations',irCode,                         &
!              result1 = mrk1o2)
  mrk1o2 = 1 ! Is adjusted in DSRDBL: =0 for L1,L2
             !                        =1 for both, combined

! Mark observations below elevation
! ---------------------------------
  CALL readkeys('MINEL' , keyValue, irc)

  CALL ckopti(1,'MINEL',keyValue,srName,                                   &
              'Mark observations below elevation',irc,irCode,              &
              ge=0,le=90,maxVal=1,result1=maxzen)

  maxzen = 90 - maxzen

! Special evelvation cut off for leos
! -----------------------------------
  mxzLeo = 0
  IF (iLeos == 1) THEN

    CALL readkeys('MINELEO' , keyValue, irc)

    CALL ckopti(1,'MINELEO',keyValue,srName,                               &
              'Elevation cut off for LEOs',irc,irCode,                     &
              ge=-90,le=90,maxVal=1,result1=mxzleo)

    mxzleo = 90 - mxzleo
  ENDIF

! Max. interval for clock interpolation
! -------------------------------------
  CALL readkeys('SECIPL' , keyValue, irc)

  CALL ckoptr(1,'SECIPL',keyValue,srName,                                   &
              'Max. interval for clock interpolation',irc,irCode,           &
              empty=0d0,ge=0.D0,le=3600.D0,maxVal=1,result1=secIpl)


! Obs still cont. if gaps smaller than
! ------------------------------------
  CALL readkeys('MXHOLE', keyValue, irc)

  CALL ckopti(1,'MXHOLE',keyValue,srName,                                  &
              'Obs still cont. if gaps smaller than',irc,irCode,           &
              ge=0,maxVal=1,result1=mxHole)

! Min. time int. for continuous obs
! ---------------------------------
  CALL readkeys('MNCONT', keyValue, irc)

  CALL ckopti(1,'MNCONT',keyValue,srName,                                  &
              'Min. time int. for continuous obs',irc,irCode,              &
              ge=0,maxVal=1,result1=mnCont)

! Non-Parametric Screening
! ------------------------

! Printing options
! ----------------
  CALL readkeys('IPRNT1' , keyValue, irc)

  CALL ckoptc(1,'IPRNT1' , keyValue,                                       &
              (/'DETAILS','ALL    ','SUMMARY','NONE   '/),                 &
              srName,'Print of non-parametric screening',irc,irCode,       &
              maxVal=1,valList=(/2,2,1,0/),result1=iprnt1)

! Max. interval of fit
! --------------------
  CALL readkeys('MXINTR',keyValue,irc)

  CALL ckopti(1,'MXINTR',keyValue,srName,                                  &
              'Max. interval of fit',irc,irCode,                           &
              ge=0,maxVal=1,result1=mxIntr)

! Screening of file difference level
! ----------------------------------
  CALL ckoptb(1,(/'IPPROC1'/),srName,                                      &
              'Screening of file difference level',irCode,                 &
              result1=ipproc(1))

! Polynomial degree for file level screening
! ------------------------------------------
  IF (ipproc(1) == 1) THEN
    CALL readkeys('QQ1', keyValue, irc)

    CALL ckopti(1,'QQ1',keyValue,srName,                                   &
                'Polynomial degree',irc,irCode,                            &
                maxVal=1,ge=0,result1=qq(1))

! Discontinuity level for file level screening
! --------------------------------------------
    CALL readkeys('DISCLV1', keyValue, irc)

    CALL ckoptr(1,'DISCLV1',keyValue,srName,                               &
                'Discontinuity level',irc,irCode,                          &
                maxVal=1,gt=0d0,result1=disclv(1))
  ENDIF

! Screening of file satellite difference level
! --------------------------------------------
  CALL ckoptb(1,(/'IPPROC2'/),srName,                                      &
              'Screening of sat.-difference level',irCode,                 &
              result1=ipproc(2))


! Polynomial degree for sat-diff level screening
! ------------------------------------------
  IF (ipproc(2) == 1) THEN
    CALL readkeys('QQ2', keyValue, irc)

    CALL ckopti(1,'QQ2',keyValue,srName,                                   &
                'Polynomial degree',irc,irCode,                            &
                maxVal=1,ge=0,result1=qq(2))

! Discontinuity level for sat-diff. level screening
! --------------------------------------------
    CALL readkeys('DISCLV2', keyValue, irc)

    CALL ckoptr(1,'DISCLV2',keyValue,srName,                               &
                'Discontinuity level',irc,irCode,                          &
                maxVal=1,gt=0d0,result1=disclv(2))
  ENDIF


! Frequency for Epoch-diff. solution
! ----------------------------------
  CALL readkeys('LTRIP', keyValue, irc)

  CALL ckoptc(1,'LTRIP', keyValue, (/'L1','L2','L3','L5'/),                &
              srName,'Frequency for epoch-diff. solution',irc,irCode,      &
              maxVal=1,valList=(/1,2,3,5/),result1=lTrip)

! Kinematic coordinate estimation
! -------------------------------
  CALL ckoptb(1,(/'KINSTA'/),srName,                                      &
              'Kinematic coordinate estimation',irCode,                   &
              result1=kinSta)

  IF (kinSta == 1 .AND. nfrchk /= 3) THEN
    WRITE(lfnerr,'(/,A,2(/,16X,A),/)')                                        &
         ' ### SR PRINPT: The kinematic coordinate estimation is only allowed',&
                         'in the "COMBINED" mode to check the observations.', &
                         'No kinematic coordinate estimation takes place.'
    kinSta = 0
  ENDIF

! Maximum observed minus computed
! -------------------------------
  CALL readkeys('OMCMAX' , keyValue, irc)

  CALL ckoptr(1,'OMCMAX',keyValue,srName,                                  &
              'Maximum observed-computed value',irc,irCode,                &
              maxVal=1,ge=0d0,result1=omcMax)

! Apriori constraints for station coordinates
! -------------------------------------------
  sigwgs(1:3) = 0D0

  CALL ckoptb(1,(/'APRCWGT'/),srName,                                      &
              'Apriori coordinate sigmas',irCode,                          &
              result1=iCrdsig)

! Sigma for X-Coordinate
! ----------------------
  IF (iCrdsig == 1) THEN

    CALL readkeys('SIGWGS1', keyValue, irc)

    CALL ckoptr(1,'SIGWGS1',keyValue,srName,                               &
                'Coordinate sigma for X-component',irc,irCode,             &
                maxVal=1,ge=0d0,result1=sigwgs(1))

! Sigma for Y-Coordinate
! ----------------------
    CALL readkeys('SIGWGS2', keyValue, irc)

    CALL ckoptr(1,'SIGWGS2',keyValue,srName,                               &
                'Coordinate sigma for Y-component',irc,irCode,             &
                maxVal=1,ge=0d0,result1=sigwgs(2))

! Sigma for Z-Coordinate
! ----------------------
    CALL readkeys('SIGWGS3', keyValue, irc)

    CALL ckoptr(1,'SIGWGS3',keyValue,srName,                               &
                'Coordinate sigma for Z-component',irc,irCode,             &
                maxVal=1,ge=0d0,result1=sigwgs(3))

! Warning for kin. coordinates if no constraints are set...
! ---------------------------------------------------------
  ELSE IF (kinSta == 1) THEN
    WRITE(lfnerr,'(/A,/,16X,A,/)')                                         &
    ' ### SR PRINPT: It is highly recommended to constrain apriori coordinates',&
                    'if you estimate kinematic coordinates.'
  ENDIF


! RMS limit for for epoch solution
! --------------------------------
  rmsmax = 0d0
  IF (kinSta == 1 .OR. iZerod == 1) THEN
    CALL readkeys('RMSMAX', keyValue, irc)

    CALL ckoptr(1,'RMSMAX',keyValue,srName,                               &
                'RMS limit for epoch solution',irc,irCode,                &
                maxVal=1,gt=0d0,result1=rmsmax)
  ENDIF

! Minimum size of a clock event
! -----------------------------
  jmpopt(2) = 0
  IF (iZerod == 1) THEN
    CALL readkeys('CLKOPT2', keyValue, irc)

    CALL ckopti(1,'CLKOPT2',keyValue,srName,                              &
                'Minimum size of a clock event',irc,irCode,               &
                maxVal=1,ge=0,empty=0,result1=jmpopt(2))
  ENDIF

! Tolerance for MS-Jumps
! ----------------------
  toljmp = 0d0
  IF (iZerod == 1) THEN
    CALL readkeys('TOLJMP', keyValue, irc)

    CALL ckoptr(1,'TOLJMP',keyValue,srName,                               &
                'Tolerance for MS-jump detection',irc,irCode,             &
                maxVal=1,gt=0d0,result1=toljmp)
  ENDIF

! Deletion flag for MS-jumps
! --------------------------
  jmpopt(5) = 0
  IF (iZerod == 1) THEN
    CALL ckoptb(1,(/'CLKOPT5'/),srName,                                  &
                'Deletion flag for files with MS-jumps',irCode,          &
                result1=jmpopt(5))
  ENDIF

! MS-jumps: handle as cycle slips
! -------------------------------
  jmpopt(1) = 0
  IF (iZerod == 1.AND.jmpopt(5)==0) THEN
    CALL ckoptb(1,(/'CLKOPT1'/),srName,                                  &
                'Repair the MS-jump as cycle slip',irCode,               &
                result1=jmpopt(1))
  ENDIF

! Deletion flag for clock events
! ------------------------------
  jmpopt(6) = 0
  IF (iZerod == 1) THEN
    CALL ckoptb(1,(/'CLKOPT6'/),srName,                                  &
                'Deletion flag for files with clock events',irCode,      &
                result1=jmpopt(6))
  ENDIF

! Clock event: mark observations
! ------------------------------
  jmpopt(3) = 0
  IF (iZerod == 1.AND.jmpopt(6)==0) THEN
    CALL readkeys('CLKOPT3', keyValue, irc)

    CALL ckopti(1,'CLKOPT3',keyValue,srName,                             &
                'Clock event: mark observations',irc,irCode,             &
                maxVal=1,ge=0,empty=0,result1=jmpopt(3))
  ENDIF

! Clock event: set ambiguities
! ----------------------------
  jmpopt(4) = 0
  IF (iZerod == 1.AND.jmpopt(6)==0) THEN
    CALL ckoptb(1,(/'CLKOPT4'/),srName,                                  &
                'Set ambiguities if a clock event was found',irCode,     &
                result1=jmpopt(4))
  ENDIF

! Cycle Slip Detection and Outlier Rejection
! ------------------------------------------
  CALL readkeys('IPRNT2'  , keyValue, irc)

  CALL ckoptc(1,'IPRNT2' , keyValue,                                       &
              (/'DETAILS','ALL    ','SUMMARY','NONE   '/),                 &
              srName,'Printing for cycle slip detection',irc,irCode,       &
              maxVal=1,valList=(/2,2,1,0/),result1=iprnt2)

! Disable cycle slip correction
! -----------------------------
  CALL ckoptb(1,(/'CYCDET'/),srName,                                       &
              'Disable correction of cycle slips',irCode,                  &
              result1=mincyc)

  IF ( mincyc==1 ) THEN
    mincyc = -1

! Accept cycle slip greater than
! ------------------------------
  ELSE

    CALL readkeys('MINCYC', keyValue, irc)

    CALL ckopti(1,'MINCYC',keyValue,srName,                                &
                'Accept slips greater than',irc,irCode,                    &
                maxVal=1,ge=0,result1=mincyc)

  ENDIF

! Test obs with cycle slip flag only
! ----------------------------------
  CALL ckoptb(1,(/'NTONLY'/),srName,                                       &
              'Test obs with cycle slip flag only',irCode,                 &
              result1=ntOnly)

! L5 is clean (except flagged epochs)
! -----------------------------------
  CALL ckoptb(1,(/'L5CLEA'/),srName,                                       &
              'L5 is clean (except flagged epochs)',irCode,                &
              result1=l5Clea)

! Sigma of L1 observations
! ------------------------
  CALL readkeys('SIGL12_1', keyValue, irc)

  CALL ckoptr(1,'SIGL12_1',keyValue,srName,                                &
              'Sigma of L1 observations',irc,irCode,                       &
              maxVal=1,gt=0d0,result1=sigl12(1))

! Sigma of L1 observations
! ------------------------
  CALL readkeys('SIGL12_2', keyValue, irc)

  CALL ckoptr(1,'SIGL12_2',keyValue,srName,                                &
              'Sigma of L2 observations',irc,irCode,                       &
              maxVal=1,gt=0d0,result1=sigl12(2))

! Wave length factor for L1
! -------------------------
  CALL readkeys('IWLSCR1',keyValue, irc)

  CALL ckoptc(1,'IWLSCR1',keyValue,(/'FULL','HALF'/),                      &
              srName,'Wave length factor for L1',irc,irCode,               &
              maxVal=1,result1=iwlscr(1))

! Wave length factor for L2
! -------------------------
  CALL readkeys('IWLSCR2',keyValue, irc)

  CALL ckoptc(1,'IWLSCR2',keyValue,(/'FULL','HALF'/),                      &
              srName,'Wave length factor for L2',irc,irCode,               &
              maxVal=1,result1=iwlscr(2))

! Search width for L1
! -------------------
  CALL readkeys('SWIDTH1',keyValue, irc)

  CALL ckopti(1,'SWIDTH1',keyValue,srName,                                 &
              'Search width for L1',irc,irCode,                            &
              maxVal=1,ge=1,result1=swidth(1))

! Search width for L5
! -------------------
  CALL readkeys('SWIDTH2' , keyValue, irc)

  CALL ckopti(1,'SWIDTH2',keyValue,srName,                                 &
              'Search width for L5',irc,irCode,                            &
              maxVal=1,ge=1,result1=swidth(2))

! Outlier rejection
! -----------------
  CALL ckoptb(1,(/'IRJECT'/),srName,'Enable outlier rejection',irCode,     &
              result1=irject)

! Max. observation gap
! --------------------
  CALL readkeys('MXOGAP'  , keyValue, irc)

  CALL ckopti(1,'MXOGAP'  , keyValue,srName,                               &
              'Max. observation gap',irc,irCode,                           &
              maxVal=1,ge=0,result1=mxogap)

! Max. ionospheric difference (both mode)
! ---------------------------------------
  mxiond(:)=0
  CALL readkeys('MXIOND_B',keyValue, irc)

  CALL ckopti(1,'MXIOND_B',keyValue,srName,                                &
              'Max. ionospheric difference (both mode)',irc,irCode,                    &
              maxVal=1,ge=0,result1=mxiond(1))

! Max. ionospheric difference (combined mode)
! ---------------------------------------
  IF (nfrchk == 3.OR.nfrchk==0) THEN
    CALL readkeys('MXIOND_C',keyValue, irc)

    CALL ckopti(1,'MXIOND_C',keyValue,srName,                              &
                'Max. ionospheric difference (combined)',irc,irCode,       &
                maxVal=1,ge=mxiond(1),result1=mxiond(2))

    CALL readkeys('MXIOND_L',keyValue, irc)

    CALL ckopti(1,'MXIOND_L',keyValue,srName,                              &
                'Use the comb. mode value for bsl longer than',irc,irCode, &
                maxVal=1,empty=0,ge=0,result1=mxiond(3))

  ENDIF

! Ambiguity setting
! -----------------

! Ambiguity if cycle slip flag set in file
! ----------------------------------------
  CALL ckoptb(1,(/'IAMNEW1'/),srName,                                      &
              'Ambiguity if cycle slip flag set in file',irCode,           &
              result1=iamnew(1))

! Ambiguity if cycle slip detection problem
! -----------------------------------------
  CALL ckoptb(1,(/'IAMNEW2'/),srName,                                      &
              'Ambiguity if cycle slip detection problem',irCode,          &
              result1=iamnew(2))

! Ambiguity after a gap larger than
! ---------------------------------
  CALL readkeys('IAMNEW3',keyValue, irc)

  CALL ckopti(1,'IAMNEW3',keyValue,srName,                                 &
              'Ambiguity after a gap larger than',irc,irCode,              &
              maxVal=1,ge=1,result1=iamnew(3))

! Use ambiguities from file
! -------------------------
  CALL ckoptb(1,(/'IAMNEW4'/),srName,                                      &
              'Use ambiguities from file',irCode,                          &
              result1=iamnew(4))

! Minimum time interval per ambiguity
! -----------------------------------
  CALL readkeys('IAMNEW5',keyValue, irc)

  CALL ckopti(1,'IAMNEW5',keyValue,srName,                                 &
              'Minimum time interval per ambiguity',irc,irCode,            &
              maxVal=1,ge=1,result1=iamnew(5))

! Remove obs. if too many ambig. in file
! --------------------------------------
  CALL readkeys('MXAMBS',keyValue, irc)

  CALL ckopti(1,'MXAMBS',keyValue,srName,                                  &
              'Remove obs. if too many ambig. in file',irc,irCode,         &
              maxVal=1,ge=0,le=maxamb,empty=0,result1=mxambs)

! Check the availablility of a satellite clock file for ZD screening
! ------------------------------------------------------------------
  IF (iZerod == 1) THEN

    CALL gtflna(0,'SATCLK',filnam,irc)

    IF (irc /= 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/)')                                       &
           ' *** SR PRINPT: For screening of zero difference observation ', &
                           'files a satellite clock file is mandatory.'

      irCode = irCode + 1
    ENDIF

  ENDIF

! Get model information from standard orbit file header
! -----------------------------------------------------
  filNam = ' '
  CALL rdstdh(filNam,stdHead,irc)

! If LEO should be processed, check STD-file
! ------------------------------------------
  IF (iLeos == 1) THEN
    CALL gtflna(0,'LEOSTD', filnam, irc)
    IF (irc == 0) THEN
      CALL init_stdHead(stdLHead)
      CALL rdstdh(filNam,stdLHead,irc)
    ENDIF
  ENDIF

! Problems reading input options
! ------------------------------
  IF (irCode /= 0) CALL exitrc(2)

  DEALLOCATE(keyValue,stat=irc)

END SUBROUTINE prinpt

END MODULE
