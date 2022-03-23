MODULE s_AOPTFIL
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE aoptfil(opt, namList, clkList, &
                   limits, parOrb, parGcc, parSao, parAtm, parErp, &
                   parSap, parRao, parRap, parRco, parDcb, parGrd, &
                   parRgb,parGsp)

! -------------------------------------------------------------------------
! Purpose:    Reads input options for ADDNEQ2:
!             input NEQ files and orbit files
!
! Author:     M. Meindl
!
! Created:    10-Jul-2001
! Last mod.:  30-Nov-2010
!
! Changes:    21-Dec-2001 HU: m_addneq replaced by p_addneq
!             23-Jan-2002 CU: Get parameter input options from staneq:
!                             parorb, parcom, parsaof
!             19-Apr-2002 RD: Optional output in STANEQ
!             25-Sep-2002 HU: Remove i_astlib
!             04-Nov-2002 HB: Get some parameter input options from the neqs:
!                             paratm
!             10-Dec-2002 CU: Get parameter input options from the neqs:
!                             parErp
!             23-Apr-2003 CU: Nullify local pointers
!             09-Jul-2003 RD: staInfo for STANEQ is a parameter now
!             22-Dec-2003 RS: Add parSap
!             06-Feb-2004 HU: Truncate station names
!             05-May-2005 HU: Read option chrono
!             17-Aug-2005 HU: Read keyword NOCHRONO instead of CHRONO
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!             11-Jan-2007 MM: Option COVMODE introduced
!             22-Mar-2007 MM: Do not read COVMODE unless required
!             25-Jan-2008 RD: add RAO/RAP parameters
!             04-May-2009 RD: Scaling of loading models added
!             06-May-2009 RD: increase maxoff (multi-year solutions), old maxsat
!             06-May-2006 HU/RD: Read step2 options
!             10-May-2009 RD: Receiver clock offsets/biases implemented
!             10-Jun-2009 RD: Get clkList from STANEQ
!             24-Jun-2009 RD: Put direct constraints on DCBs
!             13-Aug-2009 DT: Add range bias parameters
!             27-Nov-2009 RD: Station exceptions for pre-elimination
!             26-Oct-2010 SL: truncation bug corrected, use m_bern with ONLY
!             30-Nov-2010 MM: GNSS-specific parameters
!
! SR used:    exitrc, alcerr, readkeys, stripdir, staneq
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, lfnErr, lfnPrt, &
                      keyValueLength, fileNameLength, staNameLength
  USE m_time,   ONLY: t_timint
  USE m_maxdim, ONLY: maxsat
  USE d_par,    ONLY: maxParTyp
  USE d_phaecc, ONLY: init_buf
  USE p_addneq, ONLY: t_opt,    t_parOrb, t_parGcc, t_parSao, t_parAtm, &
                      t_parErp, t_parSap, t_parRco, t_parRao, t_parRap, &
                      t_parRgb, t_parGsp,                               &
                      maxOff, maxSpv, t_optLoad, t_namLst
  USE s_alcerr
  USE s_staneq
  USE s_readkeys
  USE s_stripdir
  USE s_exitrc
  USE s_ckoptb
  USE s_ckopti
  USE s_ckoptc
  USE s_ckoptd
  USE s_gtflna

  IMPLICIT NONE

! List of parameters
! ------------------
! input:
! output:
  TYPE(t_opt)                              :: opt    ! Options for ADDNEQ2
  TYPE(t_namLst),   DIMENSION(:)           :: namList! List of parameter names
                                                     !   per parameter type
  CHARACTER(LEN=*) ,DIMENSION(:,:),POINTER :: clkList
  TYPE(t_timint),   DIMENSION(maxParTyp)   :: limits
  TYPE(t_parOrb)                           :: parOrb ! Orbit param. inp. options
  TYPE(t_parGcc)                           :: parGcc ! Geocenter coord. inp. opt.
  TYPE(t_parSao),   DIMENSION(:),  POINTER :: parSao ! Sat. ant. offset inp. opt.
  TYPE(t_parSap),   DIMENSION(:),  POINTER :: parSap ! Sat. ant. pattern inp. opt.
  TYPE(t_parAtm)                           :: parAtm ! Atmosphere parameter
  TYPE(t_parErp)                           :: parErp ! Earth rotation parameters
  TYPE(t_parRco)                           :: parRco ! Receiver clock offset/bias
  TYPE(t_parRao)                           :: parRao ! Rec. ant. offset inp. opt.
  TYPE(t_parRap)                           :: parRap ! Rec. ant. pattern inp. opt.
  TYPE(t_parRgb)                           :: parRgb ! Range bias parameters
  TYPE(t_parGsp)                           :: parGsp ! GNSS-spec parameters
  TYPE(t_optLoad),  DIMENSION(:)           :: parGrd ! Scaling of Vienna grid files
  INTEGER(i4b)  ,   DIMENSION(:,:)         :: parDcb ! DCB-Parameters

! Local variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=keyValueLength)                         :: dirOrb
  CHARACTER(LEN=keyValueLength)                         :: elefile
  CHARACTER(LEN=keyValueLength)                         :: wldName, hlpName
  CHARACTER(LEN=filenameLength)                         :: filHlp
  CHARACTER(LEN=3)                                      :: ext_ele
  INTEGER(i4b)                                          :: nochrono
  INTEGER(i4b)                                          :: iWild
  INTEGER(i4b)                                          :: lenDot
  INTEGER(i4b)                                          :: iNeq, nNeq
  INTEGER(i4b)                                          :: iErr
  INTEGER(i4b)                                          :: irc, ios, iac
  INTEGER(i4b)                                          :: irCode
  LOGICAL                                               :: estVelo

! ************************************************************************

  NULLIFY(keyValue)
  irCode = 0

! InputNEQ files
! --------------
  CALL readkeys('INPFILE', keyValue, irc)
  nNeq = SIZE(keyValue)
  ALLOCATE (opt%neqFileName(nNeq), stat=iac)
  CALL alcerr(iac, 'opt%neqFileName',(/nNeq/), 'aoptfil')
  ALLOCATE (opt%orbFil(4,nNeq), stat=iac)
  CALL alcerr(iac, 'opt%orbFil', (/4,nNeq/), 'aoptfil')

  IF (irc == 0) THEN
    DO iNeq = 1, nNeq
      opt%neqFileName(iNeq) = TRIM(keyValue(iNeq))
    END DO
  ELSE
    WRITE(lfnerr,'(/,A,/)')                                               &
    ' *** SR ADDTST: Problem with filenames of the input NEQs'
    CALL exitrc(2)
  ENDIF

! Read the Directories and Extensions
! -----------------------------------
  iErr = 0
  CALL readkeys('DIR_ORB', keyValue, irc)
  READ(keyValue(1), '(A)', iostat=ios) dirOrb
  iErr = iErr + irc + ios
  CALL readkeys('EXT_ELE', keyValue, irc)
  READ(keyValue(1), *, iostat=ios) ext_ele
  iErr = iErr + irc + ios
  IF (iErr /= 0) THEN
    WRITE(lfnerr,'(/,A,/)')                                               &
    ' *** SR AOPTFIL: Problems with path and/or extension of orbit files'
    CALL exitrc(2)
  ENDIF

! Truncate station names after position noabc
! -------------------------------------------
  CALL readkeys("NO_ABC",keyValue,irc)
  IF (keyValue(1)=="NO") THEN
    opt%noabc = staNameLength
  ELSE IF (keyValue(1)=="YES") THEN
    opt%noabc = 14
  ELSE
    CALL ckopti(1,'NO_ABC',keyValue,'sr aoptfil',                        &
                'Truncate station names',irc,irCode,empty=staNameLength, &
                ge=0,le=staNameLength,result1=opt%noabc)
  ENDIF

! Correct step2 tide bug
! ----------------------
  CALL readkeys('STEP2',keyValue,irc)
  CALL ckoptb(1, (/'STEP2'/), 'sr aoptfil',                              &
              'Correct step2 tide bug', irCode,                          &
              result1=opt%step2)

  opt%step2_t = (/ 1D20, 0D0 /)
  IF (opt%step2 == 1) THEN
    CALL readkeys('STEP2A',keyValue,irc)
    CALL ckoptd(1,'STEP2A',keyValue,'sr aoptfil',                        &
                'Correct step2 tide bug, start date', irc, irCode,       &
                empty=0D0,maxVal=1,result1=opt%step2_t(1))

    CALL readkeys('STEP2B',keyValue,irc)
    CALL ckoptd(1,'STEP2B',keyValue,'sr aoptfil',                        &
                'Correct step2 tide bug, end mjd', irc, irCode,          &
                empty=1D20,maxVal=1,result1=opt%step2_t(2))

    WRITE(lfnprt,"(' ### SR AOPTFIL: Step 2 correction', &
                                 & ' from ',F8.1,' to ',F8.1,/)") &
                                                     opt%step2_t(1:2)
  ENDIF


! Order input NEQs chronologically
! --------------------------------
  CALL readkeys("NOCHRONO",keyValue,irc)
  CALL ckoptb(1, (/'NOCHRONO'/), 'sr aoptfil',                           &
              'Order input NEQs chronologically', irCode,                &
              result1=nochrono)
  opt%chrono = 1-nochrono

! CRD/VEL or CRD/CRD in var-covar files
! -------------------------------------
  CALL ckoptb(1,(/'CRD_NINT'/),'sr aoptfil','Estimate velocities',       &
              irCode,resultL=estVelo)

  IF (estVelo .AND. (opt%covarrs /= "" .OR. opt%covttrs /= "")) THEN
    CALL readkeys('COVMODE',keyValue,irc)
    CALL ckoptc(1,'COVMODE',keyValue,(/'CRD/VEL','CRD/CRD'/),'sr aoptfil', &
            'CRD/CRD or CRD/VEL in var-covar file',irc,irCode,             &
            result1=opt%covMode)
  ENDIF

! Get Information about STD and RPR files and Parameter Time Limits
! -----------------------------------------------------------------
  CALL staneq(1,opt%neqFileName(:), opt%orbFil(:,:),                      &
              namList, clkList, limits, parOrb, parGcc, parSao, parAtm,   &
              parErp, parSap, parRao, parRap, parRco, parDcb, parGrd,     &
              parRgb, parGsp)

! Create all used filenames
! -------------------------
  CALL readkeys('ELEFILE', keyValue, irc)
  IF (irc == 0) THEN
    elefile = keyValue(1)
  ELSE
    WRITE(lfnerr,'(/,A,/,17X,A,A/)')                                      &
    ' *** SR AOPTFIL: Problems with filename of orbital elements.',       &
                     'Specified name: ', TRIM(keyValue(1))
    CALL exitrc(2)
  ENDIF

! Get string with wildcards
  CALL stripDir(elefile)
  lenDot = INDEX(elefile, '.', BACK=.TRUE.)

! Loop over all NEQs
  DO iNeq = 1, SIZE(opt%neqFileName)
    hlpName = opt%orbFil(1,iNeq)
    CALL stripDir(hlpName)
    CALL stripDir(opt%orbFil(1,iNeq))
    CALL stripDir(opt%orbFil(2,iNeq))

    opt%orbFil(1,iNeq) = dirOrb(1:LEN_TRIM(dirOrb)) // opt%orbFil(1,iNeq)
    opt%orbFil(2,iNeq) = dirOrb(1:LEN_TRIM(dirOrb)) // opt%orbFil(2,iNeq)
    opt%orbFil(3,iNeq) = ''
    opt%orbFil(4,iNeq) = ''

! Create ele-filename
    IF (lenDot >0) THEN
      DO iWild = 1, lenDot
        IF (elefile(iWild:iWild) == '%' .OR.                              &
            elefile(iWild:iWild) == '?') THEN
! wildcard-string to long
          IF (iWild == INDEX(hlpName,'.', BACK=.TRUE.)) THEN
            WRITE(lfnerr,'(/,A,/,17X,A,A/)')                              &
            ' *** SR AOPTFIL: Name for orbital elements too long',        &
                             'Specified name: ', TRIM(elefile)
            CALL exitrc(2)
          ENDIF
          wldName(iWild:iWild) = hlpName(iWild:iWild)
        ELSE
          wldName(iWild:iWild) = elefile(iWild:iWild)
        ENDIF
      ENDDO
      opt%orbFil(4,iNeq) = dirOrb(1:LEN_TRIM(dirOrb)) //                  &
                           wldName(1:lenDot) // ext_ele
    ENDIF
  END DO

  DEALLOCATE(keyValue,stat=iac)

  IF (irCode/=0) CALL exitrc(2)

  CALL gtflna(0,'PHASECC',filhlp,irc)
  IF (irc == 0) &
    CALL init_buf(bufSize=(/parRao%nRao+MAX0(parRap%nRapGrd,parRap%nRapShm), &
                          MAX0(maxOff,maxSpv)/))

  RETURN
END SUBROUTINE aoptfil

END MODULE
