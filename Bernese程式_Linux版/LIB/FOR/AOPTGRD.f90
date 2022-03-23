MODULE s_AOPTGRD
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE aoptgrd(opt, parGrd)

! -------------------------------------------------------------------------
! Purpose:    Reads input options for ADDNEQ2:
!             options and constraining for scaling factors for Vienna grid files
!
! Author:     R. Dach
!
! Created:    04-May-2009
!
! Changes:    06-Jul-2010 DT: Remove unused variables
!             13-Jun-2012 MM: Adapted to new keywords
!             19-Sep-2012 RD: use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfnprt, lfnloc, &
                      keynamelength, keyvaluelength, filenamelength, &
                      stanamelength, shortlinelength
  USE d_grid,   ONLY: grdNeq
  USE p_addneq, ONLY: t_opt, t_sigma, t_optLoad

  USE s_alcerr
  USE f_linCount
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckoptr
  USE s_exitrc
  USE s_prisig
  USE s_readkeys
  USE s_gtflna
  USE s_opnfil
  USE s_opnerr
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  TYPE(t_optLoad), DIMENSION(:)          :: parGrd

! input/output:
  TYPE(t_opt)                            :: opt    ! Options for ADDNEQ2

! output:


! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=7),PARAMETER              :: srName = 'aoptgrd'

! Define some keywords
  CHARACTER(LEN=keyNameLength), DIMENSION(3,4), PARAMETER :: grdKeyw = &
  reshape( source =                                                    &
           (/ 'ALOADSET', 'ONTLDSET', 'HLOADSET',                      &
              'ALOADCLU', 'ONTLDCLU', 'HLOADCLU',                      &
              'ALOAD10 ', 'ONTLD10 ', 'HLOAD10 ',                      &
              'ALOADDEC', 'ONTLDDEC', 'HLOADDEC'/),shape = (/ 3,4 /) )

  CHARACTER(LEN=keyNameLength), DIMENSION(3,3), PARAMETER :: grdSigma = &
  reshape( source =                                                     &
           (/ 'ALOADSIG1', 'ONTLDSIG1', 'HLOADSIG1',                    &
              'ALOADSIG2', 'ONTLDSIG2', 'HLOADSIG2',                    &
              'ALOADSIG3', 'ONTLDSIG3', 'HLOADSIG3' /),                 &
           shape = (/ 3,3 /) )


! Local variables
! ---------------
  TYPE(t_sigma),                 &
       DIMENSION(:),ALLOCATABLE :: locSig

  CHARACTER(LEN=keyValueLength), &
       DIMENSION(:), POINTER    :: keyValue
  CHARACTER(LEN=fileNameLength) :: cluFil
  CHARACTER(LEN=shortLineLength):: line
  CHARACTER(LEN=shortLineLength):: parTxt
  CHARACTER(LEN=staNameLength)  :: staNam

  INTEGER(i4b)                  :: maxSigTyp
  INTEGER(i4b)                  :: oldSigTyp
  INTEGER(i4b)                  :: nGrd
  INTEGER(i4b), DIMENSION(9)    :: seqGrd
  INTEGER(i4b)                  :: nSig, iSig
  INTEGER(i4b)                  :: ii,jj
  INTEGER(i4b)                  :: iSta
  INTEGER(i4b)                  :: cluNum
  INTEGER(i4b)                  :: irCode
  INTEGER(i4b)                  :: irc, iac, ios

  REAL(r8b),    DIMENSION(9)    :: sigGrd
  REAL(r8b)                     :: sigma

  nSig = 0
  irCode = 0
  NULLIFY(keyValue)

! Get other sigmas from opt%sigma
! -------------------------------
  oldSigTyp = SIZE(opt%sigma)
  maxSigTyp = oldSigTyp + 9

  ALLOCATE(locSig(maxSigTyp),stat=iac)
  CALL alcerr(iac,'locSig',(/maxSigTyp/),srName)

! Initialize local sigma
  DO iSig = 1,maxSigTyp
    locSig(iSig)%locq(:)   = 0
  ENDDO
  locSig(:)%value     = 0.D0
  locSig(:)%name      = ''
  locSig(:)%typFlg    = 'A'
  locSig(1:oldSigTyp) = opt%sigma(:)

! Start the program output
! ------------------------
  DO ii = 1, SIZE(parGrd)
    IF ( parGrd(ii)%keyw == '' ) CYCLE
    WRITE(lfnprt,'(2(A,/))')      &
      ' Scaling factor for Vienna grid files:',  &
      ' ------------------------------------'
    EXIT
  ENDDO

! Read options for the scaling factors
! ------------------------------------
  nGrd = 0
  DO ii = 1, SIZE(grdKeyw,1)

    ! Copy the keyword into the structure
    IF (SIZE(grdNeq) < ii) THEN
      WRITE(lfnerr,'(/,A,2(/,17X,A),/)') &
      ' *** SR AOPTGRD: There is an inconcistency between the number of ', &
                       'supported Vienna grid file types between ADDNEQ2 ',&
                       'and the list in D_GRID.'
      CALL exitrc(2)
    ENDIF
    opt%grdLoad(ii)%keyw = grdNeq(ii)
    opt%grdLoad(ii)%nSta = 0

    ! No parameters of this type are in the NEQ file
    IF ( parGrd(ii)%keyw == '' ) CYCLE

    CALL readKeys(grdKeyw(ii,1), keyValue, irc)
    CALL ckoptc(1,grdKeyw(ii,1), keyValue,                              &
                (/ 'PER_STATION','PER_GROUP  ','ONE_OVERALL'/),         &
                srName, 'Scaling factor for grid files', irc, irCode,   &
                maxVal = 1, valList = (/ 0,1,-1 /),                     &
                result1 = opt%grdLoad(ii)%nSta)

    IF (opt%grdLoad(ii)%nSta == 0 .AND. parGrd(ii)%nSta /= 0) THEN
      WRITE(lfnerr,'(/,A,3(/,17X,A),/)')                                 &
      ' ### SR AOPTGRD: Scaling factors for Vienna grid files cannot be',&
                       'provided individually for each station since ',  &
                       'the NEQ file contains already a grouping of the',&
                       'parameters.'
    ELSEIF (opt%grdLoad(ii)%nSta == 1 .AND. parGrd(ii)%nSta == -1) THEN
      WRITE(lfnerr,'(/,A,2(/,17X,A),/)')                                 &
      ' ### SR AOPTGRD: Scaling factors for Vienna grid files cannot be',&
                       'provided by groups of stations since there is ', &
                       'only one set of parameters in the NEQ file.'
    ENDIF

    ! Read the cluster file
    IF (opt%grdLoad(ii)%nSta == 1 .AND. parGrd(ii)%nSta >= 0) THEN

      opt%grdLoad(ii)%nSta = linCount(grdKeyw(ii,2),5)

      ALLOCATE(opt%grdLoad(ii)%staLst(opt%grdLoad(ii)%nSta),stat=iac)
      CALL alcerr(iac,'opt%grdLoad%staLst',(/opt%grdLoad(ii)%nSta/),srName)
      ALLOCATE(opt%grdLoad(ii)%staClu(opt%grdLoad(ii)%nSta),stat=iac)
      CALL alcerr(iac,'opt%grdLoad%staClu',(/opt%grdLoad(ii)%nSta/),srName)

      CALL gtflna(1,grdKeyw(ii,2),cluFil,irc)
      CALL opnfil(lfnloc,cluFil,'OLD','FORMATTED','READONLY',' ',irc)
      CALL opnerr(lfnerr,lfnloc,irc,cluFil,srName)

      READ(lfnloc,'(////)',iostat=ios)

      iSta = 0
      DO WHILE (ios == 0)
        READ(lfnloc,'(A)',iostat=ios) line
        IF (ios /= 0 .OR. LEN_TRIM(line) == 0) EXIT

        iSta = iSta + 1
        READ(line,'(A16,2X,I3)',iostat=irc) staNam, cluNum

        IF (irc /= 0) THEN
          WRITE(lfnerr,'(/,A,/,17X,A,A,/,17X,A,I6,/)')             &
            ' *** SR AOPTGRD: Error reading cluster file records', &
                             'File name:     ', TRIM(cluFil),      &
                             'Record number: ',iSta
          CALL exitrc(2)
        ENDIF

        opt%grdLoad(ii)%staLst(iSta) = staNam
        opt%grdLoad(ii)%staClu(iSta) = cluNum
      ENDDO

      CLOSE(lfnloc)

    ENDIF

    ! Get the default value
    CALL readKeys(grdKeyw(ii,3), keyValue, irc)
    CALL ckoptc(1,grdKeyw(ii,3), keyValue, (/ 'ZERO','ONE '/), srName,       &
                'Scaling factor for grid files: default value', irc, irCode, &
                maxVal = 1, valList = (/ 0, 1 /),result1 = opt%grdLoad(ii)%x0)

    ! What types of scaling parameters are requested?
    CALL readKeys(grdKeyw(ii,4), keyValue, irc)
    CALL ckoptc(1,grdKeyw(ii,4), keyValue, (/'FULL_EFFECT        ', &
                'HORIZONTAL/VERTICAL','NORTH/EAST/UP      ' /),     &
                srName, 'Parameter decomposition', irc, irCode,     &
                maxVal = 1, result1 = opt%grdLoad(ii)%nPar)

    IF ( opt%grdLoad(ii)%nPar > parGrd(ii)%nPar ) THEN
      WRITE(lfnerr,'(/,A,3(/,17X,A),/)')                                       &
      ' ### SR AOPTGRD: Scaling factors for Vienna grid files have less ',     &
                       'parameters per station in the NEQ file than requested',&
                       'in the input panel. No transformation and constraing ',&
                       'is performed.'
      CYCLE
    ENDIF

    ! Read the apriori sigmas
    DO jj = 1,opt%grdLoad(ii)%nPar

      CALL readKeys(grdSigma(ii,jj), keyValue, irc)
      CALL ckoptr(1,grdSigma(ii,jj), keyValue, srName,                      &
                 'Sigma for scaling factors', irc, irCode,                  &
                  maxVal = 1, ge = 0d0, empty = 0d0, result1 = sigma)
      IF (sigma /= 0d0) THEN
        nSig                 = nSig + 1
        iSig                 = nSig + oldSigTyp
        locSig(iSig)%locq(1) = 22
        locSig(iSig)%locq(2) = ii
        locSig(iSig)%locq(4) = jj
        locSig(iSig)%locq(5) = opt%grdLoad(ii)%nPar
        locSig(iSig)%value   = sigma

      ENDIF

      nGrd = nGrd + 1
      sigGrd(nGrd) = sigma
      IF (opt%grdLoad(ii)%nPar == 1) seqGrd(nGrd) = (ii-1)*6 + jj
      IF (opt%grdLoad(ii)%nPar == 2) seqGrd(nGrd) = (ii-1)*6 + jj + 1
      IF (opt%grdLoad(ii)%nPar == 3) seqGrd(nGrd) = (ii-1)*6 + jj + 3
    ENDDO
  ENDDO

! Generate the program output
! ---------------------------
  DO ii = 1,SIZE(opt%grdLoad)

    IF ( parGrd(ii)%keyw == '' ) CYCLE

    IF (opt%grdLoad(ii)%nSta == -1) THEN
      parTxt = ' scaling factors for all stations together'

    ELSE IF (opt%grdLoad(ii)%nSta == 0) THEN
      parTxt = ' scaling factors for each individual station'

    ELSE IF (opt%grdLoad(ii)%nSta > 0) THEN
      parTxt = ' scaling factors for groups of stations'
    ENDIF

    WRITE(lfnprt,'(3A,I1,A,//)') &
          ' Grid type: ',TRIM(opt%grdLoad(ii)%keyw), &
          '   included with ',opt%grdLoad(ii)%npar, parTxt
  ENDDO
  CALL prisig(22, sigGrd, nGrd, seqGrd)

! Store sigmas in opt%sigma
! -------------------------
  DEALLOCATE(opt%sigma,stat=iac)
  ALLOCATE(opt%sigma(nSig+oldSigTyp),stat=iac)
  CALL alcerr(iac,'opt%sigma',(/nSig+oldSigTyp/),'aoptotr')

  nSig = 0
  DO iSig = 1,maxSigTyp
    IF (locSig(iSig)%value /= 0d0) THEN
      nSig            = nSig+1
      opt%sigma(nSig) = locSig(iSig)
    ENDIF
  ENDDO

! Deallocate local pointers
! -------------------------
  DEALLOCATE(locSig,stat=iac)
  DEALLOCATE(keyValue,stat=iac)

  IF (irCode /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE aoptgrd

END MODULE
