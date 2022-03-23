
! -------------------------------------------------------------------------
! Bernese GNSS Software Version 5.2
! -------------------------------------------------------------------------

PROGRAM qlrsum

! -------------------------------------------------------------------------
! Purpose:    Create quick look residual analyses report for slrmail.
!             (derived from old program SLRRES.f)
!
! Author:     C.Urschl
!
! Created:    08-Dec-2003
!
! Changes:    16-Apr-1004 CU: Use interface alcerr
!             28-Jun-2004 RD: Use maxsta from M_MAXDIM.f90
!             26-Jul-2004 CU: Decrease dimensions for maxpts, maxpas
!             10-Aug-2004 CU: Add error message if no sat. found
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             02-Aug-2005 CU: Check size of real variable for DNINT
!             27-Feb-2007 AG: Call DEFCON with parameter
!             30-May-2007 AG: Use s_suneff
!             04-May-2008 RD: Svn added to call of sr XYZELE
!             31-Mar-2009 DT: Add azimuth and nadir (satellite system)
!             19-May-2009 DT: Increase maxfls 20->200
!             19-May-2009 DT: Summary in OUT-file in [mm]
!             25-May-2009 DT: Extended outlier detection; write EDT-File
!             23-Jul-2009 DT: Check min. Number of Observations
!             13-Apr-2010 DT/AJ: Add angles beta, u in PLT files
!             25-May-2010 MF: Nullify keyValue & filnms
!             27-Aug-2010 DT: Own maxsta (100 instead of 3000; Compiler problems)
!             23-Sep-2010 RD: Enable CPU counter
!             01-OCT-2010 CR: new call of SUNEFF
!             03-OCT-2010 CR: new call of ARGSUN
!             17-Dec-2010 RD,MF: Use gtflna for CRDFIL
!             17-Mar-2011 CR: Call MOSUPN as a module
!             31-Aug-2011 DT: Increase maxsat 20->30
!             30-Nov-2011 SL: new title string for pritit, m_bern with ONLY
!             24-Jan-2012 LP: "STD" instead of "RMS" in SUMFIL table header
!             27-Apr-2012 RD: Nullify pointers, remove unused variables
!             15-May-2012 RD: Correct format for reporting input options
!             15-May-2012 RD: Remove unused variables
!             05-Jul-2012 RD: Array of "sun" in mosupn with the correct size
!             30-Jul-2012 RD: SR STATIS with optional arguments
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------
  USE m_bern,   ONLY: i4b, r8b, keyValueLength, fileNameLength80, &
                      fileNameLength, staNameLength, lineLength, &
                      lfnPrt, lfnRes, lfnErr, lfn001, lfn002, lfnPlt
  USE m_cpu,    ONLY: cpu_start
  USE d_const,  ONLY: gm, pi
  USE d_inpkey, ONLY: inpkey, init_inpkey
  USE d_resfil, ONLY: t_resHead
  USE d_stdorb, ONLY: t_stdhead, init_stdHead
  USE d_edit,   ONLY: t_edit, init_edit
  USE s_arglat
  USE s_argsun
  USE s_ckoptb
  USE s_ckopti
  USE s_ckoptr
  USE s_opnfil
  USE s_rdstdh
  USE s_shadow
  USE s_pritit
  USE s_inquire
  USE s_ephem
  USE s_getorf
  USE s_mosupn
  USE s_readkeys
  USE s_suneff
  USE s_statis
  USE s_defcon
  USE s_opnsys
  USE s_xyzele
  USE s_gtflna
  USE s_gtfile2
  USE s_gtflna
  USE s_dimtst
  USE s_edtadd
  USE s_alcerr
  USE s_prflna
  USE s_rdresh2
  USE s_readinpf
  USE s_opnerr
  USE s_prfile
  USE s_svn2typ
  USE s_wtedit2
  USE s_exitrc
  USE s_jmt
  USE s_getco3
  USE s_cootra
  USE s_topsta
  USE f_modsvn
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_resHead)                        :: resHed ! Residual file header
  TYPE(t_stdhead), DIMENSION(:), POINTER :: stdHdr ! Standard orbit file header
  TYPE(t_edit)                           :: edt    ! Edit file (shadow)
  TYPE(t_edit)                           :: edtOut ! Edit file (outlier)

  INTEGER(i4b), PARAMETER  :: maxpts = 1000 ! Points per satellite and station (edtOut%fil)
  INTEGER(i4b), PARAMETER  :: maxsta =  100 ! Stations
  INTEGER(i4b), PARAMETER  :: maxsat =   30 ! Satellites
  INTEGER(i4b), PARAMETER  :: maxfls =  200 ! Files per residual file
  INTEGER(i4b), PARAMETER  :: maxpas = 200 ! Satellite passes per stations
!  INTEGER(i4b), PARAMETER  :: maxpas = 2000 ! Satellite passes per stations
  INTEGER(i4b), PARAMETER  :: maxedt = 100  ! Entries for edit file

  CHARACTER(LEN=6)         :: pgName = 'QLRSUM'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength),   DIMENSION(:),   POINTER :: keyValue
  CHARACTER(LEN=fileNameLength80)                          :: filnam
  CHARACTER(LEN=fileNameLength)                            :: filEdt
  CHARACTER(LEN=fileNameLength),   DIMENSION(:,:), POINTER :: filnms
  CHARACTER(LEN=fileNameLength80), DIMENSION(:,:), POINTER :: orbfil
  CHARACTER(LEN=fileNameLength80)                          :: crdfil
  CHARACTER(LEN=staNameLength), DIMENSION(maxsta)          :: stalst
  CHARACTER(LEN=staNameLength), DIMENSION(:), POINTER      :: stName

  CHARACTER(LEN=lineLength)                                :: line
  CHARACTER(LEN=9)                                         :: tmpstr
  CHARACTER(LEN=1)                                         :: cshad
  CHARACTER(LEN=1), DIMENSION(:), ALLOCATABLE              :: sattyp

  INTEGER(i4b), DIMENSION(:), ALLOCATABLE                :: satlst
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE                :: satmod, svnmod
  INTEGER(i4b), DIMENSION(maxsat)                        :: satnum
  INTEGER(i4b), DIMENSION(maxsat,maxsta)                 :: nrres
  INTEGER(i4b)                                           :: nrsat
  INTEGER(i4b), DIMENSION(2)                             :: inpsat
  INTEGER(i4b), DIMENSION(maxpas)                        :: pmean, psig
  INTEGER(i4b), DIMENSION(maxpts,maxsat,maxsta)          :: ishad
  INTEGER(i4b), DIMENSION(:,:,:), ALLOCATABLE            :: resIndSat
  INTEGER(i4b), DIMENSION(:,:,:), ALLOCATABLE            :: resIndFil
  INTEGER(i4b)                                           :: irc, irc1, irc2, irCode
  INTEGER(i4b)                                           :: irc002, irc003, ircplt
  INTEGER(i4b)                                           :: ios
  INTEGER(i4b)                                           :: nobs, nbad
  INTEGER(i4b)                                           :: itot, ntot, ntotsav
  INTEGER(i4b)                                           :: iout
  INTEGER(i4b)                                           :: isat, ista
  INTEGER(i4b)                                           :: ifil, iorbfil
  INTEGER(i4b)                                           :: iftot
  INTEGER(i4b)                                           :: nrsta
  INTEGER(i4b)                                           :: nresfil, norbfil
  INTEGER(i4b)                                           :: iy, id, im, ih, idt
  INTEGER(i4b)                                           :: mm
  INTEGER(i4b)                                           :: if
  INTEGER(i4b)                                           :: ii, ll
  INTEGER(i4b)                                           :: iloop
  INTEGER(i4b)                                           :: ifreq
  INTEGER(i4b)                                           :: iorsys
  INTEGER(i4b)                                           :: icrarc
  INTEGER(i4b)                                           :: year, month
  INTEGER(i4b)                                           :: imeant, imeab
  INTEGER(i4b)                                           :: isigt, isib
  INTEGER(i4b)                                           :: ires
  INTEGER(i4b)                                           :: iobs
  INTEGER(i4b)                                           :: ipass
  INTEGER(i4b)                                           :: istart
  INTEGER(i4b)                                           :: iter
  INTEGER(i4b)                                           :: iele, iazi
  INTEGER(i4b)                                           :: isub, nsub
  INTEGER(i4b)                                           :: timrf0
  INTEGER(i4b)                                           :: iepoch
  INTEGER(i4b)                                           :: ishd
  INTEGER(i4b)                                           :: indx
  INTEGER(i4b)                                           :: indst, indCrd
  INTEGER(i4b)                                           :: nsat
  INTEGER(i4b)                                           :: findex
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE                :: numtot
  INTEGER(i4b)                                           :: lfn003
  INTEGER(i4b)                                           :: nStat
  INTEGER(i4b)                                           :: indSta, indSat
  INTEGER(i4b)                                           :: modEdt
  INTEGER(i4b), DIMENSION(:,:), ALLOCATABLE              :: nEdt
  INTEGER(i4b)                                           :: nEdtTot
  INTEGER(i4b)                                           :: minObs
  INTEGER(i4b)                                           :: modChk
  INTEGER(i4b)                                           :: iArc, nArc

  REAL(r8b), DIMENSION(6)                                :: xsat, xSat1
  REAL(r8b), DIMENSION(4)                                :: xsun
  REAL(r8b), DIMENSION(7)                                :: ele
  REAL(r8b), DIMENSION(maxpts*maxsat*maxsta)             :: total
  REAL(r8b), DIMENSION(maxpts)                           :: respas
  REAL(r8b), DIMENSION(maxpts)                           :: resedt
  REAL(r8b), DIMENSION(maxpts,maxsat,maxsta)             :: resepo
  REAL(r8b), DIMENSION(maxpts,maxsat,maxsta)             :: ressat
  REAL(r8b), DIMENSION(maxpts,maxsat,maxsta)             :: resazi
  REAL(r8b), DIMENSION(maxpts,maxsat,maxsta)             :: resele
  REAL(r8b), DIMENSION(maxpts,maxsat,maxsta)             :: resAziSat
  REAL(r8b), DIMENSION(maxpts,maxsat,maxsta)             :: resNadSat
  REAL(r8b), DIMENSION(maxpts,maxsat,maxsta)             :: resAglSat
  REAL(r8b), DIMENSION(maxpts,maxsat,maxsta)             :: resb0Sat
  REAL(r8b), DIMENSION(:,:), POINTER                     :: xStat
  REAL(r8b), DIMENSION(maxsat,maxsta)                    :: xrms2, xsig2
  REAL(r8b)                                              :: edtlvl, edtp
  REAL(r8b)                                              :: sigmax
  REAL(r8b)                                              :: sigsav
  REAL(r8b)                                              :: sigfac
  REAL(r8b)                                              :: xmean, xmeab, xmeant
  REAL(r8b)                                              :: xrms, xrmb
  REAL(r8b)                                              :: xsig, xsib, xsigt
  REAL(r8b)                                              :: day
  REAL(r8b)                                              :: dd, dm, hh, dt
  REAL(r8b)                                              :: mjd
  REAL(r8b)                                              :: tstart, tend
  REAL(r8b)                                              :: epoch, epoch1
  REAL(r8b)                                              :: ttt, tosc
  REAL(r8b)                                              :: res
  REAL(r8b)                                              :: delta
  REAL(r8b)                                              :: timCrd
  REAL(r8b)                                              :: xpol, ypol, ut1gps
  REAL(r8b)                                              :: SZ
  REAL(r8b)                                              :: nadir
  REAL(r8b)                                              :: aziSat, aziSat2
  REAL(r8b)                                              :: agl, agl1
  REAL(r8b)                                              :: b0, u0
  REAL(r8b)                                              :: t1, t2
  REAL(r8b)                                              :: tb1, tb2
  REAL(r8b)                                              :: dumIn, dumOut
  REAL(r8b), DIMENSION(3)                                :: xTopo
  REAL(r8b), DIMENSION(3)                                :: dumVec
  REAL(r8b), DIMENSION(4)                                :: sun, dumVec4
  REAL(r8b), DIMENSION(3,3)                              :: dumMat
  REAL(r8b)                                              :: maxRMS, maxSig

  REAL(r8b)                                              :: DIST,AZI,ZEN

  LOGICAL                                                :: edtfl  ! for shadow
  LOGICAL                                                :: open
  LOGICAL                                                :: chkFlg
  LOGICAL                                                :: edit


! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Nullify local pointers
! ----------------------
  NULLIFY(reshed%filHead)
  NULLIFY(orbfil)
  NULLIFY(stdHdr)
  NULLIFY(filnms)
  NULLIFY(keyValue)
  NULLIFY(stName)
  NULLIFY(xstat)

! Init values
! -----------
  nrsat = 0
  nrsta = 0

  maxRMS  = 0d0
  maxSig  = 0d0
  modEdt  = 0
  nEdtTot = 0

  chkFlg = .FALSE.
  minObs = 0
  modChk = 0

  tb1 = 1d10
  tb2 = 0d0

  ALLOCATE(nEdt(maxsat,maxsta), stat=irc)
  CALL alcerr(irc,'nEdt',(/maxsat,maxsta/),pgName)

  DO ifil = 1,maxsta
    DO isat = 1, maxsat
      nrres(isat,ifil) = 0
      nEdt(isat,ifil) = 0
    ENDDO
  ENDDO

  edtfl    = .FALSE.

! Open system files, define constants
! -----------------------------------
  CALL init_inpkey(inpkey)
  CALL init_edit(edt)
  CALL init_edit(edtOut)
  CALL readinpf('',inpkey)
  CALL opnsys
  CALL defcon(1)

! Print title section
! -------------------
  CALL pritit(pgName, 'Create SLR quick-look residual statistics')
  CALL prflna(131)

! Read input options
! ------------------
  CALL readkeys('RESMAX',keyValue,irc)
  CALL ckoptr(1,'RESMAX', keyValue, pgName,          &
              'Maximum of residuals', irc ,irCode,   &
              empty=0d0,ge=0d0,maxVal=1,result1=edtlvl)

  CALL readkeys('SIGMAX',keyValue,irc)
  CALL ckoptr(1,'SIGMAX', keyValue, pgName,          &
              'Maximum overall sigma', irc ,irCode,  &
              empty=0d0,ge=0d0,maxVal=1,result1=sigmax)

  CALL readkeys('SIGFAC',keyValue,irc)
  CALL ckoptr(1,'SIGFAC', keyValue, pgName,          &
              'Multiplication factor', irc ,irCode,  &
              empty=0d0,ge=0d0,maxVal=1,result1=sigfac)

! sigma in mm
  sigsav = sigmax
! sigma in m
  sigmax = sigmax/1d3

  CALL readkeys('EDIT_1',keyValue,irc)
  IF ( irc == 0 .AND. keyValue(1) == '1' ) modEdt = 1

  CALL readkeys('EDIT_2',keyValue,irc)
  IF ( irc == 0 .AND. keyValue(1) == '1' ) modEdt = 2


! Read satellite numbers
  CALL readKeys('SATLST',keyValue,irc)
  nsat = SIZE(keyValue)

  DO isat = 1, nsat
    IF (LEN_TRIM(keyValue(isat)) == 0) nsat = 0
  ENDDO

! No satellites specified
  IF (nsat == 0 .OR. irc /= 0) THEN
    WRITE(lfnerr,'(A,A)')                                &
      ' *** PG QLRSUM: There are no satellite numbers ', &
      'specified in the input panel.'
    CALL exitrc(2)
  ENDIF

! Too many satellites specified
  CALL dimtst(1,1,2,pgName,'maxsat','satellites',' ',nsat,maxsat,irc)

  ALLOCATE(satlst(nsat), stat=irc)
  CALL alcerr(irc,'satlst',(/nsat/),pgName)

  DO isat = 1, nsat
    READ(keyValue(isat),*) satlst(isat)
  ENDDO

! Options concerning minimum number of observations
! -------------------------------------------------
  CALL ckoptb(1, (/ 'CHKNUM' /), pgName, &
              'Enable change number of intervals', irc, &
              result1=ii)
  IF ( ii == 1 ) chkFlg = .TRUE.

  IF ( chkFlg ) THEN

    CALL readkeys('MINOBS',keyValue,irc)
    CALL ckopti(1,'MINOBS', keyValue, pgName,                   &
                'Minimum number of observations', irc ,irCode,  &
                empty=0,ge=0,maxVal=1,result1=minObs)

    CALL readkeys('CHKNUM_1',keyValue,irc)
    IF ( irc == 0 .AND. keyValue(1) == '1' ) modChk = 1

    CALL readkeys('CHKNUM_2',keyValue,irc)
    IF ( irc == 0 .AND. keyValue(1) == '1' ) modChk = 2

  END IF

! Get the residual filenames
! --------------------------
  CALL gtfile2('RESFIL',1,nresfil,filnms)
  IF (nresfil <= 0) THEN
    WRITE(lfnerr,'(A,/)')' *** PG QLRSUM: No residual files found.'
    CALL exitrc(2)
  ENDIF
  CALL prfile('RESFIL',' ',1)

  ALLOCATE(numtot(nresfil), stat=irc)
  CALL alcerr(irc,'numtot',(/nresfil/),pgName)

! Get the edit filename (shadow passages)
! ---------------------------------------
  CALL gtflna(0,'EDITRS',filnam,irc1)

! Loop over residual file headers to count number of files
  IF ( irc1 == 0 .OR. modEdt > 0 ) THEN
    iftot = 0

    DO iloop = 1, nresfil
      CALL opnfil(lfnres,filnms(1,iloop),'OLD','UNFORMATTED', &
                  'READONLY',' ',irc)
      CALL opnerr(lfnerr,lfnres,irc,filnms(1,iloop),pgName)
      CALL rdresh2(lfnres,resHed)

      iftot         = iftot + resHed%nFil
      numtot(iloop) = resHed%nfil

      CLOSE(lfnRes)
    ENDDO

  ENDIF

! Fill edt-structure (shadow passsages)
! -------------------------------------
  IF (irc1 == 0) THEN

    edt%filNam = TRIM(filnam)

  ! Allocate arrays for edit file structure
    ALLOCATE(edt%head(iftot),stat=irc)
    CALL alcerr(irc, 'edt%head', (/iftot/), pgName)
    ALLOCATE(edt%rec(maxEdt),stat=irc)
    CALL alcerr(irc, 'edt%rec', (/maxEdt/), pgName)

    edt%title     = 'Range observations during shadow passages'
    edt%resMax(1) = 0d0
    edt%resMax(2) = 0d0
    edt%resMax(3) = edtlvl
    edt%nSampl    = 1
    edt%nEdt      = 0
    edt%minAmb    = 0
    edt%minInt    = 1
    edt%iSampl    = 1
    edt%nEdFil    = iftot
    edtfl         = .TRUE.

  ENDIF

! Get the edit filename (mark outliers)
! -------------------------------------
  CALL gtflna(0,'EDTFIL',filEdt,irc2)

  IF ( modEdt > 0 .AND. irc2 == 0 ) THEN

    edtOut%filNam = TRIM(filEdt)

    ALLOCATE(edtOut%head(iftot),stat=irc)
    CALL alcerr(irc, 'edtOut%head', (/iftot/), pgName)
    ALLOCATE(edtOut%rec(maxEdt),stat=irc)
    CALL alcerr(irc, 'edtOut%rec', (/maxEdt/), pgName)

    edtOut%title     = 'Range observations marking as outliers'
    edtOut%resMax(1) = 0d0
    edtOut%resMax(2) = 0d0
    edtOut%resMax(3) = edtlvl
    edtOut%nSampl    = 1
    edtOut%nEdt      = 0
    edtOut%minAmb    = 0
    edtOut%minInt    = 1
    edtOut%iSampl    = 1
    edtOut%nEdFil    = iftot

    ALLOCATE(resIndSat(maxpts,maxsat,maxsta),stat=irc)
    CALL alcerr(irc, 'resIndSat', (/maxpts,maxsat,maxsta/), pgName)
    ALLOCATE(resIndFil(maxpts,maxsat,maxsta),stat=irc)
    CALL alcerr(irc, 'resIndFil', (/maxpts,maxsat,maxsta/), pgName)

  ELSE
    modEdt = 0

  ENDIF

! Get the standard orbit filenames
! --------------------------------
  CALL gtfile2('STDORB',1,norbfil,orbfil)

  IF (norbfil <= 0) THEN
    WRITE(lfnerr,'(A,/)')' ### PG QLRSUM: No orbit files found.'
  ENDIF
  CALL prfile('STDORB',' ',1)

  ALLOCATE (stdHdr(norbfil),stat=irc)
  CALL alcerr(irc,'stdHdr',(/norbfil/),pgName)

! Loop over all orbit files
! -------------------------
  DO ifil = 1, norbfil
  ! nullify pointer
    CALL init_stdhead(stdHdr(ifil))
  ! get header information from orbit file
    CALL rdstdh(orbfil(1,ifil),stdHdr(ifil),irc)
    nArc = stdHdr(ifil)%narc
  ! get first/last epochs of arcs
    DO iArc = 1, nArc
      t1 = stdHdr(ifil)%arc(iArc)%tbound(1)
      t2 = stdHdr(ifil)%arc(iArc)%tbound(2)
      IF (t1 < tb1) tb1 = t1
      IF (t2 > tb2) tb2 = t2
    ENDDO
  ENDDO

! Prepare table for sun position
  CALL mosupn(0.d0,0,tb1,tb2,0.5d0,dumOut,dumOut,dumOut,dumOut, &
              dumMat,dumMat,dumVec4,dumVec,dumOut,dumVec)

! Get station coordinates
! -----------------------
  CALL gtflna(1,'CRDFIL',crdfil, irc)

  CALL prfile('CRDFIL',' ',1)

  CALL getco3(crdfil, 1, (/'#'/), nstat, stname, xstat= xstat, &
              timcrd= timcrd)

! Print input options into protocol file
! --------------------------------------
  WRITE(lfnprt,'(2(A,/),2(/,A,F12.2,A),/,A,F12.1,/,/,A)',ADVANCE='NO')&
    ' Input options',                                              &
    ' -------------',                                              &
    ' Absolute maximum of the o-c residuals:    ',edtlvl,    ' m', &
    ' Maximum overall sigma:                    ',sigmax*1d3,' mm',&
    ' Multiplication factor for overall sigma:  ',sigfac,          &
    ' List of satellites:'

  DO isat = 1, nsat
    WRITE(lfnprt,'(2X,I3)',ADVANCE='NO') satlst(isat)
  ENDDO

  IF ( modEdt == 1 ) THEN
    WRITE(lfnprt,'(//,A)') &
      ' Edit only station/satellite with largest RMS'

  ELSEIF ( modEdt == 2 ) THEN
    WRITE(lfnprt,'(//,A)') &
      ' Edit residuals according to criteria above'
  ENDIF

  IF ( chkFlg ) THEN
    WRITE(lfnprt,'(//,A,/,A,I6)') &
          ' Check for minimum number of observations activated', &
          ' Minimum number of observations requested: ', minObs

    IF ( modChk == 1 ) THEN
      WRITE(lfnprt,'(A)') &
            ' Check per station (sum over all satellites)'

    ELSEIF ( modChk == 2 ) THEN
      WRITE(lfnprt,'(A)') &
            ' Check per station and per satellite'
    ENDIF

  ELSE
    WRITE(lfnprt,'(//,A)') &
          ' No check for minimum number of observations'
  END IF


! Loop over all residual files
! ----------------------------
  iftot = 0

  DO if = 1, nresfil

! Read residual file header
! -------------------------
    CALL opnfil(lfnres,filnms(1,if),'OLD','UNFORMATTED','READONLY',' ',irc)
    CALL opnerr(lfnerr,lfnres,irc,filnms(1,if),pgName)

    CALL rdresh2(lfnres,resHed)

    IF (resHed%dsc%ityp /= 1) THEN
      WRITE(lfnerr,'(A)')' ### PG QLRSUM: Residual file is not of ITYP = 1.'
      CYCLE
    ENDIF

    IF (if == 1) timrf0 = INT(resHed%filHead(if)%timref)

    IF (resHed%dsc%nResta == 2) resHed%dsc%nResta  = 1

! Check for maxfls
! ----------------
    CALL dimtst(1,1,1,pgName,'maxfls','number of files',' ', &
                resHed%nFil,maxfls,irc)

! Fill information for editing file (shadow passages) into arrays
! ---------------------------------------------------------------
    IF (edtfl) THEN
      iftot = 0
      DO ifil = 1, resHed%nfil
        iftot = iftot + 1
        edt%head(iftot)%cseEdt(:) = resHed%filHead(iFil)%csess(1:2)
        edt%head(iftot)%staEdt(:) = resHed%filHead(iFil)%stanam(1:2)
        IF (resHed%dsc%nResta == 0) edt%head(iftot)%staEdt(2) = ' '
        edt%head(iftot)%timEdt    = resHed%filHead(iFil)%timref
        edt%head(iftot)%idtEdt    = resHed%filHead(iFil)%ideltt
        edt%head(iftot)%meaEdt    = resHed%filHead(iFil)%meatyp
      ENDDO
    ENDIF

! Fill information for editing file (outliers) into arrays
! --------------------------------------------------------
    IF ( modEdt > 0 ) THEN
      iftot = 0
      DO ifil = 1, resHed%nfil
        iftot = iftot + 1

        edtOut%head(iftot)%cseEdt(:) = resHed%filHead(iFil)%csess(1:2)
        edtOut%head(iftot)%staEdt(:) = resHed%filHead(iFil)%stanam(1:2)
        IF (resHed%dsc%nResta == 0) edtOut%head(iftot)%staEdt(2) = ' '
        edtOut%head(iftot)%timEdt    = resHed%filHead(iFil)%timref
        edtOut%head(iftot)%idtEdt    = resHed%filHead(iFil)%ideltt
        edtOut%head(iftot)%meaEdt    = resHed%filHead(iFil)%meatyp
      ENDDO
    ENDIF

! Loop over all lines of one residual file (only type 1 = 1-freq)
! ---------------------------------------------------------------
    DO
      READ(lfnres,iostat=ios) &
        ifil,iepoch,ifreq,(inpsat(ii),ii=1,2),res
      IF (ios /= 0) EXIT ! end of file reached or error

! Loop over all "nResta" (Difference level of stations)
! -----------------------------------------------------
      DO id = 1,resHed%dsc%nResta+1

! Find satellite index
! --------------------
        indx = 0

        DO isat = 1, nrsat
          IF (inpsat(id) == satnum(isat)) indx = isat
        ENDDO

        IF (indx == 0) THEN
          DO ii = 1, nsat
            IF (inpsat(id)==satlst(ii)) THEN ! requested satellite found
              nrsat        = nrsat+1
              indx         = nrsat
              satnum(indx) = inpsat(id)
              EXIT
            ENDIF
          ENDDO
        ENDIF

        IF (nrsat == 0 .OR. indx == 0) CYCLE ! no requested satellite found

! Find station index
! ------------------
        indst = 0
        DO ista = 1, nrsta
          IF (resHed%filHead(ifil)%stanam(id) == stalst(ista)) indst = ista
        ENDDO
        IF (indst == 0) THEN
          nrsta         = nrsta+1
          indst         = nrsta
          stalst(indst) = resHed%filhead(ifil)%stanam(id)
        ENDIF

! Index of Station in Coordinate file
! -----------------------------------
        indCrd = 0
        DO ista = 1, nstat
          IF ( resHed%filHead(ifil)%stanam(id) == stname(ista) ) THEN
            indCrd = ista
            EXIT
          ENDIF
        END DO

        IF ( indCrd == 0 ) THEN
          write(*,*) ' PG QLRSUM: Station not found in a priori coordinate file.'
          write(*,*) ' Station: ', resHed%filHead(ifil)%stanam(id)
        END IF


! Test the dimensions
! -------------------
        CALL dimtst(1,1,1,pgName,'maxsat','number of satellites',' ', &
                    nrsat,maxsat,irc)

        CALL dimtst(1,1,1,pgName,'maxsta','number of stations',' ', &
                    nrsta,maxsta,irc)

        CALL dimtst(1,1,1,pgName,'maxpts','number of points',' ', &
                    nrres(indx,indst),maxpts,irc)

! Check for shadow
! ----------------
        epoch = (resHed%filHead(ifil)%timref-timrf0) +  &
                 (iepoch-1)*resHed%filHead(ifil)%ideltt/864d2
        mjd   =  resHed%filHead(ifil)%timref         +  &
                 (iepoch-1)*resHed%filHead(ifil)%ideltt/864d2
        delta = 30/1440d0
        nsub  = 8
        ishd  = 0

      ! check orbit file
        DO iorbfil = 1, norbfil

        ! orbit file found
          IF (mjd >= stdHdr(iorbfil)%arc(1)%tbound(1) .AND. &
              mjd <= stdHdr(iorbfil)%arc(1)%tbound(2)) THEN

            CALL getorf(orbfil(1,iorbfil),satnum(indx),0,1,0,mjd, &
                        icrarc,iorsys,xsat,tosc,ele,irc)

! Save orbit vector for further use
! ---------------------------------
            xSat1(1:6) = xsat(1:6)

            IF (irc == 0) THEN
              ttt = (mjd-tosc)*86400d0
              CALL xyzele(gm,ttt,xsat,xsat(4),satnum(indx), &
                          ele(1),ele(2),ele(3),ele(4),ele(5),ele(6))

              DO isub = 0, nsub
                epoch1 = mjd-isub*delta/nsub
                ttt    = (epoch1-tosc)*86400d0
                CALL ephem(gm,ele(1),ele(2),ele(3), &
                           ele(4),ele(5),ele(6),ttt,xsat,xsat(4))
                CALL suneff(iorsys,2d0,epoch1,xsun,dumVec)
                CALL shadow(xsat,xsun,ishd)
                IF (ishd == 1 .AND. isub > 0) ishd = 2
                IF (ishd > 0) EXIT
              ENDDO

            ELSE
              ishd = -1
            ENDIF

            EXIT

        ! no orbit file
          ELSE IF (iorbfil == norbfil) THEN
            ishd = -1
            WRITE(lfnerr,'(A,F14.8)') &
                 ' ### PG QLRSUM: No orbit information found for epoch: ',mjd
          ENDIF

        ENDDO


! Add residual to list
! --------------------
        CALL jmt(mjd,year,month,day)

        nrres(indx,indst) = nrres(indx,indst) + 1
        resepo(nrres(indx,indst),indx,indst) = epoch
        ressat(nrres(indx,indst),indx,indst) = -res
        ishad(nrres(indx,indst),indx,indst)  = ishd

        WRITE(tmpstr,'(I9.9)') inpsat(2)
        READ(tmpstr,'(I5,I4)') iazi,iele
        resazi(nrres(indx,indst),indx,indst) = iazi/1d2
        resele(nrres(indx,indst),indx,indst) = iele/1d2

! Add observations during shadow passage to edit file structure
!  and keep index for outlier EDT file
! -------------------------------------------------------------
        IF ( (edtfl .AND. ishd > 0) .OR. &
             modEdt > 0                 ) THEN

          findex = ifil
          DO ii = 1, if-1
            findex = findex + numtot(ii)
          ENDDO

        ! Shadow if GPS satellite 5 or 6
          IF ( edtfl .AND. &
               (modsvn(satnum(indx)) == 5 .OR. modsvn(satnum(indx)) == 6) ) THEN
            CALL edtadd(1,findex,satnum(indx),3,0,mjd,edt)
          ENDIF

        ! Keep Index for outlier edit file
          IF  ( modEdt > 0 ) THEN
            resIndSat(nrres(indx,indst),indx,indst) = inpsat(1)
            resIndFil(nrres(indx,indst),indx,indst) = findex
          END IF

        ENDIF

! Compute azimuth and nadir angle in satellite-fixed system
! ---------------------------------------------------------
        CALL cootra(iorsys,1,mjd,xSat1,SZ,xpol,ypol,ut1gps)

        CALL topsta(xStat(1:3,indCrd), xSat1, SZ, 0d0, xpol, ypol, xTopo, &
                    DIST, ZEN, AZI, resHed%filHead(iFil)%meatyp, nadir,  &
                    aziSat, mjd, satnum(indx), iorsys, aziSat2, 0 )

        resAziSat(nrres(indx,indst),indx,indst) = aziSat2 * 180d0/pi
        resNadSat(nrres(indx,indst),indx,indst) = nadir * 180d0/pi

! Compute argument of latitude and beta angle
! -------------------------------------------
        CALL arglat(xSat1,agl)
        CALL mosupn(mjd,2,dumIn,dumIn,dumIn,dumOut,dumOut,dumOut,dumOut, &
                    dumMat,dumMat,sun,dumVec,dumOut,dumVec)
        dumVec(1:3) = 0D0
        CALL argsun(mjd,sun,dumVec,xSat1,satnum(indx),b0,dumOut,u0,dumOut)

        agl1= (agl-u0)*180d0/pi
        b0  = b0*180d0/pi
        IF (agl1 >  180.d0) agl1 = agl1-360.d0
        IF (agl1 < -180.d0) agl1 = agl1+360.d0

        resAglSat(nrres(indx,indst),indx,indst) = agl1
        resb0Sat(nrres(indx,indst),indx,indst) = b0

      ENDDO !Loop over id (nReSta)

    ENDDO ! End of residual file reached, read next file
    CLOSE(lfnres)

  ENDDO ! End loop over all residual files

! No requested satellite found in residual file
  IF (nrsat == 0) THEN
    WRITE(lfnerr,'(A,/)')                                                   &
      ' *** PG QLRSUM: No requested satellite found in the residual file(s).'
    CALL exitrc(2)
  ENDIF


! Convert satellite numbers: 122 -> R22
! -------------------------------------
  ALLOCATE(svnmod(nrsat), stat=irc)
  CALL alcerr(irc,'svnmod',(/nsat/),pgName)

  ALLOCATE(sattyp(nrsat), stat=irc)
  CALL alcerr(irc,'sattyp',(/nsat/),pgName)

  ALLOCATE(satmod(nrsat), stat=irc)
  CALL alcerr(irc,'satmod',(/nsat/),pgName)

  DO isat = 1, nrsat
    svnmod(isat) = modsvn(satnum(isat))
  ENDDO

  CALL svn2typ(nrsat,svnmod,satmod,sattyp)

  DO isat = 1, nrsat
    IF (sattyp(isat) == '') sattyp(isat) = 'G'
  ENDDO


! Determine statistics from the residuals per station and satellite
! -----------------------------------------------------------------
  itot = 0
  iout = 0

  WRITE(lfnprt,'(///,2(A,/),/,A,/,1X,78("-"))')&
    ' Residual Statistics',                    &
    ' -------------------',                    &
    ' SAT  STATION         #OBS       MEAN(mm)        RMS(mm)      SIGMA(mm)'

  DO ista = 1, nrsta

    DO isat = 1, nrsat

      IF (nrres(isat,ista) > 1) THEN
        CALL statis(nrres(isat,ista),ressat(1,isat,ista), &
                    xMean=xmean,xRms=xrms,xSigma=xsig)
      ELSE IF (nrres(isat,ista) == 1) THEN
        xmean = ressat(1,isat,ista)
        xrms  = 0d0
        xsig  = 0d0
      ENDIF

    ! Save station-/stellite-specific values for later use,
    ! and find station/satellite with largest RMS
    ! -----------------------------------------------------
      xrms2(isat,ista)  = xrms
      xsig2(isat,ista)  = xsig

      IF ( xrms > maxRMS .OR. &
           (xrms == maxRMS .AND. xsig > maxSig) ) THEN
        indSta = ista
        indSat = isat
        maxRMS = xrms
        maxSig = xsig
      ENDIF

      IF (nrres(isat,ista) > 0) THEN

        WRITE(lfnprt,'(1X,A1,I2.2,1X,A16,2X,I3,F15.1,F15.1,F15.1)') &
          sattyp(isat),satmod(isat),stalst(ista),nrres(isat,ista),  &
          xmean*1.d3, xrms*1.d3, xsig*1.d3

! Remove outliers for determination of overall statistics
! -------------------------------------------------------
        DO ires = 1, nrres(isat,ista)

          IF (DABS(ressat(ires,isat,ista)) < 3d0*edtlvl) THEN
            itot = itot+1
            total(itot) = ressat(ires,isat,ista)
          ELSE
            iout = iout + 1
          ENDIF

        ENDDO

      ENDIF

    ENDDO
    WRITE(lfnprt,*)

  ENDDO

! Determine overall statistics using loop with "xsig*sigfac" outlier detect.
! --------------------------------------------------------------------------
  IF (itot > 1) THEN
    CALL statis(itot,total,xMean=xmeant,xSigma=xsigt)
  ENDIF

  ntotsav = itot + iout
  ntot    = itot

  DO
    iter = 0
    itot = 0
    IF (xsigt > sigmax) xsigt = sigmax

    DO ll = 1, ntot
      IF((DABS(total(ll)-xmeant) < edtlvl) .AND.     &
         (DABS(total(ll)-xmeant) < sigfac*xsigt)) THEN
        itot = itot + 1
        total(itot) = total(ll)
      ELSE
        iter = 1
      ENDIF
    ENDDO

    IF (itot > 1) THEN
      CALL statis(itot,total,xMean=xmeant,xRms=xrms,xSigma=xsigt)
    ELSE IF (itot == 1) THEN
      xmeant = total(1)
      xrms   = 0d0
      xsigt  = 0d0
    ELSE
      xmeant = 0d0
      xrms   = 0d0
      xsigt  = 0d0
    ENDIF

    IF (iter /= 0) THEN
      ntot = itot
    ELSE
      EXIT ! exit loop
    ENDIF

  ENDDO

! Write overall statistic
! -----------------------
  WRITE(lfnprt,'(1X,78("-"),/,A,I5,F15.1,F15.1,F15.1,//)') &
    ' Total               ',ntot,xmeant*1.d3, xrms*1.d3, xsigt*1.d3

! Generate slr statistics per station and satellite pass if requested
! -------------------------------------------------------------------
! Open summary file
  CALL gtflna(0,'SUMFIL',filnam,irc002)
  IF (irc002 == 0) THEN
    CALL opnfil(lfn002,filnam,'UNKNOWN','FORMATTED',' ',' ',irc )
    CALL opnerr(lfnerr,lfn002,irc,filnam,pgName)

  ! Open skeleton file for slrmail
    CALL gtflna(1,'SKLFIL',filnam,irc)
    IF (irc == 0) THEN
      CALL opnfil(lfn001,filnam,'UNKNOWN','FORMATTED',' ',' ',irc )
      CALL opnerr(lfnerr,lfn001,irc,filnam,pgName)
      DO
        READ(lfn001,'(A)',iostat=ios)line
        IF (ios /= 0) EXIT
        WRITE(lfn002,'(A)') TRIM(line)
      ENDDO
      CLOSE(lfn001)
    ENDIF

    WRITE(lfn002,'(/,A,A,/,A,"yy/mm/dd",A,/,1X,78("-"))')               &
      ' STATION ID       SAT  START PASSAGE   DUR   #OBS  MEAN  STD  ', &
      ' #OBS  MEAN   STD',                                              &
      '                  PRN  ',                                        &
      ' hh:mm (min)  GOOD  (mm) (mm)    BAD   (m)   (m)'
  ENDIF

! Detailed residual output for Van Husson
! ---------------------------------------
  CALL gtflna(0,'ALLFIL ',filnam,irc003)
  IF (irc003 == 0) THEN
    lfn003 = lfn002 + 1
    CALL opnfil(lfn003,filnam,'UNKNOWN','FORMATTED',' ',' ',irc)
    CALL opnerr(lfnerr,lfn003,irc,filnam,pgName)

    WRITE(lfn003,'(/,A,A,/,A,A,/,1X,110("-"))')                       &
      ' STATION ID       SAT  START PASSAGE   DUR   #OBS  RESIDUAL ', &
      '   AZI     ELE     AZI-SAT   NAD       AGL      B',            &
      '                  PRN  yy/mm/dd hh:mm (min)        (mm)     ', &
      '  (deg)   (deg)     (deg)   (deg)     (deg)   (deg)'

  ENDIF

! Residual plot file for graphic tools (gnuplot)
! ----------------------------------------------
  CALL gtflna(0,'PLTFIL ',filnam,ircplt)
  IF (ircplt == 0) THEN
    CALL opnfil(lfnplt,filnam,'UNKNOWN','FORMATTED',' ',' ',irc)
    CALL opnerr(lfnerr,lfnplt,irc,filnam,pgName)
    WRITE(lfnplt,'(3(/,A))')                                                &
      '# STATION ID       SAT   EPOCH             RESIDUAL    AZI     ELE', &
      '#                  PRN   (mjd)             (mm)       (deg)   (deg)',&
      '# -----------------------------------------------------------------'
  ENDIF

! Set maximum sigma
! -----------------
  sigmax = xsigt

! Loop over all stations
! ----------------------
  DO ista = 1, nrsta

! Loop over all satellites
! ------------------------
    DO isat = 1, nrsat
      ipass  = 0
      iobs   = 0
      istart = 1

! Loop over all residuals of this station and satellite
! -----------------------------------------------------
      DO ires = 1, nrres(isat,ista)

! Shadow flag
! -----------
        IF (ishad(ires,isat,ista) ==  0) cshad = ' '
        IF (ishad(ires,isat,ista) ==  1) cshad = 'E'  ! in shadow
        IF (ishad(ires,isat,ista) ==  2) cshad = 'E'  ! after returning into sunlight (up to 30min)
        IF (ishad(ires,isat,ista) == -1) cshad = 'X'


! Check edit requests
! -------------------
        IF ( modEdt > 0 ) THEN

        ! Variante 1: Edit only station/satellite with largest RMS
        ! --------------------------------------------------------
          IF ( modEdt == 1                                        .AND. &
               indSta == ista .AND. indSat == isat                .AND. &
               xrms2(isat,ista)*1d3 > sigsav                      .AND. &
               (DABS(ressat(ires,isat,ista)) > edtlvl             .AND. &
                DABS(ressat(ires,isat,ista)) > sigFac*xsig2(isat,ista) ) ) THEN

            nEdtTot = nEdtTot + 1
            nEdt(isat,ista) = nEdt(isat,ista) + 1

            mjd = resepo(ires,isat,ista) + timrf0

            CALL edtadd(1,resIndFil(ires,isat,ista), &
                        resIndSat(ires,isat,ista),3,0,mjd,edtOut)

          ENDIF

        ! Variante 2: Edit all residuals larger than specifications
        ! ---------------------------------------------------------
          IF ( modEdt == 2 ) THEN

            edit = .FALSE.

            IF ( (nrres(isat,ista) == 1  .OR. sigsav == 0d0) .AND. &
                 DABS(ressat(ires,isat,ista)) > edtlvl             ) &
              edit = .TRUE.

            IF ( nrres(isat,ista) > 1          .AND. &
                 xrms2(isat,ista)*1d3 > sigsav                      .AND. &
                 (DABS(ressat(ires,isat,ista)) > edtlvl             .OR.  &
                  DABS(ressat(ires,isat,ista)) > sigFac*xsig2(isat,ista) ) )  &
               edit = .TRUE.

!!!               xrms2(isat,ista)*1d3 > sigsav  .AND. &
!!!               (DABS(ressat(ires,isat,ista)) > edtlvl             .OR.  &
!!!                DABS(ressat(ires,isat,ista)) > sigFac*xsig2(isat,ista) ) ) THEN

            IF  ( edit ) THEN
              nEdtTot = nEdtTot + 1
              nEdt(isat,ista) = nEdt(isat,ista) + 1

              mjd = resepo(ires,isat,ista) + timrf0

              CALL edtadd(1,resIndFil(ires,isat,ista), &
                          resIndSat(ires,isat,ista),3,0,mjd,edtOut)
            ENDIF

          ENDIF

        ENDIF  ! modEdt



! If summary file requested
! -------------------------
        IF (irc002 == 0) THEN

          iobs         = iobs + 1
          respas(iobs) = ressat(ires,isat,ista)

! If end of pass or end of residuals --> start editting
! -----------------------------------------------------
          IF ((resepo(ires+1,isat,ista)-resepo(ires,isat,ista)>8D0/24D0) .OR.&
             (ires == nrres(isat,ista))) THEN
            ipass = ipass + 1
            tend  = resepo(ires,isat,ista)

! Check for maxpas
! ----------------
            CALL dimtst(1,1,1,pgName,'maxpas','number of passes',' ', &
                 ipass,maxpas,irc)

! Remove outliers from the data
! -----------------------------
            nobs = 0
            nbad = 0
            DO ll = 1, iobs
              IF((DABS(respas(ll)-xmeant) < edtlvl) .AND. &
                 (DABS(respas(ll)-xmeant) < sigfac*sigmax)) THEN
                nobs         = nobs + 1
                respas(nobs) = respas(ll)
              ELSE
                nbad         = nbad + 1
                resedt(nbad) = respas(ll)
              ENDIF
            ENDDO

            IF (nobs > 1) THEN
              CALL statis(nobs,respas,xMean=xmean,xRms=xrms,xSigma=xsig)
            ELSE IF (nobs == 1) THEN
              xmean = respas(1)
              xrms  = 0d0
              xsig  = 0d0
            ELSE
              xmean = 0d0
              xrms  = 0d0
              xsig  = 0d0
            ENDIF

            IF (nbad > 1) THEN
              CALL statis(nbad,resedt,xMean=xmeab,xRms=xrmb,xSigma=xsib)
            ELSE IF (nbad == 1) THEN
              xmeab = resedt(1)
              xrmb  = DSQRT(resedt(1)**2)
              xsib  = 0d0
            ENDIF

            pmean(ipass) = IDNINT(xmean*1d3)
            psig(ipass)  = IDNINT(xsig*1d3)

            imeab  = IDNINT(xmeab)
            isib   = IDNINT(xsib)
            tstart = timrf0+resepo(istart,isat,ista)
            dt     = (tend-resepo(istart,isat,ista))*24d0*60d0
            idt    = IDNINT(dt)

            CALL jmt(tstart,iy,im,dd)
            IF (iy >= 2000) THEN
              iy = iy - 2000
            ELSE
              iy = iy - 1900
            ENDIF

            id = IDINT(dd)
            hh = (dd-id)*24d0
            ih = IDINT(hh)
            dm = (hh-ih)*60d0
            mm = IDINT(dm)

            IF (nbad > 0) THEN
              WRITE(lfn002,'(1X,A16,1X,A1,I2.2,A1,1X,I2.2,"/",I2.2,"/",I2.2,1X,&
              &              I2.2,":",I2.2,1X,I4,2X,I4,2X,2I5,2I6,I7)')        &
              stalst(ista),sattyp(isat),satmod(isat),cshad,iy,im,id,ih,mm,   &
              idt,nobs,pmean(ipass),psig(ipass),nbad,imeab,isib
            ELSE
              WRITE(lfn002,'(1X,A16,1X,A1,I2.2,A1,1X,I2.2,"/",I2.2,"/",I2.2,1X,&
              &              I2.2,":",I2.2,1X,I4,2X,I4,2X,2I5)')               &
              stalst(ista),sattyp(isat),satmod(isat),cshad,iy,im,id,ih,mm,   &
              idt,nobs,pmean(ipass),psig(ipass)
            ENDIF

            istart = ires + 1
            iobs   = 0

! End of pass editting
! --------------------
          ENDIF

! End of writing summary file
! ---------------------------
        ENDIF

! Writing of all residuals
! ------------------------
        IF (irc003 == 0) THEN

          tstart = timrf0 + resepo(ires,isat,ista)
          CALL jmt(tstart,iy,im,dd)
          IF (iy >= 2000) THEN
            iy = iy - 2000
          ELSE
            iy = iy - 1900
          ENDIF
          id = IDINT(dd)
          hh = (dd-id)*24d0
          ih = IDINT(hh)
          dm = (hh-ih)*60d0
          mm = IDINT(dm)

          IF (DABS(ressat(ires,isat,ista)*1d3) > 2d9) THEN
            WRITE(lfn003,'(1X,A16,1X,A1,I2.2,2X,I2.2,"/",I2.2,"/",I2.2,1X,I2.2, &
               &              ":",I2.2,1X,I4,2X,I4,1X,"******",4X,2(1X,F7.2),   &
               &              2X,2(1X,F7.2),2X,2(1X,F7.2))')                    &
               stalst(ista),sattyp(isat),satmod(isat),iy,im,id,ih,mm,0,1,       &
               resazi(ires,isat,ista), resele(ires,isat,ista) ,                 &
               resAziSat(ires,isat,ista), resNadSat(ires,isat,ista),            &
               resAglSat(ires,isat,ista), resb0Sat(ires,isat,ista)

          ELSE
            WRITE(lfn003,'(1X,A16,1X,A1,I2.2,2X,I2.2,"/",I2.2,"/",I2.2,1X,I2.2, &
               &              ":",I2.2,1X,I4,2X,I4,1X,I6,4X,2(1X,F7.2),         &
               &              2X,2(1X,F7.2),2X,2(1X,F7.2))')                    &
               stalst(ista),sattyp(isat),satmod(isat),iy,im,id,ih,mm,0,1,       &
               IDNINT(ressat(ires,isat,ista)*1d3),                              &
               resazi(ires,isat,ista), resele(ires,isat,ista),                  &
               resAziSat(ires,isat,ista), resNadSat(ires,isat,ista),            &
               resAglSat(ires,isat,ista), resb0Sat(ires,isat,ista)

          ENDIF

        ENDIF

! Writing of all residuals into plot file
! ---------------------------------------
        IF (ircplt == 0) THEN
          tstart = timrf0 + resepo(ires,isat,ista)
          WRITE(lfnplt,'(1X,A16,2X,A1,I2.2,1A,2X,F16.10,1X,F8.1,2X,2(1X,F7.2))')&
            stalst(ista),sattyp(isat),satmod(isat),cshad,tstart,                &
            (ressat(ires,isat,ista)*1d3),resazi(ires,isat,ista),                &
            resele(ires,isat,ista)
        ENDIF

! End residual loop
! -----------------
      ENDDO


! Test minimum number of observations: per station and per satellite
! ------------------------------------------------------------------
      IF ( chkFlg .AND. modChk == 2 ) THEN

        IF ( nrres(isat,ista) == 0 .OR.                   &
             nrres(isat,ista) - nEdt(isat,ista) >= minObs  ) CYCLE

        DO ires = 1, nrres(isat,ista)
          mjd = resepo(ires,isat,ista) + timrf0

          CALL edtadd(1,resIndFil(ires,isat,ista), &
                      resIndSat(ires,isat,ista),3,0,mjd,edtOut)
        END DO

        WRITE(lfnprt,'(A,A20,2X,A1,I2.2,3X,I6)')       &
              ' TOO FEW OBSERVATIONS: ',               &
              stalst(ista), sattyp(isat),satmod(isat), &
              (nrres(isat,ista)-nEdt(isat,ista))

      END IF


! End satellite loop
! ------------------
    ENDDO

    IF (irc002 == 0) WRITE(lfn002,*)
    IF (irc003 == 0) WRITE(lfn003,*)
    IF (ircplt == 0) WRITE(lfnplt,'(/)')


! Test minimum number of observations: per station
! ------------------------------------------------
    IF ( chkFlg .AND. modChk == 1 ) THEN

      IF ( SUM(nrres(:,ista)) == 0 .OR.                     &
           SUM(nrres(:,ista)) - SUM(nEdt(:,ista)) >= minObs  ) CYCLE

      DO isat = 1, nrsat
        DO ires = 1, nrres(isat,ista)
          mjd = resepo(ires,isat,ista) + timrf0

          CALL edtadd(1,resIndFil(ires,isat,ista), &
                      resIndSat(ires,isat,ista),3,0,mjd,edtOut)
        END DO
      END DO

      WRITE(lfnprt,'(A,A20,2X,I6)')                  &
            ' TOO FEW OBSERVATIONS: ', stalst(ista), &
            (SUM(nrres(:,ista))-SUM(nEdt(:,ista)))

    END IF

! End station loop
! ----------------
  ENDDO

  IF (irc002 == 0) THEN
    WRITE(lfn002,'(1X,78("-"))')

! Determine mean and sigma (after editting)
! -----------------------------------------
    imeant = IDNINT(xmeant*1d3)
    isigt  = IDNINT(xsigt*1d3)

    IF (sigsav > 1000) THEN
      WRITE(lfn002,'(42X,I6,I7,I5)')ntot,imeant,isigt
    ELSE
      edtp = (ntotsav-ntot)*1d2/(ntotsav*1d0)
      WRITE(lfn002,'(10X,F6.1,F9.1,F8.1,9X,I6,I7,I5,F10.1,"%")') &
        edtlvl,sigsav,sigfac,ntot,imeant,isigt,edtp
    ENDIF
  ENDIF

! Write edit file if requested
! - observations during shadow passage + 30 min
! - marking outliers
! ---------------------------------------------
  IF (edtfl) CALL wtedit2(edt)

  IF ( modEdt > 0 .AND. irc2 == 0 ) THEN

    CALL wtedit2(edtOut,filEdt)

    DEALLOCATE(resIndFil,stat=irc)
    DEALLOCATE(resIndSat,stat=irc)

    WRITE(lfnprt,'(//,A,I6,/)') &
          ' NUMBER OF EDIT REQUESTS: ', nEdtTot

  END IF

! Close all files
! ---------------
  CALL inquire(UNIT=lfn002,OPENED=open)
    IF (open) CLOSE(lfn002)
  CALL inquire(UNIT=lfn003,OPENED=open)
    IF (open) CLOSE(lfn003)
  CALL inquire(UNIT=lfnplt,OPENED=open)
    IF (open) CLOSE(lfnplt)


  CALL exitrc(0)

END PROGRAM qlrsum
