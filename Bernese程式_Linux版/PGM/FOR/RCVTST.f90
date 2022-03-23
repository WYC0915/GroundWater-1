! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM rcvtst

! -------------------------------------------------------------------------
! Purpose:    Test receiver performance based on percentage of solved
!             ambiguities on baselines.
!
! Author:     U. Hugentobler
!
! Created:    11-May-2002
!
! Changes:    17-Feb-2003 LM: Use m_maxdim
!             27-Feb-2003 HU: Replace char by chr
!             17-Mar-2003 RD: Use structure for station abbreviations
!             26-Mar-2003 RD: Only one warning per abbreviation set
!             14-Apr-2003 SS: Do not stop when start of table not found
!             14-Apr-2003 RD: Use data structure for session table
!             17-Apr-2003 RD: Correct index for nPar in printing section
!             23-Apr-2003 HU: Nullify local pointers
!             16-May-2003 AJ: Initialize structure
!             20-May-2003 HU: Get year from YR4_INFO
!             30-Jun-2003 RD: Adapt to title line in MR-Version
!             29-Oct-2003 HB: Correct format statement
!             03-Nov-2003 RD: Call SR gtabbv instead of SR getabb
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             27-Feb-2007 AG: Call DEFCON with parameter
!             23-Sep-2010 RD: Enable CPU counter
!             02-Nov-2010 SL: use undef_c, use m_bern with ONLY
!             12-Mar-2012 RD: Use LISTC1 as module now
!             10-Jul-2012 RD: Use syminvg instead of symin8
!             20-Aug-2012 SL: Shape of listc1 parameter changed
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnRes, lfnErr, lfn001, lfnPrt, &
                      linelength, fileNameLength, keyvaluelength, &
                      program_name
  USE m_cpu,    ONLY: cpu_start
  USE m_maxdim, ONLY: maxrec, maxsta
  USE m_time,   ONLY: t_timint
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE d_sess,   ONLY: t_sesLst,init_sesLst
  USE d_stacrx, ONLY: t_stacrux,init_stacrux,undef_c
  USE d_abbrev, ONLY: t_abbrev,init_abbrev

  USE s_gtfile2
  USE s_gtabbv
  USE s_dordup
  USE s_opnfil
  USE s_prflna
  USE s_rdsess
  USE s_readcrux
  USE s_readabb
  USE s_pritit
  USE s_readinpf
  USE s_opnerr
  USE s_prfile
  USE s_syminvg
  USE s_sestim
  USE s_readkeys
  USE s_defcon
  USE s_exitrc
  USE s_ckoptb
  USE s_opnsys
  USE s_ckopti
  USE s_gtflna
  USE f_ikf
  USE f_listc1

  IMPLICIT NONE

! Local variables
! ---------------
  CHARACTER(LEN=fileNameLength)  :: filnam
  CHARACTER(LEN=fileNameLength)  :: filses
  CHARACTER(LEN=fileNameLength)  :: filabb
  CHARACTER(LEN=fileNameLength), &
          DIMENSION(:,:),POINTER :: filinp
  CHARACTER(LEN=keyValueLength), &
          DIMENSION(:), POINTER  :: keyValue

  TYPE(t_sesLst)                     :: sesTbl
  TYPE(t_stacrux)                    :: staCrx
  TYPE(t_abbrev)                     :: abbrev
  TYPE(t_timint)                     :: sesInt

  INTEGER(i4b)                       :: nStaAbb
  INTEGER(i4b), DIMENSION(:),POINTER :: abbIdx
  INTEGER(i4b)                       :: irc, irCode, iostat, ircres
  INTEGER(i4b)                       :: ii, jj, ij, it
  INTEGER(i4b)                       :: nsta, nrec
  INTEGER(i4b)                       :: year
  INTEGER(i4b)                       :: iord, niter, iter
  INTEGER(i4b)                       :: nfil, ifil
  INTEGER(i4b)                       :: jsta, jrec, nobs, ising
  INTEGER(i4b),DIMENSION(2)          :: ista, irec, kk=(/2,1/), npar
  INTEGER(i4b),DIMENSION(2,2)        :: ipar
  INTEGER(i4b),DIMENSION(6,maxsta,2) :: locq
  INTEGER(i4b),DIMENSION(maxsta)     :: ix,iy

  REAL(r8b)                          :: frac, tsess
  REAL(r8b),DIMENSION(2)             :: dfrac, rms
  REAL(r8b),DIMENSION(maxsta,2)      :: bnor, par, dpar, err
  REAL(r8b),DIMENSION((maxsta*(maxsta+1))/2,2) :: anor

  CHARACTER(LEN=lineLength)          :: line
  CHARACTER(LEN=89)                  :: MWW, MWN, QIF
  CHARACTER(LEN=6)                   :: pgmnam='RCVTST'
  CHARACTER(LEN=4)                   :: sess
  CHARACTER(LEN=2),DIMENSION(2)      :: abb2
  CHARACTER(LEN=4),DIMENSION(maxsta) :: stanam
  CHARACTER(LEN=20),DIMENSION(maxrec):: recnam


! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Nullify pointers
! ----------------
  CALL init_sesLst(sesTbl)
  CALL init_stacrux(staCrx)
  CALL init_abbrev(abbrev)
  NULLIFY(filinp)
  NULLIFY(keyValue)
  NULLIFY(abbIdx)

! Program name
! ------------
  program_Name='RCVTST'

! Get the name of the input file
! ------------------------------
  CALL init_inpkey(inpKey)
  CALL readinpf(' ',inpKey)

! Define system files
! -------------------
  CALL opnsys

! Define constants
! ----------------
  CALL defcon(1)

  MWW = ' File       Length   #Amb  RMS0   Max/RMS L5 Amb   #Amb  &
         &RMS0   #Amb Res'
  MWN = ' File       Length   #Amb  RMS0   Max/RMS L3 Amb   #Amb  &
         &RMS0   #Amb Res'
  QIF = ' File       Length   #Amb  RMS0   Max/RMS L5 Amb  Max/RMS &
         &L3 Amb   #Amb  RMS0   #Amb Res'

! Title and file names
! --------------------
  CALL pritit(pgmnam, 'Check Receiver Performance')
  CALL prflna
  CALL prfile('ARSUM','Ambiguity MW Wide Lane Summaries',1)

! Get input options
! -----------------
  irCode=0
  CALL readKeys('YR4_INFO', keyValue, irc)
  CALL ckopti(1,'YR4_INFO', keyValue, 'pg rcvtst', 'session year', &
                 irc, irCode,result1=year)

  CALL ckoptb(1,(/'ORDER'/), 'pg rcvtst', 'order results', &
                 irCode, result1=iord)

  CALL readKeys('NITER', keyValue, irc)
  CALL ckopti(1,'NITER', keyValue, 'pg rcvtst', 'number of iterations', &
                 irc, irCode, ge=1, result1=niter)

  IF (irCode/=0) CALL exitrc(2)

! Get input files
! ---------------
  CALL gtfile2('ARSUM', 1, nfil, filinp)

! Get sessions
! ------------
  CALL gtflna(1,'SESSION_TABLE',filSes,irc)
  CALL rdsess(filSes,sesTbl)
  IF (irc/=0) CALL exitrc(2)

! Get abbreviations
! -----------------
  CALL gtflna(1,'ABBREV',filabb,irc)
  CALL readAbb(filabb,abbrev)
  NULLIFY(abbIdx)
  IF (irc/=0) CALL exitrc(2)
! Special AIUB-option because of "station - ABC"
  DO ii=1,abbrev%nAbb
    abbrev%abb(ii)%stanam(15:15)=' '
    abbrev%abb(ii)%remark=' '
  ENDDO

! Get stainfo
! -----------
  CALL gtflna(1,'RNXINFO',filnam,irc)
  CALL readCrux(filnam, staCrx)
  DO ii=1,staCrx%ninfo
    stacrx%stainfo(ii)%stanam(15:15)=' '
  ENDDO

! Open residual file
! ------------------
  CALL gtflna(0,'RESID',filnam,ircres)
  IF (ircres==0) THEN
    CALL opnfil(lfnres,filnam,'UNKNOWN','FORMATTED',' ',' ',iostat)
    CALL opnerr(lfnerr,lfnres,iostat,filnam,pgmnam)
  ENDIF

! Loop over iterations
! --------------------
  nsta=0
  nrec=0
  npar=0
  par =0D0
  locq=0

  IterationLoop: Do iter=0,niter
    nobs=0
    rms =0D0
    bnor=0D0
    anor=0D0

! Loop over input list files
! --------------------------
    FileLoop: DO ifil=1,nfil
      CALL opnfil(lfn001,filinp(1,ifil),'OLD','FORMATTED','READONLY',' ',iostat)
      CALL opnerr(lfnerr,lfn001,iostat,filinp(1,ifil),pgmnam)

! Start of table
      DO
        READ(lfn001,"(A)",IOSTAT=iostat) line
        IF (iostat/=0) THEN
          WRITE(lfnerr,"(/,' ### PG ',A6,': Start of table not found', &
                       & /,'                File: ',A,/)") &
                       pgmnam,TRIM(filinp(1,ifil))
          CLOSE(lfn001)
          CYCLE FileLoop
        ENDIF
        IF (index(line,TRIM(mww)) /= 0) EXIT
      ENDDO
      READ(lfn001,"(//)")

      ReadLoop: DO
! Read line
        READ(lfn001,"(A)",IOSTAT=iostat) line
        IF (iostat/=0) THEN
          WRITE(lfnerr,"(/,' *** PG ',A6,': Error reading file', &
                       & /,'                File: ',A,/)") &
                       pgmnam,TRIM(filinp(1,ifil))
          CALL exitrc(2)
        ENDIF
        IF (line(2:9) == '--------') EXIT ReadLoop

        READ(line,"(1X,A2,A2,A4,52X,F8.0)",IOSTAT=iostat) &
             abb2(1),abb2(2),sess,frac
        IF (iostat/=0) THEN
          WRITE(lfnerr,"(/,' *** PG ',A6,': Error decoding line', &
                       & /,'                File: ',A, &
                       & /,'                Line: ',A,/)") &
                       pgmnam,TRIM(filinp(1,ifil)),TRIM(line)
          CALL exitrc(2)
        ENDIF
        frac=frac/100D0

! get session start
        CALL sestim(filSes,sesTbl,sess,year,sesInt%t)

        IF (sesInt%t(1) == 0d0 .OR. sesInt%t(2) == 1d20) THEN
          CALL EXITRC(2)
        ELSE
          tSess = (sesInt%t(1)+sesInt%t(2))/2d0
        ENDIF

! Get station names
        BaslinLoop: DO ii=1,2
          CALL gtabbv(0,abb2(ii),3,filAbb,abbrev,nStaAbb,abbIdx)
          IF (nStaAbb == 0) THEN
            WRITE(lfnerr,"(/,' *** PG ',A6,': Station not found in abb table', &
                         & /,'                Abbreviation: ',A,/)") &
                         pgmnam,abb2(ii)
            CALL exitrc(2)
          ELSE IF (nStaAbb > 1) THEN
            IF (LEN_TRIM(abbrev%abb(abbIdx(nStaAbb))%remark) == 0) THEN
              WRITE(lfnerr,"(/,' ### PG ',A6,': More than one station found ', &
                           &                   'in abb table', &
                           & /,'                Abbreviation: ',A,/)") &
                           pgmnam,abb2(ii)
              abbrev%abb(abbIdx(nStaAbb))%remark = 'warning wrote'
            ENDIF
            iSta(ii) = abbIdx(nStaAbb)
          ELSE
            iSta(ii) = abbIdx(nStaAbb)
          ENDIF

! get receiver
          irec(ii)=0
          DO jj=1,staCrx%ninfo
            IF (abbrev%abb(ista(ii))%stanam==stacrx%stainfo(jj)%stanam  .AND. &
                tsess+SPACING(tsess)>=stacrx%stainfo(jj)%timint%t(1)    .AND. &
                tsess               <=stacrx%stainfo(jj)%timint%t(2)    .AND. &
                stacrx%stainfo(jj)%recnam /= undef_c) THEN
              irec(ii)=jj
!!!           EXIT    take last entry
            ENDIF
          ENDDO
          IF (irec(ii)==0) THEN
            WRITE(lfnerr,"(/,' *** PG ',A6,': Station not found in stainfo file (TYPE 2)', &
                         & /,'                Station: ',A, &
                         & /,'                Session: ',A,'/',I4)") &
                         pgmnam,abbrev%abb(ista(ii))%stanam,sess,year
            CALL exitrc(2)
          ENDIF

! Setup parameter list
! locq:  1: typ, 1: receiver, 2: station
!        2: index in abbrev table
!        3: index in stainfo table (TYPE 2)
!        4: station name index
!        5: receiver name index
!        6: number of observations per receiver
!
! find station and receiver name
          jsta=listc1(1, 4,maxsta,stanam,abbrev%abb(ista(ii))%stanam(1:4),nsta)
          jrec=listc1(1,20,maxrec,recnam,stacrx%stainfo(irec(ii))%recnam,nrec)

          TypeLoop1: DO it=1,2
! find parameter list
            ipar(ii,it)=0
            DO jj=1,npar(it)
              IF (it==1 .AND. locq(5,jj,it)==jrec .OR. &
                  it==2 .AND. locq(4,jj,it)==jsta .AND. &
                              locq(5,jj,it)==jrec) THEN
                ipar(ii,it)=jj
                EXIT
              ENDIF
            ENDDO

! add paraameter to list
            IF (ipar(ii,it)==0 .AND. iter==0) THEN
              npar(it)=npar(it)+1
              IF (npar(it)>maxsta) THEN
                WRITE(lfnerr,"(/,' *** PG ',A6,': Too many parameters', &
                             & /,'                Maxpar: ',I6,/)") pgmnam,maxsta
                CALL exitrc(2)
              ENDIF

              locq(1,npar(it),it)=1
              locq(2,npar(it),it)=ista(ii)
              locq(3,npar(it),it)=irec(ii)
              locq(4,npar(it),it)=jsta
              locq(5,npar(it),it)=jrec

              ipar(ii,it)=npar(it)
            ELSEIF (ipar(ii,it)==0) THEN
              WRITE(lfnerr,"(/,' *** PG ',A6,': This error cannot occur!'/)") pgmnam
              CALL exitrc(2)
            ENDIF

            IF (iter==0) THEN
              locq(6,ipar(ii,it),it)=locq(6,ipar(ii,it),it)+1
            ENDIF
          ENDDO TypeLoop1
        ENDDO BaslinLoop

! Update normal equation system
! -----------------------------
        TypeLoop2: DO it=1,2
          IF (iter==0) THEN
            IF (frac>0D0) THEN
              dfrac(it)=DLOG(frac)
            ELSE
              dfrac(it)=-10
            ENDIF
          ELSE
            dfrac(it)=frac-par(ipar(1,it),it)*par(ipar(2,it),it)
          ENDIF

          IF (it==1) nobs = nobs+1
          rms(it) = rms(it)+dfrac(it)*dfrac(it)

          IF (iter==0) THEN
            DO ii=1,2
              bnor(ipar(ii,it),it) = bnor(ipar(ii,it),it) + dfrac(it)
              DO jj=ii,2
                ij=ikf(ipar(ii,it),ipar(jj,it))
                anor(ij,it) = anor(ij,it) +1D0
              ENDDO
            ENDDO
          ELSE
            DO ii=1,2
              bnor(ipar(ii,it),it) = bnor(ipar(ii,it),it) + &
                                     par(ipar(kk(ii),it),it)*dfrac(it)
              DO jj=ii,2
                ij=ikf(ipar(ii,it),ipar(jj,it))
                anor(ij,it) = anor(ij,it) + &
                              par(ipar(kk(ii),it),it)*par(ipar(kk(jj),it),it)
              ENDDO
            ENDDO
          ENDIF

! write residuals
          IF (it==1 .AND. ircres==0 .AND. iter==niter) THEN
            WRITE(lfnres,"(A2,A2,A4,4I4,2F8.4,4(2x,A))")     &
                  abb2(1),abb2(2),sess,                       &
                  (locq(4,ipar(jj,it),it),jj=1,2),                  &
                  (locq(5,ipar(jj,it),it),jj=1,2),                  &
                  frac,dfrac(it),                                 &
                  (stanam(locq(4,ipar(jj,it),it)),jj=1,2),          &
                  (recnam(locq(5,ipar(jj,it),it)),jj=1,2)
          ENDIF
        ENDDO TypeLoop2

      ENDDO ReadLoop
      CLOSE(lfn001)
    ENDDO FileLoop

! Solve normal equation system
! ----------------------------
    TypeLoop3: DO it=1,2
      CALL syminvg(npar(it),anor(:,it),0,ising)

      DO ii=1,npar(it)
        dpar(ii,it)=0D0
        DO jj=1,npar(it)
          dpar(ii,it)=dpar(ii,it)+anor(ikf(ii,jj),it)*bnor(jj,it)
        ENDDO
        par(ii,it) = par(ii,it)+dpar(ii,it)
        IF (iter==0) par(ii,it)=DEXP(par(ii,it))
      ENDDO

      IF (iter==niter) THEN
        rms(it)=rms(it)-DOT_PRODUCT(bnor(1:npar(it),it),dpar(1:npar(it),it))
        IF (rms(it) > 0D0 .AND. nobs-npar(it)>0) THEN
          rms(it)=DSQRT(rms(it)/(nobs-npar(it)))
        ELSE
          rms(it)=0D0
        ENDIF

        DO ii=1,npar(it)
          err(ii,it)=rms(it)*DSQRT(MAX(anor(ikf(ii,ii),it),0D0))
        ENDDO
      ENDIF
    ENDDO TypeLoop3

! print results
! -------------
    IF (iter==niter) THEN
      WRITE(lfnprt,"(/,' RMS:  ',f8.4,/)") rms(1)

! order results
      IF (iord==0) THEN
        DO ii=1,npar(1)
          ix(ii)=ii
        ENDDO
        DO ii=1,npar(2)
          iy(ii)=ii
        ENDDO
      ELSE
        CALL dordup(par(1:npar(1),1),npar(1),ix)
        CALL dordup(par(1:npar(2),2),npar(2),iy)
      ENDIF


! print receiver specific parameters
      WRITE(lfnprt,"(1X,79('-'), &
             & /,'       Receiver                fac       err       corr      num', &
             & /,1X,79('-'))")
      DO ii=npar(1),1,-1
        WRITE(lfnprt,"(7X,A,3f10.4,I7)") recnam(locq(5,ix(ii),1)), &
               par(ix(ii),1),err(ix(ii),1),dpar(ix(ii),1),locq(6,ix(ii),1)
      ENDDO
      WRITE(lfnprt,"(1X,79('-'),/)")

      WRITE(lfnprt,"(/,' RMS:  ',f8.4,/)") rms(2)

      WRITE(lfnprt,"(1X,79('-'), &
             & /,' Sta   Receiver                fac       err       corr      num     diff', &
             & /,1X,79('-'))")
      DO jj=nrec,1,-1
        DO ii=npar(2),1,-1
          IF (locq(5,iy(ii),2)==ix(jj)) THEN
            WRITE(lfnprt,"(1X,A,2X,A,3f10.4,I7,F11.4)") &
                 stanam(locq(4,iy(ii),2)),recnam(locq(5,iy(ii),2)), &
                 par(iy(ii),2),err(iy(ii),2),dpar(iy(ii),2),locq(6,iy(ii),2), &
                 par(iy(ii),2)-par(locq(5,iy(ii),2),1)
          ENDIF
        ENDDO
      ENDDO
      WRITE(lfnprt,"(1X,79('-'),/)")
    ENDIF

  ENDDO IterationLoop

  IF (ircres == 0) CLOSE (lfnres)

! End
! ---
  CALL exitrc(0)
END PROGRAM rcvtst
