MODULE s_DCBSTORE
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE dcbstore(neq)

! -------------------------------------------------------------------------
! Purpose:    Save DCB results computed by ADDNEQ2
!
! Author:     S. Schaer
!
! Created:    18-May-2000
!
! Changes:    05-Oct-2000 SS: Use "opt%title"
!             26-Jun-2001 RD: Use alcerr for allocation
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             26-Mar-2002 SS: Use "filtitle"
!             07-Aug-2002 SS: Print DCB multiplier summary
!             17-Sep-2002 SS: Check also "x0" to detect unobserved DCBs
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             13-Feb-2008 RD: Adapt to par%name == chr*20
!             19-Nov-2008 AS: Expand sysStr for system "E"
!             10-May-2009 RD: Frequency-dependent code biases
!             13-Aug-2010 RD: Warning in case of huge IFB
!             08-Jul-2011 LP: Use and fill up t_dcbFil structure from BSW6.0;
!                             use g_svnsys instead of sysstr; write bias SINEX
!             21-Jul-2011 LP: Add obstypes to dcbFil%dcbLst
!             28-Mar-2012 RD: Use SVN2CHR as module now
!             28-Mar-2012 RD: Use F_SVNSYS as module now
!             01-May-2012 LP: Replace r32r2 by OBSTYPESR3
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnprt, lfnerr, fileNameLength80
  USE m_time,   ONLY: t_timint
  USE m_maxdim, ONLY: maxsat, maxsta
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: opt,comstat
  USE d_const,  ONLY: filtitle,C
  USE d_dcbFil, ONLY: t_dcbFil,init_dcbFil
!  USE m_epoch,   ONLY: t_timWin
  USE m_global,  ONLY: g_svnsys
  USE d_rinex3, ONLY: OBSTYPESR3
  USE f_ikf
  USE s_clsrcv
  USE s_alcerr
  USE s_wtcbfl
  USE s_exitrc
  USE s_defreq
  USE s_wtbiassnx
  USE s_gtflna
  USE s_svn2chr
  USE f_svnsys
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq) :: neq

! Local Variables
! ---------------
  INTEGER(i4b)                                    :: iac
  INTEGER(i4b)                                    :: ipar
  INTEGER(i4b)                                    :: ityp
  INTEGER(i4b)                                    :: iSat,jSat,kSat
  INTEGER(i4b)                                    :: nSat
  INTEGER(i4b), DIMENSION(maxsat)                 :: satlst
  INTEGER(i4b)                                    :: nNone
  INTEGER(i4b), DIMENSION(maxsat)                 :: nonlst
  INTEGER(i4b)                                    :: satnum
  INTEGER(i4b)                                    :: iFrq
  INTEGER(i4b)                                    :: iIfb, jIfb
  INTEGER(i4b)                                    :: maxIfb
  INTEGER(i4b)                                    :: minFrq
  INTEGER(i4b)                                    :: maxFrq
  INTEGER(i4b)                                    :: numsat
  INTEGER(i4b)                                    :: numrec
  INTEGER(i4b)                                    :: numifb
  INTEGER(i4b)                                    :: icbtyp
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE         :: dcbid1
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE         :: dcbin1
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE         :: dcbin2
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE         :: dcbin3
  INTEGER(i4b)                                    :: iclass

  REAL(r8b)                                       :: dcbval
  REAL(r8b)                                       :: dcbrms
  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE       :: dcbva1
  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE       :: dcbva2
  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE       :: dcbva3
  REAL(r8b),    DIMENSION(3)                      :: clsfac
  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE       :: freq

  CHARACTER(LEN=16), DIMENSION(:), ALLOCATABLE    :: dcbid2
  CHARACTER(LEN=16), DIMENSION(:,:), ALLOCATABLE  :: dcbid3
  CHARACTER(LEN=16), DIMENSION(maxsat)            :: dcbPrt
  CHARACTER(LEN=16)                               :: iSta0
  CHARACTER(LEN=1),  DIMENSION(:), ALLOCATABLE    :: dcbsys
  CHARACTER(LEN=5)                                :: clsstr

  TYPE(t_timint)                                  :: timint
!  TYPE(t_timWin)                                  :: timWin

  TYPE(t_dcbFil)                                  :: dcbFil
  CHARACTER(LEN=fileNameLength80)                 :: dcbOut
  CHARACTER(LEN=fileNameLength80)                 :: dcbSnx
  INTEGER(i4b)                                    :: nDcb
  INTEGER(i4b)                                    :: irc, ircs, isys

  INCLUDE 'COMFREQ.inc'


! Nothing to do
! -------------
  CALL gtFlNa(0,'DCBOUT',dcbOut,irc)
  CALL gtFlNa(0,'BIASSNX',dcbSnx,ircs)

  IF ((irc == 1).AND.(ircs == 1)) THEN
    RETURN
  ENDIF


! Some initializations
! --------------------
  CALL init_dcbFil(dcbFil)

  numsat = 0
  numrec = 0
  numifb = 0
  icbtyp = 0


! Count number of satellites and receivers
! ----------------------------------------

  DO ipar = 1,neq%misc%npar

    IF ( neq%par(ipar)%locq(1) == 2 .AND. neq%par(ipar)%locq(6) /= 0 ) THEN
      numifb=maxsat*maxsta
    ENDIF


    IF ( neq%par(ipar)%locq(1) == 8 ) THEN

      IF ( neq%par(ipar)%locq(2) == 1 ) THEN
        numsat = numsat + 1
        ityp   = neq%par(ipar)%locq(5)
      ELSE
        numrec = numrec + 1
        ityp   = neq%par(ipar)%locq(6)
      END IF

      IF ( icbtyp == 0 ) THEN
        icbtyp=ityp
      ELSE IF ( ityp /= icbtyp ) THEN
        WRITE(lfnerr,'(/,A,A,/)') ' ### SR DCBSTORE: ', &
          'Mixture of DCB types not allowed'
! Comment only for GGSP/OVF test purposes
!        CALL exitrc(2)
      END IF

    END IF

  END DO


! Return, if no DCB-related info found
! ------------------------------------
  IF ( numsat + numrec + numifb == 0 ) THEN
    IF ((opt%dcbout /= '').OR.(ircs==0)) &
      WRITE(lfnerr,'(/,A,A,/)') ' ### SR DCBSTORE: ', &
      'No DCB-related information found'
    RETURN
  ELSE
    dcbFil%nVal = numsat + numrec + numifb
  END IF


! Allocate the arrays
! -------------------
  ALLOCATE( dcbid1(numsat),   stat=iac )
  CALL alcerr(iac, 'dcbid1', (/ numsat /), 'dcbstore')
  dcbid1 = 0
  ALLOCATE( dcbva1(2,numsat), stat=iac )
  CALL alcerr(iac, 'dcbva1', (/2,numsat/), 'dcbstore')
  dcbva1 = 0d0
  ALLOCATE( dcbid2(numrec),   stat=iac )
  CALL alcerr(iac, 'dcbid2', (/ numrec /), 'dcbstore')
  dcbid2 = ''
  ALLOCATE( dcbva2(2,numrec), stat=iac )
  CALL alcerr(iac, 'dcbva2', (/2,numrec/), 'dcbstore')
  dcbva2 = 0d0
  ALLOCATE( dcbsys(numrec),   stat=iac )
  CALL alcerr(iac, 'dcbsys', (/ numrec /), 'dcbstore')
  dcbsys = ''
  ALLOCATE( dcbin1(numsat),   stat=iac )
  CALL alcerr(iac, 'dcbin1', (/ numsat /), 'dcbstore')
  dcbin1 = 0
  ALLOCATE( dcbin2(numrec),   stat=iac )
  CALL alcerr(iac, 'dcbin2', (/ numrec /), 'dcbstore')
  dcbin2 = 0
  ALLOCATE( dcbin3(numifb),   stat=iac )
  CALL alcerr(iac, 'dcbin3', (/ numifb /), 'dcbstore')
  dcbin3 = 0
  ALLOCATE( dcbid3(2,numifb),   stat=iac )
  CALL alcerr(iac, 'dcbid3', (/ 2,numifb /), 'dcbstore')
  dcbid3 = ''
  ALLOCATE( dcbva3(4,numifb), stat=iac )
  CALL alcerr(iac, 'dcbva3', (/4,numifb/), 'dcbstore')
  dcbva3 = 0d0

  ALLOCATE(dcbFil%dcbLst(dcbFil%nVal),stat=iac)
  CALL alcErr(iac,'dcbFil%dcbLst',(/dcbFil%nVal/), 'dcbstore')


! Fill the arrays
! ---------------
  numsat = 0
  numrec = 0
  nDCB   = 0

  DO ipar = 1,neq%misc%npar

    IF ( neq%par(ipar)%locq(1) /= 8 ) CYCLE
!    IF (neq%par(iPar)%locq(2) == 3) CYCLE

    dcbval = neq%xxx(ipar) + neq%par(ipar)%x0
    dcbrms = comstat%rms * SQRT(ABS(neq%anor(ikf(ipar,ipar))))

    IF ( dcbrms == 0.0d0 .OR. neq%xxx(ipar) == 0.0d0 ) THEN
      IF ( opt%dcbout /= '' ) &
        WRITE(lfnerr,'(/,A,/)') ' ### SR DCBSTORE: Unobserved DCB not saved'
      CYCLE
    END IF

    timint%t(1) = neq%par(ipar)%time%mean - neq%par(ipar)%time%half
    timint%t(2) = neq%par(ipar)%time%mean + neq%par(ipar)%time%half

    nDCB = nDcb+1
    dcbFil%dcbLst(nDCB)%dcbVal = dcbval
    dcbFil%dcbLst(nDCB)%dcbRms = dcbrms
    dcbFil%dcbLst(nDCB)%flg    = "R"
    dcbFil%dcbLst(nDCB)%tim    = timint
!    dcbFil%dcbLst(nDCB)%tim    = timWin
    dcbFil%dcbLst(nDCB)%rem    = ""
    dcbFil%dcbLst(nDCB)%obstyp(1) = "   "
    dcbFil%dcbLst(nDCB)%obstyp(2) = "   "

! Satellite DCB
! -------------
    IF ( neq%par(ipar)%locq(2) == 1 ) THEN

      numsat           = numsat + 1
      dcbid1(numsat)   = neq%par(ipar)%locq(3)
      dcbva1(1,numsat) = dcbval
      dcbva1(2,numsat) = dcbrms

      dcbFil%dcbLst(nDCB)%svn    = neq%par(iPar)%locq(3)
      dcbFil%dcbLst(nDCB)%staNam = ""
      dcbFil%dcbLst(nDCB)%sys    = " "
      dcbFil%dcbLst(nDCB)%typ    = neq%par(iPar)%locq(5)
      isys = INT(dcbFil%dcbLst(nDCB)%svn/100)


! Station DCB
! -----------
    ELSE

      numrec           = numrec + 1
      dcbid2(numrec)   = neq%par(ipar)%name
      dcbsys(numrec)   = g_svnsys(neq%par(iPar)%locq(5)-1)
      dcbva2(1,numrec) = dcbval
      dcbva2(2,numrec) = dcbrms

      dcbFil%dcbLst(nDCB)%svn    = 0
      dcbFil%dcbLst(nDCB)%staNam = TRIM(neq%par(iPar)%name)
      dcbFil%dcbLst(nDCB)%sys    = g_svnsys(neq%par(iPar)%locq(5)-1)
      dcbFil%dcbLst(nDCB)%typ    = neq%par(iPar)%locq(6)
      isys = neq%par(iPar)%locq(5)-1


! Print ADDNEQ2 DCB multipliers summary
! -------------------------------------
      IF ( neq%par(ipar)%locq(6) == -2 ) THEN

        IF ( numrec == 1 ) WRITE(lfnprt,"(//,A,/,A,//,A,A,/,1X,131('-'))") &
          ' DCB Multiplier Summary:', &
          ' -----------------------', &
          ' Station name           Multiplier   RMS error    ', &
          'Suggested receiver type'

        CALL clsrcv(dcbval,dcbrms,iclass,clsstr,clsfac)

        WRITE(lfnprt,"(1X,A20,'-',A1,1X,2F13.3,4X,A5,2F8.3)") &
          neq%par(ipar)%name,g_svnsys(neq%par(iPar)%locq(5)-1),dcbval,dcbrms, &
          clsstr,clsfac(1),clsfac(2)

      END IF

    END IF


    ! Define observation types
    ! ------------------------
    IF (dcbFil%dcbLst(nDCB)%typ.EQ.1) THEN
    ! P1-P2 DCB
      IF (neq%par(iPar)%locq(7).NE.0) THEN
      ! Specific obstypes
        dcbFil%dcbLst(nDCB)%obstyp(1) = neq%misc%obst(neq%par(iPar)%locq(7))%obstyp(1)
        dcbFil%dcbLst(nDCB)%obstyp(2) = neq%misc%obst(neq%par(iPar)%locq(7))%obstyp(2)
      ELSE
      ! Standard obstypes (hardwired in D_RINEX3)
        dcbFil%dcbLst(nDCB)%obstyp(1) = OBSTYPESR3(4)
        dcbFil%dcbLst(nDCB)%obstyp(2) = OBSTYPESR3(5)
      ENDIF
    ELSE IF (dcbFil%dcbLst(nDCB)%typ.EQ.2) THEN
    ! P1-C1 DCB
      dcbFil%dcbLst(nDCB)%obstyp(1) = OBSTYPESR3(4)
      dcbFil%dcbLst(nDCB)%obstyp(2) = OBSTYPESR3(1)
    !ELSE IF (dcbFil%dcbLst(nDCB)%typ.EQ.3) THEN
    ! LC DCB
      ! dcbFil%dcbLst(nDCB)%obstyp(1) = "   "
      ! dcbFil%dcbLst(nDCB)%obstyp(2) = "   "
    ELSE IF (dcbFil%dcbLst(nDCB)%typ.EQ.4) THEN
    ! P2-C2 SAT DCB
      dcbFil%dcbLst(nDCB)%obstyp(1) = OBSTYPESR3(5)
      dcbFil%dcbLst(nDCB)%obstyp(2) = OBSTYPESR3(2)
    !ELSE IF (dcbFil%dcbLst(nDCB)%typ.EQ.5) THEN
    ! IFB SAT DCB
    ! dcbFil%dcbLst(nDCB)%obstyp(1) = OBSTYPESR3(6)
    ! dcbFil%dcbLst(nDCB)%obstyp(2) = OBSTYPESR3(7)
    ENDIF

  END DO


! Inter-frequency code biases
! ---------------------------
  numifb = 0
  iSta0  = ''
  DO ipar=1,neq%misc%npar
    IF ( neq%par(ipar)%locq(1) /= 2 ) CYCLE
    IF ( neq%par(ipar)%locq(6) == 0 ) CYCLE

    IF (icbtyp == 0) icbtyp = 4

    dcbval = neq%xxx(ipar) / C * 1d9 + neq%par(ipar)%x0
    dcbrms = comstat%rms * SQRT(ABS(neq%anor(ikf(ipar,ipar)))) / C * 1d9

!    IF ( dcbrms == 0.0 .OR. neq%xxx(ipar) == 0.0 ) THEN
!      WRITE(lfnerr,'(/,A,/)') ' ### SR DCBSTORE: Unobserved DCB not saved'
!      CYCLE
!    END IF

    nSat = 24
    DO iSat = 1,24
      satLst(iSat) = iSat+100
    ENDDO
    timint%t(1) = neq%par(ipar)%time%mean - neq%par(ipar)%time%half
    timint%t(2) = neq%par(ipar)%time%mean + neq%par(ipar)%time%half
    CALL DEFREQ(timint%t,nSat,satLst,nNone,nonLst)
    jSat = 0
    satLoop: DO iSat = 1,nSat
      DO kSat = 1,nNone
        IF (nonLst(kSat) == satLst(iSat)) CYCLE satLoop
      ENDDO
      jSat = jSat+1
      IF (iSat == jSat) CYCLE satLoop
      satLst(jSat) = satLst(iSat)
    ENDDO satLoop
    nSat = jSat

! frequency-specific
    IF (neq%par(ipar)%locq(6) == 1) THEN
      IF ( iSta0 /= neq%par(ipar)%name ) THEN
        numifb=numifb+1
        dcbid3(1,numifb) = 'G'
        dcbid3(2,numifb) = neq%par(ipar)%name
        dcbva3(:,numifb) = 0d0

        nDCB = nDcb+1
        dcbFil%dcbLst(nDCB)%dcbVal = 0d0
        dcbFil%dcbLst(nDCB)%dcbRms = 0d0
        dcbFil%dcbLst(nDCB)%tim    = timint
!      dcbFil%dcbLst(nDCB)%tim    = timWin
        dcbFil%dcbLst(nDCB)%sys    = 'G'
        dcbFil%dcbLst(nDCB)%svn    = 0
        dcbFil%dcbLst(nDCB)%staNam = TRIM(neq%par(iPar)%name)
!?        dcbFil%dcbLst(nDCB)%typ    = neq%par(iPar)%locq(6)
        dcbFil%dcbLst(nDCB)%typ    = 5
        dcbFil%dcbLst(nDCB)%rem    = ""
        dcbFil%dcbLst(nDCB)%flg    = "R"
        dcbFil%dcbLst(nDCB)%obstyp(1) = "   "
        dcbFil%dcbLst(nDCB)%obstyp(2) = "   "

      ENDIF

      DO iSat = 1,nSat
        IF (SVNSYS(1,1,(/ satLst(iSat) /)) .AND. &
            neq%par(ipar)%locq(4) == freqnm(satlst(iSat))) THEN

          numifb=numifb+1

          dcbid3(1,numifb) = 'R'
          WRITE(dcbid3(1,numifb)(2:3),'(I2.2)') satlst(isat)-100
          dcbid3(2,numifb) = neq%par(ipar)%name

          dcbva3(1,numifb)   = dcbval
          dcbva3(2,numifb)   = dcbrms
          dcbva3(3:4,numifb) = timint%t(1:2)

          nDCB = nDcb+1
          dcbFil%dcbLst(nDCB)%dcbVal = dcbval
          dcbFil%dcbLst(nDCB)%dcbRms = dcbrms
          dcbFil%dcbLst(nDCB)%tim    = timint
!          dcbFil%dcbLst(nDCB)%tim    = timWin
          dcbFil%dcbLst(nDCB)%sys    = 'R'
          dcbFil%dcbLst(nDCB)%svn    = satlst(isat)
          dcbFil%dcbLst(nDCB)%staNam = TRIM(neq%par(iPar)%name)
!?          dcbFil%dcbLst(nDCB)%typ    = neq%par(iPar)%locq(6)
          dcbFil%dcbLst(nDCB)%typ    = 5
          dcbFil%dcbLst(nDCB)%rem    = ""
          dcbFil%dcbLst(nDCB)%flg    = "R"
          dcbFil%dcbLst(nDCB)%obstyp(1) = OBSTYPESR3(6)
          dcbFil%dcbLst(nDCB)%obstyp(2) = OBSTYPESR3(7)

        ENDIF
      ENDDO
      iSta0 = neq%par(ipar)%name
    ENDIF

! satellite-specific
! ------------------
    IF (neq%par(ipar)%locq(6) == 2) THEN

      IF (iSta0 /= neq%par(ipar)%name  .AND. &
          neq%par(ipar)%locq(7) ==   1 .AND. &
          neq%par(ipar)%locq(4) >= 100) THEN

        numifb=numifb+1
        dcbid3(1,numifb) = 'G'
        dcbid3(2,numifb) = neq%par(ipar)%name
        dcbva3(:,numifb) = 0d0

        nDCB = nDcb+1
        dcbFil%dcbLst(nDCB)%dcbVal = 0d0
        dcbFil%dcbLst(nDCB)%dcbRms = 0d0
        dcbFil%dcbLst(nDCB)%tim    = timint
!        dcbFil%dcbLst(nDCB)%tim    = timWin
        dcbFil%dcbLst(nDCB)%sys    = 'G'
        dcbFil%dcbLst(nDCB)%svn    = 0
        dcbFil%dcbLst(nDCB)%staNam = TRIM(neq%par(iPar)%name)
!?        dcbFil%dcbLst(nDCB)%typ    = neq%par(iPar)%locq(6)
        dcbFil%dcbLst(nDCB)%typ    = 5
        dcbFil%dcbLst(nDCB)%rem    = ""
        dcbFil%dcbLst(nDCB)%flg    = "R"
        dcbFil%dcbLst(nDCB)%obstyp(1) = "   "
        dcbFil%dcbLst(nDCB)%obstyp(2) = "   "

      ENDIF

      numifb=numifb+1

      dcbid3(1,numifb) = ''
      CALL SVN2CHR(neq%par(ipar)%locq(4),satnum,dcbid3(1,numifb))
      WRITE(dcbid3(1,numifb)(2:3),'(I2.2)') satnum
      dcbid3(2,numifb) = neq%par(ipar)%name

      dcbva3(1,numifb)   = dcbval
      dcbva3(2,numifb)   = dcbrms
      dcbva3(3:4,numifb) = timint%t(1:2)

      nDCB = nDcb+1
      dcbFil%dcbLst(nDCB)%dcbVal = dcbval
      dcbFil%dcbLst(nDCB)%dcbRms = dcbrms
      dcbFil%dcbLst(nDCB)%tim    = timint
!      dcbFil%dcbLst(nDCB)%tim    = timWin
      dcbFil%dcbLst(nDCB)%sys    = dcbid3(1,numifb)(1:1)
      dcbFil%dcbLst(nDCB)%svn    = neq%par(ipar)%locq(4)
      dcbFil%dcbLst(nDCB)%staNam = TRIM(neq%par(iPar)%name)
!?      dcbFil%dcbLst(nDCB)%typ    = neq%par(iPar)%locq(6)
      dcbFil%dcbLst(nDCB)%typ    = 5
      dcbFil%dcbLst(nDCB)%rem    = ""
      dcbFil%dcbLst(nDCB)%flg    = "R"
      isys = INT(dcbFil%dcbLst(nDCB)%svn/100)
      dcbFil%dcbLst(nDCB)%obstyp(1) = OBSTYPESR3(6)
      dcbFil%dcbLst(nDCB)%obstyp(2) = OBSTYPESR3(7)

      iSta0 = neq%par(ipar)%name
    ENDIF

! frequency-specific, polynomial
    IF (neq%par(ipar)%locq(6) == 4) THEN

      minfrq=999999
      maxfrq=999999
      DO iSat=1,nSat
        IF ( SVNSYS(1,1,(/ satLst(iSat) /)) ) THEN
          IF (minfrq == 999999 .OR. minfrq > freqnm(satLst(iSat))) THEN
            minfrq = freqnm(satLst(iSat))
          ENDIF
          IF (maxfrq == 999999 .OR. maxfrq < freqnm(satLst(iSat))) THEN
            maxfrq = freqnm(satLst(iSat))
          ENDIF
        ENDIF
      ENDDO

      IF (minfrq /= 999999) THEN
        ALLOCATE(freq(2,maxfrq-minfrq+1),stat=iac)
        CALL alcerr(iac,'FREQ',(/2,maxfrq-minfrq+1/),'DCBSTORE')

        IF (iSta0 /= neq%par(ipar)%name ) THEN

          numifb=numifb+1
          dcbid3(1,numifb) = 'G'
          dcbid3(2,numifb) = neq%par(ipar)%name
          dcbva3(:,numifb) = 0d0

          nDCB = nDcb+1
          dcbFil%dcbLst(nDCB)%dcbVal = 0d0
          dcbFil%dcbLst(nDCB)%dcbRms = 0d0
          dcbFil%dcbLst(nDCB)%tim    = timint
!          dcbFil%dcbLst(nDCB)%tim    = timWin
          dcbFil%dcbLst(nDCB)%sys    = 'G'
          dcbFil%dcbLst(nDCB)%svn    = 0
          dcbFil%dcbLst(nDCB)%staNam = TRIM(neq%par(iPar)%name)
!?          dcbFil%dcbLst(nDCB)%typ    = neq%par(iPar)%locq(6)
          dcbFil%dcbLst(nDCB)%typ    = 5
          dcbFil%dcbLst(nDCB)%rem    = ""
          dcbFil%dcbLst(nDCB)%flg    = "R"
          dcbFil%dcbLst(nDCB)%obstyp(1) = "   "
          dcbFil%dcbLst(nDCB)%obstyp(2) = "   "

          freq = 0d0
        ENDIF

        DO iFrq = minfrq,maxfrq
          freq(1,iFrq-minfrq+1)=freq(1,iFrq-minfrq+1)+ &
                  dcbval*(iFrq-6)**(neq%par(iPar)%locq(4)-1)
          freq(2,iFrq-minfrq+1)=freq(2,iFrq-minfrq+1)+ &
                 (dcbrms*(iFrq-6)**(neq%par(iPar)%locq(4)-1))**2
        ENDDO

        IF (neq%par(iPar)%locq(4) == neq%par(iPar)%locq(5)) THEN
          DO iSat = 1,nSat
            IF (SVNSYS(1,1,(/ satlst(iSat) /))) THEN

              numifb=numifb+1
              dcbid3(1,numifb) = 'R'
              WRITE(dcbid3(1,numifb)(2:3),'(i2.2)') satLst(iSat)-100
              dcbid3(2,numifb) = neq%par(ipar)%name

              dcbva3(1:2,numifb) = freq(1:2,freqnm(satLst(iSat))-minfrq+1)
              dcbva3(2,numifb)   = DSQRT(dcbva3(2,numifb))
              dcbva3(3:4,numifb) = timint%t(1:2)

              nDCB = nDcb+1
              dcbFil%dcbLst(nDCB)%dcbVal = dcbva3(1,numifb)
              dcbFil%dcbLst(nDCB)%dcbRms = dcbva3(2,numifb)
              dcbFil%dcbLst(nDCB)%tim    = timint
!             dcbFil%dcbLst(nDCB)%tim    = timWin
              dcbFil%dcbLst(nDCB)%sys    = 'R'
              dcbFil%dcbLst(nDCB)%svn    = satlst(isat)
              dcbFil%dcbLst(nDCB)%staNam = TRIM(neq%par(iPar)%name)
!?             dcbFil%dcbLst(nDCB)%typ    = neq%par(iPar)%locq(6)
              dcbFil%dcbLst(nDCB)%typ    = 5
              dcbFil%dcbLst(nDCB)%rem    = ""
              dcbFil%dcbLst(nDCB)%flg    = "R"
              isys = INT(dcbFil%dcbLst(nDCB)%svn/100)
              dcbFil%dcbLst(nDCB)%obstyp(1) = OBSTYPESR3(6)
              dcbFil%dcbLst(nDCB)%obstyp(2) = OBSTYPESR3(7)

            ENDIF
          ENDDO
        ENDIF
        DEALLOCATE(FREQ)
        iSta0 = neq%par(ipar)%name
      ENDIF
    ENDIF
  ENDDO


! Check for exceptional big IFB
! -----------------------------
  dcbPrt = ''
  DO iIfb = 1,numifb
    IF (DABS(dcbva3(1,iIfb)) > opt%maxIfb) THEN
      maxIfb = iIfb

      DO jIfb = 1,maxSat
        IF ( dcbid3(1,iIfb) == dcbPrt(jIfb) ) THEN
          maxIfb = 0
          EXIT
        ENDIF
      ENDDO
      IF ( maxIfb /= 0 ) THEN

        DO jIfb = 1,numifb
          IF ( dcbid3(1,iIfb) == dcbid3(1,jIfb) .AND.  &  ! the same satellite
               DABS(dcbva3(1,maxIfb)) < DABS(dcbva3(1,jIfb)) ) maxIfb = jIfb
        ENDDO
        WRITE(lfnerr,'(/,A,/)')                                  &
        ' ### DCBSTORE: Exceptional IFB found for sat/sta: ' //  &
              TRIM(dcbid3(1,maxIfb)) // ':' // TRIM(dcbid3(2,maxIfb))
        DO jIfb = 1,maxSat
          IF ( dcbPrt(jIfb) == '' ) THEN
            dcbPrt(jIfb) = dcbid3(1,maxIfb)
            EXIT
          ENDIF
        ENDDO
      ENDIF
    ENDIF
  ENDDO

! Save the results
! ----------------
  IF (opt%dcbout /= '' .AND. icbtyp /= -2) &
    CALL wtcbfl(opt%dcbout,filtitle,numsat,numrec,numifb,icbtyp, &
                dcbid1,dcbva1,dcbid2,dcbva2,dcbsys,dcbid3,dcbva3,&
                dcbin1,dcbin2,dcbin3)

  dcbFil%nVal = nDCB
  IF (icbtyp /= -2) &
    CALL WTBIASSNX(dcbFil,neq%misc%nSmpNq)


  DEALLOCATE( dcbid1, stat=iac )
  DEALLOCATE( dcbva1, stat=iac )
  DEALLOCATE( dcbid2, stat=iac )
  DEALLOCATE( dcbva2, stat=iac )
  DEALLOCATE( dcbsys, stat=iac )
  DEALLOCATE( dcbid3, stat=iac )
  DEALLOCATE( dcbva3, stat=iac )
  DEALLOCATE( dcbin1, stat=iac )
  DEALLOCATE( dcbin2, stat=iac )
  DEALLOCATE( dcbin3, stat=iac )
  DEALLOCATE( dcbFil%dcbLst, stat=iac )

END SUBROUTINE dcbstore


END MODULE
