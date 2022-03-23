MODULE s_RNXDCB
CONTAINS

! -------------------------------------------------------------------------
! Bernese GNSS Software Version 5.2
! -------------------------------------------------------------------------

SUBROUTINE rnxdcb(maxrec,maxcmb,obsrec,obsaux,nepoch,nrsat,numsat, &
                  obstim,dcbfil,stanam,icbest,rectyp,icomb,sipfil,usegeos,gobsdef)

! -------------------------------------------------------------------------
! Purpose:    Direct estimation of P1-C1 and P2-C2 DCB values
!
! Author:     S.Schaer
!
! Created:    10-Aug-2009
!
! Changes:    12-Aug-2009 ss: Refined (GNSS-capable) estimation
!             09-May-2010 RD: Add inter-frequency code biases
!             15-Jun-2010 ss: Combination of RINEX-specific P-C results
!             03-Oct-2011 LP: Direct estimation of P1-P2 DCBs, too; read SIP file
!             30-Nov-2011 SL: use m_bern with ONLY
!             03-Jan-2012 ss: ired removed
!             25-Jan-2012 ss: Reject obviously bad P-C observations
!             05-Mar-2012 ss: Corrected unit inconsistency
!             06-Mar-2012 ss: Define minimum rms value
!             08-Mar-2012 ss: Added list of exclusions
!             14-Mar-2012 lp: Remove unused variables: i,k,iprn
!             27-Mar-2012 ss: Detect and reject unconnected DCB clusters
!             30-Mar-2012 ss: Changed xsum > 0d0 to IDNINT(xsum) > 0
!             30-Mar-2012 ss: Introduced sigref
!             03-Apr-2012 ss: Adaptive constraining for datum definition
!             10-Jul-2012 RD: Use syminvg instead of symin8
!             30-Jul-2012 RD: SR STATIS with optional arguments
!             30-Jul-2012 RD: Remove unused variables
!             20-Sep-2012 RD: Correctly deallocate arrays
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, fileNameLength, staNameLength, &
                      shortLineLength, lfnPrt, lfnErr, lfnloc
  USE m_maxdim, ONLY: maxsat,maxsta
  USE m_global, ONLY: maxsys,g_svnsys,g_strsys
  USE d_const,  ONLY: c,filtitle,FACTEC
  USE d_rinex3, ONLY: t_gobsdef
  USE s_alcerr
  USE s_timst2
  USE s_statis
  USE s_dordup
  USE s_wtcbfl
  USE s_mjdgps
  USE s_syminvg
  USE s_solve
  USE s_exitrc
  USE f_lengt1
  USE s_gtflna
  USE s_opnfil
  USE s_opnerr
  USE s_defreq
  USE f_ikf
  IMPLICIT NONE

! Arguments
! ---------
! Input:
  INTEGER(i4b)                               :: maxrec ! Maximum number of records
  INTEGER(i4b)                               :: maxcmb ! Maximum number of combinations
  REAL(r8b), DIMENSION(maxrec,maxcmb,maxsat) :: obsrec ! Observation records (L1/L2/P1/P2)
  REAL(r8b), DIMENSION(maxrec,2,maxsat)      :: obsaux ! Auxiliary records (C1/C2)
  INTEGER(i4b)                               :: nepoch ! Total number of epochs
  INTEGER(i4b)                               :: nrsat  ! Total number of satellites
  INTEGER(i4b), DIMENSION(maxrec)            :: numsat ! Satellite numbers
  REAL(r8b), DIMENSION(maxrec)               :: obstim ! Observation epochs
  CHARACTER(LEN=fileNameLength)              :: dcbfil ! DCB output filename
  CHARACTER(LEN=fileNameLength),OPTIONAL     :: sipFil ! File with slant TEC information
  CHARACTER(LEN=staNameLength)               :: stanam ! Station name
  CHARACTER(LEN=20)                          :: rectyp ! Receiver type
  INTEGER(i4b)                               :: icomb  ! Combination flag
                                                       ! = 0: Station-specific DCB estimation
                                                       ! = 1: Combination of all saved DCB results
                                                       ! =-1: Initialization of saved DCB arrays
  INTEGER(i4b)                               :: icbest ! Estimation of DCBs:
                                                       ! = 0: no DCB estimation
                                                       ! = 1: P1-C1 (ityp=1) or P2-C2 (ityp=2)
                                                       ! = 2: P1-P2 (ityp=3)
  INTEGER(i4b),OPTIONAL                      :: usegeos! Use sat-specific obstypes (1) or not (0)
  type(t_gobsdef),OPTIONAL                   :: gobsdef! Sat-specific obstypes

! Local variables
! ---------------
  INTEGER(i4b), SAVE                         :: ifirst=1
  INTEGER(i4b), PARAMETER                    :: maxtyp=3
!!  INTEGER(i4b), PARAMETER                    :: minobs=10
  INTEGER(i4b), PARAMETER                    :: minobs=120
  INTEGER(i4b), PARAMETER                    :: nsampl=30
  INTEGER(i4b), DIMENSION(maxsat)            :: dcbid1
  INTEGER(i4b), DIMENSION(maxsat)            :: dcbin1
  INTEGER(i4b), DIMENSION(maxsys*maxsta)     :: dcbin2
  INTEGER(i4b), DIMENSION(1)                 :: dcbin3
  INTEGER(i4b)                               :: icbtyp,ityp,isat,iepo
  INTEGER(i4b)                               :: iLin,ios,iac
!  INTEGER(i4b)                               :: ircsip ! return code sip file
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE, SAVE :: sipSat
  INTEGER(i4b)                               :: isvn,isys,nsys,nsat,nsps,nsta,nfil
  INTEGER(i4b)                               :: ndat,ndat0,ndat1,ndat2,nbad
  INTEGER(i4b)                               :: ntry,i1,i2,ilst,ilst1,ilst2
  INTEGER(i4b)                               :: nobs,npar,ndof,ising,nflag,niter
  INTEGER(i4b)                               :: iobs,ipar,ipar1,ipar2,i1i2
  INTEGER(i4b)                               :: ista,iclu,nclu,isel,nsel
  INTEGER(i4b)                               :: nweek
  INTEGER(i4b), DIMENSION(4,maxsat)          :: dcbcnt
  INTEGER(i4b), DIMENSION(6,maxsat)          :: dcbclu
  INTEGER(i4b), DIMENSION(maxsys)            :: refclu,refsat,refsta
  INTEGER(i4b), DIMENSION(2,maxsat)          :: selclu
  INTEGER(i4b), DIMENSION(maxtyp,maxsys)     :: sysbad
  INTEGER(i4b), DIMENSION(maxtyp,maxsat)     :: satbad
  INTEGER(i4b), DIMENSION(maxtyp,maxsta,maxsys) :: stabad,recbad
  INTEGER(i4b), DIMENSION(maxsta), SAVE      :: starec
  INTEGER(i4b), DIMENSION(maxtyp,maxsta), SAVE :: stacnt
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE, SAVE :: obsind
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE    :: selind
  INTEGER(i4b), SAVE                         :: totfil=0
  INTEGER(i4b), SAVE                         :: totsat=0
  INTEGER(i4b), SAVE                         :: totsta=0
  INTEGER(i4b), SAVE                         :: totrec=0
  INTEGER(i4b), SAVE                         :: totest=0
  INTEGER(i4b), DIMENSION(maxsat), SAVE      :: satlst
  INTEGER(i4b), DIMENSION(:,:), ALLOCATABLE, SAVE :: estind
  INTEGER(i4b), DIMENSION(:,:), ALLOCATABLE  :: parind
  INTEGER(i4b)                               :: numifb
  INTEGER(i4b)                               :: i,ii,called

  REAL(r8b), PARAMETER                       :: maxdif=300d0
  REAL(r8b), PARAMETER                       :: dtsim=0.1d0
  REAL(r8b), DIMENSION(2,maxsat)             :: dcbva1
  REAL(r8b), DIMENSION(2,maxsys*maxsta)      :: dcbva2
  REAL(r8b), DIMENSION(4,1)                  :: dcbva3
  REAL(r8b), DIMENSION(:,:), ALLOCATABLE, SAVE :: sipRec
  REAL(r8b), SAVE                            :: dcbepo
  REAL(r8b)                                  :: TECU
  REAL(r8b)                                  :: xdat,xdat1,xdat2,xsum,xsum1,xsum2
  REAL(r8b)                                  :: xmax,xmin,xdif,xtol,xtol0,xres
  REAL(r8b)                                  :: xmed,xmean,xsigma,xsigma0
  REAL(r8b)                                  :: tobs,gpssec,tfrac
  REAL(r8b), DIMENSION(maxsys)               :: sigsys,refsig
  REAL(r8b)                                  :: sigfit,sigref
  REAL(r8b), PARAMETER                       :: sigapr=1d0
  REAL(i4b), DIMENSION(4,maxsat)             :: dcbpar
  REAL(r8b), DIMENSION(:), ALLOCATABLE, SAVE :: obsdat
  REAL(r8b), DIMENSION(:), ALLOCATABLE       :: lobs,pobs,anor,bnor,xsol,xdof
  REAL(r8b), DIMENSION(:,:), ALLOCATABLE, SAVE :: estdat
  REAL(r8b), DIMENSION(:,:), ALLOCATABLE     :: aobs,vobs

  CHARACTER(LEN=shortLineLength)             :: title
  CHARACTER(LEN=fileNameLength)              :: dcbaux
  CHARACTER(LEN=19)                          :: epostr
  CHARACTER(LEN=staNameLength), DIMENSION(maxsys*maxsta) :: dcbid2
  CHARACTER(LEN=1), DIMENSION(maxsys*maxsta) :: dcbsys
  CHARACTER(LEN=staNameLength), DIMENSION(maxsta), SAVE :: stalst
  CHARACTER(LEN=staNameLength),DIMENSION(2,1):: dcbid3
  CHARACTER(LEN=20), DIMENSION(maxsta),SAVE  :: reclst
  CHARACTER(LEN=8),PARAMETER                 :: srName = 'RNXDCB'
  CHARACTER(LEN=100)                         :: line

  LOGICAL                                    :: ok

  INCLUDE 'COMFREQ.inc'




! Allocate arrays
! ---------------
  IF (ifirst == 1) THEN

! In case of estimation of P1-P2 DCBs: read TECU values from SIP file
! -------------------------------------------------------------------
   IF (icbest == 2) THEN
!    CALL gtflna(0,'SIPFILE',sipFil,ircSip)
    CALL OPNFIL(lfnloc,sipFil,'OLD','FORMATTED','READONLY',' ',ios)
    CALL OPNERR(lfnerr,lfnloc,ios,sipFil,srName)
    iLin = 0
    DO WHILE (ios == 0)
      READ (lfnloc,'(A)',iostat=ios) line
      IF (ios /= 0) CYCLE
      IF (LEN_TRIM(line) == 0) EXIT
      iLin=iLin+1
    ENDDO
    CLOSE(lfnloc)

    ALLOCATE(sipRec(iLin,2),stat=iac)
    CALL alcerr(iac,'sipRec',(/iLin,2/),srName)
    ALLOCATE(sipSat(iLin),stat=iac)
    CALL alcerr(iac,'sipSat',(/iLin/),srName)

    CALL OPNFIL(lfnloc,sipFil,'OLD','FORMATTED','READONLY',' ',ios)
    CALL OPNERR(lfnerr,lfnloc,ios,sipFil,srName)
    iLin = 0
    DO WHILE (ios == 0)
      READ (lfnloc,'(A)',iostat=ios) line
      IF (ios /= 0) CYCLE
      IF (LEN_TRIM(line) == 0) EXIT
      iLin=iLin+1
      READ (line,'(I3,F14.7,F11.4)',iostat=ios)        &
        sipSat(iLin),sipRec(iLin,1),sipRec(iLin,2)
    ENDDO
    CLOSE(lfnloc)
   ENDIF


! Combination not feasible at first call
! --------------------------------------
    IF (icomb == 1) THEN
      WRITE(lfnerr,'(/,A)') &
        ' *** SR RNXDCB: Combination not feasible at first call'
      CALL exitrc(2)
    ENDIF

    ALLOCATE(obsdat(maxrec),stat=iac)
    CALL alcerr(iac,'obsdat',(/maxrec/),'rnxdcb')
    ALLOCATE(obsind(maxrec),stat=iac)
    CALL alcerr(iac,'obsind',(/maxrec/),'rnxdcb')

    nobs=maxsta*maxsat*maxtyp
    ALLOCATE(estdat(2,nobs),stat=iac)
    CALL alcerr(iac,'estdat',(/2,nobs/),'rnxdcb')
    ALLOCATE(estind(6,nobs),stat=iac)
    CALL alcerr(iac,'estind',(/6,nobs/),'rnxdcb')

! Initialize arrays
! -----------------
    estdat(:,:)=0d0
    estind(:,:)=0
    stacnt(:,:)=0

    WRITE(lfnprt,'(//,A,/,A,/)') &
      ' DIRECT ESTIMATION OF P1-C1 OR P2-C2 DCB VALUES:', &
      ' -----------------------------------------------'

    ifirst=0
  ENDIF

  IF (icomb == 0) THEN

! Get mean observation epoch
! --------------------------
    dcbepo=(obstim(1)+obstim(nepoch))/2d0
    CALL timst2(1,1,dcbepo,epostr)

! Computation of P1-C1, P2-C2, and P1-P2 bias values
! --------------------------------------------------
    icbtyp=0
    DO ityp=1,maxtyp

      IF ((icbest == 1).AND.(ityp.GT.2)) CYCLE
      IF ((icbest == 2).AND.(ityp.LT.3)) CYCLE

      nsat=0
      obsdat(:)=0d0
      obsind(:)=0
      dcbpar(:,:)=0d0
      dcbcnt(:,:)=0

! Compute mean and rms values for each observed satellite
      DO isat=1,nrsat
        isvn=numsat(isat)
!!        IF (isvn/100 /= 0) CYCLE
!!        IF (isvn ==   1) CYCLE
!!        IF (isvn ==  25) CYCLE
!!        IF (isvn == 109) CYCLE
        WRITE(lfnprt,*) 'svn',isat,nrsat,isvn

        ok=.FALSE.
        xmed=0d0
        xtol=1d20
        ntry=0
        DO WHILE (.NOT. ok)
          WRITE(lfnprt,*) 'try',ntry,xtol,xmean,xmed,ndat0-ndat
          ndat=0
          ndat0=0
          ndat1=0
          ndat2=0
          nbad=0
          xmax=-1d20
          xmin= 1d20
          xtol0=xtol

          DO iepo=1,nepoch
            tobs=obstim(iepo)

! List of exclusions:
!!            IF (isvn ==   8 .AND. tobs >= 50772d0 .AND. tobs < 50773d0) CYCLE
!!            IF (isvn ==  13 .AND. tobs >= 50675d0 .AND. tobs < 50676d0) CYCLE
!!            IF (isvn ==  20 .AND. tobs >= 50395d0 .AND. tobs < 50400d0) CYCLE
!!            IF (isvn ==  29 .AND. tobs >= 50591d0 .AND. tobs < 50592d0) CYCLE
!!            IF (isvn == 110 .AND. tobs >= 51095d0 .AND. tobs < 51096d0) CYCLE
!!            IF (isvn == 110 .AND. tobs >= 51099d0 .AND. tobs < 51100d0) CYCLE
!!            IF (isvn == 110 .AND. tobs >= 51103d0 .AND. tobs < 51104d0) CYCLE
!!            IF (isvn == 110 .AND. tobs >= 51115d0 .AND. tobs < 51116d0) CYCLE
!!            IF (isvn == 110 .AND. tobs >= 51155d0 .AND. tobs < 51156d0) CYCLE
!!            IF (isvn == 110 .AND. tobs >= 51278d0 .AND. tobs < 51279d0) CYCLE
!!            IF (isvn == 115 .AND. tobs >= 52319d0 .AND. tobs < 52320d0) CYCLE
!!            IF (isvn == 116 .AND. tobs >= 51865d0 .AND. tobs < 51866d0) CYCLE
!!            IF (isvn == 116 .AND. tobs >= 51881d0 .AND. tobs < 51882d0) CYCLE
!!            IF (isvn == 116 .AND. tobs >= 51929d0 .AND. tobs < 51930d0) CYCLE
!!            IF (isvn == 124 .AND. tobs >= 52182d0 .AND. tobs < 52183d0) CYCLE

            TECU = 0d0
!           P1-C1 or P2-C2 DCBs
            IF ((icbest == 1).and.(ityp<3)) THEN
              xdat1=obsrec(iepo,ityp+2,isat)
              xdat2=obsaux(iepo,ityp,isat)
!           P1-P2 DCBs, read SIP record
            ELSE IF ((icbest == 2).and.(ityp==3)) THEN
              TECU = -999d0
              xdat1=obsrec(iepo,ityp,isat)
              xdat2=obsrec(iepo,ityp+1,isat)
              DO ii=1,iLin
                IF ((sipSat(ii).EQ.isvn).AND. &
                   ((sipRec(ii,1)-obstim(iepo))< 1/86400d0)) THEN
                   TECU = sipRec(ii,2)

                   CALLED=0
                   IF (PRESENT(USEGEOS).AND.PRESENT(GOBSDEF)) THEN
                     IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
                       CALL DEFREQ((/obstim(iepo),obstim(iepo)/),1,(/isvn/), &
                                   USEGEOS=USEGEOS,GOBSDEF=GOBSDEF,MEATYPC='C')
                       CALLED=1
                     ENDIF
                   ENDIF
                   IF (CALLED==0) THEN
                     CALL DEFREQ((/obstim(iepo),obstim(iepo)/),1,(/isvn/))
                   ENDIF
                ENDIF
              ENDDO
            ELSE
              WRITE(*,*) '*** SR RNXDCB: Unknown icbest or ityp.'
              call exitrc(2)
            ENDIF

            IF ((xdat1 /= 0d0).AND.(xdat2 /= 0d0).AND.(TECU > -900d0)) THEN
              IF (nsampl.NE.0) THEN
                CALL mjdgps(tobs,gpssec,nweek)
                tfrac=gpssec-NINT(gpssec/nsampl)*nsampl
                IF (ABS(tfrac).GT.dtsim*86400d0) CYCLE
              ENDIF

              IF ((icbest == 1).and.(ityp<3)) THEN
                xdat=(xdat1-xdat2)*1d9/c
              ELSE IF ((icbest == 2).and.(ityp==3)) THEN
                xdat=(xdat1-xdat2-TECU*FACTEC*(1/FRQ(1,isvn)**2 - 1/FRQ(2,isvn)**2))*1d9/c
              ELSE
                WRITE(*,*) '*** SR RNXDCB: Unknown icbest or ityp.'
                call exitrc(2)
              ENDIF
              IF (ABS(xdat).GT.1d6) THEN
                nbad=nbad+1
!!                WRITE(lfnprt,*) 'BAD:',iepo,isvn,xdat,xdat1,xdat2,stanam,rectyp
                CYCLE
              ENDIF
              ndat0=ndat0+1
              IF (ityp == 2 .AND. isvn ==  1 .AND. icbest == 1) THEN
                WRITE(lfnprt,*) 'G01-C2-RESULTS:',iepo,xdat,stanam,rectyp
              ENDIF
              IF (ityp == 2 .AND. isvn == 25 .AND. icbest == 1) THEN
                WRITE(lfnprt,*) 'G25-C2-ANOMALY:',iepo,xdat,stanam,rectyp
              ENDIF

              IF (xdat > xmax) xmax=xdat
              IF (xdat < xmin) xmin=xdat
              xdif=xdat-xmed
              IF (ABS(xdif) < xtol) THEN
                ndat=ndat+1
                obsdat(ndat)=xdat
              ENDIF
            ELSEIF ((xdat1 /= 0d0).AND.(xdat2 == 0d0).AND.(TECU > -900d0)) THEN
              ndat1=ndat1+1
            ELSEIF ((xdat1 == 0d0).AND.(xdat2 /= 0d0).AND.(TECU > -900d0)) THEN
              ndat2=ndat2+1
            ENDIF
          ENDDO

          IF (ndat > 0) THEN
            CALL dordup(obsdat,ndat,obsind)
            xdif=obsdat(obsind(ndat))-obsdat(obsind(1))

            i1=NINT(0.8413d0*(ndat-1)+1)
            i2=NINT(0.1587d0*(ndat-1)+1)
            xtol=4d0*(obsdat(obsind(i1))-obsdat(obsind(i2)))/2d0
            IF (xtol > xtol0) xtol=xtol0

            CALL statis(ndat,obsdat,xMed=xmed,xMean=xmean,xSigma=xsigma)
            IF (ndat > 0) xsigma0=xsigma/SQRT(ndat*1d0)

            xres=xmax-xmean
            IF (xmin-xmean < -xres) xres=xmin-xmean
          ENDIF

          IF (ndat >= minobs .AND. &
              (obsdat(obsind(ndat)) > xmed+xtol .OR. &
               obsdat(obsind(1))    < xmed-xtol .OR. &
               xdif > maxdif)) THEN
            IF (xtol == xtol0) THEN
              ok=.TRUE.
            ELSE
              ntry=ntry+1
            ENDIF
!!!!            IF (ntry > 1000) ok=.TRUE.
          ELSE
            ok=.TRUE.
          ENDIF
        ENDDO

        IF (ityp ==2 .AND. ndat2 > 10) THEN
          WRITE(lfnprt,*) 'C2-ONLY:',isvn,ndat1,ndat2,stanam,rectyp
        ENDIF

        IF (ndat0 > 0) THEN
          IF (ndat >= minobs) THEN
            dcbpar(1:4,isat)=(/xmean,xsigma0,xsigma,xmed/)
            dcbcnt(1:4,isat)=(/ndat,ndat0-ndat,ndat1,ndat2/)

            WRITE(lfnprt,*) 'TRY',ntry,'TYP',ityp,'SVN',isvn,'#DAT/0/1/2', &
              ndat,ndat0,ndat1,ndat2,ndat0-ndat,(ndat0-ndat)*1d2/(1d0*ndat0)
            WRITE(lfnprt,*) 'RES',xres,'MEAN',xmean,'SIGMA',xsigma
          ELSE

          ENDIF
        ENDIF

      ENDDO

      nsat=0
      nsys=0
      DO isys=0,maxsys-1
        nsps=0
        xdat1=0d0
        xdat2=0d0
        DO isat=1,nrsat
          isvn=numsat(isat)
          IF (isvn/100 == isys .AND. dcbcnt(1,isat) > 0) THEN
            xdat1=xdat1+dcbpar(1,isat)
            xdat2=xdat2+dcbpar(2,isat)**2
            nsps=nsps+1
            IF (dcbpar(2,isat) == 0d0) THEN
!             Adapt to handle also P1-P2 biases
              WRITE(lfnerr,'(/,A,/,16X,A,I4,/,16X,A,I4,/,16X,A,A,/16X,A,A,/,16X,A,A)') &
                ' ### SR RNXDCB: No non-zero P-C code differences', &
                                'Code bias type:   ',ityp, &
                                'Satellite number: ',isvn, &
                                'Station name:     ',stanam, &
                                'Receiver type:    ',rectyp
            ENDIF
          ENDIF
        ENDDO
        IF (nsps > 0) THEN
          nsys=nsys+1
          dcbsys(nsys)=g_svnsys(isys)
          dcbid2(nsys)=stanam
          dcbva2(1,nsys)=0d0
          dcbva2(2,nsys)=SQRT(xdat2)/nsps

          DO isat=1,nrsat
            isvn=numsat(isat)
            IF (isvn/100 == isys .AND. dcbcnt(1,isat) > 0) THEN
              nsat=nsat+1
              dcbid1(nsat)=isvn
              dcbva1(1,nsat)=dcbpar(1,isat)
              dcbva1(2,nsat)=dcbpar(2,isat)
            ENDIF
          ENDDO
          IF (dcbva2(2,nsys) == 0d0) THEN
!           Adapt to handle also P1-P2 biases
            WRITE(lfnerr,'(/,A,/,16X,A,I4,/,16X,A,A,/,16X,A,A,/,16X,A,A)') &
              ' ### SR RNXDCB: No non-zero P-C code differences', &
                              'Code bias type:   ',ityp, &
                              'Satellite system: ',g_strsys(isys), &
                              'Station name:     ',stanam, &
                              'Receiver type:    ',rectyp
            nsys=nsys-1
            nsat=nsat-nsps
          ENDIF
        ENDIF
      ENDDO

      IF (nsat > 0) THEN
        title=filtitle
        title=filtitle(1:20)//stanam//rectyp
        IF (icbest == 1) THEN
          IF (ityp.EQ.1) THEN
            dcbaux=dcbfil(1:LENGT1(dcbfil))//'_2'
            icbtyp=2
          ELSEIF (ityp.EQ.2) THEN
            icbtyp=4
            dcbaux=dcbfil(1:LENGT1(dcbfil))//'_4'
          ELSE
            WRITE(*,*) '*** SR RNXDCB: Unknown icbest or ityp.'
            call exitrc(2)
          ENDIF
        ELSEIF (icbest == 2) THEN
          dcbaux=dcbfil(1:LENGT1(dcbfil))//'_1'
          icbtyp=1
        ELSE
          WRITE(*,*) '*** SR RNXDCB: Unknown icbest or ityp.'
          call exitrc(2)
        ENDIF

        WRITE(lfnprt,*) 'CONTROL:',ityp,nsat,stanam,rectyp,ndat0,ndat,nsat
        numifb=0
        CALL wtcbfl(dcbaux,title ,nsat,nsys,numifb,icbtyp,dcbid1, &
                    dcbva1,dcbid2,dcbva2,dcbsys,dcbid3,dcbva3, &
                    dcbin1,dcbin2,dcbin3)

! Update lists
! ------------
        DO ilst1=1,totsta
          IF (stalst(ilst1) == stanam) GOTO 101
        ENDDO
        totsta=totsta+1
        stalst(totsta)=stanam
        ilst1=totsta
101     CONTINUE
        stacnt(ityp,ilst1)=stacnt(ityp,ilst1)+1

        DO ilst2=1,totrec
          IF (reclst(ilst2) == rectyp) GOTO 102
        ENDDO
        totrec=totrec+1
        reclst(totrec)=rectyp
        ilst2=totrec
102     CONTINUE

! Save receiver list index in station list
! ----------------------------------------
        starec(ilst1)=ilst2

        DO isat=1,nsat
! Observation index
          totest=totest+1

! Value of observation
          estdat(1,totest)=dcbva1(1,isat)
! Estimated RMS error of observation
          estdat(2,totest)=dcbva1(2,isat)
! Define minimum rms value
!!          IF (estdat(2,totest) < 1d0) estdat(2,totest)=1d0
!!          IF (estdat(2,totest) < 0.1d0) estdat(2,totest)=0.1d0
          IF (estdat(2,totest) < 0.01d0) estdat(2,totest)=0.01d0

          isvn=dcbid1(isat)
          DO ilst=1,totsat
            IF (satlst(ilst) == isvn) GOTO 103
          ENDDO
          totsat=totsat+1
          satlst(totsat)=isvn
          ilst=totsat
103       CONTINUE

! DCB type (ityp): 1=P1-C1, 2=P2-C2, 3=P1-P2
          estind(1,totest)=ityp
! Satellite number
!!          estind(2,totest)=dcbid1(isat)
          estind(2,totest)=ilst
! File index
          estind(3,totest)=totfil+1
! Station name index
          estind(4,totest)=ilst1
! Receiver name index
          estind(5,totest)=ilst2
! Flag: 0=OK
          estind(6,totest)=0

        ENDDO

      ENDIF

    ENDDO

! Count total number of RINEX files
! ---------------------------------
    IF (icbtyp /= 0) totfil=totfil+1

  ELSEIF (icomb == 1 ) THEN

! Least-squares combination of individual RINEX DCB results
! ---------------------------------------------------------
!!    nobs=totsta*totsat
    nobs=totest
    npar=totsat+totsta*maxsys
    ALLOCATE(lobs(nobs),stat=iac)
    CALL alcerr(iac,'lobs',(/nobs/),'rnxdcb')
    ALLOCATE(pobs(nobs),stat=iac)
    CALL alcerr(iac,'pobs',(/nobs/),'rnxdcb')
    ALLOCATE(anor(npar*(npar+1)/2),stat=iac)
    CALL alcerr(iac,'anor',(/npar*(npar+1)/2/),'rnxdcb')
    ALLOCATE(aobs(nobs,npar),stat=iac)
    CALL alcerr(iac,'aobs',(/nobs,npar/),'rnxdcb')
    ALLOCATE(bnor(npar),stat=iac)
    CALL alcerr(iac,'bnor',(/npar/),'rnxdcb')
    ALLOCATE(xsol(npar),stat=iac)
    CALL alcerr(iac,'xsol',(/npar/),'rnxdcb')
    ALLOCATE(parind(3,npar),stat=iac)
    CALL alcerr(iac,'parind',(/3,npar/),'rnxdcb')
    ALLOCATE(vobs(2,nobs),stat=iac)
    CALL alcerr(iac,'vobs',(/2,nobs/),'rnxdcb')
    ALLOCATE(xdof(nobs),stat=iac)
    CALL alcerr(iac,'xdof',(/nobs/),'rnxdcb')

    nobs=totfil*totsat
    ALLOCATE(selind(nobs),stat=iac)
    CALL alcerr(iac,'selind',(/nobs/),'rnxdcb')

    DO ityp=1,maxtyp

! First combination using all observations
! ----------------------------------------
      estind(6,1:totest)=0
      niter=-1

      sysbad(ityp,:)=0
      satbad(ityp,:)=0
      stabad(ityp,:,:)=0
      recbad(ityp,:,:)=0

! Start of combination
! --------------------
200   CONTINUE
      niter=niter+1

250   CONTINUE
      nobs=0
      npar=0

      nsat=0
      nsta=0
      nfil=0

      aobs(:,:)=0d0
      anor(:)=0d0

      refsat(:)=0
      refsta(:)=0
      refsig(:)=1d20

      DO iobs=1,totest

        IF (estind(1,iobs) /= ityp) CYCLE

        isvn=satlst(estind(2,iobs))
        isys=isvn/100

!!        IF (isvn ==   1) CYCLE
!!        IF (isvn ==  25) CYCLE
!!        IF (isvn == 109) CYCLE

! List of exclusions:
!!        IF (isvn ==   8 .AND. dcbepo >= 50772d0 .AND. dcbepo < 50773d0) CYCLE
!!        IF (isvn ==  13 .AND. dcbepo >= 50675d0 .AND. dcbepo < 50676d0) CYCLE
!!        IF (isvn ==  20 .AND. dcbepo >= 50395d0 .AND. dcbepo < 50400d0) CYCLE
!!        IF (isvn ==  29 .AND. dcbepo >= 50591d0 .AND. dcbepo < 50592d0) CYCLE
!!        IF (isvn == 110 .AND. dcbepo >= 51095d0 .AND. dcbepo < 51096d0) CYCLE
!!        IF (isvn == 110 .AND. dcbepo >= 51099d0 .AND. dcbepo < 51100d0) CYCLE
!!        IF (isvn == 110 .AND. dcbepo >= 51103d0 .AND. dcbepo < 51104d0) CYCLE
!!        IF (isvn == 110 .AND. dcbepo >= 51115d0 .AND. dcbepo < 51116d0) CYCLE
!!        IF (isvn == 110 .AND. dcbepo >= 51155d0 .AND. dcbepo < 51156d0) CYCLE
!!        IF (isvn == 110 .AND. dcbepo >= 51278d0 .AND. dcbepo < 51279d0) CYCLE
!!        IF (isvn == 115 .AND. dcbepo >= 52319d0 .AND. dcbepo < 52320d0) CYCLE
!!        IF (isvn == 116 .AND. dcbepo >= 51865d0 .AND. dcbepo < 51866d0) CYCLE
!!        IF (isvn == 116 .AND. dcbepo >= 51881d0 .AND. dcbepo < 51882d0) CYCLE
!!        IF (isvn == 116 .AND. dcbepo >= 51929d0 .AND. dcbepo < 51930d0) CYCLE
!!        IF (isvn == 124 .AND. dcbepo >= 52182d0 .AND. dcbepo < 52183d0) CYCLE

!!        IF (stalst(estind(4,iobs))(1:4) == '----' .AND. dcbepo >= 00000d0 .AND. dcbepo < 99999d0) CYCLE

! Selection of particular receiver type or group
! ----------------------------------------------
        IF (LEN_TRIM(rectyp) > 0 .AND. &
            reclst(estind(5,iobs))(1:LEN_TRIM(rectyp)) /= TRIM(rectyp)) CYCLE

        IF (estdat(2,iobs) == 0d0) THEN
          WRITE(lfnerr,'(/,A,/,16X,A,I4,/,16X,A,I4,/,16X,A,A,/,16X,A,A)') &
            ' ### SR RNXDCB: Unverified P-C code difference estimate rejected', &
                            'Code bias type:   ',ityp, &
                            'Satellite number: ',isvn, &
                            'Station name:     ',stalst(estind(4,iobs)), &
                            'Receiver type:    ',reclst(estind(5,iobs))
          CYCLE
        ENDIF

        IF (estdat(2,iobs) > 1d0) THEN
          WRITE(lfnerr,'(/,A,/,16X,A,I4,/,16X,A,I4,/,16X,A,A,/,16X,A,A,/,16X,A,F8.3)') &
            ' ### SR RNXDCB: Inaccurate P-C code difference estimate rejected', &
                            'Code bias type:   ',ityp, &
                            'Satellite number: ',isvn, &
                            'Station name:     ',stalst(estind(4,iobs)), &
                            'Receiver type:    ',reclst(estind(5,iobs)), &
                            'RMS value:        ',estdat(2,iobs)
          CYCLE
        ENDIF

        IF (estind(6,iobs) /= 0) THEN
          WRITE(lfnerr,'(/,A,/,16X,A,I4,/,16X,A,I4,/,16X,A,A,/,16X,A,A,/,16X,A,I4)') &
            ' ### SR RNXDCB: Flagged P-C code difference estimate rejected', &
                            'Code bias type:   ',ityp, &
                            'Satellite number: ',isvn, &
                            'Station name:     ',stalst(estind(4,iobs)), &
                            'Receiver type:    ',reclst(estind(5,iobs)), &
                            'Flag code:        ',estind(6,iobs)
          CYCLE
        ENDIF

        nobs=nobs+1
        selind(nobs)=iobs

        IF (estdat(2,iobs) < refsig(isys+1)) refsig(isys+1)=estdat(2,iobs)

        DO ilst1=1,npar
          IF (parind(1,ilst1) /= 1) CYCLE
          IF (parind(2,ilst1) == estind(2,iobs)) GOTO 201
        ENDDO
        npar=npar+1
        nsat=nsat+1
        refsat(isys+1)=refsat(isys+1)+1
        parind(1,npar)=1
        parind(2,npar)=estind(2,iobs)
        parind(3,npar)=isys
        ilst1=npar
        dcbclu(1,nsat)=estind(2,iobs)
        dcbclu(2,nsat)=satlst(estind(2,iobs))
        dcbclu(3,nsat)=satlst(estind(2,iobs))
        dcbclu(4,nsat)=isys
        dcbclu(5,nsat)=0
        dcbclu(6,nsat)=0
201     CONTINUE

        DO ilst2=1,npar
          IF (parind(1,ilst2) /= 2) CYCLE
          IF (parind(2,ilst2) == estind(4,iobs) .AND. &
              parind(3,ilst2) == isys) GOTO 202
        ENDDO
        npar=npar+1
        nsta=nsta+1
        refsta(isys+1)=refsta(isys+1)+1
        nfil=nfil+stacnt(ityp,estind(4,iobs))
        parind(1,npar)=2
        parind(2,npar)=estind(4,iobs)
        parind(3,npar)=isys
        ilst2=npar
202     CONTINUE

        lobs(nobs)=estdat(1,iobs)
!!        pobs(nobs)=1d0
        pobs(nobs)=sigapr**2/estdat(2,iobs)**2

        aobs(nobs,ilst1)=1d0
        aobs(nobs,ilst2)=1d0

        DO ipar1=1,npar
          DO ipar2=ipar1,npar
            i1i2=IKF(ipar1,ipar2)
            anor(i1i2)=anor(i1i2)+aobs(nobs,ipar1)*pobs(nobs)*aobs(nobs,ipar2)
          ENDDO
        ENDDO

      ENDDO

      IF (nobs == 0 .OR. npar == 0) CYCLE

! Detect and reject unconnected DCB clusters
! ------------------------------------------
      isel=0
      ista=0
      DO iobs=1,nobs
        IF (estind(4,selind(iobs)) /= ista) THEN
          ista=estind(4,selind(iobs))
          dcbclu(6,1:maxsat)=0
          refclu(1:maxsys)=0
          DO i=iobs,nobs
            IF (estind(4,selind(i)) == ista) THEN
              DO isat=1,nsat
                IF (estind(2,selind(i)) == dcbclu(1,isat)) THEN
                  dcbclu(6,isat)=1
                  dcbclu(5,isat)=dcbclu(5,isat)+1
                  iclu=dcbclu(2,isat)
                  isys=dcbclu(4,isat)
                  IF (refclu(isys+1) == 0) refclu(isys+1)=iclu
                ENDIF
              ENDDO
            ENDIF
          ENDDO

          DO isat=1,nsat
            IF (dcbclu(6,isat) == 0) CYCLE
            iclu=dcbclu(2,isat)
            DO i=1,nsat
              IF (dcbclu(2,i) == iclu .AND. refclu(dcbclu(4,i)+1) /= 0) THEN
                dcbclu(2,i)=refclu(dcbclu(4,i)+1)
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDDO

      DO isys=0,maxsys-1
        nclu=0
        DO isat=1,nsat
          IF (dcbclu(4,isat) == isys) THEN
            DO iclu=1,nclu
              IF (dcbclu(2,isat) == selclu(1,iclu)) THEN
                selclu(2,iclu)=selclu(2,iclu)+dcbclu(5,isat)
                GOTO 251
              ENDIF
            ENDDO
            nclu=nclu+1
            selclu(1,nclu)=dcbclu(2,isat)
            selclu(2,nclu)=dcbclu(5,isat)
251         CONTINUE
          ENDIF
        ENDDO

        IF (nclu <= 1) CYCLE

        WRITE(lfnerr,'(/,A,/,16X,A,I4,/,16X,A,I4)') &
          ' ### SR RNXDCB: Multiple DCB clusters detected', &
                          'Code bias type:   ',ityp, &
                          '# of clusters:    ',nclu
        nsel=0
        DO iclu=1,nclu
          IF (selclu(2,iclu) > nsel) THEN
            isel=iclu
            nsel=selclu(2,iclu)
          ENDIF
          WRITE(lfnerr,'(16X,A,I4,/,16X,A,I4)') &
                            'Cluster number:   ',selclu(1,iclu), &
                            '# of stations:    ',selclu(2,iclu)
        ENDDO

        DO isat=1,nsat
          IF (dcbclu(4,isat) /= isys) CYCLE
          IF (dcbclu(2,isat) == selclu(1,isel)) CYCLE
          WRITE(lfnerr,'(/,A,/,16X,A,I4,/,16X,A,I4,/,16X,A,I4,/,16X,A,I4)') &
            ' ### SR RNXDCB: Unconnected DCB cluster detected and rejected', &
                            'Code bias type:   ',ityp, &
                            'Satellite number: ',dcbclu(3,isat), &
                            'Cluster number:   ',dcbclu(2,isat), &
                            '# of stations:    ',dcbclu(5,isat)
          DO iobs=1,nobs
            IF (estind(2,selind(iobs)) == dcbclu(1,isat)) THEN
              WRITE(lfnerr,'(/,A,/,16X,A,I4,/,16X,A,I4,/,16X,A,A,/16X,A,A)') &
                ' ### SR RNXDCB: Unconnected DCB cluster detected and rejected', &
                                'Code bias type:   ',ityp, &
                                'Satellite number: ',satlst(estind(2,selind(iobs))), &
                                'Station name:     ',stalst(estind(4,selind(iobs))), &
                                'Receiver type:    ',reclst(estind(5,selind(iobs)))
              estind(6,selind(iobs))=1
            ENDIF
          ENDDO
        ENDDO
      ENDDO

      IF (isel /= 0) GOTO 250

! Add weight matrix for datum definition specific to each system
! --------------------------------------------------------------
      WRITE(lfnprt,*) 'REFSAT/REFSTA/REFSIG:',refsat(:),refsta(:),refsig(:)
      sigref=1d-3
      DO ipar1=1,npar
        IF (parind(1,ipar1) /= 1) CYCLE
        isys=parind(3,ipar1)
        sigref=refsig(isys+1)/SQRT(refsat(isys+1)*refsta(isys+1)*1d0)
!!        IF (refsat(isys+1) > 1) sigref=1d-3
!!        IF (satlst(parind(2,ipar1)) ==   1 .OR. &
!!            satlst(parind(2,ipar1)) ==  25 .OR. &
!!            satlst(parind(2,ipar1)) == 109) CYCLE
        DO ipar2=ipar1,npar
          IF (parind(1,ipar2) /= 1) CYCLE
!!          IF (satlst(parind(2,ipar2)) ==   1 .OR. &
!!              satlst(parind(2,ipar2)) ==  25 .OR. &
!!              satlst(parind(2,ipar2)) == 109) CYCLE
          IF (parind(3,ipar2) /= parind(3,ipar1)) CYCLE
          i1i2=IKF(ipar1,ipar2)
          anor(i1i2)=anor(i1i2)+sigapr**2/sigref**2
        ENDDO
      ENDDO

! Count number of observed systems
! --------------------------------
      nsys=0
      DO isys=0,maxsys-1
        nsps=0
        DO ipar=1,npar
          IF (parind(3,ipar) == isys) nsps=nsps+1
        ENDDO
        IF (nsps > 0) nsys=nsys+1
      ENDDO

! Invert normal equation matrix
! -----------------------------
      CALL SYMINVG(npar,anor,0,ising)

      DO ipar=1,npar
        bnor(ipar)=0d0
        DO iobs=1,nobs
          bnor(ipar)=bnor(ipar)+aobs(iobs,ipar)*pobs(iobs)*lobs(iobs)
        ENDDO
      ENDDO

      CALL SOLVE(npar,anor,bnor,xsol)

! Compute residuals
! -----------------
!!      ndof=nobs-npar
      ndof=nobs-npar+nsys
      WRITE(lfnprt,*) 'NDOF/NOBS/NPAR/NSYS/NSAT/NSTA:',ndof,nobs,npar,nsys,nsat,nsta

! Compute DOF part for each contributing observation
! --------------------------------------------------
      xdat=0d0
      DO iobs=1,nobs
        xsum1=0d0
        DO ipar1=1,npar
          xsum2=0d0
          DO ipar2=1,npar
            xsum2=xsum2+aobs(iobs,ipar2)*anor(IKF(ipar1,ipar2))
          ENDDO
          xsum1=xsum1+xsum2*aobs(iobs,ipar1)
        ENDDO
        xdof(iobs)=1d0-pobs(iobs)*xsum1
        xdat=xdat+xdof(iobs)
      ENDDO

      sigfit=0d0
      DO iobs=1,nobs
        vobs(1,iobs)=0d0
        DO ipar=1,npar
          vobs(1,iobs)=vobs(1,iobs)+aobs(iobs,ipar)*xsol(ipar)
        ENDDO
        vobs(1,iobs)=vobs(1,iobs)-lobs(iobs)
        sigfit=sigfit+vobs(1,iobs)**2*pobs(iobs)
      ENDDO

      IF (ndof > 0) THEN
        sigfit=SQRT(sigfit/ndof)
      ELSE
        sigfit=sigapr
      ENDIF
      WRITE(lfnprt,*) 'SIGFIT-TOT:',ityp,sigfit,ndof

      xdat=0
      DO iobs=1,nobs
        IF (xdof(iobs) > 0d0) THEN
!!          vobs(2,iobs)=vobs(1,iobs)*SQRT(pobs(iobs))
!!          vobs(2,iobs)=vobs(1,iobs)*SQRT(pobs(iobs)/xdof(iobs))
          vobs(2,iobs)=vobs(1,iobs)/SQRT(xdof(iobs)/pobs(iobs))
        ELSE
          vobs(2,iobs)=0d0
        ENDIF
        xdat=xdat+vobs(2,iobs)**2
        WRITE(lfnprt,*) 'RES:',ityp,iobs,vobs(1,iobs),vobs(2,iobs), &
          estdat(2,selind(iobs)),satlst(estind(2,selind(iobs))), &
          stalst(estind(4,selind(iobs))),reclst(estind(5,selind(iobs)))
      ENDDO
      IF (nobs > 0) THEN
        xdat=SQRT(xdat/nobs)
      ELSE
        xdat=0d0
      ENDIF

      WRITE(lfnprt,*) 'STATISTICS (IYP/OBS/PAR/SAT/STA,SIGFIT):',ityp,nobs,npar,ndof,nsat,nsta,sigfit,xdat

! System
      DO isys=0,maxsys-1
        xdat=0d0
        xsum=0d0
        ndat=0
        DO iobs=1,nobs
          IF (satlst(estind(2,selind(iobs)))/100 /= isys) CYCLE
          xdat=xdat+vobs(1,iobs)**2*pobs(iobs)
          xsum=xsum+xdof(iobs)
          ndat=ndat+1
        ENDDO
        sigsys(isys+1)=0d0
        IF (IDNINT(xsum) > 0) THEN
          xdat=SQRT(xdat/xsum)
          sigsys(isys+1)=xdat
          WRITE(lfnprt,*) 'SIGFIT-SYS:',ityp,xdat,xsum,isys
!!        ELSEIF (xdat > 0d0) THEN
        ELSEIF (ndat > 0) THEN
!!          sigsys(isys+1)=sigfit
          sigsys(isys+1)=sigapr
        ENDIF
      ENDDO

! Satellite
      DO ilst=1,totsat
        xdat=0d0
        xsum=0d0
        DO iobs=1,nobs
          IF (estind(2,selind(iobs)) /= ilst) CYCLE
          xdat=xdat+vobs(1,iobs)**2*pobs(iobs)
          xsum=xsum+xdof(iobs)
        ENDDO
        IF (IDNINT(xsum) > 0) THEN
          xdat=SQRT(xdat/xsum)
          WRITE(lfnprt,*) 'SIGFIT-SAT:',ityp,xdat,xsum,satlst(ilst)
        ENDIF
      ENDDO

! System
      DO isys=0,maxsys-1
        xdat=0d0
        xsum=0d0
        ndat=0
        DO iobs=1,nobs
          IF (satlst(estind(2,selind(iobs)))/100 /= isys) CYCLE
          xdat=xdat+vobs(1,iobs)**2*pobs(iobs)
          xsum=xsum+xdof(iobs)
          ndat=ndat+1
        ENDDO
        sigsys(isys+1)=0d0
        IF (IDNINT(xsum) > 0) THEN
          xdat=SQRT(xdat/xsum)
          sigsys(isys+1)=xdat
          WRITE(lfnprt,*) 'SIGFIT-SYS:',ityp,xdat,xsum,isys
!!        ELSEIF (xdat > 0d0) THEN
        ELSEIF (ndat > 0) THEN
!!          sigsys(isys+1)=sigfit
          sigsys(isys+1)=sigapr
        ENDIF

! Station
        DO ilst=1,totsta
          xdat=0d0
          xsum=0d0
          DO iobs=1,nobs
            IF (satlst(estind(2,selind(iobs)))/100 /= isys) CYCLE
            IF (estind(4,selind(iobs)) /= ilst) CYCLE
            xdat=xdat+vobs(1,iobs)**2*pobs(iobs)
            xsum=xsum+xdof(iobs)
          ENDDO
          IF (IDNINT(xsum) > 0) THEN
            xdat=SQRT(xdat/xsum)
            WRITE(lfnprt,*) 'SIGFIT-STA(SYS):',ityp,xdat,xsum,stalst(ilst),isys
          ENDIF
        ENDDO

! Receiver
        DO ilst=1,totrec
          xdat=0d0
          xsum=0d0
          DO iobs=1,nobs
            IF (satlst(estind(2,selind(iobs)))/100 /= isys) CYCLE
            IF (estind(5,selind(iobs)) /= ilst) CYCLE
            xdat=xdat+vobs(1,iobs)**2*pobs(iobs)
            xsum=xsum+xdof(iobs)
          ENDDO
          IF (IDNINT(xsum) > 0) THEN
            xdat=SQRT(xdat/xsum)
            WRITE(lfnprt,*) 'SIGFIT-REC(SYS):',ityp,xdat,xsum,reclst(ilst),isys
          ENDIF
        ENDDO

      ENDDO

! Station
      DO ilst=1,totsta
        xdat=0d0
        xsum=0d0
        DO iobs=1,nobs
          IF (estind(4,selind(iobs)) /= ilst) CYCLE
          xdat=xdat+vobs(1,iobs)**2*pobs(iobs)
          xsum=xsum+xdof(iobs)
        ENDDO
        IF (IDNINT(xsum) > 0) THEN
          xdat=SQRT(xdat/xsum)
          WRITE(lfnprt,*) 'SIGFIT-STA:',ityp,xdat,xsum,stalst(ilst)
        ENDIF
      ENDDO

! Receiver
      DO ilst=1,totrec
        xdat=0d0
        xsum=0d0
        DO iobs=1,nobs
          IF (estind(5,selind(iobs)) /= ilst) CYCLE
          xdat=xdat+vobs(1,iobs)**2*pobs(iobs)
          xsum=xsum+xdof(iobs)
        ENDDO
        IF (IDNINT(xsum) > 0) THEN
          xdat=SQRT(xdat/xsum)
          WRITE(lfnprt,*) 'SIGFIT-REC:',ityp,xdat,xsum,reclst(ilst)
        ENDIF
      ENDDO

! Find outliers
! -------------
      xdat1=0d0
      xdat2=0d0
      ilst1=0
      ilst2=0
      DO iobs=1,nobs

! Check residual value
! --------------------
        IF (ABS(vobs(1,iobs)) > xdat1) THEN
          xdat1=ABS(vobs(1,iobs))
          ilst1=iobs
        ENDIF

! Check normalized residual value
! -------------------------------
        isys=satlst(estind(2,selind(iobs)))/100
        IF (sigsys(isys+1) > 0d0) THEN
!!          vobs(2,iobs)=vobs(1,iobs)*SQRT(pobs(iobs))/sigsys(isys+1)
          vobs(2,iobs)=vobs(2,iobs)/sigsys(isys+1)
        ELSE
          vobs(2,iobs)=0d0
        ENDIF
        IF (ABS(vobs(2,iobs)) > xdat2) THEN
          xdat2=ABS(vobs(2,iobs))
          ilst2=iobs
        ENDIF
      ENDDO
      WRITE(lfnprt,*) 'BIGGEST RES:',ityp,niter,xdat1,xdat2,vobs(1,ilst1),vobs(2,ilst2)

! Flag outliers
! -------------
!!      nflag=0
!!      IF (xdat1 > 10d0) THEN
!!        WRITE(lfnerr,'(/,A,/,16X,A,I4,/,16X,A,I4,/,16X,A,A,/,16X,A,A,/,16X,A,F8.3,/,16X,A,I4)') &
!!          ' ### SR RNXDCB: P-C code difference estimate rejected', &
!!                          'Code bias type:   ',ityp, &
!!                          'Satellite number: ',satlst(estind(2,selind(ilst1))), &
!!                          'Station name:     ',stalst(estind(4,selind(ilst1))), &
!!                          'Receiver type:    ',reclst(estind(5,selind(ilst1))), &
!!                          'Residual:         ',vobs(1,ilst1), &
!!                          'Iteration index:  ',niter
!!        estind(6,selind(ilst1))=1
!!        nflag=nflag+1
!!        WRITE(*,*) 'OUTLIER1:',ityp,niter,vobs(1,ilst1),satlst(estind(2,selind(ilst1))), &
!!          stalst(estind(4,selind(ilst1))),reclst(estind(5,selind(ilst1)))
!!      ENDIF
!!      isys=satlst(estind(2,selind(ilst2)))/100
!!!!      IF (xdat2 > 4d0) THEN
!!      IF (xdat2 > 2.5d0) THEN
!!        WRITE(lfnerr,'(/,A,/,16X,A,I4,/,16X,A,I4,/,16X,A,A,/,16X,A,A,/,16X,A,F8.3,/,16X,A,I4)') &
!!          ' ### SR RNXDCB: P-C code difference estimate rejected', &
!!                          'Code bias type:   ',ityp, &
!!                          'Satellite number: ',satlst(estind(2,selind(ilst2))), &
!!                          'Station name:     ',stalst(estind(4,selind(ilst2))), &
!!                          'Receiver type:    ',reclst(estind(5,selind(ilst2))), &
!!                          'Normalized res.:  ',vobs(2,ilst2), &
!!                          'Iteration index:  ',niter
!!        estind(6,selind(ilst2))=1
!!        nflag=nflag+1
!!        WRITE(lfnprt,*) 'OUTLIER2:',ityp,niter,vobs(2,ilst2),satlst(estind(2,selind(ilst2))), &
!!          stalst(estind(4,selind(ilst2))),reclst(estind(5,selind(ilst2)))
!!      ENDIF
!!      IF (nflag > 0) WRITE(lfnprt,*) 'FLAGGED OBS:',niter,nflag,ityp,stanam,rectyp

      nflag=0
      DO iobs=1,nobs
        IF (ABS(vobs(2,iobs)) > 4d0) THEN
          WRITE(lfnerr,'(/,A,/,16X,A,I4,/,16X,A,I4,/,16X,A,A,/,16X,A,A,/,16X,A,F8.3,/,16X,A,F8.3,/,16X,A,I4)') &
            ' ### SR RNXDCB: P-C code difference estimate rejected', &
                            'Code bias type:   ',ityp, &
                            'Satellite number: ',satlst(estind(2,selind(iobs))), &
                            'Station name:     ',stalst(estind(4,selind(iobs))), &
                            'Receiver type:    ',reclst(estind(5,selind(iobs))), &
                            'Normalized res.:  ',vobs(2,iobs), &
                            'Residual:         ',vobs(1,iobs), &
                            'Iteration index:  ',niter
          estind(6,selind(iobs))=1
          nflag=nflag+1

          isys=satlst(estind(2,selind(iobs)))/100
          sysbad(ityp,isys+1)=sysbad(ityp,isys+1)+1
          ilst=estind(2,selind(iobs))
          satbad(ityp,ilst)=satbad(ityp,ilst)+1
          ilst=estind(4,selind(iobs))
          stabad(ityp,ilst,isys+1)=stabad(ityp,ilst,isys+1)+1
          ilst=estind(5,selind(iobs))
          recbad(ityp,ilst,isys+1)=recbad(ityp,ilst,isys+1)+1

          WRITE(lfnprt,*) 'OUTLIER:',isys,satlst(estind(2,selind(iobs))), &
            sigsys(isys+1),vobs(2,iobs), &
            stalst(estind(4,selind(iobs))),reclst(estind(5,selind(iobs)))
        ENDIF
      ENDDO
      IF (nflag > 0) WRITE(lfnprt,*) 'FLAGGED OBS:',nflag,ityp,stanam,rectyp

! Redo combination in case of detected outliers (flagged observations)
! --------------------------------------------------------------------
!!      nflag=0
      IF (nflag > 0) GOTO 200

! End of combination
! ------------------

! Save combined DCB information
! -----------------------------
      nsat=0
      nsta=0
      DO ipar=1,npar
        isys=parind(3,ipar)
        IF (parind(1,ipar) == 1) THEN
          nsat=nsat+1
          dcbid1(nsat)=satlst(parind(2,ipar))
          dcbva1(1,nsat)=xsol(ipar)
!!          dcbva1(2,nsat)=sigfit*SQRT(anor(IKF(ipar,ipar)))
          dcbva1(2,nsat)=sigsys(isys+1)*SQRT(anor(IKF(ipar,ipar)))
        ELSEIF (parind(1,ipar) == 2) THEN
          nsta=nsta+1
          dcbsys(nsta)=g_svnsys(parind(3,ipar))
          dcbid2(nsta)=stalst(parind(2,ipar))
          dcbva2(1,nsta)=xsol(ipar)
!!          dcbva2(2,nsta)=sigfit*SQRT(anor(IKF(ipar,ipar)))
          dcbva2(2,nsta)=sigsys(isys+1)*SQRT(anor(IKF(ipar,ipar)))
        ENDIF
      ENDDO

      title=filtitle
      WRITE(title(1:64),'(A,I4,A,I4,A,I4,A,2I4,A,2F8.3)') &
        '#SAT:',nsat,' #STA:',nsta,' #FIL:',nfil,' #OUT:',sysbad(ityp,1:2),' RMS:',sigsys(1:2)
      IF (ityp.EQ.1) THEN
        dcbaux=dcbfil(1:LENGT1(dcbfil))//'C1'
!!        dcbaux=dcbfil
        icbtyp=2
      ELSE
        dcbaux=dcbfil(1:LENGT1(dcbfil))//'C2'
        icbtyp=4
      ENDIF
      WRITE(lfnprt,*) 'STATISTICS (IYP/OBS/PAR/SAT/STA):', &
        ityp,nobs,npar,ndof,nsat,nsta
      numifb=0
      CALL wtcbfl(dcbaux,title ,nsat,nsta,numifb,icbtyp,dcbid1, &
                  dcbva1,dcbid2,dcbva2,dcbsys,dcbid3,dcbva3, &
                  dcbin1,dcbin2,dcbin3)
    ENDDO

    DO ityp=1,maxtyp
      WRITE(lfnprt,*) 'NUMBER OF OUTLIERS PER SYSTEM:',ityp,sysbad(ityp,1:maxsys)
      DO ilst=1,totsat
        WRITE(lfnprt,*) 'NUMBER OF OUTLIERS PER SATELLITE:',ityp,satbad(ityp,ilst),satlst(ilst)
      ENDDO
      DO ilst=1,totsta
        WRITE(lfnprt,*) 'NUMBER OF OUTLIERS PER STATION:  ',ityp,stabad(ityp,ilst,1:maxsys),stalst(ilst),reclst(starec(ilst))
      ENDDO
      DO ilst=1,totrec
        WRITE(lfnprt,*) 'NUMBER OF OUTLIERS PER RECEIVER: ',ityp,recbad(ityp,ilst,1:maxsys),reclst(ilst)
      ENDDO
    ENDDO

    DEALLOCATE(lobs,stat=iac)
    DEALLOCATE(pobs,stat=iac)
    DEALLOCATE(anor,stat=iac)
    DEALLOCATE(aobs,stat=iac)
    DEALLOCATE(bnor,stat=iac)
    DEALLOCATE(xsol,stat=iac)
    DEALLOCATE(parind,stat=iac)
    DEALLOCATE(vobs,stat=iac)
    DEALLOCATE(xdof,stat=iac)
    DEALLOCATE(selind,stat=iac)

  ELSE
    WRITE(lfnerr,'(/,A,/,16X,A,I4)') &
      ' *** SR RNXDCB: Illegal comination flag', &
                      'Combination flag:',icomb
    CALL exitrc(2)

  ENDIF

END SUBROUTINE rnxdcb

END MODULE
