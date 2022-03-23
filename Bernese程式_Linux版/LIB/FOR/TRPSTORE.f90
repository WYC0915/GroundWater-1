MODULE s_TRPSTORE
CONTAINS


! --------------------------------------------------------------------------
! Bernese Software Version 5.2
! --------------------------------------------------------------------------

SUBROUTINE trpstore(neq)

! --------------------------------------------------------------------------
! Purpose:    Print troposphere output and write BERNESE and SINEX
!             troposphere files
!
! Remark:     trpstore should be called after sr crdstore!!
!
! Author:     M. Meindl
!
! Created:    21-May-2003
!
! Changes:    14-Jun-2003 MM: bugfix wrt "except of boundaries" and "delete"
!             26-Aug-2003 RD: Preelimnation exception and number of NEQ-files
!             18-Nov-2003 HB: SIZE(grdIdx,2) instead of SIZE(grdIdx(1,:))
!             06-Feb-2004 HB: SIZE(opt%elimi,1) instead of SIZE(opt%elimi)
!             29-Mar-2004 CU: Add dummy variable to call of sr tropos
!             01-Jul-2004 CU: Initialize variable nEli
!             17-Aug-2004 HU: Unused variable removed
!             07-Feb-2005 MM: Do not store parameters w/o neighbours
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             13-Dec-2005 CU: Adapt call of SR tropos
!             24-Aug-2006 AG: SR tdelay instead of tropos used
!             14-Mar-2007 AG: dR set to zero in case of meteo files
!             13-Feb-2008 RD: Adapt to par%name == chr*20
!             30-May-2008 RD: Use x0 instead of tdelay for apriori tropo
!             21-Jul-2008 PS: Format statement corrected (GRD=linear)
!             18-Aug-2010 RD: Improved singularity handling
!             08-Oct-2010 RD: Introduce undef_Trp
!             19-Jan-2011 RD: Adapt to new call of GETSTAF
!             01-Sep-2011 LP: Enable writing of tropo gradients in TRO SINEX
!             20-Sep-2012 RD: Correctly deallocate arrays
!             20-Sep-2012 RD: Use M_BERN with ONLY
!             20-Sep-2012 RD: Remove unused modules
!             06-Jun-2013 SS: Set particular array element (of ipara) in any case
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! --------------------------------------------------------------------------


! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfnprt, &
                      fileNameLength, staNameLength
  USE d_datum,  ONLY: datum
  USE d_const,  ONLY: pi
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: opt, comstat
  USE d_trpest, ONLY: t_trpest, init_trpEst, dtMax, undef_Trp
  USE d_const,  ONLY: filTitle

  USE f_ikf
  USE s_alcerr
  USE s_trpvec1
  USE s_wttrpe
  USE s_getstaf
  USE s_wttrpsnx
  IMPLICIT NONE


! List of Parameters
! ------------------
! input
  TYPE(t_neq)                                     :: neq  ! Normal equation


! Local Variables
! ---------------
! general
  CHARACTER(LEN=126)                              :: line
  TYPE(t_trpest)                                  :: trpEst
  CHARACTER(LEN=fileNameLength)                   :: filNam,eccnam=''
  INTEGER(i4b), DIMENSION(1,1)                    :: locq
  CHARACTER(LEN=20)                               :: staOld
  CHARACTER(LEN=8)                                :: srName = "trpstore"

! variables for getstaf
  REAL(r8b), DIMENSION(:,:), ALLOCATABLE          :: xstat
  REAL(r8b), DIMENSION(:,:), ALLOCATABLE          :: xstell
  REAL(r8b), DIMENSION(:,:), ALLOCATABLE          :: xstecc
  CHARACTER(LEN=16), DIMENSION(:), ALLOCATABLE    :: stanam
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE         :: stanum
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE         :: icentr
  CHARACTER(LEN=1), DIMENSION(:), ALLOCATABLE     :: staflg
  INTEGER(i4b)                                    :: nCentr

! some troposphere variables
  INTEGER(i4b), DIMENSION(2)                      :: iTrGrd
  INTEGER(i4b)                                    :: iElvnq
  REAL(r8b)                                       :: minZpd, minGrd, minHlp
  REAL(r8b)                                       :: zenMax
  REAL(r8b)                                       :: xEleRef, xFac
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE         :: trpIdx
  INTEGER(i4b), DIMENSION(:,:), ALLOCATABLE       :: grdIdx
  INTEGER(i4b)                                    :: iExtra
  INTEGER(i4b)                                    :: iTropo, iTropM
  REAL(r8b)                                       :: timZpd
  REAL(r8b),DIMENSION(12)                         :: grdInf
  INTEGER(i4b),DIMENSION(:,:), ALLOCATABLE        :: iPara
  INTEGER(i4b)                                    :: iVal1, iVal2
  REAL(r8b)                                       :: dR
  REAL(r8b),DIMENSION(:,:), ALLOCATABLE           :: grdTim

! indices and loop variables
  INTEGER(i4b)                                    :: nTrp, iTrp
  INTEGER(i4b)                                    :: nGrd, iGrd
  INTEGER(i4b)                                    :: nSta, iSta
  INTEGER(i4b)                                    :: nZpd
  INTEGER(i4b)                                    :: iPar, iIdx
  INTEGER(i4b)                                    :: iIdx1, iIdx2
  INTEGER(i4b)                                    :: ii, i1, i2
  INTEGER(i4b)                                    :: iEli, nEli
  INTEGER(i4b)                                    :: iFil, iExcp

! boundary problem variables
  REAL(r8b), DIMENSION(:,:), ALLOCATABLE          :: trpWin
  REAL(r8b)                                       :: tTrp, tPrv, tNxt

! error and return codes
  INTEGER(i4b)                                    :: iac, irc, irCode


! Some initializations
! --------------------
  minZpd    = 1.d20
  minGrd    = 1.d20
  iTropo    = neq%misc%iTropo
  iExtra    = neq%misc%iExtra
  iElvnq    = neq%misc%iElvnq
  iTrGrd(1) = neq%misc%iTrGrd
  iTrGrd(2) = 0
  nEli      = 0


! Adjust troposphere model
! ------------------------
  IF (iExtra==0) THEN
    iTropM = iTropo
  ELSE IF (iExtra==1) THEN
    iTropM = -iTropo
  ELSE IF (iExtra==2) THEN
    iTropM = iTropo+100
  END IF


! Count number of troposphere parameters
! --------------------------------------
  nTrp   = 0
  nZpd   = 0
  nGrd   = 0
  nSta   = 0
  staOld = " "

  IF (iTropo /= undef_Trp) THEN
    DO iPar=1,neq%misc%nPar
      IF (neq%par(iPar)%locq(1)/=6) CYCLE
      IF (neq%aNor(ikf(iPar,iPar))==0.d0 .AND.          &
          neq%bNor(iPar)==0.d0 .AND. neq%xxx(iPar)==0.d0) CYCLE

! new station
      IF (neq%par(iPar)%name/=staOld) THEN
        staOld = neq%par(iPar)%name
        nSta   = nSta+1
      END IF

      nTrp = nTrp+1
      IF (neq%par(iPar)%locq(4)==3) nZpd = nZpd+1
      IF (neq%par(iPar)%locq(4)==1) nGrd = nGrd+1
    END DO
  END IF


! No troposphere available
! ------------------------
  IF (nTrp==0) RETURN

  IF (nZpd==0) THEN
    write(lfnerr,"(/,A,/,A,/)")                                            &
      " ## sr trpstore: Gradients estimated without zenith path delay",    &
      "                 No troposphere files written"
    RETURN
  END IF

  IF (nGrd==0) iTrGrd(1) = 0


! Print header section
! --------------------
  write(lfnprt,"(//A,/,A,/)") " Troposphere parameters:",                  &
                              " -----------------------"
  IF (iTrGrd(1)>0 .AND. iElvnq>=3) THEN
    zenMax = (90.d0-iElvnq)/180.d0*pi
    IF (iTrGrd(1)==1.OR.iTrGrd(1)==3) THEN
      xEleRef = 51.83D0
      xFac    = TAN(zenMax)/COS(zenMax)
    ELSE
      xEleRef = 45.D0
      xFac    = TAN(zenMax)
    ENDIF

    write(lfnprt,"(A,F6.1,A,/,A,I6,A,/,A,F6.1,/)")                         &
      " Reference elevation angle of gradient terms : ",xEleRef," degrees",&
      " Minimum elevation angle                     : ",iElvnq," degrees", &
      " Mapping factor at minimum elevation angle   : ",xFac
  END IF

  write(lfnprt,"(2A,/2A,/,1X,124('-'))")                                   &
      '                         Corrections (m)             RMS errors',   &
      ' (m)             Zenith vector (")         Error ellipse (m)   ',   &
      ' Station name        North    East    Zenith     North    East ',   &
      '   Zenith     Angle   RMS   Ratio  Azi   Max RMS  Min RMS  Azi '


! Allocate some arrays
! --------------------
  CALL init_trpEst(trpEst)

  trpEst%nRec = nZpd
  trpEst%nSta = nSta
  ALLOCATE(iCentr(nSta),stat=iac)
  CALL alcerr(iac,"iCentr",(/nSta/),srName)
  ALLOCATE(staNam(nSta),stat=iac)
  CALL alcerr(iac,"staNam",(/nSta/),srName)
  ALLOCATE(staNum(nSta),stat=iac)
  CALL alcerr(iac,"staNum",(/nSta/),srName)
  ALLOCATE(staFlg(nSta),stat=iac)
  CALL alcerr(iac,"staFlg",(/nSta/),srName)
  ALLOCATE(xStEll(3,nSta),stat=iac)
  CALL alcerr(iac,"xStEll",(/3,nSta/),srName)
  ALLOCATE(xStat(3,nSta),stat=iac)
  CALL alcerr(iac,"xStat",(/3,nSta/),srName)
  ALLOCATE(xStEcc(3,nSta),stat=iac)
  CALL alcerr(iac,"xStEcc",(/3,nSta/),srName)

  ALLOCATE(trpEst%sta(nSta),stat=iac)
  CALL alcerr(iac,"trpEst%sta",(/nSta/),srName)
  ALLOCATE(trpIdx(nZpd),stat=iac)
  CALL alcerr(iac,"trpIdx",(/nZpd/),srName)
  ALLOCATE(ipara(nZpd,5),stat=iac)
  CALL alcerr(iac,"ipara",(/nZpd,5/),srName)
  ALLOCATE(grdTim(nZpd,2),stat=iac)
  CALL alcerr(iac,"grdTim",(/nZpd,2/),srName)

  trpIdx = 0
  ipara  = 0
  grdTim = 0

  IF (nGrd/=0) THEN
    ALLOCATE(grdIdx(2,nGrd),stat=iac)
    CALL alcerr(iac,"grdIdx",(/2,nGrd/),srName)
    grdIdx = 0
  END IF

  trpEst%sta(:)%nTrp = 0


! Fill arrays
! -----------
  staOld = " "
  iSta   = 0
  iTrp   = 0
  iGrd   = 0

  DO iPar=1,neq%misc%nPar
    IF (neq%par(iPar)%locq(1)/=6) CYCLE
    IF (neq%aNor(ikf(iPar,iPar))==0.d0 .AND.          &
        neq%bNor(iPar)==0.d0 .AND. neq%xxx(iPar)==0.d0) CYCLE

! new station
    IF (neq%par(iPar)%name/=staOld) THEN
      staOld = neq%par(iPar)%name
      iSta   = iSta+1
      trpEst%sta(iSta)%staNam = staOld(1:staNameLength)
      trpEst%sta(iSta)%staFlg = 'A'
      staNam(iSta)            = staOld(1:staNameLength)
    END IF

! zenith parameter
    IF (neq%par(iPar)%locq(4)==3) THEN
      trpEst%sta(iSta)%nTrp = trpEst%sta(iSta)%nTrp+1
      iTrp = iTrp+1
      trpIdx(iTrp) = iPar
    END IF

! gradients
    IF (neq%par(iPar)%locq(4)==1) THEN
      iGrd = iGrd+1
      grdIdx(1,iGrd) = iPar
    END IF
    IF (neq%par(iPar)%locq(4)==2) grdIdx(2,iGrd) = iPar
  END DO


! Allocate some arrays
! --------------------
  DO iSta=1,trpEst%nSta
    nTrp = trpEst%sta(iSta)%nTrp
    ALLOCATE(trpEst%sta(iSta)%trp(nTrp),stat=iac)
    CALL alcerr(iac,"trpEst%sta%trp",(/nTrp/),srName)
    DO iTrp=1,nTrp
      trpEst%sta(iSta)%trp(iTrp)%corr   = 0.d0
      trpEst%sta(iSta)%trp(iTrp)%sigma  = 0.d0
      trpEst%sta(iSta)%trp(iTrp)%timInt = 0.d0
      trpEst%sta(iSta)%trp(iTrp)%total  = 0.d0
      trpEst%sta(iSta)%trp(iTrp)%model  = 0.d0
    END DO
  END DO


! Get station coordinates
! -----------------------
  filNam = opt%coordrs
  IF (LEN_TRIM(filNam)==0) filNam = opt%coord
  nSta = trpEst%nSta

  CALL getStaf(filNam,eccNam,nSta,staNam,staNum,nCentr,iCentr,xStat,xStEll,&
               xStEcc,datum%name,datum%aEll,datum%bEll,datum%dxEll,        &
               datum%drEll,datum%scEll)


! Loop over all stations
! ----------------------
  DO iSta=1,trpEst%nSta

! find first zenith delay
    iIdx = 1
    DO ii=1,SIZE(trpIdx)
      IF (neq%par(trpIdx(ii))%name(1:staNameLength)== &
                                  trpEst%sta(iSta)%staNam) THEN
        iIdx = ii
        EXIT
      ENDIF
    END DO


! Loop over all parameters
! ------------------------
    DO iTrp=1,trpEst%sta(iSta)%nTrp
      iPar = trpIdx(iIdx+iTrp-1)
      IF (neq%aNor(ikf(iPar,iPar))==0.d0 .AND.          &
          neq%bNor(iPar)==0.d0 .AND. neq%xxx(iPar)==0.d0) CYCLE

      timZpd = neq%par(iPar)%time%mean
      IF (iExtra==0) THEN

! model set to zero if observed meteo was used
        dR = 0.d0
      ELSE
        dR = neq%par(iPar)%x0
      END IF

! fill array
      trpEst%sta(iSta)%trp(iTrp)%timInt(1) = timZpd
      trpEst%sta(iSta)%trp(iTrp)%model     = dR
      trpEst%sta(iSta)%trp(iTrp)%total     = dR+neq%xxx(iPar)
      trpEst%sta(iSta)%trp(iTrp)%corr(3)   = neq%xxx(iPar)
      trpEst%sta(iSta)%trp(iTrp)%sigma(3)  =                               &
                           comstat%rms*sqrt(neq%aNor(ikf(iPar,iPar)))
      grdInf = 0.d0

! Set this particular array element in any case (with/without gradients)
      iPara(iIdx+iTrp-1,5) = iPar

! Gradients estimated?
! --------------------
      IF (iTrGrd(1)/=0) THEN

! search last and next gradient pair
        DO ii=1,SIZE(grdIdx,2)
          iIdx1 = grdIdx(1,ii)
          iIdx2 = grdIdx(2,ii)
          IF (iIdx1 == 0 .OR. iIdx2 == 0) CYCLE
          IF (trpEst%sta(iSta)%staNam/=neq%par(iIdx1)%name(1:staNameLength))THEN
            CYCLE
          ENDIF
          ! Skip singular parameters
          IF (neq%aNor(ikf(iIdx1,iIdx1))==0.d0 .AND.          &
              neq%bNor(iIdx1)==0.d0 .AND. neq%xxx(iIdx1)==0.d0) CYCLE
          IF (neq%aNor(ikf(iIdx2,iIdx2))==0.d0 .AND.          &
              neq%bNor(iIdx2)==0.d0 .AND. neq%xxx(iIdx2)==0.d0) CYCLE

          IF (timZpd>=neq%par(iIdx1)%time%mean .OR.                        &
              abs(timZpd-neq%par(iIdx1)%time%mean)<=dtMax) THEN
            iPara(iIdx+iTrp-1,1:2) = (/grdIdx(1,ii),grdIdx(2,ii)/)
            grdTim(iIdx+iTrp-1,1) = neq%par(iIdx1)%time%mean
          END IF
        END DO

        DO ii=SIZE(grdIdx,2),1,-1
          iIdx1 = grdIdx(1,ii)
          iIdx2 = grdIdx(2,ii)
          IF (iIdx1 == 0 .OR. iIdx2 == 0) CYCLE
          IF (trpEst%sta(iSta)%staNam/=neq%par(iIdx1)%name(1:staNameLength))THEN
            CYCLE
          ENDIF
          ! Skip singular parameters
          IF (neq%aNor(ikf(iIdx1,iIdx1))==0.d0 .AND.          &
              neq%bNor(iIdx1)==0.d0 .AND. neq%xxx(iIdx1)==0.d0) CYCLE
          IF (neq%aNor(ikf(iIdx2,iIdx2))==0.d0 .AND.          &
              neq%bNor(iIdx2)==0.d0 .AND. neq%xxx(iIdx2)==0.d0) CYCLE
          IF (timZpd<=neq%par(iIdx1)%time%mean .OR.                        &
              abs(timZpd-neq%par(iIdx1)%time%mean)<=dtMax) THEN
            iPara(iIdx+iTrp-1,3:4) = (/grdIdx(1,ii),grdIdx(2,ii)/)
            grdTim(iIdx+iTrp-1,2) = neq%par(iIdx1)%time%mean
          END IF
        END DO

! no gradient found
        IF (iPara(iIdx+iTrp-1,1)==0 .OR. iPara(iIdx+iTrp-1,2)==0 .OR.       &
            iPara(iIdx+iTrp-1,3)==0 .OR. iPara(iIdx+iTrp-1,4)==0     ) THEN
          write(lfnerr,"(/,A,/,A,A16,/,A,F12.6,/)")                        &
                " ### sr trpstore: Gradient parameter missing",            &
                "                  Station: ",trpEst%sta(iSta)%staNam,     &
                "                  Time   : ",timZpd
        ELSE


! Compute gradient and rms for requested time
! -------------------------------------------
          CALL trpvec1(iPara(iIdx+iTrp-1,:),dR,timZpd,neq,iTrGrd,grdInf)
          trpEst%sta(iSta)%trp(iTrp)%corr(1:2)  = (/grdInf(9),grdInf(11)/)
          trpEst%sta(iSta)%trp(iTrp)%sigma(1:2) = (/grdInf(10),grdInf(12)/)

! compute gradient spacing
          minHlp = neq%par(iPara(iIdx+iTrp-1,3))%time%mean- &
                   neq%par(iPara(iIdx+iTrp-1,1))%time%mean
          IF (minHlp<minGrd .AND. minHlp/=0.d0) minGrd = minHlp
        END IF
      END IF

! Write output line
! -----------------
      line  = " "
      iVal1 = nint(grdInf(4))
      iVal2 = nint(grdInf(8))
      IF (iTrGrd(1)>0) THEN
        IF (iTrGrd(1)==1.OR.iTrGrd(1)==3) THEN
          write(line(74:126),"(2X,3F7.1,I5,1X,2F9.5,I5)")                  &
            (grdInf(i1),i1=1,3),iVal1,(grdInf(i2),i2=6,7),iVal2
        ELSE
          write(line(74:126),"(28X,1X,2F9.5,I5)")                          &
            (grdInf(i2),i2=6,7),iVal2
        ENDIF
        write(line(19:27),"(F9.5)") grdInf(9)
        write(line(28:36),"(F9.5)") grdInf(11)
        write(line(47:55),"(F9.5)") grdInf(10)
        write(line(56:64),"(F9.5)") grdInf(12)
      END IF
      write(line(1:18),"(1X,A16)") trpEst%sta(iSta)%staNam
      write(line(37:45),"(F9.5)") trpEst%sta(iSta)%trp(iTrp)%corr(3)
      write(line(65:73),"(F9.5)") trpEst%sta(iSta)%trp(iTrp)%sigma(3)
      write(lfnprt,"(A)") line
    END DO
  END DO


! Compute parameter resolution
! ----------------------------
  DO iSta=1,trpEst%nSta
    DO iTrp=1,trpEst%sta(iSta)%nTrp-1
      minHlp = trpEst%sta(iSta)%trp(iTrp+1)%timInt(1)-                     &
               trpEst%sta(iSta)%trp(iTrp)%timInt(1)
      IF (minHlp<minZpd) minZpd = minHlp
    END DO
  END DO
  IF (minGrd==1.d20 .AND. iTrGrd(1)>0) THEN
    iTrGrd(2) = 1
  ELSE IF (minZpd/=0.d0 .AND. iTrGrd(1)>0) THEN
    iTrGrd(2) = NINT(minGrd/minZpd)
  END IF

! Some information for header
! ---------------------------
  trpEst%head%title  = filTitle
  trpEst%head%iFrmt  = 1
  trpEst%head%iTrpMd = iTropM
  trpEst%head%iTrMap = neq%misc%iTrMap
  trpEst%head%iTrGrd = iTrGrd
  trpEst%head%iElvnq = iElvnq
  trpEst%head%iTab   = minZpd


! Get rid of "left over" parameters due to EXCEPT OF BOUNDARIES
! -------------------------------------------------------------
! define windows for "good" parameters
  DO iEli=1,SIZE(opt%elimi,1)
    IF (opt%elimi(iEli)%mode/=5) CYCLE
    IF (opt%elimi(iEli)%locq(1)/=6 .OR. opt%elimi(iEli)%locq(4)/=3) CYCLE

    nEli = SIZE(opt%elimi(iEli)%excp)
    IF (nEli>0) THEN
      ALLOCATE(trpWin(2,nEli),stat=iac)
      CALL alcerr(iac,"trpWin",(/nEli/),srName)
      DO iExcp=1,nEli
        iFil = opt%elimi(iEli)%excp(iExcp)
        IF (iFil > SIZE (opt%neqFileName)) CYCLE
        trpWin(:,iExcp) = comstat%taecml(:,1,iFil)
      END DO
      EXIT
    END IF
  END DO

! flag "left overs" with t=0.d0
  IF (nEli>0) THEN
    DO iSta=1,trpEst%nSta
      TrpLoop: DO iTrp=1,trpEst%sta(iSta)%nTrp
        tTrp = trpEst%sta(iSta)%trp(iTrp)%timInt(1)
        DO iExcp=1,nEli
          IF (tTrp>trpWin(1,iExcp)-dtMax .AND.                             &
              tTrp<trpWin(2,iExcp)+dtMax      ) CYCLE TrpLoop
        END DO
        trpEst%sta(iSta)%trp(iTrp)%timInt(1) = 0.d0
      END DO TrpLoop
    END DO
  END IF


! Get rid of parameters w/o neighbours
! ------------------------------------
  DO iSta=1, trpEst%nSta
    IF (trpEst%sta(iSta)%nTrp == 0) CYCLE ! Gradient w/o zenit parameter

    tTrp = 0.d0
    tNxt = trpEst%sta(iSta)%trp(1)%timInt(1)
    DO iTrp=1, trpEst%sta(iSta)%nTrp
      tPrv = tTrp
      tTrp = tNxt
      IF (iTrp<trpEst%sta(iSta)%nTrp) THEN
        tNxt = trpEst%sta(iSta)%trp(iTrp+1)%timInt(1)
      ELSE
        tNxt = 0.d0
      END IF

! flag parameters with 0.d0
      IF ( (tTrp>tPrv+minZpd+dtMax .OR. tPrv==0.d0  ) .AND.               &
           (tTrp<tNxt-minZpd-dtMax .OR. tNxt==0.d0)        ) THEN
        trpEst%sta(iSta)%trp(iTrp)%timInt(1) = 0.d0
      END IF

    END DO
  END DO



! Write Bernese file
! ------------------
  filNam = " "
  CALL wttrpe(filNam,trpEst,irc)


! Write a troposphere SINEX file
! ------------------------------
  filNam = " "
  CALL wttrpsnx(2,filNam,trpEst,ipara,neq%aNor,neq%xxx,locq,comstat%rms,  &
                neq%misc%nPar,neq%misc%nSmpNq,nSta,nCentr,xStat,iCentr,    &
                staNam,datum%name,irCode,grdTim)


! Free memory
! -----------
  DEALLOCATE(iCentr,stat=iac)
  DEALLOCATE(staNam,stat=iac)
  DEALLOCATE(staNum,stat=iac)
  DEALLOCATE(staFlg,stat=iac)
  DEALLOCATE(xStEll,stat=iac)
  DEALLOCATE(xStat,stat=iac)
  DEALLOCATE(xStEcc,stat=iac)
  DEALLOCATE(trpIdx,stat=iac)
  DEALLOCATE(ipara,stat=iac)
  DEALLOCATE(grdTim,stat=iac)
  IF (nGrd>0) DEALLOCATE(grdIdx,stat=iac)
  IF (nEli>0) DEALLOCATE(trpWin,stat=iac)
  DO iSta=1,trpEst%nSta
    DEALLOCATE(trpEst%sta(iSta)%trp,stat=iac)
  END DO
  DEALLOCATE(trpEst%sta,stat=iac)


! Subroutine ends here
! --------------------
   RETURN
END SUBROUTINE trpstore


END MODULE
