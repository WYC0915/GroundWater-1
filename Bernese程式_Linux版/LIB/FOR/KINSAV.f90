MODULE s_KINSAV
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE kinsav(MXCLCQ,TITLE,NPAR,XSTAT,XSTECC,LOCQ,RMS,XXX,ANOR,XXX0,&
                  PARFLG,OPTELI, STNAME,DATUM,IWQXX,CLKHED,CLKREC,NEPOBS,&
                  IRAUX2)

! -------------------------------------------------------------------------
! Purpose:    Compute adjusted kinematic coordinates and corresponding
!             coffactor matrix and save them in the file
!
! Author:     D. Svehla
!
! Created:    25-Jun-2002
! Last mod.:  21-Sep-2011
!
! Changes:    07-Oct-2002 : DS: Use 1D20 when singular coord.
!             08-Oct-2002 : DS: Use standard. orbit to get LEO a priori orbit
!                               when KININP in missing
!             31-Oct-2002 : DS: Correct format for QXXE
!             19-Jun-2003 : RD: Use IRAUX2 for result scratch file from RESEPO
!             20-Jun-2003 : RD: Use RDSCRA to read results from RESEPO
!             23-Jun-2003 : HB: Interface for SR staFlg
!             24-Jun-2003 : RD: Solutions with "few obs" have flag "S"
!                               Get apriori if no KININP available for the sta.
!             25-Jun-2003 : RD: Sort results station by station
!                               Interpolate values for epochs w/o solutions ("X")
!             10-Apr-2004 : DS: Write SECSAV instead of SECOND in all cases (LEO)
!             08-Aug-2005 : HB: Use new SR TIMST2 (module)
!             11-Jun-2006 : HB: Get a priori values for KIN from RDSCRA
!             29-Jun-2006 : HB: Get a priori values from xxx0 for not
!                               pre-el parameters
!             21-Sep-2011 : RD: Correct apriori for kin from main-NEQ
!
! SR used:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_const, ONLY: DATE,TIME
  USE D_CLKRNX, ONLY: T_CLKHEAD,T_CLKREC
  USE d_stacrx, ONLY: MTypeSPACE
  USE f_ikf
  USE s_opnfil
  USE s_mjdgps
  USE s_rdscra
  USE s_opnerr
  USE s_timst2
  USE s_staflg
  USE s_readkin
  USE s_gtleoco
  USE s_gtflna
  IMPLICIT NONE
!
! List of Parameters
! ------------------
  INTEGER(i4b)                     :: MXCLCQ ! size of locq
  CHARACTER(LEN=80)                :: TITLE  ! Title for file (one line)
  INTEGER(i4b)                     :: NPAR   ! Number of parameters
  REAL(r8b),DIMENSION(3,*)         :: XSTAT  ! A Priori station coordinates
  REAL(r8b),DIMENSION(3,*)         :: XSTECC ! Eccentricities for stations
  INTEGER(i4b),DIMENSION(MXCLCQ,*) :: LOCQ   ! Parameter characterization
  REAL(r8b)                        :: RMS    ! A posteriori rms
  REAL(r8b),DIMENSION(*)           :: XXX    ! Solution vector
  REAL(r8b),DIMENSION(*)           :: ANOR   ! Inverse of normal equation matrix,
                                             ! upper triangle, columnwise linear.
  REAL(r8b),DIMENSION(*)           :: XXX0   ! A priori values
  INTEGER(i4b), DIMENSION(*)       :: PARFLG ! Flag for singular parameter
                                             ! =0 : parameter not singular
                                             ! =1 : parameter singular
  INTEGER(i4b), DIMENSION(*)       :: OPTELI ! Option for parameter pre-elimination
                                             ! =0 : no pre-elimination
                                             ! =1 : pre-elimination before inversion
                                             ! =2 : pree-elimination after inversion
                                             ! =3 : pre-elimination epoch-wise
  CHARACTER(LEN=*), DIMENSION(*)   :: STNAME ! List of station names
  CHARACTER(LEN=16)                :: DATUM  ! Geodetic datum
  INTEGER(i4b)                     :: IWQXX  ! Flag for writing Var/Cov information
                                             ! =0 : do not write QXX to the file
                                             ! =1 : write QXX to the file
  TYPE(T_CLKHEAD)                  :: CLKHED ! Clock header information
  TYPE(T_CLKREC)                   :: CLKREC ! ???
  INTEGER(i4b)                     :: nepobs ! Min # of obs. for epoch param.s
                                             ! (kin coord only)
  INTEGER(i4b)                     :: IRAUX2 ! Epoch result scratch file is
                                             ! 0: available / 1: not avail.


!
! Local Variables
! ---------------
  CHARACTER(LEN=1)                 :: KINFLG
  CHARACTER(LEN= fileNameLength)   :: FILSCR,FILKOU,FILAUX,filkin
  CHARACTER(LEN=20)                :: MarTyp
  CHARACTER(LEN=19)                :: TSTRNG
  INTEGER(i4b), DIMENSION(MXCLCQ,3):: LCQ2
  INTEGER(i4b)                     :: iSta,iFlag,IRCKOU,IRCSCR,IRC,IP
  INTEGER(i4b)                     :: jNum,iNum,nNum,mNum
  INTEGER(i4b)                     :: IOSTAT,NWEEK,I,II,IKINSIN,JTYP
  INTEGER(i4b)                     :: iEpo
  INTEGER(i4b)                     :: LFNKOU,LFNAUX,IRCKIN
  INTEGER(i4b)                     :: iEpo0, jEpo, nWeek1, iSta0

  REAL(r8b),    DIMENSION(6)       :: QXXE,QXX0
  REAL(r8b),    DIMENSION(3)       :: XKIN, XEST,XAPR,XAPR0
  REAL(r8b)                        :: SECOND,SECSAV
  REAL(r8b)                        :: TOBS,dsec
  REAL(r8b)                        :: tObs0,tObs1,second1
  REAL(r8b),    DIMENSION(3)       :: xKin0,xKin1


! Get file for kinematic outputs
! ------------------------------
  CALL GTFLNA(0,'KINOUT ',FILKOU,IRCKOU)
  IF (IRCKOU.NE.0) RETURN
!
  CALL GTFLNA(0,'KININP ',filkin,IRCKIN)
!
  LFNKOU=LFN002+3
  LFNAUX=LFN002+2
!
  CALL OPNFIL(LFNKOU,FILKOU,'NEW','FORMATTED',                       &
              ' ',' ',IOSTAT)
  CALL OPNERR(LFNERR,LFNKOU,IOSTAT,FILKOU,'KINSAV')
!
! Open scratch file for epoch-wise solution
! -----------------------------------------
  IF (IRAUX2 == 0) THEN
    CALL GTFLNA(1,'AUXFIL1',FILAUX,IRC)
    CALL OPNFIL(LFNAUX,FILAUX,'OLD','UNFORMATTED',                   &
                 'READONLY',' ',IOSTAT)
    CALL OPNERR(LFNERR,LFNAUX,IOSTAT,FILAUX,'KINSAV')
  END IF

! Get file name of kinematic scratch file
! ---------------------------------------
  CALL GTFLNA(0,'KINSCR ',FILSCR,IRCSCR)

! Deallocate kinematic array (allready done in PRIKIN)
! ----------------------------------------------------
!  IF (IRCSCR==0) THEN
!    CALL  READKIN(FILSCR,STNAME(1),0.D0,1,1,XAPR,IRC)
!  END IF

!
! Write header of kinematic output file
! -------------------------------------
  CALL TIMST2(1,1,CLKHED%TFIRST,TSTRNG)
!
  IF (IWQXX == 0) THEN
    WRITE(LFNKOU,11) TITLE,DATE,TIME,DATUM,TSTRNG
11        FORMAT(A64,1X,A9,1X,A5,/,80('-'),/,                        &
             'LOCAL GEODETIC DATUM: ',A16,2X,'EPOCH: ',A19,//,       &
             ' STATION NAME     WEEK  SECONDS',8X,                   &
             'X (M)',10X,'Y (M)',10X,'Z (M)',4X,'F',/)
  ELSE IF (IWQXX == 1) THEN
    WRITE(LFNKOU,21) TITLE,DATE,TIME,DATUM,TSTRNG,RMS
21        FORMAT(A64,1X,A9,1X,A5,/,80('-'),/,                        &
             'LOCAL GEODETIC DATUM: ',A16,2X,'EPOCH: ',A19,          &
             /92X,'RMS=',F10.6/,' STATION NAME     WEEK  SECONDS',8X,&
             'X (M)',10X,'Y (M)',10X,'Z (M)',4X,'F',/)
  END IF
!
! Loop over all parameters
! ------------------------

!
! Epoch wise pree-eliminated kinematic coordinates
! ------------------------------------------------
  IF (IRAUX2 == 0) THEN

    iSta0 = 0
! KIN. COORDINATES HAVE TO BE SORTED BY STATIONS
!   jNum: station number found in the scratch file
!   iNum: actual station number to print - print if iNum.eq.jNum
!   nNum: next station number for iNum
!   mNum: biggest station number found in the scratch file - max(jNum)
    iNum = 0
    mNum = 0
    nNum = 1
    kinStaLoop: DO
      IF (mNum >  0.AND.iNum == mNum) EXIT kinStaLoop ! Last station finished
      IF (mNum == 0.AND.nNum == iNum) EXIT kinStaLoop ! No kin stations
                                                      ! but at least one loop

      ! What is the next station
      REWIND(LFNAUX)
      iNum=nNum

      ! Read next record from result scratch file
      DO
        CALL RDSCRA(MXCLCQ,LFNAUX,JTYP,jNum,TOBS,LCQ2,XAPR0,XEST,QXX0,IRC)
        IF (IRC /= 0) CYCLE kinStaLoop
        IF (JTYP /= 21) CYCLE

        ! Get number of last station
        IF (mNum < jNum) mNum=jNum

        ! Get number of the next station to handle
        IF (jNum > iNum.AND.(jNum < nNum.OR.iNum == nNum)) nNum=jNum

        ! The station has to be handled in another run
        IF (jNum /= iNum) CYCLE

        ! Start write results to file
        ! ---------------------------
        iSta=LCQ2(2,1)
        iEpo=LCQ2(4,1)

        ! Non-singular result
        IF (XEST(1) /= 1D20) THEN

          ! Get apriori value
          CALL staflg(stName(iSta),TOBS,iFlag,MarTyp)
          IRC = 2
          IF (IRCKIN==0) &
            CALL  READKIN(FILSCR,STNAME(iSta),TOBS,1,0,XAPR,IRC)
          IF (irc /= 0) THEN
            XAPR(1:3)=XAPR0(1:3)
          END IF

          ! Result
          XKIN(1:3)=XAPR(1:3)+XEST(1:3)

          ! Check number of observatin contributed
          IF (lcq2(6,1) >= nEpObs) THEN
            KINFLG='K'
          ELSE
            KINFLG='S'
          ENDIF

          ! Reorder COV-information
          IF (IWQXX == 1) THEN
            QXXE(1) = QXX0(IKF(1,1))
            QXXE(2) = QXX0(IKF(2,2))
            QXXE(3) = QXX0(IKF(3,3))
            QXXE(4) = QXX0(IKF(1,2))
            QXXE(5) = QXX0(IKF(1,3))
            QXXE(6) = QXX0(IKF(2,3))
          END IF

        ! Result was singular
        ELSE
          XKIN(1:3)=0.0D0
          KINFLG='X'
        END IF

! Why is this computed again?
! TOBS comes from the RESEPO scratch file (see RDSCRA)
!        iEpo=LCQ2(4,1)
!        dsec=DBLE(iEpo-1)*CLKREC%EPOCH(1)/CLKREC%NEPO
!        TOBS=CLKHED%TFIRST+dsec/86400.D0

        CALL MJDGPS(TOBS,SECOND,NWEEK)
        SECSAV=DNINT(SECOND)

        IF (IWQXX == 1) THEN
          WRITE(LFNKOU,31) STNAME(iSta),NWEEK,SECSAV,                  &
                               (XKIN(I),I=1,3),KINFLG,QXXE(1:6)
31        FORMAT(1X,A16,1X,I4,1X,F8.0,1X,                              &
                  3F15.4,1X,A1,3X,6F13.5)
        ELSE IF (KINFLG /= 'X') THEN

          ! Interpolate for missing epochs
          IF (iEpo > iEpo0+1 .AND. iSta == iSta0) THEN
            DO jEpo = iEpo0+1,iEpo-1
              dsec=DBLE(jEpo-1)*CLKREC%EPOCH(1)/CLKREC%NEPO
              TOBS1=CLKHED%TFIRST+dsec/86400.D0
              DO ii=1,3
                xKin1(ii) = xKin0(ii)+ &
                           (xKin(ii)-xKin0(ii))/(tObs-tObs0)*(tObs1-tObs0)
              ENDDO
              CALL MJDGPS(TOBS1,SECOND1,NWEEK1)
              WRITE(LFNKOU,41) STNAME(iSta),NWEEK1,DNINT(SECOND1),     &
                          (XKIN1(I),I=1,3),'X'
            ENDDO
          ENDIF

          ! Write next estimated epoch
          WRITE(LFNKOU,41) STNAME(iSta),NWEEK,SECSAV,                  &
                          (XKIN(I),I=1,3),KINFLG
41        FORMAT(1X,A16,1X,I4,1X,F8.0,1X,3F15.4,1X,A1)

          ! Save results, may be needed for interpolation
          iSta0 = iSta
          iEpo0 = iEpo
          tObs0 = tObs
          xKin0 = xKin
        END IF

      ENDDO ! Next record from scratch file
    ENDDO kinStaLoop

!
! Kinematic coordinates without epoch wise pre-elimination
! --------------------------------------------------------
  ELSE
    iSta0 = 0
    DO 100 IP=1,NPAR
      IF (LOCQ(1,IP)/=21) GOTO 100
      IF(LOCQ(3,IP)/=1) GOTO 100
      iSta=LOCQ(2,IP)
      IKINSIN=0
      DO I=1,3
        IF (PARFLG(IP-1+I)==1) THEN
          IKINSIN=1
          XKIN(1:3)=0.0D0
          KINFLG='X'
          IF (IWQXX == 1) QXXE(1:6)=0.0D0
        END IF
      END DO

!
      iEpo=LOCQ(4,IP)
      dsec=DBLE(iEpo-1)*CLKREC%EPOCH(1)/CLKREC%NEPO
      TOBS=CLKHED%TFIRST+dsec/86400.D0
!
      IF (IKINSIN==0) THEN
        IF (locq(6,ip) >= nEpObs) THEN
          KINFLG='K'
        ELSE
          KINFLG='S'
        ENDIF
        DO I=1,3
          XEST(I)=XXX(IP-1+I)
        END DO

        CALL staflg(stName(iSta),TOBS,iFlag,MarTyp)
        IRC = 2
        IF (IRCKIN==0) &
          CALL  READKIN(FILSCR,STNAME(iSta),TOBS,1,0,XAPR,IRC)
        IF (IRC /= 0) THEN
          DO I=1,3
            XAPR(I)=XXX0(IP-1+I)
            IF (XAPR(I).EQ.1D20) XAPR(I)=XSTAT(I,iSta)
          ENDDO
        END IF
        XKIN(1:3)=XAPR(1:3)+XEST(1:3)

        IF (IWQXX == 1) THEN
          DO I=1,3
            QXXE(I)=ANOR(IKF(I+IP-1,I+IP-1))
            IF (I==1) THEN
              QXXE(4)=ANOR(IKF(I+IP-1,I+IP))
              QXXE(5)=ANOR(IKF(I+IP-1,I+IP+1))
              QXXE(6)=ANOR(IKF(I+IP  ,I+IP+1))
            END IF
          END DO
        END IF
      END IF
!
      CALL MJDGPS(TOBS,SECOND,NWEEK)
      SECSAV=DNINT(SECOND)

      IF (IWQXX == 1) THEN
        WRITE(LFNKOU,31) STNAME(iSta),NWEEK,SECSAV,                  &
                             (XKIN(I),I=1,3),KINFLG,QXXE(1:6)
      ELSE IF (KINFLG /= 'X') THEN
        IF (iEpo > iEpo0+1 .AND. iSta == iSta0) THEN
          DO jEpo = iEpo0+1,iEpo-1
            dsec=DBLE(jEpo-1)*CLKREC%EPOCH(1)/CLKREC%NEPO
            TOBS1=CLKHED%TFIRST+dsec/86400.D0
            DO ii=1,3
              xKin1(ii) = xKin0(ii)+ &
                         (xKin(ii)-xKin0(ii))/(tObs-tObs0)*(tObs1-tObs0)
            ENDDO
            CALL MJDGPS(TOBS1,SECOND1,NWEEK1)
            WRITE(LFNKOU,41) STNAME(iSta),NWEEK1,DNINT(SECOND1),     &
                        (XKIN1(I),I=1,3),'X'
          ENDDO
        ENDIF
        WRITE(LFNKOU,41) STNAME(iSta),NWEEK,SECSAV,                  &
                        (XKIN(I),I=1,3),KINFLG
        iSta0 = iSta
        iEpo0 = iEpo
        tObs0 = tObs
        xKin0 = xKin
      END IF
100 CONTINUE
  END IF
!
! CLOSE ALL FILES
! ---------------
9000 CLOSE(LFNKOU)
     IF (IRAUX2 == 0) CLOSE(UNIT=LFNAUX)

RETURN
!
END SUBROUTINE kinsav


END MODULE
