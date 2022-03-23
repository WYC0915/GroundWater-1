MODULE s_PARSTACK
  USE m_time,   ONLY: OPERATOR(+)
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE parstack(neq)

! -------------------------------------------------------------------------
! Purpose:    Transform parameters and stack them within one NEQ
!
! Author:     R. Dach
!
! Created:    06-May-2009
! Last mod.:  08-Feb-2012
!
! Changes:    06-May-2009 RD: Scaling of loading models added
!             10-May-2009 RD: Receiver clock offsets/biases implemented
!             24-Nov-2009 SS: Extension to all parameter types
!                             (particularly with regard to coordinates)
!             04-Jan-2010 SL: Stacking of HOI scaling parameters added
!             08-Feb-2012 RD: Solve "out-of-bound" problem in "IF"-statement
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_time,   ONLY: t_timint
  USE m_global, ONLY: g_syssvn
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: opt

  USE f_ikf
  USE s_defreq
  USE f_tstequiv
  USE s_svn2prn
  USE s_satblk
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)             :: neq       ! NEQ-system

! Local Variables
! ---------------
  CHARACTER(LEN=4)                      :: svn
  INTEGER(i4b),DIMENSION(neq%misc%npar) :: crsp
  INTEGER(i4b)                          :: ipar,jpar
  INTEGER(i4b)                          :: nSat
  INTEGER(i4b)                          :: ii,jj
  INTEGER(i4b)                          :: iGrd
  INTEGER(i4b)                          :: iSta
  INTEGER(i4b)                          :: iFrq
  INTEGER(i4b)                          :: blkNr
  INTEGER(i4b)                          :: iSat, jSat
  INTEGER(i4b)                          :: iGrp
  INTEGER(i4b)                          :: iSig
  INTEGER(i4b)                          :: prn

  TYPE(t_timint) :: timint
  TYPE(t_timint) :: itimint
  TYPE(t_timint) :: jtimint

  INCLUDE 'COMFREQ.inc'

! Modify the parameters according to the options
! ----------------------------------------------
  ii = 0
  DO ipar=1,neq%misc%npar


    ! Satellite to frequency specific receiver clock bias
    IF ( opt%rcoToFrq == 1 .AND. &
         neq%par(ipar)%locq(1) == 2 .AND. &
         neq%par(ipar)%locq(6) == 2 ) THEN

      timint%t(1) = neq%par(ipar)%time%mean - neq%par(ipar)%time%half
      timint%t(2) = neq%par(ipar)%time%mean + neq%par(ipar)%time%half
      CALL defreq(timint%t,1,(/ neq%par(ipar)%locq(4) /), nSat)
!     ADD_GNSS_HERE or obstype selection?

      IF (nSat == 0) THEN
        neq%par(ipar)%locq(6)=1
        neq%par(ipar)%locq(4)=freqnm(neq%par(ipar)%locq(4))
        ii=ii+1
      ENDIF
    ENDIF


    ! Scaling factors for Vienna grid files:
    ! Group stations
    IF ( neq%par(ipar)%locq(1) == 22 .AND. &
         neq%par(ipar)%locq(3) ==  0 ) THEN
      IF ( opt%grdLoad(neq%par(ipar)%locq(2))%nSta > 0 ) THEN
        iGrd = neq%par(ipar)%locq(2)
        DO iSta = 1,opt%grdLoad(iGrd)%nSta
          IF (neq%par(ipar)%name == opt%grdLoad(iGrd)%staLst(iSta)) THEN
            neq%par(ipar)%locq(3) = opt%grdLoad(iGrd)%staClu(iSta)
            neq%par(ipar)%name    = ''
            ii = ii + 1
          ENDIF
        ENDDO
      ENDIF
    ENDIF


    ! Scaling factors for Vienna grid files:
    ! One parameter for all stations
    IF ( neq%par(ipar)%locq(1) == 22 .AND. &
         neq%par(ipar)%locq(3) >=  0 ) THEN
      IF ( opt%grdLoad(neq%par(ipar)%locq(2))%nSta == -1 ) THEN
        neq%par(ipar)%locq(3) = -1
        neq%par(ipar)%name    = ''
        ii = ii + 1
      ENDIF
    ENDIF


    ! Scaling factors for Vienna grid files:
    ! Reduce the number of parameters
    IF ( neq%par(ipar)%locq(1) == 22 ) THEN
      IF ( neq%par(ipar)%locq(5)  > opt%grdLoad(neq%par(ipar)%locq(2))%nPar ) THEN
        iGrd = neq%par(ipar)%locq(2)
        IF ( opt%grdLoad(iGrd)%nPar == 1 ) THEN
          neq%par(ipar)%locq(4) = 1
          neq%par(ipar)%locq(5) = 1
          ii = ii + 1
        ENDIF
        IF ( opt%grdLoad(iGrd)%nPar == 2 ) THEN
          neq%par(ipar)%locq(5) = 2
          IF (neq%par(ipar)%locq(4) == 3) THEN
            neq%par(ipar)%locq(4) = 2
            ii = ii + 1
          ENDIF
        ENDIF
      ENDIF
    ENDIF


    ! Scaling factors for HOIs:
    IF(neq%par(ipar)%locq(1) == 27 .AND. &
       neq%par(ipar)%locq(3) ==  2 ) THEN
      IF ( opt%hoi(neq%par(ipar)%locq(2))%stack) THEN
        neq%par(ipar)%locq(3) = 1
        neq%par(ipar)%locq(4) = 0
        neq%par(ipar)%name    = ''
        ii = ii + 1
      ENDIF
    ENDIF


    ! Satellite specific antenna offsets:
    ! Collect to satellite type specific parameters
    IF ( neq%par(ipar)%locq(1) == 12 .AND. &
         neq%par(ipar)%locq(4) == 0 ) THEN

!     ADD_GNSS_HERE
      IF ( ( g_syssvn(INT(neq%par(ipar)%locq(5)/100)) == 'G' .AND. &
                                                opt%saoOpt(1) == 2 ) .OR. &
           ( g_syssvn(INT(neq%par(ipar)%locq(5)/100)) == 'R' .AND. &
                                                opt%saoOpt(2) == 2 ) ) THEN
        WRITE(svn,'(A1,I3)')g_syssvn(INT(neq%par(ipar)%locq(5)/100)),&
                            neq%par(ipar)%locq(5)
        CALL svn2prn(3,svn,neq%par(ipar)%time%mean,prn,timint,ii)
        CALL satblk(prn,neq%par(ipar)%time%mean,iFrq,blkNr)
        iGrp = 0
        DO ii = ipar-1,1,-1
          IF ( neq%par(ii)%locq(1) == 12 .AND. neq%par(ii)%locq(4) /= 0 ) THEN
            IF (neq%par(ii)%locq(5) == blkNr) THEN
              iGrp = neq%par(ii)%locq(4)
              EXIT
            ENDIF
          ENDIF
          IF ( neq%par(ii)%locq(1) == 25 .AND. neq%par(ii)%locq(2) /= 0 ) THEN
            IF (neq%par(ii)%locq(3) == blkNr) THEN
              iGrp = neq%par(ii)%locq(2)
              EXIT
            ENDIF
          ENDIF
        ENDDO

        IF (iGrp == 0) THEN
          neq%misc%nanoff = neq%misc%nanoff + 1
          iGrp = neq%misc%nanoff
          neq%misc%nsaoff(iGrp) = 1
          neq%misc%satoff(1,iGrp) = prn
        ELSE
          jSat = 0
          DO iSat = 1,neq%misc%nsaoff(iGrp)
            IF (neq%misc%satoff(iSat,iGrp) == prn) jSat = iSat
          ENDDO
          IF (jSat == 0) THEN
            neq%misc%nsaoff(iGrp) = neq%misc%nsaoff(iGrp)+1
            neq%misc%satoff(neq%misc%nsaoff(iGrp),iGrp) = prn
          ENDIF
        ENDIF
        DO iSig = 1,SIZE(opt%sigma)
          IF (opt%sigma(iSig)%locq(1) == 12 .AND. &
              opt%sigma(iSig)%locq(5) == neq%par(ipar)%locq(5)) &
            opt%sigma(iSig)%locq(5) = blknr
        ENDDO
        neq%par(ipar)%locq(2) = iGrp
        neq%par(ipar)%locq(4) = iGrp
        neq%par(ipar)%locq(5) = blknr
        ii = ii + 1
      ENDIF

    ENDIF


    ! Satellite specific antenna offsets:
    ! Collect to frequency specific parameters (GLONASS)
    IF ( neq%par(ipar)%locq(1) == 12 .AND. &
         neq%par(ipar)%locq(4) == 0 ) THEN
!     ADD_GNSS_HERE
      IF ( ( g_syssvn(INT(neq%par(ipar)%locq(5)/100)) == 'R' .AND. &
                                                opt%saoOpt(2) == 1 ) ) THEN
        WRITE(svn,'(A1,I3)')g_syssvn(INT(neq%par(ipar)%locq(5)/100)),&
                            neq%par(ipar)%locq(5)
        CALL svn2prn(3,svn,neq%par(ipar)%time%mean,prn,timint,ii)
        CALL satblk(prn,neq%par(ipar)%time%mean,iFrq,blkNr)
        iGrp = 0
        DO ii = ipar-1,1,-1
          IF ( neq%par(ii)%locq(1) == 12 .AND. neq%par(ii)%locq(4) /= 0 ) THEN
            IF (neq%par(ii)%locq(5) == iFrq+150) THEN
              iGrp = neq%par(ii)%locq(4)
              EXIT
            ENDIF
          ENDIF
          IF ( neq%par(ii)%locq(1) == 25 .AND. neq%par(ii)%locq(2) /= 0 ) THEN
            IF (neq%par(ii)%locq(3) == iFrq+150) THEN
              iGrp = neq%par(ii)%locq(2)
              EXIT
            ENDIF
          ENDIF
        ENDDO

        IF (iGrp == 0) THEN
          neq%misc%nanoff = neq%misc%nanoff + 1
          iGrp = neq%misc%nanoff
          neq%misc%nsaoff(iGrp) = 1
          neq%misc%satoff(1,iGrp) = prn
        ELSE
          jSat = 0
          DO iSat = 1,neq%misc%nsaoff(iGrp)
            IF (neq%misc%satoff(iSat,iGrp) == prn) jSat = iSat
          ENDDO
          IF (jSat == 0) THEN
            neq%misc%nsaoff(iGrp) = neq%misc%nsaoff(iGrp)+1
            neq%misc%satoff(neq%misc%nsaoff(iGrp),iGrp) = prn
          ENDIF
        ENDIF
        DO iSig = 1,SIZE(opt%sigma)
          IF (opt%sigma(iSig)%locq(1) == 12 .AND. &
              opt%sigma(iSig)%locq(5) == neq%par(ipar)%locq(5)) &
            opt%sigma(iSig)%locq(5) = iFrq+150
        ENDDO
        neq%par(ipar)%locq(2) = iGrp
        neq%par(ipar)%locq(4) = iGrp
        neq%par(ipar)%locq(5) = iFrq+150
        ii = ii + 1
      ENDIF
    ENDIF


    ! Satellite specific antenna pattern:
    ! Collect to satellite type specific parameters
    IF ( neq%par(ipar)%locq(1) == 25 .AND. &
         neq%par(ipar)%locq(2) == 0 ) THEN
!     ADD_GNSS_HERE
      IF ( ( g_syssvn(INT(neq%par(ipar)%locq(3)/100)) == 'G' .AND. &
                                                opt%sapOpt(1) == 2 ) .OR. &
           ( g_syssvn(INT(neq%par(ipar)%locq(3)/100)) == 'R' .AND. &
                                                opt%sapOpt(2) == 2 ) ) THEN
        WRITE(svn,'(A1,I3)')g_syssvn(INT(neq%par(ipar)%locq(3)/100)),&
                            neq%par(ipar)%locq(3)
        CALL svn2prn(3,svn,neq%par(ipar)%time%mean,prn,timint,ii)
        CALL satblk(prn,neq%par(ipar)%time%mean,iFrq,blkNr)
        iGrp = 0
        DO ii = ipar-1,1,-1
          IF ( neq%par(ii)%locq(1) == 25 .AND. neq%par(ii)%locq(2) /= 0 ) THEN
            IF (neq%par(ii)%locq(3) == blkNr) THEN
              iGrp = neq%par(ii)%locq(2)
              EXIT
            ENDIF
          ENDIF
          IF ( neq%par(ii)%locq(1) == 12 .AND. neq%par(ii)%locq(4) /= 0 ) THEN
            IF (neq%par(ii)%locq(5) == blkNr) THEN
              iGrp = neq%par(ii)%locq(4)
              EXIT
            ENDIF
          ENDIF
        ENDDO

        IF (iGrp == 0) THEN
          neq%misc%nanoff = neq%misc%nanoff + 1
          iGrp = neq%misc%nanoff
          neq%misc%nsaoff(iGrp) = 1
          neq%misc%satoff(1,iGrp) = prn
        ELSE
          jSat = 0
          DO iSat = 1,neq%misc%nsaoff(iGrp)
            IF (neq%misc%satoff(iSat,iGrp) == prn) jSat = iSat
          ENDDO
          IF (jSat == 0) THEN
            neq%misc%nsaoff(iGrp) = neq%misc%nsaoff(iGrp)+1
            neq%misc%satoff(neq%misc%nsaoff(iGrp),iGrp) = prn
          ENDIF
        ENDIF
        DO iSig = 1,SIZE(opt%sigma)
          IF (opt%sigma(iSig)%locq(1) == 25 .AND. &
              opt%sigma(iSig)%locq(3) == neq%par(ipar)%locq(3)) &
            opt%sigma(iSig)%locq(3) = blknr
        ENDDO
        neq%par(ipar)%locq(2) = iGrp
        neq%par(ipar)%locq(3) = blknr
        ii = ii + 1
      ENDIF
    ENDIF


    ! Satellite specific antenna pattern:
    ! Collect to frequency specific parameters (GLONASS)
    IF ( neq%par(ipar)%locq(1) == 25 .AND. &
         neq%par(ipar)%locq(2) == 0 ) THEN
!     ADD_GNSS_HERE
      IF ( ( g_syssvn(INT(neq%par(ipar)%locq(3)/100)) == 'R' .AND. &
                                                opt%sapOpt(2) == 1 ) ) THEN
        WRITE(svn,'(A1,I3)')g_syssvn(INT(neq%par(ipar)%locq(3)/100)), &
                            neq%par(ipar)%locq(3)
        CALL svn2prn(3,svn,neq%par(ipar)%time%mean,prn,timint,ii)
        CALL satblk(prn,neq%par(ipar)%time%mean,iFrq,blkNr)
        iGrp = 0
        DO ii = ipar-1,1,-1
          IF ( neq%par(ii)%locq(1) == 25 .AND. neq%par(ii)%locq(2) /= 0 ) THEN
            IF (neq%par(ii)%locq(3) == iFrq+150) THEN
              iGrp = neq%par(ii)%locq(2)
              EXIT
            ENDIF
          ENDIF
          IF ( neq%par(ii)%locq(1) == 12 .AND. neq%par(ii)%locq(4) /= 0 ) THEN
            IF (neq%par(ii)%locq(5) == iFrq+150) THEN
              iGrp = neq%par(ii)%locq(4)
              EXIT
            ENDIF
          ENDIF
        ENDDO

        IF (iGrp == 0) THEN
          neq%misc%nanoff = neq%misc%nanoff + 1
          iGrp = neq%misc%nanoff
          neq%misc%nsaoff(iGrp) = 1
          neq%misc%satoff(1,iGrp) = prn
        ELSE
          jSat = 0
          DO iSat = 1,neq%misc%nsaoff(iGrp)
            IF (neq%misc%satoff(iSat,iGrp) == prn) jSat = iSat
          ENDDO
          IF (jSat == 0) THEN
            neq%misc%nsaoff(iGrp) = neq%misc%nsaoff(iGrp)+1
            neq%misc%satoff(neq%misc%nsaoff(iGrp),iGrp) = prn
          ENDIF
        ENDIF
        DO iSig = 1,SIZE(opt%sigma)
          IF (opt%sigma(iSig)%locq(1) == 25 .AND. &
              opt%sigma(iSig)%locq(3) == neq%par(ipar)%locq(3)) &
            opt%sigma(iSig)%locq(3) = iFrq+150
        ENDDO
        neq%par(ipar)%locq(2) = iGrp
        neq%par(ipar)%locq(3) = iFrq + 150
        ii = ii + 1
      ENDIF
    ENDIF


  ENDDO


! Find the corresponding parameters
! ---------------------------------
  ii = 0
  crsp = 0

  DO ipar = 1, neq%misc%npar-1
    IF ( neq%par(ipar)%locq(1) ==  0 ) CYCLE

    DO jpar = ipar+1, neq%misc%npar
      IF ( neq%par(ipar)%locq(1) /= neq%par(jpar)%locq(1) ) CYCLE
! Velocities?
      IF ( neq%par(ipar)%locq(1) ==  1 .AND. &
           neq%par(ipar)%locq(4) /= neq%par(jpar)%locq(4) ) CYCLE

      IF ( tstequiv(neq,ipar,neq,jpar) ) THEN
        crsp(jpar)            = ipar
        neq%par(jpar)%locq(1) = 0
        ii = ii+1
        EXIT
      ENDIF

    ENDDO ! jpar
  ENDDO ! ipar

! Nothing to do
  IF (ii == 0) RETURN


! Update neq%aNor matrix, neq%bNor vector, and neq%par structure
! --------------------------------------------------------------
  neq%misc%nparms =  neq%misc%nparms - ii

  DO jpar = 1, neq%misc%npar
    ipar = crsp(jpar)

    IF (ipar == 0) CYCLE

    neq%bNor(ipar) = neq%bNor(ipar) + neq%bNor(jpar)

! Update Time Interval
! --------------------
    itimint%t(1) = neq%par(ipar)%time%mean - neq%par(ipar)%time%half
    itimint%t(2) = neq%par(ipar)%time%mean + neq%par(ipar)%time%half
    jtimint%t(1) = neq%par(jpar)%time%mean - neq%par(jpar)%time%half
    jtimint%t(2) = neq%par(jpar)%time%mean + neq%par(jpar)%time%half
    timint = itimint + jtimint
    neq%par(ipar)%time%mean = (timint%t(2) + timint%t(1)) / 2.0
    neq%par(ipar)%time%half = (timint%t(2) - timint%t(1)) / 2.0

    DO jj = 1, neq%misc%npar
      ii = crsp(jj)
      IF (ii /= 0 .AND. jj > jpar) CYCLE
      IF (ii == 0 ) ii=jj

      neq%aNor(ikf(ipar,ii)) = neq%aNor(ikf(ipar,ii)) + neq%aNor(ikf(jpar,jj))
      IF (ipar == ii .AND. jpar /= jj) &
        neq%aNor(ikf(ipar,ii)) = neq%aNor(ikf(ipar,ii)) + neq%aNor(ikf(jpar,jj))
    END DO

  END DO

END SUBROUTINE parstack


END MODULE
