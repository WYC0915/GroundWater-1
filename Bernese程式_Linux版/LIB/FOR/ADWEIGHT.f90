MODULE s_ADWEIGHT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE adweight(neq, ipart, ifil, neq0)

! -------------------------------------------------------------------------
! Purpose:    Constrain the estimated parameters according to input
!             options (add weights)
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
!
! Changes:    18-May-2000 SS: Handle DCB parameters
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             07-Feb-2002 MM: Handle relative sigmas
!             13-Feb-2002 SS: Computation of "isys" generalized
!             13-Feb-2002 MM: Rel. sigmas for GIM parameters
!             26-May-2003 MM: delta Ts for relative constraints
!                             Troposphere now continuous
!             07-Jul-2003 MM: new coordinate constraining (SR CRDELIM)
!             11-Dec-2003 MM: Constraints for vel reviewed
!             23-Dec-2003 RS: Add weights for satellite ant. patterns
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             12-Dec-2005 CU: Count number of pseudo-observations npseu
!             26-Jan-2008 RD: Add RAO/RAP parameter
!             06-May-2009 RD: Enforce zero-mean cond for PCV (old:0.001D0)
!             10-May-2009 RD: Receiver clock offsets/biases implemented
!             10-Jun-2009 RD: Add epoch satellite/receiver clocks
!             24-Jun-2009 RD: Put direct constraints on DCBs
!             08-Dec-2010 MM: GNSS-specific parameters
!             03-Jan-2011 RD/SL: Recover (absolute) constraints for SAP
!             03-Feb-2011 MM: New weighting for GSPs
!             22-Jul-2011 LP: isys and jj removed
!             18-Aug-2011 LP: Sum condition for station DCBs modified
!             29-Nov-2011 SL: use m_bern with ONLY
!             20-Jan-2012 RD: Keep solution for GRID-scaling fixed for
!                             repeatability computation
!             12-Oct-2012 RD: Correct counting of nPseuEl for rel. weights
!             16-Oct-2012 SS: opt%blkRet with user-defined sigma value
!             07-May-2013 SL/RD: Call freenet if CRD pre-eliminted
!             19-Jun-2013 RD: Indicate GLONASS-IFB from PPP
!             16-Jul-2013 RD: Exceptions for clock parameters with few observations
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b
  USE d_const,  ONLY: C
  USE m_time,   ONLY: t_timint
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: opt,clkhed
  USE f_ikf
  USE s_freenet
  USE s_crdelim
  USE s_neuwgt
  USE s_trawgt
  USE f_isstacrx
  USE f_gtweight
  USE s_blockret
  USE f_istobeel
  USE s_defreq
  USE f_tstequiv
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)  :: neq
  INTEGER(i4b) :: ipart
  INTEGER(i4b) :: ifil
  TYPE(t_neq), OPTIONAL  :: neq0

! Local Variables
! ---------------
  TYPE(t_timint)             :: timint

  INTEGER(i4b)               :: ipar,jpar
  INTEGER(i4b)               :: ii
  INTEGER(i4b)               :: isig
  INTEGER(i4b)               :: iClk,jClk
  INTEGER(i4b)               :: numRef
  INTEGER(i4b)               :: nxtIdx
  INTEGER(i4b)               :: nSat
  INTEGER(i4b)               :: freq1,freq2
  INTEGER(i4b)               :: freq,gsys

  LOGICAL, SAVE              :: first   = .TRUE.
  LOGICAL, SAVE              :: freeCrd = .FALSE.
  LOGICAL, SAVE              :: freeVel = .FALSE.
  LOGICAL, SAVE              :: freeGsp = .FALSE.
  LOGICAL, SAVE              :: estVel  = .FALSE.
  LOGICAL, SAVE              :: clkRef  = .FALSE.
  LOGICAL                    :: nStaElm

  REAL(r8b)                  :: weight, tstWgt

  REAL(r8b)                  :: maxDT, maxTrp, maxGrd, maxGim
  REAL(r8b)                  :: rBound, lBound

  INCLUDE 'COMFREQ.inc'

! Maximal delta T for two subsequent parameters
! ---------------------------------------------
  maxDT  = 1.D0/1440.D0
  maxTrp = opt%dtRelSig(1)+maxDT
  maxGrd = opt%dtRelSig(2)+maxDT
  maxGim = opt%dtRelSig(3)+maxDT

! First call
! ----------
  IF ( first ) THEN
    first = .FALSE.

! velocities estimated?
    DO iPar=1,neq%misc%nPar
      IF (neq%par(iPar)%locq(1)==1 .AND. neq%par(iPar)%locq(4)==2) THEN
        estVel = .TRUE.
        EXIT
      END IF
    END DO

! minimum constraint solution?
    DO isig = 1, SIZE(opt%sigma)
      IF ( opt%sigma(isig)%locq(1) == -1 ) THEN
        IF ( opt%sigma(isig)%locq(4) ==  1 ) freeCrd = .TRUE.
        IF ( opt%sigma(isig)%locq(4) ==  2 ) freeVel = .TRUE.
      END IF
      IF (opt%sigma(isig)%locq(1) == -30 ) freeGsp = .TRUE.
    END DO
  END IF

  nStaElm = .TRUE.
  DO iPar=1,neq%misc%nPar
    IF(neq%par(iPar)%locq(1)/=1) CYCLE
    nStaElm = nStaElm .AND. isToBeEl(neq%par(ipar),ipart,ifil)
    IF(.NOT. nStaElm) EXIT
  END DO

! Blocking of Retrograde Terms in X and Y Polar Wobble Series
! -----------------------------------------------------------
  IF ( ipart == 0 .AND. opt%blkret > 0.D0 ) CALL blockret(neq)

! Handle special options - free network, latitude, longitude or height
! --------------------------------------------------------------------
  IF ( (ipart == 0 .AND. ifil == 0) .OR. nStaElm) THEN
    IF ( freeCrd ) CALL freenet(neq,1)
    IF ( freeVel ) CALL freenet(neq,2)
    IF ( freeGsp ) CALL freenet(neq,3)
    CALL neuwgt(neq)
  END IF

! Handle normal case
! ------------------
Loop_ipar:  DO ipar = 1, neq%misc%npar

    IF ( ipart == 0 .OR. isToBeEl(neq%par(ipar),ipart,ifil) ) THEN

      IF (neq%par(ipar)%locq(1) == 1 .AND. freeCrd .AND. freeVel) CYCLE
      IF (neq%par(ipar)%locq(1) == 1 .AND. freeCrd .AND. .NOT. estVel) CYCLE

! Special coordinate/velocity handling
! ------------------------------------
      IF (neq%par(iPar)%locq(1)==1 .AND.                                   &
          neq%par(iPar)%locq(3)==1 .AND.                                   &
          neq%par(iPar)%locq(4)/=2      ) THEN
        CALL crdelim(neq,iPar,ipart,ifil)
      END IF
      IF (neq%par(iPar)%locq(1)==1) CYCLE

! Special GNSS-specific station translation handling
! --------------------------------------------------
      IF (neq%par(iPar)%locq(1)==30 .AND. neq%par(iPar)%locq(3) < 4) THEN
        IF (opt%gTraDat == 1 .AND. neq%par(iPar)%locq(3) == 1) &
             CALL trawgt(neq,iPar,ipart,ifil)
        CYCLE
      ENDIF

!! Station Velocities
!! ------------------
!      IF ( neq%par(ipar)%locq(1) == 1 .AND. neq%par(ipar)%locq(4) == 2 ) THEN
!        DO ii = 1, neq%misc%npar
!          IF ( neq%par(ii)%locq(1) == 1 .AND. neq%par(ii)%locq(4) == 1 .AND. &
!               neq%par(ii)%locq(3) == neq%par(ipar)%locq(3)            .AND. &
!               neq%par(ii)%name    == neq%par(ipar)%name )              THEN
!            weight = gtweight(neq%par(ipar),'A')
!            neq%aNor( ikf(ipar,ipar) ) = neq%aNor( ikf(ipar,ipar) ) + weight
!            neq%aNor( ikf(ipar,ii  ) ) = neq%aNor( ikf(ipar,ii  ) ) - weight
!            neq%aNor( ikf(ii,  ii  ) ) = neq%aNor( ikf(ii  ,ii  ) ) + weight
!            CYCLE Loop_ipar
!          END IF
!        END DO
!      END IF

! Frequency/Satellite specific receiver clock biases
! --------------------------------------------------
      IF ( neq%par(ipar)%locq(1) == 2 .AND. neq%par(ipar)%locq(6) /= 0 .AND. &
           .NOT. isStacrx(neq%par(ipar)) ) THEN

        weight = (opt%sigma0*1000d0)**2

        ! time-dependent inter-system biases
        IF (neq%par(ipar)%locq(6) == 5) THEN
          DO ii = ipar, neq%misc%npar
            IF (neq%par(ii)%locq(1) == 2 .AND. &
                neq%par(ii)%locq(6) == neq%par(ipar)%locq(6) .AND. &
                neq%par(ii)%name    == neq%par(ipar)%name) THEN
              IF (ii > iPar .AND. neq%par(ii)%locq(5) < 0) EXIT
              IF (neq%aNor( ikf(ii,ii) ) /= 0D0 .AND. &
                .NOT. isStacrx(neq%par(ii))) THEN
                neq%aNor( ikf(ipar,ii) ) = neq%aNor( ikf(ipar,ii) ) + weight
              ENDIF
            ENDIF
          ENDDO

          IF (ABS(neq%par(ipar)%locq(5)) == neq%par(ipar)%locq(7)) THEN
            DO ii = ipar, neq%misc%npar
              IF (neq%par(ii)%locq(1) == 2 .AND. &
                  neq%par(ii)%locq(6) == neq%par(ipar)%locq(6) .AND. &
                  neq%par(ii)%locq(5) == neq%par(ii)%locq(7) .AND. &
                  DABS(neq%par(ii)%time%mean - neq%par(ipar)%time%mean) < 0.1d0/86400d0 .AND. &
                 neq%aNor( ikf(ii,ii) ) /= 0D0 .AND. &
                 .NOT. isStacrx(neq%par(ii))) THEN
                neq%aNor( ikf(ipar,ii) ) = neq%aNor( ikf(ipar,ii) ) + weight
              ENDIF
            ENDDO
          ENDIF
          CYCLE Loop_ipar
        ENDIF

        ! Condition of sum: all requests to the same satellite/with the same frequency
        DO ii = ipar, neq%misc%npar
          IF (opt%rcoFromPPP == 1) EXIT
          IF (neq%par(ii)%locq(1) == 2 .AND. &
!              neq%par(ipar)%name == neq%par(ii)%name .AND. &
              neq%par(ii)%locq(6) == neq%par(ipar)%locq(6).AND.&
              neq%par(ii)%locq(4) == neq%par(ipar)%locq(4).AND.&
              neq%aNor( ikf(ii,ii) ) /= 0D0 .AND. &
              .NOT. isStacrx(neq%par(ii))) THEN

            IF ( neq%par(ipar)%locq(6) == 2 ) THEN

              freq1 =  999999
              timint%t(1) = neq%par(ipar)%time%mean - neq%par(ipar)%time%half
              timint%t(2) = neq%par(ipar)%time%mean + neq%par(ipar)%time%half
              CALL defreq(timint%t,1,(/ neq%par(ipar)%locq(4) /), nSat)

              IF (nSat == 0) freq1=freqnm(neq%par(ipar)%locq(4))

              freq2 = -999999
              timint%t(1) = neq%par(ii)%time%mean - neq%par(ii)%time%half
              timint%t(2) = neq%par(ii)%time%mean + neq%par(ii)%time%half
              CALL defreq(timint%t,1,(/ neq%par(ii)%locq(4) /), nSat)

              IF (nSat == 0) freq2=freqnm(neq%par(ii)%locq(4))

              IF (freq1 /= freq2) CYCLE

            ENDIF
            neq%aNor( ikf(ipar,ii) ) = neq%aNor( ikf(ipar,ii) ) + weight
          ENDIF
        ENDDO

! Condition of sum: between all GPS satellites of a request (for gps-only
!       or GPS+GLONASS) or between all GLONASS satellites of a request (only
!       for a GLONASS only request)

        ! Frequency specific, GLONASS only
        IF ( neq%par(ipar)%locq(7) == 2 .AND. &
            (neq%par(ipar)%locq(6) == 1 .OR. neq%par(ipar)%locq(6) == 4) )THEN
          DO ii = ipar, neq%misc%npar
            IF (neq%par(ii)%locq(1) == 2 .AND. &
                neq%par(ii)%name == neq%par(ipar)%name .AND.&
                neq%par(ii)%locq(6) == neq%par(ipar)%locq(6).AND.&
                neq%aNor( ikf(ii,ii) ) /= 0D0 .AND. &
                .NOT. isStacrx(neq%par(ii))) THEN
              neq%aNor( ikf(ipar,ii) ) = neq%aNor( ikf(ipar,ii) ) + weight
            ENDIF
          ENDDO

        ! Satellite specific, GLONASS-only
        ELSE IF (neq%par(ipar)%locq(7) == 2 .AND. neq%par(ipar)%locq(6) == 2) THEN
          DO ii = ipar, neq%misc%npar
            IF (neq%par(ii)%locq(1) == 2 .AND. &
                neq%par(ii)%name == neq%par(ipar)%name .AND.&
                neq%par(ii)%locq(6) == neq%par(ipar)%locq(6).AND.&
                neq%aNor( ikf(ii,ii) ) /= 0D0 .AND. &
                .NOT. isStacrx(neq%par(ii))) THEN
              neq%aNor( ikf(ipar,ii) ) = neq%aNor( ikf(ipar,ii) ) + weight
            ENDIF
          ENDDO

        ! Satellite specific, GPS for GPS-only and GPS+GLONASS
        ELSE IF (neq%par(ipar)%locq(6) ==  2 .AND. &
                 neq%par(ipar)%locq(4) < 100) THEN
          DO ii = ipar, neq%misc%npar
            IF (neq%par(ii)%locq(1) == 2 .AND. &
                neq%par(ii)%name == neq%par(ipar)%name .AND.&
                neq%par(ii)%locq(6) == neq%par(ipar)%locq(6).AND.&
                neq%par(ii)%locq(4) <  100 .AND.&
                neq%aNor( ikf(ii,ii) ) /= 0D0 .AND. &
                .NOT. isStacrx(neq%par(ii))) THEN
              neq%aNor( ikf(ipar,ii) ) = neq%aNor( ikf(ipar,ii) ) + weight
            ENDIF
          ENDDO
        ENDIF

        CYCLE Loop_ipar
      ENDIF

! Clock parameters, condition of sum
! ----------------------------------
      IF ( neq%par(ipar)%locq(1) == 23 .OR. neq%par(ipar)%locq(1) == 24 ) THEN

        ! If a clock is deleted, no constraints for any clock of
        ! this epoch are allowed
        IF (neq%par(ipar)%locq(4) == 1) CYCLE Loop_ipar

        weight = gtweight(neq%par(ipar),'A')
        IF (weight /= 0d0) THEN
          IF (.NOT. clkRef) THEN
            clkHed%ref(1)%clk(:)%sigma = weight/C
            clkRef = .TRUE.
          ENDIF
          neq%aNor( ikf(ipar,ipar) ) = neq%aNor( ikf(ipar,ipar) ) + weight

          neq%misc%npseu = neq%misc%npseu + 1
          IF (ipart /= 0 .AND. isToBeEl(neq%par(ipar),ipart,ifil)) &
            neq%misc%npseuel = neq%misc%npseuel + 1

          CYCLE Loop_ipar
        ENDIF

        DO iClk = 1,clkHed%ref(1)%nRef

          ! Few observations
          IF ( neq%par(iPar)%locq(6) <= 2 ) EXIT
          IF ( neq%par(ipar)%locq(7) == 0 ) EXIT
          IF ( neq%aNor( ikf(ipar,ipar) ) == 0d0 ) EXIT
          IF ( neq%par(ipar)%x0 == 0d0 ) CYCLE

          IF (neq%par(ipar)%name == clkHed%ref(1)%clk(iClk)%name) THEN

            numref = 0
            DO jpar = 1,neq%misc%npar
              IF ( neq%par(ipar)%time%mean /= neq%par(jpar)%time%mean) CYCLE
              IF ( neq%par(jpar)%locq(1) == 23 .OR. &
                   neq%par(jpar)%locq(1) == 24 ) THEN
                ! Few observations
                IF ( neq%par(jPar)%locq(6) <= 2 ) CYCLE
                IF ( neq%par(jpar)%locq(7) == 0 ) CYCLE
                IF ( neq%aNor( ikf(jpar,jpar) ) == 0d0 ) CYCLE
                IF ( neq%par(jpar)%x0 == 0d0 ) CYCLE
                DO jClk = 1,clkHed%ref(1)%nRef
                  IF (neq%par(jpar)%name == clkHed%ref(1)%clk(jClk)%name) THEN
                    numRef = numRef + 1
                  ENDIF
                ENDDO
              ENDIF
            ENDDO

            IF (numRef == 0) CYCLE Loop_ipar

            DO jpar = ipar,neq%misc%npar
              IF ( neq%par(ipar)%time%mean /= neq%par(jpar)%time%mean) CYCLE
              IF ( neq%par(jpar)%locq(1) == 23 .OR. &
                   neq%par(jpar)%locq(1) == 24 ) THEN
                ! Few observations
                IF ( neq%par(jPar)%locq(6) <= 2 ) CYCLE
                IF ( neq%par(jpar)%locq(7) == 0 ) CYCLE
                IF ( neq%aNor( ikf(jpar,jpar) ) == 0d0 ) CYCLE
                IF ( neq%par(jpar)%x0 == 0d0 ) CYCLE
                DO jClk = 1,clkHed%ref(1)%nRef
                  IF (neq%par(jpar)%name == clkHed%ref(1)%clk(jClk)%name) THEN
                    neq%aNor( ikf(ipar,jpar) ) = neq%aNor( ikf(ipar,jpar) ) + &
                                                 (opt%sigma0/numRef*1000d0)**2
                    IF (.NOT. clkRef) THEN
                      clkHed%ref(1)%clk(:)%sigma = clkHed%ref(1)%nRef/1000d0/C
                      clkRef = .TRUE.
                    ENDIF
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
            CYCLE Loop_ipar
          ENDIF
        ENDDO
      ENDIF

! Receiver antenna phase patterns
! --------------------------------
      IF ( neq%par(ipar)%locq(1) == 18 ) THEN
        freq = MOD(neq%par(ipar)%locq(4),100)
        gsys = MOD(neq%par(ipar)%locq(5),100)
        weight = ( opt%sigma0/0.00001D0/( neq%par(ipar)%locq(6) *         &
                                      ( neq%par(ipar)%locq(7)-1 ) ) )**2
        DO ii = ipar, neq%misc%npar
          IF ( neq%par(ii)%locq(1) == 18                    .AND.       &
               neq%par(ii)%name    == neq%par(ipar)%name    .AND.       &
               neq%par(ii)%locq(2) == neq%par(ipar)%locq(2) .AND.       &
               neq%par(ii)%locq(3) == neq%par(ipar)%locq(3) .AND.       &
               MOD(neq%par(ii)%locq(4),100) == freq         .AND.       &
               MOD(neq%par(ii)%locq(5),100) == gsys ) THEN
            neq%aNor( ikf(ipar,ii) ) = neq%aNor( ikf(ipar,ii) ) + weight
          END IF
        END DO
      END IF

! Scaling factors
! ---------------
      IF ( neq%par(ipar)%locq(1) == 22 .AND. PRESENT(neq0) ) THEN
        weight = opt%sigma0/0.000000001D0
        DO ii = ipar, neq0%misc%npar
          IF (neq0%par(ii)%locq(1) /= 22) CYCLE
          IF ( tstequiv(neq,ipar,neq0,ii) ) THEN
            neq%aNor( ikf(ipar,ipar) ) = neq%aNor( ikf(ipar,ipar) ) + weight
          ENDIF
        ENDDO
      ENDIF

! Satellite antenna phase patterns
! --------------------------------
      IF ( neq%par(ipar)%locq(1) == 25 ) THEN
        weight = ( opt%sigma0/0.00001D0/( neq%par(ipar)%locq(6) *           &
                                    ( neq%par(ipar)%locq(7)-1 ) ) )**2
        DO ii = ipar, neq%misc%npar
          IF ( neq%par(ii)%locq(1) == 25 .AND.                          &
               neq%par(ii)%locq(2) == neq%par(ipar)%locq(2) .AND.       &
               neq%par(ii)%locq(3) == neq%par(ipar)%locq(3) ) THEN
            IF (neq%par(ii)%locq(4) == neq%par(ii)%locq(6)) CYCLE
            IF (neq%par(ipar)%locq(4) == neq%par(ipar)%locq(6)) EXIT
            neq%aNor( ikf(ipar,ii) ) = neq%aNor( ikf(ipar,ii) ) + weight
          END IF
        END DO
      END IF

! General absolute constraints
! ----------------------------
      neq%aNor( ikf(ipar,ipar) ) = neq%aNor( ikf(ipar,ipar) ) + &
                                   gtweight(neq%par(ipar),'A')

      IF (gtweight(neq%par(ipar),'A') > 0d0) THEN
        neq%misc%npseu = neq%misc%npseu + 1
        IF (ipart /= 0 .AND. isToBeEl(neq%par(ipar),ipart,ifil)) &
          neq%misc%npseuel = neq%misc%npseuel + 1
      ENDIF

    ENDIF
  ENDDO Loop_ipar

! Handling of relative constraints
! --------------------------------
Loop_rPar:  DO iPar=1, neq%misc%nPar-1

! Skip if no rel sigma available
    weight = gtweight(neq%par(iPar),'R')
    IF (weight == 0.D0) CYCLE Loop_rPar

    rBound = neq%par(iPar)%time%mean+neq%par(iPar)%time%half
    nxtIdx = 0

! Loop over remaining parameters
    DO ii=iPar+1, neq%misc%nPar
      IF (neq%par(ii)%locq(1) /= neq%par(iPar)%locq(1)) CYCLE
      IF (iPart == 0 .OR. isToBeEl(neq%par(ipar),ipart,ifil) .OR.         &
          isToBeEl(neq%par(ii),ipart,ifil)) THEN

        lBound = neq%par(ii)%time%mean-neq%par(ii)%time%half

! handle different parameter types
        SELECT CASE(neq%par(iPar)%locq(1))

! Troposphere
! -----------
          CASE (6)

! continuous ZPD
            IF (neq%par(iPar)%locq(4)   == 3                     .AND.     &
                neq%par(ii)%name        == neq%par(iPar)%name    .AND.     &
                neq%par(ii)%locq(4)     == neq%par(iPar)%locq(4) .AND.     &
                neq%par(iPar)%time%half == 0.d0                  .AND.     &
                lBound-rBound           <= maxTrp) THEN
              nxtIdx = ii
              EXIT
            ENDIF

! continuous GRD
            IF (neq%par(iPar)%locq(4)   /= 3                     .AND.     &
                neq%par(ii)%name        == neq%par(iPar)%name    .AND.     &
                neq%par(ii)%locq(4)     == neq%par(iPar)%locq(4) .AND.     &
                neq%par(iPar)%time%half == 0.d0                  .AND.     &
                lBound-rBound           <= maxGrd) THEN
              nxtIdx = ii
              EXIT
            ENDIF

! GIM parameters
! --------------
          CASE (19)
            IF (neq%par(ii)%locq(4) == neq%par(iPar)%locq(4) .AND.        &
                neq%par(ii)%locq(5) == neq%par(iPar)%locq(5) .AND.        &
                DABS(lBound-rBound) <= maxDT) THEN
              nxtIdx = ii
              EXIT
            ENDIF

! continuous
            IF (neq%par(ii)%locq(4) == neq%par(iPar)%locq(4) .AND.        &
                neq%par(ii)%locq(5) == neq%par(iPar)%locq(5) .AND.        &
                neq%par(iPar)%time%half == 0.D0              .AND.        &
                neq%par(ii)%time%mean-neq%par(iPar)%time%mean <= maxGim) THEN
              nxtIdx = ii
              EXIT
            ENDIF

! Default
! -------
          CASE DEFAULT
            IF (DABS(lBound-rBound) <= maxDT) THEN
              nxtIdx = ii
              EXIT
            ENDIF

        ENDSELECT
      ENDIF
    ENDDO

    IF (nxtIdx == 0) CYCLE Loop_rPar
    neq%aNor(ikf(iPar,iPar)) = neq%aNor(ikf(iPar,iPar)) + weight
    neq%aNor(ikf(ii,ii))     = neq%aNor(ikf(ii,ii))     + weight
    neq%aNor(ikf(ii,iPar))   = neq%aNor(ikf(ii,iPar))   - weight

    neq%misc%npseu           = neq%misc%npseu + 1
    IF (ipart /= 0 .AND. (isToBeEl(neq%par(ipar),ipart,ifil) .OR. &
                          isToBeEl(neq%par(ii),  ipart,ifil))) &
      neq%misc%npseuel = neq%misc%npseuel + 1

  ENDDO Loop_rPar

! Handling of zero mean conditions
! --------------------------------
Loop_sPar:  DO iPar=1, neq%misc%nPar

    ! Skip if no zero mean condition is available
    weight = gtweight(neq%par(iPar),'S')

    IF (weight == 0.D0) CYCLE Loop_sPar

    IF ( ipart == 0 .OR. isToBeEl(neq%par(ipar),ipart,ifil) ) &
      neq%aNor(ikf(iPar,iPar)) = neq%aNor(ikf(iPar,iPar)) + weight

    nxtIdx = 0

    ! Loop over remaining parameters
    DO ii=iPar+1, neq%misc%nPar
      nxtIdx = 0

      IF (neq%par(ii)%locq(1) /= neq%par(iPar)%locq(1)) CYCLE
      IF (iPart == 0 .OR. isToBeEl(neq%par(ipar),ipart,ifil) .OR.         &
          isToBeEl(neq%par(ii),ipart,ifil)) THEN

        ! handle different parameter types
        SELECT CASE(neq%par(iPar)%locq(1))

! DCB parameters
! --------------
          CASE (8)
            IF (neq%par(ipar)%locq(2) == neq%par(ii)%locq(2)) THEN
              ! Satellite DCBs
              IF ( neq%par(ii)%locq(2) == 1 .AND. &
                   neq%par(ipar)%locq(5) == neq%par(ii)%locq(5) .AND. &
                   INT(neq%par(ipar)%locq(3)/100) == INT(neq%par(ii)%locq(3)/100) ) THEN
                nxtIdx = ii
              ENDIF

              ! Receiver DCBs
              IF ( neq%par(ii)%locq(2) == 2 .AND. &
                   neq%par(ipar)%locq(6) == neq%par(ii)%locq(6).AND. &
                   (neq%par(ipar)%locq(6) /= 3 .OR. &
                    neq%par(ipar)%locq(5) == neq%par(ii)%locq(5) )) THEN
                nxtIdx = ii
              ENDIF
            ENDIF

! GNSS-specific parameters
! ------------------------
          CASE(30)
            tstWgt = gtweight(neq%par(ii),'S')
            IF (neq%par(ipar)%locq(3) == neq%par(ii)%locq(3) .AND. &
                neq%par(ipar)%locq(4) == neq%par(ii)%locq(4) .AND. &
                tstWgt /= 0.d0) THEN
              nxtIdx = ii
            ENDIF

        ENDSELECT
      ENDIF
      IF (nxtIdx /= 0) THEN
        neq%aNor(ikf(ii,iPar)) = neq%aNor(ikf(ii,iPar)) + weight
      ENDIF
    ENDDO

    neq%misc%npseu = neq%misc%npseu + 1
    IF (ipart /= 0 .AND. isToBeEl(neq%par(ipar),ipart,ifil)) &
      neq%misc%npseuel = neq%misc%npseuel + 1

  ENDDO Loop_sPar

END SUBROUTINE adweight

END MODULE
