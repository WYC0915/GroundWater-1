MODULE f_TSTEQUIV
CONTAINS

! -------------------------------------------------------------------------
! Bernese Software Version 5.2
! -------------------------------------------------------------------------

LOGICAL FUNCTION tstequiv(neq,ipar,neq_1,ipar_1)

! -------------------------------------------------------------------------
! Purpose:    This function returns .TRUE. if two paramters from two neq
!             systems are equal (and thus may be stacked), .FALSE.
!             otherwise
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
!
! Changes:    18-May-2000 SS: Handle DCB parameters
!             31-May-2000 SS: Do not consider time boundaries related to
!                             satellite DCB parameters
!             20-Dec-2001 SS: Handle GIM parameters
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             12-Feb-2001 SS: Receiver DCB handling changed
!             07-Mar-2003 DT: Time tolerance for ERPs increased to 30 min
!             22-May-2003 MM: Piecewise linear troposphere
!             14-Aug-2003 HU: Satellite pcv added (typ 25)
!             21-Jan-2004 SS/HU: Corrections wrt satellite pcv
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             04-Oct-2006 AG: Comparison of satellite antenna offsets improved
!             25-Jan-2008 RD: add RAO/RAP parameters
!             26-May-2008 DT: Receiver clock / time biases added (locq(1)=2)
!             18-Sep-2008 DT: No stacking for dynamic orbit parameters
!             08-Apr-2009 DT: Range biases added (locq(1)=26)
!             27-Apr-2009 LM/SL: Check parameter type added
!             04-May-2009 RD: Scaling of loading models added
!             10-May-2009 RD: Receiver clock offsets/biases implemented
!             10-Jun-2009 RD: Add epoch satellite/receiver clocks
!             04-Jan-2010 SL: HOI scaling parameters (Case 27) added
!             12-Aug-2010 DT: Check technique for SAO
!             08-Sep-2010 RD: Merge SLR-time bias option
!             24-Oct-2010 RD: Distinguish between piece-wise linear param.
!             30-Nov-2010 DT: Add Helmert parameters (28)
!             30-Nov-2010 MM: GNSS-specific parameters
!             24-Mar-2011 DT: Use name(1:14) for RGB
!             20-Jul-2011 LP: Sat-spec. obstypes
!             20-Sep-2012 RD: Use M_BERN with ONLY
!             20-Sep-2012 RD: Remove unused variables
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnerr
  USE m_time,   ONLY: t_timint, OPERATOR(.isIn.)
  USE d_neq,    ONLY: t_neq
  USE d_par,    ONLY: is_techn, isParTyp, parType_linear
  USE d_satcrx, ONLY: gtsats
  USE p_addneq, ONLY: opt, staInfo

  USE s_defreq
  USE s_exitrc
  USE s_alcerr
  USE f_lincount
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq),INTENT(IN)  :: neq
  INTEGER(i4b),INTENT(IN) :: ipar
  TYPE(t_neq),INTENT(IN)  :: neq_1
  INTEGER(i4b),INTENT(IN) :: ipar_1

! Local parameters
! ----------------
  CHARACTER(LEN=8),PARAMETER :: srName = 'tstequiv'

! Local Variables
! ---------------
  TYPE(t_timint)                                 :: timint

  INTEGER(i4b)                                   :: partyp
  INTEGER(i4b)                                   :: partyp_1
  INTEGER(i4b)                                   :: icrd
  INTEGER(i4b)                                   :: icrd_1
  INTEGER(i4b)                                   :: ireq
  INTEGER(i4b)                                   :: igrp
  INTEGER(i4b)                                   :: nSat
  INTEGER(i4b)                                   :: freq1,freq2
  INTEGER(i4b)                                   :: iSta,jSta
  INTEGER(i4b)                                   :: iMan
  INTEGER(i4b),SAVE                              :: maxman,nman
  INTEGER(i4b),SAVE,DIMENSION(:),    ALLOCATABLE :: satman
  INTEGER(i4b)                                   :: iac
  INTEGER(i4b)                                   :: indobst, indobst_1, obs

  REAL(r8b),   SAVE,DIMENSION(:),    ALLOCATABLE :: timman

  LOGICAL,     SAVE                              :: first = .TRUE.
  LOGICAL,     SAVE                              :: velFlg
  LOGICAL                                        :: sameobst

  INCLUDE 'COMFREQ.inc'

  IF (first) THEN
    first = .FALSE.
    velFlg = .FALSE.
    DO ireq = 1, SIZE(opt%req)
      IF (opt%req(ireq)%locq(1) == 1) THEN
        velFlg = .TRUE.
        EXIT
      END IF
    END DO
    maxman = -1
  END IF

  partyp   = neq%par(ipar)%locq(1)
  partyp_1 = neq_1%par(ipar_1)%locq(1)

  IF ( partyp /= partyp_1 ) THEN
    tstequiv = .FALSE.
    RETURN
  END IF

  ! Check Parameter Type
  ! --------------------
  IF ( neq%par(ipar)%type      /= "" .AND. &
       neq_1%par(ipar_1)%type  /= "" .AND. &
       .NOT.isParTyp(neq%par(ipar),     parType_linear) .AND.&
       .NOT.isParTyp(neq_1%par(ipar_1), parType_linear) .AND.&
       neq%par(ipar)%type      /= neq_1%par(ipar_1)%type ) THEN
    tstequiv = .FALSE.
    RETURN
  ENDIF

  SELECT CASE ( partyp )

! Station coordinates
! -------------------
  CASE (1)
    icrd    = neq%par(ipar)%locq(3)
    icrd_1  = neq_1%par(ipar_1)%locq(3)
    IF ( ( neq%par(ipar)%name == neq_1%par(ipar_1)%name .AND.               &
           icrd               == icrd_1                       )      .AND.  &
         ( .NOT. velFlg                                 .OR.                &
           neq%par(ipar)%time%mean == neq_1%par(ipar_1)%time%mean ) ) THEN
      tstequiv = .TRUE.
    ELSE
      tstequiv = .FALSE.
    END IF

! Receiver clock offsets/biases
! -----------------------------
  CASE (2)

    IF ( neq%par(ipar)%locq(6) == 0 ) THEN   ! SLR time bias
      IF (neq%par(ipar)%name      == neq_1%par(ipar_1)%name    .AND. &
          neq%par(ipar)%locq(6)   == neq_1%par(ipar_1)%locq(6) .AND. &
          neq%par(ipar)%locq(7)   == neq_1%par(ipar_1)%locq(7)) THEN
        tstequiv = .TRUE.
      ELSE
        tstequiv = .FALSE.
      END IF

    ELSEIF ( neq%par(ipar)%locq(6) == 5 ) THEN
      IF (neq%par(ipar)%name      == neq_1%par(ipar_1)%name    .AND. &
          neq%par(ipar)%locq(3)   == neq_1%par(ipar_1)%locq(3) .AND. &
          neq%par(ipar)%locq(6)   == neq_1%par(ipar_1)%locq(6) .AND. &
          dabs(neq%par(ipar)%time%mean-neq_1%par(ipar_1)%time%mean)<=0.1d0/86400.d0) THEN
        tstequiv = .TRUE.
      ELSE
        tstequiv = .FALSE.
      END IF
    ELSEIF ( neq%par(ipar)%name == neq_1%par(ipar_1)%name    .AND. &
         neq%par(ipar)%locq(6)  == neq_1%par(ipar_1)%locq(6) .AND. &
         neq%par(ipar)%locq(4)  == neq_1%par(ipar_1)%locq(4) ) THEN

      freq1 =  999999
      freq2 =  999999

      ! Check if the satellite has changed the frequency
      IF ( abs(neq%par(ipar)%locq(6)) == 2 ) THEN

        freq1 =  999999
        timint%t(1) = neq%par(ipar)%time%mean - neq%par(ipar)%time%half
        timint%t(2) = neq%par(ipar)%time%mean + neq%par(ipar)%time%half
        CALL defreq(timint%t,1,(/ neq%par(ipar)%locq(4) /), nSat)

        IF (nSat == 0) freq1=freqnm(neq%par(ipar)%locq(4))

        freq2 = -999999
        timint%t(1) = neq_1%par(ipar_1)%time%mean - neq_1%par(ipar_1)%time%half
        timint%t(2) = neq_1%par(ipar_1)%time%mean + neq_1%par(ipar_1)%time%half
        CALL defreq(timint%t,1,(/ neq_1%par(ipar_1)%locq(4) /), nSat)

        IF (nSat == 0) freq2=freqnm(neq_1%par(ipar_1)%locq(4))

      ENDIF

      IF (freq1 /= freq2) THEN
        tstequiv = .FALSE.

      ! Check if it is still the same receiver on the station
      ELSE
        tstequiv = .TRUE.
        DO iSta = 1,staInfo%nInfo

          IF ( neq%par(ipar)%name /= staInfo%staInfo(iSta)%stanam ) CYCLE
          IF ( .NOT. (neq%par(ipar)%time%mean .isin. staInfo%staInfo(iSta)%timint)) CYCLE

          IF ( neq_1%par(ipar_1)%time%mean .isin. staInfo%staInfo(iSta)%timint ) THEN
            tstequiv = .TRUE.
            EXIT
          ENDIF

          tstequiv = .FALSE.
          DO jSta = 1,staInfo%nInfo
            IF ( neq_1%par(ipar_1)%name /= staInfo%staInfo(jSta)%stanam ) CYCLE
            IF ( .NOT. (neq_1%par(ipar_1)%time%mean .isin. staInfo%staInfo(jSta)%timint)) CYCLE

            IF ( staInfo%staInfo(iSta)%recnam == staInfo%staInfo(jSta)%recnam .AND. &
                 staInfo%staInfo(iSta)%recnum == staInfo%staInfo(jSta)%recnum ) THEN
              tstequiv = .TRUE.
              EXIT
            ENDIF
          ENDDO
        ENDDO
      ENDIF

    ELSE
      tstequiv = .FALSE.
    END IF

! Orbits
! ------
  CASE (3)
    IF ( neq%par(ipar)%locq(2) == neq_1%par(ipar_1)%locq(2)   .AND. &
         neq%par(ipar)%locq(3) == neq_1%par(ipar_1)%locq(3)   .AND. &
         neq%par(ipar)%locq(4) == neq_1%par(ipar_1)%locq(4) ) THEN

    ! Split dynamical parameters / no stacking
      IF ( neq%par(ipar)%locq(4) >= 7 .AND. opt%splitDyn == 1   .AND. &
           neq%par(ipar)%locq(7) /= neq_1%par(ipar_1)%locq(7) ) THEN
        tstequiv = .FALSE.

      ELSE
        tstequiv = .TRUE.
      END IF

    ELSE
      tstequiv = .FALSE.
    END IF

! Receiver antenna offsets
! ------------------------
  CASE (5)
    IF ( neq%par(ipar)%name    == neq_1%par(ipar_1)%name      .AND. &
         neq%par(ipar)%locq(2) == neq_1%par(ipar_1)%locq(2)   .AND. &
         neq%par(ipar)%locq(3) == neq_1%par(ipar_1)%locq(3)   .AND. &
         neq%par(ipar)%locq(4) == neq_1%par(ipar_1)%locq(4)   .AND. &
         neq%par(ipar)%locq(5) == neq_1%par(ipar_1)%locq(5) ) THEN
      tstequiv = .TRUE.
    ELSE
      tstequiv = .FALSE.
    END IF

! Troposphere (individual stations)
! ---------------------------------
  CASE (6)
    IF ( neq%par(ipar)%locq(4)   == neq_1%par(ipar_1)%locq(4)     .AND. &
         neq%par(ipar)%name      == neq_1%par(ipar_1)%name        .AND. &
         abs(neq%par(ipar)%time%mean-neq_1%par(ipar_1)%time%mean)       &
         <=1.d0/1440.d0) THEN
      tstequiv = .TRUE.
    ELSE
      tstequiv = .FALSE.
    END IF


! DCB parameters
! --------------
  CASE (8)
    igrp = neq%par(ipar)%locq(2)
    IF ( igrp == 1) THEN
      sameobst  = .FALSE.
      indobst   = neq%par(ipar)%locq(7)
      indobst_1 = neq_1%par(ipar_1)%locq(7)
      IF ((indobst>0).AND.(indobst_1>0)) sameobst  = .TRUE.

      IF (sameobst) THEN
        DO obs = 1,4
          IF (neq%misc%obst(indobst)%obstyp(obs).NE.neq_1%misc%obst(indobst_1)%obstyp(obs)) THEN
            sameobst  = .FALSE.
            EXIT
          ENDIF
        ENDDO
      ENDIF

      IF ((indobst.EQ.0).AND.(indobst_1.EQ.0)) sameobst  = .TRUE.

      IF ( neq%par(ipar)%locq(2) == neq_1%par(ipar_1)%locq(2)   .AND. &
           neq%par(ipar)%locq(3) == neq_1%par(ipar_1)%locq(3)   .AND. &
           neq%par(ipar)%locq(5) == neq_1%par(ipar_1)%locq(5)   .AND. &
           sameobst                                           ) THEN

        tstequiv = .TRUE.
      ELSE
        tstequiv = .FALSE.
      END IF
    ELSE
      IF ( neq%par(ipar)%locq(2) == neq_1%par(ipar_1)%locq(2)            .AND. &
           neq%par(ipar)%locq(5) == neq_1%par(ipar_1)%locq(5)            .AND. &
           neq%par(ipar)%locq(6) == neq_1%par(ipar_1)%locq(6)            .AND. &
           neq%par(ipar)%name    == neq_1%par(ipar_1)%name ) THEN
        tstequiv = .TRUE.
      ELSE
        tstequiv = .FALSE.
      END IF
    END IF

! Earth rotation parameters
! -------------------------
  CASE (10)
    IF ( neq%par(ipar)%locq(2)   == neq_1%par(ipar_1)%locq(2)         .AND. &
         neq%par(ipar)%locq(4)   == neq_1%par(ipar_1)%locq(4)         .AND. &
         neq%par(ipar)%locq(5)   == neq_1%par(ipar_1)%locq(5)         .AND. &
         ( ABS(neq%par(ipar)%time%mean - neq_1%par(ipar_1)%time%mean)       &
           <= 30.d0/1440.d0 ) )                                         THEN
      tstequiv = .TRUE.
    ELSE
      tstequiv = .FALSE.
    END IF

! Stochastic orbital parameters
! -----------------------------
  CASE (11)
    IF ( neq%par(ipar)%locq(3)   == neq_1%par(ipar_1)%locq(3)     .AND. &
         neq%par(ipar)%locq(5)   == neq_1%par(ipar_1)%locq(5)     .AND. &
         neq%par(ipar)%time%mean == neq_1%par(ipar_1)%time%mean ) THEN
      tstequiv = .TRUE.
    ELSE
      tstequiv = .FALSE.
    END IF

! Satellite antenna offsets
! -------------------------
  CASE (12)
    sameobst  = .FALSE.
    indobst   = neq%par(ipar)%locq(7)
    indobst_1 = neq_1%par(ipar_1)%locq(7)
    IF ((indobst>0).AND.(indobst_1>0)) sameobst  = .TRUE.

    IF (sameobst) THEN
        DO obs = 1,4
          IF (neq%misc%obst(indobst)%obstyp(obs).NE.neq_1%misc%obst(indobst_1)%obstyp(obs)) THEN
            sameobst  = .FALSE.
            EXIT
          ENDIF
        ENDDO
    ENDIF

    IF ((indobst.EQ.0).AND.(indobst_1.EQ.0)) sameobst  = .TRUE.

    IF (neq%par(ipar)%locq(3)   == neq_1%par(ipar_1)%locq(3)   .AND. &
        neq%par(ipar)%locq(5)   == neq_1%par(ipar_1)%locq(5)   .AND. &
        neq%par(ipar)%locq(6)   == neq_1%par(ipar_1)%locq(6)   .AND. &
        sameobst                                             ) THEN

      IF ( ( is_techn(neq%par(ipar), gnss=1) .AND. &
             is_techn(neq_1%par(ipar_1), gnss=1)  ) .OR. &
           ( is_techn(neq%par(ipar), slr=1) .AND. &
             is_techn(neq_1%par(ipar_1), slr=1)  )      ) THEN

        tstequiv = .TRUE.

      ELSE
        tstequiv = .FALSE.
      END IF

    ELSE
      tstequiv = .FALSE.
    END IF

! Center of mass
! --------------
  CASE (16)
    IF ( neq%par(ipar)%locq(2)   == neq_1%par(ipar_1)%locq(2) ) THEN
      tstequiv = .TRUE.
    ELSE
      tstequiv = .FALSE.
    END IF

! Receiver antenna pattern
! ------------------------
  CASE (18)
    IF ( neq%par(ipar)%name    == neq_1%par(ipar_1)%name      .AND. &
         neq%par(ipar)%locq(2) == neq_1%par(ipar_1)%locq(2)   .AND. &
         neq%par(ipar)%locq(3) == neq_1%par(ipar_1)%locq(3)   .AND. &
         neq%par(ipar)%locq(4) == neq_1%par(ipar_1)%locq(4)   .AND. &
         neq%par(ipar)%locq(5) == neq_1%par(ipar_1)%locq(5)   .AND. &
         neq%par(ipar)%locq(6) == neq_1%par(ipar_1)%locq(6)   .AND. &
         neq%par(ipar)%locq(7) == neq_1%par(ipar_1)%locq(7) ) THEN
      tstequiv = .TRUE.
    ELSE
      tstequiv = .FALSE.
    END IF

! GIM parameters
! --------------
  CASE (19)
    IF ( neq%par(ipar)%locq(2)   == neq_1%par(ipar_1)%locq(2)         .AND. &
         neq%par(ipar)%locq(4)   == neq_1%par(ipar_1)%locq(4)         .AND. &
         neq%par(ipar)%locq(5)   == neq_1%par(ipar_1)%locq(5)         .AND. &
         ( ABS(neq%par(ipar)%time%mean - neq_1%par(ipar_1)%time%mean)       &
           <= 1.d0/1440.d0 ) )                                         THEN
      tstequiv = .TRUE.
    ELSE
      tstequiv = .FALSE.
    END IF

! Scaling factors for Vienna grid files
! -------------------------------------
  CASE (22)
    IF ( neq%misc%grdNeq(neq%par(ipar)%locq(2)) == &
               neq_1%misc%grdNeq(neq_1%par(ipar_1)%locq(2)) .AND.  &
         neq%par(ipar)%locq(2) == neq_1%par(ipar_1)%locq(2) .AND.  &
         neq%par(ipar)%locq(3) == neq_1%par(ipar_1)%locq(3) .AND.  &
         neq%par(ipar)%locq(4) == neq_1%par(ipar_1)%locq(4) .AND.  &
         neq%par(ipar)%locq(5) == neq_1%par(ipar_1)%locq(5) .AND.  &
         neq%par(ipar)%name    == neq_1%par(ipar_1)%name    ) THEN
      tstequiv = .TRUE.
    ELSE
      tstequiv = .FALSE.
    END IF

! Station clocks
! --------------
  CASE (23)
    IF ( neq%par(ipar)%name      == neq_1%par(ipar_1)%name            .AND. &
         ( ABS(neq%par(ipar)%time%mean - neq_1%par(ipar_1)%time%mean)       &
           <= 1.d0/1440.d0 ) )                                         THEN
      tstequiv = .TRUE.
    ELSE
      tstequiv = .FALSE.
    END IF

! Satellite clocks
! ----------------
  CASE (24)
    IF ( neq%par(ipar)%name      == neq_1%par(ipar_1)%name            .AND. &
         ( ABS(neq%par(ipar)%time%mean - neq_1%par(ipar_1)%time%mean)       &
           <= 1.d0/1440.d0 ) )                                         THEN
      tstequiv = .TRUE.

      ! Check SATCRUX for arc splitting
      IF (maxman == -1) THEN
        IF (LEN_TRIM(opt%satCrux) > 0) THEN
          maxman = linCount(opt%satCrux,6)
        ELSE
          maxman = 0
        ENDIF
        IF (maxman > 0) THEN
          ALLOCATE(satman(maxman),stat=iac)
          CALL alcerr(iac,'satman',(/maxman/),srName)
          ALLOCATE(timman(maxman),stat=iac)
          CALL alcerr(iac,'timman',(/maxman/),srName)
          CALL gtsats(maxman,nman,satman,timman)
        ELSE
          nMan = 0
        ENDIF
      ENDIF

      ! Do not stack in case of arc-split-events
      DO iMan = 1,nMan
        IF (satman(iMan) == neq%par(ipar)%locq(3)            .AND. &
            DABS(timman(iMan) - neq%par(ipar)%time%mean) < &
                                           neq%misc%nsmpnq/86400d0/2d0) THEN
          tstequiv = .FALSE.
          EXIT
        ENDIF
        IF (satman(iMan) == neq_1%par(ipar_1)%locq(3)        .AND. &
            DABS(timman(iMan) - neq_1%par(ipar_1)%time%mean) < &
                                           neq_1%misc%nsmpnq/86400d0/2d0) THEN
          tstequiv = .FALSE.
          EXIT
        ENDIF
      ENDDO
    ELSE
      tstequiv = .FALSE.
    END IF

! Satellite antenna phase patterns
! --------------------------------
  CASE (25)
    sameobst  = .FALSE.
    READ(neq%par(ipar)%name,'(10X,I5)') indobst
    READ(neq_1%par(ipar_1)%name,'(10X,I5)') indobst_1
    IF ((indobst>0).AND.(indobst_1>0)) sameobst = .TRUE.

    IF (sameobst) THEN
        DO obs = 1,4
          IF (neq%misc%obst(indobst)%obstyp(obs).NE.neq_1%misc%obst(indobst_1)%obstyp(obs)) THEN
            sameobst = .FALSE.
            EXIT
          ENDIF
        ENDDO
    ENDIF

    IF ((indobst.EQ.0).AND.(indobst_1.EQ.0)) sameobst  = .TRUE.

    IF ( neq%par(ipar)%locq(3)   == neq_1%par(ipar_1)%locq(3)   .AND.  &
         neq%par(ipar)%locq(4)   == neq_1%par(ipar_1)%locq(4)   .AND.  &
         neq%par(ipar)%locq(5)   == neq_1%par(ipar_1)%locq(5)   .AND.  &
         neq%par(ipar)%name(1:7) == neq_1%par(ipar_1)%name(1:7) .AND.  &
         sameobst                                             ) THEN
      tstequiv = .TRUE.
    ELSE
      tstequiv = .FALSE.
    END IF

! SLR Range Biases
!  test station name, wavelength, satellite number
! ------------------------------------------------
  CASE (26)
    IF ( neq%par(ipar)%name(1:14) == neq_1%par(ipar_1)%name(1:14) .AND. &
         neq%par(ipar)%locq(4)   == neq_1%par(ipar_1)%locq(4) ) THEN

      IF ( opt%stackRGB == 3  ) THEN

        tstequiv = .TRUE.

      ELSEIF ( opt%stackRGB == 2 ) THEN
        IF ( (neq%par(ipar)%locq(5) > 0        .AND. &
              neq_1%par(ipar_1)%locq(5) > 0    .AND. &
              DNINT(neq%par(ipar)%locq(5)/100d0) ==  &
               DNINT(neq_1%par(ipar_1)%locq(5)/100d0)  ) .OR. &

             (neq%par(ipar)%locq(5) < 0        .AND. &
              neq_1%par(ipar_1)%locq(5) > 0    .AND. &
              ABS(neq%par(ipar)%locq(5))-1 ==        &
               DNINT(neq_1%par(ipar_1)%locq(5)/100d0)  ) .OR. &

             (neq%par(ipar)%locq(5) > 0        .AND. &
              neq_1%par(ipar_1)%locq(5) < 0    .AND. &
              DNINT(neq%par(ipar)%locq(5)/100d0) ==  &
               ABS(neq_1%par(ipar_1)%locq(5))-1        )        ) THEN

          tstequiv = .TRUE.

        ELSE
          tstequiv = .FALSE.
        END IF

      ELSEIF ( opt%stackRGB == 1 .AND. &
               neq%par(ipar)%locq(5) == neq_1%par(ipar_1)%locq(5) ) THEN

        tstequiv = .TRUE.

      ELSE
        tstequiv = .FALSE.
      END IF

    ELSE
      tstequiv = .FALSE.
    END IF


! Higher-order ionosphere (HOI) scaling factors
! ---------------------------------------------
  CASE (27)
    IF (neq%par(ipar)%locq(2) == neq_1%par(ipar_1)%locq(2) .AND. &
        neq%par(ipar)%locq(3) == neq_1%par(ipar_1)%locq(3) .AND. &
        neq%par(ipar)%name    == neq_1%par(ipar_1)%name) THEN
      tstequiv = .TRUE.
    ELSE
      tstequiv = .FALSE.
    END IF


! Helmert transformation parameters
! ---------------------------------
  CASE (28)
    icrd   = neq%par(ipar)%locq(2)
    icrd_1 = neq_1%par(ipar_1)%locq(2)

    IF ( icrd /= icrd_1 ) THEN
      tstequiv = .FALSE.
      RETURN
    END IF

    IF ( opt%helmert(icrd)%stack == 0 ) THEN
      tstequiv = .FALSE.

    ELSEIF ( opt%helmert(icrd)%stack == 2 ) THEN
      tstequiv = .TRUE.

    ELSEIF ( opt%helmert(icrd)%stack == 1            .AND. &
             neq%par(ipar)%name == neq_1%par(ipar_1)%name ) THEN
      tstequiv = .TRUE.

    ELSE
      tstequiv = .FALSE.

    END IF


! GNSS-specific parameters
! ------------------------
  CASE (30)
    IF (neq%par(ipar)%locq(2) == neq_1%par(ipar_1)%locq(2) .AND. &
        neq%par(ipar)%locq(3) == neq_1%par(ipar_1)%locq(3) .AND. &
        neq%par(ipar)%locq(4) == neq_1%par(ipar_1)%locq(4) .AND. &
        neq%par(ipar)%name    == neq_1%par(ipar_1)%name) THEN
      tstequiv = .TRUE.
    ELSE
      tstequiv = .FALSE.
    END IF


! Wrong parameter type
! --------------------
  CASE DEFAULT
    WRITE(lfnerr,*) ' *** SR TSTEQUIV: WRONG PARAMETER TYPE ', partyp
    CALL EXITRC(2)
  END SELECT

END FUNCTION tstequiv

END MODULE
