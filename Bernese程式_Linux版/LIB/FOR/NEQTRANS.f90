MODULE s_NEQTRANS
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE neqtrans(neq,ipart,ifil)

! -------------------------------------------------------------------------
! Purpose:    This interface routine decides which type of transformation
!             should be used and calls the corresponding routine (parameter
!             transformation or parameter binning).
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
!
! Changes:    21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             29-Dec-2001 HU: Interfaces to partrans,parbins added
!             08-Nov-2002 RD: Generate a table for different modelling of ERPs
!             10-Jun-2003 MM: Troposphere: partrans instead of parbins
!             11-Dec-2003 MM: prevent senseless looping wrt troposphere
!             08-Oct-2004 HU: Transform only once per coordinate/velocity
!             19-May-2005 CU: Correct output string
!             28-Jun-2005 MM: Unused SR import removed
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!             21-Nov-2009 RD: Intersystem bias added
!             24-Oct-2010 RD: Distinguish between piece-wise linear param.
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, lfnerr, lfnprt
  USE m_time,   ONLY: OPERATOR(.ISIN.)
  USE d_par,    ONLY: maxlcq,maxParTyp
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: opt

  USE s_alcerr
  USE s_partrans
  USE s_exitrc
  USE f_istobeel
  USE s_gtpartyp
  IMPLICIT NONE

! List of Parameters
! ------------------
! input,output:
  TYPE(t_neq)                     :: neq

! input:
  INTEGER(i4b)                    :: ipart
  INTEGER(i4b)                    :: ifil

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER     :: srName = 'neqtrans'

  CHARACTER(LEN=16),DIMENSION(5),PARAMETER :: parString =       &
  (/  'X-pole          ','Y-pole          ','length of day   ', &
      'nutation epsilon','nutation psi    '                    /)

  CHARACTER(LEN=21), DIMENSION(0:3), PARAMETER :: erpModelStr = &
  (/  'unknwon              ', 'ADDNEQ2, continuous  ',         &
      'GPSEST,  offset only ', 'GPSEST,  offset/drift'         /)

! Local Variables
! ---------------
  INTEGER(i4b)                    :: ireq
  INTEGER(i4b)                    :: ipar
  INTEGER(i4b)                    :: icrd
  INTEGER(i4b)                    :: iTyp
  INTEGER(i4b)                    :: jFil,jFil2
  INTEGER(i4b)                    :: maxisb
  INTEGER(i4b)                    :: iLcq
  INTEGER(i4b), DIMENSION(:,:,:),  &
                ALLOCATABLE, SAVE :: erpModel
                                  ! Type of ERP modelling
                                  ! (1,2 before/after,1..5 erpTyp,iFil)
                                  ! value 1: ADDNEQ2
                                  !       2: GPSEST offset only
                                  !       3: GPSEST offset/drift
                                  !       0: unknown
                                  !      -1: not in files
  INTEGER(i4b)                    :: printIt
  INTEGER(i4b)                    :: ii
  INTEGER(i4b)                    :: irc

  LOGICAL,      DIMENSION(:,:),    &
                ALLOCATABLE, SAVE :: reqStat  ! Requests are satified by the NEQ
                                              ! 1..2:    left/right boundary
                                              ! 1..iReq: request number
  LOGICAL                         :: ok


! First call, init erp-Modelling
! ------------------------------
  IF (iFil == 1 .AND. iPart == 1) THEN
    ALLOCATE(erpModel(2,5,SIZE(opt%neqFileName)),stat=irc)
    CALL alcerr(irc,'erpModel',(/2,5,SIZE(opt%neqFileName)/),srName)
    erpModel = -1

    ALLOCATE(reqStat(2,SIZE(opt%req)),stat=irc)
    CALL alcerr(irc,'reqStat',(/2,SIZE(opt%req)/),srName)
    reqStat=.FALSE.

  ENDIF

! Get the ERP-Modelling before transformation
! -------------------------------------------
  DO iPar = 1,neq%misc%nPar
    IF (iPart == 1 .AND. neq%par(iPar)%locq(1) == 10) THEN
      iTyp = neq%par(iPar)%locq(4)

      erpModel(1,iTyp,iFil) = 0
      IF (neq%par(ipar)%locq(6)   == 1  .AND. &
          neq%par(ipar)%time%half == 0d0) THEN
        erpModel(1,iTyp,iFil) = 1              ! ADDNEQ2

      ELSE IF (neq%par(ipar)%locq(6)   ==  1 .AND. &
               neq%par(ipar)%time%half >  0d0) THEN
        erpModel(1,iTyp,iFil) = 2              ! GPSEST, offset only

      ELSE IF (neq%par(ipar)%locq(6)   ==  2 .AND. &
               neq%par(ipar)%time%half == 0d0) THEN
        erpModel(1,iTyp,iFil) = 3              ! GPSEST, off. (off/drift)

      ELSE IF (neq%par(ipar)%locq(6)   ==  2 .AND. &
               neq%par(ipar)%time%half >  0d0) THEN
        erpModel(1,iTyp,iFil) = 3              ! GPSEST, drift (off/drift)

      ENDIF
    ENDIF

  ENDDO

! Check the transformation requests
! ---------------------------------
  DO ireq = 1, SIZE(opt%req)

    ! Do it only for part 1
    IF ( iPart /= 1 ) EXIT

    ! The request is already "OK"
    IF (reqStat(1,ireq) .AND. reqStat(2,ireq)) CYCLE

    ! Check only ERPs
    IF ( opt%req(ireq)%locq(1) /= 10 ) THEN
      reqStat(:,ireq) = .TRUE.
      CYCLE
    ENDIF

! Loop all parameters
! -------------------
    DO iPar = 1,neq%misc%nPar

! Get the parameters corresponding to the request
! -----------------------------------------------
      ok = .TRUE.
      DO ilcq = 1, maxLcq
        IF ( neq%par(ipar)%locq(ilcq)  /= opt%req(iReq)%locq(ilcq) .AND. &
             opt%req(iReq)%locq(ilcq)  /= 0 ) ok = .FALSE.
      END DO

      ok = ok .AND. (LEN_TRIM(opt%req(iReq)%name)==0 .OR. &
                     neq%par(ipar)%name == opt%req(iReq)%name)

      ok = ok .AND. &
           (neq%par(ipar)%time%mean .isIn. opt%req(iReq)%timint)

      IF (.NOT. ok) CYCLE

      iTyp = neq%par(iPar)%locq(4)

      ! ADDNEQ2: Each request boundary needs a parameter for its epoch
      IF (erpModel(1,iTyp,iFil) == 1) THEN
        IF (DABS(neq%par(ipar)%time%mean - &
                 opt%req(ireq)%timint%t(1))*86400d0 < &
                 neq%misc%nsmpnq/2) reqStat(1,ireq) = .TRUE.
        IF (DABS(neq%par(ipar)%time%mean - &
                 opt%req(ireq)%timint%t(2))*86400d0 < &
                 neq%misc%nsmpnq/2) reqStat(2,ireq) = .TRUE.

      ! GPSEST, offset: Each request boundary needs a parameter boundary
      ELSE IF (erpModel(1,iTyp,iFil) == 2) THEN
        IF (DABS(neq%par(ipar)%time%mean - neq%par(ipar)%time%half - &
                 opt%req(ireq)%timint%t(1))*86400d0 < &
                 neq%misc%nsmpnq/2) reqStat(1,ireq) = .TRUE.
        IF (DABS(neq%par(ipar)%time%mean + neq%par(ipar)%time%half - &
                 opt%req(ireq)%timint%t(2))*86400d0 < &
                 neq%misc%nsmpnq/2) reqStat(2,ireq) = .TRUE.

      ! GPSEST, offset/drift: Each request boundary needs a parameter boundary
      ELSE IF (erpModel(1,iTyp,iFil) == 3) THEN
        IF (DABS(neq%par(ipar)%time%mean - neq%par(ipar)%time%half - &
                 opt%req(ireq)%timint%t(1))*86400d0 < &
                 neq%misc%nsmpnq/2) reqStat(1,ireq) = .TRUE.
        IF (DABS(neq%par(ipar)%time%mean + neq%par(ipar)%time%half - &
                 opt%req(ireq)%timint%t(2))*86400d0 < &
                 neq%misc%nsmpnq/2) reqStat(2,ireq) = .TRUE.

      ENDIF

    ENDDO

  ENDDO

! Print warning after checking the last NEQ-file
! ----------------------------------------------
  IF (iFil == SIZE(opt%neqFileName) .AND. iPart == 1) THEN

    DO iPar = 1,maxParTyp

      IF (iPar /= 10) CYCLE

      DO iTyp = 1,5

        jFil = 0
        DO ii = 1,SIZE(opt%neqFileName)
          IF (erpModel(1,iTyp,ii) /= -1) jFil=ii
        ENDDO
        IF (jFil == 0) CYCLE

        DO iReq = 1,SIZE(opt%req)
          IF (opt%req(ireq)%locq(1) /= iPar) CYCLE
          IF (opt%req(ireq)%locq(4) /= iTyp) CYCLE
          IF (reqStat(1,iReq) .AND. reqStat(2,iReq)) CYCLE

          WRITE(lfnerr,'(/,A,3(/,18X,A),/)')                              &
          ' ### SR NEQTRANS: The new number of intervals is ' // &
                                                    'inconsistent with ', &
               'the parameter intervals in the NEQs. ',                   &
               'Parameter type:       ' // TRIM(parString(iTyp))
          EXIT
        ENDDO ! iReq

      ENDDO ! iTyp

    ENDDO ! iPar

    DEALLOCATE(reqStat,stat=irc)

  ENDIF



! Loop all transformation requests
! --------------------------------
  DO ireq = 1, SIZE(opt%req)
    SELECT CASE ( opt%req(ireq)%locq(1) )

! Not active
! ----------
    CASE (0)
      CONTINUE

! Station coordinates (velocities) - loop over all stations
! ---------------------------------------------------------
    CASE (1)
      DO ipar = 1, neq%misc%npar

        IF (isToBeEl(neq%par(ipar),ipart,ifil)) CYCLE

        IF ( neq%par(ipar)%locq(1) == opt%req(ireq)%locq(1).AND. &
             neq%par(ipar)%locq(3) == 1 .AND. neq%par(ipar)%locq(4) == 1) THEN
          opt%req(ireq)%name = neq%par(ipar)%name
          DO icrd = 1, 3
            opt%req(ireq)%locq(3) = icrd
            CALL partrans(neq,opt%req(ireq),ipart,ifil)
          END DO
        END IF
      END DO


! Inter-system biases - loop over all stations
! --------------------------------------------
    CASE (2)
      DO ipar = 1, neq%misc%npar
        IF (neq%par(ipar)%locq(1) == opt%req(ireq)%locq(1).AND. &
            neq%par(ipar)%locq(6) == opt%req(ireq)%locq(6)) THEN
          IF (neq%par(iPar)%name == opt%req(ireq)%name) CYCLE
          opt%req(ireq)%name = neq%par(ipar)%name
          CALL partrans(neq,opt%req(ireq),ipart,iFil)
        END IF
      END DO

      maxisb=0
      DO ipar = 1, neq%misc%npar
        IF (neq%par(ipar)%locq(1) == 2 .AND. neq%par(ipar)%locq(6) == 5) &
          maxisb = MAX(maxisb,neq%par(ipar)%locq(5),neq%par(ipar)%locq(7))
      ENDDO
      DO ipar = 1, neq%misc%npar
        IF (neq%par(ipar)%locq(1) == 2 .AND. neq%par(ipar)%locq(6) == 5) &
          neq%par(ipar)%locq(7) = maxisb
      ENDDO
      opt%req(ireq)%name = ' '


! Troposphere parameters - loop over all stations
! -----------------------------------------------
    CASE (6)
      DO ipar = 1, neq%misc%npar
        IF (neq%par(ipar)%locq(1) == opt%req(ireq)%locq(1)) THEN
          IF (neq%par(iPar)%name == opt%req(ireq)%name) CYCLE
          opt%req(ireq)%name = neq%par(ipar)%name
          CALL partrans(neq,opt%req(ireq),ipart,iFil)
        END IF
      END DO
      opt%req(ireq)%name = ' '


! Earth orientation parameters
! ----------------------------
    CASE (10)
      CALL partrans(neq,opt%req(ireq),ipart,ifil)

    CASE DEFAULT
      WRITE(lfnerr,'(" *** SR NEQTRANS: WRONG PARAMETER TYPE")')
      CALL exitrc(2)
    END SELECT
  END DO


! Get the ERP-Modelling after transformation
! ------------------------------------------
  DO iPar = 1,neq%misc%nPar
    IF (iPart == 1 .AND. neq%par(iPar)%locq(1) == 10) THEN
      iTyp = neq%par(iPar)%locq(4)

      erpModel(2,iTyp,iFil) = 0
      IF (neq%par(ipar)%locq(6)   == 1  .AND. &
          neq%par(ipar)%time%half == 0d0) THEN
        erpModel(2,iTyp,iFil) = 1              ! ADDNEQ2

      ELSE IF (neq%par(ipar)%locq(6)   ==  1 .AND. &
               neq%par(ipar)%time%half >  0d0) THEN
        erpModel(2,iTyp,iFil) = 2              ! GPSEST, offset only

      ELSE IF (neq%par(ipar)%locq(6)   ==  2 .AND. &
               neq%par(ipar)%time%half == 0d0) THEN
        erpModel(2,iTyp,iFil) = 3              ! GPSEST, off. (off/drift)

      ELSE IF (neq%par(ipar)%locq(6)   ==  2 .AND. &
               neq%par(ipar)%time%half >  0d0) THEN
        erpModel(2,iTyp,iFil) = 3              ! GPSEST, drift (off/drift)

      ENDIF

    ENDIF

  ENDDO

! Write a table in the last run in part 1
! ---------------------------------------
  IF (iFil == SIZE(opt%neqFileName) .AND. iPart == 1) THEN

    ! Something has been changed
    printIt = 0

    DO iTyp = 1,5
      DO jFil = 1,SIZE(opt%neqFileName)
        printIt = printIt + ABS(erpModel(2,iTyp,jFil)-erpModel(1,iTyp,jFil))
      ENDDO
    ENDDO

    IF (printIt > 0) THEN

      ! Loop all ERP types
      DO iTyp = 1,5

        ! Loop all Files
        jFil = 0
        DO WHILE (jFil < SIZE(opt%neqFileName))
          jFil = jFil + 1

          ! Is an ERP in this file?
          IF (erpModel(1,iTyp,jFil) /= -1) THEN

            ! Get all file numbers with the same modelling
            DO jFil2 = jFil,SIZE(opt%neqFileName)-1
              IF (erpModel(1,iTyp,jFil) /= erpModel(1,iTyp,jFil2+1) .OR. &
                  erpModel(2,iTyp,jFil) /= erpModel(2,iTyp,jFil2+1))  EXIT
            ENDDO

            ! Write a title (if necessary)
            IF (printIt > 0) THEN
              WRITE(lfnprt,'(//,A,/,A,//,A,/,A)')                &
              ' Transformation of ERP-modelling:',               &
              ' -------------------------------',                &
              ' Pole parameter       before transformation ' //  &
              '  after transformation      NEQ file number',     &
              ' -------------------------------------------' //  &
              '--------------------------------------------' //  &
              '--------------------------------------------'

              printIt = 0
            ENDIF

            ! Write the output line
            WRITE(lfnprt,'(1X,A,5X,A,3X,A,5X,A,I4,A,I4)')       &
                 parString(iTyp),                               &
                 erpModelStr(erpModel(1,iTyp,jFil)),            &
                 erpModelStr(erpModel(2,iTyp,jFil)),            &
                 'from ',jFil,' to ',jFil2

            jFil = jFil2

          ENDIF
        ENDDO ! Loop all files
      ENDDO ! Loop all ERPs

    ENDIF

    DEALLOCATE(erpModel,stat=irc)
  ENDIF

! Identify the type of a piece-wise linear parameter
! --------------------------------------------------
  CALL gtpartyp(neq)

  RETURN
END SUBROUTINE neqtrans


END MODULE
