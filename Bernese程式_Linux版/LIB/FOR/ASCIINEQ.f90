MODULE s_ASCIINEQ
CONTAINS

! -------------------------------------------------------------------------
! Bernese Software Version 5.2
! -------------------------------------------------------------------------

SUBROUTINE asciineq(filinp, neq)

! -------------------------------------------------------------------------
! Purpose:    Read formatted NEQ file
!
! Author:     L. Mervart
!
! Created:    25-Aug-1998
!
! Changes:    08-Sep-2000 HB: use fileNameLength from m_bern
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             22-Dec-2001 HU: Interface to neqalloc added
!             29-Oct-2002 MR: Format corrected (1x)
!             22-May-2003 RD: Use maxsat from m_maxdim (instead of p_addneq)
!             08-Sep-2003 HU: New NEQ format: antnam, recnam chr16 -> chr20
!             14-Apr-2005 HU: Allow 99 sat per antoff group
!             26-Jul-2005 RD/AG: Read locq w/o format (see SR NEQASCII)
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!             08-Feb-2007 RD: misc%nObs/nParms i4b->r8b
!             23-Jan-2008 RD: Read t_par using a function from the module
!             26-Jun-2008 RD: System-specific PCV in SINEX record
!             23-Jul-2008 RD: Use lfnres instead of lfnloc
!             27-Apr-2009 LM/SL: WRITE statement changed to READ,
!                                number of lines to read corrected
!             04-May-2009 RD: Scaling of loading models added, neq%version=6
!             19-Nov-2009 RD: Correct lfn
!             15-Nov-2010 RD: Error messages corrected
!             15-Jul-2011 LP: Sat-spec. obstypes; neq%version set to 8;
!                             test for mxneqver added
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_global, ONLY: maxsys
  USE m_maxdim, ONLY: maxsat
  USE d_par,    ONLY: maxLcq, readpar
  USE d_neq,    ONLY: t_neq,t_sinex_v2,maxOff,maxFrq,maxStaSin

  USE f_ikf
  USE s_opnfil
  USE s_opnerr
  USE s_neqalloc
  USE s_exitrc
  USE s_getrcv
  USE s_stripdir
  IMPLICIT NONE

! Dummy Arguments
! ---------------
  TYPE (t_neq)                   :: neq
  CHARACTER(LEN=fileNameLength)  :: filinp

! Local Variables
! ---------------
  TYPE(t_sinex_v2)                   :: sinex_v2
  INTEGER(i4b)                       :: ios
  INTEGER(i4b)                       :: nObs,nParms
  INTEGER(i4b)                       :: ii
  INTEGER(i4b)                       :: jj
  INTEGER(i4b)                       :: iDum
  INTEGER(i4b)                       :: jDum
  INTEGER(i4b)                       :: iSys,iSys1,iSys2
  INTEGER(i4b)                       :: iSyst
  INTEGER(i4b)                       :: iSize, lenKey
  INTEGER(i4b)                       :: nFrq,iClass
  INTEGER(i4b), DIMENSION(MAXFRQ)    :: iCode,iwlFac
  INTEGER(i4b)                       :: nRead
  CHARACTER(LEN=3)                   :: advFlg

! Local Parameters
! ----------------
  ! maximum neq version nr supported
  INTEGER(i4b), PARAMETER            :: mxneqver=8

! Open the output file
! --------------------
  CALL opnfil(lfnres,filinp,'OLD','FORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnerr,lfnres,ios,filinp,'asciineq')

! Read the version of NEQ file
! -----------------------------
  READ(lfnres,'(I3)') neq%version

  IF (neq%version > mxneqver) THEN
    WRITE(lfnerr,                                                           &
          "(/,' *** SR ASCIINEQ: NEQ format not supported',                 &
          & /,'                  Maximum format version supported:',I6,     &
          & /,'                  NEQ format version found        :',I6,/)") &
          mxneqver,neq%version
    CALL EXITRC(2)
  ENDIF



! Read the header
! ----------------
  IF (neq%version <= 2) THEN
    READ(lfnres,'( 2(A132,/), 2(A32,/), 2(I22,1X),/, E22.15,1X,I22,/, '//&
                ' 7(I22,1X,I22,/) )' )&
                    neq%misc%title(1),  neq%misc%title(2),  &
                    neq%misc%orbFil(1), neq%misc%orbFil(2), &
                    nObs,               neq%misc%npar,      &
                    neq%misc%lTPl,      nParms,             &
                    neq%misc%nftot,     neq%misc%nsmpnq,    &
                    neq%misc%ielvnq,    neq%misc%itropo,    &
                    neq%misc%iextra,    neq%misc%itrmap,    &
                    neq%misc%itrgrd,    neq%misc%nanoff
  ELSE
    READ(lfnres,'( 2(A132,/), 2(A32,/), E22.15,1X,I22,/, E22.15,1X,E22.15,/,'//&
                 ' 4(I22,1X,I22,/) )' ) &
                    neq%misc%title(1),  neq%misc%title(2),  &
                    neq%misc%orbFil(1), neq%misc%orbFil(2), &
                    neq%misc%nobs,      neq%misc%npar,      &
                    neq%misc%lTPl,      neq%misc%nparms,    &
                    neq%misc%nftot,     neq%misc%nsmpnq,    &
                    neq%misc%ielvnq,    neq%misc%itropo,    &
                    neq%misc%iextra,    neq%misc%itrmap,    &
                    neq%misc%itrgrd,    neq%misc%nanoff
  ENDIF

  IF (neq%version > 1) THEN
    READ(lfnres,'( 3(A16,/) )' ) &
                  neq%misc%datum, neq%misc%nutmod, neq%misc%submod
  ELSE
    neq%misc%datum  = ' '
    neq%misc%nutmod = ' '
    neq%misc%submod = ' '
  ENDIF

  IF (neq%version <= 2) THEN
    IF (nObs == -99) THEN
      READ(lfnres, '( 2E22.15 )') neq%misc%nobs,neq%misc%nparms
    ELSE
      neq%misc%nobs   = DBLE(nObs)
      neq%misc%nparms = DBLE(nParms)
    ENDIF
  ENDIF

  IF (neq%misc%nanoff > maxOff) THEN
    WRITE(lfnerr,'(/,A,I6,/)') &
          ' *** SR ASCIINEQ: nanoff too large:', neq%misc%nanoff
    CALL exitrc(2)
  END IF

  DO ii = 1, neq%misc%nanoff
    IF (neq%misc%nsaoff(ii) > maxSat) THEN
      WRITE(lfnerr,'(/,A,I6,/)') &
            ' *** SR ASCIINEQ: nsaoff too large:', neq%misc%nsaoff(ii)
      CALL exitrc(2)
    END IF
    READ(lfnres,'( I22,/, 99(10X,I22,/) )')   &
                    neq%misc%nsaoff(ii),       &
                    (neq%misc%satoff(jj,ii), jj=1,neq%misc%nsaoff(ii))
  END DO

! Information needed for SINEX format
! -----------------------------------
  READ(lfnres,*) neq%misc%nstat_sinex

  IF (neq%misc%nstat_sinex > maxStaSin) THEN
    WRITE(lfnerr,'(/,A,I6,/)') &
          ' *** SR ASCIINEQ: nstat too large:', neq%misc%nstat_sinex
    CALL exitrc(2)
  END IF

  IF (neq%version <= 2) THEN
    DO ii = 1, neq%misc%nstat_sinex
      READ(lfnres, '( E22.15,1X,E22.15,/ A32,/,A16,                   &
            &          3(E22.15,1X,E22.15,/), I22, 3(E22.15,1X),/,    &
            &          I22,/,A,/,A,/ )' ) &
                      neq%misc%sinex(ii)%timint%t(1),                 &
                      neq%misc%sinex(ii)%timint%t(2),                 &
                      sinex_v2%phasecc,                               &
                      neq%misc%sinex(ii)%stname,                      &
                      (sinex_v2%antPhs(1,jj), jj=1,maxFrq), &
                      (sinex_v2%antPhs(2,jj), jj=1,maxFrq), &
                      (sinex_v2%antPhs(3,jj), jj=1,maxFrq), &
                      sinex_v2%antfrq,                                &
                      neq%misc%sinex(ii)%antEcc(1),                   &
                      neq%misc%sinex(ii)%antEcc(2),                   &
                      neq%misc%sinex(ii)%antEcc(3),                   &
                      neq%misc%sinex(ii)%antNum,                      &
                      neq%misc%sinex(ii)%antSta,                      &
                      neq%misc%sinex(ii)%antRec

      ! Convert to latest version of SINEX record
      CALL stripdir(sinex_v2%phasecc)
      neq%misc%sinex(ii)%antpcv(:)%atxStr  = ''
      neq%misc%sinex(ii)%antpcv(:)%nFrq    = 0

      ! Check the tracking capabilities of the receiver
      CALL getrcv(neq%misc%sinex(ii)%antrec,nFrq,iCode,iwlFac,iClass,iSyst)

      DO iSys = 0,maxsys-1 ! GPS, GLONASS, Galileo., SBAS
                    ! Should never be bigger than maxsys in $I/M_GOBAL.f90

        IF (iSys == 1 .AND. (iSyst == 0 .OR. iSyst == 2)) CYCLE ! no GLONASS
        IF (iSys == 2 .AND. (iSyst == 0 .OR. iSyst == 1)) CYCLE ! no Galileo
        IF (iSys == 3) CYCLE ! no special SBAS at the moment

        neq%misc%sinex(ii)%antpcv(iSys)%adopted = 1
        neq%misc%sinex(ii)%antpcv(iSys)%atxStr  = TRIM(sinex_v2%phasecc)
        neq%misc%sinex(ii)%antpcv(iSys)%nFrq    = sinex_v2%antfrq
        neq%misc%sinex(ii)%antpcv(iSys)%antphs  = sinex_v2%antphs
        IF (neq%misc%sinex(ii)%antnum == 0 .OR. &
            neq%misc%sinex(ii)%antnum == 999999) THEN
          neq%misc%sinex(ii)%antpcv(iSys)%individ = 0
        ELSE
          neq%misc%sinex(ii)%antpcv(iSys)%individ = 1
        ENDIF
      ENDDO
    END DO
  ELSE
    DO ii = 1, neq%misc%nstat_sinex
      READ(lfnres, '( E22.15,1X,E22.15,/ A, /, 3(E22.15,1X),/, ' //   &
                   '  I22,2I11,/,A,/,A,/ )' )                         &
                      neq%misc%sinex(ii)%timint%t(1),                 &
                      neq%misc%sinex(ii)%timint%t(2),                 &
                      neq%misc%sinex(ii)%stname,                      &
                      neq%misc%sinex(ii)%antEcc(1),                   &
                      neq%misc%sinex(ii)%antEcc(2),                   &
                      neq%misc%sinex(ii)%antEcc(3),                   &
                      neq%misc%sinex(ii)%antNum, iSys1,iSys2,         &
                      neq%misc%sinex(ii)%antSta,                      &
                      neq%misc%sinex(ii)%antRec

      DO iSys = iSys1,iSys2
        READ(lfnres,'(2X,A)') neq%misc%sinex(ii)%antpcv(iSys)%atxStr
        IF (LEN_TRIM(neq%misc%sinex(ii)%antpcv(iSys)%atxStr) == 0) CYCLE

        READ(lfnres,'(3(E22.15,1X,E22.15,/), I22,1X,2I11,/ )')                     &
             (neq%misc%sinex(ii)%antpcv(iSys)%antPhs(1,jj), jj=1,maxFrq), &
             (neq%misc%sinex(ii)%antpcv(iSys)%antPhs(2,jj), jj=1,maxFrq), &
             (neq%misc%sinex(ii)%antpcv(iSys)%antPhs(3,jj), jj=1,maxFrq), &
             neq%misc%sinex(ii)%antpcv(iSys)%nFrq,                        &
             neq%misc%sinex(ii)%antpcv(iSys)%adopted,                     &
             neq%misc%sinex(ii)%antpcv(iSys)%individ
      ENDDO
    ENDDO
  ENDIF


! Keywords of for the scaling factors of the Vienna grid files
! ------------------------------------------------------------
  IF (neq%version >= 6) THEN
    READ(lfnres,'(2I11)') iSize,lenKey
    IF ( LEN(neq%misc%grdNeq(1)) < lenKey ) THEN
      WRITE(LFNERR,'(/,A,2(/,18X,A),/)')                            &
      ' *** SR ASCIINEQ: Scaling factors of the Vienna grid files.',&
      'The length of the string for the keywords is shorter in',    &
      '"${I}/D_NEQ.f90" than in "${I}/D_GRID.f90".'
      CALL EXITRC(2)
    ENDIF
    IF ( SIZE(neq%misc%grdNeq) < iSize ) THEN
      WRITE(LFNERR,'(/,A,2(/,18X,A),2(/,18X,A,I6),/)')              &
      ' *** SR ASCIINEQ: Scaling factors of the Vienna grid files.',&
      'The records for the keywords is too small to read the',      &
      'values in the NEQ file:',                                    &
      'Size in the NEQ file:                        ', iSize,       &
      'Size of neq%misc%grdNeq in "${I}/D_NEQ.f90": ', SIZE(neq%misc%grdNeq)
      CALL EXITRC(2)
    ENDIF
    DO ii = 1,iSize
      READ(lfnres,'(A)') neq%misc%grdNeq(ii)
    ENDDO
  ENDIF


! Sat-spec obstypes
! -----------------
  IF (neq%version >= 8) THEN
    READ(lfnres,'(I5)') neq%misc%nobst
    DO ii = 1,neq%misc%nobst
!      READ(lfnres,'(I3,4(1X,A3),2(1X,E22.15))') &
!      neq%misc%obst(ii)%sat,(neq%misc%obst(ii)%obstyp(jj),jj=1,4), &
!      (neq%misc%obst(ii)%timint%t(jj),jj=1,2)
      READ(lfnres,'(4(1X,A3))') (neq%misc%obst(ii)%obstyp(jj),jj=1,4)
    ENDDO
  ENDIF


  CALL neqalloc(neq,neq%misc%npar)

! Parameter Description
! ---------------------
  IF (maxLcq /= 7) THEN
    WRITE(lfnerr,'(/,A,/)') ' *** SR ASCIINEQ: maxLcq too large'
    CALL exitrc(2)
  END IF
  DO ii = 1, neq%misc%npar
    neq%par(ii) = readPar(lfnres,2,neq%version)
  END DO

! ANOR Matrix
! -----------
  nRead = 0
  DO ii = 1, neq%misc%npar
    DO jj = ii, neq%misc%npar

      nRead = nRead + 1

      IF (nRead == neq%misc%npar * (neq%misc%npar + 1) / 2 ) THEN
        advFlg = 'YES'
      ELSE
        advFlg = 'NO'
      END IF

      IF ( MOD(nRead,3) == 1 ) THEN
        READ(lfnres, '(2(1X,I5), 1X,E21.15)', ADVANCE=advFlg) &
              iDum, jDum, neq%aNor(ikf(ii,jj))
      ELSE IF ( MOD(nRead,3) == 2 ) THEN
        READ(lfnres, '(1X,E21.15)', ADVANCE=advFlg) neq%aNor(ikf(ii,jj))
      ELSE
        READ(lfnres, '(1X,E21.15)') neq%aNor(ikf(ii,jj))
      END IF

    END DO
  END DO

! BNOR Matrix
! -----------
  DO ii = 1, neq%misc%npar
    READ(lfnres,'(1X,I5,1X,E21.15)') iDum, neq%bNor(ii)
  END DO

  CLOSE(lfnres)

END SUBROUTINE asciineq

END MODULE
