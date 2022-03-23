MODULE s_NQRDHEAD
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE nqrdhead(lfn,neq)

! -------------------------------------------------------------------------
! Purpose:    Read the header part of the NEQ structure from file
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
!
! Changes:    21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             31-Mar-2003 HU: Exit if neq version not supported
!             22-May-2003 RD: Use maxsat from m_maxdim (instead of p_addneq)
!             08-Sep-2003 HU: New NEQ version
!             24-May-2005 SS/EB: Truncate station names if indicated
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             08-Feb-2007 RD: misc%nObs/nParms i4b->r8b
!             23-Jan-2008 RD: New NEQ-version (with name = chr*20)
!             26-Jun-2008 RD: System-specific PCV in SINEX record
!             30-Apr-2009 SL: mxneqver set to 5
!             04-May-2009 RD: Scaling of loading models added, neq%version=6
!             26-Oct-2010 SL: use m_bern with ONLY, use undef_i
!             15-Nov-2010 RD: New NEQ version=7: D_PAR omega is written anytime
!             14-Jul-2011 LP: Sat-spec. obstypes; neq%version set to 8
!             21-Nov-2012 RD: Do not write SINEX-PCV as a record
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, staNameLength, lfnErr
  USE m_global, ONLY: maxsys
  USE m_maxdim, ONLY: maxsat
  USE d_neq,    ONLY: t_neq,t_sinex_v1,t_sinex_v2,maxOff,maxStaSin,maxfrq
  USE p_addneq, ONLY: opt
  USE d_stacrx, ONLY: undef_i

  USE s_exitrc
  USE s_stripdir
  USE s_getrcv
  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b)  :: lfn  ! logical file number
  TYPE(t_neq)   :: neq

! Local Parameters
! ----------------
  ! maximum neq version nr supported
  INTEGER(i4b), PARAMETER            :: mxneqver=8

! Local Variables
! ---------------
  INTEGER(i4b)                       :: ii,jj
  INTEGER(i4b)                       :: iSys,iSys1,iSys2
  INTEGER(i4b)                       :: iSyst
  INTEGER(i4b)                       :: nFrq,iClass
  INTEGER(i4b)                       :: iSize, lenKey
  INTEGER(i4b), DIMENSION(MAXFRQ)    :: iCode,iwlFac
  INTEGER(i4b)                       :: nObs,nParms

! Local types
! -----------
  TYPE(t_sinex_v1),DIMENSION(maxStaSin) :: sinex_v1   ! NEQ Version 1
  TYPE(t_sinex_v2),DIMENSION(maxStaSin) :: sinex_v2   ! NEQ Version 2

! Version of the neq-file
! -----------------------
  READ(lfn) neq%version

  IF (neq%version > mxneqver) THEN
    WRITE(lfnerr,                                                           &
          "(/,' *** SR NQRDHEAD: NEQ format not supported',                 &
          & /,'                  Maximum format version supported:',I6,     &
          & /,'                  NEQ format version found        :',I6,/)") &
          mxneqver,neq%version
    CALL EXITRC(2)
  ENDIF

! Miscelaneous information
! ------------------------
  IF (neq%version <= 3) THEN
    READ(lfn) neq%misc%title  , &
              neq%misc%orbFil , &
              nObs            , &
              neq%misc%npar   , &
              neq%misc%lTPl   , &
              nParms          , &
              neq%misc%nftot  , &
              neq%misc%nsmpnq , &
              neq%misc%ielvnq , &
              neq%misc%itropo , &
              neq%misc%iextra , &
              neq%misc%itrmap , &
              neq%misc%itrgrd
  ELSE ! nobs and nparams are real instead of integer
    READ(lfn) neq%misc%title  , &
              neq%misc%orbFil , &
              neq%misc%nobs   , &
              neq%misc%npar   , &
              neq%misc%lTPl   , &
              neq%misc%nparms , &
              neq%misc%nftot  , &
              neq%misc%nsmpnq , &
              neq%misc%ielvnq , &
              neq%misc%itropo , &
              neq%misc%iextra , &
              neq%misc%itrmap , &
              neq%misc%itrgrd
  ENDIF

! NEQ version 2
! -------------
  IF (neq%version > 1) THEN
    READ(lfn) neq%misc%datum  , &
              neq%misc%nutmod , &
              neq%misc%submod

  ELSE
    neq%misc%datum  = ' '
    neq%misc%nutmod = ' '
    neq%misc%submod = ' '
  ENDIF

! NEQ version 3
! -------------
  IF (neq%version > 2) THEN
    READ(lfn) neq%misc%gravFil
  ELSE
    neq%misc%gravFil = ''
  ENDIF

! nObs, nParms are real instead of integer
! ----------------------------------------
  IF (neq%version <= 3) THEN
    IF ( nObs == -99) THEN
      READ(lfn) neq%misc%nobs,neq%misc%nparms
    ELSE
      neq%misc%nobs   = nObs
      neq%misc%nparms = nParms
    ENDIF
  ENDIF

! Satellite antenna offset arrays
! -------------------------------
  READ(lfn) neq%misc%nanoff

  IF ( neq%misc%nanoff > maxOff ) THEN
    WRITE(lfnerr,*) ' *** SR NQRDHEAD: maxOff < ', neq%misc%nanoff
    CALL exitrc(2)
  END IF

  DO ii = 1, neq%misc%nanoff
    READ(lfn) neq%misc%nsaoff(ii)
    IF ( neq%misc%nsaoff(ii) > maxSat ) THEN
      WRITE(lfnerr,*) ' *** SR NQRDHEAD: maxSat < ', neq%misc%nsaoff(ii)
      CALL exitrc(2)
    END IF
  END DO

  READ(lfn) ( (neq%misc%satoff(jj,ii),jj=1,neq%misc%nsaoff(ii)), &
              ii=1,neq%misc%nanoff )

! Information needed for SINEX format
! -----------------------------------
  READ(lfn) neq%misc%nstat_sinex

  IF ( neq%misc%nstat_sinex > maxStaSin ) THEN
    WRITE(lfnerr,*) ' *** SR NQRDHEAD: maxStaSin < ', neq%misc%nstat_sinex
    CALL exitrc(2)
  END IF

  IF (neq%version == 1) THEN
    READ(lfn) (sinex_v1(ii),ii=1,neq%misc%nstat_sinex)
    DO ii=1,neq%misc%nstat_sinex
      neq%misc%sinex(ii)%timint  = sinex_v1(ii)%timint
      neq%misc%sinex(ii)%stname  = sinex_v1(ii)%stname
      neq%misc%sinex(ii)%antecc  = sinex_v1(ii)%antecc
      neq%misc%sinex(ii)%antnum  = sinex_v1(ii)%antnum
      neq%misc%sinex(ii)%antsta  = sinex_v1(ii)%antsta//'????'
      neq%misc%sinex(ii)%antrec  = sinex_v1(ii)%antrec//'????'

      sinex_v2(ii)%phasecc       = TRIM(sinex_v1(ii)%phasecc)
      sinex_v2(ii)%antphs(:,:)   = sinex_v1(ii)%antphs(:,:)
      sinex_v2(ii)%antfrq        = sinex_v1(ii)%antfrq
    ENDDO
  ELSE IF (neq%version == 2) THEN
    DO ii=1,neq%misc%nstat_sinex
      READ(lfn) sinex_v2(ii)

      neq%misc%sinex(ii)%timint  = sinex_v2(ii)%timint
      neq%misc%sinex(ii)%stname  = sinex_v2(ii)%stname
      neq%misc%sinex(ii)%antecc  = sinex_v2(ii)%antecc
      neq%misc%sinex(ii)%antnum  = sinex_v2(ii)%antnum
      neq%misc%sinex(ii)%antsta  = sinex_v2(ii)%antsta
      neq%misc%sinex(ii)%antrec  = sinex_v2(ii)%antrec
    ENDDO
  ELSE
    DO ii=1,neq%misc%nstat_sinex
      READ(lfn) neq%misc%sinex(ii)%timint,  neq%misc%sinex(ii)%stname,  &
                neq%misc%sinex(ii)%antecc,  neq%misc%sinex(ii)%antnum,  &
                neq%misc%sinex(ii)%antsta,  neq%misc%sinex(ii)%antrec,  &
                iSys1,iSys2
      IF ( iSys1 /= -99 ) THEN
        DO iSys = iSys1,iSys2
          READ(lfn) neq%misc%sinex(ii)%antpcv(iSys)
        ENDDO
      ELSE
        READ(lfn) iSys1,iSys2
        DO iSys = iSys1,iSys2
          READ(lfn) neq%misc%sinex(ii)%antpcv(iSys)%nFrq,    &
                    neq%misc%sinex(ii)%antpcv(iSys)%antphs,  &
                    neq%misc%sinex(ii)%antpcv(iSys)%adopted, &
                    neq%misc%sinex(ii)%antpcv(iSys)%individ, &
                    neq%misc%sinex(ii)%antpcv(iSys)%atxStr
        ENDDO
      ENDIF
      DO iSys = iSys2+1,maxsys-1
        neq%misc%sinex(ii)%antpcv(iSys)%atxStr = ''
        neq%misc%sinex(ii)%antpcv(iSys)%nFrq   = 0
      ENDDO
    ENDDO
  ENDIF

! Grid keywords for Vienna grid files
! -----------------------------------
  IF (neq%version >= 6) THEN
    READ(lfn) iSize,lenKey
    IF ( LEN(neq%misc%grdNeq(1)) < lenKey ) THEN
      WRITE(LFNERR,'(/,A,2(/,18X,A),/)')                            &
      ' *** SR NQRDHEAD: Scaling factors of the Vienna grid files.',&
      'The length of the string for the keywords is shorter in',    &
      '"${I}/D_NEQ.f90" than in "${I}/D_GRID.f90".'
      CALL EXITRC(2)
    ENDIF
    IF ( SIZE(neq%misc%grdNeq) < iSize ) THEN
      WRITE(LFNERR,'(/,A,2(/,18X,A),2(/,18X,A,I6),/)')              &
      ' *** SR NQRDHEAD: Scaling factors of the Vienna grid files.',&
      'The records for the keywords is too small to read the',      &
      'values in the NEQ file:',                                    &
      'Size in the NEQ file:                        ', iSize,       &
      'Size of neq%misc%grdNeq in "${I}/D_NEQ.f90": ', SIZE(neq%misc%grdNeq)
      CALL EXITRC(2)
    ENDIF
    READ(lfn) neq%misc%grdNeq(1:iSize)
  ENDIF


! Reconstruct the antenna record for the old versions
! ---------------------------------------------------
  IF (neq%version <= 3) THEN
    DO ii=1,neq%misc%nstat_sinex
      CALL stripdir(sinex_v2(ii)%phasecc)
      neq%misc%sinex(ii)%antpcv(:)%atxStr = ''
      neq%misc%sinex(ii)%antpcv(:)%nFrq   = 0

      ! Check the tracking capabilities of the receiver
      CALL getrcv(neq%misc%sinex(ii)%antrec,nFrq,iCode,iwlFac,iClass,iSyst)

      DO iSys = 0,maxsys-1 ! GPS, GLONASS, Galileo., SBAS
                    ! Should never be bigger than maxsys in $I/M_GOBAL.f90

        IF (iSys == 1 .AND. (iSyst == 0 .OR. iSyst == 2)) CYCLE ! no GLONASS
        IF (iSys == 2 .AND. (iSyst == 0 .OR. iSyst == 1)) CYCLE ! no Galileo
        IF (iSys == 3) CYCLE ! no special SBAS at the moment

        neq%misc%sinex(ii)%antpcv(iSys)%adopted = 1
        neq%misc%sinex(ii)%antpcv(iSys)%atxStr  = TRIM(sinex_v2(ii)%phasecc)
        neq%misc%sinex(ii)%antpcv(iSys)%antphs  = sinex_v2(ii)%antphs
        neq%misc%sinex(ii)%antpcv(iSys)%nFrq    = sinex_v2(ii)%antfrq
        IF (neq%misc%sinex(ii)%antnum == 0 .OR. &
            neq%misc%sinex(ii)%antnum == undef_i) THEN
          neq%misc%sinex(ii)%antpcv(iSys)%individ = 0
        ELSE
          neq%misc%sinex(ii)%antpcv(iSys)%individ = 1
        ENDIF
      ENDDO
    ENDDO
  ENDIF


! Truncate station names if indicated
! -----------------------------------
  IF (opt%noabc < staNameLength) THEN
    DO ii=1,neq%misc%nstat_sinex
      neq%misc%sinex(ii)%stname(opt%noabc+1:) = ' '
! Truncate 4-character station names (at request of swisstopo)
!!      IF (neq%misc%sinex(ii)%stname(7:) == ' ') &
!!        neq%misc%sinex(ii)%stname(5:) = ' '
    ENDDO
  ENDIF


! Neq version 8
! -------------
  IF (neq%version >= 8) THEN
    READ(lfn) neq%misc%nobst
    DO ii = 1, neq%misc%nobst
!      READ(lfn) neq%misc%obst(ii)%sat
      DO jj = 1,4
        READ(lfn) neq%misc%obst(ii)%obstyp(jj)
      ENDDO
!      DO jj = 1,2
!        READ(lfn) neq%misc%obst(ii)%timint%t(jj)
!      ENDDO
    ENDDO
  ENDIF

END SUBROUTINE nqrdhead

END MODULE
