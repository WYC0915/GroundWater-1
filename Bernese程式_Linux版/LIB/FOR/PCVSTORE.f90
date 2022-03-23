MODULE s_PCVSTORE
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE pcvstore(neq)

! -------------------------------------------------------------------------
! Purpose:    update
!               - receiver antenna phase center offsets and/or
!                 elevation/azimuth-dependent variations
!               - satellite antenna phase center variations
!               as estimated by ADDNEQ2
!
! Author:     R. Dach
!
! Created:    20-Mar-2008
! Last mod.:  21-Jul-2011
!
! Changes:    06-May-2009 RD: Bugfix for sat-spec. PCVs
!             24-Nov-2009 MM: iOrd initialized
!             23-Dec-2010 RD: Write GLONASS-only PCV result files
!             21-Jul-2011 LP: Read indobst (index for sat-specific obstypes); not yet used
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_time,   ONLY: t_timint
  USE m_global, ONLY: g_syssvn
  USE d_const,  ONLY: date,PI,filTitle
  USE d_satfil, ONLY: typeMWTR
  USE d_neq,    ONLY: t_neq
  USE d_phaecc, ONLY: t_phasfil, alcantbu, wtphafil, recant, satant, updmodel
  USE s_alcerr
  USE s_gtflna
  USE s_gtsensor
  USE s_stripdir
  USE s_svn2prn
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  TYPE(t_neq)                       :: neq

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER            :: srName = 'pcvstore'


! Local Variables
! ---------------
  TYPE(t_phasfil), DIMENSION(:), POINTER :: satUpd
  TYPE(t_phasfil), DIMENSION(:), POINTER :: recUpd
  TYPE(t_timint)                         :: timint

  CHARACTER(LEN=fileNameLength)          :: filphc,filhlp
  CHARACTER(LEN=60)                      :: filInfo
  CHARACTER(LEN=Stanam2length)           :: name
  CHARACTER(LEN=10)                      :: model
  CHARACTER(LEN=4)                       :: svnnr

  INTEGER(i4b), DIMENSION(neq%misc%nPar) :: idxpar
  INTEGER(i4b)                           :: numb
  INTEGER(i4b)                           :: iSap,nSap
  INTEGER(i4b)                           :: iSat,jSat,kSat,nSat
  INTEGER(i4b)                           :: prnnr
  INTEGER(i4b)                           :: nRec
  INTEGER(i4b)                           :: iAnt,jAnt
  INTEGER(i4b)                           :: nAnt
  INTEGER(i4b)                           :: iSys,nSys
  INTEGER(i4b)                           :: iFr1,iFr2
  INTEGER(i4b)                           :: iFrq,nFrq
  INTEGER(i4b)                           :: iCal
  INTEGER(i4b)                           :: iZen, nZen
  INTEGER(i4b)                           :: iAzi, nAzi, mAzi
  INTEGER(i4b)                           :: iMod
  INTEGER(i4b)                           :: iPar
  INTEGER(i4b)                           :: iOrd
  INTEGER(i4b)                           :: ircPhc,ircInp
  INTEGER(i4b)                           :: iac,irc
  INTEGER(i4b)                           :: indobst

  REAL(r8b)                              :: epo
  REAL(r8b)                              :: nadmax

! Init variables
! --------------
  CALL gtflna(0,'PHASRSG',filphc,ircPhc)
  IF (ircPhc /= 0) RETURN

  CALL gtflna(0,'PHASECC',filhlp,ircInp)
  IF (ircInp /= 0) ircInp = 1

  NULLIFY(recUpd)
  NULLIFY(satUpd)
  iOrd = 0

! Count number of antenna related parameters
! ------------------------------------------
  nRec = 0
  nSat = 0
  nFrq = 0
  nSys = 0
  DO iPar = 1, neq%misc%nPar

    ! New receiver antenna
    IF (neq%par(iPar)%locq(1) ==  5 .OR. neq%par(iPar)%locq(1) == 18) THEN

      nFrq = min0 ( max0( nFrq, MOD(neq%par(iPar)%locq(4) , 100) ), 2)
      nSys =        max0( nSys, MOD(neq%par(iPar)%locq(5) , 100) )

      IF (iPar == 1) THEN
        nRec = nRec + 1
      ELSE IF (neq%par(iPar-1)%locq(1) /= 5 .AND. neq%par(iPar-1)%locq(1) /= 18) THEN
        nRec = nRec + 1
      ELSE IF (neq%par(iPar-1)%name    /= neq%par(iPar)%name    .OR. &
               neq%par(iPar-1)%locq(2) /= neq%par(iPar)%locq(2) .OR. &
               neq%par(iPar-1)%locq(3) /= neq%par(iPar)%locq(3) ) THEN
        nRec = nRec + 1
      ENDIF
    ENDIF

    ! New satellite antenna
    IF (neq%par(iPar)%locq(1) == 25) THEN
      IF (iPar == 1) THEN
        nSat = nSat + 1
      ELSE IF (neq%par(iPar-1)%locq(1) /= 25) THEN
        IF ( neq%par(iPar)%locq(2) == 0 ) THEN
          nSat = nSat + 1
        ELSE
          nSat = nSat + neq%misc%nsaoff(neq%par(iPar)%locq(2))
        ENDIF
      ELSE IF (neq%par(iPar-1)%locq(2) /= neq%par(iPar)%locq(2) ) THEN
        IF ( neq%par(iPar)%locq(2) == 0 ) THEN
          nSat = nSat + 1
        ELSE
          nSat = nSat + neq%misc%nsaoff(neq%par(iPar)%locq(2))
        ENDIF
      ELSE IF (neq%par(iPar-1)%locq(3) /= neq%par(iPar)%locq(3) ) THEN
        nSat = nSat + 1
      ENDIF
    ENDIF

  ENDDO

! Prepare the update list for receiver antennas
! ---------------------------------------------
  idxPar = 0

  ALLOCATE(recUpd(nRec),stat=iac)
  CALL alcerr(iac,'recUpd',(/nRec/),srName)

  ALLOCATE(satUpd(nSat),stat=iac)
  CALL alcerr(iac,'satUpd',(/nSat/),srName)

  CALL alcantbu(nRec,nSys,recUpd)
  CALL alcantbu(nSat,0,satUpd)

  nAnt = 0
  nSat = 0

  ! List of receiver antenna offsets
  DO iPar = 1, neq%misc%nPar

    ! New receiver antenna
    IF (neq%par(iPar)%locq(1) ==  5 .OR. neq%par(iPar)%locq(1) == 18) THEN

      idxPar(iPar) = 0
      DO jAnt = 1,nAnt
        IF (recUpd(jAnt)%name == neq%par(ipar)%name .AND. &
            recUpd(jAnt)%numb == neq%par(iPar)%locq(3)) idxPar(iPar) = jAnt
      ENDDO

      IF (idxPar(iPar) == 0) THEN
        nAnt = nAnt + 1
        recUpd(nAnt)%name = neq%par(ipar)%name
        recUpd(nAnt)%numb = neq%par(iPar)%locq(3)
        idxPar(iPar) = nAnt

        IF (recUpd(nAnt)%name(17:20) == '????') recUpd(nAnt)%name(17:20) = '    '

        DO iSys = 0,nSys
          recUpd(nAnt)%sys(isys)%typ=0
          recUpd(nAnt)%sys(isys)%nfreq=nFrq
          recUpd(nAnt)%sys(iSys)%resolu(:) = (/0,90,360,90/)
          ALLOCATE(recUpd(nAnt)%sys(iSys)%freq(nFrq),stat=iac)
          CALL alcerr(iac,'recUpd%sys%freq',(/nFrq/),srName)
          DO iFrq = 1,nFrq
            recUpd(nAnt)%sys(iSys)%freq(iFrq)%freq = 0

            ALLOCATE(recUpd(nAnt)%sys(iSys)%freq(iFrq)%off(0:iOrd,3),stat=iac)
            CALL alcerr(iac,'recUpd%sys%freq%off',(/iOrd+1,3/),srName)
            recUpd(nAnt)%sys(iSys)%freq(iFrq)%off = 0d0

            ALLOCATE(recUpd(nAnt)%sys(iSys)%freq(iFrq)%fac(0:iOrd),stat=iac)
            CALL alcerr(iac,'recUpd%sys%freq%fac',(/iOrd+1/),srName)
            recUpd(nAnt)%sys(iSys)%freq(iFrq)%fac = 1d0

            NULLIFY(recUpd(nAnt)%sys(iSys)%freq(iFrq)%pat)
          ENDDO
        ENDDO
      ENDIF
    ENDIF

    ! New satellite antenna
    IF (neq%par(iPar)%locq(1) == 25) THEN

      iSys = 0
      iOrd = 0
      iSap = neq%par(iPar)%locq(2)
      IF (iSap == 0) THEN
        nSap = 1
      ELSE
        nSap = neq%misc%nsaoff(iSap)
      ENDIF
      epo  = neq%par(iPar)%time%mean

      DO iSat = 1,nSap
        IF (iSap == 0) THEN
          WRITE(svnnr,"(A1,I3)")g_syssvn(INT(neq%par(iPar)%locq(3)/100)),neq%par(iPar)%locq(3)
          CALL svn2prn(4,svnnr,neq%par(iPar)%time%mean,prnnr,timint,irc)
          CALL gtsensor(prnnr,epo,typeMWTR,name,numb)
        ELSE
          CALL gtsensor(neq%misc%satoff(iSat,iSap),epo,typeMWTR,name,numb)
        ENDIF

        kSat = 0
        DO jSat = 1,nSat
          IF (name == satUpd(jSat)%name .AND. &
              numb == satUpd(jSat)%numb) kSat = jSat
        ENDDO
        IF (kSat == 0) THEN
          nSat = nSat + 1
          satUpd(nSat)%name=name
          satUpd(nSat)%numb=numb
          satUpd(nSat)%sys(isys)%typ=0
          satUpd(nSat)%sys(isys)%nfreq=2
          satUpd(nSat)%sys(iSys)%resolu(:) = (/0,17,360,17/)
          ALLOCATE(satUpd(nSat)%sys(iSys)%freq(2),stat=iac)
          CALL alcerr(iac,'satUpd%sys%freq',(/2/),srName)
          DO iFrq = 1,2
            ALLOCATE(satUpd(nSat)%sys(iSys)%freq(iFrq)%off(0:iOrd,3),stat=iac)
            CALL alcerr(iac,'satUpd%sys%freq%off',(/iOrd+1,3/),srName)
            satUpd(nSat)%sys(iSys)%freq(iFrq)%off = 0d0

            ALLOCATE(satUpd(nSat)%sys(iSys)%freq(iFrq)%fac(0:iOrd),stat=iac)
            CALL alcerr(iac,'satUpd%sys%freq%fac',(/iOrd+1/),srName)
            satUpd(nSat)%sys(iSys)%freq(iFrq)%fac = 1d0

            satUpd(nSat)%sys(iSys)%freq(iFrq)%freq = 0
            NULLIFY(satUpd(nSat)%sys(iSys)%freq(iFrq)%pat)
          ENDDO
        ENDIF
      ENDDO
    ENDIF
  ENDDO

! Loop the parameter list
! -----------------------
  DO iPar = 1,neq%misc%nPar

    ! Receiver antenna offsets
    ! ------------------------
    IF (neq%par(iPar)%locq(1) == 5) iFrq = MOD(neq%par(iPar)%locq(4) , 100)
    IF (neq%par(iPar)%locq(1) == 5  .AND. iFrq /= 4) THEN

      iFrq = MOD(neq%par(iPar)%locq(4) , 100)
      nSys = MOD(neq%par(iPar)%locq(5) , 100)
      iAnt = idxPar(iPar)

      iOrd = 0
      iCal = neq%par(iPar)%locq(4) / 100

      iFr1 = iFrq
      iFr2 = iFrq
      IF (iFrq == 3 ) THEN
        iFr1 = 1
        iFr2 = 2
      ENDIF

      DO iFrq = iFr1,iFr2

        ! corrections for all GNSS
        IF (nSys == 10) THEN
          DO iSys = 0,nSys-1
            recUpd(iAnt)%sys(iSys)%method    = 'estimated by ADDNEQ2'
            recUpd(iAnt)%sys(iSys)%date      = date

            IF (.NOT. ASSOCIATED(recUpd(iAnt)%sys(iSys)%freq(iFrq)%off)) THEN
              ALLOCATE(recUpd(iAnt)%sys(iSys)%freq(iFrq)%off(0:iOrd,3),stat=iac)
              CALL alcerr(iac,'recUpd%sys%freq%off',(/iOrd+1,3/),srName)
              recUpd(iAnt)%sys(iSys)%freq(iFrq)%off = 0d0
            ENDIF

            IF (.NOT. ASSOCIATED(recUpd(iAnt)%sys(iSys)%freq(iFrq)%fac)) THEN
              ALLOCATE(recUpd(iAnt)%sys(iSys)%freq(iFrq)%fac(0:iOrd),stat=iac)
              CALL alcerr(iac,'recUpd%sys%freq%fac',(/iOrd+1/),srName)
              recUpd(iAnt)%sys(iSys)%freq(iFrq)%fac = 1d0
            ENDIF

            recUpd(iAnt)%sys(iSys)%freq(iFrq)%freq = iFrq
            recUpd(iAnt)%sys(iSys)%freq(iFrq)%off(iOrd,iCal) = neq%xxx(iPar)*1D3 + &
                  DBLE(ircInp) * neq%par(iPar)%x0*1d3
          ENDDO

        ! corrections only for one GNSS
        ELSE
          iSys = nSys
          recUpd(iAnt)%sys(iSys)%method    = 'estimated by ADDNEQ2'
          recUpd(iAnt)%sys(iSys)%date      = date

          IF (.NOT. ASSOCIATED(recUpd(iAnt)%sys(iSys)%freq(iFrq)%off)) THEN
            ALLOCATE(recUpd(iAnt)%sys(iSys)%freq(iFrq)%off(0:iOrd,3),stat=iac)
            CALL alcerr(iac,'recUpd%sys%freq%off',(/iOrd+1,3/),srName)
            recUpd(iAnt)%sys(iSys)%freq(iFrq)%off = 0d0
          ENDIF

          IF (.NOT. ASSOCIATED(recUpd(iAnt)%sys(iSys)%freq(iFrq)%fac)) THEN
            ALLOCATE(recUpd(iAnt)%sys(iSys)%freq(iFrq)%fac(0:iOrd),stat=iac)
            CALL alcerr(iac,'recUpd%sys%freq%fac',(/iOrd+1/),srName)
            recUpd(iAnt)%sys(iSys)%freq(iFrq)%fac = 1d0
          ENDIF

          recUpd(iAnt)%sys(iSys)%freq(iFrq)%freq = iFrq
          recUpd(iAnt)%sys(iSys)%freq(iFrq)%off(iOrd,iCal) = neq%xxx(iPar)*1D3 + &
                  DBLE(ircInp) * neq%par(iPar)%x0*1d3
        ENDIF
      ENDDO
    ENDIF


    ! Receiver antenna pattern
    ! ------------------------
    IF (neq%par(iPar)%locq(1) == 18) iFrq = MOD(neq%par(iPar)%locq(4) , 100)
    IF (neq%par(iPar)%locq(1) == 18  .AND. iFrq /= 4) THEN

      iFrq = MOD(neq%par(iPar)%locq(4) , 100)
      nSys = MOD(neq%par(iPar)%locq(5) , 100)
      iAnt = idxPar(iPar)

      iOrd = 0
      iZen = neq%par(iPar)%locq(4) / 100
      iAzi = neq%par(iPar)%locq(5) / 100
      nZen = max0(2,ABS(neq%par(iPar)%locq(6)))
      nAzi = max0(2,neq%par(iPar)%locq(7))

      iFr1 = iFrq
      iFr2 = iFrq
      IF (iFrq == 3 ) THEN
        iFr1 = 1
        iFr2 = 2
      ENDIF

      iMod = 1
      IF (neq%par(iPar)%locq(6) < 0) iMod = 4

      mAzi = nAzi
      IF (mAzi == 2) mAzi = 1

      IF (iMod >= 2 .AND. iMod <= 4) THEN
        iAzi = iAZi + nAzi+1
        mAzi = nAzi*2+1
      ENDIF

      DO iFrq = iFr1,iFr2

        ! corrections for all GNSS
        IF (nSys == 10) THEN
          DO iSys = 0,nSys-1
            recUpd(iAnt)%sys(iSys)%method    = 'estimated by ADDNEQ2'
            recUpd(iAnt)%sys(iSys)%date      = date
            recUpd(iAnt)%sys(iSys)%typ       = iMod

            IF (iMod == 1) THEN
              recUpd(iAnt)%sys(iSys)%resolu(1) = iOrd
              recUpd(iAnt)%sys(iSys)%resolu(2) =  90/(nZen-1)
              recUpd(iAnt)%sys(iSys)%resolu(3) = 360/(nAzi-1)
              recUpd(iAnt)%sys(iSys)%resolu(4) =  90
            ELSE
              recUpd(iAnt)%sys(iSys)%resolu(1) = iOrd
              recUpd(iAnt)%sys(iSys)%resolu(2) = nZen
              recUpd(iAnt)%sys(iSys)%resolu(3) = nAzi
              recUpd(iAnt)%sys(iSys)%resolu(4) =   90
            ENDIF

            IF (.NOT. ASSOCIATED(recUpd(iAnt)%sys(iSys)%freq(iFrq)%pat)) THEN
              ALLOCATE(recUpd(iAnt)%sys(iSys)%freq(iFrq)%pat(0:iOrd,nZen,mAzi),stat=iac)
              CALL alcerr(iac,'recUpd%sys%freq%pat',(/iOrd+1,nZen,mAzi/),srName)
              recUpd(iAnt)%sys(iSys)%freq(iFrq)%pat = 0d0
            ENDIF

            recUpd(iAnt)%sys(iSys)%freq(iFrq)%freq = iFrq
            recUpd(iAnt)%sys(iSys)%freq(iFrq)%pat(iOrd,iZen,iAzi) = neq%xxx(iPar)*1D3 + &
                  DBLE(ircInp) * neq%par(iPar)%x0*1d3
            IF (iAzi == 1 .AND. recUpd(iAnt)%sys(iSys)%resolu(3) /= 360) &
              recUpd(iAnt)%sys(iSys)%freq(iFrq)%pat(iOrd,iZen,nAzi) = neq%xxx(iPar)*1D3 + &
                  DBLE(ircInp) * neq%par(iPar)%x0*1d3
          ENDDO

        ! corrections only for one GNSS
        ELSE
          iSys = nSys
          recUpd(iAnt)%sys(iSys)%method    = 'estimated by ADDNEQ2'
          recUpd(iAnt)%sys(iSys)%date      = date
          recUpd(iAnt)%sys(iSys)%typ       = iMod

          recUpd(iAnt)%sys(iSys)%resolu(1) = iOrd
          recUpd(iAnt)%sys(iSys)%resolu(2) =  90/(nZen-1)
          recUpd(iAnt)%sys(iSys)%resolu(3) = 360/(nAzi-1)
          recUpd(iAnt)%sys(iSys)%resolu(4) =  90

          IF (.NOT. ASSOCIATED(recUpd(iAnt)%sys(iSys)%freq(iFrq)%pat)) THEN
            ALLOCATE(recUpd(iAnt)%sys(iSys)%freq(iFrq)%pat(0:iOrd,nZen,mAzi),stat=iac)
            CALL alcerr(iac,'recUpd%sys%freq%pat',(/iOrd+1,nZen,mAzi/),srName)
            recUpd(iAnt)%sys(iSys)%freq(iFrq)%pat = 0d0
          ENDIF

          recUpd(iAnt)%sys(iSys)%freq(iFrq)%freq = iFrq
          recUpd(iAnt)%sys(iSys)%freq(iFrq)%pat(iOrd,iZen,iAzi) = neq%xxx(iPar)*1D3 + &
                  DBLE(ircInp) * neq%par(iPar)%x0*1d3
          IF (iAzi == 1 .AND. recUpd(iAnt)%sys(iSys)%resolu(3) /= 360) &
            recUpd(iAnt)%sys(iSys)%freq(iFrq)%pat(iOrd,iZen,nAzi) = neq%xxx(iPar)*1D3 + &
                  DBLE(ircInp) * neq%par(iPar)%x0*1d3
        ENDIF
      ENDDO
    ENDIF

    ! Satellite antenna pattern
    ! -------------------------
    IF (neq%par(iPar)%locq(1) == 25) THEN

!      READ(neq%par(ipar)%name,'(I1,6X,I3)') ifrq,iZen
      READ(neq%par(ipar)%name,'(I1,6X,I3,I5)') ifrq,iZen,indobst
      nadmax = iZen / 10d0

      iOrd   = 0
      iZen   = neq%par(iPar)%locq(4)
      iAzi   = neq%par(iPar)%locq(5)
      nZen   = max0(2,neq%par(iPar)%locq(6))
      nAzi   = max0(2,neq%par(iPar)%locq(7))

      iMod = 1
      iSys = 0

      mAzi = nAzi
      IF (mAzi == 2) mAzi = 1

      iSap = neq%par(iPar)%locq(2)
      IF (iSap == 0) THEN
        nSap = 1
      ELSE
        nSap = neq%misc%nsaoff(iSap)
      ENDIF
      epo  = neq%par(iPar)%time%mean

      DO iSat = 1,nSap

        IF (iSap == 0) THEN
          WRITE(svnnr,"(A1,I3)")g_syssvn(INT(neq%par(iPar)%locq(3)/100)),neq%par(iPar)%locq(3)
          CALL svn2prn(4,svnnr,neq%par(iPar)%time%mean,prnnr,timint,irc)
          CALL gtsensor(prnnr,epo,typeMWTR,name,numb)
        ELSE
          CALL gtsensor(neq%misc%satoff(iSat,iSap),epo,typeMWTR,name,numb)
        ENDIF

        DO jSat = 1,nSat
          IF (satUpd(jSat)%name == name .AND. satUpd(jSat)%numb == numb) THEN

            satUpd(jSat)%sys(isys)%nfreq     = 2
            satUpd(jSat)%sys(iSys)%method    = 'estimated by ADDNEQ2'
            satUpd(jSat)%sys(iSys)%date      = date
            satUpd(jSat)%sys(iSys)%typ       = iMod

            satUpd(jSat)%sys(iSys)%resolu(1) = iOrd
            satUpd(jSat)%sys(iSys)%resolu(2) = nadmax/(nZen-1)
            satUpd(jSat)%sys(iSys)%resolu(3) = 360/(nAzi-1)
            satUpd(jSat)%sys(iSys)%resolu(4) = nadmax

            DO iFrq = 1,2
              IF (.NOT. ASSOCIATED(satUpd(jSat)%sys(iSys)%freq(iFrq)%pat)) THEN
                ALLOCATE(satUpd(jSat)%sys(iSys)%freq(iFrq)%pat(0:iOrd,nZen,mAzi),stat=iac)
                CALL alcerr(iac,'satUpd%sys%freq%pat',(/iOrd+1,nZen,mAzi/),srName)
                satUpd(jSat)%sys(iSys)%freq(iFrq)%pat = 0d0
              ENDIF

              satUpd(jSat)%sys(iSys)%freq(iFrq)%freq = iFrq
              satUpd(jSat)%sys(iSys)%freq(iFrq)%pat(iOrd,iZen,iAzi) = neq%xxx(iPar)*1D3 + &
                  DBLE(ircInp) * neq%par(iPar)%x0*1d3
              IF (iZen == 1 .AND. satUpd(jSat)%sys(iSys)%resolu(3) /= 360) &
                satUpd(jSat)%sys(iSys)%freq(iFrq)%pat(iOrd,iZen,nAzi) = neq%xxx(iPar)*1D3 + &
                  DBLE(ircInp) * neq%par(iPar)%x0*1d3
            ENDDO ! Loop Frequencies
          ENDIF
        ENDDO ! Loop satellites
      ENDDO
    ENDIF
  ENDDO ! End of parameter loop

! Update the estimated antenna model components to full models
! ------------------------------------------------------------
  IF (ircInp == 0) THEN
    CALL updmodel(recUpd,recAnt,filphc)
    CALL updmodel(satUpd,satAnt,filphc)
  ENDIF

! Write updated phase center file(s)
! ---------------------------------
  CALL gtsensor(pcvmod=model)

  CALL gtflna(0,'PHASECC',filInfo,irc)
  IF (irc /= 0) filInfo = ' '
  CALL stripdir(filInfo)

  IF (ircPhc == 0) CALL wtphafil(filphc,satUpd,recUpd,filTitle,model,filInfo)

! Deallocate local pointers
! -------------------------
  DO iAnt=1,nAnt
    DO isys=0,nSys
      IF (.NOT. ASSOCIATED(recUpd(iAnt)%sys(iSys)%freq)) CYCLE
      DO ifrq=1,nFrq
        IF ( ASSOCIATED(recUpd(iAnt)%sys(iSys)%freq(iFrq)%fac)) &
          DEALLOCATE(recUpd(iAnt)%sys(iSys)%freq(iFrq)%fac,stat=iac)
        IF ( ASSOCIATED(recUpd(iAnt)%sys(iSys)%freq(iFrq)%off)) &
          DEALLOCATE(recUpd(iAnt)%sys(iSys)%freq(iFrq)%off,stat=iac)
        IF ( ASSOCIATED(recUpd(iAnt)%sys(iSys)%freq(iFrq)%pat)) &
          DEALLOCATE(recUpd(iAnt)%sys(iSys)%freq(iFrq)%pat,stat=iac)
      ENDDO
      DEALLOCATE(recUpd(iAnt)%sys(iSys)%freq,stat=iac)
    ENDDO
    DEALLOCATE(recUpd(iAnt)%sys,stat=iac)
  ENDDO
  DEALLOCATE(recUpd,stat=iac)

  DEALLOCATE(satUpd,stat=iac)

  RETURN
END SUBROUTINE pcvstore

END MODULE
