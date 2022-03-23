MODULE s_PCVSAV
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE pcvsav(title ,npar  ,locq  ,xxx   ,nanrao,antrao,numrao, &
                  prnrao,nfrrao,nancal,antcal,numcal,prncal,nfrcal, &
                  nanspv,nsaspv,satspv,gnrspv, &
                  globalwindow ,nadmax,rapzenmax)

! -------------------------------------------------------------------------
! Purpose:    update
!               - receiver antenna phase center offsets and/or
!                 elevation/azimuth-dependent variations
!               - satellite antenna phase center variations
!               as estimated by gpsest
!
! Author:     R. Dach
!
! Created:    18-Feb-2008
! Last mod.:  23-Dec-2010
!
! Changes:    02-Nov-2010 SL: use m_bern with ONLY, use undef_i, conv bug corr.
!             23-Dec-2010 RD: Write GLONASS-only PCV result files
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnErr, fileNameLength, Stanam2length
  USE m_time,   ONLY: t_timint
  USE d_const,  ONLY: date,PI
  USE d_satfil, ONLY: typeMWTR
  USE d_phaecc, ONLY: t_phasfil, alcantbu, wtphafil, recant, satant, updmodel
  USE d_stacrx, ONLY: undef_i

  USE s_alcerr
  USE s_gtflna
  USE s_gtsensor
  USE s_stripdir
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=80)                 :: title  ! title line
  INTEGER(i4b)                      :: npar   ! number of parameters
  INTEGER(i4b),      DIMENSION(:,:) :: locq   ! Parameter description
  REAL(r8b),         DIMENSION(:)   :: XXX    ! solution vector

  INTEGER(i4b)                      :: nanrao ! number of receiver
                                              ! antenna offsets
  CHARACTER(LEN=20), DIMENSION(2,*) :: antrao ! receiver and antenna
                                              ! name for request i
  INTEGER(i4b),      DIMENSION(2,*) :: numrao ! antenna numbers
                                              ! "from - to" for request i
  INTEGER(i4b),      DIMENSION(:)   :: prnrao ! satellite system
  INTEGER(i4b),      DIMENSION(:)   :: nfrrao ! frequency for receiver antenna
                                              ! phase center request i

  INTEGER(i4b)                      :: nancal ! number of receiver
                                              ! antenna pattern
  CHARACTER(LEN=20), DIMENSION(2,*) :: antcal ! receiver and antenna
                                              ! name for request i
  INTEGER(i4b),      DIMENSION(2,*) :: numcal ! antenna numbers
                                              ! "from - to" for request i
  INTEGER(i4b),      DIMENSION(:)   :: prncal ! satellite system
  INTEGER(i4b),      DIMENSION(:)   :: nfrcal ! frequency for receiver antenna
                                              ! phase center request i

  INTEGER(i4b)                 :: nanspv ! number of satellite antenna phase
                                         ! center groups to be estimated
  INTEGER(i4b), DIMENSION(:)   :: nsaspv ! nsaspv(i),i=1,..,nanspv
                                         ! number of satellites belonging to
                                         ! antenna phase center group i
  INTEGER(i4b), DIMENSION(:,:) :: satspv ! satspv(j,i),j=1,..,nsaspv(i)
                                         !             i=1,..,nanspv
                                         ! satellite numbers of each antenna
                                         ! phase center group
  INTEGER(i4b), DIMENSION(:)   :: gnrspv ! gnrspv(i),i=1,..,nanspv
                                         ! user-defined number of satellite
                                         ! antenna phase center group i

  TYPE(t_timint)               :: globalWindow ! window to be processed
                                         ! (from-to, MJD)
  REAL(r8b)                    :: nadmax
  REAL(r8b)                    :: rapzenmax

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER            :: srName = 'pcvsav'


! Local Variables
! ---------------
  TYPE(t_phasfil), DIMENSION(:), POINTER :: satUpd
  TYPE(t_phasfil), DIMENSION(:), POINTER :: recUpd

  CHARACTER(LEN=fileNameLength)          :: filphc
  CHARACTER(LEN=60)                      :: filInfo
  CHARACTER(LEN=Stanam2length)           :: name
  CHARACTER(LEN=10)                      :: model

  INTEGER(i4b), DIMENSION(nanrao)        :: idxrao
  INTEGER(i4b), DIMENSION(nancal)        :: idxcal
  INTEGER(i4b)                           :: numGrp
  INTEGER(i4b)                           :: numb
  INTEGER(i4b)                           :: iSap,iGrp
  INTEGER(i4b)                           :: iSat,jSat,nSat
  INTEGER(i4b)                           :: iAnt,jAnt,kAnt
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
  INTEGER(i4b)                           :: iRao
  INTEGER(i4b)                           :: ircPhc
  INTEGER(i4b)                           :: iac,irc

  REAL(r8b)                              :: epo

! Init variables
! --------------
  CALL gtflna(0,'PHASRSG',filphc,ircPhc)
  IF (ircPhc /= 0) RETURN

  NULLIFY(recUpd)
  NULLIFY(satUpd)

  ALLOCATE(recUpd(nanrao+nancal),stat=iac)
  CALL alcerr(iac,'recUpd',(/nanrao+nancal/),srName)

  nSat = 0
  DO iSap = 1,nanspv
    nSat = nSat+nsaspv(iSap)
  ENDDO
  ALLOCATE(satUpd(nSat),stat=iac)
  CALL alcerr(iac,'satUpd',(/nSat/),srName)

! Prepare the update list for receiver antennas
! ---------------------------------------------
  nFrq = min0( max0( maxval(nfrrao(1:nanrao)) , maxval(nfrcal(1:nancal)) ),2 )
  nSys =       max0( maxval(prnrao(1:nanrao)) , maxval(prncal(1:nancal)) )

  CALL alcantbu(nanrao+nancal,nSys,recUpd)
  CALL alcantbu(nSat,0,satUpd)

  nAnt = 0
  nSat = 0
  epo  = (globalWindow%t(1) + globalWindow%t(2)) / 2

  ! List of receiver antenna offsets
  DO iAnt = 1,nanrao
    nAnt = nAnt+1
    recUpd(iAnt)%name = antrao(2,iAnt)
    recUpd(iAnt)%numb = numrao(2,iAnt)

    IF (numrao(1,iAnt) == 0 .AND. numrao(2,iAnt) == undef_i) &
      recUpd(iAnt)%individ = 1

    idxrao(iAnt) = iAnt
  ENDDO

  ! List of receiver antenna pattern
  DO iAnt = 1,nancal
    kAnt = 0
    DO jAnt = 1,nanrao
      IF (recUpd(jAnt)%name == antcal(2,iAnt) .AND. &
          recUpd(jAnt)%numb == numcal(2,iAnt)) kAnt = jAnt
    ENDDO

    idxcal(iAnt) = kAnt
    IF (kAnt /= 0) CYCLE

    nAnt = nAnt+1
    recUpd(nAnt)%name = antcal(2,iAnt)
    recUpd(nAnt)%numb = numcal(2,iAnt)
    IF (numrao(1,iAnt) == 0 .AND. numrao(2,iAnt) == undef_i) &
      recUpd(nAnt)%individ = 1

    idxcal(iAnt) = nAnt
  ENDDO

  ! Init some arrays
  DO iAnt = 1,nAnt

    IF (recUpd(iAnt)%name(17:20) == '????') recUpd(iAnt)%name(17:20) = '    '

    DO iSys = 0,nSys
      recUpd(iant)%sys(isys)%typ=0
      recUpd(iant)%sys(isys)%nfreq=nFrq
      recUpd(iAnt)%sys(iSys)%resolu(:) = (/0,90,360,90/)
      ALLOCATE(recUpd(iAnt)%sys(iSys)%freq(nFrq),stat=iac)
      CALL alcerr(iac,'recUpd%sys%freq',(/nFrq/),srName)
      DO iFrq = 1,nFrq
        recUpd(iAnt)%sys(iSys)%freq(iFrq)%freq = 0
        NULLIFY(recUpd(iAnt)%sys(iSys)%freq(iFrq)%off)
        NULLIFY(recUpd(iAnt)%sys(iSys)%freq(iFrq)%fac)
        NULLIFY(recUpd(iAnt)%sys(iSys)%freq(iFrq)%pat)
      ENDDO
    ENDDO
  ENDDO

! List of satellites
! ------------------
  iSys = 0
  nSat = 0
  DO iSap = 1,nanspv
    DO iSat = 1,nsaspv(iSap)
      nSat = nSat + 1
      CALL gtsensor(satspv(iSat,iSap),epo,typeMWTR, &
                    satUpd(nSat)%name,satUpd(nSat)%numb)
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
    ENDDO
  ENDDO
! Loop the parameter list
! -----------------------
  DO iPar = 1,nPar

    ! Receiver antenna offsets
    ! ------------------------
    IF (locq(1,iPar) == 5 .AND. locq(3,iPar) /= 4) THEN

      iRao = locq(2,iPar)
      iAnt = idxrao(iRao)
      iFr1 = locq(3,iPar)
      iFr2 = locq(3,iPar)
      iOrd = 0
      iCal = locq(4,iPar)

      IF (locq(3,iPar) == 3 ) THEN
        iFr1 = 1
        iFr2 = 2
      ENDIF

      DO iFrq = iFr1,iFr2

        ! corrections for all GNSS
        IF (prnrao(iRao) == 10) THEN
          DO iSys = 0,prnrao(iRao)-1
            recUpd(iAnt)%sys(iSys)%method    = 'estimated by GPSEST'
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
            recUpd(iAnt)%sys(iSys)%freq(iFrq)%off(iOrd,iCal) = XXX(iPar)*1D3
          ENDDO

        ! corrections only for one GNSS
        ELSE
          iSys = prnrao(iRao)
          recUpd(iAnt)%sys(iSys)%method    = 'estimated by GPSEST'
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
          recUpd(iAnt)%sys(iSys)%freq(iFrq)%off(iOrd,iCal) = XXX(iPar)*1D3
        ENDIF
      ENDDO
    ENDIF


    ! Receiver antenna pattern
    ! ------------------------
    IF (locq(1,iPar) == 18 .AND. locq(3,iPar) /= 4) THEN

      iCal = locq(2,iPar)
      iAnt = idxcal(iCal)
      iFr1 = locq(3,iPar)
      iFr2 = locq(3,iPar)
      iOrd = 0
      iZen = locq(4,iPar)
      iAzi = locq(5,iPar)
      nZen = max0(2,ABS(locq(6,iPar)))
      nAzi = max0(2,locq(7,iPar))

      iMod = 1
      IF (locq(6,iPar) < 0) iMod = 4

      mAzi = nAzi
      IF (mAzi == 2) mAzi = 1

      IF (iMod >= 2 .AND. iMod <= 4) THEN
        iAzi = iAZi + nAzi+1
        mAzi = nAzi*2+1
      ENDIF

      IF (locq(3,iPar) == 3 ) THEN
        iFr1 = 1
        iFr2 = 2
      ENDIF

      DO iFrq = iFr1,iFr2

        ! corrections for all GNSS
        IF (prncal(iCal) == 10) THEN
          DO iSys = 0,prncal(iCal)-1
            recUpd(iAnt)%sys(iSys)%method    = 'estimated by GPSEST'
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
            recUpd(iAnt)%sys(iSys)%freq(iFrq)%pat(iOrd,iZen,iAzi) = XXX(iPar)*1D3
            IF (iAzi == 1 .AND. recUpd(iAnt)%sys(iSys)%resolu(3) /= 360) &
              recUpd(iAnt)%sys(iSys)%freq(iFrq)%pat(iOrd,iZen,nAzi) = XXX(iPar)*1D3
          ENDDO

        ! corrections only for one GNSS
        ELSE
          iSys = prncal(iCal)
          recUpd(iAnt)%sys(iSys)%method    = 'estimated by GPSEST'
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
          recUpd(iAnt)%sys(iSys)%freq(iFrq)%pat(iOrd,iZen,iAzi) = XXX(iPar)*1D3
          IF (iAzi == 1 .AND. recUpd(iAnt)%sys(iSys)%resolu(3) /= 360) &
            recUpd(iAnt)%sys(iSys)%freq(iFrq)%pat(iOrd,iZen,nAzi) = XXX(iPar)*1D3
        ENDIF
      ENDDO
    ENDIF

    ! Satellite antenna pattern
    ! -------------------------
    IF (locq(1,iPar) == 25) THEN

      iOrd   = 0
      iZen   = locq(4,iPar)
      iAzi   = locq(5,iPar)
      nZen   = max0(2,locq(6,iPar))
      nAzi   = max0(2,locq(7,iPar))

      iMod = 1
      iSys = 0

      mAzi = nAzi
      IF (mAzi == 2) mAzi = 1

      iGrp = 0
      numGrp = locq(3,iPar)
      DO iSap=1,nanspv
        IF (gnrspv(iSap) == numGrp) THEN
          iGrp = iSap
          EXIT
        ENDIF
      ENDDO
      IF (iGrp == 0) THEN
        WRITE(LFNERR,'(/,A,/16X,A,/,16X,A,I4,/,A,/)')                          &
        ' ### SR PCVSAV: Satellite antenna phase center variation parameters', &
                        'for the following satellite group not found',         &
                        'Satellite group number:',numGrp,                      &
                        'Satellite antennas belonging to this group ' //       &
                        'not updated !!!'
      ELSE

        DO iSat = 1,nsaspv(iGrp)
          CALL gtsensor(satspv(iSat,iGrp),epo,typeMWTR,name,numb)

          DO jSat = 1,nSat
            IF (satUpd(jSat)%name == name .AND. satUpd(jSat)%numb == numb) THEN

              satUpd(jSat)%sys(isys)%nfreq     = 2
              satUpd(jSat)%sys(iSys)%method    = 'estimated by GPSEST'
              satUpd(jSat)%sys(iSys)%date      = date
              satUpd(jSat)%sys(iSys)%typ       = iMod

              satUpd(jSat)%sys(iSys)%resolu(1) = iOrd
              satUpd(jSat)%sys(iSys)%resolu(2) = NINT(nadmax*180d0/pi/(nZen-1))
              satUpd(jSat)%sys(iSys)%resolu(3) = 360/(nAzi-1)
              satUpd(jSat)%sys(iSys)%resolu(4) = NINT(nadmax*180d0/pi)

              DO iFrq = 1,2
                IF (.NOT. ASSOCIATED(satUpd(jSat)%sys(iSys)%freq(iFrq)%pat)) THEN
                  ALLOCATE(satUpd(jSat)%sys(iSys)%freq(iFrq)%pat(0:iOrd,nZen,mAzi),stat=iac)
                  CALL alcerr(iac,'satUpd%sys%freq%pat',(/iOrd+1,nZen,mAzi/),srName)
                  satUpd(jSat)%sys(iSys)%freq(iFrq)%pat = 0d0
                ENDIF

                satUpd(jSat)%sys(iSys)%freq(iFrq)%freq = iFrq
                satUpd(jSat)%sys(iSys)%freq(iFrq)%pat(iOrd,iZen,iAzi) = XXX(iPar)*1D3
                IF (iZen == 1 .AND. satUpd(jSat)%sys(iSys)%resolu(3) /= 360) &
                  satUpd(jSat)%sys(iSys)%freq(iFrq)%pat(iOrd,iZen,nAzi) = XXX(iPar)*1D3
              ENDDO ! Loop Frequencies
            ENDIF
          ENDDO
        ENDDO ! Loop satellites
      ENDIF
    ENDIF
  ENDDO ! End of parameter loop

! Update the estimated antenna model components to full models
! ------------------------------------------------------------
  CALL updmodel(recUpd,recAnt,filphc)
  CALL updmodel(satUpd,satAnt,filphc)

! Write updated phase center file(s)
! ---------------------------------
  CALL gtsensor(pcvmod=model)

  CALL gtflna(0,'PHASECC',filInfo,irc)
  IF (irc /= 0) filInfo = ' '
  CALL stripdir(filInfo)

  IF (ircPhc == 0) CALL wtphafil(filphc,satUpd,recUpd,title,model,filInfo)

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
END SUBROUTINE pcvsav

END MODULE
