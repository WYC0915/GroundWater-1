MODULE s_FEWOBS
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fewobs(iObs,iAmb,reqObs,nsmea,svnmea, &
                  nfrcod,nEpcod,anzcod,nfrpha,nEppha,anzpha,   &
                  rxfile,filcod,filphs,iFrmat,scrobs,copyback)

! -------------------------------------------------------------------------
! Purpose:    Check for Bernese obs. files with a small number of epochs
!
! Remarks:
!
! Author:     R. Dach
!
! Created:    24-Aug-2010
!
! Changes:    02-Feb-2012 RD: Extended requirements on a Bernese obs. file
!             13-Jun-2012 DT: RETURN if check is not requested
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, fileNameLength, lfn001, lfn002, lfnerr
  USE m_global, ONLY: maxsys, g_strsys
  USE m_maxdim, ONLY: maxsat
  USE p_rxobv3, ONLY: t_rxobv3_req

  USE s_gtflna
  USE s_opnfil
  USE s_opnerr
  USE s_rdobsi
  USE s_wtobsi
  IMPLICIT NONE

! Variables in parameter list
! ---------------------------
! IN:
  INTEGER(i4b), DIMENSION(2)       :: iObs   ! Number of epochs with code/phase
  INTEGER(i4b)                     :: iAmb   ! Number of ambiguities
  TYPE(t_rxobv3_req)               :: reqObs ! Requirements on an obs. file
  INTEGER(i4b), DIMENSION(2)       :: nsmea  ! # of satellites in file
  INTEGER(i4b), DIMENSION(maxSat,2):: svnmea ! Satellite numbers
  INTEGER(i4b)                     :: nfrcod ! # frequencies (code)
  INTEGER(i4b)                     :: nepcod ! # epochs (code)
  INTEGER(i4b), DIMENSION(maxsat,2):: anzCod ! # code obs per sat, frq
  INTEGER(i4b)                     :: nfrpha ! # frequencies (phase)
  INTEGER(i4b)                     :: neppha ! # epochs (phase)
  INTEGER(i4b), DIMENSION(maxsat,2):: anzPha ! # phase obs per sat, frq
  CHARACTER(LEN=fileNameLength)    :: rxFile ! Name of the RINEX file (for msg.)
  INTEGER(i4b)                     :: iFrmat ! format of obs. files
  CHARACTER(LEN=fileNameLength),     &
                DIMENSION(2)       :: scrObs ! name of scratch files

! IN/OUT:
  CHARACTER(LEN=fileNameLength),&
                DIMENSION(2)       :: filcod ! Name of the code obs. file
  CHARACTER(LEN=fileNameLength),&
                DIMENSION(2)       :: filphs ! Name of the phase obs. file
  LOGICAL,      OPTIONAL           :: copyback ! run another UPDMEA or not

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER      :: srname = 'fewobs'

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength)    :: filaux ! Scratch file

  INTEGER(i4b), DIMENSION(2,0:maxsys):: anzSat
  INTEGER(i4b), DIMENSION(2,0:maxsys):: anzObs
  INTEGER(i4b)                       :: iTyp
  INTEGER(i4b)                       :: iSat
  INTEGER(i4b)                       :: iSys
  INTEGER(i4b)                       :: ii
  INTEGER(i4b)                       :: irc
  INTEGER(i4b)                       :: ios

  INTEGER(i4b), DIMENSION(2)         :: delFil ! delete observation file
                                           !  0: do not delete
                                           ! -1: no input filename
                                           ! 11: too few epochs
                                           ! 12: too many ambiguities
                                           ! 13: phase/code-only not allowed
                                           ! iSys*100+1: too few observations
                                           ! iSys*100+2: too few satellties

  INTEGER(i4b), DIMENSION(2,maxsys)  :: delSys ! GNSS to be deleted

! Init variables
! --------------
  IF ( PRESENT(copyBack) ) copyBack = .FALSE.


! Nothing to do
! -------------
  IF (LEN_TRIM(filphs(1)) == 0  .AND. LEN_TRIM(filcod(1)) == 0) RETURN

  IF (.NOT.reqObs%ireq) RETURN


! Make the statistics on the available observations per GNSS
! ----------------------------------------------------------
  anzSat = 0
  anzObs = 0
  DO iTyp = 1,2
    DO iSat = 1,nsmea(iTyp)
      iSys = svnmea(iSat,iTyp) / 100 + 1

      IF (iSys > maxsys) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/)')                          &
        ' ### SR FEWOBS: Unexpected satellite number',                      &
                        '(exceeding the range of known GNSS identifiers)',  &
                        'Satellite number: ',svnmea(isat,ityp)
        CYCLE
      ENDIF

      anzSat(iTyp,iSys) = anzSat(iTyp,iSys) + 1
      IF (iTyp == 1) anzObs(iTyp,iSys) = anzObs(iTyp,iSys) + &
                     MAX(anzCod(iSat,1),anzCod(iSat,nfrcod))
      IF (iTyp == 2) anzObs(iTyp,iSys) = anzObs(iTyp,iSys) + &
                     MAX(anzPha(iSat,1),anzPha(iSat,nfrpha))
    ENDDO
  ENDDO

  DO iTyp = 1,2
    DO iSys = 1,maxsys
      anzSat(iTyp,0) = anzSat(iTyp,0) + anzSat(iTyp,iSys)
      anzObs(iTyp,0) = anzObs(iTyp,0) + anzObs(iTyp,iSys)
    ENDDO
  ENDDO


! Init the "file-delete-value"
! ----------------------------
  delFil(1) = 0
  IF ( LEN_TRIM(filcod(1)) == 0 ) delFil(1) = -1

  delFil(2) = 0
  IF ( LEN_TRIM(filphs(1)) == 0 ) delFil(2) = -1


! Check for files with too few observations
! -----------------------------------------
  IF (iObs(1) < reqObs%reqEpo .AND. delFil(1) == 0) delFil(1) = 11
  IF (iObs(2) < reqObs%reqEpo .AND. delFil(2) == 0) delFil(2) = 11


! Check for files with too many ambiguities
! -----------------------------------------
  IF (iAmb > reqObs%reqAmb .AND. reqObs%reqAmb > 0 .AND. &
      delFil(2) == 0) delFil(2) = 12


! Check requirements for the different GNSS
! -----------------------------------------
  delSys = 0
  DO iTyp = 1,2
    IF ( delFil(iTyp) /= 0) CYCLE

    DO iSys = maxsys,0,-1
      IF ( iSys > 0 .AND. anzObs(1,iSys) + anzObs(2,iSys) == 0 ) CYCLE
      IF ( anzSat(iTyp,iSys) < reqObs%minnum(1,iSys) ) THEN
        delFil(iTyp) = iSys*100+2
        IF (iSys > 0) delSys(iTyp,iSys) = 1
      ELSEIF ( anzObs(iTyp,iSys) < reqObs%minnum(1+iTyp,iSys) ) THEN
        delFil(iTyp) = iSys*100+1
        IF (iSys > 0) delSys(iTyp,iSys) = 1
      ENDIF
    ENDDO
  ENDDO

  ! Code without phase
  IF ( delFil(1) == 0 .AND. delFil(2) /= 0 .AND. reqObs%cdOnly /= 1 ) THEN
    delFil(1) = 13
    DO iSys = 1,maxsys
      IF ( delSys(2,iSys) == 1 ) delSys(1,iSys) = 1
    ENDDO
  ENDIF

  ! Phase but not code
  IF ( delFil(2) == 0 .AND. delFil(1) /= 0 .AND. reqObs%phOnly /= 1 ) THEN
    delFil(2) = 13
    DO iSys = 1,maxsys
      IF ( delSys(1,iSys) == 1 ) delSys(2,iSys) = 1
    ENDDO
  ENDIF

! Delete observation files
! ------------------------
  DO ii = 1,2
    IF ( delFil(1) > 0 .AND. delFil(1) < 100) THEN
      CALL opnfil(lfn001,filcod(ii),'UNKNOWN','UNFORMATTED',' ',' ',ios)
      CALL opnerr(lfnerr,lfn001,ios,filcod(ii),srName)
      IF (ios == 0) CLOSE(UNIT=lfn001,STATUS='DELETE')
    ENDIF

    IF ( delFil(2) > 0 .AND. delFil(2) < 100 ) THEN
      CALL opnfil(lfn001,filphs(ii),'UNKNOWN','UNFORMATTED',' ',' ',ios)
      CALL opnerr(lfnerr,lfn001,ios,filphs(ii),srName)
      IF (ios == 0) CLOSE(UNIT=lfn001,STATUS='DELETE')
    ENDIF
  ENDDO


! Write the messages
! ------------------
  ! Minimum number of epochs per file
  IF ( delFil(1) == 11 .OR. delFil(2) == 11 ) THEN
    WRITE(lfnerr,'(/,A,/,16X,A,A,/,16X,A,2(I6,A))')                           &
          ' ### SR FEWOBS: TOO FEW EPOCHS WITH DATA FOUND IN A RINEX FILE.',  &
                          'FILE NAME:    ',TRIM(RXFILE),                      &
                          'EPOCHS FOUND: ',iObs(1),' (CODE)  ',               &
                                           iObs(2),' (PHASE)'
  ENDIF

  ! Maximum number of ambiguities per file
  IF ( delFil(2) == 12 ) THEN
    WRITE(lfnerr,'(/,A,/,16X,A,A,/,16X,A,I6)')                                &
          ' ### SR FEWOBS: TOO MANY AMBIGUITIES FOUND IN A RINEX FILE.',      &
                          'FILE NAME:        ',TRIM(RXFILE),                  &
                          'AMBIGUTIES FOUND: ',iAmb
  ENDIF

  ! Minimum number of satellites per file
  DO iSys = 0,maxsys
    IF ( iSys > 0 .AND. &
        (delFil(1) == iSys*100+2 .OR. delFil(2) == iSys*100+2 ) ) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,A,/,16X,A,2(I3,A))')                         &
            ' ### SR FEWOBS: TOO FEW SATELLITES FOR '//                       &
                             TRIM(g_strsys(iSys-1)) // ' IN A RINEX FILE.',   &
                            'FILE NAME:        ',TRIM(RXFILE),                &
                            'SATELLITES FOUND: ',anzSat(1,iSys),' (CODE)  ',  &
                                                 anzSat(2,iSys),' (PHASE)'
    ELSEIF ( delFil(1) == iSys*100+2 .OR. delFil(2) == iSys*100+2 ) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,A,/,16X,A,2(I3,A))')                         &
            ' ### SR FEWOBS: TOO FEW SATELLITES IN A RINEX FILE.',            &
                            'FILE NAME:        ',TRIM(RXFILE),                &
                            'SATELLITES FOUND: ',anzSat(1,iSys),' (CODE)  ',  &
                                                 anzSat(2,iSys),' (PHASE)'
    ENDIF
  ENDDO

  ! Minimum number of observations per file
  DO iSys = 0,maxsys
    IF ( iSys > 0 .AND. &
        (delFil(1) == iSys*100+1 .OR. delFil(2) == iSys*100+1 ) ) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,A,/,16X,A,2(I8,A))')                         &
            ' ### SR FEWOBS: TOO FEW OBSERVATIONS FOR '//                     &
                             TRIM(g_strsys(iSys-1)) // ' IN A RINEX FILE.',   &
                            'FILE NAME:          ',TRIM(RXFILE),              &
                            'OBSERVATIONS FOUND: ',anzObs(1,iSys),' (CODE)  ',&
                                                   anzObs(2,iSys),' (PHASE)'
    ELSEIF ( delFil(1) == iSys*100+1 .OR. delFil(2) == iSys*100+1 ) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,A,/,16X,A,2(I8,A))')                         &
            ' ### SR FEWOBS: TOO FEW OBSERVATIONS IN A RINEX FILE.',    &
                            'FILE NAME:          ',TRIM(RXFILE),              &
                            'OBSERVATIONS FOUND: ',anzObs(1,iSys),' (CODE)  ',&
                                                   anzObs(2,iSys),' (PHASE)'
    ENDIF
  ENDDO

  ! Code without phase
  IF ( delFil(1) == 13 ) THEN
    WRITE(lfnerr,'(/,A,/,16X,A,A)') &
          ' ### SR FEWOBS: CODE BUT NOT PHASE OBSERVATION FILE RESULTS', &
                          'FILE NAME:          ',TRIM(RXFILE)
  ENDIF

  ! Phase but not code
  IF ( delFil(2) == 13 ) THEN
    WRITE(lfnerr,'(/,A,/,16X,A,A)') &
          ' ### SR FEWOBS: PHASE BUT NOT CODE OBSERVATION FILE RESULTS', &
                          'FILE NAME:          ',TRIM(RXFILE)
  ENDIF

  ! Files are excluded
  IF (delFil(1) > 0 .AND. delFil(1) < 100 .AND. &
      delFil(2) > 0 .AND. delFil(2) < 100) THEN
    WRITE(lfnerr,'(16X,A,/)') 'NO BERNESE OBSERVATION FILE WAS CREATED.'
    filcod(1:2) = ' '
    filphs(1:2) = ' '
  ELSE IF ( delFil(1) > 0  .AND. delFil(1) < 100) THEN
    WRITE(lfnerr,'(16X,A,/)') 'NO BERNESE CODE OBSERVATION FILE WAS CREATED.'
    filcod(1:2) = ' '
  ELSE IF ( delFil(2) > 0  .AND. delFil(2) < 100) THEN
    WRITE(lfnerr,'(16X,A,/)') 'NO BERNESE PHASE OBSERVATION FILE WAS CREATED.'
    filphs(1:2) = ' '
  ELSEIF (delFil(1) > 0 .AND. delFil(2) > 0) THEN
    WRITE(lfnerr,'(16X,A,/)') 'BERNESE OBSERVATION FILE WITHOUT '       // &
                              TRIM(g_strsys(delFil(2)/100-1))//' WAS CREATED.'
    IF ( PRESENT(copyback) ) THEN
      CALL cpyObs(filcod(2),scrObs(1),nEpcod,nFrcod,delSys(1,:))
      CALL cpyObs(filphs(2),scrObs(2),nEppha,nFrpha,delSys(2,:))
      copyBack = .TRUE.
    ELSE
      CALL gtflna(1,'AUXFIL',filaux,irc)
      CALL cpyObs(scrObs(1),filaux,nEpcod,nFrcod,delSys(1,:))
      CALL cpyObs(filaux,scrObs(1),nEpcod,nFrcod)
      CALL cpyObs(scrObs(2),filaux,nEppha,nFrpha,delSys(2,:))
      CALL cpyObs(filaux,scrObs(2),nEppha,nFrpha)
    ENDIF
  ELSE IF ( delFil(1) > 0) THEN
    WRITE(lfnerr,'(16X,A,/)') 'BERNESE CODE OBSERVATION FILE WITHOUT '  // &
                              TRIM(g_strsys(delFil(1)/100-1))//' WAS CREATED.'
    IF ( PRESENT(copyback) ) THEN
      CALL cpyObs(filcod(2),scrObs(1),nEpcod,nFrcod,delSys(1,:))
      copyBack = .TRUE.
    ELSE
      CALL gtflna(1,'AUXFIL',filaux,irc)
      CALL cpyObs(scrObs(1),filaux,nEpcod,nFrcod,delSys(1,:))
      CALL cpyObs(filaux,scrObs(1),nEpcod,nFrcod)
    ENDIF
  ELSE IF ( delFil(2) > 0) THEN
    WRITE(lfnerr,'(16X,A,/)') 'BERNESE PHASE OBSERVATION FILE WITHOUT ' // &
                              TRIM(g_strsys(delFil(2)/100-1))//' WAS CREATED.'
    IF ( PRESENT(copyback) ) THEN
      CALL cpyObs(filphs(2),scrObs(2),nEppha,nFrpha,delSys(2,:))
      copyBack = .TRUE.
    ELSE
      CALL gtflna(1,'AUXFIL',filaux,irc)
      CALL cpyObs(scrObs(2),filaux,nEppha,nFrpha,delSys(2,:))
      CALL cpyObs(filaux,scrObs(2),nEppha,nFrpha)
    ENDIF
  ENDIF


  RETURN

! ------------------------------------
! Definition of additional subroutines
! ------------------------------------
  CONTAINS

! Copy observation records
! ------------------------
  SUBROUTINE cpyObs(inpFil,outFil,nEpoch,nFreq,delSys)
    CHARACTER(LEN=fileNameLength)         :: inpFil ! name of inp. obs. file
    CHARACTER(LEN=fileNameLength)         :: outFil ! name of out. obs. file
    INTEGER(i4b)                          :: nEpoch ! # of epochs
    INTEGER(i4b)                          :: nFreq  ! # of freq in file
    INTEGER(i4b), DIMENSION(maxsys),      &
                                 OPTIONAL :: delSys ! List of GNSS for del.

    ! Local Parameters
    INTEGER(i4b), DIMENSION(2), PARAMETER :: iFrqs = (/1,2/)

    ! Local variables
    CHARACTER(LEN=1)                   :: epFlag ! Epoch flag
    CHARACTER(LEN=1),                  &
         DIMENSION(maxSat,2)           :: obsFlg ! Observation flag
    INTEGER(i4b)                       :: iEpo
    INTEGER(i4b)                       :: iSat
    INTEGER(i4b)                       :: mSat
    INTEGER(i4b)                       :: iSys
    INTEGER(i4b)                       :: nsat   ! # Sat per epoch
    INTEGER(i4b), DIMENSION(maxSat)    :: nrSat  ! Sat-num. per epoch
    REAL(r8b)                          :: ObsTim ! Epoch (MJD)
    REAL(r8b), DIMENSION(2)            :: DeltaT ! Epoch correction
    REAL(r8b), DIMENSION(maxSat,2)     :: Observ ! Observations

! Open observation files
! ----------------------
    CALL opnfil(lfn001,inpFil,'OLD','UNFORMATTED',' ',' ',ios)
    CALL opnerr(lfnerr,lfn001,ios,inpFil,srName)
    CALL opnfil(lfn002,outFil,'UNKNOWN','UNFORMATTED',' ',' ',ios)
    CALL opnerr(lfnerr,lfn002,ios,outFIl,srName)

! Copy all observations
! ---------------------
    DO !iEpo=1,nEpoch

      ! Read Observations
      CALL rdobsi(lfn001,iFrmat,nFreq,iFrqs,ObsTim,  &
           DeltaT,epFlag,nSat,nrSat,obsFlg,Observ,irc)
      IF(irc /= 0) EXIT

      IF ( PRESENT(delSys) ) THEN
        mSat = 0
        DO iSat = 1,nSat
          iSys = nrSat(iSat) / 100 + 1
          IF ( delSys(iSys) == 1 ) CYCLE
          mSat = mSat + 1
          IF (mSat /= iSat) THEN
            nrSat(mSat) = nrSat(iSat)
            Observ(mSat,:) = Observ(iSat,:)
            ObsFlg(mSat,:) = obsFlg(iSat,:)
          ENDIF
        ENDDO
        nSat = mSat
      ENDIF

      ! Write Observations
      CALL wtobsi(lfn002,iFrmat,nFreq,ObsTim,     &
                  DeltaT,epFlag,nSat,nrSat,obsFlg,Observ)
    ENDDO

! Close the files
! ---------------
    CLOSE (lfn001,STATUS='DELETE')
    CLOSE (lfn002)

  END SUBROUTINE cpyObs

END SUBROUTINE fewobs

END MODULE
