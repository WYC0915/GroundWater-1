MODULE s_UPDMEA
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE updmea(iFrmat, filCod, filPha, scrObs, nFreq , window, &
                    staInfo,staNam, ambsat, ambigu, iDeltT, nSatel, &
                    SatNum, nEpoch, timRef, anzObs, nEpFlg, iOKprt, &
                    ambnep)

! -------------------------------------------------------------------------
! Purpose:    Copy Observations from scratch into Observation file,
!             update header information (RXOBV3)
!
! Parameters:
!         in: iFrmat : format of obs. files         i4b
!             filCod : name of code obs. file       chr
!             filPha : name of phase obs. file      chr
!             scrObs : name of scratch files        chr(2)
!             nFreq  : # of freq in file            i4b(2)
!             window : Observ. window               r8b(2,*)
!             staInfo: staInfo: entries for Bernese TYPE(t_staCrux)
!             staNam : Station name                 CHARACTER(LEN=staNameLength)
!             ambsat : ambiguities satellites       i4b(maxAmb)
!             ambigu : ambiguities epochs           r8b(maxAmb)
!     in/out: iDeltT : Sampling interval            i4b(2)
!             nSatel : # of satellites in file      i4b(2)
!             SatNum : Satellite numbers            i4b(maxSat,2)
!             nEpoch : # of epochs                  i4b(2)
!        out: timRef : Reference time (1st epoch)   r8b(2)
!             anzObs : # obs per sat, frq, mea      i4b(*,2,2)
!             nEpFlg : # of epoch flags             i4b(2)
!             iOKprt : # of epochs written in file  i4b(2)
!             ambnep : Amb. next valid phase epoch  r8b(maxAmb)
!
!
! Author:     R. Dach
!
! Created:    17-Feb-2002
! Last mod.:  16-Apr-2012
!
! Changes:    22-Jan-2001 RD: DIM-bug fixed
!             05-Mar-2001 RD: bug if obs. file not written
!             08-Nov-2001 RD: write no empty observation file
!             18-Dec-2001 RD: iDeltT is an array,
!                             check time window with frac. part
!             25-Sep-2002 HU: Remove i_astlib
!             17-Feb-2003 LM: Use m_maxdim
!             25-Nov-2009 MM/SS: Sort observations according to satellite
!                                number (and system)
!             15-Jul-2010 EO: Handling of stations problems
!             27-Oct-2010 SL: use m_bern with ONLY
!             16-May-2011 HB: IF condition separated into two IF's
!             16-Apr-2012 RD: safe realization of satellite sort
!
! SR used:    opnfil, opnerr, rdobsi, wtobsi
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

!*
!
  USE m_bern,   ONLY: i4b, r8b, lfnErr, lfn001, lfn002, &
                      fileNameLength, staNameLength
  USE m_maxdim, ONLY: maxsat, maxamb
  USE s_rdobsi
  USE s_opnfil
  USE f_tstflg
  USE s_wtobsi
  USE s_opnerr
  USE d_stacrx, ONLY: t_stacrux
  USE s_iOrdUp
  IMPLICIT NONE
!
!
! Global Parameters
! -----------------
  INTEGER(i4b)                       :: iFrmat ! format of obs. files
  CHARACTER(LEN=fileNameLength)      :: filCod ! name of code obs. file
  CHARACTER(LEN=fileNameLength)      :: filPha ! name of phase obs. file
  CHARACTER(LEN=fileNameLength),     &
                        DIMENSION(2) :: scrObs ! name of scratch files
  INTEGER(i4b), DIMENSION(2)         :: nFreq  ! # of freq in file
  REAL(r8b),    DIMENSION(2)         :: window ! Observ. window
  TYPE(t_staCrux)                    :: staInfo! staInfo: entries for Bernese
  CHARACTER(LEN=staNameLength)       :: staNam ! Station name
  INTEGER(i4b), DIMENSION(maxamb)    :: ambsat ! amb. satellites
  REAL(r8b),    DIMENSION(maxamb,3)  :: ambigu ! amb. (start) epochs

  INTEGER(i4b), DIMENSION(2)         :: iDeltT ! Sampling interval
  INTEGER(i4b), DIMENSION(2)         :: nSatel ! # of satellites in file
  INTEGER(i4b), DIMENSION(maxSat,2)  :: SatNum ! Satellite numbers
  INTEGER(i4b), DIMENSION(2)         :: nEpoch ! # of epochs

  REAL(r8b),    DIMENSION(2)         :: timRef ! Reference time (1st epoch)
  INTEGER(i4b), DIMENSION(maxsat,2,2):: anzObs ! # obs per sat, frq, mea
  INTEGER(i4b), DIMENSION(2)         :: nEpFlg ! # of epoch flags
  INTEGER(i4b), DIMENSION(2)         :: iOKprt ! # of epochs written in file
  REAL(r8b),    DIMENSION(maxamb)    :: ambnep ! amb. next valid phase epoch

!************************************************************************
!
! Local Parameters
! ----------------
  INTEGER(i4b), DIMENSION(2), PARAMETER :: iFrqs = (/1,2/)
!
! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength)      :: filNam ! name of obs. file
  CHARACTER(LEN=1)                   :: epFlag ! Epoch flag
  CHARACTER(LEN=1),                  &
       DIMENSION(maxSat,2)           :: obsFlg ! Observation flag
  CHARACTER(LEN=1),                  &
       DIMENSION(maxSat,2)           :: hlpFlg ! Observation flag
  INTEGER(i4b)                       :: nsat   ! # Sat per epoch
  INTEGER(i4b), DIMENSION(maxSat)    :: hlpSat ! Sat-num. per epoch
  INTEGER(i4b), DIMENSION(maxSat)    :: nrSat  ! Sat-num. per epoch
  INTEGER(i4b), DIMENSION(maxSat)    :: idxSat ! Sat-num. sort index
  INTEGER(i4b)                       :: numObs ! # obs. per sat., epoch
  INTEGER(i4b)                       :: iMea   ! Counter for mea. type
  INTEGER(i4b)                       :: iEpo   ! Counter for epochs
  INTEGER(i4b)                       :: iFreq  ! Counter for freq.
  INTEGER(i4b)                       :: iSat   ! Counter for Satellites
  INTEGER(i4b)                       :: jSat   ! Counter for Satellites
  INTEGER(i4b)                       :: iProb  ! Counter for station problems
  INTEGER(i4b)                       :: irc    ! return code
  INTEGER(i4b)                       :: ios    ! io status
  INTEGER(i4b)                       :: iAmb   ! Counter for ambiguities
  INTEGER(i4b)                       :: lamb   ! Last amb. index
  INTEGER(i4b)                       :: ambNum ! Number of ambiguities
  REAL(r8b)                          :: ObsTim ! Epoch (MJD)
  REAL(r8b), DIMENSION(2)            :: DeltaT ! Epoch correction
  REAL(r8b), DIMENSION(maxSat,2)     :: Observ ! Observations
  REAL(r8b), DIMENSION(maxSat,2)     :: hlpObs ! Observations
  REAL(r8b), DIMENSION(2)            :: timepo ! first/last epoch (MJD)
  REAL(r8b)                          :: epoch  ! Corrected epoch (MJD)
!
! Get number of ambiguities
! -------------------------
  ambNum = 0
  DO iAmb=1,maxamb
    IF(AMBSAT(iAmb) == 0) EXIT
    ambNum = ambNum + 1
  ENDDO
!
! Init the Counter for the Obs.
! ----------------------------
  DO iMea=1,2
    IF (iMea == 1) filNam=filCod
    IF (iMea == 2) filNam=filPha
    anzObs(:,:,iMea)=0
    iOKprt(iMea)=0
    nEpFlg(iMea)=0
    timepo(1:2)=0D0
    ambnep = 0.0D0
!
! Copy the Observations Scratch Files Back
! ----------------------------------------
    IF (filNam .NE. ' ') THEN
!
! Reopen Observation and Scratch File
! -----------------------------------
      CALL opnfil(lfn001,filNam,'UNKNOWN','UNFORMATTED',' ',' ',ios)
      CALL opnerr(lfnerr,lfn001,ios,filNam,'UPDMEA')
      CALL opnfil(lfn002,scrObs(iMea),'OLD','UNFORMATTED',' ',' ',ios)
      CALL opnerr(lfnerr,lfn002,ios,scrObs(iMea),'UPDMEA')
!
! Copy all Observations from Scratch File Back to Observation File
! ----------------------------------------------------------------
      iEpoLoop: DO iEpo=1,nEpoch(iMea)
!
! Read Observations
! -----------------
        CALL rdobsi(lfn002,iFrmat,nFreq(iMea),iFrqs,ObsTim,  &
             DeltaT,epFlag,nSat,nrSat,obsFlg,Observ,irc)
!
! Check End of File
! -----------------
        IF(irc /= 0) EXIT iEpoLoop
!
! Check for problematic intervals
! -------------------------------
        DO iProb = 1,staInfo%nProb
          IF (staNam .eq. staInfo%staProb(iProb)%stanam .AND. &
               ObsTim+(DeltaT(1)+.5D0)/86400d0 >= &
                staInfo%staProb(iProb)%timint%t(1) .AND. &
               ObsTim+(DeltaT(1)-.5D0)/86400d0 <= &
                staInfo%staProb(iProb)%timint%t(2)) THEN
            CYCLE iEpoLoop
          ENDIF
        END DO
!
! Check Observation window
! ------------------------
        IF ( ObsTim+(DeltaT(1)+.5D0)/86400d0 < window(1)            .OR. &
             ObsTim+(DeltaT(1)-.5D0)/86400d0 > window(2) ) THEN
          CYCLE iEpoLoop
        ENDIF

        epoch = ObsTim + DeltaT(1)/86400d0
!
! Count the Number of Obs. per Satellite for Obs. Header
! ------------------------------------------------------
        DO iSat=1,nSat
          DO iFreq=1,nFreq(iMea)
            IF (Observ(iSat,iFreq) .NE. 0D0) THEN

              lamb = 0
              IF(iMea == 2) THEN
                DO iAmb=1,ambNum
                  IF(AMBSAT(iAmb) == nrSat(iSat) .AND.            &
                       AMBIGU(iAmb,1) < (epoch + 0.5D0/86400.0D0)) THEN
                    lamb = iAmb
                  ENDIF
                END DO
              ENDIF

              IF (lamb /= 0) THEN
                IF (ambnep(lamb) == 0.0D0) THEN
                  ambnep(lamb) = epoch
                ENDIF
              ENDIF

              DO jSat=1,nSatel(iMea)
                IF (nrSat(iSat) == SatNum(jSat,iMea)) THEN
                  anzObs(jSat,iFreq,iMea)=anzObs(jSat,iFreq,iMea)+1
                ENDIF
              ENDDO

            ENDIF
          ENDDO
        ENDDO
!
! Count Number of Epochs with Observations (for prt)
! --------------------------------------------------
        IF (nSat > 0) THEN
          iOKprt(iMea)=iOKprt(iMea)+1
          IF(TSTFLG(epFlag,0)) nEpFlg(iMea)=nEpFlg(iMea)+1
        ENDIF
!
! Get First and Last Epoch
! ------------------------
        IF (timepo(1) == 0D0) timepo(1)=ObsTim
        timepo(2)=ObsTim
!
! Sort observations according to satellite number (and system)
! ------------------------------------------------------------
        CALL iOrdUp(nrSat,nSat,idxSat)
        hlpSat(1:nSat)   = nrSat(idxSat(1:nSat))
        nrSat(1:nSat )   = hlpSat(1:nSat)
        hlpFlg(1:nSat,:) = obsFlg(idxSat(1:nSat),:)
        obsFlg(1:nSat,:) = hlpFlg(1:nSat,:)
        hlpobs(1:nSat,:) = observ(idxSat(1:nSat),:)
        observ(1:nSat,:) = hlpobs(1:nSat,:)
!
! Write Observations
! ------------------
        CALL wtobsi(lfn001,iFrmat,nFreq(iMea),ObsTim,     &
                    DeltaT,epFlag,nSat,nrSat,obsFlg,Observ)
      ENDDO iEpoLoop
!
! Update timRef and nEpoch
! ------------------------
      timRef(iMea)=timepo(1)
!
      IF (iOKprt(iMea) < 2) THEN
        nEpoch(iMea)=iOKprt(iMea)
        iDeltT(iMea)=1
      ELSE
        nEpoch(iMea)=IDNINT(86400.D0*(timepo(2)-timepo(1))/iDeltT(iMea))+1
      ENDIF
!
! Check List of Satellites
! ------------------------
      iSat=nSatel(iMea)
      DO WHILE (iSat >= 1)
        numobs=0
        DO iFreq=1,nFreq(iMea)
          numobs=numobs+anzObs(iSat,iFreq,iMea)
        ENDDO
!
! Eliminate Sat. with No Obs.
! ---------------------------
        IF (numobs == 0) THEN
          nSatel(iMea)=nSatel(iMea)-1
          DO jSat=iSat,nSatel(iMea)
            SatNum(jSat,iMea)=SatNum(jSat+1,iMea)
            DO iFreq=1,nFreq(iMea)
              anzObs(jSat,iFreq,iMea)=anzObs(jSat+1,iFreq,iMea)
            ENDDO
          ENDDO
        ENDIF
        iSat=iSat-1
      ENDDO

! Close Observation and Scratch File
! ----------------------------------
      CLOSE(UNIT=lfn002,STATUS='DELETE')

      IF (nSatel(iMea) > 0) THEN
        CLOSE(UNIT=lfn001)
      ELSE
        CLOSE(UNIT=lfn001,STATUS='DELETE')
      ENDIF

    ENDIF
  ENDDO
!
  RETURN
  END SUBROUTINE

END MODULE
