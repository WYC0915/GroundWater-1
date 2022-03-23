MODULE s_RDTRPE
CONTAINS

! --------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! --------------------------------------------------------------------------

SUBROUTINE rdtrpe(filNam,trpEst,irCode)

! --------------------------------------------------------------------------
! Purpose:    Read a BERNESE troposphere file (piecwise constant or linear)
!
! Author:     M. Meindl
!
! Created:    07-May-2003
!
! Changes:    23-Sep-2008 RD: Skip non-sense tropo results (> 9m)
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! --------------------------------------------------------------------------


! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnloc, lfnerr, &
                      filenameLength, staNameLength
  USE d_trpest, ONLY: t_trpEst, t_trpRec

  USE s_opnfil
  USE s_alcerr
  USE f_djul
  USE s_opnerr
  USE s_exitrc
  USE s_gtflna
  IMPLICIT NONE


! List of parameters
! ------------------
! input
  CHARACTER(LEN=fileNameLength) :: filNam  ! Name of troposphere file
                                           ! =blank: use keyword TROPEST
! output
  TYPE(t_trpEst)                :: trpEst  ! Troposphere file structure
  INTEGER(i4b)                  :: irCode  ! Return code
                                           ! = 0: ok
                                           ! = 1: no file available

! Local variables
! ---------------
! general
  TYPE(t_trprec),                                                          &
            DIMENSION(:), ALLOCATABLE :: trpRec
  CHARACTER(LEN=staNameLength),                                            &
            DIMENSION(:), ALLOCATABLE :: staLst
  CHARACTER(LEN=1),                                                        &
            DIMENSION(:), ALLOCATABLE :: flgLst
  INTEGER(i4b),                                                            &
            DIMENSION(:), ALLOCATABLE :: idxLst
  CHARACTER(LEN=132)                  :: line
  CHARACTER(LEN=16)                   :: staNew
  CHARACTER(LEN=1)                    :: flg
  REAL(r8b),DIMENSION(3)              :: corr, sigma
  REAL(r8b)                           :: model, total
  REAL(r8b)                           :: grdTab
  LOGICAL                             :: filFlg, cont

! indices and loop variables
  INTEGER(i4b)                        :: nRec,iRec
  INTEGER(i4b)                        :: nSta,iSta
  INTEGER(i4b)                        :: nTrp,iTrp
  INTEGER(i4b)                        :: iPos

! time variables
  CHARACTER(LEN=19)                   :: timStr1, timStr2
  REAL(r8b), DIMENSION(2)             :: winTrp
  REAL(r8b)                           :: day
  INTEGER(i4b)                        :: iY, iM, iD
  INTEGER(i4b)                        :: iH, iMin, iS

! error codes
  INTEGER(i4b)                        :: irc, ioStat, iac




! Some initializations
! --------------------
  irCode = 0
  irc    = 0
  nSta   = 0
  nRec   = 0
  cont   = .FALSE.
  filFlg = .FALSE.


! Open troposphere file
! ---------------------
  IF (LEN_TRIM(filNam)==0) CALL gtflna(0,'TROPEST',filNam,irc)
  IF (irc==0) THEN
    CALL opnfil(lfnloc,filNam,'OLD',' ','READONLY',' ',ioStat)
    CALL opnerr(lfnerr,lfnloc,ioStat,filNam,'sr rdtrpe')


! Read header section of file
! ----------------------------
    READ(lfnloc,'(A80)') trpEst%head%title
    READ(lfnloc,'(/,A,///)') line

! piecwise constant or linear?
    trpEst%head%iTrGrd(2) = 0.d0
    IF (line(96:112)=='TABULAR INTERVAL:') THEN
      cont = .TRUE.
      READ(line,'(16X,I5,20X,I5,18X,I5,18X,I5,20X,F6.0)',IOSTAT=ioStat)    &
           trpEst%head%iTrpMd,trpEst%head%iTrMap,trpEst%head%iTrGrd(1),    &
           trpEst%head%iElvnq,trpEst%head%iTab
      IF (trpEst%head%iTrGrd(1)/=0) THEN
        READ(line(121:126),'(F6.0)') grdTab
        trpEst%head%iTrGrd(2) = nint(grdTab/trpEst%head%iTab)
      END IF
    ELSE
      READ(line,'(16X,I5,20X,I5,18X,I5,18X,I5)',IOSTAT=ioStat)             &
           trpEst%head%iTrpMd,trpEst%head%iTrMap,trpEst%head%iTrGrd(1),    &
           trpEst%head%iElvnq
      trpEst%head%iTab = 0.d0
    END IF
    IF (ioStat/=0) THEN
      WRITE(lfnerr,"(/,' *** SR RDTRPE: Error reading header line',/)")
      CALL exitrc(2)
    END IF
    trpEst%head%iTab  = trpEst%head%iTab/86400.d0
    trpEst%head%iFrmt = 1


! SUPPORT OLDER FORMAT WHERE IT WAS NOT KNOWN, WHETHER EXTRAPOLATED
! OR REAL METEO DATA WAS USED. ASSUMPTION: ONLY EXTRAPOL. METEO USED
! ------------------------------------------------------------------
    IF (line(1:17) == ' A PRIORI MODEL :') THEN
      trpEst%head%iTrpMd = -trpEst%head%iTrpMd
    ENDIF


! CHECK WHETHER A PRIORI TROPOSPHERE INFORMATION IS GIVEN IN
! TROPOSPHERE FILE (ITRPMD <= 0)
! ----------------------------------------------------------
    IF (trpEst%head%iTrpMd>0) THEN
      WRITE(lfnerr,&
      "(/,' *** SR RDTRPE: The troposphere file you use contains troposphere',&
                  & /,16X,'estimates from GPSEST/ADDNEQ but no information',&
                  & /,16X,'about the a priori model used at that time.',&
                  & /,16X,'Troposphere file with a positive model number is',&
                  & /,16X,'not supported.',&
                  & /,16X,'Troposphere model number : ',I4,&
                  & /,16X,'Troposphere file         : ',A,/)") &
                 trpEst%head%iTrpMd, TRIM(filNam)
      CALL exitrc(2)
    ENDIF


! SUPPORT FORMAT WHERE NO MAPPING CODE IS GIVEN (ITRMAP=0)
! --------------------------------------------------------
    IF (trpEst%head%iTrMap==0) trpEst%head%iTrMap = 1


! Read file to count records
! --------------------------
    CountLoop: DO
      READ(lfnloc,'(1X,A16,7X,A19,3X,A19)',IOSTAT=ioStat)                  &
           staNew,timStr1,timStr2
      IF (ioStat<0 .OR. LEN_TRIM(staNew)==0) EXIT CountLoop
      IF (ioStat>0) THEN
        WRITE(lfnerr,"(/,' *** SR RDTRPE: Error reading data line:',&
                                 & /,16X,'Record line:',I8,/)") nRec+1
        CALL exitrc(2)
      ENDIF
      nRec = nRec+1
    END DO CountLoop


! Allocate some memory
! --------------------
    IF (nRec>0) THEN
      ALLOCATE(staLst(nRec),STAT=iac)
      CALL alcerr(iac,'staLst',(/nRec/),'sr rdtrpe')
      ALLOCATE(flgLst(nRec),STAT=iac)
      CALL alcerr(iac,'flglst',(/nRec/),'sr rdtrpe')
      ALLOCATE(idxLst(nRec),STAT=iac)
      CALL alcerr(iac,'idxLst',(/nRec/),'sr rdtrpe')
      ALLOCATE(trpRec(nRec),STAT=iac)
      CALL alcerr(iac,'trpRec',(/nRec/),'sr rdtrpe')
    END IF


! Rewind file and skip header
! ---------------------------
    REWIND(lfnloc)
    READ(lfnloc,*)
    READ(lfnloc,'(////)')


! Read all records
! ----------------
    ReadLoop: DO iRec=1,nRec
      corr  = 0.d0
      sigma = 0.d0

! read line without gradients
      IF (trpEst%head%iTrGrd(1)==0) THEN
         READ (lfnloc,"(1X,A16,2X,1A,4X,A19,3X,A19,F9.4,F9.5,2F8.5)",      &
                                                        IOSTAT=ioStat)     &
               staNew,flg,timStr1,timStr2,model,corr(3),sigma(3),total

! Read Line with gradients
       ELSE
         READ (lfnloc,"(1X,A16,2X,1A,4X,A19,3X,A19,F9.4,F9.5,2F8.5,&
                                      & 2(F9.5,F8.5))",IOSTAT=ioStat)      &
               staNew,flg,timStr1,timStr2,model,corr(3),sigma(3),total,    &
               corr(1),sigma(1),corr(2),sigma(2)
       ENDIF

! exit on end or error
       IF (ioStat<0) EXIT ReadLoop
       IF (ioStat>0) THEN
         WRITE(lfnerr,"(/,' *** SR RDTRPE: Error reading data line:',&
                       & /,16X,'Record line:',I8,/)") iRec
         CALL exitrc(2)
       ENDIF


! Compute epoch/interval
! ----------------------
       READ(timStr1,'(I4,5(1X,I2))') iY,iM,iD,iH,iMin,iS
       day       = iD*1.d0+iH/24.d0+iMin/1440.d0+iS/86400.d0
       winTrp(1) = djul(iY,iM,day)

       IF (cont) THEN
         winTrp(2) = winTrp(1)+trpEst%head%iTab/86400.d0
       ELSE
         READ(timStr2,'(I4,5(1X,I2))') iY,iM,iD,iH,iMin,iS
         day       = iD*1.d0+iH/24.d0+iMin/1440.d0+iS/86400.d0
         winTrp(2) = djul(iY,iM,day)
       END IF


! Fill arrays
! -----------
       staLst(iRec)        = staNew
       flgLst(iRec)        = flg
       trpRec(iRec)%timInt = winTrp
       trpRec(iRec)%model  = model
       trpRec(iRec)%total  = total
       trpRec(iRec)%corr   = corr
       trpRec(iRec)%sigma  = sigma
    END DO ReadLoop


! Close file
! ----------
    CLOSE(lfnloc)


! Count number of stations
! ------------------------
    DO iRec=1,nRec
      iPos = 0

! station already in list?
      DO iSta=nSta,1,-1
        IF (staLst(iRec)==staLst(iSta)) THEN
          iPos = iSta
          EXIT
        END IF
      END DO

!new station
      IF (iPos==0) THEN
        nSta = nSta+1
        iPos = nSta
        staLst(nSta) = staLst(iRec)
      END IF

      idxLst(iRec) = iPos
    END DO


! Allocate some memory
! --------------------
    IF (nSta>0) THEN
      ALLOCATE(trpEst%sta(nSta),STAT=iac)
      CALL alcerr(iac,'trpEst%sta',(/nSta/),'sr rdtrpe')
    END IF
    trpEst%nSta = nSta


! Count records for each station
! ------------------------------
    DO iSta=1,nSta
      nTrp = 0
      DO iRec=1,nRec
        IF (idxLst(iRec)==iSta) nTrp = nTrp+1
      END DO


! Allocate memory
! ---------------
      IF (nTrp>0) THEN
        ALLOCATE(trpEst%sta(iSta)%trp(nTrp),STAT=iac)
        CALL alcerr(iac,'trpEst%sta%trp',(/nTrp/),'sr rdtrpe')


! Save array
! ----------
        iTrp = 0
        DO iRec=1,nRec
          IF (idxLst(iRec)==iSta) THEN

            ! Ignore outliers
            IF (DABS(trpRec(iRec)%model)    > 9.0d0) CYCLE
            IF (DABS(trpRec(iRec)%total)    > 9.0d0) CYCLE
            IF (DABS(trpRec(iRec)%corr(1))  > 9.0d0) CYCLE
            IF (DABS(trpRec(iRec)%corr(2))  > 9.0d0) CYCLE
            IF (DABS(trpRec(iRec)%corr(3))  > 9.0d0) CYCLE
            IF (DABS(trpRec(iRec)%sigma(1)) > 9.0d0) CYCLE
            IF (DABS(trpRec(iRec)%sigma(2)) > 9.0d0) CYCLE
            IF (DABS(trpRec(iRec)%sigma(3)) > 9.0d0) CYCLE

            iTrp = iTrp+1
            IF (iTrp==1) THEN
              trpEst%sta(iSta)%staNam = staLst(iSta)
              trpEst%sta(iSta)%staFlg = flgLst(iSta)
            END IF
            trpEst%sta(iSta)%trp(iTrp) = trpRec(iRec)
          END IF
        END DO
        trpEst%sta(iSta)%nTrp = iTrp
      END IF
    END DO
    trpEst%nRec = nRec


! Deallocate some memory
! ----------------------
    IF (nRec>0) THEN
      DEALLOCATE(staLst)
      DEALLOCATE(flgLst)
      DEALLOCATE(idxLst)
      DEALLOCATE(trpRec)
    END IF


! No file found
! -------------
  ELSE
    trpEst%nRec = 0
    trpEst%nSta = 0
    irCode = 1
  END IF


! Subroutine ends here
! --------------------
  RETURN
END SUBROUTINE rdtrpe

END MODULE
