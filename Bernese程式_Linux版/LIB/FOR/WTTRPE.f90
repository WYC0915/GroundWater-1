MODULE s_WTTRPE
CONTAINS

! --------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! --------------------------------------------------------------------------

SUBROUTINE wttrpe(filNam,trpEst,irCode)

! --------------------------------------------------------------------------
! Purpose:    Write a BERNESE troposphere file (piecwise linear)
!
! Author:     M. Meindl
!
! Created:    07-May-2003
! Last mod.:  23-Sep-2008
!
! Changes:    08-Aug-2005 HB: Use new SR TIMST2 (module)
!             23-Sep-2008 RD: Write 9.99999 for non-sense TRP results
!
! SR used:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! --------------------------------------------------------------------------


! Modules
! -------
  USE m_bern
  USE d_trpest, ONLY: t_trpest,t_trprec

  USE s_exitrc
  USE s_gtflna
  USE s_opnerr
  USE s_opnfil
  USE s_timst2
  IMPLICIT NONE


! List of parameters
! ------------------
! input
  CHARACTER(LEN=fileNameLength) :: filNam  ! file name, blank: use GTFLNA
                                           ! mechanism with keyword TROPSAV
  TYPE(t_trpest)                :: trpEst  ! Structure of troposphere file

! output
  INTEGER(i4b)                  :: irCode  ! Return code of this SR
                                           ! = 1: No filename specified,
                                           !      file not written

! Local Variables
! ---------------
! general
  TYPE(t_trprec)                   :: trpHlp

  CHARACTER(LEN=19),DIMENSION(2)   :: epoStr
  INTEGER(i4b)                     :: iTab

! indices
  INTEGER(i4b)                     :: ii
  INTEGER(i4b)                     :: iRec, iSta, iTrp

! error codes
  INTEGER(i4b)                     :: irc, ioStat




! Some initializations
! --------------------
  irCode = 0
  irc    = 0


! Open file
! ----------
  IF (LEN_TRIM(filNam)==0) CALL gtflna(0,'TROPSAV',filNam,irc)
  IF (LEN_TRIM(filNam)==0 .OR. irc/=0) THEN
    irCode = 1
    RETURN
  END IF

  CALL opnfil(lfnloc,filNam,'UNKNOWN',' ', ' ',' ',ioStat)
  CALL opnerr(lfnerr,lfnloc,ioStat,filNam,'sr wttrpe')


! Write header section
! --------------------
! piecewise constant
  IF (trpEst%head%iTab==0.d0) THEN
    write(lfnloc,"(A,/,133('-'), &
         &/,' A PRIORI MODEL:',I5,'   MAPPING FUNCTION:',I5, &
         &  '   GRADIENT MODEL:',I5,'   MIN. ELEVATION:',I5,//, &
         &  ' STATION NAME     FLG   YYYY MM DD HH MM SS   ', &
         &  'YYYY MM DD HH MM SS   MOD_U   CORR_U  SIGMA_U ', &
         &  'TOTAL_U  CORR_N  SIGMA_N  CORR_E  SIGMA_E',/)",               &
         IOSTAT=ioStat)                                                    &
            trpEst%head%title,trpEst%head%iTrpMd,trpEst%head%iTrMap,       &
            trpEst%head%iTrGrd(1),trpEst%head%iElvnq

! piecewise linear
  ELSE
    iTab = nint(trpEst%head%iTab*86400)
    write(lfnloc,"(A,/,133('-'), &
         &/,' A PRIORI MODEL:',I5,'   MAPPING FUNCTION:',I5,  &
         &  '   GRADIENT MODEL:',I5,'   MIN. ELEVATION:',I5,  &
         &  '   TABULAR INTERVAL:',I6,' /',I6,//,             &
         &  ' STATION NAME     FLG   YYYY MM DD HH MM SS   ', &
         &  'YYYY MM DD HH MM SS   MOD_U   CORR_U  SIGMA_U ', &
         &  'TOTAL_U  CORR_N  SIGMA_N  CORR_E  SIGMA_E',/)",               &
         IOSTAT=ioStat)                                                    &
            trpEst%head%title,trpEst%head%iTrpMd,trpEst%head%iTrMap,       &
            trpEst%head%iTrGrd(1),trpEst%head%iElvnq,                      &
            iTab,trpEst%head%iTrGrd(2)*iTab
  END IF

  IF (ioStat/=0) THEN
    write(lfnerr,"(/,' *** SR WTTRPE: Error writing header lines',/)")
    CALL exitrc(2)
  ENDIF


! Write records
! -------------
  iRec = 0
  StationLoop: DO iSta=1,trpEst%nSta
    WriteLoop: DO iTrp=1,trpEst%sta(iSta)%nTrp
      iRec = iRec+1

! Skip parameter if epoch=0.d0
      IF (trpEst%sta(iSta)%trp(iTrp)%timInt(1)==0.d0) CYCLE WriteLoop

! Start and end of window
      epoStr = " "
      CALL TIMST2(2,1,trpEst%sta(iSta)%trp(iTrp)%timInt(1),epoStr(1))
      IF (trpEst%head%iTab==0.d0)                                          &
           CALL TIMST2(2,1,trpEst%sta(iSta)%trp(iTrp)%timInt(2),epoStr(2))

! Copy the currect record, check for outliers
      trpHlp = trpEst%sta(iSta)%trp(iTrp)
      IF (DABS(trpHlp%model) > 9.9d0) trpHlp%model = 9.99999d0
      IF (DABS(trpHlp%total) > 9.9d0) trpHlp%total = 9.99999d0
      DO ii = 1,3
        IF (DABS(trpHlp%corr(ii) ) > 9.9d0) trpHlp%corr(ii)  = 9.99999d0
        IF (DABS(trpHlp%sigma(ii)) > 9.9d0) trpHlp%sigma(ii) = 9.99999d0
      ENDDO

! Write line without gradients
      IF (trpEst%head%iTrGrd(1)==0) THEN
        write(lfnloc,"(1X,A16,2X,1A,4X,A19,3X,A19,F9.4,F9.5,2F8.5)",       &
                                                       IOSTAT=ioStat)      &
              trpEst%sta(iSta)%staNam,trpEst%sta(iSta)%staFlg,epoStr,      &
              trpHlp%model,trpHlp%corr(3),trpHlp%sigma(3),trpHlp%total

! Write line with gradients
      ELSE
        write(lfnloc,"(1X,A16,2X,1A,4X,A19,3X,A19,F9.4,F9.5,2F8.5, &
                                      & 2(F9.5,F8.5))",IOSTAT=ioStat)      &
              trpEst%sta(iSta)%staNam,trpEst%sta(iSta)%staFlg,epoStr,      &
              trpHlp%model,trpHlp%corr(3),trpHlp%sigma(3),trpHlp%total,    &
              trpHlp%corr(1),trpHlp%sigma(1),trpHlp%corr(2),trpHlp%sigma(2)
      ENDIF

      IF (ioStat /= 0) THEN
        write(lfnerr,"(/,' *** SR WTTRPE: Error writing data line:',&
                       & /,16X,'Record line:',I8,/)") iRec
        CALL exitrc(2)
      ENDIF

    ENDDO WriteLoop
  ENDDO StationLoop


! Close file
! ----------
  CLOSE (lfnloc)


! Subroutine ends here
! --------------------
  RETURN
END SUBROUTINE wttrpe

END MODULE
