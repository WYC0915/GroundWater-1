MODULE s_RDFMTH2
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE rdFmth2(lfnRdf,head)

! -------------------------------------------------------------------------
! Purpose:   Read the entire header of a formatted observation
!            file (phase or code, zero or single differences)
!
!
! Remarks:   Updated version of SR RDFMTH.f
!
! Changes in old SR:
!            10-AUG-1994 : MR: CALL EXITRC
!            12-AUG-1994 : MR: FORMAT 4: SESSION AS CHARACTER*4
!            18-JAN-1995 : MR: INITIALIZATION OF CSESS(2)
!            28-SEP-1995 : WG: INCLUDE RANGE OBSREVATION TYPE
!
! Author:    M.Rothacher
!
! Created:   10-Jul-2002
!
! Changes:   04-Oct-2002 RD: Corrected format statement
!            17-Feb-0203 LM: Use m_maxdim
!            24-Apr-2003 RD: Corrected format statement
!            16-May-2003 HU: Deallocate arrays
!            21-May-2003 RD: Make the deallocation safe
!            08-Sep-2003 HU: Format 5: antnam, recnam, oprnam chr16 -> chr20
!            10-Sep-2003 HU: stanam is chr16
!            24-May-2004 HU: Read format 4, sampling rate is R8 for V5,
!                            do not set ifrmat
!            18-Aug-2005 HU: Epoch and nEpoFlag is I7
!            26-Jan-2011 LP: Sat.-specific obstypes, format 6
!            30-Apr-2012 LP: Dimension of obstyp changed (4->8)
!
! SR used:
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
! --------------------------------------------------------------------------
! Modules
! -------
  USE m_bern
  USE m_maxdim, ONLY : maxsat, maxamb
  USE d_gpsObs, ONLY : t_obsHead
  USE s_alcerr
  USE f_djul
  USE s_exitrc
  IMPLICIT NONE


! List of parameters
! ------------------
! input:
  INTEGER(i4b)    :: lfnRdf
  TYPE(t_obsHead) :: head

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER   :: srName = 'rdFmth'
  CHARACTER(LEN=8),DIMENSION(3),PARAMETER :: mTypCh = (/'PHASE   ','CODE    ','RANGE   '/)
  CHARACTER(LEN=3),DIMENSION(maxSat,4):: obshelp

! Local Variables
! ---------------
  CHARACTER(LEN=80),DIMENSION(maxAmb) :: string
  CHARACTER(LEN=16),DIMENSION(2)      :: clkTxt
  CHARACTER(LEN=8 ) :: mTypC1

  REAL(r8b)    :: day
  REAL(r8b)    :: rDeltt

  INTEGER(i4b) :: iac
  INTEGER(i4b) :: iSec,iMin,iHour,iDay,iMonth,iYear
  INTEGER(i4b) :: iAmb,iAmb1
  INTEGER(i4b) :: iSat
  INTEGER(i4b) :: iSta
  INTEGER(i4b) :: iFrq,nFrq
  INTEGER(i4b) :: ii,nn
  INTEGER(i4b) :: iTyp
  INTEGER(i4b) :: iErr

! Lines 1,2: Campaign, title
! --------------------------
  READ(lfnRdf,'(A16,3X,A53,/)') head%campgn,head%title

! Lines 3-5: Measurement type, creation date, reference epoch
! -----------------------------------------------------------
  READ(lfnRdf,'(19X,A8,30X,A9,1X,A5,/19X,I4,2(1X,I2),I3,2(1X,I2),19X,A9,1X,A5,/)')&
       mTypC1,head%crDate(1),head%crTime(1),iYear,iMonth,iDay,&
       iHour,iMin,iSec,head%crDate(2),head%crTime(2)

! Measurement type
  iErr = 1
  DO iTyp=1,3
    IF(mTypC1 == mTypCH(iTyp)) THEN
      iErr = 0
      EXIT
    ENDIF
  ENDDO

! Illegal measurement type
  IF (iErr /= 0) THEN
    WRITE(lfnErr,'(/,A,A8,/,16X,A,I2,/)') &
         ' *** SR rdFmth: Illegal measurement type: ',mTypC1,&
                         'Logical file number     : ',lfnRdf
    CALL EXITRC(2)
  ENDIF

  head%meaTyp=iTyp

! Reference epoch (MJD)
  day=iDay+iHour/24.D0+iMin/1440.D0+iSec/86400.D0
  head%timRef=DJUL(iYear,iMonth,day)

! Lines 6-11: General information
! -------------------------------
  head%cSess(2)=' '
  READ(lfnRdf,'(19X,I5,33X,I5)')&
       head%nDiff,head%iFrmat

  IF(head%iFrmat<5) THEN
    READ(lfnRdf,'(19X,I5,34X,A4,/,19X,I5,37X,A1,&
         &/,17X,I7,33X,I5,/,17X,I7,33X,I5,/)')&
         head%nFreq,head%cSess(1),head%nSatel,&
         head%cSess(2)(1:1),head%nEpoch,head%iDeltt,head%nEpFlg,head%irMark
  ELSE
    READ(lfnRdf,'(19X,I5,34X,A4,/,19X,I5,37X,A1,&
         &/,17X,I7,33X,F12.6,/,17X,I7,33X,I5,/)')&
         head%nFreq,head%cSess(1),head%nSatel,&
         head%cSess(2)(1:1),head%nEpoch,rDeltt,head%nEpFlg,head%irMark
    head%iDeltt = IDNINT(rDeltt)
    IF (DABS(rDeltt-head%iDeltt) > 1D-6) THEN
      WRITE(lfnErr,'(/,A,/,17X,A,F12.6,/,17X,A,I5,/)') &
          ' *** SR RDFMTH2: Sampling rate larger than 1Hz found. Not supported',&
                           'Sampling (sec)          : ',rDeltt,&
                           'Logical file number     : ',lfnRdf
      CALL exitrc(2)
    ENDIF
  ENDIF

! Illegal number of differences
  IF(head%nDiff < 0.OR.head%nDiff > 1) THEN
    WRITE(lfnErr,'(/,A,I5,/,16X,A,I5,/)')&
             ' *** SR rdfmth: Illegal number of differences: ',head%nDiff,&
                             'logical file number          : ',lfnRdf
    CALL EXITRC(2)
  ENDIF

! Illegal format number
  IF(head%iFrmat /= 5 .AND. head%iFrmat /= 4 .AND. head%iFrmat /= 6) THEN
    WRITE(lfnErr,'(/,A,I5,/,16X,A,I5,/)')&
             ' *** SR rdfmth: Illegal format number: ',head%iFrmat,&
                             'logical file number  : ',lfnRdf
    CALL EXITRC(2)
  ENDIF

! Illegal number of frequencies
  IF(head%nFreq < 1.OR.head%nFreq > 2) THEN
    WRITE(lfnErr,'(/,A,I5,/,16X,A,I5,/)') &
             ' *** SR rdFmth: Illegal number of frequencies: ',head%nFreq,&
                             'Logical file number          : ',lfnRdf
    CALL EXITRC(2)
  ENDIF

! Too many satellites
  IF(head%nSatel > maxSat) THEN
    WRITE(lfnErr,'(/,A,I5,/,16X,A,I5,/,16X,A,I5,/)')&
             ' *** SR rdFmth: Too many satellites: ',head%nSatel,&
                             'Maximum # of satel.: ',maxSat,&
                             'Logical file number: ',lfnRdf
    CALL EXITRC(2)
  ENDIF

! Illegal observation interval
  IF(head%iDeltt < 1) THEN
    WRITE(lfnErr,'(/,A,I5,/,16X,A,I5,/)')&
             ' *** SR rdFmth: Illegal observation interval: ',head%iDeltt,&
                             'Logical file number         : ',lfnRdf
    CALL EXITRC(2)
  ENDIF

! Lines 12-17: Station and receiver information
! ---------------------------------------------
  nn=head%nDiff+1
  READ(lfnRdf,'(19X,A16,12X,A16)') head%sta(1:nn)%staNam
  IF(head%iFrmat==4) THEN
    READ(lfnRdf,'(19X,A16,12X,A16)')  head%sta(1:nn)%oprNam
    READ(lfnRdf,'(19X,A16,12X,A16)')  head%sta(1:nn)%recTyp
    READ(lfnRdf,'(19X,A16,12X,A16)')  head%sta(1:nn)%antTyp
  ELSE
    READ(lfnRdf,'(19X,A20,8X,A20)')  head%sta(1:nn)%oprNam
    READ(lfnRdf,'(19X,A20,8X,A20)')  head%sta(1:nn)%recTyp
    READ(lfnRdf,'(19X,A20,8X,A20)')  head%sta(1:nn)%antTyp
  ENDIF
  READ(lfnRdf,'(19X,I6,4X,I6,12X,I6,4X,I6)') (head%sta(ii)%irUnit,&
       &head%sta(ii)%iAnten,ii=1,nn)

  READ(lfnRdf,'(1X)')

! Lines 18-19: Clock corrections
! ------------------------------
  READ(lfnRdf,'(19X,A16,12X,A16)') clkTxt(1:nn)
  READ(lfnRdf,'(1X)')
  DO iSta=1,nn
    IF(clkTxt(iSta)(1:4) == 'NONE') THEN
      head%sta(iSta)%iClock = 0
    ELSEIF(clkTxt(iSta)(1:4) == 'POLY') THEN
      READ(clkTxt(iSta),'(14X,I2)') head%sta(iSta)%iClock
      head%sta(iSta)%iClock = head%sta(iSta)%iClock+1
    ELSEIF(clkTxt(iSta)(1:4) == 'OFFS') THEN
      head%sta(iSta)%iClock = 999
    ELSE
      WRITE(lfnErr,'(/,A,A16,/,16X,A,I5,/)')&
           ' *** SR rdFmth: Illegal clock type : ',clkTxt(iSta),&
           'Logical file number: ',lfnRdf
      CALL EXITRC(2)
    ENDIF
  ENDDO

! Lines 20-21: Positioning eccentricities
! ---------------------------------------
  READ(lfnRdf,'(18X,3F8.4,4X,3F8.4)') (head%sta(iSta)%posEcc(1:3),iSta=1,nn)
  READ(lfnRdf,'(1X)')


! Allocate memory for head%sat
! ----------------------------
  IF (ASSOCIATED(head%sat)) &
       DEALLOCATE(head%sat,stat=iac)
  ALLOCATE(head%sat(head%nSatel),stat=iac)
  CALL alcerr(iac,'head%sat',(/head%nSatel/),srName)

! Number of ambiguities and number of observations
! ------------------------------------------------
  DO ii = 1,8
   head%sat(1:head%nSatel)%obstyp(ii) = '   '
  ENDDO

  DO iSat=1,maxsat
    DO ii = 1,4
      obshelp(iSat,ii) = '   '
    ENDDO
  ENDDO

  READ(lfnRdf,'(1X)')
  DO iSat=1,head%nSatel
   IF(head%iFrmat<6) THEN
    READ(lfnRdf,'(I3,7X,I5,7X,I5,8X,I5,7X,I5)') &
         head%sat(iSat)%numSat,&
         (head%sat(iSat)%numObs(ii),head%sat(iSat)%numMrk(ii),ii=1,head%nFreq)
   ELSE
    IF(head%nFreq == 1) THEN
      READ(lfnRdf,'(I3,7X,I5,7X,I5,10X,A3,7X,A3,7X,A3,7X,A3)') &
         head%sat(iSat)%numSat,&
         (head%sat(iSat)%numObs(ii),head%sat(iSat)%numMrk(ii),ii=1,head%nFreq),&
         (obshelp(iSat,ii),ii=1,4)

    ELSE
      READ(lfnRdf,'(I3,7X,I5,7X,I5,8X,I5,7X,I5,10X,A3,7X,A3,7X,A3,7X,A3)') &
         head%sat(iSat)%numSat,&
         (head%sat(iSat)%numObs(ii),head%sat(iSat)%numMrk(ii),ii=1,head%nFreq),&
         (obshelp(iSat,ii),ii=1,4)
    ENDIF
    IF (head%meaTyp==1) THEN
     head%sat(iSat)%obstyp(3) = obshelp(iSat,1)
     head%sat(iSat)%obstyp(4) = obshelp(iSat,2)
     head%sat(iSat)%obstyp(7) = obshelp(iSat,3)
     head%sat(iSat)%obstyp(8) = obshelp(iSat,4)
    ENDIF
    IF (head%meaTyp==2) THEN
     head%sat(iSat)%obstyp(1) = obshelp(iSat,1)
     head%sat(iSat)%obstyp(2) = obshelp(iSat,2)
     head%sat(iSat)%obstyp(5) = obshelp(iSat,3)
     head%sat(iSat)%obstyp(6) = obshelp(iSat,4)
    ENDIF
   ENDIF


! Illegal satellite number (wrong number of satellites ?)
   IF(head%sat(iSat)%numSat <= 0) THEN
      WRITE(lfnErr,'(/,A,I4,/,16X,A,I4,/,16X,A,/)') &
               ' *** SR rdFmth: Illegal satellite number:',head%sat(iSat)%numSat,&
                               'Logical file number     :',lfnRdf,&
                               'Inconsistency in formatted file ?'
      CALL EXITRC(2)
   ENDIF
  ENDDO
  READ(lfnRdf,'(1X)')

! Ambiguities and observation title
! ---------------------------------
  READ(lfnRdf,'(A)') string(1)
  IF (string(1)(1:8) /= ' AMB SAT') THEN
    head%numAmb=0
    READ(lfnRdf,'(1X)')
  ELSE
    IF(head%nFreq == 1) THEN
      nFrq=1
    ELSE
      nFrq=3
    ENDIF
    head%numAmb = 0
    DO iAmb=1,100000
      READ(lfnRdf,'(A)') string(iAmb)
      IF (string(iAmb) == ' ') EXIT

! Too many ambiguities
      IF(iAmb > maxAmb) THEN
        WRITE(lfnErr,'(/,A,I4,/,16X,A,I4,/,16X,A,I4,/)')&
                 ' *** SR rdFmth: Too many ambiguities >=',iAmb,&
                                 'Maximum # of ambig.   :',maxAmb,&
                                 'Logical file number   :',lfnRdf
        CALL EXITRC(2)
      ENDIF

      head%numAmb = head%numAmb + 1
    ENDDO

! Allocate memory for ambiguities
! -------------------------------
    IF (ASSOCIATED(head%ambigu)) &
          DEALLOCATE(head%ambigu,stat=iac)
    ALLOCATE(head%ambigu(head%numAmb),stat=iac)
    CALL alcErr(iac,'head%ambigu',(/head%numAmb/),srName)

    DO iAmb = 1,head%numAmb
      IF(head%nFreq == 1) THEN
        READ(string(iAmb),'(2I4,I6,3X,I1,1X,F15.0,I5)')&
             iAmb1,head%ambigu(iAmb)%ambSat,head%ambigu(iAmb)%ambIep,&
             head%ambigu(iAmb)%ambWlf(1:head%nFreq),&
             (head%ambigu(iAmb)%ambigu(iFrq),head%ambigu(iAmb)%ambCls(iFrq),iFrq=1,nFrq)
      ELSE
        READ(string(iAmb),'(2I4,I6,2X,I1,1X,I1,3(F15.0,I5))')&
             iAmb1,head%ambigu(iAmb)%ambSat,head%ambigu(iAmb)%ambIep,&
             head%ambigu(iAmb)%ambWlf(1:head%nFreq),&
             (head%ambigu(iAmb)%ambigu(iFrq),head%ambigu(iAmb)%ambCls(iFrq),iFrq=1,nFrq)
      ENDIF

! Illegal wavelength factors
      DO iFrq=1,head%nFreq
        IF(head%ambigu(iAmb)%ambWlf(iFrq) < 1.OR.head%ambigu(iAmb)%ambWlf(iFrq) > 2) THEN
          WRITE(lfnErr,'(/,A,I3,/,16X,A,I3,/,16X,A,I3,/,16X,A,I3,/)')&
                   ' *** SR rdFmth: Illegal wavelength factor:',head%ambigu(iAmb)%ambWlf(iFrq),&
                                   'Ambiguity number         :',iAmb,&
                                   'Frequency                :',iFrq,&
                                   'Logical file number      :',lfnRdf
          CALL EXITRC(2)
        ENDIF
      ENDDO
    ENDDO

    READ(lfnRdf,'(1X)')
    READ(lfnRdf,'(1X)')
  ENDIF

  RETURN
END SUBROUTINE rdFmth2

END MODULE
