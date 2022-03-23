MODULE s_WTFMTH2
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE wtfmth2(lfnWtf,head)

! -------------------------------------------------------------------------
! Purpose:   Write the entire header of a formatted observation
!            file (phase or code, zero or single differences)
!
! Remarks:   Updated version of SR WTFMTH.f
!
! Changes in old SR:
!            12-Aug-1994 : MR: FORMAT 4: SESSION AS CHARACTER*4
!            28-Sep-1995 : WG: INCLUDE RANGE OBSREVATION TYPE
!
! Author:    M.Rothacher
!
! Created:   10-Jul-2002
!
! Changed:   19-Feb-2003 HU: Prevent overflow in antenna/receiver number
!            08-Sep-2003 HU: antnam, recnam, oprnam chr16 -> chr20
!            10-Sep-2003 HU: stanam is chr16
!            24-May-2004 HU: Sampling rate is R8 for format 5, set ifrmat=5
!            18-Aug-2005 HU: Epoch and nEpoFlag is I7
!            15-Aug-2006 HB: Shift description line for observations one
!                            character
!            26-Jan-2011 LP: Sat-specific obstypes, format 6
!            30-Apr-2012 LP: Dimension of obstyp changed (4->8)
!
! SR used:   DJUL  , JMT   , RADGMS
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
! ----------------------------------------------------------------------------
! Modules
! -------
  USE m_bern
  USE m_maxdim, ONLY : maxsat
  USE d_gpsObs, ONLY : t_obsHead

  USE f_djul
  USE s_jmt
  USE s_radgms
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  INTEGER(i4b)    :: lfnWtf
  TYPE(t_obsHead) :: head

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER    :: srName = 'wtFmth'
  CHARACTER(LEN=24),DIMENSION(2),PARAMETER :: obTxt3 = &
       &(/'FRACT.(S), CLOCK (S)    ','FR+CL 1 (S), FR+CL 2 (S)'/)
  CHARACTER(LEN=20),DIMENSION(2),PARAMETER :: obTxt1 = &
       &(/'L1 OBSERVATIONS:    ','L1,L2 OBSERVATIONS: '/)
  CHARACTER(LEN=20),DIMENSION(3),PARAMETER :: obTxt2 = &
       &(/'   PHASE (M)  FFS SA','    CODE (M)  FFS SA','   RANGE (M)  FFS SA'/)
  CHARACTER(LEN= 8),DIMENSION(3),PARAMETER :: mTypCh = &
       &(/'PHASE   ','CODE    ','RANGE   '/)

! Local Variables
! ---------------
  CHARACTER(LEN=25)              :: epoTxt
  CHARACTER(LEN=16),DIMENSION(2) :: clkTxt
  CHARACTER(LEN= 1)              :: vorz
  CHARACTER(LEN=3),DIMENSION(maxSat,4):: obshelp

  REAL(r8b)    :: day
  REAL(r8b)    :: sec

  INTEGER(i4b) :: iYear,iMonth,iDay
  INTEGER(i4b) :: iHour,iMin,iSec
  INTEGER(i4b) :: iDayYr
  INTEGER(i4b) :: ii,nn
  INTEGER(i4b) :: iSat
  INTEGER(i4b) :: iSta
  INTEGER(i4b) :: iFrq,nFrq
  INTEGER(i4b) :: iAmb
  INTEGER(i4b),DIMENSION(2) :: irUnit,iAnten

! SET FORMAT VERSION TO 6
! -----------------------
  head%ifrmat=6

! LINES 1,2: CAMPAIGN, TITLE
! --------------------------
  WRITE(lfnWtf,'(A16,A3,A53,/)') head%campgn,':  ',head%title

! LINES 3-5: MEASUREMENT TYPE, CREATION DATE, REFERENCE EPOCH
! -----------------------------------------------------------
  CALL jmt(head%timRef,iYear,iMonth,day)
  day  = day+1.D-8
  iday = IDINT(day)
  CALL radgms(3,day,vorz,iHour,iMin,sec)
  iSec = IDNINT(sec)
  iDayYr = head%timRef-DJUL(iYear,1,1.D0)+1.D0

  WRITE(epoTxt,"(I4,2('-',I2),I3,2(':',I2),' (',I3,')')") &
       iYear,iMonth,iDay,iHour,iMin,iSec,iDayYr

  IF(epoTxt( 6: 6) == ' ') epoTxt( 6: 6)='0'
  IF(epoTxt( 9: 9) == ' ') epoTxt( 9: 9)='0'
  IF(epoTxt(15:15) == ' ') epoTxt(15:15)='0'
  IF(epoTxt(18:18) == ' ') epoTxt(18:18)='0'
  IF(epoTxt(22:22) == ' ') epoTxt(22:22)='0'
  IF(epoTxt(23:23) == ' ') epoTxt(23:23)='0'
  WRITE(lfnWtf,'(A,A8,19X,A,A9,1X,A5,/,A,A25,2X,A,A9,1X,A5,/)')&
       'MEASUREMENT TYPE:  ',mTypCh(head%meaTyp),&
       'CREATED :  ',head%crDate(1),head%crTime(1),&
       'REFERENCE EPOCH :  ',epoTxt,&
       'MODIFIED:  ',head%crDate(2),head%crTime(2)

! LINES 6-11: GENERAL INFORMATION
! -------------------------------
  WRITE(lfnWtf,'(A,I5,11X,A,I5,/,A,I5,11X,A,A4,/,A,I5,11X,A,A1,/,&
         &A,I7,11X,A,F12.6,/,A,I7,11X,A,I5,/)')&
         '# DIFFERENCES   :  ',head%nDiff, 'FORMAT NUMBER      :  ',head%iFrmat,&
         '# FREQUENCIES   :  ',head%nFreq, 'SESSION IDENTIFIER :   ',head%cSess(1),&
         '# SATELLITES    :  ',head%nSatel,'SUBSESSION IDENTIF.:      ',head%cSess(2)(1:1),&
         '# EPOCHS        :'  ,head%nEpoch,'OBS. INTERVAL (S)  :  ',DBLE(head%iDeltt),&
         '# FLAGGED EPOCHS:'  ,head%nEpflg,'REMARK NUMBER      :  ',head%irMark

! LINES 12-17: STATION AND RECEIVER INFORMATION
! ---------------------------------------------
  nn=head%nDiff+1

! check for overflow
  DO ii=1,nn
    irUnit(ii)=head%sta(ii)%irUnit
    IF (irUnit(ii)<0.OR.irUnit(ii)>999999) THEN
      WRITE(lfnerr,"(/,' ### SR WTFMTH2: Receiver number overflow,',&
                   &                   ' number set to zero', &
                   & /,'                 Station        : ',A,       &
                   & /,'                 Receiver number:',I15,/)") &
                         head%sta(1:nn)%staNam,head%sta(ii)%irUnit
      irUnit(ii)=0
    ENDIF
    iAnten(ii)=head%sta(ii)%iAnten
    IF (iAnten(ii)<0.OR.iAnten(ii)>999999) THEN
      WRITE(lfnerr,"(/,' ### SR WTFMTH2: Antenna number overflow,',&
                   &                   ' number set to zero', &
                   & /,'                 Station       : ',A,       &
                   & /,'                 Antenna number:',I15,/)") &
                         head%sta(1:nn)%staNam,head%sta(ii)%iAnten
      iAnten(ii)=0
    ENDIF
  ENDDO

  WRITE(lfnWtf,'(A,A16,12X,A16)')&
       'STATION NAME    :  ',head%sta(1:nn)%staNam
  WRITE(lfnWtf,'(A,A20,8X,A20)')&
       'OPERATOR NAME   :  ',head%sta(1:nn)%oprNam
  WRITE(lfnWtf,'(A,A20,8X,A20)')&
       'RECEIVER TYPE   :  ',head%sta(1:nn)%recTyp
  WRITE(lfnWtf,'(A,A20,8X,A20)')&
       'ANTENNA TYPE    :  ',head%sta(1:nn)%antTyp
  WRITE(lfnWtf,'(A,I6,"  / ",I6,12X,I6,"  / ",I6)')&
       'RECEIVER/ANTENNA:  ',(irUnit(ii),iAnten(ii),ii=1,nn)
  WRITE(lfnWtf,"(' ')")

! LINES 18-19: CLOCK CORRECTIONS
! ------------------------------
  DO iSta=1,nn
    IF(head%sta(iSta)%iClock == 0) THEN
      clkTxt(iSta) = 'NONE'
    ELSE IF(head%sta(iSta)%iClock == 999) THEN
      clkTxt(iSta) = 'OFFSET PER EPOCH'
    ELSE
      WRITE(clkTxt(iSta),'(A,I2)')'POLYNOMIAL DEG',head%sta(iSta)%iClock-1
    ENDIF
  ENDDO

  WRITE(lfnWtf,'(A,A16,12X,A16)')'CLOCK CORRECTION:  ',clkTxt(1:nn)
  WRITE(lfnWtf,"(' ')")

! LINES 20-21: POSITIONING ECCENTRICITIES
! ---------------------------------------
  WRITE(lfnWtf,'(A,3F8.4,4X,3F8.4)')&
       'POS.ECCENTR. (M): ',(head%sta(ii)%posEcc(1:3),ii=1,nn)

! NUMBER OF OBSERVATIONS
! ----------------------
  IF(head%nFreq == 1) THEN
    WRITE(lfnWtf,'(A)')'                                    Station1            Station2'
    WRITE(lfnWtf,'(A)')'SAT    #L1-OBS OK  #L1-OBS BAD  Obstype1  Obstype2  Obstype1  Obstype2'
  ELSE
    WRITE(lfnWtf,'(A)')'                                                             Station1            Station2'
    WRITE(lfnWtf,'(A)')'SAT    #L1-OBS OK  #L1-OBS BAD  #L2-OBS OK  #L2-OBS BAD  Obstype1  Obstype2  Obstype1  Obstype2'
  ENDIF

  DO iSat=1,maxsat
    DO ii = 1,4
      obshelp(iSat,ii) = '   '
    ENDDO
  ENDDO

  DO iSat=1,head%nSatel
   IF (head%meaTyp==1) THEN
     obshelp(iSat,1) = head%sat(iSat)%obstyp(3)
     obshelp(iSat,2) = head%sat(iSat)%obstyp(4)
     obshelp(iSat,3) = head%sat(iSat)%obstyp(7)
     obshelp(iSat,4) = head%sat(iSat)%obstyp(8)
   ENDIF
   IF (head%meaTyp==2) THEN
     obshelp(iSat,1) = head%sat(iSat)%obstyp(1)
     obshelp(iSat,2) = head%sat(iSat)%obstyp(2)
     obshelp(iSat,3) = head%sat(iSat)%obstyp(5)
     obshelp(iSat,4) = head%sat(iSat)%obstyp(6)
   ENDIF

   IF(head%nFreq == 1) THEN
     WRITE(lfnWtf,'(I3,7X,I5,7X,I5,10X,A3,7X,A3,7X,A3,7X,A3)')&
         head%sat(iSat)%numSat,&
         (head%sat(iSat)%numObs(ii),head%sat(iSat)%numMrk(ii),ii=1,head%nFreq),&
         (obshelp(iSat,ii),ii=1,4)
   ELSE
     WRITE(lfnWtf,'(I3,7X,I5,7X,I5,8X,I5,7X,I5,10X,A3,7X,A3,7X,A3,7X,A3)')&
         head%sat(iSat)%numSat,&
         (head%sat(iSat)%numObs(ii),head%sat(iSat)%numMrk(ii),ii=1,head%nFreq),&
         (obshelp(iSat,ii),ii=1,4)
   ENDIF
  ENDDO
  WRITE(lfnWtf,"(' ')")

! AMBIGUITIES
! -----------
  IF(head%numAmb /= 0) THEN
    IF(head%nFreq == 1) THEN
      nFrq=1
      WRITE(lfnWtf,'(A)')' AMB SAT EPOCH  WLF    L1-AMBIG.   CLUS'
    ELSE
      nFrq=3
      WRITE(lfnWtf,'(A)')&
           ' AMB SAT EPOCH  WLF    L1-AMBIG.   CLUS    L2-AMBIG.   CLUS    L5-AMBIG.   CLUS'
    ENDIF
    DO iAmb = 1,head%numAmb
      IF(head%nFreq == 1) THEN
        WRITE(lfnWtf,'(2I4,I6,3X,I1,1X,F15.0,I5)')&
             iAmb,head%ambigu(iAmb)%ambSat,head%ambigu(iAmb)%ambIep,&
             head%ambigu(iAmb)%ambWlf(1:head%nFreq),&
             (head%ambigu(iAmb)%ambigu(iFrq),head%ambigu(iAmb)%ambCls(iFrq),iFrq=1,nFrq)
      ELSE
        WRITE(lfnWtf,'(2I4,I6,2X,I1,"/",I1,3(F15.0,I5))')&
             iAmb,head%ambigu(iAmb)%ambSat,head%ambigu(iAmb)%ambIep,&
             head%ambigu(iAmb)%ambWlf(1:head%nFreq),&
             (head%ambigu(iAmb)%ambigu(iFrq),head%ambigu(iAmb)%ambCls(iFrq),iFrq=1,nFrq)
      ENDIF
    ENDDO
    WRITE(lfnWtf,"(' ')")
  ENDIF

! OBSERVATION TITLE LINES:
! -----------------------
  WRITE(lfnWtf,'(A,/,A,A,7X,A,12X,A,A24)')&
         obTxt1(head%nFreq),'   OBS.N    TIME   F #S',&
         obTxt2(head%meaTyp),'...','AT THE END:  DATE, ',obTxt3(head%nDiff+1)

  RETURN
  END SUBROUTINE wtfmth2

END MODULE
