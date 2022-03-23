MODULE s_RDHEAD2
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE rdHead2(filHed,head)

! -------------------------------------------------------------------------
! Purpose:      Read the entire header information of an observation
!               file (phase or code, zero or single differences)
!
! Remarks:      Updated version of SR RDHEAD.f
!
! Changes in old SR:
!               11-JAN-1993 : USE OF SR "OPNFIL" TO OPEN FILES
!               04-MAY-1993 : NEW FORMAT
!               10-AUG-1994 : MR: CALL EXITRC
!               12-AUG-1994 : MR: FORMAT 4: SESSION AS CHARACTER*4
!               14-DEC-1995 : MR: CORRECT ERROR MESSAGE FOR FORMAT
!               15-DEC-1995 : MR: VERSION 4.0
!               23-APR-1996 : MR: NEW VERSION 4.1 (USE 4.X FOR FORMAT)
!               22-SEP-1997 : DI: USE USE m_maxdim, ONLY: MAXSAT
!               02-MAR-2000 : LM: VARIABLE LENGTH OF filHed
!
! Author:       M.Rothacher, L.Mervart
!
! Created:      10-Jul-2002
!
! Changes:      17-Feb-2003 LM: Use m_maxdim
!               16-May-2003 HU: Deallocate arrays
!               21-May-2003 RD: Make the deallocation safe
!               08-Sep-2003 HU: antnam, recnam, oprnam chr16 -> chr20
!               24-May-2004 HU: Error if sampling lower than 1 sec
!               26-Jan-2011 LP: Sat.-specific obstypes, format 6
!               26-Apr-2012 LP: Dimension of obstyp changed (4->8)
!
! SR used:      opnErr, opnFil, exitrc, alcErr
!
! Copyright:    Astronomical Institute
!               University of Bern
!               Switzerland
! ---------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_maxdim, ONLY : maxsat, maxamb
  USE d_gpsObs, ONLY : t_obsHead
  USE s_opnfil
  USE s_alcerr
  USE s_hd3235
  USE s_opnerr
  USE s_exitrc
  IMPLICIT NONE



! List of parameters
! ------------------
! input:
  CHARACTER(LEN=*)                    :: filHed
  TYPE(t_obsHead)                     :: head

! List of functions
! -----------------
! Local types
! -----------

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER   :: srName = 'rdHead'
  CHARACTER(LEN=36),PARAMETER   :: chrOrd = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  INTEGER(i4b),PARAMETER        :: maxAmo = 30
  INTEGER(i4b),PARAMETER        :: Fmt4Lgt = 16
  INTEGER(i4b),PARAMETER        :: Fmt5Lgt = 20

! Local Variables
! ---------------
  CHARACTER(LEN=1),DIMENSION(maxAmo,maxSat,3) :: ambflo
  CHARACTER(LEN=Fmt4Lgt)                      :: hlpStr4 ! Help string for station name
                                                         ! reading for ifrmat<=4

  CHARACTER(LEN=Fmt5Lgt)                      :: hlpStr5 ! Help string for station name
                                                         ! reading for ifrmat=5

  CHARACTER(LEN=16)                           :: hlpStrR ! Receiver name ifrmt<=4
  CHARACTER(LEN=16)                           :: hlpStrA ! Antenna name ifrmt<=4
  CHARACTER(LEN=16)                           :: hlpStrO ! Operator name ifrmt<=4
  CHARACTER(LEN=3),DIMENSION(maxSat,4)        :: obshelp
  REAL(r8b),DIMENSION(maxAmo,maxSat,3)        :: ambigo
  REAL(r8b),DIMENSION(maxAmb,3)               :: ambig
  REAL(r8b)                                   :: rDeltt  ! R*8 sampling, ifrmt>=5

  INTEGER(i4b)                          :: null
  INTEGER(i4b),DIMENSION(2)             :: iSess
  INTEGER(i4b),DIMENSION(maxSat,2)      :: iWlFac
  INTEGER(i4b),DIMENSION(maxSat)        :: numAmo
  INTEGER(i4b),DIMENSION(maxAmo,maxSat) :: ambIeo
  INTEGER(i4b),DIMENSION(maxAmb,2)      :: ambWlf
  INTEGER(i4b),DIMENSION(maxAmb,3)      :: ambCls

  INTEGER(i4b),DIMENSION(maxSat)        :: numSat
  INTEGER(i4b),DIMENSION(maxSat)        :: ambSat
  INTEGER(i4b),DIMENSION(maxSat)        :: ambIep
  INTEGER(i4b)                          :: numAmb
  INTEGER(i4b)                          :: ioStat
  INTEGER(i4b)                          :: iac
  INTEGER(i4b)                          :: iChr
  INTEGER(i4b)                          :: iAmb
  INTEGER(i4b)                          :: iSat
  INTEGER(i4b)                          :: iSta,nSta
  INTEGER(i4b)                          :: iFrq,nFrq
  INTEGER(i4b)                          :: obs

! Initialize
! ----------
  numAmb = 0

! OPEN HEADER FILE
! ----------------
  CALL opnFil(lfnLoc,filHed,'OLD','UNFORMATTED','READONLY',' ',ioStat)
  CALL opnErr(lfnErr,lfnLoc,ioStat,filHed,'RDHEAD')

! RECORD 1: CAMPAIGN, TITLE AND MEASUREMENT TYPE
! ----------------------------------------------
  READ(lfnLoc) head%campgn,head%title,head%meaTyp

! RECORD 2: GENERAL INFORMATION (NUMBER OF ...)
! ---------------------------------------------
  READ(lfnLoc) head%nDiff,head%nFreq,head%nEpoch,head%nSatel,null,iSess,&
       null,head%iDeltt,head%irMark,head%nEpFlg,head%iFrmat,head%timRef,&
       head%crDate,head%crTime

! CHECK HEADER FORMAT
! -------------------
  IF(head%iFrmat /= 2 .AND. head%iFrmat /= 3 .AND. &
     head%iFrmat /= 4 .AND. head%iFrmat /= 5 .AND. head%iFrmat /= 6) THEN
    WRITE(lfnErr,'(/,A,/,16X,A,A,/,16X,A,I1,/,1(16X,A,/))')&
         ' *** SR rdHead: Illegal header format',&
                         'Header file name: ',filHed,&
                         'Header and obs. files of format version ',head%iFrmat,&
                         'can not be processed with version 5.2'
    CALL exitrc(2)
  ENDIF

! CHECK NUMBER OF SATELLITES
! --------------------------
  IF(head%nSatel > maxSat) THEN
    WRITE(lfnErr,'(/,A,I3,/,16X,A,I3,/,16X,A,A,/)')&
         ' *** SR rdHead: Too many satellites:',head%nSatel,&
                         'Maximum # of sat.  :',maxSat,&
                         'Header file name   : ',filHed
    CALL exitrc(2)
  ENDIF

! Allocate memory for head%sat
! ----------------------------
  IF (ASSOCIATED(head%sat)) &
    DEALLOCATE(head%sat,stat=iac)
  ALLOCATE(head%sat(head%nSatel),stat=iac)
  CALL alcerr(iac,'head%sat',(/head%nSatel/),srName)

! READ SESSION SEPARATELY FOR FORMAT 4
! ------------------------------------
  IF (head%iFrmat >= 4) THEN
    READ(lfnLoc) head%cSess
  ELSE

! CONVERSION OF SESSION NUMBERS TO CHARACTER STRINGS
    WRITE(head%cSess(1),'(I4.4)') iSess(1)
    iChr = MOD(iSess(2),36)+1
    head%cSess(2)(1:1) = chrOrd(iChr:iChr)
    head%cSess(2)(2:4)=' '
    IF (iSess(2) > 35) THEN
      WRITE(lfnErr,'(/,A,/,16X,A,/,16X,A,A4,/,16X,A,I4,/,16X,A,A1,/,&
           &16X,A,A,/,2(16X,A,/))')&
    ' ### SR rdHead: Possible problem with the conversion of the file',&
                    'Sequence number to version 4.x',&
                    'Session identifier                      : ',head%cSess(1),&
                    'Old file sequence number was (integer)  : ',iSess(2),&
                    'New file sequence identifier is (1 CHAR): ',head%cSess(2)(1:1),&
                    'Header file name                        : ',filHed,&
                    'Check that two files of the same station belonging',&
                    'to the same session have unique file seq. numbers'
    ENDIF
  ENDIF

! READ SAMPLING AS R*8 FOR FORMAT 5
! ---------------------------------
  IF (head%iFrmat >= 5) THEN
    READ(lfnLoc) rDeltt
    head%iDeltt = IDNINT(rDeltt)
    IF (DABS(rDeltt-head%iDeltt) > 1D-6) THEN
      WRITE(lfnErr,'(/,A,/,16X,A,F12.6,/,16X,A,A,/)') &
          ' *** SR rdHead: Sampling rate larger than 1Hz found. Not supported.',&
                          'Sampling (sec)      : ',rDeltt,&
                          'Header file name    : ',TRIM(filHed)
      CALL exitrc(2)
    ENDIF
  ENDIF

! LOOP OVER STATIONS
! -----------------------
  nSta=head%nDiff+1
  DO iSta=1,nSta

! RECORDS 3,4: STATION AND RECEIVER INFORMATION
! ---------------------------------------------
! FORMAT 5: NAMES CHR20
    IF (head%iFrmat >= 5) THEN
      READ(lfnLoc) hlpStr5,head%sta(iSta)%recTyp,&
           head%sta(iSta)%irUnit,head%sta(iSta)%iAnten,&
           head%sta(iSta)%oprNam,head%sta(iSta)%antTyp,head%sta(iSta)%iClock
      READ(lfnLoc) head%sta(iSta)%posEcc(1:3)
      head%sta(iSta)%staNam=hlpStr5
    ELSE
      READ(lfnLoc) hlpStr4,hlpStrR,&
           head%sta(iSta)%irUnit,head%sta(iSta)%iAnten,&
           hlpStrO,hlpStrA,head%sta(iSta)%iClock
      READ(lfnLoc) head%sta(iSta)%posEcc(1:3)
      head%sta(iSta)%staNam(1:Fmt4Lgt) = hlpStr4(1:Fmt4Lgt)
      head%sta(iSta)%recTyp=hlpStrR//'????'
      head%sta(iSta)%antTyp=hlpStrA//'????'
      head%sta(iSta)%oprNam=hlpStrO
    ENDIF
  ENDDO

! RECORD 5: SATELLITE NUMBERS
! ---------------------------
  READ(lfnLoc) head%sat(1:head%nSatel)%numSat

! LOOP OVER FREQUENCIES
! ---------------------
  DO iFrq=1,head%nFreq

! RECORDS 6-8: NUMBER OF OBSERVATIONS (USED AND MARKED)
! -----------------------------------------------------
    READ(lfnLoc) head%sat(1:head%nSatel)%numObs(iFrq)
    READ(lfnLoc) head%sat(1:head%nSatel)%numMrk(iFrq)
    IF (head%iFrmat == 2) THEN
      READ(lfnLoc) iWlFac(1:head%nSatel,iFrq)
    ENDIF
  ENDDO

  IF(head%nFreq == 1) THEN
    nFrq=1
  ELSE
    nFrq=3
  ENDIF

! FORMAT 2: AMBIGUITIES
! =====================
  IF (head%iFrmat == 2) THEN

! RECORD 9: NUMBER OF AMBIGUITIES PER SATELLITE
! ---------------------------------------------
    READ(lfnLoc) (numAmo(iSat),iSat=1,head%nSatel)

! CHECK NUMBER OF AMBIGUITIES
! ---------------------------
    DO iSat=1,head%nSatel
      IF(numAmo(iSat) > maxAmo) THEN
        WRITE(lfnErr,'(/,3(A,I3,/,16X),A,A,/)') &
             ' *** SR rdHead: Too many ambiguities:',numAmo(iSat),&
                             'Maximum # of ambig. :',maxAmo,&
                             'Satellite number    :',head%sat(iSat)%numSat,&
                             'Header file name    : ',filHed
        CALL exitrc(2)
      ENDIF
    ENDDO

! RECORD 10: AMBIGUITIES
! ----------------------
    DO iSat=1,head%nSatel
      DO iAmb=1,numAmo(iSat)
        READ(lfnLoc) ambIeo(iAmb,iSat),&
             (ambFlo(iAmb,iSat,iFrq),ambigo(iAmb,iSat,iFrq),iFrq=1,nFrq)
        numAmb = numAmb + 1
      ENDDO
    ENDDO

    IF (ASSOCIATED(head%ambigu)) &
      DEALLOCATE(head%ambigu,stat=iac)
    ALLOCATE(head%ambigu(numAmb),stat=iac)
    CALL alcerr(iac,'head%ambigu',(/numAmb/),srName)

! CONVERSION TO FORMAT 3
! ----------------------
    numSat(1:head%nSatel) = head%sat(1:head%nSatel)%numSat
    CALL HD3235(maxAmo,maxSat,head%meaTyp,head%nDiff,head%nFreq,&
         head%nSatel,numSat,numAmo,iWlFac,ambIeo,ambigo,ambFlo,&
         head%numAmb,ambSat,ambIep,ambWlf,ambig,ambCls)
    DO iAmb = 1,head%numAmb
      head%ambigu(iAmb)%ambSat    = ambSat(iAmb)
      head%ambigu(iAmb)%ambIep    = ambIep(iAmb)
      head%ambigu(iAmb)%ambWlf(:) = ambWlf(iAmb,:)
      head%ambigu(iAmb)%ambigu(:) = ambig(iAmb,:)
      head%ambigu(iAmb)%ambCls(:) = ambCls(iAmb,:)
    ENDDO

! FORMAT 3: AMBIGUITIES
! =====================
  ELSE

! RECORD 9: TOTAL NUMBER OF AMBIGUITIES
! -------------------------------------
    READ(lfnLoc) head%numAmb

    IF (ASSOCIATED(head%ambigu)) &
      DEALLOCATE(head%ambigu,stat=iac)
    ALLOCATE(head%ambigu(head%numAmb),stat=iac)
    CALL alcerr(iac,'ambigu',(/head%numAmb/),srName)

! RECORD 10: AMBIGUITIES
! ----------------------
    DO iAmb=1,head%numAmb
      READ(lfnLoc) head%ambigu(iAmb)%ambSat,head%ambigu(iAmb)%ambIep,&
           head%ambigu(iAmb)%ambWlf(1:head%nFreq),&
           (head%ambigu(iAmb)%ambigu(iFrq),head%ambigu(iAmb)%ambCls(iFrq),iFrq=1,nFrq)
    ENDDO
  ENDIF


! RECORD 11: Satellite-specific signal selection from external file
! -----------------------------------------------------------------
  DO obs = 1,8
    head%sat(1:head%nSatel)%obstyp(obs) = '   '
  ENDDO

  IF (head%iFrmat >= 6) THEN
   DO iSat=1,maxsat
     DO obs = 1,4
      obshelp(iSat,obs) = '   '
     ENDDO
   ENDDO

   DO obs = 1,4
     READ(lfnLoc) (obshelp(iSat,obs),iSat=1,head%nSatel)
   ENDDO

!  Phase obs
   IF (head%meaTyp==1) THEN
    DO iSat=1,head%nSatel
      head%sat(iSat)%obstyp(3) = obshelp(iSat,1)
      head%sat(iSat)%obstyp(4) = obshelp(iSat,2)
      head%sat(iSat)%obstyp(7) = obshelp(iSat,3)
      head%sat(iSat)%obstyp(8) = obshelp(iSat,4)
    ENDDO
   ENDIF

!  Code obs
   IF (head%meaTyp==2) THEN
    DO iSat=1,head%nSatel
      head%sat(iSat)%obstyp(1) = obshelp(iSat,1)
      head%sat(iSat)%obstyp(2) = obshelp(iSat,2)
      head%sat(iSat)%obstyp(5) = obshelp(iSat,3)
      head%sat(iSat)%obstyp(6) = obshelp(iSat,4)
    ENDDO
   ENDIF
  ENDIF


! CLOSE HEADER FILE
! -----------------
  CLOSE(UNIT=lfnLoc)

  RETURN
  END SUBROUTINE rdHead2

END MODULE
