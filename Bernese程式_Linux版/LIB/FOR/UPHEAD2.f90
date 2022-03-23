MODULE s_UPHEAD2
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE upHead2(obsTi1,head,iFrLst)

! -------------------------------------------------------------------------
! Purpose:    Update ambiguities  and number of obs. in header
!
! Remarks:    Updated version of SR UPHEAD.f
!
! Changes in old SR:
!             10-AUG-1994 : MR: CALL EXITRC
!             22-SEP-1997 : DI: USE MAXSAT.inc
!             15-FEB-2000 : RD: REMOVE AMB. FOR SAT. WHICH ARE NOT IN NUMSAT
!                             (E.G. DUE TO SATCRX MARKING IN SOME CASES)
!             24-FEB-2000 : RD: BUG IN AMBIGUITY HANDLING, IF MORE THAN ONE
!                                AMBIGUITY IS BEFORE THE FIRST OBSERVATION
!
! Author:     L.Mervart
!
! Created:    10-Jul-2002
!
! Changes:    20-Sep-2012 RD: Use M_BERN with ONLY
!             20-Sep-2012 RD: remove unused variables
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! --------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b
  USE d_gpsObs, ONLY: t_obsHead

  USE s_alcerr
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  REAL(r8b) :: obsTi1
  TYPE(t_obsHead) :: head
  INTEGER(i4b),DIMENSION(2,*) :: iFrLst

! List of functions
! -----------------
! Local types
! -----------

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER   :: srName = 'upHead'

! Local Variables
! ---------------
  INTEGER(i4b) :: irc
  INTEGER(i4b) :: iSatel
  INTEGER(i4b) :: iAmb,jAmb
  INTEGER(i4b) :: nSatnw,nAmbnw,iAmbnw
  INTEGER(i4b) :: iFrq,nFrq,iFreq
  INTEGER(i4b) :: nTotal
  INTEGER(i4b) :: nEpDif

  INTEGER(i4b),DIMENSION(:),ALLOCATABLE :: namSat


! Allocate memory for namSat
! --------------------------
  ALLOCATE(namSat(head%nSatel),stat=irc)
  CALL alcerr(irc,'namSat',(/head%nSatel/),srName)

  IF(head%nFreq == 1) THEN
    nFrq=1
  ELSE
    nFrq=3
  ENDIF

! UPDATE NUMBER OF SATELLITES AND SATELLITE SPECIFIC ARRAYS
! ---------------------------------------------------------
  nSatnw = 0
  DO iSatel=1,head%nSatel
    nTotal=0
    DO iFrq=1,head%nFreq
      nTotal=nTotal+head%sat(iSatel)%numObs(iFrq)+head%sat(iSatel)%numMrk(iFrq)
    ENDDO

! STILL OBSERVATIONS OF THE SATELLITE IN THE FILE ?
    IF (nTotal == 0) THEN

! REMOVE AMBIGUITIES OF REMOVED SATELLITE
      iAmbNw=0
      DO iAmb=1,head%numAmb
        IF (head%ambigu(iAmb)%ambSat == head%sat(iSatel)%numSat) CYCLE
        iAmbNw=iAmbNw+1
        head%ambigu(iAmbnw)%ambSat = head%ambigu(iAmb)%ambSat
        head%ambigu(iAmbnw)%ambIep = head%ambigu(iAmb)%ambIep
        DO iFreq=1,head%nFreq
          head%ambigu(iAmbnw)%ambWlf(iFreq)=head%ambigu(iAmb)%ambWlf(iFreq)
        ENDDO
        DO iFrq=1,nFrq
          head%ambigu(iAmbnw)%ambCls(iFrq)=head%ambigu(iAmb)%ambCls(iFrq)
          head%ambigu(iAmbnw)%ambigu(iFrq)=head%ambigu(iAmb)%ambigu(iFrq)
        ENDDO
      ENDDO
        head%numAmb=iAmbNw
    ELSE
      nSatnw=nSatnw+1
      head%sat(nSatnw)%numSat = head%sat(iSatel)%numSat
      iFrLst(1,nSatnw) = iFrLst(1,iSatel)
      iFrLst(2,nSatnw) = iFrLst(2,iSatel)

! COPY NUMBER OF OBSERVATIONS
      DO iFrq=1,head%nFreq
        head%sat(nSatnw)%numObs(iFrq) = head%sat(iSatel)%numObs(iFrq)
        head%sat(nSatnw)%numMrk(iFrq) = head%sat(iSatel)%numMrk(iFrq)
      ENDDO
    ENDIF

  ENDDO
  head%nSatel=nSatnw

! UPDATE AMBIGUITY EPOCH NUMBERS, IF NECESSARY
! --------------------------------------------
  IF (head%numAmb /= 0) THEN
    nEpDif=IDNINT((obsTi1-head%timRef)*86400.D0/head%iDeltt)

    DO iSatel=1,head%nSatel
      namSat(iSatel)=0
    ENDDO

    nAmbnw=0
    ambLoop: DO iAmb=1,head%numAmb

! INDEX IN SATELLITE ARRAY
      DO iSatel=1,head%nSatel
        IF (head%sat(iSatel)%numSat == head%ambigu(iAmb)%ambSat) GOTO 311
      ENDDO

! TO THIS PLACE YOU COME WITH AMBIGUITIES FOR SATELLITES WHICH ARE NOT IN
! THE GENERAL SATELLITE LIST -- REMOVE THEM!!
      CYCLE ambLoop
311   CONTINUE

! REMOVE AMBIGUITIES AFTER LAST OBSERVATION EPOCH OF THE SATELLITE
      IF (namSat(iSatel) > 0 .AND.head%ambigu(iAmb)%ambIep > iFrLst(2,iSatel)) CYCLE ambLoop

! REMOVE MULTIPLE AMBIGUITIES BEFORE FIRST EPOCH OF THE SATELLITE
      IF(namSat(iSatel) == 0 .OR.head%ambigu(iAmb)%ambIep > iFrLst(1,iSatel)) THEN
        namSat(iSatel)=namSat(iSatel)+1

! KEEP ALL REMAINING AMBIGUITIES IN LIST
        nAmbnw=nAmbnw+1
        head%ambigu(nAmbnw)%ambSat = head%ambigu(iAmb)%ambSat
        head%ambigu(nAmbnw)%ambIep = head%ambigu(iAmb)%ambIep - nEpDif
        IF(namSat(iSatel) == 1) head%ambigu(nAmbnw)%ambIep = 1
        DO iFreq=1,head%nFreq
          head%ambigu(nAmbnw)%ambWlf(iFreq)=head%ambigu(iAmb)%ambWlf(iFreq)
        ENDDO
        DO iFrq=1,nFrq
          head%ambigu(nAmbnw)%ambCls(iFrq)=head%ambigu(iAmb)%ambCls(iFrq)
          head%ambigu(nAmbnw)%ambigu(iFrq)=head%ambigu(iAmb)%ambigu(iFrq)
        ENDDO
      ELSE

! IF MORE THAN ONE AMBIGUITY WAS SET BEFORE THE FIRST OBSERVATION
        IF (head%ambigu(iAmb)%ambIep <= iFrLst(1,iSatel)) THEN
          DO jAmb=nAmbnw,1,-1
            IF (head%ambigu(jAmb)%ambSat == head%ambigu(iAmb)%ambSat) THEN
              DO iFreq=1,head%nFreq
                head%ambigu(jAmb)%ambWlf(iFreq)=head%ambigu(iAmb)%ambWlf(iFreq)
              ENDDO
              DO iFrq=1,nFrq
                head%ambigu(jAmb)%ambCls(iFrq)=head%ambigu(iAmb)%ambCls(iFrq)
                head%ambigu(jAmb)%ambigu(iFrq)=head%ambigu(iAmb)%ambigu(iFrq)
              ENDDO
              CYCLE ambLoop
            ENDIF
          ENDDO
        ENDIF
      ENDIF
    ENDDO ambLoop
    head%numAmb=nAmbnw
  ENDIF

! Deallocate memory
! -----------------
  DEALLOCATE(namSat)

  RETURN
  END SUBROUTINE upHead2

END MODULE
