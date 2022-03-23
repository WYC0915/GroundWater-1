! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  PROGRAM fmtObs

! -------------------------------------------------------------------------
! Purpose:    Read formatted observation files (phase or code,
!             zero or single differences) containing header and
!             observations and write unformatted header and
!             unformatted observation files
!
! Remarks:    updated version of PG OBSFMT.f
!
! Changes in old PG:
!             11-SEP-91 : OPEN BINARY OBS. FILE JUST BEFORE EPOCH
!                         LOOP (ELSE AN ERROR IN "RDFMTH" WILL BE
!                         DISASTROUS)
!             18-MAY-92 : PRINT LIST OF FILES
!             23-NOV-93 : SF: SET MAXSAT TO 30
!             10-AUG-94 : MR: CALL EXITRC
!             12-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
!             06-JUN-96 : MR: REMOVED UNUSED VARIABLES
!             24-SEP-97 : DI: USE USE m_maxdim, ONLY: MAXSAT
!             09-NOV-00 : CU: SWITCH TO NEW MENU SYSTEM
!             22-OCT-01 : HB: CHANGE CALL of FMTOBSFL
!             16-DEC-01 : HU: D_CONST ADDED
!
! Author:     M.Rothacher, L.Mervart
!
! Created:    30-Jul-2002
!
! Changes:    25-Sep-2002 HU: Remove i_astlib
!             17-Feb-2003 LM: Use m_maxdim
!             23-Apr-2003 HU: Nullify local pointers
!             15-May-2003 AJ: Initialize structure
!             19-Aug-2003 HU: Write title line only once
!             25-Aug-2003 CU: Change format string
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             27-Feb-2007 AG: Call DEFCON with parameter
!             14-Nov-2008 DT: Count range obs. correctly
!             23-Sep-2010 RD: Enable CPU counter
!             24-Nov-2011 SL: new title string for pritit, m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, fileNameLength, &
                      lfnPrt, lfnLoc, lfnErr, lfnRes
  USE m_cpu,    ONLY: cpu_start
  USE m_maxdim, ONLY: maxsat
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE d_const,  ONLY: date,time
  USE d_gpsObs, ONLY: t_obsHead,init_obshead
  USE s_fmtobsfl
  USE s_uphead2
  USE s_rdfmti
  USE s_opnfil
  USE s_alcerr
  USE s_prflna
  USE s_exitrc
  USE s_defcon
  USE s_wtobsi
  USE s_pritit
  USE s_opnsys
  USE s_rdfmth2
  USE s_opnerr
  USE s_readinpf
  USE f_tstflg
  USE s_wthead2

  IMPLICIT NONE

! maxSat: Maximum number of satellites

! Parameter
! ---------
  CHARACTER(LEN=6),  PARAMETER :: pgName = 'FMTOBS'

! Declarations
! ------------
  TYPE(t_obsHead) :: obsHead

  CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: filNam
  CHARACTER(LEN=6) :: mxnSat
  CHARACTER(LEN=1) :: epoFlg
  CHARACTER(LEN=1),DIMENSION(maxSat,2) :: obsFlg

  REAL(r8b),DIMENSION(2) :: deltat
  REAL(r8b),DIMENSION(maxSat,2) :: observ
  REAL(r8b) :: obsTim,obsTi1

  INTEGER(i4b),DIMENSION(maxsat,2) :: numOb1
  INTEGER(i4b),DIMENSION(maxsat,2) :: numMr1
  INTEGER(i4b),DIMENSION(maxSat) :: nrsat
  INTEGER(i4b),DIMENSION(:,:),ALLOCATABLE :: iFrLst
  INTEGER(i4b) :: mxcSat
  INTEGER(i4b) :: obsNum
  INTEGER(i4b) :: iFil,nFil
  INTEGER(i4b) :: iFrq,nFrq
  INTEGER(i4b) :: iSat,iSatel,nSat,nSatNw
  INTEGER(i4b) :: iEpo
  INTEGER(i4b) :: iRetrn
  INTEGER(i4b) :: iFirst
  INTEGER(i4b) :: ioStat,irc
  INTEGER(i4b) :: numObsR

! Common for maximal dimensions, common for constants
! ---------------------------------------------------
  COMMON/MCMSAT/MXCSAT,MXnSat

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Initialize common blocks for maximal dimensions
! -----------------------------------------------
  MXCSAT=MAXSAT
  MXnSat='MAXSAT'

! Nullify pointers
! ----------------
  CALL init_obshead(obsHead)
  CALL init_inpkey(inpKey)
  NULLIFY(filNam)

! Logical file numbers and file names
! -----------------------------------

! Get the name of the input file
! ------------------------------
  CALL readinpf(' ',inpKey)

! Define system files
! -------------------
  CALL opnSys

! Define constants
! ----------------
  CALL defCon(1)

! Print title and file list
! -------------------------
  CALL pritit('FMTOBS','Convert observation files (ASCII to binary)')
  CALL prflna

! Get observation file names
! --------------------------
  CALL fmtobsfl(nfil,filnam)

  WRITE(lfnPrt,'(1X,A,7X,A,10X,A,/)') &
        'FORMATTED OBSERVATION FILE','BINARY OBSERVATION FILE','EPOCHS'

! Loop over all files
! -------------------
  DO iFil=1,nFil

! Open ascii file
! ---------------
    CALL opnFil(lfnLoc,filNam(1,iFil),'OLD',' ',' ',' ',ioStat)
    CALL opnErr(lfnErr,lfnLoc,ioStat,filNam(1,iFil),pgName)

! Read formatted header
! ---------------------
    CALL rdFmth2(lfnLoc,obsHead)

! Allocate memory for iFrLst
! --------------------------
    ALLOCATE(iFrLst(2,obsHead%nSatel),stat=irc)
    CALL alcErr(irc,'iFrLst',(/2,obsHead%nSatel/),pgName)

! Initialize first and last observation number, number of observations
! --------------------------------------------------------------------
    DO iSatel = 1,obsHead%nSatel
      iFrLst(1,iSatel) = 1000000
      iFrLst(2,iSatel) = 0
      DO iFrq = 1,obsHead%nFreq
        numOb1(iSatel,iFrq) = 0
        numMr1(iSatel,iFrq) = 0
      ENDDO
    ENDDO
    iFirst = 1
    obsHead%nEpFlg = 0

! Open binary observation file
! ----------------------------
    CALL opnFil(lfnRes,filNam(3,iFil),'NEW','UNFORMATTED',' ',' ',ioStat)
    CALL opnErr(lfnErr,lfnRes,ioStat,filNam(3,iFil),'FMTOBS')

! Loop over all observations
! --------------------------
    DO iEpo = 1,100000

! Read observation from formatted file
      CALL rdFmti(lfnLoc,obsHead%iFrmat,obsHead%nFreq,obsTim,deltat,epoFlg,&
           nSat,nrSat,obsFlg,observ,iRetrn)

! End of file reached
      IF(iRetrn == 1) EXIT

! Observation number
      obsNum=IDNINT((obsTim-obsHead%timRef)*86400.d0/obsHead%iDeltt)+1

! Loop over all satellite of this epoch
! -------------------------------------
      nSatNw=0
      DO iSat=1,nSat

! Find index in array "numSat" for satellite "iSat"
        DO iSatel=1,obsHead%nSatel
          IF(nrSat(iSat) == obsHead%sat(iSatel)%numSat) GOTO 60
        ENDDO
        CYCLE

! Update first and last observation number
60      IF(iFrLst(1,iSatel) > obsNum) iFrLst(1,iSatel)=obsNum
        IF(iFrLst(2,iSatel) < obsNum) iFrLst(2,iSatel)=obsNum

! Copy information for this satellite
        nSatNw=nSatNw+1
        nrSat(nSatNw)=nrSat(iSat)
        DO iFrq=1,obsHead%nFreq
          obsFlg(nSatNw,iFrq)=obsFlg(iSat,iFrq)
          observ(nSatNw,iFrq)=observ(iSat,iFrq)

! Count number of marked and unmarked observations
          IF(observ(iSat,iFrq) /= 0.D0) THEN
            IF(TSTFLG(obsFlg(iSat,iFrq),0)) THEN
              numMr1(iSatel,iFrq)=numMr1(iSatel,iFrq)+1
            ELSE
              numOb1(iSatel,iFrq)=numOb1(iSatel,iFrq)+1
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      nSat=nSatNw

! Store first observation time
      IF(nSat /= 0) THEN
        IF(iFirst == 1) THEN
          iFirst=0
          obsTi1=obsTim
        ENDIF

! Count flagged epochs
        IF(TSTFLG(epoFlg,0)) obsHead%nEpFlg=obsHead%nEpFlg+1

! Write observation into unformatted observation file
        CALL wtObsi(lfnRes,obsHead%iFrmat,obsHead%nFreq,obsTim,deltat,epoFlg,&
             nSat,nrSat,obsFlg,observ)
      ENDIF

! Next observation
    ENDDO

! Close files
! -----------
    CLOSE(UNIT=lfnLoc)
    CLOSE(UNIT=lfnRes)

! Number of frequencies for ambiguities
! -------------------------------------
    IF(obsHead%nFreq == 1) THEN
      nFrq=1
    ELSE
      nFrq=3
    ENDIF

! Update number of epochs
! -----------------------
    obsHead%nEpoch=IDNINT((obsTim-obsTi1)/obsHead%iDeltt*86400.D0)+1

! Update number of observations
! -----------------------------
    numObsR = 0
    DO iSatel=1,obsHead%nSatel
      DO iFrq=1,obsHead%nFreq
        obsHead%sat(iSatel)%numObs(iFrq)=numOb1(iSatel,iFrq)
        obsHead%sat(iSatel)%numMrk(iFrq)=numMr1(iSatel,iFrq)

        IF ( obsHead%meatyp == 3 )  &
           numObsR = numObsR + numOb1(iSatel,iFrq) + numMr1(iSatel,iFrq)

      ENDDO
    ENDDO
    CALL upHead2(obsTi1,obsHead,iFrLst)

! New reference epoch
! -------------------
    obsHead%timRef=obsTi1

! Set modification date and time
! ------------------------------
    obsHead%crDate(2)=date
    obsHead%crTime(2)=time

! Write unformatted header file
! -----------------------------
    CALL wtHead2(filNam(2,iFil),obsHead)

    IF ( obsHead%meatyp <= 2 ) THEN
      WRITE(lfnPrt,'(1X,A32,1X,A32,I7)') &
           filNam(1,iFil),filNam(3,iFil),obsHead%nEpoch

    ELSEIF ( obsHead%meatyp == 3 ) THEN
      WRITE(lfnPrt,'(1X,A32,1X,A32,I7)') &
           filNam(1,iFil),filNam(3,iFil), numObsR

    ENDIF

! Next file
! ---------
    DEALLOCATE(iFrLst)
  ENDDO


  CALL exitrc(0)
  END PROGRAM fmtObs
