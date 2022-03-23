! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  PROGRAM obsFmt

! -------------------------------------------------------------------------
! Purpose:    Read unformatted header and unformatted observa-
!             tion files (phase or code, zero or single differences)
!             and write formatted observation files containing
!             header and observations
!
! Remarks:    updated version of PG OBSFMT.f
!
! Changes in old PG:
!             18-MAY-92 : PRINT LIST OF FILES
!             23-NOV-93 : SF: SET MAXSAT TO 30
!             10-AUG-94 : MR: CALL EXITRC
!             14-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
!             24-SEP-97 : DI: USE USE m_maxdim, ONLY: MAXSAT
!             08-MAY-98 : MR: MAXFIL=500 (OLD 200)
!             09-NOV-00 : CU: SWITCH TO NEW MENU SYSTEM
!             22-OCT-01 : HB: Change CALL of OBSFMTFL
!
! Author:     M.Rothacher, L.Mervart
!
! Created:    30-Jul-2002
!
! Changes:    25-Sep-2002 HU: Remove i_astlib
!             17-Feb-2003 LM: Use m_maxdim
!             23-Apr-2003 HU: Nullify local pointers
!             16-May-2003 AJ: Initialize structure
!             25-Aug-2003 CU: Change format string
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             27-Feb-2007 AG: Call DEFCON
!             23-Sep-2010 RD: Enable CPU counter
!             28-Jun-2011 HB: Allow for more than 100000 epochs
!             30-Nov-2011 SL: new title string for pritit, m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! --------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, fileNameLength, lfnPrt, lfnLoc, lfnRes, &
                      lfnErr
  USE m_cpu,    ONLY: cpu_start
  USE m_maxdim, ONLY: maxsat
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE d_gpsObs, ONLY: t_obsHead,init_obshead
  USE s_rdobsi
  USE s_opnfil
  USE s_prflna
  USE s_wtfmti
  USE s_pritit
  USE s_obsfmtfl
  USE s_readinpf
  USE s_opnerr
  USE s_rdhead2
  USE s_exitrc
  USE s_opnsys
  USE s_defcon
  USE s_wtfmth2
  IMPLICIT NONE

! Maximal dimensions
! ------------------

! MAXSAT: MAXIMUM NUMBER OF SATELLITES

! Parameter
! ---------
  CHARACTER(LEN=6),  PARAMETER :: pgName = 'OBSFMT'

! Declarations
! ------------
  TYPE(t_obsHead) :: obsHead

  CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: filNam
  CHARACTER(LEN=6) ::  mxnSat
  CHARACTER(LEN=1),DIMENSION(maxSat,2) :: obsFlg
  CHARACTER(LEN=1) :: epoFlg

  REAL(r8b),DIMENSION(2) :: deltat
  REAL(r8b),DIMENSION(maxSat,2) :: observ
  REAL(r8b) :: dtBlnk
  REAL(r8b) :: obsTim

  INTEGER(i4b) :: mxcSat
  INTEGER(i4b),DIMENSION(2) :: iFrqs
  INTEGER(i4b),DIMENSION(maxSat) :: nrSat
  INTEGER(i4b) :: iFrq, iFil, nFil, ioStat
  INTEGER(i4b) :: init,iretrn
  INTEGER(i4b) :: nSat
  INTEGER(i4b) :: iEpo


! COMMON FOR MAXIMAL DIMENSIONS, COMMON FOR CONSTANTS
! ---------------------------------------------------
  COMMON/MCMSAT/MXCSAT,MXNSAT

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
! -----------------------------------------------
  MXCSAT=MAXSAT
  MXNSAT='MAXSAT'

! Nullify pointers
! ----------------
  CALL init_obshead(obsHead)
  CALL init_inpkey(inpKey)
  NULLIFY(filNam)

! LOGICAL FILE NUMBERS AND FILE NAMES
! -----------------------------------

! Get the name of the input file
! ------------------------------
  CALL readinpf(' ',inpKey)

! Define system files and constants
! ---------------------------------
  CALL opnsys
  CALL defcon(0)

! Print title and file list
! -------------------------
  CALL pritit('OBSFMT','Convert observation files (binary to ASCII)')
  CALL prflna

! Get observation file names
! --------------------------
  CALL obsfmtfl(nfil, filnam)

! PREPARE FREQUENCY ARRAY
! -----------------------
  DO iFrq=1,2
    iFrqs(iFrq)=iFrq
  ENDDO

! Set minimum interval to test, if there is a break in the observations
! ---------------------------------------------------------------------
  dtBlnk=1.D-9

  WRITE(lfnPrt,'((1X,A,10X,A,7X,A,/))')&
       'BINARY OBSERVATION FILE','FORMATTED OBSERVATION FILE','EPOCHS'

! Loop over all files to be formatted
! -----------------------------------
  DO iFil=1,nFil

! Read header file
! ----------------
    CALL rdhead2(filNam(1,iFil),obsHead)

! Open files
! ----------
    CALL OPNFIL(lfnLoc,filNam(2,iFil),'OLD','UNFORMATTED',' ',' ',ioStat)
    CALL OPNERR(lfnErr,lfnLoc,ioStat,filNam(2,iFil),pgName)

    CALL OPNFIL(lfnRes,filNam(3,iFil),'NEW',' ',' ',' ',ioStat)
    CALL OPNERR(lfnErr,lfnRes,ioStat,filNam(3,iFil),pgName)

! Write header of formatted file
! ------------------------------
    CALL wtfmth2(lfnRes, obshead)

! Loop over all observations
! --------------------------
    init=1
    iEpo = 0
    DO
      iEpo = iEpo+1

! Read observation from unformatted observation file
      CALL RDOBSI(lfnLoc,obsHead%iFrmat,obsHead%nFreq,iFrqs,obsTim,&
           deltat,epoFlg,nSat,nrSat,obsFlg,observ,iretrn)

! End of file reached
      IF(iretrn == 1) EXIT

! Write observation into formatted observation file
      CALL wtfmti(lfnRes,init,obsHead%iFrmat,obsHead%nFreq,obsHead%timRef,&
           obsHead%iDeltt,dtBlnk,obsTim,deltat,epoFlg,nSat,nrSat,obsFlg,observ)
    ENDDO

! Close files
! -----------
    CLOSE(UNIT=lfnLoc)
    CLOSE(UNIT=lfnRes)

    WRITE(lfnPrt,'(1X,A32,1X,A32,I7)')filNam(2,iFil),filNam(3,iFil),iEpo-1

! Next file
! ---------
   ENDDO

   CALL exitrc(0)

 END PROGRAM obsFmt
