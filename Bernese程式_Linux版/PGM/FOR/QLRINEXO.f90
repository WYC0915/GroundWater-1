
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM qlrinexo

! -------------------------------------------------------------------------
! Purpose:    Conversion of SLR quick look files (normal point format) into
!             RINEX observation files
!
! Author:     C.Urschl
!
! Created:    04-Nov-2003
!
! Changes:    16-Feb-2004 CU: Nullify pointers
!             26-Jun-2004 RD: Use maxsta from M_MAXDIM.f90
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             27-Feb-2007 AG: Call DEFCON with parameter
!             23-Sep-2010 RD: Enable CPU counter
!             07-Oct-2010 RD: Do not close the lfnprt file
!             30-Nov-2011 SL: new title string for pritit, m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, filePathLength, fileNameLength80, &
                      lfnErr, lfn001
  USE m_cpu,    ONLY: cpu_start
  USE m_maxdim, ONLY: maxsta
  USE d_inpkey, ONLY: inpkey, init_inpkey
  USE d_stacrx, ONLY: t_stacrux, init_stacrux
  USE d_abbrev, ONLY: t_abbrev, init_abbrev
  USE d_qlfil,  ONLY: t_qlobs, init_qlfil
  USE s_opnfil
  USE s_readcrux
  USE s_pritit
  USE s_qlgtsta
  USE s_defcon
  USE s_opnsys
  USE s_gtflna
  USE s_dordup
  USE s_alcerr
  USE s_prflna
  USE s_qlwtrx
  USE s_qlgtrec
  USE s_readabb
  USE s_readinpf
  USE s_opnerr
  USE s_qlrdopt
  USE s_qlrdfil
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=8), PARAMETER         :: pgName = 'QLRINEXO'

! Local Variables
! ---------------
  TYPE(t_qlobs)                       :: qlobs   ! Obs.records from QL file
  TYPE(t_stacrux)                     :: stainfo ! Station renaming
  TYPE(t_abbrev)                      :: abbinfo ! Station abbreviation

  CHARACTER(LEN=4)                    :: sessid
  CHARACTER(LEN=2)                    :: yearid
  CHARACTER(LEN=80)                   :: title
  CHARACTER(LEN=filePathLength)       :: devout
  CHARACTER(LEN=fileNameLength80)     :: scrfil
  CHARACTER(LEN=fileNameLength80)     :: stafile
  CHARACTER(LEN=fileNameLength80)     :: abbfile
  CHARACTER(LEN=4) , DIMENSION(maxsta):: staList
  CHARACTER(LEN=4)                    :: stanam, sta4
  CHARACTER(LEN=60)                   :: sitnam
  CHARACTER(LEN=20)                   :: sitnum
  INTEGER(i4b), DIMENSION(:), POINTER :: satlst

  REAL(r8b), DIMENSION(2)             :: window

  INTEGER(i4b), DIMENSION(:), ALLOCATABLE :: indexs
  INTEGER(i4b), DIMENSION(maxsta)     :: numrec
  INTEGER(i4b)                        :: istat,nstat
  INTEGER(i4b)                        :: irc
  INTEGER(i4b)                        :: nrec

  LOGICAL                             :: stafound


! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Nullify pointer
! ---------------
  NULLIFY(satlst)
  CALL init_qlfil(qlobs)
  CALL init_stacrux(stainfo)
  CALL init_abbrev(abbinfo)

! Open system files, define constants
! -----------------------------------
  CALL init_inpkey(inpkey)
  CALL readinpf('',inpkey)
  CALL opnsys
  CALL defcon(1)

! Read input options
! ------------------
  CALL qlrdopt(window,sessid,yearid,title,devout,scrfil,satlst)

! Automatic output generation
! ---------------------------
  CALL pritit('QLRINEXO','Convert SLR quick-look files to RINEX',131)
  CALL prflna(131)

! Read input QL files (copy to scratch), get list of stations
! -----------------------------------------------------------
  CALL qlrdfil(window,satlst,scrfil,stalist,nstat,numrec)

  IF (ASSOCIATED(satlst)) DEALLOCATE(satlst,stat=irc)

! Read station information file, abbreviation table
! -------------------------------------------------
  CALL gtflna(1,'STAINFO',stafile,irc)
  CALL readcrux(stafile,stainfo)

  CALL gtflna(1,'ABBREV',abbfile,irc)
  CALL readabb(abbfile,abbinfo)

! Open scratch file
! -----------------
  CALL opnfil(lfn001,scrFil,'OLD','FORMATTED','READONLY',' ',irc )
  CALL opnerr(lfnerr,lfn001,irc,scrFil,pgName)

! Loop over all stations
! ----------------------
  DO istat = 1, nstat
    nrec = numrec(istat)
    IF (nrec == 0) CYCLE ! no observations for station istat found

! Rename station according to station info file, find 4-char. abbreviation
! ------------------------------------------------------------------------
    stanam = staList(istat)
    CALL qlgtsta(stainfo,abbinfo,stanam,window,sta4,sitnam,sitnum,stafound)
    IF (.NOT. stafound) CYCLE

! Get observation records for station istat
! -----------------------------------------
    CALL qlgtrec(lfn001,stanam,nrec,qlobs)

! Sort observations in array - using sort index
! ---------------------------------------------
    ALLOCATE(indexs(nrec),stat=irc)
    CALL alcerr(irc,'indexs',(/nrec/),pgName)
    CALL dordup(qlobs%qlrec(1:nrec)%epoch(1),nrec,indexs)

! Write RINEX files
! -----------------
    CALL qlwtrx(stanam,sta4,sitnam,sitnum,qlobs,indexs,title,  &
                sessid,yearid,devout)
    DEALLOCATE(indexs,stat=irc)

  ENDDO ! End loop over all stations

! Close file
! ----------
  CLOSE(lfn001)
!  CLOSE(lfnprt)

  CALL exitrc(0)

END PROGRAM qlrinexo

