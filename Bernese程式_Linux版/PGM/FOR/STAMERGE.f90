! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM stamerge

! -----------------------------------------------------------------------------
! Purpose:
!
! Author:     A.Steinbach
!
! Created:    07-Nov-2007
!
! Changes:    06-Jan-2011 SL: use m_bern with ONLY, enable CPU counter
!             08-Feb-2011 SL: get technique from master STAINFO file
!             15-Feb-2011 SL: set default technique to GNSS
!             16-Sep-2011 SL: no opnerr call, no lfn001, no variable file
!             02-Dec-2011 SL: new title string for pritit
!             20-Sep-2012 RD: Remove unused variables in SELACT
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -----------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b,lfnErr
  USE m_cpu,    ONLY: cpu_start
  USE d_inpkey, ONLY: inpKey,init_inpkey
  USE d_stalst, ONLY: init_staList
  USE d_stacrx, ONLY: t_stacrux,init_stacrux
  USE p_stamrg, ONLY: t_stamrg_opt
  USE s_defcon
  USE s_opnsys
  USE s_exitrc
  USE s_prflna
  USE s_pritit
  USE s_readinpf
  USE s_siminp
  USE s_prfile
  USE s_readcrux
  USE s_writcrux
  USE s_alcerr
  USE s_selopt
  USE s_selact
  USE s_alcerr
  USE s_sortup
  IMPLICIT NONE

! Local variables
! ---------------
! parameters
  CHARACTER(LEN=8),PARAMETER          :: pgmNam = "STAMERGE"

  TYPE(t_stamrg_opt)                  :: opt
  TYPE(t_stacrux)                     :: master,second,remisc

  INTEGER(i4b)                        :: iac,i,ios

  CALL init_staList(opt%staList)
  CALL init_stacrux(master)
  CALL init_stacrux(second)
  CALL init_stacrux(remisc)

! Start CPU counter
! -----------------
  CALL cpu_start(.TRUE.)

! Get the name of the input file
! ------------------------------
  CALL init_inpkey(inpKey)
  CALL readinpf(' ',inpKey)

! Open system files and define constants
! --------------------------------------
  CALL opnsys
  CALL defcon(0)

! Print header and files
! ----------------------
  CALL pritit(pgmNam,'Merge station information files',132)
  CALL prflna(132)

! Read options
! ------------
  CALL siminp(opt)

  IF(LEN_TRIM(opt%resFil) == 0.AND. &
      (opt%ctype01 == 2.OR.opt%ctype01 == 3.OR. &
       opt%ctype02 == 2.OR.opt%ctype02 == 3.OR. &
       opt%ctype03 == 2.OR.opt%ctype03 == 3.OR. &
       opt%ctype04 == 2.OR.opt%ctype04 == 3.OR. &
       opt%ctype05 == 2.OR.opt%ctype05 == 3)) THEN
     WRITE(lfnerr,"(/,' *** PG STAMERGE: Selected options require the indication ',/, &
     & 18X, 'of a result file.',//, &
     & 18X, 'Program stopped!')")
     CALL exitrc(2)
  ENDIF

! Read the master STAINFO file
! ----------------------------
  CALL readcrux(opt%mtrFil,master)
  CALL selopt(master,opt%staList,opt%timwin)

! Set technique string of combined file
! -------------------------------------
  IF(master%technique == "") master%technique = "GNSS"
  remisc%technique = master%technique

! Read the second STAINFO file
! ----------------------------
  IF(LEN_TRIM(opt%secFil) > 0) THEN
    CALL readcrux(opt%secFil,second)
    CALL selopt(second,opt%staList,opt%timwin)

! Allocate arrays for combined file
! ---------------------------------
    ALLOCATE(remisc%renamSta(master%nrenam+second%nrenam),stat=iac)
    CALL alcerr(iac,'remisc%renamSta',(/master%nrenam+second%nrenam/),pgmNam)
    ALLOCATE(remisc%stainfo(master%ninfo+second%ninfo),stat=iac)
    CALL alcerr(iac,'remisc%stainfo',(/master%ninfo+second%ninfo/),pgmNam)
    ALLOCATE(remisc%staprob(master%nprob+second%nprob),stat=iac)
    CALL alcerr(iac,'remisc%staprob',(/master%nprob+second%nprob/),pgmNam)
    ALLOCATE(remisc%coovel(master%ncoovel+second%ncoovel),stat=iac)
    CALL alcerr(iac,'remisc%coovel',(/master%ncoovel+second%ncoovel/),pgmNam)
    ALLOCATE(remisc%statype(master%nstatype+second%nstatype),stat=iac)
    CALL alcerr(iac,'remisc%statype',(/master%nstatype+second%nstatype/),pgmNam)

!   Apply selection actions to station info files
!   ---------------------------------------------
    CALL selact(master,second,remisc, &
                opt%ctype01,opt%ctype02,opt%ctype03,opt%ctype04,opt%ctype05, &
                opt%crant,opt%crrec,opt%cconsid)

!   Sort combined file
!   ------------------
    CALL sortup(remisc)

!   Write combined file
!   -------------------
    IF(LEN_TRIM(opt%resFil) > 0) CALL writcrux(opt%resFil,remisc)

  ELSE

!   No secondary file available
!   ---------------------------
    IF(opt%csort == 1) CALL sortup(master)
    IF(LEN_TRIM(opt%resFil) > 0) CALL writcrux(opt%resFil,master)

  ENDIF

! Program ends here
! -----------------
  CALL exitrc(0)

END PROGRAM stamerge
