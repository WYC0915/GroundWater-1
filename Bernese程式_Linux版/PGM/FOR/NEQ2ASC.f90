
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM neq2asc

! -------------------------------------------------------------------------
! Purpose:    Conversion binary <--> ascii of normal equation files
!
! Author:     L.Mervart
!
! Created:    25-Aug-1998
!
! Changes:    08-Sep-2000 HB: Use fileNameLength from m_bern
!             15-Feb-2001 MM: Switch to new menu
!             16-Feb-2001 MM: File array as pointer array
!             26-Jun-2001 RD: Use alcerr for allocation
!             22-Nov-2001 HU: Call pritit and cominit
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             22-Dec-2001 HU: Interfaces added
!             22-Nov-2002 RD: Argument for cominit has to be "orb"
!             23-Apr-2003 HU: Nullify local pointers
!             09-Jul-2003 RD: Init staInfo structure (used in RDSTACRX)
!             25-Aug-2003 CU: Change format string
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             30-Mar-2004 HU: Initialize noabc
!             06-Jan-2005 HU: Set staProblem%nprobl = -1
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!             27-Feb-2007 AG: Call DEFCON with parameter
!             03-Mar-2010 RD: Remove obsolete staProblem record
!             23-Sep-2010 RD: Enable CPU counter
!             15-Dec-2010 SL: use m_bern with ONLY
!             16-Dec-2010 DT: Initialize opt%covcomi
!             06-Jan-2011 MF: Nullify pointer (opt%...)
!             30-Nov-2011 SL: new title string for pritit
!             27-Jun-2012 RD: Nullify also opt%elimi
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, fileNameLength, staNameLength
  USE m_cpu,    ONLY: cpu_start
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE d_stacrx, ONLY: init_stacrux
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: t_orb,opt,staInfo
  USE s_neqain
  USE s_pritit
  USE s_cominit
  USE s_neqread
  USE s_asciineq
  USE s_defcon
  USE s_opnsys
  USE s_neqstore
  USE s_alcerr
  USE s_neqascii
  USE s_readinpf
  USE s_neqinit
  USE s_exitrc
  IMPLICIT NONE

! Local variables
! ---------------
  TYPE (t_neq)                           :: neq
  TYPE (t_orb)                           :: orb

  INTEGER(i4b)                           :: ifil
  INTEGER(i4b)                           :: nfiles
  INTEGER(i4b)                           :: irc, iac
  INTEGER(i4b)                           :: dir   ! 1 ... binary --> ascii
                                                  ! 2 ... ascii  --> binary
  CHARACTER(LEN=fileNameLength), DIMENSION(:,:), POINTER :: files

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Nullify pointers
! ----------------
  NULLIFY(files)
  NULLIFY(opt%neqFileName)
  NULLIFY(opt%orbFil)
  NULLIFY(opt%req)
  NULLIFY(opt%sigma)
  NULLIFY(opt%elimi)

! Get the name of the input file
! ------------------------------
  CALL init_inpkey(inpKey)
  CALL readinpf(' ',inpKey)

! Open system files, define constants
! -----------------------------------
  CALL opnsys
  CALL defcon(1)

! Title and filenames
! -------------------
  CALL pritit('NEQ2ASC','Convert normal equation files (binary/ASCII)')

! Read input options
! ------------------
  ALLOCATE ( opt%neqFileName(1), stat=iac )
  CALL alcerr(iac, 'opt%neqFileName', (/1/), 'neq2asc')
  ALLOCATE ( opt%orbFil(4,1), stat=iac )
  CALL alcerr(iac, 'opt%orbFil', (/4,1/), 'neq2asc')
  ALLOCATE ( opt%req(0), stat=iac )
  CALL alcerr(iac, 'opt%req', (/0/), 'neq2asc')
  ALLOCATE ( opt%sigma(0), stat=iac )
  CALL alcerr(iac, 'opt%sigma', (/0/), 'neq2asc')

  CALL neqain (nfiles, dir, files)

  opt%stacrux = ' '
  CALL init_stacrux(staInfo)
  opt%noabc = staNameLength

  opt%setupgcc = 0

  CALL neqinit(neq)
  CALL cominit(orb)

  DO ifil = 1, nfiles

    ! Conversion binary --> ascii
    ! ---------------------------
    IF (dir == 1) THEN
      opt%neqFileName(1) = files(1,ifil)
      opt%orbFil(1,1)    = ''
      opt%covcomi        = ''
      CALL neqread(neq, 1, orb)
      CALL neqascii(neq, files(2,ifil))
    END IF

    ! Conversion ascii --> binary
    ! ---------------------------
    IF (dir == 2) THEN
      opt%neqout = files(2,ifil)
      CALL asciineq(files(1,ifil), neq)
      CALL neqstore(opt%neqout,neq)
    END IF

  END DO

  DEALLOCATE(files, stat=irc)

  CALL exitrc (0)

END PROGRAM neq2asc

