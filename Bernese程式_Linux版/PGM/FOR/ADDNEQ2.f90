
! -------------------------------------------------------------------------
! Bernese GNSS Software Version 5.2
! -------------------------------------------------------------------------

PROGRAM addneq2

! -------------------------------------------------------------------------
! Purpose:    Stacking of Normal Equations
!
! Author:     L.Mervart
!
! Created:    22-Nov-1997
!
! Changes:    09-Mar-2000 LM: allow indvSol > 1
!             18-May-2000 SS: Handle DCB parameters
!             26-Jun-2001 RD: Use alcerr for allocation
!             31-Jul-2001 MM: switch to new menu
!             18-Oct-2001 RD: generate a parameter list for printing
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             22-Dec-2001 HU: Interfaces added
!             27-Mar-2002 SS: Store ION and INX results
!             25-Sep-2002 HU: Remove i_astlib
!             09-Oct-2002 CU: Add SR neqdelno to delete parameters with
!                             no/less observations
!             11-Nov-2002 RD: Finish program with SR exitrc
!             12-Nov-2002 CU: New title line value written by SR prfile,
!                             Set optional paramater ncol = 131 for
!                             SR prflna and SR prfile
!             24-Jan-2003 CU: Report singular parameters
!             27-Feb-2003 HU: Use DATUM from D_DATUM
!             23-Apr-2003 HU: Nullify local pointers
!             01-May-2003 HU: Move PRFILE to STANEQ
!             06-May-2003 MM: NQ0 sorting and new output
!             27-May-2003 CU: New SR prparlst, remove SR neqprpar
!             07-Jul-2003 MM: New aprtrans call
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             27-Jan-2004 HU: New routine covstore
!             07-Feb-2004 HU: filtitle and maxlcq not used
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             12-Dec-2005 CU: Add SR wgtstore, 3rd parameter for call of
!                             SR neqsolve, add SR neqdelno call when
!                             comparing solutions
!             27-Feb-2006 CU: New variable "statist" added, for printing of
!                             individual solution statistic
!             09-Jun-2006 RD: Compute no solution
!             31-Oct-2006 AG: Store SINEX with NEQ implemented
!             08-Feb-2007 RD: New call of PRPARLST
!             27-Feb-2007 AG: Call DEFCON with parameter
!             25-Mar-2008 RD: Store PCV estimates
!             06-May-2009 RD: New options for satellite antennas
!             06-May-2009 RD: Repeatability for all parameters in a PLT-file
!             28-May-2009 DT: Store range biases (SR rgbstore)
!             10-Jun-2009 RD: Add epoch satellite/receiver clocks
!             30-Oct-2009 SL: ipart added to NEQREAD call
!             03-Sep-2010 DT: Add orb to call of NEQDELNO
!             23-Sep-2010 RD: Enable CPU counter
!             27-Oct-2010 SL: use m_bern with ONLY
!             30-Nov-2010 MM: New output file
!             02-Dec-2010 DT: Add hlmOut to Call of NEQSOLVE; change WGTSTORE
!             14-Dec-2010 RD: Indicate part=3 in NEQSOLVE for repeatability
!             05-May-2011 RD/LP: Repeat NEQELIM after ORBTRANS in order to
!                             delete the pulses for GALILEO satellites
!             24-Nov-2011 SL: new title string for pritit
!             20-Jan-2012 RD: Repeatability wrt. the main solution
!             15-Jun-2012 MM: Call sintran1 only if necessary
!             05-Oct-2012 RD: Call NEQPRT also in noInv is active
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b,r8b,lfnLoc,lfnErr,lfnPrt
  USE m_cpu,    ONLY: cpu_start
  USE d_inpkey, ONLY: inpKey,init_inpkey
  USE d_datum,  ONLY: datum
  USE d_neq,    ONLY: t_neq,writeRec
  USE p_addneq, ONLY: t_orb,t_dx,opt,t_hlmFil
  USE s_difprint
  USE s_neqelim
  USE s_prparlst
  USE s_opnfil
  USE s_neqdelno
  USE s_neqprt
  USE s_neqtrans
  USE s_pritit
  USE s_cominit
  USE s_orbtrans
  USE s_neqsort
  USE s_dcbstore
  USE s_getdat
  USE s_neqread
  USE s_aprhelm
  USE s_neqalloc
  USE s_crdstore
  USE s_defcon
  USE s_neqclean
  USE s_parelimi
  USE s_opnsys
  USE s_neqstore
  USE s_aprtrans
  USE s_addrdopt
  USE s_orbstore
  USE s_isbstore
  USE s_neqsolve
  USE s_alcerr
  USE s_prflna
  USE s_difcomp
  USE s_polstore
  USE s_neqstack
  USE s_readinpf
  USE s_opnerr
  USE s_neqinit
  USE s_sintran1
  USE s_covstore
  USE s_ionstore
  USE s_exitrc
  USE s_adweight
  USE s_trpstore
  USE s_wgtstore
  USE s_pcvstore
  USE s_clkstore
  USE s_rgbstore
  USE s_eststore
  USE s_parstack
  IMPLICIT NONE

! Local variables
! ---------------
  TYPE(t_neq)                             :: neq_1, neq
  TYPE(t_orb)                             :: orb
  TYPE(t_hlmFil)                          :: hlmOut
  TYPE(t_dx),DIMENSION(:,:),ALLOCATABLE   :: dx

  INTEGER(i4b)                            :: ifil
  INTEGER(i4b)                            :: ipart
  INTEGER(i4b)                            :: ios, iac
  INTEGER(i4b)                            :: ireq

  REAL(r8b),DIMENSION(:,:),ALLOCATABLE    :: statist
  REAL(r8b),DIMENSION(9)                  :: dummy

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Get the name of the input file
! ------------------------------
  CALL init_inpkey(inpKey)
  CALL readinpf(' ',inpKey)

! Open system files, define constants
! -----------------------------------
  CALL opnsys
  CALL defcon(1)

! Automatic output generation
! ---------------------------
  CALL pritit('ADDNEQ2','Combine normal equation systems',131)
  CALL prflna(131)

! Define geodetic datum
! ---------------------
  datum%name = 'WGS - 84'
  CALL getdat(datum%name,datum%aell,datum%bell,datum%dxell, &
              datum%drell,datum%scell)

! Read input options
! ------------------
   CALL addrdopt

! Initialization and memory allocation
! ------------------------------------
   CALL cominit(orb)
   CALL neqinit(neq)
   CALL neqinit(neq_1)
   CALL neqalloc(neq,opt%maxpar)
   CALL prparlst(0,0,0,neq%par(1)%name,neq%par(1)%locq,neq%par(1)%time)

! First part of the program
! -------------------------
  ipart = 1
  DO ifil = 1, SIZE(opt%neqFileName)          ! Loop over all NEQ-files
    CALL neqread(neq_1,ifil,orb,ipart)        ! Read the next NEQ-file
    CALL neqdelno(neq_1,ifil,orb)             ! Delete parameters with no obs.
    CALL neqclean(neq_1)                      ! Clean NEQ system
    CALL aprhelm(neq_1,ifil)                  ! A priori Helmert transf.
    CALL aprtrans(neq_1,ipart,iFil)           ! Change a priori values
    CALL parstack(neq_1)                      ! Param. stacking within a NEQ
    CALL neqclean(neq_1)                      ! Clean NEQ system
    CALL neqtrans(neq_1,ipart,ifil)           ! Transform parameters
    CALL neqelim(neq_1,ifil)                  ! Delete parameters
    CALL neqclean(neq_1)                      ! Clean NEQ system
    CALL adweight(neq_1,ipart,ifil)           ! Set a priori weights
    CALL parelimi(neq_1,ipart,ifil)           ! Parameter pre-elimination
    CALL neqclean(neq_1)                      ! Clean NEQ system
    CALL orbtrans(neq_1,ifil,orb)             ! Orbit transformation
    CALL neqelim(neq_1,ifil)                  ! Delete parameters
    CALL neqclean(neq_1)                      ! Clean NEQ system
    CALL neqstack(neq,neq_1,ifil)             ! Stacking of the NEQs
  END DO

  CALL neqalloc(neq_1,0)                      ! Deallocate memory

! Second part of the program
! --------------------------
  ipart=2
  CALL aprtrans(neq,ipart,0)                  ! Change a priori values
  CALL adweight(neq,ipart,0)                  ! Set a priori weights
  CALL parelimi(neq,ipart,0)                  ! Parameter pre-elimination
  CALL neqclean(neq)                          ! Clean NEQ system
  CALL neqsort(neq)                           ! Sort the NQ0 system
  CALL neqstore(opt%neqout,neq)               ! Store NEQ system into file

! Stop to store only the new (no solution)
! ----------------------------------------
  IF ( opt%noinv ) THEN
    IF (opt%sincont==1) &
      CALL sintran1(neq)                      ! Transform NEQ and store SINEX
!    CALL prparlst(2,0,0,neq%par(1)%name,neq%par(1)%locq, &
!                  neq%par(1)%time,neq%misc%nparms)
    CALL neqprt(lfnprt,0,0,neq)               ! Print parameter statistics

    WRITE(lfnprt,*) 'Solution skipped ...'

    CALL exitrc(0)
  ENDIF

! Store aNor into the file (further used for SINEX)
! -------------------------------------------------
  IF ( opt%sinexrs /= '') THEN
    CALL opnfil(lfnloc,opt%sinexrs,'UNKNOWN','UNFORMATTED',' ',' ',ios)
    CALL opnerr(lfnerr,lfnloc,ios,opt%sinexrs,'ADDNEQ2')
    CALL writeRec(lfnloc,neq%misc%npar*(neq%misc%npar+1)/2,neq%aNor)
    CLOSE(lfnloc)
  END IF

! Invert the NEQ System, compute the Solution
! -------------------------------------------
  CALL adweight(neq,0,0)                      ! Set a priori weights
  CALL neqsolve(neq,ipart,0,dummy, hlmOut)    ! Compute the solution
  CALL neqprt(lfnprt,1,0,neq)                 ! Print the results
  CALL covstore(neq)                          ! Store covariances
  CALL crdstore(neq)                          ! Store coordinates
  CALL orbstore(neq,orb)                      ! Store orbits (ELE file)
  CALL polstore(neq)                          ! Store EOP results
  CALL trpstore(neq)                          ! Store troposphere
  CALL dcbstore(neq)                          ! Store DCB results
  CALL ionstore(neq)                          ! Store ION and INX results
  CALL pcvstore(neq)                          ! Store RAO,RAP,SAP results
  CALL clkstore(neq)                          ! Store clock results
  CALL isbstore(neq)                          ! Store inter-syst. bias results
  CALL rgbstore(neq)                          ! Store range biases
  CALL eststore(neq)                          ! Store all estimated parameters
  IF (opt%sinexrs /= '') CALL sintran1(neq)   ! Transform NEQ and store SINEX

! Compare the Solutions stemming from individual NEQ-files
! --------------------------------------------------------
  IF ( opt%indvSol > 0 ) THEN
    DO ireq = 1, SIZE(opt%req)
      IF (opt%req(ireq)%locq(1) == 1) opt%req(ireq)%locq(1) = 0
    END DO
    ALLOCATE( dx(SIZE(opt%neqFileName),neq%misc%npar), stat=iac )
    CALL alcerr(iac, 'dx', (/SIZE(opt%neqFileName),neq%misc%npar/), 'addneq2')
    ALLOCATE( statist(SIZE(opt%neqFileName),9), stat=iac )
    CALL alcerr(iac, 'statist', (/SIZE(opt%neqFileName),9/), 'addneq2')
    DO ifil = 1, SIZE(opt%neqFileName)          ! Loop over all NEQ-files
      CALL neqread(neq_1,ifil,orb,ipart)        ! Read the next NEQ-file
      CALL neqdelno(neq_1,ifil,orb)             ! Delete parameters with no obs.
      CALL aprhelm(neq_1,ifil)                  ! A priori Helmert transf.
      CALL aprtrans(neq_1,0,0,neq)              ! Change a priori values
      CALL parstack(neq_1)                      ! Param. stacking within a NEQ
      CALL neqclean(neq_1)                      ! Clean NEQ system
      CALL neqtrans(neq_1,0,0)                  ! Transform parameters
      CALL neqelim(neq_1,0)                     ! Delete paramters
      CALL neqclean(neq_1)                      ! Clean NEQ system
      CALL adweight(neq_1,0,0,neq)              ! Set a priori weights
      CALL parelimi(neq_1,0,0)                  ! Parameter pre-elimination
      CALL neqclean(neq_1)                      ! Clean NEQ system
      CALL neqsolve(neq_1,3,ifil,  &
                    statist(ifil,:), hlmOut)    ! Compute the solution
      CALL difcomp(ifil,neq,neq_1,dx(ifil,:))   ! Compute the differences
    END DO
    CALL neqalloc(neq_1,0)                      ! Deallocate memory
    CALL difprint(neq,SIZE(opt%neqFileName),dx,statist) ! Print the differences
    DEALLOCATE(dx, stat=iac)
  END IF

! Write var. rescaling factors and Helmert parameters
! ---------------------------------------------------
  IF(opt%wgtout /= '') THEN
    CALL wgtstore(hlmOut)
  ENDIF

  CALL exitrc(0)

END PROGRAM addneq2
