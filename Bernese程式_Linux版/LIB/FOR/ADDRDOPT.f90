MODULE s_ADDRDOPT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE addrdopt ()

! -------------------------------------------------------------------------
! Purpose:    Reads input options for ADDNEQ2
!
! Author:     M. Meindl
!
! Created:    10-Jul-2001
!
! Changes:    20-Aug-2001 HU: Source input file
!             24-Oct-2001 RD: Read sta-info flags in SR aoptcrx
!             29-Oct-2001 RD: optional parameter list printing
!             07-Nov-2001 RD: STACRUX is an optional keyword
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             23-Jan-2002 CU: Get parameter inp. options from the neqs
!                             in SR aoptfil, used in SR aoptotr/aoptorb
!             18-Feb-2002 MM: renamed aopttrp to aoptatm
!             26-Mar-2002 SS: Several input/output file names added
!             06-May-2002 SS: DCB input file added
!             05-Nov-2002 HB: Add new parameter parAtm to SR aoptfil and
!                             SR aoptatm
!             10-Dec-2002 CU: Add new parameter parErp to SR aoptfil and
!                             SR aoptpol,
!                             change order of SR calls (for output)
!             17-Mar-2003 RD: Nullify pointers
!             23-Apr-2003 CU: Nullify local pointers
!             18-May-2003 HU: Array parsao already deallocated in sr
!             27-May-2003 CU: Uncomment: print source of input file
!             09-Jul-2003 RD: Move station info from t_opt to t_staCrux
!             16-Sep-2003 RD: STACRUX->STAINFO
!             11-Dec-2003 MM: Some files not longer mandatory
!             22-Dec-2003 RS: Add parSap
!             27-Jan-2004 HU: Read names of covariance files
!             22-Sep-2005 RD: Use new module D_PAR.f90
!             13-Oct-2006 AG: New option opt%sincont added
!             25-Jan-2008 RD: add RAO/RAP parameters
!             04-May-2009 RD: Scaling of loading models added
!             10-May-2009 RD: Receiver clock offsets/biases implemented
!             28-May-2009 DT: Add range bias files (RGBINP, RGBOUT)
!             10-Jun-2009 RD: Read options for receiver/satellite clocks
!             22-Jun-2009 RD: Antenna parameters extracted from AOPTOTR panel
!             24-Jun-2009 RD: Put direct constraints on DCBs
!             13-Aug-2009 DT: Add type parRgb
!             14-Oct-2009 RD: Stop if a keyword is missing
!             27-Nov-2009 RD: Station exceptions for pre-elimination
!             27-Nov-2009 RD: Antenna parameters extracted from AOPTOTR panel
!             08-Sep-2010 RD: Move Range biases to AOPTRGB
!             08-Oct-2010 RD: Extension of parAtm
!             11-Oct-2010 RD: Clean up the old option printing
!             27-Oct-2010 SL: use m_bern with ONLY
!             29-Nov-2010 MF: Add call to init_clkHead
!             29-Nov-2010 DT: Add Helmert parameters (SR aopthlm); opt%wgtOut
!             30-Nov-2010 MM: GNSS-specific parameters, new output file
!             03-May-2011 RD: Re-enable NULIFY/DEALLOCATE for keyValue
!             06-May-2011 HB: Use d_model to initialize model names
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, staNameLength, keyValueLength
  USE m_time,   ONLY: t_timint
  USE d_model,  ONLY: setModKey, chrValLength, mod_orb_prcMod
  USE d_staCrx, ONLY: init_staCrux
  USE d_par,    ONLY: maxParTyp
  USE d_clkrnx, ONLY: init_clkHead
  USE p_addneq, ONLY: opt,clkhed,prtParLst,t_parOrb,t_parGcc, &
                      t_parSao, t_parAtm, t_parErp, staInfo, t_parSap,   &
                      t_parRao, t_parRap, t_parRco, t_optLoad, t_namLst, &
                      t_parRgb, t_parGsp, t_parHlm
  USE s_aoptnet
  USE s_aoptint
  USE s_aoptgen
  USE s_aoptorb
  USE s_aoptpol
  USE s_aoptgrd
  USE s_aoptant
  USE s_aoptotr
  USE s_aoptgsp
  USE s_aoptfil
  USE s_aoptatm
  USE s_aoptpre
  USE s_aoptclk
  USE s_aoptant
  USE s_aoptrgb
  USE s_aopthlm
  USE s_ckoptb
  USE s_ckoptc
  USE s_exitrc
  USE s_gtflna
  USE s_aoptcrx
  USE s_readkeys
  USE s_rdnutsub
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:

! input/output:

! output:

! List of Functions
! -----------------

! Local Types
! -----------
  TYPE(t_parOrb)                                        :: parOrb
  TYPE(t_parGcc)                                        :: parGcc
  TYPE(t_parSao), DIMENSION(:), POINTER                 :: parSao
  TYPE(t_parSap), DIMENSION(:), POINTER                 :: parSap
  TYPE(t_parRao)                                        :: parRao
  TYPE(t_parRap)                                        :: parRap
  TYPE(t_parAtm)                                        :: parAtm
  TYPE(t_parErp)                                        :: parErp
  TYPE(t_parRco)                                        :: parRco
  TYPE(t_optLoad),DIMENSION(3)                          :: parGrd
  TYPE(t_parRgb)                                        :: parRgb
  TYPE(t_parGsp)                                        :: parGsp
  TYPE(t_parHlm)                                        :: parHlm
  TYPE(t_timint), DIMENSION(maxParTyp)                  :: limits
  TYPE(t_namLst), DIMENSION(maxParTyp)                  :: namList


! Local Parameters
! ----------------
  CHARACTER(LEN=8),PARAMETER                            :: srName = 'addRdOpt'

! Local Variables
! ---------------
  CHARACTER(LEN=staNameLength),  DIMENSION(:,:),POINTER :: clkList
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=chrValLength)                           :: chrVal
  CHARACTER(LEN=16)                                     :: nutNam, subNam

  INTEGER(i4b)  , DIMENSION(2,3)                        :: parDcb
  INTEGER(i4b)                                          :: iTyp
  INTEGER(i4b)                                          :: irc, iac
  INTEGER(i4b)                                          :: irCode

! Init opt-pointers
! -----------------
  NULLIFY(opt%neqFileName)
  NULLIFY(opt%orbFil)
!!!  NULLIFY(opt%fact)
  NULLIFY(opt%req)
  NULLIFY(opt%elimi)
  NULLIFY(opt%sigma)
  NULLIFY(parSao)
  NULLIFY(parSap)
  NULLIFY(parAtm%trpMod)
  NULLIFY(clkList)
  NULLIFY(keyValue)

  CALL init_staCrux(staInfo)
  CALL init_clkHead(clkhed)

  irCode = 0

! Several file names
! ------------------
  CALL gtflna(1, 'CONST  ', opt%const   , irc)
  CALL gtflna(1, 'DATUM  ', opt%datumfil, irc)
  CALL gtflna(0, 'SATCRUX', opt%satcrux , irc)
  CALL gtflna(0, 'SATELL ', opt%satell  , irc)
  CALL gtflna(0, 'PHASECC', opt%phasecc , irc)
  CALL gtflna(0, 'POLE   ', opt%pole    , irc)
  CALL gtflna(0, 'SINEXIN', opt%sinexin , irc)
  CALL gtflna(0, 'IONEXCF', opt%ionexcf , irc)
  CALL gtflna(0, 'STAINFO', opt%stacrux , irc)
  CALL gtflna(1, 'COORD  ', opt%coord   , irc)
  CALL gtflna(0, 'VELAPR ', opt%velapr  , irc)
  CALL gtflna(0, 'COVCOMI', opt%covcomi , irc)
  CALL gtflna(0, 'GCCINP ', opt%gccinp  , irc)
  CALL gtflna(0, 'GCCOUT ', opt%gccout  , irc)
  CALL gtflna(0, 'DCBINP ', opt%dcbinp  , irc)
  CALL gtflna(0, 'RGBINP ', opt%rgbinp  , irc)
  CALL gtflna(0, 'IONOS  ', opt%ionos   , irc)
  CALL gtflna(0, 'NEQOUT ', opt%neqout  , irc)
  CALL gtflna(0, 'COORDRS', opt%coordrs , irc)
  CALL gtflna(0, 'VELORS ', opt%velors  , irc)
  CALL gtflna(0, 'DCBOUT ', opt%dcbout  , irc)
  CALL gtflna(0, 'ISBOUT ', opt%isbout  , irc)
  CALL gtflna(0, 'IONOSRS', opt%ionosrs , irc)
  CALL gtflna(0, 'IONEXRS', opt%ionexrs , irc)
  CALL gtflna(0, 'SINEXRS', opt%sinexrs , irc)
  CALL gtflna(0, 'PARAMIN', opt%paramin , irc)
  CALL gtflna(0, 'PARAMRS', opt%paramrs , irc)
  CALL gtflna(0, 'TROPSAV', opt%tropsav , irc)
  CALL gtflna(0, 'TROPSNX', opt%tropsnx , irc)
  CALL gtflna(0, 'POLERS ', opt%polers  , irc)
  CALL gtflna(0, 'IERSPOL', opt%ierspol , irc)
  CALL gtflna(0, 'PLOTRS ', opt%plotrs  , irc)
  CALL gtflna(0, 'COVARRS', opt%covarrs , irc)
  CALL gtflna(0, 'COVTTRS', opt%covttrs , irc)
  CALL gtflna(0, 'RGBOUT ', opt%rgbout  , irc)
  CALL gtflna(0, 'COVCOMO', opt%wgtout  , irc)
  CALL gtflna(0, 'SYSOUT ', opt%sysout  , irc)
  CALL gtflna(0, 'SYSERR ', opt%syserr  , irc)
  CALL gtflna(1, 'AUXFIL ', opt%auxfil  , irc)

  CALL ckoptb(1,(/'PRT_PAR'/),'addrdopt','Print parameter list',irc, &
              result1=opt%prt(prtParLst))

  IF (opt%sinexrs /= '') THEN
    CALL readkeys("SNXCONT",keyvalue,irc)
    CALL ckoptc(1,'SNXCONT', keyValue, &
               (/'COV','NEQ'/), &
                 'addrdopt', 'Content of SINEX', irc, irCode, &
                  valList=(/2,1/), result1=opt%sincont)
  ENDIF

! Read flags for the use of the sta-info file
! -------------------------------------------
  CALL aoptcrx(opt,staInfo)

! Get NEQ and orbit filenames, infos about STD/RPR files and time limits
! ----------------------------------------------------------------------
  CALL aoptfil(opt, namList, clkList, &
               limits, parOrb, parGcc, parSao, parAtm, parErp, parSap, &
               parRao, parRap, parRco, parDcb, parGrd, parRgb, parGsp)

! General options
! ---------------
  CALL aoptgen(opt)

! Parameter intervals
! -------------------
  CALL aoptint(opt, limits)

! Parameter preelimination
! ------------------------
  CALL aoptpre(opt, namList)

! Constraining of coordinates and velocities
! ------------------------------------------
  CALL aoptnet(opt, namList(1))

! Options and constraining for atmosphere parameters
! ---------------------------------------------------
  CALL aoptatm(opt, parAtm)

! Options and constraining for orbit parameters
! ---------------------------------------------
  CALL aoptorb(opt, parOrb)

! Options and constraining for earth rotation parameters
! ------------------------------------------------------
  CALL aoptpol(opt, parErp)

! Options for epochwise clock estimation
! --------------------------------------
  CALL aoptclk(opt, clkList, clkhed)

! Options and constraining for antenna parameters
! -----------------------------------------------
  CALL aoptant(opt, parSao, parSap, parRao, parRap)

! Options and constraining for scaling factors of Vienna grid files
! -----------------------------------------------------------------
  CALL aoptgrd(opt, parGrd)

! Options and constraining for range-bias parameters
! --------------------------------------------------
  CALL aoptrgb(opt, parRgb)

! Options and constraining for GNSS-specific parameters
! -----------------------------------------------------
  CALL aoptgsp(opt, parGsp, namList(30))

! Options and constraining for remaining parameters
! -------------------------------------------------
  CALL aoptotr(opt, parGcc, parRco, parDcb)

! Options and constraining for Helmert parameters
! -----------------------------------------------
  CALL aopthlm(opt, parHlm)

! Set model information
! ---------------------
  CALL rdnutsub(nutnam,subnam)
  chrVal = ' '
  chrVal(1:4)='BIAS'
  CALL setModKey(mod_orb_prcMod,chrVal,srName,0.D0)

  DEALLOCATE(clkList,stat=irc)
  DEALLOCATE(keyValue,stat=irc)

! Deallocate the namList array
! ----------------------------
  DO iTyp = 1,maxParTyp

    IF (namList(iTyp)%nSta > 0) THEN
      DEALLOCATE(namList(iTyp)%nam2,stat=iac)
      DEALLOCATE(namList(iTyp)%nam,stat=iac)
      DEALLOCATE(namList(iTyp)%num,stat=iac)
    ENDIF
  ENDDO

  IF (irCode /= 0) CALL exitrc(2)

END SUBROUTINE addrdopt

END MODULE

