MODULE s_MENU_NEQ
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_neq(keyWord, output)

! -------------------------------------------------------------------------
! Purpose:    Generates a staList of all stations from NQ0-files
!             in ADDNEQ2 (called by the menu program via MENUAUX)
!
! Author:     R. Dach
!
! Created:    04-Jul-2001
! Last mod.:  30-Nov-2010
!
! Changes:    21-Dec-2001 HU: m_addneq replaced by p_addneq
!             18-Apr-2002 RD: Use keywords from MENUAUX.INP
!             25-Sep-2002 HU: Remove i_astlib
!             15-Nov-2002 HU: Parameter parAtm for staneq added
!             10-Dec-2002 CU: Parameter parErp for staneq added
!             23-Apr-2003 AJ: Nullify local pointers
!             18-May-2003 HU: Deallocate array
!             21-May-2003 RD: Make the deallocation safe
!             09-Jul-2003 RD: Read station info here (parameter for STANEQ)
!             16-Sep-2003 RD: STACRUX->STAINFO
!             22-Dec-2003 RS: Parameter parSap for staneq added
!             07-Feb-2004 HU: Read option noabc
!             25-May-2005 HU: Set opt%chrono
!             22-Sep-2005 RD: Use new module D_PAR.f90
!             25-Jan-2008 RD: New call of SR STANEQ (rao/rap parameters)
!             04-May-2009 RD: Scaling of loading models added
!             10-May-2009 RD: Receiver clock offsets/biases implemented
!             10-Jun-2009 RD: Read receiver/satellite clock lists from NEQ
!             24-Jun-2009 RD: Put direct constraints on DCBs
!             24-Jun-2009 DT: staInfo%staRelPar for TYPE 004 of STA-file
!             13-Aug-2009 DT: Parameter parRgb for staneq added
!             27-Nov-2009 RD: Station exceptions for pre-elimination
!             04-Mar-2010 RD: Use AOPTCRX to read the staInfo related options
!             17-Jun-2010 RD: No deallocation if no NEQ files available
!             08-Oct-2010 RD: Extension of parAtm
!             26-Oct-2010 SL: use m_bern with ONLY, removal of unused types/vars
!             30-Nov-2010 MM: NEQGSP added
!
! SR called:  gtflna, alcerr, gtfile2, staneq, gtStaFlg,
!             init_staCrux
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, t_key, &
                      keyValueLength, staNameLength, fileNameLength
  USE m_time,   ONLY: t_timint
  USE d_staCrx, ONLY: init_staCrux
  USE d_par,    ONLY: maxpartyp
  USE p_addneq, ONLY: t_parOrb, t_parGcc, t_parSao, t_parAtm,   &
                      t_parErp, t_parSap, t_parRao, t_parRap,   &
                      t_parRco, t_parRgb, t_optLoad, t_namLst,  &
                      opt, staInfo, t_parGsp
  USE s_gtfile2
  USE s_alcerr
  USE s_aoptcrx
  USE s_staneq
  USE s_readkeys
  USE s_gtflna
  USE s_ckopti
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)                        :: keyWord  ! what to do

! output:
  TYPE(t_key)                             :: output   ! name = keyWord, if OK
                                                      ! value: Result to display


! List of Functions
! --------------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER            :: srName = 'menu_neq'

! Local Variables
! ---------------
  TYPE(t_namLst),                 &
          DIMENSION(maxParTyp)   :: namList ! List of parameter names
                                            !   per parameter types
  TYPE(t_timint),                 &
          DIMENSION(maxParTyp)   :: limits
  TYPE(t_parOrb)                 :: parOrb  ! Orbital parameters
  TYPE(t_parGcc)                 :: parGcc  ! Geocenter coordinates
  TYPE(t_parSao),                 &
          DIMENSION(:),  POINTER :: parSao  ! Satellite antenna offset par.
  TYPE(t_parSap),                 &
          DIMENSION(:),  POINTER :: parSap  ! Satellite antenna pattern par.
  TYPE(t_parAtm)                 :: parAtm  ! Atmosphere parameter
  TYPE(t_parErp)                 :: parErp  ! Earth rotation parameters
  TYPE(t_parRao)                 :: parRao  ! Receiver antenna offset par.
  TYPE(t_parRap)                 :: parRap  ! Receiver antenna pattern par.
  TYPE(t_parRgb)                 :: parRgb  ! Range bias parameter
  TYPE(t_optLoad), DIMENSION(3)  :: parGrd  ! Scaling of Vienna grid files
  TYPE(t_parRco)                 :: parRco  ! Receiver clock offset par.
  TYPE(t_parGsp)                 :: parGsp  ! GNSS-spec parameters
  INTEGER(i4b)   , DIMENSION(2,3):: parDcb  ! DCB parameters

  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=staNamelength),   &
          DIMENSION(:,:),POINTER :: clkList
  CHARACTER(LEN=fileNamelength),  &
          DIMENSION(:,:),POINTER :: filNam

  INTEGER(i4b)                   :: numFil
  INTEGER(i4b)                   :: numSta
  INTEGER(i4b)                   :: iSta
  INTEGER(i4b)                   :: iTyp
  INTEGER(i4b)                   :: ii
  INTEGER(i4b)                   :: iac, irc, irCode

! Init local variables
! --------------------
  CALL init_staCrux(staInfo)

  NULLIFY(parSao)
  NULLIFY(parSap)
  NULLIFY(parAtm%trpMod)
  NULLIFY(clkList)
  NULLIFY(filNam)
  NULLIFY(keyValue)
  irCode=0

! Incorrect keyword
! -----------------
  IF (keyWord /= 'NEQ'     .AND. &
      keyWord /= 'NEQCLK'  .AND. keyWord /= 'NEQTRP'  .AND. &
      keyWord /= 'NEQDCBR' .AND. keyWord /= 'NEQGRID' .AND. &
      keyWord /= 'NEQRCLK' .AND. keyWord /= 'NEQSCLK' .AND. &
      keyWord /= 'NEQGSP'                                  ) RETURN

! Get the name of the station info file
! -------------------------------------
  CALL gtflna(0,'STAINFO',opt%stacrux,irc)

  CALL aoptcrx(opt,staInfo)

! Get all NEQ-files
! -----------------
  CALL gtfile2('INPFILE',1,numFil,filNam)

! Allocate file list for staneq
! -----------------------------
  ALLOCATE(opt%orbFil(4,numFil),stat=irc)
  CALL alcerr(irc,'opt%orbFil',(/ 4,numFil /),srName)

! Get the staList of stations
! ---------------------------
  IF (numFil > 0) THEN
    CALL readkeys("NO_ABC",keyValue,irc)
    IF (keyValue(1)=="NO") THEN
      opt%noabc = staNameLength
    ELSE IF (keyValue(1)=="YES") THEN
      opt%noabc = 14
    ELSE
      CALL ckopti(1,'NO_ABC',keyValue,'sr aoptfil',                        &
                  'Truncate station names',irc,irCode,empty=staNameLength, &
                  ge=0,le=staNameLength,result1=opt%noabc)
    ENDIF
    opt%chrono=0
    CALL staneq(0,filnam(1,:), opt%orbFil(:,:), namList, clkList,          &
                limits, parOrb, parGcc, parSao, parAtm, parErp, parSap,    &
                parRao, parRap, parRco, parDcb, parGrd, parRgb, parGsp)
  ENDIF

! Deallocate all what is not needed
! ---------------------------------
  DEALLOCATE(opt%orbFil,stat=irc)
  IF (ASSOCIATED(parSao)) THEN
    DO ii=1,SIZE(parSao)
      DEALLOCATE(parSao(ii)%nsaoff,parSao(ii)%satoff,stat=irc)
    ENDDO
    DEALLOCATE(parSao,stat=irc)
  ENDIF
  IF (ASSOCIATED(parSap)) THEN
    DO ii=1,SIZE(parSap)
      DEALLOCATE(parSap(ii)%nsaspv,parSap(ii)%satspv,stat=irc)
    ENDDO
    DEALLOCATE(parSap,stat=irc)
  ENDIF
  DEALLOCATE(parAtm%trpMod,stat=irc)

  DEALLOCATE(filnam,stat=irc)
  DEALLOCATE(staInfo%renamsta,stat=irc)
  DEALLOCATE(staInfo%stainfo,stat=irc)
  DEALLOCATE(staInfo%staprob,stat=irc)
  DEALLOCATE(staInfo%coovel,stat=irc)
  DEALLOCATE(staInfo%staRelPar,stat=irc)
  DEALLOCATE(staInfo%statype,stat=irc)

! Get station/clock list
! ----------------------
  numSta = 0
  IF (numFil > 0) THEN

    iTyp = 0
    IF (keyWord == 'NEQ')     iTyp =  1 ! Station coordinates
    IF (keyWord == 'NEQCLK')  iTyp =  2 ! Receiver clock offsets
    IF (keyWord == 'NEQTRP')  iTyp =  6 ! Troposhere
    IF (keyWord == 'NEQDCBR') iTyp =  8 ! Receiver DCBs
    IF (keyWord == 'NEQGRID') iTyp = 22 ! Grid scaling factors
    IF (keyWord == 'NEQRCLK') iTyp = 23 ! Receiver clocks
    IF (keyWord == 'NEQSCLK') iTyp = 24 ! Satellite clocks
    IF (keyWord == 'NEQGSP')  iTyp = 30 ! GNSS-specific parameters

    IF (iTyp /= 0) numSta = namList(iTyp)%nSta
  ENDIF

! Put the results into the output-key
! -----------------------------------
  ALLOCATE(output%value(numSta),stat=irc)
  CALL alcerr(irc,'output%value',(/ numSta /),srName)
  output%value = ' '

  DO iSta = 1, numSta
    WRITE(output%value(iSta), *) TRIM(namList(iTyp)%nam(iSta))
  END DO

! Deallocate the namList array
! ----------------------------
  IF (numFil > 0) THEN
    DO iTyp = 1,maxParTyp
      IF (namList(iTyp)%nSta > 0) THEN
        DEALLOCATE(namList(iTyp)%nam2,stat=iac)
        DEALLOCATE(namList(iTyp)%nam,stat=iac)
        DEALLOCATE(namList(iTyp)%num,stat=iac)
      ENDIF
    ENDDO
  ENDIF

  RETURN
END SUBROUTINE menu_neq

END MODULE
