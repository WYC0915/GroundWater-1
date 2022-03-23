MODULE s_AOPTGEN
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE aoptgen(opt)

! -------------------------------------------------------------------------
! Purpose:    Reads general input options for ADDNEQ2:
!             title, sigma0, maxpar, timref, indvsol
!
! Author:     M. Meindl
!
! Created:    10-Jul-2001
!
! Changes:    17-Sep-2001 RD: Define a bad solution for comparison
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             30-Jan-2002 CU: Write general options in the protocol(sigma0)
!             06-Jun-2002 HU: use m_bern as first statement
!             06-Aug-2002 HU: Read new options
!             10-Dec-2002 CU: Change title line in protocol:
!                             "general options" -> "a priori information"
!             28-Jan-2003 AJ: Read new option (opt%snxinc)
!             04-Feb-2003 MM: Extended solution section
!             23-Apr-2003 CU: Nullify local pointers
!             01-May-2003 MM: timRefCrd modified
!             21-May-2003 MM: dts for relative sigmas added
!             27-May-2003 CU: Print a priori information:
!                             maxpar, comparison of ind. solutions
!             11-Dec-2003 MM: new option: SNXREG, STASORT now checkbox
!             19-May-2005 CU: Remove redundant output string
!             24-Apr-2006 AG: Read new option (opt%antred)
!             09-Jun-2006 RD: Option noinv added
!             01-May-2007 AG: Read new option (opt%erprep)
!             14-May-2008 RD: No ERP-trafo for SINEX-NEQ permitted
!             08-Nov-2010 RD: Select HELMERT parameter for repeatability
!             25-Jan-2012 RD: Correct length of description for cktopr
!             13-Jun-2012 MM: DT for relative sigmas in "hh mm ss" format
!             18-Jul-2012 RD: Transform ERP also in case of NEQ-SINEX
!             18-Jul-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnprt, lfnerr, keyValueLength
  USE p_addneq, ONLY: t_opt, prtExt
  USE s_ckopti
  USE s_ckoptr
  USE s_ckoptt
  USE f_djul
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  IMPLICIT NONE

! List of parameters
! ------------------
! input:

! output:
  TYPE(t_opt)           :: opt    ! Options for ADDNEQ2

! Local parameters
! ----------------
  CHARACTER(LEN=7), PARAMETER :: srName = 'aoptgen'
! Local variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue

  REAL(r8b)                                             :: day

  INTEGER(i4b)                                          :: year, month
  INTEGER(i4b)                                          :: ios, irc, irCode
  INTEGER(i4b)                                          :: check

  LOGICAL                                               :: estVelo,staSort

  irCode = 0
  NULLIFY(keyValue)

! Title line for output
! ---------------------
  CALL readkeys('TITLE' , keyValue, irc)
  IF (TRIM(keyValue(1)) == '')                                            &
    keyValue(1) = 'YOU SHOULD REALLY ENTER YOUR OWN TITLE!'
  opt%title = keyValue(1)(1:LEN_TRIM(keyValue(1)))

! A priori sigma
! --------------
  CALL readkeys('SIGMA0', keyValue, irc)
  IF (TRIM(keyValue(1)) == '') keyValue(1) = '0.0'
  IF (irc == 0) READ(keyValue(1), *, iostat=ios) opt%sigma0
  IF (irc /= 0 .OR. ios /= 0 .OR. opt%sigma0 < 0d0) THEN
    WRITE(lfnerr,'(/,A,/,17X,A,A,/)')                                     &
    ' *** SR AOPTGEN: Wrong entry for the apriori sigma detected.',       &
                     'Specified value:  ', TRIM(keyValue(1))
    CALL exitrc(2)
  ENDIF

! Maximum number of parameters in NEQ
! -----------------------------------
  CALL readkeys('MAXPAR', keyValue, irc)
  IF (irc == 0) READ(keyValue(1), *, iostat=ios) opt%maxpar
  IF (irc /= 0 .OR. ios /= 0) THEN
    WRITE(lfnerr,'(/,A,/,17X,A,A,/)')                                     &
    ' *** SR AOPTGEN: Wrong entry for maximum parameters detected.',      &
                     'Specified value:  ', TRIM(keyValue(1))
    CALL exitrc(2)
  ENDIF

! Individual solutions
! --------------------
  opt%ipHelm(:) = 0
  CALL readkeys('INDVSOL', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == 'YES') THEN
    opt%indvSol = 1
  ELSE IF (irc == 0 .AND. keyValue(1) == 'HELMERT') THEN
    opt%indvSol = 3
    opt%ipHelm(:) = 1
  ELSE IF (irc == 0 .AND. keyValue(1) == 'HLM_TRA') THEN
    opt%indvSol = 2
    opt%ipHelm(1:3) = 1
  ELSE IF (irc == 0 .AND. keyValue(1) == 'HLM_ROT') THEN
    opt%indvSol = 2
    opt%ipHelm(4:6) = 1
  ELSE IF (irc == 0 .AND. keyValue(1) == 'HLM_ALL') THEN
    opt%indvSol = 2
    opt%ipHelm(:) = 1
  ELSE
    opt%indvSol = 0
  END IF

! Define a bad solution for comparison
! ------------------------------------
  opt%minSol = 0
  opt%badSol = 0d0
  IF (opt%indvSol /= 0) THEN
    CALL readKeys('MINSOL', keyValue,irc)
    CALL ckopti(1,'MINSOL', keyValue,srname, &
                'Minimum number of solutions for each station',irc,irCode, &
                maxVal=1,ge=0,empty=0,result1=opt%minSol)

    CALL readKeys('BADSOL_N', keyValue,irc)
    CALL ckoptr(1,'BADSOL_N', keyValue,srname, &
                'Maximum tolerated residual, north',irc,irCode, &
                maxVal=1,ge=0d0,empty=0d0,result1=opt%badSol(1,1))

    CALL readKeys('BADSOL_E', keyValue,irc)
    CALL ckoptr(1,'BADSOL_E', keyValue,srname, &
                'Maximum tolerated residual, east',irc,irCode, &
                maxVal=1,ge=0d0,empty=0d0,result1=opt%badSol(1,2))

    CALL readKeys('BADSOL_U', keyValue,irc)
    CALL ckoptr(1,'BADSOL_U', keyValue,srname, &
                'Maximum tolerated residual, up',irc,irCode, &
                maxVal=1,ge=0d0,empty=0d0,result1=opt%badSol(1,3))

    CALL readKeys('BADSOLRN', keyValue,irc)
    CALL ckoptr(1,'BADSOLRN', keyValue,srname, &
                'Maximum tolerated RMS error, north',irc,irCode, &
                maxVal=1,ge=0d0,empty=0d0,result1=opt%badSol(2,1))

    CALL readKeys('BADSOLRE', keyValue,irc)
    CALL ckoptr(1,'BADSOLRE', keyValue,srname, &
                'Maximum tolerated RMS error, east',irc,irCode, &
                maxVal=1,ge=0d0,empty=0d0,result1=opt%badSol(2,2))

    CALL readKeys('BADSOLRU', keyValue,irc)
    CALL ckoptr(1,'BADSOLRU', keyValue,srname, &
                'Maximum tolerated RMS error, up',irc,irCode, &
                maxVal=1,ge=0d0,empty=0d0,result1=opt%badSol(2,3))
  ENDIF

! Reference epoch
! ---------------
  CALL ckoptb(1,(/'CRD_NINT'/),'sr aoptgen','Estimate velocities',         &
              irCode,resultL=estVelo)

  CALL readkeys('TIMREF', keyValue, irc)
  IF (TRIM(keyValue(1)) /= '' .AND. estVelo) THEN
    IF (irc == 0) READ(keyValue(1), *, iostat=ios) year, month, day
    IF (year < 0    .OR. month < 0   .OR. month > 12 .OR.                  &
         day < 0.d0 .OR. day > 31.d0 .OR. irc /= 0   .OR. ios /= 0) THEN
      WRITE(lfnerr,'(/,A,/,17X,A,A,/)')                                   &
      ' *** SR AOPTGEN: Wrong entry for reference epoch detected',        &
                       'Specified value:  ', TRIM(keyValue(1))
      CALL exitrc(2)
    ELSE
      opt%timRefCrd = djul(year, month, day)
    ENDIF
  ENDIF


! Regularize a priori constraint matrix
! -------------------------------------
  opt%snxReg = 0.d0
  IF (opt%sinexrs /= '') THEN
    CALL readkeys("SNXREG",keyValue,irc)
    IF (keyValue(1)=="NO") THEN
      opt%snxReg = 0.d0
    ELSE IF (keyValue(1)=="YES") THEN
      opt%snxReg = 1.d-7
    ELSE
      CALL ckoptr(1,'SNXREG',keyValue,'sr aoptgen',                        &
                  'Regularize constraint matrix',irc,irCode,empty=0.d0,    &
                  ge=0.d0,result1=opt%snxReg)
    ENDIF
  ENDIF


! Header in old addneq style for Euref
! ------------------------------------
  CALL readKeys('HEUREF', keyValue,irc)
  IF (irc == 0) READ(keyValue(1),*,iostat=ios) opt%ieuref
  IF (irc /= 0 .OR. ios /= 0) THEN
    WRITE(lfnerr,'(/,A,/,17X,A,/)')                                        &
      ' *** SR AOPTGEN: Wrong value for Euref header selection',           &
                       'Specified value:  ', TRIM(keyValue(1))
    CALL exitrc(2)
  ENDIF

! Sort stations in sinex according to DOMES
! -----------------------------------------
  CALL ckoptb(1,(/'STASORT'/),'sr aoptgen','Sort statins in SINEX',       &
              irCode,resultL=staSort)
  IF (staSort) THEN
    opt%staSort(1)=6
    opt%staSort(2)=14
  ELSE
    opt%staSort(1)=0
    opt%staSort(2)=0
  END IF
  IF (irCode/=0) CALL exitrc(2)


! Do not write non-calibrated antenna / radome combinations
! ---------------------------------------------------------
  CALL ckoptb(1, (/'ANTRED'/), 'sr aoptgen (pg addneq2)',    &
              'Do not output non-calibrated antenna/radomes', irCode,  &
              result1=opt%antred)

! Transform ERP to offset/drift representation
! --------------------------------------------
!  IF ( opt%sincont == 1 ) THEN        ! not permitted for SINEX-NEQs
!    opt%erprep = 0
!  ELSE
    CALL ckoptb(1, (/'ERPREP'/), 'sr aoptgen (pg addneq2)',    &
                'Transform ERP to offset/drift representation', irCode,  &
                result1=opt%erprep)
!  ENDIF

! Write SINEX Inconsistencies
! ---------------------------
  CALL ckoptb(1, (/'PRT_SIC'/), 'sr aoptgen (pg addneq2)',    &
              'Print SINEX inconsistencies', irCode,  &
              result1=opt%snxinc)

! Print extended solution section
! -------------------------------
  CALL ckoptb(1,(/'PRT_EXT'/),'sr aoptgen','Print additional columns',&
              irCode,result1=opt%prt(prtExt))

! No solution, store only NEQ
! ---------------------------
  CALL ckoptb(1,(/'NOINV'/),'sr aoptgen','Compute no solution',&
              irCode,resultl=opt%noinv)

! Get dts for relative sigmas
! ---------------------------
  opt%dtRelSig = 0.d0

! troposphere
  CALL readKeys('TRP_RDT',keyValue,irc)
  CALL ckoptt(1,'TRP_RDT',keyValue,'sr aoptgen',  &
              'Maximum dt for relative constraining',irc,irCode,  &
              empty=0d0,ge=0d0,maxVal=1,result1=opt%dtRelSig(1))
  CALL readKeys('GRD_RDT',keyValue,irc)
  CALL ckoptt(1,'GRD_RDT',keyValue,'sr aoptgen',  &
              'Maximum dt for relative constraining',irc,irCode,  &
              empty=0d0,ge=0d0,maxVal=1,result1=opt%dtRelSig(2))

! global ionosphere parameters
  CALL readKeys('GIM_RDT',keyValue,irc)
  CALL ckoptt(1,'GIM_RDT',keyValue,'sr aoptgen',  &
              'Maximum dt for relative constraining',irc,irCode,  &
              empty=0d0,ge=0d0,maxVal=1,result1=opt%dtRelSig(3))

  opt%dtRelSig = opt%dtRelSig/24.d0


! Write a priori sigma of unit weight into protocol
! -------------------------------------------------
  WRITE(lfnprt,'(///,2(/,A),/)')                                       &
    ' A PRIORI INFORMATION',' --------------------'

  WRITE(lfnprt,'(A,I8,/)')                                             &
    ' Number of parameters in combined NEQ less than: ', opt%maxpar

  WRITE(lfnprt,'(A,2X,F6.4,A,//)')                                     &
    ' A priori sigma of unit weight:                  ', opt%sigma0,' m'

! Write a priori information (check comparison of solutions)
! ----------------------------------------------------------
  IF (opt%indvSol /= 0) THEN
    check = 0

    WRITE(lfnprt,'(A,/,A,/)')                       &
      ' Check comparison of individual solutions:', &
      ' ----------------------------------------'

    IF (opt%minSol > 0) THEN
      WRITE(lfnprt,'(A,I6,/)') &
        ' Minimum number of solutions contributed:  ',opt%minSol
      check = 1
    ENDIF
    IF (opt%badSol(1,1) > 0) THEN
      WRITE(lfnprt,'(A,F8.1,A)')   &
        ' Maximum residuals accepted in      north: ',opt%badSol(1,1),' mm'
      check = 1
    ENDIF
    IF (opt%badSol(1,2) > 0) THEN
      WRITE(lfnprt,'(A,F8.1,A)')   &
        ' Maximum residuals accepted in      east:  ',opt%badSol(1,2),' mm'
      check = 1
    ENDIF
    IF (opt%badSol(1,3) > 0) THEN
      WRITE(lfnprt,'(A,F8.1,A,/)') &
        ' Maximum residuals accepted in      up:    ',opt%badSol(1,3),' mm'
      check = 1
    ENDIF
    IF (opt%badSol(2,1) > 0) THEN
      WRITE(lfnprt,'(A,F8.1,A)')   &
        ' Maximum component rms accepted in  north: ',opt%badSol(2,1),' mm'
      check = 1
    ENDIF
    IF (opt%badSol(2,2) > 0) THEN
      WRITE(lfnprt,'(A,F8.1,A)')   &
        ' Maximum component rms accepted in  east:  ',opt%badSol(2,2),' mm'
      check = 1
    ENDIF
    IF (opt%badSol(2,3) > 0) THEN
      WRITE(lfnprt,'(A,F8.1,A)')   &
        ' Maximum component rms accepted in  up:    ',opt%badSol(2,3),' mm'
      check = 1
    ENDIF

    IF (check == 0)  WRITE(lfnprt,'(A)') ' No check required'
    WRITE(lfnprt,'(/)')

  ENDIF

  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE aoptgen



END MODULE
