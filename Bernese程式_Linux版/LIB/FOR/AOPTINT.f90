MODULE s_AOPTINT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE aoptint(opt, limits)

! -------------------------------------------------------------------------
! Purpose:    Reads input options for ADDNEQ2:
!             number of intervals for each parameter
!
! Remark:     It is assumed that this subroutine is the FIRST one
!             reading values for "opt%req"
!
! Author:     M. Meindl
!
! Created:    10-Jul-2001
!
! Changes:    11-Dec-2001 HU: Format statement corrected
!             20-Dec-2001 RD: Separate requests for vert. trp and trp grad.
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             08-Apr-2002 RD: Use SRs ckopt to read options
!             10-Dec-2002 CU: Lowercase for title line
!             28-Mar-2003 RD: New parameter time window definition
!             23-Apr-2003 CU: Nullify local pointers
!             27-May-2003 CU: Print reference epoch
!             11-Dec-2003 MM: Xp, Yp, UT combined; dEps, dPsi combined
!             07-Feb-2005 HB: Adopt for ifc-Compiler, Version 8.1
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             22-Sep-2005 RD: Use new module D_PAR.f90
!             02-Apr-2009 RD: Reset ESTVEL for NEQs without CRD
!             20-Aug-2009 LO: Cosmetics adapted for long time series
!             21-Nov-2009 RD: Intersystem biases added
!             15-Jun-2012 MM: Keyword NUM_INT removed
!
! SR used:    exitrc, alcerr, readkeys, ckoptr, ckoptb, parInt, rdpwin
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_time,   ONLY: t_timint
  USE d_par,    ONLY: maxLcq, maxParTyp
  USE p_addneq, ONLY: t_opt
  USE s_ckoptt
  USE s_alcerr
  USE s_parint
  USE s_rdpwin
  USE s_timst2
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  TYPE(t_timint), DIMENSION(maxParTyp)     ::limits    ! time intervals

! output:
  TYPE(t_opt)                              :: opt      ! Options for ADDNEQ2

! List of functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER                           :: srName = 'aoptint'

  INTEGER(i4b),     PARAMETER                           :: maxReqTyp = 10

  CHARACTER(LEN=9), DIMENSION(maxReqTyp),   PARAMETER   :: keyWord =  &
  (/  'CRD_NINT ','ISB_NINT ','TRP_NINT ','GRD_NINT ','GRD_NINT ',    &
      'ERP_NINT ','ERP_NINT ','ERP_NINT ','NUT_NINT ','NUT_NINT ' /)

  INTEGER(i4b), DIMENSION(maxReqTyp,maxLcq),PARAMETER   :: locq =     &
  reshape ( source =                                                  &
  (/  1, 2, 6, 6, 6,10,10,10,10,10,                                      &
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,                                      &
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,                                      &
      0, 0, 3, 1, 2, 1, 2, 3, 4, 5,                                      &
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,                                      &
      0, 5, 0, 0, 0, 0, 0, 0, 0, 0,                                      &
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0    /),                                &
  shape = (/ maxReqTyp,maxLcq /) )

  CHARACTER(LEN=22),DIMENSION(maxReqTyp),   PARAMETER   :: parTyp =            &
  (/  'Station coordinates  ','Inter-system bias    ',                         &
      'Vertical troposphere ','Tropos. gradients (n)','Tropos. gradients (e)', &
      'X-Pole               ','Y-Pole               ','Length of day        ', &
      'Nutation epsilon     ','Nutation psi         '                         /)

  ! Rounding limit (dtSim/2 is used in parInt; it must compensate
  ! the 3 sec for troposphere parameters limit extension from GPSEST)
  REAL(r8b), PARAMETER :: dtSim = 10d0/86400d0
  REAL(r8b) :: tim1,tim2

! Local variables
! ---------------
  TYPE(t_timint), DIMENSION(:),POINTER                  :: parWin

  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=shortLineLength)                        :: remStr
  CHARACTER(LEN=timStrgLength2)                         :: allInt
  CHARACTER(LEN=timStrgLength)                          :: oneInt
  CHARACTER(LEN=40)                                     :: timStr

  INTEGER(i4b)                                          :: estVel
  INTEGER(i4b)                                          :: iReq, nReq
  INTEGER(i4b)                                          :: iPar, nPar
  INTEGER(i4b)                                          :: irCode
  INTEGER(i4b)                                          :: irc, iac

  REAL(r8b),     DIMENSION(maxReqTyp)                   :: parLen
  REAL(r8b)                                             :: velLen
  REAL(r8b)                                             :: t_0,dt_0
  REAL(r8b), DIMENSION(2)                               :: timMjd


! Init variables
! --------------
  irCode = 0

  NULLIFY(keyValue)
  NULLIFY(parWin)


! Parameter requests and intervals
! --------------------------------

! Loop over all requests
  DO iReq = 1, maxReqTyp
    parLen(iReq) = 0d0

! Coordinate/velocity
    IF (locq(iReq,1) == 1) THEN

      CALL ckoptb(1, (/ keyWord(iReq) /), srName,                       &
              'Length of intervals for ' // TRIM(parTyp(iReq)), irCode, &
              result1=estVel)
      IF (limits(locq(iReq,1))%t(2) == 0.d0) THEN
        parLen(iReq) = 0d0
        estVel = 0
      ENDIF

! All other parameter types
    ELSE
      CALL readKeys(keyWord(iReq), keyValue, irc)

      CALL ckoptt(1, keyWord(iReq), keyValue, srName, &
                  'Length of intervals for ' // TRIM(parTyp(iReq)), &
                  irc, irCode, &
                  maxVal=1,gt=0d0,error=0d0,empty=0d0,result1=parLen(iReq))
    ENDIF

  END DO

! Stop if there are wrong input options
! -------------------------------------
  IF (irCode /= 0) CALL exitrc(2)

! Get the parameter time window definition
! ----------------------------------------
  CALL rdpwin(' ',(/'PAR_REF','PAR_OFF'/),t_0,dt_0)

! Calculate number of requests
! ----------------------------
  nReq = estVel
  DO iReq = 1, maxReqTyp
    IF (parLen(iReq) > 0d0 .AND. limits(locq(iReq,1))%t(2) > 0.d0) THEN

      CALL parint(limits(locq(iReq,1)),dtSim,t_0,dt_0,parLen(iReq), &
                  parTyp(iReq),nPar,parWin)
      DEALLOCATE(parWin,stat=irc)

      nReq = nReq + nPar
    ENDIF
  END DO

! Put a header section into the program output
! --------------------------------------------
  IF (nReq > 0) THEN

    WRITE(lfnprt,'(A,/,A)')                                              &
    ' Change number of intervals:',' --------------------------'

    IF (t_0 /= 1d20) THEN
      timMjd(1) = t_0 + dt_0
      timMjd(2) = 0
      CALL timst2(1, 1, timMjd, timStr)
      WRITE(lfnprt,'(A,A40)') ' Reference epoch: ',timStr
    ENDIF

    WRITE(lfnprt,'(2(/,A))')                                             &
    ' Parameter type           Total interval                       ' // &
    '     # int  New length     Remark',                                 &
    ' --------------------------------------------------------------' // &
    '---------------------------------------------------------------------'

  ENDIF

! Calculate time intervals and fill opt%req
! -----------------------------------------
  ALLOCATE(opt%req(nReq), stat=iac)
  CALL alcerr(iac, 'opt%req', (/nReq/), srName)

  nReq = 0
  DO iReq = 1, maxReqTyp

    IF (((estVel == 1 .AND. locq(iReq,1) == 1) .OR. parLen(iReq) > 0d0) .AND. &
        limits(locq(iReq,1))%t(2) > 0.d0) THEN

      ! Coordinates
      IF (locq(iReq,1) == 1) THEN
        nReq = nReq + 1

        opt%req(nReq)%name = ' '
        opt%req(nReq)%locq(1:maxLcq) = locq(iReq,1:maxLcq)
        opt%req(nReq)%timint = limits(locq(iReq,1))

        ! Get time strings
        nPar = 1
        velLen = limits(1)%t(2)-limits(1)%t(1)
        CALL timst2(1,2,limits(1)%t,allInt)
        CALL timst2(1,1,velLen, oneInt)
        WRITE(oneInt(4:10),'(I5,A)') INT(velLen+dtSim),' d'

      ! Other parameter
      ELSE
        CALL parint(limits(locq(iReq,1)),dtSim,t_0,dt_0,parLen(iReq), &
                    parTyp(iReq),nPar,parWin)

        DO iPar = 1,nPar
          nReq = nReq + 1

          opt%req(nReq)%name = ' '
          opt%req(nReq)%locq(1:maxLcq) = locq(iReq,1:maxLcq)
          opt%req(nReq)%timint = parWin(iPar)
        ENDDO

        ! Get time strings
!!!! Problem fuer ifc81 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CMP_IFC8
        tim1=parWin(1)%t(1)
        tim2=parWin(nPar)%t(2)
        CALL timst2(1,2,(/ tim1,tim2 /),allInt)
#else
        CALL timst2(1,2,(/ parWin(1)%t(1), parWin(nPar)%t(2) /),allInt)
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        CALL timst2(1,1,parLen(iReq)/24d0, oneInt)
        WRITE(oneInt(4:10),'(I5,A)') INT(parLen(iReq)/24d0+dtSim),' d'

        DEALLOCATE(parWin,stat=iac)
      ENDIF

! Write the program output
! ------------------------
      ! Generate the remark
      remStr = ' '

      IF (locq(iReq,1) == 1) WRITE(remStr,'(A)')       &
              'velocity setup: coordinates for 2 epochs'

      IF (locq(iReq,1) == 10) WRITE(remStr,'(A,I5,A)') &
              'continuous modelling with',nPar+1,' parameters'

      ! Write the line to the program output
      WRITE(lfnprt,'(1X,A,3X,A,I5,A,3X,A)')             &
            parTyp(iReq),allInt,nPar,oneInt(4:19),TRIM(remStr)

    ENDIF
  END DO

! Close the table
! ---------------
  IF (nReq > 0) WRITE(lfnprt,'(/)')

  DEALLOCATE(keyValue,stat=iac)
  RETURN
END SUBROUTINE aoptint

END MODULE
