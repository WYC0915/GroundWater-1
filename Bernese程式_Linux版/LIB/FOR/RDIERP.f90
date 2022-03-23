MODULE s_RDIERP
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdierp(maxpol, globalWindow, dtSim, &
                  polmod, polpar, hpsave, npol, tpol, sigpol, isgpol, isgnut)

! -------------------------------------------------------------------------
! Purpose:    Reads the Earth orientation input options for GPSEST
!
! Remark:     "polmod" is hardwired to "1" at the moment because other
!             options are not available at the moment
!
! Author:     R. Dach
!
! Created:    26-Jun-2001
! Last mod.:  21-May-2010
!
! Changes:    30-Jul-2001 RD: "Time window" is a special option
!             24-Aug-2001 RD: add 2sec at the end of the interval for ADDNEQ2
!             27-Mar-2003 RD: New parameter time window definition
!             01-Apr-2003 HU: Comment in DIMTST adapted
!             23-Jul-2009 DT: dtERP set to 0; else SINEX problems with epoch!
!             21-May-2010 MF: Nullify polWin
!
! SR used:    dimtst, exitrc, readKeys, ckopti, ckoptr, ckoptc, ckoptt,
!             parint, rdpwin, gtflna
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_time,   ONLY: t_timint
  USE s_exitrc
  USE s_dimtst
  USE s_ckoptr
  USE s_ckoptc
  USE s_ckoptt
  USE s_readkeys
  USE s_rdpwin
  USE s_ckopti
  USE s_gtflna
  USE s_parint
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                   :: maxpol       ! max. sets of earth
                                                 ! orientation parameters
  TYPE(t_timint)                 :: globalWindow ! window to be processed
                                                 ! (from - to, MJD)
  REAL(r8b)                      :: dtsim        ! max. interval to identify
                                                 ! epoch (in days)

! output:
  INTEGER(i4b)                   :: polmod       ! model of polar wobble
  INTEGER(i4b), DIMENSION(*)     :: polpar       ! parameters to be estimated
                                                 ! (1)=xp, (2)=yp, (3)=dt
                                                 ! (4)=eps,(5)=psi
                                                 ! 1 = estimated
                                                 ! 0 = not estimated
  REAL(r8b), DIMENSION(*)        :: hpsave       ! time resolution (hours)
                                                 ! (1): for Bernese pole format
                                                 ! (2): for IERS pole format
  INTEGER(i4b)                   :: npol         ! number of pole parameter sets
  REAL(r8b), DIMENSION(2,*)      :: tpol         ! time intervals of pole
                                                 ! parameter sets
  REAL(r8b), DIMENSION(5,*)      :: sigpol       ! a priori sigma of pole param.
                                                 ! 1-5 := xp,yp,dt,de,dp
                                                 !  *  := 1..maxpol
                                                 ! 1,2,4,5 given in mas,
                                                 ! 3 in msec
  INTEGER(i4b), DIMENSION(*)     :: isgpol       ! earth rot. parameter sigmas
                                                 ! 0 : apply for relevant
                                                 !     parameter only the
                                                 !     absolute constraints
                                                 !     given in input option
                                                 !     file
                                                 ! 1 : ensure continuity with
                                                 !     respect to previous
                                                 !     polynomial (in add.
                                                 !     to absolute constraints)
                                                 ! 4 : ensure continuity with
                                                 !     respect to previous
                                                 !     polynomial and constrain
                                                 !     drifts to zero
  INTEGER(i4b), DIMENSION(*)     :: isgnut       ! earth orientation parameter
                                                 ! sigmas
                                                 ! 0 : apply for relevant
                                                 !     parameter only the
                                                 !     absolute constraints
                                                 !     given in input option
                                                 !     file
                                                 ! 1 : ensure continuity with
                                                 !     respect to previous
                                                 !     polynomial (in add.
                                                 !     to absolute constraints)
                                                 ! 4 : ensure continuity with
                                                 !     respect to previous
                                                 !     polynomial and constrain
                                                 !     drifts to zero

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'rdierp'

! ERP-keywords
  CHARACTER(LEN=keyNameLength), DIMENSION(8), PARAMETER :: erpKeyw = &
           (/ 'SIGXP  ', 'SIGYP  ', 'SIGDT  ', 'SIGEPS ', 'SIGPSI ' ,&
                                    'SIG1DT ', 'SIG1EPS', 'SIG1PSI' /)

! Expands boundaries for EOP parameter sets to prevent rounding problems
! (was done in GPSEST_P):
  REAL(r8b), PARAMETER :: dtErp = 1d0 ! (sec)
!!!  REAL(r8b), PARAMETER :: dtErp = 0d0 ! (sec)


! Local Variables
! ---------------
  TYPE(t_timint),                  &
            DIMENSION(:), POINTER ::polWin

  CHARACTER(LEN=keyValueLength) , &
        DIMENSION(:)  , POINTER   :: keyValue
  CHARACTER(LEN=fileNameLength)   :: polFil

  INTEGER(i4b)                    :: iCont
  INTEGER(i4b)                    :: ii
  INTEGER(i4b)                    :: irCode
  INTEGER(i4b)                    :: irc

  REAL(r8b)                       :: t_0,dt_0
  REAL(r8b)                       :: erpTim
  REAL(r8b)                       :: rHlp


! Init some variables
! -------------------
  irCode = 0

  NULLIFY(keyValue)
  NULLIFY(polWin)

  polmod = 1

! Get the number of parameters for X-Pole
! ---------------------------------------
  CALL readKeys('NXP', keyValue, irc)

  CALL ckopti(1,'NXP', keyValue, srName,                    &
              'Number of parameters for X-pole',irc,irCode, &
              empty=0,ge=0,maxVal=1,result1=polpar(1))

! Get the number of parameters for Y-Pole
! ---------------------------------------
  CALL readKeys('NYP', keyValue, irc)

  CALL ckopti(1,'NYP', keyValue, srName,                    &
              'Number of parameters for Y-pole',irc,irCode, &
              empty=0,ge=0,maxVal=1,result1=polpar(2))

! Get the number of parameters for LOD
! ------------------------------------
  CALL readKeys('NDT', keyValue, irc)

  CALL ckopti(1,'NDT', keyValue, srName,                    &
              'Number of parameters for LOD',irc,irCode,    &
              empty=0,ge=0,maxVal=1,result1=polpar(3))

! Get the number of parameters for nutation (dEpsilon)
! ----------------------------------------------------
  CALL readKeys('NEPS', keyValue, irc)

  CALL ckopti(1,'NEPS', keyValue, srName,                    &
              'Number of parameters for epsilon',irc,irCode, &
              empty=0,ge=0,maxVal=1,result1=polpar(4))


! Get the number of parameters for nutation (dPsi)
! ------------------------------------------------
  CALL readKeys('NPSI', keyValue, irc)

  CALL ckopti(1,'NPSI', keyValue, srName,                    &
              'Number of parameters for psi',irc,irCode,     &
              empty=0,ge=0,maxVal=1,result1=polpar(5))


! Read the number of intervals
! ----------------------------
  CALL readKeys('NUMSET', keyValue, irc)

  CALL ckoptt(1,'NUMSET', keyValue, srName,                  &
              'Length of an EOP parameter set',irc,irCode,   &
              maxVal=1,gt=0d0,result1=erpTim)

! Get the parameter time window definition
! ----------------------------------------
  CALL rdpwin('PAR_OFF',(/' ',' '/),t_0,dt_0)

! Get the corresponding time windows
! ----------------------------------
  CALL parint(globalWindow,dtSim,t_0,dt_0,erpTim, &
              'Earth orientation parameters',nPol,polWin)

! Check the maximum dimension
! ---------------------------
  CALL dimtst(1,1,2,'rdierp','maxpol',                          &
              'Earth rotation parameter',                       &
              'Parameter is defined in module "P_GPSEST.f90".', &
              npol,maxpol,irc)

! Compute the time intervals
! --------------------------
  DO ii = 1, npol
    tpol(1:2,ii) = polWin(ii)%t(1:2)
  ENDDO

  DEALLOCATE(polWin,stat=irc)

! Read the apriori sigmas
! -----------------------
  DO ii = 1, SIZE(erpKeyw)

    IF (ii <= 5) THEN
      IF (polpar(ii) == 0) THEN
        sigpol(ii,1:nPol) = 0d0
        CYCLE
      ENDIF
    ELSE IF (polpar(ii-3) == 0) THEN
      CYCLE
    ENDIF

    CALL readKeys(erpKeyw(ii), keyValue, irc)

    CALL ckoptr(1,erpKeyw(ii), keyValue, srName,                       &
                'Sigmas of Earth orientation parameters',irc,irCode,   &
                empty=0d0,ge=0d0,maxVal=1,result1=rHlp)

    IF (ii <= 5) THEN

       sigpol(ii,1:npol) = rHlp

    ELSE

      IF (rHlp /= 0d0) THEN
        sigpol(ii-3,1) = rHlp
      ELSE
        WRITE(lfnerr,'(/,A,/,16X,A,/)')                                      &
        ' *** SR RDIERP: There must be a constraint for the first parameter',&
                        'Please check the entry for the keyword "'//         &
                        TRIM(erpKeyw(ii))//'"'
        CALL exitrc(2)
      ENDIF
    ENDIF
  ENDDO

! Number of parameters for the diff. file types
! ---------------------------------------------
  hpsave(1)=0d0
  CALL gtflna(0,'POLERS',polFil,irc)
  IF (LEN_TRIM(polFil) > 0 .AND. irc == 0) THEN
    CALL readKeys('NVBERN', keyValue, irc)

    CALL ckoptt(1,'NVBERN', keyValue, srName,                           &
                'Time resolution for Bernese pole files', irc, irCode,  &
                empty=0d0,ge=0d0,maxVal=1,result1=hpsave(1))
  ENDIF

  hpsave(2)=0d0
  CALL gtflna(0,'IERSPOL',polFil,irc)
  IF (LEN_TRIM(polFil) > 0 .AND. irc == 0) THEN
    CALL readKeys('NVIERS', keyValue, irc)

    CALL ckoptt(1,'NVIERS', keyValue, srName,                           &
                'Time resolution for IERS pole files', irc, irCode,     &
                empty=0d0,ge=0d0,maxVal=1,result1=hpsave(2))
  ENDIF


! Handle the continuity method
! ----------------------------
  CALL readKeys('CONTI', keyValue, irc)

  CALL ckoptc(1,'CONTI', keyValue, (/'NO  ','ERP ','NUT ','BOTH'/), &
              srName, 'Continuity between EOP sets',irc,irCode,     &
              maxVal=1,result1=iCont)

  ! "ERP"
  IF (iCont == 2) THEN
    isgpol(1:npol) = 1
    isgnut(1:npol) = 0

  ! "NUT"
  ELSE IF (iCont == 3) THEN
    isgpol(1:npol) = 0
    isgnut(1:npol) = 1

  ! "BOTH"
  ELSE IF (iCont == 4) THEN
    isgpol(1:npol) = 1
    isgnut(1:npol) = 1
  ENDIF


! Handle the contraints method
! ----------------------------
  CALL readKeys('CONSTRA', keyValue, irc)

  CALL ckoptc(1,'CONSTRA', keyValue, (/'NO  ','ERP ','NUT ','BOTH'/), &
              srName, 'Constrain drifts the of EOP sets',irc,irCode,  &
              maxVal=1,result1=iCont)

  IF (iCont == 2 .OR. iCont == 4) THEN
    isgpol(1:npol) = isgpol(1:npol) + 4
  ENDIF
  IF (iCont == 3 .OR. iCont == 4) THEN
    isgnut(1:npol) = isgnut(1:npol) + 4
  ENDIF

! Expand time window to prevent rounding problems (done in GPSEST_P before)
! -------------------------------------------------------------------------
  tpol(1,1)    = tpol(1,1)    - dtErp / 86400d0
  tpol(2,npol) = tpol(2,npol) + dtErp / 86400d0

  DEALLOCATE(keyValue, stat=irc)

! Stop if the input was wrong
! ---------------------------
  IF (irCode /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE rdierp

END MODULE
