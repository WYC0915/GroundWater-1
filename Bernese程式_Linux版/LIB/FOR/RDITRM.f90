MODULE s_RDITRM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rditrm(maxtrp, maxtrm, globalWindow, dtSim,  &
                  ntrreq, sigtrp, trplim, npartr)


! -------------------------------------------------------------------------
! Purpose:    Reads the local troposphere input options for GPSEST
!
! Author:     R. Dach
!
! Created:    26-Jun-2001
! Last mod.:  21-May-2010
!
! Changes:    30-Jul-2001  RD: "Time window" is a special option
!             27-Mar-2003  RD: New parameter time window definition
!             01-Apr-2003  HU: Comment in DIMTST adapted
!             21-May-2010  MF: Nullify trmWin
!
! SR used:    readkeys, ckoptr, ckopti, ckoptt, exitrc, rdpwin, parint, dimtst
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_time,   ONLY: t_timint
  USE s_dimtst
  USE s_ckoptr
  USE s_ckoptt
  USE s_parint
  USE s_rdpwin
  USE s_readkeys
  USE s_exitrc
  USE s_ckopti
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)               :: maxtrm       ! max. number of troposphere models
  INTEGER(i4b)               :: maxtrp       ! max. number of trop. parameters
                                             ! per model
  TYPE(t_timint)             :: globalWindow ! window to be processed
                                             ! (from - to, MJD)
  REAL(r8b)                  :: dtsim        ! max. interval to identify
                                             ! epoch (in days)

! output:
  INTEGER(i4b)               :: ntrreq       ! number of local tropos.
                                             ! model requests
  INTEGER(i4b), DIMENSION(*) :: npartr       ! number of parameters in request i
  REAL(r8b), DIMENSION(2,*)  :: trplim       ! time interval (mjd) for request i
  REAL(r8b), DIMENSION(*)    :: sigtrp       ! a priori sigmas for troposphere
                                             ! parameters in m, m/(100m),
                                             ! m/(100m)**2, ...

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'rditrm'

! Expands boundaries for local troposphere models to prevent rounding problems
! (was done in GPSEST_P):
  REAL(r8b), PARAMETER :: dtTrm = 1d0   ! (sec)


! Local Variables
! ---------------
  TYPE(t_timint),                  &
            DIMENSION(:), POINTER ::trmWin

  CHARACTER(LEN=keyValueLength) , &
         DIMENSION(:)  , POINTER  :: keyValue

  INTEGER(i4b)                    :: iParam
  INTEGER(i4b)                    :: ii
  INTEGER(i4b)                    :: irCode
  INTEGER(i4b)                    :: irc

  REAL(r8b)                       :: t_0,dt_0
  REAL(r8b)                       :: trmTim
  REAL(r8b)                       :: rHlp

! Init some variables
! -------------------
  irCode = 0

  NULLIFY(keyValue)
  NULLIFY(trmWin)

! Read the number of models
! -------------------------
  CALL readkeys('LTRPMOD', keyValue, irc)

  CALL ckoptt(1,'LTRPMOD', keyValue, srName,                  &
              'Length of a local tropos. model',irc,irCode,   &
              maxVal=1,gt=0d0,result1=trmTim)

! Get the parameter time window definition
! ----------------------------------------
  CALL rdpwin('PAR_OFF',(/' ',' '/),t_0,dt_0)

! Get the corresponding time windows
! ----------------------------------
  CALL parint(globalWindow,dtSim,t_0,dt_0,trmTim, &
              'Local troposphere models',ntrreq,trmWin)

  DO ii = 1, ntrreq
    trplim(1:2,ii) = trmWin(ii)%t
  ENDDO

  DEALLOCATE(trmWin,stat=irc)

! Check the maximum number
! ------------------------
  CALL dimtst(1,1,2,'rditrm','maxtrm',                          &
              'local troposphere model',                        &
              'Parameter is defined in module "P_GPSEST.f90".', &
              ntrreq, maxtrm, irc)

! Read the number of parameters per model
! ---------------------------------------
  CALL readkeys('LTRPPAR', keyValue, irc)

  CALL ckopti(1,'LTRPPAR', keyValue, srName,               &
              'Number of parameters per model',irc,irCode, &
              maxVal=1, ge=0, result1=iParam)

  npartr(1:ntrreq) = iParam + 1

! Check the maximum number
! ------------------------
  CALL dimtst(1,1,2,'rditrm','maxtrp',                          &
              'parameters for the local troposphere',           &
              'Parameter is defined in module "P_GPSEST.f90".', &
              iParam+1, maxtrp, irc)

! Read the relative apriori sigma
! -------------------------------
  IF (iParam > 0) THEN
    CALL readkeys('LTRPSIG2', keyValue, irc)

    CALL ckoptr(1,'LTRPSIG2', keyValue, srName,                      &
                'Sigma of higher order for local tropo.',irc,irCode, &
                maxVal=1, ge=0d0, empty=0d0, result1=rHlp)

    sigtrp(2:iParam+1) = rHlp
  ENDIF

! Read the absolute apriori sigma
! -------------------------------
  CALL readkeys('LTRPSIG1', keyValue, irc)

  CALL ckoptr(1,'LTRPSIG1', keyValue, srName,                      &
              'Sigma of abs. term for local tropo.',irc,irCode,    &
              maxVal=1, ge=0d0, empty=0d0, result1=sigtrp(1))


! Expand time window to prevent rounding problems
! -----------------------------------------------
  trplim(1,1)      = trplim(1,1)      - dtTrm/86400d0
  trplim(2,ntrreq) = trplim(2,ntrreq) + dtTrm/86400d0

  DEALLOCATE(keyValue,stat=irc)

  IF (irCode /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE rditrm

END MODULE
