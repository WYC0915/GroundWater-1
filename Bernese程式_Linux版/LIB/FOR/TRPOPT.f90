MODULE s_TRPOPT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE trpopt(obstyp, itropo, iextra, itrmap, itrgrd)

! -------------------------------------------------------------------------
! Purpose:    Reads input options for troposphere modelling
!
! Author:     R. Dach
!
! Created:    14-Oct-2010
! Last mod.:  26-Nov-2010
!
! Changes:    26-Nov-2010 DT: Separate keywords for microwave and optical
!                             models; obstyp added to call
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_trpest, ONLY: undef_Trp
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptc
  USE s_gtflna
  USE s_gettrp
  USE f_tstkey
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)     :: obstyp ! Observation type

! output:
  INTEGER(i4b)     :: itropo ! tropospheric model
  INTEGER(i4b)     :: iextra ! =0 : use measured values
                             ! =1 : use atm. model values
                             ! =2 : use est. bernese values
  INTEGER(i4b)     :: itrmap ! mapping function for troposp.est.
                             ! 1:   1/cos(z)
                             ! 2:   hopfield
                             ! 3,4: dry/wet niell
                             ! 5,6: dry/wet gmf
                             ! 7,8: dry/wet vmf
                             ! undef_Trp if no troposphere input file
  INTEGER(i4b),     &
    DIMENSION(*)   :: itrgrd ! i=1: est. of tropospheric gradients
                             !      0: no estimation
                             !      1: tilting
                             !      2: linear
                             ! i=2: ratio of number of zenith
                             !      to gradient parameters

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6),PARAMETER :: srName = 'trpopt'


! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
         DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=fileNameLength) :: trpFil
  CHARACTER(LEN=staNameLength)  :: staNam

  REAL(r8b), DIMENSION(3)       :: rDumm3

  INTEGER(i4b)                  :: irCode
  INTEGER(i4b)                  :: irc

! Init variables
! --------------
  irCode = 0

  NULLIFY(keyValue)

! Set "iExtra"
! ------------
  iExtra = 1

  IF (tstkey('METFIL')) THEN
    CALL readKeys('METFIL', keyValue, irc)
    IF (irc == 0 .AND. LEN_TRIM(keyValue(1)) > 0) iExtra = 0
  ENDIF

  CALL gtflna(0,'TROPEST', trpFil, irc)
  IF (irc == 0 .AND. LEN_TRIM(trpFil) > 0) THEN
    IF (iExtra /= 0) THEN
      iExtra = 2
    ELSE
      WRITE(lfnerr,'(/,A,1(/,16X,A),/)')                              &
        ' *** SR RDIGEN: Introduction of both external meteo data and', &
        'troposphere estimates not permitted'
      CALL exitrc(2)
    ENDIF
  ENDIF


! Take the troposphere models from input file
! -------------------------------------------
  IF (iextra == 2) THEN
    staNam = ' '
    CALL getTrp(trpFil,0d0,staNam,0,0, &
                iTropo,iTrMap,iTrGrd(1),rDumm3,irc)
    IF (iTropo > 100) iTropo =  iTropo - 100
    IF (iTropo <   0) iTropo = -iTropo


! Tropospheric Model from input panel
! -----------------------------------
  ELSE

   ! Models for Microwave observations
   ! ---------------------------------
    IF ( obstyp >= 1 ) THEN
      CALL readKeys('TROPOS', keyValue, irc)

      CALL ckoptc(1,'TROPOS', keyValue, &
                  (/ 'NONE         ','SAASTAMOINEN ','HOPFIELD     ',   &
                     'ESSEN-FROOME ','NIELL        ','GMF          ',   &
                     'VMF          ','DRY_SAAST    ','DRY_HOPFIELD ',   &
                     'DRY_NIELL    ','DRY_GMF      ','DRY_VMF      '/), &
                     srname, 'Troposphere model',irc, irCode,           &
                     valList=(/ 0, 1, 2, 3, 5, 6, 7,                    &
                                  11,12,15,16,17/),                     &
                     result1=itropo)

   ! Models for Optical observations
   ! -------------------------------
    ELSE
      CALL readKeys('TROPOS_S', keyValue, irc)

      CALL ckoptc(1,'TROPOS_S', keyValue, &
                  (/ 'MARINI-MURRAY','MENDES-PAVLIS'/),        &
                     srname, 'Troposphere model',irc, irCode,  &
                     valList=(/ 4, 8/),                        &
                     result1=itropo)

    ENDIF

    iTrMap = undef_Trp

  ENDIF

! Exit if an input failed
! -----------------------
  IF (irCode /= 0) CALL exitrc(2)

  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE trpopt

END MODULE

