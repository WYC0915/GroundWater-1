MODULE s_RDIION
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdiion(maxfil, nioreq, ionmod, ionreq)

! -------------------------------------------------------------------------
! Purpose:    Reads the local ionosphere input options for GPSEST
!
! Author:     R. Dach
!
! Created:    27-Jun-2001
! Last mod.:  23-Apr-2003
!
! Changes:    01-Apr-2003  HU: Comment in DIMTST adapted
!             23-Apr-2003  RD: Nullify local pointers
!
! SR used:    dimtst, exitrc, gtflna, readkeys
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_dimtst
  USE s_readkeys
  USE s_exitrc
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                 :: maxfil ! max. number of files

! output:
  INTEGER(i4b)                 :: nioreq ! # ionosphere requests
  INTEGER(i4b), DIMENSION(*)   :: ionmod ! number  of ionosphere model to be
                                         ! improved for request i
  INTEGER(i4b), DIMENSION(3,*) :: ionreq ! degree of development:
                                         ! 1: in latitude
                                         ! 2: deg. of development in hour angle
                                         ! 3: max. degree of mixed coefficients

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength) , &
         DIMENSION(:)  , POINTER  :: keyValue
  CHARACTER(LEN=keyValueLength) , &
         DIMENSION(4)             :: hlpStr
  CHARACTER(LEN=fileNameLength)   :: ionFile

  INTEGER(i4b)                    :: iReq
  INTEGER(i4b)                    :: ii
  INTEGER(i4b)                    :: irc, ios, iac


! Init variables
! --------------
  NULLIFY(keyValue)

! This options is only with an apriori model possible
! ---------------------------------------------------
  CALL gtflna(0,'IONOS', ionFile, irc)
  IF (irc /= 0 .OR. LEN_TRIM(ionFile) == 0) THEN
    WRITE(lfnerr,'(/,A,/,16X,A,/)')                                       &
    ' *** SR RDIION: Local ionosphere model estimation is only possible', &
                    'if an apriori ionosphere model is specified!'
    CALL exitrc(0)
  ENDIF

! Read the string with the model characterisation
! -----------------------------------------------
  CALL readkeys('LIONSTR',keyValue,irc)

  nioreq = SIZE(keyValue)
  CALL dimtst(1,1,2, 'rdiion', 'maxfil',                        &
              'local ionosphere model param.',                  &
              'Parameter is defined in module "P_GPSEST.f90".', &
              nioreq,maxfil,irc)

! Extract the specification from the string
! -----------------------------------------
  DO iReq = 1, SIZE(keyValue)
    READ(keyValue(iReq),*,iostat=ios) (hlpStr(ii),ii=1,4)

! Get the model number to be improved
! -----------------------------------
    READ(hlpStr(1),*,iostat=ios) ionmod(iReq)
    IF (ios /= 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,I6,/,16X,A,A,/)')        &
      ' *** SR RDIION: Wrong model specification for ' // &
                             'local ionosphere modelling',&
                      'Model number:    ',iReq,           &
                      'Specified Value: ',TRIM(hlpStr(1))
      CALL exitrc(2)
    ENDIF

! Get lat.-degree for model
! -------------------------
    READ(hlpStr(2),*,iostat=ios) ionreq(1,iReq)
    IF (ios /= 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,A,/,16X,A,I6,/,16X,A,A,/)')                   &
      ' *** SR RDIION: Wrong model specification for ' //                      &
                                                  'local ionosphere modelling',&
                      'Model parameter: ','Degree of development in latitude', &
                      'Model number:    ',iReq,                                &
                      'Specified Value: ',TRIM(hlpStr(2))
      CALL exitrc(2)
    ENDIF

! Get lon.-degree for model
! -------------------------
    READ(hlpStr(3),*,iostat=ios) ionreq(2,iReq)
    IF (ios /= 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,A,/,16X,A,I6,/,16X,A,A,/)')                   &
      ' *** SR RDIION: Wrong model specification for ' //                      &
                                                  'local ionosphere modelling',&
                      'Model parameter: ','Degree of development in longitude',&
                      'Model number:    ',iReq,                                &
                      'Specified Value: ',TRIM(hlpStr(3))
      CALL exitrc(2)
    ENDIF

! Get max. degree of mixed coeff. for model
! -----------------------------------------
    READ(hlpStr(4),*,iostat=ios) ionreq(3,iReq)
    IF (ios /= 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,A,/,16X,A,I6,/,16X,A,A,/)')                   &
      ' *** SR RDIION: Wrong model specification for ' //                      &
                                                  'local ionosphere modelling',&
                      'Model parameter: ','Max. degree of mixed coefficients', &
                      'Model number:    ',iReq,                                &
                      'Specified Value: ',TRIM(hlpStr(4))
      CALL exitrc(2)
    ENDIF

  ENDDO

! Deallocate local variables
! --------------------------
  DEALLOCATE(keyValue,stat=iac)

  RETURN
END SUBROUTINE rdiion

END MODULE
