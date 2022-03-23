MODULE s_RDIMAX
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdimax(imxloc,imxfil,imxsta,imxsat,imxamb, &
                  imxpar,imxfls,imxsas,imxamp,imxsng)

! -------------------------------------------------------------------------
! Purpose:    Reads user input for array dimensions for GPSEST
!
! Author:     R. Dach
!
! Created:    09-Nov-2004
! Last mod.:  09-Nov-2004
!
! Changes:    __-___-____ __:
!
! SR used:    exitrc, readkeys, ckoptb, ckopti
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckopti
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:

! output:
  INTEGER(i4b)     :: imxloc ! User dimension for MAXLOC
  INTEGER(i4b)     :: imxfil ! User dimension for MAXFIL
  INTEGER(i4b)     :: imxsta ! User dimension for MAXSTA
  INTEGER(i4b)     :: imxsat ! User dimension for MAXSAT
  INTEGER(i4b)     :: imxamb ! User dimension for MAXAMB
  INTEGER(i4b)     :: imxpar ! User dimension for MAXPAR
  INTEGER(i4b)     :: imxfls ! User dimension for MAXFLS
  INTEGER(i4b)     :: imxsas ! User dimension for MAXSAS
  INTEGER(i4b)     :: imxamp ! User dimension for MAXAMP
  INTEGER(i4b)     :: imxsng ! User dimension for MAXSNG

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6),PARAMETER :: srName = 'rdimax'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
         DIMENSION(:), POINTER  :: keyValue

  INTEGER(i4b)                  :: irCode
  INTEGER(i4b)                  :: irc

  LOGICAL                       :: doMax


! Init variables
! --------------
  irCode = 0

  NULLIFY(keyValue)

  imxloc = 0
  imxfil = 0
  imxsta = 0
  imxsat = 0
  imxamb = 0
  imxpar = 0
  imxfls = 0
  imxsas = 0
  imxamp = 0
  imxsng = 0

! Consider user inputs?
! ---------------------
  CALL ckoptb(1,(/'SHOWMAX'/),srName,                                      &
              'Adjust memory model',irCode,resultL=doMax)

! MAXLOC:
! ------
  IF (doMax) THEN

    CALL readKeys('MAXLOC', keyValue, irc)

    CALL ckopti(1,'MAXLOC',keyValue, srName,                               &
                'MAXLOC: Max.num.of parameters to be processed',           &
                irc,irCode,maxVal=1,ge=0,empty=0,result1=imxloc)
  ENDIF

! MAXFIL:
! ------
  IF (doMax) THEN

    CALL readKeys('MAXFIL', keyValue, irc)

    CALL ckopti(1,'MAXFIL',keyValue, srName,                               &
                'MAXFIL: Max. number of files to be processed',            &
                irc,irCode,maxVal=1,ge=0,empty=0,result1=imxfil)
  ENDIF

! MAXSTA:
! ------
  IF (doMax) THEN

    CALL readKeys('MAXSTA', keyValue, irc)

    CALL ckopti(1,'MAXSTA',keyValue, srName,                               &
                'MAXSTA: Max. number of stations involved',                &
                irc,irCode,maxVal=1,ge=0,empty=0,result1=imxsta)
  ENDIF

! MAXSAT:
! ------
  IF (doMax) THEN

    CALL readKeys('MAXSAT', keyValue, irc)

    CALL ckopti(1,'MAXSAT',keyValue, srName,                               &
                'MAXSAT: Max.number of satellites involved',               &
                irc,irCode,maxVal=1,ge=0,empty=0,result1=imxsat)
  ENDIF

! MAXAMB:
! ------
  IF (doMax) THEN

    CALL readKeys('MAXAMB', keyValue, irc)

    CALL ckopti(1,'MAXAMB',keyValue, srName,                               &
                'MAXAMB: Max.number of ambiguities per file',              &
                irc,irCode,maxVal=1,ge=0,empty=0,result1=imxamb)
  ENDIF

! MAXPAR:
! ------
  IF (doMax) THEN

    CALL readKeys('MAXPAR', keyValue, irc)

    CALL ckopti(1,'MAXPAR',keyValue, srName,                               &
                'MAXPAR: Max.num. of param. simult. processed',        &
                irc,irCode,maxVal=1,ge=0,empty=0,result1=imxpar)
  ENDIF

! MAXFLS:
! ------
  IF (doMax) THEN

    CALL readKeys('MAXFLS', keyValue, irc)

    CALL ckopti(1,'MAXFLS',keyValue, srName,                               &
                'MAXFLS: Max.number of files simult. processed',           &
                irc,irCode,maxVal=1,ge=0,empty=0,result1=imxfls)
  ENDIF

! MAXSAS:
! ------
  IF (doMax) THEN

    CALL readKeys('MAXSAS', keyValue, irc)

    CALL ckopti(1,'MAXSAS',keyValue, srName,                               &
                'MAXSAS: Max.num. of satel. simult. processed',            &
                irc,irCode,maxVal=1,ge=0,empty=0,result1=imxsas)
  ENDIF

! MAXAMP:
! ------
  IF (doMax) THEN

    CALL readKeys('MAXAMP', keyValue, irc)

    CALL ckopti(1,'MAXAMP',keyValue, srName,                               &
                'MAXAMP: Max.num. of ambigu. simult. processed',           &
                irc,irCode,maxVal=1,ge=0,empty=0,result1=imxamp)
  ENDIF

! MAXSNG:
! ------
  IF (doMax) THEN

    CALL readKeys('MAXSNG', keyValue, irc)

    CALL ckopti(1,'MAXSNG',keyValue, srName,                               &
                'MAXSNG: Max.num. of param. contrib. an obser.',           &
                irc,irCode,maxVal=1,ge=0,empty=0,result1=imxsng)
  ENDIF

! Exit if an input failed
! -----------------------
  IF (irCode /= 0) CALL exitrc(2)

  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE rdimax

END MODULE
