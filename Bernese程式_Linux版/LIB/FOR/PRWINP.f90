MODULE s_PRWINP
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE PRWINP(maxpar,IANZP,RMSP,minAcc,maxAcc,fromFile,flgDel,table)

! -------------------------------------------------------------------------
! Purpose:    This is a new version of the old subroutine PRWINP.f that
!             reads the input options of the program PREWEI
!
! Author:     R. Dach
!
! Created:    16-Nov-2000
!
! Changes:    18-Oct-2001 MM: implement a new options (computation of
!                             accuracy codes from RMS values)
!             25-Sep-2002 HU: Remove i_astlib
!             23-Apr-2003 AJ: Nullify local pointers
!             19-Jul-2010 SL: tab characters removed
!             14-Nov-2011 SL: m_bern w/ ONLY, no PRITIT call (moved to program)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,    ONLY: i4b, r8b, fileNameLength, keyValueLength, &
                       keyNameLength, lfnErr
  USE s_ckoptr
  USE s_prflna
  USE s_prfile
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckopti
  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b)                      :: maxpar   ! max. number of param.
  INTEGER(i4b)                      :: iAnzp    ! Number of param. in list
  REAL(r8b), DIMENSION(maxpar)      :: RMSP     ! RMS Parameter
  INTEGER(i4b)                      :: minAcc   ! minimum accuracy
  INTEGER(i4b)                      :: maxAcc   ! maximum accuracy
  INTEGER(i4b)                      :: flgDel   ! delete or flag satellites
  INTEGER(i4b)                      :: table    ! use table?
  LOGICAL                           :: fromFile ! consider codes from file

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength)                           :: srName
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER    :: keyValue
  CHARACTER(LEN=keyNameLength)                            :: HLPSTR
  INTEGER(i4b)                                            :: irc, ircSum

  NULLIFY(keyValue)

! Initialize some variables
! -------------------------
  srName   = 'SR prwinp (PG prewei)'
  minAcc   = -1
  maxAcc   = 99
  fromFile = .FALSE.
  flgDel   = 1
  ircSum   = 0

! Use table or compute values?
! ----------------------------
  CALL ckoptb(1,(/'RADIO1','RADIO2'/),srName,                             &
              'Accuracy computation method',ircSum,result1=table)

! Read options
! ------------
  CALL readKeys('MINACC',keyValue,irc)
  CALL ckopti(1,'MINACC',keyValue,srName,'Minimum accuracy code',         &
              irc,ircSum,maxVal=1,ge=0,result1=minAcc)

  CALL ckoptb(1,(/'FROMFILE'/),srName,                                    &
              'Use codes from file',ircSum,resultL=fromFile)

  CALL readKeys('MAXACC',keyValue,irc)
  CALL ckopti(1,'MAXACC',keyValue,srName,'Maximum accuracy code',         &
              irc,ircSum,maxVal=1,ge=0,result1=maxAcc)

  CALL readKeys('FLGDEL',keyValue,irc)
  CALL ckoptc(1,'FLGDEL',keyValue,(/'REMOVED','FLAGGED','RERATED'/),      &
              srName,'Delete or flag satellites',irc,ircSum,              &
              maxVal=1,result1=flgDel)

! On error exit
! -------------
  IF (ircSum /= 0) CALL exitrc(2)

! Using the tabular values
! ------------------------
  IF (table == 1) THEN

! Loop all valid RMS parameter
! ----------------------------
    iAnzp = 0
    DO WHILE (iAnzp <= maxpar .AND. iAnzp < 9)
      iAnzp=iAnzp+1

! Read the entry for the next parameter
! -------------------------------------
      WRITE(HLPSTR,'(A6,I2.2)') 'RECORD',iAnzp
      CALL readkeys(HLPSTR,keyValue,irc)
      CALL ckoptr(1,HLPSTR,keyValue,srName,'Accuracy code from table',    &
                  irc,ircSum,maxVal=1,ge=0d0,empty=-1d0,result1=RMSP(iAnzp))
      IF (ircSum /= 0) CALL exitrc(2)
      IF (RMSP(iAnzp) == -1) THEN
        iAnzp = iAnzp-1
        EXIT
      ENDIF

! Do not exceed the array of RMSP
! -------------------------------
      IF (iAnzp > maxpar) THEN
         WRITE(LFNERR,'(/,A,/,16X,A,/,16X,A,I3,/)')                  &
           ' *** PG PRWINP: NUMBER OF WEIGHT CLASSES TOO LARGE',     &
                           'INCREASE "MAXPAR" IN THE PGM PREWEI',    &
                           'MAXIMUM NUMBER ALLOWED: ', MAXPAR
         CALL exitrc(2)
      ENDIF
    ENDDO
  ENDIF

! Print files
! -----------
  CALL prflna

  CALL readKeys('PREOUT',keyValue,irc)
  IF (TRIM(keyValue(1)) == "")                                        &
     CALL prfile ('PREFIL','Precise orbit files',2,80)

  DEALLOCATE(keyValue,stat=irc)

  RETURN

  END SUBROUTINE prwinp

END MODULE
