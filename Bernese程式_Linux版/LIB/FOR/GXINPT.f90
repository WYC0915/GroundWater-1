MODULE s_GXINPT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE gxinpt(titgen,imean,ambLod,ambID)

! -------------------------------------------------------------------------
! Purpose:    Read input options for program GPSXTR
!
! Author:     U. Hugentobler
!
! Created:    09-Apr-2001
! Last mod.:  17-Feb-2011
!
! Changes:    21-Dec-2001 HU: Use d_const
!             23-Apr-2003 CU: Nullify local pointers
!             17-Feb-2011 MM: New ambiguity resolution options
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_const,  ONLY: date,time

  USE s_readkeys
  USE s_exitrc
  USE s_ckoptc
  USE s_ckoptl

  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=80)     :: titgen     ! OUT:  General title for summary file
  INTEGER(i4b)          :: imean      ! 0= all requests, 1= req. of middle day
  INTEGER(i4b)          :: ambLod     ! -1=ALL, 0=ALL+, 1=GPS, 2=GLO, 3=GAL
  CHARACTER(LEN=3)      :: ambID      ! #AR_ID

! Local Parameters
! ----------------
  INTEGER(i4b)                                              :: irc,iostat
  INTEGER(i4b)                                              :: irCode
  CHARACTER(LEN=64)                                         :: stitle
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER      :: keyValue

  NULLIFY(keyValue)
  irCode = 0

! READ TITLE
! ----------
  CALL readkeys('TITLE', keyValue, irc)
  stitle=keyValue(1)

  titgen=' '
  titgen(1:64)=stitle
  titgen(66:74)=date
  titgen(76:80)=time

! READ IMEAN
! ----------
  CALL readkeys('IMEAN', keyValue, irc)
  IF (keyValue(1) == 'ALL') THEN
    imean=0
  ELSE
    READ(keyValue(1),*,IOSTAT=iostat)imean
    IF(iostat /= 0)THEN
      WRITE(lfnerr,"(/,'*** SR GXINPT: invalid entry for IMEAN: ',A)") &
                      TRIM(keyValue(1))
      CALL exitrc(2)
    ENDIF
  ENDIF

! Ambiguity resolution options
! ----------------------------
  CALL readKeys('AMBLOD',keyValue,irc)
  CALL ckoptc(1,'AMBLOD',keyValue,                                       &
              (/'ALL     ','EACH&ALL','GPS     ','GLONASS ','GALILEO '/),&
              'SR GXINPT','Ambiguity resolution: GNSS',irc,irCode,       &
              valList=(/-1,0,1,2,3/),result1=ambLod)

  CALL readKeys('AMBID',keyValue,irc)
  CALL ckoptl(1,'AMBID',keyValue,'SR GXINPT','Ambiguity resolution: #AR_ID', &
              irc,irCode,maxLength=3,empty="   ",result1=ambID)
!
  DEALLOCATE(keyValue,stat=irc)
  RETURN

END SUBROUTINE gxinpt

END MODULE
