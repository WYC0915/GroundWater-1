MODULE s_QLRDOPT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE qlrdopt (window,sessid,yearid,title,devout,scrfil,satlst)

! -------------------------------------------------------------------------
! Purpose:    Reads input options for QLRINEXO and CRD2RNXO
!
! Author:     C. Urschl
!
! Created:    05-Nov-2003
!
! Changes:    09-Sep-2010 DT: Change keyword SATLST to lineedit
!             12-Jun-2012 DT: SPLSTR as module now;
!                             Use m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b,lfnerr, filePathLength, fileNameLength80, &
                      keyValueLength
  USE m_maxdim, ONLY: maxsat
  USE s_exitrc
  USE s_dimtst
  USE s_alcerr
  USE s_readkeys
  USE s_gtflna
  USE s_ckoptl
  USE s_gttimwin
  USE s_splstr

  IMPLICIT NONE

! List of Parameters
! ------------------
! output:
  REAL(r8b), DIMENSION(2)         :: window ! Time window
  CHARACTER(LEN=4)                :: sessid ! Session ID for RINEX file name
  CHARACTER(LEN=2)                :: yearid ! Year ID for RINEX file name
  CHARACTER(LEN=80)               :: title  ! Title line
  CHARACTER(LEN=filePathLength)   :: devout ! Path to RINEX files
  CHARACTER(LEN=fileNameLength80) :: scrfil ! Name of scratch file
  INTEGER(i4b), DIMENSION(:), POINTER :: satlst ! List of satellite numbers

! Local Parameters
! ----------------
  CHARACTER(LEN=7), PARAMETER     :: srname = 'QLRDOPT'

! Local Variables
! ---------------
  INTEGER(i4b)                    :: irc, irCode
  INTEGER(i4b)                    :: isat, nsat

  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=2)                   :: split
  CHARACTER(LEN=255)                 :: satStr
  CHARACTER(LEN=3),DIMENSION(maxSat) :: satNum

! Init variables
! --------------
  irCode=0

  NULLIFY(keyValue)

! Read general title
! ------------------
  CALL readKeys('TITLE', keyValue, irc)
  CALL ckoptl(0,'TITLE', keyValue, srName,          &
              'Program output title', irc, irCode,  &
              empty=" ",maxVal=1,result1=title)

! Read the time window
! --------------------
  CALL gttimwin(' ',(/'RADIO_0','RADIO_1','RADIO_2'/),              &
                    (/'SESSION_YEAR','SESSION_STRG'/),              &
                    (/'STADAT','STATIM','ENDDAT','ENDTIM'/),window)

! Read the session ID
! -------------------
  CALL readKeys('SESSID', keyValue, irc)
  CALL ckoptl(1,'SESSID', keyValue, srName,                      &
              'Session identifier for RINEX files', irc ,irCode, &
              empty=' ',maxLength=4,maxVal=1,result1=sessid)

! Read the year ID
! ----------------
  CALL readkeys('YEARID', keyValue, irc)
  CALL ckoptl(1,'YEARID', keyValue, srName,                   &
              'Year identifier for RINEX files', irc ,irCode, &
              empty=' ',maxLength=2,maxVal=1,result1=yearid)

! Read the path to the RINEX files
! --------------------------------
  CALL readkeys('RXOFILE_PTH_COL_1', keyValue, irc)
  CALL ckoptl(1,'RXOFILE_PTH_COL_1', keyValue, srName, &
              'Path to the RINEX files',irc,irCode,    &
              maxVal=1,result1=devout)

! Read name of scratch file
! -------------------------
  CALL gtflna(1,'AUXFIL',scrfil,irc)

! Read satellite numbers
! ----------------------
  CALL readKeys('SATLST', keyValue, irc)

  CALL ckoptl(1,'SATLST',keyValue,srName,           &
              'Satellite Number List',irc,irCode,   &
              maxVal=1,empty=' ',result1=satStr)

  split=' ,'
  CALL splstr(satStr,maxSat,split,nsat,satNum,irc)

! No satellites specified
  IF (nsat == 0 .OR. irc /= 0) THEN
    WRITE(lfnerr,'(A,A)')                                 &
      ' *** SR QLRDOPT: There are no satellite numbers ', &
      'specified in the input panel.'
    CALL exitrc(2)
  ENDIF

! Too many satellites specified
  CALL dimtst(1,1,2,srName,'maxsat','satellites',' ',nsat,maxsat,irc)

  ALLOCATE(satlst(nsat), stat=irc)
  CALL alcerr(irc,'satlst',(/nsat/),srName)

  DO isat = 1, nsat
!!!    READ(keyValue(isat),*) satlst(isat)
    READ(satNum(isat),*) satLst(isat)
  ENDDO

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

  RETURN

END SUBROUTINE qlrdopt

END MODULE
