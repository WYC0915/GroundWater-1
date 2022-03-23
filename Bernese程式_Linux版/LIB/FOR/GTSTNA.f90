MODULE s_GTSTNA
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE gtstna(iStop,iWild,tmjd,nStat,staold,stastr, &
                  keyWord,staCrux,stanew,renamList)

! -------------------------------------------------------------------------
! Purpose:    Translation of old station names into new ones
!             with a station name translation file
!             If the translation file does not exist the old
!             station names are returned
!
! Remark:     updated version of SR gtstna.f
!
! Changes in the old SR
!             24-Nov-1992 allow for wildcard * in station name
!             28-Dec-1992 use of sr "opnfil" to open files
!             10-Sug-1994 MR: call exitrc
!             20-Sep-1994 MR: add parameter "istop"
!             12-Jul-1996 TS: allow time indication in *.stn file
!             30-Jul-1996 TS: increased timint(*,2) to 1d20
!             06-Oct-1996 TS: increased maxstn
!             01-Nov-1996 TS: check if tmjd is 0.0
!             23-Jan-1997 TS: increased timint(*,2) to 2d20 for no-obs
!             12-Jul-1999 SS: check also "stastr"
!
! Author:     R.Dach
!
! Created:    22-oct-2001
!
! Changes:    22-oct-2001 RD: use station info file, switch to F90
!             21-May-2002 RD: Interface for SR readcrux
!             25-Sep-2002 HU: Remove i_astlib
!             09-Jul-2003 RD: No staInfo flag handling anymore
!             26-Oct-2010 SL: use m_bern with ONLY
!             24-Nov-2011 SL: new optional parameter renamList, use srName
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, fileNameLength, lfnErr
  USE d_stacrx, ONLY: t_stacrux
  USE s_dimtst
  USE s_inquire
  USE s_wildcd
  USE s_exitrc
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                    :: iStop   ! Stop flag
                                             ! 0: no stop if station not found
                                             !    in translation table
                                             ! 1: stop, if station not found
  INTEGER(i4b)                    :: iWild   ! Use wildcard or not (1/0)
  REAL(r8b)                       :: tmjd    ! Time of request
  INTEGER(i4b)                    :: nStat   ! Number of station names to be
                                             ! translated
  CHARACTER(LEN=*),DIMENSION(:)   :: staold  ! Old station names
  CHARACTER(LEN=*),DIMENSION(:)   :: stastr  ! Alternative 4-character
                                             ! station ids (' ': no check)
  CHARACTER(LEN=*)                :: keyWord ! file name or keyword
                                             ! of the station info file
                                             ! (Only for output)
! input/output:
  TYPE(t_stacrux)                 :: stacrux ! Complete translation table

! output:
  CHARACTER(LEN=*),DIMENSION(:)   :: stanew  ! New station names
  INTEGER(i4b), DIMENSION(:), &
    OPTIONAL                      :: renamList

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER    :: srName = 'GTSTNA'

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength)  :: filNam

  INTEGER(i4b)                   :: iStat
  INTEGER(i4b)                   :: iRenam
  INTEGER(i4b)                   :: iTest
  INTEGER(i4b)                   :: irc

  LOGICAL                        :: yes
  LOGICAL                        :: inFile

! Check dimensions
! ----------------
  CALL dimtst(1,2,2,srName,'nstat','number of stations',' ', &
              nstat,SIZE(staold),irc)

  CALL dimtst(1,2,2,srName,'nstat','number of stations',' ', &
              nstat,SIZE(stanew),irc)

  IF (SIZE(stastr) > 1 .AND. LEN_TRIM(stastr(1)) > 0)            &
    CALL dimtst(1,2,2,srName,'nstat','number of stations',' ', &
                nstat,SIZE(stastr),irc)

! No translation table came in -- read it
! ---------------------------------------
  CALL gtflna(0,keyWord,filnam,irc)

  IF (irc /= 0) THEN
    CALL inquire(FILE=keyWord, EXIST=yes)
    IF (yes) filnam=keyWord
  ENDIF

! No transformation table available
! ---------------------------------
  IF (LEN_TRIM(filnam) == 0) THEN

    staNew(1:nStat) = staOld(1:nStat)

  ELSE

! Find station names
! ------------------
    inFile = .TRUE.
    staLoop: DO iStat = 1,nStat
      DO iRenam = 1,staCrux%nRenam

! Check the old station name
! --------------------------
        IF (iWild == 1) THEN
          CALL wildcd(staCrux%renamSta(iRenam)%oldnam,staOld(iStat),iTest)
          IF (iTest /= 1) CYCLE
        ELSE
          IF (staCrux%renamSta(iRenam)%oldnam /= staOld(iStat)) CYCLE
        ENDIF

! Check observation interval
! --------------------------
        IF ( tmjd == 0.D0                                       .OR. &
            (tmjd >= staCrux%renamSta(iRenam)%timint%t(1) .AND.      &
             tmjd <= staCrux%renamSta(iRenam)%timint%t(2)) ) THEN

! Station name found
! ------------------
          stanew(iStat) = staCrux%renamSta(iRenam)%stanam
          IF(PRESENT(renamList)) renamList(iRenam) = renamList(iRenam)+1
          CYCLE staLoop
        ENDIF
      ENDDO ! Loop rename entries in stacrux

! Is there an alternative 4-character station id avail.?
! ------------------------------------------------------
      IF (SIZE(stastr) >= 1 .AND. LEN_TRIM(stastr(1)) > 0 .AND. &
          LEN_TRIM(staStr(iStat)) > 0) THEN

        WRITE(lfnerr,'(/,A,2(/,16X,2A),/)')                                &
             ' ### SR GTSTNA: Alternative 4-character station id checked', &
                             'Station id:        ',TRIM(stastr(iStat)),    &
                             'Station info file: ',TRIM(filnam)

        DO iRenam = 1,staCrux%nRenam

! Check the old station name
! --------------------------
          IF (iWild == 1) THEN
            CALL wildcd(staCrux%renamSta(iRenam)%oldnam,stastr(iStat),iTest)
            IF (iTest /= 1) CYCLE
          ELSE
            IF (staCrux%renamSta(iRenam)%oldnam /= stastr(iStat)) CYCLE
          ENDIF

! Check observation interval
! --------------------------
          IF ( tmjd == 0.D0                                       .OR. &
              (tmjd >= staCrux%renamSta(iRenam)%timint%t(1) .AND.      &
               tmjd <= staCrux%renamSta(iRenam)%timint%t(2)) ) THEN

! Station name found
! ------------------
            stanew(iStat) = staCrux%renamSta(iRenam)%stanam
            CYCLE staLoop
          ENDIF
        ENDDO ! Loop rename entries in stacrux
      ENDIF ! use alternative station ID

! Station name not found
! ----------------------
      IF (iStop == 1) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,2(/,16X,A,A),/)')                     &
              ' *** SR GTSTNA: Station name not found in station',       &
                              'information file (TYPE 001: renaming)',   &
                              'Station name:      ',TRIM(staold(iStat)), &
                              'Station info file: ',TRIM(filnam)
        inFile = .FALSE.
      ELSE
        stanew(iStat)=staold(iStat)
      ENDIF
    ENDDO staLoop

! All stations found ?
! --------------------
    IF (.NOT. inFile) CALL exitrc(2)

  ENDIF ! station renaming records are available

  RETURN

  END SUBROUTINE gtstna

END MODULE
