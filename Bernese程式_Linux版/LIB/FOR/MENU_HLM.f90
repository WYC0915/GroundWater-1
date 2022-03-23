MODULE s_MENU_HLM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_hlm(keyWord, menuauxInp, output)

! -------------------------------------------------------------------------
! Purpose:    Generates a list of stations from two crd files
!             in HELMR1 (called by the menu program via MENUAUX)
!
! Author:     R. Dach
!
! Created:    04-Jul-2001
!
! Changes:    30-Aug-2001 RD: correct index for 2nd flag
!             30-Aug-2001 RD: use station selection file for default
!             17-Sep-2001 RD: getcoo variables allocatable
!             25-Sep-2001 RD: new station selection
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             29-Dec-2001 HU: Interface to alcerr added
!             19-Apr-2002 RD: Use keywords from MENUAUX.INP
!             16-May-2002 RD: Recover old values in the case of an error
!             13-Dec-2002 RD: New quotes-handling in SR writekey
!             23-Apr-2003 AJ: Nullify local pointers
!             16-May-2003 MM: Initialize and deallocate structure
!             17-Feb-2011 RD: Remove MAXSTA-COMMON (unused)
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, t_key, &
                      keyValueLength, fileNameLength, staNameLength
  USE d_staLst, ONLY: t_staList, init_staList
  USE p_menaux, ONLY: qt
  USE s_writekey
  USE s_alcerr
  USE s_readkeys
  USE s_ckoptc
  USE s_gtflna
  USE s_readstsg
  USE s_getco3
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)              :: keyWord     ! what to do
  CHARACTER(LEN=*)              :: menuauxInp  ! MENUAUX.INP file name

! output:
  TYPE(t_key)                   :: output      ! value: Result to display

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8),               PARAMETER :: srName  = 'menu_hlm'
  CHARACTER(LEN=1), DIMENSION(2), PARAMETER :: selFlag = (/ 'M' , 'E' /)

! Local Variables
! ---------------
  TYPE(t_staList)               :: staList

  CHARACTER(LEN=fileNameLength) :: filNam
  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:), POINTER     :: keyValue
  CHARACTER(LEN=staNameLength),  &
      DIMENSION(:), POINTER     :: stname  ! Station names from GETCOO
  CHARACTER(LEN=staNameLength),  &
      DIMENSION(:), POINTER     :: stname2 ! Station names from GETCOO
  CHARACTER(LEN=1),              &
      DIMENSION(:), POINTER     :: staflg  ! Station flags from GETCOO
  CHARACTER(LEN=1),              &
      DIMENSION(:), POINTER     :: staflg2 ! Station flags from GETCOO
  CHARACTER(LEN=1),              &
      DIMENSION(1)              :: flags

  INTEGER(i4b)                  :: maxStaLoc
  INTEGER(i4b)                  :: nstat         ! # stations from GETCOO
  INTEGER(i4b)                  :: nstat2        ! # stations from GETCOO
  INTEGER(i4b),                  &
      DIMENSION(:), ALLOCATABLE :: stanum,stanum2! Station numbers from GETCOO
  INTEGER(i4b)                  :: nflag
  INTEGER(i4b)                  :: iSta, jSta
  INTEGER(i4b)                  :: nSta
  INTEGER(i4b)                  :: ii, jj
  INTEGER(i4b)                  :: irc
  INTEGER(i4b)                  :: irCode = 0

  LOGICAL                       :: inlist


! Init pointers and variables
! ---------------------------
  NULLIFY(keyValue)
  NULLIFY(stname)
  NULLIFY(stname2)
  NULLIFY(staflg)
  NULLIFY(staflg2)
  CALL init_staList(staList)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'HELMERT') RETURN

! Have at least an empty uniline
! ------------------------------
  DEALLOCATE(output%value,stat=irc)

  ALLOCATE(output%value(1),stat=irc)
  CALL alcerr(irc,'output%value',(/1/),srName)
  output%value(1) = ' '

  WRITE(output%value(1),'(4(A,1X))') ( qt//qt,ii=1,4 )

  CALL writekey(menuauxInp,(/output/),1,irc)

! Read the coordinate files
! -------------------------
  nflag    = 1
  flags(1) = '#'  ! non-blank flags only

  CALL gtflna(1,'COORD1',filnam,irc)
  CALL getco3(filnam, nflag, flags, nstat,  stname , staflg = staflg)

  CALL gtflna(1,'COORD2',filnam,irc)
  CALL getco3(filnam, nflag, flags, nstat2, stname2, staflg = staflg2)

! Get the number of stations in the coordinate files
! --------------------------------------------------
  IF (nStat > nStat2) THEN
    maxStaLoc = nStat
  ELSE
    maxStaLoc = nStat2
  ENDIF
  IF (maxStaLoc == 0) maxStaLoc = 1

  ALLOCATE(staNum(maxStaLoc),stat=irc)
  CALL alcerr(irc,'staNum',(/maxStaLoc/),srName)

  ALLOCATE(staNum2(maxStaLoc),stat=irc)
  CALL alcerr(irc,'staNum2',(/maxStaLoc/),srName)

! Make an index for the second station
! ------------------------------------
  staNum2 = 0
  nSta    = 0
  DO iSta = 1,nStat
    DO jSta = 1,nStat2
      IF (stName(iSta) == stName2(jSta)) THEN
        nSta = nSta + 1
        staNum2(iSta) = jSta
        EXIT
      ENDIF
    ENDDO
  ENDDO


! Sort the stations
! -----------------
  staNum(1:nStat) = (/ (ii, ii=1,nStat) /)

  inList = .TRUE.
  DO WHILE (inList)
    inList = .FALSE.
    DO iSta = 1,nStat-1
      IF (stName(staNum(iSta)) > stName(staNum(iSta+1))) THEN
        ii             = staNum(iSta)
        staNum(iSta)   = staNum(iSta+1)
        staNum(iSta+1) = ii

        inList = .TRUE.
      ENDIF
    ENDDO
  ENDDO


! Get the list of files
! ---------------------
  jj = 0
  staList%nSta = 0
  CALL gtflna(0,'USESTA',filnam,irc)
  IF (irc == 0) THEN

    staList%nSta = 0
    CALL readstsg(filnam,0,staList)

    CALL readkeys('OTHSTA',keyValue,irc)

    CALL ckoptc(1,'OTHSTA',keyValue,(/'MARK   ','EXCLUDE'/),srName, &
                'Handling of stations not in file list',irc,irCode, &
                maxVal=1,error=2,result1=jj)
  ENDIF

! Generate the output list
! ------------------------
  DEALLOCATE(output%value,stat=irc)

  ALLOCATE(output%value(nSta),stat=irc)
  CALL alcerr(irc,'output%value',(/ nSta /),srName)

  output%value = ' '

  ii = 0
  DO ista = 1, nstat

    IF (staNum2(stanum(iSta)) == 0) CYCLE
    ii = ii+1

    inList = (staList%nSta == 0 .OR. jj == 0)

    DO jSta = 1, staList%nSta
      IF (staList%staNam(jSta) == stName(stanum(iSta))) THEN
        inList = .TRUE.
        EXIT
      ENDIF
    ENDDO

    IF (inList) THEN

      WRITE(output%value(ii),'(4(A,1X))') &
                qt // TRIM(stname(staNum(ista))) // qt, &
                qt // TRIM(staflg(staNum(ista))) // qt, &
                qt // TRIM(staflg2(staNum2(staNum(ista)))) // qt, &
                qt // qt

    ELSE

      WRITE(output%value(ii),'(4(A,1X))') &
                qt // TRIM(stname(staNum(ista))) // qt, &
                qt // TRIM(staflg(staNum(ista))) // qt, &
                qt // TRIM(staflg2(staNum2(staNum(ista)))) // qt, &
                qt // TRIM(selFlag(jj)) // qt

    ENDIF

  ENDDO

  DEALLOCATE(staflg,  stat=irc)
  DEALLOCATE(staflg2, stat=irc)
  DEALLOCATE(stanum,  stat=irc)
  DEALLOCATE(stanum2, stat=irc)
  DEALLOCATE(stname,  stat=irc)
  DEALLOCATE(stname2, stat=irc)

  DEALLOCATE(staList%staNam, stat=irc)
  DEALLOCATE(staList%sigma, stat=irc)

  DEALLOCATE(keyValue,stat=irc)
  RETURN
END SUBROUTINE menu_hlm

END MODULE
