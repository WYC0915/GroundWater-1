MODULE s_MENU_STA
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_sta(keyWord, output)

! -------------------------------------------------------------------------
! Purpose:    Create station list out of CRD-file
!
! Author:     H.Bock
!
! Created:    15-Aug-2001
!
! Changes:    17-Sep-2001 RD: getcoo variables allocatable
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             29-Dec-2001 HU: Interface to alcerr added
!             29-Apr-2002 RD: Use keywords from MENUAUX.INP
!             08-May-2002 RD: Use GETCO3 instead of GETCOO
!             23-Apr-2003 AJ: Nullify local pointers
!             20-Sep-2012 RD: Use M_BERN with ONLY
!             20-Sep-2012 RD: Correctly deallocate arrays
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, lfnloc, t_key, &
                      keyValueLength, staNameLength, lineLength

  USE s_alcerr
  USE s_getco3
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)                       :: keyWord         ! what to do

! output:
  TYPE(t_key)                            :: output   ! name = keyWord, if OK
                                                     ! value: Result to display

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER   :: srName = 'menu_sta'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength) :: crdFil
  CHARACTER(LEN=staNameLength),  &
    DIMENSION(:),   POINTER     :: stName
  CHARACTER(LEN=1), DIMENSION(1):: flags

  INTEGER(i4b)                  :: nStat     ! Number of stations in list
  INTEGER(i4b)                  :: nFlag
  INTEGER(i4b)                  :: iSta, jSta
  INTEGER(i4b)                  :: ii
  INTEGER(i4b)                  :: irc
  INTEGER(i4b),                  &
      DIMENSION(:),ALLOCATABLE  :: staIdx    ! Index for sorted stations

  LOGICAL                       :: sorted

  NULLIFY(stName)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'STALST') RETURN

  CALL gtflna(1,'COORD',crdFil,irc)
  IF (irc /= 0) RETURN

! Read the coordinate file
! ------------------------
  nStat = 0
  nFlag = 1
  flags = '@'
  CALL getco3(crdFil, nFlag, flags, nStat,  stName)

  IF (nStat == 0) RETURN

! Sort stations
! -------------
  ALLOCATE(staIdx(nStat),stat=irc)
  CALL alcerr(irc,'staIdx',(/nStat/),srName)

  staIdx = (/ (ii, ii=1,nStat) /)

  sorted = .TRUE.
  DO WHILE (sorted)
    sorted = .FALSE.
    DO iSta = 1,nStat-1
      IF (stName(staIdx(iSta)) > stName(staIdx(iSta+1))) THEN
        jSta           = staIdx(iSta)
        staIdx(iSta)   = staIdx(iSta+1)
        staIdx(iSta+1) = jSta

        sorted = .TRUE.
      ENDIF
    ENDDO
  ENDDO

! Put the results into the output-key
! -----------------------------------
  ALLOCATE(output%value(nStat),stat=irc)
  CALL alcerr(irc,'output%value',(/ nStat /),srName)
  output%value = ' '

  DO iSta = 1, nStat
    output%value(iSta) = stName(staIdx(iSta))
  END DO

  CLOSE(lfnloc)

  DEALLOCATE(stname, stat=irc)
  DEALLOCATE(staIdx, stat=irc)

  RETURN
END SUBROUTINE menu_sta

END MODULE
