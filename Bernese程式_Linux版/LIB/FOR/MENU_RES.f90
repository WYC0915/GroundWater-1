MODULE s_MENU_RES
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_res(keyWord, output)

! -------------------------------------------------------------------------
! Purpose:    Generates a list of all baselines from the res. files
!             in REDISP (called by the menu program via MENUAUX)
!
! Author:     R. Dach
!
! Created:    04-Jul-2001
! Last mod.:  08-Aug-2005
!
! Changes:    08-Oct-2001  RD: Modified output
!             29-Apr-2002  RD: Use keywords from MENUAUX.INP
!             30-Jul-2002  HU: Use interface for alcerr
!             29-Aug-2002  RD: Handle new formatted residual files
!             16-May-2003  MM: Initialize and deallocate structure
!             08-Aug-2005  HB: Use new SR TIMST2 (module)
!
! SR called:  gtflna, opnfil, opnerr, rdresh, timst2, init_resHead
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_resfil, ONLY: t_resHead, init_resHead
  USE p_redisp, ONLY: freqID,filTyp,difTyp
  USE s_opnfil
  USE s_alcerr
  USE s_rdresh2
  USE s_opnerr
  USE s_timst2
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
  CHARACTER(LEN=8), PARAMETER    :: srName = 'menu_res'

! Local Variables
! ---------------
  TYPE(t_resHead)                :: resHed

  CHARACTER(LEN=fileNameLength)  :: filnam
  CHARACTER(LEN=19)              :: epostr

  INTEGER(i4b)                   :: mDiff
  INTEGER(i4b)                   :: ii, jj
  INTEGER(i4b)                   :: ios,irc


! Incorrect keyword
! -----------------
  IF (keyWord /= 'RESID') RETURN

  CALL gtflna(1,'RESIDUA',filnam,irc)

  CALL opnfil(lfnres, filnam, 'OLD', 'UNFORMATTED', 'READONLY', ' ', ios)
  CALL opnerr(lfnerr, lfnres, ios, filnam, srName)

  CALL init_resHead(resHed)
  CALL rdresh2(lfnres, resHed)

  CLOSE(lfnres)

  IF (resHed%nFil == 0) RETURN

! Put the results into the output-key
! -----------------------------------
  ALLOCATE(output%value(resHed%nFil),stat=irc)
  CALL alcerr(irc,'output%value',(/ resHed%nFil /),srName)
  output%value = ' '

  DO ii = 1, resHed%nfil

    CALL timst2(1,1,resHed%filHead(ii)%timref,epostr)

! Orbit residuals:
    IF ((resHed%dsc%ndiff == 0 .AND.                       &
         resHed%filHead(ii)%stanam(1)(1:3) == 'ARC') .OR.  &
         resHed%dsc%pgName == 'ORBGEN') THEN

      DO jj = 1, resHed%filHead(ii)%nfrFil
        IF (resHed%filHead(ii)%iCarr(jj) < 1 .OR. &
            resHed%filHead(ii)%iCarr(jj) > 3) THEN
            resHed%filHead(ii)%iCarr(jj) = -5
        ENDIF
      ENDDO

      WRITE(output%value(ii),'(I4,2X,A16,4X,2X,A,5X,A,3X,3(2X,A))')     &
           ii, resHed%filHead(ii)%staNam(1), epostr,'Orbit',            &
           (TRIM(freqID(resHed%filHead(ii)%iCarr(jj)+5)),&
                                    jj=1,resHed%filHead(ii)%nfrFil)

! Zero diff observations:
    ELSE IF ((resHed%dsc%nDiff ==  0) .OR. &
             (resHed%dsc%nDiff == 99 .AND. resHed%dsc%nResta == 0)) THEN

      DO jj = 1, resHed%filHead(ii)%nfrFil
        IF (resHed%filHead(ii)%iCarr(jj) < 1 .OR. &
            resHed%filHead(ii)%iCarr(jj) > 5) THEN
            resHed%filHead(ii)%iCarr(jj) = 0
        ENDIF
      ENDDO

      mDiff = resHed%dsc%nResta + resHed%dsc%nResat + resHed%dsc%nResep

      WRITE(output%value(ii),'(I4,2X,A16,4X,2X,A,5X,A,A,3X,3(2X,A))')       &
           ii, resHed%filHead(ii)%staNam(1), epostr,                        &
           filtyp(resHed%filHead(ii)%meatyp),' ' // TRIM(difTyp(1+mDiff)),  &
           (TRIM(freqID(resHed%filHead(ii)%iCarr(jj))),&
                                     jj=1,resHed%filHead(ii)%nfrFil)

! Single diff observations:
    ELSE

      DO jj = 1, resHed%filHead(ii)%nfrFil
        IF (resHed%filHead(ii)%iCarr(jj) < 1 .OR. &
            resHed%filHead(ii)%iCarr(jj) > 5) THEN
            resHed%filHead(ii)%iCarr(jj) = 0
        ENDIF
      ENDDO

      mDiff = resHed%dsc%nResta + resHed%dsc%nResat + resHed%dsc%nResep

      WRITE(output%value(ii),                                               &
            '(I4,2X,A16,4X,2X,A16,4X,2X,A,5X,A,A,3X,3(2X,A))')              &
           ii, resHed%filHead(ii)%staNam(:), epostr,                        &
           filtyp(resHed%filHead(ii)%meatyp),' ' // TRIM(difTyp(1+mDiff)),  &
           (TRIM(freqID(resHed%filHead(ii)%iCarr(jj))),&
                                     jj=1,resHed%filHead(ii)%nfrFil)
    ENDIF
  ENDDO

! Deallocate
! ----------
  DO ii=1,resHed%nFil
    DEALLOCATE(resHed%filHead(ii)%numSat,stat=irc)
  ENDDO
  DEALLOCATE(resHed%filHead,stat=irc)


  RETURN
END SUBROUTINE menu_res

END MODULE
