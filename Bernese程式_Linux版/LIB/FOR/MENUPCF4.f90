MODULE s_MENUPCF4
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menupcf4(keyWord,pcf,pcf2,idx)

! -------------------------------------------------------------------------
! Purpose:    Synchronize PIDs, script names and options directories in
!             different uniline sections
!
! Author:     R. Dach
!
! Created:    10-Dec-2001
! Last mod.:  28-Aug-2002
!
! Changes:    28-Aug-2002  RD: Improve the algorithm
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE p_bpe,    ONLY: t_pcf

  USE s_alcerr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)                  :: keyWord  ! what to do
  TYPE(t_pcf)                       :: pcf  ! Main PCF list
  TYPE(t_pcf)                       :: pcf2 ! PCF list to sort into PCF
! output:
  INTEGER(i4b),DIMENSION(:),POINTER :: idx ! Index of pcf2 in pcf

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER       :: srname = 'menupcf4'

! Local Variables
! ---------------
  INTEGER(i4b)                      :: pid2
  INTEGER(i4b)                      :: ii,jj,kk,i1
  INTEGER(i4b)                      :: iPar
  INTEGER(i4b)                      :: irc,ios

! Allocate and init index array
! -----------------------------
  DEALLOCATE(Idx,stat=irc)

  ALLOCATE(Idx(pcf%n_Pid),stat=irc)
  CALL alcerr(irc,'Idx',(/ pcf%n_pid /), srName)
  idx = 0

! PID, script name, and option directory are identical
! ----------------------------------------------------
  DO ii = 1,pcf%n_Pid
    DO jj = 1,pcf2%n_Pid

      IF (pcf%job(ii)%iPid   == pcf2%job(jj)%iPid   .AND. &
          pcf%job(ii)%script == pcf2%job(jj)%script .AND. &
          pcf%job(ii)%option == pcf2%job(jj)%option) THEN
        idx(ii) = jj
        EXIT
      ENDIF

    ENDDO
  ENDDO

! Script name or option directory has been changed
! ------------------------------------------------
  DO ii = 1,pcf%n_Pid
    IF (idx(ii) /= 0) CYCLE

    DO jj = 1,pcf2%n_Pid

      IF (pcf%job(ii)%iPid   == pcf2%job(jj)%iPid   .AND. &
          pcf%job(ii)%script == pcf2%job(jj)%script) THEN
        idx(ii) = jj
        EXIT
      ENDIF

      IF (pcf%job(ii)%iPid   == pcf2%job(jj)%iPid   .AND. &
          pcf%job(ii)%option == pcf2%job(jj)%option) THEN
        idx(ii) = jj
        EXIT
      ENDIF

    ENDDO
  ENDDO

! PID has been changed,
! Script name and option directory are still identical
! ----------------------------------------------------
  DO ii = 1,pcf%n_Pid
    IF (idx(ii) /= 0) CYCLE

    DO jj = 1,pcf2%n_Pid

      IF (pcf%job(ii)%script == pcf2%job(jj)%script .AND. &
          pcf%job(ii)%option == pcf2%job(jj)%option) THEN

        ! Do not use any "special" action file
        i1 = pcf2%job(jj)%pType
        DO kk = 1,pcf2%n_Pid
          IF (pcf2%job(kk)%pType == 1 .AND. &
              pcf2%job(kk)%params(2) == pcf2%job(jj)%params(1)) i1 = kk
        ENDDO
        IF (i1 /= 0) CYCLE

        ! Reset all NEXTJOB-PIDs to the new one
        DO kk = 1,pcf2%n_Pid
          IF (pcf2%job(kk)%pType == 2) THEN
            DO iPar = 1,SIZE(pcf2%job(kk)%params)
              READ(pcf2%job(kk)%params(iPar),*,iostat=ios) pid2
              IF (ios /= 0) CYCLE
              IF (pid2 == pcf2%job(jj)%iPid) THEN
                pcf2%job(kk)%params(iPar) = ' '
                WRITE(pcf2%job(kk)%params(iPar),'(I3.3)') pcf%job(ii)%iPid
              ENDIF
            ENDDO
          ENDIF
        ENDDO
        pcf2%job(jj)%iPid = pcf%job(ii)%iPid

        idx(ii) = jj
        EXIT
      ENDIF

    ENDDO
  ENDDO

! Give a warning if the advanced logic was necessary for writing the PCFile
! -------------------------------------------------------------------------
  IF (keyWord /= 'PCF_SAVE') RETURN

  DO ii = 1,pcf%n_Pid
    jj = idx(ii)
    IF (jj == 0            .OR.        ii /= jj   .OR. &
        pcf%job(ii)%iPid   /= pcf2%job(jj)%iPid   .OR. &
        pcf%job(ii)%script /= pcf2%job(jj)%script .OR. &
        pcf%job(ii)%option /= pcf2%job(jj)%option) THEN
      WRITE(lfnerr,'(/,A,2(/,18X,A),/)')                                 &
        ' ### SR MENUPCF4: You did changes in one part of the PCFile.',  &
        'But you did not completelly adapted the other parts.',          &
        'Please check the stored file carefully!'
      EXIT
    ENDIF
  ENDDO


  RETURN
END SUBROUTINE menupcf4

END MODULE
