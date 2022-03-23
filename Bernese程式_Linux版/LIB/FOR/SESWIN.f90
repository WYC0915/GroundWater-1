MODULE s_SESWIN
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE seswin(sesKeyw, t1inp, t2inp, t1out, t2out, t1ses, t2ses, ircode)

! -------------------------------------------------------------------------
! Purpose:    Gives back the session boundaries for t1inp>t1out
!             resp. t2inp<t2out
!
! Author:     R. Dach
!
! Created:    23-Nov-2000
!
! Changes:    15-Dec-2000 RD: Give back session IDs
!             25-Sep-2002 HU: Remove i_astlib
!             15-Apr-2003 RD: Use strcture for session table
!             23-Apr-2003 RD: Nullify local pointers
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules:
! --------
  USE m_bern,   ONLY: i4b, r8b, fileNameLength
  USE d_sess,   ONLY: t_sesLst,t_sess,init_sesLst

  USE s_alcerr
  USE s_rdsess
  USE f_djul
  USE s_jmt
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! input
  CHARACTER(LEN=*)  :: sesKeyw ! Keyword for session file
  REAL(r8b)         :: t1inp   ! lower boundary of the input interval
  REAL(r8b)         :: t2inp   ! upper boundary of the input interval

! output
  REAL(r8b)         :: t1out   ! lower boundary of the session interval
  REAL(r8b)         :: t2out   ! upper boundary of the session interval
  CHARACTER(LEN=4)  :: t1ses   ! session ID for t1out
  CHARACTER(LEN=4)  :: t2ses   ! session ID for t2out
  INTEGER(i4b)      :: irCode  ! return code
                               !   0: session boundaries found
                               !   1: no session found
                               !   2: more than one session found

! Local Parameters
! ----------------
  CHARACTER(LEN=6),  PARAMETER         :: srName = 'seswin'

  REAL(r8b),         PARAMETER         :: dt = 0.5d0/86400d0  ! Tolerance


! Local Variables
! ---------------
  TYPE(t_sesLst)                       :: sesTbl   ! Session table
  TYPE(t_sess),     DIMENSION(:,:),     &
                           ALLOCATABLE :: sesHlp

  CHARACTER(LEN=fileNameLength)        :: sesFile  ! Name of the session file

  REAL(r8b)                            :: dd       ! day of month
  REAL(r8b)                            :: doy      ! day of year

  INTEGER(i4b)                         :: isess    ! counter for sessions
  INTEGER(i4b), DIMENSION(2)           :: sFound
  INTEGER(i4b)                         :: jj,mm    ! year, month
  INTEGER(i4b)                         :: irc,iac  ! return code from SRs


! Initialization
! --------------
  t1out=0D0
  t2out=1D20

  t1ses=' '
  t2ses=' '

  sFound = 0

  CALL init_sesLst(sesTbl)

! Get the name of the session file
! --------------------------------
  CALL gtflna(1,sesKeyw,sesFile,irc)

! Read the list of sessions
! -------------------------
  CALL rdsess(sesFile,sesTbl)

! Allocate temp. sessIDs
! ----------------------
  ALLOCATE(sesHlp(sesTbl%nSess,2),stat=iac)
  CALL alcerr(iac,'sessnr',(/sesTbl%nSess,2/),srName)

  sesHlp(1:sesTbl%nSess,1) = sesTbl%sess(1:sesTbl%nSess)
  sesHlp(1:sesTbl%nSess,2) = sesTbl%sess(1:sesTbl%nSess)

! Modify boundaries for wildcard sessions
! ---------------------------------------
  DO isess=1,sesTbl%nsess

! lower boundary for "open" session
    IF (.NOT. sesTbl%fix) THEN

      sesHlp(iSess,1)%timint%t(1) = DINT(t1inp+dt) + &
          sesTbl%sess(iSess)%timint%t(1)-DINT(sesTbl%sess(iSess)%timint%t(1))

      sesHlp(iSess,1)%timint%t(2) = DINT(t1inp+dt) + &
          sesTbl%sess(iSess)%timint%t(2)-DINT(sesTbl%sess(iSess)%timint%t(2))

      IF (sesHlp(iSess,1)%timint%t(2) < sesHlp(iSess,1)%timint%t(1)) &
        sesHlp(iSess,1)%timint%t(2) = sesHlp(iSess,1)%timint%t(2) + 1d0

      CALL jmt(t1inp+0.5/86400D0, jj, mm, dd)
      DOY=t1inp-djul(jj,1,1D0)+1D0
      WRITE(sesHlp(isess,1)%sessID(1:3),'(I3.3)') INT(DOY)

      IF (sesHlp(iSess,1)%timint%t(1)-dt > t1inp) THEN
        sesHlp(iSess,1)%timint%t = sesHlp(iSess,1)%timint%t - 1d0
        WRITE(sesHlp(isess,1)%sessID(1:3),'(I3.3)') INT(DOY)-1
      ENDIF

    ENDIF

! upper boundary is given "open"
    IF (.NOT. sesTbl%fix) THEN

      sesHlp(iSess,2)%timint%t(1) = DINT(t2inp+0.5/86400D0) + &
          sesTbl%sess(iSess)%timint%t(1)-DINT(sesTbl%sess(iSess)%timint%t(1))

      sesHlp(iSess,2)%timint%t(2) = DINT(t2inp+0.5/86400D0) + &
          sesTbl%sess(iSess)%timint%t(2)-DINT(sesTbl%sess(iSess)%timint%t(2))

      IF (sesHlp(iSess,2)%timint%t(2) < sesHlp(iSess,2)%timint%t(1)) &
        sesHlp(iSess,2)%timint%t(2) = sesHlp(iSess,2)%timint%t(2) + 1d0

      CALL jmt(t2inp+0.5/86400D0, jj, mm, dd)
      DOY=t2inp-djul(jj,1,1D0)+1D0
      WRITE(sesHlp(isess,2)%sessId(1:3),'(I3.3)') INT(DOY)

      IF (sesHlp(iSess,2)%timint%t(2)+dt < t2inp) THEN
        sesHlp(iSess,2)%timint%t = sesHlp(iSess,2)%timint%t + 1d0
        WRITE(sesHlp(isess,1)%sessID(1:3),'(I3.3)') INT(DOY)+1
      ENDIF

    ENDIF
  ENDDO

! Does one session fix the interval t1inp..t2inp?
! -----------------------------------------------
  DO isess=1,sesTbl%nsess

    IF (DABS(sesHlp(iSess,1)%timint%t(1)-sesHlp(iSess,2)%timint%t(1))>dt .OR. &
        DABS(sesHlp(iSess,1)%timint%t(2)-sesHlp(iSess,2)%timint%t(2))>dt) CYCLE

    IF (t1inp >= sesHlp(iSess,1)%timint%t(1)+dt .AND. &
        t2inp <= sesHlp(iSess,1)%timint%t(2)-dt) THEN

      t1out=sesHlp(iSess,1)%timint%t(1)
      t2out=sesHlp(iSess,2)%timint%t(2)

      t1ses=sesHlp(isess,1)%sessID
      t2ses=sesHlp(isess,2)%sessID

      sFound = 1

    ENDIF
  ENDDO

! If more than one session is involved, take the nearest boundaries
! -----------------------------------------------------------------
  IF (t1out == 0d0 .AND. t2out == 1D20) THEN

    DO isess=1,sesTbl%nsess

      IF (t1inp >= sesHlp(iSess,1)%timint%t(1)-dt .AND. &
          t1inp  < sesHlp(iSess,1)%timint%t(2)+dt) THEN

        IF (t1out < sesHlp(iSess,1)%timint%t(1)+dt) THEN
          t1out=sesHlp(iSess,1)%timint%t(1)
          t1ses=sesHlp(isess,1)%sessID
        ENDIF

        sFound(1)=sFound(1)+1

      ENDIF

      IF (t2inp <= sesHlp(iSess,2)%timint%t(2)+dt .AND. &
          t2inp  > sesHlp(iSess,2)%timint%t(1)-dt) THEN

        IF (t2out  > sesHlp(iSess,2)%timint%t(2)-dt) THEN

          t2out=sesHlp(iSess,2)%timint%t(2)
          t2ses=sesHlp(isess,2)%sessId

        ENDIF

        sFound(2)=sFound(2)+1

      ENDIF
    ENDDO

  ENDIF

! Still no lower boundary found
! -----------------------------
  IF (t1out == 0D0 .AND. sesTbl%fix) THEN
    t1out=t2inp+99999D0
    DO isess=1,sesTbl%nsess
      IF (sesHlp(iSess,1)%timint%t(1)-t1inp < (t1out-t1inp) .AND. &
          sesHlp(iSess,1)%timint%t(1)       < t2inp         .AND. &
          sesHlp(iSess,1)%timint%t(2)       > t1inp) THEN
        t1out=sesHlp(iSess,1)%timint%t(1)
        t1ses=sesHlp(isess,1)%sessID
        sFound(1)=2
      ENDIF
    ENDDO
    IF (t1out==t2inp+99999D0) t1out=0D0
  ENDIF

! Still no upper boundary found
! -----------------------------
  IF (t2out == 1D20 .AND. sesTbl%fix) THEN
    t2out=t1inp-99999D0
    DO isess=1,sesTbl%nsess
      IF (t2inp-sesHlp(iSess,2)%timint%t(2) < (t2inp-t2out) .AND. &
          sesHlp(iSess,2)%timint%t(2)       > t1inp         .AND. &
          sesHlp(iSess,2)%timint%t(1)       < t2inp) THEN
        t2out=sesHlp(iSess,2)%timint%t(2)
        t2ses=sesHlp(isess,2)%sessID
        sFound(2)=3
      ENDIF
    ENDDO
    IF (t2out==t1inp-99999D0) t2out=1D20
  ENDIF

! Set the return code
! -------------------
  irCode=1
  IF (sFound(1) == 1 .AND. sFound(2) == 1) irCode=0
  IF (sFound(1) >  1 .OR.  sFound(2) >  1) irCode=2

  DEALLOCATE(sesHlp,stat=iac)
  DEALLOCATE(sesTbl%sess,stat=irc)

  RETURN
END SUBROUTINE seswin

END MODULE
