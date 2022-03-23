MODULE s_RNXSES
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE rnxses(rnxFil, timEpo, iUsWin, window, sesUse)

! -------------------------------------------------------------------------
! Purpose:    Detect session ID for the rinex file (RXOBV3)
!
! Parameters:
!         in: rnxFil : name of the rinex file    chr
!             timEpo : first, mean, last epoch   r8b(3)
!             iUsWin : Option: Enforce time win. i4b
!        out: window : time window to cut        r8b(2)
!             sesUse : Session to use            chr
!
!
! Author:     R. Dach
!
! Created:    22-Dec-2000
! Last mod.:  08-Aug-2005
!
! Changes:    29-Oct-2002 MR: Correct format (2x)
!             23-Apr-2003 RD: Nullify local pointers
!             28-May-2003 RD: Get name of file with SR GTFLNA
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!
! SR used:    seswin, gtflna, timst2
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

!*
!
  USE m_bern
  USE s_seswin
  USE s_timst2
  USE s_gtflna
  IMPLICIT NONE
!
!
! Global Parameters
! -----------------
  CHARACTER(LEN=fileNameLength)     :: rnxFil ! name of the rinex file
  REAL(r8b)        , DIMENSION(3)   :: timEpo ! first, mean, last epoch
  INTEGER(i4b)                      :: iUsWin ! Option: Enforce time win.
  REAL(r8b)        , DIMENSION(2)   :: window ! time window to cut
  CHARACTER(LEN=4)                  :: sesUse ! Session to use
!************************************************************************
!
! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength)     :: sesFil ! Name of session file
  CHARACTER(LEN=19), DIMENSION(4)   :: strEpo ! String of timepo
  CHARACTER(LEN= 4), DIMENSION(4)   :: sesEpo ! Session IDs
  INTEGER(i4b)                      :: irc    ! return code
  INTEGER(i4b)                      :: irc1   ! return code
  INTEGER(i4b)                      :: irc2   ! return code
  INTEGER(i4b)                      :: irc3   ! return code
  INTEGER(i4b)                      :: irc4   ! return code
  REAL(r8b)        , DIMENSION(4,2) :: winEpo ! Session boundaries

! Get the Session ID for the First, Middle, and Last Epoch
! --------------------------------------------------------
  CALL seswin('SESSION_TABLE',timEpo(1), timEpo(1),           &
              winEpo(1,1),winEpo(1,2),sesEpo(1),sesEpo(1),irc1)
  CALL seswin('SESSION_TABLE',timEpo(2), timEpo(2),           &
              winEpo(2,1),winEpo(2,2),sesEpo(2),sesEpo(2),irc2)
  CALL seswin('SESSION_TABLE',(timEpo(1)+timEpo(3))/2D0,      &
              (timEpo(1)+timEpo(3))/2D0,                      &
              winEpo(3,1),winEpo(3,2),sesEpo(3),sesEpo(3),irc3)
  CALL seswin('SESSION_TABLE',timEpo(3), timEpo(3),           &
              winEpo(4,1),winEpo(4,2),sesEpo(4),sesEpo(4),irc4)
  CALL gtflna(1,'SESSION_TABLE',sesFil,irc)
!
  CALL timst2(1,1,timEpo(1),strEpo(1))
  CALL timst2(1,1,timEpo(2),strEpo(2))
  CALL timst2(1,1,timEpo(3),strEpo(4))
  CALL timst2(1,1,(timEpo(1)+timEpo(3))/2D0,strEpo(3))
!
! No Session Found
! ----------------
  IF (irc1==1 .OR. irc2==1 .OR. irc3==1 .OR. irc4==1) THEN
    WRITE(lfnerr, '(/,A,/,16X,A,/,16X,A,2(/,16X,A,A))')                &
    ' ### PG RXOBV3: NO SUITABLE SESSION FOUND',                       &
                    'SESSION FOR THE BERNESE FILES WILL BE EXTRACTED', &
                    'FROM THE NAME OF THE RINEX FILE',                 &
                    'SESSION TABLE :  ',TRIM(sesFil),                  &
                    'RINEX FILE    :  ',TRIM(rnxFil)
    IF (irc1 == 1) WRITE(lfnerr,'(16X,A,A)')                           &
                    'FIRST EPOCH   :  ',TRIM(strEpo(1))
    IF (irc2 == 1) WRITE(lfnerr,'(16X,A,A)')                           &
                    'MEAN EPOCH    :  ',TRIM(strEpo(2))
    IF (irc3 == 1) WRITE(lfnerr,'(16X,A,A)')                           &
                    'MIDDLE EPOCH  :  ',TRIM(strEpo(3))
    IF (irc4 == 1) WRITE(lfnerr,'(16X,A,A)')                           &
                    'LAST EPOCH    :  ',TRIM(strEpo(4))
    WRITE(lfnerr,*)
  ELSE
!
! More Than One Session Found for at Least One Epoch
! --------------------------------------------------
    IF(irc1==2 .OR. irc2==2 .OR. irc3==2 .OR. irc4==2)THEN
      WRITE(lfnerr, '(/,A,/,16X,A,2(/,16X,A,A))')                           &
      ' ### PG RXOBV3: MORE THAN ONE SESSION FOUND FOR AT LEAST ONE EPOCH', &
                      'PLEASE, CHECK THE SESSION TABLE CAREFULLY!!!',       &
                      'SESSION TABLE :  ',TRIM(sesFil),                &
                      'RINEX FILE    :  ',TRIM(rnxFil)
      IF (irc1 == 2) WRITE(lfnerr,'(16X,A,A)')                              &
                      'FIRST EPOCH   :  ',TRIM(strEpo(1))
      IF (irc2 == 2) WRITE(lfnerr,'(16X,A,A)')                              &
                      'MEAN EPOCH    :  ',TRIM(strEpo(2))
      IF (irc3 == 2) WRITE(lfnerr,'(16X,A,A)')                              &
                      'MIDDLE EPOCH  :  ',TRIM(strEpo(3))
      IF (irc4 == 2) WRITE(lfnerr,'(16X,A,A)')                              &
                      'LAST EPOCH    :  ',TRIM(strEpo(4))
      WRITE(lfnerr,*)
    ENDIF
!
! The Normal Case: All Epochs Are in the Same Session
! ---------------------------------------------------
    IF (sesEpo(1)==sesEpo(2) .AND. sesEpo(2)==sesEpo(4)) THEN
      sesUse=sesEpo(2)
    ENDIF
!
! The First or Last Epoch is Outside (has another session)
! --------------------------------------------------------
    IF ((sesEpo(1)/=sesEpo(2) .AND. sesEpo(2)==sesEpo(4))  .OR. &
        (sesEpo(1)==sesEpo(2) .AND. sesEpo(2)/=sesEpo(4))) THEN
      WRITE(*, '(/,A,/,16X,A,6(/,16X,A,A),/)')                                          &
      ' ### PG RXOBV3: MORE THAN ONE SESSION FOUND FOR THE OBS. IN THE RINEX FILE',    &
                      'PLEASE, CHECK THEN SESSION FOR THE BERNESE FILES CAREFULLY!!!', &
                      'SESSION TABLE  :  ',TRIM(sesFil),                               &
                      'RINEX FILE     :  ',TRIM(rnxFil),                               &
                      'FIRST EPOCH    :  ',TRIM(strEpo(1))//' ('//TRIM(sesEpo(1))//')',&
                      'MEAN EPOCH     :  ',TRIM(strEpo(2))//' ('//TRIM(sesEpo(2))//')',&
                      'LAST EPOCH     :  ',TRIM(strEpo(4))//' ('//TRIM(sesEpo(4))//')',&
                      'SESSION ID USED:  ',sesEpo(2)
      sesUse=sesEpo(2)
    ENDIF
!
! The RINEX File Covers More Than One Session with Observations
! -------------------------------------------------------------
    IF (sesEpo(1)/=sesEpo(2) .AND. sesEpo(2)/=sesEpo(4) .AND. &
        sesEpo(1)/=sesEpo(4) .AND. sesEpo(2)==sesEpo(3)) THEN
      WRITE(*, '(/,A,/,16X,A,6(/,16X,A,A),/)')                                         &
      ' ### PG RXOBV3: MORE THAN ONE SESSION FOUND FOR THE OBS. IN THE RINEX FILE',    &
                      'PLEASE, CHECK THEN SESSION FOR THE BERNESE FILES CAREFULLY!!!', &
                      'SESSION TABLE  :  ',TRIM(sesFil),                               &
                      'RINEX FILE     :  ',TRIM(rnxFil),                               &
                      'FIRST EPOCH    :  ',TRIM(strEpo(1))//' ('//TRIM(sesEpo(1))//')',&
                      'MEAN EPOCH     :  ',TRIM(strEpo(2))//' ('//TRIM(sesEpo(2))//')',&
                      'LAST EPOCH     :  ',TRIM(strEpo(4))//' ('//TRIM(sesEpo(4))//')',&
                      'SESSION ID USED:  ',sesEpo(2)
      sesUse=sesEpo(2)
    ENDIF
!
! The RINEX File Covers More Than One Session, Observations Are Wild Distributed
! ------------------------------------------------------------------------------
    IF (sesEpo(1)/=sesEpo(2) .AND. sesEpo(2)/=sesEpo(4) .AND. &
        sesEpo(1)/=sesEpo(4) .AND. sesEpo(2)/=sesEpo(3)) THEN
      WRITE(*, '(/,A,/,16X,A,6(/,16X,A,A),/)')                                         &
      ' ### PG RXOBV3: MORE THAN ONE SESSION FOUND FOR THE OBS. IN THE RINEX FILE',    &
                      'PLEASE, CHECK THEN SESSION FOR THE BERNESE FILES CAREFULLY!!!', &
                      'SESSION TABLE  :  ',TRIM(sesFil),                               &
                      'RINEX FILE     :  ',TRIM(rnxFil),                               &
                      'FIRST EPOCH    :  ',TRIM(strEpo(1))//' ('//TRIM(sesEpo(1))//')',&
                      'MEAN EPOCH     :  ',TRIM(strEpo(2))//' ('//TRIM(sesEpo(2))//')',&
                      'LAST EPOCH     :  ',TRIM(strEpo(4))//' ('//TRIM(sesEpo(4))//')',&
                      'SESSION ID USED:  ',sesEpo(2)
      sesUse=sesEpo(2)
    ENDIF
  ENDIF
!
! Set Time window for Open Definition if Required
! ------------------------------------------------
  IF (iUsWin == 1) THEN
    window(1) = winEpo(2,1)
    window(2) = winEpo(2,2)
  ENDIF
!
! Set Time window for Open Definition if Required
! ------------------------------------------------
  IF (window(1) + window(2) /= 0D0) THEN
    IF (window(1)<1000.0) THEN
      window(1) = DINT(timEpo(1)+0.5/86400.0) + MOD(window(1),1D0)
    ENDIF
    IF (window(2)<1000.0) THEN
      window(2) = DINT(timEpo(3)+0.5/86400.0) + MOD(window(2),1D0)
      IF (window(2)+0.5/86400.0 < window(1)) window(2)=window(2)+1
    ENDIF
  ELSE
    window(1)=timEpo(1)
    window(2)=timEpo(3)
  ENDIF

  RETURN
  END SUBROUTINE

END MODULE
