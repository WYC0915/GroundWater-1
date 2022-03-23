MODULE s_OPNSYS
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE opnsys

! -------------------------------------------------------------------------
! Purpose:    This is a new version of the old subroutine OPNSYS.f. Used to
!             open the general programt files SYSINP, SYSOUT (Program output),
!             and  SYSERR (for error messages). Allows to use default names,
!             and merge SYSERR to SYSOUT
!
! Author:     P. Fridez
!
! Created:    06-Nov-2002
! Last mod.:  19-Jul-2010
!
! Changes:    16-Jan-2003 PF: Write OPNERR for lfnerr to lfncon (std output)
!             11-Mar-2003 RD: Warning if SYSOUT and SYSODEF are inconsistent
!                             Delete SYSOUT and SYSERR before open to write
!             15-Mar-2003 HU: Open files as unknown
!             17-Mar-2003 RD: Use inquire for deletion of old files
!             31-Oct-2003 HU: Filenames are CHR*255
!             19-Jul-2010 SL: tab characters removed
!
!
! SR called:  ckoptb,exitrc,gtflna,opnerr,opnfil,tstkey
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern

  USE s_opnfil
  USE s_inquire
  USE s_opnerr
  USE f_tstkey
  USE s_exitrc
  USE s_ckoptb
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! input

! output

! Local Variables
! ---------------
  INTEGER(i4b)                        :: lfnCon ! standard output for error
                                                ! when opening the SYSERR
  INTEGER(i4b)                        :: ii
  INTEGER(i4b)                        :: jobnum
  INTEGER(i4b)                        :: irc ,ios
  INTEGER(i4b)                        :: ircOut
  INTEGER(i4b)                        :: irCode
  INTEGER(i4b)                        :: iostat

  CHARACTER(LEN=255)                  :: filnam ! filename to open
  CHARACTER(LEN=255)                  :: filout ! filename to open
  CHARACTER(LEN=1)                    :: cdummy

  LOGICAL                             :: resultL ! true if checkbox is avtive
  LOGICAL                             :: yes

! Open SYSINP
! ----------
  CALL gtflna(0, 'SYSINP', filnam, irc)
  IF (irc==0) THEN
    CALL OPNFIL(LFNKBD,FILNAM,'OLD',' ',' ',cdummy,IOSTAT)
    CALL OPNERR(LFNERR,LFNKBD,IOSTAT,FILNAM,'OPNSYS')
  ENDIF

! Open SYSOUT or PGMOUT
! ---------------------
  irCode = 0
  resultL = .FALSE.


  CALL gtflna(0,'SYSOUT',filOut,ircOut)

  IF (ircOut == 0) THEN
    lfnprt = lfnprf              ! checked: open default output file

    ! Delete old file
    CALL inquire(file=filOut,exist=yes)
    IF (yes) THEN
      CALL opnfil(lfnprt,filOut,'UNKNOWN',' ',' ',cdummy,iostat)
      CALL opnerr(lfnerr,lfnprt,iostat,filOut,'OPNSYS')
      CLOSE(lfnprt,status='DELETE')
    ENDIF

    CALL opnfil(lfnprt,filOut,'NEW',' ',' ',cdummy,iostat)
    CALL opnerr(lfnerr,lfnprt,iostat,filOut,'OPNSYS')

  ENDIF

! Open SYSERR
! ----------
  IF (TSTKEY('ERRMRG')) THEN
    CALL ckoptb(1, (/'ERRMRG'/),'OPNSYS', &
                'Merge program messages into output',irCode,&
                resultL=resultL)  !check status of checkbox

    IF (resultL) THEN
      lfnerr = lfnprt
    ELSE
      CALL gtflna(1,'SYSERR',filnam,irc)

      IF (filnam == 'SYSOUT') THEN
        lfnerr = lfnprt
      ELSE
        lfncon = lfnerr
        lfnerr = lfnerf

        ! Delete old file
        CALL inquire(file=filnam,exist=yes)
        IF (yes) THEN
          CALL OPNFIL(lfnerr,filnam,'UNKNOWN',' ',' ',cdummy,iostat)
          CALL OPNERR(lfncon,lfnerr,iostat,filnam,'OPNSYS')
          CLOSE(lfnerr,status='DELETE')
        ENDIF

        CALL OPNFIL(lfnerr,filnam,'NEW',' ',' ',cdummy,iostat)
        CALL OPNERR(lfncon,lfnerr,iostat,filnam,'OPNSYS')
      ENDIF

    ENDIF

! For input files still not updated:
  ELSE

    CALL gtflna(0,'SYSERR',filnam,irc)

    IF (irc /= 0 .OR. filnam == 'SYSOUT' .OR. LEN_TRIM(filnam) == 0) THEN
      lfnerr = lfnprt
    ELSE
      lfncon = lfnerr
      lfnerr = lfnerf

      ! Delete old file
      CALL OPNFIL(lfnerr,filnam,'NEW',' ',' ',cdummy,iostat)
      CALL OPNERR(lfncon,lfnerr,iostat,filnam,'OPNSYS')
      CLOSE(lfnerr,status='DELETE')

      CALL OPNFIL(lfnerr,filnam,'NEW',' ',' ',cdummy,iostat)
      CALL OPNERR(lfncon,lfnerr,iostat,filnam,'OPNSYS')
    ENDIF

  ENDIF

! Check SYSOUT and SYSODEF
! ------------------------
  IF (ircOut == 0 .AND. LEN_TRIM(filOut) > 1 .AND. tstKey('SYSODEF')) THEN
    ii = LEN_TRIM(filOut)
    READ(filOut(ii-1:ii),'(I2)',iostat=ios) jobnum

    CALL ckoptb(1, (/'SYSODEF'/),'OPNSYS', &
                'Use default name for program output',irCode,&
                resultL=resultL)

    IF (.NOT. resultL .AND. ios == 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/)')                                &
            ' ### SR OPNSYS: Special program output requested, ' //  &
                      'but default file name found.',                &
                      'Program output: ' // TRIM(filOut)
    ENDIF
  ENDIF

  IF (irCode /= 0) CALL exitrc(2)

RETURN
END SUBROUTINE opnsys

END MODULE
