MODULE s_RDSTDH
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdstdh(filstd,stdHead,irCode)

! -------------------------------------------------------------------------
! Purpose:    Read standard orbit file header information
!
! Author:     U. Hugentobler
!
! Created:    23-Jun-2001
! Last mod.:  19-May-2011
!
! Changes:    16-May-2003 HU: Deallocate arrays
!             21-May-2003 RD: Make the deallocation safe
!             02-Jun-2003 HU: Return narc
!             06-Aug-2003 HU: New STD format, check nutation model
!             29-Aug-2003 HU: Initialize nutnam, subnam
!             13-Sep-2003 HU: Interface for rdnutsub
!             28-Mar-2007 HB: Change call of CHKSYS (IERS2003 CONV)
!             06-May-2011 HB: Set meanPole model and check with
!                             second STD-file
!             19-May-2011 HB: SR chkModKey called without srName
!
! SR used:    gtflna, opnfil, opnerr, exitrc, alcerr
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_model,  ONLY: setModKey,chkModKey,mod_orb_meaPol,chrValLength
  USE d_stdorb, ONLY: t_stdhead

  USE s_opnfil
  USE s_alcerr
  USE s_exitrc
  USE s_chksys
  USE s_rdnutsub
  USE s_gtflna
  USE s_opnerr
  IMPLICIT NONE
!
! Variables in parameter list
! ---------------------------
! IN:
  CHARACTER(LEN=fileNameLength) &
                         :: filstd    ! file name, blank: use GTFLNA mechanism
                                      ! with keyword STDORB
! OUT:
  TYPE(t_stdhead)        :: stdHead   ! Structure of std header info
  INTEGER(i4b)           :: irCode    ! Return code of this SR


! Local Variables
! ---------------
  INTEGER(i4b),SAVE      :: init=1     ! =0: buffer initialized
  INTEGER(i4b),PARAMETER :: maxSat=100 ! internal maximum declaration
  INTEGER(i4b)           :: irc,iostat,irChk
  INTEGER(i4b)           :: iarc,narc,isat,nsat,iint,nint,ideg,ndeg
  INTEGER(i4b)           :: method
  INTEGER(i4b)           :: ifmt,nlin,ilin,mPol
  INTEGER(i4b),DIMENSION(maxSat) :: svntmp

  REAL(r8b)              :: tb1,tb2,axis,tlocal,polyco

  CHARACTER(LEN=8),PARAMETER         :: srName='rdStdH  '
  CHARACTER(LEN=fileNameLength),SAVE :: filorb='$'
  CHARACTER(LEN=chrValLength)        :: meanPol
  CHARACTER(LEN=80)                  :: line
  CHARACTER(LEN=16)                  :: nutnam,subnam

  LOGICAL, SAVE                      :: mFirst = .TRUE.

  irCode=0


! Reading necessary for init=1 or filorb /= filstd
! ------------------------------------------------
  IF (init == 1 .OR. (filstd /= ' ' .AND. filstd /= filorb)) THEN
    CALL rdnutsub(nutnam,subnam)

! Apply GTFLNA mechanism
    IF (filstd == ' ') THEN
      CALL gtflna(1,'STDORB',filorb,irc)
    ELSE
      filorb=filstd
    ENDIF

! STANDARD ORBIT AVAILABLE
! ------------------------
    CALL opnfil(lfnloc,filorb,'OLD','UNFORMATTED','READONLY',' ',iostat)
    CALL opnerr(lfnerr,lfnloc,iostat,filorb,'RDSTDH')

! Deallocate arrays
    IF (ASSOCIATED(stdHead%arc)) THEN
      DO iarc=1,stdHead%narc
        IF (iarc > SIZE(stdHead%arc)) EXIT
        DEALLOCATE(stdHead%arc(iarc)%sat,STAT=iostat)
      ENDDO
      DEALLOCATE(stdHead%arc,STAT=iostat)
    ENDIF

! Read number of arcs on file
    nutnam=' '
    subnam=' '
    stdHead%ifrmt=1
    READ(lfnloc,IOSTAT=iostat) narc
    IF (iostat /= 0) THEN
      WRITE(lfnerr,"(/,' *** SR RDSTDH: Reading error record 1')")
      CALL exitrc(2)
    ENDIF
    IF (narc<0) THEN
      READ(lfnloc,IOSTAT=iostat) ifmt,narc
      IF (iostat /= 0) THEN
        WRITE(lfnerr,"(/,' *** SR RDSTDH: Reading error record 1a')")
        CALL exitrc(2)
      ENDIF
      READ(lfnloc,IOSTAT=iostat) nlin
      IF (iostat /= 0) THEN
        WRITE(lfnerr,"(/,' *** SR RDSTDH: Reading error record 1b')")
        CALL exitrc(2)
      ENDIF
      DO ilin=1,nlin
        READ(lfnloc,IOSTAT=iostat) line
        IF (iostat /= 0) THEN
          WRITE(lfnerr,"(/,' *** SR RDSTDH: Reading error record 1c',I6)") ilin
          CALL exitrc(2)
        ENDIF
        IF (line(1:7).EQ.'NUTSUB:') THEN
!          READ(line,"(8X,A16,1X,A16)") nutnam,subnam
          CALL chksys(1,line,irc)
        ENDIF
        IF (line(1:7).EQ.'GRAVIT:') THEN
          meanPol = ' '
          READ(line,"(48X,A8)") meanPol(1:8)
! IERS2010 conventions
          IF (meanPol(1:8) == 'IERS2010') THEN
            mPol = 3
! IERS2000/IERS2003 conventions (Version 5.1)
          ELSEIF (meanPol(1:8) == 'IERS2003') THEN
            mPol = 2
! IERS1996 conventions (Version 5.0)
          ELSE
            meanPol(1:2) = 'NO'
            mPol = 1
          ENDIF
! Set model key from first STD-file
! ---------------------------------
          IF (mFirst) THEN
            CALL setModKey(mod_orb_meaPol,meanPol,srName,mPol*1.D0)

! Check model key from second STD-file (possibly LEO), if consistent
! ------------------------------------------------------------------
          ELSE
            CALL chkModKey(1,mod_orb_meaPol,meanPol,mPol*1.D0,irChk)
          ENDIF
          mFirst = .FALSE.
        ENDIF
      ENDDO

      stdHead%ifrmt=ifmt
    ELSE
! Default for old format
      LINE='NUTSUB: IAU80            RAY'
      CALL chksys(1,line,irc)
    ENDIF
    stdHead%narc=narc

    ALLOCATE(stdHead%arc(narc),STAT=iostat)
    CALL alcerr(iostat,'stdHead%arc',(/narc/),'RDSTDH')

! Collect information for each arc
    DO iarc=1,narc
      READ(lfnloc,IOSTAT=iostat) nsat,nint,ndeg,svntmp(1:nsat), &
                                 stdHead%arc(iarc)%source
      IF (iostat /= 0) THEN
        WRITE(lfnerr,"(/,' *** SR RDSTDH: Reading error record 2, iarc',I6)")
        CALL exitrc(2)
      ENDIF
      IF (nsat > maxSat) THEN
        WRITE(lfnerr,"(/,' *** SR RDSTDH: More than maxSat satellites.', &
             & /,16X,'Increas internal declaration', &
             & /,16X,'maxSat                      :',I6, &
             & /,16X,'Number of satellites in File:',I6)") maxSat,nsat
        CALL exitrc(2)
      ENDIF

      stdHead%arc(iarc)%nsat = nsat
      stdHead%arc(iarc)%nint = nint
      stdHead%arc(iarc)%degr = ndeg

      ALLOCATE(stdHead%arc(iarc)%sat(nsat),STAT=iostat)
      CALL alcerr(iostat,'stdHead%arc%sat',(/nsat/),'RDSTDH')

      stdHead%arc(iarc)%sat(1:nsat)%svn = svntmp(1:nsat)

! Read interval boundaries
! ------------------------
      READ(lfnloc,IOSTAT=iostat) stdHead%arc(iarc)%tosc,tb1
      IF (iostat /= 0) THEN
        WRITE(lfnerr,"(/,' *** SR RDSTDH: Reading error record 3.', &
                                 & /,16X,'narc',I6,'  iarc',I6)") narc,iarc
        CALL exitrc(2)
      ENDIF

      IF (tb1==0.D0) THEN
        stdHead%iorsys=1
        method=2
      ELSEIF (tb1==2.D0) THEN
        stdHead%iorsys=2
        method=2
      ELSE
        stdHead%iorsys=1
        method=1
      ENDIF

      IF (method==2) THEN
        READ(lfnloc,IOSTAT=iostat)tb1
        IF (iostat /= 0) THEN
          WRITE(lfnerr,"(/,' *** SR RDSTDH: Reading error record 4.', &
                                   & /,16X,'narc',I6,'  iarc',I6)") narc,iarc
          CALL exitrc(2)
        ENDIF
        DO iint=2,nint+1
          READ(lfnloc,IOSTAT=iostat) tb2
          IF (iostat /= 0) THEN
            WRITE(lfnerr,"(/,' *** SR RDSTDH: Reading error record 5.', &
                                   & /,16X,'narc',I6,'  iarc',I6, &
                                   & /,16X,'nint',I6,'  iint',I6)") &
                                   narc,iarc,nint,iint
            CALL exitrc(2)
          ENDIF
        ENDDO
      ELSE
        WRITE(lfnerr,"(/,' *** SR RDSTDH: Old binary format no ', &
                                 & 'longer supported',/, &
                               & 16X,'requires BACKSPACE',/)")
        CALL exitrc(2)
      ENDIF

! Skip remaining arc information
! ------------------------------
      DO isat=1,nsat
        READ(lfnloc,IOSTAT=iostat) axis
        IF (iostat /= 0) THEN
          WRITE(lfnerr,"(/,' *** SR RDSTDH: Reading error record 6.', &
                                   & /,16X,'narc',I6,'  iarc',I6, &
                                   & /,16X,'nsat',I6,'  isat',I6)") &
                                   narc,iarc,nsat,isat
          CALL exitrc(2)
        ENDIF
      ENDDO
      DO iint=1,nint
        READ(lfnloc,IOSTAT=iostat) tlocal
        IF (iostat /= 0) THEN
          WRITE(lfnerr,"(/,' *** SR RDSTDH: Reading error record 7.', &
                                   & /,16X,'narc',I6,'  iarc',I6, &
                                   & /,16X,'nint',I6,'  iint',I6)") &
                                   narc,iarc,nint,iint
          CALL exitrc(2)
        ENDIF
        DO ideg=1,ndeg+1
          READ(lfnloc,IOSTAT=iostat) polyco
          IF (iostat /= 0) THEN
            WRITE(lfnerr,"(/,' *** SR RDSTDH: Reading error record 8.', &
                                   & /,16X,'narc',I6,'  iarc',I6, &
                                   & /,16X,'ndeg',I6,'  ideg',I6)") &
                                   narc,iarc,ndeg+1,ideg
            CALL exitrc(2)
          ENDIF
        ENDDO
      ENDDO

! Save arc boundaries
      stdHead%arc(iarc)%tbound(1) = tb1
      stdHead%arc(iarc)%tbound(2) = tb2
    ENDDO
    CLOSE(lfnloc)
  ENDIF

  RETURN
END SUBROUTINE rdstdh

END MODULE
