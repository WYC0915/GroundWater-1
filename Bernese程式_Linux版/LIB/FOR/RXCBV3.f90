MODULE s_RXCBV3
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rxcbv3(title,crDate)

! -------------------------------------------------------------------------
! Purpose:    Extracts the satellite clocks from a clock RINEX file
!             (Shortcut of program CCRNXC)
!
! Author:     R. Dach
!
! Created:    12-Mar-2004
! Last mod.:  11-May-2011
!
! Changes:    06-May-2007 RD: Stop with error if wrong clock RINEX file
!             11-May-2011 LP: Handle also Galileo data
!
! SR used:    gtfile2, opnfil, opnerr, rdcrxh, rdcrxr, mjdgps, exitrc
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_clkrnx, ONLY: t_clkhead,init_clkHead, &
                      t_clkrec,init_clkrec,undef
  USE s_gtfile2
  USE s_opnfil
  USE s_mjdgps
  USE s_opnerr
  USE s_rdcrxh
  USE s_rdcrxr
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)               :: title   ! title line
  CHARACTER(LEN=*)               :: crDate  ! Creation date

! output:


! Local Parameter
! ---------------

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER    :: srName = 'rxcbv3'


! Local Variables
! ---------------
  TYPE(t_clkHead)                :: clkHead
  TYPE(t_clkRec)                 :: clkRec

  CHARACTER(LEN=fileNameLength),               &
    DIMENSION(:,:), POINTER      :: clkFil

  REAL(r8b)                      :: GPSSec

  INTEGER(i4b)                   :: GPSWeek
  INTEGER(i4b)                   :: iFil
  INTEGER(i4b)                   :: nFil
  INTEGER(i4b)                   :: iEpo
  INTEGER(i4b)                   :: iSat
  INTEGER(i4b)                   :: iDummy
  INTEGER(i4b)                   :: ircSum
  INTEGER(i4b)                   :: irc


! Init some things
! ----------------
  CALL init_clkHead(clkHead)
  CALL init_clkRec(clkRec)
  NULLIFY(clkFil)
  ircSum = 0


! Get the filenames
! -----------------
  CALL gtfile2 ('RCLKINP',2,nFil,clkFil)

! Loop all input files
! --------------------
  DO iFil = 1,nFil

! Read the clock RINEX input file
! -------------------------------
    clkHead%TFirst=0D0

    CALL opnfil(lfnloc,clkFil(1,iFil),'OLD','FORMATTED','READONLY',' ',irc)
    CALL opnerr(lfnerr,lfnloc,irc,clkFil(1,iFil),srName)

    CALL rdcrxh(lfnloc,lfnerr,clkHead,irc)
    IF (irc == 0) THEN

      ! Remove the station clocks
      IF (clkHead%nSta > 0) THEN
        DO iSat = 1,clkHead%nSat
          clkHead%clkName(iSat) = clkHead%clkName(iSat+clkHead%nSta)
        ENDDO
        clkHead%nSta = 0
      ENDIF

      ! Read all data
      CALL rdcrxr(lfnloc,lfnerr,(/0d0,1d20/),clkHead,clkRec,irc)

    ENDIF
    CLOSE(lfnloc)

    ircSum=ircSum+irc
    IF (irc /= 0) CYCLE


! Write the Bernese satellite clock file
! --------------------------------------
    CALL opnfil (lfnloc,clkFil(2,iFil),'UNKNOWN','FORMATTED',' ',' ',irc)
    CALL opnerr(lfnerr,lfn002,irc,clkFil(2,iFil),srName)

    WRITE(lfnloc,'(A64,1X,A15,/,80(''-''),//,A,7X,A,/)')                &
               title,CrDate,                                            &
               'SAT WEEK   TOC #PAR     A0 (SEC)',                      &
               'A1 (SEC/SEC)    A2 (SEC/SEC**2)'

    DO iEpo=1,clkRec%nEpo

      CALL mjdgps(clkHead%TFirst+clkRec%Epoch(iEpo)/86400D0,GPSSec,GPSWeek)

      DO iSat=1,clkHead%nSat
        IF (clkRec%Clock(iSat,iEpo) == unDef) CYCLE

        IF (clkHead%ClkName(iSat)(1:1) == 'G') THEN
          WRITE(lfnloc,'(A,1X,I4,1X,F8.0,2X,I1,1X,10D17.9)')           &
                ' '//clkHead%ClkName(iSat)(2:3),GPSWeek,GPSSec,        &
                1,clkRec%Clock(iSat,iEpo)*1E-6

        ELSE IF (clkHead%ClkName(iSat)(1:1) == 'R') THEN
          WRITE(lfnloc,'(A,1X,I4,1X,F8.0,2X,I1,1X,10D17.9)')           &
                '1'//clkHead%ClkName(iSat)(2:3),GPSWeek,GPSSec,        &
                1,clkRec%Clock(iSat,iEpo)*1E-6

        ELSE IF (clkHead%ClkName(iSat)(1:1) == 'E') THEN
          WRITE(lfnloc,'(A,1X,I4,1X,F8.0,2X,I1,1X,10D17.9)')           &
                '2'//clkHead%ClkName(iSat)(2:3),GPSWeek,GPSSec,        &
                1,clkRec%Clock(iSat,iEpo)*1E-6
        ENDIF
      ENDDO
    ENDDO

    WRITE(lfnloc,'(/)')
    CLOSE(lfnloc)

! Deallocate the arrays
! ---------------------
    DEALLOCATE (clkHead%Comment,stat=iDummy)
    DEALLOCATE (clkHead%DatTyp,stat=iDummy)
    DEALLOCATE (clkHead%Ref,stat=iDummy)
    DEALLOCATE (clkHead%ClkName,stat=iDummy)
    DEALLOCATE (clkHead%StaCoord,stat=iDummy)

    DEALLOCATE (clkRec%clock,stat=iDummy)
    DEALLOCATE (clkRec%sigma,stat=iDummy)
  ENDDO ! Next file

! Stop with error if one of the clock RINEX files was wrong
! ---------------------------------------------------------
  IF (ircSum /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE rxcbv3

END MODULE
