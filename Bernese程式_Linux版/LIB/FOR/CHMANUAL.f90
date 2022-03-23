MODULE s_CHMANUAL
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE chmanual(opt, titnew, ChgChr, ChgInt, ChReal, obsHead, irCode)

! -------------------------------------------------------------------------
! Purpose:    Manual changes from the input file for CHGHED
!
! Parameters:
!    in:      opt    : input options for CHGHED                t_chghed(:)
!             TITNEW : new title line                              ch*53
!             ChgChr : list of chr. entries to change        t_chghed_chr(:)
!             ChgInt : list of int. entries to change        t_chghed_int(:)
!             ChReal : list of real entries to change        t_chghed_rea(:)
!    in/out:  obsHead: Header of the observation file          t_obsHead
!    out:     irCode : return code                                 i4b
!
!
! Author:     R. Dach
!
! Created:    12-Dec-2000
! Last mod.:  16-Jun-2003
!
! Changes:    21-Dec-2001  HU: Use m_bern, other modules with ONLY
!             22-Jul-2002  HB: Use modified t_obsHead
!             23-Jun-2003  HU: Copy campaign
!
! SR used:    exitrc
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE p_chghed, ONLY: t_chghed,t_chghed_chr,t_chghed_int,t_chghed_real
  USE d_gpsobs, ONLY: t_obshead

  USE s_exitrc
  IMPLICIT NONE
!
! Global Parameters
! -----------------
  TYPE(t_chghed), DIMENSION(:) :: opt       ! Input parameter
  CHARACTER(LEN=53)            :: TITNEW    ! new title
  TYPE(t_chghed_chr), DIMENSION(:), POINTER :: chgChr  ! Change of chr-var.
  TYPE(t_chghed_int), DIMENSION(:), POINTER :: chgInt  ! Change of int-var.
  TYPE(t_chghed_real),DIMENSION(:), POINTER :: chReal  ! Change of real-var.
  TYPE(t_obshead)              :: obsHead   ! Header of the obs. file
  INTEGER(i4b)                 :: irCode    ! return Code
!
! Local Variables
! ---------------
  INTEGER(i4b)       :: iOpt     ! counter for variables to change
  INTEGER(i4b)       :: iSta     ! Counter for station

!
! MAKE SOME CHANGES IN THE HEADER FILE
! ------------------------------------
  iOptLoop: DO iOpt=1,SIZE(opt)
!
! WRONG OPTION
    IF (opt(iOpt)%iOptio < 0 .OR. opt(iOpt)%iOptio > 3) THEN
      WRITE(LFNERR,'(/,A,2(/,16X,A,I3),/)')                        &
            ' *** PG CHGHED: INVALID CHANGE OPTION',               &
                            'CHANGE OPTION NUMBER:',iOpt,          &
                            'CHANGE OPTION VALUE :',opt(iopt)%iOptio
      CALL EXITRC(2)
    ENDIF
!
! NO CHANGES
    IF (opt(iOpt)%iOptio == 0) CYCLE iOptLoop
!
! CHANGE TITLE
! ------------
    IF (iOpt == 1) THEN
      obsHead%title=TITNEW
      WRITE(LFNPRT,'(A,A20,A,A20,A,A20)') &
           ' ',opt(iOpt)%text,'     : ',  &
           ' ',' --> ',trim(obsHead%title(1:20))
!
! CAMPAIGN NAME
    ELSE IF (iOpt == 2) THEN
      IF (chgChr(opt(iopt)%idxchr)%CHROLD == obsHead%campgn .OR.  &
          opt(iOpt)%optany == 1)  THEN
        WRITE(lfnprt,'(A,A20,A,A20,A,A20)') &
           ' ',opt(iOpt)%text,'     : ',    &
           obsHead%campgn,' --> ',          &
           chgChr(opt(iopt)%idxchr)%CHRNEW
        obshead%campgn=chgChr(opt(iopt)%idxchr)%CHRNEW
      END IF
!
! STATION NAME(S)
    ELSE IF (iOpt == 3) THEN
      IF (opt(iOpt)%iOptio == 3   .AND. &
          opt(iOpt)%optany == 1 ) THEN
         WRITE(lfnerr,'(/,A,2(/,16X,A,I3),/)')               &
               ' *** PG CHGHED: INVALID CHANGE OPTION',      &
                       'CHANGE OPTION NUMBER:',iOpt,         &
                       'INVALID CHANGE OPTION VALUE:',opt(iOpt)%iOptio
        CALL exitrc(2)
      END IF
      DO iSta=1,ObsHead%nDiff+1
        IF (opt(iOpt)%iOptio == iSta .OR. opt(iopt)%iOptio == 3) THEN
          IF (chgChr(opt(iopt)%idxchr)%CHROLD == obshead%sta(iSta)%stanam .OR. &
              opt(iOpt)%optany == 1) THEN
            WRITE(lfnprt,'(A,A20,A,I1,A,A20,A,A20)')      &
                  ' ',opt(iOpt)%text,' (',iSta,') : ',    &
                  obsHead%sta(iSta)%stanam,' --> ',           &
                  chgChr(opt(iopt)%idxchr)%CHRNEW
            obshead%sta(iSta)%stanam=chgChr(opt(iopt)%idxchr)%CHRNEW
          END IF
        END IF
      ENDDO
!
! RECEIVER TYPE(S)
    ELSE IF (iOpt == 4) THEN
      DO iSta=1,ObsHead%nDiff+1
        IF (opt(iOpt)%iOptio == iSta .OR. opt(iopt)%iOptio == 3) THEN
          IF (chgChr(opt(iopt)%idxchr)%CHROLD == obshead%sta(iSta)%rectyp .OR. &
              opt(iOpt)%optany == 1) THEN
            WRITE(lfnprt,'(A,A20,A,I1,A,A20,A,A20)')      &
                  ' ',opt(iOpt)%text,' (',iSta,') : ',    &
                  obsHead%sta(iSta)%rectyp,' --> ',           &
                  chgChr(opt(iopt)%idxchr)%CHRNEW
            obshead%sta(iSta)%rectyp=chgChr(opt(iopt)%idxchr)%CHRNEW
          END IF
        END IF
      ENDDO
!
! ANTENNA TYPE(S)
    ELSE IF (iOpt == 5) THEN
      DO iSta=1,ObsHead%nDiff+1
        IF (opt(iOpt)%iOptio == iSta .OR. opt(iopt)%iOptio == 3) THEN
          IF (chgChr(opt(iopt)%idxchr)%CHROLD == obshead%sta(iSta)%anttyp .OR. &
              opt(iOpt)%optany == 1) THEN
            WRITE(lfnprt,'(A,A20,A,I1,A,A20,A,A20)')      &
                  ' ',opt(iOpt)%text,' (',iSta,') : ',    &
                  obsHead%sta(iSta)%anttyp,' --> ',           &
                  chgChr(opt(iopt)%idxchr)%CHRNEW
            obshead%sta(iSta)%anttyp=chgChr(opt(iopt)%idxchr)%CHRNEW
          END IF
        END IF
      ENDDO
!
! OPERATOR NAME
    ELSE IF (iOpt == 6) THEN
      IF (opt(iOpt)%iOptio == 3   .AND. &
          opt(iOpt)%optany == 1 ) THEN
         WRITE(lfnerr,'(/,A,2(/,16X,A,I3),/)')               &
               ' *** PG CHGHED: INVALID CHANGE OPTION',      &
                       'CHANGE OPTION NUMBER:',iOpt,         &
                       'INVALID CHANGE OPTION VALUE:',opt(iOpt)%iOptio
        CALL exitrc(2)
      END IF
      DO iSta=1,ObsHead%nDiff+1
        IF (opt(iOpt)%iOptio == iSta .OR. opt(iopt)%iOptio == 3) THEN
          IF (chgChr(opt(iopt)%idxchr)%CHROLD == obshead%sta(iSta)%oprnam .OR. &
              opt(iOpt)%optany == 1) THEN
            WRITE(lfnprt,'(A,A20,A,I1,A,A20,A,A20)')      &
                  ' ',opt(iOpt)%text,' (',iSta,') : ',    &
                  obsHead%sta(iSta)%oprnam,' --> ',           &
                  chgChr(opt(iopt)%idxchr)%CHRNEW
            obshead%sta(iSta)%oprnam=chgChr(opt(iopt)%idxchr)%CHRNEW
          END IF
        END IF
      ENDDO
!
! SESSION IDENTIFICATION
    ELSE IF(iOpt == 7) THEN
      IF (chgChr(opt(iopt)%idxchr)%CHROLD == obshead%csess(1) .OR. &
          opt(iOpt)%optany == 1) THEN
        WRITE(lfnprt,'(A,A20,A,A20,A,A20)')           &
              ' ',opt(iOpt)%text,'     : ',           &
              obsHead%csess(1),' --> ',               &
              chgChr(opt(iopt)%idxchr)%CHRNEW
        obshead%csess(1)=chgChr(opt(iopt)%idxchr)%CHRNEW(1:4)
      END IF
!
! SESSION FILE IDENTIFICATION
    ELSE IF(iOpt == 8) THEN
      IF (chgChr(opt(iopt)%idxchr)%CHROLD == obshead%csess(2)(1:1) .OR. &
          opt(iOpt)%optany == 1) THEN
        WRITE(lfnprt,'(A,A20,A,A20,A,A20)')          &
              ' ',opt(iOpt)%text,'     : ',          &
              obsHead%csess(2)(1:1),' --> ',         &
              chgChr(opt(iopt)%idxchr)%CHRNEW
        obshead%csess(2)(1:1)=chgChr(opt(iopt)%idxchr)%CHRNEW(1:1)
      END IF
!
! RECEIVER NUMBER(s)
    ELSE IF (iOpt == 9) THEN
      IF (opt(iOpt)%iOptio == 3   .AND. &
          opt(iOpt)%optany == 1 ) THEN
         WRITE(lfnerr,'(/,A,2(/,16X,A,I3),/)')               &
               ' *** PG CHGHED: INVALID CHANGE OPTION',      &
                       'CHANGE OPTION NUMBER:',iOpt,         &
                       'INVALID CHANGE OPTION VALUE:',opt(iOpt)%iOptio
        CALL exitrc(2)
      END IF
      DO iSta=1,ObsHead%nDiff+1
        IF (opt(iOpt)%iOptio == iSta .OR. opt(iopt)%iOptio == 3) THEN
          IF (chgInt(opt(iopt)%idxint)%INTOLD == obshead%sta(iSta)%irunit .OR. &
              opt(iOpt)%optany == 1) THEN
            WRITE(lfnprt,'(A,A20,A,I1,A,I12,8X,A,I12)')   &
                  ' ',opt(iOpt)%text,' (',iSta,') : ',    &
                  obsHead%sta(iSta)%irunit,' --> ',           &
                  chgInt(opt(iopt)%idxint)%INTNEW
            obshead%sta(iSta)%irunit=chgInt(opt(iopt)%idxint)%INTNEW
          END IF
        END IF
      ENDDO
!
! ANTENNA NUMBER(s)
    ELSE IF (iOpt == 10) THEN
      IF (opt(iOpt)%iOptio == 3   .AND. &
          opt(iOpt)%optany == 1 ) THEN
         WRITE(lfnerr,'(/,A,2(/,16X,A,I3),/)')               &
               ' *** PG CHGHED: INVALID CHANGE OPTION',      &
                       'CHANGE OPTION NUMBER:',iOpt,         &
                       'INVALID CHANGE OPTION VALUE:',opt(iOpt)%iOptio
        CALL exitrc(2)
      END IF
      DO iSta=1,ObsHead%nDiff+1
        IF (opt(iOpt)%iOptio == iSta .OR. opt(iopt)%iOptio == 3) THEN
          IF (chgInt(opt(iopt)%idxint)%INTOLD == obshead%sta(iSta)%ianten .OR. &
              opt(iOpt)%optany == 1) THEN
            WRITE(lfnprt,'(A,A20,A,I1,A,I12,8X,A,I12)')   &
                  ' ',opt(iOpt)%text,' (',iSta,') : ',    &
                  obsHead%sta(iSta)%ianten,' --> ',           &
                  chgInt(opt(iopt)%idxint)%INTNEW
            obshead%sta(iSta)%ianten=chgInt(opt(iopt)%idxint)%INTNEW
          END IF
        END IF
      ENDDO
!
! REMARK NUMBER,
    ELSE IF (iOpt == 11) THEN
      IF (chgInt(opt(iopt)%idxint)%INTOLD == obshead%irmark .OR. &
          opt(iOpt)%optany == 1) THEN
        WRITE(lfnprt,'(A,A20,A,I12,8X,A,I12)')        &
              ' ',opt(iOpt)%text,'     : ',           &
              obsHead%irmark,' --> ',                 &
              chgInt(opt(iopt)%idxint)%INTNEW
        obshead%irmark=chgInt(opt(iopt)%idxint)%INTNEW
      END IF
!
! OBSERVATION INTERVAL
    ELSE IF (iOpt == 12) THEN
      IF (chgInt(opt(iopt)%idxint)%INTOLD == obshead%ideltt .OR. &
          opt(iOpt)%optany == 1) THEN
        WRITE(lfnprt,'(A,A20,A,I12,8X,A,I12)')        &
              ' ',opt(iOpt)%text,'     : ',           &
              obsHead%ideltt,' --> ',                 &
              chgInt(opt(iopt)%idxint)%INTNEW
        obshead%ideltt=chgInt(opt(iopt)%idxint)%INTNEW
      END IF
!
! POSITIONING ECCENTRICITY (NORTH, East, Up)
    ELSE IF (iOpt == 13 .OR. iOpt == 14 .OR. iOpt == 15) THEN
      DO iSta=1,ObsHead%nDiff+1
        IF (opt(iOpt)%iOptio == iSta .OR. opt(iopt)%iOptio == 3) THEN
          IF (chReal(opt(iopt)%idxrea)%REAOLD == obshead%sta(iSta)%posecc(iOpt-12) .OR. &
              opt(iOpt)%optany == 1) THEN
            WRITE(lfnprt,'(A,A20,A,I1,A,F17.4,3X,A,F17.4)')  &
                  ' ',opt(iOpt)%text,' (',iSta,') : ',       &
                  obsHead%sta(iSta)%posecc(iOpt-12),' --> ',      &
                  chReal(opt(iopt)%idxrea)%REANEW
            obshead%sta(iSta)%posecc(iOpt-12)=chReal(opt(iopt)%idxrea)%REANEW
          END IF
        END IF
      ENDDO
!
! UNKNOWN VARIABLE TYPE TO CHANGE
    ELSE
      WRITE(lfnerr,'(/,A,2(/,16X,A,I3),/)')               &
            ' *** PG CHGHED: INVALID CHANGE OPTION',      &
                    'CHANGE OPTION NUMBER:',iOpt,         &
                    'MAX. VALID OPTION VALUE:',SIZE(opt)
      CALL exitrc(2)
    ENDIF
  ENDDO iOptLoop
! -----------
  CLOSE(UNIT=LFNRES)
!
! Set the return code
! -------------------
  irCode=0
!
  RETURN
  END SUBROUTINE chmanual

END MODULE
