MODULE s_CLKSTORE
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.0
! -------------------------------------------------------------------------

SUBROUTINE clkstore(neq)

! -------------------------------------------------------------------------
! Purpose:    Store clock results from ADDNEQ2
!
! Author:     R. Dach
!
! Created:    10-Jun-2009
! Last mod.:  16-Dec-2010
!
! Changes:    30-Nov-2010 MF: Add calls to init_clkHead & init_clkRec
!             16-Dec-2010 RD: t_neq is taken from D_NEQ instead of P_ADDNEQ
!             08-Jul-2013 RD: Add clkflg to clkrec (as defined in d_clkrnx)
!
! Copyright:  Astronomical Institute
!             University of Berne
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_time,   ONLY: t_timint
  USE m_global, ONLY: maxsys, g_strsys
  USE d_neq,    ONLY: t_neq
  USE d_const,  ONLY: C,filTitle
  USE d_clkRnx, ONLY: t_clkHead,t_clkRec, undef, copy_clkHead, &
                      init_clkHead, init_clkRec
  USE p_addneq, ONLY: clkHed,comstat

  USE f_ikf
  USE s_alcerr
  USE s_exitrc
  USE s_gtflna
  USE s_mjdgps
  USE s_opnerr
  USE s_opnfil
  USE s_wtcrxh
  USE s_wtcrxr
  USE s_gtsensor
  USE s_clrflg
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  TYPE(t_neq)                             :: neq    ! Main normal equation

! output:

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER             :: srName = 'clkstore'

! Local Variables
! ---------------
  TYPE(t_timint)                          :: clkEpo
  TYPE(t_clkHead)                         :: outHed
  TYPE(t_clkrec)                          :: clkRec

  CHARACTER(LEN=fileNameLength)           :: clkFil

  INTEGER(i4b)                            :: iPar
  INTEGER(i4b)                            :: iClk
  INTEGER(i4b)                            :: iSvn
  INTEGER(i4b)                            :: GPSWeek
  INTEGER(i4b)                            :: ii, jj
  INTEGER(i4b)                            :: irc, iac
  INTEGER(i4b), DIMENSION(0:maxsys)       :: clksys

  REAL(r8b)                               :: MJD0
  REAL(r8b)                               :: GPSsec

  LOGICAL                                 :: isOpen


! Initialize
! ----------
  CALL init_clkHead(outHed)
  CALL init_clkRec(clkRec)

! Write clock results into the clock RINEX file
! ---------------------------------------------
  clkSys = 0
  isOpen = .FALSE.
  MJD0 = 0d0
  CALL gtflna(0,'CLKRNX',clkFil,irc)
  IF (LEN_TRIM(clkFil) > 0 .AND. irc == 0) THEN


    ! Get first/last epoch
    clkEpo%t = (/ 0d0,1d20 /)
    DO iPar = 1,neq%misc%nPar
      IF (neq%par(iPar)%locq(1) /= 23 .AND. neq%par(iPar)%locq(1) /= 24) CYCLE
      IF (clkEpo%t(1) == 0d0 .OR. clkEpo%t(1) > neq%par(iPar)%time%mean) &
        clkEpo%t(1) = neq%par(iPar)%time%mean
      IF (clkEpo%t(2) == 1d20 .OR. clkEpo%t(2) < neq%par(iPar)%time%mean) &
        clkEpo%t(2) = neq%par(iPar)%time%mean
      IF (neq%par(iPar)%locq(1) == 23 .AND. &
          clkSys(neq%par(iPar)%locq(3)) < 3 ) &
        clkSys(neq%par(iPar)%locq(3)) = clkSys(neq%par(iPar)%locq(3)) + 1
      IF (neq%par(iPar)%locq(1) == 24 .AND. clkSys(0) < 3 ) &
        clkSys(0) = clkSys(0) + 2
    ENDDO

    ! Check for discrepancies between setting from
    ! receiver and satellite clocks regarding clkSys
    IF ( clkSys(0) == 2 ) THEN
      DO ii = 1,maxSys
        IF ( clkSys(ii) == 1 ) THEN
          clkSys(0) = 0
          EXIT
        ENDIF
      ENDDO
    ENDIF

    ! A parameter found: open file, write header, allocate data record
    IF (clkEpo%t(1) /= 0d0) THEN

      ClkHed%pgmnam = 'ADDNEQ2 V'//PGMVER//'     '
      ClkHed%pcvstr=''
      CALL gtsensor(pcvmod=ClkHed%pcvstr(1:10))

      DO ii = 0,maxSys
        IF (clksys(ii) == 0) CYCLE

        clkHed%tFirst = clkEpo%t(1)
        outHed = copy_clkHead(ClkHed)
        IF (ii > 0) THEN
          DEALLOCATE(outHed%comment,stat=iac)
          outHed%nComment = outHed%nComment + 1
          ALLOCATE(outHed%comment(outHed%nComment),stat=iac)
          CALL alcerr(iac,'outHed%comment',(/outHed%nComment/),srName)
          outHed%comment(1:clkHed%nComment)=clkHed%comment(1:clkHed%nComment)
          outHed%comment(outHed%nComment) = 'RECEIVER CLOCKS FOR ' // &
                    'EACH SATELLITE SYSTEM: ' // g_strsys(ii) // ' PART'
        ENDIF

        CALL opnfil(lfn001+ii,clkFil,'UNKNOWN','FORMATTED',' ',' ',irc)
        CALL opnerr(lfnerr,lfn001+ii,irc,clkFil,srName)
        CALL wtcrxh(lfn001+ii,lfnerr,outHed,irc)
        IF (irc /= 0) CALL exitrc(2)

        DEALLOCATE(outHed%comment   ,stat=iac)
        DEALLOCATE(outHed%dattyp    ,stat=iac)
        DEALLOCATE(outHed%ref(1)%clk,stat=iac)
        DEALLOCATE(outHed%ref       ,stat=iac)
        DEALLOCATE(outHed%clkname   ,stat=iac)
        DEALLOCATE(outHed%stacoord  ,stat=iac)


        isOpen = .TRUE.
      ENDDO

      ClkRec%nEpo = 1

      ALLOCATE(clkRec%epoch(1),stat=irc)
      CALL alcerr(irc,'clkRec%epoch',(/1/),srName)

      ALLOCATE(clkRec%clock(clkHed%nSta+clkHed%nSat,1),stat=irc)
      CALL alcerr(irc,'clkRec%clock',(/clkHed%nSta+clkHed%nSat,1/),srName)
      clkRec%clock = undef

      ALLOCATE(clkRec%sigma(clkHed%nSta+clkHed%nSat,1),stat=irc)
      CALL alcerr(irc,'clkRec%sigma',(/clkHed%nSta+clkHed%nSat,1/),srName)
      clkRec%sigma = undef

      ALLOCATE(clkRec%clkflg(clkHed%nSta+clkHed%nSat,1),stat=irc)
      CALL alcerr(irc,'clkRec%clkflg',(/clkHed%nSta+clkHed%nSat,1/),srName)
    ENDIF


    ! Write clocks for all epochs
    MJD0 = clkEpo%t(1)
    DO WHILE (MJD0 /= 0d0 .AND. MJD0 <= clkEpo%t(2))

      DO ii = 0,maxsys
        IF (clksys(ii) == 0) CYCLE

        clkEpo%t(1)  = 1d20
        clkRec%clock = undef
        DO iClk = 1,clkHed%nSta+clkHed%nSat
          DO jj = 0,7
            CALL clrflg(clkRec%clkflg(iClk,1),jj)
          ENDDO
        ENDDO

        DO iPar = 1,neq%misc%nPar

          IF (neq%par(iPar)%locq(1) /= 23 .AND. neq%par(iPar)%locq(1) /= 24) CYCLE
          IF (neq%par(iPar)%locq(1) == 23 .AND. neq%par(iPar)%locq(3) /= ii) CYCLE

          ! Singular clock parameters
          IF (neq%aNor(ikf(iPar,iPar))==0.d0 .AND. &
              neq%bNor(iPar)==0.d0 .AND. neq%xxx(iPar)==0.d0) CYCLE

          ! Few observations
          IF (neq%par(iPar)%locq(6) <= 2)  CYCLE

          ! Sat clock with arc split
          IF (neq%par(iPar)%locq(1) == 24 .AND. neq%par(iPar)%locq(7) == 2)  CYCLE

          ! Find next epoch to be processed
          IF (neq%par(iPar)%time%mean < clkEpo%t(1) .AND. &
              neq%par(iPar)%time%mean > MJD0) &
            clkEpo%t(1) = neq%par(iPar)%time%mean

          ! Find all clocks for the current epoch
          IF (MJD0 == neq%par(iPar)%time%mean) THEN

            DO iClk = 1,clkHed%nSta+clkHed%nSat

              IF (clkHed%clkName(iClk) == neq%par(iPar)%name) THEN
                clkRec%clock(iClk,1) = (neq%par(iPar)%x0 - neq%xxx(iPar)) / C*1d6
                clkRec%sigma(iClk,1) = comstat%rms * &
                                       SQRT(neq%aNor(ikf(iPar,iPar))) / C*1d6
                EXIT
              ENDIF
            ENDDO

          ENDIF

        ENDDO ! next parameter


        ! Write the clocks of the epoch
        clkRec%epoch(1) = (MJD0 - clkHed%tFirst) *86400d0
        CALL wtcrxr(lfn001+ii,lfnerr,clkHed,clkRec,irc)
        IF (irc /= 0) CALL exitrc(2)
      ENDDO

      ! Next epoch to be processed
      MJD0 = clkEpo%t(1)

    ENDDO

    ! Close the file, deallocate
    IF (isOpen) THEN

      CLOSE(lfn001)

      DEALLOCATE(clkRec%epoch,stat=irc)
      DEALLOCATE(clkRec%clock,stat=irc)
      DEALLOCATE(clkRec%sigma,stat=irc)
      DEALLOCATE(clkRec%clkflg,stat=irc)
    ENDIF
  ENDIF

! Write satellite clocks into the Bernese clock file
! --------------------------------------------------
  isOpen = .FALSE.
  CALL gtflna(0,'CLKSAV',clkFil,irc)
  IF (LEN_TRIM(clkFil) > 0 .AND. irc == 0) THEN

    DO iPar = 1,neq%misc%nPar

      IF (neq%par(iPar)%locq(1) /= 24) CYCLE

      ! Singular clock parameters
      IF (neq%aNor(ikf(iPar,iPar))==0.d0 .AND. &
          neq%bNor(iPar)==0.d0 .AND. neq%xxx(iPar)==0.d0) CYCLE

      ! Few observations
      IF (neq%par(iPar)%locq(6) <= 2)  CYCLE

      ! Sat clock with arc split
      IF (neq%par(iPar)%locq(7) == 2)  CYCLE

      ! First epoch: open file, write header
      IF (.NOT. isOpen) THEN

        CALL opnfil(lfnloc,clkFil,'UNKNOWN','FORMATTED',' ',' ',irc)
        CALL opnerr(lfnerr,lfnloc,irc,clkFil,srName)

        isOpen = .TRUE.

        WRITE(lfnloc,'(A80,/,80(''-''),//,A,7X,A,/)')                       &
               filTitle,                                                    &
               'SAT WEEK   TOC #PAR     A0 (SEC)',                          &
               'A1 (SEC/SEC)    A2 (SEC/SEC**2)'
      ENDIF

      ! Put the result into the File
      CALL mjdgps(neq%par(iPar)%time%mean,GPSSec,GPSWeek)

      iSvn = neq%par(iPar)%locq(3)

      WRITE(lfnloc,'(I3,1X,I4,1X,F8.0,2X,I1,1X,10D17.9)')           &
                   iSvn,GPSWeek,GPSSec,1,        &
                   (neq%par(iPar)%x0 - neq%xxx(iPar)) / C

    ENDDO

    ! Close file
    IF (isOpen) CLOSE(lfnloc)

  ENDIF


  RETURN
END SUBROUTINE clkstore

END MODULE s_CLKSTORE
