MODULE s_RDR3OR
CONTAINS

! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

SUBROUTINE rdr3or(lfnobs)

! ------------------------------------------------------------------------------
! Purpose:    This subroutine reads the header of a RINEX 3.0 observation file
!
! Parameters:
!         in: filnam:  RINEX file name
!             lfnobs:  File number
!             rxohead: RINEX header
!        out: rxoobs:  One observation record in structure t_rxoobs
!
! SR used:    djul, exitrc, opnerr, opnfil, alcerr
!
! Remarks:
!
! Author:     A. Gäde
!
! Created:    28-Aug-2007
!
! Changes:    28-Aug-2007 AG: Deallocation of pointer in obsrec,
!                             Warning for end of file statement excluded
!             28-Nov-2007 RD: Corrected format statments
!             24-Apr-2012 LP: Generalization for all sat-systems using maxsys
!             31-May-2012 LP: Use prn2prn
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ------------------------------------------------------------------------------

  USE m_bern
  USE m_global, ONLY : g_rnxsys,maxsys
  USE d_rinex3, ONLY : rxoobs, rxohead, init_rxoobs
  USE s_rdr3oh
  USE s_alcerr
  USE s_stripdir
  USE f_djul
  USE s_exitrc
  USE f_prn2prn
  IMPLICIT NONE
!
! Dummy list
! ----------
  INTEGER(i4b)                          :: lfnobs
  CHARACTER(LEN=60)                     :: filnam
!
! Local Variables
! ---------------
  CHARACTER(LEN=longlineLength)         :: line
  CHARACTER(LEN=25)                     :: frmt
  CHARACTER(LEN=1)                      :: satsys
  INTEGER(i4b)                          :: year
  INTEGER(i4b)                          :: month
  INTEGER(i4b)                          :: day
  INTEGER(i4b)                          :: hour
  INTEGER(i4b)                          :: minute
  INTEGER(i4b)                          :: ilin
  INTEGER(i4b)                          :: ii,iac,prnhelp
  INTEGER(i4b)                          :: isys
  REAL(r8b)                             :: second
  REAL(r8b)                             :: rday
!
! Initialize RINEX observation record
! -----------------------------------
  INQUIRE(unit = LFNOBS, name = filnam)
  CALL stripdir(filnam)
  CALL init_rxoobs(rxoobs)
  frmt = '(A1,I2.2,##(F14.3,I1,I1))'
!
! Find record identifier ">"
! --------------------------
  DO
    READ (lfnobs,'(A)', iostat = iac) line

    IF (iac > 0) THEN
      WRITE(lfnerr,"(' *** SR RDR3OR: Error reading RINEX file.', &
                        & /,16X,'Filename: ',A,/)")TRIM(adjustl(filnam))
      CALL exitrc(2)
    ELSEIF (iac < 0) THEN
!      WRITE(lfnerr,"(' ### SR RDR3OR: End of RINEX file without END', &
!                        &       ' OF FILE statement.', &
!                        & /,16X,'Filename: ',A,/)")TRIM(adjustl(filnam))
      rxohead%end = 9
      RETURN
    ELSEIF (line(1:1).EQ.'>') THEN
!
! Read observation record
! -----------------------
      READ(line,"(2X,I4,4(1X,I2.2),F11.7,2X,I1,I3,6X,F15.12)",iostat=iac) &
               year,month,day,hour,minute,second,          &
               rxoobs%flag,rxoobs%nusat,rxoobs%recloff
      IF (iac /= 0) THEN
        WRITE(lfnerr,"(' ### SR RDR3OR: Error decoding the first line of ', &
             &       'observation record:'/,/,A,/, &
             & /,16X,'Observation record skipped!', &
             & /,16X,'Filename: ',A,/)")line,TRIM(adjustl(filnam))
        CYCLE
      ELSE
        IF (year /= 0) THEN
          rday = day + hour / 24d0 + minute / 1440d0 + second / 86400d0
          rxoobs%epoch = djul(year,month,rday)
        ENDIF
        IF (rxoobs%nusat /= 0) THEN
          IF (ASSOCIATED(rxoobs%obsrec)) THEN
            DO ilin=1,SIZE(rxoobs%obsrec)
              IF (ASSOCIATED(rxoobs%obsrec(ilin)%obs)) DEALLOCATE(rxoobs%obsrec(ilin)%obs)
              IF (ASSOCIATED(rxoobs%obsrec(ilin)%lli)) DEALLOCATE(rxoobs%obsrec(ilin)%lli)
              IF (ASSOCIATED(rxoobs%obsrec(ilin)%streng)) DEALLOCATE(rxoobs%obsrec(ilin)%streng)
            ENDDO
            DEALLOCATE(rxoobs%obsrec)
          ENDIF
          ALLOCATE(rxoobs%obsrec(rxoobs%nusat),stat=iac)
          CALL alcerr(iac, 'rxoobs%obsrec', (/rxoobs%nusat/), 'rdr3or')
        ENDIF

        IF (rxoobs%flag /= 3 .AND. rxoobs%flag /= 4) THEN
          DO ilin = 1, rxoobs%nusat
            line = ''
            READ (lfnobs,'(A)', iostat = iac) line
! Error at reading record line
! ----------------------------
            IF (iac > 0) THEN
              WRITE(lfnerr,"(' *** SR RDR3OR: Error reading RINEX observation record line.', &
                                     & /,16X,'Further reading stopped!', &
                                     & /,16X,'Filename: ',A,/)")TRIM(ADJUSTL(filnam))
              rxohead%end = 4
              RETURN
            ELSEIF (iac < 0) THEN
              WRITE(lfnerr,"(' ### SR RDR3OR: End of RINEX file without END', &
                        &       ' OF FILE statement.', &
                        & /,16X,'Observation record skipped!', &
                        & /,16X,'Filename: ',A,/)")TRIM(adjustl(filnam))
              rxohead%end = 3
              RETURN
! Event flag = 0, 1 (normal case)
! -----------------------------------------------------------
            ELSEIF (rxoobs%flag < 2) THEN
              READ(line,"(A1)",iostat=iac)satsys
              isys = 999
              DO ii=0,(maxsys-1)
                IF (satsys == g_rnxsys(ii)) THEN
                  isys = ii
                  EXIT
                ENDIF
              ENDDO
              IF (isys == 999) THEN
                WRITE(lfnerr,"(' ### SR RDR3OR: Unknown satellite system string ', &
                       &       'in following line:'/,/,A,/, &
                       & /,16X,'Observation line skipped!', &
                       & /,16X,'Filename: ',A,/)")line,TRIM(ADJUSTL(filnam))
                rxoobs%obsrec(ilin)%satcod = ''
                rxoobs%obsrec(ilin)%prn = 0
              ELSE
                ALLOCATE(rxoobs%obsrec(ilin)%obs(rxohead%otyp(isys)%obsnum),stat=iac)
                CALL alcerr(iac, 'rxoobs%obsrec(isys)%obs', (/rxohead%otyp(isys)%obsnum/), 'rdr3or')
                ALLOCATE(rxoobs%obsrec(ilin)%lli(rxohead%otyp(isys)%obsnum),stat=iac)
                CALL alcerr(iac, 'rxoobs%obsrec(isys)%lli', (/rxohead%otyp(isys)%obsnum/), 'rdr3or')
                ALLOCATE(rxoobs%obsrec(ilin)%streng(rxohead%otyp(isys)%obsnum),stat=iac)
                CALL alcerr(iac, 'rxoobs%obsrec(isys)%streng', (/rxohead%otyp(isys)%obsnum/), 'rdr3or')

                WRITE(frmt(10:11),'(I2)')rxohead%otyp(isys)%obsnum
                READ(line, frmt, iostat = iac) rxoobs%obsrec(ilin)%satcod,     &
                                               rxoobs%obsrec(ilin)%prn,        &
                     (rxoobs%obsrec(ilin)%obs(ii),rxoobs%obsrec(ilin)%lli(ii), &
                     rxoobs%obsrec(ilin)%streng(ii), ii=1,rxohead%otyp(isys)%obsnum)
                IF (iac /= 0) THEN
                  WRITE(lfnerr,"(' ### SR RDR3OR: Error decoding following line in ',&
                       &       'observation record:'/,/,A,/, &
                       & /,16X,'Observation line skipped!', &
                       & /,16X,'Filename: ',A,/)")line,TRIM(ADJUSTL(filnam))
                  rxoobs%obsrec(ilin)%satcod = ''
                  rxoobs%obsrec(ilin)%prn = 0
                ENDIF
                IF (rxoobs%obsrec(ilin)%prn /= 0) THEN
                  prnhelp = rxoobs%obsrec(ilin)%prn + isys * 100
                  rxoobs%obsrec(ilin)%prn = prn2prn(prnhelp,rxoobs%epoch)
! Apply factor
                  DO ii=1,rxohead%otyp(isys)%obsnum
                    rxoobs%obsrec(ilin)%obs(ii) = rxoobs%obsrec(ilin)%obs(ii) / rxohead%otyp(isys)%factor(ii)
                  ENDDO
                ENDIF
              ENDIF
! Event flag = 2 (start moving antenna)
! -------------------------------------
            ELSEIF (rxoobs%flag == 2) THEN
              IF (ilin == 1) WRITE(lfnerr,"(' ### SR RDR3OR: Comment lines for ', &
                   &       'start of moving antenna skipped!',         &
                   & /,16X,'Filename: ',A,/)")TRIM(ADJUSTL(filnam))
              CYCLE
! Event flag = 5 (external event)
! -------------------------------
            ELSEIF (rxoobs%flag == 5) THEN
              IF (ilin == 1) WRITE(lfnerr,"(' ### SR RDR3OR: Comment lines for ', &
                   &       'external event skipped!',                  &
                   & /,16X,'Filename: ',A,/)")TRIM(ADJUSTL(filnam))
              CYCLE
! Event flag = 6 (Cycle slip)
! -------------------------------
            ELSEIF (rxoobs%flag == 6) THEN
              IF (ilin == 1) WRITE(lfnerr,"(' ### SR RDR3OR: Data record of ', &
                   &        'cycle slip skipped!',                   &
                   &  /,16X,'Filename: ',A,/)")TRIM(ADJUSTL(filnam))
              CYCLE
            ENDIF
          ENDDO
! Event flag = 3 (new site occupation)
! ------------------------------------
!!!  ELSEIF (rxoobs%flag == 3) THEN

! Event flag = 4 (header information)
! -----------------------------------
!!!  ELSEIF (rxoobs%flag == 4) THEN
        ENDIF
      ENDIF
      EXIT
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE rdr3or

END MODULE
