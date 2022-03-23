MODULE s_RDSATFIL
CONTAINS

! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

SUBROUTINE rdsatfil(filename, satfil)

! ------------------------------------------------------------------------------
! Purpose:    This subroutine reads the satellite info file SATELL
!             ("SATELLIT.") and give back the requests in the structure
!             "satfil"
!
! Parameters:
!         in: filename: Satellite info file name                          m_bern
!        out: satfil  : Informations from satellite file (SATELL)       t_satfil
!
! SR used:    exitrc, opnerr, opnfil, st2tim
!
! Remarks:
!
! Author:     D. Svehla
!
! Created:    15-Mar-2001
! Last Mod.:  21-Sep-2010
!
! Changes:    13-Jun-2001 HB: add USE m_bern and ONLY : t_satfil,
!                             add SR alcerr
!             10-Dec-2001 DS: Write SVN/sensor name when vectors are not
!                             orthog.
!             04-Jul-2002 HU: Orthonorm. test for antenna vect.
!                             0.0002 -> 0.0005
!             16-May-2003 HU: Deallocate array
!             21-May-2003 RD: Make the deallocation safe
!             20-Jul-2005 HB: Nullify satfil%satellite and satfil%sensor
!             27-Jul-2005 AG: reading of SVN number and ANTEX sensor name added
!             05-Aug-2005 AG: reading of sensor type, number and signal list
!                             added
!                             Format number implemented
!             10-Jul-2006 AG: reading of pcvmod
!             17-Oct-2006 PL: reading of rprmod
!             11-Jan-2007 AG: Exit Loop if "PART 4: REMARKS" read
!             17-Jul-2008 DT: Different scaling for RPRmodel=3 (Lageos)
!             21-May-2010 MF: Deallocate satfil%sensor%sigli
!             21-May-2010 MF: Move nullify statement for satfil%sensor%sigli
!             19-Jul-2010 SL: tab characters removed
!             21-Sep-2010 RD: ST2TIM can be used as a module now
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ------------------------------------------------------------------------------

  USE m_bern
  USE d_satfil, ONLY : t_satfil, satfilvers
  USE m_global, ONLY : g_svnsys
  USE s_opnfil
  USE s_alcerr
  USE s_opnerr
  USE s_st2tim
  USE s_exitrc
  USE s_splarg
  IMPLICIT NONE
!
! Dummy list
! ----------
  CHARACTER(LEN=fileNameLength):: filename
  TYPE(t_satfil)               :: satfil

!
! Local Variables
! ---------------
  CHARACTER(LEN=512)         :: line
  CHARACTER(LEN=40)          :: datstr
  CHARACTER(LEN=255)         :: sigli
  CHARACTER(LEN=4)           :: remind1,remind2


  INTEGER(i4b), PARAMETER    :: numtyp = 3  ! Number of types
                                            !   t_satellite
                                            !   t_sensor
                                            !   t_rprcoe
  INTEGER(i4b)               :: nlin
  INTEGER(i4b)               :: icrx,icrx1,found
  INTEGER(i4b)               :: ii, kk
  INTEGER(i4b)               :: ios
  INTEGER(i4b)               :: iac
  INTEGER(i4b)               :: nrprmod
  REAL(r8b)                  :: lengthb, lengtha, orth
  REAL(r8b), DIMENSION(3)    :: bore   , azim

  TYPE t_satfilmap
    INTEGER(i4b)             :: pos1
    INTEGER(i4b)             :: pos2
  END TYPE t_satfilmap


  TYPE(t_satfilmap), DIMENSION(numtyp) :: map

! Nullify pointer
! ---------------
  NULLIFY(satfil%satellite)
  NULLIFY(satfil%sensor)
!
! Read the entire file SATELL
! ---------------------------
  CALL opnfil(lfnloc,filename,'OLD','FORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,filename,'rdsatfil')
!
! Read and check VERSION Number
! -----------------------------
  DO
    READ (lfnloc,'(A)',END=901) line
    IF(line(1:16).EQ.'FORMAT VERSION: ') EXIT
  ENDDO
  READ (line, '(16X,F6.2)') satfil%fvers
  IF (satfil%fvers + 0.0001D0 < satfilvers) THEN
    WRITE(lfnerr,"(/,' *** SR RDSATFIL: Format version of SATELLIT file &
                &                       too old', &
                &  /,'                  Supported format version:',F8.2,      &
                &  /,'                  Format version in file  :',F8.2,/)")  &
                     satfilvers, satfil%fvers
    CALL EXITRC(2)
  ENDIF
  IF (satfil%fvers > satfilvers) THEN
    WRITE(lfnerr,"(/,' *** SR RDSATFIL: Format version of SATELLIT file &
                &                       too new', &
                &  /,'                  Supported format version:',F8.2,      &
                &  /,'                  Format version in file  :',F8.2,/)")  &
                     satfilvers, satfil%fvers
    CALL EXITRC(2)
  ENDIF
  REWIND(lfnloc)
!
! Read antenna model
! ------------------
  DO
    READ (lfnloc,'(A)',END=903) line
    IF(line(1:15).EQ.'ANTENNA MODEL: ') EXIT
  ENDDO
  READ (line, '(15X,A10)') satfil%pcvmod
!
! Read radiation pressure model
! -----------------------------
  DO
    READ (lfnloc,'(A)',END=900) line
    IF(line(1:27).EQ.'RADIATION PRESSURE MODEL : ') EXIT
  ENDDO
  READ (line, '(27X,A8)') satfil%rpmodel
  REWIND(lfnloc)
!
! Map the file SATELL
! -------------------
  nlin = 0

  Loop_001: DO
    READ (lfnloc,'(A)',END=999) line
    nlin = nlin + 1
    IF (line(1:37) == 'PART 1: PHYSICAL SATELLITE PARAMETERS') THEN
      DO
        READ (lfnloc,'(A)',END=999) line
        nlin = nlin + 1
        IF (line(1:8) == 'PRN  SVN') THEN
          READ (lfnloc,'(A)',END=999)
          nlin = nlin + 1
          map(1)%pos1 = nlin + 1
          DO
            READ (lfnloc,'(A)',END=999) line
            nlin = nlin + 1
            IF (line == '') THEN
              map(1)%pos2 = nlin-1
              EXIT Loop_001
            END IF
          END DO
        END IF
      END DO
    END IF
  END DO Loop_001

  Loop_002: DO
    READ (lfnloc,'(A)',END=999) line
    nlin = nlin + 1
    IF (line(1:24) == 'PART 2: ON-BOARD SENSORS') THEN
      DO
        READ (lfnloc,'(A)',END=999) line
        nlin = nlin + 1
        IF (line(1:9) == 'PRN  TYPE') THEN
          READ (lfnloc,'(A)',END=999)
          nlin = nlin + 1
          map(2)%pos1 = nlin + 1
          DO
            READ (lfnloc,'(A)',END=999) line
            nlin = nlin + 1
            IF (line == '') THEN
              map(2)%pos2 = nlin-1
              EXIT Loop_002
            END IF
          END DO
        END IF
      END DO
    END IF
  END DO Loop_002

  nrprmod = 0
  map(3)%pos1 = -1
  Loop_003: DO
     READ (lfnloc,'(A)',END=999) line
     nlin = nlin + 1
     IF (line(1:22) == 'PART 3: RPR PARAMETERS') THEN
        DO
           READ (lfnloc,'(A)',END=999) line
           nlin = nlin + 1
           IF (line(1:15) == 'PART 4: REMARKS') EXIT Loop_003
           IF (line(1:9) == 'PRN  YYYY') THEN
              READ (lfnloc,'(A)',END=999)
              nlin = nlin + 1
              IF (map(3)%pos1 == -1) map(3)%pos1 = nlin-1
              DO
                 READ (lfnloc,'(A)',END=999) line
                 nlin = nlin + 1
                 IF (line == '') THEN
                    map(3)%pos2 = nlin-1
                    EXIT
                 ELSE
                    nrprmod = nrprmod + 1
                 END IF
              END DO
           END IF
        END DO
     END IF
  END DO Loop_003
!
! Allocate memory for requests
! ----------------------------
  satfil%nsatellite = map(1)%pos2-map(1)%pos1+1
  satfil%nsensor = map(2)%pos2-map(2)%pos1+1
  satfil%nrprmod = nrprmod

  IF (ASSOCIATED(satfil%satellite)) DEALLOCATE(satfil%satellite,stat=iac)
  ALLOCATE(satfil%satellite(satfil%nsatellite),stat=iac)
  CALL alcerr(iac, 'satfil%satellite', (/satfil%nsatellite/), 'rdsatfil')

  IF (ASSOCIATED(satfil%sensor)) THEN
    DO ii = 1,SIZE(satfil%sensor)
       IF (ASSOCIATED(satfil%sensor(ii)%sigli)) DEALLOCATE(satfil%sensor(ii)%sigli,stat=iac)
    END DO
    DEALLOCATE(satfil%sensor,stat=iac)
  END IF
  ALLOCATE(satfil%sensor(satfil%nsensor),stat=iac)
  CALL alcerr(iac, 'satfil%sensor', (/satfil%nsensor/), 'rdsatfil')

  DO ii = 1,satfil%nsensor
    NULLIFY(satfil%sensor(ii)%sigli)
    satfil%sensor(ii)%nsigli = 0
  END DO

  IF (ASSOCIATED(satfil%rprmod)) DEALLOCATE(satfil%rprmod,stat=iac)
  ALLOCATE(satfil%rprmod(satfil%nrprmod),stat=iac)
  CALL alcerr(iac, 'satfil%rprmod', (/satfil%nrprmod/), 'rdsatfil')

  DO ii=1,nrprmod
     NULLIFY(satfil%rprmod(ii)%rprcoe)
     satfil%rprmod(ii)%nrprcoe = 0
  END DO
!
! Read the requests
! -----------------
  REWIND(lfnloc)
!
! Read PART 1: PHYSICAL SATELLITE PARAMETERS t_satellite
! ------------------------------------------------------
!
  REWIND(lfnloc)
!
! Read satellite parameters
! -------------------------
  DO ii = 1,map(1)%pos1 - 1
    READ (lfnloc,'(A)',END=999) line
  END DO

  DO ii = map(1)%pos1, map(1)%pos2
    icrx = ii - map(1)%pos1 + 1
    READ (lfnloc,'(A)',END=999) line
    READ (line, '(I3,2X,A3,I4,3X,A9,I6,5X,A40,F10.1,2F11.4,I6,F11.4,3X,A3)')  &
           satfil%satellite(icrx)%svn, satfil%satellite(icrx)%svnnr(2:4),     &
           satfil%satellite(icrx)%iblock,                                     &
           satfil%satellite(icrx)%cospar, satfil%satellite(icrx)%attflag,     &
           datstr, satfil%satellite(icrx)%mass, satfil%satellite(icrx)%formf, &
           satfil%satellite(icrx)%radpres, satfil%satellite(icrx)%admodel,    &
           satfil%satellite(icrx)%adrag,satfil%satellite(icrx)%plane
!
! Add system flag to SVN number and fill blank with '0' if necessary
! ------------------------------------------------------------------
    satfil%satellite(icrx)%svnnr(1:1) = &
       g_svnsys(int(satfil%satellite(icrx)%svn/100))
    IF (satfil%satellite(icrx)%svnnr(4:4) /= ' ') THEN
       IF (satfil%satellite(icrx)%svnnr(2:2)==' ') &
              satfil%satellite(icrx)%svnnr(2:2)='0'
       IF (satfil%satellite(icrx)%svnnr(3:3)==' ') &
              satfil%satellite(icrx)%svnnr(3:3)='0'
    ENDIF
!
! Transform time information given in Y,M,D,H,M,S into Julian date
! ----------------------------------------------------------------
    CALL st2tim(1, 2, datstr, satfil%satellite(icrx)%timint%t)
  END DO
!
! Read PART 2: ON-BOARD SENSORS
! -----------------------------
  DO ii = map(1)%pos2 + 1, map(2)%pos1 - 1
    READ (lfnloc,'(A)',END=999) line
  END DO

  DO ii = map(2)%pos1, map(2)%pos2
    icrx = ii - map(2)%pos1 + 1
    READ (lfnloc,'(A)',END=999) line
    READ (line, '(I3,2X,A4,2X,A20,3X,I5,2X,A40,3X,3F10.4,3X,3F8.4,3X,&
         &  3F8.4,3X,A20,I6,2X,A60)')      &
         satfil%sensor(icrx)%svn, satfil%sensor(icrx)%type,             &
         satfil%sensor(icrx)%sensor, satfil%sensor(icrx)%numb,          &
         datstr, (satfil%sensor(icrx)%antoff(kk),kk=1,3),               &
         (bore(kk),kk=1,3),(azim(kk),kk=1,3), satfil%sensor(icrx)%name, &
         satfil%sensor(icrx)%ifrq, sigli
!
! Check length and orthogonality of sensor vectors
! ------------------------------------------------
    lengthb=DABS(1.D0-DSQRT(bore(1)**2+bore(2)**2+bore(3)**2))
    lengtha=DABS(1.D0-DSQRT(azim(1)**2+azim(2)**2+azim(3)**2))
    orth=DABS(DOT_PRODUCT(bore,azim))

    IF (lengthb.GT.0.0005D0.OR.lengtha.GT.0.0005D0.OR.orth.GT.0.0005) GOTO 910

    satfil%sensor(icrx)%bore(:)=bore(:)
    satfil%sensor(icrx)%azim(:)=azim(:)

    IF (sigli /= ' ') THEN
      CALL splarg(sigli,satfil%sensor(icrx)%sigli)
      satfil%sensor(icrx)%nsigli = SIZE(satfil%sensor(icrx)%sigli)
    END IF
!
! Transform time information given in Y,M,D,H,M,S into Julian date
! ----------------------------------------------------------------
    CALL st2tim(1, 2, datstr, satfil%sensor(icrx)%timint%t)

  END DO

!
! Check consistency of SATELLIT. file between part 1 and part 2
! -------------------------------------------------------------
  DO icrx=1,satfil%nsensor

    found=0
    remind1='  -1'
    remind2='  -2'

check: DO icrx1 = 1,satfil%nsatellite
      IF ( satfil%satellite(icrx1)%svn == satfil%sensor(icrx)%svn ) THEN
        IF ( satfil%sensor(icrx)%timint%t(1) >= &
             satfil%satellite(icrx1)%timint%t(1) .AND. &
             satfil%sensor(icrx)%timint%t(1) < &
             satfil%satellite(icrx1)%timint%t(2) ) THEN
          remind1 = satfil%satellite(icrx1)%svnnr
        END IF
        IF( satfil%sensor(icrx)%timint%t(2) <= &
            satfil%satellite(icrx1)%timint%t(2).AND. &
            satfil%sensor(icrx)%timint%t(2) > &
            satfil%satellite(icrx1)%timint%t(1) ) THEN
          remind2 = satfil%satellite(icrx1)%svnnr
        END IF
        IF (remind1==remind2) THEN
          found=1
          EXIT check
        END IF
      END IF
    END DO check
!
!   If different or no SVN found
!   ----------------------------
    IF (found == 0) GOTO 902
  ENDDO

!
! Read PART 3: RPR-PARAMETERS
! ---------------------------
  DO ii = map(2)%pos2 + 1, map(3)%pos1 - 1
    READ (lfnloc,'(A)',END=999) line
  END DO

  icrx = 0
  DO ii = map(3)%pos1, map(3)%pos2
    READ (lfnloc,'(A)',END=999) line
    IF (line(1:15) == 'PART 4: REMARKS') EXIT
    IF (line(1:9) == 'PRN  YYYY') THEN
       READ (lfnloc,'(A)',END=999)
       DO
          READ (lfnloc,'(A)',END=999) line
          IF (line == '') THEN
             EXIT
          ELSE
             icrx = icrx + 1
             READ (line, '(I3,2X,A40,2X,I3,2X,I3)') &
                  satfil%rprmod(icrx)%svn,datstr,&
                  satfil%rprmod(icrx)%rpmodel,satfil%rprmod(icrx)%nrprcoe
             !
             ! Transform time information given in Y,M,D,H,M,S into Julian date
             ! ----------------------------------------------------------------
             CALL st2tim(1, 2, datstr, satfil%rprmod(icrx)%timint%t)
             !
             ! Allocate memory for rpr parameters
             ! ----------------------------------
             ALLOCATE(satfil%rprmod(icrx)%rprcoe(satfil%rprmod(icrx)%nrprcoe),&
                      stat=iac)
             CALL alcerr(iac, 'satfil%rprmod%rprcoe', &
                        (/satfil%rprmod(icrx)%nrprcoe/), 'rdsatfil')
             !
             ! Read RPR Parameters
             ! -------------------
             DO kk = 1,satfil%rprmod(icrx)%nrprcoe
                READ(line(55+(kk-1)*10:),'(F10.4)') &
                     satfil%rprmod(icrx)%rprcoe(kk)
                !
                ! Scaling for RPRmodel/=3
                ! -----------------------
                IF ( satfil%rprmod(icrx)%rpmodel /= 3 ) THEN
                   satfil%rprmod(icrx)%rprcoe(kk) = &
                     satfil%rprmod(icrx)%rprcoe(kk)*1.D-9

                ! Scaling for RPRmodel=3 (Lageos)
                ! -------------------------------
                ELSEIF ( satfil%rprmod(icrx)%rpmodel == 3 .AND. &
                         kk == 2                                ) THEN
                   satfil%rprmod(icrx)%rprcoe(kk) = &
                     satfil%rprmod(icrx)%rprcoe(kk)*1.D-16
                END IF
             END DO

          END IF
       END DO
    END IF
  END DO

!
! Check consistency of SATELLIT. file between part 1 and part 3
! -------------------------------------------------------------
  DO icrx=1,satfil%nrprmod

    found=0
    remind1='  -1'
    remind2='  -2'

check2: DO icrx1 = 1,satfil%nsatellite
      IF ( satfil%satellite(icrx1)%svn == satfil%rprmod(icrx)%svn ) THEN
        IF ( satfil%rprmod(icrx)%timint%t(1) >= &
             satfil%satellite(icrx1)%timint%t(1) .AND. &
             satfil%rprmod(icrx)%timint%t(1) < &
             satfil%satellite(icrx1)%timint%t(2) ) THEN
          remind1 = satfil%satellite(icrx1)%svnnr
        END IF
        IF( satfil%rprmod(icrx)%timint%t(2) <= &
            satfil%satellite(icrx1)%timint%t(2).AND. &
            satfil%rprmod(icrx)%timint%t(2) > &
            satfil%satellite(icrx1)%timint%t(1) ) THEN
          remind2 = satfil%satellite(icrx1)%svnnr
        END IF
        IF (remind1==remind2) THEN
          found=1
          EXIT check2
        END IF
      END IF
    END DO check2
!
!   If different or no SVN found
!   ----------------------------
    IF (found == 0) GOTO 904
  ENDDO

  CLOSE (lfnloc)

  RETURN

900  WRITE(lfnerr,"(' *** SR RDSATFIL: Error reading &
                                       & RADIATION PRESSURE MODEL')")
  CALL exitrc(2)
901  WRITE(lfnerr,"(' *** SR RDSATFIL: Error reading FORMAT VERSION')")
  CALL exitrc(2)
902  WRITE(lfnerr,"(' *** SR RDSATFIL: No resp. no unique SVN found for &
                & sensor with PRN:',I4)")satfil%sensor(icrx)%svn
  CALL exitrc(2)
903  WRITE(lfnerr,"(' *** SR RDSATFIL: Error reading ANTENNA MODEL')")
  CALL exitrc(2)
904  WRITE(lfnerr,"(' *** SR RDSATFIL: No resp. no unique SVN found for &
                &satellite with PRN:',I4)")satfil%rprmod(icrx)%svn
  CALL exitrc(2)
910  WRITE(lfnerr,"(' *** SR RDSATFIL: Sensor vectors should be orthonormal', &
                & /,'                  SVN   :',I6,                           &
                & /,'                  Sensor: ',A)")                         &
                       satfil%sensor(icrx)%svn,satfil%sensor(icrx)%sensor
  CALL exitrc(2)
999  WRITE(lfnerr,"(' *** SR RDSATFIL: Error reading file ',A)") TRIM(filename)
  WRITE(lfnerr,*)'errorline:',line
  CALL exitrc(2)

END SUBROUTINE rdsatfil

END MODULE
