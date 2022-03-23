!
PROGRAM SATFCNV
!
! NAME       :  SATFCNV
!
! PURPOSE    :  CONVERT SATELLIT. file from new to old format
!
! SR CALLED  :  DEFCON, OPNERR, OPNFIL, OPNSYS, GTFLNA
!
! REMARKS    :
!
! AUTHOR     :  A.GAEDE
!
! VERSION    :  5.1
!
! CREATED    :  08-NOV-05             LAST MODIFIED : 01-Feb-2012
!
! Changes:    17-Nov-2005 AG: Changing of Version and Block number added
!             16-Mar-2006 AG: Replace satellite antenna names added
!             23-Mar-2006 AG: Unused variables removed
!             10-Aug-2006 HB: USE s_timst2 module
!             25-Oct-2006 AG: Old RPR coefficients implemented
!             14-Nov-2006 AG: Coefficients from PRN23 copied for PRN32
!             27-Feb-2007 AG: Call DEFCON
!             19-Nov-2007 HB: Exclude some LEOs, which are officially not
!                             available
!             23-Sep-2010 RD: Enable CPU counter
!             28-Sep-2010 RD: Do not insert version if the string is not there
!             06-Oct-2010 RD: Exitrc added at the end
!             01-Feb-2012 HB: Exclude more LEOs to shorten SATELLIT-file
!
! COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!      1990      UNIVERSITY OF BERN
!                    SWITZERLAND
!

  USE m_bern
  USE m_cpu,    ONLY: cpu_start
  USE s_exitrc
  USE s_gtflna
  USE d_inpkey, ONLY : inpKey,init_inpkey
  USE s_opnerr
  USE s_opnfil
  USE s_opnsys
  USE s_defcon
  USE s_rdsatfil
  USE s_readinpf
  USE s_timst2
  USE d_satfil, ONLY : t_satfil,init_satfil,typeMWTR

  IMPLICIT NONE

! Dummy list
! ----------
  TYPE(t_satfil), SAVE             :: satfil
  LOGICAL,        SAVE             :: first= .TRUE.
!
! DECLARATIONS INSTEAD OF IMPLICIT
! --------------------------------
  INTEGER(i4b)      :: isats,isens,i,j
  INTEGER(i4b)      :: irc,ios,kk,test,help
  INTEGER(i4b)      :: ILIN,IOSTAT,IRCODE
!
  CHARACTER(LEN=200)                        :: LINE
  CHARACTER(LEN=40)                         :: DATSTR
  CHARACTER(LEN=fileNameLength)             :: filename
  CHARACTER(LEN=fileNameLength80)           :: FILCNV
!
  REAL(r8b), DIMENSION(32,4,4)              :: COEF
  REAL(r8b), DIMENSION(4)                   :: GLOCOEF

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

!
! Hardwired old RPR coefficients (C980101)
! ----------------------------------------
  DO i=1,32
    DO j=1,4
      COEF(i,j,:)=(/8.,-10.0000,0.0000,0.0000/)
    ENDDO
  ENDDO

  COEF(1,3,:) = (/8., -9.1088, 0.7458,-0.4868/)
  COEF(2,2,:) = (/8., -9.9373, 0.6362, 0.0480/)
  COEF(3,3,:) = (/8., -9.0395, 0.5637,-0.3960/)
  COEF(4,3,:) = (/8., -9.0502, 0.7856,-0.2487/)
  COEF(5,3,:) = (/8., -9.0414, 0.7612,-0.2309/)
  COEF(6,3,:) = (/8., -9.0354, 0.7589,-0.3092/)
  COEF(7,3,:) = (/8., -9.0238, 1.0376,-0.2241/)
  COEF(8,3,:) = (/8., -9.3342, 1.8394,-0.7143/)
  COEF(9,3,:) = (/8., -9.0317, 0.7955,-0.3569/)
  COEF(10,3,:)= (/8., -8.9546, 0.7819,-0.1772/)
  COEF(12,1,:)= (/8., -8.6817, 0.1244, 0.5131/)
  COEF(13,4,:)= (/8., -9.9599,-0.2801,-1.6732/)
  COEF(14,2,:)= (/8., -9.9290, 0.9064,-0.2510/)
  COEF(15,2,:)= (/8., -9.8985, 0.7048,-0.4749/)
  COEF(16,2,:)= (/8., -9.9108, 0.6496,-0.1170/)
  COEF(17,2,:)= (/8., -9.9010, 0.6604,-0.0770/)
  COEF(18,2,:)= (/8., -9.9359, 0.8683,-0.4783/)
  COEF(19,2,:)= (/8., -9.9850, 0.7057,-0.1449/)
  COEF(20,2,:)= (/8.,-10.0396, 0.6642,-0.4997/)
  COEF(21,2,:)= (/8., -9.9477, 0.2592, 0.0996/)
  COEF(22,3,:)= (/8., -9.0944, 0.7319,-0.0179/)
  COEF(23,3,:)= (/8., -7.8592, 0.7440,-1.0843/)
  COEF(24,3,:)= (/8., -9.1436, 1.0537,-0.2214/)
  COEF(25,3,:)= (/8., -9.0785, 0.8556,-0.3851/)
  COEF(26,3,:)= (/8., -9.0377, 0.9750,-0.4144/)
  COEF(27,3,:)= (/8., -9.0291, 0.9482,-0.4224/)
  COEF(28,3,:)= (/8., -9.0951, 0.8210,-0.1303/)
  COEF(29,3,:)= (/8., -9.1015, 0.9078,-0.5188/)
  COEF(30,3,:)= (/8., -9.0455, 0.8285,-0.5409/)
  COEF(31,3,:)= (/8., -9.0370, 0.6269,-0.6173/)
  COEF(32,3,:)= (/8., -7.8592, 0.7440,-1.0843/)

  GLOCOEF(:)  = (/0.,  0.0000, 0.0000, 0.0000/)


! GET THE NAME OF THE INPUT FILE
! ------------------------------
  CALL init_inpkey(inpKey)
  CALL readinpf(' ',inpKey)
!
! DEFINE SYSTEM FILES AND CONSTANTS
! ---------------------------------
  CALL OPNSYS
  CALL DEFCON(0)
!
! GET FILENAME OF SATELLIT. FILE IN 5.0 FORMAT
! --------------------------------------------
  CALL GTFLNA(1,'OLDFIL',FILCNV,IRCODE)
!
! OPEN EXTERNAL FILE
! ------------------
  CALL OPNFIL(LFN002,FILCNV,'UNKNOWN','FORMATTED',' ',' ',IOSTAT)
  CALL OPNERR(LFNERR,LFN002,IOSTAT,FILCNV,'OLDFIL')
!
! If called for the first time, read the entire SATELLIT. file
! ============================================================
  IF (first) THEN
    first = .FALSE.

! Get the satellite info file name
    CALL gtflna(1,'SATELL ',filename,IRC)

! Read satellite info file (SATELL)
    CALL init_satfil(satfil)
    CALL rdsatfil(filename,satfil)
  END IF
  CALL opnfil(lfn001,filename,'OLD','FORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnerr,lfn001,ios,filename,'SATFCNV')
!
! WRITE SATELLIT. file in old FORMAT
! ----------------------------------
  DO ilin=1,1000
    test=-1
    READ(lfn001,'(A)',END=990,ERR=999)LINE
    READ(LINE(1:3),'(I3)',iostat=ios)test
    IF (LINE(1:6)=='FORMAT') THEN
      CYCLE
    ELSEIF (LINE(1:6)=='RADIAT') THEN
      CYCLE
    ELSEIF (LINE(1:6)=='PART 1') THEN
      WRITE(LFN002,'(A)')TRIM(LINE)
      READ(lfn001,'(A)',END=990,ERR=999)LINE
      WRITE(LFN002,'(A)')TRIM(LINE)
      READ(lfn001,'(A)',END=990,ERR=999)LINE
      WRITE(LFN002,*)
      WRITE(LFN002,'(A)')'RADIATION PRESSURE MODEL : C980101    (CODE MODEL COD9801, SPRINGER ET AL, 98)'
    ELSEIF (LINE(1:6)=='PART 3') THEN
      DO WHILE (LINE(1:6)/='PART 4')
        READ(lfn001,'(A)',END=990,ERR=999)LINE
      ENDDO
      LINE(6:6)='3'
      WRITE(LFN002,'(A)')TRIM(LINE)
    ELSEIF (ilin == 1) THEN
      IF(INDEX(LINE,"5.1")>0) THEN
        WRITE(LINE(INDEX(LINE,"5.1"):INDEX(LINE,"5.1")+2),'("5.0")')
      ELSEIF (LEN_TRIM(line) == 0) THEN
        WRITE(lfnerr,'(/,A,/,17X,2A,/)') &
             ' *** PG SATFCNV: The first line of the satellite info file should not be empty',&
             'Filename: ',TRIM(filename)
        CALL exitrc(2)
      ENDIF
      WRITE(LFN002,'(A)')trim(LINE)
    ELSEIF (LINE(8:14)=='  BLOCK') THEN
      WRITE(LFN002,'(A,A)')'     BLOCK    COSPAR  ATTITUDE      START TIME           END TIME           ', &
                         'MASS    AREA/MASS RAD.PRESS  DP0       P2         P3     RAD.PRESS AIR DRAG AIR DRAG  IFRQ PLN SLT   '
    ELSEIF (LINE(1:9)=='PRN  SVN ') THEN
      WRITE(LFN002,'(A,A,/)')'PRN   NO.       ID      FLAG   YYYY MM DD HH MM SS  YYYY MM DD HH MM SS     (KG)    ', &
                           '(M2/KG)   MODEL   (1.E-8)    (1.E-9)    (1.E-9)      C0     MODEL     C0            '
      READ(lfn001,'(A)',END=990,ERR=999)LINE
      DO isats=1,satfil%nsatellite
! exlude some LEOs
! 911: ICESAT
! 913: TerraSAR-X
! 914: MetOp-A / GRAS
! 915: GOCE
! 921-926: COSMIC
        IF (satfil%satellite(isats)%svn >= 911 .AND. satfil%satellite(isats)%svn <= 926) CYCLE
        help = 0
        DO isens = 1,satfil%nsensor
          IF ( satfil%satellite(isats)%svn == satfil%sensor(isens)%svn ) THEN
            IF ( satfil%satellite(isats)%svn < 300                                       .AND. &
                 satfil%sensor(isens)%timint%t(1) >= satfil%satellite(isats)%timint%t(1) .AND. &
                 satfil%sensor(isens)%timint%t(2) <= satfil%satellite(isats)%timint%t(2) .AND. &
                 satfil%sensor(isens)%sensor(1:2)=='MW') THEN
              IF (satfil%satellite(isats)%iblock > 4 .AND. satfil%satellite(isats)%iblock < 100) &
                   satfil%satellite(isats)%iblock = 4
              CALL timst2(2,2,satfil%sensor(isens)%timint%t,datstr)
! GPS satellites
              IF (satfil%satellite(isats)%svn <= 32) THEN
                WRITE(LFN002,'(I3,2X,I3,3X,A9,I6,5X,A40,F10.1,F11.4,I6,3F11.4,F11.4,        &
                   &I6,F11.4,3X,I3,2X,A3)')                                      &
                   satfil%satellite(isats)%svn, satfil%satellite(isats)%iblock,            &
                   satfil%satellite(isats)%cospar, satfil%satellite(isats)%attflag,        &
                   datstr, satfil%satellite(isats)%mass, satfil%satellite(isats)%formf,    &
                   NINT(COEF(satfil%satellite(isats)%svn,satfil%satellite(isats)%iblock,1)),&
                   (COEF(satfil%satellite(isats)%svn,satfil%satellite(isats)%iblock,kk),kk=2,4), &
                   satfil%satellite(isats)%radpres, satfil%satellite(isats)%admodel,       &
                   satfil%satellite(isats)%adrag,satfil%sensor(isens)%ifrq,                &
                   satfil%satellite(isats)%plane
              ELSE
! GLONASS satellites
                WRITE(LFN002,'(I3,2X,I3,3X,A9,I6,5X,A40,F10.1,F11.4,I6,3F11.4,F11.4,        &
                              &I6,F11.4,3X,I3,2X,A3)')                                    &
                    satfil%satellite(isats)%svn, satfil%satellite(isats)%iblock,          &
                    satfil%satellite(isats)%cospar, satfil%satellite(isats)%attflag,      &
                    datstr, satfil%satellite(isats)%mass, satfil%satellite(isats)%formf,  &
                    NINT(GLOCOEF(1)),(GLOCOEF(kk),kk=2,4),                          &
                    satfil%satellite(isats)%radpres, satfil%satellite(isats)%admodel,     &
                    satfil%satellite(isats)%adrag,satfil%sensor(isens)%ifrq,              &
                    satfil%satellite(isats)%plane
              ENDIF
            ELSEIF ( satfil%satellite(isats)%svn >= 300 .AND. help == 0 )THEN
! all others
              help = 1
              CALL timst2(2,2,satfil%satellite(isats)%timint%t,datstr)
              WRITE(LFN002,'(I3,2X,I3,3X,A9,I6,5X,A40,F10.1,F11.4,I6,3F11.4,F11.4,        &
                              &I6,F11.4,3X,I3,2X,A3)')                                    &
                    satfil%satellite(isats)%svn, satfil%satellite(isats)%iblock,          &
                    satfil%satellite(isats)%cospar, satfil%satellite(isats)%attflag,      &
                    datstr, satfil%satellite(isats)%mass, satfil%satellite(isats)%formf,  &
                    NINT(GLOCOEF(1)),(GLOCOEF(kk),kk=2,4),                          &
                    satfil%satellite(isats)%radpres, satfil%satellite(isats)%admodel,     &
                    satfil%satellite(isats)%adrag,satfil%sensor(isens)%ifrq,              &
                    satfil%satellite(isats)%plane
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ELSEIF (LINE(47:51)=='START') THEN
      WRITE(LFN002,'(A160)')LINE(20:170)
    ELSEIF (LINE(1:9)=='PRN  TYPE')THEN
      WRITE(LFN002,'("PRN   SATELLITE NAME______   ",A,/)')LINE(40:170)
      READ(lfn001,'(A)',END=990,ERR=999)LINE
      DO isens=1,satfil%nsensor
! exlude some LEOs
! 911: ICESAT
! 913: TerraSAR-X
! 914: MetOp-A / GRAS
! 915: GOCE
! 921-926: COSMIC
        IF (satfil%sensor(isens)%svn >= 911 .AND. satfil%sensor(isens)%svn <= 926) CYCLE
        IF(satfil%sensor(isens)%sensor(1:4)==typeMWTR) THEN
          IF(satfil%sensor(isens)%svn < 300) THEN
            satfil%sensor(isens)%sensor(4:10)='TRANSM '
          ELSE
            satfil%sensor(isens)%sensor(10:17)=satfil%sensor(isens)%sensor(4:11)
            satfil%sensor(isens)%sensor(4:10)='TRANSM '
          ENDIF
        ENDIF
        CALL timst2(2,2,satfil%sensor(isens)%timint%t,datstr)
        WRITE(LFN002,'(I3,3X,A20,5X,A40,3X,3F10.4,3X,3F8.4,3X,3F8.4)')  &
         satfil%sensor(isens)%svn,satfil%sensor(isens)%sensor,          &
         datstr, (satfil%sensor(isens)%antoff(kk),kk=1,3),              &
         (satfil%sensor(isens)%bore(kk),kk=1,3),                        &
         (satfil%sensor(isens)%azim(kk),kk=1,3)
      ENDDO
    ELSEIF (test<=0) THEN
      WRITE(LFN002,'(A)')TRIM(LINE)
    ENDIF
  ENDDO
999 WRITE(lfnerr,"(/,' *** PG SATFCNV: Error reading line in SATELLIT. file',&
                  &/,'                 File not converted!!!')")
990 CLOSE (lfn002)

  CALL exitrc(0)
END PROGRAM SATFCNV
