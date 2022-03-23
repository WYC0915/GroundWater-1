! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

  PROGRAM KINPRE

! ------------------------------------------------------------------------------
!
! Purpose  :  Generate a precise orbit file from
!             a kinematic coordinate file
!
! Remarks  :  ---
!
! Author   :  D.Svehla
!
! Created  :  18-Mar-2002
!
! Changes  :  04-Sep-2002 DS: Convert epochs with 'X' flag
!             12-Sep-2002 DS: New MENU
!             07-Mar-2003 HU: Use m_maxdim, SP3C implemented
!             08-Mar-2003 HU: Use interface for wtpreh, wtprei
!             16-Mar-2003 DS: Check KIN orbit with LEO standard orbit
!             17-Mar-2003 DS: Use LFN001,LFN002 instead of LFNORB and LFNOR1
!             17-Mar-2003 DS: Compute correlation coef. for SP3C format
!             18-Apr-2003 DS: Edited KIN file output
!             23-Apr-2003 HU: Nullify local pointers
!             04-Jun-2003 HU: Use implicit none
!             29-Oct-2003 HB: Correct format statement
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             30-May-2005 HU: Read RINEX clock file
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             28-Jun-2005 MM: Unused variables removed
!             01-Aug-2005 HU: Epoch as structure, use RADGMS2
!             05-Aug-2005 HB: Remove unused variables
!             27-Feb-2007 AG: Call DEFCON with parameter
!             01-Nov-2007 HB: Remove USE s_gtsclk
!             18-May-2009 HB: Correct epoch when GPS Week increases
!             18-Jun-2009 AJ: Enable position check with clock file
!             23-Sep-2010 RD: Enable CPU counter
!             29-Nov-2011 SL: new title string for pritit, m_bern with ONLY
!             15-Feb-2012 HB: Correct epoch for clock only if clock >=0.5D-8
!             27-Apr-2012 RD: Nullify pointers
!             09-Aug-2012 HB: Initialize NLEO=0
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ------------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, fileNameLength, staNameLength, &
                      lfnPrt, lfnErr, lfnRp1, lfn001, lfn002
  USE m_cpu,    ONLY: cpu_start
  USE m_maxdim, ONLY: maxsat
  USE m_epoch,  ONLY: t_epoch, OPERATOR(.epochToReal.)
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE d_const,  ONLY: date,time
  USE s_opnfil
  USE s_pritit
  USE s_defcon
  USE s_leoprn
  USE s_opnsys
  USE s_gtflna
  USE s_prflna
  USE s_readinpf
  USE s_opnerr
  USE s_kininp
  USE s_wtpreh
  USE s_wtprei
  USE s_exitrc
  USE s_gtleoco
  USE f_gpsmjd
  USE f_djul
  IMPLICIT NONE

! DECLARATIONS
! ------------
  REAL(r8b),DIMENSION(3)          :: POS,VEL,XSTD
  REAL(r8b),DIMENSION(6)          :: QXXE
  REAL(r8b),DIMENSION(2)          :: localWin
  REAL(r8b)                       :: TEPO_CORR
  REAL(r8b)                       :: TFIRST,TEPO,SECOND
  REAL(r8b)                       :: DTTABE,DTTAB,T1,T2
  REAL(r8b)                       :: BASPOS,BASCLK
  REAL(r8b)                       :: EDTKIN,DIFFKD,SZ,XPOL,YPOL,RMS0
  REAL(r8b)                       :: day,sec,clkval,ttt
  REAL(r8b),DIMENSION(1)          :: dtsatc,dtsatd
  REAL(r8b),DIMENSION(4,MAXSAT)   :: sdevp,sdevv
  REAL(r8b),DIMENSION(6,MAXSAT)   :: corrp,corrv

!
  INTEGER(i4b), DIMENSION(MAXSAT) :: NUMLEO
  INTEGER(i4b), DIMENSION(1)      :: LEOSVN
  INTEGER(i4b)                    :: IFIRST,NEPO,IEPO,NLEO
  INTEGER(i4b)                    :: NWEEK,IFRMAT,ACCURA,IRCCLK,IRCODE
  INTEGER(i4b)                    :: ILEO,IOSTAT,NFILES,K
  INTEGER(i4b)                    :: ICHKKIN,IRCSTD,IWRQXX,NFILKIN
  INTEGER(i4b)                    :: ifil,kepo,i,ii
  INTEGER(i4b)                    :: nreject
  INTEGER(i4b)                    :: lfnkou,ios
  INTEGER(i4b)                    :: iyyy,mm,iday,ih,im
  INTEGER(i4b), DIMENSION(MAXSAT)  :: satwgt
  INTEGER(i4b), DIMENSION(4,MAXSAT):: accpos,accvel
!
  CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: filNam
  CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: filKin

  CHARACTER(LEN=fileNameLength)   :: FILCLK
  CHARACTER(LEN=staNameLength)    :: STATMP
!
  CHARACTER(LEN=57),DIMENSION(4)  :: TITLE
  CHARACTER(LEN=57)               :: TITLE1
  CHARACTER(LEN=80)               :: LINE
  CHARACTER(LEN=19)               :: TIMSTR
  CHARACTER(LEN=5)                :: COOSYS,DATDES
  CHARACTER(LEN=4)                :: AGENCY
  CHARACTER(LEN=3)                :: ORBTYP,TIMSYS
  CHARACTER(LEN=1)                :: KINFLAG
  CHARACTER(LEN=1),DIMENSION(4,MAXSAT) :: evtflg
!
  TYPE(t_epoch)                   :: tmjd

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Nullify pointers
! ----------------
  NULLIFY(filNam)
  NULLIFY(filKin)

! Get the name of the input file
! ------------------------------
  CALL init_inpkey(inpKey)
  CALL readinpf(' ',inpKey)

! Define system files
! -------------------
  CALL opnsys

! Define constants
! ----------------
  CALL defcon(1)

! Write title and file list
! -------------------------
  CALL pritit('KINPRE','Convert kinematic positions to precise orbits')
  CALL prflna
!
! Read all informations from INP-file
! -----------------------------------
  CALL kininp(filNam,filKin,IFRMAT,DATDES,ORBTYP,AGENCY, &
               ACCURA,TITLE,localWin,ICHKKIN,EDTKIN,IWRQXX)
!
  NFILES = size(filNam,2)
  IF (ICHKKIN.EQ.1) NFILKIN = size(filKin,1)
  SATWGT(1:MAXSAT) = ACCURA
!
! SP3c
! ----
  timsys='GPS'
  baspos=1.25D0
  basclk=1.025D0
  accpos=0
  accvel=0
  evtflg=' '
  sdevp=0D0
  sdevv=0D0
  corrp=0D0
  corrv=0D0

  NLEO=0

! Avoid numerical problems
! ------------------------
  localWin(1)=localWin(1)-1.D-10
  localWin(2)=localWin(2)+1.D-10
!
! LOOP OVER ALL FILES
! -------------------
  DO iFil=1,NFILES
!
! OPEN KIN FILE
! -------------
    CALL OPNFIL(LFN001,filNam(1,iFil),'OLD','FORMATTED',                 &
                'READONLY',' ',IOSTAT)
    CALL OPNERR(LFNERR,LFN001,IOSTAT,filNam(1,iFil),'KINPRE')

    IF (ICHKKIN.EQ.1 .AND. NFILKIN.NE.0) THEN
      LFNKOU=LFN002+3
      CALL OPNFIL(LFNKOU,filKin(1,iFil),'NEW','FORMATTED',                 &
                  'READONLY',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNKOU,IOSTAT,filKin(1,iFil),'KINPRE')
    END IF
!
! GET SATELLITE CLOCK FILE NAME
! -----------------------------
    CALL GTFLNA(0,'RNXCLK ',FILCLK,IRCCLK)
    IF (IRCCLK.EQ.0) THEN
      IF (NLEO.GT.1) THEN
        WRITE(LFNERR,"(/,' ### KINPRE: READING OF CLOCK FILE CURRENTLY', &
                     &                'NOT IMPLEMENTED',                 &
                     & /,'             FOR MORE THAN ONE RECEIVER')")
        IRCCLK=-1
      ELSE
        CALL OPNFIL(LFNRP1,FILCLK,'OLD','FORMATTED',                 &
                   'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRP1,IOSTAT,FILCLK,'KINPRE')
        TTT=0D0
      ENDIF
    ENDIF
!
! GET THE NUMBER OF EPOCHS SATELLITE NUMBERS AND TIME OF THE FIRST EPOCH
! ----------------------------------------------------------------------
    STATMP(1:staNameLength)=' '
    READ (LFN001,'(A57,//,22X,A5,20X,A19,/,96X,F10.6,//)',END=100,ERR=900)  &
          TITLE1,COOSYS,TIMSTR,RMS0

    IF (TITLE(1)(1:57)==' ') TITLE(1)(1:57)=TITLE1(1:57)
    NEPO=0
    KEPO=0
    IFIRST=1
    NLEO=0
    NUMLEO(:)=0
    DTTAB=1.D20
    NREJECT=0

    IF (ICHKKIN.EQ.1 .AND. NFILKIN.NE.0) THEN
      IF (IWRQXX == 0) THEN
        WRITE(LFNKOU,11) TITLE(1),DATE,TIME,COOSYS,TIMSTR
11      FORMAT(A64,1X,A9,1X,A5,/,80('-'),/,                          &
             'LOCAL GEODETIC DATUM: ',A16,2X,'EPOCH: ',A19,//,       &
             ' STATION NAME     WEEK  SECONDS',8X,                   &
             'X (M)',10X,'Y (M)',10X,'Z (M)',4X,'F',/)
      ELSE IF (IWRQXX == 1) THEN
        WRITE(LFNKOU,12) TITLE(1),DATE,TIME,COOSYS,TIMSTR,RMS0
12      FORMAT(A64,1X,A9,1X,A5,/,80('-'),/,                          &
             'LOCAL GEODETIC DATUM: ',A16,2X,'EPOCH: ',A19,          &
             /92X,'RMS=',F10.6/,' STATION NAME     WEEK  SECONDS',8X,&
             'X (M)',10X,'Y (M)',10X,'Z (M)',4X,'F',/)
      END IF
    END IF
!
10  DO
      IF (IWRQXX == 1) THEN
        READ (LFN001,'(1X,A16,1X,I4,1X,F8.0,1X,3F15.4,1X,A1,3X,6F13.5)',&
              END=100,ERR=900) STATMP,NWEEK,SECOND,POS,KINFLAG,QXXE
      ELSE
        READ (LFN001,'(1X,A16,1X,I4,1X,F8.0,1X,3F15.4,1X,A1)',END=100,ERR=900)&
              STATMP,NWEEK,SECOND,POS,KINFLAG
      ENDIF
      IF (KINFLAG==' ') GOTO 900
      NEPO=NEPO+1
      IF (KINFLAG=='K') THEN
        TMJD%DAY =GPSMJD(0D0,NWEEK)
        TMJD%FRAC=SECOND/86400D0
        TEPO=.epochToReal.TMJD
        IF (TEPO.GE.localWin(1) .AND. TEPO.LE.localWin(2)) THEN
!
! GET SATELLITE CLOCK CORRECTION FOR ONE EPOCH AND ONE SATELLITE
! FROM THE SATELLITE CLOCK FILE
! --------------------------------------------------------------
          IF (IRCCLK.EQ.0) THEN
!!!! Works only for one satellite
            IF (TTT.LT.TEPO-0.5D0/86400D0) THEN
              DO
                READ(LFNRP1,"(A)",IOSTAT=IOS) LINE
                IF (IOS.NE.0) THEN
                  TTT=1D20
                  EXIT
                ENDIF
                IF (LINE(1:2).EQ.'AR'.AND.LINE(4:7).EQ.STATMP(1:4)) THEN
                  READ(LINE,"(8X,I4,4I3,F10.6,6X,E19.12)") &
                             iyyy,mm,iday,ih,im,sec,clkval
                  DAY=DBLE(IDAY)
                  TTT=DJUL(iyyy,mm,day+(ih+(im+sec/60d0)/60d0)/24d0)
                  IF (TTT.GE.TEPO-0.5D0/86400D0) EXIT
                ENDIF
              ENDDO
            ENDIF
            IF (DABS(TTT-TEPO).LT.0.5D0/86400D0) THEN
              DTSATC(1)=CLKVAL
              TMJD%FRAC=TMJD%FRAC-DTSATC(1)/86400D0
              IF (tmjd%frac <0.D0) THEN
                tmjd%frac =1+tmjd%frac
                tmjd%day = tmjd%day-1
              ENDIF
            ELSE
              DTSATC(1)=999999.999999D0
            ENDIF
          ELSE
            DTSATC(1)=999999.999999D0
          ENDIF
          DTSATD(1)=999999.999999D0

          TEPO_CORR=.epochToReal.TMJD
!
! Check difference between kinematic and dynamic orbit
! ----------------------------------------------------
          IF (ICHKKIN.EQ.1) THEN
            CALL GTLEOCO(STATMP,TEPO_CORR,2,0,XSTD,SZ,XPOL,YPOL,IRCSTD)
            IF (IRCSTD.EQ.0) THEN
              DIFFKD=0.D0
              DO II=1,3
                DIFFKD=DIFFKD+(XSTD(II)-POS(II))**2
              END DO
              DIFFKD=DSQRT(DIFFKD)
              IF (DIFFKD.GE.EDTKIN) GOTO 10
              IF (NFILKIN.NE.0) THEN
                IF (IWRQXX == 1) THEN
                  WRITE(LFNKOU,31) STATMP,NWEEK,SECOND,                    &
                         (POS(I),I=1,3),'K',QXXE(1:6)
31                FORMAT(1X,A16,1X,I4,1X,F8.0,1X,                          &
                         3F15.4,1X,A1,3X,6F13.5)
                ELSE
                  WRITE(LFNKOU,41) STATMP,NWEEK,SECOND,                    &
                       (POS(I),I=1,3),'K'
41                FORMAT(1X,A16,1X,I4,1X,F8.0,1X,3F15.4,1X,A1)
                END IF
              END IF
            END IF
          END IF
          KEPO=KEPO+1
          CALL LEOPRN(STATMP(1:staNameLength),TEPO,LEOSVN(1))
          IF (KEPO.GT.1) THEN
            T1=T2
            T2=TEPO
            DTTABE=DNINT((T2-T1)*86400.D0)
            IF (DTTABE.NE.0.D0 .AND. DTTABE .LT.DTTAB) DTTAB=DTTABE
            DO ILEO=1,NLEO
              IF (NUMLEO(ILEO).EQ.LEOSVN(1)) GOTO 50
            END DO
            NLEO=NLEO+1
            IF (NLEO.GT.MAXSAT) GOTO 910
            NUMLEO(NLEO)=LEOSVN(1)
          ELSE
            T2=TEPO
            NLEO=1
            NUMLEO(1)=LEOSVN(1)
          END IF
50        IF (IFIRST.EQ.1) THEN
            TFIRST=TEPO
            IFIRST=0
          END IF
        END IF
      END IF
    END DO
100 REWIND (LFN001)
!
! REWIND CLOCK FILE IF NECESSARY
! ------------------------------
    IF (IRCCLK.EQ.0) THEN
      REWIND (LFNRP1)
      TTT=0D0
    ENDIF

    CALL WTPREH(filNam(2,iFil),LFN002,IFRMAT,NLEO,NUMLEO,SATWGT,TFIRST,  &
                KEPO,DTTAB,TITLE,DATDES,COOSYS,ORBTYP,AGENCY,            &
                timsys,baspos,basclk)
!
! READ KIN FILE
! -------------
    IF (IWRQXX.EQ.1) THEN
      READ (LFN001,'(A57,//,22X,A5,20X,A19,/,96X,F10.6,//)',ERR=900)&
            TITLE(1),COOSYS,TIMSTR,RMS0
    ELSE
      READ (LFN001,'(A57,//,22X,A5,20X,A19,///)',ERR=900) TITLE(1),&
            COOSYS,TIMSTR
    ENDIF
!
    DO IEPO=1,NEPO
      IF (IWRQXX.EQ.1) THEN
        READ (LFN001,'(1X,A16,1X,I4,1X,F8.0,1X,3F15.4,1X,A1,          &
            & 3X,6F13.5)',END=900) STATMP,NWEEK,SECOND,(POS(K),K=1,3),&
              KINFLAG,(QXXE(K),K=1,6)
      ELSE
        READ (LFN001,'(1X,A16,1X,I4,1X,F8.0,1X,3F15.4,1X,A1)',END=900)&
              STATMP,NWEEK,SECOND,(POS(K),K=1,3),KINFLAG
      ENDIF
!
      IF(KINFLAG=='K') THEN
        TMJD%DAY =GPSMJD(0D0,NWEEK)
        TMJD%FRAC=SECOND/86400D0
        TEPO=.epochToReal.TMJD
        IF (TEPO.GE.localWin(1) .AND. TEPO.LE.localWin(2)) THEN
          CALL LEOPRN(STATMP,TEPO,LEOSVN(1))
!
! GET SATELLITE CLOCK CORRECTION FOR ONE EPOCH AND ONE SATELLITE
! FROM THE SATELLITE CLOCK FILE
! --------------------------------------------------------------
          IF (IRCCLK.EQ.0) THEN
!!!! Works only for one satellite
            IF (TTT.LT.TEPO-0.5D0/86400D0) THEN
              DO
                READ(LFNRP1,"(A)",IOSTAT=IOS) LINE
                IF (IOS.NE.0) THEN
                  TTT=1D20
                  EXIT
                ENDIF
                IF (LINE(1:2).EQ.'AR'.AND.LINE(4:7).EQ.STATMP(1:4)) THEN
                  READ(LINE,"(8X,I4,4I3,F10.6,6X,E19.12)") &
                             iyyy,mm,iday,ih,im,sec,clkval
                  DAY=DBLE(IDAY)
                  TTT=DJUL(iyyy,mm,day+(ih+(im+sec/60d0)/60d0)/24d0)
                  IF (TTT.GE.TEPO-0.5D0/86400D0) EXIT
                ENDIF
              ENDDO
            ENDIF
            IF ( DABS(TTT-TEPO) < 0.5D0/86400D0 ) THEN
              DTSATC(1)=CLKVAL
            IF ( DABS(CLKVAL) >= 0.5D-8 ) THEN
              TMJD%FRAC=TMJD%FRAC-DTSATC(1)/86400D0
              IF (tmjd%frac <0.D0) THEN
                tmjd%frac =1+tmjd%frac
                tmjd%day = tmjd%day-1
              ENDIF
            ENDIF
            ELSE
              DTSATC(1)=999999.999999D0
            ENDIF
          ELSE
            DTSATC(1)=999999.999999D0
          ENDIF
          DTSATD(1)=999999.999999D0

          TEPO=.epochToReal.TMJD
!
! Check difference between kinematic and dynamic orbit
! ----------------------------------------------------
          IF (ICHKKIN.EQ.1) THEN
            CALL GTLEOCO(STATMP,TEPO,2,0,XSTD,SZ,XPOL,YPOL,IRCSTD)
!
            IF (IRCSTD.GT.0) THEN
              WRITE(LFNPRT,21)TEPO
21                FORMAT(' *** PG KINPRE: STANDARD ORBIT',           &
                  ' IS MISSING TO CHECK KIN. POSITIONS, EPOCH: ',    &
                  F15.6)
            ELSE
              DIFFKD=0.D0
              DO II=1,3
                DIFFKD=DIFFKD+(XSTD(II)-POS(II))**2
              END DO
              DIFFKD=DSQRT(DIFFKD)
              IF (DIFFKD.GE.EDTKIN) GOTO 25
              GOTO 30
25            NREJECT=NREJECT+1
              WRITE(LFNPRT,27)TEPO,DIFFKD
27              FORMAT(' *** PG KINPRE: 1D DIFFERENCE KIN-DYN', &
                ' TOO LARGE, EPOCH:',F15.6,3X,' DIFFERENCE: ',F7.3)
              CYCLE
            END IF
          END IF
30        CONTINUE
!
! WRITE EPOCH OF PRECISE ORBIT
! ----------------------------
          IF (IWRQXX.EQ.1.AND.QXXE(1).GT.0D0.AND. &
                              QXXE(2).GT.0D0.AND. &
                              QXXE(3).GT.0D0) THEN
            sdevp(1,1)=RMS0*DSQRT(QXXE(1))
            sdevp(2,1)=RMS0*DSQRT(QXXE(2))
            sdevp(3,1)=RMS0*DSQRT(QXXE(3))
!
            corrp(1,1)=QXXE(4)/DSQRT(QXXE(1))/DSQRT(QXXE(2))
            corrp(2,1)=QXXE(5)/DSQRT(QXXE(1))/DSQRT(QXXE(3))
            corrp(4,1)=QXXE(6)/DSQRT(QXXE(2))/DSQRT(QXXE(3))
          ELSE
            sdevp=0D0
            corrp=0D0
          END IF
          VEL(1:3)=0.D0
          CALL wtprei(lfn002,ifrmat,(/iwrqxx,0/),1,leosvn,tmjd,    &
                      pos,vel,dtsatc,dtsatd,accpos,accvel,evtflg,  &
                      sdevp,sdevv,corrp,corrv,ircode)
         END IF
      END IF
    END DO
!
! WRITE END OF FILE "EOF"
! -----------------------
    WRITE(LFN002,800)
800 FORMAT('EOF')

! WRITE NUMBER OF REJECTED EPOCHS
! -------------------------------
    IF (ICHKKIN.EQ.1) THEN
      WRITE(LFNPRT,77)NREJECT
77    FORMAT(/,' TOTAL NUMBER OF REJECTED KINEMATIC EPOCHS: ',I4,/)
    END IF
    GOTO 920

! ERRORS
! ------
900 WRITE(LFNERR,'(A54,/,16X,A6,A,/)')                                    &
          & '*** SR KINPRE: ERROR READING KINEMATIC COORDINATE FILE',     &
          &'FILE :',filNam(1,iFil)
    GOTO 920
910 WRITE(LFNERR,'(A,/,16X,A16,I4,/,16X,A16,I4,/,16X,A16,A,/)')           &
          & '*** SR KINPRE: NUMBER OF LEO SATELLITES GREATER THAN MAXSAT',&
          & '        MAXSAT :',MAXSAT,                                    &
          & 'NUMBER OF LEOs :',NLEO,                                      &
          & '          FILE :',filNam(1,iFil)
!
! CLOSE INPUT AND OUTPUT FILE
! ---------------------------
920 CLOSE(UNIT=LFN002)
    CLOSE(UNIT=LFN001)
    IF (IRCCLK.EQ.0) CLOSE(UNIT=LFNRP1)
    IF (ICHKKIN.EQ.1 .AND. NFILKIN.NE.0) CLOSE(UNIT=LFNKOU)
!
! NEXT FILE
! ---------
  END DO
!
!
! END OF PROGRAM
! --------------
  CALL EXITRC(0)

  END PROGRAM KINPRE
