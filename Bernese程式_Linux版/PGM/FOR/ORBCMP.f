C*
      PROGRAM ORBCMP
CC
CC NAME       :  ORBCMP
CC
CC PURPOSE    :  COMPARE TWO ORBITS IN PRECISE ORBIT FILE
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC CREATED    :  92/10/10
CC
CC CHANGES    :  26-MAY-93 : ??: SLIGHTLY DIFFERENT EPOCHS ALLOWED
CC                               (DT=1.0D-11)
CC               12-AUG-94 : MR: CALL EXITRC
CC                3-JAN-96 : TS: COSMETIC CHANGES (OPNFIL ETC.)
CC               13-MAY-96 : TS: INCREASED MAXFIL 40 --> 100
CC               15-APR-97 : TS: USE ALSO LAST EPOCH OF FILE
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               30-JUN-98 : TS: SMALL BUG CORRECTED
CC               23-OCT-01 : HB: SWITCH TO NEW MENU
CC               21-NOV-01 : HU: ORBIT COMPARISON IN INERTIAL FRAME
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               13-NOV-02 : HU: SP3C IMPLEMENTED, USE RDPREH, RDPREI
CC               18-Nov-02 : HB: WRITE FIRST FILE NAME IN RES-FILE
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               16-MAR-03 : HU: NEW PARAMETERS FOR HELMTR
CC               23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC               20-MAY-03 : RD: INIT TIME WINWDOW TO (/0D0,1D20/)
CC               11-JUN-03 : HU: USE GSTIME
CC               13-JUN-03 : HU: DEFINE CONSTANTS
CC               14-JUN-03 : HU: DESACTIVATE USE OF GMST2000
CC               06-AUG-03 : HU: NEW CALL FOR GSTIME
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               20-NOV-03 : HU: INITIALIZE IOPT
CC               19-FEB-04 : HU: PARAMETER LIST OF HELMTR CHANGED
CC               24-FEB-04 : DS: WRITE RMS WITH MORE DIGITS
CC               04-JUN-05 : HU: MJD FROM RDPREI IN TWO ARGUMENTS
CC                               INTERFACE FOR HELMTR
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               15-FEB-06 : HU: SUPPORT RSW SYSTEM
CC               07-JUL-05 : HB: USE T_EPOCH FOR GSTIME
CC                               ADD BIAS TO PARAMETER LIST OF SR NUTEFF
CC               01-AUG-05 : HU: EPOCH AS STRUCTURE
CC               08-NOV-05 : HB: LAST EPOCH BUG FIXED IF SECOND FILE HAS
CC                               LARGER SAMPLING THAN FIRST FILE
CC               15-FEB-06 : HU: SUPPORT RSW SYSTEM (2)
CC               27-JUN-06 : HB/HU: HANDLE DIFFERENT TIME TAGS CORRECTLY
CC               06-JUL-06 : HB: CORRECT SOME INDICES
CC               18-JUL-06 : HU: PROBLEM WITH LAST EPOCH SOLVED
CC               20-SEP-06 : RD: MINOR REVISION OF CALLING GETORB
CC               22-NOV-06 : RD: CORRECT END OF A TIME INTERVAL
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON,
CC                               CALL DEFCON WITH PARAMETER
CC               26-MAR-07 : AG/RD LOST CHANGE FROM 20-SEP-07 AGAIN ADDED
CC               04-SEP-08 : DT: CORRECT TIME OF PRE-FILES TO GPS-TIME
CC               24-SEP-08 : HB/LP: COMPUTE TIME DIFFERENCE BETWEEN ORBITS
CC                               WITH MORE SIGNIFICANT DIGITS
CC               19-SEP-08 : DT: USE MAXINT FOR MAXPTS
CC               03-DEC-08 : DT: REMOVE UNUSED VARIABLES VROT, TUT1
CC               29-JUN-09 : DT: CORRECTION TO 04-SEP-08/DT
CC               21-JUL-09 : DT: ADD RMS/COMPONENT TO OUTPUT (CALL TO HELMTR);
CC                               INCREASE DIGITS IN OUTPUT
CC               09-JUL-10 : DT: MERGING OF CHANGES 04-SEP-08 AND 24-SEP-08
CC               22-SEP-10 : RD: SET MAXPTS BACK TO 10000 (INSTEAD OF MAXINT)
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               30-NOV-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, fileNameLength, staNameLength,
     1                    lfnPrt, lfnErr, lfn001, lfnRpr, lfnRp1
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: maxsat, maxint
      USE m_epoch,  ONLY: t_epoch, OPERATOR(.epochToReal.), OPERATOR(+),
     1                    OPERATOR(.realToEpoch.), OPERATOR(-)
      USE d_const,  ONLY: omega,ars
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE s_nuteff
      USE s_dmlmtv
      USE s_dmlmav
      USE s_opnfil
      USE s_prflna
      USE s_poldef
      USE s_pritit
      USE s_sidmat
      USE s_readinpf
      USE s_opnerr
      USE s_rdpreh
      USE s_rdprei
      USE s_prfile
      USE s_helmtr
      USE s_defcon
      USE s_exitrc
      USE s_opnsys
      USE s_jmt
      USE s_orbcin
      USE s_gtflna
      USE s_prceff
      USE s_getorb
      USE f_gstime
      USE f_dgpsut
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IDOF  , IEND1 , IEND2 , IEPOCH, IFIL1 , IFIL2 , IFIRST,
     1          IOPT  , IORSYS, IOSTAT, IOUTL , IRC   , ISAMP , ISAT  ,
     2          ISYST , IUNIT , JJ    , K     , KSAMP , KSAT  , L     ,
     3          LSAT  , MAXPTS, MINCOO, MM    , NEPO  , NFIL  , NFLEF ,
     4          NPOINT, NSAT1 , NSAT2 , NSLIST, IRCSTD, ICRARC, IVEL
C
      REAL*8    AELL  , BASCLK, BASPOS, BELL  , DCOMAX, DD   ,DRELL(3),
     1          DT    , DTIME , DTTAB ,DXELL(3),EQEQUI, GPSUTC,
     2          RMSHLM, SCELL , SZ    , T1    , T2    , TDT   , TEND  ,
     3          TEST  , TFIRST, TIMEND, TIMSTA, TSTART, TUTC  ,
     4          TOSC  , UT1UTC, XPOLE , YPOLE , deltt , tTrf  , t1frac,
     5          t2frac
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C MAXIMUM DIMENSIONS
C ------------------
      PARAMETER(MAXPTS=10000)
ccc      PARAMETER(MAXPTS=maxint)
C
C
      CHARACTER*80 TITLE,TITLE1,TITLE2
      CHARACTER(LEN=fileNameLength) :: FILNAM
      CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: FLNAMS
      CHARACTER(LEN=staNameLength),DIMENSION(MAXPTS)     :: STNAME
      CHARACTER*16 DATUM
      CHARACTER*1  JANEIN,STAFLG(2,MAXPTS),EVTFLG(4,MAXSAT)
      CHARACTER*57 TITLES(4)
      CHARACTER*11 SYST(3)
      CHARACTER*5  DATDES,COOSYS
      CHARACTER*4  AGENCY
      CHARACTER*3  ORBTYP,TIMSYS1,TIMSYS2
      CHARACTER*2  FILTYP
      INTEGER(i4b),DIMENSION(:),POINTER                  :: SATLST
      INTEGER*4    PRTLEV,USESAT,SP13(2),IP(7),IEREC(2)
      INTEGER*4    NUMSV1(MAXSAT),NUMSV2(MAXSAT),STANUM(MAXPTS),
     1             ITYP(MAXPTS)
      INTEGER*4    SATWGT(MAXSAT),ACCPOS(4,MAXSAT),ACCVEL(4,MAXSAT)
      REAL*8       COORD1(3,MAXSAT),COORD2(3,MAXSAT)
      REAL*8       XCOORD(3,MAXPTS),YCOORD(3,MAXPTS)
      REAL*8       PAR(7),RMSPAR(7),RESMX(3)
      REAL*8       PRE(3,3),NUT(3,3),SID(3,3),BIAS(3,3)
      REAL*8       SID1(3,3),SID2(3,3)
      REAL*8       DTSATC(MAXSAT),DDTSAT(MAXSAT)
      REAL*8       VEL1(3,MAXSAT),VEL2(3,MAXSAT),VEL(3,MAXPTS)
      REAL*8       SDEVP(4,MAXSAT),SDEVV(4,MAXSAT)
      REAL*8       CORRP(6,MAXSAT),CORRV(6,MAXSAT),XV(9),ELE(7)
      REAL*8       RMSS(3)
      TYPE(t_epoch) :: TMJD,TTUT1,TTUT2,TTDT,TTDT1,TTDT2
C
C DATA
C ----
      DATA IFIRST/1/
      DATA SYST/'EARTH-FIXED','INERTIAL   ','ORBIT (RSW)'/
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(FLNAMS)
      NULLIFY(SATLST)
      CALL init_inpkey(inpKey)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL readinpf(' ',inpKey)
C
C OPEN SYSOUT
C -----------
      CALL OPNSYS
C
C DEFINE CONSTANTS
C ----------------
      CALL DEFCON(1)

! Write title and file list
! -------------------------
      CALL pritit('ORBCMP','Compare precise orbits')
      CALL prflna
      CALL prfile ('PREINP','PRECISE FILES',1)
C
C READ INPUT PARAMETERS
C ---------------------
      CALL ORBCIN(flNams,TITLE,USESAT,ISAMP,NSLIST,SATLST,TSTART,TEND,
     1            IP,DCOMAX,MINCOO,PRTLEV,ISYST)
C
C UNITS FOR THE OUTPUT ARE: IUNIT: 1 = METER / 2 = MILLIMETER
      iunit=1
C      iunit=2
      ioutl=0
      iopt =0

      IF (ISYST.EQ.1) WRITE(lfnprt,"(/,' Comparison in ',
     1                                  'Earth-Fixed System',/)")
      IF (ISYST.EQ.2) WRITE(lfnprt,"(/,' Comparison in ',
     1                                  'Inertial System',/)")
      IF (ISYST.EQ.3) WRITE(lfnprt,"(/,' Comparison in ',
     1                                  'Orbit System (RSW)',/)")
      IOPT=0
      IF (ISYST.EQ.3) IOPT=3

      WRITE(lfnPrt,712)TRIM(TITLE)
712   FORMAT(1X,A,/,1X,79('-'),/)
      WRITE(lfnPrt,711)
711   FORMAT('   T0    DT     DX      DY      DZ   ',
     1       '    RX        RY        RZ         SCALE    RMS ',
     2       '    RMS1   RMS2   RMS3')
C
C OPEN OUTPUT FILE 1
C ------------------
      IF(PRTLEV.GT.0)THEN
        CALL GTFLNA(1,'OUTPUT ',FILNAM,IRC)
        CALL OPNFIL(LFN001,FILNAM,'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN001,IOSTAT,FILNAM,'ORBCMP')
        WRITE(LFN001,"(' RESIDUALS IN ',A,' SYSTEM',/)")
     1                                 TRIM(SYST(ISYST))
      END IF

! Number of Files
! ---------------
      nFil = SIZE(flNams,2)

! Standard orbit available for IOPT=3
      CALL GTFLNA(0,'STDORB ',FILNAM,IRCSTD)
C
C LOOP OVER FIRST FILES
      IF (NFIL.GT.25) THEN
        NFLEF=2
      ELSE
        NFLEF=NFIL
      ENDIF
      DO 3000 IFIL1=1,NFLEF-1
        TITLE1=FLNAMS(1,IFIL1)
C
C OPEN ORBIT INPUT FILE 1
C -----------------------
C
C LOOP OVER SECOND FILES
        DO 2000 IFIL2=IFIL1+1,NFIL
          CALL RDPREH(FLNAMS(1,IFIL1),LFNRPR,SP13(1),NSAT1,NUMSV1,
     1                SATWGT,TFIRST,NEPO,DTTAB,TITLES,DATDES,COOSYS,
     2                ORBTYP,AGENCY,FILTYP,TIMSYS1,BASPOS,BASCLK)
          CALL RDPREH(FLNAMS(1,IFIL2),LFNRP1,SP13(2),NSAT2,NUMSV2,
     1                SATWGT,TFIRST,NEPO,DTTAB,TITLES,DATDES,COOSYS,
     2                ORBTYP,AGENCY,FILTYP,TIMSYS2,BASPOS,BASCLK)
          TITLE2=FLNAMS(1,IFIL2)
C
C NO VELOCITIES
          IF (ISYST.EQ.3.AND.MOD(SP13(1),2).NE.1
     1                  .AND.MOD(SP13(2),2).NE.1.
     2                  .AND.IRCSTD.EQ.1) THEN
            WRITE(LFNERR,"(' ### PG ORBCMP: No velocities,',
     1                      ' comparison in RSW not possible',/,
     2                  16X,'File1: ',A,/,
     3                  16X,'File2: ',A)")
     4                  TRIM(FLNAMS(1,IFIL1)),TRIM(FLNAMS(1,IFIL2))
            CLOSE(UNIT=LFNRP1)
            CLOSE(UNIT=LFNRPR)
            CYCLE
          ENDIF
C
C INIT SOME VARIABLES
C -------------------
          T1=0.D0
          IEND1=0
          T2=0.D0
          IEND2=0

          DT=0.5D0/86400.D0

          NPOINT=0
C
C ASSEMBLE ALL DATA POINTS
C ------------------------
          DO 1000 IEPOCH=1,1000000
            DO 100 KSAMP=1,ISAMP
C
C LOOK FOR NEXT EPOCH IN FIRST FILE
C ---------------------------------
10          IF(T1+DT.GT.TEND.AND.TEND.NE.1.D20)THEN
              TIMEND=T1
              GO TO 1010
            ENDIF
            CALL RDPREI(LFNRPR,SP13(1),0,NSAT1,NUMSV1,TMJD,COORD1,VEL1,
     1                  DTSATC,DDTSAT,ACCPOS,ACCVEL,EVTFLG,IEREC,
     2                  SDEVP,SDEVV,CORRP,CORRV,IEND1)
            IF (IEND1.EQ.1) GO TO 1010
            TTUT1 =TMJD
            TTDT1 =TMJD
            T1=.epochToReal.TMJD
C
C Correct for time system -> GPS-time
C -----------------------------------
            IF(TIMSYS1.EQ.'UTC' .OR. TIMSYS1.EQ.'GLO') THEN
              gpsutc = dgpsut(T1)/86400.D0
              TMJD = TMJD + gpsutc
            ELSEIF(TIMSYS1.EQ.'TAI') THEN
              TMJD = TMJD - 19.D0/86400.D0
            ENDIF
C
            T1 = .epochToReal.TMJD
            t1frac = tmjd%frac
            TIMEND = T1
C
C CHECK EPOCH
C -----------
            IF(T1+DT.LT.T2.OR.(T1+DT.LT.TSTART.AND.TSTART.NE.0.D0))THEN
              GO TO 10
            ELSE
              IF (IFIRST.EQ.1) THEN
                TIMSTA=T1
                IFIRST=0
              ENDIF
            ENDIF
C
C CORRESPONDING EPOCH IN SECOND FILE
C ----------------------------------
20          IF(IEND2.EQ.1)GO TO 1010
            IF(T2+DT.LT.T1)THEN
              CALL RDPREI(LFNRP1,SP13(2),0,NSAT2,NUMSV2,TMJD,COORD2,
     1                    VEL2,DTSATC,DDTSAT,ACCPOS,ACCVEL,EVTFLG,IEREC,
     2                    SDEVP,SDEVV,CORRP,CORRV,IEND2)
              IF (IEND2.EQ.1) GO TO 1010
              TTUT2 =TMJD
              TTDT2 =TMJD
              T2=.epochToReal.TMJD
              t2frac=tmjd%frac
            END IF
C
C Correct for time system -> GPS-time
C -----------------------------------
            IF(TIMSYS2.EQ.'UTC' .OR. TIMSYS2.EQ.'GLO') THEN
              gpsutc = dgpsut(T2)/86400.D0
              TMJD = TMJD + gpsutc
            ELSEIF(TIMSYS2.EQ.'TAI') THEN
              TMJD = TMJD - 19.D0/86400.D0
            ENDIF
C
            T2 = .epochToReal.TMJD
            t2frac = tmjd%frac
C
C CHECK EPOCH
C -----------
            IF(T2+DT.LT.T1)THEN
              GO TO 20
            ELSE IF(T2-DT.GT.T1)THEN
              GO TO 10
            ELSE
              deltt=(t2frac-t1frac)*86400.D0

              IF(KSAMP.EQ.ISAMP)THEN
C
C DIFFERENT TIME TAGS: USE VELOCITY
C ---------------------------------
                IVEL=0
                tTrf=t2
                ttdt=ttdt2
                  IF (IRCSTD.EQ.0) THEN
                    IVEL=-1                             ! first file, std
                    tTrf=t2
                    ttdt=ttdt2
                  ELSEIF (MOD(SP13(1),2).EQ.1) THEN
                    IVEL= 1                             ! vel from first file
                    tTrf=t2
                    ttdt=ttdt2
                  ELSEIF (MOD(SP13(2),2).EQ.1) THEN
                    IVEL= 2                             ! vel from second file
                    tTrf=t1
                    ttdt=ttdt1
                  ENDIF
C
C COMPUTE TRANSFORMATION MATRICES IF NECESSARY
C --------------------------------------------
                IF (IVEL.LT.0 .OR. ISYST.GT.1) THEN
C ..INITIALIZE
                  CALL POLDEF(tTrf,1,XPOLE,YPOLE,UT1UTC,GPSUTC)
C ..POLE-INFORMATION (TIME ARGUMENT: UTC)
                  TUTC=tTrf-GPSUTC
                  CALL POLDEF(TUTC,1,XPOLE,YPOLE,UT1UTC,GPSUTC)
C ..PRECESSION AND NUTATION (TIME ARGUMENT: TDB, APPROX. AS TDT)
                  TDT=tTrf+(19.D0+32.184D0)/86400.D0
                  CALL PRCEFF(IORSYS,5.D0,TDT,PRE)
                  CALL NUTEFF(IORSYS,0.1D0,TDT,NUT,EQEQUI,BIAS)
                  PRE = matmul(PRE,BIAS)
C ..SIDERIAL TIME (TIME ARGUMENT: UT1)
                  TTDT%frac =ttdt%frac +(19.D0+32.184D0)/86400.D0
CCCC
CCCC                  TTUT1%frac =ttut1%frac -GPSUTC+UT1UTC
                  TTUT1%frac =ttut1%frac +UT1UTC
                  IF (TIMSYS1.NE.'UTC' .AND. TIMSYS1.NE.'GLO') THEN
                    TTUT1%frac =ttut1%frac -GPSUTC
                  ENDIF
CCCC
                  SZ=GSTIME(0,TTUT1,TTDT,NUT(2,1),EQEQUI)
                  CALL sidmat(tdt,xpole,ypole,sz,sid1)
C
CCCC                  TTUT2%frac =ttut2%frac -GPSUTC+UT1UTC
                  TTUT2%frac =ttut2%frac +UT1UTC
                  IF (TIMSYS2.NE.'UTC' .AND. TIMSYS2.NE.'GLO') THEN
                    TTUT2%frac =ttut2%frac -GPSUTC
                  ENDIF
CCCC
                  SZ=GSTIME(0,TTUT2,TTDT,NUT(2,1),EQEQUI)
                  CALL sidmat(tdt,xpole,ypole,sz,sid2)
C
                  IF (IVEL.EQ.2) THEN
                    SID = SID1
                  ELSE
                    SID = SID2
                  ENDIF
                ENDIF
C
C LOOP OVER SATELLITES
C --------------------
                DO 90 ISAT=1,NSAT1
                  DO 30 KSAT=1,NSAT2
                    IF(NUMSV2(KSAT).EQ.NUMSV1(ISAT))GO TO 40
30                CONTINUE
                  GO TO 90
40                CONTINUE
                  JANEIN='N'
                  DO 45 LSAT=1,NSLIST
                    IF(NUMSV1(ISAT).EQ.SATLST(LSAT))JANEIN='Y'
45                CONTINUE
                  IF(USESAT.EQ.0.AND.JANEIN.EQ.'Y')GO TO 90
                  IF(USESAT.EQ.1.AND.JANEIN.EQ.'N')GO TO 90
C
C DO NOT TRANSFORM INVALID POSITIONS
C ----------------------------------
                  IF (COORD1(1,ISAT).EQ.0D0.AND.
     1                COORD1(2,ISAT).EQ.0D0.AND.
     2                COORD1(3,ISAT).EQ.0D0) GOTO 90
                  IF (COORD2(1,KSAT).EQ.0D0.AND.
     1                COORD2(2,KSAT).EQ.0D0.AND.
     2                COORD2(3,KSAT).EQ.0D0) GOTO 90
C
C APPLY VELOCITY
C --------------
C ..FROM FIRST FILE
                  IF (IVEL.EQ.1) THEN
                    coord1(1:3,isat)=coord1(1:3,isat)+
     1                               vel1(1:3,isat)*deltt
C ..FROM SECOND FILE
                  ELSEIF (IVEL.EQ.2) THEN
                    coord2(1:3,ksat)=coord2(1:3,ksat)-
     1                               vel2(1:3,ksat)*deltt
C ..FROM STANDARD ORBIT
                  ELSEIF (IVEL.LT.0) THEN
                    CALL GETORB(NUMSV1(ISAT),0,2,1,T1,ICRARC,
     1                          IORSYS,XV,TOSC,ELE,IRC)
                    IF (IRC.NE.0) GOTO 90
                    CALL DMLMTV(COORD1(1,ISAT),SID1,COORD1(1,ISAT))
                    CALL DMLMTV(COORD1(1,ISAT),NUT,COORD1(1,ISAT))
                    CALL DMLMTV(COORD1(1,ISAT),PRE,COORD1(1,ISAT))
C
                    coord1(1:3,isat)=coord1(1:3,isat)+
     1                               xv(4:6)*deltt+(xv(7:9)/2)*deltt**2
                    vel1(1:3,isat)=xv(4:6)+xv(7:9)*deltt
                    CALL DMLMAV(VEL1(1,ISAT),SID1,VEL1(1,ISAT))
                    CALL DMLMTV(VEL1(1,ISAT),SID2,VEL1(1,ISAT))
C
                    IF (ISYST.EQ.1) THEN
                      CALL DMLMAV(COORD1(1,ISAT),PRE,COORD1(1,ISAT))
                      CALL DMLMAV(COORD1(1,ISAT),NUT,COORD1(1,ISAT))
                      CALL DMLMAV(COORD1(1,ISAT),SID2,COORD1(1,ISAT))
                    ENDIF
                  ENDIF
C
C TRANSFORM COORDINATES INTO J2000
C --------------------------------
                  IF (ISYST.GT.1) THEN
                    IF (IVEL.GE.0) THEN
                      CALL DMLMTV(COORD1(1,ISAT),SID,COORD1(1,ISAT))
                      CALL DMLMTV(COORD1(1,ISAT),NUT,COORD1(1,ISAT))
                      CALL DMLMTV(COORD1(1,ISAT),PRE,COORD1(1,ISAT))
                      IF (IVEL.EQ.1) THEN
                        CALL DMLMTV(VEL1(1,ISAT),SID,VEL1(1,ISAT))
                        CALL DMLMTV(VEL1(1,ISAT),NUT,VEL1(1,ISAT))
                        CALL DMLMTV(VEL1(1,ISAT),PRE,VEL1(1,ISAT))
                      ELSEIF (IVEL.EQ.2) THEN
                        CALL DMLMTV(VEL2(1,KSAT),SID,VEL2(1,KSAT))
                        CALL DMLMTV(VEL2(1,KSAT),NUT,VEL2(1,KSAT))
                        CALL DMLMTV(VEL2(1,KSAT),PRE,VEL2(1,KSAT))
                      ENDIF
                    ENDIF
C
                    CALL DMLMTV(COORD2(1,KSAT),SID,COORD2(1,KSAT))
                    CALL DMLMTV(COORD2(1,KSAT),NUT,COORD2(1,KSAT))
                    CALL DMLMTV(COORD2(1,KSAT),PRE,COORD2(1,KSAT))
                  ENDIF
C
                  DO 46 L=1,3
                    TEST=DABS(COORD1(L,ISAT)-COORD2(L,KSAT))
                    IF(TEST.GT.DCOMAX .AND. DCOMAX.GT.0.0)THEN
                      CALL JMT(T1,JJ,MM,DD)
                      WRITE(LFNERR,451)FLNAMS(1,IFIL1),FLNAMS(1,IFIL2),
     1                                 JJ,MM,DD,NUMSV1(ISAT)
451                   FORMAT(' ### PGM ORBCMP : FILES :',2A32,/,
     1                '                : MAX COO DIFF EXCEEDED:',/,
     2                '                  TIME                 :',I4,I3,
     3                F12.6,/,
     4                '                  SATELLITE            :',I3)
                      GO TO 90
                    END IF
46                CONTINUE
                  NPOINT=NPOINT+1
                  IF (NPOINT.GT.MAXPTS) THEN
                    WRITE(LFNERR,901)MAXPTS
901                 FORMAT(/,' *** PG ORBCMP: TOO MANY DATA POINTS',
     1                     /,'                MAXPTS:',I8,/)
                    CALL EXITRC(2)
                  ENDIF
                  ITYP(NPOINT)=0
                  STANUM(NPOINT)=NUMSV1(ISAT)
ccc               STNAME(NPOINT)='SATELLITE'
                  WRITE(STNAME(NPOINT),"(F15.9)")T1
                  STAFLG(1,NPOINT)='P'
                  STAFLG(2,NPOINT)='P'
CCC                  WRITE(*,41)NPOINT,NUMSV1(ISAT),T1,
CCC     1                 ((COORD1(L,ISAT)-COORD2(L,KSAT)),L=1,3)
CCC41                FORMAT(' POINT=',I5,' SVN=',I3,' T=',F10.3,' DD=',
CCC     1                   3F10.3)
                  DO 50 K=1,3
                    XCOORD(K,NPOINT)=COORD1(K,ISAT)
                    YCOORD(K,NPOINT)=COORD2(K,KSAT)
                    IF (ISYST.GT.1) THEN
                      IF (MOD(SP13(1),2).EQ.1.OR.IRCSTD.EQ.0) THEN
                        VEL(K,NPOINT)=VEL1(K,ISAT)
                      ELSE
                        VEL(K,NPOINT)=VEL2(K,KSAT)
                      ENDIF
                    ENDIF
50                CONTINUE
90              CONTINUE
              END IF
            END IF
100         CONTINUE
1000      CONTINUE
1010      CONTINUE
C
C PERFORM HELMERT TRANSFORMATION
C ------------------------------
          IF(NPOINT.GE.MINCOO)THEN
            CALL HELMTR(TITLE1,TITLE2,NPOINT,STNAME,STANUM,STAFLG,
     1                  XCOORD,YCOORD,VEL,ITYP,IP,IOPT,IUNIT,
     2                  DATUM,AELL,BELL,DXELL,DRELL,SCELL,LFN001,
     3                  PRTLEV,IOUTL,RESMX,PAR,RMSPAR,RMSHLM,IDOF,RMSS)
            PAR(4)=PAR(4)*ars
            PAR(5)=PAR(5)*ars
            PAR(6)=PAR(6)*ars
            PAR(7)=(PAR(7)-1)*1D6
            DTIME=TIMEND-TIMSTA
            IF (DABS(DTIME).GE.10) DTIME=1.0D0
            WRITE(lfnPrt,71)TIMSTA,DTIME,
     1                      (PAR(K),K=1,7),RMSHLM,(RMSS(K),K=1,3),
     2                       FLNAMS(1,IFIL1),FLNAMS(1,IFIL2)

71          FORMAT(F8.1,F4.1, 3(1X,F7.4), 1X,3F10.6, 1X,F9.5, F8.4,
     1             1X, 3(1X,F6.4), 2X,2(1X,A32))

            RMSPAR(4)=RMSPAR(4)*ars
            RMSPAR(5)=RMSPAR(5)*ars
            RMSPAR(6)=RMSPAR(6)*ars
            RMSPAR(7)=RMSPAR(7)*1D6
          ELSE
            WRITE(LFNERR,452)FLNAMS(1,IFIL1),FLNAMS(1,IFIL2)
452         FORMAT(' *** PGM ORBCMP : FILES :',2A32,/,
     1             '                : NOT ENOUGH SAT. POS.',/)
          END IF
C
C CLOSE CURRENT SECOND OBSERVATION FILE
C -------------------------------------
          CLOSE(UNIT=LFNRP1)
          CLOSE(UNIT=LFNRPR)
2000    CONTINUE
        WRITE(lfnPrt,*)
C
C CLOSE CURRENT FIRST OBSERVATION FILE
C ------------------------------------
3000  CONTINUE
C
      CALL EXITRC(0)
      END

