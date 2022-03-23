      MODULE s_UPDRNX
      CONTAINS

C*
      SUBROUTINE UPDRNX(MAXREC,MAXCMB,MAXSAT,MAXARC,MAXSLP,IPRFLG,
     1                  IFLCOD,IFLPHS,DTSIM ,ISAT  ,SVN   ,NEPOCH,
     2                  OBSTIM,NEVT  ,TIMEVT,SIZEVT,SYSEVT,OBSREC,
     3                  OBSFLG,NOBS  ,LINTIM,LINFLG,NARC  ,ARCTIM,
     4                  ARCOFF,SATRMS,NSLIP ,NOBA  ,NBAD  ,SLPTIM,
     5                  SLPCYC)
CC
CC NAME       :  UPDRNX
CC
CC PURPOSE    :  UPDATE OBSERVATION RECORDS OF CURRENT SATELLITE
CC               PRINT SATELLITE SPECIFIC SUMMARY
CC
CC PARAMETERS :
CC        IN  :  MAXREC : MAXIMUM NUMBER OF RECORDS           I*4
CC               MAXCMB : MAXIMUM NUMBER OF OBS. TYPES        I*4
CC               MAXSAT : MAXIMUM NUMBER OF SATELLITES        I*4
CC               MAXARC : MAXIMUM NUMBER OF ARCS              I*4
CC               MAXSLP : MAXIMUM NUMBER OF SLIPS             I*4
CC               IPRFLG : PRINT FLAG (0=SUMM 1=ALL)           I*4
CC               IFLCOD : USE RAW OR SMOOTH CODE : 0= RAW     I*4
CC                                                 1= SMOOTH
CC               IFLPHS : FLAG BAD PHASE  OR NOT (0/1)        I*4
CC               DTSIM  : TOLERANCE FOR ONE EPOCH (IN DAYS)   R*8
CC               ISAT   : REQUESTED SATELLITE                 I*4
CC               SVN    : SATELLITE NUMBER                    I*4
CC               NEPOCH : NUMBER OF POINTS IN OBS. ARRAYS     I*4
CC               OBSTIM : ARRAY WITH TIME VALUES (DAYS)       R*8(*)
CC               NEVT   : NUMBER OF CLOCK EVENTS              I*4
CC               TIMEVT : EPOCH OF CLOCK EVENT                R*8(*)
CC                        (TIME IN HOURS SINCE DNINT(OBSTIM(1))
CC               SIZEVT : SIZE OF THE EVENT IN METERS         R*8(*)
CC                        (1D20 IF UNKNOWN SIZE)
CC               SYSEVT : SATELLITE SYSTEM CHARACTER FROM     CH*1(*)
CC                        G_SVNSYS
CC     IN/OUT:   OBSREC : ARRAY WITH OBSERVATIONS             R*8(*,*,*)
CC               OBSFLG : ARRAY WITH OBSERVATION FLAGS       CH*1(*,*,*)
CC                        BIT 0: OUTLIER
CC                        BIT 1: CYCLE SLIP (UNREPAIRED)
CC               NOBS   : ACTUAL NUMBER OF POINTS FOR SAT.    I*4
CC               LINTIM : OBSERVATION TIME (HOURS)            R*8(NOBS)
CC               LINFLG : OBSERVATION FLAG                   CH*1(NOBS)
CC                        BIT=0: OUTLIER
CC                        BIT=1: NEW AMBIGUITY
CC                        BIT=2: CYCLE SLIP CORRECTED
CC               NARC   : NUMBER OF "ARCS" FOR THIS SATELLITE I*4
CC               ARCTIM : BEGIN AND END TIMES OF THE ARCS     I*4(*,2)
CC                        INDICES OF THE LINTIM ARRAY
CC               SATRMS : RMS OF MELWUB AND L3-P3             I*4(2)
CC               NSLIP  : NUMBER OF CYCLE SLIPS IN ARC        I*4(*)
CC               NOBA   : NUMBER OF OBSERVATIONS IN ARC       I*4(*)
CC               NBAD   : NUMBER OF BAD OBSERVATIONS IN ARC   I*4(*)
CC               SLPTIM : EPOCH OF CYCLE SLIP                 I*8(NSLIP(IARC))
CC               SLPCYC : SIZE OF THE CYCLE SPLIP             R*8(*,*,5)
CC
CC REMARKS    :  FACTORS VALID IF INPUT OBSERVATIONS IN METERS (NOT CYCLES)
CC
CC AUTHOR     :  T.A. SPRINGER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  22-JUL-1996
CC
CC CHANGES    :  31-AUG-98 : DI: USE 'COMFREQ' (FOR GLONASS SATELLITES)
CC               29-APR-04 : RD: IFLPHS=COPY ORIGINAL PHASE FROM INPUT
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               18-MAY-11 : RD: ADD GLONASS FREQUENCY ESTIMATION AS AN OPTION
CC               28-MAR-12 : RD: USE SVN2CHR AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern,  ONLY: LFNPRT
      USE f_tstflg
      USE s_svn2chr
      USE s_clrflg
      USE s_setflg
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IA    , IARC  , IAT   , IEPO  , IEVT  , IFLCOD,
     1          IFLPHS, IFRQ  , IPRFLG, IS    , ISAT  , ISLP  , J     ,
     2          K     , MAXARC, MAXCMB, MAXREC, MAXSAT, MAXSLP, NARC  ,
     3          NEPOCH, NEVT  , NOBS
C
      REAL*8    CYCL1 , CYCL2 , DIONT , DTSIM , OBSEPO, OBSL1 , OBSL2 ,
     1          OBSOBS, OFF1  , OFF2  , STIM  , TSTTIM
C
CCC       IMPLICIT  REAL*8(A-H,O-Z)
C
      REAL*8      OBSTIM(MAXREC),OBSREC(MAXREC,MAXCMB,MAXSAT)
      REAL*8      LINTIM(MAXREC)
      REAL*8      ARCOFF(MAXARC,5),SATRMS(*)
      REAL*8      SLPCYC(MAXSLP,MAXARC,5)
      REAL*8      TIMEVT(*),SIZEVT(*)
C
      INTEGER*4   ARCTIM(MAXARC,2),NOBA(MAXARC),NBAD(MAXARC)
      INTEGER*4   SLPTIM(MAXSLP,MAXARC),NSLIP(MAXARC),SVN
      INTEGER*4   SATNUM
C
      CHARACTER*3 STRCSL
      CHARACTER*1 OBSFLG(MAXREC,MAXCMB,MAXSAT),LINFLG(MAXREC)
      CHARACTER*1 SYSEVT(*),SATCHR
C
      INCLUDE   'COMFREQ.inc'
C
C WRITE SATELLITE SUMMARY IF REQUESTED
C ------------------------------------
      IF (IPRFLG.EQ.1) THEN
        WRITE(LFNPRT,12)SVN,NOBS,NARC,SATRMS(1),SATRMS(2)
12      FORMAT(1X,78('-'),/,
     1         1X,'PRN:',I3,2X,'NOBS: ',I4,2X,'NARC: ',I2,
     2         2X,'RMS MELWUB(cyc): ',F5.2,2X,'RMS L3-P3(m): ',F5.2,/,
     3         1X,78('-'),/,
     4         4X,' ARC   OBS   BAD                  SLIPS',
     5        14X,'OFFSETS (L1, L2)',/,
     6        44X,'TIME',11X,'SIZES (L4, L5)',/,
     7        59X,'SIZES (L1, L2)',/,1X,78('-'))
        DO IARC=1,NARC
          IF (NOBA(IARC)-NBAD(IARC).EQ.0) THEN
            WRITE(LFNPRT,14)IARC,NOBA(IARC),NBAD(IARC)
14          FORMAT(2X,3(2X,I4),29X,2(14X,'-'))
          ELSE
            WRITE(LFNPRT,15)IARC,NOBA(IARC),NBAD(IARC),NSLIP(IARC),
     1                      ARCOFF(IARC,1),ARCOFF(IARC,2)
15          FORMAT(2X,3(2X,I4),18X,I4,8X,2(F14.2,1X))
          ENDIF
          DO ISLP=1,NSLIP(IARC)
            IA=ARCTIM(IARC,1)
            IS=IA+SLPTIM(ISLP,IARC)-1
            STIM=LINTIM(IS)
            STRCSL='BAD'
            IF (TSTFLG(LINFLG(IS),2)) STRCSL='FIX'
            IF (TSTFLG(LINFLG(IS),1)) STRCSL='SLP'
            WRITE(LFNPRT,18)STRCSL,ISLP,STIM,
     1                      SLPCYC(ISLP,IARC,4),SLPCYC(ISLP,IARC,5),
     2                      SLPCYC(ISLP,IARC,1),SLPCYC(ISLP,IARC,2)
18          FORMAT(34X,A3,1X,I4,2X,F6.3,2F14.3,/,50X,2F14.3)
          ENDDO
        ENDDO
      ENDIF
C
C UPDATE THE OBSERVATION ARRAY
C ============================
C
C LOOP OVER ALL CORRECTED OBSERVATIONS
C ------------------------------------
      DO 100 I=1,NOBS
C
C FIND CORRESPONDING ORIGINAL OBSERVATION
C ---------------------------------------
        DO 200 J=I,NEPOCH
C
C CHECK IF EPOCHS ARE CORRESPONDING
C ---------------------------------
          OBSEPO=(OBSTIM(J)-DNINT(OBSTIM(1)))*24D0
          IF (OBSEPO.EQ.LINTIM(I)) THEN
C
C GET CORRECT ARC
C ---------------
            IARC=1
            DO 10 K=1,NARC
              IF ((OBSEPO.GE.LINTIM(ARCTIM(K,1))) .AND.
     1            (OBSEPO.LE.LINTIM(ARCTIM(K,2)))) THEN
                IARC=K
                GOTO 11
              ENDIF
10          CONTINUE
11          CONTINUE
C
C CORRECT FOR CYCLE SLIPS
C -----------------------
            CYCL1=0D0
            CYCL2=0D0
            DO 20 K=1,NSLIP(IARC)
              IAT=ARCTIM(IARC,1)
              TSTTIM=LINTIM(IAT+SLPTIM(K,IARC)-1)
              IF (OBSEPO.GE.TSTTIM) THEN
                CYCL1=CYCL1+SLPCYC(K,IARC,1)*WLGT(1,SVN)
                CYCL2=CYCL2+SLPCYC(K,IARC,2)*WLGT(2,SVN)
              ENDIF
20          CONTINUE
C
C GENERATE THE SMOOTHED CODE: WE HAVE THE L1-P1, L2-P2 AND L4 MEAN OFFSEST
C ------------------------------------------------------------------------
            IF (IFLCOD.EQ.1) THEN
              OBSL1=OBSREC(J,1,ISAT)+ARCOFF(IARC,1)-CYCL1
              OBSL2=OBSREC(J,2,ISAT)+ARCOFF(IARC,2)-CYCL2
              DIONT=-(OBSL1-OBSL2)
              OBSREC(J,3,ISAT)=OBSL1+2D0*FACLIN(3,2,SVN)*DIONT
              OBSREC(J,4,ISAT)=OBSL2-2D0*FACLIN(3,1,SVN)*DIONT
            ENDIF
C
C UPDATE THE PHASE OBSERVATIONS OF THIS EPOCH
C -------------------------------------------
            IF (IFLPHS.EQ.1) THEN
              OFF1=DNINT(ARCOFF(IARC,1)/WLGT(1,SVN))*WLGT(1,SVN)
              OFF2=DNINT(ARCOFF(IARC,2)/WLGT(2,SVN))*WLGT(2,SVN)
              OBSL1=OBSREC(J,1,ISAT)+OFF1-CYCL1
              OBSL2=OBSREC(J,2,ISAT)+OFF2-CYCL2
            ELSE
              OBSL1=OBSREC(J,1,ISAT)
              OBSL2=OBSREC(J,2,ISAT)
C UNDO MS-JUMPS
              DO IEVT=1,NEVT
                IF (SIZEVT(IEVT).EQ.1D20) CYCLE
                IF (OBSEPO.LT.TIMEVT(IEVT)-DTSIM) CYCLE
                CALL SVN2CHR(SVN,SATNUM,SATCHR)
                IF (SYSEVT(IEVT).NE.'*'.AND.
     1              SYSEVT(IEVT).NE.SATCHR) CYCLE
                OBSL1=OBSL1-SIZEVT(IEVT)
                OBSL2=OBSL2-SIZEVT(IEVT)
              ENDDO
            ENDIF
            OBSREC(J,1,ISAT)=OBSL1/WLGT(1,SVN)
            OBSREC(J,2,ISAT)=OBSL2/WLGT(2,SVN)
ccc            OBSREC(J,1,ISAT)=OBSL1
ccc            OBSREC(J,2,ISAT)=OBSL2
C
C COPY LINFLG TO OBSFLG
C ---------------------
            IF ((TSTFLG(LINFLG(I),0))  .AND.
     1          (TSTFLG(LINFLG(I),1))) THEN
               CALL CLRFLG(LINFLG(I),1)
               CALL SETFLG(LINFLG(I+1),1)
            ENDIF
            OBSFLG(J,1,ISAT)=LINFLG(I)
C
C NEXT OBSERVATION
C ----------------
            GOTO 100
          ENDIF
C
C OLD OBS. LOOP
C -------------
200     CONTINUE
C
C NEW OBS. LOOP
C -------------
100   CONTINUE
C
C NO PREPROCESSING OF PHASE, COPY ONLY INPUT
C ------------------------------------------
      IF (IFLPHS.NE.1) THEN
        DO IEPO=1,NEPOCH
C
          IF (OBSREC(IEPO,3,ISAT).EQ.0D0.OR.
     1        OBSREC(IEPO,4,ISAT).EQ.0D0) THEN
            DO IFRQ=1,2
              IF (OBSREC(IEPO,IFRQ,ISAT).EQ.0D0) CYCLE
C
              OBSOBS=OBSREC(IEPO,IFRQ,ISAT)
C UNDO MS-JUMPS
              OBSEPO=(OBSTIM(IEPO)-DNINT(OBSTIM(1)))*24D0
              DO IEVT=1,NEVT
                IF (SIZEVT(IEVT).EQ.1D20) CYCLE
                IF (OBSEPO.LT.TIMEVT(IEVT)-DTSIM) CYCLE
                CALL SVN2CHR(SVN,SATNUM,SATCHR)
                IF (SYSEVT(IEVT).NE.'*'.AND.
     1              SYSEVT(IEVT).NE.SATCHR) CYCLE
                OBSOBS=OBSOBS-SIZEVT(IEVT)
              ENDDO
C
              OBSREC(IEPO,IFRQ,ISAT)=OBSOBS/WLGT(IFRQ,SVN)
            ENDDO
          ENDIF
        ENDDO
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
