C*
      PROGRAM STDELE
CC
CC NAME       :  STDELE
CC
CC PURPOSE    :  COMPUTE THE DIFFERENCES OF THE OSCULATING ELEMENTS FROM
CC               TWO STANDARD ORBIT FILES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  L.MERVART
CC
CC CREATED    :  06-APR-95
CC
CC CHANGES    :  25-APR-95 : LM: DISTINGUISH THE ECLIPSING SATELLITES
CC               19-JUN-95 : TS: RADIAL, ALONG AND CROSS TRACK RESIDUALS
CC               29-SEP-95 : JJ: DECLARE HELP1 AS I*4 INSTEAD OF R*8
CC               29-SEP-95 : JJ: DECLARE HELP2 AS I*4 INSTEAD OF R*8
CC               19-AUG-96 : TS: REVERSE ORDER OF "GETORF" CALLS TO AVOID
CC                               PROBLEMS WITH FILORB=FILOLD IN GETORF
CC               23-SEP-97 : TS: ADDED "MEAN" VALUE OUTPUT
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               22-AUG-00 : HB: CHANGE CALL OF POLDF1 (ADD ISUBFL)
CC               14-NOV-00 : RD: SWITCH TO THE NEW MENU SYSTEM
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               03-MAR-03 : SC: ADD TITLE SECTION
CC               23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC               11-JUN-03 : HU: USE GSTIME
CC               14-JUN-03 : HU: DESACTIVATE USE OF GMST2000
CC               06-AUG-03 : HU: NEW CALL GSTIME
CC               29-AUG-03 : HU: ORDER INPUT FILES, OUTPUT SUMMARY MODIFIED
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: LFNUM.inc, COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               07-JUL-05 : HB: USE T_EPOCH FOR GSTIME
CC                               ADD BIAS TO PARAMETER LIST OF SR NUTEFF
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               04-MAY-08 : RD: SVN ADDED TO CALL OF SR XYZELE
CC               03-SEP-08 : DT: MEAN VALUES FOR SLR SATELLITES
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               30-AUG-11 : RD: REMARK ON UNITS ADDED
CC               01-DEC-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC               25-JAN-12 : LP: Warning instead of error in case of too few files
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1995     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: keyValueLength, lfnPrt, lfnErr, lfnOr1
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: maxsat
      USE m_epoch,  ONLY: t_epoch, OPERATOR(.realToEpoch.)
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_stdorb, ONLY: t_stdhead, init_stdHead
      USE d_const, ONLY: GM
C
      USE s_nuteff
      USE s_dordup
      USE s_prflna
      USE s_rdstdh
      USE s_gtfile
      USE s_pritit
      USE s_sidmat
      USE s_readinpf
      USE s_getorf
      USE s_shdbnd
      USE s_dminv
      USE s_readkeys
      USE s_getarc
      USE s_defcon
      USE s_exitrc
      USE s_dmlmam
      USE s_opnsys
      USE s_poldf1
      USE s_xyzele
      USE s_resorb
      USE s_gtflna
      USE s_prceff
      USE s_dmlmav
      USE f_gstime
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IANT1 , IANT2 , IARC  , ICRAR1, ICRAR2, IDER  ,
     1          IFIL  , II    , INDSHD, IORSY1, IORSY2, IRC   , IRCODE,
     2          ISAT1 , ISAT2 , ISTOP , MAXARC, MAXFIL, MXCARC, MXCFIL,
     3          MXCSAT, NAR1  , NAR2  , NFIL  , NFLCOL
C
      REAL*8    ANE1  , ANE2  , ANEOLD, ANM1  , ANM2  , ANT1  , ANT2  ,
     1          DEPS  , DUMMY , EPOCH , EQEQU1, EQEQU2, GPUTC1, GPUTC2,
     2          RADK  , SZ1   , SZ2   , TDT   , TOSC1 , TOSC2 ,
     3          TUT1  , TUT2  , UTUTC1, UTUTC2, XR1   , XR2   , YR1   ,
     4          YR2
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXFIL=400,MAXARC=20)
C
C MAXFIL : MAXIMUM NUMBER OF STANDARD ORBIT FILES (OR ERP FILES)
C MAXSAT : MAXIMUM NUMBER OF SATELLITES
C MAXARC : MAXIMUM NUMBER OF ARCS
C
C DECLARATIONS
C ------------
      CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
C
      CHARACTER*32 FILNAM(4,MAXFIL),STDPOL(2,MAXFIL),FILPOL
      CHARACTER*6  MXNFIL,MXNARC,MXNSAT
      CHARACTER*1  SOURC1(10,MAXARC),SOURC2(10,MAXARC)
C
      INTEGER*4    NSAAR1(MAXARC),SATAR1(MAXSAT,MAXARC)
      INTEGER*4    NSAAR2(MAXARC),SATAR2(MAXSAT,MAXARC)
      INTEGER*4    NVAL(6)
      INTEGER*4    HELP1(3),HELP2(3),INDEXT(MAXFIL)
C
      REAL*8       TIMAR1(2,MAXARC),TIMAR2(2,MAXARC)
      REAL*8       XXX1(6),XXX2(6),YYY2(6),ELE1(7),ELE2(7)
      REAL*8       PREMA1(3,3),NUTMA1(3,3),SIDMA1(3,3),BIAS1(3,3)
      REAL*8       PREMA2(3,3),NUTMA2(3,3),SIDMA2(3,3),BIAS2(3,3)
      REAL*8       TRAMAT(3,3)
      REAL*8       HT0(2),TSHAD(2),TOSC(MAXFIL)
      REAL*8       DELE(11),DMEAN(6,11),DMEA(6,11)
C
      TYPE(t_stdhead)  :: stdHdr
      TYPE(t_epoch)    :: ttut1,ttut2,ttdt
C
C COMMON BLOCKS
C -------------
      COMMON/MCMFIL/MXCFIL,MXNFIL
      COMMON/MCMARC/MXCARC,MXNARC
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C -----------------------------------------------
      MXCFIL=MAXFIL
      MXNFIL='MAXFIL'
      MXCARC=MAXARC
      MXNARC='MAXARC'
      MXCSAT=MAXSAT
      MXNSAT='MAXSAT'
C
C DEFINE LOGICAL FILE NUMBERS
C ---------------------------
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(keyValue)
      CALL INIT_STDHEAD(stdHdr)
      CALL INIT_INPKEY(inpKey)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL readinpf(' ',inpKey)
C
C DEFINE SYSTEM FILES
C -------------------
      CALL OPNSYS
C
C PRINT TITLE
C -----------
      CALL pritit('STDELE','Compare osculating elements')
      CALL PRFLNA
C
C DEFINE CONSTANTS
C ----------------
      CALL DEFCON(1)
C
C READ FILE NAMES
C ---------------
C
C FILNAM(1,IFIL): STANDARD ORBIT FILE1
C FILNAM(2,IFIL): CORRESPONDING ERP FILE1
C FILNAM(3,IFIL): STANDARD ORBIT FILE2
C FILNAM(4,IFIL): CORRESPONDING ERP FILE2
C
C------------------------------------------
C
C READ IN A TWO COLUMN LIST
      NFLCOL=2
      CALL GTFILE('STDFIL',NFLCOL,MAXFIL,NFIL,STDPOL)
C
C ORDER STD FILES ACCORDING TO OSCULATION EPOCH
      DO IFIL=1,NFIL
        CALL RDSTDH(STDPOL(1,IFIL),stdHdr,IRC)
        TOSC(IFIL)=stdHdr%arc(1)%tosc
      ENDDO
      CALL DORDUP(TOSC,NFIL,INDEXT)
C
C USE A SPECIAL POLE FILE OR NOT
      CALL readkeys('POLFIL',keyValue,irc)
      IF (irc .EQ. 0 .AND. keyValue(1) .EQ. '1') THEN
        NFLCOL=2
      ELSE
        NFLCOL=1
        DO IFIL=1,MAXFIL
          FILNAM(2,IFIL)=' '
        END DO
      END IF
C
C ORDER THE FILES BY PAIRS INTO THE 4 COLUMN LIST
      DO IFIL=1,NFIL
        DO II=1,NFLCOL
          FILNAM(II,IFIL)=STDPOL(II,INDEXT(IFIL))
        ENDDO
      ENDDO
C
      IF (NFIL .LE. 1) THEN
        WRITE(LFNERR,'(/,A,//)')
     1       ' ### PGM STDELE: YOU HAVE TO SELECT AT LEAST 2 FILES'
        GOTO 1016
      ENDIF
      DO IFIL=2,NFIL
        DO II=1,2
          FILNAM(II+2,IFIL-1)=FILNAM(II,IFIL)
        ENDDO
      ENDDO
      NFIL=NFIL-1
C
      IF (NFLCOL .EQ. 1) CALL GTFLNA(1, 'POLE', FILPOL, IRC)
C
      IANT1=0
      IANT2=0
      IDER=1
      ISTOP=0
C
C INITIALIZE THE MEAN DIFFERENCES
C -------------------------------
      NVAL(1) = 0
      NVAL(2) = 0
      NVAL(3) = 0
      NVAL(4) = 0
      DO 50 I=1,11
        DMEA(1,I)=0.D0
        DMEA(2,I)=0.D0
        DMEA(3,I)=0.D0
        DMEA(4,I)=0.D0
        DMEAN(1,I)=0.D0
        DMEAN(2,I)=0.D0
        DMEAN(3,I)=0.D0
        DMEAN(4,I)=0.D0
50    CONTINUE
C
C LOOP OVER ALL PAIRS OF STANDARD ORBIT FILES
C -------------------------------------------
      DO 100 IFIL=1,NFIL
        close(lfnor1)
        IF (FILNAM(2,IFIL).EQ.' ') FILNAM(2,IFIL)=FILPOL
        IF (FILNAM(4,IFIL).EQ.' ') FILNAM(4,IFIL)=FILPOL
C
C WRITE THE HEADER OF THE OUTPUT
C ------------------------------
        WRITE(LFNPRT,1000) FILNAM(1,IFIL),FILNAM(3,IFIL)
1000    FORMAT(//,1X,A32,5X,A32,/)
        WRITE(LFNPRT,1001)
1001    FORMAT(1X,'EPOCH  SAT     A      E      I    NODE ',
     1         '   PER     LAT    RAD    ALO    OUT    POS    VEL',/
     2         1X,89('-'))
C
C GET THE INFORMATION ABOUT THE BOTH STANDARD ORBITS
C --------------------------------------------------
        CALL GETARC(FILNAM(1,IFIL),NAR1,NSAAR1,SATAR1,TIMAR1,SOURC1,
     1              IORSY1)
C
        CALL GETARC(FILNAM(3,IFIL),NAR2,NSAAR2,SATAR2,TIMAR2,SOURC2,
     1              IORSY2)
C
C MORE THAN ONE ARC - NOT YET IMPLEMENTED
C ---------------------------------------
        IARC=1
        IF (NAR1.GT.1 .OR. NAR2.GT.1) THEN
          WRITE(LFNERR,*)'MORE THAN ONE ARC IN ONE ORBIT FILE'
          CALL EXITRC(2)
        END IF
C
C COMPUTE THE EPOCH FOR WHICH THE OSCULATING ELEMENTS SHOULD BE
C COMPARED
C -------------------------------------------------------------
        IF (TIMAR1(1,IARC) .LT. TIMAR2(1,IARC)) THEN
          EPOCH = (TIMAR1(2,IARC)+TIMAR2(1,IARC)) / 2.D0
        ELSE
          EPOCH = (TIMAR1(1,IARC)+TIMAR2(2,IARC)) / 2.D0
        END IF
        EPOCH=DNINT(EPOCH)
C
C GET THE INFORMATION ABOUT THE POLE
C ----------------------------------
        CALL POLDF1(FILNAM(2,IFIL),EPOCH,1,XR1,YR1,UTUTC1,GPUTC1)
        CALL POLDF1(FILNAM(4,IFIL),EPOCH,1,XR2,YR2,UTUTC2,GPUTC2)
C
C PRECESSION AND NUTATION (TIME ARGUMENT: TDB, APPROX. AS TDT)
C ------------------------------------------------------------
        TDT=EPOCH+(19.D0+32.184D0)/86400.D0
        CALL PRCEFF(IORSY1,5.D0,TDT,PREMA1)
        CALL NUTEFF(IORSY1,2.D0,TDT,NUTMA1,EQEQU1,BIAS1)
        PREMA1 = matmul(PREMA1,BIAS1)
        CALL PRCEFF(IORSY2,5.D0,TDT,PREMA2)
        CALL NUTEFF(IORSY2,2.D0,TDT,NUTMA2,EQEQU2,BIAS2)
        PREMA2 = matmul(PREMA2,BIAS2)
C
C TRUE SIDERIAL TIME (TIME ARGUMENT: UT1)
C ---------------------------------------
        TUT1=EPOCH-GPUTC1+UTUTC1
        TTUT1=.realToEpoch.TUT1
        TTDT =.realToEpoch.TDT
        SZ1=GSTIME(0,TTUT1,TTDT,NUTMA1(2,1),EQEQU1)
        TUT2=EPOCH-GPUTC2+UTUTC2
        TTUT2=.realToEpoch.TUT2
        SZ2=GSTIME(0,TTUT2,TTDT,NUTMA2(2,1),EQEQU2)
C
C SIDERAL TIME MATRIX
C -------------------
        CALL SIDMAT(tdt,SZ1,XR1,YR1,SIDMA1)
        CALL SIDMAT(tdt,SZ2,XR2,YR2,SIDMA2)
C
C PREPARE THE TRANSFORMATION MATRICES (SID1*NUT1*PRE1)^-1 AND
C (SID2*NUT2*PRE2) AND THEIR PRODUCT (STORED IN TRAMAT)
C ----------------------------------------------------------
        CALL DMLMAM(NUTMA1,PREMA1,NUTMA1)
        CALL DMLMAM(SIDMA1,NUTMA1,SIDMA1)
        CALL DMINV(SIDMA1,3,DUMMY,HELP1,HELP2)
C
        CALL DMLMAM(NUTMA2,PREMA2,NUTMA2)
        CALL DMLMAM(SIDMA2,NUTMA2,SIDMA2)
C
        CALL DMLMAM(SIDMA1,SIDMA2,TRAMAT)
C
C LOOP OVER ALL SATELLITES FROM THE FIRST FILE
C --------------------------------------------
        DO 200 ISAT1=1,NSAAR1(IARC)
C
C FIND THE CORRESPONDING SATELLITE IN THE SECOND FILE
          DO 210 ISAT2=1,NSAAR2(IARC)
            IF (SATAR1(ISAT1,IARC) .EQ. SATAR2(ISAT2,IARC)) GOTO 211
210       CONTINUE
          GOTO 200
211       CONTINUE
C
C GET THE POSITIONS AND THE VELOCITIES OF THE SATELLITES
          CALL GETORF(FILNAM(3,IFIL),SATAR2(ISAT2,IARC),IANT2,IDER,
     1          ISTOP,EPOCH,ICRAR2,IORSY2,XXX2,TOSC2,ELE2,IRCODE)
C
          CALL GETORF(FILNAM(1,IFIL),SATAR1(ISAT1,IARC),IANT1,IDER,
     1          ISTOP,EPOCH,ICRAR1,IORSY1,XXX1,TOSC1,ELE1,IRCODE)
C
C ECLIPSING SATELLITE ?
          HT0(1)=86400.D0
          HT0(2)=EPOCH-0.5D0
          TSHAD(1)=0.D0
          TSHAD(2)=0.D0
          RADK=1.D0
          CALL SHDBND(SATAR1(ISAT1,IARC),HT0,TOSC1,ELE1,IORSY1,
     1                TSHAD,RADK)
          IF (TSHAD(1).EQ.0) THEN
            INDSHD = 1
          ELSE
            INDSHD = 2
          END IF
C
C TRANSFORMATION: YYY2 = (SID1*NUT1*PRE1)^-1 * (SID2*NUT2*PRE2) * XXX2
C --------------------------------------------------------------------
          CALL DMLMAV(XXX2(1),TRAMAT,YYY2(1))
          CALL DMLMAV(XXX2(4),TRAMAT,YYY2(4))
C
C COMPUTE THE OSCULATING ELEMENTS
C -------------------------------
C  ELE(1) ... ELE(7): A,E,I,KN,PER,U0,T0
C
          CALL XYZELE(GM,EPOCH,XXX1(1),XXX1(4),SATAR1(ISAT1,IARC),
     1                ELE1(1),ELE1(2),ELE1(3),ELE1(4),ELE1(5),ELE1(7))
          CALL XYZELE(GM,EPOCH,YYY2(1),YYY2(4),SATAR2(ISAT2,IARC),
     2                ELE2(1),ELE2(2),ELE2(3),ELE2(4),ELE2(5),ELE2(7))
C
C MEAN ANOMALY
          ANM1 = DSQRT((GM/ELE1(1)**3)) * (EPOCH-ELE1(7))
          ANM2 = DSQRT((GM/ELE2(1)**3)) * (EPOCH-ELE2(7))
C KEPLER'S EQUATION
          DEPS = 1.D-12
          ANEOLD = ANM1
30        ANE1 = ANM1 + ELE1(2)*DSIN(ANEOLD)
          IF (DABS(ANE1-ANEOLD) .GT. DEPS) THEN
            ANEOLD = ANE1
            GOTO 30
          END IF
C
          ANEOLD = ANM2
40        ANE2 = ANM2 + ELE2(2)*DSIN(ANEOLD)
          IF (DABS(ANE2-ANEOLD) .GT. DEPS) THEN
            ANEOLD = ANE2
            GOTO 40
          END IF
C TRUE ANOMALY
          ANT1 = 2*DATAN( DSQRT( (1.D0+ELE1(2))/(1.D0-ELE1(2)) ) *
     1                    DTAN(ANE1/2) )
          ANT2 = 2*DATAN( DSQRT( (1.D0+ELE2(2))/(1.D0-ELE2(2)) ) *
     1                    DTAN(ANE2/2) )
C ARGUMENT OF LATITUDE
          ELE1(6) = ELE1(5) + ANT1
          ELE2(6) = ELE2(5) + ANT2
C
C COMPUTE AND WRITE THE DIFERENCES
C --------------------------------
          DELE(1) = ELE1(1)-ELE2(1)
          DO 300 I=2,6
            DELE(I) = ELE1(1)*(ELE1(I)-ELE2(I))
300       CONTINUE
          DELE(10) = 0.D0
          DELE(11) = 0.D0
          DO 350 I=1,3
            DELE(I+6) = XXX1(I) - YYY2(I)
            DELE(10)  = DELE(10) + DELE(I+6)**2
            DELE(11)  = DELE(11) + (XXX1(I+3)-YYY2(I+3))**2
350       CONTINUE
          DELE(10) = DSQRT(DELE(10))
          DELE(11) = 1.D3*DSQRT(DELE(11))
C
C COMPUTE THE RADIAL, ALONG AND CROSS TRACK DIFFERENCES
C -----------------------------------------------------
          CALL RESORB(DELE(7),XXX1(1),XXX1(4),DELE(7))
C
C WRITE
C -----
C
C Check for OUTLIERS
C ------------------
          IF (DELE(10).GT.0.4D0 .OR. DELE(11).GT.0.08D0) THEN
            WRITE(LFNPRT,1010) EPOCH,SATAR1(ISAT1,IARC),
     1             (IDNINT(DELE(I)*1.D3),I=1,11)
1010        FORMAT(1X,F6.0,I3,'X',4I7,I8,6I7)
            GOTO 200
          ELSE IF (INDSHD.EQ.1) THEN
            WRITE(LFNPRT,1011) EPOCH,SATAR1(ISAT1,IARC),
     1             (IDNINT(DELE(I)*1.D3),I=1,11)
1011         FORMAT(1X,F6.0,I3,1X,4I7,I8,6I7)
          ELSE
            WRITE(LFNPRT,1012) EPOCH,SATAR1(ISAT1,IARC),
     1             (IDNINT(DELE(I)*1.D3),I=1,11)
1012         FORMAT(1X,F6.0,I3,'E',4I7,I8,6I7)
          END IF
C
C Check for GLONASS
C -----------------
          IF (SATAR1(ISAT1,IARC).GE.100 .AND.
     1        SATAR1(ISAT1,IARC).LT.200) INDSHD=INDSHD+2
C
C Check for SLR
C --------------
          IF (SATAR1(ISAT1,IARC).GE.951.AND.
     1        SATAR1(ISAT1,IARC).LT.1000) INDSHD=INDSHD+4
C
          NVAL(INDSHD) = NVAL(INDSHD) + 1
          DO 400 I=1,11
            DMEA (INDSHD,I) = DMEA(INDSHD,I)  + DELE(I)
            DMEAN(INDSHD,I) = DMEAN(INDSHD,I) + DELE(I)**2
400       CONTINUE
C
C END OF LOOP OVER ALL SATELLITES
200     CONTINUE
C
C END OF LOOP OVER ALL PAIRS OF STANDARD ORBIT FILES
100   CONTINUE
C
C COMPUTE AND WRITE THE MEAN DIFFERENCES
C --------------------------------------
      DO 500 I=1,11
        IF (NVAL(1).NE.0) DMEAN(1,I) = DSQRT(DMEAN(1,I)/NVAL(1))
        IF (NVAL(2).NE.0) DMEAN(2,I) = DSQRT(DMEAN(2,I)/NVAL(2))
        IF (NVAL(3).NE.0) DMEAN(3,I) = DSQRT(DMEAN(3,I)/NVAL(3))
        IF (NVAL(4).NE.0) DMEAN(4,I) = DSQRT(DMEAN(4,I)/NVAL(4))
        IF (NVAL(5).NE.0) DMEAN(5,I) = DSQRT(DMEAN(5,I)/NVAL(5))
        IF (NVAL(6).NE.0) DMEAN(6,I) = DSQRT(DMEAN(6,I)/NVAL(6))
C
        IF (NVAL(1).NE.0) DMEA(1,I)  = DMEA(1,I)/NVAL(1)
        IF (NVAL(2).NE.0) DMEA(2,I)  = DMEA(2,I)/NVAL(2)
        IF (NVAL(3).NE.0) DMEA(3,I)  = DMEA(3,I)/NVAL(3)
        IF (NVAL(4).NE.0) DMEA(4,I)  = DMEA(4,I)/NVAL(4)
        IF (NVAL(5).NE.0) DMEA(5,I)  = DMEA(5,I)/NVAL(5)
        IF (NVAL(6).NE.0) DMEA(6,I)  = DMEA(6,I)/NVAL(6)
500   CONTINUE
C
1013  FORMAT(1X,89('-'),/,1X,A1,I4,5X,     4I7,I8,6I7,
     1                  /,1X,A1,I4,3X,'E ',4I7,I8,6I7)
C
      IF(NVAL(1).GT.0 .OR. NVAL(2).GT.0) THEN
        WRITE(LFNPRT,1013) '#',NVAL(1),(IDNINT(DMEAN(1,I)*1.D3),I=1,11),
     1                     '#',NVAL(2),(IDNINT(DMEAN(2,I)*1.D3),I=1,11)
      END IF
C
      IF(NVAL(3).GT.0 .OR. NVAL(4).GT.0) THEN
        WRITE(LFNPRT,1013) 'g',NVAL(3),(IDNINT(DMEAN(3,I)*1.D3),I=1,11),
     1                     'g',NVAL(4),(IDNINT(DMEAN(4,I)*1.D3),I=1,11)
      END IF
C
      IF(NVAL(5).GT.0 .OR. NVAL(6).GT.0) THEN
        WRITE(LFNPRT,1013) 'l',NVAL(5),(IDNINT(DMEAN(5,I)*1.D3),I=1,11),
     1                     'l',NVAL(6),(IDNINT(DMEAN(6,I)*1.D3),I=1,11)
      END IF
C
C
      IF(NVAL(1).GT.0 .OR. NVAL(2).GT.0) THEN
        WRITE(LFNPRT,1015) ' ',NVAL(1),(IDNINT(DMEA(1,I)*1.D3),I=1,9),
     1                         NVAL(2),(IDNINT(DMEA(2,I)*1.D3),I=1,9)
      END IF
C
      IF(NVAL(3).GT.0 .OR. NVAL(4).GT.0) THEN
        WRITE(LFNPRT,1015) 'g',NVAL(3),(IDNINT(DMEA(3,I)*1.D3),I=1,9),
     1                         NVAL(4),(IDNINT(DMEA(4,I)*1.D3),I=1,9)
      END IF
C
      IF(NVAL(5).GT.0 .OR. NVAL(6).GT.0) THEN
        WRITE(LFNPRT,1015) 'l',NVAL(5),(IDNINT(DMEA(5,I)*1.D3),I=1,9),
     1                         NVAL(6),(IDNINT(DMEA(6,I)*1.D3),I=1,9)
      END IF
C
1015  FORMAT(1X,89('-'),/,1X,' MEAN',A1,/,1X,89('-'),
     1     /,1X,'M',I4,5X,     4I7,I8,4I7,
     2     /,1X,'M',I4,3X,'E ',4I7,I8,4I7)
      WRITE(LFNPRT,"(1X,89('-'))")
      WRITE(LFNPRT,"(2(/,1X,A))")
     1     'REMARK: All values are given in units of millimeters.',
     2     '        Angles are scaled to the semimajor axis.'
C
1016  CONTINUE
C
      CALL EXITRC(0)
      END
