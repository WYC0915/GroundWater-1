      MODULE s_OSCALL
      CONTAINS

C*
      SUBROUTINE OSCALL(IARC  ,IUPD  ,NINT  ,T0ARC ,TB12  ,NSAT  ,
     1                  NAVNUM,TOSC  ,A     ,E     ,XI    ,XKN   ,
     2                  PER   ,U0    ,TPER  ,RPRESS,NSTC  ,NSTCEP,
     3                  FRCTYP,INTSTC,TIMSTC,PARSTC,SOURCE,IORSYS,
     4                  ORBDSC)
CC
CC NAME       :  OSCALL
CC
CC PURPOSE    :  READ UPDATED ORBITAL PARAMETERS (OSCULATING ELEMENTS,
CC               RADIATION PRESSURE PARAMETERS) FOR ALL SATELLITES
CC               OF AN ARC.
CC
CC PARAMETERS :
CC         IN :  IARC   : ARC NUMBER IN STD-ORBIT FILE        I*4
CC               IUPD   : RETURN UPDATED ELEMENTS (=1)        I*4
CC                        OR A PRIORI ELEMENTS (=0)
CC               NINT   : NUMBER OF INTERVALS                 I*4
CC               T0ARC  : REFERENCE TIME FOR ARC              R*8
CC               TB12   : PARTIAL INTERVAL BOUNDARIES         R*8
CC       OUT :   NSAT   : NUMBER OF SATELLITES                I*4
CC               NAVNUM : SATELLITE NUMBERS                   I*4
CC               TOSC   : OSCULATION EPOCH                    R*8
CC               A,E,XI,XKN,PER,TPER,U0: OSCULATING ELEMENTS  R*8
CC               RPRESS : RADIATION PRESSURE PARAMETERS       R*8
CC               NSTC   : NUMBER OF STOCHASTIC EPOCHS FOR     I*4
CC                        REQUIRED SATELLITE/ARC COMBINATION
CC               NSTCEP : NUMBER OF PARAMETERS PER EPOCH      I*4
CC               FRCTYP : PARAMETER TYPES                     I*4
CC               INTSTC : INTERVAL NUMBERS OF STOCHASTIC      I*4
CC                        PERTURBATIONS
CC               TIMSTC : EPOCHS WITH STOCHASTIC PERTURBAT.   R*8
CC               PARSTC : PARAMETER VALUES                    R*8
CC               SOURCE : ORBIT SOURCE                        CH*1
CC               IORSYS : ORBIT SYSTEM                        I*4
CC               ORBDSC : ORBIT DESCRIPTION LINES             TYPE(t_orbmodel)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 96)
CC
CC CREATED    :  06-JAN-96
CC
CC CHANGES    :  18-JUN-96 : MR: LINE LONGER THAN 72
CC               27-JUN-96 : TS: CHANGED CHECK OF ANLTYP DUE TO ORBIT MODEL FLG
CC               19-OCT-98 : MR: SECOND ELEMENT FILE (MERGING)
CC               26-JAN-00 : TS: IGNORE STOCH. FOR SATS. WITHOUT ELE.
CC               22-JAN-03 : HU: READ TOSC WITH HIGHER PRECISION
CC                               USE M_BERN, FILENAMELENGTH
CC               06-AUG-03 : HU: NEW STD FORMAT, CHECK NUTATION MODEL
CC               12-DEC-03 : AJ: SMALL CHANGE DUE TO STOCH. ACCELERATIONS
CC               31-JAN-05 : HU: ORBIT MODEL DESCRIPTION
CC               03-FEB-05 : HU: SET RPR MODEL ADDORDING TO ELE FILE RECORDS
CC                               PARAMETER MAXOMD REMOVED
CC               22-FEB-05 : AJ: (RPR-MODEL WRITING DEACTIVATED)
CC               04-APR-05 : AJ: VARYING NUMBER OF PULSES PER EPOCH ALLOWED
CC               18-MAY-05 : HU: REFINED WRITING OF EMPIRI KEYWORD
CC               25-MAY-05 : HU: IGNORE PULSES OUTSIDE OF TIME WINDOW
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               04-AUG-08 : DT: SYSTEM FOR DYNAMIC ORBIT PARAMETERS
CC                               (DYX,RSW,DRSW)
CC               04-FEB-09 : RD: STOP READING AT AN EMPTY LINE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const,  ONLY: PI
      USE p_orbgen, ONLY: maxomd,t_orbmodel
      USE s_dimtst
      USE s_opnfil
      USE s_rdanlt
      USE f_djul
      USE s_opnerr
      USE s_eletra
      USE s_exitrc
      USE s_jmt
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IARC  , IARC1 , IBEG  , IDD   , IELFIL, IEND  ,
     1          IEP   , IEXIST, IFMT  , IFORB , IFOUND, IFRC  , III   ,
     2          INT   , IORB  , IORSYS, IOSTAT, IRAD  , IRC   , IREC  ,
     3          ISAT  , IUPD  , JJ    , MM    , MXCINT, MXCSAT, MXCVAR,
     4          NINT  , NSAT  , NUMSAT, NSTEMP, K
C
      REAL*8    DD    , T0ARC , TEST  , TIMACT, TOSC  , TYEAR
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      REAL*8       A(*),E(*),XI(*),XKN(*),PER(*),TPER(*),U0(*)
      REAL*8       TB12(*),RPRESS(MXCVAR,*),TIMSTC(MXCINT,*),
     1             PARSTC(3,MXCINT,*)
C
      INTEGER*4    NAVNUM(*),FRCTYP(3,MXCINT,*),NSTC(*),NSTCEP(MXCINT,*)
      INTEGER*4    INTSTC(MXCINT,*)
C
      CHARACTER*80 STRG80
      CHARACTER*(fileNameLength) FILNAM
      CHARACTER*10 STRG10
      CHARACTER*8  ANLTYP,ANLELE
      CHARACTER*7  INTNAM
      CHARACTER*6  MXNVAR,MXNINT,MXNSAT
      CHARACTER*6  ORBSYS
      CHARACTER*1  SOURCE(*)
      CHARACTER*1  FRCCHR(30)
C
      TYPE(t_orbmodel) orbdsc
C
      COMMON/MCMVAR/MXCVAR,MXNVAR
      COMMON/MCMINT/MXCINT,MXNINT
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
      DATA FRCCHR/'R','S','W','D','Y','X','-','-','-','-',
     1            'R','S','W','D','Y','X','-','-','-','-',
     2            'R','S','W','D','Y','X','-','-','-','-'/
C
C INITIALIZE NSAT
C ---------------
      NSAT=0
C
C READ CORRECT ORBIT INFORMATION
C ------------------------------
      IF(IUPD.EQ.0)THEN
        IBEG=22
        IEND=37
      ELSE
        IBEG=38
        IEND=53
      END IF
C
C INITIALIZE NUMBER OF STOCHASTIC EPOCHS PER SATELLITE
C ----------------------------------------------------
      DO 215 ISAT=1,MXCSAT
        NSTC(ISAT)=0
215   CONTINUE
C
C LOOP OVER TWO ELEMENT FILES (GPS, GLONASS)
C ------------------------------------------
      DO IELFIL=1,2
C
C OPEN INPUT FILE
C ---------------
        IF (IELFIL.EQ.1) THEN
          IEXIST=1
          INTNAM='IMPORB'
        ELSE
          IEXIST=0
          INTNAM='IMPORB2'
        ENDIF
C
        CALL GTFLNA(IEXIST,INTNAM,FILNAM,IRC)
        IF (IELFIL.EQ.2 .AND. IRC.NE.0) GOTO 999
C
        CALL OPNFIL(LFNOR1,FILNAM,'OLD','FORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNOR1,IOSTAT,FILNAM,'OSCALL')
C
C LOOK FOR BEGINNING OF CORRECT ARC
C ---------------------------------
        IFMT=0
        orbdsc%nlin=0
        DO
          READ(LFNOR1,5,END=900) STRG80
5         FORMAT(A80)
          IF(STRG80(1:10).EQ.'ARC-NUMBER')THEN
            READ(STRG80(22:24),6)IARC1
6           FORMAT(I3)
            IF(IARC1.EQ.IARC)GO TO 20
          ELSEIF(STRG80(1:7).EQ.'FORMAT:') THEN
            READ(STRG80,"(8X,2I6)")IFMT,orbdsc%nlin
            CALL DIMTST(1,2,2,'OSCALL','MAXOMD','ORBIT MODEL LINES',' ',
     1                  orbdsc%nlin,MAXOMD,IRC)
            DO I=1,orbdsc%nlin
              READ(LFNOR1,5) orbdsc%orbmod(I)
            ENDDO
          END IF
        ENDDO
        WRITE(LFNERR,15)IARC
15      FORMAT(//,' *** SR OSCALL: INFO FOR ARC',I3,' NOT FOUND',//)
        CALL EXITRC(2)
20      CONTINUE
C
C CHECK STARTING TIME FOR CURRENT ARC
C -----------------------------------
        READ(STRG80(56:75),7)TOSC
        IF(T0ARC+TB12(1).NE.TOSC)THEN
          WRITE(LFNERR,21)T0ARC+TB12(1),TOSC
21        FORMAT(/,' *** SR OSCALL: TSTART AND TOSC DO NOT MATCH:',
     1             2F20.12,
     2           /,'                PROCESSING STOPPED',/)
          CALL EXITRC(2)
        END IF
C
C DEFINE "SOURCE" OR ORBIT
C ------------------------
        IF (IELFIL.EQ.1) THEN
          SOURCE(1)='U'
          SOURCE(2)='P'
          CALL JMT(TOSC,JJ,MM,DD)
          TYEAR=DJUL(JJ,1,0.D0)
          IDD=TOSC-TYEAR
          WRITE(STRG10(3:10),22)JJ,IDD
22        FORMAT(I4,'.',I3)
          DO 23 I=3,10
            IF(STRG10(I:I).NE.' ')THEN
              SOURCE(I)=STRG10(I:I)
            ELSE
              SOURCE(I)='0'
            END IF
23        CONTINUE
        ENDIF
C
C CORRECT ARC FOUND
C -----------------
        IFORB=1
C
C READ ORBIT PARAMETERS FOR ALL SATELLITES
C ----------------------------------------
        DO 200 III=1,MXCSAT
          NSAT=NSAT+1
          CALL DIMTST(1,2,2,'OSCALL','MAXSAT','SATELLITES',
     1              'INCLUDE FILE USED',NSAT,MXCSAT,IRC)
          READ(STRG80(47:49),6)NAVNUM(NSAT)
          READ(STRG80(56:75),7)TOSC
7         FORMAT(F20.12)
          TEST=DABS(TOSC-(T0ARC+TB12(1)))
          IF(TEST.GT.1.D-5)THEN
            WRITE(LFNERR,30)T0ARC+TB12(1)
30          FORMAT(//,' *** SR OSCALL: OSCULATION EPOCH',F16.8,
     1                ' NOT IN FILE',//)
            CALL EXITRC(2)
          END IF
C
C SKIP ONE RECORD
C ---------------
          READ(LFNOR1,5,END=900) STRG80
C
C INITIALIZE RADIATION PRESSURE PARAMETERS
C ----------------------------------------
          DO 310 IRAD=1,9
            RPRESS(IRAD,NSAT)=0.D0
310       CONTINUE
C
C READ ORBIT PARAMETERS FOR SATELLITE NUMBER NSAT
C -----------------------------------------------
          DO 100 IORB=1,MXCVAR+1
            IF(IORB.LE.16) THEN
              READ(LFNOR1,5,END=900) STRG80
              IF(IORB.EQ.1)THEN
                READ(STRG80(IBEG:IEND),31)A(NSAT)
31              FORMAT(F16.5)
C
C DEFINE ORBIT SYSTEM (IF NOT AVAILABLE IN *.ELE-FILE,
C IORSYS=2 (J2000.0) IS ASSUMED
                READ(STRG80(73:78),311)ORBSYS
311             FORMAT(A6)
                IF(ORBSYS.EQ.'ORBSYS')THEN
                  READ(STRG80(79:80),312)IORSYS
312               FORMAT(I2)
                ELSE
                  IORSYS=2
                END IF
C
              ELSE IF(IORB.EQ.2)THEN
                READ(STRG80(IBEG:IEND),32)E(NSAT)
32              FORMAT(F16.10)
              ELSE IF(IORB.EQ.3)THEN
                READ(STRG80(IBEG:IEND),33)XI(NSAT)
                XI(NSAT)=XI(NSAT)/180*PI
33              FORMAT(F16.9)
              ELSE IF(IORB.EQ.4)THEN
                READ(STRG80(IBEG:IEND),33)XKN(NSAT)
                XKN(NSAT)=XKN(NSAT)/180*PI
              ELSE IF(IORB.EQ.5)THEN
                READ(STRG80(IBEG:IEND),33)PER(NSAT)
                PER(NSAT)=PER(NSAT)/180*PI
              ELSE IF(IORB.EQ.6)THEN
                READ(STRG80(IBEG:IEND),33)U0(NSAT)
                IF(STRG80(1:3).EQ.'ARG')THEN
                  U0(NSAT)=U0(NSAT)/180*PI
                  CALL ELETRA(2,A(NSAT),E(NSAT),PER(NSAT),0.D0,
     1                        TPER(NSAT),U0(NSAT))
                ELSE
                  TPER(NSAT)=U0(NSAT)
                  CALL ELETRA(1,A(NSAT),E(NSAT),PER(NSAT),0.D0,
     1                        TPER(NSAT),U0(NSAT))
                END IF
              ELSE IF(STRG80(1:2).EQ.'P0'.OR.STRG80(1:2).EQ.'D0'.OR.
     1                      STRG80(1:2).EQ.'R0')THEN
                READ(STRG80(IBEG:IEND),34)RPRESS(1,NSAT)
34              FORMAT(D16.9)
C
C CHECK WHETHER INPUT SATELLITE FILE AND FILE-TYPE IN ELE-FILE MATCH
                IF(IFORB.EQ.1)THEN
                  IFORB=0
                  READ(STRG80(73:80),35)ANLELE
35                FORMAT(A8)
                  CALL RDANLT(ANLTYP)
                  IF(ANLELE(1:7).EQ.ANLTYP(1:7))GO TO 37
                  IF(ANLELE(1:3).EQ.'   '.AND.
     1               ANLTYP(1:3).EQ.'OLD')GO TO 37
                  WRITE(LFNERR,36)ANLTYP,ANLELE
36                FORMAT(//,' *** SR OSCALL: SAT-FILE SPECIFICATIONS',
     1                      ' IN SAT- AND ELE-FILE DO NOT MATCH',
     2                       2(2X,A9),//)
                  CALL EXITRC(2)
                END IF
37              CONTINUE
              ELSE IF(STRG80(1:2).EQ.'P2'.OR.STRG80(1:2).EQ.'Y0'.OR.
     1                STRG80(1:2).EQ.'S0')THEN
                READ(STRG80(IBEG:IEND),34)RPRESS(2,NSAT)
              ELSE IF(STRG80(1:2).EQ.'P3'.OR.STRG80(1:2).EQ.'X0'.OR.
     1                STRG80(1:2).EQ.'W0')THEN
                READ(STRG80(IBEG:IEND),34)RPRESS(3,NSAT)
              ELSE IF(STRG80(1:2).EQ.'DC'.OR.STRG80(1:2).EQ.'RC')THEN
                READ(STRG80(IBEG:IEND),34)RPRESS(4,NSAT)
              ELSE IF(STRG80(1:2).EQ.'YC'.OR.STRG80(1:2).EQ.'SC')THEN
                READ(STRG80(IBEG:IEND),34)RPRESS(5,NSAT)
              ELSE IF(STRG80(1:2).EQ.'XC'.OR.STRG80(1:2).EQ.'WC')THEN
                READ(STRG80(IBEG:IEND),34)RPRESS(6,NSAT)
              ELSE IF(STRG80(1:2).EQ.'DS'.OR.STRG80(1:2).EQ.'RS')THEN
                READ(STRG80(IBEG:IEND),34)RPRESS(7,NSAT)
              ELSE IF(STRG80(1:2).EQ.'YS'.OR.STRG80(1:2).EQ.'SS')THEN
                READ(STRG80(IBEG:IEND),34)RPRESS(8,NSAT)
              ELSE IF(STRG80(1:2).EQ.'XS'.OR.STRG80(1:2).EQ.'WS')THEN
                READ(STRG80(IBEG:IEND),34)RPRESS(9,NSAT)
              ELSE IF(STRG80(1:3).EQ.'ARC')THEN
                GO TO 110
              ELSE IF(STRG80(1:3).EQ.'***')THEN
                GO TO 210
              END IF
            END IF
100         CONTINUE
110         CONTINUE
200       CONTINUE
210       CONTINUE
C
C READ STOCHASTIC ORBIT PARAMETERS
C --------------------------------
        DO 220 IREC=1,1000000
          READ(LFNOR1,5,END=900)STRG80
          IF (LEN_TRIM(STRG80).EQ.0) GOTO 900
          IF(STRG80(1:10).EQ.'ARC-NUMBER')THEN
            READ(STRG80(22:24),6)IARC1
            IF(IARC1.EQ.IARC)GO TO 230
          END IF
220     CONTINUE
230     CONTINUE
        READ(STRG80(22:24),6)IARC1
        IF(IARC1.NE.IARC)GO TO 900
        READ(STRG80(47:49),6)NUMSAT
        DO 240 ISAT=1,NSAT
          IF(NUMSAT.EQ.NAVNUM(ISAT))GO TO 250
240     CONTINUE
        WRITE(LFNERR,245)NUMSAT
245     FORMAT(//,' ### SR OSCALL: NO ORBIT ELEMENTS FOR SAT',I3,//)
        GOTO 210
cccc    CALL EXITRC(2)
250     CONTINUE
C
C SKIP NEXT RECORD
C ----------------
        READ(LFNOR1,5,END=900)STRG80
C
C GET ALL STOCHASTIC EPOCHS FOR ONE SATELLITE
C -------------------------------------------
        DO 290 IEP=1,10000
          READ(LFNOR1,5,END=900)STRG80
          IF (LEN_TRIM(STRG80).EQ.0) GOTO 900
          IF(STRG80(1:3).EQ.'ARC')GO TO 230
          READ(STRG80(3:4),251)NSTEMP
251       FORMAT(I2)
          READ(STRG80(9:20),252)TIMACT
252       FORMAT(F12.5)
          IF (TIMACT < T0ARC+TB12(1) - 1.D-5 .OR.
     1        TIMACT > T0ARC+TB12(NINT+1) + 1.D-5) THEN
            DO IFRC=2,NSTEMP
              READ(LFNOR1,*,END=900)
            ENDDO
          ELSE
            NSTC(ISAT)=NSTC(ISAT)+1
            NSTCEP(NSTC(ISAT),ISAT)=NSTEMP
C
C CORRECT LEFT INTERVAL BOUNDARY
C ------------------------------
ccc test angepasst fuer linear
            DO 260 INT=1,NINT+1
              TEST=DABS(TIMACT-(T0ARC+TB12(INT)))
              IF(TEST.LT.1.D-5)GO TO 270
260         CONTINUE
            WRITE(LFNERR,265)TIMACT
265         FORMAT(//,' SR OSCALL: STOCH. EPOCH',F15.8,' DOES NOT',
     1                ' CORRESPOND TO AN INTERVAL BOUNDARY',//)
            CALL EXITRC(2)
270         CONTINUE
            TIMSTC(NSTC(ISAT),ISAT)=TIMACT
            INTSTC(NSTC(ISAT),ISAT)=INT
            DO IFRC=1,NSTCEP(NSTC(ISAT),ISAT)
              READ(STRG80(IBEG:IEND),34)PARSTC(IFRC,NSTC(ISAT),ISAT)
              READ(STRG80(1:2),251)FRCTYP(IFRC,NSTC(ISAT),ISAT)
              IF(IFRC.NE.NSTCEP(NSTC(ISAT),ISAT))THEN
                READ(LFNOR1,5,END=900)STRG80
              END IF
            ENDDO
          ENDIF
290     CONTINUE
C
C CLOSE ELE-FILE
C --------------
900     CONTINUE
        CLOSE(UNIT=LFNOR1)
C
C NEXT ORBIT ELEMENT FILE
C -----------------------
      ENDDO
C
999   CONTINUE
C
C WRITE RPR MODEL TO ORBIT MODEL DESCRIPTION
      IFOUND=0
      DO I=1,orbdsc%nlin
        IF (orbdsc%orbmod(I)(1:8) .EQ. 'EMPIRI: ') IFOUND=1
      ENDDO
      IF (IFOUND.EQ.0) THEN
        orbdsc%nlin=orbdsc%nlin+1
        CALL DIMTST(1,2,2,'OSCALL','MAXOMD','ORBIT MODEL LINES',' ',
     1              orbdsc%nlin,MAXOMD,IRC)
        WRITE(orbdsc%orbmod(orbdsc%nlin),
     1        "('EMPIRI: ',3A1,' ONCE-PER-REV',1X,A)")
     2        (FRCCHR(FRCTYP(K,1,1)),K=1,3),ANLELE(1:7)
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
