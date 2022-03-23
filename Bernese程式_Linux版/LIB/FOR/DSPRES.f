      MODULE s_DSPRES
      CONTAINS

C*
      SUBROUTINE DSPRES(LFN,IFIL,FROMTO,RESSIZ,ICARR,IWLFAC,ICYCLE,
     1                  IUNIT,IRCODE)
CC
CC NAME       :  DSPRES
CC
CC PURPOSE    :  DISPLAY RESIDUALS (DOUBLE/TRIPLE) OR
CC               STORE THEM ONTO FILE
CC               RESHED%DSC%ITYP=1: ONLY RESIDUALS ACTUALLY GIVEN IN
CC                                  FILE LFNRES MAY BE DISPLAYED
CC               RESHED%DSC%ITYP=2: IT IS ASSUMED, THAT L1 AND L2
CC                                  RESIDUALS ARE ON THE INPUT FILE,
CC                                  ALLOWING TO DISPLAY ANY LINEAR COMB.
CC                                  OF L1 AND L2. L1 AND L2 RESIDUALS MUST
CC                                  FOLLOW EACH OTHER, IF EXISTENT.
CC
CC PARAMETERS :
CC         IN :  LFN    : LOGICAL FILE NUMBER FOR OUTPUT FILE I*4
CC                        LFN = 0 OR LFNPRT : RESIDUALS ARE
CC                        DISPLAYED
CC               IFIL   : FILE NUMBER TO BE PROCESSED         I*4
CC               FROMTO(I),I=1,2: START/END EPOCH             I*4
CC               RESSIZ : ONLY RESIDUALS LARGER THAN RESSIZ   R*8
CC                        ARE PRINTED
CC               ICARR  : CARRIER                             I*4
CC               IWLFAC : WAVELENGTH FACTOR                   I*4
CC               ICYCLE : TYPE OF CYCLE TO BE USED AS UNITS   I*4
CC                        =1: L1 CYCLES
CC                        =2: L2 CYCLES
CC                        =3: NARROW LANE ("APPENZELLER")
CC               IUNIT  : UNITS OF RESIDUALS                  I*4
CC                        =1: CYCLES (ACCORDING TO "IWLFAC"
CC                            AND "ICYCLE")
CC                        =2: MILLIMETERS
CC                        =3: METERS
CC               IRCODE : RETURN CODE                         I*4
CC                        =0: OK
CC                        =1: FILE NOT FOUND IN RESIDUAL FILE
CC                        =2: INVALID FREQUENCY SELECTED
CC                        =3: MAXIMUM NUMBER OF EPOCHS EXCEEDED
CC
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W.GURTNER, M.ROTHACHER, L.MERVART
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/06/20 17:06
CC
CC CHANGES    :  05-JUN-92 : ??: CHANGES FOR THE NEW MAUPRP VERSION
CC               13-JUL-92 : ??: ADD PARAMETER "MAXFRQ" IN CALL RDRESH
CC               08-FEB-93 : ??: STRING LENGTH OF "BSCROL" INCREASED
CC                               FROM 128 TO 158
CC               22-MAR-93 : ??: FORMAT(127A1) --> FORMAT(157A1)
CC               10-NOV-93 : MR: "BSCROL" INCREASED TO 171,
CC                               FORMAT(170A1)
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               25-JUL-94 : MR: CALL CLRSCR
CC               12-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               07-AUG-95 : TS: INCREASED MAXFIL 50 --> 100
CC               23-SEP-97 : DI: USE MAXSAT.inc
CC               28-JUL-98 : HH: MODIFICATIONS FOR GLONASS
CC               16-AUG-99 : RD: DIMENSIONS (SMALL/MEDIUM/LARGE)
CC               27-AUG-02 : RD: HANDLE NEW FORMATTED RESIDUAL FILES
CC               17-FEB-03 : LM: USE M_MAXDIM, PREPROCESSOR COMMANDS
CC               15-MAY-03 : HU: INITIALIZE STRUCTURES
CC               13-SEP-03 : HU: INTERFACE FOR DEFREQ
CC               10-MAR-04 : RD: USE MAXSVN FOR THE SIZE OF INDSVN
CC               10-MAR-04 : HB: CHANGE ORDER OF MODULES
CC               05-OCT-04 : HU: CLOSE FILE
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-FEB-06 : RD: ALLOW 60 SATELLITES (OLD 42)
CC               28-FEB-07 : AG: ALLOW 100 SATELLITES (OLD 60)
CC               21-JUN-10 : RD: ALLOW 60 SATELLITES (ALSO IN ZD-CASE)
CC               12-MAY-12 : RD: REMOVE DIMENSIONS EXCEPT OF LARGE
CC               12-MAY-12 : RD: USE M_BERN WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnres, lfnerr
      USE d_resFil, ONLY: t_resHead, init_reshead
      USE m_maxdim, ONLY: MAXSAT
C
      USE s_dimtst
      USE s_opnfil
      USE s_rdresh2
      USE s_lincom
      USE s_opnerr
      USE s_setflg
      USE s_defreq
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ICARR , ICYCLE, IDSTYP, IEPO2 , IEPOCH, IEPOL2,
     1          IEPOLD, IFIL  , IFILE , IFREQ , IFRQ  , IOSTAT, IPOS1 ,
     2          IPOS2 , IRC   , IRCODE, ISVN  , IUNIT , IWLFAC, JFIL  ,
     3          K     , KMAX  , KSAT  , LFN   , LINE  , MAXEPO, MDIFF ,
     4          NCTOT , NRTOT , NTITC
C
      REAL*8    RESHLP, RESMAX, RESSIZ, WAVE
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C
      PARAMETER    (MAXEPO=40000)

C
      TYPE(t_resHead) reshed
C
      INTEGER*4    FROMTO(*)
      INTEGER*4    SVNHLP(2)
      INTEGER*4    SVNHL2(2,2),IFIL2(2)
      INTEGER*4    ICYC(2)
      REAL*8       RESHL2(2)
      real*8       epofrq(2)
      CHARACTER    STRING(MAXSAT+1)*6,BSCROL(MAXEPO+2)*1024
      CHARACTER    FLG(3)*1
      CHARACTER    RESFLG*1,RESFL1*1,RESFL2*1
      CHARACTER    CHR1*1
      character*32 filres
C
      COMMON/CDSPRS/BSCROL
C
      INCLUDE 'COMFREQ.inc'
C
      INTEGER*4    INDSVN(MAXSVN)
C
cccc     DATA IFIRST/1/
ccccC
cccc       IF(IFIRST.EQ.1) THEN
cccc         IFIRST=0
cccc C
cccc C  NUMBER OF ROWS TO BE DISPLAYED: HARDWIRED
cccc         CALL RDKYBD(1,CHR1,IRC)
cccc         CALL GTMXLN(NROW)
cccc         CALL RDKYBD(-1,CHR1,IRC)
cccc         NROW=NROW-3
cccc C_BEG_IBM_MVS
cccc C       NROW=0
cccc C_END_IBM_MVS
cccc C
cccc        CALL CLRSCR( )
cccc C
cccc       ENDIF
C
C INITIALIZE STRUCTURES
      CALL init_reshead(reshed)
C
C COMPUTE UNITS FOR DISPLAY IN CYCLES
C -----------------------------------
      IF(IUNIT.EQ.1) THEN
        DO 10 IFRQ=1,2
          IF(ICYCLE.EQ.IFRQ.OR.ICYCLE.EQ.3) THEN
            ICYC(IFRQ)=1
          ELSE
            ICYC(IFRQ)=0
          ENDIF
10      CONTINUE
      ENDIF
C
C READ HEADER OF RESIDUAL FILE
C ----------------------------
ccccc      REWIND LFNRES

      call gtflna(1,'RESIDUA',filres,irc)
      call opnfil(lfnres,filres,'OLD','UNFORMATTED',
     1            'READONLY',' ',iostat)
      call opnerr(lfnerr,lfnres,iostat,filres,'dspres')

      call rdresh2(lfnres,reshed)
C
      mDiff = resHed%dsc%nResat+1
C
      epofrq(1) = reshed%filHead(iFil)%timref
      epofrq(2) = reshed%filHead(iFil)%timref
      CALL defreq( epofrq, resHed%filHead(iFil)%nSatel,
     1             resHed%filHead(iFil)%numsat )

C
C CHECK REQUESTED FILE NUMBER
C ---------------------------
      IF(IFIL.LT.1.OR.IFIL.GT.resHed%nFil) THEN
        WRITE(LFNERR,62)IFIL,resHed%nFil
62      FORMAT(/,' *** SR DSPRES: FILE NUMBER OUT OF RANGE',/,
     1                       16X,'REQUESTED FILE NUMBER        :',I4,/,
     2                       16X,'LARGEST FILE NUMBER AVAILABLE:',I4,/)
        DO jFil = 1,reshed%nFil
          DEALLOCATE(resHed%filHead(jFil)%numSat,stat=irc)
        ENDDO
        DEALLOCATE(resHed%filHead,stat=irc)
        IRCODE=1
        RETURN
      END IF
C
C DEFINE FIRST AND LAST EPOCH, IF 0,0 IS ENTERED
C ----------------------------------------------
      IF(FROMTO(1).EQ.0) FROMTO(1)=1
      IF(FROMTO(2).EQ.0) FROMTO(2)=32000
C
C CHECK WHETHER REQUESTED FREQUENCY IS ALLOWED
C --------------------------------------------
      DO 70 IFRQ=1,resHed%filhead(iFil)%nfrfil
        IF(ICARR.EQ.resHed%filHead(iFil)%iCarr(IFRQ)) THEN
          IDSTYP=1
          GOTO 100
        ENDIF
70    CONTINUE
      IF(reshed%dsc%iTyp.EQ.2               .AND.
     1   resHed%filHead(iFil)%nfrfil.EQ.2   .AND.
     2   resHed%filHead(iFil)%iCarr(1).EQ.1 .AND.
     3   resHed%filHead(iFil)%iCarr(2).EQ.2) THEN
        CALL SETFLG(FLG(1),0)
        CALL SETFLG(FLG(2),0)
        IDSTYP=2
        GO TO 100
      END IF
      WRITE(LFNERR,71)ICARR
71    FORMAT(/,' *** SR DSPRES: INVALID FREQUENCY'/,
     1                     16X,'FREQUENCY:',I3,/)
      DO jFil = 1,reshed%nFil
        DEALLOCATE(resHed%filHead(jFil)%numSat,stat=irc)
      ENDDO
      DEALLOCATE(resHed%filHead,stat=irc)
      IRCODE=2
      RETURN
100   CONTINUE
C
C  INDEX VECTOR FOR SATELLITES
      DO 110 KSAT=1,MAXSVN
        INDSVN(KSAT)=0
110   CONTINUE
      DO 120 KSAT=1,resHed%filHead(iFil)%nSatel
        CALL DIMTST(1,1,2,'DSPRES','MAXSVN','MAXIMUM SV-NUMBER',
     1       'SPECIFIED IN ${I}/COMFREQ.inc',
     2       resHed%filHead(iFil)%numSat(kSat),MAXSVN,IRC)
        INDSVN(resHed%filHead(iFil)%numSat(kSat))=KSAT
120   CONTINUE
C
C PRINT RESIDUALS
C ---------------
C
C  TITLE LINE OF SCROLLER
      IF (MDIFF .EQ. 1) THEN
        WRITE(BSCROL(1),101)
     1        resHed%filHead(iFil)%numSat(1:resHed%filhead(iFil)%nSatel)
101     FORMAT(' TIME |',60(2X,I3,1X))
        WRITE(BSCROL(2),102)
     1        ' ',('-',I=1,6*(resHed%filHead(iFil)%nSatel)+6)
102     FORMAT(1024A1)
      ELSE IF (MDIFF .EQ. 2) THEN
        WRITE(BSCROL(1),103)
     1       (resHed%filHead(iFil)%numSat(kSat:kSat+1),
     2                        kSat=1,resHed%filHead(iFil)%nSatel-1)
103     FORMAT(' TIME |',101(I3,I3))
        WRITE(BSCROL(2),104)
     1        ' ',('-',I=1,6*(resHed%filHead(iFil)%nSatel-1)+6)
104     FORMAT(1024A1)
      ENDIF
      LINE=2
      IEPOLD=0
      IEPOL2=0
      KMAX=0
      DO 140 KSAT=1,resHed%filHead(iFil)%nSatel+1
        STRING(KSAT)=' '
140   CONTINUE
C
C  DISPLAY OF TYPE 1
C  -----------------
      IF(IDSTYP.EQ.1)THEN
170       READ(LFNRES,END=180) IFILE,IEPOCH,IFREQ,
     1                         (SVNHLP(K),K=1,2),RESHLP,RESFLG
          IF(IUNIT.EQ.1) THEN
            ISVN=SVNHLP(1)
            WAVE=DABS(
     1             (FLOAT(ICYC(1))*WLGT(1,ISVN)*FACLIN(ICARR,1,ISVN)+
     2              FLOAT(ICYC(2))*WLGT(2,ISVN)*FACLIN(ICARR,2,ISVN))
     3              /FLOAT(IWLFAC))
            RESHLP=RESHLP/WAVE
          ELSE IF(IUNIT.EQ.2) THEN
            RESHLP=RESHLP*1000
          ENDIF
          IF(IEPOCH.GE.FROMTO(1).AND.IEPOCH.LE.FROMTO(2).AND.
     2       IFILE.EQ.IFIL.AND.IFREQ.EQ.ICARR)THEN
            IF(IEPOCH.NE.IEPOLD)THEN
              IF(IEPOLD.NE.0.AND.RESMAX.GE.RESSIZ)THEN
                IF(IEPOL2.NE.0.AND.IEPOLD-IEPOL2.NE.1) THEN
                  LINE=LINE+1
                  IF(LINE.GT.MAXEPO) THEN
                    WRITE(LFNERR,172) MAXEPO,LINE
172                 FORMAT(/,' *** SR DSPRES: TOO MANY EPOCHS IN FILE',
     1                             /,16X,'MAXIMUM NUMBER ALLOWED:',I5,/,
     2                               16X,'NUMBER IN FILE       >=',I5,/)
                    DO jFil = 1,reshed%nFil
                      DEALLOCATE(resHed%filHead(jFil)%numSat,stat=irc)
                    ENDDO
                    DEALLOCATE(resHed%filHead,stat=irc)
                    IRCODE=3
                    RETURN
                  END IF
                  WRITE(BSCROL(LINE),173)
173               FORMAT(' ')
                ENDIF
                LINE=LINE+1
                IF(LINE.GT.MAXEPO) THEN
                  WRITE(LFNERR,172) MAXEPO,LINE
                  DO jFil = 1,reshed%nFil
                    DEALLOCATE(resHed%filHead(jFil)%numSat,stat=irc)
                  ENDDO
                  DEALLOCATE(resHed%filHead,stat=irc)
                  IRCODE=3
                  RETURN
                ENDIF
                WRITE(BSCROL(LINE),171) IEPOLD,(STRING(K),K=1,KMAX)
171             FORMAT(I5,' |',101A)
              END IF
              IEPOL2=IEPOLD
              IEPOLD=IEPOCH
              RESMAX=0.D0
              KMAX=0
              DO 150 KSAT=1,resHed%filHead(iFil)%nSatel+1
                STRING(KSAT)=' '
150           CONTINUE
            END IF
            IF(DABS(RESHLP).GT.RESMAX)
     1        RESMAX=DABS(RESHLP)
            IPOS1=INDSVN(SVNHLP(1))
            IF(IPOS1.EQ.0) GOTO 170
            IF (MDIFF .EQ. 2) THEN
              IPOS2=INDSVN(SVNHLP(2))
              IF(IPOS2.EQ.0) GOTO 170
            ENDIF
            IF(STRING(IPOS1)(1:5).EQ.' ') THEN
              IF(IUNIT.EQ.1) THEN
                WRITE(STRING(IPOS1),151,IOSTAT=IOSTAT) RESHLP,RESFLG
151             FORMAT(F5.2,A)
              ELSE IF(IUNIT.EQ.2) THEN
                WRITE(STRING(IPOS1),152,IOSTAT=IOSTAT) IDNINT(RESHLP),
     1                                                 RESFLG
152             FORMAT(I5,A)
              ELSE
                WRITE(STRING(IPOS1),153,IOSTAT=IOSTAT) RESHLP,RESFLG
153             FORMAT(F5.1,A)
              END IF
              IF (MDIFF .EQ. 1) THEN
                IF (IPOS1 .GT. KMAX) KMAX = IPOS1
              ELSE IF (MDIFF .EQ. 2) THEN
                IF(IPOS2-1.NE.IPOS1) THEN
                  WRITE(STRING(IPOS2-1)(1:5),154)
154               FORMAT('    *')
                  IF(IPOS2.GT.KMAX) KMAX=IPOS2
                ELSE
                  IF(IPOS1.GT.KMAX) KMAX=IPOS1
                END IF
              ENDIF
            ELSE
              IF(IUNIT.EQ.1) THEN
                WRITE(STRING(resHed%filHead(iFil)%nSatel),151,
     1                IOSTAT=IOSTAT) RESHLP,RESFLG
              ELSE IF(IUNIT.EQ.2) THEN
                WRITE(STRING(resHed%filHead(iFil)%nSatel),152,
     1                IOSTAT=IOSTAT) IDNINT(RESHLP),RESFLG
              ELSE
                WRITE(STRING(resHed%filHead(iFil)%nSatel),153,
     1                IOSTAT=IOSTAT) RESHLP,RESFLG
              END IF
              IF (MDIFF .EQ. 1) THEN
                WRITE(STRING(resHed%filHead(iFil)%nSatel+1),156)
     1                SVNHLP(1)
156             FORMAT(':',I2)
              ELSE IF (MDIFF .EQ. 2) THEN
                WRITE(STRING(resHed%filHead(iFil)%nSatel+1),155)
     1                SVNHLP(1),SVNHLP(2)
155             FORMAT(':',I2,'-',I2)
              ENDIF
              KMAX=resHed%filHead(iFil)%nSatel+1
            END IF
          END IF
          IF(IEPOCH.GT.FROMTO(2))GO TO 180
          GOTO 170
180     CONTINUE
        IF(KMAX.NE.0.AND.RESMAX.GE.RESSIZ)THEN
          IF(IEPOL2.NE.0.AND.IEPOLD-IEPOL2.NE.1) THEN
            LINE=LINE+1
            IF(LINE.GT.MAXEPO) THEN
              WRITE(LFNERR,172) MAXEPO,LINE
              DO jFil = 1,reshed%nFil
                DEALLOCATE(resHed%filHead(jFil)%numSat,stat=irc)
              ENDDO
              DEALLOCATE(resHed%filHead,stat=irc)
              IRCODE=3
              RETURN
            ENDIF
            WRITE(BSCROL(LINE),173)
          ENDIF
          LINE=LINE+1
          IF(LINE.GT.MAXEPO) THEN
            WRITE(LFNERR,172) MAXEPO,LINE
            DO jFil = 1,reshed%nFil
              DEALLOCATE(resHed%filHead(jFil)%numSat,stat=irc)
            ENDDO
            DEALLOCATE(resHed%filHead,stat=irc)
            IRCODE=3
            RETURN
          ENDIF
          WRITE(BSCROL(LINE),171) IEPOLD,(STRING(K),K=1,KMAX)
        END IF
C
C DISPLAY OF TYPE 2
C -----------------
      ELSE
C
C LOOP OVER RESIDUALS
C READ L1 RESIDUAL
270       READ(LFNRES,END=280) IFIL2(1),IEPOCH,IFREQ,
     1                         (SVNHL2(K,1),K=1,2),RESHL2(1),RESFL1
          IF(IFREQ.NE.1) GOTO 270
C
C READ L2 RESIDUAL
190       READ(LFNRES,END=280) IFIL2(2),IEPO2,IFREQ,
     1                         (SVNHL2(K,2),K=1,2),RESHL2(2),RESFL2
          IF(IFREQ.EQ.1) THEN
            IFIL2(1)=IFIL2(2)
            IEPOCH=IEPO2
            SVNHL2(1,1)=SVNHL2(1,2)
            SVNHL2(2,1)=SVNHL2(2,2)
            RESHL2(1)=RESHL2(2)
            RESFL1=RESFL2
            GOTO 190
          ENDIF
C
C L2 CORRESPONDS TO L1 ?
          IF(IFREQ      .NE.2          .OR.
     1       IFIL2(1)   .NE.IFIL2(2)   .OR.
     2       IEPOCH     .NE.IEPO2      .OR.
     3       SVNHL2(1,1).NE.SVNHL2(1,2).OR.
     4       SVNHL2(2,1).NE.SVNHL2(2,2)) GOTO 270
C
C CHECK DISPLAY INTERVAL
          IF(IEPOCH.LT.FROMTO(1))GO TO 270
          IF(IEPOCH.GT.FROMTO(2))GO TO 280
C
          IF(IFIL2(1).EQ.IFIL)THEN
            IF(IEPOCH.NE.IEPOLD)THEN
              IF(IEPOLD.NE.0.AND.RESMAX.GE.RESSIZ)THEN
                IF(IEPOL2.NE.0.AND.IEPOLD-IEPOL2.NE.1) THEN
                  LINE=LINE+1
                  IF(LINE.GT.MAXEPO) THEN
                    WRITE(LFNERR,172) MAXEPO,LINE
                    DO jFil = 1,reshed%nFil
                      DEALLOCATE(resHed%filHead(jFil)%numSat,stat=irc)
                    ENDDO
                    DEALLOCATE(resHed%filHead,stat=irc)
                    IRCODE=3
                    RETURN
                  END IF
                  WRITE(BSCROL(LINE),173)
                ENDIF
                LINE=LINE+1
                IF(LINE.GT.MAXEPO) THEN
                  WRITE(LFNERR,172) MAXEPO,LINE
                  DO jFil = 1,reshed%nFil
                    DEALLOCATE(resHed%filHead(jFil)%numSat,stat=irc)
                  ENDDO
                  DEALLOCATE(resHed%filHead,stat=irc)
                  IRCODE=3
                  RETURN
                END IF
                WRITE(BSCROL(LINE),171) IEPOLD,(STRING(K),K=1,KMAX)
              END IF
              DO 260 KSAT=1,resHed%filHead(iFil)%nSatel+1
                STRING(KSAT)=' '
260           CONTINUE
              IEPOL2=IEPOLD
              IEPOLD=IEPOCH
              RESMAX=0.D0
              KMAX=0
            END IF
C
            IF(ICARR.EQ.1) THEN
              RESFLG=RESFL1
            ELSE IF(ICARR.EQ.2) THEN
              RESFLG=RESFL2
            ELSE
              RESFLG=RESFL1
              IF(RESFLG.EQ.' '.AND.RESFL2.NE.' ') RESFLG=RESFL2
            ENDIF
C ???????? SVNHL2(1,1) ???????????
            CALL LINCOM(ICARR,SVNHL2(1,1),RESHL2(1),RESHL2(2),
     1                  FLG(1),FLG(2),RESHLP,FLG(3))
            IF(IUNIT.EQ.1) THEN
              ISVN=SVNHL2(1,1)
              WAVE=DABS(
     1               (FLOAT(ICYC(1))*WLGT(1,ISVN)*FACLIN(ICARR,1,ISVN)+
     2                FLOAT(ICYC(2))*WLGT(2,ISVN)*FACLIN(ICARR,2,ISVN))
     3                /FLOAT(IWLFAC))
              RESHLP=RESHLP/WAVE
            ELSE IF(IUNIT.EQ.2) THEN
              RESHLP=RESHLP*1000
            ENDIF
            IF(DABS(RESHLP).GT.RESMAX) RESMAX=DABS(RESHLP)
            IPOS1=INDSVN(SVNHL2(1,1))
            IF(IPOS1.EQ.0) GOTO 270
            IF (MDIFF .EQ. 2) THEN
              IPOS2=INDSVN(SVNHL2(2,1))
              IF(IPOS2.EQ.0) GOTO 270
            ENDIF
            IF(STRING(IPOS1)(1:5).EQ.' ') THEN
              IF(IUNIT.EQ.1) THEN
                WRITE(STRING(IPOS1),151,IOSTAT=IOSTAT) RESHLP,RESFLG
              ELSE IF(IUNIT.EQ.2) THEN
                WRITE(STRING(IPOS1),152,IOSTAT=IOSTAT) IDNINT(RESHLP),
     1                                                 RESFLG
              ELSE
                WRITE(STRING(IPOS1),153,IOSTAT=IOSTAT) RESHLP,RESFLG
              END IF
              IF (MDIFF .EQ. 1) THEN
                IF (IPOS1 .GT. KMAX) KMAX = IPOS1
              ELSE IF (MDIFF .EQ. 2) THEN
                IF(IPOS2-1.NE.IPOS1) THEN
                  WRITE(STRING(IPOS2-1)(1:5),154)
                  IF(IPOS2.GT.KMAX) KMAX=IPOS2
                ELSE
                  IF(IPOS1.GT.KMAX) KMAX=IPOS1
                END IF
              ENDIF
            ELSE
              IF(IUNIT.EQ.1) THEN
                WRITE(STRING(resHed%filHead(iFil)%nSatel),151,
     1                IOSTAT=IOSTAT) RESHLP,RESFLG
              ELSE IF(IUNIT.EQ.2) THEN
                WRITE(STRING(resHed%filHead(iFil)%nSatel),152,
     1                IOSTAT=IOSTAT) IDNINT(RESHLP),RESFLG
              ELSE
                WRITE(STRING(resHed%filHead(iFil)%nSatel),153,
     1                IOSTAT=IOSTAT) RESHLP,RESFLG
              END IF
              IF (MDIFF .EQ. 1) THEN
                WRITE(STRING(resHed%filHead(iFil)%nSatel+1),156)
     1                SVNHL2(1,1)
              ELSE IF (MDIFF .EQ. 2) THEN
                WRITE(STRING(resHed%filHead(iFil)%nSatel+1),156)
     1                SVNHL2(1,1),SVNHL2(2,1)
              ENDIF
              KMAX=resHed%filHead(iFil)%nSatel+1
            END IF
          END IF
          GOTO 270
280     CONTINUE
        IF(KMAX.NE.0.AND.RESMAX.GE.RESSIZ)THEN
          IF(IEPOL2.NE.0.AND.IEPOLD-IEPOL2.NE.1) THEN
            LINE=LINE+1
            IF(LINE.GT.MAXEPO) THEN
              WRITE(LFNERR,172) MAXEPO,LINE
              DO jFil = 1,reshed%nFil
                DEALLOCATE(resHed%filHead(jFil)%numSat,stat=irc)
              ENDDO
              DEALLOCATE(resHed%filHead,stat=irc)
              IRCODE=3
              RETURN
            ENDIF
            WRITE(BSCROL(LINE),173)
          ENDIF
          LINE=LINE+1
          IF(LINE.GT.MAXEPO) THEN
            WRITE(LFNERR,172) MAXEPO,LINE
            DO jFil = 1,reshed%nFil
              DEALLOCATE(resHed%filHead(jFil)%numSat,stat=irc)
            ENDDO
            DEALLOCATE(resHed%filHead,stat=irc)
            IRCODE=3
            RETURN
          ENDIF
          WRITE(BSCROL(LINE),171) IEPOLD,(STRING(K),K=1,KMAX)
        END IF
      END IF
C
C  DISPLAY RESIDUALS
      NTITC=7
      NCTOT=(resHed%filHead(iFil)%nSatel+1)*6+NTITC
      NRTOT=LINE
cccc       IF(LFN.EQ.0.OR.LFN.EQ.LFNPRT) THEN
cccc         NTITR=2
cccc         NCOL=72
cccc         IF(NROW.EQ.0) THEN
cccc           WRITE(*,301)
cccc 301       FORMAT(' NUMBER OF LINES TO BE DISPLAYED (18/.../26) : ')
cccc         END IF
cccc C_BEG_IBM_MVS
cccc C       WRITE(*,302)
cccc C302    FORMAT(' *** PRESS <CLEAR> AND <ENTER> ')
cccc C       READ(*,303) CHR1
cccc C303    FORMAT(A)
cccc C_END_IBM_MVS
cccc         CALL SCROLL(BSCROL,NRTOT,NCTOT,NTITR,NTITC,NROW,NCOL)
cccc       ELSE
C
C  STORE RESIDUALS
        DO 310 I=1,NRTOT
          WRITE(LFN,311) BSCROL(I)(1:NCTOT)
311       FORMAT(A)
310     CONTINUE
ccccc      END IF
C
      DO jFil = 1,reshed%nFil
        DEALLOCATE(resHed%filHead(jFil)%numSat,stat=irc)
      ENDDO
      DEALLOCATE(resHed%filHead,stat=irc)
      CLOSE (lfnres)
C
      RETURN
      END SUBROUTINE

      END MODULE
