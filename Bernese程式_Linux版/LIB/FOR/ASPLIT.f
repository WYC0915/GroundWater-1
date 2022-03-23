      MODULE s_ASPLIT
      CONTAINS
C*
      SUBROUTINE ASPLIT(TMJD,NSAT,SVN,RMSSAT,FACLIM,EDIT,NBAD,SVNBAD,
     1                  RMSBAD,IRC)
CC
CC NAME       :  ASPLIT
CC
CC PURPOSE    :  SET ARC SPLIT IN SATCRUX FILE BASED ON ORBGEN OUTPUT
CC
CC PARAMETER  :
CC        IN  :  TMJD    : ARC "SPLIT" TIME                    R*8
CC               NSAT    : NUMBER OF SATELLITES                I*4
CC               SVN     : SATELLITE NUMBERS                   I*4(*)
CC               RMSSAT  : RMS OF SATELLITE (DEFSTD)           R*8(*)
CC               FACLIM  : ARC SPLIT OPTIONS                   R*8(2,*)
CC                         1: FACTOR WRT MEAN RMS
CC                         2: LOWER LIMIT FOR RMS (CM)
CC        OUT :  EDIT    : COMPUTED EDITING LEVEL              R*8(MAXSYS)
CC               NBAD    : NUMBER OF "SPLIT" SATELLITES        I*4
CC               SVNBAD  : PRN NUMBERS OF "SPLIT" SATS         I*4(*)
CC               RMSBAD  : RMS OF "SPLIT" SATELLITE            R*8(*)
CC               IRC     : NOT ALL INFO FOUND                  I*4
CC
CC REMARKS    :  ASSUMES TMJD IS TIME FOR "SPLIT"
CC
CC AUTHOR     :  T.A. SPRINGER
CC
CC CREATED    :  24-MAR-97
CC
CC CHANGES    :  12-SEP-97 : TS: EDIT LEVEL CHANGED TO 4*RMSMEA (WAS 3*)
CC               23-SEP-97 : DI: REMOVE UNUSED PARAMETER 'MAXSAT'
CC               05-JAN-00 : SS: "MAXMAN" FROM 1000 TO 2000
CC               30-OCT-01 : DI: WRITE SVN NUMBERS WITH I3
CC               07-OCT-03 : SS: PREVENT ARC SPLITTING IN CASE OF
CC                               NEGATIVE PRN NUMBERS
CC               07-JAN-04 : SS: USE FUNCTION "MODSVN"
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               24-JUL-07 : MM: USE 1 CM IF RMSSAT EQUALS TO 0 CM
CC               26-FEB-08 : RD: USE GTSATS FROM D_SATCRX
CC               28-JUL-09 : SS: ARC SPLIT OPTIONS SPECIFIC TO EACH GNSS
CC               26-AUG-11 : SL: USE M_BERN WITH ONLY, EDIT TO PARAMETER LIST
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnErr, lfnLoc
      USE m_global, ONLY: maxsys
      USE d_satcrx, ONLY: gtsats
      USE s_opnfil
      USE f_modsvn
      USE s_opnerr
      USE s_jmt
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IBAD  , ID    , ILIN  , IM    , IOSTAT, IRC   ,
     1          IRCCRX, ISYS  , IY    , J     , K     , MAXLIN, MAXMAN,
     2          NBAD  , NMAN  , NSAT
C
      REAL*8    DD    , TMJD
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER(MAXMAN=2000,MAXLIN=10000)
C
      CHARACTER*80 CRXLIN(MAXLIN)
      CHARACTER*32 FILNAM
C
      INTEGER*4    SVN(*),SVNBAD(*),SVNMAN(MAXMAN)
      INTEGER*4    NUMTOT(MAXSYS),NUMSAV(MAXSYS)
C
      REAL*8       RMSSAT(*),RMSBAD(*),TIMMAN(MAXMAN),FACLIM(2,*)
      REAL*8       RMSTOT(MAXSYS),RMSSAV(MAXSYS),EDIT(MAXSYS)
C
C
C GET "TOTAL" RMS
C ---------------
      RMSTOT(:)=0D0
      NUMTOT(:)=0
      DO 10 I=1,NSAT
        ISYS=SVN(I)/100+1
        RMSTOT(ISYS)=RMSTOT(ISYS)+RMSSAT(I)
        IF (RMSSAT(I).EQ.0.0D0) RMSTOT(ISYS)=RMSTOT(ISYS)+0.01D0
        NUMTOT(ISYS)=NUMTOT(ISYS)+1
10    CONTINUE
C
C SET/SAVE SOME VARIABLES
C -----------------------
      NBAD=0
      RMSSAV(:)=RMSTOT(:)
      NUMSAV(:)=NUMTOT(:)
C
C DETERMINE IF THERE ARE ANY "BAD" SATELLITES
C -------------------------------------------
20    IBAD=0
      EDIT(:)=0D0
      DO ISYS=1,MAXSYS
        IF (NUMTOT(ISYS).GT.0)
     1    EDIT(ISYS)=FACLIM(1,ISYS)*RMSTOT(ISYS)/(NUMTOT(ISYS)*1D0)
      ENDDO
      RMSTOT(:)=RMSSAV(:)
      NUMTOT(:)=NUMSAV(:)
      DO 30 I=1,NSAT
        ISYS=SVN(I)/100+1
        IF (EDIT(ISYS).NE.0D0 .AND.
     1      RMSSAT(I).GE.EDIT(ISYS) .AND.
     2      RMSSAT(I).GE.FACLIM(2,ISYS)) THEN
          RMSTOT(ISYS)=RMSTOT(ISYS)-RMSSAT(I)
          NUMTOT(ISYS)=NUMTOT(ISYS)-1
          IBAD=IBAD+1
          SVNBAD(IBAD)=MODSVN(SVN(I))
          RMSBAD(IBAD)=RMSSAT(I)
        ENDIF
30    CONTINUE
      IF (IBAD.GT.NBAD) THEN
        NBAD=IBAD
        GOTO 20
      ENDIF
C
C IF "BAD" SATELLITES UPDATE STACRUX FILE IF NECESSARY
C ----------------------------------------------------
      IF (NBAD.GT.0) THEN
C
C READ ARC-SPLITS FROM STACRUX FILE
C ---------------------------------
        CALL GTSATS(MAXMAN,NMAN,SVNMAN,TIMMAN)
C
C CHECK IF "BAD" SATELLITE ALREADY IN ARC
C ---------------------------------------
        DO 100 I=1,NMAN
          IF (TIMMAN(I).EQ.TMJD) THEN
            DO 110 J=1,NBAD
              IF (IABS(SVNMAN(I)).EQ.SVNBAD(J)) THEN
                DO 120 K=J,NBAD-1
                  SVNBAD(K)=SVNBAD(K+1)
                  RMSBAD(K)=RMSBAD(K+1)
120             CONTINUE
                NBAD=NBAD-1
              ENDIF
110         CONTINUE
          ENDIF
100     CONTINUE
      ENDIF
C
C APPEND BAD SATELLITE TO SATCRUX
C -------------------------------
      CALL GTFLNA(0,'SATCRUX',FILNAM,IRCCRX)
      IF (NBAD.GT.0 .AND. IRCCRX.EQ.0) THEN
C
C OPEN SATCRUX FILE FOR READING
C -----------------------------
        CALL OPNFIL(LFNLOC,FILNAM,'OLD','FORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILNAM,'ASPLIT')
C
C READ ALL LINES INTO BUFFER
C --------------------------
        DO 200 I=1,MAXLIN
          READ(LFNLOC,201,END=210,ERR=210)CRXLIN(I)
201       FORMAT(A80)
200     CONTINUE
C
C OPEN SATCRUX FILE FOR WRITING
C -----------------------------
210     CONTINUE
        ILIN=I-1
        CLOSE(LFNLOC)
        IBAD=NBAD
        CALL OPNFIL(LFNLOC,FILNAM,'NEW','FORMATTED',' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILNAM,'ASPLIT')
        DO 220 I=1,ILIN
          IF ((I.GT.6).AND.(CRXLIN(I)(1:6).EQ.'      ')) THEN
            DO 230 J=1,IBAD
              CALL JMT(TMJD,IY,IM,DD)
              ID=IDNINT(DD)
              WRITE(LFNLOC,231)SVNBAD(J),IY,IM,ID
231           FORMAT(2X,I3,9X,'4',9X,'0',7X,I4,1X,I2.2,1X,I2.2,
     1          ' 00 00 00 ')
230         CONTINUE
            IBAD=0
          ENDIF
          WRITE(LFNLOC,201)CRXLIN(I)
220     CONTINUE
        CLOSE(LFNLOC)
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
