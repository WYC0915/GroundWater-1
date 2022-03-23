      MODULE s_DEFSRC
      CONTAINS

C*
      SUBROUTINE DEFSRC(XNSIG,DLTMIN,RMS,Q,NPP1,NAMB,X,LOCQ,NTOT,
     1                  NVAL,MAXVAL,AMBVAL,CONDLC,
     2                  NAMBL1,MATCH,AMBCOR)
CC
CC NAME       :  DEFSRC
CC
CC PURPOSE    :  DEFINE AMBIGUITY SEARCH PARAMETERS FOR
CC               STRATEGY 2 (GENERAL SEARCH BASED ON
CC               RMS ERRORS FOR AMBIGUITIES AND ALL LINEAR
CC               COMBINATIONS).
CC
CC PARAMETERS :
CC         IN :  XNSIG  : CONFIDENCE INTERVAL IN MULTIPLES    R*8
CC                        OF RMS - ERROR
CC               DLTMIN : SEARCH WIDTH FOR GEOMETRY-FREE      R*8
CC                        LC (IN L1 CYCLES)
CC                        =X : USE THIS VALUE
CC                        =0 : COMPUTE FORMAL WIDTH
CC               RMS    : ROOT MEAN SQUARE ERROR OF ADJUST.   R*8
CC               Q(I),=1,2,..: (INVERTED NEQ-MATRIX)          R*8
CC               NPP1   : NUMBER OF PARAMETERS WITHOUT AMBI-  R*8
CC                        GUITIES
CC               NAMB   : NUMBER OF AMBIGUITIES               R*8
CC               X(I),I=1,2,..,(NPP1+NAMB): PARAMETER VECTOR  R*8
CC               LOCQ(K,I),K=1,2,..,MXCLCQ,I=1,2,..,NAMB      I*4
CC                        PARAMETER DESCRIPTION ARRAY
CC        OUT :  NTOT   : TOTAL NUMBER OF DIFFERENT AMBIGUITY R*8
CC                        SETS
CC               NVAL(I),I=1,2,..,NVAL: NUMBER OF AMBIGUITY   I*4
CC                        VALUES FOR EACH AMBIGUITY
CC               AMBVAL(K,I),K=1,2,..,MAXVAL,I=1,2,..,NAMB:   R*8
CC                        AMBIGUITY VALUES
CC               CONDLC(K,I),K=1,2, I=1,2,..,NAMB*(NAMB-1)/2  R*8
CC                        MINIMUM, MAXIMUM VALUES FOR DIFF.
CC                        BETWEEN AMBIGUITY VALUES
CC               NAMBL1 : NUMBER OF L1-AMBIGUITIES            I*4
CC               MATCH  : MATCH(1,I) : INDEX OF L1 AMBIGUITY  I*4
CC                        MATCH(2,I) : INDEX OF CORRESPONDING
CC                                     L2 AMBIGUITY
CC               AMBCOR(I,IAMB) : CORRESPONDING L2 AMBIGUITY  R*8
CC                        TO L1 AMBIGUITY IAMB
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 94)
CC
CC CREATED    :  89/07/03 15:07
CC
CC CHANGES    :  31-JUL-92 : LM: NEW OPTION - MINIMAL VALUE FOR
CC                               DELTA "DLTMIN" AS INPUT
CC               25-JUN-93 :     MXLVAL=60
CC               10-AUG-94 : MR: CALL EXITRC
CC               12-AUG-94 : SS: "DLTMIN" ALWAYS USED
CC               22-AUG-94 : SS: NEW LOGIC FOR "DLTMIN"
CC               30-AUG-03 : HU: SHARED DO LABELS REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE f_ikf
      USE s_errlic
      USE s_neaint
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAMB  , IAMB1 , IL1   , IL2   , ILW   , ILWL1 ,
     1          ILWL2 , IND   , INDII , INDIK , INDKK , IUP   , IUPL1 ,
     2          IUPL2 , K     , KAMB  , KAMB1 , L1B   , L2B   , MAXVAL,
     3          MXCLCQ, MXLVAL, NAMB  , NAMBL1, NMATCH, NPAIR , NPP1
C
      REAL*8    AMBACT, AMBLC , BEST  , DELTA , DLTMIN, RMS   , RMSAMB,
     1          SIGMIN, TEST  , XAMB  , XI    , XLW   , XLWL1 , XLWL2 ,
     2          XNSIG , XUP   , XUPL1 , XUPL2
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      PARAMETER (MXLVAL=60)
C
      CHARACTER*6 MXNLCQ
      REAL*8      NTOT,Q(*),X(*),HELP(MXLVAL)
      INTEGER*4   NVAL(*),AMBVAL(MAXVAL,*),CONDLC(2,*),LOCQ(MXCLCQ,*)
      INTEGER*4   MATCH(2,*),AMBCOR(MAXVAL,*)
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
C CHECK LOCAL DIMENSION
C ---------------------
      IF(MXLVAL.LT.MAXVAL)THEN
        WRITE(LFNERR,1)MXLVAL,MAXVAL
1       FORMAT(/,' *** SR DEFSRC: LOCAL DIMENSION MXLVAL=',I5,
     1           ' TOO SMALL',
     2                     /,16X,'MAX. VALUE EXTERNAL   =',I5,/)
        CALL EXITRC(2)
      END IF
C
C FIND ALL L1 AMBIGUITIES AND CORRESPONDING L2 AMBIGUITIES
C --------------------------------------------------------
      NAMBL1=0
      NMATCH=0
      DO 2020 IAMB=1,NAMB
        IF (LOCQ(1,NPP1+IAMB).EQ.4.AND.
     1      LOCQ(5,NPP1+IAMB).EQ.1) THEN
           NAMBL1=NAMBL1+1
           MATCH(1,NAMBL1)=IAMB
           MATCH(2,NAMBL1)=0
           DO 2010 KAMB=IAMB+1,NAMB
             IF (LOCQ(2,NPP1+KAMB).EQ.LOCQ(2,NPP1+IAMB).AND.
     1           LOCQ(3,NPP1+KAMB).EQ.LOCQ(3,NPP1+IAMB).AND.
     2           LOCQ(6,NPP1+KAMB).EQ.LOCQ(6,NPP1+IAMB).AND.
     3           LOCQ(5,NPP1+KAMB).EQ.2) THEN
               NMATCH=NMATCH+1
               MATCH(2,NAMBL1)=KAMB
             END IF
2010       CONTINUE
        END IF
2020  CONTINUE
C
C ADD UNPAIRED AMBIGUITIES
C ------------------------
      IF (NAMBL1+NMATCH.LT.NAMB) THEN
        DO 2040 IAMB=1,NAMB
          IF (LOCQ(5,NPP1+IAMB).NE.1) THEN
            DO 2030 KAMB=1,NAMB
              IF (MATCH(2,KAMB).EQ.IAMB) GOTO 2040
2030        CONTINUE
          END IF
          NAMBL1=NAMBL1+1
          MATCH(1,NAMBL1)=IAMB
          MATCH(2,NAMBL1)=0
2040    CONTINUE
      END IF
C
C DEFINE SEARCH PARAMETERS FOR "L1" - AMBIGUITIES
C -----------------------------------------------
      NTOT=1.D0
      DO 30 IAMB1=1,NAMBL1
        IAMB=MATCH(1,IAMB1)
        CALL ERRLIC(RMS,Q,NPP1+IAMB,NPP1+IAMB,1.D0,0.D0,RMSAMB)
        XUPL1=X(NPP1+IAMB)+XNSIG*RMSAMB
        IUPL1=DNINT(XUPL1)
        IF (XUPL1.GT.IUPL1) IUPL1=IUPL1+1
        XLWL1=X(NPP1+IAMB)-XNSIG*RMSAMB
        ILWL1=DNINT(XLWL1)
        IF (XLWL1.LT.ILWL1) ILWL1=ILWL1-1
C
C FIND ALL POSSIBLE VALUES FOR AMBIGUITY PAIRS
C --------------------------------------------
        IF (MATCH(2,IAMB1).NE.0) THEN
          KAMB=MATCH(2,IAMB1)
          CALL ERRLIC(RMS,Q,NPP1+KAMB,NPP1+KAMB,1.D0,0.D0,RMSAMB)
          XUPL2=X(NPP1+KAMB)+XNSIG*RMSAMB
          IUPL2=DNINT(XUPL2)
          IF (XUPL2.GT.IUPL2) IUPL2=IUPL2+1
          XLWL2=X(NPP1+KAMB)-XNSIG*RMSAMB
          ILWL2=DNINT(XLWL2)
          IF (XLWL2.LT.ILWL2) ILWL2=ILWL2-1
C
C BEST LINEAR COMBINATION OF TYPE IAMB+XI*KAMB
C --------------------------------------------
          INDII=IKF(NPP1+IAMB,NPP1+IAMB)
          INDKK=IKF(NPP1+KAMB,NPP1+KAMB)
          INDIK=IKF(NPP1+IAMB,NPP1+KAMB)
          XI=-Q(INDIK)/Q(INDKK)
C
C DEFINE SEARCH WIDTH FOR L4 LC
C -----------------------------
          IF (DLTMIN.EQ.0.D0) THEN
            SIGMIN=RMS*DSQRT(Q(INDII)+2*XI*Q(INDIK)+XI**2*Q(INDKK))
            DELTA=XNSIG*SIGMIN
          ELSE
            DELTA=DLTMIN
          END IF
C
C FIND ALL ALLOWED COMBINATIONS
C -----------------------------
          NPAIR=0
          BEST=1.D20
          AMBLC=X(NPP1+IAMB)+XI*X(NPP1+KAMB)
          DO 40 IL1=ILWL1,IUPL1
            DO IL2=ILWL2,IUPL2
              AMBACT=IL1+XI*IL2
              TEST=DABS(AMBACT-AMBLC)
              IF (TEST.LT.BEST) THEN
                L1B=IL1
                L2B=IL2
                BEST=TEST
              END IF
C
C ACCEPTABLE COMBINATION FOUND
C ----------------------------
              IF (TEST.LE.DELTA) THEN
                NPAIR=NPAIR+1
C
C STOP PROCESSING IF DIMENSION AMBVAL TOO SMALL
C ---------------------------------------------
                IF (NPAIR.GT.MAXVAL) THEN
                  WRITE(LFNERR,11)IAMB,NPAIR,MAXVAL
11                FORMAT(/,' *** SR DEFSRC: AMBIGUITY NO. ',I5,/,
     1                   16X,'# VALUES          =',I5,' TOO LARGE',/,
     2                   16X,'MAXIMUM DIMENSION =',I5,/)
                  CALL EXITRC(2)
                END IF
                AMBVAL(NPAIR,IAMB1)=IL1
                AMBCOR(NPAIR,IAMB1)=IL2
              END IF
            ENDDO
40        CONTINUE
C
C SELECT BEST COMBINATION, IF NO VALUE WAS ACCEPTABLE
C ---------------------------------------------------
          IF (NPAIR.EQ.0) THEN
            NPAIR=1
            AMBVAL(NPAIR,IAMB1)=L1B
            AMBCOR(NPAIR,IAMB1)=L2B
          END IF
          NVAL(IAMB1)=NPAIR
          NTOT=NTOT*NPAIR
        ELSE
          NVAL(IAMB1)=IUPL1-ILWL1+1
          NTOT=NTOT*NVAL(IAMB1)
C
C STOP PROCESSING IF DIMENSION AMBVAL TOO SMALL
C ---------------------------------------------
          IF (NVAL(IAMB1).GT.MAXVAL) THEN
            WRITE(LFNERR,11)IAMB,NVAL(IAMB1),MAXVAL
            CALL EXITRC(2)
          END IF
          CALL NEAINT(NVAL(IAMB),X(NPP1+IAMB),HELP)
          DO 20 K=1,NVAL(IAMB1)
            AMBVAL(K,IAMB1)=HELP(K)
20        CONTINUE
        END IF
30    CONTINUE
C
C DEFINE CONDITIONS FOR DIFFERENCES BETWEEN AMBIGUITIES
C -----------------------------------------------------
      IND=0
      DO 100 IAMB1=1,NAMBL1
        IAMB=MATCH(1,IAMB1)
        DO 90 KAMB1=IAMB1+1,NAMBL1
          KAMB=MATCH(1,KAMB1)
          CALL ERRLIC(RMS,Q,NPP1+IAMB,NPP1+KAMB,1.D0,-1.D0,RMSAMB)
          XAMB=X(NPP1+IAMB)-X(NPP1+KAMB)
          XUP=XAMB+XNSIG*RMSAMB
          XLW=XAMB-XNSIG*RMSAMB
          IUP=DNINT(XUP)
          IF (IUP.GT.XUP) IUP=IUP-1
          ILW=DNINT(XLW)
          IF (ILW.LT.XLW) ILW=ILW+1
          IF (ILW.GT.IUP) THEN
            ILW=DNINT(XAMB)
            IUP=ILW
          END IF
          IND=IND+1
          CONDLC(1,IND)=ILW
          CONDLC(2,IND)=IUP
90      CONTINUE
100   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
