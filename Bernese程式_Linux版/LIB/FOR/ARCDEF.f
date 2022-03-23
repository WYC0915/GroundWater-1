      MODULE s_ARCDEF
      CONTAINS

C*
      SUBROUTINE ARCDEF(NPAR,LOCQ,IFIL,NSFIL,SVNFIL,NUMSTC,SVNSTC,
     1                  NUMGAP,SVNGAP,ELEFIL,RPRFIL,TOSC,NUMNAC,NEWARC,
     2                  NSEFF,SVNEFF,ARCNUM,MANOVR,ELE,RPRESS)
CC
CC NAME       :  ARCDEF
CC
CC PURPOSE    : CREATE/UPDATE ACTUAL LIST OF SATELLITES (NSEFF,SVNEFF),
CC              GAP-HANDLING (GAPHDL), AND ARRAY ARCNUM, WHERE ARCNUM IS
CC              A TWO-DIMENSIONAL ARRAY (LINE I CORRESPONDING TO SATELLITE
CC              SVNEFF(I), COLUMN K CORRESPONDING TO FILE (ARC) NUMBER K.
CC              AN EXAMPLE LOOKS AS FOLLOWS
CC
CC
CC                       THE ARRAY ARCNUM
CC                    -------------------------
CC                    |        FILE(ARC)      |
CC              -------------------------------
CC              | SAT |  1  2  3  4  5  ..  M |
CC              -------------------------------
CC              |  1  |  1  1  1  1  1  ..  1 |
CC              |  2  |  1  1  0  2  2  ..  2 |
CC              |  3  |  1  1  1  0  1      1 |
CC              |  4  |  1  1  2  2  3      3 |
CC              | ... |         .....         |
CC              | ... |         .....         |
CC              |  N  |         .....         |
CC              -------------------------------
CC                       THE ARRAY ARCNUM
CC
CC               N=NUMBER OF SATELLITES, M=NUMBER OF FILES( OR ARCS)
CC               THE ELEMENTS OF THE ARRAY SPECIFY THE ARC NUMBER
CC               FO A SPECIFICIC ARC AND SATELLITE:
CC               ARCNUM(I,K) = J MEANS THAT FOR SATELLITE I FILE
CC               NUMBER K BELONGS TO ARC NUMBER J. J=0 MEANS THAT
CC               FOR THE PARTICULAR DAY NO OBSERVATIONS ARE PRESENT.
CC
CC               INITIALLY THE SUBROUTINE IS CALLED WITH NSEFF=0.
CC
CC     EXAMPLES : SAT 1 : NORMAL CASE, ALL FILES BELONG TO THE SAME ARC
CC                    2 : NORMAL GAP HANDLING : A NEW ARC IS SET UP AFTER
CC                        A GAP OF OF ONE (OR MORE FILES)
CC                    3 : SPECIAL GAP HANDLING : ARC NO 3 IS EXTRAPOLATED
CC                        TO THE START OF ARC NO 5. THIS OPTION IS ONLY
CC                        POSSIBLE, IF THE A PRIORI STANDARD ORBIT AND
CC                        RPR FILE ARE COVERING THE TIME INTERVAL OF THE
CC                        MISSING ARC(S). THE SATELLITE NUMBER MUST BE IN
CC                        THE LIST SVNGAP(K),K=1,2,..NUMGAP.
CC                    4 : THE USER MAY SET UP NEW ARCS AT ALL THE ARC-
CC                        BOUNDARIES.
CC
CC PARAMETERS :
CC        IN  : NPAR    : NUMBER OF PARAMETERS                      I*4
CC              LOCQ    : PARAMETER DESCRIPTION                     I*4(*,*)
CC              IFIL    : CURRENT FILE NUMBER                       I*4
CC              NSFIL   : NUMBER OF SATELLITES IN CURRENT FILE      I*4
CC              SVNFIL  : SATELLITE NUMBER IN CURRENT FILE          I*4(*)
CC              NUMSTC  : NUMBER OF SATELLITES FOR WHICH STOCHASTIC I*4
CC                        PARAMETERS HAVE TO BE SET UP AT BOUNDARIES
CC              SVNSTC  : CORRESPONDING SATELLITE LIST              I*4(*)
CC              NUMGAP  : NUMBER OF SATELLITES WITH SPECIAL         I*4
CC                        GAP-HANDLING
CC              SVNGAP  : CORRESPONDING SATELLITE NUMBERS           I*4(*)
CC              ELEFIL  : ELEMENTS IN CURRENT FILE                  R*8(*,*)
CC              RPRFIL  : RPR PARAMETERS IN CURRENT FILE            R*8(*,*)
CC              TOSC    : OSCULATING EPOCH OF FILE IFIL             R*8
CC     IN/OUT : NUMNAC  : NUMBER OF NEW ARCS TO BE SET UP           I*4
CC              NEWARC  : DEFINITION OF NEW ARCS                    I*4(*,*)
CC                        NEWARC(1,I) = SVN
CC                        NEWARC(2,I) = FILE NUMBER
CC              NSEFF   : TOTAL NUMBER OF SATELLITES                I*4
CC              SVNEFF  : SATELLITE NUMBERS                         I*4(*)
CC              ARCNUM  : ARRAY CONTAINING THE ARC NUMBERS          I*4(*,*)
CC              MANOVR  : WAS THERE A MANEOUVRE FOR A PARTICULAR    I*4(*)
CC                        SATELLITE ?
CC                        MANOVR(ISAT)=0    : NO MANEOUVRE
CC                                    =IFIL : MANEOUVRE IN FILE IFIL
CC              ELE     : ELEMENT SET ACCORDING TO ORDER GIVEN      R*8(*,*)
CC                        BY SVNEFF
CC              RPRESS  : RPR SETS ACCORDING TO ORDER IN SVNEFF     R*8(*,*)
CC
CC REMARKS    :
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.5  (MAY 94)
CC
CC CREATED    :  94/05/11
CC
CC CHANGES    :  07-MAR-96 : EB: SPLIT UP OF ARC ALSO VIA SATCRUX FILE
CC               29-JUL-97 : LM: CORRECT ARCNUM INITIALIZATION
CC               27-AUG-98 : MR: USE FUNCTION "MODSVN"
CC               05-JAN-00 : SS: "MAXMAN" FROM 1000 TO 2000
CC               07-JAN-04 : SS: "MODSVN" USED WRT SPLIT UP OF ARCS
CC               07-JAN-04 : SS: 10 YEARS OLD BUG FIXED
CC               26-JAN-04 : HU: NO IF STATEMENT WITH UNDEFINED INDEX
CC               30-APR-05 : RD: TAKE INDEX FOR MANOVR FROM SVNEFF
CC                               (INSTEAD OF SVNFIL)
CC               15-APR-05 : RD: MANEUVER OF STOCH. PULSES (SVNSTC)
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               26-FEB-08 : RD: USE GTSATS FROM D_SATCRX
CC               18-NOV-09 : RD: SPECIAL INDEXING FOR EARLY MANEUVERS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_satcrx, ONLY: gtsats, gtsatm2
      USE f_modsvn
      USE s_exitrc
      USE s_inlist
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IELE  , IF    , IFIL  , IGAP  , IMAN  , INEW  , IPAR  ,
     1          ISAT  , K     , KSAT  , LSTARC, MAXMAN, MXCDYN,
     2          MXCLCQ, MXCSAT, NMAN  , NPAR  , NSEFF , NSFIL , NUMGAP,
     3          NUMNAC, NUMSTC, NUMTST, NMA2
C
      REAL*8    RLIM  , TOSC
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
      PARAMETER (MAXMAN=2000)
C
      CHARACTER*6 MXNLCQ,MXNSAT,MXNDYN
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMDYN/MXCDYN,MXNDYN
C
      INTEGER*4   LOCQ(MXCLCQ,*),SVNGAP(*),NEWARC(2,*),SVNEFF(*),
     1            ARCNUM(MXCSAT,*),MANOVR(*),SVNFIL(*),SVNSTC(*)
      REAL*8      ELEFIL(7,*),ELE(7,*),RPRFIL(MXCDYN,*),
     1            RPRESS(MXCDYN,*)
      REAL*8      TIMMAN(MAXMAN),TIMMA2(MAXMAN)
      INTEGER*4   SATMAN(MAXMAN),SATMA2(MAXMAN)
C
C 1 HOUR
      DATA        RLIM/.042D0/
C
C READ SATCRUX FILE (SPLITTING UP THE ARCS)
C -----------------------------------------
      IF (IFIL.EQ.1) THEN
        CALL GTSATS(MAXMAN,NMAN,SATMAN,TIMMAN)
        CALL GTSATM2(MAXMAN,NMA2,SATMA2,TIMMA2)
      ENDIF
C
C RE-NUMBER MANOEUVRE SATELLITES IN ARRAY LOCQ :
C --------------------------------------------
      DO 8 IPAR=1,NPAR
        IF(LOCQ(1,IPAR).EQ.3.OR.LOCQ(1,IPAR).EQ.11)THEN
          CALL INLIST(LOCQ(3,IPAR),NSEFF,SVNEFF,ISAT)
          IF (ISAT.GT.0) THEN
            IF(MANOVR(ISAT).GT.0.AND.IFIL.GT.MANOVR(ISAT))THEN
              LOCQ(3,IPAR)=LOCQ(3,IPAR)+50
            END IF
          ENDIF
        END IF
8     CONTINUE
C
C RENUMBER MANOEUVRE SATELLITES IN ARRAY SVNFIL
C --------------------------------------------
      DO 9 ISAT=1,NSFIL
        CALL INLIST(SVNFIL(ISAT),NSEFF,SVNEFF,KSAT)
        IF(KSAT.GT.0) THEN
          IF(MANOVR(KSAT).GT.0.AND.IFIL.GT.MANOVR(KSAT))THEN
            SVNFIL(ISAT)=SVNFIL(ISAT)+50
          ENDIF
        END IF
9     CONTINUE
C
C RENUMBER MANOEUVRE SATELLITES IN ARRAY SVNSTC
C ---------------------------------------------
CC RD: It still does not work in all cases...
CC      DO 10 ISAT=1,NUMSTC
CC        CALL INLIST(SVNSTC(ISAT),NSEFF,SVNEFF,KSAT)
CC        IF(KSAT.GT.0) THEN
CC          IF(MANOVR(KSAT).GT.0.AND.IFIL.GT.MANOVR(KSAT))THEN
CC            SVNSTC(ISAT)=SVNSTC(ISAT)+50
CC          ENDIF
CC        END IF
CC10    CONTINUE
C
C ADD NEW SATELLITES TO THE LIST SVNEFF, IN ADDITION
C PRODUCE A LIST OF SATELLITES ACTUALLY AVAILABLE IN FILE "IFIL"
C --------------------------------------------------------------
      DO 30 IPAR=1,NPAR
        IF(LOCQ(1,IPAR).EQ.3)THEN
          NUMTST=LOCQ(3,IPAR)
          CALL INLIST(NUMTST,NSEFF,SVNEFF,IELE)
          IF(IELE.EQ.0)THEN
            NSEFF=NSEFF+1
            SVNEFF(NSEFF)=NUMTST
            MANOVR(NSEFF)=0
C
C SET ALL PREVIOUS ARC NUMBERS TO ZERO
            DO 20 IF=1,IFIL-1
              ARCNUM(NSEFF,IF)=0
20          CONTINUE
          END IF
        END IF
30    CONTINUE
C
C DEFINE ARRAY ARCNUM
C -------------------
      DO 100 ISAT=1,NSEFF
C
C MISSING SATELLITES
        CALL INLIST(SVNEFF(ISAT),NSFIL,SVNFIL,IELE)
        IF(IELE.EQ.0)THEN
          ARCNUM(ISAT,IFIL)=0
        ELSE
C
C SATELLITE ACTUALLY IN FILE "IFIL"
C ---------------------------------
C
C (A) "IFIL"=1
          IF(IFIL.EQ.1)THEN
            ARCNUM(ISAT,IFIL)=1
          ELSE
C
C (B) "IFIL">1
C
C CHECK FOR SATCRUX SATELLITES AND UPDATE NEWARC ARRAY
            DO IMAN=1,NMAN
              IF (DABS(TOSC-TIMMAN(IMAN)).LT.RLIM.AND.
     1            MODSVN(SVNEFF(ISAT)).EQ.SATMAN(IMAN)) THEN
                DO INEW=1,NUMNAC
                  IF (SVNEFF(ISAT).EQ.NEWARC(1,INEW).AND.
     1                IFIL.EQ.NEWARC(2,INEW)) GOTO 36
                ENDDO
35              NUMNAC=NUMNAC+1
                NEWARC(1,NUMNAC)=SVNEFF(ISAT)
                NEWARC(2,NUMNAC)=IFIL
C
36              CONTINUE
              ENDIF
            ENDDO
C
C NUMBER OF CURRENTLY VALID ARC
            LSTARC=0
            DO 40 IF=1,IFIL-1
              IF(ARCNUM(ISAT,IF).NE.0)THEN
                LSTARC=ARCNUM(ISAT,IF)
              END IF
40          CONTINUE
C
C NEW ARC TO BE SET UP ?
            ARCNUM(ISAT,IFIL)=LSTARC
            DO 50 INEW=1,NUMNAC
              IF(SVNEFF(ISAT).EQ.NEWARC(1,INEW).AND.
     1           IFIL.EQ.NEWARC(2,INEW))THEN
                ARCNUM(ISAT,IFIL)=LSTARC+1
                GO TO 55
              END IF
50          CONTINUE
55          CONTINUE
C
C WAS THERE A GAP ?
            IF(ARCNUM(ISAT,IFIL-1).EQ.0)THEN
              ARCNUM(ISAT,IFIL)=LSTARC+1
              DO 60 IGAP=1,NUMGAP
                IF(SVNEFF(ISAT).EQ.SVNGAP(IGAP).AND.
     1             LSTARC.NE.0)THEN
                  ARCNUM(ISAT,IFIL)=LSTARC
                END IF
60            CONTINUE
            END IF
          END IF
        END IF
100   CONTINUE
C
C MANOEUVRES ?
C ----------
      DO 200 ISAT=1,NSFIL
        IF(MODSVN(SVNFIL(ISAT)).NE.SVNFIL(ISAT))THEN
          CALL INLIST(MODSVN(SVNFIL(ISAT)),NSFIL,SVNFIL,KSAT)
          IF (KSAT.EQ.0) THEN
            DO IMAN=1,NMA2
              IF (DABS(TOSC-TIMMA2(IMAN)).LT.RLIM.AND.
     1            MODSVN(SVNFIL(ISAT)).EQ.SATMA2(IMAN)) THEN
                KSAT=-1
                EXIT
              ENDIF
            ENDDO
            IF (KSAT.EQ.0) GOTO 200
          ENDIF
          IF (KSAT == -1) THEN
            CALL INLIST(MODSVN(SVNFIL(ISAT)),NSEFF,SVNEFF,KSAT)
            IF (KSAT == 0) THEN
              CALL INLIST(SVNFIL(ISAT),NSEFF,SVNEFF,KSAT)
            ENDIF
          ELSE
            CALL INLIST(MODSVN(SVNFIL(ISAT)),NSEFF,SVNEFF,KSAT)
          ENDIF
          IF(MANOVR(KSAT).EQ.0)THEN
            MANOVR(KSAT)=IFIL
          ELSE
            WRITE(LFNERR,110)IFIL
110         FORMAT(//,' ** SR ARCDEF : 2ND MANOEUVRE DETECTED IN FILE',
     1                ' NO',I4,//)
            CALL EXITRC(2)
          END IF
        END IF
200   CONTINUE
C
C SET CORRECT ARC NUMBER FOR CURRENT LOCQ :
C ONLY PARAMETER TYPES 3 (ORBIT) AND 11 (STOCHASTIC) AFFECTED)
C ------------------------------------------------------------
      DO 300 IPAR=1,NPAR
        IF(LOCQ(1,IPAR).EQ.3.OR.LOCQ(1,IPAR).EQ.11)THEN
          CALL INLIST(LOCQ(3,IPAR),NSEFF,SVNEFF,ISAT)
          LOCQ(2,IPAR)=ARCNUM(ISAT,IFIL)
        END IF
        IF(LOCQ(1,IPAR).EQ.11)THEN
          LOCQ(7,IPAR)=-1
        END IF
300   CONTINUE
C
C FILL IN ELEMENTS AND RPRPARAMETERS INTO ARRAYS ELE AND RPRESS
C -------------------------------------------------------------
      DO 400 ISAT=1,NSEFF
        CALL INLIST(SVNEFF(ISAT),NSFIL,SVNFIL,KSAT)
        IF(KSAT.EQ.0)THEN
          DO 310 K=1,7
            ELE(K,ISAT)=0.D0
310       CONTINUE
          DO 320 K=1,MXCDYN
            RPRESS(K,ISAT)=0.D0
320       CONTINUE
        ELSE
          DO 330 K=1,7
            ELE(K,ISAT)=ELEFIL(K,KSAT)
330       CONTINUE
          DO 340 K=1,MXCDYN
            RPRESS(K,ISAT)=RPRFIL(K,KSAT)
340       CONTINUE
        END IF
400   CONTINUE
999   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
