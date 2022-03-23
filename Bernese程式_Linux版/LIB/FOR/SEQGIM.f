      MODULE s_SEQGIM
      CONTAINS

C*
      SUBROUTINE SEQGIM(OPTGIM,POLGIM,NAMGIM,EPOGIM)
CC
CC NAME       :  SEQGIM
CC
CC PURPOSE    :  GET INFORMATION FROM IONOSPHERE FILE IN CASE OF
CC               IONOSPHERE MODEL IMPROVEMENT
CC
CC PARAMETERS :
CC     IN/OUT :  OPTGIM : OPTIONS FOR GLOBAL IONOSPHERE MODEL I*4(*)
CC                        (1): MAXIMUM DEGREE
CC                        (2): MAXIMUM ORDER
CC                        (3): FLAG FOR REFERENCE FRAME
CC                             =1: GEOGRAPHICAL
CC                             =2: GEOMAGNETIC
CC                        (4): FLAG FOR POSITION OF THE SUN
CC                             =1: MEAN
CC                             =2: TRUE
CC                        (5): ESTIMATION OF LAYER HEIGHT
CC                             =0: NO
CC                             =1: ONE PARAMETER IN ALL
CC                             =2: ONE PARAMETER PER MODEL
CC                        (6): MODE OF TEMPORAL MODELING
CC                             =1: STATIC MODEL
CC                             =2: DYNAMIC MODEL
CC                        (7): TOTAL NUMBER OF MODELS
CC                        (8): MAPPING FUNCTION
CC                             =1: 1/COS
CC                        (9): STATION-SPECIFIC MODELS
CC                        (10): COMPONENT TO BE ESTIMATED
CC                              =1: DETERMINISTIC
CC                              =2: STOCHASTIC
CC               POLGIM(I,J),I=1,2,3,J=1,..,OPTGIM(7):        R*8(3,*)
CC                        I=1: HEIGHT OF SINGLE LAYER (M)
CC                        I=2: LAT. OF NORTH GEOMAGNETIC POLE
CC                        I=3: EAST LONGITUDE
CC               NAMGIM(I),I=1,..,OPTGIM(7): MODEL NUMBERS    CH*16(*)
CC               EPOGIM(I,J),I=1,2,J=1,..,OPTGIM(7): PERIODS  R*8(2,*)
CC                        OF VALIDITY / REF EPOCHS (MJD)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.0
CC
CC CREATED    :  04-OCT-95
CC
CC CHANGES    :  20-DEC-95 : SS: DECLARE "NUMGIM" AS CH*7
CC               20-DEC-95 : SS: "IONTXT" IN CALL OF SR GETGIM
CC               13-NOV-97 : SS: "IONFIL" IN CALL OF SR GETGIM
CC               17-NOV-97 : SS: "IONTIT" IN CALL OF SR GETGIM
CC               20-JAN-98 : SS: "IONINF" ADDED
CC               26-JAN-98 : SS: STATION-SPECIFIC GIMS
CC               29-APR-98 : SS: DTEC LC
CC               26-MAY-98 : SS: "IONDEV" REDIMENSIONED
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               26-MAR-12 : RD: USE GETGIM AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1995     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: LFNERR
      USE m_maxdim, ONLY: MAXGIM, MAXGIT
      USE s_getgim
      USE s_exitrc
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IALL  , IMOD  , IONTYP, IRC   , NMODEL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      CHARACTER*80 IONTIT
      CHARACTER*32 IONFIL
      CHARACTER*16 NAMGIM(*),IONTXT(MAXGIM)
C
      REAL*8 POLGIM(3,*),EPOGIM(2,*),IONDEV(10,MAXGIM)
      REAL*8 XDUMMY(MAXGIT,1)
C
C
      INTEGER*4 OPTGIM(*),IONREQ(6,MAXGIM)
      INTEGER*4 NTERM(MAXGIM),IONINF(4)
      INTEGER*4 IDUMMY(MAXGIT,2,1)
C
      COMMON/CSEQGI/IONREQ,IONDEV,NTERM,IONTXT
C
C
C RETURN, IF NO IONOSPHERE MODEL IMPROVEMENT
C ------------------------------------------
      IF (OPTGIM(5).EQ.0 .OR. OPTGIM(10).EQ.2) RETURN
C
C GET INFORMATION FROM IONOSPHERE FILE
C ------------------------------------
      CALL GTFLNA(1,'IONOS  ',IONFIL,IRC)
C
      IALL=0
      CALL GETGIM(IONFIL,IALL  ,NMODEL,IONREQ,IONDEV,NTERM ,
     1            IDUMMY,XDUMMY,XDUMMY,IONTXT,IONTIT,IONINF,
     2            IONTYP)
C
C COPY ESSENTIAL INFORMATION
C --------------------------
      OPTGIM(1)=IONREQ(2,1)
      OPTGIM(2)=IONREQ(3,1)
      OPTGIM(3)=IONREQ(4,1)
      OPTGIM(4)=IONREQ(5,1)
      OPTGIM(8)=IONREQ(6,1)
C
      IF (IONTYP.EQ.3) THEN
        OPTGIM(9)=1
      ELSE
        OPTGIM(9)=0
      ENDIF
C
      IF (IONDEV(6,1).NE.0.D0) THEN
        OPTGIM(6)=1
      ELSE
        OPTGIM(6)=2
      END IF
C
      OPTGIM(7)=NMODEL
C
      DO 110 IMOD=1,NMODEL
        POLGIM(1,IMOD)=IONDEV(1,IMOD)
        POLGIM(2,IMOD)=IONDEV(3,IMOD)
        POLGIM(3,IMOD)=IONDEV(4,IMOD)
C
        EPOGIM(1,IMOD)=IONDEV(5,IMOD)
        EPOGIM(2,IMOD)=IONDEV(6,IMOD)
C
        NAMGIM(IMOD)=IONTXT(IMOD)
C
C CHECK MODEL SPECIFICATIONS
        IF (IONREQ(2,IMOD).NE.OPTGIM(1).OR.
     1      IONREQ(3,IMOD).NE.OPTGIM(2).OR.
     2      IONREQ(4,IMOD).NE.OPTGIM(3).OR.
     3      IONREQ(5,IMOD).NE.OPTGIM(4).OR.
     4      IONREQ(6,IMOD).NE.OPTGIM(8)) THEN
          WRITE(LFNERR,910)
910       FORMAT(/,' *** SR SEQGIM: GLOBAL IONOSPHERE MODEL ',
     1           'PARAMETERS.',
     2           /,16X,'MIXTURE OF MODEL SPECIFICATIONS ',
     3           'NOT ALLOWED.',/)
          CALL EXITRC(2)
        END IF
C
C CHECK A PRIORI VALUES OF SINGLE-LAYER HEIGHT
        IF (OPTGIM(5).EQ.1.AND.
     1      POLGIM(1,IMOD).NE.POLGIM(1,1)) THEN
          WRITE(LFNERR,920)
920       FORMAT(/,' *** SR SEQGIM: GLOBAL IONOSPHERE MODEL ',
     1           'PARAMETERS.',
     2           /,16X,'A PRIORI VALUE OF SINGLE-LAYER HEIGHT ',
     3           'MUST BE',
     4           /,16X,'THE SAME FOR ALL MODELS.',/)
          CALL EXITRC(2)
        END IF
110   CONTINUE
C
C DO NOT ACCEPT DTEC INFORMATION
      IF (IONINF(4).GT.0) THEN
        WRITE(LFNERR,930)
930     FORMAT(/,' *** SR SEQGIM: DTEC INFORMATION FOUND',/)
        CALL EXITRC(2)
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
