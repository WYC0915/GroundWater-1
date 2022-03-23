      MODULE s_CDPELE
      CONTAINS
C*
      SUBROUTINE CDPELE(PGMNAM,CAMPGN,STITLE,OBSFIL,FILFRQ,
     1                  ISEL,NSATEL,SATNUM,INDSVN,OBSEPO,TIMREF,
     2                  IDELTT,NSACT,SATNRA,FLGACT,ELE)
CC
CC NAME       :  CDPELE
CC
CC PURPOSE    :  PRINT ELEVATIONS PGM CODSPP
CC
CC PARAMETERS :
CC         IN :  PGMNAM : PROGRAM NAME                        CHR*6
CC               CAMPGN : CAMPAIGN NAME                       CHR*16
CC               STITLE : SHORT TITLE                         CHR*64
CC               OBSFIL : OBS FILE NAME (DD NAME)             CHR*32
CC               FILFRQ : FREQUENCY                             I*4
CC               ISEL   : =1: PRINT HEADER                      I*4
CC                        =2: PRINT ELEALS
CC               NSATEL : NUMBER OF SATELLITES IN FILE          I*4
CC               SATNUM(I): ARRAY OF SVN NUMBERS IN FILE        I*4(*)
CC                        I=1,2,...,NSATEL
CC               INDSVN(I): INDEX SVN --> SATNUM(I)             I*4(*)
CC                        I=1,2,...,MAXSVN
CC               OBSEPO : EPOCH OF OBSERVATION (MJD)            R*8
CC               TIMREF : REFERENC EPOCH                        R*8
CC               IDELTT : OBS INTERVAL (SEC)                    I*4
CC               NSACT  : ACTUAL NUMBER OF SATELLITES           I*4
CC               SATNRA(I): ACTUAL SATELLITE NUMBERS            I*4(*)
CC                        I=1,2,...,NSACT
CC               FLGACT(I): ACTUAL OBSERVATION FLAG           CHR*1(*)
CC                        I=1,2,...,NSACT
CC                        ='G': GOOD OBS
CC                        ='M': MARKED OBS (FROM FILE)
CC                        ='E': MARKED OBS FROM ELEVATION LIMIT
CC               ELE(I)  : ELEVATIONS DEG                       R*8(*)
CC                          I=1,2,...,NSACT
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  T. SCHILDKNECHT
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/12/01 14:39
CC
CC CHANGES    :  29-OCT-93 : ??: SATELLITES WITHOUT ORBIT INFO.
CC                               CHANGE FORMAT FOR 26 SATELLITES
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               30-OCT-97 : DI: CHANGE OUTPUT FORMAT FOR 48 SATELLITES
CC               19-NOV-97 : SS: INCLUDE "PGMVER"
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               18-FEB-03 : HU: USE PGMVER FROM M_BERN
CC               10-MAR-04 : HB: REMOVE ONLY-STATEMENT FOR M_BERN
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               27-SEP-11 : SL: USE M_BERN WITH ONLY, DISPLAY 60 SAT
CC               15-OCT-12 : RD/SL: BERNESE GPS->GNSS SOFTWARE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
C
C DECLARATIONS
C
      USE m_bern,   ONLY: lineLength, lfnErr, lfnPrt, pgmVer
      USE m_maxdim, ONLY: MAXSAT
      USE d_const,  ONLY: DATE, TIME
      USE s_maxtst
      USE s_exitrc
      USE f_lengt1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IDELTT, IRC   , ISAT  , ISCHAR, ISEL  ,
     1          MXCSAT, NREC  , NSACT , NSATEL
C
      REAL*8    OBSEPO, TIMREF
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8 ELE(*),ELELOC(MAXSAT)
      INTEGER*4 SATNUM(*),INDSVN(*),SATNRA(*)
      INTEGER*4 FILFRQ
      CHARACTER(LEN=lineLength) ::  WSTRNG
      CHARACTER*64 STITLE
      CHARACTER*32 OBSFIL
      CHARACTER*16 CAMPGN
      CHARACTER*6  MXNSAT,PGMNAM
      CHARACTER*1  FLGACT(*),FLGLOC(MAXSAT)
C
CC      INCLUDE 'COMLFNUM.inc'
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
C  WHICH MODE?
      GOTO(100,200) ISEL
      WRITE(LFNERR,3)ISEL
3     FORMAT(/' *** SR CDPELE: ILLEGAL SELECTION CODE'/
     1                    16X,'SELECTION CODE:',I3,/)
      CALL EXITRC(2)
C
C  IF ISEL=1, PRINT HEADER
C  -----------------------
100   CALL MAXTST(1,'CDPELE',MXNSAT,MAXSAT,MXCSAT,IRC)
      IF(IRC.NE.0) CALL EXITRC(2)
      WRITE(LFNPRT,101)CAMPGN,PGMNAM,DATE,TIME,STITLE,PGMVER
101   FORMAT('1',/,1X,1A16,82X,'PROGRAM ',1A6,4X,1A9,1X,1A5/
     1           ' ',1A64,34X,
     2           'BERNESE GNSS SOFTWARE VERSION ',1A3/' ',131('-'))
      WRITE(LFNPRT,103)OBSFIL,FILFRQ
103   FORMAT(///' ELEVATIONS IN DEGREES OF FILE',2X,1A32/
     1          ' ------------------------------',29('-')//
     2          ' FREQUENCY :  L',I1)
      WRITE(LFNPRT,109)(SATNUM(I),I=1,NSATEL)
109   FORMAT(/' EPO/SVN ',60(I4))
      WRITE(LFNPRT,110)('----',I=1,NSATEL)
110   FORMAT(1X,'--------',60(A4))
      RETURN
C
C  IF ISEL=2, PRINT ELEVATIONS FOR ONE EPOCH
C  -----------------------------------------
200   NREC=IDNINT((OBSEPO-TIMREF)/IDELTT*86400.D0)+1
      DO I=1,NSATEL
        FLGLOC(I)=' '
        ELELOC(I)=0.D0
      ENDDO
      DO ISAT=1,NSACT
        ELELOC(INDSVN(SATNRA(ISAT)))=ELE(ISAT)
        FLGLOC(INDSVN(SATNRA(ISAT)))=FLGACT(ISAT)
      ENDDO
      WRITE(WSTRNG,201)NREC,(IDNINT(ELELOC(I)),FLGLOC(I),I=1,NSATEL)
201   FORMAT(I8,1X,60(I3,1A1))
      DO 230 I=1,NSATEL
        ISCHAR=10+(I-1)*4
        IF(FLGLOC(I).EQ.' ') THEN
          WSTRNG(ISCHAR:ISCHAR+3)='    '
        ELSEIF (FLGLOC(I).EQ.'X') THEN
          WSTRNG(ISCHAR:ISCHAR+3)=' N.O'
        ELSE
          IF(DABS(ELELOC(I)).GT.1.D3-1)
     1      WSTRNG(ISCHAR:ISCHAR+2)=' ++'
        ENDIF
230   CONTINUE
      WRITE(LFNPRT,231) WSTRNG(1:LENGT1(WSTRNG))
231   FORMAT(A)
      RETURN
C
      END SUBROUTINE

      END MODULE
