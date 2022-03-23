      MODULE s_CDPRES
      CONTAINS

C*
      SUBROUTINE CDPRES(PGMNAM,CAMPGN,STITLE,OBSFIL,FILFRQ,
     1                  ISEL,ICLPOL,NSATEL,SATNUM,INDSVN,OBSEPO,TIMREF,
     2                  IDELTT,OFFS,NSACT,SATNRA,FLGACT,RESIDU)
CC
CC NAME       :  CDPRES
CC
CC PURPOSE    :  PRINT RESIDUALS OF PGM CODSPP
CC
CC PARAMETERS :
CC         IN :  PGMNAM : PROGRAM NAME                        CHR*6
CC               CAMPGN : CAMPAIGN NAME                       CHR*16
CC               STITLE : SHORT TITLE                         CHR*64
CC               OBSFIL : OBS FILE NAME (DD NAME)             CHR*32
CC               FILFRQ : FREQUENCY                             I*4
CC               ISEL   : =1: PRINT HEADER                      I*4
CC                        =2: PRINT RESIDUALS
CC               ICLPOL : TYPE OF CLOCK MODELLING               I*4
CC                        ICLPOL>0: POL. OF DEGREE ICLPOL
CC                        ICLPOL=-1: ONE OFFSET PER EPOCH
CC               NSATEL : NUMBER OF SATELLITES IN FILE          I*4
CC               SATNUM(I): ARRAY OF SVN NUMBERS IN FILE        I*4(*)
CC                        I=1,2,...,NSATEL
CC               INDSVN(I): INDEX SVN --> SATNUM(I)             I*4(*)
CC                        I=1,2,...,MAXSVN
CC               OBSEPO : EPOCH OF OBSERVATION (MJD)            R*8
CC               TIMREF : REFERENC EPOCH                        R*8
CC               IDELTT : OBS INTERVAL (SEC)                    I*4
CC               OFFS   : RECEIVER CLOCK OFFSET FOR CURRENT     R*8
CC                        EPOCH
CC               NSACT  : ACTUAL NUMBER OF SATELLITES           I*4
CC               SATNRA(I): ACTUAL SATELLITE NUMBERS            I*4(*)
CC                        I=1,2,...,NSACT
CC               FLGACT(I): ACTUAL OBSERVATION FLAG           CHR*1(*)
CC                        I=1,2,...,NSACT
CC                        ='G': GOOD OBS
CC                        ='M': MARKED OBS (FROM FILE)
CC                        ='E': MARKED OBS FROM ELEVATION LIMIT
CC               RESIDU(I): RESIDALS FOR  (M)                   R*8(*)
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
CC CHANGES    :  11-JAN-93 : ??: CHANGE FORMATS. REMOVE "1H1".
CC               17-FEB-93 : ??: ISEL=3: PRINT SATELLITE NUMBERS ONLY
CC                               SOME BLANK LINES IN HEADER OUTPUT REMOVED
CC               18-APR-93 : ??: SMALLER FORMAT TO PRINT 26 OR 27 SATEL.
CC               29-OCT-93 : ??: ALLOW SATELLITES WITHOUT ORBIT-INFO
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               30-OCT-95 : MR: PRINT UNITS OF OFFSET (S)
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               30-OCT-97 : DI: CHANGE FORMAT FOR MAXSAT=48
CC               19-NOV-97 : SS: INCLUDE "PGMVER"
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               18-FEB-03 : HU: USE PGMVER FROM M_BERN
CC               10-MAR-04 : HB: REMOVE ONLY-STATEMENT FOR M_BERN
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               15-OCT-12 : RD/SL: BERNESE GPS->GNSS SOFTWARE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
C
C DECLARATIONS
C ------------
      USE m_bern
      USE m_maxdim, ONLY: MAXSAT
      USE d_const, ONLY: DATE, TIME
      USE s_maxtst
      USE s_exitrc
      USE f_lengt1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ICLPOL, IDELTT, IOS   , IRC   , ISAT  , ISCHAR,
     1          ISEL  , MXCSAT, NREC  , NSACT , NSATEL
C
      REAL*8    OBSEPO, TIMREF
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
CC      INCLUDE 'COMLFNUM.inc'
C
      REAL*8        RESIDU(*),RESLOC(MAXSAT),OFFS(*)
C
      INTEGER*4     SATNUM(*),INDSVN(*),SATNRA(*)
      INTEGER*4     FILFRQ
C
      CHARACTER*452 WSTRNG
      CHARACTER*64  STITLE
      CHARACTER*32  OBSFIL
      CHARACTER*16  CAMPGN
      CHARACTER*6   MXNSAT,PGMNAM
      CHARACTER*1   FLGACT(*),FLGLOC(MAXSAT)
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
C  WITCH MODE ?
C  ------------
      GOTO(100,200,300) ISEL
      WRITE(LFNERR,3)ISEL
3     FORMAT(/' *** SR CDPRES: ILLEGAL SELECTION CODE'/
     1                    16X,'SELECTION CODE:',I3,/)
      CALL EXITRC(2)
C
C  IF ISEL=1, PRINT HEADER
C  ------------------------------------------------------------------
100   CALL MAXTST(1,'CDPRES',MXNSAT,MAXSAT,MXCSAT,IRC)
      IF(IRC.NE.0) CALL EXITRC(2)
      WRITE(LFNPRT,101)CAMPGN,PGMNAM,DATE,TIME,STITLE,PGMVER
101   FORMAT('1',/,1X,A16,82X,'PROGRAM ',A6,4X,A9,1X,A5,/
     1           ' ',A64,34X,
     2           'BERNESE GNSS SOFTWARE VERSION ',A3/' ',131('-'))
      WRITE(LFNPRT,103)OBSFIL,FILFRQ
103   FORMAT(/' RESIDUALS IN METERS OF FILE',2X,A32/
     1        ' ---------------------------',34('-')//
     2        ' FREQUENCY :  L',I1,/)
C
300   IF (ICLPOL .GT. 0) THEN
        WRITE(LFNPRT,109)(' SV-',SATNUM(I),I=1,NSATEL)
109     FORMAT(' EPONR ',48(A4,I3,2X))
      ELSE IF (ICLPOL .EQ. -1) THEN
        WRITE(LFNPRT,110)('  SV-',SATNUM(I),I=1,NSATEL)
110     FORMAT(' EPONR    OFFS (S) ',48(A4,I3,2X))
      ENDIF
      RETURN
C
C  IF ISEL=2, PRINT RESIDUALS FOR ONE EPOCH
C  ------------------------------------------------------------------
200   NREC=IDNINT((OBSEPO-TIMREF)/IDELTT*86400.D0)+1
      DO I=1,NSATEL
        FLGLOC(I)='0'
        RESLOC(I)=0.D0
      ENDDO
      DO ISAT=1,NSACT
        RESLOC(INDSVN(SATNRA(ISAT)))=RESIDU(ISAT)
        FLGLOC(INDSVN(SATNRA(ISAT)))=FLGACT(ISAT)
        IF (FLGACT(ISAT).EQ.'G') FLGLOC(INDSVN(SATNRA(ISAT)))=' '
      ENDDO
C
      IF (ICLPOL .GT. 0) THEN
        WRITE(WSTRNG,201,IOSTAT=IOS)
     1        NREC,(RESLOC(I),FLGLOC(I),I=1,NSATEL)
201     FORMAT(I7,1X,48(F8.2,A1))
      ELSE IF (ICLPOL .EQ. -1) THEN
        WRITE(WSTRNG,202,IOSTAT=IOS)
     1        NREC,OFFS(NREC),(RESLOC(I),FLGLOC(I),I=1,NSATEL)
202     FORMAT(I7,2X,F10.7,1X,48(F8.2,A1))
      ENDIF
C
      DO I=1,NSATEL
        IF (ICLPOL .GT. 0) THEN
          ISCHAR=9+(I-1)*9
        ELSE IF (ICLPOL .EQ. -1) THEN
          ISCHAR=21+(I-1)*9
        ENDIF
        IF(FLGLOC(I).EQ.'0') THEN
          WSTRNG(ISCHAR:ISCHAR+8)=' '
        ELSEIF (FLGLOC(I).EQ.'X') THEN
          WSTRNG(ISCHAR:ISCHAR+8)=' NO ORBIT'
        ELSE
          IF(DABS(RESLOC(I)).GT.1.D3-1)
     1      WRITE(WSTRNG(ISCHAR:ISCHAR+7),209)RESLOC(I)
209         FORMAT(D8.1)
        ENDIF
      ENDDO
C
      WRITE(LFNPRT,231)WSTRNG(1:LENGT1(WSTRNG))
231   FORMAT(A)
C
      RETURN
      END SUBROUTINE
C

      END MODULE
