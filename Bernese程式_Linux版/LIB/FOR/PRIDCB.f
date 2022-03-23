      MODULE s_PRIDCB
      CONTAINS

C*
      SUBROUTINE PRIDCB(IPART ,LOCQ  ,XXX   ,ANOR  ,RMS   ,STNAME,
     1                  RECTYP,STFIL ,NDIFF ,NFTOT ,IPAR  )
CC
CC NAME       :  PRIDCB
CC
CC PURPOSE    :  PRINT DIFFERENTIAL CODE BIASES FOR SATELLITES AND
CC               RECEIVERS.
CC
CC PARAMETERS :
CC         IN :  IPART  : RESULTS PART 1(=1) OR 2 (=2)        I*4
CC               NPAR   : NUMBER OF PARAMETERS                I*4
CC               LOCQ(K,I),K=1,..,MAXLCQ,I=1,..,NPAR:         I*4(*,*)
CC                        CHARACTERISTICS FOR EACH PARAMETER
CC               XXX(I),I=1,..,NPAR: SOLUTION VECTOR          R*8(*)
CC               ANOR(I),I=1,..,NPAR*(NPAR+1)/2: INVERSE OF   R*8(*)
CC                        NORMAL EQUATION MATRIX
CC               RMS    : RMS ERROR OF UNIT WEIGHT            R*8
CC               STNAME(I),I=1,..,NSTAT: STATION NAMES        CH*16(*)
CC               RECTYP(K,I),K=1,2,I=...: RECEIVER TYPES      CH*20
CC               STFIL(K,I),K=1,2, I=1,2,..,NFTOT: STATIONS   I*4
CC                        IN FILES
CC               NDIFF(I):I=1,..,NFTOT: DIFFERENCE TYPE       I*4
CC                        NDIFF=0: ZERO DIFFERENCE
CC                        NDIFF=1: SINGLE DIFFERENCE
CC               NFTOT  : TOTAL NUMBER OF FILES               I*4
CC               IPAR   : PARAMETER INDEX                     I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  13-AUG-97
CC
CC CHANGES    :  22-OCT-97 : SS: CODE BIAS I/O FILES
CC               26-JAN-98 : SS: "MAXPAR" FROM 1000 TO 3000
CC               25-FEB-98 : SS: TEXT CORRECTED
CC               21-DEC-98 : SS: GPS/GLONASS DCBS FOR RECEIVERS
CC               07-APR-00 : SS: SET "ICBTYP"
CC               13-APR-00 : SS: ESTIMATE (P1-C1) CODE BIASES
CC               30-MAY-00 : SS: "MAXREC" FROM 100 TO 200
CC               05-JUN-01 : HU: "MAXPAR" FROM 3000 TO 3500
CC               06-SEP-01 : SS: "MAXPAR" FROM 3500 TO 4000
CC               07-MAY-02 : SS: DCB UPDATE
CC               06-AUG-02 : SS: X2 INSTEAD OF C2
CC               08-SEP-03 : HU: RECNAM CHR16 -> CHR20
CC               09-APR-04 : HU: IWLFAC DECLARED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               21-JUN-07 : SS: CALL SR GETRCV WITH ISYST
CC               27-JUN-07 : AG: "E" STRING IMPLEMENTED
CC               09-MAY-09 : RD: NEW CALL OF DCBCOR
CC               28-MAY-09 : RD: USE SVN2CHR TO PRINT THE SATELLITES
CC               28-MAR-12 : RD: USE SVN2CHR AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern, ONLY: LFNPRT
      USE f_ikf
      USE s_getrcv
      USE s_dcbcor
      USE s_clsrcv
      USE f_lengt1
      USE s_gtflna
      USE s_svn2chr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICLS  , IFIL  , IGPOLD, IGRP  , IPAR  , IPART ,
     1          IPRN  , IRC   , IRCDCB, ISTA  , ITYP  , MXCLCQ,
     2          NFREQ , NFTOT
C
      REAL*8    DIST  , RMS   , RMS1  , VALNEW, VALOLD, XXX1
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*132 TEXT
      CHARACTER*32  DCBFIL
      CHARACTER*16  STNAME(*)
      CHARACTER*20  RECTYP(2,*),RECSTR
      CHARACTER*6   MXNLCQ
      CHARACTER*5   CLSST0,CLSSTR
      CHARACTER*2   MATSTR
      CHARACTER*1   FLAG
C
      REAL*8        XXX(*),ANOR(*),CLSFAC(3)
C
      INTEGER*4     LOCQ(MXCLCQ,*)
      INTEGER*4     STFIL(2,*),NDIFF(*),ICODE(2),IWLFAC(2),ISYST
      INTEGER*4     IFIRST(2)
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
C
      DATA IFIRST/1,1/
C
      IF (IFIRST(IPART).EQ.1) THEN
        IGPOLD=0
C
        CALL GTFLNA(0,'DCBINP ',DCBFIL,IRCDCB)
C
C PRINT TITLE
C -----------
        TEXT=' DIFFERENTIAL CODE BIASES:'
        CALL GTFLNA(0,'DCBOUT ',TEXT(45:76),IRC)
        IF(TEXT(45:48).EQ.'    ') TEXT(45:55)='(NOT SAVED)'
C
        WRITE(LFNPRT,100) TEXT(1:LENGT1(TEXT))
100     FORMAT(A,/,1X,24('-'))
C
        IFIRST(IPART)=0
      ENDIF
C
      IGRP=LOCQ(2,IPAR)
C
C PRINT HEADER LINES
C ------------------
      IF (IGRP.NE.IGPOLD) THEN
        IF (IGRP.EQ.1) THEN
          WRITE(LFNPRT,110)
110       FORMAT(/,' PRN                    ',
     1      'OLD VALUE    IMPROVEMENT  NEW VALUE    RMS ERROR (NS)',
     2      /,1X,131('-'),/)
        ELSE
          IF (LOCQ(6,IPAR).GT.0) THEN
            WRITE(LFNPRT,120)
120         FORMAT(/,' STATION NAME           ',
     1        'OLD VALUE    IMPROVEMENT  NEW VALUE    RMS ERROR (NS)',
     2        /,1X,131('-'),/)
          ELSE
            WRITE(LFNPRT,121)
121         FORMAT(/,' STATION NAME           ',
     1        'MULTIPLIER   RMS ERROR    SUGGESTED RECEIVER TYPE    ',
     2        'GIVEN RECEIVER NAME / TYPE     MATCH',
     3        /,1X,131('-'),/)
          ENDIF
        ENDIF
C
        IGPOLD=IGRP
      ENDIF
C
C PRINT BIASES FOR SATELLITES AND RECEIVERS
C -----------------------------------------
      IF (IGRP.EQ.1) THEN
        IF (IRCDCB.EQ.0) THEN
          ITYP=LOCQ(5,IPAR)
          CALL DCBCOR(2,0,LOCQ(3,IPAR),' ',' ',0,ITYP,
     1                0.D0,DIST)
          VALOLD=DIST
        ELSE
          VALOLD=0.D0
        ENDIF
        XXX1=XXX(IPAR)
        VALNEW=VALOLD+XXX1
        RMS1=RMS*DSQRT(ANOR(IKF(IPAR,IPAR)))
C
        CALL SVN2CHR(LOCQ(3,IPAR),IPRN,FLAG)
        WRITE(LFNPRT,210) FLAG,IPRN,VALOLD,XXX1,VALNEW,RMS1
210     FORMAT(1X,A1,I2.2,16X,4F13.3)
      ELSE
        IF (IRCDCB.EQ.0) THEN
          ITYP=MAX0(LOCQ(6,IPAR),0)
          CALL DCBCOR(2,0,LOCQ(5,IPAR),STNAME(LOCQ(3,IPAR)),' ',0,ITYP,
     1                0.D0,DIST)
          VALOLD=DIST
        ELSE
          VALOLD=0.D0
        ENDIF
        XXX1=XXX(IPAR)
        VALNEW=VALOLD+XXX1
        RMS1=RMS*DSQRT(ANOR(IKF(IPAR,IPAR)))
        IF (LOCQ(5,IPAR).EQ.1) THEN
          FLAG='G'
        ELSEIF (LOCQ(5,IPAR).EQ.2) THEN
          FLAG='R'
        ELSEIF (LOCQ(5,IPAR).EQ.3) THEN
          FLAG='E'
        ENDIF
C       ADD_GNSS_HERE
        IF (LOCQ(6,IPAR).GT.0) THEN
          WRITE(LFNPRT,220) STNAME(LOCQ(3,IPAR)),FLAG,
     1      VALOLD,XXX1,VALNEW,RMS1
220       FORMAT(1X,A16,'-',A1,1X,4F13.3)
        ELSE
          RECSTR=' '
          DO IFIL=1,NFTOT
            DO ISTA=1,1+NDIFF(IFIL)
              IF (STFIL(ISTA,IFIL).EQ.LOCQ(3,IPAR))
     1          RECSTR=RECTYP(ISTA,IFIL)
            ENDDO
          ENDDO
          CALL GETRCV(RECSTR,NFREQ,ICODE,IWLFAC,ICLS,ISYST)
          IF (ICLS.EQ.1) THEN
            CLSST0='P1/P2'
          ELSEIF (ICLS.EQ.2) THEN
            CLSST0='C1/X2'
          ELSEIF (ICLS.EQ.3) THEN
            CLSST0='C1/P2'
          ENDIF
C
C CLASSIFY RECEIVER BASED ON DCB MULTIPLIER
C -----------------------------------------
          CALL CLSRCV(XXX1,RMS1,ICLS,CLSSTR,CLSFAC)
C
          IF (CLSSTR.EQ.CLSST0) THEN
            MATSTR='OK'
          ELSE
            MATSTR=' '
          ENDIF
          WRITE(LFNPRT,221) STNAME(LOCQ(3,IPAR)),FLAG,
     1      XXX1,RMS1,CLSSTR,CLSFAC(1),CLSFAC(2),RECSTR,CLSST0,MATSTR
221       FORMAT(1X,A16,'-',A1,1X,2F13.3,4X,A5,2F8.3,6X,A20,2X,A5,4X,
     1      A2)
        ENDIF
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
