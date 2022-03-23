      MODULE s_INXSAV
      CONTAINS

C*
      SUBROUTINE INXSAV(NMODEL,IONREQ,IONDEV,NTERM ,NM    ,IONCOE,
     1                  TITLE ,NSTEFF,NSAEFF,ZENMAX,RMS   ,QMAT  ,
     2                  IONIND,SCAGIM,IORSYS)
CC
CC NAME       :  INXSAV
CC
CC PURPOSE    :  SAVE IONOSPHERE MAPS IN IONEX VERSION 1.0 FORMAT
CC
CC PARAMETERS :
CC         IN :  NMODEL : NUMBER OF MODELS                    I*4
CC               IONREQ(K,I),K=1,..,6,I=1,..,NMODEL:          I*4(6,*)
CC                        (1,I): ---
CC                        (2,I): MAXIMUM DEGREE
CC                        (3,I): MAXIMUM ORDER
CC                        (4,I): FLAG FOR REFERENCE FRAME
CC                               =1: GEOGRAPHICAL
CC                               =2: GEOMAGNETIC
CC                        (5,I): FLAG FOR POSITION OF THE SUN
CC                               =1: MEAN
CC                               =2: TRUE
CC                        (6,I): MAPPING FUNCTION
CC                               =0: NONE
CC                               =1: COSZ
CC                               =2: MSLM
CC                               =3: ESM
CC               IONDEV(K,I),K=1,..,8,I=1,..,NMODEL:          R*8(8,*)
CC                        (1,I): HEIGHT OF SINGLE LAYER (M)
CC                        (2,I): ITS RMS ERROR (M)
CC                        (3,I): LATITUDE OF GEOMAGNETIC POLE
CC                        (4,I): EAST LONGITUDE
CC                        (5,I): FROM EPOCH / REF EPOCH (MJD)
CC                        (6,I): TO EPOCH / 0
CC                        (7,I): MINIMUM LATITUDE
CC                        (8,I): MAXIMUM LATITUDE
CC               NTERM(I),I=1,..,NMODEL: NUMBER OF TERMS      I*4(*)
CC                        GIVEN FOR MODEL I
CC               NM(K,J,I),K=1,..,NTERM,J=1,2,I=1,..,NMODEL:  I*4(*,2,*)
CC                        (K,1,I): DEGREE OF TERM K
CC                        (K,2,I): ORDER
CC               IONCOE(K,I),K=1,..,NTERM,I=1,..,NMODEL:      R*8(*,*)
CC                        VALUE OF COEFFICIENT K
CC               TITLE  : TITLE                               CH*80
CC               NSTEFF : NUMBER OF CONTRIBUTING STATIONS     I*4
CC               NSAEFF : NUMBER OF SATELLITES                I*4
CC               ZENMAX : MAXIMUM ZENITH DISTANCE IN RAD      R*8
CC               RMS    : ESTIMATED MEAN ERROR OF UNIT WEIGHT R*8
CC               QMAT(I),I=1,..: LINEARIZED NORMAL EQUATION   R*8(*)
CC                        MATRIX
CC               IONIND(K,I),K=1,..,NTERM,I=1,..,NMODEL:      I*4(*,*)
CC                        PARAMETER INDICES
CC               SCAGIM : SCALING FACTOR FOR                  R*8(*)
CC                        (1): ION. COEFFICIENTS
CC                        (2): SINGLE-LAYER HEIGHT
CC                        (3): DTEC PARAMETERS
CC               IORSYS : ORBIT SYSTEM                        I*4
CC                        =1: B1950.0
CC                        =2: J2000.0
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.0
CC
CC CREATED    :  22-FEB-96
CC
CC CHANGES    :  23-SEP-97 : SS: IONEX VERSION 1.0
CC               30-SEP-97 : SS: "TECMAP" AS R*8 ARRAY
CC               22-OCT-97 : SS: CODE BIAS I/O FILES
CC               23-OCT-97 : SS: USE "INXDEF(12)" FOR DCB INFO
CC               28-OCT-97 : SS: SET "IEXP=99" IF "INXDEF(12)<0"
CC               16-NOV-97 : SS: CALL SR RDCBFL WITH "MAXREC=0"
CC               17-NOV-97 : SS: "SATSTR" RETURNED BY SR RDIXCF
CC               18-NOV-97 : SS: INCLUDE "PGMVER"
CC               18-NOV-97 : SS: "IEXP" RETURNED BY SR RDIXCF
CC               26-JAN-98 : SS: INCLUDE FILE "MAXGIT"
CC               26-FEB-98 : SS: IONEX DCB FORMAT ADJUSTED
CC               09-MAR-98 : SS: MAXVAL FROM 52925 TO 69277
CC               14-APR-98 : SS: FASTER COMPUTATION OF RMS MAPS
CC               29-APR-98 : SS: DTEC LC
CC               26-MAY-98 : SS: PRINT MINIMUM TEC
CC               26-MAY-98 : SS: "IONDEV" REDIMENSIONED
CC               11-JUN-98 : SS: ALLOW DCB COMMENT
CC               25-JUN-98 : SS: USE SR DCBADT
CC               08-JUL-98 : SS: CONSIDER "IALL"
CC               10-JAN-99 : SS: GPS/GLONASS DCBS FOR RECEIVERS
CC               04-APR-00 : SS: CREATE LIST OF STATIONS
CC               07-APR-00 : SS: SET "ICBTYP"
CC               18-DEC-00 : SS: INITIALIZE "NUMLIN"
CC               04-SEP-01 : SS: SR ASLEFU REPLACED BY SR ASLEF2
CC               16-SEP-01 : SS: "MAXLIN" FROM 12 TO 30
CC               17-SEP-01 : SS: BUG FIXED CONCERNING "NUMLIN"
CC               18-SEP-01 : SS: NEW TEC MAPPING FUNCTIONS
CC               13-MAR-02 : SS: RECEIVER DCB INFORMATION
CC               03-APR-02 : SS: REFINED COMPUTATION OF RMS MAPS
CC               21-AUG-02 : SS: ALLOW MORE THAN ONE DCB SET IN FILE
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               18-FEB-03 : HU: USE PGMVER FROM M_BERN
CC               10-MAR-04 : HB: REMOVE ONLY-STATEMENT FOR M_BERN
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               09-MAY-09 : RD: WRITE FREQ-DEP. CODE BIAS INTO DCB-FILE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_maxdim, ONLY: maxsat, maxrec, maxgit
      USE d_const, ONLY: CONRE, PI
      USE f_ikf
      USE s_dimtst
      USE s_wtixfl
      USE s_rdixcf
      USE s_rdcbfl
      USE s_dcbadt
      USE s_eflsfl
      USE s_exitrc
      USE f_aslef2
      USE s_lowerc
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IADT  , IALL  , ICBTYP, IDEG  , IEXP  , IFLG1 ,
     1          IFLG2 , IIND  , ILAT  , ILIN  , ILIN0 , ILON  ,
     2          IMAP  , IMOD  , IORD  , IORSYS, IRC   , IRCDCB, IRCICF,
     3          IRCINX, IREC  , ISTA  , ISTA1 , ISTA2 , ITRM  , ITRM1 ,
     4          ITRM2 , ITYP  , IVAL  , MAXADT, MAXLIN, MAXVAL, NBAD1 ,
     5          NBAD2 , NDEG  , NLAT  , NLIN  , NLON  , NMAP  , NMODEL,
     6          NORD  , NSAEFF, NSTA  , NSTEFF, NUMLIN, NUMRE0, NUMREC,
     7          NUMSA0, NUMSAT, NVAL  , NUMIFB
C
      REAL*8    QSUM1 , QSUM2 , RMS   , XBAD1 , XDINT , XEPO  ,
     1          XINT  , XLAT1 , XLAT2 , XLMAX , XLMIN , XLON1 , XLON2 ,
     2          XTEC  , ZENMAX
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXLIN=30,MAXVAL=69277)
C
C
      CHARACTER*80  TITLE,ADTLST(MAXSAT+MAXREC+1)
      CHARACTER*60  DESTXT(MAXLIN+(MAXREC-1)/12+1),OBSTXT,TITTXT,ADTLBL
      CHARACTER*60  COMTXT(MAXLIN+(MAXREC-1)/12+1),DCBTXT
      CHARACTER*32  INXFIL,DCBFIL
      CHARACTER*20  AGESTR,SATSTR,PGMSTR,DATSTR
      CHARACTER*16  DCBID2(MAXREC),DCBID3(2,1)
      CHARACTER*4   MAPSTR,STANAM(MAXREC)
      CHARACTER*1   DCBSYS(MAXREC)
C
      REAL*8        IONCOE(MAXGIT,*),IONDEV(10,*),QMAT(*),SCAGIM(*)
      REAL*8        INXDEF(12),INXSP1(14),POLE(2),TECMAP(MAXVAL,2)
      REAL*8        DCBVA1(2,MAXSAT),DCBVA2(2,MAXREC),DCBVA3(4,1)
      REAL*8        ALFHLP(MAXGIT)
C
      INTEGER*4     NM(MAXGIT,2,*),NTERM(*),IONREQ(6,*)
      INTEGER*4     IONIND(MAXGIT,*),DCBID1(MAXSAT)
      INTEGER*4     INXINF(4),INXSP2(2)
C
CC      INCLUDE 'COMLFNUM.inc'
C
      DATA IALL/1/
C
C READ IONEX CONTROL FILE
C -----------------------
      CALL RDIXCF(MAXLIN,SATSTR,AGESTR,DESTXT,OBSTXT,COMTXT,
     1            DCBTXT,INXINF,IEXP  ,INXDEF,IRCICF)
C
      IF (IRCICF.NE.1) THEN
        WRITE(LFNERR,900)
900     FORMAT(/,' ### SR INXSAV: IONEX OUTPUT FILE NOT CREATED',/)
        RETURN
      ENDIF
C
C PREPARE IONEX INFORMATION
C -------------------------
      PGMSTR='GPSEST V'//PGMVER
      DATSTR=TITLE(66:80)
      TITTXT=TITLE(1:60)
C
      IF (TITLE(61:64).NE.' ') THEN
        WRITE(LFNERR,910)
910     FORMAT(/,' ### SR INXSAV: TITLE TRUNCATED',/)
      ENDIF
C
      IF (IONREQ(6,1).EQ.0) THEN
        MAPSTR='NONE'
      ELSEIF (IONREQ(6,1).EQ.1) THEN
        MAPSTR='COSZ'
      ELSEIF (IONREQ(6,1).EQ.2) THEN
        MAPSTR='NONE'
      ELSEIF (IONREQ(6,1).EQ.3) THEN
        MAPSTR='NONE'
      ELSE
        MAPSTR='NONE'
C
        WRITE(LFNERR,920)
920     FORMAT(/,' ### SR INXSAV: MAPPING FUNCTION UNKNOWN',/)
      ENDIF
C
      IF (IONDEV(6,1).GT.0.D0) THEN
        XINT=INXDEF(3)/86400.D0
        INXSP1(1)=IDINT((IONDEV(5,1)+IONDEV(6,1))/2.D0)+
     1    INXDEF(1)
        INXSP1(2)=IDINT((IONDEV(5,NMODEL)+IONDEV(6,NMODEL))/2.D0)+
     1    INXDEF(2)
      ELSE
        INXDEF(3)=IDNINT((IONDEV(5,NMODEL)-IONDEV(5,1))/
     1    (NMODEL-1)*24.D0)*3600.D0
        XINT=INXDEF(3)/86400.D0
        INXSP1(1)=IONDEV(5,1)
        INXSP1(2)=IONDEV(5,1)+XINT*(NMODEL-1)
C
        WRITE(LFNERR,940)
940     FORMAT(/,' ### SR INXSAV: OPTIONS CONCERNING REQUESTED ',
     1    'SNAPSHOTS IGNORED',/)
      ENDIF
C
      DO I=3,9
        INXSP1(I)=INXDEF(I)
      ENDDO
C
      INXSP1(10)=90.D0-180.D0/PI*ZENMAX
      INXSP1(11)=1.D-3*CONRE
      INXSP1(12)=1.D-3*IONDEV(1,1)
C
      INXSP1(13)=INXSP1(12)
      INXSP1(14)=0.D0
C
      INXSP2(1)=NSTEFF
      INXSP2(2)=NSAEFF
C
C PREPARE INFORMATION CONCERNING DIFFERENTIAL CODE BIASES
C -------------------------------------------------------
      DO IADT=1,MAXSAT+MAXREC+1
        ADTLST(IADT)=' '
      ENDDO
C
      IF (INXINF(3).EQ.1 .OR. INXINF(4).EQ.1) THEN
        CALL GTFLNA(0,'DCBOUT ',DCBFIL,IRCDCB)
        IF (IRCDCB.EQ.1) CALL GTFLNA(0,'DCBINP ',DCBFIL,IRCDCB)
C
        IF (IRCDCB.EQ.1) THEN
          WRITE(LFNERR,980)
980       FORMAT(/,' ### SR INXSAV: NO DCB-RELATED INFO AVAILABLE',/)
        ENDIF
      ELSE
        IRCDCB=1
      ENDIF
C
      NUMLIN=MAXLIN
      MAXADT=MAXSAT+MAXREC+1
      IF (IRCDCB.EQ.0) THEN
        ICBTYP=1
        CALL RDCBFL(DCBFIL,MAXSAT,MAXREC,0,ICBTYP,NUMSAT,NUMREC,
     1              NUMIFB,DCBID1,DCBVA1,DCBID2,DCBVA2,DCBSYS,
     2              DCBID3,DCBVA3)
C
        IF (ICBTYP.EQ.0) THEN
          WRITE(LFNERR,970) DCBFIL
970       FORMAT(/,' *** SR INXSAV: UNEXPECTED ERROR WITH RESPECT ',
     1      'TO DCB FILE',
     2      /,16X,'FILE NAME: ',A32,/)
          CALL EXITRC(2)
        ENDIF
C
CC        DO ISAT=1,NUMSAT
CC          DCBVA1(2,ISAT)=DCBVA1(2,ISAT)*INXDEF(12)
CC        ENDDO
CC        DO IREC=1,NUMREC
CC          DCBVA2(2,IREC)=DCBVA2(2,IREC)*INXDEF(12)
CC        ENDDO
        NUMSA0=NUMSAT
        IF (INXINF(3).EQ.0) NUMSA0=0
        NUMRE0=NUMREC
        IF (INXINF(4).EQ.0) NUMRE0=0
        CALL DCBADT(NUMSA0,DCBID1,DCBVA1,NUMRE0,DCBID2,DCBVA2,
     1              DCBSYS,DCBTXT,MAXADT,ADTLST)
C
C CREATE LIST OF STATIONS
        IF (NUMREC.GT.0) THEN
          ILIN0=MAXLIN+1
          DESTXT(ILIN0)=' '
          DO ILIN=1,MAXLIN
            IF (COMTXT(ILIN).EQ.' ') ILIN0=MIN0(ILIN0,ILIN)
          ENDDO
          COMTXT(ILIN0)='List of stations:'
          NSTA=1
          STANAM(1)=DCBID2(1)(1:4)
          DO IREC=2,NUMREC
            IF (DCBID2(IREC)(1:4).NE.STANAM(NSTA)) THEN
              NSTA=NSTA+1
              STANAM(NSTA)=DCBID2(IREC)(1:4)
            ENDIF
          ENDDO
          IF (NSTA.NE.NSTEFF) THEN
            WRITE(LFNERR,930)
930         FORMAT(/,' ### SR INXSAV: LIST OF STATIONS ABRIDGED',/)
          ENDIF
          DO ISTA=1,NSTA
            CALL LOWERC(STANAM(ISTA))
          ENDDO
          NLIN=(NSTA-1)/12+1
          DO ILIN=1,NLIN
            COMTXT(ILIN+ILIN0)=' '
            ISTA1=12*ILIN-11
            ISTA2=12*ILIN
            IF (ISTA2.GT.NSTA) ISTA2=NSTA
            WRITE(COMTXT(ILIN+ILIN0),'(12(A4,1X))')
     1        (STANAM(ISTA),ISTA=ISTA1,ISTA2)
          ENDDO
          NUMLIN=MAX0(NUMLIN,ILIN0+NLIN)
        ENDIF
C
        ADTLBL='DIFFERENTIAL CODE BIASES'
      ELSE
        ADTLBL=' '
      ENDIF
C
      NMAP=IDNINT((INXSP1(2)-INXSP1(1))/XINT)+1
C
      NLAT=IDNINT((INXSP1(5)-INXSP1(4))/INXSP1(6))+1
      NLON=IDNINT((INXSP1(8)-INXSP1(7))/INXSP1(9))+1
C
C CHECK MAXIMUM NUMBER OF TEC/RMS VALUES
C --------------------------------------
      NVAL=NMAP*NLAT*NLON
      CALL DIMTST(0,1,2,'INXSAV','MAXVAL','TEC/RMS VALUES',
     1  ' ',NVAL,MAXVAL,IRC)
      IF (IRC.NE.0) RETURN
C
      POLE(1)=IONDEV(3,1)
      POLE(2)=IONDEV(4,1)
C
      NDEG=IONREQ(2,1)
      NORD=IONREQ(3,1)
C
      IFLG1=IONREQ(4,1)
      IFLG2=IONREQ(5,1)
C
C COMPUTE AND STORE TEC/RMS MAPS
C ------------------------------
      DO 210 ITYP=1,2
        IF (INXINF(ITYP).EQ.0) GOTO 210
        IVAL=1
C
        DO 220 IMAP=1,NMAP
          XEPO=INXSP1(1)+(IMAP-1)*XINT
C
          IF (IONDEV(6,1).GT.0.D0) THEN
            XDINT=2.D0/86400.D0
            DO 230 IMOD=1,NMODEL
              IF (XEPO.GE.IONDEV(5,IMOD)-XDINT .AND.
     1            XEPO.LT.IONDEV(6,IMOD)+XDINT) GOTO 235
230         CONTINUE
235         CONTINUE
          ELSE
            IMOD=IMAP
          ENDIF
C
          DO 310 ILAT=1,NLAT
            XLAT1=(INXSP1(4)+(ILAT-1)*INXSP1(6))*PI/180.D0
            DO 320 ILON=1,NLON
              XLON1=(INXSP1(7)+(ILON-1)*INXSP1(9))*PI/180.D0
C
C TRANSFORM EARTH-FIXED COORDINATES INTO SUN-FIXED COORDINATES
C ------------------------------------------------------------
              CALL EFLSFL(XLAT1 ,XLON1 ,XEPO  ,POLE  ,IFLG1 ,IFLG2 ,
     1                    IORSYS,XLAT2 ,XLON2 )
C
              IF (IFLG1.EQ.1 .AND. IALL.EQ.0) THEN
                XLMIN=IONDEV(7,IMOD)
                XLMAX=IONDEV(8,IMOD)
              ELSE
                XLMIN=-PI/2.D0
                XLMAX= PI/2.D0
              ENDIF
C
              IF (XLAT2.GT.XLMIN .AND.
     1            XLAT2.LT.XLMAX) THEN
                IF (ITYP.EQ.1) THEN
                  XTEC=0.D0
                  DO 410 ITRM=1,NTERM(IMOD)
                    IDEG=NM(ITRM,1,IMOD)
                    IORD=NM(ITRM,2,IMOD)
                    XTEC=XTEC+IONCOE(ITRM,IMOD)*
     1                ASLEF2(XLAT2,XLON2,IDEG,IORD,NDEG,NORD)
410               CONTINUE
                ELSE
                  DO ITRM=1,NTERM(IMOD)
                    IDEG=NM(ITRM,1,IMOD)
                    IORD=NM(ITRM,2,IMOD)
                    ALFHLP(ITRM)=ASLEF2(XLAT2,XLON2,IDEG,IORD,NDEG,NORD)
                  ENDDO
C
                  QSUM1=0.D0
                  DO 420 ITRM1=1,NTERM(IMOD)
                    QSUM2=0.D0
                    DO 430 ITRM2=1,NTERM(IMOD)
                      IIND=IKF(IONIND(ITRM2,IMOD),IONIND(ITRM1,IMOD))
                      QSUM2=QSUM2+ALFHLP(ITRM2)*QMAT(IIND)
430                 CONTINUE
                    QSUM1=QSUM1+QSUM2*ALFHLP(ITRM1)
420               CONTINUE
                  IF (QSUM1.GT.0.D0) THEN
                    XTEC=RMS*DSQRT(QSUM1)/SCAGIM(1)*INXDEF(12)
CC                    XTEC=DSQRT(XTEC**2+(TECMAP(IVAL,1)*0.02D0)**2)
                  ELSE
                    XTEC=999.9D0
                  ENDIF
                ENDIF
                IF (DABS(XTEC).GT.999.9D0) XTEC=999.9D0
              ELSE
                XTEC=999.9D0
              ENDIF
C
              TECMAP(IVAL,ITYP)=XTEC
              IVAL=IVAL+1
320         CONTINUE
310       CONTINUE
220     CONTINUE
210   CONTINUE
C
C VERIFY TEC/RMS MAPS AND RESET OR REJECT BAD VALUES
C --------------------------------------------------
      IF (INXINF(2).EQ.1) THEN
        NBAD2=0
        DO IVAL=1,NVAL
          IF (TECMAP(IVAL,2).GT.INXDEF(11) .AND.
     1        TECMAP(IVAL,2).NE.999.9D0) THEN
            TECMAP(IVAL,1)=999.9D0
            TECMAP(IVAL,2)=999.9D0
            NBAD2=NBAD2+1
          ENDIF
        ENDDO
C
        IF (NBAD2.GT.0) THEN
          WRITE(LFNERR,950) NBAD2
950       FORMAT(/,' ### SR INXSAV:',I8,' BAD TEC/RMS VALUES ',
     1      'REJECTED',/)
        ENDIF
      ENDIF
C
      IF (INXINF(1).EQ.1) THEN
        XBAD1=INXDEF(10)
        NBAD1=0
        DO IVAL=1,NVAL
          IF (TECMAP(IVAL,1).LT.INXDEF(10)) THEN
            XBAD1=DMIN1(TECMAP(IVAL,1),XBAD1)
            TECMAP(IVAL,1)=INXDEF(10)
            NBAD1=NBAD1+1
          ENDIF
        ENDDO
C
        IF (NBAD1.GT.0) THEN
          WRITE(LFNERR,960) NBAD1,XBAD1
960       FORMAT(/,' ### SR INXSAV:',I8,' BAD TEC VALUES DOWN TO ',
     1      F7.2,' TECU RESET',/)
        ENDIF
      ENDIF
C
C WRITE IONEX OUTPUT FILE
C -----------------------
      CALL GTFLNA(1,'IONEXRS',INXFIL,IRCINX)
C
      CALL WTIXFL(INXFIL,MAXVAL,NUMLIN,MAXADT,SATSTR,PGMSTR,
     1            AGESTR,DATSTR,TITTXT,DESTXT,MAPSTR,OBSTXT,
     2            COMTXT,INXSP1,INXSP2,ADTLBL,ADTLST,INXINF,
     3            TECMAP,IEXP  )
C
      RETURN
      END SUBROUTINE

      END MODULE
