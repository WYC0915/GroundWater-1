      MODULE s_WTIXHD
      CONTAINS

C*
      SUBROUTINE WTIXHD(MAXLIN,MAXADT,SATSTR,PGMSTR,AGESTR,DATSTR,
     1                  TITTXT,DESTXT,MAPSTR,OBSTXT,COMTXT,INXSP1,
     2                  INXSP2,ADTLBL,ADTLST,IEXP0 )
CC
CC NAME       :  WTIXHD
CC
CC PURPOSE    :  WRITE IONEX HEADER
CC
CC PARAMETERS :
CC         IN :  MAXLIN : MAXIMUM NUMBER OF DES/COM LINES     I*4
CC               MAXADT : MAXIMUM NUMBER OF AUX RECORDS       I*4
CC               SATSTR : SATELLITE SYSTEM                    CH*20
CC               PGMSTR : PROGRAM                             CH*20
CC               AGESTR : AGENCY                              CH*20
CC               DATSTR : DATE AND TIME                       CH*20
CC               TITTXT : TITLE LINE                          CH*60
CC                        =' ': UNDEFINED
CC               DESTXT(I),I=1,..,MAXLIN: MULTI-LINE          CH*60(*)
CC                        DESCRIPTION
CC                        =' ': UNDEFINED
CC               MAPSTR : MAPPING FUNCTION                    CH*4
CC               OBSTXT : OBSERVABLES USED                    CH*60
CC               COMTXT(I),I=1,..,MAXLIN: MULTI-LINE COMMENT  CH*60(*)
CC                        =' ': UNDEFINED
CC               INXSP1 : SPECIFICATIONS 1                    R*8(*)
CC                        ( 1): EPOCH OF FIRST MAP (IN MJD)
CC                        ( 2): EPOCH OF LAST MAP (IN MJD)
CC                        ( 3): INTERVAL (IN SEC)
CC                        ( 4): FROM LATITUDE
CC                        ( 5): TO LATITUDE
CC                        ( 6): WITH INCREMENT (IN DEG)
CC                        ( 7): FROM LONGITUDE
CC                        ( 8): TO LONGITUDE
CC                        ( 9): WITH INCREMENT (IN DEG)
CC                        (10): ELEVATION CUTOFF (IN DEG)
CC                        (11): BASE RADIUS (IN KM)
CC                        (12): FROM HEIGHT
CC                        (13): TO HEIGHT
CC                        (14): WITH INCREMENT (IN KM)
CC               INXSP2 : SPECIFICATIONS 2                    I*4(*)
CC                        =0: UNDEFINED
CC                        (1): NUMBER OF STATIONS
CC                        (2): NUMBER OF SATELLITES
CC               ADTLBL : LABEL OF AUX DATA                   CH*60
CC                        =' ': NO AUX DATA
CC               ADTLST(I),I=1,..,MAXADT: LIST OF AUX DATA    CH*80(*)
CC                        =' ': UNDEFINED
CC               IEXP0  : DEFAULT EXPONENT                    I*4
CC                        =-1: RECOMMENDED VALUE
CC                        =99: EXPONENT AUTOMATICALLY ADAPTED
CC
CC REMARKS    :  IONEX VERSION 1.0
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  11-SEP-97
CC
CC CHANGES    :  01-OCT-97 : SS: "IEXP0=99" AS SPECIAL OPTION
CC               23-AUG-99 : SS: DECLARE "VORZ" AS CH*1
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_jmt
      USE s_radgms
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IADT  , IDAY  , IDIM  , IEXP0 , IHOUR , IINT  ,
     1          ILIN  , IMIN  , IMONTH, ISEC  , IYEAR , MAXADT, MAXLIN,
     2          NMAP
C
      REAL*8    XDAY  , XINT  , XSEC  , XVER
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*80  ADTLST(*)
      CHARACTER*60  TITTXT,DESTXT(*),OBSTXT,COMTXT(*),ADTLBL
      CHARACTER*20  SATSTR,PGMSTR,AGESTR,DATSTR,INXLBL,INXREC(22)
      CHARACTER*4   MAPSTR
      CHARACTER*1   VORZ
C
      REAL*8        INXSP1(*)
C
      INTEGER*4     INXSP2(*)
C
C
      DATA XVER/1.0D0/
C
C SET IONEX RECORDS
C -----------------
      DATA INXREC/'IONEX VERSION / TYPE','PGM / RUN BY / DATE ',
     1            'COMMENT             ','DESCRIPTION         ',
     2            'EPOCH OF FIRST MAP  ','EPOCH OF LAST MAP   ',
     3            'INTERVAL            ','# OF MAPS IN FILE   ',
     4            'MAPPING FUNCTION    ','ELEVATION CUTOFF    ',
     5            'OBSERVABLES USED    ','# OF STATIONS       ',
     6            '# OF SATELLITES     ','BASE RADIUS         ',
     7            'MAP DIMENSION       ','HGT1 / HGT2 / DHGT  ',
     8            'LAT1 / LAT2 / DLAT  ','LON1 / LON2 / DLON  ',
     9            'EXPONENT            ','START OF AUX DATA   ',
     1            'END OF AUX DATA     ','END OF HEADER       '/,
     2     INXLBL/'IONOSPHERE MAPS     '/
C
C WRITE FIRST IONEX RECORD
C ------------------------
      WRITE(LFNLOC,911) XVER,INXLBL,SATSTR,INXREC(1)
911   FORMAT(F8.1,12X,A20,A20,A20)
C
      WRITE(LFNLOC,912) PGMSTR,AGESTR,DATSTR,INXREC(2)
912   FORMAT(3A20,A20)
C
C WRITE TITLE
C -----------
      IF (TITTXT.NE.' ') WRITE(LFNLOC,921) TITTXT,INXREC(3)
921   FORMAT(A60,A20)
C
C WRITE MULTI-LINE DESCRIPTION
C ----------------------------
      DO ILIN=1,MAXLIN
        IF (DESTXT(ILIN).EQ.' ') GOTO 110
        WRITE(LFNLOC,922) DESTXT(ILIN),INXREC(4)
922     FORMAT(A60,A20)
      ENDDO
110   CONTINUE
C
C WRITE EPOCH OF FIRST/LAST MAP
C -----------------------------
      CALL JMT(INXSP1(1),IYEAR,IMONTH,XDAY)
      IDAY=IDINT(XDAY)
      CALL RADGMS(3,XDAY,VORZ,IHOUR,IMIN,XSEC)
      ISEC=IDNINT(XSEC)
      WRITE(LFNLOC,931) IYEAR,IMONTH,IDAY,IHOUR,IMIN,ISEC,INXREC(5)
931   FORMAT(6I6,24X,A20)
C
      CALL JMT(INXSP1(2),IYEAR,IMONTH,XDAY)
      IDAY=IDINT(XDAY)
      CALL RADGMS(3,XDAY,VORZ,IHOUR,IMIN,XSEC)
      ISEC=IDNINT(XSEC)
      WRITE(LFNLOC,932) IYEAR,IMONTH,IDAY,IHOUR,IMIN,ISEC,INXREC(6)
932   FORMAT(6I6,24X,A20)
C
      IINT=IDNINT(INXSP1(3))
      WRITE(LFNLOC,933) IINT,INXREC(7)
933   FORMAT(I6,54X,A20)
C
      XINT=INXSP1(3)/86400.D0
      NMAP=IDNINT((INXSP1(2)-INXSP1(1))/XINT)+1
      WRITE(LFNLOC,934) NMAP,INXREC(8)
934   FORMAT(I6,54X,A20)
C
C WRITE MAPPING FUNCTION, ELEVATION CUTOFF, OBSERVABES USED
C ---------------------------------------------------------
      WRITE(LFNLOC,941) MAPSTR,INXREC(9)
941   FORMAT(2X,A4,54X,A20)
C
      WRITE(LFNLOC,942) INXSP1(10),INXREC(10)
942   FORMAT(F8.1,52X,A20)
C
      WRITE(LFNLOC,943) OBSTXT,INXREC(11)
943   FORMAT(A60,A20)
C
C WRITE NUMBER OF STATIONS/SATELLITES
C -----------------------------------
      IF (INXSP2(1).GT.0) WRITE(LFNLOC,951) INXSP2(1),INXREC(12)
951   FORMAT(I6,54X,A20)
C
      IF (INXSP2(2).GT.0) WRITE(LFNLOC,952) INXSP2(2),INXREC(13)
952   FORMAT(I6,54X,A20)
C
C WRITE BASE RADIUS, MAP DIMENSION
C --------------------------------
      WRITE(LFNLOC,961) INXSP1(11),INXREC(14)
961   FORMAT(F8.1,52X,A20)
C
      IF (INXSP1(14).EQ.0.D0) THEN
        IDIM=2
      ELSE
        IDIM=3
      ENDIF
      WRITE(LFNLOC,962) IDIM,INXREC(15)
962   FORMAT(I6,54X,A20)
C
C WRITE GRID SPECIFICATIONS
C -------------------------
      WRITE(LFNLOC,971) (INXSP1(I),I=12,14),INXREC(16)
971   FORMAT(2X,3F6.1,40X,A20)
C
      WRITE(LFNLOC,972) (INXSP1(I),I=4,6),INXREC(17)
972   FORMAT(2X,3F6.1,40X,A20)
C
      WRITE(LFNLOC,973) (INXSP1(I),I=7,9),INXREC(18)
973   FORMAT(2X,3F6.1,40X,A20)
C
C WRITE DEFAULT EXPONENT AND MULTI-LINE COMMENT
C ---------------------------------------------
      IF (IEXP0.NE.99) WRITE(LFNLOC,981) IEXP0,INXREC(19)
981   FORMAT(I6,54X,A20)
C
      DO ILIN=1,MAXLIN
        IF (COMTXT(ILIN).EQ.' ') GOTO 120
        WRITE(LFNLOC,982) COMTXT(ILIN),INXREC(3)
982     FORMAT(A60,A20)
      ENDDO
120   CONTINUE
C
C WRITE AUX DATA
C --------------
      IF (ADTLBL.NE.' ' .AND. ADTLST(1).NE.' ') THEN
        WRITE(LFNLOC,983) ADTLBL,INXREC(20)
983     FORMAT(A60,A20)
C
        DO IADT=1,MAXADT
          IF (ADTLST(IADT).EQ.' ') GOTO 130
          WRITE(LFNLOC,984) ADTLST(IADT)
984       FORMAT(A80)
        ENDDO
130     CONTINUE
C
        WRITE(LFNLOC,985) ADTLBL,INXREC(21)
985     FORMAT(A60,A20)
      ENDIF
C
C WRITE RECORD CLOSING IONEX HEADER
C ---------------------------------
      WRITE(LFNLOC,991) INXREC(22)
991   FORMAT(60X,A20)
C
      RETURN
      END SUBROUTINE

      END MODULE
