      MODULE s_RDIXCF
      CONTAINS

C*
      SUBROUTINE RDIXCF(MAXLIN,SATSTR,AGESTR,DESTXT,OBSTXT,COMTXT,
     1                  DCBTXT,INXINF,IEXP  ,INXDEF,IRCICF)
CC
CC NAME       :  RDIXCF
CC
CC PURPOSE    :  READ IONEX CONTROL FILE
CC
CC PARAMETERS :
CC         IN :  MAXLIN : MAXIMUM NUMBER OF LINES             I*4
CC        OUT :  SATSTR : SATELLITE SYSTEM                    CH*20
CC               AGESTR : AGENCY                              CH*20
CC               DESTXT(I),I=1,..,MAXLIN: MULTI-LINE          CH*60(*)
CC                        DESCRIPTION
CC               OBSTXT : OBSERVABLES USED                    CH*60
CC               COMTXT(I),I=1,..,MAXLIN: MULTI-LINE COMMENT  CH*60(*)
CC               DCBTXT : DCB COMMENT                         CH*60
CC               INXINF : INFORMATION TO BE SAVED             I*4(*)
CC                        (1): TEC MAPS
CC                        (2): RMS MAPS
CC                        (3): CODE BIASES FOR SATELLITES
CC                        (4): CODE BIASES FOR RECEIVERS
CC               IEXP   : DEFAULT EXPONENT                    I*4
CC                        =-1: RECOMMENDED VALUE
CC                        =99: AUTOMATICALLY ADAPTED
CC               INXDEF : DEFINITIONS                         R*8(*)
CC                        ( 1): TIME OF FIRST MAP (IN DAYS)
CC                        ( 2): TIME OF LAST MAP (IN DAYS)
CC                        ( 3): INTERVAL (IN SEC)
CC                        ( 4): FROM LATITUDE
CC                        ( 5): TO LATITUDE
CC                        ( 6): WITH INCREMENT (IN DEG)
CC                        ( 7): FROM LONGITUDE
CC                        ( 8): TO LONGITUDE
CC                        ( 9): WITH INCREMENT (IN DEG)
CC                        (10): MAXIMUM TEC VALUE (IN TECU)
CC                        (11): MINIMUM RMS VALUE (IN TECU)
CC                        (12): SCALING FACTOR FOR RMS VALUES
CC               IRCICF : RETURN CODE (1=OK)                  I*4
CC
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  10-SEP-97
CC
CC CHANGES    :  23-SEP-97 : SS: RETURN "IRCICF"
CC               30-SEP-97 : SS: "INXDEF(3)=0" NOT ALLOWED
CC               17-NOV-97 : SS: RETURN "SATSTR"
CC               18-NOV-97 : SS: RETURN "IEXP"
CC               09-JUN-98 : SS: CHECK IONEX DATA GRID CORRECTLY
CC               11-JUN-98 : SS: ALLOW DCB COMMENT
CC               14-MAR-02 : SS: RECEIVER DCB INFORMATION
CC               06-JUL-04 : RD: FORMAT STATEMENT CORRECTED
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               04-MAY-12 : RD: USE DMOD FROM MODULE, USE M_BERN WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnloc, lfnerr
      USE l_basfun, ONLY: dmod
      USE s_opnfil
      USE s_opnerr
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IDIF  , IEXP  , IHH1  , IHH2  , IINT  , ILIN  ,
     1          IMM1  , IMM2  , IOSTAT, IRCICF, ISS1  , ISS2  , MAXLIN
C
      REAL*8    XDIF
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*60  DESTXT(*),OBSTXT,COMTXT(*),DCBTXT,TEXT
      CHARACTER*32  FILICF
      CHARACTER*20  AGESTR,SATSTR
C
      REAL*8        INXDEF(*),GRDFAC(4)
C
      INTEGER*4     INXINF(*)
C
C
C INITIALIZE "DESTXT" AND "COMTXT"
C --------------------------------
      DO ILIN=1,MAXLIN
        DESTXT(ILIN)=' '
        COMTXT(ILIN)=' '
      ENDDO
C
C OPEN IONEX CONTROL FILE
C -----------------------
      CALL GTFLNA(1,'IONEXCF',FILICF,IRCICF)
      CALL OPNFIL(LFNLOC,FILICF,'OLD','FORMATTED',
     1  'READONLY',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILICF,'RDIXCF')
C
C READ AGENCY
C -----------
      READ(LFNLOC,900) SATSTR,AGESTR
900   FORMAT(///////,6X,A20,//////,6X,A20)
C
      IF (SATSTR.EQ.' ') THEN
        WRITE(LFNERR,905)
905     FORMAT(/,' ### SR RDIXCF: SATELLITE SYSTEM UNDEFINED',/)
        GOTO 100
      ENDIF
C
      IF (AGESTR.EQ.' ') THEN
        WRITE(LFNERR,915)
915     FORMAT(/,' ### SR RDIXCF: AGENCY UNDEFINED',/)
        GOTO 100
      ENDIF
C
C READ MULTI-LINE DESCRIPTION
C ---------------------------
      READ(LFNLOC,'(////)')
      DO ILIN=1,1000
        READ(LFNLOC,920) TEXT
920     FORMAT(6X,A60)
        IF (TEXT.NE.' ') THEN
          IF (ILIN.GT.MAXLIN) THEN
            WRITE(LFNERR,925)
925         FORMAT(/,' ### SR RDIXCF: TOO LONG DESCRIPTION',/)
            GOTO 100
          ENDIF
          DESTXT(ILIN)=TEXT
        ELSE
          IF (ILIN.GT.1) GOTO 110
        ENDIF
      ENDDO
110   CONTINUE
C
C READ OBSERVABLES USED
C ---------------------
      READ(LFNLOC,930) OBSTXT
930   FORMAT(////,6X,A60)
C
      IF (OBSTXT.EQ.' ') THEN
        WRITE(LFNERR,935)
935     FORMAT(/,' ### SR RDIXCF: OBSERVABLES USED UNDEFINED',/)
        GOTO 100
      ENDIF
C
C READ MULTI-LINE COMMENT
C -----------------------
      READ(LFNLOC,'(////)')
      DO ILIN=1,1000
        READ(LFNLOC,940) TEXT
940     FORMAT(6X,A60)
        IF (TEXT.NE.' ') THEN
          IF (ILIN.GT.MAXLIN) THEN
            WRITE(LFNERR,945) MAXLIN
945         FORMAT(/,' ### SR RDIXCF: TOO LONG COMMENT',I6,/)
            GOTO 100
          ENDIF
          COMTXT(ILIN)=TEXT
        ELSE
          IF (ILIN.GT.1) GOTO 120
        ENDIF
      ENDDO
120   CONTINUE
C
C READ DCB COMMENT
C ----------------
      READ(LFNLOC,953) DCBTXT
953   FORMAT(////,6X,A60)
C
C READ "INXINF"
C -------------
      READ(LFNLOC,950,ERR=105) (INXINF(I),I=1,4),IEXP
950   FORMAT(////,2(/,59X,I2),/,2(/,59X,I2),//,59X,I2)
C
      IF (IEXP.EQ.99) THEN
        WRITE(LFNERR,951)
951     FORMAT(/,' ### SR RDIXCF: EXPONENT AUTOMATICALLY ADAPTED',/)
      ELSEIF (IEXP.NE.-1) THEN
        WRITE(LFNERR,952) IEXP
952     FORMAT(/,' ### SR RDIXCF: EXPONENT ',I2,' NOT RECOMMENDED',/)
      ENDIF
C
C READ "INXDEF"
C -------------
      READ(LFNLOC,960,ERR=105) IHH1,IMM1,ISS1,IHH2,IMM2,ISS2,IINT,
     1  (INXDEF(I),I=4,12)
960   FORMAT(////,2(/,59X,3(I2,1X)),//,59X,I6,
     1  /////,3(/,59X,F6.1),/,3(/,59X,F6.1),
     2  /////,2(/,59X,F6.1),//,59X,F6.1)
C
      INXDEF(1)=IHH1/24.D0+IMM1/1440.D0+ISS1/86400.D0
      INXDEF(2)=IHH2/24.D0+IMM2/1440.D0+ISS2/86400.D0
C
      INXDEF(3)=DBLE(IINT)
C
C CHECK INTERVAL
C --------------
      IF (INXDEF(3).GT.0.D0) THEN
        IDIF=IDNINT(DMOD(86400.D0*(INXDEF(2)-INXDEF(1)),INXDEF(3)))
        IF (IDIF.NE.0) THEN
          WRITE(LFNERR,970)
970       FORMAT(/,' ### SR RDIXCF: INCONSISTENT IONEX INTERVAL',/)
          GOTO 100
        ENDIF
      ELSE
        WRITE(LFNERR,970)
        GOTO 100
      ENDIF
C
C CHECK WHETHER "LAT1", "LAT2", ... ARE MULTIPLES OF "DLAT" AND "DLON"
C --------------------------------------------------------------------
      GRDFAC(1)=INXDEF(4)/INXDEF(6)
      GRDFAC(2)=INXDEF(5)/INXDEF(6)
      GRDFAC(3)=INXDEF(7)/INXDEF(9)
      GRDFAC(4)=INXDEF(8)/INXDEF(9)
      DO I=1,4
        XDIF=DABS(GRDFAC(I)-DNINT(GRDFAC(I)))
        IF (XDIF.GT.1.D-4) THEN
          WRITE(LFNERR,980)
980       FORMAT(/,' ### SR RDIXCF: IRREGULAR IONEX DATA GRID',/)
          GOTO 100
        ENDIF
      ENDDO
C
C SET RETURN CODE
C ---------------
      IRCICF=1
      GOTO 200
C
105   CONTINUE
      WRITE(LFNERR,990)
990   FORMAT(/,' ### SR RDIXCF: ERROR READING IONEX CONTROL FILE',/)
C
100   CONTINUE
      IRCICF=0
C
C CLOSE IONEX CONTROL FILE
C ------------------------
200   CONTINUE
      CLOSE (UNIT=LFNLOC)
C
      RETURN
      END SUBROUTINE

      END MODULE
