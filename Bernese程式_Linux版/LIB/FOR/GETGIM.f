      MODULE s_GETGIM
      CONTAINS

C*
      SUBROUTINE GETGIM(IONFIL,IALL  ,NMODEL,IONREQ,IONDEV,NTERM ,
     1                  NM    ,IONCOE,IONSIG,IONTXT,IONTIT,IONINF,
     2                  IONTYP)
CC
CC NAME       :  GETGIM
CC
CC PURPOSE    :  EXTRACT ESSENTIAL INFORMATION FROM IONOSPHERE FILE
CC               CONTAINING GLOBAL IONOSPHERE MODELS
CC
CC PARAMETERS :
CC         IN :  IONFIL : EXTERNAL IONOSPHERE FILE NAME       CH*32
CC               IALL   : PROGRAM SWITCH                      I*4
CC                        =0: WITHOUT COEFFICIENTS
CC                        =1: WITH    COEFFICIENTS
CC        OUT :  NMODEL : NUMBER OF MODELS                    I*4
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
CC               IONDEV(K,I),K=1,..,8,I=1,..,NMODEL:          R*8(10,*)
CC                        (1,I): HEIGHT OF SINGLE LAYER (M)
CC                        (2,I): ITS RMS ERROR (M)
CC                               =0: UNDEFINED
CC                        (3,I): LATITUDE OF GEOMAGNETIC POLE
CC                        (4,I): EAST LONGITUDE
CC                        (5,I): FROM EPOCH / REF EPOCH (MJD)
CC                        (6,I): TO EPOCH / 0
CC                        (7,I): MINIMUM LATITUDE
CC                        (8,I): MAXIMUM LATITUDE
CC                        (9,I): MAXIMUM TEC (TECU)
CC                               =0: UNDEFINED
CC                        (10,I): ITS RMS ERROR (TECU)
CC                               =0: UNDEFINED
CC               NTERM(I),I=1,..,NMODEL: NUMBER OF TERMS      I*4(*)
CC                        GIVEN FOR MODEL I
CC               NM(K,J,I),K=1,..,NTERM,J=1,2,I=1,..,NMODEL:  I*4(*,2,*)
CC                        (K,1,I): DEGREE OF TERM K
CC                        (K,2,I): ORDER
CC               IONCOE(K,I),K=1,..,NTERM,I=1,..,NMODEL:      R*8(*,*)
CC                        VALUE OF COEFFICIENT K
CC               IONSIG(K,I),K=1,..,NTERM,I=1,..,NMODEL:      R*8(*,*)
CC                        RMS ERROR OF COEFFICIENT K
CC               IONTXT(I),I=1,..,NMODEL: MODEL NUMBERS       CH*16(*)
CC               IONTIT : TITLE LINE                          CH*80
CC               IONINF : ADDITIONAL INFORMATION              I*4(*)
CC                        (1): NUMBER OF STATIONS
CC                             =0: UNDEFINED
CC                        (2): NUMBER OF SATELLITES
CC                             =0: UNDEFINED
CC                        (3): ELEVATION CUT-OFF ANGLE (DEG)
CC                        (4): TIME INTERVAL (SEC)
CC                             =0: DETERMINISTIC COMPONENT
CC                             >0: STOCHASTIC COMPONENT
CC               IONTYP : MODEL TYPE                          I*4
CC                        =1: LOCAL
CC                        =2: GLOBAL
CC                        =3: STATION-SPECIFIC
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.0
CC
CC CREATED    :  26-SEP-95
CC
CC CHANGES    :  02-OCT-95 : SS: SOME CHECKS BUILT IN
CC               12-OCT-95 : SS: LOOK FOR REDUNDANT MODELS
CC               06-NOV-95 : SS: PRINT REMARK LINE
CC               20-DEC-95 : SS: RETURN "IONTXT"
CC               28-DEC-95 : SS: TID INDICATOR INTRODUCED
CC               19-FEB-96 : SS: SKIP LINES WITH ADDITIONAL INFORMATION
CC               13-OCT-97 : SS: RETURN MINIMUM ELEVATIONS
CC               13-NOV-97 : SS: "IONFIL" AS INPUT PARAMETER
CC               17-NOV-97 : SS: RETURN "IONTIT"
CC               20-JAN-98 : SS: RETURN "IONINF"
CC               26-JAN-98 : SS: STATION-SPECIFIC GIMS
CC               29-APR-98 : SS: DTEC LC
CC               26-MAY-98 : SS: RETURN MAXIMUM TEC
CC               18-SEP-01 : SS: NEW TEC MAPPING FUNCTIONS
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               19-MAR-03 : SS: SAFE HANDLING OF BLANK DATE FIELDS
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               21-SEP-10 : RD: ST2TIM CAN BE USED AS A MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1995     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_maxdim, ONLY: MAXGIM, MAXGIT
      USE d_const, ONLY: PI
      USE s_exitrc
      USE s_dimtst
      USE s_opnfil
      USE s_opnerr
      USE s_st2tim
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IALL  , IDEG  , IFIRST, IMOD  , IMOD1 , IMOD2 ,
     1          IONTYP, IORD  , IOSTAT, IRC   , IREQ2 , IREQ3 , IREQ4 ,
     2          IREQ5 , IREQ6 , ITRM  , KMOD  , NMODEL, NREF
C
      REAL*8    XCOE  , XDEV1 , XDEV10, XDEV2 , XDEV3 , XDEV4 , XDEV7 ,
     1          XDEV8 , XDEV9 , XSIG
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      INTEGER*4 IONREQ(6,*),NM(MAXGIT,2,*),NTERM(*),IONINF(*)
C
      REAL*8 IONCOE(MAXGIT,*),IONSIG(MAXGIT,*),IONDEV(10,*)
C
      CHARACTER*80 WARSTR,IONTIT
      CHARACTER*32 IONFIL,OLDFIL
      CHARACTER*19 EPOSTR(2)
      CHARACTER*16 IONTXT(*),IONSTR
      CHARACTER*8  POLSTR(2)
C
C
      DATA OLDFIL/' '/
C
C CONTROL "IFIRST"
C ----------------
      IF (IONFIL.NE.OLDFIL) THEN
        OLDFIL=IONFIL
        IFIRST=1
      ENDIF
C
C INITIALIZE "IONINF"
C -------------------
      DO I=1,4
        IONINF(I)=0
      ENDDO
C
C OPEN IONOSPHERE FILE
C --------------------
      CALL OPNFIL(LFNLOC,IONFIL,'OLD','FORMATTED',
     1            'READONLY',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,IONFIL,'GETGIM')
C
C READ ALL IONOSPHERE MODELS
C --------------------------
      DO 110 IMOD=1,100000
C
C READ HEADER INFORMATION
        READ(LFNLOC,115,END=120) IONTIT,IONSTR,IONTYP,IREQ2,IREQ3,
     1    IREQ4,IREQ5,IREQ6,XDEV1,XDEV2,POLSTR(1),POLSTR(2),
     2    EPOSTR(1),EPOSTR(2),XDEV7,XDEV8
115     FORMAT(A80,//,49X,A16,/,3(48X,I5,/),/,3(48X,I5,/),48X,
     1    2(F8.2,1X),//,2(48X,A8,/),/,2(49X,A19,/),/,
     2    2(48X,F8.2,/))
C
C READ/SKIP ADDITIONAL INFORMATION
        XDEV9=0.D0
        XDEV10=0.D0
130     READ(LFNLOC,'(A80)') WARSTR
C
        IF (WARSTR(3:33).EQ.'NUMBER OF CONTRIBUTING STATIONS')
     1    READ(WARSTR,'(48X,I5)') IONINF(1)
        IF (WARSTR(3:35).EQ.'NUMBER OF CONTRIBUTING SATELLITES')
     1    READ(WARSTR,'(48X,I5)') IONINF(2)
        IF (WARSTR(3:25).EQ.'ELEVATION CUT-OFF ANGLE')
     1    READ(WARSTR,'(48X,I5)') IONINF(3)
        IF (WARSTR(3:25).EQ.'TIME INTERVAL (SECONDS)')
     1    READ(WARSTR,'(48X,I5)') IONINF(4)
        IF (WARSTR(3:38).EQ.'MAXIMUM TEC AND ITS RMS ERROR (TECU)')
     1    READ(WARSTR,'(48X,F8.2,F9.2)') XDEV9,XDEV10
C
        IF (WARSTR(11:17).NE.'WARNING') GOTO 130
        READ(LFNLOC,'(/)')
C
        IF (IREQ4.EQ.1) THEN
          XDEV3=0.D0
          XDEV4=0.D0
        ELSE
          READ(POLSTR(1),'(F8.2)') XDEV3
          READ(POLSTR(2),'(F8.2)') XDEV4
        ENDIF
C
C CHECK MAXIMUM NUMBER OF MODELS
        CALL DIMTST(1,2,2,'GETGIM','MAXGIM',
     1    'GLOBAL IONOSPHERE MODELS','INCLUDE FILE USED',
     2    IMOD,MAXGIM,IRC)
C
C CHECK TYPE CODE OF IONOSPHERE MODEL
        IF (IONTYP.NE.2 .AND.
     1      IONTYP.NE.3) THEN
          WRITE(LFNERR,910)
910       FORMAT(/,' *** SR GETGIM: ILLEGAL MODEL TYPE',/)
          CALL EXITRC(2)
        END IF
C
C PRINT REMARK LINE
        IF (WARSTR(49:80).NE.' '.AND.
     1      IFIRST.EQ.1) THEN
          WRITE(LFNERR,915) IONSTR,WARSTR(50:80)
915       FORMAT(/,' ### SR GETGIM: GLOBAL IONOSPHERE MODEL : ',A16,/,
     1                         16X,'COMMENT / WARNING       : ',A,/)
        END IF
C
        IONTXT(IMOD)=IONSTR
        IONREQ(1,IMOD)=0
        IONREQ(2,IMOD)=IREQ2
        IONREQ(3,IMOD)=IREQ3
        IONREQ(4,IMOD)=IREQ4
        IONREQ(5,IMOD)=IREQ5
        IONREQ(6,IMOD)=IREQ6
        IONDEV(1,IMOD)=1.D3*XDEV1
        IONDEV(2,IMOD)=1.D3*XDEV2
        IONDEV(3,IMOD)=PI/180.D0*XDEV3
        IONDEV(4,IMOD)=PI/180.D0*XDEV4
        CALL ST2TIM(1,1,EPOSTR(1),IONDEV(5,IMOD))
        CALL ST2TIM(1,1,EPOSTR(2),IONDEV(6,IMOD))
        IONDEV(7,IMOD)=PI/180.D0*XDEV7
        IONDEV(8,IMOD)=PI/180.D0*XDEV8
        IONDEV(9,IMOD)=XDEV9
        IONDEV(10,IMOD)=XDEV10
C
C COMPUTE NUMBER OF TERMS
        NTERM(IMOD)=(IREQ2+1)**2-(IREQ2-IREQ3)*(IREQ2-IREQ3+1)
C
C CHECK MAXIMUM NUMBER OF TERMS
        IF (IALL.EQ.1) THEN
          CALL DIMTST(1,1,2,'GETGIM','MAXGIT',
     1      'TERMS PER GLOBAL IONOSPHERE MODEL','INCLUDE FILE USED',
     2      NTERM(IMOD),MAXGIT,IRC)
        ENDIF
C
C READ COEFFICIENTS
        DO 210 ITRM=1,NTERM(IMOD)
          READ(LFNLOC,215,ERR=990) IDEG,IORD,XCOE,XSIG
215       FORMAT(I4,I8,F18.8,F11.4)
C
          IF (IALL.EQ.1) THEN
            NM(ITRM,1,IMOD)=IDEG
            NM(ITRM,2,IMOD)=IORD
            IONCOE(ITRM,IMOD)=XCOE
            IONSIG(ITRM,IMOD)=XSIG
          END IF
210     CONTINUE
C
C SKIP ONE LINE
        READ(LFNLOC,*,ERR=990)
110   CONTINUE
C
990   WRITE(LFNERR,980)
980   FORMAT(/,' *** SR GETGIM: UNEXPECTED ERROR',/)
      CALL EXITRC(2)
C
C TOTAL NUMBER OF MODELS
120   NMODEL=IMOD-1
C
C SOME CHECKS
C -----------
      IF (IFIRST.EQ.1 .AND. IONTYP.NE.3) THEN
C
C PERIODS OF VALIDITY
        NREF=0
        DO 310 IMOD=1,NMODEL
          IF (IONDEV(6,IMOD).EQ.0.D0) NREF=NREF+1
310     CONTINUE
        IF (NREF.NE.0.AND.
     1      NREF.NE.NMODEL) THEN
          WRITE(LFNERR,940)
940       FORMAT(/,' *** SR GETGIM: GLOBAL IONOSPHERE MODELS.',/,
     1           16X,'INCONSISTANCY CONCERNING ',
     2           'PERIODS OF VALIDITY.',/)
          CALL EXITRC(2)
        END IF
C
        IF (NREF.EQ.NMODEL) THEN
C
C NUMBER OF EPOCH-SPECIFIC MODELS
          IF (NREF.LT.2) THEN
            WRITE(LFNERR,950)
950         FORMAT(/,' *** SR GETGIM: GLOBAL IONOSPHERE MODELS.',/,
     1             16X,'AT LEAST TWO EPOCH-SPECIFIC MODELS ',
     2             'MUST BE GIVEN.',/)
            CALL EXITRC(2)
          END IF
C
C OPTIONS FOR EPOCH-SPECIFIC MODELS, CHRONOLOGICAL ORDER
          DO 320 IMOD2=2,NMODEL
            IMOD1=IMOD2-1
            IF (IONREQ(2,IMOD2).NE.IONREQ(2,IMOD1).OR.
     1          IONREQ(3,IMOD2).NE.IONREQ(3,IMOD1).OR.
     2          IONREQ(4,IMOD2).NE.IONREQ(4,IMOD1).OR.
     3          IONREQ(5,IMOD2).NE.IONREQ(5,IMOD1).OR.
     4          IONREQ(6,IMOD2).NE.IONREQ(6,IMOD1).OR.
     5          IONDEV(5,IMOD2).EQ.IONDEV(5,IMOD1)) THEN
              WRITE(LFNERR,960)
960           FORMAT(/,' *** SR GETGIM: GLOBAL IONOSPHERE MODELS.',/,
     1               16X,'SEQUENCE OF EPOCH-SPECIFIC MODELS ',
     2               'NOT USABLE.',/)
              CALL EXITRC(2)
            END IF
            IF (IONDEV(5,IMOD2).LT.IONDEV(5,IMOD1)) THEN
              WRITE(LFNERR,965)
965           FORMAT(/,' *** SR GETGIM: GLOBAL IONOSPHERE MODELS.',/,
     1               16X,'SEQUENCE OF EPOCH-SPECIFIC MODELS ',
     2               'NOT GIVEN',/,
     3               16X,'IN CHRONOLOGICAL ORDER.',/)
              CALL EXITRC(2)
            END IF
320       CONTINUE
        ELSE IF (NREF.EQ.0) THEN
C
C REDUNDANT MODELS
          DO 410 IMOD=1,NMODEL
            DO 420 KMOD=1,NMODEL
              IF (KMOD.EQ.IMOD) GO TO 420
C
              IF (IONDEV(5,KMOD).GE.IONDEV(5,IMOD).AND.
     1            IONDEV(5,KMOD).LT.IONDEV(6,IMOD).OR.
     2            IONDEV(6,KMOD).GT.IONDEV(5,IMOD).AND.
     3            IONDEV(6,KMOD).LE.IONDEV(6,IMOD)) THEN
                WRITE(LFNERR,970)
970             FORMAT(/,' *** SR GETGIM: GLOBAL IONOSPHERE ',
     1                 'MODELS.',/,
     2                 16X,'MODEL SEQUENCE WITH REDUNDANT ',
     3                 'PERIODS OF VALIDITY.'/)
                CALL EXITRC(2)
              END IF
420         CONTINUE
410       CONTINUE
        END IF
C
        IFIRST=0
      END IF
C
C CLOSE IONOSPHERE FILE
C ---------------------
      CLOSE (UNIT=LFNLOC)
C
      RETURN
      END SUBROUTINE

      END MODULE
