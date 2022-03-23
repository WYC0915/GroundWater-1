      MODULE s_SAVGIM
      CONTAINS

C*
      SUBROUTINE SAVGIM(IONFIL,IONTYP,NMODEL,IONREQ,IONDEV,NTERM ,
     1                  NM    ,IONCOE,IONSIG,IONTXT,IONTIT,IONINF)
CC
CC NAME       :  SAVGIM
CC
CC PURPOSE    :  SAVE INFORMATION CONCERNING GLOBAL IONOSPHERE MODELS
CC
CC PARAMETERS :
CC         IN :  IONFIL : EXTERNAL IONOSPHERE FILE NAME       CH*32
CC               IONTYP : MODEL TYPE                          I*4
CC                        =1: LOCAL
CC                        =2: GLOBAL
CC                        =3: STATION-SPECIFIC
CC               NMODEL : NUMBER OF MODELS                    I*4
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
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  16-JAN-98
CC
CC CHANGES    :  26-JAN-98 : SS: STATION-SPECIFIC GIMS
CC               29-APR-98 : SS: DTEC LC
CC               26-MAY-98 : SS: SAVE MAXIMUM TEC
CC               23-AUG-99 : SS: DECLARE "VORZ" AS CH*1
CC               18-SEP-01 : SS: NEW TEC MAPPING FUNCTIONS
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               20-MAR-03 : SS: MAKE USE OF SR TIMST2
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-AUG-05 : HB: USE NEW SR TIMST2 (MODULE)
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1998     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_maxdim, ONLY: MAXGIT, MAXGIM
      USE d_const, ONLY: PI
      USE s_opnfil
      USE s_opnerr
      USE s_cordup
      USE s_timst2
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IMOD  , IONTYP, IOSTAT, IPOS  , ITRM  , NMODEL
C
      REAL*8    XDEV1 , XDEV10, XDEV2 , XDEV3 , XDEV4 , XDEV7 , XDEV8 ,
     1          XDEV9
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      CHARACTER*80  IONTIT
      CHARACTER*48  TEXT(27)
      CHARACTER*32  IONFIL
      CHARACTER*19  EPOSTR(2)
      CHARACTER*16  IONTXT(*)
      CHARACTER*8   POLSTR(2)
C
      REAL*8        IONCOE(MAXGIT,*),IONSIG(MAXGIT,*),IONDEV(10,*)
C
      INTEGER*4     IONREQ(6,*),NM(MAXGIT,2,*),NTERM(*),IONINF(*)
      INTEGER*4     IONPOS(MAXGIM)
C
C
C RETURN, IF NO IONOSPHERE FILE NAME SPECIFIED
C --------------------------------------------
      IF (IONFIL.EQ.' ') RETURN
C
C RETURN, IF TYPE OF IONOSPHERE MODEL INVALID
C -------------------------------------------
      IF (IONTYP.NE.2 .AND.
     1    IONTYP.NE.3) THEN
        WRITE(LFNERR,941)
941     FORMAT(/,' ### SR SAVGIM: TYPE OF IONOSPHERE MODEL INVALID',/)
        RETURN
      ENDIF
C
C OPEN IONOSPHERE OUTPUT FILE
C ---------------------------
      CALL OPNFIL(LFNLOC,IONFIL,'UNKNOWN','FORMATTED',
     1  ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,IONFIL,'SAVGIM')
C
C DEFINE TEXT FOR GLOBAL IONOSPHERE MODELS
C ----------------------------------------
      TEXT( 1)='MODEL NUMBER / STATION NAME                    :'
      TEXT( 2)='MODEL TYPE (1=LOCAL,2=GLOBAL,3=STATION)        :'
      TEXT( 3)='MAXIMUM DEGREE OF SPHERICAL HARMONICS          :'
      TEXT( 4)='MAXIMUM ORDER                                  :'
      TEXT( 5)='DEVELOPMENT WITH RESPECT TO                     '
      TEXT( 6)='  GEOGRAPHICAL (=1) OR GEOMAGNETIC (=2) FRAME  :'
      TEXT( 7)='  MEAN (=1) OR TRUE (=2) POSITION OF THE SUN   :'
      TEXT( 8)='MAPPING FUNCTION (0=NONE,1=COSZ,2=MSLM,3=ESM)  :'
      TEXT( 9)='HEIGHT OF SINGLE LAYER AND ITS RMS ERROR (KM)  :'
      TEXT(10)='COORDINATES OF EARTH-CENTERED DIPOLE AXIS       '
      TEXT(11)='  LATITUDE OF NORTH GEOMAGNETIC POLE (DEGREES) :'
      TEXT(12)='  EAST LONGITUDE (DEGREES)                     :'
      TEXT(13)='PERIOD OF VALIDITY                              '
      TEXT(14)='  FROM EPOCH / REFERENCE EPOCH (Y,M,D,H,M,S)   :'
      TEXT(15)='  TO EPOCH                                     :'
      TEXT(16)='LATITUDE BAND COVERED                           '
      TEXT(17)='  MINIMUM LATITUDE (DEGREES)                   :'
      TEXT(18)='  MAXIMUM LATITUDE (DEGREES)                   :'
      TEXT(19)='ADDITIONAL INFORMATION                          '
      TEXT(20)='  NUMBER OF CONTRIBUTING STATIONS              :'
      TEXT(21)='  NUMBER OF CONTRIBUTING SATELLITES            :'
      TEXT(22)='  ELEVATION CUT-OFF ANGLE (DEGREES)            :'
      TEXT(23)='  TIME INTERVAL (SECONDS)                      :'
      TEXT(24)='  MAXIMUM TEC AND ITS RMS ERROR (TECU)         :'
      TEXT(25)='COMMENT / WARNING                              :'
      TEXT(26)='COEFFICIENTS                                    '
      IF (IONINF(4).EQ.0) THEN
        TEXT(27)='DEGREE  ORDER    VALUE (TECU)   RMS (TECU)      '
      ELSE
        TEXT(27)='DEGREE  ORDER    VALUE          RMS (TECU**2)   '
      ENDIF
C
C ORDER STATION-SPECIFIC MODELS
C -----------------------------
      IF (IONTYP.EQ.3) THEN
        CALL CORDUP(IONTXT,NMODEL,1,16,IONPOS)
      ELSE
        DO IMOD=1,NMODEL
          IONPOS(IMOD)=IMOD
        ENDDO
      ENDIF
C
C LOOP OVER ALL MODELS
C --------------------
      DO IMOD=1,NMODEL
        IPOS=IONPOS(IMOD)
C
C WRITE TITLE LINES
        WRITE(LFNLOC,901) IONTIT
901     FORMAT(A80,/,80('-'))
C
        CALL TIMST2(2,1,IONDEV(5,IPOS),EPOSTR(1))
        CALL TIMST2(2,1,IONDEV(6,IPOS),EPOSTR(2))
C
        XDEV1=1.D-3*IONDEV(1,IPOS)
        XDEV2=1.D-3*IONDEV(2,IPOS)
        XDEV3=180.D0/PI*IONDEV(3,IPOS)
        XDEV4=180.D0/PI*IONDEV(4,IPOS)
        IF (IONREQ(4,IPOS).EQ.1) THEN
          POLSTR(1)=' '
          POLSTR(2)=' '
        ELSE
          WRITE(POLSTR(1),'(F8.2)') XDEV3
          WRITE(POLSTR(2),'(F8.2)') XDEV4
        ENDIF
        XDEV7=180.D0/PI*IONDEV(7,IPOS)
        XDEV8=180.D0/PI*IONDEV(8,IPOS)
        XDEV9=IONDEV(9,IPOS)
        XDEV10=IONDEV(10,IPOS)
C
C WRITE HEADER INFORMATION
        WRITE(LFNLOC,921) TEXT(1),IONTXT(IPOS),TEXT(2),IONTYP,
     1    TEXT(3),IONREQ(2,IPOS),TEXT(4),IONREQ(3,IPOS),TEXT(5),
     2    TEXT(6),IONREQ(4,IPOS),TEXT(7),IONREQ(5,IPOS),
     3    TEXT(8),IONREQ(6,IPOS),TEXT(9),XDEV1,XDEV2,
     4    TEXT(10),TEXT(11),POLSTR(1),TEXT(12),POLSTR(2),
     5    TEXT(13),TEXT(14),EPOSTR(1),TEXT(15),EPOSTR(2),
     6    TEXT(16),TEXT(17),XDEV7,TEXT(18),XDEV8,TEXT(19)
921     FORMAT(A48,1X,A16,/,3(A48,I5,/),A48,/,3(A48,I5,/),A48,
     1    2(F8.2,1X),/,A48,/,2(A48,A8,/),A48,/,2(A48,1X,A19,/),
     2    A48,/,2(A48,F8.2,/),A48)
C
C ADDITIONAL INFORMATION
        IF (IONINF(1).GT.0) WRITE(LFNLOC,922) TEXT(20),IONINF(1)
        IF (IONINF(2).GT.0) WRITE(LFNLOC,922) TEXT(21),IONINF(2)
        WRITE(LFNLOC,922) TEXT(22),IONINF(3)
        IF (IONINF(4).GT.0) WRITE(LFNLOC,922) TEXT(23),IONINF(4)
922     FORMAT(A48,I5)
        IF (XDEV9.NE.0.D0) WRITE(LFNLOC,923) TEXT(24),XDEV9,XDEV10
923     FORMAT(A48,F8.2,F9.2)
C
        WRITE(LFNLOC,924) TEXT(25),TEXT(26),TEXT(27)
924     FORMAT(2(A48,/),A48)
C
C WRITE COEFFICIENTS
        DO ITRM=1,NTERM(IPOS)
          WRITE(LFNLOC,931) NM(ITRM,1,IPOS),NM(ITRM,2,IPOS),
     1      IONCOE(ITRM,IPOS),IONSIG(ITRM,IPOS)
931       FORMAT(I4,I8,F18.8,F11.4)
        ENDDO
C
C SKIP ONE LINE
        WRITE(LFNLOC,*)
      ENDDO
C
C CLOSE IONOSPHERE OUTPUT FILE
C ----------------------------
      CLOSE(UNIT=LFNLOC)
C
      RETURN
      END SUBROUTINE

      END MODULE
