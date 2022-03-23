      MODULE s_IONMAX
      CONTAINS

C*
      SUBROUTINE IONMAX(IMODEL,IONREQ,IONDEV,NTERM ,NM    ,IONCOE,
     1                  TECMAX,TECMIN)
CC
CC NAME       :  IONMAX
CC
CC PURPOSE    :  SEARCH FOR THE MAXIMUM AND MINIMUM TEC OF GLOBAL
CC               IONOSPHERE MODELS (WITHIN LATITUDE BAND COVERED).
CC
CC PARAMETERS :
CC         IN :  IMODEL : MODEL INDEX                         I*4
CC               IONREQ(K,I),K=1,..,6,I=1,..,NMODEL:          I*4(6,*)
CC                        (1,I): MINIMUM ELEVATION (IN DEG)
CC                        (2,I): MAXIMUM DEGREE
CC                        (3,I): MAXIMUM ORDER
CC                        (4,I): FLAG FOR REFERENCE FRAME
CC                               =1: GEOGRAPHICAL
CC                               =2: GEOMAGNETIC
CC                        (5,I): FLAG FOR POSITION OF THE SUN
CC                               =1: MEAN
CC                               =2: TRUE
CC                        (6,I): MAPPING FUNCTION
CC                               =1: 1/COS
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
CC        OUT :  TECMAX : (1): MAXIMUM TEC VALUE (TECU)       R*8(*)
CC                        (2): LATITUDE
CC                        (3): SUN-FIXED LONGITUDE
CC               TECMIN : (1): MINIMUM TEC VALUE (TECU)       R*8(*)
CC                        (2): LATITUDE
CC                        (3): SUN-FIXED LONGITUDE
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  08-JAN-98
CC
CC CHANGES    :  26-JAN-98 : SS: INCLUDE FILE "MAXGIT"
CC               25-MAY-98 : SS: R*8 LOOPS REPLACED
CC               26-MAY-98 : SS: "IONDEV" REDIMENSIONED
CC               04-SEP-01 : SS: SR ASLEFU REPLACED BY SR ASLEF2
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1998     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_maxdim, ONLY: MAXGIT
      USE d_const, ONLY: PI
      USE f_aslef2
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IDEG  , ILAT  , ILON  , IMODEL, IORD  , ISTEP ,
     1          ITEC  , ITRM  , NARG  , NDEG  , NLAT  , NLON  , NORD  ,
     2          NSTEP , NTRM
C
      REAL*8    DARG  , DDARG , EMAX  , EMIN  , ETST  , XLAT  ,
     1          XLAT0 , XLON  , XLON0
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      REAL*8        IONCOE(MAXGIT,*),IONDEV(10,*)
      REAL*8        TECMAX(*),TECMIN(*)
C
      INTEGER*4     IONREQ(6,*),NM(MAXGIT,2,*),NTERM(*)
C
C
C INITIALIZE OUTPUT PARAMETERS
C ----------------------------
      DO I=1,3
        TECMAX(I)=0.D0
        TECMIN(I)=0.D0
      ENDDO
C
C RETURN, IF CURRENT MODEL NOT OBSERVED
C -------------------------------------
      IF (IONDEV(7,IMODEL).EQ.0.D0 .AND.
     1    IONDEV(8,IMODEL).EQ.0.D0) THEN
        WRITE(LFNERR,901)
901     FORMAT(/,' ### SR IONMAX: UNOBSERVED TEC',/)
        RETURN
      ENDIF
C
C RETURN, IF CONSTANT TEC ESTIMATED
C ---------------------------------
      NTRM=NTERM(IMODEL)
C
      IF (NTRM.EQ.1) THEN
        TECMAX(1)=IONCOE(1,IMODEL)
        TECMIN(1)=IONCOE(1,IMODEL)
C
        RETURN
      ENDIF
C
C PART 1: ROUGH SCANNING OF THE TEC STRUCTURE
C -------------------------------------------
      EMAX=-1.D10
      EMIN= 1.D10
C
      DARG=PI/180.D0*2.5D0
C
      NLAT=IDNINT(PI/2.D0/DARG)
      NLON=IDNINT(PI/DARG)
C
      NDEG=IONREQ(2,IMODEL)
      NORD=IONREQ(3,IMODEL)
C
      DO 101 ILAT=-NLAT,NLAT
        XLAT=ILAT*DARG
        IF (XLAT.LT.IONDEV(7,IMODEL) .OR.
     1      XLAT.GT.IONDEV(8,IMODEL)) GOTO 101
C
        DO 102 ILON=-NLON,NLON
          XLON=ILON*DARG
          ETST=0.D0
          DO 103 ITRM=1,NTRM
            IDEG=NM(ITRM,1,IMODEL)
            IORD=NM(ITRM,2,IMODEL)
            ETST=ETST+IONCOE(ITRM,IMODEL)*
     1        ASLEF2(XLAT,XLON,IDEG,IORD,NDEG,NORD)
103       CONTINUE
C
          IF (ETST.GT.EMAX) THEN
            EMAX=ETST
            TECMAX(2)=XLAT
            TECMAX(3)=XLON
          ENDIF
          IF (ETST.LT.EMIN) THEN
            EMIN=ETST
            TECMIN(2)=XLAT
            TECMIN(3)=XLON
          ENDIF
102     CONTINUE
101   CONTINUE
C
      TECMAX(1)=EMAX
      TECMIN(1)=EMIN
C
C PART 2: FINE SCANNING
C ---------------------
      NARG=10
C
      NSTEP=2
      DO 201 ISTEP=1,NSTEP
        DDARG=DARG/NARG
C
        DO 202 ITEC=1,2
          IF (ITEC.EQ.1) THEN
            XLAT0=TECMAX(2)
            XLON0=TECMAX(3)
          ELSE
            XLAT0=TECMIN(2)
            XLON0=TECMIN(3)
          ENDIF
C
          DO 203 ILAT=-NARG,NARG
            XLAT=XLAT0+ILAT*DDARG
            IF (XLAT.LT.IONDEV(7,IMODEL) .OR.
     1          XLAT.GT.IONDEV(8,IMODEL)) GOTO 203
C
            DO 204 ILON=-NARG,NARG
              XLON=XLON0+ILON*DDARG
              ETST=0.D0
              DO 205 ITRM=1,NTRM
                IDEG=NM(ITRM,1,IMODEL)
                IORD=NM(ITRM,2,IMODEL)
                ETST=ETST+IONCOE(ITRM,IMODEL)*
     1            ASLEF2(XLAT,XLON,IDEG,IORD,NDEG,NORD)
205           CONTINUE
C
              IF (ITEC.EQ.1) THEN
                IF (ETST.GT.TECMAX(1)) THEN
                  TECMAX(1)=ETST
                  TECMAX(2)=XLAT
                  TECMAX(3)=XLON
                ENDIF
              ELSE
                IF (ETST.LT.TECMIN(1)) THEN
                  TECMIN(1)=ETST
                  TECMIN(2)=XLAT
                  TECMIN(3)=XLON
                ENDIF
              ENDIF
204         CONTINUE
203       CONTINUE
202     CONTINUE
C
        DARG=DARG/NARG
201   CONTINUE
C
      TECMAX(3)=DATAN2(DSIN(TECMAX(3)),DCOS(TECMAX(3)))
      TECMIN(3)=DATAN2(DSIN(TECMIN(3)),DCOS(TECMIN(3)))
C
      RETURN
      END SUBROUTINE

      END MODULE
