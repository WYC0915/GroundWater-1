      MODULE s_PDGIM
      CONTAINS

C*
      SUBROUTINE PDGIM (NPARN ,LOCQ  ,PARTYP,OPTGIM,POLGIM,EPOGIM,
     1                  SCAGIM,TOBS  ,DTSIM ,WGSEPO,XVSAT ,SZ    ,
     2                  ZENITH,ICARR ,IORSYS,MEATYP,NDIFF ,IS12  ,
     3                  SVN   ,STNAME,IP    ,AHELP )
CC
CC NAME       :  PDGIM
CC
CC PURPOSE    :  COMPUTE PARTIAL DERIVATIVES WITH RESPECT TO GLOBAL
CC               IONOSPHERE MODEL PARAMETERS (COEFFICIENTS AND
CC               SINGLE-LAYER HEIGHT PARAMETERS).
CC
CC PARAMETERS :
CC         IN :  NPARN  : NUMBER OF NON-AMBIGUITY PARAMETERS  I*4
CC               LOCQ(K,I),K=1,..,MAXLCQ,I=1,..,NPAR:         I*4(*,*)
CC                        CHARACTERISTICS FOR EACH PARAMETER
CC               PARTYP : PARAMETER DESCRIPTION               t_partyp(*)
CC               OPTGIM : OPTIONS FOR GLOBAL IONOSPHERE MODEL I*4(*)
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
CC                             =0: NONE
CC                             =1: 1/COS
CC                        (9): STATION-SPECIFIC MODELS
CC                        (10): COMPONENT TO BE ESTIMATED
CC                              =1: DETERMINISTIC
CC                              =2: STOCHASTIC
CC               POLGIM(I,J),I=1,2,3,J=1,..,OPTGIM(7):        R*8(3,*)
CC                        I=1: HEIGHT OF SINGLE LAYER (M)
CC                        I=2: LAT. OF NORTH GEOMAGNETIC POLE
CC                        I=3: EAST LONGITUDE
CC               EPOGIM(I,J),I=1,2,J=1,..,OPTGIM(7): PERIODS  R*8(2,*)
CC                        OF VALIDITY / REF EPOCHS (MJD)
CC               SCAGIM : SCALING FACTOR FOR                  R*8(*)
CC                        (1): ION. COEFFICIENTS
CC                        (2): SINGLE-LAYER HEIGHT
CC                        (3): DTEC PARAMETERS
CC               TOBS   : OBSERVATION TIME (MJD)              R*8
CC               DTSIM  : TIME INTERVAL TO IDENTIFY OBSERV.   R*8
CC                        OF THE SAME EPOCH
CC               WGSEPO(K,I),K=1,2,3,I=1,2: GEOCENTRIC        R*8(3,2)
CC                        STATION COORDINATES CORRECTED FOR
CC                        EARTH TIDES
CC               XVSAT  : POSITION AND VELOCITY OF SATELLITE  R*8(*)
CC                        IN SYSTEM OF EPOCH
CC               SZ     : SIDERAL TIME (GREENWICH)            R*8
CC               ZENITH(I),I=1,2: ZENITH DISTANCES (WITH      R*8(2)
CC                        RESPECT TO REFERENCE ELLIPSOID)
CC               ICARR  : FREQUENCY                           I*4
CC               IORSYS : ORBIT SYSTEM                        I*4
CC                        =1: B1950.0
CC                        =2: J2000.0
CC               MEATYP : MEASUREMENT TYPE                    I*4
CC                        =1: PHASE
CC                        =2: CODE
CC               NDIFF  : DIFFERENCE TYPE                     I*4
CC                        NDIFF=0: ZERO DIFFERENCE
CC                        NDIFF=1: SINGLE DIFFERENCE
CC               IS12(I),I=1,2: STATION NUMBERS INVOLVED      I*4
CC               SVN    : SATELLITE NUMBER                    I*4
CC               STNAME(I),I=1,..,NSTAT: STATION NAMES        CH*16(*)
CC     IN/OUT :  IP     : PARAMETER INDEX                     I*4
CC               AHELP(I),I=1,..,NPAR: ONE LINE OF A-MATRIX   R*8(*)
CC
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.0
CC
CC CREATED    :  01-DEC-95
CC
CC CHANGES    :  04-DEC-95 : SS: TOLERANCE "DEPO" INTRODUCED
CC               12-DEC-95 : SS: HANDLE BOUNDARY PROBLEM FOR DYNAMIC
CC                               MODELS CORRECTLY
CC               13-DEC-95 : SS: USE MODEL-SPECIFIC "XHGT"
CC               20-DEC-95 : SS: "IONTXT" IN CALL OF SR GETGIM
CC               14-AUG-97 : SS: USE "NDIFF"
CC               13-NOV-97 : SS: "IONFIL" IN CALL OF SR GETGIM
CC               17-NOV-97 : SS: "IONTIT" IN CALL OF SR GETGIM
CC               20-JAN-98 : SS: "IONINF" ADDED
CC               26-JAN-98 : SS: STATION-SPECIFIC GIMS
CC               06-APR-98 : SS: COMPUTATION OF PARTIALS OPTIMIZED
CC               29-APR-98 : SS: DTEC LC
CC               04-MAY-98 : SS: INCLUDE 'COMFREQ.inc' FOR GLONASS
CC               26-MAY-98 : SS: "IONDEV" REDIMENSIONED
CC               30-JUL-01 : DS: NEW PARAMETER STNAME
CC               30-JUL-01 : DS: CALL GIMARG WITH STNAME
CC               04-SEP-01 : SS: SR ASLEFU REPLACED BY SR ASLEF2
CC               12-NOV-02 : SS: "DEPO" FROM 1 TO 60 SEC
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               26-MAY-03 : RD: CORRECT DIMENSION OF WGSEPO (SEE PRCEPO)
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC               04-JAN-10 : CK/SL: ADD "ALFXMF" AND "XSL" TO CALL OF SR GIMARG
CC               16-NOV-10 : RD: UPDATE INTERVAL FOR PIECE-WISE LINEAR PARAM.
CC               26-MAR-12 : RD: SWITCH FROM TIMSTR TO TIMST2
CC               26-MAR-12 : RD: USE GETGIM AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1995     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: LFNERR
      USE d_const,  ONLY: ars
      USE m_maxdim, ONLY: MAXGIT, MAXGIM
      USE p_gpsest, ONLY: t_partyp
      USE d_par,    ONLY: parType_linear,
     1                    parType_linearLeftPoint,
     2                    parType_linearMiddlePoint,
     3                    parType_linearRightPoint
      USE s_getgim
      USE s_timst2
      USE s_gimarg
      USE s_exitrc
      USE f_aslef2
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IACT  , ICARR , IDEG  , IFIRST, IFLG1 , IFLG2 , ILAT  ,
     1          ILST  , IM    , IM1   , IM2   , IMF   , IMOD  , IONTYP,
     2          IORD  , IORSYS, IP    , IPAR  , IRC   , IREQ  , ISTA  ,
     3          ITERM , MEATYP, MXCLCQ, NDEG  , NDIFF , NMODEL, NORD  ,
     4          NPARN , NSTA
C
      REAL*8    AH1   , DEPO  , DEPO1 , DEPO2 , DHGT  , EPO1  , DTSIM ,
     1          EPO2  , ESUM  , ESUM1 , ETOT  , ETOT1 , FAC1  , FAC2  ,
     2          FACTIM, SZ    , TOBS  , XHGT  , XHGT1 , XLAT  , XLAT1 ,
     3          XSFL  , XSFL1
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      TYPE(t_partyp), DIMENSION(:) :: partyp
C
      CHARACTER*80  IONTIT
      CHARACTER*32  IONFIL
      CHARACTER*19  EPOSTR
      CHARACTER*16  IONTXT(MAXGIM),STNAME(*)
      CHARACTER*6   MXNLCQ
C
      REAL*8 POLGIM(3,*),EPOGIM(2,*),SCAGIM(*)
      REAL*8 WGSEPO(9,2),XVSAT(*),ZENITH(2),AHELP(*)
      REAL*8 IONCOE(MAXGIT,MAXGIM),IONSIG(MAXGIT,MAXGIM)
      REAL*8 IONDEV(10,MAXGIM),POLE(2)
      REAL*8 FACSLM(3),FACSL1(3),FACMEA(3)
      REAL*8 ALFXMF,XSL(3)
C
      INTEGER*4 LOCQ(MXCLCQ,*),OPTGIM(*),IONINF(4),IS12(*),SVN
      INTEGER*4 IONREQ(6,MAXGIM),NTERM(MAXGIM),NM(MAXGIT,2,MAXGIM)
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
      COMMON/CPDGIM/IONDEV,IONCOE,IONSIG,IONREQ,NTERM,NM,IONTXT
C
C
      DATA IFIRST/1/
      DATA DHGT/1.D0/
C
      IF (IFIRST.EQ.1) THEN
C
C SET SOME FACTORS
C ----------------
        FACMEA(1)= 1.D0
        FACMEA(2)=-1.D0
        FACMEA(3)= 0.D0
C
C READ A PRIORI IONOSPHERE MODELS, IF MODEL IMPROVEMENT IS REQUESTED
C ------------------------------------------------------------------
        IF (OPTGIM(5).GT.0) THEN
          CALL GTFLNA(1,'IONOS  ',IONFIL,IRC)
C
          CALL GETGIM(IONFIL,1     ,NMODEL,IONREQ,IONDEV,NTERM ,
     1                NM    ,IONCOE,IONSIG,IONTXT,IONTIT,IONINF,
     2                IONTYP)
        ELSE
          NMODEL=OPTGIM(7)
        END IF
C
        IFIRST=0
      END IF
C
C RETURN, IF IONOSPHERE-FREE LINEAR COMBINATION IS OBSERVED
C ---------------------------------------------------------
      IF (ICARR.EQ.3) RETURN
C
      IREQ=LOCQ(2,IP)
      IMOD=LOCQ(3,IP)
      IDEG=LOCQ(4,IP)
C
C RETURN, IF DEGREE OF IONOSPHERIC COEFFICIENT IS NOT ZERO
C --------------------------------------------------------
      IF (IREQ.NE.2 .AND. IDEG.NE.0) RETURN
C
      NSTA=NDIFF+1
C
      IACT=0
      IF (OPTGIM(9).EQ.1) THEN
        IF (IMOD.GT.0) THEN
          DO ISTA=1,NSTA
            IF (IMOD.EQ.IS12(ISTA)) IACT=ISTA
          ENDDO
          IF (IACT.EQ.0) RETURN
        ENDIF
      ENDIF
C
C CHECK TIME BOUNDARIES CONSIDERING TOLERANCE "DEPO" (SEE SR IONOSP)
C ------------------------------------------------------------------
      DEPO=60.D0/86400.D0
C
      DEPO1=0.D0
      DEPO2=0.D0
C
      IF (IMOD.NE.0) THEN
        IF (OPTGIM(6).EQ.1) THEN
          EPO1=EPOGIM(1,IMOD)
          IF (IMOD.EQ.1) DEPO1=DEPO
          EPO2=EPOGIM(2,IMOD)
          IF (IMOD.EQ.NMODEL) DEPO2=DEPO
        ELSE
          EPO1=EPOGIM(1,IMOD)
          IF (IMOD.EQ.1.OR.
     1        IMOD.EQ.NMODEL) DEPO1=DEPO
C
          IF (IMOD.EQ.1) THEN
            EPO2=EPOGIM(1,IMOD+1)
            IF (IMOD+1.EQ.NMODEL) DEPO2=DEPO
          ELSE IF (IMOD.EQ.NMODEL) THEN
            EPO2=EPOGIM(1,IMOD-1)
            IF (IMOD-1.EQ.1) DEPO2=DEPO
          ELSE IF (TOBS.GE.EPO1) THEN
            EPO2=EPOGIM(1,IMOD+1)
            IF (IMOD+1.EQ.NMODEL) DEPO2=DEPO
          ELSE
            EPO2=EPOGIM(1,IMOD-1)
            IF (IMOD-1.EQ.1) DEPO2=DEPO
          END IF
        END IF
      ELSE
        IF (OPTGIM(6).EQ.1) THEN
          EPO1=EPOGIM(1,1)
          DEPO1=DEPO
          EPO2=EPOGIM(2,NMODEL)
          DEPO2=DEPO
        ELSE
          EPO1=EPOGIM(1,1)
          DEPO1=DEPO
          EPO2=EPOGIM(1,NMODEL)
          DEPO2=DEPO
        END IF
      END IF
C
      IF (EPO2.GT.EPO1) THEN
        IF (TOBS.LT.EPO1-DEPO1.OR.TOBS.GE.EPO2+DEPO2) RETURN
      ELSE
        IF (TOBS.LT.EPO2-DEPO2.OR.TOBS.GE.EPO1+DEPO1) RETURN
      END IF
C
      IF (OPTGIM(6).EQ.1.OR.IMOD.EQ.0) THEN
        FACTIM=1.D0
      ELSE
        FACTIM=(EPO2-TOBS)/(EPO2-EPO1)
      END IF
C
      IF (IREQ.NE.2) THEN
C
C PART 1: IONOSPHERIC COEFFICIENTS
C --------------------------------
        DO IPAR=IP,NPARN
          IF (LOCQ(1,IPAR).NE.19 .OR.
     1        LOCQ(2,IPAR).EQ. 2 .OR.
     2        LOCQ(3,IPAR).NE.IMOD) GOTO 110
          ILST=IPAR
          AHELP(ILST)=0.D0
        ENDDO
C
110     CONTINUE
        DO 111 ISTA=1,NSTA
          IF (IACT.NE.0 .AND. IACT.NE.ISTA) GOTO 111
          XHGT=POLGIM(1,IMOD)
          POLE(1)=POLGIM(2,IMOD)
          POLE(2)=POLGIM(3,IMOD)
          NDEG=OPTGIM(1)
          NORD=OPTGIM(2)
          IFLG1=OPTGIM(3)
          IFLG2=OPTGIM(4)
          IMF=OPTGIM(8)
C
          CALL GIMARG(WGSEPO(1,ISTA)      ,XVSAT ,TOBS  ,SZ    ,
     1                ZENITH(ISTA) ,XHGT  ,POLE  ,ICARR ,IFLG1 ,
     2                IFLG2 ,IMF   ,IORSYS,SVN   ,XLAT  ,XSFL  ,
     3                STNAME(IS12(ISTA))  ,FACSLM,ALFXMF,XSL   )
C
C SAVE MIN. AND MAX. LATITUDE OF IONOSPHERIC PIERCE POINTS (IN ARC SEC)
          ILAT=IDNINT(ars*XLAT)
          IF (ILAT.LT.LOCQ(6,IP)) LOCQ(6,IP)=ILAT
          IF (ILAT.GT.LOCQ(7,IP)) LOCQ(7,IP)=ILAT
C
          DO 112 IPAR=IP,ILST
            IDEG=LOCQ(4,IPAR)
            IORD=LOCQ(5,IPAR)
            IF (IREQ.EQ.1) THEN
              AHELP(IPAR)=AHELP(IPAR)+(-1.D0)**(ISTA-1)/SCAGIM(1)*
     1          FACMEA(MEATYP)*FACTIM*FACSLM(1)*
     2          ASLEF2(XLAT,XSFL,IDEG,IORD,NDEG,NORD)
            ELSE
              AHELP(IPAR)=AHELP(IPAR)+1.D0/SCAGIM(3)*
     1          FACTIM*FACSLM(3)*
     2          ASLEF2(XLAT,XSFL,IDEG,IORD,NDEG,NORD)
            ENDIF
C
C Update the parameter type description
C -------------------------------------
            IF( partyp(ipar)%type(1:1) == parType_linear(1:1)) THEN
              IF (tobs-dtsim > epo1) THEN
C
                IF (partyp(ipar)%type == parType_linear) THEN
                  partyp(ipar)%type = parType_linearLeftPoint
                ELSEIF (partyp(ipar)%type ==
     1                               parType_linearRightPoint) THEN
                  partyp(ipar)%type = parType_linearMiddlePoint
                ENDIF
C
              ELSEIF (tobs+dtsim < epo1) THEN
C
                IF (partyp(ipar)%type == parType_linear) THEN
                  partyp(ipar)%type = parType_linearRightPoint
                ELSEIF (partyp(ipar)%type ==
     1                               parType_linearLeftPoint) THEN
                  partyp(ipar)%type = parType_linearMiddlePoint
                ENDIF
C
              ENDIF
C
              partyp(ipar)%omega=DABS(epo2-epo1)
            ENDIF
C
112       CONTINUE
111     CONTINUE
C
C SAVE INDEX OF LAST PARAMETER
        IP=ILST
      ELSE
C
C PART 2: SINGLE-LAYER HEIGHT PARAMETERS
C --------------------------------------
C
C LOOK FOR APPLICABLE MODEL CONSIDERING TOLERANCE "DEPO"
        IF (OPTGIM(6).EQ.1) THEN
          DO 211 IM1=1,NMODEL
            IM2=IM1
C
            DEPO1=0.D0
            IF (IM1.EQ.1) DEPO1=DEPO
            DEPO2=0.D0
            IF (IM1.EQ.NMODEL) DEPO1=DEPO
C
            IF (TOBS.GE.EPOGIM(1,IM1)-DEPO1.AND.
     1          TOBS.LT.EPOGIM(2,IM1)+DEPO2) GO TO 220
211       CONTINUE
        ELSE
          DO 212 IM2=2,NMODEL
            IM1=IM2-1
C
            DEPO1=0.D0
            IF (IM2.EQ.2) DEPO1=DEPO
            DEPO2=0.D0
            IF (IM2.EQ.NMODEL) DEPO2=DEPO
C
            IF (TOBS.GE.EPOGIM(1,IM1)-DEPO1.AND.
     1          TOBS.LT.EPOGIM(1,IM2)+DEPO2) GO TO 220
212       CONTINUE
        ENDIF
C
C NO SUITABLE MODEL FOUND / UNEXPECTED ERROR
        CALL TIMST2(1,1,TOBS,EPOSTR)
        WRITE(LFNERR,910) EPOSTR
910     FORMAT(/,' *** SR PDGIM : NO SUITABLE IONOSPHERE MODEL FOUND',
     1                     /,16X,'DATE AND TIME: ',A,/)
        CALL EXITRC(2)
C
C APPLICABLE MODEL FOUND
220     CONTINUE
        DO 230 ISTA=1,NSTA
          ETOT=0.D0
          ETOT1=0.D0
          DO 240 IM=IM1,IM2
            IF (OPTGIM(6).EQ.1) THEN
              FAC1=1.D0
            ELSE
              FAC1=(EPOGIM(1,IM2)-TOBS)/(EPOGIM(1,IM2)-EPOGIM(1,IM1))
              FAC2=(TOBS-EPOGIM(1,IM1))/(EPOGIM(1,IM2)-EPOGIM(1,IM1))
            END IF
C
            XHGT=POLGIM(1,IM)
            POLE(1)=POLGIM(2,IM)
            POLE(2)=POLGIM(3,IM)
            NDEG=OPTGIM(1)
            NORD=OPTGIM(2)
            IFLG1=OPTGIM(3)
            IFLG2=OPTGIM(4)
            IMF=OPTGIM(8)
C
            CALL GIMARG(WGSEPO(1,ISTA)      ,XVSAT ,TOBS  ,SZ    ,
     1                  ZENITH(ISTA) ,XHGT  ,POLE  ,ICARR ,IFLG1 ,
     2                  IFLG2 ,IMF   ,IORSYS,SVN   ,XLAT  ,XSFL  ,
     3                  STNAME(IS12(ISTA))  ,FACSLM,ALFXMF,XSL   )
C
            XHGT1=XHGT+DHGT
C
            CALL GIMARG(WGSEPO(1,ISTA)      ,XVSAT ,TOBS  ,SZ    ,
     1                  ZENITH(ISTA) ,XHGT1 ,POLE  ,ICARR ,IFLG1 ,
     2                  IFLG2 ,IMF   ,IORSYS,SVN   ,XLAT1 ,XSFL1 ,
     3                  STNAME(IS12(ISTA))  ,FACSL1,ALFXMF,XSL   )
C
            ESUM=0.D0
            ESUM1=0.D0
            DO 250 ITERM=1,NTERM(IM)
              IDEG=NM(ITERM,1,IM)
              IORD=NM(ITERM,2,IM)
              ESUM=ESUM+IONCOE(ITERM,IM)*
     1          ASLEF2(XLAT,XSFL,IDEG,IORD,NDEG,NORD)
              ESUM1=ESUM1+IONCOE(ITERM,IM)*
     1          ASLEF2(XLAT1,XSFL1,IDEG,IORD,NDEG,NORD)
250         CONTINUE
            IF (IM.EQ.IM1) THEN
              ETOT=ETOT+FAC1*ESUM
              ETOT1=ETOT1+FAC1*ESUM1
            ELSE
              ETOT=ETOT+FAC2*ESUM
              ETOT1=ETOT1+FAC2*ESUM1
            END IF
240       CONTINUE
C
          AH1=(ETOT1-ETOT)/DHGT
C
          AHELP(IP)=AHELP(IP)+(-1.D0)**(ISTA-1)/SCAGIM(2)*
     1      FACMEA(MEATYP)*FACTIM*(FACSLM(2)*ETOT+FACSLM(1)*AH1)
C
C Update the parameter type description
C -------------------------------------
          IF( partyp(ip)%type(1:1) == parType_linear(1:1)) THEN
            IF (tobs-dtsim > EPOGIM(1,IM)) THEN
C
              IF (partyp(ip)%type == parType_linear) THEN
                partyp(ip)%type = parType_linearLeftPoint
              ELSEIF (partyp(ip)%type == parType_linearRightPoint) THEN
                partyp(ip)%type = parType_linearMiddlePoint
              ENDIF
C
            ELSEIF (tobs+dtsim < EPOGIM(1,IM)) THEN
C
              IF (partyp(ip)%type == parType_linear) THEN
                partyp(ip)%type = parType_linearRightPoint
              ELSEIF (partyp(ip)%type == parType_linearLeftPoint) THEN
                partyp(ip)%type = parType_linearMiddlePoint
              ENDIF
C
            ENDIF
C
            partyp(ip)%omega=DABS(epo2-epo1)
          ENDIF
230     CONTINUE
      END IF
C
      RETURN
      END SUBROUTINE

      END MODULE
