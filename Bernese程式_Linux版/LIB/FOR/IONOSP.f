      MODULE s_IONOSP
      CONTAINS

C*
      SUBROUTINE IONOSP(XSTAT ,XSAT  ,TOBS  ,SZ    ,ZEN   ,ICARR ,
     1                  IORSYS,NAMSTA,SVN   ,ZENMAX,DR    ,
     2                  MEATYP,AZI   ,HOI   ,XSTELL)
CC
CC NAME       :  IONOSP
CC
CC PURPOSE    :  COMPUTE IONOSPHERIC DISTANCE CORRECTION DR USING
CC               LOCAL OR GLOBAL IONOSPHERE MODELS (IONTYP=1,2)
CC
CC PARAMETERS :
CC         IN :  XSTAT(K),K=1,2,3: STATION COORDINATES IN     R*8
CC                        GEOCENTRIC FRAME
CC               XSAT(K),K=1,2,3: SATELLITE COORDINATES IN    R*8
CC                        SYSTEM OF EPOCH
CC               TOBS   : OBSERVATION TIME IN JULIAN DATE     R*8
CC               SZ     : TRUE SIDERAL TIME                   R*8
CC               ZEN    : ZENITH DISTANCE                     R*8
CC               ICARR  : OBSERVED CARRIER                    I*4
CC                        =1: L1
CC                        =2: L2
CC                        =3: L3 (IONOSPHERE-FREE LC)
CC                        =4: L4 (GEOMETRY-FREE LC)
CC                        =5: L5 (WIDE-LANE LC)
CC               IORSYS : ORBIT SYSTEM                        I*4
CC                        =1: B1950.0
CC                        =2: J2000.0
CC               NAMSTA : STATION NAME                        CH*16
CC               SVN    : SATELLITE NUMBER                    I*4
CC               ZENMAX : MAXIMUM ZENITH DISTANCE (RAD)       R*8
CC               MEATYP : MEASUREMENT TYP                     I*4
CC                        =1: PHASE OBSERVATIONS
CC                        =2: CODE OBSERVATIONS
CC                        =3: RANGE OBSERVATIONS
CC               AZI    : AZIMUTH ANGLE (FROM NORTH TO EAST,  R*8
CC                        IN RADIAN)
CC               XSTELL(J),J=1..3: ELLIPSOIDAL STATION        R*8
CC                         COORDINATES (LAT/LON IN RAD,
CC                         HEIGHT IN M)
CC        OUT :  DR     : DISTANCE CORRECTION IN M            R*8
CC               HOI    : HIGHER-ORDER IONOSPHERE PARAMETERS  R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER, S.SCHAER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/29 12:14
CC
CC CHANGES    :  12-APR-94 : ??: CORRECT BOUNDARY PROBLEM
CC               10-AUG-94 : MR: CALL EXITRC
CC               05-DEC-95 : SS: NEW IONOSPHERE MODEL (TYPE 2)
CC               05-DEC-95 : SS: CALL WITH "IORSYS"
CC               05-DEC-95 : SS: CHECK LATITUDE BAND OF GIM
CC               05-DEC-95 : SS: USE "FACSLM(1)"
CC               05-DEC-95 : SS: HANDLE BOUNDARY PROBLEM
CC               13-DEC-95 : SS: USE MODEL-SPECIFIC "XHGT"
CC               20-DEC-95 : SS: "IONTXT" IN CALL OF SR GETGIM
CC               28-DEC-95 : SS: DEFINE TOLERANCE "XTOL"
CC               29-DEC-95 : SS: INCREASE "XTOL" FROM 0.5 TO 1.5 DEG
CC               05-FEB-96 : SS: INCREASE "XTOL" FROM 1.5 TO 5.0 DEG
CC               06-AUG-96 : SS: USE OF "DEPO" IN MULTI-MODEL MODE
CC               04-JUN-97 : SS: SET "XTOL" TO 45 DEG
CC               13-NOV-97 : SS: "IONFIL" IN CALL OF SR GETGIM
CC               17-NOV-97 : SS: "IONTIT" IN CALL OF SR GETGIM
CC               10-DEC-97 : SS: MANAGE UNDEFINED GIMS
CC               20-JAN-98 : SS: "IONINF" ADDED
CC               26-JAN-98 : SS: STATION-SPECIFIC GIMS
CC               29-APR-98 : SS: DTEC LC
CC               04-MAY-98 : SS: PASS "SVN"
CC               26-MAY-98 : SS: "IONDEV" REDIMENSIONED
CC               05-AUG-99 : SS: CONSIDER "ZENMAX"
CC               16-DEC-00 : HU: INCREASE "XTOL" FROM 5.0 TO 10.0
CC               18-JAN-01 : SS: SET "XTOL" TO 45.0
CC               29-JUL-01 : DS: CALL GIMARG WITH NAMSTA
CC               04-SEP-01 : SS: SR ASLEFU REPLACED BY SR ASLEF2
CC               12-NOV-02 : SS: "DEPO" FROM 1 TO 60 SEC
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.INC REMOVED, M_BERN ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               04-JAN-10 : CK/SL: APPLY HIGHER ORDER IONOSPHERIC EFFECTS
CC               04-JAN-10 : SL: MEATYP,AZI,HOI ADDED, F_IONOSP2->S_IONOSP2
CC               19-NOV-10 : SL: USE M_BERN WITH ONLY
CC               26-MAR-12 : RD: SWITCH FROM TIMSTR TO TIMST2
CC               26-MAR-12 : RD: USE GETGIM AS MODULE NOW
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, r8b, lfnErr
      USE m_maxdim, ONLY: MAXGIM, MAXGIT
      USE d_const,  ONLY: PI
      USE l_basfun, ONLY: dmod
      USE s_chkion
      USE s_getgim
      USE s_ionbsf
      USE s_timst2
      USE s_ionosi
      USE s_gimarg
      USE s_exitrc
      USE f_aslef2
      USE s_jmt
      USE s_radgms
      USE s_gtflna
      USE s_ionosp2
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICARR , IDAY  , IDEG  , IFIRST, IFLG1 , IFLG2 , IHOUR ,
     1          IM    , IM1   , IM2   , IMF   , IMIN  , IMODE , IMODEL,
     2          IMONTH, IONTYP, IORD  , IORSYS, IRC   , ISEC  , ITERM ,
     3          IYEAR , NDEG  , NMODEL, NORD
C
      REAL*8    B0    , DAY   , DB    , DB0   , DE0   , DEPO  ,
     1          DEPO1 , DEPO2 , DLAT  , DLAT1 , DLAT2 , DR    , DRZEN ,
     2          DS    , DS0   , ELNUM , ESUM  , FAC1  , FAC2  , FACTOR,
     3          HION  , S0    , SEC   , SZ    , TOBS  , XDEV1 , XDEV2 ,
     4          XHGT  , XLAT  , XLAT1 , XLAT2 , XSFL  , XTOL  , XTST1 ,
     5          XTST2 , ZEN   , ZENMAX, ZP    , ALFXMF, XHGTM
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
C
      CHARACTER     VORZ,EPOSTR*19,IONFIL*32,IONTIT*80
      CHARACTER*16  IONTXT(MAXGIM),NAMSTA
      INTEGER*4     IONREQ(6,MAXGIM),NTERM(MAXGIM),NM(MAXGIT,2,MAXGIM)
      INTEGER*4     IONINF(4),SVN
      REAL*8        IONCOE(MAXGIT,MAXGIM),IONSIG(MAXGIT,MAXGIM)
      REAL*8        IONDEV(10,MAXGIM),XSTAT(3),XSAT(3)
      REAL*8        POLE(2),FACSLM(3)
      REAL*8        XSL(3),XSLM(3),POLEM(2)
C
      INTEGER(i4b)          ,OPTIONAL :: MEATYP
      REAL(r8b)             ,OPTIONAL :: AZI
      REAL(r8b),DIMENSION(3),OPTIONAL :: HOI
      REAL(r8b),DIMENSION(3),OPTIONAL :: XSTELL
C
      COMMON/CIONSP/IONREQ,NM,IONCOE,IONSIG,IONDEV,IONTXT
C
C
      DATA IFIRST/1/

C
C READ MODEL COEFFICIENTS AT FIRST CALL
C -------------------------------------
      IF(IFIRST.EQ.1)THEN
        CALL CHKION(IONTYP)
C
        IF (IONTYP.EQ.1) THEN
          CALL IONOSI(1     ,NMODEL,IONREQ,IONDEV,NTERM ,NM    ,
     1                IONCOE,IONSIG)
        ELSE IF (IONTYP.GE.2) THEN
          CALL GTFLNA(1,'IONOS  ',IONFIL,IRC)
C
          CALL GETGIM(IONFIL,1     ,NMODEL,IONREQ,IONDEV,NTERM ,
     1                NM    ,IONCOE,IONSIG,IONTXT,IONTIT,IONINF,
     2                IONTYP)
C
          IF (IONINF(4).GT.0) THEN
            WRITE(LFNERR,310)
310         FORMAT(/,' *** SR IONOSP: DTEC INFORMATION FOUND',/)
            CALL EXITRC(2)
          ENDIF
C
          IF (IONDEV(6,1).NE.0.D0) THEN
            IMODE=1
          ELSE
            IMODE=2
          END IF
C
          XTOL=45.0D0*PI/180.D0
          DLAT= 0.01D0*PI/180.D0
          DLAT1=0.D0
          DLAT2=0.D0
        END IF
C
        IFIRST=0
      END IF
C
C RETURN, IF NO IONOSPHERE MODELS APPLIED OR IONOSPHERE-FREE LC OBSERVED
C ----------------------------------------------------------------------
      IF (IONTYP.EQ.0.OR.(ICARR.EQ.3.AND..NOT.PRESENT(HOI))) THEN
        DR=0.D0
        RETURN
      END IF
C
      IF (IONTYP.EQ.1) THEN
C
C PART 1: APPLY LOCAL IONOSPHERE MODELS
C -------------------------------------
C LOOK FOR CORRECT MODEL
        DO 10 IMODEL=1,NMODEL
          IF(TOBS.GT.IONDEV(4,IMODEL).AND.TOBS.LE.IONDEV(5,IMODEL))THEN
            B0=IONDEV(1,IMODEL)
            S0=DMOD(IONDEV(2,IMODEL),1.D0)*2.D0*PI+IONDEV(3,IMODEL)-PI
            S0=DMOD(S0+10.D0*PI,2.D0*PI)
            DE0=IONDEV(6,IMODEL)
            DB0=IONDEV(7,IMODEL)
            DS0=IONDEV(8,IMODEL)
            HION=IONREQ(4,IMODEL)*1000.D0
            GO TO 20
          END IF
10      CONTINUE
C
C NO SUITABLE IONOSPHERE MODEL FOUND
        CALL JMT(TOBS,IYEAR,IMONTH,DAY)
        IDAY=IDINT(DAY)
        CALL RADGMS(3,DAY-IDAY,VORZ,IHOUR,IMIN,SEC)
        ISEC=IDNINT(SEC)
        WRITE(LFNERR,11) MOD(IYEAR,100),IMONTH,IDAY,IHOUR,IMIN,ISEC
11      FORMAT(/,' *** SR IONOSP: NO SUITABLE IONOSPHERE MODEL FOUND',/,
     1                       16X,'DATE AND TIME:',I4,'-',I2,'-',I2,
     2                            I4,':',I2,':',I2,/)
        CALL EXITRC(2)
C
C IONOSPHERE MODEL FOUND
20      CONTINUE
C
C COMPUTE ELECTRON DENSITY IN DIRECTION TO THE SATELLITE
        CALL IONBSF(XSTAT,XSAT,TOBS,SZ,ZEN,HION,ICARR,B0,S0,
     1              DE0,DB0,DS0,FACTOR,DB,DS,ZP)
        ELNUM=0.D0
        DO 30 ITERM=1,NTERM(IMODEL)
          ELNUM=ELNUM+IONCOE(ITERM,IMODEL)*DB**NM(ITERM,1,IMODEL)
     1                                    *DS**NM(ITERM,2,IMODEL)
30      CONTINUE
        DRZEN=FACTOR*ELNUM
        DR=DRZEN/DCOS(ZP)
C
      ELSE IF (IONTYP.GE.2) THEN
C
C PART 2: APPLY GLOBAL IONOSPHERE MODELS
C --------------------------------------
C
C LOOK FOR APPLICABLE MODEL CONSIDERING TOLERANCE "DEPO"
        DEPO=60.D0/86400.D0
C
        IF (IMODE.EQ.1) THEN
          DO 211 IM1=1,NMODEL
            IM2=IM1
C
            IF (TOBS.GE.IONDEV(5,IM1)-DEPO.AND.
     1          TOBS.LT.IONDEV(6,IM1)+DEPO) THEN
              IF (IONTYP.EQ.3) THEN
                IF (NAMSTA.EQ.IONTXT(IM1)) GOTO 220
              ELSE
                GOTO 220
              ENDIF
            ENDIF
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
            IF (TOBS.GE.IONDEV(5,IM1)-DEPO1.AND.
     1          TOBS.LT.IONDEV(5,IM2)+DEPO2) GO TO 220
212       CONTINUE
        ENDIF
C
C NO SUITABLE MODEL FOUND
        CALL TIMST2(1,1,TOBS,EPOSTR)
        WRITE(LFNERR,210) EPOSTR,NAMSTA
210     FORMAT(/,' *** SR IONOSP: NO SUITABLE IONOSPHERE MODEL FOUND',
     1    /,16X,'DATE AND TIME : ',A,
     2    /,16X,'STATION NAME  : ',A16,/)
        CALL EXITRC(2)
C
C APPLICABLE MODEL FOUND
220     CONTINUE
C
        IF (IMODE.EQ.1) THEN
          FAC1=1.D0
        ELSE
          FAC1=(IONDEV(5,IM2)-TOBS)/(IONDEV(5,IM2)-IONDEV(5,IM1))
          FAC2=(TOBS-IONDEV(5,IM1))/(IONDEV(5,IM2)-IONDEV(5,IM1))
        END IF
C
        NDEG=IONREQ(2,IM1)
        NORD=IONREQ(3,IM1)
        IFLG1=IONREQ(4,IM1)
        IFLG2=IONREQ(5,IM1)
        IMF=IONREQ(6,IM1)
C
        ESUM    =0.D0
        POLEM(1)=0.D0
        POLEM(2)=0.D0
        XSLM(1) =0.D0
        XSLM(2) =0.D0
        XSLM(3) =0.D0
        XHGTM   =0.D0
        DO 230 IM=IM1,IM2
          XHGT=IONDEV(1,IM)
          POLE(1)=IONDEV(3,IM)
          POLE(2)=IONDEV(4,IM)
          CALL GIMARG(XSTAT ,XSAT  ,TOBS  ,SZ    ,ZEN   ,XHGT  ,
     1                POLE  ,ICARR ,IFLG1 ,IFLG2 ,IMF   ,IORSYS,
     2                SVN   ,XLAT  ,XSFL  ,NAMSTA,FACSLM,
     3                ALFXMF,XSL)
C
C CHECK LATITUDE BAND
          IF (IONDEV(7,IM).EQ.0.D0 .AND.
     1        IONDEV(8,IM).EQ.0.D0) THEN
            WRITE(LFNERR,265) IONTXT(IM)
265         FORMAT(/,' ### SR IONOSP: GLOBAL IONOSPHERE MODEL ',
     1        'UNDEFINED',
     2        /,16X,'MODEL NUMBER             : ',A16,/)
          ELSEIF (ZEN.LE.ZENMAX) THEN
            XTST1=IONDEV(7,IM)-XLAT
            IF (XTST1.GT.DLAT1+DLAT) THEN
              DLAT1=XTST1
              XLAT1=180.D0/PI*IONDEV(7,IM)
              XDEV1=180.D0/PI*DLAT1
              WRITE(LFNERR,250) IONTXT(IM),XLAT1,XDEV1
250           FORMAT(/,' ### SR IONOSP: APPLICABLE LATITUDE BAND OF ',
     1               'GLOBAL',
     2               /,16X,'IONOSPHERE MODEL EXCEEDED',
     3               /,16X,'MODEL NUMBER             : ',A16,
     4               /,16X,'MINIMUM LATITUDE ALLOWED : ',F7.2,
     5               /,16X,'DEVIATION (DEGREES)      : ',F7.2,/)
            END IF
C
            XTST2=XLAT-IONDEV(8,IM)
            IF (XTST2.GT.DLAT2+DLAT) THEN
              DLAT2=XTST2
              XLAT2=180.D0/PI*IONDEV(8,IM)
              XDEV2=180.D0/PI*DLAT2
              WRITE(LFNERR,255) IONTXT(IM),XLAT2,XDEV2
255           FORMAT(/,' ### SR IONOSP: APPLICABLE LATITUDE BAND OF ',
     1               'GLOBAL',
     2               /,16X,'IONOSPHERE MODEL EXCEEDED',
     3               /,16X,'MODEL NUMBER             : ',A16,
     4               /,16X,'MAXIMUM LATITUDE ALLOWED : ',F7.2,
     5               /,16X,'DEVIATION (DEGREES)      : ',F7.2,/)
            END IF
C
C EXIT, IF TOLERANCE "XTOL" EXCEEDED
            IF (DLAT1.GT.XTOL.OR.DLAT2.GT.XTOL) THEN
              XTOL=180.D0/PI*XTOL
              WRITE(LFNERR,260) XTOL
260           FORMAT(/,' *** SR IONOSP: TOLERANCE FOR LATITUDE BAND ',
     1               'EXCEEDED',
     2               /,16X,'TOLERANCE (DEGREES)      : ',F7.2,/)
              CALL EXITRC(2)
            ENDIF
          ENDIF
C
C         INTERPOLATION OF GEOMAGNETIC POLE, INTERSECTION POINT
C         AND HEIGHT OF LAYER
          IF (IM.EQ.IM1) THEN
            POLEM(1)=FAC1*POLE(1)
            POLEM(2)=FAC1*POLE(2)
            XSLM(1) =FAC1*XSL(1)
            XSLM(2) =FAC1*XSL(2)
            XSLM(3) =FAC1*XSL(3)
            XHGTM   =FAC1*XHGT
          ELSE
            POLEM(1)=POLEM(1)+FAC2*POLE(1)
            POLEM(2)=POLEM(2)+FAC2*POLE(2)
            XSLM(1) =XSLM(1) +FAC2*XSL(1)
            XSLM(2) =XSLM(2) +FAC2*XSL(2)
            XSLM(3) =XSLM(3) +FAC2*XSL(3)
            XHGTM   =XHGTM   +FAC2*XHGT
          END IF
C
          DO 240 ITERM=1,NTERM(IM)
            IDEG=NM(ITERM,1,IM)
            IORD=NM(ITERM,2,IM)
            IF (IM.EQ.IM1) THEN
              ESUM=ESUM+FAC1*IONCOE(ITERM,IM)*
     1          ASLEF2(XLAT,XSFL,IDEG,IORD,NDEG,NORD)
            ELSE
              ESUM=ESUM+FAC2*IONCOE(ITERM,IM)*
     1          ASLEF2(XLAT,XSFL,IDEG,IORD,NDEG,NORD)
            END IF
240       CONTINUE
230     CONTINUE
C
        DR=FACSLM(1)*ESUM
      END IF
C
C APPLY HIGHER ORDER IONOSPHERIC DISTANCE CORRECTION FOR PHASE AND CODE OBS.
C ----------------------------------------------------------------------------
C
      IF(PRESENT(HOI).AND.PRESENT(XSTELL).AND.MEATYP.LE.2) THEN
        CALL IONOSP2(XSTAT ,ZEN   ,MEATYP ,AZI   ,POLEM ,
     1               XHGTM ,SVN   ,ESUM   ,ICARR ,ALFXMF,
     2               XSLM  ,IFLG1 ,TOBS   ,HOI   ,XSTELL)
        DR=DR+(HOI(1)+HOI(2)+HOI(3))
      END IF
C
      RETURN
      END SUBROUTINE

      END MODULE
