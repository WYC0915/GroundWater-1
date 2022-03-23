      MODULE s_TRPVEC
      CONTAINS

C*
      SUBROUTINE TRPVEC(IPAR  ,NPAR  ,LOCQ  ,XXX   ,ANOR  ,RMS   ,
     1                  ITRGRD,IEXTRA,ITROPO,XSTELL,time,GRDINF,
     2                  IPARAGRD)
CC
CC NAME       :  TRPVEC
CC
CC PURPOSE    :  COMPUTE (A) AZIMUTH AND TILTING ANGLE OF THE
CC               TROPOSPHERIC DELAY VECTOR BASED ON ITS NORTH, EAST,
CC               AND ZENITH COMPONENTS, (B) THE FORMAL ACCURACY OF
CC               THE TILTING ANGLE, AND (C) THE ERROR ELLIPSE OF THE
CC               HORIZONTAL COMPONENTS.
CC
CC PARAMETERS :
CC         IN :  IPAR   : INDEX OF ZENITH PARAMETER           I*4
CC               NPAR   : NUMBER OF PARAMETERS                I*4
CC               LOCQ(K,I),K=1,..,MAXLCQ,I=1,..,NPAR:         I*4(*,*)
CC                        CHARACTERISTICS FOR EACH PARAMETER
CC               XXX(I),I=1,..,NPAR: SOLUTION VECTOR          R*8(*)
CC               ANOR(I),I=1,..,NPAR*(NPAR+1)/2: INVERSE OF   R*8(*)
CC                        NORMAL EQUATION MATRIX
CC               RMS    : RMS ERROR OF UNIT WEIGHT            R*8
CC               ITRGRD : (1): EST. OF TROPOSPHERIC GRADIENTS I*4(*)
CC                             =0: NO ESTIMATION
CC                             =1: TILTING
CC                             =2: LINEAR
C                              =3: TAN(Z)
CC                             =4: CHEN & HERRING
CC                        (2): RATIO OF NUMBER OF ZENITH TO
CC                             GRADIENT PARAMETERS
CC               IEXTRA : EXTRAPOLATED METEO USED             I*4
CC                        =0: NO
CC                        =1: YES
CC                        =2: ESTIMATED VALUES USED
CC               ITROPO : TROPOSPHERIC MODEL                  I*4
CC               XSTELL(K),K=1,2,3: ELLIPSOIDAL STATION       R*8
CC                        COORDINATES
CC        OUT :  GRDINF : (1): TILTING ANGLE IN ARC SEC       R*8(*)
CC                        (2): ITS RMS ERROR
CC                        (3): (1)/(2) - NORMALIZED TILTING
CC                             ANGLE IN UNITS OF RMS ERROR
CC                             (TO TEST TROPOSPHERIC GRADIENT
CC                             FOR STATISTICAL SIGNIFICANCE)
CC                        (4): AZIMUTH OF ZENITH VECTOR IN
CC                             DEG
CC                        (5): ITS RMS ERROR
CC                        (6): MAX RMS ERROR OF HORIZONTAL
CC                             COMPONENT IN M
CC                        (7): MIN RMS ERROR
CC                        (8): AZIMUTH OF PRINCIPAL AXIS OF
CC                             ERROR ELLIPSE IN DEG
CC                        (9): NORTH COMPONENT OF GRADIENT
CC                        (10): ITS RMS
CC                        (11): EAST COMPONENT OF GRADIENT
CC                        (12): ITS RMS
CC               IPARAGRD(4)  : INDICES OF NORTH AND EAST (PREVIOUS AND
CC                              NEXT, RESPECTIVELY) GRADIENT PARAMETER
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC CREATED    :  07-APR-97
CC
CC CHANGES    :  15-APR-97 : SS: CHECK WHETHER ALL COMPONENTS AVAILABLE
CC               08-OCT-97 : MR: RATIO ZENITH/GRADIENT PARAMETERS
CC               28-AUG-00 : MR: USE A PRIORI MODEL FOR COMPUTATION OF
CC                               TILTING
CC               12-MAY-03 : MM: PIECEWISE LINEAR TROPOSPHERE IMPLEMENTED
CC               29-MAR-04 : CU: ADD DUMMY VARIABLE TO CALL OF SR TROPOS
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               13-DEC-05 : CU: ADAPT CALL OF SR TROPOS
CC               24-AUG-06 : AG: SR TDELAY INSTEAD OF TROPOS USED
CC               04-JAN-11 : PS: CHEN/HERRING GRADIENT MAPPING ADDED
CC               01-SEP-11 : LP: OUTPUT OF IPARAGRD
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: HREF, HUMREF, PI, PREF, TREF
      USE f_ikf
      USE s_tdelay
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I1    , I2    , ICOM  , IEXTRA, II    , IPAR  ,
     1          IPAR0 , IPL   , IPN   , IREQ  , ISTA  , ISTR  , ISTREQ,
     2          ITROPA, ITROPO, ITYP  , JJ    , MXCLCQ, NCOM  , NPAR
C
      REAL*8    DR    , DT    , HUM   , PRESS , Q11   , Q12   , Q22   ,
     1          QSUM1 , QSUM2 , RMS   , TEMP  , XTOL  , XVAL1 , XVAL2 ,
     2          XVAL3 , XXX1  , XXX2  , XXX3  , ZEN
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*6 MXNLCQ
C
      REAL*8 XXX(*),ANOR(*),XSTELL(3)
      REAL*8 GRDINF(*),PD(3),time
      REAL*8 XXXA(5),XXXHLP(3),XFT(3,5),QXY(5,5),QXX(3,3)
C
      INTEGER*4 IPARA(5),IPARAGRD(4)
      INTEGER*4 LOCQ(MXCLCQ,*),ITRGRD(*)
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
C
C INITIALIZE OUTPUT ARRAY
C -----------------------
      DO 110 I1=1,12
        GRDINF(I1)=0.D0
110   CONTINUE
C
C RETURN, IF NO GRADIENT PARAMETERS ARE ESTIMATED
C -----------------------------------------------
      IF (ITRGRD(1).EQ.0) RETURN
C
      ITYP=LOCQ(1,IPAR)
      IREQ=LOCQ(2,IPAR)
      ISTA=LOCQ(3,IPAR)
      ICOM=LOCQ(4,IPAR)
      NCOM=LOCQ(5,IPAR)
      ISTR=LOCQ(6,IPAR)
C
      IF (ITYP.NE.6 .AND. ICOM.NE.3 .AND. NCOM.NE.3) THEN
        WRITE(LFNERR,910)
910     FORMAT(/,' *** SR TRPVEC: UNEXPECTED ERROR',/)
        CALL EXITRC(2)
      ENDIF
C
C SEARCH LAST GRADIENT SET OF CURRENT STATION
C -------------------------------------------
      IPAR0=IPAR-ITRGRD(2)
C
      DO IPL=IPAR,IPAR0,-1
        IF (LOCQ(1,IPL).EQ.ITYP .AND.
     1      LOCQ(3,IPL).EQ.ISTA .AND.
     2      LOCQ(4,IPL).EQ.2) GOTO 115
      ENDDO
      GOTO 900
C
C SEARCH NEXT GRADIENT SET OF CURRENT STATION
C -------------------------------------------
115   CONTINUE
C
      IF (IPL.EQ.IPAR-1) THEN
        IPN=IPL-1
        GOTO 120
      END IF
C
      IPAR0=IPAR+ITRGRD(2)
C
      DO IPN=IPAR,IPAR0
        IF (LOCQ(1,IPN).EQ.ITYP .AND.
     1      LOCQ(3,IPN).EQ.ISTA .AND.
     2      LOCQ(4,IPN).EQ.1) GOTO 120
      ENDDO
C
C RETURN, IF GRADIENT PARAMETERS ARE MISSING (NO OBSERVATIONS)
C ------------------------------------------------------------
900   CONTINUE
      WRITE(LFNERR,930) IREQ
930   FORMAT(/,' ### SR TRPVEC: GRADIENT PARAMETERS MISSING',/,
     1  16X,'REQUEST NUMBER: ',I4,/)
      RETURN
C
C PARAMETER INDICES
C -----------------
120   CONTINUE
C
      ISTREQ=ISTR-LOCQ(6,IPL)
      IPARA=(/IPL-1,IPL,IPN,IPN+1,IPAR/)
      IPARAGRD=(/IPL-1,IPL,IPN,IPN+1/)
C
C COMPUTE GRADIENT AND RMS FOR REQUESTED TIME
C -------------------------------------------
      XXXA=(/XXX(IPL-1),XXX(IPL),XXX(IPN),XXX(IPN+1),XXX(IPAR)/)
      DT=1.d0*(ISTREQ)/ITRGRD(2)
C
C TRANSFORMATION FUNCTION
      XFT(1,:)=(/1.d0-DT,0.d0,DT,0.d0,0.d0/)
      XFT(2,:)=(/0.d0,1.d0-DT,0.d0,DT,0.d0/)
      XFT(3,:)=(/0.d0,0.d0,0.d0,0.d0,1.d0/)
C
C PART OF ANOR CONCERNING GRADIENTS
      DO II=1,5
        DO JJ=1,5
          QXY(II,JJ)=ANOR(IKF(IPARA(II),IPARA(JJ)))
        END DO
      END DO
C
C TRANSFORMATIONS
      XXXHLP=MATMUL(XFT,XXXA)
      QXX=MATMUL(MATMUL(XFT,QXY),TRANSPOSE(XFT))
      XXX1=XXXHLP(1)
      XXX2=XXXHLP(2)
      XXX3=XXXHLP(3)
C
C ADD A PRIORI ZENITH DELAY TO DELAY ESTIMATE
C -------------------------------------------
      IF (IEXTRA.EQ.0) THEN
C
C A PRIORI MODEL VALUE IS SET TO ZERO, IF OBSERVED METEO VALUES USED
        DR=0.D0
      ELSE
        ZEN=0.D0
        CALL TDELAY(time,ZEN,XSTELL,ITROPO,1,0D0,TEMP,PRESS,HUM,DR)
      ENDIF
      XXX3 = DR + XXX3
C
C TILTING ANGLE AND AZIMUTH OF THE TROPOSPHERIC DELAY VECTOR
C ----------------------------------------------------------
      IF (ITRGRD(1).EQ.1.OR.ITRGRD(1).EQ.3.OR.ITRGRD(1).EQ.4) THEN
        XTOL=1.D0
C
        IF (XXX3.GT.XTOL) THEN
          XVAL1=DSQRT(XXX1**2+XXX2**2)
          XVAL2=XVAL1/XXX3
        ELSE
          WRITE(LFNERR,920) IREQ,XXX3,XTOL
920       FORMAT(/,' ### SR TRPVEC: TILTING ANGLE AND ITS RMS ERROR',
     1      ' NOT COMPUTED',/,
     2      16X,'REQUEST NUMBER                 : ',I4,/,
     3      16X,'ZENITH DELAY                   : ',F6.1,' M',/,
     4      16X,'MINIMUM ZENITH DELAY TOLERATED : ',F6.1,' M',/)
          XVAL2=0.D0
        ENDIF
C
        GRDINF(1)=648000.D0/PI*XVAL2
      ENDIF
C
      XVAL1=0.D0
      IF (XXX1.NE.0.D0 .OR. XXX2.NE.0.D0)
     1  XVAL1=DATAN2(XXX2,XXX1)+PI
C
      GRDINF(4)=180.D0/PI*XVAL1
C
C THE FORMAL ACCURACY OF THE TILTING ANGLE (AND THE AZIMUTH)
C ----------------------------------------------------------
      IF (ITRGRD(1).EQ.1.OR.ITRGRD(1).EQ.3.OR.ITRGRD(1).EQ.4) THEN
        XVAL1=DSQRT(XXX1**2+XXX2**2)
C
        IF (XXX3.GT.XTOL .AND. XVAL1.GT.0.D0) THEN
          PD(1)=XXX1/XXX3/XVAL1
          PD(2)=XXX2/XXX3/XVAL1
          PD(3)=-XVAL1/XXX3**2
        ELSE
          PD(1)=0.D0
          PD(2)=0.D0
          PD(3)=0.D0
        ENDIF
C
        QSUM1=0.D0
        DO 210 I1=1,3
          QSUM2=0.D0
          DO 220 I2=1,3
            QSUM2=QSUM2+PD(I2)*QXX(I2,I1)
220       CONTINUE
          QSUM1=QSUM1+QSUM2*PD(I1)
210     CONTINUE
C
        GRDINF(2)=648000.D0/PI*RMS*DSQRT(QSUM1)
C
        XVAL1=XXX1**2+XXX2**2
C
        IF (XVAL1.GT.0.D0) THEN
          PD(1)=-XXX2/XVAL1
          PD(2)= XXX1/XVAL1
          PD(3)=0.D0
C
          QSUM1=0.D0
          DO 310 I1=1,3
            QSUM2=0.D0
            DO 320 I2=1,3
              QSUM2=QSUM2+PD(I2)*QXX(I2,I1)
320         CONTINUE
            QSUM1=QSUM1+QSUM2*PD(I1)
310       CONTINUE
C
          GRDINF(5)=180.D0/PI*RMS*DSQRT(QSUM1)
        ELSE
          GRDINF(5)=0.D0
        ENDIF
      ENDIF
C
C THE ERROR ELLIPSE OF THE HORIZONTAL COMPONENTS
C ----------------------------------------------
      Q11=QXX(1,1)
      Q22=QXX(2,2)
      Q12=QXX(1,2)
C
      XVAL1=DSQRT((Q11-Q22)**2+4.D0*Q12**2)
C
      GRDINF(6)=RMS*DSQRT((Q11+Q22+XVAL1)/2.D0)
      GRDINF(7)=RMS*DSQRT((Q11+Q22-XVAL1)/2.D0)
C
      XVAL1=2.D0*Q12
      XVAL2=Q11-Q22
      XVAL3=0.D0
      IF (XVAL1.NE.0.D0 .OR. XVAL2.NE.0.D0)
     1  XVAL3=DATAN2(XVAL1,XVAL2)/2.D0
      IF (XVAL3.LT.0.D0) XVAL3=XVAL3+PI
C
      GRDINF(8)=180.D0/PI*XVAL3
C
C TEST VALUE
C ----------
      IF (ITRGRD(1).EQ.1.OR.ITRGRD(1).EQ.3.OR.ITRGRD(1).EQ.4) THEN
        IF (GRDINF(2).GT.0.D0) THEN
          GRDINF(3)=GRDINF(1)/GRDINF(2)
        ELSE
          GRDINF(3)=0.D0
        ENDIF
      ENDIF
C
C NORTH AND EAST GRADIENT AND RMS
C -------------------------------
      GRDINF(9) = XXX1
      GRDINF(10)= RMS*DSQRT(Q11)
      GRDINF(11)= XXX2
      GRDINF(12)= RMS*DSQRT(Q22)
C
      RETURN
      END SUBROUTINE

      END MODULE
